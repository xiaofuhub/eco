$PBExportHeader$w_book_navigation.srw
forward
global type w_book_navigation from w_sheet
end type
type dw_booknavigationall from u_pics_dw within w_book_navigation
end type
type dw_bktemplateall from u_pics_dw within w_book_navigation
end type
type st_3 from statictext within w_book_navigation
end type
type st_2 from statictext within w_book_navigation
end type
type st_1 from statictext within w_book_navigation
end type
type mle_nav_instr from uo_mle within w_book_navigation
end type
type mle_sitxt from uo_mle within w_book_navigation
end type
type mle_alert from uo_mle within w_book_navigation
end type
type dw_bktemp_fordddw from u_dw within w_book_navigation
end type
type sle_conno from uo_pics_sle within w_book_navigation
end type
type shl_1 from statichyperlink within w_book_navigation
end type
type shl_2 from statichyperlink within w_book_navigation
end type
type cb_close from u_cb within w_book_navigation
end type
type cb_clear from u_cb within w_book_navigation
end type
type cb_update from u_cb within w_book_navigation
end type
type cb_temp from u_cb within w_book_navigation
end type
type cb_add from u_cb within w_book_navigation
end type
type cb_sp from u_cb within w_book_navigation
end type
type cb_find from u_cb within w_book_navigation
end type
type dw_booknavigation from u_dw within w_book_navigation
end type
type dw_bk_nvgt_elmts from u_dw within w_book_navigation
end type
type dw_bknvg_forupdate from u_dw within w_book_navigation
end type
type dw_bktemplate from u_dw within w_book_navigation
end type
end forward

global type w_book_navigation from w_sheet
integer x = 214
integer y = 221
integer width = 3136
integer height = 1976
dw_booknavigationall dw_booknavigationall
dw_bktemplateall dw_bktemplateall
st_3 st_3
st_2 st_2
st_1 st_1
mle_nav_instr mle_nav_instr
mle_sitxt mle_sitxt
mle_alert mle_alert
dw_bktemp_fordddw dw_bktemp_fordddw
sle_conno sle_conno
shl_1 shl_1
shl_2 shl_2
cb_close cb_close
cb_clear cb_clear
cb_update cb_update
cb_temp cb_temp
cb_add cb_add
cb_sp cb_sp
cb_find cb_find
dw_booknavigation dw_booknavigation
dw_bk_nvgt_elmts dw_bk_nvgt_elmts
dw_bknvg_forupdate dw_bknvg_forupdate
dw_bktemplate dw_bktemplate
end type
global w_book_navigation w_book_navigation

type variables
boolean ib_changed=false
int i_row
end variables

forward prototypes
public function string wf_get_comments (string lconno)
public function integer wf_insert_template ()
public function integer wf_update_booknavigation ()
public function integer wf_insert_element ()
end prototypes

public function string wf_get_comments (string lconno);string ls_comments
//setNull(ls_comments)
//
//select comments
//into :ls_comments
//from booknavigation
//where bookelement = 'Others'
//and conno = :lconno
//using sqlservertrans;

return ls_comments
end function

public function integer wf_insert_template ();long i, li_cnt, j, li_cur, li_null, rtn
string ls_temp, ls_elemt

setNull(li_null)
delete 
from booknavigationtemplates
using sqlservertrans;
if f_check_dberror(sqlservertrans, 'insert booknavigationtemplates')=false then
	return -1
else
	commit using sqlservertrans;
end if
for i=1 to 17
	dw_bktemplate.InsertRow(0)
	choose case i
		case 1
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=1
			dw_bktemplate.object.bookelement[i]='Title/Author'
			dw_bktemplate.object.position[i]=1
		case 2
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=1
			dw_bktemplate.object.bookelement[i]='Annotation'
			dw_bktemplate.object.position[i]=2
		case 3
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Table of Content'
			dw_bktemplate.object.position[i]=3
		case 4
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Preface'
			dw_bktemplate.object.position[i]=4
		case 5
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Introduction'
			dw_bktemplate.object.position[i]=5
		case 6
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Prolog'
			dw_bktemplate.object.position[i]=6
		case 7
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Part'
			dw_bktemplate.object.position[i]=7
		case 8
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=1
			dw_bktemplate.object.bookelement[i]='Chapter'
			dw_bktemplate.object.position[i]=8
		case 9
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Section'
			dw_bktemplate.object.position[i]=9	
		case 10
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Subsection'
			dw_bktemplate.object.position[i]=10
		case 11
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Story'
			dw_bktemplate.object.position[i]=11
		case 12
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Epilog'
			dw_bktemplate.object.position[i]=12
		case 13
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Appendix'
			dw_bktemplate.object.position[i]=13
		case 14
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Glossary'
			dw_bktemplate.object.position[i]=14
		case 15
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Bibliography'
			dw_bktemplate.object.position[i]=15
		case 16
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=li_null
			dw_bktemplate.object.bookelement[i]='Index'
			dw_bktemplate.object.position[i]=16
		case 17
			dw_bktemplate.object.template[i]='default'
			dw_bktemplate.object.navigationlevel[i]=1
			dw_bktemplate.object.bookelement[i]='Closing Announcement'
			dw_bktemplate.object.position[i]=17
	end choose
next
rtn=dw_bktemplate.update()
if rtn=1 then
	commit using sqlservertrans;
else
	rollback using sqlservertrans;
	return -1
end if	
return 1
end function

public function integer wf_update_booknavigation ();long i, j, k, li_cnt, rtn, li_level, li_pos, li_cnt2, li_null, li_cur

string ls_conno=' ', ls_elmt,ls_elmt2, ls_commt, ls_temp, ls_connold,ls_conno2, ls_sort
n_ds lds_bkdata


lds_bkdata=create n_ds
lds_bkdata.dataobject='d_bknvg_forupdate'
ls_temp='default'
setNull(li_null)

dw_bktemplate.settransobject(sqlservertrans)
li_cnt2=dw_bktemplate.Retrieve(ls_temp)
lds_bkdata.settransobject(sqlservertrans)
li_cnt=lds_bkdata.retrieve()
ls_sort=" conno A, bookelement A"
lds_bkdata.SetSort(ls_sort)
lds_bkdata.Sort()
for j=1 to li_cnt// insert fram of table booknavigation
	ls_conno=trim(lds_bkdata.object.conno[j])
	if ls_conno<>ls_connold then
		for i=1 to li_cnt2
			li_cur=dw_bknvg_forupdate.InsertRow(0)
			choose case i
				case 1
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Title/Author'
					dw_bknvg_forupdate.object.position[li_cur]=1
				case 2
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Annotation'
					dw_bknvg_forupdate.object.position[li_cur]=2
				case 3
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Table of Content'
					dw_bknvg_forupdate.object.position[li_cur]=3
				case 4
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Preface'
					dw_bknvg_forupdate.object.position[li_cur]=4
				case 5
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Introduction'
					dw_bknvg_forupdate.object.position[li_cur]=5
				case 6
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Prolog'
					dw_bknvg_forupdate.object.position[li_cur]=6
				case 7
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Part'
					dw_bknvg_forupdate.object.position[li_cur]=7
				case 8
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Chapter'
					dw_bknvg_forupdate.object.position[li_cur]=8
				case 9
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Section'
					dw_bknvg_forupdate.object.position[li_cur]=9	
				case 10
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Subsection'
					dw_bknvg_forupdate.object.position[li_cur]=10
				case 11
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Story'
					dw_bknvg_forupdate.object.position[li_cur]=11
				case 12
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Epilog'
					dw_bknvg_forupdate.object.position[li_cur]=12
				case 13
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Appendix'
					dw_bknvg_forupdate.object.position[li_cur]=13
				case 14
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Glossary'
					dw_bknvg_forupdate.object.position[li_cur]=14
				case 15
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Bibliography'
					dw_bknvg_forupdate.object.position[li_cur]=15
				case 16
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Index'
					dw_bknvg_forupdate.object.position[li_cur]=16
				case 17
					dw_bknvg_forupdate.object.conno[li_cur]=ls_conno
					dw_bknvg_forupdate.object.navigationlevel[li_cur]=li_null
					dw_bknvg_forupdate.object.bookelement[li_cur]='Closing Announcement'
					dw_bknvg_forupdate.object.position[li_cur]=17
			end choose
		next //i=1 to li_cnt2
		ls_connold=ls_conno
	end if	
next// j=t to li_cnt
ls_sort=" conno A, bookelement A"
dw_bknvg_forupdate.SetSort(ls_sort)
dw_bknvg_forupdate.Sort()
li_cnt=lds_bkdata.RowCount()
li_cnt2=dw_bknvg_forupdate.RowCount()
k=1
ls_connold=' '
for j=1 to li_cnt
	ls_conno=trim(lds_bkdata.object.conno[j])
	if j=1 then
		ls_connold=ls_conno
	end if
	ls_elmt=trim(lds_bkdata.object.bookelement[j])
	li_level=lds_bkdata.object.navigationlevel[j]
	ls_commt=trim(lds_bkdata.object.comments[j])
	ls_elmt=left(ls_elmt,3)
	for i= k to li_cnt2
		ls_conno2=trim(dw_bknvg_forupdate.object.conno[i])
		ls_elmt2=trim(dw_bknvg_forupdate.object.bookelement[i])
		ls_elmt2=left(ls_elmt2,3)
		if ls_conno2< ls_conno then
			continue
		elseif ls_conno2=ls_conno and ls_elmt2<ls_elmt then
			continue
		elseif ls_conno2=ls_conno and ls_elmt2=ls_elmt then
			dw_bknvg_forupdate.object.navigationlevel[i]=li_level
			dw_bknvg_forupdate.object.comments[i]=ls_commt
		elseif ls_conno2=ls_conno and ls_elmt2>ls_elmt then
				k=i
			exit
		elseif ls_conno2> ls_conno then
		   k=i
			exit
		end if
	next
next
ls_sort=" conno A, position A"
dw_bknvg_forupdate.SetSort(ls_sort)
dw_bknvg_forupdate.Sort()
li_cnt2=dw_bknvg_forupdate.rowcount()

delete 
from booknavigation
using sqlservertrans;
if f_check_dberror(sqlservertrans, 'delete from booknavigation')=false then
	return -1
end if
rtn=dw_bknvg_forupdate.update()
if rtn=1 then
	commit using sqlservertrans;
else
	rollback using sqlservertrans;
	return -1
end if
messagebox('',string(li_cnt2)+' rows are updated or inserted')
return 1


end function

public function integer wf_insert_element ();long i, li_cnt, j, li_cur,  rtn
string  ls_elemt, ls_null

setNull(ls_null)
delete 
from booknavigationelements
using sqlservertrans;
if f_check_dberror(sqlservertrans, 'insert booknavigationelements')=false then
	return -1
else
	commit using sqlservertrans;
end if
dw_bk_nvgt_elmts.SettransObject(sqlservertrans)
for i=1 to 17
	dw_bk_nvgt_elmts.InsertRow(0)
	choose case i
		case 1
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Title/Author'
			dw_bk_nvgt_elmts.object.position[i]=1
		case 2
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Annotation'
			dw_bk_nvgt_elmts.object.position[i]=2
		case 3
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Table of Content'
			dw_bk_nvgt_elmts.object.position[i]=3
		case 4
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Preface'
			dw_bk_nvgt_elmts.object.position[i]=4
		case 5
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Introduction'
			dw_bk_nvgt_elmts.object.position[i]=5
		case 6
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Prolog'
			dw_bk_nvgt_elmts.object.position[i]=6
		case 7
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Part'
			dw_bk_nvgt_elmts.object.position[i]=7
		case 8
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Chapter'
			dw_bk_nvgt_elmts.object.position[i]=8
		case 9
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Section'
			dw_bk_nvgt_elmts.object.position[i]=9	
		case 10
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Subsection'
			dw_bk_nvgt_elmts.object.position[i]=10
		case 11
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Story'
			dw_bk_nvgt_elmts.object.position[i]=11
		case 12
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Epilog'
			dw_bk_nvgt_elmts.object.position[i]=12
		case 13
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Appendix'
			dw_bk_nvgt_elmts.object.position[i]=13
		case 14
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Glossary'
			dw_bk_nvgt_elmts.object.position[i]=14
		case 15
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Bibliography'
			dw_bk_nvgt_elmts.object.position[i]=15
		case 16
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Index'
			dw_bk_nvgt_elmts.object.position[i]=16
		case 17
			dw_bk_nvgt_elmts.object.description[i]=ls_null
			dw_bk_nvgt_elmts.object.listing[i]='true'
			dw_bk_nvgt_elmts.object.element[i]='Closing Announcement'
			dw_bk_nvgt_elmts.object.position[i]=17
	end choose
next
rtn=dw_bk_nvgt_elmts.update()
if rtn=1 then
	commit using sqlservertrans;
else
	rollback using sqlservertrans;
	return -1
end if	
return 1
end function

on w_book_navigation.create
int iCurrent
call super::create
this.dw_booknavigationall=create dw_booknavigationall
this.dw_bktemplateall=create dw_bktemplateall
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.mle_nav_instr=create mle_nav_instr
this.mle_sitxt=create mle_sitxt
this.mle_alert=create mle_alert
this.dw_bktemp_fordddw=create dw_bktemp_fordddw
this.sle_conno=create sle_conno
this.shl_1=create shl_1
this.shl_2=create shl_2
this.cb_close=create cb_close
this.cb_clear=create cb_clear
this.cb_update=create cb_update
this.cb_temp=create cb_temp
this.cb_add=create cb_add
this.cb_sp=create cb_sp
this.cb_find=create cb_find
this.dw_booknavigation=create dw_booknavigation
this.dw_bk_nvgt_elmts=create dw_bk_nvgt_elmts
this.dw_bknvg_forupdate=create dw_bknvg_forupdate
this.dw_bktemplate=create dw_bktemplate
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_booknavigationall
this.Control[iCurrent+2]=this.dw_bktemplateall
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.mle_nav_instr
this.Control[iCurrent+7]=this.mle_sitxt
this.Control[iCurrent+8]=this.mle_alert
this.Control[iCurrent+9]=this.dw_bktemp_fordddw
this.Control[iCurrent+10]=this.sle_conno
this.Control[iCurrent+11]=this.shl_1
this.Control[iCurrent+12]=this.shl_2
this.Control[iCurrent+13]=this.cb_close
this.Control[iCurrent+14]=this.cb_clear
this.Control[iCurrent+15]=this.cb_update
this.Control[iCurrent+16]=this.cb_temp
this.Control[iCurrent+17]=this.cb_add
this.Control[iCurrent+18]=this.cb_sp
this.Control[iCurrent+19]=this.cb_find
this.Control[iCurrent+20]=this.dw_booknavigation
this.Control[iCurrent+21]=this.dw_bk_nvgt_elmts
this.Control[iCurrent+22]=this.dw_bknvg_forupdate
this.Control[iCurrent+23]=this.dw_bktemplate
end on

on w_book_navigation.destroy
call super::destroy
destroy(this.dw_booknavigationall)
destroy(this.dw_bktemplateall)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.mle_nav_instr)
destroy(this.mle_sitxt)
destroy(this.mle_alert)
destroy(this.dw_bktemp_fordddw)
destroy(this.sle_conno)
destroy(this.shl_1)
destroy(this.shl_2)
destroy(this.cb_close)
destroy(this.cb_clear)
destroy(this.cb_update)
destroy(this.cb_temp)
destroy(this.cb_add)
destroy(this.cb_sp)
destroy(this.cb_find)
destroy(this.dw_booknavigation)
destroy(this.dw_bk_nvgt_elmts)
destroy(this.dw_bknvg_forupdate)
destroy(this.dw_bktemplate)
end on

event pfc_preopen;call super::pfc_preopen;this.windowstate =maximized!
this.of_SetBase(TRUE)
this.inv_base.of_Center()

this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)	
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)

inv_resize.of_Register(cb_add, "scale")
inv_resize.of_Register(mle_alert, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_close, "scale")
inv_resize.of_Register(mle_nav_instr, "scale")
inv_resize.of_Register(mle_sitxt, "scale")
inv_resize.of_Register(cb_find, "scale")
inv_resize.of_Register(cb_sp, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_temp, "scale")
inv_resize.of_Register(shl_2, "scale")
inv_resize.of_Register(shl_1, "scale")
inv_resize.of_Register(st_1, "scale")
inv_resize.of_Register(st_2, "scale")
inv_resize.of_Register(st_3, "scale")
inv_resize.of_Register(sle_conno, "scale")
inv_resize.of_Register(dw_bktemplate, "scale")
inv_resize.of_Register(dw_bk_nvgt_elmts, "scale")
inv_resize.of_Register(dw_booknavigation, "scale")
inv_resize.of_Register(dw_bktemp_fordddw, "scale")
inv_resize.of_Register(dw_bknvg_forupdate, "scale")
//inv_resize.of_Register(dw_bknvg_conno_display, "scale")


end event

event activate;call super::activate;this.PostEvent('pfc_preopen')
end event

event pfc_postopen;call super::pfc_postopen;long li_count,  li_cur, i,j, li_cnt,li_level, li_sqlcode, rtn
string lconno, ls_elemt, ls_temp, ls_yesno, ls_conno

SetNull(lconno)

// Get the control number which was passed in 
lconno=message.stringParm

// Check it out for validation
if len(lconno)>8 then
	ls_yesno=mid(lconno, 9)
	lconno=mid(lconno, 1, 8)
end if
// make some datawindow invisible
dw_bknvg_forupdate.visible=false

if NOT(IsNull(lconno)) and lconno<>"" then
	dw_bktemp_fordddw.SetTransObject(SqlServerTrans)
	dw_bktemp_fordddw.Retrieve()
	sle_conno.text=lconno
	// Click the find button
	cb_find.triggerEvent(clicked!)
	dw_bktemplate.visible=false
	dw_bk_nvgt_elmts.visible=false
	dw_booknavigation.visible=true

	mle_alert.visible=true
	mle_nav_instr.visible=true
	mle_sitxt.visible=true
	cb_sp.enabled=true
	cb_update.enabled=true
	if ls_yesno='noupdate' then
		cb_update.enabled=false
	end if
	cb_temp.enabled=false
	sle_conno.text=lconno
else
	dw_bktemplate.SetTransobject(SqlServerTrans)
	dw_bktemp_fordddw.SetTransObject(SqlServerTrans)
	dw_bktemp_fordddw.Retrieve()
	ls_temp=dw_bktemp_fordddw.object.template[1]
	ls_temp='CDS'
	dw_bktemplate.retrieve(ls_temp)
	dw_bktemplate.visible=true
	cb_add.enabled=true
	dw_bk_nvgt_elmts.visible=false
	dw_booknavigation.visible=false

	mle_alert.visible=false
	mle_nav_instr.visible=false
	mle_sitxt.visible=false
	cb_sp.enabled=false
	cb_update.enabled=false
	cb_temp.enabled=true
	sle_conno.text=''
end if
SetPointer(Arrow!)












end event

type dw_booknavigationall from u_pics_dw within w_book_navigation
boolean visible = false
integer x = 187
integer y = 380
integer width = 69
integer height = 76
integer taborder = 50
string dataobject = "d_booknavigationall"
boolean vscrollbar = false
boolean border = false
end type

type dw_bktemplateall from u_pics_dw within w_book_navigation
boolean visible = false
integer x = 69
integer y = 376
integer width = 69
integer height = 76
integer taborder = 40
string dataobject = "d_bktemplateall"
boolean vscrollbar = false
boolean border = false
end type

type st_3 from statictext within w_book_navigation
integer x = 110
integer y = 1664
integer width = 293
integer height = 96
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Navigation Instruction"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_2 from statictext within w_book_navigation
integer x = 110
integer y = 1568
integer width = 293
integer height = 96
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Special Instruction"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_1 from statictext within w_book_navigation
integer x = 146
integer y = 1504
integer width = 256
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "PCS Alert"
alignment alignment = right!
boolean focusrectangle = false
end type

type mle_nav_instr from uo_mle within w_book_navigation
integer x = 402
integer y = 1664
integer width = 2633
integer height = 96
integer taborder = 70
integer textsize = -8
long backcolor = 134217728
boolean displayonly = true
end type

type mle_sitxt from uo_mle within w_book_navigation
integer x = 402
integer y = 1568
integer width = 2633
integer height = 96
integer taborder = 60
integer textsize = -8
end type

type mle_alert from uo_mle within w_book_navigation
integer x = 402
integer y = 1472
integer width = 2633
integer height = 96
integer taborder = 50
integer textsize = -8
end type

type dw_bktemp_fordddw from u_dw within w_book_navigation
boolean visible = false
integer x = 69
integer y = 264
integer width = 283
integer height = 72
integer taborder = 30
string dataobject = "d_bktemp_fordddw"
boolean vscrollbar = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;//WRITTEN BY SHENGJIANG CHENG
long li_count,  i,j, li_cnt,li_level, li_pos

string  ls_temp, ls_elemt

ls_temp=data
li_cnt=dw_bktemplate.retrieve(ls_temp)
dw_booknavigation.reset()

dw_bktemplate.visible=false
cb_temp.enabled=false
cb_add.enabled=false
dw_bk_nvgt_elmts.visible=false
dw_booknavigation.visible=true

mle_alert.visible=false
mle_nav_instr.visible=false
mle_sitxt.visible=false
cb_sp.enabled=false
cb_update.enabled=false
sle_conno.text=''
end event

event constructor;call super::constructor;this.settransobject(sqlservertrans)
end event

type sle_conno from uo_pics_sle within w_book_navigation
integer x = 69
integer y = 108
integer width = 283
integer height = 72
integer taborder = 20
fontcharset fontcharset = ansi!
end type

type shl_1 from statichyperlink within w_book_navigation
integer x = 69
integer y = 24
integer width = 315
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string pointer = "HyperLink!"
long backcolor = 67108864
string text = "Control No:"
boolean focusrectangle = false
end type

type shl_2 from statichyperlink within w_book_navigation
boolean visible = false
integer x = 69
integer y = 196
integer width = 302
integer height = 72
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string pointer = "HyperLink!"
long backcolor = 67108864
string text = "Template:"
boolean focusrectangle = false
end type

type cb_close from u_cb within w_book_navigation
integer x = 2779
integer y = 1792
integer width = 270
integer height = 64
integer taborder = 60
integer textsize = -7
fontcharset fontcharset = ansi!
string text = "&Close"
end type

event clicked;call super::clicked;//datawindowChild dwc_bktemp
//
//dw_bktemp_fordddw.GetChild('template',dwc_bktemp)
//dwc_bktemp.Reset()
//dw_bknvg_forsort.reset()
//dw_bktemp_fordddw.reset()
//dw_bktemp_moredata.reset()
//dw_booknavigation_data.reset()
//dw_booknavigation_display.reset()
//dw_bknvg_conno_display.reset()
////cb_clear.triggerEvent(clicked!)
//GarbageCollect()
ib_disableclosequery= true
close (Parent)

end event

type cb_clear from u_cb within w_book_navigation
integer x = 2450
integer y = 1792
integer width = 270
integer height = 64
integer taborder = 50
integer textsize = -7
fontcharset fontcharset = ansi!
string text = "C&lear"
end type

event clicked;call super::clicked;long li_count,  i,j, li_cnt,li_level

string  ls_temp, ls_elemt

// Retrieve the new default template 

ls_temp='CDS'
SetPointer(HourGlass!)
dw_bktemplate.visible=True
cb_add.enabled=false
cb_temp.enabled=false
dw_bk_nvgt_elmts.visible=false
dw_booknavigation.visible=false
mle_alert.visible=false
mle_nav_instr.visible=false
mle_sitxt.visible=false

cb_sp.enabled=false
cb_update.enabled=false

sle_conno.text=''
dw_bktemplate.SetTransObject(SqlServerTrans)
dw_bktemplate.Retrieve(ls_temp)
//dw_bktemplate.object.template[1]=ls_temp
//dw_bktemplate.SetItem(1,'template','default')
dw_bktemp_fordddw.Retrieve()
li_cnt=dw_bktemplate.RowCount()
parent.title ='Book Navigation'

SetPointer(Arrow!)
sle_conno.SetFocus()
end event

type cb_update from u_cb within w_book_navigation
integer x = 2085
integer y = 1792
integer width = 288
integer height = 64
integer taborder = 40
integer textsize = -7
fontcharset fontcharset = ansi!
string text = "&Update"
end type

event clicked;call super::clicked;Long li_cur, i,j, li_cnt,li_level, li_len, rtn
String  ls_conno, ls_elemt, ls_99, ls_navinstr, ls_alert, ls_sitxt,ls_template

ls_conno=Trim(sle_conno.text)
ls_99=Mid(ls_conno,3,2)
li_len=Len(ls_conno)
IF ls_99<>'99' OR li_len<>8 THEN
	Messagebox(' ','Invalid control #, You can not update booknavigation level.')
	RETURN
END IF
IF dw_booknavigation.visible =FALSE THEN
	Messagebox('','You can not update booknavigation table now. After you entered conno,'+&
			'~nthen hit the enter key,'+&
				'~n then click update button.')
	RETURN
END IF
SetPointer(hourglass!)

dw_booknavigation.AcceptText()

ls_template = dw_booknavigation.Object.template[1]

rtn=dw_booknavigation.Update()
IF rtn=1 THEN
	COMMIT USING sqlservertrans;
ELSE
	ROLLBACK USING sqlservertrans;
END IF
parent.title ='Book Navigation'
ls_alert=mle_alert.text
ls_sitxt=mle_sitxt.text

IF IsNull(ls_alert)=FALSE AND ls_alert<>'' THEN
	UPDATE mchar
	SET pcs_alert=:ls_alert
	WHERE conno=:ls_conno
	USING sqlservertrans;
	IF f_check_dberror(sqlservertrans,'update mchar set pcs_alert=ls_alert')=FALSE THEN
		RETURN
	ELSE
		COMMIT USING sqlservertrans;
	END IF
END IF

IF IsNull(ls_sitxt)=FALSE AND ls_sitxt<>'' THEN
	SELECT COUNT(*) INTO :rtn
	FROM specinst
	WHERE conno=:ls_conno
	USING sqlservertrans;
	IF rtn=1 THEN
		UPDATE specinst
		SET sitxt=:ls_sitxt
		WHERE conno=:ls_conno
		USING sqlservertrans;
		IF f_check_dberror(sqlservertrans,'update specinst set sitxt=ls_sitxt')=FALSE THEN
			RETURN
		ELSE
			COMMIT USING sqlservertrans;
		END IF
	ELSEIF rtn=0 THEN
		INSERT INTO specinst
		VALUES(:ls_conno, :ls_sitxt)
		USING sqlservertrans;
		IF f_check_dberror(sqlservertrans,'insert into specinst')=FALSE THEN
			RETURN
		ELSE
			COMMIT USING sqlservertrans;
		END IF
	END IF
END IF

// Get the navigation instruction
ls_navinstr= f_build_nav_instruction(ls_conno,ls_template)

// Update mchar with the navigation instruction which was build with the external function call to f_build_nav_instruction
UPDATE mchar
SET nav_instr=:ls_navinstr
WHERE conno=:ls_conno
USING sqlservertrans;
IF f_check_dberror(sqlservertrans,'update mchar set nav_instr=ls_navinstr')=FALSE THEN
	RETURN
ELSE
	COMMIT USING sqlservertrans;
//	dw_bknvg_conno_display.AcceptText() need check later what to do with this statement
END IF
mle_nav_instr.text=ls_navinstr
SetPointer(arrow!)


	
	
	
	
	
end event

type cb_temp from u_cb within w_book_navigation
boolean visible = false
integer x = 549
integer y = 1792
integer width = 439
integer height = 64
integer taborder = 30
integer textsize = -7
fontcharset fontcharset = ansi!
string text = "S&ave as Template"
end type

event clicked;call super::clicked;//WRITTEN BY SHENGJIANG CHENG
long li_count,  li_cur, i,j, li_cnt,li_level, li_len, rtn, li_0or1, li_pos
datawindowChild dwc_temp
n_ds lds_temp
string  ls_temp, ls_elemt, ls_99


SetPointer(HourGlass!)
lds_temp=create n_ds
lds_temp.dataobject='d_bktemplate'
lds_temp.setTransobject(sqlservertrans)
if dw_bktemplate.visible= true then
	li_count=dw_bktemplate.RowCount()
	ls_temp=dw_bktemp_fordddw.object.template[1]
	rtn=messagebox(' ','You want to update existing template or insert new template?'+&
	  '~nIf you want to update, choose Yes; if want to insert, choose No; if you do not want '+&
	  '~nto do neither, choose cancel.',Question!,YesNoCancel!, 1)
	if rtn=1 then
		if ls_temp='default' then
			messagebox(' ','Default is not allowed to be changed.',StopSign!)
			return
		end if
		rtn=dw_bktemplate.update()
		if rtn=1 then
			commit using sqlserverTrans;
		else
			rollback using sqlserverTrans;
		end if
	elseif rtn=2 then
		open(w_bkget_template)
		ls_temp=Message.stringParm
		ls_temp=upper(mid(ls_temp,1,1))+lower(mid(ls_temp,2))
		if ls_temp='Cancel' then return
		rtn=dw_bktemplate.RowsCopy(1, li_count,Primary!, lds_temp,1, Primary!)
		for i=1 to li_count
			lds_temp.setItem(i,'template',ls_temp)
			lds_temp.setitemstatus(i,0, primary!, NewModified!)
		next
		rtn=lds_temp.update()
		if rtn=1 then
		  commit using sqlservertrans;
		  dw_bktemplate.Retrieve(ls_temp)
		else 
		  rollback using sqlservertrans;
		  return
		end if
	elseif rtn=3 then 
		return
	end if
	dw_bktemp_fordddw.GetChild('template',dwc_temp)
	dwc_temp.SetTransobject( SqlserverTrans)
	dwc_temp.Retrieve()
	dw_bktemp_fordddw.Retrieve()
	dw_bktemp_fordddw.object.template[1]=ls_temp
elseif dw_booknavigation.visible then
	rtn=messagebox('','You want to insert new template using booknavigation data of this specific book?',Question!,YesNo!,1)
	if rtn=2 then return
	open(w_bkget_template)
	ls_temp=Message.stringParm
	ls_temp=upper(mid(ls_temp,1,1))+lower(mid(ls_temp,2))
	if ls_temp='Cancel' then return
	li_cnt=dw_booknavigation.RowCount()
	for i=1 to li_cnt
		ls_elemt=dw_booknavigation.object.bookelement[i]
		li_level=dw_booknavigation.object.navigationlevel[i]
		li_pos=dw_booknavigation.object.position[i]
		lds_temp.insertRow(0)
		lds_temp.object.template[i]=ls_temp
		lds_temp.object.bookelement[i]=ls_elemt
		lds_temp.object.navigationlevel[i]=li_level
		lds_temp.object.position[i]=li_pos
	next
	rtn=lds_temp.update()
	if rtn=1 then
	  commit using sqlservertrans;
//	  dw_bktemplate.Retrieve(ls_temp)
	else 
	  rollback using sqlservertrans;
	  return
	end if
	dw_bktemp_fordddw.GetChild('template',dwc_temp)
	dwc_temp.SetTransobject( SqlserverTrans)
	dwc_temp.Retrieve()
	dw_bktemp_fordddw.Retrieve()
end if

parent.title ='Book Navigation'
SetPointer(Arrow!)

end event

type cb_add from u_cb within w_book_navigation
boolean visible = false
integer x = 987
integer y = 1792
integer width = 402
integer height = 64
integer taborder = 20
boolean enabled = false
string text = "&Add Element"
end type

event clicked;call super::clicked;string ls_elmt, ls_char, ls_word, ls_line='', ls_tempold, ls_temp, ls_conno, ls_connold
long li_cnt, i, li_maxpos, li_null, rtn, li_count

SetNull(li_null)
openwithParm(w_bkget_template,'add')
ls_elmt=Message.stringParm
if ls_elmt='Cancel' then
	return
end if
ls_elmt=lower(ls_elmt)
li_cnt=len(ls_elmt)
ls_word=''
for i=1 to li_cnt
	ls_char=mid(ls_elmt,i,1)
	if ls_char<>' ' then
		ls_word=ls_word+ls_char
	elseif ls_char=' ' then
		if ls_word<>'of' and ls_word<> 'and' and ls_word<>'or' and ls_word<> 'is' and ls_word<>'are' then
			ls_word=upper(left(ls_word,1))+mid(ls_word,2)
		end if
		ls_line=ls_line+ls_word+' '
		ls_word=''
	end if
	if i=li_cnt then
		if ls_word<>'of' and ls_word<> 'and' and ls_word<>'or' and ls_word<> 'is' and ls_word<>'are' then
			ls_word=upper(left(ls_word,1))+mid(ls_word,2)
		end if
		ls_line=ls_line+ls_word
	end if
next
ls_elmt=trim(ls_line)
select max(position) into :li_maxpos
from booknavigationtemplates
using sqlservertrans;
if f_check_dberror(sqlservertrans,'select max position from navigationtempates')=false then
	return
end if
dw_bktemplateall.setTransobject(sqlservertrans)
li_cnt=dw_bktemplateall.Retrieve()
for i=1 to li_cnt
	ls_temp=dw_bktemplateall.object.template[i]
	if ls_temp<>ls_tempold then
		dw_bktemplateall.insertRow(i)
		dw_bktemplateall.object.template[i]=ls_temp
		dw_bktemplateall.object.bookelement[i]=ls_elmt
		dw_bktemplateall.object.navigationlevel[i]=li_null
		dw_bktemplateall.object.position[i]=li_maxpos+1
		ls_tempold=ls_temp
		li_cnt++
	end if
next
dw_bktemplateall.Sort()
rtn=dw_bktemplateall.update()
if rtn=1 then 
//	commit using sqlservertrans;
else
	rollback using sqlservertrans;
end if
dw_bk_nvgt_elmts.SetTransObject(sqlservertrans)
dw_bk_nvgt_elmts.Retrieve()
li_cnt=dw_bk_nvgt_elmts.InsertRow(0)
dw_bk_nvgt_elmts.object.element[li_cnt]=ls_elmt
dw_bk_nvgt_elmts.object.position[li_cnt]=li_maxpos +1
dw_bk_nvgt_elmts.object.listing[li_cnt]='true'
rtn=dw_bk_nvgt_elmts.update()
if rtn=1 then 
	commit using sqlservertrans;
else
	rollback using sqlservertrans;
end if
rtn=messagebox('','Would You like to add element to existing books in navigation database?',Question!,YesNo!,2)
ls_connold=' '
if rtn=1 then
	select count(distinct conno) into :li_count
	from booknavigation
	using sqlservertrans;
	if f_check_dberror(sqlservertrans,'select number of conno in navigation table')=false then
		return
	end if
	rtn=messagebox('','This action will result in update of all '+string(li_count)+' books ,continue?',Question!,YesNo!,1)
	if rtn=2 then return
	select max(position) into :li_maxpos
	from booknavigation
	using sqlservertrans;
	if f_check_dberror(sqlservertrans,'select max position from navigation')=false then
		return
	end if
	dw_booknavigationall.SetTransObject(SqlServerTrans)
	li_cnt=dw_booknavigationall.Retrieve()
	for i=1 to li_cnt
		ls_conno=dw_booknavigationall.object.conno[i]
		if ls_conno<>ls_connold then
			dw_booknavigationall.InsertRow(i)
			dw_booknavigationall.object.conno[i]=ls_conno
			dw_booknavigationall.object.bookelement[i]=ls_elmt
			dw_booknavigationall.object.position[i]=li_maxpos+1
			ls_connold=ls_conno
			li_cnt++
		end if
	next
	dw_booknavigationall.Sort()
	rtn=dw_booknavigationall.update()
	if rtn=1 then
		commit using sqlservertrans;
	else
		rollback using sqlservertrans;
		return
	end if
end if// end of rtn=1
	
cb_clear.triggerEvent(clicked!)

end event

type cb_sp from u_cb within w_book_navigation
integer x = 37
integer y = 1792
integer width = 475
integer height = 64
integer taborder = 20
integer textsize = -7
fontcharset fontcharset = ansi!
string text = "&Special Instruction..."
end type

event clicked;call super::clicked;string Lconno, ls_sitxt, ls_connonvg
Lconno = trim(sle_conno.text)
ls_connonvg=lconno+'navigation'
openwithparm(w_special_instruction,ls_connonvg)
select sitxt into :ls_sitxt
from specinst
where conno=:lconno
using sqlservertrans;
if f_check_dberror(sqlservertrans,'select sitxt from specinst')=false then
	return
end if
mle_sitxt.text=ls_sitxt
end event

type cb_find from u_cb within w_book_navigation
integer x = 1719
integer y = 1792
integer width = 306
integer height = 64
integer taborder = 10
integer textsize = -7
fontcharset fontcharset = ansi!
string text = "&Find"
boolean default = true
end type

event clicked;call super::clicked;long li_count,   i,j, li_cnt,li_level, li_len, rtn,li_pos,li_bkseq

string  ls_conno, ls_elemt, ls_99, ls_temp, ls_ttl, ls_dwoname, ls_comments, ls_alert, ls_sitxt,&
		ls_nav_instr, ls_bkmed,ls_template
datawindowChild dwc_bktemp

ls_conno=trim(sle_conno.text)

SetNull(ls_template)

// Make sure they type the control number correctly
ls_99=mid(ls_conno,3,2)
li_len=len(ls_conno)
if ls_99<>'99' or li_len<>8 then
	messagebox(' ','Invalid control number')
	sle_conno.SetFocus()
	return
end if

SetPointer(HourGlass!)

// Make sure control number exist in the database and Title information also exist in TTLINIT table
select count(*) into :li_cnt
from ttlinit a, mchar b
where a.chno =b.chno and
      b.conno =:ls_conno
using SqlServerTrans;
if f_check_dberror(sqlservertrans,'select count(*) from ttlinit')=false then
	return
end if
if li_cnt=0 then 
	messagebox('Error','The control number does not exist in the database.'+&
	 '~nRe enter control number.')
	sle_conno.SetFocus()
	return
end if

// Get the template name
select distinct template 
into :ls_template
from booknavigation
where conno=:ls_conno
using sqlservertrans;

// If template does not exist, assign CDS to the template name
If IsNull(ls_template) or ls_template="" then
	ls_template='CDS'
end if

//messagebox('template',ls_template)
dw_booknavigation.visible=true
cb_add.enabled=false
dw_bktemplate.visible=false
mle_alert.visible=true
mle_nav_instr.visible=true
mle_sitxt.visible=true

cb_sp.enabled=true
cb_update.enabled=true
//cb_temp.enabled=false

// Get the book navigation information
dw_booknavigation.SetTransObject(SqlServerTrans)
dw_booknavigation.Retrieve(ls_conno,ls_template)

li_cnt=dw_booknavigation.RowCount()

// Get the PCS alert information
select pcs_alert into :ls_alert
from mchar
where conno=:ls_conno
using sqlservertrans;
if f_check_dberror(sqlservertrans,'select pcs_alert from mchar')=false then
	return
end if

// Get the special instruction information
select sitxt into :ls_sitxt
from specinst
where conno=:ls_conno
using sqlservertrans;
if f_check_dberror(sqlservertrans,'select sitxt from specinst')=false then
	return
end if

// Get the navigation instruction. If it exist
select nav_instr into :ls_nav_instr
from mchar
where conno=:ls_conno
using sqlservertrans;
if f_check_dberror(sqlservertrans,'select nav_instr from mchar')=false then
	return
end if

mle_alert.text=ls_alert
mle_sitxt.text=ls_sitxt
mle_nav_instr.text=ls_nav_instr
if li_cnt=0 then
	dw_booknavigation.Reset()
	dw_bktemplate.SetTransObject(SqlServerTrans)
	ls_temp='CDS'
	dw_bktemplate.Retrieve(ls_temp)
	li_count=dw_bktemplate.RowCount()
	for i=1 to li_count
		ls_elemt=dw_bktemplate.object.bookelement[i]
		li_level=dw_bktemplate.object.navigationlevel[i]
		li_pos=dw_bktemplate.object.position[i]
		dw_booknavigation.insertRow(0)
		dw_booknavigation.object.conno[i]=ls_conno
		dw_booknavigation.object.bookelement[i]=ls_elemt
		dw_booknavigation.object.navigationlevel[i]=li_level
		dw_booknavigation.object.position[i]=li_pos
		dw_booknavigation.object.template[i]='CDS'
	next
end if

select a.ttl, b.bkmed, b.bkseq 
into :ls_ttl, :ls_bkmed, :li_bkseq
from ttlinit a, mchar b
where a.chno =b.chno and
      b.conno =:ls_conno
using SqlServerTrans;
if f_check_dberror(sqlservertrans,'select ttl from ttlinit')=false then
	return
end if

n_cst_string 	inv_string

ls_ttl =  inv_string.of_GlobalReplace(ls_ttl, '"', "'")
dw_booknavigation.object.t_title.text=ls_ttl
dw_booknavigation.object.t_bkno.text=trim(ls_bkmed)+string(li_bkseq)
parent.title ='Book Navigation'
//ls_comments = wf_get_comments(ls_conno)
//dw_bknvg_conno_display.object.comments[1] = ls_comments
SetPointer(Arrow!)
end event

type dw_booknavigation from u_dw within w_book_navigation
event postclicked ( )
boolean visible = false
integer x = 402
integer width = 2633
integer height = 1472
integer taborder = 40
string dataobject = "d_booknavigation"
end type

event postclicked();int li_null

setNull(li_null)
if ib_changed=false then
	this.SetItem(i_row,'navigationlevel',li_null)
end if
end event

event constructor;call super::constructor;this.settransobject(sqlservertrans)
end event

event itemchanged;call super::itemchanged;string ls_dwoname, ls_data,ls_template
long li_cnt, i, rtn,li_position

ls_dwoname=dwo.name
ls_dwoname=lower(ls_dwoname)
ls_dwoname=trim(ls_dwoname)
ls_data=data
ls_template = this.object.template[row]
li_position = this.object.position[row]
//MessageBox("info","Position = "+string(li_position)+" Template = "+ls_template)
if ls_dwoname='navigationlevel' and ls_template = 'CDS' then
	if li_position >= 29 and li_position <= 35 then
		if data <> '6' then
			MessageBox('ERROR','Non-hierarchal navigation structures can only be level 6.')
			RETURN 2
		end if
	elseif li_position <= 29 then
		if data = '6' then
			MessageBox('ERROR','Navigation structures can not be level 6.')
			RETURN 2
		end if
	end if
	ib_changed=true
else
	ib_changed=true	
end if


end event

event clicked;call super::clicked;
if dwo.name='navigationlevel' then
	ib_changed=false
	i_row=row
	this.postevent('postclicked')
end if
end event

type dw_bk_nvgt_elmts from u_dw within w_book_navigation
boolean visible = false
integer x = 402
integer width = 2633
integer height = 1472
integer taborder = 40
string dataobject = "d_bk_nvgt_elmts"
end type

event constructor;call super::constructor;this.settransobject(sqlservertrans)
end event

type dw_bknvg_forupdate from u_dw within w_book_navigation
event postclicked ( )
integer x = 402
integer width = 2633
integer height = 1472
integer taborder = 50
string dataobject = "d_bknvg_forupdate"
boolean hsplitscroll = true
end type

event postclicked();int li_null

setNull(li_null)
if ib_changed=false then
	this.SetItem(i_row,'navigationlevel',li_null)
end if
end event

event clicked;call super::clicked;
if dwo.name='navigationlevel' then
	ib_changed=false
	i_row=row
	this.postevent('postclicked')
end if
end event

event constructor;call super::constructor;this.settransobject(sqlservertrans)
end event

event itemchanged;call super::itemchanged;string ls_dwoname, ls_data
long li_cnt, i, rtn

ls_dwoname=dwo.name
ls_dwoname=lower(ls_dwoname)
ls_dwoname=trim(ls_dwoname)
ls_data=data
if ls_dwoname='navigationlevel' then
	ib_changed=true
end if


end event

type dw_bktemplate from u_dw within w_book_navigation
event postclicked ( )
boolean visible = false
integer x = 402
integer width = 2633
integer height = 1472
integer taborder = 40
string dataobject = "d_bktemplate"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event postclicked();int li_null

setNull(li_null)
if ib_changed=false then
	this.SetItem(i_row,'navigationlevel',li_null)
end if
end event

event constructor;call super::constructor;this.settransobject(sqlservertrans)
end event

event itemchanged;call super::itemchanged;string ls_dwoname, ls_data
long li_cnt, i, rtn

ls_dwoname=dwo.name
ls_dwoname=lower(ls_dwoname)
ls_dwoname=trim(ls_dwoname)
ls_data=data
if ls_dwoname='navigationlevel' then
	ib_changed=true
end if


end event

event clicked;call super::clicked;
if dwo.name='navigationlevel' then
	ib_changed=false
	i_row=row
	this.postevent('postclicked')
end if
end event


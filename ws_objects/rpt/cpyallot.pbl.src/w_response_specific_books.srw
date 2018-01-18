$PBExportHeader$w_response_specific_books.srw
forward
global type w_response_specific_books from w_response
end type
type dw_inlude_specific_books from u_pics_dw within w_response_specific_books
end type
type cb_ok from u_cb within w_response_specific_books
end type
type cb_cancel from u_cb within w_response_specific_books
end type
end forward

global type w_response_specific_books from w_response
integer x = 878
integer y = 640
integer width = 2103
integer height = 1388
string title = "Include Books for Report"
dw_inlude_specific_books dw_inlude_specific_books
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_response_specific_books w_response_specific_books

on w_response_specific_books.create
int iCurrent
call super::create
this.dw_inlude_specific_books=create dw_inlude_specific_books
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_inlude_specific_books
this.Control[iCurrent+2]=this.cb_ok
this.Control[iCurrent+3]=this.cb_cancel
end on

on w_response_specific_books.destroy
call super::destroy
destroy(this.dw_inlude_specific_books)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;//em_date.SetFocus()


this.of_SetBase(true)
this.inv_base.of_Center()
ib_disableclosequery =true
dw_inlude_specific_books.SetTransobject(SqlServerTrans)
dw_inlude_specific_books.InsertRow(0)
end event

type dw_inlude_specific_books from u_pics_dw within w_response_specific_books
event ue_setcol ( )
event ue_setfocus ( )
integer x = 23
integer y = 28
integer width = 2030
integer height = 1092
integer taborder = 10
string dataobject = "d_inlude_specific_books"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event ue_setcol;long li_row_count
li_row_count=this.RowCount()
this.ScrollToRow(li_row_count)
this.SetRow(li_row_count)
this.SetFocus()
this.SetColumn('bkmed')
end event

event ue_setfocus();long li_cur
li_cur =this.Rowcount()
this.ScrollToRow(li_cur)
this.SetRow(li_cur)
this.SetFocus()
this.SetColumn('bkmed')
end event

event itemchanged;call super::itemchanged;long li_bkno,li_count, li_re, li_cur
string ls_bkmed,ls_date, ls_chno,ls_ttl, ls_cascd
date ld_date
datetime ld_date_dt

if dwo.name='bkseq' then
	li_bkno=long(data)
	ls_bkmed=this.GetItemString(row,'bkmed')
	select  count(*) into :li_count
	from mchar
	where bkseq=:li_bkno and bkmed=:ls_bkmed
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans,'select count from mchar using bkseq,bkmed') then
		return
	end if
	if li_count<=0 then
		messagebox('','This book number was not found in the database.Please check the book'+&
			'~nnumber and retype.')
		return 2
	end if
	select  chno, dsdt,cascd into :ls_chno, :ld_date_dt, :ls_cascd
	from mchar
	where bkseq=:li_bkno and bkmed=:ls_bkmed 
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans,'select chno,dsdt from mchar using bkseq,bkmed') then
		return
	end if
	ld_date=date(ld_date_dt)
	ls_date=string(ld_date,'mm/dd/yyyy')
	if IsNull(ld_date_dt) and ls_cascd='P' then 
		messagebox('','this bkseq is included by default, no need to type it again')
		return 2
	end if
	if  IsDate(ls_date) and (not IsNull(ls_chno)) then
		messagebox(' ','This book was included as part of a prior distribution schedule on '+&
			'~n'+ls_date+'. You can not include this book again.')
		return 2
	end if
	select ttl into :ls_ttl
	from ttlinit
	where chno= :ls_chno
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans,'select ttl from ttlinit using chno') then
		return
	end if
	this.object.ttl[row]=ls_ttl
	li_cur=this.InsertRow(0)
	this.PostEvent ('ue_setfocus')
end if

end event

event rowfocuschanged;call super::rowfocuschanged;//this.ScrollToRow(currentrow)
//this.SetRow(currentrow)
//this.SetFocus()
//this.SetColumn('bkmed')
end event

type cb_ok from u_cb within w_response_specific_books
integer x = 352
integer y = 1160
integer width = 306
integer height = 88
integer taborder = 20
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;long li_row_count,i, li_bkseq, li_re
str_distrib_schedule lstr
string ls_med, ls_repeat

li_re=dw_inlude_specific_books.AcceptText()
if li_re= -1 then
	messagebox('','This book number is not found in the database.'+&
		'~nPlease check the book number and retype.')
	return
end if
li_row_count=dw_inlude_specific_books.RowCount()
for i=1 to li_row_count
	li_bkseq=dw_inlude_specific_books.GetItemNumber(i,'bkseq')
	ls_med=dw_inlude_specific_books.GetItemString(i,'bkmed')
//	ls_repeat=dw_inlude_specific_books.GetItemString(i,'repeat')
// suppose there is no repeating, only adding let ls_reap='Y' means adding
	if not Isnull(li_bkseq) then
		lstr.arraylong[i]= li_bkseq
		lstr.arraymed[i]= ls_med
		lstr.arrayrpt[i]= 'Y'
	elseif i=1 then
		lstr.arraylong[i]= -1
	end if	
next
CloseWithReturn(w_response_specific_books,lstr)
end event

type cb_cancel from u_cb within w_response_specific_books
integer x = 1198
integer y = 1160
integer width = 306
integer height = 88
integer taborder = 30
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;str_distrib_schedule lstr


lstr.arraylong[1]= -1
ib_disableclosequery = TRUE
CloseWithReturn(w_response_specific_books,lstr)

end event


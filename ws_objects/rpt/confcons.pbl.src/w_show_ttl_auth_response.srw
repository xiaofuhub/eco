$PBExportHeader$w_show_ttl_auth_response.srw
forward
global type w_show_ttl_auth_response from w_response
end type
type dw_show_ttl_auth from u_pics_dw within w_show_ttl_auth_response
end type
type cb_ok from u_cb within w_show_ttl_auth_response
end type
end forward

global type w_show_ttl_auth_response from w_response
integer x = 878
integer y = 640
integer width = 2062
integer height = 460
string title = "Title and Author"
dw_show_ttl_auth dw_show_ttl_auth
cb_ok cb_ok
end type
global w_show_ttl_auth_response w_show_ttl_auth_response

on w_show_ttl_auth_response.create
int iCurrent
call super::create
this.dw_show_ttl_auth=create dw_show_ttl_auth
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_show_ttl_auth
this.Control[iCurrent+2]=this.cb_ok
end on

on w_show_ttl_auth_response.destroy
call super::destroy
destroy(this.dw_show_ttl_auth)
destroy(this.cb_ok)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)

END IF


end event

event open;call super::open;//em_date.SetFocus()
string ls_msg, ls_bkmed, ls_bkseq
long li_pos, li_bkseq

this.of_SetBase(true)
this.inv_base.of_Center()
ib_disableclosequery =true

ls_msg=message.StringParm
li_pos=pos(ls_msg,';')
ls_bkmed= left(ls_msg, li_pos - 1 )
ls_bkseq= mid(ls_msg, li_pos + 1)
li_bkseq=long(ls_bkseq)
dw_show_ttl_auth.SetTransobject(SqlServerTrans)
dw_show_ttl_auth.Retrieve(ls_bkmed, li_bkseq)
this.title='Title and Author for book number '+ls_bkmed+string(li_bkseq)
end event

type dw_show_ttl_auth from u_pics_dw within w_show_ttl_auth_response
event ue_setcol ( )
event ue_setfocus ( )
integer x = 23
integer y = 28
integer width = 2016
integer height = 232
integer taborder = 10
string dataobject = "d_show_ttl_auth"
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
	select  chno, dsdt,cascd into :ls_chno, :ld_date, :ls_cascd
	from mchar
	where bkseq=:li_bkno and bkmed=:ls_bkmed 
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans,'select chno,dsdt from mchar using bkseq,bkmed') then
		return
	end if
	
	ls_date=string(ld_date,'mm/dd/yyyy')
	if IsNull(ls_date) and ls_cascd='P' then 
		messagebox('','this bkseq is included by default, no need to type in again')
		return 2
	end if
	if  IsDate(ls_date) and (not IsNull(ls_chno)) then
		messagebox(' ','This book was included as part of a prior distribution schedule on '+&
			'~n'+ls_date+'. You can not to include this book again?')
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

type cb_ok from u_cb within w_show_ttl_auth_response
integer x = 800
integer y = 260
integer width = 229
integer height = 88
integer taborder = 20
fontcharset fontcharset = ansi!
string text = "&OK"
end type

event clicked;call super::clicked;
Close(parent)
end event


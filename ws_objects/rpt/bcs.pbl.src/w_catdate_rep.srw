$PBExportHeader$w_catdate_rep.srw
forward
global type w_catdate_rep from w_final_review
end type
end forward

global type w_catdate_rep from w_final_review
int X=563
int Y=473
boolean TitleBar=true
string Title="Catalog Date Report"
end type
global w_catdate_rep w_catdate_rep

type variables
date catdate
end variables

on w_catdate_rep.create
call w_final_review::create
end on

on w_catdate_rep.destroy
call w_final_review::destroy
end on

type em_fr from w_final_review`em_fr within w_catdate_rep
int Y=397
end type

type cb_cancel from w_final_review`cb_cancel within w_catdate_rep
int X=915
end type

type cb_search from w_final_review`cb_search within w_catdate_rep
boolean Visible=false
boolean Enabled=false
end type

type cb_ok from w_final_review`cb_ok within w_catdate_rep
int X=426
end type

event cb_ok::clicked;catdate = date(em_fr.text)
OpenSheet(w_bcs_cat_rep, w_pics_main, 0, Original!)

end event

type st_2 from w_final_review`st_2 within w_catdate_rep
int X=183
int Y=249
int Width=1404
boolean Visible=false
string Text=""
end type

type st_1 from w_final_review`st_1 within w_catdate_rep
int X=362
int Y=169
int Width=983
string Text="Please enter the Catalog date:"
end type


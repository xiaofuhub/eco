$PBExportHeader$w_shipdate_rep.srw
forward
global type w_shipdate_rep from w_final_review
end type
type st_3 from statictext within w_shipdate_rep
end type
end forward

global type w_shipdate_rep from w_final_review
boolean TitleBar=true
string Title="Ship Date Report"
st_3 st_3
end type
global w_shipdate_rep w_shipdate_rep

type variables
date shipdate
end variables

on w_shipdate_rep.create
int iCurrent
call w_final_review::create
this.st_3=create st_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=st_3
end on

on w_shipdate_rep.destroy
call w_final_review::destroy
destroy(this.st_3)
end on

type em_fr from w_final_review`em_fr within w_shipdate_rep
int Y=393
end type

type cb_cancel from w_final_review`cb_cancel within w_shipdate_rep
int X=915
end type

type cb_search from w_final_review`cb_search within w_shipdate_rep
boolean Visible=false
boolean Enabled=false
end type

type cb_ok from w_final_review`cb_ok within w_shipdate_rep
int X=426
end type

event cb_ok::clicked;shipdate = date(em_fr.text)
OpenSheet(w_sheet_bcs_rep1, w_pics_main, 0, Original!)

end event

type st_2 from w_final_review`st_2 within w_shipdate_rep
int X=179
int Width=1404
string Text="will retrieve all the records prior to this date,"
end type

type st_1 from w_final_review`st_1 within w_shipdate_rep
int Width=1267
string Text="Please enter the Ship date. This Report"
end type

type st_3 from statictext within w_shipdate_rep
int X=330
int Y=273
int Width=1134
int Height=73
boolean Enabled=false
boolean BringToTop=true
string Text="where the stage II date out is NULL."
boolean FocusRectangle=false
long BackColor=79741120
int TextSize=-10
int Weight=700
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type


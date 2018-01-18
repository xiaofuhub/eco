$PBExportHeader$w_pa_get_conno_ri.srw
forward
global type w_pa_get_conno_ri from w_response
end type
type cb_ok from u_cb within w_pa_get_conno_ri
end type
type cb_cancel from u_cb within w_pa_get_conno_ri
end type
type em_conno from u_em within w_pa_get_conno_ri
end type
type st_1 from u_st within w_pa_get_conno_ri
end type
type st_2 from statictext within w_pa_get_conno_ri
end type
end forward

global type w_pa_get_conno_ri from w_response
int X=897
int Y=573
int Width=929
int Height=681
boolean ControlMenu=false
cb_ok cb_ok
cb_cancel cb_cancel
em_conno em_conno
st_1 st_1
st_2 st_2
end type
global w_pa_get_conno_ri w_pa_get_conno_ri

on w_pa_get_conno_ri.create
int iCurrent
call w_response::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_conno=create em_conno
this.st_1=create st_1
this.st_2=create st_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
this.Control[iCurrent+2]=cb_cancel
this.Control[iCurrent+3]=em_conno
this.Control[iCurrent+4]=st_1
this.Control[iCurrent+5]=st_2
end on

on w_pa_get_conno_ri.destroy
call w_response::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_conno)
destroy(this.st_1)
destroy(this.st_2)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;em_conno.SetFocus()
end event

type cb_ok from u_cb within w_pa_get_conno_ri
int X=110
int Y=473
int Width=293
int Height=81
int TabOrder=20
string Text="&OK"
int TextSize=-10
end type

event clicked;call super::clicked;string Lcontr_no,Lricd
long Lbkseq
	
IF ISNULL(em_conno.text) OR em_conno.text = ""  Then
	MessageBox("Error","Invalid control number.")
	em_conno.SetFocus()
	Return
ELSE
	Lcontr_no = em_conno.text
	select bkseq,ricd into :Lbkseq,:Lricd from mchar where conno = :Lcontr_no using sqlservertrans;
	if f_check_dberror(sqlservertrans,"MCHAR") then
		if NOT(IsNull(Lbkseq)) then
			MessageBox("REISSUE ERROR","The control number ~'"+Lcontr_no+"~' has a book number ~'"+string(Lbkseq)+ "~' assinged to it. ~r~nMake sure you select a control number that does not have a book number and reissue flag is set to ~'RI~'.",StopSign!)
			em_conno.SetFocus()
			return
		elseif (IsNull(Lricd) OR Lricd<>'RI') then
			MessageBox("REISSUE ERROR","This control number has not been assigned for reissue.~r~nMake sure you select a control number that does not have a book number and reissue flag is set to ~'RI~'",StopSign!)
			em_conno.SetFocus()
			return
		end if
	end if
	CloseWithReturn(w_pa_get_conno_ri,Lcontr_no)
END IF



end event

type cb_cancel from u_cb within w_pa_get_conno_ri
int X=490
int Y=473
int Width=293
int Height=81
int TabOrder=30
string Text="&Cancel"
int TextSize=-10
end type

event clicked;call super::clicked;close(w_pa_get_conno_ri)



end event

type em_conno from u_em within w_pa_get_conno_ri
int X=517
int Y=293
int Width=334
int TabOrder=10
Alignment Alignment=Center!
string Mask="########"
MaskDataType MaskDataType=StringMask!
string DisplayData=""
double Increment=0
string MinMax=""
int TextSize=-10
int Weight=700
end type

event modified;call super::modified;cb_ok.Enabled = True

end event

type st_1 from u_st within w_pa_get_conno_ri
int X=28
int Y=301
int Width=467
string Text="Control Number:"
int TextSize=-10
end type

type st_2 from statictext within w_pa_get_conno_ri
int X=37
int Y=49
int Width=842
int Height=153
boolean Enabled=false
boolean BringToTop=true
string Text="Please enter a control number which is assigned for reissue."
boolean FocusRectangle=false
long BackColor=79741120
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type


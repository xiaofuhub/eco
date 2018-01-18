$PBExportHeader$w_mdi_quick_select.srw
forward
global type w_mdi_quick_select from Window
end type
type mdi_1 from mdiclient within w_mdi_quick_select
end type
end forward

global type w_mdi_quick_select from Window
int X=307
int Y=173
int Width=2926
int Height=2073
boolean TitleBar=true
string Title="Runtime ReportWriter"
string MenuName="m_quick_select"
long BackColor=276856960
boolean ControlMenu=true
boolean MinBox=true
boolean MaxBox=true
boolean Resizable=true
boolean ToolBarVisible=false
WindowType WindowType=mdihelp!
event ue_postopen pbm_custom01
mdi_1 mdi_1
end type
global w_mdi_quick_select w_mdi_quick_select

type variables
// Variables used to communicate between instances
// of sheets
int dept
string dept_name
end variables

on ue_postopen;//Window w_quick_select is open explicitly to demonstrate that only one
//instance of this window should be opened at any given time
Open(w_quick_select)
end on

event open;// Open the sheet in Maximized mode
this.windowstate = maximized!
This.PostEvent("ue_postopen")

end event

event close;close(w_mdi_quick_select)

end event

on w_mdi_quick_select.create
if this.MenuName = "m_quick_select" then this.MenuID = create m_quick_select
this.mdi_1=create mdi_1
this.Control[]={ this.mdi_1}
end on

on w_mdi_quick_select.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.mdi_1)
end on

type mdi_1 from mdiclient within w_mdi_quick_select
long BackColor=16777215
end type


$PBExportHeader$base_center.srw
$PBExportComments$(PB70Base) - Center all windows
forward
global type base_center from window
end type
end forward

global type base_center from window
integer width = 2706
integer height = 1620
boolean titlebar = true
string title = "Untitled"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
boolean center = true
end type
global base_center base_center

type variables
int ii_NonWorkspaceHeight
constant int ii_NormalFontDiff = 100
end variables

forward prototypes
public function integer f_adjust ()
end prototypes

public function integer f_adjust ();//*-----------------------------------------------------------------*/
//*    f_Adjiust: Adjust window for Large Fonts					   	*/
//*-----------------------------------------------------------------*/
integer li_diff

/*  Adjust for Large Fonts in TitleBar  */
ii_NonWorkspaceHeight = this.Height - WorkSpaceHeight ( ) 
li_diff = ii_NonWorkspaceHeight - ii_NormalFontDiff
If li_diff > 8 Then
	this.Resize ( this.Width, this.Height + li_diff )
End If

Return 1
end function

on base_center.create
end on

on base_center.destroy
end on

event open;//*-----------------------------------------------------------------*/
//*    open: Adjust window for Large Fonts					   	*/
//*-----------------------------------------------------------------*/
f_Adjust ( ) 
end event


$PBExportHeader$u_dw.sru
$PBExportComments$(K90) - PFC Extender Datawindow
forward
global type u_dw from datawindow
end type
end forward

global type u_dw from datawindow
int Width=338
int Height=244
int TabOrder=1
BorderStyle BorderStyle=StyleLowered!
boolean VScrollBar=true
boolean LiveScroll=true
event rbuttonup pbm_dwnrbuttonup
event lbuttonup pbm_lbuttonup
event lbuttondown pbm_lbuttondown
end type
global u_dw u_dw

type variables
Public:
n_cst_dwsrv_rowselection	inv_rowselect

Protected:
boolean	ib_rmbmenu = true
boolean	ib_rmbfocuschange = true
end variables

forward prototypes
public function integer of_getparentwindow (ref window aw_parent)
public function integer of_setrowselect (boolean ab_switch)
end prototypes

public function integer of_getparentwindow (ref window aw_parent);powerobject	lpo_parent

lpo_parent = this.GetParent()

// Loop getting the parent of the object until it is of type window!
do while IsValid (lpo_parent) 
	if lpo_parent.TypeOf() <> window! then
		lpo_parent = lpo_parent.GetParent()
	else
		exit
	end if
loop

if IsNull(lpo_parent) Or not IsValid (lpo_parent) then
	setnull(aw_parent)	
	return -1
end If

aw_parent = lpo_parent
return 1

end function

public function integer of_setrowselect (boolean ab_switch);//Check arguments
If IsNull(ab_switch) Then
	Return -1
End If

IF ab_Switch THEN
	IF IsNull(inv_RowSelect) Or Not IsValid (inv_RowSelect) THEN
		inv_RowSelect = Create n_cst_dwsrv_rowselection
		inv_RowSelect.of_SetRequestor ( this )
		Return 1
	END IF
ELSE 
	IF IsValid (inv_RowSelect) THEN
		Destroy inv_RowSelect
		Return 1
	END IF	
END IF

Return 0
end function

event clicked;// Check arguments
IF IsNull(xpos) or IsNull(ypos) or IsNull(row) or IsNull(dwo) THEN
	Return
END IF

// Notify the RowSelect service that the row has been clicked.
IF IsValid (inv_RowSelect) THEN
	inv_RowSelect.Event pfc_clicked ( xpos, ypos, row, dwo )
END IF
end event

event destructor;of_SetRowSelect ( FALSE ) 
end event

event rbuttondown;long	ll_currow
string	ls_colname
string	ls_curcolname

// Validate arguments
if not ib_rmbfocuschange or IsNull (dwo) or row <= 0 then
	return
end if

// Send notification to row selection service
if IsValid (inv_rowselect) then
	inv_rowselect.event pfc_rbuttondown (xpos, ypos, row, dwo)
end if

if dwo.type <> "column" then
	return
end if

// Perform no action if already over current row/column
ls_colname = dwo.name
ls_curcolname = this.GetColumnName()
ll_currow = this.GetRow()
if (ls_colname = ls_curcolname) and (row = ll_currow) then
	return
end if

// Set row & column
if this.SetRow (row) = 1 then
	this.SetColumn (ls_colname)
end if
end event


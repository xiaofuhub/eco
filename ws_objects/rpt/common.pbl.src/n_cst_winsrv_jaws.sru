$PBExportHeader$n_cst_winsrv_jaws.sru
forward
global type n_cst_winsrv_jaws from n_cst_winsrv
end type
end forward

global type n_cst_winsrv_jaws from n_cst_winsrv
event ue_controlgotfocus ( dragobject adrg_control )
end type
global n_cst_winsrv_jaws n_cst_winsrv_jaws

type variables
string is_lastcontrol
string is_queue
dragobject idrg_control
u_em iem_jaws
dec idec_interval = .3
end variables

forward prototypes
public function integer of_settimer (decimal adec_interval)
public function decimal of_gettimer ()
public function integer of_setrequestor (w_master aw_requestor)
public function integer of_reread ()
public function integer of_read (string as_sentence, boolean ab_wait)
public function integer of_read (string as_sentence)
end prototypes

event ue_controlgotfocus(dragobject adrg_control);//////////////////////////////////////////////////////////////////////////////
//
//	event:	ue_controlgetfocus ()
//
//	Arguments:		
//	adrg_control   Control which just got focus
//
//	Returns:  none
//
//	Description:
//	Display the Jawstag stored in the tag value of the current control
//
//	Note:  The format is Jaws=<Jawstag to be displayed>
//////////////////////////////////////////////////////////////////////////////
//	Author:	Lijun Yang
//
//	Date:		11/15/99
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	11/15/99		L.Y.			Initial creation
//=============================================================================

string			ls_ClassName
string			ls_Jawstag
long 				ll_pos, ll_cpu
datawindow		ldw_control
n_cst_string 	lnv_string

idrg_control = adrg_control
ls_ClassName = adrg_control.Classname ()

//MessageBox ('classname', ls_classname)

Choose Case ls_ClassName
	Case is_lastControl, '', 'u_em'
		Return
End Choose

Choose Case adrg_control.TypeOf()
	Case DataWindow!
		// If control with focus is a datawindow, use current column's header	
		ldw_control = adrg_control
		ls_ClassName = ldw_control.GetColumnName()
		If is_lastControl = ls_ClassName Then Return
		// Check the column header for any Jawstag information.
		ls_JawsTag = ldw_control.Describe (ls_ClassName + "_t.text")
		
		//Remove '(', ')','&','~r', '~n' from the header
		ll_pos = Pos (ls_Jawstag, '(')
		If ll_pos > 0 then ls_JawsTag = left (ls_JawsTag, ll_pos - 1)
		
		ls_JawsTag = lnv_string.of_GlobalReplace (ls_JawsTag, '&', '')
		ls_JawsTag = lnv_string.of_GlobalReplace (ls_JawsTag, '"', '')
		ls_JawsTag = lnv_string.of_GlobalReplace (ls_JawsTag, '~r', ' ')
		ls_JawsTag = lnv_string.of_GlobalReplace (ls_JawsTag, '~n', ' ')
		ls_JawsTag = lnv_string.of_GlobalReplace (ls_JawsTag, '  ', ' ')
		ls_JawsTag = lnv_string.of_GlobalReplace (ls_JawsTag, '  ', ' ')	
	Case else
		// Check the control tag for any Jawstag information.
		ls_Jawstag = lnv_string.of_GetKeyValue (adrg_control.tag, "jaws", ";")
End Choose

is_LastControl = ls_ClassName

//MessageBox ('Jawstag', ls_Jawstag)

Choose Case ls_Jawstag
	Case '', '!', '?'
		Return
End Choose 

//display jaws tag

of_read (ls_Jawstag)

end event

public function integer of_settimer (decimal adec_interval);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  		of_SetTimer
//
//	Access:  		public
//
//	Arguments:		
//	adec_interval	interval for display the Jaws tag value
//
//	Returns:  		Integer
//						1 if it succeeds and -1 if an error occurs.
//
//	Description:  	Set teh time interval to display the Jaws tag value
//
//////////////////////////////////////////////////////////////////////////////
//	Author:	Lijun Yang
//
//	Date:		11/16/99
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	11/16/99		L.Y.			Initial creation
//=============================================================================

If IsNull(adec_interval) Then
	Return -1
End If

idec_interval = adec_interval

Return 1
end function

public function decimal of_gettimer ();//////////////////////////////////////////////////////////////////////////////
//
//	Function:  		of_GetTimer
//
//	Access:  		public
//
//	Arguments:	(none)
//
//	Returns:  		Decimal
//						interval for display the Jaws tag value
//
//	Description:  	Get the time interval to display the Jaws tag value
//
//////////////////////////////////////////////////////////////////////////////
//	Author:	Lijun Yang
//
//	Date:		11/16/99
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	11/16/99		L.Y.			Initial creation
//=============================================================================


Return idec_interval
end function

public function integer of_setrequestor (w_master aw_requestor);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  		of_SetRequestor
//
//	Access:  		public
//
//	Arguments:		
//	aw_requestor	The window requesting this service
//
//	Returns:  		Integer
//						1 if it succeeds and -1 if an error occurs.
//
//	Description:  	Associates a window with this service.
//
//////////////////////////////////////////////////////////////////////////////
//	Author:	Lijun Yang
//
//	Date:		11/16/99
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	11/16/99		L.Y.			Initial creation
//=============================================================================

If IsNull(aw_requestor) Or Not IsValid(aw_requestor) Then
	Return -1
End If

iw_requestor = aw_requestor
iw_requestor.openuserObject (iem_jaws)

//iem_jaws.of_setautoselect (true)
iem_jaws.SetMask (stringmask!, fill ('x', 60))
iem_jaws.TabOrder = 0
iem_jaws.border = false
iem_jaws.displayonly = true
iem_jaws.visible = false
iem_jaws.textcolor = iw_requestor.backcolor
//iem_jaws.textcolor = c.BLACK
iem_jaws.backcolor = iw_requestor.backcolor
iem_jaws.width = iw_requestor.width

Return 1
end function

public function integer of_reread ();//////////////////////////////////////////////////////////////////////////////
//
//	Function:  		of_Reread
//
//	Access:  		public
//
//	Returns:  		Integer
//						1 if it succeeds and -1 if an error occurs.
//
//	Description:  	Reread the tag value in the atcive control
//
//////////////////////////////////////////////////////////////////////////////
//	Author:	Lijun Yang
//
//	Date:		01/26/2000
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	01/26/2000		L.Y.			Initial creation
//=============================================================================

is_lastcontrol = ''
//Appeon start
//Appeon cann't  support call the non-instance object instance variabel.
//If Not IsValid (idrg_control) Then Return c.NO_ACTION
If Not IsValid (idrg_control) Then Return 0
//appeon end
event ue_controlgotfocus (idrg_control)

Return 1
end function

public function integer of_read (string as_sentence, boolean ab_wait);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  		of_read (as_sentence)
//
// Argument:
//	string as_words sentence for Jaws to read
//
//	Access:  		public
//
//	Returns:  		Integer
//						1 if it succeeds and -1 if an error occurs.
//
//	Description:  	the tag value in the atcive control
//
//////////////////////////////////////////////////////////////////////////////
//	Author:	Lijun Yang
//
//	Date:		01/26/2000
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	01/26/2000		L.Y.			Initial creation
//=============================================================================

long ll_cpu
dragobject adrg_control

If ab_wait then
	ll_cpu = cpu() + idec_interval * 1000 + 100
	
	Do While cpu () < ll_cpu
		Yield ()
	Loop
End If

adrg_control = idrg_control

//display jaws tag
iem_jaws.text = as_sentence
iem_jaws.visible = true
iem_jaws.SetFocus ()

//MessageBox('what is readed',iem_jaws.text)

ll_cpu = cpu() + idec_interval * 1000

Do While cpu () < ll_cpu
	Yield ()
Loop

adrg_control.SetFocus ()
idrg_control = adrg_control
iem_jaws.visible = false

Return 1
end function

public function integer of_read (string as_sentence);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  		of_read (as_sentence)
//
// Argument:
//	string as_words sentence for Jaws to read
//
//	Access:  		public
//
//	Returns:  		Integer
//						1 if it succeeds and -1 if an error occurs.
//
//	Description:  	the tag value in the atcive control
//
//////////////////////////////////////////////////////////////////////////////
//	Author:	Lijun Yang
//
//	Date:		01/26/2000
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	01/26/2000		L.Y.			Initial creation
//=============================================================================

Return of_read (as_sentence, false)

end function

on n_cst_winsrv_jaws.create
call super::create
end on

on n_cst_winsrv_jaws.destroy
call super::destroy
end on

event destructor;call super::destructor;If isValid (iem_jaws) then iw_requestor.CloseUserobject (iem_jaws)
end event


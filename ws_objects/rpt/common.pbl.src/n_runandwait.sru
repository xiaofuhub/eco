$PBExportHeader$n_runandwait.sru
$PBExportComments$Functions to allow waiting for executed program to finish
forward
global type n_runandwait from nonvisualobject
end type
type process_information from structure within n_runandwait
end type
type startupinfo from structure within n_runandwait
end type
end forward

type process_information from structure
	unsignedlong		hprocess
	unsignedlong		hthread
	unsignedlong		dwprocessid
	unsignedlong		dwthreadid
end type

type startupinfo from structure
	unsignedlong		cb
	string		lpreserved
	string		lpdesktop
	string		lptitle
	unsignedlong		dwx
	unsignedlong		dwy
	unsignedlong		dwxsize
	unsignedlong		dwysize
	unsignedlong		dwxcountchars
	unsignedlong		dwycountchars
	unsignedlong		dwfillattribute
	unsignedlong		dwflags
	unsignedlong		wshowwindow
	unsignedlong		cbreserved2
	long		lpreserved2
	unsignedlong		hstdinput
	unsignedlong		hstdoutput
	unsignedlong		hstderror
end type

global type n_runandwait from nonvisualobject autoinstantiate
end type

type prototypes
Function Boolean CreateProcessA ( &
	String lpApplicationName, &
	String lpCommandLine, &
	ULong lpProcessAttributes, &
	ULong lpThreadAttributes, &
	Boolean bInheritHandles, &
	ULong dwCreationFlags, &
	ULong lpEnvironment, &
	String lpCurrentDirectory, &
	STARTUPINFO lpStartupInfo, &
	Ref PROCESS_INFORMATION lpProcessInformation &
	) Library "kernel32.dll" alias for "CreateProcessA;Ansi"
	
Function ULong WaitForSingleObject ( &
	ULong hHandle, &
	ULong dwMilliseconds &
	) Library "kernel32.dll"

Function Boolean CloseHandle ( &
	ULong hObject &
	) Library "kernel32.dll"

Function Boolean GetExitCodeProcess ( &
	ULong hProcess, &
	Ref ULong lpExitCode &
	) Library "kernel32.dll"

Function Boolean TerminateProcess ( &
	ULong hProcess, &
	ULong uExitCode &
	) Library "kernel32.dll"

end prototypes

type variables
Boolean ib_terminate
ULong iul_millsecs

CONSTANT ulong INFINITE			= -1

CONSTANT ulong SW_HIDE			= 0
CONSTANT ulong SW_SHOWNORMAL	= 1
CONSTANT ulong SW_NORMAL		= 1
CONSTANT ulong SW_SHOWMINIMIZED	= 2
CONSTANT ulong SW_SHOWMAXIMIZED	= 3
CONSTANT ulong SW_MAXIMIZE		= 3
CONSTANT ulong SW_SHOWNOACTIVATE	= 4
CONSTANT ulong SW_SHOW		= 5
CONSTANT ulong SW_MINIMIZE		= 6
CONSTANT ulong SW_SHOWMINNOACTIVE	= 7
CONSTANT ulong SW_SHOWNA		= 8
CONSTANT ulong SW_RESTORE		= 9
CONSTANT ulong SW_SHOWDEFAULT	= 10
CONSTANT ulong SW_FORCEMINIMIZE	= 11
CONSTANT ulong SW_MAX			= 11

CONSTANT ulong STARTF_USESHOWWINDOW	= 1
CONSTANT ulong STARTF_USESIZE			= 2
CONSTANT ulong STARTF_USEPOSITION		= 4
CONSTANT ulong STARTF_USECOUNTCHARS		= 8
CONSTANT ulong STARTF_USEFILLATTRIBUTE	= 16
CONSTANT ulong STARTF_RUNFULLSCREEN		= 32
CONSTANT ulong STARTF_FORCEONFEEDBACK	= 64
CONSTANT ulong STARTF_FORCEOFFFEEDBACK	= 128
CONSTANT ulong STARTF_USESTDHANDLES		= 256
CONSTANT ulong STARTF_USEHOTKEY		= 512

CONSTANT ulong CREATE_DEFAULT_ERROR_MODE	= 67108864
CONSTANT ulong CREATE_FORCEDOS		= 8192
CONSTANT ulong CREATE_NEW_CONSOLE		= 16
CONSTANT ulong CREATE_NEW_PROCESS_GROUP	= 512
CONSTANT ulong CREATE_NO_WINDOW		= 134217728
CONSTANT ulong CREATE_SEPARATE_WOW_VDM	= 2048
CONSTANT ulong CREATE_SHARED_WOW_VDM	= 4096
CONSTANT ulong CREATE_SUSPENDED		= 4
CONSTANT ulong CREATE_UNICODE_ENVIRONMENT	= 1024
CONSTANT ulong DEBUG_PROCESS			= 1
CONSTANT ulong DEBUG_ONLY_THIS_PROCESS	= 2
CONSTANT ulong DETACHED_PROCESS		= 8

CONSTANT ulong HIGH_PRIORITY_CLASS		= 128
CONSTANT ulong IDLE_PRIORITY_CLASS		= 64
CONSTANT ulong NORMAL_PRIORITY_CLASS		= 32
CONSTANT ulong REALTIME_PRIORITY_CLASS	= 256

CONSTANT ulong WAIT_ABANDONED	= 128
CONSTANT ulong WAIT_COMPLETE		= 0
CONSTANT ulong WAIT_OBJECT_0		= 0
CONSTANT ulong WAIT_TIMEOUT		= 258

end variables

forward prototypes
public function unsignedlong of_run (string as_exepath, windowstate a_windowstate)
public function unsignedlong of_run (string as_exepath, trigevent a_windowstate)
private function unsignedlong of_run (string as_exefullpath, unsignedlong ai_showwindow)
public subroutine of_set_options (boolean ab_terminate, decimal adec_seconds)
end prototypes

public function unsignedlong of_run (string as_exepath, windowstate a_windowstate);// -----------------------------------------------------------------------------
// SCRIPT:     n_runandwait.of_run
//
// PURPOSE:    This function takes the Normal!, Maximized and
//					Minimized! enumerated values and passes the
//             corresponding value to the form of the function
//					that actually does the processing.
//
// ARGUMENTS:  as_exepath		- Path of program to execute
//             a_windowstate	- Show window option
//
// RETURN:		Return code from processing
//
// DATE        PROG/ID		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-----------------------------------------------------
// 07/16/2003	RolandS		Initial Coding
// -----------------------------------------------------------------------------

ulong li_rtn

CHOOSE CASE a_windowstate
	CASE Normal!
		li_rtn = this.of_run(as_exepath, SW_SHOWNORMAL)
	CASE Maximized!
		li_rtn = this.of_run(as_exepath, SW_SHOWMAXIMIZED)
	CASE Minimized!
		li_rtn = this.of_run(as_exepath, SW_SHOWMINIMIZED)
END CHOOSE

Return li_rtn

end function

public function unsignedlong of_run (string as_exepath, trigevent a_windowstate);// -----------------------------------------------------------------------------
// SCRIPT:     n_runandwait.of_run
//
// PURPOSE:    This function takes the Hide! enumerated value and
//             passes SW_HIDE to the form of the function that
//					actually does the processing.
//
// ARGUMENTS:  as_exepath		- Path of program to execute
//             a_windowstate	- Show window option
//
// RETURN:		Return code from processing
//
// DATE        PROG/ID		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-----------------------------------------------------
// 07/16/2003	RolandS		Initial Coding
// -----------------------------------------------------------------------------

ulong li_rtn

CHOOSE CASE a_windowstate
	CASE Hide!
		// run the passed program
		li_rtn = this.of_run(as_exepath, SW_HIDE)
	CASE ELSE
		// valid trigevent but invalid windowstate
		SetNull(li_rtn)
END CHOOSE

Return li_rtn

end function

private function unsignedlong of_run (string as_exefullpath, unsignedlong ai_showwindow);// -----------------------------------------------------------------------------
// SCRIPT:     n_runandwait.of_run
//
// PURPOSE:    This function starts the process and waits for it to
//             finish.  If a timeout period has been set, it
//					optionally can terminate the process.
//
// ARGUMENTS:  as_exefullpath	- Path of program to execute
//             ai_showwindow	- Show window option
//
// RETURN:		Return code of the program or:
//					-1  = Create Process failed
//					258 = Process terminated after timeout
//
// DATE        PROG/ID		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-----------------------------------------------------
// 07/16/2003	RolandS		Initial Coding
// -----------------------------------------------------------------------------

STARTUPINFO lstr_si
PROCESS_INFORMATION lstr_pi
ULong lul_null, lul_CreationFlags, lul_ExitCode, lul_msecs
String ls_null

// initialize arguments
SetNull(lul_null)
SetNull(ls_null)
lstr_si.cb = 72
lstr_si.dwFlags = STARTF_USESHOWWINDOW
lstr_si.wShowWindow = ai_showwindow
lul_CreationFlags = CREATE_NEW_CONSOLE + NORMAL_PRIORITY_CLASS

// create process/thread and execute the passed program
If CreateProcessA(ls_null, as_exefullpath, lul_null, &
			lul_null, False, lul_CreationFlags, lul_null, &
			ls_null, lstr_si, lstr_pi) Then
	// wait for the process to complete
	If iul_millsecs > 0 Then
		// wait until process ends or timeout period expires
		lul_ExitCode = WaitForSingleObject(lstr_pi.hProcess, iul_millsecs)
		// terminate process if not finished
		If ib_terminate And lul_ExitCode = WAIT_TIMEOUT Then
			TerminateProcess(lstr_pi.hProcess, -1)
			lul_ExitCode = WAIT_TIMEOUT
		Else
			// check for exit code
			GetExitCodeProcess(lstr_pi.hProcess, lul_ExitCode)
		End If
	Else
		// wait until process ends
		WaitForSingleObject(lstr_pi.hProcess, INFINITE)
		// check for exit code
		GetExitCodeProcess(lstr_pi.hProcess, lul_ExitCode)
	End If
	// close process and thread handles
	CloseHandle(lstr_pi.hProcess)
	CloseHandle(lstr_pi.hThread)
Else
	// return failure
	lul_ExitCode = -1
End If

Return lul_ExitCode

end function

public subroutine of_set_options (boolean ab_terminate, decimal adec_seconds);// -----------------------------------------------------------------------------
// SCRIPT:     n_runandwait.of_set_options
//
// PURPOSE:    This function sets a timeout period so that it can stop
//             waiting after so many seconds.  It also sets an option
//					to terminate the process if it is still running after
//					the timeout period expires.
//
// ARGUMENTS:  ab_terminate	- Terminate if still running
//             adec_seconds	- Timeout period in seconds
//
// DATE        PROG/ID		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-----------------------------------------------------
// 07/16/2003	RolandS		Initial Coding
// -----------------------------------------------------------------------------

ib_terminate = ab_terminate
iul_millsecs = adec_seconds * 1000

end subroutine

on n_runandwait.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_runandwait.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on


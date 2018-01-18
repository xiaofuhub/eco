$PBExportHeader$uo_external_function.sru
forward
global type uo_external_function from nonvisualobject
end type
end forward

global type uo_external_function from nonvisualobject
end type
global uo_external_function uo_external_function

type prototypes
// Sound
Function boolean sndPlaySoundA (string SoundName, ulong Flags) Library "WINMM.DLL"
Function ulong waveOutGetNumDevs () Library "WINMM.DLL"

FUNCTION ulong FindWindow ( ref string WindowClass, ref string WindowText ) LIBRARY "USER32.DLL" ALIAS FOR "FindWindowA" 

// Get free memory
//Subroutine GlobalMemoryStatus (ref os_memorystatus memorystatus ) Library "KERNEL32.DLL"

// Get module handle
Function ulong GetModuleHandleA(string modname) Library "KERNEL32.DLL"

// Window functions
Function ulong GetWindowTextA(ulong handle, ref string wintext, ulong length) Library "USER32.DLL"
Function ulong GetWindowsDirectoryA (ref string dirtext, ulong textlen) library "KERNEL32.DLL"
Function ulong GetSystemDirectoryA (ref string dirtext, ulong textlen) library "KERNEL32.DLL"
Function ulong FindWindowA( ref string lpClassName, ref string lpWindowName) Library "USER32.DLL"

// User/computer information
function boolean GetUserNameA(ref string  lpBuffer, ref ulong nSize) library "ADVAPI32.DLL"
function boolean GetComputerNameA(ref string  lpBuffer, ref ulong nSize) library "KERNEL32.DLL"

// Get text size
Function boolean SystemParametersInfoA(uint wActon, uint wParam, REF int pvParam, uint fUpdateProfile) Library "USER32.DLL"
Function ulong GetDC(ulong hWnd) Library "USER32.DLL"
Function long ReleaseDC(ulong hWnd, ulong hdcr) Library "USER32.DLL"
//Function boolean GetTextExtentPoint32A(ulong hdcr, string lpString, long nCount, ref os_size size) Library "GDI32.DLL"
Function ulong SelectObject(ulong hdc, ulong hWnd) Library "GDI32.DLL"

// Dialogs
//function long PFC_PrintDlg (ulong hwnd, ref s_printdlgattrib printstruct) library "pbvm80.dll"
end prototypes

type variables
Protected:
string		is_separator
string		is_ClassName[] = {"FNWND380","FNWNS380"}
end variables

forward prototypes
public function unsignedinteger of_findwindow (string as_window_name)
end prototypes

public function unsignedinteger of_findwindow (string as_window_name);ulong		lul_whnd
long 		ll_idx, ll_upper

ll_upper = UpperBound(is_ClassName)

FOR ll_idx = 1 TO ll_upper
	lul_whnd = FindWindowA( is_classname[ll_idx], as_window_name)
	IF lul_whnd > 0 THEN
		EXIT
	END IF
NEXT

//messagebox("window name",as_window_name)

return lul_whnd
end function

event constructor;//
end event

on uo_external_function.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_external_function.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on


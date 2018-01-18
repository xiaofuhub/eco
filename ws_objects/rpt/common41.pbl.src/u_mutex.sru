$PBExportHeader$u_mutex.sru
forward
global type u_mutex from nonvisualobject
end type
end forward

shared variables
string ss_unique_name = &
         "I like to drink orange juice with diet coke"
end variables

global type u_mutex from nonvisualobject autoinstantiate
end type

type prototypes
Function Long GetLastError() Library 'kernel32.dll'
Function ULong CreateMutex(ULong lpsa, Boolean fInitialOwner, String lpszMutexName) Library 'kernel32.dll' Alias for CreateMutexA
end prototypes

type variables

end variables

forward prototypes
public function boolean of_isrunning ()
end prototypes

public function boolean of_isrunning ();String ls_name


// Create the Mutex only when running application from executable.
If Handle(GetApplication()) > 0 Then
	
	// Mutex is named the same as the application with null terminator.
	ls_name = GetApplication().AppName + Char(0)
	
	CreateMutex(0, True, ls_name)
	
	// The GetLastError function determines wheter the mutex was successfully created.
	// The mutex should only be created if a mutex with the same name does not already exist.
	If GetLastError() = 183 Then Return True
End If

Return False
end function

on u_mutex.create
TriggerEvent( this, "constructor" )
end on

on u_mutex.destroy
TriggerEvent( this, "destructor" )
end on


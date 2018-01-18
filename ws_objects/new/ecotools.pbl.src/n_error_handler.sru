$PBExportHeader$n_error_handler.sru
$PBExportComments$Userobject for error checking
forward
global type n_error_handler from nonvisualobject
end type
end forward

global type n_error_handler from nonvisualobject
end type
global n_error_handler n_error_handler

type variables
STRING is_title, is_msg
STRING is_logfilename = "err.log"

end variables

forward prototypes
public function integer of_error ()
public function integer of_error (boolean ab_logtofile)
public function integer of_error (boolean ab_logtofile, icon a_icon)
public function integer of_setvalues (string as_title, string as_msg)
end prototypes

public function integer of_error ();// Call the version with 2 args and defaults for both args...
INTEGER rc

rc = of_error( True, Information!)

RETURN rc


end function

public function integer of_error (boolean ab_logtofile);// Call the version with 2 args and set default for the 2nd arg...
INTEGER rc

rc = of_error( ab_logtofile, Information!)

RETURN rc

end function

public function integer of_error (boolean ab_logtofile, icon a_icon);// Display error msg; log errors to file
LONG ll_filenum
STRING ls_logline

IF is_title = "" THEN is_title = "Problem Notification"
IF is_msg = "" THEN is_msg = "Explanation message not supplied."

// Display message...
MessageBox(is_title,is_msg, a_icon)

// Log to file? ...
IF ab_logtofile THEN
  ll_filenum = FileOpen(is_logfilename, linemode!, Write!) // Appends to file
  IF ll_filenum > 0 THEN
	   ls_logline = String(today()) +" / "+String(Now()) + "~n  " + is_title + "~n  " + is_msg
	 	IF FileWrite(ll_filenum, ls_logline) = 1 THEN
		  FileClose(ll_filenum)
		  RETURN 1
		ELSE
		  FileClose(ll_filenum)
		  RETURN -1
		END IF
	ELSE
		RETURN -1
	END IF
	RETURN -1
ELSE
	RETURN 1	
END IF



end function

public function integer of_setvalues (string as_title, string as_msg);// Store passed values into instance variables for later use...
IF IsNull(as_title) THEN is_title = "<null>" ELSE is_title = as_title
IF IsNull(as_msg) THEN is_msg = "<null>" ELSE is_msg = as_msg

RETURN 1


end function

on n_error_handler.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_error_handler.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on


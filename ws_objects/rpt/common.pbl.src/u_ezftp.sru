$PBExportHeader$u_ezftp.sru
$PBExportComments$EZFTP control
forward
global type u_ezftp from olecustomcontrol
end type
end forward

global type u_ezftp from olecustomcontrol
integer width = 137
integer height = 120
integer taborder = 10
borderstyle borderstyle = stylelowered!
string binarykey = "u_ezftp.udo"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
event nextdirectoryentry ( string filename,  long attributes,  double length )
event transferprogress ( long bytestransferred,  long totalbytes )
end type
global u_ezftp u_ezftp

type variables
int	ii_rc	/* 0: OK, -1: error */
end variables

forward prototypes
private function string uf_errormessage (unsignedlong al_errorno)
public function integer uf_login (string as_host, string as_userid, string as_password)
public subroutine uf_logout ()
public function string uf_get_currentdirectory ()
public subroutine uf_listdirectory ()
public subroutine uf_set_currentdirectory (string as_directory)
public subroutine uf_download (string as_localfile, string as_remotefile, boolean ab_binarymode)
public subroutine uf_upload (string as_localfile, string as_remotefile, boolean ab_binarymode)
end prototypes

event nextdirectoryentry;// attributes

// 0 = pipe, link
// 16 = directory
// 128 = file
end event

private function string uf_errormessage (unsignedlong al_errorno);string	ls_errmsg


CHOOSE CASE al_errorno
	CASE 1000
		ls_errmsg = "General error."
	CASE 1001
		ls_errmsg = "Out of handles."
	CASE 1002
		ls_errmsg = "Timeout. The operation was aborted because the remote system didn't respond."
	CASE 1003
		ls_errmsg = "Extended error."
	CASE 1004
		ls_errmsg = "Internal error."
	CASE 1005
		ls_errmsg = "Invalid URL."
	CASE 1006
		ls_errmsg = "Unrecognized scheme."
	CASE 1007
		ls_errmsg = "Name not resolved. The remote domain name couldn't be resolved."
	CASE 1008
		ls_errmsg = "Protocol not found."
	CASE 1009
		ls_errmsg = "Invalid option."
	CASE 1010
		ls_errmsg = "Bad option length."
	CASE 1011
		ls_errmsg = "Option not settable."
	CASE 1012
		ls_errmsg = "Shutdown."
	CASE 1013
		ls_errmsg = "Incorrect user name. The user name specified in the UserName property was not accepted by the remote system."
	CASE 1014
		ls_errmsg = "Incorrect password. The password specified in the Password property was not accepted by the remote system."
	CASE 1015
		ls_errmsg = "Login failure. The server didn't allow the user name / password combination."
	CASE 1016
		ls_errmsg = "Invalid operation."
	CASE 1017
		ls_errmsg = "Operation cancelled."
	CASE 1018
		ls_errmsg = "Incorrect handle type."
	CASE 1019
		ls_errmsg = "Not local handle."
	CASE 1020
		ls_errmsg = "Not proxy request."
	CASE 1021
		ls_errmsg = "Internet registry value not found."
	CASE 1022
		ls_errmsg = "Bad registry parameter."
	CASE 1023
		ls_errmsg = "No direct access."
	CASE 1027
		ls_errmsg = "Transfer in progress."
	CASE 1028
		ls_errmsg = "FTP connected."
	CASE 1029
		ls_errmsg = "FTP disconnected. The remote system unexpectedly closed the connection."
	CASE ELSE
		ls_errmsg = "Unknown error. Contact your system administrator."
END CHOOSE

ii_rc = -1

RETURN ls_errmsg

end function

public function integer uf_login (string as_host, string as_userid, string as_password);// Login

// Assume succesful connect
ii_rc = 0

This.object.RemoteAddress = as_host
This.object.UserName = as_userid
This.object.Password = as_password

This.object.Connect()

// uf_errormessage will set this return variable to -1
RETURN ii_rc
end function

public subroutine uf_logout ();This.object.Disconnect()
end subroutine

public function string uf_get_currentdirectory ();// Get the current directory on the host,i.e. UNIX pwd

string	ls_getdirectory

ls_getdirectory = This.object.RemoteDirectory

RETURN ls_getdirectory

end function

public subroutine uf_listdirectory ();// Will return list of files on host, i.e UNIX ls
// Triggers the event nextdirectoryentry

This.object.GetDirectory(".")

end subroutine

public subroutine uf_set_currentdirectory (string as_directory);// Set the current directory on the host,i.e. UNIX cd

This.object.RemoteDirectory(as_directory)

end subroutine

public subroutine uf_download (string as_localfile, string as_remotefile, boolean ab_binarymode);SetPointer ( HourGlass! )


This.object.Localfile = as_localfile
This.object.Remotefile = as_remotefile
This.object.Binary = ab_binarymode

This.object.GetFile()
end subroutine

public subroutine uf_upload (string as_localfile, string as_remotefile, boolean ab_binarymode);string ls_txt

SetPointer ( HourGlass! )


This.object.Localfile = as_localfile
This.object.Remotefile = as_remotefile
This.object.Binary = ab_binarymode

This.object.putfile()


end subroutine

event externalexception;string	ls_errmsg

ls_errmsg = This.uf_errormessage(resultcode)

Messagebox("EZFTP", description + "~r~nErrorcode: " + string(resultcode) + ": " + ls_errmsg)

ii_rc = -1


end event

on u_ezftp.create
end on

on u_ezftp.destroy
end on


Start of PowerBuilder Binary Data Section : Do NOT Edit
04u_ezftp.bin 
2700000a00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff00000001000000000000000000000000000000000000000000000000000000004b0647b001c3dba700000003000002000000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000102001affffffff00000002ffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000001b300000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000001001affffffffffffffff000000036580f76711cf781945446cb800005453000000004b0647b001c3dba74b0647b001c3dba7000000000000000000000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000070000001800000000000000010000000200000003000000040000000500000006fffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
26ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000fffe000201056580f76711cf781945446cb80000545300000001fb8f0821101b01640008ed8413c72e2b00000030000001830000000b00000100000000600000010100000068000001020000007000000103000000780000010400000080000001050000008c000001060000009800000107000000a400000108000000b000000109000000bc00000000000000c400000003000100000000000300000320000000030000032000000003000000000000000800000001000000000000000800000001000000000000000800000001000000000000000800000001000000000000000800000001000000000000000b000000000000000b000000000000000100010900000007006e69620000797261000001070000000972657375656d616e0001040000000a00636f6c0069666c610600656c0d00000172000000746f6d656464616500736572000001050000000b6f6d6572696665740300656c0c0000015f000000636f74736f72706b01007370090000015f000000657478650078746e00000102000000097478655f79746e65000108000000090073617000726f77730100006400090000765f00006973726500006e6f005c00640065005700200062000100000000032000000320000000000000000000000000013c0018000000000014023800140238000000f00000000000001ee00000000000001efc00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
14u_ezftp.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point

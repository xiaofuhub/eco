$PBExportHeader$w_pics_retrieve_msg_box.srw
forward
global type w_pics_retrieve_msg_box from w_popup
end type
type sle_retrieve from u_sle within w_pics_retrieve_msg_box
end type
end forward

global type w_pics_retrieve_msg_box from w_popup
integer x = 709
integer y = 740
integer width = 2784
integer height = 164
boolean titlebar = false
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
toolbaralignment toolbaralignment = floating!
sle_retrieve sle_retrieve
end type
global w_pics_retrieve_msg_box w_pics_retrieve_msg_box

on w_pics_retrieve_msg_box.create
int iCurrent
call super::create
this.sle_retrieve=create sle_retrieve
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_retrieve
end on

on w_pics_retrieve_msg_box.destroy
call super::destroy
destroy(this.sle_retrieve)
end on

event open;Integer li_rc

// Allow for pre and post open events to occur
This.Event pfc_preopen()
This.Post Event pfc_postopen()

// Default window title is application title
If Len (This.title) = 0 Then
	If IsValid (gnv_app.iapp_object) Then
		This.title = gnv_app.iapp_object.DisplayName
	End If
End If

// Allow preference service to restore settings if necessary
If IsValid(inv_preference) Then
	If gnv_app.of_IsRegistryAvailable() Then
		If Len(gnv_app.of_GetUserKey())> 0 Then 
			li_rc = inv_preference.of_Restore( &
				gnv_app.of_GetUserKey()+'\'+this.ClassName()+'\Preferences')
		ElseIf IsValid(gnv_app.inv_debug) Then				
			MessageBox ("PowerBuilder Foundation Class Library", "The PFC User Preferences service" +&
							" has been requested but The UserRegistrykey property has not" +&
							" been Set on The application manager Object.~r~n~r~n" + &
  							"Call of_SetRegistryUserKey on The Application Manager" +&
							" to Set The property.", &
							Exclamation!)
		End If
	Else
		If Len(gnv_app.of_GetUserIniFile()) > 0 Then
			li_rc = inv_preference.of_Restore (gnv_app.of_GetUserIniFile(), This.ClassName()+' Preferences')
		ElseIf IsValid(gnv_app.inv_debug) Then		
			MessageBox ("PowerBuilder Class Library", "The PFC User Preferences service" +&
							" has been requested but The UserINIFile property has not" +&
							" been Set on The application manager Object.~r~n~r~n" + &
  							"Call of_SetUserIniFile on The Application Manager" +&
							" to Set The property.", &
							Exclamation!)		
		End If
	End If
End If
IF Message.StringParm <> "" THEN
	sle_retrieve.text = Message.StringParm
END IF
this.of_SetBase(true)
this.inv_base.of_Center()
end event

type sle_retrieve from u_sle within w_pics_retrieve_msg_box
integer x = 37
integer y = 32
integer width = 2706
integer height = 96
integer taborder = 0
integer textsize = -10
integer weight = 700
long textcolor = 255
string text = "Retrieving the record(s), please wait..."
boolean displayonly = true
end type


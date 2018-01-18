$PBExportHeader$w_pics_update_msg_box.srw
forward
global type w_pics_update_msg_box from w_popup
end type
type sle_update from u_sle within w_pics_update_msg_box
end type
end forward

global type w_pics_update_msg_box from w_popup
int X=673
int Y=713
int Width=1948
int Height=165
boolean TitleBar=false
boolean ControlMenu=false
boolean MinBox=false
boolean MaxBox=false
boolean Resizable=false
sle_update sle_update
end type
global w_pics_update_msg_box w_pics_update_msg_box

on w_pics_update_msg_box.create
int iCurrent
call w_popup::create
this.sle_update=create sle_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=sle_update
end on

on w_pics_update_msg_box.destroy
call w_popup::destroy
destroy(this.sle_update)
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
	sle_update.text = Message.StringParm
END IF

end event

type sle_update from u_sle within w_pics_update_msg_box
int X=33
int Y=33
int Width=1875
int Height=93
int TabOrder=0
boolean DisplayOnly=true
string Text="Updating the record(s), Please Wait..."
long TextColor=255
int TextSize=-10
int Weight=700
end type


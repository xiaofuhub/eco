$PBExportHeader$w_change_group.srw
forward
global type w_change_group from window
end type
type st_uid_display from statictext within w_change_group
end type
type st_userid from statictext within w_change_group
end type
type cb_2 from commandbutton within w_change_group
end type
type cb_1 from commandbutton within w_change_group
end type
type st_1 from statictext within w_change_group
end type
type ddlb_newgroup from dropdownlistbox within w_change_group
end type
type st_oldgroup from statictext within w_change_group
end type
type sle_oldgroup from singlelineedit within w_change_group
end type
end forward

global type w_change_group from window
integer x = 1047
integer y = 776
integer width = 1559
integer height = 832
boolean titlebar = true
string title = "Change Group"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 79741120
st_uid_display st_uid_display
st_userid st_userid
cb_2 cb_2
cb_1 cb_1
st_1 st_1
ddlb_newgroup ddlb_newgroup
st_oldgroup st_oldgroup
sle_oldgroup sle_oldgroup
end type
global w_change_group w_change_group

on w_change_group.create
this.st_uid_display=create st_uid_display
this.st_userid=create st_userid
this.cb_2=create cb_2
this.cb_1=create cb_1
this.st_1=create st_1
this.ddlb_newgroup=create ddlb_newgroup
this.st_oldgroup=create st_oldgroup
this.sle_oldgroup=create sle_oldgroup
this.Control[]={this.st_uid_display,&
this.st_userid,&
this.cb_2,&
this.cb_1,&
this.st_1,&
this.ddlb_newgroup,&
this.st_oldgroup,&
this.sle_oldgroup}
end on

on w_change_group.destroy
destroy(this.st_uid_display)
destroy(this.st_userid)
destroy(this.cb_2)
destroy(this.cb_1)
destroy(this.st_1)
destroy(this.ddlb_newgroup)
destroy(this.st_oldgroup)
destroy(this.sle_oldgroup)
end on

event open;string ls_old_group, ls_userid

ls_userid = message.stringparm
st_uid_display.text = ls_userid

// Get old group from user table


  SELECT picsuser.group_ 
    INTO :ls_old_group  
    FROM picsuser  
   WHERE picsuser.userid = :ls_userid
	Using sqlservertrans;


sle_oldgroup.text = ls_old_group
end event

type st_uid_display from statictext within w_change_group
integer x = 631
integer y = 44
integer width = 384
integer height = 76
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
boolean focusrectangle = false
end type

type st_userid from statictext within w_change_group
integer x = 201
integer y = 40
integer width = 247
integer height = 76
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Userid:"
boolean focusrectangle = false
end type

type cb_2 from commandbutton within w_change_group
integer x = 791
integer y = 496
integer width = 247
integer height = 108
integer taborder = 21
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Close"
end type

event clicked;close(parent)
end event

type cb_1 from commandbutton within w_change_group
integer x = 421
integer y = 496
integer width = 247
integer height = 108
integer taborder = 10
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&OK"
end type

event clicked; string ls_new_group,ls_userid
 
  ls_new_group = ddlb_newgroup.text 
  ls_userid = st_uid_display.text
 
 
 
 
 IF ls_new_group = "" THEN
	MessageBox("Error","No Group Assigned",stopsign!)
   Return 1
ELSE 
 UPDATE picsuser  
     SET group = :ls_new_group
	  where picsuser.userid = :ls_userid
	  USING sqlservertrans;
END IF


close(parent)

end event

type st_1 from statictext within w_change_group
integer x = 178
integer y = 316
integer width = 270
integer height = 76
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "New Group:"
boolean focusrectangle = false
end type

type ddlb_newgroup from dropdownlistbox within w_change_group
integer x = 631
integer y = 312
integer width = 485
integer height = 328
integer taborder = 30
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean vscrollbar = true
string item[] = {"BCS","PCS","CDS","QAS","ADMIN"}
end type

type st_oldgroup from statictext within w_change_group
integer x = 187
integer y = 168
integer width = 247
integer height = 76
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Group:"
boolean focusrectangle = false
end type

type sle_oldgroup from singlelineedit within w_change_group
integer x = 631
integer y = 160
integer width = 480
integer height = 92
integer taborder = 20
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean autohscroll = false
end type


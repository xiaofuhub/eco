$PBExportHeader$k90_page3.sru
$PBExportComments$(K90) - PFC Extender Step 3:  Specify new prefixes
forward
global type k90_page3 from k90_page
end type
type tv_prefixes from treeview within k90_page3
end type
type st_info from statictext within k90_page3
end type
type cb_otherobj from commandbutton within k90_page3
end type
type st_instruct from statictext within k90_page3
end type
type sle_prefix from singlelineedit within k90_page3
end type
type cb_add from commandbutton within k90_page3
end type
type uo_scroll from k90_promoter within k90_page3
end type
type st_instruct2 from statictext within k90_page3
end type
end forward

global type k90_page3 from k90_page
string tag = "Specify New Extension Prefixes"
event ue_up ( )
event ue_down ( )
tv_prefixes tv_prefixes
st_info st_info
cb_otherobj cb_otherobj
st_instruct st_instruct
sle_prefix sle_prefix
cb_add cb_add
uo_scroll uo_scroll
st_instruct2 st_instruct2
end type
global k90_page3 k90_page3

type variables
/*  When True, User isn't prompted  */
boolean ib_longnamesok=FALSE

/*  When True, 1 prefix added, can't add another  */
boolean ib_prefixadded

/*  The handle of the new prefix item  */
long il_newitem

/*  Holds object prefixes  */
datastore ids_prefixes

/*  Name of object at pfe level   */
string is_pfeobject

/*  Bad state, can't continue  */
boolean ib_continue=TRUE
end variables

forward prototypes
public function boolean of_checkrequired ()
public function integer of_promote (string as_prefix)
public function integer of_demote (string as_prefix)
public function integer of_getnewprefixes (readonly string as_object, readonly string as_library)
public function boolean of_isidentifier (readonly character as_char)
public function boolean of_validateprefix (string as_prefix)
public function integer of_buildtree (string as_prefixes[], string as_newprefix)
end prototypes

event ue_up;//*-------------------------------------------------------------------
//*  Event:				ue_up
//*  Purpose:			Promote prefix up 1 level
//*-------------------------------------------------------------------
treeviewitem ltvi_item 
long ll_item, ll_itemabove

/*  Get the item  */
ll_item = tv_prefixes.FindItem ( CurrentTreeItem!, 0 )
ll_itemabove = ll_item - 1

/*  Get 1 item above.   Can't go higher than top level  */
IF ll_itemabove > 0 THEN
	tv_prefixes.GetItem ( ll_itemabove, ltvi_item ) 
	IF ltvi_item.Label = iw_parent.is_hierarchy[1] THEN
		Beep ( 1 )
		Return 
	END IF
END IF

/*  Go ahead and promote the item  */
IF ll_item > 0 THEN 
	tv_prefixes.GetItem ( ll_item, ltvi_item ) 
	of_Promote ( ltvi_item.Label )
	ib_changed = TRUE
END IF 
end event

event ue_down;//*-------------------------------------------------------------------
//*  Event:				ue_down
//*  Purpose:			Demote prefix down 1 level
//*-------------------------------------------------------------------
treeviewitem ltvi_item 
long ll_item, ll_itembelow

/*  Get the item  */
ll_item = tv_prefixes.FindItem ( CurrentTreeItem!, 0 )

ll_itembelow = ll_item + 1

/*  Get 1 item below.   Can't go lower than pfe  */
IF ll_itembelow > 0  THEN
	IF tv_prefixes.GetItem ( ll_itembelow, ltvi_item ) <> 1 THEN 
		Beep ( 1 ) 
		Return 
	ELSEIF ltvi_item.Label = "(" + is_pfeobject + ")" THEN
		Beep ( 1 )
		Return 
	END IF
END IF

/*  Go ahead and demote the item  */
IF ll_item > 0 THEN 
	tv_prefixes.GetItem ( ll_item, ltvi_item ) 
	of_Demote ( ltvi_item.Label )
	ib_changed = TRUE
END IF 
end event

public function boolean of_checkrequired ();//*-------------------------------------------------------------------
//*  Function:			of_CheckRequired
//*  Purpose:			Did the user enter the new prefix?
//*-------------------------------------------------------------------
Return ( il_newitem > 0 ) 
end function

public function integer of_promote (string as_prefix);//*-------------------------------------------------------------------
//*  Function:			of_Promote
//*  Purpose:			Promote the new prefix
//*-------------------------------------------------------------------
int li_i, li_max, li_j, li_idx
string ls_newlist[], ls_empty[]

SetPointer ( HourGlass! )

li_max = UpperBound ( iw_parent.is_hierarchy )

/*  Find current index of the new prefix  */
FOR li_i = 1 to li_max
	IF iw_parent.is_hierarchy[li_i]  = as_prefix THEN 
		li_idx = li_i
		EXIT
	END IF 
NEXT

/*  Now move it up 1 level  */
IF li_idx > 0 THEN
	FOR li_i = 1 to li_max
		IF iw_parent.is_hierarchy[li_i]  = "pfc" THEN 
			li_j++
			ls_newlist[li_j] = iw_parent.is_hierarchy[li_i]
			Continue
		END IF 
		IF li_i = li_idx - 1 THEN
			li_j++
			ls_newlist[li_j] = as_prefix
			li_j++
			ls_newlist[li_j] = iw_parent.is_hierarchy[li_i]
			li_i++
		ELSE
			li_j++
			ls_newlist[li_j] = iw_parent.is_hierarchy[li_i]
		END IF 
	NEXT
END IF

/*  Reset the hierarchy  */
iw_parent.is_hierarchy = ls_empty 
iw_parent.is_hierarchy = ls_newlist

/*  Rebuild the tree  */
of_BuildTree ( ls_newlist, sle_prefix.Text ) 

Return 1
end function

public function integer of_demote (string as_prefix);//*-------------------------------------------------------------------
//*  Function:			of_Demote
//*  Purpose:			Demote the new prefix
//*-------------------------------------------------------------------
int li_i, li_max, li_j, li_idx
string ls_newlist[], ls_empty[]

SetPointer ( HourGlass! )

li_max = UpperBound ( iw_parent.is_hierarchy )

/*  Find current index of the new prefix  */
FOR li_i = 1 to li_max
	IF iw_parent.is_hierarchy[li_i]  = as_prefix THEN 
		li_idx = li_i
		EXIT
	END IF 
NEXT

/*  Now move it down 1 level  */
IF li_idx > 0 THEN
	FOR li_i = 1 to li_max
		IF iw_parent.is_hierarchy[li_i]  = "pfc" THEN 
			li_j++
			ls_newlist[li_j] = iw_parent.is_hierarchy[li_i]
			Continue
		END IF 
		IF li_i = li_idx THEN
			li_j++
			li_i++
			ls_newlist[li_j] = iw_parent.is_hierarchy[li_i]
			li_j++
			ls_newlist[li_j] = as_prefix
		ELSE
			li_j++
			ls_newlist[li_j] = iw_parent.is_hierarchy[li_i]
		END IF 
	NEXT
END IF

/*  Reset the hierarchy  */
iw_parent.is_hierarchy = ls_empty 
iw_parent.is_hierarchy = ls_newlist

/*  Rebuild the tree  */
of_BuildTree ( ls_newlist, sle_prefix.Text ) 

Return 1
end function

public function integer of_getnewprefixes (readonly string as_object, readonly string as_library);//*-------------------------------------------------------------------
//*  Function:			of_GetNewPrefixes
//*  Purpose:			Given object, determine the hierarchy
//*-------------------------------------------------------------------
long ll_max, ll_i
string ls_prefix, ls_empty[]
int li_i 

iw_parent.is_hierarchy = ls_empty

ids_prefixes.SetFilter ( "sObject = '" + as_object + "' AND sLibrary = '" + as_library + "'" )
ids_prefixes.Filter ( ) 
ids_prefixes.Sort ( )

ll_max = ids_prefixes.RowCount ( ) 

FOR ll_i = 1 to ll_max
	ls_prefix = ids_prefixes.GetItemString ( ll_i, "sPrefix" ) 
	li_i++
	iw_parent.is_hierarchy[li_i] = ls_prefix
NEXT

IF UpperBound ( iw_parent.is_hierarchy ) = 0 THEN 
	f_error ( "Object '" + as_object + "' has no ancestors.  Please choose another.", &
			"k90_page3", "of_GetNewPrefixes", Information!, OK!, 1 )
	Return 0
END IF 

cb_add.Enabled = ( sle_prefix.Text > "" ) 

Return UpperBound ( iw_parent.is_hierarchy )
end function

public function boolean of_isidentifier (readonly character as_char);/*  Is the passed character a valid identifier character  */

CHOOSE CASE as_char
	CASE "a" to "z","_","-","$","%", "0" to "9"
		Return TRUE
	CASE ELSE
		Return FALSE
END CHOOSE
end function

public function boolean of_validateprefix (string as_prefix);int li_c, li_chars, li_p, li_prefixes
char lc_teststring[]

lc_teststring = Lower ( Trim ( as_prefix ) ) 
li_chars = UpperBound ( lc_teststring )
FOR li_c = 1 to li_chars
	IF Not of_IsIdentifier ( lc_teststring[li_c] ) THEN
		MessageBox ( "Prefix Error", "The character " + lc_teststring[li_c] + " is not allowed" + &
			" in PowerBuilder object names.  Please correct prefix name.", Information!, OK!, 1 )
		sle_prefix.SetFocus ( ) 
		Return FALSE
	END IF
NEXT

li_prefixes = UpperBound ( iw_parent.is_hierarchy ) 
FOR li_p = 1 to li_prefixes 
	IF Lower ( Trim ( as_prefix ) ) = iw_parent.is_hierarchy[li_p] THEN
		MessageBox ( "Prefix Error", as_prefix + " is an existing prefix.  Please choose another.", &
			Information!, OK!, 1 ) 
		sle_prefix.SetFocus ( ) 
		Return FALSE
	END IF 
NEXT

IF Not ib_LongNamesOK THEN
	IF Len ( as_prefix ) > 3 THEN 
		IF MessageBox ( "Prefix Error", "This prefix could result in a long filename.  Is it okay" + &
			" to use long filenames?", Information!, YesNo!, 1 ) = 2 THEN
			sle_prefix.SetFocus ( ) 
			Return FALSE
		END IF
	END IF
END IF
	
IF Lower ( Trim ( as_prefix ) ) = "pfe" &
OR Lower ( Trim ( as_prefix ) ) = "pfc" THEN
	MessageBox ( "Prefix Error", as_prefix + " is a reserved prefix.  Please choose another.", Information!, OK! ) 
	sle_prefix.SetFocus ( ) 
	Return FALSE
END IF

//IF Lower ( Trim ( as_prefix ) ) = iw_parent.is_hierarchy[li_p] THEN
//	f_error ( 3, as_prefix + " is an existing prefix.  Please choose another.", &
//		"of_ValidatePrefix", "k90_page3", OK!, 1, "" ) 
//	sle_prefix.SetFocus ( ) 
//	Return FALSE
//END IF 

Return TRUE
end function

public function integer of_buildtree (string as_prefixes[], string as_newprefix);//*-------------------------------------------------------------------
//*  Function:			of_BuildTree
//*  Purpose:			Rebuild the tree based on the changes made
//*-------------------------------------------------------------------
int li_i, li_max
long ll_parent, ll_item
treeviewitem ltvi_item

SetPointer ( HourGlass! )

li_max = UpperBound ( as_prefixes )
 
tv_prefixes.SetReDraw ( FALSE ) 
tv_prefixes.DeleteItem ( 0 ) 

/*  Add all the extension prefixes to the tree  */
FOR li_i = 1 to li_max
	IF as_prefixes[li_i] = as_newprefix THEN
		/*  Show different picture for the new item  */
		ltvi_item.Label = as_prefixes[li_i]
		ltvi_item.Data  = "prefix"
		ltvi_item.Level = 1
		ltvi_item.PictureIndex = 2
		ltvi_item.SelectedPictureIndex = 2
		ltvi_item.Children = False
		ltvi_item.Bold = False
		/*  Insert item at the bottom of the tree  */
		ll_parent = tv_prefixes.InsertItemLast ( ll_parent, ltvi_item )
		il_newitem = ll_parent 
	ELSE
		IF as_prefixes[li_i] = "pfe" THEN
			/*  Existing items  */
			ltvi_item.Label = "(" + is_pfeobject + ")"
			ltvi_item.Data  = "prefix"
			ltvi_item.Level = 1
			ltvi_item.PictureIndex = 1
			ltvi_item.SelectedPictureIndex = 1
			ltvi_item.Children = False
			ltvi_item.Bold = False
			/*  Insert item at the bottom of the tree  */
			ll_parent = tv_prefixes.InsertItemLast ( ll_parent, ltvi_item )
					
		ELSE
			/*  Existing items  */
			ltvi_item.Label = as_prefixes[li_i]
			ltvi_item.Data  = "prefix"
			ltvi_item.Level = 1
			ltvi_item.PictureIndex = 1
			ltvi_item.SelectedPictureIndex = 1
			ltvi_item.Children = False
			ltvi_item.Bold = False
			/*  Insert item at the bottom of the tree  */
			ll_parent = tv_prefixes.InsertItemLast ( ll_parent, ltvi_item )
		END IF
	END IF
NEXT

tv_prefixes.SetReDraw ( TRUE ) 
tv_prefixes.ExpandAll ( 1 ) 
tv_prefixes.SelectItem ( il_newitem ) 

Return 1
end function

event ue_getfocus;call super::ue_getfocus;//*-------------------------------------------------------------------
//*  Event:				ue_getfocus
//*  Purpose:			Insert preset prefixes
//*-------------------------------------------------------------------
string ls_prefix, ls_empty[], ls_library
long ll_found, ll_i, ll_max
int li_i, li_rc

SetPointer ( HourGlass! )

ib_continue = TRUE

IF iw_parent.of_GetPage3Status ( FALSE ) THEN

	iw_parent.SetReDraw ( TRUE )
	iw_parent.is_hierarchy = ls_empty 

	tv_prefixes.DeleteItem ( 0 ) 
	
	/*  Load object hierarchies  */
	li_rc = iw_parent.of_GetPrefixes ( ids_prefixes ) 

	cb_add.Enabled = ( sle_prefix.Text > "" ) 
	IF li_rc = -1 THEN
		iw_parent.of_GetPage3Status ( TRUE )
		ib_continue = FALSE
		ids_prefixes.Reset ( ) 
		st_info.Text = "Unable to determine the hierarchy from 'w_master'.  Please choose another object."
		Return -1 
	END IF

	ids_prefixes.Sort ( )

	ib_prefixadded = FALSE

	/*  Get hierarchy based on w_master  */
	ll_found = ids_prefixes.Find ( "sObject = 'w_master'", 1, ids_prefixes.RowCount ( ) ) 
	IF ll_found > 0 THEN 
		/*  This code prevents a problem if the same object is defined 
			 more than once in different libraries  */
		ls_library = ids_prefixes.GetItemString ( ll_found, "sLibrary" ) 

		ids_prefixes.SetFilter ( "sObject = 'w_master' AND sLibrary = '" + ls_library + "'" )
		ids_prefixes.Filter ( ) 
		ll_max = ids_prefixes.RowCount ( ) 
		FOR ll_i = 1 to ll_max
			ls_prefix = ids_prefixes.GetItemString ( ll_i, "sPrefix" ) 
			IF ls_prefix = "pfe" THEN
				is_pfeobject = ids_prefixes.GetItemString ( ll_i, "sObject" ) 
			END IF
			li_i++
			iw_parent.is_hierarchy[li_i] = ls_prefix
		NEXT

		/*  Add the current hierarchy  */
		of_BuildTree ( iw_parent.is_hierarchy, "" ) 

		st_info.Text = "The current &hierarchy was determined based on 'w_master'" + &
			".~nIf this isn't correct, click 'Use Other Object' and choose another object."

	ELSE

		st_info.Text = "Unable to determine the hierarchy from 'w_master'.  Please choose another object."

	END IF
END IF

ids_prefixes.SetFilter ( "" )
ids_prefixes.Filter ( ) 

iw_parent.Event ue_next ( of_CheckRequired ( ) ) 

Return 1
end event

event ue_losefocus;call super::ue_losefocus;//*-------------------------------------------------------------------
//*  Event:				ue_losefocus
//*  Purpose:			Insert preset prefixes
//*-------------------------------------------------------------------
IF ib_changed THEN
	IF il_newitem > 0 THEN
		iw_parent.of_SetHierarchy ( il_newitem )
	END IF
	ib_changed = FALSE
END IF

iw_parent.Event ue_next ( of_CheckRequired ( ) ) 

Return 1
end event

on k90_page3.create
int iCurrent
call super::create
this.tv_prefixes=create tv_prefixes
this.st_info=create st_info
this.cb_otherobj=create cb_otherobj
this.st_instruct=create st_instruct
this.sle_prefix=create sle_prefix
this.cb_add=create cb_add
this.uo_scroll=create uo_scroll
this.st_instruct2=create st_instruct2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tv_prefixes
this.Control[iCurrent+2]=this.st_info
this.Control[iCurrent+3]=this.cb_otherobj
this.Control[iCurrent+4]=this.st_instruct
this.Control[iCurrent+5]=this.sle_prefix
this.Control[iCurrent+6]=this.cb_add
this.Control[iCurrent+7]=this.uo_scroll
this.Control[iCurrent+8]=this.st_instruct2
end on

on k90_page3.destroy
call super::destroy
destroy(this.tv_prefixes)
destroy(this.st_info)
destroy(this.cb_otherobj)
destroy(this.st_instruct)
destroy(this.sle_prefix)
destroy(this.cb_add)
destroy(this.uo_scroll)
destroy(this.st_instruct2)
end on

event constructor;call super::constructor;//*-------------------------------------------------------------------
//*  Event:				constructor
//*  Purpose:			Initialization
//*-------------------------------------------------------------------
idrg_focus = sle_prefix

end event

type tv_prefixes from treeview within k90_page3
integer x = 41
integer y = 572
integer width = 1390
integer height = 420
integer taborder = 40
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 1090519039
integer accelerator = 105
borderstyle borderstyle = stylelowered!
boolean editlabels = true
boolean linesatroot = true
boolean hideselection = false
string picturename[] = {"Library5!","CreateLibrary5!"}
end type

event selectionchanging;call super::selectionchanging;//*-------------------------------------------------------------------
//*  Event:				selectionchanging
//*  Purpose:			If the new prefix is selected, show scrollbar
//*-------------------------------------------------------------------
treeviewitem ltvi_item 

IF newhandle > 0 THEN 
	this.GetItem ( newhandle, ltvi_item ) 

	IF ltvi_item.PictureIndex = 2 THEN 
		uo_scroll.Move ( uo_scroll.X, this.Y + PointerY ( ) ) 
		uo_scroll.Visible = TRUE 
	ELSE
		uo_scroll.Visible = FALSE
	END IF 

END IF
end event

event beginlabeledit;call super::beginlabeledit;//*-------------------------------------------------------------------
//*  Event:				beginlabeledit
//*  Purpose:			User can only edit the new item
//*-------------------------------------------------------------------
IF handle <> il_newitem THEN
	Return 1
END IF
end event

event endlabeledit;call super::endlabeledit;//*-------------------------------------------------------------------
//*  Event:				endlabeledit
//*  Purpose:			Update the hierarchy with the changed prefix
//*-------------------------------------------------------------------
IF Not of_ValidatePrefix ( newtext ) THEN 
	Return 1
END IF

iw_parent.is_hierarchy[il_newitem] = newtext

sle_prefix.text = newtext
ib_changed = TRUE
end event

event selectionchanged;call super::selectionchanged;//*-------------------------------------------------------------------
//*  Event:				selectionchanged
//*  Purpose:			If the new prefix is selected, show scrollbar
//*-------------------------------------------------------------------
treeviewitem ltvi_item 

IF newhandle > 0 THEN 
	this.GetItem ( newhandle, ltvi_item ) 

	IF ltvi_item.PictureIndex = 2 THEN 
		uo_scroll.Move ( uo_scroll.X, this.Y + ( newhandle * 20 ) ) 
		uo_scroll.Visible = TRUE 
	ELSE
		uo_scroll.Visible = FALSE
	END IF 

ELSE
	uo_scroll.Visible = FALSE
END IF
end event

type st_info from statictext within k90_page3
integer x = 41
integer y = 224
integer width = 1522
integer height = 216
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean focusrectangle = false
end type

type cb_otherobj from commandbutton within k90_page3
integer x = 41
integer y = 468
integer width = 489
integer height = 88
integer taborder = 30
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Use Other Object..."
end type

event clicked;call super::clicked;//*-------------------------------------------------------------------
//*  Event:				clicked
//*  Purpose:			Let user chose another object to base
//*						hierarchy on
//*-------------------------------------------------------------------
int li_parts, li_ObjType, li_prefixes
string ls_Rtn, ls_Object, ls_Parts[], ls_Library

SetPointer ( HourGlass! )

/*  Choose object dialog  */
OpenWithParm ( k90_chooseobjects, ids_prefixes )

/*  User's object choice  */
ls_Rtn = Message.StringParm 
IF ls_Rtn = "" THEN Return

SetPointer ( HourGlass! )

ib_changed = TRUE

/*  Parse out the parts of the return string  */
li_parts = iw_parent.inv_common.f_ParseToArray ( ls_Rtn, "~n", ls_Parts )
IF li_parts >= 1 THEN ls_Object = ls_Parts[1]
IF li_parts >= 2 THEN ls_Library = ls_Parts[2]
IF li_parts >= 3 THEN li_ObjType = Integer ( ls_Parts[3] ) 

/*  Get new hierarchy  */
li_prefixes = of_GetNewPrefixes ( ls_Object, ls_Library ) 

/*  Rebuild the tree  */
tv_prefixes.DeleteItem ( 0 ) 

is_pfeobject = ls_Object

of_BuildTree ( iw_parent.is_hierarchy, "" ) 

st_info.Text = "The current &hierarchy was determined based on '" + &
	ls_Object + "'" + &
	".~nIf this isn't correct, click 'Use Other Object' and choose another object."

ib_prefixadded = FALSE
il_newitem = 0 

tv_prefixes.ExpandAll ( 1 )
end event

type st_instruct from statictext within k90_page3
integer x = 41
integer y = 24
integer width = 1093
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Specify the prefix for the new &extension layer:"
boolean focusrectangle = false
end type

type sle_prefix from singlelineedit within k90_page3
event editchanged pbm_char
integer x = 41
integer y = 96
integer width = 1129
integer height = 88
integer taborder = 10
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
integer accelerator = 101
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

event editchanged;//*-------------------------------------------------------------------
//*  Event:				editchanged
//*  Purpose:			Change the tree item if prefix changes
//*-------------------------------------------------------------------
int li_i, li_max 
string ls_oldname
treeviewitem ltvi_item 

IF Not ib_continue THEN Return 

If Not ib_prefixadded THEN
	cb_add.Enabled = ( this.Text > "" ) 
ELSE 
	IF Not of_ValidatePrefix ( this.Text ) THEN Return 

	tv_prefixes.GetItem ( il_newitem, ltvi_item ) 
	ls_oldname = ltvi_item.Label
	ltvi_item.Label = Lower ( this.Text ) 
	li_max = UpperBound ( iw_parent.is_hierarchy ) 
	FOR li_i = 1 to li_max
		IF iw_parent.is_hierarchy[li_i] = ls_oldname THEN 
			iw_parent.is_hierarchy[li_i] = ltvi_item.Label
			EXIT
		END IF
	NEXT 
		
	tv_prefixes.SetItem ( il_newitem, ltvi_item ) 
END IF

ib_changed = TRUE
end event

event getfocus;//*-----------------------------------------------------------------*/
//*    getfocus:  Highlight all the text on focus
//*-----------------------------------------------------------------*/
this.SelectText ( 1, Len ( this.Text ) ) 
end event

event losefocus;//*-----------------------------------------------------------------*/
//*    losefocus:  Deselects text on lose focus
//*-----------------------------------------------------------------*/
this.SelectText ( 1, 0 ) 
end event

type cb_add from commandbutton within k90_page3
integer x = 1193
integer y = 96
integer width = 329
integer height = 88
integer taborder = 20
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "A&dd"
end type

event clicked;call super::clicked;//*-------------------------------------------------------------------
//*  Event:				clicked
//*  Purpose:			Add the new prefix to the tree
//*-------------------------------------------------------------------
int li_i, li_max, li_j, li_k, li_c, li_chars
string ls_newlist[], ls_empty[], ls_newlist2[]
char lc_teststring[]
boolean lb_added

IF Not of_ValidatePrefix ( sle_prefix.Text ) THEN Return

li_max = UpperBound ( iw_parent.is_hierarchy )

IF li_max <= 0 THEN 
	MessageBox ( "Wait", "Please specify the existing hierarchy before adding the new prefix.", Information!, OK!, 1 )
	Return
END IF

/*  Insert new prefix as second item  */
FOR li_i = 1 to li_max
	IF li_i = 1 THEN
		li_j++
		ls_newlist[li_j] = iw_parent.is_hierarchy[li_i]
		li_j++
		ls_newlist[li_j] = Lower ( sle_prefix.Text )
		lb_added = TRUE
	ELSE
		li_j++
		ls_newlist[li_j] = iw_parent.is_hierarchy[li_i]
	END IF 
NEXT

/*  Reset the hierarchy  */
iw_parent.is_hierarchy = ls_empty 
iw_parent.is_hierarchy = ls_newlist

/*  Rebuild the tree  */
of_BuildTree ( ls_newlist, sle_prefix.Text ) 

ib_prefixadded = TRUE
ib_changed = TRUE
iw_parent.Event ue_next ( of_CheckRequired ( ) ) 
this.Enabled = FALSE
end event

type uo_scroll from k90_promoter within k90_page3
boolean visible = false
integer x = 1449
integer y = 572
integer taborder = 50
end type

on uo_scroll.destroy
call k90_promoter::destroy
end on

type st_instruct2 from statictext within k90_page3
integer x = 41
integer y = 1016
integer width = 1358
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Use the scroll bar to move the new prefix within the h&ierarchy."
boolean focusrectangle = false
end type


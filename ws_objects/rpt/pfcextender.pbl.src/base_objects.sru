$PBExportHeader$base_objects.sru
$PBExportComments$(PB70Base) - List objects in PBL
forward
global type base_objects from datastore
end type
end forward

global type base_objects from datastore
string dataobject = "d_objects"
end type
global base_objects base_objects

forward prototypes
public function long f_populate (readonly string as_libraries[], readonly libdirtype a_type)
end prototypes

public function long f_populate (readonly string as_libraries[], readonly libdirtype a_type);//*-----------------------------------------------------------------*/
//*    f_populate:  Populate the list of objects for passed type
//*-----------------------------------------------------------------*/
long ll_l, ll_libs, ll_start, ll_end, ll_i, ll_last
string ls_libraries[], ls_init[]

this.Reset ( ) 

ll_libs = UpperBound ( as_libraries ) 

Choose Case a_type

	Case DirApplication!
		this.Modify ( "app_pic.Visible=1" )
		
	Case DirDataWindow!
		this.Modify ( "dw_pic.Visible=1" )
		
	Case DirUserObject!
		this.Modify ( "uo_pic.Visible=1" )
		
	Case DirWindow!
		this.Modify ( "win_pic.Visible=1" )
		
End Choose

For ll_l = 1 to ll_libs
	ll_start = this.RowCount ( ) + 1
	this.ImportString ( LibraryDirectory ( as_libraries[ll_l], a_type ) )
	ll_end = this.RowCount ( ) 
	/*  Library has no objects of type  */
	If ll_end < ll_start Then Continue
	ls_libraries = ls_init
	ll_last = ( ll_end - ll_start ) + 1 
	For ll_i = 1 to ll_last
		ls_libraries[ll_i] = as_libraries[ll_l]
	Next 
	this.Object.Library.Current[ll_start, ll_end] = ls_libraries
Next 

this.Sort ( )

Return this.RowCount ( )
end function

on base_objects.create
call super::create
TriggerEvent( this, "constructor" )
end on

on base_objects.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on


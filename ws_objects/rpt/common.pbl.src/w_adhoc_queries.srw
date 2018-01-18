$PBExportHeader$w_adhoc_queries.srw
forward
global type w_adhoc_queries from w_sheet
end type
type st_1 from statictext within w_adhoc_queries
end type
type cb_exit from commandbutton within w_adhoc_queries
end type
type dw_adhocs_queries from u_pics_dw within w_adhoc_queries
end type
end forward

global type w_adhoc_queries from w_sheet
integer width = 3323
integer height = 1892
windowstate windowstate = maximized!
st_1 st_1
cb_exit cb_exit
dw_adhocs_queries dw_adhocs_queries
end type
global w_adhoc_queries w_adhoc_queries

on w_adhoc_queries.create
int iCurrent
call super::create
this.st_1=create st_1
this.cb_exit=create cb_exit
this.dw_adhocs_queries=create dw_adhocs_queries
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.dw_adhocs_queries
end on

on w_adhoc_queries.destroy
call super::destroy
destroy(this.st_1)
destroy(this.cb_exit)
destroy(this.dw_adhocs_queries)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_adhocs_queries, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(st_1, "Scale")


end event

type st_1 from statictext within w_adhoc_queries
integer x = 41
integer y = 1584
integer width = 1134
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 33554432
long backcolor = 67108864
string text = "Select and Double click the report name"
boolean focusrectangle = false
end type

type cb_exit from commandbutton within w_adhoc_queries
integer x = 2816
integer y = 1576
integer width = 402
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;close(parent)
end event

type dw_adhocs_queries from u_pics_dw within w_adhoc_queries
integer x = 32
integer y = 32
integer width = 3191
integer height = 1512
integer taborder = 10
string title = "Adhoc Queries"
string dataobject = "d_adhocs_queries2"
boolean hscrollbar = true
end type

event ue_postconstructor;call super::ue_postconstructor;this.of_SetTransObject(SQLServerTrans)
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

this.retrieve()

end event

event doubleclicked;call super::doubleclicked;string ls_msg,ls_file_location,ls_filename,ls_type, ls_dbname, ls_logid, ls_logpass

ls_type = this.object.type[row]
ls_filename = this.object.filename[row]
ls_file_location = this.object.filelocation[row]

IF ls_type = 'CR' THEN
	// This is a Crystal report
	ls_filename = Mid(ls_filename, 1, Pos(ls_filename, '.') - 1)
	OpenSheetwithparm(w_sheet_pics_ole_crystal,ls_filename,w_pics_main, 0, Original!)
ELSE
	// 10/06/2008 remove hard-coded database instance values
	select upper(name) into :ls_dbname from v$database using sqlservertrans;
	ls_logid = sqlservertrans.logid
	ls_logpass = sqlservertrans.logpass

	ls_msg = 'winsql.exe -d' + ls_dbname + ' -p' + ls_logpass + ' -u' + ls_logid +  ' ' + ls_file_location + '\' + '"' + ls_filename + '"'

	Run(ls_msg, Maximized!)
END IF


end event


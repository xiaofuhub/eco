$PBExportHeader$w_eco_about.srw
$PBExportComments$Extension About window
forward
global type w_eco_about from pfc_w_about
end type
end forward

global type w_eco_about from pfc_w_about
integer x = 795
integer width = 2208
integer height = 1120
end type
global w_eco_about w_eco_about

on w_eco_about.create
call super::create
end on

on w_eco_about.destroy
call super::destroy
end on

type p_about from pfc_w_about`p_about within w_eco_about
end type

type st_application from pfc_w_about`st_application within w_eco_about
integer x = 41
integer y = 756
end type

type st_version from pfc_w_about`st_version within w_eco_about
integer x = 46
integer y = 840
end type

type cb_ok from pfc_w_about`cb_ok within w_eco_about
integer x = 1815
integer y = 908
end type

type st_copyright from pfc_w_about`st_copyright within w_eco_about
integer x = 46
integer y = 928
integer height = 68
end type


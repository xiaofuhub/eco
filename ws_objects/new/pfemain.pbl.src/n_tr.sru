$PBExportHeader$n_tr.sru
$PBExportComments$Extension Transaction class
forward
global type n_tr from pfc_n_tr
end type
end forward

global type n_tr from pfc_n_tr
end type
global n_tr n_tr

type prototypes
FUNCTION integer F_AUTH_ECOUSER(string as_version) RPCFUNC ALIAS FOR "ECO_SECURITY.F_AUTH_ECOUSER"
end prototypes
on n_tr.create
call super::create
end on

on n_tr.destroy
call super::destroy
end on


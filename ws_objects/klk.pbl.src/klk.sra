$PBExportHeader$klk.sra
$PBExportComments$Generated Application Object
forward
global type klk from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global type klk from application
string appname = "klk"
end type
global klk klk

on klk.create
appname = "klk"
message = create message
sqlca = create transaction
sqlda = create dynamicdescriptionarea
sqlsa = create dynamicstagingarea
error = create error
end on

on klk.destroy
destroy( sqlca )
destroy( sqlda )
destroy( sqlsa )
destroy( error )
destroy( message )
end on


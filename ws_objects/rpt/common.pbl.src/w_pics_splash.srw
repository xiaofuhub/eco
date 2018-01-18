$PBExportHeader$w_pics_splash.srw
$PBExportComments$Extension Splash window
forward
global type w_pics_splash from pfc_w_splash
end type
end forward

global type w_pics_splash from pfc_w_splash
integer x = 256
integer y = 400
integer width = 2368
integer height = 860
end type
global w_pics_splash w_pics_splash

on w_pics_splash.create
call super::create
end on

on w_pics_splash.destroy
call super::destroy
end on

type st_copyright from pfc_w_splash`st_copyright within w_pics_splash
integer x = 1385
integer y = 768
integer width = 923
long backcolor = 8421376
end type

type st_version from pfc_w_splash`st_version within w_pics_splash
integer x = 55
integer y = 744
integer width = 1115
long backcolor = 8421376
end type

type st_application from pfc_w_splash`st_application within w_pics_splash
integer x = 50
integer y = 620
integer width = 1641
long backcolor = 8421376
end type

type gb_allaround from pfc_w_splash`gb_allaround within w_pics_splash
integer x = 14
integer y = 12
integer width = 2350
integer height = 844
long backcolor = 8421376
end type

type ln_1 from pfc_w_splash`ln_1 within w_pics_splash
integer beginy = 16
integer endx = 2354
integer endy = 16
end type

type ln_2 from pfc_w_splash`ln_2 within w_pics_splash
integer beginy = 704
integer endy = 704
end type

type ln_3 from pfc_w_splash`ln_3 within w_pics_splash
integer beginx = 1765
integer endx = 1765
end type

type ln_4 from pfc_w_splash`ln_4 within w_pics_splash
end type

type p_splash from pfc_w_splash`p_splash within w_pics_splash
end type


$PBExportHeader$w_sheet_pics_ole_crystal.srw
forward
global type w_sheet_pics_ole_crystal from w_sheet
end type
type ole_crystal from olecustomcontrol within w_sheet_pics_ole_crystal
end type
type cb_print from commandbutton within w_sheet_pics_ole_crystal
end type
type cb_refresh from commandbutton within w_sheet_pics_ole_crystal
end type
type cb_close from commandbutton within w_sheet_pics_ole_crystal
end type
end forward

global type w_sheet_pics_ole_crystal from w_sheet
integer width = 3392
integer height = 2188
string title = "OLE Crystal report"
boolean vscrollbar = true
windowstate windowstate = maximized!
boolean ib_alwaysvalidate = true
ole_crystal ole_crystal
cb_print cb_print
cb_refresh cb_refresh
cb_close cb_close
end type
global w_sheet_pics_ole_crystal w_sheet_pics_ole_crystal

type variables
OLEObject sc1, RptObj,ole_DatabaseTable, ole_ConnectionProperty
boolean oleobj_created=FALSE

end variables

on w_sheet_pics_ole_crystal.create
int iCurrent
call super::create
this.ole_crystal=create ole_crystal
this.cb_print=create cb_print
this.cb_refresh=create cb_refresh
this.cb_close=create cb_close
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.ole_crystal
this.Control[iCurrent+2]=this.cb_print
this.Control[iCurrent+3]=this.cb_refresh
this.Control[iCurrent+4]=this.cb_close
end on

on w_sheet_pics_ole_crystal.destroy
call super::destroy
destroy(this.ole_crystal)
destroy(this.cb_print)
destroy(this.cb_refresh)
destroy(this.cb_close)
end on

event pfc_postopen;call super::pfc_postopen;Integer  li_Return,Lstage,llCount,i
Boolean lb_Error
String rpt_Name,ls_Msg,a_Sql,ls_Userid,ls_Passwrd,ls_Instance, Lconno,ls_Userid2,ls_Passwrd2,ls_Instance2
Date tday,ld_fr,ld_review_date,Ldstg2dt
Long ll_bkseq

IF (IsNull(message.StringParm)=FALSE AND message.StringParm<>"") THEN
	
   	ls_Instance = 'net8pics'
	ls_Userid = 'pcsadmin'
	ls_Passwrd = 'pcsadmin'

   	ls_Instance2 = 'net8web'
	ls_Userid2 = 'picadmin'
	ls_Passwrd2 = 'picadmin'


	rpt_Name = message.StringParm

	//Connect to the object using its Program ID and check that the connection was established:
	
	sc1 = CREATE OleObject
	
	li_Return = sc1.ConnectToNewObject("CrystalRuntime.Application.10")

	IF li_Return < 0 THEN
		CHOOSE CASE li_Return
			CASE -1
				ls_Msg = "Invalid Call: the argument is the Object property of a control"
			CASE -2
				ls_Msg = "Class name not found"
			CASE -3
				ls_Msg = "Object could not be created"
			CASE -4
				ls_Msg = "Could not connect to object"
			CASE -9
				ls_Msg = "Other error"
			CASE -15
				ls_Msg = "MTS is not loaded on this computer"
			CASE -16
				ls_Msg = "Invalid Call: this function not applicable"
			CASE ELSE
				ls_Msg = "Other error"
		END CHOOSE
	   
		Messagebox("Connecting to CRP Object", "Error: " + String(li_Return) + " " +ls_Msg)
	      DESTROY sc1
		oleobj_Created = FALSE
		RETURN 
	ELSE
		RptObj = CREATE OleObject		
	END IF

	
//	// Open report
//	ls_Msg = "p:\reports\"+rpt_Name+".rpt"
////	TRY
//		RptObj = sc1.OpenReport(ls_Msg, 1)
////	CATCH (OleRuntimeError ex)
// //   	lb_Error = TRUE
//  //  	Messagebox('Error', ex.GetMessage()+" Report name = "+ls_Msg)
//	//FINALLY
//    	IF lb_Error THEN
//        DESTROY RptObj
//        DESTROY sc1
//		  oleobj_Created = FALSE
//        RETURN
//    	END IF
////	END TRY

	// Appeon implementation
	string ls_type,ls_path
	ls_type = AppeonGetClienttype()
	if ls_type = 'PB' Then
   	         ls_Msg = "p:\reports\"+rpt_Name+".rpt"
			 TRY
				  RptObj = sc1.OpenReport(ls_Msg, 1)
				 CATCH (OleRuntimeError ex)
						  lb_Error = TRUE
						  Messagebox('Error', ex.GetMessage()+" Report name = "+ls_Msg)
				 FINALLY
					  IF lb_Error THEN
							  DESTROY RptObj
							  DESTROY sc1
							 oleobj_Created = FALSE
							  RETURN
					  END IF
			 END TRY
	Else
//            ls_Msg = Appeongetcachedir() +'\plugin\'+rpt_Name+".rpt"
            ls_Msg = Appeongetcachedir() +'\plugin\'+rpt_Name+".rpt" // virtual directory 'reports' on IIS server
            RptObj = sc1.OpenReport(ls_Msg, 1)
	End if
	/////////////////// appeon implementation
 
 	//get rid of any data saved with report
	RptObj.discardsaveddata()
	
	
	 //	this.ole_crystal.object.ReportSource(RptObj)
   	 this.ole_crystal.object.reportsource = RptObj
		 
	 //set properties of Crystal Viewer control
//	 this.ole_crystal.object.enableexportbutton    = TRUE
//	 this.ole_crystal.object.enablePrintButton     = TRUE
//	 this.ole_crystal.object.enableprogresscontrol = TRUE
//	 this.ole_crystal.object.enablesearchcontrol   = TRUE
//	 this.ole_crystal.object.enablestopbutton      = TRUE
//	 this.ole_crystal.object.enablezoomcontrol     = TRUE
//	 this.ole_crystal.object.displaytoolbar        = TRUE
//	 this.ole_crystal.object.displaytabs           = TRUE
//	 this.ole_crystal.object.enableGroupTree       = TRUE
//	 this.ole_crystal.object.enablePopUpMenu       = TRUE
//	 this.ole_crystal.object.enableSelectExpertButton = TRUE
//
// appeon - CONTROL PROPERTIES SETTINGS like print button etc for olecustomcontrol
	this.ole_crystal.object.DisplayBorder             = TRUE
	this.ole_crystal.object.DisplayGroupTree          = TRUE
	this.ole_crystal.object.DisplayTabs               = TRUE
	this.ole_crystal.object.DisplayToolbar            = TRUE
	this.ole_crystal.object.EnableAnimationCtrl       = TRUE
	this.ole_crystal.object.EnableCloseButton         = TRUE
	this.ole_crystal.object.EnableDrillDown           = TRUE
	this.ole_crystal.object.EnableExportButton        = TRUE
	this.ole_crystal.object.EnableGroupTree           = TRUE
	this.ole_crystal.object.EnableNavigationControls  = TRUE
	this.ole_crystal.object.EnablePrintButton         = TRUE
	this.ole_crystal.object.EnableProgressControl     = TRUE
	this.ole_crystal.object.EnableRefreshButton       = TRUE
	this.ole_crystal.object.EnableSearchControl       = FALSE
	this.ole_crystal.object.EnableSearchExpertButton  = FALSE
	this.ole_crystal.object.EnableSelectExpertButton  = FALSE
	this.ole_crystal.object.EnableStopButton          = TRUE
	this.ole_crystal.object.EnableToolbar             = TRUE
	this.ole_crystal.object.EnableZoomControl         = TRUE
// appeon
	
	tday = Today()

	select max(fr) 
	into :ld_fr
	from mchar
	using sqlservertrans;

	Oleobject lo_parameter
	lo_parameter = create oleobject

	rpt_Name = Lower(rpt_Name)
	IF rpt_Name = 'finalreview' THEN
		IF IsValid(w_sheet_final_review) THEN
			ld_fr = date(w_sheet_final_review.dw_final_approval.object.fr[1])
//			RptObj.ParameterFields.add.item[1].AddCurrentValue (ld_fr)
			lo_parameter = RptObj.ParameterFields.add.item[1]
			lo_parameter.AddCurrentValue(ld_fr)

		ELSE
// 			RptObj.ParameterFields.add.item[1].AddCurrentValue (tday)
			lo_parameter = RptObj.ParameterFields.add.item[1]
			lo_parameter.AddCurrentValue(tday)

		END IF
	ELSEIF rpt_Name = 'dtb_qas' THEN
		IF IsValid(w_qa_zedval_summary) THEN
			ld_review_date = date(w_qa_zedval_summary.ld_review_date)
//			RptObj.ParameterFields.add.item[1].AddCurrentValue (ld_review_date)
			lo_parameter = RptObj.ParameterFields.add.item[1]
			lo_parameter.AddCurrentValue(ld_review_date)
		END IF
	ELSEIF rpt_Name = 'dtb_zedval_nlsval' THEN
		IF IsValid(w_qa_zedval_reports) THEN
			ll_bkseq = w_qa_zedval_reports.ll_bkseq
//			RptObj.ParameterFields.add.item[1].AddCurrentValue (ll_bkseq)
			lo_parameter = RptObj.ParameterFields.add.item[1]
			lo_parameter.AddCurrentValue(ll_bkseq)
		END IF
	ELSEIF rpt_Name = 'stage1annotationreport' THEN
// 		RptObj.ParameterFields.add.item[1].AddCurrentValue (ld_fr)
			lo_parameter = RptObj.ParameterFields.add.item[1]
			lo_parameter.AddCurrentValue(ld_fr)
	ELSEIF rpt_Name = 'stage1exception' THEN
// 		RptObj.ParameterFields.add.item[1].AddCurrentValue (ld_fr)
		lo_parameter = RptObj.ParameterFields.add.item[1]
		lo_parameter.AddCurrentValue(ld_fr)
	ELSEIF rpt_Name = 'bcsindividualcontrolreport' THEN
		IF IsValid(w_sheet_bcs_stage1) THEN
			Lconno = w_Sheet_bcs_stage1.pass_Conno
//			RptObj.ParameterFields.add.item[1].AddCurrentValue (Lconno)
			lo_parameter = RptObj.ParameterFields.add.item[1]
			lo_parameter.AddCurrentValue(lconno)
		END IF
	ELSEIF rpt_Name = 'stage2list' THEN
		IF IsValid(w_sheet_bcs_stage2) THEN
			Ldstg2dt = w_sheet_bcs_stage2.Ldstg2dt

//			RptObj.ParameterFields.add.item[1].AddCurrentValue (Ldstg2dt)
			lo_parameter = RptObj.ParameterFields.add.item[1]
			lo_parameter.AddCurrentValue(ldstg2dt)
		END IF
	ELSEIF rpt_Name = 'trackerdetail' OR rpt_Name = 'tracker' THEN
		ls_Instance = 'tracker'
		ls_Userid = 'trkadmin'
		ls_Passwrd = 'trkadmin'
	ELSEIF rpt_name = 'editannotation' THEN
		IF IsValid(w_sheet_annotation) THEN
			Lconno = w_sheet_annotation.em_conno.text
			Lstage = w_sheet_annotation.anno_stage
			choose case Lstage
				case 1
// 					RptObj.ParameterFields.add.item[1].AddCurrentValue ('Add Annotation')
					lo_parameter = RptObj.ParameterFields.add.item[1]
					lo_parameter.AddCurrentValue('Add Annotation')
				case 2
// 					RptObj.ParameterFields.add.item[1].AddCurrentValue ('CDS Edit Annotation')
					lo_parameter = RptObj.ParameterFields.add.item[1]
					lo_parameter.AddCurrentValue('CDS Edit Annotation')
				case 3
// 					RptObj.ParameterFields.add.item[1].AddCurrentValue ('PMS Edit Annotation')
					lo_parameter = RptObj.ParameterFields.add.item[1]
					lo_parameter.AddCurrentValue('PMS Edit Annotation')
				case 4
// 					RptObj.ParameterFields.add.item[1].AddCurrentValue ('CDS Late Spinoff')
					lo_parameter = RptObj.ParameterFields.add.item[1]
					lo_parameter.AddCurrentValue('CDS Late Spinoff')
				case 5
// 					RptObj.ParameterFields.add.item[1].AddCurrentValue ('View/Print Annotation')
					lo_parameter = RptObj.ParameterFields.add.item[1]
					lo_parameter.AddCurrentValue('View/Print Annotation')
				case 6
// 					RptObj.ParameterFields.add.item[1].AddCurrentValue ('PCS Edit Annotation')
					lo_parameter = RptObj.ParameterFields.add.item[1]
					lo_parameter.AddCurrentValue('PCS Edit Annotation')
				case else
// 					RptObj.ParameterFields.add.item[1].AddCurrentValue ('Edit Annotation')
					lo_parameter = RptObj.ParameterFields.add.item[1]
					lo_parameter.AddCurrentValue('Edit Annotation')
			end choose
// 			RptObj.ParameterFields.add.item[2].AddCurrentValue (Lconno)
					lo_parameter = RptObj.ParameterFields.add.item[2]
					lo_parameter.AddCurrentValue(lconno)
		ELSEIF IsValid(w_sheet_view_annotation) THEN
			Lconno = w_sheet_view_annotation.em_conno.text
// 			RptObj.ParameterFields.add.item[2].AddCurrentValue (Lconno)
//			RptObj.ParameterFields.add.item[1].AddCurrentValue ('View/Print Annotation')
					lo_parameter = RptObj.ParameterFields.add.item[1]
					lo_parameter.AddCurrentValue('View/Print Annotation')
					lo_parameter = RptObj.ParameterFields.add.item[2]
					lo_parameter.AddCurrentValue(lconno)

		ELSEIF IsValid(w_sheet_final_review) THEN
			Lconno = w_sheet_final_review.Lconno
//			RptObj.ParameterFields.add.item[1].AddCurrentValue ('Final review')
// 			RptObj.ParameterFields.add.item[2].AddCurrentValue (Lconno)
			lo_parameter = RptObj.ParameterFields.add.item[1]
			lo_parameter.AddCurrentValue('Final Review')
			lo_parameter = RptObj.ParameterFields.add.item[2]
			lo_parameter.AddCurrentValue(lconno)
		ELSE			
			Open(w_cds_gets_conno)
			IF (IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"") THEN
				Lconno = Message.StringParm
			End IF
//			RptObj.ParameterFields.add.item[1].AddCurrentValue ('Edit Annotation')
//			RptObj.ParameterFields.add.item[2].AddCurrentValue (Lconno)
			lo_parameter = RptObj.ParameterFields.add.item[1]
			lo_parameter.AddCurrentValue('Edit Annotation')
			lo_parameter = RptObj.ParameterFields.add.item[2]
			lo_parameter.AddCurrentValue(lconno)

		END IF
	ELSEIF rpt_name = 'titleinformationlistbycontrolnumber' THEN			
		IF IsValid(w_books_query) THEN
			Lconno = w_books_query.bkquery_conno
		ELSE
			cb_refresh.Visible = TRUE
			Open(w_cds_gets_crystal_title_info)
			IF (IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"") THEN
				Lconno = Message.StringParm
			End IF
		END IF
		//appeon start
		//RptObj.ParameterFields.add.item(1).AddCurrentValue (Lconno)
		RptObj.ParameterFields.item(1).AddCurrentValue (Lconno)
		//appeon end
//			lo_parameter = RptObj.ParameterFields.add.item[1]
//			lo_parameter = RptObj.ParameterFields.add.item(1)
//			lo_parameter.AddCurrentValue(lconno)
	END IF

	if ls_type = 'PB' Then
	// Logon to internal db 
		sc1.LogOnServer( "crdb_oracle.dll",ls_Instance,"",ls_Userid,ls_Passwrd)
	// Logon to web db 
		sc1.LogOnServer( "crdb_oracle.dll",ls_Instance2,"",ls_Userid2,ls_Passwrd2)
	ELSE
	// Logon to internal db 
		sc1.LogOnServer(Appeongetcachedir() +'\plugin\'+ "crdb_oracle.dll",ls_Instance,"",ls_Userid,ls_Passwrd)
	// Logon to web db 
		sc1.LogOnServer(Appeongetcachedir() +'\plugin\'+ "crdb_oracle.dll",ls_Instance2,"",ls_Userid2,ls_Passwrd2)
	END IF
	
	this.ole_crystal.object.ReportSource(rptobj)

	// Run
	this.ole_crystal.object.ViewReport()
	
	oleobj_Created = TRUE
	
	IF (rpt_Name = 'bcsindividualcontrolreport' ) THEN
		//print out the report
		RptObj.PrintOut()
		cb_close.TriggerEvent(Clicked!)
	END IF
END IF

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(ole_crystal, "Scale")
inv_resize.of_Register(cb_close, "Scale")
inv_resize.of_Register(cb_refresh, "Scale")
inv_resize.of_Register(cb_print, "Scale")


end event

type ole_crystal from olecustomcontrol within w_sheet_pics_ole_crystal
event closebuttonclicked ( ref boolean usedefault )
event firstpagebuttonclicked ( ref boolean usedefault )
event lastpagebuttonclicked ( ref boolean usedefault )
event prevpagebuttonclicked ( ref boolean usedefault )
event nextpagebuttonclicked ( ref boolean usedefault )
event gotopagenclicked ( ref boolean usedefault,  integer pagenumber )
event stopbuttonclicked ( integer loadingtype,  ref boolean usedefault )
event refreshbuttonclicked ( ref boolean usedefault )
event printbuttonclicked ( ref boolean usedefault )
event grouptreebuttonclicked ( boolean ocx_visible )
event zoomlevelchanged ( integer zoomlevel )
event searchbuttonclicked ( string searchtext,  ref boolean usedefault )
event drillongroup ( any groupnamelist,  integer drilltype,  ref boolean usedefault )
event drillondetail ( any fieldvalues,  long selectedfieldindex,  ref boolean usedefault )
event showgroup ( any groupnamelist,  ref boolean usedefault )
event selectionformulabuttonclicked ( ref string selctionformula,  ref boolean usedefault )
event selectionformulabuilt ( string selctionformula,  ref boolean usedefault )
event ocx_clicked ( long ocx_x,  long ocx_y,  any eventinfo,  ref boolean usedefault )
event dblclicked ( long ocx_x,  long ocx_y,  any eventinfo,  ref boolean usedefault )
event downloadstarted ( integer loadingtype )
event downloadfinished ( integer loadingtype )
event viewchanging ( long oldviewindex,  long newviewindex )
event viewchanged ( long oldviewindex,  long newviewindex )
event onreportsourceerror ( string errormsg,  long errorcode,  ref boolean usedefault )
event exportbuttonclicked ( ref boolean usedefault )
event searchexpertbuttonclicked ( ref boolean usedefault )
event drillongraph ( long pagenumber,  long ocx_x,  long ocx_y,  ref boolean usedefault )
event drillonsubreport ( any groupnamelist,  string subreportname,  string title,  long pagenumber,  long index,  ref boolean usedefault )
event helpbuttonclicked ( )
event focuschanged ( boolean hasfocus )
event oncontextmenu ( any objectdescription,  long ocx_x,  long ocx_y,  ref boolean usedefault )
event onchangeobjectrect ( any objectdescription,  long ocx_x,  long ocx_y,  long ocx_width,  long ocx_height )
event onlaunchhyperlink ( ref string hyperlink,  ref boolean usedefault )
integer x = 27
integer y = 24
integer width = 3122
integer height = 1800
integer taborder = 10
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_sheet_pics_ole_crystal.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

type cb_print from commandbutton within w_sheet_pics_ole_crystal
boolean visible = false
integer x = 41
integer y = 1856
integer width = 466
integer height = 96
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print Report"
end type

event clicked;rptobj.PrinterSetup(0)
rptobj.PrintOut(False)

end event

type cb_refresh from commandbutton within w_sheet_pics_ole_crystal
boolean visible = false
integer x = 2194
integer y = 1856
integer width = 402
integer height = 96
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Refresh"
end type

event clicked;IF oleobj_Created THEN
   DESTROY RptObj
   DESTROY sc1	
END IF
Close(parent)

OpenSheetwithparm(w_sheet_pics_ole_crystal,"titleinformationlistbycontrolnumber",w_pics_main, 0, Original!)
end event

type cb_close from commandbutton within w_sheet_pics_ole_crystal
integer x = 2679
integer y = 1856
integer width = 466
integer height = 96
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close "
boolean cancel = true
end type

event clicked;IF oleobj_Created THEN
   DESTROY RptObj
   DESTROY sc1	
END IF
cb_refresh.Visible = FALSE

Close(parent)
end event


Start of PowerBuilder Binary Data Section : Do NOT Edit
04w_sheet_pics_ole_crystal.bin 
2B00000a00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff000000010000000000000000000000000000000000000000000000000000000002196ac001cad70c00000003000000800000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000102001affffffff00000002ffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe0000000000000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000001001affffffffffffffff00000003a1b8a30b4a3e8aaaa51d698811a0e8090000000002196ac001cad70c02196ac001cad70c000000000000000000000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000760000000000000001fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
2Effffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00000300000002580000469700002e82ffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000b0000000bffff000bffff000bffff000bffff000b00000008000b0000000bffff000b0000000b0000000b0000000bffff0000ffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
14w_sheet_pics_ole_crystal.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point

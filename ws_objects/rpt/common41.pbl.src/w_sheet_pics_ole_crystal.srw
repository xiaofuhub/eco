﻿$PBExportHeader$w_sheet_pics_ole_crystal.srw
forward
global type w_sheet_pics_ole_crystal from w_sheet
end type
type cb_refresh from commandbutton within w_sheet_pics_ole_crystal
end type
type ole_crystal from olecontrol within w_sheet_pics_ole_crystal
end type
type cb_close from commandbutton within w_sheet_pics_ole_crystal
end type
end forward

global type w_sheet_pics_ole_crystal from w_sheet
integer x = 214
integer y = 221
integer width = 3392
integer height = 2188
string title = "OLE Crystal report"
boolean vscrollbar = true
windowstate windowstate = maximized!
boolean ib_alwaysvalidate = true
cb_refresh cb_refresh
ole_crystal ole_crystal
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
this.cb_refresh=create cb_refresh
this.ole_crystal=create ole_crystal
this.cb_close=create cb_close
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_refresh
this.Control[iCurrent+2]=this.ole_crystal
this.Control[iCurrent+3]=this.cb_close
end on

on w_sheet_pics_ole_crystal.destroy
call super::destroy
destroy(this.cb_refresh)
destroy(this.ole_crystal)
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
//	ls_Passwrd = 'pcsadmin1' // Tracker 2120 version password validation 12/1/2008

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

	
	// Open report
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
	
	
	
 	//get rid of any data saved with report
	RptObj.discardsaveddata()
	
	
	 //	this.ole_crystal.object.ReportSource(RptObj)
   	 this.ole_crystal.object.reportsource = RptObj
	 //set properties of Crystal Viewer control
	 this.ole_crystal.object.enableexportbutton    = TRUE
	 this.ole_crystal.object.enablePrintButton     = TRUE
	 this.ole_crystal.object.enableprogresscontrol = TRUE
	 this.ole_crystal.object.enablesearchcontrol   = TRUE
	 this.ole_crystal.object.enablestopbutton      = TRUE
	 this.ole_crystal.object.enablezoomcontrol     = TRUE
	 this.ole_crystal.object.displaytoolbar        = TRUE
	 this.ole_crystal.object.displaytabs           = TRUE
	 this.ole_crystal.object.enableGroupTree       = TRUE
	 this.ole_crystal.object.enablePopUpMenu       = TRUE
	 this.ole_crystal.object.enableSelectExpertButton = TRUE

	
	tday = Today()

	select max(fr) 
	into :ld_fr
	from mchar
	using sqlservertrans;
	
	rpt_Name = Lower(rpt_Name)
	IF rpt_Name = 'finalreview' THEN
		IF IsValid(w_sheet_final_review) THEN
			ld_fr = date(w_sheet_final_review.dw_final_approval.object.fr[1])
			RptObj.ParameterFields.item[1].AddCurrentValue (ld_fr)
		ELSE
 			RptObj.ParameterFields.item[1].AddCurrentValue (tday)
		END IF
	ELSEIF rpt_Name = 'dtb_qas' THEN
		IF IsValid(w_qa_zedval_summary) THEN
			ld_review_date = date(w_qa_zedval_summary.ld_review_date)
			RptObj.ParameterFields.item[1].AddCurrentValue (ld_review_date)
		END IF
	ELSEIF rpt_Name = 'dtb_zedval_nlsval' THEN
		IF IsValid(w_qa_zedval_reports) THEN
			ll_bkseq = w_qa_zedval_reports.ll_bkseq
			RptObj.ParameterFields.item[1].AddCurrentValue (ll_bkseq)
		END IF
	ELSEIF rpt_Name = 'stage1annotationreport' THEN
 		RptObj.ParameterFields.item[1].AddCurrentValue (ld_fr)
	ELSEIF rpt_Name = 'stage1exception' THEN
 		RptObj.ParameterFields.item[1].AddCurrentValue (ld_fr)
	ELSEIF rpt_Name = 'bcsindividualcontrolreport' THEN
		IF IsValid(w_sheet_bcs_stage1) THEN
			Lconno = w_Sheet_bcs_stage1.pass_Conno
			RptObj.ParameterFields.item[1].AddCurrentValue (Lconno)
		END IF
	ELSEIF rpt_Name = 'stage2list' THEN
		IF IsValid(w_sheet_bcs_stage2) THEN
			Ldstg2dt = w_sheet_bcs_stage2.Ldstg2dt
//			MessageBox('stage 2 date in',string(Ldstg2dt))			
			RptObj.ParameterFields.item[1].AddCurrentValue (Ldstg2dt)
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
 					RptObj.ParameterFields.item[1].AddCurrentValue ('Add Annotation')
				case 2
 					RptObj.ParameterFields.item[1].AddCurrentValue ('CDS Edit Annotation')
				case 3
 					RptObj.ParameterFields.item[1].AddCurrentValue ('PMS Edit Annotation')
				case 4
 					RptObj.ParameterFields.item[1].AddCurrentValue ('CDS Late Spinoff')
				case 5
 					RptObj.ParameterFields.item[1].AddCurrentValue ('View/Print Annotation')
				case 6
 					RptObj.ParameterFields.item[1].AddCurrentValue ('PCS Edit Annotation')
				case else
 					RptObj.ParameterFields.item[1].AddCurrentValue ('Edit Annotation')
			end choose
 			RptObj.ParameterFields.item[2].AddCurrentValue (Lconno)
		ELSEIF IsValid(w_sheet_view_annotation) THEN
			Lconno = w_sheet_view_annotation.em_conno.text
 			RptObj.ParameterFields.item[2].AddCurrentValue (Lconno)
			RptObj.ParameterFields.item[1].AddCurrentValue ('View/Print Annotation')
		ELSEIF IsValid(w_sheet_final_review) THEN
			Lconno = w_sheet_final_review.Lconno
			RptObj.ParameterFields.item[1].AddCurrentValue ('Final review')
 			RptObj.ParameterFields.item[2].AddCurrentValue (Lconno)
		ELSE			
			Open(w_cds_gets_conno)
			IF (IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"") THEN
				Lconno = Message.StringParm
			End IF
	//		MessageBox('conno',lconno)
			RptObj.ParameterFields.item[1].AddCurrentValue ('Edit Annotation')
			RptObj.ParameterFields.item[2].AddCurrentValue (Lconno)
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
		RptObj.ParameterFields.item[1].AddCurrentValue (Lconno)
	END IF

// Logon to PCSP using Crdb_oracle.dll
	sc1.LogOnServer("crdb_oracle.dll",ls_Instance,"",ls_Userid,ls_Passwrd)
//	sc1.LogOnServer("p2sora7.dll",ls_Instance,"",ls_Userid,ls_Passwrd)
// Logon to PICP for title information list report
	sc1.LogOnServer("crdb_oracle.dll",ls_Instance2,"",ls_Userid2,ls_Passwrd2)
//	sc1.LogOnServer("p2sora7.dll",ls_Instance2,"",ls_Userid2,ls_Passwrd2)
	

	// Run
	this.ole_crystal.object.ViewReport()
	
	oleobj_Created = TRUE
	
//	IF (rpt_Name = 'bcsindividualcontrolreport' OR &
//		rpt_Name = 'finalreview' OR & 
//		rpt_name = 'editannotation' ) THEN
// PR 1954 : removed the final review from this list

//	IF (rpt_Name = 'bcsindividualcontrolreport' OR &
//		rpt_name = 'editannotation' ) THEN
//		//print out the report
//		RptObj.PrintOut()
//		cb_close.TriggerEvent(Clicked!)
//	END IF
	
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

type ole_crystal from olecontrol within w_sheet_pics_ole_crystal
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
integer x = 18
integer y = 32
integer width = 3113
integer height = 1804
integer taborder = 10
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_sheet_pics_ole_crystal.win"
omactivation activation = activateondoubleclick!
omdisplaytype displaytype = displayascontent!
omcontentsallowed contentsallowed = containsany!
end type

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
string text = "Close OLE Box"
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
2100000a00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff00000001a1b8a30b4a3e8aaaa51d698811a0e8090000000000000000000000000eddc2b001c9da4600000003000000c000000000004f00010065006c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000102000affffffff00000002ffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000001400000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020012ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000100000076000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000002fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
2Affffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0200000100000008000000000000000000000000000000280000004000190248000007160000011a0000001d000001310000003300000000001ed758000000000000030000000258000013d8000013d8ffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000bffff000b0000000bffff000bffff000bffff000bffff000b00000008000b0000000bffff000b0000000b0000000b0000000bffff0000ffff00000000001a6be0000000000000002800000040001902480000ffff000001830000001d000001830000001d00000000001ad26800000000000000280000004000190248000004f6000001890000001d000001a00000003300000000001e865000000000000000280000004000190248000004f8000001a00000001d000001b70000003300000000001b5dc8000000000000002800000040001902480000ffff000001b70000001d000001b70000001d00000000001ad258000000000000002800000040001902480000052c000001bd0000001d000001d40000003300000000001e8610000000000000002800000040001902480000052e000001d40000001d000001eb0000003300000000001afe180000000000000028000000400019024800000530000001eb0000001d000002020000003300000000001e85d0000000000000002800000040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
14w_sheet_pics_ole_crystal.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point

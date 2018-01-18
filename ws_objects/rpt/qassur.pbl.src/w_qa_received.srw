$PBExportHeader$w_qa_received.srw
forward
global type w_qa_received from w_sheet
end type
type st_4 from statictext within w_qa_received
end type
type em_2 from editmask within w_qa_received
end type
type em_1 from editmask within w_qa_received
end type
type st_3 from statictext within w_qa_received
end type
type st_2 from statictext within w_qa_received
end type
type dw_3 from u_pics_dw within w_qa_received
end type
type st_1 from statictext within w_qa_received
end type
type dw_2 from u_pics_dw within w_qa_received
end type
type cb_exit from commandbutton within w_qa_received
end type
type cb_clear from commandbutton within w_qa_received
end type
type cb_update from commandbutton within w_qa_received
end type
type dw_1 from u_pics_dw within w_qa_received
end type
end forward

global type w_qa_received from w_sheet
integer x = 233
integer y = 288
integer width = 2830
integer height = 1816
string title = "Quality Assurance Books Received"
event ue_clear ( )
event ue_update ( )
st_4 st_4
em_2 em_2
em_1 em_1
st_3 st_3
st_2 st_2
dw_3 dw_3
st_1 st_1
dw_2 dw_2
cb_exit cb_exit
cb_clear cb_clear
cb_update cb_update
dw_1 dw_1
end type
global w_qa_received w_qa_received

type variables
n_ds ids
n_qa_services inv_qa
boolean ib_changesmade

string is_len
string is_vols
string is_minlastside
string is_qarecdt

boolean ib_active = false
end variables

forward prototypes
public function integer wf_calculate_units (string media, integer len, integer qnty, integer minlastside, string lcntr, long lbkseq, integer volumns)
public function integer of_setbkmed (long al_row)
public function integer wf_set_prod_units ()
public function integer of_filter ()
end prototypes

event ue_clear();dw_1.reset()
//dw_2.reset()
dw_1.insertrow(0)
dw_1.setfocus()

ib_changesmade = false

end event

event ue_update();/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  ue_update
//
//	Description: Update QAStg table
//
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			06/16/2008      Phase-2 ESTPT changes	Do not Update DB mchar record with 
//																			Len, Vols and Min last side
//																			These cols are readonly
//																	Call function to update prod units
// Murali K. 			06/19/2008								validate length_ instead of len for BR records
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Long ll_row, ll_qrow,Lbkseq
Int li_ret,llen,Lvols,lminlastside,ll_totunits,ll_qnty
n_ds lds
Boolean lb1,lb2
Date ld_date,lqarecdt,tday
String lbkmed,Lbkno,Lcntr,lqastg,lmed,ls_userid,Lother_conno,Lconno


dw_1.AcceptText()
dw_1.SetFocus()

tday = Today()
ls_userid = gnv_app.of_GetUserId()

// Discard Deleted Rows

dw_1.RowsDiscard(1, dw_1.DeletedCount(), delete!)

//validate

FOR ll_row = 1 TO dw_1.RowCount()
	
	ld_date = Date(dw_1.object.qastg_qarecdt[ll_row])
	
	IF IsNull(dw_1.object.qastg_qastg[ll_row]) AND &
		ld_date > Date('1/1/1980') THEN
//		IF IsNull(dw_1.object.mchar_len[ll_row]) OR dw_1.object.mchar_len[ll_row]<0 THEN
//			dw_1.SetRow(ll_row)
//			dw_1.SetColumn("mchar_len")
//			Messagebox("Invalid Input","Tracks/BPages is invalid, please correct this field.")
//			RETURN
//		END IF
		// 06/19/2008 validate length_ instead of len for BR records
		IF IsNull(dw_1.object.mchar_length_[ll_row]) OR dw_1.object.mchar_length_[ll_row]<0 THEN
			dw_1.SetRow(ll_row)
			dw_1.SetColumn("mchar_length_")
			Messagebox("Invalid Input","BPages is invalid, please correct this field.")
			RETURN
		END IF


		IF IsNull(dw_1.object.mchar_vols[ll_row]) OR dw_1.object.mchar_vols[ll_row]<0 THEN
			dw_1.SetRow(ll_row)
			dw_1.SetColumn("mchar_vols")
			Messagebox("Invalid Input","Cassettes/Cartridge Volumes is invalid, please correct this field.")
			RETURN
		END IF
//		if trim(dw_1.object.mchar_bkmed[ll_row])="RC" and (isnull(dw_1.object.mchar_minlastside[ll_row]) or dw_1.object.mchar_minlastside[ll_row]< 0 )then
//			dw_1.setrow(ll_row)
//			dw_1.setcolumn("mchar_minlastside")
//			messagebox("Invalid Input","Minutes last Side is invalid, please correct this field.")
//			return
//		end if


	END IF
	
	IF ld_date > Date(Today()) THEN
		dw_1.SetRow(ll_row)
		dw_1.SetColumn("qastg_qarecdt")
		Messagebox("Invalid Input","Received Date is invalid, please correct this field.")
		RETURN
	END IF
	
	// Mark the MCHAR Table.
	IF NOT(IsNull(dw_1.object.mchar_bkseq[ll_row])) THEN
		f_update_mchar_time("",Long(dw_1.object.mchar_bkseq[ll_row]),"B","U")
	END IF
NEXT 

// 06/16/2008
// Update prod units
wf_set_prod_units()

// set up qstg_insert, will hold new rows in qstg
lds = CREATE n_ds
lds.dataObject = "d_ds_qastg_insert"
lds.SetTransObject(SqlServerTrans)
lds.Reset()

//set hidden fields
FOR ll_row =1 TO dw_1.RowCount()
	IF IsNull(String(dw_1.object.qastg_qastg[ll_row])) AND &
				NOT IsNull(dw_1.object.mchar_len[ll_row]) AND &
				NOT IsNull(dw_1.object.mchar_vols[ll_row]) AND &
				(NOT IsNull(dw_1.object.mchar_minlastside[ll_row]) OR &
					   Trim(dw_1.object.mchar_bkmed[ll_row])="BR" OR & 
					   Trim(dw_1.object.mchar_bkmed[ll_row]) = "DB" &
				) THEN
				
		IF Trim(dw_1.object.mchar_bkmed[ll_row]) = "RC" OR Trim(dw_1.object.mchar_bkmed[ll_row]) = "DB" THEN
   			dw_1.object.qastg_qastg[ll_row] = String(1)
		ELSE
			dw_1.object.qastg_qastg[ll_row] = String(2)
		END IF
		ll_qrow = lds.InsertRow(0)
		lds.object.bkmed[ll_qrow] = dw_1.object.mchar_bkmed[ll_row]
		lds.object.bkseq[ll_qrow] = dw_1.object.mchar_bkseq[ll_row]
		lds.object.cntr[ll_qrow] = dw_1.object.prod_cntr[ll_row]
		lds.object.qarecdt[ll_qrow] = Date(dw_1.object.qastg_qarecdt[ll_row])
		lds.object.qastg[ll_qrow] = dw_1.object.qastg_qastg[ll_row]
		lds.object.qastatcd[ll_qrow] = "I"
		lds.object.created_date[ll_qrow] = tday
		lds.object.modified_date[ll_qrow] = tday
		lds.object.created_by[ll_qrow] = ls_userid
		lds.object.modified_by[ll_qrow] = ls_userid
		
		lbkmed = dw_1.object.mchar_bkmed[ll_row]
		Lbkseq = dw_1.object.mchar_bkseq[ll_row]
		Lbkno = Trim(lbkmed)+Trim(String(Lbkseq))
		Lcntr = dw_1.object.prod_cntr[ll_row]
		lqarecdt = Date(dw_1.object.qastg_qarecdt[ll_row])
		lqastg = dw_1.object.qastg_qastg[ll_row]
		llen = dw_1.object.mchar_len[ll_row]
		Lvols = dw_1.object.mchar_vols[ll_row]
		lminlastside = dw_1.object.mchar_minlastside[ll_row]
		lmed = dw_1.object.mchar_med[ll_row]
		ll_qnty = dw_1.object.mchar_qnty[ll_row]
		Lconno = dw_1.object.mchar_conno[ll_row]
		Lother_conno = dw_1.object.other_media_conno[ll_row]
		
		ll_totunits = wf_calculate_units(lmed,llen,ll_qnty,lminlastside,Lcntr,Lbkseq,Lvols)

		// Insert into prdrqastg table. (RS21)
		INSERT INTO prdrqastg@pic_link  
				(bkmed,bkseq,bkno,cntr,qastg,qarecdt,qastatcd)  
		values (:lbkmed,:Lbkseq,:Lbkno,:Lcntr,:lqastg,:lqarecdt,'I')
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"Inserting PRDRQASTG")=FALSE THEN
			ROLLBACK USING SqlServerTrans;
			RETURN
		END IF
		
		// 06/16/2008 do not update len, vols for RC/DB record ESTPT changes
		// These cols are readonly
		
		// Update MCHAR for other media conno if it exist
//		IF NOT(IsNull(Lother_conno)) OR Lother_conno <> "" THEN
//			UPDATE MCHAR
//			SET len = :llen, vols = :Lvols, minlastside = :lminlastside
//			WHERE conno = :Lother_conno
//			USING SQlServerTrans;
//			IF f_check_dberror(SqlServerTrans,"Updating MCHAR for other conno ")=FALSE THEN
//				ROLLBACK USING SqlServerTrans;
//				RETURN
//			END IF		
//		END IF
		/////////////////////// 06/16/2008
		
	END IF
NEXT

// do the main update
li_ret = dw_1.TRIGGER EVENT pfc_update(TRUE,TRUE)
IF li_ret = -1 THEN
	IF SqlServerTrans.sqlCode <> 0 THEN
		Messagebox("Error Updating QASTG Table", &
			"Error: " + String(SqlServerTrans.sqlDbCode) + &
			" Message: " + String(SqlServerTrans.sqlErrText))
		ROLLBACK USING SqlServerTrans;
	END IF
	RETURN
END IF


// insert the rows in qastg
li_ret = lds.Update()
IF li_ret = -1 THEN
	IF SqlServerTrans.sqlCode <> 0 THEN
		Messagebox("Error Updating QASTG Table", &
		"Error: " + String(SqlServerTrans.sqlDbCode) + &
		" Message: " + String(SqlServerTrans.sqlErrText))
		ROLLBACK USING SqlServerTrans;
	END IF
	RETURN
END IF

// All updates are successfull, therfore commit your work
// to both databases.
COMMIT USING SqlServerTrans;

lds.Reset()

ib_changesmade = FALSE
Messagebox('Update', 'Update successful')
end event

public function integer wf_calculate_units (string media, integer len, integer qnty, integer minlastside, string lcntr, long lbkseq, integer volumns);Long units,subunits, ll_length
Int vols
String lmed,lcntrtype,lbkmed,lflash_indicator


lmed = Trim(media)
lbkmed = dw_1.object.mchar_bkmed[dw_1.GetRow()]
ll_length = dw_1.object.mchar_length_[dw_1.GetRow()]

SELECT cntrtype INTO :lcntrtype FROM ancntr
	where cntr = :Lcntr
	AND cntrmed = :lmed
	USING SqlServerTrans;
	
//Messagebox("cntrtype",Lcntrtype)

IF lcntrtype = 'T' THEN
	IF lmed = 'RC' THEN
		// First calculate the duplication units for Cassettes
		vols = volumns // Ceiling(len/4) 06/19/2008
		units = vols * qnty
		
		// Update the prod table for duplication of this RC book with
		// the correct number of units.
		
		// Messagebox("output","bkseq = "+string(Lbkseq))
		UPDATE prod set units = :units, subunits = :vols
			where bkseq = :Lbkseq
			AND	bkmed = :lmed
			AND	cntr 	= :Lcntr
			AND	prodstage = 'DU'
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
		
		// Secondly calculate the mastering units for cassettes
		IF IsNull(minlastside)=FALSE THEN
			units = ((len - 1) * 88) + minlastside
		ELSE
			units = ((len - 1) * 88)
		END IF
		subunits = Ceiling(len/2)
		
		UPDATE prod set units = :units,subunits = :subunits
			where bkseq = :Lbkseq
			AND	bkmed = :lmed
			AND	cntr 	= :Lcntr
			AND	prodstage in ('MA','AB')
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
ELSEIF lmed = 'RTB' THEN

		// First calculate the duplication units for Cassettes
		vols = volumns // Ceiling(len/4) 06/19/2008
		units = vols * qnty

		// 06/19/2008
		IF lbkmed = 'RC' THEN
			// Update the prod table for duplication of this RC book with
			// the correct number of units.
			UPDATE prod set units = :units, subunits = :vols
				where bkseq = :Lbkseq
				AND	bkmed = 'RC'
				AND	cntr 	= :Lcntr
				AND	prodstage = 'DU'
			USING SqlServerTrans;
			// Messagebox("output","bkseq = "+string(Lbkseq))
			IF f_check_dberror(SqlServerTrans,"RTB - Duplication for PROD")=FALSE THEN
				RETURN -1
			END IF
		END IF // 06/19/2008 ONLY FOR RC
		
		// Update the prod table for duplication of this DB book with
		// the correct number of units.
		
		// 06/19/2008
		IF lbkmed = 'DB' THEN
			
			UPDATE prod set units = :units, subunits = NULL
				where bkseq = :Lbkseq
				AND	bkmed = 'DB'
				AND	cntr 	= :Lcntr
				AND	prodstage = 'FC'
			USING SqlServerTrans;
			
			// Messagebox("output","bkseq = "+string(Lbkseq))
			IF f_check_dberror(SqlServerTrans,"RTB - Duplication for PROD")=FALSE THEN
				RETURN -1
			END IF
			
		END IF
		
		// Secondly calculate the mastering units for cassettes
		// 06/19/2008
		IF lbkmed = 'RC' THEN
		
				IF IsNull(minlastside)=FALSE THEN
					units = ll_length // ((len - 1) * 88) + minlastside // 06/19/2008
				ELSE
					units = ll_length // ((len - 1) * 88)
				END IF
				subunits = len
				
				UPDATE prod set units = :units,subunits = :subunits
					where bkseq = :Lbkseq
					AND	bkmed = :lbkmed
					AND	cntr 	= :Lcntr
					AND	prodstage in ('MA','AB')
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB - Mastering for PROD")=FALSE THEN
					RETURN -1
				END IF
			END IF			
		
		// Thirdly calculate the Z mastering units for cassettes
		IF IsNull(minlastside)=FALSE THEN
			units = ll_length // ((len - 1) * 88) + minlastside
		ELSE
			units = ll_length // ((len - 1) * 88)
		END IF
		subunits = 1
		
		UPDATE prod set units = :units,subunits = :subunits
			where bkseq = :Lbkseq
			AND	bkmed = 'DB'
			AND	cntr 	= :Lcntr
			AND	prodstage = 'ZM'
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"RTB - Z Mastering for PROD")=FALSE THEN
			RETURN -1
		END IF

	ELSEIF lmed = 'BR' THEN
		// First calculate the duplication units for Braille
		units = ll_length * qnty // len * qnty // 06/19/2008
		vols = volumns
		//Messagebox("data2","Vols ="+string(vols)+"qnty ="+string(qnty))
		subunits = vols * qnty
		
		// Update the prod table for duplication of this BR book with
		// the correct number of units.
		
		UPDATE prod set units = :units,subunits = :subunits
			where bkseq = :Lbkseq
			AND	bkmed = 'BR'
			AND	cntr 	= :Lcntr
			AND	prodstage in ('PR','EM')
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
		// Secondly calculate the mastering units for Braille
		units = len
		UPDATE prod set units = :units
			where bkseq = :Lbkseq
			AND	bkmed = 'BR'
			AND	cntr 	= :Lcntr
			AND	prodstage in ('MA','AB')
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
	ELSEIF lmed = 'P/B' THEN
		// First calculate the duplication units for Braille
		units = qnty
		
		// Update the prod table for duplication of this Print Braille book with
		// the correct number of units.
		
		UPDATE prod set units = :units
			where bkseq = :Lbkseq
			AND	bkmed = 'BR'
			AND	cntr 	= :Lcntr
			AND	prodstage in ('PB','EM')
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
		// Secondly calculate the mastering units for Print Braille
		units = qnty
		UPDATE prod set units = :units
			where bkseq = :Lbkseq
			AND	bkmed = 'BR'
			AND	cntr 	= :Lcntr
			AND	prodstage = 'PU'
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
	END IF
//------------------------------------------------ Not a Total Contract --	
ELSEIF  lcntrtype <> 'T' THEN
	IF lcntrtype = 'M' THEN
		IF lmed = 'RC' THEN
			// Calculate the mastering units for cassettes
			IF IsNull(minlastside)=FALSE THEN
				units = ((len - 1) * 88) + minlastside
			ELSE
				units = ((len - 1) * 88)
			END IF
			subunits = Ceiling(len/2)
			UPDATE prod set units = :units, subunits = :subunits
				where bkseq = :Lbkseq
				AND	bkmed = :lmed
				AND	cntr 	= :Lcntr
				AND	prodstage in ('MA','AB')
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		ELSEIF lmed = 'BR' THEN
			// Calculate the mastering units for Braille
			units = len
			UPDATE prod set units = :units
				where bkseq = :Lbkseq
				AND	bkmed = 'BR'
				AND	cntr 	= :Lcntr
				AND	prodstage in ('MA','AB')
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		ELSEIF lmed = 'P/B'	THEN
			// Calculate the mastering units for Print Braille
			units = qnty
			UPDATE prod set units = :units
				where bkseq = :Lbkseq
				AND	bkmed = 'BR'
				AND	cntr 	= :Lcntr
				AND	prodstage = 'PU'
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		END IF
//-------------- Duplication contract
	ELSEIF lcntrtype = 'D' THEN
		IF lmed = 'RC' THEN

			// Calculate the duplication units for cassettes
			IF lbkmed = 'RC' THEN
				vols = volumns // Ceiling(len/4)
				units = vols * qnty
				// Update the prod table for duplication of this RC book with
				// the correct number of units.
				UPDATE prod set units = :units, subunits = :vols
					where bkseq = :Lbkseq
					AND	bkmed = 'RC'
					AND	cntr 	= :Lcntr
					AND	prodstage = 'DU'
				USING SqlServerTrans;
				// Messagebox("output","bkseq = "+string(Lbkseq))
				IF f_check_dberror(SqlServerTrans,"RTB - Duplication for PROD")=FALSE THEN
					RETURN -1
				END IF
				
			ELSEIF lbkmed = 'DB' THEN
				units = 1
				// Update the prod table for duplication of this DB book with
				// the correct number of units.
				UPDATE prod set units = :units, subunits = :vols
					where bkseq = :Lbkseq
					AND	bkmed = 'DB'
					AND	cntr 	= :Lcntr
					AND	prodstage = 'FC'
				USING SqlServerTrans;
				// Messagebox("output","bkseq = "+string(Lbkseq))
				IF f_check_dberror(SqlServerTrans,"RTB - Duplication for PROD")=FALSE THEN
					RETURN -1
				END IF
				
			END IF		
		ELSEIF lmed = 'BR' THEN
			// Calculate the duplication units for Braille
			units = len * qnty
			vols = volumns
			subunits = vols * qnty
			UPDATE prod set units = :units,subunits = :subunits
				where bkseq = :Lbkseq
				AND	bkmed = 'BR'
				AND	cntr 	= :Lcntr
				AND	prodstage in ('PR','EM')
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		ELSEIF lmed = 'P/B'	THEN
			// Calculate the duplication units for Print Braille
			units = qnty
			UPDATE prod set units = :units
				where bkseq = :Lbkseq
				AND	bkmed = 'BR'
				AND	cntr 	= :Lcntr
				AND	prodstage in ('PB','EM')
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		ELSEIF lmed = 'FD' THEN
			// Calculate the duplication units for Formated Disk
			vols = volumns // Ceiling(len/2) 06/19/2008
			units = vols * qnty
			UPDATE prod set units = :units
				where bkseq = :Lbkseq
				AND	bkmed = :lmed
				AND	cntr 	= :Lcntr
				AND	prodstage = 'DU'
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		END IF
	END IF	
END IF
COMMIT USING SqlServerTrans;
RETURN units
end function

public function integer of_setbkmed (long al_row);
// 03/26/2008

dw_1.setcolumn('mchar_bkmed')
dw_1.object.mchar_bkmed[al_row] = ' '
RETURN 1
end function

public function integer wf_set_prod_units ();/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function : wf_set_prod_units
//
//	Description: Calculate Prod Units and Update Prod table
//
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			06/16/2008      Phase-2 ESTPT changes	Called from ue_update
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//

int li_loop
String ls_data,Lcntr,Lprodstage,lmed, lsbkmed
Long ll_row, ll_row2,ll_bkseq
Long ll_minlastside, ll_volumn, ll_totunit, ll_length, ll_qnty,Lbkseq
Boolean lb_bk_exist, lb_needcopy
Long ll_index, ll_lastrow, ll_rc

ll_rc = dw_1.Rowcount()

FOR li_loop = 1 TO ll_rc

		ll_minlastside = dw_1.object.mchar_minlastside[li_loop]
		ll_length = dw_1.object.mchar_len[li_loop]
		ll_qnty = dw_1.object.mchar_qnty[li_loop]
		ll_volumn = dw_1.object.mchar_vols[li_loop]
		
		Lcntr = dw_1.object.prod_cntr[li_loop]
		Lbkseq = dw_1.object.prod_bkseq[li_loop]
		Lprodstage = dw_1.object.prod_prodstage[li_loop]
		lmed = dw_1.object.mchar_med[li_loop]
		

		CHOOSE CASE Trim(dw_1.object.mchar_med[li_loop])
			CASE "RC"
				ll_volumn = dw_1.object.mchar_vols[li_loop]  // Ceiling(ll_length / 4)
//				dw_1.object.mchar_vols[li_loop] = ll_volumn
				IF IsNull(ll_minlastside)=FALSE THEN
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[li_loop] = ll_totunit
				ELSE
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,0,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[li_loop] = ll_totunit
				END IF
			CASE "RTB"
				ll_volumn =dw_1.object.mchar_vols[li_loop]  // Ceiling(ll_length / 4)
//				dw_1.Object.mchar_vols[li_loop] = ll_volumn
				IF IsNull(ll_minlastside)=FALSE THEN
					ll_totunit = wf_calculate_units(Lmed,ll_length,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[li_loop] = ll_totunit
				ELSE
					ll_totunit = wf_calculate_units(Lmed,ll_length,ll_qnty,0,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[li_loop] = ll_totunit
				END IF
			CASE "FD"
				ll_volumn = dw_1.object.mchar_vols[li_loop]  // Ceiling(ll_length / 2)
//				dw_1.object.mchar_vols[li_loop] = ll_volumn
				IF IsNull(ll_minlastside)=FALSE THEN
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[li_loop] = ll_totunit
				ELSE
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,0,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[li_loop] = ll_totunit
				END IF
			CASE "BR"
				IF IsNull(ll_length)=FALSE THEN
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[li_loop] = ll_totunit
				ELSE
					ll_totunit = wf_calculate_units(lmed,0,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[li_loop] = ll_totunit
				END IF
			CASE "P/B"
				IF IsNull(ll_qnty)=FALSE THEN
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[li_loop] = ll_totunit
				ELSE
					ll_totunit = wf_calculate_units(lmed,ll_length,0,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[li_loop] = ll_totunit
				END IF
			END CHOOSE
NEXT
RETURN 1
end function

public function integer of_filter ();/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function : of_filter
// Args: None
//	Description:  filter data based on date range or a book number
//
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			09/23/2008      Phase-2 
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
date ld_data
date ld_from, ld_to
long ll_rc
string ls_filter, ls_sort

dw_2.show()

dw_3.accepttext()
ld_to = date(dw_3.object.qarecdt2[1])
ld_from = date(dw_3.object.qarecdt[1]) 

IF Len(Trim(string(ld_from))) > 0  AND( isnull(ld_to) or  Len(Trim(string(ld_to))) = 0 ) THEN
	ls_filter = "date(qastg_qarecdt) >= date('" + string(ld_from, 'mm/dd/yyyy') + "')" 
	dw_2.setfilter('')
	dw_2.filter()
	dw_2.setfilter(ls_filter)
	 dw_2.filter()
END IF


IF Len(Trim(string(ld_from))) > 0  AND Len(Trim(string(ld_to))) > 0 THEN
	ls_filter = "date(qastg_qarecdt) between date('" + string(ld_from, 'mm/dd/yyyy') + "') and date('" + string(ld_to,'mm/dd/yyyy') +"')"
	dw_2.setfilter('')
	dw_2.filter()
	dw_2.setfilter(ls_filter)
	 dw_2.filter()
END IF

IF Len(trim(em_1.text)) > 0 THEN
	ls_filter = 'bkseq = ' + em_1.text
	dw_2.setfilter('')
	dw_2.filter()
	dw_2.setfilter(ls_filter)
	dw_2.filter()
END IF
em_2.text=string(dw_2.rowcount())
dw_1.setfocus()
ls_sort = 'qastg_qarecdt, bkseq asc'
dw_2.setsort(ls_sort)
dw_2.sort()

RETURN 1
end function

on w_qa_received.create
int iCurrent
call super::create
this.st_4=create st_4
this.em_2=create em_2
this.em_1=create em_1
this.st_3=create st_3
this.st_2=create st_2
this.dw_3=create dw_3
this.st_1=create st_1
this.dw_2=create dw_2
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.cb_update=create cb_update
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.em_2
this.Control[iCurrent+3]=this.em_1
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.dw_3
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.dw_2
this.Control[iCurrent+9]=this.cb_exit
this.Control[iCurrent+10]=this.cb_clear
this.Control[iCurrent+11]=this.cb_update
this.Control[iCurrent+12]=this.dw_1
end on

on w_qa_received.destroy
call super::destroy
destroy(this.st_4)
destroy(this.em_2)
destroy(this.em_1)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.dw_3)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.cb_update)
destroy(this.dw_1)
end on

event open;call super::open;this.windowstate = maximized!

string ls_mchar_keycols[] = {"mchar_bkseq","mchar_bkmed"}
string ls_mchar_updatecols[] = {"mchar_len","mchar_vols","mchar_minlastside","mchar_length_"}
string ls_prod_keycols[] = {"prod_bkseq","prod_bkmed","prod_prodstage","prod_cntr"}
string ls_prod_updatecols[] = {"prod_units"}
string ls_qastg_keycols[] = {"qastg_bkseq","qastg_bkmed","qastg_cntr","qastg_qastg"}



trigger event ue_clear()
ib_disableclosequery = true
dw_1.of_setmultitable(true)
dw_1.inv_multitable.of_addtoupdate&
	("mchar",ls_mchar_keycols,ls_mchar_updatecols, true, 0)
dw_1.inv_multitable.of_addtoupdate&
	("prod",ls_prod_keycols,ls_prod_updatecols, true, 0)

dw_1.SetFocus()
end event

event closequery;int li_choice

//messagebox("Closequery","closequery")
if ib_changesmade = true then
	li_choice = messagebox("Discard Changes?","This data has not been updated.  Discard changes without saving?",StopSign!,OkCancel!,2)
   if li_choice = 1 then
		return 0
	else
		return 1
	end if
end if

return 0


end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_1, "Scale")
inv_resize.of_Register(dw_2, "Scale")
inv_resize.of_Register(st_4, "FixedToRight&Bottom")
inv_resize.of_Register(em_2, "FixedToRight&Bottom")
//inv_resize.of_Register(dw_3, "FixedToRight&Bottom")
inv_resize.of_Register(cb_update, "FixedToRight&Bottom")
inv_resize.of_Register(cb_clear, "FixedToRight&Bottom")
inv_resize.of_Register(cb_exit, "FixedToRight&Bottom")


end event

event pfc_postopen;call super::pfc_postopen;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  postopen
//
//	Description:
//	Retrieve based on date range
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version							Tracking#
//									
// Murali K.			09/19/2008      004 PICS 2.0 Modifications	 Reqs: QAS.A.1.1, QAS.A.1.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

//date ld_data
//date ld_from, ld_to
//long ll_rc
//
//ld_to = date(dw_3.object.qarecdt2[1])
//ld_from = date(dw_3.object.qarecdt[1]) 
//IF NOT Isnull(ld_to) THEN
//	dw_2.dataobject = 'd_qa_prod_recv_by_recv_date'
//	dw_2.settransobject(sqlservertrans)
//	ll_rc = dw_2.Retrieve(ld_from, ld_to)
//	em_2.text=string(ll_rc	)
//	dw_1.setfocus()
//END IF
of_filter()

end event

type st_4 from statictext within w_qa_received
integer x = 37
integer y = 1564
integer width = 992
integer height = 120
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Total Number of Books that passed Autotest and waiting for QA Tests:"
boolean focusrectangle = false
end type

type em_2 from editmask within w_qa_received
integer x = 1065
integer y = 1572
integer width = 183
integer height = 92
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 134217750
borderstyle borderstyle = stylelowered!
string mask = "#########"
end type

type em_1 from editmask within w_qa_received
integer x = 1801
integer y = 100
integer width = 343
integer height = 92
integer taborder = 40
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "#########"
end type

event modified;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  modified for em_1
//
//	Description:
//	Retrieve based on entered book number
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version							Tracking#
//									
// Murali K.			02/05/2008      004 PICS 2.0 Modifications	 Reqs: QAS.A.1.1, QAS.A.1.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
long ll_rc
dw_3.reset()
dw_3.insertrow(0)
//dw_2.dataobject = 'd_qa_prod_recv_by_recv_date_bkseq'
//dw_2.settransobject(sqlservertrans)
//ll_rc = dw_2.Retrieve(Long(this.text))
//
//IF ll_rc < 1 THEN
//	em_2.text=String(0)
//    messagebox('Error', 'Book Number not found.')
//ELSE
//	em_2.text=string(ll_rc	)
//	dw_1.setfocus()
//END IF
of_filter()


end event

type st_3 from statictext within w_qa_received
integer x = 1682
integer y = 24
integer width = 608
integer height = 76
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Book Number"
boolean focusrectangle = false
end type

type st_2 from statictext within w_qa_received
integer x = 1394
integer y = 24
integer width = 123
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "OR"
boolean focusrectangle = false
end type

type dw_3 from u_pics_dw within w_qa_received
integer x = 46
integer y = 100
integer width = 1170
integer height = 96
integer taborder = 30
string dataobject = "d_qa_qarecdt_range"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event ue_postconstructor;call super::ue_postconstructor;date lqarecdt

this.SetTransObject(sqlservertrans) 
this.Retrieve()

dw_1.SetFocus()

end event

event itemchanged;call super::itemchanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  itemchanged
//
//	Description:
//	Retrieve based on date range
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version							Tracking#
//									
// Murali K.			02/05/2008      004 PICS 2.0 Modifications	 Reqs: QAS.A.1.1, QAS.A.1.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

date ld_data
date ld_from, ld_to
long ll_rc

//ld_data = Date(data)
////messagebox("data",string(data))
//dw_2.Retrieve(ld_data)
//

CHOOSE CASE dwo.name
	// From date range	
	CASE 'qarecdt'
		em_1.text=''
		ld_to = date(this.object.qarecdt2[1])
		ld_from = date(data)
		IF ld_from > ld_to THEN
			Messagebox('Error','From Date cannot be greater than to date.')
			RETURN 1
		END IF
//		IF NOT Isnull(ld_to) THEN
//			dw_2.dataobject = 'd_qa_prod_recv_by_recv_date'
//			dw_2.settransobject(sqlservertrans)
//			ll_rc = dw_2.Retrieve(ld_from, ld_to)
//			em_2.text=string(ll_rc	)
//			IF ll_rc < 1 THEN
//			    messagebox('Error', 'No records found for that date range.')
//			ELSE
//				dw_1.setfocus()
//			END IF
//		END IF
		of_filter()
	CASE 'qarecdt2'
		em_1.text=''
		ld_from = date(this.object.qarecdt[1])
		IF Isnull(ld_from) THEN
			Messagebox('Error','Please select a from date.')
			RETURN 0
		END IF
		ld_to = date(data)
		IF ld_to < ld_from THEN
			Messagebox('Error','To Date cannot be  earlier than From date.')
			RETURN 1
		END IF
//		dw_2.dataobject = 'd_qa_prod_recv_by_recv_date'
//		dw_2.settransobject(sqlservertrans)
//		ll_rc = dw_2.Retrieve(ld_from, ld_to)
//		em_2.text=string(ll_rc	)
//		IF ll_rc < 1 THEN
//		    messagebox('Error', 'No records found for that date range.')
//		ELSE
//			dw_1.setfocus()
//		END IF
		of_filter()
END CHOOSE
end event

type st_1 from statictext within w_qa_received
integer x = 178
integer y = 24
integer width = 891
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select QA received Date Range"
boolean focusrectangle = false
end type

type dw_2 from u_pics_dw within w_qa_received
event ue_enterkey pbm_dwnprocessenter
integer x = 32
integer y = 1072
integer width = 2738
integer height = 476
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_qa_prod_recv_by_recv_date"
boolean hscrollbar = true
boolean livescroll = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;long ll_rc
dw_2.SetTransObject(sqlservertrans)
this.hide()
ll_rc = dw_2.retrieve()


end event

type cb_exit from commandbutton within w_qa_received
integer x = 2514
integer y = 1564
integer width = 251
integer height = 92
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;close(parent)
end event

type cb_clear from commandbutton within w_qa_received
integer x = 2217
integer y = 1564
integer width = 251
integer height = 92
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "C&lear"
end type

event clicked;
Parent.event trigger ue_clear()
end event

type cb_update from commandbutton within w_qa_received
integer x = 1920
integer y = 1564
integer width = 251
integer height = 92
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;Parent.trigger event ue_update()

end event

type dw_1 from u_pics_dw within w_qa_received
event ue_enterkey pbm_dwnprocessenter
integer x = 18
integer y = 240
integer width = 2738
integer height = 756
integer taborder = 10
string dataobject = "d_qa_prod_recv"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)

end event

event constructor;call super::constructor;// This Datastore holds individual rows that we query

ids = create n_ds
ids.dataobject = "d_qa_prod_recv"
ids.SetTransObject(sqlservertrans)
dw_1.SetTransObject(sqlservertrans)
ids.reset()
this.setfocus()



end event

event itemchanged;call super::itemchanged;
// 03/26/2008 Add book media
// 10/15/2008 if checked in do not receive.

String ls_data,Lcntr,Lprodstage,lmed, lsbkmed
Long ll_row, ll_row2,ll_bkseq, VOLUMN
Long ll_minlastside, ll_volumn, ll_totunit, ll_length, ll_qnty,Lbkseq
Boolean lb_bk_exist, lb_needcopy
Long ll_index, ll_lastrow, ll_null,  ll_orig_vol

setnull(ll_null)
// trim it

ls_data = Trim(data)  


CHOOSE CASE dwo.Name
	case "mchar_bkseq"  // for booknumber field
		
	// is it a number?
	if not isnumber(ls_data) then
	  messagebox("Input Error", "Invalid Book Sequence Number")
	  return 1
	end if
	
	// 03/26/2008
	Post of_setbkmed(row)
	
// 03/26/2008 retrieve based on bkseq and bkmed
CASE 'mchar_bkmed'

	IF Isnull(data) OR Len(Trim(data)) = 0 THEN
		  Messagebox("Input Error", "Please enter the book media to retrieve book information")
		  RETURN 1
	END IF

	lb_bk_exist = false
	if rowcount() > 0 then
		LBKSEQ = this.object.mchar_bkseq[row]
		lsbkmed = data
		for ll_index = 1 to rowcount()
			// 03/26/2008 change this to add bkmed also
			if lbkseq = long(this.object.mchar_bkseq[ll_index]) AND lsbkmed = this.object.mchar_bkmed[ll_index] then
				lb_bk_exist = true
			end if
		next
	end if

	// 10/15/2008 if checked in do not receive.
	IF inv_qa.of_ischeckedin(lbkseq,lsbkmed) THEN
	     Messagebox('Error', 'Book already checked in/uploaded by the producer.')
		This.object.mchar_bkseq[row] = ll_null
		This.object.mchar_bkmed[row] = ''
		This.setcolumn('mchar_bkseq')
		RETURN 1
	END IF
	
	// if so, error
	if lb_bk_exist = true then
	  messagebox("Input Error", "This book number/book media has already been entered.")
	  return 1
	end if
	
	// use the temp datastore to retrieve the books information
	lbkseq = this.object.mchar_bkseq[row]
	ids.reset()
	ll_row = ids.retrieve(STRING(LBKSEQ), STRING(DATA)) // bkseq and bkmed
	if ll_row = -1 then
		messagebox("Retrieve Error", "Could not query ls_database, please check connection")
	   return 1
	end if
	if ll_row = 0 then
		messagebox("Book not found", "No match for this book was found in the database")
	   return 1
	end if
	
	// sort it, we want the highest stage we can get
	if ll_row > 1 then
		ids.setsort("qastg_qastg D, mchar_med")
		ids.sort()
		ll_row2 = ll_row
	end if
	ll_row2 = 1
	
	// copy the vars from the temp ds to the dw
  	this.object.mchar_bkseq[row] = ids.object.mchar_bkseq[ll_row2]
	this.object.mchar_bkmed[row] = ids.object.mchar_bkmed[ll_row2]
	this.object.ttlinit_ttl[row] = ids.object.ttlinit_ttl[ll_row2]
	this.object.mchar_len[row] = ids.object.mchar_len[ll_row2]
	// 06/19/2008 add Length_
	this.object.mchar_length_[row] = ids.object.mchar_length_[ll_row2]
	this.object.mchar_flash_indicator[row] = ids.object.mchar_flash_indicator[ll_row2]
	this.object.mchar_med[row] = ids.object.mchar_med[ll_row2]
	this.object.mchar_qnty[row] = ids.object.mchar_qnty[ll_row2]
	this.object.mchar_vols[row] = ids.object.mchar_vols[ll_row2]
	this.object.mchar_minlastside[row] = ids.object.mchar_minlastside[ll_row2]
	this.object.mchar_conno[row] = ids.object.mchar_conno[ll_row2]
	this.object.other_media_conno[row] = ids.object.other_media_conno[ll_row2]
	this.object.prod_cntr[row] = ids.object.prod_cntr[ll_row2]
	this.object.prod_bkseq[row] = ids.object.prod_bkseq[ll_row2]
	this.object.prod_bkmed[row] = ids.object.prod_bkmed[ll_row2]
	this.object.prod_prodstage[row] = ids.object.prod_prodstage[ll_row2]
	this.object.qastg_qarecdt[row] = ids.object.qastg_qarecdt[ll_row2]
	this.object.qastg_qastg[row] = ids.object.qastg_qastg[ll_row2]
	
	
	// change status to notmodified so no update takes place, takes two steps
	this.SetItemStatus(row,0,primary!,dataModified!)
	this.SetItemStatus(row,0,primary!,notModified!)
	
	// 12/29/2008 Tracker 2148
	Lsbkmed = this.object.mchar_bkmed[row]
	IF LSBKMED = 'BR' THEN
			ll_length = this.object.mchar_length_[row]
			this.Object.mchar_len[row] = ll_length 
			ll_volumn = round(ll_length / 250,0)
			ll_orig_vol = 	this.Object.mchar_vols[row]
			IF Isnull(ll_orig_vol) THEN
				this.Object.mchar_vols[row] = ll_volumn
			END IF
	END IF
	// 12/29/2008
	
	// if no "len" set column to len, otherwise set to bkno so user can enter another book
	IF IsNull(this.object.mchar_len[row]) THEN
		this.SetColumn("mchar_len")
		ELSE
		this.SetColumn("mchar_bkseq")
	END IF
	
	// fill in default values from last row
	IF IsNull(dw_1.object.qastg_qarecdt[row]) THEN
		dw_1.object.qastg_qarecdt[row] = Today()
	END IF
	
///////////////


//	// if user enters len or minlastside
	
	// 06/19/2008
	
//	CASE "mchar_len", "mchar_vols","mchar_minlastside","qastg_qarecdt"
//12/4/2008

CASE 'mchar_length_'
	Lsbkmed = this.object.mchar_bkmed[row]
	IF LSBKMED = 'BR' THEN
			ll_length = long(data)
			this.Object.mchar_len[row] = ll_length // 12/4/2008
			volumn = round(ll_length / 250,0)
			this.Object.mchar_vols[row] = volumn
		END IF
	// 12/4/2008
	
	CASE  "mchar_vols"
	
		dw_1.AcceptText()
		
		ll_minlastside = dw_1.object.mchar_minlastside[row]
		ll_length = dw_1.object.mchar_len[row]
		ll_qnty = dw_1.object.mchar_qnty[row]
		ll_volumn = Long(data) // dw_1.object.mchar_vols[row] // 06/19/2008
		
		Lcntr = dw_1.object.prod_cntr[row]
		Lbkseq = dw_1.object.prod_bkseq[row]
		Lprodstage = dw_1.object.prod_prodstage[row]
		lmed = dw_1.object.mchar_med[row]
		
		IF dwo.Name <> "qastg_qarecdt" THEN  // do some non-zero validation
			// can't be zero
			IF IsNull(ls_data) OR NOT IsNumber(ls_data)  THEN
				RETURN 1
			END IF
			IF Long(ls_data) < 0 OR Long(ls_data)>99999 AND dwo.Name <> "qastg_qarecdt"THEN
				RETURN 1
			END IF
		ELSE
			
		END IF
		// 06/19/2008 commented out
//		// magic formula to calculate volumes
//		IF dwo.Name = "mchar_len" THEN
//			ll_length = Long(ls_data)
//		END IF
//		IF dwo.Name = "mchar_minlastside" THEN
//			ll_minlastside = Long(ls_data)
//			IF ll_minlastside > 88 THEN
//				RETURN 1
//			END IF
//		END IF
		
		//messagebox("data",Lmed+" "+Lprodstage+" "+string(ll_length)+" "+string(ll_qnty)+" "+string(ll_minlastside)+" "+Lcntr+" "+string(Lbkseq)+" "+string(ll_volumn))

		CHOOSE CASE Trim(this.object.mchar_med[row])
			CASE "RC"
				ll_volumn = Ceiling(ll_length / 4)
	//			dw_1.object.mchar_vols[row] = ll_volumn
				IF IsNull(ll_minlastside)=FALSE THEN
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[row] = ll_totunit
				ELSE
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,0,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[row] = ll_totunit
				END IF
			CASE "RTB"
				ll_volumn = Ceiling(ll_length / 4)
		//		dw_1.Object.mchar_vols[row] = ll_volumn
				IF IsNull(ll_minlastside)=FALSE THEN
					ll_totunit = wf_calculate_units(Lmed,ll_length,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[row] = ll_totunit
				ELSE
					ll_totunit = wf_calculate_units(Lmed,ll_length,ll_qnty,0,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[row] = ll_totunit
				END IF
			CASE "FD"
				ll_volumn = Ceiling(ll_length / 2)
			//	dw_1.object.mchar_vols[row] = ll_volumn
				IF IsNull(ll_minlastside)=FALSE THEN
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[row] = ll_totunit
				ELSE
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,0,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[row] = ll_totunit
				END IF
			CASE "BR"
				IF IsNull(ll_length)=FALSE THEN
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[row] = ll_totunit
				ELSE
					ll_totunit = wf_calculate_units(lmed,0,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[row] = ll_totunit
				END IF
			CASE "P/B"
				IF IsNull(ll_qnty)=FALSE THEN
					ll_totunit = wf_calculate_units(lmed,ll_length,ll_qnty,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[row] = ll_totunit
				ELSE
					ll_totunit = wf_calculate_units(lmed,ll_length,0,ll_minlastside,Lcntr,Lbkseq,ll_volumn)
					dw_1.object.prod_units[row] = ll_totunit
				END IF
			END CHOOSE
		parent.ib_changesmade = TRUE  // changes made, don't forget to query before close

	CASE ELSE
		// changes made
		parent.ib_changesmade = TRUE
END CHOOSE

// save values for use later
CHOOSE CASE dwo.Name
	CASE "mchar_len", "mchar_vols","mchar_minlastside","qastg_qarecdt"
		is_len = String(dw_1.object.mchar_len[row])
		is_vols = String(dw_1.object.mchar_vols[row])
		is_minlastside = String(dw_1.object.mchar_minlastside[row])
		is_qarecdt = String(dw_1.object.qastg_qarecdt[row],'mm/dd/yyyy')
		ib_active = TRUE
END CHOOSE

// save values for use later
CHOOSE CASE dwo.Name
	CASE "mchar_len"
		is_len = data
	CASE "mchar_vols"
		is_vols = data
	CASE "mchar_minlastside"
		is_minlastside = data
	CASE "qastg_qarecdt"
		is_qarecdt = String(data,'mm/dd/yyyy')
END CHOOSE
		

// if we're on the last row, add another
IF row = RowCount() THEN
	InsertRow(0)
END IF
	




end event

event destructor;call super::destructor;destroy ids
end event

event itemerror;call super::itemerror;
IF dwo.Name = "mchar_bkseq" THEN
  return(1)
END IF

// 10/15/2008 suppress error message
IF dwo.Name = "mchar_bkmed" THEN
  return(1)
END IF


end event

event sqlpreview;call super::sqlpreview;//messagebox("SQLPREVIEW",sqlsyntax)
end event

event itemfocuschanged;call super::itemfocuschanged;long ll_minlastside, ll_length, ll_volumn, ll_totunit, Lbkmed, ll_bkseq
string lsbkmed

choose case dwo.Name
	case "qastg_qarecdt"
		if isnull(dw_1.object.qastg_qarecdt[row]) then
			dw_1.object.qastg_qarecdt[row] = today()
		end if
end choose

choose case dwo.Name
	case "mchar_len","mchar_minlastside", "mchar_vols","qastg_qarecdt"
		ll_minlastside = dw_1.object.mchar_minlastside[row]
		ll_length = dw_1.object.mchar_len[row]
		
		// magic formula to calculate volumes
		
		CHOOSE CASE trim(this.object.prod_bkmed[row])
			CASE "DB"
				dw_1.object.prod_units[row] = 1
			CASE "RC"
				ll_volumn = Ceiling(ll_length / 4)
				dw_1.Object.mchar_vols[row] = ll_volumn
				IF IsNull(ll_minlastside)=FALSE THEN
					ll_totunit = ((ll_length - 1) * 88 ) + ll_minlastside
					dw_1.object.prod_units[row] = ll_totunit
				END IF
			CASE "FD"
				ll_volumn = Ceiling(ll_length / 2)
				dw_1.Object.mchar_vols[row] = ll_volumn
				IF IsNull(ll_minlastside)=FALSE THEN
					ll_totunit = ((ll_length - 1) * 88 ) + ll_minlastside
					dw_1.object.prod_units[row] = ll_totunit
				END IF
			END CHOOSE
		Parent.ib_changesmade = true  // changes made, don't forget to query before close

	
end choose
end event

event pfc_deleterow;// override ancestor

integer	li_rc
long ll_row, ll_count

ll_row = getrow()
ll_count = rowcount()

if IsValid (inv_rowmanager) then
	li_rc = inv_rowmanager.event pfc_deleterow () 
else	
	li_rc = this.DeleteRow (0) 
end if

// Notify the Linkage Service 
IF IsValid ( inv_Linkage ) THEN 
	If li_rc > 0 Then inv_Linkage.Event pfc_DeleteRow (0) 
END IF 

if ll_row = ll_count or ll_count=0 then
	setrow(insertrow(0))
end if


return li_rc
end event


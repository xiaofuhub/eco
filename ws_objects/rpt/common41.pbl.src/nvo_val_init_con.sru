$PBExportHeader$nvo_val_init_con.sru
forward
global type nvo_val_init_con from nonvisualobject
end type
end forward

global type nvo_val_init_con from nonvisualobject
end type
global nvo_val_init_con nvo_val_init_con

forward prototypes
public function boolean of_val_casub_ajyfn (string ajyfn, string casub)
public function boolean of_val_ricd (string ricd, string riprevbkmed, long riprevbkseq)
end prototypes

public function boolean of_val_casub_ajyfn (string ajyfn, string casub);ajyfn=RightTrim(ajyfn)
casub=RightTrim(casub)

IF			(Mid(casub,1,2) = "JF") and &
			(ajyfn="JF") THEN
	RETURN TRUE
ELSEIF	(Mid(casub,1,2) = "JN") and &
			(ajyfn="JN") THEN
	RETURN TRUE
ELSEIF	(ajyfn="AN" or ajyfn="YN") and &
			(Mid(casub,1,1) >= "0") and &
			(Mid(casub,1,1) <= "9") THEN
	RETURN TRUE
ELSEIF	(casub="SPA" or casub="OFL") THEN
	RETURN TRUE
ELSEIF	(ajyfn="AF" or ajyfn="YF") and &
			(Mid(casub,1,1) >= "A") and &
			(Mid(casub,1,1) < "J" or Mid(casub,1,1) > "J") and &
			(Mid(casub,1,1) <= "Z") THEN
	RETURN TRUE
ELSE
	RETURN FAlSE
END IF
end function

public function boolean of_val_ricd (string ricd, string riprevbkmed, long riprevbkseq);// Valiadate against ricd and riprevbkmed and riprevbkseq
// ricd and prevbkmed,prevbkseq has a one to one relationship.
// Either both are NULL or both have a value.

IF ((IsNull(ricd) or ricd="") and &
	 (IsNull(riprevbkmed) or riprevbkmed="") and &
	 (IsNull(riprevbkseq) or riprevbkseq=0)) THEN
		RETURN TRUE
ELSEIF ((IsNull(ricd)=FALSE or ricd<>"") and &
		 (IsNull(riprevbkmed) or riprevbkmed="") and &
	    (IsNull(riprevbkseq) or riprevbkseq=0)) THEN
		RETURN FALSE
ELSEIF ((IsNull(ricd) or ricd="") and &
		(IsNull(riprevbkmed)=FALSE  or riprevbkmed<>"") and &
		(IsNull(riprevbkseq)=FALSE  or riprevbkseq<>0)) THEN
		RETURN FALSE
ELSEIF ((IsNull(ricd)=FALSE or ricd<>"") and &
		(IsNull(riprevbkmed)=FALSE  or riprevbkmed<>"") and & 
		(IsNull(riprevbkseq)=FALSE  or riprevbkseq<>0)) THEN
		RETURN TRUE
END IF

end function

on nvo_val_init_con.create
TriggerEvent( this, "constructor" )
end on

on nvo_val_init_con.destroy
TriggerEvent( this, "destructor" )
end on


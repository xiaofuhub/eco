$PBExportHeader$uo_mle.sru
forward
global type uo_mle from u_mle
end type
end forward

global type uo_mle from u_mle
boolean VScrollBar=true
int TextSize=-10
string FaceName="Courier"
FontFamily FontFamily=Modern!
FontPitch FontPitch=Fixed!
end type
global uo_mle uo_mle

forward prototypes
public function integer of_print (integer ai_copies, boolean ab_wordwrap, decimal ad_margin)
end prototypes

public function integer of_print (integer ai_copies, boolean ab_wordwrap, decimal ad_margin);/////////////////////////////////////////////////////////////////////////////////////////
//  Description:The object function prints the contents of this MLE to the
//default printer.
//
//  Arguments:
//ai_copies   (integer)Number of copies of the text in the MLE to be printed
//ab_wordwrap (boolean)Word wrap on or off
//ad_margin(decimal)Left and Right margin on paper when job is printed
//
//  Return values:  >-1 Number of lines printed
//-7 Error opening print job
//-8 Error sending string to print job
//-9 Error closing print job
/////////////////////////////////////////////////////////////////////////////////////////

Long ll_job
Integer li_width, li_cnt, i, li_rc, li_NewWidth, li_start

IF this.LineCount() < 1 THEN return 0

this.SetRedraw(FALSE)

// Save the width of the MLE
li_width = this.width

// Change the width of the MLE so that it will print the full width of the 8.5x11 paper
// This will need to be tweeked if the paper isn't 8.5x11 portrait
IF ab_wordwrap THEN
	// The numbers (2300 and 350) may need to be adjusted depending on the printer in use.
	// For the test printer 2300 pb units way equal to 7.5 inches
	li_NewWidth = 2300 - ( 2 * 350 * ad_margin )
	this.Width = li_NewWidth
ELSE
	// If wordwrap is off make the width of the MLE very large so it won't wrap the words.
	// This will print within 1/4" of the right paper edge, any text that appears after 
	// that will not be printed.
	this.width = 10000
END IF

// Start the loop for the number of copies
FOR i = 1 TO ai_copies

	li_start = 1

	// Open the Print Job
	ll_job = PrintOpen("MultiLineEdit Print Job #" + string(i))
	// IF ll_job = -1 THEN return this.of_SetError(-7)
	
	// Select some text in the line so that it positions the insertion point (file pointer)
	li_rc  = this.SelectText(li_start,2)
	
	// Loop thru the lines in the MLE
	DO WHILE li_rc > 0
		// Print the current line to the print job
		Print(ll_job,(ad_margin * 1000),this.TextLine())
		// IF Print(ll_job,(ad_margin * 1000),this.TextLine()) = -1 THEN return this.of_SetError(-8)
		// Get the starting position of the next line
		li_start = li_start + this.LineLength() + 3
		// Select some text in the line so that it positions the insertion point (file pointer)
		li_rc  = this.SelectText(li_start,2)
		// increment the counter
		li_cnt ++
	LOOP
	
	// Close the Print Job
	PrintClose(ll_job)
	// IF PrintClose(ll_job) = -1 THEN return this.of_SetError(-9)

NEXT

// Restore the width of the MLE
this.Width = li_width

this.SetRedraw(TRUE)

return li_cnt


end function


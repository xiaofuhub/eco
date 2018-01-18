$PBExportHeader$nca_word.sru
$PBExportComments$Word Toolkit V1.0
forward
global type nca_word from nonvisualobject
end type
end forward

global type nca_word from nonvisualobject autoinstantiate
end type

type variables
Private:
Boolean		ib_CheckReg
Boolean		ib_Word97
end variables

forward prototypes
protected function boolean checkreg ()
public function boolean isword ()
public function integer spellcheck (ref string as_text)
protected subroutine replacechar (ref string as_String, string as_From, string as_To)
public function integer grammarcheck (ref string as_text)
public function integer thesaurus (ref string as_text)
public function integer wordcount (string as_Text)
public function integer fileopen (ref string as_text)
public function integer splitdirfile (string as_path, ref string as_dir, ref string as_filespec)
protected function integer lastpos (string as_string, string as_value)
end prototypes

protected function boolean checkreg ();
/********************************************************************
	CheckReg

	<DESC>	This function checks the registry to see if the Word OLE
				Automation entries are available and that atleast Word 97
				or better is available.</DESC>

	<RETURN> boolean:
				<LI> TRUE - Word is available
				<LI> FALSE - Word is not available</RETURN>

	<ACCESS> Protected

	<USAGE>	lb_CheckReg = this.CheckReg()</USAGE>

********************************************************************/
String ls_Val

RegistryGet( 'HKEY_CLASSES_ROOT\Word.Application\CurVer', '', ls_Val )
IF left(ls_Val,4) = 'Word' THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF

end function

public function boolean isword ();
/********************************************************************
	IsWord

	<DESC>	Function Returns whether the word is availabel for OLE
				Automation functions.</DESC>

	<RETURN> Boolean:
				<LI>TRUE - Word available.
				<LI>FALSE - Word not available or version is out of date.
				</RETURN>

	<ACCESS> Public

	<USAGE>	lnac_Word.IsWord()</USAGE>

********************************************************************/

IF NOT ib_CheckReg THEN
	ib_Word97 = this.CheckReg()
	ib_CheckReg = TRUE
END IF

RETURN ib_Word97

end function

public function integer spellcheck (ref string as_text);/********************************************************************
	SpellCheck

	<DESC>	This function uses the Word Spell Checking functions to
				spell check the contents of the passed string. The
				corrected string replaces the passed string.</DESC>

	<RETURN> integer:
				<LI>1, All ok
				<LI>else, function failed</RETURN>

	<ACCESS> Public

	<ARGS>	as_Text: Text to be spell checked.</ARGS>

	<USAGE>	lnca_Word.SpellCheck( ls_Words )</USAGE>

********************************************************************/
OleObject lole_Spell
n_cst_string 	inv_string
Long ll_RC

// First off check that word is available
IF NOT IsWord() THEN RETURN -1

SetPointer( HourGlass! )

// Word is available so replace all CRLF's with CR's
//this.ReplaceChar( as_Text, '~r~n', '~r' )

// Connect to word and do spell check
lole_Spell = CREATE OleObject
ll_RC = lole_Spell.ConnectToNewObject( 'Word.Application' )
IF ll_RC <> 0 THEN
	DESTROY lole_Spell
	RETURN ll_RC
END IF

lole_Spell.Application.Visible = False
lole_Spell.Application.WindowState = 2
lole_spell.application.normaltemplate.saved=TRUE // 02/04/2010 normal.dot spell check problem.
//lole_Spell.Application.Quit Savechanges=FALSE // 2/24/2010
lole_Spell.Documents.Add()
lole_Spell.Selection.TypeText( as_Text )
lole_Spell.Selection.HomeKey()
lole_Spell.Selection.MoveStart()
lole_Spell.ActiveDocument.CheckSpelling()
lole_Spell.ActiveDocument.Select()
as_Text = lole_Spell.Selection.Text
lole_Spell.ActiveDocument.Close( False )

lole_Spell.Application.Quit(0,1,FALSE) // //2/4/2010

lole_Spell.DisconnectObject()
DESTROY lole_Spell

// Change the CR's back to CRLF's
//this.ReplaceChar( as_Text, '~r', '~r~n' )
as_Text =  inv_string.of_GlobalReplace(as_Text, "~r", "")
as_Text =  inv_string.of_GlobalReplace(as_Text, "~n", "")
as_Text =  inv_string.of_GlobalReplace(as_Text, "~r~n", "")

RETURN 1

end function

protected subroutine replacechar (ref string as_String, string as_From, string as_To);
/********************************************************************
	ReplaceChar

	<DESC>	Replace a String with a String. This function is inlined
				for speed but you could replace this with a call to your
				class library equivalent.</DESC>

	<ACCESS> Protected

	<ARGS>	as_String: String to replace the characters in.			
				as_From: String of characters to replace			
				as_To: String of characters to change to</ARGS>

	<USAGE>	this.ReplaceChar( ls_Str, '~r~n', '~r' )</USAGE>

********************************************************************/
Integer li_Pos, li_Width, li_Len, li_LenP1

li_Len = Len( as_From )
li_LenP1 = li_Len + 1
li_Width = Len( as_From ) - 1
li_Pos = Pos( as_String, as_From )
DO WHILE li_Pos > 0
	as_String = Left( as_String, li_Pos -1 ) + as_To + &
		Right( as_String, Len( as_String ) - li_Pos - li_Width )
	li_Pos = Pos( as_String, as_From, li_Pos + li_LenP1 )
LOOP

end subroutine

public function integer grammarcheck (ref string as_text);
/********************************************************************
	GrammarCheck

	<DESC>	This function uses the Word Grammar Checking functions to
				grammar check the contents of the passed string. The
				corrected string replaces the passed string.</DESC>

	<RETURN> integer:
				<LI>1, All ok
				<LI>else, function failed</RETURN>

	<ACCESS> Public

	<ARGS>	as_Text: Text to be grammar checked.</ARGS>

	<USAGE>	lnca_Word.GrammaCheck( ls_Words )</USAGE>

********************************************************************/
OleObject lole_Spell
Long ll_RC

// First off check that word is available
IF NOT IsWord() THEN RETURN -1

SetPointer( HourGlass! )

// Word is available so replace all CRLF's with CR's
this.ReplaceChar( as_Text, '~r~n', '~r' )

// Connect to word and do spell check
lole_Spell = CREATE OleObject
ll_RC = lole_Spell.ConnectToNewObject( 'Word.Application' )
IF ll_RC <> 0 THEN
	DESTROY lole_Spell
	RETURN ll_RC
END IF

lole_Spell.Application.Visible = False
lole_Spell.Application.WindowState = 2
lole_Spell.Documents.Add()
lole_Spell.Selection.TypeText( as_Text )
lole_Spell.Selection.HomeKey()
lole_Spell.Selection.MoveStart()
lole_Spell.ActiveDocument.CheckGrammar()
lole_Spell.ActiveDocument.Select()
as_Text = lole_Spell.Selection.Text
lole_Spell.ActiveDocument.Close( False )
lole_Spell.Application.Quit
lole_Spell.DisconnectObject()
DESTROY lole_Spell

// Change the CR's back to CRLF's
this.ReplaceChar( as_Text, '~r', '~r~n' )

RETURN 1

end function

public function integer thesaurus (ref string as_text);
/********************************************************************
	Thesaurus

	<DESC>	This function uses the Word Dialog function to open the
				Thesaurs dialog. The choosen word replaces the passed
				string.</DESC>

	<RETURN> integer:
				<LI>1, All ok
				<LI>else, function failed</RETURN>

	<ACCESS> Public

	<ARGS>	as_Text: Default/Choosen Word.</ARGS>

	<USAGE>	lnca_Word.Thesaurs( ls_Word )</USAGE>

********************************************************************/
OleObject lole_Dialog
Long ll_RC

// First off check that word is available
IF NOT IsWord() THEN RETURN -1

SetPointer( HourGlass! )

// Make sure there are no CRLF's in the text
this.ReplaceChar( as_Text, '~r~n', '' )

// Connect to word and do Dialog check
lole_Dialog = CREATE OleObject
ll_RC = lole_Dialog.ConnectToNewObject( 'Word.Application' )
IF ll_RC <> 0 THEN
	DESTROY lole_Dialog
	RETURN ll_RC
END IF

lole_Dialog.Application.Visible = False
lole_Dialog.Application.WindowState = 2
lole_Dialog.Documents.Add()
lole_Dialog.Selection.TypeText( as_Text )
lole_Dialog.Selection.HomeKey()
lole_Dialog.Selection.MoveStart()
lole_Dialog.Application.Dialogs[194].Show
lole_Dialog.ActiveDocument.Select()
as_Text = lole_Dialog.Selection.Text
lole_Dialog.ActiveDocument.Close( False )
lole_Dialog.Application.Quit
lole_Dialog.DisconnectObject()
DESTROY lole_Dialog

// Make sure there are no trailing CR's in the text
this.ReplaceChar( as_Text, '~r', '' )

RETURN 1


end function

public function integer wordcount (string as_Text);
/********************************************************************
	WordCount

	<DESC>	This function uses the Word Dialog function to open the
				Word Count dialog. The text passed into the function is
				analysed by the word count dislag.</DESC>

	<RETURN> integer:
				<LI>1, All ok
				<LI>else, function failed</RETURN>

	<ACCESS> Public

	<ARGS>	as_Text: Text to be counted.</ARGS>

	<USAGE>	lnca_Word.WordCount( ls_Word )</USAGE>
	
********************************************************************/
OleObject lole_Dialog
Long ll_RC

// First off check that word is available
IF NOT IsWord() THEN RETURN -1

SetPointer( HourGlass! )

// Replace any CRLF's with CR's
this.ReplaceChar( as_Text, '~r~n', '~r' )

// Connect to word and do Dialog check
lole_Dialog = CREATE OleObject
ll_RC = lole_Dialog.ConnectToNewObject( 'Word.Application' )
IF ll_RC <> 0 THEN
	DESTROY lole_Dialog
	RETURN ll_RC
END IF

lole_Dialog.Application.Visible = False
lole_Dialog.Application.WindowState = 2
lole_Dialog.Documents.Add()
lole_Dialog.Selection.TypeText( as_Text )
lole_Dialog.Selection.HomeKey()
lole_Dialog.Selection.MoveStart()
lole_Dialog.Application.Dialogs[228].Show
lole_Dialog.ActiveDocument.Select()
as_Text = lole_Dialog.Selection.Text
lole_Dialog.ActiveDocument.Close( False )
lole_Dialog.Application.Quit
lole_Dialog.DisconnectObject()
DESTROY lole_Dialog

RETURN 1


end function

public function integer fileopen (ref string as_text);
/********************************************************************
	FileOpen

	<DESC>	This function uses the Word Dialog function to open the
				File Open dialog. The file choosen by the user replaces
				the default directory passed in.
				
				For best effect only pass in the path to the default
				file, otherwise word will give unpredicatable
				results that will confuse your user!</DESC>

	<RETURN> integer:
				<LI>1, All ok
				<LI>-99, User Pressed Cancel
				<LI>else, function failed</RETURN>

	<ACCESS> Public

	<ARGS>	as_Text: Default Path/Choosen File.</ARGS>

	<USAGE>	lnca_Word.fileOpen( ls_File )</USAGE>

********************************************************************/
//OleObject lole_Dialog, lole_Open, lole_Find
//String ls_FN, ls_Path, ls_Spec, ls_Dir
//Long ll_RC
//
//// First off check that word is available
//IF NOT IsWord() THEN RETURN -1
//
//SetPointer( HourGlass! )
//
//// Connect to word and do Dialog check
//lole_Dialog = CREATE OleObject
//ll_RC = lole_Dialog.ConnectToNewObject( 'Word.Application' )
//IF ll_RC <> 0 THEN
//	DESTROY lole_Dialog
//	RETURN ll_RC
//END IF
//
//lole_Dialog.Application.Visible = False
//lole_Dialog.Application.WindowState = 2
//
//// If the developer has passed in a filename then we
//// strip it out into path and file spec. That way
//// we can change the directory to the directory he wants
//// and use the file spec for the dialog.
//this.SplitDirFile( as_Text, ls_Dir, ls_Spec )
//lole_Dialog.Application.ChangeFileOpenDirectory( ls_Dir )
//lole_Open = lole_Dialog.Application.Dialogs[80]
//lole_Open.Name = ls_Spec
//ll_RC = lole_Open.Display
//IF ll_RC <> -1 THEN
//	lole_Dialog.Application.Quit
//	lole_Dialog.DisconnectObject()
//	DESTROY lole_Dialog
//	DESTROY lole_Open
//	DESTROY lole_Find
//	RETURN -99
//END IF
//
//lole_Find = lole_Dialog.Application.Dialogs[99]
//ls_FN = lole_Open.Name
//ls_Path = lole_Find.SearchPath
//lole_Dialog.Application.Quit
//lole_Dialog.DisconnectObject()
//DESTROY lole_Dialog
//DESTROY lole_Open
//DESTROY lole_Find
//
//// Get rid of any quotes in the path or filename
//this.ReplaceChar( ls_FN, '"', '' )
//this.ReplaceChar( ls_Path, '"', '' )
//IF Right( ls_Path, 1 ) <> '\' THEN ls_Path += '\'
//as_Text = ls_Path + ls_FN
//
RETURN 1

end function

public function integer splitdirfile (string as_path, ref string as_dir, ref string as_filespec);// PB6 Bug Fix
/********************************************************************
	SplitDirFile

	<DESC>	Split a Full filename and path into the dir and the file
				spec. For example C:\*.* would be split into C:\ and *.*
				</DESC>

	<RETURN> integer:
				<LI>1, processed ok
				<LI>-1, error spliting path</RETURN>

	<ACCESS> Protected

	<ARGS>	as_path: Full path to split out.
				as_dir: The Dir portion of the path.
				as_filespec: The Filespec of the path.</ARGS>

	<USAGE>	this.SplitDirFile( ls_Path, ls_Dir, ls_FN )</USAGE>

********************************************************************/
Long ll_Pos

IF Len( as_path ) = 0 THEN RETURN -1

ll_Pos = this.LastPos( as_Path, '\' )
IF ll_Pos <= 0 THEN RETURN -1

as_Dir = Left( as_Path, ll_Pos )
IF ll_Pos = Len( as_Path ) THEN
	as_FileSpec = '*.*'
ELSE
	as_FileSpec = Mid( as_Path, ll_Pos + 1 )
END IF

RETURN 1
end function

protected function integer lastpos (string as_string, string as_value);// PB6 Bug Fix
/********************************************************************
	LastPos

	<DESC>	Return the loast position of a substring in a string</DESC>

	<RETURN> integer:
				position in the string</RETURN>

	<ACCESS> Protected

	<ARGS>	as_string: Locate in this string
				as_value: What to find</ARGS>

	<USAGE>	li_Pos = this.LastPos( 'bob', 'b' )</USAGE>

********************************************************************/
Long ll_Pos, ll_OldPos

ll_Pos = Pos( as_String, as_Value )
IF ll_Pos = 0 THEN RETURN ll_Pos

DO WHILE ll_Pos <> 0
	ll_OldPos = ll_Pos
	ll_Pos = Pos( as_String, as_Value, ll_Pos + 1 )
LOOP

RETURN ll_OldPos
end function

on nca_word.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nca_word.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;
/********************************************************************
	nca_word: Word Tookit

	<OBJECT>	This object provides an interface into the Word 97
				Application for useful functions such as Spell Checking
				and Grammar Checking. The interface to Word 97 is through
				OLE Automation.</OBJECT>

	<USAGE>	Use this object to add spell checking and grammar checking
				features to your application. This ibject is auto
				instanciate so just declare the object and call the
				public function of your choice.</USAGE>

	<DESC>	Initialsze object settings.</DESC>

	Date		Ref	Author		Comments
 11/16/98			Ken Howe		First Version
********************************************************************/
ib_CheckReg = FALSE
ib_Word97 = FALSE

end event


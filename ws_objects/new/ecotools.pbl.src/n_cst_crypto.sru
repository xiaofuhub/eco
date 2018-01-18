$PBExportHeader$n_cst_crypto.sru
$PBExportComments$Password Encryption NVO - Uses MS Crytography SHA1 method 06/25/09 Murali
forward
global type n_cst_crypto from nonvisualobject
end type
end forward

global type n_cst_crypto from nonvisualobject
event _readme ( )
end type
global n_cst_crypto n_cst_crypto

type prototypes
Function ulong CryptAcquireContext &
	(Ref ulong phProv, &
	 String pszContainer, &
	 String pszProvider, &
	 ulong dwProvType, &
	 ulong dwFlags) Library "advapi32.dll" Alias For "CryptAcquireContextA;Ansi"
    
Function ulong CryptGetProvParam &
	(ulong hProv, &
	 ulong dwParam, &
	 Ref Blob pbData, &
	 Ref ulong pdwDataLen, &
	 ulong dwFlags) Library "advapi32.dll" Alias For "CryptGetProvParam"

Function ulong CryptCreateHash &
	(ulong hProv, &
	 ulong Algid, &
	 ulong hKey, &
	 ulong dwFlags, &
	 Ref ulong phHash) Library "advapi32.dll" Alias For "CryptCreateHash"

Function ulong CryptHashData &
	(ulong hHash, &
	 String pbData, &
	 ulong dwDataLen, &
	 ulong dwFlags) Library "advapi32.dll" Alias For "CryptHashData;Ansi"

Function ulong CryptDeriveKey &
	(ulong hProv, &
	 ulong Algid, &
	 ulong hBaseData, &
	 ulong dwFlags, &
	 Ref ulong phKey) Library "advapi32.dll" Alias For "CryptDeriveKey"

Function ulong CryptDestroyHash &
    (ulong hHash) Library "advapi32.dll" Alias For "CryptDestroyHash"

Function ulong CryptEncrypt &
	(ulong hKey, &
	 ulong hHash, &
	 ulong Final, &
	 ulong dwFlags, &
	 Ref string pbData, &
	 Ref ulong pdwDataLen, &
	 ulong dwBufLen) Library "advapi32.dll" Alias For "CryptEncrypt;Ansi"

Function ulong CryptDestroyKey &
	(ulong hKey) Library "advapi32.dll" Alias For "CryptDestroyKey"

Function ulong CryptReleaseContext &
	(ulong hProv, &
	 ulong dwFlags) Library "advapi32.dll" Alias For "CryptReleaseContext"

Function ulong CryptDecrypt &
	(ulong hKey, &
	 ulong hHash, &
	 ulong Final, &
	 ulong dwFlags, &
	 Ref String pbData, &
	 Ref ulong pdwDataLen) Library "advapi32.dll" Alias For "CryptDecrypt;Ansi"

Function Long CryptGetHashParam &
( uLong hhash, &
uLong dwParam, &
ref Blob pbData, &
ref uLong dwDataLength, &
uLong dwFlags) Library "advapi32.dll" Alias for "CryptGetHashParam;Ansi"
end prototypes

type variables
Private:

Constant String SERVICE_PROVIDER = "Microsoft Base Cryptographic Provider v1.0"
Constant String KEY_CONTAINER = "MyKeyContainer"
Constant ulong PROV_RSA_FULL = 1
Constant ulong PP_NAME = 4
Constant ulong PP_CONTAINER = 6
Constant ulong CRYPT_NEWKEYSET = 8
Constant ulong ALG_CLASS_DATA_ENCRYPT = 24576
Constant ulong ALG_CLASS_HASH = 32768
Constant ulong ALG_TYPE_ANY = 0
Constant ulong ALG_TYPE_STREAM = 2048
Constant ulong ALG_SID_RC4 = 1
Constant ulong ALG_SID_MD5 = 3
Constant ulong CALG_MD5 = 32771
Constant ulong CALG_RC4 = 26625
Constant ulong ENCRYPT_ALGORITHM = 26625
Constant String NUMBER_ENCRYPT_PASSWORD = "´o¸sçPQ]" // no need to use
Constant ulong CALG_SHA = 32772
Constant ulong HP_HASHVAL =2
end variables

forward prototypes
public function unsignedlong decryptnumber (string snumber)
public function string EncryptNumber (ulong lnumber)
public function string decryptdata (string data, string password)
public function string encryptdata (string data, string password)
public function string encryptdecrypt (string data, string password, boolean encrypt)
public function string getcspdetails ()
public function string of_hash_p (string as_password)
end prototypes

event _readme;/*
'**************************************
' Name: Encryption/Decryption using CryptoAPI
' Description:This is a module that you 
'     can add to your project to encrypt/decrypt using the CryptoAPI.
'     This is the standard API used regardless of who provides
'     the dll which actually does the encryption. Microsoft give
'     you one such dll as standard with windows or NT, but the API 
'     ensures that you have the same interface to anyone elses,
'     or even write your own. These different encryption dlls are
'     called Cryptographic Service Providers (CSP's) and the standard
'     Microsoft one is called "Microsoft Base Cryptographic Provider v1.0".
'     To use a different CSP all you have to do is change a constant in this 
'     module. This module ensures that there are no carriage returns or
'     line feeds in the encrypted value so that you can easily write it
'     to an ini file for example. 
'     This version contains a fix to the original version.
' By: Barry Dunne
'
'
' Inputs:There are two main functions:
'
'Function EncryptData(ByVal Data As String, ByVal Password As String) As String
' Where Data is the String to encrypt and password is used to encrypt it
'
'Function DecryptData(ByVal Data As String, ByVal Password As String) As String
' Where Data is the encrypted String and password is used to decrypt it
'
' Returns:None
'
'Assumes:None
'
'Side Effects:None
'This code is copyrighted and has limited warranties.
'Please see http://www.Planet-Source-Code.com/xq/ASP/txtCodeId.5795/lngWId.1/qx/vb/scripts/ShowCode.htm
'for details.
'**************************************

Option Explicit
'This is based on the 2 MSDN articles
' "Example C Program: Using CryptAcquire Context"
' "Example C Program: Encrypting a File"
'     
'Example usage:
'
'Private Const MY_PASSWORD As String = "isdflkaatdfuhwfnasdf"
'
'Public Sub Main()
'MsgBox EncryptData("hello world", MY_PASSWORD)
'MsgBox DecryptData(EncryptData("hello world", MY_PASSWORD), MY_PASSWORD)
'End Sub


Private Declare Function CryptAcquireContext Lib "advapi32.dll" Alias "CryptAcquireContextA" _
    (ByRef phProv As Long, _
    ByVal pszContainer As String, _
    ByVal pszProvider As String, _
    ByVal dwProvType As Long, _
    ByVal dwFlags As Long) As Long
    


Private Declare Function CryptGetProvParam Lib "advapi32.dll" _
    (ByVal hProv As Long, _
    ByVal dwParam As Long, _
    ByRef pbData As Any, _
    ByRef pdwDataLen As Long, _
    ByVal dwFlags As Long) As Long
    


Private Declare Function CryptCreateHash Lib "advapi32.dll" _
    (ByVal hProv As Long, _
    ByVal Algid As Long, _
    ByVal hKey As Long, _
    ByVal dwFlags As Long, _
    ByRef phHash As Long) As Long
    


Private Declare Function CryptHashData Lib "advapi32.dll" _
    (ByVal hHash As Long, _
    ByVal pbData As String, _
    ByVal dwDataLen As Long, _
    ByVal dwFlags As Long) As Long
    


Private Declare Function CryptDeriveKey Lib "advapi32.dll" _
    (ByVal hProv As Long, _
    ByVal Algid As Long, _
    ByVal hBaseData As Long, _
    ByVal dwFlags As Long, _
    ByRef phKey As Long) As Long
    


Private Declare Function CryptDestroyHash Lib "advapi32.dll" _
    (ByVal hHash As Long) As Long


Private Declare Function CryptEncrypt Lib "advapi32.dll" _
    (ByVal hKey As Long, _
    ByVal hHash As Long, _
    ByVal Final As Long, _
    ByVal dwFlags As Long, _
    ByVal pbData As String, _
    ByRef pdwDataLen As Long, _
    ByVal dwBufLen As Long) As Long


Private Declare Function CryptDestroyKey Lib "advapi32.dll" _
    (ByVal hKey As Long) As Long


Private Declare Function CryptReleaseContext Lib "advapi32.dll" _
    (ByVal hProv As Long, _
    ByVal dwFlags As Long) As Long


Private Declare Function CryptDecrypt Lib "advapi32.dll" _
    (ByVal hKey As Long, _
    ByVal hHash As Long, _
    ByVal Final As Long, _
    ByVal dwFlags As Long, _
    ByVal pbData As String, _
    ByRef pdwDataLen As Long) As Long
    Private Const SERVICE_PROVIDER As String = "Microsoft Base Cryptographic Provider v1.0"
    Private Const KEY_CONTAINER As String = "Metallica"
    Private Const PROV_RSA_FULL As Long = 1
    Private Const PP_NAME As Long = 4
    Private Const PP_CONTAINER As Long = 6
    Private Const CRYPT_NEWKEYSET As Long = 8
    Private Const ALG_CLASS_DATA_ENCRYPT As Long = 24576
    Private Const ALG_CLASS_HASH As Long = 32768
    Private Const ALG_TYPE_ANY As Long = 0
    Private Const ALG_TYPE_STREAM As Long = 2048
    Private Const ALG_SID_RC4 As Long = 1
    Private Const ALG_SID_MD5 As Long = 3
    Private Const CALG_MD5 As Long = ((ALG_CLASS_HASH Or ALG_TYPE_ANY) Or ALG_SID_MD5)
    Private Const CALG_RC4 As Long = ((ALG_CLASS_DATA_ENCRYPT Or ALG_TYPE_STREAM) Or ALG_SID_RC4)
    Private Const ENCRYPT_ALGORITHM As Long = CALG_RC4
    Private Const NUMBER_ENCRYPT_PASSWORD As String = "´o¸sçPQ]"


--Public Function EncryptData(ByVal Data As String, ByVal Password As String) As String
    Dim sEncrypted As String
    Dim lEncryptionCount As Long
    Dim sTempPassword As String
    'It is possible that the normal encrypti
    '     on will give you a string
    'containing cr or lf characters which ma
    '     ke it difficult to write to files
    'Do a loop changing the password and kee
    '     p encrypting until the result is ok
    'To be able to decrypt we need to also s
    '     tore the number of loops in the result
    'Try first encryption
    lEncryptionCount = 0
    sTempPassword = Password & lEncryptionCount
    sEncrypted = EncryptDecrypt(Data, sTempPassword, True)
    'Loop if this contained a bad character


    Do While (InStr(1, sEncrypted, vbCr) > 0) _
        Or (InStr(1, sEncrypted, vbLf) > 0) _
        Or (InStr(1, sEncrypted, Chr$(0)) > 0) _
        Or (InStr(1, sEncrypted, vbTab) > 0)
        
        'Try the next password
        lEncryptionCount = lEncryptionCount + 1
        sTempPassword = Password & lEncryptionCount
        sEncrypted = EncryptDecrypt(Data, sTempPassword, True)
        'Don't go on for ever, 1 billion attempt
        '     s should be plenty


        If lEncryptionCount = 99999999 Then
            Err.Raise vbObjectError + 999, "EncryptData", "This data cannot be successfully encrypted"
            EncryptData = ""
            Exit Function
        End If
    Loop
    'Build encrypted string, starting with n
    '     umber of encryption iterations
    EncryptData = EncryptNumber(lEncryptionCount) & sEncrypted
End Function


--Public Function DecryptData(ByVal Data As String, ByVal Password As String) As String
    Dim lEncryptionCount As Long
    Dim sDecrypted As String
    Dim sTempPassword As String
    'When encrypting we may have gone throug
    '     h a number of iterations
    'How many did we go through?
    lEncryptionCount = DecryptNumber(Mid$(Data, 1, 8))
    'start with the last password and work b
    '     ack
    sTempPassword = Password & lEncryptionCount
    sDecrypted = EncryptDecrypt(Mid$(Data, 9), sTempPassword, False)
    DecryptData = sDecrypted
End Function


--Public Function GetCSPDetails() As String
    Dim hCryptProv As Long
    Dim lLength As Long
    Dim yContainer() As Byte
    'Get handle to CSP


    If CryptAcquireContext(hCryptProv, KEY_CONTAINER, SERVICE_PROVIDER, PROV_RSA_FULL, CRYPT_NEWKEYSET) = 0 Then
        HandleError "Error during CryptAcquireContext for a new key container." & vbCrLf & _
        "A container with this name probably already exists."
        Exit Function
    End If
    'For developer info, show what the CSP &
    '     container name is
    lLength = 1000
    ReDim yContainer(lLength)


    If CryptGetProvParam(hCryptProv, PP_NAME, yContainer(0), lLength, 0) <> 0 Then
        GetCSPDetails = "Cryptographic Service Provider name: " & ByteToStr(yContainer, lLength)
    End If
    lLength = 1000
    ReDim yContainer(lLength)


    If CryptGetProvParam(hCryptProv, PP_CONTAINER, yContainer(0), lLength, 0) <> 0 Then
        GetCSPDetails = GetCSPDetails & vbCrLf & "Key Container name: " & ByteToStr(yContainer, lLength)
    End If
    
    'Release provider handle.


    If hCryptProv <> 0 Then
        CryptReleaseContext hCryptProv, 0
    End If
End Function


--Private Function EncryptDecrypt(ByVal Data As String, ByVal Password As String, ByVal Encrypt As Boolean) As String
    Dim hCryptProv As Long
    Dim lLength As Long
    Dim sTemp As String
    Dim hHash As Long
    Dim hKey As Long
    'Get handle to CSP


    If CryptAcquireContext(hCryptProv, KEY_CONTAINER, SERVICE_PROVIDER, PROV_RSA_FULL, CRYPT_NEWKEYSET) = 0 Then


        If CryptAcquireContext(hCryptProv, KEY_CONTAINER, SERVICE_PROVIDER, PROV_RSA_FULL, 0) = 0 Then
            HandleError "Error during CryptAcquireContext for a new key container." & vbCrLf & _
            "A container with this name probably already exists."
            Exit Function
        End If
    End If
    '---------------------------------------
    '     -----------------------------
    'The data will be encrypted with a sessi
    '     on key derived from the
    'password.
    'The session key will be recreated when 
    '     the data is decrypted
    'only if the password used to create the
    '     key is available.
    '---------------------------------------
    '     -----------------------------
    'Create a hash object.


    If CryptCreateHash(hCryptProv, CALG_MD5, 0, 0, hHash) = 0 Then
        HandleError "Error during CryptCreateHash!"
    End If
    'Hash the password.


    If CryptHashData(hHash, Password, Len(Password), 0) = 0 Then
        HandleError "Error during CryptHashData."
    End If
    'Derive a session key from the hash obje
    '     ct.


    If CryptDeriveKey(hCryptProv, ENCRYPT_ALGORITHM, hHash, 0, hKey) = 0 Then
        HandleError "Error during CryptDeriveKey!"
    End If
    'Do the work
    sTemp = Data
    lLength = Len(Data)


    If Encrypt Then
        'Encrypt data.


        If CryptEncrypt(hKey, 0, 1, 0, sTemp, lLength, lLength) = 0 Then
            HandleError "Error during CryptEncrypt."
        End If
    Else
        'Encrypt data.


        If CryptDecrypt(hKey, 0, 1, 0, sTemp, lLength) = 0 Then
            HandleError "Error during CryptDecrypt."
        End If
    End If
    'This is what we return.
    EncryptDecrypt = Mid$(sTemp, 1, lLength)
    'Destroy session key.


    If hKey <> 0 Then
        CryptDestroyKey hKey
    End If
    'Destroy hash object.


    If hHash <> 0 Then
        CryptDestroyHash hHash
    End If
    
    'Release provider handle.


    If hCryptProv <> 0 Then
        CryptReleaseContext hCryptProv, 0
    End If
End Function


Private Sub HandleError(ByVal Error As String)
    'You could write the error to the screen
    '     or to a file
    Debug.Print Error
End Sub


--Private Function ByteToStr(ByRef ByteArray() As Byte, ByVal lLength As Long) As String
    Dim i As Long


    For i = LBound(ByteArray) To (LBound(ByteArray) + lLength)
        ByteToStr = ByteToStr & Chr$(ByteArray(i))
    Next i
End Function


--Private Function EncryptNumber(ByVal lNumber As Long) As String
    Dim i As Long
    Dim sNumber As String
    sNumber = Format$(lNumber, "00000000")


    For i = 1 To 8
        EncryptNumber = EncryptNumber & Chr$(Asc(Mid$(NUMBER_ENCRYPT_PASSWORD, i, 1)) + Val(Mid$(sNumber, i, 1)))
    Next i
End Function


--Private Function DecryptNumber(ByVal sNumber As String) As Long
    Dim i As Long


    For i = 1 To 8
        DecryptNumber = (10 * DecryptNumber) + (Asc(Mid$(sNumber, i, 1)) - Asc(Mid$(NUMBER_ENCRYPT_PASSWORD, i, 1)))
    Next i
End Function
*/
end event

public function unsignedlong decryptnumber (string snumber);

ulong i
ulong ll_ret

For i = 1 To 8
	ll_ret = (10 * ll_ret) + (Asc(Mid(sNumber, i, 1)) - Asc(Mid(NUMBER_ENCRYPT_PASSWORD, i, 1)))
Next

RETURN ll_ret

end function

public function string EncryptNumber (ulong lnumber);/*
Private Function EncryptNumber(ByVal lNumber As Long) As String
    Dim i As Long
    Dim sNumber As String
    sNumber = Format$(lNumber, "00000000")


    For i = 1 To 8
        EncryptNumber = EncryptNumber & Chr$(Asc(Mid$(NUMBER_ENCRYPT_PASSWORD, i, 1)) + Val(Mid$(sNumber, i, 1)))
    Next i
End Function
*/

Ulong i
String sNumber
String ls_ret

sNumber = String(lNumber, "00000000")

For i = 1 To 8
	ls_ret = ls_ret + Char(Asc(Mid(NUMBER_ENCRYPT_PASSWORD, i, 1)) + long(Mid(sNumber, i, 1)))
Next

RETURN ls_ret
end function

public function string decryptdata (string data, string password);

ulong lEncryptionCount
String sDecrypted
String sTempPassword
String ls_ret

// When encrypting we may have gone through a number of iterations

// How many did we go through?
lEncryptionCount = DecryptNumber(Mid(Data, 1, 8))
// start with the last password and work back
sTempPassword = Password + string(lEncryptionCount)
sDecrypted = EncryptDecrypt(Mid(Data, 9, len(Data)), sTempPassword, False)

ls_ret = sDecrypted

RETURN ls_ret

end function

public function string encryptdata (string data, string password);char vbCr = "~r"
char vbLf = "~n"
char vbTab = "~t"

// Tracker #2214 Murali 06/26/09

String sEncrypted
Ulong lEncryptionCount
String sTempPassword
String ls_ret

// It is possible that the normal encryption will give you a string
// containing cr or lf characters which make it difficult to write to files
// Do a loop changing the password and keep encrypting until the result is ok
// To be able to decrypt we need to also store the number of loops in the result
// Try first encryption

lEncryptionCount = 0

sTempPassword = Password  // + string(lEncryptionCount)
sEncrypted = EncryptDecrypt(Data, sTempPassword, True)

// Loop if this contained a bad character
Do While (Pos(sEncrypted, vbCr, 1) > 0) &
	Or (Pos(sEncrypted, vbLf, 1) > 0) &
	Or (Pos(sEncrypted, Char(0), 1) > 0) &
	Or (Pos(sEncrypted, vbTab, 1) > 0)
        
	// Try the next password
   lEncryptionCount = lEncryptionCount + 1
   sTempPassword = Password + String(lEncryptionCount)
   sEncrypted = EncryptDecrypt(Data, sTempPassword, True)
	// Don't go on for ever, 1 billion attempts should be plenty
   If lEncryptionCount = 99999999 Then
      MessageBox("EncryptData", "This data cannot be successfully encrypted")
		RETURN ""
	End If
Loop
// Build encrypted string, starting with number of encryption iterations
ls_ret = sEncrypted // EncryptNumber(lEncryptionCount) + 

RETURN ls_ret
end function

public function string encryptdecrypt (string data, string password, boolean encrypt);// #2214 06/26/09 encrypt using SHA1 method

ulong hCryptProv
ulong lLength
String sTemp
ulong hHash
ulong hKey

string ls_ret

// Get handle to CSP
If CryptAcquireContext(hCryptProv, KEY_CONTAINER, SERVICE_PROVIDER, PROV_RSA_FULL, CRYPT_NEWKEYSET) = 0 Then
  If CryptAcquireContext(hCryptProv, KEY_CONTAINER, SERVICE_PROVIDER, PROV_RSA_FULL, 0) = 0 Then
		MessageBox("DEBUG", "Error during CryptAcquireContext for a new key container." + "~r~n" + &
		"A container with this name probably already exists.")
		RETURN ""
  End If
End If
// --------------------------------------------------------------------
// The data will be encrypted with a session key derived from the
// password. The session key will be recreated when the data is decrypted
// only if the password used to create the key is available.
// --------------------------------------------------------------------

// Create a hash object.
If CryptCreateHash(hCryptProv, CALG_SHA, 0, 0, hHash) = 0 Then
  MessageBox("DEBUG", "Error during CryptCreateHash!")
End If

// Hash the password.
If CryptHashData(hHash, Password, Len(Password), 0) = 0 Then
  MessageBox("DEBUG", "Error during CryptHashData.")
End If

// Derive a session key from the hash object.
If CryptDeriveKey(hCryptProv, ENCRYPT_ALGORITHM, hHash, 0, hKey) = 0 Then
  MessageBox("DEBUG", "Error during CryptDeriveKey!")
End If

// Do the work
sTemp = Data
lLength = Len(Data)

If Encrypt Then
  // Encrypt data.
  If CryptEncrypt(hKey, 0, 1, 0, sTemp, lLength, lLength) = 0 Then
		MessageBox("DEBUG", "Error during CryptEncrypt.")
  End If
Else
  // Decrypt data.
  If CryptDecrypt(hKey, 0, 1, 0, sTemp, lLength) = 0 Then
		MessageBox("DEBUG", "Error during CryptDecrypt.")
  End If
End If

// This is what we return.
ls_ret = Mid(sTemp, 1, lLength)

// Destroy session key.
If hKey <> 0 Then
  CryptDestroyKey(hKey)
End If

// Destroy hash object.
If hHash <> 0 Then
  CryptDestroyHash(hHash)
End If

// Release provider handle.
If hCryptProv <> 0 Then
  CryptReleaseContext(hCryptProv, 0)
End If

RETURN ls_ret
end function

public function string getcspdetails ();/*
Public Function GetCSPDetails() As String
    Dim hCryptProv As Long
    Dim lLength As Long
    Dim yContainer() As Byte
    'Get handle to CSP


    If CryptAcquireContext(hCryptProv, KEY_CONTAINER, SERVICE_PROVIDER, PROV_RSA_FULL, CRYPT_NEWKEYSET) = 0 Then
        HandleError "Error during CryptAcquireContext for a new key container." & vbCrLf & _
        "A container with this name probably already exists."
        Exit Function
    End If
    'For developer info, show what the CSP &
    '     container name is
    lLength = 1000
    ReDim yContainer(lLength)


    If CryptGetProvParam(hCryptProv, PP_NAME, yContainer(0), lLength, 0) <> 0 Then
        GetCSPDetails = "Cryptographic Service Provider name: " & ByteToStr(yContainer, lLength)
    End If
    lLength = 1000
    ReDim yContainer(lLength)


    If CryptGetProvParam(hCryptProv, PP_CONTAINER, yContainer(0), lLength, 0) <> 0 Then
        GetCSPDetails = GetCSPDetails & vbCrLf & "Key Container name: " & ByteToStr(yContainer, lLength)
    End If
    
    'Release provider handle.


    If hCryptProv <> 0 Then
        CryptReleaseContext hCryptProv, 0
    End If
End Function
*/

ulong hCryptProv
ulong lLength
Blob{1000} yContainer
Blob{1000} empty 
String ls_ret

// Get handle to CSP -- this doesn't work if the key container already exists!
//If CryptAcquireContext(hCryptProv, KEY_CONTAINER, SERVICE_PROVIDER, PROV_RSA_FULL, CRYPT_NEWKEYSET) = 0 Then
//  MessageBox("DEBUG", "Error during CryptAcquireContext for a new key container." + "~r~n" + &
//  "A container with this name probably already exists.")
//  RETURN ""
//End If
// Get handle to CSP
If CryptAcquireContext(hCryptProv, KEY_CONTAINER, SERVICE_PROVIDER, PROV_RSA_FULL, CRYPT_NEWKEYSET) = 0 Then
  If CryptAcquireContext(hCryptProv, KEY_CONTAINER, SERVICE_PROVIDER, PROV_RSA_FULL, 0) = 0 Then
		MessageBox("DEBUG", "Error during CryptAcquireContext for a new key container." + "~r~n" + &
		"A container with this name probably already exists.")
		RETURN ""
  End If
End If

// For developer info, show what the CSP & container name is
lLength = 1000
If CryptGetProvParam(hCryptProv, PP_NAME, yContainer, lLength, 0) <> 0 Then
  ls_ret = "Cryptographic Service Provider name: " + String(yContainer)
End If

lLength = 1000
yContainer = empty
If CryptGetProvParam(hCryptProv, PP_CONTAINER, yContainer, lLength, 0) <> 0 Then
  ls_ret = ls_ret + "~r~n" + "Key Container name: " + String(yContainer)
End If

// Release provider handle.
If hCryptProv <> 0 Then
  CryptReleaseContext(hCryptProv, 0)
End If

RETURN ls_ret
end function

public function string of_hash_p (string as_password);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function : of_hash_P
// Args		:  String password - plain password text entered by user
//	Description:
// 	MS Cryptography 1.0 encrypted password from DB
//	String - Returns  hashed SHA-1 hex value 
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			06/24/2009      4.2							2214
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


// SHA of "The quick brown fox jumps over the lazy dog"
// should be 2fd4e1c67a2d28fced849ee1bb76e7391b93eb12
// 0...-....1....-....2....-....3....-....4

// SHA of "" (empty data)
// should be da39a3ee5e6b4b0d3255bfef95601890afd80709
// 0...-....1....-....2....-....3....-....4

Blob lblb_hash
uLong lul_bloblen

lblb_hash = Blob(Space(20))
lul_bloblen = Len(lblb_hash)

String ls_hashed
Char lc_hash[]
Long ll_from
String ls_HexHashData 
CHAR HexDigits[0 TO 15] = &
{'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'}

ulong hCryptProv
ulong lLength
String sTemp
ulong hHash
ulong hKey
uLong len
Long ret
integer i
blob bl_byte
integer b
integer r
integer l
integer err_number

string ls_result, password

Password =  as_password

// Get handle to CSP
If CryptAcquireContext(hCryptProv, KEY_CONTAINER,SERVICE_PROVIDER, PROV_RSA_FULL, CRYPT_NEWKEYSET) = 0 Then
	If CryptAcquireContext(hCryptProv, KEY_CONTAINER,SERVICE_PROVIDER, PROV_RSA_FULL, 0) = 0 Then
		MessageBox("DEBUG", "Error during CryptAcquireContext for a new key container." + "~r~n" + &
		"A container with this name probably already exists.")
		RETURN ""
	End If
End If

// Create a hash object.
ret = CryptCreateHash(hCryptProv, CALG_SHA, 0, 0, hHash)
If ret = 0 Then
	MessageBox("DEBUG", "Error during CryptCreateHash!")
End If

// Hash the password.
len = Len(Password)
ret = CryptHashData(hHash, Password, len, 0)
If ret = 0 Then
	MessageBox("DEBUG", "Error during CryptHashData.")
End If

// Create a hash object.
If CryptCreateHash(hCryptProv, CALG_SHA, 0, 0, hHash) = 0 Then
	MessageBox("DEBUG", "Error during CryptCreateHash!")
End If

// Hash the password.
len = Len(Password)
If CryptHashData(hHash, Password, len, 0) = 0 Then
	MessageBox("DEBUG", "Error during CryptHashData.")
End If
string ls

// Get the hash value
IF (CryptGetHashParam(hHash, HP_HASHVAL, lblb_hash, lul_bloblen, 0)) <> 0 THEN
	ls = string(lblb_hash)
	FOR i = 1 TO 20
		bl_byte = BlobMid (lblb_hash, i, 1) 
		//b = Asc (String(bl_byte))
		// If we ever use Powerbuilder 10, we will need unicode support:
		 b = Asc (String(bl_byte,EncodingANSI!))
		r = Mod (b, 16) // right 4 bits
		l = b / 16 // left 4 bits
		
		// 0a58cd47cad3f79822abe91e280ad4c0ac58a02f  - Tester12 from pb
		// 0a58cd47cad3f79822abe91e280ad4c0ac58a02f - Tester12 from sha1 calculator
		IF r <=16 AND l <=16 THEN
			LS =  String(bl_byte,EncodingANSI!) // ˜
			ls_result += HexDigits [l] + HexDigits [r]
		ELSE
			// extended chars giving issues use asca function
			LS = String(bl_byte,EncodingANSI!) // ˜
			b = asca(ls)
			r = Mod (b, 16) // right 4 bits
			l = b / 16 // left 4 bits
			ls_result += HexDigits [l] + HexDigits [r]
		END IF
	NEXT
ELSE
	err_number = 99 //GetLastError()
	MessageBox("DEBUG", "gethashparam as hex failed! err: " + string(err_number))
END IF

RETURN ls_result

end function

on n_cst_crypto.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_cst_crypto.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on


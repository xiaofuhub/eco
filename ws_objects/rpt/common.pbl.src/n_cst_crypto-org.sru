$PBExportHeader$n_cst_crypto-org.sru
$PBExportComments$Password Encryption BVO 06/18/09 Murali
forward
global type n_cst_crypto-org from nonvisualobject
end type
end forward

global type n_cst_crypto-org from nonvisualobject
end type
global n_cst_crypto-org n_cst_crypto-org

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
uLong dwFlags) Library "advapi32.dll" Alias for "CryptGetHashParam"

end prototypes

type variables
Private:

Constant String SERVICE_PROVIDER = "Microsoft Base Cryptographic Provider v1.0"
Constant String KEY_CONTAINER = "Metallica"
Constant ulong PROV_RSA_FULL = 1
Constant ulong PP_NAME = 4
Constant ulong PP_CONTAINER = 6
Constant ulong CRYPT_NEWKEYSET =  8
Constant ulong ALG_CLASS_DATA_ENCRYPT = 24576
Constant ulong ALG_CLASS_HASH = 32768
Constant ulong ALG_TYPE_ANY = 0
Constant ulong ALG_TYPE_STREAM = 2048
Constant ulong ALG_SID_RC4 = 1
Constant ulong ALG_SID_MD5 = 3
//Constant ulong CALG_MD5 = ((ALG_CLASS_HASH Or ALG_TYPE_ANY) Or ALG_SID_MD5)
Constant ulong CALG_MD5 = 32771
Constant ulong CALG_SHA = 32772
//Constant ulong CALG_RC4 = ((ALG_CLASS_DATA_ENCRYPT Or ALG_TYPE_STREAM) Or ALG_SID_RC4)
Constant ulong CALG_RC4 = 26625
//Constant ulong ENCRYPT_ALGORITHM = CALG_RC4
Constant ulong ENCRYPT_ALGORITHM = 26625
Constant String NUMBER_ENCRYPT_PASSWORD = "´o¸sçPQ]"
Constant ulong HP_HASHVAL = 2
Constant ulong CRYPT_VERIFYCONTEXT =0
end variables

forward prototypes
public function ulong decryptnumber (string snumber)
public function string encryptnumber (unsignedlong lnumber)
public function string decryptdata (string data, string password)
public function string encryptdata (string data, string password)
public function string encryptdecrypt (string data, string password, boolean encrypt)
public function string getcspdetails ()
public function string of_hash_a_password (string as_password)
end prototypes

public function ulong decryptnumber (string snumber);ulong i
ulong ll_ret

For i = 1 To 8
ll_ret = (10 * ll_ret) + (Asc(Mid(sNumber, i, 1)) - Asc(Mid(NUMBER_ENCRYPT_PASSWORD, i, 1)))
Next

RETURN ll_ret


end function

public function string encryptnumber (unsignedlong lnumber);Ulong i
String sNumber
String ls_ret

sNumber = String(lNumber, "00000000")

For i = 1 To 8
ls_ret = ls_ret + Char(Asc(Mid(NUMBER_ENCRYPT_PASSWORD, i, 1)) + long(Mid(sNumber, i, 1)))
Next

RETURN ls_ret

end function

public function string decryptdata (string data, string password);ulong lEncryptionCount
String sDecrypted
String sTempPassword
String ls_ret

// When encrypting we may have gone through a number of iterations

// How many did we go through?
lEncryptionCount = DecryptNumber(Mid(Data, 1, 8))
// start with the last password and work back
sTempPassword = Password + string(lEncryptionCount)
// uncomment following
sDecrypted = EncryptDecrypt(Mid(Data, 9, len(Data)), sTempPassword, False)

ls_ret = sDecrypted

RETURN ls_ret

end function

public function string encryptdata (string data, string password);char vbCr = "~r"
char vbLf = "~n"
char vbTab = "~t"

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

sTempPassword = Password + string(lEncryptionCount)
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
ls_ret = EncryptNumber(lEncryptionCount) + sEncrypted

RETURN ls_ret


end function

public function string encryptdecrypt (string data, string password, boolean encrypt);ulong hCryptProv
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
If CryptCreateHash(hCryptProv, CALG_MD5, 0, 0, hHash) = 0 Then
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

public function string getcspdetails ();
ulong hCryptProv
ulong lLength
Blob{1000} yContainer
Blob{1000} empty 
String ls_ret

// Get handle to CSP -- this doesn't work if the key container already exists!
//If CryptAcquireContext(hCryptProv, KEY_CONTAINER, SERVICE_PROVIDER, PROV_RSA_FULL, CRYPT_NEWKEYSET) = 0 Then
// MessageBox("DEBUG", "Error during CryptAcquireContext for a new key container." + "~r~n" + &
// "A container with this name probably already exists.")
// RETURN ""
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

public function string of_hash_a_password (string as_password);string Password 
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

string ls_result

Password = 'tt' // as_password

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

// Get the hash value
string ls
IF (CryptGetHashParam(hHash, HP_HASHVAL, lblb_hash, lul_bloblen, 0)) <> 0 THEN
	FOR i = 1 TO 20
		bl_byte = BlobMid (lblb_hash, i, 1) 
		ls = string(bl_byte)
		
	//	b = Asc (String(bl_byte))
		// If we ever use Powerbuilder 10, we will need unicode support:
		 b = Asc (String(bl_byte,EncodingANSI!))
		r = Mod (b, 16) // right 4 bits
		l = b / 16 // left 4 bits
		ls_result += HexDigits [l] + HexDigits [r]
	NEXT
ELSE
	err_number = 99 //GetLastError()
	MessageBox("DEBUG", "gethashparam as hex failed! err: " + string(err_number))
END IF

RETURN ls_result

end function

on n_cst_crypto-org.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_cst_crypto-org.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on


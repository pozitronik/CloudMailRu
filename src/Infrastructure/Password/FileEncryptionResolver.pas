unit FileEncryptionResolver;

{Resolves file encryption cipher for a connection.
	Extracts encryption password retrieval and GUID validation from ConnectionManager
	for better separation of concerns. Follows the same pattern as TAccountCredentialsProvider.}

interface

uses
	CloudSettings,
	PasswordManager,
	PasswordUIProvider,
	CipherValidator,
	AccountsManager,
	TCHandler,
	Logger,
	Cipher;

type
	{Result of password mismatch dialog - determines next action in password retrieval flow}
	TPasswordMismatchResult = (
		pmrAccepted,  {User accepted new password - update stored GUID}
		pmrIgnored,   {User chose to continue without valid password}
		pmrRetry      {User wants to re-enter password}
	);

	IFileEncryptionResolver = interface
		['{D9182176-B3D8-45CE-807D-6E286E9A41BB}']

		{Resolves file encryption for a connection: retrieves password, validates GUID, creates cipher.
			Returns TNullCipher when encryption is disabled or password retrieval fails.
			@param ConnectionName Account name for TC storage key and dialog display
			@param CloudSettings Cloud settings - CryptFilesPassword and encryption mode will be read/updated
			@return ICipher instance (real cipher or TNullCipher)}
		function ResolveCipher(const ConnectionName: WideString; var CloudSettings: TCloudSettings): ICipher;
	end;

	TFileEncryptionResolver = class(TInterfacedObject, IFileEncryptionResolver)
	private
		FPasswordManager: IPasswordManager;
		FPasswordUI: IPasswordUIProvider;
		FCipherValidator: ICipherValidator;
		FAccountsManager: IAccountsManager;
		FTCHandler: ITCHandler;
		FLogger: ILogger;

		function GetFilesPassword(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
		function InitCloudCryptPasswords(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
		function HandlePasswordMismatch(const ConnectionName: WideString; var CloudSettings: TCloudSettings): TPasswordMismatchResult;
	public
		constructor Create(PasswordManager: IPasswordManager; PasswordUI: IPasswordUIProvider; CipherValidator: ICipherValidator; AccountsManager: IAccountsManager; TCHandler: ITCHandler; Logger: ILogger);
		function ResolveCipher(const ConnectionName: WideString; var CloudSettings: TCloudSettings): ICipher;
	end;

	{Null implementation for testing - returns TNullCipher (no encryption)}
	TNullFileEncryptionResolver = class(TInterfacedObject, IFileEncryptionResolver)
	public
		function ResolveCipher(const ConnectionName: WideString; var CloudSettings: TCloudSettings): ICipher;
	end;

implementation

uses
	SysUtils,
	Vcl.Controls,
	System.Generics.Collections,
	System.UITypes,
	WFXTypes,
	CloudConstants,
	LanguageStrings,
	SettingsConstants,
	CipherProfile;

constructor TFileEncryptionResolver.Create(PasswordManager: IPasswordManager; PasswordUI: IPasswordUIProvider; CipherValidator: ICipherValidator; AccountsManager: IAccountsManager; TCHandler: ITCHandler; Logger: ILogger);
begin
	inherited Create;
	FPasswordManager := PasswordManager;
	FPasswordUI := PasswordUI;
	FCipherValidator := CipherValidator;
	FAccountsManager := AccountsManager;
	FTCHandler := TCHandler;
	FLogger := Logger;
end;

function TFileEncryptionResolver.ResolveCipher(const ConnectionName: WideString; var CloudSettings: TCloudSettings): ICipher;
var
	Profile: TCipherProfile;
begin
	Result := TNullCipher.Create;

	{For non-encrypted accounts, skip password retrieval entirely}
	if CloudSettings.AccountSettings.EncryptFilesMode = EncryptModeNone then
		Exit;

	GetFilesPassword(ConnectionName, CloudSettings);

	{Create cipher when encryption is enabled}
	if CloudSettings.AccountSettings.EncryptFilesMode <> EncryptModeNone then
	begin
		{Validate password before creating cipher - GUID check always uses legacy AES/SHA-1}
		if (CloudSettings.AccountSettings.CryptedGUIDFiles <> EmptyWideStr) and
			not FCipherValidator.CheckPasswordGUID(CloudSettings.CryptFilesPassword, CloudSettings.AccountSettings.CryptedGUIDFiles) then
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WRONG_ENCRYPT_PASSWORD);
		end
		else
		begin
			{Resolve cipher profile -- empty or unknown falls back to legacy default}
			if not TCipherProfileRegistry.FindById(CloudSettings.AccountSettings.CipherProfileId, Profile) then
				Profile := TCipherProfileRegistry.GetDefaultProfile;

			Result := Profile.CreateCipher(CloudSettings.CryptFilesPassword);
		end;
	end;
end;

{Depending on the account settings, initializes and retrieves the files encryption password.
	The password retrieves from the TC passwords storage or user input. Returns true if password retrieved, false otherwise.
	If file encryption is not enabled, immediately returns true.}
function TFileEncryptionResolver.InitCloudCryptPasswords(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
var
	crypt_id: WideString;
	StorePassword: Boolean;
begin
	Result := True;
	StorePassword := False;
	crypt_id := ConnectionName + PASSWORD_SUFFIX_FILECRYPT;

	if EncryptModeAlways = CloudSettings.AccountSettings.EncryptFilesMode then {password must be taken from tc storage, otherwise ask user and store password}
	begin
		case FPasswordManager.GetPassword(crypt_id, CloudSettings.CryptFilesPassword) of
			FS_FILE_OK:
				begin
					exit(True);
				end;
			FS_FILE_READERROR: //password not found in store => act like EncryptModeAskOnce
				begin
					CloudSettings.AccountSettings.EncryptFilesMode := EncryptModeAskOnce;
				end;
			FS_FILE_NOTSUPPORTED: //user doesn't know master password
				begin
					exit(False);
				end;
		end;
	end;
	if EncryptModeAskOnce = CloudSettings.AccountSettings.EncryptFilesMode then
	begin
		if mrOk <> FPasswordUI.AskPassword(Format(ASK_ENCRYPTION_PASSWORD, [ConnectionName]), PREFIX_ASK_ENCRYPTION_PASSWORD, CloudSettings.CryptFilesPassword, StorePassword, True, FTCHandler.FindTCWindow) then
			Result := False
	end;
end;

{Prompts user when entered password doesn't match stored GUID.
	Returns user's choice: accept new password, ignore mismatch, or retry entry.}
function TFileEncryptionResolver.HandlePasswordMismatch(const ConnectionName: WideString; var CloudSettings: TCloudSettings): TPasswordMismatchResult;
var
	ActionsList: TDictionary<Int32, WideString>;
begin
	Result := pmrIgnored;
	ActionsList := TDictionary<Int32, WideString>.Create;
	try
		ActionsList.AddOrSetValue(mrYes, PROCEED_UPDATE);
		ActionsList.AddOrSetValue(mrNo, PROCEED_IGNORE);
		ActionsList.AddOrSetValue(mrRetry, PROCEED_RETYPE);

		case FPasswordUI.AskAction(PREFIX_ERR_PASSWORD_MATCH, ERR_PASSWORD_MATCH, ActionsList, FTCHandler.FindTCWindow) of
			mrYes:
				begin
					CloudSettings.AccountSettings.CryptedGUIDFiles := FCipherValidator.GetCryptedGUID(CloudSettings.CryptFilesPassword);
					FAccountsManager.SetCryptedGUID(ConnectionName, CloudSettings.AccountSettings.CryptedGUIDFiles);
					Result := pmrAccepted;
				end;
			mrNo:
				Result := pmrIgnored;
			mrRetry:
				Result := pmrRetry;
		end;
	finally
		ActionsList.Free;
	end;
end;

{Retrieves encryption password for files, validating against stored GUID.
	Returns True if password obtained (or encryption disabled), False if user cancelled.}
function TFileEncryptionResolver.GetFilesPassword(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
var
	PasswordValid: Boolean;
begin
	if CloudSettings.AccountSettings.EncryptFilesMode = EncryptModeNone then
		Exit(True);

	repeat
		if not InitCloudCryptPasswords(ConnectionName, CloudSettings) then
			Exit(False);

		PasswordValid := FCipherValidator.CheckPasswordGUID(CloudSettings.CryptFilesPassword, CloudSettings.AccountSettings.CryptedGUIDFiles);

	until PasswordValid or (HandlePasswordMismatch(ConnectionName, CloudSettings) <> pmrRetry);

	Result := True;
end;

{TNullFileEncryptionResolver}

function TNullFileEncryptionResolver.ResolveCipher(const ConnectionName: WideString; var CloudSettings: TCloudSettings): ICipher;
begin
	Result := TNullCipher.Create;
end;

end.

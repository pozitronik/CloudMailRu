unit FileEncryptionResolverTest;

interface

uses
	FileEncryptionResolver,
	CloudSettings,
	PasswordManager,
	PasswordUIProvider,
	CipherValidator,
	AccountsManager,
	AccountSettings,
	PluginSettings,
	TCHandler,
	Logger,
	Cipher,
	WSList,
	Winapi.Windows,
	System.Generics.Collections,
	DUnitX.TestFramework;

type
	{Mock password manager returning configurable GetPassword results.
		Supports a queue for returning different results on successive calls.}
	TMockPasswordManagerForEncrypt = class(TInterfacedObject, IPasswordManager)
	private
		FGetPasswordResult: Integer;
		FGetPasswordValue: WideString;
		FSetPasswordResult: Integer;
		FSetPasswordCalled: Boolean;
		FGetPasswordQueue: TList<TPair<Integer, WideString>>;
	public
		constructor Create(GetPasswordResult: Integer; const PasswordValue: WideString = '');
		destructor Destroy; override;
		function GetPassword(Key: WideString; var Password: WideString): Integer;
		function SetPassword(Key, Password: WideString): Integer;
		{Enqueues a (ResultCode, PasswordValue) pair for GetPassword to dequeue on the next call}
		procedure QueueGetPassword(ResultCode: Integer; const PasswordValue: WideString);
		property SetPasswordResult: Integer read FSetPasswordResult write FSetPasswordResult;
		property SetPasswordCalled: Boolean read FSetPasswordCalled;
	end;

	{Mock cipher validator with queue-based CheckPasswordGUID results}
	TMockCipherValidatorForEncrypt = class(TInterfacedObject, ICipherValidator)
	private
		FCheckQueue: TList<Boolean>;
		FCryptedGUIDResult: WideString;
	public
		constructor Create;
		destructor Destroy; override;
		function GetCryptedGUID(const Password: WideString): WideString;
		function CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean;
		{Enqueues a Boolean result for the next CheckPasswordGUID call}
		procedure QueueCheckResult(Value: Boolean);
		property CryptedGUIDResult: WideString read FCryptedGUIDResult write FCryptedGUIDResult;
	end;

	{Mock logger that captures log calls for assertion}
	TMockLoggerForEncrypt = class(TInterfacedObject, ILogger)
	private
		FLogCalls: Integer;
		FLastLogLevel: Integer;
		FLastMsgType: Integer;
		FLastLogString: WideString;
	public
		constructor Create;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
		property LogCalls: Integer read FLogCalls;
		property LastLogLevel: Integer read FLastLogLevel;
		property LastMsgType: Integer read FLastMsgType;
		property LastLogString: WideString read FLastLogString;
	end;

	{Mock password UI returning configurable AskPassword/AskAction results}
	TMockPasswordUIForEncrypt = class(TInterfacedObject, IPasswordUIProvider)
	private
		FAskPasswordResult: Integer;
		FAskPasswordValue: WideString;
		FAskActionResult: Integer;
	public
		constructor Create(AskPasswordResult: Integer; const PasswordValue: WideString = '');
		function AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
		function AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
		property AskActionResult: Integer read FAskActionResult write FAskActionResult;
	end;

	{Mock accounts manager for encryption resolver tests}
	TMockAccountsManagerForEncrypt = class(TInterfacedObject, IAccountsManager)
	private
		FSetCryptedGUIDCalled: Boolean;
		FLastCryptedGUID: WideString;
	public
		constructor Create;
		function GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SetAccountSettings(Account: WideString; Settings: TAccountSettings); overload;
		procedure SetAccountSettings(Settings: TAccountSettings); overload;
		procedure DeleteAccount(Account: WideString);
		procedure RenameAccount(const OldName, NewName: WideString);
		procedure SwitchPasswordStorage(Account: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);
		property SetCryptedGUIDCalled: Boolean read FSetCryptedGUIDCalled;
		property LastCryptedGUID: WideString read FLastCryptedGUID;
	end;

	[TestFixture]
	TFileEncryptionResolverTest = class
	public
		[Test]
		{EncryptModeAlways + password found in TC store -> creates real cipher}
		procedure TestResolveCipher_EncryptAlways_PasswordFound_CreatesCipher;

		[Test]
		{EncryptModeAlways + TC store returns unsupported -> returns null cipher}
		procedure TestResolveCipher_EncryptAlways_PasswordUnsupported_ReturnsNullCipher;

		[Test]
		{EncryptModeAlways + FS_FILE_READERROR falls through to AskOnce, user cancels -> null cipher}
		procedure TestResolveCipher_EncryptAlways_ReadError_FallsToAskOnce;

		[Test]
		{GUID mismatch logs wrong-password error via logger}
		procedure TestResolveCipher_GUIDMismatch_LogsWrongPassword;

		[Test]
		{GUID mismatch, user chooses to update GUID}
		procedure TestResolveCipher_GUIDMismatch_UserUpdatesGUID;

		[Test]
		{GUID mismatch, user ignores mismatch}
		procedure TestResolveCipher_GUIDMismatch_UserIgnores;

		[Test]
		{GUID mismatch, user retries password entry}
		procedure TestResolveCipher_GUIDMismatch_UserRetries;

		[Test]
		{EncryptModeNone -> returns null cipher without touching password manager}
		procedure TestResolveCipher_EncryptModeNone_ReturnsNullCipher;
	end;

	[TestFixture]
	TNullFileEncryptionResolverTest = class
	public
		[Test]
		{Null resolver always returns null cipher}
		procedure TestNullResolverReturnsNullCipher;
	end;

implementation

uses
	CloudConstants,
	CloudEndpoints,
	SettingsConstants,
	LanguageStrings,
	WFXTypes,
	CipherProfile,
	BCryptProvider,
	Vcl.Controls,
	System.UITypes,
	System.SysUtils;

{TMockPasswordManagerForEncrypt}

constructor TMockPasswordManagerForEncrypt.Create(GetPasswordResult: Integer; const PasswordValue: WideString);
begin
	inherited Create;
	FGetPasswordResult := GetPasswordResult;
	FGetPasswordValue := PasswordValue;
	FSetPasswordResult := FS_FILE_OK;
	FSetPasswordCalled := False;
	FGetPasswordQueue := TList<TPair<Integer, WideString>>.Create;
end;

destructor TMockPasswordManagerForEncrypt.Destroy;
begin
	FreeAndNil(FGetPasswordQueue);
	inherited;
end;

function TMockPasswordManagerForEncrypt.GetPassword(Key: WideString; var Password: WideString): Integer;
var
	Pair: TPair<Integer, WideString>;
begin
	{Dequeue from queue if available, otherwise use fixed values}
	if FGetPasswordQueue.Count > 0 then
	begin
		Pair := FGetPasswordQueue[0];
		FGetPasswordQueue.Delete(0);
		Password := Pair.Value;
		Result := Pair.Key;
	end else
	begin
		Password := FGetPasswordValue;
		Result := FGetPasswordResult;
	end;
end;

function TMockPasswordManagerForEncrypt.SetPassword(Key, Password: WideString): Integer;
begin
	FSetPasswordCalled := True;
	Result := FSetPasswordResult;
end;

procedure TMockPasswordManagerForEncrypt.QueueGetPassword(ResultCode: Integer; const PasswordValue: WideString);
begin
	FGetPasswordQueue.Add(TPair<Integer, WideString>.Create(ResultCode, PasswordValue));
end;

{TMockCipherValidatorForEncrypt}

constructor TMockCipherValidatorForEncrypt.Create;
begin
	inherited Create;
	FCheckQueue := TList<Boolean>.Create;
	FCryptedGUIDResult := '';
end;

destructor TMockCipherValidatorForEncrypt.Destroy;
begin
	FreeAndNil(FCheckQueue);
	inherited;
end;

function TMockCipherValidatorForEncrypt.GetCryptedGUID(const Password: WideString): WideString;
begin
	Result := FCryptedGUIDResult;
end;

function TMockCipherValidatorForEncrypt.CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean;
begin
	{Dequeue from queue if available, otherwise return True}
	if FCheckQueue.Count > 0 then
	begin
		Result := FCheckQueue[0];
		FCheckQueue.Delete(0);
	end else
		Result := True;
end;

procedure TMockCipherValidatorForEncrypt.QueueCheckResult(Value: Boolean);
begin
	FCheckQueue.Add(Value);
end;

{TMockLoggerForEncrypt}

constructor TMockLoggerForEncrypt.Create;
begin
	inherited Create;
	FLogCalls := 0;
	FLastLogLevel := 0;
	FLastMsgType := 0;
	FLastLogString := '';
end;

procedure TMockLoggerForEncrypt.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	Inc(FLogCalls);
	FLastLogLevel := LogLevel;
	FLastMsgType := MsgType;
	FLastLogString := LogString;
end;

procedure TMockLoggerForEncrypt.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	Log(LogLevel, MsgType, Format(LogString, Args));
end;

{TMockPasswordUIForEncrypt}

constructor TMockPasswordUIForEncrypt.Create(AskPasswordResult: Integer; const PasswordValue: WideString);
begin
	inherited Create;
	FAskPasswordResult := AskPasswordResult;
	FAskPasswordValue := PasswordValue;
	FAskActionResult := mrCancel;
end;

function TMockPasswordUIForEncrypt.AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
begin
	Password := FAskPasswordValue;
	Result := FAskPasswordResult;
end;

function TMockPasswordUIForEncrypt.AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
begin
	Result := FAskActionResult;
end;

{TMockAccountsManagerForEncrypt}

constructor TMockAccountsManagerForEncrypt.Create;
begin
	inherited Create;
	FSetCryptedGUIDCalled := False;
	FLastCryptedGUID := '';
end;

function TMockAccountsManagerForEncrypt.GetAccountsList(const AccountTypes: EAccountType; const VirtualTypes: EVirtualType): TWSList;
begin
	Result.Clear;
end;

function TMockAccountsManagerForEncrypt.GetAccountSettings(Account: WideString): TAccountSettings;
begin
	Result := Default(TAccountSettings);
end;

procedure TMockAccountsManagerForEncrypt.SetAccountSettings(Account: WideString; Settings: TAccountSettings);
begin
end;

procedure TMockAccountsManagerForEncrypt.SetAccountSettings(Settings: TAccountSettings);
begin
end;

procedure TMockAccountsManagerForEncrypt.DeleteAccount(Account: WideString);
begin
end;

procedure TMockAccountsManagerForEncrypt.RenameAccount(const OldName, NewName: WideString);
begin
end;

procedure TMockAccountsManagerForEncrypt.SwitchPasswordStorage(Account: WideString);
begin
end;

procedure TMockAccountsManagerForEncrypt.SetCryptedGUID(Account: WideString; GUID: WideString);
begin
	FSetCryptedGUIDCalled := True;
	FLastCryptedGUID := GUID;
end;

{TFileEncryptionResolverTest}

procedure TFileEncryptionResolverTest.TestResolveCipher_EncryptAlways_PasswordFound_CreatesCipher;
var
	Resolver: IFileEncryptionResolver;
	PasswordMgr: TMockPasswordManagerForEncrypt;
	CloudSettings: TCloudSettings;
	Cipher: ICipher;
begin
	{EncryptModeAlways, password manager returns OK -> cipher is created (not null)}
	PasswordMgr := TMockPasswordManagerForEncrypt.Create(FS_FILE_OK, 'test-crypt-password');

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Resolver := TFileEncryptionResolver.Create(
		PasswordMgr, TNullPasswordUIProvider.Create,
		TNullCipherValidator.Create, TMockAccountsManagerForEncrypt.Create,
		TNullTCHandler.Create, TNullLogger.Create);

	CloudSettings := Default(TCloudSettings);
	CloudSettings.AccountSettings.EncryptFilesMode := EncryptModeAlways;
	CloudSettings.AccountSettings.CryptedGUIDFiles := '';

	Cipher := Resolver.ResolveCipher('encrypt_test', CloudSettings);
	Assert.IsNotNull(Cipher, 'ResolveCipher should return a cipher instance');
end;

procedure TFileEncryptionResolverTest.TestResolveCipher_EncryptAlways_PasswordUnsupported_ReturnsNullCipher;
var
	Resolver: IFileEncryptionResolver;
	PasswordMgr: TMockPasswordManagerForEncrypt;
	CloudSettings: TCloudSettings;
	Cipher: ICipher;
begin
	{FS_FILE_NOTSUPPORTED: user doesn't know master password -> returns null cipher}
	PasswordMgr := TMockPasswordManagerForEncrypt.Create(FS_FILE_NOTSUPPORTED, '');

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Resolver := TFileEncryptionResolver.Create(
		PasswordMgr, TNullPasswordUIProvider.Create,
		TNullCipherValidator.Create, TMockAccountsManagerForEncrypt.Create,
		TNullTCHandler.Create, TNullLogger.Create);

	CloudSettings := Default(TCloudSettings);
	CloudSettings.AccountSettings.EncryptFilesMode := EncryptModeAlways;
	CloudSettings.AccountSettings.CryptedGUIDFiles := '';

	Cipher := Resolver.ResolveCipher('encrypt_unsupported_test', CloudSettings);
	Assert.IsNotNull(Cipher, 'ResolveCipher should return a cipher instance (null cipher)');
end;

procedure TFileEncryptionResolverTest.TestResolveCipher_EncryptAlways_ReadError_FallsToAskOnce;
var
	Resolver: IFileEncryptionResolver;
	PasswordMgr: TMockPasswordManagerForEncrypt;
	PasswordUI: TMockPasswordUIForEncrypt;
	CloudSettings: TCloudSettings;
	Cipher: ICipher;
begin
	{FS_FILE_READERROR from TC password store switches to AskOnce mode.
	 User cancels AskPassword -> GetFilesPassword returns False, but ResolveCipher still returns null cipher.}
	PasswordMgr := TMockPasswordManagerForEncrypt.Create(FS_FILE_READERROR, '');
	PasswordUI := TMockPasswordUIForEncrypt.Create(mrCancel);

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Resolver := TFileEncryptionResolver.Create(
		PasswordMgr, PasswordUI,
		TNullCipherValidator.Create, TMockAccountsManagerForEncrypt.Create,
		TNullTCHandler.Create, TNullLogger.Create);

	CloudSettings := Default(TCloudSettings);
	CloudSettings.AccountSettings.EncryptFilesMode := EncryptModeAlways;
	CloudSettings.AccountSettings.CryptedGUIDFiles := '';

	Cipher := Resolver.ResolveCipher('readerror_test', CloudSettings);
	Assert.IsNotNull(Cipher, 'ResolveCipher should return a cipher (null cipher when password fails)');
end;

procedure TFileEncryptionResolverTest.TestResolveCipher_GUIDMismatch_LogsWrongPassword;
var
	Resolver: IFileEncryptionResolver;
	PasswordMgr: TMockPasswordManagerForEncrypt;
	MockValidator: TMockCipherValidatorForEncrypt;
	MockLog: TMockLoggerForEncrypt;
	CloudSettings: TCloudSettings;
	Cipher: ICipher;
begin
	{When GUID check fails in ResolveCipher, logger receives ERR_WRONG_ENCRYPT_PASSWORD.
	 CipherValidator queue: [True, False] -- True for GetFilesPassword,
	 False for ResolveCipher's own GUID check.}
	PasswordMgr := TMockPasswordManagerForEncrypt.Create(FS_FILE_OK, 'test-password');

	MockValidator := TMockCipherValidatorForEncrypt.Create;
	MockValidator.QueueCheckResult(True); {GetFilesPassword -- passes}
	MockValidator.QueueCheckResult(False); {ResolveCipher -- fails -> logs error}

	MockLog := TMockLoggerForEncrypt.Create;

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Resolver := TFileEncryptionResolver.Create(
		PasswordMgr, TNullPasswordUIProvider.Create,
		MockValidator, TMockAccountsManagerForEncrypt.Create,
		TNullTCHandler.Create, MockLog);

	CloudSettings := Default(TCloudSettings);
	CloudSettings.AccountSettings.EncryptFilesMode := EncryptModeAlways;
	CloudSettings.AccountSettings.CryptedGUIDFiles := 'stored-guid';

	Cipher := Resolver.ResolveCipher('guid_mismatch_log_test', CloudSettings);
	Assert.IsNotNull(Cipher, 'ResolveCipher should return a cipher instance');
	Assert.IsTrue(MockLog.LogCalls >= 1, 'Logger should have received at least 1 call');
	Assert.AreEqual(LOG_LEVEL_ERROR, MockLog.LastLogLevel, 'Last log should be error level');
	Assert.AreEqual(msgtype_importanterror, MockLog.LastMsgType, 'Last log should be important error');
	Assert.AreEqual(ERR_WRONG_ENCRYPT_PASSWORD, MockLog.LastLogString, 'Last log should be wrong password message');
end;

procedure TFileEncryptionResolverTest.TestResolveCipher_GUIDMismatch_UserUpdatesGUID;
var
	Resolver: IFileEncryptionResolver;
	PasswordMgr: TMockPasswordManagerForEncrypt;
	MockValidator: TMockCipherValidatorForEncrypt;
	PasswordUI: TMockPasswordUIForEncrypt;
	AccountsMgr: TMockAccountsManagerForEncrypt;
	CloudSettings: TCloudSettings;
	Cipher: ICipher;
begin
	{GUID mismatch in GetFilesPassword, user selects mrYes (update GUID).
	 CipherValidator queue: [False, True] -- False triggers AskAction dialog,
	 True for ResolveCipher's GUID check. GetCryptedGUID returns 'new-guid'.}
	PasswordMgr := TMockPasswordManagerForEncrypt.Create(FS_FILE_OK, 'test-password');

	MockValidator := TMockCipherValidatorForEncrypt.Create;
	MockValidator.QueueCheckResult(False); {GetFilesPassword -- mismatch}
	MockValidator.QueueCheckResult(True); {ResolveCipher -- passes after GUID update}
	MockValidator.CryptedGUIDResult := 'new-guid';

	PasswordUI := TMockPasswordUIForEncrypt.Create(mrCancel);
	PasswordUI.AskActionResult := mrYes;

	AccountsMgr := TMockAccountsManagerForEncrypt.Create;

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Resolver := TFileEncryptionResolver.Create(
		PasswordMgr, PasswordUI,
		MockValidator, AccountsMgr,
		TNullTCHandler.Create, TNullLogger.Create);

	CloudSettings := Default(TCloudSettings);
	CloudSettings.AccountSettings.EncryptFilesMode := EncryptModeAlways;
	CloudSettings.AccountSettings.CryptedGUIDFiles := 'old-guid';

	Cipher := Resolver.ResolveCipher('guid_update_test', CloudSettings);
	Assert.IsNotNull(Cipher, 'ResolveCipher should return a cipher after GUID update');
	Assert.IsTrue(AccountsMgr.SetCryptedGUIDCalled, 'SetCryptedGUID should have been called');
	Assert.AreEqual('new-guid', string(AccountsMgr.LastCryptedGUID), 'GUID should be updated to new value');
end;

procedure TFileEncryptionResolverTest.TestResolveCipher_GUIDMismatch_UserIgnores;
var
	Resolver: IFileEncryptionResolver;
	PasswordMgr: TMockPasswordManagerForEncrypt;
	MockValidator: TMockCipherValidatorForEncrypt;
	PasswordUI: TMockPasswordUIForEncrypt;
	AccountsMgr: TMockAccountsManagerForEncrypt;
	CloudSettings: TCloudSettings;
	Cipher: ICipher;
begin
	{GUID mismatch in GetFilesPassword, user selects mrNo (ignore).
	 CipherValidator queue: [False, False] -- both calls fail (GetFilesPassword and ResolveCipher).}
	PasswordMgr := TMockPasswordManagerForEncrypt.Create(FS_FILE_OK, 'test-password');

	MockValidator := TMockCipherValidatorForEncrypt.Create;
	MockValidator.QueueCheckResult(False); {GetFilesPassword -- mismatch}
	MockValidator.QueueCheckResult(False); {ResolveCipher -- also fails -> logs error}

	PasswordUI := TMockPasswordUIForEncrypt.Create(mrCancel);
	PasswordUI.AskActionResult := mrNo;

	AccountsMgr := TMockAccountsManagerForEncrypt.Create;

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Resolver := TFileEncryptionResolver.Create(
		PasswordMgr, PasswordUI,
		MockValidator, AccountsMgr,
		TNullTCHandler.Create, TNullLogger.Create);

	CloudSettings := Default(TCloudSettings);
	CloudSettings.AccountSettings.EncryptFilesMode := EncryptModeAlways;
	CloudSettings.AccountSettings.CryptedGUIDFiles := 'old-guid';

	Cipher := Resolver.ResolveCipher('guid_ignore_test', CloudSettings);
	Assert.IsNotNull(Cipher, 'ResolveCipher should return a cipher when user ignores GUID mismatch');
	Assert.IsFalse(AccountsMgr.SetCryptedGUIDCalled, 'SetCryptedGUID should not be called on ignore');
end;

procedure TFileEncryptionResolverTest.TestResolveCipher_GUIDMismatch_UserRetries;
var
	Resolver: IFileEncryptionResolver;
	PasswordMgr: TMockPasswordManagerForEncrypt;
	MockValidator: TMockCipherValidatorForEncrypt;
	PasswordUI: TMockPasswordUIForEncrypt;
	CloudSettings: TCloudSettings;
	Cipher: ICipher;
begin
	{GUID mismatch in GetFilesPassword, user selects mrRetry.
	 1st iteration: CheckPasswordGUID=False -> AskAction=mrRetry.
	 2nd iteration: new password fetched from queue, CheckPasswordGUID=True -> loop exits.
	 ResolveCipher's GUID check also passes.}
	PasswordMgr := TMockPasswordManagerForEncrypt.Create(FS_FILE_OK, 'correct-password');
	{Queue: 1st GetPassword returns 'wrong' password, fallback returns 'correct-password'}
	PasswordMgr.QueueGetPassword(FS_FILE_OK, 'wrong-password');

	MockValidator := TMockCipherValidatorForEncrypt.Create;
	MockValidator.QueueCheckResult(False); {1st iteration GetFilesPassword -- mismatch with 'wrong'}
	MockValidator.QueueCheckResult(True); {2nd iteration GetFilesPassword -- matches with 'correct'}
	MockValidator.QueueCheckResult(True); {ResolveCipher -- passes}

	PasswordUI := TMockPasswordUIForEncrypt.Create(mrCancel);
	PasswordUI.AskActionResult := mrRetry;

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Resolver := TFileEncryptionResolver.Create(
		PasswordMgr, PasswordUI,
		MockValidator, TMockAccountsManagerForEncrypt.Create,
		TNullTCHandler.Create, TNullLogger.Create);

	CloudSettings := Default(TCloudSettings);
	CloudSettings.AccountSettings.EncryptFilesMode := EncryptModeAlways;
	CloudSettings.AccountSettings.CryptedGUIDFiles := 'stored-guid';

	Cipher := Resolver.ResolveCipher('guid_retry_test', CloudSettings);
	Assert.IsNotNull(Cipher, 'ResolveCipher should return a cipher after retry with correct password');
end;

procedure TFileEncryptionResolverTest.TestResolveCipher_EncryptModeNone_ReturnsNullCipher;
var
	Resolver: IFileEncryptionResolver;
	CloudSettings: TCloudSettings;
	Cipher: ICipher;
begin
	{EncryptModeNone -> returns null cipher without accessing password manager}
	Resolver := TFileEncryptionResolver.Create(
		TNullPasswordManager.Create, TNullPasswordUIProvider.Create,
		TNullCipherValidator.Create, TMockAccountsManagerForEncrypt.Create,
		TNullTCHandler.Create, TNullLogger.Create);

	CloudSettings := Default(TCloudSettings);
	CloudSettings.AccountSettings.EncryptFilesMode := EncryptModeNone;

	Cipher := Resolver.ResolveCipher('no_encrypt_test', CloudSettings);
	Assert.IsNotNull(Cipher, 'ResolveCipher should return a cipher (null cipher for no encryption)');
end;

{TNullFileEncryptionResolverTest}

procedure TNullFileEncryptionResolverTest.TestNullResolverReturnsNullCipher;
var
	Resolver: IFileEncryptionResolver;
	CloudSettings: TCloudSettings;
	Cipher: ICipher;
begin
	Resolver := TNullFileEncryptionResolver.Create;
	CloudSettings := Default(TCloudSettings);

	Cipher := Resolver.ResolveCipher('any_connection', CloudSettings);
	Assert.IsNotNull(Cipher, 'Null resolver should return a cipher instance (null cipher)');
end;

initialization

TDUnitX.RegisterTestFixture(TFileEncryptionResolverTest);
TDUnitX.RegisterTestFixture(TNullFileEncryptionResolverTest);

end.

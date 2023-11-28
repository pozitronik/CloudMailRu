unit AbstractAccountSettings;

{This base class contains and handles only the set of abstract parameters variables}
interface

type
	TAbstractAccountSettings = class abstract
	protected
		{Todo: rename to CamelCase}
		FName: WideString;
		FEmail: WideString;
		FPassword: WideString;
		FUseTCPasswordManager: boolean;
		FTwostepAuth: boolean;
		FUnlimitedFileSize: boolean;
		FSplitLargeFiles: boolean;
		FPublicAccount: boolean;
		FPublicUrl: WideString;
		FDescription: WideString;
		FEncryptFilesMode: integer;
		FEncryptFileNames: boolean;
		FShardOverride: WideString; //hidden option, allows to override working shard for account
		FUploadUrlOverride: WideString; //hidden option, alows to override upload server for account
		FCryptedGUIDFiles: WideString;

		FSaveOnChange: boolean;
	private
		procedure SetDescription(const Value: WideString);
		procedure SetEmail(const Value: WideString);
		procedure SetEncryptFilenames(const Value: boolean);
		procedure SetEncryptFilesMode(const Value: integer);
		procedure SetPublicAccount(const Value: boolean);
		procedure SetPublicUrl(const Value: WideString);
		procedure SetShardOverride(const Value: WideString);
		procedure SetSplitLargeFiles(const Value: boolean);
		procedure SetTwostepAuth(const Value: boolean);
		procedure SetUnlimitedFileSize(const Value: boolean);
		procedure SetUploadUrlOverride(const Value: WideString);
		procedure SetUseTCPasswordManager(const Value: boolean);
		procedure SetName(const Value: WideString);
		procedure SetPassword(const Value: WideString);
		procedure SetCryptedGUIDFiles(const Value: WideString);
	public
		property Name: WideString read FName write SetName;
		property Email: WideString read FEmail write SetEmail;
		property Password: WideString read FPassword write SetPassword;
		property UseTCPasswordManager: boolean read FUseTCPasswordManager write SetUseTCPasswordManager;
		property TwostepAuth: boolean read FTwostepAuth write SetTwostepAuth;
		property UnlimitedFilesize: boolean read FUnlimitedFileSize write SetUnlimitedFileSize;
		property SplitLargeFiles: boolean read FSplitLargeFiles write SetSplitLargeFiles;
		property PublicAccount: boolean read FPublicAccount write SetPublicAccount;
		property PublicUrl: WideString read FPublicUrl write SetPublicUrl;
		property Description: WideString read FDescription write SetDescription;
		property EncryptFilesMode: integer read FEncryptFilesMode write SetEncryptFilesMode;
		property EncryptFilenames: boolean read FEncryptFileNames write SetEncryptFilenames;
		property ShardOverride: WideString read FShardOverride write SetShardOverride;
		property UploadUrlOverride: WideString read FUploadUrlOverride write SetUploadUrlOverride;
		property CryptedGUIDFiles: WideString read FCryptedGUIDFiles write SetCryptedGUIDFiles;

		property SaveOnChange: boolean read FSaveOnChange write FSaveOnChange;
		procedure SetSettingValue(OptionName: WideString; OptionValue: Variant); virtual; abstract;
		procedure Save(); virtual; abstract;
	end;

implementation

{TAbstractAccountSettings}

procedure TAbstractAccountSettings.SetCryptedGUIDFiles(const Value: WideString);
begin
	FCryptedGUIDFiles := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetDescription(const Value: WideString);
begin
	FDescription := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetEmail(const Value: WideString);
begin
	FEmail := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetEncryptFilenames(const Value: boolean);
begin
	FEncryptFileNames := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetEncryptFilesMode(const Value: integer);
begin
	FEncryptFilesMode := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetName(const Value: WideString);
begin
	FName := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetPassword(const Value: WideString);
begin
	FPassword := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetPublicAccount(const Value: boolean);
begin
	FPublicAccount := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetPublicUrl(const Value: WideString);
begin
	FPublicUrl := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetShardOverride(const Value: WideString);
begin
	FShardOverride := Value;
end;

procedure TAbstractAccountSettings.SetSplitLargeFiles(const Value: boolean);
begin
	FSplitLargeFiles := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetTwostepAuth(const Value: boolean);
begin
	FTwostepAuth := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetUnlimitedFileSize(const Value: boolean);
begin
	FUnlimitedFileSize := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetUploadUrlOverride(const Value: WideString);
begin
	FUploadUrlOverride := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractAccountSettings.SetUseTCPasswordManager(const Value: boolean);
begin
	FUseTCPasswordManager := Value;
	if FSaveOnChange then
		Save();
end;

end.

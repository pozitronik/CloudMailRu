unit AccountSettings;

interface

uses
	SysUtils,
	ParsingHelper,
	PathHelper,
	CloudConstants,
	SettingsConstants;

type
	EAccountType = set of (ATPrivate, ATPublic);
	EVirtualType = set of (VTTrash, VTShared, VTInvites);

	TAccountSettings = record
		Account: WideString; {The account name itself}
		Email: WideString;
		Password: WideString;
		UseTCPasswordManager: Boolean;
		TwostepAuth: Boolean;
		UnlimitedFileSize: Boolean;
		SplitLargeFiles: Boolean;
		PublicAccount: Boolean;
		Description: WideString;
		EncryptFilesMode: Integer;
		EncryptFileNames: Boolean;
		ShardOverride: WideString; //hidden option, allows to override working shard for account
		UploadUrlOverride: WideString; //hidden option, allows to override upload server for account
		CryptedGUIDFiles: WideString; //The hash of files encryption password to check its validity
		AuthMethod: Integer; //Authentication method: 0=classic web, 4=OAuth app password
		UseAppPassword: Boolean; //True if password is an app password (for OAuth)
	private
		FUser: WideString;
		FDomain: WideString;
		FPublicUrl: WideString;
		function GetAccountType: EAccountType;
		function GetIsRemoteDescriptionsSupported: Boolean;
		function GetDomain: WideString;
		function GetUser: WideString;
		function GetPublicUrl: WideString;
	public
		property User: WideString read GetUser;
		property Domain: WideString read GetDomain;
		property PublicUrl: WideString read GetPublicUrl write FPublicUrl;
		property IsRemoteDescriptionsSupported: Boolean read GetIsRemoteDescriptionsSupported;
		property AccountType: EAccountType read GetAccountType;
	end;

implementation

{TAccountSettings}

function TAccountSettings.GetAccountType: EAccountType;
begin
	if self.PublicAccount then
		exit([ATPublic]);
	exit([ATPrivate]);
end;

function TAccountSettings.GetDomain: WideString;
begin
	if FDomain = EmptyWideStr then
		ExtractEmailParts(Email, FUser, FDomain);
	result := FDomain;
end;

function TAccountSettings.GetIsRemoteDescriptionsSupported: Boolean;
begin
	result := not((EncryptFilesMode <> EncryptModeNone) and EncryptFileNames);
end;

function TAccountSettings.GetUser: WideString;
begin
	if FUser = EmptyWideStr then
		ExtractEmailParts(Email, FUser, FDomain);
	result := FUser;
end;

{Returns PublicUrl with guaranteed trailing slash for consistent URL handling}
function TAccountSettings.GetPublicUrl: WideString;
begin
	if FPublicUrl = EmptyWideStr then
		Exit(EmptyWideStr);
	Result := IncludeSlash(FPublicUrl);
end;

end.

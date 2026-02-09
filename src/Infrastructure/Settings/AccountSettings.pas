unit AccountSettings;

interface

uses
	SysUtils,
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
		UnlimitedFileSize: Boolean;
		SplitLargeFiles: Boolean;
		CloudMaxFileSize: Int64;
		PublicAccount: Boolean;
		Description: WideString;
		EncryptFiles: Boolean;
		CryptPasswordStorage: Integer;
		CryptFilesPassword: WideString; {Only populated when storage = CryptPasswordStorageIniFile}
		CipherProfileId: WideString; //Cipher profile identifier for encryption backend selection
		Server: WideString; //Server profile name, empty = cloud.mail.ru default
		CryptedGUIDFiles: WideString; //The hash of files encryption password to check its validity
		AuthMethod: Integer; //Authentication method: 0=classic web, 4=OAuth app password
		UseAppPassword: Boolean; //True if password is an app password (for OAuth)
	private
		FUser: WideString;
		FDomain: WideString;
		FPublicUrl: WideString;
		procedure ParseEmailParts;
		function GetAccountType: EAccountType;
		function GetDomain: WideString;
		function GetUser: WideString;
		function GetPublicUrl: WideString;
	public
		property User: WideString read GetUser;
		property Domain: WideString read GetDomain;
		property PublicUrl: WideString read GetPublicUrl write FPublicUrl;
		property AccountType: EAccountType read GetAccountType;
	end;

implementation

{TAccountSettings}

procedure TAccountSettings.ParseEmailParts;
var
	AtPos: Integer;
begin
	AtPos := Pos('@', Email);
	if (AtPos > 0) and (AtPos < Length(Email)) then
	begin
		{Standard email format: split into user and domain parts}
		FUser := Copy(Email, 0, AtPos - 1);
		FDomain := Copy(Email, AtPos + 1, Length(Email) - AtPos);
	end else begin
		{No @ found - treat entire string as username (self-hosted servers allow arbitrary logins)}
		FUser := Email;
		FDomain := EmptyWideStr;
	end;
end;

function TAccountSettings.GetAccountType: EAccountType;
begin
	if self.PublicAccount then
		exit([ATPublic]);
	exit([ATPrivate]);
end;

function TAccountSettings.GetDomain: WideString;
begin
	if FDomain = EmptyWideStr then
		ParseEmailParts;
	result := FDomain;
end;

function TAccountSettings.GetUser: WideString;
begin
	if FUser = EmptyWideStr then
		ParseEmailParts;
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

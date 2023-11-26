unit Settings;

interface

uses
	IniFiles,
	Classes,
	SysUtils,
	SETTINGS_CONSTANTS,
	AccountSettings;

procedure GetAccountsListFromIniFile(IniFilePath: WideString; var AccountsList: TStringList);
procedure DeleteAccountFromIniFile(IniFilePath: WideString; AccountName: WideString);
procedure AddVirtualAccountsToAccountsList(AccountsIniFilePath: WideString; var AccountsList: TStringList; VirtualAccountsEnabled: TArray<boolean>);

procedure GetStreamingExtensionsFromIniFile(IniFilePath: WideString; var StreamingExtensions: TStringList);
procedure DeleteStreamingExtensionsFromIniFile(IniFilePath: WideString; StreamingExtension: WideString);

implementation

procedure GetAccountsListFromIniFile(IniFilePath: WideString; var AccountsList: TStringList);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.ReadSections(AccountsList);
	IniFile.Destroy;
end;

procedure DeleteAccountFromIniFile(IniFilePath: WideString; AccountName: WideString);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.EraseSection(AccountName);
	IniFile.Destroy;
end;

procedure AddVirtualAccountsToAccountsList(AccountsIniFilePath: WideString; var AccountsList: TStringList; VirtualAccountsEnabled: TArray<boolean>);
var
	VAccounts: TStringList;
	Account: WideString;
begin
	VAccounts := TStringList.Create;
	for Account in AccountsList do
	begin
		if GetAccountSettingsFromIniFile(AccountsIniFilePath, Account).public_account then
			Continue; //public accounts ignored
		if VirtualAccountsEnabled[0] then
			VAccounts.Add(Account + TrashPostfix);
		if VirtualAccountsEnabled[1] then
			VAccounts.Add(Account + SharedPostfix);
		if VirtualAccountsEnabled[2] then
			VAccounts.Add(Account + InvitesPostfix);
	end;
	AccountsList.AddStrings(VAccounts);
	VAccounts.Free;
end;

//loads all streaming extensions list
procedure GetStreamingExtensionsFromIniFile(IniFilePath: WideString; var StreamingExtensions: TStringList);
var
	IniFile: TIniFile;
	TempList: TStrings;
	line: String;
begin
	IniFile := TIniFile.Create(IniFilePath);
	TempList := TStringList.Create;
	IniFile.ReadSections(TempList);
	for line in TempList do
	begin
		if line.StartsWith(StreamingPrefix) then
			StreamingExtensions.Add(line.Substring(Length(StreamingPrefix)));
	end;
	TempList.Destroy;
	IniFile.Destroy;
end;

procedure DeleteStreamingExtensionsFromIniFile(IniFilePath: WideString; StreamingExtension: WideString);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.EraseSection(StreamingPrefix + StreamingExtension);
	IniFile.Destroy;
end;

end.

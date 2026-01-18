unit IAccountsManagerInterface;

{Interface for account settings management, decoupled from INI file implementation}

interface

uses
	AccountSettings;

type
	IAccountsManager = interface
		['{7B8C9D0E-1F2A-3B4C-5D6E-7F8A9B0C1D2E}']
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SwitchPasswordStorage(Account: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);
	end;

	{Null implementation for testing - returns defaults, no-op for writes}
	TNullAccountsManager = class(TInterfacedObject, IAccountsManager)
	public
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SwitchPasswordStorage(Account: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);
	end;

implementation

{TNullAccountsManager}

function TNullAccountsManager.GetAccountSettings(Account: WideString): TAccountSettings;
begin
	Result := Default(TAccountSettings);
	Result.Account := Account;
end;

procedure TNullAccountsManager.SwitchPasswordStorage(Account: WideString);
begin
	{No-op for null implementation}
end;

procedure TNullAccountsManager.SetCryptedGUID(Account: WideString; GUID: WideString);
begin
	{No-op for null implementation}
end;

end.

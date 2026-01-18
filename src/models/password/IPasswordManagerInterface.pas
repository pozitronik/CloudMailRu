unit IPasswordManagerInterface;

{Interface for password management operations, decoupled from Total Commander implementation}

interface

uses
	PLUGIN_TYPES;

const
	{Password manager key constants}
	PASSWORD_SUFFIX_FILECRYPT = ' filecrypt'; {Suffix for file encryption password keys}
	PASSWORD_KEY_PROXY = 'proxy'; {Key prefix for proxy passwords}

type
	IPasswordManager = interface
		['{6FCDC8B2-5015-446E-9DED-1F9EBEEBB771}']
		function GetPassword(Key: WideString; var Password: WideString): Integer;
		function SetPassword(Key, Password: WideString): Integer;
	end;

	{Null implementation for testing - no password storage}
	TNullPasswordManager = class(TInterfacedObject, IPasswordManager)
	public
		function GetPassword(Key: WideString; var Password: WideString): Integer;
		function SetPassword(Key, Password: WideString): Integer;
	end;

implementation

{TNullPasswordManager}

function TNullPasswordManager.GetPassword(Key: WideString; var Password: WideString): Integer;
begin
	{No password stored - return not found}
	Result := FS_FILE_NOTFOUND;
end;

function TNullPasswordManager.SetPassword(Key, Password: WideString): Integer;
begin
	{Pretend to save successfully}
	Result := FS_FILE_OK;
end;

end.

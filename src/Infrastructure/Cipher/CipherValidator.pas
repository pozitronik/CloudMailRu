unit CipherValidator;

{Abstraction for password validation via encrypted GUID comparison,
	enabling testability without actual cryptographic operations.}

interface

type

	{Interface for validating encryption passwords}
	ICipherValidator = interface
		['{6E672079-034D-42E7-8CB8-364D6D0F2657}']
		{Generates an encrypted GUID from a password for later validation}
		function GetCryptedGUID(const Password: WideString): WideString;

		{Validates a password by comparing its encrypted GUID with a stored control value}
		function CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean;
	end;

	{Null implementation that always validates passwords.
		Useful for testing scenarios where encryption is not relevant.}
	TNullCipherValidator = class(TInterfacedObject, ICipherValidator)
	public
		function GetCryptedGUID(const Password: WideString): WideString;
		function CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean;
	end;

	{Production implementation of ICipherValidator that delegates to TFileCipher.}
	TCipherValidator = class(TInterfacedObject, ICipherValidator)
	public
		function GetCryptedGUID(const Password: WideString): WideString;
		function CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean;
	end;

implementation

uses
	FileCipher;

{TNullCipherValidator}

function TNullCipherValidator.GetCryptedGUID(const Password: WideString): WideString;
begin
	Result := '';
end;

function TNullCipherValidator.CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean;
begin
	Result := True;
end;

{TCipherValidator}

function TCipherValidator.GetCryptedGUID(const Password: WideString): WideString;
begin
	Result := TFileCipher.GetCryptedGUID(Password);
end;

function TCipherValidator.CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean;
begin
	Result := TFileCipher.CheckPasswordGUID(Password, ControlGUID);
end;

end.

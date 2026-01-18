unit CipherValidator;

{Production implementation of ICipherValidator that delegates to TFileCipher.}

interface

uses
	ICipherValidatorInterface;

type

	TCipherValidator = class(TInterfacedObject, ICipherValidator)
	public
		function GetCryptedGUID(const Password: WideString): WideString;
		function CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean;
	end;

implementation

uses
	FileCipher;

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

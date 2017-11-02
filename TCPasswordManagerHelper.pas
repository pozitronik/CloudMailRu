unit TCPasswordManagerHelper;

{Обертка над обращениями к менеджеру паролей Total Commander}
interface

Uses Plugin_Types;

type
	TTCPasswordManager = class
	private
		CryptHandleProc: TCryptHandler;
		LogHandleProc: TLogHandler;

	public
		constructor Create(CryptHandleProc: TCryptHandler; LogHandleProc: TLogHandler = nil);
		destructor Destroy(); override;
		function GetPassword(Key: WideString; var Password: WideString): integer;
		function SetPassword(Key, Password: WideString): integer;
	end;

implementation

{TTCPasswordManager}

constructor TTCPasswordManager.Create(CryptHandleProc: TCryptHandler; LogHandleProc: TLogHandler);
begin

end;

destructor TTCPasswordManager.Destroy;
begin

	inherited;
end;

function TTCPasswordManager.GetPassword(Key: WideString; var Password: WideString): integer;
begin

end;

function TTCPasswordManager.SetPassword(Key, Password: WideString): integer;
begin

end;

end.

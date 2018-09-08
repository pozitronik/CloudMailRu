unit HashInfo;

interface

uses system.sysutils;

const
	ERR_WRONG_FORMAT = 'Parameter should be in hash:size:name or hash:size format.';
	ERR_WRONG_HASH_LENGTH = 'Hash length shoud be exactly 40 symbols.';
	ERR_WRONG_SIZE_FORMAT = 'Size should be in numeric format.';

type

	THashInfo = class
	public
		hash: WideString;
		size: int64;
		name: WideString;
		valid: boolean;
		errorString: WideString;
		constructor Create(parameter: WideString);
		destructor Destroy; override;

	end;

implementation

{THashInfo}
(*Парсер параметров команды hash. Корректный формат параметра:
 hash:size:name
 hash:size
*)
constructor THashInfo.Create(parameter: WideString);
const
	divisor = ':';
var
	divisor_position: integer;
	sizeString: WideString;
begin
	self.valid := false;
	divisor_position := Pos(WideString(divisor), parameter);
	if divisor_position = 0 then
	begin
		self.errorString := ERR_WRONG_FORMAT;
		exit;
	end;
	self.hash := Copy(parameter, 0, divisor_position - 1);
	if Length(hash) <> 40 then
	begin
		self.errorString := ERR_WRONG_HASH_LENGTH;
		exit;
	end;

	parameter := Copy(parameter, divisor_position + 1);
	divisor_position := Pos(WideString(divisor), parameter);
	if divisor_position = 0 then
		sizeString := parameter
	else
	begin
		sizeString := Copy(parameter, 0, divisor_position - 1);
		parameter := Copy(parameter, divisor_position + 1);
	end;

	try
		self.size := StrToInt64(sizeString);
	Except
		on E: EConvertError do
		begin
			self.errorString := ERR_WRONG_SIZE_FORMAT;
			exit;
		end;
	end;
	self.valid := true;
	if divisor_position = 0 then
		self.name := self.hash
	else
		self.name := parameter;

end;

destructor THashInfo.Destroy;
begin
	inherited;
end;

end.

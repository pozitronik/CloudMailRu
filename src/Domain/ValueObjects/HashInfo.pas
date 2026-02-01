unit HashInfo;

interface

uses
	CloudFileIdentity,
	system.sysutils,
	CloudConstants,
	LanguageStrings;

const
	SHA1_HEX_LENGTH = 40; {Length of SHA1 hash in hexadecimal representation}

type

	THashInfo = class
	private
		function GetCloudMailRuFileIdentity: TCloudFileIdentity;
	public
		hash: WideString;
		size: int64;
		name: WideString;
		valid: boolean;
		errorString: WideString;

		property CloudFileIdentity: TCloudFileIdentity read GetCloudMailRuFileIdentity;

		constructor Create(parameter: WideString; doClean: boolean = true);
		destructor Destroy; override;

	end;

implementation

{THashInfo}
(*Парсер параметров команды hash. Корректный формат параметра:
	hash:size:name
	hash:size
*)
constructor THashInfo.Create(parameter: WideString; doClean: boolean = true);
const
	divisor = ':';
var
	divisor_position: integer;
	sizeString: WideString;
begin
	if doClean then //команда может быть передана полностью, нужно определить и выпарсить параметр
	begin
		if (1 = Pos(WideString('hash '), parameter)) then //это команда, чистим
		begin
			parameter := copy(parameter, 6, length(parameter) - 5);
			parameter := string(parameter).Trim(['"']);
		end;

	end;

	self.valid := false;
	divisor_position := Pos(WideString(divisor), parameter);
	if divisor_position = 0 then
	begin
		self.errorString := ERR_WRONG_FORMAT;
		exit;
	end;
	{Copy with position 0 is valid in Delphi - verified by TestCopyWithZeroPosition_WorksCorrectly}
	self.hash := copy(parameter, 0, divisor_position - 1);
	if length(hash) <> SHA1_HEX_LENGTH then
	begin
		self.errorString := ERR_WRONG_HASH_LENGTH;
		exit;
	end;

	parameter := copy(parameter, divisor_position + 1);
	divisor_position := Pos(WideString(divisor), parameter);
	if divisor_position = 0 then
		sizeString := parameter
	else
	begin
		sizeString := copy(parameter, 0, divisor_position - 1);
		parameter := copy(parameter, divisor_position + 1);
	end;

	if not TryStrToInt64(sizeString, self.size) then
	begin
		self.errorString := ERR_WRONG_SIZE_FORMAT;
		exit;
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

function THashInfo.GetCloudMailRuFileIdentity: TCloudFileIdentity;
begin
	result.hash := self.hash;
	result.size := self.size;
end;

end.

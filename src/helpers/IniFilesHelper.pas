unit IniFilesHelper;

interface

uses
	IniFiles,
	System.RegularExpressions,
	Variants,
	SysUtils,
	Windows,
	IOUtils,
	LANGUAGE_STRINGS;

type
	TIniFilesHelper = class helper for TIniFile
		function ReadInt64(const Section, Ident: string; Default: int64): int64; overload;
		procedure WriteInt64(const Section, Ident: string; Value: int64);
		procedure WriteString(const Section, Ident, Value: String); //owerride default Write%Anything% metod
		function ValidateSectionName(const Section: string): boolean;
		function ValidateIdentName(const Ident: string): boolean;

		procedure WriteIntegerIfNotDefault(const Section, Ident: string; Value, Default: Integer); overload;
		procedure WriteInt64IfNotDefault(const Section, Ident: string; Value, Default: int64); overload;
		procedure WriteStringIfNotDefault(const Section, Ident: string; Value, Default: String);
		procedure WriteBoolIfNotDefault(const Section, Ident: string; Value, Default: boolean);

		procedure SetSingleOptionValue(const Section, Ident: WideString; OptionValue: Variant);
	end;

implementation

{TIniFilesHelper}

function TIniFilesHelper.ReadInt64(const Section, Ident: string; Default: int64): int64;
var
	IntStr: string;
begin
	IntStr := ReadString(Section, Ident, '');
	if (IntStr.Length > 2) and (IntStr.StartsWith('0x', true)) then
		IntStr := '$' + IntStr.Substring(2);
	result := StrToInt64Def(IntStr, Default);
end;

function TIniFilesHelper.ValidateIdentName(const Ident: string): boolean;
var
	RegEx: TRegEx;
begin
	RegEx := TRegEx.Create('^([a-z]|[A-Z]|\.|\$|\:)([a-z]|[A-Z]|[0-9]|_|~|-|\.|:|\$|\s)+');
	result := RegEx.Match(Ident).Success;
end;

function TIniFilesHelper.ValidateSectionName(const Section: string): boolean;
var
	RegEx: TRegEx;
begin

	RegEx := TRegEx.Create('\[|\]|\n');
	result := not RegEx.Match(Section).Success;
end;

procedure TIniFilesHelper.WriteInt64(const Section, Ident: string; Value: int64);
begin
	WriteString(Section, Ident, IntToStr(Value));
end;

procedure TIniFilesHelper.WriteInt64IfNotDefault(const Section, Ident: string; Value, Default: int64);
begin
	if Value <> Default then
	begin
		self.WriteInt64(Section, Ident, Value);
	end else begin
		self.DeleteKey(Section, Ident)
	end;
end;

procedure TIniFilesHelper.WriteIntegerIfNotDefault(const Section, Ident: string; Value, Default: Integer);
begin
	if Value <> Default then
	begin
		self.WriteInteger(Section, Ident, Value);
	end else begin
		self.DeleteKey(Section, Ident)
	end;
end;

procedure TIniFilesHelper.WriteStringIfNotDefault(const Section, Ident: string; Value, Default: String);
begin
	if Value <> Default then
	begin
		self.WriteString(Section, Ident, Value);
	end else begin
		self.DeleteKey(Section, Ident)
	end;
end;

procedure TIniFilesHelper.WriteString(const Section, Ident, Value: String);
begin
	if not(self.ValidateSectionName(Section)) then
		raise EIniFileException.CreateFmt(ERR_INVALID_SECTION_NAME, [Section]);
	if not(self.ValidateIdentName(Ident)) then
		raise EIniFileException.CreateFmt(ERR_INVALID_IDENTIFIER_NAME, [Ident]);
	inherited WriteString(Section, Ident, Value);
end;

procedure TIniFilesHelper.SetSingleOptionValue(const Section, Ident: WideString; OptionValue: Variant);
var
	basicType: Integer;
begin
	basicType := VarType(OptionValue);
	case basicType of
		varNull:
			self.DeleteKey(Section, Ident); //remove value in that case
		varInteger:
			self.WriteInteger(Section, Ident, OptionValue);
		varString, varUString, varOleStr:
			self.WriteString(Section, Ident, OptionValue);
		varBoolean:
			self.WriteBool(Section, Ident, OptionValue);
	end;

end;

procedure TIniFilesHelper.WriteBoolIfNotDefault(const Section, Ident: string; Value, Default: boolean);
begin
	if Value <> Default then
	begin
		self.WriteBool(Section, Ident, Value);
	end else begin
		self.DeleteKey(Section, Ident)
	end;

end;

end.

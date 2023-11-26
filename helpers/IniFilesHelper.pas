unit IniFilesHelper;

interface

uses
	IniFiles,
	System.RegularExpressions,
	SysUtils,
	CMRStrings;

type
	TIniFilesHelper = class helper for TIniFile
		function ReadInt64(const Section, Ident: string; Default: int64): int64;
		procedure WriteInt64(const Section, Ident: string; Value: int64);
		procedure WriteString(const Section, Ident, Value: String); //owerride default Write%Anything% metod
		function ValidateSectionName(const Section: string): boolean;
		function ValidateIdentName(const Ident: string): boolean;
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

procedure TIniFilesHelper.WriteString(const Section, Ident, Value: String);
begin
	if not(self.ValidateSectionName(Section)) then
		raise EIniFileException.CreateFmt(ERR_INVALID_SECTION_NAME, [Section]);
	if not(self.ValidateIdentName(Ident)) then
		raise EIniFileException.CreateFmt(ERR_INVALID_IDENTIFIER_NAME, [Ident]);
	inherited WriteString(Section, Ident, Value);
end;

end.

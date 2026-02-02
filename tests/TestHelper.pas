unit TestHelper;

interface

uses
	IOUtils,
	SysUtils,
	CloudCallbackTypes;

function DataPath(Path: WideString): WideString;
procedure DataFileContents(DataFileName: WideString; out VarName: WideString);
function RandomString(const Len: Integer): WideString;
{Returns a fixed thread ID callback for tests - avoids Windows API dependency}
function TestThreadID: TGetThreadIDFunc;

implementation

function DataPath(Path: WideString): WideString;
begin
	Exit(TPath.GetFullPath(TPath.Combine(TPath.Combine(TPath.GetLibraryPath, '..\..\data'), Path))); //up to two levels, due binary is in subdir
end;

procedure DataFileContents(DataFileName: WideString; out VarName: WideString);
begin
	if TFile.Exists(DataPath(DataFileName)) then
		VarName := TFile.ReadAllText(DataPath(DataFileName))
	else
		raise Exception.CreateFmt('File %s not found!', [DataFileName]);
end;

function RandomString(const Len: Integer): WideString;
const
	CharSet = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
var
	I: Integer;
begin
	Result := '';
	Randomize; // Initialize the random number generator

	for I := 1 to Len do
		Result := Result + CharSet[Random(Length(CharSet)) + 1];
end;

function TestThreadID: TGetThreadIDFunc;
begin
	Result := function: TThreadID begin Result := 1; end;
end;

end.

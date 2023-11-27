unit TestHelper;

interface

uses
	IOUtils,
	SysUtils;

function DataPath(Path: WideString): WideString;
procedure DataFileContents(DataFileName: WideString; out VarName: WideString);
function RandomString(const Len: Integer): WideString;

implementation

function DataPath(Path: WideString): WideString;
begin
	exit(TPath.Combine(TPath.GetLibraryPath, '..\..\data', Path)); //up to two levels, due binary is in subdir
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
	i: Integer;
begin
	Result := '';
	Randomize; // Initialize the random number generator

	for i := 1 to Len do
		Result := Result + CharSet[Random(Length(CharSet)) + 1];
end;

end.

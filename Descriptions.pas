unit Descriptions;

{Simple && read-only descript.ion files support}
interface

uses
	System.Types, System.Classes, System.StrUtils, Generics.Collections, System.SysUtils, System.WideStrUtils;

const
	FORMAT_AS_IS = 0;
	FORMAT_CLEAR = 1;
	FORMAT_ONELINE = 2;

	MULTILINE_DIVIDER = #$04#$C2; //multiline divider in ansii files
	MULTILINE_DIVIDERW = chr($04) + chr($C2); //multiline divider in utf-8 and utf-16 formatted files (BE/LE)

type

	TDescription = class

	private
	var
		items: TDictionary<WideString, WideString>;
		ion_filename: WideString;
		encoding: TEncoding;

		function GetionFilename: WideString;
		function FormatValue(Value: WideString; FormatType: Integer): WideString;

	public
		constructor Create(ion_filename: WideString);
		destructor Destroy; override;
		function Read(): Integer;
		function GetValue(item: WideString; FormatType: Integer = FORMAT_ONELINE): WideString;
		procedure Clear;
		function DetermineEncoding(): TEncoding;
		property ionFilename: WideString read GetionFilename;
	end;

implementation

{TDescription}

procedure TDescription.Clear;
begin
	self.items.Clear;
end;

constructor TDescription.Create(ion_filename: WideString);
begin
	self.items := TDictionary<WideString, WideString>.Create;
	self.ion_filename := ion_filename;
end;

destructor TDescription.Destroy;
begin
	self.items.Free;
	inherited;
end;

function TDescription.DetermineEncoding(): TEncoding;
var
	F: File;
	Buffer: array [0 .. 2] of byte;
begin
	AssignFile(F, ion_filename);
	Reset(F, 1);
	BlockRead(F, Buffer, SizeOf(Buffer));
	CloseFile(F);
	if (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then exit(TEncoding.UTF8);
	if (Buffer[0] = $FE) and (Buffer[1] = $FF) then exit(TEncoding.BigEndianUnicode);
	if (Buffer[0] = $FF) and (Buffer[1] = $FE) then exit(TEncoding.Unicode);
	exit(TEncoding.Default);
end;

function TDescription.FormatValue(Value: WideString; FormatType: Integer): WideString;
var
	Divider: WideString;
begin
	if (self.encoding = TEncoding.UTF8) or (self.encoding = TEncoding.BigEndianUnicode) or (self.encoding = TEncoding.Unicode) then Divider := MULTILINE_DIVIDERW
	else Divider := MULTILINE_DIVIDER;

	case FormatType of
		FORMAT_AS_IS: Result := Value;
		FORMAT_CLEAR: Result := WideStringReplace(WideStringReplace(Value, '\n', sLineBreak, [rfReplaceAll]), Divider, '', [rfReplaceAll]);
		FORMAT_ONELINE: Result := WideStringReplace(WideStringReplace(Value, '\n', '  ', [rfReplaceAll]), Divider, '', [rfReplaceAll]);
	end;

end;

function TDescription.GetionFilename: WideString;
begin
	Result := self.ion_filename;
end;

function TDescription.GetValue(item: WideString; FormatType: Integer): WideString;
begin
	if not(items.TryGetValue(item, Result)) then exit('');
	Result := self.FormatValue(Result, FormatType);

end;

function TDescription.Read(): Integer;
var
	fStream: TStreamReader;
	line, key, Value: WideString;
	t: Integer;
begin
	Result := 0; //not used
	self.Clear;
	fStream := nil;
	try
		self.encoding := DetermineEncoding();
		fStream := TStreamReader.Create(self.ion_filename, self.encoding, False);
		while not fStream.EndOfStream do
		begin
			line := fStream.ReadLine;
			if StartsStr('"', line) then
			begin
				t := PosEx('" ', line);
				Value := copy(line, t + 2, length(line));
				key := copy(line, 2, t - 2);
			end else begin
				t := PosEx(' ', line);
				Value := copy(line, t + 1, length(line));
				key := copy(line, 0, t - 1);
			end;

			items.Add(key, Value);
		end;
	except
		fStream.Free;
		exit(-1);
	end;
	fStream.Free;
end;

end.

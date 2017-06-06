unit Descriptions;

interface

uses
	System.Types, System.Classes, System.StrUtils, Generics.Collections, System.SysUtils, System.WideStrUtils;

const
	FORMAT_AS_IS = 0; //raw comment string, include all control chars
	FORMAT_CLEAR = 1; //clear comment string without control chars
	FORMAT_ONELINE = 2; //like FORMAT_CLEAR, but line breaks replaced to double space

	MULTILINE_DIVIDER = #$04#$C2; //multiline divider in ansii files
	MULTILINE_DIVIDERW = chr($04) + chr($C2); //multiline divider in utf-8 and utf-16 formatted files (BE/LE)

	ENCODING_DEFAULT = 0;
	ENCODING_UTF8 = 1;
	ENCODING_UNICODE = 2;
	ENCODING_UNCODE_BE = 3;

type

	TDescription = class

	private
	var
		items: TDictionary<WideString, WideString>;
		ion_filename: WideString;
		encoding: TEncoding;
		divider: WideString;

		function GetionFilename: WideString;
		function FormatValue(Value: WideString; FormatType: Integer): WideString;
		function DetermineDivider(): WideString;

	public
		constructor Create(ion_filename: WideString; encoding: Integer = ENCODING_UTF8);
		destructor Destroy; override;
		function Read(): Integer;
		function Write(filename: WideString = NullChar): Integer;
		function GetValue(item: WideString; FormatType: Integer = FORMAT_ONELINE): WideString;
		function SetValue(item: WideString; Value: WideString): Boolean;
		procedure Clear;
		function DetermineEncoding(): TEncoding;
		property ionFilename: WideString read GetionFilename;
		class function CopyValue(from_filename, to_filename, item: WideString): Integer;
	end;

implementation

{TDescription}

procedure TDescription.Clear;
begin
	self.items.Clear;
end;

class function TDescription.CopyValue(from_filename, to_filename, item: WideString): Integer;
var
	ion_from, ion_to: TDescription;
begin
	try
		ion_from := self.Create(from_filename);
		ion_to := self.Create(to_filename);
		ion_from.Read();
		ion_to.Read();
		ion_to.SetValue(item, ion_from.GetValue(item, FORMAT_CLEAR));
		ion_to.Write();
		ion_to.Destroy;
		ion_from.Destroy;
	except
		exit(-1);
	end;
	exit(0);
end;

constructor TDescription.Create(ion_filename: WideString; encoding: Integer = ENCODING_UTF8);
begin
	self.items := TDictionary<WideString, WideString>.Create;
	self.ion_filename := ion_filename;
	case encoding of
		ENCODING_DEFAULT: self.encoding := TEncoding.Default;
		ENCODING_UTF8: self.encoding := TEncoding.UTF8;
		ENCODING_UNICODE: self.encoding := TEncoding.Unicode;
		ENCODING_UNCODE_BE: self.encoding := TEncoding.BigEndianUnicode;
	end;
end;

destructor TDescription.Destroy;
begin
	self.items.Free;
	inherited;
end;

function TDescription.DetermineDivider: WideString;
begin
	if (self.encoding = TEncoding.UTF8) or (self.encoding = TEncoding.BigEndianUnicode) or (self.encoding = TEncoding.Unicode) then result := MULTILINE_DIVIDERW
	else result := MULTILINE_DIVIDER;
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
begin
	case FormatType of
		FORMAT_AS_IS: result := Value;
		FORMAT_CLEAR: result := WideStringReplace(WideStringReplace(Value, '\n', sLineBreak, [rfReplaceAll]), self.divider, '', [rfReplaceAll]);
		FORMAT_ONELINE: result := WideStringReplace(WideStringReplace(Value, '\n', '  ', [rfReplaceAll]), self.divider, '', [rfReplaceAll]);
	end;
end;

function TDescription.GetionFilename: WideString;
begin
	result := self.ion_filename;
end;

function TDescription.GetValue(item: WideString; FormatType: Integer): WideString;
begin
	if not(items.TryGetValue(item, result)) then exit('');
	result := self.FormatValue(result, FormatType);
end;

function TDescription.SetValue(item, Value: WideString): Boolean;
begin
	result := true;
	try
		items.AddOrSetValue(item, Value);
	except
		result := false;
	end;
end;

function TDescription.Write(filename: WideString = NullChar): Integer;
var
	fStream: TStreamWriter;
	line, Key, tKey, Value: WideString;
begin
	if filename = NullChar then filename := self.ion_filename;

	result := 0; //not used
	fStream := nil;

	try
		fStream := TStreamWriter.Create(filename, false, self.encoding);
		for Key in items.Keys do
		begin
			items.TryGetValue(Key, Value);
			if Pos(Space, Key) <> 0 then tKey := '"' + Key + '"'
			else tKey := Key;

			if Pos(sLineBreak, Value) <> 0 then Value := WideStringReplace(Value, sLineBreak, '\n', [rfReplaceAll]) + divider;
			if Length(Value) > 0 then
			begin
				line := Key + Space + Value;
				fStream.Write(line);
			end;
		end;
		fStream.Flush;

	except
		fStream.Destroy;
		exit(-1);
	end;
	fStream.Destroy;
end;

function TDescription.Read(): Integer;
var
	fStream: TStreamReader;
	line, Key, Value: WideString;
	t: Integer;
begin
	result := 0; //not used
	self.Clear;
	fStream := nil;
	try
		self.encoding := DetermineEncoding();
		self.divider := DetermineDivider();
		fStream := TStreamReader.Create(self.ion_filename, self.encoding, false);
		while not fStream.EndOfStream do
		begin
			line := fStream.ReadLine;
			if StartsStr('"', line) then
			begin
				t := PosEx('" ', line);
				Value := copy(line, t + 2, Length(line));
				Key := copy(line, 2, t - 2);
			end else begin
				t := PosEx(Space, line);
				Value := copy(line, t + 1, Length(line));
				Key := copy(line, 0, t - 1);
			end;

			items.Add(Key, Value);
		end;
	except
		fStream.Destroy;
		exit(-1);
	end;
	fStream.Destroy;
end;

end.

unit Description;

interface

uses
	System.Types,
	System.Classes,
	System.StrUtils,
	Generics.Collections,
	System.SysUtils,
	System.WideStrUtils,
	WindowsFileSystem,
	Windows;

const
	FORMAT_AS_IS = 0; //raw comment string, include all control chars
	FORMAT_CLEAR = 1; //clear comment string without control chars
	FORMAT_ONELINE = 2; //like FORMAT_CLEAR, but line breaks replaced to double space

	MULTILINE_DIVIDER = #$04#$C2; //multiline divider in ansii files
	MULTILINE_DIVIDERW = chr($04) + chr($C2); //multiline divider in utf-8 and utf-16 formatted files (BE/LE)

	ENCODING_DEFAULT = 0; //ANSI
	ENCODING_UNICODE = 1;
	ENCODING_UNCODE_BE = 2;
	ENCODING_UTF8 = 3;

	DESCRIPTION_TEMP_EXT = 'ion'; {Extension for temporary description files}

type

	TDescription = class {todo: class rename and interface extraction}

	private
	var
		items: TDictionary<WideString, WideString>;
		ion_filename: WideString;
		encoding: TEncoding;
		divider: WideString;
		FFileSystem: IFileSystem;

		function GetIonFilename: WideString;
		function FormatValue(Value: WideString; FormatType: Integer): WideString;
		function DetermineDivider(): WideString;

	public
		constructor Create(ion_filename: WideString; FileSystem: IFileSystem; encoding: Integer = ENCODING_UTF8);

		destructor Destroy; override;
		function Read(): Integer;
		function Write(filename: WideString = NullChar): Integer;
		function GetValue(item: WideString; FormatType: Integer = FORMAT_ONELINE): WideString;
		function SetValue(item: WideString; Value: WideString): boolean;
		function DeleteValue(item: WideString): boolean;
		function RenameItem(OldItem, NewItem: WideString): boolean;

		procedure Clear;
		function DetermineEncoding(): TEncoding;
		property IonFilename: WideString read GetIonFilename;

		function CopyFrom(from_description: TDescription; item: WideString; move: boolean = false): Integer;
	end;

implementation

{TDescription}

function TDescription.CopyFrom(from_description: TDescription; item: WideString; move: boolean): Integer;
begin
	if not assigned(from_description) then
		exit(1);
	self.SetValue(item, from_description.GetValue(item, FORMAT_AS_IS));
	if move then
	begin
		from_description.DeleteValue(item);
		from_description.Write();
	end;
	exit(0);
end;

procedure TDescription.Clear;
begin
	self.items.Clear;
end;

constructor TDescription.Create(ion_filename: WideString; FileSystem: IFileSystem; encoding: Integer = ENCODING_UTF8);
begin
	self.items := TDictionary<WideString, WideString>.Create;
	self.ion_filename := ion_filename;
	self.FFileSystem := FileSystem;

	if not FFileSystem.FileExists(ion_filename) then
		FFileSystem.CreateEmptyFile(ion_filename);

	case encoding of
		ENCODING_DEFAULT:
			self.encoding := TEncoding.Default;
		ENCODING_UTF8:
			self.encoding := TEncoding.UTF8;
		ENCODING_UNICODE:
			self.encoding := TEncoding.Unicode;
		ENCODING_UNCODE_BE:
			self.encoding := TEncoding.BigEndianUnicode;
	end;
end;

function TDescription.DeleteValue(item: WideString): boolean;
begin
	self.items.Remove(item);
	exit(true);
end;

destructor TDescription.Destroy;
begin
	self.items.Free;
	FFileSystem := nil;
	inherited;
end;

function TDescription.RenameItem(OldItem, NewItem: WideString): boolean;
var
	Value: WideString;
begin
	Value := self.GetValue(OldItem);
	if EmptyWideStr = Value then
		exit(false);
	self.SetValue(NewItem, Value);
	self.DeleteValue(OldItem);
	exit(true);
end;

function TDescription.DetermineDivider: WideString;
begin
	if (self.encoding = TEncoding.UTF8) or (self.encoding = TEncoding.BigEndianUnicode) or (self.encoding = TEncoding.Unicode) then
		result := MULTILINE_DIVIDERW
	else
		result := MULTILINE_DIVIDER;
end;

function TDescription.DetermineEncoding(): TEncoding;
var
	Buffer: TBytes;
	Content: WideString;
begin
	result := self.encoding;

	{Read first 3 bytes to detect BOM}
	Buffer := FFileSystem.ReadFileHeader(ion_filename, 3);
	if Length(Buffer) < 2 then
		exit; {File too small or unreadable, return default encoding}

	{Check for BOM signatures}
	if (Length(Buffer) >= 3) and (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
		exit(TEncoding.UTF8);
	if (Buffer[0] = $FE) and (Buffer[1] = $FF) then
		exit(TEncoding.BigEndianUnicode);
	if (Buffer[0] = $FF) and (Buffer[1] = $FE) then
		exit(TEncoding.Unicode);

	{Encoding still not determined - try reading with chosen encoding.
		If content is empty or unreadable, use ANSI.}
	try
		Content := FFileSystem.ReadAllText(ion_filename, self.encoding);
		if Content <> EmptyWideStr then
			result := self.encoding
		else
			result := TEncoding.Default;
	except
		result := TEncoding.Default;
	end;
end;

function TDescription.FormatValue(Value: WideString; FormatType: Integer): WideString;
begin
	case FormatType of
		FORMAT_AS_IS:
			result := Value;
		FORMAT_CLEAR:
			result := WideStringReplace(WideStringReplace(Value, '\n', sLineBreak, [rfReplaceAll]), self.divider, '', [rfReplaceAll]);
		FORMAT_ONELINE:
			result := WideStringReplace(WideStringReplace(Value, '\n', '  ', [rfReplaceAll]), self.divider, '', [rfReplaceAll]);
	end;
end;

function TDescription.GetIonFilename: WideString;
begin
	result := self.ion_filename;
end;

function TDescription.GetValue(item: WideString; FormatType: Integer): WideString;
begin
	if not(items.TryGetValue(item, result)) then
		exit(EmptyWideStr);
	result := self.FormatValue(result, FormatType);
end;

function TDescription.SetValue(item, Value: WideString): boolean;
begin
	result := true;
	try
		if EmptyWideStr = Value then
			DeleteValue(item)
		else
			items.AddOrSetValue(item, Value);
	except
		result := false;
	end;
end;

function TDescription.Write(filename: WideString = NullChar): Integer;
var
	Lines: TStringList;
	line, Key, tKey, Value: WideString;
begin
	if filename = NullChar then
		filename := self.ion_filename;

	result := 0;
	FFileSystem.DeleteFile(filename);
	if items.Count = 0 then
		exit;

	Lines := TStringList.Create;
	try
		try
			for Key in items.Keys do
			begin
				items.TryGetValue(Key, Value);
				if Pos(Space, Key) <> 0 then
					tKey := '"' + Key + '"'
				else
					tKey := Key;

				if Pos(sLineBreak, Value) <> 0 then
					Value := WideStringReplace(Value, sLineBreak, '\n', [rfReplaceAll]) + divider;
				if Length(Value) > 0 then
				begin
					line := tKey + Space + Value;
					Lines.Add(line);
				end;
			end;

			FFileSystem.WriteAllLines(filename, Lines, self.encoding);
		except
			result := -1;
		end;
	finally
		Lines.Free;
	end;
end;

function TDescription.Read(): Integer;
var
	Reader: TStreamReader;
	line, Key, Value: WideString;
	t: Integer;
begin
	result := 0;
	self.Clear;
	self.encoding := DetermineEncoding();
	self.divider := DetermineDivider();

	Reader := FFileSystem.OpenTextReader(self.ion_filename, self.encoding);
	try
		try
			while not Reader.EndOfStream do
			begin
				line := Reader.ReadLine;
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
			result := -1;
		end;
	finally
		Reader.Free;
	end;
end;

end.

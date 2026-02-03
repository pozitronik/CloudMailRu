unit TranslationManager;

{Loads and applies .lng translation files to LanguageStrings vars at runtime.
	Uses TranslationRegistry from LanguageStrings to map key names to var addresses.}

interface

uses
	FileSystem;

type
	TTranslationManager = class
	private
		FFileSystem: IFileSystem;
		FLanguageDir: WideString;

		{Parses a single line from a .lng file into key and value.
			Returns True if the line contains a valid KEY="value" pair.}
		function ParseLine(const Line: WideString; out Key, Value: WideString): Boolean;

		{Applies a single key-value pair via TranslationRegistry.
			Returns True if the key was found and applied.}
		function ApplyTranslation(const Key, Value: WideString): Boolean;

		{Returns full file path for a language name}
		function GetFilePath(const LanguageName: WideString): WideString;
	public
		constructor Create(const AFileSystem: IFileSystem; const ALanguageDir: WideString);

		{Lists available translations (file names without extension) from the language directory}
		function GetAvailableTranslations: TArray<WideString>;

		{Reads TRANSLATION_NAME value from a .lng file without applying it.
			Returns empty string if the key is missing or file is unreadable.}
		function ReadTranslationName(const LanguageName: WideString): WideString;

		{Checks that a .lng file is structurally valid. Returns False with ErrorMsg on structural errors.}
		function ValidateFile(const LanguageName: WideString; out ErrorMsg: WideString): Boolean;

		{Resets all strings to defaults, loads and applies a .lng file.
			Returns True on success, False with ErrorMsg on failure.}
		function Apply(const LanguageName: WideString; out ErrorMsg: WideString): Boolean;

		{Restores all strings to their English defaults}
		procedure Reset;
	end;

implementation

uses
	SysUtils,
	Classes,
	Generics.Collections,
	LanguageStrings;

constructor TTranslationManager.Create(const AFileSystem: IFileSystem; const ALanguageDir: WideString);
begin
	inherited Create;
	FFileSystem := AFileSystem;
	FLanguageDir := ALanguageDir;
end;

function TTranslationManager.GetFilePath(const LanguageName: WideString): WideString;
begin
	Result := FLanguageDir + LanguageName + '.lng';
end;

function TTranslationManager.ParseLine(const Line: WideString; out Key, Value: WideString): Boolean;
var
	TrimmedLine: WideString;
	EqPos: Integer;
	RawValue: WideString;
begin
	Result := False;
	Key := '';
	Value := '';

	TrimmedLine := Trim(Line);
	if (TrimmedLine = '') or (TrimmedLine[1] = '#') then
		Exit; {Comment or empty line -- not an error, just skip}

	EqPos := Pos('=', TrimmedLine);
	if EqPos < 2 then
		Exit; {No '=' or starts with '=' -- malformed}

	Key := Trim(Copy(TrimmedLine, 1, EqPos - 1));
	RawValue := Trim(Copy(TrimmedLine, EqPos + 1, MaxInt));

	{Value must be quoted}
	if (Length(RawValue) < 2) or (RawValue[1] <> '"') or (RawValue[Length(RawValue)] <> '"') then
		Exit;

	{Strip quotes and convert \n escape sequences to line breaks}
	Value := Copy(RawValue, 2, Length(RawValue) - 2);
	Value := StringReplace(Value, '\n', SLineBreak, [rfReplaceAll]);
	Result := True;
end;

function TTranslationManager.ApplyTranslation(const Key, Value: WideString): Boolean;
var
	P: PWideString;
begin
	Result := TranslationRegistry.TryGetValue(UpperCase(Key), P);
	if Result then
		P^ := Value;
end;

function TTranslationManager.ReadTranslationName(const LanguageName: WideString): WideString;
var
	FilePath: WideString;
	Lines: TStringList;
	I: Integer;
	Key, Value: WideString;
begin
	Result := '';
	FilePath := GetFilePath(LanguageName);

	if not FFileSystem.FileExists(FilePath) then
		Exit;

	Lines := FFileSystem.ReadAllLines(FilePath, TEncoding.UTF8);
	try
		for I := 0 to Lines.Count - 1 do
		begin
			if not ParseLine(Lines[I], Key, Value) then
				Continue;

			if UpperCase(Key) = 'TRANSLATION_NAME' then
			begin
				Result := Value;
				Exit;
			end;
		end;
	finally
		Lines.Free;
	end;
end;

function TTranslationManager.GetAvailableTranslations: TArray<WideString>;
var
	Files: TStringList;
	I: Integer;
begin
	Files := FFileSystem.FindFiles(FLanguageDir + '*.lng');
	try
		SetLength(Result, Files.Count);
		for I := 0 to Files.Count - 1 do
			Result[I] := ChangeFileExt(ExtractFileName(Files[I]), '');
	finally
		Files.Free;
	end;
end;

function TTranslationManager.ValidateFile(const LanguageName: WideString; out ErrorMsg: WideString): Boolean;
var
	FilePath: WideString;
	Lines: TStringList;
	I: Integer;
	TrimmedLine: WideString;
	Key, Value: WideString;
begin
	ErrorMsg := '';
	FilePath := GetFilePath(LanguageName);

	if not FFileSystem.FileExists(FilePath) then
	begin
		ErrorMsg := Format('Translation file not found: %s', [FilePath]);
		Result := False;
		Exit;
	end;

	Lines := FFileSystem.ReadAllLines(FilePath, TEncoding.UTF8);
	try
		for I := 0 to Lines.Count - 1 do
		begin
			TrimmedLine := Trim(Lines[I]);
			{Skip empty lines and comments}
			if (TrimmedLine = '') or (TrimmedLine[1] = '#') then
				Continue;

			if not ParseLine(Lines[I], Key, Value) then
			begin
				ErrorMsg := Format('Malformed line %d: %s', [I + 1, Lines[I]]);
				Result := False;
				Exit;
			end;
		end;
	finally
		Lines.Free;
	end;

	Result := True;
end;

function TTranslationManager.Apply(const LanguageName: WideString; out ErrorMsg: WideString): Boolean;
var
	FilePath: WideString;
	Lines: TStringList;
	I: Integer;
	TrimmedLine: WideString;
	Key, Value: WideString;
begin
	ErrorMsg := '';
	FilePath := GetFilePath(LanguageName);

	if not FFileSystem.FileExists(FilePath) then
	begin
		ErrorMsg := Format('Translation file not found: %s', [FilePath]);
		Result := False;
		Exit;
	end;

	{Reset to defaults before applying, so missing keys keep English values}
	Reset;

	Lines := FFileSystem.ReadAllLines(FilePath, TEncoding.UTF8);
	try
		for I := 0 to Lines.Count - 1 do
		begin
			TrimmedLine := Trim(Lines[I]);
			if (TrimmedLine = '') or (TrimmedLine[1] = '#') then
				Continue;

			if not ParseLine(Lines[I], Key, Value) then
			begin
				ErrorMsg := Format('Malformed line %d: %s', [I + 1, Lines[I]]);
				Result := False;
				Exit;
			end;

			{Unknown keys are silently ignored for forward compatibility}
			ApplyTranslation(Key, Value);
		end;
	finally
		Lines.Free;
	end;

	Result := True;
end;

procedure TTranslationManager.Reset;
begin
	InitializeDefaults;
end;

end.

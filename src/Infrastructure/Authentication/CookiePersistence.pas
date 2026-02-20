unit CookiePersistence;

{Cookie file persistence for VK ID authentication.
	Saves and loads Indy cookies + CSRF token to/from TSV files,
	allowing session reuse across application restarts.}

interface

uses
	IdCookieManager,
	FileSystem;

type
	TCookiePersistence = class
	private
		FFilePath: WideString;
		FFileSystem: IFileSystem;

		const CSRF_PREFIX = '__csrf__';
		const COOKIE_DIR = 'cookies';
		const COOKIE_EXT = '.cookies';
	public
		constructor Create(const FilePath: WideString; FileSystem: IFileSystem);

		{Saves cookies and CSRF token to file. Returns True on success.}
		function Save(CookieManager: TIdCookieManager; const CSRFToken: WideString): Boolean;

		{Loads cookies and CSRF token from file. Returns True if file existed and had valid content.}
		function Load(CookieManager: TIdCookieManager; var CSRFToken: WideString): Boolean;

		{Deletes the cookie file if it exists.}
		procedure Delete;

		{Returns True if the cookie file exists.}
		function Exists: Boolean;

		{Builds full cookie file path from config directory and account name.
			Result: <ConfigDir>\cookies\<sanitized_name>.cookies}
		class function BuildFilePath(const ConfigDir, AccountName: WideString): WideString; static;

		{Replaces non-filesystem-safe characters in account name.
			@ -> _at_, other unsafe chars -> _}
		class function SanitizeAccountName(const AccountName: WideString): WideString; static;
	end;

implementation

uses
	SysUtils, Classes,
	IdCookie, IdURI;

constructor TCookiePersistence.Create(const FilePath: WideString; FileSystem: IFileSystem);
begin
	inherited Create;
	FFilePath := FilePath;
	FFileSystem := FileSystem;
end;

function TCookiePersistence.Save(CookieManager: TIdCookieManager; const CSRFToken: WideString): Boolean;
var
	Lines: TStringList;
	I: Integer;
	Cookie: TIdCookie;
	SecureFlag, HttpOnlyFlag: WideString;
begin
	Result := False;
	if (FFilePath = '') or (CookieManager = nil) then
		Exit;

	// Ensure cookies directory exists; silently ignored if path is not writable
	// (e.g., in tests using TMemoryFileSystem)
	try
		ForceDirectories(ExtractFilePath(FFilePath));
	except
	end;

	Lines := TStringList.Create;
	try
		// First line: CSRF token
		Lines.Add(CSRF_PREFIX + #9 + CSRFToken);

		// Remaining lines: one cookie per line as TSV
		// Format: Name<TAB>Value<TAB>Domain<TAB>Path<TAB>Secure(0/1)<TAB>HttpOnly(0/1)
		for I := 0 to CookieManager.CookieCollection.Count - 1 do
		begin
			Cookie := CookieManager.CookieCollection.Cookies[I];

			if Cookie.Secure then
				SecureFlag := '1'
			else
				SecureFlag := '0';

			if Cookie.HttpOnly then
				HttpOnlyFlag := '1'
			else
				HttpOnlyFlag := '0';

			Lines.Add(
				Cookie.CookieName + #9 +
				Cookie.Value + #9 +
				Cookie.Domain + #9 +
				Cookie.Path + #9 +
				SecureFlag + #9 +
				HttpOnlyFlag
			);
		end;

		FFileSystem.WriteAllLines(FFilePath, Lines, TEncoding.UTF8);
		Result := True;
	finally
		Lines.Free;
	end;
end;

function TCookiePersistence.Load(CookieManager: TIdCookieManager; var CSRFToken: WideString): Boolean;
var
	Lines: TStringList;
	I: Integer;
	Line: WideString;
	Parts: TStringList;
	CookieStr: WideString;
	ServerCookieURL: TIdURI;
	FoundCSRF: Boolean;
begin
	Result := False;
	CSRFToken := '';

	if (FFilePath = '') or (CookieManager = nil) then
		Exit;

	if not FFileSystem.FileExists(FFilePath) then
		Exit;

	Lines := FFileSystem.ReadAllLines(FFilePath, TEncoding.UTF8);
	try
		if Lines.Count = 0 then
			Exit;

		FoundCSRF := False;
		Parts := TStringList.Create;
		try
			Parts.Delimiter := #9;
			Parts.StrictDelimiter := True;

			for I := 0 to Lines.Count - 1 do
			begin
				Line := Trim(Lines[I]);
				if Line = '' then
					Continue;

				Parts.DelimitedText := Line;

				// CSRF token line: __csrf__<TAB>token
				if (Parts.Count >= 2) and (Parts[0] = CSRF_PREFIX) then
				begin
					CSRFToken := Parts[1];
					FoundCSRF := True;
					Continue;
				end;

				// Cookie line: Name<TAB>Value<TAB>Domain<TAB>Path<TAB>Secure<TAB>HttpOnly
				if Parts.Count < 4 then
					Continue;

				// Build Set-Cookie header string for AddServerCookie
				CookieStr := Parts[0] + '=' + Parts[1]
					+ '; Domain=' + Parts[2]
					+ '; Path=' + Parts[3];

				if (Parts.Count >= 5) and (Parts[4] = '1') then
					CookieStr := CookieStr + '; Secure';

				if (Parts.Count >= 6) and (Parts[5] = '1') then
					CookieStr := CookieStr + '; HttpOnly';

				ServerCookieURL := TIdURI.Create('https://cloud.mail.ru/');
				try
					CookieManager.AddServerCookie(CookieStr, ServerCookieURL);
				finally
					ServerCookieURL.Free;
				end;
			end;
		finally
			Parts.Free;
		end;

		// Valid file must have at least a CSRF token
		Result := FoundCSRF and (CSRFToken <> '');
	finally
		Lines.Free;
	end;
end;

procedure TCookiePersistence.Delete;
begin
	if (FFilePath <> '') and FFileSystem.FileExists(FFilePath) then
		FFileSystem.DeleteFile(FFilePath);
end;

function TCookiePersistence.Exists: Boolean;
begin
	Result := (FFilePath <> '') and FFileSystem.FileExists(FFilePath);
end;

class function TCookiePersistence.BuildFilePath(const ConfigDir, AccountName: WideString): WideString;
begin
	Result := IncludeTrailingPathDelimiter(ConfigDir)
		+ COOKIE_DIR + PathDelim
		+ SanitizeAccountName(AccountName)
		+ COOKIE_EXT;
end;

class function TCookiePersistence.SanitizeAccountName(const AccountName: WideString): WideString;
var
	I: Integer;
	Ch: WideChar;
begin
	Result := StringReplace(AccountName, '@', '_at_', [rfReplaceAll]);
	for I := 1 to Length(Result) do
	begin
		Ch := Result[I];
		// Keep only filesystem-safe characters
		if not CharInSet(Ch, ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.']) then
			Result[I] := '_';
	end;
end;

end.

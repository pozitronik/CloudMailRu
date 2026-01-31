unit OpenSSLProvider;

{Centralized OpenSSL library provider for the application.

	Purpose:
	- Provides unified access to OpenSSL EVP functions for SHA1 hashing
	- Reuses library handle from Indy if already loaded (for HTTPS)
	- Respects LoadSSLDLLOnlyFromPluginDir security setting
	- Prevents duplicate DLL loading across different components

	Usage:
	1. Create provider early in application initialization (after settings loaded)
	2. Pass IOpenSSLProvider to components that need OpenSSL functions
	3. Provider handles library loading and function resolution transparently}

interface

uses
	Windows;

type
	{EVP function pointers for hashing and cipher operations}
	TOpenSSLFunctions = record
		{Hash functions (SHA1)}
		EVP_MD_CTX_new: function: Pointer; cdecl;
		EVP_MD_CTX_free: procedure(ctx: Pointer); cdecl;
		EVP_sha1: function: Pointer; cdecl;
		EVP_DigestInit_ex: function(ctx, md, impl: Pointer): Integer; cdecl;
		EVP_DigestUpdate: function(ctx: Pointer; d: Pointer; cnt: NativeUInt): Integer; cdecl;
		EVP_DigestFinal_ex: function(ctx: Pointer; md: PByte; var s: Cardinal): Integer; cdecl;
		Loaded: Boolean; {True if all hash functions resolved}

		{Cipher functions (AES-256 CFB-8 + PBKDF2)}
		EVP_CIPHER_CTX_new: function: Pointer; cdecl;
		EVP_CIPHER_CTX_free: procedure(ctx: Pointer); cdecl;
		EVP_aes_256_cfb8: function: Pointer; cdecl;
		EVP_EncryptInit_ex: function(ctx, cipher_type, impl, key, iv: Pointer): Integer; cdecl;
		EVP_EncryptUpdate: function(ctx: Pointer; outbuf: PByte; var outlen: Integer; inbuf: PByte; inlen: Integer): Integer; cdecl;
		EVP_DecryptInit_ex: function(ctx, cipher_type, impl, key, iv: Pointer): Integer; cdecl;
		EVP_DecryptUpdate: function(ctx: Pointer; outbuf: PByte; var outlen: Integer; inbuf: PByte; inlen: Integer): Integer; cdecl;
		EVP_sha256: function: Pointer; cdecl;
		PKCS5_PBKDF2_HMAC: function(pass: PAnsiChar; passlen: Integer; salt: PByte; saltlen: Integer; iter: Integer; digest: Pointer; keylen: Integer; outkey: PByte): Integer; cdecl;
		CipherLoaded: Boolean; {True if all cipher functions resolved}
	end;

	{Interface for OpenSSL library access - allows dependency injection}
	IOpenSSLProvider = interface
		['{A7E5B3C1-8F2D-4A6E-9C1B-3D5E7F8A2B4C}']
		{Check if OpenSSL functions are available
			@return True if library is loaded and functions resolved}
		function IsAvailable: Boolean;
		{Get the resolved OpenSSL function pointers
			@return Record containing function pointers and availability flag}
		function GetFunctions: TOpenSSLFunctions;
		{Get the loaded library handle (0 if not loaded)
			@return Windows handle to the loaded OpenSSL library}
		function GetLibraryHandle: THandle;
	end;

	{OpenSSL provider implementation that centralizes library loading.
		Checks Indy's already-loaded library first, then falls back to loading
		using plugin path settings.}
	TOpenSSLProvider = class(TInterfacedObject, IOpenSSLProvider)
	private
		FPluginPath: string;
		FLoadFromPluginDirOnly: Boolean;
		FHandle: THandle;
		FFunctions: TOpenSSLFunctions;
		FLoadAttempted: Boolean;
		{Try to load OpenSSL library, checking Indy first}
		procedure TryLoadLibrary;
		{Resolve function pointers from loaded library}
		procedure LoadFunctionPointers;
		{Try to load a specific DLL by name
			@param DLLName Name of the DLL to load
			@return True if library was loaded successfully}
		function TryLoadDLL(const DLLName: string): Boolean;
	public
		{Create OpenSSL provider with path settings
			@param PluginPath Path to the plugin directory
			@param LoadFromPluginDirOnly If true, only load from plugin directory}
		constructor Create(const PluginPath: string; LoadFromPluginDirOnly: Boolean);
		destructor Destroy; override;
		function IsAvailable: Boolean;
		function GetFunctions: TOpenSSLFunctions;
		function GetLibraryHandle: THandle;
	end;

	{Null implementation for testing or when OpenSSL is not needed}
	TNullOpenSSLProvider = class(TInterfacedObject, IOpenSSLProvider)
	public
		function IsAvailable: Boolean;
		function GetFunctions: TOpenSSLFunctions;
		function GetLibraryHandle: THandle;
	end;

implementation

uses
	SysUtils,
	IdSSLOpenSSLHeaders;

const
	{Platform-specific subdirectory for DLLs}
{$IFDEF WIN64}
	PlatformDllPath = 'x64';
{$ENDIF}
{$IFDEF WIN32}
	PlatformDllPath = 'x32';
{$ENDIF}

{TOpenSSLProvider}

constructor TOpenSSLProvider.Create(const PluginPath: string; LoadFromPluginDirOnly: Boolean);
begin
	inherited Create;
	FPluginPath := PluginPath;
	FLoadFromPluginDirOnly := LoadFromPluginDirOnly;
	FHandle := 0;
	FLoadAttempted := False;
	FillChar(FFunctions, SizeOf(FFunctions), 0);
end;

destructor TOpenSSLProvider.Destroy;
begin
	{Do not free library handle - it may be shared with Indy or used elsewhere}
	inherited;
end;

procedure TOpenSSLProvider.TryLoadLibrary;
var
	PlatformPath: string;
begin
	if FLoadAttempted then
		Exit;
	FLoadAttempted := True;

	{First check if Indy already loaded the crypto library}
	FHandle := GetCryptLibHandle();
	if FHandle <> 0 then
	begin
		LoadFunctionPointers;
		Exit;
	end;

	{Load ourselves using plugin path settings}
	if FLoadFromPluginDirOnly then
	begin
		{Try platform subdirectory first}
		PlatformPath := IncludeTrailingPathDelimiter(FPluginPath) + PlatformDllPath;
		if DirectoryExists(PlatformPath) then
		begin
			{Try OpenSSL 1.1.x first (preferred)}
{$IFDEF WIN64}
			if TryLoadDLL(PlatformPath + '\libcrypto-1_1-x64.dll') then
				Exit;
{$ELSE}
			if TryLoadDLL(PlatformPath + '\libcrypto-1_1.dll') then
				Exit;
{$ENDIF}
			{Try OpenSSL 1.0.x (legacy)}
			if TryLoadDLL(PlatformPath + '\libeay32.dll') then
				Exit;
			{Try generic name}
			if TryLoadDLL(PlatformPath + '\libcrypto.dll') then
				Exit;
		end;

		{Try plugin root directory}
{$IFDEF WIN64}
		if TryLoadDLL(FPluginPath + 'libcrypto-1_1-x64.dll') then
			Exit;
{$ELSE}
		if TryLoadDLL(FPluginPath + 'libcrypto-1_1.dll') then
			Exit;
{$ENDIF}
		if TryLoadDLL(FPluginPath + 'libeay32.dll') then
			Exit;
		if TryLoadDLL(FPluginPath + 'libcrypto.dll') then
			Exit;
	end
	else
	begin
		{Search system path - try all known names}
{$IFDEF WIN64}
		if TryLoadDLL('libcrypto-1_1-x64.dll') then
			Exit;
{$ELSE}
		if TryLoadDLL('libcrypto-1_1.dll') then
			Exit;
{$ENDIF}
		if TryLoadDLL('libeay32.dll') then
			Exit;
		if TryLoadDLL('libcrypto.dll') then
			Exit;
	end;
end;

function TOpenSSLProvider.TryLoadDLL(const DLLName: string): Boolean;
begin
	FHandle := LoadLibrary(PChar(DLLName));
	Result := FHandle <> 0;
	if Result then
		LoadFunctionPointers;
end;

procedure TOpenSSLProvider.LoadFunctionPointers;
var
	LegacyNew: function: Pointer; cdecl;
	LegacyFree: procedure(ctx: Pointer); cdecl;
begin
	if FHandle = 0 then
		Exit;

	{Try OpenSSL 1.1.x function names first}
	@FFunctions.EVP_MD_CTX_new := GetProcAddress(FHandle, 'EVP_MD_CTX_new');
	@FFunctions.EVP_MD_CTX_free := GetProcAddress(FHandle, 'EVP_MD_CTX_free');

	{Fall back to OpenSSL 1.0.x function names}
	if not Assigned(FFunctions.EVP_MD_CTX_new) then
	begin
		@LegacyNew := GetProcAddress(FHandle, 'EVP_MD_CTX_create');
		@LegacyFree := GetProcAddress(FHandle, 'EVP_MD_CTX_destroy');
		@FFunctions.EVP_MD_CTX_new := @LegacyNew;
		@FFunctions.EVP_MD_CTX_free := @LegacyFree;
	end;

	@FFunctions.EVP_sha1 := GetProcAddress(FHandle, 'EVP_sha1');
	@FFunctions.EVP_DigestInit_ex := GetProcAddress(FHandle, 'EVP_DigestInit_ex');
	@FFunctions.EVP_DigestUpdate := GetProcAddress(FHandle, 'EVP_DigestUpdate');
	@FFunctions.EVP_DigestFinal_ex := GetProcAddress(FHandle, 'EVP_DigestFinal_ex');

	FFunctions.Loaded := Assigned(FFunctions.EVP_MD_CTX_new) and
		Assigned(FFunctions.EVP_MD_CTX_free) and
		Assigned(FFunctions.EVP_sha1) and
		Assigned(FFunctions.EVP_DigestInit_ex) and
		Assigned(FFunctions.EVP_DigestUpdate) and
		Assigned(FFunctions.EVP_DigestFinal_ex);

	{Resolve cipher functions -- separate flag so hash-only usage is unaffected}
	@FFunctions.EVP_CIPHER_CTX_new := GetProcAddress(FHandle, 'EVP_CIPHER_CTX_new');
	@FFunctions.EVP_CIPHER_CTX_free := GetProcAddress(FHandle, 'EVP_CIPHER_CTX_free');
	@FFunctions.EVP_aes_256_cfb8 := GetProcAddress(FHandle, 'EVP_aes_256_cfb8');
	@FFunctions.EVP_EncryptInit_ex := GetProcAddress(FHandle, 'EVP_EncryptInit_ex');
	@FFunctions.EVP_EncryptUpdate := GetProcAddress(FHandle, 'EVP_EncryptUpdate');
	@FFunctions.EVP_DecryptInit_ex := GetProcAddress(FHandle, 'EVP_DecryptInit_ex');
	@FFunctions.EVP_DecryptUpdate := GetProcAddress(FHandle, 'EVP_DecryptUpdate');
	@FFunctions.EVP_sha256 := GetProcAddress(FHandle, 'EVP_sha256');
	@FFunctions.PKCS5_PBKDF2_HMAC := GetProcAddress(FHandle, 'PKCS5_PBKDF2_HMAC');

	FFunctions.CipherLoaded := Assigned(FFunctions.EVP_CIPHER_CTX_new) and
		Assigned(FFunctions.EVP_CIPHER_CTX_free) and
		Assigned(FFunctions.EVP_aes_256_cfb8) and
		Assigned(FFunctions.EVP_EncryptInit_ex) and
		Assigned(FFunctions.EVP_EncryptUpdate) and
		Assigned(FFunctions.EVP_DecryptInit_ex) and
		Assigned(FFunctions.EVP_DecryptUpdate) and
		Assigned(FFunctions.EVP_sha256) and
		Assigned(FFunctions.PKCS5_PBKDF2_HMAC);
end;

function TOpenSSLProvider.IsAvailable: Boolean;
begin
	TryLoadLibrary;
	Result := FFunctions.Loaded;
end;

function TOpenSSLProvider.GetFunctions: TOpenSSLFunctions;
begin
	TryLoadLibrary;
	Result := FFunctions;
end;

function TOpenSSLProvider.GetLibraryHandle: THandle;
begin
	TryLoadLibrary;
	Result := FHandle;
end;

{TNullOpenSSLProvider}

function TNullOpenSSLProvider.IsAvailable: Boolean;
begin
	Result := False;
end;

function TNullOpenSSLProvider.GetFunctions: TOpenSSLFunctions;
begin
	FillChar(Result, SizeOf(Result), 0);
end;

function TNullOpenSSLProvider.GetLibraryHandle: THandle;
begin
	Result := 0;
end;

end.

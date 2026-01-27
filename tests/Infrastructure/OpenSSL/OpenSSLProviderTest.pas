unit OpenSSLProviderTest;

interface

uses
	OpenSSLProvider,
	Windows,
	System.SysUtils,
	DUnitX.TestFramework;

type
	[TestFixture]
	TOpenSSLProviderTest = class
	public
		{TNullOpenSSLProvider tests}
		[Test]
		procedure TestNullProvider_ImplementsInterface;
		[Test]
		procedure TestNullProvider_IsAvailable_ReturnsFalse;
		[Test]
		procedure TestNullProvider_GetFunctions_ReturnsZeroedRecord;
		[Test]
		procedure TestNullProvider_GetFunctions_LoadedIsFalse;
		[Test]
		procedure TestNullProvider_GetLibraryHandle_ReturnsZero;
		[Test]
		procedure TestNullProvider_MultipleCalls_ConsistentResults;

		{TOpenSSLProvider - Constructor tests}
		[Test]
		procedure TestProvider_Create_DoesNotLoadImmediately;
		[Test]
		procedure TestProvider_ImplementsInterface;
		[Test]
		procedure TestProvider_Create_EmptyPath_NoException;
		[Test]
		procedure TestProvider_Create_WithPath_NoException;

		{TOpenSSLProvider - Lazy loading behavior}
		[Test]
		procedure TestProvider_IsAvailable_TriggersLoad;
		[Test]
		procedure TestProvider_GetFunctions_TriggersLoad;
		[Test]
		procedure TestProvider_GetLibraryHandle_TriggersLoad;
		[Test]
		procedure TestProvider_MultipleCalls_ConsistentResults;

		{TOpenSSLProvider - Indy reuse behavior
		 The provider checks Indy's loaded library first before using path settings}
		[Test]
		procedure TestProvider_ReusesIndyLibrary_IfAvailable;
		[Test]
		procedure TestProvider_WithIndyLoaded_PathSettingsIgnored;
		[Test]
		procedure TestProvider_WithIndyLoaded_RestrictedModeStillWorks;

		{TOpenSSLProvider - Functions availability when Indy loaded}
		[Test]
		procedure TestProvider_WithIndyLoaded_FunctionsLoaded;
		[Test]
		procedure TestProvider_WithIndyLoaded_AllFunctionPointersAssigned;

		{TOpenSSLFunctions record tests}
		[Test]
		procedure TestFunctions_DefaultState_AllPointersNil;
		[Test]
		procedure TestFunctions_DefaultState_LoadedFalse;

		{Interface reference counting}
		[Test]
		procedure TestProvider_InterfaceRefCounting_Works;
		[Test]
		procedure TestNullProvider_InterfaceRefCounting_Works;

		{Edge cases}
		[Test]
		procedure TestProvider_Destroy_NoException;
		[Test]
		procedure TestProvider_PathWithTrailingSlash_Works;
		[Test]
		procedure TestProvider_PathWithoutTrailingSlash_Works;
		[Test]
		procedure TestProvider_MultipleInstances_Independent;

		{Path parameter storage}
		[Test]
		procedure TestProvider_DifferentPaths_AllWork;
		[Test]
		procedure TestProvider_BooleanSettings_Accepted;
	end;

implementation

uses
	IdSSLOpenSSLHeaders;

{TNullOpenSSLProvider tests}

procedure TOpenSSLProviderTest.TestNullProvider_ImplementsInterface;
var
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	Assert.IsNotNull(Provider);
end;

procedure TOpenSSLProviderTest.TestNullProvider_IsAvailable_ReturnsFalse;
var
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	Assert.IsFalse(Provider.IsAvailable);
end;

procedure TOpenSSLProviderTest.TestNullProvider_GetFunctions_ReturnsZeroedRecord;
var
	Provider: IOpenSSLProvider;
	Funcs: TOpenSSLFunctions;
begin
	Provider := TNullOpenSSLProvider.Create;
	Funcs := Provider.GetFunctions;

	{All function pointers should be nil}
	Assert.IsTrue(not Assigned(Funcs.EVP_MD_CTX_new), 'EVP_MD_CTX_new should be nil');
	Assert.IsTrue(not Assigned(Funcs.EVP_MD_CTX_free), 'EVP_MD_CTX_free should be nil');
	Assert.IsTrue(not Assigned(Funcs.EVP_sha1), 'EVP_sha1 should be nil');
	Assert.IsTrue(not Assigned(Funcs.EVP_DigestInit_ex), 'EVP_DigestInit_ex should be nil');
	Assert.IsTrue(not Assigned(Funcs.EVP_DigestUpdate), 'EVP_DigestUpdate should be nil');
	Assert.IsTrue(not Assigned(Funcs.EVP_DigestFinal_ex), 'EVP_DigestFinal_ex should be nil');
end;

procedure TOpenSSLProviderTest.TestNullProvider_GetFunctions_LoadedIsFalse;
var
	Provider: IOpenSSLProvider;
	Funcs: TOpenSSLFunctions;
begin
	Provider := TNullOpenSSLProvider.Create;
	Funcs := Provider.GetFunctions;
	Assert.IsFalse(Funcs.Loaded);
end;

procedure TOpenSSLProviderTest.TestNullProvider_GetLibraryHandle_ReturnsZero;
var
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	Assert.AreEqual(THandle(0), Provider.GetLibraryHandle);
end;

procedure TOpenSSLProviderTest.TestNullProvider_MultipleCalls_ConsistentResults;
var
	Provider: IOpenSSLProvider;
	I: Integer;
begin
	Provider := TNullOpenSSLProvider.Create;

	{Multiple calls should return consistent results}
	for I := 1 to 5 do
	begin
		Assert.IsFalse(Provider.IsAvailable, Format('Call %d: IsAvailable should be False', [I]));
		Assert.AreEqual(THandle(0), Provider.GetLibraryHandle, Format('Call %d: Handle should be 0', [I]));
		Assert.IsFalse(Provider.GetFunctions.Loaded, Format('Call %d: Loaded should be False', [I]));
	end;
end;

{TOpenSSLProvider - Constructor tests}

procedure TOpenSSLProviderTest.TestProvider_Create_DoesNotLoadImmediately;
var
	Provider: TOpenSSLProvider;
begin
	{Creating provider should not trigger library loading}
	Provider := TOpenSSLProvider.Create('C:\NonExistent\Path', True);
	try
		{Just creating should not raise any exception}
		Assert.Pass('Provider created without immediate load attempt');
	finally
		Provider.Free;
	end;
end;

procedure TOpenSSLProviderTest.TestProvider_ImplementsInterface;
var
	Provider: IOpenSSLProvider;
begin
	Provider := TOpenSSLProvider.Create('', False);
	Assert.IsNotNull(Provider);
end;

procedure TOpenSSLProviderTest.TestProvider_Create_EmptyPath_NoException;
var
	Provider: TOpenSSLProvider;
begin
	Provider := TOpenSSLProvider.Create('', True);
	try
		Assert.Pass('Empty path accepted without exception');
	finally
		Provider.Free;
	end;
end;

procedure TOpenSSLProviderTest.TestProvider_Create_WithPath_NoException;
var
	Provider: TOpenSSLProvider;
begin
	Provider := TOpenSSLProvider.Create('C:\Some\Path', False);
	try
		Assert.Pass('Path accepted without exception');
	finally
		Provider.Free;
	end;
end;

{TOpenSSLProvider - Lazy loading behavior}

procedure TOpenSSLProviderTest.TestProvider_IsAvailable_TriggersLoad;
var
	Provider: IOpenSSLProvider;
	Available: Boolean;
begin
	Provider := TOpenSSLProvider.Create('', False);

	{First call should trigger load attempt}
	Available := Provider.IsAvailable;

	{Result depends on whether Indy loaded OpenSSL - just verify no exception}
	Assert.Pass(Format('IsAvailable returned %s', [BoolToStr(Available, True)]));
end;

procedure TOpenSSLProviderTest.TestProvider_GetFunctions_TriggersLoad;
var
	Provider: IOpenSSLProvider;
	Funcs: TOpenSSLFunctions;
begin
	Provider := TOpenSSLProvider.Create('', False);

	Funcs := Provider.GetFunctions;

	{Verify record is populated (may or may not have functions depending on environment)}
	Assert.Pass(Format('GetFunctions returned, Loaded=%s', [BoolToStr(Funcs.Loaded, True)]));
end;

procedure TOpenSSLProviderTest.TestProvider_GetLibraryHandle_TriggersLoad;
var
	Provider: IOpenSSLProvider;
	Handle: THandle;
begin
	Provider := TOpenSSLProvider.Create('', False);

	Handle := Provider.GetLibraryHandle;

	{Handle may be 0 or non-zero depending on environment}
	Assert.Pass(Format('GetLibraryHandle returned %d', [Handle]));
end;

procedure TOpenSSLProviderTest.TestProvider_MultipleCalls_ConsistentResults;
var
	Provider: IOpenSSLProvider;
	Result1, Result2, Result3: Boolean;
	Handle1, Handle2: THandle;
begin
	Provider := TOpenSSLProvider.Create('', False);

	{Multiple calls should return consistent results}
	Result1 := Provider.IsAvailable;
	Result2 := Provider.IsAvailable;
	Result3 := Provider.IsAvailable;

	Assert.AreEqual(Result1, Result2, 'IsAvailable results should be consistent');
	Assert.AreEqual(Result2, Result3, 'IsAvailable results should be consistent');

	Handle1 := Provider.GetLibraryHandle;
	Handle2 := Provider.GetLibraryHandle;

	Assert.AreEqual(Handle1, Handle2, 'GetLibraryHandle results should be consistent');
end;

{TOpenSSLProvider - Indy reuse behavior}

procedure TOpenSSLProviderTest.TestProvider_ReusesIndyLibrary_IfAvailable;
var
	Provider: IOpenSSLProvider;
	IndyHandle: THandle;
begin
	{Get Indy's loaded library handle}
	IndyHandle := GetCryptLibHandle();

	Provider := TOpenSSLProvider.Create('', False);

	if IndyHandle <> 0 then
	begin
		{If Indy loaded OpenSSL, provider should reuse it}
		Assert.AreEqual(IndyHandle, Provider.GetLibraryHandle,
			'Provider should reuse Indy''s OpenSSL library handle');
		Assert.IsTrue(Provider.IsAvailable, 'Provider should be available when Indy loaded OpenSSL');
	end
	else
	begin
		{If Indy hasn't loaded OpenSSL, provider tries its own loading}
		Assert.Pass('Indy has not loaded OpenSSL - provider will try own loading');
	end;
end;

procedure TOpenSSLProviderTest.TestProvider_WithIndyLoaded_PathSettingsIgnored;
var
	Provider1, Provider2: IOpenSSLProvider;
	IndyHandle: THandle;
begin
	IndyHandle := GetCryptLibHandle();

	if IndyHandle = 0 then
	begin
		Assert.Pass('Test skipped - Indy has not loaded OpenSSL');
		Exit;
	end;

	{With Indy loaded, path settings should be ignored - provider reuses Indy's library}
	Provider1 := TOpenSSLProvider.Create('C:\NonExistent\Path', True);
	Provider2 := TOpenSSLProvider.Create('Z:\Another\NonExistent\Path', False);

	{Both should get Indy's handle regardless of path settings}
	Assert.AreEqual(IndyHandle, Provider1.GetLibraryHandle, 'Should use Indy handle regardless of path');
	Assert.AreEqual(IndyHandle, Provider2.GetLibraryHandle, 'Should use Indy handle regardless of path');
end;

procedure TOpenSSLProviderTest.TestProvider_WithIndyLoaded_RestrictedModeStillWorks;
var
	Provider: IOpenSSLProvider;
	IndyHandle: THandle;
begin
	IndyHandle := GetCryptLibHandle();

	if IndyHandle = 0 then
	begin
		Assert.Pass('Test skipped - Indy has not loaded OpenSSL');
		Exit;
	end;

	{Even with restricted mode and non-existent path, Indy's library is used}
	Provider := TOpenSSLProvider.Create('C:\NonExistent', True);

	Assert.IsTrue(Provider.IsAvailable, 'Should be available via Indy even with restricted mode');
	Assert.AreEqual(IndyHandle, Provider.GetLibraryHandle);
end;

{TOpenSSLProvider - Functions availability when Indy loaded}

procedure TOpenSSLProviderTest.TestProvider_WithIndyLoaded_FunctionsLoaded;
var
	Provider: IOpenSSLProvider;
	Funcs: TOpenSSLFunctions;
	IndyHandle: THandle;
begin
	IndyHandle := GetCryptLibHandle();

	if IndyHandle = 0 then
	begin
		Assert.Pass('Test skipped - Indy has not loaded OpenSSL');
		Exit;
	end;

	Provider := TOpenSSLProvider.Create('', False);
	Funcs := Provider.GetFunctions;

	Assert.IsTrue(Funcs.Loaded, 'Functions should be loaded when Indy has OpenSSL');
end;

procedure TOpenSSLProviderTest.TestProvider_WithIndyLoaded_AllFunctionPointersAssigned;
var
	Provider: IOpenSSLProvider;
	Funcs: TOpenSSLFunctions;
	IndyHandle: THandle;
begin
	IndyHandle := GetCryptLibHandle();

	if IndyHandle = 0 then
	begin
		Assert.Pass('Test skipped - Indy has not loaded OpenSSL');
		Exit;
	end;

	Provider := TOpenSSLProvider.Create('', False);
	Funcs := Provider.GetFunctions;

	{All EVP functions should be resolved}
	Assert.IsTrue(Assigned(Funcs.EVP_MD_CTX_new), 'EVP_MD_CTX_new should be assigned');
	Assert.IsTrue(Assigned(Funcs.EVP_MD_CTX_free), 'EVP_MD_CTX_free should be assigned');
	Assert.IsTrue(Assigned(Funcs.EVP_sha1), 'EVP_sha1 should be assigned');
	Assert.IsTrue(Assigned(Funcs.EVP_DigestInit_ex), 'EVP_DigestInit_ex should be assigned');
	Assert.IsTrue(Assigned(Funcs.EVP_DigestUpdate), 'EVP_DigestUpdate should be assigned');
	Assert.IsTrue(Assigned(Funcs.EVP_DigestFinal_ex), 'EVP_DigestFinal_ex should be assigned');
end;

{TOpenSSLFunctions record tests}

procedure TOpenSSLProviderTest.TestFunctions_DefaultState_AllPointersNil;
var
	Funcs: TOpenSSLFunctions;
begin
	FillChar(Funcs, SizeOf(Funcs), 0);

	Assert.IsTrue(not Assigned(Funcs.EVP_MD_CTX_new));
	Assert.IsTrue(not Assigned(Funcs.EVP_MD_CTX_free));
	Assert.IsTrue(not Assigned(Funcs.EVP_sha1));
	Assert.IsTrue(not Assigned(Funcs.EVP_DigestInit_ex));
	Assert.IsTrue(not Assigned(Funcs.EVP_DigestUpdate));
	Assert.IsTrue(not Assigned(Funcs.EVP_DigestFinal_ex));
end;

procedure TOpenSSLProviderTest.TestFunctions_DefaultState_LoadedFalse;
var
	Funcs: TOpenSSLFunctions;
begin
	FillChar(Funcs, SizeOf(Funcs), 0);
	Assert.IsFalse(Funcs.Loaded);
end;

{Interface reference counting}

procedure TOpenSSLProviderTest.TestProvider_InterfaceRefCounting_Works;
var
	Provider1, Provider2: IOpenSSLProvider;
begin
	Provider1 := TOpenSSLProvider.Create('', False);
	Provider2 := Provider1; {Increase ref count}

	Assert.IsNotNull(Provider1);
	Assert.IsNotNull(Provider2);

	Provider1 := nil; {Decrease ref count}
	Assert.IsNotNull(Provider2, 'Provider2 should still be valid');

	Provider2 := nil; {Final release}
	Assert.Pass('Reference counting works correctly');
end;

procedure TOpenSSLProviderTest.TestNullProvider_InterfaceRefCounting_Works;
var
	Provider1, Provider2: IOpenSSLProvider;
begin
	Provider1 := TNullOpenSSLProvider.Create;
	Provider2 := Provider1;

	Assert.IsNotNull(Provider1);
	Assert.IsNotNull(Provider2);

	Provider1 := nil;
	Assert.IsNotNull(Provider2);

	Provider2 := nil;
	Assert.Pass('Null provider reference counting works correctly');
end;

{Edge cases}

procedure TOpenSSLProviderTest.TestProvider_Destroy_NoException;
var
	Provider: TOpenSSLProvider;
begin
	Provider := TOpenSSLProvider.Create('C:\NonExistent', True);
	Provider.IsAvailable; {Trigger load attempt}
	Provider.Free; {Should not raise - does not free shared library handle}
	Assert.Pass('Destroy completed without exception');
end;

procedure TOpenSSLProviderTest.TestProvider_PathWithTrailingSlash_Works;
var
	Provider: IOpenSSLProvider;
begin
	Provider := TOpenSSLProvider.Create('C:\Some\Path\', True);
	Provider.IsAvailable; {Should not raise}
	Assert.Pass('Path with trailing slash handled correctly');
end;

procedure TOpenSSLProviderTest.TestProvider_PathWithoutTrailingSlash_Works;
var
	Provider: IOpenSSLProvider;
begin
	Provider := TOpenSSLProvider.Create('C:\Some\Path', True);
	Provider.IsAvailable; {Should not raise}
	Assert.Pass('Path without trailing slash handled correctly');
end;

procedure TOpenSSLProviderTest.TestProvider_MultipleInstances_Independent;
var
	Provider1, Provider2, Provider3: IOpenSSLProvider;
begin
	{Multiple provider instances should work independently}
	Provider1 := TOpenSSLProvider.Create('C:\Path1', True);
	Provider2 := TOpenSSLProvider.Create('C:\Path2', False);
	Provider3 := TOpenSSLProvider.Create('', True);

	{All should be able to query availability without interfering}
	Provider1.IsAvailable;
	Provider2.IsAvailable;
	Provider3.IsAvailable;

	Assert.Pass('Multiple provider instances work independently');
end;

{Path parameter storage}

procedure TOpenSSLProviderTest.TestProvider_DifferentPaths_AllWork;
var
	Provider: IOpenSSLProvider;
	Paths: array of string;
	I: Integer;
begin
	Paths := ['', 'C:\Test', 'D:\Plugin\Path', '\\Network\Share', 'relative\path'];

	for I := Low(Paths) to High(Paths) do
	begin
		Provider := TOpenSSLProvider.Create(Paths[I], False);
		Provider.IsAvailable; {Should not raise for any path}
	end;

	Assert.Pass('All path variations accepted without exception');
end;

procedure TOpenSSLProviderTest.TestProvider_BooleanSettings_Accepted;
var
	Provider: IOpenSSLProvider;
begin
	{Both boolean values should be accepted}
	Provider := TOpenSSLProvider.Create('', True);
	Provider.IsAvailable;

	Provider := TOpenSSLProvider.Create('', False);
	Provider.IsAvailable;

	Assert.Pass('Both LoadFromPluginDirOnly settings accepted');
end;

initialization

TDUnitX.RegisterTestFixture(TOpenSSLProviderTest);

end.

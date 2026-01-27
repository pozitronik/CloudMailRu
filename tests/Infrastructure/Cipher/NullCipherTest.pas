unit NullCipherTest;

interface

uses
	FileCipher,
	CloudDirItemList,
	CloudDirItem,
	System.SysUtils,
	System.IOUtils,
	System.Classes,
	DUnitX.TestFramework;

type
	[TestFixture]
	TNullCipherTest = class
	public
		[Test]
		{Verifies TNullCipher can be assigned to ICipher variable}
		procedure TestImplementsICipher;

		[Test]
		{Verifies CryptFile copies file unchanged and returns CIPHER_OK}
		procedure TestCryptFileCopiesUnchanged;

		[Test]
		{Verifies DecryptFile copies file unchanged and returns CIPHER_OK}
		procedure TestDecryptFileCopiesUnchanged;

		[Test]
		{Verifies CryptStream copies stream unchanged}
		procedure TestCryptStreamCopiesUnchanged;

		[Test]
		{Verifies DecryptStream copies stream unchanged}
		procedure TestDecryptStreamCopiesUnchanged;

		[Test]
		{Verifies CryptFileName returns filename unchanged}
		procedure TestCryptFileNameReturnsUnchanged;

		[Test]
		{Verifies DecryptFileName returns filename unchanged}
		procedure TestDecryptFileNameReturnsUnchanged;

		[Test]
		{Verifies DecryptDirListing sets visible_name to name for all items}
		procedure TestDecryptDirListingSetsVisibleName;

		[Test]
		{Verifies CryptFile returns CIPHER_IO_ERROR for non-existent source}
		procedure TestCryptFileNonExistentSource;

		[Test]
		{Verifies DecryptFile returns CIPHER_IO_ERROR for non-existent source}
		procedure TestDecryptFileNonExistentSource;

		[Test]
		{Verifies CryptStream handles empty stream}
		procedure TestCryptStreamEmptyStream;

		[Test]
		{Verifies CryptFileName extracts filename from path}
		procedure TestCryptFileNameExtractsFromPath;
	end;

implementation

procedure TNullCipherTest.TestImplementsICipher;
var
	Cipher: ICipher;
begin
	Cipher := TNullCipher.Create;
	Assert.IsNotNull(Cipher);
end;

procedure TNullCipherTest.TestCryptFileCopiesUnchanged;
var
	Cipher: ICipher;
	SourceFile, DestFile: string;
	OriginalContent, CopiedContent: string;
begin
	SourceFile := TPath.GetTempFileName;
	DestFile := TPath.GetTempFileName;
	try
		OriginalContent := 'Test content for null cipher copy';
		TFile.WriteAllText(SourceFile, OriginalContent);

		Cipher := TNullCipher.Create;
		Assert.AreEqual(CIPHER_OK, Cipher.CryptFile(SourceFile, DestFile));

		CopiedContent := TFile.ReadAllText(DestFile);
		Assert.AreEqual(OriginalContent, CopiedContent, 'Content should be unchanged');
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(DestFile) then TFile.Delete(DestFile);
	end;
end;

procedure TNullCipherTest.TestDecryptFileCopiesUnchanged;
var
	Cipher: ICipher;
	SourceFile, DestFile: string;
	OriginalContent, CopiedContent: string;
begin
	SourceFile := TPath.GetTempFileName;
	DestFile := TPath.GetTempFileName;
	try
		OriginalContent := 'Test content for null cipher decrypt copy';
		TFile.WriteAllText(SourceFile, OriginalContent);

		Cipher := TNullCipher.Create;
		Assert.AreEqual(CIPHER_OK, Cipher.DecryptFile(SourceFile, DestFile));

		CopiedContent := TFile.ReadAllText(DestFile);
		Assert.AreEqual(OriginalContent, CopiedContent, 'Content should be unchanged');
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(DestFile) then TFile.Delete(DestFile);
	end;
end;

procedure TNullCipherTest.TestCryptStreamCopiesUnchanged;
var
	Cipher: ICipher;
	SourceStream, DestStream: TMemoryStream;
	OriginalBytes, CopiedBytes: TBytes;
	BytesCopied: Integer;
begin
	SourceStream := TMemoryStream.Create;
	DestStream := TMemoryStream.Create;
	try
		OriginalBytes := TEncoding.UTF8.GetBytes('Stream content for null cipher');
		SourceStream.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		SourceStream.Position := 0;

		Cipher := TNullCipher.Create;
		BytesCopied := Cipher.CryptStream(SourceStream, DestStream);

		Assert.AreEqual(Integer(Length(OriginalBytes)), BytesCopied, 'Should copy all bytes');

		SetLength(CopiedBytes, DestStream.Size);
		DestStream.Position := 0;
		DestStream.ReadBuffer(CopiedBytes[0], DestStream.Size);

		Assert.AreEqual(
			TEncoding.UTF8.GetString(OriginalBytes),
			TEncoding.UTF8.GetString(CopiedBytes),
			'Content should be unchanged'
		);
	finally
		SourceStream.Free;
		DestStream.Free;
	end;
end;

procedure TNullCipherTest.TestDecryptStreamCopiesUnchanged;
var
	Cipher: ICipher;
	SourceStream, DestStream: TMemoryStream;
	OriginalBytes, CopiedBytes: TBytes;
	BytesCopied: Integer;
begin
	SourceStream := TMemoryStream.Create;
	DestStream := TMemoryStream.Create;
	try
		OriginalBytes := TEncoding.UTF8.GetBytes('Stream content for null cipher decrypt');
		SourceStream.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		SourceStream.Position := 0;

		Cipher := TNullCipher.Create;
		BytesCopied := Cipher.DecryptStream(SourceStream, DestStream);

		Assert.AreEqual(Integer(Length(OriginalBytes)), BytesCopied, 'Should copy all bytes');

		SetLength(CopiedBytes, DestStream.Size);
		DestStream.Position := 0;
		DestStream.ReadBuffer(CopiedBytes[0], DestStream.Size);

		Assert.AreEqual(
			TEncoding.UTF8.GetString(OriginalBytes),
			TEncoding.UTF8.GetString(CopiedBytes),
			'Content should be unchanged'
		);
	finally
		SourceStream.Free;
		DestStream.Free;
	end;
end;

procedure TNullCipherTest.TestCryptFileNameReturnsUnchanged;
var
	Cipher: ICipher;
begin
	Cipher := TNullCipher.Create;
	Assert.AreEqual('testfile.txt', Cipher.CryptFileName('testfile.txt'));
end;

procedure TNullCipherTest.TestDecryptFileNameReturnsUnchanged;
var
	Cipher: ICipher;
begin
	Cipher := TNullCipher.Create;
	Assert.AreEqual('testfile.txt', Cipher.DecryptFileName('testfile.txt'));
end;

procedure TNullCipherTest.TestDecryptDirListingSetsVisibleName;
var
	Cipher: ICipher;
	DirListing: TCloudDirItemList;
begin
	SetLength(DirListing, 3);
	DirListing[0].name := 'file1.txt';
	DirListing[0].visible_name := '';
	DirListing[1].name := 'file2.doc';
	DirListing[1].visible_name := '';
	DirListing[2].name := 'folder';
	DirListing[2].visible_name := '';

	Cipher := TNullCipher.Create;
	Cipher.DecryptDirListing(DirListing);

	Assert.AreEqual('file1.txt', DirListing[0].visible_name);
	Assert.AreEqual('file2.doc', DirListing[1].visible_name);
	Assert.AreEqual('folder', DirListing[2].visible_name);
end;

procedure TNullCipherTest.TestCryptFileNonExistentSource;
var
	Cipher: ICipher;
begin
	Cipher := TNullCipher.Create;
	Assert.AreEqual(CIPHER_IO_ERROR, Cipher.CryptFile('C:\NonExistent\File.txt', TPath.GetTempFileName));
end;

procedure TNullCipherTest.TestDecryptFileNonExistentSource;
var
	Cipher: ICipher;
begin
	Cipher := TNullCipher.Create;
	Assert.AreEqual(CIPHER_IO_ERROR, Cipher.DecryptFile('C:\NonExistent\File.txt', TPath.GetTempFileName));
end;

procedure TNullCipherTest.TestCryptStreamEmptyStream;
var
	Cipher: ICipher;
	SourceStream, DestStream: TMemoryStream;
	BytesCopied: Integer;
begin
	SourceStream := TMemoryStream.Create;
	DestStream := TMemoryStream.Create;
	try
		Cipher := TNullCipher.Create;
		BytesCopied := Cipher.CryptStream(SourceStream, DestStream);

		Assert.AreEqual(0, BytesCopied, 'Empty stream should copy 0 bytes');
		Assert.AreEqual(Int64(0), DestStream.Size, 'Destination should be empty');
	finally
		SourceStream.Free;
		DestStream.Free;
	end;
end;

procedure TNullCipherTest.TestCryptFileNameExtractsFromPath;
var
	Cipher: ICipher;
begin
	Cipher := TNullCipher.Create;
	Assert.AreEqual('testfile.txt', Cipher.CryptFileName('C:\Some\Path\testfile.txt'));
	Assert.AreEqual('document.pdf', Cipher.CryptFileName('D:\Documents\document.pdf'));
end;

initialization

TDUnitX.RegisterTestFixture(TNullCipherTest);

end.

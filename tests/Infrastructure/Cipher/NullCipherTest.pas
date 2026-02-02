unit NullCipherTest;

interface

uses
	Cipher,
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
		{Verifies CryptFile returns CIPHER_IO_ERROR for non-existent source}
		procedure TestCryptFileNonExistentSource;

		[Test]
		{Verifies DecryptFile returns CIPHER_IO_ERROR for non-existent source}
		procedure TestDecryptFileNonExistentSource;

		[Test]
		{Verifies CryptStream handles empty stream}
		procedure TestCryptStreamEmptyStream;

		[Test]
		{Verifies TPassThroughStream.Write delegates to source stream}
		procedure TestPassThroughStreamWrite;
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

procedure TNullCipherTest.TestPassThroughStreamWrite;
var
	Source: TMemoryStream;
	Wrapper: TPassThroughStream;
	Buffer: TBytes;
	Written: Integer;
begin
	Source := TMemoryStream.Create;
	try
		Wrapper := TPassThroughStream.Create(Source);
		try
			Buffer := TEncoding.UTF8.GetBytes('write test');
			Written := Wrapper.Write(Buffer[0], Length(Buffer));
			{ Write delegates to source TMemoryStream, which accepts writes }
			Assert.AreEqual(Integer(Length(Buffer)), Written);
			Assert.AreEqual(Int64(Length(Buffer)), Source.Size);
		finally
			Wrapper.Free;
		end;
	finally
		Source.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TNullCipherTest);

end.

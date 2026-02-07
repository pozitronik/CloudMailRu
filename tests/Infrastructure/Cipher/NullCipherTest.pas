unit NullCipherTest;

{Tests for TNullCipher and TPassThroughStream.
	Covers all pass-through operations: stream wrapping, file copying, error handling.}

interface

uses
	Windows,
	Cipher,
	System.SysUtils,
	System.IOUtils,
	System.Classes,
	DUnitX.TestFramework;

type
	[TestFixture]
	TNullCipherTest = class
	public
		{Interface tests}
		[Test]
		procedure TestImplementsICipher;
		[Test]
		procedure TestImplementsIFileCipher;

		{TPassThroughStream tests}
		[Test]
		procedure TestPassThroughStreamRead;
		[Test]
		procedure TestPassThroughStreamWrite;
		[Test]
		procedure TestPassThroughStreamGetSize;
		[Test]
		procedure TestPassThroughStreamSeek;

		{GetEncryptingStream / GetDecryptingStream}
		[Test]
		procedure TestGetEncryptingStreamReturnsPassThrough;
		[Test]
		procedure TestGetDecryptingStreamReturnsPassThrough;

		{CryptStream / DecryptStream}
		[Test]
		procedure TestCryptStreamCopiesData;
		[Test]
		procedure TestCryptStreamEmptySourceReturnsZero;
		[Test]
		procedure TestDecryptStreamCopiesData;

		{CryptFile / DecryptFile}
		[Test]
		procedure TestCryptFileCopiesContent;
		[Test]
		procedure TestCryptFileNonexistentSourceReturnsIOError;
		[Test]
		procedure TestDecryptFileCopiesContent;
	end;

implementation

{Interface tests}

procedure TNullCipherTest.TestImplementsICipher;
var
	Cipher: ICipher;
begin
	Cipher := TNullCipher.Create;
	Assert.IsNotNull(Cipher);
end;

procedure TNullCipherTest.TestImplementsIFileCipher;
var
	FileCipher: IFileCipher;
begin
	FileCipher := TNullCipher.Create;
	Assert.IsNotNull(FileCipher);
end;

{TPassThroughStream tests}

procedure TNullCipherTest.TestPassThroughStreamRead;
var
	Source: TMemoryStream;
	Wrapper: TPassThroughStream;
	Original, ReadBack: TBytes;
	BytesRead: Integer;
begin
	Source := TMemoryStream.Create;
	try
		Original := TEncoding.UTF8.GetBytes('read test data');
		Source.WriteBuffer(Original[0], Length(Original));
		Source.Position := 0;

		Wrapper := TPassThroughStream.Create(Source);
		try
			SetLength(ReadBack, Length(Original));
			BytesRead := Wrapper.Read(ReadBack[0], Length(ReadBack));

			Assert.AreEqual(Integer(Length(Original)), BytesRead, 'Should read all bytes');
			Assert.AreEqual(
				TEncoding.UTF8.GetString(Original),
				TEncoding.UTF8.GetString(ReadBack),
				'Read data should match source'
			);
		finally
			Wrapper.Free;
		end;
	finally
		Source.Free;
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

procedure TNullCipherTest.TestPassThroughStreamGetSize;
var
	Source: TMemoryStream;
	Wrapper: TPassThroughStream;
	Data: TBytes;
begin
	Source := TMemoryStream.Create;
	try
		Data := TEncoding.UTF8.GetBytes('size test content');
		Source.WriteBuffer(Data[0], Length(Data));

		Wrapper := TPassThroughStream.Create(Source);
		try
			Assert.AreEqual(Source.Size, Wrapper.Size, 'Wrapper size should match source size');
		finally
			Wrapper.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TNullCipherTest.TestPassThroughStreamSeek;
var
	Source: TMemoryStream;
	Wrapper: TPassThroughStream;
	Data: TBytes;
	NewPos: Int64;
begin
	Source := TMemoryStream.Create;
	try
		Data := TEncoding.UTF8.GetBytes('seek test content here');
		Source.WriteBuffer(Data[0], Length(Data));
		Source.Position := 0;

		Wrapper := TPassThroughStream.Create(Source);
		try
			{Seek from beginning}
			NewPos := Wrapper.Seek(Int64(5), soBeginning);
			Assert.AreEqual(Int64(5), NewPos, 'Seek from beginning');
			Assert.AreEqual(Int64(5), Source.Position, 'Source position should follow');

			{Seek from current}
			NewPos := Wrapper.Seek(Int64(3), soCurrent);
			Assert.AreEqual(Int64(8), NewPos, 'Seek from current');

			{Seek from end}
			NewPos := Wrapper.Seek(Int64(0), soEnd);
			Assert.AreEqual(Source.Size, NewPos, 'Seek to end');
		finally
			Wrapper.Free;
		end;
	finally
		Source.Free;
	end;
end;

{GetEncryptingStream / GetDecryptingStream}

procedure TNullCipherTest.TestGetEncryptingStreamReturnsPassThrough;
var
	NullCipher: ICipher;
	Source: TMemoryStream;
	Wrapper: TStream;
	Data, ReadBack: TBytes;
	BytesRead: Integer;
begin
	NullCipher := TNullCipher.Create;
	Source := TMemoryStream.Create;
	try
		Data := TEncoding.UTF8.GetBytes('encrypting pass-through');
		Source.WriteBuffer(Data[0], Length(Data));
		Source.Position := 0;

		Wrapper := NullCipher.GetEncryptingStream(Source);
		try
			SetLength(ReadBack, Length(Data));
			BytesRead := Wrapper.Read(ReadBack[0], Length(ReadBack));

			Assert.AreEqual(Integer(Length(Data)), BytesRead);
			Assert.AreEqual(
				TEncoding.UTF8.GetString(Data),
				TEncoding.UTF8.GetString(ReadBack),
				'Encrypting stream should pass data through unchanged'
			);
		finally
			Wrapper.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TNullCipherTest.TestGetDecryptingStreamReturnsPassThrough;
var
	NullCipher: ICipher;
	Source: TMemoryStream;
	Wrapper: TStream;
	Data, ReadBack: TBytes;
	BytesRead: Integer;
begin
	NullCipher := TNullCipher.Create;
	Source := TMemoryStream.Create;
	try
		Data := TEncoding.UTF8.GetBytes('decrypting pass-through');
		Source.WriteBuffer(Data[0], Length(Data));
		Source.Position := 0;

		Wrapper := NullCipher.GetDecryptingStream(Source);
		try
			SetLength(ReadBack, Length(Data));
			BytesRead := Wrapper.Read(ReadBack[0], Length(ReadBack));

			Assert.AreEqual(Integer(Length(Data)), BytesRead);
			Assert.AreEqual(
				TEncoding.UTF8.GetString(Data),
				TEncoding.UTF8.GetString(ReadBack),
				'Decrypting stream should pass data through unchanged'
			);
		finally
			Wrapper.Free;
		end;
	finally
		Source.Free;
	end;
end;

{CryptStream / DecryptStream}

procedure TNullCipherTest.TestCryptStreamCopiesData;
var
	FileCipher: IFileCipher;
	Source, Dest: TMemoryStream;
	Data, ReadBack: TBytes;
	BytesCopied: Integer;
begin
	FileCipher := TNullCipher.Create;
	Source := TMemoryStream.Create;
	Dest := TMemoryStream.Create;
	try
		Data := TEncoding.UTF8.GetBytes('stream copy content');
		Source.WriteBuffer(Data[0], Length(Data));

		BytesCopied := FileCipher.CryptStream(Source, Dest);

		Assert.AreEqual(Integer(Length(Data)), BytesCopied, 'Should copy all bytes');

		Dest.Position := 0;
		SetLength(ReadBack, Dest.Size);
		Dest.ReadBuffer(ReadBack[0], Dest.Size);
		Assert.AreEqual(
			TEncoding.UTF8.GetString(Data),
			TEncoding.UTF8.GetString(ReadBack),
			'Copied data should match original'
		);
	finally
		Source.Free;
		Dest.Free;
	end;
end;

procedure TNullCipherTest.TestCryptStreamEmptySourceReturnsZero;
var
	FileCipher: IFileCipher;
	Source, Dest: TMemoryStream;
	BytesCopied: Integer;
begin
	FileCipher := TNullCipher.Create;
	Source := TMemoryStream.Create;
	Dest := TMemoryStream.Create;
	try
		BytesCopied := FileCipher.CryptStream(Source, Dest);

		Assert.AreEqual(0, BytesCopied, 'Empty source should return 0');
		Assert.AreEqual(Int64(0), Dest.Size, 'Destination should remain empty');
	finally
		Source.Free;
		Dest.Free;
	end;
end;

procedure TNullCipherTest.TestDecryptStreamCopiesData;
var
	FileCipher: IFileCipher;
	Source, Dest: TMemoryStream;
	Data, ReadBack: TBytes;
	BytesCopied: Integer;
begin
	FileCipher := TNullCipher.Create;
	Source := TMemoryStream.Create;
	Dest := TMemoryStream.Create;
	try
		Data := TEncoding.UTF8.GetBytes('decrypt stream copy');
		Source.WriteBuffer(Data[0], Length(Data));

		BytesCopied := FileCipher.DecryptStream(Source, Dest);

		Assert.AreEqual(Integer(Length(Data)), BytesCopied, 'Should copy all bytes');

		Dest.Position := 0;
		SetLength(ReadBack, Dest.Size);
		Dest.ReadBuffer(ReadBack[0], Dest.Size);
		Assert.AreEqual(
			TEncoding.UTF8.GetString(Data),
			TEncoding.UTF8.GetString(ReadBack),
			'Decrypted data should match original'
		);
	finally
		Source.Free;
		Dest.Free;
	end;
end;

{CryptFile / DecryptFile}

procedure TNullCipherTest.TestCryptFileCopiesContent;
var
	FileCipher: IFileCipher;
	TempDir, SourceFile, DestFile: string;
	OriginalContent, CopiedContent: string;
begin
	FileCipher := TNullCipher.Create;
	TempDir := TPath.GetTempPath;
	SourceFile := TPath.Combine(TempDir, 'NullCipherTest_src_' + IntToStr(GetTickCount) + '.tmp');
	DestFile := TPath.Combine(TempDir, 'NullCipherTest_dst_' + IntToStr(GetTickCount) + '.tmp');
	try
		OriginalContent := 'File copy test content for null cipher';
		TFile.WriteAllText(SourceFile, OriginalContent);

		Assert.AreEqual(CIPHER_OK, FileCipher.CryptFile(SourceFile, DestFile));
		Assert.IsTrue(TFile.Exists(DestFile), 'Destination file should exist');

		CopiedContent := TFile.ReadAllText(DestFile);
		Assert.AreEqual(OriginalContent, CopiedContent, 'File content should be copied unchanged');
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(DestFile) then TFile.Delete(DestFile);
	end;
end;

procedure TNullCipherTest.TestCryptFileNonexistentSourceReturnsIOError;
var
	FileCipher: IFileCipher;
begin
	FileCipher := TNullCipher.Create;
	Assert.AreEqual(CIPHER_IO_ERROR, FileCipher.CryptFile('C:\nonexistent_path\no_file.tmp', 'C:\output.tmp'));
end;

procedure TNullCipherTest.TestDecryptFileCopiesContent;
var
	FileCipher: IFileCipher;
	TempDir, SourceFile, DestFile: string;
	OriginalContent, CopiedContent: string;
begin
	FileCipher := TNullCipher.Create;
	TempDir := TPath.GetTempPath;
	SourceFile := TPath.Combine(TempDir, 'NullCipherDecrypt_src_' + IntToStr(GetTickCount) + '.tmp');
	DestFile := TPath.Combine(TempDir, 'NullCipherDecrypt_dst_' + IntToStr(GetTickCount) + '.tmp');
	try
		OriginalContent := 'Decrypt file test - should be identical copy';
		TFile.WriteAllText(SourceFile, OriginalContent);

		Assert.AreEqual(CIPHER_OK, FileCipher.DecryptFile(SourceFile, DestFile));

		CopiedContent := TFile.ReadAllText(DestFile);
		Assert.AreEqual(OriginalContent, CopiedContent, 'Decrypted file should match original');
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(DestFile) then TFile.Delete(DestFile);
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TNullCipherTest);

end.

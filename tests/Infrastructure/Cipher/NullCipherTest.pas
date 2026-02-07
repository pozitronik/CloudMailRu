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

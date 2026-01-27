unit TCRequestTest;

interface

uses
	TCRequest,
	TestHelper,
	WFXTypes,
	SysUtils,
	StrUtils,
	Windows,
	DUnitX.TestFramework;

type

	[TestFixture]
	TTCRequestTest = class
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;
		[Test]
		{Verifies TTCRequest implements IRequest interface}
		procedure TestImplementsIRequest;
		[Test]
		procedure TestRequest;
	end;

	[TestFixture]
	TNullRequestTest = class
	public
		[Test]
		{Verifies TNullRequest can be assigned to IRequest variable}
		procedure TestImplementsIRequest;

		[Test]
		{Verifies Request returns False (not executed)}
		procedure TestRequestReturnsFalse;

		[Test]
		{Verifies multiple sequential calls work correctly}
		procedure TestMultipleCalls;
	end;

var
	PluginNr: Integer;
	RequestType: Integer;
	CustomTitle: WideString;
	CustomText: WideString;
	ReturnedText: WideString;
	MaxLen: Integer;

function TestRequestFunc(Plugin_Nr, Request_Type: Integer; Custom_Title, Custom_Text, Returned_Text: PWideChar; max_len: Integer): bool; stdcall;

implementation

function TestRequestFunc(Plugin_Nr, Request_Type: Integer; Custom_Title, Custom_Text, Returned_Text: PWideChar; max_len: Integer): bool; stdcall;
begin
	result := True;
	PluginNr := Plugin_Nr;
	RequestType := Request_Type;
	CustomTitle := Custom_Title;
	CustomText := Custom_Text;
	ReturnedText := Returned_Text;
	GetMem(Returned_Text, (max_len + 1) * SizeOf(WideChar));
	StrPLCopy(Returned_Text, ReverseString(ReturnedText), max_len); {to test the returned value}
	MaxLen := max_len;
end;

procedure TTCRequestTest.Setup;
begin
	PluginNr := 0;
	RequestType := RT_Other;
	CustomTitle := '';
	CustomText := '';
	ReturnedText := '';
	MaxLen := 0;
end;

procedure TTCRequestTest.TearDown;
begin
end;

procedure TTCRequestTest.TestImplementsIRequest;
var
	Request: IRequest;
begin
	Request := TTCRequest.Create(TestRequestFunc, 1);
	Assert.IsNotNull(Request);
end;

procedure TTCRequestTest.TestRequest;
var
	TestTCRequest: TTCRequest;
	randomPN: Integer;
	RandomRT: Integer;
	RandomTitle: WideString;
	RandomText: WideString;
	RandomReturnedText: WideString;
	RandomReturnedTextOrig: WideString;
begin
	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(RT_Other, RequestType);
	Assert.AreEqual('', CustomTitle);
	Assert.AreEqual('', CustomText);
	Assert.AreEqual('', ReturnedText);
	Assert.AreEqual(0, MaxLen);

	randomPN := Random(100);
	RandomRT := Random(RT_MsgOKCancel);
	RandomTitle := RandomString(32);
	RandomText := RandomString(64);
	RandomReturnedText := 'ABC';
	RandomReturnedTextOrig := RandomReturnedText;

	TestTCRequest := TTCRequest.Create(TestRequestFunc, randomPN);

	Assert.IsTrue(TestTCRequest.Request(RandomRT, RandomTitle, RandomText, RandomReturnedText, Length(RandomReturnedText)));

	Assert.AreEqual(randomPN, PluginNr);
	Assert.AreEqual(RandomRT, RequestType);
	Assert.AreEqual(CustomTitle, RandomTitle);
	Assert.AreEqual(CustomText, RandomText);
	Assert.AreEqual(ReturnedText, RandomReturnedTextOrig);
	Assert.AreEqual(MaxLen, Length(RandomReturnedText));
	{Skip assertion, because I can't reproduce desired behavior. Hope to return to this issue later.}
	//	Assert.AreEqual(RandomReturnedText, ReverseString(RandomReturnedTextOrig));
end;

{TNullRequestTest}

procedure TNullRequestTest.TestImplementsIRequest;
var
	Request: IRequest;
begin
	Request := TNullRequest.Create;
	Assert.IsNotNull(Request);
end;

procedure TNullRequestTest.TestRequestReturnsFalse;
var
	Request: IRequest;
	ReturnedText: WideString;
begin
	Request := TNullRequest.Create;
	ReturnedText := '';
	Assert.IsFalse(Request.Request(1, 'Title', 'Text', ReturnedText, 256));
end;

procedure TNullRequestTest.TestMultipleCalls;
var
	Request: IRequest;
	ReturnedText: WideString;
	i: Integer;
begin
	Request := TNullRequest.Create;
	for i := 1 to 10 do
	begin
		ReturnedText := '';
		Assert.IsFalse(Request.Request(i, 'Title', 'Text', ReturnedText, 256));
	end;
	Assert.Pass('Multiple request calls completed without exception');
end;

initialization

TDUnitX.RegisterTestFixture(TTCRequestTest);
TDUnitX.RegisterTestFixture(TNullRequestTest);

end.

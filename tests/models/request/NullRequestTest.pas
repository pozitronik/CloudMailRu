unit NullRequestTest;

interface

uses
	IRequestInterface,
	DUnitX.TestFramework;

type
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

implementation

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

TDUnitX.RegisterTestFixture(TNullRequestTest);

end.

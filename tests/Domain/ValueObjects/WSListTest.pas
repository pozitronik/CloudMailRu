unit WSListTest;

interface

uses
	WSList,
	Classes,
	SysUtils,
	DUnitX.TestFramework;

type

	[TestFixture]
	TWSListTest = class
	public
		[Test]
		procedure TestAddSingle;
		[Test]
		procedure TestAddMultiple;
		[Test]
		procedure TestClear;
		[Test]
		procedure TestCountEmpty;
		[Test]
		procedure TestCountAfterAdd;
		[Test]
		procedure TestContainsFound;
		[Test]
		procedure TestContainsNotFound;
		[Test]
		procedure TestContainsEmpty;
		[Test]
		procedure TestContainsCaseSensitive;
		[Test]
		procedure TestContainsFirstElement;
		[Test]
		procedure TestContainsLastElement;
		[Test]
		procedure TestClearThenAdd;
		[Test]
		procedure TestAddEmptyString;
		[Test]
		procedure TestContainsEmptyString;
	end;

implementation

{ TWSListTest }

procedure TWSListTest.TestAddSingle;
var
	List: TWSList;
begin
	List.Add('item1');

	Assert.AreEqual(Integer(1), Integer(List.Count));
	Assert.AreEqual('item1', List[0]);
end;

procedure TWSListTest.TestAddMultiple;
var
	List: TWSList;
begin
	List.Add('first');
	List.Add('second');
	List.Add('third');

	Assert.AreEqual(Integer(3), Integer(List.Count));
	Assert.AreEqual('first', List[0]);
	Assert.AreEqual('second', List[1]);
	Assert.AreEqual('third', List[2]);
end;

procedure TWSListTest.TestClear;
var
	List: TWSList;
begin
	List.Add('item1');
	List.Add('item2');
	List.Clear();

	Assert.AreEqual(Integer(0), Integer(List.Count));
end;

procedure TWSListTest.TestCountEmpty;
var
	List: TWSList;
begin
	Assert.AreEqual(Integer(0), Integer(List.Count));
end;

procedure TWSListTest.TestCountAfterAdd;
var
	List: TWSList;
begin
	Assert.AreEqual(Integer(0), Integer(List.Count));

	List.Add('a');
	Assert.AreEqual(Integer(1), Integer(List.Count));

	List.Add('b');
	Assert.AreEqual(Integer(2), Integer(List.Count));
end;

procedure TWSListTest.TestContainsFound;
var
	List: TWSList;
begin
	List.Add('apple');
	List.Add('banana');
	List.Add('cherry');

	Assert.IsTrue(List.Contains('banana'));
end;

procedure TWSListTest.TestContainsNotFound;
var
	List: TWSList;
begin
	List.Add('apple');
	List.Add('banana');

	Assert.IsFalse(List.Contains('orange'));
end;

procedure TWSListTest.TestContainsEmpty;
var
	List: TWSList;
begin
	Assert.IsFalse(List.Contains('anything'));
end;

procedure TWSListTest.TestContainsCaseSensitive;
var
	List: TWSList;
begin
	List.Add('Apple');

	Assert.IsTrue(List.Contains('Apple'));
	Assert.IsFalse(List.Contains('apple'));
	Assert.IsFalse(List.Contains('APPLE'));
end;

procedure TWSListTest.TestContainsFirstElement;
var
	List: TWSList;
begin
	List.Add('first');
	List.Add('second');
	List.Add('third');

	Assert.IsTrue(List.Contains('first'));
end;

procedure TWSListTest.TestContainsLastElement;
var
	List: TWSList;
begin
	List.Add('first');
	List.Add('second');
	List.Add('last');

	Assert.IsTrue(List.Contains('last'));
end;

procedure TWSListTest.TestClearThenAdd;
var
	List: TWSList;
begin
	List.Add('item1');
	List.Clear();
	List.Add('new_item');

	Assert.AreEqual(Integer(1), Integer(List.Count));
	Assert.AreEqual('new_item', List[0]);
end;

procedure TWSListTest.TestAddEmptyString;
var
	List: TWSList;
begin
	List.Add('');

	Assert.AreEqual(Integer(1), Integer(List.Count));
	Assert.AreEqual('', List[0]);
end;

procedure TWSListTest.TestContainsEmptyString;
var
	List: TWSList;
begin
	List.Add('item');
	List.Add('');
	List.Add('another');

	Assert.IsTrue(List.Contains(''));
end;

initialization

TDUnitX.RegisterTestFixture(TWSListTest);

end.

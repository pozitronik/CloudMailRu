unit SafeJSONTest;

interface

uses
	SafeJSON,
	JSON,
	DUnitX.TestFramework;

type

	[TestFixture]
	TSafeJSONTest = class
	private
		const
			{Basic object}
			JSON_SIMPLE = '{"name":"test","size":12345,"count":42,"active":true}';
			{Nested objects}
			JSON_NESTED = '{"body":{"user":{"name":"John","age":30},"count":{"files":5,"folders":3}}}';
			{Array}
			JSON_ARRAY = '{"items":[{"id":1,"name":"first"},{"id":2,"name":"second"},{"id":3,"name":"third"}]}';
			{Null values}
			JSON_NULL_VALUES = '{"name":null,"size":null,"active":null}';
			{Mixed types}
			JSON_MIXED = '{"string":"hello","number":123,"float":3.14,"bool":true,"null":null,"array":[1,2,3]}';
			{Empty structures}
			JSON_EMPTY_OBJECT = '{}';
			JSON_EMPTY_ARRAY = '{"items":[]}';
			{Numeric edge cases}
			JSON_LARGE_INT64 = '{"size":9223372036854775807}';
			JSON_NEGATIVE = '{"value":-12345}';
			JSON_ZERO = '{"value":0}';
			{Boolean variants}
			JSON_BOOLEANS = '{"yes":true,"no":false}';
			{Deep nesting}
			JSON_DEEP = '{"level1":{"level2":{"level3":{"value":"deep"}}}}';
			{Unicode (actual UTF-8 encoded)}
			JSON_UNICODE_UTF8 = '{"name":"Привет"}';
	public
		{Parse tests}
		[Test]
		procedure TestParse_ValidJSON_ReturnsNonNull;
		[Test]
		procedure TestParse_InvalidJSON_ReturnsNull;
		[Test]
		procedure TestParse_EmptyString_ReturnsNull;

		{Get tests}
		[Test]
		procedure TestGet_ExistingKey_ReturnsValue;
		[Test]
		procedure TestGet_MissingKey_ReturnsNull;
		[Test]
		procedure TestGet_OnNull_ReturnsNull;
		[Test]
		procedure TestGet_Chained_ReturnsNestedValue;
		[Test]
		procedure TestGet_ChainedMissing_ReturnsNull;
		[Test]
		procedure TestGet_DeepNesting_Works;

		{AsString tests}
		[Test]
		procedure TestAsString_ValidString_ReturnsValue;
		[Test]
		procedure TestAsString_NullValue_ReturnsDefault;
		[Test]
		procedure TestAsString_MissingKey_ReturnsDefault;
		[Test]
		procedure TestAsString_EmptyString_ReturnsEmpty;
		[Test]
		procedure TestAsString_Number_ReturnsStringRepresentation;
		[Test]
		procedure TestAsString_CustomDefault_ReturnsDefault;
		[Test]
		procedure TestAsString_UnicodeUTF8_ReturnsCorrectly;

		{AsInt tests}
		[Test]
		procedure TestAsInt_ValidNumber_ReturnsValue;
		[Test]
		procedure TestAsInt_NullValue_ReturnsDefault;
		[Test]
		procedure TestAsInt_MissingKey_ReturnsDefault;
		[Test]
		procedure TestAsInt_Zero_ReturnsZero;
		[Test]
		procedure TestAsInt_Negative_ReturnsNegative;
		[Test]
		procedure TestAsInt_InvalidString_ReturnsDefault;
		[Test]
		procedure TestAsInt_CustomDefault_ReturnsDefault;

		{AsInt64 tests}
		[Test]
		procedure TestAsInt64_ValidNumber_ReturnsValue;
		[Test]
		procedure TestAsInt64_LargeValue_ReturnsCorrectly;
		[Test]
		procedure TestAsInt64_NullValue_ReturnsDefault;

		{AsBool tests}
		[Test]
		procedure TestAsBool_True_ReturnsTrue;
		[Test]
		procedure TestAsBool_False_ReturnsFalse;
		[Test]
		procedure TestAsBool_NullValue_ReturnsDefault;
		[Test]
		procedure TestAsBool_MissingKey_ReturnsDefault;
		[Test]
		procedure TestAsBool_CustomDefault_ReturnsDefault;

		{Array tests}
		[Test]
		procedure TestItem_ValidIndex_ReturnsElement;
		[Test]
		procedure TestItem_OutOfBounds_ReturnsNull;
		[Test]
		procedure TestItem_NegativeIndex_ReturnsNull;
		[Test]
		procedure TestItem_OnNonArray_ReturnsNull;
		[Test]
		procedure TestCount_Array_ReturnsLength;
		[Test]
		procedure TestCount_EmptyArray_ReturnsZero;
		[Test]
		procedure TestCount_NonArray_ReturnsZero;

		{Type checking tests}
		[Test]
		procedure TestIsNull_OnNull_ReturnsTrue;
		[Test]
		procedure TestIsNull_OnValue_ReturnsFalse;
		[Test]
		procedure TestIsNull_OnNullJsonValue_ReturnsTrue;
		[Test]
		procedure TestIsObject_OnObject_ReturnsTrue;
		[Test]
		procedure TestIsObject_OnArray_ReturnsFalse;
		[Test]
		procedure TestIsArray_OnArray_ReturnsTrue;
		[Test]
		procedure TestIsArray_OnObject_ReturnsFalse;

		{Memory management tests}
		[Test]
		procedure TestFree_OwnedValue_FreesMemory;
		[Test]
		procedure TestFree_MultipleCalls_Safe;

		{Wrap tests}
		[Test]
		procedure TestWrap_NilValue_ReturnsNull;
		[Test]
		procedure TestWrap_ValidValue_Works;

		{Raw accessor test}
		[Test]
		procedure TestRaw_ReturnsUnderlyingValue;
	end;

implementation

{Parse tests}

procedure TSafeJSONTest.TestParse_ValidJSON_ReturnsNonNull;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.IsFalse(JSON.IsNull);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestParse_InvalidJSON_ReturnsNull;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse('not valid json');
	try
		Assert.IsTrue(JSON.IsNull);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestParse_EmptyString_ReturnsNull;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse('');
	try
		Assert.IsTrue(JSON.IsNull);
	finally
		JSON.Free;
	end;
end;

{Get tests}

procedure TSafeJSONTest.TestGet_ExistingKey_ReturnsValue;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.IsFalse(JSON.Get('name').IsNull);
		Assert.AreEqual('test', JSON.Get('name').AsString);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestGet_MissingKey_ReturnsNull;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.IsTrue(JSON.Get('nonexistent').IsNull);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestGet_OnNull_ReturnsNull;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Null;
	Assert.IsTrue(JSON.Get('anything').IsNull);
end;

procedure TSafeJSONTest.TestGet_Chained_ReturnsNestedValue;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_NESTED);
	try
		Assert.AreEqual('John', JSON.Get('body').Get('user').Get('name').AsString);
		Assert.AreEqual(30, JSON.Get('body').Get('user').Get('age').AsInt);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestGet_ChainedMissing_ReturnsNull;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_NESTED);
	try
		{Missing intermediate key - should safely return null}
		Assert.IsTrue(JSON.Get('body').Get('missing').Get('value').IsNull);
		Assert.AreEqual('default', JSON.Get('body').Get('missing').Get('value').AsString('default'));
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestGet_DeepNesting_Works;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_DEEP);
	try
		Assert.AreEqual('deep', JSON.Get('level1').Get('level2').Get('level3').Get('value').AsString);
	finally
		JSON.Free;
	end;
end;

{AsString tests}

procedure TSafeJSONTest.TestAsString_ValidString_ReturnsValue;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.AreEqual('test', JSON.Get('name').AsString);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsString_NullValue_ReturnsDefault;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_NULL_VALUES);
	try
		{Null JSON value should return empty string (default)}
		Assert.AreEqual('', JSON.Get('name').AsString);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsString_MissingKey_ReturnsDefault;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.AreEqual('', JSON.Get('missing').AsString);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsString_EmptyString_ReturnsEmpty;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse('{"empty":""}');
	try
		Assert.AreEqual('', JSON.Get('empty').AsString);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsString_Number_ReturnsStringRepresentation;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		{Numbers can be read as strings}
		Assert.AreEqual('12345', JSON.Get('size').AsString);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsString_CustomDefault_ReturnsDefault;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.AreEqual('custom', JSON.Get('missing').AsString('custom'));
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsString_UnicodeUTF8_ReturnsCorrectly;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_UNICODE_UTF8);
	try
		{UTF-8 encoded Cyrillic should parse correctly}
		Assert.AreEqual(WideString('Привет'), JSON.Get('name').AsString);
	finally
		JSON.Free;
	end;
end;

{AsInt tests}

procedure TSafeJSONTest.TestAsInt_ValidNumber_ReturnsValue;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.AreEqual(42, JSON.Get('count').AsInt);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsInt_NullValue_ReturnsDefault;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_NULL_VALUES);
	try
		Assert.AreEqual(0, JSON.Get('size').AsInt);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsInt_MissingKey_ReturnsDefault;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.AreEqual(0, JSON.Get('missing').AsInt);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsInt_Zero_ReturnsZero;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_ZERO);
	try
		Assert.AreEqual(0, JSON.Get('value').AsInt);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsInt_Negative_ReturnsNegative;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_NEGATIVE);
	try
		Assert.AreEqual(-12345, JSON.Get('value').AsInt);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsInt_InvalidString_ReturnsDefault;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse('{"value":"not a number"}');
	try
		Assert.AreEqual(0, JSON.Get('value').AsInt);
		Assert.AreEqual(999, JSON.Get('value').AsInt(999));
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsInt_CustomDefault_ReturnsDefault;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.AreEqual(999, JSON.Get('missing').AsInt(999));
	finally
		JSON.Free;
	end;
end;

{AsInt64 tests}

procedure TSafeJSONTest.TestAsInt64_ValidNumber_ReturnsValue;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.AreEqual(Int64(12345), JSON.Get('size').AsInt64);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsInt64_LargeValue_ReturnsCorrectly;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_LARGE_INT64);
	try
		Assert.AreEqual(Int64(9223372036854775807), JSON.Get('size').AsInt64);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsInt64_NullValue_ReturnsDefault;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_NULL_VALUES);
	try
		Assert.AreEqual(Int64(0), JSON.Get('size').AsInt64);
	finally
		JSON.Free;
	end;
end;

{AsBool tests}

procedure TSafeJSONTest.TestAsBool_True_ReturnsTrue;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_BOOLEANS);
	try
		Assert.IsTrue(JSON.Get('yes').AsBool);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsBool_False_ReturnsFalse;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_BOOLEANS);
	try
		Assert.IsFalse(JSON.Get('no').AsBool);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsBool_NullValue_ReturnsDefault;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_NULL_VALUES);
	try
		Assert.IsFalse(JSON.Get('active').AsBool);
		Assert.IsTrue(JSON.Get('active').AsBool(True));
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsBool_MissingKey_ReturnsDefault;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.IsFalse(JSON.Get('missing').AsBool);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestAsBool_CustomDefault_ReturnsDefault;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.IsTrue(JSON.Get('missing').AsBool(True));
	finally
		JSON.Free;
	end;
end;

{Array tests}

procedure TSafeJSONTest.TestItem_ValidIndex_ReturnsElement;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_ARRAY);
	try
		Assert.AreEqual(1, JSON.Get('items').Item(0).Get('id').AsInt);
		Assert.AreEqual('first', JSON.Get('items').Item(0).Get('name').AsString);
		Assert.AreEqual('third', JSON.Get('items').Item(2).Get('name').AsString);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestItem_OutOfBounds_ReturnsNull;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_ARRAY);
	try
		Assert.IsTrue(JSON.Get('items').Item(100).IsNull);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestItem_NegativeIndex_ReturnsNull;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_ARRAY);
	try
		Assert.IsTrue(JSON.Get('items').Item(-1).IsNull);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestItem_OnNonArray_ReturnsNull;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		{Calling Item on an object should return null}
		Assert.IsTrue(JSON.Item(0).IsNull);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestCount_Array_ReturnsLength;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_ARRAY);
	try
		Assert.AreEqual(3, JSON.Get('items').Count);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestCount_EmptyArray_ReturnsZero;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_EMPTY_ARRAY);
	try
		Assert.AreEqual(0, JSON.Get('items').Count);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestCount_NonArray_ReturnsZero;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		{Count on non-array should return 0}
		Assert.AreEqual(0, JSON.Count);
		Assert.AreEqual(0, JSON.Get('name').Count);
	finally
		JSON.Free;
	end;
end;

{Type checking tests}

procedure TSafeJSONTest.TestIsNull_OnNull_ReturnsTrue;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Null;
	Assert.IsTrue(JSON.IsNull);
end;

procedure TSafeJSONTest.TestIsNull_OnValue_ReturnsFalse;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.IsFalse(JSON.IsNull);
		Assert.IsFalse(JSON.Get('name').IsNull);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestIsNull_OnNullJsonValue_ReturnsTrue;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_NULL_VALUES);
	try
		{Explicit JSON null should be detected as null}
		Assert.IsTrue(JSON.Get('name').IsNull);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestIsObject_OnObject_ReturnsTrue;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.IsTrue(JSON.IsObject);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestIsObject_OnArray_ReturnsFalse;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_ARRAY);
	try
		Assert.IsFalse(JSON.Get('items').IsObject);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestIsArray_OnArray_ReturnsTrue;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_ARRAY);
	try
		Assert.IsTrue(JSON.Get('items').IsArray);
	finally
		JSON.Free;
	end;
end;

procedure TSafeJSONTest.TestIsArray_OnObject_ReturnsFalse;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.IsFalse(JSON.IsArray);
	finally
		JSON.Free;
	end;
end;

{Memory management tests}

procedure TSafeJSONTest.TestFree_OwnedValue_FreesMemory;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	JSON.Free;
	{After free, should be null}
	Assert.IsTrue(JSON.IsNull);
end;

procedure TSafeJSONTest.TestFree_MultipleCalls_Safe;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	JSON.Free;
	JSON.Free; {Should not crash}
	JSON.Free; {Should not crash}
	Assert.IsTrue(JSON.IsNull);
end;

{Wrap tests}

procedure TSafeJSONTest.TestWrap_NilValue_ReturnsNull;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Wrap(nil);
	Assert.IsTrue(JSON.IsNull);
end;

procedure TSafeJSONTest.TestWrap_ValidValue_Works;
var
	JSONObj: TJSONObject;
	JSON: TSafeJSON;
begin
	JSONObj := TJSONObject.ParseJSONValue(JSON_SIMPLE) as TJSONObject;
	try
		JSON := TSafeJSON.Wrap(JSONObj);
		Assert.IsFalse(JSON.IsNull);
		Assert.AreEqual('test', JSON.Get('name').AsString);
	finally
		JSONObj.Free; {We own it, not TSafeJSON}
	end;
end;

{Raw accessor test}

procedure TSafeJSONTest.TestRaw_ReturnsUnderlyingValue;
var
	JSON: TSafeJSON;
begin
	JSON := TSafeJSON.Parse(JSON_SIMPLE);
	try
		Assert.IsNotNull(JSON.Raw);
		Assert.IsTrue(JSON.Raw is TJSONObject);
	finally
		JSON.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TSafeJSONTest);

end.

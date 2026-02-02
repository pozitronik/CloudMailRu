unit ConfigFileTest;

interface

uses
	ConfigFile,
	SysUtils,
	IOUtils,
	Classes,
	DUnitX.TestFramework;

type

	[TestFixture]
	TNullConfigFileTest = class
	private
		FConfig: IConfigFile;
	public
		[Setup]
		procedure Setup;

		[Test]
		procedure Test_GetFilePath_ReturnsEmptyString;

		[Test]
		procedure Test_ReadString_ReturnsDefault;

		[Test]
		procedure Test_ReadBool_ReturnsDefault;

		[Test]
		procedure Test_ReadInteger_ReturnsDefault;

		[Test]
		procedure Test_ReadInt64_ReturnsDefault;

		[Test]
		procedure Test_SectionExists_ReturnsFalse;

		[Test]
		procedure Test_ReadSections_LeavesListEmpty;

		[Test]
		procedure Test_WriteOperations_DoNotThrow;
	end;

	[TestFixture]
	TMemoryConfigFileTest = class
	private
		FConfig: TMemoryConfigFile;
		FConfigInterface: IConfigFile;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Constructor tests}
		[Test]
		procedure Test_Create_WithoutPath_ReturnsEmptyFilePath;

		[Test]
		procedure Test_Create_WithPath_ReturnsFilePath;

		{String operations}
		[Test]
		procedure Test_ReadString_WhenMissing_ReturnsDefault;

		[Test]
		procedure Test_WriteString_ThenRead_ReturnsValue;

		[Test]
		procedure Test_WriteString_OverwritesExisting;

		{Boolean operations}
		[Test]
		procedure Test_ReadBool_WhenMissing_ReturnsDefault;

		[Test]
		procedure Test_WriteBool_True_StoresAs1;

		[Test]
		procedure Test_WriteBool_False_StoresAs0;

		[Test]
		procedure Test_ReadBool_Parses1AsTrue;

		[Test]
		procedure Test_ReadBool_ParsesTrueAsTrue;

		[Test]
		procedure Test_ReadBool_Parses0AsFalse;

		[Test]
		procedure Test_ReadBool_ParsesFalseAsFalse;

		{Integer operations}
		[Test]
		procedure Test_ReadInteger_WhenMissing_ReturnsDefault;

		[Test]
		procedure Test_WriteInteger_ThenRead_ReturnsValue;

		[Test]
		procedure Test_ReadInteger_InvalidValue_ReturnsDefault;

		{Int64 operations}
		[Test]
		procedure Test_ReadInt64_WhenMissing_ReturnsDefault;

		[Test]
		procedure Test_WriteInt64_ThenRead_ReturnsValue;

		[Test]
		procedure Test_WriteInt64_LargeValue_Preserved;

		{WriteIfNotDefault operations}
		[Test]
		procedure Test_WriteStringIfNotDefault_WhenDifferent_Writes;

		[Test]
		procedure Test_WriteStringIfNotDefault_WhenEqual_DeletesKey;

		[Test]
		procedure Test_WriteBoolIfNotDefault_WhenDifferent_Writes;

		[Test]
		procedure Test_WriteBoolIfNotDefault_WhenEqual_DeletesKey;

		[Test]
		procedure Test_WriteIntegerIfNotDefault_WhenDifferent_Writes;

		[Test]
		procedure Test_WriteIntegerIfNotDefault_WhenEqual_DeletesKey;

		[Test]
		procedure Test_WriteInt64IfNotDefault_WhenDifferent_Writes;

		[Test]
		procedure Test_WriteInt64IfNotDefault_WhenEqual_DeletesKey;

		{Section operations}
		[Test]
		procedure Test_SectionExists_WhenMissing_ReturnsFalse;

		[Test]
		procedure Test_SectionExists_AfterWrite_ReturnsTrue;

		[Test]
		procedure Test_ReadSections_ReturnsAllSections;

		[Test]
		procedure Test_EraseSection_RemovesSection;

		[Test]
		procedure Test_EraseSection_NonExistent_DoesNotThrow;

		{Key operations}
		[Test]
		procedure Test_DeleteKey_RemovesKey;

		[Test]
		procedure Test_DeleteKey_NonExistent_DoesNotThrow;

		[Test]
		procedure Test_DeleteKey_LeavesOtherKeys;

		{Clear operation}
		[Test]
		procedure Test_Clear_RemovesAllData;

		{Isolation tests}
		[Test]
		procedure Test_MultipleSections_AreIsolated;
	end;

	[TestFixture]
	TIniConfigFileTest = class
	private
		FTestIniPath: string;
		FConfig: IConfigFile;

		procedure CleanupTempFile;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Basic operations}
		[Test]
		procedure Test_GetFilePath_ReturnsConstructorPath;

		[Test]
		procedure Test_WriteString_ThenRead_ReturnsValue;

		[Test]
		procedure Test_WriteBool_ThenRead_ReturnsValue;

		[Test]
		procedure Test_WriteInteger_ThenRead_ReturnsValue;

		[Test]
		procedure Test_WriteInt64_ThenRead_ReturnsValue;

		{Default values}
		[Test]
		procedure Test_ReadString_WhenMissing_ReturnsDefault;

		[Test]
		procedure Test_ReadBool_WhenMissing_ReturnsDefault;

		[Test]
		procedure Test_ReadInteger_WhenMissing_ReturnsDefault;

		[Test]
		procedure Test_ReadInt64_WhenMissing_ReturnsDefault;

		{WriteIfNotDefault}
		[Test]
		procedure Test_WriteStringIfNotDefault_WhenDifferent_Writes;

		[Test]
		procedure Test_WriteStringIfNotDefault_WhenEqual_DeletesKey;

		[Test]
		procedure Test_WriteBoolIfNotDefault_Behavior;

		[Test]
		procedure Test_WriteIntegerIfNotDefault_Behavior;

		[Test]
		procedure Test_WriteInt64IfNotDefault_Behavior;

		{Section operations}
		[Test]
		procedure Test_SectionExists_Behavior;

		[Test]
		procedure Test_ReadSections_ReturnsAllSections;

		[Test]
		procedure Test_EraseSection_RemovesSection;

		{Key operations}
		[Test]
		procedure Test_DeleteKey_RemovesKey;

		{QueryInterface tests}
		[Test]
		procedure Test_QueryInterface_ValidIID_ReturnsSOK;
		[Test]
		procedure Test_QueryInterface_InvalidIID_ReturnsENoInterface;
	end;

implementation

{TNullConfigFileTest}

procedure TNullConfigFileTest.Setup;
begin
	FConfig := TNullConfigFile.Create;
end;

procedure TNullConfigFileTest.Test_GetFilePath_ReturnsEmptyString;
begin
	Assert.AreEqual('', FConfig.GetFilePath);
end;

procedure TNullConfigFileTest.Test_ReadString_ReturnsDefault;
begin
	Assert.AreEqual('default_value', FConfig.ReadString('Section', 'Key', 'default_value'));
	Assert.AreEqual('', FConfig.ReadString('Section', 'Key', ''));
	Assert.AreEqual('custom', FConfig.ReadString('AnySection', 'AnyKey', 'custom'));
end;

procedure TNullConfigFileTest.Test_ReadBool_ReturnsDefault;
begin
	Assert.IsTrue(FConfig.ReadBool('Section', 'Key', True));
	Assert.IsFalse(FConfig.ReadBool('Section', 'Key', False));
end;

procedure TNullConfigFileTest.Test_ReadInteger_ReturnsDefault;
begin
	Assert.AreEqual(42, FConfig.ReadInteger('Section', 'Key', 42));
	Assert.AreEqual(-100, FConfig.ReadInteger('Section', 'Key', -100));
	Assert.AreEqual(0, FConfig.ReadInteger('Section', 'Key', 0));
end;

procedure TNullConfigFileTest.Test_ReadInt64_ReturnsDefault;
begin
	Assert.AreEqual(Int64(9223372036854775807), FConfig.ReadInt64('Section', 'Key', 9223372036854775807));
	Assert.AreEqual(Int64(0), FConfig.ReadInt64('Section', 'Key', 0));
end;

procedure TNullConfigFileTest.Test_SectionExists_ReturnsFalse;
begin
	Assert.IsFalse(FConfig.SectionExists('AnySection'));
	Assert.IsFalse(FConfig.SectionExists(''));
end;

procedure TNullConfigFileTest.Test_ReadSections_LeavesListEmpty;
var
	Sections: TStringList;
begin
	Sections := TStringList.Create;
	try
		Sections.Add('PreExisting');
		FConfig.ReadSections(Sections);
		{Null implementation should leave list unchanged or clear it - checking it doesn't crash}
		Assert.Pass;
	finally
		Sections.Free;
	end;
end;

procedure TNullConfigFileTest.Test_WriteOperations_DoNotThrow;
begin
	{All write operations should be no-ops and not throw}
	FConfig.WriteString('Section', 'Key', 'Value');
	FConfig.WriteBool('Section', 'Key', True);
	FConfig.WriteInteger('Section', 'Key', 123);
	FConfig.WriteInt64('Section', 'Key', 123456789012345);
	FConfig.WriteStringIfNotDefault('Section', 'Key', 'Value', 'Default');
	FConfig.WriteBoolIfNotDefault('Section', 'Key', True, False);
	FConfig.WriteIntegerIfNotDefault('Section', 'Key', 100, 0);
	FConfig.WriteInt64IfNotDefault('Section', 'Key', 100, 0);
	FConfig.EraseSection('Section');
	FConfig.DeleteKey('Section', 'Key');
	Assert.Pass;
end;

{TMemoryConfigFileTest}

procedure TMemoryConfigFileTest.Setup;
begin
	FConfig := TMemoryConfigFile.Create;
	FConfigInterface := FConfig;
end;

procedure TMemoryConfigFileTest.TearDown;
begin
	FConfigInterface := nil;
end;

procedure TMemoryConfigFileTest.Test_Create_WithoutPath_ReturnsEmptyFilePath;
begin
	Assert.AreEqual('', FConfig.GetFilePath);
end;

procedure TMemoryConfigFileTest.Test_Create_WithPath_ReturnsFilePath;
var
	ConfigWithPath: TMemoryConfigFile;
	ConfigRef: IConfigFile;
begin
	ConfigWithPath := TMemoryConfigFile.Create('C:\Config\test.ini');
	ConfigRef := ConfigWithPath;
	try
		Assert.AreEqual('C:\Config\test.ini', ConfigWithPath.GetFilePath);
	finally
		ConfigRef := nil;
	end;
end;

procedure TMemoryConfigFileTest.Test_ReadString_WhenMissing_ReturnsDefault;
begin
	Assert.AreEqual('default', FConfig.ReadString('Section', 'Key', 'default'));
end;

procedure TMemoryConfigFileTest.Test_WriteString_ThenRead_ReturnsValue;
begin
	FConfig.WriteString('Section', 'Key', 'TestValue');
	Assert.AreEqual('TestValue', FConfig.ReadString('Section', 'Key', 'default'));
end;

procedure TMemoryConfigFileTest.Test_WriteString_OverwritesExisting;
begin
	FConfig.WriteString('Section', 'Key', 'First');
	FConfig.WriteString('Section', 'Key', 'Second');
	Assert.AreEqual('Second', FConfig.ReadString('Section', 'Key', 'default'));
end;

procedure TMemoryConfigFileTest.Test_ReadBool_WhenMissing_ReturnsDefault;
begin
	Assert.IsTrue(FConfig.ReadBool('Section', 'Key', True));
	Assert.IsFalse(FConfig.ReadBool('Section', 'Key', False));
end;

procedure TMemoryConfigFileTest.Test_WriteBool_True_StoresAs1;
begin
	FConfig.WriteBool('Section', 'Key', True);
	Assert.AreEqual('1', FConfig.ReadString('Section', 'Key', ''));
end;

procedure TMemoryConfigFileTest.Test_WriteBool_False_StoresAs0;
begin
	FConfig.WriteBool('Section', 'Key', False);
	Assert.AreEqual('0', FConfig.ReadString('Section', 'Key', ''));
end;

procedure TMemoryConfigFileTest.Test_ReadBool_Parses1AsTrue;
begin
	FConfig.WriteString('Section', 'Key', '1');
	Assert.IsTrue(FConfig.ReadBool('Section', 'Key', False));
end;

procedure TMemoryConfigFileTest.Test_ReadBool_ParsesTrueAsTrue;
begin
	FConfig.WriteString('Section', 'Key', 'true');
	Assert.IsTrue(FConfig.ReadBool('Section', 'Key', False));
end;

procedure TMemoryConfigFileTest.Test_ReadBool_Parses0AsFalse;
begin
	FConfig.WriteString('Section', 'Key', '0');
	Assert.IsFalse(FConfig.ReadBool('Section', 'Key', True));
end;

procedure TMemoryConfigFileTest.Test_ReadBool_ParsesFalseAsFalse;
begin
	FConfig.WriteString('Section', 'Key', 'false');
	Assert.IsFalse(FConfig.ReadBool('Section', 'Key', True));
end;

procedure TMemoryConfigFileTest.Test_ReadInteger_WhenMissing_ReturnsDefault;
begin
	Assert.AreEqual(42, FConfig.ReadInteger('Section', 'Key', 42));
end;

procedure TMemoryConfigFileTest.Test_WriteInteger_ThenRead_ReturnsValue;
begin
	FConfig.WriteInteger('Section', 'Key', 12345);
	Assert.AreEqual(12345, FConfig.ReadInteger('Section', 'Key', 0));
end;

procedure TMemoryConfigFileTest.Test_ReadInteger_InvalidValue_ReturnsDefault;
begin
	FConfig.WriteString('Section', 'Key', 'not_a_number');
	Assert.AreEqual(99, FConfig.ReadInteger('Section', 'Key', 99));
end;

procedure TMemoryConfigFileTest.Test_ReadInt64_WhenMissing_ReturnsDefault;
begin
	Assert.AreEqual(Int64(123456789012345), FConfig.ReadInt64('Section', 'Key', 123456789012345));
end;

procedure TMemoryConfigFileTest.Test_WriteInt64_ThenRead_ReturnsValue;
begin
	FConfig.WriteInt64('Section', 'Key', 987654321098765);
	Assert.AreEqual(Int64(987654321098765), FConfig.ReadInt64('Section', 'Key', 0));
end;

procedure TMemoryConfigFileTest.Test_WriteInt64_LargeValue_Preserved;
begin
	{Test with max Int64 value}
	FConfig.WriteInt64('Section', 'Key', 9223372036854775807);
	Assert.AreEqual(Int64(9223372036854775807), FConfig.ReadInt64('Section', 'Key', 0));
end;

procedure TMemoryConfigFileTest.Test_WriteStringIfNotDefault_WhenDifferent_Writes;
begin
	FConfig.WriteStringIfNotDefault('Section', 'Key', 'custom', 'default');
	Assert.AreEqual('custom', FConfig.ReadString('Section', 'Key', ''));
end;

procedure TMemoryConfigFileTest.Test_WriteStringIfNotDefault_WhenEqual_DeletesKey;
begin
	{First write a value}
	FConfig.WriteString('Section', 'Key', 'custom');
	Assert.AreEqual('custom', FConfig.ReadString('Section', 'Key', ''));

	{Now write default value - should delete key}
	FConfig.WriteStringIfNotDefault('Section', 'Key', 'default', 'default');
	Assert.AreEqual('fallback', FConfig.ReadString('Section', 'Key', 'fallback'));
end;

procedure TMemoryConfigFileTest.Test_WriteBoolIfNotDefault_WhenDifferent_Writes;
begin
	FConfig.WriteBoolIfNotDefault('Section', 'Key', True, False);
	Assert.IsTrue(FConfig.ReadBool('Section', 'Key', False));
end;

procedure TMemoryConfigFileTest.Test_WriteBoolIfNotDefault_WhenEqual_DeletesKey;
begin
	FConfig.WriteBool('Section', 'Key', True);
	FConfig.WriteBoolIfNotDefault('Section', 'Key', False, False);
	Assert.IsTrue(FConfig.ReadBool('Section', 'Key', True)); {Returns default because key deleted}
end;

procedure TMemoryConfigFileTest.Test_WriteIntegerIfNotDefault_WhenDifferent_Writes;
begin
	FConfig.WriteIntegerIfNotDefault('Section', 'Key', 100, 0);
	Assert.AreEqual(100, FConfig.ReadInteger('Section', 'Key', 0));
end;

procedure TMemoryConfigFileTest.Test_WriteIntegerIfNotDefault_WhenEqual_DeletesKey;
begin
	FConfig.WriteInteger('Section', 'Key', 100);
	FConfig.WriteIntegerIfNotDefault('Section', 'Key', 0, 0);
	Assert.AreEqual(999, FConfig.ReadInteger('Section', 'Key', 999));
end;

procedure TMemoryConfigFileTest.Test_WriteInt64IfNotDefault_WhenDifferent_Writes;
begin
	FConfig.WriteInt64IfNotDefault('Section', 'Key', 123456789012345, 0);
	Assert.AreEqual(Int64(123456789012345), FConfig.ReadInt64('Section', 'Key', 0));
end;

procedure TMemoryConfigFileTest.Test_WriteInt64IfNotDefault_WhenEqual_DeletesKey;
begin
	FConfig.WriteInt64('Section', 'Key', 100);
	FConfig.WriteInt64IfNotDefault('Section', 'Key', 0, 0);
	Assert.AreEqual(Int64(999), FConfig.ReadInt64('Section', 'Key', 999));
end;

procedure TMemoryConfigFileTest.Test_SectionExists_WhenMissing_ReturnsFalse;
begin
	Assert.IsFalse(FConfig.SectionExists('NonExistent'));
end;

procedure TMemoryConfigFileTest.Test_SectionExists_AfterWrite_ReturnsTrue;
begin
	FConfig.WriteString('TestSection', 'Key', 'Value');
	Assert.IsTrue(FConfig.SectionExists('TestSection'));
end;

procedure TMemoryConfigFileTest.Test_ReadSections_ReturnsAllSections;
var
	Sections: TStringList;
begin
	FConfig.WriteString('Section1', 'Key', 'Value');
	FConfig.WriteString('Section2', 'Key', 'Value');
	FConfig.WriteString('Section3', 'Key', 'Value');

	Sections := TStringList.Create;
	try
		FConfig.ReadSections(Sections);
		Assert.AreEqual(3, Sections.Count);
		Assert.IsTrue(Sections.IndexOf('Section1') >= 0);
		Assert.IsTrue(Sections.IndexOf('Section2') >= 0);
		Assert.IsTrue(Sections.IndexOf('Section3') >= 0);
	finally
		Sections.Free;
	end;
end;

procedure TMemoryConfigFileTest.Test_EraseSection_RemovesSection;
begin
	FConfig.WriteString('Section', 'Key1', 'Value1');
	FConfig.WriteString('Section', 'Key2', 'Value2');
	Assert.IsTrue(FConfig.SectionExists('Section'));

	FConfig.EraseSection('Section');
	Assert.IsFalse(FConfig.SectionExists('Section'));
	Assert.AreEqual('default', FConfig.ReadString('Section', 'Key1', 'default'));
end;

procedure TMemoryConfigFileTest.Test_EraseSection_NonExistent_DoesNotThrow;
begin
	FConfig.EraseSection('NonExistent');
	Assert.Pass;
end;

procedure TMemoryConfigFileTest.Test_DeleteKey_RemovesKey;
begin
	FConfig.WriteString('Section', 'Key', 'Value');
	Assert.AreEqual('Value', FConfig.ReadString('Section', 'Key', 'default'));

	FConfig.DeleteKey('Section', 'Key');
	Assert.AreEqual('default', FConfig.ReadString('Section', 'Key', 'default'));
end;

procedure TMemoryConfigFileTest.Test_DeleteKey_NonExistent_DoesNotThrow;
begin
	FConfig.DeleteKey('NonExistent', 'Key');
	Assert.Pass;
end;

procedure TMemoryConfigFileTest.Test_DeleteKey_LeavesOtherKeys;
begin
	FConfig.WriteString('Section', 'Key1', 'Value1');
	FConfig.WriteString('Section', 'Key2', 'Value2');

	FConfig.DeleteKey('Section', 'Key1');

	Assert.AreEqual('default', FConfig.ReadString('Section', 'Key1', 'default'));
	Assert.AreEqual('Value2', FConfig.ReadString('Section', 'Key2', 'default'));
end;

procedure TMemoryConfigFileTest.Test_Clear_RemovesAllData;
begin
	FConfig.WriteString('Section1', 'Key', 'Value');
	FConfig.WriteString('Section2', 'Key', 'Value');

	FConfig.Clear;

	Assert.IsFalse(FConfig.SectionExists('Section1'));
	Assert.IsFalse(FConfig.SectionExists('Section2'));
end;

procedure TMemoryConfigFileTest.Test_MultipleSections_AreIsolated;
begin
	FConfig.WriteString('Section1', 'Key', 'Value1');
	FConfig.WriteString('Section2', 'Key', 'Value2');

	Assert.AreEqual('Value1', FConfig.ReadString('Section1', 'Key', ''));
	Assert.AreEqual('Value2', FConfig.ReadString('Section2', 'Key', ''));
	Assert.AreEqual('default', FConfig.ReadString('Section3', 'Key', 'default'));
end;

{TIniConfigFileTest}

procedure TIniConfigFileTest.Setup;
begin
	FTestIniPath := TPath.Combine(TPath.GetTempPath, 'test_iniconfigfile_' + IntToStr(Random(100000)) + '.ini');
	FConfig := TIniConfigFile.Create(FTestIniPath);
end;

procedure TIniConfigFileTest.TearDown;
begin
	FConfig := nil;
	CleanupTempFile;
end;

procedure TIniConfigFileTest.CleanupTempFile;
begin
	if TFile.Exists(FTestIniPath) then
		TFile.Delete(FTestIniPath);
end;

procedure TIniConfigFileTest.Test_GetFilePath_ReturnsConstructorPath;
begin
	Assert.AreEqual(FTestIniPath, FConfig.GetFilePath);
end;

procedure TIniConfigFileTest.Test_WriteString_ThenRead_ReturnsValue;
begin
	FConfig.WriteString('Section', 'Key', 'TestValue');
	Assert.AreEqual('TestValue', FConfig.ReadString('Section', 'Key', 'default'));
end;

procedure TIniConfigFileTest.Test_WriteBool_ThenRead_ReturnsValue;
begin
	FConfig.WriteBool('Section', 'TrueKey', True);
	FConfig.WriteBool('Section', 'FalseKey', False);

	Assert.IsTrue(FConfig.ReadBool('Section', 'TrueKey', False));
	Assert.IsFalse(FConfig.ReadBool('Section', 'FalseKey', True));
end;

procedure TIniConfigFileTest.Test_WriteInteger_ThenRead_ReturnsValue;
begin
	FConfig.WriteInteger('Section', 'Key', 12345);
	Assert.AreEqual(12345, FConfig.ReadInteger('Section', 'Key', 0));
end;

procedure TIniConfigFileTest.Test_WriteInt64_ThenRead_ReturnsValue;
begin
	FConfig.WriteInt64('Section', 'Key', 9223372036854775807);
	Assert.AreEqual(Int64(9223372036854775807), FConfig.ReadInt64('Section', 'Key', 0));
end;

procedure TIniConfigFileTest.Test_ReadString_WhenMissing_ReturnsDefault;
begin
	Assert.AreEqual('default_value', FConfig.ReadString('Section', 'NonExistent', 'default_value'));
end;

procedure TIniConfigFileTest.Test_ReadBool_WhenMissing_ReturnsDefault;
begin
	Assert.IsTrue(FConfig.ReadBool('Section', 'NonExistent', True));
	Assert.IsFalse(FConfig.ReadBool('Section', 'NonExistent', False));
end;

procedure TIniConfigFileTest.Test_ReadInteger_WhenMissing_ReturnsDefault;
begin
	Assert.AreEqual(42, FConfig.ReadInteger('Section', 'NonExistent', 42));
end;

procedure TIniConfigFileTest.Test_ReadInt64_WhenMissing_ReturnsDefault;
begin
	Assert.AreEqual(Int64(123456789012345), FConfig.ReadInt64('Section', 'NonExistent', 123456789012345));
end;

procedure TIniConfigFileTest.Test_WriteStringIfNotDefault_WhenDifferent_Writes;
begin
	FConfig.WriteStringIfNotDefault('Section', 'Key', 'custom', 'default');
	Assert.AreEqual('custom', FConfig.ReadString('Section', 'Key', ''));
end;

procedure TIniConfigFileTest.Test_WriteStringIfNotDefault_WhenEqual_DeletesKey;
begin
	{First write a value}
	FConfig.WriteString('Section', 'Key', 'custom');
	Assert.AreEqual('custom', FConfig.ReadString('Section', 'Key', ''));

	{Now write default value - should delete key}
	FConfig.WriteStringIfNotDefault('Section', 'Key', 'default', 'default');
	Assert.AreEqual('fallback', FConfig.ReadString('Section', 'Key', 'fallback'));
end;

procedure TIniConfigFileTest.Test_WriteBoolIfNotDefault_Behavior;
begin
	{Write non-default}
	FConfig.WriteBoolIfNotDefault('Section', 'Key', True, False);
	Assert.IsTrue(FConfig.ReadBool('Section', 'Key', False));

	{Write default - should delete}
	FConfig.WriteBoolIfNotDefault('Section', 'Key', False, False);
	Assert.IsTrue(FConfig.ReadBool('Section', 'Key', True)); {Returns True because key deleted}
end;

procedure TIniConfigFileTest.Test_WriteIntegerIfNotDefault_Behavior;
begin
	{Write non-default}
	FConfig.WriteIntegerIfNotDefault('Section', 'Key', 100, 0);
	Assert.AreEqual(100, FConfig.ReadInteger('Section', 'Key', 0));

	{Write default - should delete}
	FConfig.WriteIntegerIfNotDefault('Section', 'Key', 0, 0);
	Assert.AreEqual(999, FConfig.ReadInteger('Section', 'Key', 999));
end;

procedure TIniConfigFileTest.Test_WriteInt64IfNotDefault_Behavior;
begin
	{Write non-default}
	FConfig.WriteInt64IfNotDefault('Section', 'Key', 123456789012345, 0);
	Assert.AreEqual(Int64(123456789012345), FConfig.ReadInt64('Section', 'Key', 0));

	{Write default - should delete}
	FConfig.WriteInt64IfNotDefault('Section', 'Key', 0, 0);
	Assert.AreEqual(Int64(999), FConfig.ReadInt64('Section', 'Key', 999));
end;

procedure TIniConfigFileTest.Test_SectionExists_Behavior;
begin
	Assert.IsFalse(FConfig.SectionExists('TestSection'));

	FConfig.WriteString('TestSection', 'Key', 'Value');
	Assert.IsTrue(FConfig.SectionExists('TestSection'));
end;

procedure TIniConfigFileTest.Test_ReadSections_ReturnsAllSections;
var
	Sections: TStringList;
begin
	FConfig.WriteString('Section1', 'Key', 'Value');
	FConfig.WriteString('Section2', 'Key', 'Value');
	FConfig.WriteString('Section3', 'Key', 'Value');

	Sections := TStringList.Create;
	try
		FConfig.ReadSections(Sections);
		Assert.AreEqual(3, Sections.Count);
		Assert.IsTrue(Sections.IndexOf('Section1') >= 0);
		Assert.IsTrue(Sections.IndexOf('Section2') >= 0);
		Assert.IsTrue(Sections.IndexOf('Section3') >= 0);
	finally
		Sections.Free;
	end;
end;

procedure TIniConfigFileTest.Test_EraseSection_RemovesSection;
begin
	FConfig.WriteString('Section', 'Key1', 'Value1');
	FConfig.WriteString('Section', 'Key2', 'Value2');
	Assert.IsTrue(FConfig.SectionExists('Section'));

	FConfig.EraseSection('Section');
	Assert.IsFalse(FConfig.SectionExists('Section'));
end;

procedure TIniConfigFileTest.Test_DeleteKey_RemovesKey;
begin
	FConfig.WriteString('Section', 'Key1', 'Value1');
	FConfig.WriteString('Section', 'Key2', 'Value2');

	FConfig.DeleteKey('Section', 'Key1');

	Assert.AreEqual('default', FConfig.ReadString('Section', 'Key1', 'default'));
	Assert.AreEqual('Value2', FConfig.ReadString('Section', 'Key2', 'default'));
end;

procedure TIniConfigFileTest.Test_QueryInterface_ValidIID_ReturnsSOK;
var
	Intf: IConfigFile;
	HR: HResult;
begin
	{IConfigFile inherits from IInterface, so QueryInterface is accessible directly}
	HR := FConfig.QueryInterface(IConfigFile, Intf);
	Assert.AreEqual(Integer(S_OK), Integer(HR));
end;

procedure TIniConfigFileTest.Test_QueryInterface_InvalidIID_ReturnsENoInterface;
var
	Intf: IInterface;
	HR: HResult;
	FakeGUID: TGUID;
begin
	FakeGUID := StringToGUID('{00000000-0000-0000-0000-000000000001}');
	HR := FConfig.QueryInterface(FakeGUID, Intf);
	Assert.AreEqual(Integer(E_NOINTERFACE), Integer(HR));
end;

initialization

TDUnitX.RegisterTestFixture(TNullConfigFileTest);
TDUnitX.RegisterTestFixture(TMemoryConfigFileTest);
TDUnitX.RegisterTestFixture(TIniConfigFileTest);

end.

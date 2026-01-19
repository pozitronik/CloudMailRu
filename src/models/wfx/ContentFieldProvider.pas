unit ContentFieldProvider;

interface

uses
	Windows, SysUtils, AnsiStrings, DateUtils,
	CMRDirItem, CMRConstants, PLUGIN_TYPES,
	SystemHelper,
	IContentFieldProviderInterface;

type
	{ Provides content field metadata and value extraction for WFX plugin.
	  Encapsulates field definitions and extraction logic previously in TMailRuCloudWFX. }
	TContentFieldProvider = class(TInterfacedObject, IContentFieldProvider)
	private type
		TFieldDefinition = record
			Name: AnsiString;
			FieldType: Integer;
		end;
	private const
		FIELD_COUNT = 18;
		FIELD_INDEX_DESCRIPTION = 14;
		FieldDefinitions: array[0..FIELD_COUNT - 1] of TFieldDefinition = (
			(Name: 'tree';          FieldType: ft_stringw),
			(Name: 'name';          FieldType: ft_stringw),
			(Name: 'grev';          FieldType: ft_numeric_32),
			(Name: 'size';          FieldType: ft_numeric_64),
			(Name: 'kind';          FieldType: ft_stringw),
			(Name: 'weblink';       FieldType: ft_stringw),
			(Name: 'rev';           FieldType: ft_numeric_32),
			(Name: 'type';          FieldType: ft_stringw),
			(Name: 'home';          FieldType: ft_stringw),
			(Name: 'mtime';         FieldType: ft_datetime),
			(Name: 'hash';          FieldType: ft_stringw),
			(Name: 'virus_scan';    FieldType: ft_stringw),
			(Name: 'folders_count'; FieldType: ft_numeric_32),
			(Name: 'files_count';   FieldType: ft_numeric_32),
			(Name: 'description';   FieldType: ft_stringw),
			(Name: 'deleted_at';    FieldType: ft_datetime),
			(Name: 'deleted_from';  FieldType: ft_stringw),
			(Name: 'deleted_by';    FieldType: ft_stringw)
		);
	private
		function GetDescriptionValue(const Item: TCMRDirItem; FieldValue: Pointer;
			const Context: TContentFieldContext): Integer;
		function GetDateTimeValue(UnixTimestamp: Int64; FieldValue: Pointer): Integer;
	public
		function GetSupportedField(FieldIndex: Integer; FieldName: PAnsiChar; MaxLen: Integer): Integer;
		function GetValue(FieldIndex: Integer; const Item: TCMRDirItem;
			FieldValue: Pointer; const Context: TContentFieldContext): Integer;
		function GetFieldCount: Integer;
	end;

implementation

function TContentFieldProvider.GetSupportedField(FieldIndex: Integer; FieldName: PAnsiChar; MaxLen: Integer): Integer;
begin
	if (FieldIndex < 0) or (FieldIndex >= FIELD_COUNT) then
		exit(ft_nomorefields);

	System.AnsiStrings.StrPCopy(FieldName, FieldDefinitions[FieldIndex].Name);
	Result := FieldDefinitions[FieldIndex].FieldType;
end;

function TContentFieldProvider.GetFieldCount: Integer;
begin
	Result := FIELD_COUNT;
end;

function TContentFieldProvider.GetDateTimeValue(UnixTimestamp: Int64; FieldValue: Pointer): Integer;
var
	FileTime: TFileTime;
begin
	FileTime.dwHighDateTime := 0;
	FileTime.dwLowDateTime := 0;
	FileTime := DateTimeToFileTime(UnixToDateTime(UnixTimestamp));
	Move(FileTime, FieldValue^, SizeOf(FileTime));
	Result := ft_datetime;
end;

function TContentFieldProvider.GetDescriptionValue(const Item: TCMRDirItem; FieldValue: Pointer;
	const Context: TContentFieldContext): Integer;
begin
	if Context.IsAccountRoot then
	begin
		StrPCopy(FieldValue, Context.AccountDescription);
	end
	else if Context.DescriptionsEnabled then
	begin
		StrPCopy(FieldValue, Context.FileDescription);
	end
	else
	begin
		StrPCopy(FieldValue, '<disabled>');
	end;
	Result := ft_stringw;
end;

function TContentFieldProvider.GetValue(FieldIndex: Integer; const Item: TCMRDirItem;
	FieldValue: Pointer; const Context: TContentFieldContext): Integer;
begin
	Result := ft_nosuchfield;

	{ Account root only supports description field }
	if Context.IsAccountRoot then
	begin
		if FieldIndex = FIELD_INDEX_DESCRIPTION then
			Result := GetDescriptionValue(Item, FieldValue, Context);
		exit;
	end;

	case FieldIndex of
		0: { tree - only for shared/invite items (mtime=0) }
			begin
				if Item.mtime <> 0 then
					exit(ft_nosuchfield);
				StrPCopy(FieldValue, Item.tree);
				Result := ft_stringw;
			end;
		1: { name }
			begin
				StrPCopy(FieldValue, Item.name);
				Result := ft_stringw;
			end;
		2: { grev - only for shared/invite items (mtime=0) }
			begin
				if Item.mtime <> 0 then
					exit(ft_nosuchfield);
				Move(Item.grev, FieldValue^, SizeOf(Item.grev));
				Result := ft_numeric_32;
			end;
		3: { size }
			begin
				Move(Item.size, FieldValue^, SizeOf(Item.size));
				Result := ft_numeric_64;
			end;
		4: { kind }
			begin
				StrPCopy(FieldValue, Item.kind);
				Result := ft_stringw;
			end;
		5: { weblink }
			begin
				StrPCopy(FieldValue, Item.weblink);
				Result := ft_stringw;
			end;
		6: { rev - only for shared/invite items (mtime=0) }
			begin
				if Item.mtime <> 0 then
					exit(ft_nosuchfield);
				Move(Item.rev, FieldValue^, SizeOf(Item.rev));
				Result := ft_numeric_32;
			end;
		7: { type }
			begin
				StrPCopy(FieldValue, Item.type_);
				Result := ft_stringw;
			end;
		8: { home }
			begin
				StrPCopy(FieldValue, Item.home);
				Result := ft_stringw;
			end;
		9: { mtime - only for regular items (mtime<>0) }
			begin
				if Item.mtime = 0 then
					exit(ft_nosuchfield);
				Result := GetDateTimeValue(Item.mtime, FieldValue);
			end;
		10: { hash }
			begin
				StrPCopy(FieldValue, Item.hash);
				Result := ft_stringw;
			end;
		11: { virus_scan }
			begin
				StrPCopy(FieldValue, Item.virus_scan);
				Result := ft_stringw;
			end;
		12: { folders_count - only for folders }
			begin
				if Item.type_ = TYPE_FILE then
					exit(ft_nosuchfield);
				Move(Item.folders_count, FieldValue^, SizeOf(Item.folders_count));
				Result := ft_numeric_32;
			end;
		13: { files_count - only for folders }
			begin
				if Item.type_ = TYPE_FILE then
					exit(ft_nosuchfield);
				Move(Item.files_count, FieldValue^, SizeOf(Item.files_count));
				Result := ft_numeric_32;
			end;
		14: { description }
			begin
				Result := GetDescriptionValue(Item, FieldValue, Context);
			end;
		15: { deleted_at - only for trash items }
			begin
				if Item.deleted_at = 0 then
					exit(ft_nosuchfield);
				Result := GetDateTimeValue(Item.deleted_at, FieldValue);
			end;
		16: { deleted_from - only for trash items }
			begin
				if Item.deleted_from = EmptyWideStr then
					exit(ft_nosuchfield);
				StrPCopy(FieldValue, Item.deleted_from);
				Result := ft_stringw;
			end;
		17: { deleted_by - only for trash items }
			begin
				if Item.deleted_by = 0 then
					exit(ft_nosuchfield);
				StrPCopy(FieldValue, Item.deleted_by.ToString);
				Result := ft_stringw;
			end;
	end;
end;

end.

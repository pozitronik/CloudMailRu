unit TimestampStoreAdapter;

{Adapter wrapping TTimestampMetadata as IRemoteMetadataStore.
	Owns the wrapped TTimestampMetadata instance and frees it on release.
	Serializes TTimestampEntry as tab-separated "LocalMTime<TAB>CloudMTime" strings
	for cross-store transfers via GetEntryAsString/SetEntryFromString.}

interface

uses
	RemoteMetadataStore,
	TimestampMetadata;

type
	TTimestampStoreAdapter = class(TInterfacedObject, IRemoteMetadataStore)
	private
		FMetadata: TTimestampMetadata;
	public
		{Takes ownership of the passed TTimestampMetadata instance}
		constructor Create(Metadata: TTimestampMetadata);
		destructor Destroy; override;

		procedure Read;
		procedure Write;
		function DeleteEntry(const Key: WideString): Boolean;
		function RenameEntry(const OldKey, NewKey: WideString): Boolean;
		function GetEntryAsString(const Key: WideString): WideString;
		procedure SetEntryFromString(const Key, Value: WideString);
		function GetFileName: WideString;
	end;

implementation

uses
	SysUtils,
	TimestampEntry;

constructor TTimestampStoreAdapter.Create(Metadata: TTimestampMetadata);
begin
	inherited Create;
	FMetadata := Metadata;
end;

destructor TTimestampStoreAdapter.Destroy;
begin
	FMetadata.Free;
	inherited;
end;

procedure TTimestampStoreAdapter.Read;
begin
	FMetadata.Read;
end;

procedure TTimestampStoreAdapter.Write;
begin
	FMetadata.Write;
end;

function TTimestampStoreAdapter.DeleteEntry(const Key: WideString): Boolean;
begin
	Result := FMetadata.DeleteEntry(Key);
end;

function TTimestampStoreAdapter.RenameEntry(const OldKey, NewKey: WideString): Boolean;
begin
	Result := FMetadata.RenameEntry(OldKey, NewKey);
end;

function TTimestampStoreAdapter.GetEntryAsString(const Key: WideString): WideString;
var
	Entry: TTimestampEntry;
begin
	Entry := FMetadata.GetEntry(Key);
	if Entry.IsEmpty then
		Exit('');
	Result := IntToStr(Entry.LocalMTime) + #9 + IntToStr(Entry.CloudMTime);
end;

procedure TTimestampStoreAdapter.SetEntryFromString(const Key, Value: WideString);
var
	Parts: TArray<string>;
	Entry: TTimestampEntry;
begin
	if Value = '' then
	begin
		FMetadata.DeleteEntry(Key);
		Exit;
	end;

	Parts := string(Value).Split([#9]);
	if Length(Parts) >= 2 then
	begin
		Entry.LocalMTime := StrToInt64Def(Parts[0], 0);
		Entry.CloudMTime := StrToInt64Def(Parts[1], 0);
		FMetadata.SetEntry(Key, Entry);
	end;
end;

function TTimestampStoreAdapter.GetFileName: WideString;
begin
	Result := FMetadata.MetadataFileName;
end;

end.

unit DescriptionStoreAdapter;

{Adapter wrapping TDescription as IRemoteMetadataStore.
	Owns the wrapped TDescription instance and frees it on release.
	Enables BaseRemoteMetadataSyncManager to work with description files
	through the common metadata store interface.}

interface

uses
	RemoteMetadataStore,
	Description;

type
	TDescriptionStoreAdapter = class(TInterfacedObject, IRemoteMetadataStore)
	private
		FDescription: TDescription;
	public
		{Takes ownership of the passed TDescription instance}
		constructor Create(Description: TDescription);
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

constructor TDescriptionStoreAdapter.Create(Description: TDescription);
begin
	inherited Create;
	FDescription := Description;
end;

destructor TDescriptionStoreAdapter.Destroy;
begin
	FDescription.Free;
	inherited;
end;

procedure TDescriptionStoreAdapter.Read;
begin
	FDescription.Read;
end;

procedure TDescriptionStoreAdapter.Write;
begin
	FDescription.Write;
end;

function TDescriptionStoreAdapter.DeleteEntry(const Key: WideString): Boolean;
begin
	Result := FDescription.DeleteValue(Key);
end;

function TDescriptionStoreAdapter.RenameEntry(const OldKey, NewKey: WideString): Boolean;
begin
	Result := FDescription.RenameItem(OldKey, NewKey);
end;

function TDescriptionStoreAdapter.GetEntryAsString(const Key: WideString): WideString;
begin
	Result := FDescription.GetValue(Key, FORMAT_AS_IS);
end;

procedure TDescriptionStoreAdapter.SetEntryFromString(const Key, Value: WideString);
begin
	FDescription.SetValue(Key, Value);
end;

function TDescriptionStoreAdapter.GetFileName: WideString;
begin
	Result := FDescription.IonFilename;
end;

end.

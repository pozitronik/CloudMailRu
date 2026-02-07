unit MockSyncGuards;

{Shared mock IDescriptionSyncGuard and ITimestampSyncGuard for testing.
	Tracks all call types with counters, boolean flags, and last-path fields.}

interface

uses
	DescriptionSyncGuard,
	TimestampSyncGuard,
	RealPath,
	CloudMailRu;

type
	{Tracks all description sync operations}
	TMockDescriptionSyncGuard = class(TInterfacedObject, IDescriptionSyncGuard)
	public
		{Call counters}
		DeletedCalls: Integer;
		RenamedCalls: Integer;
		DownloadedCalls: Integer;
		UploadedCalls: Integer;

		{Boolean flags}
		OnFileDeletedCalled: Boolean;
		OnFileRenamedCalled: Boolean;
		OnFileDownloadedCalled: Boolean;
		OnFileUploadedCalled: Boolean;

		{Last-call parameters}
		LastDeletedPath: TRealPath;
		LastOldPath: TRealPath;
		LastNewPath: TRealPath;
		LastRemotePath: TRealPath;
		LastLocalPath: WideString;

		constructor Create;

		procedure OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
		procedure OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
	end;

	{Tracks all timestamp sync operations with configurable download return}
	TMockTimestampSyncGuard = class(TInterfacedObject, ITimestampSyncGuard)
	public
		{Call counters}
		UploadedCalls: Integer;
		DownloadedCalls: Integer;
		DeletedCalls: Integer;
		RenamedCalls: Integer;

		{Boolean flags}
		OnFileRenamedCalled: Boolean;

		{Configurable return value for OnFileDownloaded}
		DownloadReturnValue: Int64;

		constructor Create;

		procedure OnFileUploaded(const RemotePath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
		function OnFileDownloaded(const RemotePath: TRealPath; const LocalPath: WideString; CloudMTime: Int64; Cloud: TCloudMailRu): Int64;
		procedure OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
	end;

implementation

{TMockDescriptionSyncGuard}

constructor TMockDescriptionSyncGuard.Create;
begin
	inherited Create;
	DeletedCalls := 0;
	RenamedCalls := 0;
	DownloadedCalls := 0;
	UploadedCalls := 0;
	OnFileDeletedCalled := False;
	OnFileRenamedCalled := False;
	OnFileDownloadedCalled := False;
	OnFileUploadedCalled := False;
	LastLocalPath := '';
end;

procedure TMockDescriptionSyncGuard.OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
begin
	Inc(DeletedCalls);
	OnFileDeletedCalled := True;
	LastDeletedPath := RealPath;
end;

procedure TMockDescriptionSyncGuard.OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
begin
	Inc(RenamedCalls);
	OnFileRenamedCalled := True;
	LastOldPath := OldPath;
	LastNewPath := NewPath;
end;

procedure TMockDescriptionSyncGuard.OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
	Inc(DownloadedCalls);
	OnFileDownloadedCalled := True;
	LastRemotePath := RealPath;
	LastLocalPath := LocalPath;
end;

procedure TMockDescriptionSyncGuard.OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
	Inc(UploadedCalls);
	OnFileUploadedCalled := True;
	LastRemotePath := RealPath;
	LastLocalPath := LocalPath;
end;

{TMockTimestampSyncGuard}

constructor TMockTimestampSyncGuard.Create;
begin
	inherited Create;
	UploadedCalls := 0;
	DownloadedCalls := 0;
	DeletedCalls := 0;
	RenamedCalls := 0;
	OnFileRenamedCalled := False;
	DownloadReturnValue := 0;
end;

procedure TMockTimestampSyncGuard.OnFileUploaded(const RemotePath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
	Inc(UploadedCalls);
end;

function TMockTimestampSyncGuard.OnFileDownloaded(const RemotePath: TRealPath; const LocalPath: WideString; CloudMTime: Int64; Cloud: TCloudMailRu): Int64;
begin
	Inc(DownloadedCalls);
	Result := DownloadReturnValue;
end;

procedure TMockTimestampSyncGuard.OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
begin
	Inc(DeletedCalls);
end;

procedure TMockTimestampSyncGuard.OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
begin
	Inc(RenamedCalls);
	OnFileRenamedCalled := True;
end;

end.

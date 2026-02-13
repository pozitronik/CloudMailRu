unit PluginSettings;

interface

uses
	SettingsConstants,
	ConnectionSettings,
	AccountSettings;

type
	TPluginSettings = record
		ConnectionSettings: TConnectionSettings;
		IniDir: TIniDirTypes; {See INI_DIR_* constants}
		LoadSSLDLLOnlyFromPluginDir: boolean;
		DescriptionEnabled: boolean;
		DescriptionEditorEnabled: boolean;
		DescriptionCopyToCloud: boolean;
		DescriptionCopyFromCloud: boolean;
		DescriptionTrackCloudFS: boolean;
		DescriptionFileName: WideString;
		TimestampMode: integer;              {See TimestampMode* constants}
		TimestampFileName: WideString;       {Metadata filename, default: .cloud_timestamps}
		TimestampConflictMode: integer;      {See TimestampConflict* constants}
		CopyBetweenAccountsMode: integer;
		ChunkOverwriteMode: integer;
		DeleteFailOnUploadMode: integer;
		OperationErrorMode: integer;
		RetryAttempts: integer;
		AttemptWait: integer;
		OverwriteLocalMode: integer;
		DisableMultiThreading: boolean;
		LogUserSpace: boolean;
		IconsMode: integer;
		ShowTrashFolders: boolean;
		ShowSharedFolders: boolean;
		ShowInvitesFolders: boolean;
		LogLevel: integer;
		PrecalculateHash: boolean;
		ForcePrecalculateSize: int64;
		CheckCRC: boolean;
		HashCalculatorStrategy: integer;
		SSLBackend: integer; {See SSLBackend* constants - selects SSL/TLS implementation}
		ThumbnailExtensions: WideString;
		Language: WideString; {Selected translation name, empty = English default}
		FileHistoryEnabled: boolean; {Show version history tab in file properties dialog}
		HideDescriptionFile: boolean; {Hide descript.ion from cloud directory listings}
		HideTimestampFile: boolean; {Hide .cloud_timestamps from cloud directory listings}
		SkipDescriptionDownload: boolean; {Skip downloading descript.ion files}
		SkipTimestampDownload: boolean; {Skip downloading .cloud_timestamps files}
	private
		FIniFilePath: WideString;
		FAccountsIniFilePath: WideString;
		FThumbnailExtList: TArray<string>; {Pre-parsed, lowercased, sorted for binary search}
		function GetEnabledVirtualTypes: EVirtualType;
	public
		{Parse ThumbnailExtensions string into sorted lookup array. Call after setting ThumbnailExtensions.}
		procedure BuildThumbnailExtList;
		{Check if the given file extension is in the thumbnail extensions list. O(log n).}
		function IsThumbnailExtension(const Ext: WideString): boolean;
		property IniFilePath: WideString read FIniFilePath write FIniFilePath;
		property AccountsIniFilePath: WideString read FAccountsIniFilePath write FAccountsIniFilePath;
		property EnabledVirtualTypes: EVirtualType read GetEnabledVirtualTypes;
	end;

implementation

uses
	SysUtils,
	Generics.Collections;

{TPluginSettings}

procedure TPluginSettings.BuildThumbnailExtList;
var
	Raw: TArray<string>;
	I: integer;
begin
	Raw := string(ThumbnailExtensions).Split([',']);
	SetLength(FThumbnailExtList, Length(Raw));
	for I := 0 to High(Raw) do
		FThumbnailExtList[I] := LowerCase(Trim(Raw[I]));
	TArray.Sort<string>(FThumbnailExtList);
end;

function TPluginSettings.IsThumbnailExtension(const Ext: WideString): boolean;
var
	FoundIndex: integer;
begin
	if (Ext = '') or (Length(FThumbnailExtList) = 0) then
		Exit(False);
	Result := TArray.BinarySearch<string>(FThumbnailExtList, LowerCase(Ext), FoundIndex);
end;

function TPluginSettings.GetEnabledVirtualTypes: EVirtualType;
begin
	Result := [];
	if ShowTrashFolders then
		Result := Result + [VTTrash];
	if ShowSharedFolders then
		Result := Result + [VTShared];
	if ShowInvitesFolders then
		Result := Result + [VTInvites];
end;

end.

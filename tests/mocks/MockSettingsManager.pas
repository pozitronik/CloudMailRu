unit MockSettingsManager;

{Shared mock IPluginSettingsManager for testing. Provides convenience setters
	for all commonly tested settings fields and observable state tracking.}

interface

uses
	System.SysUtils,
	System.Classes,
	PluginSettingsManager,
	PluginSettings,
	StreamingSettings;

type
	TMockSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FSettings: TPluginSettings;
		FSaveCalled: Boolean;
		FSwitchProxyPasswordStorageCalled: Boolean;
	public
		constructor Create;

		{IPluginSettingsManager}
		function GetSettings: TPluginSettings;
		procedure SetSettings(Value: TPluginSettings);
		procedure Save;
		procedure SwitchProxyPasswordStorage;
		function GetStreamingSettings(const FileName: WideString): TStreamingSettings;
		procedure SetStreamingSettings(const FileName: WideString; StreamSettings: TStreamingSettings);
		procedure GetStreamingExtensionsList(ExtensionsList: TStrings);
		procedure RemoveStreamingExtension(const Extension: WideString);
		function GetAccountsIniFilePath: WideString;
		procedure Refresh;

		{Convenience setters -- modify specific fields without read-modify-write}
		procedure SetDescriptionTrackCloudFS(Value: Boolean);
		procedure SetDescriptionCopyFromCloud(Value: Boolean);
		procedure SetDescriptionCopyToCloud(Value: Boolean);
		procedure SetTimestampMode(Value: Integer);
		procedure SetOperationErrorMode(Mode: Integer);
		procedure SetRetryAttempts(Attempts: Integer);
		procedure SetAttemptWait(WaitMs: Integer);
		procedure SetCheckCRC(Value: Boolean);
		procedure SetDeleteMode(Mode: Integer);
		procedure SetLogUserSpace(Value: Boolean);
		procedure SetHideDescriptionFile(Value: Boolean);
		procedure SetHideTimestampFile(Value: Boolean);
		procedure SetSkipDescriptionDownload(Value: Boolean);
		procedure SetSkipTimestampDownload(Value: Boolean);

		{Observable state}
		property SaveCalled: Boolean read FSaveCalled;
		property SwitchProxyPasswordStorageCalled: Boolean read FSwitchProxyPasswordStorageCalled;
	end;

implementation

constructor TMockSettingsManager.Create;
begin
	inherited Create;
	FSaveCalled := False;
	FSwitchProxyPasswordStorageCalled := False;
end;

function TMockSettingsManager.GetSettings: TPluginSettings;
begin
	Result := FSettings;
end;

procedure TMockSettingsManager.SetSettings(Value: TPluginSettings);
begin
	FSettings := Value;
end;

procedure TMockSettingsManager.Save;
begin
	FSaveCalled := True;
end;

procedure TMockSettingsManager.SwitchProxyPasswordStorage;
begin
	FSwitchProxyPasswordStorageCalled := True;
end;

function TMockSettingsManager.GetStreamingSettings(const FileName: WideString): TStreamingSettings;
begin
	Result := Default(TStreamingSettings);
end;

procedure TMockSettingsManager.SetStreamingSettings(const FileName: WideString; StreamSettings: TStreamingSettings);
begin
	{No-op}
end;

procedure TMockSettingsManager.GetStreamingExtensionsList(ExtensionsList: TStrings);
begin
	ExtensionsList.Clear;
end;

procedure TMockSettingsManager.RemoveStreamingExtension(const Extension: WideString);
begin
	{No-op}
end;

function TMockSettingsManager.GetAccountsIniFilePath: WideString;
begin
	Result := EmptyWideStr;
end;

procedure TMockSettingsManager.Refresh;
begin
	{No-op}
end;

{Convenience setters}

procedure TMockSettingsManager.SetDescriptionTrackCloudFS(Value: Boolean);
begin
	FSettings.DescriptionTrackCloudFS := Value;
end;

procedure TMockSettingsManager.SetDescriptionCopyFromCloud(Value: Boolean);
begin
	FSettings.DescriptionCopyFromCloud := Value;
end;

procedure TMockSettingsManager.SetDescriptionCopyToCloud(Value: Boolean);
begin
	FSettings.DescriptionCopyToCloud := Value;
end;

procedure TMockSettingsManager.SetTimestampMode(Value: Integer);
begin
	FSettings.TimestampMode := Value;
end;

procedure TMockSettingsManager.SetOperationErrorMode(Mode: Integer);
begin
	FSettings.OperationErrorMode := Mode;
end;

procedure TMockSettingsManager.SetRetryAttempts(Attempts: Integer);
begin
	FSettings.RetryAttempts := Attempts;
end;

procedure TMockSettingsManager.SetAttemptWait(WaitMs: Integer);
begin
	FSettings.AttemptWait := WaitMs;
end;

procedure TMockSettingsManager.SetCheckCRC(Value: Boolean);
begin
	FSettings.CheckCRC := Value;
end;

procedure TMockSettingsManager.SetDeleteMode(Mode: Integer);
begin
	FSettings.DeleteFailOnUploadMode := Mode;
end;

procedure TMockSettingsManager.SetLogUserSpace(Value: Boolean);
begin
	FSettings.LogUserSpace := Value;
end;

procedure TMockSettingsManager.SetHideDescriptionFile(Value: Boolean);
begin
	FSettings.HideDescriptionFile := Value;
end;

procedure TMockSettingsManager.SetHideTimestampFile(Value: Boolean);
begin
	FSettings.HideTimestampFile := Value;
end;

procedure TMockSettingsManager.SetSkipDescriptionDownload(Value: Boolean);
begin
	FSettings.SkipDescriptionDownload := Value;
end;

procedure TMockSettingsManager.SetSkipTimestampDownload(Value: Boolean);
begin
	FSettings.SkipTimestampDownload := Value;
end;

end.

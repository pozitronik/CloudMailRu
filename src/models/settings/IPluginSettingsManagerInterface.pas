unit IPluginSettingsManagerInterface;

{Interface for plugin settings management, decoupled from INI file implementation}

interface

uses
	PluginSettings;

type
	IPluginSettingsManager = interface
		['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
		function GetSettings: TPluginSettings;
		procedure SwitchProxyPasswordStorage;
	end;

	{Null implementation for testing - returns defaults, no-op for writes}
	TNullPluginSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FSettings: TPluginSettings;
	public
		function GetSettings: TPluginSettings;
		procedure SwitchProxyPasswordStorage;
	end;

implementation

{TNullPluginSettingsManager}

function TNullPluginSettingsManager.GetSettings: TPluginSettings;
begin
	Result := FSettings;
end;

procedure TNullPluginSettingsManager.SwitchProxyPasswordStorage;
begin
	{No-op for null implementation}
end;

end.

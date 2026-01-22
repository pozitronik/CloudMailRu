unit CloudMailRuFactory;

{Factory for creating TCloudMailRu instances with specific configurations.
	Centralizes instance creation logic that was previously scattered in static methods.}

interface

uses
	CloudMailRu;

type
	{Factory for creating TCloudMailRu instances.
		Provides specialized constructors for different use cases.}
	TCloudMailRuFactory = class
	public
		{Create a temporary TCloudMailRu for accessing public/shared content.
			Configures minimal dependencies for public URL resolution.
			@param TempCloud Output parameter that receives the created instance
			@param PublicUrl The public URL to access
			@return True if login succeeded, False otherwise}
		class function CreatePublicCloud(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean; static;
	end;

implementation

uses
	CloudSettings,
	IAuthStrategyInterface,
	WindowsFileSystem,
	TCLogger,
	TCProgress,
	TCRequest;

class function TCloudMailRuFactory.CreatePublicCloud(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean;
var
	Settings: TCloudSettings;
begin
	Settings := Default(TCloudSettings);
	Settings.AccountSettings.PublicAccount := True;
	Settings.AccountSettings.PublicUrl := PublicUrl;
	TempCloud := TCloudMailRu.Create(
		Settings,
		nil,
		TNullAuthStrategy.Create,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create);
	Result := TempCloud.Login;
end;

end.

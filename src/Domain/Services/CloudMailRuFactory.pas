unit CloudMailRuFactory;

{Factory for creating TCloudMailRu instances with specific configurations.
	Centralizes instance creation logic that was previously scattered in static methods.}

interface

uses
	CloudMailRu;

type
	{Interface for creating public cloud instances.
		Enables dependency injection and testing of components that need temporary clouds.}
	IPublicCloudFactory = interface
		['{A7D3E8B1-5C2F-4A9D-8E6B-1F3C5D7A9B2E}']

		{Create a temporary TCloudMailRu for accessing public/shared content.
			@param TempCloud Output parameter that receives the created instance
			@param PublicUrl The public URL to access
			@return True if login succeeded, False otherwise}
		function CreatePublicCloud(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean;
	end;

	{Default implementation using real cloud connections.}
	TPublicCloudFactory = class(TInterfacedObject, IPublicCloudFactory)
	public
		function CreatePublicCloud(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean;
	end;

	{Static factory for creating TCloudMailRu instances.
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
	Winapi.Windows,
	CloudSettings,
	AuthStrategy,
	FileCipher,
	WindowsFileSystem,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	HTTPManager,
	OpenSSLProvider;

{TPublicCloudFactory - default implementation}

function TPublicCloudFactory.CreatePublicCloud(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean;
begin
	Result := TCloudMailRuFactory.CreatePublicCloud(TempCloud, PublicUrl);
end;

{TCloudMailRuFactory - static methods}

class function TCloudMailRuFactory.CreatePublicCloud(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean;
var
	Settings: TCloudSettings;
	Logger: ILogger;
	Progress: IProgress;
begin
	Settings := Default(TCloudSettings);
	Settings.AccountSettings.PublicAccount := True;
	Settings.AccountSettings.PublicUrl := PublicUrl;
	Logger := TNullLogger.Create;
	Progress := TNullProgress.Create;
	TempCloud := TCloudMailRu.Create(
		Settings,
		TSingleThreadHTTPManager.Create(Settings.ConnectionSettings, Logger, Progress),
		function: TThreadID begin Result := GetCurrentThreadID; end,
		TNullAuthStrategy.Create,
		TWindowsFileSystem.Create,
		Logger,
		Progress,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create,
		TNullOpenSSLProvider.Create);
	Result := TempCloud.Login;
end;

end.

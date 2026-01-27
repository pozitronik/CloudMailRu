unit CloudMailRuInterface;

{Interface definition for cloud connections.}

interface

uses
	CloudAuthorizationState,
	CloudFileDownloader,
	CloudFileUploader,
	CloudShareService,
	CloudListingService,
	CloudFileOperations;

type
	ICloudMailRu = interface
		['{C9BF2862-7907-4E1C-AB48-DA95A5E9F3A3}']

		{Authorization state - indicates if cloud is ready for operations}
		function GetAuthorizationState: TAuthorizationState;
		{Authorization error details when state is asFailed}
		function GetAuthorizationError: TAuthorizationError;
		{Attempt authorization. Returns True on success.
			State transitions: asPending/asFailed -> asAuthorizing -> asAuthorized/asFailed}
		function Authorize: Boolean;
		{Reset authorization state to asPending for re-authentication.
			Use when token expires or user requests reconnect.}
		procedure InvalidateAuthorization;

		{Service accessors - only valid when AuthorizationState = asAuthorized}
		function GetDownloader: ICloudFileDownloader;
		function GetUploader: ICloudFileUploader;
		function GetShareService: ICloudShareService;
		function GetListingService: ICloudListingService;
		function GetFileOps: ICloudFileOperations;
		function GetIsPublicAccount: Boolean;

		{Properties}
		property AuthorizationState: TAuthorizationState read GetAuthorizationState;
		property AuthorizationError: TAuthorizationError read GetAuthorizationError;
		property Downloader: ICloudFileDownloader read GetDownloader;
		property Uploader: ICloudFileUploader read GetUploader;
		property ShareService: ICloudShareService read GetShareService;
		property ListingService: ICloudListingService read GetListingService;
		property FileOps: ICloudFileOperations read GetFileOps; //Todo: rename property to FileOperations
		property IsPublicAccount: Boolean read GetIsPublicAccount;
	end;

implementation

end.

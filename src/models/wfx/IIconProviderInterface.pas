unit IIconProviderInterface;

interface

uses
	CMRDirItem, CMRIncomingInvite, RealPath;

type
	TIconType = (
		itUseDefault,       { Use TC default icon }
		itSystemTrash,      { System trash icon }
		itInternal,         { Load from DLL resources }
		itInternalOverlay,  { Load from resources + combine with folder }
		itExternal,         { Load from external .ico file }
		itExternalOverlay   { Load external + combine with folder }
	);

	{ Result of icon determination }
	TIconInfo = record
		IconType: TIconType;
		IconName: WideString;
		class function UseDefault: TIconInfo; static;
		class function SystemTrash: TIconInfo; static;
		class function Create(AType: TIconType; const AName: WideString): TIconInfo; static;
	end;

	{ Context for icon determination - provides all external data needed }
	TIconContext = record
		IconsMode: Integer;
		IsPublicAccount: Boolean;
		Item: TCMRDirItem;
		InviteItem: TCMRIncomingInvite;
		HasItem: Boolean;
		HasInviteItem: Boolean;
	end;

	{ Determines what icon to display based on path and context.
	  Separates icon selection logic from icon loading/rendering. }
	IIconProvider = interface
		['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
		function GetIcon(const RealPath: TRealPath; const Context: TIconContext): TIconInfo;
	end;

implementation

class function TIconInfo.UseDefault: TIconInfo;
begin
	Result.IconType := itUseDefault;
	Result.IconName := '';
end;

class function TIconInfo.SystemTrash: TIconInfo;
begin
	Result.IconType := itSystemTrash;
	Result.IconName := 'cloud_trash';
end;

class function TIconInfo.Create(AType: TIconType; const AName: WideString): TIconInfo;
begin
	Result.IconType := AType;
	Result.IconName := AName;
end;

end.

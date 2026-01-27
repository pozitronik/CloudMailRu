unit IconProvider;

{Determines what icon to display based on path and context.
	Does NOT load icons - only determines icon name and render mode.
	Separates icon selection logic from icon loading/rendering.}

interface

uses
	SysUtils,
	CMRDirItem, CMRIncomingInvite, CMRConstants, RealPath,
	SettingsConstants;

type
	TIconType = (itUseDefault, {Use TC default icon}
		itSystemTrash, {System trash icon}
		itInternal, {Load from DLL resources}
		itInternalOverlay, {Load from resources + combine with folder}
		itExternal, {Load from external .ico file}
		itExternalOverlay {Load external + combine with folder}
		);

	{Result of icon determination}
	TIconInfo = record
		IconType: TIconType;
		IconName: WideString;
		class function UseDefault: TIconInfo; static;
		class function SystemTrash: TIconInfo; static;
		class function Create(AType: TIconType; const AName: WideString): TIconInfo; static;
	end;

	{Context for icon determination - provides all external data needed}
	TIconContext = record
		IconsMode: Integer;
		IsPublicAccount: Boolean;
		Item: TCMRDirItem;
		InviteItem: TCMRIncomingInvite;
		HasItem: Boolean;
		HasInviteItem: Boolean;
	end;

	{Determines what icon to display based on path and context.
		Separates icon selection logic from icon loading/rendering.}
	IIconProvider = interface
		['{21958F24-1F8B-4816-80F5-1F8D4450C8B1}']
		function GetIcon(const RealPath: TRealPath; const Context: TIconContext): TIconInfo;
	end;

	{Determines what icon to display based on path and context.
		Does NOT load icons - only determines icon name and render mode.}
	TIconProvider = class(TInterfacedObject, IIconProvider)
	private
		function GetIconForTrashRoot: TIconInfo;
		function GetIconForSharedRoot: TIconInfo;
		function GetIconForInvitesRoot: TIconInfo;
		function GetIconForInviteItem(const InviteItem: TCMRIncomingInvite): TIconInfo;
		function GetIconForAccountRoot(IsPublicAccount: Boolean; IconsMode: Integer): TIconInfo;
		function GetIconForDirectoryItem(const Item: TCMRDirItem; IconsMode: Integer): TIconInfo;
		function ApplyRenderMode(const IconName: WideString; IconsMode: Integer): TIconInfo;
	public
		function GetIcon(const RealPath: TRealPath; const Context: TIconContext): TIconInfo;
	end;

implementation

{TIconInfo}

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

{TIconProvider}

function TIconProvider.GetIcon(const RealPath: TRealPath; const Context: TIconContext): TIconInfo;
var
	EffectiveIconsMode: Integer;
begin
	{Skip updir items}
	if RealPath.upDirItem then
		exit(TIconInfo.UseDefault);

	EffectiveIconsMode := Context.IconsMode;

	{Trash directory at account root - always show system trash icon}
	if RealPath.trashDir and RealPath.isInAccountsList then
		exit(GetIconForTrashRoot);

	{Shared directory}
	if RealPath.sharedDir then
	begin
		if RealPath.isInAccountsList then
			exit(GetIconForSharedRoot)
		else
			{Force overlay mode inside shared directory}
			if EffectiveIconsMode = IconsModeDisabled then
				EffectiveIconsMode := IconsModeInternalOverlay;
	end;

	{Invites directory}
	if RealPath.invitesDir then
	begin
		if RealPath.isInAccountsList then
			exit(GetIconForInvitesRoot)
		else if Context.HasInviteItem then
			exit(GetIconForInviteItem(Context.InviteItem))
		else
			exit(TIconInfo.UseDefault);
	end;

	{Icons disabled - use default}
	if EffectiveIconsMode = IconsModeDisabled then
		exit(TIconInfo.UseDefault);

	{Account list (root level)}
	if RealPath.isInAccountsList then
		exit(GetIconForAccountRoot(Context.IsPublicAccount, EffectiveIconsMode));

	{Regular directory items}
	if Context.HasItem then
		exit(GetIconForDirectoryItem(Context.Item, EffectiveIconsMode));

	Result := TIconInfo.UseDefault;
end;

function TIconProvider.GetIconForTrashRoot: TIconInfo;
begin
	Result := TIconInfo.SystemTrash;
end;

function TIconProvider.GetIconForSharedRoot: TIconInfo;
begin
	Result := TIconInfo.Create(itInternalOverlay, 'shared');
end;

function TIconProvider.GetIconForInvitesRoot: TIconInfo;
begin
	Result := TIconInfo.Create(itInternalOverlay, 'shared_incoming');
end;

function TIconProvider.GetIconForInviteItem(const InviteItem: TCMRIncomingInvite): TIconInfo;
begin
	if InviteItem.name = '' then
		exit(TIconInfo.UseDefault);

	if InviteItem.isMounted then
		Result := TIconInfo.Create(itInternalOverlay, 'shared_incoming')
	else
		Result := TIconInfo.Create(itInternalOverlay, 'shared');
end;

function TIconProvider.GetIconForAccountRoot(IsPublicAccount: Boolean; IconsMode: Integer): TIconInfo;
begin
	if IsPublicAccount then
		Result := ApplyRenderMode('cloud_public', IconsMode)
	else
		Result := ApplyRenderMode('cloud', IconsMode);
end;

function TIconProvider.GetIconForDirectoryItem(const Item: TCMRDirItem; IconsMode: Integer): TIconInfo;
begin
	{Only directories and shared items get custom icons}
	if (Item.type_ <> TYPE_DIR) and (Item.kind <> KIND_SHARED) then
		exit(TIconInfo.UseDefault);

	if Item.kind = KIND_SHARED then
		Result := ApplyRenderMode('shared', IconsMode)
	else if Item.isPublished then
		Result := ApplyRenderMode('shared_public', IconsMode)
	else
		Result := TIconInfo.UseDefault;
end;

function TIconProvider.ApplyRenderMode(const IconName: WideString; IconsMode: Integer): TIconInfo;
begin
	case IconsMode of
		IconsModeInternal:
			Result := TIconInfo.Create(itInternal, IconName);
		IconsModeInternalOverlay:
			Result := TIconInfo.Create(itInternalOverlay, IconName);
		IconsModeExternal:
			Result := TIconInfo.Create(itExternal, IconName);
		IconsModeExternalOverlay:
			Result := TIconInfo.Create(itExternalOverlay, IconName);
		else
			Result := TIconInfo.UseDefault;
	end;
end;

end.

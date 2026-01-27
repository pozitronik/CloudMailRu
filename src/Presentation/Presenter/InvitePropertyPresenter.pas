unit InvitePropertyPresenter;

{Presenter for InviteProperty dialog - handles display logic for incoming invites.
 Follows MVP pattern: View (TInvitePropertyForm) implements IInvitePropertyView,
 Presenter orchestrates display, Model is TCMRIncomingInvite record.}

interface

uses
	CMRIncomingInvite,
	CMRConstants,
	CloudAccessUtils,
	StringHelper,
	LanguageStrings,
	System.SysUtils;

type
	{View interface for InviteProperty dialog}
	IInvitePropertyView = interface
		['{6BEC77FE-533D-444D-9CA5-E7DA3874BB20}']
		{Form display}
		procedure SetCaption(Caption: WideString);

		{Item properties}
		procedure SetItemName(Name: WideString);
		procedure SetOwnerEmail(Email: WideString);
		procedure SetOwnerName(Name: WideString);
		procedure SetAccess(Access: WideString);
		procedure SetSize(Size: WideString);

		{Token/mount path display}
		procedure SetTokenLabel(LabelText: WideString);
		procedure SetTokenValue(Value: WideString);

		{Button state}
		procedure SetMountEnabled(Enabled: Boolean);
		procedure SetRejectEnabled(Enabled: Boolean);
		procedure SetUnmountCopyEnabled(Enabled: Boolean);
		procedure SetUnmountDeleteEnabled(Enabled: Boolean);
	end;

	{Presenter for InviteProperty dialog}
	TInvitePropertyPresenter = class
	private
		FView: IInvitePropertyView;
		FItem: TCMRIncomingInvite;
		FAccountName: WideString;
	public
		constructor Create(View: IInvitePropertyView);

		{Initialize view state based on invite item}
		procedure Initialize(Item: TCMRIncomingInvite; AccountName: WideString);

		{Properties}
		property Item: TCMRIncomingInvite read FItem;
		property AccountName: WideString read FAccountName;
	end;

implementation

const
	{Default label for invite token - matches DFM default value}
	INVITE_TOKEN_LABEL = 'Invite token:';

{TInvitePropertyPresenter}

constructor TInvitePropertyPresenter.Create(View: IInvitePropertyView);
begin
	inherited Create;
	FView := View;
end;

procedure TInvitePropertyPresenter.Initialize(Item: TCMRIncomingInvite; AccountName: WideString);
begin
	FItem := Item;
	FAccountName := AccountName;

	{Set item properties}
	FView.SetItemName(Item.name);
	FView.SetOwnerEmail(Item.owner.email);
	FView.SetOwnerName(Item.owner.name);
	FView.SetAccess(TCloudAccessUtils.AccessToString(Item.access));
	FView.SetSize(FormatSize(Item.size, TYPE_BYTES));
	FView.SetCaption(Format(INVITE_FORM_TITLE, [AccountName, Item.name]));

	{Configure view based on mount state}
	if Item.isMounted then
	begin
		{Already mounted: show mount path, disable reject}
		FView.SetTokenLabel(MOUNTED_AS);
		FView.SetTokenValue(Item.home);
		FView.SetRejectEnabled(False);
		FView.SetMountEnabled(True);
		FView.SetUnmountCopyEnabled(True);
		FView.SetUnmountDeleteEnabled(True);
	end
	else
	begin
		{Not mounted: show invite token, disable unmount buttons}
		FView.SetTokenLabel(INVITE_TOKEN_LABEL);
		FView.SetTokenValue(Item.invite_token);
		FView.SetMountEnabled(True);
		FView.SetRejectEnabled(True);
		FView.SetUnmountCopyEnabled(False);
		FView.SetUnmountDeleteEnabled(False);
	end;
end;

end.

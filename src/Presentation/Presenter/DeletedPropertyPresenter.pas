unit DeletedPropertyPresenter;

{Presenter for DeletedProperty dialog - handles display logic for deleted (trash) items.
 Follows MVP pattern: View (TDeletedPropertyForm) implements IDeletedPropertyView,
 Presenter orchestrates display, Model is TCMRDirItemList record.}

interface

uses
	CMRDirItemList,
	CMRDirItem,
	CMRConstants,
	SETTINGS_CONSTANTS,
	LANGUAGE_STRINGS,
	PluginHelper,
	DateUtils,
	System.SysUtils;

type
	{View interface for DeletedProperty dialog}
	IDeletedPropertyView = interface
		['{C3D4E5F6-A7B8-9012-CDEF-345678901234}']
		{Form display}
		procedure SetCaption(Caption: WideString);

		{Item properties}
		procedure SetName(Name: WideString);
		procedure SetDeletedFrom(Path: WideString);
		procedure SetDeletedAt(DateTime: WideString);
		procedure SetDeletedBy(UserId: WideString);
		procedure SetSize(Size: WideString);

		{Button state}
		procedure SetRestoreEnabled(Enabled: Boolean);
		procedure SetRestoreAllEnabled(Enabled: Boolean);
		procedure SetEmptyEnabled(Enabled: Boolean);
	end;

	{Presenter for DeletedProperty dialog}
	TDeletedPropertyPresenter = class
	private
		FView: IDeletedPropertyView;
		FItems: TCMRDirItemList;
		FIsTrashDir: Boolean;
		FAccountName: WideString;

		{Calculate total size of all items}
		function CalculateSummarySize: Int64;
	public
		constructor Create(View: IDeletedPropertyView);

		{Initialize view state based on deleted items}
		procedure Initialize(Items: TCMRDirItemList; TrashDir: Boolean; AccountName: WideString);

		{Properties}
		property Items: TCMRDirItemList read FItems;
		property IsTrashDir: Boolean read FIsTrashDir;
		property AccountName: WideString read FAccountName;
	end;

implementation

{TDeletedPropertyPresenter}

constructor TDeletedPropertyPresenter.Create(View: IDeletedPropertyView);
begin
	inherited Create;
	FView := View;
end;

function TDeletedPropertyPresenter.CalculateSummarySize: Int64;
var
	Item: TCMRDirItem;
begin
	Result := 0;
	for Item in FItems do
		Result := Result + Item.size;
end;

procedure TDeletedPropertyPresenter.Initialize(Items: TCMRDirItemList; TrashDir: Boolean; AccountName: WideString);
var
	FormCaption, NameCaption, FromCaption, AtCaption, ByCaption, SizeCaption: WideString;
begin
	FItems := Items;
	FIsTrashDir := TrashDir;
	FAccountName := AccountName;

	{Set default enabled states}
	FView.SetRestoreEnabled(True);
	FView.SetRestoreAllEnabled(False);
	FView.SetEmptyEnabled(True);

	if Length(Items) = 0 then
	begin
		{Empty trash}
		NameCaption := EMPTY;
		FormCaption := Format(ACCOUNT_TRASH, [AccountName]);
		SizeCaption := EmptyWideStr;
		FromCaption := EmptyWideStr;
		AtCaption := EmptyWideStr;
		ByCaption := EmptyWideStr;
		FView.SetRestoreEnabled(False);
		FView.SetRestoreAllEnabled(False);
		FView.SetEmptyEnabled(False);
	end
	else if Length(Items) = 1 then
	begin
		{Single item: show full details}
		NameCaption := Items[0].name;
		FromCaption := Items[0].deleted_from;
		AtCaption := DateTimeToStr(UnixToDateTime(Items[0].deleted_at));
		ByCaption := Items[0].deleted_by.ToString;
		SizeCaption := FormatSize(Items[0].size, TYPE_BYTES);
		FormCaption := Format(DELETED_ITEM, [NameCaption]);
		FView.SetRestoreAllEnabled(False);
	end
	else
	begin
		{Multiple items: show summary}
		NameCaption := MULTIPLE_ITEMS;
		FromCaption := UNSET_ITEM;
		AtCaption := UNSET_ITEM;
		ByCaption := UNSET_ITEM;
		SizeCaption := FormatSize(CalculateSummarySize, TYPE_BYTES);
		FormCaption := MULTIPLE_ITEMS_DELETED;
	end;

	if TrashDir then
	begin
		{Properties for trash directory itself: allow Empty/RestoreAll/Cancel}
		FormCaption := AccountName + TrashPostfix;
		NameCaption := FormCaption;
		FromCaption := UNSET_ITEM;
		AtCaption := UNSET_ITEM;
		ByCaption := UNSET_ITEM;
		FView.SetRestoreEnabled(False);
		FView.SetRestoreAllEnabled(True);
	end;

	FView.SetCaption(FormCaption);
	FView.SetName(NameCaption);
	FView.SetDeletedFrom(FromCaption);
	FView.SetDeletedAt(AtCaption);
	FView.SetDeletedBy(ByCaption);
	FView.SetSize(SizeCaption);
end;

end.

unit AskPasswordPresenter;

{Presenter for AskPassword dialog - handles display logic for password/text input dialogs.
 Follows MVP pattern: View (TAskPasswordForm) implements IAskPasswordView,
 Presenter orchestrates display. Supports three modes: password, text, and action selection.}

interface

uses
	System.Generics.Collections,
	System.SysUtils,
	LanguageStrings;

type
	{Describes a button to be added to the dialog}
	TActionButton = record
		Code: Integer;
		Title: WideString;
	end;

	{View interface for AskPassword dialog}
	IAskPasswordView = interface
		['{3D57E60B-BC87-4742-A15A-43EDFFC59D9B}']
		{Form display}
		procedure SetCaption(Caption: WideString);
		procedure SetLabelText(Text: WideString);

		{Password edit control}
		procedure SetPasswordVisible(Visible: Boolean);
		procedure SetPasswordChar(Ch: Char);
		function GetPassword: WideString;

		{OK button}
		procedure SetOkButtonVisible(Visible: Boolean);
		procedure SetOkButtonEnabled(Enabled: Boolean);

		{Checkbox control}
		procedure SetCheckboxVisible(Visible: Boolean);
		procedure SetCheckboxEnabled(Enabled: Boolean);
		procedure SetCheckboxChecked(Checked: Boolean);
		function GetCheckboxChecked: Boolean;

		{Dynamic button creation}
		procedure AddActionButton(Title: WideString; ResultCode: Integer);
	end;

	{Presenter for AskPassword dialog}
	TAskPasswordPresenter = class
	private
		FView: IAskPasswordView;
	public
		constructor Create(View: IAskPasswordView);

		{Initialize for password input mode}
		procedure InitializePasswordMode(Title, LabelText: WideString; UseTCPwdMngr, DisablePWDManagerCB: Boolean);

		{Initialize for plain text input mode}
		procedure InitializeTextMode(Title, LabelText: WideString);

		{Initialize for action selection mode}
		procedure InitializeActionMode(Title, LabelText: WideString; Actions: TDictionary<Integer, WideString>);

		{Called when password text changes - validates and enables/disables OK button}
		procedure OnPasswordChanged(Password: WideString);

		{Get dialog results}
		function GetPassword: WideString;
		function GetUseTCPwdMngr: Boolean;
	end;

implementation

{TAskPasswordPresenter}

constructor TAskPasswordPresenter.Create(View: IAskPasswordView);
begin
	inherited Create;
	FView := View;
end;

procedure TAskPasswordPresenter.InitializePasswordMode(Title, LabelText: WideString; UseTCPwdMngr, DisablePWDManagerCB: Boolean);
begin
	FView.SetCaption(Title);
	FView.SetLabelText(LabelText);
	FView.SetPasswordVisible(True);
	FView.SetPasswordChar('*');
	FView.SetOkButtonVisible(True);
	FView.SetOkButtonEnabled(False);
	FView.SetCheckboxVisible(True);
	FView.SetCheckboxEnabled(not DisablePWDManagerCB);
	FView.SetCheckboxChecked(UseTCPwdMngr);
end;

procedure TAskPasswordPresenter.InitializeTextMode(Title, LabelText: WideString);
begin
	FView.SetCaption(Title);
	FView.SetLabelText(LabelText);
	FView.SetPasswordVisible(True);
	FView.SetPasswordChar(#0);
	FView.SetOkButtonVisible(False);
	FView.SetCheckboxVisible(False);

	{Add OK and Cancel buttons}
	FView.AddActionButton(OK, 1); {mrOk = 1}
	FView.AddActionButton(CANCEL, 2); {mrCancel = 2}
end;

procedure TAskPasswordPresenter.InitializeActionMode(Title, LabelText: WideString; Actions: TDictionary<Integer, WideString>);
var
	ButtonCode: Integer;
begin
	FView.SetCaption(Title);
	FView.SetLabelText(LabelText);
	FView.SetPasswordVisible(False);
	FView.SetOkButtonVisible(False);
	FView.SetCheckboxVisible(False);

	for ButtonCode in Actions.Keys do
		FView.AddActionButton(Actions.Items[ButtonCode], ButtonCode);
end;

procedure TAskPasswordPresenter.OnPasswordChanged(Password: WideString);
begin
	FView.SetOkButtonEnabled(Password <> EmptyWideStr);
end;

function TAskPasswordPresenter.GetPassword: WideString;
begin
	Result := FView.GetPassword;
end;

function TAskPasswordPresenter.GetUseTCPwdMngr: Boolean;
begin
	Result := FView.GetCheckboxChecked;
end;

end.

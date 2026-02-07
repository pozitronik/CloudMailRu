unit AskEncryptionPasswordPresenter;

{Presenter for AskEncryptionPassword dialog - handles display logic for file encryption
 password prompt. Unlike AskPasswordPresenter, this dialog has no close button (X)
 and offers an explicit "No encryption this time" skip button instead of implicit cancel.}

interface

uses
	System.SysUtils,
	LanguageStrings;

type
	{View interface for AskEncryptionPassword dialog}
	IAskEncryptionPasswordView = interface
		['{F4A1B2C3-D5E6-4F78-9A0B-1C2D3E4F5A6B}']
		procedure SetCaption(const Value: WideString);
		procedure SetLabelText(const Value: WideString);
		procedure SetPasswordChar(Value: Char);
		function GetPassword: WideString;
		procedure SetOkButtonEnabled(Value: Boolean);
		procedure SetSkipButtonCaption(const Value: WideString);
	end;

	{Presenter for AskEncryptionPassword dialog}
	TAskEncryptionPasswordPresenter = class
	private
		FView: IAskEncryptionPasswordView;
	public
		constructor Create(View: IAskEncryptionPasswordView);

		{Initialize the dialog for encryption password entry}
		procedure Initialize(Title, Text: WideString);

		{Called when password text changes - validates and enables/disables OK button}
		procedure OnPasswordChanged(Password: WideString);

		{Get dialog results}
		function GetPassword: WideString;
	end;

implementation

{TAskEncryptionPasswordPresenter}

constructor TAskEncryptionPasswordPresenter.Create(View: IAskEncryptionPasswordView);
begin
	inherited Create;
	FView := View;
end;

procedure TAskEncryptionPasswordPresenter.Initialize(Title, Text: WideString);
begin
	FView.SetCaption(Title);
	FView.SetLabelText(Text);
	FView.SetPasswordChar('*');
	FView.SetOkButtonEnabled(False);
	FView.SetSkipButtonCaption(DFM_ASKENC_BTN_SKIP);
end;

procedure TAskEncryptionPasswordPresenter.OnPasswordChanged(Password: WideString);
begin
	FView.SetOkButtonEnabled(Password <> EmptyWideStr);
end;

function TAskEncryptionPasswordPresenter.GetPassword: WideString;
begin
	Result := FView.GetPassword;
end;

end.

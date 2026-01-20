unit PasswordUIProvider;

{Abstraction for password UI dialogs, enabling testability of ConnectionManager
	without requiring actual GUI forms.}

interface

uses
	System.Generics.Collections,
	Windows;

type

	{Interface for password prompts and action dialogs}
	IPasswordUIProvider = interface
		['{706C0432-E3C2-43A4-8B3A-2CD483B6FA26}']
		{Prompts user for a password with optional TC password manager checkbox.
			Returns mrOk on success, mrCancel or other values on cancel/error.
			Password and UseTCPwdMngr are modified only on mrOk result.}
		function AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;

		{Prompts user to choose an action from a list of buttons.
			Returns the button code (e.g., mrYes, mrNo, mrRetry) that was clicked.}
		function AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
	end;

	{Null implementation that always returns cancel/failure.
		Useful for testing scenarios where no UI interaction is expected.}
	TNullPasswordUIProvider = class(TInterfacedObject, IPasswordUIProvider)
	public
		function AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
		function AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
	end;

	{Production implementation of IPasswordUIProvider that delegates to TAskPasswordForm.}
	TPasswordUIProvider = class(TInterfacedObject, IPasswordUIProvider)
	public
		function AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
		function AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
	end;

implementation

uses
	System.UITypes,
	AskPassword;

{TNullPasswordUIProvider}

function TNullPasswordUIProvider.AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
begin
	Result := mrCancel;
end;

function TNullPasswordUIProvider.AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
begin
	Result := mrCancel;
end;

{TPasswordUIProvider}

function TPasswordUIProvider.AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
begin
	Result := TAskPasswordForm.AskPassword(Title, Text, Password, UseTCPwdMngr, DisablePWDManagerCB, ParentWindow);
end;

function TPasswordUIProvider.AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
begin
	Result := TAskPasswordForm.AskAction(Title, Text, ActionsList, ParentWindow);
end;

end.

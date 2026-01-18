unit PasswordUIProvider;

{Production implementation of IPasswordUIProvider that delegates to TAskPasswordForm.}

interface

uses
	IPasswordUIProviderInterface,
	System.Generics.Collections,
	Windows;

type

	TPasswordUIProvider = class(TInterfacedObject, IPasswordUIProvider)
	public
		function AskPassword(Title, Text: WideString; var Password: WideString;
			var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
		function AskAction(Title, Text: WideString;
			ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
	end;

implementation

uses
	AskPassword;

{TPasswordUIProvider}

function TPasswordUIProvider.AskPassword(Title, Text: WideString;
	var Password: WideString; var UseTCPwdMngr: Boolean;
	DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
begin
	Result := TAskPasswordForm.AskPassword(Title, Text, Password, UseTCPwdMngr,
		DisablePWDManagerCB, ParentWindow);
end;

function TPasswordUIProvider.AskAction(Title, Text: WideString;
	ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
begin
	Result := TAskPasswordForm.AskAction(Title, Text, ActionsList, ParentWindow);
end;

end.

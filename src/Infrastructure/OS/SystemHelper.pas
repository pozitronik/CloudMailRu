unit SystemHelper;

{System utilities helper methods}
interface

uses
	Windows;

procedure ProcessMessages;

implementation

procedure ProcessMessages;
var
	Msg: TMsg;
begin
	while true do
	begin
		if not PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
			Break;
		if Msg.Message <> $0012 then
		begin
			TranslateMessage(Msg);
			DispatchMessage(Msg);
		end;
	end;
end;

end.

unit PLUGIN_Main;

{ Функции и процедуры для инициализации и корректной работы плагина }
interface

uses
	Windows,
	SysUtils,
	PLUGIN_Types;

function FindTCWindow: HWND;
{ ------------------------------------------------------------------------------ }

implementation

function FindTCWindow: HWND;
begin
	Result := FindWindow('TTOTAL_CMD', nil); { TODO : При нескольких запущенных тоталах получать нужный хендл }
end;

end.

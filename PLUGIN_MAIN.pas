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

function FindTCWindow: HWND;  { TODO : Вытащить в хелпер }
begin
	Result := FindWindow('TTOTAL_CMD', nil); { Хендл отдаётся корректно даже при нескольких запущенных тоталах }
end;

end.

unit PLUGIN_Main;
{Функции и процедуры для инициализации и корректной работы плагина}
interface
uses
 Windows,
 ShlObj,
 SysUtils,
 PLUGIN_Types;


function FindTCWindow: HWND;
{------------------------------------------------------------------------------}

implementation

function FindTCWindow: HWND;
begin
 Result := FindWindow ('TTOTAL_CMD', nil);
end;

end.

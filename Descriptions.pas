unit Descriptions;

{ Simple && read-only descript.ion files support }
interface

uses
	System.Types, System.AnsiStrings,  Generics.Collections;

type

	TDescription = class

	private
	var
		items: TDictionary<String, String>;

	public
		constructor Create();
		destructor Destroy; override;
		function Read(ion_filename: WideString): integer;
		function GetValue(item: String): String;
		procedure Clear;

	end;

implementation

{ TDescription }

procedure TDescription.Clear;
begin
 self.items.Clear;
end;

constructor TDescription.Create;
begin
	self.items := TDictionary<String, String>.Create;
end;

destructor TDescription.Destroy;
begin
	self.items.Free;
end;

function TDescription.GetValue(item: String): String;
begin
	if not items.TryGetValue(item, result) then result := '';
end;

function TDescription.Read(ion_filename: WideString): integer; // Loads desript.ion file
var
	f: TextFile;
	line, key, value: WideString;
	t: integer;
begin
	self.Clear;
	AssignFile(f, ion_filename);
	Reset(f);
	while not Eof(f) do
	begin
		Readln(f, line);
		if StartsStr('"', line) then
		begin
			t := PosEx('" ', line);
			value := copy(line, t + 2, length(line));
			key := copy(line, 2, t - 2);
		end else begin
			t := PosEx(' ', line);
			value := copy(line, t + 1, length(line));
			key := copy(line, 0, t - 1);
		end;

		items.Add(key, value);
	end;
	CloseFile(f);
end;

end.

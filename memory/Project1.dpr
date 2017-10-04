program Project1;

uses
	Vcl.Forms, Unit1 in 'Unit1.pas'{Form1};

{$R *.res}

begin
	ReportMemoryLeaksOnShutdown := true;
	Application.Initialize;
	Application.MainFormOnTaskbar := true;
	Application.CreateForm(TForm1, Form1);
	Application.Run;

end.

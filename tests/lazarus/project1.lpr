program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  Application.CreateForm(TForm1, Form1);
  Form1.Caption := Form1.Caption + '******** ainda com BUGS qdo comipilado no LAZARUS ******';
  Application.Run;
end.


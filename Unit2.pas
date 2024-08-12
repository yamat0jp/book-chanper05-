unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ToolWin, Vcl.ActnCtrls;

type
  TPi = record
    action: integer;
    prob: Single;
  end;

  TAnswer = TArray<TArray<TPi>>;

  TGridWorld = class
  const
    wid = 4;
    hei = 3;
  private
    FPi: TAnswer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure states(out d: TArray<TPoint>);
    function change(state: TPoint): integer;
  end;

  TAgent = class
  private
    FPi: TAnswer;
    env: TGridWorld;
    function GetPi(state: TPoint): TArray<TPi>;
    procedure SetPi(state: TPoint; const Value: TArray<TPi>);
    function GetQ(state: TPoint; action: integer): Single;
    procedure SetQ(state: TPoint; action: integer; const Value: Single);
  public
    procedure Init; virtual;
    property Pi[state: TPoint]: TArray<TPi> read GetPi write SetPi;
    property Q[state: TPoint; action: integer]: Single read GetQ write SetQ;
  end;

  TMcAgent = class(TAgent)
  const
    gamma = 0.9;
    alpha = 0.01;
  public
    constructor Create;
  end;

  TForm2 = class(TForm)
    ActionManager1: TActionManager;
    Action2: TAction;
    ActionToolBar1: TActionToolBar;
    Action1: TAction;
    procedure Action2Execute(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
    mc_agent: TMcAgent;
    grid_world: TGridWorld;
    render: TAgent;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Action2Execute(Sender: TObject);
begin
  Close;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  grid_world := TGridWorld.Create;
  mc_agent := TMcAgent.Create;
  mc_agent.FPi := grid_world.FPi;
  mc_agent.Init;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  grid_world.Free;
  mc_agent.Free;
end;

procedure TForm2.FormPaint(Sender: TObject);
const
  size = 150;
  margin = 20;
var
  states: TArray<TPoint>;
begin
  for var i := 1 to grid_world.wid + 1 do
  begin
    Canvas.MoveTo(i * size, size);
    Canvas.LineTo(i * size, (grid_world.hei + 1) * size);
  end;
  for var i := 1 to grid_world.hei + 1 do
  begin
    Canvas.MoveTo(size, i * size);
    Canvas.LineTo((grid_world.wid + 1) * size, i * size);
  end;
  for var i := 1 to grid_world.wid do
    for var j := 1 to grid_world.hei do
    begin
      Canvas.MoveTo(i * size, j * size);
      Canvas.LineTo((i + 1) * size, (j + 1) * size);
      Canvas.MoveTo(i * size, (j + 1) * size);
      Canvas.LineTo((i + 1) * size, j * size);
    end;
  grid_world.states(states);
  for var state in states do
    for var prob in mc_agent.Pi[state] do
    begin
      Canvas.TextOut(state.X * size + size div 2, state.Y * size + margin,
        prob.prob.ToString);
      Canvas.TextOut(state.X * size + size div 2, (state.Y + 1) * size - 2 *
        margin, prob.prob.ToString);
      Canvas.TextOut(state.X * size + margin, state.Y * size + size div 2,
        prob.prob.ToString);
      Canvas.TextOut((state.X + 1) * size - 2 * margin,
        state.Y * size + size div 2, prob.prob.ToString);
    end;
  Finalize(states);
end;

{ TGridWorld }

function TGridWorld.change(state: TPoint): integer;
begin
  result := state.X - 1 + (state.Y - 1) * wid;
end;

constructor TGridWorld.Create;
begin
  SetLength(FPi, wid * hei);
end;

destructor TGridWorld.Destroy;
begin
  for var i := 0 to wid * hei - 1 do
    Finalize(FPi[i]);
  Finalize(FPi);
  inherited;
end;

procedure TGridWorld.states(out d: TArray<TPoint>);
begin
  d := [];
  for var j := 0 to hei - 1 do
    for var i := 0 to wid - 1 do
      d := d + [Point(i + 1, j + 1)];
end;

{ TAgent }

function TAgent.GetPi(state: TPoint): TArray<TPi>;
begin
  result := FPi[env.change(state)];
end;

function TAgent.GetQ(state: TPoint; action: integer): Single;
begin

end;

procedure TAgent.Init;
var
  states: TArray<TPoint>;
  random_actions: TArray<TPi>;
  act: TPi;
begin
  SetLength(random_actions, 4);
  for var i := 0 to 3 do
  begin
    act.action := i;
    act.prob := 0.25;
    random_actions[i] := act;
  end;
  env.states(states);
  for var state in states do
    Pi[state] := random_actions;
  Finalize(states);
  Finalize(random_actions);
end;

procedure TAgent.SetPi(state: TPoint; const Value: TArray<TPi>);
begin
  FPi[env.change(state)] := Value;
end;

procedure TAgent.SetQ(state: TPoint; action: integer; const Value: Single);
begin

end;

{ TMcAgent }

constructor TMcAgent.Create;
begin
  inherited;
end;

end.

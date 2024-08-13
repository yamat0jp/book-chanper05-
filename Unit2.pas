unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ToolWin, Vcl.ActnCtrls,
  Vcl.StdCtrls;

type
  TAgentData = record
    state: TPoint;
    action: integer;
    reward: Single;
  end;

  TNextAgentData = record
    next_state: TPoint;
    reward: Single;
    done: Boolean;
  end;

  TAnswer = TArray<TArray<Single>>;

  TGridWorld = class
  const
    wid = 4;
    hei = 3;
  private
    FPi: TAnswer;
    FQ: TAnswer;
    reward_map: TArray<Single>;
    action_space: TArray<integer>;
    action_meaning: TArray<string>;
    goal_state, wall_state, start_state, agent_state: TPoint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure init;
    procedure states(out d: TArray<TPoint>);
    function change(state: TPoint): integer;
    procedure reversed(din: TArray<TAgentData>; out dout: TArray<TAgentData>);
    function nextState(state: TPoint; action: integer): TPoint;
    function step(action: integer): TNextAgentData;
    function reward(state, next_state: TPoint; action: integer): Single;
    procedure reset;
  end;

  TAgent = class
  private
    FEnv: TGridWorld;
    function GetPi(state: TPoint): TArray<Single>;
    procedure SetPi(state: TPoint; const Value: TArray<Single>);
    function GetQ(state: TPoint; action: integer): Single;
    procedure SetQ(state: TPoint; action: integer; const Value: Single);
  protected
    FPi: TAnswer;
    FQ: TAnswer;
    function argmax(d: TArray<Single>): integer;
    property Env: TGridWorld read FEnv write FEnv;
  public
    procedure init; virtual;
    function getAction(state: TPoint): integer; virtual;
    property Pi[state: TPoint]: TArray<Single> read GetPi write SetPi;
    property Q[state: TPoint; action: integer]: Single read GetQ write SetQ;
  end;

  TMcAgent = class(TAgent)
  const
    gamma = 0.9;
    alpha = 0.01;
  private
    memory: TArray<TAgentData>;
  public
    procedure init; override;
    function getAction(state: TPoint): integer; override;
    procedure add(state: TPoint; action: integer; reward: Single);
    procedure update;
    procedure greedy_probs(out action_probs: TArray<Single>; state: TPoint;
      epsilon: Single = 0; action_size: integer = 4);
    procedure reset;
  end;

  TForm2 = class(TForm)
    ActionManager1: TActionManager;
    Action2: TAction;
    ActionToolBar1: TActionToolBar;
    Action1: TAction;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure Action2Execute(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
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

uses Math, Types;

procedure TForm2.Action1Execute(Sender: TObject);
const
  episodes = 10000;
var
  state: TPoint;
  action: integer;
  data: TNextAgentData;
begin
  for var episode := 1 to episodes do
  begin
    grid_world.reset;
    mc_agent.reset;
    state := grid_world.agent_state;

    while True do
    begin
      action := mc_agent.getAction(state);
      data := grid_world.step(action);

      mc_agent.add(state, action, data.reward);
      if data.done then
      begin
        mc_agent.update;
        break;
      end;
      state := data.next_state;
    end;
  end;
  FormPaint(nil);
end;

procedure TForm2.Action2Execute(Sender: TObject);
begin
  Close;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  grid_world := TGridWorld.Create;
  mc_agent := TMcAgent.Create;
  mc_agent.FPi := grid_world.FPi;
  mc_agent.FQ := grid_world.FQ;
  mc_agent.init;
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
  p: TPoint;
  probs: TArray<Single>;
  action: integer;
begin
  Canvas.FillRect(ClientRect);
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
  if RadioButton2.Checked then
  begin
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
    begin
      probs := mc_agent.Pi[state];
      Canvas.TextOut(state.X * size + margin, state.Y * size + size div 2,
        probs[0].ToString(ffFixed, 5, 2));
      Canvas.TextOut((state.X + 1) * size - 2 * margin,
        state.Y * size + size div 2, probs[1].ToString(ffFixed, 5, 2));
      Canvas.TextOut(state.X * size + size div 2, state.Y * size + margin,
        probs[2].ToString(ffFixed, 5, 2));
      Canvas.TextOut(state.X * size + size div 2, (state.Y + 1) * size - 2 *
        margin, probs[3].ToString(ffFixed, 5, 2));
    end;
    Finalize(states);
  end
  else
  begin
    grid_world.states(states);
    for var state in states do
    begin
      action := mc_agent.argmax(mc_agent.Pi[state]);
      Canvas.TextOut(state.X * size + size div 2, state.Y * size + size div 2,
        grid_world.action_meaning[action]);
    end;
    Finalize(states);
  end;
  p := grid_world.wall_state;
  Canvas.Brush.Color := clBlack;
  Canvas.Rectangle(p.X * size, p.Y * size, (p.X + 1) * size, (p.Y + 1) * size);
  Canvas.Brush.Color := Color;
end;

procedure TForm2.RadioButton1Click(Sender: TObject);
begin
  FormPaint(nil);
end;

{ TGridWorld }

function TGridWorld.change(state: TPoint): integer;
begin
  result := state.X - 1 + (state.Y - 1) * wid;
end;

constructor TGridWorld.Create;
begin
  SetLength(FPi, wid * hei);
  SetLength(FQ, wid * hei);
  SetLength(reward_map, wid * hei);
  init;
end;

destructor TGridWorld.Destroy;
begin
  for var i := 0 to wid * hei - 1 do
  begin
    Finalize(FPi[i]);
    Finalize(FQ[i]);
  end;
  Finalize(FPi);
  Finalize(FQ);
  Finalize(action_space);
  Finalize(action_meaning);
  Finalize(reward_map);
  inherited;
end;

procedure TGridWorld.init;
begin
  action_space := [0, 1, 2, 3];
  action_meaning := ['Å©', 'Å®', 'Å™', 'Å´'];
  for var i := 0 to High(reward_map) do
    reward_map[i] := 0;
  goal_state := Point(4, 1);
  wall_state := Point(2, 2);
  start_state := Point(1, 3);
  agent_state := start_state;
  reward_map[change(goal_state)] := 1.0;
  reward_map[change(Point(4, 2))] := -1.0;
  reward_map[change(wall_state)] := Nan;
end;

function TGridWorld.nextState(state: TPoint; action: integer): TPoint;
var
  action_move_map: TArray<TPoint>;
  move: TPoint;
begin
  action_move_map := [Point(-1, 0), Point(1, 0), Point(0, -1), Point(0, 1)];
  move := action_move_map[action];
  result := Point(state.X + move.X, state.Y + move.Y);
  if (result.X < 1) or (result.X > wid) or (result.Y < 1) or (result.Y > hei)
  then
    result := state
  else if result = wall_state then
    result := state;
end;

procedure TGridWorld.reset;
begin
  agent_state := start_state;
end;

procedure TGridWorld.reversed(din: TArray<TAgentData>;
  out dout: TArray<TAgentData>);
begin
  dout := [];
  for var data in din do
    dout := [data] + dout;
end;

function TGridWorld.reward(state, next_state: TPoint; action: integer): Single;
begin
  result := reward_map[change(next_state)];
end;

procedure TGridWorld.states(out d: TArray<TPoint>);
begin
  d := [];
  for var j := 0 to hei - 1 do
    for var i := 0 to wid - 1 do
      d := d + [Point(i + 1, j + 1)];
end;

function TGridWorld.step(action: integer): TNextAgentData;
var
  state, next_state: TPoint;
begin
  state := agent_state;
  next_state := nextState(state, action);
  agent_state := next_state;
  result.next_state := next_state;
  result.reward := reward(state, next_state, action);
  result.done := agent_state = goal_state;
end;

{ TAgent }

function TAgent.argmax(d: TArray<Single>): integer;
var
  max: Single;
begin
  max := MaxValue(d);
  for var i := 0 to High(d) do
    if d[i] = max then
      result := i;
end;

function TAgent.getAction(state: TPoint): integer;
begin

end;

function TAgent.GetPi(state: TPoint): TArray<Single>;
begin
  result := FPi[Env.change(state)];
end;

function TAgent.GetQ(state: TPoint; action: integer): Single;
begin
  result := FQ[Env.change(state)][action];
end;

procedure TAgent.init;
begin

end;

procedure TAgent.SetPi(state: TPoint; const Value: TArray<Single>);
begin
  FPi[Env.change(state)] := Value;
end;

procedure TAgent.SetQ(state: TPoint; action: integer; const Value: Single);
begin
  FQ[Env.change(state)][action] := Value;
end;

{ TMcAgent }

procedure TMcAgent.add(state: TPoint; action: integer; reward: Single);
var
  data: TAgentData;
begin
  data.state := state;
  data.action := action;
  data.reward := reward;
  memory := memory + [data];
end;

function TMcAgent.getAction(state: TPoint): integer;
var
  action_probs: TArray<Single>;
  prob: Single;
begin
  inherited;
  action_probs := Pi[state];
  prob := Random;
  for var i := 0 to High(action_probs) do
  begin
    result := i;
    if action_probs[i] > prob then
      break
    else
      prob := prob - action_probs[i];
  end;
end;

procedure TMcAgent.greedy_probs(out action_probs: TArray<Single>; state: TPoint;
  epsilon: Single; action_size: integer);
var
  base_prob: Single;
  max_action: integer;
begin
  epsilon := 0.3;
  SetLength(action_probs, action_size);
  max_action:=argmax(FQ[env.change(state)]);
  base_prob := epsilon / action_size;
  for var action := 0 to action_size - 1 do
    action_probs[action] := base_prob;
  action_probs[max_action] := action_probs[max_action] + 1 - epsilon;
end;

procedure TMcAgent.init;
var
  states: TArray<TPoint>;
  random_actions: TArray<Single>;
begin
  inherited;
  Randomize;
  random_actions := [0.05, 0.45, 0.25, 0.25];
  Env.states(states);
  for var state in states do
  begin
    Pi[state] := random_actions;
    FQ[Env.change(state)] := [0, 0, 0, 0];
  end;
  Initialize(memory);
  Finalize(states);
  Finalize(random_actions);
end;

procedure TMcAgent.reset;
begin
  Initialize(memory);
end;

procedure TMcAgent.update;
var
  G: Single;
  tmp: TArray<TAgentData>;
  tmp_q: Single;
  action_probs: TArray<Single>;
begin
  G := 0;
  Env.reversed(memory, tmp);
  for var data in tmp do
  begin
    G := gamma * G + data.reward;
    tmp_q := Q[data.state, data.action];
    Q[data.state, data.action] := tmp_q + (G - tmp_q) * alpha;
    greedy_probs(action_probs, data.state);
    Pi[data.state] := Copy(action_probs, 0, Length(action_probs));
    Finalize(action_probs);
  end;
  Finalize(tmp);
end;

end.

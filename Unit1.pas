unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls,
  System.Actions, Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, System.Types,
  System.UITypes, System.Math, System.Generics.Collections;

const
  wid = 4;
  hei = 3;

type
  TPi = record
    action: integer;
    action_prob: Single;
  end;

  TAnswer = class
  private
    FWid, FHei: integer;
    function GetData(state: TPoint): TArray<TPi>;
    procedure SetData(state: TPoint; const Value: TArray<TPi>);
  protected
    FData: TArray<TArray<TPi>>;
  public
    constructor Create(w, h: integer);
    destructor Destroy; override;
    function Check(obj: TAnswer): Boolean;
    procedure Init(Data: TArray<TPi>);
    procedure Assign(ans: TAnswer);
    property Datas[state: TPoint]: TArray<TPi> read GetData
      write SetData; default;
  end;

  TAgentData = record
    state: TPoint;
    action: integer;
    reward: Single;
  end;

  TRandomAgent = class
  const
    gamma = 0.9;
    action_size = 4;
  private
    FCnts: TArray<integer>;
    FV: TArray<Single>;
    memory: TArray<TAgentData>;
    function GetCnts(state: TPoint): integer;
    procedure SetCnts(state: TPoint; const Value: integer);
    function GetV(state: TPoint): Single;
    procedure SetV(state: TPoint; const Value: Single);
  public
    Pi: TAnswer;
    constructor Create;
    destructor Destroy; override;
    procedure add(state: TPoint; action: integer; reward: Single);
    procedure reset;
    procedure eval;
    function getAction(state: TPoint): integer;
    procedure Init;
    property cnts[state: TPoint]: integer read GetCnts write SetCnts;
    property V[state:TPoint]: Single read GetV write SetV;
  end;

  TMcAgent = class(TRandomAgent)
  public

  end;

  TAgentEnv = record
    next_state: TPoint;
    reward: Single;
    done: Boolean;
  end;

  TForm1 = class(TForm)
    ActionManager1: TActionManager;
    Action1: TAction;
    ActionToolBar1: TActionToolBar;
    Action2: TAction;
    Action3: TAction;
    Action4: TAction;
    Action5: TAction;
    procedure FormPaint(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
    procedure Action4Execute(Sender: TObject);
    procedure Action5Execute(Sender: TObject);
  private
    FV, reward_map: TArray<Single>;
    action_space: TArray<integer>;
    action_meaning: TArray<string>;
    goal_state, wall_state, start_state: TPoint;
    agent_state: TPoint;
    procedure make_map(out action_move_map: TArray<TPoint>;
      Value: array of integer);
    function GetShape: TPoint;
    function GetV(state: TPoint): Single;
    procedure SetV(state: TPoint; const Value: Single);
    procedure SetReward(state: TPoint; const Value: Single);
    function GetReward(state: TPoint): Single;
    { Private 宣言 }
  public
    { Public 宣言 }
    Answer: TAnswer;
    agent: TRandomAgent;
    function nextState(state: TPoint; action: integer): TPoint;
    function reward(state, next_state: TPoint; action: integer): Single;
    procedure evalOnestep(Pi: TAnswer; V: TArray<Single>;
      const gamma: Single = 0.9);
    procedure policyEval(Pi: TAnswer; V: TArray<Single>; gamma: Single;
      const threshold: Single = 0.001);
    procedure greedyPolicy(out pi_data: TAnswer; V: TArray<Single>;
      gamma: Single);
    procedure valueIterOnestep(V: TArray<Single>; gamma: Single);
    procedure valueIter(V: TArray<Single>; gamma: Single;
      threshold: Single = 0.001; is_render: Boolean = true);
    function argmax(d: TArray<TPi>): integer;
    function Init: Single;
    function step(act: integer): TAgentEnv;
    function reset: TPoint;
    property shape: TPoint read GetShape;
    property V[state: TPoint]: Single read GetV write SetV;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function states: TArray<TPoint>;
var
  cnt: integer;
begin
  cnt := 0;
  SetLength(result, wid * hei);
  for var j := 1 to hei do
    for var i := 1 to wid do
    begin
      result[cnt] := Point(i, j);
      inc(cnt);
    end;
end;

function change(state: TPoint): integer;
begin
  result := state.X - 1 + (state.Y - 1) * wid;
end;

procedure TForm1.Action1Execute(Sender: TObject);
var
  gamma: Single;
begin
  gamma := Init;
  policyEval(Answer, FV, gamma);
  FormPaint(nil);
end;

procedure TForm1.Action2Execute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Action3Execute(Sender: TObject);
var
  gamma: Single;
  new_pi: TAnswer;
  is_render: Boolean;
begin
  gamma := Init;
  is_render := true;

  new_pi := TAnswer.Create(wid, hei);
  while true do
  begin
    policyEval(Answer, FV, gamma, 0.001);
    greedyPolicy(new_pi, FV, gamma);
    if is_render then
    begin
      FormPaint(nil);
      Sleep(1000);
    end;
    if Answer.Check(new_pi) then
      break;
    Answer.Assign(new_pi);
  end;
  new_pi.Free;
  FormPaint(nil);
end;

// 価値反復法
procedure TForm1.Action4Execute(Sender: TObject);
var
  gamma: Single;
begin
  gamma := Init;

  valueIter(FV, gamma);

  greedyPolicy(Answer, FV, gamma);
  FormPaint(nil);
end;

// モンテカルロ法
procedure TForm1.Action5Execute(Sender: TObject);
const
  episodes = 1000;
var
  state: TPoint;
  action: integer;
  Data: TAgentEnv;
begin
  for var epi := 1 to episodes do
  begin
    state := reset;
    agent.reset;
    while true do
    begin
      action := agent.getAction(state);
      Data := step(action);

      agent.add(state, action, Data.reward);
      if Data.done then
      begin
        agent.eval;
        break;
      end;
      state := Data.next_state;
    end;
  end;
  FormPaint(nil);
end;

function TForm1.argmax(d: TArray<TPi>): integer;
var
  max_value: Single;
begin
  max_value := NegInfinity;
  for var item in d do
    if max_value < item.action_prob then
      max_value := item.action_prob;
  for var item in d do
    if item.action_prob = max_value then
      result := item.action;
end;

procedure TForm1.evalOnestep(Pi: TAnswer; V: TArray<Single>;
  const gamma: Single);
var
  action: integer;
  items: TArray<TPi>;
  new_V, r: Single;
  next_state: TPoint;
begin
  for var state in states do
  begin
    if state = goal_state then
    begin
      V[change(state)] := 0;
      continue;
    end;
    items := Pi[state];
    new_V := 0;

    for var item in items do
    begin
      action := item.action;
      next_state := nextState(state, action);
      r := reward(state, next_state, action);
      new_V := new_V + item.action_prob * (r + gamma * GetV(next_state));
      V[change(state)] := new_V;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  action_space := [0, 1, 2, 3];
  action_meaning := ['LEFT', 'RIGHT', 'UP', 'DOWN'];
  goal_state := Point(4, 1);
  wall_state := Point(2, 2);
  start_state := Point(1, 3);
  agent_state := start_state;
  SetLength(FV, wid * hei);
  Answer := TAnswer.Create(wid, hei);
  SetLength(reward_map, wid * hei);
  SetReward(goal_state, 1.0);
  SetReward(wall_state, Nan);
  SetReward(Point(4, 2), -1.0);
  agent := TRandomAgent.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Finalize(action_space);
  Finalize(action_meaning);
  Finalize(reward_map);
  Finalize(FV);
  Answer.Free;
  agent.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
const
  size = 150;
var
  probs: TArray<TPi>;
  m, n: integer;
begin
  Canvas.FillRect(ClientRect);
  for var i := 1 to wid + 1 do
  begin
    Canvas.MoveTo(i * size, size);
    Canvas.LineTo(i * size, (hei + 1) * size);
  end;
  for var i := 1 to hei + 1 do
  begin
    Canvas.MoveTo(size, i * size);
    Canvas.LineTo((wid + 1) * size, i * size);
  end;
  Canvas.Brush.Color := Canvas.Pen.Color;
  Canvas.FloodFill(2 * size + 1, 2 * size + 1, clBlack, fsBorder);
  Canvas.Brush.Style := bsClear;
  Canvas.Brush.Color := Color;
  Canvas.TextOut(goal_state.X * size, goal_state.Y * size, 'R 1.0(GOAL)');
  Canvas.TextOut(4 * size, 2 * size, 'R -1.0');
  for var state in states do
  begin
    Canvas.TextOut(state.X * size + 10, (state.Y + 1) * size - 20,
      SimpleRoundTo(V[state]).ToString);
    probs := Answer[state];
    m := state.X * size + size div 2;
    n := state.Y * size + size div 2;
    for var prob in probs do
    begin
      if prob.action_prob = 0 then
        continue;
      Canvas.MoveTo(m, n);
      Canvas.TextOut(Canvas.PenPos.X, Canvas.PenPos.Y,
        action_meaning[prob.action]);
      inc(n, Canvas.TextHeight('a'));
    end;
  end;
end;

function TForm1.GetReward(state: TPoint): Single;
begin
  result := reward_map[state.X - 1 + (state.Y - 1) * wid];
end;

function TForm1.GetShape: TPoint;
begin
  result.X := wid;
  result.Y := hei;
end;

function TForm1.GetV(state: TPoint): Single;
begin
  result := FV[state.X - 1 + (state.Y - 1) * wid];
end;

procedure TForm1.greedyPolicy(out pi_data: TAnswer; V: TArray<Single>;
  gamma: Single);
var
  action_values, action_probs: TArray<TPi>;
  item: TPi;
  next_state: TPoint;
  r, Value: Single;
  max_action: integer;
begin
  pi_data := TAnswer.Create(wid, hei);
  for var state in states do
  begin
    action_values := [];
    for var action in action_space do
    begin
      next_state := nextState(state, action);
      r := reward(state, next_state, action);
      Value := r + gamma * GetV(next_state);
      item.action := action;
      item.action_prob := Value;
      action_values := action_values + [item];
      item.action_prob := 0;
      action_probs := action_probs + [item];
    end;
    max_action := argmax(action_values);
    action_probs[max_action].action_prob := 1.0;
    pi_data[state] := action_probs;

    Finalize(action_values);
    Finalize(action_probs);
  end;
end;

function TForm1.Init: Single;
var
  action_probs: TArray<TPi>;
  obj: TPi;
begin
  result := 0.9;
  for var state in states do
  begin
    action_probs := [];
    for var action in action_space do
    begin
      obj.action := action;
      obj.action_prob := 0.25;
      action_probs := action_probs + [obj];
    end;
    Answer[state] := action_probs;
    V[state] := 0;
    Finalize(action_probs);
  end;
end;

procedure TForm1.make_map(out action_move_map: TArray<TPoint>;
  Value: array of integer);
var
  cnt: integer;
  p: TPoint;
begin
  action_move_map := [];
  cnt := 0;
  while cnt < High(Value) do
  begin
    p.X := Value[cnt];
    p.Y := Value[cnt + 1];
    inc(cnt, 2);
    action_move_map := action_move_map + [p];
  end;
end;

function TForm1.nextState(state: TPoint; action: integer): TPoint;
var
  action_move_map: TArray<TPoint>;
  move: TPoint;
begin
  make_map(action_move_map, [-1, 0, 1, 0, 0, -1, 0, 1]);
  move := action_move_map[action];
  result := Point(state.X + move.X, state.Y + move.Y);
  if (result.X < 1) or (result.X > wid) or (result.Y < 1) or (result.Y > hei)
  then
    result := state
  else if result = wall_state then
    result := state;
  Finalize(action_move_map);
end;

procedure TForm1.policyEval(Pi: TAnswer; V: TArray<Single>; gamma: Single;
  const threshold: Single);
var
  old_V: TArray<Single>;
  delta, tmp: Single;
begin
  while true do
  begin
    old_V := Copy(V, 0, Length(V));
    evalOnestep(Pi, V, gamma);
    delta := 0;
    for var i := 0 to wid * hei - 1 do
    begin
      tmp := Abs(V[i] - old_V[i]);
      if delta < tmp then
        delta := tmp;
    end;
    if delta < threshold then
      break;
  end;
end;

function TForm1.reset: TPoint;
begin
  agent_state := start_state;
  result := agent_state;
end;

function TForm1.reward(state, next_state: TPoint; action: integer): Single;
begin
  result := GetReward(next_state);
end;

procedure TForm1.SetReward(state: TPoint; const Value: Single);
begin
  reward_map[state.X - 1 + (state.Y - 1) * wid] := Value;
end;

procedure TForm1.SetV(state: TPoint; const Value: Single);
begin
  FV[state.X - 1 + (state.Y - 1) * wid] := Value;
end;

function TForm1.step(act: integer): TAgentEnv;
var
  state, next_state: TPoint;
begin
  state := agent_state;
  next_state := nextState(state, act);
  result.next_state := next_state;
  result.reward := reward(state, next_state, act);
  result.done := next_state = goal_state;
  agent_state := next_state;
end;

procedure TForm1.valueIter(V: TArray<Single>; gamma: Single; threshold: Single;
  is_render: Boolean);
var
  old_V: TArray<Single>;
  delta, tmp: Single;
begin
  while true do
  begin
    if is_render then
    begin
      FormPaint(nil);
      Sleep(1000);
    end;
    old_V := Copy(V, 0, Length(V));
    valueIterOnestep(V, gamma);

    delta := 0;
    for var state in states do
    begin
      tmp := Abs(V[change(state)] - old_V[change(state)]);
      if delta < tmp then
        delta := tmp;
    end;
    if delta < threshold then
      break;
  end;
end;

procedure TForm1.valueIterOnestep(V: TArray<Single>; gamma: Single);
var
  action_values: TArray<Single>;
  next_state: TPoint;
  r: Single;
  Value: Single;
begin
  for var state in states do
  begin
    if state = goal_state then
    begin
      V[change(state)] := 0;
      continue;
    end;
    action_values := [];
    for var action in action_space do
    begin
      next_state := nextState(state, action);
      r := reward(state, next_state, action);
      Value := r + gamma * V[change(next_state)];
      action_values := action_values + [Value];
    end;
    V[change(state)] := MaxValue(action_values);
    Finalize(action_values);
  end;
end;

{ TAnswer }

procedure TAnswer.Assign(ans: TAnswer);
begin
  for var state in states do
    Datas[state] := Copy(ans[state]);
end;

function TAnswer.Check(obj: TAnswer): Boolean;
var
  item1, item2: TPi;
begin
  result := true;
  for var state in states do
    for var i := 0 to High(Datas[state]) do
    begin
      item1 := obj[state][i];
      item2 := Datas[state][i];
      if (item1.action <> item2.action) or
        (item1.action_prob <> item2.action_prob) then
        result := false;
    end;
end;

constructor TAnswer.Create(w, h: integer);
begin
  inherited Create;
  FWid := w;
  FHei := h;
  SetLength(FData, w * h);
end;

destructor TAnswer.Destroy;
var
  obj: TArray<TPi>;
begin
  for var state in states do
  begin
    obj := Datas[state];
    Finalize(obj);
  end;
  Finalize(FData);
  inherited;
end;

function TAnswer.GetData(state: TPoint): TArray<TPi>;
begin
  result := FData[state.X - 1 + (state.Y - 1) * FWid];
end;

procedure TAnswer.Init(Data: TArray<TPi>);
begin
  for var state in states do
    Datas[state] := Copy(Data, 0, Length(Data));
end;

procedure TAnswer.SetData(state: TPoint; const Value: TArray<TPi>);
begin
  FData[state.X - 1 + (state.Y - 1) * FWid] := Copy(Value, 0, Length(Value));
end;

{ TRandomAgent }

procedure TRandomAgent.add(state: TPoint; action: integer; reward: Single);
var
  Data: TAgentData;
begin
  Data.state := state;
  Data.action := action;
  Data.reward := reward;
  memory := memory + [Data];
end;

constructor TRandomAgent.Create;
var
  Data: TPi;
  Datas: TArray<TPi>;
begin
  Pi := Form1.Answer;
  FV := Form1.FV;
  Datas := [];
  for var i := 0 to 3 do
  begin
    Data.action := i;
    Data.action_prob := 0.25;
    Datas := Datas + [Data];
  end;
  Pi.Init(Datas);
  Finalize(Datas);
  SetLength(FCnts, wid * hei);
  memory := [];
end;

destructor TRandomAgent.Destroy;
begin
  Pi.Free;
  Finalize(memory);
  inherited;
end;

procedure TRandomAgent.eval;
var
  G: Single;
  reverse: TArray<TAgentData>;
  function reversed(d: TArray<TAgentData>): TArray<TAgentData>;
  begin
    result := [];
    for var Data in d do
      result := [Data] + result;
  end;

begin
  G := 0;
  reverse := reversed(memory);
  for var Data in reverse do
  begin
    G := gamma * G + Data.reward;
    cnts[Data.state] := cnts[Data.state] + 1;
    V[Data.state] := V[Data.state] + (G - V[Data.state]) / cnts[Data.state];
  end;
  Finalize(reverse);
end;

function TRandomAgent.getAction(state: TPoint): integer;
var
  action_probs: TArray<TPi>;
  e, i: Extended;
begin
  action_probs := Pi[state];
  e := Random;
  i := 0;
  for var prob in action_probs do
  begin
    i := i + prob.action_prob;
    result := prob.action;
    if i > e then
      break;
  end;
end;

function TRandomAgent.GetCnts(state: TPoint): integer;
begin
  result := FCnts[change(state)];
end;

function TRandomAgent.GetV(state: TPoint): Single;
begin
  result := FV[change(state)];
end;

procedure TRandomAgent.Init;
var
  random_action: TArray<TPi>;
  Data: TPi;
begin
  for var i := 0 to action_size - 1 do
  begin
    Data.action := i;
    Data.action_prob := 0;
    random_action := random_action + [Data];
  end;
  for var state in states do
  begin
    Pi[state] := random_action;
    V[state] := 0;
    cnts[state] := 0;
  end;
  Finalize(random_action);
end;

procedure TRandomAgent.reset;
begin
  Randomize;
  Initialize(memory);
end;

procedure TRandomAgent.SetCnts(state: TPoint; const Value: integer);
begin
  FCnts[change(state)] := Value;
end;

procedure TRandomAgent.SetV(state: TPoint; const Value: Single);
begin
  FV[change(state)] := Value;
end;

end.

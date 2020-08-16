
type Hour = int
type Minute = int
type Second = int

type TimeValue = Hour | Minute | Second

type Time = {
  Hour: Hour
  Minute: Minute
  Second: Second
}

type RowGenerator = {
  RowList: int List
  RowAction: int -> int -> string
  TimeValue: TimeValue
}

let modulo x y = y % x

let onOrOff trueValue falseValue measure elem = 
  match (>=) measure elem with
  | true -> trueValue 
  | false -> falseValue

let rows = [
  {
    RowList = [1]; 
    RowAction = modulo 2 >> onOrOff "O" "Y"; 
    TimeValue = Second
  };
  {
    RowList = [5;10;15;20]; 
    RowAction = onOrOff "R" "O"; 
    TimeValue = Hour
  };
  {
    RowList = [1;2;3;4]; 
    RowAction = modulo 5 >> onOrOff "R" "O"; 
    TimeValue = Hour
  };
  {
    RowList = [5;10;15;20;25;30;35;40;45;50;55]; 
    RowAction = onOrOff "Y" "O"; 
    TimeValue = Minute
  };
  {
    RowList = [1;2;3;4]; 
    RowAction = modulo 5 >> onOrOff "Y" "O"; 
    TimeValue = Minute};
]

let rowLights list action = 
  let foldAction lightTracker elem = lightTracker + action elem
  list |> List.fold foldAction ""

let rowResults inputTime = 
  let action (elem: RowGenerator) = 
    match elem.TimeValue with
    | Second -> inputTime.Second |> elem.RowAction |> rowLights elem.RowList
    | Minute -> inputTime.Minute |> elem.RowAction |> rowLights elem.RowList
    | Hour ->   inputTime.Hour   |> elem.RowAction |> rowLights elem.RowList
  rows |> List.map action

let timeString = "21:57:21"
let toTime (timeArray: string[]) = {Hour = int timeArray.[0]; Minute = int timeArray.[1]; Second = int timeArray.[2]}
let timeParse (timeStr: string) = timeStr.Split ':'
let inputTime = timeString |> timeParse |> toTime

let allRows = rowResults inputTime

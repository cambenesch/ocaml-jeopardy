
(**[find_cell st] is a random available cell for bot to pick. *)
let find_cell st= 
  let cells = State.cells_not_picked st in 
  let () = Random.self_init () in  
  let n= Random.int (List.length cells) in
  List.nth cells n

(* [level st] is the level of the bot in state [st]. *)
let level st = match (State.get_setting st) with  
  | Bot i -> i
  | _ -> failwith "Used bot.ml when no bot was supposed to be present."

(*[find_wage st] is the wage for bot to declare. This is decided based on the 
  difference in score between the two players, the round of the game, and the 
  maximum possible wager. Bot's goal is to win the game, not necessarily 
  to score as many points as possible. *)
let find_wage st jeo =
  let diff = snd (State.current_score st) - fst(State.current_score st)
             |> float_of_int in 
  let score = snd (State.current_score st) in 
  let point = if State.is_daily_double st then 
      let category = State.current_category_id st in 
      let cell = State.current_cell_id st in 
      let round = State.round_status st in
      Jeopardy.get_points jeo category cell round else 5 in 
  let max_wage = max score point in 
  let pre_max = if State.in_final st = Progress2 && diff <= 0.0 then 1.0 -. diff
    else if State.in_final st = Progress2 && diff > 0.0 then diff -. 1.0
    else if diff <= 0.0 && State.round_status st = 1 then diff *. -1.0
    else if diff > 0.0 && State.round_status st = 1 then diff *. 0.5
    else if diff <= 0.0 && State.round_status st = 2 then diff *. -1.5
    else diff *. 0.1 in 
  let min_max = max 5 (int_of_float pre_max) in 
  min min_max max_wage

(*[get_answer st jeo round] is the correct answer to the current clue in 
  state [st]. *)
let get_answer st jeo round= 
  if State.round_over st && State.round_status st = 2 then 
    Jeopardy.get_final_answer jeo 
  else
    let category = State.current_category_id st in 
    let cell = State.current_cell_id st in 
    Jeopardy.get_answer jeo category cell (State.round_status st)

(** [next_cmd st jeo] generates the next move by bot based on [st] and [jeo]. 
    If bot must answer a question, bot gets question wrong randomly, with the 
    long-run percentage correctness higher if bot's difficulty level is higher. 
    Bot gives wrong answer related to the question keyword. If the bot does not 
    answer question correctly, it will pass or answer incorrectly with equal 
    probability. Bot does not take longer than 10 seconds. *)
let next_cmd jeo st=
  Unix.sleep 2;
  print_string "---Bot entered: ";
  let cmd = 
    if State.current_clue st = "" && 
       not (State.round_over st && State.round_status st = 2) then 
      "pick " ^ (find_cell st)
    else if (State.is_daily_double st ||
             (State.round_over st && State.round_status st = 2)) 
         && (snd (State.current_wage st) = -1) then 
      "wager " ^ (string_of_int (find_wage st jeo)) 
    else begin
      let answer = get_answer st jeo (State.round_status st) in 
      let q_word = (Str.split (Str.regexp " +") answer) |> List.hd in
      let bogus_lst = if q_word = "Who" 
        then ["Player 1"; "myself"; "Kaitlyn Mackenzie Li"; "Jamie Wang"; 
              "going to win"; "Claire Cui"; "Rachel Nash"]
        else if q_word = "Where" 
        then ["nowhere"; "Hollister 372"; "Skokie, IL"; "North Korea"; 
              "the Grand Canyon"; "Stonehenge"; "Rhodes Hall"]
        else ["marijuana"; "tree"; "indigo"; "glasses"; "coffee"; "pizza"; 
              "pineapple pizza"; "the answer to this question"; "the point"] in 
      let () = Random.self_init () in
      let correct = (Random.int 11) <= level st in
      let index = Random.int (List.length bogus_lst) in
      let bogus = List.nth bogus_lst index in
      if correct then "answer " ^ answer
      else if Random.int 2 = 1 then "answer " ^ q_word ^ " is " ^ bogus ^ "?" 
      else "pass"
    end in 
  print_endline cmd;
  cmd 

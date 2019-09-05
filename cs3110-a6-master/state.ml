
(** The type representing final jeopardy. *)
type final = None | Progress1 | Progress2 | Done

(** The type representing the current player. *)
type player = P1 | P2

(** The type representing player 2. *)
type setting = Person | Bot of int

(** The abstract type of values representing the game state. *)
type t = {
  board: (Jeopardy.category_id*Jeopardy.cell_id*int*bool) list;
  dd_cell_ids: Jeopardy.cell_id list; (*daily double*)
  current_category_id: Jeopardy.category_id;
  current_cell_id: Jeopardy.cell_id;
  current_clue: Jeopardy.clue;
  current_wage: int*int;
  score: int*int;
  in_final: final;
  round: int; (* 1 is jeopardy, 2 is double/final jeopardy *)
  turn: player;
  setting: setting;
  timer_time: float;
}

(** The type representing the result of a command. *)
type result = Legal of t | Illegal

(** [init board jeo] is a helper function to set the initial game board*)
let init_board jeo r = 
  let rec set_board acc = function
    | [] -> acc
    | h::t -> let cells = List.map (fun x ->
        (h, x, Jeopardy.get_points jeo h x r, false)) 
        (Jeopardy.cell_ids jeo h r) in
      set_board (cells@acc) t in 
  set_board [] (List.rev (Jeopardy.category_ids jeo r))

(** [seed_daily_double board] is to randomly tag a cell in board [board] as 
    Jeopardy daily double, returns a cell id*)
let seed_daily_double board= 
  let () = Random.self_init () in 
  let length = List.length board in 
  let (w, x, y, z) = List.nth board (Random.int length) in x

(** [display_board st] prints the jeopardy board *)
let display_board st = 
  let rec print_board init = function
    | []-> Format.fprintf  Format.std_formatter "\n\n"
    | (w, x, y, z)::t-> if w != init then 
        Format.fprintf  Format.std_formatter "\n %15s   " w;
      if z then 
        Format.fprintf  Format.std_formatter " %s " "xx"
      else
        Format.fprintf  Format.std_formatter " %d " y;
      print_board w t in 
  print_board "" st.board

(** [is_daily_double st] is true if current picked cell is daily double. *)
let is_daily_double st =
  List.mem st.current_cell_id st.dd_cell_ids 

(** [current_wage st] is the current wage that the player has declared. *)
let current_wage st =
  st.current_wage

(**[current_category_id st] is the current cell that the player has picked. *)
let current_category_id st =
  st.current_category_id

(**[current_cell_id st] is the current cell that the player has picked. *)
let current_cell_id st =
  st.current_cell_id

(** [current_clue st] is the current clue in the cell the player has picked. *)
let current_clue st =
  st.current_clue

(** [current_score st] is the sum of points that the player has currently
     earned. *)
let current_score st =
  st.score

(** [get_setting st] is the setting of Player 2, a human or bot with difficulty
    level. *)
let get_setting st =
  st.setting

(** [round_over st] is [true] if all questions in [st] are answered. *)
let round_over st = 
  let rec all_cell_picked = function
    | []-> st.current_cell_id = ""
    | (w,x,y,z)::t -> if z then (all_cell_picked t) else false in 
  all_cell_picked st.board 

(** [new_round st] is [true] if all question in [st] are unanswered. *)
let new_round st = 
  let rec no_cells_picked = function
    | []-> st.current_cell_id = ""
    | (w,x,y,z)::t -> if z then false else (no_cells_picked t) in 
  no_cells_picked st.board 

(** [init_state jeo] is the initial state of round 1 of the game [jeo]. 
    In that state all questions in round 1 are unanswered, there is no question
    currently being answered, and the score is [0] for both players. 
    There is one daily double in the board. *)
let init_state jeo =
  let init = init_board jeo 1 in
  { 
    board = init;
    dd_cell_ids= [(seed_daily_double init)];
    current_category_id = "";
    current_cell_id = "";
    current_clue = "";
    current_wage = (-1,-1);
    score = (0,0);
    in_final = None;
    round = 1;
    turn = P1;
    setting = Bot 5;
    timer_time = Unix.time()
  }

(** [init_state_second jeo st] is the initial state of round 2 of the
    game [jeo]. 
    In that state all questions in round 2 are unanswered, there is no question
    currently being answered, and the score is carried over from round 1.
    There are two daily doubles in the board. *)
let init_state_second jeo st = 
  let init = init_board jeo 2 in
  { 
    board = init;
    dd_cell_ids= [(seed_daily_double init);(seed_daily_double init)];
    current_category_id = "";
    current_cell_id = "";
    current_clue = "";
    current_wage = (-1,-1);
    score = st.score;
    in_final = None;
    round = 2;
    (* whoever has fewer points at end of round 1 goes first in round 2 *)
    turn =
      if fst st.score < snd st.score then P1 
      else if fst st.score > snd st.score then P2 
      else begin
        if st.turn = P1 then P2 else P1
      end;
    setting = st.setting;
    timer_time = st.timer_time
  }

(** [in_final st] is the status of final jeopardy round.*)
let in_final st =
  st.in_final

(** [round_status st] is the current round that [st] is in. *)
let round_status st =
  st.round

(** [player_turn st] is the player who has the turn in the state [st]. *)
let player_turn st =
  st.turn

(** [is_cell_picked cell board] returns true if the cell is already picked in
    board [board]*)
let rec is_cell_picked cell = function
  | []-> true
  | (w, x, y, z)::t -> if x=cell then z else is_cell_picked cell t

(** [set_cell_picked cell board] set the cell [cell] to true indicating it is
    already done
    in board [board]*)
let set_cell_picked cell board =
  List.map (fun (w, x, y, z)-> if x=cell
             then (w, x, y, true)
             else (w, x, y, z)) board

(** get_category_id_from_cell cell board] returns the category identifier
    corresponding to cell [cell] in board [board]. The functions in Jeopardy
    module require catgory id *)
let rec get_category_id_from_cell cell = function
  | [] -> ""
  | (w,x,y,z)::t-> if x=cell then w else get_category_id_from_cell cell t

(** [cells_not_picked st] is the available cells which are 
    not picked in board [board]*)
let cells_not_picked st=
  List.fold_left (fun init (w, x, y, z) -> if (is_cell_picked x st.board)
                   then init else x::init) [] st.board 

(** [pick cell jeo st] is [Legal of t] if cell is not already picked and 
    they have answered the clue of the previous picked cell
    Otherwise the result is [Illegal]. *)
let pick cell jeo st p = 
  if is_cell_picked cell st.board || st.current_cell_id <> "" then Illegal
  else
    let category_id = get_category_id_from_cell cell st.board in
    Legal {
      board = set_cell_picked cell st.board;
      dd_cell_ids = st.dd_cell_ids;
      current_category_id = category_id;
      current_cell_id = if List.mem cell st.dd_cell_ids then begin
          print_endline "Daily Double! Please wager some points.";
          cell
        end
        else cell;
      current_clue = Jeopardy.get_clue jeo category_id cell st.round;
      current_wage = st.current_wage;
      score = st.score;
      in_final = None;
      round = st.round;
      turn = st.turn;
      setting = st.setting;
      timer_time = Unix.time()
    }

(** [wager wage jeo st p] is [Legal of t] if daily double, Otherwise the result 
    is [Illegal]. *)
let wager wage jeo st p = 
  if (is_daily_double st) && wage >= 5 then
    let pt = 
      Jeopardy.get_points jeo st.current_category_id st.current_cell_id st.round
    in
    if wage <= (max (if p=P1 then fst st.score else snd st.score) pt) then
      Legal {
        board = st.board;
        dd_cell_ids = st.dd_cell_ids;
        current_category_id = st.current_category_id;
        current_cell_id = st.current_cell_id;
        current_clue = st.current_clue;
        current_wage = if p=P1 then (wage, snd st.current_wage)
          else (fst st.current_wage, wage);
        score = st.score;
        in_final = None;
        round = st.round;
        turn = st.turn;
        setting = st.setting;
        timer_time = Unix.time()
      }
    else Illegal
  else if (round_over st && st.round = 2 && wage >= 0 &&
           wage <= (if p=P1 then fst st.score else snd st.score)) then 
    Legal {
      board = st.board;
      dd_cell_ids = st.dd_cell_ids;
      current_category_id = st.current_category_id;
      current_cell_id = st.current_cell_id;
      current_clue = Jeopardy.get_final_clue jeo;
      current_wage = if p = P1 then (wage, snd st.current_wage) 
        else (fst st.current_wage, wage);
      score = st.score;
      in_final = if st.in_final = None then Progress1
        else if st.in_final = Progress1 then Progress2 else st.in_final;
      round = st.round;
      turn = st.turn;
      setting = st.setting;
      timer_time = Unix.time()
    }
  else Illegal

(** [is_question ans] is true if [ans] is in the form of a question and false 
    if [ans] is not *)
let is_question ans = 
  let r = Str.regexp_case_fold " *\\(Who\\|What\\|Where\\) \\(.\\)*\\(\\?\\)" in 
  Str.string_match r ans 0

(** [is_correct response target] is true if a [response] contains the [target] 
    and false otherwise*)
let is_correct response target = 
  let r = Str.regexp_case_fold target in 
  Str.string_match r response 0

(** [ans_dd_final ans cor regs st p] is the state [st] after an answer [ans] has
    been submitted and compared to the correct response [cor] and [regs] and the
    score has been adjusted accordingly with the corresponding point value [p]
    [ans_dd_final] is a helper function for [answer] specifically for daily
    double and final jeopardy *)
let ans_dd_final ans cor regs st p = 
  Legal {
    board = st.board;
    dd_cell_ids = st.dd_cell_ids;
    current_category_id = "";
    current_cell_id = "";
    current_clue = "";
    current_wage = (-1,-1);
    score = 
      if (Unix.time() -. st.timer_time) > 20.0
      then begin print_endline ("You did not answer within 20 seconds! " ^ 
                                "The correct question was: " ^ cor
                                ^"\nYou get " 
                                ^ string_of_int (if p=P1 
                                                 then fst st.current_wage 
                                                 else snd st.current_wage)
                                ^ " points deducted."); 
        (if p=P1 then ((fst st.score)-(fst st.current_wage), snd st.score)
         else (fst st.score, (snd st.score)-(snd st.current_wage)))
      end
      else if is_correct ans regs && (is_question ans || st.round = 1)
      then begin
        print_endline ("Correct! You get " 
                       ^ string_of_int (if p=P1 
                                        then fst st.current_wage 
                                        else snd st.current_wage)
                       ^ " points.");
        (if p=P1 then ((fst st.score)+(fst st.current_wage), snd st.score)
         else (fst st.score, (snd st.score) + (snd st.current_wage)))
      end
      else if is_correct ans regs then begin
        print_endline ("Answer was correct but not in question format! "
                       ^"\nYou get " 
                       ^ string_of_int (if p=P1
                                        then fst st.current_wage
                                        else snd st.current_wage)
                       ^ " points deducted.");
        (if p=P1 then ((fst st.score)-(fst st.current_wage), snd st.score)
         else (fst st.score, (snd st.score)-(snd st.current_wage)))
      end
      else begin
        print_endline ("Incorrect. The correct question was: " ^ cor
                       ^"\nYou get " 
                       ^ string_of_int (if p=P1
                                        then fst st.current_wage
                                        else snd st.current_wage)
                       ^ " points deducted.");
        (if p=P1 then ((fst st.score)-(fst st.current_wage), snd st.score)
         else (fst st.score, (snd st.score)-(snd st.current_wage)))
      end;
    in_final = if st.in_final = Progress1 then Progress2 
      else if st.in_final = Progress2 then Done else None;
    round = st.round;
    turn = if p=P1 then P2 else P1;
    setting = st.setting;
    timer_time = st.timer_time
  }

(** [ans_cell ans cor regs st pt p] is the state [st] after an answer [ans] has 
    been submitted and compared to the correct response [cor] and [regs] and the
    score has been adjusted accordingly with the corresponding point value [pt]
    [ans_cell] is a helper function for [answer] specifically for clues in the 
    regular board *)
let ans_cell ans cor regs st pt p = 
  Legal {
    board = st.board;
    dd_cell_ids = st.dd_cell_ids;
    current_category_id = "";
    current_cell_id = "";
    current_clue = "";
    current_wage = (-1,-1);
    score = 
      if (Unix.time() -. st.timer_time) > 10.0
      then begin print_endline ("You did not answer within 10 seconds! " ^ 
                                "The correct question was: " ^ cor
                                ^"\nYou get " ^ string_of_int pt 
                                ^ " points deducted."); 
        (if p=P1 then ((fst st.score) - pt, snd st.score) 
         else (fst st.score, (snd st.score) - pt))
      end
      else if is_correct ans regs && (is_question ans || st.round = 1)
      then begin
        print_endline ("Correct! You get " ^ string_of_int pt ^ " points.");
        (if p=P1 then ((fst st.score) + pt, snd st.score) 
         else (fst st.score, (snd st.score) + pt))
      end
      else begin 
        print_endline ("Incorrect. The correct question was: " ^ cor
                       ^"\nYou get " ^ string_of_int pt 
                       ^ " points deducted.");
        (if p=P1 then ((fst st.score) - pt, snd st.score)
         else (fst st.score, (snd st.score) - pt)) end;
    in_final = None;
    round = st.round;
    turn = if p=P1 then P2 else P1;
    setting = st.setting;
    timer_time = st.timer_time
  }

(** [answer ans jeo st p] is [Legal of t] if answer is successful. Otherwise the 
    result is [Illegal]. *)
let answer ans jeo st p = 
  if ((is_daily_double st || (round_over st && st.round = 2)) 
      && (if p=P1 then fst st.current_wage = -1 else snd st.current_wage = -1))
  || st.current_clue = "" then Illegal 
  else if is_daily_double st || (round_over st && st.round = 2) then
    let curr_cat = st.current_category_id in
    let cor = if is_daily_double st
      then Jeopardy.get_answer jeo curr_cat st.current_cell_id st.round
      else Jeopardy.get_final_answer jeo in 
    let regs = if is_daily_double st
      then Jeopardy.get_regexp jeo curr_cat st.current_cell_id st.round
      else Jeopardy.get_final_regexp jeo in 
    ans_dd_final ans cor regs st p
  else 
    let cor = 
      Jeopardy.get_answer jeo st.current_category_id st.current_cell_id st.round 
    in
    let pt = 
      Jeopardy.get_points jeo st.current_category_id st.current_cell_id st.round 
    in
    let regs =
      Jeopardy.get_regexp jeo st.current_category_id st.current_cell_id st.round 
    in 
    ans_cell ans cor regs st pt p


(** [pass jeo st p] is [Illegal] if daily double or final round. Otherwise,
    passes the current question without answer. *)
let pass jeo st p = 
  if is_daily_double st || (round_over st && st.round = 2)
     || st.current_cell_id = "" then Illegal
  else begin
    let pt = 
      Jeopardy.get_points jeo st.current_category_id st.current_cell_id st.round
    in 
    Legal {
      board = st.board;
      dd_cell_ids = st.dd_cell_ids;
      current_category_id ="";
      current_cell_id = "";
      current_clue = "";
      current_wage = (-1, -1);
      score = if (Unix.time() -. st.timer_time) > 10.0
        then begin print_endline ("You took longer than 10 seconds!"
                                  ^"\nYou get " ^ string_of_int pt 
                                  ^ " points deducted.");
          if p=P1 then ((fst st.score) - pt, snd st.score) 
          else (fst st.score, (snd st.score) - pt)
        end
        else begin print_endline ("Passed. No points won or deducted.");
          st.score end;
      in_final = None;
      round = st.round;
      turn = if p = P1 then P2 else P1;
      setting = st.setting;
      timer_time = st.timer_time
    }
  end

(** [set jeo st setting] is [Illegal] if [st] is not at the beginning of round
    1 of game [jeo] or if [setting] is not ["person"] or ["bot" 0..9]. 
    Otherwise, sets Player 2 to [setting]. *)
let set jeo st setting =
  if (st.round = 1 && new_round st) && setting = "person" 
  then begin
    print_endline "Player 2 is now a person.";
    Legal {
      board = st.board;
      dd_cell_ids = st.dd_cell_ids;
      current_category_id ="";
      current_cell_id = "";
      current_clue = "";
      current_wage = (-1, -1);
      score = st.score;
      in_final = None;
      round = st.round;
      turn = st.turn;
      setting = Person;
      timer_time = st.timer_time
    } end
  else if (st.round = 1 && new_round st) && 
          (String.length setting = 5 && String.sub setting 0 4 = "bot "
           && String.sub setting 4 1 |> int_of_string >= 0
           && String.sub setting 4 1 |> int_of_string <= 9)
  then begin
    let difficulty = String.sub setting 4 1 |> int_of_string in 
    print_endline ("Player 2 is now a bot with difficulty level " 
                   ^ (string_of_int difficulty) ^ ".");
    Legal {
      board = st.board;
      dd_cell_ids = st.dd_cell_ids;
      current_category_id ="";
      current_cell_id = "";
      current_clue = "";
      current_wage = (-1, -1);
      score = st.score;
      in_final = None;
      round = st.round;
      turn = st.turn;
      setting = Bot difficulty;
      timer_time = st.timer_time
    } end
  else Illegal




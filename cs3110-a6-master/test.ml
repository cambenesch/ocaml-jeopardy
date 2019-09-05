open OUnit2
open Jeopardy
open State
open Command

let board1 = Yojson.Basic.from_file "cornell-board.json" |> from_json
let board1_category_ids =["OCaml"; "Places"; "Professors"; "Dining"; "Trivia"]
let board1_category1_cell_ids = ["OCaml 10"; "OCaml 20"; "OCaml 30"; "OCaml 40"; 
                                 "OCaml 50"]
let board1_final_clue = "The person who founded Cornell University."

let make_category_ids_test
    (name : string) 
    (jeo : Jeopardy.t)
    (round : round)
    (exp_output : category_id list) : test = 
  name >:: (fun _ -> 
      assert_equal exp_output (category_ids jeo round))

let make_cell_ids_test
    (name : string) 
    (jeo : Jeopardy.t)
    (category : category_id)
    (round : round)
    (exp_output : cell_id list) : test = 
  name >:: (fun _ -> 
      assert_equal exp_output (cell_ids jeo category round)) 

let make_get_points_test 
    (name : string) 
    (jeo: Jeopardy.t)
    (category: category_id)
    (cell: cell_id)
    (round: round)
    (exp_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal exp_output (get_points jeo category cell round)) 

let make_get_clue_test
    (name : string) 
    (jeo: Jeopardy.t)
    (category: category_id)
    (cell: cell_id)
    (round: round)
    (exp_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal exp_output (get_clue jeo category cell round)) 

let make_get_answer_test
    (name : string) 
    (jeo: Jeopardy.t)
    (category: category_id)
    (cell: cell_id)    
    (round: round)
    (exp_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal exp_output (get_answer jeo category cell round))

let make_get_final_clue_test
    (name : string) 
    (jeo: Jeopardy.t)
    (exp_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal exp_output (get_final_clue jeo))

let make_get_final_answer_test
    (name : string) 
    (jeo: Jeopardy.t)
    (exp_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal exp_output (get_final_answer jeo))

let jeopardy_tests = 
  [
    make_category_ids_test "test 1" board1 1 (board1_category_ids);
    make_cell_ids_test "test 2" board1 "OCaml" 1 board1_category1_cell_ids;
    make_get_points_test "test 5" board1 "OCaml" "OCaml 30" 1 30;
    make_get_clue_test "test 6" board1 "OCaml" "OCaml 40" 1 "An infinite list.";
    make_get_answer_test "test 7" board1 "OCaml" "OCaml 40" 1 
      "What is a stream?";
    make_get_final_clue_test "test 8" board1 board1_final_clue;
    make_get_final_answer_test "test 9" board1 "Who is Ezra Cornell?";
  ]

(** [make_parse_test name str exp_output] constructs an OUnit test named [name]
    that asserts the quality of [exp_output] with [parse str]. *)
let make_parse_test
    (name : string)
    (str : string)
    (exp_output : command) : test = 
  name >:: (fun _ ->
      assert_equal exp_output (parse str))

(** [make_parse__malf_test name str exp_output] constructs an OUnit test named
    [name] that asserts if [parse str] raises a Malformed exception. *)
let make_parse_malf_test
    (name : string)
    (str : string) : test = 
  name >:: (fun _ ->
      assert_raises (Malformed) (fun () -> parse str))

(** [make_parse__empty_test name str exp_output] constructs an OUnit test named
    [name] that asserts if [parse str] raises an Empty exception. *)
let make_parse_empty_test
    (name : string)
    (str : string) : test = 
  name >:: (fun _ ->
      assert_raises (Empty) (fun () -> parse str))

let command_tests =
  [
    make_parse_test "pick cell" "pick cell" (Pick ["cell"]);
    make_parse_test "pick    cell" "pick    cell" (Pick ["cell"]);
    make_parse_test "quit" "quit" Quit;
    make_parse_test "score" "score" Score;
    make_parse_test "score    " " score   " Score;
    make_parse_test "pass" "pass" Pass;
    make_parse_test "answer yes" "answer yes" (Answer["yes"]);
    make_parse_test "wager 100" "wager 100" (Wager["100"]);

    make_parse_malf_test "PICK clock" "PICK clock";
    make_parse_malf_test "score clock" "score clock";
    make_parse_malf_test "quit clock" "quit clock";
    make_parse_malf_test "wager" "wager";
    make_parse_malf_test "pick" "pick";
    make_parse_empty_test "" "";
    make_parse_empty_test "" "      "; 
  ]

let st = init_state board1
let () = display_board st

let next_st = match (pick "OCaml 30" board1 st P1) with
  | Legal x -> x
  | Illegal -> st
let () = display_board next_st

let next_st_clue = "To avoid requiring something to be stated more than once " ^ 
                   "by factoring out the recurring pattern."


let next_st2 = match (answer "blah" board1 next_st P1) with
  | Legal x -> x
  | Illegal -> next_st
let () = display_board next_st2

let next_st3 = match (pick "OCaml 20" board1 next_st2 P1) with
  | Legal x -> x
  | Illegal -> next_st2
let () = display_board next_st3

let next_st3_clue = "Picking test cases by just looking at the specification " ^ 
                    "and ignoring the implementation."

let next_st4 = 
  match (answer "What is black-box testing?" board1 next_st3 P1) with
  | Legal x -> x
  | Illegal -> next_st3
let () = display_board next_st4


let make_current_cell_id_test
    (name : string)
    (st : State.t)
    (exp_output : string) : test = 
  name >:: (fun _ ->
      assert_equal exp_output (current_cell_id st))

let make_current_clue_test
    (name : string)
    (st : State.t)
    (exp_output : string) : test = 
  name >:: (fun _ ->
      assert_equal exp_output (current_clue st))

let make_current_score_test
    (name : string)
    (st : State.t)
    (exp_output : int*int) : test = 
  name >:: (fun _ ->
      assert_equal exp_output (current_score st))

let state_tests = 
  [
    (*check initial state*)
    make_current_cell_id_test "init state" st "";
    make_current_cell_id_test "init clue" st "";

    (* pick cell and answer wrong*)
    make_current_cell_id_test "picked OCaml 30" next_st "OCaml 30";
    make_current_clue_test "OCaml 30 clue" next_st next_st_clue;
    make_current_score_test "OCaml 30 score" next_st2 (-30,0);

    make_current_cell_id_test "picked OCaml 20" next_st3 "OCaml 20";
    make_current_clue_test "OCaml20 clue" next_st3 next_st3_clue;
    make_current_score_test"before answer OCaml20 score" next_st3 (-30,0);
    make_current_score_test "OCaml20 score" next_st4 (-10,0);

  ]

(* *  test state -- prints board
   let st = init_state board1
   let () = display_board st
   let next_st = match (pick "30cat1" board1 st) with
   | Legal x -> x
   | Illegal -> failwith "error"
   let () = display_board next_st *)


let suite =
  "test suite for A6"  >::: List.flatten [
    jeopardy_tests;
    command_tests;
    state_tests;

  ]

let _ = run_test_tt_main suite


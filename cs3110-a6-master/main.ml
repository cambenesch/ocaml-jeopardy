
(* helper functions for act_on_com *)

(** [com_pick cell st jeo] is the  state when the cell [cell] in state [st] 
    has been picked by the player in the jeopardy game [jeo] *)
let com_pick cell st jeo p= 
  let state_res = State.pick cell jeo st p in
  let new_state res = 
    match res with
    | State.Legal t -> 
      if State.is_daily_double t then t
      else begin
        print_endline(State.current_clue t);
        print_endline "You have 10 seconds to answer or pass.";
        t
      end
    | State.Illegal -> 
      print_endline ("Cannot pick a category or invalid cell or need to answer" 
                     ^ " the clue or pass. Try again."); st
  in new_state state_res 

(** [com_ans cell st jeo] is the state when the player gives an answer [ans] for 
    the current picked cell of state [st] in the jeopardy game [jeo] *)
let com_answer ans st jeo p= 
  let state_res = State.answer ans jeo st p in
  let new_state res = 
    match res with
    | State.Legal t -> t
    | State.Illegal -> 
      print_endline ("Please pick a category first, wager first for Daily" ^
                     " Double or Final Jeopardy, or provide an answer."); st
  in new_state state_res 

(** [com_wager amt st jeo] is the state when the player wagers an amount [amt]
    for the current daily double or final jeopardy of state [st] in the jeopardy
    game [jeo] *)
let com_wager amt st jeo p= 
  let state_res = State.wager amt jeo st p in
  let new_state res = 
    match res with
    | State.Legal t -> begin 
        print_endline(State.current_clue t);
        print_endline "You have 20 seconds to answer.";
        t
      end
    | State.Illegal -> 
      print_endline ("Please enter a valid point value. You can only wager for"
                     ^ " Daily Double or Final Jeopardy. For Daily Double, you"
                     ^ " must wager between 5 points and your total score or"
                     ^ " the value of the cell picked, whichever is greater."
                     ^ " For Final Jeopardy, you must wager between 0 points"
                     ^ " and your total score."); st
  in new_state state_res

(** [com_set setting st jeo] is the state when the player sets Player 2 to be
    [setting] during [st] in the jeopardy game [jeo]. *)
let com_set setting st jeo =
  let state_res = State.set jeo st setting in
  let new_state res = 
    match res with
    | State.Legal t -> t
    | State.Illegal -> 
      print_endline ("You can only set Player 2 to be a bot or a person before "
                     ^ "the game has started."); st
  in new_state state_res

(** [act_on_com com st jeo] changes the state [st] of the jeopardy board 
    specified in the command [com] in jeopardy [jeo] if [com] is [Pick],  
    [Answer], or [Wager], prints the player's current score if [com] is [Score],
    quits the game if [com] is [Quit], and displays a win message with the score
    if all cells in the board have been picked and final jeopardy is over. 
    Requires: [com] is a valid command. *)
let act_on_com com st jeo p= 
  match com with 
  | Command.Score -> begin
      if State.round_over st && State.round_status st = 2
      then print_endline "Cannot check score during Final Jeopardy."
      else begin
        print_endline "Current score: ";
        print_endline ("  Player 1: " 
                       ^ string_of_int (fst (State.current_score st))); 
        print_endline ("  Player 2: " 
                       ^ string_of_int (snd (State.current_score st))); 
      end;  
      st 
    end
  | Command.Pass -> begin
      let nst = State.pass jeo st p in 
      match nst with 
      | Illegal -> print_endline ("Choose a category first. It is also illegal "
                                  ^ "to pass Daily Double or Final. \n"); st
      | Legal t -> t
    end
  | Command.Set object_phrase -> 
    let obj_phr = String.concat " " object_phrase in
    com_set obj_phr st jeo
  | Command.Pick object_phrase -> 
    let obj_phr = String.concat " " object_phrase in
    com_pick obj_phr st jeo p
  | Command.Answer object_phrase -> 
    let obj_phr = String.concat " " object_phrase in
    com_answer obj_phr st jeo p
  | Command.Wager object_phrase -> 
    let obj_phr = String.concat " " object_phrase  in 
    let amt = int_of_string obj_phr in
    com_wager amt st jeo p
  | Command.Quit -> print_endline "Goodbye."; exit 0 

(**  [prompt st jeo] prints the state of the current board and prompts the 
     player for a command. *)
let rec prompt st jeo =
  let p1s = fst (State.current_score st) in
  let p2s = snd (State.current_score st) in
  if State.in_final st = Done then begin 
    print_endline "Final Jeopardy Over! Final score: ";
    print_endline ("  Player 1: " ^ string_of_int p1s); 
    print_endline ("  Player 2: " ^ string_of_int p2s); 
    exit 0; end
  else begin
    let nst = 
      if State.round_over st && State.round_status st = 2
         && State.in_final st = None then begin
        if p1s <= 0 || p2s <= 0
        then begin
          if p1s <= 0 && p2s <= 0
          then
            print_endline ("Both players finished Double Jeopardy! with"
                           ^ " non-positive score. No winners.")
          else if p1s <= 0 then 
            print_endline ("Player 1 finished Double Jeopardy! with"
                           ^ " non-positive score. Player 2 wins!")
          else  
            print_endline ("Player 2 finished Double Jeopardy! with"
                           ^ " non-positive score. Player 1 wins!");
          print_endline "Final score: ";
          print_endline ("  Player 1: " ^ string_of_int p1s); 
          print_endline ("  Player 2: " ^ string_of_int p2s); 
          exit 0; end
        else begin
          print_endline "Double Jeopardy Over! Time for Final Jeopardy. ";
          print_endline "Current score: ";
          print_endline ("  Player 1: " ^ string_of_int p1s); 
          print_endline ("  Player 2: " ^ string_of_int p2s);
          print_string ("Player " ^ string_of_int (if State.player_turn st =P1
                                                   then 1 else 2));
          print_endline ", please wager some points.";
          st
        end
      end
      else if State.in_final st = Progress2 
           && (if State.player_turn st = P1 
               then fst (State.current_wage st) = -1 
               else snd (State.current_wage st) = -1 ) 
      then begin
        ANSITerminal.erase(Screen);
        print_string ("Player " ^ string_of_int (if State.player_turn st =P1 
                                                 then 1 else 2));
        print_endline (", please wager some points. Remember, you cannot check"
                       ^ " the score during Final Jeopardy.");
        st
      end
      else if (State.round_over st && State.round_status st = 1) then begin 
        print_endline ("Jeopardy Over! Time for Double Jeopardy! Remember, if " 
                       ^ "you do not answer in the form of a question, you will"
                       ^ " get penalized.");
        if p1s <> p2s then
          let player = if p1s < p2s then "1" else "2" in
          print_endline ("Player " ^ player ^ " is behind. Player " ^ player
                         ^ " goes first.")
        else ();
        State.init_state_second jeo st 
      end
      else if (State.new_round st && State.round_status st = 1) then begin
        print_endline ("Please set Player 2 to be either a person or a bot with"
                       ^ " difficulty level 0..9. If not set, Player 2 is a bot"
                       ^ " with difficulty level 5.");
        st
      end
      else st in 
    begin
      let p =State.player_turn nst in
      if(State.current_clue nst = "" && State.in_final nst = None 
         && not (State.round_over st && State.round_status st = 2))
      then (* display board *) begin
        print_endline "Current Jeopardy Board:"; 
        State.display_board nst; 
        print_string ("Player " ^  string_of_int (if p=P1 then 1 else 2)
                      ^ "'s turn. ")
      end;
      print_endline ("Please enter the command.\n");
      print_string "> ";
      let line = 
        if p=P1||State.get_setting nst = Person 
        then read_line() 
        else Bot.next_cmd jeo nst in
      match Command.parse line with 
      | exception Command.Empty -> begin 
          print_endline "Empty command.";
          prompt nst jeo
        end
      | exception Command.Malformed -> begin
          print_endline "Invalid command. Please start command with \n
           \"set\" to set Player 2 to \"person\" or \"bot\",\n
           \"score\" to view your current score,\n
           \"pick\" to choose a category and cell separated by whitespace,\n 
           \"answer\" to answer in the form of a question,\n
           \"pass\" to pass the question,\n
           \"wager\" to wager a point value, or\n
           \"quit\" to quit the game.\n";
          prompt nst jeo
        end 
      | cmd ->let nnst = act_on_com cmd nst jeo p in 
        prompt nnst jeo
    end
  end 

(** [play_game f] starts the jeopardy game in file [f]. 
    It checks to see that the file for exists and if it doesn't prompts 
    the user again for a valid board file *)
let rec play_game f =
  if Sys.file_exists f then
    let jeo = Yojson.Basic.from_file f |> Jeopardy.from_json in
    let st = State.init_state jeo in 
    prompt st jeo
  else begin
    print_endline "File does not exist, try again."; 
    print_endline "Please enter the name of the game file you want to load.\n";
    print_string  "> ";
    play_game (read_line ())
  end 

(** [main ()] prompts for the game to play, then starts it. *)
let main ()=
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to Jeopardy!\n");

  print_endline "Please enter the name of the board file you want to load.\n";
  print_string  "> "; 

  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()

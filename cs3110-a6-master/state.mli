(** The abstract type of values representing the game state. *)
type t 

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

(** [init_state jeo] is the initial state of round 1 of the game [jeo]. 
    In that state all questions in round 1 are unanswered, there is no question
    currently being answered, and the score is [0] for both players. 
    There is one daily double in the board. *)
val init_state : Jeopardy.t -> t

(** [init_state_second jeo st] is the initial state of round 2 of the
    game [jeo]. 
    In that state all questions in round 2 are unanswered, there is no question
    currently being answered, and the score is carried over from round 1.
    There are two daily doubles in the board. *)
val init_state_second : Jeopardy.t -> t -> t

(** [display st] print out all cells in [st] *)
val display_board: t-> unit

(** [is_daily_double st] is true if current picked cell is daily double. *)
val is_daily_double: t ->bool

(** [current_wage st] is the current wage that the player has declared. *)
val current_wage: t ->int*int

(** [current_category_id st] is the current category identifier picked by player 
    in state [st]. *)
val current_category_id : t -> string

(** [current_cell_id st] is the current cell identifier picked by player 
    in state [st]. *)
val current_cell_id : t -> string

(** [current_clue st] is the current clue waiting for answer. *)
val current_clue : t -> string

(** [current_score st] is the sum of points that the player has currently
     earned. *)
val current_score : t -> int*int

(** [round_over st] is [true] if all questions in [st] excluding final 
    are answered. *)
val round_over: t-> bool

(** [new_round st] is [true] if all question in [st] are unanswered. *)
val new_round: t-> bool

(** The type representing final jeopardy *)
type final = None | Progress1 | Progress2 | Done

(** The type representing the current player. *)
type player = P1 | P2

(** The type representing player 2. *)
type setting = Person | Bot of int

(** [in_final st] is the status of final jeopardy round.*)
val in_final: t -> final 

(** [round_status st] is the current round that [st] is in. *)
val round_status: t -> int

(** [player_turn st] is the player who has the turn in the state [st]. *)
val player_turn: t -> player

(** [get_setting st] is the setting of Player 2, a human or bot with difficulty
    level. *)
val get_setting: t -> setting

(** [cells_not_picked st] is the available cells which are not picked in [st].*)
val cells_not_picked: t -> string list

(** [pick cell jeo st p] is [Legal of t] if cell is not already picked, 
    Otherwise the result is [Illegal]. *)
val pick: Jeopardy.cell_id -> Jeopardy.t -> t -> player -> result

(** [wager wage jeo st p] is [Legal of t] if daily double or final jeopardy, 
    Otherwise the result is [Illegal]. *)
val wager: int -> Jeopardy.t -> t -> player -> result

(** [answer ans jeo st p] is [Legal of t] if answer is successful, Otherwise the 
    result is [Illegal]. *)
val answer: string -> Jeopardy.t -> t -> player -> result

(** [pass jeo st p] is [Legal of t], just pass the current question without
    answer. 
    If daily double or final round, it is [Illegal] *)
val pass: Jeopardy.t -> t -> player -> result

(** [set jeo st setting] is [Illegal] if [st] is not at the beginning of round
    1 of game [jeo] or if [setting] is not ["person"] or ["bot" 0..9]. 
    Otherwise, sets Player 2 to [setting]. *)
val set: Jeopardy.t -> t -> string -> result
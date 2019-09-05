(** The abstract type of values representing boards. *)
type t

type round = int

(** The type of category identifiers. *)
type category_id = string

(** The type of cell identifiers. *)
type cell_id = string

(** The type of clues. *)
type clue = string

(** The type of answers. *)
type answer = string

(** The type of accepted regular expressions. *)
type regexp = string

(** Raised when an unknown category is encountered. *)
exception UnknownCategory of category_id

(** Raised when an unknown cell is encountered. *)
exception UnknownCell of cell_id

(** The type of cells is represented by a list of tuples containing the cell
    identifier, the points, the clue, and the answer. *)
type cells = (cell_id*int*clue*answer*regexp) list

(** The type of a category is represented by a pair containing the category
    identifier and the cells in the category. *)
type category = round*category_id*cells

(** The type of final jeopardy is represented by a tuple containing the clue and
    the answer and the accepted regexp. *)
type final = (clue*answer*regexp)

(** [from_json j] is the jeopardy board that [j] represents.
    Requires: [j] is a valid JSON jeopardy representation. *)
val from_json : Yojson.Basic.json -> t

(** [category_ids jeo round] is a set-like list of all of the category
    identifiers in round [round] of jeopardy board [jeo]. *)
val category_ids : t -> round -> category_id list

(** [cell_ids jeo category round] is an ordered list of all of the cell
    identifiers from category [category] in round [round] of jeopardy [jeo]. *)
val cell_ids : t -> category_id -> round -> cell_id list

(** [get_cells jeo category round] is a ordered list of all the cells in a
    category identifier [category].
    Raises [UnknownCategory category] if [category] is not a 
    category identifier. *)
val get_cells : t -> category_id -> int-> cells 

(**  [get_points jeo category cell round] is the point value of [cell] in
     [category] in round [round] of board [jeo].  
     Raises [UnknownCategory category] if [category] is not a category
     identifier in round [round] of [jeo].
     Raises [UnknownCell cell] if [cell] is not a cell in category [category] in
     [jeo]. *)
val get_points : t -> category_id -> cell_id -> round -> int

(**  [get_clue jeo category cell round] is the clue of [cell] in [category] in
     round [round] of board [jeo].  
     Raises [UnknownCategory category] if [category] is not a category
     identifier in round [round] of [jeo].
     Raises [UnknownCell cell] if [cell] is not a cell in category [category] in
     [jeo]. *)
val get_clue : t -> category_id -> cell_id -> round -> clue

(**  [get_answer jeo category cell round] is the answer of [cell] in [category]
     in round [round] of  board [jeo].  
     Raises [UnknownCategory category] if [category] is not a category
     identifier in round [round] of [jeo].
     Raises [UnknownCell cell] if [cell] is not a cell in category [category] in
     [jeo]. *)
val get_answer : t -> category_id -> cell_id -> round -> answer

(**  [get_regexp jeo category cell round] is the regexp of [cell] in [category]
     in round [round] of board [jeo].  
     Raises [UnknownCategory category] if [category] is not a category
     identifier in round [round] of[jeo].
     Raises [UnknownCell cell] if [cell] is not a cell in category [category] in
     round [round] of [jeo]. *)
val get_regexp : t -> category_id -> cell_id -> round -> regexp

(** [get_final_clue jeo] is the clue of Final Jeopardy in board [jeo]. *)
val get_final_clue : t -> clue

(** [get_final_answer jeo] is the answer of Final Jeopardy in board [jeo]. *)
val get_final_answer : t -> answer

(** [get_final_regexp jeo] is the regexp of Final Jeopardy in board [jeo]. *)
val get_final_regexp : t -> regexp

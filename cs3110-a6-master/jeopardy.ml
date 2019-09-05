open Yojson.Basic.Util

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
    identifier, the points, the clue, the answer, and the accepted regexp. *)
type cells = (cell_id*int*clue*answer*regexp) list

(** The type of a category is represented by a pair containing the category
    identifier and the cells in the category. *)
type category = round*category_id*cells

(** The type of final jeopardy is represented by a tuple containing the clue and
    the answer and the accepted regexp. *)
type final = (clue*answer*regexp)

(** The type of a jeopardy board represented by the categories it contains and
    the final jeopardy question. *)
type t = {
  categories : category list;
  final : final
}

(** [from_json json] is the jeopardy board that [json] represents.
    Requires: [j] is a valid JSON adventure representation. *)
let from_json json = {
  final = (let final_of_json j = 
             let clue = j |> member "clue" |> to_string in 
             let answer = j |> member "answer" |> to_string in 
             let regexp = j |> member "regexp" |> to_string in 
             (clue, answer, regexp) in 
           json |> member "final" |> final_of_json); 
  categories = 
    let cell_of_json j = 
      let cell_id = j |> member "cell id" |> to_string in
      let points = j |> member "points" |> to_int in
      let clue = j |> member "clue" |> to_string in
      let answer = j |> member "answer" |> to_string in
      let regexp = j |> member "regexp" |> to_string in 
      (cell_id, points, clue, answer, regexp) in
    let category_of_json j = 
      let round = j |> member "round" |> to_int in
      let category_id = j |> member "id" |> to_string in
      let cells = j |> member "cells" |> to_list |> List.map cell_of_json 
      in (round, category_id, cells) in
    json |> member "categories" |> to_list |> List.map category_of_json}

(** [category_ids jeo round] is an ordered list of all of the category 
    identifiers in jeopardy board [jeo] in round [round]. *)
let category_ids jeo round = 
  (** [get_categories acc categories] is an ordered list of all the 
      category identifiers in a jeopardy board [categories] prepended onto
      [acc]. *)
  let rec get_categories round acc = function
    | [] -> List.rev acc
    | (x, y, z)::t when x = round -> get_categories round (y::acc) t 
    | (x, y, z)::t -> get_categories round acc t in
  get_categories round [] jeo.categories

(** [get_cells jeo category round] is an ordered list of all the cells in a
    category identifier [category].
    Raises [UnknownCategory category] if [category] is not a category 
    identifier. *)
let get_cells jeo category round =
  let rec find_category round = function
    | [] -> raise (UnknownCategory category)
    | (w, x, c)::t when x = category && w = round -> c
    | (w, x, c)::t -> find_category round t in
  find_category round jeo.categories

(** [cell_ids jeo captegory round] is a set-like list of all cell ids from 
    category [category] in round [round] of jeopardy [jeo].
    Raises [UnknownCategory category] if [category] is not a category 
    identifier.*)
let cell_ids jeo category round = 
  List.map (fun (w,x,y,z,a) -> w) (get_cells jeo category round)

(**  [get_points jeo category cell] is the point value of [cell] in [category]
     in round [round] of board [jeo].  
     Raises [UnknownCategory category] if [category] is not a category
     identifier in round [round] of [jeo].
     Raises [UnknownCell cell] if [cell] is not a cell in category [category] in
     round [round] of [jeo]. *)
let get_points jeo category cell round = 
  let rec find_cell = function
    | [] -> raise (UnknownCell cell)
    | (w, x, y, z, a)::t when w = cell -> x
    | (w, x, y, z, a)::t -> find_cell t
  in get_cells jeo category round |> find_cell

(**  [get_clue jeo category cell] is the clue of [cell] in [category] in round
     [round] of board [jeo].
     Raises [UnknownCategory category] if [category] is not a category
     identifier in round [round] of [jeo].
     Raises [UnknownCell cell] if [cell] is not a cell in category [category] in
     round [round] of [jeo]. *)
let get_clue jeo category cell round = 
  let rec find_cell = function
    | [] -> raise (UnknownCell cell)
    | (w, x, y, z, a)::t when w = cell -> y
    | (w, x, y, z, a)::t -> find_cell t
  in get_cells jeo category round |> find_cell

(**  [get_answer jeo category cell] is the answer of [cell] in [category] in 
     board [jeo].
     Raises [UnknownCategory category] if [category] is not a category
     identifier in round [round] of [jeo].
     Raises [UnknownCell cell] if [cell] is not a cell in category [category] in
     round [round] of [jeo]. *)
let get_answer jeo category cell round = 
  let rec find_cell = function
    | [] -> raise (UnknownCell cell)
    | (w, x, y, z, a)::t when w = cell -> z
    | (w, x, y, z, a)::t -> find_cell t
  in get_cells jeo category round |> find_cell

(**  [get_regexp jeo category cell] is the regexp of [cell] in [category] in 
     round [round] of board [jeo].
     Raises [UnknownCategory category] if [category] is not a category
     identifier in round [round] of [jeo].
     Raises [UnknownCell cell] if [cell] is not a cell in category [category] in
     round [round] of [jeo]. *)
let get_regexp jeo category cell round = 
  let rec find_cell = function
    | [] -> raise (UnknownCell cell)
    | (w, x, y, z, a)::t when w = cell -> a
    | (w, x, y, z, a)::t -> find_cell t
  in get_cells jeo category round |> find_cell

(** [get_final_clue jeo] is the clue of Final Jeopardy in board [jeo]. *)
let get_final_clue jeo = match jeo.final with
  | (x, y, a) -> x

(** [get_final_answer jeo] is the answer of Final Jeopardy in board [jeo]. *)
let get_final_answer jeo = match jeo.final with
  | (x, y, a) -> y

(** [get_final_regexp jeo] is the regexp of Final Jeopardy in board [jeo]. *)
let get_final_regexp jeo = match jeo.final with
  | (x, y, a) -> a
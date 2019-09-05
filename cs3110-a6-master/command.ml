(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["pick Colors 20"], then the object phrase is 
      [["colors"; "20"]].
    - If the player command is ["pick Colors    20"], then the object phrase is
      again [["colors"; "20"]]. 
      An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Answer of object_phrase
  | Pick of object_phrase
  | Pass 
  | Quit
  | Score
  | Wager of object_phrase
  | Set of object_phrase

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    pick    Colors  20   "] is [Pick ["Colors"; "20"]]
    - [parse "quit"] is [Quit]
    - [parse "score"] is [Score].  
      Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
      characters (only ASCII character code 32; not tabs or newlines, etc.).
      Raises: [Empty] if [str] is the empty string or contains only spaces. 
      Raises: [Malformed] if the command is malformed. A command
      is {i malformed} if the verb is not "quit", "score", "pick", "answer", 
      "pass", or "wager", or if the verb is "quit", "pass", or "score" and
      there  is a non-empty object phrase, or if the verb is "pick", "wager", or
      "answer" and there is an empty object phrase. *)
let parse str =
  (* print_endline str;  *)
  let parse_list = function
    | [] -> raise Empty 
    | h::t -> if t = [] then
        if (String.equal h "score") then Score
        else if (String.equal h "pass") then Pass
        else if (String.equal h "quit") then Quit
        else raise Malformed else
      if (String.equal h "answer") then Answer t 
      else if (String.equal h "pick") then Pick t
      else if (String.equal h "wager") then Wager t
      else if (String.equal h "set") then Set t
      else raise Malformed in
  String.split_on_char ' ' str |> List.filter ((<>) "") |> parse_list 

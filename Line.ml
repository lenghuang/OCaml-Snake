let diff () = (*User input that asks for an integer to determine level of difficulty*)
  print_string "Type a number on a scale of 1 to 5 of how difficult you want the game to be. 1 being Braindead Easy and 5 being Impossible.\nInput: ";
  read_int ()

let cls () = print_string "\x1B[2J";; (*CLears the screen except for gameboard for aesthetic purposes. Uses terminal code*)

let set_pos (x,y) =
  print_string ("\x1b[" ^ string_of_int y ^ ";" ^ string_of_int x ^ "H");;
  (*Converts the int values of tuple coordinate in order to concatenate it to the terminal code*)

let show_board () =
  set_pos (0,0);
  print_endline "+--------------------+";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "|                    |";
  print_endline "+--------------------+";;

(*Snake itself is a list of tuples which represent the coordinates*)
let show_snake_hd snake =
  let (x,y) = List.hd snake
  in set_pos(x+1, y+1); print_char '#';;

let show_snake_tl snake =
  ignore(List.map (fun (x,y) -> set_pos(x+1, y+1); print_char 'O') (List.tl snake));;

let show_snake snake =
  show_snake_hd snake; show_snake_tl snake;;

(*Same logic as show_snake, but with wall.*)
let show_walls walls =
  ignore(List.map (fun (x,y) -> set_pos(x+1, y+1); print_char 'x') walls);;

(*Same logic as show_snake, but with apple.*)
let show_apple (x,y) = set_pos(x+1, y+1); print_char 'a';;

(*Helper function for main function. Resets board with new things*)
let show_game apple snake walls =
  cls (); (*Clears screen*)
  show_board(); (*Prints board*)
  show_apple apple; (*Places apple*)
  show_snake snake; (*Places snake*)
  show_walls walls; (*Places walls*)
  set_pos(0,18);; (*Set line of input*)

(*Defines direction of snakes movement*)
type move = Up | Down | Right | Left;;


let rec get_move ()=
  let s = read_line() in (*reads in line as a string*)
  if String.length s = 0 then
    get_move() (*if person inputs nothing repeat*)
  else
    match s.[0] with (*comapre index 0 of input with types*)
    | 'w' -> Up | 's' -> Down | 'a' -> Left | 'd' -> Right
    | _ -> get_move();;

(*Generates coordinate point from 0-15 and adds one to account for wall*)
let rec new_apple snake walls =
  let x = (Random.int 20) + 1 and
      y = (Random.int 14) + 1 in
  if List.mem (x,y) snake then (*classhecks if tuple is in list snake. Retry if apple is on snake*)
    new_apple snake walls
  else if List.mem (x,y) walls then
    new_apple snake walls
  else
    (x,y);; (*it works there and put it in*)

(*Add tuple coordinates into existing snake body*)
let add_head move snake =
  match snake with
  |  [] -> []
  | ((x, y) :: _) ->
    match move with
    | Up    -> (x,y-1) :: snake
    | Down  -> (x,y+1) :: snake
    | Right -> (x+1,y) :: snake
    | Left  -> (x-1,y) :: snake;;

(*Gets rid of the last element in snake*)
let drop_tail = function
  |  [] -> [];
  | snake -> List.rev (List.tl (List.rev snake));;

(*Checks if head of snake is the same as coord of apple, which means it is eaten*)
let is_eated apple snake =
  match snake with
  |  [] -> false
  | (head :: body) -> head = apple;;

(*Makes snake' to temporarily check what will happen. If it will eat an apple, then add to it. If not, remove the last, AKA move up*)
let update_state move snake apple walls =
  let snake' = add_head move snake in
  if is_eated apple snake' then
    (snake', new_apple snake' walls)
  else
    (drop_tail snake', apple);;

(*Defines bounds in which game will stop*)
let is_gameover snake walls =
  match snake with
  |  [] -> false
  | (((x,y) as head) :: body) ->
      x = 0 || x = 21 || y = 0 || y = 15 || List.mem head body || List.mem head walls;; (*Dim of box + 1*)

(*Recursive function to forecase whether snake fails or not*)
let rec game_loop snake (*list of tuples*) apple (*tuple*) walls =
  let move = get_move() in (*get the move*)
  let (snake', apple') = update_state move snake apple walls in (*calls in forecast*)
  show_game apple' snake' walls; (*restarts board with new parameters*)
  if is_gameover snake' walls then (*if for future case snake, game ends, end.*)
    print_endline ("Game over snake length:" ^
                    string_of_int (List.length snake'))
  else
    game_loop snake' apple' walls;; (*else continue looping the game*)

(*Make a list of tuples that represent the coord of each wall*)
let rec make_walls (i:int) =
  match i with
  | 0 -> []
  | _ -> ((Random.int 20) + 1, (Random.int 14) + 1) :: make_walls (i-1);;

(*Function that determines range of integer for random wall generation*)
let wall_param diff () =
  match diff () with
  | 1 -> 0
  | 2 -> (Random.int 10) + 1 (*Range 1 - 11*)
  | 3 -> (Random.int 10) + 11 (*Range 11 - 20*)
  | 4 -> (Random.int 10) + 21 (*Range 21 - 30*)
  | 5 -> (Random.int 20) + 31 (*Range 31 - 50*)
  | _ -> 100 (*Lol you messed up bro*)

(* main function *)
let () =
  Random.self_init();
  let walls = make_walls (wall_param diff ()) in
  let snake = [(1,1)] in (*Sets coord for snake and makes it size 1x1*)
  let apple = new_apple snake walls in
  show_game apple snake walls;
  game_loop snake apple walls;;
  (*run the game!*)

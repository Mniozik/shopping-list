(* 
let clear_terminal    =    zdefiniowana jako wartosc
let clear_terminal () =    zdefiniowana jako funkcja [musimy dac 'pusty nawias' jesli nie ma argumentow podawanych]*)

(* open Printf *)

(* ocamlc shoppinglist.ml -o shoppinglist *)

(* Funkcja do czyszczenia terminalu *)
let clear_terminal () =  
  let m_command = match Sys.os_type with
    | "Unix" -> "clear"
    | "Win32" -> "cls"
    | _ -> "" 
  in 
  ignore (Sys.command m_command)

(* Funkcja do wczytywania listy zakupowej z pliku *)
let load_list () =
  try
    let in_channel = open_in "shopping_list.txt" in
    let rec read_lines lines =
      try
        let line = input_line in_channel in
        read_lines (line :: lines)
      with
        End_of_file -> List.rev lines
    in
    let lines = read_lines [] in
    close_in in_channel;
    lines
  with
    | Sys_error _ -> []


(* Funkcja do zapisania listy zakupowej do pliku *)
let save_list list =
  let out_channel = open_out "shopping_list.txt" in
    List.iter (fun item -> Printf.fprintf out_channel "%s\n" item) list;
  close_out out_channel

(* Funkcja do dodawania elementu do listy zakupowej *)
let add_item item list =
  item :: list

(* Funkcja do usuwania elementu z listy zakupowej *)
let remove_item item list =
  List.filter (fun x -> x <> item) list

(* Funkcja do wyświetlania listy zakupowej *)
let print_list list =
  if List.length list = 0 then
    print_endline "-                      -"
  else
    let numbered_list = List.mapi (fun i item -> string_of_int (i+1) ^ ". " ^ item) list in 
    List.iter (fun item -> print_endline item) numbered_list

(* Główna funkcja programu *)
let main () =
  clear_terminal ();

  (* Inicjalizacja listy zakupowej *)
  let shopping_list = ref (load_list ()) in

  (* Pętla programu *)
  while true do
    
    (* Wyświetlenie listy zakupowej *)
    print_endline "---- Lista zakupowa ----";
    print_list !shopping_list;
    print_endline "------------------------\n";

    (* Wyświetlenie menu *)
    print_endline "-------------- MENU --------------";
    print_endline "Wybierz co chcesz zrobic:";
    print_endline "1. Dodaj produkt do listy";
    print_endline "2. Usun dany produkt z listy";
    print_endline "3. Usun wszystkie produkty z listy";
    print_endline "4. Zapisz liste do pliku";
    print_endline "5. Wyjscie";
    print_endline "----------------------------------";

    (* Oczekiwanie na wybór użytkownika *)
    let choice = read_int () in
 
    (* Wykonanie akcji na podstawie wyboru użytkownika *)
    match choice with
    | 1 ->
      print_endline "\nPodaj nazwe produktu:";
      let item = read_line () in
      shopping_list := add_item item !shopping_list;
      clear_terminal ();
      print_endline "Produkt zostal dodany do listy.\n";

    | 2 ->
      print_endline "\nPodaj nazwe produktu do usuniecia:";
      let item = read_line () in
      shopping_list := remove_item item !shopping_list;
      clear_terminal ();
      print_endline "Produkt zostal usuniety z listy.\n";

    | 3 -> 
      shopping_list := [];
      clear_terminal ();
      print_endline "Wszystkie produkty z listy zostaly usuniete.\n";

    | 4 ->
      save_list !shopping_list;
      clear_terminal ();
      print_endline "Lista zakupowa zostala zapisana do pliku.\n";
      
    | 5 ->
      (* Wyjście z programu *)
      print_endline "\nWyjscie z menu.";
      exit 0;

    | _ ->
      clear_terminal();
      print_endline "Nieprawidlowy wybor.\n";
  done;;

(* Wywołanie głównej funkcji programu *)
main ();;

(* ocamlc shoppinglist.ml -o shoppinglist *)


(* Deklaracja nowego typu 'item' jako rekord *)
type item = {
  name : string;
  quantity : int;
}

(* Funkcja do czyszczenia terminalu *)
let clear_terminal () =  
  let command = match Sys.os_type with
    | "Unix" -> "clear"
    | "Win32" -> "cls"
    | _ -> "" 
  in 
  ignore (Sys.command command)

(* Funkcja do wczytywania listy zakupowej z pliku *)
let load_list () =
  try (* rozpoczecie bloku try, w ktorym znajduje sie kod ktory moze zglosic wyjatek*)
    let in_channel = open_in "shopping_list.txt" in
    let rec read_lines lines =
      try
        let line = input_line in_channel in
        let splitted = String.split_on_char ' ' line in
        match splitted with
        | name :: quantity_str :: _ ->
          let quantity = int_of_string quantity_str in
          read_lines ({ name = name; quantity = quantity } :: lines)
        | _ -> read_lines lines
      with
        End_of_file -> List.rev lines
    in
    let lines = read_lines [] in
    close_in in_channel;
    lines
  with
    | Sys_error _ -> []


(* Funkcja do zapisywania listy zakupowej do pliku *)
let save_list list =
  let out_channel = open_out "shopping_list.txt" in
    List.iter (fun item -> Printf.fprintf out_channel "%s %d\n" item.name item.quantity) list;
  close_out out_channel


(* Funkcja do dodawania produktu (elementu) do listy zakupowej *)
let add_item name quantity list =
  { name = name; quantity = quantity } :: list

(* Funkcja do usuwania elementu z listy zakupowej *)
let remove_item item_name list =
  List.filter (fun x -> x.name <> item_name) list

(* Funkcja do wyświetlania listy zakupowej *)
let print_list list =
  if List.length list = 0 then
    print_endline "-                      -"
  else
    let numbered_list = List.mapi (fun i item -> string_of_int (i+1) ^ ". " ^ item.name ^ " (ilosc: " ^ string_of_int item.quantity ^ ")") list in 
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
      let item_name = read_line () in
      print_endline "\nPodaj ilosc produktu:";
      let item_quantity = read_int () in
      shopping_list := add_item item_name item_quantity !shopping_list;
      clear_terminal ();
      print_endline "Produkt zostal dodany do listy.\n";

    | 2 ->
      print_endline "\nPodaj nazwe produktu do usuniecia:";
      let item_name = read_line () in
      shopping_list := remove_item item_name !shopping_list;
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
      print_endline "Nieprawidlowy wybor. \n";
  done;;

(* Wywołanie głównej funkcji programu *)
main ();;

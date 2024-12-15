open Batteries

let rec move map (y, x) dir =
  let (y2, x2) = dir (y, x) in
  match Grid.get map (y2, x2) with
  | '#' -> false
  | '.' ->
      Grid.set map (y2, x2) @@ Grid.get map (y, x);
      Grid.set map (y, x) '.';
      true
  | 'O' ->
      if move map (y2, x2) dir then
        move map (y, x) dir
      else false 
  | c ->
      let backup = Grid.copy map in
      if y <> y2 then
        let shift_x = if c = '[' then 1 else -1 in
        if move backup (y2, x2) dir && move backup (y2, x2 + shift_x) dir then (
          ignore @@ move map (y2, x2) dir;
          ignore @@ move map (y2, x2 + shift_x) dir;
          move map (y, x) dir)
        else false
      else
        if move map (y2, x2) dir then (
          Grid.set map (y2, x2) @@ Grid.get map (y, x);
          Grid.set map (y, x) '.';
          true)
        else false

let part1 =
  let lines = Line_oriented.lines_of_file "input.txt" in
  let map =
    lines
    |> List.take_while (flip String.contains '#')
    |> Array.of_list % List.map (Array.of_list % String.to_list)
    |> Grid.copy
  in
  let dirs =
    lines
    |> List.drop_while (not % flip String.contains '<')
    |> String.concat "" 
  in
  
  let player_pos = Grid.find (fun _ c -> c = '@') in

  String.iter (fun c ->
    ignore @@ move map (player_pos map) @@ match c with
    | '^' -> Grid.north
    | '>' -> Grid.east
    | 'v' -> Grid.south
    | '<' -> Grid.west
    | _ -> failwith "Unknown direction"
  ) dirs;

  Grid.fold (fun (y, x) c acc ->
    if c = 'O' then acc + (100 * y) + x else acc
  ) map 0

let double_width map =
  Array.map (fun line ->
    line
    |> Array.to_list
    |> List.concat_map (fun c ->
        match c with
        | '#' -> ['#'; '#']
        | 'O' -> ['['; ']']
        | '.' -> ['.'; '.']
        | '@' -> ['@'; '.']
        | _ -> failwith "Impossible character in map")
    |> Array.of_list
  ) map

let part2 = 
  let lines = Line_oriented.lines_of_file "input.txt" in
  let map =
    lines
    |> List.take_while (flip String.contains '#')
    |> Array.of_list % List.map (Array.of_list % String.to_list)
    |> double_width
    |> Grid.copy
  in
  let dirs =
    lines
    |> List.drop_while (not % flip String.contains '<')
    |> String.concat "" 
  in
  
  let player_pos = Grid.find (fun _ c -> c = '@') in

  String.iter (fun c ->
    ignore @@ move map (player_pos map) @@ match c with
    | '^' -> Grid.north
    | '>' -> Grid.east
    | 'v' -> Grid.south
    | '<' -> Grid.west
    | _ -> failwith "Unknown direction"
  ) dirs;
  
  Grid.fold (fun (y, x) c acc ->
    if c = '[' then acc + (100 * y) + x else acc
  ) map 0

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2

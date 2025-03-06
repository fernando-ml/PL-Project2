(* utility functions *)
(* padding function to ensure 8 bits *)
let pad_to_8 lst = 
  let len = List.length lst in
  if len < 8 then
    List.init (8 - len) (fun _ -> 0) @ lst
  else if len > 8 then
    List.rev (List.filteri (fun i _ -> i < 8) (List.rev lst))
  else lst

(* convert decimal to binary list - for inputs *)
let decimal_to_binary num =
  let n = abs num in (* to handle negative numbers *)
  let rec convert remaining powers acc =
    match powers with
    | [] -> List.rev acc
    | power :: rest ->
        let bit = if remaining >= power then 1 else 0 in
        let new_remaining = if bit = 1 then remaining - power else remaining in
        convert new_remaining rest (bit :: acc)
  in
  convert n [128; 64; 32; 16; 8; 4; 2; 1] []

(* convert binary list to decimal - for results *)
let binary_to_decimal binary =
  let powers = [128; 64; 32; 16; 8; 4; 2; 1] in
  let rec convert bits powers acc =
    match (bits, powers) with
    | [], _ | _, [] -> acc
    | bit :: rest_bits, power :: rest_powers ->
        let value = if bit = 1 then power else 0 in
        convert rest_bits rest_powers (acc + value)
  in
  convert binary powers 0

(* convert binary list to signed decimal (using two's complement interpretation) *)
let binary_to_signed_decimal binary =
  (* so if the most significant bit is 0, it's a positive number *)
  if List.hd binary = 0 then
    binary_to_decimal binary
  else
    (* however, if the most significant bit is 1, it's a negative number in two's complement *)
    let value = binary_to_decimal binary in
    value - 256  (* subtract 2^8 to get the correct negative value *)

(* binary operations *)
(* logical operations *)
(* NOT: flip all bits *)
let binary_not a =
  List.map (fun x -> if x = 0 then 1 else 0) a

(* AND: 1 AND 1 = 1, otherwise 0 *)
let binary_and a b =
  List.map2 (fun x y -> if x = 1 && y = 1 then 1 else 0) a b

(* OR: 0 OR 0 = 0, otherwise 1 *)
let binary_or a b =
  List.map2 (fun x y -> if x = 1 || y = 1 then 1 else 0) a b

(* XOR: 1 if bits are different, 0 if bits are the same *)
let binary_xor a b =
  List.map2 (fun x y -> if x = y then 0 else 1) a b

(* Arithmetic operations *)
(* addition implementation - adding from least significant bit to most significant *)
let binary_add a b =
  let rec add_with_carry a b carry acc =
    match (a, b) with
    | [], [] -> if carry = 1 then 1 :: acc else acc
    | [], b :: bs -> add_with_carry [] bs ((b + carry) / 2) (((b + carry) mod 2) :: acc)
    | a :: as_, [] -> add_with_carry as_ [] ((a + carry) / 2) (((a + carry) mod 2) :: acc)
    | a :: as_, b :: bs ->
        let sum = a + b + carry in
        add_with_carry as_ bs (sum / 2) ((sum mod 2) :: acc)
  in
  pad_to_8 (add_with_carry (List.rev a) (List.rev b) 0 [])

(* subtraction: take two's complement of second number and add *)
let binary_sub a b =
  let complement = binary_add (binary_not b) (decimal_to_binary 1) in
  binary_add a complement

(* convert signed decimal to binary (using two's complement for negatives) *)
let signed_decimal_to_binary num =
  if num >= 0 then 
    decimal_to_binary num
  else
    (* For negative numbers:
      1. Convert absolute value to binary
      2. Take two's complement (invert all bits and add 1) *)
    let abs_binary = decimal_to_binary (abs num) in
    let inverted = binary_not abs_binary in
    binary_add inverted (decimal_to_binary 1)

(* Convert hex string to binary *)
let hex_to_binary hex_str =
  let hex_str = if String.length hex_str >= 2 && String.sub hex_str 0 2 = "0x" 
                then String.sub hex_str 2 (String.length hex_str - 2)
                else hex_str in
  let hex_to_int c =
    match c with
    | '0'..'9' -> int_of_char c - int_of_char '0'
    | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
    | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
    | _ -> failwith "Invalid hex character"
  in
  let hex_char_to_binary c =
    let n = hex_to_int c in
    [n / 8 mod 2; n / 4 mod 2; n / 2 mod 2; n mod 2]
  in
  let binary = List.flatten (List.map hex_char_to_binary (List.init (String.length hex_str) (String.get hex_str))) in
  pad_to_8 binary

(* Convert binary to hex string *)
let binary_to_hex binary =
  (* Ensure we have 8 bits *)
  let bin = pad_to_8 binary in
  
  (* Convert groups of 4 bits to hex digits *)
  let bits_to_hex start_idx =
    let value = 
      (List.nth bin start_idx) * 8 + 
      (List.nth bin (start_idx + 1)) * 4 + 
      (List.nth bin (start_idx + 2)) * 2 + 
      (List.nth bin (start_idx + 3)) * 1 
    in
    if value < 10 then
      String.make 1 (char_of_int (value + int_of_char '0')) (* values 0-9 to ASCII *)
    else
      String.make 1 (char_of_int (value - 10 + int_of_char 'A')) (* values 10-15 to A-F *)
  in
  bits_to_hex 0 ^ bits_to_hex 4 (* concatenate hex digits *)

(* Binary list to string representation *)
let binary_to_string binary =
  String.concat "" (List.map string_of_int binary)

(* Main program loop *)
let () =
  let rec program_loop () =
    Printf.printf "What operation do you want to perform (NOT, OR, AND, XOR, ADD, SUB or QUIT)? ";
    flush stdout;
    match String.uppercase_ascii (read_line ()) with
    | "QUIT" -> Printf.printf "Goodbye!\n"
    
    | "NOT" ->
        Printf.printf "Enter Hex value: ";
        flush stdout;
        let input = read_line () in
        let hex_val = if String.length input >= 2 && String.sub input 0 2 = "0x"
                     then String.uppercase_ascii (String.sub input 2 (String.length input - 2))
                     else String.uppercase_ascii input in
        let binary = hex_to_binary hex_val in
        let result = binary_not binary in
        Printf.printf "Result of NOT %s = [%s] = %s\n\n" hex_val (binary_to_string result) (binary_to_hex result);
        program_loop ()
    
    | "ADD" | "SUB" as op -> (* handle addition and subtraction *)
        Printf.printf "Enter first decimal value: ";
        flush stdout;
        let num1 = int_of_string (read_line ()) in
        Printf.printf "Enter second decimal value: ";
        flush stdout;
        let num2 = int_of_string (read_line ()) in
        
        if num1 < -128 || num1 > 127 || num2 < -128 || num2 > 127 then
          begin
            Printf.printf "Error: Values must be between -128 and 127\n\n";
            program_loop ()
          end
        else
          let bin1 = signed_decimal_to_binary num1 in
          let bin2 = signed_decimal_to_binary num2 in
          (* for output formatting *)
          if op = "SUB" then begin
            (* for subtraction, we take the two's complement of the second number and add *)
            let neg_bin2 = binary_add (binary_not bin2) (decimal_to_binary 1) in (* perform addition following slides (after NOT, we add 1)*);
            Printf.printf "\n>>> %d - %d <<<\n" num1 num2; (* decoration for output using >>> op <<< *)
            Printf.printf " [%s] = %d\n" (binary_to_string bin1) num1; (* first number in binary *)
            Printf.printf "+[%s] = %d\n" (binary_to_string neg_bin2) (-num2); (* second number in binary *)
            Printf.printf "%s\n" (String.make 18 '-'); (* decoration for output *)
            let result = binary_add bin1 neg_bin2 in 
            Printf.printf " [%s] = %d\n\n" (binary_to_string result) (binary_to_signed_decimal result);
          end else begin
            (* for addition *)
            Printf.printf "\n>>> %d + %d <<<\n" num1 num2; (* decoration for output using >>> op <<< *)
            Printf.printf " [%s] = %d\n" (binary_to_string bin1) num1; (* first number in binary *)
            Printf.printf "+[%s] = %d\n" (binary_to_string bin2) num2; (* second number in binary *)
            Printf.printf "%s\n" (String.make 18 '-'); (* decoration for output *)
            let result = binary_add bin1 bin2 in (* perform addition *)
            Printf.printf " [%s] = %d\n\n" (binary_to_string result) (binary_to_signed_decimal result); (* result in binary and decimal *)
          end;
          program_loop ()
    
    | "AND" | "OR" | "XOR" as op -> (* handle logical operations *)
        Printf.printf "Enter first Hex value: ";
        flush stdout;
        let hex1 = read_line () in
        Printf.printf "Enter second Hex value: ";
        flush stdout;
        let hex2 = read_line () in
        
        let hex1_clean = if String.length hex1 >= 2 && String.sub hex1 0 2 = "0x" (* handle hex prefix *)
                        then String.uppercase_ascii (String.sub hex1 2 (String.length hex1 - 2)) (* remove prefix *)
                        else String.uppercase_ascii hex1 in (* handle decimal input *)
        let hex2_clean = if String.length hex2 >= 2 && String.sub hex2 0 2 = "0x" (* the same we did for hex1 *)
                        then String.uppercase_ascii (String.sub hex2 2 (String.length hex2 - 2)) (* the same we did for hex1 *)
                        else String.uppercase_ascii hex2 in (* the same we did for hex1 *)
        
        let bin1 = hex_to_binary hex1_clean in
        let bin2 = hex_to_binary hex2_clean in
        
        let result = match op with
          | "AND" -> binary_and bin1 bin2
          | "OR" -> binary_or bin1 bin2
          | "XOR" -> binary_xor bin1 bin2
          | _ -> failwith "Invalid operation"
        in
        (* for output formatting *)
        Printf.printf "\n>>> 0x%s %s 0x%s <<<\n" hex1_clean op hex2_clean; (* decoration for output using >>> op <<< *)
        Printf.printf "[%s] = 0x%s\n" (binary_to_string bin1) hex1_clean;
        Printf.printf "[%s] = 0x%s\n" (binary_to_string bin2) hex2_clean;
        Printf.printf "%s\n" (String.make 18 '-'); (* decoration for output *)
        Printf.printf "[%s] = 0x%s\n\n" (binary_to_string result) (binary_to_hex result);
        program_loop ()
    
    | _ ->
        Printf.printf "Invalid operation\n\n";
        program_loop ()
  in
  program_loop ()

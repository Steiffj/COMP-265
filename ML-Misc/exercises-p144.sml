fun intToReal list = 
    map (real) list;

fun ordList list = 
    map (ord) list;

fun ordString string = 
    ordList(explode(string));

(* #6 - Square Sum *)
fun squareSum list = 
    foldr (op +) 0 (map (fn i => i * i) list);

(* #10 - Duplicate List *)
fun doubleList list = 
    foldr (op @) [] (map (fn elem => [elem] @ [elem]) list);

(* #11 - Length *)
fun lengthRedundant list = 
    foldr (op +) 0 (map (fn elem => 1) list);

(* #13 - True Count *)
fun trueCount list = 
    foldr (op +) 0 (map (fn elem => if elem then 1 else 0) list);

(* #16 Dumplode *)
fun implodeRedundant list = 
    foldr (op ^) "" (map (fn ch => str(ch)) list);

(* #16 - List Concatentation *)
fun concatRedundant list = 
    foldr (op @) [] list;

(* 
   Note on Max & Min:
     The accumulating value needs to be an element in the list parameter rather than 0 (or any other int) so the foldr function compares the max/min value of the list to a value that already exists in the list.
     Otherwise, Max will return the accumulating value itself if it's greater than any of the elements in the list. The same is true for Min if the accumulating value is less than any of elements in the list 
   
   Example of Problem:
     MAX: foldr (fn .. => ...) 0 [~1, ~2, ~3] --> 0, but should return -1  
*)

(* #17 - Max Value *)
fun max list = 
    foldr (fn (e1, e2) => if e1 > e2 then e1 else e2) (hd list) list;   

(* #18 - Min Value *)
fun min list = 
    foldr (fn (e1, e2) => if e2 > e1 then e1 else e2) (hd list) list;

(* #19 - Member *)
fun member(value, list) = 
    foldr (fn (e1, e2) => e1 orelse e2) false (map (fn elem => elem = value) list);
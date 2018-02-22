(* #1 - Playing Card Suits *)
datatype suit = Hearts | Diamonds | Spades | Clubs;

(* #2 - Suit toString *)
fun suitname s = 
    case s of
        Hearts => "Hearts"
        | Diamonds => "Diamonds"
        | Spades => "Spades"
        | Clubs => "Clubs";

(* #3 - Number *)
datatype number = 
    numInt of int | numReal of real;

val i6 = numInt 6;
val i9 = numInt 9;
val r4_2 = numReal 4.2;
val r5_5 = numReal 5.5;

(* #4 - Number Addition *)
fun numberAdd(numInt n1, numInt n2) = numInt(n1 + n2)
    | numberAdd(numInt n1, numReal n2) = numReal(real n1 + n2)
    | numberAdd(numReal n1, numInt n2) = numReal(n1 + real n2)
    | numberAdd(numReal n1, numReal n2) = numReal(n1 + n2);

(* #5 - Add values in an intnest *)
datatype intnest = 
    INT of int | LIST of intnest list;

val iNest1 = INT 55;
val iNest4 = LIST [INT 1, INT 2, INT 3, INT 4];

fun nestSum(INT i) = i
    | nestSum(LIST nest) = foldr (op +) 0 (map (fn(INT i) => i) nest);

(* #6 - Multiply values in a myList datatype *)
datatype 'element myList = NIL
    | CONS of 'element * 'element myList;

val emptyList = NIL;
val aMyList = CONS(1, CONS(2, CONS(3, CONS(4, CONS(5, NIL)))));

fun myListProduct NIL = 1
    | myListProduct (CONS(head, tail)) = head * myListProduct(tail);

(* #7 - Reverse myList  *)
fun myListReverse (NIL, result) = result
    | myListReverse (CONS(x, xs), result) = 
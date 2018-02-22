(* Enumerations *)
datatype day = Mon | Tue | Wed | Thu | Fri | Sat | Sun;

fun isWeekDay day = 
    not(day = Sat orelse day = Sun);

(* Integers with positive and negative infinity *)
datatype intEx = 
    Value of int | PosInf | NegInf;

fun squareEx PosInf = PosInf
    | squareEx NegInf = PosInf
    | squareEx (Value x) = Value (x * x) handle Overflow => PosInf;

(* Division with built-in error handling *)
fun safeDivide(a, b) = 
    if b = 0 then NONE else SOME (a div b);

datatype 'x bunch = 
    One of 'x
    | Group of 'x list;

fun size (One _) = 1
    | size (Group x) = length x;

fun sum (One x) = x
    | sum (Group xlist) = foldr (op +) 0 xlist;

(* Polymorphic binary tree *)
datatype 'd tree = Empty
    | Node of 'd tree * 'd * 'd tree;

(* Increment all nodes in a tree *)
fun incAll Empty = Empty
    | incAll (Node(x, y, z)) = Node( (incAll x), y + 1, (incAll z) );

(* Return the sum of all the values of nodes in a tree *)
fun sumAll Empty = 0
    | sumAll (Node(x,y,z)) = (sumAll x) + y + (sumAll z);

fun listAll Empty = nil
    | listAll (Node(x,y,z)) = (listAll x) @ y :: (listAll z);

fun searchTree e Empty = false
    | searchTree e (Node(left, y, right)) = 
        e = y
        orelse (searchTree e left)
        orelse (searchTree e right);

(* Works best for trees with ints or reals. It actually sets up the tree to work well as a binary search tree, as long as the initial tree is structured properly as well *)
fun addLeaf((Empty), newLeaf) = Node(Empty, newLeaf, Empty)
    | addLeaf((Node(less, current, more)), newLeaf) = 
        if newLeaf < current then Node(addLeaf(less, newLeaf), current, more)
        else Node(less, current, addLeaf(more, newLeaf));


val tree123 = Node(
    Node(Empty, 1, Empty),  (* leaf node with value 1 *)
    2,                      (* interior node with value 1 - actually corresponds to the first instance of 'node' *)
    Node(Empty, 3, Empty)   (* leaf node with value 3 *)
);
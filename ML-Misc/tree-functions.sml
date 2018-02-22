
(* Jordan Steiff - 2/22/2018 *)

(* === Datatype Declaration === *)

(* Polymorphic binary tree *)
datatype 'd tree = Empty
    | Node of 'd tree * 'd * 'd tree;


(* === Variables for easy testing === *)

val tree1 = Node(Empty, 1, Empty);

val tree7 = Node(Empty, 7, Empty);

val tree25 = Node(
    Node(Empty, 2, Empty),
    5,
    Empty
);

val tree123 = Node(
    Node(Empty, 1, Empty),
    2,                      
    Node(Empty, 3, Empty)
);

val tree1234567 = Node(
    Node(
        Node(Empty, 1, Empty),
        2,
        Node(Empty, 3, Empty)
    ),
    4,
    Node(
        Node(Empty, 5, Empty),
        6,
        Node(Empty, 7, Empty)
    )
);

val treeABC = Node(
    Node(Empty, "A", Empty),
    "B",                      
    Node(Empty, "C", Empty)
);

val treeABCDE = Node(
    Node(Empty, "A", Empty),
    "B",
    Node(
        Node(Empty, "C", Empty),
        "D",
        Node(Empty, "E", Empty)
    )
);

(* === Add Leaf Functions === *)

(* This function works for trees with ints or reals because of the < operator. 
   It sets up the tree to work well as a binary search tree, as long as the initial tree is structured properly *)

fun addLeaf((Empty), newLeaf) = Node(Empty, newLeaf, Empty)
    | addLeaf((Node(less, current, more)), newLeaf) = 
        if newLeaf < current then Node(addLeaf(less, newLeaf), current, more)
        else Node(less, current, addLeaf(more, newLeaf));

(* This works for trees of any type, but it doesn't insert the new value in an especially-meaningful way
    (It always places the new value as the leftmost leaf of the tree) *)

fun addAnyLeaf((Empty), newLeaf) = Node(Empty, newLeaf, Empty)
    | addAnyLeaf((Node(Empty, current, right)), newLeaf) = Node(Node(Empty, newLeaf, Empty), current, right)

    | addAnyLeaf((Node(left, current, Empty)), newLeaf) = Node(left, current, Node(Empty, newLeaf, Empty))

    | addAnyLeaf((Node(left, current, right)), newLeaf) = Node(addAnyLeaf(left, newLeaf), current, right);     (* Recursively search for a leaf under which to place the new value, 
                                                                                                                    starting with the current node's left child (an arbitrary choice) 
                                                                                                                    if the current node has non-Empty children *)


(* === Other Functions === *)

(* Increment all nodes in a tree *)
fun incAll Empty = Empty
    | incAll (Node(x, y, z)) = Node( (incAll x), y + 1, (incAll z) );

(* Return the sum of all the ints or reals in a tree *)
fun sumAll Empty = 0
    | sumAll (Node(x,y,z)) = (sumAll x) + y + (sumAll z);

(* Concatenates all strings in a tree using inorder traversal *)
fun concatAll Empty = ""
    | concatAll (Node(Empty, a, Empty)) = a
    | concatAll (Node(x,y,z)) = (concatAll x) ^ y ^ (concatAll z);

(* Return all values of a tree in a list *)
fun listAll Empty = nil
    | listAll (Node(x,y,z)) = (listAll x) @ y :: (listAll z);

(* Search a tree for an element e *)
fun searchTree e Empty = false
    | searchTree e (Node(left, y, right)) = 
        e = y
        orelse (searchTree e left)
        orelse (searchTree e right);
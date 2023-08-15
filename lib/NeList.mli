(* This is taken from the opam package neList: https://github.com/johnyob/ocaml-non-empty-list,
   but modified in order to add some additional functionality *)

open! Base

(** A non-empty list is defined as a immutable (singly) linked list containing at least 1 element.
    Identical in terms of complexity to [List]. The API is inspired by Janestreet's [Base.List]
    (but is not identical). *)

(** The non-empty list type. The use of the [( :: )] infix constructor
    allows the usage of the built-in list syntactic sugar provided by OCaml.

    For example, a singleton is given by [ [1] ]. A list containing 2 elements is given by
    [ [1; 2] ]. *)
type 'a t = ( :: ) of 'a * 'a list [@@deriving eq, ord, show, sexp]

type 'a neList = 'a t

include Monad.S with type 'a t := 'a t

(** [init n ~f] returns the non-empty list [[(f 0); (f 1); ...; (f (n-1))]]. *)
val init : int -> f:(int -> 'a) -> 'a t

(** [of_list] creates a non-empty list from a standard list,
    returning [None] if the empty list is provided. *)
val of_list : 'a list -> 'a t option

(* Similar to {!NeList.of_list}, but raises an [Invalid_argument] if the empty list is provided. *)
val of_list_exn : 'a list -> 'a t

(** [of_array] creates a non-empty list from an array.
    [of_array arr] is equivalent to [of_list (List.to_array arr)]. *)
val of_array : 'a array -> 'a t option

(** Simialar to {!NeList.of_array}, but raises an [Invalid_argument] if an empty array is provided. *)
val of_array_exn : 'a array -> 'a t

(** [to_list] creates a standard list from a non-empty list. *)
val to_list : 'a t -> 'a list

(** [to_array] creates an array from a non-empty list.
    [to_array t] is equivalent to [Array.of_list (to_list t)]. *)
val to_array : 'a t -> 'a array

(** [length t] returns the number of elements in [t].
    Note that [length t >= 1] for all [t]. *)
val length : 'a t -> int

(** [is_singleton t] returns [true] if [t] is a singleton (a non-empty list containing
    a single element). *)
val is_singleton : 'a t -> bool

(** [cons x t] returns the non-empty list [[x; ...t]]. *)
val cons : 'a -> 'a t -> 'a t

(** [append t1 t2] returns a list [t] consisting of all elements
    of [t1] and [t2] (order preserved). *)
val append : 'a t -> 'a t -> 'a t

(** [rev_append t1 t2] reverses [t1] and then appends it to [t2].
    Equivalent to [append (rev t1) t2], however, is more efficient. *)
val rev_append : 'a t -> 'a t -> 'a t

(** Concatenates a non-empty list of non-empty lists. *)
val concat : 'a t t -> 'a t

(** [hd] returns the first element of the given non-empty list [t].
    Note that no exception can occur. *)
val hd : 'a t -> 'a

(** [tl] returns the tail of the non-empty list. Note that the [list] type is used
    for the returned value, since removing an element from a non-empty list may yield
    an empty list (e.g. a singleton). *)
val tl : 'a t -> 'a list

(** [last] returns the final element of the non-empty list. *)
val last : 'a t -> 'a

(** [lastWithPriors] returns the final element of the non-empty list,
    along with the preceding list of elements. *)
val lastWithPriors : 'a t -> 'a list * 'a

(** Returns the [n]-th element of the list. The head of the list has position 0.
    [None] is returned if the list [t] is too short or [n] is negative. *)
val nth : 'a t -> int -> 'a option

(** Equivalent to [nth]. However, an [Invalid_argument] exception is raised if the list
    [t] is too short or [n] is negative. *)
val nth_exn : 'a t -> int -> 'a

(** Checks whether the provided element [x] is a member of the non-empty list [t], using
    [equal]. *)
val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool

(** [find t ~f] returns the first element of the non-empty list [t] that
    satisfied [f]. *)
val find : 'a t -> f:('a -> bool) -> 'a option

(** Similar to {!NeList.find}, but raises a [Not_found] if there is no such element
    that satisfies [f]. *)
val find_exn : 'a t -> f:('a -> bool) -> 'a

(** Similar to [find], however, the index is passed as an argument to [f] as well. *)
val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option

val findi_exn : 'a t -> f:(int -> 'a -> bool) -> int * 'a

(** [find_map t ~f] returns [Some x], the first element [x] for which [f x] retunrs [Some x].
    Returns [None] if there is no such element. *)
val find_map : 'a t -> f:('a -> 'b option) -> 'b option

val find_map_exn : 'a t -> f:('a -> 'b option) -> 'b

(** Similar to {!NeList.find_map}, however, the index is passed as an argument to [f] as well. *)
val find_mapi : 'a t -> f:(int -> 'a -> 'b option) -> (int * 'b) option

val find_mapi_exn : 'a t -> f:(int -> 'a -> 'b option) -> int * 'b

(** [Or_unequal_lengths] is used for functions that take multiple non-empty lists (denoted [t1], etc).
    Defines the dependent type: [{'a : length t1 = length t2 = ... length tn}].
    Extends the [Base.List.Or_unequal_lengths] implementation with the Monad methods, improves
    readabily and reuseability of other library functions. *)
module Or_unequal_lengths : sig
  include module type of List.Or_unequal_lengths
  include Monad.S with type 'a t := 'a t
end

(** Reverses the non-empty list. *)
val rev : 'a t -> 'a t

(** [rev_map t ~f] is equivalent to [rev (map t ~f)]. However, it is more efficient. *)
val rev_map : 'a t -> f:('a -> 'b) -> 'b t

(** [rev_map2 t1 t2 ~f] is equivalent to [map2 t1 t2 ~f >>| rev]. Returns [Unequal_lengths]
    if [length t1 <> length t2]. *)
val rev_map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t

(** Similar to [rev_map2], however, raises [Invalid_argument] if [length t1 <> length t2]. *)
val rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

(** [for_all [x1; ...; xn] ~f] returns [true] iff [f xi] is [true] for all [1 <= i <= n].
    This is a short-circuiting operation, evaluting [f xi] in a left-to-right order. *)
val for_all : 'a t -> f:('a -> bool) -> bool

(** Similar to {!NeList.for_all}, however, the index is passed as an argument to [f] as well. *)
val for_alli : 'a t -> f:(int -> 'a -> bool) -> bool

(** Similar to {!NeList.for_all}, but defines a 2 non-empty list quanitifer *)
val for_all2 : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool Or_unequal_lengths.t

val for_all2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool

(** [for_all [x1; ...; xn] ~f] returns [true] iff there exists [1 <= i <= n] such that [f xi] is [true].
    This is a short-circuiting operation, evaluting [f xi] in a left-to-right order. *)
val exists : 'a t -> f:('a -> bool) -> bool

(** Similar to {!NeList.exists}, however, the index is passed as an argument to [f] as well. *)
val existsi : 'a t -> f:(int -> 'a -> bool) -> bool

(** Similar to {!NeList.exists}, but defines a 2 non-empty list quanitifer. *)
val exists2 : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool Or_unequal_lengths.t

val exists2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool

(** [filter t ~f] returns a *list* of elements of [t] that satisfy the predicate [f].
    The order is preserved and evaluation is left-to-right. *)
val filter : 'a t -> f:('a -> bool) -> 'a list

val filter_map : 'a t -> f:('a -> 'b option) -> 'b list
val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a list
val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b list

(** [rev_filter t ~f] is equivalent to [List.rev (filter t ~f)], however, is more efficient. *)
val rev_filter : 'a t -> f:('a -> bool) -> 'a list

val rev_filter_map : 'a t -> f:('a -> 'b option) -> 'b list
val rev_filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b list

(** [map ~f [x1; ...; xn]] ([n >= 1]) applies [f] to [x1], ..., [xn] (left-to-right order),
    yielding the resultant non-empty list [[f x1; ..., f xn]]. *)
val map : 'a t -> f:('a -> 'b) -> 'b t

(** Similar to {!NeList.map}, however, the index is passed as an argument to [f] as well. *)
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t

(** Similar to {!NeList.map}, but defined for 2 non-empty lists. *)
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t

val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

(** [concat_map t ~f] is equivalent to [concat (map t ~f)]. *)
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t

val concat_mapi : 'a t -> f:(int -> 'a -> 'b t) -> 'b t

(** [iter [x1; ...; xn] ~f] is equivalent to [f x1; ...; f xn] *)
val iter : 'a t -> f:('a -> unit) -> unit

val iteri : 'a t -> f:(int -> 'a -> unit) -> unit

(** [iter2 [x1; ...; xn] [y1; ...; ym] ~f] is equvalent to [f x1 y2; ...; f xn yn].
    Returns [Unequal_lengths] if [m <> n]. *)
val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit Or_unequal_lengths.t

val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit

(** [fold_left [x1; ...; xn] ~init ~f] returns [f (... f (f init x1) x2 ...) xn].
    [fold_left t ~init ~f] is equivalent to [List.fold_left (to_list t) ~init ~f] *)
val fold_left : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b

(** Similar to {!NeList.fold_left}, however, the index is passed as an argument to [f] as well. *)
val fold_lefti : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b

(** [fold_right [x1; ...; xn] ~init ~f] returns [f x1 (... f x(n-1) (f init xn) ...)].
    [fold_right t ~init ~f] is equivalent to [List.fold_right (to_list t) ~init ~f] *)
val fold_right : 'a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b

(** Similar to {!NeList.fold_right}, however, the index is passed as an argument to [f] as well. *)
val fold_righti : 'a t -> init:'b -> f:(int -> 'a -> 'b -> 'b) -> 'b

(** [reduce [x1; ...; xn] ~f] returns [f (... f (f x1 x2) x3 ...) xn].
    Note that no exception cqan occur. *)
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a

(** [count t ~f] returns the number of elements of [t] that satisfy [f]. *)
val count : 'a t -> f:('a -> bool) -> int

(** Similar to {!NeList.count}, however, the index is passed as an argument to [f] as well. *)
val counti : 'a t -> f:(int -> 'a -> bool) -> int

(** [zip [x1; ...; xn] [y1; ...; ym]] returns [[(x1, y1); ...; (xn, yn)]]. Returns [Unequal_lengths]
    if [m <> n] *)
val zip : 'a t -> 'b t -> ('a * 'b) t Or_unequal_lengths.t

val zip_exn : 'a t -> 'b t -> ('a * 'b) t

(** [unzip [(x1, y1); ...; (xn, yn)]] returns [([x1; ...; xn], [y1; ...; yn])]. *)
val unzip : ('a * 'b) t -> 'a t * 'b t

val partition_map : 'a t -> f:('a -> ('b, 'c) Base.Either.t) -> 'b list * 'c list
val partition_tf : 'a t -> f:('a -> bool) -> 'a list * 'a list
val split_n : 'a t -> int -> 'a list * 'a list
val all_options : 'a option t -> 'a t option
val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a
val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a

(* An empty rope, containing no strings. *)
val rope = Rope.empty;

(* Initialise rope from a string. 
 *
 * You probably want to avoid initialising the rope with very long strings,
 * because a rope is meant to represent a long string 
 * by holding nodes that contain smaller strings in a binary tree. 
 * The implementation avoids building strings that are ever larger than 1024,
 * but that was done in an attempt to find the ideal length for performance. 
 * A user shouldn't notice any delays in larger lengths like 65535 either. 
 *
 * In their text buffer (a piece-tree, which is slower than a rope), 
 * the VS Code team had other issues with excessively large strings.
 * https://code.visualstudio.com/blogs/2018/03/23/text-buffer-reimplementation#_avoid-the-string-concatenation-trap *)
val rope = Rope.fromString "hello, world!\n";

(* Convert a rope to a string.
 *
 * This may involve allocating an extremely large string in some cases,
 * which should be avoided for the reason mentioned in the above comment. *)
val str = Rope.toString rope;

(* Insert a string into the rope. 
 *
 * There isn't any validation to check that you inserted at a reasonable
 * position. 
 * If you insert at an index lower than 0, your inserted string is just
 * prepended to the start. 
 * If you insert at an index greater than the length, your inserted string is
 * just appended to the end.
 *
 * One thing to watch out for if you are using the line-rope is making sure 
 * that you don't insert in the middle of a \r\n pair, separating \r from \n. 
 * That would mess up the line metadata the rope contains and make the line
 * metadata invalid. *)
val rope = Rope.insert (14, "goodbye, world!", rope);

(* Append a string into the rope. *)
val rope = Rope.append ("hello again\n", rope);

(* Append a string into the rope, providing line metadata with it. 
 *
 * The point of this function is for performance: the other insertion functions
 * calculate the line metadata by scanning the string itself, but in some cases
 * this is already known. The larger example below is such a case. *)
val rope = Rope.appendLine ("my new line", Vector.fromList [], rope);

(** Second larger example motivating String.appendLine below. *)
(*** Returns the start index of a line, 
 *** returning the index of \r if line ends with a \r\n pair. *)
fun getLineStart line =
  let
    val lastIdx = String.size line - 1
    val lastChr = String.sub (line, lastIdx)
  in
    if lastChr = #"\n" andalso lastIdx - 1 >= 0 then
      if String.sub (line, lastIdx - 1) = #"\r" then lastIdx - 1 else lastIdx
    else
      lastIdx
  end;

(*** Appends the lines in a file to a rope. *)
fun readLines (rope, file) =
  case TextIO.inputLine file of
    SOME line =>
      let
        (* Don't need to scan string to find line breaks, 
         * because we already know. *)
        val lineIdx = getLineStart (line)
        val vec = Vector.fromList [lineIdx]
        val rope = Rope.appendLine (line, vec, rope)
      in
        readLines (rope, file)
      end
  | NONE => rope;

val licenseRope = readLines (Rope.empty, TextIO.openIn "LICENSE");

(* Deletes the given range from rope, from the start index to the end index. 
 *
 * As with insert, one should make sure they don't corrupt the line metadata. 
 * Specifically, in a \r\n pair, the line metadata points to \r. 
 * Deleting \r would corrupt it, but deleting \n would be fine. 
 * In general, if you want to delete a line break, you would want to delete both
 * \r and \n. The user thinks of the \r\n pair as a single character so they are
 * expecting the whole line break to be deleted. *)

(** Initialise new rope. *)
val rope = Rope.fromString "hello, world!";
(** New rope contains "hello world!" without comma. *)
val rope = Rope.delete (5, 1, rope);

(* Folds over the characters in a rope, starting from the given index. 
 *
 * This is meant to be an alternative to queries for a specific line or a
 * substring.
 * If a rope is meant to avoid allocating large strings, then it seems more
 * performant to query its contents through higher-order functions rather than 
 * allocating substrings and querying the substring. *)
val rope = Rope.fromString "hello!";;

fun apply (chr, lst) = chr :: lst;
(** val result = [#"!",#"o",#"l",#"l",#"e"] : char list *)
val result = Rope.foldFromIdx (apply, 1, rope, []);

(* Folds over the characters in a rope, accepting a predicate function
 * that terminates the fold when it returns true. *)
fun apply (chr, acc) =
  (print (Char.toString chr); acc + 1);

fun term acc = acc = 3;

(** Below function prints first three letters, "hel", 
 ** and then steops folding. *)
val _ = Rope.foldFromIdxTerm (apply, term, 0, rope, 0);

(* Folds over the characters in a rope, starting from the given line number.
 * 
 * This is just like the foldFromIdxTerm function, except that it starts folding
 * from the given line number instead. *)
val rope = Rope.fromString "hello, world!\ngoodbye, world!\nhello again!";

fun apply (chr, _) =
  print (Char.toString chr);

fun term _ = false;

(** Below line prints the whole string, one character at a time. *)
Rope.foldLines (apply, term, 0, rope, ());
(** Prints starting from #"g" in "goodbye". *)
Rope.foldLines (apply, term, 1, rope, ());
(** Prints the very last line. *)
Rope.foldLines (apply, term, 2, rope, ());

(** Prints the whole string if specifying a line before 0, which doesn't exist. *)
Rope.foldLines (apply, term, ~3, rope, ());
(** Raises a subscript exception: there is no corresponding line in the rope. *)
Rope.foldLines (apply, term, 4, rope, ());

amethyst
========

_ Name will change soon
$:hluna("A programming language by bluebear94
_ the troll

Note
----

Decisions about the language are still made. As long as the major version number is 0, a particular version may not be backward-compatible with another.

Philosophy
----------

I wanted to invent a language that followed the path of TI-Basic. In October, I wrote an initial attempt at Java (named Labyrinth, since it was supposed to be difficult to understand), but it quickly became messy, so I left the project for a while, until I rewrote it in Scala at the beginning of December (and named it Amethyst).

I made some choices to fulfill the TI-Basic philosophy:

* The interface consists of a top screen (implemented by a BufferedImage, similar to how calculator graphics are implemented) for graphics and a bottom screen resembling the TI-89 homescreen (with the input allowing multiple lines.
* Unclosed parentheses, brackets, braces, quotes, and guillemets are permitted.
* "Global variables" are actually persistent files.
* No optimization is performed for you, especially not tail-call optimization. Do it yourself.
* No objects or even structures. Use lists or maps.
* There is a last-answer variable: one accounting for Void return values, and one not. (Apparently, the TI-89 has an `ans()` function, but it's locked in program mode. How scandalous!)
* There is no passing by reference; only value.

However, I deviated from typical calculator programming languages in order to reduce the annoyance of using it:

* First class functions and lists, including lambdas (You can pass functions as arguments, as well as nest lists as much as you want, or at least as much as your computer can handle).
* A built-in map type, since maps are frequently used, especially in this code.
* Three numerical types: arbitrary-precision, 64-bit integers, and 64-bit IEEE floating-points (a. k. a. doubles).
* Two list types: arrays (like 83+) and linked lists (like 89), because each has its strengths and weaknesses. Plus, their dimensions are limited only by memory.
* Programs are planned to be stored using bytecode, with an option to compile without running (I think the TI-89's inability to do so was a big annoyance).
* A Void type and value.
* Zero-based, rather than one-based, indexing for lists.

This language is still in its infancy; many commands are still not implemented. However, they will eventually exist.

How to Get
----------

Amethyst is usable at this time, but is not in condition to release.

* Install Git and SBT if you had not already done so.
* Open up a terminal and `cd` into a directory.
* Create a directory (`mkdir amethyst`) and `cd` into it.
* Do `git clone https://github.com/bluebear94/amethyst` to get the project (update with `git pull origin master`)
* Type `sbt`
* As soon as SBT finishes loading, type `run`
* Then wait for everything to compile and choose an option: `1` to start the developer console, and `2` to start the GUI program

Getting Started
---------------

In order to run a piece of code, simply type it in the bottom panel and press the <kbd>Run</kbd> button (or, alternatively, <kbd>Ctrl</kbd> + <kbd>Enter</kbd>).

The current types are:

* Void
* Mountain (arbitrary-precision integers)
* Hill (64-bit integers)
* String (UTF-8 encoded)
* Fish (64-bit IEEE floating-point numbers)
* Array
* Linked List
* Function

The current operators are:

* \+
* \-
* @ (arithmetic mean of two numbers, rounded down)
* \*
* /
* \\ (int division)
* %
* ^ (exponentiation)
* ==
* \!=
* <
* <=
* >
* >=
* && (logical and SC)
* || (logical or SC)
* &' (logical and NSC)
* |' (logical or NSC)
* ^^ (logical xor)
* ? and : (ternary operator; `a ? b : c` returns `b` if `a` is true, and `c` otherwise)
* \+> (map a collection using a function; e. g. `{2, 4, 6, 8} +> $:hluna` displays the four numbers on separate lines

Precedence can, of course, be specified by parentheses, and `&&` and `||` are short-circuit operators (and they are not guaranteed to return either 0 or 1), while `&'`, `|'`, and `^^` do not short-circuit and return either 0 or 1.

Variables can be assigned using the `=` operator (with the right side empty, it just deletes the variable). Global variables are denoted by `$`, and they persist through multiple sessions. Some operators also provide an assignment equivalent, as well as doubled versions that can appear before *or* after an lvalue.

    a = 3 _ Assigns 3 to a
    b = 4 _ Assigns 4 to b
    b = _ Hasta la vista, b
    a = 5 _ Assigns 5 to a
    a += 1 _ a is now 6
    $:hluna(++a) _ shows 7
    $:hluna(a) _ shows 7
    $:hluna(a**) _ shows 7
    $:hluna(a) _ shows 14
    a[3] _ returns ↼1

Functions use lambdas, and the body is delimited with the `λ` and `Endλ` keywords. The last line defines the return value of the function. The number of arguments can be retrieved with `#0`, and each argument can be retrieved with an octothrope preceding a one-indexed value. Therefore, a function to square a number would be defined as:

    square = λ; #1 * #1; Endλ

Lists come in two flavors: *arrays* delimited by *braces*, and *linked lists* delimited by *square brackets*. They differ in the computational complexity of a given operation. They can be indexed using brackets as well; e. g. `{3, 5, 7}[1]` returns the second element of a list. They support batch operations as well (`&&` and `||` dooo not work; instead, use `&'` and `|'` which lack short-circuiting but work on lists). Indexing *does* work on strings and even *integers* as well.

Each expression modifies the last-answer variable `Ans`, and `Answer` as well for non-void values.

The `If` keyword, followed by an expression, executes the next line if that expression evaluates to true. The execution of subsequent code does not depend on the truth or falsity of that expression; for example:

    If a
    $:hluna("a
    $:hluna("b

will always print `b`. Note that there is no separate Boolean type; instead, `0`, `↼0`, `0.`, `""`, `{}`, and `[]` all evalute to false.

The `If` statement, if the following code is delimited by `Then` and `EndIf`, will execute the whole block conditionally.

The `For` .. `EndFor` loop will execute a block of code for each value of a variable. Its syntax is:

    For variable, start, end [,incr]
    (statements)
    EndFor

The `While` .. `EndWhile` loop executes the body while its head expression evaluates to true, while the `Repeat` .. `EndRept` loop defers the check to after the body executes. They are analogous to `while (...) {...}` and `do ... while(!...);` constructs in C-based languages.

Documentation of Commands
-------------------------

Coming soon.

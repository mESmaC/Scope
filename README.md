```
// the projects namespace will be the same as before

program main;

/* 
    Comments are replaced with a more common style.

    Imports are exactly the same as they are in pascal, i think they are already quite intuitive.
*/
uses 
    crt,
    WebScope;

/*
    All Variables are considered to be immutable unless otherwise specified.
    you can use the mut keyword to make a variable mutable.
    Ownership and borrowing annotations are added for reinforced concurrency.
*/

var
    mut name: string;
    age: int;
    owner data: int; // Ownership annotation
    borrow temp: int; // Borrowing annotation

/*
    variables and type syntax will remain the same. 

    statements and loops will remain the same.

    these are the only changes to the syntax.
*/


/*
    Class syntax is now more straightforward and more akin to c# or java.
*/
public class publicClass {

    try {

    } finally {

    };

};



private class privateClass {

};

proc mainProcedure() {

};

func mainFunction() {

};

/* 
    Replaces pascals outdated begin and end keywords.
    Curly brackets are more internationally known and visually understood.
    instead of having pascals main statement being begin and end, we now 
    call init. 
*/

init {
    mainProcedure();
    mainFunction();
}.
```

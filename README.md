This project is intended to create a domain-specific language for CSV manipulation akin to SQL, using only Ocaml.

Every CSV file is required to be labeled table where the first row is the header row and the trable is rectangular.

```
BNF grammar:
p ::=
    | c
    | c p
            
c ::=
    | x := t;
    | print t;
    | save t str;
            
t ::=  
    | x 
    | load str
    | project names from t
    | join t1 with t2 on key
    | rename
    | ( t )
            
names ::=
key ::=
            
x ::= identifiers
str ::= string literals
```

The driver program exists in bin/main.ml.

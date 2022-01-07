trace_theory
=====

An OTP library

How to run the code
-----

    $ rebar3 compile
    $ rebar3 shell

The rebar3 shell will appear and it will be able to execute the command.

Example Commands:

```
trace_theory:perform_computation(["a", "b", "c", "d", "e", "f", "g", "h"], [{"a", [1,0,0,0],[3,0,0,0,0]}, {"b",[1,0,0,0],[0,1,1,0,0]},{"c",[0,1,0,0],[0,0,0,0,5]}, {"d",[0,1,0,0],[0,0,v,z,0]}, {"e", [0,0,2,0], [0,0,1,0,0]}, {"f", [0,0,1,0],[v,0,0,x,0]}, {"g", [0,0,0,1],[0,0,0,0,0]}, {"h", [0,0,0,1],[1,1,0,0]}], ["a", "c", "e", "g", "b", "d", "f", "h", "c", "b"]).
```
```
trace_theory:perform_computation(["a", "b", "c", "d", "e", "f"], [{"a", [1,0,0,0,0], [1,0,0,0,0,1]}, {"b", [0,1,0,0,0], [0,1,2,0,0,0]}, {"c", [1,0,0,0,0], [3,0,1,0,0,0]}, {"d", [0,0,0,0,1],[0,0,0,1,1,0]}, {"e", [0,0,1,0,0],[0,1,-1,0,0,0]}, {"f", [0,0,0,1,0], [1,0,0,1,0,0]}], ["a", "c", "d", "c", "f", "b", "b", "e"]).
```
```
trace_theory:perform_computation(["a", "b", "c", "d", "e", "f", "g", "h"], [{"a", [1,0,0,0],[1,1,0,0,0]}, {"b",[1,0,0,0],[0,1,1,0,0]},{"c",[1,0,0,0],[1,0,v,z,0]}, {"d",[0,1,0,0],[1,1,0,0,0]}, {"e", [0,1,0,0], [0,1,1,0,0]}, {"f", [0,0,1,0],[0,1,1,0,0]}, {"g", [0,0,1,0],[z,0,x,1,0]}, {"h", [0,0,0,1],[z,0,x,0]}], ["e", "h", "c", "f", "a", "f", "d", "h", "g"]).
```

To insert productions to the function they need to be transformed:

* Create list of variables existing in productions it has size N

    example for productions:
    ```
    (a) x <- 2y + z + 5
    (b) y <- x
    (c) z <- y
    ```
    list:
    ```
    [x,y,z]
    ```
    
* Change left site of production to the table of size N, where you will mark, which variable you are changing.
    From the previous example:
    ```
    (a) [1,0,0] <- 2y + z + 5
    (b) [0,1,0] <- x
    (c) [0,0,1] <- yx
    ```
    
* Change right site of production to the table, of size N+1 where you will mark which variable you are using.
    From the previous example:
    ``` 
         x,y,z      x,y,z,1
    (a) [1,0,0] <- [0,2,1,5]
    (b) [0,1,0] <- [1,0,0,0]
    (c) [0,0,1] <- [y,x,0,0]

For test data2:

```
trace_theory:perform_computation(["a", "b", "c", "d", "e", "f"], [{"a", [1,0,0,0,0], [1,0,0,0,0,1]}, {"b", [0,1,0,0,0], [0,1,2,0,0,0]}, {"c", [1,0,0,0,0], [3,0,1,0,0,0]}, {"d", [0,0,0,0,1],[0,0,0,1,1,0]}, {"e", [0,0,1,0,0],[0,1,-1,0,0,0]}, {"f", [0,0,0,1,0], [1,0,0,1,0,0]}], ["a", "c", "d", "c", "f", "b", "b", "e"]).
```

```
D = {(a,a)(a,c)(a,f)(b,b)(b,e)(c,a)(c,c)(c,e)(c,f)(d,d)(d,f)(e,b)(e,c)(e,e)(f,a)(f,c)(f,d)(f,f)}
I = {(a,b)(a,d)(a,e)(b,a)(b,c)(b,d)(b,f)(c,b)(c,d)(d,a)(d,b)(d,c)(d,e)(e,a)(e,d)(e,f)(f,b)(f,e)}
FNF WRONG = [c,a][c,b][d,b][f,e]
1 -> 2
6 -> 7
7 -> 8
2 -> 4
4 -> 5
4 -> 8
3 -> 5
1[label=a]
2[label=c]
3[label=d]
4[label=c]
5[label=f]
6[label=b]
7[label=b]
8[label=e]
FNF = [a,b,d][b,c][c][e,f]
ok
```

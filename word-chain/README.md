# Word Chain

## Task
Given two words of equal length, write a program that can build a chain of words connecting the first to the second. 
Each word in the chain must be in this word list and every step along the chain changes only one letter from the previous word.
For example, given the start word "cat" and the end word "dog", a valid chain would be:
```
"cat", "cot", "cog", "dog".
```
Another example "duck" to "ruby" would have a valid word chain:
```
"duck", "ruck", "rusk", "ruse", "rube", "ruby"
```
If you get your code working, try timing it. 

Does it take less than a second for the above examples? 
And is the timing the same forwards and backwards? 
Does your code find the shortest possible valid word chain?

## Source
https://nwrug.org/quizzes/word-chains-kata

## Solutions - explanation
### Solution based on BFS
The Graph is not
constructed beforehand. Neighbours are discovered lazily.
Starting from `end`, neighbours are discovered and for each neighbour a list representing
a path is created. The process is repeated by taking the heads of the paths, discovering their
neighbours and extending the paths. If `start` is encountered when discovering neighbours, a path is found.
The path is also the shortest path (check the properties of BFS algorithm).
The reason why we are exploring from `end` to `start` is because in Scala it is efficient to append elements to 
the front of a list.

### Solution based on A*
Function `f` is computed as:
`f = g + h`
where
* `g` distance from the start (e.g. `dog` is at distance 1 from `cog` because these two words are different just
at one position)
* `h` heuristic - number of different characters (less differences are better)

The graph is built beforehand. Check documentation in the code for more details.

Problem descriptions by [mwttg](https://github.com/mwttg/code-katas-scala/tree/master/kata_2020-01-word-chain/readme.md).


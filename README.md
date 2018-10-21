# sudoku_r_game
Sudoku game in R console! 
In-build matrix generator of three difficulty level (i.e. (E)asy, (M)edium, (H)ard)) together with interactive game session in which user assign values to a particular matrix cell.

```R
# command to start the game
start_game()
```

List of commands while in game session:
```R
# assign value
1, 2, 3   # row 1, column 2, value 3
9, 8, .   # row 9, column 8, empty value

# revert last assignment
rev
revert

# restart game
res
restart

# change difficulty level
E   # easy
M   # medium
H   # hard
```

# ocaml-jeopardy
Interactive OCaml Jeopardy game. Open-ended group midterm project for CS 3110. 

Type command `make play` in the main directory to play the game. <br><br>
User may use command `set person` to choose a two-player user-vs-user game or `set bot [difficulty level]` to choose a one-player user-vs-bot game. <br>
Bot answers correctly or incorrectly at random, with probability of correctness determined by user-specified bot difficulty level. <br>
Bot chooses point values strategically based on overall game progress and the score. <br><br>
User must answer (`answer` command) within 10-second time limit or answer is considered incorrect. <br>
User also has option to `pass` on a question rather than risk guessing incorrectly. <br><br>
`wager` command is used for Daily Doubles and Final Jeopardy. <br>
`score` command shows the current score. <br><br>
Any json board may be used in place of the default ones, given that it fits the schema. <br><br>

Use command `make play` in directory `cs3110-a6-master` to play the game. The first two questions of a sample player-vs-bot game are shown below. <br>

![display1](display1.PNG)
![display2](display2.PNG)
![display3](display3.PNG)
![display4](display4.PNG)

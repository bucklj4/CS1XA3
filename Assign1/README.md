Assignment 1 README

### Question 1 ###
This part of the script tells the user if their repo is up to date or not. It does this by gathering commits from the remote repo using git fetch and then checks if the local repo is the same as the remote, upstream repo by comparing their SHA1 hashes.

### Question 2 ###
This part of the script puts all the uncommitted changes of the project in a file called changes.log by using the git diff command.

### Question 3 ###
This part of the script returns a file called todo.log that contains all of the lines in the project's source files where the tag #TODO appears. It ignores the changes.log file (could raise duplicates of the same #TODO lines) and the ProjectAnalyze.sh script itself.

### Question 4 ###
This part of the script adds 'main = undefined' to any Haskell files which do not already contain this line to ensure even plain, non-module Haskell files are checked. Then, it checks all Haskell files for errors and returns them in a file called error.log

### Question 5 (Bonus Feature) ###
This part of the script asks the user if they want a print-out of the three created files, and should they please, it prints them out.

### Question 6 (Bonus Feature) ###
This part of the script asks the user if they want to watch ASCII Star Wars, and subsequently an ASCII Rick Roll, and plays them should they please.

### Credits ###

Running multiple commands on same xargs argument: https://stackoverflow.com/questions/6958689/xargs-with-multiple-commands-as-argument

Bash coloured text using ANSI escape codes: https://misc.flogisoft.com/bash/tip_colors_and_formatting

ASCII Star Wars using Telnet (Requires Internet connection, so unable to use on Mills server): https://askubuntu.com/questions/699159/ascii-animations-that-are-viewed-in-the-command-line

ASCII Star Wars + Rick Roll using Python script that starts local server: https://github.com/nitram509/ascii-telnet-server

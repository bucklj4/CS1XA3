#!/bin/bash

BRED="\e[41m"
BBLUE="\e[44m"
NORM="\e[0m"

# Question 1

# Gather commits from remote repo
(git fetch origin)

localRepo=$(git rev-parse HEAD)
remoteRepo=$(git rev-parse @{u})
echo ${localRepo}
echo ${remoteRepo}

if [ ${localRepo} = ${remoteRepo} ]
then
	echo -e "${BBLUE}The local repo is up to date with the remote repo.${NORM}"
else
	echo -e "${BRED}The local repo is NOT up to date with the local repo.${NORM}"
fi

# Question 2

(git diff) > changes.log

# Question 3
(grep -r -h "#TODO" --exclude="ProjectAnalyze.sh" --exclude="changes.log" --exclude="todo.log" .) > todo.log

# Question 4
(find -name "*.hs" | xargs -I {} sh -c "printf 'Errors for the Haskell file: '; readlink -f {}; printf '\n\n'; ghc -fno-code {}; echo; echo '--------------------'; echo;") &> error.log

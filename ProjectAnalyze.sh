#!/bin/bash

# Question 1

# Gather commits from remote repo
(git fetch origin)

localRepo=$(git rev-parse HEAD)
remoteRepo=$(git rev-parse @{u})
echo ${localRepo}
echo ${remoteRepo}

if [ ${localRepo} = ${remoteRepo} ]
then
	echo "The local repo is up to date with the remote repo."
else
	echo "The local repo is NOT up to date with the local repo."
fi

# Question 2

(git diff) >> changes.log

# Question 3
(grep -r -h "#TODO" --exclude="ProjectAnalyze.sh" --exclude="changes.log" --exclude="todo.log" .) >> todo.log

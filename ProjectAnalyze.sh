#!/bin/bash

# Colour ANSI codes
BRED="\e[41m"
BBLUE="\e[44m"
REV="\e[7m"
BMAG="\e[45m"
NORM="\e[0m"

###

# Question 1

# Gather commits from remote repo
(git fetch origin)

# Stores the HEAD and upstream's SHA1 hashes
localRepo=$(git rev-parse HEAD)
remoteRepo=$(git rev-parse @{u})

# Checks if the local repo is the same as the upstream repo (comparing hashes)
if [ ${localRepo} = ${remoteRepo} ]
then
	echo -e "${BBLUE}The local repo is up to date with the remote repo.${NORM}"
else
	echo -e "${BRED}The local repo is NOT up to date with the local repo.${NORM}"
fi

###

# Question 2

# Returns the uncommited changes and puts them in a file
(git diff) > changes.log
echo "changes.log created."

###

# Question 3

# Returns any and all lines with #TODO in them in the project and puts them in a file
(grep -r -h "#TODO" --exclude="ProjectAnalyze.sh" --exclude="changes.log" --exclude="todo.log" .) > todo.log
echo "todo.log created."

###

# Question 4

# Adds 'main = undefined' to any Haskell files which lack this line to avoid Main is not defined error.
find -name "*.hs" | xargs -I {} grep -l -L "main = undefined" {} | xargs -I{} sh -c "(echo 'main = undefined') >> {}"

# Returns the errors of all Haskell files in the project (and paths to the erroneous files) and puts them in a file
(find -name "*.hs" | xargs -I {} sh -c "printf '${NORM}${BRED}Errors for the Haskell file: '; readlink -f {}; printf '${NORM}${REV}\n\n${NORM}${BRED}'; ghc -fno-code {}; echo -e '${NORM}${REV}'; echo;") &> error.log
echo "error.log created."

###

# Question 5

# Asks the user if they want the created files printed out and does as they please
echo "Would you like a print-out of the created files? (Y/N)"
while read response;
do
	if [ ${response} = "Y" ] || [ ${response} = "y" ]
	then
		echo
		echo -e "${BMAG}Contents of changes.log:${NORM}${REV}"
		cat changes.log
		echo -e "${NORM}"
		echo -e "${BMAG}Contents of todo.log:${NORM}${REV}"
                cat todo.log
                echo -e "${NORM}"
		echo -e "${BMAG}Contents of error.log:${NORM}${REV}"
                cat error.log
                echo -e "${NORM}"
		break
	elif [ ${response} = "N" ] || [ ${response} = "n" ]
	then
		echo "Thanks for using the Project Analyzer."
		break
	else
		echo -e "${BRED}Please enter either Y for yes or N for no.${NORM}"
	fi
done

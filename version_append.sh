#!/usr/bin/bash

shopt -s globstar

files=()
for i in ./**/*
do
    if [ -f "$i" ];
    then
        #printf "Path: %s\n" "${i##/*}" # shortest suffix removal
        noSrc=${i/'./src'/} # remove src
        noPublic=${noSrc/'./public'/}
        if [[ $noPublic == /* ]]
        then
            $files += "$noPublic"
        fi
    fi
done
echo $files[@]
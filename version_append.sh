#!/bin/bash

# This is used to help with file caching.
# Most browsers cache JS file, meaning that when a new version is pushed
# perople visiting the page will load the cached JS file, thus
# not seeing any of the changes from the update.
# This appends a hash to each local path in index.html and /dist.elm.js
# meaning that files can be cached for arbitrarily long amounts of time
# but still detect and load new versions when changed on the server
# this script is run automatically as part of the dockerfile

shopt -s globstar

files=()
paths=()
j=0
for i in ./public/**/*
do
    if [ -f "$i" ];
    then
        #printf "Path: %s\n" "${i##/*}" # shortest suffix removal
        hash=`sha256sum "${i##/*}" | awk '{ print $1 }'`
        #hash= sha256sum "${i}"
        noSrc=${i/'./src'/} # remove src
        noPublic=${noSrc/'./public'/}
        if [[ $noPublic == /* ]]
        then
            paths[$j]="${noPublic}"
            files[$j]="$noPublic?v=${hash:0:12}"
            j=$((j+1))
        fi
    fi
done
# Update the html and elm files
html=`cat ./public/index.html`
elm=`cat ./public/dist/elm.js`
css=`cat ./public/css/blur.css`
for i in $(seq 0 $j)
do
    old_name=${paths[$i]}
    echo $old_name
    new_name=${files[$i]}
    echo $new_name
    html=${html//$old_name/$new_name}
    elm=${elm//$old_name/$new_name}
    css=${css//$old_name/$new_name}
done
echo $html > ./public/index.html
echo $elm > ./public/dist/elm.js
echo $css > ./public/css/blur.css
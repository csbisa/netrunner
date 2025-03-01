#!/bin/bash

SED_STR=$(paste replace-source.txt replace-jp.txt -d/ | sed 's/^/s\//;s/$/\//;s/\&/\\&/g;s/\[/\\[/g;s/\]/\\]/g' | tr '\n' ';')

find ./src/clj/ -type f -exec sed -i "$SED_STR" {} \;

# don't want to update localization files, naturally
git checkout HEAD src/cljc/i18n/
# there's something missing. move-card will take server strings but those are in english form
# TODO check if also replacing the cljs 'fixes' this
git checkout HEAD src/clj/game/core/actions.clj

#!/bin/bash

[[ $# -ne 2 ]] && echo "Usage: $0  ‹git dir› ‹commit message›" && exit 1
dir="$1"
msg="$2"

apps=apps.js

cd "$dir" \
    && echo 'apps = [' >$apps \
    && find . -name meta.txt | cut -c3- | sed 's/.........$/",/' | sed 's/^/"/' | tr -d '\n' >>$apps \
    && echo '];' >>$apps \
    && git pull origin gh-pages \
    && git add -A . \
    && git commit -am "$msg" \
    && git push origin gh-pages
cd -

#!/bin/bash

# Generate content for other.erldocs.com

[[ $# -eq 0 ]] && echo "Usage: $0  ‹tmp dir› ‹website dir› ‹repo URL›" && exit 1

odir="$2"
[[ ! -d "$odir" ]] && echo "$odir is not a directory" && exit 2
url="$3"

generator='./erldocs_other'
[[ ! -x $generator ]] && [[ ! -L $generator ]] && echo "$generator not executable" && exit 2

kf (){
    local key="$1"
    local metafile="$2"
    erl -noshell -eval 'try {ok, Terms} = file:consult("'"$metafile"'"), {_, Value} = lists:keyfind('$key', 1, Terms), io:format("~s\n", [Value]) catch error:{badmatch,{error,enoent}} -> no end.' -s init stop
}


tmp="$1"/$RANDOM
mkdir -p $tmp
rm -rf $tmp/*
log=$tmp/_.txt

$generator     \
    "$url"     \
    -o $tmp    \
    --base '/' \
    2>&1 | tee $log

err_code=${PIPESTATUS[0]}
[[ $err_code -ne 0 ]] && echo "$generator failed, given $url" && exit 3

meta=$tmp/meta.terms
url=$(kf url $meta)
[[ "$url" = '' ]] && echo "generating meta failed, given $url" && exit 3
target_path=$(kf target_path $meta)
dest="$odir"/$target_path
mkdir -pv "$dest"
rm -rf    "$dest" # Instead of `rm -rf "$dest"/*` => Can `stat` "$dest" for info!
mkdir -p  "$dest"

mv -v $log "$dest"/
mv -v $meta "$dest"/meta.txt
mv -v $tmp/repo/repo.css "$odir"/
for decor in 'erldocs.css' 'erldocs.js' 'jquery.js'; do
    path=$(find $tmp/repo -name $decor | head -n 1)
    [[ '' != "$path" ]] && mv -v "$path" "$odir"/
    find $tmp/repo -name $decor -delete
done
find $tmp/repo -type d -name '.xml' -exec rm -r "{}" \;  2>/dev/null
echo "mv'ing $tmp/repo/* to $dest/"
mv $tmp/repo/* "$dest"/

echo    "Just gen'd $url over at $dest"
echo -e "\t"http://other.erldocs.com/$target_path

if [[ 42 -eq 0 ]]; then # Desactivated
    if [[ -d "$odir"/.git ]]; then
        cd "$odir" \
            && git pull origin gh-pages \
            && git add -A $target_path \
            && git commit -am "Generated docs for $url" \
            && git push origin gh-pages
        cd -
    fi
fi

rm -rf $tmp
echo ============================================================

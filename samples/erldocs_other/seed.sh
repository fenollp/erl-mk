#!/bin/bash

# Extract URLs of Erlang projects to serve as seed.

root='https://erlangcentral.org/erlang-projects'
outf=repos.seed
touch $outf

# Temp dir
tmp=repos
mkdir $tmp || exit 1

# Max number of pages
tmp_root=$tmp/root
curl -fsSLo $tmp_root $root
max=$(grep -P '^href="\d+">\d+</a><a$' $tmp_root | cut -d '"' -f 2 | sort -r | head -n 1)
rm $tmp_root
[[ "$max" = '' ]] && exit 2

for i in $(seq 1 "$max"); do
    echo $root/$i
    # Fetch HTML page
    curl -fsSLo $tmp/$i $root/$i

    # Extract projects' path
    grep -P '"[^\s][^"]+"\s+target=[^,]+$' $tmp/$i | cut -d '"' -f 2 | tee --append $outf

    sleep 2
done

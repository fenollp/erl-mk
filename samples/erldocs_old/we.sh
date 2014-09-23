#!/bin/bash -x
mkdir -p deps/

if [[ 4 = 4 ]]; then
    case git://github.com/erlydtl/erlydtl.git in
        *github.com*)
            curl -sS 'https://codeload.github.com/erlydtl/erlydtl/zip/master' -o deps/erlydtl.zip ;
            unzip -q deps/erlydtl.zip -d deps/ && rm deps/erlydtl.zip && mv 'deps/erlydtl-master' 'deps/erlydtl' ;
            ;; *)
            git archive --format tar --remote git://github.com/erlydtl/erlydtl.git erlydtl deps/erlydtl.tar &&
            tar xvf deps/erlydtl.tar && rm deps/erlydtl.tar    ||
            git clone -n -- git://github.com/erlydtl/erlydtl.git deps/erlydtl &&
            cd deps/erlydtl && git checkout -q erlydtl && cd ../..
    esac
else
    git clone -n -- git://github.com/erlydtl/erlydtl.git deps/erlydtl &&
    cd deps/erlydtl && git checkout -q erlydtl && cd ../.. ;
fi

if [[ -f deps/erlydtl/Makefile ]];
then echo 'make -C deps/erlydtl all' ;
    make -C deps/erlydtl all  ;
else echo 'cd deps/erlydtl && rebar get-deps compile && cd ../..' ;
    cd deps/erlydtl && rebar get-deps compile && cd ../..  ;
fi

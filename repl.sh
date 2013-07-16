python3 -m http.server &> /dev/null &
server_pid=$!
rlwrap lein2 trampoline cljsbuild repl-listen
kill $server_pid

./build/sch.js: ./src/sch.hs
	hastec -Wall -fno-warn-unused-do-bind -O2 ./src/sch.hs -isrc -o ./build/sch.js

#!bash

mkdir -p docs/dist
mkdir -p docs/src
elm make src/Main.elm --output docs/dist/main.js
cp src/index.html docs/
cp src/style.css docs/src

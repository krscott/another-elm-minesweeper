#!bash

mkdir -p dist/dist
mkdir -p dist/src
elm make src/Main.elm --output dist/dist/main.js
cp src/index.html dist
cp src/style.css dist/src

# another-elm-minesweeper
Yet another Elm implementation of Minesweeper

## Intro
I created this project as an exercise to learn Elm.

## Build
Compile using elm make, then copy over the html and css files.

```bash
mkdir -p dist
elm make src/Main.elm --output dist/main.js
cp src/index.html src/style.css dist
```

## Live Dev Environment
Elm-live will refresh the browser window refresh whenever an .elm file is
updated in `src`.

Install elm-live if it is not already installed:
```bash
npm install --global elm-live
```

Then, run the script `live.sh` to start elm-live.
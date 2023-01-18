# Dog Nabbit

Dog Nabbit is a simple dice game, designed by Christopher St. Clair.

Rules are in [dog-nabbit-rules.pdf](https://raw.githubusercontent.com/billstclair/dog-nabbit/main/site/dog-nabbit-rules.pdf).

## Elm Reactor

The non-networked simulator may be run in Elm Reactor:

```
cd .../dog-nabbit
elm reactor
```

In another shell, compile the source into `site/elm.js`:

```
cd .../dog-nabbit
bin/build
```

Then aim your browser at https://localhost:8000/site/index.html

## elm-test

If you install `elm-test`, you can test the JSON encoding/decoding.

Once:

```
cd .../dog-nabbit
npm install elm-test
```

Test the JSON encoding/decoding:

```
cd .../dog-nabbit
elm-test
```



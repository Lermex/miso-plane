# Miso Plane

A flappy bird clone written in [Haskell](http://haskell.org/) with [Miso](https://github.com/dmjio/miso) using assets by [Kenney](http://kenney.nl/assets/tappy-plane). Adapted with modifications from [Elm Plane](https://github.com/odedw/elm-plane).

## Play

https://miso-plane.lermex.net/

## Build

You'll need to have [Nix](https://nixos.org/nix/download.html).
Then:
```
nix-shell -A env
make build
```

You can then open the compiled html file manually.

Optionally, `make open` uses npx and browserify to open the file with auto-reload.
# Demo-App
eine einfache Todo-Liste

## Backends

- [Haskell](./server/Haskell)
- [C#](./server/Csharp)
- [F#](./server/Fsharp)

## Client

- [Elm](./client/Elm)

## Build

Ich verwende hier `make` - für Windows funktioniert [diese Version](http://gnuwin32.sourceforge.net/packages/make.htm)

je nach gewünschtem Backend über:

- `make haskell`
  - Voraussetzung: `stack` installiert und eingerichtet
- `make csharp`
  - Voraussetzung: **.net Core 2.x** installiert (getestet mit 2.0)
- `make fsharp`
  - Voraussetzung: **.net Core 2.x** installiert (getestet mit 2.0)


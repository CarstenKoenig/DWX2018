---
author: Carsten König
title: Einstieg in Elm
date: 26. Juni 2018
github: https://github.com/CarstenKoenig/DWX2018 
---

## Code / Slides

[github.com/CarstenKoenig/DWX2018](https://github.com/CarstenKoenig/DWX2018)

:::notes
Ablauf:

- was ist Elm
- TEA 1
- Demo: `beginnerProgram`
- TEA 2 / Decoder
- Demo: Model, Decoder, Http
- optional: ganzes Projekt vorstellen
- weitere Hinweise
- FAQ

:::

## was ist Elm?

- *freundliche* funktionale Sprache
  - ML Syntaxfamilie (Ocaml, F#, Haskell, ...)
  - *pure*/*total*
- wird in JavaScript übersetzt
- Web-Frontendentwicklung

:::notes

- Beginnerfreundlich, gute Kompilerfehler, gute Dokumentation, nicht zu abstrakt
- recht einfach zu erlernen
- etwas gewöhnungsbedürftige Syntax - lohnt sich aber zu lernen

:::

# **T**he **E**lm **A**rchitecture

:::notes

man könnte sagen Elm ist diese Architektur
mit einer Sprache darum herum

auch **MVU** genannt

:::

## Model

![Zustand](../images/Model.png)

:::notes

- single source of truth
- der Zustand der Applikation

:::

## View

![`view : Model -> Html ..`](../images/View.png)

---

![Runtime <-> DOM](../images/ViewRuntime.png)

## Events

![`: Html Msg`](../images/Update1.png)

---

![](../images/Update2.png)

---

![`update : Msg -> Model ..`](../images/Update3.png)

---

![`update : Msg -> Model -> Model`](../images/Update4.png)

## Elm Architektur

![TEA](../images/TEA.png)

## DEMO

# TEA für Fortgeschrittene

## Seiteneffekte

::: incremental

- wie definieren wir einen Timer?
  - **Subscriptions**
- wie kommunizieren wir mit dem Backend?
  - **Commands**

:::

## Sub

![`subscribe`](../images/Sub1.png)

---

![Verarbeitung über `update`](../images/Sub2.png)

## Cmd

![Runtime bekommt `Cmd`](../images/Cmd1.png)

---

![Verarbeitung über `update`](../images/Cmd2.png)

## erweitertes Program

```haskell
program :
    { init          : (model, Cmd msg)
    , update        : msg -> model -> (model, Cmd msg)
    , subscriptions : model -> Sub msg
    , view          : model -> Html msg
    }
    -> Program Never model msg
```

# Decoder / Requests

## Json &rarr; Elm-Typ

```haskell
decodeString : Decoder a -> String -> Result String a
```

## primitive Decoder

```haskell
Json.Decode.int    : Decoder Int
Json.Decode.string : Decoder String
Json.Decode.bool   : Decoder Bool
..
```

## Kombinatoren

```haskell
Json.Decode.list  :              Decoder a -> Decoder (List a)
Json.Decode.field : String ->    Decoder a -> Decoder a

Json.Decode.map2  : (a -> b -> val) ->
                    Decoder a -> Decoder b -> Decoder val

```

## Kommunikation mit Backend

```haskell
Http.send : (Result Error a -> msg) -> Request a -> Cmd msg

Http.get  : String         -> Decoder a -> Request a
Http.post : String -> Body -> Decoder a -> Request a
```

## DEMO

# Tools und Installation

## Installation

- Installer für Windows und Mac
- Alle Plattformen: `npm install -g elm`
- empfehlenswert: `npm install -g elm-format`

## Editor

- Online:
  - TryElm [http://elm-lang.org/try](http://elm-lang.org/try)
  - Ellie [https://ellie-app.com/new](https://ellie-app.com/new)
- Editor-Support
  - VS.code mit [vscode-elm](https://github.com/Krzysztof-Cieslak/vscode-elm)
  - Atom, Brackets, Emacs, IntelliJ, ... siehe [Elm Guide](https://guide.elm-lang.org/install.html)
  
## elm make

- initialisieren eines Projekt `elm make`
- kompilieren eines Projekts nach JavaScript `elm make Main.elm --output=main.js`

## elm repl

**R**ead **E**val **P**rint **L**oop

- Konsole `elm repl`
- Online [http://elmrepl.cuberoot.in/](http://elmrepl.cuberoot.in/)

---

### elm reactor

- Kompilieren und Anzeigen von Elm Modulen im Browser
- *Debugger*

---

### elm package

- Herunterladen von Packages `elm package install elm-lang/core`
- Veröffentlichen von Packages `elm package publish`
  - erzwingt **semver**
- zeigt Unterschiede zwischen Versionen `elm package diff elm-lang/core 3.0.0 4.0.0`

# JavaScript Interop

## Ports

---

- sollten in eigenem `port module` liegen
- `port name : output -> Cmd msg` für Elm nach JS
- `port name : (input -> msg) -> Sub msg` für JS nach Elm

```haskell
port module Alert exposing (..)


port show : String -> Cmd msg
```

---

Javascript kann `Cmd` Ports *subscriben*:

```js
var app = Elm.Main.embed(node);
app.ports.show.subscribe (function(text){
  alert(text);
});
```

und an `Sub` Ports senden:

```js
app.ports.name.send(input)
```

---

- Daten über *Ports* übertragen
- automatische [Konvertierung](https://guide.elm-lang.org/interop/javascript.html#customs-and-border-protection) der meisten Elm-Datentypen
- `Value` zum Austausch empfohlen
  - Verwendung über `Decoder`
  - und [`decodeValue`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode#decodeValue)

# Vielen Dank

## Links und co.

- Elm Guide Online: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)
- Installieren: [https://guide.elm-lang.org/install.html](https://guide.elm-lang.org/install.html)
- Package Verzeichnis / Docs: [http://package.elm-lang.org/](http://package.elm-lang.org/)
- *fancy Search* [https://klaftertief.github.io/elm-search/](https://klaftertief.github.io/elm-search/)
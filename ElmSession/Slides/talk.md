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
- Demo: Todo / Einstieg Html
- TEA 1
- Demo: `beginnerProgram`
- TEA 2 / Decoder
- Demo: Model, Decoder, Http
- optional: ganzes Projekt vorstellen
- weitere Hinweise
- FAQ
:::

## was ist Elm?

- Web-Frontendentwicklung 
- wird in JavaScript übersetzt
- **rein** funktionale Sprache
- ML Syntaxfamilie (Ocaml, F#, Haskell, ...)

:::notes
Help
:::

# **T**he **E**lm **A**rchitecture

:::notes
Einführung
:::

## Model

![Zustand](../images/Model.png)

:::notes

- single source of truth
- der Zustand der Applikation

:::

## View

![`view : Model -> Html`](../images/View.png)

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

# DEMO

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

# DEMO

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

***

## The Elm Architecture

---

### Model

Beschreibung des gesamten Zustands eines Programms

```elm
type alias Model =
    { augenzahl : Int
    }
```

---

### View

Wandelt das **Modell** in eine HTML Darstellung um, die Elm dann anzeigt

```elm
view : Model -> Html Never
view model =
    viewDice model.augenzahl
```

---

### Messages

zeigt eine gewünschte Zustandsänderung an

```elm
type Msg
    = AugenzahlAendern Int
```

werden z.B. in der View über Events ausgelöst

```elm
viewSwitchButton n =
    button
        [ onClick (AugenzahlAendern n)
```

---

### Update

verknüpft eine **Message** mit aktuellen Zustand und liefert neuen Zustand

```elm
update : Msg -> Model -> Model
update msg model =
    case msg of
        AugenzahlAendern n ->
            { model | augenzahl = n }
```

der neue Zustand wird dann über `view` angezeigt

---

### neue `main`

```elm
main : Program Never Model Msg
main =
    beginnerProgram
        { model = init
        , update = update
        , view = view
        }
```

---

## Effekte

- bisher alles **pure**
- wie bekommen wir Seiteneffekte?
  - Zufallszahl
  - HTTP Requests
  - Systemzeit
  - ...

---

### Subscriptions

Wenn wir über nicht von der View ausgelöste Ereignisse benachrichtigt werden wollen

- WebSocket Nachricht
- Taste gedrückt
- Maus bewegt
- Timer
- ...

---

### Subscriptions

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick
```

---

### Commands

Lassen uns Seiteneffekte auslösen

- HTTP Request
- Zufallszahl erzeugen
- ...

---

### Commands

```elm
update msg model =
    case msg of
	    ...

        ZufaelligAendern ->
            ( model, zufallszahlErzeugen )


zufallszahlErzeugen : Cmd Msg
zufallszahlErzeugen =
    Rnd.generate AugenzahlAendern (Rnd.int 1 6)
```

---

### neue `main`

```elm
main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
```

***

## Komponenten mit der TEA

---

### Dice - Komponente

- in eigenes Modul mit entsprechenden `init`, `update`, `view`, `Cmd` Funktionen
- und eigenem `Model`, `Msg` Typ


```elm
module Dice exposing (Model, Msg, init, update, view, random)
```

---

### in Main

Komponenten-Model in `Model` aufnehmen:


```elm
type alias Model =
    { dice : Dice.Model
    }

```

---

### in Main

Komponenten-*Messages* wrappen

```elm
type Msg
    = DiceMsg Dice.Msg
    | ZufaelligAendern
```

---

### in Main

Mappen der Komponenten-Messages, Commands, Subscriptions 
mit `Html.map`, `Cmd.map` und `Sub.map`

```elm
init : ( Model, Cmd Msg )
init =
    let
        ( diceModel, diceInitCmd ) =
            Dice.init
    in
        ( Model diceModel, Cmd.map DiceMsg diceInitCmd )
```

---

## Demo mehrere Kopien

***

## Einbetten in HTML

---

```html
  <div id="main"></div>
  <script src="app.js"></script>
  <script>
    var node = document.getElementById('main');
    var app = Elm.App.embed(node);
  </script>
```

***

## Ports

---

- sollten in eigenem `port module` liegen
- `port name : output -> Cmd msg` für Elm nach JS
- `port name : (input -> msg) -> Sub msg` für JS nach Elm


```elm
port module Alert exposing (..)


port show : String -> Cmd msg
``` 

---

Javascript kann `Cmd` Ports *subscriben*:

```js
var app = Elm.TeaDemoPorts.embed(node);
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
- es gelten [Einschränkungen](https://guide.elm-lang.org/interop/javascript.html#customs-and-border-protection)
- `Value` zum Austausch empfohlen
  - Verwendung über `Decoder` 
  - und [`decodeValue`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode#decodeValue)


*** 

## Links und co.

- Elm Guide Online: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)
- Installieren: [https://guide.elm-lang.org/install.html](https://guide.elm-lang.org/install.html)
- Package Verzeichnis / Docs: [http://package.elm-lang.org/](http://package.elm-lang.org/)
- *fancy Search* [https://klaftertief.github.io/elm-search/](https://klaftertief.github.io/elm-search/)

***

# Vielen Dank






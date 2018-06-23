---
author: Carsten König
title: Einstieg in Elm
date: 26. Juni 2018
github: https://github.com/CarstenKoenig/DWX2018 
---

# ![Elm](../images/Elm.png)

:::notes

Agenda:

- paar kurze Worte zu Elm
- Elm Architektur (Demo)
- TEA ausgebaut (Sub, Cmd, Decoder, Requests, Demo)
- Fragen

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

## Zustand

![single source of truth](../images/Model.png)

:::notes

- der Zustand der Applikation
- wird von der Runtime verwaltet
- wie alles in Elm *unveränderlich*

:::

## View

![`view : Model -> Html ..`](../images/View.png)

:::notes

- immer wenn *neu gezeichnet* werden soll
- `view` wandelt Model in eine VDom Modell um

:::

---

![Runtime <-> DOM](../images/ViewRuntime.png)

:::notes

- Runtime *diff*t die Rep. mit DOM und ändert nur nötige Knoten

:::

## Events

![`: Html Msg`](../images/Update1.png)

:::notes

- VDom kann Events binden
- Ereignisse geben Messages weiter
- Typ der Message ist durch den Typ der Rep. fixiert

:::

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

![`subscriptions`](../images/Sub1.png)

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

# JavaScript Interop

## Elm-App einbetten / Flags

```html
  <script src="elm.js"></script>

  <div id="main"></div>

  <script>
    var node = document.getElementById('main');
    var app = Elm.Main.embed(node);

    // ..

    var app = Elm.Main.fullscreen(
      { baseUrl: 'http://localhost:8080/' });
  </script>

```

## Ports

---

## Elm &rarr; JS

Elm:

```haskell
port module PortModule exposing (..)

port toJS : String -> Cmd msg
```

JavaScript:

```js
var app = Elm.Main.embed(node);
app.ports.toJS.subscribe (function(text){
  alert(text);
});
```

---

## Elm &larr; JS

Elm:

```haskell
port module PortModule exposing (..)

port fromJs : (String -> msg) -> Sub msg
```

JavaScript:

```js
app.ports.fromJS.send(input);
```

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

---

### elm-format

automatisches Formatieren von `.elm` Dateien - weit verbeitet in der Community

    npm install -g elm-format

---

### elm-live

auto-compile/reload nach Speichern von Quellcodedateien

    npm install -g elm-live

[architectcodes/elm-live](https://github.com/architectcodes/elm-live)

---

### elm-analyse

*linter*  für Elm

    npm install -g elm-analyse

[stil4m/elm-analyse](https://github.com/stil4m/elm-analyse)

---

### html-to-elm

HTML &rarr; Elm [Online-Konverter](https://mbylstra.github.io/html-to-elm/)

---

### json-to-elm

JSON &rarr; Elm [Online-Konverter](https://eeue56.github.io/json-to-elm/)

Erstellt *Decoder* und *Encoder* aus einem JSON Text

# Fragen?

# Vielen Dank

## Links und co.

- Code & Slides [github.com/CarstenKoenig/DWX2018](https://github.com/CarstenKoenig/DWX2018)

- Elm Guide Online: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)
- Installieren: [https://guide.elm-lang.org/install.html](https://guide.elm-lang.org/install.html)
- Package Verzeichnis / Docs: [http://package.elm-lang.org/](http://package.elm-lang.org/)
- *fancy Search* [https://klaftertief.github.io/elm-search/](https://klaftertief.github.io/elm-search/)
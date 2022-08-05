# Un lenguaje vale más que mil dibujos.
La idea original de este proyecto esta sacada del siguiente [articulo](https://cs.famaf.unc.edu.ar/~mpagano/henderson-funcgeo2.pdf) 

## Requerimientos

Usamos [gloss](https://hackage.haskell.org/package/gloss) para graficar.

## Instalación

Si tenés algún Linux debería ser suficiente con que instales el paquete de `ghc` y `cabal`. En macOS se instalan fácil con `brew`.

Para instalar `gloss` usamos `cabal`:

```bash
cabal update
cabal install gloss
```

Podés comprobar que funcione haciendo:

```bash
$ ghci
Prelude> import Graphics.Gloss
Prelude Graphics.Gloss> let win = InWindow "window" (200,200) (0,0)
Prelude Graphics.Gloss> display win white $ circle 100
```

Si tuviste un fallo en el proceso abajo hay algunas ayudas.

## Posibles problemas de instalación

### Missing C library

Si al tratar de instalar gloss tiene el siguiente mensaje de error:

```
    Missing C library: GL
```

pueden solucionarlo instalando las siguientes librerías de sistema.

```bash
sudo apt-get install freeglut3 freeglut3-dev
```

### Could not load module

Si al cargar el archivo `Main.hs` les tira

```
Main.hs:4:1: error:
    Could not load module ‘Graphics.UI.GLUT.Begin’
    It is a member of the hidden package ‘GLUT-2.7.0.16’.
    You can run ‘:set -package GLUT’ to expose it.
    (Note: this unloads all the modules in the current scope.)
    Use -v (or `:set -v` in ghci) to see a list of the files searched for
```

Deben pasar pasar un argumento a `ghci`:

```bash
ghci -package GLUT
```

### macOS issue: NSInternalInconsistencyException

En macOS hay un problema en el cual les tira el siguiente mensaje:

```
2022-03-25 08:54:19.343 ghc[2327:42375] GLUT Fatal Error: internal error: NSInternalInconsistencyException, reason: NSWindow drag regions should only be invalidated on the Main Thread!
```

Eso se soluciona pasando `-fno-ghci-sandbox` a `ghci`. Eso sí, cuando ejecuten el código de la función `main`, jamás podrán cerrar la ventana: ¡tendrán que hacer un `kill -s KILL` del proceso!

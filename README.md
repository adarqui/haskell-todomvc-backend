# haskell-todomvc-backend

Copied from my todomvc-haskell-servant-purescript repo.

Just wanted to separate this out for use with other front ends; for example, I am currently learning react-flux.

This backend is separated into a few different components:
- pure TodoMVC
- servant api

## building

```
stack build
```

## running

Listens on 1080 by default.

```
stack exec -- haskell-todomvc-backend-servant
stack exec -- haskell-todomvc-backend-servant port
stack exec -- haskell-todomvc-backend-servant 8080
```

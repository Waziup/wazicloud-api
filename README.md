WAZIUP API Server
=================

waziup-api is a component of the [Waziup platform](https://github.com/Waziup/Platform)

Install
=======

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install --fast` to install this package.

Run
===

Once installed, you can run the program as follows:
```
stack run
```

Command line help is available at `waziup-servant --help`.
API documentation can be viewed at http://localhost:3000/docs

Develop
=======

Any IDE can be used to develop. I like ghcid with this `.ghcid`:
```
--command="stack ghci --no-package-hiding --load waziup:waziup-servant --ghci-options=-fno-break-on-exception --ghci-options=-fno-break-on-error --ghci-options=-v1 --ghci-options=-ferror-spans --ghci-options=-j --ghci-options=-fno-warn-orphans" --warnings --test=:main
```
ghcid will reload the code for each change, and launch the server if everything is OK.

Work with subtrees:
```
git subtree pull --prefix=keycloak-hs keycloak-hs master --squash
git subtree push --prefix=keycloak-hs keycloak-hs master --squash
```

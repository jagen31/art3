# The Art Language

## INSTALL LOCALLY

`make install`
You must have `raco` installed and on the path.
https://download.racket-lang.org/

## TODO

- Fix the strange quasiquote unquote problem in rewriters (workaround is to use `with-syntax` instead.  which is probably better practice anyhow)
- Figure out a better way to do bindings for rewriters (use `syntax-spec` better)
- Assess the viability of merge rules / within? rules.
- better support for infinite intervals (so you don't have to always provide an end)
- possibly making lib coordinates into coordinate families
- IMPROVE THE QUALITY OF THE WRITING IN THE DOCS. Sheesh.

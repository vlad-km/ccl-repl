# ccl-repl
Its trivial Moren feature for compile source files on host compiler JSCL.

*Only for*  `os:Windows` `lisp:ccl`

## By steps

### on command line

```
wx86cl64.exe -I wx86cl64.image
```

### on ccl standard repl

```lisp
(cd "path-where-yours-jscl-placed")
(load "ccl-jscl.lisp")
(jscl:bootstrap)
(cd "path-where-yours-ccl-repl-placed")
(load "ps.lisp")
(load "hsdf.lisp")
(load "ccl-repl.lisp")
(save-application "path-name-where-yours-ccl-image-will-be-stored")
```

### on moren development console

```lisp
(ccl:connect :args (ccl:options "-I" "path-name-where-yours-ccl-image-will-be-stored"))
(ccl:kb-setup)
(ccl:repl)
```

### on ccl-repl session

```lisp
(setq my-sys (hsdf:create :name "my-sys" :path "path"))
(hsdf:add-component my-sys "file1")
(hsdf:add-component my-sys "file2")
(hsdf:compile-it my-sys "module.js")
```


## Copyright
Copyright © 2017,2018 Vladimir Mezentsev

## License
GNU General Public License v3.0

            
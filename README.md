THIS WORK IS DEPRECATED
the code will be included in nyacc's examples directory

# guile-fhbe - work in progress
alternate backends for nyacc ffi-helper

with nyacc-3.05 or later in path ...

```
$ export GUILE_LOAD_PATH=`pwd`:$GUILD_LOAD_PATH 
$ guild compile-ffi -X -o cairo.scm-1 -b fhbe/bytestructures ffi/cairo.ffi
$ guild compile-ffi -X -o cairo.scm-2 -b fhbe/bstructs ffi/cairo.ffi
```

Or

```
> (use-modules (fhbe bstructs))
> (ccode->bstructs-sexp "typedef struct { double x; double y; } foo_t;")
$1 = ...
> ,pp $1
$2 = (begin
       (define-bstruct struct-foo (struct (i int) (d double)))
       (define-bstruct struct-foo* (* struct-foo))
       (export struct-foo struct-foo*))
```


# guile-fhbe
alternate backends for nyacc ffi-helper

with nyacc branch dev-3.02 in path ...

```
$ export GUILE_LOAD_PATH=`pwd`:$GUILD_LOAD_PATH 
$ guild compile-ffi -X -o cairo.scm-1 -b fhbe/bytestructures ffi/cairo.ffi
$ guild compile-ffi -X -o cairo.scm-2 -b fhbe/bstructs ffi/cairo.ffi
```

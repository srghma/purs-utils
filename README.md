# update-module-name-purs

# How it works

```sh
# description

$ update-module-name-purs --help
  Update module name in directory recursively

  Usage: update-module-name-purs (-d|--directory DIRECTORY)
    Adds or updates `module Foo.Bar` based on path

  Available options:
    -d,--directory DIRECTORY Base dir with .purs files
    -h,--help                Show this help text

# example usage

$ update-module-name-purs --directory /home/srghma/projects/purescript-halogen-nextjs/app

processing ./app/Foo/Bar.purs
  nothing changed
processing ./app/Foo/Baz.purs
  updated module name to "Foo.Quux"
```

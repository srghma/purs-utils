# update-module-name-purs

# How it works

![hj](https://i.imgur.com/M8OTg6X.png)

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
  updated module name to "Foo.Baz"
```

# How to install

build and

`ln -s $HOME/projects/update-module-name-purs/.stack-work/dist/x86_64-linux-nix/Cabal-3.0.1.0/build/update-module-name-purs-exe/update-module-name-purs-exe $HOME/.bin/update-module-name-purs`

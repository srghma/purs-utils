# update-module-name-purs

# How it works

![hj](https://i.imgur.com/M8OTg6X.png)

# description

```sh
$ purs-tidy-module-name --help                                                                                                                                 <<<
Finds .purs files and updates their module name

Usage: purs-tidy-module-name [-c|--color when] COMMAND

 ./src/Foo/Bar.purs -> module Foo.Bar
 ./test/Foo/Bar.purs -> module Test.Foo.Bar

Available options:
 -c,--color when          When to use colors [default: auto] [possible values:
                          auto, never] (default: auto)
 -h,--help                Show this help text
 --version                Show version information

Available commands:
 format-in-place          Update files
 check                    Throw if files are not updated
```

```sh
$ purs-tidy-module-name format-in-place --help
Usage: purs-tidy-module-name format-in-place
        [[-r|--root DIRECTORY] [-s|--src DIRECTORY] [-t|--test DIRECTORY]
          [-c|--custom ARG] |
          [DIRECTORY]]

 Update files

Available options:
 -r,--root DIRECTORY      Base dir with two directories - src/ and test/. Can
                          pass multiple -r
 -s,--src DIRECTORY       Source directory.
 -t,--test DIRECTORY      Test directory.
 DIRECTORY                Positional arguments treated as --root directories.
 -h,--help                Show this help text
```

```sh
... check command is same
```

# example usage

```sh
$ purs-tidy-module-name format-in-place /home/srghma/projects/purescript-halogen-nextjs/app

# same as

$ purs-tidy-module-name format-in-place --root /home/srghma/projects/purescript-halogen-nextjs/app

# same as

$ purs-tidy-module-name format-in-place --src /home/srghma/projects/purescript-halogen-nextjs/app/src --test /home/srghma/projects/purescript-halogen-nextjs/app/test

# same as

$ purs-tidy-module-name format-in-place --src /home/srghma/projects/purescript-halogen-nextjs/app/src --custom Test=/home/srghma/projects/purescript-halogen-nextjs/app/test

# same as

$ cd /home/srghma/projects/purescript-halogen-nextjs/app/ && purs-tidy-module-name format-in-place

processing ./app/Foo/Bar.purs
  nothing changed
processing ./app/Foo/Baz.purs
  updated module name to "Foo.Baz"
```

# How to install

build and

```
nix-build -A purs-utils.components.exes.purs-tidy-module-name && purs-tidy-module-name --help
nix-build -A purs-utils.components.exes.purs-generate-css-modules && purs-generate-css-modules --help
```

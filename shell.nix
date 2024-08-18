# (import ./default.nix).shellFor {
#   tools = {
#     cabal = "latest";
#     hlint = "latest";
#     haskell-language-server = "latest";
#   };
# }

# nix-shell
# cabal new-repl update-module-name-purs:library:update-module-name-purs-exe
# cabal new-build update-module-name-purs

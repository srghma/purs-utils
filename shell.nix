(import ./default.nix).shellFor {
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
    hpack = "latest"; # instead of stack
    hspec-discover = "latest";
    stylish-haskell = "latest"; # stylish-haskell -i -r .
  };
}

# XXX: in vscode settings set PATH instead of GHCUP

# nix-shell --run 'hpack && code .'
# code .
# cabal new-build purs-utils

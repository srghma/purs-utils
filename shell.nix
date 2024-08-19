(import ./default.nix).shellFor {
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
    hpack = "latest";
    hspec-discover = "latest";
  };
}

# XXX: in vscode settings set PATH instead of GHCUP

# nix-shell --run 'hpack && code .'
# code .
# cabal new-build purs-utils

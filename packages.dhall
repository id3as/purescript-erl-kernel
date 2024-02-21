let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.15.3-20220629/packages.dhall
        sha256:48ee9f3558c00e234eae6b8f23b4b8b66eb9715c7f2154864e1e425042a0723b

in  upstream
  with convertable-options =
    { repo = "https://github.com/natefaubion/purescript-convertable-options"
    , dependencies = [ "effect", "maybe", "record" ]
    , version = "f20235d464e8767c469c3804cf6bec4501f970e6"
    }
  with erl-process.repo = "https://github.com/id3as/purescript-erl-process.git"
  with erl-process.version = "67787f787d3f6a0523f931e651156ec82709e7f1"
  with erl-untagged-union.version = "781b2894f9ffcc91b7aea482e435bb9284596f62"
  with erl-binary =
    { repo = "https://github.com/id3as/purescript-erl-binary.git"
    , dependencies = [ "erl-lists", "maybe", "prelude" ]
    , version = "e3a5da78a9264a800eb7bad918a58de5ac57ba4c"
    }
  with pathy =
    { repo = "https://github.com/id3as/purescript-pathy.git"
    , version = "c23c7b772c37bc499503ea63867287200fa44966"
    , dependencies = [ "prelude" ]
    }

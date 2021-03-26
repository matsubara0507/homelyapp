GHC_FLAGS = [
    "-v1",
    "-j8",
    "-fdiagnostics-color=always",
    "-ferror-spans",
    "-Wall",
    "-Wcompat",
    "-Wincomplete-record-updates",
    "-Wincomplete-uni-patterns",
    "-Wredundant-constraints",
    "-optP-Wno-nonportable-include-path",
    "-DBAZEL_BUILD=1",
    "-XNoImplicitPrelude",
    "-XDataKinds",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XGeneralizedNewtypeDeriving",
    "-XLambdaCase",
    "-XMultiWayIf",
    "-XNumericUnderscores",
    "-XOverloadedLabels",
    "-XOverloadedStrings",
    "-XPolyKinds",
    "-XRankNTypes",
    "-XStandaloneDeriving",
    "-XTupleSections",
    "-XTypeApplications",
    "-XTypeFamilies",
    "-XTypeOperators",
    "-XTypeSynonymInstances",
]
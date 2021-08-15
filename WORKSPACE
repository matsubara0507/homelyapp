workspace(name = "homelyapp")

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

http_archive(
    name = "rules_python",
    sha256 = "b6d46438523a3ec0f3cead544190ee13223a52f6a6765a29eae7b7cc24cc83a0",
    urls = ["https://github.com/bazelbuild/rules_python/releases/download/0.1.0/rules_python-0.1.0.tar.gz"],
)

http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-915beedca12199fb6240c5c1f48d95f2c686aa4b",
    urls = ["https://github.com/tweag/rules_haskell/archive/915beedca12199fb6240c5c1f48d95f2c686aa4b.tar.gz"],
    sha256 = "b47628d52138aa652a0782e02efdf3b302fce980fcaf941546845e7ee488e3d2",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
)

stack_snapshot(
    local_snapshot = "//:stack-snapshot.yaml",
    name = "stackage",
    packages = [
        "base",
        "blaze-html",
        "dotenv",
        "elmap",
        "esqueleto",
        "extensible",
        "extensible-elmap",
        "file-embed",
        "mix",
        "mix-json-logger",
        "mix-plugin-persistent-sqlite",
        "persistent",
        "persistent-sqlite",
        "rio",
        "servant-blaze",
        "servant-elmap",
        "servant-server",
        "tasty",
        "tasty-hspec",
        "th-lift-instances",
        "warp",
        "yaml",
    ],
)

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

rules_haskell_toolchains(version = "8.10.4")

load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "git_repository",
)

git_repository(
    name = "rules_elm",
    remote = "https://github.com/matsubara0507/rules_elm",
    commit = "83f046f28bc5de4c776f980f149cc0bc5577a49c",
    shallow_since = "1628248957 +0900",
)

load(
    "@rules_elm//elm:repositories.bzl",
    rules_elm_repositories = "repositories",
)

rules_elm_repositories()

load(
    "@rules_elm//elm:toolchain.bzl",
    rules_elm_toolchains = "toolchains",
)

rules_elm_toolchains(version = "0.19.1")

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
    strip_prefix = "rules_haskell-fa8d2c53449ee1659f5f503cfe3141d27cb8b869",
    urls = ["https://github.com/tweag/rules_haskell/archive/fa8d2c53449ee1659f5f503cfe3141d27cb8b869.tar.gz"],
    sha256 = "2b9367d5c5878e4416fca5d4b4c11a22c3e24d73264d4b3b124d1c75bb0bd925",
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
    name = "stackage",
    packages = [
        "base",
        "dotenv",
        "esqueleto",
        "extensible",
        "mix",
        "mix-json-logger",
        "mix-plugin-persistent-sqlite",
        "persistent",
        "persistent-sqlite",
        "persistent-template",
        "rio",
        "servant-server",
        "tasty",
        "tasty-hspec",
        "th-lift-instances",
        "warp",
        "yaml",
    ],
    local_snapshot = "//:stack-snapshot.yaml",
)

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

rules_haskell_toolchains(version = "8.8.4")

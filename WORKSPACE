workspace(name = "io_tweag_inline_java")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
  name = "io_tweag_rules_haskell",
  strip_prefix = "rules_haskell-59cdb5b2fae065d8dc3837f7a9535e3060aaa383",
  urls = ["https://github.com/tweag/rules_haskell/archive/59cdb5b2fae065d8dc3837f7a9535e3060aaa383.tar.gz"]
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

http_archive(
  name = "io_tweag_rules_nixpkgs",
  strip_prefix = "rules_nixpkgs-0.5.2",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.5.2.tar.gz"]
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
  "nixpkgs_git_repository",
  "nixpkgs_package",
)

nixpkgs_git_repository(
  name = "nixpkgs",
  # Keep consistent with ./nixpkgs.nix.
  revision = "1fa2503f9dba814eb23726a25642d2180ce791c3"
)

# These dependencies are built by Nix.
prebuilt_packages = [
  "Cabal",
  "base",
  "bytestring",
  "choice",
  "constraints",
  "containers",
  "deepseq",
  "directory",
  "distributed-closure",
  "exceptions",
  "filemanip",
  "filepath",
  "ghc",
  "hspec",
  "hspec-discover",
  "inline-c",
  "language-java",
  "mtl",
  "process",
  "singletons",
  "streaming",
  "template-haskell",
  "temporary",
  "text",
  "vector",
]

nixpkgs_package(
  name = "inline-java-toolchain",
  repository = "@nixpkgs",
  nix_file_content = """
let pkgs = import <nixpkgs> {{}};
in pkgs.buildEnv {{
  name = "inline-java-toolchain";
  paths = with pkgs; [
    (haskell.packages.ghc822.ghcWithPackages (p: with p; [{0}]))
    openjdk
  ];
}}
""".format(" ".join(prebuilt_packages))
)

nixpkgs_package(
  name = "openjdk",
  repository = "@nixpkgs",
  build_file_content = """
filegroup (
  name = "lib",
  srcs = ["lib/openjdk/jre/lib/amd64/server/libjvm.so"],
  visibility = ["//visibility:public"],
)
filegroup (
  name = "bin",
  srcs = ["bin/javac"],
  visibility = ["//visibility:public"],
)
filegroup (
  name = "jni_header",
  srcs = ["include/jni.h"],
  visibility = ["//visibility:public"],
)
filegroup (
  name = "jni_md_header",
  srcs = ["include/jni_md.h"],
  visibility = ["//visibility:public"],
)"""
)

register_toolchains("//:inline-java-toolchain")

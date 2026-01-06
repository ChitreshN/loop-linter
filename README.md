# Loop Linter for Clash

**Loop Linter** is a static analysis tool implemented as a GHC Core-to-Core plugin. It is designed specifically for [Clash](https://clash-lang.org/) hardware designs to identify **combinational loops** (tight loops with no registers) at compile time.

## 📦 Installation & Usage

To use the Loop Linter in your Clash project, you need to add it as a source dependency and enable the plugin.

### 1. Add Source Repository
Add the following to your `cabal.project` file to pull the specific version:

```cabal
source-repository-package
  type: git
  location: https://github.com/ChitreshN/loop-linter
  tag: 0.5.1
```

### 2. Add Library Dependency
Add `loop-linter` to your project's `.cabal` file (or `package.yaml`) under `build-depends`:

```cabal
library
  build-depends:
    base,
    clash-prelude,
    loop-linter
```

### 3. Enable the Plugin
Enable the plugin in your GHC options within your `.cabal` file:

```cabal
  ghc-options: -fplugin=LoopLinter.Plugin
```

### 4. Build and Check
Run your standard build command:

```bash
cabal build
```

The linter writes its findings to a log file in your project root:

```text
loop-linter.log
```

## 📖 Documentation

Further details on the implementation and usage can be found in the `docs` directory:

*   [Technical Design](docs/design.adoc): Detailed architectural decisions and algorithm descriptions.
*   [Usage Guide](docs/documentation.adoc): Comprehensive guide on how the plugin works and its limitations.


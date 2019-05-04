Count the imports in a haskell project.

I wrote this to help analyse what things would be sensible to put into a custom
prelude. And also because I was curious about source imports.

The code is based on
[`graphmod-plugin`](https://github.com/mpickering/graphmod-plugin).

# Manual Usage

In order to run the plugin manually, pass the options appropiately to GHC when
compiling your package.

```
-fplugin=HsCountImports -fplugin-opt=HsCountImports:output
```

This puts a bunch of files in the `output` directory. You can collate them in the shell:

```
cat output/* | sort | uniq -c | sort -n
```

# Stack Usage

If you want to use this in a stack project, the simplest way I know of is to add
`hs-count-imports` to your stack.yaml *and* your package.cabal.

In stack.yaml, it should go in `extra-deps`, for example

```
extra-deps:
- /path/to/hs-count-imports/hs-count-imports
```

(Note the doubled final path component: the first points at this repository, the second is contained in it.)

Then in your .cabal file, add `hs-count-imports` to `build-depends`. You may need to do this in multiple places.

Finally, build with

```
stack build --ghc-options="-fplugin=GraphMod -fplugin-opt=GraphMod:output"
```

Be careful to build all components you're interested in, and to clear the
`output` directory between runs.

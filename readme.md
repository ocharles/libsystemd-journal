When uploading to Hackage, it seems that one needs to upload the documentation
manually:

```
dir=$(mktemp -d dist-docs.XXXXXX)
cabal configure --builddir="$dir"
cabal haddock --builddir="$dir" --haddock-for-hackage --haddock-option=--hyperlinked-source
cabal upload --publish -d $dir/*-docs.tar.gz
```

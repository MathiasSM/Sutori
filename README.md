# Sutori programming language

A programming language designed to make programs like stories. Or something.

This project is to be a full fledged compiler for the Sutori language, implemented in Haskell, using Alex and Happy tools for lexer and parser generation, among other things.

## Current status

Please refer to the Github project Milestones (and their related issues).

## Build

This project uses stack. To build the project:

```bash
git clone repo.git sutori
cd sutori
stack build
```

This might trigger an installation of a local GHC matching the needed version, if it's not already installed. 

To run the project:

```
stack exec sutori
```

You probably want to run it against some source file; for now, please pipe the file into the above command, like so:

```
cat file.sut | stack exec sutori
# or
stack exec sutori < file.sut
```

## Test

**Currently, there's not test suite.** There are some test files for the language in the `test` folder, along with a basic `Spec.hs` file for later.

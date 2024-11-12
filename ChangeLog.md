# Changelog

## 0.10.0

- Support for half-precision floating point `__fp16`, `_Float16`, and `_Float16x`.
- Support for bfloat16 `__bf16`.
- Support alignment specifier `_Alignas` in struct declatations.

## 0.9.4

- Support `happy-2.1`.

## 0.9.3

- Add support for `__builtin_bit_cast` to support clang toolchain,
  enabling builds with GHC 9.4+ on Windows.

## 0.9.2

- Add support for 128 bit builtin integer types

## 0.9.1

- Support parsing an empty list of clobbers after a colon in `asm` statements

## 0.9.0.2

- Correct lower bound on GHC

## 0.9.0.1

- Scrap dependency on syb

Thanks to @vmchale

## 0.9

- Make `Trav` a monad transformer
- Add `runTravTWithTravState`
- Tests now use Cabal-3
- Implement alignment calculation and correct size calculation
- Add `Show` instance for `Type`
- Add machine descriptions for x86_64 and armv7l
- Small correction in output of prettyprinter
- Allow zeros as line numbers in preprocessed directives

Thanks to @aweinstock314, @ersran9, @expipiplus1, @jkoppel, @trofi and @visq

## Changes since 0.7.0

Wed May 23 07:08:02 2018 +0200
  * Support IEC 60559 types and literals. Closes #52.

Wed May 16 07:08:15 2018 +0200
  * introduce flag for optional IEC 60559 type support (see #43)

Wed May 16 06:36:04 2018 +0200
  * WIP: towards a complete Export (#51)

Thu Feb 22 13:22:50 2018 -0500
  * [syntax] GHC.Generics.Generic and Control.DeepSeq.NFData instances (#49)
    (.cabal) drop splitBase flag

Tue Feb 20 08:34:39 2018 +0300
  * Selectively hide Prelude.<>:  GHC 8.4 compat (#45)

Tue Feb 20 00:09:16 2018 -0500
  * (Analysis) Minimal support for `__float128` (#47)

Sat Nov 25 08:59:59 2017 +0100
  * Consider builtin_convertvector from #34 for pretty printing

Sat Nov 25 00:46:33 2017 -0600
  * add support for `_Float128` (#41)

## Changes since 0.6.0

Mon Jul 31 2017
  * Improve TypeCheck output (#31)

Sun Jul 30 2017
  * Add `__float128` (fixes #33)

Mon Maz 22 2017
  * Support clang’s `__builtin_convertvector` (Fixes #34)

Wed Mar 8 2017
  * Move file and parent information to new datatype FilePosition for performance reasons

## Changes since 0.5.0

Thu Feb 23 2017
  * Make gccParseCPPArgs grab the arg to -MF -MT and -MQ

Wed Feb 15 2017
  * In enums, allow multiple attribute specifiers per enumerator

Tue Feb 21 2017
  * Allow typedef redefinition if it denotes the same type

Tue Feb 21 2017
  * Change TypeDefRef to store Type, not Maybe Type

Tue Feb 14 2017
  * Parse (and ignore) Clang __attribute__((availability(id=major.minor.rev)))

Sun Sep 11 2016
  * Add `__builtin_bswap32/64`.

Wed Jun 22 2016
  * Add `_Alignof` to Lexer.x (fixes #7)

Mon Jun 27 2016
  * Updates for C11 (part 1)
  * `_Nullable` and `_Nonnull` support as well as Warnings/lint fixes provided by Anthony Cowley (https://github.com/acowley)

Wed Mar 16 2016
  * Consider storage specifier "ThreadSpec" for global and local declarations

Tue Mar 15 2016
  * Support C11 `_NoReturn`, genearlize is_inline to FunSpecs (Syntax) / FunAttrs (SemRep)

## Changes since 0.4.3

Wed Mar 2 2016
  * Add direct base type BaseInt128 (complements previous `__int128` patch)
    ryan.gl.scott@gmail.com

Sun Feb 28 2016
  * Parse gcc-specific `__int128` type

Thu Dec 4 2014
  * Scott Kovach <dskovach@gmail.com>: added derived Eq,Ord instances to NodeInfo

## Changes since 0.4.2

Sat Jan 11 2014
  * Allow unicode characters in string/char literals and filenames

Mon Oct 27 2014
  * macos-attributes

Tue Aug 13 2013
  * Do not derive Error instances for newtypes (type parameter has non-parametric role)

Mon Aug 12 2013
  * Fix bug caused by applying posFile to nopos (reported by Mikhail Sosonkin)

## Changes since 0.4.1

Tue Mar 19 2013
  * TypeCheck: Return Left str instead of fail str (do not rely on MonadError instance of Either)
  * Improve printing of SUERefs and Ident

Thu Feb 28 2013
  * Fix parsing and printing of octal character escapes.

Tue Jun 12 2012
  * Export Annotated type class from AST module

Wed Aug 24 2012
  * Patch for alex-3.0

## Changes 0.3.1 - 0.4.1

Tue Aug 16 2011:
  * Port to ghc-7.2

Fri Apr 15 2011: Alexander Bernauer <bernauer@inf.ethz.ch>
  * Show instances (popular request) for AST types, DumpAst demo

## Changes 0.3 - 0.3.1

Thu Aug 21 benedikt.huber@gmail.com
  * add aliases for exposed parsers, in order to document them

Fri Aug 15 benedikt.huber@gmail.com
  * Remove NameMap from Data.Name. We will do this right when neccessary.
  * Parser public API: expose parsers and the Parser Monad
  * ParserMonad: Return updated name supply when executing parser
  * Parser: Expose expression, statement, declaration and file parsers
  * Data: Add newNameSupply ~ (namesStartingFrom 0)

Thu Aug 14 17:13:29 CEST 2008  iavor.diatchki@gmail.com
  * Add a utility function to create a "blank" set of cpp arguments.
  * Make that analysis traversal monad abstract.
  * Export the type synonym "Register" (and bump version)

Wed Aug 13 12:00:57 CEST 2008  benedikt.huber@gmail.com
  * add Data.Position: internalIdentAt

## Old Changes

Mon Jun  9 23:12:46 CEST 2008  benedikt.huber@gmail.com
  * License switched to 3-clause BSD
  $
-  In accordance with the original authors, Language.C is now licensed as BSD-3.
   See:
   http://haskell.org/pipermail/c2hs/2008-June/000833.html
   http://haskell.org/pipermail/c2hs/2008-June/000834.html
   http://haskell.org/pipermail/c2hs/2008-June/000835.html


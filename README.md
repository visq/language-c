# Language.C

Language.C is a parser and pretty-printer framework for C11 and the extensions of gcc.

See http://visq.github.io/language-c/

## C Language Compatibility

Currently unsupported C11 constructs:
 - static assertion 6.7.10 (`_Static_assert`)
 - generic selection 6.5.1.1 (`_Generic`)
 - `_Atomic`, `_Thread_local`
 - Universal character names

Currently unsupported GNU C extensions:
 - `__auto_type`
 - `__builtin_offsetof`
   `char a[__builtin_offsetof (struct S, sa->f)`
 - `_Decimal32`
 - Extended assembler
   `__asm__ __volatile__ ("" : : : )`;
   `__asm__ goto ("" : : : : label)`;
 - `__attribute__((packed))`: types featuring this attribute may have an
   incorrect size or alignment calculated.

### IEC 60559:

Since `language-c-0.8`, extended floating point types are supported (gcc 7 feature). Package maintainers may decide to disable these types (flag `iecFpExtension`) to work around the fact that the `_Float128` type is redefined by glibc >= 2.26 if gcc < 7 is used for preprocessing:

```
  /* The type _Float128 exists only since GCC 7.0.  */
  # if !__GNUC_PREREQ (7, 0) || defined __cplusplus
  typedef __float128 _Float128;
  # endif
```

## Examples

A couple of small examples are available in `examples`.

## Testing

See `test/README`.

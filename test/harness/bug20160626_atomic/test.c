/* Form 1 */
/* without storage class */

/* simple */
_Atomic int a1;
int unsigned _Atomic a2;
/* qualified */
inline volatile _Atomic int a3(void)
{
}
/* attributed */
inline _Noreturn __attribute__((unused)) _Atomic int * a4(void)
{
}
inline _Noreturn _Atomic __attribute__((unused)) int * a5(void)
{
}

/* with storage class */

/* simple */
extern _Atomic int b1;
int unsigned _Atomic extern b2;
/* qualified */
static inline volatile _Atomic int b3(void)
{
}
/* attributed */
static inline _Noreturn __attribute__((unused)) _Atomic int * b4(void);
inline extern _Noreturn _Atomic __attribute__((unused)) int * b5(void)
{
}

/* Form 2 */
/* without storage class */

/* simple */
_Atomic(int) x1;
/* qualified */
inline volatile _Atomic(int) x2(void)
{
}
/* attributed */
inline _Noreturn __attribute__((unused)) _Atomic(int) x3(void)
{
}

/* with storage class */

/* simple */
extern _Atomic(int) x1a;
/* qualified */
static volatile  _Atomic(int) x2a;
/* attributed, newline between _Atomic and ( */
static volatile __attribute__((unused)) _Atomic
( unsigned long long ) x3a;

/* This won't parse in language-c, because _Atomic needs to be directly followed by ( */
/*   in the preprocessed input */

/*  extern _Atomic */
/*  make #line 1 "f" */
/*  ( int ) x4a; */

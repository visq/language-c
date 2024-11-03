/* extended floating point types */
static void f(void)
{
    _Float32 a1 = 2.0f;
    _Float32x b1 = 2.0fi;
    _Float64 c1 = 2.0f + a1 + b1;
    _Float64x d1 = 2.0;
    _Float128 e1 = 2.0;
    __float128 g1 = 2.0 + e1;
    _Float16 h1 = 2.0;
    _Float16x i1 = 2.0;
 
    _Float32 a = 2.0f32;
    _Float32x b = 2.0if32x;
    _Float64 c = 2.0f64;
    _Float64x d = 2.0f64x;
    _Float128 e = 1.0f128 + 2.0f128j;
    __float128 g = 2.0;
#ifdef HAS_FLOAT_128X /* not supported in i686 gcc7 */
    _Float128x f = 2.0f128x;
    _Float128x f1 = 2.0 + d1 + e1;
#endif

}

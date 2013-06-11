create or replace
PACKAGE GZ_GEODESIC  AUTHID CURRENT_USER AS 

  /* A package to compute geodesic distances very accurately (in the nanometers!) */
  -- Hardwired via g_a and g_r to use GRS80 at present.
  -- Unfortunately 1.5 times slower than Oracle Spatial at present. (After using
  -- new_arctan this really helped !!!)
  
FUNCTION AngNormalize(x NUMBER) RETURN NUMBER;
FUNCTION AngRound( x NUMBER) RETURN NUMBER;
FUNCTION Astroid(x NUMBER,y NUMBER) RETURN NUMBER;
FUNCTION A1M1F(eps NUMBER) RETURN NUMBER;
FUNCTION A2M1F(eps NUMBER) RETURN NUMBER;
FUNCTION A3F(f NUMBER,eps NUMBER) RETURN NUMBER;
FUNCTION ASINH(x NUMBER) RETURN NUMBER;
FUNCTION ATANH(x NUMBER) RETURN NUMBER;
FUNCTION C_ATAN2(YIn NUMBER, XIn NUMBER) RETURN NUMBER Deterministic;
FUNCTION CBRT(x NUMBER) RETURN NUMBER;
PROCEDURE C1F(eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE);
PROCEDURE C12F(eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,C2 IN OUT NOCOPY MDSYS.SDO_LIST_TYPE);

PROCEDURE C1PF(eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE);
PROCEDURE C2F(eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE);
PROCEDURE C3F(f NUMBER,eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE);
FUNCTION EXPM1(x NUMBER) RETURN NUMBER;
FUNCTION HYPOT(x NUMBER,y NUMBER) RETURN NUMBER;
FUNCTION Inverse(plat1 NUMBER,plon1 NUMBER,
                 plat2 NUMBER,plon2 NUMBER,
                 s12 IN OUT NOCOPY NUMBER, azi1 IN OUT NOCOPY NUMBER,
                 azi2 IN OUT NOCOPY NUMBER,m12 IN OUT NOCOPY NUMBER) RETURN NUMBER;
FUNCTION InverseStart(sbet1 NUMBER, cbet1 NUMBER,sbet2 NUMBER, cbet2 NUMBER,lam12 NUMBER,
                                    salp1 IN OUT NOCOPY NUMBER, calp1 IN OUT NOCOPY NUMBER,
                   --                 // Only updated if return val >= 0
                                    salp2 IN OUT NOCOPY NUMBER, calp2 IN OUT NOCOPY NUMBER,
--                                    // Scratch areas of the right size
                                    C1a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, C2a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN NUMBER;

FUNCTION ISFINITE(x NUMBER) RETURN BOOLEAN;
PROCEDURE Lengths(eps NUMBER, sig12 NUMBER, ssig1 NUMBER, csig1 NUMBER, 
                  ssig2 NUMBER, csig2 NUMBER,cbet1 NUMBER, cbet2 NUMBER,
                  s12b IN OUT NOCOPY NUMBER,m12a IN OUT NOCOPY NUMBER, 
                  m0 IN OUT NOCOPY NUMBER, 
                  C1a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, C2a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE);
FUNCTION Lambda12(sbet1 NUMBER, cbet1 NUMBER,  sbet2 NUMBER,  cbet2 NUMBER,salp1 NUMBER,  calp1 IN OUT NOCOPY NUMBER,
                                salp2 IN OUT NOCOPY NUMBER, calp2 IN OUT NOCOPY NUMBER,
                                sig12 IN OUT NOCOPY NUMBER,
                                ssig1 IN OUT NOCOPY NUMBER, csig1 IN OUT NOCOPY NUMBER,
                                ssig2 IN OUT NOCOPY NUMBER, csig2 IN OUT NOCOPY NUMBER,
                                eps IN OUT NOCOPY NUMBER, diffp BOOLEAN, dlam12 IN OUT NOCOPY NUMBER,
                                 C1a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,  
                                 C2a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,  C3a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN NUMBER;
                                 
FUNCTION LOG1P(x NUMBER) RETURN NUMBER;
FUNCTION NEW_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER DETERMINISTIC;
PROCEDURE SINCOSNORM(sinx IN OUT NOCOPY NUMBER,cosx IN OUT NOCOPY NUMBER);
FUNCTION SINSERIES(sinx NUMBER,cosx NUMBER,C MDSYS.SDO_LIST_TYPE,NN PLS_INTEGER) RETURN NUMBER;

g_A3x          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
g_C3x          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
g_C4x          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
g_ord             PLS_INTEGER := 6;
--// Underflow guard.  We require
--  //   eps2 * epsilon() > 0
--  //   eps2 + epsilon() == epsilon()
g_eps2            NUMBER := 3.16227766016837933199889354443271853372*1.E-64;
g_pi              NUMBER := 3.1415926535897932384626433832795028842;
g_to_radians      NUMBER := 0.0174532925199432957692369076848861271344;  -- pi/180
g_to_degrees      NUMBER := 57.29577951308232087679815481410517033235; --180./pi
--SQL> select 1.00000000000000000000000000000000000001 a from dual;  1 + 1.E-38
g_tol0            NUMBER := 1.E-38;
g_tol1            NUMBER; -- := 100.*g_tol0;
g_tol2            NUMBER; -- := sqrt(g_tol0);
g_xthresh         NUMBER; -- := 1000. * g_tol2;
g_a               NUMBER := 6378137.0;      -- semi-major axis for GRS80
g_r               NUMBER := 298.257222101;  -- reciprocal flattening for GRS80
g_f               NUMBER; --:= 1./g_r;
g_f1              NUMBER; -- := 1. -g_f;
g_e2              NUMBER; -- := g_f*(2. - g_f);
g_ep2             NUMBER; -- := (g_e2/(g_f*g_f));  --// e2 / (1 - e2)
g_n               NUMBER; -- := g_f/(2. - g_f);
g_b               NUMBER; -- := g_a * g_f1;
g_etol2           NUMBER; -- := g_tol2/sqrt(abs(g_e2));
END GZ_GEODESIC;
/
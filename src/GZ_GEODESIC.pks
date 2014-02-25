create or replace
PACKAGE GZ_GEODESIC AUTHID CURRENT_USER AS 

-- Translated from Charles Karney's Geographic Lib 1.32
-- http:// geographiclib.sourceforge.net
-- Blocked by the US goverment but comes with excellent documentation,
-- examples and theoretical/practical papers.

g_A3x          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
g_C3x          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
g_C4x          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
g_ord          PLS_INTEGER := 6;   -- Highest order allowed is 8
g_nA1_         PLS_INTEGER := g_ord;
g_nA2_         PLS_INTEGER := g_ord;
g_nA3_         PLS_INTEGER := g_ord;
g_nA3x         PLS_INTEGER := g_ord;
g_nC1_         PLS_INTEGER := g_ord;
g_nC2_         PLS_INTEGER := g_ord;
g_nC3_         PLS_INTEGER := g_ord;
g_nC3x         PLS_INTEGER := TRUNC((g_nC3_ * (g_nC3_ -1))/2);
g_nC4_         PLS_INTEGER := g_ord;
g_nC4x         PLS_INTEGER := TRUNC((g_nC4_ * (g_nC4_ +1))/2);
g_nC1p_        PLS_INTEGER := g_ord;


g_a            NUMBER := 6378137.0;      -- semi-major axis for GRS80
--g_r            NUMBER := 298.257222101;  -- reciprocal flattening for GRS80
g_r            NUMBER := 298.257222100882711243162836607614495;

g_f            NUMBER := 1./g_r;
g_f1           NUMBER := 1. -g_f;
g_e2           NUMBER := g_f*(2. - g_f);
g_ep2          NUMBER := g_e2/(g_f1*g_f1);  --// e2 / (1 - e2)
--g_ep2          NUMBER := g_e2/(1.-g_e2);  --// e2 / (1 - e2)
g_n            NUMBER := g_f/(2. - g_f);
g_b            NUMBER := g_a * g_f1;
    
g_c2           NUMBER;
--           ((Math::sq(_a) + Math::sq(_b) *
--           (_e2 == 0 ? 1 :
--            (_e2 > 0 ? Math::atanh(sqrt(_e2)) : atan(sqrt(-_e2))) /
--            sqrt(abs(_e2))))/2) -- authalic radius squared

            
      -- The sig12 threshold for "really short".  Using the auxiliary sphere
      -- solution with dnm computed at (bet1 + bet2) / 2, the relative error in
      -- the azimuth consistency check is sig12^2 * abs(f) * min(1, 1-f/2) / 2.
      -- (Error measured for 1/100 < b/a < 100 and abs(f) >= 1/1000.  For a
      -- given f and sig12, the max error occurs for lines near the pole.  If
      -- the old rule for computing dnm := (dn1 + dn2)/2 is used, then the error
      -- increases by a factor of 2.)  Setting this equal to epsilon gives
      -- sig12 := etol2.  Here 0.1 is a safety factor (error decreased by 100)
      -- and max(0.001, abs(f)) stops etol2 getting too large in the nearly
      -- spherical WHEN.
--    , _etol2(0.1 * tol2_ /
--             sqrt( max(real(0.001), abs(_f)) * min(real(1), 1 - _f/2) / 2 ))


--// Underflow guard.  We require
--  //   eps2 * epsilon() > 0
--  //   eps2 + epsilon() == epsilon()
g_eps2            NUMBER := 3.16227766016837933199889354443271853372*1.E-64;
g_tiny            NUMBER := 1.E-38;
g_pi              NUMBER := 3.1415926535897932384626433832795028842;
g_to_radians      NUMBER := 0.0174532925199432957692369076848861271344;  -- pi/180
g_to_degrees      NUMBER := 57.29577951308232087679815481410517033235; --180./pi
--SQL> select 1.00000000000000000000000000000000000001 a from dual;  1 + 1.E-38
g_tolb            NUMBER;
g_tol0            NUMBER := 1.E-22;
g_tol1            NUMBER := 100.*g_tol0;
g_tol2            NUMBER; -- := sqrt(g_tol0);
g_xthresh         NUMBER; -- := 1000. * g_tol2;

g_etol2           NUMBER; -- := g_tol2/sqrt(abs(g_e2));

FUNCTION GEO_ANGLE(Geom IN MDSYS.SDO_GEOMETRY,pvertex number default 1) RETURN NUMBER;
FUNCTION GEO_ANGLES(Geom IN MDSYS.SDO_GEOMETRY,Value_index VARCHAR2 default 'TRUE') RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION GEO_AZIMUTH(Geom IN MDSYS.SDO_GEOMETRY,pvertex PLS_INTEGER default 1,pinitial VARCHAR2 default 'TRUE') RETURN NUMBER;
FUNCTION GEO_AZIMUTHS(Geom IN MDSYS.SDO_GEOMETRY,Value_index VARCHAR2 default 'TRUE',pinitial VARCHAR2 default 'TRUE') RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION Geo_Perimeter(Geom IN MDSYS.SDO_GEOMETRY,dec_digits NUMBER default 8) RETURN NUMBER DETERMINISTIC;
FUNCTION Geo_Length(Geom IN MDSYS.SDO_GEOMETRY) RETURN NUMBER DETERMINISTIC;
FUNCTION Geo_Length(XYs IN MDSYS.SDO_ORDINATE_ARRAY,Info IN MDSYS.SDO_ELEM_INFO_ARRAY,pSRID NUMBER default 8265.) RETURN MDSYS.SDO_LIST_TYPE DETERMINISTIC;
FUNCTION Geo_Lengths(Geom IN MDSYS.SDO_GEOMETRY,Value_index VARCHAR2 default 'TRUE') RETURN MDSYS.SDO_LIST_TYPE DETERMINISTIC;
FUNCTION Geo_Length(x1 number,y1 number,x2 number,y2 number,pSRID NUMBER default 8265.) RETURN NUMBER DETERMINISTIC;
FUNCTION GEO_AREA(Geom IN MDSYS.SDO_GEOMETRY,dec_digits NUMBER default 8)RETURN NUMBER DETERMINISTIC;
FUNCTION GEO_AREAS( Geom IN MDSYS.SDO_GEOMETRY,Value_index VARCHAR2 default 'TRUE',dec_digits NUMBER default 8) RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION Ring_AREA(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,pSRID NUMBER DEFAULT 8265.,pstart PLS_INTEGER default 1, pend PLS_INTEGER default 0)
           RETURN NUMBER DETERMINISTIC;          
Procedure A3coeff;
Procedure C3coeff;
Procedure C4coeff;
Function A1m1f(eps Number) Return Number;
Function A2m1f(eps Number) Return Number;
Function A3f(eps Number) return Number;
Procedure C1f(eps Number, c IN OUT NOCOPY Mdsys.Sdo_List_Type);
Procedure C2f(eps Number, c IN OUT NOCOPY Mdsys.Sdo_list_Type);
Procedure C3f(eps number,  c IN OUT NOCOPY Mdsys.sdo_list_type);
Procedure C4f(eps number,  c IN OUT NOCOPY Mdsys.sdo_list_type);
PROCEDURE to_Gnomonic(plat1 NUMBER,plon1 NUMBER,plat NUMBER,plon NUMBER,
                      x IN OUT NOCOPY NUMBER, y IN OUT NOCOPY NUMBER,
                      azi IN OUT NOCOPY NUMBER, rk IN OUT NOCOPY NUMBER );
FUNCTION AngNormalize(x NUMBER) RETURN NUMBER;
FUNCTION ATANH(x NUMBER) RETURN NUMBER;
FUNCTION Astroid(x NUMBER,y NUMBER) RETURN NUMBER;
FUNCTION CBRT(x NUMBER) RETURN NUMBER;
FUNCTION NEW_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER;
FUNCTION InverseStart(sbet1 NUMBER, cbet1 NUMBER,dn1 NUMBER, 
                       sbet2 NUMBER, cbet2 NUMBER, dn2 NUMBER, lam12 NUMBER,
                       salp1 IN OUT NOCOPY NUMBER, calp1 IN OUT NOCOPY NUMBER,
                       salp2 IN OUT NOCOPY NUMBER, calp2 IN OUT NOCOPY NUMBER,
                                    dnm  IN OUT NOCOPY NUMBER,
                                    C1a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, C2a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN NUMBER;
                                    
FUNCTION Lambda12(sbet1 NUMBER, cbet1 NUMBER,  dn1 NUMBER,
                   sbet2 NUMBER, cbet2 NUMBER,  dn2 NUMBER,
                   salp1 NUMBER, calp1 IN OUT NOCOPY NUMBER,
                   salp2 IN OUT NOCOPY NUMBER, calp2 IN OUT NOCOPY NUMBER,
                   sig12 IN OUT NOCOPY NUMBER,
                   ssig1 IN OUT NOCOPY NUMBER, csig1 IN OUT NOCOPY NUMBER,
                   ssig2 IN OUT NOCOPY NUMBER, csig2 IN OUT NOCOPY NUMBER,
                   eps IN OUT NOCOPY NUMBER, domg12 IN OUT NOCOPY NUMBER, 
                   diffp BOOLEAN, dlam12 IN OUT NOCOPY NUMBER,
                   C1a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                   C2a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,  C3a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN NUMBER;

PROCEDURE Lengths(eps NUMBER, sig12 NUMBER, ssig1 NUMBER, csig1 NUMBER, dn1 NUMBER,
                  ssig2 NUMBER, csig2 NUMBER,dn2 NUMBER,cbet1 NUMBER, cbet2 NUMBER,
                  s12b IN OUT NOCOPY NUMBER,m12b IN OUT NOCOPY NUMBER,
                  m0 IN OUT NOCOPY NUMBER,scalep BOOLEAN, M12 IN OUT NOCOPY NUMBER,
                  M21 IN OUT NOCOPY NUMBER,
                  C1a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, C2a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE); 

FUNCTION Inverse(plat1 NUMBER,plon1 NUMBER,
                 plat2 NUMBER,plon2 NUMBER,
                 s12 IN OUT NOCOPY NUMBER, azi1 IN OUT NOCOPY NUMBER,
                 azi2 IN OUT NOCOPY NUMBER,mm12 IN OUT NOCOPY NUMBER,
                 m12 IN OUT NOCOPY NUMBER,m21 IN OUT NOCOPY NUMBER,
                 area IN OUT NOCOPY NUMBER,
                 areap BOOLEAN default FALSE, azimuth BOOLEAN default FALSE,
                 reduced_Length BOOLEAN default FALSE,
                 geodesic_scale  BOOLEAN default FALSE,
                 a NUMBER default 6378137.,
                 F NUMBER default 298.257222101) RETURN NUMBER;
FUNCTION POLYGON_AREA( XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Info MDSYS.SDO_ELEM_INFO_ARRAY, Areas IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,pSRID NUMBER default 8265., Positive BOOLEAN default TRUE) RETURN NUMBER DETERMINISTIC;
procedure test_to_gnomonic;
END GZ_GEODESIC;
/
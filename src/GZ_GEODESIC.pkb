create or replace
PACKAGE BODY GZ_GEODESIC AS
/**
 * Updated: 12/27/2011 to use new_arctan from GZ_MATH instead of c_atan2.
 * Translated: Oct 2010 by Sidey Timmins from Code in C++ written by Charles Karney.
 *             Most problems in translation revolved around the C idiom for setting
 *             variables based upon a test: example
 *             return x >= 180 ? x - 360 : x < -180 ? x + 360 : x;
 *             It is not known if all of these idioms were translated correctly.
 *             Charles provide a complete suite of "perfect" accurate data
 *             which this translation has not been tested against.
 *             However, translation was relatively painless and went much
 *             easier than expected because few lines were retyped.
 * \file hpp
 * Header for GeographicLib::Geodesic class
 *
 * Copyright (c) Charles Karney (2009, 2010) <charles@karney.com>
 * and licensed under the LGPL.  For more information, see
 * http://geographiclib.sourceforge.net/
 **********************************************************************/
  /**
   * %Geodesic calculations
   *
   * The shortest path between two points on a ellipsoid at (lat1, lon1)
   * and (lat2, lon2) is called the   Its length is s12 and
   * the geodesic from point 1 to point 2 has azimuths azi1 and azi2 at
   * the two end points.  (The azimuth is the heading measured clockwise from
   * north.  azi2 is the "forward" azimuth, i.e., the heading that takes you
   * beyond point 2 not back to point 1.)
   *
   * If we fix the first point and increase s12 by ds12, then the second
   * point is displaced ds12 in the direction azi2.  Similarly we
   * increase azi1 by dazi1 (radians), the the second point is displaced
   * m12 dazi1 in the direction azi2 + 90<sup>o</sup>.  The quantity
   * m12 is called the "reduced length" and is symmetric under interchange
   * of the two points.  On a flat surface, he have m12 = s12.  The ratio
   * s12/m12 gives the azimuthal scale for an azimuthal equidistant
   * projection.
   *
   * Given lat1, lon1, azi1, and s12, we can determine lat2,lon2, azi2, m12.
   * This is the direct geodesic problem.  (If s12 is sufficiently large that 
   * the geodesic wraps more than halfway around the earth, there will be 
   * another geodesic between the points with a smaller s12.)
   *
   * Given lat1, lon1, lat2, and lon2, we can determine azi1, azi2, s12, m12.  
   * This is the inverse geodesic problem.  Usually, the solution to the inverse 
   * problem is unique.  In cases where there are multiple solutions (all with 
   * the same s12, of course), all the solutions
   * can be easily generated once a particular solution is provided.
   *
   * As an alternative to using distance to measure s12, the class can also
   * use the arc length a12 (in degrees) on the auxiliary sphere.  This is a
   * mathematical construct used in solving the geodesic problems.  However, an
   * arc length in excess of 180<sup>o</sup> indicates that the geodesic is not
   * a shortest path.  In addition, the arc length between an equatorial
   * crossing and the next extremum of latitude for a geodesic is
   * 90<sup>o</sup>.
   *
   * The calculations are accurate to better than 15 nm.  (See \ref geoderrors
   * for details.)
   *
   * References: "Algorithms for geodesics" Charles F F karney
   *             http://arxiv.org/PS_cache/arxiv/pdf/1109/1109.4448v1.pdf
   * and "Geodesics on an ellipsoid of revolution" Charles F F Karney
   *      http://arxiv.org/PS_cache/arxiv/pdf/1102/1102.1215v1.pdf
   * 
   * For more information on geodesics see: http://geographiclib.sourceforge.net/
   * and many online references for geodesics are here
   * http://geographiclib.sourceforge.net/geodesic-papers/biblio.html
   **********************************************************************/
FUNCTION AngNormalize(x NUMBER) RETURN NUMBER  AS
--  Place angle in [-180, 180).  Assumes x is in [-540, 540).
BEGIN
--       return x >= 180 ? x - 360 : x < -180 ? x + 360 : x;
    If x >= 180. then
       RETURN (x-360.);
    Elsif x < -180. then
       RETURN (x+360.);
    Else
       RETURN x;
    End if;

END AngNormalize;
--
FUNCTION AngRound( x NUMBER) RETURN NUMBER  AS

--    The makes the smallest gap in x = 1/16 - nextafter(1/16, 0) = 1/2^57
--    for reals = 0.7 pm on the earth if x is an angle in degrees.  (This
--    is about 1000 times more resolution than we get with angles around 90
--    degrees.)  We use this to avoid having to deal with near singular
--    cases when x is non-zero but tiny (e.g., 1.0e-200).
      z     NUMBER := 0.0625;  -- 1/16
      delta NUMBER;
      y     NUMBER := abs(x);
BEGIN
-- // The compiler must not "simplify" z - (z - y) to y
--      y = y < z ? z - (z - y) : y;
      
      If y < z then
         delta := z-y;
         y := z - delta;
      End If;
      If x < 0. then
        y := -y;
      End if;
      RETURN y;
      
END AngRound;
--
FUNCTION Astroid(x NUMBER,y NUMBER) RETURN NUMBER AS

--   Solve k^4+2*k^3-(x^2+y^2-1)*k^2-2*y^2*k-y^2 = 0 for positive root k.
--   This solution is adapted from Geocentric::Reverse.

-- Example: To make the arithmetic understandable take a solution where k is about 2:
--      16 + 16 -(1+1.8856180831641275^2 -1) *4 -2*2*1.8856180831641275 -1.8856180831641275^2=0
--
--SQL> select astroid(1.,1.8856180831641275) from dual;
--
--ASTROID(1.,1.8856180831641275)
-----------------------
--             2.0000000000000007521  = k <<<<< which is about 2
    k   NUMBER;
    p   NUMBER;
    q   NUMBER;
    r   NUMBER;
    u   NUMBER;
    v   NUMBER;
    uv  NUMBER;
    w   NUMBER;
    disc NUMBER;
    s   NUMBER;
    r2  NUMBER;
    r3  NUMBER;
    T3  NUMBER;
    T   NUMBER;
    ang NUMBER;
    
BEGIN
      p := x *x;
      q := y * y;
      r := (p + q - 1.) / 6.;
    if ( NOT(q = 0. and r <= 0.) ) then

--     Avoid possible division by zero when r = 0 by multiplying equations
--     for s and t by r^3 and r, resp.
        S := p * q *0.25;            -- S = r^3 * s
        r2 := r*r;
        r3 := r * r2;
--      The discriminant of the quadratic equation for T3.  This is zero on
--      the evolute curve p^(1/3)+q^(1/3) = 1
        disc :=  S * (S + 2. * r3);
        u := r;
        if (disc >= 0.0) then
          T3 := S + r3;
--        Pick the sign on the sqrt to maximize abs(T3).  This minimizes loss
--        of precision due to cancellation.  The result is unchanged because
--        of the way the T is used in definition of u.
          if T3 < 0.0 then
            t3 := t3 -sqrt(disc);
          else
            t3 := t3 +sqrt(disc);  --  T3 = (r * t)^3
          end if;

--        N.B. cbrt (cube root) always returns the real root.  cbrt(-8) = -2.
          T := cbrt(T3); -- T = r * t
--        T can be zero; but then r2 / T -> 0.
          if T <> 0.0 then
            u := u + T + r2/T;
          else
            u := u + T;
          end if;

        else 
--        T is complex, but the way u is defined the result is real.
          ang := new_arctan(sqrt(-disc), -(S + r3));
--        There are three possible cube roots.  We choose the root which
--        avoids cancellation.  Note that disc < 0 implies that r < 0.
          u := u + 2. * r * cos(ang / 3.);
        end if;

        v := sqrt(u*u + q);    -- guaranteed positive
--      Avoid loss of accuracy when u < 0.
        if u < 0.0 then
          uv := q/(v-u);
        else
          uv := u + v;   --guaranteed positive
        end if;
 
        w := (uv - q) / (2. * v);           -- positive?
--      Rearrange expression for k to avoid loss of accuracy due to
--      subtraction.  Division by 0 not possible because uv > 0, w >= 0.
        k := uv / (sqrt(uv + w*w) + w);   -- guaranteed positive
    else                --// q == 0 && r <= 0
--      y = 0 with |x| <= 1.  Handle this case directly.
--      for y small, positive root is k = abs(y)/sqrt(1-x^2)
      k := 0.;
    end if;
    return k;

END Astroid;
--
FUNCTION A1M1F(eps NUMBER) RETURN NUMBER  AS 
--    These are Maxima generated functions to provide series approximations to
--    the integrals for the ellipsoidal 
--    static real A1m1f(real eps) throw();
--    static void C1f(real eps, real c[]) throw();
--    static void C1pf(real eps, real c[]) throw();
--    static real A2m1f(real eps) throw();
--    static void C2f(real eps, real c[]) throw();
--    static real A3f(real f, real eps) throw();
--    static void C3f(real f, real eps, real c[]) throw();

--   The scale factor A1-1 = mean value of I1-1
   eps2  NUMBER := eps*eps;
   t     NUMBER;
   a1    NUMBER;
BEGIN

     t := eps2*(eps2*(eps2*(25.*eps2+64.)+256.)+4096.)/16384.;
     a1 := (t + eps) / (1. - eps);
  RETURN a1;
  
END A1M1F;
--
FUNCTION A2M1F(eps NUMBER) RETURN NUMBER  AS 
--   The scale factor A2-1 = mean value of I2-1
  t        NUMBER;
  eps2     NUMBER := eps*eps;
  a2       NUMBER;
BEGIN

    t := eps2*(eps2*(eps2*(1225.*eps2+1600.)+2304.)+4096.)/16384.;
    a2 := t * (1. - eps) - eps;
    RETURN a2;
    
END A2M1F;
--
FUNCTION A3F(f NUMBER,eps NUMBER) RETURN NUMBER AS
--  The scale factor A3 = mean value of I3
   del    NUMBER; -- := (f - eps) / (1. - eps);
   nu2    NUMBER := 1.;
--  Correct limit for del -> 0 is nu2 = mu/2 / (1 - mu/2).  However,
--  it doesn't matter because the correction vanishes in this limit.

   g      NUMBER;
   a3     NUMBER;
BEGIN
     del := (f - eps) / (1. - eps);
      if del <> 0. then --      nu2 = sq( del != 0 ? eps / del : 1 );
         nu2 := (eps/del);
         nu2 := nu2 * nu2;
      else
         nu2 := 1.;
      end if;

    g := (del*(del*(del*(del*(nu2*(nu2*(15.*nu2+14.)+24.)*del+nu2*((32.-16.*nu2)*
          nu2+32.))+32.*nu2)-32.*nu2*nu2)-128.*nu2)*del*del+2048.)/2048.;

    a3 := (2. - f)/(2. - del) * g;    
    RETURN a3;
  
END A3F;
--
FUNCTION new_A3F(f NUMBER,eps NUMBER) RETURN NUMBER AS
--   Evaluation sum(_A3c[k] * eps^k, k, 0, nA3x-1) by Horner's method
    v     NUMBER := 0;
    
BEGIN
    for i in reverse 1..g_A3x.count loop 
      v := eps * v + g_A3x(i);   -- this had _A3x[--i] but C is zero based
    end loop;
    RETURN v;
END;
--
PROCEDURE A3COEFF(A3x IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) AS 
-- The coefficients C1(l) in the Fourier expansion of B1
 
BEGIN
      A3x(1) := 1.;
      A3x(2) := (g_n-1.)/2.;
      A3x(3) := (g_n*(3.*g_n-1.)-2.)/8.;
      A3x(4) := (g_n*(g_n*(5.*g_n-1.)-3.)-1.)/16.;
      A3x(5) := (g_n*((-5.*g_n-20.)*g_n-4.)-6.)/128.;
      A3x(6) := ((-5.*g_n-10.)*g_n-6.)/256.;
      A3x(7) := (-15.*g_n-20.)/1024.;
      A3x(8) := -25./2048.;

END A3COEFF;
--
FUNCTION ASINH(x NUMBER) RETURN NUMBER  AS
--    The inverse hyperbolic sine function.  This is defined in terms of
--    Math::log1p(x) in order to maintain accuracy near x = 0.  In
--    addition, the odd parity of the function is enforced.
   y   NUMBER := abs(x);  --  Enforce odd parity
   
BEGIN

   y := log1p(y * (1. + y/(hypot(1., y) + 1.)));
   if x < 0. then
     y := -y;
   end if;
   RETURN y;
   
END ASINH;
--
FUNCTION ATANH(x NUMBER) RETURN NUMBER  AS
--     The inverse hyperbolic tangent function.  This is defined in terms of
--     Math::log1p(x) in order to maintain accuracy near x = 0.  In
--     addition, the odd parity of the function is enforced.

   y   NUMBER := abs(x);  --  Enforce odd parity
   
BEGIN

--  Not defined for 1 or -1
   if y <> 1. then
      y := log1p(2. * y/(1. - y))*0.5;
   else
      dbms_output.put_line('ATANH called with argument of 1. <<<');
   end if;
   if x < 0. then
     y := -y;
   end if;
   RETURN y;
   
END ATANH;
--
FUNCTION NEW_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER DETERMINISTIC AS 

/*
--*************** COPIED FROM GZ_MATH !!! **************************************
--Program Name: new_arctan
--Author: Sidey Timmins
--Creation Date: 10/10/2011
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has two parameters:
  --
  --   REQUIRED Parameters: 
  --            Y:  Y ordinate as per convention Y first!
  --            X:  X abscissa
  --
--Purpose:   -- Calculates the arctan function -  atan2(y,x) 
--              as fast as possible (10 times faster than the built in 
--              10G function atan2 which takes 25 seconds for 200000 values) with 
--              an accuracy of 1E-38. It surpasses the accuracy of the Oracle
--              library function.
--              The domain is unlimited. (Note this function gives no error
--              and returns zero when both x and y are zero.)
--
-- Method:      First the range is reduced and then the arctangent is computed 
--              using the ratio of 2 polynomials derived from Legendre polynomials.
--              The result is then adjusted for the quadrant.

-- Accuracy:  The maximum error in the range - -infinity to infinity is < 1.E-38
--                             
-- Reference: Ulrich H Kurzweg and Sidey P. Timmins "A New Method for obtaining
--            Highly Accurate Approximations to the Arctangent Function". 
--            submitted to IEEE Transactions on Computers, Oct 12, 2011
--             http://www.mae.ufl.edu/~uhk/MORE-EVALUATION-ARCTAN.pdf
--Dependencies: None but see try_faster_atan2 for rigorous testing.
--******************************************************************************
*/
 
   pi        CONSTANT NUMBER  := 3.1415926535897932384626433832795028842;
   piByTwo   CONSTANT NUMBER  := 1.5707963267948966192313216916397514421;
   piBy6     CONSTANT NUMBER  := 0.5235987755982988730771072305465838140;
   tanpiby6  CONSTANT NUMBER  := 0.5773502691896257645091487805019574556476;
   rad2deg   CONSTANT NUMBER  := 57.29577951308232087679815481410517033240;


   x           NUMBER; 
   result     NUMBER;
   complement  BOOLEAN           := FALSE;
   region      NUMBER           := 0.0;

   u           NUMBER;
   
-- Function to perform multipoint range reduction (see reference)
-- Like a ladder, we place our foot or argument in the right slot.

   Function Reduce( a number, step number) Return number as
     aa number := a;
   Begin
     aa := a-step;
        if aa <> 0.0 then
          aa :=  (1.0 +a*step)/aa; 
        end if;
    Return aa;
   End;

--
BEGIN

/* arctan2(Y,X) is the quadrant-correct arc tangent atan(Y/X).  If the 
   denominator X is zero, then the numerator Y must not be zero.  All
   arguments are legal including the bad: Y = X = 0.  returns zero */
 
-- check for error conditions 
  IF XIn = 0.0 THEN
    IF YIn = 0.0 THEN 
      RETURN  0.0;    -- We return zero although arctan(0,0) is undefined
    ELSIF YIn < 0.0 THEN 
      if degrees then
        RETURN 270.0;
      else
        RETURN -pibytwo;
      end if;
    ELSE 
      if degrees then
        RETURN 90.;
      else
      RETURN pibytwo;
      end if;
    END IF;
  END IF;
 

  IF xin = 0.0 or yin = 0.0 then
    x := 0.;
  ELSIF ABS(YIn) > ABS(XIn) THEN
     x   := ABS(XIn)/ABS(YIn);     
  ELSE
     x   := ABS(YIn/XIN);
  END IF; 
  
  -- reduce arg to under tan(pi/12)
--dbms_output.put_line('x ' || x  || ' x was ' ||  round(1./x,8));
  u := x; 
  if x > 1. then
        x := (x-tanpiby6)/(1.0 +tanpiby6*x); 
       region := piby6;
       if x > 1. then
        x := (x-tanpiby6)/(1.0 +tanpiby6*x); 
       region := region + piby6;
--       dbms_output.put_line('NNNnnow x ' || x);
       end if;
   end if;
-- dbms_output.put_line('NNNnnnow x ' || x);


-- These steps are described in the reference, Kurzweg and Timmins.
-- They maintain p the argument to the F function > 50.
    

if x >= 0.6825 then
      IF x >=0.8 THEN -- [0.8, 1]
        if x >=0.93 then
        x := Reduce(x,0.965);   
        region := region+ 0.76758834183269989405469721325594408446591;
        elsif x >=0.8625 then
        x := Reduce(x,0.895);     
        region := region+ 0.7300457928869180218270609795909734179136;
        else
        x := Reduce(x,0.83); 
        region := region+ 0.69276783539712221066245282985001349200858;
        end if;
      ELSE            -- [.6825,0.8]
        if x>= 0.74 then
        x := Reduce(x,0.77);           
        region := region+ 0.6561787179913948753839202592287673132869;
        else      
        x := Reduce(x,0.71);      
        region := region+ 0.617405891751572666519713944848509584856;
        end if;
      END IF;
elsif x >= 0.48 then     
      IF x > 0.575 THEN  -- [.575,.6825]
        if x > 0.6275 then
        x := Reduce(x,0.655);
        region := region+ 0.5798821323938133197612842815963766066022798; 
        else
        x := Reduce(x,0.6);
        region := region+ 0.5404195002705841554435783646085999101395;
        end if;
      ELSE              -- [.48,.575]
        if x > 0.525 then
        x := Reduce(x,0.55);     
        region := region+ 0.5028432109278608273308820292452775557764;
        else
        x := Reduce(x,0.5);
        region := region+ 0.4636476090008061162142562314612144020295;
        end if;
      END IF;
  elsif x >= 0.32 then    
      IF x > 0.4 THEN  -- [.4,.48]
        if x > 0.44 then
        x := Reduce(x,0.46);
        region := region+ 0.4311387407187821833927318363979782211726;
        else
        x := Reduce(x,0.42);
        region := region+ 0.397627991522129313999543868880928300641;
        end if;
      ELSE              -- [.32,.4] 
 
        if x > 0.36 then
        x := Reduce(x,0.38);
        region := region+ 0.3631470099461762897239450193942226137986;
        else
        x := Reduce(x,0.34);
        region := region+ 0.3277385067805554459968354061588850531541;
        end if;
        
       END IF;
       
  elsif x >= 0.16 then
   
     IF x > 0.24 THEN  -- [.24,.32]
        if x > 0.28 then
        x := Reduce(x,0.3);
        region := region+ 0.2914567944778670919956046214328911935031;
        else
        x := Reduce(x,0.26);
        region := region+ 0.2543680585532659314288571233246097128713;
        end if;
        
     ELSE               -- [.16,.24]
        if x > 0.2 then
        x := Reduce(x,0.22);
        region := region+ 0.2165503049760892764819793149864512840659; 
        else
        x := Reduce(x,0.18);
        region := region+ 0.178092938231197549667920299591228165849;
        end if;
     END IF; 
     
  else 
     IF x>= 0.08 THEN   -- [.08,16]
        if x >= 0.12 then
        x := Reduce(x,0.14);
        region := region+ 0.1390959414820713242965428338727458027604;         
        else
        x := Reduce(x,0.1);  
        region := region+ 0.099668652491162027378446119878020590243278;        
        end if;
        
     ELSE               -- [0.01,.08]
        if x>= 0.04 then
        x := Reduce(x,0.06);
        region := region+ 0.059928155121207884431815313861521409431737;         
        elsif x> 0.01 then
        x := Reduce(x,0.02);
        region := region+ 0.019997333973150533060753196901596487949233;
          
        elsif x <> 0.0 then
              x := 1./x;    
        end if;
     END IF;
  end if;
 
--   dbms_output.put_line('x now is ' || x || ' reg ' || region);

    if yin <> 0.0 and ((ABS(YIn) >= ABS(XIn) and u <= 1.) or (ABS(YIn) < ABS(XIn) and u > 1.)) then
     complement := TRUE;
     end if;


     if x > 1.E15 then
      result := 1./x + region;
    else

--     dbms_output.put_line('x ' || round(x,8) || ' yin ' || yin);
      u := x*x;
--      result := x*(55. + 105.*u)/(9.+u*(90. + 105*u)) + region;
--      result := x*(231. + u*(1190.+1155.*u))/ (25.+ u*(525.+ u*(1575. + 1155.*u))) + region;
--        result := x*(3031.8 + u*(29491.+u*(69069.+45045*u)))/ (245.+ u*(8820.+ u*(48510. + u*(84084.+u*45045.))))
--        result := x*1./35.*(15159. + u*(147455.+u*(345345.+225225.*u)))/ (35.+ u*(1260.+ u*(6930. + u*(12012.+u*6435.))))
--                 + region;
--        result := x*(61567. + u*(962676. + u*(3960356.4 + u*(5921916. + u*2909907.))))/
--        (3969. + u*(218295. + u*(1891890. + u*(5675670. + u*(6891885. + u*2909907))))) + region;
         result := 11.*x*(27985.+ u*(437580. + u*(1800162. + u*(2691780. + u*1322685.))))/
            (315.*(63.+u*(3465.+u*(30030.+u*(90090.+u*(109395.+46189.*u)))))) + region;
      end if;
--      dbms_output.put_line('res ' || result || ' region ' || round(region,10));
  IF complement = TRUE THEN
     result := pibyTwo -result;
  END IF;
  
  IF XIn < 0.0 THEN 
      result := pi-result;
  END IF;
  
  
  IF YIn < 0.0 THEN 
     result := -result;
  END IF;
  If degrees Then
     result := result *rad2deg;
     if result < 0.0 then
       result := result + 360.;
     end if;
  End if;

  RETURN result;

END NEW_ARCTAN;
--     
FUNCTION C_ATAN2(YIn NUMBER, XIn NUMBER) RETURN NUMBER
 Deterministic IS
/*
**************************************************************************************
--Program Name: c_atan2
--Author: Sidey Timmins
--Creation Date: 11/16/2006
--Updated: 10/18/2010  Sidey Timmins to use Newton method for more accuracy
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has two parameters:
  --
  --   REQUIRED Parameters: 
  --            Y:  Y ordinate as per convention Y first!
  --            X:  X abscissa
  --
--Purpose:   -- Calculates the arctan function -  atan2(y,x) 
--              as fast as possible (4 times faster than the built in 
--              function atan2 which takes 4 seconds for 20000 values) with 
--              an accuracy of 23 to 24 decimal digits.
--              The domain is unlimited. (Note this function gives no error
--              and returns zero when both x and y are zero.)
--
-- Method:      where x = ABS(Yin/XIn) the raw atan is:
--         atan(x)= x(c1 + c2*x**2 + c3*x**4)/(c4 + c5*x**2 + c6*x**4 + x**6)
-- or using Medina's polynomial
--  h3(x) = x ¿ x^3/3+ x^5/5¿ x^7/7 + x^9/9¿ x^11/11 
--           + 5x^13/64 ¿ 3x^14/224 + x^15/960¿13x^16/64+ 247x^17/544 ¿73 x^18/144
--           + 215 x^19/608 ¿ 13x^20/80+ 65x^21/1344 ¿ 3x^22/352 + x^23/1472
--         and then is improved with Newton's method and djusted for the quadrant.

-- Accuracy:  The maximum error in the range - two pi to two pi is less than
--            5E-24. The relative errors is < 1.E-22, that is 
--   
--                      c_atan2/arctan > (1. - 1.E-22) and < (1. + 1.E-22)

-- Reference: http://www.gansle.com/approx/approx.pdf
--            "A Guide to Approximations" by Jack G Ganssle
--            http://myweb.lmu.edu/hmedina/Papers/Arctan.pdf
--            "A Sequence of Polynomials for Approximating Arctangent" by Herbert A Medina
--            Newtons method improvement:
--            http://www.math.niu.edu/~rusin/known-math/99/arctan
--Dependencies: None but see c_test_atan2 for rigorous testing.
***************************************************************************************
*/
 
  fourbypi  NUMBER             := 1.27323954473516268615107010698011489627;
  pi        CONSTANT NUMBER    := 3.1415926535897932384626433832795028842;
  piByTwo   NUMBER             := 1.5707963267948966192313216916397514421;
  piBy6     NUMBER             := 0.5235987755982988730771072305465838140;
  piBy12    NUMBER             := 0.2617993877991494365385536152732919070167;
  tanpiby6  NUMBER             := 0.5773502691896257645091487805019574557;
  tanpiby12 NUMBER             := 0.2679491924311227064725536584941276331;
 
  c1        NUMBER             := 48.70107004404898384;
  c2        NUMBER             := 49.5326263772254345;
  c3        NUMBER             := 9.40604244231624;
  c4        NUMBER             := 48.70107004404996166;
  c5        NUMBER             := 65.7663163908956299;
  c6        NUMBER             := 21.587934067020262;
  x         NUMBER;
 
  result    NUMBER;
  
  a3        NUMBER  := 0.3333333333333333333333333333333333333333;
  a5        NUMBER  := 0.2;
  a7        NUMBER  := 0.1428571428571428571428571428571428571429;
  a9        NUMBER  := 0.1041666666666666666666666666666666666667;
  a10       NUMBER  := 0.05;
  a11       NUMBER  := 0.2443181818181818181818181818181818181818;
  a12       NUMBER  := 0.25;
  a13       NUMBER  := 0.12980769230769230769230769230769230769230;
  a14       NUMBER  := 0.0357142857142857142857142857142857142857;
  a15       NUMBER  := 0.004166666666666666666666666666666666666667;
  /*
  m3        NUMBER  := 0.3333333333333333333333333333333333333333;
  m5        NUMBER  := 0.2;
  m7        NUMBER  := 0.1428571428571428571428571428571428571429;
  m9        NUMBER  := 0.1111111111111111111111111111111111111111;
  m11       NUMBER  := 0.0909090909090909090909090909090909090909;
  m13       NUMBER  := 0.078125;
  m14       NUMBER  := 0.0133928571428571428571428571428571428571;
  m15       NUMBER  := 0.001041666666666666666666666666666666666667;
  m16       NUMBER  := 0.203125;
  m17       NUMBER  := 0.4540441176470588235294117647058823529412;
  m18       NUMBER  := 0.5069444444444444444444444444444444444444;
  m19       NUMBER  := 0.3536184210526315789473684210526315789474;
  m20       NUMBER  := 0.1625;
  m21       NUMBER  := 0.0483630952380952380952380952380952380952;
  m22       NUMBER  := 0.008522727272727272727272727272727272727273;
  m23       NUMBER  := 0.000679347826086956521739130434782608695652;
 */ 
  t1        NUMBER  := 4130240.588996024013440146267;
  t2        NUMBER  := -349781.8562517381616631012487;
  t3        NUMBER  := 6170.317758142494245331944348;
  t4        NUMBER  := -27.94920941380194872760036319;
  t5        NUMBER  := 0.0175143807040383602666563058;
  t6        NUMBER  := 5258785.647179987798541780825;
  t7        NUMBER  := -1526650.549072940686776259893;
  t8        NUMBER  := 54962.51616062905361152230566;
  t9        NUMBER  := -497.495460280917265024506937;
  
  s1        NUMBER :=1.6867629106;
  s2        NUMBER :=0.4378497304;
  s3        NUMBER :=1.6867633134;
  x2        NUMBER;
  x3        NUMBER;
  x4        NUMBER;
  x5        NUMBER;
  x7        NUMBER;
  x9        NUMBER;
  x11       NUMBER;
  r         NUMBER;
  r2        NUMBER;
  tanr      NUMBER;

  complement BOOLEAN           := FALSE;
  region     PLS_INTEGER       := 0;

  
BEGIN

/* arctan2(Y,X) is the quadrant-correct arc tangent atan(Y/X).  If the 
   denominator X is zero, then the numerator Y must not be zero.  All
   arguments are legal except Y = X = 0. */
 
-- check for error conditions 
  IF XIn = 0.0 THEN
    IF YIn = 0.0 THEN 
      RETURN  0.0;    -- We return zero although arctan(0,0) is undefined
    ELSIF YIn < 0.0 THEN 
      RETURN -pibytwo;
    ELSE 
      RETURN pibytwo;
    END IF;
  END IF;
  

  IF ABS(YIn) > ABS(XIn) THEN
     x   := ABS(XIn)/ABS(YIn);
     complement := TRUE;
  ELSE
     x   := ABS(YIn/XIN);
  END IF;

--dbms_output.put_line('x ' || x);
-- reduce arg to under tan(pi/12)

  if (x > tanpiby12) THEN
     x := (x-tanpiby6)/(1.0 +tanpiby6*x); 
     region := 1;
  end if;

  x2 := x * x;
--  dbms_output.put_line('X ' || x);
--  if x >-0.1 and x <= 0.15 then
    x3 := x * x2;
    x5 := x3 * x2;
    x4 := x2 * x2;
    x7 := x5 * x2;
    x9 := x5 *x4;
    x11 := x9 * x2;
-- Use improved Taylor Series to evaluate 
--    result := x- x3*(a3 - x2*a5 + x4*a7) + x9*(a9 + x*a10 -x2*a11+ x3*a12-x4*a13 +x5*a14 - x3*x3*a15);
--    result := x - x3/3. + x5/5. - x7/7. + x9/9.+ x9*(5.*x4/64. -x2/11. -3.*x5/224. + x4*x2/960. - 
--              13.*x7/64. +247.*x7*x/544.- 73.*x9/144. + 215.*x*x9/608. -13.*x11/80. + 65.*x11*x/1344. -3.*x2*x11/352. + x3*x11/1472.);
-- With the Newton method below, using this is moot instead of the shorter
-- method from Ganssle: result := (x*(c1 + x2*(c2 + x2*c3))/(c4 + x2*(c5 + x2*(c6 + x2))));
-- This was the old way before
--   result := x - x3*m3 + x5*m5 - x7*m7 + x9*m9+ x9*(x4*m13 -x2*m11 -x5*m14 + x4*x2*m15 - 
--              x7*m16 +x7*x*m17- x9*m18 + x*x9*m19  -x11*m20 + x11*x*m21 -x2*x11*m22 + x3*x11*m23);

--  else
    
-- Use Horner's rule to evaluate 
   result := (x*(c1 + x2*(c2 + x2*c3))/(c4 + x2*(c5 + x2*(c6 + x2))));

-- even this gives phenomenal results
--   result := (x*(s1 + x2*s2)/(s3 + x2));
--  end if;

-- Now calculate the tangent for an improved atan2 using Newton's method 
-- applied to the equation  sin(t) - a * cos(t) = 0.
-- 1) got a rough estimate t of the solution of tan(t) ~= a 

-- 2) then use an improvement formula
-- Calculate r = tan(t) - a
-- improve   t_new = t - r / (1 + a * (a + r))

  r := result;
  r := r * fourbypi;
  r2 := r*r;
-- high precision tangent code is from Ganssle 
  tanr := r*(t1 + r2*(t2 + r2*(t3 + r2*(t4 + r2*t5))))/(t6 + r2*(t7 + r2*(t8 + r2*(t9 + r2))));

  r := tanr - x;
  result := result - r/(1.+x*(x+r));
  
  IF region = 1 THEN
     result  := result + piby6;
  END IF;
  
  IF complement = TRUE THEN
     result := pibyTwo -result;
  END IF;
  
  IF XIn < 0.0 THEN 
      result := pi-result;
  END IF;
  
  IF YIn < 0.0 THEN 
      RETURN -result;
  ELSE 
      RETURN result;
  END IF;
  

END;
--
FUNCTION CBRT(x NUMBER) RETURN NUMBER  AS
-- Cube root function
  y         NUMBER := abs(x);
  onethird  NUMBER := 0.3333333333333333333333333333333333333333;
BEGIN
   y := Power(y,onethird);
   if x < 0. then
     y := -y;
   end if;
   RETURN y;
   
END CBRT;
--
PROCEDURE C1F(eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) AS 
-- The coefficients C1(l) in the Fourier expansion of B1
 
  eps2    NUMBER := eps*eps;
  d       NUMBER := eps;
BEGIN

      c(1) := d*(eps2*(eps2*(19.*eps2-64.)+384.)-1024.)/2048.;
      d := d*eps;
      c(2) := d*(eps2*(eps2*(7.*eps2-18.)+128.)-256.)/4096.;
      d := d*eps;
      c(3) := d*((72.-9.*eps2)*eps2-128.)/6144.;
      d := d*eps;
      c(4) := d*((96.-11.*eps2)*eps2-160.)/16384.;
      d := d*eps;
      c(5) := d*(35.*eps2-56.)/10240.;
      d := d*eps;
      c(6) := d*(9.*eps2-14.)/4096.;
      d := d*eps;
      c(7) := -33.*d/14336.;
      d := d*eps;
      c(8) := -429.*d/262144.;

END C1F;
--
PROCEDURE C12F(eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,C2 IN OUT NOCOPY MDSYS.SDO_LIST_TYPE)  AS

-- The coefficients C1(l) in the Fourier expansion of B1 for g_ord = 6
-- and the coefficients of C2
  eps2    NUMBER := eps*eps;
  d       NUMBER := eps;
BEGIN
      c(1) := d*((6.-eps2)*eps2-16.)*0.03125;
      c2(1) := d*(eps2*(eps2+2.)+16.)*0.03125;
      d := d*eps;
      c(2) := d*((64.-9.*eps2)*eps2-128.)*0.00048828125;
      c2(2) := d*(eps2*(35.*eps2+64.)+384.)*0.00048828125;
      d := d*eps;
      c(3) := d*(9.*eps2-16.)*0.001302083333333333333333333333333333333333;
      c2(3) := d*(15.*eps2+80.)*0.001302083333333333333333333333333333333333;
      d := d*eps;
      c(4) := d*(3.*eps2-5.)*0.001953125;
      c2(4) := d*(7.*eps2+35.)*0.001953125;
      d := d*eps;
      c(5) := -7.*d/1280.;
      c2(5) := 63.*d/1280.;
      d := d*eps;
      c(6) := -7.*d*0.00048828125;
      c2(6) := 77.*d*0.00048828125;
END C12F;
--      
PROCEDURE C12F8(eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,C2 IN OUT NOCOPY MDSYS.SDO_LIST_TYPE)  AS

-- The coefficients C1(l) in the Fourier expansion of B1 for g_ord=8
 
  eps2    NUMBER := eps*eps;
  d       NUMBER := eps;
BEGIN

      c(1) := d*(eps2*(eps2*(19.*eps2-64.)+384.)-1024.)*0.00048828125;
      c2(1) := d*(eps2*(eps2*(41.*eps2+64.)+128.)+1024.)*0.00048828125;
      d := d*eps;
      c(2) := d*(eps2*(eps2*(7.*eps2-18.)+128.)-256.)*0.000244140625;
      c2(2) := d*(eps2*(eps2*(47.*eps2+70.)+128.)+768.)*0.000244140625;
      d := d*eps;
      c(3) := d*((72.-9.*eps2)*eps2-128.)*0.000162760416666666666666666666666666666667;
      c2(3) := d*(eps2*(69.*eps2+120.)+640.)*0.000162760416666666666666666666666666666667;
      d := d*eps;
      c(4) := d*((96.-11.*eps2)*eps2-160.)*0.00006103515625;
      c2(4) := d*(eps2*(133.*eps2+224.)+1120.)*0.00006103515625;
      d := d*eps;
      c(5) := d*(35.*eps2-56.)*0.00009765625;
      c2(5) := d*(105.*eps2+504.)*0.00009765625;
      d := d*eps;
      c(6) := d*(9.*eps2-14.)*0.000244140625;
      c2(6) := d*(33.*eps2+154.)*0.000244140625;
      d := d*eps;
      c(7) := -33.*d*0.00006975446428571428571428571428571428571429;
      c2(7) := 429.*d*0.00006975446428571428571428571428571428571429;
      d := d*eps;
      c(8) := -429.*d*0.000003814697265625;
      c2(8) := 6435.*d*0.000003814697265625;

END C12F8;
--
--
PROCEDURE C1PF(eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) AS 
--   The coefficients C1p(l) in the Fourier expansion of B1p
-- NOT CALLED  <<<<<<<<<<<<<<<<<<<<<<<<
    eps2   NUMBER := eps*eps;
    d      NUMBER := eps;
BEGIN
      c.extend(8);
      c(1) := d*(eps2*((9840.-4879.*eps2)*eps2-20736.)+36864.)/73728.;
      d := d*eps;
      c(2) := d*(eps2*((120150.-86171.*eps2)*eps2-142080.)+115200.)/368640.;
      d := d*eps;
      c(3) := d*(eps2*(8703.*eps2-7200.)+3712.)/12288.;
      d := d*eps;
      c(4) := d*(eps2*(1082857.*eps2-688608.)+258720.)/737280.;
      d := d*eps;
      c(5) := d*(41604.-141115.*eps2)/92160.;
      d := d*eps;
      c(6) := d*(533134.-2200311.*eps2)/860160.;
      d := d*eps;
      c(7) := 459485.*d/516096.;
      d := d*eps;
      c(8) := 109167851.*d/82575360.;

END C1PF;
--
PROCEDURE C2F(eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE)  AS
 
   eps2 NUMBER := eps*eps;
   d    NUMBER := eps;
BEGIN

      c(1) := d*(eps2*(eps2*(41.*eps2+64.)+128.)+1024.)/2048.;
      d := d*eps;
      c(2) := d*(eps2*(eps2*(47.*eps2+70.)+128.)+768.)/4096.;
      d := d*eps;
      c(3) := d*(eps2*(69.*eps2+120.)+640.)/6144.;
      d := d*eps;
      c(4) := d*(eps2*(133.*eps2+224.)+1120.)/16384.;
      d := d*eps;
      c(5) := d*(105.*eps2+504.)/10240.;
      d := d*eps;
      c(6) := d*(33.*eps2+154.)/4096.;
      d := d*eps;
      c(7) := 429.*d/14336.;
      d := d*eps;
      c(8) := 6435.*d/262144.;

END C2F;
--
PROCEDURE C3F(f NUMBER,eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE)AS

--  The coefficients C3[l] in the Fourier expansion of B3 for g_ord=6
   
   del    NUMBER; -- := (f - eps) / (1. - eps);
   nu2    NUMBER := 1.;
--  -Correct limit for del -> 0 is nu2 = mu/2 / (1 - mu/2).  However,
-- it doesn't matter because the correction vanishes in this limit.

   d    NUMBER := eps;
BEGIN
      del := (f - eps) / (1. - eps);
      if del <> 0. then --      nu2 = sq( del != 0 ? eps / del : 1 );
         nu2 := (eps/del);
         nu2 := nu2 * nu2;
      end if;

      c(1) := d*(del*(del*(del*(((1.-nu2)*nu2-2.)*del-nu2-4.)-2.*nu2-8.)-16.)+32.)*0.0078125;
      d := d*eps;
      c(2) := d*(del*((-del-2.*nu2-4.)*del-12.)+16.)*0.00390625;
      d := d*eps;
      c(3) := d*(del*((-7.*nu2-8.)*del-36.)+40.)*0.000651041666666666666666666666666666666667;
      d := d*eps;
      c(4) := d*(7.-7.*del)*0.001953125;
      d := d*eps;
      c(5) := 21.*d*0.000390625;
END C3f;
--
PROCEDURE C3F7(f NUMBER,eps NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE)AS

--  The coefficients C3[l] in the Fourier expansion of B3 for g_ord=8
   
   del    NUMBER; -- := (f - eps) / (1. - eps);
   nu2    NUMBER := 1.;
--Correct limit for del -> 0 is nu2 = mu/2 / (1 - mu/2).  However,
-- it doesn't matter because the correction vanishes in this limit.

   d    NUMBER := eps;
BEGIN
      del := (f - eps) / (1. - eps);
      if del <> 0. then  --      nu2 = sq( del != 0 ? eps / del : 1 );
         nu2 := (eps/del);
         nu2 := nu2 * nu2;
      end if;

      c(1) := d*(del*(del*(del*(del*(del*((nu2*((80.-61.*nu2)*nu2+80.)-64.)*del+
          nu2*(48.*nu2+128.)-128.)+(128.-128.*nu2)*nu2-256.)-128*nu2-512.)-256.*nu2-
          1024.)-2048.)+4096.)/16384.;
      d := d*eps;
      c(2) := d*(del*(del*(del*(del*((nu2*(37.*nu2+48.)+16.)*del+(80.-56.*nu2)*nu2)-
          64.)-128.*nu2-256.)-768.)+1024.)/16384.;
      d := d*eps;
      c(3) := d*(del*(del*(del*(((140.-91.*nu2)*nu2+48.)*del+64.*nu2)-224.*nu2-256.)-
          1152.)+1280.)/49152.;
      d := d*eps;
      c(4) := d*(del*(del*((23.*nu2+10.)*del-48.*nu2-32.)-224.)+224.)/16384.;
      d := d*eps;
      c(5) := d*(del*((-165.*nu2-60.)*del-720.)+672.)/81920.;
      d := d*eps;
      c(6) := d*(88.-99.*del)/16384.;
      d := d*eps;
      c(7) := 429.*d/114688.;

END C3F7;
--
PROCEDURE C4F(k2 NUMBER,C IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, C4x IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) AS

-- Evaluate C4 coeffs by Horner's method
-- Elements c[0] thru c[nC4 - 1] are set
   
   t       NUMBER := 0.0;
   mult    NUMBER := 1.;
   nC4     PLS_INTEGER := 8;
   nC4x    PLS_INTEGER; -- := (nC4 * (nC4 + 1)) / 2;
   j       PLS_INTEGER;
   k       PLS_INTEGER;
   i       PLS_INTEGER;
   once    PLS_INTEGER;
BEGIN
   nC4x := (nC4 * (nC4 + 1)) / 2;
   k := nC4;
   j := nC4x;
   While j <= nc4x loop
     t := 0.0;
     i := nC4 - k + 1;
     once := 1;
     While i <> 0 loop
       if once = 1 then
          once := 0;
          i := i-1;
       end if;
       j := j - 1;
       t := k2 * t + C4x(j);
       k := k -1;
       c(k) := t;
     end loop;
   end loop;
   
   for k in 1..nC4-1 loop
      mult := mult*k2;
      c(k) := c(k) *mult;
   end loop;
 
END C4F;
--
PROCEDURE C3COEFF(C3x IN OUT NOCOPY MDSYS.SDO_LIST_TYPE)AS

--  The coefficients C3[l] in the Fourier expansion of B3
   
BEGIN

      C3x(1) := (1.-g_n)/4.;
      C3x(2) := (1.-g_n*g_n)/8.;
      C3x(3) := (g_n*((-5.*g_n-1.)*g_n+3.)+3.)/64.;
      C3x(4) := (g_n*((2.-2.*g_n)*g_n+2.)+5.)/128.;
      C3x(5) := (g_n*(3.*g_n+11.)+12.)/512.;
      C3x(6) := (10.*g_n+21.)/1024.;
      C3x(7) := 243./16384.;
      C3x(8) := ((g_n-3.)*g_n+2.)/32.;
      C3x(9) := (g_n*(g_n*(2.*g_n-3.)-2.)+3.)/64.;
      C3x(10) := (g_n*((-6.*g_n-9.)*g_n+2.)+6.)/256.;
      C3x(11) := ((1.-2.*g_n)*g_n+5.)/256.;
      C3x(12) := (69.*g_n+108.)/8192.;
      C3x(13) := 187./16384.;
      C3x(14) := (g_n*((5.-g_n)*g_n-9.)+5.)/192.;
      C3x(15) := (g_n*(g_n*(10.*g_n-6.)-10.)+9.)/384.;
      C3x(16) := ((-77.*g_n-8.)*g_n+42.)/3072.;
      C3x(17) := (12.-g_n)/1024.;
      C3x(18) := 139./16384.;
      C3x(19) := (g_n*((20.-7.*g_n)*g_n-28.)+14.)/1024.;
      C3x(20) := ((-7.*g_n-40.)*g_n+28.)/2048.;
      C3x(21) := (72.-43.*g_n)/8192.;
      C3x(22) := 127./16384.;
      C3x(23) := (g_n*(75.*g_n-90.)+42.)/5120.;
      C3x(24) := (9.-15.*g_n)/1024.;
      C3x(25) := 99./16384.;
      C3x(26) := (44.-99.*g_n)/8192.;
      C3x(27) := 99./16384.;
      C3x(28) := 429./114688.;

END C3COEFF;
--
PROCEDURE C4COEFF(C4x IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) AS

--  The coefficients C3[l] in the Fourier expansion of B3

BEGIN
 
      C4x(1) := (g_ep2*(g_ep2*(g_ep2*(g_ep2*(g_ep2*((8704.-7168.*g_ep2)*g_ep2-10880.)+
                14144.)-19448.)+29172.)-51051.)+510510.)/765765.;
      C4x(2) := (g_ep2*(g_ep2*(g_ep2*(g_ep2*((8704.-7168.*g_ep2)*g_ep2-10880.)+14144.)-
                19448.)+29172.)-51051.)/1021020.;
      C4x(3) := (g_ep2*(g_ep2*(g_ep2*((2176.-1792.*g_ep2)*g_ep2-2720.)+3536.)-4862.)+
                7293.)/306306.;
      C4x(4) := (g_ep2*(g_ep2*((1088.-896.*g_ep2)*g_ep2-1360.)+1768.)-2431.)/175032.;
      C4x(5) := (g_ep2*((136.-112.*g_ep2)*g_ep2-170.)+221.)/24310.;
      C4x(6) := ((68.-56.*g_ep2)*g_ep2-85.)/13260.;
      C4x(7) := (17.-14.*g_ep2)/3570.;
      C4x(8) := -1./272.;
      C4x(9) := (g_ep2*(g_ep2*(g_ep2*(g_ep2*(g_ep2*(7168.*g_ep2-8704.)+10880.)-14144.)+
                19448.)-29172.)+51051.)/9189180.;
      C4x(10) := (g_ep2*(g_ep2*(g_ep2*(g_ep2*(1792.*g_ep2-2176.)+2720.)-3536.)+4862.)-
                7293.)/1837836.;
      C4x(11) := (g_ep2*(g_ep2*(g_ep2*(896.*g_ep2-1088.)+1360.)-1768.)+2431.)/875160.;
      C4x(12) := (g_ep2*(g_ep2*(112.*g_ep2-136.)+170.)-221.)/109395.;
      C4x(13) := (g_ep2*(56.*g_ep2-68.)+85.)/55692;
      C4x(14) := (14.*g_ep2-17.)/14280.;
      C4x(15) := 7./7344.;
      C4x(16) := (g_ep2*(g_ep2*(g_ep2*((2176.-1792.*g_ep2)*g_ep2-2720.)+3536.)-4862.)+
              7293.)/15315300.;
      C4x(17) := (g_ep2*(g_ep2*((1088.-896.*g_ep2)*g_ep2-1360.)+1768.)-2431.)/4375800.;
      C4x(18) := (g_ep2*((136.-112.*g_ep2)*g_ep2-170.)+221.)/425425.;
      C4x(19) := ((68.-56.*g_ep2)*g_ep2-85.)/185640.;
      C4x(20) := (17.-14.*g_ep2)/42840.;
      C4x(21) := -7./20400.;
      C4x(22) := (g_ep2*(g_ep2*(g_ep2*(896.*g_ep2-1088.)+1360.)-1768.)+2431.)/42882840.;
      C4x(23) := (g_ep2*(g_ep2*(112.*g_ep2-136.)+170.)-221.)/2382380.;
      C4x(24) := (g_ep2*(56.*g_ep2-68.)+85.)/779688.;
      C4x(25) := (14.*g_ep2-17.)/149940.;
      C4x(26) := 1./8976.;
      C4x(27) := (g_ep2*((136.-112.*g_ep2)*g_ep2-170.)+221.)/27567540;
      C4x(28) := ((68.-56.*g_ep2)*g_ep2-85.)/5012280.;
      C4x(29) := (17.-14.*g_ep2)/706860.;
      C4x(30) := -7./242352.;
      C4x(31) := (g_ep2*(56.*g_ep2-68.)+85.)/67387320.;
      C4x(32) := (14.*g_ep2-17.)/5183640.;
      C4x(33) := 7./1283568.;
      C4x(34) := (17.-14.*g_ep2)/79639560.;
      C4x(35) := -1./1516944.;
      C4x(36) := 1./26254800.;

END C4COEFF;
--
FUNCTION HYPOT(x NUMBER,y NUMBER) RETURN NUMBER  AS
-- Hypotenuse function avoiding both underflow and overflow
   xx  NUMBER := abs(x);
   yy  NUMBER := abs(y);
   a   NUMBER;
   b   NUMBER;
BEGIN

   a := xx;
   b := yy;
   if yy > xx then
     a := yy;
     b := xx;
   end if;
   b := b / a;
   RETURN a * sqrt(1.+b*b);
   
END;
--
FUNCTION Inverse(plat1 NUMBER,plon1 NUMBER,
                 plat2 NUMBER,plon2 NUMBER,
                 s12 IN OUT NOCOPY NUMBER, azi1 IN OUT NOCOPY NUMBER,
                 azi2 IN OUT NOCOPY NUMBER,m12 IN OUT NOCOPY NUMBER) RETURN NUMBER AS


/**
 * \file cpp
 * \brief Implementation for GeographicLib::Geodesic class
 *
 * Copyright (c) Charles Karney (2009, 2010) <charles@karney.com>
 * and licensed under the LGPL.  For more information, see
 * http://geographiclib.sourceforge.net/
 *
 * This is a reformulation of the geodesic problem.  The notation is as
 * follows:
 * - at a general point (no suffix or 1 or 2 as suffix)
 *   - phi = latitude
 *   - beta = latitude on auxiliary sphere
 *   - omega = longitude on auxiliary sphere
 *   - lambda = longitude
 *   - alpha = azimuth of great circle
 *   - sigma = arc length along greate circle
 *   - s = distance
 *   - tau = scaled distance (= sigma at multiples of pi/2)
 * - at northwards equator crossing
 *   - beta = phi = 0
 *   - omega = lambda = 0
 *   - alpha = alpha0
 *   - sigma = s = 0
 * - a 12 suffix means a difference, e.g., s12 = s2 - s1.
 * - s and c prefixes mean sin and cos
 **********************************************************************/
    C1a        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    C2a        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    C3a        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    lat1       NUMBER := plat1;
    lon1       NUMBER := plon1;
    lat2       NUMBER := plat2;
    lon2       NUMBER := plon2;
    lon12      NUMBER;
    temp       NUMBER;
    phi        NUMBER;
    sbet1      NUMBER;
    cbet1      NUMBER;
    sbet2      NUMBER;
    cbet2      NUMBER;
    dummy      NUMBER;
    sig12      NUMBER;
    calp1      NUMBER;
    salp1      NUMBER;
    calp2      NUMBER :=0.0;
    salp2      NUMBER :=0.0;
    v          NUMBER;
    dv         NUMBER;
    ov         NUMBER;
    lam12      NUMBER;
    slam12     NUMBER;
    clam12     NUMBER;
    ssig1      NUMBER;
    csig1      NUMBER;
    ssig2      NUMBER;
    csig2      NUMBER;
    eps        NUMBER;

    g_k2       NUMBER;

    u_ssig1    NUMBER;
    u_csig1    NUMBER;
    u_c2a      NUMBER;
    u_a1m1     NUMBER;
    dalp1      NUMBER;
    sdalp1     NUMBER;
    cdalp1     NUMBER;
    nsalp1     NUMBER;
    lonsign    PLS_INTEGER :=1;
    latsign    PLS_INTEGER :=1;
    swapp      PLS_INTEGER;
    numit      PLS_INTEGER :=0;
    maxit      PLS_INTEGER := 20; --  50; 
    trip       PLS_INTEGER;
    nC1        PLS_INTEGER := g_ord;
    nC2        PLS_INTEGER := g_ord;
    nC3        PLS_INTEGER := g_ord;
    diffp      BOOLEAN;
    meridian   BOOLEAN := FALSE;
    Bad        VARCHAR2(100);
BEGIN

   s12 := NULL;
   if lat1 is NULL or lat1 < -90. or lat1 > 90. then
      Bad := 'Bad Latitude ' || lat1;   
   end if;
   if lat2 is NULL or lat2 < -90. or lat2 > 90. then
      Bad := 'Bad Latitude ' || lat2;   
   end if;
   if lon1 is NULL or lon1 < -180. or lon1 > 180. then
      Bad := 'Bad Longitude ' || lon1;
   end if;
  if lon2 is NULL or lon2 < -180. or lon2 > 180. then
      Bad := 'Bad Longitude' || lon2;
   end if;
   if Bad is NOT NULL then
      dbms_output.put_line('ERROR ' || bad);
      RETURN NULL;
   end if;
--   Initialization
   g_tol1 := 1.e30*g_tol0;  -- 100.*g_tol0;

   g_tol2 := sqrt(g_tol0);
   g_xthresh := 1000. * g_tol2;

   g_f := 1./g_r;
   g_f1 := 1. -g_f;

   g_e2 := g_f*(2. - g_f);
   g_ep2  := (g_e2/(g_f1*g_f1));  --// e2 / (1 - e2)
   g_n := g_f/(2. - g_f);
   g_b  := g_a * g_f1;

   if sqrt(abs(g_e2)) < 0.1 then
     g_etol2  := g_tol2/0.1;
   else
     g_etol2  := g_tol2/sqrt(abs(g_e2));
   end if; 

 --  g_A3x.extend(8);
 --  A3Coeff(g_A3x);
 --  g_C3x.extend(28);
 --  C3Coeff(g_C3x);
 --  g_C4x.extend(36);
 --  C4Coeff(g_C4x);
   
   
   
    lon1 := AngNormalize(lon1);
    lon12 := AngNormalize(AngNormalize(lon2) - lon1);


--    If very close to being on the same meridian, then make it so.
--    Not sure this is necessary...
    lon12 := AngRound(lon12);
--    Make longitude difference positive.
    if lon12 >= 0.0 then
      lonsign := 1;
    else
      lonsign := -1;
    end if;

    lon12 := lon12*lonsign;
    if (lon12 = 180.0) then
      lonsign := 1;
    end if;
--    If really close to the equator, treat as on equator.
    lat1 := AngRound(lat1);
    lat2 := AngRound(lat2);

--    Swap points so that point with higher (abs) latitude is point 1
    swapp := 1;
    if abs(lat1) < abs(lat2) then
      swapp := -1;
    end if;

    if (swapp < 0) then
      lonsign := -lonsign;
      temp := lat1;
      lat1 := lat2;
      lat2 := temp;
    end if;
--    Make lat1 <= 0
    if lat1 >= 0.0 then
      latsign := -1;
    end if;
    lat1 := lat1*latsign;
    lat2 := lat2*latsign;

--    Now we have--    
--        0 <= lon12 <= 180
--         -90 <= lat1 <= 0
--        lat1 <= lat2 <= -lat1
--    
--     longsign, swapp, latsign register the transformation to bring the
--     coordinates to this canonical form.  In all cases, 1 means no change was
--     made.  We make these transformations so that there are few cases to
--     check, e.g., on verifying quadrants in atan2.  In addition, this
--     enforces some symmetries in the results returned.


    phi := lat1 * g_to_radians;
--  Ensure cbet1 = +epsilon at poles
    sbet1 := g_f1 * sin(phi);
--    dbms_output.put_line('SBET1 ' || sbet1 || ' phi ' ||phi || ' g_f1 ' || g_f1);
    if lat1 <> -90.0 then
       cbet1 := cos(phi);
    else
       cbet1 := g_eps2;
    end if;
 
    SinCosNorm(sbet1, cbet1);
 
    phi := lat2 * g_to_radians;
--  Ensure cbet2 = +epsilon at poles
    sbet2 := g_f1 * sin(phi);
--    dbms_output.put_line('SBET2 ' || sbet2 || ' phi ' ||phi || ' g_f1 ' || g_f1);
    if abs(lat2) = 90. then
      cbet2 := g_eps2;
    else
      cbet2 := cos(phi);
    end if;
    SinCosNorm(sbet2, cbet2);


    lam12 := lon12 * g_to_radians;
    if lon12 <> 180.0 then
        slam12 := sin(lam12);
--        dbms_output.put_line('Taking sin lam12' || slam12);
    else
       slam12 := 0.0;
--       dbms_output.put_line('SLAM12 is zero' || slam12);
    end if;
   
    clam12 := cos(lam12);      -- lon12 == 90 isn't interesting


--    index zero elements of these arrays were unused in originaal "C" code
--    real C1a[nC1 + 1], C2a[nC2 + 1], C3a[nC3];
    C1a.extend(nC1+1);
    C2a.extend(nC2+1);
    C3a.extend(nC3);
    
    if lat1 = -90. or slam12 = 0.0 then
       meridian := TRUE;
    end if;
   
    if (meridian = TRUE) then
-- dbms_output.put_line('meridian is TRUE' || slam12);
--      Endpoints are on a single full meridian, so the geodesic might lie on
--      a meridian.

      calp1 := clam12; 
      salp1 := slam12; --Head to the target longitude
      calp2 := 1.; 
      salp2 := 0;           --// At the target we're heading north

 
 --       // tan(bet) = tan(sig) * cos(alp),
        ssig1 := sbet1;
        csig1 := calp1 * cbet1;
        ssig2 := sbet2; 
        csig2 := calp2 * cbet2;

--      // sig12 = sig2 - sig1
      temp := csig1 * ssig2 - ssig1 * csig2;
      if temp < 0.0 then
         temp := 0.0;
      end if;
      sig12 := new_arctan(temp,csig1 * csig2 + ssig1 * ssig2);
--      dbms_output.put_line('calling lengths');
      Lengths(g_n, sig12, ssig1, csig1, ssig2, csig2,
                cbet1, cbet2, s12, m12, dummy, C1a, C2a);
--dbms_output.put_line('BACK FROM lengths');
--       Add the check for sig12 since zero length geodesics might yield m12 <
--       0.  Test case was
--      
--          echo 20.001 0 20.001 0 | Geod -i
--      
--       In fact, we will have sig12 > pi/2 for meridional geodesic which is
--       not a shortest path.

      if (sig12 < 1. or m12 >= 0.) then
        m12 := m12*g_a;
        s12 := s12*g_b;
        sig12 := sig12*g_to_degrees;
      else
--         m12 < 0, i.e., prolate and too close to anti-podal
        meridian := false;
--        dbms_output.put_line('meridian is ffalse');
      end if;
    end if;

    if (meridian = FALSE and
        sbet1 = 0.0 and (g_f <= 0.0 or lam12 <= g_pi - g_f * g_pi)) then  
--   // and sbet2 == 0
--      Mimic the way Lambda12 works with calp1 = 0
        
--      Geodesic runs along equator
--dbms_output.put_line('meridian is FALSE' || s12);
      calp1 := 0.0;
      calp2 := 0.0; 
      salp1 := 1.;
      salp2 := 1.;
      s12   := g_a * lam12;
--      dbms_output.put_line('g_a ' || g_a || ' lam12 ' || lam12);
      m12   := g_b * sin(lam12 / g_f1);
      sig12 := lon12 / g_f1;
--      dbms_output.put_line('sig12 ' ||sig12 || ' s12 ' || s12);
    elsif (meridian = FALSE) then
--dbms_output.put_line('meridian is false');
--      Now point1 and point2 belong within a hemisphere bounded by a
--      meridian and geodesic is neither meridional or equatorial.

--      Figure a starting point for Newton's method

--      dbms_output.put_line('calling inversestart salp2' || salp2 || ' calp2 ' || calp2);
--      dbms_output.put_line('sbet1' || sbet1);
--dbms_output.put_line('cbet1 ' || cbet1);
--dbms_output.put_line('sbet2 ' || sbet2);
--dbms_output.put_line('cbet2' || cbet2);
      sig12 := InverseStart(sbet1, cbet1, sbet2, cbet2,
                           lam12,salp1, calp1, salp2, calp2,C1a, C2a);
--      dbms_output.put_line('back from inversestart sig12 ' || sig12);
--      dbms_output.put_line('lam12' || lam12);
--dbms_output.put_line('salp1' || salp1);
--dbms_output.put_line('calp1 ' || calp1);
--dbms_output.put_line('salp2 ' || salp2);
--dbms_output.put_line('calp2' || calp2);
--dbms_output.put_line('sig12' || sig12);
      if (sig12 >= 0.) then
--      Short lines (InverseStart sets salp2, calp2)
        s12 := sig12 * g_a * sqrt(1.0 - g_e2 * cbet1*cbet1);
        m12 := s12;
        sig12 := sig12*g_to_degrees;
      else 
--      Newton's method

        ov := 0.0;
        numit := -1;
        trip := 0;
        
        While numit< maxit loop
        numit := numit + 1;
        diffp := FALSE;
          if trip < 1 then
             diffp := TRUE;
          end if;
          v := Lambda12(sbet1, cbet1, sbet2, cbet2, salp1, calp1,
                            salp2, calp2, sig12, ssig1, csig1, ssig2, csig2,
                            eps, diffp, dv, C1a, C2a, C3a) - lam12;
/*
    dbms_output.put_line('eps' || eps);
dbms_output.put_line('dv ' || dv);
dbms_output.put_line('v ' || v);
dbms_output.put_line('calp2' || calp2);
dbms_output.put_line('salp2' || salp2);
dbms_output.put_line('ssig1' || ssig1);
dbms_output.put_line('csig1' || csig1);
dbms_output.put_line('ssig2' || ssig2);
dbms_output.put_line('csig2' || csig2);
dbms_output.put_line('sig12' || sig12);      
       dbms_output.put_line('VV ' || round(v,15) || ' dv ' || round(dv,20));
       */
          if (abs(v) <= g_eps2 or (trip >= 1) ) then
            temp := ov;
            if g_tol1 > ov then
              temp := g_tol1;
            end if;
            if (abs(v) > temp) then
              numit := maxit;
            end if;
            exit;
          end if;
          
          dalp1 := -v/dv;

          sdalp1 := sin(dalp1); 
          cdalp1 := cos(dalp1);
          nsalp1 := salp1 * cdalp1 + calp1 * sdalp1;
          calp1  := calp1 * cdalp1 - salp1 * sdalp1;
          salp1  := 0.0;
          if nsalp1 > 0.0 then
            salp1 := nsalp1;
          end if;

          SinCosNorm(salp1, calp1);
--        In some regimes we don't get quadratic convergence because slope
--         -> 0.  So use convergence conditions based on epsilon instead of
--         sqrt(epsilon).  The first criterion is a test on abs(v) against
--         100 * epsilon.  The second takes credit for an anticipated
--         reduction in abs(v) by v/ov (due to the latest update in alp1) and
--         checks this against epsilon.
          if (abs(v) < g_tol1 or v*v < (ov * g_tol0)) then          
             trip := trip + 1;
          end if;
          ov := abs(v);
        end loop;


        Lengths(eps, sig12, ssig1, csig1, ssig2, csig2,
                  cbet1, cbet2, s12, m12, dummy, C1a, C2a);

        m12 := m12*g_a;
        s12 := s12*g_b;
        sig12 := sig12*g_to_degrees;

        if (numit >= maxit) then
        dbms_output.put_line('Failure ' || numit);
--          Signal failure to converge by negating the distance and azimuths.
--          I don't know if I like this!!!
          s12   := -s12; 
          sig12 := -sig12; 
          m12   := -m12;
          salp1 := -salp1; 
          calp1 := -calp1;
          salp2 := -salp2; 
          calp2 := -calp2;
        end if;
      end if;
    end if;
--  Convert calp, salp to azimuth accounting for lonsign, swapp, latsign.
--  This code is needed to calculate the azimuth's but its slow so commented
    if (swapp < 0) then
      temp := salp1;
      salp1 := salp2;
      salp2 := temp;
      temp  := calp1;
      calp1 := calp2;
      calp2 := temp;
    end if;

--  minus signs give range [-180, 180). 0- converts -0 to +0.

    azi1 := 0. - new_arctan(- swapp * lonsign * salp1,
                     + swapp * latsign * calp1) / g_to_radians;
    azi2 := 0. - new_arctan(- swapp * lonsign * salp2,
                     + swapp * latsign * calp2) / g_to_radians;

--   Return value in [0, 180], unless it's negated to signal convergence failure

    return sig12;

END Inverse;
--
FUNCTION Lambda12(sbet1 NUMBER, cbet1 NUMBER,  sbet2 NUMBER,  cbet2 NUMBER,
                                salp1 NUMBER,  calp1 IN OUT NOCOPY NUMBER,
                                salp2 IN OUT NOCOPY NUMBER, calp2 IN OUT NOCOPY NUMBER,
                                sig12 IN OUT NOCOPY NUMBER,
                                ssig1 IN OUT NOCOPY NUMBER, csig1 IN OUT NOCOPY NUMBER,
                                ssig2 IN OUT NOCOPY NUMBER, csig2 IN OUT NOCOPY NUMBER,
                                eps IN OUT NOCOPY NUMBER, diffp BOOLEAN, dlam12 IN OUT NOCOPY NUMBER,
                                 C1a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,  
                                 C2a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,  C3a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN NUMBER AS

     salp0    NUMBER;
     calp0    NUMBER;
     somg1    NUMBER;  
     comg1    NUMBER;  
     somg2    NUMBER;  
     comg2    NUMBER;  
     omg12    NUMBER;  
     lam12    NUMBER; 
     mu       NUMBER;  
     k2       NUMBER;
     B312     NUMBER;
     h0       NUMBER;
     temp     NUMBER;
     dummy    NUMBER;
     dummy1   NUMBER;
     dummy2   NUMBER;
     nC3        PLS_INTEGER := g_ord;
BEGIN  
--    dbms_output.put_line('in Lambda');
    if (sbet1 = 0.0 and calp1 = 0.0) then
--      Break degeneracy of equatorial line.  This case has already been handled.
      calp1 := -g_eps2;
--       dbms_output.put_line('set calp1 to -g_eps2 ' || calp1);
    end if;

--      // sin(alp1) * cos(bet1) = sin(alp0),
      salp0 := salp1 * cbet1;
      calp0 := hypot(calp1, salp1 * sbet1); --// calp0 > 0
--       dbms_output.put_line('calp1 ' || calp1);
--        dbms_output.put_line('salp1 ' || salp1);
--         dbms_output.put_line('sbet1 ' || sbet1);
-- dbms_output.put_line('calp0 ' || calp0);
    
--    // tan(bet1) = tan(sig1) * cos(alp1)
--    // tan(omg1) = sin(alp0) * tan(sig1) = tan(omg1)=tan(alp1)*sin(bet1)
    ssig1 := sbet1; 
    somg1 := salp0 * sbet1;
    csig1 := calp1 * cbet1;
    comg1 := csig1;
    SinCosNorm(ssig1, csig1);
    SinCosNorm(somg1, comg1);

--    Enforce symmetries in the case abs(bet2) = -bet1.  Need to be careful
--    about this case, since this can yield singularities in the Newton
--    iteration.
--    // sin(alp2) * cos(bet2) = sin(alp0),
     if cbet2 <> cbet1 then
       salp2 := salp0/cbet2;
     else
       salp2 := salp1;
     end if;
    
--    // calp2 = sqrt(1 - sq(salp2))
--    //       = sqrt(sq(calp0) - sq(sbet2)) / cbet2
--    // and subst for calp0 and rearrange to give (choose positive sqrt
--    // to give alp2 in [0, pi/2]).
     if cbet2 <> cbet1 or abs(sbet2) <> -sbet1 then
       if cbet1 < -sbet1 then
         calp2 := sqrt( (calp1 * cbet1)*(calp1 *cbet1) + (cbet2 - cbet1) * (cbet1 + cbet2)) / cbet2;
       else
         calp2 := sqrt( (calp1 * cbet1)*(calp1 *cbet1) + (sbet1 - sbet2) * (sbet1 + sbet2)) / cbet2;
       end if;
     else
       calp2 := abs(calp1);
     end if;
  
--     dbms_output.put_line('CALP2 ' || calp2 || ' cbet2 ' || cbet2 || ' both ' || (calp2 *cbet2));
--    // tan(bet2) = tan(sig2) * cos(alp2)
--    // tan(omg2) = sin(alp0) * tan(sig2).
    ssig2 := sbet2; 
    somg2 := salp0 * sbet2;
--    dbms_output.put_line('sbet2 ' || sbet2);
    csig2 := calp2 * cbet2;
    comg2 := csig2;
--    dbms_output.put_line('comg2 ' || comg2);
    SinCosNorm(ssig2, csig2);
    SinCosNorm(somg2, comg2);

--    // sig12 = sig2 - sig1, limit to [0, pi]
    temp := csig1 * ssig2 - ssig1 * csig2;
    if temp < 0. then
      temp := 0.;
    end if;
    sig12 := new_arctan(temp,csig1 * csig2 + ssig1 * ssig2);
--dbms_output.put_line('sig12 ' || sig12);
--    // omg12 = omg2 - omg1, limit to [0, pi]
    temp := comg1 * somg2 - somg1 * comg2;
--    dbms_output.put_line('temp ' || temp);
    if temp < 0. then
       temp := 0.;
    end if;
    omg12 := new_arctan(temp,comg1 * comg2 + somg1 * somg2);
-- dbms_output.put_line('omg12 ' || omg12);
    mu := calp0*calp0;
    k2 := mu * g_ep2;
--    dbms_output.put_line('mu ' || mu || ' g_ep2 ' ||g_ep2);
    eps := k2 / (2. * (1. + sqrt(1. + k2)) + k2);
--    dbms_output.put_line('CALP0 ' || calp0 || ' k2 ' ||k2 || ' eps '||eps);
    C3f(g_f, eps, C3a);
    B312 := (SinSeries(ssig2, csig2, C3a, nC3-1) -
             SinSeries(ssig1, csig1, C3a, nC3-1));

    h0 := -g_f * A3f(g_f, eps);
--    dbms_output.put_line('omg12 ' || omg12);
 --   dbms_output.put_line('salp0 ' || salp0);
 --   dbms_output.put_line('h0 ' || h0);
--   dbms_output.put_line('sig12 ' || sig12);
--    dbms_output.put_line('B312 '||B312);
    lam12 := omg12 + salp0 * h0 * (sig12 + B312);

    if (diffp) then
      if (calp2 = 0.0) then
        dlam12 := -2. * sqrt(1. - g_e2 * (cbet1*cbet1)) / sbet1;
      else
 
        Lengths(eps, sig12, ssig1, csig1, ssig2, csig2,
                cbet1, cbet2, dummy1, dlam12, dummy2, C1a, C2a);
        dlam12 := dlam12/(calp2 * cbet2);
      end if;
    end if;
-- dbms_output.put_line('leaving Lambda' || lam12 || ' dlam12 ' || dlam12);
    return lam12;
END LAMBDA12;
--
FUNCTION InverseStart(sbet1 NUMBER, cbet1 NUMBER,sbet2 NUMBER, cbet2 NUMBER,lam12 NUMBER,
                                    salp1 IN OUT NOCOPY NUMBER, calp1 IN OUT NOCOPY NUMBER,
                   --                 // Only updated if return val >= 0
                                    salp2 IN OUT NOCOPY NUMBER, calp2 IN OUT NOCOPY NUMBER,
--                                    // Scratch areas of the right size
                                    C1a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, C2a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN NUMBER AS
  sig12      NUMBER := -1.;
  sbet12     NUMBER;
  cbet12     NUMBER;
  sbet12a    NUMBER;
  cbet12a    NUMBER;
  bet12a     NUMBER;
  omg12      NUMBER;
  omg12a     NUMBER;
  somg12     NUMBER;
  ssig12     NUMBER;
  csig12     NUMBER;
  comg12     NUMBER;
  m0         NUMBER;
  dummy      NUMBER;
  x          NUMBER;
  y          NUMBER; 
  lamscale   NUMBER;
  betscale   NUMBER;
  mu         NUMBER;
  k          NUMBER;
  k2         NUMBER;
  eps        NUMBER;
  shortline  BOOLEAN := FALSE;
BEGIN
--    Return a starting point for Newton's method in salp1 and calp1 (function
--    value is -1).  If Newton's method doesn't need to be used, return also
--    salp2 and calp2 and function value is sig12.

--      // bet12 = bet2 - bet1 in [0, pi); bet12a = bet2 + bet1 in (-pi, 0]
      sbet12 := sbet2 * cbet1 - cbet2 * sbet1;
      cbet12 := cbet2 * cbet1 + sbet2 * sbet1;
--      dbms_output.put_line('cbet2 ' || cbet2 || ' sbet ' || sbet2);
--      dbms_output.put_line('cbet1 ' || cbet1 || ' sbet ' || sbet1);
      sbet12a := sbet2 * cbet1 + cbet2 * sbet1;
--dbms_output.put_line('cbet12 ' || cbet12 || ' sbet ' || sbet12 || ' lam12 ' || lam12);
    if cbet12 >= 0.0 and sbet12 < 0.5 and lam12 <= g_pi / 6. then
      shortline := TRUE;
--      dbms_output.put_line('SHORT LINE ');
    end if;

    if shortline then
      omg12 := lam12 / sqrt(1. - g_e2 * cbet1*cbet1);
    else
      omg12 := lam12;
    end if;
    somg12 := sin(omg12);
    comg12 := cos(omg12);

    salp1 := cbet2 * somg12;
    if comg12 >= 0.0 then
      calp1 := sbet12 + cbet2 * sbet1 * somg12*somg12 / (1. + comg12);
    else
      calp1 := sbet12a - cbet2 * sbet1 * somg12*somg12 / (1. - comg12);
    end if;
--dbms_output.put_line('salp1 ' || salp1 || ' calp1 ' || calp1);
--dbms_output.put_line('salp2 ' || salp2 || ' calp2 ' || calp2);
--dbms_output.put_line('somg ' || somg12 || ' comg12 ' || comg12);
      ssig12 := hypot(salp1, calp1);
      csig12 := sbet1 * sbet2 + cbet1 * cbet2 * comg12;
--      dbms_output.put_line('ssig12 ' || ssig12);
--dbms_output.put_line('g_eto12 ' || g_etol2);
    if (shortline and ssig12 < g_etol2) then
--    really short lines
      salp2 := cbet1 * somg12;
      calp2 := (sbet12 - cbet1 * sbet2 * somg12*somg12 / (1. + comg12));
--    Set return value
      sig12 := new_arctan(ssig12, csig12);
--      dbms_output.put_line('really SHORT ' || sig12 || ' g_etol ' || g_etol2);
    elsif (csig12 >= 0.0 or
               ssig12 >= 3. * abs(g_f) * g_pi * cbet1*cbet1) then
--      Nothing to do, zeroth order spherical approximation is OK
        NULL;
--        dbms_output.put_line('at NULL');
    else 
--      Scale lam12 and bet2 to x, y coordinate system where antipodal point
--      is at origin and singular point is at y = 0, x = -1.

      if (g_f >= 0.0) then            -- In fact f == 0 does not get here
--          x = dlong, y = dlat
            mu := sbet1*sbet1;
            k2 := mu * g_ep2;
            eps := k2 / (2. * (1. + sqrt(1. + k2)) + k2);
            lamscale := g_f * cbet1 * A3f(g_f, eps) * g_pi;
        betscale := lamscale * cbet1;

        x := (lam12 - g_pi) / lamscale;
        y := sbet12a / betscale;
 
      else                   -- _f < 0
--        x = dlat, y = dlong

          cbet12a := cbet2 * cbet1 - sbet2 * sbet1;
          bet12a := new_arctan(sbet12a, cbet12a);

--        In the case of lon12 = 180, this repeats a calculation made in
--        Inverse.
        Lengths(g_n, g_pi + bet12a, sbet1, -cbet1, sbet2, cbet2,
                cbet1, cbet2, dummy, x, m0, C1a, C2a);
        x := -1. + x/(g_f1 * cbet1 * cbet2 * m0 * g_pi);
        if x < -0.01 then
          betscale := sbet12a / x;
        else
          betscale := -g_f * cbet1*cbet1 * g_pi;
        end if;
        lamscale := betscale / cbet1;
        y := (lam12 - g_pi) / lamscale;
      end if;

      if (y > -g_tol1 and x >  -1. - g_xthresh) then
--      strip near cut
        if (g_f >= 0.0) then
          salp1 := 1.;
          if -x < 1. then
            salp1 := -x; 
          end if;
          calp1 := - sqrt(1. - salp1*salp1);
        else 
          calp1 := -1.;
          if x > -1. then
            calp1 := x;
          end if;
          salp1 :=   sqrt(1. - calp1*calp1);
        end if;
      else 
--        Estimate alp2, by solving calp2 * (salp2 + x) - y * salp2 = 0.  (For
--        f < 0, we're solving for pi/2 - alp2 and calp2 and salp2 are swapped.)
        k := Astroid(x, y);
--        estimate omg12a = pi - omg12
          
          if g_f >= 0.0 then
           omg12a := lamscale * ( -x * k/(1. + k));
          else
           omg12a := lamscale * (-y * (1. + k)/k );
          end if;
          somg12 := sin(omg12a);
          comg12 := -cos(omg12a);
--        Update spherical estimate of alp1 using omg12 instead of lam12
        salp1 := cbet2 * somg12;
        calp1 := sbet12a - cbet2 * sbet1 * somg12*somg12 / (1. - comg12);
      end if;
    end if;
    SinCosNorm(salp1, calp1);
    return sig12;

END InverseStart;
--
FUNCTION EXPM1(x NUMBER) RETURN NUMBER  AS
--      Exponential function accurate near x = 0
--      exp(x) - 1 accurate near x = 0.  This is taken from
--      N. J. Higham, Accuracy and Stability of Numerical Algorithms, 2nd
--      Edition (SIAM, 2002), Sec 1.14.1, p 19.
   y  NUMBER; -- := exp(x);
   z  NUMBER; -- := y - 1.;
BEGIN
--     The reasoning here is similar to that for log1p.  The expression
--     mathematically reduces to exp(x) - 1, and the factor z/log(y) = (y - 1)/log(y)
--     is a slowly varying quantity near y = 1 and is accurately computed.
    y := exp(x);
    z := y-1.;
   if abs(x) > 1.0 then
     NULL;
   elsif z = 0. then
     z := x;
   else
     z := x*z/ln(y);
   end if;
   
   RETURN z;
END;
--
FUNCTION ISFINITE(x NUMBER) RETURN BOOLEAN AS
--DECLARE
--   min_num NUMBER(38,127);
--   max_num NUMBER(38,-84);
--BEGIN
   /* 127 is largest scale, so begin with 1 and move
      decimal point 127 places to the left. Easy. */
--   min_num := 1E-127;
--   DBMS_OUTPUT.PUT_LINE(min_num);

   /* -84 is smallest scale value. Add 37 to normalize
      the scientific-notation, and we get E+121. */
--   max_num := 9.9999999999999999999999999999999999999E+121;
--   DBMS_OUTPUT.PUT_LINE(max_num);
--END;
--/

BEGIN
  if abs(x) <= 9.999e125 then
    RETURN TRUE;
  end if;
  RETURN FALSE;
END;
--
PROCEDURE Lengths(eps NUMBER, sig12 NUMBER, ssig1 NUMBER, csig1 NUMBER, 
                  ssig2 NUMBER, csig2 NUMBER,cbet1 NUMBER, cbet2 NUMBER,
                  s12b IN OUT NOCOPY NUMBER,m12a IN OUT NOCOPY NUMBER, 
                  m0 IN OUT NOCOPY NUMBER, 
                  C1a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, C2a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) AS
  A1m1        NUMBER;
  A2m1        NUMBER;
  AB1         NUMBER;
  AB2         NUMBER;
  nC1         PLS_INTEGER := g_ord;
  nC2         PLS_INTEGER := g_ord;
BEGIN 
--    Return m12a = (reduced length)/_a; also calculate s12b = distance/_b,
--    and m0 = coefficient of secular term in expression for reduced length.
--dbms_output.put_line('in lengths ' || eps);
--    C1f(eps, C1a);
--    C2f(eps, C2a);
    C12f(eps, C1a,C2a);

    A1m1 := A1m1f(eps);
    AB1 := (1. + A1m1) * (SinSeries(ssig2, csig2, C1a, nC1) -
                          SinSeries(ssig1, csig1, C1a, nC1));
    A2m1 := A2m1f(eps);
    AB2 := (1. + A2m1) * (SinSeries(ssig2, csig2, C2a, nC2) -
                          SinSeries(ssig1, csig1, C2a, nC2));
    m0 := A1m1 - A2m1;
--    dbms_output.put_line('m0 ' || m0 || ' A1m1 ' || A1M1 || ' A2m1 ' || A2m1);
--    Missing a factor of _a.
--    Add parens around (csig1 * ssig2) and (ssig1 * csig2) to ensure accurate
--    cancellation in the case of coincident points.
    m12a := (sqrt(1. - g_e2 * cbet2*cbet2) * (csig1 * ssig2) -
             sqrt(1. - g_e2 * cbet1*cbet1) * (ssig1 * csig2))
          - g_f1 * csig1 * csig2 * ( m0 * sig12 + (AB1 - AB2) );
--      dbms_output.put_line('sig12 ' ||sig12 || ' g_f1 ' || g_f1);
--      dbms_output.put_line('cbet1 ' ||cbet1 || ' csig1 ' || csig1 || ' ssig1 ' || ssig1);
--      dbms_output.put_line('cbet2 ' ||cbet2 || ' csig2 ' || csig2 || ' ssig2 ' || ssig2);
--      dbms_output.put_line('m12a ' || m12a || ' AB1 ' || AB1 || ' Ab2 ' || AB2); 
--  Missing a factor of _b
    s12b :=  (1. + A1m1) * sig12 + AB1;
--    dbms_output.put_line('leaving lengths');
END Lengths;
--
FUNCTION LOG1P(x NUMBER) RETURN NUMBER  IS
--     log(x + 1) accurate near x = 0.  This is taken See D. Goldberg,
--     <a href="http://docs.sun.com/source/806-3568/ncg_goldberg.html"> What
--     every computer scientist should know about floating-point arithmetic</a>
--     (1991), Theorem 4.  See also, Higham (op. cit.), Answer to Problem 1.5,
--     p 528.
-- TESTED: 
--SQL>  select log1p(1.E-50) from dual;
--LOG1P(1.E-50)
----------------------
--            1.0000E-50

  y  NUMBER; -- := 1 + x;
  z  NUMBER; -- := y-1.;
BEGIN
  y := 1. + x;
  z := y - 1.;
--      Here's the explanation for this magic: y = 1 + z, exactly, and z
--      approx x, thus log(y)/z (which is nearly constant near z = 0) returns
--      a good approximation to the true log(1 + x)/x.  The multiplication x *
--      (log(y)/z) introduces little additional error.
     If z = 0. then
        z := x;
     else
       z := x * ln(y)/z;
     end if;

   RETURN z; 
END LOG1P;
--
PROCEDURE SCALEM( a12 NUMBER,M12 IN OUT NOCOPY NUMBER,M21 IN OUT NOCOPY NUMBER) AS 

-- NOT CALLED <<<<<<<<<<<<<<<<<<<<<<<<<
    u_c1a    MDSYS.SDO_LIST_TYPE;
    u_c2a    MDSYS.SDO_LIST_TYPE;
--    // sig2 := sig1 + sig12
    sig12    NUMBER := a12;
    a12a     NUMBER := abs(a12);
    ssig12   NUMBER := 0.0;
    csig12   NUMBER := 0.0;
    ssig2    NUMBER ;
    csig2    NUMBER;
    ssig1sq  NUMBER;
    ssig2sq  NUMBER;
    w1       NUMBER;
    w2       NUMBER;
    B12      NUMBER;
    B22      NUMBER;
    AB1      NUMBER;
    AB2      NUMBER;
    J12      NUMBER;

    u_k2       NUMBER;
    u_ssig1    NUMBER;
    u_csig1    NUMBER;

    u_a1m1     NUMBER;
    u_a2m1     NUMBER;
    u_B11      NUMBER;
    u_B21      NUMBER;
    nC1        PLS_INTEGER := g_ord;
    nC2        PLS_INTEGER := g_ord;
    
  BEGIN
    sig12 := sig12 * g_to_radians;
    a12a     := a12a - 180. * floor(a12a / 180.);
    if a12a <> 0. then
      ssig12 := sin(sig12);
    end if;
    if a12a <> 90. then
      csig12 := cos(sig12);
    end if; 
    ssig2    := u_ssig1 * csig12 + u_csig1 * ssig12;
    csig2    := u_csig1 * csig12 - u_ssig1 * ssig12;
    ssig1sq  := u_ssig1*u_ssig1;
    ssig2sq  :=   ssig2*ssig2;
    w1       := sqrt(1. + u_k2 * ssig1sq);
    w2       := sqrt(1. + u_k2 * ssig2sq);
    B12 := SinSeries(ssig2, csig2, u_C1a, nC1);
    B22 := SinSeries(ssig2, csig2, u_C2a, nC2);
    AB1      := (1. + u_A1m1) * (B12 - u_B11);
    AB2      := (1. + u_A2m1) * (B22 - u_B21);
    J12      := (u_A1m1 - u_A2m1) * sig12 + (AB1 - AB2);
    M12 := csig12 + (u_k2 * (ssig2sq - ssig1sq) * ssig2/ (w1 + w2)
                    - csig2 * J12) * u_ssig1 / w1;
    M21 := csig12 - (u_k2 * (ssig2sq - ssig1sq) * u_ssig1/ (w1 + w2)
                    - u_csig1 * J12) * ssig2 / w2;

END SCALEM;
--
PROCEDURE SINCOSNORM(sinx IN OUT NOCOPY NUMBER,cosx IN OUT NOCOPY NUMBER) AS

   r    NUMBER;
BEGIN
   r := hypot(sinx,cosx);
   sinx := sinx/r;
   cosx := cosx/r;
END;
--
FUNCTION SINSERIES(sinx NUMBER,cosx NUMBER,C MDSYS.SDO_LIST_TYPE,NN PLS_INTEGER) RETURN NUMBER  IS

--   Evaluate y = sum(c[i] * sin(2 * i * x), i, 1, n) using Clenshaw
--   summation.  (c[0] was unused in original "C" code)
--   Approx operation count = (n + 5) mult and (2 * n + 2) add

   ar     NUMBER := 2.* (cosx-sinx) * (cosx+sinx); --  2 * cos(2 * x)
   n     PLS_INTEGER := nn;
   y0     NUMBER := 0.0;
   y1     NUMBER := 0.0;
BEGIN
-- Have to remember CC is ZERO based here <<<<<<<<<<<<<<<<<<
-- But the code does not use the zero elements in the array!
--C    0 1 2 3 4 5 6 7 8
--sql    1 2 3 4 5 6 7 8

-- if n is odd we want c[n--];
   if MOD(n,2) = 1 then   -- y0 = n & 1 ? c[n--] : 0, y1 = 0; // Accumulators for sum
     y1 := c(n);
     n := n-1;
   end if;
   While n > 1 loop    -- n is even now
--    Unroll loop x 2, so accumulators return to their original role
      y1 := ar * y0 - y1 + c(n);
      n := n-1;
      y0 := ar * y1 - y0 + c(n);
      n := n-1;
   End Loop;
   y0 := y0 * 2.*sinx*cosx; --  sin(2 * x) * y0
   
   RETURN y0;
END SINSERIES;
--
END GZ_GEODESIC;
/
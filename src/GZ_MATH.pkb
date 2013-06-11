create or replace
PACKAGE BODY "GZ_MATH" AS


-- UPDATED: Jan 10,2012 with functions for hyperbolic asinh,atanh and 
--          exponentials and logs near zero and 1 respectively.
-- These next 5 functions copied from the Geodesic package by Charles Karney

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
-- Written by Charles Karney.
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
FUNCTION EXPM1(x NUMBER) RETURN NUMBER  AS
--      Exponential function accurate near x = 0
--      exp(x) - 1 accurate near x = 0.  This is taken from
--      N. J. Higham, Accuracy and Stability of Numerical Algorithms, 2nd
--      Edition (SIAM, 2002), Sec 1.14.1, p 19.
-- Written by Charles Karney.
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
FUNCTION LOG1P(x NUMBER) RETURN NUMBER  IS
--     Logarithm of 1 plus x.
--     log(x + 1) accurate near x = 0.  This is taken See D. Goldberg,
--     <a href="http://docs.sun.com/source/806-3568/ncg_goldberg.html"> What
--     every computer scientist should know about floating-point arithmetic</a>
--     (1991), Theorem 4.  See also, Higham (op. cit.), Answer to Problem 1.5,
--     p 528.
-- Written by Charles Karney.
-- TESTED: 
--SQL>  select geodesic.log1p(1.E-50) from dual;
--GEODESIC.LOG1P(1.E-50)
----------------------
--            1.0000E-50
-- whereas Oracle get zero for ln(1+x) when x>= 1.E-39
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
FUNCTION NEW_ARCCOS(XIn NUMBER) RETURN NUMBER Deterministic IS
/*
--******************************************************************************
--Program Name: new_arccos
--Author: Sidey Timmins
--Creation Date: 12/27/2006
--Modified:  10/17/2011 To use new_arctan. Tested 12/21/11 on Xeon 7350
--Usage:
  -- This function has one parameter:
  --
  --   REQUIRED Parameter: 
  
  --            X:  X abscissa a number in the range [-1,1]
  --
--Purpose:   -- Calculates the arccosine function -  acos(x) 
--              as fast as possible (>10 times faster than the built in 
--              10G function that takes 15 seconds for 100000 values) with 
--              an accuracy >=38 decimal digits.
--              The domain is limited from -1. to 1.
--
-- Method:      It uses trigonometric identities to call the arc tangent
--              (see c_atan2):       
--              acos(x)  = 2*atan(sqrt((1-x)/(1+x)))       x>=0
--                       = pi - 2*atan(sqrt((1+x)/(1-x)))  x<0
--
-- Accuracy:  The maximum error in the range - 1 to 1 is less than 1.0E-38 
--            with an argument of 


-- Reference: Ulrich H Kurzweg and Sidey P. Timmins "A New Method for obtaining
--            Highly Accurate Approximations to the Arctangent Function". 
--            submitted to IEEE Transactions on Computers, Oct 12, 2011

--Dependencies: c_atan2.   Also see c_test_atan2 for rigorous testing.
--******************************************************************************
*/
    pi    constant NUMBER         := 3.1415926535897932384626433832795028842;
    X         NUMBER := XIn;
    Y         NUMBER;
    result   NUMBER;
BEGIN


-- Restrict the domain
  IF X > 1.0 THEN
    Y := 1.;
  ELSIF X < -1.0 THEN
    Y := -1.;
  ELSE
--    In the allowed domain..
--    acos(x) = 2*atan(sqrt((1-x)/(1+x)))       x>=0
--            = pi - 2*atan(sqrt((1+x)/(1-x)))  x<0
    IF X < 0. THEN
      Y := sqrt((1.+X)/(1.-X));
    ELSE
      Y := sqrt((1.-X)/(1.+X));
    END IF;
  END IF;
  
  result := 2*new_arctan(Y,1.);

  IF X < 0.0 THEN
    result := pi - result;
  ELSIF X = 0.0 THEN
    result := pi * 0.5;   -- special case to keep error small
  END IF;
  
  RETURN result;
       
END NEW_ARCCOS;
--
FUNCTION NEW_ARCSIN(XIn NUMBER) RETURN NUMBER
 Deterministic IS
/*
--******************************************************************************
--Program Name: c_asin
--Author: Sidey Timmins
--Creation Date: 12/27/2006
--Modified:  10/17/2011 To use new_arctan
--Usage:
  -- Call this function from inside a PL/SQL program or directly from SQL.
  
  --   REQUIRED Parameter: 
  
  --            X:  X abscissa (number) in the range [-1,1]
  --
--Purpose:   -- Calculates the arcsine function -  asin(x) 
--              as fast as possible (>10 times faster than the built in 
--              function that takes 4.7 seconds for 20000 values) with 
--              an accuracy of >= 38 decimal digits.
--              The domain is limited from -1 to 1.
--
-- Method:      This function uses a trigonometric identity to call the
--              arctangent function to create the desired result:
--              asin(x) = pi/2 - acos(x)

-- Accuracy:  The maximum error in the range - 1 to 1 is less than 1.0E-38 
--            with an :


-- Reference: Ulrich H Kurzweg and Sidey P. Timmins "A New Method for obtaining
--            Highly Accurate Approximations to the Arctangent Function". 
--            submitted to IEEE Transactions on Computers, Oct 12, 2011            
--     
--Dependencies: c_atan2.  
--******************************************************************************
*/
    piByTwo   CONSTANT  NUMBER     := 1.5707963267948966192313216916397514421;
    X         NUMBER := XIn;
    Y         NUMBER;
    result   NUMBER;
BEGIN

-- Limit the domain
  IF X > 1.0 THEN
    result := pibyTwo;
  ELSIF X < -1.0 THEN
    result := -pibyTwo;
  ELSE
--  Domain is good
--   asin(x) = 2*atan(x/(1+sqrt(1-x*x)))
--    Y := XIn/ (1. + sqrt(1.0 + XIn*XIn));
-- Use fast c_atan2 function to generate the desired result 
--  RETURN  2*c_atan2(Y,1.);

--  This is simpler
    result := pibyTwo  - new_arccos(X);
  END IF;
 
  RETURN result;
       
END NEW_ARCSIN;
--
FUNCTION NEW_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER AS 

/*
--******************************************************************************
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
 
--   dbms_output.put_line('x now is ' || to_char(x,'9999EEEE') || ' reg ' || region);

    if yin <> 0.0 and ((ABS(YIn) >= ABS(XIn) and u <= 1.) or (ABS(YIn) < ABS(XIn) and u > 1.)) then
     complement := TRUE;
     end if;


     if ABS(x) > 1.E15 then
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
 --     dbms_output.put_line('res ' || result || ' region ' || round(region,10));
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
FUNCTION FASTER_ATAN2(YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER AS

/*
--******************************************************************************
--Program Name: faster_atan2
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
--              as fast as possible (13 times faster than the built in 
--              10G function atan2 which takes 25 seconds for 200000 values) with 
--              an accuracy of <4E-15. 
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
   pi        CONSTANT NUMBER    := 3.1415926535897932384626433832795028842;
   piByTwo   CONSTANT NUMBER    := 1.5707963267948966192313216916397514421;
   piBy6     CONSTANT NUMBER    := 0.5235987755982988730771072305465838140;
   piBy12    CONSTANT NUMBER    := 0.2617993877991494365385536152732919070167;
   tanpiby6  CONSTANT NUMBER    := 0.5773502691896257645091487805019574556545;
   tanpiby12 CONSTANT NUMBER    := 0.2679491924311227064725536584941276331;
   tanpiby24 CONSTANT NUMBER    := 0.1316524975873958534715264574097171035924;
   tanpiby10 CONSTANT NUMBER    := 0.3249196962329063261558714122151344649574;
   piby5     CONSTANT NUMBER    := 0.62831853071795864769252867665590057684;
   tanpiby5  CONSTANT NUMBER    := 0.7265425280053608858954667574806187496074;
   tanpiby32 CONSTANT NUMBER    := 0.3249196962329063261558714122151344649574;
   rad2deg   CONSTANT NUMBER    := 57.29577951308232087679815481410517033235;
   piby10    CONSTANT NUMBER    :=  0.31415926535897932384626433832795028842;
   x           NUMBER;
   xx          NUMBER;
   result      NUMBER;
   complement  BOOLEAN           := FALSE;
   region      NUMBER := 0;
   region2     NUMBER := 0;
   u           NUMBER;
--   c1          NUMBER             := 1.6867629106;
--   c2          NUMBER             := 0.4378497304;
--   c3          NUMBER             := 1.6867633134;
--   c1          NUMBER             := 1.6867877072231;
--   c2          NUMBER             := 0.43783372321953;
--   c3          NUMBER             := 1.68678767157921;
--  a3        NUMBER  := 0.3333333333333333333333333333333333333333;
--  a5        NUMBER  := 0.2;
--  a7        NUMBER  := 0.1428571428571428571428571428571428571429;
--  a9        NUMBER  := 0.1041666666666666666666666666666666666667;
--  x3        NUMBER;
-- Original code using c1,c2,c3
-- A very fast arctangent function more than 30 times faster than Oracle.
-- Maximum error is 6.25E-08 radians, relative 2.4E-07 radians.
-- I think this is sufficiently
-- accurate for most GIS applications!
--
-- Reference: Tim Wescott:
--            http://objectmix.com/dsp/212569-implementation-arctan-x-2.html
--  Jack Ganssle http://www.ganssle.com/approx/approx.pdf
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
        RETURN -pi * 0.5;
      end if;
    ELSE
      if degrees then
        RETURN 90.0;
      else
      RETURN pi * 0.5;
      end if;
    END IF;
  END IF;
/*
  IF ABS(XIN) = 1.0 and ABS(YIN) < 1.0 THEN
     x := ABS(YIN);
  ELSIF ABS(YIN) = 1.0 and abs(XIN) < 1.0 THEN
     x := ABS(XIN);
  ELSIF ABS(YIn) > ABS(XIn) THEN
     x   := ABS(XIn/YIn);
     complement := TRUE;
  ELSE
     x   := ABS(YIn/XIN);
  END IF;
 */

  IF ABS(YIn) > ABS(XIn) THEN
     complement := TRUE;
  END IF;
-- Try and avoid a division
  IF xin = 0.0 or yin = 0.0 then
    x := 0.;
  ELSIF ABS(XIN) < tanpiby12 * ABS(YIN) THEN
     x := ABS(YIN/XIN);
--     if x > 1.E24 then
--     result := region;
--     end if;
  ELSIF ABS(YIn) < tanpiby12 * ABS(XIn) THEN
     x   := ABS(XIn/YIn);
--     if x > 1.E24 then
--     result := region;
--     end if;
--     complement := TRUE;
  ELSIF ABS(XIN) > ABS(YIN) THEN
     xx   := ABS(YIn/XIN);

-- This range reduction is expensive although it reduces the error to about 0.0000007 radians!!
-- reduce arg to under tan(pi/12)
--  xx := x;
-- if (x > 0.5) THEN
--     x := (x-tanpiby5)/(1.0 +tanpiby5*x);
--     region := piby5;
--  elsif xx >= .155 then
--     x := (x-tanpiby10)/(1.0 +tanpiby10*x);
--     region := piby10;
--  end if;
  if (xx > tanpiby12) THEN
--     x := (x-tanpiby6)/(1.0 +tanpiby6*x);
     x := (1.0 +tanpiby6*xx)/ (xx-tanpiby6);
     region := piby6;
  elsif xx <> 0.0 then
      x := 1./xx;
  end if;
  ELSIF ABS(YIN) >= ABS(XIN) THEN
     xx   := ABS(XIn/YIN);
       if (xx > tanpiby12) THEN
--     x := (x-tanpiby6)/(1.0 +tanpiby6*x);
     x := (1.0 +tanpiby6*xx)/ (xx-tanpiby6);
     region := piby6;
   elsif xx <> 0.0 then
     x := 1./xx;
  end if;

  END IF;


--  This only reduces the Hart error from 6.25E-08 to 2.8E-08
-- reduce arg to under tan(pi/24)
--   if (x > tanpiby24) THEN
--     x := (x-tanpiby12)/(1.0 +tanpiby12*x);
--     region2 := 1.;
--  elsif  ( x < -tanpiby24) THEN
--     x := (x+tanpiby12)/(1.0 -tanpiby12*x);
--     region2 := -1.;
--  end if;

--  dbms_output.put_line('yin ' || yin || ' x ' || round(x,6) || ' xx ' || round(xx,6));
     if ABS(x) > 1.E15 then
      result := 1./x + region;
    else
--    if result is NULL then
      u := x*x;
-- Use Horner's rule to evaluate
---   result := (x*(c1 + u*c2))/(c3 + u) + region;
--     result := x*(55. + 105.*u)/(9.+u*(90. + 105*u)) + region;
--     result := x*(231. + u*(1190.+1155.*u))/ (25.+ u*(525.+ u*(1575. + 1155.*u))) + region;
     result := x*(3031.8 + u*(29491.+u*(69069.+45045*u)))/ (245.+ u*(8820.+ u*(48510. + u*(84084.+u*45045.))))
                 + region;
    end if;

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

END FASTER_ATAN2;
--
FUNCTION OLD_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER AS 

/*
--******************************************************************************
--Program Name: new_arctan
--Author: Sidey Timmins
--Creation Date: 07/00/2011
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has two parameters:
  --
  --   REQUIRED Parameters: 
  --            Y:  Y ordinate as per convention Y first!
  --            X:  X abscissa
  --
--Purpose:   -- Calculates the arctan function -  atan2(y,x) 
--              as fast as possible (12 times faster than the built in 
--              function atan2 which takes 4 seconds for 20000 values) with 
--              an accuracy of < 2E-27.
--              The domain is unlimited. (Note this function gives no error
--              and returns zero when both x and y are zero.)
--
-- Method:      First the range is reduced and then the arctangent is computed 
--              using a Taylor series.
--         The result is then adjusted for the quadrant.

-- Accuracy:  The maximum error in the range - -infinity to infinity is < 2E-27
--                             
-- Reference: Ulrich H Kurzweg
--   http://www.mae.ufl.edu/~uhk/MORE-EVALUATION-ARCTAN.pdf
--Dependencies: None but see try_faster_atan2 for rigorous testing.
--******************************************************************************
*/
 
   pi        NUMBER             := 3.1415926535897932384626433832795028842;
   piByTwo   NUMBER             := 1.5707963267948966192313216916397514421;

   piBy6     NUMBER             := 0.5235987755982988730771072305465838140;
--   piBy12    CONSTANT NUMBER   := 0.2617993877991494365385536152732919070164;
--   piby24    CONSTANT NUMBER  := 0.1308996938995747182692768076366459535082;
   tanpiby6  CONSTANT NUMBER   := 0.5773502691896257645091487805019574556476;
--   tanpiby12 NUMBER             := 0.2679491924311227064725536584941276331;
--   tanpiby24 CONSTANT NUMBER   := 0.1316524975873958534715264574097171035924;
   rad2deg   NUMBER;

   x           NUMBER; 
   xx          NUMBER;
   result     NUMBER;
   complement  BOOLEAN           := FALSE;
   region      NUMBER := 0.;
   u           NUMBER;
 

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
        RETURN -pi * 0.5;
      end if;
    ELSE 
      if degrees then
        RETURN 90.;
      else
      RETURN pi * 0.5;
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
 xx := x;

   if x >=0.85 then
   if x >=0.95 then
      x := x-1.;
      if x <> 0.0 then
        x :=  (1.0 +xx)/x;
      end if;      
      region := region+ 0.7853981633974483096156608458198757210546;
   else
      x := x-0.9;
      if x <> 0.0 then
        x :=  (1.0 +xx*0.9)/x;
      end if;      
      region := region+ 0.7328151017865065916407920727342802519847;
   end if;
   elsif x >= 0.65 then
    if x>= 0.75 then
      x := x-0.8;
      if x <> 0.0 then
        x :=  (1.0 +xx*0.8)/x;
      end if;      
      region := region+ 0.674740942223552663056520973609813615077;
   else
      x := x-0.7;
      if x <> 0.0 then
        x :=  (1.0 +xx*0.7)/x;
      end if;      
      region := region+ 0.610725964389208616543758876490236093837;
    end if;
   elsif x >= 0.45 then
    if x>= 0.55 then
      x := x-0.6;
      if x <> 0.0 then
        x :=  (1.0 +xx*0.6)/x;
      end if;      
      region := region+ 0.5404195002705841554435783646085999101395;
   else
      x := x-0.5;
      if x <> 0.0 then
        x :=  (1.0 +xx*0.5)/x;
      end if;
      region := region+ 0.4636476090008061162142562314612144020295;
   end if;
   elsif x >= 0.25 then
   if x>= 0.35 then
      x := x-0.4;
      if x <> 0.0 then
        x :=  (1.0 +xx*0.4)/x;
      end if;      
      region := region+ 0.3805063771123648863035879168104337074528;
   else
      x := x-0.3;
      if x <> 0.0 then
        x :=  (1.0 +xx*0.3)/x;
      end if;      
      region := region+ 0.2914567944778670919956046214328911935013;
    end if;
   else
     if x>= 0.15 then
      x := x-0.2;
      if x <> 0.0 then
        x :=  (1.0 +xx*0.2)/x;
      end if;    
     region := region+ 0.1973955598498807583700497651947902934471;
    elsif x>= 0.05 then
      x := x-0.1;
      if x <> 0.0 then
        x :=  (1.0 +xx*0.1)/x;
      end if;    
     region := region+ 0.0996686524911620273784461198780206049018;
    elsif x <> 0.0 then
          x := 1./x;    
    end if;
   end if;
 
--   dbms_output.put_line('x now is ' || x);

    if yin <> 0.0 and ((ABS(YIn) >= ABS(XIn) and u <= 1.) or (ABS(YIn) < ABS(XIn) and u > 1.)) then
     complement := TRUE;
     end if;


     if ABS(x) > 1.E15 then
      result := 1./x + region;
    else

--     dbms_output.put_line('x ' || round(x,8) || ' yin ' || yin);
      u := x*x;
--      result := x*(55. + 105.*u)/(9.+u*(90. + 105*u)) + region;
      result := x*(231. + u*(1190.+1155.*u))/ (25.+ u*(525.+ u*(1575. + 1155.*u))) + region;
--        result := x*(3031.8 + u*(29491.+u*(69069.+45045*u)))/ (245.+ u*(8820.+ u*(48510. + u*(84084.+u*45045.))))
 --                + region;
--        result := x*(61567. + u*(962676. + u*(3960356.4 + u*(5921916. + u*2909907.))))/
--        (3969. + u*(218295. + u*(1891890. + u*(5675670. + u*(6891885. + u*2909907))))) + region;
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
     rad2deg := 57.29577951308232087679815481410517033240;
     result := result *rad2deg;
     if result < 0.0 then
       result := result + 360.;
     end if;
  End if;

  RETURN result;

END OLD_ARCTAN;
--
FUNCTION  ANGLE_DOTP(x1 NUMBER,y1 NUMBER,x0 NUMBER, y0 NUMBER, x2 NUMBER, y2 NUMBER) 
RETURN NUMBER  Deterministic IS


-- Calculate the angle from the dotproduct of 2 lines sharing a common vertex (x0,y0) --
-- the second pair of coordinates!
--                      +  (x1,y1)
--                     /
--                    /   
--        (x0,y0)     +----------------+ (x2,y2)

  rad2deg   CONSTANT NUMBER := 57.29577951308232087679815481410517033240;
  dot_p     NUMBER := NULL;

BEGIN

-- Calculate the dot product and take the arc cosine of the result
-- cos(angle) = (x2-x0) * (x1-x0) + (y2-y0) * (y1-y0)
--          ______________________________________________________
--         sqrt((x2-x0)^2 + (y2-y0)^2) * sqrt((x1-x0)^2 + (y1-y0)^2)

  dot_p := rad2deg*GZ_MATH.new_arccos(  ((x2-x0) * (x1-x0) + (y2-y0) * (y1-y0)) /
         (sqrt((x2-x0)**2. + (y2-y0)**2.) * sqrt((x1-x0)**2. + (y1-y0)**2.)));

  RETURN  dot_p;
END;
--
FUNCTION ORIENT2D (paX   NUMBER,  paY   NUMBER,
                      pbX   NUMBER,  pbY   NUMBER,
                      pcX   NUMBER,  pcY   NUMBER)
                                
            RETURN          NUMBER 
            Deterministic IS
/*
--******************************************************************************
--Program Name: mk_orient2D
--Author: Sidey Timmins
--Creation Date: 8/15/2006

--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has three required parameters:
  --
  --   REQUIRED Parameters:
  --      paX,paY           - a point (x,y)
  --      pbX,pbY           - a 2nd point (x,y)
  --      pcX,pcY           - a 3rd point (x,y)
-- Purpose:
  -- Return a positive value if the points a, b, and c occur
  --             in counterclockwise order; a negative value if they occur
  --             in clockwise order; and zero if they are collinear.  The
  --             result equals twice the signed area of the triangle defined 
  --             by the three points.
  
  -- This procedure calculates the determinant:
--         | ax  ay   1 |
--         | bx  by   1 |    =   | ax -cx    ay -cy |
--         | cx  cy   1 |        | bx -cx    by -cy |
--Dependencies: None
--References: Joatan Shewchuk: Adaptive Precision Floating-Point Arithmetic and 
--Fast Robust Geometric Predicates,
--  http://www.google.com/search?hl=en&source=hp&q=shewchuk++geoemtric+redicates&aq=f&aqi=m1&aql=&oq=
--Limits:
--******************************************************************************
*/

   ccwerrboundA  NUMBER:= 1.76324152623343126195310480583336851684E-38;
  
   detleft               NUMBER;
   detright              NUMBER;
   det                   NUMBER;
   detsum                NUMBER;
   errbound              NUMBER;

BEGIN

  detleft  := (paX - pcX) * (pbY - pcY);
  detright := (paY - pcY) * (pbX - pcX);
  det := detleft - detright;

  if (detleft > 0.0) THEN
     if (detright <= 0.0) THEN
        RETURN det;
     End If;
    detsum := detleft + detright;

  ElsIf (detleft < 0.0)  THEN
     if (detright >= 0.0) THEN
       RETURN det;
     End If;
    detsum := -detleft - detright;
  Else
    RETURN det;
  End If;

  errbound := detsum * ccwerrboundA;
  
  if ( (det >= errbound) or (-det >= errbound) or (det = 0.) ) THEN
     RETURN det;
  End If;

-- At this point we need to call this procedure which was never completed.
-- However, we could use factoring or exact arithmetic to computet the
-- determinant more exactly.
--  det := c_orient2d_adapt(pax, pAy, pbx, pby, pcx, pcy, detsum);

   
END ORIENT2D;
--
Function QUICK_SINCOS(Xin NUMBER,cosX IN OUT NOCOPY NUMBER,in_degrees VARCHAR2 default 'N') RETURN NUMBER Deterministic IS
/*
--******************************************************************************
--Program Name: quick_sincos
--Author: Sidey Timmins
--Creation Date: 04/18/11
--Updated: 11/23/2011 Different method from Kurzweg and Timmins's paper
--Updated: 04/26/11 Newer coefficients from Robin Green's paper - cannot find original ones!
--Usage:  result := quick_sincos(30.,cosx,'Y');
--
  -- This PL/SQL function has 2 required parameters:
  -- 
  --          Input
  --             x:    Input argument in radians or if in_degrees is set to 'Y'
  --                   then input argiment in degrees.
  --            in_degrees: 'Y' means input argument is in degrees.
  --          Output
  --          cosx:  contains approximate cosine(x) on exit.
  
-- Output:    returns:  approximate sine(x).

--Purpose: This function calculates the sine and cosine quickly and probably
--         accurately enough for most GIS purposes (< 1 pico degree). 
--         Minmax absolute error is <= 1.E-17
--         Maximum relative error is  (1 - quick_sin/sine);  4.5E-15 (at arg=1)
--         About 2 times faster than sin and cos = sqrt(1-sin*sin).

-- Reference: Ulrich H Kurzweg and Sidey P. Timmins "A New Analytic Method to
--            Approximate Trigonometric functions based upon Legendre Polynomials". 
--
-- Reference: Robin Green "Faster Math functions".
--            http://www.research.scea.com/gdc2003/fast-math-functions_p2.pdf
-- Tested:  From -4 to 4 radians with test_quick_sincos. Example:
--  arg Quick_sincos              sine      Difference
ii 0    Qsine 0             sin 0            diff    .000E+00
ii .1   Qsine .0998334168   sin .0998334166  diff  -1.000E-10
ii .2   Qsine .1986693314   sin .1986693308  diff  -6.000E-10
ii .3   Qsine .2955202077   sin .2955202067  diff  -1.100E-09
ii .4   Qsine .3894183426   sin .3894183423  diff  -3.000E-10
ii .5   Qsine .4794255371   sin .4794255386  diff   1.500E-09
ii .6   Qsine .5646424719   sin .5646424734  diff   1.500E-09
ii .7   Qsine .6442176893   sin .6442176872  diff  -2.100E-09
ii .8   Qsine .717356091    sin .7173560909  diff  -1.000E-10
ii .9   Qsine .7833269089   sin .7833269096  diff   7.000E-10
ii 1    Qsine .841470986    sin .8414709848  diff  -1.200E-09
ii 1.1  Qsine .8912073605   sin .8912073601  diff  -5.000E-10
ii 1.2  Qsine .9320390857   sin .932039086   diff   3.000E-10
ii 1.3  Qsine .9635581851   sin .9635581854  diff   3.000E-10
ii 1.4  Qsine .9854497299   sin .98544973    diff   1.000E-10

-- Called by: 
-- Dependencies: none
--******************************************************************************
*/ 
   deg2rad   CONSTANT NUMBER := 0.0174532925199432957692369076848861271344;  -- pi/180.
   twopi     CONSTANT NUMBER := 6.2831853071795864769252867665590057684;
   pi        CONSTANT NUMBER := 3.1415926535897932384626433832795028842;
   piby2     CONSTANT NUMBER := 1.5707963267948966192313216916397514421;
   piby4     CONSTANT NUMBER := 0.78539816339744830961566084581987572105;
   pi3by2    CONSTANT NUMBER := 4.7123889803846898576939650749192543263;
   piby12    CONSTANT NUMBER := 0.261799387799149436538553615273291907016;
   piby6     CONSTANT NUMBER := 0.5235987755982988730771072305465838140328;
   tan_piby6 CONSTANT NUMBER := 0.5773502691896257645091487805019574556476;
   root32    CONSTANT NUMBER := 0.8660254037844386467637231707529361834714; 
 
-- One has to be careful with Robin Green's pdf. There must be 2 versions
-- because the left set of coefficients is better than the right. The test
-- x>pi/4 is also critical since these coefficients are only for -1 ..0..1
--       From Faster Math Functions pdf        From Blue Slide pdf
--   s1        CONSTANT NUMBER := -0.166666546;  --      -0.166666686534881591796875;
--   s2        CONSTANT NUMBER := 0.00833216076; --       0.00833282412;
--   s3        CONSTANT NUMBER := -0.000195152832; --    -0.000195878418;
   x         NUMBER := Abs(Xin);
   xx        NUMBER;
   u         NUMBER;
   sinx      NUMBER;
   tanx      NUMBER;
   tx        NUMBER;
   tt        NUMBER;
   tx2       NUMBER;
   tx4       NUMBER;
   swap      NUMBER := 0.0;
Begin

-- Adjust to range -twopi to +twopi
   IF x > twopi THEN
     if in_degrees = 'Y' then
       xx := Mod(Xin,360.) *deg2rad;
     else
      xx := Mod(Xin,twopi);
     end if;
     x := Abs(xx);
   ELSE
     if in_degrees = 'Y' then
       xx := Xin*deg2rad;
     else
       xx := Xin;
     end if;
   END IF;

   If x > pi THEN
     x   := MOD(x,pi);
   END IF;
  
   IF x > piby2 THEN
     x := pi -x;
   END IF;
 
   IF x >piby4 THEN
      x := piby2 -x;
      swap := 1.0;
   END IF;
   
--   IF x > piby6 THEN
--     tan_a := tan_piby6;
--     x := x - piby6;
--   END IF;
   
-- Calculate an accurate approximation to the sin: max error of 2E-17 radians
--   x := x*1.27323954473516268615107010698011489627;  -- only necessary for the Hart/piby4
--   x := x*0.333333333333333333333333333333333333333333333;
--   x := x* .04;
   u := x*x;
-- This line deprecated - only about 1.E-9 radian accuracy
   sinx := x + x*(u*(-0.166666546 + u*(0.00833216076  -0.000195152832*u)));
--   sinx := x + x*(u*(-0.16666666666666602381875 + u*(0.00833333332990032  -0.0001984071815851*u)));
--   tx := sinx*sinx;
--   sinx := sinx*(5.+ tx*(-20. + tx*16.));
--   tx := sinx*sinx;
--   sinx := sinx*(5.+ tx*(-20. + tx*16.));
-- These formulae are from original research by Kurzweg and Timmins
-- 
-- They were created by substituing x -> x/3, x/9, x/28, x/81 etc
-- into the formulas for equation (4) in their paper and then multiplying by 3/3,
-- 9/9, 27/27, 81/81 etc as appropriate.

-- Example: substitute a/3 for a in T(2,a) =  945a - 105a^3 + a^5
--                                            ===================
--                                            945 -420a^2 + 15a^4
-- and we get:
--              (76545a -945a^3 + a^5)/ (229635 - 11340a^2 + 45a^4)

--    tanx := x*(99202320. +x2*(-34020.+x2))/(1785641760.+x2*(-2449440.+270.*x2));  -- 2 loops   
--    tanx := x*(43740.-x2)/(2361960.-324.*x2);  -- 3 loops
--   tanx := x*(4860.-x2)/(87480.-108.*x2);  -- 2 loops

-- This one approximately matches the speed of the Green approximation above
-- but has accuracy better than 1.E-16.

--   tanx := x*(1224720.+x2*(-3780.+x2))/(7348320. +x2*(-90720.+90.0*x2)); -- 1 LOOP !!
--    tanx := x*(1224720.+u*(-3780.+u))/(7348320. +u*(-90720.+90.*u)); -- 1 LOOP !!
-- we pulled a factor of 90 out of the denominator and then compensated in next 2 lines
--    tanx := x*(1224720.+u*(-3780.+u))/(81648. +u*(-1008.+ u)); -- 1 LOOP !!
--    sinx := tanx/(8100.+tanx*tanx);         -- convert tan(x/2) to sin(x) but drop 2*
--    sinx := sinx*(540.-23328000.*sinx*sinx);     -- where looping (if any) required- add *2 as needed
-- You can then "tune" these last coefficients if you wish for slight improvement
--   tanx := x*(1224720.+u*(-3780.+u))/(7348320. +u*(-90720.+89.99999999769*u)); -- 1 LOOP !!
--   tanx := x*(15120.+u*(-420. +u))/(30240. +u*(-3360. +u*30.));
--   sinx := tanx/(1.+tanx*tanx);         -- convert tan(x/2) to sin(x) but drop 2*
--   sinx := sinx*(6.-32.*sinx*sinx);     -- where looping (if any) required- add *2 as needed
--==============================================================================
-- But this one is simpler avoiding a division and has accuracy < 1E-17

--   sinx := x*(18600435. +u*(-229635.+u*243.))/(55801305. +u*(344452.5 + u*(1366.875 + u*(5.0625+ 
--                 u*(0.023437115145   +   u*0.000165799878)))));
--   sinx := sinx*(3.-4.*sinx*sinx);      
  -- This is Hart 3045 good to 1E-24 or so.
--    sinx := x*(0.7853981633974483096156601796 + u*(-0.080745512188280781706862804 + u*(.0024903945701927201571930285 +u*(-0.0000365762041821772220077958+u*(0.0000003133616890376649961942+u*(-0.0000000017572476730203082208+u*(0.69484525587260867E-11+u*(-0.204095582821973E-13+u*0.459117304906E-16)))))))); 

--==============================================================================
--   tanx :=  x*(0.1+(u*(31500.-u*14.))/(94500000.+u*(-420000.+150.*u)));
--     tanx :=  x*(135135.E6+u*(-17325.E4 + u*(37800. -u)))/(135135.E7+u*(-62370.E5+ u*(315.E4 -280*u)));
--   dbms_output.put_line('tanx ' || tanx);
--   tanx := x*(1500.-u)/(15000.-60*u);
--   tanx := x*(945.E4+u*(-10500. +u))/(945.E5 +u*(-420000 + u*150.));
--   sinx := tanx/(1.+tanx*tanx);
--   tx := sinx*sinx;
--   sinx := sinx*(10.+ tx*(-160. + tx*512.));
--    dbms_output.put_line('sinx ' || sinx);
   if sinx > 1. then
      sinx := 1.;
   end if;
   if swap <> 0.0 then
      cosx := sinx;
      sinx := sqrt(1.-sinx*sinx);
   else
      cosx := sqrt(1.-sinx*sinx);
   end if;

 -- Sine is an odd function
 
   IF (xx > pi and xx < twopi) or (xx < 0.0 and xx > -pi) THEN 
        sinx := -sinx;
   END IF;
      
 -- The Cosine is an even function  
 
   IF Abs(xx) > piby2 and Abs(xx) < (pi3by2) THEN
        cosx := -cosx;
   END IF;
     
   RETURN sinx;
  
End QUICK_SINCOS;
--
FUNCTION NEW_SINCOS(InX NUMBER, COSX IN OUT NOCOPY NUMBER) 
    RETURN NUMBER Deterministic IS
/*  
--****************************************************************************** 
--
--Program Name: sincos
--Author: Sidey Timmins
--Creation Date: 11/16/2006
--Revision Date: 11/28/06
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 2 parameters:
  --
  --   REQUIRED Parameters: 
  --            Input:            X:  X input value (in radians)
  --            Output:        cosx: will contain cosine of X on output.
  --
--Purpose:   -- Calculate simultaneously sin(x) and cos(x) - respectively
--              the sine and cosine functions with 38-39 digits of accuracy.
--              Slightly less accurate than library function and minor time advantage.
-- Accuracy: Over the range - two pi to two pi by 1/10000 the maximum errors are:

--
-- Reference: 
-- Dependencies: None but see c_test_sincos for rigorous testing.
--******************************************************************************
*/


  twopi     CONSTANT NUMBER     := 6.2831853071795864769252867665590057684;
  pi        CONSTANT NUMBER     := 3.1415926535897932384626433832795028842;
  piby4     CONSTANT NUMBER     := 0.78539816339744830961566084581987572105;
  piBy2     CONSTANT NUMBER     := 1.5707963267948966192313216916397514421;
  pi3by2    CONSTANT NUMBER     := 4.7123889803846898576939650749192543263;
  
  x         NUMBER := ABS(Inx);
  tan_a     NUMBER :=0.0;
  a         NUMBER;
  u         number;
  xx        NUMBER;
  sinx      number;
  tanx      number;
  t2        NUMBER;
  ta        NUMBER;
  rtan_a    NUMBER;
  r         NUMBER;
  swap      NUMBER := 0.0;
  
BEGIN

-- Adjust to range -twopi to +twopi

   IF x > twopi THEN
     xx := Mod(Inx,twopi);
     x := Abs(xx);
   ELSE
     xx := Inx;
   END IF;

  If x > pi THEN
    x   := MOD(x,pi);
  END IF;
  
  IF x > piBy2 THEN
     x := pi -x;
  END IF;

  IF x >piby4 THEN
      x := piby2 -x;
      swap := 1.0;
   END IF;
   
  a := x*0.5;
  
  if a > 0.71883 then
    tan_a := 1.0;
--    rtan_a := 1.;
    a := a - 0.785398163397448309615660845819875721049292349;  -- atan(1);
  elsif a > .558599315 then
    tan_a := 0.75;
--    rtan_a := 1.33333333333333333333333333333333333333333333;
    a := a - 0.643501108793284386802809228717322638041510591; -- atan(0.75)
  elsif a > 0.35877067 then
    tan_a := 0.5;
--    rtan_a := 2.;
    a := a - 0.463647609000806116214256231461214402028537054; -- atan(0.5)
  elsif a > 0.124354925 then
    tan_a := 0.25;
--    rtan_a := 4.;
    a := a - 0.244978663126864154172082481211275810914144098; -- atan(.25)
  end if;
/*
 if a > 0.46875 then
    tan_a := 0.5;
    a := a - 0.463647609000806116214256231461214402028537054; -- atan(0.5)
  elsif a > 0.40625 then
    tan_a := 0.4375;
    a := a - 0.412410441597387306899791289667126937046808192; -- atan(0.4375);
  elsif a > 0.34375  then
    tan_a := 0.375;
    a := a - 0.358770670270572220395920063926460499776975655; -- atan(0.375)
  elsif a > 0.28125 then
    tan_a := 0.3125;
    a := a-  0.302884868374971405560556094505558213291539411; -- atan(0.3125)
  elsif a > 0.21875 then
    tan_a := 0.25;
    a := a - 0.244978663126864154172082481211275810914144098; -- atan(.25)
  elsif a > 0.15625 then
    tan_a := 0.1875;
    a := a - 0.185347949995694764886025961228544644515266439; -- atan(0.1875)
  elsif a > 0.09375 then
    tan_a := 0.125;
    a := a - 0.124354994546761435031354849163871025573170192; -- atan(0.125)
  elsif a > 0.03125 then
    tan_a := 0.0625;
    a := a - 0.062418809995957348473979112985505113606273887; -- atan(0.0625);
  end if;
*/
--  dbms_output.put_line('a ' || a ||' tan ' || tan_a);
  u := a*a;
  tanx := a*(13749310575.+u*(-1964187225. + u*(64324260. + u*(-675675. +u*(2145.- u)))))/
  (13749310575. + u*(-6547290750. + u*(413513100. + u*(-7567560. + u*(45045. - 66.*u)))));
--               (33*(416645775 + u*(-198402750 + u*(12530700 + u*(-229320 + u*(1365 - 2*u))))));

--  r := tanx-tan_a;
--  tanx := tanx - r/(1.+x*0.5*tanx);
--  sinx := 2*tanx/(1.+tanx*tanx);
--    if tan_a = 0.0 then
    ta := 1.-tan_a*tanx;
    t2 := tan_a+tanx;
--    else
--     ta := rtan_a - tanx;
--     t2 := 1. + tanx*rtan_a;
--     end if;
    sinx := 2.*t2*ta/(ta*ta + t2*t2);
 
  IF (sinx > 1.0) THEN
   sinx := 1.0;
  END IF;
 
  if swap <> 0.0 then
      cosx := sinx;
      sinx := sqrt(1.-sinx*sinx);
   else
-- Calculate cosine
      cosx := sqrt(1.-sinx*sinx);
   end if;

 -- Sine is an odd function
 
  IF (xx > pi and xx < twopi) or (xx < 0.0 and xx > -pi) THEN 
      sinx := -sinx;
  END IF;
  
 -- The Cosine is an even function  
 
  IF Abs(xx) > piby2 and Abs(xx) < (pi3by2) THEN
      cosx := -cosx;
  END IF;
  
  RETURN sinx;
  
END NEW_SINCOS;
--
FUNCTION SINCOS(InX NUMBER, COSX IN OUT NOCOPY NUMBER) 
    RETURN NUMBER Deterministic IS
/*  
--****************************************************************************** 
--
--Program Name: sincos
--Author: Sidey Timmins
--Creation Date: 11/16/2006
--Revision Date: 11/28/06
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 2 parameters:
  --
  --   REQUIRED Parameters: 
  --            Input:            X:  X input value (in radians)
  --            Output:        cosx: will contain cosine of X on output.
  --
--Purpose:   -- Calculate simultaneously sin(x) and cos(x) -
--              the sine and cosine functions as fast as possible (3 times faster 
--              than the built in functions that take 1 second for 20000 sine
--              and cosine values) with about 19 digits of accuracy. Worst is
--              15 digits of accuracy near zero and pi.
--
-- Accuracy: Over the range - two pi to two pi by 1/10000 the maximum errors are:
--           sine       -.001884954475928113753023025004501920291813 correct
--           sinx       -.001884954475928117129897194923190991656619 very close
--           error       .000000000000000003376874169918689071364806

--           cosine      .8846460864518815658418479233550142026277  correct
--           cosx        .8846460864518815658354808691145121305329  very close
--           error       .0000000000000000000063670542405020720948
--    Near pi
--           sine       -.00150796390221547396236390642315296548 
--           sinx       -.001507963902215478183896385939205188475162
--           cosine      -.99999886302178844781346870679287144982
--           cosx        -.999998863021788447807102780963812085428

--
-- Reference: http://cache-www.intel.com/cd/00/00/29/37/293747_293747.pdf
--     ?originalurl=http://www.intel.com/cd/ids/developer/asmo-na/eng/293747.htm
--  The title of the pdf is:
-- "Slerping Clock Cycles" JMP van Waveren, Id Software, Inc
-- For cosine code
-- http://www.gansle.com/approx/approx.pdf
--            "A Guide to Approximations" by Jack G Ganssle

-- Updates: with better approximations for cosine on 11/22 
--          allowed range to be the real number line

-- Dependencies: None but see c_test_sincos for rigorous testing.
--******************************************************************************
*/
 
  X         NUMBER             := Abs(InX);
  
  twopi     CONSTANT NUMBER             := 6.2831853071795864769252867665590057684;
  pi        CONSTANT NUMBER             := 3.1415926535897932384626433832795028842;                                 
  piByTwo   CONSTANT NUMBER             := 1.5707963267948966192313216916397514421;
  pi3by2    CONSTANT NUMBER             := 4.7123889803846898576939650749192543263;
 
  -- these coefficients are optimized to give accurate results throughout
  -- the range [-2pi to 2pi]
    c1        CONSTANT NUMBER             := 0.9999999999999999999936329;
    c2        CONSTANT NUMBER             :=-0.49999999999999999948362843;
    c3        CONSTANT NUMBER             := 0.04166666666666665975670054;
    c4        CONSTANT NUMBER             :=-0.00138888888888885302082298;
    c5        CONSTANT NUMBER             := 0.000024801587301492746422297;
    c6        CONSTANT NUMBER             :=-0.00000027557319209666748555;
    c7        CONSTANT NUMBER             := 0.0000000020876755667423458605;
    c8        CONSTANT NUMBER             :=-0.0000000000114706701991777771;
    c9        CONSTANT NUMBER             := 0.0000000000000477687298095717;
    c10       CONSTANT NUMBER             :=-0.00000000000000015119893746887;

  x2          NUMBER;
  xx          NUMBER;
  sinx        NUMBER;
  
  
BEGIN

  
-- Adjust to range -twopi to +twopi
   IF x > twopi THEN
     xx := Mod(Inx,twopi);
     x := Abs(xx);
   ELSE
     xx := Inx;
   END IF;

  If x > pi THEN
    x   := MOD(x,pi);
  END IF;
  
  IF x > piByTwo THEN
     x := pi -x;
  END IF;

  x2 := x * x;
  
  -- This approximation is better for small angles (ist reference) and gets
  -- a zero argument right
-- near zero 
--sine   error .000000000000000003376874169918689071364806
--cosine error .0000000000000000000063670542405020720948 
-- near pi
--sine   error .000000000000000004221532479516052222995162
--cosine error .0000000000000000000063659258290593643911

   IF (x < 1.5E-3 ) THEN
   
 -- Use Horner's rule to evaluate the sine
  
    sinx :=  x*(x2*(x2*(x2*(x2 *(-2.39E-08 * x2 + 2.7526E-06) - 1.98409E-04) + 8.3333315E-03)
           -1.666666664E-01) + 1.0); 
 
     IF (sinx > 1.0) THEN
       sinx := 1.0;
     END IF;

-- Calculate cosine
     cosx := sqrt(1.0 - sinx*sinx);
    
  ELSE 
 -- Use Horner's Rule to evaluate the cosine (2nd reference)
 
   cosx   := c1 + x2*(c2 + x2*(c3 + x2*(c4 + x2*(c5 + x2*(c6 +x2*(c7 + x2*(c8 + 
              x2*(c9 + x2*c10))))))));

    IF cosx > 1.0 THEN
      cosx := 1.0;
    END IF;
  
-- Calculate sine. This could be done another way (sine = cosine(pi/2 - x)
-- but sqrt is very fast.

    sinx := sqrt(1.0 - cosx*cosx);
   
  END IF;
 
 -- Sine is an odd function
 
  IF (xx > pi and xx < twopi) or (xx < 0.0 and xx > -pi) THEN 
      sinx := -sinx;
  END IF;
  
 -- The Cosine is an even function  
 
  IF Abs(xx) > pibyTwo and Abs(xx) < (pi3by2) THEN
      cosx := -cosx;
  END IF;
  
  RETURN sinx;
  
END SINCOS;
--
FUNCTION NEW_SIN(InX NUMBER) RETURN NUMBER Deterministic IS
  cos    NUMBER;
  sin    NUMBER;
  
BEGIN
  sin := new_sincos(Inx,cos);
  RETURN sin;
END;
--
FUNCTION FAST_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER AS 
/*
--******************************************************************************
--Program Name: fast_arctan
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

-- Accuracy:  The maximum error in the range - -infinity to infinity is < 1.E-17 ??
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

   Function Reduce( a NUMBER, step NUMBER) Return NUMBER as
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


 
--   dbms_output.put_line('x now is ' || to_char(x,'9999EEEE') || ' reg ' || region);

    if yin <> 0.0 and ((ABS(YIn) >= ABS(XIn) and u <= 1.) or (ABS(YIn) < ABS(XIn) and u > 1.)) then
     complement := TRUE;
     end if;


     if ABS(x) > 1.E15 then
      result := 1./x + region;
    else

--     dbms_output.put_line('x ' || round(x,8) || ' yin ' || yin);
      u := x*x;
      
--      result := x*(55. + 105.*u)/(9.+u*(90. + 105*u)) + region;
      result := x*(.5238095238095238095238095238095238095238 + u)/
                 (.0857142857142857142857142857142857142857 + u*(.857142857142857142857142857142857142857 +u)) + region;
--      result := x*(231. + u*(1190.+1155.*u))/ (25.+ u*(525.+ u*(1575. + 1155.*u))) + region;
--        result := x*(3031.8 + u*(29491.+u*(69069.+45045*u)))/ (245.+ u*(8820.+ u*(48510. + u*(84084.+u*45045.))))
--        result := x*1./35.*(15159. + u*(147455.+u*(345345.+225225.*u)))/ (35.+ u*(1260.+ u*(6930. + u*(12012.+u*6435.))))
--                 + region;
--        result := x*(61567. + u*(962676. + u*(3960356.4 + u*(5921916. + u*2909907.))))/
--        (3969. + u*(218295. + u*(1891890. + u*(5675670. + u*(6891885. + u*2909907))))) + region;
 --        result := 11.*x*(27985.+ u*(437580. + u*(1800162. + u*(2691780. + u*1322685.))))/
--            (315.*(63.+u*(3465.+u*(30030.+u*(90090.+u*(109395.+46189.*u)))))) + region;
      end if;
 --     dbms_output.put_line('res ' || result || ' region ' || round(region,10));
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

END FAST_ARCTAN;
--
FUNCTION NEW_TAN(InX NUMBER) RETURN NUMBER Deterministic IS
/*  
--****************************************************************************** 
--
--Program Name: new_tan
--Author: Sidey Timmins
--Creation Date: 11/23/2011
--Revision Date: 
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 1 parameter:
  --
  --   REQUIRED Input Parameter:          
  --            X:  X input value (in radians)
  --
--Purpose:   -- Calculate tan(x)-
--              
--
-- Accuracy: Over the range - two pi to two pi typical accuracy < 1.E-30
-- IN some cases it surpasses the accuracy of the library function. It is
-- about twice as fast.
--
-- Reference: 
-- Dependencies: 
--******************************************************************************
*/


  twopi     CONSTANT NUMBER     := 6.2831853071795864769252867665590057684;
  pi        CONSTANT NUMBER     := 3.1415926535897932384626433832795028842;                                 
  piBy2     CONSTANT NUMBER     := 1.5707963267948966192313216916397514421;
  pi3by2    CONSTANT NUMBER     := 4.7123889803846898576939650749192543263;
  piBy4     CONSTANT NUMBER     := 0.78539816339744830961566084581987572105;
  recip_piby4 CONSTANT NUMBER   := 1.273239544735162686151070106980114896275;
  x         NUMBER := Inx;
  tan_a     NUMBER :=0.0;
  a         NUMBER;
  u         number;
  xx        NUMBER;
  tanx      number;

  octant    INTEGER;
  big       CONSTANT NUMBER := 1.E120;
  diff      NUMBER;
  
BEGIN

  if abs(x) >= twopi then          
   x := mod(x,twopi);
  end if;
 
  if x < 0.0 then
    x := x + twopi;
  end if;

  octant := TRUNC(x * recip_piby4); --trunc(x/piby4);
 
  if x > pi then
    diff := piby2 - (x-pi); --mod(x,pi);
  else
    diff := piby2 -x;
  end if;
 

  if abs(diff) < 1.E-30  then

-- Should return infinity but this will have to do..
     If diff >= 0.0 and octant <> 6 then 
       RETURN big; 
     Else
       RETURN -big;
     End if;
  end if;
  
  if octant = 0 then
    NULL;
  elsif octant = 1 then
    x := piby2 - x;
  elsif octant = 2 then
    x := x - piby2;
  elsif octant = 3 then
    x := pi - x;
  elsif octant = 4 then
    x := x - pi;
  elsif octant = 5 then
    x := pi3by2 -x;
  elsif octant = 6 then
    x := x - pi3by2;
  elsif octant = 7 then
    x := twopi - x;
  end if;
  
  a := x*0.5;
  
 
  if a > 0.71883 then
    tan_a := 1.0;
    a := a - 0.785398163397448309615660845819875721049292349;  -- atan(1);
  elsif a > .558599315 then
    tan_a := 0.75;
    a := a - 0.643501108793284386802809228717322638041510591; -- atan(0.75)
  elsif a > 0.35877067 then
    tan_a := 0.5;
    a := a - 0.463647609000806116214256231461214402028537054; -- atan(0.5)
  elsif a > 0.124354925 then
    tan_a := 0.25;
    a := a - 0.244978663126864154172082481211275810914144098; -- atan(.25)
  end if;

--  dbms_output.put_line('a ' || a ||' tan ' || tan_a || ' oct ' || octant);
  u := a*a;
  tanx := a*(13749310575.+u*(-1964187225. + u*(64324260. + u*(-675675. +u*(2145.- u)))))/
               (33.*(416645775. + u*(-198402750. + u*(12530700. + u*(-229320 + u*(1365. - 2.*u))))));
  
  tanx := (tan_a+tanx)/(1.-tan_a*tanx);
  tanx := 2.*tanx/(1.-tanx*tanx);

  octant := mod(octant,4);
  IF tanx <> 0.0 and (octant = 1 or octant = 2) then
    tanx := 1./tanx;
  END IF;
 
 -- tanx is an odd function
  
  IF octant > 1 then
    tanx := -tanx;
  END IF;
  
  RETURN tanx;
  
END NEW_TAN;
--
FUNCTION LADDERF(nsteps integer default 10, desired_argument number default NULL) RETURN NUMBER AS

-- A reference arctangent function to calculate either 
--            1) a ladder of 10 atans at 10 steps (depends upon nsteps)
--         or 2) iteratively determine an arbitrary arctangent
--
-- Values can be checked at a big number calculator:
--              http://www.alpertron.com.ar/BIGCALC.HTM
--
-- We are using this as an absolute arctangent reference function since its
-- accuracy probably exceeds 40 digits.

--Procedure Ladder ( step_count default 10,  user_argument default zero ) generates arctangents by iterating towards either a series of equally spaced arguments (steps) or an arbitrary user argument in the open interval (0, 1).  It starts from 1 (at the top) and iterates downwards to the next step or the user argument and prints the result. For simplicity the method here does not incorporate arctan(1/a) = aF(a) below half the bottom step, limit n when p is large or set the result to the user argument  near zero. The tolerance used to check proximity to the next step (1.E-35) should be set to 1.125E-16 for IEEE double precision.

--Step 1: Initialization
--a)	Set our position a to 1 and call its arctangent, À/4, the previous_result.
--b)	If user argument is zero, build a ladder of steps.  Set the spacing to 1/step_count, the  
--      next_step  =  (a  spacing/2) , and bottom_step = next_step (step_count-1)*spacing.
--
--Else for the user argument, set spacing to 0.1 and next and bottom steps to the argument itself.
--c)	Setup a microstep to use between steps, Da,and ensure Da <= min(spacing/10.,0.01).
--d)	Save original_Da = Da and set the gap from the current position to the next step as (a  next_step).

--Step 2: Iteration
--                      Loop while a greater than or equal to the bottom step
--                              Set last_a = a, decrement a by Da and setup p = (a*last_a+1)/ D a  if Da is not zero, else p=0.
--                           Calculate an intermediary or final result:  result = previous_result  pF(p).
--                           Example:  at loop 1, from a = 1, Da = 0.01 and p = 199 we compute
--                                            arctan(0.99) = arctan(1) - arctan(0.01/(1(0.99)+1) @ À/4  199F(199)
--
--                              Test: Is our position a close enough to the next step?
--                                       No: If  (a  next_step) <= gap and gap > 1.E-35 then reset gap to ABS(a-next-step) and
--                           ensure Da <=gap, save best result = result.  
--                                      Yes: Else  print next_step and best_result
--                                                 Reset  Da to the original_Da, set the next step to (next_step  spacing) and gap to (a-next_step).                                      
--                              Always save previous_result = result.
--                       End Loop;   

  a               NUMBER;
  u               NUMBER;
  last_a          NUMBER;
  delta_a         NUMBER;
  orig_delta_a    NUMBER;
  previous_arctan NUMBER;
  spacing         NUMBER;
  next_step       NUMBER;
  bottom_step     NUMBER;
  p               NUMBER;
  small_p         NUMBER := 10000000.;
  large_p         NUMBER :=1.;
  F               NUMBER;
  gap             NUMBER;
  result         NUMBER;
  best_result     NUMBER;
BEGIN 
  --Beginning at arctan(1) := pi/4  
  --Step towards various goals (0.95,0.85, 0.75,..0.05) using equation (20) as follows:

  --     arctan(a+Da) := arctan(a) + p*F(p) where p := ((a(a+ delta_a)+1)/ delta_a    

  -- call these: result  := previous arctan   + F term
  
   a := 1.; 
   previous_arctan := 0.785398163397448309615660845819875721049292349843;								-- a is our position on the ladder
   best_result := previous_arctan;
   
   if desired_argument is NULL then				-- Build a ladder with
     spacing := 1./nsteps; -- 0.09765625*0.5; --1.0/nsteps;									-- spacing of 0.1 for 10 steps
     next_step := a  - spacing*0.5;				-- first find arctan at top step:=0.95
     bottom_step := next_step - (nsteps-1)*spacing;  		-- where to stop, 0.05 for 10 steps
   else
     if desired_argument = 0 then
       RETURN 0.0;
     end if;
     spacing := 0.1;
     next_step := desired_argument;				-- or for some specified argument
     bottom_step := next_step;
     
   end if;

   gap := a - next_step;

   delta_a := 0.01;
 
   if delta_a > spacing*0.1 then
     delta_a := spacing*0.1;					-- set small delta to add to a
   end if;
   if delta_a > gap * 0.1 then   
     delta_a := gap*0.1;
   end if;
   orig_delta_a := delta_a;
                                                                -- distance to next step
--   dbms_output.put_line('a ' || a || ' gap ' || gap || ' D ' || delta_a || ' desied step ' || desired_argument);
   While a >= bottom_step Loop								-- Loop until position a goes past bottom step
      last_a := a;
      a := a - delta_a;						-- decrement a directly to 0.99 on 1st loop
      if delta_a <> 0.0 then
        p := (a * last_a+1)/delta_a;				-- setup argument for F term
        if p > large_p then
          large_p := p;
        end if;
      else
        p := 0.0;
      end if;
      u := p*p;
      If p >= 1E8 then
        F := 3*p/(1.+3*u);
 --   elsIf p >= 1E9 then
 --     F := 5.*p/(11.+21.*u)/(3.*(3.+30.*u+34.*u));
      else
   -- At loop 1, this is the computation:  a:=1, Da := 0.01 and p := 199

   -- arctan(0.99) := arctan(1) - arctan(0.01/(1(0.99)+1) @  À/4  199F(199,n:=4)
 -- This is order n=4      
 --     F := p *1./35.*(15159. + u*(147455.+u*(345345.+225225.*u)))/ (35.+ u*(1260.+ u*(6930. + u*(12012.+u*6435.))));
 -- and this is n=5
 
        F := 11.*p*(27985+ u*(437580. + u*(1800162. + u*(2691780. + u*1322685.))))/
             (315*(63.+u*(3465.+u*(30030.+u*(90090.+u*(109395.+46189.*u))))));
      end if;
      result := previous_arctan - F;				-- F function is equation 9, n :=4
      if p < small_p and p <> 0 then
          small_p := p;
      end if;-- except when p > 1E9, use n :=1
     -- Are we close enough to our goal?
   
      if a - next_step <= gap and gap > 1.E-35 then	        -- Use 1.125E-16 for double precision 
          gap := ABS(a - next_step);				-- keep track of distance to goal
          if gap < delta_a then
            delta_a := gap;					-- make Da smaller as we get close
          end if;
          best_result := result;				-- save our best result before next goal
          
     -- When we get close enough, print best result 
      else
  --         dbms_output.put_line('next step ' || next_step || ' atan ' || best_result);								-- 0.95, .7597627548757.. for 1st goal
  
           next_step := next_step - spacing;			-- reset next goal to 0.85, 0.75 ..
           gap := 1.;						-- reset gap to find next step
           delta_a := orig_delta_a;
  --         dbms_output.put_line('small p' || small_p || ' large_p ' || large_p);
            return best_result;-- and delta to a larger value
      end if;

    previous_arctan := result;					-- recursively reuse result in next loop

   End Loop;

   RETURN result;
   End Ladderf;
--
-- =============================================================================
--
--                      Unit TESTING !!
--
PROCEDURE TRY_QUICK_SINCOS AS

  rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
  x      NUMBER;
  big    NUMBER :=0.0;
  bigrel NUMBER := 0.0;
  bad    PLS_INTEGER;
  badrel PLS_INTEGER;
  cosx   NUMBER;
  sinx   NUMBER;
  time1      date;
  ii     PLS_INTEGER :=1;
--==============================================================================
    Function RPAD(input NUMBER,width NUMBER) RETURN VARCHAR2 AS
-- Right pad input with blanks (if necessary) so it has a certain width
   output   VARCHAR2(100) := SUBSTR(input,1,width);
   n        PLS_INTEGER;
   Begin
   if length(input) <> width then
      n := width - length(input);
      output := output || SUBSTR('                                          ',1,n);
   end if;
   Return output;
   end;
--==============================================================================
--==============================================================================
   Function RPADV(input VARCHAR2,width NUMBER) RETURN VARCHAR2 AS
-- Right pad input with blanks (if necessary) so it has a certain width
   output   VARCHAR2(100) := input;
   n        PLS_INTEGER;
   Begin
   if length(input) < width then
      n := width - length(input);
      output := output || SUBSTR('                                          ',1,n);
   end if;
   Return output;
   end;
--==============================================================================
BEGIN

 time1 := current_timestamp;
 for ii in 0..1000000 loop
    x := ii*0.000001;
    sinx := quick_sincos(x,cosx);
 end loop;
dbms_output.put_line((current_timestamp-time1));

  for ii in -315..315 loop
       if ii = -126 then
          x := -4.*acos(-1.);
       elsif ii = 126 then
          x := 4*acos(-1.);
       else
          x := ii/100.;
       end if;
       sinx := quick_sincos(x,cosx);

--      sinx := gz_util_zone.sincos(x,cosx);

      if abs(sinx- sin(x)) > big then
        big := abs(sinx-sin(x));
        bad := ii;
     end if;

      if sin(x) <> 0. and abs(1. -sinx/sin(x)) > big then
        bigrel := abs(1. -sinx/sin(x));
        badrel := ii;
     end if;
--     if sin(x) <> 0. then
--     dbms_output.put_line('angle ' || RPAD(ROUND(x*rad2deg,1),6) ||' ii '  || Rpad(x,4) || ' Qsine ' || RPAD(round(sinx,15),15) || '  sin ' || RPAD(round(sin(x),15),15)  || ' diff ' || RPADV(TO_CHAR(round(1.-sinx/sin(x),22),'99.999EEEE'),23));
--     end if;
 end loop;
 dbms_output.put_line('biggest absolute error' || RPADV(To_char(round(big,22),'999.99999EEEE'),24) || ' bad ' || bad);
 dbms_output.put_line('biggest relative error' || RPADV(To_char(round(bigrel,22),'999.99999EEEE'),24) || ' bad ' || badrel);
 dbms_output.put_line((current_timestamp-time1));

END TRY_QUICK_SINCOS;
--
PROCEDURE TRY_NEW_ARCTAN AS 
  x    NUMBER;
   x0   NUMBER := -81.57512700000000904765329323709011077881;
  y0  NUMBER := 28.54515100000000416002876590937376022339;
  x1  NUMBER :=-81.57513600000001474654709454625844955444;
  y1  NUMBER := 28.54515100000000416002876590937376022339;
  big NUMBER :=0.0;

  bad PLS_INTEGER;

  time1      date;

BEGIN
 ---abc
 time1 := current_timestamp;
-- a.extend(2);
-- a(1) := 1.;
-- a(2) := 200.;
-- b :=a;
-- c := 1./0.0000001 + 3;
-- dbms_output.put_line('b1 ' || b(1) || ' b2 ' ||b(2) || 'c ' ||c);
--   x := new_arctan(0.5773502691896257645091487805019574556545,1.);
--   x := new_arctan(0.5773502691996257645091487805019574556545,1.);
 --  dbms_output.put_line('x ' || x);
--   dbms_output.put_line('A ' || atan2(0.5773502691896257645091487805019574556545,1.));
--   x := new_arctan(0.2679491924311227064725536584941276331,1.);
--   dbms_output.put_line('x ' || x );
--   dbms_output.put_line('A ' || atan2(0.2679491924311227064725536584941276331,1.));
 for ii in 0..100000 loop 

       x0 := ii/100000.;

--      X := GZ_UTIL_ZONE.fast_ATAN2(x0,1.);
 
       X := GZ_MATH.faster_atan2(x0,1.);


--     x := atan2(x0,1.);
 
      if abs(x-ladderf(10,x0)) > big then
        big := abs(x- ladderf(10,x0));
        bad := ii;
      end if;
      

--     dbms_output.put_line('ii '  || (ii/1000.) || ' uhk ' || x   || ' diff ' || to_char(round(x-ladderf(10,x0),40),'999.99999EEEE')); -- || ' diff ' || round((atan2(ii/100.,1.)-x)/(ii*ii),10));
 
 end loop;
 dbms_output.put_line('biggest ' || to_char(big,'999.99999EEEE') || ' bad  ' || bad);
 dbms_output.put_line((current_timestamp-time1));
END TRY_NEW_ARCTAN;

END GZ_MATH;
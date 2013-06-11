CREATE OR REPLACE
PACKAGE "GZ_MATH"  AUTHID CURRENT_USER  AS

FUNCTION ASINH(x NUMBER) RETURN NUMBER;
FUNCTION ATANH(x NUMBER) RETURN NUMBER;
FUNCTION EXPM1(x NUMBER) RETURN NUMBER;
FUNCTION HYPOT(x NUMBER,y NUMBER) RETURN NUMBER;
FUNCTION LOG1P(x NUMBER) RETURN NUMBER;
FUNCTION  ANGLE_DOTP(x1 NUMBER,y1 NUMBER,x0 NUMBER, y0 NUMBER, x2 NUMBER, y2 NUMBER) 
RETURN NUMBER  Deterministic;

-- Derived from new_arctan and checked with: http://www.alpertron.com.ar/BIGCALC.HTM
FUNCTION NEW_ARCCOS(XIn NUMBER) RETURN NUMBER Deterministic;
FUNCTION NEW_ARCSIN(XIn NUMBER) RETURN NUMBER Deterministic; 
-- Calculates artangents to 38 to 40 digits faster than the system library function atan2
FUNCTION NEW_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER;
FUNCTION OLD_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER;
-- Absolute reference arctangent function
FUNCTION   LADDERF(nsteps integer default 10, desired_argument number default NULL) RETURN NUMBER;
-- Calculates the clockwisesness of 3 points
FUNCTION ORIENT2D (paX   NUMBER,  paY   NUMBER,
                      pbX   NUMBER,  pbY   NUMBER,
                      pcX   NUMBER,  pcY   NUMBER)
                                
            RETURN          NUMBER 
            Deterministic;
-- Calculate sine and cosine to about ? digits            
Function QUICK_SINCOS(Xin NUMBER,cosX IN OUT NOCOPY NUMBER,in_degrees VARCHAR2 default 'N') RETURN NUMBER Deterministic;

FUNCTION NEW_SIN(InX NUMBER) RETURN NUMBER Deterministic;

-- Calculate sine and cosine to about 19 digits
FUNCTION NEW_SINCOS(InX NUMBER, COSX IN OUT NOCOPY NUMBER) RETURN NUMBER Deterministic;

FUNCTION NEW_TAN(InX NUMBER) RETURN NUMBER Deterministic;
FUNCTION SINCOS(InX NUMBER, COSX IN OUT NOCOPY NUMBER) RETURN NUMBER Deterministic;

-- Utilities

-- Unit Testing
PROCEDURE TRY_QUICK_SINCOS;
PROCEDURE TRY_NEW_ARCTAN;
END GZ_MATH;
/

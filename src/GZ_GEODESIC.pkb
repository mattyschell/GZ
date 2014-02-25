create or replace
PACKAGE BODY GZ_GEODESIC AS

-- Translated from Charles Karney's Geographic Lib 1.32
-- http:// geographiclib.sourceforge.net
-- Blocked by the US goverment but comes with excellent documentation,
-- examples and theoretical/practical papers.
-- Updated:  10/2013 m12x was calculated correctly and all Ms now passed to
--           enable Gnomonic projection
--           07/2013 to use new_arctan (copied from GZ_MATH).
 -- Translated: Jul31,Aug1  2013 by Sidey Timmins from Code in C++ written by Charles Karney.
 --             Most problems in translation revolved around the C idiom for setting
 --            variables based upon a test: example
 --            return x >= 180 ? x - 360 : x < -180 ? x + 360 : x;
 
/*
 * file Geodesic.cpp
 * brief Implementation for GeographicLib::Geodesic class
 *
 * Copyright (c) Charles Karney (2009-2013) <charles@karney.com> and licensed
 * under the MIT/X11 License.  For more information, see
 * http:--geographiclib.sourceforge.net/
 *
 * This is a reformulation of the geodesic problem.  The notation is as
 * follows:
 * - at a general point (no suffix or 1 or 2 as suffix)
 *   - phi := latitude
 *   - beta := latitude on auxiliary sphere
 *   - omega := longitude on auxiliary sphere
 *   - lambda := longitude
 *   - alpha := azimuth of great circle
 *   - sigma := arc length along great circle
 *   - s := distance
 *   - tau := scaled distance (= sigma at multiples of pi/2)
 * - at northwards equator crossing
 *   - beta := phi := 0
 *   - omega := lambda := 0
 *   - alpha := alpha0
 *   - sigma := s := 0
 * - a 12 suffix means a difference, e.g., s12 := s2 - s1.
 * - s and c prefixes mean sin and cos
 **********************************************************************

--#include <GeographicLib/Geodesic.hpp>
--#include <GeographicLib/GeodesicLine.hpp>

--#if defined(_MSC_VER)
-- Squelch warnings about potentially uninitialized local variables
--#  pragma warning (disable: 4701)
--#endif

--namespace GeographicLib {

--  using namespace std;

  -- Underflow guard.  We require
  --   tiny_ * epsilon() > 0
  --   tiny_ + epsilon() == epsilon()
  --const Math::real Geodesic::tiny_ := sqrt(numeric_limits<real>::min());
  --const Math::real Geodesic::tol0_ := numeric_limits<real>::epsilon();
  -- Increase multiplier in defn of tol1_ from 100 to 200 to fix inverse WHEN
  -- 52.784459512564 0 -52.784459512563990912 179.634407464943777557
  -- which otherwise failed for Visual Studio 10 (Release and Debug)
  
  
  const Math::real Geodesic::tol1_ := 200 * tol0_;
  const Math::real Geodesic::tol2_ := sqrt(tol0_);
  -- Check on bisection interval
  const Math::real Geodesic::tolb_ := tol0_ * tol2_;
  const Math::real Geodesic::xthresh_ := 1000 * tol2_;

  Geodesic::Geodesic(real a, real f)
    : _a(a)
    , _f(f <= 1 ? f : 1/f)
    , _f1(1 - _f)
    , _e2(_f * (2 - _f))
    , _ep2(_e2 / Math::sq(_f1))       -- e2 / (1 - e2)
    , g_n(_f / ( 2 - _f))
    , _b(_a * _f1)
    , _c2((Math::sq(_a) + Math::sq(_b) *
           (_e2 == 0 ? 1 :
            (_e2 > 0 ? Math::atanh(sqrt(_e2)) : atan(sqrt(-_e2))) /
            sqrt(abs(_e2))))/2) -- authalic radius squared
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
    , _etol2(0.1 * tol2_ /
             sqrt( max(real(0.001), abs(_f)) * min(real(1), 1 - _f/2) / 2 ))
  {
    if (!(Math::isfinite(_a) && _a > 0))
      throw GeographicErr("Major radius is not positive");
    if (!(Math::isfinite(_b) && _b > 0))
      throw GeographicErr("Minor radius is not positive");
    A3coeff();
    C3coeff();
    C4coeff();
  }

  const Geodesic Geodesic::WGS84(Constants::WGS84_a<real>(),
                                 Constants::WGS84_f<real>());
*/
FUNCTION GEO_ANGLE(Geom IN MDSYS.SDO_GEOMETRY,pvertex number default 1) RETURN NUMBER AS

-- Return angle between two segments at vertex specified.
-- So for a ring the angles start at vertex 1, for non-rings they start at vertex 2.

  Angles         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  XYOrd          MDSYS.SDO_ORDINATE_ARRAY;
  XYs            MDSYS.SDO_ORDINATE_ARRAY;
  Info           MDSYS.SDO_ELEM_INFO_ARRAY := Geom.sdo_Elem_Info;
  Gtype          NUMBER := Geom.Sdo_gtype;
  vertex         PLS_INTEGER := NVL(pvertex,1);
BEGIN

    if gtype = 2003 and Info(3) = 3 then  -- optimized rectangle
  
     XYOrd := Geom.sdo_ordinates;
     Xys:= MDSYS.SDO_ORDINATE_ARRAY(XYOrd(1),XYOrd(2),XyOrd(3),XYOrd(2),
                                    XYOrd(3),XYOrd(4),XyOrd(1),XYOrd(4),XYOrd(1),XYOrd(2));
     Info := Mdsys.Sdo_Elem_Info_Array(1,1003,1);
     
     angles := Geo_Angles(Mdsys.sdo_Geometry(2003,Geom.sdo_SRID,NULL,Info,Xys));
    Else
      angles := Geo_Angles(Geom);
    End If;
     
    For ii in 1..TRUNC(Angles.count) loop
      If vertex = Angles(ii*2) then
        Return Angles(ii*2-1);
      end if;
    End Loop;
    
    Return NULL;
END Geo_Angle;
--
FUNCTION GEO_ANGLES(Geom IN MDSYS.SDO_GEOMETRY, Value_index VARCHAR2 default 'TRUE') RETURN MDSYS.SDO_LIST_TYPE AS

-- Return angles between each segment in Value, Index order when Value_Index is
-- 'TRUE' or just values when not.

  Angles            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Xys               MDSYS.SDO_ORDINATE_ARRAY:= Geom.SDO_ORDINATES;  
  Info              MDSYS.SDO_ELEM_INFO_ARRAY := Geom.SDO_ELEM_INFO;
  n                 PLS_INTEGER;
  no_info           PLS_INTEGER;   -- no of rings from Info
  istart            PLS_INTEGER;
  iend              PLS_INTEGER;
  ii                PLS_INTEGER;
  kount             PLS_INTEGER;
  loops             PLS_INTEGER;
  next             PLS_INTEGER :=0;
  
  angle             NUMBER;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;
  x3                NUMBER;
  y3                NUMBER;
  area              number;
  a1                number;
  az1               number; -- initial azimuth in degrees for geodesic line
  az2               number;
  az3               number;
  mm12              number;
  m12               number;
  m21               number;
  d_K               number;  -- the geodesic distance computed by Charles Karney's code
  s12               number;
  ss12              number;
  
  its_a_ring        BOOLEAN;
BEGIN

  no_info := TRUNC(Info.count/3);
  n := Xys.count/2;
  if Value_Index <> 'TRUE' then
    Angles.extend(n - no_info);  -- just a list of angles
  else                         
   Angles.extend(n*2- no_info*2); -- Angle comes 1st followed by its index.
   --                        Note there are n-1 vertices when the xys.count/2 = n
  end if;
  
  FOR loops in 1..no_info LOOP
    istart := loops*3+1;   -- is 4
    if loops*3 = Info.count then
      iend := Xys.count-2;  -- iend points to the y coordinate of 2nd to last vertex
    else
      iend := Info(istart)-3;  -- of vertex n-1 (vertex n =1)
    end if;

    ii := Info(istart-3);  -- is Info(1)
    kount := TRUNC((iend-ii)/2);
    its_a_ring := FALSE;

-- Is it a ring?
    if Xys(ii) = Xys(iend+1) and Xys(ii+1) = XYs(iend+2) then
       its_a_ring := TRUE;
--       dbms_output.put_line('its a ring '|| iend || ' ii ' || ii);
       kount := kount + 1;
    else
       iend := ii+1;
       ii := ii+2; -- Ignore 1st and last vertex
--       dbms_output.put_line('its NOT a ring'|| iend || ' ii ' || ii);
    end if;
      
    x2 := Xys(iend-1);
    y2 := Xys(iend);

    x3 := Xys(ii);
    y3 := Xys(ii+1);

    For i in 1..kount Loop
      x1 := x2;
      y1 := y2;
      x2 := x3;
      y2 := y3;
      ii := ii+2;
      
  
  --          (x1,y1)  +          + (x3,y3)  future (x2,y2)
  --                     \       /   \
  --                       \   /       \
  --               (x2,y2)  +           + future (x3,y3)
  --                     future (x1,y1)

      x3 := Xys(ii);
      y3 := Xys(ii+1);

      s12 := inverse(y2,x2,y1,x1,a1,az1,az2,mm12,m12,m21,SS12,FALSE,TRUE,TRUE,TRUE);

      s12 := inverse(y2,x2,y3,x3,a1,az2,az3,mm12,m12,m21,SS12,FALSE,TRUE,TRUE,TRUE);
 
--   dbms_output.put_line('ii ' || ii ||' az1 ' || az1 || ' az2 ' || az2);
      Angle := ABS(az1-az2);
  
      if angle > 180. then
         angle := 360. -angle;
      end if;

      next := next+1;
      Angles(next) := ROUND(angle,4);
      
      if Value_Index = 'TRUE' then
        next := next+1; 
        Angles(next) := TRUNC(ii/2);
      end if;
     
    End Loop;
  End Loop;

  if Angles(1) is NULL then
    Angles := NULL;
  else
    Angles.trim(angles.count-next);
  end if;

  RETURN Angles;

END GEO_Angles;
--
FUNCTION GEO_AZIMUTH(Geom IN MDSYS.SDO_GEOMETRY,pvertex PLS_INTEGER default 1,pinitial VARCHAR2 default 'TRUE') RETURN NUMBER AS

-- For a particular vertex, return azimuth (clockwise angle in degrees from 
-- North) for a) SRID=8265 the geodesic line starting at the vertex specified. or
--        for b) Cartesian, the line starting at the vertex specified.
-- For SRID=8265, If initial <> 'TRUE' it returns the final rather than the initial azimuth.

  Azimuths       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  XYOrd          MDSYS.SDO_ORDINATE_ARRAY;
  XYs            MDSYS.SDO_ORDINATE_ARRAY;
  Info           MDSYS.SDO_ELEM_INFO_ARRAY := Geom.sdo_Elem_Info;
  Gtype          NUMBER := Geom.Sdo_gtype;
  vertex         PLS_INTEGER := NVL(pvertex,1);
  
BEGIN

    if gtype = 2003 and Info(3) = 3 then  -- optimized rectangle
  
     XYOrd := Geom.sdo_ordinates;
     Xys:= MDSYS.SDO_ORDINATE_ARRAY(XYOrd(1),XYOrd(2),XyOrd(3),XYOrd(2),
                                    XYOrd(3),XYOrd(4),XyOrd(1),XYOrd(4),XYOrd(1),XYOrd(2));
     Info := Mdsys.Sdo_Elem_Info_Array(1,1003,1);
     
     Azimuths := Geo_Azimuths(Mdsys.sdo_Geometry(2003,Geom.sdo_SRID,NULL,Info,Xys),pinitial);

    Else    
      azimuths := Geo_Azimuths(Geom,pinitial);
    End If;

-- Use the Value, Index array to get the azimuth we want ..

    For ii in 1..TRUNC(Azimuths.count) loop
      If vertex = Azimuths(ii*2) then
        Return Azimuths(ii*2-1);
      end if;
    End Loop; 
    
    Return NULL;
END;
--
FUNCTION GEO_AZIMUTHS(Geom IN MDSYS.SDO_GEOMETRY,Value_index VARCHAR2 default 'TRUE',pinitial VARCHAR2 default 'TRUE') RETURN MDSYS.SDO_LIST_TYPE AS

-- Return azimuth (clockwise angle in degrees from North) for all
-- geodesic lines (segments) in a geometry (excluding non-existent segments between rings).

  Xys              MDSYS.SDO_ORDINATE_ARRAY := Geom.sdo_ordinates;
  Info             MDSYS.SDO_ELEM_INFO_ARRAY := Geom.sdo_elem_info;
  Azimuths         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  x1               NUMBER;
  y1               NUMBER;
  x2               NUMBER;
  y2               NUMBER;
  dx               NUMBER;
  dy               NUMBER;
  interpretation   number;
  SRID             NUMBER := NVL(Geom.Sdo_SRID,0); 
  angle            number;
  azimuth          number;
  area             number;
  a1               number;
  az1              number; -- initial azimuth in degrees for geodesic line
  az2              number;
  mm12             number;
  m12              number;
  m21              number;
  d_K              number;  -- the geodesic distance computed by Charles Karney's code
  s12              number;
  ss12             number;
  
  ring              PLS_INTEGER := 0;
  next              PLS_INTEGER := 0;
  next_bad_vtx      PLS_INTEGER;
  n                 PLS_INTEGER;
  
  function find_next_bad return pls_integer as
  -- find next non-existent segment end vertex (between rings)
    next_bad  pls_integer;
  begin
     ring := ring + 1;
     for ii in 1..TRUNC(Info.count/3) Loop
 
          if ii = ring and ii <> TRUNC(Info.count/3) then
             next_bad := TRUNC((Info(ii*3+1)+1)/2);
             exit;
          elsif ii = TRUNC(Info.count/3) then
             next_bad := n+1;          
          end if;          
     end loop;

     return next_bad;
  end;
  
BEGIN

  interpretation := Info(3);
  if interpretation > 1 then
     RETURN NULL;
  end if;

  n := Xys.count/2;
  if Value_Index = 'TRUE' then
    azimuths.extend(2*(n- TRUNC(Info.count/3)));
  else 
    azimuths.extend(n- TRUNC(Info.count/3));
  end if;
  x2 := Xys(1);
  y2 := Xys(2);
  next_bad_vtx := find_next_bad();

  For ii in 2..n Loop
 
 -- Ignore non-existent segment between rings
 
    IF ii = next_bad_vtx then
      next_bad_vtx := find_next_bad();
      x2 := Xys(ii*2-1);
      y2 := Xys(ii*2); 
    Else
      x1 := x2;
      y1 := y2;
  
      x2 := Xys(ii*2-1);
      y2 := Xys(ii*2);
      next := next + 1;

-- Handle Cartesian and Geodetic coordinates
      if SRID <> 8265. then
        dx := x2-x1;
        dy := y2-y1; 
        angle := New_Arctan(dy,dx,TRUE);
-- Convert from anti-clockwise bearing from East
        if angle < 0. then 
          angle := angle +360.;        
        end if;
        -- to clockwise azimuth from North
        azimuth := MOD(450. - angle,360);

        Azimuths(next) := ROUND(azimuth,4);

      elsif x1 <> x2 or y1 <> y2 then
        
        s12 := inverse(y1,x1,y2,x2,a1,az1,az2,mm12,m12,m21,SS12,FALSE,TRUE,TRUE,TRUE);

        if pinitial <> 'TRUE' then
          az1 := az2;
        end if;
        if az1 < 0. then
          azimuth := az1+360.;
        else
          azimuth := az1;
        end if;
        Azimuths(next) := ROUND(azimuth,6);
      else
        Azimuths(next) := 0.0;
      end if;
      
      if Value_index = 'TRUE' then
         next := next +1;
         Azimuths(next) := ii-1;
      end if;
    End If;
          
   End Loop;
   
   Azimuths.trim(Azimuths.count-next);
    
   Return Azimuths;
   
END GEO_AZIMUTHS;
--
FUNCTION Geo_Perimeter(Geom IN MDSYS.SDO_GEOMETRY,dec_digits NUMBER default 8) RETURN NUMBER DETERMINISTIC AS

Begin
   Return ROUND(Geo_Length(Geom),dec_digits);
End;
--
FUNCTION Geo_Length(Geom IN MDSYS.SDO_GEOMETRY) RETURN NUMBER DETERMINISTIC AS

-- Calculate Length of a geometry in meters (for 8265) or units for any other SRID
-- for a line string, polygon or optimized rectangle

   Lengths           MDSYS.SDO_LIST_TYPE;
   Xys               MDSYS.SDO_ORDINATE_ARRAY;
   XYOrd             MDSYS.SDO_ORDINATE_ARRAY;
   Gtype            NUMBER := Geom.sdo_gtype;
   Info             Mdsys.Sdo_Elem_Info_Array := Geom.sdo_Elem_info;
   interpretation   number;
   
BEGIN

   if Info.count > 0 then
     interpretation := Info(3);
   end if;
   
   if (gtype = 2002 or gtype = 2003) and interpretation = 1  then
   
     Xys := Geom.Sdo_Ordinates;
   
      lengths :=  Geo_Length(Xys,Info,Geom.Sdo_SRID);
      Return Lengths(1);
   elsif gtype = 2003 and interpretation = 3 then  -- optimized rectangle
  
     XYOrd := Geom.sdo_ordinates;
     Xys:= MDSYS.SDO_ORDINATE_ARRAY(XYOrd(1),XYOrd(2),XyOrd(3),XYOrd(2),
                                    XYOrd(3),XYOrd(4),XyOrd(1),XYOrd(4),XYOrd(1),XYOrd(2));
     Info := Mdsys.Sdo_Elem_Info_Array(1,1003,1);
     Lengths :=  Geo_Length(Xys,Info,Geom.Sdo_SRID);
     Return Lengths(1);
   end if;
   
   RETURN NULL;
END Geo_Length;
--
FUNCTION Geo_Lengths(Geom IN MDSYS.SDO_GEOMETRY,Value_index VARCHAR2 default 'TRUE') RETURN MDSYS.SDO_LIST_TYPE DETERMINISTIC AS

-- Calculate Length of a geometry in meters (for 8265) or units for any other SRID

   Lengths           MDSYS.SDO_LIST_TYPE;
   Xys               MDSYS.SDO_ORDINATE_ARRAY;
   XYOrd             MDSYS.SDO_ORDINATE_ARRAY;
   Gtype            NUMBER := Geom.sdo_gtype;
   Info             Mdsys.Sdo_Elem_Info_Array := Geom.sdo_Elem_info;
   interpretation   number;

BEGIN

   if Info.count > 0 then
     interpretation := Info(3);
   end if;
   
   if (gtype = 2002 or gtype = 2003) and interpretation = 1  then
   
     Xys := Geom.Sdo_Ordinates;
        
   elsif gtype = 2003 and interpretation = 3 then  -- optimized rectangle
  
     XYOrd := Geom.sdo_ordinates;
     Xys:= MDSYS.SDO_ORDINATE_ARRAY(XYOrd(1),XYOrd(2),XyOrd(3),XYOrd(2),
                                    XYOrd(3),XYOrd(4),XyOrd(1),XYOrd(4),XYOrd(1),XYOrd(2));
     Info := Mdsys.Sdo_Elem_Info_Array(1,1003,1);
   else
      Return NULL;
   End if; 
   Lengths := Geo_Length(Xys,Info,Geom.Sdo_SRID);
   
   if Value_index = 'TRUE' then
      Lengths.extend(Lengths.count);
      For ii in REVERSE 1..TRUNC(Lengths.count/2) Loop
         Lengths(ii*2) := ii-1;
         Lengths(ii*2-1) := Lengths(ii);
      End Loop;
   end if;
   RETURN Lengths;
   
END Geo_Lengths;
--
FUNCTION Geo_Length(XYs IN MDSYS.SDO_ORDINATE_ARRAY,Info IN MDSYS.SDO_ELEM_INFO_ARRAY,pSRID NUMBER default 8265.) RETURN MDSYS.SDO_LIST_TYPE DETERMINISTIC AS

-- Return Length of a line in meters (for 8265) or units for any other SRID. 
-- Method: Accumulate distances from start to finish.

  Lengths          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  x1               NUMBER;
  y1               NUMBER;
  x2               NUMBER;
  y2               NUMBER;
  dx               NUMBER;
  dy               NUMBER;
  SRID             NUMBER := NVL(pSRID,0.0);
  interpretation   number;
  perimeter_len    NUMBER := 0.0;
 
  area number;
  az1  number;
  az2  number;
  mm12 number;
  m12  number;
  m21  number;
  d_K  number;  -- the geodesic distance computed by Charles Karney's code
  s    number;
  
  ring              PLS_INTEGER := 0;
  next              PLS_INTEGER := 1;
  next_bad_vtx      PLS_INTEGER;
  n                 PLS_INTEGER;
  
  function find_next_bad return pls_integer as
  -- find next non-existent segment end vertex (between rings)
    next_bad  pls_integer;
  begin
     ring := ring + 1;
     for ii in 1..TRUNC(Info.count/3) Loop
 
          if ii = ring and ii <> TRUNC(Info.count/3) then
             next_bad := TRUNC((Info(ii*3+1)+1)/2);
             exit;
          elsif ii = TRUNC(Info.count/3) then
             next_bad := n+1;          
          end if;          
     end loop;

     return next_bad;
  end;
  
BEGIN

  interpretation := Info(3);
  if interpretation > 1 then
     RETURN NULL;
  end if;

  n := Xys.count/2;
  lengths.extend(n+1- TRUNC(Info.count/3));

  x2 := Xys(1);
  y2 := Xys(2);
  next_bad_vtx := find_next_bad();

  For ii in 2..n Loop
  
    IF ii = next_bad_vtx then
      next_bad_vtx := find_next_bad();
      x2 := Xys(ii*2-1);
      y2 := Xys(ii*2); 
    Else
      x1 := x2;
      y1 := y2;
  
      x2 := Xys(ii*2-1);
      y2 := Xys(ii*2);
      next := next + 1;

-- Handle Cartesian and Geodetic coordinates
      if SRID <> 8265. then
        dx := x2-x1;
        dy := y2-y1;       
        Lengths(next) := ROUND(sqrt(dx*dx + dy*dy),6);
        perimeter_len := perimeter_len + Lengths(next);
      elsif x1 <> x2 or y1 <> y2 then
        s := inverse(y1,x1,y2,x2,d_K,az1,az2,mm12,m12,m21,area);
        Lengths(next) := ROUND(d_K,6);
        perimeter_len := perimeter_len + d_K;
      else
        Lengths(next) := 0.0;
      end if;
    End If;
          
   End Loop;
   
   Lengths(1) := perimeter_len;
   Lengths.trim(Lengths.count-next);

  RETURN Lengths;

END Geo_Length;
--
FUNCTION Geo_Length(x1 number,y1 number,x2 number,y2 number,pSRID NUMBER default 8265.) RETURN NUMBER DETERMINISTIC AS

-- Return Length of a line in meters (for 8265) or units for any other SRID. 
-- Method: Accumulate distances from start to finish.

  SRID number := NVL(pSRID,0.0);
  area number;
  az1  number;
  az2  number;
  mm12 number;
  m12  number;
  m21  number;
  d_K  number :=0.0;  -- the geodesic distance computed by Charles Karney's code
  s    number;
  dx   number := x2-x1;
  dy   number := y2-y1;
BEGIN

   If x1 is NULL or y1 is NULL then
      RETURN 0.0;
   End If;
   
   if x1 <> x2 or y1 <> y2 then
      if SRID = 8265. then
         s := inverse(y1,x1,y2,x2,d_K,az1,az2,mm12,m12,m21,area);
      else
         d_k := sqrt(dx*dx+dy*dy);      
      end if;
  end if;

  RETURN d_K;

END Geo_Length;
--
FUNCTION POLYGON_AREA( XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Info MDSYS.SDO_ELEM_INFO_ARRAY, Areas IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,pSRID NUMBER default 8265., Positive BOOLEAN default TRUE) RETURN NUMBER DETERMINISTIC AS

-- Calculate polygon area including all rings in meters (for 8265) or units for 
-- other SRIDs.
--------------------------------------------------------------------------------
--Program Name: Polygon_Area
--Author: Sidey Timmins
--Creation Date: 8/04/2008
--Updated:  07/29/2013 To use Ring_area
--Updated:  03/23/2012 To calculate centroid correctly - was getting x positive
--                     for negative longitudes! Area calculation was wrong.
--          03/11/2011 To Handle more than one ring. Note this function shows
--          how to step simply thru the coordinates when there are multiple rings
--
--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 4 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT

  --      XYs          -  Array of X,Y coordinates
  --      Info         -  Element Information Array corresponding to the Xys.
  --      SRID         - the spatial reference Id
  --      OUTPUT
  --      Areas        - Arrathe areas.
--
-- Purpose: Return the area of the polygon(s) or a non_self_intersecting edge. 
--          Also returns areas of each ring . Area values are in square input 
--          units or meters for SRID=8265.
--          Accepts redundant coordinates. 

-- Reference: DH Maling, "Measurements from Maps"
--            http://en.wikipedia.org/wiki/Centroid
--------------------------------------------------------------------------------

   Area        NUMBER := 0.0;
   SRID        NUMBER := NVL(pSRID,0.0);
   oarea       number;
   ring_type   NUMBER;
   Ring_xys    mdsys.sdo_ordinate_array := mdsys.sdo_ordinate_array();
   geom        mdsys.sdo_geometry;
   delta       NUMBER;
   kount       PLS_INTEGER;
   ii          PLS_INTEGER;
   istart      PLS_INTEGER;
   iend        PLS_INTEGER;
   n           PLS_INTEGER;
   next        PLS_INTEGER :=1;
   last_area   NUMBER := 0.0;
   
   
BEGIN       
--                       vertices
--                n-1 ______________
--                    |             |
--                    |     _____   |
--                    |     |   |   |
--                    |     |___|   |
--                    |_____________|
--                   1=n          2


-- Compute area(s) of a polygon (with optional holes) or a multipolygon
-- (with optional holes).

      Areas := MDSYS.SDO_LIST_TYPE();
      n := TRUNC(Info.count/3);
      areas.extend(n+1);
      areas(1) := 0.0;

      FOR loops in 1..n LOOP
        istart := loops*3+1;   -- is 4
        if loops=n then
          iend := Xys.count-2;  -- iend points to the y coordinate
        else
          iend := Info(istart)-3;  -- of vertex n-1 (vertex n =1)
        end if;
        ring_type := Info(istart-2);

        ii := Info(istart-3);  -- is Info(1)
        kount := TRUNC((iend-ii)/2)+1;  -- Ignore last vertex (same as start)

        -- To handle a hole, we just keep adding (but since the coordinates are
        -- stored clockwise, their area contribution is negative).

        If SRID = 8265. THEN
          iend := iend + 2;
          delta := Ring_Area(Xys,8265.,ii,iend);
          ring_xys.trim(ring_xys.count);
          ring_xys.extend(iend-ii+1);
          for ij in ii..iend loop
            ring_xys(ij-ii+1) := Xys(ij);
          end loop;
--          geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1),
--                   ring_xys);
--          oarea := sdo_geom.sdo_area(geom,0.05,'unit=sq_meter');
          Area := Area + delta;

              next := next+1;
              areas(next) := ROUND((area - last_area),8);
              last_area := area;
 
          
--          dbms_output.put_line('Area ' || Lpadv(round(delta,3),20) || ' ring type ' || ring_type);
        ELSE
        
        for i in 1..kount loop

          -- area is the sum of:   (current x * y ahead - x ahead * current y)
          -- we avoid the factoring of the formula because differencing can
          -- lead to error... area = sum of current x * (y ahead - ybehind) !!!
          -- delta is impervious to repeated ordinates
                    
          delta := (XYs(iend-1)*XYs(ii+1) - XYs(ii)*XYs(iend));
          
          Area := Area + delta;
                     
          ii := ii+2;
          iend := ii-1;           
        end loop;

-- keep track of individual areas

        next := next+1;
-- This will store hole areas negative
--
--  say you have a square area 100 with a hole area 16. Then area is now
-- (100-16) = 84 and area of the hole should be stored as -16

-- whereas an island should be stored as 116 -100 = 16
         areas(next) := ROUND((area - last_area)*0.5,8);

         last_area := area;
        if loops= n then
             Area := area*0.5;
        end if;

        END IF;
        END LOOP;
 
        areas.trim(areas.count-next);
  
-- Store the total area.
        if positive then
          areas(1) := ROUND(abs(Area),8);
        else
          areas(1) := ROUND(Area,8);
        end if;
  RETURN Area;

END POLYGON_AREA;
--
FUNCTION GEO_AREA(Geom IN MDSYS.SDO_GEOMETRY,dec_digits NUMBER default 8)
           RETURN NUMBER DETERMINISTIC AS

-- Calculate a Polygon's area (in meters for SRID 8265) or units for other
-- spatial reference ids.
--
-- Area calculation was checked with data supplied by Charles Karney:
--The computations are carried out with high precision arithmetic (60
--decimal places) assuming that the given vertices are exact.  Sufficient
--terms (15) are included in the geodesic series so the results are
--correct to about 45 decimal places.  The results are then rounded to 1
--pm (for the perimeter) and to 1 um^2 (for the area); the error in these
--results is thus 0.5 pm (perimeter) and 0.5 um^2 (area).  (The
--calculations were repeated using 75 digit arithmetic and retaining 20
--terms in the series as a check.)

--Gostynin_miasto (Gostynin City)
--edges: num 967 mean 38.16542 min 0.09587 max 314.76301
--perimeter:           area:
--  36905.956945078041    32404209.679309117563  GRS80
--  36905.956945078041    32404209.67930911754362 <-- This function with ord=8
--  36905.956945078041    32404209.67931403929554 <-- This function with ord=6

  Xys     MDSYS.SDO_ORDINATE_ARRAY;
  XYOrd   MDSYS.SDO_ORDINATE_ARRAY;
  Gtype   number := Geom.sdo_gtype;
  Info    Mdsys.sdo_elem_Info_array;
  
  Areas   Mdsys.SDO_List_Type;
BEGIN

   if gtype = 2002 or gtype = 2003 then
   
     Info := Geom.Sdo_Elem_Info;
 
 -- Handle optimized rectangle
 
     If Info(3) = 3 then
       XYOrd := Geom.sdo_ordinates;
       Xys:= MDSYS.SDO_ORDINATE_ARRAY(XYOrd(1),XYOrd(2),XyOrd(3),XYOrd(2),
                                    XYOrd(3),XYOrd(4),XyOrd(1),XYOrd(4),XYOrd(1),XYOrd(2));
       Return ROUND(Polygon_Area(Xys,MDSYS.Sdo_Elem_Info_Array(1,1003,1),Areas,Geom.Sdo_SRID),dec_digits);
     Else
       Xys := Geom.Sdo_Ordinates;     
       Return ROUND(Polygon_Area(Xys,Info,Areas,Geom.Sdo_SRID),dec_digits);
     
     End if;
   end if;
   
   Return NULL;
   
END GEO_AREA;
--
FUNCTION GEO_AREAS( Geom IN MDSYS.SDO_GEOMETRY,Value_index VARCHAR2 default 'TRUE',dec_digits NUMBER default 8) RETURN MDSYS.SDO_LIST_TYPE AS

------------------------------------------------------------------------------
--Program Name: Geo_Areas
--Author: Sidey Timmins
--Creation Date: 11/29/2013
--Updated:  

--Usage: Values := GZ_Geodesic.geo_Areas(Geometry);
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      Geom          -  A ring geometry or linestring with 1st vertex=last
  --      OUTPUT
  --      Areas        -  areas of each ring followed by the ring number
--
-- Purpose:  Area values are in square input units. Accepts redundant coordinates.

-- Reference: DH Maling, "Measurements from Maps"
--            http://en.wikipedia.org/wiki/Centroid
--------------------------------------------------------------------------------


   Areas       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Info        MDSYS.SDO_ELEM_INFO_ARRAY := Geom.SDO_ELEM_INFO;
   Area        NUMBER := 0.0;
 
   Xys         MDSYS.SDO_ORDINATE_ARRAY := Geom.SDO_ORDINATES;      
   XyOrd       MDSYS.SDO_ORDINATE_ARRAY;
   
BEGIN       
--                       vertices
--                n-1 ______________
--                    |             |
--                    |     _____   |
--                    |     |   |   |
--                    |     |___|   |
--                    |_____________|
--                   1=n          2


-- Compute area(s) of a polygon (with optional holes) or a multipolygon
-- (with optional holes) and keep track of each component.

     If Info(3) = 3 then -- optimized rectangle
       XYOrd := Xys;
       Xys:= MDSYS.SDO_ORDINATE_ARRAY(XYOrd(1),XYOrd(2),XyOrd(3),XYOrd(2),
                                    XYOrd(3),XYOrd(4),XyOrd(1),XYOrd(4),XYOrd(1),XYOrd(2));
       Info := MDSYS.Sdo_Elem_Info_Array(1,1003,1);    
    End If;
    
    Area := Polygon_Area(Xys,Info,Areas,Geom.Sdo_SRID);      
    If Value_Index = 'TRUE' then
       Areas.extend (Areas.count);
       For ii in REVERSE 1..TRUNC(Areas.count/2) Loop
          Areas(ii*2) := ii-1;
          Areas(ii*2-1) := Round(Areas(ii),dec_digits);
       End Loop;
    End If;
  RETURN Areas;

END GEO_AREAS;
--
FUNCTION Ring_AREA(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,pSRID NUMBER DEFAULT 8265.,pstart PLS_INTEGER default 1, pend PLS_INTEGER default 0)
           RETURN NUMBER DETERMINISTIC IS

--------------------------------------------------------------------------------
--Program Name: geo_area
--Author: Sidey Timmins
--Creation Date: 7/26/2013

--Usage:  Xys := Geometry.sdo_ordinates;
--        Area (in meters ) := Polygon_Area(Xys);

  --  Parameters:
  --      Xys           - the coordinates of a single closed polygon
  --      SRID          - The Oracle spatial reference id (projection)
  --
-- Purpose: Computes area on a planar earth or the ellipsoid. For the ellipsoid
-- it call Charles Karney's Geographic Lib and the accuracy surpasses sdo_area.
-- Testing: Excellent but did not compute entire hemisphere with this:
-- ring_Xys := mdsys.sdo_ordinate_array(-180,0,180,0,180,90,-180,0);
--------------------------------------------------------------------------------

  n            PLS_INTEGER;
  next        PLS_INTEGER := 2;
  ii           PLS_INTEGER := pstart;
  iend         PLS_INTEGER := pend;
  kount        PLS_INTEGER;
  cross_       PLS_INTEGER :=0;
  
  x1           NUMBER;
  y1           NUMBER;
  x2           NUMBER;
  y2           NUMBER;
  x3           NUMBER;
  y3           NUMBER;
  SRID         NUMBER := NVL(pSRID,0.0);

  a1           NUMBER;
  s            NUMBER;
  s12          NUMBER;
  SS12         NUMBER;
  area0        NUMBER;
  az1          number;
  az2          number;
  mm12         number;
  m12          number;
  m21          number;
  d_K          number;  -- the geodesic distance computed by Charles Karney's code
 
-- Authalic radius 
--  AR           NUMBER := 6371007.18088351429821304500954888028409;
--  f            NUMBER := 298.2572221008827112431628366;
--  AR_Sq        NUMBER := 40589732498869.30427587653936614505912995;
    
  
  Area         NUMBER := 0.0;
  
  --
  Function Sumx(u NUMBER, v NUMBER, t IN OUT NOCOPY NUMBER) return NUMBER as
  
-- Produces an error free sum.
-- SUM = SUMX(u, v) returns the rounded sum U+V in SUM and the error in t
-- such that SUM +t = U+V exactly.
-- It is not known if this code is required for Oracle PL/SQL

    up       number;
    vpp      number;
    sumx     number;
    
  Begin
    sumx := u+v;
    up := sumx -v;
    vpp := sumx -up;
    up := up -u;
    vpp := vpp -v;
    t := -(up+vpp);
    return sumx;
  End;
  --
  Function Angdif(x number, y number) return number as
  
--   Compute y - x.  x and y must both lie in [-180, 180].  The result is
-- equivalent to computing the difference exactly, reducing it to (-180,
-- 180] and rounding the result.  Note that this prescription allows -180
-- to be returned (e.g., if x is tiny and negative and y = 180).

     d      number;
     t      number;
  Begin
  
     d := sumx(-x,y,t);
     if d - 180. + t > 0.0 then
       d := d - 360.0;
     elsif d+180. + t <= 0.0 then
       d := d + 360.0;
     end if;
     return d + t;
  End;
  --
  Function TRANSIT(lon1 NUMBER, lon2 NUMBER)
                 RETURN PLS_INTEGER AS
  -- Determine when we cross the prime Meridian
  -- 0: no crossing
  -- 1: West to East
  -- -1: East to West

  lon1x      NUMBER;
  lon2x      NUMBER;
  lon12      NUMBER;
  trnsit     PLS_INTEGER := 0;
  Begin
    lon1x :=  AnGNormalize(lon1);
    lon2x :=  AnGNormalize(lon2);
    lon12 := lon2x-lon1x;
 --   lon12 := AngDif(lon1x,lon2x);
 --   dbms_output.put_line('lon1x ' || lon1x || ' lon2x ' || lon2x || ' lon12 ' || lon12);
    if lon1x < 0.0 and lon2x >= 0.0 and lon12 > 0.0 then
       trnsit := 1;
    elsif lon2x < 0.0 and lon1x >= 0.0 and lon12 < 0.0 then
       trnsit := -1;
    end if;
    Return trnsit;
  End;

BEGIN

    if iend =0 then iend := XYs.count; end if;
    n := TRUNC((iend-pstart+1)/2);
    
    x2 := Xys(pstart);
    y2 := Xys(pstart+1);
    x3 := Xys(iend-3);
    y3 := Xys(iend-2);
     
    if x2 <> xys(iend-1) or y2 <> xys(iend) then
      dbms_output.put_line('>>> ITS NOT a valid polygon <<<' || pstart || ' iend ' || iend);
    end if;
    
    iend := iend-2;  -- points to y coordinate of n-1 vertex
    If SRID <> 8265. THEN
        for i in 1..n-1 loop
          -- area is the sum of -- current x * (y ahead - y behind)
          -- can lead to error: Area := Area + XYs(ij) *  (XYs(ij+3) - XYs(iend));
          -- This is preferred to avoid small differences
          -- area is sum of (current x * y ahead - x ahead * current y)
          
          Area := Area + (XYs(iend-1)*XYs(ii+1) - XYs(ii)*XYs(iend));
 
          ii := ii+2;
          iend := ii-1;
        end loop;
        Area := Area*0.5;
     
      RETURN Area;
    END IF;
    
-- Use GZ_Geodesic to calculate the areas extremely accurately..

    x2 := Xys(pstart);
    y2 := Xys(pstart+1);
    next := pstart+1;
    While next <= iend Loop
        x1 := x2;
        y1 := y2;
--        dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
        next := next + 2;
        x2 := Xys(next-1);
        y2 := Xys(next);
  
        s12 := inverse(y1,x1,y2,x2,a1,az1,az2,mm12,m12,m21,SS12,TRUE);
--        dbms_output.put_line('next ' || next || ' ss12 ' || round(ss12,15) );
--        ss12 := GZ_geodesic.inverse(y1,x1,y2,x2,a1,az1,az2,m12,TRUE);
--        dbms_output.put_line('Next ' || next || ' ss12 ' || round(ss12,15));
        Area := Area  - ss12;
        cross_ := cross_ + transit(x1,x2);
--        if a1 < 0 then
--        dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
--        dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2);
--          dbms_output.put_line('next ' || next || ' ss12 ' || round(ss12,15) || 'cross ' || cross_);
--        end if;
    End Loop;
    
    area0 := 4. *g_pi * g_c2;
--dbms_output.put_line('area ' || round(area,15));

    if MOD(ABS(cross_),2) = 1 then
--    dbms_output.put_line('AAArea ' || round(area,15));
       if area < 0.0 then
          Area := Area + area0*0.5;
--          dbms_output.put_line('AArea ' || round(area,15));
       else
          Area := Area - area0*0.5;
--          dbms_output.put_line('Area ' || round(area,15));
       end if;
    end if;
--    dbms_output.put_line('Area +area0*0.5' || (area+area0*0.5));
    if cross_ = 0 and (Area +area0*0.5) < 1.E-23 then
      Area := area0*0.25;
    elsif Area > area0*0.5 then
       Area := Area - area0;
    elsif Area <= -area0*0.5 then
       Area := Area + area0;
    end if;
 --   dbms_output.put_line('area ' || round(area,15) || ' area0 ' || round(area0,3));
    RETURN (Area);

END Ring_AREA;
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
PROCEDURE SINCOSNORM(sinx IN OUT NOCOPY NUMBER,cosx IN OUT NOCOPY NUMBER) AS

   r    NUMBER;
BEGIN
   r := hypot(sinx,cosx);
   sinx := sinx/r;
   cosx := cosx/r;
END;
--
FUNCTION SINCOSSERIES(sinp BOOLEAN default TRUE,sinx NUMBER,cosx NUMBER,C MDSYS.SDO_LIST_TYPE,NN PLS_INTEGER) RETURN NUMBER  IS

--   Evaluate y = sum(c[i] * sin(2 * i * x), i, 1, n) using Clenshaw
--   summation.  (c[0] was unused in original "C" code)
--   Approx operation count = (n + 5) mult and (2 * n + 2) add

   ar     NUMBER := 2.* (cosx-sinx) * (cosx+sinx); --  2 * cos(2 * x)
   n      PLS_INTEGER := nn;
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
   if sinp then
     y0 := y0 * 2.*sinx*cosx; --  sin(2 * x) * y0
   else
     y0 := cosx*(y0-y1);
   end if;
   RETURN y0;
END SINCOSSERIES;
--
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
PROCEDURE to_Gnomonic(plat1 NUMBER,plon1 NUMBER,plat NUMBER,plon NUMBER,
                      x IN OUT NOCOPY NUMBER, y IN OUT NOCOPY NUMBER,
                      azi IN OUT NOCOPY NUMBER, rk IN OUT NOCOPY NUMBER ) AS
   
   a1         NUMBER;                   
   s12        NUMBER;
   M          NUMBER;
   azi0       NUMBER;
   mm12       NUMBER;
   m21        NUMBER;
   rho        NUMBER;
   area       NUMBER;
   
BEGIN

-- Turn azimuth on and reduced length and geodesic scale
  s12 := Inverse(plat1,plon1,plat,plon,a1,azi0,azi,mm12,M,m21,area,FALSE,TRUE,TRUE,TRUE);
  
--  dbms_output.put_line('az ' || azi0 || ' azi ' ||azi || ' a1 ' || a1);
--  dbms_output.put_line('mm12 ' || mm12 || ' m12 ' || M || ' M21 ' || M21 ||' s12 ' || s12);
  rk := M;
  if M <= 0.0 then
    x := NULL;
    y := NULL;
  else
    rho := mm12/M;
    azi0 := azi0*g_to_radians;
    x := rho * sin(azi0); --/g_a;
    y := rho * cos(azi0); --/g_a;
  end if;
END;

FUNCTION Inverse(plat1 NUMBER,plon1 NUMBER,
                 plat2 NUMBER,plon2 NUMBER,
                 s12 IN OUT NOCOPY NUMBER, azi1 IN OUT NOCOPY NUMBER,
                 azi2 IN OUT NOCOPY NUMBER,mm12 IN OUT NOCOPY NUMBER,
                 m12 IN OUT NOCOPY NUMBER,m21 IN OUT NOCOPY NUMBER,
                 area IN OUT NOCOPY NUMBER,
                 areap BOOLEAN default FALSE,azimuth BOOLEAN default FALSE,
                 reduced_Length BOOLEAN default FALSE,
                 geodesic_scale  BOOLEAN default FALSE,
                 a NUMBER default 6378137.,
                 F NUMBER default 298.257222101) RETURN NUMBER AS

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
    C4a        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    C4x        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    lat1       NUMBER := plat1;
    lon1       NUMBER := plon1;
    lat2       NUMBER := plat2;
    lon2       NUMBER := plon2;
    lon12      NUMBER;
    dn1        NUMBER;
    dn2        NUMBER;
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
    a12        NUMBER;
    lam12      NUMBER;
    slam12     NUMBER;
    clam12     NUMBER;
    ssig1      NUMBER;
    csig1      NUMBER;
    ssig2      NUMBER;
    csig2      NUMBER;
    eps        NUMBER;
    omg12      NUMBER;


    dalp1      NUMBER;
    sdalp1     NUMBER;
    cdalp1     NUMBER;
    nsalp1     NUMBER;
    salp0      NUMBER;
    calp0      NUMBER;
    k2         NUMBER;
    ep2        NUMBER;
    alp12      NUMBER;
    A4         NUMBER;
    C2         NUMBER:= 40589732498869.30427587653936614505912995;
    dnm        NUMBER :=0.0;
    S12x       NUMBER;
    M12x       NUMBER;
 
 
    B41        NUMBER;
    B42        NUMBER;
    somg12     NUMBER;
    domg12     NUMBER;
    dbet1      NUMBER;
    dbet2      NUMBER;
    salp12     NUMBER;
    calp12     NUMBER;
    salp1a     NUMBER;
    calp1a     NUMBER;
    salp1b     NUMBER;
    calp1b     NUMBER;

    SS12       NUMBER := 0.0;
    lonsign    NUMBER :=1.;
    latsign    NUMBER :=1.;
    swapp      PLS_INTEGER;
    numit      PLS_INTEGER :=0;
    maxit1     PLS_INTEGER := 20; --  50;
    maxit2     PLS_INTEGER := 50; --  50;
  
    
    tripn      BOOLEAN;
    tripb      BOOLEAN;
    meridian   BOOLEAN := FALSE;
    Bad        VARCHAR2(1000);
    
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

-- Ellipsoid specific constants: GRS80 here

  g_a   := a;      --   := 6378137.0;      -- semi-major axis for GRS80
--  g_r   := F;      --   := 298.257222101;  -- reciprocal flattening for GRS80
--  g_r           := 298.2572221008827112431628366;
  g_f   := 1./g_r;
  g_f1  := 1. -g_f;
  g_e2  := g_f*(2. - g_f);
  g_ep2 := g_e2/(1.-g_e2);  --// e2 / (1 - e2)
  g_n   := g_f/(2. - g_f);
  g_b   := g_a * g_f1;
--  dbms_output.put_line('a ' || a || ' F ' || F || ' f1 ' || g_f1);
--  dbms_output.put_line('b ' || g_b || ' F ' || F || ' f1 ' || g_f1);   
--  g_c2  
--           ((Math::sq(_a) + Math::sq(_b) *
--           (_e2 == 0 ? 1 :
--            (_e2 > 0 ? Math::atanh(sqrt(_e2)) : atan(sqrt(-_e2))) /
--            sqrt(abs(_e2))))/2) -- authalic radius squared


   if sqrt(abs(g_e2)) < 0.1 then
     g_etol2  := g_tol2/0.1;
   else
     g_etol2  := g_tol2/sqrt(abs(g_e2));
   end if;

   if g_A3x.count = 0 then
    -- Authalic radius squared
    g_c2 := (g_a*g_a + g_b*g_b * atanh(sqrt(g_e2))/sqrt(g_e2)) *0.5;
    A3coeff;
    C3coeff;
    C4coeff;
   end if; 


-- Sidey AngDiff
    lon1 := AngNormalize(lon1);
    lon12 := AngNormalize(AngNormalize(lon2) - lon1);


--    If very close to being on the same meridian, then make it so.
--    Not sure this is necessary...

    lon12 := AngRound(lon12);
    
--    Make longitude difference positive.   SIDEY Started here

    if lon12 >= 0.0 then
      lonsign := 1;
    else
      lonsign := -1;
    end if;

    lon12 := lon12*lonsign;
    
--    if (lon12 = 180.0) then
--      lonsign := 1;
--    end if;

--    If really close to the equator, treat as on equator.
    lat1 := AngRound(lat1);
    lat2 := AngRound(lat2);

--    Swap points so that point with higher (abs) latitude is point 1
    swapp := 1;
    if abs(lat1) < abs(lat2) then
      swapp := -1;
      lonsign := -lonsign;
      temp := lat1;
      lat1 := lat2;
      lat2 := temp;
    end if;
    
--    Make lat1 <= 0

    if lat1 >= 0.0 then
      latsign := -1;
      lat1 := lat1*latsign;
      lat2 := lat2*latsign;
    end if;
    
    
--    dbms_output.put_line('lon12 ' || lon12 || ' lonsign ' || lonsign || ' lat ' || latsign);
--  dbms_output.put_line('lon12 ' || lon12 || 'lat1 ' || lat1 || ' lat2 '|| lat2);
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
       cbet1 := g_tiny;
    end if;

    SinCosNorm(sbet1, cbet1);

    phi := lat2 * g_to_radians;
    
--  Ensure cbet2 = +epsilon at poles
    sbet2 := g_f1 * sin(phi);
--    dbms_output.put_line('SBET2 ' || sbet2 || ' phi ' ||phi || ' g_f1 ' || g_f1);
    if abs(lat2) = 90. then
      cbet2 := g_tiny;
    else
      cbet2 := cos(phi);
    end if;
    SinCosNorm(sbet2, cbet2);
    

    -- If cbet1 < -sbet1, then cbet2 - cbet1 is a sensitive measure of the
    -- |bet1| - |bet2|.  Alternatively (cbet1 >= -sbet1), abs(sbet2) + sbet1 is
    -- a better measure.  This logic is used in assigning calp2 in Lambda12.
    -- Sometimes these quantities vanish and in that case we force bet2 = +/-
    -- bet1 exactly.  An example where is is necessary is the inverse problem
    -- 48.522876735459 0 -48.52287673545898293 179.599720456223079643
    -- which failed with Visual Studio 10 (Release and Debug)

    if (cbet1 < -sbet1) then
      if (cbet2 = cbet1) then
        if sbet2 < 0.0 then
          sbet2 := sbet1;
        else
        sbet2 := -sbet1;
        end if;
      end if;
    elsif (abs(sbet2) = -sbet1) then
        cbet2 := cbet1;
    end if;


    dn1 := sqrt(1. + g_ep2* (sbet1*sbet1));
    dn2 := sqrt(1. + g_ep2* (sbet2*sbet2));
--dbms_output.put_line('dn1 ' || dn1 || ' dn2 ' || dn2);
    lam12 := lon12 * g_to_radians;
    if lon12 <> 180.0 then
        slam12 := sin(lam12);
--        dbms_output.put_line('Taking sin lam12' || slam12);
    else
       slam12 := 0.0;
--       dbms_output.put_line('SLAM12 is zero' || slam12);
    end if;

    clam12 := cos(lam12);      -- lon12 == 90 isn't interesting


--    index zero elements of these arrays are unused in original "C" code
--    real C1a[nC1 + 1], C2a[nC2 + 1], C3a[nC3];

     C1a.extend(g_nC1_+1);
     C2a.extend(g_nC2_+1);
     C3a.extend(g_nC3_);
     C4a.extend(g_nC4_+1);
     C4x.extend(g_nC4x);
 


    if lat1 = -90. or slam12 = 0.0 then
       meridian := TRUE;
    end if;

    if (meridian = TRUE) then
-- dbms_output.put_line('meridian is TRUE' || slam12);
--      Endpoints are on a single full meridian, so the geodesic might lie on
--      a meridian.

      calp1 := clam12;
      salp1 := slam12;      --Head to the target longitude
      calp2 := 1.;
      salp2 := 0;           -- At the target we're heading north


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
      Lengths(g_n, sig12, ssig1, csig1,dn1, ssig2, csig2,dn2,
                cbet1, cbet2, s12x, m12x, dummy,geodesic_scale, M12,M21, C1a, C2a);
--dbms_output.put_line('BACK FROM lengths' || s12x  || ' temp ' || temp || ' sig12 ' || sig12);
--       Add the check for sig12 since zero length geodesics might yield m12 <
--       0.  Test case was
--
--          echo 20.001 0 20.001 0 | Geod -i
--
--       In fact, we will have sig12 > pi/2 for meridional geodesic which is
--       not a shortest path.

      if (sig12 < 1. or m12x >= 0.) then
        m12x := m12x*g_b;   -- corrected on Oct 25, 2013
        s12x := s12x*g_b;
--        dbms_output.put_line('S12x ' || s12x );
        a12 := sig12*g_to_degrees;
      else
--         m12 < 0, i.e., prolate and too close to anti-podal
        meridian := false;
--        dbms_output.put_line('meridian is ffalse');
      end if;
    end if;

    if (meridian = FALSE and sbet1 = 0 and --// sbet2 = 0.0
        (g_f <= 0.0 or lam12 <= g_pi - g_f * g_pi)) then
--   // and sbet2 == 0
--      Mimic the way Lambda12 works with calp1 = 0

--      Geodesic runs along equator
--dbms_output.put_line('meridian is FALSE' || s12);
      calp1 := 0.0;
      calp2 := 0.0;
      salp1 := 1.;
      salp2 := 1.;
      s12x   := g_a * lam12;
--      dbms_output.put_line('g_a ' || g_a || ' lam12 ' || lam12);
      omg12 := lam12 / g_f1;
      sig12 := omg12;
      m12x  := g_b * sin(sig12);
      a12 := lon12/g_f1;
      if geodesic_scale then
        M12 := cos(sig12);
        M21 := M12;
      end if;
--      dbms_output.put_line('sig12 ' ||sig12 || ' s12 ' || s12);
    elsif (meridian = FALSE) then
--dbms_output.put_line('mmmeridian is false');
--      Now point1 and point2 belong within a hemisphere bounded by a
--      meridian and geodesic is neither meridional or equatorial.

--      Figure a starting point for Newton's method

--      dbms_output.put_line('calling inversestart salp2' || salp2 || ' calp2 ' || calp2);
--      dbms_output.put_line('sbet1' || sbet1);
--dbms_output.put_line('cbet1 ' || cbet1);
--dbms_output.put_line('sbet2 ' || sbet2);
--dbms_output.put_line('cbet2' || cbet2);
      sig12 := InverseStart(sbet1, cbet1, dn1,sbet2, cbet2, dn2,
                           lam12,salp1, calp1, salp2, calp2,dnm, C1a, C2a);
--      dbms_output.put_line('back from inversestart sig12 ' || sig12);
--      dbms_output.put_line('lam12' || lam12);
--dbms_output.put_line('salp1' || salp1);
--dbms_output.put_line('calp1 ' || calp1);
--dbms_output.put_line('salp2 ' || salp2);
--dbms_output.put_line('calp2' || calp2);
--dbms_output.put_line('sig12' || sig12);
      if (sig12 >= 0.) then
--      Short lines (InverseStart sets salp2, calp2)
        s12x := sig12 * g_b * dnm;
        m12x := dnm * dnm * g_b * sin(sig12/dnm);
        if geodesic_scale then
          M12 := cos(sig12)/dnm;
          M21 := M12;
        end if;
        a12 := sig12*g_to_degrees;
        omg12 := lam12/(g_f1 * dnm);
        
      else

        -- Newton's method.  This is a straightforward solution of f(alp1) =
        -- lambda12(alp1) - lam12 = 0 with one wrinkle.  f(alp) has exactly one
        -- root in the interval (0, pi) and its derivative is positive at the
        -- root.  Thus f(alp) is positive for alp > alp1 and negative for alp <
        -- alp1.  During the course of the iteration, a range (alp1a, alp1b) is
        -- maintained which brackets the root and with each evaluation of
        -- f(alp) the range is shrunk, if possible.  Newton's method is
        -- restarted whenever the derivative of f is negative (because the new
        -- value of alp1 is then further from the solution) or if the new
        -- estimate of alp1 lies outside (0,pi); in this case, the new starting
        -- guess is taken to be (alp1a + alp1b) / 2.

        

        numit := 0;
        salp1a := g_tiny;
        calp1a := 1.;
        salp1b := g_tiny;
        calp1b := -1.;
        tripn := FALSE;
        tripb := FALSE;

        For  numit in 0..maxit2-1 loop
        
        
          v := Lambda12(sbet1, cbet1, dn1,sbet2, cbet2, dn2,salp1, calp1,
                            salp2, calp2, sig12, ssig1, csig1, ssig2, csig2,
                            eps, omg12,numit < Maxit1, dv, C1a, C2a, C3a) - lam12;
          
--     dbms_output.put_line('>> ' || omg12); 
--dbms_output.put_line('calp1' || calp1);
--dbms_output.put_line('salp1' || salp1);
--dbms_output.put_line('calp2' || calp2);
--dbms_output.put_line('salp2' || salp2);
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
--       dbms_output.put_line('VV ' || round(v,15) || ' dv ' || round(dv,20));
          -- 2 * tol0 is approximately 1 ulp for a number in [0, pi].
          -- Reversed test to allow escape with NaNs
          
          
             temp := 8.;
             if tripn = FALSE then
               temp := 2.;
              end if;
          exit when (tripb or NOT(abs(v) >= temp*g_tol0));
         
     
          --// Update bracketing values
          if (v > 0.0 and (numit > maxit1 or calp1/salp1 > calp1b/salp1b)) then
             salp1b := salp1; 
             calp1b := calp1; 
          elsif (v < 0.0 and (numit > maxit1 or calp1/salp1 < calp1a/salp1a)) then
             salp1a := salp1; 
             calp1a := calp1; 
          end if;

          if numit < maxit1 and dv > 0.0 then
          dalp1 := -v/dv;

          sdalp1 := sin(dalp1);
          cdalp1 := cos(dalp1);
          nsalp1 := salp1 * cdalp1 + calp1 * sdalp1;
          if nsalp1 > 0.0 and ABS(dalp1) < g_pi then
          calp1  := calp1 * cdalp1 - salp1 * sdalp1;
          salp1  := nsalp1;

          SinCosNorm(salp1, calp1);
--        In some regimes we don't get quadratic convergence because slope
--         -> 0.  So use convergence conditions based on epsilon instead of
--         sqrt(epsilon). 

           tripn := (ABS(v) <= 16. * g_tol0);
           continue;
          end if;
          end if;
          -- Either dv was not postive or updated value was outside legal
          -- range.  Use the midpoint of the bracket as the next estimate.
          -- This mechanism is not needed for the WGS84 ellipsoid, but it does
          -- catch problems with more eccentric ellipsoids.  Its efficacy is
          -- such for the WGS84 test set with the starting guess set to alp1 =
          -- 90deg:
          -- the WGS84 test set: mean = 5.21, sd = 3.93, max = 24
          -- WGS84 and random input: mean = 4.74, sd = 0.99
          salp1 := (salp1a + salp1b)/2;
          calp1 := (calp1a + calp1b)/2;
          SinCosNorm(salp1, calp1);
          tripn := FALSE;
          tripb := (abs(salp1a - salp1) + (calp1a - calp1) < g_tolb) or
                   (abs(salp1 - salp1b) + (calp1 - calp1b) < g_tolb);

         
        End Loop;

        Lengths(eps, sig12, ssig1, csig1, dn1, ssig2, csig2,dn2,
                  cbet1, cbet2, s12x, m12x, dummy,geodesic_scale, M12,M21,C1a, C2a);

        m12x := m12x*g_b;
        s12x := s12x*g_b;
        a12 := sig12*g_to_degrees;
        omg12 := lam12 -omg12;
        
        
      end if;
    end if;

    s12 := s12x;
    if reduced_length then
      mm12 := m12x;
    end if;
    
-- New code from GeographicLib 1.32
      if (areap) then
-- From Lambda12: sin(alp1) * cos(bet1) = sin(alp0)
        salp0 := salp1 * cbet1;
        calp0 := sqrt(calp1*calp1 + (salp1 * sbet1)*(salp1 * sbet1));
        if (calp0 <> 0.0 and salp0 <> 0.0) then
-- From Lambda12: tan(bet) = tan(sig) * cos(alp)
          ssig1 := sbet1;
          csig1 := calp1 * cbet1;
          ssig2 := sbet2;
          csig2 := calp2 * cbet2;
--           dbms_output.put_line('ssig1 ' || ssig1 || 'csig1 ' || csig1);
--           dbms_output.put_line('ssig2 ' || ssig2 || 'csig2 ' || csig2);
          k2 := calp0*calp0 * g_ep2;
--          dbms_output.put_line('k2 ' || k2);
          eps := k2 / (2. * (1. + sqrt(1. + k2)) + k2);
-- Multiplier = a^2 * e^2 * cos(alpha0) * sin(alpha0).
          A4 := g_a*g_a * calp0 * salp0 * g_e2;
          SinCosNorm(ssig1, csig1);
          SinCosNorm(ssig2, csig2);
          C4f(eps, C4a);
--          for ii in 1..c4a.count loop
--             dbms_output.put_line('c4a ' || c4a(ii));
--          end loop;
          B41 := SinCosSeries(FALSE,ssig1, csig1, C4a, g_nC4_-1);
          B42 := SinCosSeries(FALSE,ssig2, csig2, C4a, g_nC4_-1);
--          dbms_output.put_line('A4 ' || a4 || ' b41 ' || b41 || ' b42 ' || b42);
          SS12 := A4 * (B42 - B41);
           
        else
-- Avoid problems with indeterminate sig1, sig2 on equator
          SS12 := 0.0;
        end if;

        if (meridian = FALSE and omg12 < 0.75 * g_pi and sbet2 - sbet1 < 1.75) then
-- Use tan(Gamma/2) = tan(omg12/2)
-- * (tan(bet1/2)+tan(bet2/2))/(1+tan(bet1/2)*tan(bet2/2))
-- with tan(x/2) = sin(x)/(1+cos(x))

          somg12 := sin(omg12);
          domg12 := 1. + cos(omg12);
          dbet1 := 1. + cbet1;
          dbet2 := 1. + cbet2;
          alp12 := 2. * new_arctan(somg12 * (sbet1 * dbet2 + sbet2 * dbet1),
             domg12 * ( sbet1 * sbet2 + dbet1 * dbet2 ) );
        else
        -- alp12 = alp2 - alp1, used in atan2 so no need to normalize
          salp12 := salp2 * calp1 - calp2 * salp1;
          calp12 := calp2 * calp1 + salp2 * salp1;
-- The right thing appears to happen if alp1 = +/-180 and alp2 = 0, viz
-- salp12 = -0 and alp12 = -180.  However this depends on the sign
-- being attached to 0 correctly.  The following ensures the correct behavior.

          if (salp12 = 0.0 and calp12 < 0.0) then
            salp12 := g_tiny * calp1;
            calp12 := -1;
          end if;
          alp12 := new_arctan(salp12, calp12);
        end if;
        SS12 := SS12 + c2 * alp12;
        SS12 := SS12 * swapp * lonsign * latsign;
        area := SS12;
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
      if geodesic_scale then
        temp := M21;
        M21 := M12;
        M12 := temp;
      end if;
    end if;

    salp1 := salp1*swapp * lonsign;
    calp1 := calp1*swapp * latsign;
    salp2 := salp2 * swapp * lonsign;
    calp2 := calp2 * swapp * latsign;
--  minus signs give range [-180, 180). 0- converts -0 to +0.

    if azimuth then
--    dbms_output.put_line('salp1 ' || salp1 || ' calp1 ' || calp1 || ' swapp ' || swapp || ' lonsign ' ||lonsign);
--    dbms_output.put_line('salp2 ' || salp2 || ' calp2 ' || calp2);
    azi1 := 0. - new_arctan(- salp1,calp1) * g_to_degrees;
    azi2 := 0. - new_arctan(- salp2,calp2) * g_to_degrees;
    end if;
--   Return value in [0, 180], unless it's negated to signal convergence failure

    return a12;
   
END Inverse;
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
  Function Astroid(x number,y number) Return Number as
  
    -- Solve k^4+2*k^3-(x^2+y^2-1)*k^2-2*y^2*k-y^2 := 0 for positive root k.
    -- This solution is adapted from Geocentric::Reverse.
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

    k     Number;    
    p     Number:= x*x;
    q     Number:= y*y;
    r     Number;
    s     Number;
    r2    Number;
    r3    Number;
    disc  Number;
    u     Number;
    T3    Number;
    T     Number;
    ang   Number;
    v     Number;
    uv  NUMBER;
    w   NUMBER;
      
    Begin
      r := (p + q - 1.) / 6.;
      
    if ( NOT(q = 0.0 and r <= 0.) ) Then

        -- Avoid possible division by zero when r := 0 by multiplying equations
        -- for s and t by r^3 and r, resp.
        s := p * q / 4.;            -- S := r^3 * s
        r2 := r*r;
        r3 := r * r2;
        
        -- The discrimant of the quadratic equation for T3.  This is zero on
        -- the evolute curve p^(1/3)+q^(1/3) := 1
        
        disc := S * (S + 2. * r3);
        u := r;
        
      if (disc >= 0.0) Then
         T3 := S + r3;
         
        -- Pick the sign on the sqrt to maximize abs(T3).  This minimizes loss
        -- of precision due to cancellation.  The result is unchanged because
        -- of the way the T is used in definition of u.
        
        if T3 < 0.0 then
          T3 := T3 - sqrt(disc); -- T3 := (r * t)^3
        else
           T3 := T3 + sqrt(disc); -- T3 := (r * t)^3
        end if;
        
        -- N.B. cbrt always returns the real root.  cbrt(-8) := -2.
        T := cbrt(T3); -- T := r * t
        
        -- T can be zero; but then r2 / T -> 0.
        if T <> 0 then
          u := u + T +  r2 / T;
        else
          u := u + T ;
        End If;
      
      else 
        -- T is complex, but the way u is defined the result is real.
        
        ang := new_arctan(sqrt(-disc), -(S + r3));
        
        -- There are three possible cube roots.  We choose the root which
        -- avoids cancellation.  Note that disc < 0 implies that r < 0.
        
        u := u + 2. * r * cos(ang / 3.);
      End If;
      
  
        v := sqrt(u*u + q);    -- guaranteed positive
        
        -- Avoid loss of accuracy when u < 0.
        if u < 0. then
         uv :=  q / (v - u);
        else
          uv := u + v; -- u+v, guaranteed positive
        end if;
        w := (uv - q) / (2. * v);           -- positive?
        
      -- Rearrange expression for k to avoid loss of accuracy due to
      -- subtraction.  Division by 0 not possible because uv > 0, w >= 0.
      
       k := uv / (sqrt(uv + w*w) + w);   -- guaranteed positive
    else               -- q == 0 && r <= 0
      -- y := 0 with |x| <= 1.  Handle this WHEN directly.
      -- for y small, positive root is k := abs(y)/sqrt(1-x^2)
      k := 0;
    end if;
    return k;
    
  END ASTROID;
--
FUNCTION InverseStart(sbet1 NUMBER, cbet1 NUMBER,dn1 NUMBER, 
                       sbet2 NUMBER, cbet2 NUMBER, dn2 NUMBER, lam12 NUMBER,
                       salp1 IN OUT NOCOPY NUMBER, calp1 IN OUT NOCOPY NUMBER,
                       salp2 IN OUT NOCOPY NUMBER, calp2 IN OUT NOCOPY NUMBER,
                                    dnm  IN OUT NOCOPY NUMBER,
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
  sbetm2     NUMBER;

  M12b       NUMBER;
  m0         NUMBER;
  dummy      NUMBER;
  dummy1     NUMBER;
  dummy2     NUMBER;
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
    if cbet12 >= 0.0 and sbet12 < 0.5 and cbet2* lam12 < 0.5 then
      shortline := TRUE;
--      dbms_output.put_line('SHORT LINE ');
    end if;

   
    if shortline then
      sbetm2 := (sbet1 + sbet2)*(sbet1+sbet2);
 --     // sin((bet1+bet2)/2)^2
  --    // =  (sbet1 + sbet2)^2 / ((sbet1 + sbet2)^2 + (cbet1 + cbet2)^2)

      sbetm2 := sbetm2/(sbetm2 + (cbet1 + cbet2)*(cbet1 + cbet2));
      dnm := sqrt(1. + g_ep2 * sbetm2);
      omg12 := lam12 / (g_F1 * dnm);
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
      if comg12 >= 0. then
        calp2 := (sbet12 - cbet1 * sbet2 * somg12*somg12 / (1. + comg12));
      else
        calp2 := (sbet12 - cbet1 * sbet2 * somg12*somg12 / (1. - comg12));
      end if;
      
      SinCosNorm(salp2,calp2);
      
--    Set return value
      sig12 := new_arctan(ssig12, csig12);
--      dbms_output.put_line('really SHORT ' || sig12 || ' g_etol ' || g_etol2);
    elsif (abs(g_n) > 0.1 or -- Skip astroid calc if too eccentric
           csig12 >= 0.0 or
           ssig12 >= 6. * abs(g_n) * g_pi * cbet1*cbet1) then
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
            lamscale := g_f * cbet1 * A3f(eps) * g_pi;
        betscale := lamscale * cbet1;

        x := (lam12 - g_pi) / lamscale;
        y := sbet12a / betscale;

      else                   -- _f < 0
--        x = dlat, y = dlong

          cbet12a := cbet2 * cbet1 - sbet2 * sbet1;
          bet12a := new_arctan(sbet12a, cbet12a);

--        In the case of lon12 = 180, this repeats a calculation made in
--        Inverse.
        Lengths(g_n, g_pi + bet12a, sbet1, -cbet1, dn1, sbet2, cbet2, dn2,
                cbet1, cbet2, dummy, m12b, m0, FALSE, dummy1,dummy2,C1a, C2a);
                
        x := -1. + m12b/(cbet1 * cbet2 * m0 * g_pi);
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
          if x > g_tol1 then
            calp1 := 0.0;
          else
            calp1 := -1.;
          end if;
          if x > calp1 then
            calp1 := x;
          end if;
          salp1 :=   sqrt(1. - calp1*calp1);
        end if;
      else
       -- Estimate alp1, by solving the astroid problem.
        
        -- Could estimate alpha1 = theta + pi/2, directly, i.e.,
        --   calp1 = y/k; salp1 = -x/(1+k);  for _f >= 0
        --   calp1 = x/(1+k); salp1 = -y/k;  for _f < 0 (need to check)
        --
        -- However, it's better to estimate omg12 from astroid and use
        -- spherical formula to compute alp1.  This reduces the mean number of
        -- Newton iterations for astroid cases from 2.24 (min 0, max 6) to 2.12
        -- (min 0 max 5).  The changes in the number of iterations are as
        -- follows:
        --
        -- change percent
        --    1       5
        --    0      78
        --   -1      16
        --   -2       0.6
        --   -3       0.04
        --   -4       0.002
        --
        -- The histogram of iterations is (m = number of iterations estimating
        -- alp1 directly, n = number of iterations estimating via omg12, total
        -- number of trials = 148605):
        --
        --  iter    m      n
        --    0   148    186
        --    1 13046  13845
        --    2 93315 102225
        --    3 36189  32341
        --    4  5396      7
        --    5   455      1
        --    6    56      0
        --
        -- Because omg12 is near pi, estimate work with omg12a = pi - omg12

        k := Astroid(x, y);
--        estimate omg12a = pi - omg12

          if g_f >= 0.0 then
           omg12a := lamscale * (-x * k/(1. + k));
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
    
    if salp1 > 0. then
      SinCosNorm(salp1, calp1);
    else
      salp1 := 1.;
      calp1 := 0.0;
    end if;
    return sig12;

END InverseStart;
--
FUNCTION Lambda12(sbet1 NUMBER, cbet1 NUMBER,  dn1 NUMBER,
                   sbet2 NUMBER,  cbet2 NUMBER,  dn2 NUMBER,
                   salp1 NUMBER,  calp1 IN OUT NOCOPY NUMBER,
                   salp2 IN OUT NOCOPY NUMBER, calp2 IN OUT NOCOPY NUMBER,
                   sig12 IN OUT NOCOPY NUMBER,
                   ssig1 IN OUT NOCOPY NUMBER, csig1 IN OUT NOCOPY NUMBER,
                   ssig2 IN OUT NOCOPY NUMBER, csig2 IN OUT NOCOPY NUMBER,
                   eps IN OUT NOCOPY NUMBER, domg12 IN OUT NOCOPY NUMBER, 
                   diffp BOOLEAN, dlam12 IN OUT NOCOPY NUMBER,
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
     M12      NUMBER;
     M21      NUMBER;

BEGIN
--    dbms_output.put_line('in Lambda');
    if (sbet1 = 0.0 and calp1 = 0.0) then
--      Break degeneracy of equatorial line.  This case has already been handled.
      calp1 := -g_tiny;

    end if;

--      // sin(alp1) * cos(bet1) = sin(alp0),
      salp0 := salp1 * cbet1;
      calp0 := hypot(calp1, salp1 * sbet1); --// calp0 > 0


--    // tan(bet1) = tan(sig1) * cos(alp1)
--    // tan(omg1) = sin(alp0) * tan(sig1) = tan(omg1)=tan(alp1)*sin(bet1)
    ssig1 := sbet1;
    somg1 := salp0 * sbet1;
    csig1 := calp1 * cbet1;
    comg1 := csig1;
    SinCosNorm(ssig1, csig1);
    --SinCosNorm(somg1, comg1);   -- don't need to normalize

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
    -- SinCosNorm(somg2, comg2);  -- don't need to normalize

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
    C3f(eps, C3a);
--    for ii in 1..C3a.count loop
--       dbms_output.put_line('ii ' || ii || ' c3a ' || C3a(ii));
--    end loop;
    B312 := (SinCosSeries(TRUE,ssig2, csig2, C3a, g_nC3_-1) -
             SinCosSeries(TRUE,ssig1, csig1, C3a, g_nC3_-1));

    h0 := -g_f * A3f(eps);
--    dbms_output.put_line('omg12 ' || omg12 || ' domg12 ' || domg12);
--    dbms_output.put_line('salp0 ' || salp0);
--    dbms_output.put_line('h0 ' || h0);
--   dbms_output.put_line('sig12 ' || sig12);
--    dbms_output.put_line('B312 '||B312);
    domg12 := salp0 * h0 * (sig12 + B312);
    lam12 := omg12 + domg12;

    if (diffp) then
      if (calp2 = 0.0) then
        dlam12 := -2. * g_f1 * dn1 / sbet1;
      else

        Lengths(eps, sig12, ssig1, csig1, dn1,ssig2, csig2,dn2,
                cbet1, cbet2, dummy1, dlam12, dummy2, FALSE, M12,M21,C1a, C2a);
        dlam12 := dlam12 * g_f1/(calp2 * cbet2);
      end if;
    end if;
-- dbms_output.put_line('leaving Lambda' || lam12 || ' dlam12 ' || dlam12);
    return lam12;
END LAMBDA12;
--
PROCEDURE Lengths(eps NUMBER, sig12 NUMBER, ssig1 NUMBER, csig1 NUMBER, dn1 NUMBER,
                  ssig2 NUMBER, csig2 NUMBER,dn2 NUMBER,cbet1 NUMBER, cbet2 NUMBER,
                  s12b IN OUT NOCOPY NUMBER,m12b IN OUT NOCOPY NUMBER,
                  m0 IN OUT NOCOPY NUMBER,scalep BOOLEAN, M12 IN OUT NOCOPY NUMBER,
                  M21 IN OUT NOCOPY NUMBER,
                  C1a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, C2a IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) AS
-- New Version taken from Geographic Lib132

  A1m1        NUMBER;
  A2m1        NUMBER;
  AB1         NUMBER;
  AB2         NUMBER;
  J12         NUMBER;
  t           NUMBER;
  csig12      NUMBER;

BEGIN

-- Return m12b := (reduced length)/_b; also calculate s12b := distance/_b,
 -- and m0 := coefficient of secular term in expression for reduced length.

--dbms_output.put_line('in lengths ' || eps);
    C1f(eps, C1a);
    C2f(eps, C2a);
--     for ii in 1..C1a.count loop
--      dbms_output.put_line('C1a ' || C1a(ii) || ' C2a ' || C2a(ii));
--    end loop;

    A1m1 := A1m1f(eps);
    AB1 := (1. + A1m1) * (SinCosSeries(TRUE,ssig2, csig2, C1a, g_nC1_) -
                          SinCosSeries(TRUE,ssig1, csig1, C1a, g_nC1_));
    A2m1 := A2m1f(eps);
    AB2 := (1. + A2m1) * (SinCosSeries(TRUE,ssig2, csig2, C2a, g_nC2_) -
                          SinCosSeries(TRUE,ssig1, csig1, C2a, g_nC2_));
    m0 := A1m1 - A2m1;

--     dbms_output.put_line('m0 ' || m0 || ' A1m1 ' || A1M1 || ' AB1 ' || AB1 || ' Sig12 '|| sig12);
    J12 := m0 *sig12+ (AB1 - AB2);

--    Missing a factor of _b.
--    Add parens around (csig1 * ssig2) and (ssig1 * csig2) to ensure accurate
--    cancellation in the case of coincident points.

    
    m12b := dn2 * (csig1 * ssig2) - dn1 * (ssig1 * csig2) - csig1 * csig2 * J12;

--    dbms_output.put_line('m12a ' || m12b );      
--      dbms_output.put_line('sig12 ' ||sig12 || ' g_f1 ' || g_f1);
--      dbms_output.put_line('cbet1 ' ||cbet1 || ' csig1 ' || csig1 || ' ssig1 ' || ssig1);
--      dbms_output.put_line('cbet2 ' ||cbet2 || ' csig2 ' || csig2 || ' ssig2 ' || ssig2);
--      dbms_output.put_line('m12a ' || m12a || ' AB1 ' || AB1 || ' Ab2 ' || AB2);

--  Missing a factor of _b
    s12b :=  (1. + A1m1) * sig12 + AB1;
--    dbms_output.put_line('s12b ' || s12b ); 
    if (scalep) then
      csig12 := csig1 * csig2 + ssig1 * ssig2;
      t  := g_ep2 * (cbet1 - cbet2) * (cbet1 + cbet2) / (dn1 + dn2);
      M12 := csig12 + (t * ssig2 - csig2 * J12) * ssig1 / dn1;
      M21 := csig12 - (t * ssig1 - csig1 * J12) * ssig2 / dn2;
    end if;

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
--              as fast as possible (1000000 values in 4.8 seconds -
--              15 times faster than the built in
--              11G function atan2 which takes 16 seconds for 200000 values)
--              (10 times faster than the built in
--              10G function atan2 which takes 25 seconds for 200000 values) with
--              an accuracy of 1E-38. It surpasses the accuracy of the Oracle
--              library function.
--              The domain is unlimited. (Note this function gives no error
--              and returns zero when both x and y are zero.)
--
-- Method:      First the range is reduced and then the arctangent is computed
--              using the ratio of 2 polynomials derived from Legendre polynomials.
--              The result is then adjusted for the quadrant.

-- Accuracy:  The maximum error in the range - -infinity to infinity is < 1.E-38.
--            Tested against the ladder function and the very accurate calculator:
--            http://www.alpertron.com.ar/BIGCALC.HTM
--
-- Reference: Ulrich H Kurzweg and Sidey P. Timmins "A New Method for obtaining
--            High Accuracy Approximations to the Arctangent Function".
--            submitted to IEEE DSP, 2013
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
   complement  BOOLEAN          := FALSE;
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
        RETURN 90.0;
      else
      RETURN pibytwo;
      end if;
    END IF;
  END IF;


  IF xin = 0.0 or yin = 0.0 then
    x := 0.0;
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



-- These "ladder" steps are described in the reference, Kurzweg and Timmins.
-- They maintain p the argument to the F function > 50.


if x >= 0.6825 then
      IF x >=0.8 THEN -- [0.8, 1]
        if x >=0.93 then
        x := Reduce(x,0.965);
        region := region+ 0.7675883418326998940546972132559440844659;
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
        region := region+ 0.6174058917515726665197139448485095848569;
        end if;
      END IF;
elsif x >= 0.48 then
      IF x > 0.575 THEN  -- [.575,.6825]
        if x > 0.6275 then
        x := Reduce(x,0.655);
        region := region+ 0.5798821323938133197612842815963766066022798;
        else
        x := Reduce(x,0.6);
        region := region+ 0.5404195002705841554435783646085999101352;
        end if;
      ELSE              -- [.48,.575]
        if x > 0.525 then
        x := Reduce(x,0.55);
        region := region+ 0.5028432109278608273308820292452775557764;
        else
        x := Reduce(x,0.5);
        region := region+ 0.4636476090008061162142562314612144020285;
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

--   dbms_output.put_line('x now is ' || x); --to_char(x,'9999EEEE') || ' reg ' || region);

    if yin <> 0.0 and ((ABS(YIn) > ABS(XIn) and u < 1.) or (ABS(YIn) < ABS(XIn) and u > 1.)) then
     complement := TRUE;
     end if;



     if ABS(x) > 1.E10 then
        u := x*x;
        if ABS(x) > 1.E20 then
           result := 3.*x/(1.+3.*u) + region;
        else
           result := x*(55.+105*u)/( 9.+90.*u + 105*u*u) + region;        
        end if;
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

Function A3f(eps Number) return Number as
  
-- Evaluate sum(_A3x(k) * eps^k, k, 0, nA3x_-1) by Horner's method
    
    v  Number:= 0.0;
Begin
    
  for i in REVERSE 1..g_nA3x Loop  -- C goes from 0 to G_NA3x-1
      v := eps * v + g_A3x(i);  -- v = eps * v + _A3x[--i];
  end loop;
  
  return v;
  
End;
--

Procedure C3f(eps number,  c IN OUT NOCOPY Mdsys.sdo_list_type) AS
    -- Evaluate C3 coeffs by Horner's method
    -- Elements c(1) thru c(nC3_ - 1) are set
    -- The code is different for C3f and C4f because one has for example
    --  6*5/2 and the other has 6 * 7 /2

   t       NUMBER;
   mult    NUMBER := 1.;

   j       PLS_INTEGER;
   k       PLS_INTEGER;
   i       PLS_INTEGER;  
    
Begin

   j := g_nC3x+1;
   
   for kk in 1..g_nC3_-1 loop  
     k := g_nC3_-kk;
     t := 0.0;  
     for i in 1..g_nc3_-k loop
       j := j - 1;
--       dbms_output.put_line('J' || j || ' k ' || k || ' count ' || c.count || ' gc3x ' || g_C3x.count); 
       t := eps * t + g_C3x(j);
     end loop;
     c(k) := t;
   end loop;
 
    for k in 1..g_nC3_ Loop
      mult := mult*eps;
      c(k) := c(k)* mult;
    End Loop;
    
END C3f;
--
Procedure C4f(eps number,  c IN OUT NOCOPY Mdsys.sdo_list_type) As
    -- Evaluate C4 coeffs by Horner's method
    -- Elements c(1) thru c(nC4_ - 1) are set
     -- The code is different for C3f and C4f because one has for example
    --  6*5/2 and the other has 6 * 7 /2
   t       NUMBER;
   mult    NUMBER := 1.;

   j       PLS_INTEGER;
   k       PLS_INTEGER;
   i       PLS_INTEGER;
   
BEGIN

   j := g_nC4x+1;
   
   for kk in 1..g_nC4_ loop
     k := g_nC4_-kk+1;
     t := 0.0;  
     for i in  1..g_nc4_-k+1 loop
       j := j - 1;
--             dbms_output.put_line('J' || j || ' k ' || k || ' count ' || c.count || ' g_nc4 ' || g_nc4_); 
       t := eps * t + g_C4x(j);
     end loop;
     c(k) := t;
   end loop;


   for k in 2..g_nC4_ loop
      mult := mult*eps;
      c(k) := c(k) *mult;
   end loop;

End C4f;
--
  Function A1m1f(eps Number) Return Number AS
   -- The scale factor A1-1 := mean value of (d/dsigma)I1 - 1
   -- Generated by Maxima on 2010-09-04 10:26:17-04:00
   
      eps2  Number := eps*eps;
      t     Number := 0.0;
      valu  pls_integer := TRUNC(g_nA1_/2);
      
    BEGIN
    
    CASE valu
    
    WHEN 0 THEN NULL; 
    WHEN 1 THEN t := eps2/4.;
    WHEN 2 THEN t := eps2*(eps2+16.)/64.;
    WHEN 3 THEN t := eps2*(eps2*(eps2+4.)+64.)/256.;
    WHEN 4 THEN t := eps2*(eps2*(eps2*(25.*eps2+64.)+256.)+4096.)/16384.;

    ELSE
      dbms_output.put_line ('Bad value of g_nA1_'); 
    END CASE;
    
    return (t + eps) / (1. - eps);
  End;
--
  Procedure C1f(eps Number, c IN OUT NOCOPY Mdsys.Sdo_List_Type) as
  
  -- The coefficients C1(l) in the Fourier expansion of B1
 
      eps2    Number := eps*eps;
      d       Number := eps;
      
    BEGIN
    
    c := Mdsys.Sdo_List_Type();
    c.extend(g_nC1_+1);
    
 
    
    If g_NC1_ =0 Then 
      NULL;
    Elsif g_NC1_ = 1 Then
      c(1) := -d/2.;    
    Elsif g_NC1_ = 2 Then
      c(1) := -d/2.;
      d := d * eps;
      c(2) := -d/16.;
    
    Elsif g_NC1_ = 3 Then
      c(1) := d*(3.*eps2-8.)/16.;
      d := d * eps;
      c(2) := -d/16.;
      d := d * eps;
      c(3) := -d/48.;
     
    Elsif g_NC1_ = 4 Then
      c(1) := d*(3.*eps2-8.)/16.;
      d := d * eps;
      c(2) := d*(eps2-2.)/32.;
      d := d * eps;
      c(3) := -d/48.;
      d := d * eps;
      c(4) := -5.*d/512.;
      
    Elsif g_NC1_ = 5 Then
      c(1) := d*((6.-eps2)*eps2-16.)/32.;
      d := d * eps;
      c(2) := d*(eps2-2.)/32.;
      d := d * eps;
      c(3) := d*(9.*eps2-16.)/768.;
      d := d * eps;
      c(4) := -5.*d/512.;
      d := d * eps;
      c(5) := -7.*d/1280.;
      
    Elsif g_NC1_ = 6 Then
      c(1) := d*((6.-eps2)*eps2-16.)/32.;
      d := d * eps;
      c(2) := d*((64.-9.*eps2)*eps2-128.)/2048.;
      d := d * eps;
      c(3) := d*(9.*eps2-16.)/768.;
      d := d * eps;
      c(4) := d*(3.*eps2-5.)/512.;
      d := d * eps;
      c(5) := -7.*d/1280.;
      d := d * eps;
      c(6) := -7.*d/2048.;
  
    Elsif g_NC1_ = 7 Then
      c(1) := d*(eps2*(eps2*(19.*eps2-64.)+384.)-1024.)/2048.;
      d := d * eps;
      c(2) := d*((64.-9.*eps2)*eps2-128.)/2048.;
      d := d * eps;
      c(3) := d*((72.-9.*eps2)*eps2-128.)/6144.;
      d := d * eps;
      c(4) := d*(3.*eps2-5.)/512.;
      d := d * eps;
      c(5) := d*(35.*eps2-56.)/10240.;
      d := d * eps;
      c(6) := -7.*d/2048.;
      d := d * eps;
      c(7) := -33.*d/14336.;
      
    Elsif g_NC1_ = 8 Then
      c(1) := d*(eps2*(eps2*(19.*eps2-64.)+384.)-1024.)/2048.;
      d := d * eps;
      c(2) := d*(eps2*(eps2*(7.*eps2-18.)+128.)-256.)/4096.;
      d := d * eps;
      c(3) := d*((72.-9.*eps2)*eps2-128.)/6144.;
      d := d * eps;
      c(4) := d*((96.-11.*eps2)*eps2-160.)/16384.;
      d := d * eps;
      c(5) := d*(35.*eps2-56.)/10240.;
      d := d * eps;
      c(6) := d*(9.*eps2-14.)/4096.;
      d := d * eps;
      c(7) := -33.*d/14336.;
      d := d * eps;
      c(8) := -429.*d/262144.;
      
    Else
       Dbms_output.put_line('Bad value of nC1_'||g_nC1_);
    End If;
    
  END C1f;

 
Procedure C1pf(eps number,c IN OUT NOCOPY Mdsys.Sdo_List_Type) AS
 -- The coefficients C1p(l) in the Fourier expansion of B1p 
 
      eps2  Number:= eps*eps;
      d     Number:= eps;
Begin

    c := Mdsys.Sdo_List_Type();
    c.extend(g_nC1p_+1);
    
    c(1) := 0.0;
    
    If g_nC1p_ = 0 Then
      NULL;
    ElsIf g_nC1p_ = 1 Then
      c(2) := d/2.;
      
    ElsIf g_nC1p_ =  2 Then
      c(2) := d/2.;
      d := d * eps;
      c(3) := 5.*d/16.;
     
    ElsIf g_nC1p_ = 3 Then
      c(2) := d*(16.-9.*eps2)/32.;
      d := d * eps;
      c(3) := 5.*d/16.;
      d := d * eps;
      c(4) := 29.*d/96.;
     
    ElsIf g_nC1p_ = 4 Then
      c(2) := d*(16.-9.*eps2)/32.;
      d := d * eps;
      c(3) := d*(30.-37.*eps2)/96.;
      d := d * eps;
      c(4) := 29.*d/96.;
      d := d * eps;
      c(5) := 539.*d/1536.;
      
    ElsIf g_nC1p_ = 5 Then
      c(2) := d*(eps2*(205.*eps2-432.)+768.)/1536.;
      d := d * eps;
      c(3) := d*(30.-37.*eps2)/96.;
      d := d * eps;
      c(4) := d*(116.-225.*eps2)/384.;
      d := d * eps;
      c(5) := 539.*d/1536.;
      d := d * eps;
      c(6) := 3467.*d/7680.;
     
    ElsIf g_nC1p_ = 6 Then
      c(2) := d*(eps2*(205.*eps2-432.)+768.)/1536.;
      d := d * eps;
      c(3) := d*(eps2*(4005.*eps2-4736.)+3840.)/12288.;
      d := d * eps;
      c(4) := d*(116.-225.*eps2)/384.;
      d := d * eps;
      c(5) := d*(2695.-7173.*eps2)/7680.;
      d := d * eps;
      c(6) := 3467.*d/7680.;
      d := d * eps;
      c(7) := 38081.*d/61440.;
      
    ElsIf g_nC1p_ = 7 Then
      c(2) := d*(eps2*((9840.-4879.*eps2)*eps2-20736.)+36864.)/73728.;
      d := d * eps;
      c(3) := d*(eps2*(4005.*eps2-4736.)+3840.)/12288.;
      d := d * eps;
      c(4) := d*(eps2*(8703.*eps2-7200.)+3712.)/12288.;
      d := d * eps;
      c(5) := d*(2695.-7173.*eps2)/7680.;
      d := d * eps;
      c(6) := d*(41604.-141115.*eps2)/92160.;
      d := d * eps;
      c(7) := 38081.*d/61440.;
      d := d * eps;
      c(8) := 459485.*d/516096.;
      
    ElsIf g_nC1p_ = 8 Then
      c(2) := d*(eps2*((9840.-4879.*eps2)*eps2-20736.)+36864.)/73728.;
      d := d * eps;
      c(3) := d*(eps2*((120150.-86171.*eps2)*eps2-142080.)+115200.)/368640.;
      d := d * eps;
      c(4) := d*(eps2*(8703.*eps2-7200.)+3712.)/12288.;
      d := d * eps;
      c(5) := d*(eps2*(1082857.*eps2-688608.)+258720.)/737280.;
      d := d * eps;
      c(6) := d*(41604.-141115.*eps2)/92160.;
      d := d * eps;
      c(7) := d*(533134.-2200311.*eps2)/860160.;
      d := d * eps;
      c(8) := 459485.*d/516096.;
      d := d * eps;
      c(9) := 109167851.*d/82575360.;
     
    Else
      Dbms_output.put_line('Bad value of nC1p_'|| g_nC1p_);
  End If;
  End C1pf;
-- 
  Function A2m1f(eps Number) Return Number AS
  -- The scale factor A2-1 := mean value of (d/dsigma)I2 - 1
  
      eps2    Number := eps*eps;
      t       Number := 0.0;
      
    BEGIN
    
    CASE (g_nA2_/2)
  
    WHEN  0 THEN NULL;  
    WHEN  1 THEN t := eps2/4.;     
    WHEN  2 THEN t := eps2*(9.*eps2+16.)/64.;     
    WHEN  3 THEN t := eps2*(eps2*(25.*eps2+36.)+64.)/256.;     
    WHEN  4 THEN t := eps2*(eps2*(eps2*(1225.*eps2+1600.)+2304.)+4096.)/16384.;
      
    ELSE
      dbms_output.put_line('Bad value of g_nA2_'|| g_na2_);
    END CASE;
    
    return t * (1. - eps) - eps;
    
  END A2M1F;
-- 
  Procedure C2f(eps Number, c IN OUT NOCOPY Mdsys.Sdo_list_Type) AS
   -- The coefficients C2(l) in the Fourier expansion of B2
 
      eps2    Number := eps*eps;
      d       Number := eps;
      
    BEGIN
    
    c:= Mdsys.sdo_list_Type();
    c.extend(g_NC2_+1);
    
    
    If g_nC2_ = 1 Then 
      c(1) := d/2.;     
    Elsif g_nC2_ =  2 Then
      c(1) := d/2.;
      d := d * eps;
      c(2) := 3.*d/16.;
     
    Elsif g_nC2_ =  3 Then
      c(1) := d*(eps2+8.)/16.;
      d := d * eps;
      c(2) := 3.*d/16.;
      d := d * eps;
      c(3) := 5.*d/48.;
  
    Elsif g_nC2_ =  4 Then
      c(1) := d*(eps2+8.)/16.;
      d := d * eps;
      c(2) := d*(eps2+6.)/32.;
      d := d * eps;
      c(3) := 5.*d/48.;
      d := d * eps;
      c(4) := 35.*d/512.;
     
    Elsif g_nC2_ =  5 Then
      c(1) := d*(eps2*(eps2+2.)+16.)/32.;
      d := d * eps;
      c(2) := d*(eps2+6.)/32.;
      d := d * eps;
      c(3) := d*(15.*eps2+80.)/768.;
      d := d * eps;
      c(4) := 35.*d/512.;
      d := d * eps;
      c(5) := 63.*d/1280.;
      
    Elsif g_nC2_ =  6 Then
      c(1) := d*(eps2*(eps2+2.)+16.)/32.;
      d := d * eps;
      c(2) := d*(eps2*(35.*eps2+64.)+384.)/2048.;
      d := d * eps;
      c(3) := d*(15.*eps2+80.)/768.;
      d := d * eps;
      c(4) := d*(7.*eps2+35.)/512.;
      d := d * eps;
      c(5) := 63.*d/1280.;
      d := d * eps;
      c(6) := 77.*d/2048.;
      
    Elsif g_nC2_ =  7 Then
      c(1) := d*(eps2*(eps2*(41.*eps2+64.)+128.)+1024.)/2048.;
      d := d * eps;
      c(2) := d*(eps2*(35.*eps2+64.)+384.)/2048.;
      d := d * eps;
      c(3) := d*(eps2*(69.*eps2+120.)+640.)/6144.;
      d := d * eps;
      c(4) := d*(7.*eps2+35.)/512.;
      d := d * eps;
      c(5) := d*(105.*eps2+504.)/10240.;
      d := d * eps;
      c(6) := 77.*d/2048.;
      d := d * eps;
      c(7) := 429.*d/14336.;
    
    Elsif g_nC2_ =  8 Then
      c(1) := d*(eps2*(eps2*(41.*eps2+64.)+128.)+1024.)/2048.;
      d := d * eps;
      c(2) := d*(eps2*(eps2*(47.*eps2+70.)+128.)+768.)/4096.;
      d := d * eps;
      c(3) := d*(eps2*(69.*eps2+120.)+640.)/6144.;
      d := d * eps;
      c(4) := d*(eps2*(133.*eps2+224.)+1120.)/16384.;
      d := d * eps;
      c(5) := d*(105.*eps2+504.)/10240.;
      d := d * eps;
      c(6) := d*(33.*eps2+154.)/4096.;
      d := d * eps;
      c(7) := 429.*d/14336.;
      d := d * eps;
      c(8) := 6435.*d/262144.;

    Else
      Dbms_output.put_line('Bad value of nC2_' || g_nc2_);
    End if;
   
    
  END;
--  
  Procedure A3coeff  AS
  -- The scale factor A3 := mean value of (d/dsigma)I3
  
  Begin
    
    g_A3x.extend(g_nA3_);
    g_A3x(1) := 0.0;
    
    If (g_nA3_) = 0 Then    
      NULL;
    Elsif g_nA3_ = 1 then
      g_A3x(1) := 1.; 
      
    Elsif g_nA3_ = 2 then
      g_A3x(1) := 1.;
      g_A3x(2) := -1./2.;
 
    Elsif g_nA3_ = 3 then
      g_A3x(1) := 1.;
      g_A3x(2) := (g_n-1.)/2.;
      g_A3x(3) := -1./4.;
 
    Elsif g_nA3_ = 4 then
      g_A3x(1) := 1.;
      g_A3x(2) := (g_n-1.)/2.;
      g_A3x(3) := (-g_n-2)/8.;
      g_A3x(4) := -1./16.;
  
    Elsif g_nA3_ = 5 then
      g_A3x(1) := 1.;
      g_A3x(2) := (g_n-1.)/2.;
      g_A3x(3) := (g_n*(3.*g_n-1)-2.)/8.;
      g_A3x(4) := (-3.*g_n-1.)/16.;
      g_A3x(5) := -3./64.;
  
    Elsif g_nA3_ =  6 then
      g_A3x(1) := 1.;
      g_A3x(2) := (g_n-1.)/2.;
      g_A3x(3) := (g_n*(3.*g_n-1.)-2.)/8.;
      g_A3x(4) := ((-g_n-3.)*g_n-1.)/16.;
      g_A3x(5) := (-2.*g_n-3.)/64.;
      g_A3x(6) := -3./128.;
   
    Elsif g_nA3_ = 7 then
      g_A3x(1) := 1.;
      g_A3x(2) := (g_n-1.)/2.;
      g_A3x(3) := (g_n*(3.*g_n-1)-2.)/8.;
      g_A3x(4) := (g_n*(g_n*(5.*g_n-1.)-3.)-1.)/16.;
      g_A3x(5) := ((-10.*g_n-2.)*g_n-3.)/64.;
      g_A3x(6) := (-5.*g_n-3)/128.;
      g_A3x(7) := -5./256.;
      
    Elsif g_nA3_ =  8 then
      g_A3x(1) := 1.;
      g_A3x(2) := (g_n-1.)/2.;
      g_A3x(3) := (g_n*(3.*g_n-1.)-2.)/8.;
      g_A3x(4) := (g_n*(g_n*(5.*g_n-1.)-3.)-1.)/16.;
      g_A3x(5) := (g_n*((-5.*g_n-20.)*g_n-4.)-6.)/128.;
      g_A3x(6) := ((-5.*g_n-10.)*g_n-6.)/256.;
      g_A3x(7) := (-15.*g_n-20.)/1024.;
      g_A3x(8) := -25./2048.;

    Else
      Dbms_output.put_line('Bad value of nA3_'|| g_NA3_);
    End If;
  END A3Coeff;
-- 
  Procedure C3coeff AS
   -- The coefficients C3(l) in the Fourier expansion of B3 
   -- This array start at zero in C
   
    Begin
    
    g_C3x.extend(g_nC3x);

    
    If g_nC3_ <= 1 Then
      NULL;     
    ElsIf g_nC3_ =2 Then
      g_C3x(1) := 1./4.;
      
    Elsif g_nC3_ = 3 Then
      g_C3x(1) := (1.-g_n)/4.;
      g_C3x(2) := 1./8.;
      g_C3x(3) := 1./16.;
      
    Elsif g_nC3_ = 4 Then
      g_C3x(1) := (1.-g_n)/4.;
      g_C3x(2) := 1./8.;
      g_C3x(3) := 3./64.;
      g_C3x(4) := (2.-3.*g_n)/32.;
      g_C3x(5) := 3./64.;
      g_C3x(6) := 5./192.;
     
    Elsif g_nC3_ = 5 Then
      g_C3x(1) := (1.-g_n)/4.;
      g_C3x(2) := (1.-g_n*g_n)/8.;
      g_C3x(3) := (3.*g_n+3.)/64.;
      g_C3x(4) := 5./128.;
      g_C3x(5) := ((g_n-3.)*g_n+2.)/32.;
      g_C3x(6) := (3.-2.*g_n)/64.;
      g_C3x(7) := 3./128.;
      g_C3x(8) := (5.-9.*g_n)/192.;
      g_C3x(9) := 3./128.;
      g_C3x(10) := 7./512.;
      
    Elsif g_nC3_ = 6 Then
     g_C3x(1) := (1.-g_n)/4.;
     g_C3x(2) := (1.-g_n*g_n)/8.;
     g_C3x(3) := ((3.-g_n)*g_n+3.)/64.;
     g_C3x(4) := (2.*g_n+5.)/128.;
     g_C3x(5) := 3./128.;
     g_C3x(6) := ((g_n-3.)*g_n+2.)/32.;
     g_C3x(7) := ((-3.*g_n-2.)*g_n+3.)/64.;
     g_C3x(8) := (g_n+3.)/128.;
     g_C3x(9) := 5./256.;
     g_C3x(10) := (g_n*(5.*g_n-9.)+5.)/192.;
     g_C3x(11) := (9.-10.*g_n)/384.;
     g_C3x(12) := 7./512.;
     g_C3x(13) := (7.-14.*g_n)/512.;
     g_C3x(14) := 7./512.;
     g_C3x(15) := 21./2560.;
      
    Elsif g_nC3_ = 7 Then
     g_C3x(1) := (1.-g_n)/4.;
     g_C3x(2) := (1.-g_n*g_n)/8.;
     g_C3x(3) := (g_n*((-5.*g_n-1.)*g_n+3.)+3.)/64.;
     g_C3x(4) := (g_n*(2.*g_n+2.)+5.)/128.;
     g_C3x(5) := (11.*g_n+12.)/512.;
     g_C3x(6) := 21./1024.;
     g_C3x(7) := ((g_n-3.)*g_n+2.)/32.;
     g_C3x(8) := (g_n*(g_n*(2.*g_n-3.)-2.)+3.)/64.;
     g_C3x(9) := ((2.-9.*g_n)*g_n+6.)/256.;
     g_C3x(10) := (g_n+5.)/256.;
     g_C3x(11) := 27./2048.;
     g_C3x(12) := (g_n*((5.-g_n)*g_n-9.)+5.)/192.;
     g_C3x(13) := ((-6.*g_n-10)*g_n+9.)/384.;
     g_C3x(14) := (21.-4.*g_n)/1536.;
     g_C3x(15) := 3./256.;
     g_C3x(16) := (g_n*(10.*g_n-14.)+7.)/512.;
     g_C3x(17) := (7.-10.*g_n)/512.;
     g_C3x(18) := 9./1024.;
     g_C3x(19) := (21.-45.*g_n)/2560.;
     g_C3x(20) := 9./1024.;
     g_C3x(21) := 11./2048.;
      
    Elsif g_nC3_ = 8 Then
      g_C3x(1) := (1.-g_n)/4.;
      g_C3x(2) := (1.-g_n*g_n)/8.;
      g_C3x(3) := (g_n*((-5.*g_n-1.)*g_n+3.)+3.)/64.;
      g_C3x(4) := (g_n*((2.-2.*g_n)*g_n+2.)+5.)/128.;
      g_C3x(5) := (g_n*(3.*g_n+11.)+12.)/512.;
      g_C3x(6) := (10.*g_n+21.)/1024.;
      g_C3x(7) := 243./16384.;
      g_C3x(8) := ((g_n-3.)*g_n+2.)/32.;
      g_C3x(9) := (g_n*(g_n*(2.*g_n-3.)-2.)+3.)/64.;
      g_C3x(10) := (g_n*((-6.*g_n-9.)*g_n+2)+6)/256.;
      g_C3x(11) := ((1.-2.*g_n)*g_n+5.)/256.;
      g_C3x(12) := (69.*g_n+108.)/8192.;
      g_C3x(13) := 187./16384.;
      g_C3x(14) := (g_n*((5.-g_n)*g_n-9.)+5.)/192.;
      g_C3x(15) := (g_n*(g_n*(10.*g_n-6.)-10.)+9.)/384.;
      g_C3x(16) := ((-77.*g_n-8.)*g_n+42.)/3072.;
      g_C3x(17) := (12.-g_n)/1024.;
      g_C3x(18) := 139./16384.;
      g_C3x(19) := (g_n*((20.-7.*g_n)*g_n-28.)+14.)/1024.;
      g_C3x(20) := ((-7.*g_n-40.)*g_n+28.)/2048.;
      g_C3x(21) := (72.-43.*g_n)/8192.;
      g_C3x(22) := 127./16384.;
      g_C3x(23) := (g_n*(75.*g_n-90.)+42.)/5120.;
      g_C3x(24) := (9.-15.*g_n)/1024.;
      g_C3x(25) := 99./16384.;
      g_C3x(26) := (44.-99.*g_n)/8192.;
      g_C3x(27) := 99./16384.;
      g_C3x(28) := 429./114688.;
     
    Else
      Dbms_output.put_line('Bad value of nC3_'||g_nc3_);
    End if;
  End C3Coeff;
--
  Procedure C4coeff As
  -- Generated by Maxima on 2012-10-19 08:02:34-04:00

  -- The coefficients C4(l) in the Fourier expansion of I4 
  -- This array starts at zero in C
  
  Begin
    
    g_C4x.extend(g_nC4x);
    If g_nC4_ = 0 then
      NULL;
    ElsIf g_nC4_ = 1 then
      g_C4x(1) := 2./3.;
      
    ElsIf g_nC4_ = 2 then
      g_C4x(1) := (10.-4.*g_n)/15.;
      g_C4x(2) := -1./5.;
      g_C4x(3) := 1./45.;
     
    ElsIf g_nC4_ =  3 then
      g_C4x(1) := (g_n*(8.*g_n-28.)+70.)/105.;
      g_C4x(2) := (16.*g_n-7.)/35.;
      g_C4x(3) := -2./105.;
      g_C4x(4) := (7.-16.*g_n)/315.;
      g_C4x(5) := -2./105.;
      g_C4x(6) := 4./525.;
      
    ElsIf g_nC4_ =  4 then
      g_C4x(1) := (g_n*(g_n*(4.*g_n+24.)-84.)+210.)/315.;
      g_C4x(2) := ((48.-32.*g_n)*g_n-21.)/105.;
      g_C4x(3) := (-32.*g_n-6.)/315.;
      g_C4x(4) := 11./315.;
      g_C4x(5) := (g_n*(32.*g_n-48.)+21.)/945.;
      g_C4x(6) := (64.*g_n-18.)/945.;
      g_C4x(7) := -1./105.;
      g_C4x(8) := (12.-32.*g_n)/1575.;
      g_C4x(9) := -8./1575.;
      g_C4x(10) := 8./2205.;
      
    ElsIf g_nC4_ =  5 then
      g_C4x(1) := (g_n*(g_n*(g_n*(16.*g_n+44.)+264.)-924.)+2310.)/3465.;
      g_C4x(2) := (g_n*(g_n*(48.*g_n-35.)+528.)-231.)/1155.;
      g_C4x(3) := (g_n*(1088.*g_n-352.)-66.)/3465.;
      g_C4x(4) := (121.-368.*g_n)/3465.;
      g_C4x(5) := 4./1155.;
      g_C4x(6) := (g_n*((352.-48.*g_n)*g_n-528.)+231.)/10395.;
      g_C4x(7) := ((704.-896.*g_n)*g_n-198.)/10395.;
      g_C4x(8) := (80.*g_n-99.)/10395.;
      g_C4x(9) := 4./1155.;
      g_C4x(10) := (g_n*(320.*g_n-352.)+132.)/17325.;
      g_C4x(11) := (384.*g_n-88.)/17325.;
      g_C4x(12) := -.8/1925.;
      g_C4x(13) := (88.-256.*g_n)/24255.;
      g_C4x(14) := -16./8085.;
      g_C4x(15) := 64./31185.;
      
    ElsIf g_nC4_ =   6 then
      g_C4x(1) := (g_n*(g_n*(g_n*(g_n*(100.*g_n+208.)+572.)+3432.)-12012.)+30030.)/45045.;
      g_C4x(2) := (g_n*(g_n*(g_n*(64.*g_n+624.)-4576.)+6864.)-3003.)/15015.;
      g_C4x(3) := (g_n*((14144.-10656.*g_n)*g_n-4576.)-858)/45045.;
      g_C4x(4) := ((-224.*g_n-4784.)*g_n+1573.)/45045.;
      g_C4x(5) := (1088.*g_n+156.)/45045.;
      g_C4x(6) := 97./15015.;
      g_C4x(7) := (g_n*(g_n*((-64.*g_n-624.)*g_n+4576.)-6864.)+3003.)/135135.;
      g_C4x(8) := (g_n*(g_n*(5952.*g_n-11648.)+9152.)-2574.)/135135.;
      g_C4x(9) := (g_n*(5792.*g_n+1040.)-1287.)/135135.;
      g_C4x(10) := (468.-2944.*g_n)/135135.;
      g_C4x(11) := 1./9009.;
      g_C4x(12) := (g_n*((4160.-1440.*g_n)*g_n-4576.)+1716.)/225225.;
      g_C4x(13) := ((4992.-8448.*g_n)*g_n-1144)/225225.;
      g_C4x(14) := (1856.*g_n-936.)/225225.;
      g_C4x(15) := 8./10725.;
      g_C4x(16) := (g_n*(3584.*g_n-3328.)+1144.)/315315.;
      g_C4x(17) := (1024.*g_n-208.)/105105.;
      g_C4x(18) := -136./63063.;
      g_C4x(19) := (832.-2560.*g_n)/405405.;
      g_C4x(20) := -128./135135.;
      g_C4x(21) := 128./99099.;
      
    ElsIf g_nC4_ =   7 then
      g_C4x(1) := (g_n*(g_n*(g_n*(g_n*(g_n*(56.*g_n+100.)+208.)+572.)+3432.)-12012.)+30030.)/
        45045.;
      g_C4x(2) := (g_n*(g_n*(g_n*(g_n*(16.*g_n+64.)+624.)-4576.)+6864.)-3003.)/15015.;
      g_C4x(3) := (g_n*(g_n*(g_n*(1664.*g_n-10656.)+14144.)-4576.)-858.)/45045.;
      g_C4x(4) := (g_n*(g_n*(10736.*g_n-224.)-4784.)+1573.)/45045.;
      g_C4x(5) := ((1088.-4480.*g_n)*g_n+156.)/45045.;
      g_C4x(6) := (291.-464.*g_n)/45045.;
      g_C4x(7) := 10./9009.;
      g_C4x(8) := (g_n*(g_n*(g_n*((-16.*g_n-64.)*g_n-624.)+4576.)-6864.)+3003.)/135135.;
      g_C4x(9) := (g_n*(g_n*((5952.-768.*g_n)*g_n-11648.)+9152.)-2574.)/135135.;
      g_C4x(10) := (g_n*((5792.-10704.*g_n)*g_n+1040.)-1287.)/135135.;
      g_C4x(11) := (g_n*(3840.*g_n-2944.)+468.)/135135.;
      g_C4x(12) := (112.*g_n+15.)/135135.;
      g_C4x(13) := 10./9009.;
      g_C4x(14) := (g_n*(g_n*(g_n*(128.*g_n-1440.)+4160.)-4576.)+1716.)/225225.;
      g_C4x(15) := (g_n*(g_n*(6784.*g_n-8448.)+4992.)-1144.)/225225.;
      g_C4x(16) := (g_n*(1664.*g_n+185.)-936.)/225225.;
      g_C4x(17) := (168.-1664.*g_n)/225225.;
      g_C4x(18) := -4./25025.;
      g_C4x(19) := (g_n*((3584.-1792.*g_n)*g_n-3328.)+1144.)/315315.;
      g_C4x(20) := ((1024.-2048.*g_n)*g_n-208)/105105.;
      g_C4x(21) := (1792.*g_n-680.)/315315.;
      g_C4x(22) := 64./315315.;
      g_C4x(23) := (g_n*(3072.*g_n-2560.)+832.)/405405.;
      g_C4x(24) := (2048.*g_n-384.)/405405.;
      g_C4x(25) := -512./405405.;
      g_C4x(26) := (640.-2048.*g_n)/495495.;
      g_C4x(27) := -256./495495.;
      g_C4x(28) := 512./585585.;
      
    ElsIf g_nC4_ =   8 then
      g_C4x(1) := (g_n*(g_n*(g_n*(g_n*(g_n*(g_n*(588.*g_n+952.)+1700.)+3536.)+9724.)+58344.)-
        204204.)+510510.)/765765.;
      g_C4x(2) := (g_n*(g_n*(g_n*(g_n*(g_n*(96.*g_n+272.)+1088.)+10608.)-77792.)+116688.)-
        51051.)/255255.;
      g_C4x(3) := (g_n*(g_n*(g_n*(g_n*(3232.*g_n+28288.)-181152.)+240448.)-77792.)-14586.)/
        765765.;
      g_C4x(4) := (g_n*(g_n*((182512.-154048.*g_n)*g_n-3808.)-81328.)+26741.)/765765.;
      g_C4x(5) := (g_n*(g_n*(12480.*g_n-76160.)+18496.)+2652.)/765765.;
      g_C4x(6) := (g_n*(20960.*g_n-7888.)+4947.)/765765.;
      g_C4x(7) := (4192.*g_n+850.)/765765.;
      g_C4x(8) := 193./85085.;
      g_C4x(9) := (g_n*(g_n*(g_n*(g_n*((-96.*g_n-272.)*g_n-1088.)-10608.)+77792.)-116688.)+
        51051.)/2297295.;
      g_C4x(10) := (g_n*(g_n*(g_n*((-1344.*g_n-13056.)*g_n+101184.)-198016.)+155584.)-43758.)/
        2297295.;
      g_C4x(11) := (g_n*(g_n*(g_n*(103744.*g_n-181968.)+98464.)+17680.)-21879)/2297295.;
      g_C4x(12) := (g_n*(g_n*(52608.*g_n+65280.)-50048.)+7956.)/2297295.;
      g_C4x(13) := ((1904.-39840.*g_n)*g_n+255.)/2297295.;
      g_C4x(14) := (510.-1472.*g_n)/459459.;
      g_C4x(15) := 349./2297295.;
      g_C4x(16) := (g_n*(g_n*(g_n*(g_n*(160.*g_n+2176.)-24480.)+70720.)-77792.)+29172.)/
        3828825.;
      g_C4x(17) := (g_n*(g_n*((115328.-41472.*g_n)*g_n-143616.)+84864.)-19448.)/3828825.;
      g_C4x(18) := (g_n*((28288.-126528.*g_n)*g_n+31552.)-15912.)/3828825.;
      g_C4x(19) := (g_n*(64256.*g_n-28288.)+2856.)/3828825.;
      g_C4x(20) := (-928.*g_n-612.)/3828825.;
      g_C4x(21) := 464./1276275.;
      g_C4x(22) := (g_n*(g_n*(g_n*(7168.*g_n-30464.)+60928.)-56576.)+19448.)/5360355.;
      g_C4x(23) := (g_n*(g_n*(35840.*g_n-34816.)+17408.)-3536.)/1786785.;
      g_C4x(24) := ((30464.-2560.*g_n)*g_n-11560.)/5360355.;
      g_C4x(25) := (1088.-16384.*g_n)/5360355.;
      g_C4x(26) := -16./97461.;
      g_C4x(27) := (g_n*((52224.-32256.*g_n)*g_n-43520.)+14144.)/6891885.;
      g_C4x(28) := ((34816.-77824.*g_n)*g_n-6528.)/6891885.;
      g_C4x(29) := (26624.*g_n-8704.)/6891885.;
      g_C4x(30) := 128./2297295.;
      g_C4x(31) := (g_n*(45056.*g_n-34816.)+10880.)/8423415.;
      g_C4x(32) := (24576.*g_n-4352.)/8423415.;
      g_C4x(33) := -6784./8423415.;
      g_C4x(34) := (8704.-28672.*g_n)/9954945.;
      g_C4x(35) := -1024./3318315.;
      g_C4x(36) := 1024./1640925.;
      
    Else
      Dbms_output.put_line('Bad value of nC4_'|| g_nC4_);
    End If;
  End;

--} -- namespace GeographicLib
procedure test_to_gnomonic as


   x  number;
   y  number;
   azi  number;
   rk   number;
begin

-- Center of the projection, point to project
   to_Gnomonic(0,0,30.,20,x,y,azi,rk);
   dbms_output.put_line('X ' || round(x,25) || ' Y ' || round(y,25));
end ;
END GZ_GEODESIC;
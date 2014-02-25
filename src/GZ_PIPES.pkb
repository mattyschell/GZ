create or replace
PACKAGE BODY GZ_PIPES AS


--==============================================================================
-- A package to
-- Capabilities:
--       1) Get minimum angle in a geometry  -> Get_Small_Angle
--       2) Get small angle geometry(s) gets a "V" enclosing each smallest angle
--                                           --> Get_Small_Angle_Geom



-- Lower level capabilities:
--    1) Get all the angles in a geometry.  --> Get_Angles.
--                           returns and Angle list when smallest =-1
--                           returns the smallest Angle found and its position 
--                                                  when smallest =0.
--                           returns the  Angle found <= smallest and its position 
--                                                  when smallest > 0.
--

FUNCTION Measure_Gap(Geom IN MDSYS.SDO_GEOMETRY,pwidth NUMBER default 100.) RETURN NUMBER AS

  Xys               MDSYS.SDO_ORDINATE_ARRAY:= Geom.SDO_ORDINATES;
  Info              MDSYS.SDO_ELEM_INFO_ARRAY := Geom.SDO_ELEM_INFO; 
  Bad_Segs          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  
  
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;
  x3                NUMBER;
  y3                NUMBER;
  width             NUMBER;
  cab               NUMBER;
  d21               NUMBER;
  d31               NUMBER;
  d32               NUMBER;
  dx21              NUMBER;
  dy21              NUMBER;
  dx31              NUMBER;
  dy31              NUMBER;
  dx32              NUMBER;
  dy32              NUMBER;
  cos_lat           NUMBER;
  sin_lat           NUMBER;
  D2                NUMBER;
  L_Length          NUMBER;
  cutoff_width      NUMBER := pwidth;
  n                 PLS_INTEGER;
  no_info           PLS_INTEGER;
  bad_ptr           PLS_INTEGER;
BEGIN


  n := TRUNC(XYS.count/2);
  
  no_info := TRUNC(Info.count/3);
  Bad_segs.extend(no_info);
  
 -- First make a list of segments that dont exist from the Info (last vertex of each ring)
 
  For ii in 2..no_info loop 
   Bad_segs(ii-1) := TRUNC((Info(ii*3-2)+1)/2)-1;  -- point to non-existent segment
  End loop;
  Bad_segs(Bad_segs.count) := n;   -- This vertex does NOT exist
  
  
  sin_lat := quick_sincos(Xys(2),cos_lat,'Y');
  D2 := meters_to_degrees('Y',cutoff_width,XYs(2));
--  dbms_output.put_line('D2 ' || D2);
  D2 := D2*D2;
-- dbms_output.put_line('D2 ' || D2  || ' cos_lat ' || round(cos_lat,9)); 
--  cos_lat := 1.;
--  d2 := 1.;
  
  For ij in 1..n Loop  -- This is the vertex loop
  
   x1 := Xys(ij*2-1);
   y1 := Xys(ij*2);
   x3 := x1;
   y3 := y1;
   bad_ptr := 1;
--    dbms_output.put_line('IJ ' || ij || ' n ' || n);
  for ii in 1..n Loop   -- This is the segment loop
    
    x2 := x3;
    y2 := y3;
    x3 := Xys(ii*2-1);
    y3 := Xys(ii*2);
--    dbms_output.put_line('ij ' || ij || ' ii ' || ii);
    if ii = Bad_segs(bad_ptr) then
      bad_ptr := bad_ptr+1;
      continue;
    end if;
    if ii <> ij and (x3 <> x2 or y3 <> y2) and (x1 <> x2 or y1 <> y2) and (x1 <> x3 or y1 <> y3) then

-- This is the sine rule combined with the cosine rule such that D (the height 
-- of the triangle) = a sin(alpha) where alpha is the angle between a and c.
-- Cosine rule: b*b = c*c +a*a -2ac cos(alpha)

-- D = a sin(alpha) = a sqrt(1-cos(alpha)*cos(alpha)) = a sqrt(1- (c*c+a*a-b*b)^2))
--                                                         -----------------------
--                                                              2ac

--                1
--                +
--       a     .  |    .     b
--           .    D         .
--         .      |             .
--        +----------------------+
--      2            c           3
 
      dx21 := (x2-x1)*cos_lat;
      dy21 := y2-y1;
      
 
      dx31 := (x3-x1)*cos_lat;
      dy31 := y3-y1;
      

      dx32 := (x3-x2)*cos_lat;
      dy32 := y3-y2;
 --     L_length := ABS(dx31) + ABS(dy31);
 --     IF L_Length <= ABS(dx21) + ABS(dy21) or L_Length <= ABS(dx32) + ABS(dy32) THEN
 
-- generate deltas squared (read d21 as d21 squared)

      d21 := dx21*dx21 + dy21*dy21;
      d31 := dx31*dx31 + dy31*dy31;
      d32 := dx32*dx32 + dy32*dy32;
      cab := (d32+d21 -d31)/(2.*sqrt(d32));
      

      if d21 > d32 or d31 > d32 or (d21 - cab*cab) > D2 then
--      dbms_output.put_line('sqrt(d21 minus cab*cab) ' || round(sqrt(d21-cab*cab),9));
         continue;
      end if;
      /*
           dbms_output.put_line('dx21 ' || dx21 || ',' || dy21);
      dbms_output.put_line('d21 ' || d21);
            dbms_output.put_line('dx31 ' || dx31 || ',' || dy31);
      dbms_output.put_line('d31 ' || d31);
           dbms_output.put_line('dx32 ' || dx32 || ',' || dy32);
      dbms_output.put_line('d32 ' || d32);
       dbms_output.put_line('cab ' || cab);
      dbms_output.put_line('D21 minus cab*cab ' || (d21-cab*cab));
      dbms_output.put_line('x1 ' || x1 || ',' || y1);
      dbms_output.put_line('x2 ' || x2 || ',' || y2);
      dbms_output.put_line('x3 ' || x3 || ',' || y3);
      */
      width := sqrt(d21 - cab*cab);
      dbms_output.put_line('found one at vertex '||ij || ' with segment ' || (ii-1) || ' width ' || width);
     end if;
 --    END IF;
   end loop;
   
  End LOOP;
  Return width;
END;
--
FUNCTION AZIMUTH(Geom IN MDSYS.SDO_GEOMETRY,psegment PLS_INTEGER default 1,pinitial VARCHAR2 default 'TRUE') RETURN NUMBER AS

-- For a particular segment in a geometry, returns the azimuth (the clockwise 
--angle in degrees from North)
--        for a) the geodesic line between the start point and the end point or
--        for b) Cartesian data, the line between the start point and the end point.
-- For SRID=8265, If initial <> 'TRUE' it returns the final azimuth rather than the 
-- initial azimuth.

BEGIN
    
    Return GZ_GEODESIC.Geo_Azimuth(Geom,psegment,pinitial);
    
END;
--
FUNCTION Get_Small_Angle(Geom IN MDSYS.SDO_GEOMETRY, max_angle_limit NUMBER default 0.) RETURN NUMBER  AS

-- Returns minimum angle in a geometry. Parameter maximum_angle may be used to limit the
-- angles that are searched for. Function returns NULL however if the smallest angle
-- found in the geometry is more than maximum_angle. 

  Angles            MDSYS.SDO_LIST_TYPE;

BEGIN

  Angles := Get_Angles(Geom,max_angle_limit);

  If angles is not NULL then
    If max_angle_limit = 0. or Angles(1) <= max_angle_limit then
      RETURN Angles(1);  -- return the smallest angle found
    End if;
  end if;
  
  RETURN NULL; --<-- all angles are greater than maximum_angle

END Get_Small_Angle;
--
FUNCTION GET_AZIMUTHS(Geom IN MDSYS.SDO_GEOMETRY,options VARCHAR2 default 'VI') RETURN MDSYS.SDO_LIST_TYPE  AS

BEGIN

--  smallest, the 2nd parameter has no meaning for this call

    Return Get_Angles(Geom,0.0,options=>'VI',bearings=>'TRUE');
END;
--
FUNCTION GET_Angles(Geom IN MDSYS.SDO_GEOMETRY,smallest NUMBER default 0.0, options VARCHAR2 default 'SMALL',bearings VARCHAR2 default 'FALSE') RETURN MDSYS.SDO_LIST_TYPE  AS

-- Function callable from PL/SQL to measure Angles in a geometry. (It handles rings correctly).
--
-- a)  When options = 'SMALL' it returns: the smallest angle in a geometry,and its position 
-- b)       options = 'ANGLES'  an angle list (A1,A2,A3,..
-- c)       options = 'VI':    angle,index pairs with the smallest repeated at the front
--     For a ring it will return values for 
--     each vertex but for when the 1st vertex is not the same as the last,
--     no values are returned for vertex 1 and the last vertex.
--
--    when smallest  = say 5 degrees: angle, vertex pairs for each angle <= smallest.

--SQL> select gz_qa.get_angles(sdogeometry) angle_list from cmp_150v8 where geo_id='1500000US530330303141';

-- ANGLE_LIST
----------------smallest position A1    A2        A3        A4      A5        ..
--SDO_LIST_TYPE(82.4369, 17,     167.2396, 158.949, 102.9733, 173.7558, 150.9714, 164.
--4053, 179.298, 165.4775, 117.1999, 171.4071, 89.8185, 179.8077, 179.9497, 89.9484,
--122.2755, 113.4667, 82.4369, 164.1819, 173.8649)
--                    =======

rad2deg   CONSTANT NUMBER    := 57.29577951308232087679815481410517033240;
  Angles            MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();
  Xys               MDSYS.SDO_ORDINATE_ARRAY:= Geom.SDO_ORDINATES;
  Info              MDSYS.SDO_ELEM_INFO_ARRAY := Geom.SDO_ELEM_INFO;
  n                 PLS_INTEGER;
  no_info           PLS_INTEGER;   -- no of rings from Info
  istart            PLS_INTEGER;
  iend              PLS_INTEGER;
  ii                PLS_INTEGER;
  kount             PLS_INTEGER;
  loops             PLS_INTEGER;
  next             PLS_INTEGER :=2;
  azimuth           NUMBER;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;
  x3                NUMBER;
  y3                NUMBER;
  dx                NUMBER;
  dy                NUMBER;
  dx2               NUMBER;
  dy2               NUMBER;
  dx3               NUMBER;
  dy3               NUMBER;
  abs_dx2_dy2       NUMBER;
  angle1            NUMBER;
  angle2            NUMBER;
  angle             NUMBER;
  small             NUMBER := 1000.0;
  threshold         NUMBER := smallest;
  where_is          NUMBER := 0;
  GTYPE             NUMBER := Geom.sdo_gtype;
  Interpretation    NUMBER;
  siny              NUMBER;
  sinysq            NUMBER;
  yfactor           NUMBER;
  cosy              NUMBER;
  --    the latitude coefficients
  a_1               NUMBER := 0.9933056431085355608716884491956183391161;
  a_2               NUMBER := 0.009973972089654263505971285993829439678765;
  a_3               NUMBER := 0.00008445133481869081114188534958332938725393;
  c1                NUMBER;
  c2                NUMBER;
 -- a1   number;
 -- az1  number;
 -- az2  number;
 -- m12  number;
 -- m21  number;
 -- mm12 number;
 -- d_K  number;  -- the geodesic distance computed by Charles Karney's code
 -- s12    number;
 --ss12   number;
  old_bearing number;
  poptions          VARCHAR2(1) := UPPER(SUBSTR(options,1,1));
  
procedure set_c1c2(y number) as

--  Sets the longitude coefficients as a function of ABS(latitude).
    begin
    
    if y < 34.5 then
      if y >= 27.5 then
--        p := 5;  -- for 27.5 .. 34.4999 degrees
        c1 := 0.99999882342768;
        c2 := 0.00335602313364;
      elsif y >= 20.5 then
--        p := 4;  -- for 20.5.. 27.4999 degrees
        c1 := 0.99999954254499;
        c2 := 0.00335267920716;
      elsif y >= 13.5 then
--        p := 3;  -- for 13.5 .. 20.4999 degrees
        c1 := 0.99999987773583;
        c2 := 0.00335000490016;
      elsif y >= 6.5 then
--        p := 2;  -- for 6.5 .. 13.4999 degrees
        c1 := 0.99999998463761;
        c2 := 0.00334815672;
      else
--        p := 1;  --for 0 .. 6.4999 degrees
        c1 := 0.99999999981136;
        c2 := 0.00334723822659;
      end if;
   elsif y < 52.5 then
      if y >= 45.5 then
--        p := 8;  --for 45.5 .. 52.4999 degrees
        c1 := 0.99999456031055;
        c2 := 0.00336625111507;

      elsif y >= 41.5 then
--        p := 7;  --for 41.5 .. 45.4999 degrees This range is different
        c1 := 0.99999581110019;
        c2 := 0.00336390796249;
      else
--        p := 6;  --for 34.5 .. 41.4999 degrees
        c1 := 0.99999759542303;
        c2 := 0.00335984120472;
      end if;
   elsif y < 59.5 then
--     p := 9;  --for 52.5 .. 59.4999 degrees
       c1 := 0.99999207002972;
        c2 := 0.00337022115208;
   elsif y < 66.5 then
--      p := 10;  -- for 59.5 .. 66.4999 degrees
      c1 := 0.99998940805689;
      c2 := 0.00337382242390;
   else  
--      p := 11;  -- 66.5 .. 90 degrees
      c1 := 0.99998688314480;
      c2 :=0.00337683963728;
   end if;

   end; 
BEGIN


-- Ignore point, multipoint and hetereogenous collections
  if geom is NULL or gtype = 2001 or gtype = 2004 or gtype = 2005 then
     RETURN NULL;
  end if;

  interpretation := Info(3);
  if interpretation > 1 then
     RETURN NULL;
  end if;

-- Caller just wants a list of angles
  if poptions = 'A' or Bearings = 'TRUE' then
    next := 0;
  elsif smallest = 0. then 
    threshold := small;
   elsif smallest > 0. then
      small := smallest;
  end if;

  no_info := TRUNC(Info.count/3);
  n := Xys.count/2;
  if poptions = 'S' then
    Angles.extend(2);
  elsif poptions = 'A' then
    Angles.extend(n - no_info);  -- just a list of angles
  else                         
   Angles.extend(n*2+4); -- smallest comes first, then its offset, then
  --                               all angles. Note there are n-1 vertices when
  --                               the xys.count/2 = n
  end if;
 -- dbms_output.put_line('EXTENDED '|| angles.count || ' n ' || n ||' '||no_info);
  FOR loops in 1..no_info LOOP
    istart := loops*3+1;   -- is 4
    if loops*3 = Info.count then
      iend := Xys.count-2;  -- iend points to the y coordinate of 2nd to last vertex
    else
      iend := Info(istart)-3;  -- of vertex n-1 (vertex n =1)
    end if;

    ii := Info(istart-3);  -- is Info(1)
    kount := TRUNC((iend-ii)/2);

-- Is it a ring?
    if Bearings = 'FALSE' and Xys(ii) = Xys(iend+1) and Xys(ii+1) = XYs(iend+2) then
       kount := kount + 1;
    else
       iend := ii+1;
       ii := ii+2; -- Ignore 1st and last vertex
       if Bearings = 'TRUE' then
         kount := kount +1;
       end if;
    end if;
     
 
    x2 := Xys(iend-1);
    y2 := Xys(iend);
    siny := quick_sincos(y2,cosy,'Y');   -- get a cosine
    set_c1c2(ABS(y2));
    cosy := cosy*(c1+ siny*siny*c2);
    sinysq := siny*siny;
    yfactor := a_1+ sinysq*(a_2 + a_3*sinysq);

    x3 := Xys(ii);
    y3 := Xys(ii+1);

    For i in 1..kount Loop
      x1 := x2;
      y1 := y2;
      x2 := x3;
      y2 := y3;
      ii := ii+2;
      
--    for bearings just calculate the angle of the line segment from East.

      IF Bearings = 'TRUE' THEN
           
          dx  := x2-x1;
          dy  := y2-y1;
          angle := Faster_atan2(dy*yfactor,dx*cosy,TRUE);
      -- Convert from anti-clockwise bearing from East
        if angle < 0. then 
          angle := angle +360.;        
        end if;
        -- to clockwise azimuth from North
        azimuth := MOD(450. - angle,360);
          next := next+1;
          Angles(next) := ROUND(azimuth,4);
          if i <> kount then
            x3 := Xys(ii);
            y3 := Xys(ii+1);
          end if;
      ELSE
  
  --          (x1,y1)  +          + (x3,y3)  future (x2,y2)
  --                     \       /   \
  --                       \   /       \
  --               (x2,y2)  +           + future (x3,y3)
  --                     future (x1,y1)
      dx  := x1-x2;
      dy  := y1-y2;
      x3 := Xys(ii);
      y3 := Xys(ii+1);
      dx2 := x3-x1;
      dy2 := y3-y1;
      dx3 := x3-x2;
      dy3 := y3-y2;
      abs_dx2_dy2 := ABS(dx2) + ABS(dy2);
--      dbms_output.put_line('abs ' || abs_dx2_dy2 || 'abs ' || (abs(dx) + abs(dy)) || ' aabs ' || (abs(dx3)+abs(dy3)));
      IF abs_dx2_dy2 <= ABS(dx) + ABS(dy) or
         abs_dx2_dy2 <= ABS(dx3) + ABS(dy3) or smallest <= 0.0 or smallest > 45. then

-- These factors typically reduce error to less than 0.01 degrees compared to
-- other methods of calculation (commented below).

        Angle1 := Faster_atan2(dy*yfactor,dx*cosy,TRUE);
  
        Angle2 := Faster_atan2(dy3*yfactor,dx3*cosy,TRUE);

 
--   dbms_output.put_line('ii ' || ii || ' angle1 ' || round(angle1,6) || ' ' ||round(angle2,6));
--  angle1 :=  GZ_QA.geo_bearing(x2,y2,x1,y1,old_bearing)*rad2deg;
--  angle2 :=  GZ_QA.geo_bearing(x2,y2,x3,y3,old_bearing)*rad2deg;
--   dbms_output.put_line('ii ' || ii || ' angle1 ' || round(angle1,6) || ' ' ||round(angle2,6) || ' dy ' || dy || ' dx ' || dx || ' dy3 ' || dy3 || 'dx3 ' ||dx3);

-- s12 := GZ_GEODESIC.inverse(y2,x2,y1,x1,a1,az1,az2,mm12,m12,m21,SS12,FALSE,TRUE,TRUE,TRUE);
--  dbms_output.put_line('az1 ' || round(az1,6)|| ' az2 ' || round(az2,6));
--  s12 := GZ_GEODESIC.inverse(y2,x2,y3,x3,a1,az1,az2,mm12,m12,m21,SS12,FALSE,TRUE,TRUE,TRUE);
--  dbms_output.put_line('az1 ' || round(az1,6)|| ' az2 ' || round(az2,6));
-- dbms_output.put_line('x1 ' || x1 || ' y1' || y1 || ' x2 ' || x2 || ' y2 ' || y2 || ' x3 ' || x3 || ' y3 ' || y3);
 
 
        Angle := ABS(angle1-angle2);
  
        if angle > 180. then
          angle := 360. -angle;
        end if;
        
--             dbms_output.put_line('SMALL ' || ii || ' angle ' || round(angle,4)  || ' small' || small);
         if angle < small and smallest >= 0. then
          small := angle;
          Angles(1) := small;
          Angles(2) := TRUNC(ii/2);  -- vertex number
        end if;
        
        if poptions = 'A' then
          next := next+1;
          Angles(next) := ROUND(angle,4);
        elsif (angle <= threshold and poptions='V') then
          next := next+1;
          Angles(next) := ROUND(angle,4); 
          next := next+1;
          Angles(next) := TRUNC(ii/2);
        end if;

     End if;
     
     END IF;
    End Loop;
  End Loop;

  if Angles(1) is NULL then
    Angles := NULL;
  else
    Angles.trim(angles.count-next);
  end if;

  RETURN Angles;

END GET_Angles;
--
FUNCTION Get_Small_Angle_Geom(Geom IN MDSYS.SDO_GEOMETRY,max_angle_limit NUMBER default 0.) RETURN MDSYS.SDO_GEOMETRY  AS

-- Returns one (or more) 'V' geometries with sharp angles found within the
-- input geometry. Parameter maximum_angle specifies the maximum angle to return.

--  Example of a returned geometry:
--
--     back vertex           apex vertex
--      +--------------------+
--                     ..
--                ..
--            +  ahead vertex


  Angles            MDSYS.SDO_LIST_TYPE;
  VGeom             MDSYS.SDO_GEOMETRY;
  XYs               MDSYS.SDO_ORDINATE_ARRAY := Geom.SDO_ORDINATES;
  VXYs              MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  Info              MDSYS.SDO_ELEM_INFO_ARRAY := GEOM.SDO_ELEM_INFO;
  Vinfo             Mdsys.sdo_Elem_Info_Array:= Mdsys.sdo_Elem_Info_Array();

  back              PLS_INTEGER;      -- back array location for behind vertex
  ii                PLS_INTEGER;
  loc               PLS_INTEGER :=0;  -- array location in output VXys
  seg               PLS_INTEGER;
  interpretation    NUMBER;
  GTYPE             NUMBER := Geom.SDO_GTYPE;
  
BEGIN 

-- Ignore point, multipoint and hetereogenous collections
  if geom is NULL or gtype = 2001 or gtype = 2004 or gtype = 2005 then
     RETURN NULL;
  end if;

  interpretation := Geom.Sdo_Elem_Info(3);
  if interpretation > 1 then
     dbms_output.put_line('Geometry has unusable interpretation ' || interpretation);
     RETURN NULL;
  end if;

  Angles := Get_Angles(Geom,max_angle_limit);

  If angles is not NULL then
  
    For i in 2..TRUNC(Angles.count/2)  loop  -- 1 and 2: smallest angle and its position
    
      ii := Angles(i*2)*2-1;         -- position is second value in each pair

-- Figure out the vertex at the end of the ring

      for current_info in 1..TRUNC(Info.count/3) Loop
       
        if current_info+1 > TRUNC(Info.count/3) then
          back :=  Xys.count-3;
        else
          back := Info(current_info*3+1)-4;
        end if;
        

        if ii = Info(current_info*3-2) then
            exit;
        elsif ii <= back then
          back := ii-2;
          exit;
        end if;
      end loop;
 
      Vxys.extend(6);
   
      loc := loc +2;      
      Vxys(loc-1) := Xys(back);     -- get behind vertex
      Vxys(loc) :=   Xys(back+1);
      
      for jj in 1..4 loop
        loc := loc +1;
        Vxys(loc) := Xys(ii+jj-1);  -- get apex vertex and following vertex
      end loop;
  
    End Loop;

-- Setup Info array for all V's (each 3 pairs of coordinates)

    If Vxys.count > 0 then
        Gtype := 2002.;                               
        if  Vxys.count > 6 then
           Gtype := 2006.;
        End if;
      
        Vinfo.extend(TRUNC(loc/2));
        
        for i in 1..TRUNC(loc/6) loop
          Vinfo(i*3-2) := (i-1)*6+1;
          Vinfo(i*3-1) := 2;
          Vinfo(i*3) := 1;
        end loop;
      
        VGeom := MDSYS.SDO_GEOMETRY(GTYPE,Geom.SDO_SRID,NULL,Vinfo,VXys);
    
        RETURN VGeom;
    End if;
  End IF; -- End of Angles is not NULL
  
  RETURN NULL;  -- No small angles found

END Get_Small_Angle_Geom;
--
Procedure find_Info(coord pls_integer,Info_array in out nocopy mdsys.sdo_elem_info_array, Xys_count PLS_INTEGER,istart IN OUT NOCOPY PLS_INTEGER, iend IN OUT NOCOPY PLS_INTEGER,ring_type IN OUT NOCOPY PLS_INTEGER) as
    
--  For a particular coordinate, find the start and end vertices and ring type 
--  from its corresponding Info range.

    Info_count  pls_integer := TRUNC(Info_array.count/3);
    
    Begin
         for ij in 1..Info_count loop

            If ij <> Info_count then
               iend := Info_Array(ij*3+1)-1;
            else
               iend  := Xys_count;
            end if;
 
            if coord >= Info_Array(ij*3-2) and coord <= iend then
               istart := TRUNC((Info_Array(ij*3-2)+1)/2);  -- start vertex in ring
               iend := TRUNC(iend/2);                      -- last vertex in ring
               ring_type := Info_Array(ij*3-1);
               exit;
            end if;
         end loop;
END Find_Info;
--
FUNCTION Get_Length(Geom IN MDSYS.SDO_GEOMETRY, seg_no PLS_INTEGER) RETURN NUMBER  AS

-- Compute length for a single line segment in a geometry. Returns zero for
-- a bad segment number (zero or segment=last vertex).

  Xys          MDSYS.SDO_ORDINATE_ARRAY := Geom.sdo_ordinates;
  Info         MDSYS.SDO_ELEM_INFO_ARRAY := Geom.sdo_elem_info;
  x1           NUMBER;
  y1           NUMBER;
  x2           NUMBER;
  y2           NUMBER;

  dist         NUMBER := 0.0;
  
  istart       PLS_INTEGER;  -- start and last vertex in the ring
  iend         PLS_INTEGER;
  ring_type    PLS_INTEGER;   

BEGIN

  find_info(seg_no*2-1,Info,Xys.count,istart,iend,ring_type);
  
  If seg_no > 0 and seg_no <> iend then
   
    x1 := Xys(seg_no*2-1);
    y1 := Xys(seg_no*2  );
    x2 := Xys(seg_no*2+1);
    y2 := Xys(seg_no*2+2);
    
    dist := distance_fcn(x1,y1,x2,y2,Geom.sdo_srid);
  End if;
  
  RETURN dist; 

END Get_length;
--
FUNCTION PERIMETER_AREA_RATIO(Geom IN MDSYS.SDO_GEOMETRY) RETURN NUMBER DETERMINISTIC AS

-- Returns the perimater/sqrt(area) ratio for a geometry.
-- Returns zero if the area is bad (zero).

--      A=1, P=4      A=2, P=6            A=5, P=12      A=10, P=22    A=100,P=202
--       +--+         +----+       +-------------------+      
--       |  |         |    |       |                   |
--       +--+         +----+       +-------------------+
--                                 
--         4               4.2426..       5.3665          6.957..      20.2  P/A ratio

  Xys            MDSYS.SDO_ORDINATE_ARRAY := Geom.sdo_ordinates;
  area           NUMBER;
  perim          NUMBER;

BEGIN

    area  := sdo_geom.sdo_area(geom,0.05,'unit=sq_meter');
    perim := sdo_geom.sdo_length(geom,0.05,'unit=meter');
    
    if area <> 0. then
       return perim/sqrt(area);
    end if;
    RETURN 0.;
    
END PERIMETER_AREA_RATIO;
--
FUNCTION FIND_MPW_AT_SCALE(Geom IN MDSYS.SDO_GEOMETRY,pipe_type VARCHAR2 default 'A',scale_rf NUMBER default 500000.,paspect_ratio NUMBER default 1.) return NUMBER as

-- SQL callable function to find pipes and return minimum pipe width (MPW) found 
-- in each geometry at a particular scale (rf = representative fraction). 
-- Pipes are narrow peninsula ('P') or openings like river estuary shapes ('E'). 
-- 'A' finds both (all).

   Max_Width         NUMBER :=0.0;
   Min_Width_found   NUMBER :=0.0;
   Min_length        NUMBER;
   
Begin

  Max_width :=  1.*scale_rf * 0.001016;   --0.01 inches at 1:500,000
  Min_length := max_width*paspect_ratio;
  dbms_output.put_line('Max width ' || max_width);
  Min_width_found := Find_Min_Pipe_Width(Geom,pipe_type,Max_width,min_length);
  
  Return Min_width_found;
  
End Find_MPW_at_Scale;
--
FUNCTION FIND_MIN_PIPE_WIDTH(Geom IN MDSYS.SDO_GEOMETRY,pipe_type VARCHAR2 default 'A',max_width NUMBER default 150., minimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN NUMBER  AS

-- SQL callable function to find pipes and return minimum pipe width found 
-- in each geometry. Pipes are narrow peninsula ('P') or openings like river 
-- estuary shapes ('E'). 'A' finds both (all).

   Pipe_Pars        PIPES_TYPE;
   Width            NUMBER;   -- Width of the pipe
   Min_Pipe_Width   NUMBER := 1.E10;
 

BEGIN

   Pipe_pars := Find_Get_Pipes(Geom,pipe_type,max_width,minimum_len,aspect_ratio);
   
   If Pipe_pars.count > 0 then
      For ii in 1..Pipe_pars.count loop
--        dbms_output.put_line('seg ' || pipe_pars(ii).seg || ' mseg ' || pipe_pars(ii).mseg || ' L ' || round(pipe_pars(ii).length,4) || ' AL ' || round(pipe_pars(ii).Accum_len,4)  );
        Width := Pipe_Pars(ii).Width_p;
        if width < Min_Pipe_Width and Pipe_Pars(ii).accum_len > minimum_len then
           Min_Pipe_Width := Width;
        end if;
      End Loop;
      RETURN Round(Min_Pipe_Width,4);
   End if;
   
   RETURN NULL;
   
END FIND_MIN_PIPE_WIDTH;
--
Procedure Copy_Xys(XY_Arr MDSYS.SDO_ORDINATE_ARRAY,from_aa PLS_INTEGER, to_bb PLS_INTEGER,Out_Arr IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,out_idx IN OUT PLS_INTEGER)
                   AS
-- Straightforward code to copy part of a single loop's coordinates

  bendx    PLS_INTEGER := XY_Arr.count-1;
  bendy    PLS_INTEGER := Xy_Arr.count;
  to_b     PLS_INTEGER := to_bb;
  inn      PLS_INTEGER;

Begin

  if Out_idx <=1 then
    Out_arr := MDSYS.SDO_ORDINATE_ARRAY();
    Out_Idx := 0;
  end if;
  
  if to_b < from_aa then -- Ignore last vertex when we copy thru end of the loop
     to_b := to_b + Xy_Arr.count;
     Out_Arr.extend(to_b-from_aa-1);
  else
    Out_Arr.extend(to_b-from_aa+1);
  end if;

   for i in from_aa..to_b Loop
-- Dont copy last vertex which is the same as the first vertex!
      if to_b > Xy_arr.count and (i = bendx or i = bendy) then
        NULL;
      else
-- This is the usual Mod function we want: Mod(1,10) = Mod(0,10) +1,
-- Mod(2,1) = Mod(1,10) +1, .. to Mod(10,10) is Mod(9,10) +1 = 10  !!

        inn := MOD(i-1,XY_arr.count)+1;
        out_idx := out_idx + 1;
        Out_Arr(out_idx) := XY_Arr(inn);
      end if;
   end loop;

End Copy_Xys;
--
FUNCTION GET_PIPES(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', pmax_width NUMBER default 0.0, pminimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY  AS

-- This function is similar to Get_Pipe_Segs but tries to aggregate the pipe
-- segments into closed rings. It is an SQL callable function to get pipes as 
-- closed rings so they can be displayed and shaded.
-- Pipes are narrow peninsulas or river estuary shapes.
  
   Pipe_Geom          MDSYS.SDO_GEOMETRY;
   XYOrd              MDSYS.SDO_ORDINATE_ARRAY := Geom.Sdo_ordinates;
   PipesXYOrd         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   PipeXYOrd          MDSYS.SDO_ORDINATE_ARRAY;
   Info               MDSYS.SDO_ELEM_INFO_ARRAY := Geom.sdo_Elem_Info;
   Info_Arr           MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY();
   PSL                MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   PSL_All            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Widths             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Class_it           Char_Array := Char_Array();
   Ring_Jump          XY_List;

   Pipe_Pars          PIPES_TYPE;
   from_vertex        PLS_INTEGER;
   to_vertex          PLS_INTEGER;
   n                  PLS_INTEGER;
   out_index          PLS_INTEGER := 0;
   next               PLS_INTEGER :=0;
   which              PLS_INTEGER;
   Big                NUMBER := 1.E10;
   area               NUMBER;
   avg_width          NUMBER;
   max_width          NUMBER := pmax_width;
   hi_width           NUMBER := max_width;
   pipe_perim         NUMBER;
   pipe_len           NUMBER;
   accum_length       NUMBER;
   avg_width2         NUMBER;
   avg_width3         NUMBER := Big;
   hi_width3          NUMBER;
   narrow_width       NUMBER;
   wide_width         NUMBER;
   B2_4AC             NUMBER;
   minimum_len        NUMBER := pminimum_len;

Procedure Get_low_high_width as

   kount    pls_integer:=0;
   ok       boolean;

begin
    narrow_width := Big;
    wide_width := 0.0;
    avg_width := 0.0;
    
    for jj in 1..Widths.count loop
      ok := TRUE;
      for kk in 1..jj-1 loop
        if kk<>jj and widths(jj) = Widths(kk) then   -- remove duplicates
          ok := FALSE;
          exit;
        end if;
      end loop;
      
      if ok then 
        kount := kount+1;
        if Widths(jj) < narrow_width then
           narrow_width := Widths(jj);
        elsif Widths(jj) > wide_width then
           wide_width := Widths(jj);
        end if;
        avg_width := avg_width + Widths(jj);
        
      end if;
 

    end loop;
    if kount <> 0 then
      avg_width := avg_width/kount;
    end if;
    
--    dbms_output.put_line('AV ' || round(avg_width,3) || ' wide ' || round(wide_width,3));
end;
   
Function Check_PSL_pairs return boolean as
   done               BOOLEAN := FALSE;
begin
      for jj in 1..TRUNC(PSL_All.count/2) loop
           if from_vertex >= PSL_All(jj*2-1) and from_vertex <= PSL_All(jj*2) then
              done := TRUE;
              exit;
           end if;
           if to_vertex >= PSL_All(jj*2-1) and to_vertex <= PSL_All(jj*2) then
              done := TRUE;
              exit;
           end if;
      end loop;
      Return done;
end;
--==============================================================================

BEGIN


-- Use automatic width detection when the maximum pipe width is not specified

   if max_width <= 0.0 then
     max_width := 125.;
     hi_width := get_pipe_width(Geom,max_width,'HIGH');
   else
     hi_width := max_width;
   end if;
   
--   dbms_output.put_line('hi_width ' || hi_width || ' max ' || max_width);

   
--   If minimum_len = 0.0 then
--     minimum_len := hi_width*0.5;
--   End If;
   
   Pipe_Pars := Find_Get_Pipes(Geom,pipe_type,hi_width,minimum_len,aspect_ratio,max_angle_diff);

   Class_it := Classify_Vertices(Pipe_pars,Ring_Jump,Geom,max_width);

--   for ii in 1..Class_it.count loop
--      if Class_it(ii) <> 'P' then
--  --    dbms_output.put_line(ii||'type:' ||Class_it(ii));
--      end if;
--   end loop;
   
--   dbms_output.put_line('PPP ' || pipe_pars.count);
  
   If Pipe_pars.count <> 0 then

      Info_arr.extend(pipe_pars.count*3);
      For ii in 1..Pipe_pars.count Loop
  
          pipe_len :=  Pipe_pars(ii).Accum_len;
          from_vertex := Pipe_pars(ii).Seg;
          to_vertex := Pipe_pars(ii).Mseg;
          which := TRUNC(Pipe_pars(ii).Projector*0.1);

          if NOT Check_PSL_pairs()  then
--            dbms_output.put_line('PPP ' || pipe_pars(ii) || ',' || pipe_pars(ii+1) ||','||pipe_pars(ii+2) || ' which ' || which);
            Pipe_geom := Get_Pipe_Geom(XyOrd,Info,from_vertex,to_vertex,which,max_width,PSL,avg_width,accum_length);
           
            if pipe_geom is NOT NULL then
 
            --      Approximate the pipe_length in meters 
        PipeXYord := Pipe_geom.sdo_ordinates;
        pipe_perim := Perimeter(PipeXyOrd);
--        dbms_output.put_line('pipexy ' || pipexyord.count);
--               dbms_output.put_line('AW ' || avg_width || ' PP ' || pipe_perim);
        Widths := Get_widths(Pipe_geom,max_width*1.5,'FALSE');
--             for kk in 1..trunc(pipexyord.count/2) loop
--               dbms_output.put_line('ii ' || kk || ' xy ' ||round(pipexyord(kk*2-1),7) ||','||round(pipexyord(kk*2),7));
--             end loop;
        if Widths is NULL then 
           Widths := MDSYS.SDO_LIST_TYPE(Pipe_pars(ii).Width_p, Pipe_pars(ii).Width_q);
        end if;
--             dbms_output.put_line('WWW ' || widths(1));
        if Widths is NOT NULL then
          
        Get_low_high_width;
--        dbms_output.put_line('AWW ' || avg_width);
--ignore        pipe_len := pipe_perim*0.5 - avg_width;
        area := ABS(ring_area(PipeXyord));
--        dbms_output.put_line('AREA ' || area );
        -- Use quadratic equation from L * W = Area and 2L + 2W = Perimeter
        -- W = (Perim/2 - sqrt((Perim/2)**2 -4*Area))/2
        
        
        avg_width3 := Big;
        B2_4AC := (pipe_perim*0.5)*(pipe_perim*0.5) - 4.*area;
        if b2_4AC >=0.0 then
--dbms_output.put_line('B2_4ac ' || b2_4Ac || ' area ' || area);
        avg_width3 := (pipe_perim*0.5- sqrt(B2_4AC))*0.5;
        hi_width3 := 2.* avg_width3 - (avg_width + avg_width2)*0.5;
        end if;
        
--        if avg_width2 > avg_width and avg_width2 > avg_width3 and (1.- avg_width/avg_width2) < (1.-avg_width3/avg_width2) then
--           avg_width := (avg_width + avg_width2)*0.5;
--        elsif avg_width2 <= avg_width and avg_width > avg_width3 and (1.- avg_width2/avg_width) < (1.-avg_width3/avg_width) then
--           avg_width := (avg_width + avg_width2)*0.5;
--        else
         -- Check that we have a rectangular like object
--            if hi_width3 < max_width then
--              avg_width := avg_width3;
--            else
--               avg_width := hi_width3;
--            end if;
 
--           dbms_output.put_line('SHOSE AWW3 ' || avg_width3);
--        end if;
        
--ignore        pipe_len := area/avg_width;
---         dbms_output.put_line('AWW ' || avg_width);
        dbms_output.put_line('pipe len ' || round(pipe_len,3) || ' avg width ' || round(avg_width,3)); --pipe_len/avg_width ' || round((pipe_len/avg_width),3) || ' perim ' || round(pipe_perim,3) || ' Area ' || round(area,3) || 'Estimate ' || round(avg_width*pipe_len,3));
-- dbms_output.put_line('pipe len ' || round(pipe_len,3) || 'AV width ' || round(avg_width,3));

--      Build a pipe if its length, aspect ratio is good

          if pipe_len > minimum_len then
--          if pipe_len > hi_width and pipe_len/avg_width >= aspect_ratio then
           dbms_output.put_line('>>> got a pipe ' || from_vertex || ' to ' || to_vertex || ' area ' || round(area,2));
  -- Only mark those that are successful as done 
 --           for jj in 1..PSL.count loop
 --             dbms_output.put_line('PSL ' || PSL(jj));
 --           end loop;
            n := PSL_All.count;
            PSL_All.extend(PSL.count);
            for jj in 1..PSL.count loop
              PSL_All(n+jj) := PSL(jj);
            end loop;
            
--          Copy the pipe geometry into a 2007 aggregation of all pipes found          
            
            Info_Arr(next+1) := out_index+1;
            Info_Arr(next+2) := 1003; --2;
            Info_Arr(next+3) := 1;
            next := next+3;
            Copy_Xys(Pipe_Geom.sdo_ordinates,1,Pipe_Geom.sdo_ordinates.count,PipesXyOrd,out_index);

         End if;
        End If;
        End If;
          End If;
 
        End Loop;
        Info_Arr.trim(Info_Arr.count-next);
      Pipe_geom :=  MDSYS.SDO_GEOMETRY(2007,Geom.sdo_SRID,NULL,Info_Arr,PipesXYOrd);
   End if;
   
   if Info_Arr.count = 0 then
      RETURN NULL;
   end if;
   
   RETURN Pipe_Geom;
   
END GET_PIPES;
--
PROCEDURE FIND_PIPES(Intable VARCHAR2, IdColumn VARCHAR2, GeomColumn VARCHAR2,OutTable VARCHAR2,pipe_type VARCHAR2 default 'A',max_width NUMBER default 150., minimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) AS

-- Pipe_type 'P' peninsula type pipes
--            
--            with an appended 'R' looks for Rivers between adjacent geometries
   TYPE RefCursorType IS REF CURSOR;
   CursorTable RefCursorType;
   
   Unpacked_table  VARCHAR2(30) := Intable||'_2003';
   Adjacency_table VARCHAR2(30) := Intable||'_Adj';
   Geometry1       MDSYS.SDO_GEOMETRY;
   Geometry2       MDSYS.SDO_GEOMETRY;
   Geometry_both   MDSYS.SDO_GEOMETRY;
   Pipe_Geom       MDSYS.SDO_GEOMETRY;
   Array_UniqId    MDSYS.SDO_LIST_TYPE;
   Adj_Ids         MDSYS.SDO_LIST_TYPE;
   Array_SdoGeom   SDO_GEOMETRY_ARRAY;
   
   Xys             MDSYS.SDO_ORDINATE_ARRAY;
   Info_Array      MDSYS.SDO_Elem_Info_Array;
   Info_both       MDSYS.SDO_Elem_Info_Array;
   sql_stmt        VARCHAR2(4000);
   sql_stmt2       VARCHAR2(4000);
   kount           PLS_INTEGER;
   loop_counter    PLS_INTEGER;
   Current_Id      NUMBER;
   Last_Id         NUMBER:=0.0;
   RowLimit        NUMBER :=50;
   Look_for_Rivers VARCHAR2(1) := SUBSTR(pipe_type,2,1);
   
BEGIN
   if Length(Pipe_type) = 1 then
      Look_for_Rivers := 'N';
   end if;
--==============================================================================

--  Begin by unpacking the geometries and then find adjacent geometries if any

--    unpack_geometry(Intable,IdColumn,GeomColumn,Unpacked_table,TRUE);
    
--    find_adjacency(Unpacked_table,IdColumn,GeomColumn,Adjacency_table);
    
    sql_stmt := 'SELECT count(1) from ' ||Adjacency_table;
    EXECUTE IMMEDIATE sql_stmt into kount;
    
    sql_stmt := 'CREATE TABLE ' ||OutTable ||'('||IdColumn||' NUMBER,'||GeomColumn||' MDSYS.SDO_GEOMETRY) NOLOGGING';
    EXECUTE IMMEDIATE sql_stmt;
    
    sql_stmt2 := 'SELECT '||GeomColumn|| ' FROM ' || Unpacked_table || ' WHERE ' || 
                 IdColumn ||'=:1';
                 
    sql_stmt := 'SELECT UNIQID, ADJ_ID from ' ||Adjacency_table;
    
    OPEN CursorTable for sql_stmt;
    
    Loop
      FETCH CursorTable BULK COLLECT INTO Array_UniqId,Adj_Ids LIMIT RowLimit;
        EXIT WHEN Array_UniqId.COUNT = 0;
      loop_counter := loop_counter + 1;
    
       For ii in 1..Array_UniqId.count Loop
          Current_Id := Array_UniqId(ii);
          If Current_id <> Last_id then
             EXECUTE IMMEDIATE sql_stmt2 into Geometry1 using Current_Id;
             Info_array := Geometry1.Sdo_Elem_Info;
             Last_id := Current_Id;
             kount := 1;
          else
             kount := kount + 1;
          end if;
          
          If Adj_Ids(ii) is NULL or (kount=1 and Look_for_Rivers = 'N') THEN
--            Pipe_Geom := Get_Pipes(Geometry1,pipe_type,max_width,minimum_len,aspect_ratio,max_angle_diff);
            Pipe_Geom := Get_Pipe_Segs(Geometry1,pipe_type,max_width,minimum_len,aspect_ratio,max_angle_diff);
          ELSIF Look_for_Rivers = 'R' then
          
             EXECUTE IMMEDIATE sql_stmt2 into Geometry2 using Adj_Ids(ii);
            
         -- If the Geometry has a nearby neighbor, then concatenate to its neighbor(s)
         -- Make a concatenated XY array as it is easier for checking for intersections
          -- to handle just a single set of Xys and a single Info array. We can determine
          -- which geometry an intersection comes from because at present we have
          -- the entire set of oordinates for Geometry1 (Oid).
      
            Info_both := Info_array;
            Xys := GZ_TOPOFIX.Concatenate_Xys_Info(Geometry1,Geometry2,Info_both,NULL);
            Geometry_both := MDSYS.SDO_Geometry(2007,8265.,NULL,Info_both,XYs);
          
          
          Pipe_Geom := Get_Pipes(Geometry_both,pipe_type,max_width,minimum_len,aspect_ratio,max_angle_diff);
          ELSE
            continue;
          END IF;
    
    -- Note that if we concatenated 2 geometries we don't know which pipes belong to
    -- which object. Since they may belong to both, it may not matter.
    
          if Pipe_Geom is NOT NULL then
          sql_stmt := 'INSERT /*+ APPEND */ INTO ' || OutTable ||
                         ' ('||IDColumn||','||GeomColumn||') VALUES (:1,:2)';
                         
          EXECUTE IMMEDIATE sql_stmt using Current_Id,Pipe_Geom;
          end if;
      End Loop;
  Commit;
  END LOOP;
  
END FIND_PIPES;
--
FUNCTION FIND_PIPES(Geom IN MDSYS.SDO_GEOMETRY,pipe_type VARCHAR2 default 'A',max_width NUMBER default 150., minimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN NUMBER  AS

-- SQL callable function to examine a geometry for pipes and return either 
-- the minimum pipe width found (or NULL if no pipes found).

-- This function can be put in an SQL statement in the predicate:
--
-- select fullcodel from txlaincplace where NVL(gz_pipes.find_pipes(sdogeometry,'A'),1000) < 250

-- in each geometry. Pipes are narrow peninsula ('P') or openings like river 
-- estuary shapes ('E'). 'A' finds both (all).

   Pipe_Pars        PIPES_TYPE;
   Width            NUMBER;   -- Width of the pipe
   Big              NUMBER := 1.E10;
   Min_Pipe_Width   NUMBER := Big;
 

BEGIN

   Pipe_pars := Find_Get_Pipes(Geom,pipe_type,max_width,minimum_len,aspect_ratio);
   
   If Pipe_pars.count > 0 then
      For ii in 1..Pipe_pars.count loop

-- Actually return the widest width for a particular pipe so that this width
-- can be passed to get_pipe_segs.

        Width := Pipe_Pars(ii).Width_p;
        if Pipe_Pars(ii).Width_q > Width then
           Width := Pipe_Pars(ii).Width_q;
        end if;
        
 -- Now keep this minimum  - maximum width
 
        if width < Min_Pipe_Width and Pipe_Pars(ii).accum_len > minimum_len then
           Min_Pipe_Width := Width;
        end if;
      End Loop;
      if Min_Pipe_Width = BIG then
         RETURN NULL;
      end if;
   Else
      RETURN NULL;
   End if;

   RETURN Round(Min_Pipe_Width,1);
   
END FIND_PIPES;
--
Function dotp(x1 number,y1 number,x2 number,y2 number,
                 x3 number,y3 number,x4 number,y4 number) return number as
                 
 -- return dot product for two vectors: 
 --        >0 lines go in same direction, 
 --         0 lines are perpendicular, 
 --        <0 lines go in opposite directions
 
    dx21 number := x2-x1;
    dy21 number := y2-y1;
    dx43 number := x4-x3;
    dy43 number := y4-y3;
    len1 number;
    len2 number;
    
    dot_product   number :=0.0;
    
begin
    len1 := sqrt(dx21*dx21 + dy21*dy21);
    len2 := sqrt(dx43*dx43 + dy43*dy43);
    
    if len1 <> 0. and len2 <> 0. then
       dot_product := (dx21*dx43 + dy21*dy43)/(len1*len2);
    end if;
    
    return dot_product;   
end dotp;
  --
Function angle_in_degrees(x1 number,y1 number,x2 number,y2 number,
                 x3 number,y3 number,x4 number,y4 number) return number as

 -- compute (Cartesian) angle in degrees between 2 directed segments:
 --                 (x1,y1) -> (x2,y2) and (x3,y3) -> (x4,y4)
 
    rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;

    angle number;
    
begin
      
    -- from the dot product we can "estimate" the parallelness.
    -- One might consider < 45 parallel and >= 45 non-parallel 
    
    angle := GZ_MATH.new_arccos(dotp(x1,y1,x2,y2,x3,y3,x4,y4))*rad2deg;
    return angle;
    
end angle_in_degrees;
  --
FUNCTION WEED(Pipe_type VARCHAR2,XYS IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Info_array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,

  Codes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Subject_Segs IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Matches      IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, 
  Widths_p     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Widths_q     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Lengths      IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Xps          IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Yps          IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Xqs          IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Yqs          IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, area_tolerance NUMBER default 0.) RETURN PLS_INTEGER AS
--------------------------------------------------------------------------------  
-- "Weed" out bad segment parallel matching relationships that cause problems downstream.

--   For example:   Segment 1 has a close relationship with 3, so its more distant 
--                  parallel relationship with 5 is of no interest and likewise
--                  its antiparallel relationship with 7.  Both 5 and 7 are more
--                  distant in distance and segment difference.
--   These extra relationships cause problems when we use linked lists to
--   build the pipes.

--           ----      -----
--          |   |     |    |
--        1 ^   v3   5^    v 7
--          |   |_____|    |_____

-- We also don't want these sort of relationships: 2 with 6 where there is
-- obviously points on a line from vertex 6 to vertex 2 that are outside the polygon.

--          BAD                           OK
--        
--           6
--        ----<--                       ----<---
--             / 4                              | 5
--             -----                             -----
--                  |  3                             |   3
--             -->--                            --->--
--          1 |  2                              | 
--                                      ---------
--------------------------------------------------------------------------------  
  n            PLS_INTEGER := Codes.count;
  
  x1           NUMBER;
  y1           NUMBER;
  x2           NUMBER;
  y2           NUMBER;
  x3           NUMBER;
  y3           NUMBER;
  x4           NUMBER;
  y4           NUMBER;
  xc           NUMBER;
  yc           NUMBER;
  projector    NUMBER;
  epsilon      NUMBER;
  direction1   NUMBER;
  direction2   NUMBER;
  difference1  NUMBER;
  difference2  NUMBER;
  
  close_       PLS_INTEGER;
  closest      PLS_INTEGER;
  inside       PLS_INTEGER;
  istart       PLS_INTEGER;
  last_inside  PLS_INTEGER;
  seg1         PLS_INTEGER;
  seg2         PLS_INTEGER;
  last_ii      PLS_INTEGER:=0;
  last_seg     PLS_INTEGER:=0;
  last_vtx     PLS_INTEGER;
  mseg1        PLS_INTEGER;
  mseg2        PLS_INTEGER;
  pi           PLS_INTEGER;
  ring_type    PLS_INTEGER;
  ring_type2   PLS_INTEGER;
  found        PLS_INTEGER;
  kount        PLS_INTEGER:=0;
  counter      PLS_INTEGER :=0;
  
  Inner_ring   BOOLEAN;

    
  Procedure Pack(parray in out nocopy mdsys.sdo_list_type) as 
  
 -- Pack an array using the Codes array as a signal where to omit elements from 
 -- the input array.
 -- The element is weeded - because it has a 'bad' relationship (parallelism
 -- with another remote segment - for example segment 1 with segment 5).
 
--           ----      -----
--          |   |     |    |
--        1 ^   v3   5^    v 7        
--          |   |_____|    |_____    
 
     next  pls_integer := 0;
     
  begin
     for ii in 1..Codes.count loop
        If Codes(ii) <> 0.0 then
          next := next+1;
          parray(next) := parray(ii);
        end if;
     end loop;
     parray.trim(parray.count-next);
  end;
/*  
  Function close_measure(seg1 pls_integer, seg2 pls_integer)  return pls_integer as
        seg_count   pls_integer;
        last_vertex pls_integer;
        
--         6     5
--      +------+----+
--      |           | 4
--    7 |      +----+
--      |   2 /   3
--      +----+
--        1
-- Segment number difference  1,2,3 gets confused at end of a polygon (7 to 1)      
-- Find the shortest measure, so when we are near the start/end of polygon - 
-- consider 1 to be equivalent to segment 8 so difference between 7 and 1 is 1.
-- Elsewhere just find the segment difference.

    Begin
        last_vertex := find_info(seg1*2-1,Info_Array,Xys.count,ring_type);
        seg_count := ABS(seg2 - seg1);
        -- Try 7 and 2       2 + 8 -1 -7 < 5 so seg_count becomes 2
        if seg1 > seg2 and seg2+last_vertex-1-seg1 < seg_count then
          seg_count := seg2+last_vertex-1-seg1;
        elsif seg2 > seg1 and seg1+last_vertex-1-seg2 < seg_count then
          seg_count := seg1+last_vertex-1-seg2;
        end if;
        return seg_count;
    End;
*/    
BEGIN
  
  -- Strike out demi parallel segments that are too far apart when one of them
  -- (Subject_seg or Matching Seg) has another closer relationship
  -- To avoid the expense of Point in Polygon we want to not do that test on
  -- closest segments.
 
   For ii in 1..n Loop
      seg1 := Subject_Segs(ii);
      if seg1 <> last_seg then
        last_ii := ii;
      end if;
      mseg1 := Matches(ii);
      
--    Is this a closest?
      closest := 1000000;
      found := 0;
      
      For ij in last_ii..n loop
        exit when Subject_Segs(ij) <> seg1;
        close_ := close_measure(seg1,Matches(ij),Info_Array,Xys.count);
        if close_ < closest then
           closest := close_;
           found := ij;
        end if;
      End Loop;
      
      projector := (Codes(ii) - TRUNC(Codes(ii)))*100.;
--      dbms_output.put_line('ii ' || ii || ' code ' || codes(ii) || ' P ' || projector);
--    Turn off finding pipes between inner ring coordinates except when they 
--    are very parallel

      Inner_ring := FALSE;
      x1 := Xys(seg1*2-1);
      y1 := Xys(seg1*2);
      x2 := Xys(seg1*2+1);
      y2 := Xys(seg1*2+2);
      x3 := Xys(mseg1*2-1);
      y3 := Xys(mseg1*2);
      x4 := Xys(mseg1*2+1);
      y4 := Xys(mseg1*2+2);
      If ring_type = 2003 then
        last_vtx := last_info(seg1*2-1,istart,Info_Array,Xys.count);
        If ring_type2 = 2003 and angle_in_degrees(x1,y1,x2,y2,x4,y4,x3,y3) > 20. then
          Inner_ring := TRUE;
        End If;
      End if;

      IF Inner_ring or projector <=4 or projector = 10 or projector = 40 then 
         Codes(ii) :=0.0;
--         dbms_output.put_line('DRopped ' || seg1 || ' mseg ' ||mseg1 || ' ii ' || ii);
         kount := kount + 1; 
--      ELSIF (ii > 1 and seg1 <> Subject_Segs(ii-1)) or
--        (ii < n and seg1 <> Subject_Segs(ii+1)) THEN  -- its unique
--        NULL;

--    This is not a closest pair of segments

      Elsif found <> ii THEN
        
        
        direction1 := dotp(x1,y1,x2,y2,x3,y3,x4,y4);
     
      
--    If they go in the same direction, that is no good. Example: segments 1 and 5:

--           ----      -----
--          |   |     |    |
--        1 ^   v3   5^    v 7          Note that we don't get this relationship
--          |   |_____|    |_____       when the pipe_type is set to 'P'.

        IF direction1 > 0 THEN
          Codes(ii) :=0.0;
--          dbms_output.put_line('DRopped ' || seg1 || ' mseg ' ||mseg1 || ' ii ' || ii);
          kount := kount + 1;     
        ELSE
      
-- Now deal with the harder problem of antiparallel segments like 1 and 7.      
-- They leapfrog two other relationships, 1 with 3 and 3 with 7
-- because the picture is not this everwidening pipe:

--               -----  (x2,y2)  
--              |     |    
--              v 5   ^ 3
--              |     | 
--   (x3,y3) ----      -----
--          |     (x1,y1)  |
--        7 v              | 1
--          |              |_____
--        (x4,y4)
      
-- Check to see that there are points outside the polygon and drop the
-- relationship if any outside points are found.


         epsilon := -0.025;
         For ij in 1..20 loop  -- from 2.5% to 97.5% between (x1,y1) and (x4,y4)
           epsilon := epsilon + 0.05;
--           xc := (x1+x2)*0.5*(1.-epsilon) +(x3+x4)*0.5*epsilon;   -- was between pts 2 and 3
--           yc := (y1+y2)*0.5*(1.-epsilon) + (y3+y4)*0.5*epsilon;
           xc := x1*(1.-epsilon) + x4*epsilon;   -- was between pts 2 and 3
           yc := y1*(1.-epsilon) + y4*epsilon;
                  
           inside := POINT_IN_POLY(xc,yc,Xys,Info_array);
           
           if inside = 1 then
             xc := x2*(1.-epsilon) + x3*epsilon;   -- was between pts 2 and 3
             yc := y2*(1.-epsilon) + y3*epsilon;
                    
             inside := POINT_IN_POLY(xc,yc,Xys,Info_array);
           end if;
           counter := counter +1;

            If (inside = 0  and Pipe_Type= 'P') then --or (last_inside <> inside) then
               Codes(ii) := 0.0;
--          dbms_output.put_line('dropped ' || seg1 || ' mseg ' ||mseg1 || ' ii ' || ii);
               kount := kount + 1; 
               exit;
            End if;
   
         End Loop;
 
       END IF;
    END IF;
   End Loop;
   
 -- Remove all of the undesired elements from the arrays
--   dbms_output.put_line('>>>>>>>counter >>>>>>>>' || counter);
   if kount <> 0 then
     Pack(Subject_Segs);
     Pack(Matches);     
     Pack(Widths_p);
     Pack(Widths_q);
     Pack(Lengths);
     Pack(Xps);
     Pack(Yps);
     Pack(Xqs);
     Pack(Yqs);
     Pack(Codes);   -- must be packed last
   End If;
   
   Return Codes.count;
   
END WEED;
--
Function Last_Info(coord pls_integer,istart IN OUT NOCOPY PLS_INTEGER, Info_array IN OUT NOCOPY SDO_ELEM_INFO_ARRAY,XYS_count PLS_INTEGER) return pls_integer as
    
--  Get start and last vertex for a particular Info range specified by a ring coordinate
    
    last    pls_integer;
    Info_count   pls_integer := TRUNC(Info_array.count/3);
    
    Begin
         for ij in 1..Info_count loop

            istart := Info_Array(ij*3-2);
            If ij <> Info_count then
               last := Info_Array(ij*3+1)-1;
            else
               last := Xys_count;
            end if;

            if coord >= Info_Array(ij*3-2) and coord <= last then
               istart := TRUNC((istart+1)/2);
               last := TRUNC(last/2);
 --              dbms_output.put_line('start ' || istart || ' last ' || last);
               return last;
            end if;
         end loop;
End; 
--
Function greater_measure(seg1 pls_integer, seg2 pls_integer,Info_array IN OUT NOCOPY SDO_ELEM_INFO_ARRAY,XYS_count PLS_INTEGER)  return pls_integer as
        seg_count   pls_integer;
        last_vertex pls_integer;
        small       pls_integer :=4;
        istart      pls_integer;
        
-- For 2 segments in the same ring, find adjacencies: 
-- the segment difference where seg1 is presumed to be > seg2.
-- At the end of the ring, when seg2 maybe near the last vertex,
-- measure the difference as if segment 1 is now last+1, segment 2 is now last+2, ..
Begin
        last_vertex := last_info(seg1*2-1,istart,Info_array,Xys_count);
        seg_count := (seg1 - seg2);
        
-- We only want small differences like 1,2,3 so when we have a triangle or square
-- we don't want more than half the number of vertexes
 
        if TRUNC(last_vertex/2) < small then
           small := TRUNC(last_vertex/2);
        end if;
        if seg2 > seg1 and seg2 > last_vertex - small and seg1 + last_vertex -seg2- istart > 0 then
           seg_count := seg1 + last_vertex -seg2- istart;
        elsif seg2 > seg1 then
           seg_count := 100000; -- just a failure flag so we don't return negative
        end if;
        return seg_count;
End;
--
Function lesser_measure(seg1 pls_integer, seg2 pls_integer,Info_array IN OUT NOCOPY SDO_ELEM_INFO_ARRAY,XYS_count PLS_INTEGER)  return pls_integer as
        seg_count   pls_integer;
        last_vertex pls_integer;
        small       pls_integer := 4;
        istart      pls_integer;
        
-- For 2 segments in the same ring, find adjacencies:         
--      the segment difference where seg1 is presumed to be < seg2.
-- At the end of the ring, when seg1 maybe near the last vertex,
-- measure the difference as if segment 1 is now last+1, segment 2 is now last+2, ..

Begin
        last_vertex := last_info(seg2*2-1,istart,Info_array,Xys_count);
        seg_count := (seg2 - seg1);
--        dbms_output.put_line('seg_count ' || seg_count || ' seg1 ' || seg1 || ' seg2 ' || seg2);
-- We only want small differences like 1,2,3 
 
        if TRUNC(last_vertex/2) < small then
           small := TRUNC(last_vertex/2);
        end if;
        if seg1 > seg2 and seg1 > last_vertex - small and seg2 + last_vertex -seg1-istart > 0 then
--        dbms_output.put_line('last ' || last_vertex || ' istart ' || istart || ' small ' || small);
           seg_count := seg2 + last_vertex -seg1-istart;
        elsif seg1 > seg2 then
           seg_count := 100000; -- just a failure flag so we don't return negative
        end if;
        return seg_count;
End;
--
Function close_measure(seg1 pls_integer, seg2 pls_integer,Info_array IN OUT NOCOPY SDO_ELEM_INFO_ARRAY,XYS_count PLS_INTEGER)  return pls_integer as
        seg_count   pls_integer;
        last_vertex pls_integer;
        istart      pls_integer;


--For 2 segments in the same ring, find adjacencies:
--         6     5
--      +------+----+
--      |           | 4
--    7 |      +----+
--      |   2 /   3
--      +----+
--        1
-- Segment number difference  1,2,3 gets confused at end of a polygon (7 to 1)      
-- Find the shortest measure, so when we are near the start/end of polygon - 
-- consider 1 to be equivalent to segment 8 so difference between 7 and 1 is 1.
-- Elsewhere just find the segment difference.        
--        
--    find the shortest measure when we are near the start/end of polygon, else
-- just find the segment difference.

Begin
        last_vertex := last_info(seg1*2-1,istart,Info_array,Xys_count);
        seg_count := ABS(seg2 - seg1);
        if seg1 > seg2 and seg2+last_vertex-1 -seg1-istart < seg_count and seg2+last_vertex-seg1 -istart > 0 then
           seg_count := seg2+last_vertex-seg1 -istart;
        elsif seg2 > seg1 and seg1+last_vertex-1 -seg2-istart < seg_count and seg1+last_vertex-seg2 -istart > 0 then
          seg_count := seg1+last_vertex-seg2 -istart;
        end if;
        return seg_count;
End;
--
FUNCTION FIND_GET_PIPES(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150.,  minimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN PIPES_TYPE AS

-- SQL callable function to find pipes in a single geometry. 
--  A pipe is defined as a narrow part on a polygon where either the angle is <  angle check or the distance to the next
-- vertex is < short_check AND the length back (on both sides) to where the line
-- is no longer approximately straight is > minimum_len.


--            |
--            |__v_______________________
--            ____________________________>   angle < angle_check
--               ^      L > minimum_len
--            |
--            |__v_________________________
--            _____________________________|   d < short check
--               ^      L > minimum_len
--               |
--             Neck check is across the feature as shown here.

-- SQL> select gz_qa.find_pipes(sdogeometry) vertex,geo_id from cmp_150v995 where
--      gz_qa.get_small_angle(sdogeometry) < 2. and gz_qa.find_pipes(sdogeometry) is not null;

 rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
  Angles            MDSYS.SDO_LIST_TYPE;

  Pipe_Pars         PIPES_TYPE;
  Empty_PP          PIPES_TYPE;
  Xys               MDSYS.SDO_ORDINATE_ARRAY;
 
  Info_array        MDSYS.SDO_ELEM_INFO_ARRAY;
  Codes             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Matches           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Subject_Segs      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Widths_p          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Widths_q          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Lengths           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Link_fwd          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Link_bck          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  
  MBRs              MBRS_TYPE;
  Xps               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Yps               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Xqs               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Yqs               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Accum_lengths     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Order_array       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Pipe_nos          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Pipe_lengths      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  XLL               NUMBER;
  YLL               NUMBER;
  XUR               NUMBER;
  YUR               NUMBER;

  no_to_do          PLS_INTEGER;
  j                 PLS_INTEGER;
  k                 PLS_INTEGER;
  kk                PLS_INTEGER;
  seg               PLS_INTEGER;
  mseg              PLS_INTEGER;
  end_seg           PLS_INTEGER;
  end_mseg          PLS_INTEGER;
  match_behind      PLS_INTEGER;
  seg1              PLS_INTEGER;
  seg2              PLS_INTEGER;
  Info_count        PLS_INTEGER;

  peninsula_dist    NUMBER :=0.0;
  dist2             NUMBER :=0.0;
  SRID              NUMBER := NVL(Geom.SDO_SRID,0.0);
  checkit           NUMBER;
  width             NUMBER := 1.;
  found_width       NUMBER;
  cutoff_width      NUMBER := 1.0* max_width;  -- used to be 1.2
  length_match      NUMBER;
  length_ss         NUMBER;
  len               NUMBER;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;
  x3                NUMBER;
  y3                NUMBER;
  x4                NUMBER;
  y4                NUMBER;
  projector         NUMBER;
  last_Accum_len    NUMBER :=0.0;
  last_ss           PLS_INTEGER;
  last_seg          PLS_INTEGER;
  pos               PLS_INTEGER;
  n                 PLS_INTEGER;
  next              PLS_INTEGER := 0;
  last              PLS_INTEGER;
  pipes             PLS_INTEGER;
  pipe_no           PLS_INTEGER;
  linkfwd           PLS_INTEGER;
  loops             PLS_INTEGER := 0;
  istart            PLS_INTEGER;
  iend              PLS_INTEGER;

  pipe_types        VARCHAR2(2) := 'A'||Upper(pipe_type);
  sql_stmt          VARCHAR2(4000);

   Function Pythagoras(px1 number,py1 number,px2 number,py2 number) return number as
   
     dx   number := px2-px1;
     dy   number  := py2-py1;
     
   begin
   
    return sqrt(dx*dx + dy*dy);
    
   end;

  Function Scan_Info(segment pls_integer) return pls_integer as
  
-- Given a segment number that maybe the first segment in a ring, get the
-- next segment which is the last segment in that hole

  begin
    For ii in 2.. TRUNC(Info_array.count/3) loop
    
-- If its the 1st segment in a ring, return last segment in that ring
      if segment = TRUNC((Info_array(ii*3-2)+1)/2) then
         if ii*3 <> Info_array.count then
           return TRUNC((Info_array(ii*3+1)+1)/2)-2;
         else
           return TRUNC(Xys.count/2)-1;
         end if;
      else
         if ii*3 <> Info_array.count and segment = TRUNC((Info_array(ii*3+1)+1)/2)-2 then
           return TRUNC((Info_array(ii*3-2)+1)/2);
         elsif ii*3 = Info_array.count and segment = TRUNC(Xys.count/2)-2 then
           return TRUNC((Info_array(ii*3-2)+1)/2);
         end if;
      end if;
    End loop;
    return 0;
  end;

/*
  Function find_Info(coord pls_integer) return pls_integer as
    
--  Set ibeg and iend (start and end segments) from a particular Info range 
--  and a coordinate
    
    last    pls_integer;
    
    Begin
         for ij in 1..Info_count loop
--          dbms_output.put_line('info ' || info(ij*3-2));
            If ij <> Info_count then
               last := Info_Array(ij*3+1)-1;
            else
               last := Xys.count;
            end if;
 --            dbms_output.put_line('info ' || info(ij*3-2) || ' last ' || last);
            if coord >= Info_Array(ij*3-2) and coord <= last then
               istart := TRUNC((Info_Array(ij*3-2)+1)/2);
               last := TRUNC(last/2);
 --              dbms_output.put_line('start ' || istart || ' last ' || last);
               return last;
            end if;
         end loop;
End; 
*/

  Function Look_ahead(position pls_integer, find pls_integer, pmatch pls_integer) return pls_integer as

-- Look ahead for another part of the pipe
--seg 107 mseg 158 L 27.0169
--seg 108 mseg 158 L 103.8095 
--seg 108 mseg 157 L 4.4772 
--seg 109 mseg 157 L 61.3013 
--seg 110 mseg 157 L .0111 
--seg 110 mseg 156 L 79.8392 
--seg 110 mseg 136 L 9.8798   <-- gap
--seg 110 mseg 135 L 8.9999   <-- gap
--seg 111 mseg 156 L 44.6972  <-- seg and mseg continue here 
--seg 111 mseg 155 L 10.8309 

    last_pos   pls_integer := position + 10;
    match      pls_integer := pmatch;
    mtry       pls_integer;
    found      pls_integer :=0;
  begin
     if last_pos > Subject_segs.count then
       last_pos := Subject_segs.count;
     end if;
     for loops in 1..2 loop
       if loops = 3 then
       for ii in position ..last_pos loop
        if Pipe_nos(ii) = 0 and  
          (Subject_segs(ii)-find) <=1 and Matches(ii) = match then
           found := ii;
           exit;
        end if;
     end loop;
       end if;
     if loops <> 3 then
     for ii in position ..last_pos loop
        if Pipe_nos(ii) = 0 then 
          if ABS(Subject_segs(ii)-find) <= 1 and ABS(Matches(ii) - match) <=1 then
           if Widths_p(ii) = 10000000000. and found <> 0 then
             return found;
           else
             return ii;
           end if;
          else
 
            mtry := Scan_info(match);
--            if find = 327 or find=328 then
--               dbms_output.put_line('mtry ' || mtry || ' match was ' || match || ' find ' || find || ' ss ' || subject_segs(ii));
--            end if;
            if mtry <> 0 then
              if ABS(Subject_segs(ii)-find) <=1  then
--               match_behind := mtry;
--               dbms_output.put_line('returning ii ' || ii || ' mtry ' || mtry );
               return ii;
              end if;
            end if; 
          end if;
        end if;
     end loop;
     end if;
     if loops = 3 then
     for ii in position ..last_pos loop
        if Pipe_nos(ii) = 0 and  
          ABS(Subject_segs(ii)-find) <=1 and Matches(ii) = match -2 then
           return ii;
        end if;
     end loop;
     end if;
     end loop;
     return 0;
  end;

  Function Build_pipes return number as
  
  -- Function to aggregate adjacent segments and matching segments into 
  -- a discrete pipe.
  
     Pipe_lengths     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
     no_pipes   pls_integer := 0;
     pipe_no    pls_integer := 1;
     done       pls_integer := 0;
     behind     pls_integer := 0;
     backup    pls_integer := 0;
     loops      pls_integer :=0;
     try        pls_integer;
     seg1       pls_integer;
     seg2       pls_integer;
     seg1_j     pls_integer;
     seg2_j     pls_integer;
     len        number;
     same_pipe  boolean;
     

 

  begin
-- Assign pipes and accumulate the distances to get a pipe length
    pos := 1;
    last_ss := Subject_segs(1);
    Pipe_lengths.extend(1);
    Pipe_lengths(1) :=0.0;
  /*
     While done=0 and pos <= Subject_segs.count loop
       loops := loops + 1;
       if Pipe_nos(pos) = 0.0 then
-- These simple pictures suffice when there are no  'U's
-- like this     -----
--                    \
--                    /
--               -----   When this happens there are multiple parallel
--              |        segments for a given segment which interrupts
--               -----   the sorted segments

-- If this piece belongs to the current pipe part, then include it.
-- Pipe building to get an accumulated length. Link on either the Subject segs
-- or on the matches.
--
--
--       +-------------+----------+
--             1             2          3             Subject Segs: 1 or 2 connect to 1

--       +-----------+----------------+
--            m+1          m                          Matching  unspecified

--       OR
--
--       +-------------+----------+--------------+
--             1             2          3             Subject Segs: 1, 2 or 3 connect to 1

--       +-----------+----------------+
--            m+1          m                          Matching: m and m+1

 

--      Is current position part of current pipe?
--
        same_pipe := false;
        if behind = 0 then
          behind := pos;
        end if;

--        if  (Subject_segs(pos) - Subject_segs(behind) <= 2 and 
--           (ABS(matches(behind) -Matches(pos)) <=1))  then
--             same_pipe := true;
--        else
--         when there is an interruption, look ahead
           try := Look_ahead(pos,Subject_segs(behind),matches(behind));

           if try <> 0 then
             if backup = 0 and try <> pos then 
--              dbms_output.put_line('SETTING BACKUP ' ||pos || ' SS ' || subject_segs(pos) || ' and ' || Matches(pos));
             backup := pos; 
             end if;
             pos := try;
             same_pipe := true;
           elsif backup <> 0 then
             pos := backup;
--             dbms_output.put_line('SET pos to backup ' ||pos );
             if Pipe_nos(pos) = 0 and (Subject_segs(pos) - Subject_segs(behind) <= 2 and 
               (ABS(matches(behind) -Matches(pos)) <=2))  then
             same_pipe := true;
             backup :=0;
             else

             backup :=0;
             behind :=0;
             continue;
             end if;
--           else
--         looking ahead failed so just exit
--            exit;
           end if;
--        end if;
        if same_pipe then
--        if match_behind = 438 then 
--          dbms_output.put_line('>>>at ' ||pos || ' same pipe '||pipe_no||' SS ' || subject_segs(pos) || ' and ' || Matches(pos));
--          match_behind :=0;
--        end if;
           if Lengths(pos) <> Lengths(behind) or behind = pos then
              Pipe_lengths(pipe_no) := Pipe_lengths(pipe_no) + Lengths(pos);
--              dbms_output.put_line('at ' ||ii || ' ' || ' SS ' || subject_segs(ii) || ' ' ||Matches(ii));
--            dbms_output.put_line('At pos ' || pos || ' PIPE ' || pipe_no || ' Len now ' || round(pipe_lengths(pipe_no),3) ||  ' ' || round(lengths(pos),3));
            end if;
            Pipe_nos(pos) := pipe_no;
            behind := pos;
--            match_behind := Matches(behind);
        else
--        new pipe starts
--        dbms_output.put_line('at ' ||pos || ' new pipe ' || (pipe_no+1) ||' SS ' || subject_segs(pos) || ' and ' || Matches(pos) || ' L ' || round(lengths(pos),3));
           behind := pos;

           pipe_no := pipe_no +1;
           Pipe_lengths.extend(1);
           Pipe_nos(pos) := pipe_no;
           Pipe_lengths(pipe_no) :=  Lengths(pos);
        end if; 

        end if;
        pos := pos+1;
 --       dbms_output.put_line('POS ' || pos || ' backup ' || backup);
        if pos > Subject_segs.count and backup <> 0 then
          pos := backup;
          backup := 0;
        end if;
     end loop;
     
  */
--     for ii in 1..Subject_Segs.count loop
--       dbms_output.put_line('ii ' || ii || ' pipe ' || pipe_nos(ii) || ' S ' || subject_segs(ii) || ' M '|| Matches(ii) || ' C ' || Codes(ii) || ' LB ' || Link_bck(ii) || ' LF ' || Link_fwd(ii));       
--     end loop;
    
 --  Make Pipe Lengths from each side
 
     for ii in 1..pipe_no loop
         seg1 := 1000000;
         seg1_j :=0;
         seg2 := 0;
         seg2_j :=0;
         for jj in 1..Subject_segs.count loop
          If Pipe_nos(jj) = ii then
             if Subject_segs(jj) < seg1 then
             seg1 := Subject_segs(jj);
             seg1_j := jj;
             end if;
             if Subject_segs(jj) > seg2 then
             seg2 := Subject_segs(jj);
             seg2_j := jj;
             end if;
          end if;
        end loop;
     
--        dbms_output.put_line('seg1 ' || seg1 || ' seg2 ' || seg2 || ' xys ' || xys.count);
        len := GZ_UTIL_ZONE.Accurate_length(Xys,seg1,seg2);
--        dbms_output.put_line('LEN ' || round(len,4) || ' was ' || round(Pipe_lengths(ii),4));
     end loop;
     
     for ii in 1..Subject_Segs.count loop
 --      dbms_output.put_line('ii ' || ii || ' pipe ' || pipe_nos(ii) || ' K ' || subject_segs.count);
       Accum_lengths(ii) := Pipe_lengths(Pipe_nos(ii));
     end loop;
     return pipe_no;
  end;

/*  
  Procedure move_end_of_rings as

-- When a polygon begins in the middle of a pipe, then we have this situation
-- (suppose their are 100 vertices in the 1st ring):

--    Subject   matches
--    Segs                         _
--     96       94                | |
--     97       93                | |
--     98       92                | |
--     99       91                | |
--      1       25     start vtx  + |
--      2       24                | |
--      3       23
--     ...
  
  begin
       For ii in 1..rings loop
          If ii <> rings then
            end_segment := TRUNC(Info(ii*3+1)/2)-1;
          else
            end_segment := TRUNC((Xys.count-2)/2);
          end if;
          last_segment := end_segment;
  
       -- Look for a range to move
       start_range :=0;
        for ij in 1..Matches.count loop
           If start_range =0 and (Matches(ij) =  end_segment or Matches(ij) = end_segment-1) then
              start_range := ij;
           end if;
           If Matches(ij) = last_segment then
               end_range := ij;
               last_segment := last_segment-1;
           End if;
         end loop;
         
         
       End Loop;
  end;
*/
BEGIN
--==================================Setup=======================================

-- Make a set of rectangles (minimum bounding) that encircle the polygon's
-- perimeter. These are used for fast distance measurements between vertices.
--
    Xys := geom.sdo_ordinates;
    Info_Array := geom.sdo_elem_info;
    Info_count := TRUNC(Info_Array.count/3);
    Set_Many_Mbrs(XYs,Info_Array,MBRs,XLL,YLL,XUR,YUR);
    
--    for ii in 1..MBRs.count loop
--       dbms_output.put_line('ii ' || ii || ' mbr ' || mbrs(ii).lo || ' ' || Mbrs(ii).hi);
--    end loop;


--  Check segments that are near to each other to see if they are semiparallel.
-- Generates coordinate numbers (Subject_Segs and Matches) that are always odd
--    XY(1),Xy(2)              XY(3),Xy(4)                   XY(5),Xy(6)
--    segment: 1               3                               5 etc
--    +-------------------------+-------------------------------+
dbms_output.put_line('CUTOFF '|| cutoff_width);

 checkit := CHECK_NEARBY(XYs,Info_Array,MBRs,XYs,Info_Array,MBRs,Codes,Subject_Segs,Matches,Widths_p,Widths_q,Lengths,
                         Xps,Yps,Xqs,Yqs,SRID,max_angle_diff,cutoff_width,2,pipe_types,TRUE);
--dbms_output.put_line('checkit ' || checkit);

-- Find peninsula Length
-- 4 cases       1)                          2)
--              --   shorter           ---------------- longer      Match
--          ------------                       --                Subject_seg
--               3)                          4)
--               ------  ahead                ----  behind          Match
--          -------                             -------           Subject_seg

--  Widths := Get_Pipe_widths(Geom,Subject_Segs,Matches,Distances,cutoff_width);
--  if Widths is NULL then
--      RETURN NULL;
--  end if;


   n := Codes.count;

    if n = 0 then 
       RETURN Empty_PP;
    end if;
 
 
    Order_array.extend(n);
       
    -- Convert vertices to segment numbers
    for ii in 1..n loop
      Subject_Segs(ii) := 1. + TRUNC(Subject_Segs(ii)/2);
      Matches(ii) := 1. + TRUNC(Matches(ii)/2);   
      Order_array(ii) := ii;
--      dbms_output.put_line('SS ' || subject_segs(ii) || ' found with ' || matches(ii) || ' WP ' || round(widths_p(ii),1));
    end loop;

   
    Shellsort4(Subject_segs,Matches,Widths_p,Order_array,1,n);
--    for ii in 1..n loop
--      dbms_output.put_line('SS ' || subject_segs(ii) || ' found with ' || matches(ii));
--    end loop;    
    Sort_also(Lengths,Order_array);
    Sort_also(Widths_q,Order_array);
    Sort_also(Codes,Order_array);
    Sort_also(Xps,Order_array);
    Sort_also(Yps,Order_array);
    Sort_also(Xqs,Order_array);
    Sort_also(Yqs,Order_array); 
    
-- We need to weed situations where a segment is actually part of a closer pipe
-- yet Check_nearby also reports its parallelism with another further away.
--
--   For example:   1 has a relationship with 3, so its parallel relationship
--                  with 5 is of no interest and its antiparallel relationship
--                  with 7 (which has a greater segment difference) is also not useful.
--   These extra relationships cause problems when  we use linked lists to
--   build the pipes and get the extra unwanted relationships.

--           ----      -----
--          |   |     |    |
--        1 ^   v3   5^    v 7
--          |   |_____|    |_____


    n := Weed(pipe_type,Xys, Info_Array,Codes,Subject_Segs,Matches,Widths_p,Widths_q,Lengths,
                         Xps,Yps,Xqs,Yqs);                 
    if n = 0 then
       RETURN Empty_PP;
    end if;
    
--    for ii in 1..n loop
 
--      dbms_output.put_line('SSnow ' || subject_segs(ii) || ' found with ' || matches(ii) || ' WP ' || round(widths_p(ii),1)|| ' WQ ' || round(widths_q(ii),1));
--    end loop;
    -- Sort the arrays so the distances can be accumulated.
    
    Accum_Lengths.extend(n);
    Pipe_nos.extend(n);
    Link_fwd.extend(n);
    Link_bck.extend(n);
    for ii in 1..n loop  
      Accum_lengths(ii) := 0.0;  
      Pipe_nos(ii) := 0.0;
      Link_bck(ii) := 0.0;
      Link_fwd(ii) := 0.0;
    end loop;

--    dbms_output.put_line('AFTER WEED'); 
 
 -- Build the Pipes by aggregating the matched segments.

   -- Now link to find pipe ends. 
   -- LB=0 (Link Back) is the start, LF=0 (Link Forward) is the end
   
    for ii in 1..n loop
      if Link_fwd(ii) = 0 then
        for jj in ii+1..n loop

          if ii <> jj and close_measure(Matches(jj),Matches(ii),Info_array,Xys.count) <=1 and 
             greater_measure(Subject_segs(jj),Subject_segs(ii),Info_array,Xys.count) <=1 then
             Link_bck(jj) := ii;
             Link_fwd(ii) := jj;
--             if Subject_segs(jj) = 239 or Subject_segs(ii) = 162 then
--             dbms_output.put_line('&&&&&&&&loop1 setting ' || ii || ' and ' || jj);
--             end if;
             exit;
          end if;
        end loop;
       end if;
    end loop;
    
    for ii in 1..n loop
      if Link_fwd(ii) = 0 then
        for jj in ii+1..n loop
          if ii <> jj and close_measure(Matches(jj),Matches(ii),Info_array,Xys.count) <=2 
               and greater_measure(Subject_segs(jj),Subject_segs(ii),Info_array,Xys.count) <=2 then
             if link_bck(jj) = 0 then
               Link_bck(jj) := ii;
--               if Subject_segs(jj) = 239 or Subject_segs(ii) = 162 then
--               dbms_output.put_line('%%%%%%%%%%%%%%Loop2 setting ' || jj || ' to ' || ii);
--               end if;
             end if;
             Link_fwd(ii) := jj;
--             dbms_output.put_line('loop2 setting ' || ii || ' to ' || jj);
             exit;
          end if;
        end loop;
       end if;
    end loop;
 --- This is new September   
    for ii in 1..n loop
      if Link_fwd(ii) = 0 then
        for jj in ii+1..n loop
          if ii <> jj and close_measure(Matches(jj),Matches(ii),Info_array,Xys.count) <=3 
               and greater_measure(Subject_segs(jj),Subject_segs(ii),Info_array,Xys.count) <=1 then
             if link_bck(jj) = 0 then
               Link_bck(jj) := ii;
--               if Subject_segs(jj) = 239 or Subject_segs(ii) = 162 then
--               dbms_output.put_line('********Loop2 setting ' || jj || ' to ' || ii);
--               end if;
             end if;
             Link_fwd(ii) := jj;
--             dbms_output.put_line('loop2 setting ' || ii || ' to ' || jj);
             exit;
          end if;
        end loop;
       end if;
    end loop;
    
    for ii in 1..n loop
      if Link_fwd(ii) = 0 then
        for jj in ii+1..n loop
          if ii <> jj and close_measure(Matches(jj),Matches(ii),Info_array,Xys.count) <=1 
               and greater_measure(Subject_segs(jj),Subject_segs(ii),Info_array,Xys.count) <=3 then
             if link_bck(jj) = 0 then
               Link_bck(jj) := ii;
--               if Subject_segs(jj) = 239 or Subject_segs(ii) = 162 then
--               dbms_output.put_line('$$$$$$$Loop2 setting ' || jj || ' to ' || ii);
--               end if;
             end if;
             Link_fwd(ii) := jj;
--             dbms_output.put_line('loop2 setting ' || ii || ' to ' || jj);
             exit;
          end if;
        end loop;
       end if;
    end loop;
--- End of new
    
    
    for ii in 2..n loop
      if Link_bck(ii) = 0 then
        for jj in 1..ii-1 loop
          if (close_measure(Matches(jj),Matches(ii),Info_array,Xys.count) <=2 and lesser_measure(Subject_segs(jj),Subject_segs(ii),Info_array,Xys.count) <=1) then 
             Link_bck(ii) := jj;
--             dbms_output.put_line('loop33 setting ' || ii || ' to ' || jj);
             if link_fwd(jj) = 0.0 then
               Link_fwd(jj) := ii;
--               dbms_output.put_line('loop3 setting ' || jj || ' to ' || ii);
             end if;
             exit;
          end if;
        end loop;
       end if;
    end loop;
    --

/*
    for ii in 1..n loop
      if Link_bck(ii) = 0 then
        for jj in 1..n loop
          if ii<> jj and Link_fwd(ii) <> jj and (((greater_measure(Matches(jj),Matches(ii)) <=1) 
               and (greater_measure(Subject_segs(jj),Subject_segs(ii)) <=2 )) or
               ((greater_measure(Subject_segs(jj),Subject_segs(ii)) <=1) and (greater_measure(Matches(jj),Matches(ii))=2 or greater_measure(Matches(jj),Matches(ii))=3)))
               then
-- if ii<> jj and Link_fwd(ii) <> jj and (((Matches(jj)-1 = Matches(ii) or Matches(jj)=Matches(ii)) 
--               and (Subject_segs(jj) = Subject_segs(ii) or Subject_segs(jj)-1 = Subject_segs(ii) or Subject_segs(jj)-2 = Subject_segs(ii) )) or
--               ((Subject_segs(jj) = Subject_segs(ii) or Subject_segs(jj)+1 = Subject_segs(ii)) and (Matches(jj)-2 = Matches(ii) or Matches(jj)-3 = Matches(ii))))
--               then
             -- Dont link forward when we are at the start of a pipe
             if ii > 2 and (Link_fwd(ii-1) = 0 or Matches(ii-1) - Matches(ii) > 3) then
               exit;
             else
             Link_bck(ii) := jj;
             dbms_output.put_line('loop4 setting ' || ii || ' to ' || jj);
             if link_bck(jj) <> ii then
             dbms_output.put_line('Loop4 setting '||jj || ' to ' ||ii);
             Link_fwd(jj) := ii;
             end if;
             exit;
             end if;
          end if;
        end loop;
       end if;
    end loop;
*/
  for ii in 1..n loop
      if Link_fwd(ii) = 0 then
        for jj in ii+1..n loop
--                 if ii=1 then
--         dbms_output.put_line('CLose ' ||  close_measure(Matches(jj),Matches(ii)) || ' GR ' || greater_measure(Subject_Segs(jj),Subject_segs(ii)));
--         end if;
         if close_measure(Matches(ii),Subject_Segs(jj),Info_array,XYs.count) <=2 
               and greater_measure(Subject_segs(ii),Matches(jj),Info_array,XYs.count) <=2 then
--          if close_measure(Matches(jj),Matches(ii)) <=2 
--               and greater_measure(Subject_segs(jj),Subject_segs(ii)) <=2 then
 
--             if link_bck(jj) = 0 then
               Link_bck(jj) := ii;
--               if Subject_segs(jj) = 239 or Subject_segs(ii) = 162 then
--               dbms_output.put_line('#######@@@loop5 setting ' || jj || ' to ' || ii || ' M ' ||Matches(ii) || ' SS ' || Subject_segs(jj));
--            dbms_output.put_line('G' || greater_measure(Subject_segs(ii),Matches(jj),Info_array,XYs.count) ||
--                                 'C' || close_measure(Matches(ii),Subject_Segs(jj),Info_array,XYs.count));
--             end if;
             Link_fwd(ii) := jj;
--             dbms_output.put_line('loop55 setting ' || ii || ' to ' || jj);
             exit;
          end if;
        end loop;
       end if;
    end loop;

--     dbms_output.put_line('LB ' || link_bck(1) || ' LF ' || link_fwd(1));
--     dbms_output.put_line('LB ' || link_bck(2) || ' LF ' || link_fwd(2));
--     dbms_output.put_line('LB ' || link_bck(3) || ' LF ' || link_fwd(3));
--     dbms_output.put_line('LB ' || link_bck(4) || ' LF ' || link_fwd(4));
--      for ii in 1..Subject_Segs.count loop
--       dbms_output.put_line('BF ' || ii || ' pipe ' || pipe_nos(ii) || ' S ' || subject_segs(ii) || ' M '|| Matches(ii) || ' C ' || Codes(ii) || ' LB ' || Link_bck(ii) || ' LF ' || Link_fwd(ii));       
--     end loop;   
   pipe_no := 0;
-- Now trace the Pipe(s) from start to finish
   next :=0;
   While next < n loop
      next := next + 1;
--      dbms_output.put_line('Next ' || next || ' n ' || n );
--    Start begins with link back set to zero   
      if Link_bck(next) = 0 then
         loops := 0;
         Linkfwd := next; 
         pipe_no := pipe_no+1;
--         dbms_output.put_line('Found Pipe ' || pipe_no || ' at ' || next);
         Pipe_nos(next) := pipe_no;
         While loops < 1000 and linkfwd > 0 loop
           loops := loops + 1;
           last := linkfwd;
           linkfwd  := Link_fwd(linkfwd);
           Link_fwd(last) := -ABS(Link_fwd(last));
           if ABS(linkfwd) > next then
             next := ABS(linkfwd);
           end if;
--           dbms_output.put_line('AT ii ' || next || ' Now lf ' || linkfwd);
           if linkfwd >0 then              
              Pipe_nos(linkfwd) := pipe_no;
              if Link_fwd(linkfwd) < 0 then     -- detected a loop
                 if last < n and Link_bck(last+1) = last then
                  Link_fwd(last) := last+1; 
                 else
                 Link_fwd(last) :=0;
                 end if;
                 Link_bck(linkfwd) :=0;
--              pipe_no := pipe_no+1;
              dbms_output.put_line('LOOP detected at ' || pipe_no || ' last ' || last);
              loops :=0;
              While loops < 1000 and linkfwd <> 0 loop
              loops := loops + 1;
              if linkfwd >0 then              
                Pipe_nos(linkfwd) := pipe_no;
--                dbms_output.put_line('updating ' || linkfwd || ' with pipe ' || pipe_no);
              end if;
              linkfwd := ABS(Link_fwd(linkfwd));
              end loop;

              exit;
             end if;
           end if;
         End Loop;
      end if;
   end loop;
   
   -- Check for consistency
   
   for ii in 1..n loop
     linkfwd := ABS(Link_fwd(ii));
     if linkfwd <> 0 and Link_bck(linkfwd) =0 then
        Link_fwd(ii) :=0;
     end if;
   end loop;
   
   for ii in 1..n loop
     linkfwd := ABS(Link_fwd(ii));
     if ii > 1 and Pipe_nos(ii) = 0 and linkfwd <> 0 and Link_bck(ii) <>0 then
        If close_measure(Subject_segs(ii),Subject_segs(ii-1),Info_array,Xys.count) < 2 and close_measure(Matches(ii),Matches(ii-1),Info_array,Xys.count) <=2 then
           Pipe_nos(ii) := Pipe_nos(ii-1);
        Elsif close_measure(Subject_segs(ii),Subject_segs(ii-1),Info_array,Xys.count) <= 2 and close_measure(Matches(ii),Matches(ii-1),Info_array,Xys.count) < 2 then
           Pipe_nos(ii) := Pipe_nos(ii-1);
        End if;
     end if;
   end loop;
   
   
--     for ii in 1..Subject_Segs.count loop
--       dbms_output.put_line('bf ' || ii || ' pipe ' || pipe_nos(ii) || ' S ' || subject_segs(ii) || ' M '|| Matches(ii) || ' C ' || Codes(ii) || ' LB ' || Link_bck(ii) || ' LF ' || Link_fwd(ii));       
--     end loop;

   for ii in 1..n loop
     Link_fwd(ii) := ABS(Link_fwd(ii));
     If Pipe_nos(ii) = 0 and Link_fwd(ii) <> 0 then
        if Pipe_nos(Link_fwd(ii)) = 0 then
           pipe_no := pipe_no+1;
--           dbms_output.put_line('at ii ' || ii || ' set fwd pipe for ' || link_fwd(ii)|| ' pipe ' || pipe_no);
           Pipe_nos(Link_fwd(ii)) := pipe_no;
        end if;
        Pipe_nos(ii) := Pipe_nos(Link_fwd(ii));
     end if;
     If Pipe_nos(ii) = 0 and Link_bck(ii) <> 0 then
        if Pipe_nos(Link_bck(ii)) = 0 then
           pipe_no := pipe_no+1;
--           dbms_output.put_line('at ii ' || ii || ' set bck pipe for ' || link_fwd(ii) || ' pipe ' || pipe_no);
           Pipe_nos(Link_bck(ii)) := pipe_no;
        end if;        
        Pipe_nos(ii) := Pipe_nos(Link_bck(ii));
     end if;
     
   end loop;

-- Check for a bad forward link
  
/*  
   for ii in 1..n loop
     linkfwd := Link_fwd(ii);
     loops :=0;
     While linkfwd <> 0 and loops < 1000 loop
       loops := loops + 1;
       -- End the pipe when the numbers dont match
       if Pipe_nos(linkfwd) <> Pipe_nos(ii) then
          Link_fwd(ii) := 0;
       end if;
     end loop;
   end loop;
*/   
--     for ii in 1..Subject_Segs.count loop
--       if link_bck(ii) = 0 then
--       dbms_output.put_line('ii ' || ii || ' pipe ' || pipe_nos(ii) || ' S ' || subject_segs(ii) || ' M '|| Matches(ii) || ' C ' || Codes(ii) || ' LB ' || Link_bck(ii) || ' LF ' || Link_fwd(ii) || ' length ' || round(Lengths(ii),3));       
--       end if;
--     end loop;
     
      --  Make Pipe Lengths from sum of all overlap lengths
      
     Pipe_lengths.extend(pipe_no);
     for ii in 1..pipe_no loop

         last_seg := 0;
         for jj in 1..Subject_segs.count loop
          If Pipe_nos(jj) = ii then
             if Link_bck(jj) =0 then
               seg1 := Subject_segs(jj)+1;
               if seg1 > Last_info(seg1*2-1,istart,Info_Array,Xys.count) then
                  seg1 := istart;
               end if;
               len := Lengths(jj);
             end if;
             if Link_bck(jj) <> 0 then 
            len := len + Lengths(jj);
            end if;
            last_seg := Subject_segs(jj);
             if Link_fwd(jj) = 0 then
            seg2 := Subject_segs(jj)-1;
            if seg2 = seg1 then
              seg2 := seg1+1;
            end if;
            
             end if;
          end if;
        end loop;
     
--        dbms_output.put_line('seg1 ' || seg1 || ' seg2 ' || seg2 || ' xys ' || xys.count);
--        if seg1 < seg2 then
--          len := len + Serena_ZONE.Accurate_length(Xys,seg1,seg2);
--        end if;
--        dbms_output.put_line('LEN ' || round(len,4) );
        Pipe_lengths(ii) := round(len,4);
        for jj in 1..Subject_segs.count loop
          If Pipe_nos(jj) = ii then
             Accum_lengths(jj) := round(len,4);
          End if;
        end loop;
     end loop;
     
     

--     for ii in 1..Subject_Segs.count loop
--       if link_bck(ii) = 0 then
--       dbms_output.put_line('ii ' || ii || ' pipe ' || pipe_nos(ii) || ' S ' || subject_segs(ii) || ' M '|| Matches(ii) || ' C ' || Codes(ii) || ' LB ' || Link_bck(ii) || ' LF ' || Link_fwd(ii) || ' length ' || round(Lengths(ii),3)|| 'AL ' || accum_lengths(ii));       
--       end if;
--     end loop;
--  pipes := Build_pipes;

/*
  
  For no in 1..pipe_no loop
   -- Here we build entire pipes
  for ii in 1..n loop
     If Pipe_nos(ii) = no and Link_bck(ii) =0 then
      seg := Subject_Segs(ii);
      mseg := Matches(ii);
      istart := ii;
     end if;
     If Pipe_nos(ii) = no and Link_fwd(ii) =0 then
      end_seg := Subject_Segs(ii);
      end_mseg := Matches(ii);
      iend := ii;
     end if;
  end loop;
--dbms_output.put_line('OK');


      Pipe_pars(no).pipe_no := no; 
      Pipe_pars(no).Length := Lengths(istart); 
 
      Pipe_pars(no).Accum_Len := Accum_Lengths(istart);
      Pipe_pars(no).Seg := seg;  -- start vertex 
      Pipe_pars(no).end_Seg := end_seg;  -- end vertex 
      Pipe_pars(no).Mseg := mseg;       -- matching segment
      Pipe_pars(no).end_Mseg := end_mseg;  -- end matching seg
/*
      -- save the projecting vertex 1-> 2 or 2->4
      Pipe_pars(next).Projector := projector;
--      dbms_output.put_line('Saved '|| seg || ' mseg ' || mseg ||' P '||projector);

--      dbms_output.put_line('stored projector ' || projector || 'ss ' || seg || ' m ' || mseg  || ' w ' || round(widths_p(ii),3) || ' w ' || round(widths_q(ii),3)); -- ||' x1 ' || round(x1,7) || 'y1 ' || round(y1,7) );
--     dbms_output.put_line(' x2 ' || round(x2,7) || 'y2 ' || round(y2,7)  );
-- Store remaining details
      Pipe_pars(no).Width_p := Widths_p(ii);
      Pipe_pars(no).Width_q := Widths_q(ii); --length_match;
      Pipe_pars(no).XP := Xps(ii);
      Pipe_pars(no).YP := Yps(ii); 
      Pipe_pars(no).XQ := Xqs(ii);
      Pipe_pars(no).YQ := Yqs(ii); 

 


 -- END LOOP;
 */ 
 
-- Select the pipe segments to keep and save as a structure array

  next :=0;

  For ii in 1..n Loop


     found_width := Widths_p(ii);
     if Widths_q(ii) < found_width then
       found_width := Widths_q(ii);
     end if;
--     if Widths_q(ii) > 1.5 * max_width or Widths_p(ii) > 1.5*max_width then
--        found_width := max_width + 1.;
--     end if;
     projector := (Codes(ii) - TRUNC(Codes(ii)))*100.;
--     dbms_output.put_line('SSS ' ||round(found_width,3) || ',' || max_width || ' PJ ' || projector);
--     if found_width <= cutoff_width and (projector = 12 or projector = 43 or projector = 42 or projector = 13) 
--        then --accum_lengths(ii) > 0.0 then

--   Edit very Short pipes that do not go forward or back

     if Accum_Lengths(ii) < minimum_len and Link_bck(ii) = 0 and Link_fwd(ii) = 0 then
       Pipe_nos(ii) :=  0;
     end if;
-------------------------------VVVVVVVVVnew Sidey
     if Pipe_nos(ii) <> 0 then --and Accum_Lengths(ii) > minimum_len then
--     if subject_segs(ii) = 764 then
   dbms_output.put_line((next+1)||'PIPE ' || Pipe_nos(ii)|| ' SS ' || subject_segs(ii) || ' M ' || matches(ii) || ' W ' || round(Widths_p(ii),1) || ' W ' || round(Widths_q(ii),1) || ' Code ' || codes(ii) || ' bck ' || Link_bck(ii) || ' fwd ' || Link_fwd(ii) ||' AL ' || ROUND(Accum_lengths(ii),1) || ' L ' || round(Lengths(ii),1)); -- || ' xp ' || ROUND(Xps(ii),7)|| ','||round(Yps(ii),7) || ROUND(Xqs(ii),7)|| ','||round(Yqs(ii),7)); -- || ' LL ' || round(Lengths(ii),1));
--   dbms_output.put_line( ' xp ' || ROUND(Xps(ii),7)|| ','||round(Yps(ii),7) || ROUND(Xqs(ii),7)|| ','||round(Yqs(ii),7)); --'SS ' || (subject_segs(ii)) || ' match ' || (matches(ii)) || ' D ' || round(distances(ii),8));
--    end if;

      next := next + 1;
      Pipe_pars(next).pipe_no := Pipe_nos(ii); 
      Pipe_pars(next).Length := Lengths(ii);  
      Pipe_pars(next).Accum_Len := Accum_Lengths(ii);

      seg :=  Subject_Segs(ii);
      Pipe_pars(next).Seg := seg;  -- start vertex
      mseg := Matches(ii);
      Pipe_pars(next).Mseg := mseg;       -- matching segment

      -- save the projecting vertices 12, 43, 13 or 42
      --       4                3         4              3
      --       +-------<--------+          +-------------+
      --          |          |             |          |           | = projectors
      --        1 +---->-----+2        1 +------------+ 2
      --              12                      42
      
      Pipe_pars(next).Projector := projector;
     
--      dbms_output.put_line('stored projector ' || projector || 'ss ' || seg || ' m ' || mseg  || ' w ' || round(widths_p(ii),3) || ' w ' || round(widths_q(ii),3)); -- ||' x1 ' || round(x1,7) || 'y1 ' || round(y1,7) );
--     dbms_output.put_line(' x2 ' || round(x2,7) || 'y2 ' || round(y2,7)  );
-- Store remaining details
      Pipe_pars(next).Width_p := Widths_p(ii);
      Pipe_pars(next).Width_q := Widths_q(ii);
      Pipe_pars(next).XP := Xps(ii);
      Pipe_pars(next).YP := Yps(ii); 
      Pipe_pars(next).XQ := Xqs(ii);
      Pipe_pars(next).YQ := Yqs(ii);
      Pipe_pars(next).Seg_length := 
            Distance_fcn(Xys(seg*2-1),Xys(seg*2),Xys(seg*2+1),Xys(seg*2+2));
      Pipe_pars(next).MSeg_length := 
            Distance_fcn(Xys(mseg*2-1),Xys(mseg*2),Xys(mseg*2+1),Xys(mseg*2+2));
      
      
-- Now figure the overlap length
      if projector = 12 then
         Pipe_pars(next).overlap := Pipe_pars(next).Seg_length;
      elsif projector = 43 then
         Pipe_pars(next).overlap := 
            Distance_fcn(Xps(ii),Yps(ii),Xqs(ii),Yqs(ii)); 
      elsif projector = 13 then
         Pipe_pars(next).overlap := 
            Distance_fcn(Xys(seg*2-1),Xys(seg*2),Xqs(ii),Yqs(ii));
      elsif projector = 42 then
         Pipe_pars(next).overlap := 
            Distance_fcn(Xps(ii),Yps(ii),Xys(seg*2+1),Xys(seg*2+2)); 
      end if;
      
-- Now figure the moverlap length
      if projector = 43 then
         Pipe_pars(next).moverlap := Pipe_pars(next).mSeg_length;
      elsif projector = 12 then
         Pipe_pars(next).moverlap := 
            Distance_fcn(Xps(ii),Yps(ii),Xqs(ii),Yqs(ii)); 
      elsif projector = 42 then
         Pipe_pars(next).moverlap := 
            Distance_fcn(Xys(mseg*2+1),Xys(mseg*2+2),Xqs(ii),Yqs(ii));
      elsif projector = 13 then
         Pipe_pars(next).moverlap := 
            Distance_fcn(Xps(ii),Yps(ii),Xys(mseg*2-1),Xys(mseg*2)); 
      end if;           
      Pipe_pars(next).link_bck := Link_bck(ii);
      Pipe_pars(next).link_fwd := Link_fwd(ii);
--  if mseg >= 0 and mseg<= 702 then
--      dbms_output.put_line('next '|| next || ' seg '|| seg || ' mseg ' || mseg ||' P '||projector || ' AL ' || round(Pipe_pars(next).Length,3)|| ' Seg Len ' || Round(Pipe_pars(next).Seg_Length,6)|| ' Overlap ' || Round(Pipe_pars(next).Overlap,6)); --' Wp ' || round(widths_p(ii),3) || ' Wq ' || round(Widths_q(ii),3));
--      end if;
--        dbms_output.put_line('next '|| next || ' pipe ' || Pipe_nos(ii) || ' seg '|| seg || ' mseg ' || mseg || ' back ' || Link_bck(ii) || ' fwd ' || Link_fwd(ii));
     end if;
  End Loop;
  
--dbms_output.put_line('PIPES ' || next || ' Pipes ' || pipe_no);
  if next > 0 then
     RETURN Pipe_pars;
  else
     RETURN Empty_PP;
  end if;

END FIND_GET_PIPES;
--

PROCEDURE find_adjacency(pInTable VARCHAR2,pInUniqIdColumn VARCHAR2,pInSdoGeomColumn VARCHAR2,pOutTableName VARCHAR2,pdistance NUMBER default 100.,pRowLimit NUMBER DEFAULT 50) AS

--------------------------------------------------------------------------------

-- Program Name: geog_info_uac_find_adjacency
-- Author: Nick Padfield
-- Creation Date: 5/30/2007
--
-- Usage: 
--   Call this program from inside another PL/SQL program.  This program
--   has three required parameters:
--
--     REQUIRED Parameters:
--        pInTable             - Input table containing an SDO_GEOMETRY column
--       pInUniqIdField       - Column name containing a uniqe ID
--       pInSdoGeomColumn     - Column name for the SDO_GEOMETRY to be used
--                              to find adjacencies between polygons
--       pOutTableName  - The name of the output table to be created
--       pdistance      - a maximum distance to find adjacencies
--       pRowLimit      - a row limit to limit the rows processed at one time,
--                        defaults to 50.
--
--Purpose:
--  Create a new table showing the spatial adjacencies between polygons (which
--  other polygons if any each polygon touches. (Note a touch includes sharing
--  a coordinate at a "pie" node.) Example: Polygon 1 touches 2,6,22,23
--
--   UNIQID     ADJ_ID  ADJ_COUNT       FREQ (this column is somewhat redundant)
----------- ---------- ---------- ----------
--        1          2          4          8
--        1          6          4          5
--        1         22          4          4
--        1         23          4          8
--        2          1          8          4
--        2          3          8          5
--        2          6          8          5
--        2          7          8          7
-- .....
--
--Dependencies: none
--
--
-- Modification History:

--------------------------------------------------------------------------------


   TYPE RefCursorType IS REF CURSOR;
   CursorTable RefCursorType;
 
 -- An array of Arrays
 
   TYPE           LIST_ARRAYS is VARRAY(1000) OF MDSYS.SDO_LIST_TYPE;
   MBRZ            LIST_ARRAYS;
   
   InTable         VARCHAR2(30)           := UPPER(pInTable);
   OutTableName    VARCHAR2(30)           := UPPER(pOutTableName);
   InUniqIdColumn  VARCHAR2(30)           := UPPER(pInUniqIdColumn);
   InSdoGeomColumn VARCHAR2(30)           := UPPER(pInSdoGeomColumn);
   sql_stmt        VARCHAR2(4000);

   
   XYs             MDSYS.SDO_ORDINATE_ARRAY;
   Info            MDSYS.SDO_ELEM_INFO_ARRAY;
   Array_UniqId    MDSYS.SDO_LIST_TYPE;
   Array_TmpId     MDSYS.SDO_LIST_TYPE;
   xLLs            MDSYS.SDO_LIST_TYPE;
   yLLs            MDSYS.SDO_LIST_TYPE;
   xURs            MDSYS.SDO_LIST_TYPE;
   yURs            MDSYS.SDO_LIST_TYPE;
   MBRs            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   MBR             MDSYS.SDO_LIST_TYPE;
   Array_SdoGeom   SDO_GEOMETRY_ARRAY;
   TmpCount        NUMBER                  := 0;
   Cur_UniqId      NUMBER;
   Cur_SdoGeom     MDSYS.SDO_GEOMETRY;
   ContinueStatus  BOOLEAN                 := TRUE;
   
   RowLimit        NUMBER                  :=pRowlimit;
   xLL             NUMBER;
   yLL             NUMBER;
   xUR             NUMBER;
   yUR             NUMBER;
   xLL2            NUMBER;
   yLL2            NUMBER;
   xUR2            NUMBER;
   yUR2            NUMBER;
   kount           NUMBER;
   distance        NUMBER := pdistance;
   delta_x         NUMBER;
   delta_y         NUMBER;
   loop_counter    PLS_INTEGER := 0;
   m               PLS_INTEGER;
   mm              PLS_INTEGER;
   next           PLS_INTEGER;
   done            BOOLEAN;
   start_time      TIMESTAMP;

BEGIN
     start_time := current_timestamp;
      ----------------------------------------------------------
      -- Create the empty output table
 
         sql_stmt := 'CREATE TABLE ' || OutTableName ||
         ' (Uniqid VARCHAR2(30),Adj_id NUMBER,Adj_count NUMBER) NOLOGGING';
         EXECUTE IMMEDIATE sql_stmt;
    
      ---------------------------------------------------------
      -- Get an Array of UniqIds

         OPEN CursorTable FOR 'SELECT ' || InUniqIdColumn || ',xLL,yLL,xUR,yUR,'|| InSdoGeomColumn ||' FROM ' || InTable ;


      ---------------------------------------------------------
      LOOP
      
         FETCH CursorTable BULK COLLECT INTO Array_UniqId,xLLs,yLLs,xURs,yURs,Array_SdoGeom LIMIT RowLimit;
         EXIT WHEN Array_UniqId.COUNT = 0 or loop_counter = 1;
          loop_counter := loop_counter + 1;
 
       --   dbms_session.free_unused_user_memory;
 
         FOR i IN Array_UniqId.FIRST..Array_UniqId.LAST LOOP
            Cur_UniqId := Array_UniqId(i);
            Cur_SdoGeom := Array_SdoGeom(i);
            delta_x := round(meters_to_degrees('X',distance,ABS(yLLs(i))),8);
            delta_y := round(meters_to_degrees('Y',distance,ABS(yLLS(i))),8);
            xLL := xLLs(i) - delta_x;
            yLL := yLLs(i) - delta_y;
            xUR := xURs(i) + delta_x;
            yUR := yURs(i) + delta_y;
            
            
-- Check count of nearbys
--sql_stmt := 'SELECT count(1) FROM ' || InSchema || '.' || InTable || ' s'|| 
--               ' WHERE GZ_PIPES.MBRS_OVERLAP(s.xLL,s.yLL,s.xUR,s.yUR,:1,:2,:3,:4) = 1'||
--                           ' AND s.' || InUniqIdColumn || '<>:5 ';
--               EXECUTE IMMEDIATE sql_stmt into kount USING x1,y1,x2,y2,Cur_UniqId;
            
--            if Cur_uniqId = 90517380 then
--                dbms_output.put_line('Kount was ' || kount);
--            end if;
-- This only finds touching relationships and is slower           
--               sql_stmt := 'SELECT /*ORDERED*/ s.' || InUniqIdColumn || ' FROM ' || InSchema || '.' || InTable || 
--               ' s WHERE NOT( (( s.XUR <:1)  OR (s.YUR <:2)) OR  (( s.XLL >:3) OR (s.YLL > :4))) AND '||
--               'SDO_ANYINTERACT(s.' || InSdoGeomColumn || ',:5) = ''TRUE''' || --TODAY
--                           ' AND s.' || InUniqIdColumn || '<>:6 ';
--               EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO Array_TmpId USING x1,y1,x2,y2,Cur_SdoGeom,Cur_UniqId;             

-- Twice as fast !! and finds near touches that we require. 
-- Only finds them for Ids greater than the Subject Id

sql_stmt := 'SELECT  s.' || InUniqIdColumn || ',s.MBRS FROM ' ||  InTable || ' s ' || 
--               ' WHERE GZ_PIPES.MBRS_OVERLAP(s.xLL,s.yLL,s.xUR,s.yUR,:1,:2,:3,:4) = 1 '||
--               ' WHERE NOT( (( s.XUR <:1)  OR (s.YUR <:2)) OR  (( s.XLL >:3) OR (s.YLL > :4)))'||
                 ' WHERE SDO_FILTER(s.'||InSdoGeomColumn||', mdsys.sdo_geometry(2003,8265,NULL,' ||
                        'mdsys.sdo_elem_info_array(1,1003,3),mdsys.sdo_ordinate_array(:1,:2,:3,:4)),''querytype=WINDOW'') = ''TRUE''' ||
                           ' AND s.' || InUniqIdColumn || '>:5';
                EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO Array_TmpId,MBRZ USING xLL,yLL,xUR,yUR,Cur_UniqId;             
  
 
       IF (Array_TmpId.EXISTS(1)) THEN
       
-- Make many MBRs for the Subject Id
        
        MBRs := SET_GEOM_MANY_MBR(Cur_SdoGeom);
        xLL := MBRs(1);
        yLL := MBRs(2);
        xUR := MBRs(3);
        yUR := MBRs(4);
        m := MBRs(5);  -- number of MBRs
        next := 0;
        
-- Now check if there are any coordinates within the MBRs of the Subject Id
-- If any are found, quit the test and accept the 2nd geometry

        For ii in 1..Array_TmpId.count loop
           MBR := MBRZ(ii);
           mm := MBR(5);
           done := FALSE;
--           dbms_output.put_line('ii ' || Cur_UniqId || ' '||Array_TmpId(ii) || ' m ' || m || ' mbrs '|| Mbrs.count);
           For k in 1..m Loop
            xLL := MBRs(k*6+1)-delta_x;
            yLL := MBRs(k*6+2)-delta_y;
            xUR := MBRs(k*6+3)+delta_x;
            yUR := MBRs(k*6+4)+delta_y;
           For j in 1..mm Loop
--    Check for local MBR overlap
            xLL2 := MBR(j*6+1);
            yLL2 := MBR(j*6+2);
            xUR2 := MBR(j*6+3);
            yUR2 := MBR(j*6+4);
            -- Can we verify that there is an intergeometry pipe here?
            If NOT ((( XUR <xLL2)  OR (YUR <yLL2)) OR  (( XLL >xUR2) OR (YLL > yUR2))) then
              next := next + 1;
              Array_TmpId(next) := Array_TmpId(ii);
              done := TRUE;
            end if;
            exit when done;
          End Loop;
          
          exit when done;
          End Loop;
        End Loop;
            ------------------------------------------------------
            -- Do Insert
            if next > 0 then
 
                  Array_TmpId.trim(Array_TmpId.COUNT-next);
                  TmpCount := Array_TmpId.COUNT;
                  FORALL j IN Array_TmpId.FIRST..Array_TmpId.LAST
                     EXECUTE IMMEDIATE 'INSERT /*+ APPEND */ INTO ' || OutTableName ||
                     ' (uniqid,adj_id,adj_count) VALUES (:1,:2,:3)'
                        USING Cur_UniqId,Array_TmpId(j),next;
             end if;
                  TmpCount := 0;
               ELSE -- No adjacent entities were found
                  sql_stmt := 'INSERT /*+ APPEND */ INTO ' || OutTableName ||
                  ' (uniqid,adj_count) VALUES (:1,:2)';
                  EXECUTE IMMEDIATE sql_stmt USING Cur_UniqId,0;
               END IF;
 
            COMMIT;
            
            Array_TmpId.DELETE;
            
            ------------------------------------------------------
         END LOOP;
         Array_SdoGeom.delete;
      END LOOP;
      CLOSE CursorTable;
 dbms_output.put_line((current_timestamp-start_time));

END find_adjacency;
--
FUNCTION GET_APIPE_PAIR(XYs MDSYS.SDO_ORDINATE_ARRAY, Pipe_pars Pipes_Type, pi pls_integer) RETURN MDSYS.SDO_ORDINATE_ARRAY AS

-- Get a portion of a pipe between the projectors

   Pxys       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(); 
   seg        PLS_INTEGER;
   mseg       PLS_INTEGER;
   projector  NUMBER;
BEGIN
        projector := Pipe_pars(pi).projector;    
        seg := Pipe_pars(pi).Seg;
        mseg := Pipe_pars(pi).Mseg;
        if projector =12 or projector = 13 or projector = 42 or projector = 43 then
          Pxys.extend(8);
        end if;

--         4                  3   Here both 1 and 2 project
--         +--------<---------+
--             +---->------+
--             1           2        
        if projector = 12 then
          Pxys(1) := Xys(seg*2-1);
          PXYs(2) := Xys(seg*2);     
          PXYs(3) := Xys(seg*2+1);
          PXYs(4) := Xys(seg*2+2);
 
          PXYs(5) := Pipe_pars(pi).Xq;
          PXYs(6) := Pipe_pars(pi).Yq;
          PXYs(7) := Pipe_pars(pi).Xp;
          PXYs(8) := Pipe_pars(pi).Yp;
          
--         4            3   Here 1 and 3 project
--         +---------<--+
--             +---->------+
--             1           2

        elsif projector = 13 then
          PXYs(1) := Xys(seg*2-1);
          PXYs(2) := Xys(seg*2);
          PXYs(3) := Pipe_pars(pi).Xq;
          PXYs(4) := Pipe_pars(pi).Yq;
 
          PXYs(5) := Xys(mseg*2-1);
          PXYs(6) := Xys(mseg*2);
          PXYs(7) := Pipe_pars(pi).Xp;
          PXYs(8) := Pipe_pars(pi).YP;
 
-- Do matching (parallel) segments
--                4      3   Here both 4 and 3 project
--                +---<--+
--             +---->------+
--             1           2
        elsif projector = 43 then
          PXYs(1) := Pipe_pars(pi).Xp;
          PXYs(2) := Pipe_pars(pi).YP;
          PXYs(3) := Pipe_pars(pi).Xq;
          PXYs(4) := Pipe_pars(pi).Yq;
          PXYs(5) := Xys(mseg*2-1);
          PXYs(6) := Xys(mseg*2);      
          PXYs(7) := Xys(mseg*2+1);
          PXYs(8) := Xys(mseg*2+2);
          
--                4          3   here 4 projects and 2 also
--                +---<-------+
--             +---->------+
--             1           2
        elsif projector = 42 then
          PXYs(1) := Pipe_pars(pi).Xp;
          PXYs(2) := Pipe_pars(pi).Yp; 
          PXYs(3) := Xys(seg*2+1);
          PXYs(4) := Xys(seg*2+2);
          PXYs(5) := Pipe_pars(pi).Xq;
          PXYs(6) := Pipe_pars(pi).Yq; 
          PXYs(7) := Xys(mseg*2+1);
          PXYs(8) := Xys(mseg*2+2);
        end if;
        
        RETURN Pxys;
END GET_APIPE_PAIR;
--
FUNCTION GET_PIPE_ENDS(XYs MDSYS.SDO_ORDINATE_ARRAY, Pipe_pars Pipes_Type) RETURN MDSYS.SDO_ORDINATE_ARRAY AS

-- Make a pipe ends into 1,2,3,4 order
--                                                   4   <----  3
--    in other words from 1 to 2 and then 3 to 4     1   ---->  2   
--                   

   VXYs MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   PXYs MDSYS.SDO_ORDINATE_ARRAY;
   next      PLS_INTEGER := 0;
   projector  NUMBER;
   
BEGIN

    For ii in 1..Pipe_pars.count Loop
           
        projector := Pipe_pars(ii).projector;            
        Vxys.extend(8);
 
        Pxys := Get_APipe_Pair(Xys,Pipe_Pars,ii);
        if projector >= 12 and PXys.count > 0 then
           for jj in 1..8 loop
              Vxys(next+jj) := Pxys(jj);
           end loop;
        end if;
        next := next+8;
    End Loop;
    
    Return VXys;
END;
--
Function Find_it_inseg(vertex pls_integer,Pipe_pars IN OUT NOCOPY Pipes_Type,first varchar2 default 'Y') return pls_integer as

-- Search the Pipe parameter object for a particular Segment number

Begin
  IF first = 'Y' then
  For ij in 1..Pipe_pars.count loop
   if Pipe_pars(ij).Seg = vertex then
     Return ij;
   end if;
  end loop;
  ELSE
  For ij in reverse 1..Pipe_pars.count loop
   if Pipe_pars(ij).Seg = vertex then
     Return ij;
   end if;
  end loop;
  
  END IF;
Return 0;
end;
--
Function Find_it_inMseg(vertex pls_integer,Pipe_pars IN OUT NOCOPY Pipes_Type,first  varchar2 default 'Y') return pls_integer as

-- Search the Pipe parameter object for a particular Matching Segment number

Begin
   IF first = 'N' then
  For ij in 1..Pipe_pars.count loop
   if Pipe_pars(ij).MSeg = vertex then
     Return ij;
   end if;
  end loop;
  ELSE
  For ij in Reverse 1..Pipe_pars.count loop
   if Pipe_pars(ij).MSeg = vertex then
     Return ij;
   end if;
  end loop;
  END IF;
Return 0;
end;
--
FUNCTION CHECK_OPENING(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
               Info IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
               Pipe_pars IN OUT NOCOPY Pipes_Type,
               x NUMBER,y NUMBER, pseg PLS_INTEGER, pmseg PLS_INTEGER,
               on_pseg  VARCHAR2 default 'Y', way VARCHAR2,debug_it VARCHAR2 default 'FALSE') RETURN NUMBER AS

-- Check for an opening starting at x,y on pseg or pmseg (depends on on_pseg='Y' or 'N')
-- Knowing the Xys from the geometry and start and end of the ring for 
-- segments (sstart and slast) and for the matching segments (mstart and mlast).
  
  deg2rad   CONSTANT NUMBER :=0.0174532925199432957692369076848861271344;  -- pi/180.
    angle    NUMBER;
    xs       NUMBER;  -- start of sseg
    ys       NUMBER;
    xm       NUMBER;  -- start of mseg
    ym       NUMBER;
    xn       NUMBER;  -- end of sseg, start of rseg
    yn       NUMBER;
    xr       NUMBER;  -- end of mseg, start of nseg
    yr       NUMBER;
    x1       NUMBER;
    y1       NUMBER;
    x2       NUMBER;
    y2       NUMBER;
    xway     NUMBER;  -- coordinates of a way point
    yway     NUMBER;
    
    d        NUMBER;
    dist     NUMBER :=0.;
    distlast NUMBER;
    dist_sav NUMBER;
    dist_travelled NUMBER;
    lenrs    NUMBER;
    lenmn    NUMBER;
    factor   NUMBER := 2.;
    r        NUMBER;  -- line parameter from s -> r
    t        NUMBER;  -- line parameter from m -> n
    df       NUMBER := 0.2; --fraction along each segment
    tr       NUMBER := 1.;  -- ratio of
    tm       NUMBER := 1.;  -- ratio of
    width    NUMBER := 1000000.;
    hypot    NUMBER;
    open_up  NUMBER := 1.;
    xc       NUMBER;
    yc       NUMBER;
    len_chosen  NUMBER;
    ratio    NUMBER;
    
    inside   PLS_INTEGER;
    sstart   PLS_INTEGER;
    send     PLS_INTEGER;
    mstart   PLS_INTEGER;
    mend     PLS_INTEGER;
    loops    PLS_INTEGER :=0;
    loop_max PLS_INTEGER ;
    sseg     PLS_INTEGER := pseg;
    mseg     PLS_INTEGER := pmseg;
    rseg     PLS_INTEGER ;
    nseg     PLS_INTEGER ;
    ring_type PLS_INTEGER;
    kount    PLS_INTEGER;
    total_kount PLS_INTEGER :=0;
    loop2max PLS_INTEGER :=5;
    loop_count PLS_INTEGER;
    
    on_s     VARCHAR2(1) := on_pseg;
    
BEGIN 


-- Start from a point x,y on one of the segments and measure the distance
-- across as we procede into the opening. The distances, d< d1 < d2 must increase!
-- On pic + are vertices and * is the perpendicular point.
-- 
--    on_s is 'N' here                          on_s is 'B' here
--
--                       /
--    <- towards pipe   / nseg                  nseg   /  ^
--      mseg     (x,y) /                              /   |    |
--        <-----*-----+                         -----+    |    |
--                                                   d   d1    d2
--       --->---+                               -----+    |    |
--        sseg   \ rseg                               \   |    |
--                \                              rseg  \  v


-- Note that segment numbers also describe vertices.  So segment 100 describes
-- vertex 100 and the end of the segment is vertex 101, the start of the next segment

    loop_max := ABS(sseg-mseg)-1;
    if loop_max >10 then loop_max := 10; end if;

-- Find the start and end of each ring that the subject seg and matching seg fall in

    find_info(sseg*2-1,Info,Xys.count,sstart,send,ring_type);
    find_info(mseg*2-1,Info,Xys.count,mstart,mend,ring_type);
    
-- Setup to iterate in case the segments are very short. Note that as we proceed
-- (using the pic on the right) if rseg is shorter than nseg that we will
-- have to generate a way point on nseg when we have reached the end of rseg
-- (and vice versa);
      if debug_it = 'TRUE' then
        
        dbms_output.put_line('x ' || round(x,7) || ',' || round(y,7));
        dbms_output.put_line('LOOP BEGIN <<<<<<<<<<'||way ||' ' ||on_s  || ' rseg ' || rseg || ',' ||nseg);
      end if;
      xway := x;
      yway := y;
      dist := 0.0;
      distlast := 0.0;
      dist_travelled := 0.0;
      loops := 0;
--    dbms_output.put_line('LOOP BEGIN <<<<<<<<<<'); 
-- 
    WHILE dist < width* factor and loops < loop_max LOOP
    
      loops := loops+1;
-- dbms_output.put_line('loops ' || loops || ' loop max ' || loop_max || ' width ' || width || ' factor ' || factor);
-- want to move to the next vertex to describe the end of the segment

       If on_s='Y' then
        xs := xway;
        ys := yway;
       End if;

       if on_s='N' then
        xm := xway;
        ym := yway;
       End if;
--        dbms_output.put_line('xs ' || round(xs,7) || ' ys ' || round(ys,7));
--        dbms_output.put_line('xm ' || round(xm,7) || ' ym ' || round(ym,7));
--     Go to next vertex

        
        
-- Look for a large opening
--                  (xn,yn)          (xm,ym)     <---- mseg increases
--                  +-----------------+-----------+
--    away from pipe <------             towards pipe -->
--                           (xs,ys)  +--------------+
--                                    |           ---> seg increases
--                                    |
--                                    |
--                                    + (xr,yr)
 
-- Go LEFT -- This is a so-called end of an opening

       If way = 'LEFT' then  
 
          rseg := sseg-1;
          if rseg = 0 then rseg := send-1; end if;
  
        if loops = 1 then
           mseg := mseg+1;
          if mseg >= mend then mseg := mstart; end if;
        end if;
        if on_s = 'N' then
          nseg := mseg;
        else
        nseg := mseg+1;
        if nseg >= mend then nseg := mstart; end if;
        dbms_output.put_line('loop top ' ||sseg || ' mseg ' || mseg || ' nseg ' || nseg || ' mstart ' || mstart|| ' mend ' || mend);
        end if;
        

        -- Look for a large opening                  /
--    <--mseg increases  (xm,ym)    (xn,yn) / 
--                       +-----------------+
--          towards pipe <------             away from pipe -->
--                       +--------------+ (xs,ys)
--                  ---> seg increases  |           
--                                      |
--                                      |
--                                       + (xr,yr)

-- GO RIGHT -- This is a so-called start
      else
      
       
        if on_s ='Y' or loops = 1 then
          nseg := mseg-1;
          if nseg =0 then nseg := mend-1; end if;
        end if;
        if loops = 1 then 
         sseg := sseg +1;
         if sseg >= send then sseg := sstart; end if;
        end if;
        if on_s = 'Y' then
          rseg := sseg;
        else
        rseg :=sseg+1;
        if rseg >=send then rseg := sstart; end if;
        end if; 
                         
      End if;

        if on_s = 'Y' or on_s = 'B' then
          xm := Xys(mseg*2-1);
          ym := Xys(mseg*2);
        end if;
         
       
       if on_s='N' or on_s = 'B' then
        xs := Xys(sseg*2-1);
        ys := Xys(sseg*2);
       end if;

-- If both of the attached segments are pipes, return zero

       if loops=1 and (find_it_inseg(sseg,Pipe_pars) <> 0 or find_it_inmseg(sseg,Pipe_pars) <> 0)
       and (find_it_inseg(mseg,Pipe_pars) <> 0 or find_it_inmseg(mseg,Pipe_pars) <> 0) then      
        dbms_output.put_line('returning zero!!!' || rseg || ' nseg ' || nseg);
         Return 0;
      end if;

--    "Tunnel" or Funnel opening where wider parts are not a pipe
--
--             |         |
--             |         |
--             |         |
--             |         |
--              \       /
--               |     |  pipe
--               |     |

      if loops>1 and find_it_inseg(rseg,Pipe_pars) = 0 and find_it_inmseg(nseg,Pipe_pars) = 0 
      and find_it_inseg(nseg,Pipe_pars) = 0 then      
        dbms_output.put_line('returning !!!' || rseg || ' nseg ' || nseg);
         Return open_up;
      end if;
-- These are the next vertices on the segments

      xr := Xys(rseg*2-1);
      yr := Xys(rseg*2);
      xn := Xys(nseg*2-1);
      yn := Xys(nseg*2);
   

      
      if debug_it = 'TRUE' then
      dbms_output.put_line('sseg ' || sseg || ' rseg ' || rseg);
     dbms_output.put_line('mseg ' || mseg || ' nseg ' || nseg);  
     dbms_output.put_line('xs ' || round(xs,7) || ',' || round(ys,7) || ' xr ' || round(xr,7) || ',' ||round(yr,7)); 
      dbms_output.put_line('xm ' || round(xm,7) || ',' || round(ym,7) || ' xn ' || round(xn,7) || ',' ||round(yn,7)); 
      end if;
  -- To march along each segment we want to ensure they are similar lengths
  
      lenrs := GZ_PIPES.distance_fcn(xr,yr,xs,ys);
      lenmn := GZ_PIPES.distance_fcn(xm,ym,xn,yn);
      
--      dbms_output.put_line('xs ' || round(xs,7) || ',' || round(ys,7) || ' xr ' || round(xr,7) || ',' ||round(yr,7)); 
--      dbms_output.put_line('xm ' || round(xm,7) || ',' || round(ym,7) || ' xn ' || round(xn,7) || ',' ||round(yn,7)); 
--      dbms_output.put_line('sseg ' || sseg || ' rseg ' || rseg || ' mseg '||mseg || ' nseg ' || nseg);
--      dbms_output.put_line('RS ' || round(lenrs,3) || ' MN ' || round(lenmn,3));

-- Since in general they will be different, we need to remember the way point
-- for the next iteration and whether we did not reach the next vertex on_s

      if lenrs > lenmn then
         tr := lenmn/lenrs;   -- ratio to keep distance proportional
         tm := 1.;
         len_chosen := lenmn;
      else
         tm := lenrs/lenmn;
         tr := 1.;
         len_chosen := lenrs;
      end if;
      
       distlast := GZ_PIPES.distance_fcn(xs,ys,xm,ym);
      if loops = 1 then
         width := distlast;
      end if; 
      
      -- Move if possible at least width/2 per iteration
      
      loop_count := round(2.*len_chosen/width);
      if loop_count <= 1 then
         loop_count := 1;
          ratio := 1.0;
      elsif loop_count > 5 then
         ratio := 0.2;
         loop_count := 5;
      else
         ratio := 1./loop_count;
      end if;
      dist_travelled := dist_travelled + loop_count *width*0.5;
      
-- Picture at the mouth is desired to have the hypotenuse at 45 degrees
-- Using the cosine rule we get c = sqrt(a^2 + b^2 - 2ab*cos(angle))

-- We will generate hypotenuses from sides d, w+d
--                                         d,  w+2d etc
--
-- So the cosine rule is hypotenuse = sqrt(d^2 + (w+d)^2 - 2d(w+d)cos(angle))
--           +
--         ..
--          d|
--           |
--          d|
--           |            d   d    d    d    d
--           *    W    +---.----.----.----.----+
--           |         |   .2   .4   .6   .8   1
--           |
      angle := angle_in_degrees(xs,ys,xr,yr,xm,ym,xn,yn);
      
      hypot := sqrt( d*d + (width + d)*(width+d) - 2*d*(width+d)*cos(angle*deg2rad)) *0.99;
--      hypot := sqrt(2. - 2.*cos(angle*deg2rad)) *0.99;
      
      
     dbms_output.put_line('hypot ' || round(hypot,6) );
-- This is the distance across we want to compare


      if debug_it = 'TRUE' then
    dbms_output.put_line('Width pipe ' || round(distlast,3) || ' sseg ' || sseg || ' mseg ' || mseg);
end if;
      kount :=0;
      for ii in 1..loop_count loop
        r := ii*ratio*tr;
        t := ii*ratio*tm;              -- multiply to space evenly away from confluence
        x1 := xr*r + xs*(1.-r);
        y1 := yr*r + ys*(1.-r);
        x2 := xn*t + xm*(1.-t);
        y2 := yn*t + ym*(1.-t);
        dist_sav := distlast;
        dist := GZ_PIPES.distance_fcn(x1,y1,x2,y2);
        if dist > distlast then
           kount := kount+1;
           dbms_output.put_line('Kount ' || kount || ' ' || loop_count || ' dist ' || dist);
           distlast := dist; --*1.1;
        else
          dbms_output.put_line('fail ' || round(dist,3) || ',' || round(distlast,3) || ' ratio ' || ratio || ' LC ' || loop_count);
        dbms_output.put_line('x1 ' || round(x1,7) || ',' || round(y1,7));
        dbms_output.put_line('x2 ' || round(x2,7) || ',' || round(y2,7));
        end if;
        if debug_it = 'TRUE' then
        dbms_output.put_line('r ' || round(r,5) || ' t ' || round(t,5));
        dbms_output.put_line('xs ' || round(xs,7) || ',' || round(ys,7));
        dbms_output.put_line('xr ' || round(xr,7) || ',' || round(yr,7));
        dbms_output.put_line('xm ' || round(xm,7) || ',' || round(ym,7));
        dbms_output.put_line('xn ' || round(xn,7) || ',' || round(yn,7));
        dbms_output.put_line('D ' || round(dist,3) || ' K ' || kount || ' r ' || round(r,6) || ' t ' || round(t,6));
--        dbms_output.put_line('x ' || round(x1,7) || ',' || round(y1,7) || ' x ' || round(x2,7) || ',' ||round(y2,7)); 
--        dbms_output.put_line('yn ' || round(yn,7) || ' ym ' || round(ym,7)); 
         end if;
-- Ensure we are not at a corner and we are going outside the polygon

       if dist > 1.4* width or ii = loop_count then
       
          xc := x1*0.5 + x2*0.5;
          yc := y1*0.5 + y2*0.5;
--          if debug_it = 'TRUE' then
          dbms_output.put_line('x1 ' || round(x1,7) || ',' || round(y1,7));
          dbms_output.put_line('xc ' || round(xc,7) || ',' || round(yc,7));
          dbms_output.put_line('x2 ' || round(x2,7) || ',' || round(y2,7));
--          end if;
          inside := POINT_IN_POLY(xc,yc,Xys,Info);
          if inside = 0 then
             kount := 0;
             dbms_output.put_line('Kount reset to zero');
             exit;
          end if;
        end if;
       
      end loop;
      total_kount := total_kount + kount;
         if kount < loop_count then
-- Require at least 3 increases out of five
--      if kount < loop2max then -- and dist_travelled > 4.* width) then --or (total_kount < 3 and loops = 2) then
         open_up := 0.0;
         dbms_output.put_line('CCLOSED ' || round(distlast,3) || ' kount ' || kount );
         exit;
--      elsif kount > 3 then
--         exit;
      end if;

--   if the length on rs was greater than on mn then we have a way point on sseg     
--   Setup this way point

    if lenrs > lenmn then   -- used up the M segment
       xway := x1;
       yway := y1;
       on_s := 'Y';
       mseg := nseg;
    elsif lenrs < lenmn then  -- used up the S segment
       xway := x2;
       yway := y2;
       on_s := 'N';
       sseg := rseg;
    else                -- segments same length
       xway := x1;
       yway := y1;
       on_s := 'B';   -- both are in sync but we assign the way point to sseg
       mseg := nseg;   
       sseg := rseg;
    end if;
    
--    exit when dist > 2.*width; --total_kount > 4;
   dbms_output.put_line('loop bottom ' || loops || ' loopmax ' || loop_max||' dist ' || round(dist,3) || ' WF  ' ||round(width*factor,3) || ' W ' || round(width,3) || ' K ' || total_kount);
  dbms_output.put_line('loop bottom ' ||sseg || ' mseg ' || mseg || ' nseg ' || nseg || ' DT ' || dist_travelled);
  END LOOP;
  dbms_output.put_line('open up '||open_up);
     if total_kount = 0 then -- or dist < 1.4*width  then
         open_up := 0.0;
         dbms_output.put_line('CLOSEDD ' || round(distlast,3));    
      end if;
    
     RETURN open_up;  
END CHECK_OPENING;
--
FUNCTION FIND_OPENINGS(Geom MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., pminimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_LIST_TYPE AS

    XYs      MDSYS.SDO_ORDINATE_ARRAY := Geom.sdo_ordinates;
    Info     MDSYS.SDO_ELEM_INFO_ARRAY := Geom.Sdo_Elem_Info;
    PXYs     MDSYS.SDO_ORDINATE_ARRAY;
    Openings MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Used_Segs MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Pipe_pars GZ_PIPES.Pipes_Type;
    sseg     NUMBER;
    mseg     NUMBER;  -- PLUS 1 !!
    rseg     NUMBER ;
    nseg     NUMBER ;
    perim    NUMBER;
    area     NUMBER;
    ap       NUMBER;
    dist     NUMBER :=0.;
    distlast NUMBER;
    dist_travelled NUMBER;
    lenrs    NUMBER;
    lenmn    NUMBER;
    len_chosen  NUMBER;
    minimum_len NUMBER := pminimum_len;
    projector NUMBER;
    ratio     NUMBER;
    xs       NUMBER;
    ys       NUMBER;
    xm       NUMBER;
    ym       NUMBER;
    xn       NUMBER;
    yn       NUMBER;
    xr       NUMBER;
    yr       NUMBER;
    x1       NUMBER;
    y1       NUMBER;
    x2       NUMBER;
    y2       NUMBER;
    width    NUMBER := max_width;
    factor   NUMBER := 2.;    -- an opening tolerance 
    r        NUMBER;  -- line parameter from s -> r
    t        NUMBER;  -- line parameter from m -> n
    tr       NUMBER := 1.;  -- ratio of
    tm       NUMBER := 1.;  -- ratio of
    kount    PLS_INTEGER := 0;
    total_kount PLS_INTEGER;
    loops    PLS_INTEGER := 0;
    loop_max PLS_INTEGER;
    loop_count  PLS_INTEGER;
    n        PLS_INTEGER;
    next    PLS_INTEGER;
    last_valu PLS_INTEGER;
    plast   PLS_INTEGER;
    pos       PLS_INTEGER;
    pseg      PLS_INTEGER;
    last_next PLS_INTEGER := 0;
    next_xy  PLS_INTEGER;
    open_kount PLS_INTEGER :=0;
    Info_count pls_integer;
    no_of_rings PLS_INTEGER;
    last_ring_vtx  PLS_INTEGER;
    m        PLS_INTEGER;
    istart   PLS_INTEGER;
    last     PLS_INTEGER;
    way       VARCHAR2(5);
    
    open_up  BOOLEAN;
 
    Function Findit(find_seg pls_integer) return pls_integer as
    
--  Find Pipe # for a particular segment searching both the Subject Segments 
--  and the Matching segments in Pipe parameters

    Begin
        For pp in 1..Pipe_pars.count loop
           if Pipe_pars(pp).seg = find_seg or Pipe_pars(pp).mseg = find_seg then
              Return Pipe_pars(pp).pipe_no;
           end if;
        End loop;
  
        Return 0; -- no find
    End;
    
    Function measure(seg1 pls_integer, seg2 pls_integer)  return pls_integer as
        seg_count   pls_integer;
    Begin
        seg_count := ABS(seg2 - (seg1+last-1));
        return seg_count;
    End;
    
    Function Traverse(pp pls_integer) return pls_integer as
    
--  Traverse pipe and find last reference when it ends

      pipe_no  number := Pipe_Pars(pp).pipe_no;
      pi       pls_integer := pp;
      
    Begin
       For pi in pp..Pipe_pars.count loop
          if pipe_no = Pipe_pars(pi).pipe_no and Pipe_pars(pi).link_fwd=0 then
             return pi;
          end if;
       End Loop;
       dbms_output.put_line('>>>fail return ' || pp );
       return pp;
    End;
    
BEGIN

    If minimum_len = 0.0 then
     minimum_len := max_width*0.5;
    End If;
    Xys := Geom.sdo_ordinates;
    Info := Geom.Sdo_Elem_Info;
    Info_count := TRUNC(info.count/3);
    n   := TRUNC(XYs.count/2);
    
-- Used Segments cannot be reused

    Used_Segs.extend(n);
    for ii in 1..n loop
       Used_Segs(ii) := 0.0;
    end loop;

-- Look for pipes

    Pipe_Pars := GZ_PIPES.Find_Get_Pipes(Geom,pipe_type,max_width,minimum_len,min_aspect_ratio,max_angle_diff);

-- If there are none, then return the original geometry unchanged

    IF Pipe_pars.count=0 THEN
       RETURN NULL;
    END IF;
dbms_output.put_line('pipe pars ' || pipe_pars.count);
    
-- Get the Pipe end coordinates
    PXys := GZ_PIPES.Get_Pipe_Ends(Xys,Pipe_pars);
    Openings.extend(Pipe_pars.count);

-- Look for openings, places where the segments diverge

    FOR p in 1..Pipe_pars.count LOOP
    
    Openings(p) := 0.0;
    total_kount :=0;
    sseg := Pipe_pars(p).seg;
    mseg := Pipe_pars(p).mseg;
    loop_max := ABS(sseg-mseg)-1;
    if loop_max >5 then loop_max := 5; end if;
--    dbms_output.put_line('TRYING ' ||sseg || ' with ' || mseg || ' Loop max ' || loop_max);
--    Last_info(sseg,istart,Info,Xys.count);
--    dbms_output.put_line('last ' || last || ' measure ' || measure(2,17));
--    Last_info(mseg*2-1,istart,Info,Xys.count);


--  Used_segs:   zero - unused
--               1      used
--              -1     unused but start of pipe so may be used as part of a polygon
--               2      its a pipe segment

    If (Pipe_pars(p).link_bck = 0 or Pipe_pars(p).link_fwd = 0) then 
      Used_segs(sseg) := -1;
      Used_segs(mseg) := -1;
    End if;
    --  Mark the Pipe Segments with a 2
    If (Pipe_pars(p).link_bck <> 0 and Pipe_pars(p).link_fwd <> 0) then
      if Used_segs(sseg) = 0.0 then
      Used_segs(sseg) := 2;
--      dbms_output.put_line('Marked ' || sseg);
      end if;
      if Used_segs(mseg) = 0.0 then
      Used_segs(mseg) := 2;
--      dbms_output.put_line('marked ' || mseg);
      end if;
    End if;
    open_up := FALSE;

--  Possibility of a opening up
    
    If (Pipe_pars(p).link_bck = 0 or Pipe_pars(p).link_fwd = 0) then     
       open_up := TRUE;
--       dbms_output.put_line('PP ' || p || ' Findit ' || findit(nseg) || ' findit ' || findit(rseg));
    End if;
     
   -- Sometimes we have 4 pipes coming together. Ignore this, it is not an opening
    nseg := mseg+1;
    if nseg >= last then
       nseg := istart;
    end if;
    rseg := sseg-1;
    if rseg = 0 then rseg := last-1; end if;
      if Pipe_pars(p).link_bck = 0 and Findit(nseg) <> 0 and Findit(rseg) <> 0 and
                                        Findit(nseg) <> Pipe_pars(p).pipe_no and
                                        Findit(rseg) <> Pipe_pars(p).pipe_no then
         open_up := FALSE;
      end if;
      
    nseg := mseg-1;
    if nseg =0 then
      nseg := last-1;
    end if;
    rseg := sseg+1;
    if rseg >last then rseg := istart; end if;
      if Pipe_pars(p).link_fwd = 0 and Findit(nseg) <> 0 and Findit(rseg) <> 0 and
                                        Findit(nseg) <> Pipe_pars(p).pipe_no and
                                        Findit(rseg) <>  Pipe_pars(p).pipe_no then
--       dbms_output.put_line('pp ' || p || ' Findit ' || findit(nseg) || ' findit ' || findit(rseg));
         open_up := FALSE;
      end if;
   
-- End of a peninsula
--      if loop_max < 2 and Pipe_pars(p).link_fwd = 0 then
--         open_up := FALSE;
--      end if;
-- End of a peninsula near the the beginning of the face      
      plast := Traverse(p);
      if Measure(sseg,mseg) < 3 and Measure(Pipe_pars(plast).seg,Pipe_pars(plast).mseg) > Measure(sseg,mseg) then
        open_up := FALSE;
      end if;
      
      
      last := Last_info(mseg*2-1,istart,Info,Xys.count);
      last_ring_vtx := last;
      if close_measure(sseg,mseg,Info,Xys.count) < 4 then
         open_up := FALSE;
      end if;
--      dbms_output.put_line('CLOSE ' || close_measure(sseg,mseg,last_ring_vtx) || ' ' || sseg || ' ' || mseg);
-- Check if this is the start of a pipe then see if it opens up

      If open_up then
         
        projector := Pipe_pars(p).projector;
        If Pipe_pars(p).link_bck = 0 then
         way := 'LEFT';
         dbms_output.put_line('OPEN up to left is a possibility ' || Pipe_pars(p).seg||'-' || ' and ' || Pipe_pars(p).mseg||'+');
        
-- Look for a large opening
--                  (xn,yn)          (xm,ym)     <---- mseg increases
--                  +-----------------+-----------+
--    away from pipe <------             towards pipe -->
--                           (xs,ys)  +--------------+
--                                    |           ---> seg increases
--                                    |
--                                    |
--                                    + (xr,yr)
 
          mseg := mseg+1;
         if mseg >= last then
            mseg := istart;
          end if;
 
        Else
          way := 'RIGHT';
         dbms_output.put_line('OPEN UP to right is a possibility ' || Pipe_pars(p).seg || '+' || Pipe_pars(p).mseg||'-');
-- Look for a large opening                  /
--    <--mseg increases  (xm,ym)    (xn,yn) / 
--                       +-----------------+
--          towards pipe <------             away from pipe -->
--                       +--------------+ (xs,ys)
--                  ---> seg increases  |           
--                                      |
--                                      |
--                                       + (xr,yr)

 
            mseg := mseg-1;
            if mseg < istart then
              mseg := last;
            end if;
            sseg := sseg+1;
            if sseg >= last then
              sseg := istart;
            end if;
        End if;
      


-- Setup to iterate in case the segments are very short
      rseg := sseg;
      nseg := mseg;
      dist := 0.;
      distlast := 0.;
      dist_travelled := 0.;
      loops := 0;
-- dbms_output.put_line('loops ' || loops || ' loop max ' || loop_max || ' width ' || width || ' factor ' || factor);
    WHILE dist < width* factor and loops < loop_max LOOP
    
      loops := loops+1;
      
      sseg := rseg;
      mseg := nseg;
      next := (p-1)*8;
      
-- Get the actual pipe ends to begin
----           (7,8)+----------+ (5,6)
--             (1,2)+----------+ (3,4)
/*      
      if loops = 1 and way = 'LEFT'then 
          xs := Pxys(next+1);
          ys := Pxys(next+2);
          xm := Pxys(next+7);
          ym := PXys(next+8);
--        Set up previous vertex which is going to be decremented below
          rseg := sseg;
          if xs = Xys(rseg*2-1) and ys = Xys(rseg*2) then
             NULL;   -- start of pipe matches sseg, cases 12 and 13
          else
             rseg := rseg+1;   -- cases 43 and 43
          end if;
--        Set up next vertex which is going to be incremented below
          nseg := mseg+1;
          if xm = Xys(nseg*2-1) and ym = Xys(nseg*2) then
             NULL;   -- incremented below
          else
             nseg := nseg-1;
          end if;
      elsif loops = 1 and way = 'RIGHT' then
          xs := Pxys(next+3);
          ys := Pxys(next+4);
          xm := Pxys(next+5);
          ym := PXys(next+6);
--        Set up next vertex which is going to be incremented below
          rseg := sseg+1;
          if xs = Xys(rseg*2-1) and ys = Xys(rseg*2) then
             NULL;    -- end of pipe matches vertex sseg+1
          else
             rseg := rseg -1;
          end if;
--        Set up previous vertex which is going to be decremented below          
          nseg := mseg;
          if xm = Xys(nseg*2-1) and ym = Xys(nseg*2) then
             NULL;   -- end of pipe matches mseg
          else
             nseg := nseg + 1;
          end if;
      else
      */
        xs := Xys(sseg*2-1);
        ys := Xys(sseg*2);
        xm := Xys(mseg*2-1);
        ym := Xys(mseg*2);
--        dbms_output.put_line('xs ' || round(xs,7) || ' ys ' || round(ys,7));
--        dbms_output.put_line('xm ' || round(xm,7) || ' ym ' || round(ym,7));
--      end if;
-- Go LEFT
       If way = 'LEFT' then
        rseg := rseg-1;
        if rseg = 0 then rseg := last-1; end if;
        nseg := nseg+1;
        if nseg > last then nseg := istart+1; end if;
      else
-- GO RIGHT
        nseg := nseg-1;
        if nseg =0 then
          nseg := last-1;
        end if;
        rseg := rseg+1;
        if rseg >last then rseg := istart; end if;      
      End if;

      xr := Xys(rseg*2-1);
      yr := Xys(rseg*2);
      xn := Xys(nseg*2-1);
      yn := Xys(nseg*2);
      
      dbms_output.put_line('sseg ' || sseg || ' rseg ' || rseg || ' projector ' || projector);
      dbms_output.put_line('mseg ' || mseg || ' nseg ' || nseg);
      
  -- To march along each segment we want to ensure they are similar lengths
      lenrs := GZ_PIPES.distance_fcn(xr,yr,xs,ys);
      lenmn := GZ_PIPES.distance_fcn(xm,ym,xn,yn);
      
--      dbms_output.put_line('sseg ' || sseg || ' rseg ' || rseg || ' mseg '||mseg || ' nseg ' || nseg);
--      dbms_output.put_line('RS ' || round(lenrs,3) || ' MN ' || round(lenmn,3));
      if lenrs > lenmn then
         tm := lenmn/lenrs;   -- ratio to keep distance proportional
         tr := 1.;
         len_chosen := lenmn;         
      else
         tr := lenrs/lenmn;
         tm := 1.;
         len_chosen := lenrs;
      end if;
-- Move if possible at least width/2 per iteration
      
      loop_count := round(2.*len_chosen/width);
      if loop_count <= 1 then
         loop_count := 1;
          ratio := 1.;
      elsif loop_count > 5 then
         ratio := .2;
         loop_count := 5;
      else
         ratio := 1./loop_count;
      end if;
      dist_travelled := dist_travelled + loop_count *width*0.5;
  --    dbms_output.put_line('tm ' || round(tm,6) || ' tr ' || round(tr,6));
      distlast := GZ_PIPES.distance_fcn(xs,ys,xm,ym);
--    dbms_output.put_line('Width pipe ' || round(distlast,3));
      kount :=0;
      for ii in 1..loop_count loop
        r := ii*ratio*tr;
        t := ii*ratio*tm;              -- multiply to space evenly away from confluence
        x1 := xr*r + xs*(1.-r);
        y1 := yr*r + ys*(1.-r);
        x2 := xn*t + xm*(1.-t);
        y2 := yn*t + ym*(1.-t);
        dist := GZ_PIPES.distance_fcn(x1,y1,x2,y2);
        if dist > distlast then
           kount := kount+1;
           distlast := dist; --*1.1;
        end if;
        dbms_output.put_line('D ' || round(dist,3) || ' K ' || kount || ' r ' || round(r,6) || ' t ' || round(t,6));
        dbms_output.put_line('x ' || round(x1,7) || ' y ' || round(y1,7) || ' x ' || round(x2,7) || ' y ' ||round(y2,7)); 
        dbms_output.put_line('yn ' || round(yn,7) || ' ym ' || round(ym,7)); 
      end loop;
      total_kount := total_kount + kount;
-- Require at least 3 increases out of five
      if (kount < 3 and dist_travelled > 2.* width) then --or (total_kount < 3 and loops = 2) then
         open_up := FALSE;
         dbms_output.put_line('CLOSED ' || p || ' sseg ' || Pipe_pars(p).seg || ' mseg ' || Pipe_pars(p).mseg);
         exit;
--      elsif kount >= 3 then
--         exit;
      end if;
      
   
  END LOOP;
     if total_kount = 0 then --distlast < 2.*width  then
         open_up := FALSE;
         dbms_output.put_line('CLOSEDD ' || p || ' sseg ' || Pipe_pars(p).seg || ' mseg ' || Pipe_pars(p).mseg);    
      end if;
     if open_up then
        open_kount := open_kount + 1;
        Openings(p) := 1.0;
        dbms_output.put_line('OPEN ' || p || ' sseg ' || Pipe_pars(p).seg || ' mseg ' || Pipe_pars(p).mseg);
     end if;
    END IF;
  END LOOP;
  dbms_output.put_line('OPENINGS ' || open_kount);
END FIND_OPENINGS;
--
FUNCTION REMOVE_PIPES(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., pminimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY  AS

    Xys      MDSYS.SDO_ORDINATE_ARRAY;
    PXYs     MDSYS.SDO_ORDINATE_ARRAY;
    Ring_XYs MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Out_Xys  MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Info     MDSYS.SDO_ELEM_INFO_ARRAY;
    Out_Info MDSYS.SDO_ELEM_INFO_ARRAY :=MDSYS.SDO_ELEM_INFO_ARRAY();
    Openings MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Used_Segs MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Areas    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Out_Geom MDSYS.SDO_GEOMETRY := Geom;
    Pipe_pars GZ_PIPES.Pipes_Type;
    sseg     NUMBER;
    mseg     NUMBER;  -- PLUS 1 !!
    rseg     NUMBER ;
    nseg     NUMBER ;
    perim    NUMBER;
    area     NUMBER;
    ap       NUMBER;
    dist     NUMBER :=0.;
    distlast NUMBER;
    dist_travelled NUMBER;
    lenrs    NUMBER;
    lenmn    NUMBER;
    minimum_len NUMBER := pminimum_len;
    xs       NUMBER;
    ys       NUMBER;
    xm       NUMBER;
    ym       NUMBER;
    xn       NUMBER;
    yn       NUMBER;
    xr       NUMBER;
    yr       NUMBER;
    x1       NUMBER;
    y1       NUMBER;
    x2       NUMBER;
    y2       NUMBER;
    width    NUMBER := max_width;
    factor   NUMBER := 2.;    -- an opening tolerance 
    r        NUMBER;  -- line parameter from s -> r
    t        NUMBER;  -- line parameter from m -> n
    tr       NUMBER := 1.;  -- ratio of
    tm       NUMBER := 1.;  -- ratio of
    kount    PLS_INTEGER := 0;
    total_kount PLS_INTEGER;
    loops    PLS_INTEGER := 0;
    loop_max PLS_INTEGER;
    n        PLS_INTEGER;
    next    PLS_INTEGER;
    last_valu PLS_INTEGER;
    plast   PLS_INTEGER;
    pos       PLS_INTEGER;
    pseg      PLS_INTEGER;
    last_next PLS_INTEGER := 0;
    next_xy  PLS_INTEGER;
    open_kount PLS_INTEGER :=0;
    Info_count pls_integer;
    no_of_rings PLS_INTEGER;
    last_ring_vtx  PLS_INTEGER;
    m        PLS_INTEGER;
    istart   PLS_INTEGER;
    last     PLS_INTEGER;
    way       VARCHAR2(5);
    
    open_up  BOOLEAN;
 
    Function Findit(find_seg pls_integer) return pls_integer as
    
--  Find Pipe # for a particular segment searching in both the Subject Segments 
-- and the Matching segments
    Begin
        For pp in 1..Pipe_pars.count loop
           if Pipe_pars(pp).seg = find_seg or Pipe_pars(pp).mseg = find_seg then
              Return Pipe_pars(pp).pipe_no;
           end if;
        End loop;
  
        Return 0; -- no find
    End;
    
    Function Scanit(pstart pls_integer,find_seg pls_integer,next_seg in out nocopy pls_integer) return pls_integer as
-- scan pipe segments looking for the start of another pipe beyond current
-- segment, find_seg
    Begin
--        dbms_output.put_line('scanning for ' || find_seg || ' last ' || last);
        For pp in pstart..Pipe_pars.count loop
--         dbms_output.put_line('pp ' || pp || ' seg ' || Pipe_pars(pp).seg||' B ' ||Pipe_pars(pp).link_bck || ' F '|| Pipe_pars(pp).link_fwd);
           if (Pipe_pars(pp).link_bck=0 or Pipe_pars(pp).link_fwd=0) and
              Pipe_pars(pp).seg >= find_seg and Pipe_pars(pp).seg <= last then
              next_seg := Pipe_pars(pp).seg;
              Return pp;
            elsif Pipe_pars(pp).seg > find_seg then
            next_seg := last;
              Return 0;   -- no find
           end if;
        End loop;
        next_seg := last;
        Return 0;
    End;
    
    Function Copy_Xys(pmc pls_integer,pnc pls_integer, mm in out nocopy  pls_integer) return pls_integer as

-- Copy non-pipe (polygon) segments. Don't copy a previously used segment.
-- As we copy, mark (with a 1)  all the segments we use.
 
      mc        pls_integer:= pmc;
      nc        pls_integer:= pnc;
      seg_no    pls_integer;
      bad_count pls_integer :=0;
    Begin

-- Check to see if there are any previously used within the desired range.

        For jj in mc..nc loop
             seg_no := TRUNC((jj+1)/2);
             If MOD(jj,2) = 1 then
 --            dbms_output.put_line('SEG CHECK ' || seg_no || ' used ' || used_segs(seg_no));
               if Used_Segs(seg_no)  = 1.0 then 
                 return 0;
               end if;
               if Used_Segs(seg_no)  = 2.0 then   -- pipe segments
                  bad_count := bad_count +1;
               end if;
             end if;
        End loop;
        dbms_output.put_line('bad count ' || bad_count);
        if bad_count > 1 then
           return 0;
        end if;
        if seg_no > 1 and Used_Segs(seg_no-1) = -1. then
          nc := nc-2;
        end if;
        dbms_output.put_line('MCC ' || mc || ' nc ' || nc);
        if mm > 1 and Ring_Xys(mm-1) = Xys(mc) and Ring_Xys(mm) = Xys(mc+1) then
           mc := mc+2;
        end if;
       
-- Copy and mark

        For jj in mc..nc loop
             mm := mm + 1;
             Ring_Xys(mm) := Xys(jj);
             If MOD(jj,2) = 1 then
               Used_Segs(TRUNC((jj+1)/2))  := 1.0;
--                dbms_output.put_line('marked ' || TRUNC((jj+1)/2));
             end if;
        End loop;
        return mm;
    End;
    
    Procedure Copy_startof_Pipe(pp pls_integer, mm in out nocopy  pls_integer) as
      mc   pls_integer := (pp-1)*8;
      bias pls_integer := 1;
    Begin
    
-- Copy a pipe opening, jumping across the opening from (1,2) to (7,8)
    
--  Pipe ends are stored like this  7,8 ------<------- 5,6
--                                  1,2 ------>------- 3,4

-- Copy either 4 coordinates (1,2 and 7,8) or just the pipe end, 2 coordinates (7,8)

        if mm > 1 and Ring_Xys(mm-1) = PXys(mc-1) and Ring_Xys(mm) = PXys(mc) then
           bias := bias+2;  -- Skip this vertex since we have it
        end if;
        dbms_output.put_line('bias ' || bias || ' MC ' || mc);
        For jj in mc+bias..mc+8 loop
           if jj<= mc+2 or jj >= mc+7 then
             mm := mm + 1;
             Ring_Xys(mm) := PXys(jj);
--             dbms_output.put_line('mm ' || mm || Pxys(jj));
--          else
--           dbms_output.put_line('not '||  Pxys(jj));
          end if;
        End loop;
    End;
    Procedure find_Info(coord pls_integer) as

--  Although we know the ring we start with, the ring may change if there are 
--  inner rings that create smaller polygons. Here 2 pipes are shown created
--  by an inner ring. Going from SS to MM we switch to ring2 and then back to
--  ring1 at ss to mm.

--          inner ring
--           (ring 2)
--  mm| |ss          MM| |
--    | |______________| | SS
--    |                  |
--    |                  |
--    |__________________|  Outer ring (ring 1)
--
--  Set ibeg and iend (start and end vertices) for a particular ring 
--  specified by an input coordinate
    
    Begin
         for ij in 1..Info_count loop

            If ij <> Info_count then
               last := Info(ij*3+1)-1;
            else
               last := Xys.count;
            end if;
 --            dbms_output.put_line('info ' || info(ij*3-2) || ' last ' || last);
            if coord >= Info(ij*3-2) and coord <= last then
               istart := TRUNC((Info(ij*3-2)+1)/2);
               last := TRUNC(last/2);
 --              dbms_output.put_line('start ' || istart || ' last ' || last);
               exit;
            end if;
         end loop;
-- This is an override for when there is a pipe at the end of a ring so the
-- desired last here is 97, not 100

--             98      97
--        -------------------
--    99 |_________
--        100     | 1
--                |
         if last_valu is not NULL and last_valu >= istart and last_valu <= last then
           last := last_valu;
         end if;
         
--         dbms_output.put_line('last '|| last || ' LV ' || last_valu);
    End;
    
    Function Build_a_ring(pos pls_integer,plink_bck pls_integer, plink_fwd pls_integer,projector number) return pls_integer as

--
--
--    To build a ring jumping across pipe openings as needed, there are just
--    two methods: 1) copy ring coordinates
--                 2) copy the pipe ends previously found by projecting points
--                    across the opening.

--      It sounds simple, since all we need to do is use these methods correctly 
--   until we return to our start point and complete the ring. In the first picture
--   we just to copy segment 1, part of 2, across from SS to MM, segment 7,
--   across from ss to mm and then part of 10 back to the polygon start.

--     9
--    ___                5
--    | |              -----
--    | |8             |  __| 4
--  mm| |ss    7     MM| |
--    | |______________| | SS
--10  |                  |
--    |                  | 2
--    |__________________|  Single ring 
--
--     1 (segment #)

--
--    A simple change in the picture can make the process more complicated.

--          inner ring
--           (ring 2)
--  mm| |ss          MM| |
--    | |______________| | SS
--    |                  |
--    |                  |
--    |__________________|  Outer ring (ring 1)

      mcoord    pls_integer;
      ncoord    pls_integer;
      find_seg  pls_integer;
      next_seg  pls_integer;
      link_bck  pls_integer;
      link_fwd  pls_integer;
      mm        pls_integer :=0;
      pp        pls_integer;
      ok        pls_integer;
      pstart    pls_integer :=pos+1;
      done      boolean := FALSE;
  
      loups     pls_integer :=0;
    Begin
    
      If plink_bck = 0 then
      dbms_output.put_line('****************LINK back is ZERO' || used_segs(istart));
      
--    There are 2 possible situations:
--    1) link back is zero -> pipe cannot be extended back, it does extend forwards
--       Proceed from the matching segment counter clockwise to the next subject seg.
--       If none and you are at the end of the ring, you are done

--     ii 1 pipe 1 S 1 M 398 C 1423.13 LB 0 LF 2  <<--
--     ii 2 pipe 1 S 4 M 399 C 1423.42 LB 1 LF 3
--     ii 3 pipe 1 S 5 M 399 C 1423.13 LB 2 LF 4 

-- If we have not used the start of this ring, use it.
       mm :=0;
       If sseg > 1 and Used_segs(istart) <= 0 then
         mcoord := istart*2-1;
         ncoord := sseg*2-2;
         ok := Copy_Xys(mcoord,ncoord,mm);
         dbms_output.put_line('************************COPIED start of ring');
       end if;
        
-- Draw the projector across the pipe

       Ring_xys(mm+1) := Xys(sseg*2-1);
       Ring_xys(mm+2) := Xys(sseg*2);
       Ring_xys(mm+3) := PXys(next_xy+1);
       Ring_xys(mm+4) := PXys(next_xy+2);
       Ring_xys(mm+5) := PXys(next_xy+7);
       Ring_xys(mm+6) := PXys(next_xy+8);
       mm := mm+6;
       find_seg := mseg;
--     Find another segment
       While NOT done and loups < 20 loop
         loups := loups + 1;
         pp := Scanit(pstart,find_seg+1,next_seg);
--         dbms_output.put_line('PP ' || pp);
         if pp = 0 then
           done := TRUE;
         else
           Openings(pp) := -1.0;    -- mark it as being used
         end if;
  --   Copy from the end of the segment
 
         find_seg := find_seg+1;
  
         mcoord := find_seg*2-1;
         ncoord := next_seg*2;
         dbms_output.put_line('******************copying from m to n ' || mcoord || ' n ' || ncoord);
         ok := Copy_Xys(mcoord,ncoord,mm);
         if ok=0 or NOT done then
         if ok = 0 then
            mm := 0;
            done := TRUE;
         else
         dbms_output.put_line('*******************copying start of pipe ' || pp);
           Copy_startof_Pipe(pp,mm);
           find_seg := Pipe_pars(pp).mseg;
           Find_info(find_seg*2-1);
         end if;
         end if;
       End Loop;
     Elsif pLink_fwd = 0 then
     dbms_output.put_line('<><><><><><><><><LINK FORWARD is ZERO');
--   2) Link forward is zero -> pipe cannot be extended forwards, the ring
--      goes forward.

--ii 20 pipe 1 S 12 M 389 C 1423.12 LB 19 LF 0     <<--
--ii 21 pipe 2 S 20 M 36  C 1423.13 LB 0 LF 22
--ii 22 pipe 2 S 20 M 35  C 1423.43 LB 21 LF 23
--ii 23 pipe 2 S 20 M 34  C 1423.4 LB 22 LF 24

-- If we have not used the start of this ring, use it.
       mm :=0;
--         If sseg > 1 and Used_segs(istart) <= 0 then
--         mcoord := istart*2-1;
--         ncoord := sseg*2-2;
--         ok := Copy_Xys(mcoord,ncoord,mm);
--         dbms_output.put_line('************************COPIED start of ring');
--       end if;
        dbms_output.put_line('Mm ' || mm);
       Ring_xys(mm+1) := PXys(next_xy+5);
       Ring_xys(mm+2) := PXys(next_xy+6);
       Ring_xys(mm+3) := PXys(next_xy+3);
       Ring_xys(mm+4) := PXys(next_xy+4);
       mm := mm+4;
        dbms_output.put_line('mm ' || mm);
       find_seg := sseg;
       While NOT done and loups < 20 loop
         loups := loups + 1;
         pp := Scanit(pstart,find_seg+1,next_seg);
         dbms_output.put_line('pp ' || pp || ' finding ' || (find_seg+1));
         if pp = 0 then
           done := TRUE;
           if loups = 1 then
             next_seg := mseg;
             dbms_output.put_line('next seg ' || next_seg);
           end if;
         else
           Openings(pp) := -1.0;  -- mark it as being used
         end if;
  --   Copy from the end of the segment
  --       if loups <> 1 then
            find_seg := find_seg + 1;
  --       end if;

 
         mcoord := find_seg*2-1;
         ncoord := next_seg*2;
          dbms_output.put_line('<><><><><><><><>copying from M to N ' || mcoord || ' n ' || ncoord);
         ok := Copy_Xys(mcoord,ncoord,mm);
         dbms_output.put_line('OK ' || ok || ' MM ' || mm);
         if ok=0 or NOT done then
         if ok = 0 then
            mm := 0;
            done := TRUE;
         else
         dbms_output.put_line('<><><><><><><>copying start of pipe ' || pp);
           Copy_startof_Pipe(pp,mm);
           find_seg := Pipe_pars(pp).mseg;
           Find_info(find_seg*2-1);
         end if;
         end if;
       End Loop;
     End if;

     if mm>=8 then
       if (Ring_Xys(mm-1) <> Ring_Xys(1) or Ring_Xys(mm) <> Ring_Xys(2))  then
       mm := mm+2;
       Ring_Xys(mm-1) := Ring_Xys(1);
       Ring_Xys(mm)   := Ring_Xys(2);
       end if;
        dbms_output.put_line('<><><><><><><>GOOD RESULT '|| mm);
     elsif mm < 8 then
        mm := 0;
        dbms_output.put_line('NO RESULT');
     end if;
     Return mm;
    End;
    Function measure(seg1 pls_integer, seg2 pls_integer)  return pls_integer as
        seg_count   pls_integer;
    Begin
        seg_count := ABS(seg2 - (seg1+last-1));
        return seg_count;
    End;
    Function Traverse(pp pls_integer) return pls_integer as
    
--  Traverse pipe and find last reference when it ends

      pipe_no  number := Pipe_Pars(pp).pipe_no;
      pi       pls_integer := pp;
      
    Begin
       For pi in pp..Pipe_pars.count loop
          if pipe_no = Pipe_pars(pi).pipe_no and Pipe_pars(pi).link_fwd=0 then
             return pi;
          end if;
       End Loop;
       dbms_output.put_line('>>>fail return ' || pp );
       return pp;
    End;
   
BEGIN

    If minimum_len = 0.0 then
     minimum_len := max_width*0.5;
   End If;
    Xys := Geom.sdo_ordinates;
    Info := Geom.Sdo_Elem_Info;
    Info_count := TRUNC(info.count/3);
    n   := TRUNC(XYs.count/2);
    
-- Used Segments cannot be reused

    Used_Segs.extend(n);
    for ii in 1..n loop
       Used_Segs(ii) := 0.0;
    end loop;
    
    area := GZ_PIPES.polygon_area(Xys,Info,Areas);
    perim := GZ_PIPES.perimeter(Xys);
    
    ap := area/perim;
    dbms_output.put_line('Area/Perimeter ratio: ' || round(ap,3) || ' perim ' || perim || ' Area ' || area);
-- Look for pipes

    Pipe_Pars := GZ_PIPES.Find_Get_Pipes(Geom,pipe_type,max_width,minimum_len,min_aspect_ratio,max_angle_diff);

-- If there are none, then return the original geometry unchanged

    IF Pipe_pars.count=0 THEN
       RETURN Geom;
    END IF;
dbms_output.put_line('pipe pars ' || pipe_pars.count);

-- Get the Pipe end coordinates
    PXys := GZ_PIPES.Get_Pipe_Ends(Xys,Pipe_pars);
    Openings.extend(Pipe_pars.count);

-- Look for openings, places where the segments diverge

    FOR p in 1..Pipe_pars.count LOOP
    
    Openings(p) := 0.0;
    total_kount :=0;
    sseg := Pipe_pars(p).seg;
    mseg := Pipe_pars(p).mseg;
    loop_max := ABS(sseg-mseg)-1;
    if loop_max >5 then loop_max := 5; end if;
--    dbms_output.put_line('TRYING ' ||sseg || ' with ' || mseg || ' Loop max ' || loop_max);
    Find_info(sseg);
--    dbms_output.put_line('last ' || last || ' measure ' || measure(2,17));
    Find_info(mseg*2-1);


--  Used_segs:   zero - unused
--               1      used
--              -1     unused but start of pipe so may be used as part of a polygon
--               2      its a pipe segment

    If (Pipe_pars(p).link_bck = 0 or Pipe_pars(p).link_fwd = 0) then 
      Used_segs(sseg) := -1;
      Used_segs(mseg) := -1;
    End if;
    --  Mark the Pipe Segments with a 2
    If (Pipe_pars(p).link_bck <> 0 and Pipe_pars(p).link_fwd <> 0) then
      if Used_segs(sseg) = 0.0 then
      Used_segs(sseg) := 2;
--      dbms_output.put_line('Marked ' || sseg);
      end if;
      if Used_segs(mseg) = 0.0 then
      Used_segs(mseg) := 2;
--      dbms_output.put_line('marked ' || mseg);
      end if;
    End if;
    open_up := FALSE;

--  Possibility of a opening up
    
    If (Pipe_pars(p).link_bck = 0 or Pipe_pars(p).link_fwd = 0) then     
       open_up := TRUE;
--       dbms_output.put_line('PP ' || p || ' Findit ' || findit(nseg) || ' findit ' || findit(rseg));
    End if;
     
   -- Sometimes we have 4 pipes coming together. Ignore this, it is not an opening
    nseg := mseg+1;
    if nseg >= last then
       nseg := istart;
    end if;
    rseg := sseg-1;
    if rseg = 0 then rseg := last-1; end if;
      if Pipe_pars(p).link_bck = 0 and Findit(nseg) <> 0 and Findit(rseg) <> 0 and
                                        Findit(nseg) <> Pipe_pars(p).pipe_no and
                                        Findit(rseg) <> Pipe_pars(p).pipe_no then
         open_up := FALSE;
      end if;
      
    nseg := mseg-1;
    if nseg =0 then
      nseg := last-1;
    end if;
    rseg := sseg+1;
    if rseg >last then rseg := istart; end if;
      if Pipe_pars(p).link_fwd = 0 and Findit(nseg) <> 0 and Findit(rseg) <> 0 and
                                        Findit(nseg) <> Pipe_pars(p).pipe_no and
                                        Findit(rseg) <>  Pipe_pars(p).pipe_no then
--       dbms_output.put_line('pp ' || p || ' Findit ' || findit(nseg) || ' findit ' || findit(rseg));
         open_up := FALSE;
      end if;
   
-- End of a peninsula
--      if loop_max < 2 and Pipe_pars(p).link_fwd = 0 then
--         open_up := FALSE;
--      end if;
-- End of a peninsula near the the beginning of the face      
      plast := Traverse(p);
      if Measure(sseg,mseg) < 3 and Measure(Pipe_pars(plast).seg,Pipe_pars(plast).mseg) > Measure(sseg,mseg) then
        open_up := FALSE;
      end if;
      
      
      Find_info(mseg*2-1);
      last_ring_vtx := last;
      if close_measure(sseg,mseg,Info,Xys.count) < 4 then
         open_up := FALSE;
      end if;
--      dbms_output.put_line('CLOSE ' || close_measure(sseg,mseg,last_ring_vtx) || ' ' || sseg || ' ' || mseg);
-- Check if this is the start of a pipe then see if it opens up

      If open_up then
       
      
        If Pipe_pars(p).link_bck = 0 then
         way := 'LEFT';
         dbms_output.put_line('OPEN up to left is a possibility ' || Pipe_pars(p).seg||'-' || ' and ' || Pipe_pars(p).mseg||'+');
        
-- Look for a large opening
--                  (xn,yn)          (xm,ym)     <---- mseg increases
--                  +-----------------+-----------+
--    away from pipe <------             towards pipe -->
--                           (xs,ys)  +--------------+
--                                    |           ---> seg increases
--                                    |
--                                    |
--                                    + (xr,yr)
          mseg := mseg+1;
          if mseg >= last then
            mseg := istart;
          end if;
        Else
          way := 'RIGHT';
         dbms_output.put_line('OPEN UP to right is a possibility ' || Pipe_pars(p).seg || '+' || Pipe_pars(p).mseg||'-');
-- Look for a large opening                  /
--    <--mseg increases  (xm,ym)    (xn,yn) / 
--                       +-----------------+
--          towards pipe <------             away from pipe -->
--                       +--------------+ (xs,ys)
--                  ---> seg increases  |           
--                                      |
--                                      |
--                                       + (xr,yr)
          mseg := mseg-1;
          if mseg < istart then
            mseg := last;
          end if;       
        End if;
      


-- Setup to iterate in case the segments are very short
      rseg := sseg;
      nseg := mseg;
      dist := 0.;
      distlast := 0.;
      dist_travelled := 0.;
      loops := 0;
-- dbms_output.put_line('loops ' || loops || ' loop max ' || loop_max || ' width ' || width || ' factor ' || factor);
    WHILE dist < width* factor and loops < loop_max LOOP
    
      loops := loops+1;
      
      sseg := rseg;
      mseg := nseg;
      next := (p-1)*8;
      
-- Get the actual pipe ends to begin
----           (7,8)+----------+ (5,6)
--             (1,2)+----------+ (3,4)
/*      
      if loops = 1 and way = 'LEFT'then 
          xs := Pxys(next+1);
          ys := Pxys(next+2);
          xm := Pxys(next+7);
          ym := PXys(next+8);
--        Set up previous vertex which is going to be decremented below
          rseg := sseg;
          if xs = Xys(rseg*2-1) and ys = Xys(rseg*2) then
             NULL;   -- start of pipe matches sseg, cases 12 and 13
          else
             rseg := rseg+1;   -- cases 43 and 43
          end if;
--        Set up next vertex which is going to be incremented below
          nseg := mseg+1;
          if xm = Xys(nseg*2-1) and ym = Xys(nseg*2) then
             NULL;   -- incremented below
          else
             nseg := nseg-1;
          end if;
      elsif loops = 1 and way = 'RIGHT' then
          xs := Pxys(next+3);
          ys := Pxys(next+4);
          xm := Pxys(next+5);
          ym := PXys(next+6);
--        Set up next vertex which is going to be incremented below
          rseg := sseg+1;
          if xs = Xys(rseg*2-1) and ys = Xys(rseg*2) then
             NULL;    -- end of pipe matches vertex sseg+1
          else
             rseg := rseg -1;
          end if;
--        Set up previous vertex which is going to be decremented below          
          nseg := mseg;
          if xm = Xys(nseg*2-1) and ym = Xys(nseg*2) then
             NULL;   -- end of pipe matches mseg
          else
             nseg := nseg + 1;
          end if;
      else
      */
        xs := Xys(sseg*2-1);
        ys := Xys(sseg*2);
        xm := Xys(mseg*2-1);
        ym := Xys(mseg*2);
--        dbms_output.put_line('xs ' || round(xs,7) || ' ys ' || round(ys,7));
--        dbms_output.put_line('xm ' || round(xm,7) || ' ym ' || round(ym,7));
--      end if;
-- Go LEFT
       If way = 'LEFT' then
        rseg := rseg-1;
        if rseg = 0 then rseg := last-1; end if;
        nseg := nseg+1;
        if nseg > last then nseg := istart+1; end if;
      else
-- GO RIGHT
        nseg := nseg-1;
        if nseg =0 then
          nseg := last-1;
        end if;
        rseg := rseg+1;
        if rseg >last then rseg := istart; end if;      
      End if;

      xr := Xys(rseg*2-1);
      yr := Xys(rseg*2);
      xn := Xys(nseg*2-1);
      yn := Xys(nseg*2);
      
  -- To march along each segment we want to ensure they are similar lengths
      lenrs := GZ_PIPES.distance_fcn(xr,yr,xs,ys);
      lenmn := GZ_PIPES.distance_fcn(xm,ym,xn,yn);
      
--      dbms_output.put_line('sseg ' || sseg || ' rseg ' || rseg || ' mseg '||mseg || ' nseg ' || nseg);
--      dbms_output.put_line('RS ' || round(lenrs,3) || ' MN ' || round(lenmn,3));
      if lenrs > lenmn then
         tm := lenmn/lenrs;   -- ratio to keep distance proportional
         tr := 1.;
         dist_travelled := dist_travelled + lenmn;
      else
         tr := lenrs/lenmn;
         tm := 1.;
         dist_travelled := dist_travelled + lenrs;
      end if;
      
  --    dbms_output.put_line('tm ' || round(tm,6) || ' tr ' || round(tr,6));
      distlast := GZ_PIPES.distance_fcn(xs,ys,xm,ym);
--    dbms_output.put_line('Width pipe ' || round(distlast,3));
      kount :=0;
      for ii in 1..5 loop
        r := ii*0.1*tr;
        t := ii*0.1*tm;              -- multiply to space evenly away from confluence
        x1 := xr*r + xs*(1.-r);
        y1 := yr*r + ys*(1.-r);
        x2 := xn*t + xm*(1.-t);
        y2 := yn*t + ym*(1.-t);
        dist := GZ_PIPES.distance_fcn(x1,y1,x2,y2);
        if dist > distlast then
           kount := kount+1;
           distlast := dist; --*1.1;
        end if;
--        dbms_output.put_line('D ' || round(dist,3) || ' K ' || kount || ' r ' || round(r,6) || ' t ' || round(t,6));
--        dbms_output.put_line('x ' || round(x1,7) || ' y ' || round(y1,7) || ' x ' || round(x2,7) || ' y ' ||round(y2,7)); 
      end loop;
      total_kount := total_kount + kount;
-- Require at least 3 increases out of five
      if (kount < 3 and dist_travelled > 2.* width) then --or (total_kount < 3 and loops = 2) then
         open_up := FALSE;
         dbms_output.put_line('CLOSED ' || p || ' sseg ' || Pipe_pars(p).seg || ' mseg ' || Pipe_pars(p).mseg);
         exit;
--      elsif kount >= 3 then
--         exit;
      end if;
      
   
  END LOOP;
     if total_kount = 0 then --distlast < 2.*width  then
         open_up := FALSE;
         dbms_output.put_line('CLOSEDD ' || p || ' sseg ' || Pipe_pars(p).seg || ' mseg ' || Pipe_pars(p).mseg);    
      end if;
     if open_up then
        open_kount := open_kount + 1;
        Openings(p) := 1.0;
        dbms_output.put_line('OPEN ' || p || ' sseg ' || Pipe_pars(p).seg || ' mseg ' || Pipe_pars(p).mseg);
     end if;
    END IF;
  END LOOP;
  dbms_output.put_line('OPENINGS ' || open_kount);

    
    next :=0;
  If open_kount > 0 then
    next_xy := -8;
    no_of_rings :=0;
    Ring_Xys.extend(n*4);
    FOR p in 1..Pipe_pars.count LOOP
    
      
 -- Trace from one opening until we close the ring
      next_xy := next_xy+8;
      m :=0;
      
      If Openings(p) = 1.0 then
      dbms_output.put_line('trying polygon starting at ' || p);
--    Build a ring and then add its ring coordinates
         sseg := Pipe_pars(p).seg;
         mseg := Pipe_pars(p).mseg;
         Find_info(mseg*2-1);
   -- Set last iff the matching seg is last -1.
-- WE NEED to maintain last at 15 !!!!!      
--      dbms_output.put_line('>>>>>>>last ' || last || ' mseg ' || mseg || ' pipe ' || pipe_pars(p).pipe_no);
    
        for ij in 1..Pipe_pars.count loop
           If Pipe_pars(ij).link_bck=0 and Pipe_pars(ij).pipe_no = Pipe_pars(p).pipe_no and
              Pipe_pars(ij).mseg=last-1 then
              last_valu := Pipe_pars(p).mseg;
              last := last_valu;
              exit;
           end if;
        end loop;
      
-- When we start a ring we can have the start of the ring as part of a pipe
-- (link forward =0). Mark it used so we dont copy later
--         99
--      -----------
-- 100  |
--       ----+------+------------
--         1    2        3

/*
      If Used_segs(istart) < 2 and Pipe_pars(p).link_fwd = 0 then
        for ij in 1..p-1 loop 
           if Pipe_pars(ij).pipe_no = Pipe_pars(p).pipe_no then
              pseg := Pipe_pars(ij).seg;
              Used_segs(pseg) := 2.;     -- Mark it as used
              dbms_output.put_line('MARKING ' || pseg);
           end if;
        end loop;
      End if;
*/
      dbms_output.put_line('>>>>>>>LVLVLV ' || last_valu);
         dbms_output.put_line('seg ' || sseg || ' MSEG ' || mseg || ' bck ' || pipe_pars(p).link_bck || ' fwd ' || pipe_pars(p).link_fwd ||' last ' || last);
         pos := p;
         m := Build_a_ring(pos,Pipe_pars(p).link_bck,Pipe_pars(p).link_fwd,Pipe_pars(p).projector);
         if m > 0 then
         no_of_rings := no_of_rings+1;
         dbms_output.put_line('>>>>>>>>>>>>>>SAVING A RING ' || no_of_rings);
         Out_Xys.extend(m);
         Out_Info.extend(3);
         Out_Info(no_of_rings*3-2) := last_next+1;
         Out_Info(no_of_rings*3-1) := 1003;
         Out_Info(no_of_rings*3  ) := 1;
         For ij in 1..m loop
            next := next+1;
            Out_Xys(next) := Ring_Xys(ij);
         End loop;
         last_next := next;
         end if;
      End If;
    
    END LOOP;


-- Guard against catastrophic failure

    if Out_XYs.count > 0 then
      Out_Geom := MDSYS.SDO_Geometry(2007,8265.,NULL,Out_Info,Out_Xys);
    end if; 
    
-- But allow at present the whole geometry to be a pipe and return NULL
-- Commenting these 2 lines  returns the whole geometry when it is all a pipe

    ELSIF open_kount = 0 then
      Out_Geom := NULL;
    END IF;
    
    RETURN Out_Geom;
END REMOVE_PIPES;
--

Function Check_Angles(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,pseg1 PLS_INTEGER,pseg2 PLS_INTEGER, first_coord PLS_INTEGER,last_coord PLS_INTEGER, threshold NUMBER default 170.) RETURN NUMBER AS

-- Check for consistent bearings between seg1 and seg2 in the coordinate array, Xys.
-- Compute average angle difference

--  Xys: the coordinate array to use
--  seg1,seg2: the segment numbers to check for an angular difference
-- first_coord, last_coord: the first coordinate and the last coordinate in the ring

  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;  
  x3                NUMBER;
  y3                NUMBER;
  angle             NUMBER;
  total             NUMBER :=0;
  kount             PLS_INTEGER := 0;
  loops             PLS_INTEGER := 0;
  seg1              PLS_INTEGER := pseg1;
  seg2              PLS_INTEGER := pseg2;
  tmp               PLS_INTEGER;
  
Begin

  if seg1 > seg2 then
     tmp := seg1;
     seg1 := seg2;
     seg2 := tmp;  
  end if;
  For seg_no in seg1+1..seg2 loop
    If seg_no = seg1+1 then
       x1 := Xys(seg1*2-1);
       y1 := Xys(seg1*2);
       x2 := Xys(seg_no*2-1);
       y2 := Xys(seg_no*2);
    Else
       x1 := x2;
       y1 := y2;
       x2 := x3;
       y2 := y3;
    End if;

    if seg_no*2 = last_coord then
       x3 := Xys(first_coord);
       y3 := Xys(first_coord+1);
    else
       x3 := Xys(seg_no*2-1);
       y3 := Xys(seg_no*2);
    end if;
    angle := GZ_QA.angle(x1,y1,x2,y2,x3,y3,TRUE);
    
    
    if angle >= threshold then   
       kount := kount + 1;
       total := total + angle;
    end if;
    loops := loops+1;
  End Loop;
  
  -- Work out average angle
  
  if kount > 0 then
    total := total/kount;
  end if;
  
  Return total;
End check_angles;
--
FUNCTION Find_it(vtx pls_integer,Pipe_pars IN OUT NOCOPY Pipes_Type,first VARCHAR2 default 'Y') return mdsys.sdo_list_type as

-- Try to find a vertex in the segment list. Vertex may be there or may not. If 
-- its not there it may be the end of the preceding segment (vertex -1).
-- So with the start we expect to use the end of Seg and the beginning of Mseg
--  and vice versa
--         Mseg   /                       \  Mseg
--         --<---/                         \--<---
--         -->---                           -->---
--          Seg  \                         /  Seg

   pi              PLS_INTEGER;
   seg_no          PLS_INTEGER;
   at_start_end    NUMBER;

Begin
-- First check Seg which goes --->----
 
-- at_start_end uses the same convention as we use elsewhere:
--  1 means start of an S segment and 2 means the end of an S segment
      
        pi := 0;
        at_start_end :=0;   -- 1 means start of vtx, 2 means end of vtx-1
        seg_no := vtx;
        pi := Find_it_inseg(vtx,Pipe_pars,first);
 
        if pi > 0 then
           at_start_end := 1.;
        elsif pi = 0 and vtx > 1 then
          pi := Find_it_inseg(vtx-1,Pipe_pars,first);
--          dbms_output.put_Line('finding ' ||(vtx-1) || ' ' || pi);
          if pi > 0 then
            seg_no := vtx-1;
            at_start_end := 2.;
          end if;
        end if;
-- Now check Mseg  which goes in this direction ----<---

-- at_start_end uses the same convention as we use elsewhere:
--  3 means start of an M segment and 4 means the end of an M segment

        if pi = 0 then
         pi := Find_it_inMseg(vtx,Pipe_pars,first);
         if pi > 0 then
           at_start_end := 3.;    -- 3 means start of vtx, 4 means end of vtx-1
         elsif pi = 0 and vtx > 1 then
           pi := Find_it_inMseg(vtx-1,Pipe_pars,first);
           if pi > 0 then
           seg_no := vtx-1;
             at_start_end := 4.;
           end if;
         end if;
        end if;
        

  RETURN mdsys.sdo_list_type(seg_no,pi,at_start_end);
  
END;
--
FUNCTION BUILD_RINGS(Pipe_pars IN OUT NOCOPY Pipes_Type,Class_it IN OUT NOCOPY CHAR_ARRAY,Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Info MDSYS.SDO_ELEM_INFO_ARRAY,Used IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN XY_List AS

-- Build Rings

  Ring_Jump         XY_List;
  result           MDSYS.SDO_LIST_TYPE;
  at_start_end      PLS_INTEGER;
 
  last_vtx          PLS_INTEGER;
  no_rings          PLS_INTEGER;
  ring_no           PLS_INTEGER;
  ring              PLS_INTEGER;
  last_rpos         PLS_INTEGER;
  what              PLS_INTEGER;
  istart            PLS_INTEGER;
  iend              PLS_INTEGER;
  pi                PLS_INTEGER;
  seg               PLS_INTEGER;
  mseg              PLS_INTEGER;
  rpos              PLS_INTEGER := 0;
  seg_no            PLS_INTEGER;
  vtx               PLS_INTEGER;
  ring_count        PLS_INTEGER := TRUNC(Info.count/3);
  
  projector         NUMBER;
  last_ring         NUMBER := 0.0;
  xstart            NUMBER;
  ystart            NUMBER;
  
  ring_dnsat_one    BOOLEAN := FALSE;
  
     Procedure find_Info(coord pls_integer) as

--  Set istart and iend (start and end segments) for a particular ring 
--  specified by the input coordinate offset in the XYs array
    
    Begin
         for ij in 1..ring_count loop
         
            iend := Xys.count;
            
            If ij <> ring_count then
               iend := Info(ij*3+1)-1;
            end if;
            
            if coord >= Info(ij*3-2) and coord <= iend then
               istart := TRUNC((Info(ij*3-2)+1)/2);  -- start segment = start vertex
               iend := TRUNC(iend/2)-1;  --end segment = end_vertex -1
               exit;
            end if;
         end loop;

    End;
    Function find_ring(coord pls_integer) return pls_integer as
    
    begin
    
    
      for iring in 1..ring_count loop
         
            iend := Xys.count;
            
            If iring <> ring_count then
               iend := Info(iring*3+1)-1;
            else
              iend := Xys.count;
            end if;
            
            if coord >= Info(iring*3-2) and coord <= iend then
               return iring;
            end if;
       end loop;
       
--       dbms_output.put_line('coord ' || coord || ' iend ' || iend);
     End;
BEGIN


-- Determine the ring jump vertices, where a ring begins or ends  

   
  last_ring := 0;
  ring := 1;
  seg_no := 0;
  last_rpos :=0;
  For ii in 1..2*Class_it.count Loop
    seg_no := MOD(seg_no,Class_it.count)+1;
    ring := Find_ring(seg_no*2);
    If Used(seg_no) = 0. and Class_it(seg_no) = 'S' then
--    dbms_output.put_line('IN this ring '||ring || ' seg ' ||seg_no);
    ring_no := Find_ring(seg_no*2);
-- We need to walk arbitrarily around each ring and manipulate the
-- vertex (vtx) we are at.Note if we start at vertex 1 at the diagram below,
-- that we will complete the biggest ring before moving onto the other smaller rings.
 
-- Sometimes we may have a short segment that has been marked as a 'c'
-- 

       vtx := seg_no;   
       Used(vtx) := 1.0;     -- we can go to later parts of the polygon 
--                              so the vertices are already used.

-- So with the start we expect to use the end of Seg and the beginning of Mseg
--  and vice versa
--                 -----------------------
--         Mseg   /                       \  Mseg
--    -------<---E                         S--<------------
--    |   1-->--S                           E->-----      |
--    |   | Seg  \                         /  Seg   |     |
--    |   |       -------------------------         |_____|
--    |___|
--                Start (S)  to End (E) on one ring 


-- First check Seg, and then Mseg
        rpos := rpos +1;
        
        result := Find_it(vtx,Pipe_pars,'N');
        what := result(1);
        pi := result(2);
        at_start_end := result(3);   -- extract whether at start or end;
 --    dbms_output.put_line('vtx ' || vtx || ' pi ' || pi );   
  if pi =0 then
  
     pi := 1/0;
  end if;
      
      Projector := Pipe_pars(pi).projector;
      mseg := Pipe_pars(pi).mseg;
      seg := Pipe_pars(pi).seg;
--      dbms_output.put_line('vtx ' || vtx || ' pi ' || pi || ' P ' || projector || ' mseg ' || mseg || ' at sE ' || at_start_end);
-- Set up the 1st vertex in the ring, the jump vertex across the pipe so the ring 
-- may close. Also set the vertex before so we know what this created vertex
-- attaches to.
      if last_rpos = 0 then
      last_rpos := rpos;
      if at_start_end < 3 then  -- this means we found vtx
      if projector = 12 or projector = 42 then
        if Pipe_pars(pi).Xq <> Xys(mseg*2-1) and  Pipe_pars(pi).Yq <> Xys(mseg*2) then
--        dbms_output.put_line('heeere');
       Ring_jump(rpos).x := Xys(mseg*2-1);
        Ring_jump(rpos).y := Xys(mseg*2);
        rpos := rpos+1;
        end if;
        Ring_jump(rpos).x := Pipe_pars(pi).Xq;
        Ring_jump(rpos).y := Pipe_pars(pi).Yq;
        Ring_jump(rpos).start_vertex := mseg;  -- point before is vertex mseg
      Elsif projector = 13 or projector = 43 then
        Ring_jump(rpos).x := Xys(mseg*2-1);
        Ring_jump(rpos).y := Xys(mseg*2);
        Ring_jump(rpos).start_vertex := mseg;  -- redundant but copier will ignore it
      end if;
      else
        
       if projector = 42 or projector = 43 then
       if Pipe_pars(pi).Xp <> Xys(seg*2-1) and  Pipe_pars(pi).Yp <> Xys(seg*2) then
       Ring_jump(rpos).x := Xys(seg*2-1);
        Ring_jump(rpos).y := Xys(seg*2);
        rpos := rpos+1;
        end if;
        Ring_jump(rpos).x := Pipe_pars(pi).Xp;
        Ring_jump(rpos).y := Pipe_pars(pi).Yp;
        Ring_jump(rpos).start_vertex := seg;  
--        dbms_output.put_line('sssset start ' || xstart || ', ' || ystart || ' pi ' || pi);
      Elsif projector = 12 or projector = 13 then
        Ring_jump(rpos).x := Xys(seg*2-1);
        Ring_jump(rpos).y := Xys(seg*2);
        Ring_jump(rpos).start_vertex := seg;
      end if;
      
      end if;
      
-- Set the ring, we must start and end each portion within the same ring
      
      
-- Now set the 2nd vertex - the one across from the start and continuing the ring
      
      xstart := Ring_jump(last_rpos).x;
      ystart := Ring_jump(last_rpos).y;
--       dbms_output.put_line('>>>>>>>set start ' || round(xstart,8) || ', ' || round(ystart,8) || ' rpos ' ||rpos);
--       if rpos = 2 then
--        dbms_output.put_line('>>>>>>>at 2 ' || ring_jump(rpos).x || ', ' || ring_jump(rpos).y || ' rpos ' ||rpos);
--       end if;
      rpos := rpos+1;
      end if;
      if at_start_end < 3 then
      if projector = 12 or projector = 42 then
        Ring_jump(rpos).x := Xys(seg*2+1);
        Ring_jump(rpos).y := Xys(seg*2+2);
        Ring_jump(rpos).next_vertex := seg+1;
--        dbms_output.put_line('set across from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      Elsif projector = 13 or projector = 43 then
        Ring_jump(rpos).x := Pipe_pars(pi).Xq;
        Ring_jump(rpos).y := Pipe_pars(pi).Yq;
        Ring_jump(rpos).next_vertex := seg+1;
--         dbms_output.put_line('set Qacross from Start ' || round(Ring_jump(rpos).x,8)||','||round(Ring_jump(rpos).y,8));
      end if;
      else
       if projector = 12 or projector = 13 then
        Ring_jump(rpos).x := Pipe_pars(pi).Xp;
        Ring_jump(rpos).y := Pipe_pars(pi).Yp;
        Ring_jump(rpos).next_vertex := mseg+1;
--         dbms_output.put_line('set Across from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      Elsif projector = 42 or projector = 43 then
        Ring_jump(rpos).x := Xys(mseg*2+1);
        Ring_jump(rpos).y := Xys(mseg*2+2);
        Ring_jump(rpos).next_vertex := mseg+1;
--         dbms_output.put_line('set QAcross from start ' || round(Ring_jump(rpos).x,8)||','||round(Ring_jump(rpos).y,8));
      end if;
      
      end if;
      rpos := rpos+1;
      Ring_jump(rpos).start_vertex := vtx;
--      dbms_output.put_line('set an start vertex ' || vtx);
      For ij in 1..Class_it.count loop
      seg_no := seg_no + 1;

         vtx := MOD(seg_no-1,Class_it.count)+1;
--         if vtx >= 895 then
--          dbms_output.put_line(seg_no||'vertex ' || vtx || 'rpos ' ||rpos||' ' ||used(vtx));
--end if;

-- Sometimes the last ring in Info does not begin at vertex 1

        ring := Find_ring(vtx*2);
        if vtx = 1 and ring <> ring_no then
          ring_dnsat_one := TRUE;    -- ring does not start at 1
        elsif vtx = 1 then
        ring_dnsat_one := FALSE;
        end if;
         if Used(vtx) = 0. and Class_it(vtx) = 'E' and ring = ring_no then
         
-- Sometimes the last ring in Info does not begin at vertex 1 so we have to set
-- another end and start

            If ring_dnsat_one then
            ring_dnsat_one := FALSE;
             Find_Info(vtx*2);
--             Used(last_vtx) := 1.;
            Ring_jump(rpos).next_vertex := iend;
--dbms_output.put_line('set an end  vertex ' || vtx);
            rpos := rpos + 1;
            Ring_jump(rpos).start_vertex := istart;
--dbms_output.put_line('set a beginning  vertex ' || vtx);
            
            End If;
            
            Used(vtx) := 1.;
            Ring_jump(rpos).next_vertex := vtx;
--dbms_output.put_line('set an end  vertex ' || vtx);
        
            result := Find_it(vtx,Pipe_pars);       
            pi := result(2);
            at_start_end := result(3);   -- extract whether at start or end;
      
-- If we cannot find it in the pipe description then assume the projector is 12
            if pi = 0 then
              Projector := 12;
            else
      Projector := Pipe_pars(pi).projector;
            end if;
--      dbms_output.put_line('vtx ' || vtx || Class_it(vtx) || ' pi ' || pi || ' P ' || projector || ' at sE ' || at_start_end);
-- Is it a ring piece start or a ring piece end?
--dbms_output.put_line('vtx ' ||xys(vtx*2-1) || ',' || Xys(vtx*2) || ' xst ' || xstart ||','||ystart);
           if Xys(vtx*2-1) = xstart and Xys(vtx*2) = ystart then
                 last_rpos := 0;
   
      
--            elsif projector = 13 or projector = 12 then
              
--              Ring_jump(rpos).x := Xys(vtx*2-1);
--              Ring_jump(rpos).y := Xys(vtx*2);
            elsif projector = 43 or projector = 42 then
              rpos := rpos+1;
              Ring_jump(rpos).x := Pipe_pars(pi).Xp;
              Ring_jump(rpos).y := Pipe_pars(pi).Yp;
              Ring_jump(rpos).start_vertex := seg;
--               dbms_output.put_line('set an end ' || Ring_jump(rpos).x || ',' || Ring_jump(rpos).y);
              if Ring_jump(rpos).x = xstart and Ring_jump(rpos).y = ystart then
                 last_rpos := 0;
              end if;
            end if;
-- Jump across the gap and exit when we have completed a ring -> to find another start
            
           
            if last_rpos = 0 then
--               dbms_output.put_line('^^^^^^^^^^^^^^^^^^^^^^COMPleted a ring'||rpos);
               no_rings := no_rings+1;
            end if;
            exit when last_rpos=0;
-- When we don't exit, continue building this ring
      rpos := rpos+1;
--      dbms_output.put_line('Now rpos ' || rpos|| ' ' || at_start_end);
      if at_start_end < 3 then
--      dbms_output.put_line('Hello');
      seg_no := Pipe_pars(pi).mseg;
--      dbms_output.put_line('Hello oracle '||projector);
      if projector = 12 or projector = 13 then
        Ring_jump(rpos).x := Pipe_pars(pi).Xp;
        Ring_jump(rpos).y := Pipe_pars(pi).Yp;
        Ring_jump(rpos).next_vertex := seg_no+1;
--        dbms_output.put_line('set across from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      Elsif projector = 42 or projector = 43 then
        Ring_jump(rpos).x := Xys(seg_no*2+1);
        Ring_jump(rpos).y := Xys(seg_no*2+2);
        Ring_jump(rpos).next_vertex := seg_no+1;
--         dbms_output.put_line('set Qacross from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      end if;
      else
      seg_no := Pipe_pars(pi).seg;
--      dbms_output.put_line('Hello ooracle ');
       if projector = 12 or projector = 13 then
        Ring_jump(rpos).x := Xys(vtx*2-1);
        Ring_jump(rpos).y := Xys(vtx*2);
        Ring_jump(rpos).next_vertex := seg_no+1;
--         dbms_output.put_line('set Across from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      Elsif projector = 42 or projector = 43 then
        Ring_jump(rpos).x := Pipe_pars(pi).Xp;
        Ring_jump(rpos).y := Pipe_pars(pi).Yp;
        Ring_jump(rpos).next_vertex := seg_no+1;
--         dbms_output.put_line('set QAcross from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      end if;
        end if;
        seg_no := Ring_jump(rpos).next_vertex;
        ring_no := Find_ring(seg_no*2);
--         dbms_output.put_line('jumped across to MSEG ' || (seg_no));
        rpos := rpos+1;
      Ring_jump(rpos).start_vertex := seg_no;
      Used(seg_no) := 1.0;
      seg_no := seg_no-1;
--      dbms_output.put_line(vtx||'set another start vertex ' || seg_no || ' rpos ' || rpos|| ' class ' || class_it.count); 
      end if;
      End Loop;
      
    End If;
--    exit when no_rings = 1;
--    exit when seg_no > Class_it.count;
  End Loop;
  
-- Handle incomplete rings

  if last_rpos <> 0 then
    for ii in reverse last_rpos..rpos loop
      Ring_jump.delete(ii);
    end loop;
      rpos := last_rpos-1;
  end if;

-- dbms_output.put_line('RPOS ' || rpos || ' last ' || last_rpos);
--  for ii in 1..rpos loop
--    if Ring_jump(ii).x is not NULL then
--       dbms_output.put_line(ii||'ring jump XY ' || Ring_jump(ii).x || ',' || Ring_jump(ii).y); 
--    else
--       dbms_output.put_line(ii||'ring jump range ' || Ring_jump(ii).start_vertex || ' to ' || Ring_jump(ii).end_vertex);
--    end if;
--  end loop;
  
  Return Ring_Jump;
END BUILD_RINGS;
--
/*
FUNCTION Ring_Preparer(Class_it Char_Array,  ) RETURN MDSYS.SDO_LIST_TYPE AS

  Start_End_Pts   MDSYS.SDO_LIST_TYPE :+ MDSYS.SDO_LIST_TYPE;
BEGIN
--------------------------------------------------------------------------------   
-- Mark Start and End of each ring
-- For each ring: 

   last_coord := 0;
   For ring in 1..ring_count Loop
      first_coord := last_coord+1;   -- odd
      if ring <> ring_count then
         last_coord := Info(ring*3+1) -1;   -- even
      else
         last_coord := n;
      end if;
      find_info(first_coord);
      
-- Loop over segment numbers, from the start_segment to the end_segment
-- So we are processing the vertices as segments

      For vtx in TRUNC(first_coord/2)+1..TRUNC(last_coord/2)-1  loop
      
-- Set current segment (vertex) and the segments ahead and behind

        current_seg := vtx;
        ahead := current_seg+1;
        behind := current_seg-1;
        if behind < istart then
           behind := iend;
        end if;
        
        if ahead > iend then
           ahead := istart;
        end if;
        
-- Change Chaff to 'T' when ahead and behind not equal to 'U'

        If Class_it(vtx) = 'U' and Lengths(vtx+1-ring) < max_width and
           Class_it(behind) <> 'U' and Class_it(ahead) <> 'U' then
           Class_it(vtx) := 'T';
           dbms_output.put_line('!!!!!! changed ' || vtx);
        End If;
      End Loop;
  End Loop;
  
  Return Start_End_pts;
  
END Ring_Preparer;
*/
--
FUNCTION CLASSIFY_Vertices(Pipe_pars IN OUT NOCOPY Pipes_Type,Ring_Jump IN OUT NOCOPY XY_List,pGeom IN MDSYS.SDO_GEOMETRY,  max_width NUMBER default 150., minimum_len NUMBER default 0.) RETURN CHAR_ARRAY  AS

-- Classify all the vertices, 1) 'P' is a pipe                          | |
--                                                                      | |
--
--                            2) 'C' a pipe continuation at a corner __...
--                                                                   __  .
--                                                                     | |
--                                                                    ...
--                            3) 'C' a cap  (dots)                    | |
--                                                                    | |
--                            4) 'T' a pipe closure at a T __| |___
--                                          (dots)         __...___
--
--                            5) 'u' a pipe that is part of a ring and has
--                                two jump vertices on it.
--                            6) 'U' an unclassified vertex, ring after classification
--                            5) 'p' a pipe end where a jump across occurs
 
 
  Geom              MDSYS.SDO_GEOMETRY     := pGeom;
  XYs               MDSYS.SDO_ORDINATE_ARRAY := Geom.SDO_ORDINATES;
  Info              MDSYS.SDO_ELEM_INFO_ARRAY:= GEOM.SDO_ELEM_INFO; 
  Angles            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Lengths           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Areas             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Pipe_Overlap      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Pipe_NOL          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Used              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Pipe_ends         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  result           MDSYS.SDO_LIST_TYPE;
  Class_it          CHAR_ARRAY := CHAR_ARRAY();
  

-- The pipe type specifies the details of the overlap and the widths
-- and projector
--                -->

--             P(xp,yp)   Q (xq,yq)
--         +-----------------+
--             ^          ^
--   Width_p   |          |  Width q
--             v          v
--             +----------+
--                 <--   

--           

  at_start_end      PLS_INTEGER;
  ring_count        PLS_INTEGER;
  last_pipev        PLS_INTEGER :=-1;
  n                 PLS_INTEGER;
  first_coord       PLS_INTEGER;
  current_pos       PLS_INTEGER :=1;
  seg_no            PLS_INTEGER;
  last_coord        PLS_INTEGER :=0;
  rpos              PLS_INTEGER :=0;
  pi                PLS_INTEGER;
  p_count           PLS_INTEGER := 0;
  vtx               PLS_INTEGER;
  
  p_indx            PLS_INTEGER := 0;
  mseg              PLS_INTEGER;
  seg               PLS_INTEGER;
  nvtx              PLS_INTEGER;
  np                PLS_INTEGER;
  no_rings          PLS_INTEGER :=0;
  area              NUMBER;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;  
  x3                NUMBER;
  y3                NUMBER;

  current_angle     NUMBER;
  angle_epsilon     NUMBER := 10.; -- degrees
  ahead_angle       NUMBER;
  more_ahead_angle  NUMBER;
  behind_angle      NUMBER;
  Len_so_far        NUMBER;
  temp              NUMBER;
  last_ring         NUMBER :=0.0;
  overlap           NUMBER;
  pi_se             NUMBER;
  projector         NUMBER;
  xstart            NUMBER;
  ystart            NUMBER;
  weed_width        NUMBER := max_width*0.5; -- to weed too short isolated pipe segs
  
  current_seg       PLS_INTEGER;
  istart            PLS_INTEGER;
  iend              PLS_INTEGER;
  ahead             PLS_INTEGER;
  behind            PLS_INTEGER;
  tmp               PLS_INTEGER;
  more_tmp          PLS_INTEGER;
  more_more_tmp     PLS_INTEGER;
  more_ahead        PLS_INTEGER;
  more_more_ahead   PLS_INTEGER;
  more_more_more_ahead   PLS_INTEGER;
  more_behind       PLS_INTEGER;
  pos               PLS_INTEGER;
  last_vtx          PLS_INTEGER;
  ring_no           PLS_INTEGER;
  ring              PLS_INTEGER;
  last_rpos         PLS_INTEGER;
  what              PLS_INTEGER;
  ring_dnsat_one    BOOLEAN := FALSE;


  
   Procedure find_Info(coord pls_integer) as

--  Set istart and iend (start and end segments) for a particular ring 
--  specified by the input coordinate offset in the XYs array
    
    Begin
         for ij in 1..ring_count loop
         
            iend := Xys.count;
            
            If ij <> ring_count then
               iend := Info(ij*3+1)-1;
            end if;
            
            if coord >= Info(ij*3-2) and coord <= iend then
               istart := TRUNC((Info(ij*3-2)+1)/2);  -- start segment = start vertex
               iend := TRUNC(iend/2)-1;  --end segment = end_vertex -1
               exit;
            end if;
         end loop;

    End;
    
    Function find_ring(coord pls_integer) return pls_integer as
    
    begin
      for iring in 1..ring_count loop
         
            iend := Xys.count;
            
            If iring <> ring_count then
               iend := Info(iring*3+1)-1;
            else
              iend := Xys.count;
            end if;
            
            if coord >= Info(iring*3-2) and coord <= iend then
               return iring;
            end if;
       end loop;
       
       dbms_output.put_line('coord ' || coord || ' iend ' || iend);
     End;
     
   Function check_angle(input number,check_it number,one number default 1.) return boolean as
-- Check angle between (for example)  85 and 95 degrees for an ostensible 90 degrees.   
   Begin
     
     if input <= check_it+angle_epsilon*one and input >= check_it-angle_epsilon*one then
        return TRUE;
     end if;
     return FALSE;
   End;

 BEGIN

-- The Method is to check in line adjacencies to determine if a segment is part
-- of a pipe. The method attempts to negotiate corners and other obstacles.

   
   Xys := Geom.Sdo_Ordinates;
   Info := Geom.Sdo_Elem_Info;
   ring_count := TRUNC(Info.count/3);
   n := XYs.count;
   
   nvtx := TRUNC(n/2);
   Pipe_Overlap.extend(nvtx);
   Pipe_NOL.extend(nvtx);
 
   -- To index these arrays, we use index+1- ring number below to account for: 
   --   1) There are no angles available for the end of each ring!
   
   Angles := Get_angles(Geom,-1.);

-- and 2) There are no lengths between rings 

   Lengths := GZ_QA.Get_Gcds(Geom,'ALL');
   
-- Determine extent of overlap for each segment.
--
--   +----------+---------+
--        -->   |         |
--    This part |         |  is needed to complete the rectangle
--    of pipe   | +-------+
--              | |
--              | |
--              | +
--              + |


--------------------------------------------------------------------------------   
-- Classify all segments and vertices: 'U' unclassified (a ring vertex), 
--               'P' pipe, 'c' is a corner, 'C' cap, 'T' is part of a T
--              and 'N' end node between rings (no segment)
--   Reserve 'p' for the end of a pipe where we jump across
   
   Class_it.extend(nvtx);
   For ii in 1..nvtx loop  
      Class_it(ii) := 'U';   -- <-- The left over 'U's will be rings
   end loop;
 
 -- Mark the pipe segments determined by Find_get_pipes, first 
 
   For ii in 1..Pipe_pars.count Loop
      Class_it(Pipe_pars(ii).seg) := 'P';     -- <-- wholly or partially pipes
      Class_it(Pipe_pars(ii).mseg) := 'P';    -- ditto
   end loop;
--dbms_output.put_line('AT BEGIN ' || Class_it(326) );

-- Calculate the Overlap lengths. From the length not overlapped we can
-- tell if a segment is only partially a pipe.

   For ii in 1..Pipe_Overlap.count loop
     Pipe_Overlap(ii) := 0.0;
     Pipe_NOL(ii) := -1.0;
   End Loop;
   
   For ii in 1..Pipe_pars.count Loop
     seg_no := Pipe_pars(ii).seg;     
     overlap := 0.0;

--  Subject Segments are sorted ascending.     
     For ij in 1..Pipe_pars.count Loop
--           if pipe_pars(ij).seg = 445 and seg_no = 445 then
--       dbms_output.put_line(' Overlap ' || round(overlap,3) ||' PP ' || round(Pipe_pars(ij).overlap,3));
--     end if;
       if Pipe_pars(ij).seg = seg_no  then
         overlap := overlap + Pipe_pars(ij).overlap;
--         if seg_no = 760 then
--       dbms_output.put_line(' Overlap ' || round(overlap,3) ||' PP ' || round(Pipe_pars(ij).overlap,3));
--     end if;
       elsif Pipe_pars(ij).seg > seg_no then
         exit;
       end if;     
     End Loop;
     ring_no := Find_ring(seg_no*2);
     if seg_no*2 = iend then
       Pipe_NOL(seg_no) := 0.0;
     else
       Pipe_NOL(seg_no) := Lengths(seg_no+1-ring_no) -overlap;
     end if;
     Pipe_Overlap(seg_no) := overlap/Pipe_pars(ii).seg_Length;
--     if seg_no = 760 then
--       dbms_output.put_line('SETTTTTT' || Pipe_pars(ii).seg_length || ' Overlap ' || overlap || ' ' || ring_no);
--     end if;
   End Loop;

-- Matches are also sorted but in ranges that decrease so cannot exit loop early 

    For ii in 1..Pipe_pars.count Loop
     seg_no := Pipe_pars(ii).mseg;
     overlap := Pipe_NOL(seg_no);
     tmp := 0;
     For ij in 1..Pipe_pars.count Loop
       if Pipe_pars(ij).mseg = seg_no and (ii=1 or (ii>1 and seg_no <> Pipe_pars(ii-1).mseg)) then
         overlap := overlap + Pipe_pars(ij).moverlap;
         tmp := 1;
       end if;     
     End Loop;
     if tmp <> 0 then
       ring_no := Find_ring(seg_no*2);
       if seg_no*2 = iend then
         Pipe_NOL(seg_no) := 0.0;
       else
         Pipe_NOL(seg_no) := Lengths(seg_no+1-ring_no) -overlap;
       end if;
       Pipe_Overlap(seg_no) := overlap/Pipe_pars(ii).mseg_Length;
--       if seg_no = 326 then
--         dbms_output.put_line('Settttt' || Pipe_pars(ii).seg_length || ' Overlap ' || overlap || ' ' || ring_no|| ' LL ' || Lengths(seg_no+1-ring_no));
--       end if;
     end if;
   End Loop;

-- Weed very short isolated Pipes or pipes that have very short overlap

     For ring in 1..ring_count Loop
      first_coord := last_coord+1;   -- odd
      if ring <> ring_count then
         last_coord := Info(ring*3+1) -1;   -- even
      else
         last_coord := n;
      end if;
      find_info(first_coord);
  
-- Loop over segment numbers, from the start_segment to the end_segment
-- So we are processing the vertices as segments

      For vtx in TRUNC(first_coord/2)+1..TRUNC(last_coord/2)-1  loop
      
-- Set current segment (vertex) and the segments ahead and behind

        current_seg := vtx;
        ahead := current_seg+1;
        behind := current_seg-1;
        if behind < istart then
           behind := iend;
        end if;

        if ahead > iend then
           ahead := istart;
        end if;
  -- Change it back to an unclassified 
--        if vtx =760 then
--          dbms_output.put_line('vtx ' || vtx ||' L ' || round(lengths(vtx+1-ring),3) || Class_it(behind)||Class_it(vtx)||Class_it(ahead) || round(Pipe_NoL(vtx),3)||' ' || weed_width);
--        end if;
-- Subtracting the Not overlapped length (NOL) to get the overlap length

        If Class_it(vtx) = 'P' and (Lengths(vtx+1-ring)-Pipe_NOL(vtx)) < weed_width and 
          Class_it(behind) = 'U' and Class_it(ahead) = 'U' then
  
          Class_it(vtx) := 'U';
 
--          dbms_output.put_line('vtx ' || vtx || ' reclassified');
        End If;
                 
      End Loop;
   End Loop;
   
--dbms_output.put_line('AT BEGIN ' || Class_it(347) ||Class_it(348));
   
-- Classify partial pipes 


   
   -- For each segment in each ring: 
   last_coord :=0;
   For ring in 1..ring_count Loop
      first_coord := last_coord+1;   -- odd
      if ring <> ring_count then
         last_coord := Info(ring*3+1) -1;   -- even
      else
         last_coord := n;
      end if;
      find_info(first_coord);
      
-- Loop over segment numbers, from the start_segment to the end_segment
-- So we are processing the vertices as segments

      For vtx in TRUNC(first_coord/2)+1..TRUNC(last_coord/2)-1  loop
  
-- Classify partly overlapped pipes as 'u' - part of a ring
--      If Pipe_overlap(vtx) > 0.0 and  Pipe_Overlap(vtx) < 0.7 and Pipe_NOL(vtx) > max_width then
--         Class_it(vtx) := 'u';    
--      End if;
      
-- Set current segment (vertex) and the segments ahead and behind

        current_seg := vtx;
        ahead := current_seg+1;
        behind := current_seg-1;
        if behind < istart then
           behind := iend;
        end if;
        more_behind := behind-1;
        if more_behind < istart then
           more_behind := iend;
        end if;
        if ahead > iend then
           ahead := istart;
        end if;
        more_ahead := ahead+1;
        if more_ahead > iend then
           more_ahead := istart;
         end if;
        more_more_ahead := more_ahead+1;
        if more_more_ahead > iend then
           more_more_ahead := istart;
         end if;
        more_more_more_ahead := more_more_ahead+1;
        if more_more_more_ahead > iend then
           more_more_more_ahead := istart;
         end if; 
        
        current_angle := Angles(vtx+1-ring);
        ahead_angle := Angles(ahead+1-ring);
--        dbms_output.put_line('vtx' ||vtx || 'ahead ' || ahead || ' more ' || more_ahead || ' ring ' || ring || ' ' || angles.count);
        more_ahead_angle := Angles(more_ahead+1-ring);
--        dbms_output.put_line('vtx' ||vtx || 'behind ' || behind || ' more ' || more_ahead || ' ring ' || ring);
        behind_angle := Angles(behind+1-ring);
        
        
-- Mark short corner segments of a pipe with a 'c' to indicate the corner
-- really is part of an extended pipe
--
--
--        |      |  <-- This is a pipe
--   -----+      |
--               +
--               |     
--     vtx1      |
--   -----+------+ vtx2   Near 90 degrees with 180 after (case 2) 
--    ^   Near 180 degrees with 90 after (case 1)             
--    |  
--    Pipe
--         if vtx >= 134 and vtx <= 136 then
--  dbms_output.put_line('at vertex ' ||vtx||class_it(vtx));
--end if;
         IF Class_it(vtx) <> 'P' THEN
         if vtx >= 160 and vtx <= 160 then
  dbms_output.put_line('L ' || round(Lengths(vtx+1-ring),3) || ' A ' || round(current_angle,3) || ' aH ' || round(ahead_angle,3) || 'MaH ' || round(more_ahead_angle,3)|| ' BH ' || round(behind_angle,3)|| Class_it(behind)||Class_it(ahead)||Class_it(more_ahead)||Class_it(more_more_ahead));
end if;
         
        if Lengths(vtx+1-ring) < max_width and Class_it(behind) = 'P' and
           check_angle(current_angle,180.) and check_angle(ahead_angle,90.) then          
           Class_it(vtx) := 'c';
--           dbms_output.put_line('>>>>>>>>>>cc'|| vtx);
        end if;
        if Lengths(vtx+1-ring) <max_width and Class_it(ahead) = 'P' and
          check_angle(current_angle,90.) and check_angle(ahead_angle,180.) then        
           Class_it(vtx) := 'c';
--           dbms_output.put_line('>>>>>>>>>>ccc'|| vtx);
        end if;
        if Lengths(vtx+1-ring) + Lengths(behind+1-ring) < max_width and Class_it(more_behind) = 'P' and
           check_angle(current_angle,180.) and check_angle(ahead_angle,90.) then  
           Class_it(vtx) := 'c';
--           dbms_output.put_line('>>>>>>>>>>ccd'|| vtx);
        end if;
        if Lengths(vtx+1-ring) + + Lengths(ahead+1-ring)< max_width and Class_it(more_ahead) = 'P' and
           check_angle(current_angle,90.)  then          
           Class_it(vtx) := 'c';
           if Class_it(ahead) <> 'P' then
             Class_it(ahead) := 'c';
           end if;
--           dbms_output.put_line('>>>>>>>>>>ccc'|| vtx);
        end if;
        
 --- This checks this strange case            ___
 --                                        __|
 --
--       if vtx >=523 and vtx <= 526 then
--         dbms_output.put_line(vtx||'LL ' || ROUND(Lengths(vtx+1-ring),3)|| ' ' ||ROUND(Lengths(ahead+1-ring),3)|| Class_it(behind) || round(current_angle,3) ||' ' || round(ahead_angle,3)||' '  || round(more_ahead_angle,3));
--       end if;
        if Lengths(vtx+1-ring) < 2*max_width and Class_it(behind) = 'P' and  Class_it(ahead) = 'P' and
           check_angle(current_angle,90.,2) and check_angle(ahead_angle,90.,2) then          
           Class_it(vtx) := 'c';
--           dbms_output.put_line('>>>>>**>>>>cC'|| vtx);
        end if;
-- Allow 2 segments
         if Lengths(vtx+1-ring) + Lengths(ahead+1-ring) < 2* max_width and (Class_it(behind) = 'P' or Class_it(more_ahead) = 'P') and
           check_angle(current_angle,90.,2) and check_angle(ahead_angle,180.,2) and check_angle(more_ahead_angle,90.,2) then          
           Class_it(vtx) := 'c';
--           dbms_output.put_line('>>>>>**>>>>cc'|| vtx);
        end if;
 -- Mark a cap on a Pipe (with one segment on the cap as shown)
 
--              +------+ vtx1
--              |      |  
--        Pipe  |      ^  Pipe 
--              |      |
--              |      |
--              +      +
--   
        if Lengths(vtx+1-ring) < max_width and Class_it(behind) = 'P' and
           Class_it(ahead) = 'P' and
           check_angle(current_angle,90.) and check_angle(ahead_angle,90.) then  
           Class_it(vtx) := 'C';
--           dbms_output.put_line('>>>>>>>>>>>CC'|| vtx);
        end if;
        
 -- Allow 2 segments on the cap       
        if Lengths(vtx+1-ring) + Lengths(ahead+1-ring) < max_width and Class_it(behind) = 'P' and
           Class_it(ahead) <> 'P' and Class_it(more_ahead) = 'P' and 
           check_angle(current_angle,90.) and check_angle(ahead_angle,180.) and
           more_ahead_angle > 90.-angle_epsilon  and
           more_ahead_angle < 90.+angle_epsilon then
           Class_it(vtx) := 'C';
--           dbms_output.put_line('>>>>>>>>>>CCC'|| vtx);
        end if;   
 -- Allow 2-4 segments on the cap
 
        Len_so_far :=  Lengths(vtx+1-ring);
        tmp := vtx;
--                   if vtx >= 193 and vtx <=195 then
--  dbms_output.put_line('vtx ' || vtx);
--end if;        
        for ij in 2..4 loop
           tmp := tmp +1;
           if tmp > iend then
             tmp := istart;
           end if;
           more_tmp := tmp+1;
           if more_tmp > iend then
             more_tmp := istart;
           end if;
           more_more_tmp := more_tmp+1;
           if more_more_tmp > iend then
             more_more_tmp := istart;
           end if;
           -- check the length
           Len_so_far := len_so_far + Lengths(tmp+1-ring);
--                               if vtx >= 572 and vtx <=575 then
--  dbms_output.put_line('at ' || (vtx+ij-1) || 'length'|| round(len_so_far,3) || ' tmp ' ||tmp || ' MT' || more_tmp || ' MMtmp ' || more_more_tmp || UPPER(Class_it(behind)) ||Class_it(more_more_tmp)||round(Angles(more_more_tmp+1-ring),3));
--end if;
           if len_so_far > max_width or Class_it(tmp) = 'P' then
--                    if vtx >= 572 and vtx <=575 then
--  dbms_output.put_line('exiting on length'|| round(len_so_far,3));
--end if;
              exit;
           end if;
           -- and check the angle continue across the cap
           temp := Angles(tmp+1-ring);
           if check_angle(temp,180.,1.5) = FALSE then
--                               if vtx >= 572 and vtx <=575 then
--  dbms_output.put_line('exiting on angle'|| round(temp,3));
--end if;
              exit;
           end if;
           
           If (Class_it(behind) = 'P' or Class_it(behind) = 'c') and Class_it(more_more_tmp) = 'P'and 
            check_angle(current_angle,90.) and check_angle(Angles(more_more_tmp+1-ring),90.) then
             Class_it(vtx) := 'C';
             if Class_it(tmp) = 'U' then
             Class_it(tmp) :='C';
             end if;
             if Class_it(more_tmp) = 'U' then
             Class_it(more_tmp) :='C';
             end if;
--           dbms_output.put_line('>>>>>>>>>>CCC'|| vtx);
           exit;
           end if;
        end loop; 
   
--
--  Mark part of a 'T':                |  |
--                                _____|  |____
--
--                                _____+..+_____
--
-- If the Left distance + length of the segment + Right distance <= Allowed
-- Pipe width, then we call this a 'T' part of a pipe
--
--       if vtx >=759 and vtx <= 760 then
--            dbms_output.put_line('AATTT vtx ' || vtx || ' ' || round(lengths(vtx+1-ring),3) || Class_it(behind) || Class_it(vtx) || Class_it(ahead));       
--        end if;
        
        if Lengths(vtx+1-ring) < max_width and Class_it(behind) = 'P' and
           Class_it(ahead) = 'P' and
           check_angle(current_angle,180.) and check_angle(ahead_angle,180.) then
           Class_it(vtx) := 'T';
--           dbms_output.put_line('>>>>>>>>>>>>>>>>>>>>>>>>>>>>TTT'||vtx);
        end if;
-- Allow there to be two segments in the gap        
        if Lengths(vtx+1-ring) + Lengths(ahead+1-ring)< max_width and Class_it(behind) = 'P' and
           Class_it(more_ahead) = 'P' and
           check_angle(current_angle,180.) and check_angle(ahead_angle,180.) and
           more_ahead_angle > 180.-angle_epsilon  and
           more_ahead_angle < 180.+angle_epsilon  then
           Class_it(vtx) := 'T';
           
--           dbms_output.put_line('<<<<<<<<<<<<<<<<<<<<<<<<<<TTT');
        end if; 
    -- Allow 2-4 segments in the gap
 
        Len_so_far :=  Lengths(vtx+1-ring);
        tmp := vtx;
 --                  if vtx >= 193 and vtx <=195 then
--  dbms_output.put_line('vtx ' || vtx);
--end if;        
        for ij in 2..4 loop
           tmp := tmp +1;
           if tmp > iend then
             tmp := istart;
           end if;
           more_tmp := tmp+1;
           if more_tmp > iend then
             more_tmp := istart;
           end if;
           more_more_tmp := more_tmp+1;
           if more_more_tmp > iend then
             more_more_tmp := istart;
           end if;
           -- check the length
           Len_so_far := len_so_far + Lengths(tmp+1-ring);
--                               if vtx >= 193 and vtx <=195 then
--  dbms_output.put_line('at ' || (vtx+ij-1) || 'length'|| round(len_so_far,3) || ' tmp ' ||tmp || ' MT' || more_tmp || ' MMtmp ' || more_more_tmp || UPPER(Class_it(behind)) ||Class_it(more_more_tmp)||round(Angles(more_more_tmp+1-ring),3));
--end if;
           if len_so_far > max_width or Class_it(tmp) = 'P' then
--                    if vtx >= 572 and vtx <=575 then
--  dbms_output.put_line('exiting on length'|| round(len_so_far,3));
--end if;
              exit;
           end if;
           -- and check the angle continue across the cap
           temp := Angles(tmp+1-ring);
           if check_angle(temp,180.,1.5) = FALSE then
--                               if vtx >= 572 and vtx <=575 then
--  dbms_output.put_line('exiting on angle'|| round(temp,3));
--end if;
              exit;
           end if;
           
           If (Class_it(behind) = 'P' or Class_it(behind) = 'c') and Class_it(more_more_tmp) = 'P'and 
            check_angle(current_angle,180.) and check_angle(Angles(more_more_tmp+1-ring),180.) then
             Class_it(vtx) := 'T';
             if Class_it(tmp) = 'U' then
             Class_it(tmp) :='T';
             end if;
             if Class_it(more_tmp) = 'U' then
             Class_it(more_tmp) :='T';
             end if;
--           dbms_output.put_line('>>>>>>>>>>ttt'|| vtx);
           exit;
           end if;
        end loop;      
        END IF;
      End Loop;
      Class_it(TRUNC(last_coord/2)) := 'N';
   End Loop;
--dbms_output.put_line('NOW ' || Class_it(323) || Class_it(324) ||class_it(325));
--------------------------------------------------------------------------------   
-- Remove all 'Chaff' with very short lengths between pipes
-- For each ring: 
--dbms_output.put_line('AT NOW ' || Class_it(787) );
   last_coord := 0;
   For ring in 1..ring_count Loop
      first_coord := last_coord+1;   -- odd
      if ring <> ring_count then
         last_coord := Info(ring*3+1) -1;   -- even
      else
         last_coord := n;
      end if;
      find_info(first_coord);
      
-- Loop over segment numbers, from the start_segment to the end_segment
-- So we are processing the vertices as segments

      For vtx in TRUNC(first_coord/2)+1..TRUNC(last_coord/2)-1  loop
      
-- Set current segment (vertex) and the segments ahead and behind

        current_seg := vtx;
        ahead := current_seg+1;
        behind := current_seg-1;
        if behind < istart then
           behind := iend;
        end if;
        
        if ahead > iend then
           ahead := istart;
        end if;
        
-- Change Chaff to 'T' when ahead and behind not equal to 'U'
--        if vtx >=193 and vtx <= 195 then
--            dbms_output.put_line('at vtx ' || vtx || ' ' || round(lengths(vtx+1-ring),3) || Class_it(behind) || Class_it(vtx) || Class_it(ahead));       
--        end if;

        If Class_it(vtx) = 'U' and Lengths(vtx+1-ring) < max_width and
           Class_it(behind) <> 'U' and Class_it(ahead) <> 'U' then
           Class_it(vtx) := 'T';
--           dbms_output.put_line('!!!!!! changed ' || vtx);
        End If;
        
        
         
        Len_so_far :=  Lengths(vtx+1-ring);
        tmp := vtx;
--                   if vtx >= 522 and vtx <=526 then
--  dbms_output.put_line('vefore chaff vtx ' || vtx|| Class_it(vtx));
--end if; 

-- Now check for consecutive pieces of Chaff

        for ij in 2..4 loop
           tmp := tmp +1;
           if tmp > iend then
             tmp := istart;
           end if;
           more_tmp := tmp+1;
           if more_tmp > iend then
             more_tmp := istart;
           end if;
           more_more_tmp := more_tmp+1;
           if more_more_tmp > iend then
             more_more_tmp := istart;
           end if;
           -- check the length
           Len_so_far := len_so_far + Lengths(tmp+1-ring);
--                               if vtx >= 193 and vtx <=195 then
--  dbms_output.put_line('at ' || (vtx+ij-1) || 'length'|| round(len_so_far,3) || ' tmp ' ||tmp || ' MT' || more_tmp || ' MMtmp ' || more_more_tmp || UPPER(Class_it(behind)) ||Class_it(more_more_tmp)||round(Angles(tmp+1-ring),3));
--end if;
           if len_so_far > max_width or Class_it(tmp) = 'P' then
--                    if vtx >= 193 and vtx <=195 then
--  dbms_output.put_line('exiting on length'|| round(len_so_far,3));
--end if;
              exit;
           end if;
           -- and check the angle continue across the gap
           temp := Angles(tmp+1-ring);
           if check_angle(temp,180.,1.5) = FALSE then
--                               if vtx >= 193 and vtx <=195 then
--  dbms_output.put_line('exiting on angle'|| round(temp,3));
--end if;
              exit;
           end if;
           
           If (Class_it(behind) = 'P' or Class_it(behind) = 'c') and Class_it(more_more_tmp) = 'P' then 
             Class_it(vtx) := 'T';
             if Class_it(tmp) = 'U' then
             Class_it(tmp) :='T';
             end if;
             if Class_it(more_tmp) = 'U' then
             Class_it(more_tmp) :='T';
             end if;
--           dbms_output.put_line('>>>>>>>>>>>>>>>>>>>>>>>>>ttt'|| vtx);
           exit;
           end if;
        end loop;      
      End Loop;
  End Loop;
--dbms_output.put_line('AT END ' || Class_it(787) );
--   For ii in 1..Pipe_Overlap.count Loop
--      if  (Pipe_overlap(ii) > 0.0 and Pipe_Overlap(ii) < 0.999) or pipe_overlap(ii) > 1.01 then
--         dbms_output.put_line('At Pipe' || ii || ' overlap ' || round(Pipe_overlap(ii),3) || ' ' || Class_it(ii) || ' ' || round(Pipe_NOL(ii),3));
--      end if;
 --     If Pipe_overlap(ii) > 0.0 and  Pipe_Overlap(ii) < 1. then
 --        Class_it(ii) := 'U';    
 --     End if;
--   end loop;

-- dbms_output.put_line('<<<<<<<>>>> are U there Oracle?');
-- Now mark all the ring starts (pipe ends) with a 'p'
    
  last_coord := 0;
   For ring in 1..ring_count Loop
      first_coord := last_coord+1;   -- odd
      if ring <> ring_count then
         last_coord := Info(ring*3+1) -1;   -- even
      else
         last_coord := n;
      end if;
      find_info(first_coord);
      
-- Loop over segment numbers, from the start_segment to the end_segment
-- So we are processing the vertices as segments

      For vtx in TRUNC(first_coord/2)+1..TRUNC(last_coord/2)-1  loop
      
-- Set current segment (vertex) and the segments ahead and behind

        current_seg := vtx;
        ahead := current_seg+1;
        behind := current_seg-1;
        if behind < istart then
           behind := iend;
        end if;
        
        if ahead > iend then
           ahead := istart;
        end if;
        
-- Classify poorly overlapped pipes as 'U'
--     If Pipe_overlap(vtx) > 0.0 and  Pipe_Overlap(vtx) < 0.7 then
--         Class_it(vtx) := 'p';    
--      End if;

        result := Find_it(vtx,Pipe_pars);
        pi := result(2);
--        If Pipe_NOL(vtx) > max_width and Class_it(vtx) = 'P' then
--          dbms_output.put_line(vtx ||' NOL ' || round(Pipe_NOL(vtx),3));       
--        End if;
--        if vtx = 760 then
--           dbms_output.put_line('<><>< '||vtx ||' ' || behind || Class_it(behind) || Class_it(vtx)||class_it(ahead));
--        end if;
        If (vtx > 1 and Class_it(vtx) = 'U' and Class_it(vtx-1) = 'N')  then
--            (vtx = Class_it.count-1 and Class_it(vtx) = 'U') then
        
--         dbms_output.put_line('<><><><><> set Ring start ' || vtx||Class_it(behind) || Class_it(vtx)||class_it(ahead)|| ' Link_back ' || Pipe_pars(pi).link_bck ||',' ||Pipe_pars(pi).link_fwd);
           Class_it(vtx) := 'R';
-- Oct 18 was
--        ElsIf (Class_it(vtx) <> 'U' and Class_it(behind) = 'U') then
        ElsIf (Class_it(vtx) = 'P' and Class_it(behind) = 'U') then
          
--            dbms_output.put_line('$$$$$$$$ set Ring End ' || vtx||Class_it(behind) || Class_it(vtx)||class_it(ahead)); --|| ' Link_back ' || Pipe_pars(pi).link_bck ||',' ||Pipe_pars(pi).link_fwd);
           Class_it(vtx) := 'E';

--Oct 18 
--      Elsif (Class_it(vtx) = 'U' and (Class_it(behind) ='P' or Class_it(behind) ='c')) then --or (Class_it(vtx) ='c' and Class_it(behind) ='P') then
        Elsif (Class_it(vtx) = 'U' and (Class_it(behind) ='P' or Class_it(behind) ='c' or Class_it(behind) ='E')) then --or (Class_it(vtx) ='c' and Class_it(behind) ='P') then
--        Elsif (Class_it(vtx) = 'U' and Class_it(behind) <> 'U' and Class_it(behind) <> 'p' ) then

        result := Find_it(vtx,Pipe_pars,'N');
        what := result(1);
        pi := result(2);
        if pi <> 0 then
--          dbms_output.put_line('$$$$$$$$ Set ring start ' || vtx|| Class_it(vtx)||Class_it(behind)) ; --|| ' Link_back ' || Pipe_pars(pi).link_bck ||',' ||Pipe_pars(pi).link_fwd);

           Class_it(vtx) := 'S';
--Oct 18 next 2 lines
--        elsif Class_it(behind) = 'c' then
--          Class_it(behind) := 'S';
        
        end if;
-- Handle 2 consecutive start and ends
           if Class_it(ahead) = 'P' and Lengths(vtx+1-ring) > max_width then
           Class_it(ahead) := 'E';
           end if;
           
        End If;
 --       If (Class_it(vtx) <> 'U' and Class_it(vtx) <> 'p' and Class_it(ahead) = 'U') then
 --          Class_it(ahead) := 'p';
 --          dbms_output.put_line('$$$$$$$$ Set ring start ' || vtx);
 --       End If;
 
 --     We can make a consistency check since the start and end have to be on 
 --     the same ring. Mark last ring with a +.1 etc once a start and end found.
  
        If Class_it(vtx) = 'S' or Class_it(vtx) = 'E' then
           if last_ring = 0.0 or last_ring - TRUNC(last_ring) = 0.1 then
--             if last_ring <> 0.0 then
--             dbms_output.put_line('>>>SUCCESS paired ring at vertex ' || last_vtx||' last_ring '||last_ring);
--             end if;
             last_ring := ring;
--             dbms_output.put_line('>>>Starting new ring '||ring ||' at vertex ' || vtx);
           elsif ring = TRUNC(last_ring) then
             last_ring := last_ring +0.1;
--             dbms_output.put_line('>>>Continuing ring '||ring ||' at vertex ' || vtx);
           else             
            dbms_output.put_line('>>>Problem at vertex ' || vtx||' last_ring '||last_ring);
            last_ring := ring;
           end if;
           last_vtx := vtx;
        End if;
       
      End Loop;
  End Loop;
  
--  dbms_output.put_line('<<<<<<<>>>> are u there Oracle?');
  

  
-- Segments 1 thru Pipe_pars(1).Seg are ring parts

 
  Used.extend(Class_it.count);
  For ii in 1..Used.count Loop
     Used(ii) := 0.0;
  End Loop;


-- We can mark all of the outside rings that don't involve any pipe segments
-- as ring vertices

  For ii in 1..TRUNC(Info.count/3) loop
    If Info(ii*3-1) = 1003 then
      istart := TRUNC((Info(ii*3-2)+1)/2);
      if ii < TRUNC(Info.count/3) then
         iend := TRUNC((Info(ii*3+1)-1)/2);
      else
         iend := TRUNC(Xys.count/2);
      end if;
      For ij in istart..iend Loop
        vtx := ij;      
        result := Find_it(vtx,Pipe_pars);
        pi := result(2);
       exit when pi <> 0;
      End Loop;
      if pi <> 0 then
      For ij in istart..iend Loop
         If Class_it(ij) = 'U' then
            Class_it(ii) := 'r';
            Used(ii) := 1.0;
         ElsIf Class_it(ij) = 'R' then
            Used(ii) := 1.0;
         End if;
      End Loop;
      end if;
    End if;
  End Loop;

  For vtx in 1..Class_it.count Loop
    If Class_it(vtx) = 'R' or Class_it(vtx) = 'r' or Class_it(vtx) = 'u' then
       p_count := p_count +1;
--       dbms_output.put_line(p_count || ' at vtx ' || vtx || Class_it(vtx));
    End If;
  End Loop;
  Pipe_ends.extend(p_count);
  For vtx in 1..Class_it.count Loop
    If Class_it(vtx) = 'p' or Class_it(vtx) = 'r' or Class_it(vtx) = 'u' then
       p_indx := p_indx +1;
       Pipe_ends(p_indx) := vtx;
    End If;
  End Loop;
  
   For vtx in 1..Class_it.count Loop
   dbms_output.put_line(vtx || class_it(vtx));
   end loop;
--  dbms_output.put_line('>>>P_count ' || p_count);
  
-- Determine the ring jump vertices, where a ring begins or ends  

   RING_JUMP:= Build_Rings(Pipe_pars,Class_it,Xys,Info,Used);
   Return Class_it;


  last_ring := 0;
  ring := 1;
  seg_no := 0;
  last_rpos :=0;
  For ii in 1..2*Class_it.count Loop
    seg_no := MOD(seg_no,Class_it.count)+1;
    ring := Find_ring(seg_no*2);
    If Used(seg_no) = 0. and Class_it(seg_no) = 'S' then
--    dbms_output.put_line('IN this ring '||ring || ' seg ' ||seg_no);
    ring_no := Find_ring(seg_no*2);
-- We need to walk arbitrarily around each ring and manipulate the
-- vertex (vtx) we are at.Note if we start at vertex 1 at the diagram below,
-- that we will complete the biggest ring before moving onto the other smaller rings.
 
-- Sometimes we may have a short segment that has been marked as a 'c'
-- 

       vtx := seg_no;   
       Used(vtx) := 1.0;     -- we can go to later parts of the polygon 
--                              so the vertices are already used.

-- So with the start we expect to use the end of Seg and the beginning of Mseg
--  and vice versa
--                 -----------------------
--         Mseg   /                       \  Mseg
--    -------<---E                         S--<------------
--    |   1-->--S                           E->-----      |
--    |   | Seg  \                         /  Seg   |     |
--    |   |       -------------------------         |_____|
--    |___|
--                Start (S)  to End (E) on one ring 


-- First check Seg, and then Mseg
        rpos := rpos +1;
        
        result := Find_it(vtx,Pipe_pars,'N');
        what := result(1);
        pi := result(2);
        at_start_end := result(3);   -- extract whether at start or end;
--     dbms_output.put_line('vtx ' || vtx || ' pi ' || pi );   
  if pi =0 then
  
     pi := 1/0;
  end if;
      
      Projector := Pipe_pars(pi).projector;
      mseg := Pipe_pars(pi).mseg;
      seg := Pipe_pars(pi).seg;
--      dbms_output.put_line('vtx ' || vtx || ' pi ' || pi || ' P ' || projector || ' mseg ' || mseg || ' at sE ' || at_start_end);
-- Set up the 1st vertex in the ring, the jump vertex across the pipe so the ring 
-- may close. Also set the vertex before so we know what this created vertex
-- attaches to.
      if last_rpos = 0 then
      last_rpos := rpos;
      if at_start_end < 3 then  -- this means we found vtx
      if projector = 12 or projector = 42 then
        if Pipe_pars(pi).Xq <> Xys(mseg*2-1) and  Pipe_pars(pi).Yq <> Xys(mseg*2) then
--        dbms_output.put_line('heeere');
       Ring_jump(rpos).x := Xys(mseg*2-1);
        Ring_jump(rpos).y := Xys(mseg*2);
        rpos := rpos+1;
        end if;
        Ring_jump(rpos).x := Pipe_pars(pi).Xq;
        Ring_jump(rpos).y := Pipe_pars(pi).Yq;
        Ring_jump(rpos).start_vertex := mseg;  -- point before is vertex mseg
      Elsif projector = 13 or projector = 43 then
        Ring_jump(rpos).x := Xys(mseg*2-1);
        Ring_jump(rpos).y := Xys(mseg*2);
        Ring_jump(rpos).start_vertex := mseg;  -- redundant but copier will ignore it
      end if;
      else
        
       if projector = 42 or projector = 43 then
       if Pipe_pars(pi).Xp <> Xys(seg*2-1) and  Pipe_pars(pi).Yp <> Xys(seg*2) then
       Ring_jump(rpos).x := Xys(seg*2-1);
        Ring_jump(rpos).y := Xys(seg*2);
        rpos := rpos+1;
        end if;
        Ring_jump(rpos).x := Pipe_pars(pi).Xp;
        Ring_jump(rpos).y := Pipe_pars(pi).Yp;
        Ring_jump(rpos).start_vertex := seg;  
        dbms_output.put_line('sssset start ' || xstart || ', ' || ystart || ' pi ' || pi);
      Elsif projector = 12 or projector = 13 then
        Ring_jump(rpos).x := Xys(seg*2-1);
        Ring_jump(rpos).y := Xys(seg*2);
        Ring_jump(rpos).start_vertex := seg;
      end if;
      
      end if;
      
-- Set the ring, we must start and end each portion within the same ring
      
      
-- Now set the 2nd vertex - the one across from the start and continuing the ring
      
      xstart := Ring_jump(last_rpos).x;
      ystart := Ring_jump(last_rpos).y;
       dbms_output.put_line('>>>>>>>set start ' || round(xstart,8) || ', ' || round(ystart,8) || ' rpos ' ||rpos);
       if rpos = 2 then
        dbms_output.put_line('>>>>>>>at 2 ' || ring_jump(rpos).x || ', ' || ring_jump(rpos).y || ' rpos ' ||rpos);
       end if;
      rpos := rpos+1;
      end if;
      if at_start_end < 3 then
      if projector = 12 or projector = 42 then
        Ring_jump(rpos).x := Xys(seg*2+1);
        Ring_jump(rpos).y := Xys(seg*2+2);
        Ring_jump(rpos).next_vertex := seg+1;
        dbms_output.put_line('set across from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      Elsif projector = 13 or projector = 43 then
        Ring_jump(rpos).x := Pipe_pars(pi).Xq;
        Ring_jump(rpos).y := Pipe_pars(pi).Yq;
        Ring_jump(rpos).next_vertex := seg+1;
         dbms_output.put_line('set Qacross from Start ' || round(Ring_jump(rpos).x,8)||','||round(Ring_jump(rpos).y,8));
      end if;
      else
       if projector = 12 or projector = 13 then
        Ring_jump(rpos).x := Pipe_pars(pi).Xp;
        Ring_jump(rpos).y := Pipe_pars(pi).Yp;
        Ring_jump(rpos).next_vertex := mseg+1;
         dbms_output.put_line('set Across from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      Elsif projector = 42 or projector = 43 then
        Ring_jump(rpos).x := Xys(mseg*2+1);
        Ring_jump(rpos).y := Xys(mseg*2+2);
        Ring_jump(rpos).next_vertex := mseg+1;
         dbms_output.put_line('set QAcross from start ' || round(Ring_jump(rpos).x,8)||','||round(Ring_jump(rpos).y,8));
      end if;
      
      end if;
      rpos := rpos+1;
      Ring_jump(rpos).start_vertex := vtx;
      dbms_output.put_line('set an start vertex ' || vtx);
      For ij in 1..Class_it.count loop
      seg_no := seg_no + 1;

         vtx := MOD(seg_no-1,Class_it.count)+1;
         if vtx >= 895 then
          dbms_output.put_line(seg_no||'vertex ' || vtx || 'rpos ' ||rpos||' ' ||used(vtx));
end if;

-- Sometimes the last ring in Info does not begin at vertex 1

        ring := Find_ring(vtx*2);
        if vtx = 1 and ring <> ring_no then
          ring_dnsat_one := TRUE;    -- ring does not start at 1
        elsif vtx = 1 then
        ring_dnsat_one := FALSE;
        end if;
         if Used(vtx) = 0. and Class_it(vtx) = 'E' and ring = ring_no then
         
-- Sometimes the last ring in Info does not begin at vertex 1 so we have to set
-- another end and start

            If ring_dnsat_one then
            ring_dnsat_one := FALSE;
             Find_Info(vtx*2);
             Used(last_vtx) := 1.;
            Ring_jump(rpos).next_vertex := iend;
dbms_output.put_line('set an end  vertex ' || vtx);
            rpos := rpos + 1;
            Ring_jump(rpos).start_vertex := istart;
dbms_output.put_line('set a beginning  vertex ' || vtx);
            
            End If;
            
            Used(vtx) := 1.;
            Ring_jump(rpos).next_vertex := vtx;
dbms_output.put_line('set an end  vertex ' || vtx);
        
            result := Find_it(vtx,Pipe_pars);       
            pi := result(2);
            at_start_end := result(3);   -- extract whether at start or end;
      
-- If we cannot find it in the pipe description then assume the projector is 12
            if pi = 0 then
              Projector := 12;
            else
      Projector := Pipe_pars(pi).projector;
            end if;
      dbms_output.put_line('vtx ' || vtx || Class_it(vtx) || ' pi ' || pi || ' P ' || projector || ' at sE ' || at_start_end);
-- Is it a ring piece start or a ring piece end?
--dbms_output.put_line('vtx ' ||xys(vtx*2-1) || ',' || Xys(vtx*2) || ' xst ' || xstart ||','||ystart);
           if Xys(vtx*2-1) = xstart and Xys(vtx*2) = ystart then
                 last_rpos := 0;
   
      
--            elsif projector = 13 or projector = 12 then
              
--              Ring_jump(rpos).x := Xys(vtx*2-1);
--              Ring_jump(rpos).y := Xys(vtx*2);
            elsif projector = 43 or projector = 42 then
              rpos := rpos+1;
              Ring_jump(rpos).x := Pipe_pars(pi).Xp;
              Ring_jump(rpos).y := Pipe_pars(pi).Yp;
              Ring_jump(rpos).start_vertex := seg;
               dbms_output.put_line('set an end ' || Ring_jump(rpos).x || ',' || Ring_jump(rpos).y);
              if Ring_jump(rpos).x = xstart and Ring_jump(rpos).y = ystart then
                 last_rpos := 0;
              end if;
            end if;
-- Jump across the gap and exit when we have completed a ring -> to find another start
            
           
            if last_rpos = 0 then
               dbms_output.put_line('^^^^^^^^^^^^^^^^^^^^^^COMPleted a ring'||rpos);
               no_rings := no_rings+1;
            end if;
            exit when last_rpos=0;
-- When we don't exit, continue building this ring
      rpos := rpos+1;
      dbms_output.put_line('Now rpos ' || rpos|| ' ' || at_start_end);
      if at_start_end < 3 then
      dbms_output.put_line('Hello');
      seg_no := Pipe_pars(pi).mseg;
      dbms_output.put_line('Hello oracle '||projector);
      if projector = 12 or projector = 13 then
        Ring_jump(rpos).x := Pipe_pars(pi).Xp;
        Ring_jump(rpos).y := Pipe_pars(pi).Yp;
        Ring_jump(rpos).next_vertex := seg_no+1;
        dbms_output.put_line('set across from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      Elsif projector = 42 or projector = 43 then
        Ring_jump(rpos).x := Xys(seg_no*2+1);
        Ring_jump(rpos).y := Xys(seg_no*2+2);
        Ring_jump(rpos).next_vertex := seg_no+1;
         dbms_output.put_line('set Qacross from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      end if;
      else
      seg_no := Pipe_pars(pi).seg;
      dbms_output.put_line('Hello ooracle ');
       if projector = 12 or projector = 13 then
        Ring_jump(rpos).x := Xys(vtx*2-1);
        Ring_jump(rpos).y := Xys(vtx*2);
        Ring_jump(rpos).next_vertex := seg_no+1;
         dbms_output.put_line('set Across from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      Elsif projector = 42 or projector = 43 then
        Ring_jump(rpos).x := Pipe_pars(pi).Xp;
        Ring_jump(rpos).y := Pipe_pars(pi).Yp;
        Ring_jump(rpos).next_vertex := seg_no+1;
         dbms_output.put_line('set QAcross from start ' || Ring_jump(rpos).x||','||Ring_jump(rpos).y);
      end if;
        end if;
        seg_no := Ring_jump(rpos).next_vertex;
        ring_no := Find_ring(seg_no*2);
         dbms_output.put_line('jumped across to MSEG ' || (seg_no));
        rpos := rpos+1;
      Ring_jump(rpos).start_vertex := seg_no;
      Used(seg_no) := 1.0;
      seg_no := seg_no-1;
      dbms_output.put_line(vtx||'set another start vertex ' || seg_no || ' rpos ' || rpos|| ' class ' || class_it.count); 
      end if;
      End Loop;
      
    End If;
--    exit when no_rings = 1;
--    exit when seg_no > Class_it.count;
  End Loop;
  
-- Handle incomplete rings

  if last_rpos <> 0 then
    for ii in reverse last_rpos..rpos loop
      Ring_jump.delete(ii);
    end loop;
      rpos := last_rpos-1;
  end if;

 dbms_output.put_line('RPOS ' || rpos || ' last ' || last_rpos);
  for ii in 1..rpos loop
    if Ring_jump(ii).x is not NULL then
       dbms_output.put_line(ii||'ring jump XY ' || Ring_jump(ii).x || ',' || Ring_jump(ii).y); 
    else
       dbms_output.put_line(ii||'ring jump range ' || Ring_jump(ii).start_vertex || ' to ' || Ring_jump(ii).next_vertex);
    end if;
  end loop;
  
  RETURN Class_it;   
   
END Classify_Vertices;
--
FUNCTION GET_PIPELESS(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., pminimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 65.) RETURN MDSYS.SDO_GEOMETRY  AS

  New_Geom              MDSYS.SDO_GEOMETRY;
  Pipe_pars             Pipes_type;
  Class_it              Char_Array := Char_Array();
  Ring_Jump             XY_List;
  hi_width              NUMBER;
  minimum_len           NUMBER := pminimum_len;
  area_cutoff           NUMBER := pminimum_len*pminimum_len;
  pipe_vertex_count     PLS_INTEGER :=0;
  pipe_jump_count       PLS_INTEGER :=0;   -- where we jump across to close rings
  pipe_pct_count        PLS_INTEGER :=0;   --pipe shared ('p' and corners 'c' and 'T'

BEGIN

   -- Next line does automatic selection of the pipe width
--   hi_width := get_pipe_width(Geom,max_width,'HIGH');
--   dbms_output.put_line('hi_width ' || hi_width);
   
   hi_width := max_width;
   
   If minimum_len = 0.0 then
     minimum_len := hi_width*0.5;
     area_cutoff := minimum_len*minimum_len;
   End If;
   
   dbms_output.put_line('HW ' || max_width || ' mL ' || minimum_len || 'aspect ratio ' || aspect_ratio || ' AC ' || area_cutoff);
   Pipe_Pars := Find_Get_Pipes(Geom,pipe_type,hi_width,minimum_len,aspect_ratio,max_angle_diff);

   Class_it := Classify_Vertices(Pipe_pars,Ring_Jump,Geom,max_width);
   


   for ii in 1..Class_it.count loop
      if Class_it(ii) <> 'U' and Class_it(ii) <> 'P' then
        pipe_pct_count := pipe_pct_count+1;
      end if;
      if Class_it(ii) = 'P' then
        pipe_vertex_count := pipe_vertex_count+1;
      end if;
      if Class_it(ii) = 'p' then
        pipe_jump_count := pipe_jump_count+1;
        dbms_output.put_line('Jump vertex at ' || ii);
      end if;
   end loop;
   dbms_output.put_line('Total number of pipe vertices ' || (pipe_vertex_count+ Pipe_pct_count)|| ' Strictly Pipes ' ||pipe_vertex_count);
   dbms_output.put_line('Total number of jump vertices ' || pipe_jump_count);
   

   
   New_geom := Ring_Builder(Ring_Jump,Geom,area_cutoff);
   RETURN New_Geom;
   
END Get_PipeLess;
--
Function Ring_Builder(Ring_Jump XY_List,Geom MDSYS.SDO_GEOMETRY,Area_cutoff NUMBER default 0.0) RETURN MDSYS.SDO_GEOMETRY AS

-- Using the Ring_Jump description of the Rings, build all the rings into a geometry
-- Ring Jump description consister of either 1) a range of start and end vertices
--                                        or 2) an individual vertex x,y


   Xys             MDSYS.SDO_ORDINATE_ARRAY := Geom.Sdo_Ordinates;
   Info            MDSYS.SDO_ELEM_INFO_ARRAY := Geom.Sdo_Elem_info;

   New_Xys         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   New_Info        MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY();
   New_Geom        MDSYS.SDO_GEOMETRY;

   Areas           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   
   area            NUMBER;
   last_area       NUMBER;
   xstart          NUMBER;
   ystart          NUMBER;
   x               NUMBER;
   y               NUMBER;
   Kind            NUMBER := 2003.;
   from_a          PLS_INTEGER;
   to_b            PLS_INTEGER;
   pos             PLS_INTEGER := -1;
   last_ring       PLS_INTEGER := 0;
   indx            PLS_INTEGER := 0;
   ring            PLS_INTEGER := 1;  
   fpos            PLS_INTEGER := 0;   
   next           PLS_INTEGER := 0;
   no_holes        PLS_INTEGER := 0;
   inside          PLS_INTEGER;
   ibeg            PLS_INTEGER;
   ring_start      PLS_INTEGER :=0;
   Info_count      PLS_INTEGER := TRUNC(Info.count/3);
   first_in_ring   PLS_INTEGER;
   last           PLS_INTEGER;
   trap            BOOLEAN := FALSE;
  
   function find_and_mark_info(coord PLS_INTEGER) return pls_integer as

-- Mark a 1003 ring as used (set it negative)

 
     first        pls_integer;
     
     begin
   -- Mark a ring as being used 
           for ij in 1..Info_count loop
            first := Info(ij*3-2);
            If ij <> Info_count then
               last := Info(ij*3+1)-1;
            else
               last := Xys.count;
            end if;
--             dbms_output.put_line('Info ' || Info(ij*3-1) || ' coord ' || coord ||' info ' || info(ij*3-2) || ' last ' || last);
            if coord >= Info(ij*3-2) and coord <= last then
--             dbms_output.put_line('marked info ' || info(ij*3-1));
              if info(ij*3-1) = 1003 then
               Info(ij*3-1) := -ABS(Info(ij*3-1));
               end if;
               return first;
            end if;
         end loop;
     end;
  procedure copy_xys(pstart pls_integer,pend pls_integer,first pls_integer) as
  
-- Copy XYs either forward or backward, without dups.
-- We never copy Xys backwards as inner rings go in the opposite
-- direction from outer rings.

   istart       pls_integer := pstart;
   iend         pls_integer := pend;
   
   begin
   
 -- Forward: istart should be odd and iend even
 
      if istart < iend then
      
        if next > 1 and New_Xys(next-1) = Xys(istart) and
                       New_Xys(next) = Xys(istart+1) then
         istart := istart + 2;          
        end if;
        
        for ii in istart..iend loop
          next := next+1;
          New_Xys(next) := Xys(ii);
        end loop;
        
      elsif istart > iend+1 then


        if next > 1 and New_Xys(next-1) = Xys(istart) and
                       New_Xys(next) = Xys(istart+1) then
         istart := istart + 2;          
        end if;
        
        for ii in istart..last loop
          next := next+1;
          New_Xys(next) := Xys(ii);
        end loop;
        istart := first;  -- Start at the beginning of the ring !!!
        if next > 1 and New_Xys(next-1) = Xys(istart) and
                       New_Xys(next) = Xys(istart+1) then
         istart := istart + 2;          
        end if;
        
        for ii in istart..iend loop
          next := next+1;
          New_Xys(next) := Xys(ii);
        end loop;        
/*      
      if next > 1 and New_Xys(next-1) = Xys(iend-1) and
                       New_Xys(next) = Xys(iend) then
         iend := iend + 2;          
        end if;
        dbms_output.put_line('COPYYING in REVERSE'|| istart || ',' ||iend);
-- Backward: say we go from 10..1   
       for ii in reverse iend..istart loop     -- 

          if mod(ii,2) = 0 then   -- get a Y
            next := next+2;
            New_Xys(next) := Xys(ii);
          else                    -- get an X
            New_Xys(next-1) := Xys(ii);
          end if;

       end loop; 
*/
      end if;
   end;
BEGIN

   dbms_output.put_line('Number of vertices '|| (Xys.count/2) || ' Number of rings ' || Trunc(Info.count/3));
   Areas := Polygon_Area(Geom);
   
   
   for ii in 1..Areas.count Loop
     dbms_output.put_line('Area ' || round(Areas(ii),2));
     if Areas(ii) < 0.0 then
        no_holes := no_holes + 1;
     end if;
   end loop;
   dbms_output.put_line('Area cutoff' || area_cutoff);

-- Ring jump must include all the vertices to be copied, not just the jump vertices.


   New_Xys.extend(Xys.count*4);
   While indx < Ring_jump.count loop
   
        if ring <> last_ring then
          fpos := fpos+3;
          New_Info.extend(3);
          New_info(fpos-2) := next+1;
          New_info(fpos-1) := 1003;
          New_info(fpos) := 1;
 dbms_output.put_line('start new ring at ' || (next+1));

          trap := TRUE;  -- trap to remember first pair of coordinates
          last_ring := ring;
          ring_start := next+1;
        end if;

--Either copy a single vertex         
        indx := indx +1;
        If Ring_jump(indx).x is not NULL then
          x := Ring_jump(indx).x;
          y := Ring_jump(indx).y;
          if next = 0 or (next > 1 and (x <> New_Xys(next-1) or y <> New_Xys(next))) then
          next := next +2;
          New_Xys(next-1) := x;
          New_Xys(next) := y;
          dbms_output.put_line('copied single vertex  ' || next);
          end if;
-- or a range of vertex(es)          
        Else
          from_a := Ring_jump(indx).start_vertex*2-1;
          to_b   := Ring_jump(indx).next_vertex*2;
          -- keep track that this outer ring has been used
          first_in_ring := find_and_mark_info(from_a);
          x := Xys(from_a);
          y := Xys(from_a+1);
          dbms_output.put_line('copied from  ' || from_a|| ' to '||to_b || ' next ' || next);
          Copy_Xys(from_a,to_b,first_in_ring);  -- Copy a range of vertices or a single vertex
          dbms_output.put_line('copied from  ' || New_xys(next-1)|| ' to '|| New_Xys(next) || ' next ' || next);
        End if;
         dbms_output.put_line('next ' || next);
-- Save the start of each ring to check we close it
        if trap then
          xstart := x;
          ystart := y;
          dbms_output.put_line('xstart ' || xstart ||','|| ystart);
          trap := FALSE;
-- Have we finished a ring?
        elsif New_Xys(next-1) = xstart and New_Xys(next) = ystart then
         dbms_output.put_line('Finished RING ' || ring || ' indx ' || indx || ' RJ ' || Ring_jump.count);

-- Check the area is greater than the area cutoff

           area := Ring_Area(New_Xys,8265.,ring_start,next);
           dbms_output.put_line('area ' || area || ' cut ' || area_cutoff);        
           if area <= area_cutoff then   
              fpos := fpos-3;
              New_Info.trim(3);
              next := ring_start-1;
              last_ring :=0;
           else
           ring := ring + 1;

-- Ring is finished but it may contain a doughnut hole
           if no_holes > 0 then
             For ij in 1..TRUNC(Info.count/3) loop
                if Info(ij*3-1) = 2003 then
                  ibeg := Info(ij*3-2);
                  x := Xys(ibeg);
                  y := Xys(ibeg+1);
                  inside := GZ_QA.POINT_IN_POLY_CC(x,y,New_xys,ring_start,next);
--      dbms_output.put_line('inside ' || inside || ' xc ' || xc || ' yc ' || yc);
                     
                   first_in_ring:= find_and_mark_info(ibeg);
                   if ij = TRUNC(Info.count/3) then
                     to_b := Xys.count;
                   else
                     to_b := Info(ij*3+1)-1;
                   end if;
                   
-- Check on the area before copying small holes

                   area := Ring_Area(Xys,8265.,ibeg,to_b);
                   dbms_output.put_line('check ring ' || ij || ' inside ' || inside || ' area ' || round(area,2)|| ' cut ' || area_cutoff);
                   if inside = 1 and ABS(area) > area_cutoff then                     
                     fpos := fpos+3;
                     New_Info.extend(3);
                     New_info(fpos-2) := next+1;
                     New_info(fpos-1) := 2003;
                     New_info(fpos) := 1;
                     dbms_output.put_line('start new hole ring at ' || (next+1) || ' Area ' || round(area,3));                    
                     Copy_Xys(ibeg,to_b,first_in_ring);                     
                   end if;
                end if;
             End loop;
           end if;
           end if;
        end if;
  
   end loop;
    For ij in 1..TRUNC(Info.count/3) loop
    dbms_output.put_line('Info ' || info(ij*3-1));
    end loop;
   
   -- Copy any unused outer rings that don't have pipes
   -- This will handle an island with a doughnut hole since its hole will come next.
/*     
   last_area := 0.0;
   For ij in 1..TRUNC(Info.count/3) loop

-- Only handle unmarked islands and their holes  (if any)

      if Info(ij*3-1) = -1003 then
        last_area := 0.0;  -- dont want any holes from this ring...
      end if;
-- Dont copy tiny holes inside outer rings that did not meet cutoff
dbms_output.put_line('Ring # ' || ij || ' info ' || Info(ij*3-1) ||' last ' || last_area || ' cutoff ' || area_cutoff);
      if Info(ij*3-1) > 0 and (Info(ij*3-1) =1003 or (last_area > area_cutoff and Info(ij*3-1) =2003)) then

         ibeg := Info(ij*3-2);
         
         if ij = TRUNC(Info.count/3) then
           to_b := Xys.count;
         else
           to_b := Info(ij*3+1)-1;
         end if;
         first_in_ring:= find_and_mark_info(ibeg);
         
-- Check the area is not miniscule 

         area := Ring_Area(Xys,8265.,ibeg,to_b);
         dbms_output.put_line('Ring # ' || ij || ' area ' || area);
         if ABS(area) > area_cutoff then
           if ABS(Info(ij*3-1)) = 1003 then
              last_area := area;
           end if;
           --dbms_output.put_line('Ring # ' || ij || ' accepted');
           fpos := fpos+3;
           New_Info.extend(3);
           New_info(fpos-2) := next+1;
           New_info(fpos-1) := ABS(Info(ij*3-1));
           New_info(fpos) := 1;
           dbms_output.put_line('Start new ring without pipes at ' || (next+1) || ' Area ' || round(area,3));
           Copy_Xys(ibeg,to_b,first_in_ring);
         end if;
      end if;
   End loop;
*/ 
   If New_info.count > 3 then
     kind := 2007.;
   End if;
   New_xys.trim(New_Xys.count-next);
   New_geom := Mdsys.SDO_Geometry(kind,8265.,NULL,New_info,New_Xys);
   
   Return New_Geom;
   
END Ring_Builder;
--
PROCEDURE Make_Pipe_Pairs_table(In_Table VARCHAR2,face_id NUMBER, Out_Table VARCHAR2, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., minimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) AS

-- SQL callable function to get pairs of semi-parallel line segements (pipes). 
-- Pipes are narrow peninsulas or river estuary shapes found in Census polygonal geometries.

  VGeom             MDSYS.SDO_GEOMETRY;
  Geom              MDSYS.SDO_GEOMETRY;
  XYs               MDSYS.SDO_ORDINATE_ARRAY ;
  VXYs              MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  PXYs              MDSYS.SDO_ORDINATE_ARRAY;
  Info              MDSYS.SDO_ELEM_INFO_ARRAY;
  Vinfo             Mdsys.sdo_Elem_Info_Array:= Mdsys.sdo_Elem_Info_Array(1,2,1,5,2,1);
  Pipe_pars         Pipes_type;

-- The pipe type specifies the details of the overlap and the widths
-- and projector
--                -->

--             P(xp,yp)   Q (xq,yq)
--         +-----------------+
--             ^          ^
--   Width_p   |          |  Width q
--             v          v
--             +----------+
--                 <--   

--           

  seg               PLS_INTEGER;
  mseg              PLS_INTEGER;
  current_pipe      PLS_INTEGER;
  current_pos       PLS_INTEGER :=1;
  kount             PLS_INTEGER :=0;

  Big               NUMBER := 1.E10;
  GTYPE             NUMBER := 2002.;
  projector         NUMBER;
  pipe_len          NUMBER;
  aspect_ratio      NUMBER;
  min_width         NUMBER;
 
  sql_stmt          VARCHAR2(4000);
 Begin


   sql_stmt := 'SELECT SDOGEOMETRY FROM ' || In_Table || ' WHERE FACE_ID=:1';
   EXECUTE IMMEDIATE sql_stmt into Geom using face_id;
   XYs := Geom.SDO_ORDINATES;
   Info := GEOM.SDO_ELEM_INFO;
-- Using the desired pipe type (all or peninsulas or estuaries) and the other
-- selection parameters, get the pairs of pipe segments.

-- Select on type, maximum width apart, aspect ratio (length/width) and the
-- maximum angle difference between line segments.

   Pipe_Pars := Find_Get_Pipes(Geom,pipe_type,max_width,minimum_len,min_aspect_ratio,max_angle_diff);
--   dbms_output.put_line('PPP ' || pipe_pars.count);
   If Pipe_pars.count > 0 then

   if GZ_QA.Table_Exists(Out_table) = TRUE then
     execute immediate 'TRUNCATE TABLE ' || Out_Table;
   else
     execute immediate 'CREATE TABLE ' || Out_Table || ' (ID NUMBER,GEOMETRY MDSYS.SDO_GEOMETRY) NOLOGGING';
   end if;
    sql_stmt := 'INSERT INTO ' || out_Table ||' Values(:1,:2)';
   
   For ii in 1..Pipe_pars.count Loop
       if ii = 1 then
         current_pipe := Pipe_pars(1).pipe_no;
       end if;
       if Pipe_pars(ii).Width_p < max_width and Pipe_pars(ii).Width_q < max_width then
        pipe_len := Pipe_pars(ii).Accum_Len;
        min_width := Pipe_pars(ii).Width_p;
        if Pipe_pars(ii).Width_q < min_width then
         min_width := Pipe_pars(ii).Width_q;
        end if;
        aspect_ratio := Pipe_pars(ii).Accum_Len/min_width;
        projector := Pipe_pars(ii).projector; 
        if pipe_len = 0.0 or pipe_len < minimum_len then 
           NULL;
           dbms_output.put_line('DROPPED seg ' || pipe_pars(ii).seg || ' with ' ||pipe_pars(ii).mseg || ' Length ' || round(Pipe_pars(ii).Accum_Len,3));
        elsif projector >= 12 then

-- Describe the overlap portions, 2 segments in a geometry
  
        
        VXYs := Get_APipe_pair(XYs,Pipe_Pars,ii);
  --  Build the Geometry       
        If VXys.count is not NULL then
          VGeom := MDSYS.SDO_GEOMETRY(2006,Geom.SDO_SRID,NULL,Vinfo,VXys);
          kount := kount + 1;
        EXECUTE IMMEDIATE sql_stmt using ii,VGeom;
        End If;
        end if;
        End If;
    End Loop;
    commit;


    
  end if;
  dbms_output.put_line('Count ' || kount);
  RETURN ;
   
END Make_Pipe_Pairs_Table;
--
FUNCTION GET_PIPE_Segs(pGeom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., minimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY  AS

-- A very simple SQL callable function to get pairs of semi-parallel line 
-- segments (pipes). At present there is no weeding except the lines must overlap
-- and each segment length >= minimum_len (defaults to zero).

-- Pipes are narrow peninsulas or river estuary shapes found in Census polygonal geometries.

  VGeom             MDSYS.SDO_GEOMETRY;
  Geom              MDSYS.SDO_GEOMETRY := pGeom;
  XYs               MDSYS.SDO_ORDINATE_ARRAY := Geom.SDO_ORDINATES;
  VXYs              MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  PXYs              MDSYS.SDO_ORDINATE_ARRAY;
  Info              MDSYS.SDO_ELEM_INFO_ARRAY := GEOM.SDO_ELEM_INFO;
  Vinfo             Mdsys.sdo_Elem_Info_Array:= Mdsys.sdo_Elem_Info_Array();
  Pipe_pars         Pipes_type;

-- The pipe type specifies the details of the overlap, the widths and the 
-- projector (the point that projects onto the other segment).
--                -->

--             P(xp,yp)   Q (xq,yq)
--         +-----------------+
--             ^          ^
--   Width_p   |          |  Width q
--             v          v
--             +----------+
--                 <--   

--           

  next             PLS_INTEGER :=0;
  seg               PLS_INTEGER;
  mseg              PLS_INTEGER;
  current_pos       PLS_INTEGER :=1;
  p1                PLS_INTEGER;
  p2                PLS_INTEGER;
  Big               NUMBER := 1.E10;
  GTYPE             NUMBER := 2002.;
  projector         NUMBER;
  pipe_len          NUMBER;
  aspect_ratio      NUMBER;
  min_width         NUMBER;
  hi_width          NUMBER;
  
 Begin


-- Use automatic width detection when the maximum pipe width is not specified

   if max_width <= 0.0 then
     hi_width := get_pipe_width(Geom,125.,'HIGH');
   else
     hi_width := max_width;
   end if;
-- Pre-process the geometry so holes go clockwise so that matching segments
-- go in the opposite direction than the current segments:
--         <--987--                 islands do this   ------->
--                 |
--         ---123-->                outer polygon   ----------->

--    For ii in 2.. TRUNC(Info.count/3) loop
--      if Info(ii*3-1) = 2003 then
--         p1 := Info(ii*3-2);
--         if ii*3 <> Info.count then
--           p2 := Info(ii*3+1)-1;
--         else
--           p2 := Xys.count;
--         end if;
         -- reverse them
--         GZ_UTIL_ZONE.reverse_ordinates(Xys,p1,p2);
--      end if;
--    End Loop;
--    If p1 is not NULL then
--      Geom.sdo_ordinates := Xys;
--    End if;

-- Using the desired pipe type (all or peninsulas or estuaries) and the other
-- selection parameters, get the pairs of pipe segments.

-- Select on type, maximum width apart, aspect ratio (length/width) and the
-- maximum angle difference between line segments.
  dbms_output.put_line('called find-get with' || hi_width ||' ' || minimum_len||' ' || min_aspect_ratio ||' ,' || max_angle_diff);
 Pipe_Pars := Find_Get_Pipes(Geom,pipe_type,hi_width,minimum_len,min_aspect_ratio,max_angle_diff);

--   dbms_output.put_line('Pipe segment count ' || pipe_pars.count);

   If Pipe_pars.count > 0 then
   
   For ii in 1..Pipe_pars.count Loop

--      Does it make any sense to filter them based upong widths?

 --      if Pipe_pars(ii).Width_p < max_width and Pipe_pars(ii).Width_q < max_width then
        pipe_len := Pipe_pars(ii).Accum_Len;
        min_width := Pipe_pars(ii).Width_p;
        if Pipe_pars(ii).Width_q < min_width then
          min_width := Pipe_pars(ii).Width_q;
        end if;
        aspect_ratio := Pipe_pars(ii).Accum_Len/min_width;
        projector := Pipe_pars(ii).projector;

-- It does not seem to make sense to filter the found pipe segments.
        
        if pipe_len = 0.0 or pipe_len < minimum_len then --or aspect_ratio < min_aspect_ratio then
           NULL;
--         dbms_output.put_line('DROPPING seg ' || pipe_pars(ii).seg || ' with ' ||pipe_pars(ii).mseg || ' Length ' || round(Pipe_pars(ii).Accum_Len,3));

-- Here we accept anything that overlaps

        elsif projector >= 12 then
--        dbms_output.put_line('Accepting seg ' || pipe_pars(ii).seg || ' with ' ||pipe_pars(ii).mseg || '  L ' || round(pipe_pars(ii).length,4) || ' P ' ||pipe_pars(ii).projector);

-- Describe the overlap portions, 2 segments in a geometry
 
          Vxys.extend(8);        
          PXYs := Get_APipe_pair(XYs,Pipe_Pars,ii);
          
          For jj in 1..8 Loop
            VXYs(next+jj) := PXYs(jj);
          End loop;
          next := next +8;

        end if;
 --       End If;
    End Loop;
 
 --  Build the returned geometry with 1 or more linestrings
 
    If Vxys.count <> 0 then
      if Vxys.count > 4 then
        Gtype := 2006.;
      End if;
      Vinfo.extend(3*TRUNC(next/4));
      
      for i in 1..TRUNC(next/4) loop
         Vinfo(i*3-2) := (i-1)*4+1;
         Vinfo(i*3-1) := 2;
         Vinfo(i*3) := 1;
      end loop;
      VGeom := MDSYS.SDO_GEOMETRY(GTYPE,Geom.SDO_SRID,NULL,Vinfo,VXys);
    End if;
    
  end if;
  
  RETURN VGeom;   -- May be NULL
   
END GET_Pipe_Segs;
--
PROCEDURE  NEWSET_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0,
                      ploops  PLS_INTEGER default 2) AS
/*
--##############################################################################
--Program Name: set_mbr
--Author: Sidey Timmins
--Creation Date: 7/25/08
-- New idea but slower !!!
--Updated: 11/04/2010 To go through the array in different directions
--Usage:
  -- This PL/SQL function has 8 parameters:
  --             XYs:      the input array of xy coordinates
  --             pLB, pUB  :      elements caller must specify to start and stop
  --                              at in the ordinate array
  --             ploops:   When set to 1, forces a quick (and incomplete) scan
   Output        xLL,yLL,xUR,yUR  the returned MBR (Lower left, Upper right).
--
--                ---------+ (xUR,yUR)
--                |        |
--                |        |
--      (xLL,yLL) +--------+
--
--Purpose: This function determines the MBR of a geometry. It is about 20% faster
--         than Oracle sdo_geom.sdo_mbr.

--Dependencies: None

--##############################################################################
*/
   n       PLS_INTEGER := Xys.count;
   ii      PLS_INTEGER;
   LB      PLS_INTEGER := pLB;
   UB      PLS_INTEGER := pUB;

   inc     PLS_INTEGER;
   mid     PLS_INTEGER;
   loops   PLS_INTEGER :=2;
   m       PLS_INTEGER;
   xtry    NUMBER;
   ytry    NUMBER;

BEGIN

   If UB = 0 then   UB := n;   end if;

-- We want the loop below to just do comparisons and not stores
-- so check the the midpoint and the end of the edge.
-- Remember we start at the beginning.

   mid := TRUNC((LB+UB)/2);
-- Ensure mid is even and the tests below work when n=4
   if TRUNC(mid/2) *2 <> mid then  mid := mid +1;  end if;

   xLL := XYs(mid-1);
   yLL := XYS(mid);
   xUR := XYs(mid-1);
   yUR := XYs(mid);

   If XYs(UB-1) < xLL then
          xLL := XYs(UB-1);
   ElsIf XYs(UB-1) > xUR then
          xUR := XYs(UB-1);
   End If;

   If XYs(UB) < yLL then
         yLL := XYs(UB);
   ElsIf XYs(UB) > yUR then
         yUR := XYs(UB);
   End If;

   IF n <=4 THEN    -- we are done !!
      NULL;
   ELSE

-- Loop twice using a comb to avoid a lot of wasted loads and stores
-- Set the increment through the coordinates for the comb

     inc := sqrt(UB - LB +1);

     if TRUNC(inc/2)*2 <> inc then  inc := inc + 1; end if;
--dbms_output.put_line('inc' || inc);
-- for less than 400 coordinates, brute force seems fastest
     if inc < 20 then
        inc := 2;
--        dbms_output.put_line('IInc' || inc);
     elsif inc > 40 then
        inc := inc * 2;
     end if;
-- Get a rough estimate when ploops = 1 specified by caller
     If inc = 2 or ploops = 1 then  loops := 1; end if;
     If ploops = 1 and (UB-LB+1) >= 100 then inc := inc*4; end if;
-- dbms_output.put_line('INC' || inc);
     For jj in 1..loops Loop
       m := TRUNC((UB-LB+1)/(inc*2));
  
       if loops = 2 and jj = 1 then
         ii := UB + inc-2;
         inc := -inc;
       else
         ii := pLB  - inc;
       end if;
-- Most of the time this loop doesn't do anything except additions and tests.

       For i in 1..m Loop
         ii := ii + inc;
--         dbms_output.put_line('II' || ii || ' xys ' || xys.count || ' inc ' || inc);
         xtry := Xys(ii);
         ytry := Xys(ii+1);
         ii := ii + inc;
         IF xtry > Xys(ii) THEN
           If xtry > xUR then xUR := xtry; end if;
           If XYs(ii) < xLL then xLL := XYs(ii); end If;
         ELSE
           If xtry < xLL then xLL := xtry; end if;
           If XYs(ii) > xUR then xUR := Xys(ii); end if;
         END IF;
         IF ytry > Xys(ii+1) THEN
           If ytry > yUR then yUR := ytry; end if;
           If XYs(ii+1) < yLL then yLL := XYs(ii+1); end If;
         ELSE
           If ytry < yLL then yLL := ytry; end if;
           If XYs(ii+1) > yUR then yUR := Xys(ii+1); end if;
         END IF;
       End Loop;
       inc := 2;   -- now we check every coordinate pair
--   dbms_output.put_line('LL ' || xLL || ' y ' || yLL || ' UR ' || xUR || ' y ' || yUR);
     End Loop;
  END IF;

END NEWSET_MBR;
--

FUNCTION GET_NARROW_SEGS(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 500.,choose VARCHAR2 default 'SMALL',pgap PLS_INTEGER default 2) RETURN MDSYS.SDO_GEOMETRY  AS

  Widths            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Distances         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Where_is          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Matches           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();


  VGeom             MDSYS.SDO_GEOMETRY;
  XYs               MDSYS.SDO_ORDINATE_ARRAY := Geom.SDO_ORDINATES;
  VXYs              MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  Info              MDSYS.SDO_ELEM_INFO_ARRAY := GEOM.SDO_ELEM_INFO;
  Vinfo             Mdsys.sdo_Elem_Info_Array:= Mdsys.sdo_Elem_Info_Array();
  
  gtype             NUMBER := Geom.sdo_gtype;
  interpretation    NUMBER;
  
  seg               PLS_INTEGER;
  mseg              PLS_INTEGER;
  loc               PLS_INTEGER :=0;
BEGIN

-- Ignore point, multipoint and hetereogenous collections
  if geom is NULL or gtype = 2001 or gtype = 2004 or gtype = 2005 then
     RETURN NULL;
  end if;

  interpretation := Geom.Sdo_Elem_Info(3);
  if interpretation > 1 then
     RETURN NULL;
  end if;

  Widths := Get_Pipe_Widths(Geom,Where_is,Matches,Distances,cutoff_width);
  
  if Widths is NOT NULL then
    Vxys.extend(Widths.count*8);
    
    For ii in 1..Widths.count Loop
      seg := Where_is(ii);
      mseg := Matches(ii);
--      dbms_output.put_line('ii ' || ii || ' Seg ' || seg || ' mseg ' || mseg);
      loc := loc + 4;
      Vxys(loc-3) := Xys(seg*2-1);
      Vxys(loc-2) := Xys(seg*2);      
      Vxys(loc-1) := Xys(seg*2+1);
      Vxys(loc) :=   Xys(seg*2+2);
      loc := loc + 4;
      Vxys(loc-3) := Xys(mseg*2-1);
      Vxys(loc-2) := Xys(mseg*2);      
      Vxys(loc-1) := Xys(mseg*2+1);
      Vxys(loc) :=   Xys(mseg*2+2);
    
    End Loop;
 
       -- Setup Info array

    If Vxys.count = 8 then
      Gtype := 2002.;
      Vinfo := Mdsys.sdo_elem_info_array(1,2,1);                               
    Else
      Gtype := 2006.;
      Vinfo.extend(3*TRUNC(loc/4));
      
      for i in 1..TRUNC(loc/4) loop
         Vinfo(i*3-2) := (i-1)*4+1;
         Vinfo(i*3-1) := 2;
         Vinfo(i*3) := 1;
      end loop;
      
    End if;
    VGeom := MDSYS.SDO_GEOMETRY(GTYPE,Geom.SDO_SRID,NULL,Vinfo,VXys);
    
    RETURN VGeom;
  end if;
  
  RETURN NULL;
 
END GET_NARROW_SEGS;
--
FUNCTION GET_PIPE_WIDTH(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 125.,choose VARCHAR2 default 'HIGH',pgap PLS_INTEGER default 1) RETURN NUMBER  AS

-- Returns either 1) narrowest (minimum) width found in a geometry 
--             or 2) the width to use to find most pipes.
--
-- Geom - input geometry
-- cutoff_width  -- just for efficiency to avoid checking widths between segments very far apart.)
-- choose 'SMALL' gets the narrowest width, 'HIGH' gets the pipe width
-- pgap - defaults to 2, the segment gap to use
--
--SQL> select gz_pipes.get_pipe_width(sdogeometry) from txlaincplace where fullcodel=93532520;

--GZ_PIPES.GET_PIPE_WIDTH(SDOGEOMETRY)
------------------------------------
--                                42.1

  Widths            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Distances         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Where_is          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Matches           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Hgram             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(); 
  
  avg_bin           NUMBER;
  current_hi        NUMBER;
  cutoff_to_use     NUMBER := cutoff_width;
  bot               NUMBER;
  top               NUMBER;
  range_            NUMBER;
  small_width       NUMBER;
  big_width         NUMBER;
  gtype             NUMBER := Geom.Sdo_gtype;
  interpretation    NUMBER;
  hi_width          NUMBER;
  last_width        NUMBER;
  factor_a          NUMBER := 1.5;
  factor_b          NUMBER := 2.;
  factor_c          NUMBER := 0.5;
  hi_bin            PLS_INTEGER;
  nbins             PLS_INTEGER :=10;
  shoulder_bin      PLS_INTEGER;
  no_hi_bins        PLS_INTEGER :=0;
  no_zero_bins      PLS_INTEGER := 0;
  bin_sum           PLS_INTEGER := 0;
  bump_up           BOOLEAN;
BEGIN

-- Ignore point, multipoint and hetereogenous collections
  if geom is NULL or gtype = 2001 or gtype = 2004 or gtype = 2005 then
     RETURN NULL;
  end if;

  interpretation := Geom.Sdo_Elem_Info(3);

  if interpretation > 1 then
     RETURN NULL;
  end if;

--  The loop here avoids trusting the caller who may set the maximum width 
-- to look for too small.

   if cutoff_to_use <= 0.0 then
     cutoff_to_use := 125.;
   end if;

    -- The input cutoff maybe too narrow and so the last bin may contain
    -- the highest count. We desire that the bin with the highest count
    -- not be the last bin since there may be more pipes that are not being found.

 For loops in 1..3 Loop
  
    cutoff_to_use := cutoff_to_use*factor_a;
    small_width  := cutoff_to_use*factor_b;
    avg_bin := 0.0;
    big_width := 0.0;
    hi_bin := 2;
    no_hi_bins :=0;
    no_zero_bins := 0;
    bin_sum :=0;
    last_width := -1.;
--    dbms_output.put_line('Cutoff Width ' || cutoff_to_use);
    Widths := Get_Pipe_Widths(Geom,Where_is,Matches,Distances,cutoff_to_use,pgap,FALSE);

--  There maybe no widths returned if the cutoff width is too narrow

    if Widths is NOT NULL then
          
      for ii in 1..widths.count loop
--      dbms_output.put_line('Width ' ||round(widths(ii),3));
           if Widths(ii) < small_width then
             small_width := Widths(ii);
           end if;
           if Widths(ii) > big_width then
             big_width := Widths(ii);
           end if;
        end loop;
        if UPPER(choose)='SMALL' then  -- return the narrowest
           RETURN small_width;       
        end if;
      
--  Get the typical high width (highest of a bin above the most frequent bin)

 --     if widths.count > 100 then
 --       nbins := 15;
 --     end if;
      
      range_ := (big_width-small_width)/nbins + 0.0001;
      Hgram.extend(nbins);

      
      top := small_width;
 
      For bin in 1..nbins Loop
        Hgram(bin) := 0.0;
        bot := top;
        top := top + range_;
        for ii in 1..widths.count loop
         if Widths(ii) >= bot and Widths(ii) < top then
--           if Widths(ii) <> last_width and Widths(ii) >= bot and Widths(ii) < top then
             Hgram(bin) := Hgram(bin) + 1.0;
           end if;
--           last_width := Widths(ii);
        end loop;
-- Choose the high bin
        if bin = 1 or Hgram(bin) > Hgram(hi_bin) then
          hi_bin := bin;
        end if;
        if Hgram(bin) = 0 then
           no_zero_bins := no_zero_bins+1;
        end if;
       
        if Hgram(bin) <= 5 then
        bin_sum := bin_sum+1;
        end if;
--        dbms_output.put_line('bin ' || bin || ' hgram ' || hgram(bin) || ' bot ' || round(bot,2) || ' ' || round(top,2) || ' hi ' || hi_bin);
      End Loop;
      
      
      shoulder_bin := hi_bin+1;
      if hi_bin < nbins-1 and Hgram(shoulder_bin) > 0.5*Hgram(hi_bin) then
        shoulder_bin := shoulder_bin+1;
      end if;
      if shoulder_bin < nbins-1 and Hgram(shoulder_bin) > 3*Hgram(shoulder_bin+1) then
        factor_c := 0.;
      end if;
      For bin in hi_bin+2..nbins Loop
        If Hgram(bin) > Hgram(shoulder_bin) then
          no_hi_bins := no_hi_bins+1;
        End If;
      End Loop;
      
-- Loop again when we need to increase the range
-- or when we need to reduce the range.

      if loops = 1 and (hi_bin >= nbins-1 or no_zero_bins >1)  then
            continue;
-- Bins just seem to go on increasing beyond the shoulder bin 
      elsif loops = 1 and no_hi_bins > 2 then
          factor_a := 0.5;
          factor_b := 1.;
            continue;
      elsif loops <= 2  and bin_sum > 3 then
--      dbms_output.put_line('bin_sum ' || bin_sum);
        continue;
      end if;
      
      current_hi := Hgram(hi_bin);
      For bin in hi_bin+1..nbins loop
      -- Choose the next one if it is at least 40% of the high bin
        if bin < nbins and Hgram(bin) > 0.4*current_hi then
              hi_bin := bin;
--        elsif bin < nbins and Hgram(bin+1) > 0.4*current_hi then
--            NULL;
        else
          exit;
        end if;
      End Loop;
 --          dbms_output.put_line('hi_bin ' || hi_bin);
      if hi_bin < (nbins -1) then
/*
-- Work out an average      
        for bin in hi_bin+1..nbins loop
          avg_bin := avg_bin + Hgram(bin);
        end loop;
        dbms_output.put_line('AVG bin was' || avg_bin);
        avg_bin := avg_bin/ (nbins-hi_bin);
-- Take the last bin that is higher than the average if there is a 2nd peak        
        bump_up := FALSE;
        for bin in reverse hi_bin+1..nbins loop
          if Hgram(bin) > 1.5* avg_bin and Hgram(bin) > 0.4*Hgram(hi_bin) then
            bump_up := TRUE;
            hi_bin := bin-1;
            exit;
          end if;
        end loop;
         dbms_output.put_line('NOW hi_bin ' || hi_bin || ' range ' || range_ || ' small ' || small_width);
*/
        if bump_up then        
          hi_width := round(small_width + hi_bin*range_,1);
        else
          hi_width := round(small_width + (hi_bin+factor_c)*range_,1);
        end if;
--        dbms_output.put_line('bin ' || hi_bin || ' avg '|| avg_bin);        
      else
         
         hi_width := round(small_width + (hi_bin+0.5)*range_,1);
      end if;
         RETURN hi_width;
    else
      RETURN NULL;
    end if;
  End Loop;
  RETURN NULL;
END GET_PIPE_WIDTH;
--
FUNCTION GET_WIDTHS_BY_IDS(pTopology VARCHAR2, Edge_Id1 NUMBER, Edge_Id2 NUMBER, cutoff_width NUMBER default 100.) RETURN MDSYS.SDO_LIST_TYPE AS

  Topology       VARCHAR2(30) := UPPER(pTopologY);
  Edge_Geom      MDSYS.SDO_GEOMETRY;
  Geom           MDSYS.SDO_GEOMETRY;
  Info_Array     MDSYS.SDO_ELEM_INFO_ARRAY;
  Info2_Array    MDSYS.SDO_ELEM_INFO_ARRAY;
  XYOrd          MDSYS.SDO_ORDINATE_Array;
  XYOrd2         MDSYS.SDO_ORDINATE_Array;

  Widths         MDSYS.SDO_LIST_TYPE;
  sql_stmt       VARCHAR2(4000);
  p              PLS_INTEGER;
  q              PLS_INTEGER;
  n              PLS_INTEGER;
  m              PLS_INTEGER;
  
BEGIN

  

  
  sql_stmt := 'SELECT GEOMETRY FROM '||Topology||'_EDGE$ WHERE EDGE_ID =:1';
  EXECUTE IMMEDIATE sql_stmt into Geom using edge_id1;

  XyOrd := Geom.sdo_ordinates;
--  Serena_Zone.reverse_ordinates(XYOrd);  -- just for testing
  Info_Array := Geom.Sdo_Elem_Info;

    EXECUTE IMMEDIATE sql_stmt into Edge_Geom using edge_id2;
  
    XyOrd2 := Edge_Geom.sdo_ordinates;
--    Serena_Zone.reverse_ordinates(XYOrd2);  -- ditto
    Info2_Array := Edge_Geom.Sdo_Elem_Info;
    n := XYord.count;
    m := XYord2.count;
    p := Info_array.count;
    q := Info2_array.count;

-- Here we concatenate all the new coordinates with their Info triplet(s)
  
     Info_array.extend(q);
     for ii in 1..q loop
   
        if MOD(ii,3) = 1 then
          Info_array(ii+p) := Info2_array(ii) + n;
        else
          Info_array(ii+p) := Info2_array(ii);
        end if;
     end loop;
     
-- Concatenate the Xys

     XYOrd.extend(m);
     for ii in 1..m Loop
       XyOrd(ii+n) := XYord2(ii);
     end loop;


  Geom := MDSYS.SDO_GEOMETRY(2006,8265.,NULL,Info_Array,XYORD);
 
  Widths := Get_Widths(Geom,cutoff_width);
  
  Return Widths;
  
END GET_WIDTHS_BY_IDS;
--
FUNCTION FIND_ALL_OPENINGS(Geom IN MDSYS.SDO_GEOMETRY,pipe_type VARCHAR2 default 'A',max_width NUMBER default 150., minimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) return MDSYS.SDO_GEOMETRY AS

    Xys         MDSYS.SDO_ORDINATE_ARRAY := Geom.Sdo_Ordinates;
    New_Geom    MDSYS.SDO_GEOMETRY;
    New_XYs     MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    New_Info    MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY();
    Info        MDSYS.SDO_ELEM_INFO_ARRAY := Geom.Sdo_Elem_Info;
    Found       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    abc          MDSYS.SDO_LIST_TYPE;
    Pipe_pars         Pipes_type;
    WW                MDSYS.SDO_LIST_TYPE;
    Ring             Xy_list;
 
    n                PLS_INTEGER := TRUNC(Xys.count/2);
    angle            NUMBER;
    thousand         NUMBER := 100000.;
    x                NUMBER;
    y                NUMBER;
    x1               NUMBER;
    y1               NUMBER;
    x2               NUMBER;
    y2               NUMBER;
    old_found        NUMBER;
    projector        NUMBER;
    result          NUMBER;
    pi               NUMBER;
    coord            PLS_INTEGER;
    first_in_ring    PLS_INTEGER;
    last_in_ring     PLS_INTEGER := 0;
    seg1             PLS_INTEGER;
    seg2             PLS_INTEGER;
    seg              PLS_INTEGER;
    mseg             PLS_INTEGER;
    rseg             PLS_INTEGER;
    nseg             PLS_INTEGER;
    seg_found        PLS_INTEGER;
    mseg_found       PLS_INTEGER;
    sstart           PLS_INTEGER;
    slast            PLS_INTEGER;
    last_seg         PLS_INTEGER;
    next_seg         PLS_INTEGER;
    last_mseg        PLS_INTEGER;
    next_mseg        PLS_INTEGER;
    mstart           PLS_INTEGER;
    mlast            PLS_INTEGER;
    kount            PLS_INTEGER:= 0;
    ring_type        PLS_INTEGER;
    m                PLS_INTEGER;
    vtx              PLS_INTEGER;
    Xys_count        PLS_INTEGER := XYs.count;
    pinfo            PLS_INTEGER :=0;
    loops            PLS_INTEGER;
    vertex           PLS_INTEGER;
    next            PLS_INTEGER := 0;
    
    on_seg           VARCHAR2(1);
    way              VARCHAR2(5);
    debug_it         VARCHAR2(5);
    is_pipe          BOOLEAN;
    open_up          BOOLEAN;
    done             BOOLEAN;
    
    Function make_angle(seg1 pls_integer,seg2 pls_integer) return number as
         
-- make an angle between 2 opposing line segments  ^    |
--                                                 |    v

         x1     number := Xys(seg1*2-1);
         y1     number := Xys(seg1*2);
         x2     number := Xys(seg1*2+1);
         y2     number := Xys(seg1*2+2);
         x3     number := Xys(seg2*2-1);
         y3     number := Xys(seg2*2);
         x4     number := Xys(seg2*2+1);
         y4     number := Xys(seg2*2+2);
         angle_measured  number;
    Begin
    
        angle_measured := angle_in_degrees(x1,y1,x2,y2,x4,y4,x3,y3);
        return angle_measured;
    End;
    
    Function check_angle(input_angle number,check_it number,angle_epsilon number default 10.) return boolean as

-- Check input_angle (for example) is between 85 and 95 degrees for an ostensible 90 degrees.

--    result := check_angle(87,90.,5.);  --    85 <= 87 <= 95
   Begin
     
     if input_angle <= check_it+angle_epsilon and input_angle >= check_it-angle_epsilon then
        return TRUE;
     end if;
     return FALSE;
   End;
    
BEGIN
dbms_output.put_line('called find-get with' || max_width ||' ' || minimum_len||' ' || min_aspect_ratio ||' ,' || max_angle_diff);
  Pipe_Pars := Find_Get_Pipes(Geom,pipe_type,max_width,minimum_len,min_aspect_ratio,max_angle_diff);

  WW := GET_WIDTHS(Geom,max_width,'MATCH',2,'TRUE');

  -- This is the highest S segment
  m := (WW(WW.count) - TRUNC(WW(WW.count)/thousand) *thousand);
  Found.extend(n);
 
 
 -- We want to close a ring so the appropriate data structure is a linked list
 -- describing the segment attachments. We can then break the links to jump
 -- across at the beginning of pipes.
 -- We begin with segment 1 -> 2, 2 -> 3 etc to the end of the ring where
 -- say vertex 101 = 1 so segment 100 -> 1
 --
 --     start    next     x, y
 --     -------+------
 --     |   1 ->  2  |
 --     --------------
 --     |   2 ->  3  |
 --     --------------
 --     ...
 --      
 --     -----------------------------
 --     | 100 ->  0  |   x(1), y(1) |
 --     -----------------------------
 
 -- Find all the pipe segments and mark them since some of them are
 -- adjacent to openings
  
  For ij in 1..n Loop
     Ring(ij).start_vertex := ij;
     
     if ij > last_in_ring then
        coord := ij*2-1;
        find_info(coord,Info,Xys.count,first_in_ring,last_in_ring,ring_type);
     end if;
     
     if ij <> last_in_ring then
       Ring(ij).next_vertex := ij+1; -- this means next coordinate is
       --                               Xys((ij+1)*2-1), Xys((ij+1)*2)
     else
       Ring(ij).next_vertex := 0;  -- Don't want to go around the ring again
       Ring(ij).x := Ring(first_in_ring).x;  -- but this is the next coordinate
       Ring(ij).y := Ring(first_in_ring).y;
     end if;
     Found(ij) := 0.0;
     for ii in 1..WW.count loop
        seg2 := (WW(ii) - TRUNC(WW(ii)/thousand) *thousand);
        seg1 := (wW(ii) - seg2)/thousand;
--  if seg1 = 345 then
  
--     dbms_output.put_line('found ' ||seg1 ||','||seg2);
--  end if;
        if seg1 = ij or seg2 = ij then
           Found(ij) := 1.;
           exit;
        end if;
     end loop;
  End Loop;
 
 
 -- Now look for openings at each vertex.
 -- Case a) it is at the beginning or end of a pipe
 --      b) it is not in the pipe parameters info but there is a break in
 --         the seguence Found from the widths. When there is not a break then 
 --         the Found =0 segments are part of the ring(s) to be kept. 
  
  
  last_in_ring := 0;
  
  For vtx in 1..n Loop
  
  -- Bypass non existent segments at end of each ring
  
     if vtx > last_in_ring then
        coord := vtx*2-1;
        find_info(coord,Info,Xys.count,first_in_ring,last_in_ring,ring_type);
     elsif vtx = last_in_ring then
        continue;
     end if;
 
 -- Search the Pipe_parameters to see if it is part of a pipe
 
    is_pipe := FALSE;

    pi := Find_it_inseg(vtx,Pipe_pars);
    if pi = 0 then
      pi := Find_it_inmseg(vtx,Pipe_pars);
    end if;

-- The Pipe parameters describe where the ends of pipes are
    
    if (pi <> 0 and (Pipe_pars(pi).link_bck <> 0 and Pipe_pars(pi).link_fwd <> 0 )) then
       continue;
    end if;
    
     dbms_output.put_line('vtx ' ||vtx || ' ' || pi);
      if vtx =365 then
         dbms_output.put_line('vtx ' ||vtx || ' ' || pi);
         debug_it := 'TRUE';
      end if;
-- There may be multiple records for a segment
    loops := 0;
    While pi > 0 and pi <= Pipe_pars.count and loops < 10 and
         (vtx = Pipe_pars(pi).seg or vtx = Pipe_pars(pi).mseg) Loop
 
      is_pipe := TRUE;
      loops := loops + 1;              
      seg := Pipe_pars(pi).seg;
      mseg := Pipe_pars(pi).mseg;
      done := FALSE;
      
--   If we have already done this pair then exit      
-- BUG ??? AND or OR ???

--      if vtx = 344 or vtx = 345 then
--      if loops = 1 then
--         dbms_output.put_line('found ' || found(seg)||','||found(mseg) || ' ' || seg || ' ' || mseg || ' vtx ' || vtx);
--      end if;
      if Found(seg) = 4. OR  Found(mseg) = 4. then
         done := TRUE;
 --        exit;
      end if;
      if found(vtx) <> 4. then
       Found(vtx) := 2.;
      end if;
-- Now using the Pipe_Pars check whether the pairs of lines are an opening
-- Set the start and end of the ring(s) for seg and mseg 
     
      find_info(seg*2-1,Info,Xys_count,sstart,slast,ring_type);
      find_info(mseg*2-1,Info,Xys_count,mstart,mlast,ring_type);
      projector := Pipe_pars(pi).projector;
      
-- Have to determine whether it is an opening left (away from the pipe to
-- the right) or right (away from the pipe to the left).

      way := NULL;
      If Pipe_pars(pi).link_bck = 0 then
         way := 'LEFT';
         x := Pipe_pars(pi).xp;
         y := Pipe_pars(pi).yp;
         if projector = 12 or projector = 13 then
            on_seg := 'N';
         else
            on_seg := 'Y';
         end if;
      Elsif Pipe_pars(pi).link_fwd = 0 then
         way := 'RIGHT';
         x := Pipe_pars(pi).xq;
         y := Pipe_pars(pi).yq;
         if projector = 13 or projector = 43 then
            on_seg := 'Y';
         else
            on_seg := 'N';
         end if;
      Else
        continue;   -- not at the beginning or end of a pipe
      End if;
      
      
 --    if seg = 365 then
      dbms_output.put_line('>>>>>>** seg ' || seg || ' mseg ' || mseg ||' WAY IS ' || way ||' ' ||on_seg);
 --     end if;
      debug_it := 'FALSE';
      if seg = 761 then
        debug_it := 'TRUE';
        dbms_output.put_line('>>>>>>>>>>>>>>>>>>>>>> seg ' || seg || ' mseg ' || mseg ||' WAY IS ' || way ||' ' ||on_seg ||'<<<<<<<<<<<<');
      end if;
      
-- A 4 way pipe intersection is an example of a place we don't want to look
-- look for an opening. We have to distinguish this 4 way on the left
-- from the legitimate opening on the right. We will disallow when
-- seg+1 is a pipe and mseg-1 is a pipe

--                                           +
--                 |    |                     \      
--                 |    |                      \                               /
--                 |    |                       +                             /
--    +------------+    +-----------+           |                            /
--                                              |                           /
--    +------------+    +--------------+        +----------+     +----------+
--      mseg-1     |    |    seg+1                         |     |
--                 |    |                                  |     |
--          mseg   |    |  seg                             |     |
--                 |    |                                  |     |


--               4 way pipe intersection                  an opening

    

-- There is a possibility of an opening

--    If (Pipe_pars(pi).link_bck = 0 or Pipe_pars(pi).link_fwd = 0) then     
       open_up := TRUE;
--    End if;
     
 
    nseg := mseg+1;
    if nseg >= mlast then
       nseg := mstart;
    end if;
    rseg := seg-1;
    if rseg = 0 then rseg := slast-1; end if;
    
-- Have to check the angle between rseg and nseg is near 180.

    angle := make_angle(rseg,nseg);
    
    if way = 'LEFT' and Found(nseg) <> 0 and Found(rseg) <> 0 and check_angle(angle,180.,20.) then
--    if Pipe_pars(pi).link_bck = 0 and Found(nseg) <> 0 and Found(rseg) <> 0 and
--                                    check_angle(angle,180.,20.) and
--                                    Pipe_pars(Found(nseg)).pipe_no <> Pipe_pars(pi).pipe_no and
--                                    Pipe_pars(Found(rseg)).pipe_no <> Pipe_pars(pi).pipe_no then
        open_up := FALSE;
--        exit;
    end if;
      
    nseg := mseg-1;
    if nseg =0 then
      nseg := mlast-1;
    end if;
    rseg := seg+1;
    if rseg > slast then rseg := sstart; end if;
    
-- Have to check the angle between rseg and nseg is near 180.

    angle := make_angle(rseg,nseg);
    
    if way = 'RIGHT' and Found(nseg) <> 0 and Found(rseg) <> 0 and check_angle(angle,180.,20.) then
--    if Pipe_pars(pi).link_fwd = 0 and Found(nseg) <> 0 and Found(rseg) <> 0 and
--                                  check_angle(angle,180.,20.) and
--                                  Pipe_pars(Found(nseg)).pipe_no <> Pipe_pars(pi).pipe_no and
--                                  Pipe_pars(Found(rseg)).pipe_no <>  Pipe_pars(pi).pipe_no then
--       dbms_output.put_line('pp ' || p || ' Findit ' || findit(nseg) || ' findit ' || findit(rseg));
         open_up := FALSE;
--         exit;
    end if;
    
 
--      if seg <= 3 then
--        debug_it := 'TRUE';
--      end if;
      result := CHECK_OPENING(Xys,Info,Pipe_Pars,x,y, seg, mseg, on_seg, way,debug_it);
       if seg <=5 then
      dbms_output.put_line('Result ' || result || ' seg ' || seg || ' mseg ' || mseg || ' x ' ||x || ' y ' || y);
      end if;
      
--      if pi <> 0 and (Pipe_pars(pi).link_bck = 0 or Pipe_pars(pi).link_fwd =0) and result = 0 then
--         result := 1;
--     end if;
      
      if result = 1.  then
      dbms_output.put_line('seg ' || seg || ' mseg ' || mseg || ' proj '|| projector||on_seg||way||sstart || ' ' ||slast || ' ' || mstart || ' ' || found(vtx));
         
-- We can break a link now to leave a pipe piece out of the picture

        if way = 'LEFT' then
          Ring(seg).next_vertex := mseg;
          Ring(seg).x := x;
          Ring(seg).y := y;
        elsif way = 'RIGHT' then
          Ring(mseg).next_vertex := seg;
          Ring(mseg).x := x;
          Ring(mseg).y := y;
        end if;
         Found(seg) := 4.;
         Found(mseg) := 4.;
         dbms_output.put_line('RResult ' || result || ' seg ' || seg || ' mseg ' || mseg || ' x ' ||round(x,7) || ' y ' ||round( y,7));
 projector := Pipe_pars(pi).projector;
        If way = 'RIGHT' then
        
           if projector = 12 or projector = 42 then
             x1 := Xys(seg*2+1);
             y1 := Xys(seg*2+2);
             x2 := Pipe_pars(pi).Xq;
             y2 := Pipe_pars(pi).Yq;
           elsif projector = 13 or projector = 43 then
             x1 := Xys(mseg*2-1);
             y1 := Xys(mseg*2);
             x2 := Pipe_pars(pi).Xq;
             y2 := Pipe_pars(pi).Yq;
           
           end if;
        Else  -- opening up left
          if projector = 12 or projector = 13 then
             x1 := Xys(seg*2-1);
             y1 := Xys(seg*2  );
             x2 := Pipe_pars(pi).Xp;
             y2 := Pipe_pars(pi).Yp;
           elsif projector = 42 or projector = 43 then
             x1 := Xys(mseg*2+1);
             y1 := Xys(mseg*2+2);
             x2 := Pipe_pars(pi).Xp;
             y2 := Pipe_pars(pi).Yp;
           end if;
        End If;
        dbms_output.put_line('projector ' || projector ||' way ' || way || 'x1 ' || round(x1,7) ||','||round(y1,7) || 'x2 ' || round(x2,7) ||','||round(y2,7));
        New_XYs.extend(4);
        New_Info.extend(3);
        pinfo := pinfo+3;
        New_Info(pinfo-2) := next+1;
        New_Info(pinfo-1) := 2;
        New_Info(pinfo) := 1;
        New_Xys(next+1) := x1;
        New_Xys(next+2) := y1;
        New_Xys(next+3) := x2;
        New_Xys(next+4) := y2;
        next := next +4;

      end if;

      -- Occasionally a single segment has 2 openings on it, so we cannot exit
        if pi < Pipe_pars.count and Pipe_pars(pi+1).seg <> seg then
          exit;
        end if;
      pi := pi +1;
    End Loop;

 
    If is_pipe or done then
      continue;
    End If;
    
-- If is pipe is False then the two segments near the opening may not be pipes 
-- or if they are may converge like this so they fail the test above.

-- So we have to construct the parameters to check_opening at seg and mseg
--
--           +           +
--            \         /    mseg
--             +        +---------
--             |
--       seg   |

-- We get a list back from Get_widths that looks like this with breaks in it:

-- 134   
-- 135     -- just "noise" that we need to ignore
-- 145 
-- 146 
-- 193     -- ditto
-- 194 
-- 195 
-- 255   -- This discontinuity is known to be an opening
-- 345 
-- 346 
-- 347 
-- 348 
-- 349 
-- 350 
-- 351 
-- 352 
-- 353 
-- 354 
-- 355 
-- 356 
-- 357 
-- 358 
-- 359 
-- 362  -- This discontinuity is known to be an opening
-- 499 
-- 500 

-- We usually just want to check where get_widths has discontinuities in the segment order
-- If we use the sorted Seg/Match array from Get_Widths
--           1) it has all nearby matching segments whether parallel or not
--           2) it includes non-pipe segments at the mouth

-- These are our potential openings

--    if vtx = 345 then    
--    dbms_output.put_line(vtx||' is NOT a PIPE!!!!!!' || Found(vtx-1)||' ' ||Found(vtx) || ' ' || Found(vtx+1));
--    end if;

-- If the segment is a ring segment with other ring segments on either side then
-- don't both looking for an opening.

    if Found(vtx) = 0 and vtx >  1 and Found(vtx-1) =0 and vtx < n and Found(vtx+1) = 0 then
      continue;
    end if;

 
 -- Need to find seg -1 in pipe_pars so we can find mseg      
-- on seg can be 'Y' and x,y can be Xys(seg*2-1), Xys(seg*2)     
      
-- Find the matching segments in the WW widths

    last_seg := 0;
    last_mseg := 0;
    next_mseg := (WW(1) - TRUNC(WW(1)/thousand) *thousand);
    next_seg := (wW(1) - next_mseg)/thousand;
    
   
    dbms_output.put_line('looking for '||vtx);
 
    way := NULL;
    for ii in 1..WW.count loop
        if ii > 1 then 
          last_seg := seg;
          last_mseg := mseg;
        end if;
        seg := next_seg;
        mseg := next_mseg;
        if ii < WW.count then
           next_mseg := (WW(ii+1) - TRUNC(WW(ii+1)/thousand) *thousand);
           next_seg := (wW(ii+1) - next_mseg)/thousand;
        else
           next_seg := next_seg+10;
           next_mseg := next_mseg+10;
        end if;

-- Need to know whether we are going away from the pipe with increasing seg
-- number (going 'LEFT' or with decreasing seg number (going 'RIGHT'

 
        pi := Find_it_inseg(vtx,Pipe_pars);
        if pi <> 0 then
          vertex := vtx;
          way := 'LEFT';
        else
          vertex := vtx +1;
          pi := Find_it_inseg(vertex,Pipe_pars);
          if pi <> 0 then
          way := 'RIGHT';
          else
            way := NULL;
          end if;
        end if;
--        if pi = 0 then
-- dbms_output.put_line('PI ' ||pi || ' vtx ' || vtx);
-- end if;
-- Have to get the way right - left is wrong for 345 and 517
-- Have we found an opening going Left?
        if seg = vtx and seg +1 <> mseg then 
--           way := 'RIGHT';
--           dbms_output.put_line(way || ' found ' || vtx);
           exit;
        end if;
 /*       
-- Have we found an opening going Right?
        if seg = ij and seg <> next_seg then 
           way := 'RIGHT';
            dbms_output.put_line(way || ' found ' || ij);
           exit;
        end if;

-- The WW are not sorted on the Matching seg so we cannot tell         
        -- Have we found an opening going Left?
        if mseg = ij and seg <> last_seg then 
           way := 'RIGHT';
           exit;
        end if;
        
-- Have we found an opening going Right?
        if mseg = ij and seg <> next_seg then 
           way := 'LEFT';
           exit;
        end if;
*/
     end loop;
        if seg = 345 or mseg = 517 or vtx = 365 then
  dbms_output.put_line(vtx ||' ' ||way|| seg);
    end if;
     if way is NOT NULL and (Found(seg) <> 4. and Found(mseg) <> 4.) then 
--     dbms_output.put_line('NOW ' ||way || ' found ' || vtx);
     on_seg := 'Y';
     x := Xys(seg*2-1);
     y := Xys(seg*2);
     
     result := CHECK_OPENING(Xys,Info,Pipe_pars,x,y, seg, mseg, on_seg, way,debug_it);
  
     
    if seg = 761 or seg = 365 then
  dbms_output.put_line(vtx ||' ' ||result||'SSSeg ' || seg || ' mseg ' || mseg || ' proj '|| projector||on_seg||way);
    end if;
       if result = 1.  then
      dbms_output.put_line('seg ' || seg || ' mseg ' || mseg || ' proj '|| projector||on_seg||way || 'FOUND' || found(seg) ||',' || found(mseg));
         Found(seg) := 4.;
         Found(mseg) := 4.;
         dbms_output.put_line('Result ' || result || ' seg ' || seg || ' mseg ' || mseg || ' vtx ' || vtx);
 
       dbms_output.put_line('PI ' ||pi || ' vtx ' || vtx);
      
        projector := Pipe_pars(pi).projector;
        If way = 'RIGHT' then
        
           if projector = 12 or projector = 42 then
             x1 := Xys(seg*2+1);
             y1 := Xys(seg*2+2);
             x2 := Pipe_pars(pi).Xq;
             y2 := Pipe_pars(pi).Yq;
           elsif projector = 13 or projector = 43 then
             x1 := Xys(mseg*2-1);
             y1 := Xys(mseg*2);
             x2 := Pipe_pars(pi).Xq;
             y2 := Pipe_pars(pi).Yq;
           
           end if;
        Else  -- opening up left
          if projector = 12 or projector = 13 then
             x1 := Xys(seg*2-1);
             y1 := Xys(seg*2  );
             x2 := Pipe_pars(pi).Xp;
             y2 := Pipe_pars(pi).Yp;
           elsif projector = 42 or projector = 43 then
             x1 := Xys(mseg*2+1);
             y1 := Xys(mseg*2+2);
             x2 := Pipe_pars(pi).Xp;
             y2 := Pipe_pars(pi).Yp;
           end if;
        End If;
        New_XYs.extend(4);
        New_Info.extend(3);
        pinfo := pinfo+3;
        New_Info(pinfo-2) := next+1;
        New_Info(pinfo-1) := 2;
        New_Info(pinfo) := 1;
        New_Xys(next+1) := x1;
        New_Xys(next+2) := y1;
        New_Xys(next+3) := x2;
        New_Xys(next+4) := y2;
        next := next +4;
       
      end if;
   
     End If;
     
  End Loop;
  
  
  For ii in 1..n Loop
    If Found(ii) = 0. then       
       kount := kount + 1;
       dbms_output.put_line('ij ' || ii ||' ' || kount || ' ' || found(ii));
    end if;
  End Loop;
  New_Geom := MDSYS.SDO_GEOMETRY(2006,8265.,NULL,New_Info,New_XYs);
  Return New_Geom;
END FIND_ALL_OPENINGS;
--
FUNCTION GET_WIDE_SEGS(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 500.,pgap PLS_INTEGER default 2,shortest VARCHAR2 default 'FALSE') RETURN MDSYS.SDO_GEOMETRY AS

-- A function to return segments in a geometry that share widths (perpendicular 
-- distances) that are narrower or equal to the cutoff_width

   Seg_Geom      MDSYS.SDO_GEOMETRY;
   XYOrd         MDSYS.SDO_ORDINATE_ARRAY := Geom.Sdo_Ordinates;
   Xys           MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   Info          MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY();
   WW            MDSYS.SDo_LIST_TYPE;
   Angles        MDSYS.SDo_LIST_TYPE;
   
   angle_diff    NUMBER := 0.;
   gtype         NUMBER;
   thousand      NUMBER := 100000.;
   code          NUMBER;
   max_angle_limit NUMBER := 35.;
   loc           PLS_INTEGER :=0;
   seg           PLS_INTEGER;
   mseg          PLS_INTEGER;
BEGIN

   WW := GET_WIDTHS(Geom,cutoff_width,'MATCH',pgap,shortest);


-- Get the bearings  
   Angles :=  Get_Angles(Geom,-1.,'TRUE');

 For ii in 1..Angles.count Loop
   dbms_output.put_line( ii || ' ' || Angles(ii));
 end loop;
-- locations stored as segment * 100,000 + msegment + code

   Xys.extend(6*WW.count);
   
   For ii in 1..WW.count Loop
      seg := TRUNC(ww(ii)/thousand);
      mseg := TRUNC(WW(ii) - seg*thousand);
      
      If max_angle_limit > 0 then

-- We want to measure the angle between either line segments that go in 
-- opposite directions (anti-parallel).
 
           angle_diff := ABS(ABS(Angles(seg) - Angles(mseg)) - 180.);
      End If;
--      dbms_output.put_line('Seg ' || seg || ' mseg ' || mseg || ' ' ||round( angle_diff,3) || ' ' || round(angles(seg),3) || ' ' || round(angles(mseg),3));     

      if angle_diff  <= max_angle_limit then
                              
      code := WW(ii) - seg*thousand - mseg;

-- Place a point followed by a segment

      if code = .01 then
         Xys(loc+1) := XYOrd(seg*2-1);
         Xys(loc+2) := XYOrd(seg*2);
      elsif code = .02 then
         Xys(loc+1) := XYOrd(seg*2+1);
         Xys(loc+2) := XYOrd(seg*2+2);
      elsif code = .03 then
         Xys(loc+1) := XYOrd(mseg*2-1);
         Xys(loc+2) := XYOrd(mseg*2);
      elsif code = .04 then
         Xys(loc+1) := XYOrd(mseg*2+1);
         Xys(loc+2) := XYOrd(mseg*2+2);
      end if;
      loc := loc+2;
      
      if code <=.02 then
         Xys(loc+1) := XYOrd(mseg*2-1);
         Xys(loc+2) := XYOrd(mseg*2);
         Xys(loc+3) := XYOrd(mseg*2+1);
         Xys(loc+4) := XYOrd(mseg*2+2);
      
      else
         Xys(loc+1) := XYOrd(seg*2-1);
         Xys(loc+2) := XYOrd(seg*2);
         Xys(loc+3) := XYOrd(seg*2+1);
         Xys(loc+4) := XYOrd(seg*2+2);
      
      end if;
      loc := loc+4;
      End If;
   End Loop;
   
   
   If Xys.count > loc then
       Xys.trim(Xys.count-loc);     
   End if;
 -- Setup Info array for each point/ linestring combo

    If loc > 0 then
        Gtype := 2004.;                               
      
        Info.extend(loc);
        
        for i in 1..TRUNC(loc/6) loop
          Info(i*6-5) := (i-1)*6+1;   -- point
          Info(i*6-4) := 1;
          Info(i*6-3) := 1;
          Info(i*6-2) := (i-1)*6+3;   -- linestring
          Info(i*6-1) := 2;
          Info(i*6) := 1;
        end loop;
      
        Seg_Geom := MDSYS.SDO_GEOMETRY(GTYPE,Geom.SDO_SRID,NULL,Info,Xys);
    
        RETURN Seg_Geom;
    End if;
 
  
  RETURN NULL;  
END;
--
FUNCTION GET_WIDTHS(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 500.,return_where VARCHAR2 default 'TRUE',pgap PLS_INTEGER default 1,shortest VARCHAR2 default 'TRUE') RETURN MDSYS.SDO_LIST_TYPE  AS

-- A function to measure widths in a geometry. Widths are perpendicular distances
-- between two line segments that do not share a vertex. The distance are measured
-- from vertices to the opposing lines.
-- When parameter shortest is set to 'TRUE' (the default) this function also
-- reports distances between verticies on the opposing lines when they are equal to
-- or under the cutoff_width.

--      Geometry - the input sdogeometry
--      cutoff_width - maximum width to look for (defaults to 500 meters)
--      return_where - 'TRUE' returns the width found at a particular vertex/segment
--                     location. The vertex and segment are encoded as 100,000*vertex + segment number
--                     'MATCH' returns just the encoded vertex/segment numbers
--                     'FALSE' returns just the Widths.
--      pgap - is designed to only measure distance from segments gap apart.

--     Here segments 1 and 3 
--     are to be tested with gap=2              Here the gap chosen is to be 1

--                          +  3
--                         / \                     +  3
--                      4 +   \                       .
--                             \                          .
--                 +------------+               +------------+
--          vertex 1            2               1            2
--
--      shortest  - returns short distances found even when there are no perpendicular
--                  distances.
--       

-- Returns a list of width, vertex position pairs less than the cutoff_width:
-- SQL> select GZ_PIPES.get_widths(sdogeometry,50.) from GZCPB2.V699TM_merge_face where face_id =14;

--GZ_PIPES.GET_WIDTHS(SDOGEOMETRY,50.)
--------------------------------------------------------------------------------
--SDO_LIST_TYPE(5.499, 12, 5.499, 13, 44.36, 41, 15.067, 59, 6.462, 59, 30.733, 60
--            , 2.481, 124, 22.051, 125, 11.792, 143, 1.995, 360, 26.157, 368)

  Widths            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  WW                MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
--  Distances         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
--  Where_is          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
--  Matches           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Found             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  thousand          NUMBER := 100000.;
  always           BOOLEAN;
  
BEGIN
    always := shortest = 'TRUE';
    
    Widths := Get_Pipe_Widths(Geom,g_Where_is,g_Matches,g_Distances,cutoff_width,pgap,always);

--  There maybe no widths returned if the cutoff width is too narrow

    if Widths is NULL then
--    dbms_output.put_line('>>>>>>>>>>>>>>>>>>>>returning NULL');
       RETURN NULL;
    end if;

-- return the widths and where they nearly intersect

    if Widths.count > 0 and return_where = 'TRUE' then
        WW.extend(widths.count*2);
        for ii in 1..Widths.count loop
-- dbms_output.put_line('W ' || Widths(ii) || ' Where ' || g_where_is(ii) || ' Matches ' || g_matches(ii));
            WW(ii*2-1) := Widths(ii);
            -- .01 and .02 refer to vertices 1 and 2: Where_is vertex is too close
            if g_Matches(ii) - TRUNC(g_Matches(ii)) =0.01 then
              WW(ii*2) := g_Where_is(ii)* thousand + TRUNC(g_Matches(ii));
            elsif g_Matches(ii) - TRUNC(g_Matches(ii)) =0.02 then
              WW(ii*2) := (g_Where_is(ii)+1)* thousand + TRUNC(g_Matches(ii));
-- .03 and .04 refer to Matches vertex being too close
            elsif g_Matches(ii) - TRUNC(g_Matches(ii)) =0.03 then
              WW(ii*2) := TRUNC(g_Matches(ii))*thousand + g_Where_is(ii);
            else
             WW(ii*2) :=  (TRUNC(g_Matches(ii))+1.)*thousand + g_Where_is(ii);
            end if;
        end loop;
        
        RETURN WW;
        
-- return the matching segments only under tolerance

    elsif Widths.count > 0 and return_where = 'MATCH' then
        WW.extend(widths.count);
        for ii in 1..Widths.count loop
--        dbms_output.put_line('W ' || Widths(ii) || ' Where ' || g_where_is(ii) || ' Matches ' || g_matches(ii));

-- .01 and .02 refer to vertices 1 and 2: Where_is vertex is too close
--            if Matches(ii) - TRUNC(Matches(ii)) =0.01 then
              WW(ii) := g_Where_is(ii)* thousand + g_Matches(ii);
--            elsif Matches(ii) - TRUNC(Matches(ii)) =0.02 then
--              WW(ii) := (Where_is(ii)+1)* thousand + TRUNC(Matches(ii));
-- .03 and .04 refer to Matches vertex being too close
--            elsif Matches(ii) - TRUNC(Matches(ii)) =0.03 then
--              WW(ii) := TRUNC(Matches(ii))*thousand + Where_is(ii);
--            else
--             WW(ii) :=  (TRUNC(Matches(ii))+1.)*thousand + Where_is(ii);
--            end if;
        end loop;
        g_Matches.extend(widths.count);
        Shellsort2(WW,g_matches);

        RETURN WW;
        
-- return just the widths
    elsif Widths.count > 0 and return_where = 'FALSE'then
--       dbms_output.put_line('>>>>>>>>>>>>>>>>>>>>returning ' || Widths(1));

       RETURN Widths;
    else
       RETURN NULL;
    end if;
    
END GET_WIDTHS;
--
function simple_intersect( x1 number,y1 number,x2 number,y2 number,
                         x3 number,y3 number,x4 number,y4 number) return boolean as
                         
-- Check for a segment intersection between the 2 lines. Here used for the line 
-- joining the subject point and the projected point, and an intervening segment. 
-- The intervening segment maybe immediately before or after the current segment

   s           number;
   t           number;
   X21         number := X2-X1;
   Y21         number := Y2-Y1;
   X43         number := X4-X3;
   Y43         number := Y4-Y3;
   X31         number := X3-X1;
   Y31         number := Y3-Y1;

   det         number;

begin
  -- Check parametric equations for two lines: s is on line 1 from (x1,y1) to (x2,y2)
  --                                           t is on line 2 from (x3,y3) to (x4,y4)
  -- For example xx = (1.-s) * x1 + s*x2
  --             yy = (1.-s) * y1 + s*y2
  
   det :=  X21 * Y43 - X43 * Y21;
  
  
   if  det <> 0.0 then
     s := (-X43 * Y31 + Y43 * X31) /det;
     t := (-X21 * Y31 + Y21 * X31) /det;
  
     If s > 0.0 and s < 1.0 and t > 0.0 and t < 1.0 then
        return TRUE;
     end if;
  
    end if;
    return FALSE;

end simple_intersect;  
    --
FUNCTION GET_SansPIPE_Segs(pGeom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., minimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY  AS

-- SQL callable function to get a geometry sans pairs of semi-parallel line segements (pipes). 
-- Pipes are narrow peninsulas or river estuary shapes found in Census polygonal geometries.


--  TYPE CHAR_ARRAY IS VARRAY(1048576) OF VARCHAR2(1);
  PolyGeom          MDSYS.SDO_GEOMETRY;  -- pieces of a polygon
  Geom              MDSYS.SDO_GEOMETRY     := pGeom;
  XYs               MDSYS.SDO_ORDINATE_ARRAY := Geom.SDO_ORDINATES;
  PXYs              MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  Info              MDSYS.SDO_ELEM_INFO_ARRAY:= GEOM.SDO_ELEM_INFO;
  Pinfo             Mdsys.sdo_Elem_Info_Array:= Mdsys.sdo_Elem_Info_Array();
  Pipe_pars         Pipes_type;
  Angles            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Lengths           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Areas             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Pipe_Overlap      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Class_it          CHAR_ARRAY := CHAR_ARRAY();
  
  Ring_Xys          MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  Ring_Info          Mdsys.sdo_Elem_Info_Array:= Mdsys.sdo_Elem_Info_Array();

-- The pipe type specifies the details of the overlap and the widths
-- and projector
--                -->

--             P(xp,yp)   Q (xq,yq)
--         +-----------------+
--             ^          ^
--   Width_p   |          |  Width q
--             v          v
--             +----------+
--                 <--   

--           

  next             PLS_INTEGER :=0;
  seg               PLS_INTEGER;
  mseg              PLS_INTEGER;
  ring_count        PLS_INTEGER;
  last_v            PLS_INTEGER;
  last_pipev        PLS_INTEGER :=-1;
  n                 PLS_INTEGER;
  first_coord       PLS_INTEGER;
  current_pipe      PLS_INTEGER;
  current_pos       PLS_INTEGER :=1;
  p1                PLS_INTEGER;
  p2                PLS_INTEGER;
  seg_no            PLS_INTEGER;
  last_seg          PLS_INTEGER :=0;
  last_out          PLS_INTEGER :=0;
  last_found        PLS_INTEGER :=0;
  last_coord        PLS_INTEGER :=0;
  seg1              PLS_INTEGER;
  seg2              PLS_INTEGER;
  vcount            PLS_INTEGER :=0;
  nvtx              PLS_INTEGER;
  np                PLS_INTEGER;
  area              NUMBER;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;  
  x3                NUMBER;
  y3                NUMBER;
  xlast             NUMBER :=-181.;
  ylast             NUMBER := 91.;
  xnear             NUMBER;
  ynear             NUMBER;
  current_angle     NUMBER;
  Big               NUMBER := 1.E10;
  GTYPE             NUMBER := 2002.;
  projector         NUMBER;
  pipe_len          NUMBER;
  ring_type         NUMBER;
  aspect_ratio      NUMBER;
  threshold         NUMBER := 160.;
  min_width         NUMBER;
  last_pipe_seg     NUMBER := 0.;
  result_angle      NUMBER;
  angle_epsilon     NUMBER := 10.; -- degrees
  left             NUMBER;
  right            NUMBER;
  ahead_angle       NUMBER;
  more_ahead_angle  NUMBER;
  behind_angle      NUMBER;
  Len_so_far        NUMBER;
  temp              NUMBER;
  overlap           NUMBER;
  
  
  fpos              PLS_INTEGER :=0;
  current_seg       PLS_INTEGER;
  jj                PLS_INTEGER;
  last_jj           PLS_INTEGER := 1000000;
  found             PLS_INTEGER;
  found2            PLS_INTEGER;
  ipos              PLS_INTEGER;
  mpos              PLS_INTEGER;
  near              PLS_INTEGER;
  last_ring_seg     PLS_INTEGER;
  istart            PLS_INTEGER;
  iend              PLS_INTEGER;
  ahead             PLS_INTEGER;
  behind            PLS_INTEGER;
  hi_seg            PLS_INTEGER;
  tmp               PLS_INTEGER;
  more_tmp          PLS_INTEGER;
  more_more_tmp     PLS_INTEGER;
  more_ahead        PLS_INTEGER;
  more_more_ahead   PLS_INTEGER;
  more_more_more_ahead   PLS_INTEGER;
  more_behind       PLS_INTEGER;
  last             PLS_INTEGER;
  pos               PLS_INTEGER;
  inc               PLS_INTEGER;
  segment_found     BOOLEAN;
 
  Procedure Copy_vtx(iseg pls_integer) as
   
  Begin
--    The check for vertex 1 is necessary, for vertex 2 unnecessary but won't hurt 
--     if xlast <> Xys(iseg*2-1) or ylast <> Xys(iseg*2) then
       next := next +2;
       Ring_Xys(next-1) := Xys(iseg*2-1);
       Ring_Xys(next) := Xys(iseg*2);
--       Ring_Xys(next).x := Xys(iseg*2-1);
--       Ring_Xys(next).y := Xys(iseg*2);
--     end if;
  End;
  
  Procedure Copy_vtxp(ipos pls_integer) as
   
  Begin
       next := next +2;
       Ring_Xys(next-1) := Pipe_pars(ipos).Xp;
       Ring_Xys(next) := Pipe_pars(ipos).Yp;  
--       Ring_Xys(next).x := Pipe_pars(ipos).Xp;
--       Ring_Xys(next).y := Pipe_pars(ipos).Yp;      
  End;
  
  Procedure Copy_vtxq(ipos pls_integer) as
   
  Begin
       next := next +2;
       Ring_Xys(next-1) := Pipe_pars(ipos).Xq;
       Ring_Xys(next) := Pipe_pars(ipos).Yq;   
--       Ring_Xys(next).x := Pipe_pars(ipos).Xq;
--       Ring_Xys(next).y := Pipe_pars(ipos).Yq;      
  End;
  
  Procedure Extend_info as
  Begin
  
      Ring_info.extend(3);
           fpos := fpos + 3;
           Ring_info(fpos-2) := next+1;
           Ring_info(fpos-1) := 2;
           Ring_info(fpos) := 1;
 
  End;
      Procedure find_Info(coord pls_integer) as

--  Set istart and iend (start and end segments) for a particular ring 
--  specified by an input coordinate
    
    Begin
         for ij in 1..TRUNC(Info.count/3) loop

            If ij <> TRUNC(Info.count/3) then
               iend := Info(ij*3+1)-1;
            else
               iend := Xys.count;
            end if;
 --            dbms_output.put_line('info ' || info(ij*3-2) || ' last ' || last);
            if coord >= Info(ij*3-2) and coord <= iend then
               istart := TRUNC((Info(ij*3-2)+1)/2);  -- start segment = start vertex
               iend := TRUNC(iend/2)-1;  --end segment = end_vertex -1
 --              dbms_output.put_line('start ' || istart || ' last ' || last);
               exit;
            end if;
         end loop;

    End;


/*  
  Procedure set_near(choice VARCHAR2) as
    
  begin
 
 -- get Left near
     if choice = 'LEFT' then
     if ipos > 1 and mpos = 0 then    
        near := Pipe_pars(ipos-1).seg+1;
        if choice = 'RIGHT' then
        near := Pipe_pars(ipos+1).seg;
        end if;
     elsif ipos > 1 and mpos = 0 then
     
     
     end if;
     xnear := Xys(near*2-1);
     ynear := Xys(near*2);
     end if;
  end;
  */
 Begin

-- Pre-process the geometry so holes go clockwise so that matching segments
-- go in the opposite direction than the current segments:
--         <--987--                 islands do this   ------->
--                 |
--         ---123-->                outer polygon   ----------->

    For ii in 2.. TRUNC(Info.count/3) loop
      if Info(ii*3-1) = 2003 then
         p1 := Info(ii*3-2);
         if ii*3 <> Info.count then
           p2 := Info(ii*3+1)-1;
         else
           p2 := Xys.count;
         end if;
         -- reverse them
--         GZ_UTIL_ZONE.reverse_ordinates(Xys,p1,p2);
      end if;
    End Loop;
--    If p1 is not NULL then
--      Geom.sdo_ordinates := Xys;
--    End if;
-- Using the desired pipe type (all or peninsulas or estuaries) and the other
-- selection parameters, get the pairs of pipe segments.

-- Select on type, maximum width apart, aspect ratio (length/width) and the
-- maximum angle difference between line segments.

   Pipe_Pars := Find_Get_Pipes(Geom,pipe_type,max_width,minimum_len,min_aspect_ratio,max_angle_diff);
--   dbms_output.put_line('PPP ' || pipe_pars.count);
   If Pipe_pars.count = 0 then
  
      Return PolyGeom;  -- NULL;  
   
   End If;
   
   Xys := Geom.Sdo_Ordinates;
   Info := Geom.Sdo_Elem_Info;
   ring_count := TRUNC(Info.count/3);
   n := XYs.count;
   Pipe_Overlap.extend(n);
   nvtx := TRUNC(n/2);
   
   
-- Processor to just extract the non pipe vertices 

  
   seg := 0;    -- index for the Pipe_pars
   last_seg := -1;
   next := 0;
   np := Pipe_pars.count;
   hi_seg := Pipe_pars(np).seg;
   Ring_xys.extend(n*4);
   last := 0;
--  
   While seg < nvtx Loop
   
     seg := seg+1;
     
     pos := 0;
     mpos :=0;
     segment_found := FALSE;
     for ij in 1..np loop
        if seg = Pipe_pars(ij).seg then
          pos := ij;
          inc :=1;
          segment_found := TRUE;
          exit;
        end if;       
     end loop; 
     if pos = 0 then
       for ij in REVERSE 1..np loop
          if seg = Pipe_pars(ij).mseg then
             pos := ij;
             inc := -1;
             segment_found := TRUE;
             exit;
          end if;
      end loop;
      end if; 
     
-- If the segment is not found then we output its coordinates as part of
-- one or more rings.


     IF NOT segment_found Then
       if seg < 10 then
     dbms_output.put_line('NOT FOUND seg ' || seg || ' last ' || last_seg);
     end if;   
-- If we have encountered a gap in the Ring verticies we are copying, 
-- start new Info entry

        if seg <> last_seg+1 then
          extend_info;  
        end if;
        Copy_vtx(seg);
        last_seg := seg;
    ELSE
         

-- Check to see if we want part of the segment to make a ring
if seg < 10 then
          dbms_output.put_line('seg ' || seg || ' next ' || next || ' pos ' || pos);
        end if;
     WHILE pos > 0 and pos <= np and (Pipe_pars(pos).seg = seg or Pipe_pars(pos).mseg = seg) Loop
      projector := Pipe_pars(pos).projector;
     if inc = 1 and projector = 12 then
        NULL;
     elsif inc=1 and projector = 42 then
        if seg <> last_seg+1 then
        extend_info;
        end if;
        Copy_vtx(seg);
        Copy_vtxp(pos);
        if seg < 10 then
          dbms_output.put_line('seg ' || seg || ' next ' || next || ' fpos ' || fpos);
        end if;
        last_seg := seg;
     elsif inc = 1 and projector = 13 then
     if seg <> last_seg+1 then
        extend_info;
        end if;
        Copy_vtxq(pos); 
--        Copy_vtx(seg+1);
        last_seg := seg;
      elsif inc = 1 and projector = 43 then
      if seg <> last_seg+1 then
        extend_info;
        end if;
        Copy_vtx(seg);
        Copy_vtxp(pos);
        Extend_info;
        Copy_vtxq(pos); 
 --       Copy_vtx(seg+1);
        last_seg := seg;
     elsif inc = -1 and projector = 43 then
        NULL;
     elsif inc = -1 and projector = 42 then
        if seg <> last_seg + 1 then
        extend_info;
        end if;
        Copy_vtx(seg);
        Copy_vtxq(pos);
        last_seg := seg;
     elsif inc = -1 and projector = 13 then
        Copy_vtxp(pos); 
        --Copy_vtx(seg+1);
        last_seg := seg;
      elsif inc = -1 and projector = 12 then
        if seg <> last_seg+1 then
        extend_info;
        end if;
        Copy_vtx(seg);
        Copy_vtxq(pos);
        Extend_info;
        Copy_vtxp(pos); 
--        Copy_vtx(seg+1);      
        last_seg := seg;
      end if;
      pos := pos + inc;
      End Loop;
    End If;
    
    if next > 0 then
      xlast := Ring_Xys(next-1); --.x;
      ylast := Ring_Xys(next); --.y;
    end if;
  
   End Loop;
   
  
  Ring_Xys.trim(Ring_Xys.count-next);
  PolyGeom := MDSYS.SDO_GEOMETRY(2002,Geom.SDO_SRID,NULL,Ring_info,Ring_Xys);
  return polyGeom;
  

   
   last_coord := 0;
   last_v := 0;
   Pxys.extend(Xys.count);


    area := GZ_PIPES.polygon_area(Xys,Info,Areas);
    for ii in 1.. areas.count loop
      dbms_output.put_line('ii ' || ii || ' area ' || round(areas(ii),3));
    end loop;
    
    for ii in 1..100 loop
       dbms_output.put_line('ii ' || ii || ' ' || Class_it(ii));
    end loop;
-- For each ring 

   For ring in 1..ring_count Loop
      first_coord := last_coord+1;   -- odd
      if ring <> ring_count then
         last_coord := Info(ring*3+1) -1;   -- even
      else
         last_coord := n;
      end if;
      last_ring_seg := TRUNC(last_coord/2) -1;   -- last segment in a ring
      ring_type := Info(ring*3-1);
      last_found := -1;

-- Loop over segment numbers, from the start_segment to the end_segment
-- So we are processing the vertices as segments and ignoring the last vertex
-- which does not have a segment or an angle. (The last vertex sits on top of
-- the 1st vertex of each ring - hence no segment)

      For seg_no in TRUNC(first_coord/2)+1..TRUNC(last_coord/2)  loop

--         dbms_output.put_line('seg ' || seg_no || ' class ' || class_it(seg_no)|| ' fc ' || (TRUNC(first_coord/2)+1) ||' lc ' || (TRUNC(last_coord/2)-1) );
--       Find a segment in the Pipe_parameters
--       If its there it is a pipe or partially a pipe.

         jj := seg_no*2;
         found := 0;
         found2:= 0;
         ipos := 0;
         mpos := 0;
-- Process segments. First see if it occurs in the Pipe parameters.
-- Exit the loop if it is part of a pipe (found = segment number)

         if Class_it(seg_no) = 'P' then
           found := seg_no;
           for ii in 1..Pipe_pars.count loop
              if Pipe_pars(ii).seg = seg_no or Pipe_pars(ii).mseg = seg_no then
                ipos := ii;
                if Pipe_pars(ii).mseg = seg_no then
                   mpos := ipos;
                end if;
                exit;
              end if;
           end loop;
         end if;


--------------------------------------------------------------------------------

--         Segment was not found but it may be part of the end of the pipe
--         Check if it touches a pipe on either side
--         dbms_output.put_line('found ' || found);
/*        
         If found = 0. then

--
-- It may be part of a 'T':            |  |
--                                  ___|  |____
--                                ______.._____
--
-- If the Left distance + length of the segment + Right distance <= Allowed
-- Pipe width, then we call this a 'T' part of a pipe
 --           left := Distance_fcn(x1,y1,xnear,ynear);
            
--            If Pipe_pars(ipos).Length + left + right <= max_width then
--              Class_it(seg_no) := 'T';
--            End If;

             For ii in 1..Pipe_pars.count Loop
              if found <> -1 and (seg_no-1 = Pipe_pars(ii).seg or seg_no-1 = Pipe_pars(ii).mseg) then
                 found := found - 1;
                 dbms_output.put_line('found ' || (seg_no-1) || ' f ' || found);
              end if;
         
              if found <> -2 and (seg_no+1 = Pipe_pars(ii).seg or seg_no+1 = Pipe_pars(ii).mseg) then 
--                          (seg_no+2 = Pipe_pars(ii).seg or seg_no+2 = Pipe_pars(ii).mseg)) then
                 found := found - 2;
                 if (seg_no+2 = Pipe_pars(ii).seg or seg_no+2 = Pipe_pars(ii).mseg) then
                  dbms_output.put_line('FFound ' || (seg_no+2) || ' F ' || found);
                 else
                 dbms_output.put_line('Found ' || (seg_no+1) || ' F ' || found);
                 end if;
              end if;
              
-- Check to see if it is part of a cap;
--                                     ____
--                                     |  |
--                                  ___|  |____
--

              if (seg_no > Pipe_pars(ii).seg and seg_no < Pipe_pars(ii).mseg) and 
                  (Pipe_pars(ii).mseg - Pipe_pars(ii).seg <=6) then 
                  found := -3;
              end if;
              
-- Check to see if it is part of a corner;
--                                     
--                                     |  |
--                                  ___|  |
--                                        .   Dots show corner segsments
--                                  ____...
--

              If found <> -3 then
                if found2 <> -1 and (seg_no - Pipe_pars(ii).seg ) > 0 and (seg_no -Pipe_pars(ii).seg) < 4  then
                 found2 := found2 - 1;
                 dbms_output.put_line('11found ' || (Pipe_pars(ii).seg) || ' f ' || found || ' segno ' || seg_no);
                end if;
                if found2 =-1 and (Pipe_pars(ii).seg - seg_no) > 0 and (Pipe_pars(ii).seg - seg_no) < 4 then
                 found2 := found2 - 2;
                 dbms_output.put_line('12found ' || (Pipe_pars(ii).seg) || ' f ' || found || ' segno ' || seg_no);
                 
                end if;
                if found2 = -3 then
                   found :=-3;
                 end if;
              End If;

              exit when found = -3;
             End Loop;          
             if found <> -3 then
               found := 0.;
             end if;
                 if found=-3 and seg_no >=367 and seg_no <= 370 then
           dbms_output.put_line(seg_no||'>>corner FOUND ' || found  || ' jj ' ||jj || ' last ' || last_coord);
             end if;
           End If;
          
-- Check to see if it is just part of a few isolated segments:
--                   |   | These horizontal ones
--                   |   |_...._
--                   |         |
--                   |         |

          If found = 0 then
          
            For ii in 1..Pipe_pars.count Loop
              if found <> -1 and (Pipe_pars(ii).mseg - seg_no) > 0 and (Pipe_pars(ii).mseg - seg_no) < 4 then
                 found := found - 1;
                 seg1 := Pipe_pars(ii).mseg+1;
                 dbms_output.put_line('1found ' || (seg1) || ' f ' || found || ' segno ' || seg_no);
              end if;
         
              if found <> -2 and (seg_no -Pipe_pars(ii).mseg )  >0 and (seg_no -Pipe_pars(ii).mseg )  < 4 then              
                 found := found - 2; 
                 seg2 := Pipe_pars(ii).mseg-1;
                 dbms_output.put_line('2Found ' || (seg2) || ' F ' || found|| ' segno ' || seg_no);                
              end if;
              
              exit when found = -3;
            End Loop;
             if seg_no >=367 and seg_no <= 370 then
           dbms_output.put_line(seg_no||'>>isolated FOUND ' || found  || ' jj ' ||jj || ' last ' || last_coord);
             end if;
             if found <> -3 then
               found := 0.;
             else
--   check that the segments are linear

             result_angle := Check_angles(Xys,seg1,seg2,first_coord,last_coord,threshold);
             dbms_output.put_line('res  ' ||round(result_angle,3) || ' seg1 ' ||seg1 || ' seg2 ' || seg2);
             if  result_angle < threshold then
                found := 0;
             end if;
            end If;
         end if;
        

--         dbms_output.put_line('JJ ' ||jj || ' F ' || found || ' LF ' || last_found);
-- We still have to output the vertex to finish the last segment when the pipe
-- first starts. But not the last vertex of a pipe
 --           if seg_no >=367 and seg_no <= 370 then
--           dbms_output.put_line(seg_no||'>>**********FOUND ' || found  || ' jj ' ||jj || ' last ' || last_coord);
--             end if;

--         IF jj = last_coord AND seg_no = last_pipev+1 THEN
--           NULL;
--             if jj =2 then
--           dbms_output.put_line('NOT FOUND ' );
--             end if;
--         ELSE
--         When we found a segment, how much do we want to not output
--         If it is an isolated segment or very short it may not be part of a pipe


           if found > 0 and Pipe_pars(ipos).Accum_len < 2. then
              if ipos > 1 and Pipe_pars(ipos-1).seg < seg_no -3 then
              found := 0;
              end if;
           end if;
*/
           if Class_it(seg_no) = 'p' or Class_it(seg_no) = 'C' or Class_it(seg_no) = 'T' then
              found := seg_no;
           end if;

--         If its not a pipe output the vertex
--    if seg_no >= 705 and seg_no <= 709 then
--      dbms_output.put_line('at seg_no ' || seg_no || ' lastPV ' || last_pipev || ' found ' || found || class_it(seg_no));    
--    end if;
          
    
           if found =0  or seg_no <> last_pipev+1 then
             next := next + 2;
--             if jj =1 then
--           dbms_output.put_line('>>>>output ' || jj || ' FOUND ' || found || ' xy ' || round(xys(jj-1),7) || ',' || round(xys(jj),7)|| ' LF ' || last_found|| ' LC ' || last_coord);
--             end if;
             
             PXys(next-1) := Xys(jj-1);
             PXys(next)   := Xys(jj);
 
             last_jj:= jj;
 
--             if jj = last_coord-2 then --and Class_it(seg_no+1) = 'U' then
--             next := next + 2;
              
--              PXys(next-1) := Xys(jj+1);
--             PXys(next)   := Xys(jj+2);
--             end if;
--            elsif Class_it(seg_no)='P' then
/*
            elsif Pipe_Overlap(seg_no) >0.5 and Pipe_overlap(seg_no) < 0.95 and Class_it(seg_no)='P' and (ipos > 0 and Pipe_pars(ipos).projector = 13) then
               next := next + 2;
--             if jj =1 then
           dbms_output.put_line('>>>>output ' || jj || ' FOUND ' || found || ' xy ' || round(xys(jj-1),7) || ',' || round(xys(jj),7)|| ' LF ' || last_found|| ' LC ' || last_coord);
--             end if;
             
             PXys(next-1) := Pipe_pars(ipos).Xq;
             PXys(next)   := Pipe_pars(ipos).Yq;
            elsif Pipe_Overlap(seg_no) >0.5 and Pipe_overlap(seg_no) < 0.95 and Class_it(seg_no)='P' and (ipos > 0 and Pipe_pars(ipos).projector = 42) then
               next := next + 2;
--             if jj =1 then
           dbms_output.put_line('>>>>output ' || jj || ' FOUND ' || found || ' xy ' || round(xys(jj-1),7) || ',' || round(xys(jj),7)|| ' LF ' || last_found|| ' LC ' || last_coord);
--             end if;
             
             PXys(next-1) := Pipe_pars(ipos).Xp;
             PXys(next)   := Pipe_pars(ipos).Yp;
*/
/*             if Pipe_Overlap(seg_no) >0.5 and Pipe_overlap(seg_no) < 0.95 and   (ipos > 0 and Pipe_pars(ipos).projector = 13) then
               next := next + 2;
             PXys(next-1) := Xys(jj-1);
             PXys(next)   := Xys(jj); 

               next := next + 2;
--             if jj =1 then
           dbms_output.put_line('>>>>output ' || jj || ' FOUND ' || found || ' xy ' || round(xys(jj-1),7) || ',' || round(xys(jj),7)|| ' LF ' || last_found|| ' LC ' || last_coord);
--             end if;
             
             PXys(next-1) := Pipe_pars(ipos).Xq;
             PXys(next)   := Pipe_pars(ipos).Yq;
            elsif Pipe_Overlap(seg_no) >0.5 and Pipe_overlap(seg_no) < 0.95 and (mpos > 0 and Pipe_pars(mpos).projector = 42) then
                 next := next + 2;
             PXys(next-1) := Xys(jj-1);
             PXys(next)   := Xys(jj); 
      
               next := next + 2;
--             if jj =1 then
           dbms_output.put_line('>>>>output ' || jj || ' FOUND ' || found || ' xy ' || round(xys(jj-1),7) || ',' || round(xys(jj),7)|| ' LF ' || last_found|| ' LC ' || last_coord);
--             end if;
             
             PXys(next-1) := Pipe_pars(ipos).Xp;
             PXys(next)   := Pipe_pars(ipos).Yp;           
            end if;
            */
            END IF;

-- Create new Info string

           If (found = 0 and last_found <> 0) or (last_found=-1 and next <> 0) then
             Pinfo.extend(3);
             vcount := vcount+1;
            dbms_output.put_line('vcount' ||  (next-1));
             Pinfo(vcount*3-2) := next-1;
             Pinfo(vcount*3-1) := 2;
             Pinfo(vcount*3 ) := 1;
   
           End if;

-- Remebember the last pipe segment
           
           If found <> 0 then
             last_pipev := seg_no;
           End if;
           last_found := found;
--         END IF;

      End Loop;
   End Loop;
   
   Pxys.trim(PXys.count-next);
 
 --  Build the returned Geometry
 
    If Pxys.count <> 0 then
 
      if Pxys.count > 4 then Gtype := 2006.; End if;
 
      PolyGeom := MDSYS.SDO_GEOMETRY(GTYPE,Geom.SDO_SRID,NULL,Pinfo,PXys);
    End if;
    
  
  RETURN PolyGeom;   -- May be NULL
   
END GET_SansPipe_Segs;
--
   function seg_intersect( x1 number,y1 number,x2 number,y2 number,
                             x3 number,y3 number,x4 number,y4 number) return boolean as
    -- Check for a segment intersection between the line joining the subject 
    -- point and the projected point, and an intervening segment. The intervening
    -- segment maybe immediately before or after the current segment
       s           number;
       t           number;
       X21         number := X2-X1;
       Y21         number := Y2-Y1;
       X43         number := X4-X3;
       Y43         number := Y4-Y3;
       X31         number := X3-X1;
       Y31         number := Y3-Y1;
    
       det         number;


   begin
-- Check parametric equations for two lines: s is on line 1 from (x1,y1) to (x2,y2)
--                                           t is on line 2 from (x3,y3) to (x4,y4)
-- For example xx = (1.-s) * x1 + s*x2
--             yy = (1.-s) * y1 + s*y2

     det :=  X21 * Y43 - X43 * Y21;


     if  det <> 0.0 then
       s := (-X43 * Y31 + Y43 * X31) /det;
       t := (-X21 * Y31 + Y21 * X31) /det;
 
       If s > 0.0 and s < 1.0 and t > 0.0 and t < 1.0 then
          return TRUE;
       end if;

      end if;
      return FALSE;
   
    end seg_intersect;  
    --
FUNCTION GET_Pipe_WIDTHS(Geom IN MDSYS.SDO_GEOMETRY,Where_is IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Matches IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Distances IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                          cutoff_width NUMBER default 500.,pgap PLS_INTEGER default 2,always BOOLEAN default FALSE) RETURN MDSYS.SDO_LIST_TYPE  AS

--Find shortest gap between coordinates which are gap apart.
--Example, gap=1 find vertices too close, gap=2 find a sliver triangle

--This is the new fast version that iterates once over all segments and uses
--quiz_MBR's list of potentially interacting segments to determine which segments to check. 
--

  deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
  Xys               MDSYS.SDO_ORDINATE_ARRAY:= Geom.SDO_ORDINATES;
  List_pairs        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(); 
  Widths            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(); 
  Bad_Segs          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Info              MDSYS.SDo_ELEM_INFO_ARRAY := Geom.sdo_elem_Info;
  MBRs              MBRs_TYPE;
  N                 PLS_INTEGER;
  j                 PLS_INTEGER := 0;
  apart             PLS_INTEGER ;
  last_where        PLS_INTEGER;
  first_seg         PLS_INTEGER;
  bad_ptr1          PLS_INTEGER := 1;
  bad_seg2          PLS_INTEGER;
  bad_seg3          PLS_INTEGER;
  bad_ptr3          PLS_INTEGER := 1;
  bad_ptr3sav       PLS_INTEGER;
  no_info           PLS_INTEGER;
  check_it          PLS_INTEGER;
  mseg              PLS_INTEGER;
  next_seg          PLS_INTEGER;
  start_seg         PLS_INTEGER;
  mstart_seg        PLS_INTEGER;
  end_seg           PLS_INTEGER;
  list_ptr          PLS_INTEGER;
  loop_end          PLS_INTEGER;
  n4                PLS_INTEGER;
  checks            PLS_INTEGER :=0;
  loops             PLS_INTEGER :=0;
  seg               PLS_INTEGER;
  skip             PLS_INTEGER :=0;

  last_match        NUMBER;
  last_dist         NUMBER;
  million           NUMBER := 1000000.;
  x0                NUMBER;
  y0                NUMBER;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;
  x3                NUMBER;
  y3                NUMBER;
  ya                NUMBER;
  xb                NUMBER;
  yb                NUMBER;
  xlast             NUMBER;
  ylast             NUMBER;
  xnext             NUMBER;
  ynext             NUMBER;

  xf                NUMBER;
  yf                NUMBER;
  cosy_a            NUMBER;
  siny_a            NUMBER;
  sinysq            NUMBER;
  cos3y             NUMBER;
  cos6y             NUMBER;

  yfactor           NUMBER;
  width             NUMBER;
  width1            NUMBER;
  width2            NUMBER;
  width3            NUMBER;
  width4            NUMBER;
  xLL               NUMBER;
  yLL               NUMBER;
  xUR               NUMBER;
  yUR               NUMBER;
  xLL2              NUMBER;
  yLL2              NUMBER;
  xUR2              NUMBER;
  yUR2              NUMBER;
  xbuffer           NUMBER;
  ybuffer           NUMBER;
  xybuffer          NUMBER;
  distance          NUMBER :=0.0;
  distance_try      NUMBER :=0.0;
  distance_try1     NUMBER :=0.0;
  distance_try2     NUMBER :=0.0;
  distance_try3     NUMBER :=0.0;
  distance_try4     NUMBER :=0.0;
  dist_save         NUMBER :=0.0;
  pos               NUMBER;
  a_1               NUMBER := 0.9933056431085355608716884491956183391161;
  a_2               NUMBER := 0.009973972089654263505971285993829439678765;
  a_3               NUMBER := 0.00008445133481869081114188534958332938725393;
  a_4               NUMBER :=-0.0000000207345260587865496039317049547208138981;
  c1                NUMBER;
  c2                NUMBER;
  projector         NUMBER;
  suffix            NUMBER;
  Big               NUMBER := 1.E10;
  SRID              NUMBER := NVL(Geom.sdo_SRID,0);
  dist_factor       NUMBER := 111319.490793274;
  D2                NUMBER;
  temp              NUMBER;
  dummy             BOOLEAN;
  debug_it          BOOLEAN;
   
 
    function perpendicular_width(xin number,yin number,x0 number,y0 number,x1 number,y1 number,x2 number,y2 number,
                            x3 number,y3 number,xL number,yL number,xR number, yR number,xb number,yb number,xf number,yf number,always boolean default false) return number as
 
    -- Measure perpendicular distance in meters accurately between a point and a line
    deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
     u             number;
     delta         number;
     approx_width  number := 1.E10;
     length_sq     number;
     dx            number := x2-x1;
     dy            number := y2-y1;
     dx_along      number;
     dy_along      number;
     cosy2         number;
     xnear         number;
     ynear         number;
     x             number := xin;
     y             number := yin;
     siny          number;
     cosy          number;
     cos_delta     number;
     sin_delta     number;
     distance      number;
     
     x11           number := x1;
     y11           number := y1;
     x22           number := x2;
     y22           number := y2;
     
     cab           number;
     dx1i          number;
     dy1i          number;
     dx2i          number;
     dy2i          number;
     dx21          number;
     dy21          number;
     d1i           number;
     d2i           number;
     d21           number;
    begin
--     dbms_output.put_line('xin ' || xin || ' yin ' || yin);

-- This is the sine rule combined with the cosine rule such that D (the height 
-- of the triangle) = a sin(alpha) where alpha is the angle between a and c.
-- Cosine rule: b*b = c*c +a*a -2ac cos(alpha)

-- D = a sin(alpha) = a sqrt(1-cos(alpha)*cos(alpha)) = a sqrt(1- (c*c+a*a-b*b)^2))
--                                                         -----------------------
--                                                              2ac

--                (xin,yin)
--                +
--       a     .  |    .     b
--           .    D         .
--         .      |             .
--        +----------------------+
--      1            c           2
 
 
      dx1i := (x1-xin)*cosy_a;
      dy1i := y1-yin;
      
 
      dx2i := (x2-xin)*cosy_a;
      dy2i := y2-yin;
      

      dx21 := (x2-x1)*cosy_a;
      dy21 := y2-y1;
     
--      if ABS(dx21) + ABS(dy21) < ABS(dx1i)+ABS(dy1i) or
--          ABS(dx21) + ABS(dy21) < ABS(dx2i)+ABS(dy2i) then
--        Return approx_width;
--      end if;
 
-- generate deltas squared (read d21 as d21 squared)

      
      d1i := dx1i*dx1i + dy1i*dy1i;
      d2i := dx2i*dx2i + dy2i*dy2i;
      d21 := dx21*dx21 + dy21*dy21;
      cab := (d21+d1i -d2i)/(2.*sqrt(d21));      

--      if (d2i - cab*cab) > D2 then
--      if seg = 364 and mseg = 367 then
--       dbms_output.put_line('xin ' || xin || ',' || yin);
--        dbms_output.put_line('x1 ' || x1 || ',' || y1);
--        dbms_output.put_line('x2 ' || x2 || ',' || y2);
--        dbms_output.put_line('d1i ' || d1i);
--        dbms_output.put_line('d2i ' || d2i);
--        dbms_output.put_line('d21 ' || d21);
--        dbms_output.put_line('cab ' || cab);
--        end if;
        if d1i-cab*cab > D2 then
--        if debug_it then
--      dbms_output.put_line('(d1i minus cab*cab) ' || round((d1i-cab*cab),12) || ' D2 ' || round(D2,12));
--         end if;
         return approx_width;
        end if;





     u := ((Xin - X1) * dx + (Yin-Y1) * dy);

-- When always is set we return the distance from xin,yin to x1,y1

     If u >= 0. or always then 
        length_sq :=  dx*dx + dy*dy;
--        if always then
--        dbms_output.put_line('UU ' ||u || ' lensq ' || round(length_sq,6));
--        end if;
        if always and (u > length_sq or u < 0.) then
            u := 0.;
        end if;
        
        if u <= length_sq  and length_sq > 0. then

           u := u/length_sq;

           xnear := X1 + u * dx;
           ynear := Y1 + u * dy;
           
--           if always then
--        dbms_output.put_line('xnear ' || xnear || ' x1 ' || x1 || ' xin ' || xin||','||yin || ' x1 ' || x1 || ',' || y1|| 'x2 ' || x2 ||','||y2);
--        end if;
           -- We have a perpendicular but does it intersect either the segment
           -- before or the segment after. That is not allowed.
           -- Caller has set up xb,yb (back) and xf,yf (forward)
--           dbms_output.put_line('xin ' || xin ||','||yin ||'xnear ' || round(xnear,7) ||','||round(ynear,7));
--           dbms_output.put_line( 'xb ' || xb || ','||yb||' x1 ' || x1 || ',' || y1);

-- Check the perpendicular line from xin,yin to the segment (x1,y1 -> x2,y2) 
-- does not intersect the preceding and following segments
-- Note seg intersect does not count intersections on vertices

           if x0 is NOT NULL and seg_intersect(xin,yin,xnear,ynear,x0,y0,x1,y1) then
              Return 0.0;  -- A zero width is interpreted as a failure
           end if;
           if x3 is NOT NULL and seg_intersect(xin,yin,xnear,ynear,x2,y2,x3,y3) then
              Return 0.0;  -- zero width => failure
           end if;

-- Check the perpendicular line from xin,yin to the segment does not intersect 
-- the user specified segment


           if xb is NOT NULL and seg_intersect(xin,yin,xnear,ynear,xb,yb,xf,yf) then
--              if debug_it then
--                 dbms_output.put_line('return3 ');
--              end if;
              Return 0.0;  -- A zero width is interpreted as a failure
           end if;
 
           
           if SRID <> 8265 then
               distance := sqrt(dx*dx+dy*dy);
               dx := (xin - xnear);
               dy := (yin - ynear);
               approx_width := sqrt(dx*dx+dy*dy);
               RETURN approx_width;
           
           end if;
--           dbms_output.put_line('xnear ' || round(xnear,7) ||','||round(ynear,7));
--         
--         calculate local sines, cosines using trig identities
--         sin(a+delta) = sin_a + delta*cos_a
--         cos(a+delta) = cos_a - delta*sin_a
     
           delta := (ABS(yin+ynear)*0.5-ya)*deg2rad;
           if delta > 0.001 then
             sin_delta := sin(delta);
             cos_delta := sqrt(1-sin_delta*sin_delta);
           else
             sin_delta := delta;
             cos_delta := 1. - delta*delta*0.5;
           end if;
           siny := siny_a*cos_delta + sin_delta*cosy_a;
           cosy := cosy_a*cos_delta - sin_delta*siny_a;
           sinysq := siny*siny;
           cos3y := cosy*(1.-4.*sinysq);
           cos6y := 2.*cos3y*cos3y -1.;
  
           cosy := cosy*(c1+ siny*siny*c2);
           yfactor := (a_1+ sinysq*(a_2 + a_3*sinysq) + a_4*cos6y);

--         if meters then

            if xR is NULL then
              dx_along := (xL-xnear)*cosy;
              dy_along := (yL-ynear)*yfactor;
            else
              dx_along := (xL-xR)*cosy; --dx*cosy;
              dy_along := (yL-yR)*yfactor; --dy*yfactor;
            end if;
            
            
            dx := (xin - xnear)*cosy;
            dy := (yin - ynear)*yfactor;
             
-- Very bad Oracle error: ORA-03113: end-of-file on communication channel
            if  abs(dx) < 1.E-20 then
               dx :=0.0;
            end if;
            
            if  abs(dy) < 1.E-20 then
               dy :=0.0;
            end if;
 
             approx_width := sqrt(dx*dx+dy*dy) * dist_factor;
--             dbms_output.put_line('aw ' || round(approx_width,9) || ' dx ' || round(dx,10) || ' dy ' || round(dy,10));
             if approx_width < 0.06 then
                approx_width := gz_topofix.short_oracle_dist(xin,yin,xnear,ynear);
--                dbms_output.put_line('aw ' || round(approx_width,9));
             end if;
-- Calculate an along edge distance from xL,yL to xR,yR

             distance_try := sqrt(dx_along*dx_along+dy_along*dy_along) * dist_factor;
 
 --dbms_output.put_line('ap ' || round(distance,5));
 --           approx_dist := serena_super.fast_distance(x11,y11,x22,y22,8265.);
 --             dbms_output.put_line('AP ' || round(Approx_dist,5));
--           else   -- eof on communication channel ?? Added these 2 lines
--             dx := ROUND(Xin-xnear,9);
--             dy := ROUND(Yin-ynear,9);
--             Distance := sqrt((dx*dx + dy*dy));
--           end if;
--             if approx_width < 100. and approx_width <> 0. then
--                dbms_output.put_line('width' || round(approx_width,3) ||' xin ' || round(xin,6) || ' y ' || round(yin,6));
--                dbms_output.put_line('width' || round(approx_width,3) ||' xin ' || round(xnear,6) || ' y ' || round(ynear,6));
--             end if;
        end if;
     end if;
     RETURN approx_width;
    end perpendicular_width;
    
    
    procedure set_c1c2(y number) as
        
    begin
    
    if y < 34.5 then
      if y >= 27.5 then
--        p := 5;  -- for 27.5 .. 34.4999 degrees
        c1 := 0.99999882342768;
        c2 := 0.00335602313364;
      elsif y >= 20.5 then
--        p := 4;  -- for 20.5.. 27.4999 degrees
        c1 := 0.99999954254499;
        c2 := 0.00335267920716;
      elsif y >= 13.5 then
--        p := 3;  -- for 13.5 .. 20.4999 degrees
        c1 := 0.99999987773583;
        c2 := 0.00335000490016;
      elsif y >= 6.5 then
--        p := 2;  -- for 6.5 .. 13.4999 degrees
        c1 := 0.99999998463761;
        c2 := 0.00334815672;
      else
--        p := 1;  --for 0 .. 6.4999 degrees
        c1 := 0.99999999981136;
        c2 := 0.00334723822659;
      end if;
   elsif y < 52.5 then
      if y >= 45.5 then
--        p := 8;  --for 45.5 .. 52.4999 degrees
        c1 := 0.99999456031055;
        c2 := 0.00336625111507;

      elsif y >= 41.5 then
--        p := 7;  --for 41.5 .. 45.4999 degrees This range is different
        c1 := 0.99999581110019;
        c2 := 0.00336390796249;
      else
--        p := 6;  --for 34.5 .. 41.4999 degrees
        c1 := 0.99999759542303;
        c2 := 0.00335984120472;
      end if;
   elsif y < 59.5 then
--     p := 9;  --for 52.5 .. 59.4999 degrees
     c1 := 0.99999207002972;
     c2 := 0.00337022115208;
   elsif y < 66.5 then
--      p := 10;  -- for 59.5 .. 66.4999 degrees
      c1 := 0.99998940805689;
      c2 := 0.00337382242390;
   else  
--      p := 11;  -- 66.5 .. 90 degrees
      c1 := 0.99998688314480;
      c2 :=0.00337683963728;
   end if;

   end; 
   
   function check_bad_segs(valu pls_integer) return pls_integer as

   begin
      for kk in 1..Bad_segs.count-1 loop
        if valu = Bad_segs(kk) then
           return kk;
        end if;
      end loop;
      return 0;
   end;
   Procedure Pack(parray in out nocopy mdsys.sdo_list_type) as 
  
 -- Pack an array using the Widths array as a signal where to omit elements from 
 -- the input array.
 
 
     next  pls_integer := 0;
     
    begin
     for ii in 1..Widths.count loop
        If Widths(ii) > 0.0 then
          next := next+1;
          parray(next) := parray(ii);
        end if;
     end loop;
     parray.trim(parray.count-next);
    end;
BEGIN


-- Use of too much memory here can make Get_Widths function 7 times slower!!

  n := Xys.count/2;
  n4 := n*n;
  if n > 10 and n4 > n*4 then
     n4 := 100; --n*4;
  end if;
  n4 := 100;
--  dbms_output.put_line('N4 ' || n4 || ' N ' || N);
  Widths.extend(n4);
  
  if n4 < Where_is.count then
    Where_is.trim(Where_is.count);
    Matches.trim(Matches.count);
    Distances.trim(Distances.count);
  end if;
  
  Where_is.extend(n4-Where_is.count);
  Matches.extend(n4-Matches.count);
  Distances.extend(n4-Distances.count);

 
  ya := ABS(Xys(n*2));
  cosy_a := cos(ya*deg2rad);
  siny_a := sqrt(1.-cosy_a*cosy_a);
  set_c1c2(ya);

  if SRID = 8265. then
       xbuffer := round(meters_to_degrees('X',cutoff_width,ya),8);
       ybuffer := round(meters_to_degrees('Y',cutoff_width,ya),8);
       if ybuffer > xbuffer then
        xybuffer := ybuffer;
        else
         xybuffer := xbuffer;
       end if;
  else
       xybuffer := cutoff_width;
  end if;
  
--  dbms_output.put_line('xbuffer ' || round(xbuffer,12) || ' ybuf ' || round(ybuffer,12));
  D2 := ybuffer*ybuffer;
  
  -- set up many MBRs with a distance tolerance in degrees to check
  SET_MANY_MBRS(XYs,Info,MBRs,xLL,yLL,xUR,yUR,0,xybuffer);
 

-- for jk in 1..Mbrs.count loop
--   if mBRs(jk).mbr_list.count = 0 then
--    dbms_output.put_line('jk ' || jk || ' MBR ' || MBRS(jk).start_overlap || ' lo ' || MBRS(jk).lo || ' hi  '|| MBRS(jk).hi || ' list ' || mBRs(jk).mbr_list.count|| ' endp ' || mbrs(jk).endpoints);
-- else
--    temp :=  mBRs(jk).mbr_list(1);
--    dbms_output.put_line('jk ' || jk || ' MBR ' || MBRS(jk).start_overlap || ' lo ' || MBRS(jk).lo || ' hi  '|| MBRS(jk).hi || ' list ' || mBRs(jk).mbr_list.count || ' temp ' || temp|| ' endp ' || mbrs(jk).endpoints);  
-- end if;    
-- end loop;

  no_info := TRUNC(Info.count/3);
  Bad_segs.extend(no_info);
  
 -- First make a list of segments that dont exist from the Info (last vertex of each ring)
  For ii in 2..no_info loop 
   Bad_segs(ii-1) := TRUNC((Info(ii*3-2)+1)/2)-1;  -- point to non-existent segment
--    dbms_output.put_line('Bad ' || bad_segs(ii-1) || ' info ' || info(ii*3-2)); 
  End loop;
  Bad_segs(Bad_segs.count) := n;   -- This vertex does NOT exist
--  dbms_output.put_line('Badd ' || bad_segs(Bad_segs.count) || ' xys ' || xys.count);
  first_seg := 1; 
  bad_seg2 := Bad_Segs(1);

-- Loop over gaps, from the minimum gap to the maximum gap which is
-- comparing segment 1 with the last segment of the edge

--  FOR gap in  pgap+1..n-1 LOOP
    x1 := NULL;
    y1 := NULL;
    x2 := Xys(1);
    y2 := Xys(2);
--    apart := gap*2+2;
--     dbms_output.put_line(' apart ' || apart || 'pgap '||pgap);
--    IF gap+1 = bad_seg1 THEN   -- Dont draw a line from last vertex of ring
--    dbms_output.put_line('bad ' || bad_seg1 || ' ptr ' || bad_ptr1);
--      bad_ptr1 := bad_ptr1 + 1;
--      bad_seg1 := Bad_segs(bad_ptr1);
--      dbms_output.put_line('skipped drawing line from ' || apart ||' to ' || (apart+2));
--    ELSIF apart <= Xys.count THEN
--    dbms_output.put_line('Chose gap ' || gap || ' apart ' || apart);
--    xnext := Xys(apart-1);
--    ynext := Xys(apart);

--    bad_ptr2 :=1;
--    bad_seg2 :=  Bad_Segs(1);

-- Data loop, over every segment looking ahead with all gaps but only using
-- the segments Quiz_MBR says to check

  next_seg := 0;
  start_seg := 0;
--  dbms_output.put_line('HI' || no_info || ' n ' || n || ' pgap ' || pgap);
  loop_end := n-pgap;
  if loop_end < 2 then
    loop_end := 2;
  end if;
  
 
  FOR ii in 2..n-pgap LOOP

     seg := ii-1;
 
 --dbms_output.put_line('SEG ' ||seg || ' next ' || next_seg);
 
--    Setup the segments to check ahead

     if seg >= next_seg then
--     dbms_output.put_line('Calling QUIZ_MBR at seg ' || seg);
       list_pairs := quiz_mbr(seg,MBrs);
       start_seg := TRUNC((list_pairs(1)+1)/2);  -- may be partial Walk away
       if ii <> 2 then  skip := 2;  end if;
       next_seg := TRUNC(ABS(list_pairs(2))/2); 
--       if list_pairs(2) <> TRUNC(list_pairs(2)) then
--         next_seg := next_seg-1;
--       end if;
       if list_pairs.count=2 and list_pairs(2) < 0 then  -- complete walk away
          start_seg := next_seg;
       end if;
--       dbms_output.put_line('Start_seg ' || start_seg || ' Next seg set to ' || next_seg || ' LP count ' || List_pairs.count);
       for kk in 1..trunc(list_pairs.count/2) loop
--       dbms_output.put_line('BeforeLP ' || list_pairs(kk*2-1) || ' lp ' || list_pairs(kk*2) || ' xys ' || xys.count);
         list_pairs(kk*2-1) := TRUNC((list_pairs(kk*2-1)+1)/2);
         list_pairs(kk*2) := TRUNC(ABS(list_pairs(kk*2))/2)-1;
--       dbms_output.put_line('LP ' || list_pairs(kk*2-1) || ' lp ' || list_pairs(kk*2) || ' xys ' || xys.count);
       end loop;
     end if;
     
-- Exploit walk away and don't process these segments

     if seg < start_seg then
       skip := skip+1;
        continue;
     end if;
--    dbms_output.put_line('Seg ' || seg);
    
-- Skip the non-existent segments between rings

     check_it := check_bad_segs(seg);

     If check_it <> 0  THEN
      x1 := NULL;
      y1 := NULL;
      first_seg := 1+Bad_segs(check_it);
      x2 := Xys(first_seg*2-1);  -- move to next ring
      y2 := Xys(first_seg*2);
      skip :=0;
      bad_seg2 := Bad_segs(check_it+1);
--      dbms_output.put_line('Skipped Seg ' || seg);
      continue;
     
     End if;

--    dbms_output.put_line('ii ' || ii || ' n ' || n);
--                      Back                                       Forward
--   vertex order is:   (x0,y0)   (x1,y1)           (x2,y2)        (x3,y3)   SUBJECT
--                or    (xb,yb)   (xlast,ylast)     (xnext,ynext)  (xf,yf)   GAP AHEAD
 
     if skip = 0 then
      x0 := x1;
      y0 := y1;
      x1 := x2;
      y1 := y2;
     else
      if skip > 1 then  -- when skip =1 don't disturb x0,y0 which may be null
      x0 := Xys(ii*2-5);
      y0 := Xys(ii*2-4);      
      end if;
      x1 := Xys(ii*2-3);
      y1 := Xys(ii*2-2);
     end if;
     skip :=0;
      x2 := Xys(ii*2-1);
      y2 := Xys(ii*2);
/*
   FOR ii in 2..n-pgap LOOP

     seg := ii-1;
     
-- Exploit walk away and don't process these segments

--     if seg < start_seg then
--        continue;
--     end if;
    dbms_output.put_line('Seg ' || seg);
-- Skip the non-existent segments between rings

     check_it := check_bad_segs(seg);

     If check_it <> 0  THEN
      x1 := NULL;
      y1 := NULL;
      first_seg := 1+Bad_segs(check_it);
      x2 := Xys(first_seg*2-1);  -- move to next ring
      y2 := Xys(first_seg*2);
      bad_seg2 := Bad_segs(check_it+1);
      dbms_output.put_line('Skipped Seg ' || seg);
      continue;
     
     End if;
--dbms_output.put_line('SEG ' ||seg || ' next ' || next_seg);
--    Setup the segments to check ahead
     if seg >= next_seg then
--     dbms_output.put_line('Calling QUIZ_MBR at seg ' || seg);
       list_pairs := quiz_mbr(seg,MBrs);
       start_seg := TRUNC((list_pairs(1)+1)/2);  -- partial Walk away
--       mstart_seg := start_seg;
--       if list_pairs(1) <> TRUNC(list_pairs(1)) then
--         mstart_seg := million * (list_pairs(1) - TRUNC(list_pairs(1)));
--       end if;
       next_seg := TRUNC(ABS(list_pairs(2))/2); -- -1;
       if list_pairs.count=2 and list_pairs(2) < 0 then  -- complete walk away
          start_seg := next_seg;
       end if;
--       dbms_output.put_line('Start_seg ' || start_seg || ' Next seg set to ' || next_seg || ' LP count ' || List_pairs.count);
       for kk in 1..trunc(list_pairs.count/2) loop
--       dbms_output.put_line('BeforeLP ' || list_pairs(kk*2-1) || ' lp ' || list_pairs(kk*2) || ' xys ' || xys.count);
         list_pairs(kk*2-1) := TRUNC((list_pairs(kk*2-1)+1)/2);
         list_pairs(kk*2) := TRUNC(ABS(list_pairs(kk*2))/2)-1;
--       dbms_output.put_line('LP ' || list_pairs(kk*2-1) || ' lp ' || list_pairs(kk*2) || ' xys ' || xys.count);
       end loop;
     end if;
--    dbms_output.put_line('ii ' || ii || ' n ' || n);
--                      Back                                       Forward
--   vertex order is:   (x0,y0)   (x1,y1)           (x2,y2)        (x3,y3)   SUBJECT
--                or    (xb,yb)   (xlast,ylast)     (xnext,ynext)  (xf,yf)   GAP AHEAD
 
      x0 := x1;
      y0 := y1;
      x1 := x2;
      y1 := y2;
      x2 := Xys(ii*2-1);
      y2 := Xys(ii*2);
*/      
      if x1 < x2 then
         xLL := x1-xbuffer;
         xUR := x2+xbuffer;
      else
         xLL := x2-xbuffer;
         xUR := x1+xbuffer;
      end if;
      
      if y1 < y2 then
         yLL := y1-ybuffer;
         yUR := y2+ybuffer;
       else
         yLL := y2-ybuffer;
         yUR := y1+ybuffer;
       end if;
       
      ya := ABS(y2);
      if MOD(ii,20) = 2 then
      siny_a := quick_sincos(ya*deg2rad,cosy_a); 
      end if;
      set_c1c2(ya);
      
      if seg  < bad_seg2 then
        x3 := Xys(ii*2+1);  y3 := Xys(ii*2+2);
      else
        x3 := NULL; y3 := NULL;
      end if;
     
      list_ptr := 2;
      end_seg := List_pairs(list_pairs.count);
      
--    Loop over the segments in the ranges that Quiz_MBR has identified. The segments
--    we generate here are all valid segment numbers: mseg within each range
--    Force entry with mseg

      mseg := seg +pgap -1;
--      if mseg < mstart_seg then
--         mseg := mstart_seg;
--      end if;
--  if seg = 2341 then
--      dbms_output.put_line('Entering ' || ii|| ' seg ' || seg|| ' mseg ' || mseg || ' end ' || end_seg || ' next ' || next_seg);
--end if;
      WHILE  mseg <= end_seg LOOP
      mseg := mseg+1;

      loops := loops+1;
      While list_ptr < List_pairs.count and mseg > List_pairs(list_ptr) loop 
     
         list_ptr := list_ptr +2;
         mseg := List_pairs(list_ptr-1);
         if mseg < seg + pgap then
            mseg := seg+pgap;
         end if;
      end loop;

      pos := mseg*2; 
      xlast := Xys(pos-1);
      ylast := Xys(pos);

      exit when pos >= Xys.count;
      
-- Also exit when we are wrapping around a ring and user does not want to
-- compare first seg with the last segment in the ring (pgap=2)
-- Note that this code only handles pgap <=2. 

--      exit when seg = first_seg and mseg = bad_seg2-pgap+1;

      xnext := Xys(pos+1);
      ynext := Xys(pos+2);
      
--      if pos = 404 or (round(y2,6) = 30.240049 and round(y3,6) = 30.304216) or (round(ylast,6) = 30.240049 and round(ynext,6) = 30.304216) then
--         dbms_output.put_line('>>>>>POS ' || pos || ' gap ' || gap || ' II*2 ' || (ii*2) || ' apart ' || apart ||' bad_seg2 ' || bad_seg3 ||' xlast ' || round(xlast,6) || ',' || round(ylast,6)||','|| round(xnext,6) || ',' || round(ynext,6));
--      end if;
      
--  if seg = 2341 then
--      dbms_output.put_line('EnterED ' || ii|| ' seg ' || seg|| ' mseg ' || mseg || ' end ' || end_seg || ' next ' || next_seg);
--end if;      
       
     
       
      if xnext < xlast then
         xLL2 := xnext;
         xUR2 := xlast;
       else
         xLL2 := xlast;
         xUR2 := xnext;       
       end if;
       
       IF xLL > xUR2 or xUR < xLL2 then
          continue;
       END IF;
       
       
       
       
      if ynext < ylast then
         yLL2 := ynext;
         yUR2 := ylast;
       else
         yLL2 := ylast;
         yUR2 := ynext;       
       end if;
       
       IF yLL > yUR2 or yUR < yLL2 then
       continue;
       END IF;
       
 -- Begin by projecting the current segment onto the matching segment
--
        width := 0.0;
        
        checks := checks+1;
--        debug_it := FALSE;
--if seg = 29 and mseg > 50 and mseg <= 62 then
-- dbms_output.put_line('Checking ' || seg || ' mseg ' || mseg || ' end ' || end_seg || ' pos ' || pos);
-- dbms_output.put_line('xlast ' || xlast || ','||ylast);
-- dbms_output.put_line('xnext ' || xnext || ','||ynext);
-- dbms_output.put_line('x1 ' || x1 || ','||y1 || ' x2 ' || x2 ||','||y2);
-- debug_it := TRUE;
--       end if;
       if pos > 3 then
           xb := Xys(pos-3); yb := Xys(pos-2);
        else
           xb := NULL; yb := NULL;
        end if;
        if pos < Xys.count then
           xf := Xys(pos+1); yf := Xys(pos+2);
        else
           xf := NULL; yf := NULL;
        end if; 

-- We measure perpendicular widths from mseg to seg and vice versa.
-- Function perpendicular does not allow intersections with the segment immediately 
-- before or after. Here the vertical perpendicular intersects x2,y2 to x3,y3
--
--                            mseg
--                    xlast,ylast  xnext,ynext
--                         +--------+     
--               x3,y3  +  |
--                        .
--                         |.
---    +-----+---------------+
--  x0,y0   x1,y1     seg     x2,y2
 
        width4 := Perpendicular_width(xnext,ynext,x0,y0,x1,y1,x2,y2,x3,y3,xlast,ylast,xnext,ynext,xb,yb,xlast,ylast);

        distance_try4 := distance_try;
        width3 := Perpendicular_width(xlast,ylast,x0,y0,x1,y1,x2,y2,x3,y3,x1,y1,NULL,NULL,xnext,ynext,xf,yf,always);

        distance_try3 := distance_try;
 
--        dbms_output.put_line('calculating width1 xlast '||xlast||','||ylast||','||xnext||','||ynext);
        if x2 = xlast and y2 = ylast then
          width1 := Perpendicular_width(x1,y1,xb,yb,xlast,ylast,xnext,ynext,xf,yf,x1,y1,NULL,NULL,NULL,NULL,x3,y3);
        else       
          width1 := Perpendicular_width(x1,y1,xb,yb,xlast,ylast,xnext,ynext,xf,yf,x1,y1,NULL,NULL,x2,y2,x3,y3);
        end if;
        distance_try1 := distance_try;
        width2 := Perpendicular_width(x2,y2,xb,yb,xlast,ylast,xnext,ynext,xf,yf,x1,y1,x2,y2,x0,y0,x1,y1);
        distance_try2 := distance_try;

--if seg = 23 and mseg=24 then
--  dbms_output.put_line('w1 ' || round(width1,6) ||' w2 ' || round(width2,8) || ' w3 ' || round(width3,8) || ' w4 ' || round(width4,8));  
--end if;

        if width1 <> 0. and width1 <> Big then
          projector := 1.;
          distance := distance_try1;
          width := width1;
        end if;
        
-- vertices 1 and 4 are "leading" so take the shorter of 1 and 4

        if (width4 <> 0. and width4 <> Big) and (width = 0. or width4 < width) then
          projector := 4.;
          distance := distance_try4;
          width := width4;
        end if;
        
-- vertices 2 and 3 are "trailing" so take them only if we have no width so far.
--
--  We can get no projection when we have the "vase" shape and the projectors 
--  are off the end of the lines. With the vase shape you have the wide end 
--  usually projecting and the narrow end may not project.

-- Here we are continuing to take the smallest distances

        if width2 <> 0. and width2 <> Big and (width = 0. or width2 < width)then
          projector := 2.;
          distance := distance_try2;
          width := width2;
        end if;
        if (width3 <> 0. and width3 <> Big) and (width = 0. or width3 < width) then
          projector := 3.;
          distance := distance_try3;
          width := width3;
        end if;
 
-- dbms_output.put_line('X ' || round(x1,6) || ',' || round(y1,6) || 'Xl ' || round(xlast,6) || ','||round(ylast,6) || 'xn ' || round(xnext,6) ||','||round(ynext,6));


--          if seg = 274 and mseg = 276 then
--           dbms_output.put_line('W1 ' || round(width1,3) || ' W2 ' || round(width2,3) || ' W3 ' || round(width3,3) || ' W4 ' || round(width4,3)); 
--           dbms_output.put_line('W ' || round(width,3) || ' segment ' || trunc((pos-1)/2)|| ' with  ' || ii);       
--          end if;

        
        if width < cutoff_width and width <> 0. then
--        dbms_output.put_line('WW' || round(width,3) ||' D ' || round(distance,3) || 'at ' || seg || ' with ' || mseg);
--         dbms_output.put_line('W1 ' || round(width1,3) || ' W2 ' || round(width2,3) || ' W3 ' || round(width3,3) || ' W4 ' || round(width4,3)); 
--        dbms_output.put_line('X ' || round(x1,6) || ',' || round(y1,6) ||' XX ' ||round(x2,6) || ',' || round(y2,6) ||   'Xl ' || round(xlast,6) || ','||round(ylast,6) || 'xn ' || round(xnext,6) ||','||round(ynext,6));

          j := j+1;
          if j > Widths.count then
            Widths.extend(n4);
            Where_is.extend(n4);
            Matches.extend(n4);
            Distances.extend(n4);                   
          end if;
          
         Widths(j) := ROUND(width,3);
         Distances(j) := ROUND(distance,3);
         where_is(j) := ii-1;
         Matches(j) := TRUNC((pos)/2) +0.01*projector;  -- meaning the vertex projects
--dbms_output.put_line('SAVED ' || (ii-1) || ' with ' || Matches(j) || ' pos was ' || pos || ' W ' || Widths(j)); -- || ' gap ' || gap);
--        else
--          dist_save := distance;
        end if;
 

-- This width is never recorded iff the case is 12 and gap is the minimum gap 
/*
        if gap = pgap  then
          width := Perpendicular(x2,y2,xlast,ylast,xnext,ynext);
          projector := 2.;

          if width < cutoff_width and width <> 0. then
        dbms_output.put_line('WW' || round(width,3) ||' D ' || round(distance,3) );
        dbms_output.put_line('X ' || round(x2,7) ||','||round(y2,7) );
        dbms_output.put_line('X1 ' || round(x1,7) ||','||round(y1,7) );
        dbms_output.put_line('Xl ' || round(xlast,7) ||','||round(ylast,7) );
        dbms_output.put_line('Xn ' || round(xnext,7) ||','||round(ynext,7) );
            j := j+1;
           Widths(j) := ROUND(width,3);
           Distances(j) := ROUND(distance,3);
           where_is(j) := ii;
            Matches(j) := TRUNC((pos-1)/2) +0.01*projector;  -- meaning vertex 2 projects
           end if;        
        end if;
*/        
/*       
        width := Perpendicular(x2,y2,xlast,ylast,xnext,ynext);
        dbms_output.put_line('w ' || round(width,3)); 
        if width < cutoff_width and width <> 0. then
--        dbms_output.put_line('w ' || round(width,3) ||' d ' || round(distance,3));
--        dbms_output.put_line('x ' || round(x1,7) ||','||round(y1,7));
 
         j := j+1;
         Widths(j) := ROUND(width,3);
         Distances(j) := ROUND(dist_save,3);
         where_is(j) := ii-1.;
         Matches(j) := TRUNC(pos/2) +0.01;   -- meaning vertex 1 projects
         end if;
*/ 

      End Loop;
    End Loop;
--    END IF;
--  END LOOP;
--dbms_output.put_line('J was ' || j);

--dbms_output.put_line('CHECKS ' || checks || ' loops ' || loops);
  Shellsort4(Where_is,Matches,Widths,Distances,1,j);
  
  Where_is.trim(Where_is.count-j);
  Matches.trim(Matches.count-j);
  Widths.trim(Widths.count-j);
  Distances.trim(Distances.count-j);
  if Widths.count=0 then
     RETURN NULL;
  end if;
  
-- We can optionally compress out duplicates where we are describing the next 
-- segment's width
--II 28 M 35.02 D 20.051 W 98.57  <--
--II 29 M 35.01 D 98.57 W 98.57   <--
--II 29 M 34.02 D 59.833 W 51.583
--II 30 M 39.02 D 19.918 W 84.616
--II 30 M 38.02 D 19.918 W 86.5
--II 30 M 37.03 D 7.332 W 95.369   <--
--II 30 M 36.04 D 32.973 W 95.369  <--

  For ii in 1..Where_is.count loop
     suffix := Matches(ii) - TRUNC(Matches(ii));
     If suffix = .02 or suffix = .03 then
     For jj in ii+1..Where_is.count loop
     
         exit when suffix = .02 and Where_is(jj) > Where_is(ii)+1;
         If suffix = 0.02 and Where_is(jj) = Where_is(ii)+1 and 
              TRUNC(Matches(ii)) = TRUNC(Matches(jj)) and Widths(ii) = Widths(jj) then
            Widths(ii) := - Widths(ii);
--            dbms_output.put_line('dropped ii ' || Widths(ii));
            exit;
         End If;
         exit when suffix = .03 and Where_is(jj) > Where_is(ii)+1;
         If suffix = 0.03 and Where_is(jj) = Where_is(ii) and 
              TRUNC(Matches(ii)) = TRUNC(Matches(jj))+1 and Widths(ii) = Widths(jj) then
            Widths(jj) := - Widths(jj);
--             dbms_output.put_line('Dropped jj ' || Widths(jj));
            exit;
         End If;
     End Loop;
     End if;
  End Loop;
  
  Pack(Where_is);
  pack(Matches);
  Pack(Distances);
  Pack(Widths);
  
-- To fix a problem Matt encountered  
  if Where_is.count=0 then
     RETURN NULL;
  end if;
  
 -- Accumulate the distances to get a pipe length
  pos := 1;
  last_where :=Where_is(1);
  last_match := 0.;
  last_dist := Distances(1);
  for ii in 2..Distances.count loop
 --   dbms_output.put_line('ii ' || where_is(ii) || ' L ' || last_where || ' Diff '|| (distances(ii)-last_dist));
  
        if Where_is(ii) -last_where <= 1 then
 --          if last_match <> TRUNC(Matches(ii)) then -- and Where_is(ii) <> Where_is(ii-1) then --ABS(Distances(ii) - last_dist) > 1.5 then
             last_where := Where_is(ii); --last_where + 1;
             
--             dbms_output.put_line('adding on ' || distances(ii) ||' from ' ||ii);
             Distances(pos) := Distances(pos) + Distances(ii);
--             last_dist := Distances(ii);
--           end if;
        else
           pos := ii;
           last_where := Where_is(ii);
        end if;
        last_match := TRUNC(Matches(ii));
  end loop;
    
--  for ii in 1..Widths.count loop
--    dbms_output.put_line('II ' || Where_is(ii) || ' M ' || Matches(ii) ||' D ' || Distances(ii) || ' W ' || Widths(ii));
--  end loop;
  RETURN Widths;

END GET_Pipe_WIDTHS;
--
FUNCTION CHECK_NEARBY(Subject_XYs         IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                        Subject_Info_Array  IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                        Subject_MBR         IN OUT NOCOPY MBRS_TYPE,
                         XYs                IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         Info_Array         IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                         MBR                IN OUT NOCOPY MBRS_TYPE,
                         Codes              IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Subject_Segs       IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Matches            IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Widths_p           IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Widths_q           IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Lengths            IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Xps                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Yps                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Xqs                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Yqs                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         SRID               NUMBER,
                         angle_tolerance    NUMBER default 35.,
                         dist_tolerance     NUMBER default 500.,
                         segment_tolerance  NUMBER default 0.,
                         find               VARCHAR2 default 'B',
                         the_same           BOOLEAN default FALSE)
RETURN PLS_INTEGER AS
/*
********************************************************************************
--Program Name: check_nearby
--Author: Sidey Timmins
--Creation Date: 11/24/08

--Usage:
  -- This PL/SQL function has 22 parameters:
  --             Subject_XYs:     the array of xy coordinates of the subject 2003 polygon
  --             Subject_Info_Array:   the Elem_Info array for the subject polygon
  --             Subject_MBR:   an array of MBRs for the subject polygon
  --             XYs:          the array of xy coordinates for the test geometry
  --             Info_Array:   the Elem_Info array for the test geometry
  --             MBR:          an array of MBRs for the test geometry
  --             Matches:      an OUTPUT array to be filled with pairs of
  --                           vertex numbers describing matching parallel
  --                           segments.
  --             tolerance :  distance in meters to check for touching relationships
  --             the_same:   the 2 edges are the same edge and we are looking for
  --                         segments that are demi_parallel within the polygon.
-- Output:       result: -10: test geometry intersects the subject polygon.
--                       -111:
--                         0: test geometry is disjoint.
--
--Purpose: This function determines by discrete testing whether a geometry
--         has segments parallel to another polygon.
--
-- Reference: - Based upon an idea first demonstrated in find_segment
--              which takes the square root of the number of segments
--              and divides an edge into that many sections to find which
--              segment a clipped one came from.
--
--Dependencies: find_nearby

********************************************************************************
*/
deg2rad              NUMBER   :=0.0174532925199432957692369076848861271344;
     Sort_order      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
     x1         NUMBER;
     y1         NUMBER;
     x2         NUMBER;
     y2         NUMBER;
     xLL        NUMBER;
     yLL        NUMBER;
     xUR        NUMBER;
     yUR        NUMBER;
     xbuffer    NUMBER := dist_tolerance;
     ybuffer    NUMBER := dist_tolerance;
     siny       NUMBER;
     cosy       NUMBER;

     ii         PLS_INTEGER;
     loupe      PLS_INTEGER;
     inext      PLS_INTEGER;
     try        PLS_INTEGER := 2;
     found      PLS_INTEGER := 0;
     j          PLS_INTEGER;
     k          PLS_INTEGER;
     n          PLS_INTEGER;
     m          PLS_INTEGER;

     pstart     PLS_INTEGER := 2;
     pend       PLS_INTEGER;
   start_point      PLS_INTEGER;
   npoints          PLS_INTEGER;
   try_end          PLS_INTEGER;
   mpairs           PLS_INTEGER := TRUNC(XYs.count/2)-1;
   inc              PLS_INTEGER;
   istart           PLS_INTEGER :=2;
   modulus          PLS_INTEGER;
   loupes           PLS_INTEGER := 2;
   checkit          NUMBER;
   pos              PLS_INTEGER;
   no_of_mbrs       PLS_INTEGER;

   no_to_do         PLS_INTEGER;

   touch            PLS_INTEGER := 0;
   next             PLS_INTEGER := 0;
 
  BEGIN

-- Determine vertical and horizontal buffer distances. To give ourselves a
-- little slack, use the extreme yUR and yLL coordinates.

--select sdo_geom.sdo_length(mdsys.sdo_geometry(2002,8265,null,sdo_elem_info_array(1,2,1),
--sdo_ordinate_array(0,0,1,0)),0.05,'unit=meter') x,
--sdo_geom.sdo_length(mdsys.sdo_geometry(2002,8265,null,sdo_elem_info_array(1,2,1),
--sdo_ordinate_array(0,0,0,1)),0.05,'unit=meter') y from dual
--         X          Y
---------- ----------
--111319.491 110574.389  at equator
--85393.4091 111044.261  at latitude 40

    if SRID = 8265. then
       y2 := abs(MBR(1).xUR);    -- x needs more with increasing latitude (use yUR)
       xbuffer := meters_to_degrees('X',dist_tolerance,y2);
       y1 := abs(MBR(1).yUR);    -- y needs more with decreasing latitudes  (use yLL)
       ybuffer := meters_to_degrees('Y',dist_tolerance,y1);
    end if;

-- dbms_output.put_line('tolerance ' || dist_tolerance|| ' xbuffer ' || xbuffer || ' ybuf ' || ybuffer);
    n := Subject_Xys.count;
    if n < XYs.count then n := XYs.count; end if;

    Codes.extend(n);
    Subject_Segs.extend(n);
    Matches.extend(n);
    Widths_p.extend(n);
    Widths_q.extend(n);
    Lengths.extend(n);
    Xps.extend(n);
    Yps.extend(n);
    Xqs.extend(n);
    Yqs.extend(n);

    Matches(1) := 0;


    no_to_do := Subject_MBR(1).hi;   -- First MBR contains number to do in each

    
   -- For the first loop we divide and conquer by approximating the Mother as
   -- 40 segments going from x1,y1 to x40,y40, x80,y80, etc
   -- Then we use pairs of these to define "Windows"
   -- The inner while check to see whether the test segment overlaps a window:
   --
   --                    ---------------------- Xinext,Yinext
   --                    |     /px2,py2        |
   --                    |    /px1,py1         |
   --               x1,y1-----------------------



    ii := 1 - no_to_do;
 
 --   no_of_mbrs := Subject_MBR(5);
--dbms_output.put_line('no of mbrs ' || no_of_mbrs);
    For i in 2..MBR.count LOOP
-- Check bounding box of the "Subject" segment in question to see
        -- if the test segment overlaps

-- First MBR is the overall one and the rest start at 7;
 

-- We might have a horizontal or vertical line which almost touches
        xLL := Subject_MBR(i).xLL - xbuffer;
        yLL := Subject_MBR(i).yLL - ybuffer;
        xUR := Subject_MBR(i).xUR + xbuffer;
        yUR := Subject_MBR(i).yUR + ybuffer;
        try  := ABS(Subject_MBR(i).lo);
        try_end := TRUNC(Subject_MBR(i).hi);
--        if try_end = 1304 then
--          dbms_output.put_line('try end ' || try_end  || ' ' || Subject_MBR(i).hi);
--        end if;
        if  Subject_MBR(i).hi - try_end = .01  then
          try_end := try_end -4;
        end if;
  

--        mpairs := MBR(5);
        if the_same then
           istart := i;
        end if;
--        pos := 1 + (istart-1) * 6;
--        dbms_output.put_line('istart ' || istart || 'mpairs ' || mpairs);
        For ij in istart..MBR.count Loop
--           pos := pos + 6;
--           dbms_output.put_line('trying MBR '||i || ' with MBR '|| ij);
           x1 := MBR(ij).xLL;
           y1 := MBR(ij).yLL;
           x2 := MBR(ij).xUR;
           y2 := MBR(ij).yUR;
          -- This is the correct way to formulate this test (see any graphics book
         -- for checking whether a line overlaps a window)
-- If (the_same = TRUE and ij = i) or (x1  < xLL and x2 < xLL) or (x1  > xUR and x2 > xUR) or
         If (x1  < xLL and x2 < xLL) or (x1  > xUR and x2 > xUR) or
            (y1  < yLL and y2 < yLL) or (y1  > yUR and y2 > yUR) then
--            if try > 190 then
--            dbms_output.put_line('OUT at i ' || i || ' pstart ' || MBR(pos+4) || ' PEND ' ||MBR(pos+5) || ' try ' || try || ' try_end ' || try_end);
--            end if;
 

           NULL;              -- Its completely outside the window
         Else

              pstart := ABS(MBR(ij).lo);
              pend := MBR(ij).hi;
--              if pstart >= 500 then
--              dbms_output.put_line('ppstart ' || pstart || ' PEND ' || pend || ' try ' || try || ' end ' || try_end || ' xys ' || xys.count);
--              end if;
              if (pend >= Xys.count) then
                  pend := Xys.count;
--                  pstart := try; --  NEW March 26/09  pend - 3;
--              if pend >= 570 then
--              dbms_output.put_line('pstart CHANGED ' || pstart || ' PEND ' || pend || ' try ' || try || ' end ' || try_end);
--              end if;
              end if;
              
--            When they are the same find_nearby should not tell us about identical
--            segments
--if try >=98 then
 --               dbms_output.put_line('CALLING ' || try || ' tryend ' || try_end || ' pstart ' ||pstart || ' pend ' || pend || ' I is ' ||i);
--              end if;
              found := find_nearby(try,try_end,pstart,pend,xbuffer,ybuffer,Subject_XYs,Subject_Info_Array,XYs,Info_Array,Codes,Subject_Segs,Matches,Widths_p,Widths_q,Lengths,Xps,Yps,Xqs,Yqs,next,SRID,angle_tolerance,dist_tolerance,find,the_same);
--              if try >=98 then
--                dbms_output.put_line('FOUND ' || found || ' pstart ' || pstart || ' pend ' || pend || ' I is ' ||i);
--              end if;
              If found <> 0 then
--                dbms_output.put_line('try ' || try || ' try_end ' || try_end || ' pstart ' || pstart || ' pend ' || pend);
--                RETURN found;
                  NULL;
              End if;
         End IF;
         END Loop; -- end of loop over all test geometry MBRs

       END LOOP;   -- end of loop over all Subject MBRs

     Codes.trim(Codes.count-next);
     Subject_Segs.trim(Subject_segs.count-next);
     Matches.trim(Matches.count - next);
     Widths_p.trim(Widths_p.count-next);
     Widths_q.trim(Widths_q.count-next);
     Lengths.trim(Lengths.count-next);
     Xps.trim(Xps.count-next);
     Yps.trim(Yps.count-next);
     Xqs.trim(Xqs.count-next);
     Yqs.trim(Yqs.count-next);
     
     next := Subject_Segs.count;
     if next = 0 then
         RETURN 0;
     end if;

     Sort_order.extend(next);
     For ii in 1..Subject_Segs.count Loop
--     dbms_output.put_line('subj ' || subject_segs(ii) || ' ' || matches(ii) || ' dist ' || distances(ii));
        Sort_order(ii) := ii;
     end loop;

     Stablesort3(Subject_Segs,Matches,Sort_Order,1,next,TRUE);
     Sort_Also(Widths_p,sort_order);
     Sort_Also(Widths_q,sort_order);
     Sort_Also(Lengths,sort_order);
     Sort_Also(Codes,sort_order);
     Sort_Also(Xps,sort_order);
     Sort_Also(Yps,sort_order);
     Sort_Also(Xqs,sort_order);
     Sort_Also(Yqs,sort_order);
     
-- remove dups
     next := 0;
     For ii in 1..Subject_Segs.count Loop
-- also check that we don't tell the user that segment 1 matches segment 3 or
-- segment 1 matches segment 5 etc (segment_tolerance)
        if (ii = 1) or ((ii >1 and (Subject_Segs(ii) <> Subject_Segs(ii-1) or Matches(ii) <> Matches(ii-1)))) then
--            and (abs(Subject_Segs(ii) -Matches(ii)) > segment_tolerance)) then
           next := next+1;
           Subject_Segs(next) := Subject_Segs(ii);
           Matches(next) := Matches(ii);
 
           if ii < next then
              dbms_output.put_line('$$$$$$$$$$$$$$$$$$$$$trouble ' || next || ' j ' || ii);
           end if;
           Widths_p(next) := Widths_p(ii);
           Widths_q(next) := Widths_q(ii);
           Lengths(next) := Lengths(ii);
           Xps(next) := Xps(ii);
           Yps(next) := Yps(ii);
           Xqs(next) := Xqs(ii);
           Yqs(next) := Yqs(ii);
           Codes(next) := Codes(ii);
        end if;
     End Loop;

--dbms_output.put_line('next ' || next);

     Subject_Segs.trim(Subject_segs.count-next);
     Matches.trim(Matches.count - next);
     Widths_p.trim(Widths_p.count-next);
     Widths_q.trim(Widths_q.count-next);
     Lengths.trim(Lengths.count-next);
     Xps.trim(Xps.count-next);
     Yps.trim(Yps.count-next);
     Xqs.trim(Xqs.count-next);
     Yqs.trim(Yqs.count-next);
     Codes.trim(Codes.count-next);

     RETURN next;


END CHECK_NEARBY;
--
FUNCTION FIND_NEARBY(   try PLS_INTEGER, try_end PLS_INTEGER, pstart PLS_INTEGER, Inpend PLS_INTEGER,
                         xbuffer IN OUT NOCOPY NUMBER, ybuffer IN OUT NOCOPY NUMBER,
                         Subject_XYs        IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         Subject_Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                         XYs                IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         Info_Array         IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                         Codes              IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Subject_Segs       IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Matches            IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Widths_p           IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Widths_q           IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Lengths            IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Xps                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Yps                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Xqs                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Yqs                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         next               IN OUT NOCOPY PLS_INTEGER,
                         SRID               NUMBER,
                         angle_tolerance    NUMBER,
                         pdist_tolerance    NUMBER,
                         pfind              VARCHAR2 default 'B',
                         the_same           BOOLEAN default FALSE)
RETURN PLS_INTEGER AS
/*
********************************************************************************
--Program Name: find_nearby
--Author: Sidey Timmins
--Creation Date: 11/24/08

--Usage:
  -- This PL/SQL function has 25 parameters:
  --             try       : subscript to start search the subject Xys
  --             try_end   : subscript to stip searching the subject Xys
  --             pstart    : subscript to start searching the query Xys
  --             pend      : subscript to end searching the query Xys
  --             xbuffer, ybuffer : epsilon's to buffer vertical and horizontal lines.
  --             Subject_XY:     the array of xy coordinates of the subject 2003 polygon
  --             Subject_Info_Array:   the Elem_Info array for the subject polygon
  --             Subject_MBR:   an array of MBRs for the subject polygon
  --             XYs:          the array of xy coordinates for the test geometry
  --             Info_Array:   the Elem_Info array for the test geometry
  --             tolerance :  distance  in meters to check for touching relationships
  --
-- Output:       result: -10: test geometry intersects the subject polygon.
--                        -1: test geometry has one segment on the boundary
--                            (other segments may be inside or outside)
--                         0: test geometry is disjoint.
--
--Purpose: This function determines by discrete testing where segemnts in a
--         polygon are parallel to other segments in the polygon. Testing is over a
--         narrow range of segments determined by check_nearby.
-- Reference: -
-- Testing:
--
--Dependencies: Line_intersect

********************************************************************************
*/
deg2rad         NUMBER   :=0.0174532925199432957692369076848861271344;
     x1         NUMBER;
     y1         NUMBER;
     x2         NUMBER;
     y2         NUMBER;
     x3         NUMBER;
     y3         NUMBER;
     x4         NUMBER;
     y4         NUMBER;
     xp         NUMBER;
     yp         NUMBER;
     x          NUMBER;
     y          NUMBER;
     xq         NUMBER;
     yq         NUMBER;
     xLL        NUMBER;
     yLL        NUMBER;
     xUR        NUMBER;
     yUR        NUMBER;
     projector  NUMBER;
     distance_p NUMBER;
     distance_q NUMBER;
     tmp        NUMBER;
     overlap_length NUMBER;
     dist_tolerance NUMBER:= pdist_tolerance;
     sin_angle_tolerance NUMBER;
     cos_angle_tolerance NUMBER;
     cos_75     NUMBER;
     i          PLS_INTEGER;
     ii         PLS_INTEGER;
     inext      PLS_INTEGER;

     jpos       PLS_INTEGER;

     check_value1     PLS_INTEGER := 1324;  --Parallel
     check_value2     PLS_INTEGER := 1423;  --Antiparallel
     start_point      PLS_INTEGER;
     npoints          PLS_INTEGER;
     inc              PLS_INTEGER;
     inside           PLS_INTEGER;

     code             NUMBER;
     xc               NUMBER;
     yc               NUMBER;
     xc2              NUMBER;
     yc2              NUMBER;
     xc3              NUMBER;
     yc3              NUMBER;
     distance         NUMBER;
     delta            NUMBER;
     epsilon          NUMBER := 0.5; --0.0001;

     checkit          NUMBER;
     find             VARCHAR2(1) := SUBSTR(pfind,1,1);
     same             VARCHAR2(1) := NVL(SUBSTR(pfind,2,1),'N');
     
     inside_code      PLS_INTEGER :=1;
     ring_type        PLS_INTEGER;
     pos              PLS_INTEGER;
     pend             PLS_INTEGER := Inpend-3;
     no_of_mbrs       PLS_INTEGER;
     no_to_do         PLS_INTEGER;
     Info_count       PLS_INTEGER := TRUNC(Subject_Info_Array.count/3);
     touch            PLS_INTEGER := 0;
     ok               BOOLEAN;
     print_it         BOOLEAN;
     
    Function find_ring_type(coord pls_integer) return pls_integer as
    
--  Figure whether a coordinate falls within an inner or outer ring
    
    lend        pls_integer;
    ring_kind   pls_integer := Subject_Info_Array(2);
    
    Begin
         for ij in 2..Info_count loop
            If ij <> Info_count then
               lend := Subject_Info_Array(ij*3+1)-1;
            else
               lend := Subject_Xys.count;
            end if;

            if coord >= Subject_Info_Array(ij*3-2) and coord <= lend then
               ring_kind := Subject_Info_Array(ij*3-1);
               return ring_kind;
            end if;
         end loop;
     return ring_kind;
     End;   
     
  BEGIN
--         parallel                  anti-parallel
--      1 +--->----+ 2             1 +--->----+ 2
--          3+--->-----+4      4+------<---+3

       if find = 'B' then   -- find segments going both in same direction and opposite
          NULL;
       elsif find ='A' then   -- find just antiparallel
          check_value1 := 1423;
       elsif find = 'P' then   -- find just parallel
          check_value2 := 1324;
       end if;
--       epsilon := meters_to_degrees('X',0.025,Subject_Xys(2),0.000001)/
--                  meters_to_degrees('X',dist_tolerance,Subject_Xys(2),0.000001);
       
       sin_angle_tolerance := GZ_MATH.sincos(angle_tolerance*deg2rad,cos_angle_tolerance);
       tmp := GZ_MATH.sincos(77.*deg2rad,cos_75);
       npoints := Subject_XYs.count;
       inc := 2;
       start_point := try -inc;

       If start_point < (1-inc) then  -- USED TO BE <= 1
           start_point := 1;
       End if;
       i := start_point;
--dbms_output.put_line('start ' || i || ' try_end ' || try_end || ' pstart ' || pstart );


       While i < try_end  Loop
--          loops := loops + 1;
--          if mod(loops,100) = 0 then
--          dbms_output.put_line(' i ' || i || ' end ' || try);
--          end if;
        -- Check bounding box of the reference segment in question to see
        -- if the test segment overlaps

          ii := Mod(i,npoints)+inc;
          if ii >= npoints then
            ii := 3;
          end if;
          inext := ii + inc;
          if inext > npoints then
             inext := 3;
          end if;

-- First have to figure out which segment coordinate is greater
-- First check X
          if Subject_XYs(inext) > Subject_XYs(ii) then
            xLL := Subject_Xys(ii) - xbuffer;
            xUR := Subject_Xys(inext) + xbuffer;
          else
            xLL := Subject_Xys(inext) - xbuffer;
            yUR := Subject_Xys(ii) + xbuffer;
          end if;
-- Now check Y
         if Subject_XYs(inext+1) > Subject_XYs(ii+1) then
            yLL := Subject_Xys(ii+1) - ybuffer;
            yUR := Subject_Xys(inext+1) + ybuffer;
         else
            yLL := Subject_Xys(inext+1) -ybuffer;
            yUR := Subject_Xys(ii+1) + ybuffer;
         end if;

        pos := pstart;
        ring_type := find_ring_type(ii);
        inside_code :=1;
        if ring_type = 2003 then
           inside_code := 0;
        end if;
        

        x1 := Subject_Xys(ii);
        y1 := Subject_Xys(ii+1);
        x2 := Subject_Xys(inext);
        y2 := Subject_Xys(inext+1);
        x4 := XYs(pos);
        y4 := XYs(pos+1);
        
--         if ii = 515 or inext = 515 then
--           dbms_output.put_line('ii ' || ii || ' inext ' || inext || ' POS ' || pos || ' pend ' || pend);
--        end if;
-- dbms_output.put_line(' j' || j || ' n ' || n || ' k ' || k ||  ' m ' || m);

        While pos <= pend Loop
           jpos := pos;
           pos := pos + 2;
--           dbms_output.put_line('pos ' || pos);
           x3 := x4;
           y3 := y4;
           x4 := XYs(pos);
           y4 := XYs(pos+1);
-- Determine if the segments overlap
          -- This is the correct way to formulate this test (see any graphics book
         -- for checking whether a line overlaps a window)

         -- This test : (the_same = TRUE and abs(ii - jpos) < 3) prevents
         -- us from reporting segment n is nearly parallel with the next (touching) segemnt.

         If (the_same = TRUE and abs(ii - jpos) < 1) or 
              ii > jpos-3 or
              (x3  < xLL and x4 < xLL) or (x3  > xUR and x4 > xUR) or
              (y3  < yLL and y4 < yLL) or (y3  > yUR and y4 > yUR) then
           NULL;                               -- Its outside the window
--         if pos <= 215 and ii >= 191 then
--         dbms_output.put_line('REJECTed ' || ii || ' inext ' || inext ||' pos ' || pos || ' pend ' || pend);
--         end if;
         Else
--          if 1=1 then
--        if ii = 197 or inext = 197  then
--         dbms_output.put_line('Tested ' || ii || ' inext ' || inext ||' pos ' || pos || ' pend ' || pend);
--        end if;
--         dbms_output.put_line(' j' || j || ' n ' || n || ' k ' || k ||  ' m ' || m);
-- if ii = 15 or inext = 15 or jpos = 15 then
--   dbms_output.put_line('count ' || Subject_XYs.count);
--   dbms_output.put_line('II is ' || ii || ' inext ' || inext || ' jpos ' || jpos);
--      dbms_output.put_line('x1 ' || ROUND(x1,5) || ',' || ROUND(y1,5));
--      dbms_output.put_line('x2 ' || ROUND(x2,5) || ',' || ROUND(y2,5));
--     dbms_output.put_line('X ' || ROUND(Subject_XYs(ii),5) || ',' || ROUND(Subject_XYs(ii+1),5));
--     dbms_output.put_line('X ' || ROUND(Subject_XYs(inext),5) || ',' ||ROUND(Subject_XYs(inext+1),5));
-- end if;
--  if (ii = 1303 )  then
--  dbms_output.put_line('>>>>>>>>>>>>>>>>>>>testing ' || ii || ' pos ' ||pos || ' pstart ' || pstart || ' pend ' || pend);
--  end if;
    print_it := FALSE;
--    if (ii=643 or ii = 645) and jpos = 651 then
--       print_it:=TRUE;
--    end if;
-- Check to see if the line is semiparallel since one end of the line is within the window
           checkit := Line_parallel(x1,y1,x2,y2,x3,y3,x4,y4,
                      xp,yp,xq,yq,distance_p,distance_q,SRID,cos_angle_tolerance,dist_tolerance,cos_75,print_it);
-- if ii = 645  and jpos = 651 then
--  dbms_output.put_line('>>>>checkit ' || checkit);
--  end if;

--    if ii = 1303  then
--       dbms_output.put_line('Checkit ' || checkit || check_value1||check_value2||' pos ' || pos || ' ii ' || ii || ' D ' || distance || ' CV ' || check_value1 || ' pfind ' || pfind);
--       dbms_output.put_line('x1 ' || ROUND(x1,7) || ' y1 ' || ROUND(y1,7));
--        dbms_output.put_line('x2 ' || ROUND(x2,7) || ' y2 ' || ROUND(y2,7));
--         dbms_output.put_line('x3 ' || ROUND(x3,7) || ' , ' || ROUND(y3,7));
--        dbms_output.put_line('x4 ' || ROUND(x4,7) || ' , ' || ROUND(y4,7));
--      dbms_output.put_line('xp ' || ROUND(xp,7) || ' yp ' || ROUND(yp,7));
--      dbms_output.put_line('xq ' || ROUND(xq,7) || ' yq ' || ROUND(yq,7));
--    end if;

              distance := distance_p;
              if distance_q < distance then
                distance := distance_q;
              end if;
-- They intersect
              if checkit = 1. then
                NULL;
              Elsif checkit = 0. or TRUNC(checkit) = 1234. then
                 NULL;
              Elsif checkit = -1. or (checkit > 10. and checkit < 100.) then
--              dbms_output.put_line('checkit ' || checkit);
                 touch := -1.;
                 NULL;
-- nearly parallel lines about tolerance apart
              Elsif  distance <> 0. and (TRUNC(checkit) = check_value1 or TRUNC(checkit) = check_value2) then
--              if distance = 0. then
--                if (ii = 515 or inext = 515 ) and (jpos = 575 or jpos = 577) then
--              dbms_output.put_line('checkit ' || checkit || ' pos ' || pos || ' ii ' || ii || ' inext ' || inext || ' distance ' || ROUND(distance,4));
--              end if;
                touch := -111;

                ok := TRUE;
                For j in 1..next Loop
                   If jpos = Subject_Segs(j) and ii = Matches(j) then
                      ok := FALSE;
--                       dbms_output.put_line('ok is ffalse');
                      exit;
                   End If;
                End Loop;
--              If caller only wants to find 'P' (peninsular shaped features)
--              then an interior point will be inside.
--              River shaped features will be outside.
               -- fraction is .12 or .34 or .3
                  code := (checkit-TRUNC(checkit))*100.;
                  projector := TRUNC(code*.1);

                  
                if code = 12 then
                  overlap_length := Distance_fcn(x1,y1,x2,y2,SRID);
--                   if ii=1519 then
--                    dbms_output.put_line('>>>for ii ' || ii || ' jpos ' || jpos || ' CODE was ' || code || ' OL ' || round(overlap_length,3));
--                    dbms_output.put_line('>>>xp ' || xp || ' yp ' || yp || 'x2 ' || x2 || ' y2 ' || y2);
--                  end if;
                elsif code = 34 or code= 43 then
                  overlap_length := Distance_fcn(x3,y3,x4,y4,SRID);
                elsif code = 14 then
                dbms_output.put_line('>>>CODE is 14');
                  overlap_length := Distance_fcn(x1,y1,xq,yq,SRID);
                elsif code = 32 then
                dbms_output.put_line('>>>CODE is 32');
                  overlap_length := Distance_fcn(xp,yp,x2,y2,SRID);
                elsif code = 42 then
                  overlap_length := Distance_fcn(xp,yp,x2,y2,SRID);
                 
                  
                elsif code= 13 then
                  overlap_length := Distance_fcn(x1,y1,xq,yq,SRID);
--                  dbms_output.put_line('>>>for ii ' || ii || ' jpos ' || jpos || ' CODE was ' || code || ' OL ' || round(overlap_length,3));
                else
                  overlap_length := 0.0;
--                   dbms_output.put_line('>>>for ii ' || ii || ' jpos ' || jpos || ' CODE was unexpected ' || code  || ' checkit ' || checkit);
                end if;
--                if ii < 12 then 
--                dbms_output.put_line('>>>for ii ' || ii || ' jpos ' || jpos || ' CODE was ' || code || ' OL ' || round(overlap_length,3));
--                end if;

-- Remove everything when the ring type is a hole
-- When it is an outside ring, remove only peninsulas ('P') or estuaries ('E').

--                if ring_type=1003 and (SUBSTR(pfind,2,1) = 'P' or SUBSTR(pfind,2,1) = 'E') then
                if ring_type >=1003 and (SUBSTR(pfind,2,1) = 'P' or SUBSTR(pfind,2,1) = 'E') then
 
--                  if projector = 1 or projector = 2 then
--                    x := (x1+x2)*0.5; y := (y1+y2)*0.5;
--                  elsif projector = 3 or projector = 4 then
--                    x := (x3+x4)*0.5; y := (y3+y4)*0.5;
--                  end if;
                  if projector = 1  then
                    x := x1; y := y1;
                  elsif projector = 3 then
                    x := x3; y :=y3;
                  elsif projector = 4 then
                    x := x4; y :=y4;
                  end if;

                  if TRUNC(checkit) = 1423 and (projector = 1 or projector = 4) then
                    xc := x*(1.-epsilon) + xp*epsilon;
                    yc := y*(1.-epsilon) + yp*epsilon;
                  else
                    xc := x*(1.-epsilon) + xq*epsilon;
                    yc := y*(1.-epsilon) + yq*epsilon;
                  end if;
                  inside := POINT_IN_POLY(xc,yc,Subject_xys,Subject_Info_array);
--                   if ii >= 1521 or jpos = 1545 then
--                      dbms_output.put_line('inside ' || inside || 'code ' || inside_code || ' ii ' || ii || 'jpos ' || jpos || ' P ' || projector || ' checkit ' || checkit || ' xc ' || xc || ',' || yc);
--                   end if;
                   if (SUBSTR(pfind,2,1) = 'P' and inside = inside_code ) or
                      (SUBSTR(pfind,2,1) = 'E' and inside = 1-inside_code ) then
                      ok := TRUE;
--                      if ii = 1019 or jpos = 1191 then
--                      dbms_output.put_line('OK is TRUE ' || inside || ' RT ' || ring_type ||' code ' || inside_code || ' x ' ||round(x,7) || ' y ' || round(y,7)|| ' OL ' || round(overlap_length,2));
--                       dbms_output.put_line('OK is TRUE ' || ii || ' pos ' || pos || ' ' ||round(xc,7) || ' yc ' || round(yc,7));
--                        dbms_output.put_line('Projector' || projector || ' ' ||round(xp,7) || ' yp ' || round(yp,7));
--                        dbms_output.put_line(SUBSTR(pfind,2,1) || round(x,7) || ' y' || round(y,7));
--                        dbms_output.put_line('x1 ' || round(x1,7) || ' y1 ' || round(y1,7));
--                         dbms_output.put_line('x2 ' || round(x2,7) || ' y2 ' || round(y2,7));
--                         dbms_output.put_line('x3 ' || round(x3,7) || ' y3 ' || round(y3,7));
--                         dbms_output.put_line('x4 ' || round(x4,7) || ' y4 ' || round(y4,7));
--                    end if;
--                       if ii >= 287  and jpos = 321 then
--                      dbms_output.put_line('OK is TRUe' || round(xc,7) || ' yc ' || round(yc,7) || ' CHECKIT ' || checkit || ' ii ' || ii || 'pos ' ||pos);
--                        dbms_output.put_line('ok is TRUe' || round(x1,7) || ' yi ' || round(y1,7));
--                         dbms_output.put_line('ok is TRUe' || round(x2,7) || ' ya ' || round(y2,7));
--                       end if;
                   else
                     
                    
                      ok := FALSE;
--                      if ii = 1019 or jpos = 1191 then
--                     dbms_output.put_line('OK is faalse ' || inside || ' x ' ||round(x,7) || ' y ' || round(y,7) || ' RT ' || ring_type);
--                       dbms_output.put_line('OK is faalse' || ii || ' pos ' || pos || ' ' ||round(xc,7) || ' yc ' || round(yc,7));
--                        dbms_output.put_line('Projector' || projector || ' ' ||round(xp,7) || ' yp ' || round(yp,7));
--                        dbms_output.put_line(SUBSTR(pfind,2,1) || round(x,7) || ' y' || round(y,7));
--                        dbms_output.put_line('x1 ' || round(x1,7) || ' y1 ' || round(y1,7));
--                         dbms_output.put_line('x2 ' || round(x2,7) || ' y2 ' || round(y2,7));
--                         dbms_output.put_line('x3 ' || round(x3,7) || ' y3 ' || round(y3,7));
--                         dbms_output.put_line('x4 ' || round(x4,7) || ' y4 ' || round(y4,7));
--                    end if;
                   end if;
                end if;

--dbms_output.put_line(substr(pfind,2,1));
                if ok = TRUE then
                  next := next + 1;
                  if next > Subject_Segs.count then
                     Codes.extend(codes.count);
                     Subject_Segs.extend(Subject_Segs.count);
                     Matches.extend(Matches.count);
                     Widths_p.extend(Widths_p.count);
                     Widths_q.extend(Widths_q.count);
                     Lengths.extend(Lengths.count);
                     Xps.extend(Xps.count);
                     Yps.extend(Yps.count);
                     Xqs.extend(Xqs.count);
                     Yqs.extend(Yqs.count);
                  end if;
--                  if pos = 349 and ii = 355 then
--                  dbms_output.put_line('iii ' || ii || ' jpos ' || jpos || ' try ' || try || ' pstart ' || pstart || ' pend ' || pend|| ' D ' || round(distance,6));
--             dbms_output.put_line('x1 ' || ROUND(x1,6) || ' y1 ' || ROUND(y1,6));
--             dbms_output.put_line('x2 ' || ROUND(x2,6) || ' y2 ' || ROUND(y2,6));
--                  end if;
--                  if (the_same = TRUE and ii < jpos) or the_same = FALSE then
--dbms_output.put_line('OL ' || round(overlap_length,2) || ' code ' || code || ' ii ' || ii || ' jpos ' || jpos);
                    Subject_Segs(next) := ii;
                    Matches(next) := jpos;
                    Widths_p(next) := distance_p;
                    Widths_q(next) := distance_q;
                    Lengths(next) := overlap_length;
                    Xps(next) := xp;
                    Yps(next) := yp;
                    Xqs(next) := xq;
                    Yqs(next) := yq;
                    Codes(next) := checkit;
--                  dbms_output.put_line('>>>>>>>>>>>storing ' || checkit || ' fro jpos ' ||jpos || ' ii ' || ii || ' at next ' || next);
                end if;
--                RETURN touch;
              end if;
--            exit when next >0;
         End IF;
         END Loop; -- end of loop over test polygon's Xys
         i := i + inc;
--         exit when next >0;
       END LOOP;   -- end of loop over all segments

     RETURN touch;  -- Zero - disjoint


END FIND_NEARBY;
--
FUNCTION GET_RINGS(Geometry MDSYS.SDO_GEOMETRY,ring_order MDSYS.SDO_LIST_TYPE,ring_shifts MDSYS.SDO_LIST_TYPE default NULL) RETURN MDSYS.SDO_GEOMETRY AS

-- Get a ring or rings in a particular order that the intelligent user requires.

-- example:  select gz_pipes.get_rings(sdogeometry,mdsys.sdo_list_type(4,8,2,3,1),mdsys.sdo_list_type(0,0,0,0,0)) 
--       from txlaincplace where fullcodel=94006700

-- gets the hole (8) after outer ring 4 and then get outer rings 2, 3, and 1

-- Optionally shifts each ring by an arbitrary vertex shift (ring_shifts).

 New_Geometry   MDSYS.SDO_GEOMETRY := Geometry;
 Xys            MDSYS.SDO_ORDINATE_ARRAY := GEOMETRY.SDO_ORDINATES;
 Info           MDSYS.SDO_ELEM_INFO_ARRAY := GEOMETRY.SDO_ELEM_INFO;
 New_Info       MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY();
 ring_Xys       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
 New_Xys        MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
 Part_Xys       MDSYS.SDO_ORDINATE_ARRAY;
 
 
 no_in_ring   PLS_INTEGER;
 istart       PLS_INTEGER;
 iend         PLS_INTEGER;
 ring_count   PLS_INTEGER := TRUNC(Info.count/3);
 next         PLS_INTEGER := 0;
 shift        PLS_INTEGER := 0;
 new_ring     PLS_INTEGER := 0;
 ring         PLS_INTEGER;
 
BEGIN

-- Circulate the ring(s) by a desired ring shift so that the geometry has the
-- rings in a different order

   
   for ii in 1..ring_order.count loop
     ring := ring_order(ii);
     if Ring_shifts is NOT NULL then
        shift := Ring_shifts(ii);
     end if;
     Ring_Xys.trim(Ring_Xys.count);
     istart := Info(ring*3-2);
     if ring <> ring_count then
       iend :=  Info(ring*3+1)-1;
     else
       iend := Xys.count;
     end if;
     no_in_ring := iend-istart+1;
     Ring_xys.extend(no_in_ring);
     For ij in istart..iend loop
        Ring_Xys(ij+1-istart) := Xys(ij);
     End Loop;
     Part_Xys := Circulate_Coordinates(Ring_Xys,shift);
     
     new_ring := new_ring + 1;
     New_info.extend(3);
     New_info(new_ring*3-2) := next+1;
     New_info(new_ring*3-1) := Info(ring*3-1);
     New_info(new_ring*3) := 1;
     New_XYs.extend(no_in_ring);
     For ij in 1..no_in_ring loop
       next := next +1;
       New_Xys(next) := Part_Xys(ij);
     End Loop;
   end loop;
   
   New_Geometry.sdo_elem_info := New_info;
   New_Geometry.sdo_ordinates := New_Xys;
   RETURN New_Geometry;
   
END GET_RINGS;
--
FUNCTION CIRCULATE_COORDINATES(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,shift PLS_INTEGER) RETURN MDSYS.SDO_ORDINATE_ARRAY AS

-- Shifts a coordinate array counter clockwise by the desired shift in vertices.
-- Example, shift of 1 makes the loop begin and end at vertex 2 instead of
-- the original start and end, vertex 1.
-- Example starting with  (1,2, 3,4, 5,6, 7,8, 9,10, 11,12)
--SQL> exec gz_qa.try_circulate_coordinates(1);
-- vertex coord
--ii 1 x 3 y 4   -- note shift by 1 forward
--ii 2 x 5 y 6
--ii 3 x 7 y 8
--ii 4 x 9 y 10
--ii 5 x 11 y 12
--ii 6 x 3 y 4
-- Relative shifts of -1, -2 are accepted and treated accordingly: example
-- starting with (1,2, 3,4, 5,6, 7,8, 9,10, 11,12)
--SQL> exec gz_qa.try_circulate_coordinates(-2);
-- vertex coord
--ii 1 x 7 y 8  -- note shift backwards by 2
--ii 2 x 9 y 10
--ii 3 x 11 y 12
--ii 4 x 3 y 4
--ii 5 x 5 y 6
--ii 6 x 7 y 8

    New_Xys         MDSYS.SDO_ORDINATE_ARRAY := Xys;
    n               PLS_INTEGER := TRUNC(Xys.count/2);
    jj              PLS_INTEGER;
    ishift          PLS_INTEGER := shift;

BEGIN


  if ishift > n-1 or ishift < 0 then
    ishift := MOD(ishift,n-1);  -- hnadle relative backup too.
  end if;

  if ishift <> 0 then
    ishift := ishift*2;
    For ii in 1..n loop
       jj := ii*2;
       if jj + ishift > New_Xys.count then
          ishift := 4-jj;
       elsif jj+ishift <= 0 then
          ishift := New_xys.count-jj +ishift;
       end if;
       New_xys(jj-1) := Xys(jj-1+ishift);
       New_xys(jj) := Xys(jj+ishift);

    End loop;
  end if;
  
  RETURN New_Xys;
  
END Circulate_Coordinates;
--
Function Distance_Fcn(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER IS
/*
--##############################################################################
--Program Name: distance_fcn
--Author: Sidey Timmins
--Creation Date: 03/17/2010
--Updated:  10/17.2011 To match the  fast_distance code and reduce error
--          10/29/2010 To calculate its own sincos and to better match Charles
--          Karney's results from his excellent code.
--Usage:
  -- Call this function from inside a PL/SQL program.

  --   REQUIRED Parameter:
  --        INPUT
  --             x1,y1:  vertex coordinates in degrees (or meters)
  --             x2,y2:  2nd vertex coordinates in degrees (or meters)
  --             SRID:  the coodinate system reference identification number.
  --
--Purpose:   -- Calculates the distance between 2 points accurately. Input may
--              be projected in meters or geodetic coordinates. For the latter,
--              distance_fcn returns the geodesic distance between 2 points
--              quickly and accurately.
--              The geodesic approximation is twenty times the speed of
--              sdo_geom.sdo_length and has remarkable accuracy: typically less
--              than 1 part in 10^7. 
--              Max error: <0.3 mm (for degree differences < 0.05)  
--                         <0.6 mm of error (for 0.1 degrees difference in the coordinates).
--              The Vincenty code (used for dx or dy > 0.1 degrees)
--              is good to 0.1 mm to halfway round the ellipsoid!
--
-- Method:      Uses empirically derived constants to approximate the distance
--              using the arcsin formula for the GCD. The constants were
--              derived using the optim minimization package in GNU Octave
--              using Charles Karneys values as a reference (see geodesic package).
--              All values are more accurate than Oracle's sdo_length results.
--              All the constants and formulas are original work by Sidey Timmins.
--
-- Reference:   http://en.wikipedia.org/wiki/Great_circle_distance
--              Charles Karney:         http://geographiclib.sourceforge.net/
--Dependencies: fast_vincenty_gcd
--##############################################################################
*/
   deg2rad  CONSTANT      NUMBER   :=0.0174532925199432957692369076848861271344;
   y        NUMBER;
   dx       NUMBER;
   dy       NUMBER;
   cosy     NUMBER;
   ysin     NUMBER;
   ycos     NUMBER; 
   sinysq   NUMBER;
   cos3y    NUMBER;
   cos6y    NUMBER;

   c1       NUMBER;
   c2       NUMBER;
   c3       NUMBER;
 
   a1       NUMBER;
   a2       NUMBER;
   a3       NUMBER;
   a4       NUMBER;  

   dist_in_meters NUMBER;
   dist_factor CONSTANT NUMBER := 111319.490793274;

Begin

-- Don't know if this is what causes eof on communication channel
  dx := ROUND(x2,10)-ROUND(x1,10);
  dy := ROUND(y2,10)-ROUND(y1,10);
--  dx := x2-x1;
--  dy := y2-y1;

-- Pythagoras distance

  If SRID is NULL or SRID <> 8265.0 Then
     dist_in_meters := sqrt(dx*dx+ dy*dy);

  Elsif (abs(dx) + abs(dy) > 0.1) then
    dist_in_meters := GZ_SUPER.fast_vincenty_gcd(x1,y1,x2,y2);
  Else

-- This code is derived from ACCURATE_GCD and has been tested against
-- Charles Karney'code in the geodesic package. The results are amazing.
-- For lat/long differences(total) < 0.1 degrees, the error is < 0.001 meters.
-- For differences < 0.04 degrees, the distance error is < 0.0003 meters.
-- Provided lat/long differences are < 0.1 degrees, the relative error is
-- less than 1 part in 1 million.

    y := (y1+y2)*0.5;
    ysin := GZ_MATH.sincos(y*deg2rad,ycos);   
   
   sinysq := ysin*ysin;
   cos3y := ycos*(1.-4.*sinysq);
   cos6y := 2.*cos3y*cos3y -1.;
   
   y := ABS(y);
   
   if y <= 24. then
        c1 := 0.003347061907865575755074; 
        c2 := 0.000016826832013440621174;
        c3 := 0.000000000009352420357112;
         
   elsif y <= 36. then
        c1 := 0.0033470518185758997876606;
        c2 := 0.0000168736201830895541617;
        c3 := 0.0000000014026974169470607;
   elsif y <= 42. then
        c1 := .0033470457109299896564;
        c2 := .0000169021764208900755;
        c3 :=.0000000055242907862626;

   elsif y <= 56 then
        c1 := 0.0033470179370439039178088;
        c2 := 0.00001694243873772214493124;
        c3 :=  -0.000000003585375148916;
        cos3y := cos6y;
   else
        c1 := 0.00334701834482486001845;
        c2 := 0.00001694100614762668061439;
        c3 := -0.0000000031647677941953266;
        cos3y := cos6y;
   end if;

     cosy := ycos* (1.+ysin*ysin*c1 + ysin*ysin*ysin*ysin*c2 + cos3y*c3); 
     dx := dx*cosy;
 -- Very bad Oracle error: ORA-03113: end-of-file on communication channel
      if  abs(dx) < 1.E-20 then
         dx :=0.0;
      end if;

      if  abs(dy) < 1.E-20 then
         dy :=0.0;
      end if;
--------------------------------------------------------------------------------
--        NEW The 4 a coefficents replace all of the code in fast_distance for a1,a2
   if y <= 30. then
    a1 := 0.993305643333099948016;
    a2 := 0.009973968487145947892;
    a3 := 0.000084460038905218909;
    a4 := -0.00000002082764844523;
  elsif y <= 50. then
    a1 := 0.993305643012120789093;
    a2 := 0.009973973110766169983;
    a3 := 0.000084449884961131338;
    a4 := -0.00000002061530927320;
  else
    a1 := 0.99330564278475112093181;
    a2 := 0.00997397339160238707762;
    a3 := 0.00008445028915233565374;
    a4 := -0.0000000207580988718419;  
  end if; 
    dy := dy * (a1+ a2*sinysq + a3*sinysq*sinysq + a4*cos6y);

    dist_in_meters := sqrt(dy*dy + dx*dx) * dist_factor;
 
  End if;

  Return dist_in_meters;

End DISTANCE_FCN;
--
Function NEWDistance_Fcn(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER IS
/*
--##############################################################################
--Program Name: distance_fcn
--Author: Sidey Timmins
--Creation Date: 03/17/2010
--Updated:  10/17.2011 To match the  fast_distance code and reduce error
--          10/29/2010 To calculate its own sincos and to better match Charles
--          Karney's results from his excellent code.
--Usage:
  -- Call this function from inside a PL/SQL program.

  --   REQUIRED Parameter:
  --        INPUT
  --             x1,y1:  vertex coordinates in degrees (or meters)
  --             x2,y2:  2nd vertex coordinates in degrees (or meters)
  --             SRID:  the coodinate system reference identification number.
  --
--Purpose:   -- Calculates the distance between 2 points accurately. Input may
--              be projected in meters or geodetic coordinates. For the latter,
--              distance_fcn returns the geodesic distance between 2 points
--              quickly and accurately.
--              The geodesic approximation is twenty times the speed of
--              sdo_geom.sdo_length and has remarkable accuracy: typically less
--              than 1 part in 10^7. 
--              Max error: <0.3 mm (for degree differences < 0.05)  
--                         <1.5 mm of error (for 0.1 degrees difference in the coordinates).
--              The Vincenty code (used for dx or dy > 0.1 degrees)
--              is good to 0.1 mm to halfway round the ellipsoid!
--
-- Method:      Uses empirically derived constants to approximate the distance
--              using the arcsin formula for the GCD. The constants were
--              derived using the optim minimization package in GNU Octave
--              using Charles Karneys values as a reference (see geodesic package).
--              All values are more accurate than Oracle's sdo_length results.
--              All the constants and formulas are original work by Sidey Timmins.
--
-- Reference:   http://en.wikipedia.org/wiki/Great_circle_distance
--              Charles Karney:         http://geographiclib.sourceforge.net/
--Dependencies: fast_vincenty_gcd
--##############################################################################
*/
   deg2rad  CONSTANT      NUMBER   :=0.0174532925199432957692369076848861271344;
   y        NUMBER := abs(y1);
   dx       NUMBER;
   dy       NUMBER;
   cosy     NUMBER;
   cosy2    NUMBER;
   ysin     NUMBER;
   ycos     NUMBER;
   ycos2    NUMBER;
   ysin2    NUMBER;
   cosdy    NUMBER;
   sinysq   NUMBER;
   cos3y    NUMBER;
   cos6y    NUMBER;
   cos4i    NUMBER;
   c1       NUMBER;
   c2       NUMBER;
   c3       NUMBER;
   c4       NUMBER :=0.0;
   a1       NUMBER :=  0.99330564310853556;
   a2       NUMBER :=  0.0099739720896542635;
   a3       NUMBER :=  0.0000844513348186908111;
   a4       NUMBER := -0.0000000207345260587865496;
   delta    NUMBER;
   tmp      NUMBER;
   sindelta NUMBER;

   dist_in_meters NUMBER;
   dist_factor CONSTANT NUMBER := 111319.490793274;

Begin

-- Don't know if this is what causes eof on communication channel
  dx := ROUND(x2,10)-ROUND(x1,10);
  dy := ROUND(y2,10)-ROUND(y1,10);
  if dx <> 0. then
        tmp := ABS(dy/dx);
        cos4i := (1. - tmp*tmp)/(1+tmp*tmp);
        cos4i := 2.*cos4i*cos4i-1.;
--        dbms_output.put_line('ii ' || ii || ' cos4i ' || round(cos4i,9) || ' cos ' || round(cos(4*ii*deg2rad),9));
     else
        cos4i := 1.;
     end if;
-- Pythagoras distance

  If SRID is NULL or SRID <> 8265.0 Then
     dist_in_meters := sqrt(dx*dx+ dy*dy);

--  Elsif (abs(dx) + abs(dy) > 0.1) then
--    dist_in_meters := GZ_SUPER.fast_vincenty_gcd(x1,y1,x2,y2);
  Else

-- This code is derived from Accurate_GCD and has been tested against
-- Charles Karney'code in the geodesic package. The results are amazing.
-- For lat/long differences(total) < 0.1 degrees, the error is < 0.001 meters.
-- For differences < 0.04 degrees, the distance error is < 0.0003 meters.
-- Provided lat/long differences are < 0.1 degrees, the relative error is
-- less than 1 part in 1 million.

    y := (y1+y2)*0.5;
    ysin := GZ_MATH.sincos(y1*deg2rad,ycos);
    delta := (y2-y1)*0.5*deg2rad;           -- calculate sin((y1+y2)*0.5)
          ycos2 := ycos - ysin * delta*2.;        -- small angle approximation for cos(y2)
    cosdy := 1. - delta*delta*0.5;          -- using sin(a+eps) = sin(a)cos(eps) + cos(a)sin(eps)
    sindelta := delta - delta*delta*delta/6.; -- make sin of delta using Taylor

--    if y1 < 0.0 then
--      ysin := -ysin;
--    end if;
    ysin2 := ysin*cosdy + ycos*sindelta;      -- small angle approximation for formula above

-- this code is just to handle user error when y > 90
--   if cosy < 0. then
 --     ysin2 := GZ_MATH.sincos(y2*deg2rad,ycos2);
--   end if;
   
   
   sinysq := ysin2*ysin2;
   cos3y := ycos*(1.-4.*sinysq);
   cos6y := 2.*cos3y*cos3y -1.;
   if y <= 24. then
        c1 := 0.003347061907865575755074; 
        c2 := 0.000016826832013440621174;
        c3 := 0.000000000009352420357112;
         
   elsif y <= 36. then
        c1 := 0.0033470518185758997876606;
        c2 := 0.0000168736201830895541617;
        c3 := 0.0000000014026974169470607;
   elsif y <= 42. then
        c1 := 0.0033470457534885715433906;
        c2 := 0.00001690186892214974728357;
        c3 := 0.00000000545305593665276185;
        c1 :=.003347045719749241351;
 c2 :=.000016902034110228922;
 c3 := 0.000000005481953660958;
--        c1 := .99999994945253288561;
--        c2 := .00334730515225742928;
--        c3 := .00001655604322520772;
          c1 := .0033470457109299896564;
          c2 := .0000169021764208900755;
          c3 :=.0000000055242907862626;
--          c1 := .003347138132048280674;
--          c2 := .000016669920059231121;
--          c3 := .000000005699012787096;
--          c4 := -.00000014211669736440;

   elsif y <= 56 then
        c1 := 0.0033470179370439039178088;
        c2 := 0.00001694243873772214493124;
        c3 :=  -0.000000003585375148916;
        cos3y := cos6y;
   else
        c1 := 0.00334701834482486001845;
        c2 := 0.00001694100614762668061439;
        c3 := -0.0000000031647677941953266;
        cos3y := cos6y;
   end if;
/*
    if y <= 24. then
         c1 := 0.0033470620336923338253;
         c2 := 0.0000168264347199898364;
         c3 := 0.00000000000921879373328;
--         c1 := 0.003347061521327742575;
--         c2 := 0.00001683135553163179;
--         c3 := 0.0000000000175686319;
    elsif y <= 34 then --
         c1 := 0.0033470720173417586018;
         c2 := 0.00001682349775009371217;
         c3 := 0.0000000019062699749546;
--         c1 :=0.003346990809897268017;
--         c2 :=0.0000169964964124559591;
--         c3 :=-0.000000007576881601715;
    elsif y < 48 then
         c1 := 0.00334700970184269469619;
         c2 := 0.0000169587923430779926736;
         c3 := -0.0000000049235088957732;
    else
         c1 := 0.0033470187473401207076;
         c2 := 0.0000169406413549998409;
         c3 :=-0.000000003273932628759;
    end if;
*/    

--    if dx <> 0. then
--    cosy := ycos* (1.+ysin*ysin*c1 + ysin*ysin*ysin*ysin*c2 + cos3y*c3 + c4*ABS(dy/dx));
--    else
     cosy := ycos* (1.+ysin*ysin*c1 + ysin*ysin*ysin*ysin*c2 + cos3y*c3); 
--     (1.000000009115 -0.000000133376* ABS(dy/dx));
--    end if;

 -- Very bad Oracle error: ORA-03113: end-of-file on communication channel
      if  abs(dx) < 1.E-20 then
         dx :=0.0;
      end if;

      if  abs(dy) < 1.E-20 then
         dy :=0.0;
      end if;
--------------------------------------------------------------------------------
--        NEW The 4 a coefficents replace all of the code in fast_distance for a1,a2
  if y <= 30 then
    a1 :=.993305643333099948016;
    a2 := .009973968487145947892;
   a3 := .000084460038905218909;
   a4 := -.00000002082764844523;
  elsif y <= 50 then
   a1 :=   .993305643012120789093;
   a2 := .009973973110766169983;
   a3 := .000084449884961131338;
   a4 := -.00000002061530927320;
  else
 a1 :=  .99330564278475112093181;
  a2 := .00997397339160238707762;
a3 := .00008445028915233565374;
a4 := -.0000000207580988718419;
  
  end if;
          dy := dy * (a1+ a2*sinysq + a3*sinysq*sinysq + a4*cos6y);
--dbms_output.put_line('dx ' || round(dx,12) || ' dy ' || round(dy,12));
--------------------------------------------------------------------------------
--dbms_output.put_line('c1' || c1 || ' c2 ' ||  c2  || 'cosy ' || round(cosy,12) || ' dy ' || round(dy,12));
          if abs(dy) >=  0.0001 then
             cosy2 := cosy - ysin * dy*deg2rad; -- small angle approximation for cos(y2)
             --           - 2.5E-9*(1.-cos4i)
             dist_in_meters := (sqrt(dy*dy + dx*dx*cosy*cosy2)) * dist_factor;
          else
             dist_in_meters := (sqrt(dx*dx*cosy*cosy + dy*dy) )* dist_factor;
          end if;
  End if;

  Return dist_in_meters;

End NEWDISTANCE_FCN;
--
Function Old_Distance_Fcn(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER IS
/*
--------------------------------------------------------------------------------
--Program Name: distance_fcn
--Author: Sidey Timmins
--Creation Date: 03/17/2010
--Updated:  10/17.2011 To match the  fast_distance code and reduce error
--          10/29/2010 To calculate its own sincos and to better match Charles
--          Karney's results from his excellent code.
--Usage:
  -- Call this function from inside a PL/SQL program.

  --   REQUIRED Parameter:
  --        INPUT
  --             x1,y1:  vertex coordinates in degrees (or meters)
  --             x2,y2:  2nd vertex coordinates in degrees (or meters)
  --             SRID:  the coodinate system reference identification number.
  --
--Purpose:   -- Calculates the distance between 2 points accurately. Input may
--              be projected in meters or geodetic coordinates. For the latter,
--              distance_fcn returns the geodesic distance between 2 points
--              quickly and accurately.
--              The geodesic approximation is twenty times the speed of
--              sdo_geom.sdo_length and has remarkable accuracy: typically less
--              than 1 part in 10^7. 
--              Max error: <0.3 mm (for degree differences < 0.05)  
--                         <1.5 mm of error (for 0.1 degrees difference in the coordinates).
--              The Vincenty code (used for dx or dy > 0.1 degrees)
--              is good to 0.1 mm to halfway round the ellipsoid!
--
-- Method:      Uses empirically derived constants to approximate the distance
--              using the arcsin formula for the GCD. The constants were
--              derived using the optim minimization package in GNU Octave
--              using Charles Karneys values as a reference (see geodesic package).
--              All values are more accurate than Oracle's sdo_length results.
--              All the constants and formulas are original work by Sidey Timmins.
--
-- Reference:   http://en.wikipedia.org/wiki/Great_circle_distance
--              Charles Karney:         http://geographiclib.sourceforge.net/
--Dependencies: fast_vincenty_gcd
--##############################################################################
*/
   deg2rad  CONSTANT      NUMBER   :=0.0174532925199432957692369076848861271344;
   y        NUMBER := abs(y1);
   dx       NUMBER;
   dy       NUMBER;
   cosy     NUMBER;
   cosy2    NUMBER;
   ysin     NUMBER;
   ycos     NUMBER;
   ycos2    NUMBER;
   ysin2    NUMBER;
   cosdy    NUMBER;
   sinysq   NUMBER;
   cos3y    NUMBER;
   cos6y    NUMBER;
   c1       NUMBER;
   c2       NUMBER;

   a1       CONSTANT NUMBER :=  0.99330564310853556;
   a2       CONSTANT NUMBER :=  0.0099739720896542635;
   a3       CONSTANT NUMBER :=  0.0000844513348186908111;
   a4       CONSTANT NUMBER := -0.0000000207345260587865496;
   delta    NUMBER;
   sindelta NUMBER;

   dist_in_meters NUMBER;
   dist_factor CONSTANT NUMBER := 111319.490793274;

Begin

-- Don't know if this is what causes eof on communication channel
  dx := ROUND(x2,10)-ROUND(x1,10);
  dy := ROUND(y2,10)-ROUND(y1,10);

-- Pythagoras distance

  If SRID is NULL or SRID <> 8265.0 Then
     dist_in_meters := sqrt(dx*dx+ dy*dy);

--  Elsif (abs(dx) + abs(dy) > 0.1) then
--    dist_in_meters := GZ_SUPER.fast_vincenty_gcd(x1,y1,x2,y2);
  Else

-- This code is derived from Accurate_GCD and has been tested against
-- Charles Karney'code in the geodesic package. The results are amazing.
-- For lat/long differences(total) < 0.1 degrees, the error is < 0.001 meters.
-- For differences < 0.04 degrees, the distance error is < 0.0003 meters.
-- Provided lat/long differences are < 0.1 degrees, the relative error is
-- less than 1 part in 1 million.

    ysin := GZ_MATH.sincos(y*deg2rad,ycos);
    delta := (y2-y1)*0.5*deg2rad;           -- calculate sin((y1+y2)*0.5)
---          ycos2 := ycos - ysin * delta*2.;        -- small angle approximation for cos(y2)
    cosdy := 1. - delta*delta*0.5;          -- using sin(a+eps) = sin(a)cos(eps) + cos(a)sin(eps)
    sindelta := delta - delta*delta*delta/6.; -- make sin of delta using Taylor

    if y1 < 0.0 then
      ysin := -ysin;
    end if;
    ysin2 := ysin*cosdy + ycos*sindelta;      -- small angle approximation for formula above


   if y < 34.5 then
      if y >= 27.5 then
--        p := 5;  -- for 27.5 .. 34.4999 degrees  t = 31 (centerpoint)
        c1 := 0.99999882342768;
        c2 := 0.00335602313364;
      elsif y >= 20.5 then
--        p := 4;  -- for 20.5.. 27.4999 degrees   t = 24
        c1 := 0.99999954254499;
        c2 := 0.00335267920716;
      elsif y >= 13.5 then
--        p := 3;  -- for 13.5 .. 20.4999 degrees  t = 17
        c1 := 0.99999987773583;
        c2 := 0.00335000490016;
      elsif y >= 6.5 then
--        p := 2;  -- for 6.5 .. 13.4999 degrees   t = 10
        c1 := 0.99999998463761;
        c2 := 0.00334815672;
      else
--        p := 1;  --for 0 .. 6.4999 degrees       t =  3
        c1 := 0.99999999981136;
        c2 := 0.00334723822659;
      end if;
   elsif y < 52.5 then
      if y >= 45.5 then
--        p := 8;  --for 45.5 .. 52.4999 degrees    t = 49
        c1 := 0.99999456031055;
        c2 := 0.00336625111507;
      elsif y >= 41.5 then
--        p := 7;  --for 41.5 .. 45.4999 degrees This range is different
        c1 := 0.99999581110019;
        c2 := 0.00336390796249;
--        t := 45.;      -- nominal centerpoint for this unusual range
      else
--        p := 6;  --for 34.5 .. 41.4999 degrees    t = 38
        c1 := 0.99999759542303;
        c2 := 0.00335984120472;
      end if;
   elsif y < 59.5 then
--     p := 9;  --for 52.5 .. 59.4999 degrees       t = 56
     c1 := 0.99999207002972;
     c2 := 0.00337022115208;
   elsif y < 66.5 then
--      p := 10;  -- for 59.5 .. 66.4999 degrees    t = 63
      c1 := 0.99998940805689;
      c2 := 0.00337382242390;
   else
--      p := 11;  -- 66.5 .. 90 degrees             t = 70
      c1 := 0.99998688314480;
      c2 :=0.00337683963728;
   end if;

   cosy := ycos* (c1+ ysin*ysin*c2);

-- this code is just to handle user error when y > 90
   if cosy < 0. then
      ysin2 := GZ_MATH.sincos(y2*deg2rad,ycos2);
   end if;

 -- Very bad Oracle error: ORA-03113: end-of-file on communication channel
      if  abs(dx) < 1.E-20 then
         dx :=0.0;
      end if;

      if  abs(dy) < 1.E-20 then
         dy :=0.0;
      end if;
--------------------------------------------------------------------------------
--        NEW The 4 a coefficents replace all of the code in fast_distance for a1,a2
          sinysq := ysin2*ysin2;
          cos3y := ycos*(1.-4.*sinysq);
          cos6y := 2.*cos3y*cos3y -1.;

          dy := dy * (a1+ a2*sinysq + a3*sinysq*sinysq + a4*cos6y);
--dbms_output.put_line('c1' || c1 || ' c2 ' ||  c2  || 'cosy ' || round(cosy,12) || ' dy ' || round(dy,12));
--------------------------------------------------------------------------------

          if abs(dy) >=  0.0001 then
             cosy2 := cosy - ysin * dy*deg2rad; -- small angle approximation for cos(y2)
             dist_in_meters := sqrt(dy*dy + dx*dx*cosy*cosy2) * dist_factor;
          else
             dist_in_meters := sqrt(dx*dx*cosy*cosy + dy*dy) * dist_factor;
          end if;
  End if;

  Return dist_in_meters;

End Old_DISTANCE_FCN;
--
FUNCTION FASTER_ATAN2(YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER AS

-- A very fast arctan function about 15 times faster than Oracle with maximum
-- error about 4E-15. See Kurzweg and Timmins in GZ_Math_new_arctan.
-- Returned angles are measured anti-clockwise from the X axis. (Note by
-- convention, the first argument is Y not X.)

   pi        CONSTANT NUMBER    := 3.1415926535897932384626433832795028842;
   piByTwo   CONSTANT NUMBER    := 1.5707963267948966192313216916397514421;
   piBy6     CONSTANT NUMBER    := 0.5235987755982988730771072305465838140;
   tanpiby6  CONSTANT NUMBER    := 0.577350269189625764509148780501957455647;
   tanpiby12 CONSTANT NUMBER    := 0.2679491924311227064725536584941276331;

   rad2deg   CONSTANT NUMBER    := 57.29577951308232087679815481410517033240;

   x           NUMBER;
   xx          NUMBER;
   result     NUMBER;
   region      NUMBER := 0; 
   u           NUMBER;
   absx        NUMBER := ABS(Xin);
   absy        NUMBER := ABS(Yin);

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


-- Try and avoid a division
  IF xin = 0.0 or yin = 0.0 then
    x := 0.;
  ELSIF absx < tanpiby12 * absy THEN
     x := absy/absx;

  ELSIF absy < tanpiby12 * absx THEN
     x   := absx/absy;

  ELSIF absx >= absy THEN
     xx   := absy/absx;
     if (xx > tanpiby12) THEN  
       x := (1.0 +tanpiby6*xx)/ (xx-tanpiby6);
       region := piby6;
     elsif xx <> 0.0 then
       x := 1./xx;
     end if;
  ELSIF absy > absx THEN
     xx   := absx/absy;
     if (xx > tanpiby12) THEN
       x := (1.0 +tanpiby6*xx)/ (xx-tanpiby6);
       region := piby6;
     elsif xx <> 0.0 then
       x := 1./xx;
    end if;
  END IF;

-- Calculate but take care of very small x

     if x > 1.E15 then
      result := 1./x + region;
    else

      u := x*x;
-- Use Horner's rule to evaluate.
-- Other less accurate formulas (See reference).

---    result := (x*(c1 + u*c2))/(c3 + u) + region;
--     result := x*(55. + 105.*u)/(9.+u*(90. + 105*u)) + region;
--     result := x*(231. + u*(1190.+1155.*u))/ (25.+ u*(525.+ u*(1575. + 1155.*u))) + region;

     result := x*(3031.8 + u*(29491.+u*(69069.+45045*u)))/ (245.+ u*(8820.+ u*(48510. + u*(84084.+u*45045.))))
                 + region;
    end if;

  IF absy > absx THEN
     result := pibyTwo -result;
  END IF;

  if XIn < 0.0 then  result := pi-result;  end if;

  if YIn < 0.0 then  result := -result;  end if;
  
  If degrees Then
     result := result *rad2deg;
     if result < 0.0 then
       result := result + 360.;
     end if;
  End if;

  RETURN result;


END FASTER_ATAN2;
--
FUNCTION Find_Distances(x IN OUT NOCOPY NUMBER,y IN OUT NOCOPY NUMBER,XYS IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,GC_distances  IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,pstart PLS_INTEGER default 1,pend PLS_INTEGER default 0,pnvert_outer PLS_INTEGER default 0,forwards VARCHAR2 default 'ASC') RETURN MDSYS.SDO_LIST_TYPE IS
/*
**************************************************************************************
--Program Name: Find_distances
--Author: Sidey Timmins
--Creation Date: 08/10/2009
--Usage:
  --   REQUIRED Parameters: 
  --            (x,y):        coordinates for the reference point (rf)
  --            Xys:          coordinates of the geometry to measure to from the rf
  --            Gc_distances: output array to store the distances in
  --            pstart:       vertex to start at in XYs (defaults to 1)
  --            pend:         vertex to end at in XYs. Note pstart may be greater than
  --                          pend to enable searching from say vertex 90 thru 100 (= 
  --                          vertex 1) to pend = 10.
  --            nvert_outer:  vertex to end or the # vertices in the outer ring
  --            forwards:     if true, the vertex order and distances are near to far
  --                           and vice versa.
  --
--Purpose:      Finds distances in order (from close to far when forwards=TRUE)  
--              from the input reference point (x,y) to each vertex of the XYs. 
--              Returns: Vertex order (vertices ordered by increasing distance).
--
-- Method:      Calculates the distance from the reference point to each vertex 
--              in XYs and then sorts.
-- Dependencies: distance_fcn
***************************************************************************************
*/
   Vertex_order   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

   xtest          NUMBER;
   ytest          NUMBER;
   istart         PLS_INTEGER := pstart*2-1;
   iend           PLS_INTEGER := pend*2;
   n              PLS_INTEGER := pnvert_outer*2; 
   kount          PLS_INTEGER;
   n2             PLS_INTEGER;
   jj             PLS_INTEGER := 0;
   ii             PLS_INTEGER;
   ij             PLS_INTEGER;
   
BEGIN

    if n <= 0 then n := Xys.count; end if;
 
    -- Setup the range which may or may not go through the last vertex   
--            example     20 .. 50
    if pend >= pstart then
      kount := iend-istart+1;
    else      
--            example     90 to 100 and then 1 .. 10  goes through last vertex
--              
      kount := iend + n -istart +1;
    end if;
    
    n2 := istart + kount-2;
    if kount <= 0 then
       dbms_output.put_line('n ' || n || ' p ' || pstart);
    end if;
    
    Vertex_Order.extend(TRUNC(kount/2));
    gc_distances.trim(gc_distances.count);
    GC_distances.extend(TRUNC(kount/2));
   
--   dbms_output.put_line('n2 ' || n2 || ' istart ' || istart || ' iend ' || iend|| ' pstart ' || pstart || ' pend ' || pend || 'nvo ' || pnvert_outer|| ' k ' || kount);
 
    -- To handle a single range thru the end of a loop, we
    -- allow istart to be > iend     So 90 to  100 and 1 to 10
 
    ii := istart-2;
    While ii < n2 loop
      ii := ii+2; 
      ij := ii;
      if ij > n then ij := ij -n; end if;
      jj := jj+1;
      xtest := Xys(ij);
      ytest := Xys(ij+1);
         
      -- Use this empirical function to get a very accurate approximation to 
      -- the great circle distance.

      gc_distances(jj) := distance_fcn(x,y,xtest,ytest);
      Vertex_order(jj) := TRUNC((ij+1)/2); 

    End Loop;

    -- Now sort from near to far
    
    Shellsort2(gc_distances,Vertex_order,1,Vertex_order.count,forwards);
    
--     if vertex_order.count < 100 then
--    for ii in 1..Vertex_order.count loop
--        dbms_output.put_line('ii ' || ii || ' vertex order ' || vertex_order(ii) || ' d ' || round(gc_distances(ii),8));
--      end loop;
--    end if;
    Return Vertex_order;
        
END Find_Distances;
--
FUNCTION LINE_PARALLEL( X1 IN OUT NOCOPY NUMBER,
                              Y1 IN OUT NOCOPY NUMBER,
                              X2 IN OUT NOCOPY NUMBER,
                              Y2 IN OUT NOCOPY NUMBER,
                              X3 IN OUT NOCOPY NUMBER,
                              Y3 IN OUT NOCOPY NUMBER,
                              X4 IN OUT NOCOPY NUMBER,
                              Y4 IN OUT NOCOPY NUMBER,
                              Xp IN OUT NOCOPY NUMBER,
                              Yp IN OUT NOCOPY NUMBER,
                              Xq IN OUT NOCOPY NUMBER,
                              Yq IN OUT NOCOPY NUMBER,
                              distance_p IN OUT NOCOPY NUMBER,
                              distance_q IN OUT NOCOPY NUMBER,
                              SRID NUMBER,
                              cos_angle_tolerance NUMBER default 0.8660254,
                              dist_tolerance NUMBER default 500.0,
                              perp_angle_tolerance NUMBER default 0.707107,print_it BOOLEAN default FALSE)
            RETURN NUMBER Deterministic IS
/*
********************************************************************************
--Program Name: line_parallel
--Author: Sidey Timmins
--Creation Date: 2/13/2009 from line intersect to find demi-parallel lines
--Updated:   08/07/2013 To handle the vase case (\ /)where the projectors are both
--          slightly off the ends
--
--               \         /
--                +       /
--                 \     +   One projector drawn here past the end
--                  . *

--           12/02/2008 To better handle parallel lines and report differently
--                    segments touching at 1 point or overlapping at 2 points.
--Usage:
  -- Call this program from inside another PL/SQL program.  This procedure
  -- has 14 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT

  --      X1,Y1      - start of  (x,y) of line 1 (point A)
  --      X2,Y2      - end point (x,y) of line 1 (point B)
  --      X3,Y3      - start of  (x,y) of line 2
  --      X4,Y4      - end point (x,y) of line 2
  --      SRID       - spatial reference ID
  --      cos_angle_tolerance - the cosine of the maximum angle between the lines:
  --                            for example for 30 degrees, cos = .8660254
  --                   Lines with a larger angle are not considered. When the
  --                   caller calls line_parallel thousands of times, it is
  --                   obviously wasteful to keep figuring out the cos(angle_tolerance).
  --     
  --      OUTPUT
  --      xp,yp      -- the near projection point A onto line 2 or from line 2 onto line1 
  --      xq,yq      -- the far projection point B onto line 2 or from line2 onto line1.
  --                 -- Note that xp,yp is closer to x1,y than xq,yq which is nearer
  --                    to x2,y2.
  --      distance_p -- the distance between parallel (or near_parallel) lines from 
  --                    point P
  --      distance_q -- the distance between the linea from point Q
  --      
  --
  --                   C
  --            A+-----.------+B
  --          zero   r_out    1.0
  --
-- Purpose: Determine whether a point is on a line
  -- Return:              1:  point is on line
--                        0:  not on line
--              For these 2 return values, the distances are xi and yi respectively
--                        and these distances are both less than the distance tolerance.
--                       1324.n: x1,y1 and x2,y2 are parallel or nearly parallel
--                           with x3,y3 and x4,y4
--                            n = (1,2,3,4) is the projector. Example.2 means
--                                2 projects onto 3->4
--                       1423.n: x1,y1 and x2,y2 are parallel or nearly parallel
--                           with x4,y4 and x3,y3
--                            n = (1,2,3,4) is the projector. Example.4 means
--                                4 projects onto 1->2
--                       1234: Lines are perpendicular. Projection point
--                       13: x1,y1 and x3,y3 are the same point
--                       14: x1,y1 and x4,y4 are the same point
--                       23: x2,y2 and x3,y3 are the same point
--                       24: x2,y2 and x4,y4 are the same point

-- Dependencies:
-- Updates: This data for derived and parent caused a problem
--  x1   NUMBER :=  70971.6838873964;         xes(1) :=71241.4748303597;
--  y1   NUMBER :=  184331.932613249;         yes(1) :=184666.358957741;   <--
--  x2   NUMBER :=  71241.4748303597;         xes(2) :=70439.6838592369;
--  y2   NUMBER :=  184666.358957743;  <--    yes(2) :=183672.478212245;

--Case 1:
-- so we upped the tolerance from 1.E-5 to 2.E-5 which enabled x1,x2 to
-- be changed by as much as 1.E-8 (20 times bigger than the difference
-- shown above)
--Case 2:
-- After addding 4 million meters to x and 2 million meters to y the new
-- tolerances shown below enabled x1,y1 (and/or x2,y2) to be changed by 1.E-7

-- Reference: A Programmer's Geometry: Adrian Bowyer and John Woodwark
-- http://web.cecs.pdx.edu/~york/cs410510f08/Bowyer.pdf
-- http://emweb.unl.edu/math/mathweb/vectors/vectors.html
-- http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html
--
-- Called by: 
-- Dependencies: none
********************************************************************************
*/
  
   deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;
   rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033240;
   geometry           MDSYS.SDO_GEOMETRY;
   geometry2          MDSYS.SDO_GEOMETRY;
   s    NUMBER;
   t    NUMBER;
   Det  NUMBER;
-- These tolerances handles much worse errors see above: case 2
--                              These handled the original problem: case 1
   tolerance NUMBER := 1.E-4;   -- 2.E-5
   tolerance2 NUMBER := 1.E-6;  -- 1.E-8
   p0   NUMBER := -tolerance2;
   p1   NUMBER := 1.0+tolerance2;

   
   length_2 NUMBER;
 
   t1         NUMBER;  -- line parameter for projection point (vertex) onto other segment
   t2         NUMBER; 
   t3         NUMBER; 
   t4         NUMBER; 
   X21        NUMBER := X2-X1;
   Y21        NUMBER := Y2-Y1;
   X43        NUMBER := X4-X3;
   Y43        NUMBER := Y4-Y3;
   X31        NUMBER := X3-X1;
   Y31        NUMBER := Y3-Y1;
   X32        NUMBER := X3-X2;
   Y32        NUMBER := Y3-Y2;
   X41        NUMBER := X4-X1;
   Y41        NUMBER := Y4-Y1;
   xii        NUMBER;
   yii        NUMBER;
   x          NUMBER;
   y          NUMBER;

   denom    NUMBER;
   denom2   NUMBER;
 
   theta    NUMBER;
   costheta NUMBER;
   sintheta NUMBER;

   BIG      NUMBER := 1.E10;
   len1     NUMBER;
   len2     NUMBER;
   odist    NUMBER;
   dotp     NUMBER;
   dxp      NUMBER;
   dyp      NUMBER;
   off_end  NUMBER := .25*dist_tolerance;

   lenp     NUMBER;
   dot_perp NUMBER;
   approx_distance NUMBER := 0.;
   approx_length   NUMBER := 0.;
   siny            NUMBER;
   cosy            NUMBER := 1.0;
   distance1       NUMBER := BIG;
   distance2       NUMBER := BIG;

   dist_factor     NUMBER := 111319.490793274;
   relationship    VARCHAR2(200);
   projector       NUMBER :=0.0;
   overlap         NUMBER :=0.0;
   ok              BOOLEAN := FALSE;
   result         NUMBER := 0.;

   Function overlapz(px1 number,py1 number,px2 number,py2 number,px3 number,py3 number,px4 number,py4 number) return boolean as
   
   u       number;
   lap     boolean;
   Begin
           u := ((px3-px2)*(px2-px1) + (py3-py2)*(py2-py1))/
               -((px4-px3)*(px2-px1) + (py4-py3)*(py2-py1));
               
      lap := u >=0. and u <= 1.;
      return lap;
   End;

BEGIN

-- Check for zero length lines lines (points)
   IF (x43 = 0.0 and y43 = 0.) or ( x21 = 0. and y21 = 0.) then
      dbms_output.put_line('returning zero');
      RETURN 0.;
   END IF;

   xp := NULL; yp := NULL;
   xq := NULL; yq := NULL;
   distance_p := Big; distance_q := Big;
   
-- Check parametric equations for two lines: s is on line 1
--                                           t is on line 2


   det := X43 * Y21 - X21 *Y43; -- (X4 - X3) * (Y2 - Y1)  - (X2 - X1) * (Y4 - Y3) ;

--   dbms_output.put_line('DET is :' || ROUND(det,7) );

-- Check if the lines effectively touch at the given tolerance by
-- calculating the closest distance between them

   dotp := X21 * X43 + Y21 * Y43;
   len1 := sqrt(X21 * X21 + Y21 * Y21);
   denom2 := X43 * X43 + Y43 * Y43;
   len2 := sqrt(denom2);

-- We want the smallest angle and we don't yet know the line orientation
-- so take the absolute

   costheta := abs(dotp)/(len1 * len2);

--dbms_output.put_line('costheta is :' ||round(costheta,7)); -- || ' angle2 ' || round(angle2,7) || 'angle ' || round((acos(costheta)* rad2deg),4) || ' cosat ' || ROUND(cos_angle_tolerance,5));
--   dbms_output.put_line('x1 ' || round(x1,7) || ' y1 ' || round(y1,7) );
--   dbms_output.put_line('x2 ' || round(x2,7) || ' y2 ' || round(y2,7) );
--   dbms_output.put_line('x3 ' || round(x3,7) || ' y3 ' || round(y3,7) );
--   dbms_output.put_line('x4 ' || round(x4,7) || ' y4 ' || round(y4,7) );

--end if;
-- Check if the lines intersect

   If det <> 0.0 THEN
      s := (X43*Y31 - Y43*X31)/det; 
      t := (X21*Y31 - Y21*X31)/det; 
--        dbms_output.put_line('SS is :' || s || ' T ' || t);
         If s >= 0 and s <= 1.0 and t >= 0.0 and t <= 1.0 then
            xp := X1 + s * X21;
            yp := Y1 + s * Y21;
            
            if x1 = x3 and y1 = y3 then result := 13.; end if;
            if x1 = x4 and y1 = y4 then result := 14.; end if;
            if x2 = x3 and y2 = y3 then result := 23.; end if;
            if x2 = x4 and y2 = y4 then result := 24.; end if;
           RETURN result;
         End If;
   End If;
   
-- Are the two lines demi-parallel?

-- Check the angle the two lines make with each other.
-- Cosine(zero)=1.0 so any angle with a cosine > tolerance is a smaller angle.

   IF costheta >= cos_angle_tolerance  THEN

      denom := X21 * X21 + Y21 * Y21;  -- square of length: line segment 1 
--------------------------------------------------------------------------------
--    Find out if one or more vertices project.
--      dbms_output.put_line('projecting x3 onto x1,y1 to x2,y2');
      t3 := (X21*X31 + Y21*Y31)/denom;
--      dbms_output.put_line('projecting x1 onto x3,y3 to x4,y4');
      t1 := -(X31*X43 + Y31*Y43)/denom2;
--      dbms_output.put_line('projecting x4 onto x1,y1 to x2,y2');
      t4 := (X21*X41 + Y21*Y41)/denom;
--      dbms_output.put_line('projecting x2 onto x3,y3 to x4,y4');
      t2 := -(X32*X43 + Y32*Y43)/denom2;
      
      
      if t1 >= 0. and t1 <= 1.0 then
         overlap := overlap+1.0;
      end if;
      if t2 >= 0. and t2 <= 1.0 then
         overlap := overlap+1.0;
      end if;
      if t3 >= 0. and t3 <= 1.0 then
         overlap := overlap+1.0;
      end if;   
      if t4 >= 0. and t4 <= 1.0 then
         overlap := overlap+1.0;
      end if;

 
--------------------------------------------------------------------------------      
--    dbms_output.put_line('projecting x3 onto x1,y1 to x2,y2');
      xii := X1 + t3 * X21;
      yii := Y1 + t3 * Y21;
      if t3 >= 0. and t3 <= 1.0 then

--       dbms_output.put_line('xii ' || xii || ' yii ' || yii || ' t3 ' || t3 || ' dotp ' || dotp);
 -- Determining overlap is easy for the wider end - one vertex always projects!
 -- But at the narrow end, there may or may not be a projector. Both may
 -- not project when the ends are like looking at a silouette of a flower pot, the
 -- axis of the pot and the perpendicular to that axis hits the ends of
 -- one or both of the lines (like the way the pot sides end at the bottom.)
 
        distance1 := Distance_fcn(x3,y3,xii,yii,SRID);
         if dotp > 0. and (xii <> x2 or yii <> y2) then  
           xp := xii; yp := yii;
           distance_p := distance1;
           projector := 30. ;
         elsif dotp <0 and (xii <> x1 or yii <> y1) then
           xq := xii; yq := yii;
           distance_q := Distance_fcn(x3,y3,xq,yq,SRID);
           projector := 3. ;
         end if;
      elsif overlap = 1.0 and perp_angle_tolerance > 0.0 then
         if dotp  > 0. and t4 >= 0.0 and t4 <= 1.0 then
--         if Distance_fcn(x1,y1,xii,yii,SRID) < off_end then
           if overlapz(x4,y4,x3,y3,x1,y1,x2,y2) then
            xp := x1; yp := y1;
            projector := 30.;
            distance_p := Distance_fcn(x3,y3,xp,yp,SRID);
          end if;
 
         elsif dotp  < 0. and t4 >= 0.0 and t4 <= 1.0 then
--         if Distance_fcn(x2,y2,xii,yii,SRID) < off_end then
            if overlapz(x4,y4,x3,y3,x2,y2,x1,y1) then
            xq := x2; yq := y2;
            projector := 3.;
            distance_q := Distance_fcn(x3,y3,xq,yq,SRID);
         end if;
        end if; 
      end if;
--------------------------------------------------------------------------------
--    dbms_output.put_line('projecting x1 onto x3,y3 to x4,y4: P'|| projector);
      If t1 >= 0. and t1 <= 1.0 Then
      
        xp := X3 + t1 * X43;
        yp := Y3 + t1 * Y43;
--      dbms_output.put_line('t ' || t1 || ' xii ' || xii || ' yii ' || yii);

        distance1 := Distance_fcn(x1,y1,xp,yp,SRID);
        if dotp > 0. and (xp <> x4 or yp <> y4) then
        if distance1 < distance_p then        
          projector := 10.;
          distance_p := distance1;
         end if;
        elsif dotp < 0. and (xp <> x3 or yp <> y3) then
          projector := 10.+projector;
          if distance1 < distance_p then
          distance_p := distance1;
          end if;
        end if;
          
      Elsif overlap = 1.0 and perp_angle_tolerance > 0.0 then

         if dotp > 0. and t2 >= 0. and t2 <= 1.0 then  -- same direction
--            if Distance_fcn(x3,y3,xp,yp,SRID) < off_end then
            if overlapz(x2,y2,x1,y1,x4,y4,x3,y3) then
            xp := x3;
            yp := y3;
            projector := 10.;
            distance1 := Distance_fcn(x1,y1,xp,yp,SRID);
            if distance1 < distance_p then
               distance_p := distance1;
            end if;
            end if;
         elsif dotp < 0. and t2 >= 0. and t2 <= 1.0 then  -- opposite
--         if Distance_fcn(x4,y4,xp,yp,SRID) < off_end then
         if overlapz(x2,y2,x1,y1,x3,y3,x4,y4) then
            xp := x4;
            yp := y4;
            projector := 10.+ projector;
            distance1 := Distance_fcn(x1,y1,xp,yp,SRID);
            if distance1 < distance_p then
             distance_p := distance1;
            end if;
            
          end if;
         end if;
                      
      End If;


--------------------------------------------------------------------------------
--          dbms_output.put_line('projecting x4 onto x1,y1 to x2,y2:'|| projector);
      xii := X1 + t4 * X21;
      yii := Y1 + t4 * Y21;
      if t4 >= 0. and t4 <= 1.0 then        
--        dbms_output.put_line('Xii ' || xii || ' yii ' || yii || ' t4 ' || t4);
        distance1 := Distance_fcn(x4,y4,xii,yii,SRID);
--        dbms_output.put_line('dotp ' || dotp);
        if dotp > 0. and (xii <> x2 or yii <> y2) then
           xq := xii; yq := yii;
           distance1 := Distance_fcn(x4,y4,xq,yq,SRID);
           if distance1 < distance_q then
              distance_q := distance1;
           end if;
           projector := projector + 4. ;
        elsif dotp <0 and (xii <> x1 or yii <> y1) then
           xp := xii; yp := yii;
           distance1 := Distance_fcn(x4,y4,xp,yp,SRID);
           if distance1 < distance_p then
             distance_p := distance1;
           end if;
           projector := projector + 40. - TRUNC(projector*0.1)*10.;
        end if;
 
      elsif overlap = 1.0 and perp_angle_tolerance > 0.0 then

         if dotp > 0. and t3 >= 0. and t3 <= 1. then
--         if Distance_fcn(x2,y2,xii,yii,SRID) < off_end then
         if overlapz(x3,y3,x4,y4,x1,y1,x2,y2) then
            xq := x2; yq := y2;
            projector := projector + 4.;
            distance1 := Distance_fcn(x4,y4,xq,yq,SRID);
            if distance1 < distance_q then
              distance_q := distance1;
            end if;
          end if;
         elsif dotp < 0. and t3 >= 0. and t3 <= 1. then
--         if Distance_fcn(x1,y1,xii,yii,SRID) < off_end then
         if overlapz(x3,y3,x4,y4,x2,y2,x1,y1) then
            xp := x1; yp := y1;
            distance1 := Distance_fcn(x4,y4,xp,yp,SRID);
            if distance1 < distance_p then
               distance_p := distance1;
            end if;
            projector := projector + 40. - TRUNC(projector*0.1)*10.;
         end if;
         end if;
      end if;
--------------------------------------------------------------------------------
--    dbms_output.put_line('projecting x2 onto x3,y3 to x4,y4');
     
      If t2 >= 0. and t2 <= 1.0 then
        xq := X3 + t2 * X43;
        yq := Y3 + t2 * Y43;

        distance1 := Distance_fcn(x2,y2,xq,yq,SRID);
        if distance1 < distance_q then
          distance_q := distance1;
        end if;
        projector := 2. + TRUNC(projector*0.1)*10.;
      Elsif overlap = 1.0 and perp_angle_tolerance > 0.0 then  
         if dotp > 0. and t1 >= 0. and t1 <= 1.0 then  -- same direction
--           if Distance_fcn(x4,y4,xq,yq,SRID) < off_end then
           if overlapz(x1,y1,x2,y2,x3,y3,x4,y4) then
            xq := x4;
            yq := y4;
            distance1 := Distance_fcn(x2,y2,xq,yq,SRID);
            if distance1 < distance_q then
              distance_q := distance1;
            end if;
            projector := projector + 2.;
          end if;
         elsif dotp < 0. and t1 >= 0. and t1 <= 1.0 then  -- opposite
--         if Distance_fcn(x3,y3,xq,yq,SRID) < off_end then
         if overlapz(x1,y1,x2,y2,x4,y4,x3,y3) then
            xq := x3;
            yq := y3;
            distance1 := Distance_fcn(x2,y2,xq,yq,SRID);
            if distance1 < distance_q then
              distance_q := distance1;
            end if;
            projector := 2. + TRUNC(projector*0.1)*10.;
         end if;
         end if;
          
      end if;
--dbms_output.put_line('Projector ' || projector);

      
-- We want to build the following projector codes going in the direction of line 1 -> 2: 
-- Lines in same direction:     12,34,14,32
--       in opposite direction: 12,43,42,13

-- Use component parts of 10,30, 2 and 4 for same direction
-- and 10,40,2,3 in opposite directions


--dbms_output.put_line('Projector ' || projector);


 
--------------------------------------------------------------------------------

      if dotp = 0. then
        result := 1234.;
        if projector = 12 and distance_p < distance_q then
        projector := 1.;
        elsif projector = 12 and distance_p > distance_q then
        projector := 2.;
        elsif projector = 43 and distance_p < distance_q then
        projector := 4.;
        elsif projector = 43 and distance_p > distance_q then
        projector := 3.;
        end if;
      elsif dotp > 0. then
        result := 1324;  -- parallel
      else
        result := 1423.;  -- antiparallel
      end if;
      
-- returned projectors may be 34, 12,14, 32 for parallel
-- and 12, 43,42 and 13 for antiparallel
-- Correct misdirected projectors (they are to go from 1 -> 2

--      if dotp < 0. and projector = 34 then
--        projector := 43;      
--      end if;
--      if dotp < 0. and projector = 31 then
--        projector := 13;      
--      end if;
--      if dotp > 0. and projector = 41 then
--        projector := 14;      
--      end if;

      if result >= 1234. then
        result := result + projector*0.01;
      end if;

        distance1 := distance_p;
        if distance_q < distance1 then
           distance1 := distance_q;
        end if;
                if print_it then
        dbms_output.put_line('distance1 '|| distance1 || ' ' ||result || ' ' || dist_tolerance);
        dbms_output.put_line('costheta is :' ||round(costheta,7));
        end if;
        if distance1 > dist_tolerance then
          result := 0.;
        end if;

--dbms_output.put_line('xp ' || xp || ' yp ' || yp);
--dbms_output.put_line('xq ' || xq || ' yq ' || yq);
      if (result >= 1324. and distance1 < dist_tolerance ) or result > 0. then
--      if (result >= 1324. and distance < dist_tolerance and (approx_length*sintheta + distance) < dist_tolerance) or result > 0. then
         RETURN result;
--      else
--         RETURN 0.;
      end If;

   END IF;

-- Lines are parallel (or coincident)
   RETURN -approx_distance;


END LINE_PARALLEL;
--
FUNCTION NEW_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER AS


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
  --            degrees: TRUE result will be in degrees
  --
--Purpose:   -- Calculates the arctan function -  atan2(y,x)
--              as fast as possible (12 times faster than the built in
--              function atan2 which takes 3 seconds for 50000 values) with
--              an accuracy of < 2E-27. (Exadata hardware is about 3.3 times faster
--              than old hardware.)
--              The domain is unlimited. (Note this function gives no error
--              and returns zero when both x and y are zero.)
--
-- Method:      First the range is reduced and then the arctangent is computed
--              using a formula from the reference.
--              The result is then adjusted for the quadrant.

-- Accuracy:  The maximum error in the range - -infinity to infinity is < 2E-27
--
-- Reference: Ulrich H Kurzweg
--   http://www.mae.ufl.edu/~uhk/MORE-EVALUATION-ARCTAN.pdf
--Dependencies: None but see try_faster_atan2 for rigorous testing.
--******************************************************************************


   pi        CONSTANT NUMBER  := 3.1415926535897932384626433832795028842;
   piByTwo   CONSTANT NUMBER  := 1.5707963267948966192313216916397514421;

   piBy6     CONSTANT NUMBER  := 0.5235987755982988730771072305465838140;
   tanpiby6  CONSTANT NUMBER  := 0.5773502691896257645091487805019574556545;
   rad2deg   CONSTANT NUMBER  := 57.29577951308232087679815481410517033240;

   x           NUMBER;
   xx          NUMBER;
   result     NUMBER;
   complement  BOOLEAN := FALSE;
   region      NUMBER := 0.;
   region2     NUMBER := 0.;
   region3     NUMBER := 0.;
   region4     NUMBER := 0.0;
   u           NUMBER;
   absx        NUMBER := ABS(Xin);
   absy        NUMBER := ABS(Yin);

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
  ELSIF absy > absx THEN
     x   := absx/absy;
  ELSE
     x   := absy/absx;
  END IF;

  -- reduce arg to under tan(pi/12)

  u := x;
  if x > 1. then
        x := (x-tanpiby6)/(1.0 +tanpiby6*x);
       region := piby6;
       if x > 1. then
        x := (x-tanpiby6)/(1.0 +tanpiby6*x);
       region := region + piby6;
       end if;
   end if;

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
--      dbms_output.put_line('< 0.5 ' || x);
    end if;
   end if;


    if yin <> 0.0 and ((absy >= absx and u <= 1.) or (absy < absx and u > 1.)) then
     complement := TRUE;
     end if;


     if x > 1.E15 then
      result := 1./x + region;
    else

      u := x*x;
--      result := x*(55. + 105.*u)/(9.+u*(90. + 105*u)) + region;
--      result := x*(231. + u*(1190.+1155.*u))/ (25.+ u*(525.+ u*(1575. + 1155.*u))) + region;
        result := x*(3031.8 + u*(29491.+u*(69069.+45045*u)))/ (245.+ u*(8820.+ u*(48510. + u*(84084.+u*45045.))))
                 + region;
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
FUNCTION divergence(x11 NUMBER,y11 NUMBER,x22 NUMBER,y22 NUMBER, x33 NUMBER, y33 NUMBER, x44 NUMBER, y44 NUMBER,cpx NUMBER, cpy NUMBER,cutoff NUMBER,
                     xp in out nocopy number, yp in out nocopy number, xq in out nocopy number,yq in out nocopy number,perp_angle_tolerance NUMBER default 0.707107) RETURN MDSYS.SDO_LIST_TYPE as

-- A function to measure the angle and distance between two segments
-- A segment begins at its beginning and any measurement at (x2,y2) or (x4,y4)
-- belongs to the next segment!

--           1     -------    2      
--        (x1,y1)  |         (x2,y2)
--                 |
--        3     --------------    4      
--      (x3,y3)                (x4,y4)

  rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033240;
   x1            NUMBER :=x11;
   y1            NUMBER :=y11;
   x2            NUMBER :=x22;
   y2            NUMBER :=y22;
   x3            NUMBER :=x33;
   y3            NUMBER :=y33;
   x4            NUMBER :=x44;
   y4            NUMBER :=y44;

  angle1            NUMBER;
  angle2            NUMBER;
  angle_found       NUMBER;
  dx                NUMBER;
  dy                NUMBER;
  dist1             NUMBER;
  dist2             NUMBER;
  xp1               NUMBER;
  yp1               NUMBER;
  xp2               NUMBER;
  yp2               NUMBER;
  Big               NUMBER := 1.E10;
  score             NUMBER :=0.0;
  dist_to           NUMBER := -Big;
  dist_to2          NUMBER := -Big;
  
  start_point       BOOLEAN := TRUE;
  
BEGIN

-- Measure the angle and distance (divergence) between two segments starting at
-- side1_vertex and side2 vertex.


    dx := x2-x1;
    dy := y2-y1;

    Angle1 := Faster_atan2(dy,dx,TRUE);
--dbms_output.put_line('x1 ' || ROUND(x1,7) ||',' || round(y1,7));
--dbms_output.put_line('x2 ' || ROUND(x2,7) ||',' || round(y2,7));

-- We know we are being passed segments going in opposite directions. So correct
-- for this antiparallelism

    dx := x3-x4;  -- use 3 minus 4 instead of 4 minus 3
    dy := y3-y4;
    Angle2 := Faster_atan2(dy,dx,TRUE);
--dbms_output.put_line(' angle1 ' || round(angle1,4) || ' ' ||round(angle2,4));
    Angle_found := ABS(angle1-angle2);

    if angle_found > 180. then
      angle_found := 360. -angle_found;
    end if; 

--dbms_output.put_line('AF ' || round(angle_found,3) ||'  angle1 ' || round(angle1,4) || ' ' ||round(angle2,4));
--dbms_output.put_line('X1 '||round(x1,7) ||','||round(y1,7) || ',' || round(x2,7) ||',' || round(y2,7)); 

--dbms_output.put_line('X3 '||round(x3,7) ||','||round(y3,7) || ',' || round(x4,7) ||',' || round(y4,7));
-- Measure perpendicular distance between 1st segment's start point and the 2nd line

    dist1 := Perpendicular(x1,y1,x3,y3,x4,y4,xp,yp,TRUE,perp_angle_tolerance);
   
--   dbms_output.put_line('dist1 ' || round(dist1,3) ||' trying ' ||round(x1,7) ||','||round(y1,7) || ' onto ' || round(x3,7) ||',' || round(x4,7)); 
    -- Also measure a distance from the current position to the perpendicular point
    -- dist1 <> Big if (x1,y1) projects onto the 2nd line
    
    if dist1 <> Big then
      dist_to := Distance_Fcn(cpx,cpy,xp,yp);
--      dbms_output.put_line('Dist1 ' || round(dist1,3) ||' Dist_to ' || round(dist_to,3) ||' trying ' ||round(cpx,7) ||','||round(cpy,7) || ' to ' || round(xp,7) ||',' || round(yp,7) ); 
    end if;
    
-- Measure perpendicular distance between 1st segment's end point and the 2nd line 

    dist2 := Perpendicular(x2,y2,x3,y3,x4,y4,xq,yq,TRUE,perp_angle_tolerance);
--    dbms_output.put_line('Dist2 ' || round(dist2,3) ||' trying ' ||round(x2,7) ||','||round(y2,7) || ' onto ' || round(x3,7) ||',' || round(x4,7)); 
    -- dist2 <> Big if (x2,y2) projects onto the 2nd line
    
    if dist2 <> Big then
      dist_to2 := Distance_Fcn(cpx,cpy,xq,yq);
--      if dist1 = Big then
--         start_point := FALSE;
--dbms_output.put_line('Dist2 ' || round(dist2,3) ||' Dist_to2 ' || round(dist_to2,3) ||' trying ' ||round(cpx,7) ||','||round(cpy,7) || ' to ' || round(xq,7) ||',' || round(yq,7) ); 
--      end if;
    end if;
--        dbms_output.put_line('Dist2 ' || round(dist2,3) ||' Dist_to2 ' || round(dist_to2,3) ||' trying ' ||round(x2,7) ||','||round(y2,7) || ' onto ' || round(x3,7) ||',' || round(y3,7) || ' ' || round(x4,7) ||',' || round(y4,7)); 
    
-- Setup a score := angle from parallel + (shortest distance -cutoff)/cutoff) * 10. 
-- A higher score is worse. Zero is best.

-- Dont add onto the score if the distance is equal to Big

    if dist_to <> -Big and dist_to < dist_to2 then
--    dbms_output.put_line('>>> calculating a score ' || round(dist1,3) || ' cutoff ' || cutoff);
      score := 10.*ABS(dist1-cutoff)/cutoff;
    elsif dist_to2 <> -Big and dist_to2 < dist_to then
--     dbms_output.put_line('>>> Calculating a score ' || round(dist2,3) || ' cutoff ' || cutoff);
      score := 10.*ABS(dist2-cutoff)/cutoff;
    elsif dist_to <> -Big then
       score := 10.*ABS(dist1-cutoff)/cutoff; 
    end if;
    
--    if start_point then
--      if dist1 <> Big then
       return MDSYS.SDO_LIST_TYPE(score,angle_found,dist1,dist2,dist_to,dist_to2,x1,y1,xp,yp); -- xp,yp);
--    else
--      return MDSYS.SDO_LIST_TYPE(score,angle_found,dist2,dist1,dist_to2,dist_to,x2,y2,xq,yq); --,xq,yq);
--    end if;
    
END divergence;
--
Function trip(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,start_of_ring1 pls_integer, end_of_ring1 pls_integer, start_of_ring2 pls_integer, end_of_ring2 pls_integer,
               side_1 in out nocopy pls_integer,inc1 pls_integer,side_2 in out nocopy pls_integer,inc2 pls_integer,cpx number,cpy number,cutoff number,
               case_is in out nocopy number, score in out nocopy number, angle_apart in out nocopy number,
               width in out nocopy number, dist_found in out nocopy number,
               x1  in out nocopy number, y1  in out nocopy number,
               x2  in out nocopy number, y2  in out nocopy number,
               x3  in out nocopy number, y3  in out nocopy number,
               x4  in out nocopy number, y4  in out nocopy number,
               xpa in out nocopy number, ypa in out nocopy number,
               xqa in out nocopy number, yqa in out nocopy number,
               xpb in out nocopy number, ypb in out nocopy number,
               xqb in out nocopy number, yqb in out nocopy number
               ) return  mdsys.sdo_list_type as
   
   -- Take a single segment trip and measure the score
   -- (the perpendicular distance plus the angle between the lines.)
   -- We are really interested in the furthest the pipe appears to extend.
   
      diverge_a   mdsys.sdo_list_type;
      diverge_b   mdsys.sdo_list_type;
      Big         NUMBER := 1.E10;

 
      angle1      number;
      score2      number;
      closest     number;
      dist_plong  number;
      dist_qlong  number;
      width_p     number;   -- p is close
      width_q     number;   -- q is far
      dist1       number;
      dist2       number;
      dist3       number;
      dist4       number;
      xb1         number;
      yb1         number;
      xf2         number;
      yf2         number;
      xb3         number;
      yb3         number;
      xf4         number;
      yf4         number;


      cpx_nu      number;
      cpy_nu      number;
      next_one1     PLS_INTEGER;
      next_one2     PLS_INTEGER; 
      
   Begin

       Setup_side(side_1,inc1,next_one1,start_of_ring1,end_of_ring1);
--dbms_output.put_line('side1 ' || side_1 || ' side2 ' || side_2 || ' xy ' || xys.count);
--dbms_output.put_line('next1 ' || next_one1 );
             -- Set up Side 1's vertices       
      x1 := Xys(side_1*2-1);
      y1 := Xys(side_1*2);
      x2 := Xys(next_one1*2-1);
      y2 := Xys(next_one1*2);
      if side_1 > 1 then
        xb1 := Xys(side_1*2-3); yb1 := Xys(side_1*2-2);
      else
        xb1 := NULL; yb1 := NULL;
      end if;
      if next_one1*2 < Xys.count then
        xf2 := Xys(next_one1*2+1); yf2 := Xys(next_one1*2+2);
      else
        xf2 := NULL; yf2 := NULL;
      end if;
       Setup_Side(side_2,inc2,next_one2,start_of_ring2,end_of_ring2);
       
      IF side_1 = side_2 then
         case_is := 0;     -- useless to make measurements between same segment
      ELSE
-- dbms_output.put_line(' next2 ' || next_one2);      
             -- Set up Side 2's vertices       
      x3 := Xys(side_2*2-1);
      y3 := Xys(side_2*2);
      x4 := Xys(next_one2*2-1);
      y4 := Xys(next_one2*2);
      if side_2 > 1 then
       xb3 := Xys(side_2*2-3); yb3 := Xys(side_2*2-2);
      else
        xb3 := NULL; yb3 := NULL;
      end if;
      if next_one2*2 < Xys.count then
        xf4 := Xys(next_one2*2+1); yf4 := Xys(next_one2*2+2);
      else
        xf4 := NULL; yf4 := NULL;
      end if;
--
-- There are 4 cases:        We list the cases digits in forwards order!
-- We are interested in how far we have established the pipe extends.
-- The pipe extent is marked thus:  (n).
--             a
-- 1)       1-->--2           Case 1) = 12.   a1 and a2 project
--          |     |                           Extent: (2) on a
--      -------<--------             
--             b
--
--                  a         Case 2) = 43.   b3 and b4 project
-- 2)      --------------------              neither a1 or a2 project but both b's do
--              |       |                    Extent: projection of (3) onto a
--              4---<-- 3
--                  b
--
--                 a          Case 3) = 13
-- 3)         1------->-------               a1 projects and b3 project
--            |      |                       Extent: projection of (3) onto a
--      --------<----3
--            b
--
--            a
-- 4)     -->------2          Case 4) =42   a2 projects and b4 projects
--          |      |                        Extent:  (2) on a
--          4----------<------
--                  b
--                 
-- 5)     \ a                  Case 5) =2   either a2. Extent is (2) on a
--         \___________
--                             alternate                  or
-- 6)      -----<-----
--                    \ b      Case 6) =4    b4 projects. Extent (4) onto a
--                     \

--   possibilities are therefore
--     a1  a2    X   X
--     a1  X     X   b2
--     X   X     b1  b2
--     X   a2    b1  X
--            
       -- First we attempt to project line 1 -> 2 onto line 3 - > 4
       -- Since we project onto b, we have xpb,ypb, etc
       diverge_a := divergence(x1,y1,x2,y2,x3,y3,x4,y4,cpx,cpy,cutoff,xpb,ypb,xqb,yqb);
       dist1 := diverge_a(3);
       dist2 := diverge_a(4);
       -- now check for local intersections with the before and after segments
       
--       if dist1 <> Big and (seg_intersect(x1,y1,xpb,ypb,xb3,yb3,x3,y3) or
 --                            seg_intersect(x1,y1,xpb,ypb,x4,y4,xf4,yf4)) then
--          dist1 := Big;  -- mark it as no good
--       end if;
       
--       if dist2 <> Big and (seg_intersect(x2,y2,xqb,yqb,xb3,yb3,x3,y3) or
 --                            seg_intersect(x2,y2,xqb,yqb,x4,y4,xf4,yf4)) then
--          dist2 := Big;  -- mark it as no good
--       end if;
       
       -- Then the reverse. We have q and p reversed in the argument list so
       -- in the direction 1 ->  2 (opposite of 3 -> 4) p comes before q
       
       diverge_b := divergence(x3,y3,x4,y4,x1,y1,x2,y2,cpx,cpy,cutoff,xqa,yqa,xpa,ypa);
       dist3 := diverge_b(3);
       dist4 := diverge_b(4);
       
       -- now check for local intersections with the before and after segments
       
--       if dist3 <> Big and (seg_intersect(x3,y3,xqa,yqa,xb1,yb1,x1,y1) or
--                             seg_intersect(x3,y3,xqa,yqa,x2,y2,xf2,yf2)) then
--          dist3 := Big;  -- mark it as no good
--       end if;
       
--       if dist4 <> Big and (seg_intersect(x4,y4,xpa,ypa,xb1,yb1,x2,y2) or
--                             seg_intersect(x4,y4,xpa,ypa,x2,y2,xf2,yf2)) then
--          dist4 := Big;  -- mark it as no good
--       end if;
--       dbms_output.put_line('d1 ' || dist1 || ' d2 ' || dist2 || ' d3 ' || dist3 || ' d4 ' || dist4);
       case_is :=0;
       if dist1 <> Big and dist2 <> Big then
         case_is := 12;
         width_p := dist1;
         width_q := dist2;
         dist_plong := diverge_a(5);
         dist_qlong := diverge_a(6);
--         box := mdsys.sdo_list_type(x1,y1,x2,y2,xqb,yqb,xpb,ypb);
       elsif dist4 <> Big and dist3 <> Big then
         case_is := 43;
         width_p := dist4;
         width_q := dist3;
         dist_plong := diverge_b(6);
         dist_qlong := diverge_b(5);
--         box := mdsys.sdo_list_type(xpa,ypa,xqa,yqa,x3,y3,x4,y4);
       elsif dist1 <> Big and dist3 <> Big then
         case_is := 13;
         width_p := dist1;
         width_q := dist3;
         dist_plong := diverge_a(5);
         dist_qlong := diverge_b(5);
     --    dbms_output.put_line('D ' || dist_plong || ' Dq ' || dist_qlong);
--         box := mdsys.sdo_list_type(x1,y1,xqa,yqa,x3,y3,xpb,ypb);
       elsif dist4 <> Big and dist2 <> Big then
         case_is := 42;
         width_p := dist4;
         width_q := dist2;
         dist_plong := diverge_b(6);
         dist_qlong := diverge_a(6);
--         box := mdsys.sdo_list_type(xpa,ypa,x2,y2,xqb,yqb,x4,y4);
       elsif dist2 <> Big then
         case_is := 2;
         width_p := Big;
         width_q := dist2;
         dist_qlong := diverge_a(6);
       elsif dist4 <> Big then
         case_is := 4;
         width_p := dist4;
         width_q := Big;
         dist_plong := diverge_b(6);
       end if;
--       dbms_output.put_line('CASE ' || case_is);
       if dist_plong = 0.0 AND dist_qlong = 0.0 then
         case_is := 0.0;
       end if;
       
       score  := diverge_a(1);
       score2 := diverge_b(1);
       angle1 := diverge_a(2);
       
-- So we get 2 distances, dist_a which is distance (a1 to other line)


 
 
-- Since the segments go in different directions, we can end up with overlapping
-- segments with no widths calculated when we only use the start of the segments:
--               OK                                No good
--        -----<------                            ------<-----------
--            |       |                        X                    X
--            ------->------                   ---------->-------

--       if dist_a = Big and dist_b = Big then
--          if Distance_fcn(x1,x2,x4,y4) < Distance_fcn(x2,y2,x3,y3) then
--             dist_a := GZ_SUPER.Perpendicular(x4,y4,x1,y1,x2,y2,xp,yp,FALSE, TRUE);
--          else             
--             dist_b := GZ_SUPER.Perpendicular(x2,y2,x3,y3,x4,y4,xp2,yp2,FALSE, TRUE);
--          end if;       
--       end if;
     --  dbms_output.put_line('D ' || dist_along || ' D2 ' || dist_along2);
-- Get the lowest score  the angle
       if score2 < score then  score := score2; end if;
   END IF;
       Return mdsys.sdo_list_type(angle1,score,width_p,width_q,dist_plong,dist_qlong,
                                   x1,y1,xpb,ypb,  x3,y3,xqa,yqa, cpx_nu,cpy_nu);
       
End trip;
--
Function navigate (Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,side1_vertex PLS_INTEGER,side2_vertex PLS_INTEGER,which PLS_INTEGER,
                start_of_ring1 PLS_INTEGER, end_of_ring1 PLS_INTEGER, start_of_ring2 PLS_INTEGER,end_of_ring2 PLS_INTEGER,dist_threshold NUMBER default NULL,
                Subject_segs IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, Matches IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                Scores IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, Angles_apart IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                Cases IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Distances IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                Xps IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Yps IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                Xfs IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Yfs IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                Xos IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Yos IN OUT NOCOPY MDSYS.SDO_LIST_TYPE
                ) RETURN MDSYS.SDO_LIST_TYPE as

-- We make a succession of trips and record our results.
-- We navigate from the current pointer which points to the middle of arrays
-- Subject segments, matches and scores.
--
--                      cp
--                      |
--                      v
--     Subject segs ..  side1 vertex       .. room to go forwards
--     Matches          side2 vertex
--     Scores           score for these
-- We can go forwards or backwards as directed.
-- We write into these arrays our results and overwrite if necessary (when we get a
-- failure)

    Side_1      pls_integer;
    Side_2      pls_integer;

    cplast      pls_integer;
    loops       pls_integer :=0;
    results     mdsys.sdo_list_type;
    case_is     number;

    success     number := 60.;
    angle_12    number;


    
    xpa         number;
    ypa         number;
    xqa         number;
    yqa         number;
    xpb         number;
    ypb         number;
    xqb         number;
    yqb         number; 
    first_succeed boolean;
    done        boolean := FALSE;


-- Make an excursion along the pipe xys in a particular direction.
-- If we fail beyond the initial step we have located the start point.
-- If we succeed, we have found an updated start point and return it.

--
-- Since we have to 2 loops, one for side1 and 1 for side2 the
-- excursion is at most 1 segment from the current situation.
-- We begin with the successful segment and its projection and try the next.

-- Possibilities: "(" = known projection, "|" new perpendicular projection
-- 
--                a)    +------------------------------------+   Side 1
--                         (      |
--                         +------+----------+
--      success known for Side 2  vertex next2  succeeds
--
--
--        success known for  Side1  vertex next1  succeeds
--                 b)     +---------+------------+
--                        (         |
--                     +--------------------------------------+  Side 2
--
-- 
--                                  /
--                        Side 1   / next1
--                 c)   +---------+
--                         (
--                         +     |   <-- note how vertex next1 succeeds but
--                           \
--                    side 2   \
--                               \ / The perpendicular from vertex
--                                +  next2 fails and does not project onto side 1
--
---                                  
--                        Side 1  
--                 d)   +--------------+ vertex next2 succeeds but
--                         (         / |
--                         +       /   |   
--                           \   /     |
--                    side 2   \       |
--                               \     |
--                                +____|  vertex next2 succeeds
--
   result       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(NULL,NULL,NULL,NULL,NULL);
   
   Widths        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
--   Box           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  
   Big           NUMBER := 1.E10;

   dist1         NUMBER;
   dist2         NUMBER;
   dist12        NUMBER := Big;
   dist3         NUMBER;
   dist4         NUMBER;
   dist34        NUMBER := Big;
   x1            NUMBER;
   y1            NUMBER;
   x2            NUMBER;
   y2            NUMBER;
   x3            NUMBER;
   y3            NUMBER;
   x4            NUMBER;
   y4            NUMBER;
   xi            NUMBER;
   yi            NUMBER;
   cpx_orig      NUMBER;
   cpy_orig      NUMBER;
   score         NUMBER;
   angle_found   NUMBER;
   angle_apart   NUMBER;
   width         NUMBER;
  
   projector     NUMBER :=0.0;
   onto          NUMBER;
   projector1    NUMBER :=0.0;
   current_posx  NUMBER;
   current_posy  NUMBER;

   dist_found    NUMBER;
   dist_close    NUMBER;
   dist_far      NUMBER;
   cutoff        NUMBER := dist_threshold;
   cutoff_width  NUMBER;
   dual          VARCHAR2(2);
   cpleft        pls_integer;

   side1         PLS_INTEGER;
   next1         PLS_INTEGER := side1_vertex;
   side2         PLS_INTEGER ;
   next2         PLS_INTEGER := side2_vertex; 
   next_side1    PLS_INTEGER;
   next_side2    PLS_INTEGER;
   pick          PLS_INTEGER;
   success1      PLS_INTEGER;
   success2      PLS_INTEGER;
   inc1          PLS_INTEGER := 1;
   inc2          PLS_INTEGER := -1;
   cp            PLS_INTEGER;
   mp            PLS_INTEGER;
   amount        PLS_INTEGER := 2000;
--==============================================================================      
   procedure left_pack_ntrim(left pls_integer,right pls_integer,a_ray in out nocopy mdsys.sdo_list_type) as

-- left pack and trim the arrays that were filled from the center

      n_of       pls_integer := right-left+1;
   begin
       for ii in 1..n_of loop
           a_ray(ii) := a_ray(ii+left-1);       
       end loop;
       a_ray.trim(a_ray.count-n_of);
   end;
   
   Function Findit(find_seg pls_integer) return pls_integer as
    
--  Look for a particular segment in both the Subject Segments and the Matching segments
    Begin
        For pp in 1..Subject_Segs.count loop
           if Subject_Segs(pp) = find_seg or Matches(pp) = find_seg then
              Return pp;
           end if;
        End loop;
  
        Return 0; -- no find
    End;
--==============================================================================
   Begin
 
 -- Setup our current_position using the vertex (which) 
 -- that projected onto the 2nd segment
 -- We measure all the distances (lengths) from the current position

   if which < 3 then
     pick := which*2-2;
     current_posx := Xys(side1_vertex*2+pick-1);
     current_posy := Xys(side1_vertex*2+pick);
   else
     pick := which*2-6;
     current_posx := Xys(side2_vertex*2+pick-1);
     current_posy := Xys(side2_vertex*2+pick);
   end if;
   cpx_orig := current_posx;
   cpy_orig := current_posy;
   
   Subject_Segs.extend(amount- Subject_Segs.count);
   Matches.extend(amount - Matches.count);
   Angles_Apart.extend(amount - Angles_apart.count);
   Cases.extend(amount - Cases.count);
   Distances.extend(amount - Distances.count);
   Scores.extend(amount - Scores.count);
   Widths.extend(amount - Widths.count);
   Xes.extend(amount - Xes.count);
   Yes.extend(amount - Yes.count);
   Xps.extend(amount - Xps.count);
   Yps.extend(amount - Yps.count);
   Xfs.extend(amount - Xfs.count);   -- forward
   Yfs.extend(amount - Yfs.count);
   Xos.extend(amount - Xos.count);   -- other projected
   Yos.extend(amount - Yos.count);
   
   
-- This is our current middle point position in the arrays, from which
-- we can go back or forwards

   mp := TRUNC(amount/2);
   cp := mp;
 
--==============================================================================
--
-- Start with 2 known semiparallel segments and a vertex on side 1 that projects
-- onto side 2 or vice versa
   
      side_1 := side1_vertex;
      side_2 := side2_vertex;
      
 --     dbms_output.put_line('STARTING WITH : ' || side_1 || ',' || side_2 || ' which ' || which || 'cpx ' || round(cpx_orig,7) || ',' || round(cpy_orig,7));
 -- 
 --   Begin by recording the score for the current 2 segments that are known
 --   to be semiparallel.
 --   We have a vertex at the current position to measure along the path distances from

      results := trip(Xys,start_of_ring1,end_of_ring1,start_of_ring2,end_of_ring2,
                       side_1,0,side_2,0,current_posx,current_posy,cutoff,
                       case_is,score, angle_apart, width, dist_found,
                       x1,y1,x2,y2,x3,y3,x4,y4,
                       xpa,ypa,xqa,yqa,xpb,ypb,xqb,yqb);
      
      angle_found  := results(1);
      dist_close := results(3);
      dist_far   := results(4);
 --     if results(1) < success then
 --       dbms_output.put_line('Success at ' || (side_1) || ' with ' || side_2 || ' ANGLE ' || round(results(1),3) || ' CASE ' || case_is);
 --     else
 --      dbms_output.put_line('Failure at ' || (side_1) || ' with ' || side_2 || ' ANGLE ' || round(results(1),3));       
 --     end if;
     IF case_is > 0 THEN   -- Sidey did this was <> 0
     
      Cases(cp) := case_is;
      Scores(cp) := score;
      Angles_apart(cp) := angle_apart;
      Distances(cp) := 0.0;
--    dbms_output.put_line('Success at ' || (side_1) || ' with ' || side_2 || ' CASE: ' || case_is || ' dc ' || round(dist_close,3) || ' dist_f ' || round(dist_far,3));
      success1 := side1_vertex;
      success2 := side2_vertex;
 
      if case_is = 12 or case_is = 13 then
        Subject_Segs(cp) := side1_vertex;
        Matches(cp) := side2_vertex;
        Widths(cp) := dist_close;
        Xes(cp) := x1;
        Yes(cp) := y1;
        Xps(cp) := xpb;
        Yps(cp) := ypb;
        if case_is = 12 then
          Xfs(cp) := x2;
          Yfs(cp) := y2;
          Xos(cp) := xqb;
          Yos(cp) := yqb;
        elsif case_is = 13 then
          Xfs(cp) := x3;
          Yfs(cp) := y3;
          Xos(cp) := xqa;
          Yos(cp) := yqa;               
        end if;
      elsif case_is = 42 or case_is = 43  then 
        Subject_Segs(cp) := side1_vertex;   
        Matches(cp) := side2_vertex;
        Widths(cp) := results(4);       
        Xes(cp) := x4;
        Yes(cp) := y4;
        Xps(cp) := xpa;
        Yps(cp) := ypa;
        if case_is = 42 then
          Xfs(cp) := x2;
          Yfs(cp) := y2;
          Xos(cp) := xqb;
          Yos(cp) := yqb;
        elsif case_is = 43 then
          Xfs(cp) := x3;
          Yfs(cp) := y3;
          Xos(cp) := xqa;
          Yos(cp) := yqa;               
        end if;
      elsif case_is = 4 then
        Subject_Segs(cp) := side1_vertex;  
        Matches(cp) := side2_vertex;
        Widths(cp) := results(4);       
        Xes(cp) := x4;
        Yes(cp) := y4;
        Xps(cp) := xpa;
        Yps(cp) := ypa;
        
      elsif case_is = 2 then
        Subject_Segs(cp) := side1_vertex;   
        Matches(cp) := side2_vertex;
        Widths(cp) := results(4);       
        Xfs(cp) := x2;
        Yfs(cp) := y2;
        Xos(cp) := xqb;
        Yos(cp) := yqb;
      end if;
 
--==============================================================================
    --  Going backwards on the matching segments
    --  for any 2 segments there are 3 remaining adjacent possibilities
    --              done    <---           remaining             ---->
    
    --              side_1  |   side_1        side_1 +1      side_1     +1
    --              side_2  |   side_2-1      side_2         side_2-2     -1
      cplast := cp;

      While loops < TRUNC(amount/2) Loop
        loops := loops + 1;
         
        for ii in 1..6 loop
        
          side_1 := Subject_segs(cplast);
          side_2 := Matches(cplast);
--          dbms_output.put_line('cplast ' || cplast || ' SS ' || side_1 || ' M ' || side_2);

      
          if ii = 1 then
            inc1 := 0;
            inc2 := -1;
          elsif ii = 2 then
            inc1 :=+1;
            inc2 :=0;
          elsif ii = 3 then 
            inc2 :=-1;
          elsif ii = 4 then
             inc1 :=0;
             inc2 := -2;
          elsif ii=5 then
             inc1 := 2;
             inc2 := 0;
          else
             inc2 :=-1;
          end if;

-- Describe our pairing with a 2 characters: 'PP','NP','PN','NN'
-- where 'P' = pipe and 'N' = 'not a pipe

          dual := 'N';
          if findit(side_1+inc1) > 0 then
             dual := 'P';
          end if;
          if findit(side_2+inc2) > 0 then
             dual := dual||'P';
          else
             dual := dual||'N';
          end if; 

--dbms_output.put_line('ii ' || ii || ' side1 ' || (side_1+inc1) || ' side2 ' || (side_2+inc2));
          results := trip(Xys,start_of_ring1,end_of_ring1,start_of_ring2,end_of_ring2,
                     side_1,inc1,side_2,inc2,current_posx,current_posy,cutoff,
                     case_is,score, angle_apart, width, dist_found, 
                     x1,y1,x2,y2,x3,y3,x4,y4,
                     xpa,ypa,xqa,yqa,xpb,ypb,xqb,yqb);
 --        dbms_output.put_line('ii ' || ii || ' case ' || case_is); 
          angle_found  := results(1);
          dist_close := results(3);
          dist_far   := results(4);

          IF case_is > 4 THEN
--          dbms_output.put_line('ii ' || ii || ' cpx ' || current_posx || ' CASE ' || case_is || ' Trying side ' ||(side_1) || ' at side2 ' || (side_2) || ' res1 ' || round(results(1),4)|| ' res ' || round(results(3),4) || ' Res ' || round(results(4),4) || ' RR ' || round(results(5),4) || ' R ' || round(results(6),4));
       
-- For the Subject Segments (SS) and the matches (M) note the way the SS 
-- is equal or decreases and the M is equal or increases.
-- Note: we do not compare the same segment or segments that differ by 1 since
-- they connect.
-- This is a case for a 2-sided pipe   and this is a 1-sided pipe

--SS 145 M 8 score 0                      SS 111 M 93 score 0  
--SS 144 M 8 score 0                      SS 111 M 94 score 9.1695
--SS 143 M 8 score 6.1762                 SS 111 M 95 score 0
--SS 143 M 9 score 0                      SS 110 M 96 score 7.7667
--SS 143 M 10 score 8.8638                SS 109 M 97 score 7.6372
--                                        SS 108 M 97 score 7.6372
--                                        SS 108 M 98 score 0
--                                        SS 107 M 98 score 9.6271
--                                        SS 107 M 99 score 0
--                                        SS 106 M 99 score 0
--                                        SS 106 M 100 score 0
--                                        SS 105 M 100 score 0

--
-- 
         cutoff_width := cutoff;
         if side_1 = success1 or side_2 = success2 then
            cutoff_width := cutoff*1.2;
         end if;
-- check side_1  <> side_2
         if (((angle_found < success and (dist_close < cutoff_width or dist_far < cutoff_width) and dual <> 'NN') or ((dist_close < cutoff_width and dist_far < cutoff_width) and angle_found < 90. and dual = 'PP')) AND --(180.-success))) AND
                side_1 >= Subject_segs(cp) and side_2 <= Matches(cp) and ABS(side_1 - side_2) > 1) and case_is > 4  then
        if side_1 =  344 then    
                 dbms_output.put_line('>>>>>>success at cp ' || (cp-1) || ' Score ' ||results(2) || ' ,' ||(side_1) || ' with ' || (side_2) || ' ANGLE ' || round(results(1),3)|| ' Cases: ' || case_is || ' DC ' || round(dist_close,3) || ' DF ' || round(dist_far,3)||dual);
        end if;
--          we don't want a with b and then b with a

            exit when side_1 = Matches(cplast) and side_2 = Subject_segs(cplast);
            exit when cp =1;
            cp := cp-1;
              
            Cases(cp) := case_is;         
            Scores(cp) := score;
            Angles_apart(cp) := angle_apart;
            success1 := side_1;
            success2 := side_2;
--            dbms_output.put_line('cpx ' || current_posx || ' cpy ' || current_posy);
            
-- We indicated which segment is a projected result with a trailing 0.1.
-- This means we don't want the beginning of the segment, we want a point
-- projected onto from the other segment

           
          if case_is = 12 or case_is = 42 then
              Subject_segs(cp) := side_1;                   
              Matches(cp) := side_2;
              Widths(cp) := results(3);
              Distances(cp) := results(5);
--              dbms_output.put_line('cp ' || cp || ' SS ' ||side_1 || 'M ' || side_2);
              Xfs(cp) := x2;
              Yfs(cp) := y2;
              Xos(cp) := xqb;
              Yos(cp) := yqb;  
              if case_is = 12 then
                Xes(cp) := x1;
                Yes(cp) := y1;
                Xps(cp) := xpb;
                Yps(cp) := ypb;
              elsif case_is = 42 then
                Xes(cp) := x4;
                Yes(cp) := y4;
                Xps(cp) := xqa;
                Yps(cp) := yqa;               
              end if;   
              
              current_posx := Xfs(cp);
              current_posy := Yfs(cp);
--              dbms_output.put_line('Cpx ' || current_posx || ' cpy ' || current_posy);
              
            elsif case_is = 13 or case_is = 43 then
              Subject_segs(cp) := side_1;                   
              Matches(cp) := side_2;
              Widths(cp) := results(4);
              Distances(cp) := results(6);
--              dbms_output.put_line('Cp ' || cp || ' SS ' ||side_1 || 'M ' || side_2);
              Xfs(cp) := x3;
              Yfs(cp) := y3;
              Xos(cp) := xqa;
              Yos(cp) := yqa;
               if case_is = 13 then
                Xes(cp) := x1;
                Yes(cp) := y1;
                Xps(cp) := xpb;
                Yps(cp) := ypb;
              elsif case_is = 43 then
                Xes(cp) := x4;
                Yes(cp) := y4;
                Xps(cp) := xpa;
                Yps(cp) := ypa;               
              end if;   
--              dbms_output.put_line('cpx ' || current_posx || ' Cpy ' || current_posy);
 
              current_posx := Xfs(cp);
              current_posy := Yfs(cp);
                      
    
            end if;
                
            
--          When we complete n+1 with n, then n with n+1 is not allowed and we are done
--          Also when we get to n+2 and n, we know they are connected by n+1
            if ABS(side_1-side_2) = 2 then   -- hence the "2"
                cplast := cp;
                exit;
            else
                exit when cp <> cplast;
            end if;
--          else
--           dbms_output.put_line('failure at ' || (side_1) || ' with ' || (side_2) || ' ANGLE ' || round(results(1),3));
--             dbms_output.put_line('case ' || case_is || ' success at cp ' || (cp) || ' ,' || subject_segs(cp) || ' Matches ' || Matches(cp) ||'  DIST ' || round(Distances(cp),3) || ' cpx ' || current_posx);
          end if;
          END IF;
        end loop;
 
        exit when cp = cplast; -- or cp=1;
        cplast := cp;
     End Loop;
 
--==============================================================================
 --  Going forwards on the matching segments
 --  for any 2 segments there are 3 remaining adjacent possibilities
    --              done                remaining
    
    --              side_1  |   side_1        side_1 -1      side_1 -1
    --              side_2  |   side_2+1      side_2         side_2 +1
     cpleft := cplast;
     cplast := mp;
     cp := mp;
     current_posx := cpx_orig;
     current_posy := cpy_orig;
--dbms_output.put_line('TRING other direction cplast ' || cplast);
     While loops < TRUNC(amount/2) Loop
        loops := loops + 1;
         
        for ii in 1..5 loop
          side_1 := Subject_segs(cplast);
          side_2 := Matches(cplast);
--            dbms_output.put_line('cplast ' || cplast || ' SS ' || side_1 || ' M ' || side_2);
          if ii = 1 then
            inc1 := 0;
            inc2 := +1;
          elsif ii = 2 then
            inc1 := -1;
            inc2 :=0;
          elsif ii = 3 then
            inc2 := +1;
          elsif ii=4 then
            inc1 := 0;
            inc2 := +2;
          else
            inc1 := -1;
            inc2 := +2;
          end if;
          
          dual := 'N';
          if findit(side_1+inc1) > 0 then
             dual := 'P';
          end if;
          if findit(side_2+inc2) > 0 then
             dual := dual||'P';
          else
             dual := dual||'N';
          end if; 
          results := trip(Xys,start_of_ring1,end_of_ring1,start_of_ring2,end_of_ring2,
                          side_1,inc1,side_2,inc2,current_posx,current_posy,cutoff,
                          case_is,score, angle_apart, width, dist_found,
                          x1,y1,x2,y2,x3,y3,x4,y4,
                       xpa,ypa,xqa,yqa,xpb,ypb,xqb,yqb);
        
         angle_found  := results(1);
         dist_close := results(3);
         dist_far   := results(4);
         IF case_is > 4 THEN
           cutoff_width := cutoff;
           if side_1 = success1 or side_2 = success2 then
            cutoff_width := cutoff*1.2;
           end if;
--         dbms_output.put_line('II ' || ii || ' case ' || case_is || ' trying side ' ||(side_1) || ' at side2 ' || (side_2) || ' RES1 ' || round(results(1),4) || ' rees ' || round(results(3),4) || ' Res ' || round(results(4),4) || ' rr ' || round(results(5),4) || ' r  ' || round(results(6),4));
          if (((angle_found < success and (dist_close < cutoff_width or dist_far < cutoff_width) and dual <> 'NN') or ((dist_close < cutoff_width and dist_far < cutoff_width) and angle_found < 90.and dual = 'PP')) AND --(180.-success))) AND
              side_1 <= Subject_segs(cp) and side_2 >= Matches(cp) and ABS(side_1 - side_2) > 1)  and case_is > 4  then
   if side_1 =  344 then    
                 dbms_output.put_line('>>>>>SSuccess at cp ' || (cp-1) || ' Score ' ||results(2) || ' ,' ||(side_1) || ' with ' || (side_2) || ' ANGLE ' || round(results(1),3)|| ' Cases: ' || case_is || ' DC ' || round(dist_close,3) || ' DF ' || round(dist_far,3)||dual);
        end if;
--          we don't want a with b and then b with a

            exit when side_1 = Matches(cplast) and side_2 = Subject_segs(cplast);
            
--              dbms_output.put_line('Success at cp ' || (cp+1) || ' ,' ||(side_1) || ' with ' || (side_2) || ' ANGLE ' || round(results(1),3) || ' case: ' || case_is);
--               dbms_output.put_line('close ' ||round(dist_close,3) ||' far ' || round(dist_far,3) || ' cutoff ' || cutoff);
            cp := cp+1;
           
            Cases(cp) := case_is;
            Scores(cp) := score;
            Angles_apart(cp) := angle_apart;
            success1 := side_1;
            success2 := side_2;

            if case_is = 12 or case_is = 13 then
               Subject_segs(cp) := side_1;                
               Matches(cp) := side_2; 
--               dbms_output.put_line('ccp ' || cp || ' SS ' ||side_1 || 'M ' || side_2);
               Widths(cp) := results(3);
               Distances(cp) := results(5);
               Xes(cp) := x1;
               Yes(cp) := y1;
               Xps(cp) := xpb;
               Yps(cp) := ypb;
               if case_is = 12 then
                Xfs(cp) := x2;
                Yfs(cp) := y2;
                Xos(cp) := xqb;
                Yos(cp) := yqb;
               elsif case_is = 13 then
                Xfs(cp) := x3;
                Yfs(cp) := y3;
                Xos(cp) := xqa;
                Yos(cp) := yqa;               
               end if; 
               current_posx := Xfs(cp);
               current_posy := Yfs(cp);
            elsif case_is = 42 or case_is = 43 then            
               Subject_segs(cp) := side_1;                
               Matches(cp) := side_2;    
--               dbms_output.put_line('cpp ' || cp || ' SS ' ||side_1 || 'M ' || side_2);
               Widths(cp) := results(4);
               Distances(cp) := results(6);
               Xes(cp) := x4;
               Yes(cp) := y4;
               Xps(cp) := xpa;
               Yps(cp) := ypa;
               if case_is = 42 then
                Xfs(cp) := x2;
                Yfs(cp) := y2;
                Xos(cp) := xqb;
                Yos(cp) := yqb;
              elsif case_is = 43 then
                Xfs(cp) := x3;
                Yfs(cp) := y3;
                Xos(cp) := xqa;
                Yos(cp) := yqa;               
              end if;   
               current_posx := Xfs(cp);
               current_posy := Yfs(cp);
            end if;           
            
--            dbms_output.put_line('SUCCess at cp ' || (cp) || ' ,' || subject_segs(cp) || ' matcvhes ' || Matches(ii) || ' DIST ' || Distances(cp));
            
--          When we complete n+1 with n, then n with n+1 is not allowed and we are done
            if ABS(side_1-side_2) = 2 then
                cplast := cp;
                exit;
             else
                exit when cp <> cplast;
            end if;
--          else
--           dbms_output.put_line('failure at ' || (side_1) || ' with ' || (side_2) || ' ANGLE ' || round(results(1),3));
          
          end if;
          END IF;
      end loop;
      exit when cp = cplast;   -- nothing further found
      cplast := cp;
     End Loop;

  
     left_pack_ntrim(cpleft,cplast,Subject_Segs);
     left_pack_ntrim(cpleft,cplast,Matches);
     left_pack_ntrim(cpleft,cplast,Scores);
     left_pack_ntrim(cpleft,cplast,Widths);
     left_pack_ntrim(cpleft,cplast,Distances);
     left_pack_ntrim(cpleft,cplast,Cases);
     left_pack_ntrim(cpleft,cplast,Xes);
     left_pack_ntrim(cpleft,cplast,Yes);
     left_pack_ntrim(cpleft,cplast,Xps);
     left_pack_ntrim(cpleft,cplast,Yps);
     left_pack_ntrim(cpleft,cplast,Xfs);
     left_pack_ntrim(cpleft,cplast,Yfs);
     left_pack_ntrim(cpleft,cplast,Xos);
     left_pack_ntrim(cpleft,cplast,Yos);
--     dbms_output.put_line('navigate');
--             for ii in 1..cplast-cpleft+1 loop
--           dbms_output.put_line('SS ' || subject_segs(ii) || ' M ' || matches(ii) || ' score ' || round(Scores(ii),4) || ' W ' || round(widths(ii),3) || ' D ' || round(Distances(ii),1)); -- || ' ' ||round(xes(ii),6)||','||round(yes(ii),6) || ','||round(xps(ii),6) || ','||round(yps(ii),6));
--        end loop;
  ELSE
     Subject_segs.trim(Subject_segs.count);
     RETURN NULL;
  END IF;

        
        Return Widths;
   End Navigate;   
--
Function Get_Pipe_Geom(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Info MDSYS.SDO_ELEM_INFO_ARRAY,start_vertex PLS_INTEGER,across_vertex PLS_INTEGER,
                        
                        which PLS_INTEGER,cutoff NUMBER,PSL IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,average_width IN OUT NOCOPY NUMBER, accum_length IN OUT NOCOPY NUMBER ) RETURN MDSYS.SDO_GEOMETRY AS

-- Traverse amd Measure the widths of a pipe starting from the start and locating
-- the end.

-- The Processed segment list, contains either 1 range (a pair of segment numbers) or
-- 2 ranges. Used to help the caller from calling this function again with parallel
-- lines already processed

   Subject_segs  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Matches       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Angles_apart  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Scores        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Cases         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Distances     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Widths        Mdsys.sdo_list_type := Mdsys.sdo_list_type();
   Xes           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Yes           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Xps           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Yps           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Xfs           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Yfs           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Xos           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Yos           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(); 
   New_Xys       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(); 
   Geom          MDSYS.SDO_GEOMETRY;
   
   cutoff_width     NUMBER := cutoff;   
   start1           NUMBER;
   start2           NUMBER;
   end1             NUMBER;
   end2             NUMBER;
   million          number := 1000000.;
   pipe_len         NUMBER;
   dist_threshold   NUMBER :=0.;
   intersects_itself number;

   projector     NUMBER;
   Big           NUMBER := 1.E10;
   LB            PLS_INTEGER;
   UB            PLS_INTEGER;

   sv            PLS_INTEGER := start_vertex;
   av            PLS_INTEGER := across_vertex;
   
   next_sv       PLS_INTEGER;
   next_av       PLS_INTEGER;
   n             PLS_INTEGER;

   first         PLS_INTEGER;
   last          PLS_INTEGER;
   ibeg          PLS_INTEGER;
   istop         PLS_INTEGER;
   kount         PLS_INTEGER;
   nof            PLS_INTEGER;
   next          PLS_INTEGER :=0;
   nof_xy        PLS_INTEGER := TRUNC(Xys.count/2);

   loops         PLS_INTEGER := 0;
   
   procedure copy_xys(istart pls_integer,iend pls_integer) as
   
   begin
 -- istart should be odd and iend even
 
      if istart < iend then
       for ii in istart..iend loop
          next := next+1;
          New_Xys(next) := Xys(ii);
       end loop;
      else
-- say we go from 10..1   
       for ii in reverse iend..istart loop     -- 
          
--          dbms_output.put_line('II ' || ii || ' next ' || next);
          if mod(ii,2) = 0 then   -- get a Y
            New_Xys(next+2) := Xys(ii);
            next := next+2;
          else                    -- get an X
            New_Xys(next-1) := Xys(ii);
          end if;

       end loop;      
      end if;
   end;
 
  function find_intersections RETURN NUMBER AS
 
 -- Exhaustive combinatorial search of every segment intersecting any
 -- other segment.That is, 1 with 3, 1 with 4,.., 2 with 4, 2 with 5 ..
 -- Just returns the first intersection it finds.
 -- 
      m        pls_integer := TRUNC(next/2); 
      x0       number;
      y0       number;
      x1       number;
      y1       number;
      x2       number;
      y2       number;
      x3       number;
      y3       number;
      
   begin

      x1 := New_xys(1);
      y1 := New_Xys(2);


-- Do 2 things here, check for line intersections.
-- The intersections require 2 segments.

      for ii in 2..m-1 loop
        x0 := x1;
        y0 := y1;
        x1 := New_Xys(ii*2-1);
        y1 := New_Xys(ii*2);
        x3 := New_Xys(ii*2+1);
        y3 := New_Xys(ii*2+2);
 
        
        for jj in ii+2..m loop
 
            x2 := x3;
            y2 := y3;
            x3 := New_Xys(jj*2-1);
            y3 := New_Xys(jj*2);
   
            -- check for an intersection between line segments

            if simple_intersect(x0,y0,x1,y1,x2,y2,x3,y3) then

               -- return end vertices of line segment
               return ii*million + jj;
            end if;
            
        end loop;     
      end loop;
      return 0.;
   end;
   
Begin
--dbms_output.put_line('in get_pipe_geom'|| new_xys.count);
  -- Generate an array of widths by finding points along an edge starting from 
  -- start vertex


-- Loop over adjacent segments walking in opposite directions. For each type
-- 1) peninsula, 2) estuary, 3) isthmus and 4) a hole the segments across the
-- pipe go in opposite directions.

-- Caller can just pass the lower segment number in the start_vertex and the
-- higher segment number in the across_vertex. Caller can reverse this when the 
-- desired next start_vertex is near the end of the start ring:
--      | 10
--      *---------- +      vertex 1
--                 |
--      *----------n       n = node at vertex 100
--      | 90       100

 
       
      -- The ii segment may be longer than the jj segment
      -- so we exit when we can no longer get a perpendicular distance
 
       
       -- In sucession try dropping a perpendicular from 
       --  1) (x1,y1) in 3 to 4   2) (x2,y2) onto 3 to 4
       --  3) (x3,y3) onto 1 to 2 4) (x4,y4) onto 1 to 2
  

       For ii in 1..TRUNC(Info.count/3) loop
          first := TRUNC((Info(ii*3-2)+1)/2);
          if ii = TRUNC(Info.count/3) then
             last := TRUNC(Xys.count/2);
--             dbms_output.put_line('Set Last to ' || last || ' across ' || across_vertex);
          else
             last := TRUNC(Info(ii*3+1)/2);
--             dbms_output.put_line('SEt last to ' || last || ' across ' || across_vertex);
          end if;
          if start_vertex < last and start1 is NULL then
             start1 := first;
             end1 := last;
          end if;
          if across_vertex  <= last and start2 is NULL then
             start2 := first;
             end2 := last;
          end if;
       End Loop;

-- Try excursions forwards or backwards until it fails.
-- then go the opposite direction and save the results



--       dbms_output.put_line('***************calling NAVIGATE ' || start1 || ',' || end1 || ',' ||start2 || ',' || end2 || ' count ' || xys.count);
       Widths := Navigate(Xys,sv,av,which,start1,end1,start2,end2,cutoff_width,
                           Subject_segs,Matches,Scores,Angles_apart,Cases,Distances,Xes,Yes,Xps,Yps,Xfs,Yfs,Xos,Yos);

    n := Subject_Segs.count;
--dbms_output.put_line('NN ' || n);

--  These are the Processed Segment List, a range specified as pairs of segments
    PSL.trim(PSL.count);

    if n =0 then --or n= 1 and ABS(subject_segs(1)-Matches(1)) > 5 then 
       RETURN NULL;
    end if;


-- Calculate the average_width.
    average_width := 0.0;
    Accum_length :=0.0;
    nof := 0;
    For ii in 1..n loop
--    dbms_output.put_line('Widths ' || round(widths(ii),3));
       if widths(ii) <> Big then
       average_width := average_width + widths(ii);
       accum_length := accum_length + distances(ii);
       nof := nof + 1;
       end if;
    end loop;
    if nof <> 0 then
     average_width := average_width/nof;
    else 
     average_width := 1.;
    end if;
    
--  The count we expect 
    kount := (4 + Subject_segs(1) - Subject_segs(n) +2 + Matches(n)-matches(1) + 2)*2;
    
    
    New_xys.extend(kount);
    
-- Decipher the pipe description: This is for a peninsula. In both cases
-- we start at segment 98 and go to 108
--
--SS 101 M 103                  SS 108 M 98
--SS 101 M 104                  SS 107 M 98
--SS 100 M 105                  SS 107 M 99
--SS 100 M 106                  SS 106 M 99 
--SS 99 M 106                   SS 106 M 100
--SS 99 M 107                   SS 105 M 100
--SS 98 M 107                   SS 104 M 101
--SS 98 M 108                   SS 103 M 101

   PSL.extend(2);
--   dbms_output.put_line('xys ' || xys.count || ' SS ' || subject_segs(1)|| ' M ' || matches(1) || ' next ' || next);
--   dbms_output.put_line('xys ' || xys.count || ' SS ' || subject_segs(n)|| ' M ' || matches(n) || ' next ' || next);

   if Subject_segs(n) = Matches(n)+2 or Subject_segs(1) = Matches(1)+3 or Subject_segs(1) = Matches(1)+4 then   
     ibeg := Matches(1)*2-1;
     istop := Subject_segs(1)*2+2;
 
     PSL(1) := TRUNC((ibeg+1)/2);
     PSL(2) := TRUNC(istop/2)-1;
--    dbms_output.put_line('1beg ' || ibeg || ' istop ' || istop|| ' SS ' || Subject_segs(1)|| ' M ' || Matches(1));
    
     copy_xys(ibeg,istop);
--dbms_output.put_line('next ' || next || ' new ' || new_xys.count);

-- Replace 1st coordinate if it is a perpendicular
      if Cases(1) = 12 or Cases(1) = 42 or Cases(1) = 4 then
        New_Xys(1) := Xps(1);
        New_Xys(2) := Yps(1); 
      end if;
--dbms_output.put_line('case ' || cASES(1));
      if Cases(1) = 13 or Cases(1) = 43 then
        New_Xys(next-1) := Xos(1);
        New_Xys(next) := Yos(1);
      end if;    
   
   intersects_itself := find_intersections;
   
--   dbms_output.put_line('Itself ' || intersects_itself);
   
   Elsif  Subject_segs(1) = Matches(1)-2 or Subject_segs(1) = Matches(1)-3 or  Subject_segs(1) = Matches(1)-4 then
       ibeg := Subject_segs(n)*2-1;
       istop := Matches(n)*2+2;

       PSL(1) := TRUNC((ibeg+1)/2);
       PSL(2) := TRUNC(istop/2)-1;
--    dbms_output.put_line('2beg ' || ibeg || ' istop ' || istop|| ' SS ' || Subject_segs(n)|| ' M ' || Matches(n));

    copy_xys(ibeg,istop);


-- Add last coordinate if it is a perpendicular

-- Replace 1st coordinate if it is a perpendicular
      if Cases(n) = 42 or Cases(n) = 43 or Cases(n) = 4 then
        New_Xys(1) := Xps(n);
        New_Xys(2) := Yps(n); 
      end if;
-- Replace last coordinate if it is a perpendicular
--  dbms_output.put_line('case ' ||cases(n));
      if Cases(n) = 13 or Cases(n) = 12 then
        New_Xys(next-1) := Xps(n);
        New_Xys(next) := Yps(n);
      end if;    
   
   -- Check for intersections
   
   intersects_itself := find_intersections;
   
--   dbms_output.put_line('ITSelf ' || intersects_itself);
   
-- Two ended pipe, flares (funnels) at both ends  
   Else
--   dbms_output.put_line('2 ended pipe ' || n || ' case ' ||cases(1) || ' casen ' ||cases(n)|| ' SS ' || subject_segs(1));
   ibeg := Subject_segs(n)*2-1;
   istop := Subject_segs(1)*2+2;
--dbms_output.put_line('2 ended pipe ' ||ibeg || ' istop ' || istop ||' SS ' || Subject_segs(n)|| ' SE ' || Subject_Segs(1));
   copy_xys(ibeg,istop);
  
 
   PSL(1) := TRUNC((ibeg+1)/2);
   PSL(2) := TRUNC(istop/2)-1;
 
--Replace at beginning    
    if Cases(n) = 42 or Cases(n) = 43 then
----    dbms_output.put_line('Hhhere');
-- dbms_output.put_line('hhhere ' || ibeg || ' istop ' || istop || ' next ' || next);
      New_Xys(1) := Xps(n);
      New_Xys(2) := Yps(n);
    end if;
    
    if Cases(1) = 13 or Cases(1) = 43 then 
--     dbms_output.put_line('hHHhere '|| next);
    New_Xys(next-1) := Xos(1);
    New_Xys(next) := Yos(1);
    
    end if;
    ibeg := Matches(1)*2-1;

    if Cases(1) = 12 or Cases(1) = 42 then  -- projector replaces 1st vertex
      ibeg := ibeg + 2;
      next := next + 2;
      New_Xys(next-1) := Xos(1);
      New_Xys(next) := Yos(1);
      
    end if;
 
     istop := Matches(n)*2+2;

     PSL.extend(2);
     PSL(3) := TRUNC((ibeg+1)/2);
     PSL(4) := TRUNC(istop/2)-1;

--     dbms_output.put_line('3hhhere ' || ibeg || ' istop ' || istop ||' SS ' || Matches(1)|| ' M ' || Matches(n));
     copy_xys(ibeg,istop); 

-- Replace     
      if Cases(n) = 12 or Cases(n) = 13 then
--    dbms_output.put_line('hhHere'|| next);
      New_Xys(next-1) := Xps(n);
      New_Xys(next) := Yps(n);
     
    end if;  
  
   
   End if;
    if New_xys(1) <> New_xys(next-1) or New_xys(2) <> New_xys(next) then
       next := next +1;
--     dbms_output.put_line('NEXT ' || next || ' count ' || new_xys.count);
        New_xys(next) := New_Xys(1);
        next := next +1;
         New_xys(next) := New_Xys(2);
    end if;
    New_xys.trim(New_xys.count-next);
--    dbms_output.put_line('NOW count ' || new_xys.count);

    if new_xys.count = 0 then    
      RETURN NULL;
    end if;
    Geom := Mdsys.sdo_geometry(2002,8265.,NULL,mdsys.sdo_elem_info_array(1,2,1),
                                New_Xys);

  RETURN geom;
End Get_pipe_Geom;
--
FUNCTION METERS_TO_DEGREES(X_OR_Y VARCHAR2 default 'X',pLength NUMBER, pLatitude NUMBER, tolerance NUMBER default 0.01) RETURN NUMBER AS

-- Basically a reverse distance function to convert meters to degrees as a
-- function of latitude. The conversion may be along a parallel ('X') or a 
-- meridian ('Y').

--     x_or_y  -   Specifies the direction the length is oriented. 'X' is for a small
--                   circle and 'Y' is part of a meridian.
--     plength -   Length in meters to be converted to degrees
--     platitude - Latitude of the center of the line length
--     tolerance - A tolerance in meters to reach. Thus if plength is 1 and
--                 tolerance =0.01 then the result may be for a length between 0.99 and 1.01;

--SQL> select gz_qa.meters_to_degrees('X',100,50,0.000001) from dual;

--GZ_QA.METERS_TO_DEGREES('X',100,50)
-----------------------------------
--                         .001394783
--
--SQL> select sdo_geom.sdo_length(mdsys.sdo_geometry(2002,8265,null,mdsys.sdo_elem_info_array(1,2,1),
--mdsys.sdo_ordinate_array(0,50,0.001394782809,50)),0.05,'unit=meter') Length from dual;

-- Length
-----------
--99.9999835

   y           NUMBER := ABS(pLatitude);
   cosy        NUMBER;
   siny        NUMBER;
   degrees     NUMBER;              -- the returned result
   d           NUMBER := pLength;
   lastd       NUMBER := 0.0;
   dist_factor NUMBER := 111319.490793274;
   
   loops       PLS_INTEGER := 0;
   do_x_or_y   VARCHAR2(1) := UPPER(x_or_y);
   
BEGIN

--   Guess for latitude is length/dist_factor
     degrees := pLength/dist_factor;
     
     if do_x_or_y = 'X' then
       siny := quick_sincos(y,cosy,'Y');
       if y >= 90. then   -- avoid divide by zero.
          RETURN NULL;
       end if;
       degrees := degrees / cosy;
     end if;
     
-- Then iterate to get closer using a difference between a calculated value
-- for distance and the guess

     While ABS(d - lastd) > tolerance and loops <= 10 Loop
       loops := loops + 1;
       lastd := d;
       if do_x_or_y = 'X' then
          d := Distance_Fcn(0.,y,degrees,y);
       else
          d := Distance_fcn(0.,y-degrees*0.5,0.,y+degrees*0.5);
       end if;
       
       degrees := degrees - degrees*(d-plength)/plength;
--       dbms_output.put_line('loops ' || loops || ' diff ' || ROUND(d-lastd,10) || ' Deg ' || round(degrees,10) || ' dist ' || round(d,10));
     end Loop;
     
     RETURN degrees;


END METERS_TO_DEGREES;
--
FUNCTION OPTIMAL_SEQUENCE
RETURN VINTEGER_ARRAY DETERMINISTIC IS

-- Reference: Robert Sedgewick "Algorithms in C" Addison Wesley 1998
--            DL Shell. "A High-Speed Sorting Procedure"
--                       Communications of the ACM, 2,7 30-32 (1959)
--            This improved shellsort sequence was found at:
--            http://cybertiggyr.com/gene/shiva-0/
hes  VINTEGER_ARRAY;

BEGIN
    hes := VINTEGER_ARRAY();
    hes.extend(16);
    hes(1) := 1;
    hes(2) := 4;
    hes(3) := 9;
    hes(4) := 24;
    hes(5) := 58;
    hes(6) := 171;
    hes(7) := 340;
    hes(8) := 1097;
    hes(9) := 2673;
    hes(10) := 5467;
    hes(11) := 13353;
    hes(12) := 35957;
    hes(13) := 128823;
    hes(14) := 451488;
    hes(15) := 494198;
    hes(16) := 499871;
  RETURN hes;
END;
--
FUNCTION Perimeter(Geom MDSYS.SDo_GEOMETRY,pvtx1 PLS_INTEGER default 1,pvtx2 PLS_INTEGER default 0, method VARCHAR2 default 'FAST') RETURN NUMBER DETERMINISTIC AS

  Xys              MDSYS.SDO_ORDINATE_ARRAY;
  XyOrd            MDSYS.SDO_ORDINATE_ARRAY;
  Info_Array       MDSYS.SDO_ELEM_INFO_ARRAY := Geom.sdo_Elem_Info;
  istart           PLS_INTEGER;
  last_vertex      PLS_INTEGER;

BEGIN

  If Info_array(3) = 3 then
     XYOrd := Geom.sdo_ordinates;
     Xys:= MDSYS.SDO_ORDINATE_ARRAY(XYOrd(1),XYOrd(2),XyOrd(3),XYOrd(2),
                                    XYOrd(3),XYOrd(4),XyOrd(1),XYOrd(4),XYOrd(1),XYOrd(2));
         
     Return Perimeter(XYs,Geom.sdo_srid,pvtx1,pvtx2,1,4,method);
  Else
     last_vertex := last_info(pvtx1*2-1,istart,Info_array,Geom.sdo_ordinates.count);
     Return Perimeter(Geom.sdo_ordinates,Geom.sdo_srid,pvtx1,pvtx2,istart,last_vertex,method);
  End if;
END;
--
FUNCTION Perimeter(XYs IN MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER default 8265.,pvtx1 PLS_INTEGER default 1,pvtx2 PLS_INTEGER default 0,istart PLS_INTEGER default 1, last_vertex PLS_INTEGER default 0, method VARCHAR2 default 'FAST') RETURN NUMBER DETERMINISTIC AS

-- Return Perimeter or length of a line string. 
-- Method: Accumulate distances from start to finish.
-- When pvtx1 and pvtx2 are input, measure the distance from vertex 1 to vertex 2.
-- Note that vertex 1 may be greater than vertex 2 (to go round the end of the ring).

  Info             MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1);
  perimeter_len    NUMBER := 0.0;
  x1               NUMBER;
  y1               NUMBER;
  x2               NUMBER;
  y2               NUMBER;
  
  max_loops        PLS_INTEGER := 1;
  vtx1             PLS_INTEGER := pvtx1;
  vtx2             PLS_INTEGER := pvtx2;
  m                PLS_INTEGER := pvtx1;
  n                PLS_INTEGER := TRUNC(Xys.count/2);
  az1  number;
  az2  number;
  m12  number;
  m21  number;
  mm12 number;
  ss12 number;
  d_K  number;  -- the geodesic distance computed by Charles Karney's code
  s    number;
  
BEGIN

    

   If Xys is NULL or vtx1 = vtx2 then
      RETURN perimeter_len;
   End If;
           
   if vtx2 = 0 then
     vtx2 := n;
   end if;
   
   if vtx1 > vtx2 then
     max_loops := 2;
   else
     n := vtx2;
   end if;
   
   For loops in 1..max_loops loop
   
     if loops = 2 then
        m := istart;
        n := vtx2;
     end if;
     
     x2 := Xys(m*2-1);
     y2 := Xys(m*2);
     For ii in m+1..n Loop
       x1 := x2;
       y1 := y2;
       x2 := XYs(ii*2-1);
       y2 := Xys(ii*2);
       if method = 'FAST' then  
          perimeter_len := perimeter_len + distance_fcn(x1,y1,x2,y2,SRID);
       elsif method = 'ORACLE' then
          perimeter_len := perimeter_len + sdo_geom.sdo_length(
                                           mdsys.sdo_geometry(2002,SRID,NULL,
                                           Info,Mdsys.sdo_ordinate_array(x1,y1,x2,y2)),
                                           0.05,'unit=meter');
       else
         s := GZ_GEODESIC.inverse(y1,x1,y2,x2,d_K,az1,az2,mm12,m12,m21,ss12);
         perimeter_len := perimeter_len + d_K;
       end if;
     End Loop;
   End Loop;
   
  RETURN perimeter_len;

END Perimeter;
--
FUNCTION Perpendicular(xin IN OUT NOCOPY NUMBER,yin IN OUT NOCOPY NUMBER,  -- the point
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,  -- the line
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                       xnear IN OUT NOCOPY NUMBER,ynear IN OUT NOCOPY NUMBER,  -- the perpendicular point
                       meters BOOLEAN default FALSE,  -- when true return meters
                       perp_angle_tolerance NUMBER default 0.)
                       RETURN NUMBER deterministic IS
/*
********************************************************************************
--Program Name: Perpendicular
--Author: Sidey Timmins
--Creation Date: 10/16/2008
-- Updated: 10/20/2010 To return the distance when not using geodetic coordinates
--Usage:  --
  --   REQUIRED Parameters:
  --      INPUT 
  --      (xin,yin)   - The point to start from 
  --      (x1,y1)     - Coordinates for start of test line            
  --      (x2,y2)     - Coordinates for end of the line to test.
  --      meters      -- TRUE return meters
  --      perp_angle_tolerance - A cosine limit - cos(75) for example = 0.258.
  --                     So a value of 0.258 checks for perpendiculars > 75 degrees
  --      OUTPUT
  --      (xnear,ynear): Calculated nearest point on line.
  --          Returns the perpendicular distance in meters when the point
  --          (xin,yin) projects onto the given line or 1.E10 when it does not.
  --
-- Purpose:  Find whether a point projects onto aline, and if so, returns the distance.  
                          
-- Reference: Paul Bourke: "Minimum Distance between a point and a line".
--             http://local.wasp.uwa.edu.au/~pbourke/geometry/pointline/
********************************************************************************
*/                        
     u             NUMBER;
     distance      NUMBER := 1.E10;
     length_sq     NUMBER;
     len12         NUMBER;
     dx            NUMBER := x2-x1;
     dy            NUMBER := y2-y1;
     dxp           NUMBER;
     dyp           NUMBER;
     lenp          NUMBER;
     x             NUMBER;
     y             NUMBER;
     dot_perp      NUMBER:= 1.E10;
     dot_perp2     NUMBER:= 1.E10;

BEGIN
--     dbms_output.put_line('xin ' || xin || ' yin ' || yin);

     u := ((Xin - X1) * dx + (Yin-Y1) * dy);

--     dbms_output.put_line('U ' ||U || ' ' || (u/(dx*dx + dy*dy)));
--     dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
--     dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2);
     xnear := NULL;
     ynear := NULL;
           
     length_sq :=  dx*dx + dy*dy;
     If length_sq > 0.0 then
     
        u := u/length_sq;
        xnear := X1 + u * dx;
        ynear := Y1 + u * dy; 
        if (u >=0.0 and u <= 1.0) then

           if meters then
             Distance := distance_fcn(xin,yin,xnear,ynear,8265.);
           else   -- eof on communication channel ?? Added these 2 lines
             dx := ROUND(Xin-xnear,9);
             dy := ROUND(Yin-ynear,9);
             Distance := sqrt((dx*dx + dy*dy));
           end if;
           
        elsif perp_angle_tolerance > 0.0 then
         len12 := sqrt(length_sq);
         
         dxp := xin - xnear;
         dyp := yin - ynear;
         lenp := sqrt(dxp*dxp+dyp*dyp);
         
-- Work out cosine (the dot product)

         if lenp > 0.0 then
           dot_perp := abs(dx*dxp + dy*dyp) /(len12*lenp);
         end if;
         if u < 0. then
              x := x1;
              y := y1;
             
         else
            x := x2;
            y := y2;
              
          end if;
          dxp := xin-x;
          dyp := yin-y;
          lenp := sqrt(dxp*dxp+dyp*dyp);

          if lenp >0. then
            dot_perp2 := abs(dx*dxp + dy*dyp) /(len12*lenp);
          end if;
         
--         dbms_output.put_line('dotp ' || round(dot_perp2,6) || ' x1 ' || round(x1,6) ||',' ||round(y1,6) || 'x2 ' || round(x2,6) ||','||round(y2,6) || ' xin ' || round(xin,6) || ',' || round(yin,6));
         if dot_perp2 < perp_angle_tolerance then --and dot_perp2 < perp_angle_tolerance then
  
           if meters then
               Distance := distance_fcn(xin,yin,x,y,8265.);
           else   -- eof on communication channel ?? Added these 2 lines
               dx := ROUND(xin-x,9);
               dy := ROUND(yin-y,9);
               Distance := sqrt((dx*dx + dy*dy));
           end if;
           xnear := x; ynear := y;
         end if;
      end if;
--        dbms_output.put_line('distance ' || distance);
      elsif length_sq = 0. then  -- zero length line
           NULL;
      end if;
 
     RETURN Distance;

END Perpendicular;
--
FUNCTION POINT_IN_POLY( Xc NUMBER, Yc NUMBER,XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY)
                          RETURN PLS_INTEGER Deterministic IS
/*
********************************************************************************
--Program Name: point_in_poly (point in 2003 type polygon)
--Author: Sidey Timmins
--Creation Date: 7/1/08
--Updated: July 2013 to handle multiple outside rings
--Usage:
 -- This PL/SQL function has 4 parameters: 
 --             xc,yc:  the coordinates of the point to test
 --             XYs:    the array of xy coordinates for the polygon to be checked
 --             Info:   the Elem_Info array for the polygon to be checked
 
 --
-- Output: result: 1 point is inside polygon. 
--                 0: outside.
--                 -1 on boundary.

--Purpose: This function determines by discrete testing whether a point xc,yc 
--        is in the interior (1), on the boundary (-1) or outside a polygon (0).
--        Exhaustive testing means tests are made for each edge that the
--        test point falls within its y range.
-- 
-- Reference: - Exterior and interior rings cannot be nested. For example, if
--            a country has a lake and there is an island in the lake 
--            (and perhaps a lake on the island), a separate polygon must be 
--            defined for the island; the island cannot be defined as an interior 
--            polygon ring within the interior polygon ring of the lake. 

-- http://download.oracle.com/docs/html/B14255_01/sdo_objrelschema.htm#
--i1009746www.geometryalgorithms.com/Archive/algorithm_0103/algorithm_0103.htm
-- All code is original by Sidey Timmins
--
-- Testing: 6 times faster than SDO_GEOM.RELATE - finds an inside point in Lee
-- county (5856 times) in 12 seconds versus 73 seconds.
--Dependencies: Point_in_poly_cc

********************************************************************************
*/ 

    inside      PLS_INTEGER := 0;
    inside_this PLS_INTEGER;
    clockwise   NUMBER;

    rings       PLS_INTEGER;
    current_ring PLS_INTEGER :=1;
    urings      PLS_INTEGER :=0;
    ring_type   PLS_INTEGER;
    LB          PLS_INTEGER;
    UB          PLS_INTEGER;
    j           PLS_INTEGER;
 
-- x1 NUMBER;
-- y1 NUMBER;
-- x2 NUMBER;
-- y2 NUMBER;
-- x3 NUMBER;
-- y3 NUMBER;

BEGIN

 rings := TRUNC(Info_Array.count/3);
 For i in 1..rings Loop
   If Info_Array(i*3-1) = 1003 then
      urings := urings+1;
   End if;
 End Loop;
  
 
 For i in 1.. rings Loop

--===================================================================
/* Shoot a test ray along +X axis. The strategy is to compare vertex Y values
 * to the testing point's Y and quickly discard edges which are entirely to one
*/ 
   j := (i-1) *3 + 1;
   LB := Info_Array(j);
   ring_type := Info_Array(j+1);
   if ring_type = 1003 then
     current_ring := i;
   end if;
   
   If i = rings then
      UB := Xys.count;
   Else
      UB := Info_Array(j+3) -1;
   End If;
--dbms_output.put_line('Info ' || info_Array(j) || ' LB ' || LB || ' UB ' ||UB);

-- Ignore holes in current ring when it is not inside the outer ring

   if i > current_ring and inside = 0 then
      continue;
   end if;

   inside_this := Point_in_poly_cc(Xc,Yc,Xys,LB,UB);
--   dbms_output.put_line('inside_this' || inside_this);
   
   If i = current_ring and inside = 0 then
     inside := inside_this;
     if inside_this < 0 then
       inside := -1;
     End if;
-- We are done when we are outside the main ring or on the boundary
-- dbms_output.put_line('inside ' || inside);

   Else
-- Per the reference above, all further rings are holes.
-- The comments are just to show the logic if this were not so.
-- If we only have 2 rings then this ring is a hole
-- if rings = 2 then
     If inside_this = 1 then
        inside := 0;
        exit;
     Elsif inside_this < 0 then
        inside := -1;
        exit;
     End if;
 
-- Else
-- first determine if this ring is a hole or an island
-- x1 := XYs(LB);
-- y1 := XYs(LB+1);
-- j := TRUNC((LB+UB)/2) +1;
-- x2 := Xys(j);
-- y2 := Xys(j+1);
-- x3 := XYs(UB-3);
-- y3 := XYS(UB-2);
-- clockwise := orient2d(x1,y1,x2,y2,x3,y3);

-- Is it clockwise or anticlockwise?
-- if clockwise < 0 then
-- Its a hole. Save inside when its in the hole or on the boundary
-- If inside_this = 1 then
-- inside := 0;
-- Elsif inside_this = -1 then
-- inside := -1;
-- End if; 
-- Else
-- Its an Island in a hole. Save inside when its on an island or on the boundary
-- If inside_this = 1 then
-- inside := 1;
-- Elsif inside_this = -1 then
-- inside := -1;
-- End if;
-- End If;
-- We are done when we are on a boundary
-- Exit when inside = -1;
-- end If;
   End If;
-- This line belongs at the end of the "if i = 1 then" phrase if we
-- were to uncomment all the stuff above

 End Loop;

 RETURN Inside;

END POINT_IN_POLY;
--
FUNCTION POINT_IN_POLY_CC(Xc     NUMBER,
                           Yc     NUMBER,
                           XYs    IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                           pLB    NUMBER default 1,
                           pUB    NUMBER default 0
)
         RETURN         PLS_INTEGER Deterministic IS
/*
********************************************************************************
--Program Name: point_in_poly_cc (point in polygon counting crossings)
--Author: Sidey Timmins
--Updated: 04/29/2011 Test to see if on boundary for vertical or horizontal lines.
--Creation Date: 7/21/08

--Usage:
  -- This PL/SQL function has 5 parameters:
  --             xc,yc: the coordinates of the point to test
  --             XYs:   the array of xy coordinates for the polygon to check
  --             pLB:   the element to start at in the X and Y arrays
  --             pUB:   the element to end searching the X and Y arrays
  --
-- Output:       result: 1: point is inside polygon.
--                       0: outside.
--                      -n: on boundary. N is the element number in the ordinate
--                          array. This enable very quick tests for on the
--                          boundary because if a vertex of an edge is on the
--                          boundary at position n, then the next vertex to be
--                          tested may be at positions n-1 or n+1. Thus the caller
--                          may check this and avoid calling point_in_poly_cc.
--
--Purpose: This function determines by discrete testing whether a point xc,yc
--         is in the interior (1), on the boundary (-1) or outside a polygon (0).
--         Exhaustive testing means tests are made for each edge that the
--         test point falls within the edges y range. This code is not intended
--         for SRID 8265 which is non-planar.
--
-- Reference: -  www.geometryalgorithms.com/Archive/algorithm_0103/algorithm_0103.htm
--               All code is original by Sidey Timmins
--
-- Testing:  6 times faster than SDO_GEOM.RELATE - finds an inside point in Lee
--           county (5856 times) in 12 seconds versus 73 seconds.
--Dependencies: None

********************************************************************************
*/

  inside      PLS_INTEGER;
  xfound      NUMBER;
  XDiff       NUMBER;
  YDiff       NUMBER;
  Xci         NUMBER;
  Yci         NUMBER;
  r           NUMBER;
  cp          NUMBER;
  xlast       NUMBER;
  ylast       NUMBER;

  i           INTEGER;
  next        PLS_INTEGER := 0;
  pos         PLS_INTEGER := 0;
  LB          PLS_INTEGER := TRUNC((pLB+1)/2);
  UB          PLS_INTEGER := pUB;

BEGIN

  If UB = 0 then
    UB := XYs.count;
  End If;

--===================================================================
/* Shoot a test ray along +X axis.  The strategy is to compare vertex Y values to
** the testing point's Y and quickly discard edges which the ray does not overlap
*/

  inside := 0;
  xlast := XYs(UB-3);
  ylast := XYs(UB-2);

  i := pLB-1;

  UB := trunc(UB/2)-1;
  For ii in LB..UB LOOP
     i := i + 2;
--     dbms_output.put_line('ii ' || ii || ' i ' || i);
     IF (yc < ylast and ylast < XYs(i)) or
        (yc > ylast and ylast > Xys(i)) then
         ylast := XYs(i);
     ELSE
-- test a horizontal ray
     If ii <> LB then
       xlast := XYs(i-3);
     End If;

     if yc = ylast and yc = XYs(i) then
         cp := 0;
         Xdiff := XYs(i-1) - xlast;
         Ydiff := XYs(i) - ylast;
         Xci   := xc - xlast;
         Yci   := yc - ylast;
      ElsIf (yc >= ylast and yc < XYs(i)) or
       (ylast > yc and yc >= XYs(i)) or
       (yc = ylast and xc = xlast) then
         Xdiff := XYs(i-1) - xlast;
         Xci   := xc - XYs(i-1);
         Ydiff := XYs(i) - ylast;
         Yci   := yc - XYs(i);

 -- Calculate the cross product (2 * area of ABC)
         cp := Xci * YDiff - Yci * XDiff  ;

 -- Check the distance on PC
          Xci   := xc - xlast;
          Yci   := yc - ylast;
          next := next + 1;
          xfound := xlast + Xdiff * yci/Ydiff;

          If xc > xfound or (ylast <> yc and yc = Xys(i) and xc = XYs(i-1)) then
            next := next -1;
          End If;

        End if;

-- Calculate the Line parameter on AB
--
--                         + C
--
--          A +------------.-------+ B
--                         P

-- Make an extra check for boundary by checking the parametric equation
-- of each line. CP is the normal from C to the polygon edge AB.
-- line parameter r = (AC dot AB)/AB^2
--
--      r=0      P = A
--      r=1      P = B
--      r<0      P is on the backward extension of AB
--      r>1      P is on the forward extension of AB
--      0<r<1    P is interior to AB

       If cp = 0.0 and (Xdiff <> 0. or YDiff <> 0.) then
           r := (Xci* Xdiff + Yci * Ydiff)/
                (XDiff*Xdiff + YDiff*YDiff);
-- Its on the boundary
            If (r >= 0.0 and r <= 1.0) Then
-- may need more testing for next 3 lines.
               if i > 2 then
                 i := i-2;
               end if;
               Return (-i+1);
            End if;

      End If;

      xlast := XYs(i-1);
      ylast := XYs(i);
      END IF;
  End Loop;

   Inside := mod(next,2);

   RETURN Inside;


END POINT_IN_POLY_CC;
--
FUNCTION POLYGON_AREA(Geom MDSYS.SDO_GEOMETRY) RETURN MDSYS.SDO_LIST_TYPE AS

-- Returns the Areas of all rings in a polygon. Result is in meters or input units

  XYs        MDSYS.SDO_ORDINATE_ARRAY := Geom.SDO_ORDINATES;
  XYOrd      MDSYS.SDO_ORDINATE_ARRAY;
  Info       MDSYS.SDO_ELEM_INFO_ARRAY := Geom.SDO_ELEM_INFO;
  Area       Number;
  Areas      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Gtype      NUMBER := Geom.SDO_GTYPE;

BEGIN

-- Accept a closed line string

  If Gtype = 2002 then
    if XyOrd(1) = XyOrd(XyOrd.count-1) and XyOrd(2) = XyOrd(XyOrd.count) then
       Gtype := 2003;
    end if;
  End if;
  
  if Gtype = 2003 or Gtype = 2007 then
-- Handle optimized rectangle

      If Info(3) = 3 then  
       XYOrd := Xys;
       Xys:= MDSYS.SDO_ORDINATE_ARRAY(XYOrd(1),XYOrd(2),XyOrd(3),XYOrd(2),
                                    XYOrd(3),XYOrd(4),XyOrd(1),XYOrd(4),XYOrd(1),XYOrd(2));  
      End if;
      
     Area := Polygon_area(Xys,Info,Areas,Geom.sdo_SRID);
     Return Areas;
  end if;
  
  Return NULL;
END POLYGON_AREA;
--
Function Remove_rings(Geom MDSYS.SDO_GEOMETRY,Area_threshold NUMBER, Less_than VARCHAR2 default 'TRUE') RETURN  MDSYS.SDO_GEOMETRY AS

-- remove rings that fall below an Area threshold
-- Written to remove chaff (ridiculous small polygons) that find_get_pipes
-- had to deal with.
-- Maybe we should use a perimeter/area  threshold

  New_Geom      MDSYS.SDO_GEOMETRY;
  XYs        MDSYS.SDO_ORDINATE_ARRAY := Geom.SDO_ORDINATES;
  Info       MDSYS.SDO_ELEM_INFO_ARRAY := Geom.SDO_ELEM_INFO;
  Area       Number;
  GType      NUMBER := 2003.;
  Areas      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  ring_type  PLS_INTEGER;
  istart     PLS_INTEGER;
  iend       PLS_INTEGER;
  rings      PLS_INTEGER := 0;
  pos        PLS_INTEGER;
  loup       PLS_INTEGER;
BEGIN
   
-- Get the areas and then flag the small rings 
   Area := Polygon_area(Xys,Info,Areas,Geom.sdo_SRID);
   
   For ii in 2..Areas.count Loop  -- 1st area is Total
      If (Less_than = 'TRUE' and ABS(areas(ii)) <  Area_threshold) or
         (Less_than = 'FALSE' and ABS(areas(ii)) >  Area_threshold) then
        Info(ii*3-4) := -Info(ii*3-4); -- Just flag the Info ring type
        rings := rings - 1;
      End if;
   End Loop;
   
   if rings = 0 then   -- If there were no ring changes, geometry is unchanged
     return Geom;
   end if;

-- Now compress the geoemtry removing the flagged rings and updating the geometry
   
   For ii in 1..TRUNC(Info.count/3) loop
     ring_type := Info(ii*3-1);
     istart := Info(ii*3-2);
     if ii <> TRUNC(Info.count/3) then
        iend := Info(ii*3+1)-1;
     else
        iend := Xys.count;
     end if;

--   if rings is negative, then we have not compressed the Xys yet.

     if ring_type < 0 then  -- Skip this ring
        NULL;
     else
       if rings < 0 then  -- keep this ring
        rings := 0;
        pos := 1;
       end if; 
        loup := 0;
                   -- copy these Xys backwards (sometimes)
        For ii in istart..iend loop
           Xys(pos+ii-istart) := Xys(ii);
           loup := loup +1;
        End Loop;
        rings:= rings+1;
        Info(rings*3-2) := pos;
        Info(rings*3-1) := ABS(ring_type);
        if rings <> 1 and ABS(ring_type) = 1003 then
          GType := 2007;
        end if;
        pos := pos + iend - istart+1;
     end if;
   End Loop;
   
   Xys.trim(Xys.count-pos+1);
   Info.trim(Info.count-rings*3);
   Dbms_output.put_line('Rings now ' || rings);
   
   New_Geom := MDSYS.SDO_GEOMETRY(Gtype,Geom.SDO_SRID,NULL,Info,Xys);

   Return New_Geom;
   
END REMOVE_RINGS;
--
FUNCTION POLYGON_AREA( XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Info MDSYS.SDO_ELEM_INFO_ARRAY, Areas IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,SRID NUMBER default 8265., Positive BOOLEAN default TRUE) RETURN NUMBER AS
/*
********************************************************************************
--Program Name: Polygon_Area
--Author: Sidey Timmins
--Creation Date: 8/04/2008
--Updated:  07/29/2013 Converted to use Ring_area
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
  --      SRID         -  the spatial reference Id
  --      OUTPUT
  --      Areas        - Array to receive the areas.
--
-- Purpose: Find a centroid of a single closed polygon and return the area(s) of
--         the polygon(s) or a non_self_intersecting edge. Area values are in
--         square input units. Accepts redundant coordinates.

-- Reference: DH Maling, "Measurements from Maps"
--            http://en.wikipedia.org/wiki/Centroid
********************************************************************************
*/

   Area        NUMBER := 0.0;
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
              areas(next) := (area - last_area);
              last_area := area;
 
          
          dbms_output.put_line('Area ' || Lpadv(round(delta,3),20) || ' ring type ' || ring_type);
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
         areas(next) := (area - last_area)*0.5;

         last_area := area;
        if loops= n then
             Area := area*0.5;
        end if;

        END IF;
        END LOOP;
 
        areas.trim(areas.count-next);
  
-- Store the total area.
        if positive then
          areas(1) := abs(Area);
        else
          areas(1) := Area;
        end if;
  RETURN Area;

END POLYGON_AREA;
--
FUNCTION RING_AREA(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER DEFAULT 8265.,pstart PLS_INTEGER default 1, pend PLS_INTEGER default 0)
           RETURN NUMBER DETERMINISTIC IS

--------------------------------------------------------------------------------
--Program Name: ring_area
--Author: Sidey Timmins
--Creation Date: 7/26/2013

--Usage:  Xys := Geometry.sdo_ordinates;
--        Area (in meters ) := Ring_Area(Xys);

  --  Parameters:
  --      Xys           - the coordinates of a single ring
  --      SRID          - The Oracle spatial reference id (projection)
  --
-- Purpose: Computes area on a planar earth or the ellipsoid. For the ellipsoid
-- it calls Charles Karney's Geographic Lib and the accuracy surpasses sdo_area!!!
-- Testing: Excellent but did not compute entire hemisphere with this:
-- ring_Xys := mdsys.sdo_ordinate_array(-180,0,180,0,180,90,-180,0);
--------------------------------------------------------------------------------

  pi     constant NUMBER      := 3.1415926535897932384626433832795028842;
  n            PLS_INTEGER;
  next        PLS_INTEGER := 2;
  ii           PLS_INTEGER := pstart;
  iend         PLS_INTEGER := pend;
  kount        PLS_INTEGER;
  cross_       PLS_INTEGER :=0;
  
  sph_tri_area NUMBER :=0.0;
  x1           NUMBER;
  y1           NUMBER;
  x2           NUMBER;
  y2           NUMBER;
  x3           NUMBER;
  y3           NUMBER;
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
  d_K          number;  -- the geodesic distance computed by Charles Karney's GeographicLib
 
-- Authalic radius for GRS1980 
  AR           NUMBER := 6371007.18088351429821304500954888028409;
  f            NUMBER := 298.2572221008827112431628366;
  AR_Sq        NUMBER := 40589732498869.30427587653936614505912995;
    
  
  Area         NUMBER := 0.0;
  
  Function AngNormalize(x NUMBER) RETURN NUMBER  AS
  
--  Place angle in [-180, 180).  Assumes x is in [-540, 540).

  Begin
--       return x >= 180 ? x - 360 : x < -180 ? x + 360 : x;
    If x >= 180. then
       RETURN (x-360.);
    Elsif x < -180. then
       RETURN (x+360.);
    Else
       RETURN x;
    End if;

  End AngNormalize;
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
  
--  Compute y - x.  x and y must both lie in [-180, 180].  The result is
-- equivalent to computing the difference exactly, reducing it to (-180,
-- 180] and rounding the result.  Note that this prescription allows -180
-- to be returned (e.g., if x is tiny and negative and y = 180).

     d      number;
     t      number;
  Begin
  
     d := sumx(-x,y,t);
     if d - 180.0 + t > 0.0 then
       d := d - 360.0;
     elsif d+180.0 + t <= 0.0 then
       d := d + 360.0;
     end if;
     return d + t;
  End;
  --
  Function TRANSIT(lon1 NUMBER, lon2 NUMBER)
                 RETURN PLS_INTEGER AS

  lon1x      NUMBER;
  lon2x      NUMBER;
  lon12      NUMBER;
  trnsit     PLS_INTEGER := 0;
  Begin
  
    lon1x :=  AngNormalize(lon1);
    lon2x :=  AngNormalize(lon2);
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
    
-- Use GZ_Geodesic to calculate the areas extremely accurately...

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
  
        s12 := GZ_GEODESIC.inverse(y1,x1,y2,x2,a1,az1,az2,mm12,m12,m21,SS12,TRUE);
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
    
    area0 := 4. *pi * AR_sq;
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

END RING_AREA;
--
FUNCTION QUICK_RING_AREA(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER DEFAULT 8265.,pstart PLS_INTEGER default 1, pend PLS_INTEGER default 0)
           RETURN NUMBER DETERMINISTIC IS

--------------------------------------------------------------------------------
--Program Name: quick_ring_area
--Author: Sidey Timmins
--Creation Date: 7/25/2011

--Usage:  Xys := Geometry.sdo_ordinates;
--        Area (in meters ) := Polygon_Area(Xys);

  --  Parameters:
  --      Xys           - the coordinates of a single closed polygon
  --      SRID          - The Oracle spatial reference id (projection)
  --
-- Purpose: Computes area for a single ring on a planar earth or the ellipsoid. 
-- It uses an exact area formula for planar SRIDs or a spherical triangle 
-- approximation for the ellipsoid (L'Huilier's Theorem). See ring_area for very
-- accurate area computation (more accurate than sdo_geom.sdo_area).
--
-- Reference: Gerard Michon: Spheroids & Scalene Ellipsoids
--     http://www.numericana.com/answer/ellipsoid.htm
--------------------------------------------------------------------------------

  deg2rad     NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
  n            PLS_INTEGER;
  next         PLS_INTEGER := 2;
  ii           PLS_INTEGER := pstart;
  iend         PLS_INTEGER := pend;
  kount        PLS_INTEGER;
  
  sph_tri_area NUMBER :=0.0;
  x1           NUMBER;
  y1           NUMBER;
  x2           NUMBER;
  y2           NUMBER;
  x3           NUMBER;
  y3           NUMBER;
  a            NUMBER;
  b            NUMBER;
  c            NUMBER;
  s            NUMBER;
  az1          number;
  mm12         number;
  m12          number;
  m21          number;
  az2          number;
  sig          number;
  are          number;
  sinphi       number;
  ff           number;    
  f1           NUMBER := 0.9966471893188163625818349538152355351343;
                           
  e            NUMBER := 0.0818191910428318507068859991008242525399;
                           
  e2           NUMBER := 0.006694380022903415749574948586289306212847;
  
--              R=  6378137. radius squared
  R_Sq          NUMBER := 40680631590769.; --40589732742451.84;
  R             NUMBER := 6378137.; --6371007.2;
  BB2           NUMBER := 40408299983328.76931725432420763849393114 *0.5;
  
--  R_Sq          NUMBER := 40589732498869.30427587653936614505912989;
--  R             NUMBER := 6371007.18088351429821304500954888028409; 
    
  -- Calculate factor = 1./R 
  factor       NUMBER := 0.0000001567855942887397997252175674495546270016;
  Area         NUMBER := 0.0;
  
  ok           BOOLEAN;

  Function ORIENT2D(paX NUMBER, paY NUMBER,
                  pbX NUMBER, pbY NUMBER,
                  pcX NUMBER, pcY NUMBER)  RETURN NUMBER AS

/*
********************************************************************************
--Program Name: c_orient2D
--Author: Sidey Timmins
--Creation Date: 8/15/2006

--Usage:  result := orient2D(three points: x1,y1  x2,y2   x3,y3);
  --
  --   REQUIRED Parameters:
  --      paX,paY           - a point A, (x,y)
  --      pbX,pbY           - a 2nd point B, (x,y)
  --      pcX,pcY           - a 3rd point C, (x,y)
-- Purpose:
  -- Returns a positive value if the points A, B, and C occur
  --             in counterclockwise order; a negative value if they occur
  --             in clockwise order; and zero if they are collinear.
********************************************************************************
*/
   det          NUMBER;
  Begin
    det   := (paX - pcX) * (pbY - pcY) - (paY - pcY) * (pbX - pcX);
 --   det   := (paX - pcX) * (pbY - pcY) - (paY - pcY) * (pbX - pcX);
    Return det;
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
    
--    b := gz_super.fast_vincenty_gcd(x3,y3,x2,y2)*factor;
    sig := GZ_GEODESIC.inverse(y3,x3,y2,x2,b,az1,az2,mm12,m12,m21,are);
    b := b*factor;
--  Do the triangles consecutively: 1,2,4 and then 2,3,4 and so on..
--
--        +  4
--       /  \   -
--      /    \      -
--     /      \          +  3
--    /         \     .
--   +-----------+ .
--   1           2
/*
    x3 := Xys(pstart);
    y3 := Xys(pstart+1);
    x2 := Xys(iend-1);
    y2 := Xys(iend);
    dbms_output.put_line('x3 ' || x3 || ' y3 ' || y3 || ' x2 ' || x2 || ' y2 ' || y2|| 'iend ' || iend);

    next := pstart+1;
    While next <= iend Loop   -- Girard Michon's method
        x1 := x2;
        y1 := y2;
        x2 := x3;
        y2 := y3;
        next := next + 2;
        x3 := Xys(next-1);
        y3 := Xys(next);
  
        
        dbms_output.put_line('x1 ' || x1 || ' x3 ' || x3 || ' y2 ' || y2);
        sinphi := sin(y2*deg2rad);
        ff := bb2*(gz_GEODESIC.atanh(e*sinphi)/e + sinphi/(1.-e2*sinphi*sinphi));
        area := area - ff*(x3-x1)*deg2rad*0.5;
*/       

    next := pstart+1;
    While next <= iend-4 Loop   
        x1 := x2;
        y1 := y2;
        next := next + 2;
        x2 := Xys(next-1);
        y2 := Xys(next);
   
        c := b;
        ok := TRUE;
        if abs(x2-x1) > 0.02 then
          ok := FALSE;
        end if;
        if abs(y2-y1) > 0.02 then
          ok := FALSE;
        end if;
        if abs(x2-x3) > 0.02 then
          ok := FALSE;
        end if;
        if abs(y2-y3) > 0.02 then
          ok := FALSE;
        end if;
       
        if ok then
        a := GZ_super.fast_vincenty_gcd(x1,y1,x2,y2)*factor;
  
        b := GZ_super.fast_vincenty_gcd(x2,y2,x3,y3)*factor;
 
        s := 0.5*(a+b+c);
        end if;

        If ok = FALSE or (s-a) < 0. or (s-b) < 0. or (s-c) < 0. Then
          sig := GZ_GEODESIC.inverse(y1,x1,y2,x2,a,az1,az2,mm12,m12,m21,are);
          a := a*factor;
          sig := GZ_GEODESIC.inverse(y3,x3,y2,x2,b,az1,az2,mm12,m12,m21,are);
          b := b*factor;
          sig := GZ_GEODESIC.inverse(y3,x3,y1,x1,c,az1,az2,mm12,m12,m21,are);
          c := c*factor;
          
          s := 0.5*(a+b+c);
        end if;
        If (s-a) > 0. and (s-b) > 0. and (s-c) > 0. Then
          sph_tri_area := gz_math.new_arctan(sqrt(tan(s*0.5)*tan((s-a)*0.5)*tan((s-b)*0.5)*tan((s-c)*0.5)),1.);
         
          if Orient2d(x1,y1,x2,y2,x3,y3) < 0.0 then
            Area := Area - sph_tri_area;
          else
            Area := Area + sph_tri_area;
          end if;
        else
         dbms_output.put_line('problem in ring_area'|| next);
         dbms_output.put_line('s-a ' || (s-a) || ' s-b ' || (s-b) || ' s-c ' || (s-c));
         dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1 || ' x2 ' || x2 || ' y2 ' || y2);
         dbms_output.put_line('x3 ' || x3 || ' y3 ' || y3);
        End If;

    End Loop; 

    Area := Area * 4. * R_sq; 
    RETURN Area;

END QUICK_RING_AREA;
--
Function QUICK_SINCOS(Xin NUMBER,cosX IN OUT NOCOPY NUMBER,in_degrees VARCHAR2 default 'N') RETURN NUMBER Deterministic IS

--******************************************************************************
--Program Name: quick_sincos
--Author: Sidey Timmins 
--Creation Date: 07/16/13 New Coefficients
--Updated: 03/14/2013 Updated degrees to work when negative and > 360.
--         
--Usage:  result := quick_sincos(30.,cosx,'Y');
--
  -- This PL/SQL function has 2 required parameters:
  --
  --          Input
  --             x:    Input argument in radians or if in_degrees is set to 'Y'
  --                   then input argument in degrees.
  --            in_degrees: 'Y' means input argument is in degrees.
  --          Output
  --          cosx:  contains approximate cosine(x) on exit.
-- Output:    returns:  approximate sine(x).

--Purpose: This function calculates the sine and cosine quickly and probably
--         accurately enough for most GIS purposes (< 1 millionth of a degree).
--         Speed; 2.5 times speed of sinx := sin(x) and cosx = sqrt(1-sinx*sinx)
--                3.0E-06 secs per value.
--         Max absolute error is <= 2.5E-12.
--         Maximum relative error is 5E-12 (1 - quick_sin/sine);
--         It is instructive to compare the accuracy of this function with:
--         "Fast Polynomial Approximations to Sine and Cosine", Charles K Garrett
--         Feb 17, 2012. http://www.krisgarrett.net/papers/l2approx.pdf
--******************************************************************************

   deg2rad   CONSTANT NUMBER :=0.0174532925199432957692369076848861271344;  -- pi/180.
   twopi     CONSTANT NUMBER := 6.2831853071795864769252867665590057684;
   pi        CONSTANT NUMBER := 3.1415926535897932384626433832795028842;
   piby2     CONSTANT NUMBER := 1.5707963267948966192313216916397514421;
   piby4     CONSTANT NUMBER := 0.78539816339744830961566084581987572105;
   pi3by2    CONSTANT NUMBER := 4.7123889803846898576939650749192543263;

   x         NUMBER := Xin;
   xx        NUMBER;
   x2        NUMBER;
   sinx      NUMBER;
   swap      NUMBER := 0.0;
Begin

-- Adjust to range -twopi to +twopi
   IF x > twopi or -x > twopi THEN
     if in_degrees = 'Y' then
       xx := Mod(Xin,360.);
       if xx = 180. or xx = -180. then
          cosx := 1.;
          RETURN 0.0;
       else
          xx := xx*deg2rad;
       end if;
     else
      xx := Mod(Xin,twopi);
     end if;
 
   ELSE
     if in_degrees = 'Y' then
       xx := Mod(Xin,360.);
       if xx = 180. or xx = -180. then
          cosx := 1.;
          RETURN 0.0;
       else
          xx := xx*deg2rad;
       end if;
     else
       xx := Xin;
     end if;
 
   END IF;
 
   x := xx;
   if x < 0. then
     x := -x;
   end if;

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
 
-- Calculate an accurate approximation to the sin: max error of 2.5E-12 radians
   x2 := x*x;
-- These coefficients calculated by SAMIN. The first coefficient was just a guess!
--SAMIN results
--==> Normal convergence <==
--Convergence tolerances: Function: .0000000000001 Parameters: .0000000002
--Objective function value at minimum:   3.6169956E-12
--           parameter        search width
--.        0.1666666662      0
--.        0.0083333278      0
--.        0.0001983895      0
--.        0.0000027154      0
--================================================
--I 1 Coeff .1666666661915963153
--I 2 Coeff .00833332777350720435
--I 3 Coeff .00019838949158372156
--I 4 Coeff .00000271535784242563

   sinx := x*(0.999999999995+x2*(-0.16666666619159631530 + x2*(0.00833332777350720435 +x2*(-0.00019838949158372156 + x2*0.00000271535784242563))));

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
FUNCTION GET_MBR_GEOM(Geom IN MDSYS.SDO_GEOMETRY,max_width NUMBER) RETURN MDSYS.SDO_GEOMETRY AS

-- Return rectangles around teh geometry with a maximum overlap of max_width.
    MBRs            MBRs_Type;
    MBR_Geom        MDSYS.SDO_GEOMETRY;
    Xyord           MDSYS.SDO_ORDINATE_ARRAY  := Geom.sdo_ordinates;
    Info_array      MDSYS.SDO_ELEM_INFO_ARRAY := Geom.sdo_elem_info;
    Xys             MDSYS.SDO_ORDINATE_ARRAY  := MDSYS.SDO_ORDINATE_ARRAY();
    Info            MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY();
    
    xybuffer        NUMBER;
    ybuffer         NUMBER;
    xLL             NUMBER;
    yLL             NUMBER;
    xUR             NUMBER;
    yUR             NUMBER;
    n               PLS_INTEGER;
BEGIN

   xybuffer := round(meters_to_degrees('X',max_width,ABS(XYOrd(2))),8);
   ybuffer := round(meters_to_degrees('Y',max_width,ABS(XYOrd(2))),8);


dbms_output.put_line('x ' || xybuffer || ' Y ' || ybuffer);
   SET_MANY_MBRs(XYord,Info_Array,MBRs,xLL,yLL,xUR,yUR,xybuffer);
    
   n := MBrs.count-1;
   Info.extend(n*3);
   Xys.extend(n*4);
   
   -- Make a set of optimized rectangles describing each MBR extent
   
   For ii in 1..n Loop
      Info(ii*3-2) := (ii-1)*4+1;
      Info(ii*3-1) := 1003;
      Info(ii*3) := 3;
      Xys(ii*4-3) := MBRs(ii+1).xLL-xybuffer;
      Xys(ii*4-2) := MBRs(ii+1).yLL-ybuffer;
      Xys(ii*4-1) := MBRs(ii+1).xUR+xybuffer;
      Xys(ii*4)   := MBRs(ii+1).yUR+ybuffer;
   End Loop;
   
   MBR_Geom := MDSYS.SDO_GEOMETRY(2007,Geom.SDO_SRID,NULL,Info,Xys);
   Return MBR_Geom;
   
END;
--

function LINE_OVERLAP(x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,xLL NUMBER,yLL NUMBER,xUR NUMBER,yUR NUMBER) RETURN NUMBER AS

-- Using Cohen-Sutherland Clipping outcodes, determine if a line overlaps a rectangle.
-- Taken from Wikipedia

     left_    pls_integer :=1;
     right_   pls_integer :=2;
     bottom_  pls_integer :=4;
     top_     pls_integer :=8;
  
     code1    pls_integer :=0;
     code2    pls_integer :=0;
   
   begin

    -- Compute two outcodes
      
      if x1 < xLL then
          code1 := code1 + left_;
      elsif x1 > xUR then
          code1 := code1 + right_;
      end if;
      
      if y1 < yLL then
          code1 := code1 + bottom_;
      elsif y1 > yUR then
          code1 := code1 + top_;
      end if;
      
      if x2 < xLL then
          code2 := code2 + left_;
      elsif x2 > xUR then
          code2 := code2 + right_;
      end if;
      
      
      if y2 < yLL then
          code2 := code2 + bottom_;
      elsif y2 > yUR then
          code2 := code2 + top_;
      end if;
    -- Oracle has an AND function that works on bits
    
      if bitand(code1,code2) <> 0 then
        return 0;   -- no Overlap
      end if;
      
      return 1;  -- Overlap possible
  
   end Line_overlap;
--     
FUNCTION QUIZ_MBR(seg PLS_INTEGER, MBRs IN OUT NOCOPY MBRS_TYPE) RETURN MDSYS.SDO_LIST_TYPE AS

-- For a given segment, this function returns a list of pairs in a particular MBR
-- describing segments that may interact with the specified segment.

-- This example shows 4 ranges: the range that segment 1 falls in (1-38) and 3  
--                              others, 37-74, 163-182, 667-686
-- 1-38 means segments 1 thru 18, so quiz_mbr does not need to be called again
-- until the caller reaches segment 19.

--GZ_TOPOFIX.TRY_SET_MANY_MBR(1,SDOGEOMETRY)
-------------------------------------------------
--SDO_LIST_TYPE(1, 38, 37, 74, 163, 182, 667, 686)

  list_pairs       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

  jj               PLS_INTEGER;
  no               PLS_INTEGER :=0;
  lo               PLS_INTEGER;
  hi               PLS_INTEGER;
  pos              PLS_INTEGER; -- :=0;
  coord            PLS_INTEGER := seg*2-1;
  next            PLS_INTEGER :=2;

  
BEGIN
-- the number of MBRs (rectangles) is actually 1 less, since 1st is an overall
  -- figure out which segment range the seg falls within
  
  For ii in 2..MBRs.count loop 
      if coord >= MBRs(ii).lo and coord <= MBRs(ii).hi then
         pos := ii;
         if (coord+1) <> MBRs(ii).hi or ii = MBRs.count then
         exit;
         end if;
      end if;
  End Loop;
  
--  if pos is NULL  then
--  dbms_output.put_line('coord ' || coord);
--  For ii in 2..MBRs.count loop 
--      dbms_output.put_line('Lo ' || MBRs(ii).lo || ' hi ' || MBRs(ii).hi);
--  End Loop;
--  end if;

-- Build a list of pairs, the start-stop for the segment and other ranges
-- that that start-stop range interacts with.
  
-- If the start of the range is negative, then the segments walk away from each 
-- other without interaction 

  no := 1+MBRs(pos).mbr_list.count; 

    list_pairs.extend(no*2);
    
-- Always return at least 1 pair, the range of segments that seg falls within

    list_pairs(1) := MBRs(pos).lo;
    list_pairs(2) := MBRs(pos).hi;

    if TRUNC(list_pairs(2)) <> list_pairs(2) then  -- end of ring
       list_pairs(2) := list_pairs(2) -1;
    end if;
    
    if MBRs(pos).start_overlap = -1. then
      list_pairs(2) := -list_pairs(2);  -- Signal walk away
    elsif no = 1 and MBRs(pos).start_overlap  <> 0. then
      list_pairs(1) := MBRs(pos).start_overlap; -- Signal partial walk away up to this point
    end if;

-- Look ahead only to get overlaps

    for kk in 1..MBRs(pos).mbr_list.count loop
      jj := MBRs(pos).mbr_list(kk);
      lo := MBRs(jj).lo;
      hi := MBRs(jj).hi;
      if hi <> MBRs(jj).hi then   -- end of ring
        hi := hi-1;
      end if;
      next := next + 2;
      if next > list_pairs.count then list_pairs.extend(2); end if;
        list_pairs(next-1) := lo;
        list_pairs(next) := hi;
    end loop;
 
  RETURN list_pairs;
  
END QUIZ_MBR;
--
FUNCTION SET_GEOM_MANY_MBR(Geom IN MDSYS.SDO_GEOMETRY)
                      RETURN MDSYS.SDO_LIST_TYPE IS

--******************************************************************************
--Program Name: Set_geom_many_mbr
--Author: Sidey Timmins
--Creation Date: 06/25/2013
--Updated: 12/02/2008 To handle holes in polygons
--Usage:  MBR_List := Set_Geom_Many_MBR(Geoemtry);
  -- 
  --             Geom:      the input geometry            
  --
  -- Output:   MBR_List:   an output array filled with groups of 6 numbers:
  --                     xLL,yLL,xUR,yUR followed by the lower and upper bounds
  --                     for this MBR (start/stop elements)
  --  The very first group contains the overall MBR or extent:
  --                     xLL,yLL,xUR,yUR  (Lower left, Upper right).
--
--                ---------+ (xUR,yUR)
--                |        |
--                |        |
--      (xLL,yLL) +--------+   
--                       and the number of MBRs in position 5, and the number of
--                       coordinates for each MBR (except possibly the last).
--
--Purpose: 
--          Takes a geometry and produces many MBRs (Minimum Bounding 
--          Rectangles or rectangles) along the geometry and the overall 
--          extent (xLL,yLL) to (xUR,yUR). 
--          These MBRS enable a geometry to be subdivided and tested quickly for
--          overlap with another geometry.
--                            ____________
--                           |           |          Example: House shaped polygon
--                          /             \                 
--                        /                 \              
--                       |___________________|
--                           ____________
--                       ____|___________|____     
--                       |   |           |   |       might have these 5 MBRs
--                       |___|___________|___|       placed around it's perimeter
--                       |________|__________|
--
--         This function determines many touching MBRs around the perimeter of
--         a geometry. These MBRs can be used to very efficiently limit xy searches.
--         If the LB is not equal to 1 then the Info Array is ignored and MBRs 
--         are determined using pLB and pUB.
                      
--Dependencies: SET_MBR
--******************************************************************************
     Xys             MDSYS.SDO_ORDINATE_ARRAY := Geom.sdo_ordinates;
     Info_array      MDSYS.SDO_ELEM_INFO_ARRAY := Geom.sdo_elem_info;
     MBRs            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
     xLL             NUMBER;
     yLL             NUMBER;
     xUR             NUMBER;
     yUR             NUMBER;
     xLL2            NUMBER;
     yLL2            NUMBER;
     xUR2            NUMBER;
     yUR2            NUMBER;
     endpoints       PLS_INTEGER;
     LB              PLS_INTEGER := 1;
     UB              PLS_INTEGER := 0;
     InUB            PLS_INTEGER;
     LBB             PLS_INTEGER;
     UBB             PLS_INTEGER;
     m               PLS_INTEGER;
     mm              PLS_INTEGER;
     no_to_do        PLS_INTEGER;
     no_holes        PLS_INTEGER := 0;
     jj              PLS_INTEGER;
     len_required    PLS_INTEGER;
     next            PLS_INTEGER := 1;

BEGIN
 
     if UB <= 0 then UB := XYs.count; end If;
     InUB := UB;
     If LB = 1 then
       no_holes := TRUNC(Info_Array.count/3) -1;
     End if;
     
      If no_holes > 0 then
         next := 4;
         UB := Info_Array(4) -1;
      End if;
      
--  New 2/13/2013  next 4 lines         
      if InUB-LB+1 <= 10 then
        m := 1;
      elsif  InUB-LB+1 <= 10 then
        m := 1;
      else
        m := TRUNC(sqrt((InUB-LB+1))/2);   -- 06/25/2013 divide by 2 for fewer MBRs
      end if;
 
        
-- To get MBRs rectangles touching we have to reuse the last vertex as the
-- next start vertex (hence +2 below).

      no_to_do := TRUNC((InUB-LB+1)/m);
--  dbms_output.put_line('noto do' || no_to_do || ' inub ' ||inub || ' lb ' || LB);
      if TRUNC(no_to_do/2)*2 <> no_to_do then
        no_to_do := no_to_do +1;
      End if;

      if no_to_do = 2 then
        m := 1;
      else
        if m*(no_to_do-2)+2 < InUB-LB+1 then 
          no_to_do := no_to_do + 2;
        end if;          
        If (m* (no_to_do-2)) +2 < (InUB-LB+1) then  
          no_to_do := no_to_do + 2; 
        End If;
      end if;

      mm := m + no_holes;

      len_required := mm*6 +6;
      If (MBRs.count) < len_required then
        If len_required < 1000 then len_required := 1000; End if;
        MBRs.extend(len_required);
      End If;
      LBB := LB;
--         dbms_output.put_line('MM ' || mm || ' holes ' || no_holes || ' no to do ' || no_to_do || ' count ' || Xys.count || 'inub ' || InUB);

      For ii in 1..mm Loop
        UBB := LBB + no_to_do-1;
        If UBB > UB then UBB := UB; End if;

        endpoints := Set_MBR(XYs,xLL2,yLL2,xUR2,yUR2,LBB,UBB);

--          dbms_output.put_line('II ' || ii || ' LBB ' || LBB || ' UBB ' || UBB || ' count ' ||Xys.count || ' UB ' || UB);
--          dbms_output.put_line('II ' || ii || ' xLL ' || XLL2 || ' yLL ' || YLL2);
--          dbms_output.put_line('II ' || ii || ' xUR ' || XUR2 || ' yLL ' || YUR2);
        jj := ii*6;
        MBRs(jj+1) := xLL2;
        MBRs(jj+2) := yLL2;
        MBRs(jj+3) := xUR2;
        MBRs(jj+4) := yUR2;
        MBRs(jj+5) := LBB;
        MBRs(jj+6) := UBB;
-- Work out overall MBR 
        if ii = 1 or xLL2 < xLL then xLL := xLL2; end if;
        if ii = 1 or xUR2 > xUR then xUR := xUR2; end If;
        if ii = 1 or yLL2 < yLL then yLL := yLL2; end if;
        if ii = 1 or yUR2 > yUR then yUR := yUR2; end If;
        m := ii;
-- handle holes          
        If no_holes > 0 and UBB = UB then
           next := next + 3; 
           LBB := UB + 1;
           if next < Info_Array.count then
             UB := Info_Array(next)-1;
           else
             exit when UB = Xys.count;              
             UB := XYs.count;
           end if;
           exit when LBB >= InUB;
        Else  
          LBB := UBB-1;
        End if;

      End Loop;
      
      MBRs(1) := xLL;        -- Overall extent of this data, Lower Left
      MBRs(2) := yLL;
      MBRs(3) := xUR;        -- Upper Right
      MBRs(4) := yUR;
      MBRS(5) := m;          -- the number of MBRs
      MBRS(6) := no_to_do;   -- the number of coordinates for each MBR (except last)
      MBRs.trim(MBRs.count - (m+1)*6);
 
      RETURN MBRS;
      
END SET_GEOM_MANY_MBR;
--
FUNCTION     SET_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0,
                      ploops  PLS_INTEGER default 2,
                      xbuffer NUMBER default 0.0, ybuffer NUMBER default 0.0) Return PLS_INTEGER AS
/*
--##############################################################################
--Program Name: set_mbr
--Author: Sidey Timmins
--Creation Date: 7/25/08
--Updated:  12/05/2013 To return Tell_tales showing whether LL and UR extremes
--          occur only at the endpoints.
--          11/04/2010 To go through the array in different directions
--Usage:
  -- This PL/SQL function has 7 parameters:
  --             XYs:      the input array of xy coordinates
  --             pLB, pUB  :      elements caller must specify to start and stop
  --                              at in the ordinate array. 
  --                              pLB should be odd and pUB even.
   Output        xLL,yLL,xUR,yUR  the returned MBR (Lower left, Upper right).
--
--                ---------+ (xUR,yUR)
--                |        |
--                |        |
--      (xLL,yLL) +--------+
--
--Purpose: This function determines the MBR of a geometry. It is about 20% faster
--         than Oracle sdo_geom.sdo_mbr.

--Dependencies: None

--##############################################################################
*/

   pxLL    NUMBER;
   pyLL    NUMBER;
   pxUR    NUMBER;
   pyUR    NUMBER;
   n       PLS_INTEGER := Xys.count;
   ii      PLS_INTEGER;
   LB      PLS_INTEGER := pLB;
   UB      PLS_INTEGER := pUB;

   inc     PLS_INTEGER;
   mid     PLS_INTEGER;
   loops   PLS_INTEGER :=2;
   m       PLS_INTEGER;
   
   tell_tales PLS_INTEGER := 1;  -- assert extremes were at endpoints

BEGIN

   If UB = 0 or UB > n then   UB := n;   end if;

-- We want the loop below to just do comparisons and not stores
-- so check the the midpoint and the end of the edge.
-- Remember we start at the beginning.

   xLL := XYs(LB);
   yLL := XYS(LB+1);
   xUR := XYs(LB);
   yUR := XYs(LB+1);


   If XYs(UB-1) < xLL then
      xLL := XYs(UB-1);
   ElsIf XYs(UB-1) > xUR then
      xUR := XYs(UB-1);
   End If;

   If XYs(UB) < yLL then
      yLL := XYs(UB);
   ElsIf XYs(UB) > yUR then
      yUR := XYs(UB);
   End If;
 
   IF n <=4 THEN    -- we are done !!
      NULL;
   ELSE 
   
   pxLL := xLL+xbuffer;
   pyLL := yLL+ybuffer;
   pxUR := xUR-xbuffer;
   pyUR := yUR-ybuffer;
   
--   dbms_output.put_line('LL ' || xLL || ' y ' || yLL || ' UR ' || xUR || ' y ' || yUR || ' TT ' || tell_tales);
   mid := TRUNC((LB+UB)/2);
-- Ensure mid is even and the tests below work when n=4
   if TRUNC(mid/2) *2 <> mid then  mid := mid +1;  end if;
--  dbms_output.put_line('TT ' || tell_tales);
     If XYs(mid-1) <= pxLL then
            pxLL := XYs(mid-1);
            tell_tales := 0;
--            dbms_output.put_line('TT1 ' || tell_tales);
     ElsIf XYs(mid-1) >= pxUR then
            pxUR := XYs(mid-1);
            tell_tales := 0;
--            dbms_output.put_line('TT2 ' || tell_tales);
     End If;
  
     If XYs(mid) <= pyLL then
           pyLL := XYs(mid);
           tell_tales := 0;
--           dbms_output.put_line('TT3 ' || tell_tales || ' mid ' || mid || ' yLL ' || yLL);
     ElsIf XYs(mid) >= pyUR then
           pyUR := XYs(mid);
           tell_tales := 0;
--           dbms_output.put_line('TT4 ' || tell_tales);
     End If;
 
--dbms_output.put_line('LL ' || xLL || ' y ' || yLL || ' UR ' || xUR || ' y ' || yUR || ' TT ' || tell_tales);

-- Loop twice using a comb to avoid a lot of wasted loads and stores
-- Set the increment through the coordinates for the comb

     inc := sqrt(UB - LB +1);

     if TRUNC(inc/2)*2 <> inc then  inc := inc + 1; end if;

-- for less than 400 coordinates, brute force seems fastest
     if inc < 20 then
        inc := 2;
     elsif inc > 40 then
        inc := inc * 2;
     end if;
-- Get a rough estimate when ploops = 1 specified by caller

     If inc = 2 or ploops = 1 then  loops := 1; end if;
     If ploops = 1 and (UB-LB+1) >= 100 then inc := inc*2; end if;

-- Don't do the endpoints again so tell tales can be set.

     For jj in 1..loops Loop
       m := 1+TRUNC((UB-LB-5)/inc);
       inc := inc -1;
       if loops = 2 and jj = 1 then
         ii := UB +inc-3;
         inc := -inc;
       else
         ii := pLB-inc+2;
       end if;
--       dbms_output.put_line('tell_tales ' || tell_tales ||' ii'  || ii|| ' M ' || m || ' inc ' || inc || ' LB ' || LB || ' UB ' || UB);

-- Most of the time this loop doesn't do anything except additions and tests.

       For i in 1..m Loop
         ii := ii + inc;
         If XYs(ii) <= pxLL then
            pxLL := XYs(ii);
            tell_tales  := 0;
         ElsIf XYs(ii) >= pxUR then
            pxUR := XYs(ii);
            tell_tales  := 0;
         End If;
         ii := ii +1;
         If XYs(ii) <= pyLL then
           pyLL := XYs(ii);
           tell_tales  := 0;
         ElsIf XYs(ii) >= pyUR then
           pyUR := XYs(ii);
           tell_tales  := 0;
         End If;
       End Loop;
       inc := 2;   -- now we check every coordinate pair
--      if ii = Xys.count or ii=2 then
--       dbms_output.put_line('>>>>>>>>>>>>>>>>at bottom of loop II ' || ii);
--      end if;
--   dbms_output.put_line('tell tale ' || tell_tales ||' LL ' || pxLL || ' y ' || pyLL || ' UR ' || pxUR || ' y ' || pyUR);
     End Loop;
  END IF;

  if pxLL < xLL then
    xLL := pxLL;
  end if;
  if pyLL < yLL then
    yLL := pyLL;
  end if;
  if pxUR > xUR then
    xUR := pxUR;
  end if;
  if pyUR > yUR then
    yUR := pyUR;
  end if;
  
  Return tell_tales;
  
END SET_MBR;
--
PROCEDURE SET_MANY_MBRS(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                      MBRs IN OUT NOCOPY MBRS_TYPE,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      skip PLS_INTEGER DEFAULT 0,
                      tolerance NUMBER default 0.0,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0,
                      pbeg PLS_INTEGER default 0)  AS
/*
--##############################################################################
--Program Name: set_many_mbr
--Author: Sidey Timmins
--Creation Date: 11/03/08
--Updated:     04/15/2013 Ensured that non overlapping MBRs are detected when
--                        tolerance is > 0.
--             02/26/2013 Fixed situation where 2 MBRs share a vertex and
--                        overlap occurs between last segment of one and 1st of next.
--             08/18/2011 To specify the MBRs each MBR interacts with. This
--                       enables much more efficient processing using MBRs.
--             12/02/2008 To handle holes in polygons
--Usage:
  -- This PL/SQL function has10 parameters:
  --             XYs:      the input array of xy coordinates
  --             Info_Array: the Elem Info Array from the geometry
  --             MBRs:     an output array to be filled with MBRs
  --                       Groups of 6, xLL,yLL,xUR,yUR followed by
  --                       the lower and upper bounds for this MBR (start/stop coordinates)
  --             skip:     when skip <> 0 then skip describes a coordinate
  --                       position to start finding overlaps.
  --                       Thus if there are 2 lines concatenated together
  --                       and we only want to find overlaps between 1 and 2,
  --                       we do not want overlaps between the MBR sets for
  --                       line 1. Skip usually equals Info_array(4) when this is desired.
  --             tolerance: A distance in degrees to find interactions between
  --                        vertices.
  --             pLB, pUB  :     (lower bound and upper bound)- elements caller
  --                              must specify to start and stop at in the
  --                              ordinate array. pLB defaults to 1
  --                              and pUB defaults to Xys.count
  --             pbeg: where to start storing in MBR minus 1 (usually zero). Only
  --                   non-zero when you want to store mbrs for multiple
  --                   polygons within the same MBR array. You keep track
  --                   of where they begin and end!
  --
   Output        xLL,yLL,xUR,yUR  the returned MBR (Lower left, Upper right).
--
--                ---------+ (xUR,yUR)
--                |        |
--                |        |
--      (xLL,yLL) +--------+

--
--Purpose:
--          Takes an XY ordinate array and produces many MBRs (Minimum Bounding
--          Rectangles or boxes) and the overall extent (xLL,yLL) to (xUR,yUR)
--          spanning the coordinates. These MBRS enable a geometry to be subdivided.
--                            ________________             
--                            |/\/|/\/\/|\_/\|
--                       _____/_________|   /|     Example: Trapezoid polygon
--                       |   /|         |  / |              might have these 4 MBRs
--                       |  / |         | /  |              placed around the perimeter
--                       | /  |         |/   |
--                       |/___|_________/___ |
--                       |\/\/\/\/\/\/\/|
--                       ----------------
--
--         This function determines many touching MBRs around the perimeter of
--         a geometry. These MBRs can be used very efficiently to limit xy searches.
--         If the LB is not equal to 1 then the Info Array is ignored and MBRs
--         are determined using pLB and pUB.

--Dependencies: SET_MBR
--##############################################################################
*/
     rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
     twopi     CONSTANT NUMBER := 6.2831853071795864769252867665590057684;
     pi        CONSTANT NUMBER := 3.1415926535897932384626433832795028842;
     
     endpoints       NUMBER;
     xLL2            NUMBER;
     yLL2            NUMBER;
     xUR2            NUMBER;
     yUR2            NUMBER;
     dx1             NUMBER;
     dy1             NUMBER;
     dx2             NUMBER;
     dy2             NUMBER;
     xlast           NUMBER;
     ylast           NUMBER;
     xnext           NUMBER;
     ynext           NUMBER;
     x               NUMBER;
     y               NUMBER;
     bearing         NUMBER;
     last_bearing    NUMBER;
     average_Xextent   NUMBER := 0.0;
     average_Yextent   NUMBER := 0.0;
     raverage_Xextent   NUMBER := 0.0;
     raverage_Yextent   NUMBER := 0.0;
     accum_angle     NUMBER :=0.0;
     angle_turned    NUMBER;
     old_bearing     NUMBER;
     dist1           NUMBER;
     dist2           NUMBER;
     xsav            NUMBER;
     ysav            NUMBER;
     xp              NUMBER;
     yp              NUMBER;
     
     check_it        PLS_INTEGER;
     kstart          PLS_INTEGER;
     pdim            PLS_INTEGER := 8;
     LB              PLS_INTEGER := pLB;
     UB              PLS_INTEGER := pUB;
     nextii          PLS_INTEGER := 1;
     found           PLS_INTEGER;
     InUB            PLS_INTEGER;
     iend            PLS_INTEGER;
     LBstart         PLS_INTEGER;
 
     LBB             PLS_INTEGER;
     UBB             PLS_INTEGER := 0;
     left_over       PLS_INTEGER;
     m               PLS_INTEGER;
     mm              PLS_INTEGER;
     nc              PLS_INTEGER;
     no_to_do        PLS_INTEGER;
     no_to_do2       PLS_INTEGER;
     no_holes        PLS_INTEGER := 0;
     ii              PLS_INTEGER := 0;
     jj              PLS_INTEGER;
     jk              PLS_INTEGER;
     pos             PLS_INTEGER;
     jpos            PLS_INTEGER;
     len_required    PLS_INTEGER;
     next            PLS_INTEGER := 1;
     where_overlap     PLS_INTEGER := 0;
     is_ring         PLS_INTEGER := Info_Array(2);
     
     function even(n_in pls_integer) return pls_integer as
       n_even pls_integer := n_in;
     begin
       if 2*TRUNC(n_even/2) <> n_even then
         n_even := n_even+1;
       end if;      
       return n_even;       
     end;
/*
     function MBRS_OVERLAP(x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,xLL NUMBER,yLL NUMBER,xUR NUMBER,yUR NUMBER) RETURN NUMBER AS

-- Using Cohen-Sutherland Clipping outcodes, determine if a line overlaps a rectangle.
-- Taken from Wikipedia

     left_    pls_integer :=1;
     right_   pls_integer :=2;
     bottom_  pls_integer :=4;
     top_     pls_integer :=8;
  
     code1    pls_integer :=0;
     code2    pls_integer :=0;
   
   begin

    -- Compute two outcodes
      
      if x1 < xLL then
          code1 := code1 + left_;
      elsif x1 > xUR then
          code1 := code1 + right_;
      end if;
      
      if y1 < yLL then
          code1 := code1 + bottom_;
      elsif y1 > yUR then
          code1 := code1 + top_;
      end if;
      
      if x2 < xLL then
          code2 := code2 + left_;
      elsif x2 > xUR then
          code2 := code2 + right_;
      end if;
      
      
      if y2 < yLL then
          code2 := code2 + bottom_;
      elsif y2 > yUR then
          code2 := code2 + top_;
      end if;
    -- Oracle has an AND function that works on bits
    
      if bitand(code1,code2) <> 0 then
        return 0;   -- no Overlap
      end if;
      
      return 1;  -- Overlap possible
  
   end;
--     
*/     
BEGIN

   MBRs(1).xLL := 0.;
   if UB = 0 then UB := XYs.count; end if;

   InUB := UB;

   if LB = 1 then no_holes := TRUNC(Info_Array.count/3) -1; end if;

    If no_holes > 0 then
       next := 4;

       for ii in 1..no_holes loop
         if Info_array(ii*3+1) > UB then
           dbms_output.put_line('UB ' || UB || ' info ' || (Info_array(ii*3+1)));
--           TRACK_APP_ERROR('SET_MANY_MBRS: INFO array does not match Xys');
         end if;
       end loop;
       UB := Info_Array(4) -1;
    End if;
    nc := (InUB-LB+1);
    m := TRUNC(sqrt(nc/2));
    if nc <= 10 then
      m := 1;
    elsif nc <= 20 then
      m := 2;
    end if;
    
--        dbms_output.put_line('m ' || m || 'inub ' || inub || ' ' ||lb);
    if m > 60 then
       m := m * TRUNC(m/20) ;  -- m := m*8;    -- This is new  08/18/2011
    end if;
    if m <= 0 then  m := 1; end if;
 
-- To get MBRs rectangles touching we have to reuse the last vertex as the
-- next start vertex (hence -2).

    no_to_do := TRUNC(nc/m);

--  dbms_output.put_line('noto do' || no_to_do || ' inub ' ||inub || ' lb ' || LB || ' m ' || m);
    no_to_do := even(no_to_do);
--          dbms_output.put_line('NOW noto do' || no_to_do );
 
    if no_to_do <= 2 then
      m := 1;
    else
      m := TRUNC(nc/(no_to_do -2));
--        dbms_output.put_line('now m ' || m);
        
      If (m* (no_to_do-2)) < nc then 
         m := m + 1;
--         dbms_output.put_line('Now m*notodo ' || (no_to_do-4)|| ' ' ||(InUB-LB+1));
         If (m* (no_to_do-4)) >= nc then
           no_to_do := no_to_do -2;
         End if;
      End If;
-- try and minimize left over coordinates
      left_over := m* (no_to_do-2) -nc;
--      dbms_output.put_line('left_over ' || left_over);
--      if no_to_do > 10 then
--        if (m+1) *(no_to_do-4) >= nc and nc - (m+1) *(no_to_do-4) < left_over then
--           m := m+1;
--           dbms_output.put_line('RESET m ' || m );
--           no_to_do := no_to_do -2;
--        End if;       
--      end if;
--        dbms_output.put_line('Now m ' || m || ' no to do ' || no_to_do);
    end if;
    -- if we have 26/2 => 13 then we want at least 14 so 2*no_to_do2 >= no_to_do
    no_to_do2 := even(no_to_do/2);


    LBB := LB;
    LBstart := LBB;

--    dbms_output.put_line('M ' || m || ' mm ' || mm || ' UB ' || UB || ' notodo2 ' || no_to_do2);
--         dbms_output.put_line('M ' || m || ' holes ' || no_holes || ' no to do ' || no_to_do || ' count ' || Xys.count || 'inub ' || InUB);
    WHILE UBB <> UB LOOP --For ii in 1..mm Loop
        
        UBB := LBB + no_to_do-1;
        If UBB > UB then UBB := UB;  End if;
        ii := ii + 1;
        
-- This is new below 03/26/2013
        if ii = nextii then
         -- Setup average extents at start of each ring
         mm := TRUNC((UB-LBB+1)/no_to_do);
         if mm < 1 then mm := 1; end if;
         endpoints := Set_MBR(XYs,xLL2,yLL2,xUR2,yUR2,LBB,UB,1);

         average_Xextent := 2.*(xUR2-xLL2) /mm;
         average_Yextent := 2.*(yUR2-yLL2)  /mm;
--         dbms_output.put_line('XEX ' || round(average_Xextent,6) || ' YEX ' || round(average_Yextent,6));
        end if;
        
        endpoints := Set_MBR(XYs,xLL2,yLL2,xUR2,yUR2,LBB,UBB,2,tolerance);
        
-- If (and only if) we go very far between start and end of our range (LBB to UBB) then
-- break the range in half

        if UBB-LBB > 6 and average_Xextent <> 0.0 and average_Yextent <> 0.0 then
          if ABS(xUR2-xLL2) > 2. * average_Xextent or
             ABS(yUR2-yLL2) > 2. * average_Yextent then
             
             UBB := LBB + no_to_do2-1;
             If UBB > UB then
               UBB := UB;
             End if;
             endpoints := Set_MBR(XYs,xLL2,yLL2,xUR2,yUR2,LBB,UBB,2,tolerance);

--               dbms_output.put_line('II ' || ii || ' LBB ' || LBB || ' UBB ' || UBB || ' count ' ||Xys.count || ' UB ' || UB);
          elsif left_over > 0 and left_over < no_to_do2 and UBB <> UB then
             if left_over >= 4 then
               UBB := UBB +4;
             else
               uBB := UBB +2;
             end if;
             if UBB > UB then
                UBB := UB;
             end if;
             left_over := MOD(UB-UBB,no_to_do-2);
             endpoints := Set_MBR(XYs,xLL2,yLL2,xUR2,yUR2,LBB,UBB,2,tolerance);
--             dbms_output.put_line('updated UBB, '||UBB || ' UB ' || UB ||' now leftover ' || left_over);
          end if;
        end if;
--          dbms_output.put_line('II ' || ii || ' LBB ' || LBB || ' UBB ' || UBB || ' count ' ||Xys.count || ' UB ' || UB);
--          dbms_output.put_line('II ' || ii || ' xLL ' || XLL2 || ' yLL ' || YLL2);
--          dbms_output.put_line('II ' || ii || ' xUR ' || XUR2 || ' yLL ' || YUR2);


        jj := ii+1; 

        MBRs(jj).xLL := xLL2;
        MBRs(jj).yLL := yLL2;
        MBRs(jj).xUR := xUR2;
        MBRs(jj).yUR := yUR2;
        MBRs(jj).lo  := LBB;
        MBRs(jj).start_overlap  := 0.0;    -- may overlap
        MBRs(jj).endpoints := endpoints;   --  1 means extremes are at endpoints
        
-- It is very useful when skip is zero to find out if the segments themselves
-- have any potential to overlap.  We just want to know if the segments just
-- walk away from each other and never loop back. If they do so the angle
-- between the segments will be >= 90.
 
        found :=0;
        where_overlap := 0;
        IF skip =0 or LBB < skip THEN
 
          kstart := 2;
          check_it := UBB+2;
          iend := TRUNC((UBB-LBB+1)/2);
          if (skip <> 0 and check_it > skip) then
            xlast := Xys(LBB);
            ylast := Xys(LBB+1);
            jpos := 2;
            kstart := 3;
          elsif is_ring = 2 then
             xlast := Xys(LBB);
             ylast := Xys(LBB+1);
             jpos := LBB+1;
          elsif LBB = LBstart and is_ring = 1003 then 
            xlast := Xys(UB-3);
            ylast := Xys(UB-2);
            jpos := LBB-1;
          else  -- LBB <> LBstart so we are partway thru the ring
            xlast := Xys(LBB-2);
            ylast := Xys(LBB-1);
            jpos := LBB-1;
          end if;
          

-- Two MBRs can end and next one begin on a vertex. So check for this situation 1st:
--
--          +-_-_-_-_-_-_-_-_
--                  Angle close to zero
--                  

          
          for kk in kstart..iend loop
            xsav := xlast;
            ysav := ylast;
 -- Check dot product which is in the range 1 to zero for
 --                                 angles 0 to 90.
 -- (when divided by the lengths of the vectors).
            jpos := jpos+1;
            dy1 := ylast -XYs(jpos);
            dy2 := XYs(jpos+2) -Xys(jpos);
            ylast := Xys(jpos);
            jpos := jpos +1;
            dx1 := xlast- XYs(jpos);
            dx2 := XYs(jpos+2) -Xys(jpos);
            xlast := Xys(jpos);
--            if jj=27 and kk=2 then
--             dbms_output.put_line('dx1 ' || dx1 || ' dy1 ' || dy1 || ' dx2 ' || dx2 || ' dy2 ' || dy2 || ' lbstart ' || lbstart);
--  dbms_output.put_line('xlast ' || xsav ||','||ysav );
--dbms_output.put_line('x ' || Xys(jpos)||','||Xys(jpos+1));
--dbms_output.put_line('x ' || Xys(jpos-2)||','||Xys(jpos-1));
--   end if;
-- "Forget" to divide by lengths and range is positive to zero instead.
            if dx1*dx2 + dy1*dy2 > 0. then

-- Between MBRs, we check the perpendicular distance to see whether it is
-- within tolerance
             
                 if kk=2 then
                    dist1 := Perpendicular(xsav,ysav,xlast,ylast,XYs(jpos+1),XYs(jpos+2),xp,yp); 
                    dist2 := Perpendicular(XYs(jpos+1),XYs(jpos+2),xlast,ylast,xsav,ysav,xp,yp);
--          dbms_output.put_line('D1 ' || dist1 || ' D2 ' || dist2);
                    if dist1 > tolerance and dist2 > tolerance then
                       continue;
                    end if;
                    MBRs(jj).endpoints := 0;
  -- save segment 1 interacts with the last segment for a ring
                    if check_it = UBB+2 then
                      MBRs(nextii+1).mbr_list(1) := jj;
                    end if;
                 end if;
                  where_overlap := jpos-2;
--dbms_output.put_line('set may overlap ' || where_overlap ||' for ' || jj|| ' and kk ' ||kk|| ' UBB ' || UBB|| ' jpos ' || jpos);
--dbms_output.put_line('xlast ' || xsav ||','||ysav );
--dbms_output.put_line('x ' || Xys(jpos)||','||Xys(jpos+1));
--dbms_output.put_line('x ' || Xys(jpos-2)||','||Xys(jpos-1));
                  MBRs(jj).start_overlap := where_overlap;
                  found := jpos;
                  exit;
            end if;
          end loop;
         
-- Flag to say no potential for intersection within this MBR
          if where_overlap =0 then
          
             x := Xys(LBB);
             y := Xys(LBB+1);
             jpos := LBB+1;
             for kk in 2..iend loop
               xlast := x;
               ylast := y;
 -- Check accumulated angle
               jpos := jpos +1;
               x := Xys(jpos);
               jpos := jpos+1;
               y := Xys(jpos);
               last_bearing := bearing;
               bearing :=  GZ_QA.geo_bearing(xlast,ylast,x,y,old_bearing)*rad2deg;
               angle_turned := bearing-last_bearing;
               if angle_turned < -180. then
                  angle_turned := 360.+ angle_turned;
               elsif angle_turned > 180. then 
                  angle_turned := 360.-angle_turned;
               end if;

--               accum_angle :=  accum_angle + angle_turned;
               if kk = 2 then
                 accum_angle := 0.;
                 angle_turned := 0.;
               end if;

--if jj = 3 then
--  dbms_output.put_line('jpos ' || jpos ||' A ' ||   round(angle_turned,6)  || ' B ' || round(bearing,6));         
--end if;
             if ABS(angle_turned) >= 91. then  
--             if ABS(accum_angle) >= 91. then 
                   where_overlap := jpos-3;
                   exit;
             end if;
             end loop;
-- If there is no potential for overlap then mark it
             if where_overlap = 0 then
               MBRs(jj).start_overlap := -1.0;
--              dbms_output.put_line('NO overlap for  ' || jj);
-- If there is a potential for partial overlap then mark first occurrence -2
             else
               MBRs(jj).start_overlap := where_overlap;
--               dbms_output.put_line('>>>SETTING may overlap ' || where_overlap ||' for ' || jj);
             end if;
          elsif found < UBB-1 then
               found := 0;            -- set semaphore to ignore
          end if;
        END IF;

        raverage_Xextent := (raverage_Xextent + ABS(XUR2-xLL2));
        raverage_Yextent := (raverage_Yextent + ABS(YUR2-yLL2));
--        dbms_output.put_line('ii ' || ii || ' XEX ' || round(average_Xextent,6) || ' YEX ' || round(average_Yextent,6) || ' ' ||round(xur2-xLL2,6) || ' ' || round(yur2-yLL2,6));
-- NEW   If we are at the end of a ring set a flag not to use the non existent
--       segment between the 2 rings
        if UBB = UB then
          MBRs(jj).hi := UBB+.01;  -- This fraction is transparent
        else
          MBRs(jj).hi := UBB;
        end if;
        if found <> 0 then
           MBRs(jj).hi := -MBRs(jj).hi;   -- use semaphore that  that there is a V vertex between MBRs 
        end if;
-- Work out overall MBR
        If ii = 1 or xLL2 < xLL then
           xLL := xLL2;
        End if;
        if ii = 1 or xUR2 > xUR then
           xUR := xUR2;
        End If;
        If ii = 1 or yLL2 < yLL  then
           yLL := yLL2;
        End if;
        if ii = 1 or yUR2 > yUR then
           yUR := yUR2;
        End If;
 
-- handle holes
        If no_holes > 0 and UBB = UB then
           nextii := ii+1;
           next := next + 3;
           LBB := UB + 1;
           LBstart := LBB;
 
           if next < Info_Array.count then
             UB := Info_Array(next)-1;
           else
             exit when UB = Xys.count;
             UB := XYs.count;
           end if;
           exit when LBB >= InUB;
        Else
--            exit when UBB >= UB;
          LBB := UBB-1;
        End if;
--        dbms_output.put_line('jj ' || jj || ' ' || Mbrs(jj).xLL || ' ' || mbrs(jj).lo || ' ' ||mbrs(jj).hi);
    End Loop;
    MBRs(1).xLL := xLL;        -- Overall MBR of this data set
    MBRs(1).yLL := yLL;
    MBRs(1).xUR := xUR;
    MBRs(1).yUR := yUR;
    MBRS(1).lo  := MBRs.count;          -- the number of MBRs for this data set
    MBRS(1).hi := no_to_do;
 
-- dbms_output.put_line('Real XEX ' || round(((xUR-xLL)/(mbrs.count-1)),6) || ' YEX ' || round(((yUR-yll)/(mbrs.count-1)),6));
-- dbms_output.put_line('Real XEX ' || round(raverage_Xextent/ii,6) || ' YEX ' || round(raverage_Yextent/ii,6));
--    dbms_output.put_line('m ' || m || ' mm ' || mm || ' ii ' || ii );
-- Work out the touching relationships between MBRs and record them
-- We already know that MBR n touches MBR n-1 and n+1.
-- Place pointers to the ones that touch in mbr_List


    
    For jj in 2..MBRs.count Loop   -- The first MBR is the overall MBR so start at 2
  
      xLL2 := MBRs(jj).xLL - tolerance;
      yLL2 := MBRs(jj).yLL - tolerance;
      xUR2 := MBRs(jj).xUR + tolerance;
      yUR2 := MBRs(jj).yUR + tolerance;
--      dbms_output.put_line('jj ' ||jj ||' ' || xLL2 || ',' || YLl2 ||',' || xur2||','||yur2);
      pos := MBRs(jj).mbr_list.count;
      
      for kk in jj+1..MBRs.count Loop  -- work out inter MBR relationships

        where_overlap := 0;
-- Test for Overlap
-- dbms_output.put_line('KK ' ||kk || ' ' || Mbrs(kk).xLL || ',' ||mbrs(kk).yll ||' ' ||mbrs(kk).xUR||',' ||Mbrs(kk).yUR);
        IF   (kk=jj+1 and MBRs(jj).endpoints = 1 ) or 
           ((MBRs(kk).xUR  < xLL2 or MBRs(kk).xLL  > xUR2) or (MBRs(kk).yUR < yLL2 or MBRs(kk).yLL > yUR2)) THEN

--            dbms_output.put_line('jj ' ||jj || ' and ' || kk ||' dont overlap' || xLL2 || ',' || YLl2 ||',' || xur2||','||yur2 || ' EP ' || MBRS(jj).endpoints);
--            dbms_output.put_line('tol '|| tolerance ||'          dont overlap' || MBRS(kk).xLL || ',' || MBRS(kk).YLL ||',' || MBRS(kk).xur||','||MBRS(kk).yur);
          NULL;   -- none
        ELSIF skip = 0 or MBRs(kk).lo >= skip THEN

-- Figure out if the coordinates really touch at a tolerance.
-- The MBRs touch either in the X direction or the Y.

-- First check the X's
--            _______ _______
--            |      |       |     sideways overlapping
--            ----------------
--
           
            LBB := MBRs(kk).lo;
            UBB := ABS(MBRs(kk).hi);
            if kk = jj+1 then  -- we know the first value of kk is attached to jj
               LBB := LBB+2;
            end if;
            
            jpos := LBB;
--            dbms_output.put_line('checkin ' || jj || ' with ' || kk ||' mbr ' ||MBRs(kk).hi  || ' xys ' || (xys(jpos)+tolerance));
            if kk=jj+1 and (MBRs(jj).hi < 0) then
--              dbms_output.put_line('FOUND 0 ' || jj || ' with ' || kk);
              MBRs(jj).hi := ABS(MBRs(jj).hi);
              where_overlap := 1;
          elsif  (MBRs(kk).xUR  = MBRs(jj).xLL) then
            While jpos <= UBB loop
               if Xys(jpos) > xLL2 then
                  where_overlap := 1;
--                   dbms_output.put_line('FOUND 1 ' || jj || ' with ' || kk);
                  exit;
               end if;
               jpos := jpos +2;
            end loop;
          elsif  (MBRs(kk).xLL  = MBRs(jj).xUR) then
            While jpos <= UBB loop
               if Xys(jpos) < xUR2 then
                  where_overlap := 1;
--                   dbms_output.put_line('FOUND 2 ' || jj || ' with ' || kk);
                  exit;
               end if;
               jpos := jpos +2;
            end loop;
-- Now check the Y's
--            _______
--            |      |     top and bottom overlapping
--            --------
--            |      |
--            --------
--
        elsif (MBRs(kk).yUR  = MBRs(jj).yLL)  then

            jpos := jpos+1;
            While jpos <= UBB loop
               if Xys(jpos) >= yLL2  then
                  where_overlap := 1;
--                   dbms_output.put_line('FOUND 3 ' || jj || ' with ' || kk|| ' at jpos ' || jpos );
                  exit;
               end if;
               jpos := jpos +2;
            end loop;
        elsif (MBRs(kk).yUR  = MBRs(jj).yLL) or (MBRs(kk).yLL  = MBRs(jj).yUR) then

            jpos := jpos+1;
            While jpos <= UBB loop
               if Xys(jpos) < yUR2 then
                  where_overlap := 1;
--                   dbms_output.put_line('FOUND 4 ' || jj || ' with ' || kk);
                  exit;
               end if;
               jpos := jpos +2;
            end loop;
        ELSE
-- If they completely overlap then the checking above is unnecessary.
          where_overlap := 1;
--           dbms_output.put_line('FOUND 5 ' || jj || ' with ' || kk);    
        end if;

      END IF;
   
-- Store overlapping MBRs for the subject MBR

      IF where_overlap  = 1 THEN
          pos := pos + 1;
          MBRs(jj).mbr_list(pos) := kk;
--            dbms_output.put_line('jj ' || jj || ' overlaps ' || kk);
      END IF;
      
      
      end loop;
      

      MBRs(jj).hi := ABS(MBRs(jj).hi);
    End Loop;

END SET_MANY_MBRS;
--
Procedure setup_side(side in out nocopy pls_integer,inc1 pls_integer, next_one in out nocopy pls_integer,start_of_ring pls_integer, end_of_ring pls_integer) as
   
   inside  pls_integer := side;
 
   Begin
--     dbms_output.put_line('start ' || start_of_ring || ' end ' || end_of_ring);
      if inc1 >= 0 then
         side := side + inc1;
         if side >= end_of_ring then
            side := start_of_ring;
         end if;
         next_one := side + 1;   
         if next_one >= end_of_ring then
            next_one := start_of_ring;
         end if;
      else
         side := side +inc1;
         if side < start_of_ring then
           side := end_of_ring-1;
         end if;
         next_one := side +1;   
         if next_one >= end_of_ring then
           next_one := start_of_ring;
         end if;
      end if;
--dbms_output.put_line('now side  ' || side || ',' || next_one);

End Setup_side;
--
PROCEDURE SHELLSORT2( Arr         IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Arr2   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       InLB          PLS_INTEGER default 1,
                       InUB          PLS_INTEGER default 0,
                       forwards      VARCHAR2 default 'ASC')
 AS
/*******************************************************************************
--Program Name: stablesort2
--Author: Sidey Timmins
--Creation Date: 01/04/2007
--Updated:  12/01/2010 To be more efficient when the Arr2 has duplicates.
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 2 required parameters:
  --
  --   REQUIRED Parameters:
 --          Arr             - Input number Array to sort. Sort is ascending.

 --          Arr2     - Companion Array to be also sorted (usually
 --                            contains 1.. Arr.count. This array can be used
 --                            to sort other arrays:

 --                       FOR i IN 1..N LOOP
 --                          Arr2(i) := i;
 --                       END LOOP;

 --                       shellsort2(Data_Array,Arr2,1,n);

 --                       FOR i IN 1..N LOOP
 --                          Sorted_Array2(i) := Array2(Arr2(i));
 --                       END LOOP;

 --          LB              - lower bound of Arr to sort, defaults to 1
 --          UB              - upper bound of Arr to sort, defaults to Arr.count
--
--Purpose:
  -- Sorts 2 arrays and is stable when the order array has unique ascending values.
  -- Example
  --          input  arrays                          sorted output
  --    unsorted Nodes   Arr2        Node                  Arr2
--     216000003385025             1         8185025                   3
--     216000008785025             2         8185025                   4
--             8185025             3         8385025                   5
--             8185025             4         216000003385025           1
--             8385025             5         216000008785025           2
--     216000008785025             6         216000008785025           6

-- Sort in place with no additional storage. Stablesort is stable since it uses
-- the value of the Arr2 to maintain the order of equal Array values
-- (Note Arr2 elements 3 and 4 in the diagram)
-- Reference: Robert Sedgewick "Algorithms" Addison Wesley 1984
--            DL Shell. "A High-Speed Sorting PROCEDURE"
--                       Communications of the ACM, 2,7 30-32 (1959)
--Dependencies: none
--Updates:
--          01/04/07 Added trunc to fix SQL divide bug. No change in behaviour.
********************************************************************************
*/

  LB               PLS_INTEGER               := InLB;
  LBh              PLS_INTEGER;
  UB               PLS_INTEGER               := InUB;
  h                PLS_INTEGER;
  i                PLS_INTEGER;
  j                PLS_INTEGER;
  jmh              PLS_INTEGER;

  Ladd             NUMBER;
  valu             NUMBER;
  last             NUMBER;
  n                PLS_INTEGER;
  loups            PLS_INTEGER :=0;
  BEGIN

  IF UB = 0 THEN
    UB := Arr.count;
  END IF;

  n := UB - LB +1;
  IF (n <= 1) THEN
    RETURN;
  END IF;

 -- compute largest increment: h

   h := 1;

-- Successive values of h are 1, 4, 13, 40, 121,..
-- see references to understand this loop
 
   WHILE (3 * h +1 < n) LOOP
      h := 3 * h + 1;
   END LOOP;
 -- After this line, have the largest increment <= n
 

   IF forwards = 'ASC' THEN

-- Sort by insertion in increments of h

   While h > 0  LOOP

-- Sort by insertion in increments of h
     LBh := LB+h;
      For i IN LBh..UB LOOP
        valu := Arr(i);
        ladd := Arr2(i);
        j := i;
        jmh := j - h;
 
        WHILE ( j >= LBh) and (valu < Arr(jmh)) LOOP
           Arr(j) := Arr(jmh);
           Arr2(j) := Arr2(jmh);
           j := jmh;
           jmh := j - h;
           loups := loups + 1;
  
        END LOOP;
        Arr(j) := valu;
        Arr2(j) := ladd;
      END LOOP;
     h := trunc(h / 3);
  End Loop;

-- Array is now sorted ascending
   ELSE

-- Sort the order array backwards (decreasing)

    While h > 0  LOOP

-- Sort by insertion in increments of h
     LBh := LB+h;
      For i IN LBh..UB LOOP
        valu := Arr(i);
        ladd := Arr2(i);
        j := i;
        jmh := j - h;
        WHILE ( j >= LBh) and (valu > Arr(jmh)) LOOP
           Arr(j) := Arr(jmh);
           Arr2(j) := Arr2(jmh);
           j := jmh;
           jmh := j - h;
        END LOOP;
        Arr(j) := valu;
        Arr2(j) := ladd;
      END LOOP;
     h := trunc(h / 3);
    End Loop;
   END IF;


END SHELLSORT2;
--
PROCEDURE STABLESORT3( Arr           IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Order_array   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Extra_array   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       InLB          PLS_INTEGER default 1,
                       InUB          PLS_INTEGER default 0,
                       forwards      BOOLEAN default TRUE)
 AS
/*******************************************************************************
--Program Name: stablesort3
--Author: Sidey Timmins
--Creation Date: 01/04/2007

--Usage:
  -- This procedure has 3 required parameters:
  --
  --   REQUIRED Parameters:
 --          Arr             - Input number Array to sort. Sort is ascending.

 --          Order_array     - Companion Array to be used to affect the sort
 --                            (usually contains 1.. Arr.count. This array can
 --                            be used to sort other arrays:

 --                       FOR i IN 1..N LOOP
 --                          Order_array(i) := i;
 --                       END LOOP;

 --                       shellsort2(Data_Array,Order_array,1,n);

 --                       FOR i IN 1..N LOOP
 --                          Sorted_Array2(i) := Array2(Order_array(i));
 --                       END LOOP;
 --          Extra_array     - Another array to be sorted.
 --          LB              - lower bound of Arr to sort, defaults to 1
 --          UB              - upper bound of Arr to sort, defaults to Arr.count
--
--Purpose:
  -- Sorts 2 arrays and is stable when the order array has unique ascending values.
  -- Example
  --          input  arrays                          sorted output
  --    unsorted Arr     Order_Array         Arr                 Order_Array
--     216000003385025             1         8185025                   3
--     216000008785025             2         8185025                   4
--             8185025             3         8385025                   5
--             8185025             4         216000003385025           1
--             8385025             5         216000008785025           2
--     216000008785025             6         216000008785025           6

-- Sort in place with no additional storage. Stablesort is stable since it uses
-- the value of the Order_Array to maintain the order of equal Array values
-- (Note Order_Array elements 3 and 4 in the diagram)
-- Reference: Robert Sedgewick "Algorithms" Addison Wesley 1984
--            DL Shell. "A High-Speed Sorting Procedure"
--                       Communications of the ACM, 2,7 30-32 (1959)
--Dependencies: none
--Updates:
--          01/04/07 Added trunc to fix SQL divide bug. No change in behaviour.
********************************************************************************
*/

  LB               PLS_INTEGER               := InLB;
  UB               PLS_INTEGER               := InUB;
  h                PLS_INTEGER;
  i                PLS_INTEGER;
  j                PLS_INTEGER;

  Ladd             NUMBER;
  valu             NUMBER;
  last             NUMBER;
  extra            NUMBER;
  n                PLS_INTEGER;

  BEGIN

  IF UB = 0 THEN
    UB := Arr.count;
  END IF;

  n := UB - LB +1;
  IF (n <= 1) THEN
    RETURN;
  END IF;

 -- compute largest increment: h

   h := 1;

   IF (n >= 14) THEN
-- Successive values of h are 1, 4, 14, 40, 121,..
-- see references to understand this loop
     h := 1;
     WHILE (h <= n) LOOP
       h := 3 * h + 1;
     END LOOP;
 -- After this line, have the largest increment <= n
     h := trunc(h / 3);

   END IF;

   IF forwards = TRUE THEN
     While h > 0  LOOP

-- Sort by insertion in increments of h

       For i IN (LB + h)..UB LOOP
         valu := Arr(i);
         ladd := Order_array(i);
         extra := Extra_Array(i);
         j := i - h;
         WHILE ( j >= LB) AND ((Arr(j) > valu) or (Arr(j) = valu and ladd < Order_Array(j))) LOOP
           Arr(j + h) := Arr(j);
           Order_array(j + h) := Order_array(j);
           Extra_array(j+h) := Extra_Array(j);
           j := j - h;
         END LOOP;
         Arr(j + h) := valu;
         Order_array(j + h) := ladd;
         Extra_array(j+h) := extra;
       END LOOP;
       h := trunc(h / 3);
     End Loop;

-- Array is now sorted ascending
   ELSE
-- Sort the order array backwards (decreasing)
   While h > 0  LOOP

       For i IN (LB + h)..UB LOOP
         valu := Arr(i);
         ladd := Order_array(i);
         extra := Extra_Array(i);
         j := i - h;
         WHILE ( j >= LB) AND (Arr(j) < valu or (Arr(j) = valu and ladd > Order_Array(j) )) LOOP
           Arr(j + h) := Arr(j);
           Order_array(j + h) := Order_array(j);
           Extra_array(j+h) := Extra_Array(j);
           j := j - h;
         END LOOP;
         Arr(j + h) := valu;
         Order_array(j + h) := ladd;
         Extra_array(j+h) := extra;
       END LOOP;
       h := trunc(h / 3);
     End Loop;
   END IF;


End Stablesort3;
--
PROCEDURE SHELLSORT4( Arr         IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Arr2   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Arr3   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Arr4   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       InLB          PLS_INTEGER default 1,
                       InUB          PLS_INTEGER default 0
                      )
 AS
/*******************************************************************************
--Program Name: stablesort2
--Author: Sidey Timmins
--Creation Date: 01/04/2007
--Updated:  12/01/2010 To be more efficient when the Arr2 has duplicates.
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 2 required parameters:
  --
  --   REQUIRED Parameters:
 --          Arr             - Input number Array to sort. Sort is ascending.

 --          Arr2     - Companion Array to be also sorted (usually
 --                            contains 1.. Arr.count. This array can be used
 --                            to sort other arrays:

 --                       FOR i IN 1..N LOOP
 --                          Arr2(i) := i;
 --                       END LOOP;

 --                       shellsort2(Data_Array,Arr2,1,n);

 --                       FOR i IN 1..N LOOP
 --                          Sorted_Array2(i) := Array2(Arr2(i));
 --                       END LOOP;

 --          LB              - lower bound of Arr to sort, defaults to 1
 --          UB              - upper bound of Arr to sort, defaults to Arr.count
--
--Purpose:
  -- Sorts 2 arrays and is stable when the order array has unique ascending values.
  -- Example
  --          input  arrays                          sorted output
  --    unsorted Nodes   Arr2        Node                  Arr2
--     216000003385025             1         8185025                   3
--     216000008785025             2         8185025                   4
--             8185025             3         8385025                   5
--             8185025             4         216000003385025           1
--             8385025             5         216000008785025           2
--     216000008785025             6         216000008785025           6

-- Sort in place with no additional storage. Stablesort is stable since it uses
-- the value of the Arr2 to maintain the order of equal Array values
-- (Note Arr2 elements 3 and 4 in the diagram)
-- Reference: Robert Sedgewick "Algorithms" Addison Wesley 1984
--            DL Shell. "A High-Speed Sorting PROCEDURE"
--                       Communications of the ACM, 2,7 30-32 (1959)
--Dependencies: none
--Updates:
--          01/04/07 Added trunc to fix SQL divide bug. No change in behaviour.
********************************************************************************
*/

  LB               PLS_INTEGER               := InLB;
  LBh              PLS_INTEGER;
  UB               PLS_INTEGER               := InUB;
  h                PLS_INTEGER;
  i                PLS_INTEGER;
  j                PLS_INTEGER;
  jmh              PLS_INTEGER;

  Ladd             NUMBER;
  madd             NUMBER;
  nadd             NUMBER;
  valu             NUMBER;
  last             NUMBER;
  n                PLS_INTEGER;
  loups            PLS_INTEGER :=0;
  BEGIN

  IF UB = 0 THEN
    UB := Arr.count;
  END IF;

  n := UB - LB +1;
  IF (n <= 1) THEN
    RETURN;
  END IF;

 -- compute largest increment: h

   h := 1;

   IF (n >= 14) THEN
-- Successive values of h are 1, 4, 14, 40, 121,..
-- see references to understand this loop
     h := 1;
     WHILE (h <= n) LOOP
       h := 3 * h + 1;
     END LOOP;
 -- After this line, have the largest increment <= n
     h := trunc(h / 3);

   END IF;


-- Sort by insertion in increments of h

   While h > 0  LOOP

-- Sort by insertion in increments of h
     LBh := LB+h;
      For i IN LBh..UB LOOP
        valu := Arr(i);
        ladd := Arr2(i);
        madd := Arr3(i);
        nadd := Arr4(i);
        j := i;
        jmh := j - h;
-- Special sort with First array ascending and 2nd array descending
        WHILE ( j >= LBh) and (valu < Arr(jmh) or (Arr(jmh) = valu and ladd > Arr2(jmh) ) ) LOOP
           Arr(j) := Arr(jmh);
           Arr2(j) := Arr2(jmh);
           Arr3(j) := Arr3(jmh);
           Arr4(j) := Arr4(jmh);
           j := jmh;
           jmh := j - h;
        END LOOP;
        Arr(j) := valu;
        Arr2(j) := ladd;
        Arr3(j) := madd;
        Arr4(j) := nadd;
      END LOOP;
     h := trunc(h / 3);
  End Loop;


END SHELLSORT4;
--
FUNCTION SORT_PIPES_OBJ(A IN PIPES_TYPE) RETURN PIPES_TYPE AS

    
    Matches           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Subject_Segs      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Order_Array       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Dummy_Array       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    
    B PIPES_TYPE;

    pos          PLS_INTEGER :=0;
    n            PLS_INTEGER;
    current     PLS_INTEGER;
    
BEGIN

   n := A.count;
 
   Matches.extend(n);
   Subject_Segs.extend(n);
   Order_array.extend(n);
   Dummy_array.extend(n);
       
    -- Convert vertices to segment numbers
    for ii in 1..n loop
      Subject_Segs(ii) := A(ii).Seg;
      Matches(ii) := A(ii).Mseg;   
      Order_array(ii) := ii;
      Dummy_array(ii) := 1000+ii;
--      dbms_output.put_line('SS ' || subject_segs(ii) || ' found with ' || matches(ii));
    end loop;

-- This is a special sort, SS ascending, Matches descending (generally)

   GZ_Pipes.Shellsort4(Subject_segs,Matches, Order_Array,Dummy_Array,1,n);
   
   For ii in 1..A.count Loop
      current := Order_Array(ii);
      pos := pos+1;
      B(pos) := A(current);

   End Loop;
--    for ii in 1..n loop
--      dbms_output.put_line('SS ' || B(ii).SS || ' found with ' || B(ii).matches);
--    end loop;
    
  RETURN B;
  
END SORT_PIPES_OBJ;
--
procedure sort_also(Arr in out nocopy Mdsys.sdo_list_type,Order_array in out nocopy mdsys.sdo_list_type) as

-- Sort an array using an array to reorder it.

--  new          mdsys.sdo_list_type := mdsys.sdo_list_type(5,4,3,2,1);
--  order_array  mdsys.sdo_list_type := new;
--  sort_also(new,order_array) produces new contains 1,2,3,4,5  

-- Used to help after one or more arrays have already been sorted by shellsort2.

      temp   Mdsys.sdo_list_type := Arr;
      
begin
      
      for ii in 1..Arr.count loop
        Arr(ii) := temp(Order_array(ii));
      end loop;
end;
  --
FUNCTION C_TAN(InX NUMBER)  RETURN NUMBER Deterministic IS
/*
**************************************************************************************
--
--Program Name: c_tan
--Author: Sidey Timmins
--Creation Date: 12/27/2006
--Updated: March 31/2010 To better handle very small arguments
--
--Usage:
  -- Call this program from inside another PL/SQL program.
  --
  --   REQUIRED Parameter:
  --            Input:            X:  X input value (in radians)
  --
--Purpose:   -- Calculate tan(x) - the tangent function as fast as possible
--              (4 times faster than the built in functions that take 1.5 seconds
--              for 20000 tan values) with about 14 digits of accuracy. Worst is
--              15 digits of accuracy near zero and pi.
--
-- Accuracy: Over the range - two pi to two pi by 1/10000 the maximum errors are:
--
-- Reference: http://cache-www.intel.com/cd/00/00/29/37/293747_293747.pdf
--    original url = http://www.intel.com/cd/ids/developer/asmo-na/eng/293747.htm
--  The title of the pdf is:
-- "Slerping Clock Cycles" JMP van Waveren, Id Software, Inc
-- For cosine code
-- http://www.gansle.com/approx/approx.pdf
--            "A Guide to Approximations" by Jack G Ganssle

--Dependencies: None.   Also see c_test_tan for rigorous testing.
***************************************************************************************
*/

  X         NUMBER             := InX;

  twopi     NUMBER             := 6.2831853071795864769252867665590057684;
  pi        NUMBER             := 3.1415926535897932384626433832795028842;
  piByTwo   NUMBER             := 1.5707963267948966192313216916397514421;
  piBy4     NUMBER             := 0.78539816339744830961566084581987572105;
  fourbypi  NUMBER             := 1.27323954473516268615107010698011489627;
  pi3By2    NUMBER             := 4.7123889803846898576939650749192543263;
  tanx      NUMBER;
  x2        NUMBER;
  octant    INTEGER;
  big       NUMBER := 1.E120;
  diff      NUMBER;

  -- these coefficients are optimized to give accurate results for the tangent 
  -- throughout the range [-2pi to 2pi]. Hart TAN 4244: page 216
  c1        NUMBER             := -34287.4662577359568109624;
  c2        NUMBER             := 2566.7175462315050423295;
  c3        NUMBER             := -26.5366371951731324538;
  c4        NUMBER             := -43656.1579281292375769579;
  c5        NUMBER             := 12244.4839556747426927793;
  c6        NUMBER             :=  -336.611376245464339493;


BEGIN

  x := mod(x,twopi);
  if x < 0.0 then
    x := x + twopi;
  end if;
  octant := trunc(x/piby4);

  diff := pibytwo - mod(x,pi);

  if abs(diff) < 1.E-20 then

-- Should return infinity but this will have to do..
     If diff > 0.0 then
       RETURN big;
     Else
       RETURN -big;
     End if;
  end if;

  if octant = 0 then
    NULL;
  elsif octant = 1 then
    x := pibytwo - x;
  elsif octant = 2 then
    x := x - pibytwo;
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

  if abs(x) < 0.000001 then
     tanx := x;
  else
    x := x * fourbypi;
    x2 := x*x;
    tanx := (x*(c1 + x2*(c2 + x2*c3))/(c4 + x2*(c5 + x2*(c6 + x2))));
  end if;

  octant := mod(octant,4);
  IF octant = 1 or octant = 2 then
    tanx := 1./tanx;
  END IF;

  IF octant > 1 then
    tanx := -tanx;
  END IF;

  RETURN tanx;

END C_TAN;
--
FUNCTION GET_XYS( Geom IN MDSYS.SDO_GEOMETRY, 
                   pring1 PLS_INTEGER,pseg1_to_find PLS_INTEGER default 0,
                   pring2 PLS_INTEGER DEFAULT NULL, pseg2_to_find PLS_INTEGER default NULL,
   convert VARCHAR2 default '8265'
) RETURN sdo_geometry AS

--------------------------------------------------------------------------------
--Program Name: GetXys
--Author: Sidey + Matt!
--Creation Date:  June? 2009
--Updated:      4/1/20111 To allow absolute segments when ring is zero
--              Feb 5/2010 to return a range of segments and preserve SRID: Sidey
--
--Usage:
-- validate gives : 13349 [Element <1>] [Ring <1>][Edge <20028>][Edge <20040>]
-- select MATTOOL.get_xys(a.sdogeometry,1,20028,NULL,20040) from
-- j12802711008001_srcwrk a where a.oid = 4453141
--
--Purpose: Returns a ring, a segment, 2 segments or a range of segments from a geometry
--
--Method: Makes a new geometry
--Dependencies: None
--
--------------------------------------------------------------------------------


    XYOrd             MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Info              MDSYS.SDO_ELEM_INFO_ARRAY;
    geometry          MDSYS.SDO_GEOMETRY;

    xlast     NUMBER;
    ylast     NUMBER;
    xnew      NUMBER;
    ynew      NUMBER;

    k         PLS_INTEGER := -1;
    ring1     PLS_INTEGER := pring1;
    seg1_to_find   PLS_INTEGER := pseg1_to_find;
    ring2     PLS_INTEGER := pring2;
    seg2_to_find   PLS_INTEGER := pseg2_to_find;
    rings     PLS_INTEGER;
    LB        PLS_INTEGER;
    UB        PLS_INTEGER;
    j         PLS_INTEGER;
    kount     PLS_INTEGER :=0;
    next      PLS_INTEGER;

    GTYPE     NUMBER:= geom.SDO_GTYPE;
    SRID      NUMBER:= geom.SDO_SRID;

    retval    SDO_GEOMETRY;

    procedure print_all_digits as
    
    begin
         -- See full precision of coordinates in all their detail

     if UPPER(convert) = 'PRINT' then
        for ii in TRUNC((LB-1)/2)..TRUNC((LB+kount-2)/2) loop
           dbms_output.put_line(XYOrd(ii*2+1)||','||XYOrd(ii*2+2)||',');
        end loop;
     end if;
    
    end;

BEGIN

     If Gtype = 2001 or Gtype = 2004 then
       dbms_output.put_line('forget about it');
       RETURN NULL;
     End If;

     If convert <> '8265' and UPPER(convert) <> 'PRINT' then
        SRID :=NULL;
     end if;

     Info := geom.sdo_elem_info;
     XYORD := geom.sdo_ordinates;
     rings := TRUNC(Info.count/3);  -- number of rings (1003 or 2003)

     if seg1_to_find = 0 then
       seg1_to_find := -1; 
       ring2 := ring1;
       if ring1*3 = Info.count then
         seg2_to_find := TRUNC(XyOrd.count/2);
       else
         seg2_to_find := TRUNC(Info(ring1*3+1)-1)/2;       
       end if;
       seg2_to_find := seg2_to_find - TRUNC(Info(ring1*3-2)+1)/2;
       dbms_output.put_line('seg1 ' || seg1_to_find || ' seg2 ' || seg2_to_find);
     end if;
--   If the ring is not specified

     if ring1 = 0 then
       next := 1;
       While next+3 < Info.count and 2*Abs(seg1_to_find) > Info(next+3) loop
          next := next +3;
       End loop;
       ring1 := TRUNC(next/3) +1;
       seg1_to_find := seg1_to_find - TRUNC(Info(next)/2);
     end if;
-- if the 2nd ring is not specified
     if ring2 = 0 then
       next := 1;
       While next+3 < Info.count and 2*Abs(seg2_to_find) > Info(next+3) loop
          next := next +3;
       End loop;
       ring2 := TRUNC(next/3)+1;
       seg2_to_find := seg2_to_find - TRUNC(Info(next)/2);
     end if;
     j := (ring1-1) *3 + 1;
     LB := Info(j) + (abs(seg1_to_find) -1) *2;    --  ( I presume he gives the relative segment number)
--     dbms_output.put_line('ring ' || ring1 || ' CN ' || LB);

     if LB +3 > XYOrd.count then LB := XYOrd.count -3; end if;
      if LB < 1 or LB +3 > XYOrd.count then
        RETURN NULL;
     end if;
     xlast := XYOrd(LB);
     ylast := XYOrd(LB+1);
     xnew := XYOrd(LB+2);
     ynew := XYOrd(LB+3);
     kount :=4;
     
     IF ring2 IS NOT NULL
     THEN
        j := (ring2-1) *3 + 1;
     END IF;

     if seg2_to_find is NULL or ( seg1_to_find = seg2_to_find and ring1=ring2) then
       retval := sdo_geometry (2002, SRID, null,
                              sdo_elem_info_array (1,2,1),
                              sdo_ordinate_array (xlast,ylast, xnew,ynew));
-- Return a range:
     elsif (seg1_to_find < 0 or seg2_to_find < 0) and ring1 = ring2 then

        kount := ((abs(seg2_to_find) - abs(seg1_to_find)) +2) *2;
        if kount > XyOrd.count then
          kount := XyOrd.count;
        end if;
--        dbms_output.put_line('kount ' || kount);
        For ii in 1..kount Loop
           XYOrd(ii) := XYOrd(LB+ii-1);
        End Loop;

        LB :=1;
        XYOrd.trim(XYOrd.count-kount);
        
        if xyord(1) = xyord(Xyord.count-1) and xyord(2) = xyord(xyord.count) then
          retval := sdo_geometry (2003, SRID, null,
                              sdo_elem_info_array (1,1003,1), XYOrd);
        else
          retval := sdo_geometry (2002, SRID, null,
                              sdo_elem_info_array (1,2,1), XYOrd);
        end if;
     else
        print_all_digits;
        LB := Info(j) + (seg2_to_find -1) *2;    --  ( I presume he gives the relative segment number)

        retval := sdo_geometry (2006, SRID, null,
                              sdo_elem_info_array (1,2,1, 5,2,1),
                              sdo_ordinate_array (xlast,ylast, xnew,ynew,
                              XYOrd(LB),XYOrd(LB+1),XYOrd(LB+2),XYOrd(LB+3)));
                  
     end if;
     
     print_all_digits;

     RETURN retval;

END Get_Xys;
--
--
Function LPADV(input NUMBER,width NUMBER,format VARCHAR2 default NULL) RETURN VARCHAR2 AS
-- Left pad input with blanks (if necessary) so it has a certain width
   output   VARCHAR2(100) := input;
   n        PLS_INTEGER;
Begin
   
   if format is NOT NULL then
      output := to_char(input,format);
   end if;
   if length(output) < width then
      n := width - length(output);
      output :=SUBSTR('                                          ',1,n)|| output;
   end if;
   Return output;
end;
   --
Function RPADV(input NUMBER,width NUMBER,format VARCHAR2 default NULL) RETURN VARCHAR2 AS
-- Right pad input with blanks (if necessary) so it has a certain width
   output   VARCHAR2(100) := input;
   n        PLS_INTEGER;
Begin
   if format is NOT NULL then
      output := to_char(input,format);
   end if;
   if length(input) < width then
      n := width - length(input);
      output := output || SUBSTR('                                          ',1,n);
   end if;
   Return output;
end;
   --
PROCEDURE UNPACK_GEOMETRY(pInTable VARCHAR2,pInIDColumn VARCHAR2,pInGeomColumn VARCHAR2,pOutTable VARCHAR2,Renumber BOOLEAN default FALSE) AS
/**
##############################################################################################################
# Program Name: unpack_geometry
# Author: Sidey Timmins
# Creation Date: 06/01/07
#
# Usage:
#    This PL/SQL procedure has 5 parameters:
#                    pInTable: table to be converted
#                    pInIdColumn: a column containing a unique id
#                    pInGeomColumn: the SDOGEOMETRY column to unpack
#                    pOutTable: a new output table name that will be created
#                    Renumber: when TRUE, rings from each id are renumbered
#
# Purpose: This procedure makes a new output table with 2007 geometries
#          unpacked.  New records are made containing single ring geometries.
#          Indexes are built on the columns.
#          If parameter Renumber is set TRUE, the Id column is incremented by a
#          fraction (0.0001) for each subgeometry. (For example if the input
#          Id was 1234 the output ids would be 1234.0001, 1234.0002, ..)
#
# Restriction:
# Dependencies: Uses Mbr_List_Obj Type
#
##############################################################################################################
*/
    TYPE GEOM_ARRAY    IS VARRAY(1048576) OF MDSYS.SDO_GEOMETRY;
    TYPE TblCursorType IS REF CURSOR;
    
    TYPE            LIST_ARRAYS is VARRAY(1000) OF MDSYS.SDO_LIST_TYPE;
    MBRZ            LIST_ARRAYS := LIst_Arrays();
    Table_cursor TblCursorType;

   
    InTable         VARCHAR2(200) := UPPER(pInTable);
    InIdColumn      VARCHAR2(30)  := UPPER(pInIdColumn);
    InGeomColumn    VARCHAR2(30)  := UPPER(pInGeomColumn);
    OutTable        VARCHAR2(200) := UPPER(pOutTable);

    Ids             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    OutIds          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    XLLs            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    YLLs            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    XURs            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    YURs            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    MBRs            MDSYS.SDO_LIST_TYPE;
    InGeomArray           GEOM_ARRAY;
    OutGeomArray          GEOM_ARRAY := GEOM_ARRAY();
 
    MBRgeom               MDSYS.SDO_GEOMETRY;
    MBRgeom_ordinates     MDSYS.SDO_ORDINATE_ARRAY;

    Curr_SdoGeom          MDSYS.SDO_GEOMETRY;
    g                     MDSYS.SDO_GEOMETRY;
    sql_stmt        VARCHAR2(4000);
    schema         VARCHAR2(30);
    Row_limit       PLS_INTEGER := 50;  -- we only get 50 geometries at a time
    no_to_output    PLS_INTEGER := 50;
    ElemNum         PLS_INTEGER;
    fraction        NUMBER := 0.0001;
    next            PLS_INTEGER := 0;
    kount           PLS_INTEGER := 0;
    xLL             NUMBER;
    yLL             NUMBER;
    xUR             NUMBER;
    yUR             NUMBER;

BEGIN
  -----------------------------------------------------------------------------
 
   -- New table creation
    
    sql_stmt := 'CREATE TABLE ' || OutTable || ' NOLOGGING AS ' ||
      'SELECT z.'||InIdColumn||',z.myobj.xmin xLL, z.myobj.ymin yLL, z.myobj.xmax xUR, z.myobj.ymax yUR,z.myobj.MBRs MBRS,z.'||InGeomColumn ||' FROM (' ||
      'SELECT /*ORDERED*/ g.'||InIdColumn||',MBR_LIST_OBJ(GZ_PIPES.SET_GEOM_MANY_MBR(g.'|| InGeomColumn ||')) myobj,g.'|| InGeomColumn ||
      ' from '||InTable || ' g ' ||
      'WHERE g.' || InGeomColumn ||'.sdo_gtype <> 2007) z'; 
    
    EXECUTE IMMEDIATE sql_stmt;
    
    OutGeomArray.extend(no_to_output);
    OutIds.extend(no_to_output);
    xLLs.extend(no_to_output);
    yLLs.extend(no_to_output);
    xURs.extend(no_to_output);
    yURs.extend(no_to_output);
    MBRZ.extend(no_to_output);

 
    sql_stmt := 'SELECT g.' || InIdColumn || ',g.' ||InGeomColumn ||
                          ' FROM '  || InTable || ' g ' ||
                          ' WHERE g.' || InGeomColumn ||'.sdo_gtype = 2007 ';   

    OPEN Table_cursor FOR sql_stmt;


---------------LOOP OVER MULTI RING GEOMETRIES -----------------

  LOOP -- the following statement fetches '50' rows in each iteration
   FETCH Table_cursor BULK COLLECT INTO Ids,InGeomArray LIMIT Row_limit;

   FOR ii in 1..InGeomArray.count LOOP

     g := InGeomArray(ii);
     ElemNum := SDO_UTIL.GetNumElem(g);
-------------------------------------------------------
-- Cycle through the SDO_GEOMETRY and extract each complete element
     For i IN 1 .. ElemNum Loop
      next := next + 1;
      OutIds(next) := Ids(ii);
      IF Renumber = TRUE Then
         OutIds(next) := OutIds(next) + i * fraction;
      End If;
      Curr_Sdogeom :=  SDO_UTIL.EXTRACT(g,i);
      OutGeomArray(next) := Curr_SdoGeom;
      MBRs := SET_GEOM_MANY_MBR(Curr_SdoGeom);
 
      xLLs(next) := MBRs(1);
      yLLs(next) := MBRs(2);
      xURs(next) := MBRs(3);
      yURs(next) := MBRs(4);
      MBRZ(next) := MBRS;
-- Output every so often
      IF next = no_to_output THEN
         FORALL k IN 1..no_to_output
         EXECUTE IMMEDIATE
            'INSERT /*+ APPEND */ INTO ' || OutTable || ' ('  || InIdColumn ||',xLL,yLL,xUR,yUR,MBRS,' ||
            InGeomColumn || ') VALUES (:1,:2,:3,:4,:5,:6,:7) '
                     using OutIds(k),xLLs(k),yLLs(k),xURs(k),yURs(k),MBRZ(k),OutGeomArray(k);
         next := 0;
      END IF;
     End Loop;

     END LOOP;
  
      If InGeomArray.count <> 0 then
        NULL;
      Else
        OutIds.trim(no_to_output-next);
        OutGeomArray.trim(no_to_output-next);
        no_to_output := next;
      End If;
-- Insert geometries in batches to reduce the number of transactions
    IF next = no_to_output and next <> 0 THEN

       FORALL k IN 1..no_to_output
       EXECUTE IMMEDIATE
            'INSERT /*+ APPEND */ INTO ' || OutTable || ' ('  || InIdColumn ||',xLL,yLL,xUR,yUR,MBRS,' ||
            InGeomColumn || ') VALUES (:1,:2,:3,:4,:5,:6,:7) '
                     using OutIds(k),xLLs(k),yLLs(k),xURs(k),yURs(k),MBRZ(k),OutGeomArray(k);
      next := 0;
    END IF;

    EXIT WHEN inGeomArray.COUNT = 0;

  END LOOP;
  commit;
  
  sql_stmt := 'CREATE INDEX ' || OutTable || '_IDX on ' || OutTable ||'('||InIdColumn||')';
  EXECUTE IMMEDIATE sql_stmt;
  sql_stmt := 'CREATE INDEX ' || OutTable || '_XLIDX on ' || OutTable ||'(XLL)';
  EXECUTE IMMEDIATE sql_stmt;
  sql_stmt := 'CREATE INDEX ' || OutTable || '_YLIDX on ' || OutTable ||'(YLL)';
  EXECUTE IMMEDIATE sql_stmt;
  sql_stmt := 'CREATE INDEX ' || OutTable || '_XUIDX on ' || OutTable ||'(XUR)';
  EXECUTE IMMEDIATE sql_stmt;
  sql_stmt := 'CREATE INDEX ' || OutTable || '_YUIDX on ' || OutTable ||'(YUR)';
  EXECUTE IMMEDIATE sql_stmt;
  
  
  sql_stmt := 'SELECT USER from dual';
   Execute immediate sql_stmt into schema;
   
-- This takes a while..to do the spatial index

  GZ_SUPER.add_geom_metadata(OutTable,InGeomColumn,schema,8265);
  sql_stmt := 'CREATE INDEX ' || OutTable || '_SPIDX on ' || OutTable ||'('||InGeomColumn||') IndexType is mdsys.spatial_index';
  EXECUTE IMMEDIATE sql_stmt;  
  
END UNPACK_GEOMETRY;
--
procedure test_ring_area as

  Xys      mdsys.sdo_ordinate_array;
  Xys2     mdsys.sdo_ordinate_array;
  New_Xys  mdsys.sdo_ordinate_array;
  Xycopy   mdsys.sdo_ordinate_array:= mdsys.sdo_ordinate_array();
  ring_xys mdsys.sdo_ordinate_array:= mdsys.sdo_ordinate_array();
  ring_xys2 mdsys.sdo_ordinate_array:= mdsys.sdo_ordinate_array();
  drop_it  mdsys.sdo_list_type := mdsys.sdo_list_type();
  miss     mdsys.sdo_list_type;
  new_miss     mdsys.sdo_list_type;
  geom     mdsys.sdo_geometry;
  geom2     mdsys.sdo_geometry;
  sql_stmt VARCHAR2(4000);
  pos      pls_integer;
  vtx      pls_integer;
  oarea    number;
  area     number;
  oldarea  number;
  diff     number;
  x1       number;
  y1       number;
  x2       number;
  y2       number;
  x3       number;
  y3       number;
  x4       number;
  y4       number;
  before_area number;
  area_lost number :=0.0;
  max_diff number :=0.0;
  
  procedure remove_vtx(vtx number, Xys in out nocopy mdsys.sdo_ordinate_array) as
  
  begin
  
     for ii in vtx*2-2..Xys.count-2 loop
        Xys(ii) := Xys(ii+2);
     end loop;
  end;
  
  function compare(Xys1 in out nocopy mdsys.sdo_ordinate_array,Xys2 in out nocopy mdsys.sdo_ordinate_array) return mdsys.sdo_list_type as

-- Compare original (Xys1) with new version (Xys2)  
     missing mdsys.sdo_list_type := mdsys.sdo_list_type();
     found boolean;
     next pls_integer := 0;
     tol   number := 0.000001;
  begin
  
     for ii in 1..TRUNC(XYs1.count/2) loop
        found := false;
        for ij in 1..TRUNC(Xys2.count/2) loop
            if ABS(Xys2(ij*2) - Xys1(ii*2)) < tol and ABS(Xys2(ij*2) - Xys1(ii*2)) < tol then
               found := true;
               exit;
            end if;
        end loop;
        if found = false then
--        dbms_output.put_line('MISSING ' || ii);
           missing.extend(1);
           next := next + 1;
           missing(next) := ii;
        end if;
     end loop;
     return missing;
  end;
  
  function remove(Xys1 in out nocopy mdsys.sdo_ordinate_array,missing in out nocopy mdsys.sdo_list_type) return mdsys.sdo_ordinate_array as
    New_xys   mdsys.sdo_ordinate_array := mdsys.sdo_ordinate_array();
    found     boolean;
    next      pls_integer :=0;
  begin
  new_xys.extend(Xys1.count-missing.count*2);
    for ii in 1..TRUNC(XYs1.count/2) loop
         found:= false;
         for ij in 1..missing.count loop
            if ii = missing(ij) then
               found := true;
--               dbms_output.put_line('skipped'||xys1(ii*2-1) ||','||Xys1(ii*2)||',');
               exit;
            end if;
         end loop;
         if found = false then
         next := next +1;
         New_xys(next) := Xys1(ii*2-1);
         next := next +1;
         New_xys(next) := Xys1(ii*2);
--         dbms_output.put_line('X'||New_xys(next-1) ||','||New_Xys(next)||',');
         end if;
    end loop;
    return new_xys;
  end;
begin

   ring_Xys := mdsys.sdo_ordinate_array(0,0,179.9,0,179.9,90,0,0);
--   ring_Xys := mdsys.sdo_ordinate_array(0,30,0.1,30,0.2,30,0.3,30,0.4,30,0.5,30,0.6,30,0.7,30,0.8,30,0.9,30,1,30,0,30);
  area := ABS(ring_area(ring_Xys));
  
   geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1),ring_xys);
   oarea := sdo_geom.sdo_area(geom,0.05,'unit=sq_meter');
     
--   dbms_output.put_line('  Oracle  area ' || ROUND(oarea,3)|| ' Area     ' || round(area,3)); --|| ' %Error ' || round((oarea-area)/area*100.,3));
--return;
  sql_stmt := 'select gz_QA.get_xys(sdogeometry,1,-36,1,453) from txlaincplace where fullcodel=94006700';
--  sql_stmt := 'select gz_QA.get_xys(sdogeometry,1,-383,1,453) from txlaincplace where fullcodel=94006700';
  execute immediate sql_stmt into geom;
  Ring_xys := geom.sdo_ordinates;
--  sql_stmt := 'select gz_QA.get_xys(sdogeometry,1,-383,1,453) from txlaincplace where fullcodel=94006700';
--  execute immediate sql_stmt into geom;
--  Ring_xys.extend(2);
--  Xys := geom.sdo_ordinates;
--  Ring_xys(ring_xys.count-1) := Xys(1);
--  Ring_xys(ring_xys.count) := Xys(2);
    sql_stmt := 'select gz_QA.get_xys(sdogeometry,1,-543,1,644) from txlaincplace where fullcodel=94006700';
    execute immediate sql_stmt into geom;
    Xys := geom.sdo_ordinates;
    pos := Ring_xys.count;
    Ring_xys.extend(xys.count);
    for ii in 1..Xys.count loop
      Ring_xys(pos+ii) := Xys(ii);
   end loop;
/* 
   ring_xys.extend(20);
   for ii in 1..14 loop
  -- Quad with more sampling on bottom
--   ring_Xys := mdsys.sdo_ordinate_array(0,0,0.1,0,0.2,0,0.3,0,0.4,0,0.5,0,0.6,0,0.7,0,0.8,0,0.9,0,1,0,0,0);
--   ring_Xys := mdsys.sdo_ordinate_array(0,0,0.1,0,0.1,0.1,0,0.1,0,0);
--   ring_Xys := mdsys.sdo_ordinate_array(0,0,0.05,0,0.1,0,0,0);
 --   ring_Xys := mdsys.sdo_ordinate_array(0,0,0.025,0,0.05,0,0.075,0,0.1,0,0,0);
 
   for jj in 1..ring_Xys.count loop
       Ring_Xys(jj) := 0.0;
      if Mod(jj,2) =1 and jj <> 1 and jj <> Ring_xys.count-1 then
       Ring_xys(jj) := (jj+1)/2*0.01;
      elsif MOD(jj,2) = 0 then
         Ring_Xys(jj) := Ring_Xys(jj)+ii*5.;
      end if;
   end loop;
*/
/*
   area := ring_area(Ring_Xys);
   oldarea := quick_ring_area(Ring_Xys);
   x2 := Ring_Xys(1*2-1);
   y2 := RIng_Xys(1*2);
   x3 := Ring_Xys(2*2-1);
   y3 := RIng_Xys(2*2); 
   For ii in 3..TRUNC(Ring_XYS.count/2) loop
    x1:= x2;
    y1 := y2;
    x2 := x3;
    y2 := y3;
 
    x3 := Ring_Xys(ii*2-1);
    y3 := Ring_Xys(ii*2);
    Xys := mdsys.sdo_ordinate_array(x1,y1,x2,y2,x3,y3,x1,y1);
   area := ABS(ring_area(Xys));
  
   geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1),xys);
   oarea := sdo_geom.sdo_area(geom,0.05,'unit=sq_meter');
   
   if area <> 0. and round((oarea-area)/area*100.,3) > 0.002 then
   dbms_output.put_line('Place ' || ii || '  Oracle  area ' || ROUND(oarea,3)); -- || ' Old Area     ' || round(oldarea,3));
   dbms_output.put_line('Place ' || ii || '  Correct area ' || ROUND(Area,3)  || ' Correct Area ' || round(Area,3)  || ' %Error ' || round((oarea-area)/area*100.,3));
   end if;
  end loop;
  return;
  */
  geom := mdsys.sdo_geometry(2002,8265.,null,mdsys.sdo_elem_info_array(1,2,1),Ring_xys);
  geom2 :=  gz_QA.get_xys(geom,1,-274,1,275);
  xys := geom2.sdo_ordinates;
  
  for ii in 1..trunc(xys.count/2) loop
    dbms_output.put_line('x ' || Xys(ii*2-1) ||',' || Xys(ii*2));
  end loop;
--  geom2 := serena_zone.line_simplify(geom,1000.);
--  Ring_xys := Geom2.sdo_ordinates;
  area := ring_area(ring_Xys); 
  geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1),Ring_xys);
 oarea := sdo_geom.sdo_area(geom,0.05,'unit=sq_meter');
  dbms_output.put_line('BEFORE oarea ' || oarea || ' area ' || round(area,12)|| ' count ' ||Ring_xys.count);
  before_area := oarea;

  geom2 := sdo_util.simplify(geom,0.001,0.000005);
  Xys2 := geom2.sdo_ordinates;
  geom2.sdo_srid := 8265.;
   oarea := sdo_geom.sdo_area(geom2,0.05,'unit=sq_meter');
 --  for ii in 1.. ring_xys.count loop
 --     ring_xys(ii) := round(ring_xys(ii),16);
 --   end loop;
   area := ring_area(Xys2);
   
   dbms_output.put_line('AFTER oarea ' || oarea || ' area ' || round(area,12)|| ' count ' ||xys2.count);
   miss := compare(ring_xys,xys2);
   
   ring_xys2.extend(8);
   for ii in 1..miss.count loop
      vtx := miss(ii);
      ring_xys2(1) := Ring_xys(vtx*2-3);
      ring_xys2(2) := Ring_xys(vtx*2-2);
      ring_xys2(3) := Ring_xys(vtx*2-1);
      ring_xys2(4) := Ring_xys(vtx*2);
      ring_xys2(5) := Ring_xys(vtx*2+1);
      ring_xys2(6) := Ring_xys(vtx*2+2);
      ring_xys2(7) := Ring_xys2(1);
      ring_xys2(8) := Ring_xys2(2);
      area := ring_area(ring_Xys2);
      geom2.sdo_ordinates := ring_xys2;
      oarea := sdo_geom.sdo_area(geom2,0.05,'unit=sq_meter');
      area_lost := area_lost + ABS(area);
      dbms_output.put_line('#' || ii || ' At miss ' || miss(ii) ||  ' oarea ' || oarea || ' area lost' || round(area_lost,6));
      
   end loop;
 
   --geom2 := sdo_util.simplify(geom,0.01,0.005);
 --  XYcopy := Ring_Xys;
 --  Ring_Xys.trim(2);

          
 --    geom2.sdo_ordinates := Ring_Xys;
 --    oarea := sdo_geom.sdo_area(geom2,0.05,'unit=sq_meter');
  for ii in 1..miss.count loop
  new_miss := mdsys.sdo_list_type(miss(ii));
  new_Xys :=  remove(Ring_xys,new_miss);
   area := ring_area(New_Xys);
   geom.sdo_ordinates := New_Xys;
    oarea := sdo_geom.sdo_area(geom,0.05,'unit=sq_meter');
   diff := (oarea - before_area);
--   if diff > max_diff then
     max_diff := diff;
     dbms_output.put_line( 'ii ' || ii ||' Diff ' || round(max_diff,3) || ' oarea ' || oarea || ' area ' || round(before_area,6) );
--   end if;
  end loop;
end;

--==============================================================================
-- Utilities
--==============================================================================
FUNCTION TRY_RING_AREA(geom mdsys.sdo_geometry) return number as

  Xys   mdsys.sdo_ordinate_array;
  Info  mdsys.sdo_elem_info_array;

area   number;

begin

  Xys := geom.sdo_ordinates;
  Info := geom.sdo_elem_info;
  
  area := ring_area(Xys);
  return area;

end;

PROCEDURE TRY_QUICK_SINCOS AS

  rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
  x      NUMBER;
  big    NUMBER :=0.0;
  bigrel NUMBER := 0.0;
  bad    PLS_INTEGER;
  badrel PLS_INTEGER;
  cosx   NUMBER;
  sinx   NUMBER;
  ii     NUMBER;
  time1      date;
  
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
BEGIN

 time1 := current_timestamp;
  for i in 0..3150 loop
      ii := i*0.1;
       if ii = -126 then
          x := -4.*acos(-1.);
       elsif ii = 126 then
          x := 4*acos(-1.);
       else
          x := ii/100.;
       end if;
--       sinx := sin(x);
--       cosx := cos(x); --sqrt(1-sinx*sinx);
       sinx := GZ_PIPES.quick_sincos(x,cosx);

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
--     dbms_output.put_line('angle ' || RPAD(ROUND(x*rad2deg,1),6) ||' ii '  || Rpad(x,4) || ' Qsine ' || RPAD(round(sinx,10),12) || '  sin ' || RPAD(round(sin(x),10),12)  || ' diff ' || RPADV(TO_CHAR(round(1.-sinx/sin(x),20),'99.999EEEE'),23));
--     end if;

 end loop;
 dbms_output.put_line('biggest absolute error' || RPADV(round(big,22),24,'999.99999EEEE') || ' bad ' || bad);
 dbms_output.put_line('biggest relative error' || RPADV(round(bigrel,22),24,'999.99999EEEE') || ' bad ' || badrel);
 dbms_output.put_line((current_timestamp-time1));

END TRY_QUICK_SINCOS;

Procedure test_lesser_measure as

   Info      mdsys.sdo_elem_info_array := mdsys.sdo_elem_info_array(1,1003,1);
   result   pls_integer :=0;
begin


  result :=  lesser_measure(9,3,Info,20);
  dbms_output.put_line('result ' || result);
  result :=  lesser_measure(3,9,Info,20);
  dbms_output.put_line('result ' || result);
   result :=  greater_measure(3,9,Info,20);
  dbms_output.put_line('result ' || result);
  
  Info(1) := Info(1)+1000;
  
  
  result :=  lesser_measure(509,503,Info,1020);
  dbms_output.put_line('result ' || result);
  
  result :=  lesser_measure(503,509,Info,1020);
  dbms_output.put_line('result ' || result);
  
  
 
  return;  
  result :=  close_measure(9,3,Info,20);
  dbms_output.put_line('result ' || result);
  result :=  close_measure(3,9,Info,20);
  dbms_output.put_line('result ' || result);
  
  
  Info(1) := Info(1)+1000;
  
  
  result :=  lesser_measure(509,503,Info,1020);
  dbms_output.put_line('result ' || result);
  
  result :=  greater_measure(503,509,Info,1020);
  dbms_output.put_line('result ' || result);
   
  result :=  close_measure(509,503,Info,1020);
  dbms_output.put_line('result ' || result);
  result :=  close_measure(503,509,Info,1020);
  dbms_output.put_line('result ' || result);

--  
  result :=  lesser_measure(507,503,Info,1020);
  dbms_output.put_line('result ' || result);
  
  result :=  greater_measure(503,507,Info,1020);
  dbms_output.put_line('result ' || result);
  
  result :=  close_measure(507,503,Info,1020);
  dbms_output.put_line('result ' || result);
  result :=  close_measure(503,507,Info,1020);
  dbms_output.put_line('result ' || result);
end;
procedure test_sort_also as

  arr         mdsys.sdo_list_type :=  mdsys.sdo_list_type(10,9,8,7,6,5,4,3,1,2);
  order_arr   mdsys.sdo_list_type :=  mdsys.sdo_list_type(9,10,8,7,6,5,4,3,2,1);
  
  new_arr         mdsys.sdo_list_type;
begin
  new_arr := arr;
  sort_also(new_arr,order_arr);
  for ii in 1..new_arr.count loop
     dbms_output.put_line('ii ' || ii || ' ' || new_arr(ii));
  end loop;
end;
procedure test_check_opening as

      Geom MDSYS.SDO_GEOMETRY;
      XYs     MDSYS.SDO_ORDINATE_ARRAY;
      Info    MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY(1.1003,1);
      
      x    number;
      y    number;
      result  number;
      
      pseg     PLS_INTEGER := 365;
      pmseg    PLS_INTEGER := 498;
      on_pseg  VARCHAR2(1) := 'N';
      way   VARCHAR2(5) := 'LEFT';
      sstart PLS_INTEGER := 1;
      slast  PLS_INTEGER := 491;
      mstart PLS_INTEGER := 493;
      mlast  PLS_INTEGER := 632;
      sql_stmt   VARCHAR2(4000);
      
begin

  sql_stmt := 'select sdogeometry from txlaincplace where fullcodel=:1';
  execute immediate sql_stmt into Geom using 94049550;
  
  Xys := Geom.Sdo_ordinates;
  x := -96.0644304;
  y :=  35.8634891;
 -- result := CHECK_OPENING(Xys,Info,x,y, pseg, pmseg, on_pseg, way);
  
  dbms_output.put_line('Result ' || result);
  
end;
procedure try_set_many_mbr(seg number default 1,pgeom  IN MDSYS.SDO_GEOMETRY default NULL) as
  geom              MDSYS.SDO_GEOMETRY := pGeom;
  XYOrd             MDSYS.SDO_ORDINATE_ARRAY;
  Info_array        MDSYS.SDO_ELEM_INFO_ARRAY:= MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1);
  MBRs              MBRs_TYPE;
  List_pairs        MDSYS.SDO_LIST_TYPE;
  xLL               number;
  yLL               number;
  xUR               number;
  yUR               number;
  temp              pls_integer;
  sql_stmt          VARCHAR2(4000);
  
begin

   if geom is NULL then
 --  geom  := mdsys.sdo_geometry(2002,8265.,NULL, mdsys.sdo_elem_info_array(1,2,1),
 --               mdsys.sdo_ordinate_array(-2,-0.5,-1,-0.3,0,0,0.5,0,1,0,1,1,2,1,2,2,1,2,0,2,0,0.1));
    sql_stmt := 'SELECT sdogeometry from txlaincplace where fullcodel=94863020';
   execute immediate sql_stmt into geom;
   end if;
                
   Xyord := geom.sdo_ordinates;
   Info_array := Geom.sdo_Elem_info;
   
   
   SET_MANY_MBRs(XYord,Info_Array,MBRs,xLL,yLL,xUR,yUR,0,0.00001003);
    for jk in 1..Mbrs.count loop
   if mBRs(jk).mbr_list.count = 0 then
    dbms_output.put_line('jk ' || jk || ' MBR ' || MBRS(jk).start_overlap || ' lo ' || MBRS(jk).lo || ' hi  '|| MBRS(jk).hi || ' list ' || mBRs(jk).mbr_list.count|| ' endp ' || mbrs(jk).endpoints);
 else
    temp :=  mBRs(jk).mbr_list(1);
    dbms_output.put_line('jk ' || jk || ' MBR ' || MBRS(jk).start_overlap || ' lo ' || MBRS(jk).lo || ' hi  '|| MBRS(jk).hi || ' list ' || mBRs(jk).mbr_list.count || ' temp ' || temp|| ' endp ' || mbrs(jk).endpoints);  
 end if;    
 end loop;
--   for ii in 1..MBRs.count loop
--      dbms_output.put_line('ii ' || ii || ' MBR ' || MBRs(ii));
--   end loop;
   
   list_pairs := quiz_mbr(seg,MBrs);
   
   for ii in 1..list_pairs.count loop
   dbms_output.put_line('ii ' || ii || ' LP ' || List_pairs(ii));
   end loop;
end;
--
procedure test_lines(seg1 pls_integer default 1,pseg2 pls_integer default 0) as

geom              MDSYS.SDO_GEOMETRY;
Xys               MDSYS.SDO_ORDINATE_ARRAY;
Info              MDSYS.SDO_ELEM_INFO_ARRAY;
MBRs              MBRs_Type;
List_pairs        MDSYS.SDO_LIST_TYPE;
sql_stmt          VARCHAR2(4000);

n                 PLS_INTEGER;
seg2              PLS_INTEGER := pseg2;
seg               PLS_INTEGER;
next_seg          PLS_INTEGER :=0;
start_seg         PLS_INTEGER;


x1                NUMBER;
y1                NUMBER;
x2                NUMBER;
y2                NUMBER;

x3                NUMBER;
y3                NUMBER;
x4                NUMBER;
y4                NUMBER;

  xLL               number;
  yLL               number;
  xUR               number;
  yUR               number;
  xybuffer          number := 1.2505E-06;
   
   
begin
   
   sql_stmt := 'SELECT sdogeometry from timmi001.acf_hd_test70';
   execute immediate sql_stmt into geom;
   
   
   Xys := Geom.sdo_ordinates;
   Info := Geom.Sdo_Elem_Info;
   
   SET_MANY_MBRs(XYs,Info,MBRs,xLL,yLL,xUR,yUR,0,xybuffer);   
      
    x1 := Xys(seg1*2-1);
    y1 := Xys(seg1*2);
    x2 := Xys(seg2*2-1);
    y2 := Xys(seg2*2);
 
 -- Loop to see if the line we wish to draw across the lake from seg1 to seg2
 -- intersects any other part of the lake
  
    Loop

     if seg >= next_seg then
--     dbms_output.put_line('Calling QUIZ_MBR at seg ' || seg);
       list_pairs := quiz_mbr(seg,MBrs);
       start_seg := TRUNC((list_pairs(1)+1)/2);  -- partial Walk away

       next_seg := TRUNC(ABS(list_pairs(2))/2); 
       if list_pairs.count=2 and list_pairs(2) < 0 then  -- complete walk away
          start_seg := next_seg;
       end if;
--       dbms_output.put_line('Start_seg ' || start_seg || ' Next seg set to ' || next_seg || ' LP count ' || List_pairs.count);
       for kk in 1..trunc(list_pairs.count/2) loop
--       dbms_output.put_line('BeforeLP ' || list_pairs(kk*2-1) || ' lp ' || list_pairs(kk*2) || ' xys ' || xys.count);
         list_pairs(kk*2-1) := TRUNC((list_pairs(kk*2-1)+1)/2);
         list_pairs(kk*2) := TRUNC(ABS(list_pairs(kk*2))/2)-1;
--       dbms_output.put_line('LP ' || list_pairs(kk*2-1) || ' lp ' || list_pairs(kk*2) || ' xys ' || xys.count);
       end loop;
     end if;
  dbms_output.put_line('x1 ' || x1 || ' x2 ' || x2);  
  
   -- Check if this line intersects another segment
     x3 := Xys(seg*2-1);
     y3 := Xys(seg*2);
     x4 := Xys(seg*2+1);
     y4 := Xys(seg*2+2);
     
 --   
     if seg_intersect(x1,y1,x2,y2,x3,y3,x4,y4) then
     
      -- Ignore if the segment attaches to our anchors
      
        if seg = seg1+1 or seg = seg1-1 or seg = seg2+1 or seg= seg2-1 then
           continue;
        end if;
        dbms_output.put_line('Intersect at ' || seg);
     
     end if;
   end loop;
end;

procedure test_get_widths as

   geom       mdsys.sdo_geometry;
   Xys        mdsys.sdo_ordinate_array;
   geom2      mdsys.sdo_geometry;
   Xysnew     mdsys.sdo_ordinate_array;
   widths     mdsys.sdo_list_type;
begin

    execute immediate 'select sdogeometry from txlaincplace where fullcodel=:1'
        into geom using 94863020;
 
  
    for ii in 1..51 loop --trunc(xys.count) loop
    
      geom2 := get_rings(geom,mdsys.sdo_list_type(1,2,3),mdsys.sdo_list_type(0,ii,0));
    
      widths := get_widths(geom2);
      
      if widths is null then
         dbms_output.put_line('at shift ' || ii || ' nothing found');
      
      end if;
    
    end loop;
end;

END GZ_PIPES;
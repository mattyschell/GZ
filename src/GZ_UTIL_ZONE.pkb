create or replace
PACKAGE BODY GZ_UTIL_ZONE AS
-- Updated 5/4/2012 Some nice cleanup and now verifies non closed loops and
--                  fixes them when they self intersect. Zone may be easily 
--                  controlled with nice (even set to 0.9).
-- Updated 2/1/2012 With Improvements to River code filter lengths for edges with
-- too few vertices per length
PROCEDURE RUN_SIMPLIFY(In_Table VARCHAR2,scale number,nice number,out_table VARCHAR2) AS

  Edge_ids      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Geom          MDSYS.SDO_GEOMETRY;
  Out_GEOM      MDSYS.SDO_GEOMETRY;
  sql_stmt      VARCHAR2(4000);
  sql_stmt2      VARCHAR2(4000);

BEGIN
   sql_stmt := 'CREATE TABLE '||out_table ||' (edge_id NUMBER, geometry MDSYS.SDO_GEOMETRY) NOLOGGING';
   EXECUTE IMMEDIATE sql_stmt;
  
   sql_stmt := 'SELECT EDGE_ID FROM '||IN_table;
  
   EXECUTE IMMEDIATE sql_stmt BULK COLLECT into Edge_ids;
   
   
   sql_stmt := 'SELECT GEOMETRY FROM '||IN_table ||' WHERE EDGE_ID=:1';

   
   sql_stmt2 := 'INSERT INTO '||out_table||' (edge_id,geometry) Values(:1,:2)';
    
   For ii in 1..Edge_ids.count Loop
      
     EXECUTE IMMEDIATE sql_stmt INTO Geom using Edge_ids(ii);
     
     out_geom := line_simplify(geom,scale,nice,id=>edge_ids(ii));
     
     EXECUTE IMMEDIATE sql_stmt2 using Edge_ids(ii),out_geom;
     
     if mod(ii,100)=50 then
       COMMIT;
     end if;
     
   End Loop;
   
   commit;
END;
FUNCTION  LINE_SIMPLIFY(In_Geometry  MDSYS.SDO_GEOMETRY,
                        InScale NUMBER default 500000.,
                        In_nice_factor NUMBER default 0.0,Inminimum_len NUMBER default 0.,
                        Inlen NUMBER default 0.,ID NUMBER default 0.,InRiver_param NUMBER default -1.,InTopology VARCHAR2 default NULL,Intarget_scale NUMBER default 0.0) RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC AS


   Start_zone       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   In_zone          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Order_zone       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Mean_zone        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Zone_constant    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Deviations       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Max_Deviations   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Area_under_Curve MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   self_segs         MDSYS.SDO_LIST_TYPE;

   XYs               MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   EndXYs            MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   Xes               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Yes               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Xess              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Yess              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   bearings          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   vertex            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   keep_it           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Gkeep_it          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Start_Elements    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   result           MDSYS.SDO_LIST_TYPE;


   GenXYArray        MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   XYArray           MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   XYArray2          MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();

   Info              MDSYS.SDO_ELEM_INFO_ARRAY;
   Info_Array        MDSYS.SDO_ELEM_INFO_ARRAY:= MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1);
   PolyInfo_Array    MDSYS.SDO_ELEM_INFO_ARRAY:= MDSYS.SDO_ELEM_INFO_ARRAY();

   B_to_V            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   quads             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(0.,0.,0.,0.,0.,0.,0.,0.);
   geometry          MDSYS.SDO_GEOMETRY;
   poly_geometry     MDSYS.SDO_GEOMETRY;
   Gen_geometry      MDSYS.SDO_GEOMETRY;
   geometry2         MDSYS.SDO_GEOMETRY;
   geometry_out     MDSYS.SDO_GEOMETRY;
   deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
   measure           Number := 100.; --now in meters --0.0006;  -- was 0.001 -- was 0.0002
   the_time          timestamp;
   In_start         PLS_INTEGER;
   In_End           PLS_INTEGER;
   Inn_start         PLS_INTEGER;
   Inn_End           PLS_INTEGER;
   Nzone            NUMBER;
   n                PLS_INTEGER;
   start_z          NUMBER;
   freq             PLS_INTEGER;
   pos              PLS_INTEGER;
   pos1             PLS_INTEGER;
   pos2             PLS_INTEGER;
   pos3             PLS_INTEGER;
   next             PLS_INTEGER;
   counter          PLS_INTEGER := 0;
   enough           PLS_INTEGER := 100;
   loop_count       PLS_INTEGER := 1;
   area             NUMBER := 0.0;
   dist             NUMBER;
   dist2            NUMBER;
   deviation        NUMBER;
   siny             NUMBER;
   cosy             NUMBER;
   cos2y            NUMBER;
   y                NUMBER;
   temp             NUMBER;
   XLL              NUMBER;
   XUR              NUMBER;
   YLL              NUMBER;
   YUR              NUMBER;
   threshold2       NUMBER;
   Straight_len     NUMBER;
   Straight_Angle   NUMBER := 177.; --177.;
   Nearly_str_angle NUMBER := 175.;
   thousand         NUMBER := 10000000.;
   mbr_area         NUMBER;
   oarea            NUMBER;
   tolerance        NUMBER := 2.5;
   Minimum_len      NUMBER := InMinimum_len;
   nice             NUMBER := In_nice_factor;   

   check_length     NUMBER;
   check_area       NUMBER;
   end_length       NUMBER;
  odistance        NUMBER;
  sql_stmt         VARCHAR2(4000);
  mbig NUMBER;
  msmall NUMBER;
  mlastsmall number;
  mlastbig   number;
  mbest PLS_INTEGER;
  m                 PLS_INTEGER;
  kount             PLS_INTEGER;
  jj                PLS_INTEGER;
  jstart            PLS_INTEGER := 1;
  notodo            PLS_INTEGER;
  gtype             NUMBER := 2001;  -- We don't expect this gtype
  srid              NUMBER;
  one               NUMBER := 1.;
  zero              NUMBER :=0.;
  dx                NUMBER;
  dy                NUMBER;
     x0             NUMBER;
     y0             NUMBER;
     x1             NUMBER;
     y1             NUMBER;
     x2             NUMBER;
     y2             NUMBER;
     x3             NUMBER;
     y3             NUMBER;
     x4             NUMBER;
     y4             NUMBER;
     xlast          NUMBER;
     ylast          NUMBER;
     dhold          NUMBER:= 0.001;
     x_intersect    NUMBER;
     y_intersect    NUMBER;
     distance       NUMBER;
     length         NUMBER;
     d              NUMBER;
     bearing0       NUMBER;
     bearing1       NUMBER;
     bearing2       NUMBER;
     included_angle NUMBER;
     last_included_angle NUMBER := 0;
     perimeter        NUMBER;
     gen_length     NUMBER;
     last_area      NUMBER;
     area_above     NUMBER;
     area_below     NUMBER;
     indexp         NUMBER;
     last_angle     NUMBER := 0.;
     bendiness      NUMBER := 0.;
     direct_bendiness NUMBER :=0.0;
     not_right_bends NUMBER := 0.;
     right_bends    NUMBER := 0.;
     straight_bends NUMBER := 0.;
     min_allowed_area NUMBER;
     max_allowed_area NUMBER;
     rough_length   NUMBER;
     plratio        NUMBER := 1.;
     p_point_ratio  NUMBER := 1.;
     max_lost_area_fraction  NUMBER;
     max_area       NUMBER := 0.;
     medium_length  NUMBER;
     difference     NUMBER := 2.;
     this_angle     NUMBER;
     end_to_end_angle NUMBER := 180.;
     smallest_angle NUMBER := 999.;
     direct_length  NUMBER;
     closed         BOOLEAN := FALSE;
     ok             BOOLEAN;
     changed        NUMBER;
     its_straight   PLS_INTEGER;
     close_tolerance   NUMBER := tolerance;
     b1             NUMBER;
     b2             NUMBER;
     len            NUMBER := Inlen;
     pred_bcount    NUMBER;
     check_distance NUMBER;
     River_param    NUMBER := InRiver_param;
     scale         NUMBER := Inscale;
     smooth_length   NUMBER := 21.;
     seg1            NUMBER;
     seg2            NUMBER;
     vertex_removed         NUMBER;
     Last_RBend     PLS_INTEGER :=0;
     Current_bend   PLS_INTEGER;
     div             PLS_INTEGER;
     bear_count      PLS_INTEGER := 0;
     allowed_deviation   NUMBER :=   1000.; --1000.; --100 ; --800.;
     max_allowed_deviation   NUMBER :=1500.; --1250.; --200; --1000;
     Min_INZone     PLS_INTEGER :=15;  -- was 30;
     Minimum_In_Zone PLS_INTEGER :=2;
     max_allowed_angle_change NUMBER := 120.;  -- was 120
     geom_is_valid             VARCHAR2(4000) := 'TRUE';
     valid_loop      PLS_INTEGER := 0;
     very_small      BOOLEAN := FALSE;
     nearly_closed   BOOLEAN := FALSE;
     cloops          PLS_INTEGER;
     mult            PLS_INTEGER := 1;
     Nzone_min       PLS_INTEGER :=1;
     mm              PLS_INTEGER;
     last_quad       PLS_INTEGER := 0;
     quad            PLS_INTEGER;
     loops           PLS_INTEGER :=0;
     max_loops       PLS_INTEGER :=4;
     cycles          PLS_INTEGER := 0;
     loop_max        PLS_INTEGER;
     error_msg       VARCHAR2(1000);
     allow_touches   BOOLEAN := FALSE;
BEGIN

--============================================================================== 
 
-- Setup default parameters since Oracle can pass you a NULL and the defaults
-- above only work when the invocation is abbreviated.
   
   If In_Geometry is NULL and InTopology is NULL then
      RETURN NULL;
   end if;
   
 
   if Inscale is NULL or Inscale = 0.0 then Scale := 500000.0; end if;
 
   if Nice is NULL then nice := 1.0; end if;
   
   if Minimum_len is NULL then Minimum_len := 0.0; end if;
   
   if len is NULL then len := 0.0; end if;
   
   if River_param is NULL then River_param := -1.; end if;

--==============================================================================   
   
-- Begin by checking the geometry   
--   the_time := current_timestamp;
-- This is useful because it tells how many times Super called Line Simplify..

--   if Inscale <> Intarget_scale then
--     dbms_output.put_line('FOR id ' || id || ' scale: '||scale || ' nice ' || nice|| ' target ' || INtarget_scale|| ' '|| the_time);
--   end if;
-- Track App Error prints these messages and logs to a tracking table
-- and then raises application error 20001

   if In_geometry is NULL then
      error_msg := ':'||ID||':FATAL ERROR:>>>Geometry is NULL';
      GZ_TOPOFIX.Track_App_Error(error_msg,InTopology,'ZONE');
      RETURN NULL;
   end if;
 
 
   gtype := In_Geometry.sdo_gtype;
   Info := In_Geometry.sdo_elem_info;
   
-- Don't generalize a point or multi point
-- We only want a single "line" or edge

    If gtype = 2002 and Info(3)=1  then
        NULL;
    Else
       If Info(3) <> 1 then
          error_msg :=':'||ID||':FATAL ERROR:>>>BAD Geometry interpretation ' || Info(3) ;
          GZ_TOPOFIX.Track_App_Error(error_msg,InTopology,'ZONE',IN_Geometry);
       else
          error_msg :=':'||ID||':FATAL ERROR:>>>BAD Geometry gtype ' || gtype;
          GZ_TOPOFIX.Track_App_Error(error_msg,InTopology,'ZONE',In_Geometry);
       end if;
      RETURN In_Geometry;
    End if;  
    
    
-- Limit nice factor. Nice is setup so smaller nice values give more detail
-- and bigger nice values give a more generalised shape.

    if nice <= 0.0 then
      nice := 1.0;
    elsif nice > 2.0 then
      nice := 2.0;
   elsif nice < 0.25 then
      nice := 0.25;
   end if;

-- Some limits on the scale 
   
   if scale > 50E6 then scale := 50E6; end if;
   if scale < 10000. then scale := 10000.; end if;


-- Remove the "fluff". This idea was not found to be useful.
--   ok := REMOVE_CLOSE_XYS(in_Geometry,tolerance);
--   if ok = TRUE then
--     dbms_output.put_line('changed');
--   end if;

    srid  := In_Geometry.sdo_srid;
    Xys := In_Geometry.sdo_ordinates;
    XYArray2.extend(4);
    n := Trunc(XYs.count/2);
    m := XYs.count;
    IF XYs(1) = XYs(m-1) and Xys(2) = Xys(m) Then
         closed := TRUE;
    END IF;
    
-- dbms_output.put_line('NO OF VERTICES ' || N || ' scale '|| scale || ' ' ||nice);

-- If the input geometry has 2 vertices, or its a closed triangle,
-- no generalization is possible.

    If n = 2 or (n = 4 and closed = TRUE) Then
        RETURN In_Geometry;
    End if;


--==============================================================================
-- From here on we are going to simplify the line

-- Keep a list of the vertices to keep in keep_it (1=keep, 0=maybe)

     keep_it.extend(n);
     For ii in 1..n Loop
       keep_it(ii) := 1.;
     End Loop;
-- and separate X and Y for convenience into the Xes and Yes arrays

     Xes.extend(n);
     Yes.extend(n);
     pos := 0;
     For kk in 1..n Loop
       pos := pos + 1;
       Xes(kk) := XYs(pos);
       pos := pos +1;
       Yes(kk) := XYs(pos);
     End Loop;
     
     siny := GZ_UTIL_ZONE.sincos(Xys(2)*deg2rad,cosy);
     
-- Just keep a point every 11 km  (was 5.5 km)

     Straight_len := 111120.*cosy * 0.1;
       
-- This also does not seem to be such a good idea for non-closed for some reason.
-- Also upsets the filtering!

--       if closed <> TRUE then
--         GZ_UTIL_ZONE.Remove_Too_Straight(Keep_it,Xes,Yes,Straight_len,Straight_angle,FALSE);
--       end if;

     kount := 0;
     For ii in 1..n Loop
--       if keep_it(ii) <> 0. then  -- These lines go with Remove_Too_Straight above
          kount := kount + 1;
          Xes(kount) := Xes(ii);
          Yes(kount) := Yes(ii);
--          Xys(kount*2-1) := Xes(ii);  -- ditto
--          Xys(kount*2) := Yes(ii);    -- ditto
--       end if;
       keep_it(ii) := 0.;
     End Loop;
--       dbms_output.put_line('kount ' || kount || ' n ' || n);
     keep_it(1) := 1.;
     keep_it(kount) := 1.;
     
     if kount < n then
       Xes.trim(n-kount);
       Yes.trim(n-kount);
       Xys.trim(Xys.count-kount*2);
       Keep_it.trim(Keep_it.count-kount);
       n := kount;
       m := Xys.count;
     end if;
--==============================================================================

-- Presume resolution the eye can resolve is about .02 inches = 0.000508 meters

-- At scale               0.02 inches
-- 500,000          ,   254 meters            
-- 1,000,000            508 meters            
-- 5,000,000            2540 meters           
-- 20,000000           10160 meters  

-- If we were to change this minimumlength to say half of these values, then
-- zone will pick segments that are half as long -- which will choose nearly
-- twice as many vertices!! So changing minimum length has major consequences.

     If Minimum_len is NULL or Minimum_len <= 0. then
       Minimum_len := scale * 0.001016 *nice * 0.5; --0.000508* nice;
     End if;
       
--     dbms_output.put_line('ID ' || ID ||  ' min len ' || ROUND(Minimum_len,6));

-- Apple says we can resolve 326 pixels per inch on a Retinal display.
-- so each pixel is (1/326)*2.54cm *0.01 cm per meter = 0.000077914 meters !!

-- So if we want a line made up of 2-3 pixels  this becomes
-- At scale          This is in meters     2.5 times bigger        0.01 inches
-- 500,000         ,    39 meters              100 meters            127 meters
-- 1,000,000            78 meters              200 meters            254 meters
-- 5,000,000            390 meters             975 meters           1270 meters
-- 20,000000           1558 meters           3900 meters           5080 meters

--==============================================================================

-- If measure becomes smaller - then we can have more in a zone because we are
-- sampling closer. As we go towards the poles, measure needs to increase
-- to describe the same distance: Minimum len.
--       measure := ROUND(measure / cosy,6);

-- Make Measure smaller when scale of the map increases (Scale decreases).
--       measure := measure * Scale/5000000.;

     Inn_start := 1;
     Inn_end := XYs.count;
     EndXys.extend(4);
     EndXys(1) := Xys(1);
     EndXys(2) := Xys(2);
     EndXys(3) := Xys(m-1);
     EndXys(4) := Xys(m);
     length := GZ_UTIL_ZONE.accurate_length(EndXys);
     end_length :=length;
     -- Edge can be a polygon
     
      if length = 0.0 then length := perimeter * 0.5; end if;
      perimeter := GZ_UTIL_ZONE.accurate_length(Xys);
      
--dbms_output.put_line('perimeter ' || ROUND(perimeter,4) || ' end len ' || ROUND(length,4) || ' P/L ' || ROUND(perimeter/length,4) || ' P/point ' || round(p_point_ratio,4) || ' pred_bcount ' || ROUND(pred_bcount,5));

      if perimeter = 0.0 then
        RETURN In_Geometry;
      end if;


-- Here we work out the perimeter to length ratio

--        p_point_ratio := perimeter/n;
      plratio := perimeter/length;
--             dbms_output.put_line('perimeter ' || ROUND(perimeter,4) || ' end len ' || ROUND(length,4) || ' P/L ' || ROUND(perimeter/length,4) || ' P/point ' || round(p_point_ratio,4) || ' pred_bcount ' || ROUND(pred_bcount,5));


 -- NEW code to enable Zone to simplify very short edges

--      if len <> 0. and len < 2500. then
---         measure := len/10.;
--      end if;

--==============================================================================
-- Setup the input to Zone - the bearings between each vertex evenly sampled
-- by measure. (Note at present Bearings_along_aline expects Inn_start=1
-- and Inn_end to be Xys.count).


     bearings := Bearings_Along_aline(measure,Inn_Start, Inn_End,XYs,vertex,b_to_v,pred_bcount);
-- dbms_output.put_line('bearings ' || bearings.count || ' btov ' || B_to_V.count);
     close_tolerance := Minimum_len*0.5;
     if bearings.count = 0 then
        if closed = TRUE then
           if perimeter < Minimum_len then
              GEN_geometry := In_Geometry;
              changed := GZ_UTIL_ZONE.Remove_Close_Xys(Gen_geometry,Keep_it,scale,area,close_tolerance);
              Return Gen_Geometry;   -- Probably cannot draw a "better" loop.
           end if;
        end if;
        GEN_geometry := In_Geometry;
        changed := GZ_UTIL_ZONE.Remove_Close_Xys(Gen_geometry,Keep_it,scale,area,close_tolerance);
        Return Gen_geometry;

     end if;
-- Check "Bendiness" of the edge

       if closed = TRUE and bearings.count < 25 then
          While mult * bearings.count < 25 loop
             mult := mult *2;
          End Loop;
          measure := measure /mult;
          b_to_v.trim(b_to_v.count);
          bearings.trim(bearings.count);
          bearings := Bearings_Along_aline(measure,Inn_Start, Inn_End,XYs,vertex,b_to_v,pred_bcount);
       end if;

--==============================================================================
-- Neasure the bendiness to determine whether the edge is an artificial 
-- boundary or a river. We want to use smoothing if it is a river instead of the
-- Zone algorithm.

    bendiness := Measure_Bendiness(Bearings,B_to_V,Xes,Yes,XYs, rough_length,cycles,bear_count,straight_bends,right_bends,not_right_bends);
   
--  dbms_output.put_line('Not ' || not_right_bends || ' str ' || straight_bends || ' bear ' || bear_count);              
--   dbms_output.put_line('bendiness ' || round(bendiness,5));
-- If the edge is a river with many small bends then it is much longer
-- than a line approximating it length

--dbms_output.put_line('rB/BC ' || (right_bends/bear_count) || ' pred bc ' || pred_bcount || ' L1/rough L ' || (perimeter/rough_length));
--dbms_output.put_line('  bendiness ' || bendiness || ' RB ' || (right_bends/bear_count) ||' L/c'|| (perimeter/(cycles+0.01)));

-- Figure out if it is a river and try smoothing rivers if it is

 
      if UPPER(River_param) <> 0. and cycles > 2 and closed = FALSE and bear_count > 0 and scale <= 20000000. and scale > 500000.
         and pred_bcount > 0. and (pred_bcount < 9. or (perimeter/rough_length > 1.2 and pred_bcount < 15.))
         and n > 20  and bendiness > 0.993 and right_bends/bear_count < 0.01 and perimeter/(cycles+0.01) < 5000. then
-- dbms_output.put_line('SMOOTHING');
           Gen_Geometry := Try_Smoothing(scale,minimum_len,rough_length,cycles,perimeter,
                           Gtype, SRID,Xes,Yes,Keep_it);
                           
 -- Check Validity of edges with Matthews function
          geom_is_valid := GZ_Utilities.VALIDATE_LINES_WITH_CONTEXT(Gen_geometry,tolerance);
 
--  dbms_output.put_line(' Geom is Valid ' || geom_is_valid ||' gen count ' || Gen_geometry.sdo_ordinates.count);
          If Gen_Geometry is NOT NULL THEN
--             loops :=0;
--             loop_max := TRUNC(Gen_geometry.sdo_ordinates.count/4);
--             While geom_is_valid <> 'TRUE' and loops < loop_max LOOP
--             loops := loops +1;
             self_segs := GZ_SUPER.check_for_self_intersect(Gen_geometry);

-- If it does make a similar 13349 error message.
             if self_segs.count > 0 then
              seg2 := MOD(self_segs(1),100000.);
              seg1 := TRUNC((self_segs(1)-seg2)/100000.);
              geom_is_valid := '13349 [Element <1>] [Ring <1>][Edge <'||seg1||'>][Edge <'||seg2||'>]'; 
--               dbms_output.put_line(' Calling Fix geom: Geom is Valid ' || geom_is_valid);
              vertex_removed := GZ_UTIL_ZONE.New_Fix_Geometry(Gen_geometry,geom_is_valid,Xys,closed);
--            dbms_output.put_line(' AFTER Geom is Valid ' || geom_is_valid ||'  vertex removed ' || vertex_removed);
             else
              geom_is_valid := 'TRUE';
            
             end if;
--             End Loop;
--      dbms_output.put_line(' Geom IS Valid ' || geom_is_valid);
       
             Return Gen_geometry;
 
         end if;
 
      end if;


      Minimum_in_zone := 0.25*Minimum_len/measure;
--      If Minimum_in_zone < 1 then
--        Minimum_in_zone := 1;
--      End If;
--dbms_output.put_line('min in zone ' || Minimum_in_zone || ' bear ' || bearings.count);
       allowed_deviation := Minimum_len; -- *0.5;
       max_allowed_deviation := Minimum_len;-- * 0.5;
--      dbms_output.put_line('min len ' || ROUND(Minimum_len,6) || ' max dev ' ||max_allowed_deviation || ' right bends ' || right_bends|| ' predicted ' || pred_bcount );
-- Check area of closed polygon

    IF closed Then
        PolyInfo_Array.extend(3);
        PolyInfo_Array(1) := 1;
        PolyInfo_Array(2) := 1003;
        PolyInfo_Array(3) := 1;
        poly_geometry := MDSYS.SDO_GEOMETRY(2003,8265,NULL,PolyInfo_Array,XYs);
        area := SDO_GEOM.SDO_AREA(poly_Geometry,0.00005,'unit=sq_meter');

--dbms_output.put_line('ID ' || id || ' area ' || ROUND(area,6) || ' ML ' || round(minimum_len,3) || ' perimeter ' || round(perimeter,3));
        if area < (Minimum_len*Minimum_len) then
              Minimum_in_zone := 1;
          very_small := TRUE;
--          dbms_output.put_line('area ' || area || ' very small');
        end if;
        difference := 0.;
    END IF;

       Min_INZone := ROUND(Minimum_len/(1852.*60.*measure));

--       dbms_output.put_line('scale ' || scale || 'min len ' || ROUND(Minimum_len,6) || ' min in zone ' || min_inzone  || ' measure ' || measure);

       Info_Array := In_Geometry.sdo_elem_info;

--for ii in 1..50 loop --Bearings.count loop
--dbms_output.put_line('ii ' || ii ||  ' B ' ||ROUND(Bearings(ii),5));
--end loop;
--for ii in 1 .. B_to_V.count loop
--if ii >= 1 and ii < 60 then
--dbms_output.put_line('ii ' || ii ||  ' B to V ' ||ROUND(B_TO_V(ii),5));
--end if;
--end loop;

       If bearings.count = 0 then
          GenXYArray.extend(4);
          GenXYArray(1) := Xys(1);
          GenXYArray(2) := Xys(2);
          GenXYArray(3) := Xys(Xys.count-1);
          GenXYArray(4) := Xys(Xys.count);
          GEN_geometry := MDSYS.SDO_GEOMETRY(Gtype,SRID,NULL,Info_Array,GenXYArray);
          if closed = TRUE then
--          dbms_output.put_line('returning 1');
             RETURN In_Geometry;
          else
             RETURN Gen_Geometry;
          end if;
       Else
--    dbms_output.put_line('bearing count ' || bearings.count || ' vertex ' || vertex.count);
          In_start := 1;
          In_End := Bearings.count;
          length := GZ_UTIL_ZONE.accurate_length(Xys);
          perimeter := length;
          if length = 0.0 then
--          dbms_output.put_line('returning 2');
          RETURN In_Geometry;
          end if;
          
          GEN_geometry := Coarse_Sample(In_geometry,Minimum_len*0.5);
          direct_length := GZ_UTIL_ZONE.accurate_length(Gen_Geometry.SDO_ordinates);
          
          if NOT closed then
          area := GZ_UTIL_ZONE.Area_Along_line(1, n,Xes,Yes,area_above,area_below);
          end if;
          x0 := XYs(1);
          y0 := XYs(2);
          x2 := XYs(m-1);
          y2 := XYs(m);
          jj := TRUNC(m/10);
          if jj < 2 then
            jj := 1;
          end if;
          jj := jj*2;
          For ii in 1..4 loop
             jstart := jstart + jj;
             if jstart < m then
               this_angle := GZ_UTIL_ZONE.angle(x0,y0,x1,y1,x2,y2,b1,b2);
               if this_angle < end_to_end_angle then
                  end_to_end_angle := this_angle;
               end if;
             end if;
          End Loop;

          if scale >= 1000000. then
            Nzone := ROUND(perimeter*200./scale +0.5);
--            dbms_output.put_line('perimeter ' || perimeter || ' L ' || length  || ' ratio ' || ROUND(perimeter/length,8));
          else
            Nzone := ROUND(perimeter*(300. - nice*100.)/scale +0.5);  --*200./scale 50 made it self intersect for oid 166
--dbms_output.put_line('perimeter' || perimeter || ' L ' || length  || ' ratio ' || ROUND(perimeter/length,8));
--            if perimeter/length < 1.002 then
--              Nzone := 1;
--              nice := 2.;
--              Minimum_len := Minimum_len *2;
--              dbms_output.put_line('Nzone now ' || nzone);
--            end if;
          end if;
        
--dbms_output.put_line('id ' || id || ' Nzone now ' || nzone || ' periemter ' || round(perimeter,3) || ' ML ' || minimum_len|| ' p/ML ' || Round(perimeter/Minimum_len,3)  || ' RL/ML ' || Round(Rough_length/Minimum_len,3));-- || ' RB/bear ' || round(right_bends/bear_count,3));
           if (right_bends+straight_bends+not_right_bends) <> 0 then
          direct_bendiness := (right_bends+straight_bends)/(right_bends+straight_bends+not_right_bends);
          else
           dbms_output.put_line('ZER denom id ' || id );
          end if;
-- dbms_output.put_line('DB ' || round(direct_bendiness,3) || ' DL ' || round(Direct_length,3) || ' DL/M: ' ||TRUNC(Direct_length/Minimum_len +0.4) || ' perimeter ' || round(perimeter,3));
          if Nzone <= 1 and direct_bendiness > 0.8 and TRUNC(Direct_length/Minimum_len) = 2 then
            Nzone :=2;
--            dbms_output.put_line('A');
            if direct_bendiness > 0.85 and Nzone < TRUNC(Direct_length/(2.*Minimum_len) +0.5) then
             Nzone := TRUNC(Direct_length/(2.*Minimum_len) +0.5);
             end if;
--          if (right_bends/bear_count) > 0.7 and Nzone < TRUNC(Direct_length/Minimum_len +0.4) then
          end if;
--           dbms_output.put_line('Nzone now ' || nzone);
          If Nzone <= 1 and scale <= 1000000. and area >= minimum_len * minimum_len and perimeter/rough_length >= 1.001 then

            Nzone := 4;
--            if closed = FALSE and length >= minimum_len then
--            Minimum_len := 0.4 * length;
--            Min_INZone := ROUND(Minimum_len/(1852.*60.*measure));
--            End if;
          Elsif Nzone < 2 and TRUNC(Direct_length/(0.5*Minimum_len) +0.4) >=2 then --and perimeter/Minimum_len > 1.5 then
--          dbms_output.put_line('B');
             Nzone := 2;
          Elsif Nzone = 2 and right_bends > 2 then
             Nzone := Nzone+2;             
          End If;
          
--           dbms_output.put_line('ML ' || minimum_len ||' DL ' || round(direct_length,3) || ' length ' || round(length,3) || ' NZone ' || nzone);
           
          if end_length < 0.5* minimum_len and Direct_length < 0.75*minimum_len then
              GEN_geometry := MDSYS.SDO_GEOMETRY(Gtype,SRID,NULL,Info_Array,EndXys);
              Return Gen_geometry;
          end if;

-- This just turns on Remove_area alone
--          If very_small = TRUE and closed = FALSE then
--            NZone := 1;
--          End If;

--       dbms_output.put_line('NZ ' || nzone || ' length ' || ROUND(length,0) || ' area ' || ROUND(area,0) || ' Nzone ' || Nzone || ' minimum_len ' || minimum_len || ' PERim ' || round(perimeter,3));

-- Check the aspect ratio of the curve -- is it highly elongated like a polygon
-- closed by another edge?
--                       <--- d------->
--                       +-----------
--                       L           \
--                       +-----------/
          IF area < minimum_len * minimum_len THEN

             if length <> 0. then
               d := area/length;
             else
               d:= 0.;
             end if;
          Else
             d := 0.;
          END IF;


          IF closed = TRUE and NZone <=6 then
--             NZone := 6;
             b1 := MOD(Bearings(1),360.);
             b2 := MOD(Bearings(Bearings.count),360.);
--dbms_output.put_line('nzone is ' || nzone );
             if very_small = TRUE then
                Nzone := 5;
--                dbms_output.put_line('nzzone is ' || nzone);
             elsif abs(b1-b2) < 10. then
               Nzone := Nzone + 2;
--               dbms_output.put_line('nzonee is ' || nzone);
             elsif nzone < 4 then
               Nzone := 4;
             end if;
         
             If minimum_len > length * 0.25 then
               Minimum_len := length * 0.25;
             End if;
             Min_INZone := ROUND(Minimum_len/(1852.*60.*measure));
--             dbms_output.put_line('min Len ' || ROUND(Minimum_len,6) || ' min in zone ' || min_inzone  || ' nz ' || Nzone);
          ELSIF length < 1.5 * minimum_len and d/(length+.000001) > 1.5 and area < minimum_len * minimum_len and  area > minimum_len * minimum_len * 0.4 and Nzone <= 4 then
             NZone := 5;
             length := sqrt(area) * 4.; --length + GZ_UTIL_ZONE.approx_length(EndXys);
             If minimum_len > length * 0.25 then
               Minimum_len := length * 0.25;
             End if;
--             dbms_output.put_line('NzoneB now ' || nzone);
             Min_INZone := ROUND(Minimum_len/(1852.*60.*measure));
--            dbms_output.put_line('length ' || length || ' Min len ' || ROUND(Minimum_len,6) || ' min in zone ' || min_inzone  || ' nzone ' || nzone );

           ELSIF length < 1.5 * minimum_len and d/(length+.000001) > 1.5 and area < minimum_len * minimum_len and area > minimum_len * minimum_len * 0.25 and Nzone <= 4 then
             NZone := 4;
--             dbms_output.put_line('NzoneC now ' || nzone);
             length := sqrt(area) * 4.;
             If minimum_len > length * 0.25 then
               Minimum_len := length * 0.25;
             End if;
             Min_INZone := ROUND(Minimum_len/(1852.*60.*measure));
--            dbms_output.put_line('Length ' || length || ' Min len ' || ROUND(Minimum_len,6) || ' min in zone ' || min_inzone  || ' nzone ' || nzone );

          END IF;
-- dbms_output.put_line(' st/right bends ratio ' || ((straight_bends+right_bends)/Bearings.count));
--  dbms_output.put_line(' right bends' || right_bends || ' NZone ' ||Nzone || ' min len ' || minimum_len);
/*
          if closed= FALSE and (straight_bends+right_bends)/Bearings.count > 0.9 then
              If scale <= 5000000. then
--              Nzone := right_bends+1;
                Nzone_min := right_bends +2;
              if Nzone <= right_bends then
                Nzone := right_bends + 2+ TRUNC(Nzone/2);
              elsif Nzone < Nzone_min then
                 Nzone := Nzone_min;
              end if;
              end if;
          end if;
*/
-- very wiggly lines endup with too many zones

--       If len/distance > 150000. then
--         nzone := ROUND(nzone * 0.8);
--       Elsif len/distance > 120000. then
--         nzone := ROUND(nzone * 0.9);
--       End if;

--          Nzone := TRUNC(xes.count/50);  -- was 120


--         dbms_output.put_line('ID ' || id || ' nzone was ' || nzone || ' nz ' || TRUNC(xes.count/50) ||' count ' || xes.count || ' dist ' || ROUND(distance,4)|| ' len ' || length );

      If Nzone > (n-1) then
         Nzone := n-1;
      end if;

-- Just use Visvalingam's remove area algorithm when the edge is closed or
-- part of a closed edge.
--dbms_output.put_line('EEA ' ||end_to_end_angle || ' len ' || length || ' 3*' || (3*minimum_len) || ' EL ' || end_length);
          if closed = TRUE or 
         (closed= FALSE and length < 3.0 * minimum_len and (end_to_end_angle < 135. or end_length < 0.2*length) ) then
         if closed = FALSE then
         nearly_closed := TRUE;
--          dbms_output.put_line('nearly closed is true');
         end if;
      end if;

      if closed = TRUE then
         NZONE := nzone + 1;
     end if;
     if closed = FALSE and pred_bcount < 5. then
        minimum_len := minimum_len*0.9;
     end if;
--      nzone := nzone +1;

--   dbms_output.put_line('Expected Number of zones ' || Nzone || ' bearings ' || bearings.count);
/*
          If bearings.count/2 < Min_Inzone and bearings.count/2 > TRUNC(Min_Inzone *0.6667) then
-- Zone wants at least 2 extra
             Minimum_in_zone := (bearings.count)/2 -3;
             if Minimum_in_zone < 2 then
                Minimum_in_zone := 2;
             end if;
          Else
             Minimum_in_zone := min_InZone;
         End If;
*/
--         dbms_output.put_line('min len ' || ROUND(Minimum_len,6) || ' min in zone ' || min_inzone  );

--         dbms_output.put_line('Vertex count ' || vertex.count || ' bearing ' || bearings.count);
--         IF Id = 1 then

---         dbms_output.put_line('MINIMUM IN ZONE ' || Minimum_in_zone || ' minimum length ' || minimum_len || ' bearings count  ' || bearings.count);
--         END IF;
---------1

--dbms_output.put_line('Before VIPS Nzone now ' || nzone || ' nzmin ' || nzone_min);
    if closed = TRUE then
       NZone_min :=4;
-- Is it really elongated?
       if perimeter/sqrt(area) > 8. and In_nice_factor =0.0 then
           nice := 0.9;
       end if;
    end if;
--         for i in 1..vertex.count Loop
--           dbms_output.put_line('ii ' || i || ' vertex ' || vertex(i));
--         end loop;
--   dbms_output.put_line('in_start ' || in_start || ' In _end  ' || in_end);


         GZ_UTIL_ZONE.Find_vips(allowed_deviation,max_allowed_deviation,
                     max_allowed_angle_change, Minimum_In_Zone,measure,Minimum_len,
                     Bearings, Vertex,b_to_v,Keep_it,Xes,Yes,In_Start, In_End,Nzone,
                                  Nzone_min,
                                  Start_zone,
                                  In_zone,
                                  Order_zone ,
                                  Mean_zone,
                                  Zone_constant, Deviations, Max_Deviations,
                                  Area_under_Curve,Difference) ;
--      dbms_output.put_line('Elapsed time B : ' || (current_timestamp - the_time));
--    dbms_output.put_line('Elapsed time : ' || (current_timestamp - the_time));

-- Zone needs minimum length twice as big as the desired minimum length
     next := 1;
     For ii in 2..Keep_it.count Loop
--       If nzone_min = 1 and ii <> Keep_it.count then
--        Keep_it(ii) := 0.;
--       end if;
       If Keep_it(ii)  = 1. or Keep_it(ii)  = -2. then
          next := next + 1;
       end if;
     end loop;
-- dbms_output.put_line('after vips : ' || next || ' nzone ' || nzone_min);
 /*
    if very_small = FALSE then
    medium_length := minimum_len * 4;  -- was 4.;
    GZ_UTIL_ZONE.Round_Bends(medium_length,Xes,Yes,Keep_it);
     next := 1;
     For ii in 2..Keep_it.count Loop
       If Keep_it(ii)  = 1. or Keep_it(ii)  = -2. then
          next := next + 1;
       end if;
     end loop;

dbms_output.put_line('After bends : ' || next);
    end if;
    */
--  Here we start using a 1/100 inch (half of what we used above) as the
-- resolved minimum resolution (minumum length - ML).
-- So we drop vertices whose representative area is < 0.25 * ML^2
          Max_lost_area_fraction    := 0.2;
          if (bendiness > 0.2 or plratio >=2.) and closed = FALSE and perimeter/scale > 0.005 then
            minimum_len := minimum_len*0.75;
          else
            minimum_len := minimum_len;
          end if;
          max_allowed_deviation := Minimum_len *0.5;
--          Min_Allowed_area := minimum_len * minimum_len * 0.5/nice;-- * 0.5; --0.35; --00016-- 0.0025;
--          Max_allowed_area := minimum_len * minimum_len/nice;-- * 0.5;
          Min_Allowed_area := minimum_len * minimum_len * 0.2*nice;-- * 0.5; --0.35; --00016-- 0.0025;
          Max_allowed_area := minimum_len * minimum_len*nice*0.5;-- * 0.5;

     if area = 0 then
        area := 1.;
     end if;
--     dbms_output.put_line('area ' || area || ' min allowed ' || Min_Allowed_area  || ' max ' || max_allowed_area);
--     dbms_output.put_line('ID ' || ID || ' B ' || round(bendiness,3)|| ' p/A ' || ROUND(perimeter/sqrt(area),5) || ' ML : ' || ROUND(minimum_len,0)  || ' min in zone ' || Minimum_in_zone); --area ' || Min_allowed_area ||' max area ' || Max_allowed_area || ' Nzone ' || Nzone);

--       end if;
    if scale <= 8000000. then
       loop_count := 1;          -- WAS 2 for 5 MIllion
       Straight_len := 111120.*cosy;
    else
       loop_count := 1;
       Straight_len := 111120.*cosy;
    end if;

    next := 1;
    For ii in 2..Keep_it.count Loop
      If Keep_it(ii)  = 1. or Keep_it(ii)  = -2. then
        if ii <> keep_it.count then
           pos := ii;
        end if;
        next := next + 1;
      end if;
    End Loop;
-- Measure the smallest angle. Note pos already assigned just above

   if closed = TRUE then
--     dbms_output.put_line('keep ' || keep_it.count || ' xes ' || xes.count);
      pos1 := 1;
      pos2 := 1;
      pos3 := 1;
      For ii in 2..Keep_it.count Loop
        If Keep_it(ii)  = 1. or Keep_it(ii)  = -2. then
          pos1 := pos2;
          pos2 := pos3;
          pos3 := ii;
--            dbms_output.put_line('pos1 ' || pos1 || ' pos ' || pos || ' pos2 ' || pos2);
           x1 := Xes(pos1);
           y1 := Yes(pos1);
           x2 := Xes(pos2);
           y2 := Yes(pos2);
           x3 := Xes(pos3);
           y3 := Yes(pos3);
           if pos2 <> pos1 then
           included_angle := GZ_UTIL_ZONE.angle(x1,y1,x2,y2,x3,y3,b1,b2);
           if included_angle < smallest_angle then
              smallest_angle := included_angle;
           end if;
           end if;
        end if;
      End Loop;

--   dbms_output.put_line('smallest_angle ' || smallest_angle);

-- Allow more than 1 Visvalingam loops for closed edges
--dbms_output.put_line('max allowed dev was ' || round(max_allowed_deviation,4));
      loop_count := 3;
      if next <= 4 then
          Minimum_len := sqrt(area);
          max_allowed_deviation := Minimum_len * 0.9;
--          dbms_output.put_line('max allowed devA ' || round(max_allowed_deviation,4));
          Max_allowed_area := area * 0.25;
          Min_Allowed_area := Max_Allowed_area * 0.5;
--            dbms_output.put_line('set min len to sqrt area : ' || minimum_len);
      end if;
    elsif nearly_closed = TRUE then
          Minimum_len := sqrt(area);
          max_allowed_deviation := Minimum_len * 0.8;
--           dbms_output.put_line('max allowed devB ' || round(max_allowed_deviation,4));
          Max_allowed_area := area * 0.5;
          Min_Allowed_area := Max_Allowed_area * 0.5;
--             dbms_output.put_line('nearly closed ' || max_allowed_deviation || ' ' || min_allowed_area || ' ' || max_allowed_area);
--    elsif pred_bcount < 5. and (straight_bends+right_bends)/Bearings.count < 0.2  then
--    elsif pred_bcount > 0. and pred_bcount < 1. then
--          max_allowed_deviation := Minimum_len;
--          dbms_output.put_line('max allowed devC ' || round(max_allowed_deviation,4));
--          Max_allowed_area := Minimum_len * Minimum_len* 0.5;
--          Min_Allowed_area := Max_Allowed_area * 0.5;
--dbms_output.put_line('set max' || max_allowed_area);
    end if;



-- For closed edges, if the smallest_angle < 35 then let Remove area do the job
/*
   if smallest_angle <= 35. or next = 4 then
       dbms_output.put_line('using remove area alone' || next || ' smallest ' || smallest_angle || ' ml ' || minimum_len);
         For ii in 2..Keep_it.count-1 Loop
            If Keep_it(ii)  = 1. then
               keep_it(ii) := 0.;
            End if;
        End Loop;
        Nzone :=1;
        Area_under_Curve(1) := max_allowed_area*2.;
        Start_zone(2) := Xes.count;
   end if;
*/
-- dbms_output.put_line(' Area ' || area || 'nz ' || nzone);
   if very_small = TRUE then
--      loop_count := 1;
      Minimum_len := sqrt(area);
--            dbms_output.put_line('Very small true next ' || next || ' area ' || area);
      max_allowed_deviation := max_allowed_deviation * 0.75; --Minimum_len * 0.9;
--       dbms_output.put_line('max allowed devD ' || round(max_allowed_deviation,4) || ' nz ' || nzone);
      Max_allowed_area := area * 0.25;
      Min_Allowed_area := Max_Allowed_area * 0.5;
--   elsif closed = TRUE and perimeter/sqrt(area) >= 8. and area/(minimum_len*minimum_len) < 4. then
--          max_allowed_deviation := Max_allowed_deviation*1.5;
--          Max_allowed_area := Max_allowed_area*2;
--          Min_Allowed_area := Max_Allowed_area* 0.5;
   
   end if;
--   dbms_output.put_line('BEFORE REMOVE ' || next || ' count ' || keep_it.count || ' NZ ' || NZONE || ' loop_count ' || loop_count);
--dbms_output.put_line('min allowed area ' || round(min_allowed_area,4));
--dbms_output.put_line('max allowed area ' || round(max_allowed_area,4));
--dbms_output.put_line('max allowed dev ' || round(max_allowed_deviation,4));
-- For ii in 1..Keep_it.count Loop
--        If Keep_it(ii)  = 1. or Keep_it(ii)  = -2. then
--           dbms_output.put_line('keeping ' || ii || ' keep ' || keep_it(ii));
--        end if;
--      End Loop;


-- Use Visvalingam's algorithm, decrementing area slightly on each loop
-- This will make some vertices more important so they will be retained

   
   WHILE loops < loop_count LOOP
      loops := loops + 1;
--      dbms_output.put_line('Loop ' || loops ||'min allowed area ' || round(min_allowed_area,4) || ' ML ' || round(minimum_len,3));
--dbms_output.put_line('max allowed area ' || round(max_allowed_area,4));
--dbms_output.put_line('max allowed dev ' || round(max_allowed_deviation,4));
--    dbms_output.put_line('loops ' || loops || ' max allowed dev ' || round(max_allowed_deviation,2) || ' min allowed area ' || round(min_allowed_area,2) || ' max allowed area ' || round(max_allowed_area,2));

--    if closed = TRUE and area < minimum_len*minimum_len*4. and next >= 5 then
--    dbms_output.put_line('area ' ||area || ' ml ' || (minimum_len*minimum_len*4.));
--       NULL;
--    else
-- Do this for Remove_area so it has fewer arguments

      Start_Elements := GZ_UTIL_ZONE.SET_START_ELEMENTS(In_start, In_end,Nzone,Xes.count+1,B_TO_V,Start_zone);

      GZ_UTIL_ZONE.Remove_Area(allowed_deviation,max_allowed_deviation,
                     max_allowed_angle_change, Min_allowed_area,max_allowed_area,
                     max_lost_area_fraction,measure,Minimum_len,
                     Keep_it,Xes,Yes,Nzone,Start_Elements,
                     Deviations, Max_Deviations,Area_under_Curve);



      GZ_UTIL_ZONE.Remove_Too_Straight(Keep_it,Xes,Yes,Straight_len,Straight_angle);


 
--  end if;

-- We use a weighting of 1,0.9,0.81,0.729..

      max_allowed_deviation := max_allowed_deviation * 0.9;
      Min_Allowed_area := Min_Allowed_area * 0.9;
      Max_allowed_area := Max_Allowed_area * 0.9;

-- Count number of output vertices;
 
      next := 1;
      pos2 := 1;
      pos1 := 1;
      pos := 1;
      max_area := 0.;
      last_area := 0.;
  
      For ii in 2..Keep_it.count Loop
        If Keep_it(ii)  = 1.  or Keep_it(ii)  = -2. then
           next := next + 1;
        end if;
      End Loop;


 /* 
      if very_small = FALSE then
        if pred_bcount < 2.5 then
           medium_length := minimum_len * 2.;
  --         dbms_output.put_line('med ' || medium_length || ' ' || pred_bcount);
        else
           medium_length := minimum_len ;  -- was 4.;
  --         dbms_output.put_line('Med ' || medium_length || ' ' || pred_bcount);
        end if;
        check_area := min_allowed_area*1.5;
        check_length := minimum_len;
--      dbms_output.put_line('check-area' || check_area);
--      if closed = TRUE and scale >= 5000000 then
--        GZ_UTIL_ZONE.REMOVE_VS(check_area,check_length,Xes,Yes,Keep_it);
--      end if;
--    GZ_UTIL_ZONE.Round_Bends(medium_length,Xes,Yes,Keep_it);
--     next := 1;
--     For ii in 2..Keep_it.count Loop
--       If Keep_it(ii)  = 1. or Keep_it(ii)  = -2. then
--          next := next + 1;
--       end if;
--     end loop;

--dbms_output.put_line('After bends : ' || next);
      end if;
      */
--==============================================================================      
--    Construct the Generalised edge

       GenXYArray.extend(Next*2);
       Gkeep_it.trim(Gkeep_it.count);
       Gkeep_it.extend(Next);
       next :=  0;
       pos := 0;
       for i in 1..Keep_it.Count Loop
--          dbms_output.put_line('i ' || i || ' keep ' || keep_it(i));
         if keep_it(i) = 1. or Keep_it(i)  = -2. then
            pos := pos +1;
            next := next + 1;
            Gkeep_it(next) := Keep_it(i);
            GenXYArray(pos) := Xes(i);
            pos := pos + 1;
            GenXYArray(pos) := Yes(i);
--             dbms_output.put_line('K ' || i || ' ' || Gkeep_it(next) ||' x ' || round(xes(i),6) || ' y ' || round(yes(i),6));
         End If;
       End Loop;

       gen_length := GZ_UTIL_ZONE.accurate_length(GenXYArray);

       GEN_geometry := MDSYS.SDO_GEOMETRY(Gtype,SRID,NULL,Info_Array,GenXYArray);
-- return gen_geometry;
-- dbms_output.put_line(' count ' || GENXYARRAY.count || ' pos ' || pos);      
--==============================================================================
-- Check that the edge does not self intersect. This will be a problem and
-- the caller will just call us again for a valid input.
       if closed = TRUE and pos <=10  then
         close_tolerance := minimum_len*0.5;
         
       else
         close_tolerance := minimum_len *0.5;
        changed := GZ_UTIL_ZONE.Remove_Close_Xys(Gen_geometry,GKeep_it,scale,area,close_tolerance);
--dbms_output.put_line('changed ' || changed);
       cloops := 0;
       if closed then
          cloops :=0;
       end if;

        WHILE changed <> 0 and cloops < 1 Loop
          cloops := cloops + 1; 
          changed := GZ_UTIL_ZONE.Remove_Close_Xys(Gen_geometry,GKeep_it,scale,area,close_tolerance);        
        end loop;
        
        GenXYARRAY := Gen_geometry.sdo_ordinates;
       end if;
--       end if;
  
-- Check Validity of edges with Matthews function
          geom_is_valid := GZ_Utilities.VALIDATE_LINES_WITH_CONTEXT(Gen_geometry,tolerance);
--if geom_is_valid <> 'TRUE' then
--dbms_output.put_line('For id ' || id || ' Geom is Valid ' || geom_is_valid || ' pos ' || pos || ' gen ' || genxyarray.count);
--end if;
-- If its not true then we need to 
          if geom_is_valid <> 'TRUE' and loop_count < max_loops then
             mm := TRUNC(GenXYArray.count/2) -1;  -- highest segment number;
--             result := GZ_Topofix.CHECK_GEOM_SELF_INTERSECT(GenXYArray,Info_Array,allow_touches,n,tolerance);
--             if result.count > 0 then
--                dbms_output.put_line('RES ' || result(1) ||' ' || geom_is_valid || ' Kepp ' || keep_it.count);
--             end if;
             loop_count := loop_count + 1;
          end if;

             max_allowed_deviation := max_allowed_deviation *0.75;
             Min_Allowed_area := Min_Allowed_area * 0.75; --0.75;
             Max_allowed_area := Max_Allowed_area * 0.75; --0.75;
---  dbms_output.put_line('GGeom is Valid ' || geom_is_valid);

     IF closed = TRUE and geom_is_valid <> 'TRUE' THEN
--     dbms_output.put_line(' Geom is Valid ' || geom_is_valid);

       GEN_geometry := MDSYS.SDO_GEOMETRY(2003,SRID,NULL,SDO_ELEM_INFO_ARRAY(1,1003,1),GenXYArray);
           geom_is_valid := SDO_GEOM.Validate_Geometry_with_context(Gen_Geometry,0.05);
     
--           dbms_output.put_line(' Calling fix geom: Geom is Valid ' || geom_is_valid);
       vertex_removed := GZ_UTIL_ZONE.New_Fix_Geometry(Gen_geometry,geom_is_valid,Xys,closed);
--      dbms_output.put_line(' Geom is Valid ' || geom_is_valid ||'  vertex removed ' || vertex_removed);
      if geom_is_valid = 'TRUE' then
      
        GenXYArray := Gen_geometry.sdo_ordinates;
      end if;
 
 
--    dbms_output.put_line('VV ' || vertex_removed || ' geom is Valid ' || geom_is_valid);
      ELSIF  closed <> TRUE and geom_is_valid <> 'TRUE' THEN
      
         GEN_geometry := MDSYS.SDO_GEOMETRY(2002,SRID,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),GenXYArray);
      
      self_segs := GZ_SUPER.check_for_self_intersect(Gen_geometry);

-- If it does make a similar 13349 error message.
      if self_segs.count > 0 then
        seg2 := MOD(self_segs(1),100000.);
        seg1 := TRUNC((self_segs(1)-seg2)/100000.);
        geom_is_valid := '13349 [Element <1>] [Ring <1>][Edge <'||seg1||'>][Edge <'||seg2||'>]'; 
--         dbms_output.put_line(' Calling Fix geom: Geom is Valid ' || geom_is_valid);
        vertex_removed := GZ_UTIL_ZONE.New_Fix_Geometry(Gen_geometry,geom_is_valid,Xys,closed);
--      dbms_output.put_line(' AFTER Geom is Valid ' || geom_is_valid ||'  vertex removed ' || vertex_removed);
      else
        geom_is_valid := 'TRUE';
      
      end if;
--      dbms_output.put_line(' Geom is Valid ' || geom_is_valid);
       
      if geom_is_valid = 'TRUE' then
      
        GenXYArray := Gen_geometry.sdo_ordinates;
      end if;
       END IF;
       

/*
-- Check Validity of closed loops as a polygon
          valid_loop := 0;
          geom_is_valid := 'NOT_TRUE';
          WHILE valid_loop < 4 and geom_is_valid <> 'TRUE' LOOP
            valid_loop := valid_loop + 1;

           GEN_geometry := MDSYS.SDO_GEOMETRY(2003,SRID,NULL,SDO_ELEM_INFO_ARRAY(1,1003,1),GenXYArray);
           geom_is_valid := SDO_GEOM.Validate_Geometry_with_context(Gen_Geometry,0.05);
           dbms_output.put_line('ID ' || ID || ' ' || GenXyarray.count || ' valid ' || geom_is_valid);

--         Reverse it if its clockwise (13367)

           if SUBSTR(geom_is_valid,1,5) = '13367' then
              GZ_UTIL_ZONE.reverse_ordinates(GenXyArray);
              GEN_geometry := MDSYS.SDO_GEOMETRY(2003,SRID,NULL,SDO_ELEM_INFO_ARRAY(1,1003,1),GenXYArray);
              geom_is_valid := SDO_GEOM.Validate_Geometry_with_context(Gen_Geometry,0.05);
              GZ_UTIL_ZONE.reverse_ordinates(GenXyArray);
           end if;

-- Exit as soon as its valid

           IF geom_is_valid = 'TRUE' THEN
              exit;
--  Try and fix polygon boundary crossing itself (13349)
           ELSIF SUBSTR(geom_is_valid,1,5) = '13349' THEN
--              vertex_removed := GZ_UTIL_ZONE.New_Fix_Geometry(Gen_geometry,geom_is_valid);
--dbms_output.put_line('VV ' || vertex_removed);

--               geom_is_valid := SDO_GEOM.Validate_Geometry_with_context(Gen_Geometry,0.05);
--dbms_output.put_line('ID ' || ID || ' ' || GenXyarray.count || ' valid ' ||SUBSTR(geom_is_valid,1,80));

/*
-- Sometimes when it is closed we need to try the vertex after
               if geom_is_valid <> 'TRUE' then
                  vertex_removed := vertex_removed+1;
                  next :=  0;
                  pos := 0;
                  jj := 0;
                  for i in 1..Keep_it.Count Loop
                   if keep_it(i) = 1. or Keep_it(i)  = -2. then
                     jj := jj + 1;
                     next := next + 1;
                     if vertex_removed <> jj then
                     pos := pos+1;

                     if pos > GenXyArray.count then
                       GenXYArray.extend(2);
                     end if;
                     GenXYArray(pos) := Xes(i);
                     pos := pos + 1;
                     GenXYArray(pos) := Yes(i);
--                      dbms_output.put_line('i ' || ' x ' || round(Xes(pos),6) || ' y ' || round(yes(pos),6));
                     end if;

                    End If;
                   End Loop;

--                  dbms_output.put_line('VV ' || vertex_removed);
                  GEN_geometry := MDSYS.SDO_GEOMETRY(2003,SRID,NULL,SDO_ELEM_INFO_ARRAY(1,1003,1),GenXYArray);
               geom_is_valid := SDO_GEOM.Validate_Geometry_with_context(Gen_Geometry,0.05);

               end if;
--                dbms_output.put_line('IDD ' || ID || ' ' || GenXyarray.count || ' valid ' ||geom_is_valid);
*/
/*
               if geom_is_valid = 'TRUE' then
                  jj := 0;
                  next := 0;
                  For i in 1..Keep_it.count Loop
                    If Keep_it(i)  <> 0. then
                      jj := jj + 1;
                      if jj = vertex_removed and i <> 1 and i <> Keep_it.count then
                        Keep_it(i) := 0.;
                      else
                        next := next + 1;
                      end if;
                    end if;
                  End Loop;
               end if;
               */
               if SUBSTR(geom_is_valid,1,5) = '13356' then
                  geom_is_valid := 'TRUE';
               end if;
--           END IF;
--        END LOOP;   -- End of checking closed loops validity
-- dbms_output.put_line('next ' || next || ' geom ' || geom_is_valid);


-- Update the Visvalingam parameters for a closed loop
/*
        if loops = 2 and next <= 4 then
           Minimum_len := sqrt(area);
            dbms_output.put_line('next ' || next || ' area ' || area);
           max_allowed_deviation := Minimum_len * 0.9;
           Max_allowed_area := area * 0.25;
           Min_Allowed_area := Max_Allowed_area * 0.5;
         elsif next <= 4 then
           max_allowed_deviation := max_allowed_deviation * 0.5;
           Min_Allowed_area := Min_Allowed_area * 0.5;
           Max_allowed_area := Max_Allowed_area * 0.5;
         end if;
         */
--       END IF;
-- dbms_output.put_line('min ' || round(min_allowed_area,2) || ' max ' || round(max_allowed_area,2));
--         dbms_output.put_line('gen_length ' || round(gen_length,3) || ' DIV ' || round(gen_length/perimeter,6) || '  next ' || next);
--dbms_output.put_line('next ' || next || ' geom ' || geom_is_valid);
-- Check if we can exit the loop?
       exit when geom_is_valid = 'TRUE';
--       exit when closed = TRUE and geom_is_valid = 'TRUE' and next > 4;

--       exit when gen_length/perimeter > 0.975; -- and max_area < max_allowed_area; -- 0.985;
       if loops <> loop_count then
          GenXyArray.trim(GenXyArray.count);
          For ii in 1..Keep_it.count Loop
             If Keep_it(ii)  = -1. or Keep_it(ii) = -2. then
                Keep_it(ii) := 0.;
             end if;
          End Loop;

       end if;
   END Loop;   -- End of generalization loops
-- dbms_output.put_line('AT end loop Geom is Valid ' || geom_is_valid);

--    ChainXYArray := c_convex_hull(Xes,Yes);
--    ChainXYArray := c_filterxy(measure*2.,Inn_Start, Inn_End,Xes,Yes);

-- Construct the geometry to return and check it for vertices that are too 
-- close. Don't do this for closed loops that are valid.

    GEN_geometry := MDSYS.SDO_GEOMETRY(Gtype,SRID,NULL,Info_Array,GenXYArray);

-- We want to remove any segments less than some fraction of the minimum length.
-- Should it be 0.5,or smaller? Here we use nearly minum_len/7.

 
--dbms_output.put_line('BEFORE REMOVE_close ' || next || ' count ' || GENXYARRAY.count  || ' close tol ' || close_tolerance || ' min len ' || minimum_len);
    if next >= 3 and geom_is_valid <> 'TRUE' then -- (closed = TRUE and geom_is_valid = 'TRUE' and next <=5) then
       jj := 0;
       for i in 1..Keep_it.Count Loop
         if keep_it(i) = 1. or Keep_it(i)  = -2. then
           jj := jj + 1;
           Keep_it(jj) := Keep_it(i);
--           dbms_output.put_line('keep_it ' || keep_it(jj));
          end if;
       End Loop;

       changed := GZ_UTIL_ZONE.Remove_Close_Xys(Gen_geometry,Keep_it,scale,area,close_tolerance);
--       dbms_output.put_line('changed ' || changed);
--       loops := 0;
--        WHILE changed <> 0 and loops < 7 Loop
--        loops := loops + 1;
--        GenXyarray := Gen_geometry.sdo_ordinates;
--          dbms_output.put_line('ID ' || ID || ' changed ' ||changed || ' gen count ' || Gen_geometry.sdo_ordinates.count);
--        changed := GZ_UTIL_ZONE.Remove_Close_Xys(Gen_geometry,Keep_it,scale,close_tolerance);
--       dbms_output.put_line('close_tolerance' || close_tolerance);
--        end loop;
--      For ii in 1..TRUNC(Genxyarray.count/2) loop
--          dbms_output.put_line('After ' || ii || ' X ' || round(GenxyArray(ii*2-1),6) || ' Y ' || round(GenxyArray(ii*2),6));
--       end loop;
    end if;
      End if;
 

--dbms_output.put_line('returning ' || gen_geometry.sdo_ordinates.count); --After REMOVE_close ' || changed);
    RETURN Gen_geometry;


END LINE_SIMPLIFY;
--
FUNCTION TRY_SMOOTHING(scale NUMBER,minimum_len NUMBER,rough_length NUMBER,cycles PLS_INTEGER,perimeter NUMBER,
Gtype NUMBER, SRID NUMBER,
Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN MDSYS.SDO_GEOMETRY AS


-- Try smoothing the ordinates. Return NULL if we decide this cannot be done.
-- One reason is that the result (after removing coordinates that are too close)
-- is that the result is a straight line. Smoothing has no sense of the importance
-- of points, it is entirely mechanical - so a straight line may not be right
-- for a particular scale.
-- Another reason is that the smoothing length is < 3.

   GenXYArray  MDSYS.SDO_ORDINATE_ARRAY;
   Info_Array      MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1);
   GEN_geometry   MDSYS.SDO_GEOMETRY;
   
   div             NUMBER;
   smooth_length   NUMBER;
   check_distance  NUMBER;
   changed         NUMBER;
   close_tolerance NUMBER;
   n               PLS_INTEGER := Xes.count;

BEGIN
--dbms_output.put_line('rB/BC ' || (right_bends/bear_count) || ' pred bc ' || pred_bcount || ' L1/rough L ' || (perimeter/rough_length) || '  bendiness ' || bendiness || ' RB ' || (right_bends/bear_count) ||' L/c'|| (perimeter/(cycles+0.01)));


-- dbms_output.put_line('id ' || ID || ' len1/rough ' || round(perimeter/(rough_length+0.001),3)|| ' rough ' || ROUND(rough_length,5) || ' pred_bcount ' || round(pred_bcount,4) || ' cycles ' || cycles|| ' len/cycles ' || Round(perimeter/(cycles+0.01),3) || ' bendiness ' || round(bendiness,10));
/*
         if perimeter/rough_length > 1.2 then
--         dbms_output.put_line('ID ' || ID || ' len1 ' || round(perimeter,3)|| ' rough ' || ROUND(rough_length,5) || ' pred_bcount ' || round(pred_bcount,4) || ' cycles ' || cycles);
--         smooth_length := TRUNC(perimeter*0.666666/minimum_len);
         smooth_length := TRUNC(cycles/8);

*/

---------------------NEW CODE
       close_tolerance := minimum_len*0.75;
       if perimeter/rough_length > 1.1 then
--         dbms_output.put_line('ID ' || ID || ' len1 ' || round(perimeter,3)|| ' rough ' || ROUND(rough_length,5) || ' pred_bcount ' || round(pred_bcount,4) || ' cycles ' || cycles);
--         if perimeter/cycles < 1500. then
         div := 48000./TRUNC(Round(rough_length/cycles,0)+0.5);  -- was 4200.
--           dbms_output.put_line(' div ' || div);
         if div < 2.5 then div := 2.5; end if;
         div := div*5000000./scale;  
--         else
--           div := trunc(3.75*scale/5000000.);
--         end if;

         if div > 0 then
            smooth_length := TRUNC((cycles+TRUNC(div/2))/div);
         end if;
  ---------------------NEW CODE
  
         if (TRUNC(smooth_length /2)*2) +1 <> smooth_length then
           smooth_length := smooth_length +1;
         end if;
  -- Set up for a shorter 2nd stage filter.
        if TRUNC(perimeter/Xes.count) >= 250. and smooth_length =3 then
          smooth_length := -3;
        end if;
--        dbms_output.put_line('smooth ' || smooth_length || ' div ' || div || ' xes ' || xes.count);

-- The smoothing length must be an odd number  >= 3 so we collapse
-- 3 or more values into a single output.

        if ABS(smooth_length) >= 3 then

-- The vertices need to be at a constant distance approximately
/*
         Xess.extend(n);
         Yess.extend(n);
dbms_output.put_line('N was ' || n);
         For ii in 1..n Loop
            Xess(ii) := Xes(ii);
            Yess(ii) := Yes(ii);
         End Loop;

         jj := 1;
         Xes.extend(n*3);
         Yes.extend(n*3);
         Keep_it.extend(n*3);
         xlast := Xess(1);
         ylast := Yess(1);
         Xes(1) := xlast;
         Yes(1) := ylast;
         For ii in 2..n Loop
            dx := Xess(ii)-xlast;
            dy := Yess(ii)-ylast;
            notodo := TRUNC(sqrt(dx*dx + dy*dy)/dhold+ 0.5);

            if notodo > 1 then
               dx := dx/notodo;
               dy := dy/notodo;
               For kk in 1..notodo-1 Loop
                 jj := jj + 1;
                 Keep_it(jj) := 0.;
                 Xes(jj) := Xlast + dx*kk;
                 Yes(jj) := Ylast + dy*kk;
               End Loop;
            end if;
              jj := jj +1;
              Keep_it(jj) := 0.;
              Xes(jj) := Xess(ii);
              Yes(jj) := Yess(ii);

            xlast := Xess(ii);
            ylast := Yess(ii);
         End Loop;
         n := jj;
         Keep_it(1) := 1.;
         Keep_it(n):=1.;
         Xes.trim(Xes.count-n);
         Yes.trim(Yes.count-n);
         Keep_it.trim(Keep_it.count-n);
         dbms_output.put_line('N now ' || n);
*/
         check_distance := minimum_len*0.5;
         GenXYArray := GZ_UTIL_ZONE.Smooth_line(smooth_length,check_distance,Xes,Yes,Keep_it);
--    dbms_output.put_line('genxy arra count ' ||genxyarray.count);
         If GenXyArray.count > 4 then
    -- dbms_output.put_line('ID ' || ID || ' B ' || round(bendiness,3)|| ' p/A ' || ROUND(perimeter/sqrt(area),5) || ' pred_bcount ' || round(pred_bcount,4) || ' cycles ' || cycles);
           GEN_geometry := MDSYS.SDO_GEOMETRY(Gtype,SRID,NULL,Info_Array,GenXYArray);
      ---------------------NEW CODE
           close_tolerance := minimum_len*0.25;
 
--           dbms_output.put_line('CT ' || close_tolerance || ' scale ' || scale);
           changed := GZ_UTIL_ZONE.Remove_Close_Xys(Gen_geometry,Keep_it,scale,close_tolerance);
--           dbms_output.put_line('NOWgenxy arra count ' ||gen_geometry.sdo_ordinates.count || ' Changed ' || changed);
      ---------------------NEW CODE
      -- If it has more than 2 vertices, return this geometry - else try another method
           if changed = 0. or changed > 4. then
             RETURN Gen_geometry;
           end if;
         end if;
      end if;
    end if;

    
    RETURN NULL;  -- this means smoothing is not to be used.

 
END TRY_SMOOTHING;
--
FUNCTION MEASURE_BENDINESS(Bearings IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,B_to_V IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, rough_length IN OUT NOCOPY NUMBER,cycles IN OUT NOCOPY PLS_INTEGER,
bear_count IN OUT NOCOPY PLS_INTEGER,straight_bends IN OUT NOCOPY NUMBER, right_bends IN OUT NOCOPY NUMBER,not_right_bends IN OUT NOCOPY NUMBER) RETURN NUMBER AS

  quads          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(0.,0.,0.,0.,0.,0.,0.,0.);
  EndXys         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  bearing1       NUMBER;
  bearing2       NUMBER;
  included_angle NUMBER;
  
  quad           PLS_INTEGER;
  last_quad      PLS_INTEGER :=0;
  
  Dist_to_LRbend  NUMBER;
  Dist2_to_LRbend  NUMBER;
  last_angle     NUMBER;
  
  mm             PLS_INTEGER;
  next          PLS_INTEGER :=0;
  Last_RBend     PLS_INTEGER :=0.;

  current_bend   PLS_INTEGER;
  n              PLS_INTEGER;
  bendiness      NUMBER :=0.0;
  
BEGIN

  EndXys.extend(4);
  cycles :=0;
  bearing2 := Bearings(1);
  
   for ii in 2..Bearings.count Loop
      bearing1 := bearing2;
      bearing2 := Bearings(ii);

      IF bearing2 <> bearing1 THEN
        bear_count := bear_count + 1;
   
  -- Count roughly the number of cycles in a river edge
  
      quad := MOD(TRUNC((MOD(bearing1+7200.,360.)+45.)/45.),360.);
--      dbms_output.put_line('Q:' || quad || ' B ' || bearing1);
      if quad <> last_quad  and abs(bearing2 - bearing1) > 10. then
         last_quad := quad;
         quads(quad) := quads(quad) + 1;
      end if;
  
      included_angle :=  abs(mod(bearing1+180.,360.) - bearing2);
      if included_angle > 180.0 then
         included_angle := 360. - included_angle;
      end if;
  --       dbms_output.put_line('ii ' || ii || ' B2 ' || round(bearing2,3) || ' IA ' || round(included_angle,3));  
  --
      if included_angle > 175. or (included_angle > 85. and included_angle < 95.) then
  --            right_bends := right_bends + 1.;
  --            if (included_angle > 175. and (last_angle > 85. and last_angle < 95.)) or
  --               ((included_angle > 85. and included_angle < 95.) and last_angle > 175.) then

-- Count Right angles
         if (included_angle > 87. and included_angle < 93.) then
             right_bends := right_bends + 1.;
             
    -- Look for Long Right angle bends
             if Last_RBend = 0. then
                Last_RBend := B_to_v(ii);
             else
                current_bend := B_to_V(ii);
                EndXys(1) := XYs(Last_Rbend*2-1);
                EndXys(2) := Xys(last_Rbend*2);
                EndXys(3) := XYs(current_bend*2-1);
                EndXys(4) := XYs(Current_bend*2);
                Dist_to_LRBend := GZ_UTIL_ZONE.accurate_length(EndXys,1,2);
                Dist2_to_LRBend := GZ_UTIL_ZONE.accurate_length(Xys,Last_Rbend,Current_bend);
    --                  dbms_output.put_line('Last ' || Last_RBend || ' current ' || current_bend);
    --                  dbms_output.put_line('D1 ' || Dist_to_LRBend || ' D2 ' || Dist2_to_LRBend);
--                If Dist2_to_LRBend > 0.0 and  (1.- Dist_to_LRBend/Dist2_to_LRBend) < 0.05 then
--                   right_bends := bear_count;
    --                      dbms_output.put_line('Right bend DETECTED');
--                end if;
                Last_Rbend := current_bend;
             end if;
--                   dbms_output.put_line('Right bend at ii ' || ii || ' ' ||ROUND(included_angle,6)  || ' B ' || round(bearing2,4));

-- Count straights - nearly 180 degrees

         else
            straight_bends := straight_bends + 1.;
--                   dbms_output.put_line('Straight bend at ii ' || ii );
          end if;
      else
           not_right_bends := not_right_bends + 1.;
--               dbms_output.put_line('NOT Right bend at ii ' || ii );
      end if;
      last_angle := included_angle;
      END IF;
   end Loop;
-- All bearings the same..
   if bear_count <= 0 then
      bear_count :=1;
   end if;
   if not_right_bends+ straight_bends + right_bends = 0 then
     straight_bends :=1;
   end if;
--   dbms_output.put_line('Not ' || not_right_bends || ' str ' || straight_bends || 'right_bends ' || right_bends ||' bear ' || bear_count || 'BC ' || bearings.count);
  
   bendiness := (not_right_bends+ straight_bends)/Bear_count;
  
   
   for ii in 1..quads.count loop
     if quads(ii) > cycles then
        cycles := quads(ii);
     end if;
   end loop;

--dbms_output.put_line('id ' || ID || ' len1 ' || round(perimeter,3)||  ' pred_bcount ' || round(pred_bcount,4) || ' cycles ' || cycles|| ' bendiness ' || round(bendiness,9) || ' right ' || right_bends || ' bear ' || bear_count);

-- Make a "rough" length that captures the trend without tracing each bend

   n := TRUNC(XYs.count/2);
   
   mm := sqrt(n);
   if mm < 2 then
     mm := 2;
   end if;
   EndXys.extend(mm*2+2);

     for i in 1..n Loop
      if MOD(i,mm) = 1 or i = n then
        next := next + 1;
        EndXys(next) := Xes(i);
        next := next + 1;
        EndXys(next) := Yes(i);
      end if;
   end Loop;
   EndXys.trim(EndXys.count-next);
   rough_length := GZ_UTIL_ZONE.accurate_length(EndXys);
   if n < 5 then
      rough_length := 1.5*rough_length;
   end if;
   Return bendiness;

END MEASURE_BENDINESS;

PROCEDURE REMOVE_VS(check_area NUMBER,check_length NUMBER,
                        Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                        Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                        Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE
                    ) AS
-- Procedure to remove small triangular shapes and thereby smooth the line.
--  Case 1) Small areas: Just drop the vertex and have a stright line joing two vertices
--       2) Large areas: Go to the points 25% away from the "V" vertex and connect by
--                       a straight line.
deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   XYs           MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   y             NUMBER;
   x0            NUMBER;
   y0            NUMBER;
   x1            NUMBER;
   y1            NUMBER;
   x2            NUMBER;
   y2            NUMBER;
   xnu           NUMBER;
   ynu           NUMBER;
   bearing_deviation NUMBER;

   area          NUMBER;

   dx            NUMBER;
   dy            NUMBER;
   bearing1      NUMBER;
   bearing2      NUMBER;

   length1       NUMBER := 0.;
   length2       NUMBER := 0.;

   next          PLS_INTEGER;
   pos           PLS_INTEGER;
   j             PLS_INTEGER;
   k             PLS_INTEGER;
   lasti         PLS_INTEGER := 1;
BEGIN
--    Check the v's on  the Generalised edge
    Xys.extend(4);
    next :=  0;
--    dbms_output.put_line('keep of last ' || keep_it(1));
--                       dbms_output.put_line('keep count ' || Keep_it.count);
--                       dbms_output.put_line('keep of last ' || keep_it(Keep_it.count));
    For i in 1..Keep_it.Count Loop
-- Chosen vertices are marked with a 1 (Zone) or -2 (Remove_area)
      if Keep_it(i) = 1. or Keep_it(i) = -2.  then
--       dbms_output.put_line('AT i ' || I || ' keep  ' || keep_it(i) || ' lasti ' || lasti);
         pos := i;
         x0 := x1;
         y0 := y1;
         x1 := x2;
         y1 := y2;
         next := next + 1;
         x2 := Xes(pos);
         next := next + 1;
         y2 := Yes(pos);

         length1 := length2;

         Xys(1) := x1;
         Xys(2) := y1;
         Xys(3) := x2;
         Xys(4) := y2;
--                dbms_output.put_line('XYS ' || Xys(1) || ' Xys(2)' || Xys(2));
         length2 := GZ_UTIL_ZONE.accurate_length(Xys);
         bearing1 := GZ_UTIL_ZONE.Geo_Bearing(x0,y0,x1,y1);
         bearing2 := GZ_UTIL_ZONE.Geo_Bearing(x1,y1,x2,y2);

          if bearing1 < 0. then
            bearing1 := 360. + bearing1;
          end if;

          if bearing2 < 0. then
            bearing2 := 360. + bearing2;
          end if;

-- Start once we have 3 vertices
           IF next > 3 THEN
-- We check the area of the triangle and the included angle is < 50
                area := GZ_UTIL_ZONE.Triangle_area(x0,y0,x1,y1,x2,y2);
                bearing_deviation := 180.-abs(abs(bearing2 - bearing1) -180.);
                dbms_output.put_line('at ' || i || ' area ' || round(area,3) || ' bd ' || round(bearing_deviation,3) || ' b1 ' || ROUND(bearing1,4) || 'b2 ' || ROUND(bearing2,4) || ' lastI ' || lasti);
dbms_output.put_line('x ' || x0 || ' y ' || y0);
dbms_output.put_line('X ' || x1 || ' Y ' || y1);
dbms_output.put_line('x ' || x2 || ' y ' || y2);
                if area < check_area and bearing_deviation > 80.0 then
dbms_output.put_line('dropping ' || lasti || ' bd ' ||round(bearing_deviation,4));
                    Keep_it(lasti) := 0.;
-- we just dropped a point so we have to ensure loop will have correct values
                    x1 := x0;
                    y1 := y0;
                    bearing2 := GZ_UTIL_ZONE.Geo_Bearing(x1,y1,x2,y2);
                    if bearing2 < 0. then
                     bearing2 := 360. + bearing2;
                   end if;
                   dbms_output.put_line('bearing2 ' || round(bearing2,4));
                elsif bearing_deviation > 100. and (area > check_area or
                      length1 > check_length or length2 > check_length) then
             dbms_output.put_line('DROPping ' || lasti);
                    Keep_it(lasti) := 0.;
-- Create 2 new points halfway along the segments of the "V" and join them

                   j := lasti;

                   k := i;
                   xnu := 0.5*x0 + 0.5*x1;
                   ynu := 0.5*y0 + 0.5*y1;
                   pos := GZ_UTIL_ZONE.find_nearest(xnu,ynu,Xes,Yes,j,k);
                   Keep_it(pos) := -2.;
                  dbms_output.put_line('Adding ' || pos);
                   k := k + (i-lasti);
                   if k >= Keep_it.count then
                     k  := Keep_it.count-1;
                   end if;
                   xnu := 0.5*x2 + 0.5*x1;
                   ynu := 0.5*y2 + 0.5*y1;
                   pos := GZ_UTIL_ZONE.find_nearest(xnu,ynu,Xes,Yes,j,k);
                   Keep_it(pos) := -2.;
                   dbms_output.put_line('Adding ' || pos);
                end if;
                lasti := i;

            END IF;
          END If;
        End Loop;
       dbms_output.put_line('Keep of last ' || keep_it(Keep_it.count));

END;
--
PROCEDURE FETCH_GEOMETRY(sql_stmt VARCHAR2,InOID1 PLS_INTEGER,
                        Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY) AS
/**
 ################################################################################
 # Program Name: fetch_geometry
 # Author: Sidey Timmins
 # Creation Date: 09/12/08
 # Usage:
 #
 # REQUIRED Parameters:
 #        sql_stmt - a sql statement to execute to fetch a geometry
 #        InOid1 - an OID specifying which geometry to pull.
 #        Geometry - an SDO geometry.
 # Purpose:
 #         Fetches a single geometry into a geometry array after freeing space
 # for the input array.
 #
 # Called_by:
 # Dependencies: none
 #
 ################################################################################
*/
   OID1    NUMBER := InOid1;

BEGIN

   Geometry := NULL;
   EXECUTE IMMEDIATE sql_stmt into Geometry using Oid1;

--
END Fetch_Geometry;
--
FUNCTION SMOOTH_LINE(psmooth_length NUMBER,minimum_length NUMBER,Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN  MDSYS.SDO_ORDINATE_ARRAY IS

-- A function to smooth a line using weights (filtering).

   GenXYArray        MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   Xold              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Yold              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Xnew              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Ynew              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Weights           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Positions         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Triangle          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(0.083333,0.25,0.333333,0.25,0.083333); --0.23,0.54,0.23);
   n                 PLS_INTEGER := Xes.count;
   nn                PLS_INTEGER := Xes.count;
   twopi             CONSTANT NUMBER   :=6.2831853071795864769252867665590057684;     -- 2 * pi
   next              PLS_INTEGER;
   half              PLS_INTEGER;
   half1             PLS_INTEGER;
   j                 PLS_INTEGER;
   jj                PLS_INTEGER := 0;
   k                 PLS_INTEGER;
   m                 PLS_INTEGER;
   pos               PLS_INTEGER;
   pos1              PLS_INTEGER;
   pos2              PLS_INTEGER;
   last_pos          PLS_INTEGER :=0;
   loops             PLS_INTEGER;

   x0                NUMBER;
   y0                NUMBER;
   x1                NUMBER;
   y1                NUMBER;
   x2                NUMBER;
   y2                NUMBER;
   b1                NUMBER;
   b2                NUMBER;
   x2nu              NUMBER;
   y2nu              NUMBER;
   included_angle    NUMBER;
   angle_threshold   NUMBER := 120.;
   xsum              NUMBER;
   ysum              NUMBER;
   wsum              NUMBER := 0.;
   loop_count        PLS_INTEGER := 2;
   smooth_length     NUMBER  := ABS(psmooth_length);
   smooth_length1    NUMBER  := smooth_length;
BEGIN

    if smooth_length > 5 then
      Weights.extend(smooth_length);
    else
      Weights.extend(5);
    end if;
    Xold.extend(n);
    Yold.extend(n);
    Xnew.extend(n);
    Ynew.extend(n);
    Positions.extend(n);
 
 
 -- If the meters per vertex count is too large then use a shorter 2nd filter.
 -- Current cutoff is 250 meters/vertex - below 2nd stage is 5, above is 3
 
   if psmooth_length < 0 then
    Triangle := MDSYS.SDO_LIST_TYPE(0.23,0.54,0.23);
   end if;

   half := TRUNC(smooth_length/2);
   half1 := half;

   For loops in 1..loop_count Loop
--   dbms_output.put_line('loops is ' || loops);

      next := 0;
      For ii in 1..n Loop
         if loops = 1 then
              next := next + 1;
              Xold(next) := Xes(ii);
              Yold(next) := Yes(ii);
         elsif (ii=1 or ii=n or mod(ii,smooth_length)=(half+1)) then
              next := next + 1;
              Xold(next) := Xnew(ii);
              Yold(next) := Ynew(ii);
         end if;
      End Loop;

      If loops <> 1 then
        n := next;
        smooth_length := TRUNC(smooth_length/4)+1;
--        exit when smooth_length = 1;
        loop_count := 2;
        smooth_length := Triangle.count;
        half := TRUNC(smooth_length/2);
--       dbms_output.put_line('now N is ' || n || ' smooth '|| smooth_length);
     end if;

     wsum := 0.;
     For ii in 0..smooth_length-1 Loop
       if loops =1 then
      --Hamming window
         Weights(ii+1) := 0.54 - 0.46 * cos(twopi*(ii)/(smooth_length-1.));
       else
         Weights(ii+1) := Triangle(ii+1);
       end if;
       wsum := wsum + Weights(ii+1);
     End Loop;

     For ii in 1..smooth_length Loop    --Hamming window
       Weights(ii) := Weights(ii)/wsum;
--       dbms_output.put_line('i '|| ii || ' W ' || round(weights(ii),12));
     End Loop;
--     dbms_output.put_line('wsum'|| round(wsum,10));

     Xnew(1) := Xold(1);
     Ynew(1) := Yold(1);
     Xnew(n) := Xold(n);
     Ynew(n) := Yold(n);
     For ii in 1..n-half Loop
        xsum := 0.;
        ysum := 0.;
        For j in 1..smooth_length Loop
          m := ii+j-1;
          if m > n then
            m := n;
          end if;
          xsum := xsum + Weights(j)*Xold(m);
          ysum := ysum + Weights(j)*Yold(m);
        End Loop;
        Xnew(ii+half) := xsum;
        Ynew(ii+half) := ysum;
     End Loop;
     next :=  0;
     for i in 1..n Loop
        if i=1 or i=n or mod(i,smooth_length)=(half+1) then
              next := next + 1;
        end if;
     end loop;
  End Loop;

    GenXYArray.extend(next*2);
    Positions(1) := 1.;
    
-- Figure out the spacing of the samples (x) : consider smooth_length = 21
-- followed by 3. We get samples at 1, 11, 32, 53 ..
--       X                                  X                                                      X
--       1          11                      32                       53                           74
--       x           x          .           x             .           x         .
--       <----------21---------> <----------21-----------><----------21--------->
         next :=  0;
--         dbms_output.put_line('Smooth was ' || smooth_length || ' h ' || half);
         for i in 1..n Loop
           if i=1 or i=n or mod(i,smooth_length)=(half+1) then
              if i = 1 then
                pos := i;
--                dbms_output.put_line('pos was ' || pos);
              elsif i = n then
                pos := nn;
--                dbms_output.put_line('pos was ' || pos);
              else
                if loop_count=1 then
                  j := i- half1 -1;
                else
                  j := 1 + (i-2)*smooth_length1 ;
                end if;
                if j <1 then
                   j := 1;
                end if;
                k := j + smooth_length1 + smooth_length1;
                if k > nn then
                   k := nn;
                end if;

                pos := GZ_UTIL_ZONE.find_nearest(Xnew(i),Ynew(i),Xes,Yes,j,k);

--                dbms_output.put_line('pos was ' || pos || ' mid ' || (trunc((j+k)/2)) || ' last ' || last_pos);
              end if;
-- Avoid creating a self intersecting loop: ensure we dont go backwards!
--              Keep_it(pos) := 1.;
              if pos > last_pos then
                last_pos := pos; 
                jj := jj + 1;
                Xold(jj) := Xes(pos);
                Yold(jj) := Yes(pos);
                Keep_it(jj) := pos;
                next := next + 1;
                GenXYArray(next) := Xes(pos);
                next := next + 1;
                GenXYArray(next) := Yes(pos);
                Positions(TRUNC(next/2)) := pos;
             end if;
--             dbms_output.put_line('i '|| i || ' x ' || round(Xnew(i),6) || ' y ' || ROUND(ynew(i),6));
           End If;
         End Loop;
--dbms_output.put_line('next was>>>>>>>>>>>>>>>>>>>>> ' || NEXT || ' jj ' || jj);
        
 
-- Do some Corner cutting which adds shapepoints but cuts corners to soften bends
-- If we add points (x) between the original points (+) (which we drop) we get rounded bends
--
--            +----x---------x----+                  x----------x
--            |                   |                /              \
--            x                   x               x                x


         GenXYArray.extend(next*4);
         x1 := Xold(1);
         y1 := Yold(1);
         x2 := Xold(2);
         y2 := Yold(2);
         Xnew(1) := x1;
         Ynew(1) := y1;
         n := jj;
         next := 1;
         For ii in 3..n Loop
            x0 := x1;
            y0 := y1;
            x1 := x2;
            y1 := y2;
            x2 := Xold(ii);
            y2 := Yold(ii);
            pos1 := Positions(ii-2)+1;
            pos2 := Positions(ii)-1;
            included_angle := GZ_UTIL_ZONE.angle(x0,y0,x1,y1,x2,y2,b1,b2);
--             dbms_output.put_line('i '|| ii || ' angle ' || round(included_angle,3));
            if included_angle < angle_threshold then
 
-- We are generating 2 new vertices on either side of (x1,y1), 60% towards it.

              x2nu := x1 * 0.6 + x2* 0.4;
              y2nu := y1 * 0.6 + y2 *0.4;
              x1 := x0 * 0.4 + x1* 0.6;
              y1 := y0 * 0.4 + y1 *0.6;
--              dbms_output.put_line('i '|| ii || ' K ' || Keep_it(ii-1));
              j := TRUNC(Keep_it(ii-1)*0.6) + TRUNC(Keep_it(ii-2)*0.4) - 2*smooth_length1;
              if j < 1 then
                j := 1;
              end if;
              k := j + 4*smooth_length1;
              if k > nn then
                k := nn;
              end if;
              
-- And now we find the nearest original points to our smooth x,y positions

              pos1 := GZ_UTIL_ZONE.find_nearest(x1,y1,Xes,Yes,j,k);
--              dbms_output.put_line('Pos was ' || pos1 || ' mid ' || (trunc((j+k)/2)));
              
              j := TRUNC(Keep_it(ii-1)*0.6)  + TRUNC(Keep_it(ii)*0.4) - 2*smooth_length1;
              if j < 1 then
                j := 1;
              end if;
              k := j + 4*smooth_length1;
              if k > nn then
                k := nn;
              end if;
              
              pos2 := GZ_UTIL_ZONE.find_nearest(x2nu,y2nu,Xes,Yes,j,k);
            end if;
            
-- We check to ensure that we never go backwards with the corner cutting, else
-- we just don't do it. The original points subscripts are stored in Positions.
--dbms_output.put_line('back ' || Positions(ii-2) || ' pos1 ' || pos1 || 'pos2 ' || pos2 || ' nxt ' || Positions(ii));
            if included_angle < angle_threshold and pos1 > Positions(ii-2) and pos2 < Positions(ii) and pos2 > pos1 then
--              dbms_output.put_line('OK at ii ' || ii || ' Pos was ' || pos2|| ' y ' || yes(pos) || ' mid ' || (trunc((j+k)/2)));
              next := next + 1;
              Xnew(next) := Xes(pos1);
              Ynew(next) := Yes(pos1);
              next := next + 1;
              Xnew(next) := Xes(pos2);
              Ynew(next) := Yes(pos2);
              x1 := Xes(pos2);
              y1 := Yes(pos2);
            else
              next := next + 1;
--              dbms_output.put_line('NO good at AT ii ' || ii || ' next was ' || next || ' y ' || yold(ii-1));
              Xnew(next) := Xold(ii-1);
              Ynew(next) := Yold(ii-1);
            end if;
         End Loop;
         next := next + 1;
         Xnew(next) := Xes(nn);
         Ynew(next) := Yes(nn);
--         dbms_output.put_line('next ' ||next);
         n := next;
         if n > Keep_it.count then
            keep_it.extend(n-keep_it.count);
         end if;
         next := 0;
         For ii in 1..n Loop
           keep_it(ii) := 1;
           next := next + 1;
           GenXYArray(next) := Xnew(ii);
           next := next + 1;
           GenXYArray(next) := Ynew(ii);
         End Loop;

         GenXyarray.trim(GenXyArray.count-next);
--     dbms_output.put_line('Now output vertices = ' || TRUNC(genxyarray.count/2));
    RETURN GenXyArray;


END SMOOTH_LINE;
--
FUNCTION Find_nearest(x NUMBER,y NUMBER,
                      Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      pstart PLS_INTEGER,pend PLS_INTEGER) RETURN PLS_INTEGER IS

-- Find nearest coordinates to (x,y) and returns its position in the X,Y arrays.
-- Called by Smooth_line

    x1               NUMBER := x;
    y1               NUMBER := y;
    x2               NUMBER;
    y2               NUMBER;
    distance         NUMBER;
    shortest         NUMBER := 1.E10;
    pos              PLS_INTEGER := 0;
BEGIN
     for ii in pstart..pend Loop
        x2 := Xes(ii);
        y2 := Yes(ii);
        distance := GZ_UTIL_ZONE.accurate_gcd(x1,y1,x2,y2);
        if distance < shortest then
           pos := ii;
           shortest := distance;
        end if;
     end loop;
     RETURN pos;

END;
--
PROCEDURE Remove_Too_Straight(Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Straight_len NUMBER,
                      Straight_angle NUMBER default 175.,
                      check_for_anchors BOOLEAN default TRUE) AS

   rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   xlast           NUMBER;
   ylast           NUMBER;
   x               NUMBER;
   y               NUMBER;
   y0              NUMBER;
   xtest           NUMBER;
   ytest           NUMBER;
   xycheck         NUMBER;
   xcheck          NUMBER;
   ycheck          NUMBER;
   distance        NUMBER;
   included_angle  NUMBER;
   includ_angle    NUMBER;
   last_angle      NUMBER := 0.;
   b1              NUMBER := NULL;
   b2              NUMBER;
   b3              NUMBER;
   xdistance       NUMBER;
   ydistance       NUMBER;
   dx              NUMBER;
   dy              NUMBER;
   b22             NUMBER;
   b33             NUMBER;
   f               NUMBER;
   len             NUMBER;
   delta           NUMBER := 0.1;
   wiggles         BOOLEAN;
   dropped         PLS_INTEGER := 0;
   kk              PLS_INTEGER;
   n               PLS_INTEGER := Xes.count;
BEGIN

    xlast := Xes(1);
    ylast := Yes(1);
    y0 := ylast;
    for ii in 1..2 Loop
      xtest := xlast + delta;
      ytest := ylast + delta;
      xdistance := GZ_UTIL_ZONE.accurate_gcd(xlast,ylast,xtest,ylast);
      ydistance := GZ_UTIL_ZONE.accurate_gcd(xlast,ylast,xlast,ytest);
      f := xdistance/ydistance;
      if ii = 1 then
        xycheck := sqrt(0.1*0.1*f*f + 0.1*0.1) * straight_len/ydistance*0.98;
      end if;
      delta := 0.005;
    end loop;
--    dbms_output.put_line('f ' || round(f,8) || ' xycheck ' || round(xycheck,8));
    kk := 2;
    if check_for_anchors then
    While kk <= n and keep_it(kk) <> 1. and keep_it(kk) <> -2. Loop
       kk := kk + 1;
    End loop;
    end if;
    if kk > n then
       kk := n;
    end if;
    xtest := Xes(kk);
    ytest := Yes(kk);
    For ii in 2..n Loop

       if check_for_anchors = FALSE or Keep_it(ii) = 1. or Keep_it(ii)  = -2. then
--       dbms_output.put_line('ii ' || ii || ' keep_it ' || keep_it(ii));
          x := xtest;
          y := ytest;
          kk := ii+1;
          if check_for_anchors then
          While kk <= n and keep_it(kk) <> 1. and keep_it(kk) <> -2. Loop
            kk := kk + 1;
          End loop;
          end if;
          if kk > n then
            kk := n;
          end if;
          xtest := Xes(kk);
          ytest := yes(kk);

          if xtest <> x or ytest <> y then
            dx := (xtest-xlast) * f;
            dy := ytest-ylast;
            len := sqrt(dx*dx  + dy*dy);
           distance := GZ_UTIL_ZONE.accurate_gcd(xlast,ylast,xtest,ytest);
--            if len >= xycheck and distance < Straight_len then
 --            dbms_output.put_line('dist ' || round(distance,4) || ' len ' || Round(len,5));
--            end if;

            included_angle := GZ_UTIL_ZONE.angle(xlast,ylast,x,y,xtest,ytest,b2,b3);
--            b2 := GZ_UTIL_ZONE.geo_bearing(xlast,ylast,x,y);
--            b3 := GZ_UTIL_ZONE.geo_bearing(x,y,xtest,ytest);

--           included_angle := abs(mod(b2+180.,360.) - b3);
/*
--
--            if includ_angle > 180.0 then
--              includ_angle := 360. - includ_angle;
--            end if;
            if ABS(y - y0) > 0.05 then
                 y0 := y;
                 xcheck := x + delta;
                 ycheck := y + delta;
                 xdistance := GZ_UTIL_ZONE.accurate_gcd(x,y,xcheck,y);
                 ydistance := GZ_UTIL_ZONE.accurate_gcd(x,y,x,ycheck);
                 f := xdistance/ydistance;
            end if;
            b22 := GZ_UTIL_ZONE.fast_atan2(y-ylast,(x-xlast)*f) *rad2deg;
            if b22 < 0. then
              b22 := b22 + 360.;
            end if;

            b33 := GZ_UTIL_ZONE.fast_atan2(ytest-y,(xtest-x)*f) *rad2deg;
            if b33 < 0. then
              b33 := b33 + 360.;
            end if;
            included_angle := abs(mod(b22+180.,360.) - b33);
--*/
            if included_angle > 180.0 then
              included_angle := 360. - included_angle;
            end if;
--            b2 := b22;
--            b3 := b33;
--            dbms_output.put_line('b2 ' || round(b2,6) || ' b22 ' || round(b22,6) || ' D ' || ROUND(b2-b22,6));
--            dbms_output.put_line('b3 ' || round(b3,6) || ' b33 ' || round(b33,6) || ' D ' || ROUND(b3-b33,6));

            if distance < Straight_len then --len < xycheck then --distance < Straight_len then

-- Does the line wiggle back and forth or are we following a slow curve?
--            \                            ----
--             \-------                         \
--                      \                        |
--
               wiggles := FALSE;
               if b1 is NOT NULL then
                  wiggles := TRUE;
                  if (b1 > b2 and b2 > b3) or (b1 < b2 and b2 < b3) then
-- But the line can be still very straight so ignore half of these
                    if MOD(ii,2) = 1 then
                     wiggles := FALSE;   -- we are following a curve.
                    end if;
                  end if;
               end if;
-- Remove this point
-- dbms_output.put_line('dist ' || round(distance,4) || ' angle ' || ROUND(included_angle,6) || ' at ' || ii);
               if included_angle > 178.2 or
               (wiggles and included_angle >= Straight_angle and last_angle >= Straight_angle) then
--                 dbms_output.put_line('RMoved dist ' || round(distance,4) || ' angle ' || ROUND(included_angle,6) || ' at ' || ii);
                  Keep_it(ii) := 0.;
                  dropped := dropped + 1;
--                  dbms_output.put_line('dropped ' || ii);
               else
                  xlast := x;
                  ylast := y;
               end if;
            else
                xlast := x;
                ylast := y;
            end if;
            b1 := b2;
            last_angle := included_angle;
          end if;
       end if;
    End Loop;
--    dbms_output.put_line('dropped ' || dropped || ' n ' || round(dropped*100./n,2));

END;
--
FUNCTION CENTROID(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Xc IN OUT NUMBER,Yc IN OUT NUMBER)

RETURN NUMBER AS
/*
********************************************************************************
--Program Name: centroid
--Author: Sidey Timmins
--Updated: 04.10.2012 to use QZ_QA code to fix situation where xc was positive
-- despite input negative longitudes. Area calculation was wrong.
--Creation Date: 8/04/2008
--Usage:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      XYs          - Array of X,Y coordinates
  --      OUTPUT
  --       Xc           - Centroid X
  --       Yc           - Centroid Y
--
-- Purpose: Find a centroid of a single closed polygon and returns the area of
--         the polygon (in squared input units). Does not need an SRID.

-- Reference: DH Maling, "Measurements from Maps"
----            http://en.wikipedia.org/wiki/Centroid
********************************************************************************
*/

   n           PLS_INTEGER := XYs.count-2;
   Area        NUMBER := 0.0;

   nm          PLS_INTEGER := TRUNC(n/2) ;
   ii          PLS_INTEGER := 1;
   kount       PLS_INTEGER;
   iend        PLS_INTEGER;
   delta       NUMBER;

BEGIN

--        Centroid is +
--                       vertices
--                n-1 ____________
--                    |           |
--                    |           |
--                    |     +     |
--                    |           |
--                    |___________|
--                   1=n          2

--  Area := XYs(1) * (XYs(4) - XYs(n));
--  Xc := (XYs(1) + XYs(3)) * (Xys(1)*XYs(4) - XYs(3)*XYs(2));
--  Yc := (XYs(2) + XYs(4)) * (Xys(1)*XYs(4) - XYs(3)*XYs(2));

--  for i in 2..nm loop
--     ii := ii + 2;
--     Area := Area + XYs(ii) *  (XYs(ii+3) - XYs(ii-1));
--     Xc := Xc + (XYs(ii)   + XYs(ii+2)) * (XYs(ii)*XYs(ii+3) - XYs(ii+2)*XYs(ii+1));
--     Yc := Yc + (XYs(ii+1) + XYs(ii+3)) * (XYs(ii)*XYs(ii+3) - XYs(ii+2)*XYs(ii+1));
--  end loop;
 
  xc :=0.0;
  yc :=0.0;
  iend := Xys.count-2;
  kount := TRUNC((iend-ii)/2)+1;  -- Ignore last vertex (same as start)
  
  for i in 1..kount loop

-- area is the sum of -- current x * y ahead - x ahead y behind)

    Area := Area + XYs(iend-1) *  XYs(ii+1) -  XYs(ii) * XYs(iend);
    
-- delta is impervious to repeated ordinates

    delta := (XYs(iend-1)*XYs(ii+1) - XYs(ii)*XYs(iend));
    Xc := Xc + (XYs(iend-1)   + XYs(ii)) * delta;
    
    Yc := Yc + (XYs(iend) + XYs(ii+1)) * delta;
    ii := ii+2;
    iend := ii-1;
  end loop;
  
  area := area * 0.5;
  If Area <> 0.0 then
    Xc := Xc/(6.*Area);
    Yc := Yc/(6.*Area);
  End if;
  RETURN area;

END CENTROID;
--
FUNCTION Fast_Vincenty_gcd ( x1 NUMBER, y1 NUMBER, x2 NUMBER, y2 NUMBER, units VARCHAR2 default 'm') RETURN NUMBER Deterministic IS
/*
**************************************************************************************
--Program Name: c_fast_vincenty_gcd
--Author: Sidey Timmins
--Creation Date: 8/16/2006
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 4 required parameters:
  --
  --   REQUIRED Parameters:
  --      x1,y1           - 1st point longitude,latitude) (degrees)
  --      x2,y2           - 2nd point longitude,latitude  (degrees)
  --
  --   OPTIONAL Parameter:
  --      unit         - 'USM' : US Miles
  --                   - 'm' : meters (default)
  --                   - 'ft': feet (US)
-- Purpose:
--           Calculate Great circle distance (shortest line on the GRS80
--  ellipsoid - a model of the earth - a sphere with flattening at the poles)
--  very accurately between 2 points in geographic coordinates (degrees).
--  The trace of this line on the elipsoid is called a geodesic.

--  This function duplicates the Oracle call returned value to 3 decimal digits:
--     set length = sdo_geom.sdo_length(geometry,0.5,'unit=meter');
--  This function is unfortunately really no faster (but see c_fast_gcd which
--  achieves greater speed using an ellipsoid only for longitude angles larger
--  than 0.005 degrees!).

--  Returns: Great circle distance in meters
--
-- Reference: T.Vincenty "Direct and Inverse Solutions of Geodesics on the
--            Ellipsoid with applications of nested equations", Survey Review,
--            Vol XXII, 176, April 1975
--            http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf
--            http://www.moveable-type.co.uk/scripts/LatLongVincenty.html
-- for ellipsoids see: http://
-- www.pcigeomatics.com/cgi-bin/pcihlp/PROJ%7CEARTH+MODELS%7CELLIPSOIDS%7CELLIPSOID+CODES
--
-- Tested: http://williams.best.vwh.net/gccalc.htm. Same to first 9 digits
-- Dependencies:
--               GZ_UTIL_ZONE.c_sincos, GZ_UTIL_ZONEc_atan2
--Limits:
***************************************************************************************
*/

  twopi       CONSTANT NUMBER   :=6.2831853071795864769252867665590057684;     -- 2 * pi
  deg2rad     NUMBER   := 0.0174532925199432957692369076848861271344;  -- pi/180.
  a           CONSTANT NUMBER   := 6378137.;        -- GRS-80  ellipsoid radius in meters
--  b           NUMBER   := 6356752.31414035584785210686153 ;   -- minor radius
  b           CONSTANT NUMBER := 1895947286.799605837/298.257222101;
  a2          CONSTANT NUMBER   := 40680631590769.;               -- ellipsoid radius ^2
--  b2          NUMBER   := 40408299984087.05552164;     -- minor WGS84 radius ^2
--  b2          NUMBER   := 40408299983324.2452439396;     -- minor radius ^2
--  b2          NUMBER   := 40408299983328.76931725432420763255772;     -- minor radius ^2
  b2          NUMBER := b *b;
  F           NUMBER ;
  lambda      NUMBER ;
  u1          NUMBER ;
  u2          NUMBER ;
  tantheta1   NUMBER ;
  tantheta2   NUMBER ;
  temp        NUMBER ;
  tmp         NUMBER ;
  tmp2        NUMBER ;
  sinU1       NUMBER ;
  sinU2       NUMBER ;
  cosU1       NUMBER ;
  cosU2       NUMBER ;
  sinU1CosU2  NUMBER ;
  sinU2CosU1  NUMBER ;
  sinU1sinU2  NUMBER ;
  cosU1cosU2  NUMBER;
  lambdaP     NUMBER ;
  sinAlpha    NUMBER ;
  cosSqAlpha  NUMBER ;
  sinLambda   NUMBER ;
  cosLambda   NUMBER ;
  sinSigma    NUMBER ;
  cosSigma    NUMBER ;
  Sigma       NUMBER ;

  deltaSigma  NUMBER ;
  cos2sigmaM  NUMBER ;
  cos22       NUMBER ;      -- 2*cos2sigmaM ^2
  c           NUMBER ;
  L           NUMBER ;
  uSq         NUMBER ;
  aa          NUMBER ;
  BB          NUMBER ;
  gcd         NUMBER ;
  factor      NUMBER := -2.25;  -- empirically derived
  threshold   NUMBER := 1.E-12;
  siny        NUMBER := sin(y1*deg2rad);
BEGIN


-- F :=  (a-b)/a;   -- 1./ 298.257223563;           -- flattening
 F := 1./298.257222101;
 tantheta1 := GZ_MATH.new_tan(y1 * deg2rad) ;     -- theta is latitude
 tantheta2 := GZ_MATH.new_tan(y2 * deg2rad) ;

 L := (x2 - x1) * deg2rad ; -- Difference in longitude
 temp :=  b/a;   --(298.257223563 - 1.)/ 298.257223563 ;


 u1 := GZ_UTIL_ZONE.atan2(temp * Tantheta1,1.) ;
 u2 := GZ_UTIL_ZONE.atan2(temp * Tantheta2,1.) ;

 sinU1 := GZ_UTIL_ZONE.sincos(u1,cosU1) ;
 sinU2 := GZ_UTIL_ZONE.sincos(u2,cosU2) ;

 sinU1CosU2 := sinU1 * cosU2;
 sinU2CosU1 := sinU2 * cosU1;
 sinU1sinU2 := sinU1 * sinU2;
 cosU1cosU2 := cosU1 * cosU2;

 lambda := L ;
 lambdaP := twopi ;

 FOR iter IN 1 .. 20 LOOP

 --   itercount := itercount + 1;
    sinLambda := GZ_UTIL_ZONE.sincos(lambda,cosLambda) ;

-- Note: exponentiation is not as accurate.
    sinSigma := Sqrt((cosU2 * sinLambda) * (cosU2 * sinLambda) +
   (sinU2cosU1 - sinU1cosU2 * cosLambda)* (sinU2cosU1 - sinU1cosU2 * cosLambda)) ;
    If (sinSigma = 0.) Then
        gcd := 0. ;
        RETURN gcd;                --  co-incident points
    End If ;
    cosSigma := sinU1sinU2 + cosU1cosU2 * cosLambda ;
    Sigma    := GZ_UTIL_ZONE.atan2(sinSigma,cosSigma) ;
    sinAlpha := cosU1cosU2 * sinLambda / sinSigma ;
    cosSqAlpha := 1. - sinAlpha * sinAlpha ;
    If (cosSqAlpha = 0.) Then
      gcd := Abs(a * L) ;      -- two points on equator
      RETURN gcd ;
    End If ;
    cos2sigmaM := cosSigma - 2. * sinU1sinU2 / cosSqAlpha ;
    cos22 := 2. * cos2sigmaM * cos2sigmaM;
    c          := F / 16. * cosSqAlpha * (4. + F * (4. - 3. * cosSqAlpha)) ;
    lambdaP    := lambda ;
    temp := (Sigma + c * sinSigma *(cos2sigmaM + c * cosSigma *(cos22 -1.))) ;

-- Accelerate the computation by reducing the number of iterations
-- This factor is empirical.

    lambda     := L + (1. - c*factor) * F * sinAlpha * temp ;
    factor := 1.0;
    EXIT WHEN Abs(lambda - lambdaP) < threshold ;
  END LOOP ;


  If Abs(lambda - lambdaP) > threshold Then
    gcd := -1. ;
    RETURN gcd ;                       -- formula failed to converge
  End If ;

  uSq  := cosSqAlpha * (a2 - b2) / (b2) ;
  temp := (4096. + uSq * (-768. + uSq * (320. - 175. * uSq)))  ;
  aa   := 1. + uSq * temp /16384.;
  BB   := uSq / 1024. * (256. + uSq * (-128. + uSq * (74. - 47. * uSq))) ;
  deltaSigma := BB * sinSigma * (cos2sigmaM + BB / 4. * (cosSigma *
                (cos22 -1.) - BB / 6. * cos2sigmaM *
                (-3. + 4. * sinSigma * sinSigma) * (2.*cos22 -3.))) ;
-- From Vincenty (1975) these approximate formulas also don't match Oracle
--  aa := 1. + uSq * (64.+uSq*(-12.+5.*uSq)) /256.;
--  BB := uSq/512. * (128.+uSq*(-64.+37.*Usq));
--  deltaSigma := BB * sinSigma * (cos2sigmaM + BB / 4. * cosSigma *(-1.+cos22));
 -- Final Vincenty Great circle distance in meters
  gcd  := b * aa * (Sigma - deltaSigma) ;

 -- result is already in meters

   IF (units = 'ft') THEN       -- US statutory feet
     gcd := gcd * 39.37/12.;
   ELSIF (units = 'USM') THEN   -- US Miles
     gcd := gcd * 39.37/63360.;
   END IF ;

     RETURN gcd;
END;
--
PROCEDURE shellsort2(Arr IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Order_array IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,InLB PLS_INTEGER default 1,InUB PLS_INTEGER default 0) AS
/*
 #####################################################################################################
 # Program Name: shellsort2
 # Author: Sidey Timmins
 # Creation Date: 01/04/2007
 #
 # Usage:
 #   Call this program from inside another PL/SQL program.  This program
 #   has 2 required parameters:
 #
 #     REQUIRED Parameters:
 #            Arr             - Input number Array to sort. Sort is ascending.
 #
 #            Order_array     - Companion Array to be also sorted (usually
 #                              contains 1.. Arr.count. This array can be used
 #                              to sort other arrays:
 #
 #                         FOR i IN 1..N LOOP
 #                            Order_array(i) := i;
 #                         END LOOP;
 #
 #                         c_shellsort(Data_Array,Order_array,1,n);
 #
 #                         FOR i IN 1..N LOOP
 #                            Sorted_Array2(i) := Array2(Order_array(i));
 #                         END LOOP;
 #
 #            LB              - lower bound of Arr to sort, defaults to 1
 #            UB              - upper bound of Arr to sort, defaults to Arr.count
 #
 # Purpose:
 #   Sorts 2 arrays
 #   Example
 #            input  arrays                          sorted output
 #        unsorted Nodes   Order_Array        Node                  Order_Array
 #      216000003385025             1         8185025                   4
 #      216000008785025             2         8185025                   3
 #              8185025             3         8385025                   5
 #              8185025             4         216000003385025           1
 #              8385025             5         216000008785025           2
 #      216000008785025             6         216000008785025           6
 #
 #  Sort in place with no additional storage. Note Shellsort is not stable (that
 #  is it may move equal values that are already sorted as shown above for
 #  elements 3 and 4).
 #  Reference: Robert Sedgewick "Algorithms" Addison Wesley 1984
 #             DL Shell. "A High-Speed Sorting Procedure"
 #                        Communications of the ACM, 2,7 30-32 (1959)
 # Dependencies:
 #    none
 #
 # Modification History:
 #    01/04/07 Added trunc to fix SQL divide bug. No change in behaviour.
 #
 #####################################################################################################
*/
  LB               PLS_INTEGER               := InLB;
  UB               PLS_INTEGER               := InUB;
  h                PLS_INTEGER;
  i                PLS_INTEGER;
  j                PLS_INTEGER;
  Ladd             INTEGER;
  valu             NUMBER;
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

   While h > 0  LOOP
-- Sort by insertion in increments of h
     For i IN (LB + h)..UB LOOP
       valu := Arr(i);
       ladd := Order_array(i);
       j := i - h;
       WHILE ( j >= LB) LOOP
         Exit When (Arr(j) < valu );
         Arr(j + h) := Arr(j);
         Order_array(j + h) := Order_array(j);
         j := j - h;
       END LOOP;
       Arr(j + h) := valu;
       Order_array(j + h) := ladd;
     END LOOP;
     h := trunc(h / 3);
   End Loop;
-- Array is now sorted ascending
END shellsort2;
--
FUNCTION SEARCH_HILO(To_find NUMBER,Array IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,In_end PLS_INTEGER,hi PLS_INTEGER default 1) RETURN PLS_INTEGER as

--   Search an unsorted array for either
--              1) position of highest array value that has array value <= to_find  (hi=1)
--          or  2) position of lowest array value that has array value >= to_find   (hi<>1)

    pos   PLS_INTEGER := 1;
    best  NUMBER := 0.;
BEGIN
  if hi = 1 then
     For ii in 1..In_end Loop
        if Array(ii) <= To_find and Array(ii) > best then
           best := Array(ii);
           pos := ii;
        end if;
     End Loop;
  else
      best := 1.E10;
      For ii in 1..In_end Loop
        if Array(ii) >= To_find and Array(ii) < best then
           best := Array(ii);
           pos := ii;
        end if;
     End Loop;
  end if;
  RETURN pos;

END;
--
FUNCTION UPDATE_Areas(control PLS_INTEGER,current IN OUT NOCOPY PLS_INTEGER,search PLS_INTEGER, first PLS_INTEGER, last PLS_INTEGER,
                      Keep_it    IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Xes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Yes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      length1 IN OUT NOCOPY NUMBER, length2 IN OUT NOCOPY NUMBER) RETURN NUMBER IS
   deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
--   rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;

   XYs              MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();

    y                NUMBER;
    x0               NUMBER;
    y0               NUMBER;
    x1               NUMBER;
    y1               NUMBER;
    x2               NUMBER;
    y2               NUMBER;
    area             NUMBER := 0.0;
    dx               NUMBER;
    dy               NUMBER;
    siny             NUMBER;
    cosy             NUMBER;
    cos2y            NUMBER;
    s                NUMBER;
    a                NUMBER;
    b                NUMBER;
    c                NUMBER;
    in_current       NUMBER := current;
--    bearing1         NUMBER;
--    bearing2         NUMBER;
    yfactor          NUMBER;
    dist_factor      NUMBER := 111319.89;  -- = 1855.3315*60
    behind           PLS_INTEGER;
    ahead            PLS_INTEGER;
    live_behind      PLS_INTEGER := 0;
    live_ahead       PLS_INTEGER := 0;

BEGIN

-- We have 2 lists:     Original                   New
--                      0 (not processed)          1 (keep - anchor or end)
--                      1 (keep - anchor or end)   -2 (keeper identified by area)
--
-- Dropped points are marked with a -1.


    if search = 0 then
       NULL;
    elsif search = -1 then
-- search for the vertex behind first as the current vertex
       current := current -1;
       while current > first and keep_it(current) < 0.0 loop
              current := current-1;
       end Loop;
       if current <= first then
           current := in_current;
          RETURN 0.0;
       end if;
    elsif search = +1 then
-- search for the vertex in front of the current vertex
       current := current+1;
       while current < last and keep_it(current) < 0.0 loop
              current := current+1;
       end Loop;
       if current >= last then
       current := in_current;
          RETURN 0.0;
       end if;
    end if;

    behind := current-1;
    while behind > first and keep_it(behind) < 0.0 loop
       if live_behind = 0 and (keep_it(behind) = -2. or keep_it(behind) = 1.) then
         live_behind := behind;
       end if;
       behind := behind-1;
    end Loop;
    if behind < first then
      RETURN 0.0;
    end if;

    ahead := current+1;
    while ahead < last and keep_it(ahead) < 0.0 loop
       if live_ahead = 0 and (keep_it(ahead) = -2. or keep_it(ahead) = 1.) then
         live_ahead := ahead;
       end if;
       ahead := ahead+1;
    end Loop;
    if ahead > last then
       RETURN 0.0;
    end if;

    x0 := Xes(behind);
    y0 := Yes(behind);
    x1 := Xes(current);
    y1 := Yes(current);
    x2 := Xes(ahead);
    y2 := Yes(ahead);

    y := abs(y1);
    siny := GZ_UTIL_ZONE.sincos(y*deg2rad,cosy);
    cos2y := cosy*cosy* (1.000 + siny*0.00461);  -- empirical factor improves
--                                                  values along parallels

-- Contiguous USA

      if y < 10. then
         yfactor := 0.992 * (1. + 24. * y /dist_factor);
      elsif y < 37. then
         yfactor := 0.992 * (1. + 14.6 * y /dist_factor);
      elsif y < 47. then
         yfactor := 0.992 * (1. + 15.4 * y /dist_factor);
      else -- Alaska
         yfactor := 0.992 * (1. + 16.33 * y  /dist_factor);
      end if;

      yfactor := yfactor* yfactor;

      IF control = 1 or control = 3 THEN    -- 1 does area
      area := GZ_UTIL_ZONE.Triangle_area(x0,y0,x1,y1,x2,y2);
/*
      dx := x2-x1;
      dy := y2-y1;
      a := sqrt(dx*dx*cos2y + dy*dy * yfactor)* dist_factor;

-- Hero's area formula gives best results: Area = sqrt(s*(s-a)*(s-b)*(s-c))

      dx := x2 - x0;
      dy := y2 - y0;
      b := sqrt(dx*dx*cos2y + dy*dy * yfactor)* dist_factor;
      dx := x1 - x0;
      dy := y1 - y0;
      c := sqrt(dx*dx*cos2y + dy*dy * yfactor)* dist_factor;
      s := (a+b+c) * 0.5;
      area := sqrt(abs(s*(s-a)*(s-b)*(s-c))) ;
*/
      END IF;
--    area := GZ_UTIL_ZONE.Triangle_area(x0,y0,x1,y1,x2,y2);
--     if included_angle = -1. then
--    dbms_output.put_line('b ' || behind || ' c ' || current || ' a ' || ahead || ' area ' || round(area,0));
 --   end if;

-- Also work out angle between the selected vertices
      IF control >=2 THEN  -- 1 does length only, 3 does area and lengths
      if live_behind <> 0 then
        x0 := Xes(live_behind);
        y0 := Yes(live_behind);
      end if;
      if live_ahead <> 0 then
        x2 := Xes(live_ahead);
        y2 := Yes(live_ahead);
      end if;

      dx := x1-x0;
      dy := y1-y0;
      length1 := sqrt(dx*dx*cos2y + dy*dy * yfactor) * dist_factor;
      dx := x2-x1;
      dy := y2-y1;
      length2 := sqrt(dx*dx*cos2y + dy*dy * yfactor) * dist_factor;
      END IF;
--    dy := y1 - y0;
--    dx := (x1 - x0) * cosy * 1.003544;
--    bearing1 := GZ_UTIL_ZONE.fast_atan2(dy,dx) * rad2deg;
--    dy := y2 - y1;
--    dx := (x2 - x1) * cosy * 1.003544;
--    bearing2 := GZ_UTIL_ZONE.fast_atan2(dy,dx) * rad2deg;

--    if bearing1 < 0. then
--      bearing1 := 360. + bearing1;
--    end if;

--    if bearing2 < 0. then
--      bearing2 := 360. + bearing2;
--    end if;

--    included_angle := abs(mod(bearing1+180.,360.) - bearing2);
--    if included_angle > 180.0 then
--       included_angle := 360. - included_angle;
--    end if;

--    Xys.extend(4);

--    Xys(1) := x0;
--    Xys(2) := y0;
--    Xys(3) := x1;
--    Xys(4) := y1;
--    length1 := Approx_length(Xys);
--    Xys(1) := x2;
--    Xys(2) := y2;
--    length2 := Approx_length(Xys);
    RETURN area;
END UPDATE_AREAS;
--
PROCEDURE REMOVE_AREA(allowed_deviation NUMBER,
                      max_allowed_deviation NUMBER,
                      max_allowed_angle_change NUMBER,
                      min_allowed_area NUMBER,
                      max_allowed_area NUMBER,
                      max_lost_area_fraction    NUMBER,
                      measure NUMBER, Minimum_len NUMBER,
                      Keep_it    IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Xes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Yes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Nzone IN OUT NOCOPY PLS_INTEGER,
                                  Start_Elements IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Deviation IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Max_Deviation IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Area_under_Curve IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) AS
/*
********************************************************************************
--Program Name: Remove_Area
--Author: Sidey Timmins
--Creation Date: 1/28/2008
--Usage:
  -- Call this procedure from inside a PL/SQL program.

  --   REQUIRED Parameter:

  --            allowed_deviation : average allowed deviation (area of
  --                  polygon bounded by curve and the approximating
  --                  line segment, divided by the length of that segment).
  --            max_allowed_deviation : maximum allowed deviation (distance
  --                from curve to approximating line segment).
  --            Min_InZone: Minimum number of vertices in a zone
  --
  --            Xes and Yes: the coordinates of the curve
  --    Output:
 
  --            Deviation: average deviation
  --            Max_Deviation: maximum deviation from the curve to the
  --                 approximating line segemnt.
  --
--Purpose:   -- Performs line generalisation by implementing the BendSimplify
--              algorithm of Visvalingam.
-- Method:      Imagine a curve made by push pins holding an elastic string on a
--              cork board.(Visvalingam Figure 1).
--              Each push-pin (vertex) has an associated (effective) area
--              described by the lines to the 2 adjacent push pins and the base
--              of the triangle when the current push pin is released. The
--              base is the straight line joining the 2 adjacent pins.
--              Proceeding in turn we drop vertices starting from the smallest
--              effective area, immediately recalculating the effective areas
--              for the 2 adjacent pins, and then proceeding to the next
--              bigger area. (Occasionally the effective area decreases - if so
--              we set it equal to the last area so as not to lose order.)
--              At some threshold, we stop dropping vertices and the new
--              generalized line results.
-- Reference:
--            "Line Generalisation by Repeated Elimination of the Smallest Area"
--            Visvalingam, M,. and Whyatt, J.D. (1992)
--            CISRG Discussion paper Series # 10, University of Hull, pp1-16
--            "Line Generalisation by Repeated Elimination of Points"
--            Visvalingam, M,. and Whyatt, J.D. (1993),
--            Cartographic Journal, 30 (1),46-51
--
--Dependencies: GZ_UTIL_ZONE.Set_Start_Elements, GZ_UTIL_ZONE.Triangle_area,
--       GZ_UTIL_ZONE.Stablesort2,GZ_UTIL_ZONE.Update_Areas
********************************************************************************
*/
    deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
    areas            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    order_array      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
--    angles           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    XYs              MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    pos              PLS_INTEGER;
    pos2             PLS_INTEGER;
    restore_count    PLS_INTEGER :=0;
    drop_count       PLS_INTEGER;
    vnumber          PLS_INTEGER := 0;
    next             PLS_INTEGER;
    cutoff           PLS_INTEGER;
    current          PLS_INTEGER;
    behind           PLS_INTEGER;
    ahead            PLS_INTEGER;
    j                PLS_INTEGER;
    jj               PLS_INTEGER;
    jk               PLS_INTEGER := 0;

    angle            NUMBER;

    length           NUMBER;
    kk               PLS_INTEGER;
    kount            PLS_INTEGER;
    pos_last         PLS_INTEGER;

    area             NUMBER;
    areab            NUMBER;
    areah            NUMBER;
    area_lost        NUMBER;
    x0               NUMBER;
    y0               NUMBER;
    x1               NUMBER;
    y1               NUMBER;
    x2               NUMBER;
    y2               NUMBER;
    y                NUMBER;
    len1             NUMBER;
    len2             NUMBER;
    longer           NUMBER := minimum_len * 3.;
    included_angle   NUMBER;
    valu             NUMBER;
    other            NUMBER;
--    mod_number       PLS_INTEGER := 10;
    counter          NUMBER := 0;
    max_lost_area    NUMBER;
    tiny_area        NUMBER := min_allowed_area * 0.00000;
    last_area        NUMBER;
    average_dev_limit NUMBER := allowed_deviation;
    big_area         NUMBER;
    new_area         NUMBER :=0.;
    not_used         NUMBER := 0;
    zero_count       NUMBER := 0;
    ji               PLS_INTEGER;
    sort_it          BOOLEAN;
BEGIN

--     Start_Elements := GZ_UTIL_ZONE.SET_START_ELEMENTS(Inn_start, Inn_end,Nzone,Xes.count+1,B_TO_V,Start_zone);
     areas.extend(2000);
     order_array.extend(2000);
     XYs.extend(4);
          FOR kk in 1..keep_it.count Loop
            if keep_it(kk)  = 1. or keep_it(kk) = -2. then
              restore_count := restore_count + 1;
--              dbms_output.put_line('keeping ' || kk || ' by zone ' || keep_it(kk));
            elsif keep_it(kk)  = -1. then
              not_used := not_used + 1;
            else
               zero_count := zero_count + 1;
            end if;
           end loop;

--           dbms_output.put_line('Before restored count ' || restore_count || ' not used ' || not_used || ' Z ' || zero_count);

     For ii in 1..Nzone Loop
         jk := 0;
--  dbms_output.put_line('II ' || ii || ' nzone ' || nzone);
         kount := Start_Elements(ii+1) - Start_Elements(ii) +1;

         max_lost_area := Area_under_Curve(ii) * max_lost_area_fraction;
     -- Adjust for meridians getter closer together
         big_area := Area_under_Curve(ii);
--         IF kount > 1 and (Max_Deviation(ii) > max_allowed_deviation or
--            Deviation(ii) > average_dev_limit or
--dbms_output.put_line('Zone ' || ii || ' AREA under curve ' || Area_under_curve(ii) || ' max ' || max_allowed_area || ' mdev ' || max_deviation(ii) || ' max-allow ' || max_allowed_deviation);
            IF kount > 1 and (Area_under_Curve(ii) > max_allowed_area or Max_Deviation(ii) > max_allowed_deviation) THEN
--           IF Area_under_Curve(ii) > 0. THEN
--            dbms_output.put_line('zone ' || ii || ' AREA under curve ' || Area_under_curve(ii) || ' max ' || max_allowed_area);
--  dbms_output.put_line('zone ' || ii || ' ' || max_deviation(ii) || ' max ' || max_allowed_deviation);
--  dbms_output.put_line('Zone ' || ii || ' ' || deviation(ii) || ' max ' || average_dev_limit);
-- overlap the last zone
              pos := Start_Elements(ii);
-- look ahead to fix any problems
            if ii < (Nzone-1) then
              pos2 := Start_Elements(ii+2);
              pos_last := Start_Elements(ii+1);

-- free last vertex up so it can be moved
--              XYs(1) := Xes(pos2);
--              XYs(2) := Yes(pos2);
--              Xys(3):=  Xes(pos_last);
--              Xys(4) := Yes(pos_last);
--              length := GZ_UTIL_ZONE.approx_length(Xys);
--              dbms_output.put_line('length ' || length || ' longer ' || longer || ' pos2 ' || pos2);
--              if length < longer then
--                dbms_output.put_line('FREED ' || pos2);
--                 Keep_it(pos2) := 0.0;
--              end if;
            else
--              keep_it(Start_Elements(ii+1)) := 0.0;
              pos2 := Xes.count;
              pos_last := pos2;
            end if;

            x1 := Xes(pos);
            y1 := Yes(pos);
            x2 := Xes(pos+1);
            y2 := Yes(pos+1);

            next := 0;
            restore_count := 0;
--            dbms_output.put_line('pos ' || pos || ' pos2 ' || pos2);
            for jj in pos+1..pos2-1 Loop
                x0 := x1;
                y0 := y1;
                x1 := x2;
                y1 := y2;
                x2 := Xes(jj+1);
                y2 := yes(jj+1);
                next := next + 1;
                if next > areas.count then
                   areas.extend(1000);
                   order_array.extend(1000);
                end if;
                areas(next) := GZ_UTIL_ZONE.Triangle_area(x0,y0,x1,y1,x2,y2);
                order_array(next) := jj;
--                if jj >= 165 and jj <= 186 then
--                dbms_output.put_line('JJ ' || jj || ' area ' || areas(next));
--                end if;
             end Loop;

             GZ_UTIL_ZONE.Stablesort2(areas,order_array,1,next);
--             for jj in 1..next loop
--                if order_array(jj) = 165 or order_array(jj) = 175 then
--                   dbms_output.put_line('jj ' || jj  || ' AREA ' || areas(jj) || ' order ' || order_array(jj));
--                end if;
--                 new_area := new_area + areas(jj);
--             end loop;
--             dbms_output.put_line('next ' || next  || ' AREA ' || new_area);

             last_area := areas(1);

             LOOP
               counter := counter + 1;
               jj := 1;
               jk := jk + 1;
               While jj <= next and Keep_it(order_array(jj)) <> 0.0 Loop
                 jj := jj+1;
               End Loop;

               Exit when (jj > next); -- and Keep_it(order_array(jj)) <> 0.0) ;
--
                   vnumber := order_array(jj);
/*
                   if areas(jj) < last_area then
-- the added amount is just to keep the areas in order

                      last_area := last_area + jk * 0.0000000001;
                      kk := 1;
                      while order_array(kk) <> vnumber loop
                        kk := kk+1;
                      end loop;

                      other := order_array(kk);
                      if (kk < next and last_area > areas(kk+1)) then
-- Just put the new value in the right slot using insertion sort
                        while (kk < next and last_area > areas(kk+1)) loop
                           areas(kk) := areas(kk+1);
                           order_array(kk) := order_array(kk+1);
                           kk := kk+1;
                        end loop;
                      elsif (kk > 1 and last_area < areas(kk-1)) then
                        while (kk > 1 and last_area < areas(kk-1)) loop
                           areas(kk) := areas(kk-1);
                           order_array(kk) := order_array(kk-1);
                           kk := kk-1;
                        end loop;
                      end if;

                      areas(kk) := last_area;
                      order_array(kk) := other;
                   end if;
*/

                 last_area := areas(jj);

-- Control of 2 means we just do the lengths
              area := GZ_UTIL_ZONE.Update_areas(2,vnumber,0,pos,pos2,Keep_it,Xes,Yes,len1,len2);
--    if vnumber >= 1600 and vnumber <= 1605 then
--      dbms_output.put_line('checking ' || vnumber || ' A ' || round(areas(jj),6) || ' at jj ' || jj || ' keep ' || keep_it(vnumber));
--    end if;
              if areas(jj) < min_allowed_area and keep_it(vnumber) = 0.0 then

-- Minus 1 means its dropped and below the area thresholds
                   Keep_it(vnumber) := -1.;
--                   if vnumber >= 1600 and vnumber <= 1605 then
--  dbms_output.put_line('DROPPing ' || vnumber);
--                   end if;
                elsif keep_it(vnumber) = 0.0 then
-- Minus 2 means its selected and above the area thresholds
--                   if len1 > allowed_deviation or len2 > allowed_deviation  then
                   Keep_it(vnumber) := -2.;
--                   if vnumber >= 1600 and vnumber <= 1605 then
--                  dbms_output.put_line('Keeping ' || vnumber || ' X ' || Xes(vnumber) || ' Y ' || Yes(vnumber));
--                   else
--                      Keep_it(vnumber) := -1.;
--                   end if;
                end if;
--             for ji in 1..next loop
--                   dbms_output.put_line('JJ ' || ji  || ' AREA ' || areas(ji) || ' order ' || order_array(ji));
--             end loop;
                   behind := vnumber;
                   areab := GZ_UTIL_ZONE.Update_areas(1,behind,-1,pos,pos2,Keep_it,Xes,Yes,len1,len2);

                   ahead := vnumber;
                   areah := GZ_UTIL_ZONE.Update_areas(1,ahead,+1,pos,pos2,Keep_it,Xes,Yes,len1,len2);
--dbms_output.put_line('areab ' || ROUND(areab,5) || ' areah ' || round(areah,5));

-- NEW
                   if areab < last_area then
                      areab := last_area;
                   end if;
                   if areah < last_area then
                      areah := last_area;
                   end if;


                   kk := 1;
                   while order_array(kk) <> behind loop
                     kk := kk+1;
                   end loop;
                   if areab <> 0 and areab <> areas(kk) then

                      other := order_array(kk);
                      if (kk < next and areab > areas(kk+1)) then
-- Just put the new value in the right slot using insertion sort
                        while (kk < next and areab > areas(kk+1)) loop
                           areas(kk) := areas(kk+1);
                           order_array(kk) := order_array(kk+1);
                           kk := kk+1;
                        end loop;
                      elsif (kk > 1 and areab < areas(kk-1)) then
                        while (kk > 1 and areab < areas(kk-1)) loop
                           areas(kk) := areas(kk-1);
                           order_array(kk) := order_array(kk-1);
                           kk := kk-1;

                        end loop;
                      end if;
                      areas(kk) := areab;
                      order_array(kk) := other;
                   end if;

                   kk := 1;
                   while order_array(kk) <> ahead loop
                     kk := kk+1;
                   end loop;
                   if areah <> 0 and areah <> areas(kk) then
                      other := order_array(kk);
                      if (kk < next and areah > areas(kk+1)) then
-- Just put the new value in the right slot using insertion sort
                        while (kk < next and areah > areas(kk+1)) loop
                           areas(kk) := areas(kk+1);
                           order_array(kk) := order_array(kk+1);
                           kk := kk+1;
                        end loop;
                      elsif (kk > 1 and areah < areas(kk-1)) then
                        while (kk > 1 and areah < areas(kk-1)) loop
                           areas(kk) := areas(kk-1);
                           order_array(kk) := order_array(kk-1);
                           kk := kk-1;
                        end loop;
                      end if;

                      areas(kk) := areah;
                      order_array(kk) := other;
                   end if;

--                   dbms_output.put_line('NEXT ' || next);
--                for ji in 1..next loop
--                   dbms_output.put_line('JJ ' || ji  || ' AREA ' || areas(ji) || ' order ' || order_array(ji));
--             end loop;
           END LOOP;

        END IF;
     End Loop;
--      dbms_output.put_line('next is ' || next);
     not_used :=0;
     zero_count :=0;
     restore_count := 0;
          FOR kk in 1..keep_it.count Loop
            if keep_it(kk)  = 1. or keep_it(kk) = -2. then
              restore_count := restore_count + 1;
            elsif keep_it(kk)  = -1. then
              not_used := not_used + 1;
            else
               zero_count := zero_count + 1;
            end if;
           end loop;

--           dbms_output.put_line(' After restored count ' || restore_count || ' not used ' || not_used || ' Z ' || zero_count);

END Remove_Area;
--
FUNCTION AVERAGE(Array IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,In_start PLS_INTEGER, In_end PLS_INTEGER) RETURN NUMBER as

--  Calculate the average of an array

    avge     NUMBER := 0.;
BEGIN

     For ii in In_Start..In_end Loop
        avge := avge + Array(ii);
     End Loop;

     avge := avge/(In_end-In_start+1);
     Return avge;

END Average;
--
PROCEDURE ROUND_BENDS(  medium_length NUMBER,
                        Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                        Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                        Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE
                    ) AS
deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   XYs               MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   Geometry          MDSYS.SDO_GEOMETRY;
   y             NUMBER;
   x0            NUMBER;
   y0            NUMBER;
   x1            NUMBER;
   y1            NUMBER;
   x2            NUMBER;
   y2            NUMBER;
   x3            NUMBER;
   y3            NUMBER;
   x4            NUMBER;
   y4            NUMBER;
   x5            NUMBER;
   y5            NUMBER;
   x6            NUMBER;
   y6            NUMBER;

   siny          NUMBER;
   cosy          NUMBER;
   cos2y         NUMBER;
   dx            NUMBER;
   dy            NUMBER;
   bearing1      NUMBER;
   bearing2      NUMBER;
   bearing3      NUMBER;
   length1       NUMBER := 0.;
   length2       NUMBER := medium_length;
   len           NUMBER;
   bearing_deviation1        NUMBER := 0.;
   bearing_deviation2        NUMBER := 0.;
   bearing_a     NUMBER := 3.;
   bearing_aa    NUMBER := 3.;
   bearing_b     NUMBER := 2.;
   bearing_bb    NUMBER := 2.;
   bearing_deviation NUMBER;
   center_bearing_1 NUMBER := 2.;
   center_bearing_2 NUMBER := 2.;
   center_bearing NUMBER ;
   bearing       NUMBER;
   back_center_brng NUMBER;


   ahead         PLS_INTEGER;
   behind        PLS_INTEGER;
   pos           PLS_INTEGER := 0;
   lasti         PLS_INTEGER;
   next          PLS_INTEGER;
   center        PLS_INTEGER;
   k             PLS_INTEGER;
   how_far       PLS_INTEGER := 0;
   last          PLS_INTEGER := Keep_it.count-how_far;
   clockwise     BOOLEAN;
BEGIN

   how_far := TRUNC(keep_it.count/10);
   if how_far < 4 then
     how_far := 4;
   end if;
   if how_far > 20 then
     how_far := 20;
   end if;
   how_far := 2;
   last := Keep_it.count-how_far;
   Xys.extend(4);
-- Count number of output vertices;
    next := 0;
    For ii in 1..Keep_it.count Loop
      If Keep_it(ii)  = 1. or Keep_it(ii)  = -2. then
         next := next + 1;
      end if;
    End Loop;

--    Check the v's on  the Generalised edge
    y := abs(Yes(1));
    siny := GZ_UTIL_ZONE.sincos(y*deg2rad,cosy);
    cos2y := cosy*cosy* (1.000 + siny*0.00461);  -- empirical factor

    next :=  0;



    for i in 1..Keep_it.Count Loop
        if keep_it(i) = 1. or keep_it(i) = -2.  then
              lasti := pos;
              pos := i;
              x0 := x1;
              y0 := y1;
              x1 := x2;
              y1 := y2;
              next := next + 1;
              x2 := Xes(pos);
              next := next + 1;
              y2 := Yes(pos);
-- Start once we have 3 vertices
              if next > 3 then
                length1 := length2;
                bearing_deviation1 := bearing_deviation2;

                Xys(1) := x1;
                Xys(2) := y1;
                Xys(3) := x2;
                Xys(4) := y2;
--                dbms_output.put_line('XYS ' || Xys(1) || ' Xys(2)' || Xys(2));
                length2 := GZ_UTIL_ZONE.accurate_length(Xys);
                dy := y1 - y0;
                dx := (x1 - x0) * cosy * 1.003544;
                bearing1 := GZ_UTIL_ZONE.fast_atan2(dy,dx) * rad2deg;
                dy := y2 - y1;
                dx := (x2 - x1) * cosy * 1.003544;
                bearing2 := GZ_UTIL_ZONE.fast_atan2(dy,dx) * rad2deg;

                if bearing1 < 0. then
                  bearing1 := 360. + bearing1;
                end if;

                if bearing2 < 0. then
                  bearing2 := 360. + bearing2;
                end if;

                bearing_deviation2 := 180.-abs(abs(bearing2 - bearing1) -180.);
--                dbms_output.put_line('bd1 ' || ROUND(bearing_deviation1,4) || 'bd2 ' || ROUND(bearing_deviation2,4) || ' lastI ' || lasti);
                if (bearing_deviation2 > 70.) and
--                   (length1 < medium_length or length2 < medium_length) and
                   (lasti > how_far and lasti <= last) then

                    clockwise := TRUE;
                    if bearing2 > bearing1 then
                      clockwise := FALSE;
                      IF bearing2 > 270. and bearing1 < 90. then
                        clockwise := TRUE;
                      END IF;
                    end if;
-- All bearings are going in the direction the edge goes
                    x6 := Xes(lasti+2);
                    y6 := Yes(lasti+2);
                    x5 := Xes(lasti+1);
                    y5 := Yes(lasti+1);
                    x4 := Xes(lasti);
                    y4 := Yes(lasti);
                    x3 := Xes(lasti-1);
                    y3 := Yes(lasti-1);
                    dy := y6 - y5;
                    dx := (x6 - x5) * cosy * 1.003544;
                    bearing3 := GZ_UTIL_ZONE.fast_atan2(dy,dx) * rad2deg;
                    if bearing3 < 0. then
                      bearing3 := 360. + bearing3;
                    end if;
                    dy := y5 - y4;
                    dx := (x5 - x4) * cosy * 1.003544;
                    bearing2 := GZ_UTIL_ZONE.fast_atan2(dy,dx) * rad2deg;
                    if bearing2 < 0. then
                      bearing2 := 360. + bearing2;
                    end if;
                    dy := y4 - y3;
                    dx := (x4 - x3) * cosy * 1.003544;
                    bearing1 := GZ_UTIL_ZONE.fast_atan2(dy,dx) * rad2deg;
                    if bearing1 < 0. then
                      bearing1 := 360. + bearing1;
                    end if;
                    center_bearing_2 := bearing1;
                    bearing_a := bearing1;
                    center_bearing_1 := bearing3;
                    bearing_aa := bearing2;
                    k := lasti-1;
                    x6 := x5;
                    y6 := y5;
-- First find the center
                   for j in lasti+1..lasti+how_far-1 loop
                      if bearing_b <> center_bearing_2 then
                        bearing_b := center_bearing_2;
                      end if;
                      if center_bearing_2 <> bearing_a then
                        center_bearing_2 := bearing_a;
                      end if;
                      x5 := x6;
                      y5 := y6;
                      x6 := Xes(j+1);
                      y6 := yes(j+1);
                      dy := y6 - y5;
                      dx := (x6 - x5) * cosy * 1.003544;
                      bearing_a := GZ_UTIL_ZONE.fast_atan2(dy,dx) * rad2deg;
                      if bearing_a < 0. then
                        bearing_a := 360. + bearing_a;
                      end if;
                      center_bearing := center_bearing_2;
                      center :=j;
--                      dbms_output.put_line('a ' || bearing_a);
                      exit when (bearing_b < center_bearing_2 and bearing_a > center_bearing_2)
                             or (bearing_b > center_bearing_2 and bearing_a < center_bearing_2);
                      if bearing_bb <> center_bearing_2 then
                        bearing_bb := center_bearing_1;
                      end if;
                      if center_bearing_1 <> bearing_aa then
                      center_bearing_1 := bearing_aa;
                      end if;
                      k := k-1;
                      x4 := x3;
                      y4 := y3;
                      x3 := Xes(k);
                      y3 := Yes(k);
                      dy := y4 - y3;
                      dx := (x4 - x3) * cosy * 1.003544;
                      bearing_aa := GZ_UTIL_ZONE.fast_atan2(dy,dx) * rad2deg;
                      if bearing_aa < 0. then
                        bearing_aa := 360. + bearing_aa;
                      end if;
                      center_bearing := center_bearing_1;
                      center := k;
--                      dbms_output.put_line('b ' || bearing_bb);
                      exit when (bearing_bb > center_bearing_1 and bearing_aa < center_bearing_1)
                            or  (bearing_bb < center_bearing_1 and bearing_aa > center_bearing_1);
                   end loop;
-- Now search for a vertex to the left or right which is some distance away
-- to round the bend
--                   dbms_output.put_line(' i is :' || I || ' center ' || center || ' keep ' || keep_it.count);


                   ahead := 0;
                   behind := 0;
                   center := lasti;
                   k := center+1;
--                   forward_check := MOD(center_bearing - 45.+360.,360.);
                   back_center_brng := MOD(center_bearing+180.,360.);
--                   back_check := MOD(back_center_brng+45,360.);
--                   dbms_output.put_line('CENTER ' || center  ||' CENTER brng ' || ROUND(center_bearing,3) || ' back c b ' || ROUND(back_center_brng,3));
--                   if clockwise = TRUE then
--                     forward_check := MOD(center_bearing +45.,360.);
--                     back_check := MOD(back_center_brng-45.+360.,360.);
--                     dbms_output.put_line('TRUE ');
--                   else
--                     dbms_output.put_line('FALSE ');
--                   end if;

--                   Xys(1) := Xes(i);
--                   Xys(2) := Yes(i);
                   for j in center..Keep_it.Count-1 Loop
                     k := k - 1;
                     if ahead = 0  then
--                       Xys(3) := Xes(j);
--                       Xys(4) := Yes(j);
                       dx := Xes(j+1) - Xes(j);
                       dy := Yes(j+1) - Yes(j);
                       bearing := GZ_UTIL_ZONE.fast_atan2(dy,dx) * rad2deg;
                       if bearing < 0. then
                           bearing := bearing + 360.;
                       end if;
--                        dbms_output.put_line('bearing ' || ROUND(bearing,3)  || ' J ' || j);
--                       length2 := Approx_length(Xys);
--                       length2 := sqrt(dx*dx+dy*dy);
--                       dbms_output.put_line('len ' || ROUND(length2,4) || ' small ' || small_length);
                        bearing_deviation := 180.-abs(abs(bearing - center_bearing) -180.);
                        if bearing_deviation >= 45.then
                          Keep_it(j) := 1.;
                          ahead := 1;
--                          dbms_output.put_line('ahead set at ' || j);
--                          dbms_output.put_line('Xys ' || Xys(1) || ' Xys(2)' || Xys(2));
--                          dbms_output.put_line('Xys ' || Xys(3) || ' Xys(4)' || Xys(4));
-- Geometry := SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_ORDINATE_ARRAY(xYs(1),Xys(2),Xys(3),XYs(4))); --445859691,33.));
--   len := sdo_geom.sdo_length(geometry,0.0005,'unit=meter');
--   dbms_output.put_line('LEN ' || len);
                       end if;
                     End If;

                     if behind = 0 and k > 1 then
--                       Xys(3) := Xes(k);
--                       Xys(4) := Yes(k);
--                       length2 := Approx_length(Xys);
                         dx := Xes(k-1) - Xes(k);
                         dy := Yes(k-1) - Yes(k);
                         bearing := GZ_UTIL_ZONE.fast_atan2(dy,dx) * rad2deg;
                         if bearing < 0. then
                           bearing := bearing + 360.;
                         end if;
                        bearing_deviation := 180.-abs(abs(bearing - back_center_brng) -180.);
--                        dbms_output.put_line('Bearing ' || ROUND(bearing,3) || ' k ' || k);
                         if bearing_deviation >= 45.then
                          Keep_it(k) := 1.;
--                         dbms_output.put_line('behind set at ' || k);
                          behind := 1;
                       end if;
                     end if;
                     exit when ahead = 1 and behind = 1;
                   end loop;
--                   if ahead = 1 and behind = 1 then
--                      Keep_it(lasti) := -1.;
--                   end if;
                end if;
              end if;
        End If;
    End Loop;

END ROUND_BENDS;
--
PROCEDURE FIND_VIPS(allowed_deviation NUMBER,
                      max_allowed_deviation NUMBER,
                      max_allowed_angle_change NUMBER,
                      Min_In_Zone PLS_INTEGER,
                      measure NUMBER,Minimum_len NUMBER,
                      Bearings   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Vertex     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      B_to_V IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Xes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Yes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Inn_start PLS_INTEGER, Inn_End PLS_INTEGER,
                                  Nzone IN OUT NOCOPY PLS_INTEGER,
                                  Nzone_min PLS_INTEGER,
                                  Start_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  In_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Order_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Mean_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Zone_constant IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Deviation IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Max_Deviation IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Area_under_Curve IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Difference NUMBER default 0.
                                  ) AS

/*
********************************************************************************
--Program Name: find_vips
--Author: Sidey Timmins
--Creation Date: 2/21/2008
--Usage:
  -- Call this procedure from inside a PL/SQL program.

  --   REQUIRED Parameter:

  --            allowed_deviation : average allowed deviation (area of
  --                  polygon bounded by curve and the approximating
  --                  line segment, divided by the length of that segment).
  --            max_allowed_deviation : maximum allowed deviation (distance
  --                from curve to approximating line segment).
  --            max_allowed_anlge_change: curretnly notused
  --            Min_InZone: Minimum number of vertices in a zone
  --
  --            Bearings:  Array of bearings sampled at a constant interval
  --                  along the curve.
  --            Vertex
  --            Xes and Yes: the coordinates of the curve
  --            Nzone: the number of generalized line segments desired
  --    Output:
  --            Start_zone: beginning of a found zone
  --            In_zone: number of elements in the zone
  --            Order_Zone: order zone was picked
  --            Mean_Zone: average zone value (bearing of the generalized line)
  --            Zone_constant: indicator of how "good" the zonation is with
  --                           1 being perfect and -1 being meaningless.
  --            Deviation: average deviation
  --            Max_Deviation: maximum deviation from the curve to the
  --                 approximating line segemnt.
  --
--Purpose:   -- Tweaks boundaries of zones found by zone which divides values in
--              an array into regions so the within zone variance is smaller than
--              the between zone variance. Data must be non-negative.
-- Method:      The boundaries of earlier picked zones can be perturbed by
--              the inclusion of data elsewhere. By rezoning adjacent zones
--              the "best" zone boundary can be found.
--              This function uses a comparison of 2 independently computed
--              estimates of variance (Dixon,WJ and FJ Massey 1957 pp 147 )
--              to divide values of a 1-d function into 1 or more zones.
-- Reference:
--            "Application of a Statistical Zonation method to Reservoir
--             Evaluation and Digitized-Log Analysis" by Dan Gill
--             AAPG Bulletin, v 54, #5 May 1970 pp 719-729.
--            " Measurements from Maps" D H Maling, 1989.
--
--Dependencies: GZ_UTIL_ZONE.get_deviations, GZ_UTIL_ZONE.zone
********************************************************************************
*/
  deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
  rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
  Nzone_to_find    PLS_INTEGER := Nzone;
  Nzone2           PLS_INTEGER := Nzone_to_find*2;
  NZones           PLS_INTEGER;
  In_starts        PLS_INTEGER;
  In_Ends          PLS_INTEGER;
  Start_zones      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  In_zones         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Order_zones      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Mean_zones       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Zone_constants   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Rezoned          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

  End_Elements     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Start_Elements   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  valu             NUMBER;
  other            NUMBER;
  bearing_deviation NUMBER := 0.;
  current          PLS_INTEGER;
  next             PLS_INTEGER;
  ii               PLS_INTEGER;
  jj               PLS_INTEGER;
  k                PLS_INTEGER;
  amount           PLS_INTEGER := 4;
  bad              PLS_INTEGER := 0;
  pos              PLS_INTEGER;
  pos1             PLS_INTEGER;
  pos2             PLS_INTEGER;
  last             PLS_INTEGER;
  trap             PLS_INTEGER;
  bearing0         NUMBER := 0;
  bearing1         NUMBER := 0;
  obearing_deviation  NUMBER;
  included_angle   NUMBER;
  trash            NUMBER;
  dx               NUMBER;
  dy               NUMBER;
  avge             NUMBER;
  base_dist        NUMBER;
  siny             NUMBER;
  cosy             NUMBER;
  cos2y            NUMBER;
  y                NUMBER;

  start_z          NUMBER;

  ij               PLS_INTEGER;
  In_start         PLS_INTEGER;
  In_End           PLS_INTEGER;
  more             PLS_INTEGER := 0;
  Minimum_In_zone  PLS_INTEGER;
  Found            PLS_INTEGER;
--  the_time         timestamp;
BEGIN
-- the_time := current_timestamp;
  Start_zone := MDSYS.SDO_LIST_TYPE();
  In_zone    := MDSYS.SDO_LIST_TYPE();
  Order_zone := MDSYS.SDO_LIST_TYPE();
  Mean_zone  := MDSYS.SDO_LIST_TYPE();
  Zone_constant := MDSYS.SDO_LIST_TYPE();
  Deviation     := MDSYS.SDO_LIST_TYPE();
  Max_Deviation := MDSYS.SDO_LIST_TYPE();
-- First get an initial zonation

  Nzone := GZ_UTIL_ZONE.ZONE(Bearings, Inn_Start, Inn_End,Nzone_to_find,Nzone_min,
                             Vertex,B_to_V,Xes,Yes,
                                  Start_zone,
                                  In_zone,
                                  Order_zone,
                                  Mean_zone,
                                  Zone_constant, Minimum_len,
                                  Min_In_Zone,Difference) ;
--       for ii in 1..nzone loop
--      dbms_output.put_line('After zone ' || ii || ' start ' || start_zone(ii) || ' in zone ' || in_zone(ii) || ' mean ' || ROUND(Mean_zone(ii),7) ||  ' order ' || order_Zone(ii));
--    end loop;
--   dbms_output.put_line('Nzone is : ' || Nzone);

-- dbms_output.put_line('Elapsed time after zone : ' || (current_timestamp - the_time));
-- Now order them

  Start_Zones.extend(Nzone2);
  In_Zones.extend(Nzone2);
  Order_Zones.extend(Nzone2);
  Mean_Zones.extend(Nzone2);
  Zone_constants.extend(Nzone2);
  Deviation.extend(Nzone2);
  Max_Deviation.extend(Nzone2);
  Rezoned.extend(Nzone2);
  Area_under_Curve.extend(NZone2);

  For i in 1..Nzone Loop
    Order_Zones(i) := i;
    Start_Zones(i) := Start_Zone(i);
    In_Zones(i) := In_Zone(i);
    Mean_Zones(i) := Mean_Zone(i);
    Zone_Constants(i) := Zone_constant(i);
    Rezoned(i) := 0.;
    Area_under_Curve(i) := 0.0;
  End Loop;
-- Just an insertion sort to get Starts ordered

   FOR i IN 2..Nzone LOOP
        valu := Start_Zones(i);
        other := Order_Zones(i);
        jj := i-1;
        WHILE ((jj >= 1) and (Start_Zones(jj) > valu)) LOOP
           Start_Zones(jj+1) := Start_zones(jj);
           Order_Zones(jj+1) := Order_zones(jj);
           jj := jj -1 ;
        END LOOP;
        Start_Zones(jj+1) := valu;
        Order_Zones(jj+1) := other;
     END LOOP;

-- Finish the sort

    For i in 1..Nzone Loop
          jj := Order_Zones(i);

          Start_Zone(i) := Start_Zones(i);
          In_Zone(i) := In_Zones(jj);
          Mean_Zone(i) := Mean_Zones(jj);
          if Mean_Zone(i) < 0. then
            Mean_Zone(i) := Mean_Zone(i) +360.;
          end if;
          Zone_Constant(i) := Zone_constants(jj);
          Order_Zones(i) := Order_Zone(jj);
    End Loop;


-- Remove any 2 consecutive zones that have only a very slight change
   Nzones := Nzone;
   ij := 1;
   While ij < Nzones Loop

     If abs(Mean_Zone(ij) - Mean_Zone(ij+1)) < 5. then
        In_Zone(ij) := In_Zone(ij) + In_Zone(ij+1);
        For ii in ij+1..Nzone-1 Loop
          jj := ii+1;
          Start_Zone(ii) := Start_Zone(jj);
          In_Zone(ii) := In_Zone(jj);
          Mean_Zone(ii) := Mean_Zone(jj);
          Zone_Constant(ii) := Zone_constant(jj);
          Order_Zones(ii) := Order_Zones(jj);
        End Loop;
        Nzones := Nzones-1;
     end if;
     ij := ij + 1;
   End Loop;

   Nzone := Nzones;
--   dbms_output.put_line('Nzones now ' || Nzone);
--      for ii in 1..nzone loop
--       dbms_output.put_line('Before average ' || ii || ' start ' || start_zone(ii) || ' in zone ' || (start_zone(ii)+abs(in_zone(ii))-1));
--       avge := average(bearings,start_zone(ii), start_zone(ii)+abs(in_zone(ii))-1);
--     dbms_output.put_line('Before zone ' || ii || ' start ' || start_zone(ii) || ' in zone ' || in_zone(ii) || ' mean ' || ROUND(Mean_zone(ii),7) || ' avge ' || ROUND(avge,7) || ' order ' || order_Zones(ii));
--    end loop;

-- Get Deviations (average distance of original edge from generalized
-- straight segment.

-- If the generalized is the original curve then the deviations are zero.
   IF Nzone = (Xes.count-1) Then
      Deviation(1) := 0.;
      Max_Deviation(1) := 0.;
      Deviation(2) := 0.;
      Max_Deviation(2) := 0.;
   Else

     NULL;
--   the_time := current_timestamp;
-- Remove any double Vs we may have created
--    GZ_UTIL_ZONE.Remove_Vs(Nzone,Min_in_zone,measure,Bearings,Vertex,B_TO_V,Xes,Yes,
--                           Inn_start, Inn_End, Start_zone,In_zone,Mean_zone,Deviation,Max_Deviation,Zone_constant);
   End If;
     GZ_UTIL_ZONE.GET_DEVIATIONS(max_allowed_deviation,Vertex, B_to_V,Xes,Yes, Inn_start, Inn_end,Nzone, 1,Nzone,
                    Start_zone,In_zone, Deviation, Max_Deviation,Area_under_Curve);
--   dbms_output.put_line('Elapsed time after get_dev : ' || (current_timestamp - the_time));

-- Now check each pair of adjacent zones to see if there is a better boundary
-- and rezone the deviation is excessive

--    for ii in 1..nzone loop
--    avge := average(bearings,start_zone(ii), start_zone(ii)+abs(in_zone(ii))-1);
--      dbms_output.put_line('After sort ' || ii || ' start ' || start_zone(ii) || ' in zone ' || in_zone(ii) || ' mean ' || ROUND(Mean_zone(ii),2) || ' maxdev ' || Round(max_deviation(ii),2) || ' dev ' || ROUND(deviation(ii),7));
--   dbms_output.put_line( ii  || ' dev ' || Round(deviation(ii),2) );
--    end loop;
    Start_Elements := GZ_UTIL_ZONE.SET_START_ELEMENTS(Inn_start, Inn_end,Nzone,Xes.count+1,B_TO_V,Start_zone);

    For ii in 1..Start_Elements.count-1 Loop
--     dbms_output.put_line('start el : ' || start_elements(ii) || ' for ' || ii || ' order ' || order_zones(ii));
      Keep_it(Start_Elements(ii)) := 1.;
    End Loop;

RETURN;

END FIND_VIPS;
--
FUNCTION ACCURATE_LENGTH(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                       in_pos pls_integer default 1,In_N pls_integer default 0)
                       RETURN NUMBER DETERMINISTIC IS
/*
********************************************************************************
--Program Name: accurate_length
--Author: Sidey Timmins
--Creation Date: 10/17/2008
--Usage:
  -- Call this function from inside a PL/SQL program.

  --   REQUIRED Parameter:
  --        INPUT
  --             Xys:  Array of xy ordinates in degrees.
  --             In_pos : vertex to start at, default 1
  --             In_n   : vertex to end at, default TRUNC(Xys.count/2)
--Purpose:   -- Calculate the approximate length of an edge in meters quickly and
--              accurately from geodetic coordinates.

-- Method:  See accurate_gcd whose lengths are summed.   
--
--Dependencies: GZ_UTIL_ZONE.accurate_gcd
********************************************************************************
*/

  length      NUMBER := 0.;

  x1          NUMBER;
  y1          NUMBER;
  x2          NUMBER;
  y2          NUMBER;

  len         NUMBER;
  pos         PLS_INTEGER := In_pos;
  n           PLS_INTEGER := In_N;


BEGIN

     
     If n = 0 then
       pos := pos-1;
       n := Xys.count;
     else
       pos := pos*2-2;
       n := 2*n -pos;
     End If;

      if Xys.count = 0.0 then
        RETURN 0.0;
      end if;

      pos := pos + 1;
      x2 := Xys(pos);
      pos := pos + 1;
      y2 := Xys(pos);

      For ii in 2..TRUNC(n/2) Loop
        x1 := x2;
        y1 := y2;
        pos := pos + 1;
        x2 := Xys(pos);
        pos := pos + 1;
        y2 := Xys(pos);

       length := length + accurate_gcd(x1,y1,x2,y2);
      End Loop;
   RETURN Length;


END ACCURATE_LENGTH;
--
FUNCTION ACCURATE_GCD( x1 IN OUT NOCOPY NUMBER, y1 IN OUT NOCOPY NUMBER,
                       x2 IN OUT NOCOPY NUMBER, y2 IN OUT NOCOPY NUMBER,
                       SRID NUMBER DEFAULT 8265.)
                       RETURN NUMBER DETERMINISTIC IS
/*
********************************************************************************
--Program Name: accurate_gcd
--Author: Sidey Timmins
--Creation Date: 03/17/2010
--Updated: 04/18/2012 To get same result in both directions and be more accurate.
--         10/29/2010 To calculate its own sincos and to better match Charles
--         Karney's results from his excellent code.
--Usage:
  -- Call this function from inside a PL/SQL program.

  --   REQUIRED Parameter:
  --        INPUT
  --             x1,y1:  vertex coordinates in degrees.
  --             x2,y2:  2nd vertex coordinates in degrees.
  --
--Purpose:   -- Calculate the accurate great circle distance between 2 vertices
--              in meters quickly and and accurately from geodetic coordinates.
--              Eight times the speed of sdo_geom.sdo_length and good to
--              less than mm of error up to 0.1 degrees.

-- Method:      Uses empirically derived constants to approximate the distance
--              using the arcsin formula for the GCD. The constants were
--              derived using the optim minimization package in GNU Octave
--              using Charles Karneys values as a reference.
--              All the constants and formulas are original work by Sidey Timmins.
-- Reference:   http://en.wikipedia.org/wiki/Great_circle_distance
--              Charles Karney:         http://geographiclib.sourceforge.net/
--Dependencies:NONE
********************************************************************************
*/
   deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
   gcd        NUMBER := 0.;
   a1         NUMBER;
   a2         NUMBER;
   b          NUMBER;
   c1         NUMBER;
   c2         NUMBER;

--                          Authalic radius * acos(-1.)/180.
  authalic       NUMBER := 6371007.18;
  authalic_perd  NUMBER := 111195.051975;
--  deg2rad        NUMBER := 0.017453292519943295;  -- For IEEE accuracy for 15-16 digits


-- Jack Ganssle Cosine coefficients good to 14.7 digits
   cc1       CONSTANT NUMBER := 0.99999999999999806767;
   cc2       CONSTANT NUMBER :=-0.4999999999998996568;
   cc3       CONSTANT NUMBER := 0.04166666666581174292;
   cc4       CONSTANT NUMBER :=-0.001388888886113613522;
   cc5       CONSTANT NUMBER := 0.000024801582876042427;
   cc6       CONSTANT NUMBER :=-0.0000002755693576863181;
   cc7       CONSTANT NUMBER := 0.0000000020858327958707;
   cc8       CONSTANT NUMBER :=-0.000000000011080716368;

   t          NUMBER;
   tt         NUMBER;
  y           NUMBER := abs(y1);
  yy          NUMBER;
  yy2         NUMBER;
  dx          NUMBER;
  dy          NUMBER;
  siny        NUMBER;
  cosy        NUMBER;
  cosyy       NUMBER;
  sinyy       NUMBER;
  siny2       NUMBER;
  cosy2       NUMBER;
  cosy1       NUMBER;
  cosdy       NUMBER;
  sindy       NUMBER;
  cosy3       NUMBER;
  siny_mid    NUMBER;
  cos_sq      NUMBER;
  p           PLS_INTEGER :=1;
  delta       NUMBER;
  yfactor     NUMBER :=1.;
  dist_factor NUMBER := 111319.490793274;
  sindelta    NUMBER;
  
  Function sine(yin NUMBER, cosine IN OUT NOCOPY NUMBER) Return NUMBER Deterministic as
    sin_valu   number;
  Begin
    yy2 := yin*yin;
    IF yin < 1.5E-3 THEN

-- Use Horner's rule to evaluate the sine

     sin_valu :=  yin*(yy2*(yy2*(yy2*(yy2 *(-2.39E-08 * x2 + 2.7526E-06) - 1.98409E-04) + 8.3333315E-03)
         -1.666666664E-01) + 1.0);
     cosine := sqrt(1.-sin_valu*sin_valu);
   ELSE
-- Evaluate the cosine
     cosine := cc1 + yy2*(cc2 + yy2*(cc3 + yy2*(cc4 + yy2*(cc5 +yy2*(cc6 + yy2*(cc7 + yy2*cc8))))));
     sin_valu := sqrt(1.-cosine*cosine);
   END IF;
   Return sin_valu;
   End;
   
BEGIN

      dx := x2-x1;
      dy := y2-y1;

--Use Pythagoras for planar projections

      if NOT(Is_Geodetic(SRID)) then
         RETURN sqrt(dx*dx + dy*dy);
         
-- and special Oracle code for very short distances

      elsif abs(dx) < 0.0000057 and abs(dy) < 0.0000057 then   
         
-- This code returns the correct Oracle distance for SRID 8265 for
-- differences in latitude and longitude both <= 0.0000057 degrees. 
-- RMS error is 2E-9 meters (2 nanometers) and maximum error is 6 nanometers.
--
-- According to Siva Ramada, Oracle uses the authalic sphere for calculations
-- less than 1 meters or thereabouts. The authalic radius used here is larger
-- than the accepted value because it matches Oracle values better!

        yy := (y1+y2)*0.5*deg2rad;
        dx :=   dx*cos(yy);
        RETURN sqrt(dx*dx + dy*dy)*authalic_perd;
        
-- Use Vincenty for large differences

      elsif abs(dx) + abs(dy) > 0.2 then
         gcd := fast_vincenty_gcd(x1,y1,x2,y2);
         RETURN gcd;
      end if;
      
      yy := y*deg2rad;
      
      siny := sine(yy,cosyy);
     
      delta := (y2-y1)*0.5*deg2rad;           -- calculate sin((y1+y2)*0.5)
      cosdy := 1. - delta*delta*0.5;           -- using sin(a+eps) = sin(a)cos(eps) + cos(a)sin(eps)
      sindelta := delta - delta*delta*delta/6.;
      siny_mid := siny*cosdy + cosyy*sindelta;      -- small angle approximation for formula above

-- Note that the obvious thing to do (put these coedfficients in an array) has
-- a high startup cost to extend 5 arrays and fill them!!

   if y < 34.5 then
      if y >= 27.5 then
--       p := 5;  --20.5-27.4999
         a1 := 0.9932996344181496;
         a2 := 0.0100190915085627;
         b := 1.99383172928565e-008;
         c1 := 0.99999882342768365;
         c2 := 0.0033560231336456;
         t := 31.;
      elsif y >= 20.5 then
--       p := 4;  --13.5-20.499
         a1 := 0.9933032652183578;
         a2 := 0.0100024008612311;
         b := 1.41304231339307e-008;
         c1 := 0.99999954254499568;
         c2 := 0.00335267920716177;
         t := 24.;
      elsif y >= 13.5 then
--       p := 3;  --6.5-13.499
         a1 := 0.99330497373465199;
         a2 := 0.00998904720322573;
         b := 8.02456497494140e-009;
         c1 := 0.99999987773583565;
         c2 := 0.00335000490016898;
         t := 17.;
      elsif y >= 6.5 then
--       p := 2;
         a1 := 0.9933055334108255;
         a2 := 0.00997977946635228;
         b := 3.00739642940506e-009;
         c1 := 0.99999998463761131;
         c2 := 0.00335000490016898;
         t := 10.;
      else
--       p := 1;  --0-6.4999
         a1 := 0.99330562068398087;
         a2 := 0.00997515540930006;
         b := 2.60227919461295e-010;
         c1 := 0.99999999981136289;
         c2 := 0.00334723822659243;
         t := 3.;
      end if;
   elsif y < 52.5 then
      if y >= 45.5 then
--       p := 8;  --
         a1 := 0.9932783482104429;
         a2 := 0.0100699787885871;
         b := 2.50221540058326e-008;
         c1 := 0.99999456031055145;
         c2 := 0.0033662511150798;
         t := 49.;
      elsif y >= 41.5 then
--       p := 7;  --
         a1 := 0.9932845136377421;
         a2 := 0.0100584521647363;
         b := 2.56283586223214e-008;
         c1 := 0.9999958111001982;
         c2 := 0.00336390796249147;
         t := 45;
      else
--       p := 6;
         a1 := 0.9932934620093448;
         a2 := 0.0100381481123478;
         b := 2.40962083085169e-008;
         c1 := 0.99999759542303179;
         c2 := 0.0033598412047284;
         t := 38.;
      end if;
   elsif y < 59.5 then
--       p := 9;  --51-61
         a1 := 0.9932657810331951;
         a2 := 0.0100899846107383;
         b := 2.20706708206713e-008;
         c1 := 0.99999207002972046;
         c2 := 0.00337022115208835;
         t := 56;
   elsif y < 66.5 then
--       p := 10;  --62-71
         a1 := 0.993252463959083;
         a2 := 0.0101079774347408;
         b := 1.68099737740735e-008;
         c1 := 0.99998940805689707;
         c2 := 0.00337382242390574;
         t := 63.;
   else
--       p := 11;  -- 72-89
         a1 := 0.9932398462514362;
         a2 := 0.0101230460033092;
         b := 1.05932343670463e-008;
         c1 := 0.99998688314480166;
         c2 := 0.00337683963728868;
         t := 70.;
   end if;

-- Parabolic approximation to the error from the derivative:
--             dy/dtheta   varies as 1 + sin(theta) * sin(theta)

   cosy := cosyy* (c1+ siny*siny*c2);
   cosy2 := cosy - siny * dy*deg2rad; -- small angle approximation for cos(y2)
   cos_sq := cosy*cosy2;

   yy := y2*deg2rad;   
   siny := sine(yy,cosy);

   cosy := cosy*(c1+siny*siny*c2);   
   cosy1 := cosy + siny*dy*deg2rad;
   
   cos_sq := (cos_sq + cosy*cosy1)*0.5;
--   tt := abs(y1+y2)*0.5 -t;  -- tt doesn't seem to help much
--   dbms_output.put_line('tt '|| round(tt*tt*b,10)); -- || ' cosy ' || cosy);
   dy := dy*(a1+ a2*siny_mid*siny_mid); -- + tt*tt*b);


-- Looks like Pythagoras but actually is a great-circle formula modified
-- for small angles (whereby sin(theta) ~= theta ~= arcsin(theta)).
--  See reference
-- for 2 points: (lambda1,theta1) and (lambda2,theta2) (longitude,latitude respectively)
--                dlambda apart and dtheta apart then
--
-- length = radius * 2*arcsin(sqrt( sin(dtheta*0.5) * sin(dtheta*0.5) +
--                 cos(theta1)*cos(theta2) * sin(dlambda*0.5)* sin(dlambda*0.5))

-- Since the input is in degrees, then the dist_factor := radius * pi/180.

    gcd := sqrt(dy*dy + dx*dx*cos_sq) * dist_factor;

   RETURN gcd;

END ACCURATE_GCD;
--
FUNCTION Rad2deg RETURN Number deterministic as
rad_two_deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;

Begin
  Return  rad_two_deg;
end;
--
FUNCTION TRIANGLE_Area(x0 IN OUT NOCOPY NUMBER,y0 IN OUT NOCOPY NUMBER,
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                       SRID NUMBER default 8265.)
                       RETURN NUMBER DETERMINISTIC IS
/*
********************************************************************************
--Program Name: triangle_area
--Author: Sidey Timmins
--Creation Date: 1/12/2009
--Usage:
  -- Call this function from inside a PL/SQL program.

  --   REQUIRED Parameters:
  --        INPUT
  --             x0,y0:  xy ordinates for point 0 of triangle
  --             x1,y1:  xy ordinates for point 1 of triangle
  --             x2,y2:  xy ordinates for point 2 of triangle

--Purpose:   -- Calculate approximate triangle area in square meters quickly and
--              accurately from geodetic coordinates. Typical error less than
--              1 sq meter in 1 million.

-- Method:      Uses empirically derived constants to approximate triangle
--              lengths from the differences in latitudes, longitudes with the
--              Pythagoras formula and then uses the Hero formula to calculate area.
-- Calculate area of a spherical triangle in square meters. Note the earth
-- is an ellipsoid, not a sphere.


-- Reference: Original work by Sidey Timmins.
--
--Dependencies: sincos
********************************************************************************
*/

  area        NUMBER;
--  pi        CONSTANT NUMBER := 3.1415926535897932384626433832795028842;
  s           NUMBER;
  a           NUMBER;
  b           NUMBER;
  c           NUMBER;
  sig         NUMBER;
  az1         NUMBER;
  az2         NUMBER;
  m12         NUMBER;
  factor   CONSTANT   NUMBER := .000000156785594288739197829882343743975478;
  oarea       NUMBER;
  check_it    NUMBER;
  R2          NUMBER := 40680631590769.;  -- 6378137 squared
--  dist_factor NUMBER := 111319.490793274;

BEGIN


  a := accurate_gcd(x1,y1,x2,y2,SRID) + 1.E-6;
  b := accurate_gcd(x0,y0,x2,y2,SRID) + 1.E-6;
  c := accurate_gcd(x0,y0,x1,y1,SRID) + 1.E-6;
--  dbms_output.put_line('A ' || a || ' B ' || b || ' C ' || c); 
--  a := sdo_geom.sdo_length(sdo_geometry(2002,8265,null,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_ORDINATE_ARRAY(x0,y0,x1,y1)),0.05,'unit=meter');
--  b := sdo_geom.sdo_length(sdo_geometry(2002,8265,null,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_ORDINATE_ARRAY(x1,y1,x2,y2)),0.05,'unit=meter');
--  c := sdo_geom.sdo_length(sdo_geometry(2002,8265,null,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_ORDINATE_ARRAY(x0,y0,x2,y2)),0.05,'unit=meter');    
  if x0 = x2 and y0 = y2 then
     RETURN 0.0;
  end if;
  
  if SRID = 8265. then      
    s := (a+b+c) * 0.5;
  
    if (s-a) < 0.0 or (s-b) < 0.0 or (s-c) < 0.0 then
--    dbms_output.put_line('calling geodesic');
       sig := gz_geodesic.inverse(y0,x0,y1,x1,a,az1,az2,m12);
       sig := gz_geodesic.inverse(y2,x2,y1,x1,b,az1,az2,m12);
       sig := gz_geodesic.inverse(y2,x2,y0,x0,c,az2,az1,m12);
--       dbms_output.put_line('AA ' || a || ' B ' || b || ' C ' || c); 
    end if;
  
--    factor := pi/(180.*dist_factor);
    a := a*factor;
    b := b*factor;
    c := c*factor;
    s := (a+b+c) * 0.5;
  end if;

-- Needle shaped triangles are very hard to calculate accurately
  if (s-a)*(s-b)*(s-c) < 0.0 then
--       dbms_output.put_line('sa ' || (s-a) || 'sb ' || (s-b) || ' sc ' || (s-c));
--       dbms_output.put_line('x0 ' || x0 || ' y0 ' || y0);
--       dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
--       dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2);
    
 
      area := sdo_geom.sdo_area(sdo_geometry(2003,8265,null,SDO_ELEM_INFO_ARRAY(1,1003,1),SDO_ORDINATE_ARRAY(x0,y0,x1,y1,x2,y2,x0,y0)),0.05,'unit=sq_meter');
  elsif Not(Is_geodetic(SRID)) then
      s := 0.5 *(a+b+c);
-- Hero's area formula gives the result: Area = sqrt(s*(s-a)*(s-b)*(s-c))
      area := sqrt(abs(s*(s-a)*(s-b)*(s-c))) ;   -- ABS is just for safety
  else
      area := 4.*GZ_UTIL_ZONE.fast_atan2(sqrt((c_tan(s*0.5)*c_tan((s-a)*0.5)*c_tan((s-b)*0.5)*c_tan((s-c)*0.5))),1)*R2; --*0.9999999; --*0.99999876;

  end if;
/*
      if c > b and c > a then
         check_it := a+b-c;
      elsif b > c and b > a then
         check_it := a+c-b;
      else
         check_it := b+c-a;
      end if;
-- Compare with an alternative method

      y := ABS(y0+y1)* 0.5;
      yc := y* deg2rad ;
      sinyc := sincos(yc,cosyc);

-- This empirical factor was found by comparing the results with Oracle's area
-- function SDO_GEOM.SDO_AREA. Results are amazing
--area  644648.8   new  644387.1  oarea  644612.1
--area 1298418.2   new 1297749.4  oarea 1298345.0
--area 1308953.6   new 1308423.3  oarea 1308879.2
--area 1304011.4   new 1303338.0  oarea 1303942.8
--area 1291397.9   new 1290872.1  oarea 1291322.3
--area 1296857.1   new 1296188.1  oarea 1296789.4
--area 1275412.2   new 1274890.0  oarea 1275332.5
--area 2541587.3   new 2540279.4  oarea 2541440.2
--area 7753026.7   new 7749860.4  oarea 7752539.1

      factor := 0.9965 + y0 *  y0 * 1.6E-06 + abs(y0) *7.6E-5;

      new_area := factor * to_meters * cosyc*(x0*y1 - y0*x1 + x1*y2 - y1*x2 + x2*y0 - y2*x0) ;
*/
--     if check_it < 0.04 then
--      oarea := sdo_geom.sdo_area(sdo_geometry(2003,8265,null,SDO_ELEM_INFO_ARRAY(1,1003,1),SDO_ORDINATE_ARRAY(x0,y0,x1,y1,x2,y2,x0,y0)),0.05,'unit=sq_meter');

--dbms_output.put_line('area ' || ROUND(area,6) ||' oarea ' || ROUND(oarea,6));

--      area := oarea;
--     end if;
 
RETURN area;

END Triangle_Area;
--
FUNCTION  SET_START_ELEMENTS(loop_begin PLS_INTEGER, loop_end PLS_INTEGER,
                           Nzone PLS_INTEGER, In_kount PLS_INTEGER,
                           B_TO_V     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                           Start_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE
                           )
                           RETURN MDSYS.SDO_LIST_TYPE AS

  Start_Elements           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

  pos              PLS_INTEGER;
  start_z          NUMBER;

BEGIN

   Start_elements.extend(Nzone+1);
-- describe the end by adding a non-existent zone
   Start_Elements(Nzone+1) := in_kount;
   for i in 1..Nzone Loop
      If i = 1 then
        pos := 1;
      Else
        start_z := Start_Zone(i);
        if start_z > 1 then
          pos := B_to_V(start_z);
-- The min_in_zone constraint can make zoen be one off
          if pos = Start_Elements(i-1) then
            pos := pos + 1;
          end if;
        else
          pos := 1;
        end if;
      end if;

      Start_Elements(i) := pos;
   End Loop;

  RETURN Start_Elements;

END Set_Start_Elements;
--
PROCEDURE GET_DEVIATIONS(max_allowed_deviation NUMBER,
                                  Vertex       IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  B_TO_V     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Xes          IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Yes          IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Inn_start PLS_INTEGER, Inn_End PLS_INTEGER,
                                  Nzone PLS_INTEGER,
                                  loop_begin PLS_INTEGER, loop_end PLS_INTEGER,
                                  Start_zone    IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  In_zone       IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Deviation     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Max_Deviation IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Area_under_Curve IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) AS

  /*****************************************************************************
--Program Name: get_deviations
--Author: Sidey Timmins
--Creation Date: 3/12/2008
--Updated:

--Usage:
--  This program has 5 required parameters:

 --   REQUIRED Parameters:
  --      INPUT
  --            max_allowed_deviation : maximum allowed deviation (distance
  --                from curve to approximating line segment).
  --            Vertex -- position of the vertices in the Bearings and with
  --                reference to Start_zone
  --            Xes and Yes: the coordinates of the curve
  --            Nzone: number of segments (zones) for this edge
  --            Start_zone: beginning of a found zone
  --            In_zone: number of elements in the zone
  --            Order_Zone: order zone was picked
  --            Deviation: average deviation
  --            Max_Deviation: maximum deviation from the curve to the
  --                 approximating line segemnt.


  --      OUTPUT
  --       Returns average and maximum deviations.

--Purpose:
  --  Measure distance (deviations) of one curve from another straight segment.

-- Reference:  "Measurements from Maps" DH Maling. Pergamon Press 1989.
-- Dependencies:  GZ_UTIL_ZONE.sincos, GZ_UTIL_ZONE.atan2,
-- GZ_UTIL_ZONE.search_greater,GZ_UTIL_ZONE.build_apolygon
--Called by: Find_VIPS
--
********************************************************************************
*/
  XYArray            MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  XYArray2           MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();

  Xess               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Yess               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

  areas              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Info_Array         MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY();
  Start_Elements     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  geometry           MDSYS.SDO_GEOMETRY;
  geometryp          MDSYS.SDO_GEOMETRY;
  geometryp2         MDSYS.SDO_GEOMETRY;
  geometry2          MDSYS.SDO_GEOMETRY;
  geometry_out       MDSYS.SDO_GEOMETRY;

  deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
  twopi     CONSTANT NUMBER  := 6.2831853071795864769252867665590057684; -- pi*2
  In_start         PLS_INTEGER;
  In_End           PLS_INTEGER;
  pos              PLS_INTEGER;
  pos2             PLS_INTEGER;
  start_z          NUMBER;

  base_dist        NUMBER;
  siny             NUMBER;
  cosy             NUMBER;
  cos2y            NUMBER;
  y                NUMBER;
  area             NUMBER;
  oarea            NUMBER;
  distance         NUMBER;
  odistance        NUMBER;
  gcd              NUMBER;
  max_distance     NUMBER;
  current_distance NUMBER;

  angle            NUMBER;
  sin_angle        NUMBER;
  cos_angle        NUMBER;
  x1               NUMBER;
  y1               NUMBER;
  x2               NUMBER;
  y2               NUMBER;
  xmid             NUMBER;
  ymid             NUMBER;
  xnew             NUMBER;
  ynew             NUMBER;
  area_above       NUMBER;
  area_below       NUMBER;

  BIG              NUMBER := 1.E12;
  xintersect       NUMBER;
  yintersect       NUMBER;
  xfrom            NUMBER;
  yfrom            NUMBER;
  max_allowed_dev  NUMBER := max_allowed_deviation * 0.75;
  my_angle         NUMBER;
  n                PLS_INTEGER;
  jj               PLS_INTEGER;
  m                PLS_INTEGER := 1;
  k                PLS_INTEGER;
  next             PLS_INTEGER := 1;
  delta            PLS_INTEGER;
  deltasave        PLS_INTEGER;
  tolerance        NUMBER := 1.E-12;
  result           VARCHAR2(4000);
  ksave            PLS_INTEGER;
  intersecting     BOOLEAN;
  sql_stmt         VARCHAR2(4000);
BEGIN
-- To check the generalized line we need an average deviation and a maximum
-- deviation (perpendicualr distance from point on original curve to new line).
-- Average Deviation (excursion from a straight line) = area/base.

-- We have to first generate intersections to measure areas correctly using the
-- herringbone method (Maling page 338).
--
--                n
--   Area = i/2 (Sum (xi * y(i+1) - x(i+1) * y(i))
--                i
--
--   Note that the area of this figure can be computed (vertex = +) as:
--
--     0.5 *( 0 * 1 - 1.75* 1 + 1.75*2 - 1 * 1 + 1 * 1 - 0 * 2
--        1.75 * 0 - 3.5 * 1 + 3.5 * 1 - 3 * 0 + 3 * 1 - 1.75 * 1 +
--        3 * 1 - 4 * 1 + 4 * 2 - 3.5 * 1 + 3.5 * 1 - 3 * 2) = 4/2 = 2
--
--           + (1,2)                 +  (3.5,2)
--          /   \                  /   \
--         /A=.875 \  (1.75,1)    / A=.5\              A = area
--  (0,1) +--------- + ----------+--------+ (4,1)
--                     \ A=.625 / (3,1)
--                        \    /
--                           + (3.5,0)
--
-- whereas the area of this one without the intermediate vertices cannot
-- (note absence of 2 vertices so polygon intersects itself).
--           +                       +
--          /  \                   /   \
--         /      \               /     \
--        +-------- \ ---------- /-------+
--                     \        /
--                        \    /
--                           +

      XYArray2.extend(4);
      Info_Array.extend(3);
      Info_Array(1) := 1;
      Info_Array(2) := 2;
      Info_Array(3) := 1;

--      For kk in 1..vertex.count loop
--        dbms_output.put_line(' vertex ' || vertex(kk));
--      end loop;
--  dbms_output.put_line(' loop begin ' || Loop_begin || ' end ' || loop_end);
--for i in 1..Nzone Loop
--   dbms_output.put_line(' start_zone ' || start_zone(i) || ' at i ' || i);
--end loop;
--    for ii in 1..B_to_V.count loop
--      dbms_output.put_line('ii ' || ii || ' BtoV ' || B_to_V(ii));
--    end loop;
 Start_Elements := GZ_UTIL_ZONE.SET_START_ELEMENTS(Inn_start, Inn_end,Nzone,Xes.count+1,B_TO_V,Start_zone);

--    for ii in 1..start_elements.count loop
--      dbms_output.put_line('ii ' || ii || ' start_el ' || start_elements(ii));
--    end loop;
  for i in 1..Nzone Loop

    if i = 1 then
       pos := 1;
    else
       pos := pos2;
    end if;

    if i < Nzone then
       pos2 := Start_Elements(i+1);
    else
       pos2 := Xes.count;
    end if;
--    for ii in 1..start_elements.count loop
--      dbms_output.put_line('ii ' || ii || ' start_el ' || start_elements(ii));
--    end loop;
--    dbms_output.put_line('pos ' || pos || ' pos2 ' || pos2 || ' nzone ' || nzone);
-- Calculate the base of the polygon (the distance from vertex1 to vertex 2 of
-- the generalized line)

      -- Adjust for meridians getter closer together
      y := (Yes(pos2)+Yes(pos)) * 0.5;
      siny := GZ_UTIL_ZONE.sincos(y*deg2rad,cosy);
      cos2y := cosy * cosy;
      base_dist := sqrt((Xes(pos2)-Xes(pos)) * (Xes(pos2)-Xes(pos)) * cos2y +
                   (Yes(pos2)-Yes(pos)) * (Yes(pos2)-Yes(pos)));

-- Build 2 XY arrays, the input curve XYArray, and the generalized segment
-- XYArray2
      XYArray.trim(XYarray.count);
      n := (pos2-pos+1)*2;

      XYArray.extend(n);
      XYArray2(1) := ROUND(Xes(pos),8);
      XYArray2(2) := ROUND(Yes(pos),8);
      XYArray2(3) := ROUND(Xes(pos2),8);
      XYArray2(4) := ROUND(Yes(pos2),8);


-- Set up a sampling delta (measure) at which to compute the perpendicular
-- distance from a point on the input curve to the generalized segment.
-- When we get up to 75% of the allowed deviation this delta reverts to 1
-- to ensure we get the maximum distance.

      m := pos+1;
      next := pos+1;
      delta := TRUNC((n/2) /20);
      if delta < 1 then
        delta := 1;
      End If;

      deltasave := delta;
      distance := BIG;
      max_distance := 0.;

-- An ad hoc method of computing the perpendicular distance from a point to a
-- edge (great or small circle) is derived here.
-- We rotate the edge clockwise (if angle is +ve) until it is an equator.
--                                 + (x2,y2)
--                               /
--                             /      angle  (measured from x-axis)
--                  (x1,y1) +--------------
--
--  Perform a local transformation rotating about the point (x1+x2)/2, (y1+y2)/2
--  on an axis going through this point and the center of the earth (assume spheroid)
      x1 := Xes(pos);
      y1 := Yes(pos);
      x2 := Xes(pos2);
      y2 := Yes(pos2);

      IF (pos2 - pos) = 1  or pos2 <= pos then
        deviation(i) := 0.;
        max_deviation(i) :=0.;

      ELSE
      angle :=  GZ_UTIL_ZONE.fast_atan2(y2-y1,x2-x1);
      if angle < 0. then
        angle :=  (twopi + angle);
      end if;
      sin_angle := GZ_UTIL_ZONE.sincos(angle,cos_angle);
      ymid := y1; --(y1+y2)*0.5;
      xmid := x1; --(x1+x2)*0.5;
      k := pos;

      While k <= pos2 Loop
-- Build a geometry. The rounds are to avoid a nasty error elsewhere in the program:
--                    ORA-03114: not connected to ORACLE
        XYArray((k-pos)*2+1) := ROUND(Xes(k),8);
        XYArray((k-pos)*2+2) := ROUND(Yes(k),8);

-- Check the maximum excursion (distance) every so often
        If m = next then
-- Rotation matrix with sign flipped for minus angle

ynew := ROUND((Yes(k) -ymid),8) * cos_angle - ROUND((Xes(k) -xmid),8)* sin_angle;
--             ynew := (Yes(k) -ymid) * cos_angle - (Xes(k) -xmid)* sin_angle;


--  Note: a Nautical mile is 1 minute of arc = 1852 m exactly !
--   References: http://en.wikipedia.org/wiki/Nautical_mile
--   References: http://en.wikipedia.org/wiki/Minute_of_arc

           distance := abs(ynew) *1852. * 60. ;

--           if distance > max_distance then
--                  xnew := (Xes(k) -xmid) * cos_angle + (Yes(k) -ymid)* sin_angle;
--                  xintersect := xnew;
--                  yintersect :=0.;
--                  xfrom := xnew;
--                  yfrom := ynew;
--                  ksave := k;

--               if pos = 90 then
--                  gcd := c_fast_vincenty_gcd(xfrom,yfrom,xintersect,yintersect);
--                 dbms_output.put_line('gcd :' || Round(gcd,4) || ' distance ' || Round(max_distance,4)  || 'jj ' || jj );
--             end If;
--            End If;
--        dbms_output.put_line('dist ' || distance);
-- save the maximum distance found so far.
           if distance > max_distance then
              max_distance := distance;
              if distance >= max_allowed_dev then
-- Backup by delta when we first detect a big excursion
                 if delta > 1 then
                    k := k-delta;
                    if k < pos then
                      k := pos;
                    End if;
                    next := k;
                 End if;
                 delta := 1;
               End If;
           elsif distance < max_allowed_dev then
               delta := deltasave;
           End if;
           m := m + delta;

        End If;
        next := next + 1;
        k := k + 1;
      End Loop;
--dbms_output.put_line('max dist ' || max_distance);
--dbms_output.put_line('GEOMETRY2 : ' || XYArray2(1) || ' ' || XYArray2(2));
--dbms_output.put_line('GEOMETRY2 : ' || XYArray2(3) || ' ' || XYArray2(4));

      geometry2 := MDSYS.SDO_GEOMETRY(2002,8265,NULL,Info_Array,XYArray2);
--      result := SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(geometry2,0.000001);
--      dbms_output.put_line('res ' ||result || XYArray2.count);
      geometry := MDSYS.SDO_GEOMETRY(2002,8265,NULL,Info_Array,XYArray);
--      result := SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(geometry,0.000001);
--       dbms_output.put_line('res ' ||result || XYArray.count);
--    geometryp := MDSYS.SDO_GEOMETRY(3001,8265,MDSYS.SDO_POINT_TYPE(xintersect,yintersect,NULL),NULL,NULL);
--    geometryp2 := MDSYS.SDO_GEOMETRY(3001,8265,MDSYS.SDO_POINT_TYPE(xfrom,yfrom,NULL),NULL,NULL);
--    EXECUTE IMMEDIATE  'SELECT SDO_GEOM.SDO_DISTANCE(:1,:2,0.0001,''unit=m'') from dual' into odistance
--        using geometryp,geometry2;

      sql_stmt := 'INSERT INTO POINTS (ID,GEOMETRY) VALUES (:1,:2)';
--      execute immediate sql_stmt using k,geometry2;
      sql_stmt := 'INSERT INTO POINTS (ID,GEOMETRY) VALUES (:1,:2)';
--      execute immediate sql_stmt using -k,geometry;
--      commit;

-- Finally using part of the original edge and the generalized segment,
-- build a polygon so we can compute the area. The area and the base then give
-- the average deviation from the original curve.

--      dbms_output.put_line('ZONE ' || i );
      geometry_out := GZ_UTIL_ZONE.build_Apolygon(geometry,geometry2,tolerance,cos_angle,sin_angle,xmid,ymid);

      if geometry_out is NULL Then
        dbms_output.put_line('WASS NULL: COUNT for XYArray2 ' || XYArray2.count || ' ' || XYArray.count);
         deviation(i) := 0.;
         max_deviation(i) := 0.;
      Else
      XYArray := geometry_out.SDO_ORDINATES;
      Xess.trim(Xess.count);
      Xess.extend(XYArray.count/2);
      Yess.trim(Yess.count);
      Yess.extend(XYArray.count/2);
      for k in 1..XYArray.count/2 Loop
         Xess(k) := XYArray(k*2-1);
         Yess(k) := XYArray(k*2);
      End Loop;
      area := Area_Along_line(1, Xess.count,Xess,Yess,area_above,area_below,TRUE,FALSE,TRUE);
      Area_under_Curve(i) := area;
--dbms_output.put_line('For i ' || i || ' area ' || ROUND(area,6) || ' base ' || base_dist || ' xes ' || XYArray.count);
--       EXECUTE IMMEDIATE  'SELECT SDO_GEOM.SDO_AREA(:1,0.001) from dual' into oarea using geometry_out;
-- dbms_output.put_line('area ' || ROUND(area,6) || ' oracle ' || ROUND(oarea,6));
      If base_dist <> 0 then
        deviation(i) := area/(base_dist * 60. * 1852.);
      End if;
      max_deviation(i) := max_distance;
--      dbms_output.put_line('angle ' || (angle*57.2958));
      End If;
      END IF;
 --for k in 1..XYArray2.count/2 loop
 --dbms_output.put_line('xx ' || XYarray2(k*2-1) || ' YY' || XYarray(k*2));
 --End Loop;
--  dbms_output.put_line('Deviation ' || deviation(i) || ' MAX deviation ' || max_deviation(i) || ' oracel '|| odistance || ' at k ' || ksave || ' xi ' || xintersect || ' yi ' || yintersect);
  End Loop;

END GET_DEVIATIONS;
--
FUNCTION FIND_MAX_DEVIATION(Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                            Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                            pos PLS_INTEGER,pos2 PLS_INTEGER,
                            area IN OUT NOCOPY NUMBER,
                            indexp IN OUT NOCOPY NUMBER) RETURN NUMBER AS
  twopi     CONSTANT NUMBER  := 6.2831853071795864769252867665590057684; -- pi*2
  distance         NUMBER := 0.;
  max_distance     NUMBER := 0.;
  angle            NUMBER;
  sin_angle        NUMBER;
  cos_angle        NUMBER;
  x1               NUMBER;
  y1               NUMBER;
  x2               NUMBER;
  y2               NUMBER;
  x1new            NUMBER;
  y1new            NUMBER;
  x2new            NUMBER;
  y2new            NUMBER;
  xmid             NUMBER;
  ymid             NUMBER;
  xnew             NUMBER;
  ynew             NUMBER;
BEGIN

     indexp := 0;
     area := 0.;
-- An ad hoc method of computing the perpendicular distance from a point to a
-- edge (great or small circle) is derived here.
-- We rotate the edge clockwise (if angle is +ve) until it is an equator.
--                                 + (x2,y2)
--                               /
--                             /      angle  (measured from x-axis)
--                  (x1,y1) +--------------
--
--  Perform a local transformation rotating about the point (x1+x2)/2, (y1+y2)/2
--  on an axis going through this point and the center of the earth (assume spheroid)
      x1 := Xes(pos);
      y1 := Yes(pos);
      x2 := Xes(pos2);
      y2 := Yes(pos2);


      IF (pos2 - pos) = 1  or pos2 <= pos then
         dbms_output.put_line('at null ' || pos || ' ' || pos2);
         NULL;
      ELSE
      angle :=  GZ_UTIL_ZONE.fast_atan2(y2-y1,x2-x1);
      if angle < 0. then
        angle :=  (twopi + angle);
      end if;
      sin_angle := GZ_UTIL_ZONE.sincos(angle,cos_angle);
      ymid := y1; --(y1+y2)*0.5;
      xmid := x1; --(x1+x2)*0.5;
      x1new := ROUND((y1 -ymid),8) * sin_angle + ROUND((x1 -xmid),8)* cos_angle;
      y1new := ROUND((y1 -ymid),8) * cos_angle - ROUND((x1 -xmid),8)* sin_angle;
      x2new := ROUND((y2 -ymid),8) * sin_angle + ROUND((x2 -xmid),8)* cos_angle;
      y2new := ROUND((y2 -ymid),8) * cos_angle - ROUND((x2 -xmid),8)* sin_angle;
      For k in pos+1..pos2-1 Loop

-- Check the maximum excursion (distance) every so often
-- Rotation matrix with sign flipped for minus angle

        ynew := ROUND((Yes(k) -ymid),8) * cos_angle - ROUND((Xes(k) -xmid),8)* sin_angle;
--             ynew := (Yes(k) -ymid) * cos_angle - (Xes(k) -xmid)* sin_angle;


--  Note: a Nautical mile is 1 minute of arc = 1852 m exactly !
--   References: http://en.wikipedia.org/wiki/Nautical_mile
--   References: http://en.wikipedia.org/wiki/Minute_of_arc

        distance := abs(ynew) *1852. * 60. ;
-- save the maximum distance found so far.
--        dbms_output.put_line('D ' || ROUND(distance,4) || ' max ' || max_distance);
        if distance > max_distance then
          max_distance := distance;
          xnew := ROUND((Yes(k) -ymid),8) * sin_angle + ROUND((Xes(k) -xmid),8)* cos_angle;
          area := GZ_UTIL_ZONE.Triangle_area(x1new,y1new,x2new,y2new,xnew,ynew);
          dbms_output.put_line('A ' || ROUND(area,4) );
          indexp := k;
        End if;

     End Loop;
     END IF;
     RETURN max_distance;
END;
--
PROCEDURE REVERSE_ORDINATES(XY IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                             inLB  PLS_INTEGER default 1,
                             InUB  PLS_INTEGER default 0)  AS
/*******************************************************************************
--Program Name: mk_reverse_ordinates
--Author: Sidey Timmins
--Creation Date: 03/27/2007

--Usage:
  --   REQUIRED Parameters:
  --         INPUT/OUTPUT
 --          XY           - SDO_ORDINATE_ARRAY to be reversed;
  --         LB           - place at which to begin reversing (lower limit- odd)
  --         UB           - place at which to end reversing (upper limit - even)
--Purpose:
  --  Reverse the coordinates for a single polygon to change clockwise to
  --  anticlockwise or visa versa.
  --  LB and UB may just describe part of the XY array.

  -- For example: LB=3 and UB = 10 will do this to an array originally filled
  -- with 1,2,3,4,5,6,7,8,9,10:     1,2,9,10,7,8,5,6,3,4

-- Reference: Original algorithm based upon only going half way thru an array!
-- Dependencies:  none
--
********************************************************************************
*/
   n         PLS_INTEGER;
   m         PLS_INTEGER;
   n1        PLS_INTEGER;
   n2        PLS_INTEGER;
   i2        PLS_INTEGER;
   tempx     NUMBER;
   tempy     NUMBER;
   LB        PLS_INTEGER := inLB;
   UB        PLS_INTEGER := inUB;

BEGIN
   If UB = 0 or UB >= XY.count then  UB := XY.count; End If;
   If LB <= 0 or LB >= UB then  LB := 1; End If;
   n := UB - LB + 1;

   If n > 2 then 

 -- Calculate the start n1, the end n2, and the swing point m
 
     n1 := trunc(LB/2)+1;
     m := UB + (n1 -1)*2;
     n2 := trunc(trunc(n/2)/2);
     n2 := n2 + n1 -1;
  
  -- Go throught the xy array reversing coordinates, two pairs at a time
  -- one pair below the midpoint and one above
  
     FOR i in n1..n2 LOOP
        i2 := i*2-1;
        tempx := xy(i2);
        tempy := xy(i2+1);
        xy(i2)   := xy(m-i2);
        xy(i2+1) := xy(m-i2+1);
        xy(m-i2)     := tempx;
        xy(m-i2+1)   := tempy;
     END LOOP;
  End if;

END REVERSE_ORDINATES;
--
PROCEDURE     SET_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0) AS
/*
********************************************************************************
--Program Name: set_mbr
--Author: Sidey Timmins
--Creation Date: 7/25/08

--Usage:
  -- This PL/SQL function has 7 parameters:
  --             XYs:      the input array of xy coordinates
  --             pLB, pUB  :      elements caller must specify to start and stop
  --                              at in the ordinate array
   Output        XLL,yLL,xUR,yUR  the returned MBR (Lower left, Upper right).
--
--                ---------+ (xUR,yUR)
--                |        |
--                |        |
--      (xLL,yLL) +--------+
--
--Purpose: This function determines the MBR of a geometry. It is about 20% faster
--         than Oracle sdo_geom.sdo_mbr.

--Dependencies: None

********************************************************************************
*/
   n       PLS_INTEGER := Xys.count;
   ii      PLS_INTEGER;
   LB      PLS_INTEGER := pLB;
   UB      PLS_INTEGER := pUB;

   inc     PLS_INTEGER;
   mid     PLS_INTEGER;
   loops   PLS_INTEGER :=2;
   m       PLS_INTEGER;

BEGIN

   If UB = 0 then
         UB := n;
   End if;
   if n = 0 then
      RETURN;
   end if;
   mid := TRUNC((LB + UB)/2);
   if TRUNC(mid/2) *2 <> mid then
        mid := mid +1;
       If mid = 1 then
         mid := 2;
       End if;
   End if;

   xLL := XYs(mid-1);
   yLL := XYS(mid);
   xUR := XYs(mid-1);
   yUR := XYs(mid);


   If n = 2 then
      RETURN;
   End if;
-- Loop twice using a comb to avoid a lot of wasted loads and stores

   inc := sqrt(UB - LB +1);

   if TRUNC(inc/2)*2 <> inc then
      inc := inc + 1;
   End if;
-- for less than 400 coordinates, brute force seems fastest
   if inc < 20 then
      inc := 2;
   End if;
   If inc = 2 then
     loops := 1;
   End If;

   For jj in 1..loops Loop

     m := TRUNC((UB-LB+1)/inc);
     inc := inc -1;
     ii := pLB  - inc;

-- Most of the time this loop doesn't do anything except additions and tests.

     For i in 1..m Loop
       ii := ii + inc;
       If XYs(ii) < xLL then
          xLL := XYs(ii);
       ElsIf XYs(ii) > xUR then
          xUR := XYs(ii);
       End If;
       ii := ii +1;
       If XYs(ii) < yLL then
         yLL := XYs(ii);
       ElsIf XYs(ii) > yUR then
         yUR := XYs(ii);
       End If;
     End Loop;
    inc := 2;

   End Loop;


END SET_MBR;
--
PROCEDURE STABLESORT2( Arr         IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Order_array   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       InLB          PLS_INTEGER default 1,
                       InUB          PLS_INTEGER default 0,
                       forwards      BOOLEAN default TRUE)
 AS
/*******************************************************************************
--Program Name: stablesort2
--Author: Sidey Timmins
--Creation Date: 01/04/2007
--Updated: 11/26/2010 S Timmins to use faster code from devbench
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 2 required parameters:
  --
  --   REQUIRED Parameters:
 --          Arr             - Input number Array to sort. Sort is ascending.

 --          Order_array     - Companion Array to be also sorted (usually
 --                            contains 1.. Arr.count. This array can be used
 --                            to sort other arrays:

 --                       FOR i IN 1..N LOOP
 --                          Order_array(i) := i;
 --                       END LOOP;

 --                       shellsort2(Data_Array,Order_array,1,n);

 --                       FOR i IN 1..N LOOP
 --                          Sorted_Array2(i) := Array2(Order_array(i));
 --                       END LOOP;

 --          LB              - lower bound of Arr to sort, defaults to 1
 --          UB              - upper bound of Arr to sort, defaults to Arr.count
--
--Purpose:
  -- Sorts 2 arrays and is stable when the order array has unique ascending values.
  -- Example
  --          input  arrays                          sorted output
  --    unsorted Nodes   Order_Array        Node                  Order_Array
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
  LBh              PLS_INTEGER;
  UB               PLS_INTEGER               := InUB;
  h                PLS_INTEGER;
  i                PLS_INTEGER;
  j                PLS_INTEGER;
  jmh              PLS_INTEGER;

  Ladd             INTEGER;
  valu             NUMBER;
  last             NUMBER;
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

-- Sort by insertion in increments of h

    While h > 0  LOOP

-- Sort by insertion in increments of h
     LBh := LB+h;
      For i IN LBh..UB LOOP
        valu := Arr(i);
        ladd := Order_array(i);
        j := i;
        jmh := j - h;
        WHILE ( j >= LBh) and (valu < Arr(jmh) or (Arr(jmh) = valu and ladd < Order_Array(jmh))) LOOP
           Arr(j) := Arr(jmh);
           Order_array(j) := Order_array(jmh);
           j := jmh;
           jmh := j - h;
        END LOOP;
        Arr(j) := valu;
        Order_array(j) := ladd;
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
        ladd := Order_array(i);
        j := i;
        jmh := j - h;
        WHILE ( j >= LBh) and (valu > Arr(jmh) or (Arr(jmh) = valu and ladd > Order_Array(jmh))) LOOP
           Arr(j) := Arr(jmh);
           Order_array(j) := Order_array(jmh);
           j := jmh;
           jmh := j - h;
        END LOOP;
        Arr(j) := valu;
        Order_array(j) := ladd;
      END LOOP;
     h := trunc(h / 3);
    End Loop;
   END IF;

End Stablesort2;
--
PROCEDURE XYBRESENHAM(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      SQRS IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      SEGS IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      inext IN OUT NOCOPY PLS_INTEGER,
                      iside IN OUT NOCOPY NUMBER,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      width IN OUT NOCOPY NUMBER) AS
/*
********************************************************************************
--Program Name: xybresenham
--Author: Sidey Timmins
--Creation Date: 3/5/2008
--Usage:
  -- Call this function from inside a PL/SQL program or directly from SQL.

  --   REQUIRED Parameter:
  --        INPUT
  --             Xys:  Array of xy ordinates
  --        OUTPUT
  --            SQRS:  Output array of cells (square numbers)
  --            SEGS:  Output array of segment numbers corresponding to SQRS
  --                   detailing the segment for each square.
  --            inext: Current size of SQRS (must be initialized to zero).
  --            iside: the number of cells along a side
  --            XLL,yLL the lower left of the data
  --            xUR,yUR: the upper right of the data
  --            width: width (extent of the data)

--Purpose:   -- produce an array of cell numbers corresponding to every
--              grid square the edge passes through (see wiki reference).
--              Written so a second line maybe gridded on a second call
--              with the same XLL,YLL,XUR,YUR.

-- Method:      Uses Bresenham's line algorithm to determine each square that the
--              input line passes through using comparisons rather than the
--              traditonal y = mx + b formula. Originally plotters had a pen
--              on a beam and had step motors to position the beam at various
--              grid squares or "points". The algorithm determines which points
--              in an 2-dimensional raster should be plotted in order to form a
--              close approximation to a straight line between two given points.
--              NOTE!! This procedure uses a variation of the Bresenham
--              algorithm to return the supercover line: when a line goes
--              through a corner it returns all 4 grid squares! See 2nd ref.
-- Reference:
--            Jack E Bresenham: "Algorithm for computer control of a digital
--            Plotter", IBM Systems Journal, Vol 4 # 1, 1965, pp 25-30.
--            http://lifc.univ-fcomte.fr/~dedu/projects/bresenham/index.html
--            http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
--
--Dependencies: none
********************************************************************************
*/
   amount  PLS_INTEGER := 1000;
   mult    NUMBER:= 1000000;
   nodec   NUMBER:= 6;
   cell    NUMBER;

   QSN     NUMBER;
   QSN2    NUMBER;

   A       NUMBER;
   B       NUMBER;
   x       NUMBER;
   y       NUMBER;

   x2      NUMBER;
   y2      NUMBER;
   d2x     NUMBER;
   d2y     NUMBER;
   ep      NUMBER;  -- the error previous
   xend    NUMBER;
   yend    NUMBER;

   d       NUMBER;
   dp      NUMBER;
   dq      NUMBER;
   dx      NUMBER;
   dy      NUMBER;

   BIG     NUMBER := 10000000.;
   i       PLS_INTEGER;
   bias    PLS_INTEGER := -3;
   n       PLS_INTEGER;
BEGIN

-- Grid each line with a grid 100 x 100.
--      iside := 40;  --100;
      n := XYs.count;
      If n = 0 then
        RETURN;
      end if;
-- Skip this on a 2nd call for an intersecting line
  If xLL is NULL THEN

      GZ_UTIL_ZONE.set_mbr(XYs,XLL,YLL,XUR,YUR);

   -- Figure out the largest delta
      dx := (ROUND(XUR,nodec) - ROUND(xLL,nodec))*mult;
      dy := (ROUND(YUR,nodec) - ROUND(yLL,nodec))*mult;

      iside := dx;
      if dy > dx then
         iside := dy;
      end if;
      iside := ROUND(iside/100000 + 0.5) * 50;
      if iside < 50 then
         iside := 50;
      elsif iside > 1000 then
         iside := 1000;
      end if;
dbms_output.put_line('dx ' || dx || ' dy ' || dy || ' iside ' || iside);
dbms_output.put_line('xLL ' || XLL || ' xUR ' || xUR);
dbms_output.put_line('yLL ' || yLL || ' yUR ' || yUR);
-- Calculate cell_width
      if dx > dy then
        width := TRUNC(dx/iside);
        if width * iside < dx then
          width := width + 1;
        end if;
        dx := dx/mult/iside;
        dy := dx;
      else
        width := TRUNC(dy/iside);
        if width * iside < dy then
         width := width + 1;
        end if;
        dy := dy/mult/iside;
        dx := dy;
      end if;
      if width < 1. then
        width := 1.;
      End if;
      SQRS.extend(amount);
      SEGS.extend(amount);
   ELSE
   -- we want to store coordinate numbers in the segment array
-- just set the bias on a 2nd call.
      bias := SEGS(SEGS.count);
--       dbms_output.put_line('xll is NOT NULL' || bias);
   END IF;


  -- We will use x,y to denote x1,y1 (the start of each segment)
   x := TRUNC((XYs(1) - xLL)* mult/width);
   y := TRUNC((XYs(2) - yLL)* mult/width);

dbms_output.put_line('width ' || width || ' dx ' || dx || ' mult ' || mult);
 dbms_output.put_line('xLL ' || XLL || ' YLL ' || yLL  );
   For ii in 2..TRUNC(n/2) LOOP
     i := ii*2;
-- This multiplication converts the coordinates to integers
     x2 := TRUNC((XYs(i-1) - xLL)* mult/width);
     y2 := TRUNC((XYs(i) - yLL)* mult/width);

-- Start saving QSNS
--    dbms_output.put_line('x1 ' || x || ' y1 ' || y || ' XYS ' || XYS(i-3) || ' ' || XYS(i-2));
--     dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2|| ' XYS ' || XYS(i-1) || ' ' || XYS(i));

       QSN := x + y * Iside +1;

       dx := ABS(x2-x);
       dy := ABS(y2-y);
       d2x := dx + dx;
       d2y := dy + dy;

       if d2x > (SQRS.count - inext-2 ) or d2y >(SQRS.count - inext-2 ) then
         if d2x > amount then
           amount := d2x;
         end if;
         if d2y > amount then
           amount := d2y;
         end if;
         SQRS.extend(amount);
         SEGS.extend(amount);
       End If;
       inext := inext +1;
       SQRS(inext) := QSN;
       SEGS(inext) := i+bias;

       QSN2 := x2 + y2 * Iside + 1;

--       if n = 4 then
--         dbms_output.put_line('ii ' || ii || ' QSN ' || qsn || ' seg ' || (i+bias) || ' qsn2 ' || qsn2);
--       end if;
-- When squares are adjacent (moving like a rook on a chessboard) there are no
-- in between squares.
-- We don't need to save QSN2 since it will saved on the next loop iteration

 --    IF QSN2 = QSN or ABS(QSN-QSN2) = 1 or ABS(QSN-QSN2) = iside THEN

       IF inext > 1 and (QSN2 = QSN and SEGS(inext) = SEGS(inext-1)) THEN
         NULL;
       ELSE
-- Case when delta y is less than delta x (less than 45 degrees)
     IF dy <= dx THEN

       if (x > x2) then
         A := -1;
       Else
         A := 1;
       end if;

       If (y > y2) then
         B := -1;
       Else
         B := 1;
       end if;

       ep := dx;
       d := dx;

           For j in 0..(dx-1) LOOP
              x := x + A;
              d := d + d2y;
              If  d > d2x then
                y := y + B;
                d := d -d2x;
                If (d + ep) < d2x then
                  cell := x + (y-B) * Iside + 1;
                  inext := inext +1;
                  SQRS(inext) := cell;
                  SEGS(inext) := i+bias;
                Elsif (d + ep) > d2x then
                  cell := (x-A) + y * Iside + 1;
                  inext := inext +1;
                  SQRS(inext) := cell;
                  SEGS(inext) := i+bias;
                End If;

--     dbms_output.put_line('X ' || x ||  ' y ' || y || ' x1 ' || x1 || ' y1 ' || y1 || ' Q' || QSN || ' A ' || A || ' x2 ' || x2 || ' y2 ' || y2  ||' dy ' || dy || ' dx ' || dx || ' xend ' || xend);
              End If;

             cell :=  x + y* Iside + 1;
             inext := inext +1;
             SQRS(inext) := cell;
             SEGS(inext) := i+bias;
             ep := d;
         END LOOP;

     ELSE
--     dy > dx (more than 45 degrees)
        if (y > y2) then
          A := -1;
        Else
          A := 1;
        end if;

        If (x > x2) then
          B := -1;
        Else
          B:= 1;
        end if;

        ep := dy;
        d := dy;

          For j in 0..(dy-1) LOOP
              y := y + A;
              d := d + d2x;
              If d > d2y then
                x := x + B;
                d := d - d2y;
                If (d + ep) < d2y then
                  cell := (x-B) + y* Iside + 1;
                  inext := inext +1;
                  SQRS(inext) := cell;
                  SEGS(inext) := i+bias;
                Elsif (d + ep) > d2y then
                  cell := x + (y-A) * Iside + 1;
                  inext := inext +1;
                  SQRS(inext) := cell;
                  SEGS(inext) := i+bias;
                End If;

              End If;
              cell := x +  y * Iside + 1;
              inext := inext +1;
              SQRS(inext) := cell;
              SEGS(inext) := i+bias;
              ep := d;
         END LOOP;
     END IF;
    END IF;
     x := x2;
     y := y2;

   END LOOP;

-- Output last vertex

    IF inext > 1 and (QSN2 <> SQRS(inext) or SEGS(inext) <> n+bias) then
      inext := inext +1;
      if inext >= SQRS.count then
        SQRS.extend(1);
        SEGS.extend(1);
      end if;
      SQRS(inext) := QSN2;
      SEGS(inext) := n+bias;
    END IF;

   SQRS.trim(SQRS.count-inext);
   SEGS.trim(SEGS.count-inext);


END XYBRESENHAM;
--
FUNCTION AREA_ALONG_LINE(In_start PLS_INTEGER, In_End PLS_INTEGER,
                         Xes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Yes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Area_above IN OUT NOCOPY NUMBER,
                         Area_below IN OUT NOCOPY NUMBER,
                         In_Degrees Boolean default TRUE,
                         pInUseProjection Boolean default FALSE,
                         check_area Boolean default FALSE
                                ) RETURN NUMBER
 AS
/*******************************************************************************
--Program Name: area_along_aline
--Author: Sidey Timmins
--Creation Date: 3/14/2008
--Updated:

--Usage:
--  This program has 4 required parameters:

 --   REQUIRED Parameters:
  --      INPUT
  ---     In_start, In_End : Start and end elements of the polygon in the
  --                        Xes, Yes arrays.
  --      Xes            - an array of X coordinates
  --      Yes            - an array of Y coordinates
  --      In_Degrees     - whether Xes, yes are in degrees
  --      pInUseProjection - whether to use a Lambert Azimuthal Equal Area projection

  --      OUTPUT
  --       Returns area of a simple (closed and not self-intersecting) polygon
  --       in square meters. If a projection
  --       is not used and the input is in degrees the result is multiplied by
  --       the cos of the latitude (to correct for the meridians getting closer
  --       towards the pole) and better match the result from SDO_GEOM.SDO_AREA .

--Purpose:
  --  Calculates area using the herringbone method.

--Reference:  "Measurements from Maps" DH Maling. Pergamon Press 1989, page 338
--Dependencies:  GZ_UTIL_ZONE.sincos
--
********************************************************************************
*/

  PolyInfo_Array    MDSYS.SDO_ELEM_INFO_ARRAY:= MDSYS.SDO_ELEM_INFO_ARRAY();
  PolyXys           MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  poly_geometry     MDSYS.SDO_GEOMETRY;

  pos              PLS_INTEGER  := 1;

  x0                NUMBER;
  y0                NUMBER;
  x1                NUMBER;
  y1                NUMBER;
  X00               NUMBER;
  Y00               NUMBER;
  X01               NUMBER;
  Y01               NUMBER;
  X11               NUMBER;
  Y11               NUMBER;
  xc                NUMBER;
  yc                NUMBER;
  y                 NUMBER;
  x1_0              NUMBER;
  cosx1_0           NUMBER;
  sinx1_0           NUMBER;
  siny1             NUMBER;
  cosy1             NUMBER;
  sinyc             NUMBER;
  cosyc             NUMBER;
  x1_c              NUMBER;
  cosx1_c           NUMBER;
  sinx1_c           NUMBER;
  kount             PLS_INTEGER;
  deg2rad      CONSTANT NUMBER    :=0.0174532925199432957692369076848861271344;
  sql_stmt          VARCHAR2(4000);
  total_area        NUMBER := 0.0;
  area              NUMBER := 0.0;
  R                 CONSTANT NUMBER := 6378137.0;
  to_meters         CONSTANT NUMBER := 6173827200.;
  g                 NUMBER;
  ksp               NUMBER;
  factor            NUMBER;
  xstart            NUMBER;
  ystart            NUMBER;
  px                PLS_INTEGER := 0;
  oarea             NUMBER := 0.;
  pos_area          NUMBER := 0;
  yfactor           NUMBER;
  dist_factor       CONSTANT NUMBER := 6196014515.236907424901819538; --111319.490793274^2 * 0.5;
BEGIN


   kount := In_End - In_Start +1;
   area_above := 0.0;
   area_below := 0.0;
   If Xes.count = 0 then
      RETURN 0;
   End If;
   PolyXys.extend(kount*2+2);
   PolyInfo_array.extend(3);
   Pos := In_Start;
--   dbms_output.put_line('POS ' || pos || ' xes.count ' ||Xes.count || ' inend ' ||IN_End);
   x0 := Xes(pos);
   y0 := Yes(pos);
   xstart := x0;
   ystart := y0;
         px := px + 1;
       PolyXys(px) := x0;
        px := px + 1;
       PolyXys(px) := y0;
--              if kount = 19 then
--dbms_output.put_line('area ' || area || ' x0 ' || round(x0,6) || ' y0 ' || round(y0,6) || ' x1 ' || round(x1,6) || ' y1 ' || round(y1,6) );
--      end if;
   pos := pos + 1;
         y := ABS(Yes(In_Start)+Yes(In_End))* 0.5;
        yc := y* deg2rad ;
        sinyc := GZ_UTIL_ZONE.sincos(yc,cosyc);
    IF pInUseProjection = TRUE THEN
-- Convert to radians and then do a Lambert Azimuthal Equal Area projection
      x0 := x0* deg2rad;
      y0 := y0 * deg2rad;
      xc := (x0+Xes(In_End)* deg2rad) * 0.5;
      yc := (y0+Yes(In_End)* deg2rad) * 0.5;
      sinyc := GZ_UTIL_ZONE.sincos(yc,cosyc);
      x1_c := (x0-xc);

      siny1 := GZ_UTIL_ZONE.sincos(y0,cosy1);
      sinx1_c := GZ_UTIL_ZONE.sincos(x1_c,cosx1_c);

--Lambert Azimuthal Equal Area forward equations--mapping lat,long to x,y
-- g = sin_center_lat * sin(lat) + cos_center_lat * cos(lat) * cos(dlon)
      g := sinyc * siny1 + cosyc * cosy1 * cosx1_c;
      ksp := R  * sqrt(2./ (1.0 + g));
-- Project X1, Y1
-- X = ksp * cos(lat) * sin(dlon)
-- Y = ksp * ( cos_center_lat * sin(lat) - sin_center_lat * cos(lat) * cos(dlon)

        X01 := ksp*cosy1 * sinx1_c;
        Y01 := ksp*(cosyc * siny1 - sinyc * cosy1 * cosx1_c);
        X00 := X01;
        Y00 := Y01;

  END IF;
            polyInfo_array(1) := 1;
            polyInfo_array(2) := 1003;
            polyInfo_array(3) := 1;
   While pos <= In_End LOOP
    IF pInUseProjection = TRUE THEN

-- Convert to radians and then do a a Lambert Azimuthal Equal Area projection
        x1 := Xes(pos) * deg2rad;
        y1 := Yes(pos) * deg2rad;
        x1_c := (x1-xc);

        siny1 := GZ_UTIL_ZONE.sincos(y1,cosy1);
        sinx1_c := GZ_UTIL_ZONE.sincos(x1_c,cosx1_c);


-- g = sin_center_lat * sin(lat) + cos_center_lat * cos(lat) * cos(dlon)
        g := sinyc * siny1 + cosyc * cosy1 * cosx1_c;
        ksp := R * Sqrt(2. / (1.0 + g));
-- Project X1, Y1
-- X = ksp * cos(lat) * sin(dlon)
-- Y = ksp * ( cos_center_lat * sin(lat) - sin_center_lat * cos(lat) * cos(dlon)

        X11 := ksp*cosy1 * sinx1_c;
        Y11 := ksp*(cosyc * siny1 - sinyc * cosy1 * cosx1_c);
-- Calculate areas correctly using the herringbone method (Maling page 338).
        area := area + (x00 * y11   - x11 * y00);

        X00 := X11;
        Y00 := Y11;
     Else
-- Calculate areas correctly using the herringbone method (Maling page 338).
--
--                n
--   Area = i/2 (Sum (xi * y(i+1) - x(i+1) * y(i))
--                i
--
--   Note that the area of this figure can be computed (vertex = +) as:
--
--     0.5 *( 0 * 1 - 1.75* 1 + 1.75*2 - 1 * 1 + 1 * 1 - 0 * 2
--        1.75 * 0 - 3.5 * 1 + 3.5 * 1 - 3 * 0 + 3 * 1 - 1.75 * 1 +
--        3 * 1 - 4 * 1 + 4 * 2 - 3.5 * 1 + 3.5 * 1 - 3 * 2) = 4/2 = 2
--
--           + (1,2)                 +  (3.5,2)
--          /   \                  /   \
--         /A=.875 \  (1.75,1)    / A=.5\              A = area
--  (0,1) +--------- + ----------+--------+ (4,1)
--                     \ A=.625 / (3,1)
--                        \    /
--                           + (3.5,0)
       x1 := Xes(pos);
       y1 := Yes(pos);

    if x1 = xstart and y1 = ystart then
       px := px + 1;
       PolyXys(px) := x1;
        px := px + 1;
       PolyXys(px) := y1;
         area := area + (x0 * y1   - x1 * y0 );
        PolyXys.trim(PolyXys.count-px);

         poly_geometry := MDSYS.SDO_GEOMETRY(2003,8265,NULL,PolyInfo_Array,PolyXYs);
         pos_area := SDO_GEOM.SDO_AREA(poly_Geometry,0.5,'unit=sq_meter');
         if pos_area > 2000000000000  then
--          dbms_output.put_line('PX ' || px || ' area ' || pos_area);
         GZ_UTIL_ZONE.reverse_ordinates(polyXys, 1,px);
         poly_geometry := MDSYS.SDO_GEOMETRY(2003,8265,NULL,PolyInfo_Array,PolyXYs);
         end if;
 --         sql_stmt := 'INSERT INTO POINTS (ID,GEOMETRY) VALUES (:1,:2)';
 --     execute immediate sql_stmt using px,poly_geometry;
 --      oarea := oarea + SDO_GEOM.SDO_AREA(poly_Geometry,0.5,'unit=sq_meter');
--       dbms_output.put_line('px ' || px || ' Oarea ' || SDO_GEOM.SDO_AREA(poly_Geometry,0.00005,'unit=sq_meter'));
--      if px = 70 or px = 68 then
--      sql_stmt := 'INSERT INTO POINTS (ID,GEOMETRY) VALUES (:1,:2)';
--      execute immediate sql_stmt using px,poly_geometry;
--      commit;
--      end if;
      Polyxys.trim(PolyXys.count);
      PolyXys.extend(kount*2+2);
      px := 0;
      total_area := total_area + area;
      area := 0;
         pos := pos + 1;

-- move to the start of the next polygon
         if pos < in_end then

            x1 := Xes(pos);
            y1 := Yes(pos);
            xstart := x1;
            ystart := y1;
         end if;
    else
         area := area + (x0 * y1   - x1 * y0 );
    end if;

       px := px + 1;
       if px > PolyXys.count then
          dbms_output.put_line('px ' || px || 'poly ' || polyXys.count || ' kount ' || kount || ' xes ' || xes.count);
       end if;
       PolyXys(px) := x1;
        px := px + 1;
       PolyXys(px) := y1;
       x0 := x1;
       y0 := y1;

     End If;
     pos := pos + 1;

  END LOOP;
  if x0 <> xstart or y0 <> ystart then
        px := px + 1;
       PolyXys(px) := x1;
        px := px + 1;
       PolyXys(px) := y1;
  end if;
-- Add last Product
   IF pInUseProjection = TRUE THEN
      area := 0.5*(area + (x00 * y01   - x01 * y00));
   ELSE


      IF In_degrees = FALSE THEN
        if x0 = xstart and y0 = ystart then
           total_area := 0.5*(total_area +area);
        else
           total_area := 0.5*(total_area +area + (x0 * Yes(In_Start))  - ( y0 * Xes(In_start))) ;
        end if;
      ELSE
        y := ABS(Yes(In_Start)+Yes(In_End))* 0.5;
        yc := y* deg2rad ;
        sinyc := GZ_UTIL_ZONE.sincos(yc,cosyc);
-- This empirical factor was found by comparing the results with Oracle's area
-- function SDO_GEOM.SDO_AREA. Results are amazing
--        factor := 0.997 + Yes(in_Start) *  Yes(in_Start) * 3.5E-06 - abs(yes(In_Start)) *0.9E-6;
--        factor := 0.9968 + Yes(in_Start) *  Yes(in_Start) * 3.47E-06 - abs(yes(In_Start)) *2.2E-6;
---        factor := 0.9969 + Yes(in_Start) *  Yes(in_Start) * 3.485E-06 - abs(yes(In_Start)) *2.4E-6;
        factor := 0.9965 + Yes(in_Start) *  Yes(in_Start) * 1.6E-06 + abs(yes(In_Start)) *7.6E-5;
--        cosyc := cosyc* (0.9999945+ sinyc*sinyc*0.00335914);
--        yfactor := 0.9932923 + (1119.22*sinyc*sinyc)/dist_factor;
--        factor := A + Yes(in_Start) *  Yes(in_Start) * B - abs(yes(In_Start)) *C;
   --     factor := 0.9969 + Yes(in_Start) *  Yes(in_Start) * 3.37E-06 - abs(yes(In_Start)) *0.97E-6;
--        if x0 = xstart and y0 = ystart then
--           total_area := factor * to_meters * cosyc*(total_area + area);
--        else
-- User described an invalid polygon without the last vertex = start vertex
          if x0 = xstart and y0 = ystart then
            total_area := factor * to_meters * cosyc*(total_area +area);
--            total_area := dist_factor * cosyc*(total_area +area);
          else
-- User passed an incomplete polygon
--         dbms_output.put_line('area incomplete');

--dbms_output.put_line('x0 ' || x0 || ' y0 ' || y0);
--dbms_output.put_line('xs ' ||Xes(in_Start) || ' ys ' || yes(in_start));
          total_area := factor * to_meters * cosyc*(total_area +area + (x0 * Yes(In_Start))  - (y0 * Xes(In_start))) ;
--                     total_area := dist_factor * cosyc*(total_area +area + (x0 * Yes(In_Start))  - (y0 * Xes(In_start))) ;
          end if;
--        end if;
      End If;

   END IF;
-- Anti-Clockwise areas are positive. Area will be negative if the order of
-- traversal is clockwise.
   total_area := Abs(total_area);
--        if check_area and oarea <> 0. and abs(oarea -total_area)/oarea > .1 then
--        dbms_output.put_line('AREA is ' || ROUND(total_area,5) || ' oarea ' || ROUND(oarea,5) || ' DIFF ' || ROUND(oarea-total_area,4));
--        end if;
--   dbms_output.put_line('returning area ' || total_area || ' for ' ||in_start || ' to ' || in_end);
  RETURN total_area;


END AREA_ALONG_LINE;
--
FUNCTION ATAN2(YIn NUMBER, XIn NUMBER) RETURN NUMBER
 Deterministic IS
/*
**************************************************************************************
--Program Name: atan2
--Author: Sidey Timmins
--Creation Date: 11/16/2006
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has two parameters:
  --
  --   REQUIRED Parameters:
  --            Y:  Y ordinate as per convention Y first!
  --            X:  X abscissa
  --
--Purpose:   -- Calculates the arctan function -  atan2(y,x)
--              as fast as possible (20 times faster than the built in
--              function atan2 which takes 4 seconds for 20000 values) with
--              an accuracy of 13 to 14 decimal digits.
--              The domain is unlimited. (Note this function gives no error
--              and returns zero when both x and y are zero.)
--
-- Method:      where x = ABS(Yin/XIn) the raw atan is:
--         atan(x)= x(c1 + c2*x**2 + c3*x**4)/(c4 + c5*x**2 + c6*x**4 + x**6)
--         and then it its adjusted for the quadrant.

-- Accuracy:  The maximum error in the range - two pi to two pi is less than
--            6E-15. The relative errors is < 1.E-15, that is
--
--                      atan2/arctan > (1. - 1.E-15) and < (1. + 1.E-15)
--
--            It occurs when y/x = -0.594
--    Oracle  atan2 : -1.3090077412598893409607651149280456611   correct
--           c_atan2: -1.30900774125989456555301528465713930015  very close
--             error   0.00000000000000522459225016972909363905

-- Reference: http://www.ganssle.com/approx/approx.pdf
--            "A Guide to Approximations" by Jack G Ganssle
--
--Dependencies: None but see c_test_atan2 for rigorous testing.
***************************************************************************************
*/


  pi        CONSTANT NUMBER             := 3.1415926535897932384626433832795028842;
  piByTwo   CONSTANT NUMBER             := 1.5707963267948966192313216916397514421;
  piBy6     CONSTANT NUMBER             := 0.5235987755982988730771072305465838140;
  tanpiby6  CONSTANT NUMBER             := 0.5773502691896257645091487805019574557;
  tanpiby12 CONSTANT NUMBER             := 0.2679491924311227064725536584941276331;


  c1        CONSTANT NUMBER             := 48.70107004404898384;
  c2        CONSTANT NUMBER             := 49.5326263772254345;
  c3        CONSTANT NUMBER             := 9.40604244231624;
  c4        CONSTANT NUMBER             := 48.70107004404996166;
  c5        CONSTANT NUMBER             := 65.7663163908956299;
  c6        CONSTANT NUMBER             := 21.587934067020262;
  x         NUMBER;

  result    NUMBER;
  x2         NUMBER;

  complement BOOLEAN           := FALSE;
  region     BOOLEAN           := FALSE;


BEGIN

/* arctan2(Y,X) is the quadrant-correct arc tangent atan(Y/X).  If the
   denominator X is zero, then the numerator Y must not be zero.  All
   arguments are legal except Y = X = 0. */

-- check for error conditions
  IF XIn = 0.0 THEN
    IF YIn = 0.0 THEN
      RETURN  0.0;    -- We return zero although arctan(0,0) is undefined
    ELSIF YIn < 0.0 THEN
      RETURN -pi * 0.5;
    ELSE
      RETURN pi * 0.5;
    END IF;
  END IF;


  IF ABS(YIn) > ABS(XIn) THEN
     x   := ABS(XIn)/ABS(YIn);
     complement := TRUE;
  ELSE
     x   := ABS(YIn/XIN);
  END IF;

-- reduce arg to under tan(pi/12)
  if (x > tanpiby12) THEN
     x := (x-tanpiby6)/(1.0 +tanpiby6*x);
     region := TRUE;
  end if;

  x2 := x * x;
-- Use Horner's rule to evaluate
  result := (x*(c1 + x2*(c2 + x2*c3))/(c4 + x2*(c5 + x2*(c6 + x2))));

  IF region = TRUE THEN
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
FUNCTION FAST_ATAN2(YIn NUMBER, XIn NUMBER) RETURN NUMBER Deterministic IS
/*
**************************************************************************************
--Program Name: fast_atan2
--Author: Sidey Timmins
--Creation Date: 11/16/2006
--Updated: March 4,2010
--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has two parameters:
  --
  --   REQUIRED Parameters:
  --            Y:  Y ordinate as per convention Y first!
  --            X:  X abscissa
  --
--Purpose:   -- Calculates the arctan function -  atan2(y,x)
--              as fast as possible (20 times faster than the built in
--              function atan2 which takes 4 seconds for 20000 values) with
--              an accuracy of 10 decimal digits.
--              The domain is unlimited. (Note this function gives no error
--              and returns zero when both x and y are zero.)
--
-- Method:      First the range is reduced and then the arctangent is computed
--              using a Taylor series. Take  x = ABS(Yin/XIn) the raw atan is:
--         atan(x) = x -x^3/3 + x^5/5 - x^7/7 + x^9/9.
--              This slow converging series is more accurate than the Ganssle
--         formula:
--         atan(x)= x(c1 + c2*x**2 )/(c3+c2)
--         The result is then adjusted for the quadrant.

-- Accuracy:  The maximum error in the range - two pi to two pi is less than
--            7E-11.

-- Reference: http://www.ganssle.com/approx/approx.pdf
--            "A Guide to Approximations" by Jack G Ganssle
--             http://myweb.lmu.edu/hmedina/Papers/Arctan.pdf
--            "A Sequence of Polynomials for Approximating Arctangent" by Herbert A Medina
--Dependencies: None but see c_test_atan2 for rigorous testing.
***************************************************************************************
*/


  pi        CONSTANT NUMBER        := 3.1415926535897932384626433832795028842;
  piByTwo   CONSTANT NUMBER        := 1.5707963267948966192313216916397514421;
  piBy6     CONSTANT NUMBER        := 0.5235987755982988730771072305465838140;
  piBy12    CONSTANT NUMBER        := 0.2617993877991494365385536152732919070167;
  tanpiby6  CONSTANT NUMBER        := 0.5773502691896257645091487805019574556545;
  tanpiby12 CONSTANT NUMBER        := 0.2679491924311227064725536584941276330567;
  tanpiby24 CONSTANT NUMBER        := 0.1316524975873958534715264574097171035924;

  c1        CONSTANT NUMBER             := 1.6867629106;
  c2        CONSTANT NUMBER             := 0.4378497304;
  c3        CONSTANT NUMBER             := 1.6867633134;
  a3        CONSTANT NUMBER  := 0.3333333333333333333333333333333333333333;
  a5        CONSTANT NUMBER  := 0.2;
  a7        CONSTANT NUMBER  := 0.1428571428571428571428571428571428571429;
  a9        CONSTANT NUMBER  := 0.1041666666666666666666666666666666666667;
  --a10       NUMBER  := 0.05;
  --a11       NUMBER  := 0.2443181818181818181818181818181818181818;
  --a12       NUMBER  := 0.25;
  --a13       NUMBER  := 0.12980769230769230769230769230769230769230;
  --a14       NUMBER  := 0.0357142857142857142857142857142857142857;
  --a15       NUMBER  := 0.004166666666666666666666666666666666666667;
  x         NUMBER;

  result    NUMBER;
  x2        NUMBER;
  x3        NUMBER;
  x4        NUMBER;
  x5        NUMBER;

  complement BOOLEAN           := FALSE;
  region    NUMBER := 0;
  region2   NUMBER := 0;

BEGIN

/* arctan2(Y,X) is the quadrant-correct arc tangent atan(Y/X).  If the
   denominator X is zero, then the numerator Y must not be zero.  All
   arguments are legal except Y = X = 0. */

-- check for error conditions
  IF XIn = 0.0 THEN
    IF YIn = 0.0 THEN
      RETURN  0.0;    -- We return zero although arctan(0,0) is undefined
    ELSIF YIn < 0.0 THEN
      RETURN -pi * 0.5;
    ELSE
      RETURN pi * 0.5;
    END IF;
  END IF;


  IF ABS(YIn) > ABS(XIn) THEN
     x   := ABS(XIn)/ABS(YIn);
     complement := TRUE;
  ELSE
     x   := ABS(YIn/XIN);
  END IF;

-- reduce arg to under tan(pi/12)
  if (x > tanpiby12) THEN
     x := (x-tanpiby6)/(1.0 +tanpiby6*x);
     region := 1.;
  end if;
-- reduce arg to under tan(pi/24)
   if (x > tanpiby24) THEN
     x := (x-tanpiby12)/(1.0 +tanpiby12*x);
     region2 := 1.;
  elsif  ( x < -tanpiby24) THEN
     x := (x+tanpiby12)/(1.0 -tanpiby12*x);
     region2 := -1.;
  end if;
-- dbms_output.put_line('region2 ' || region2 || ' x ' || x);
  x2 := x * x;
  if x >-0.15 and x <= 0.15 then
    x3 := x * x2;
    x5 := x3 * x2;
    x4 := x2 * x2;
-- Use Taylor Series to evaluate
    result := x- x3*(a3 - x2*a5 + x4*a7) + x5*x4*a9; -- +x*a10 -x2*a11+ x3*a12-x4*a13 +x5*a14 - x3*x3*a15);
  else
-- Use Horner's rule to evaluate
    result := (x*(c1 + x2*c2)/(c3 + x2));
  end if;

  result  := result + region*piby6;
  result  := result + region2*piby12;

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


END FAST_ATAN2;
--
FUNCTION NEW_BEARINGS_ALONG_ALINE(measure NUMBER,
                                In_start PLS_INTEGER, IN_End PLS_INTEGER,
                                XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                                Mapto_Vertex     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                B_to_V IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                pred_bcount IN OUT NOCOPY NUMBER
                                ) RETURN MDSYS.SDO_LIST_TYPE
 AS
 
 Bearings     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
 Distances    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
 Diffs        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
 Prediction   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
 RS_Bearings     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
 x0           NUMBER;
 y0           NUMBER;
 x1           NUMBER;
 y1           NUMBER;
 check_it     NUMBER;
 delta         NUMBER;
 ds           NUMBER;
 dist         NUMBER;
 max_change   NUMBER := 135.;
 recip_measure NUMBER := 1./measure;
 hi_check     NUMBER;
 last_angle   NUMBER;
 last_quad_angle NUMBER;
 diff         NUMBER;
 angle        NUMBER;
 b0           NUMBER;
 b1           NUMBER;
 b2           NUMBER;
 b3           NUMBER;
 bearing      NUMBER;
 bpred        NUMBER;
 last_vnumber NUMBER;
 place        NUMBER;
 fraction     NUMBER;
 amount       NUMBER;
 bmin         NUMBER := 0.0;
 bsave        NUMBER;
 bias         NUMBER :=0.0;
 last        NUMBER;
 lastd        NUMBER;
 bcount       NUMBER;
 left_over    NUMBER := 0.0;
 last_bias    NUMBER;
 d            NUMBER;
 bmax         NUMBER;
 bsum         NUMBER;
 anti_clock   NUMBER;
 bearing_avg  NUMBER;
 
 len         NUMBER :=0.0;
 no_to_do     PLS_INTEGER;
 jj           PLS_INTEGER :=In_start;
 n            PLS_INTEGER := TRUNC((In_end-In_start+1)/2);
 m            PLS_INTEGER;
 pos          PLS_INTEGER:= 1;
 dpos         PLS_INTEGER;
 pp           PLS_INTEGER :=0;
 lastp        PLS_INTEGER :=1;
 start_bend   PLS_INTEGER :=1;
 start_place  PLS_INTEGER :=1;
 odd          PLS_INTEGER := 1;
 last_bend    PLS_INTEGER;
 next         PLS_INTEGER := 0;
 bad          PLS_INTEGER := 0;
 n2000        PLS_INTEGER := 1000;
 trap         BOOLEAN := FALSE;
 Continuous   BOOLEAN;
BEGIN

-- Set up the Bearings from the azimuths on the beginning of a great circle
--
-- Our convention will be that bearings begin at a vertex whereas distances
-- end at the next vertex. So Distance(1) =0 and Bearings(1) is the azimuth
-- from vertex 1 to vertex 2.

 Bearings.extend(n+1);
 Distances.extend(n+1);
 
 Distances(1) := 0.;
 x1 := Xys(jj);
 y1 := Xys(jj+1);
 For ii in 2..n Loop
    jj:= jj+2;
    x0 := x1;
    y0 := y1;
    x1 := Xys(jj);
    y1 := Xys(jj+1);
    
-- Each vertex at each distance has a bearing
-- Skip duplicate vertices !!

    Dist := Accurate_GCD(x0,y0,x1,y1);
    If Dist > 0.0 then
      pos := pos+1;
      Distances(pos) := Dist;
      len := len+Dist;
      angle := Geo_bearing(x0,y0,x1,y1);
--      if ii >=320 and ii<=360 then
--         dbms_output.put_line('ii ' || ii || ' x0 ' || round(x0,9) || ' y0 ' || round(y0,9) || ' B ' || round(bearings(pos-1),4));
--      end if;

-- Unwrap the phase. We do this because we need to compare similar bearings
-- near 360 that have wildly different values. For example, we know that 10 degrees
-- is not far from 359 degrees. All angles here are measure counterclockwise 
-- from th X-axis.

-- Unwrapping phase is not a simple method - and althoug this is robust
-- and simple, it is not necessarily exact. Perhaps we should test > 170
-- degrees for example?

       if (last-angle) > 175. then
             bias := bias + 360.0;
       elsif (last-angle) < -175. then
             bias := bias - 360.0;
       end if;
--       if ii < 20 then
--          dbms_output.put_line('Ii ' || ii || ' last ' || round(last,2) || ' angle ' || round(angle,2) || ' bias ' || bias);
--       end if;
     last := angle;
     angle := angle+bias;
     Bearings(pos-1) := angle;
     if angle < bmin then bmin :=  angle; end if;
    end if;
 End Loop;

 Bearings(pos) := Bearings(pos-1);
 
  -- Ensure all bearings are positive!
    
 if bmin < 0.0 then
     bmin := ABS(bmin);
     bsave := bmin;
     bmin := TRUNC(bmin/360.)*360.;
     if bmin < bsave then
        bmin := bmin + 360.;
     end if;
    For ii in 1..pos loop
     Bearings(ii) := Bearings(ii) +bmin;
    End loop;
 end if;
 
-- for ii in 1..pos loop --pos loop
--   dbms_output.put_line('ii ' || ii|| ' Brng ' ||round(bearings(ii),3) || ' D ' || round(Distances(ii),3)); 
--  end loop;
 m := len/measure +0.5;
-- dbms_output.put_line('N ' || n || ' M ' || m || ' pos ' || pos || ' d ' || len);
 
 -- For a 2 vertex line, we are done
 
 if pos = 2 or m < 2 then
   RS_Bearings.extend(1);
   RS_Bearings(1) := Bearings(1);
   B_to_V.extend(1);
   B_to_V(1) := 1.;
 else

-- Test if this is a river and we can predict the next bearing
-- Our prediction is in the past so we actually know it:
--            0    1    2    3     4
--                      ^ try and predict middle value at position 2.

   Prediction.extend(pos);
   Prediction(1) := Bearings(1);
   Prediction(2) := Bearings(2);
   b0 := Bearings(1);
   b1 := b0;
   b2 :=  b0;
   b3 := b0;
   pred_bcount := 0;
   
   For ii in 2..pos Loop
--       bpred :=  angle2 + 0.5*(angle1*4.-angle2 -3.*angle0);
        bearing := Bearings(ii);
--        bmax := b0;
--        if b1 > bmax then bmax := b1; end if;
--        if b2 > bmax then bmax := b2; end if;
--        if b3 > bmax then bmax := b3; end if;
--        if bearing > bmax then bmax := bearing; end if;
--        ok := TRUE;
--        if (b0 <= 90. or b1 <= 90. or b2 <= 90. or b3 <= 90. or bearing <= 90.) and bmax >= 270. then
--           if b0 >= 270. then b0 := -360.+b0; end if;
--           if b1 >= 270. then b1 := -360.+b1; end if;
--           if b2 >= 270. then b2 := -360.+b2; end if;
--           if b3 >= 270. then b3 := -360.+b3; end if;
--           if bearing >= 270. then bearing := -360. +bearing; end if;
--           ok := FALSE;
--        end if;
-- We need to quick handling of region 1 (0-90) and region 4 (270-360)
-- This formula presumes equispaced samples which we do not have.        
        bpred := (b1*4. + b3*4. - bearing - b0)/6.;
        if ii > 4 then
          Prediction(ii-2) := bpred;
        end if;
--        dbms_output.put_line('  pos ' ||pos || ' bear ' || ROUND(angle2,4) || ' bpred ' || round(bpred,4) || ' last  ' || round(angle1,4));
       if ii > 4 and abs(b2-bpred) < 25. then
         pred_bcount := pred_bcount + 1.;
--           dbms_output.put_line('  POS ' ||pos || ' bear ' || ROUND(b2,4) || ' bpred ' || round(bpred,4) || ' b0  ' || round(b0,4) || ' b1 ' || round(b1,4) || ' b3 ' || round(b3,4));
      else
         bad := bad + 1;
--         dbms_output.put_line('  POS ' ||pos || ' bear ' || ROUND(b2,4) || ' bpred ' || round(bpred,4) || ' b0  ' || round(b0,4) || ' b1 ' || round(b1,4) || ' b3 ' || round(b3,4));
       end if;
--       if NOT ok then
--           if b0 <0. then b0 := 360.+b0; end if;
--           if b1 <0. then b1 := 360.+b1; end if;
--           if b2 <0. then b2 := 360.+b2; end if;
--           if b3 <0. then b3 := 360.+b3; end if;
--           if bearing <0. then bearing := 360. +bearing; end if;
--        end if;
    b0 := b1;
    b1 := b2;
    b2 := b3;
    b3 := bearing;
  End Loop;


-- We now have to interpolate at a regular sample interval (measure) to
-- produce the desired output, regularly sampled (RS) bearings with their
-- appropriate vertices stored in B_to_V (a mapping of bearings to vertices).

   if m < 100 then m := m*2; else m := m*1.25; end if;
   RS_Bearings.extend(m);
   B_to_V.extend(m);
-- For the present we report all vertex numbers starting from 1, NOT THE start vertex
--   start_vertex := TRUNC((In_Start+1)/2);
   last_vnumber := 1;
   ds := 0.0;
   bearing_avg :=0.0;
   bcount :=0.0;
   
   For ii in 1..pos-1 Loop   -- go through the edge's bearings at each vertex
     bcount := bcount + 1;
     bearing_avg := bearing_avg + Bearings(ii) *Distances(ii+1);  -- make a weighted sum
     ds := ds + Distances(ii+1);
     delta := ds*recip_measure;
     no_to_do := TRUNC(delta);
     fraction := delta - no_to_do;
   
--   Look ahead to see if the line's trend is continuous
    
     Continuous := FALSE;
     if ABS(Bearings(ii+1) - Bearings(start_bend)) < 10. then
        Continuous := TRUE;     
     end if;

-- 50% of the time we round up and 50% of the time we round down
---     if fraction >= 0.5 then
--       if odd = 1 then
---         no_to_do := no_to_do +1;
--       end if;
--       odd := 1-odd;
---     end if;
          
       
     IF delta < 0.5 or (Continuous and delta < 1.0) THEN
         NULL;         
     ELSE
       
       If no_to_do >= (RS_bearings.count - pp) then
           if no_to_do > n2000 then
            n2000 := (TRUNC(no_to_do+n2000)/n2000)*n2000;
           end if;
  
           RS_Bearings.extend(n2000);
           B_to_V.extend(n2000);
           m := m + n2000;
       End If;
       If NOT Continuous then
         no_to_do := no_to_do + 1;
         left_over :=0.0;
       End if;

-- If the measured distance is spanned by just one bearing, we can replicate it
     
       If bcount = 1 then
--   dbms_output.put_line('ii ' || ii || ' bc ' || bcount || ' Notodo ' || no_to_do || ' B ' || Bearings(ii));
          For kk in pp+1..pp+no_to_do loop
            RS_Bearings(kk) := Bearings(ii);
            B_to_V(kk) := ii;
          End Loop;
          start_bend := ii+1;
          pp  := pp+no_to_do;
          bcount := 0;
          
-- If it took more than one bearing to span the measured distance then we
-- allocate Bearings by distance: for example

--    0       0.7        1                  2 measures
--    +----------+--------------------------+
--    B(1)       B(2)

       Elsif bcount > 1 then
--       dbms_output.put_line('III ' || ii);
--  Here we use the distances to allocate: Bearing(1) is used for the
-- first measure and Bearing(2) for the second one
-- We use the first bearing because it start the interval unless its effective
-- extent is < half a measure, else we use a weighted bearing


         amount := ds/no_to_do;
          
         last_bend := last_vnumber;
         dpos := last_vnumber;
         d := 0.0;
         lastd := 0.0;
         place := 0.0;
         bsum := 0.0;

         For kk in pp+1..pp+no_to_do loop
         
  -- Generate an LRS value to use to select from the original bearings
 
           place := place + amount; 
           While d < place loop
             dpos := dpos + 1;
             d := d + Distances(dpos);
             if d < place then
                bsum := bsum + Bearings(dpos-1)*Distances(dpos);
             else
                bsum := bsum + Bearings(dpos-1)*(Distances(dpos)+place-d);
             end if;
--             if ii < 20 then
--            dbms_output.put_line('bsum is ' || round(bsum,3) || ' B ' || round(Bearings(dpos-1),4) || ' D ' || round(distances(dpos),4));
--           end if;
           end loop;
--            if ii < 20 then
--            dbms_output.put_line('Bsum is ' || round(bsum,3) || 'amount ' || amount);
--           end if;
           bsum := bsum/amount;
           lastd := d;
-- When we have a picture like this

--    0       0.7        1                  2 measures
--    +--+--+-+--+------------+-------------+
--    B(1)       B(4)       B(5)
           
--we accumulate the bearings weighting them by their distance          
           if kk > RS_Bearings.count then
             dbms_output.put_line('RS_b' || Rs_bearings.count || ' kk ' || kk  || ' no_to ' || no_to_do);
           end if;
           RS_Bearings(kk) := bsum; --Bearings(dpos-1);
--           if ii < 20 then
--           dbms_output.put_line('set B to ' || round(bsum,3));
--           end if;
           if d-place > amount then
             bsum := Bearings(dpos-1)*amount;
           else
           bsum := Bearings(dpos-1)*(d-place);
           end if;
           if B_to_V(kk) is NULL then
              B_to_V(kk) := start_bend;       
           End if;
           End Loop;
           pp := pp+no_to_do;
           bcount :=0;
        End If;
        
        ds := left_over;
/*       
--  we are skipping bearings that may belong to the last zone or the next one
       If lastp > 1 then 
-- try and figure out which zone to give the values
        diff := ABS(Bearings(last_bend) - Bearings(ii))*0.33;
--        if last_vnumber < 70 then
--   dbms_output.put_line('AT ' ||ii || ' start_bend ' || start_bend || ' last_bend ' || last_bend || 'lastv ' ||(last_vnumber) || ' bii ' || round(bearings(ii),4) || ' blst ' || round(bearings(last_bend),4));
--  dbms_output.put_line('b ' || round(Bearings(ii),4) || '  diff ' || round(diff,3));
--        end if;
        jj := start_bend; --last_bend+1;
--        start_bend := ii; --last_vnumber+1;

        For kk in reverse jj..ii-1 Loop 
        check_it := Bearings(ii); --last_vnumber);
--        if check_it < 10. and Bearings(kk) > 350. then
--           check_it := check_it + 360.;
--        elsif check_it >350. and Bearings(kk) <10. then
--           check_it := check_it - 360.;
--        end if;
--        if last_vnumber < 70 then
--           dbms_output.put_line('KK ' || kk || ' abs ' || round(Bearings(kk)-check_it,4) || ' B(kk) ' || round(bearings(kk),4) || ' lastv ' || last_vnumber ||' btov ' || b_to_v(pp));
--        end if;
        if kk >= B_To_V(pp) and ABS(Bearings(kk)-check_it) < diff then
            start_bend := kk;

        else
           B_to_V(pp+1) := start_bend; --last_vnumber;
--                       if last_vnumber < 60 then
--            dbms_output.put_line('set btov when ' ||ii || ' to ' || start_bend || ' pp+1 ' || (pp+1) || ' btov ' || b_to_v(pp+1));
--            end if;
           exit;
        end if;
        End Loop;
      End if;
*/   
-- We now may have more or less bearings than there are slots (based upon distance)
-- to store. If there are too few data for the slots we just replicate.

-- Note how bearings start at a vertex and its corresponding distance ends at
-- the next vertex.
--
-- Bearing(1)        Distance(2) ends
--          +--------+ Bearing(2) starts going South east
-- Distance(1) =0    \
--                    \
--                     + Distance(3) ends

--       amount :=  (ii-last_vnumber+1)/no_to_do;
--       place := -amount;
-- and here we use the distances
/*
       amount := ds/no_to_do;
       last_bend := last_vnumber;
       dpos := last_vnumber +1;
       d := Distances(dpos);
       place := -amount;
       lastp := pp+1;
       For kk in pp+1..pp+no_to_do loop
-- Generate an LRS value to use to select from the original bearings
-- Here we just do regularly spaced samples
         place := place + amount; 
         jj := TRUNC(place+0.5);

--         RS_Bearings(kk) := Bearings(jj+last_vnumber);
-- and here we use the distances
         While d < place loop
           dpos := dpos + 1;
           d := d + Distances(dpos);
         end loop;
         RS_Bearings(kk) := Bearings(dpos-1);
--         if kk = lastp then
           if B_to_V(kk) is NULL then
           B_to_V(kk) := start_bend;
--                      if last_vnumber < 60 then
--            dbms_output.put_line('Set btov when ' ||ii || ' to ' || start_bend || ' pp ' || pp || ' btov ' || b_to_v(kk));
--            end if;
--           if pp < 70 then
--        dbms_output.put_line('Stored ' ||(pp+1) || ' to ' || (kk) || ' with ' ||start_bend);
--       end if;
           end if;
--         else
--           B_to_V(kk) := last_vnumber;
--           if pp < 70 then
--        dbms_output.put_line('stored ' ||(pp+2) || ' to ' || (pp+no_to_do) || ' with ' ||last_vnumber);
--       end if;
--         end if;
       End loop;
       start_bend := dpos;
       bearing_avg :=0.0;
       pp := pp + no_to_do;
--       next := next + 1;
--       Mapto_vertex(next) := pp;   --- ????? Is this right????
-- There may be some small distance left over but lets toss it
--       ds := ds - measure*no_to_do;
      
       left_over := delta - measure*no_to_do;
       if left_over < 0.0 then left_over := 0.0; end if;
       ds := left_over; -- 0.0;    -- by setting it to zero!
*/
       last_vnumber := ii;

     END IF;
   End Loop;
   next := next+1;
--   mapto_vertex(next) := pp+1;

   if m > pp then
   Rs_Bearings.trim(m-pp);
--   RS_Bearings(pp):= RS_Bearings(pp-1);
   B_To_V.trim(m-pp);
   end if;
   
   For ii in 2..pp loop
     if B_to_V(ii) < B_to_V(ii-1) then
       B_to_V(ii) := B_to_V(ii-1);
     end if;
   End Loop;
   Diffs.extend(pp);
   
-- Check  that we don't have an impossibility due to sampling - a knot!
/*
   b1 := RS_Bearings(1);
   b2 := RS_Bearings(2);
   For ii in 3..pp loop
     b0 := b1;
     b1 := b2;
     b2 := RS_Bearings(ii);
   
-- Just check for an inadvertent knot caused by sampling
     if b0 > 170. and b0 <= 180. and b1 < 10. and b2 >= 350. and b2 <360. then
        dbms_output.put_line('fixed it <<<<<<<<<<<<<<<<<<<<<<<<<<<<'||ii);
        RS_Bearings(ii-1) := b3;
     end if;
     if b0 > 170. and b0 <= 180. and b1 >= 180. and b1 < 190 and b2 > 350. and b2 <=360. then
  dbms_output.put_line('Fixed it <<<<<<<<<<<<<<<<<<<<<<<<<<<<'||ii);
        RS_Bearings(ii-1) := b3;
     end if;
          if b0 > 350. and b0 <= 360. and b1 >= 180. and b1 < 190. and b2 <10. then
  dbms_output.put_line('FIxed it <<<<<<<<<<<<<<<<<<<<<<<<<<<<'||ii);
        RS_Bearings(ii-1) := b3;
     end if;
     if b0 >= 180. and b0 < 190. and b1 < 10. and b2 >= 180. and b2 < 170. then
  dbms_output.put_line('FIXed it <<<<<<<<<<<<<<<<<<<<<<<<<<<<'||ii);
        RS_Bearings(ii-1) := b3;
     end if;
   End Loop;
*/   

--   B_To_V(pp) := B_to_V(pp-1);

 
 -- Now see if there are any spirals which need to be handled
 -- We handle a maximum change of say 90 degrees (max_change) at
 -- any vertex.
 
 -- Example spiral:
 --       180
 --      ----
 --     /     \ 120
 --270  |     | 90 |  450
 --     \         /
 --  315 \_______/   405
 --        360
 --       
 last_angle := RS_Bearings(1);
 last_quad_angle := last_angle;
 hi_check := 360.-max_change;
 
-- Since the Zone function does not know that 359 is very close to 10 degrees,
-- this is what we have to do.
--
--        add nothing       |  quadrant 1:  add 360 if last is in quadrant 4
--             last is NULL |-----------
--                          |  quadrant 4: subtract 360 if last is in quadrant 1



-- Now unwrap angle as if when we are on a spiral to remove the ambiguity
-- at zero/360 degress (due East). (atan2 measures counter_clockwise from the x-axis)

-- last := NULL;

--      for ii in 1..rs_bearings.count loop
--    dbms_output.put_line('II ' || ii|| ' B ' ||round(rs_bearings(ii),3) || ' b to v ' || b_to_v(ii)); 
--   end loop;
 /*
 bias :=0.0;
 if pp >= 1 then
 Diffs.extend(pp);
 last_quad_angle := MOD(RS_Bearings(1),360.0);
 last_angle := last_quad_angle;
/*
 last_angle := NULL;
 
 -- Now unwrap angle as if when we are on a spiral to remove the ambiguity
-- at zero/360 degress (due East). (atan2 measures counter_clockwise from the x-axis)

   For ii in 1..pp Loop 
       if (last_angle-RS_Bearings(ii)) > 180. then
             bias := bias + 360.0;
       elsif (last_angle-RS_Bearings(ii)) < -180. then
             bias := bias - 360.0;
       end if;
       
       last_angle := RS_Bearings(ii);
       Rs_bearings(ii) := RS_Bearings(ii) +bias;
       Diffs(ii) := bias;
   end loop;
  
   bias := 0.0;
   for ii in 1..pp Loop
     if RS_Bearings(ii) < bias then
       bias := RS_Bearings(ii);
     end if;
   end loop;
   dbms_output.put_line('bias ' || bias);
   if bias < 0.0 then
     if TRUNC(bias/360.)*360. > bias then
        bias := bias-360.;
     end if;
     bias := abs(TRUNC(bias/360.))*360.;
     dbms_output.put_line('Bias ' || bias);
     if bias > 0.0 then
     for ii in 1..pp Loop
       RS_Bearings(ii) := RS_Bearings(ii) + bias;
     end loop;
   end if;
 end if;
 
 IF 1=1 THEN 
 For ii in 2..pp Loop 
  last_bias := bias;
--   if ABS(RS_Bearings(ii-1)-RS_Bearings(ii)) <= 180.  
--     or ABS(last_angle-RS_Bearings(ii)) >= 170. then
--      bias := 0.0;
--   end if;]
--  if NOT TRAP and ABS(last_angle-RS_Bearings(ii)) >= 170. then
--    start_place := ii;
--    TRAP := TRUE;
--  end if;
   if ii >= 187 and ii <= 190 then
     dbms_output.put_line('last ' || last_angle || 'rs ' || rs_bearings(ii) || ' rsii-1 ' || rs_bearings(ii-1));
   end if;
 -- Handle two quadrants: quadrant 1 and quadrant 4
-- First calculate the anti-clockwise traversed angle -- does
-- it go through 0=360 degrees?

   if last_quad_angle <= 180. and RS_Bearings(ii) >= 180. then
     anti_clock := last_quad_angle +360. -RS_Bearings(ii);
   elsif last_quad_angle >= 180. and RS_Bearings(ii) <= 180. then
     anti_clock := RS_Bearings(ii) + 360. - last_quad_angle;
  else
     anti_clock := 1000.;  
  end if;
  if ii >= 187 and ii <= 190 then
     dbms_output.put_line('ac ' || anti_clock || 'lq ' || last_quad_angle );
   end if;
    if anti_clock > max_change then
       NULL;
-- if last angle was in quad 4 and we are now in quad 1
    elsif last_quad_angle >= hi_check and RS_Bearings(ii) <= max_change and anti_clock <= max_change then
       bias := bias +360.; 
-- if last angle was in quad 1 and we are now in quad 4
    elsif last_quad_angle <= max_change and RS_Bearings(ii) >= hi_check then
-- instead of adding -360 to the current value we add 360 to all of our previous values
-- until we detect an abrupt turn in direction
/*
          last_angle := RS_bearings(ii);
          if NOT TRAP and last_bias = 360. then
            TRAP := TRUE;
          else
          For jj in reverse 1..ii-1 Loop  
                if TRAP = TRUE  then
             dbms_output.put_line('add 360 ' || ii || ' back ' || jj || ' last ' || last_angle);
                exit when ABS(RS_Bearings(jj+1)-RS_Bearings(jj)) <=180.;
           
             dbms_output.put_line('Add 360 ' || ii || ' back ' || jj || ' last ' || last_angle);
                exit when RS_Bearings(jj)-last_angle >= max_change;
                end if;
             dbms_output.put_line('add 360 at ' || ii || ' to ' || jj);
             last_angle := RS_bearings(jj);
             RS_Bearings(jj) := RS_Bearings(jj)+360.;
          End Loop;
          end if;
          bias := 0.0;
          trap := TRUE;

     bias := bias - 360.;
    end if;
    last_angle := RS_bearings(ii);
    RS_Bearings(ii) := RS_Bearings(ii) + bias;
    Diffs(ii) := bias;
    last_quad_angle := MOD(last_angle,360.0);
 End loop;
  bias := 0.0;
 for ii in 1..pp Loop
   if RS_Bearings(ii) < bias then
     bias := RS_Bearings(ii);
   end if;
 end loop;
 dbms_output.put_line('bias ' || bias);
 if bias < 0.0 then
   if TRUNC(bias/360.)*360. > bias then
      bias := bias-360.;
   end if;
   bias := abs(TRUNC(bias/360.))*360.;
  dbms_output.put_line('Bias ' || bias);
 if bias > 0.0 then
 for ii in start_place..pp Loop
   RS_Bearings(ii) := RS_Bearings(ii) + bias;
 end loop;
 end if;
 end if;
 ELSE
  For ii in 2..pp Loop 
  last_bias := bias;
 --  if ABS(MOD(RS_Bearings(ii-1),360.)-RS_Bearings(ii)) <= 180.  
-- if ABS(RS_Bearings(ii-1)-RS_Bearings(ii)) <= 180. 
--     or ABS(last_angle-RS_Bearings(ii)) >= 170. then
--      bias := 0.0;
--   end if;
   if ii >= 1106 and ii < 1111 then
     dbms_output.put_line('last ' || last_angle || 'last quad ' || last_quad_angle ||'rs ' || rs_bearings(ii) || ' rsii-1 ' || rs_bearings(ii-1));
   end if;
  
-- Think of a plane diving in a movie with the altimeter winding down. It turns
-- like a clock backwards. Evertime we go from say 12000 to 11999 it has 
-- subtracted 1000 from the long hand.
-- In the same way everytime we go from NW to NE we add 360 and 
-- everytime we go from NE to NW we subtract 360.
  
-- Handle two quadrants: quadrant 1 and quadrant 4

-- if last angle was in quad 4 and we are now in quad 1
    if last_quad_angle >= hi_check and RS_Bearings(ii) <= max_change and 
       ABS(RS_Bearings(ii)-last_quad_angle) <= max_change then
       bias := bias+360.;
-- if last angle was in quad 1 and we are now in quad 4
--                           120 degrees                     >= 240
    elsif last_quad_angle <= max_change and RS_Bearings(ii) >= hi_check and 
       ABS(RS_Bearings(ii)-last_quad_angle) <= max_change then
          bias := bias - 360.; -- 0.0;

    end if;
    last_angle := RS_bearings(ii);
    RS_Bearings(ii) := RS_Bearings(ii) + bias;
    Diffs(ii) := bias;
    last_quad_angle := MOD(last_angle,360.0);
 End loop;
 bias := 0.0;
 for ii in 1..pp Loop
   if RS_Bearings(ii) < bias then
     bias := RS_Bearings(ii);
   end if;
 end loop;
 dbms_output.put_line('bias ' || bias);
 if bias < 0.0 then
   if TRUNC(bias/360.)*360. > bias then
      bias := bias-360.;
   end if;
   bias := abs(TRUNC(bias/360.))*360.;
  dbms_output.put_line('Bias ' || bias);
 if bias > 0.0 then
 for ii in 1..pp Loop
   RS_Bearings(ii) := RS_Bearings(ii) + bias;
 end loop;
 end if;
 end if;
 
 END IF;
 
 End if;
*/ 
 if pp <> RS_Bearings.count then
    RS_Bearings.trim(RS_Bearings.count-pp);
    B_to_v.trim(B_to_v.count-pp);
 end if;

 
 end if;
   if bearings.count <> 0 then
    pred_bcount := (bad/bearings.count)*100;
  else
    pred_bcount := 100.;
  end if;
-- dbms_output.put_line('rs bearing count ' || rs_bearings.count || ' pred ' || pred_bcount || ' bad ' || bad);
-- for ii in 1..rs_bearings.count loop
--    dbms_output.put_line('ii ' || ii|| ' B ' ||round(rs_bearings(ii),3)|| ' mod ' || round(mod(rs_bearings(ii),360.),4) || ' bias ' || Diffs(ii)); 
-- end loop;
--    for ii in 1..rs_bearings.count loop
--    dbms_output.put_line('II ' || ii|| ' B ' ||round(rs_bearings(ii),3) || ' b to v ' || b_to_v(ii)); 
--   end loop;

 RETURN RS_Bearings;
-- 
END NEW_BEARINGS_ALONG_ALINE;
--

FUNCTION BEARINGS_ALONG_ALINE(measure NUMBER,
                                In_start PLS_INTEGER, IN_End PLS_INTEGER,
                                XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                                Mapto_Vertex     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                B_to_V IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                pred_bcount IN OUT NOCOPY NUMBER
                                ) RETURN MDSYS.SDO_LIST_TYPE
 AS
  /*****************************************************************************
--Program Name: Bearings_along_aline
--Author: Sidey Timmins
--Creation Date: 2/21/2008
--Updated:

--Usage:
--  This program has 6 required parameters:

 --   REQUIRED Parameters:
  --      INPUT
  --      measure        - distance metric to sample the bearings along the line
  ---     In_start, In_end: Range of indexes of Xes and yes to process.
  --      XYs            - an array of XY coordinates
  --      vertex         - an array of order numbers 1..nshowing
  --                       where each edge starts in the bearings.

  --      OUTPUT
  --          Bearings   - an array to put bearings in.

--Purpose:
  -- Sample bearings along a line using a sample distance (measure). We may
  -- not generate a value when the distance along the curve is less than measure
  -- in which case the bearing is averaged from x0,y0 (current start point) to
  -- x1,y1 or x2,y2, .. until the curve distance is >= measure. Thus the bearing
  -- will be averaged in this case.

--Dependencies:  GZ_UTIL_ZONE.atan2, GZ_UTIL_ZONE.sincos
--
********************************************************************************
*/

  bearings         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  n                PLS_INTEGER := 0;
  lastn            PLS_INTEGER := 0;
  nn               PLS_INTEGER;
  n2000            PLS_INTEGER := 2000;
  jj               PLS_INTEGER;
  pos              PLS_INTEGER  := 1;
  pp               PLS_INTEGER  := 0;
  inc              PLS_INTEGER;
  next             PLS_INTEGER := 0;
  a1           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(
   0.99330562068398087,0.99330553341082550,0.99330497373465199,0.9933032652183578,
   0.9932996344181496,0.9932934620093448,0.9932845136377421,0.9932783482104429,
   0.9932657810331951,0.993252463959083,0.9932398462514362);
   a2            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(
   0.00997515540930006,0.00997977946635228,0.00998904720322573,0.0100024008612311,
   0.0100190915085627,0.0100381481123478,0.0100584521647363,0.0100699787885871,
   0.0100899846107383,0.0101079774347408,0.0101230460033092);
   b             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(
   2.60227919461295e-010,3.00739642940506e-009,8.02456497494140e-009,1.41304231339307e-008,
   1.99383172928565e-008,2.40962083085169e-008,2.56283586223214e-008,2.50221540058326e-008,
   2.20706708206713e-008,1.68099737740735e-008,1.05932343670463e-008);
   c1            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(
   0.99999999981136289,0.99999998463761131,0.99999987773583565,0.99999954254499568,
   0.99999882342768365,0.99999759542303179,0.99999581110019820,0.99999456031055145,
   0.99999207002972046,0.99998940805689707,0.99998688314480166);
   c2            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(
   0.00334723822659243,0.00334815672000158,0.00335000490016898,0.00335267920716177,
   0.00335602313364560,0.00335984120472840,0.00336390796249147,0.00336625111507980,
   0.00337022115208835,0.00337382242390574,0.00337683963728868);
   t       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(3.,10.,17.,24.,31.,38.,45.,49.,56.,63.,70.);

  y                 NUMBER;
  x1                NUMBER;
  y1                NUMBER;
  xstart            NUMBER;
  ystart            NUMBER;
  dx                NUMBER;
  dy                NUMBER;
  ds                NUMBER := 0.;
  kount             PLS_INTEGER;
  lastp             PLS_INTEGER := 1;
  vnumber           PLS_INTEGER := 1;
  last_vnumber      PLS_INTEGER;
  next_vnumber      PLS_INTEGER;
  old_vnumber       PLS_INTEGER := 0;
  last_angle        NUMBER;
  delta             NUMBER;
  tds               NUMBER :=0.;
  tdn               PLS_INTEGER := 0;
  p                 PLS_INTEGER;
  rmeasure          NUMBER := 1./measure;
  tt                NUMBER;
  --      Be very careful not to alter these constants!!
  deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
  rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
  dist_factor NUMBER := 111319.490793274;
  flip              PLS_INTEGER;
  bad               pls_integer := 0;
  angle             NUMBER;
  angle0            NUMBER;
  angle1            NUMBER;
  angle2            NUMBER;
  angle3            NUMBER;
  bpred             NUMBER;   -- the predicted bearing
  bias              NUMBER := 0.;
  last              NUMBER := NULL;
  siny              NUMBER;
  cosy              NUMBER;
  cosy2             NUMBER;
  siny2             NUMBER;
  cosdy             NUMBER;
  old_angle         NUMBER;
  dyy               NUMBER;
  deltax            NUMBER;
  deltay            NUMBER;
  x1_0              NUMBER;
  y1_0              NUMBER;
  siny0             NUMBER;
  cosy0             NUMBER;
  siny1             NUMBER;
  cosy1             NUMBER;
  sinx1_0           NUMBER;
  cosx1_0           NUMBER;
  siny1_0           NUMBER;
  cosy1_0           NUMBER;
  sums              NUMBER := 0.;
  bmin              NUMBER;
  dxx               NUMBER;
BEGIN

   pred_bcount := 1000.0;
   kount := In_End - In_Start +1;
   bearings.extend(n2000);
   b_to_v.extend(n2000);

   mapto_vertex := MDSYS.SDO_LIST_TYPE();
   mapto_vertex.extend(TRUNC(kount/2));

   Pos := In_Start;
   xstart := XYs(pos);
   ystart := Xys(pos+1);


   last_vnumber := TRUNC((pos+1)/2);
--   dbms_output.put_line('last vn ' || last_vnumber || ' pos ' || pos || ' rm ' || rmeasure);
   pos := pos + 2;
   ds := 0.;
-- Sample curve or edge every so often and extract a bearing. if we have a short
-- segment that produces no bearing, we hold onto the first coordinate until the
-- last coordinate does produce some, thereby smoothing out the curve.

   While pos <= kount LOOP

     x1 := XYs(pos);
     y1 := Xys(pos+1);

-- To get consistent bearings independent of direction we always choose the largest y
     y := ABS(ystart);
     if ABS(y1) >= y then
        flip := 0;
     else
        flip := 1;
        x1 := xstart;
        y1 := ystart;
        y := ABS(ystart);
        xstart := XYs(pos);
        ystart := Xys(pos+1);
     end if;
-- Here we setup the change in x and y from the most recent vertex
     dx := x1 - xstart;
     dy := y1 - ystart;


-- Calculate the sine and cosine of the latitude and the midpoint
     siny0 := GZ_UTIL_ZONE.sincos(y*deg2rad,cosy0);
     dyy := dy*0.5*deg2rad;           -- calculate sin((y0+y1)*0.5)
     cosdy := 1. - dyy*dyy*0.5;       -- using sin(a+eps) = sin(a)cos(eps) + cos(a)sin(eps)
     siny2 := siny0*cosdy + cosy0*dyy; -- small angle approximation for formula above
     if y >= 45.5 then
       p := TRUNC((y-45.5)/7.) + 8;
       if p > 11 then               -- at the moment the constants stop at 73 degrees
          p := 11;
       end if;
     elsif y >=6.5 then
       p := TRUNC((y-6.5)/7.) + 2;
     end if;

     cosy := cosy0* (c1(p)+ siny0*siny0*c2(p));
     tt := (y1-t(p));
     dy := dy*(a1(p)+ a2(p)*siny2*siny2 + tt*tt*b(p));
     if abs(dy) > 0.0001 then
        cosy2 := cosy - siny0 * dy*deg2rad; -- small angle approximation for cos(y2)
        ds := ds + sqrt(dx*dx*cosy*cosy2 + dy*dy) * dist_factor;
        dxx := dx*sqrt(cosy*cosy2);
     else
-- Calculate along curve distance
        if abs(dx) > 0.000006 then
          cosy2 := cosy*cosy;
        else
          cosy2 := cosy0*cosy0;
        end if;
--        cosy2 := cosyy;
        ds := ds + sqrt(dy*dy + dx*dx*cosy2) * dist_factor;
        dxx := dx*sqrt(cosy2);
     end if;
     delta := ds*rmeasure;

     n := trunc(delta);
     if (delta -n) >= 0.5 then   -- was 0.5
        n := n+1;
     end if;
--     tdn := tdn + n;

--if last_vnumber < 10 then
--    dbms_output.put_line('N ' || n || ' delta ' || round(delta,10) || 'at end vtx ' || TRUNC(pos/2) || ' last vn ' || last_vnumber); -- Tds ' || ROUND(tds*rmeasure,4) ||' tdn ' || tdn || ' at ' || pos);
--end if;
--dbms_output.put_line('N ' || n || ' at pos ' || (TRUNC(pos+1)/2) || ' ds ' || round(ds,4) || ' delta ' || ROUND(delta,4));


 -- Here we setup the change in x and y from the last vertex that had
 -- sufficent accumulated length to measure a bearing
--       if lastn = 0 then
--         dx := x1 - xstart;
--         dy := y1 - ystart;
--         dbms_output.put_line('xstart ' || xstart || ' ys ' || ystart);
--       end if;
-- Use local bearings (from node position to next coordinate)

-- Data is probably on the sphere and so angles will be inaccurate when points
-- are more than 10 km apart.

     IF ((dx <> 0.) or (dy <> 0.)) THEN

-- Atan2 is very slow (remember 38 digits of precision) so use
-- a very accurate approximation and convert to degrees.

 --     siny0 := GZ_UTIL_ZONE.sincos(y0,cosy0);
 --      siny1 := GZ_UTIL_ZONE.sincos(y1,cosy1);
       y1_0 := (y1-ystart)* deg2rad;
       x1_0 := (x1-xstart)* deg2rad;
       siny1 := siny0*(1.0-y1_0*y1_0*0.5) +cosy0*y1_0; -- small angle approx
       cosy1 := cosy0 - siny0*y1_0;
--       sinx1_0 := GZ_UTIL_ZONE.sincos(x1_0,cosx1_0);
       sinx1_0 := x1_0;                  -- small angle approximations for sine
       cosx1_0 := 1.0 - x1_0*x1_0*0.5;    -- and cosine

       deltax := cosy1 * sinx1_0;
       deltay := (cosy0 * siny1 - siny0 * cosy1 * cosx1_0);
       if flip = 1 then
         deltay := -deltay;
         deltax := -deltax;
--         dy := -dy;
--         dxx := -dxx;
       end if;
--       old_angle := rad2deg * GZ_UTIL_ZONE.fast_atan2(dy,dxx);
       angle := rad2deg * GZ_UTIL_ZONE.fast_atan2(deltay,deltax);
--       if abs(angle-old_angle) > 0.5 then
--       dbms_output.put_line('dx ' || round(dxx,9) || ' dy ' || round(dy,9));
--       dbms_output.put_line('Dx ' || round(deltax,9) || ' Dy ' || round(deltay,9));
--         dbms_output.put_line('old sinx1_0 ' || round(sinx1_0,8) || ' correct ' || round(sin(x1_0),8));
--         dbms_output.put_line('old scosx1_0 ' || round(cosx1_0,8) || ' correct ' || round(cos(x1_0),8));
--         dbms_output.put_line('old siny1 ' || round(siny1,8) || ' correct ' || round(sin(y1*deg2rad),8));
--         dbms_output.put_line('old cosy1 ' || round(cosy1,8) || ' correct ' || round(cos(y1*deg2rad),8));

--       dbms_output.put_line('old ' || round(old_angle,4) || ' new ' || round(angle,4) || ' diff ' || round(angle-old_angle,6));
--  end if;
       IF (angle < 0.) THEN
          angle := angle + 360.0;
       END IF;

-- Now unwrap angle as if when we are on a spiral to remove the ambiguity
-- at zero/360 degress (due East). (atan2 measures counter_clockwise from the x-axis)


       if (last-angle) > 170. then  -- 180??
             bias := bias + 360.0;
       elsif (last-angle) < -170. then   -- -180?
             bias := bias - 360.0;
       end if;


-- Set a bias depending upon what the current angle and last are (see below)
--
--        add nothing       |  quadrant 1:  add 360 if last is in quadrant 4
--             last is NULL |-----------
--                          |  quadrant 4: subtract 360 if last is in quadrant 1

--       If last is not NULL then
--         if angle > 270. and last <= 90. then
--            bias := -360.;
--         elsif angle < 90. and last >= 270. then
--            bias := 360.;
--         elsif last >= 270. and (last-angle) > 180. then
--            bias := 360.;
--         else
--            bias := 0.;
--         End if;
--         if pos > 1000 then
--         dbms_output.put_line('AAngle ' || ROUND(angle,3) || ' bias ' || bias || ' last ' || ROUND(last,2));
--         End if;
--       Else
--          if (angle > 270 and bias = 360.) or (angle < 90. and bias = -360.) then
--             bias := 0.;
--           End If;
--           if pos > 1000 then
--       dbms_output.put_line('Angle ' || ROUND(angle,3) || ' bias ' || bias || ' last ' || ROUND(last,2));
--           end If;
--       End If;
-- we have 3 regions
--
--                          |  quadrant 1:  last is < 90
--             last is NULL |-----------
--                          |  quadrant 4: last > 270


--         if bias = 0. and (angle <= 90. or angle >= 270.) then
            last := angle;
--         else
--            last := NULL;
--        End if;

-- Unwrap. To understand consider the angles, 340,350,0,5. They all are very close
-- to North and so we wish to consider them as: 340, 350,360,365. The zone
-- function does not know that 340,350 and near 0 and 5 so we must resolve this
-- ambiguity.
         angle := angle + bias;

--         dbms_output.put_line('angle ' || ROUND(angle,3) || ' bias ' || bias || ' last ' || ROUND(last,3) || ' p ' ||p);

    ELSE
-- angle is undefined
      angle := -999.;
    END IF;


--    dss := ds;

--    if n < 4 then
--       nn := trunc(delta+0.5);
--       if n > 0 then
--       ds := ds - measure * n;
--       if nn > n then
--         ds := 0.;
--         n := nn;
--       end if;
--      end if;
-- If we round up, we can overshoot corners (Aug 25/2008)
---    If delta - n > 0.5 then
---       n := n + 1;
 ---   End If;
--    Else
     IF n > 0 THEN
     ds := ds - measure * n;
     ds := 0.;
       If n > (bearings.count - pp) then
         if n > n2000 then
           n2000 := n + n;
         End if;
         bearings.extend(n2000);
         b_to_v.extend(n2000);
       End If;

-- Test if this is a river and we cna predict the next bearing
       if angle0 is NULL then
          angle0 := angle;
          angle1 := angle;
          angle2 := angle;
          angle3 := angle;
       end if;

--       bpred :=  angle2 + 0.5*(angle1*4.-angle2 -3.*angle0);
        bpred := (angle1*4. + angle3*4. - angle - angle0)/6.;
--        dbms_output.put_line('  pos ' ||pos || ' bear ' || ROUND(angle2,4) || ' bpred ' || round(bpred,4) || ' last  ' || round(angle1,4));
       if abs(angle2-bpred) < 25. then
         pred_bcount := pred_bcount + 1.;
      else
         bad := bad + 1;
--         dbms_output.put_line('  POS ' ||pos || ' bear ' || ROUND(angle2,4) || ' bpred ' || round(bpred,4) || ' last  ' || round(angle2,4) || ' before ' || round(angle1,4));
       end if;
-- Finally save the bearings

      for i in pp+1..pp+n loop
         bearings(i) := angle;
--         dbms_output.put_line('  p ' ||i || ' bear ' || ROUND(angle,4) || ' pos ' || pos || ' last v ' || last_vnumber || ' next ' || next_vnumber);
           b_to_v(i) := last_vnumber;

      End Loop;


      last_vnumber := TRUNC((pos+1)/2);
      old_vnumber := last_vnumber;
      last_angle := angle;
      lastp := pp+1;
      pp := pp + n;
-- Reset xstart since this vertex had sufficent distance from the previous
-- xstart,ystart to generate bearings
      if flip = 0 then
      xstart := x1;
      ystart := y1;
      end if;
    ELSE
--      if delta >= 0.5 then

      if flip = 0 then
      xstart := x1;
      ystart := y1;
      end if;
--      dbms_output.put_line(' angle ' || round(angle,4) || ' last  ' || round(last_angle,4));
      if ABS(angle - last_angle) < 5.   then
          last_vnumber := TRUNC((pos+1)/2);
      end if;
    END IF;
    angle0 := angle1;
    angle1 := angle2;
    angle2 := angle3;
    angle3 := angle;

-- Remember first vertex position with these bearings
-- remember the vertex position to be able to map bearing position to the
-- nearest vertex.
    next := next + 1;
 --   if (p +1-n) <= 0 then
--      mapto_vertex(next) := 1;
--    else
      mapto_vertex(next) := pp; --lastp; --p+1-n;
--      if mapto_vertex(next) > p then
--        mapto_vertex(next) := p;
--      end if;
--    end if;

--    if p < 30 then

--    end if;
--    dbms_output.put_line('pos ' || pos || ' next ' || next || ' mapto_vertex ' || mapto_vertex(next));

--    t := n/delta;
--    t1 := 1. - t;
--    x0 := x0 * t1 + x1*t;
--    y0 := y0 * t1 + y1*t;


    lastn := n;
--    if lastn <> 0 then   --- does not make sense !!
--    last_vnumber := TRUNC((pos+1)/2);
--    end if;
    pos := pos + 2;
  END LOOP;
-- dbms_output.put_line('p is ' || p || ' bcount ' ||bearings.count  || ' ' ||b_to_v.count);

  mapto_vertex(mapto_vertex.count) := lastp;

  if pp < bearings.count then
    bearings.trim(bearings.count-pp);
    b_to_v.trim(b_to_v.count - pp);
  End If;

-- Make sure bearings are all positive!

   if bearings.count > 0 then
   bmin := bearings(1);
    For ii in 1..bearings.count loop
      if bearings(ii) < bmin then
         bmin := bearings(ii);
      end if;
    end Loop;
--    For ii in 1..bearings.count loop
--     bearings(ii) := bearings(ii) -bmin;
--    End loop;

    end if;
  /*
  last_vnumber := b_to_v(1);
  next_vnumber := b_to_v(1);
  last := 1;
  For ii in 2..b_to_v.count Loop
    if b_to_v(ii) > last_vnumber+1 then
      next_vnumber := b_to_v(ii)-1;
      if ii-1 > last then
      For jj in last..ii-1 loop
           b_to_v(jj) := last_vnumber + TRUNC((jj-last)*1./(ii-1-last)*(next_vnumber-last_vnumber));
      End Loop;
      end if;
      last := ii;
      last_vnumber := b_to_v(ii);
      next_vnumber := b_to_v(ii);
    elsif b_to_v(ii) <> next_vnumber then
      last := ii;
      last_vnumber := b_to_v(ii);
      next_vnumber := b_to_v(ii);
    end if;
  End Loop;
  */
  if bearings.count <> 0 then
    pred_bcount := (bad)/(bearings.count)*100;
  else
    pred_bcount := 100.;
  end if;
--  dbms_output.put_line('bad %' || ROUND(bad/(bearings.count)*100.,4) || ' pred ' || pred_bcount);
--dbms_output.put_line(' bear ' || bearings.count || ' bad ' || bad);
--  for ii in 1..bearings.count loop
--     dbms_output.put_line('II ' || ii || ' B ' || round(bearings(ii),2));
--  end loop;
  RETURN bearings;


END Bearings_along_aline;
--
FUNCTION
          BINARY_SEARCH(
                           PInFind NUMBER,
                           PInArray IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                           InN PLS_INTEGER, Frequency IN OUT NOCOPY PLS_INTEGER)
 RETURN PLS_INTEGER  Deterministic IS

/*
**************************************************************************************
--Program Name: Binary_Search
--Author: Sidey Timmins
--Creation Date: 10/24/2006
--Updated: 11/02/2006

--Usage:
  -- This PL/SQL function has 3 parameters:
  --                 PinFind: the value (number) to find
  --                 PinArray: the array (Varray of NUMBER) to search.
  --                 InN: the length of the input array to be searched
  --
--Purpose: This function searches a sorted array (ascending) and returns
--         the position (index) of the first element with the value PinFind.
--         If PinFind is found, the returned value is always between 1 and InN.
--
--         When not found, this function returns 0 !
--
-- Reference: http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary
--            http://www.fredosaurus.com/notes-cpp/algorithms/searching/binarysearch.html
--
--Dependencies: None
--
--Updated: 10/06/08 to handle Oracle bug
--         11/02/06 to back up to the first of any duplicate occurrences
***************************************************************************************
*/

    Hi             INTEGER   := InN;
    Low            INTEGER   := 1;
    Mid            INTEGER;
    current        INTEGER;

    Found          BOOLEAN := FALSE;

BEGIN

       WHILE (low <= Hi) and not Found LOOP
           Mid := low + (hi-low)/2;

           IF (pInFind > pInArray(Mid)) THEN
               low := Mid+1;

           ELSIF (PinFind < pInArray(Mid)) THEN
               Hi := Mid-1;

           ELSE
               Found := TRUE;

           END IF;
        END LOOP;

-- Do not replace this line with "IF found = True..
        IF pInFind = pInArray(Mid) THEN
-- backup to get the 1st occurrence
            LOOP
                EXIT WHEN (Mid = 1 or pInFind <> PInArray(Mid-1));
                Mid := Mid -1;
            END LOOP;

            current := Mid-1;
            frequency := 0;
            WHILE (current <InN) LOOP
                 EXIT WHEN (pInFind <> PInArray(current+1));
                 current := current + 1;
                 frequency := frequency + 1;
            END LOOP;
            Return Mid;
        END IF;

        Return 0;

END;
--
FUNCTION BUILD_APOLYGON(
                geometry1 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                geometry2 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                tolerance NUMBER,
                cos_angle IN OUT NOCOPY NUMBER,
                sin_angle IN OUT NOCOPY NUMBER,
                xmid IN OUT NOCOPY NUMBER,
                ymid IN OUT NOCOPY NUMBER)
RETURN MDSYS.SDO_GEOMETRY AS
  /*****************************************************************************
--Program Name: build_apolygon
--Author: Sidey Timmins
--Creation Date: 3/14/2008
--Updated:

--Usage:
--  This program has 3 required parameters:

 --   REQUIRED Parameters:
  --      INPUT

  --      geometry1            - an SDOGEOMETRY describing an edge.
  --      geometry2            - an SDOGEOMETRY describing an edge.
  --      tolerance

  --      OUTPUT
  --         Returns an SDOGEOMETRY with the intersection points

--Purpose:
  --

--Dependencies:  GZ_UTIL_ZONE.intersect_2edges, GZ_UTIL_ZONE.find_segment,
--               GZ_UTIL_ZONE.orient2d, GZ_UTIL_ZONE.reverse_ordinates,
--               GZ_UTIL_ZONE.shellsort2
********************************************************************************
*/
--  Build a polygon from 2 lines that intersect

  SegXys           MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  Out_XYArray      MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  XYArray          MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  XYArray2         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  XYArrayi         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  XYArrayii        MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  XYArrayorder     MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  Info_Array       MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY();
  Info_Array_out       MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY();
  geometry_out                MDSYS.SDO_GEOMETRY;
  geometry_intersect          MDSYS.SDO_GEOMETRY;
  geometry_intersect2          MDSYS.SDO_GEOMETRY;
  geometry          MDSYS.SDO_GEOMETRY;
  px1               NUMBER;
  py1               NUMBER;
  px2               NUMBER;
  py2               NUMBER;
  left              NUMBER;
  right             NUMBER;
  segment           NUMBER;
  seg_nos           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  order_array       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

  Xes               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Yes               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  layer_tolerance   NUMBER:= 1.E-16;

  ii                PLS_INTEGER :=1;
  jj                PLS_INTEGER;
  n                 PLS_INTEGER;
  next              PLS_INTEGER := 0;
  kount             PLS_INTEGER;
  inext             PLS_INTEGER := 0;
  icount            PLS_INTEGER := 0;
  start_            PLS_INTEGER;
  last              PLS_INTEGER;
  mid               PLS_INTEGER;
  kk                PLS_INTEGER;
  vertex            NUMBER;
  next_vertex       NUMBER;
  gtype             NUMBER;
  seg               NUMBER :=0;
  result            VARCHAR2(100);
BEGIN

  Info_Array.extend(3);
  Info_Array(1) := 1;
  Info_Array(2) := 2;
  Info_Array(3) := 1;
  SegXys.extend(4);

-- First find the intersections
--   EXECUTE IMMEDIATE  'SELECT SDO_GEOM.SDO_INTERSECTION(:1,:2,' ||layer_tolerance||') from dual'
--          into geometry_intersect using geometry1,geometry2;
   XYArray := geometry1.SDO_ORDINATES;
-- Now find which line segment in the first geometry has each intersection
/*
   XYArrayii := geometry_intersect.SDO_ORDINATES;
--   for kk in 1..XYArray.count/2 Loop
--      dbms_output.put_line('kk ' || kk || ' X ' || XYARRay(kk*2-1) || ' Y ' || xyarray(kk*2) );
--   end loop;
       for kk in 1..XYArrayii.count/2 Loop
       for ii in seg+1..TRUNC(XYArray.count/2)-1 loop
         SegXys(1) := Xyarray(ii*2-1);
         SegXys(2) := Xyarray(ii*2);
         SegXys(3) := Xyarray(ii*2+1);
         SegXys(4) := Xyarray(ii*2+2);
         geometry := MDSYS.SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),SegXYs);
       EXECUTE IMMEDIATE  'SELECT SDO_GEOM.SDO_INTERSECTION(:1,:2,' ||layer_tolerance||') from dual'
          into geometry_intersect2 using geometry,geometry2;
          If geometry_intersect2 is not NULL then
             seg := ii;
--             dbms_output.put_line('segxys ' || segxys(1) || ' y' || segxys(2) || ' x ' || segxys(3) || ' y ' || segxys(4));
             exit;
          end if;
        end loop;
      dbms_output.put_line('ORACLE intersect ' || XYARrayii(kk*2-1) || ' ' || XYARrayii(kk*2) || ' at seg ' || seg);
   End Loop;
*/
   XYArray2 := geometry2.SDO_ORDINATES;

--   dbms_output.put_line('count1 ' || xyarray.count || ' count2 ' || xyarray2.count);
   XYArrayi :=  GZ_UTIL_ZONE.NEW_INTERSECT_2EDGES(XYArray,XYArray2,cos_angle,sin_angle,xmid,ymid);
--   If XYArrayi.count <> XYArrayii.count  then
--   dbms_output.put_line('ii count is ' || XYArrayi.count); -- || ' oracle ' || XYarrayii.count);
--   End If;
--          for kk in 1..XYArrayi.count/2 Loop
--      dbms_output.put_line('SIDEY intersect ' || ROUND(XYARrayi(kk*2-1),8) || ' ' || ROUND(XYARrayi(kk*2),8));
--    End Loop;
--    For k in 1..XYArrayii.count Loop
--      dbms_output.put_line('XY ' || XYarrayii(k) || ' or ' || XYarrayi(k));
--    End Loop;
--    if XYArrayi.count = 2 then
 --      For k in 1..XYArrayi.count Loop
--      dbms_output.put_line('xyi ' || XYarrayi(k) );
--    End Loop;
--           For k in 1..XYArray.count Loop
--      dbms_output.put_line('xy ' || XYarray(k) );
--    End Loop;
--           For k in 1..XYArray2.count Loop
--      dbms_output.put_line('XY ' || XYarray2(k) );
--    End Loop;
--    end if;

   n := XYArrayi.count;
--    dbms_output.put_line('# of intersections ' || n);

   seg_nos.extend(n);
   order_array.extend(n);

   kount := XYarray.count/2;
   XYArrayorder.extend(kount*2);

   Xes.extend(kount);
   Yes.extend(kount);
--   dbms_output.put_line('kount is ' || kount || ' xyi count ' || XYArrayi.count);
   For k in 1..kount Loop
     Xes(k) := XYArray(k*2-1);
     Yes(k) := XYArray(k*2);
   End loop;

-- Test each intersection point and find its intersecting segment
   While ii < n LOOP
      px1 := XYArrayi(ii);
      ii := ii +1;
      py1 := XYArrayi(ii);
      ii := ii +1;
      px2 := px1;
      py2 := py1;
--      dbms_output.put_line('px1 ' || px1 || ' py1 ' || py1);
--      dbms_output.put_line('Xes(1) ' || Xes(1) || ' yes ' || yes(1));
      segment := GZ_UTIL_ZONE.FIND_SEGMENT(px1,py1,px2,py2,Xes,Yes,.003,left,right,TRUE);
      next := next +1;
      seg_nos(next) := abs(segment);
--      dbms_output.put_line('segment ' || segment || ' next ' || next);
--      dbms_output.put_line('Xes(segment) ' || Xes(segment) || ' yes ' || yes(segment));
--      dbms_output.put_line('Xes(seg+1) ' || Xes(segment+1) || ' yes ' || yes(segment+1));
      order_array(next) := next;
   End Loop;
-- Sort the segements
   GZ_UTIL_ZONE.shellsort2(seg_nos,order_array,1,next);
   icount := next;
-- get the
   For ii in 1..next Loop
     jj := Order_array(ii);
     XyArrayorder(ii*2-1) := XYArrayi(jj*2-1);
     XyArrayorder(ii*2) := XYArrayi(jj*2);
--     dbms_output.put_line('XYS :' || XyArrayorder(ii*2-1)|| ' ' ||XyArrayorder(ii*2));
   End Loop;
--  For ii in 1..next Loop
--   dbms_output.put_line('OR XYS :' || XyArrayi(ii*2-1)|| ' ' ||XyArrayi(ii*2));
--  End Loop;
   Out_XYArray.extend(kount*2 + 8*n);
   Info_Array_out.extend(3*n+3);

   inext := 0;
   last := 0;
   next := 1;
   ii := 1;
   While ii < (n-2) LOOP
-- each pair of intersection points can make a line and thus complete a polygon
      px1 := XYArrayorder(ii);
      ii := ii +1;
      py1 := XYArrayorder(ii);
      ii := ii +1;
--        dbms_output.put_line('Px1 ' || px1 || ' Py1 ' || py1);
      px2 := XYArrayorder(ii);
      py2 := XYArrayorder(ii+1);
--      dbms_output.put_line('Px2 ' || px2 || ' Py2 ' || py2);
      inext := inext + 1;
      Info_Array_out(inext) := last+1;
      Info_Array_out(inext+1) := 1003;
      Info_Array_out(inext+2) := 1;
      last := last +1;
      start_ := last;
      Out_XYArray(last) := px1;
      last := last+1;
      Out_XYArray(last) := py1;
      vertex := seg_nos(next) +1;
      next_vertex := seg_nos(next+1);
      For k in vertex..next_vertex  Loop
         last := last +1;
         Out_XYArray(last) := Xes(k);
         last := last+1;
         Out_XYArray(last) := Yes(k);
      End Loop;
      next := next+1;
      if px2 <> Out_XYArray(last-1) and py2 <> Out_XYArray(last) then
        last := last +1;
        Out_XYArray(last) := px2;
        last := last+1;
        Out_XYArray(last) := py2;
      End if;
      last := last +1;
      Out_XYArray(last) := px1;
      last := last+1;
      Out_XYArray(last) := py1;
      inext := inext+2;
-- start is odd, last is even, half is even
      mid := TRUNC((start_ + last-1)/2) -1;
--      if orient2D (px1, py1, Xes(vertex), Yes(vertex),px2, py2) < 0.0 then
--      dbms_output.put_line('NO reversed in zone');
--        GZ_UTIL_ZONE.reverse_ordinates(Out_XyArray, start_,last);
--      End If;

   End Loop;

--   dbms_output.put_line('inext ' || inext || ' last ' || last || ' out ' || Out_XYArray.count);
   If Info_Array_out.count > inext then
      Info_Array_out.trim(Info_Array_out.count-inext);
   End If;
   if Out_XYArray.count > last then
       Out_XYArray.trim(Out_XYArray.count-last);
   End if;
--   kk := 1;
--   while kk < Info_Array_out.count loop
--      dbms_output.put_line('Info_Array ' || Info_Array_out(kk) || ' ' || Info_Array_out(kk+1) || ' ' || Info_Array_out(kk+2));
--      kk := kk + 3;
--   end loop;

   If Info_Array_out.count > 3 then
     gtype := 2007;
   Else
     gtype := 2003;
   End If;

   geometry_out := MDSYS.SDO_GEOMETRY(gtype,8265,NULL,Info_Array_out,Out_XYArray);

--    result := SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(geometry_out,0.001);
--    If result = 'TRUE' then
--       NULL;
--       dbms_output.put_line('Geometry is VALIDATED' || Out_XYArray.count);
--    else
-- Sometimes we get an oracle 13349 (polygon boundary crosses itself)
-- because Oracle projects
--       dbms_output.put_line('POLYGON ' || ii || ' ' || Out_XYArray.count);
--       dbms_output.put_line(result);
--    End If;


  RETURN geometry_out;

END Build_apolygon;
--
FUNCTION FIND_SEGMENT (
      px1             IN NUMBER,
      py1             IN NUMBER,
      px2             IN NUMBER,
      py2             IN NUMBER,
      xes             IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
      yes             IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
      layer_tolerance   IN NUMBER DEFAULT 0.2,
      left            IN OUT NOCOPY NUMBER,
      right           IN OUT NOCOPY NUMBER,
      allow_points    BOOLEAN default FALSE
   ) RETURN NUMBER
   AS
   /*
   *****************************************************************************
   --Program Name: find_segment
   --Author: Sidey Timmins
   --Creation Date: 1/26/2007
   --Update: 6/19/2007 Always returns a segment number even when one end of the
   --         daughter is off the line by a significant amount (like 0.05 m) but
   --         the other end IS ON THE MOTHER VERTEX.

   --Usage:
   -- Call this program from inside another PL/SQL program.  This program
   -- has 9 required parameters:
   --
   --   REQUIRED Parameters:
   --      INPUT
   --      px1,py1      - test line start point (x,y) to be checked
   --      px2,py2      - test line end point (x,y) to be checked
   --      xes          - Array of X coordinates for the "Mother" edge
   --      yes          - Array of Y coordinates fro the "Mother" edge
   --      layer_tolerance - Oracle tolerance
   --      left
   --      right
   --      allow points - Boolean: False ignores degenerate lines (start = endpoint)
   -- Purpose:
   -- Return:             n:  segment n goes in same direction as "Mother" edge
   --                    -n:  segment n goes in opposite direction to its
   --                          corresponding "Mother" edge.
                         0:  not on any "Mother" segements

   -- Algorithm:  Original devised by author to avoid the high cost of linear
   --             search. Divides the Mother edge into about 40 segments
   --             going from x1,y1 to x40,y40 etc and then checks the overlap
   --             of the test segment with the window described by (x1,y1) to
   --             (x40,y40). After locating the fortieth that the test
   --             segment overlaps (or if the checks at this resolution fail)
   --             the Mother edge is searched vertex by vertex.

   -- Dependencies: GZ_UTIL_ZONE.point_on_line
   --
   *****************************************************************************
   */

     x1         NUMBER := px1;
     y1         NUMBER := py1;
     x2         NUMBER := px2;
     y2         NUMBER := py2;

     r1         NUMBER;
     r2         NUMBER;
     s1         NUMBER;
     s2         NUMBER;
     checkit    PLS_INTEGER;
     checkit2   PLS_INTEGER;

     i          PLS_INTEGER;
     ii         PLS_INTEGER;
     loupe      PLS_INTEGER;
     inext      PLS_INTEGER;
     try        PLS_INTEGER :=2;

     j          PLS_INTEGER;
     k          PLS_INTEGER;
     n          PLS_INTEGER;
     m          PLS_INTEGER;
     npoints    PLS_INTEGER;
     inc        PLS_INTEGER;
     modulus    PLS_INTEGER;
     start_point PLS_INTEGER :=1;
     loupes     PLS_INTEGER  :=2;

   BEGIN

     If px1 = px2 and py1 = py2 and allow_points = FALSE then
        RETURN 0;
     End if;
     npoints := xes.count;
     modulus := sqrt(npoints);
     inc := (npoints/modulus);

     If inc <= 1 then
       inc := 1;
       Loupes :=  1;
     end if;

   -- For the first loop we divide and conquer by approximating the Mother as
   -- 40 segments going from x1,y1 to x40,y40, x80,y80, etc
   -- Then we use pairs of these to define "Windows"
   -- The inner while check to see whether the test segment overlaps a window:
   --
   --                    ---------------------- Xinext,Yinext
   --                    |     /px2,py2        |
   --                    |    /px1,py1         |
   --               x1,y1-----------------------

     FOR Loupe in 1..Loupes LOOP

        If start_point <= 0 then
           start_point := 1;
        End if;
        i := start_point;

        While i<=(xes.count-1+start_point) LOOP
        -- Check bounding box of the "Mother" segment in question to see
        -- if the test segment overlaps

          ii := Mod(i,npoints)+inc;
          if ii >= npoints then
            ii := 1;
          end if;
          inext := ii + inc;
          if inext > npoints then
             inext := npoints;
          end if;
         -- First have to figure out which "Window" coordinate is greater

          if xes(inext) > xes(ii) then
            j := ii;
            k := inext;
          else
            j := inext;
            k := ii;
          end if;
         if yes(inext) > yes(ii) then
            n := ii;
            m := inext;
         else
            n := inext;
            m := ii;
         end if;

          -- This is the correct way to formulate this test (see any graphics book
         -- for checking whether a line overlaps a window)

         If (x1  < xes(j) and x2 < xes(j)) or
           (x1  > xes(k) and x2  > xes(k)) or
           (y1  < yes(n) and y2  < yes(n)) or
           (y1  > yes(m) and y2 > yes(m)) then
           NULL;                               -- Its outside the window
         Else
            if inc <> 1 then
              try := ii;
              goto LABEL;
            end if;

        -- There is a possibility the test segment overlaps this Mother segemnt

       -- Check one end of the test line
           checkit := GZ_UTIL_ZONE.point_on_line(x1,y1,xes(ii),yes(ii),
                                   xes(inext),yes(inext),r1,s1,layer_tolerance);

--     Now check x2 and y2 (we know its on this line unless x1,y1
--     are at a vertex) in which case x2,y2 may be on the next segment


          checkit2 := GZ_UTIL_ZONE.point_on_line(x2,y2,xes(ii),yes(ii),
                                   xes(inext),yes(inext),r2,s2,layer_tolerance);
-- dbms_output.put_line('checkit ' || checkit || ' checkit2 ' || checkit2);
              if checkit = 1 and checkit2 = 1 then
--             dbms_output.put_line('r1 ' || r1 || ' r2 ' || r2 || ' ii ' || ii || ' x1 ' || x1 || ' y1 ' || y1 || ' x2 ' || x2 || ' y2 ' || y2 || ' xes ' ||xes(ii) || ' yes ' || yes(ii) || ' ' ||xes(inext)|| ' ' || yes(inext));
-- test Segment is on this segment of the Mother
                 if r2 >= r1 then
                    left := r1;
                    right := r2;
                    RETURN ii;   -- going in the same direction
                 elsif r2 < r1 then
                    left := r2;
                    right := r1;
                    RETURN -ii;  -- going backwards
                 end if;
-- One end of the line is on the mother but out of tolerance
--              elsif x1 = xes(ii) and y1 = yes(ii) then
--                 if r2 >= 0 and r2 <= 1 and s2 < (layer_tolerance * 2) then
--                    if r2 >= r1 then
--                       left := r1;
--                       right := r2;
--                       RETURN ii;   -- going in the same direction
--                    elsif r2 < r1 then
--                       left := r2;
--                       right := r1;
--                       RETURN -ii;  -- going backwards
--                    end if;
--                 end if;
--              elsif x2 = xes(ii) and y2 = yes(ii) then
--                if r1 >= 0 and r1 <= 1 and s1 < (layer_tolerance * 2) then
--                    if r2 >= r1 then
--                       left := r1;
--                       right := r2;
--                       RETURN ii;   -- going in the same direction
--                    elsif r2 < r1 then
--                       left := r2;
--                       right := r1;
--                       RETURN -ii;  -- going backwards
--                    end if;
--                 end if;

              end if;

         End IF;

         i := i + inc;
       END LOOP;   -- end of loop over all segments
  << LABEL >>
       inc := 1;
       start_point := try-2;  -- we must backup at least one
     END LOOP;
     RETURN 0;

   END FIND_SEGMENT;
--
FUNCTION LINE_INTERSECT( X1 IN OUT NOCOPY NUMBER,
                              Y1 IN OUT NOCOPY NUMBER,
                              X2 IN OUT NOCOPY NUMBER,
                              Y2 IN OUT NOCOPY NUMBER,
                              X3 IN OUT NOCOPY NUMBER,
                              Y3 IN OUT NOCOPY NUMBER,
                              X4 IN OUT NOCOPY NUMBER,
                              Y4 IN OUT NOCOPY NUMBER,
                              Xi IN OUT NOCOPY NUMBER,
                              Yi IN OUT NOCOPY NUMBER)
            RETURN NUMBER Deterministic IS
/*
********************************************************************************
--Program Name: line_interesect
--Author: Sidey Timmins
--Creation Date: 4/24/2007

--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 10 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT

  --      X1,Y1      - start of  (x,y) of line 1 (point A)
  --      X2,Y2      - end point (x,y) of line 1 (point B)
  --      X3,Y3      - start of  (x,y) of line 2
  --      X4,Y4      - end point (x,y) of line 2
  --
  --      OUTPUT
  --      xi,yi      -- the interesection point
  --      R         - the (returned) line parameter along line 1 (A to B)
  --                     will be from 0 to 1 if the interesection point C
  --                     is on the line.
  --
  --
  --                   C
  --            A+-----.------+B
  --          zero   prout    1.0
  --
-- Purpose: Determine whether a point is on a line
  -- Return:              zero to 1.:  point is on line
  --                      -1:  no intersection
  --                     -2: parallel
  --                     -3,-4: concident
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
--
-- Dependencies: none
********************************************************************************
*/
   s    NUMBER;
   t    NUMBER;
   Det  NUMBER;
-- These tolerances handles much worse errors see above: case 2
--                              These handled the original problem: case 1
   tolerance NUMBER := 1.E-4;   -- 2.E-5
   tolerance2 NUMBER := 1.E-6;  -- 1.E-8
   p0   NUMBER := -tolerance2;
   p1   NUMBER := 1.0+tolerance2;

--   length_2 NUMBER;
   r        NUMBER;
BEGIN
-- Check parametric equations for two lines: s is on line 1
--                                           t is on line 2

   det := (X4 - X3) * (Y2 - Y1)  - (X2 - X1) * (Y4 - Y3) ;


   IF det <> 0 THEN
      s := ((X4 - X3) * (Y3 - Y1) - (Y4 - Y3) * (X3 - X1)) /det;
      t := ((X2 - X1) * (Y3 - Y1) - (Y2 - Y1) * (X3 - X1)) /det;

      If s >= 0 and s <= 1.0 and t >= 0 and t <= 1.0 then

        xi := X1 + s * (X2 - X1);
        yi := Y1 + s * (Y2 - Y1);

-- check point is on line
--        length_2 := ((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1));

--      IF length_2 <> 0 THEN
--         r := 0.5*((X1 - xi) * (Y2 - Y1) - (Y1 - yi) * (X2 - X1))  /
--             length_2;
--        dbms_output.put_line(' CHECK r is :' || r || ' for intersection ' || xi || ' ' || yi);
--      END IF;
        RETURN s;
      Else
        RETURN  -1.;
      End if;


   END IF;
   if x1 = x3 and y1 = y3 and x2 = x4 and y2 = y4 then
        RETURN -3.;
   elsif x1 = x4 and y1 = y4 and x2 = x3 and y2 = y3 then
       RETURN -4.;
   end if;
-- Lines are parallel (but not coincident)
   RETURN -2.;


END;
--
FUNCTION NEW_INTERSECT_2EDGES(XYArray  IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                            XYArray2 IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                            cos_angle IN OUT NOCOPY NUMBER,
                            sin_angle IN OUT NOCOPY NUMBER,
                            xmid IN OUT NOCOPY NUMBER,
                            ymid IN OUT NOCOPY NUMBER)
RETURN MDSYS.SDO_ORDINATE_ARRAY AS
/*******************************************************************************
--Program Name: intersect_2edges
--Author: Sidey Timmins
--Creation Date: 3/10/2008
--Updated:

--Usage:
--  This program has 2 required parameters:

 --   REQUIRED Parameters:
  --      INPUT
  --      XYArray        - an array of XY coordinates for the original edge
  --      XYArray2       - an array of XY coordinates for the straight line
  --                       connecting the endpoints of Xyarray
  --      cos_angle      - cosine of orientation angle of line connecting the edndpoints
  --      sin_angle      - sine of orientation angle
  --      xmid, ymid     - rotation point.
  --      OUTPUT
  --       Returns an XY Array of intersections

--Purpose:
  --  Calculates intersections between 2 edges.

-- Reference:  "An Efficient Algorithm for the Calculation of Line Intersections
--              and Polygonization in GIS"
--       Sidey Timmins, Unpublished thesis, 1987, COGS, Lawrencetown, NS
--
--Dependencies:  GZ_UTIL_ZONE.xyBresenham, GZ_UTIL_ZONE.Stablesort2,
--  GZ_UTIL_ZONE.line_intersect, GZ_UTIL_ZONE.binary_search
--
********************************************************************************
*/
  XYArray_out       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  intersections     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

  SQRS              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  SEGS              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();


     x1             NUMBER;
     y1             NUMBER;
     x2             NUMBER;
     y2             NUMBER;
     x3             NUMBER;
     y3             NUMBER;
     x4             NUMBER;
     y4             NUMBER;

     x_intersect    NUMBER;
     y_intersect    NUMBER;
     result         NUMBER;

     ii             PLS_INTEGER;

     inext          PLS_INTEGER;
     j              PLS_INTEGER;
     k              PLS_INTEGER;
     npoints        PLS_INTEGER;
     next           PLS_INTEGER := 0;
     i_count        PLS_INTEGER := 0;

     counter        PLS_INTEGER := 0;
     kk             PLS_INTEGER;
     n              PLS_INTEGER;
     other          NUMBER;


     the_time        timestamp;
BEGIN

  intersections.extend(200);
-- unpack First XY Array
--  the_time := current_timestamp;

  x1 := XYArray2(1);
  y1 := XYArray2(2);
  x2 := XYArray2(3);
  y2 := XYArray2(4);



   -- For the first loop we divide and conquer by approximating the Mother as
   -- 40 segments going from x1,y1 to x40,y40, x80,y80, etc
   -- Then we use pairs of these to define "Windows"
   -- The inner while check to see whether the test segment overlaps a window:
   --
   --                    ---------------------- Xinext,Yinext
   --                    |     /px2,py2        |
   --                    |    /px1,py1         |
   --               x1,y1-----------------------

     n := TRUNC(XYArray.count/2);
     other := n*2+1;
--     dbms_output.put_line('NN ' || n);
     for i in 1..n-1  LOOP

           ii := i*2 -1;
           inext := ii+2;
        -- There is a possibility the test segment overlaps this Mother segemnt
            counter  := counter + 1;
            result := GZ_UTIL_ZONE.line_intersect(x1,y1,x2,y2,XYarray(ii),XYArray(ii+1),
                                XyArray(inext),XYArray(inext+1),x_intersect,y_intersect);
--    if i =130 or result >= 0. and result <= 1. then
--             dbms_output.put_line('I ' || i || ' result' ||Round(result,9) || ' x1 ' || x1 || ' y1 ' || y1 || ' x2 ' || x2 || ' y2 ' || y2);
--              dbms_output.put_line(' x1 ' || xyarray(ii) || ' y1 ' || xyarray(ii+1) || ' x2 ' || xyarray(inext) || ' y2 ' || xyarray(inext+1));
--   dbms_output.put_line('x3 ' || XYarray(ii) || ' y3 ' || XYarray(ii+1));
--   dbms_output.put_line('x4 ' || XYarray(inext) || ' y4 ' || XYarray(inext+1));
--   dbms_output.put_line('xi ' || x_intersect || ' yi ' || y_intersect);
--   end if;
            If result >= 0. and result <= 1. then
--            dbms_output.put_line('II was ' || ii);
--               dbms_output.put_line('II' || i || ' ' || Sqrs(i) || ' ' || ii || ' other ' || other);
--   dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
--   dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2);


--   dbms_output.put_line('xi ' || x_intersect || ' yi ' || y_intersect);
               if next >= XYArray_out.count then
                 XYArray_out.extend(100);
               End If;
               next := next + 1;
               XYArray_out(next) := x_intersect;
               next := next + 1;
               XYArray_out(next) := y_intersect;
               i_count := i_count + 1;
               if i_count > intersections.count then
                 intersections.extend(100);
               End If;
               intersections(i_count) := ii*1000000. + other;
            Elsif result = -3. or result = -4. then
               next := next + 1;
               if next >= XYArray_out.count then
                 XYArray_out.extend(100);
               End If;
               XYArray_out(next) := x3;
               next := next + 1;
               XYArray_out(next) := y3;
               i_count := i_count + 1;
               if i_count > intersections.count then
                 intersections.extend(100);
               End If;
               intersections(i_count) := ii*1000000. + other;
               next := next + 1;
               XYArray_out(next) := x4;
               next := next + 1;
               XYArray_out(next) := y4;
               i_count := i_count + 1;
               if i_count > intersections.count then
                 intersections.extend(100);
               End If;
               intersections(i_count) := ii*1000000. + other;
            End If;

           exit when result = -3. or result = -4.;
         END LOOP;

--     For kk in 1..i_count Loop
--       dbms_output.put_line('intersections ' || intersections(kk));
--     End Loop;
  XYArray_out.trim(XYArray_out.count - next);

  RETURN XYArray_out;


END NEW_INTERSECT_2EDGES;
--
FUNCTION orient2D (paX NUMBER, paY NUMBER,
                                  pbX NUMBER, pbY NUMBER,
                                  pcX NUMBER, pcY NUMBER)

            RETURN NUMBER
 Deterministic IS
/*
**************************************************************************************
--Program Name: orient2D
--Author: Sidey Timmins
--Creation Date: 8/15/2006

--Usage:
  -- Call this function from inside another PL/SQL program.  There are
  -- 6 required parameters:
  --
  --   REQUIRED Parameters:
  --      paX,paY           - a point (x,y)
  --      pbX,pbY           - a 2nd point (x,y)
  --      pcX,pcY           - a 3rd point (x,y)
-- Purpose:
  -- Return a positive value if the points a, b, and c occur
  --             in counterclockwise order; a negative value if they occur
  --             in clockwise order; and zero if they are collinear.  The
  --              result is also a rough approximation of twice the signed
  --              area of the triangle defined by the three points.
  -- This procedure calculates the determinant:
--         | ax  ay   1 |
--         | bx  by   1 |    =   | ax -cx    ay -cy |
--         | cx  cy   1 |        | bx -cx    by -cy |
--   --Dependencies: None
--
--Limits:
***************************************************************************************
*/

   ccwerrboundA  CONSTANT NUMBER:= 1.76324152623343126195310480583336851684E-38;


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
--dbms_output.put_line('Aerrbound ' || errbound);
if ( (det >= errbound) or (-det >= errbound) or (det = 0.0) ) THEN
--   dbms_output.put_line('DET is : ' || det);
   RETURN det;
End If;

--  det := c_orient2d_adapt(pax, pAy, pbx, pby, pcx, pcy, detsum);
--dbms_output.put_line( 'Got to the end of orient2D and fell thru' || to_char(det));
RETURN det;

END;
--
FUNCTION POINT_ON_LINE (
      pcX   IN NUMBER,
      pcY   IN NUMBER,
      pX1   IN NUMBER,
      pY1   IN NUMBER,
      pX2   IN NUMBER,
      pY2   IN NUMBER,
      prout IN OUT NOCOPY NUMBER,
      psout IN OUT NOCOPY NUMBER,
      layer_tolerance   IN NUMBER DEFAULT 0.2  -- ignored now
   ) RETURN NUMBER DETERMINISTIC
   AS
/*
********************************************************************************
  Program Name: point_on_line
  Author: Sidey Timmins
  Creation Date: 1/17/2007
  Updated: 6/29/2007
           7/27/07 to use the Oracle "tolerance"
           1/29/09 to test against epsilon (for Geographic coordinates in this package)
  Usage:
   Call this program from inside another PL/SQL program.  This program
   has 8 required parameters:

   REQUIRED Parameters:
      INPUT
      pcX,pcY      - test point C (x,y) to be checked
      pX1,pY1      - start or end point A(x,y) of line
      pX2,pY2      - start or end point B(x,y) of line

      OUTPUT
      prout        - the line parameter along the line AB
                     will be from 0 to 1 if the test point C is on the line.

                     By calling this function twice with 2 points one can
                     determine the direction of the line described by these
                     2 points. The greater prout value is towards the
                     end node of the original line.

                   C
            A+-----.------+B
                   I (intersection point)
          zero   prout    1.0
       psout        - the line parameter along the line CI
                      will be from 0 to 1 if the test point C is on the line.
 Purpose: Determine whether a point is on a line

 Returns:                1:  point is on line
                         0:  not on line

  Dependencies: NONE
  Updates: This data for derived and parent caused a problem
   x1   NUMBER :=  70971.6838873964;         xes(1) :=71241.4748303597;
   y1   NUMBER :=  184331.932613249;         yes(1) :=184666.358957741;   <
   x2   NUMBER :=  71241.4748303597;         xes(2) :=70439.6838592369;
   y2   NUMBER :=  184666.358957743;    <    yes(2) :=183672.478212245;

 Case 1:
        we upped the tolerance from 1.E-5 to 2.E-5 which enabled x1,x2 to
        be changed by as much as 1.E-8 (20 times bigger than the difference
        shown above)
 Case 2:
       After addding 4 million meters to x and 2 million meters to y the new
       tolerances shown below enabled x1,y1 (and/or x2,y2) to be changed by 1.E-7
********************************************************************************
*/
      r    NUMBER;
      s    NUMBER;
-- These tolerances handles much worse errors see above: case 2
--                                   These handled the original problem: case 1
      epsilon      NUMBER := 1.E-4;  -- 2.E-5
      epsilon2     NUMBER := 1.E-6;  -- 1.E-8
      p0           NUMBER := -epsilon2;
      p1           NUMBER := 1.0+epsilon2;
      length_2     NUMBER;
      tolerance    NUMBER := layer_tolerance*layer_tolerance + epsilon2;

   BEGIN
--  Check parametric equation of two lines: r is on the line
--                                          s is on the perpendicular to the line
--  CP is the normal from C to the line AB (points 1 and 2).

      length_2 := ((pX2 - pX1) * (pX2 - pX1) + (pY2 - pY1) * (pY2 - pY1));
      prout := -1;
      IF length_2 <> 0 THEN
         s := 0.5*((pX1 - pcX) * (pY2 - pY1) - (pY1 - pcY) * (pX2 - pX1))  /
             length_2;

         r := ((pcX - pX1) * (pX2 - pX1) + (pcY - pY1) * (pY2 - pY1)) /
               length_2;
          prout := r;
          psout := s;
-- make a simultaneous line parameter and distance test
         if (r >= p0 and r <= p1) and abs(s) < epsilon then -- used to be < tolerance
            RETURN 1;
         else
            RETURN 0;
         end if;
      ELSE
--    length is zero which means we have a degenerate line segment - a point
--    best we can do is return zero
        NULL;
      END IF;

      RETURN 0;


END POINT_ON_LINE;
--
FUNCTION SEARCH_GREATER(   PInFind NUMBER,
                           PInArray IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                           InN PLS_INTEGER)
 RETURN PLS_INTEGER  Deterministic IS

/*
**************************************************************************************
--Program Name: Search_greater
--Author: Sidey Timmins
--Creation Date: 10/24/2006
--Updated: 11/02/2006

--Usage:
  -- This PL/SQL function has 3 parameters:
  --                 PinFind: the value (number) to find
  --                 PinArray: the array (Varray of NUMBER) to search.
  --                 InN: the length of the input array to be searched
  --
--Purpose: This function searches a sorted array (ascending) and returns
--         the position (index) of the first element with the value >=PinFind.
--         If PinFind is found, the returned value is always between 1 and InN.
--
--         When not found, this function returns 0 !
--
-- Reference: http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary
--            http://www.fredosaurus.com/notes-cpp/algorithms/searching/binarysearch.html
--
--Dependencies: None
--
--Updated: 11/02/06 to back up to the first of any duplicate occurrences
--
***************************************************************************************
*/

    Hi             PLS_INTEGER   := InN;
    Low            PLS_INTEGER   := 1;
    Mid            PLS_INTEGER;

    Found          BOOLEAN := FALSE;

BEGIN

       WHILE (low <= Hi) and not Found LOOP

           Mid := low + (hi-low)/2;
           IF (pInFind > pInArray(Mid)) THEN
                    low := Mid+1;
           ELSIF (PinFind < pInArray(Mid)) THEN
               Hi := Mid-1;
           ELSE
               Found := TRUE;

           END IF;
        END LOOP;

-- Do not replace this line with "IF found = True..
        IF pInFind <= pInArray(Mid) THEN
           LOOP
              EXIT WHEN (Mid = 1 or pInFind <> PInArray(Mid-1));
            Mid := Mid -1;
           END LOOP;

           Return Mid;
        ELSIF  pInFind > pInArray(Mid)  and Mid < InN THEN
            Return (Mid+1);
        END If;
        Return 0;

END SEARCH_GREATER;
--
FUNCTION SINCOS(InX NUMBER, COSX IN OUT NOCOPY NUMBER)
    RETURN NUMBER Deterministic IS
/*
********************************************************************************
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
********************************************************************************
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
FUNCTION FAST_SINCOS(InX NUMBER,cosx IN OUT NOCOPY NUMBER)
    RETURN NUMBER Deterministic IS
/*
********************************************************************************
--
--Program Name: fast_sincos
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
--Purpose:   -- Calculate cos(x) -
--              the cosine function as fast as possible (3 times faster
--              than the built in functions that take 1 second for 20000 sine
--              and cosine values) with about 7 digits of accuracy.
--
-- Accuracy:
--
-- Reference:
-- For cosine code
-- http://www.gansle.com/approx/approx.pdf
--            "A Guide to Approximations" by Jack G Ganssle

-- Updates: with better approximations for cosine on 11/22
--          allowed range to be the real number line

-- Dependencies: None but see c_test_sincos for rigorous testing.
********************************************************************************
*/

  X         NUMBER             := Abs(InX);

  twopi     CONSTANT NUMBER             := 6.2831853071795864769252867665590057684;
  pi        CONSTANT NUMBER             := 3.1415926535897932384626433832795028842;
  piByTwo   CONSTANT NUMBER             := 1.5707963267948966192313216916397514421;
  pi3by2    CONSTANT NUMBER             := 4.7123889803846898576939650749192543263;

  -- these coefficients are optimized to give accurate results throughout
  -- the range [-2pi to 2pi]

    c1        CONSTANT NUMBER             := 0.999999953464;
    c2        CONSTANT NUMBER             :=-0.4999999053455;
    c3        CONSTANT NUMBER             := 0.0416635846769;
    c4        CONSTANT NUMBER             :=-0.0013853704264;
    c5        CONSTANT NUMBER             := 0.000023233; --better coefficient than Hart's


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
   cosx   := c1 + x2*(c2 + x2*(c3 + x2*(c4 + c5*x2)));

    IF cosx > 1.0 THEN
      cosx := 1.0;
    END IF;

    sinx := sqrt(1.0-cosx*cosx);
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


END FAST_SINCOS;
--
FUNCTION NEW_REMOVE_CLOSE_XYS(Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,pKeep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,ptolerance NUMBER default 0.05)
 RETURN NUMBER Deterministic IS
/**
 ################################################################################
 # Program Name: Remove_DupXys
 # Author: Sidey Timmins
 # Creation Date: 7/22/2008
 # Update: 03/11/2010 To not remove vertices when there are only 2.
 #
 # Usage:
 #   This PL/SQL procedure has 2 parameters:
 #                   Geom: the Geometry to check and fix.
 #                   ptolerance: a tolerance in meters - the closest that
 #                               points will be kept.
 #   Returns TRUE only when the geometry is changed;
 #
 # Purpose: This procedure removes vertices from an edge or polygon which are too close.
 #          The resolution for our geodetic coordinates is 1 millionth of a degree.
 #          A longitude difference of 1.E-06 degrees at 64 degrees North with the
 #          same latitude is 0.047 meters and less than the 0.05 meter tolerance.
 #
 # Method: Filters vertices to determine the ones to remove.
 # Dependencies:
 #  accurate_length
 ################################################################################
*/

    XYOrd             MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    NewXYOrd          MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Info_Array        MDSYS.SDO_ELEM_INFO_ARRAY;
    Keep_it           MDSYS.SDO_LIST_TYPE;

    deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
    ii          PLS_INTEGER;
    xlast       NUMBER;
    ylast       NUMBER;
    xnew        NUMBER;
    ynew        NUMBER;
    k           PLS_INTEGER := -1;
    ycheck      NUMBER;
    xcheck      NUMBER;
    distance    NUMBER;
    rings       PLS_INTEGER;
    remaining   PLS_INTEGER := 0;
    LB          PLS_INTEGER;
    UB          PLS_INTEGER;
    j           PLS_INTEGER;
    jlast       PLS_INTEGER :=1;
    drop_it     PLS_INTEGER :=2;
    n           PLS_INTEGER;
    closed      BOOLEAN := FALSE;

    angle_threshold NUMBER := 87.;
    tolerance       NUMBER := ptolerance;

    GTYPE           NUMBER;
    SRID            NUMBER;
    theta1          NUMBER;
    theta2          NUMBER;
    len1            NUMBER;
    len2            NUMBER;
    len3            NUMBER;
    b1              NUMBER;
    b2              NUMBER;
    xbef            NUMBER := 0.;       
    ybef            NUMBER := 91.;  -- Flag to not work out included angle for non loop
    xbef2           NUMBER ;
    ybef2           NUMBER ;
    xbeh            NUMBER;
    ybeh            NUMBER;
    area1           NUMBER;
    area2           NUMBER;
    xnext           NUMBER;
    ynext           NUMBER;
    coslat          NUMBER;
    ok              NUMBER := 180.; -- ("OK" value for angles and length !!)
    included_angle  NUMBER := 90.;
    included_angle2 NUMBER := 90.;
    
    Function dlen(x1 number,y1 number,x2 number,y2 number) return number as
    
      dx  number := (x2-x1)*coslat;
      dy  number := y2-y1;
      d   number;
    begin
    
      d := sqrt(dx*dx + dy*dy);
      return d;
    end;
-- Written as a procedure so as NOT to create new geometry objects and waste memory

BEGIN

   SRID := geom.SDO_SRID;
   GTYPE := geom.SDO_GTYPE;

   If NOT( Gtype = 2002 or Gtype = 2003)  then  -- Doesn't do points or collections or 2007
      RETURN 0;
   End If;

-- Detects dups because most edges are ok. Don't remove any when their are only
-- 4 coordinates or less.

    Info_Array := geom.SDO_ELEM_INFO;
    XYORD := geom.SDo_Ordinates;
    NewXYord.extend(XYord.count);

    If XYOrd is NULL or XYORD.count <= 4 then
      RETURN 0;
    End If;

    coslat := cos(XYord(2)*deg2rad);
-- Caller can specify anchor vertices which are not to be dropped
-- Set up these anchors if not passed
    n := TRUNC(XYOrd.count/2);
    If pKeep_it is NOT NULL then
       Keep_it := pKeep_it;
       if Keep_it.count < n then
         Keep_it.extend(n);
         for ii in 1..n loop
            Keep_it(ii) := 0.;
         end loop;
         Keep_it(1) := 1.;
         Keep_it(n) := 1.;
       end if;
    End If;
    If ptolerance <= 0. then tolerance := 0.05; End if;

-- These parameters were tested from 0 to 72 degrees North with random coordinates
-- to check that this procedure got the same results as the Oracle function
-- SDO_UTIL.REMOVE_DUPLICATE_VERTICES. Note that the Oracle function may change the
-- coordinates!

    
    ylast := XYOrd(2);
    if ylast > 50. then
      xcheck   := 0.0000016 * tolerance/0.05;
      ycheck   := 0.0000005 * tolerance/0.05;
    else
      xcheck   := 0.00000075 * tolerance/0.05;
      ycheck   := 0.00000065 * tolerance/0.05;
    End If;
    tolerance := tolerance *1.01;   -- just a guard factor
    
    rings := TRUNC(Info_Array.count/3);
    FOR i in 1.. rings LOOP

      j := (i-1) *3 + 1;
      LB := Info_Array(j);

      xlast := XYOrd(LB);
      ylast := XYOrd(LB+1);
      LB := TRUNC(LB/2) + 2;
      If i = rings then
        UB := TRUNC(XYOrd.count/2);
      Else
        UB := TRUNC((Info_Array(j+3) -1)/2);
      End If;

      if xlast = XYOrd(UB*2-1) and ylast = XYord(UB*2) then
        closed := TRUE;
 --       dbms_output.put_line('closed ' );
        If XYORD.count <= 8 then
           RETURN 0;
        End If;
        xbef := XYOrd(UB*2-3);
        ybef := XYOrd(UB*2-2);
      end if;
      k := k + 2;

      Info_Array(j) := k;
      NewXYOrd(k) := xlast;
      NewXYOrd(k+1) := ylast;

      remaining := UB-LB+1;
      
-- For a closed loop check angle at the start/end node
      if closed = TRUE then
--  When remaining is > 0 we may still drop vertices and still have a least 4 vertices
         remaining := remaining -4;
                 
         xnew := XYOrd(LB*2+3);
         ynew := XYOrd(LB*2+4);
 
        if (xbef = xlast and ybef= ylast)  then
           xbef := XYOrd(UB*2-5);
           ybef := XYOrd(UB*2-4);
        end if;
        included_angle := angle(xbef,ybef,xlast,ylast,xnew,ynew,b1,b2);
--           dbms_output.put_line('ii ' || ii || ' angle ' ||ROUND(included_angle,10) || ' keep ' || keep_it(jj));

        
-- set up drop_it iff the angle is bad at the start of the polygon
        if included_angle < angle_threshold then
           xbef2 := XYOrd(UB*2-5);
           ybef2 := XYOrd(UB*2-4);
 
           xnext := XYOrd(LB*2+1);
           ynext := XYOrd(LB*2+2);
 
 -- Measure the angle without vertex (n-1) or vertex 2
 --                    + (n-2)
 --                    |
 --            1 +_____+ (n-1)
 --               \ tight angle
 --                \
 --              2  +-----+ 3
 --
           if angle(xbef2,ybef2,xlast,ylast,xnew,ynew,b1,b2) > 
                angle(xbef,ybef,xlast,ylast,xnext,ynext,b1,b2) then
             keep_it(UB-1) := 2.;   -- drop 2nd to last vertex (last = first)
           else
             keep_it(LB +1):= 2.;  -- drop 2nd vertex
           end if;
--           if Keep_it(UB-1) <> 1. and included_angle2 > angle_threshold then
--             drop_it := UB-1;   -- drop 2nd to last vertex (last = first)
--           elsif included_angle2 > angle_threshold then
--             drop_it := LB +1;  -- drop 2nd vertex
        end if;
      end if;
  
      For jj in LB..UB LOOP
        ii := jj*2-1;
        xnew := XYOrd(ii);
        ynew := XYOrd(ii+1);
--        dbms_output.put_line('jj' || jj);


-- Figure out if the angle is becoming acute         
        len1 := len2;
        len2 := dlen(xnew,ynew,xlast,ylast);
        if jj <> UB then
          xnext := XYOrd(ii+2);
          ynext := XYOrd(ii+3);
        end if;
        len3 := dlen(xnext,ynext,xlast,ylast);
--        if len3 < len1 and jj > 2 then
          if jj > 2 then
           xbef := XYOrd(ii-4);
           ybef := XYOrd(ii-3);
           xbeh := XYOrd(ii-2);
           ybeh := XYOrd(ii-1);
           theta1 := angle(xbef,ybef,xbeh,ybeh,xnext,ynext,b1,b2);
           theta2 := angle(xbef,ybef,xnew,ynew,xnext,ynext,b1,b2);
           included_angle := angle(xbef,ybef,xbeh,ybeh,xnew,ynew,b1,b2);
--           if jj >=27 then
--            dbms_output.put_line('jj' || jj || ' A ' || round(included_angle,4));
--            dbms_output.put_line('b1 ' ||round(b1,6) || ' b2 ' || round(b2,6));
--            dbms_output.put_line('xbef ' || xbef || ' y  '|| ybef);
--            dbms_output.put_line('xbeh ' || xbeh || ' y' || ybeh);
--            dbms_output.put_line('xbef ' || xnew || ' y '|| ynew);
--           end if;
        else
           theta1 := 0.;
           theta2 := 0.;
           included_angle := ok;
        end if;
-- Setup the distance between vertices
-- Empirical set of comparisons so we rarely calculate the distance.
        distance := ok;
        if abs(xnew - xlast) < xcheck and abs(ynew - ylast) < ycheck then
            
-- This new function is very accurate and is usually sub-millimeter accurate.
             distance := accurate_gcd(xlast,ylast,xnew,ynew);
--             dbms_output.put_line('d ' || round(distance,10));
        end if;
        
-- If the vertex is suppposed to be dropped at the beginning or end of a loop
-- do nothing
--dbms_output.put_line('r ' ||remaining || ' jj ' || jj || ' k ' || keep_it(jj));
--dbms_output.put_line('r ' ||remaining || ' jj ' || jj || ' angle ' || round(included_angle,4) || ' kk ' || k);
        IF keep_it(jj) = drop_it THEN 
           NULL;

-- Main decision point. We will save if we must save (user specified by Keep_it
--                                         or we need the vertex to make a loop);


        ELSIF Keep_it(jj) = 1 or remaining = 0 or distance <= tolerance or 
              included_angle < angle_threshold then
--        dbms_output.put_line('r ' ||remaining || ' jj ' || jj || ' abs  ' || abs(xnew -xlast) || ' xcheck ' || xcheck ||' abs  ' || abs(ynew-ylast) || ' ycheck ' || ycheck);
            
-- drop it if within tolerance (equal or less)
-- For angles there are 3 vertices involved and we must drop the last vertex
-- by storing the current when the angle < 20

-- Check for a Zig-zag. If so keep the biggest angle.
--              +--------+              +--------+               +
--                       /                  theta1   \              \  theta2
--                      +--------+                       +              +-------+
--
-- If the angle ignoring the current vertex is bigger than ignoring the
-- last vertex, drop the current vertex.
--dbms_output.put_line('A');
          if keep_it(jj) =0 and remaining > 0 and (distance <= tolerance or 
             (included_angle < angle_threshold and theta1 > theta2)) then
             NULL;
--             dbms_output.put_line('B' || UB);
          else
                    
-- dbms_output.put_line('C');
-- If the last vertex is too close, then overwrite the previous one
             if jj= UB and remaining > 0 and distance <= tolerance then
               NULL;
             else
               k := k + 2;
             end if;
             NewXYOrd(k) := xnew;
             NewXYOrd(k+1) := ynew;
             xbef := xlast;
             ybef := ylast;
             xlast := xnew;
             ylast := ynew;
             remaining := remaining - 1;
          end if;


        ELSE
-- Usual case - vertices wide apart and we save a vertex
          k := k+2;
--          dbms_output.put_line('Saving ' || JJ);
          jlast := jj;
-- Store coordinates
          NewXYOrd(k) := xnew;
          NewXYOrd(k+1) := ynew;
          xbef := xlast;
          ybef := ylast;
          xlast := xnew;
          ylast := ynew;
        End If;
 
      End Loop;
    END LOOP;
/*
      For jj in LB..UB LOOP
        ii := jj*2-1;
        xnew := XYOrd(ii);
        ynew := XYOrd(ii+1);
     dbms_output.put_line('jj' || jj);
 


        
-- If the vertex is supppsed to be dropped at the beginning or end of a loop
-- do nothing
        IF jj = drop_it THEN 
           NULL;
--  When remaining is > 0 we may still drop vertices and still have a least 4 vertices
--
-- Empirical set of comparisons so we rarely calculate the distance.
--dbms_output.put_line('r ' ||remaining || ' jj ' || jj || ' abs  ' || abs(xnew -xlast) || ' xcheck ' || xcheck ||' abs  ' || abs(ynew-ylast) || ' ycheck ' || ycheck);
        ELSIF remaining > 0 AND 
              ((Keep_it(jlast) <> 1. or Keep_it(jj) <> 1.)and abs(xnew - xlast) < xcheck and abs(ynew - ylast) < ycheck)
          OR ((Keep_it(jlast) <> 1. or Keep_it(jj) <> 1.) and included_angle < angle_threshold) then
        
-- This new function is very accurate and is usually sub-millimeter accurate.
            distance := accurate_gcd(xlast,ylast,xnew,ynew);
dbms_output.put_line('d ' || round(distance,10));

-- drop it if within tolerance (equal or less)
          if distance <= tolerance then
             NULL;
             
-- For angles there are 3 vertices involved and we must drop the last vertex
-- by storing the current when the angle < 20

          elsif Keep_it(jlast) <> 1. and included_angle < angle_threshold then
              dbms_output.put_line('sssaving ' || JJ);
                 XYOrd(k) := xnew;
                 XYOrd(k+1) := ynew;
                 xbef := xlast;
                 ybef := ylast;
                 xlast := xnew;
                 ylast := ynew;
                 remaining := remaining - 1;
 
          else
-- Else we are going to store
-- If we increment k, we don't overwrite
            if k = 1 or (distance > tolerance and included_angle >= angle_threshold) then
              k := k+2;
              dbms_output.put_line('saving ' || JJ);            
            else
-- drop the previous vertex by overwriting
              remaining := remaining - 1;
            End If;

-- keep it and store coordinates
           jlast := jj;
           XYOrd(k) := xnew;
           XYOrd(k+1) := ynew;
           xbef := xlast;
           ybef := ylast;
           xlast := xnew;
           ylast := ynew;

          End If;
        ELSE
-- Usual case - vertices wide apart and we save a vertex
          k := k+2;
          dbms_output.put_line('Saving ' || JJ);
          jlast := jj;
-- Store coordinates
          XYOrd(k) := xnew;
          XYOrd(k+1) := ynew;
          xbef := xlast;
          ybef := ylast;
          xlast := xnew;
          ylast := ynew;
        End If;
 
      End Loop;
    END LOOP;
*/

    k := k + 1;
    If k <> NewXYOrd.count then
       NewXYOrd.trim(XYOrd.count-k);  -- trim by the difference
  
       geom := MDSYS.SDO_GEOMETRY(GTYPE,SRID,NULL,Info_Array,NewXYOrd);
       RETURN XYOrd.count;
    Else
       RETURN 0;
    End If;

END NEW_Remove_close_Xys;
--
FUNCTION REMOVE_CLOSE_XYS(Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,scale NUMBER,area NUMBER default 0.0,ptolerance NUMBER default 0.05)
 RETURN NUMBER Deterministic IS
/**
 ################################################################################
 # Program Name: Remove_DupXys
 # Author: Sidey Timmins
 # Creation Date: 7/22/2008
 # Update: 03/11/2010 To not remove vertices when there are only 2.
 #
 # Usage:
 #   This PL/SQL procedure has 2 parameters:
 #                   Geom: the Geometry to check and fix.
 #                   ptolerance: a tolerance in meters - the closest that
 #                               points will be kept.
 #   Returns TRUE only when the geometry is changed;
 #
 # Purpose: This procedure removes vertices from an edge or polygon which are too close.
 #          The resolution for our geodetic coordinates is 1 millionth of a degree.
 #          A longitude difference of 1.E-06 degrees at 64 degrees North with the
 #          same latitude is 0.047 meters and less than the 0.05 meter tolerance.
 #
 # Method: Filters vertices to determine the ones to remove.
 # Dependencies:
 #  GZ_UTIL_ZONE.accurate_length
 ################################################################################
*/

    XYOrd             MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Info_Array        MDSYS.SDO_ELEM_INFO_ARRAY;
    geom2       mdsys.sdo_geometry;

    ii          PLS_INTEGER;
    xlast       NUMBER;
    ylast       NUMBER;
    xi          NUMBER;
    yi          NUMBER;
    xnew        NUMBER;
    ynew        NUMBER;
    Minimum_len NUMBER := ptolerance; --scale * 0.001016*0.5;
    area_cutoff NUMBER;
    tri_area    NUMBER;
    last_distance NUMBER := minimum_len;
    distance_across NUMBER := minimum_len;
    k           PLS_INTEGER := 1;
    lastk       PLS_INTEGER :=1;
    ycheck      NUMBER;
    xcheck      NUMBER;
    distance    NUMBER := 1.E10;
    rings       PLS_INTEGER;
    remaining     PLS_INTEGER := 0;
    LB          PLS_INTEGER;
    UB          PLS_INTEGER;
    j           PLS_INTEGER;
    drop_it     PLS_INTEGER :=0;
    closed      BOOLEAN := FALSE;

    angle_threshold NUMBER := 25.;   -- was 25. degrees
    tolerance   NUMBER := ptolerance;
    tolerance2  NUMBER;
    next_angle  NUMBER;
    GTYPE       NUMBER;
    SRID        NUMBER:= Geom.SDO_SRID;
    b1          NUMBER;
    b2          NUMBER;
    xa          NUMBER;
    ya          NUMBER;
    xb          NUMBER := 0.;       -- Beyond North pole!
    yb           NUMBER := 91.;
    xc           NUMBER;
    yc           NUMBER;
    Z            NUMBER :=1.;

    area1        NUMBER;
    area2        NUMBER;
    xnext        NUMBER;
    ynext        NUMBER;
    height       NUMBER;
    pre_angle    NUMBER;
    check_area   NUMBER :=0.0;
    check_intersect NUMBER;
    sumof        NUMBER :=0.0;
    included_angle NUMBER := 90.;
    included_angle2 NUMBER := 90.;
    just_dropped BOOLEAN := FALSE;
-- Written as a procedure so as NOT to create new geometry objects and waste memory

BEGIN
 
   GTYPE := geom.SDO_GTYPE;
   If Gtype = 2001 or Gtype = 2004 then  -- Doesn't do points
      RETURN 0;
   End If;
   
  
-- Detects dups because most edges are ok. Don't remove any when their are only
-- 4 coordinates or less.

    Info_Array := geom.SDO_ELEM_INFO;
    XYORD := geom.SDo_Ordinates;
  

    If XYORD.count <= 4 then
      RETURN 0;
    End If;

    If ptolerance <= 0. then
       tolerance := 0.05;
    End if;

    area_cutoff := minimum_len*minimum_len *0.33;
    tri_area := 1.E20;
-- These parameters were tested from 0 to 72 degrees North with random coordinates
-- to check that this procedure got the same results as the Oracle function
-- SDO_UTIL.REMOVE_DUPLICATE_VERTICES. Note that the Oracle function may change the
-- coordinates!

    tolerance2 := tolerance *1.01;
    xlast := XYOrd(1);
    ylast := XYOrd(2);
    if ylast > 50. then
      xcheck   := 0.0000016 * tolerance/0.05;
      ycheck   := 0.0000005 * tolerance/0.05;
    else
      xcheck   := 0.00000075 * tolerance/0.05;
      ycheck   := 0.00000065 * tolerance/0.05;
    End If;

    rings := TRUNC(Info_Array.count/3);
    FOR i in 1.. rings LOOP

      j := (i-1) *3 + 1;
      LB := Info_Array(j);

      xa := XYOrd(LB);
      ya := XYOrd(LB+1);
      LB := TRUNC(LB/2) + 1;
      If i = rings then
        UB := TRUNC(XYOrd.count/2);
      Else
        UB := TRUNC((Info_Array(j+3) -1)/2);
      End If;

-- Setup a default keep_it when it has not gone thru Zone and Visvalingam's algo
      for jj in LB..UB loop
        sumof := sumof + keep_it(jj);
      end loop;
--     dbms_output.put_line('sumof ' || sumof);
      if sumof = 2. then
       for jj in LB+1..UB-1 loop
        keep_it(jj) := -2.;
       end loop;         
      end if;
      
      if xa = XYOrd(UB*2-1) and ya = XYord(UB*2) then
        closed := TRUE;
        check_area := Centroid(XYOrd,xc,yc);
--        dbms_output.put_line('closed ' || check_area);
        If XYORD.count <= 8 then
           RETURN 0;
        End if;

     
        
-- When we start for a ring we have these picture:

--                      (xa,ya)  +--------------+ (xlast,ylast)
--                              /start           \   under consideration
--                             / = accepted       \
--                            /                    + (xnew,ynew)
--                           /                          current
--     (xc,yc) +------------+ (xb,yb)  b = behind
--     c = further behind


        xb := XYOrd(UB*2-3);
--        dbms_output.put_line(' xxb ' || round(xb,10));
        yb := XYOrd(UB*2-2);
        xlast := XYOrd(LB*2+1);
        ylast := XYOrd(LB*2+2);
        included_angle := GZ_UTIL_ZONE.angle(xb,yb,xa,ya,xlast,ylast,b1,b2,SRID);
--        dbms_output.put_line(' angle2 ' ||ROUND(included_angle,10));
        -- set up drop_it iff the angle is bad at the start of the polygon
        if included_angle < angle_threshold then
           xc := XYOrd(UB*2-5);
           yc := XYOrd(UB*2-4);
--           included_angle2 := GZ_UTIL_ZONE.angle(xbef2,ybef2,xlast,ylast,xnew,ynew,b1,b2);
--dbms_output.put_line(' angle2 ' ||ROUND(included_angle2,10));
--           if included_angle2 > angle_threshold then
             area1 := GZ_UTIL_ZONE.Triangle_area(xc,yc,xb,yb,xa,ya,SRID);
             xnew := XYOrd(LB*2+3);
             ynew := XYOrd(LB*2+4);
             area2 := GZ_UTIL_ZONE.Triangle_area(xa,ya,xlast,ylast,xnew,ynew,SRID);

--             dbms_output.put_line('area1 ' || round(area1,6) || ' area2 ' || round(area2,6));
-- We want to drop a vertex because the start angle is poor but we must ensure that
-- the new segment from the end of the loop to the 3rd to last vertex will not intersect
-- the 2nd segment

             check_intersect := line_intersect(xa,ya,xc,yc,xlast,ylast,xnew,ynew,xi,yi);
             if area1 < area2 and check_intersect = -1. then
               Keep_it(UB-1) := 0.;   -- drop 2nd to last vertex (last = first)
--               dbms_output.put_line(' dropping 2nd to last' || (UB-1));
             else
               Keep_it(LB +1) := 0.;  -- drop 2nd vertex
             end if;
--           if Keep_it(UB-1) <> 1. and included_angle2 > angle_threshold then
--             drop_it := UB-1;   -- drop 2nd to last vertex (last = first)
--           elsif included_angle2 > angle_threshold then
--             drop_it := LB +1;  -- drop 2nd vertex
 --          end if;
           included_angle := 180.;
        end if;
      end if;
      
      
--  Now we proceed with this picture:
--                                                 new = current
--               (xlast,ylast)   +--------------+ (xnew,ynew)
--                              /last
--                             / = under consideration
--                            /
--                           /
--     (xb,yb) +------------+ (xa,ya)  a = accepted
--     b = behind

      Info_Array(j) := k;
      XYOrd(k) := xa;
      XYOrd(k+1) := ya;
  


      remaining := UB-LB+1;

--  When remaining is > 0 we may still drop vertices and still have a least 
--  2 vertices for a normal edge and 4 vertices for a loop 

  -- A loop requires at least 4 vertices for a triangle (1st and last the same.)
  -- but a rectangle is preferred.
      if closed = TRUE then
         remaining := remaining -6;   -- new used to be 4
         area_cutoff := area *0.2;
      else
         remaining := remaining -2;
      end if;
--  dbms_output.put_line('remaining '|| remaining);
--  dbms_output.put_line('UB ' || UB || ' xyord ' || xyord.count || 'AC ' || area_cutoff);
       For jj in LB+1..UB LOOP
--       dbms_output.put_line('jj ' || jj || ' lb ' || lb || ' xb ' || round(xb,10)|| ' xlast ' || round(xlast,8) || ' ylast ' || round(ylast,8));
        ii := jj*2-1;
        xnew := XYOrd(ii);
        ynew := XYOrd(ii+1);
           last_distance := distance;
           distance := GZ_UTIL_ZONE.accurate_gcd(XYOrd(k),XYOrd(k+1),xnew,ynew,SRID);

           pre_angle := 180.0;           
           if k > 2 then
             distance_across :=  GZ_UTIL_ZONE.accurate_gcd(XYOrd(k-2),XYOrd(k-1),xnew,ynew,SRID);
             pre_angle :=  GZ_UTIL_ZONE.angle(XYOrd(k-2),XYOrd(k-1),XYOrd(k),XYOrd(k+1),xnew,ynew,b1,b2,SRID);
             z := GZ_UTIL_ZONE.orient2D(XYOrd(k-2),XYOrd(k-1),XYOrd(k),XYOrd(k+1),xlast,ylast) *
                  GZ_UTIL_ZONE.orient2D(XYOrd(k),XYOrd(k+1),xlast,ylast,xnew,ynew);
           end if;

           included_angle := 180.;
           next_angle := 180.;
           tri_area := 1.E20;
-- Setup to calculate areas of skinny triangles         
         IF jj-LB > 1 THEN
         
               if (xb <> xlast or yb <> ylast) and yb <> 91. then

                 included_angle := GZ_UTIL_ZONE.angle(xb,yb,xlast,ylast,xnew,ynew,b1,b2);
--dbms_output.put_line('xb ' || round(xb,10) || ' yb ' || round(yb,10));
--dbms_output.put_line('xl ' || round(xlast,10) || ' yl ' || round(ylast,10));
--dbms_output.put_line('xn ' || round(xnew,10) || ' yn ' || round(ynew,10));
-- dbms_output.put_line('ii ' || ii || ' angle ' ||ROUND(included_angle,10) || ' keep ' || keep_it(jj)|| ' distance ' || round(distance,4));
              end if;

             if k > 2 and (distance < minimum_len or
             GZ_UTIL_ZONE.accurate_gcd(XYOrd(k-2),XYOrd(k-1),XYOrd(k),XYOrd(k+1),SRID)
                 < minimum_len or distance_across < minimum_len) then
                               
              if included_angle < angle_threshold and distance_across < minimum_len then
               tri_area := Triangle_area(xb,yb,xlast,ylast,xnew,ynew,SRID);
--               dbms_output.put_line('TRI ' || round(tri_area,3));
--               dbms_output.put_line('jj ' || jj || ' xbef ' || round(xbef,6) || ' ybef ' || round(ybef,6));

--               dbms_output.put_line('jj ' || jj || ' xlast ' || round(xlast,6) || ' ylast ' || round(ylast,6));
--   dbms_output.put_line('jj ' || jj || ' xnew ' || round(xnew,6) || ' ynew  ' || round(ynew,6));
             end if; 
--               dbms_output.put_line('xbef ' || xbef || 'ybef ' || ybef);
--                dbms_output.put_line('xbef ' || xlast || 'ybef ' || ylast);
--                 dbms_output.put_line('xnew ' || xnew || 'ybef ' || ynew);
             end if;
             
             if jj <= UB -1 then
                next_angle := GZ_UTIL_ZONE.angle(xb,yb,xlast,ylast,XYord(ii+2),XYord(ii+3),b1,b2,SRID);             
             end if;
         END IF;
         

--dbms_output.put_line('jj ' || jj || ' D ' || round(distance,3) || ' tol ' || round(tolerance,3) || '  TR ' || round(tri_area,3) || ' AC ' || round(area_cutoff,6) || ' remain ' || remaining);
--dbms_output.put_line('IA ' || round(included_angle,4) || ' keep ' || keep_it(jj) || ' ML ' || minimum_len || ' across ' || round(distance_across,3));
--dbms_output.put_line('jj ' || jj || ' xyord(k) ' || round(xyord(k),6) || ' yord ' || round(xyord(k+1),6));
--dbms_output.put_line('jj ' || jj || ' xlast ' || round(xlast,6) || ' ylast ' || round(ylast,6));
--dbms_output.put_line('jj ' || jj || ' xnew ' || round(xnew,6) || ' ynew  ' || round(ynew,6));


         IF jj <> UB and remaining > 0 and ((Z > 0.0 and Keep_it(jj) = -2. and included_angle <= angle_threshold and next_angle > included_angle) or
            (Keep_it(jj) = -2. and distance < 0.75*tolerance) or (Keep_it(jj) = -2. and tri_area < area_cutoff) or Keep_it(jj)= 0.0) THEN
             if just_dropped then
                remaining := remaining -1;
             end if;
            just_dropped := TRUE;
--            dbms_output.put_line('at NULL' || k);
--  If we are supposed to save it, do so
        ELSIF jj = UB or (Keep_it(jj) = 1. and included_angle > angle_threshold) or remaining = 0 THEN
--dbms_output.put_line('here');
-- Remove tiny triangle slivers
         
        IF NOT just_dropped and lastk> 1 and remaining > 0 and jj <> UB and (tri_area < area_cutoff or last_distance < tolerance) then
-- we want to drop the last vertex
           k := lastk;
--           dbms_output.put_line('drop last vtx'  || ' last ' || round(last_distance,3) || 'tol ' || tolerance || ' tri ' || round(tri_area,3) || ' cut ' || area_cutoff);
           remaining := remaining - 1;
        ELSE
           k := k + 2;
        END IF;

           just_dropped := FALSE;
           XYOrd(k) := xnew;
           XYOrd(k+1) := ynew;
           xb := xlast;
           yb := ylast;
           xlast := xnew;
           ylast := ynew;
           lastk := k;
           Keep_it(TRUNC(k+1)/2) := Keep_it(jj);
--            dbms_output.put_line('Saving ' || jj || ' K ' || k || ' xnew ' || xnew || ' ynew ' || ynew);
-- If the vertex is to be dropped don't change anything
-- As soon as we get to a vertex we know its distance and can drop it.


--        ELSIF distance < tolerance THEN
--           NULL;
           
-- We don't know the angle at a vertex becuase we don't know yet its
-- forward arm which may change if a vertex is dropped along that arm

        ELSE

-- Don't do this for a non-closed line string at the beginning
        if (xb <> xlast or yb <> ylast) and yb <> 91. then

          included_angle := GZ_UTIL_ZONE.angle(xb,yb,xlast,ylast,xnew,ynew,b1,b2,SRID);
-- dbms_output.put_line('II ' || ii || ' angle ' ||ROUND(included_angle,10) || ' keep ' || keep_it(jj));
        end if;
        

-- When the included angle is too small, this affects a vertex we have already accepted.
--
--
--        (xbef,ybef)  +---------+ (xlast,ylast)
--                               /
--                              /
--                             /
--                            +  (xnew,ynew)

-- So (xlast,ylast) becomes (xnew,ynew) and (xbef,ybef) stays the same.


-- For angles there are 3 vertices involved and we must drop the last vertex
-- by storing the current when the angle < 20

-- work out the effective height of the triangle (Area = 0.5 * base * height)

--          height := 2.* tri_area/distance_across;
--dbms_output.put_line('inc angle ' || round(included_angle,5) || 'AT ' || angle_threshold || 'tri ' || round(tri_area,4) || ' AC ' || area_cutoff);
--dbms_output.put_line('height is ' || round(height,3) || ' across ' || round(distance_across,3));
              if included_angle < angle_threshold and tri_area < area_cutoff then
--               dbms_output.put_line('dropping ' || jj  || ' K ' || k || ' tri ' || round(tri_area,3));
                 if k =1 then
                   k := 3;
                 else
                   remaining := remaining - 1;
                 end if;
                 just_dropped := TRUE;
                 XYOrd(k) := xnew;
                 XYOrd(k+1) := ynew;
                 xlast := xnew;
                 ylast := ynew;                 
                 lastk := k;
                  Keep_it(TRUNC(k+1)/2) := Keep_it(jj);
          else --if (last_distance >= tolerance or included_angle >= angle_threshold) then
----dbms_output.put_line('D ' || round(last_distance,3) || ' tol '|| round(tolerance,3) || ' IA ' || round(included_angle,3) || ' athresh ' || angle_threshold);
          -- Remove tiny triangle slivers
        IF NOT just_dropped and lastk > 1 and remaining > 0 and tri_area < area_cutoff then
-- we want to drop the last vertex
           k := lastk;
           remaining := remaining - 1;
--                     dbms_output.put_line('DROPPing ' || jj ||' k ' || k);
        ELSE
          
-- Usual case - vertices wide apart and we save a vertex
              k := k+2;
              just_dropped := FALSE;
--                        dbms_output.put_line('saving ' || jj ||' k ' || k|| ' xnew ' || round(xnew,6) || ',' || round(ynew,6));
        END IF;

-- keep it and store coordinates

           XYOrd(k) := xnew;
           XYOrd(k+1) := ynew;
           xb := xlast;
           yb := ylast;
           xlast := xnew;
           ylast := ynew;
           lastk :=k;
           Keep_it(TRUNC(k+1)/2) := Keep_it(jj);
          End If;


        END IF;
      End Loop;
    END LOOP;


    k := k + 1;

      If k <> XYOrd.count then
         XYOrd.trim(XYOrd.count-k);
         if closed and ((check_area > 0 and Centroid(XyOrd,xc,yc) < 0) or
                        (check_area < 0 and Centroid(XyOrd,xc,yc) > 0)) then
         Return 0;
      else
--       for ij in 1..xyord.count loop
--          dbms_output.put_line('xy ' || xyord(ij));
--       end loop;
       geom := MDSYS.SDO_GEOMETRY(GTYPE,SRID,NULL,Info_Array,XYOrd);
--       for ii in 1..trunc(xyord.count/2) loop
--          k:= ii*2-1;
--          dbms_output.put_line('xy ' || round(xyord(k),6) || ' y ' || round(xyord(k+1),6));
--       end loop;
       RETURN XYOrd.count;
     end if;
    Else
       RETURN 0;
    End If;


END Remove_close_Xys;
--
FUNCTION OLD_REMOVE_CLOSE_XYS(Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,ptolerance NUMBER default 0.05)
 RETURN NUMBER Deterministic IS
/**
 ################################################################################
 # Program Name: Remove_DupXys
 # Author: Sidey Timmins
 # Creation Date: 7/22/2008
 # Update: 03/11/2010 To not remove vertices when there are only 2.
 #
 # Usage:
 #   This PL/SQL procedure has 2 parameters:
 #                   Geom: the Geometry to check and fix.
 #                   ptolerance: a tolerance in meters - the closest that
 #                               points will be kept.
 #   Returns TRUE only when the geometry is changed;
 #
 # Purpose: This procedure removes vertices from an edge or polygon which are too close.
 #          The resolution for our geodetic coordinates is 1 millionth of a degree.
 #          A longitude difference of 1.E-06 degrees at 64 degrees North with the
 #          same latitude is 0.047 meters and less than the 0.05 meter tolerance.
 #
 # Method: Filters vertices to determine the ones to remove.
 # Dependencies:
 #  GZ_UTIL_ZONE.accurate_length
 ################################################################################
*/

    XYOrd             MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Info_Array        MDSYS.SDO_ELEM_INFO_ARRAY;

    ii          PLS_INTEGER;
    xlast       NUMBER;
    ylast       NUMBER;
    xnew        NUMBER;
    ynew        NUMBER;
    k           PLS_INTEGER := -1;
    ycheck      NUMBER;
    xcheck      NUMBER;
    distance    NUMBER;
    rings       PLS_INTEGER;
    remaining     PLS_INTEGER := 0;
    LB          PLS_INTEGER;
    UB          PLS_INTEGER;
    j           PLS_INTEGER;
    drop_it     PLS_INTEGER :=0;
    closed      BOOLEAN := FALSE;

    angle_threshold NUMBER := 25.;
    tolerance   NUMBER := ptolerance;
    tolerance2  NUMBER;
    GTYPE       NUMBER;
    SRID        NUMBER;
    b1          NUMBER;
    b2          NUMBER;
    xbef        NUMBER := 0.;       -- Beyond North pole!
    ybef        NUMBER := 91.;
    xbef2        NUMBER ;
    ybef2        NUMBER ;
    area1        NUMBER;
    area2        NUMBER;
    xnext        NUMBER;
    ynext        NUMBER;
    included_angle NUMBER := 90.;
    included_angle2 NUMBER := 90.;
-- Written as a procedure so as NOT to create new geometry objects and waste memory

BEGIN

   GTYPE := geom.SDO_GTYPE;
   If Gtype = 2001 or Gtype = 2004 then  -- Doesn't do points
      RETURN 0;
   End If;

-- Detects dups because most edges are ok. Don't remove any when their are only
-- 4 coordinates or less.

    Info_Array := geom.SDO_ELEM_INFO;
    XYORD := geom.SDo_Ordinates;
    

    If XYORD.count <= 4 then
      RETURN 0;
    End If;

    If ptolerance <= 0. then
       tolerance := 0.05;
    End if;

-- These parameters were tested from 0 to 72 degrees North with random coordinates
-- to check that this procedure got the same results as the Oracle function
-- SDO_UTIL.REMOVE_DUPLICATE_VERTICES. Note that the Oracle function may change the
-- coordinates!

    tolerance2 := tolerance *1.01;
    ylast := XYOrd(2);
    if ylast > 50. then
      xcheck   := 0.0000016 * tolerance/0.05;
      ycheck   := 0.0000005 * tolerance/0.05;
    else
      xcheck   := 0.00000075 * tolerance/0.05;
      ycheck   := 0.00000065 * tolerance/0.05;
    End If;

    rings := TRUNC(Info_Array.count/3);
    FOR i in 1.. rings LOOP

      j := (i-1) *3 + 1;
      LB := Info_Array(j);

      xlast := XYOrd(LB);
      ylast := XYOrd(LB+1);
      LB := TRUNC(LB/2) + 2;
      If i = rings then
        UB := TRUNC(XYOrd.count/2);
      Else
        UB := TRUNC((Info_Array(j+3) -1)/2);
      End If;

      if xlast = XYOrd(UB*2-1) and ylast = XYord(UB*2) then
        closed := TRUE;
 --       dbms_output.put_line('closed ' );
        If XYORD.count <= 8 then
           RETURN 0;
        End If;
        xbef := XYOrd(UB*2-3);
        ybef := XYOrd(UB*2-2);
      end if;
      k := k + 2;

      Info_Array(j) := k;
      XYOrd(k) := xlast;
      XYOrd(k+1) := ylast;

      remaining := UB-LB+1;
      if closed = TRUE then
         remaining := remaining -4;
      end if;
  
      For jj in LB..UB LOOP
        ii := jj*2-1;
        xnew := XYOrd(ii);
        ynew := XYOrd(ii+1);

        IF jj <> drop_it THEN
-- Empirical set of comparisons so we rarely calculate the difference.
-- Don't do this for a non-closed line string
        if (xbef <> xlast or ybef <> ylast) and ybef <> 91. then

          included_angle := GZ_UTIL_ZONE.angle(xbef,ybef,xlast,ylast,xnew,ynew,b1,b2);
-- dbms_output.put_line('ii ' || ii || ' angle ' ||ROUND(included_angle,10) || ' keep ' || keep_it(jj));
        end if;
        
-- set up drop_it iff the angle is bad at the start of the polygon
        if closed = TRUE and jj = LB and included_angle < angle_threshold then
           xbef2 := XYOrd(UB*2-5);
           ybef2 := XYOrd(UB*2-4);
           included_angle2 := GZ_UTIL_ZONE.angle(xbef2,ybef2,xlast,ylast,xnew,ynew,b1,b2);
--dbms_output.put_line(' angle2 ' ||ROUND(included_angle2,10));
           if included_angle2 > angle_threshold then
             area1 := GZ_UTIL_ZONE.Triangle_area(xbef2,ybef2,xlast,ylast,xbef,ybef);
             xnext := XYOrd(ii+2);
             ynext := XYOrd(ii+3);
             area2 := GZ_UTIL_ZONE.Triangle_area(xnext,ynext,xlast,ylast,xnew,ynew);
             if area1 < area2 then
               drop_it := UB-1;   -- drop 2nd to last vertex (last = first)
             else
               drop_it := LB +1;  -- drop 2nd vertex
             end if;
--           if Keep_it(UB-1) <> 1. and included_angle2 > angle_threshold then
--             drop_it := UB-1;   -- drop 2nd to last vertex (last = first)
--           elsif included_angle2 > angle_threshold then
--             drop_it := LB +1;  -- drop 2nd vertex
           end if;
           included_angle := 180.;
        end if;
        
--  When remaining is > 0 we may still drop vertices and still have a least 4 vertices
--
        If remaining > 0 AND jj> 0 and jj < Keep_it.count and ((Keep_it(jj) <> 1. and abs(xnew - xlast) < xcheck and abs(ynew - ylast) < ycheck)
        or (( (jj>1 and Keep_it(jj-1) <> 1.) or Keep_it(jj) <> 1.) and included_angle < 20.)) then
        
-- This new function is very accurate and is usually sub-millimeter accurate.
            distance := GZ_UTIL_ZONE.accurate_gcd(xlast,ylast,xnew,ynew);

-- drop it if within tolerance (equal or less)
          if k<> 1 and (distance <= tolerance or (Keep_it(jj-1) <> 1. and included_angle < angle_threshold)) and jj <>UB then

-- For angles there are 3 vertices involved and we must drop the last vertex
-- by storing the current when the angle < 20

              if Keep_it(jj-1) <> 1. and included_angle < angle_threshold then
                 XYOrd(k) := xnew;
                 XYOrd(k+1) := ynew;
                 xbef := xlast;
                 ybef := ylast;
                 xlast := xnew;
                 ylast := ynew;
                 remaining := remaining - 1;
            end if;

          else

            if k = 1 or (distance > tolerance and included_angle >= angle_threshold) then
              k := k+2;
            else
-- drop the previous vertex at the end
              remaining := remaining - 1;
            End If;

-- keep it and store coordinates

           XYOrd(k) := xnew;
           XYOrd(k+1) := ynew;
           xbef := xlast;
           ybef := ylast;
           xlast := xnew;
           ylast := ynew;

          End If;
        Else
-- Usual case - vertices wide apart and we save a vertex
          k := k+2;

-- Store coordinates
          XYOrd(k) := xnew;
          XYOrd(k+1) := ynew;
          xbef := xlast;
          ybef := ylast;
          xlast := xnew;
          ylast := ynew;
        End If;
        END IF;
      End Loop;
    END LOOP;

    k := k + 1;
    If k <> XYOrd.count then
       XYOrd.trim(XYOrd.count-k);
       SRID := geom.SDO_SRID;
       geom := MDSYS.SDO_GEOMETRY(GTYPE,SRID,NULL,Info_Array,XYOrd);
       RETURN XYOrd.count;
    Else
       RETURN 0;
    End If;


END OLD_Remove_close_Xys;
--
FUNCTION ZONE(INX IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  In_start PLS_INTEGER, In_End PLS_INTEGER,
                                  Nzone PLS_INTEGER, Nzone_min PLS_INTEGER,
                                  Vertex     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  B_to_V IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Xes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Yes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Start_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  In_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Order_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Mean_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Zone_constant IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Minimum_len NUMBER,
                                  Min_Inzone PLS_INTEGER default 3, Difference NUMBER default 0.
                                  )
RETURN PLS_INTEGER AS
/*
********************************************************************************
--Program Name: zone
--Author: Sidey Timmins
--Creation Date: 2/21/2008
--Usage:
  -- Call this function from inside a PL/SQL program.

  --   REQUIRED Parameters:

  --            InX:  Array of numbers
  --            In_Start, In_End : array elements into InX to start, end processing.
  --            NZone: number of zones to attempt to create.
  --            Min_InZone: Minimum number of elements allowed in the zone.
  --                 This parameter prevents zonation if set too big.
  --   Output:
  --            Start_zone: beginning of a found zone
  --            In_zone: number of array elements in the zone
  --            Order_Zone: order zone was picked
  --            Mean_Zone: average zone value
  --            Zone_constant: indicator of how "good" the zonation is with
  --               1 being perfect, 0 being homogenous and -1 being meaningless.
  --
  --            Returns number of zones found. (A zone is a set of consecutive
  --            array elements with similar values. Data is assumed to be stationary
  --            - with regions of similar means, not trending upwards or
  --            downwards like the stock market !)
  --
--Purpose:   -- Find one or more zones in an array by dividing the extent
--              into regions so the within zone variance is smaller than
--              the between zone variance. Data must be non-negative.
-- Method:      This function uses a comparison of 2 independently computed
--              estimates of variance (Dixon,WJ and FJ Massey 1957 pp 147
--              Introduction to Statistical analysis") to divide values of a
--              1-d function into 1 or more zones - regions with similar values.
-- Reference:
--            "Application of a Statistical Zonation method to Reservoir
--             Evaluation and Digitized-Log Analysis" by Dan Gill
--             AAPG Bulletin, v 54, #5 May 1970 pp 719-729.
--             Coded with corrrections from original FORTRAN code by Dan Gill,
--             Kansas Geological Survey Computer Contribution, 1970
--Dependencies: none
--Called by: GZ_UTIL_ZONE.Find_VIPS
********************************************************************************
*/
  Q            NUMBER := 0.;
  S            NUMBER := 0.;
  XSQR         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  PrevR        NUMBER;
  PrevB        NUMBER;
  PrevLz       NUMBER;
  LRS_PrevLZ   NUMBER;
  PrevW        NUMBER;
  NZ           NUMBER := 0;
  N            NUMBER;
  LastI        PLS_INTEGER;
  LastL        PLS_INTEGER;
  LL           PLS_INTEGER;
  KK           PLS_INTEGER;
  last         PLS_INTEGER;
  UZ           NUMBER;
  LZ           NUMBER;
  CON          NUMBER;
  P            NUMBER;
  R            NUMBER;
  B            NUMBER;
  W            NUMBER;
  bearing0     NUMBER;
  bearing1     NUMBER;
  bearing2     NUMBER;
  bearing3     NUMBER;
  bearing00    NUMBER;
  bearing33    NUMBER;
  included_angle NUMBER := 180.;
  pre_angle    NUMBER;
  post_angle   NUMBER;

  Deltaz       NUMBER := 1000000.;
  deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.

  nearly1      NUMBER := 1. - 1.E-12;
  about1       NUMBER := 1. + 1.E-12;

  negnearly1   NUMBER := -nearly1;
  negabout1    NUMBER := -about1;
  x0           NUMBER;
  y0           NUMBER;
  x1           NUMBER;
  y1           NUMBER;
  x2           NUMBER;
  y2           NUMBER;
  y            NUMBER;
  dx           NUMBER;
  dy           NUMBER;
  distance1    NUMBER := 0.;
  distance2    NUMBER := 0.;
  siny         NUMBER;
  cosy         NUMBER;
  cosy2        NUMBER;
  yfactor      NUMBER;
  minimum_dist NUMBER := minimum_len;
  big_dist     NUMBER := 2.5*minimum_dist;
  dist_factor  NUMBER := 111319.490793274; --111319.89;  -- = 1855.3315*60
  c1           NUMBER;
  x3           NUMBER;
  y3           NUMBER;
  pos3         PLS_INTEGER;
  x4           NUMBER;
  y4           NUMBER;
  dist4        NUMBER;
  x5           NUMBER;
  y5           NUMBER;
  b2           NUMBER;
  xsquared     NUMBER;
  v1           NUMBER;
  v2           NUMBER;
  d1           NUMBER;
  d2           NUMBER;
  Minimum_in_zone PLS_INTEGER := Min_inzone;
  J            PLS_INTEGER;
  M            PLS_INTEGER;
  L            PLS_INTEGER;
  K            PLS_INTEGER;
  J1           PLS_INTEGER;
  K1           PLS_INTEGER;
  longer       PLS_INTEGER;
  pos          PLS_INTEGER;
  pos1         PLS_INTEGER;
  pos2         PLS_INTEGER;
  pre          PLS_INTEGER;
  post         PLS_INTEGER;
  posb         PLS_INTEGER;
  xb           NUMBER;
  yb           NUMBER;
  Nzone2       PLS_INTEGER := Nzone * 2;
  Nzone_q1     PLS_INTEGER := TRUNC(Nzone/4);
  Nzone_q3     PLS_INTEGER ;
  lzz          NUMBER;
  neg_count    PLS_INTEGER := 0;
  nn           PLS_INTEGER := Yes.count;
  decim        NUMBER := 0.00001;
  closed       BOOLEAN := FALSE;
BEGIN

    If Minimum_in_zone <1 then
       Minimum_in_zone := 1;
    end if;

    If Nzone_q1 < 3 then
       Nzone_q1 := 3;
    end if;
    Nzone_q3 := Nzone - Nzone_q1;
    if Nzone_min > Nzone_q1 and Nzone_min <= Nzone then
      Nzone_q1 := Nzone_min;
    end if;

--    dbms_output.put_line('nzone_q1 ' || nzone_q1 || ' Nzone_q3 ' || Nzone_q3);

    longer := Minimum_in_zone * 4;

    N  := In_end-In_start+1;

-- Make all arrays longer so they don't need to be extended
    Start_zone.extend(Nzone2);
    In_zone.extend(Nzone2);
    Order_zone.extend(Nzone2);
    Mean_zone.extend(Nzone2);
    Zone_constant.extend(Nzone2);

-- IS it closed?
    If Xes(1) = Xes(nn) and Yes(1) = Yes(nn) then
      closed := TRUE;
    End If;

    If (Yes(1)<> Yes(nn)) then
        y := abs(Yes(1)+ Yes(nn)) * 0.5;
      Else
         m := ROUND(nn/2);
         y := abs(Yes(1)+ Yes(m)) * 0.5;
      End if;
      siny := GZ_UTIL_ZONE.sincos(y*deg2rad,cosy);
--      cos2y := cosy*cosy* (1.000 + siny*0.00461);  -- empirical factor improves
--                                                    values along parallels
      c1 := 1. - y * 1.09E-07;
      cosy := cosy* (c1+ siny*siny*0.00336);

-- Contiguous USA
--      if y < 10. then
--         yfactor := 0.992 * (1. + 24. * y /dist_factor);
--      elsif y < 37. then
--         yfactor := 0.992 * (1. + 14.6 * y /dist_factor);
--      elsif y < 47. then
--         yfactor := 0.992 * (1. + 15.4 * y /dist_factor);
--      else -- Alaska
--         yfactor := 0.992 * (1. + 16.33 * y  /dist_factor);
--      end if;
      yfactor := 0.9933 * (1. + 1130. * siny*siny /dist_factor);
      yfactor := yfactor* yfactor;

-- Form Sum Of Inx and Sum of Inx**2
    For ii in In_Start..In_end LOOP
      Q := Q + Inx(ii);
      xsquared := Inx(ii) * Inx(ii);
      S := S + xsquared;
    End Loop;

 -- Set up first zone
    Start_zone(1) := In_Start;
    In_zone(1) := N;
    Order_zone(1) := 1;
    Mean_zone(1) := Q;
    Zone_constant(1) := 0.0;

    Q := Q*Q / N;
    LL := 1;   -- the current zone under consideration
    
-- If user asked for just 1 zone we are done so avoid loop

    if NZone=1 then NZ :=1; end if;

-- Increment number of zones
  WHILE NZ <= (Nzone-1) LOOP
    NZ := NZ + 1;
    PrevR := negnearly1;
    PrevLZ := 0.0;
    lastI := 1;
    lastL := 1;
--
-- When we subdivide the current zone (L) we break the current range into 2 parts,
-- the top and the bottom. The top extends from the start of the zone (J) to the
-- current possible subdivision (I) and the bottom start at the value beyond I to
-- the bottom of the current zone (K).

    For L in 1..NZ Loop

      J := Start_zone(L);
-- be very careful not to change K - it affects the calculations below
      K := J + In_Zone(L) -1;
      J1 := J + Minimum_In_Zone-1;
      K1 := K - Minimum_In_Zone;

      if J = 1 then
        pos := 1;
      else
        pos := b_to_v(J);
      end if;
-- (x0,y0) is the start of the zone
      x0 := Xes(pos);
      y0 := Yes(pos);

      KK  := K+1;
      If KK < In_end then
        pos2 := b_to_v(kk);    -- used to be b_to_v(kk+1)??
      else
        pos2 := Xes.count;
      end if;
-- (x2,y2) is the start of the next zone
      x2 := Xes(pos2);
      y2 := Yes(pos2);
--dbms_output.put_line('x2 ' || round(x2,8) || ' y2 ' || round(y2,8) || ' pos2 ' || pos2);
-- Must be at least 1or 2 values in a zone to subdivide it

        If K1 >= J1 Then

        Exit When N <= (NZ+1);  -- guard division below

        UZ := 0.0;
        LZ := Mean_zone(L);
        CON := 0.0;
        For ii in 1..NZ Loop
          If ii <> L then
             CON := CON + (Mean_zone(ii) * Mean_zone(ii)) / In_zone(ii);
          End If;
        End Loop;

        For ii in J..J1-1 LOOP
           UZ := UZ + Inx(ii);
           LZ := LZ - Inx(ii);
        End Loop;


--         dbms_output.put_line('TRying to split zone ' || L || ' J ' || J || ' LZ ' ||(lz-lzz));
-- To check angle besides the included angle which we wish to maximise, the
-- pre and post angles are of interest because we don't want those to be acute.
--    (xb,yb)                         (x1,y1)                      (x3,y3)
--       +                               +                           +
--                     pre                             post
--                      +       include angle            +
--                    (x0,y0)                         (x2,y2)
--
--      We cannot measure any angles when there are less than 3 segments (3 zones)
--
         xb := NULL;
         x3 := NULL;
         If J > 1 and NZ > 1 then

--      the starts of the zones are not sorted yet so find the first zone before
           pre := GZ_UTIL_ZONE.search_hiLo(Start_Zone(L)-1.,Start_Zone,NZ);
--           bearing00 := MOD(Mean_Zone(pre)/In_Zone(pre) +360.,360.);
           pre := Start_zone(pre);
           posb := B_to_V(pre);
           xb := Xes(posb);
           yb := Yes(posb);
--            dbms_output.put_line(J || 'set xb ' || xb);
--           dbms_output.put_line('bear0 ' || ROUND(bearing00,5) || ' pre ' || pre || ' J ' || J || 'start ' || start_zone(pre));
         Elsif closed = TRUE and NZ > 2 then
            last := 0;
            for ii in 1..NZ loop

              if Start_Zone(ii) > last then
                 last := Start_Zone(ii);
                 post := ii;
              end if;
            end loop;
            post := GZ_UTIL_ZONE.search_hiLo(In_end-1.,Start_Zone,NZ);
--            bearing00 := MOD(Mean_Zone(post)/In_Zone(post) +360.,360.);
            post := Start_zone(post);
            posb := B_to_V(post);
            xb := Xes(posb);
            yb := yes(posb);
--            dbms_output.put_line(NZ ||' Set XB ' || xb);
--            dbms_output.put_line('bear00 ' || ROUND(bearing00,5) || 'mean ' || mean_zone(L) || ' in ' || in_zone(L));
         Else
--            dbms_output.put_line(J ||'Xb is null');
            bearing00 := -1.;
         End if;

         If J+In_Zone(L) < In_End and NZ > 2 then
           post := GZ_UTIL_ZONE.search_hilo(J+In_Zone(L), Start_Zone,NZ);
--           bearing33 := MOD(Mean_Zone(post)/ In_Zone(post) +360.,360.);
-- we want the start of the 2nd zone after the current
           post := Start_zone(post) + In_Zone(post);
--           dbms_output.put_line('post is ' || post);
           if post > In_end then
              pos3 := Xes.count;
           else
           pos3 := B_to_V(post);
           end if;
           x3 := Xes(pos3);
           y3 := Yes(pos3);
--           dbms_output.put_line('x3 is ' || round(x3,6) || ' y3 ' || round(y3,6) ||' post ' || post || 'pos3 ' || pos3);
--           dbms_output.put_line('bear3 ' || ROUND(bearing33,5) || ' post ' || post || ' J ' || J || 'start ' || start_zone(post));
         elsif closed = TRUE and NZ > 2 then
--           bearing33 := MOD(Mean_Zone(1)/In_Zone(1) +360.,360.);
-- we want the start of the 2nd zone after the start
           if J+In_Zone(L) > In_End  then
              post := GZ_UTIL_ZONE.search_hilo(1, Start_Zone,NZ,0);
--              dbms_output.put_line('post is ' || post);
           else
             post := GZ_UTIL_ZONE.search_hilo(J+1, Start_Zone,NZ,0);
--             dbms_output.put_line('ppost is ' || post);
             post := GZ_UTIL_ZONE.search_hilo(Start_Zone(post), Start_Zone,NZ,0);
--             dbms_output.put_line('Post ' || post);
           end if;
           post := Start_zone(post);
           pos3 := B_to_V(post);
           x3 := Xes(pos3);
           y3 := Yes(pos3);
--           dbms_output.put_line('X3 is ' || round(x3,6) || ' y3 ' || round(y3,6) ||' post  ' || post || 'pos3 ' || pos3);
--           dbms_output.put_line('X3 is ' || x3);
--           dbms_output.put_line('bear33 ' || ROUND(bearing33,5));
         else
           x3 := NULL;
           bearing33 := -1.;
--           dbms_output.put_line('X3 is null');
         end if;

--        dbms_output.put_line('j1 is ' || j1 || ' k1 ' || k1 || ' len ' || (k1-j1+1) || ' inx ' || inx.count);
-- First put all values in zone L in zone L+1
        For i in J1..K1 LOOP
           UZ := UZ + Inx(i);
           LZ := LZ - Inx(i);

-- P the variable part = UZ + LZ + CON
-- So there are I-J+1 elements in the top part and K-I in the bottom:
-- Say 1.. 4  5..   10
--     J   I  M      K

             P := ((UZ*UZ) / (I-J+1)) + ((LZ*LZ)/ (K-I)) + CON;

             If P > Q then
-- Variances: B (between) and W (Within)
                B := (P-Q)/NZ;
                W := (S-P)/(N-NZ+1);

-- Zonation constant varies from -1 to 1 with this definition

                  R := (B-W)/(B);
--
-- If this Zonation constant exceeds a Previous zonation attempt then
--if nZ > 3 then
--                  dbms_output.put_line('I ' || I || ' R ' || round(R,6) || ' PrevR ' || ROUND(PrevR,6) || ' inc ' || round(included_angle,3) || ' B ' || round(b,2) || ' W ' || round(w,2));
--                end if;
                IF R > PrevR  THEN

-- New angle checking and length checking code
--                bearing1 := MOD(UZ/(I-J+1),360.);
--                bearing2 := MOD(Inx(i+1)+360.,360.); --MOD(LZ/ (K-I),360.);

-- We want the bearing relative to the next vertex at bearing value for element M
-- We cannot measure included angles until we at least have a triangle

                pos1 := B_to_V(I+1);
                x1 := Xes(pos1);
                y1 := Yes(pos1);
                IF NZ > 2 THEN
                bearing1 := GZ_UTIL_ZONE.Geo_Bearing(x0,y0,x1,y1);
                bearing2 := GZ_UTIL_ZONE.Geo_Bearing(x1,y1,x2,y2);

                if bearing1 = bearing2 then
                    included_angle := 0.0;
                else
                  included_angle := abs(mod(bearing1+180.,360.) - bearing2);
                  if included_angle > 180.0 then
                    included_angle := 360. - included_angle;
                  end if;
                end if;
                END IF;

                IF  included_angle > 60. or NZ <= Nzone_q1 THEN
--                pos1 := B_to_V(I)+1; --GZ_UTIL_ZONE.search_greater(I,vertex,vertex.count);
--                if pos1 > Xes.count then
--                  pos1 := Xes.count;
--                end if;
                dx  := Xes(pos)-Xes(pos1);
                dy  := Yes(pos)-Yes(pos1);
                cosy2 := cosy - siny * dy*deg2rad;
                if cosy2 > 1. or cosy2 < 0. then
                   cosy2 := cosy;
                end if;
                distance1 := sqrt(cosy*cosy2 * dx*dx + dy*dy*yfactor) * dist_factor;

                dx  := Xes(pos1)-Xes(pos2);
                dy  := Yes(pos1)-Yes(pos2);
                cosy2 := cosy - siny * dy*deg2rad;
                if cosy2 > 1. or cosy2 < 0. then
                   cosy2 := cosy;
                end if;
                distance2 := sqrt(cosy*cosy2 * dx*dx + dy*dy*yfactor) * dist_factor;

                IF (distance1 >= minimum_dist and distance2 >= minimum_dist) or NZ <= Nzone_q1  THEN

                if bearing00 >= 0. then
                   bearing0 := bearing00;
                Else
                   bearing0 := bearing1;
                End if;
                if xb is NOT NULL and (xb <> x0 or yb <> y0) then
                  bearing0 := GZ_UTIL_ZONE.Geo_Bearing(xb,yb,x0,y0);
--                 if bearing0 is NULL then
--                    dbms_output.put_line('xb ' ||ROUND(xb,8) || ' yb ' || round(yb,8));
--                    dbms_output.put_line('x0 ' ||ROUND(x0,8) || ' y0 ' || round(y0,8));
--                 end if;
                  pre_angle := abs(mod(bearing0+180.,360.) - bearing1);
                Else
                  pre_angle := 180.;
                End If;
--                if nz = 5 then
--                dbms_output.put_line('pre ' ||ROUND(bearing0,5) || ' bear1 ' || ROUND(bearing1,5));
--                end if;
                if pre_angle > 180.0 then
                   pre_angle := 360. - pre_angle;
                end if;


                If bearing33 >= 0.0 then
                    bearing3 := bearing33;
                Else
                    bearing3 := bearing2;
                 End if;

                If x3 is NOT NULL and (x3 <> x2 or y3 <> y2) then
                 bearing3 := GZ_UTIL_ZONE.Geo_Bearing(x2,y2,x3,y3);
--                dbms_output.put_line(' B3 ' || bearing3 || ' b2 ' || bearing2);
--                dbms_output.put_line(' x2 ' || x2 || ' y2 ' || y2);
--                dbms_output.put_line(' x3 ' || x3 || ' y3 ' || y3);
                 post_angle := abs(mod(bearing2+180.,360.) - bearing3);
                if post_angle > 180.0 then
                  post_angle := 360. - post_angle;
                end if;
                else
                   post_angle := 180.;
                end if;
--dbms_output.put_line('distance 1 : ' || distance1 || ' d2 ' || distance2);
--                  if nz = 4 and i >= 150 and i <= 237 then
--dbms_output.put_line(' at i ' || i || 'inc ' ||ROUND(included_angle,5) || ' pre ' ||ROUND(pre_angle,5) || ' post ' ||ROUND(post_angle,5) || 'i-j ' || (I-j+1) || ' longer ' || longer || ' min ' ||Minimum_in_zone || ' K-i ' || (K-I) );
--                  end if;
--                If (abs(Inx(I)-Inx(I+1)) >= difference) and
--                  If  ((NZ <=2 and included_angle > 25. and distance1 > minimum_dist and distance2 > big_dist) or(NZ <=2 and included_angle > 25. and distance1 > big_dist and distance2 > minimum_dist) or
--  if nz >2 and i >=500 and i < 510  then
--                  dbms_output.put_line('NOT good at I ' || i || ' Bov ' || B_to_V(i) || ' inc ' ||ROUND(included_angle,5) || ' pre ' ||ROUND(pre_angle,5) || ' post ' ||ROUND(post_angle,5) || 'i-j ' || (I-j+1) || ' K-i ' || (K-I) || ' nz ' || nz || ' ' || nzone_q1);
--                  end if;
                If  ((NZ <= Nzone_q1 and included_angle > 35.) or
                     (included_angle > 85. and pre_angle > 75.0 and post_angle > 75.0 and (I-J+1) >= Minimum_in_zone and (K-I) >= Minimum_in_zone) or
                     (included_angle> 50. and pre_angle > 75.0 and post_angle > 50.0 and (I-J+1) >= longer and (K-I) > Minimum_in_zone ) or
                     (included_angle> 50. and pre_angle > 50.0 and post_angle > 75.0 and (K-I) >= longer and (I-J+1) > Minimum_in_zone) or
                     (NZ < Nzone_q3 and included_angle > 50. and pre_angle > 50.0 and post_angle > 50.0 and (I-J+1) >= longer and (K-I) > Minimum_in_zone) or
                     (NZ < Nzone_q3 and included_angle > 50. and pre_angle > 50.0 and post_angle > 50.0 and (K-I) >= longer and (I-J+1) > Minimum_in_zone)) Then

                  if NZ > 3 and( pre_angle <= 30.0 or post_angle <= 30.0) then
                      NULL;
--                      dbms_output.put_line('Ignored at I ' || i  || ' R ' ||round(R,5) || ' pre ' || round(pre_angle,2) || ' post ' || round(post_angle,2));
                  else
--                 if nz <= 4 then
--                  dbms_output.put_line('good at I ' || i || ' Bov ' || B_to_V(i) || ' inc ' ||ROUND(included_angle,5) || ' pre ' ||ROUND(pre_angle,5) || ' post ' ||ROUND(post_angle,5) || 'i-j ' || (I-j+1) || ' K-i ' || (K-I) || ' nz ' || nz || ' ' || nzone_q1);
--                  end if;
                    d1 := distance1;
                    d2 := distance2;
--                  end if;
                  PrevR := R;
                  PrevLZ := LZ;
                  LastI := I;
                  LastL := L;
                  LL := NZ +1;
                  end if;
                End If;
                END IF;
                END IF;
                END IF;
             End If;

        End Loop;
      End If;

    End Loop;

-- Check the included angle that we are creating by breaking a zone into
-- two pieces. Also check the pre and post included angles which will change

--                +--------------------------+   before

--                +                          +   after
--                  -    angle          -
--                      -        -
--                         +
--


    If LL >= NZ and PrevR > negnearly1 THEN

--      dbms_output.put_line('zone  ' || LL || ' angle ' || ROUND(included_angle,5) ||' bear1 ' || ROUND(bearing1,5) || ' bear2 ' || ROUND(bearing2,5));
--      dbms_output.put_line('zone  ' || LL || ' pre ' || ROUND(pre_angle,5) ||' bear0 ' || ROUND(bearing0,5) || ' bear1 ' || ROUND(bearing1,5));
--      dbms_output.put_line('zone  ' || LL || ' post ' || ROUND(postincluded_angle,5) ||' bear2 ' || ROUND(bearing2,5) || ' bear3 ' || ROUND(bearing3,5));
-- check first the angles and also the continuity of the bearings
--      If (nz/Nzone > 1.0 and (included_angle < 45.0 or pre_angle < 45. or postincluded_angle < 45.))  then
--      If (nz/Nzone > 0.85 and (included_angle < 40.0 or pre_angle < 40. or postincluded_angle < 40.)) or (Inx(LastI+1) = Inx(LastI+TRUNC(Minimum_in_zone/2)) and abs(Inx(LastI)-Inx(LastI+1)) < 1.) then

--   Angles bad or bad zonation pick and an abrupt corner is being cut off
--   by variance change
--   dbms_output.put_line('rejected at ' || LastL);
--         If LL > NZ then
--           LL := LL-1;
--         End If;
--      else
-- Its ok to make a new zone. LL is the 2nd (NEW) part of zone L
        L := LastL;
        J := Start_Zone(L);
        M := In_Zone(L);
        Start_Zone(LL) := LastI +1;
        In_Zone(L) := LastI - J +1;
        In_Zone(LL) := M - (LastI - J + 1);
        Zone_constant(LL) := prevR;
        If LL > 1 then
          deltaZ := prevR - Zone_constant(LL-1);
        End if;

        Mean_Zone(L) := Mean_Zone(L) - PrevLZ;
        Mean_Zone(LL) := PrevLZ;

        Order_Zone(LL) := LL;
--        IF LL = 2 Then
--        dbms_output.put_line('zone ' || L || ' Start ' || Start_Zone(L) || ' In_Zone ' || In_Zone(L) );
--      dbms_output.put_line('                 order ' || Order_Zone(L) || ' mean ' || (Round(mean_Zone(L)/In_Zone(L),2)) || ' Zone constant ' || Round(Zone_constant(L),6));
--        END IF;
--           dbms_output.put_line('Zone ' || LL || ' (Start) ' || Start_Zone(LL) || ' In_Zone ' || In_Zone(LL) || ' deltaz ' || ROUND(deltaz,7)  || ' d1 ' || round(d1,1) || ' d2 ' || round(d2,1));
--       dbms_output.put_line('                 Order ' || Order_Zone(LL) || ' mean ' || (Round(mean_Zone(LL)/In_Zone(LL),2)) || ' Zone constant ' || ROUND(Zone_constant(LL),6));
--      End If;
    END If;
--    dbms_output.put_line('LL ' || LL || ' PrevR' ||PrevR);
--        Exit when abs(deltaz) < 0.00005;
--     Exit when deltaz > 0.0 and deltaz < 0.00001;
--     Exit when PrevR > 0.99999;   -- Perfect zonation
--     if (PrevR > nearly1 and PrevR < about1) or
--               (Prevr > negabout1 and Prevr < negnearly1) then
-- dbms_output.put_line('exiting ' || PrevR || ' neg ' || negabout1 || ' ' || negnearly1);
--     End if;

--    Exit when (PrevR > nearly1 and PrevR < about1) or
--               (Prevr > negabout1 and Prevr < negnearly1);   -- Perfect zonation is a 1
--    dbms_output.put_line('LL ' || LL || ' Nzone ' || Nzone || ' Prevr ' || PrevR);

    Exit when LL >= nzone or PrevR = negnearly1;   -- Found nzone zones
  END LOOP;
--dbms_output.put_line('LL ' || LL || ' prevR ' || PrevR);
  NZ := LL;
-- Work out the mean of the zone
  For ii in 1..NZ Loop
    If In_Zone(ii) <> 0 then
      Mean_zone(ii) := Mod(Mean_zone(ii)/ In_zone(ii),360.);
    End If;
  End Loop;

  RETURN NZ;

END ZONE;
--

FUNCTION New_Fix_Geometry(Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,geom_error IN OUT NOCOPY VARCHAR2,XYs IN MDSYS.SDO_Ordinate_Array,closed BOOLEAN default TRUE) RETURN NUMBER AS

-- Try and fix some of the Validation errors for an edge.
-- We check the 13349 message to look for the intersecting segments and usually
-- just remove a vertex. However, for small polygons this may not work and it is
-- easier to just add an extra vertex. This is particularly true when segment 1
-- cuts across the loop and intersects the last segment.


  valid_geometry MDSYS.SDO_GEOMETRY;
  GenXys    MDSYS.SDO_ORDINATE_ARRAY := Geom.sdo_ordinates;
  PolyXys        MDSYS.SDO_ORDINATE_ARRAY := GenXys;
  self_segs      MDSYS.SDO_LIST_TYPE;
  edge1          PLS_INTEGER := 1;
  edge2          PLS_INTEGER := 1;
  vertex         PLS_INTEGER :=0;
  m              PLS_INTEGER;
  len            PLS_INTEGER := length(geom_error);
  jj             PLS_INTEGER;
  last           PLS_INTEGER;
  lastm          PLS_INTEGER;
  range         PLS_INTEGER;
  seg            PLS_INTEGER;
  seg1           PLS_INTEGER;
  seg2           PLS_INTEGER;
  seg11          PLS_INTEGER;
  seg22          PLS_INTEGER;
  seg3           PLS_INTEGER;
  seg2_end       PLS_INTEGER;
  pos            PLS_INTEGER;
  pos2           PLS_INTEGER;
  area           NUMBER;
  xc             NUMBER;
  yc             NUMBER;
  x              NUMBER;
  y              NUMBER;

  geom_is_valid  VARCHAR2(4000);
  
  function find_matching_xy(x NUMBER,y NUMBER, XYOrd IN  MDSYS.SDO_ORDINATE_ARRAY,epsilon NUMBER default 1.E-6) RETURN PLS_INTEGER AS 
/*
  --      INPUT
  --      (x,y)       - A pair of ordinates (point) to find
  --      XYOrd       - Array of ordinates to search
  --      epsilon     - a small difference in degrees to measure equality  
--                      between the point to find and the nearest ordinate.

-- Purpose: Searches for an (x,y) pair and returns matching vertex number
*/
  found        PLS_INTEGER := 0;
  i            PLS_INTEGER := -1;
  
  Begin


  For jj in 1..TRUNC(XYOrd.count/2) loop
      i := i + 2;
      if ABS(x - XYord(i)) <= epsilon  and ABS(y - XYOrd(i+1)) <= epsilon then
        found := TRUNC((i+1)/2);   -- return the segment number.
        exit;
      end if;
  End Loop;
      Return found; 
  End; 

  BEGIN

--  Extract the 2 segment numbers from the error message

  if SUBSTR(geom_error,1,5) = '13349' then
  for ii in 3..7 loop

    if SUBSTR(geom_error,len-ii,1) = '<' then
      vertex := SUBSTR(geom_error,len-ii+1,ii-2);
      for jj in ii+1..20 loop

        if SUBSTR(geom_error,len-jj,1) = '>' then
            last := jj+1;
        end if;
        if SUBSTR(geom_error,len-jj,1) = '<' then
          edge2 := SUBSTR(geom_error,len-jj+1,jj-last);
          exit;
        end if;
      end loop;
      exit;
    end if;
  end loop;
  
  -- Make vertex the greater one when one of the segments is 1
  if vertex = 1 then
     vertex := edge2;
     edge2 := 1;
  end if;
--  dbms_output.put_line('V ' ||vertex || ' ' ||PolyXys.count || ' edge2 ' || edge2);

  if vertex = 0 then
     RETURN 0;
  end if;
  
   
-- If we have extracted two segment numbers from the error message

  IF closed = FALSE THEN   -- Try adding a vertex to the 2nd arm,edge2
    if vertex > edge2 then
      seg := vertex;
      vertex := edge2;
      edge2 := seg;
     end if;    
--     dbms_output.put_line('vertex ' || vertex || ' edge2 ' || edge2);
     
     pos := edge2*2-1;
--     for ii in pos-6.. pos+3 loop
--       dbms_output.put_line('xy bad? ' || round(Genxys(ii),8));
--     end loop;
 
    
     seg1 := find_matching_xy(GenXys(pos),GenXys(pos+1),Xys);
     pos := edge2*2+1;
--     dbms_output.put_line('pos ' || (edge2*2-1) || ' pos 2nd ' || pos);
     seg2 := find_matching_xy(GenXys(pos),GenXys(pos+1),Xys);
 
 -- First try adding one vertex, then two if necessary
 
     For loops in 1..1 Loop
     GenXys.extend(2);
     for ii in Reverse pos+2..GenXys.count loop
       GenXys(ii) := GenXys(ii-2);
      end loop;
     

--      dbms_output.put_line('>>seg1 ' || seg1 || ' seg2 ' || seg2);
--      dbms_output.put_line('gen ' || round(genxys(1),7) || ' , ' || round(genxys(2),7));
     If loops = 1 then 
      range := TRUNC((seg2-seg1+2)/4);
      seg := TRUNC((seg1+seg2)/2)-range;
      seg2_end := seg2-range;
     Else
      range := TRUNC((seg2-seg1+3)/6);
      seg := TRUNC((seg1+seg2)/3)-range;
      seg2_end := seg2-range;
      seg3 := TRUNC((seg1+seg2)/2)+range;
     End if;
--     dbms_output.put_line('>>seg ' ||seg || ' pos ' || pos || ' seg2 end ' || seg2_end);
     WHILE seg < seg2_end LOOP 
     seg := seg + 1;
     x := Xys(seg*2-1);
     y := Xys(seg*2);
     GenXys(pos) := x;
     GenXys(pos+1) := y;
     If loops = 2 then
       seg3 := seg3+1;
       x := Xys(seg3*2-1);
       y := Xys(seg3*2);
       GenXys(pos+2) := x;
       GenXys(pos+3) := y;     
     End if;
--     dbms_output.put_line('Gen ' || round(genxys(1),7) || ' , ' || round(genxys(2),7));
--     dbms_output.put_line('seg ' || seg || ' pos ' || pos || ' x ' || x || ' y ' || y);
     valid_geometry := MDSYS.SDO_GEOMETRY(2002,8265.,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),GenXys);
      
     self_segs := GZ_SUPER.check_for_self_intersect(valid_geometry);
--     dbms_output.put_line('SELF ' || self_segs(1));
     if self_segs.count=0 then
       geom_is_valid := 'TRUE';
--       dbms_output.put_line('Success');
     else
        seg22 := MOD(self_segs(1),100000.);
        seg11 := TRUNC((self_segs(1)-seg22)/100000.);
        geom_is_valid := '13349 [Element <1>] [Ring <1>][Edge <'||seg11||'>][Edge <'||seg22||'>]';
--        dbms_output.put_line(geom_is_valid);
     end if;
    If  substr(geom_is_valid,1,4) = 'TRUE' then
      Geom.sdo_ordinates := GenXys;
      exit;
    end if;
    END LOOP;
    End Loop;
    geom_error := geom_is_valid;
    
    If  substr(geom_is_valid,1,4) = 'TRUE' then
    Return 0;   -- no vertex dropped
    End if;
--    dbms_output.put_line('POS ' || pos);
    for ii in pos+2..GenXys.count loop
       GenXys(ii-2) := GenXys(ii);
    end loop;
    GenXys.trim(2);
  END IF;
  
  -- New approach: Try adding a vertex first when edge2 = 1
--  dbms_output.put_line('here'||edge2);
  IF vertex =1 THEN
     seg2 := find_matching_xy(PolyXys(3),PolyXys(4),Xys);
     PolyXys.extend(2);
     for ii in Reverse 3..PolyXys.count loop
       PolyXys(ii) := PolyXys(ii-2);
      end loop;
     seg := TRUNC(seg2/2)-1;
     WHILE seg < seg2-1 LOOP
       seg := seg+1;
       x := Xys(seg*2-1);
       y := Xys(seg*2);
       PolyXys(3) := x;
       PolyXys(4) := y;
       
       valid_geometry := MDSYS.SDO_GEOMETRY(2003,Geom.sdo_srid,NULL,SDO_ELEM_INFO_ARRAY(1,1003,1),PolyXys);
       geom_is_valid := SDO_GEOM.Validate_Geometry_with_context(valid_Geometry,0.05);
--      dbms_output.put_line('geom is valid ' || geom_is_valid);
       If  substr(geom_is_valid,1,4) = 'TRUE' then
         Geom.sdo_ordinates := PolyXys;
         geom_error := geom_is_valid;
         Return 0;
       end if;
     END LOOP;
     
     for ii in 5..PolyXys.count loop
       PolyXys(ii-2) := PolyXys(ii);
    end loop;
    PolyXys.trim(2);
     
  END IF;
-- When its the 2nd to last edge intersecting edge 1 then
--dbms_output.put_line('vertex ' || vertex || ' edge2 ' || edge2 );
--dbms_output.put_line('count for poly was ' || PolyXys.count);
    if vertex < edge2 or edge2 = 1 or (vertex-edge2) >= 2 then --(edge2 = 1 and vertex*2 = PolyXys.count-4)
       if edge2 <> 1 and vertex*2 + 2 < polyxys.count then
         m := vertex*2+1;
       else
         m := vertex*2-1;
       end if;
--      dbms_output.put_line('M ' || m || ' v *2 ' || (vertex*2) || ' e ' || (edge2*2));
      for ii in m..PolyXys.count-2 loop
       PolyXys(ii) := PolyXys(ii+2);
      end loop;
    else
        m := edge2*2-1;
      for ii in m..PolyXys.count-2 loop
       PolyXys(ii) := PolyXys(ii+2);
      end loop;
    end if;
    PolyXys.trim(2);

    area := GZ_UTIL_ZONE.Centroid(PolyXys,xc,yc);
--    dbms_output.put_line('area ' || round(area,6));
    if area < 0. then
      PolyXys := GenXys;

       if m = vertex*2 -1 and edge2 <> 1 then
      m := edge2*2-1;
      elsif m = vertex*2 +1 then
        m := vertex*2 -1;
      else
        m := vertex*2+1;
      end if;
  
      for ii in m..PolyXys.count-2 loop
       PolyXys(ii) := PolyXys(ii+2);
      end loop;
      PolyXys.trim(2);
    end if;
    lastm := m;
    valid_geometry := MDSYS.SDO_GEOMETRY(2003,Geom.sdo_srid,NULL,SDO_ELEM_INFO_ARRAY(1,1003,1),PolyXys);
    geom_is_valid := SDO_GEOM.Validate_Geometry_with_context(valid_Geometry,0.05);
--  dbms_output.put_line('GEom is valid ' || geom_is_valid);
    if substr(geom_is_valid,1,4) <> 'TRUE' then
           PolyXys := GenXys;

       if (lastm = vertex*2 +1 or lastm = vertex*2-1) and edge2 <> 1 then
      m := edge2*2-1;
      else
        m := vertex*2+1;
      end if;

      for ii in m..PolyXys.count-2 loop
       PolyXys(ii) := PolyXys(ii+2);
      end loop;
      PolyXys.trim(2);
    end if;
--    dbms_output.put_line('set result to polyXys');
    Geom.sdo_ordinates := PolyXys;
    geom_error := geom_is_valid;

    RETURN vertex+1;
  end if;

  RETURN 0;

END New_FIX_GEOMETRY;
--
FUNCTION FIX_Geometry(PolyXys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,geom_error VARCHAR2) RETURN NUMBER AS

-- Try and fix some of the Validation errors
  origPolyXys    MDSYS.SDO_ORDINATE_ARRAY := PolyXys;
  edge1          PLS_INTEGER := 1;
  edge2          PLS_INTEGER := 1;
  vertex         PLS_INTEGER :=0;
  m              PLS_INTEGER;
  len            PLS_INTEGER := length(geom_error);
  jj             PLS_INTEGER;
  last           PLS_INTEGER;
  area           NUMBER;
  xc             NUMBER;
  yc             NUMBER;
  BEGIN

--  PolyXys := Geom.Sdo_ordinates;
--  dbms_output.put_line('sub' || SUBSTR(geom_error,1,5));
  if SUBSTR(geom_error,1,5) = '13349' then
  for ii in 3..7 loop
--    dbms_output.put_line('sub' || SUBSTR(geom_error,len-ii,1));
    if SUBSTR(geom_error,len-ii,1) = '<' then
      vertex := SUBSTR(geom_error,len-ii+1,ii-2);
      for jj in ii+1..20 loop
--    dbms_output.put_line('sub' || SUBSTR(geom_error,len-ii,1));
        if SUBSTR(geom_error,len-jj,1) = '>' then
            last := jj+1;
        end if;
        if SUBSTR(geom_error,len-jj,1) = '<' then
          edge2 := SUBSTR(geom_error,len-jj+1,jj-last);
          exit;
        end if;
      end loop;
      exit;
    end if;
  end loop;
  if vertex = 1 then
     vertex := edge2;
     edge2 := 1;
  end if;
--  dbms_output.put_line('V ' ||vertex || ' ' ||PolyXys.count || ' edge2 ' || edge2);
--  for ii in 1..TRUNC(PolyXys.count/2) loop
--       jj := ii*2-1;
--       dbms_output.put_line('x ' ||ROUND(PolyXys(jj),9) ||' y' || ROUND(PolyXys(jj+1),9));
--  end loop;

  if vertex <> 0 then
-- When its the 2nd to last edge intersecting edge 1 then
dbms_output.put_line('v*2 ' || (vertex*2) || ' edge2 ' || edge2 || ' ' ||(polyxys.count-4));
--dbms_output.put_line('count for poly was ' || PolyXys.count);
    if vertex < edge2 or edge2 = 1 or (vertex-edge2) > 2 then --(edge2 = 1 and vertex*2 = PolyXys.count-4)
       if edge2 <> 1 and vertex*2 + 2 < polyxys.count then
         m := vertex*2+1;
       else
         m := vertex*2-1;
       end if;
      dbms_output.put_line('M ' || m || ' v *2 ' || (vertex*2) || ' e ' || (edge2*2));
      for ii in m..PolyXys.count-2 loop
       PolyXys(ii) := PolyXys(ii+2);
      end loop;
    else
        m := edge2*2-1;
       dbms_output.put_line('m ' || m);
      for ii in m..PolyXys.count-2 loop
       PolyXys(ii) := PolyXys(ii+2);
      end loop;
    end if;
    PolyXys.trim(2);
--    dbms_output.put_line('count for poly now ' || PolyXys.count);
    area := GZ_UTIL_ZONE.Centroid(PolyXys,xc,yc);
--    dbms_output.put_line('area ' || round(area,6));
    if area < 0. then
      PolyXys := OrigPolyXys;

       if m = vertex*2 -1 then
      m := edge2*2-1;
      elsif m = vertex*2 +1 then
        m := vertex*2 -1;
      else
        m := vertex*2+1;
      end if;
       dbms_output.put_line('mm ' || m);
      for ii in m..PolyXys.count-2 loop
       PolyXys(ii) := PolyXys(ii+2);
      end loop;
      PolyXys.trim(2);
    end if;
--    Geom.sdo_ordinates := PolyXys;
    RETURN vertex+1;
  end if;
--  elsif SUBSTR(geom_error,1,5) = '13343' then
--    dbms_output.put_line('xy ' || polyXys(1) || ' ' || polyXys(2));
--    dbms_output.put_line('xy ' || polyXys(polyxys.count-1) || ' ' || polyXys(polyXys.count));
  end if;
  RETURN 0;

END FIX_GEOMETRY;
--
FUNCTION Coarse_Sample(PolyGeom IN MDSYS.SDO_GEOMETRY,sample_dist NUMBER) RETURN MDSYS.SDO_GEOMETRY AS

/*
********************************************************************************
--Program Name: Coarse_Sample
--Author: Sidey Timmins
--Creation Date: 10/20/2008

--Usage:  --
  --   REQUIRED Parameters:
  --      INPUT
  --      PolyXys   - A polygons xy coordinates
  --      PolyInfo  - Info Array for the polygon
  --      sample_distance - How often to sample in meters.
  --      OUTPUT
  --      CoarseInfo: Info Array just for the outer rings of the new generalized
  --                  polygon.
-- Purpose:  Produces a generalized version of the input polygon which can
--           be used to calculate nearest distances more expediently.
--           Coarsely sample the state boundary every sample distance or
--           every sqrt(number of vertices)/2 or at corners.
--
-- Reference: Original idea using my distance function which enable rapid
-- measurement of distances between points in geodetic coordinates.
********************************************************************************
*/
    PolyXys            MDSYS.SDO_ORDINATE_ARRAY := PolyGeom.SDO_ORDINATES;
    PolyInfo           MDSYS.SDO_ELEM_INFO_Array:= PolyGeom.Sdo_Elem_Info;
    Coarse_Info        MDSYS.SDO_ELEM_INFO_Array:= MDSYS.SDO_ELEM_INFO_Array();
    Coarse_Xys         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Coarse_Geom        MDSYS.SDO_GEOMETRY;
    x                  NUMBER;
    y                  NUMBER;
    xtest              NUMBER;
    ytest              NUMBER;
    xlast              NUMBER;
    ylast              NUMBER;
    xbehind            NUMBER;
    ybehind            NUMBER;
    dist               NUMBER;
    included_angle     NUMBER;
    behind_dist        NUMBER;
    half_sample_dist   NUMBER := sample_dist*0.5;
    factor             NUMBER;
    b1                 NUMBER;
    b2                 NUMBER;

    nvert_outer        PLS_INTEGER;
    ring_count         PLS_INTEGER;
    ring_type          PLS_INTEGER;
    nsample            PLS_INTEGER;
    msample            PLS_INTEGER;
    outer_rings        PLS_INTEGER := 0;
    ring               PLS_INTEGER := 0;
    istart             PLS_INTEGER := 1;
    last               PLS_INTEGER := 1;
    next               PLS_INTEGER := 0;
    old_next           PLS_INTEGER;
    current            PLS_INTEGER := 0;
    last_coord         PLS_INTEGER;
    counter            PLS_INTEGER;
BEGIN

    Coarse_Xys.extend(PolyXys.count);
    ring_count := TRUNC(PolyInfo.count/3);

    FOR jj in 1..ring_count LOOP
       if PolyInfo(jj*3-1) = 2 then -- 1003 then
          outer_rings := outer_rings+1;
       end if;
    End loop;

    Coarse_Info.extend(outer_rings*3);

-- Calculate how often to sample. The primary sampling is by distance with
-- an extra check to ensure not too many coordinates are bypassed. Note how
-- the sqrt of the number of coordinates/2 samples polygons with fewer
-- verticess more often that the more usual sqrt of the number of vertices would.
--
--    # coordinates     # vertices   New #     Sqrt(2N)/2      Usual Sqrt(N)
--    2N                  N         vertices
--    20                  10           5           2                  3
--    200                100          14           7                 10
--    2000              1000          45          22                 32
--    20000            10000         142          70                100

    nsample := SQRT(PolyXys.count)/1.5;
    if nsample < 2 and PolyXys.count < 20 then
      nsample := 2;
    elsif nsample = 0 then
      nsample := 1;
    end if;
    if PolyXys.count/nsample < 4 then
      nsample := 1;
    end if;

    FOR jj in 1..ring_count LOOP
-- Only do outer rings
       ring_type :=PolyInfo((jj-1)*3+2);
       if ring_type = 2 then
         ring := ring + 1;
         Coarse_Info((ring-1)*3+1) := next+1;
         Coarse_Info((ring-1)*3+2) := 2;
         Coarse_Info((ring-1)*3+3) := 1;
       msample := nsample;
       If jj < ring_count then
         nvert_outer := TRUNC((PolyInfo(jj*3+1)-1)/2) - istart + 1;
       else
         nvert_outer := TRUNC(PolyXYs.count/2) - istart + 1;
       end if;
       if TRUNC(nvert_outer/msample) < 4 then
         msample := 1;
       end if;
--       dbms_output.put_line('nvert_outer ' || nvert_outer || ' istart ' || istart);
       old_next := next;
       counter := 0;
       factor := 1.;
-- For small polygons we may have too large a sampling distance. This loop
-- attempts to ensure we sample enough to get more than 3 vertices (a triangle).

       WHILE (next - old_next) < 6 and counter < 8 LOOP
         counter := counter + 1;
         next := old_next;
         if counter <> 1 then
           factor := factor * 1.414;
         end if;
         current := PolyInfo((jj-1)*3+1) -1;
         last_coord := current + nvert_outer*2;
         xtest := PolyXys(current+1);
         ytest := PolyXys(current+2);

         Coarse_Xys(next+1) := xtest;
         Coarse_Xys(next+2) := ytest;
         xlast := PolyXys(last_coord-3);
         ylast := PolyXys(last_coord-2);
         xbehind := xlast;
         ybehind := ylast;
         next := next+2;
         current := current+2;
         last := 1;
         For ii in 2..nvert_outer-1 Loop
            x := xtest;
            y := ytest;
            xtest := PolyXys(current+1);
            ytest := PolyXys(current+2);
            included_angle := GZ_UTIL_ZONE.angle(xlast,ylast,x,y,xtest,ytest,b1,b2);
            dist := GZ_UTIL_ZONE.accurate_gcd(x,y,xtest,ytest) * factor;
            behind_dist := GZ_UTIL_ZONE.accurate_gcd(xbehind,ybehind,xtest,ytest) * factor;
--          if ii = 988 or ii = 989 or ii = 990 then
--             dbms_output.put_line('x ' || x || ' y ' || y);
--             dbms_output.put_line('xt ' || xtest || ' yt ' || ytest);
--             dbms_output.put_line('angle ' || round(included_angle, 4) || ' dist ' || ROUND(dist,4));
--          end if;
-- This is the sampling strategy which may be improved.
-- If the enhanced distance is greater than the sampling distance or
-- we have bypassed too many vertices (msample) or
-- included angle < 120 and either the distance or behind distance > half sample distance
-- THEN we sample the line.

            if dist >= sample_dist or (ii-last+1) >= msample or
             ((dist > half_sample_dist or behind_dist > half_sample_dist) and included_angle < 120.) then
               if ((dist > half_sample_dist  ) and included_angle < 120.) and last <> (ii-1) then
                 Coarse_Xys(next+1) := x;
                 Coarse_Xys(next+2) := y;
                 next := next + 2;
               end if;
               Coarse_Xys(next+1) := xtest;
               Coarse_Xys(next+2) := ytest;
               xbehind := xtest;
               ybehind := ytest;
               last := ii;
               next := next + 2;
            end if;
            current := current + 2;
--          dbms_output.put_line('ii ' || ii || ' cur ' || current);
            xlast := x;
            ylast := y;
         End Loop;
--       dbms_output.put_line('ring count ' || ring_count || 'old_next ' || old_next || ' next ' || (next+2-old_next) || ' ' || Coarse_Xys.count || ' cur ' || current || ' ' || PolyXys.count);
       END LOOP;
-- Always end the ring

       Coarse_Xys(next+1) := PolyXys(current+1);
       Coarse_Xys(next+2) := PolyXys(current+2);
       next := next+2;
       end if;
       if jj <> ring_count then
         istart := TRUNC((PolyInfo(jj*3+1)+1)/2);
       end if;
    END LOOP;
    Coarse_Xys.trim(Coarse_Xys.count-next);
   if Coarse_Info.count =3 then
     Coarse_Geom := MDSYS.SDO_GEOMETRY(2002,8265,NULL,Coarse_Info,Coarse_Xys);
   else
     Coarse_Geom := MDSYS.SDO_GEOMETRY(2007,8265,NULL,Coarse_Info,Coarse_Xys);
   end if;
    RETURN Coarse_Geom;

END Coarse_Sample;
--
FUNCTION GEO_BEARING(px0 NUMBER,py0 NUMBER,px1 NUMBER,py1 NUMBER)
                RETURN NUMBER DETERMINISTIC IS
/*
**************************************************************************************
--Program Name: Geo_Bearing
--Author: Sidey Timmins
--Creation Date: 03/19/2009
--Updated:  04/07/2010 To use same method as chainer with Great circle bearings
--Usage:
  -- Call this function from inside another PL/SQL program.
  -- There are 2 ordered pairs describing a vector:
  --
  --   REQUIRED Parameters:
  --           (px1,py1): coordinates in degrees of 1st vertex
  --           (px2,py2): coordinates in degrees of 2nd vertex
--
--                                 + (x2,y2)
--                               /
--                             /    Bearing is counter clockwise angle from East
--                    (x1,y1) +---------------+ X axis
--

--
--Purpose:  Approximates horizontal angles on the earth's surface between nearby
--          points with geodetic coordinates. It does not calculate spherical angles.
--
-- Method:  Calculates a bearing measured from the x-axis counterclockwise. 
--
-- Accuracy: Typically less than 1.e-3 degrees compared to the azimuth formula.
-- This is the initial bearing (sometimes referred to as forward azimuth) which
--if followed in a straight line along a great-circle arc will take you from the
--start point to the end point:
-- ¿ = atan2( sin(¿long).cos(lat2),cos(lat1).sin(lat2) ¿ sin(lat1).cos(lat2).cos(¿long) )

 # Reference: http://www.movable-type.co.uk/scripts/latlong.html
 #    Personal communication from Peter H. Dana
 #    http://www.colorado.edu/geography/gcraft/notes/mapproj/mapproj_f.html
 #
 # Dependencies:
 #    atan2,sincos
***************************************************************************************
*/
   deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
   rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;

--   a2          NUMBER   := 40680631590769.;               -- ellipsoid radius ^2
--   b2          NUMBER   := 40408299983328.76931725432420763255772;
   e2        CONSTANT NUMBER := 0.006694380022900787625359114703055206936; --1. - b2/a2;

   x0                    NUMBER := px0;
   y0                    NUMBER := py0;
   x1                    NUMBER := px1;
   y1                    NUMBER := py1;

   dx                    NUMBER;
   dy                    NUMBER;
   x1_0                  NUMBER;
   cosx1_0               NUMBER;
   sinx1_0               NUMBER;
   siny0                 NUMBER;
   siny1                 NUMBER;
   cosy1                 NUMBER;
   cosy0                 NUMBER;
   tany0                 NUMBER;
   Bearing               NUMBER;
   Bearingc              NUMBER;
   cotphi                NUMBER;
   Lambda12              NUMBER;
   tanpsi2               NUMBER;

 
BEGIN


-- Here we setup the change in x and y from the most recent vertex
     dx := x1 - x0;
     dy := y1 - y0;

     if dx = 0. and dy = 0. then
        RETURN NULL;
     end if;


    -- Convert to radians and then do a local secant projection

      y0 := y0* deg2rad;
      x1_0 := (x1-x0)* deg2rad;
      y1 := y1 * deg2rad;

-- Set up constants for the projection at sin_center_lat and cos_center_lat:
-- Note: since we just want the arctan of Y/X,  the multiplication by
--       ksp cancels. So we don't need to calculate g and ksp !
--==     Commented code has 2 dashes and 2 == below.

--        g = sin_center_lat * sin(lat) + cos_center_lat * cos(lat) * cos(dlon)
--==      g := sin(y0) * sin(y1) + cos(y0) * cos(y1) * cos(x1-x0);
--==      ksp := 2. / (1.0 + g);

-- Project X1, Y1
-- X = ksp * cos(lat) * sin(dlon)
-- Y = ksp * ( cos_center_lat * sin(lat) - sin_center_lat * cos(lat) * cos(dlon)

        siny0 := GZ_MATH.sincos(y0,cosy0);
        siny1 := GZ_MATH.sincos(y1,cosy1);
        sinx1_0 := GZ_MATH.sincos(x1_0,cosx1_0);

        dx := cosy1 * sinx1_0;
        dy := (cosy0 * siny1 - siny0 * cosy1 * cosx1_0);

-- Cunningham's formula (page 120 from G. Bomford)


       bearing := GZ_UTIL_ZONE.atan2(dy,dx) * rad2deg;
--       dbms_output.put_line('start bearing ' || bearing );
       if bearing < 0.0 then
         bearing := 360. + bearing;
       end if;
       if sinx1_0 = 0.0 or cosy1 = 0.0  then
          NULL;
--          dbms_output.put_line('= zero  ' || bearing);
       else
--         tany0 := tan(y0);
--         lambda12 := (1.- e2) * tan(y1)/tany0 + e2 * (cosy0/cosy1);
--         cotphi := (lambda12  - cosx1_0) * siny0/sinx1_0;
         tanpsi2 := (1.- e2) * tan(y1) + e2 * (siny0/cosy1);
         cotphi := (cosy0*tanpsi2  - siny0*cosx1_0)/sinx1_0;
--         dbms_output.put_line('tanpsi2 ' ||tanpsi2);
--         dbms_output.put_line('tanpsi2 ' ||((1.- e2) * tan(y1) + e2 * (sin(y0)/cos(y1))) );
--         dbms_output.put_line('cotphi ' || cotphi);
--         dbms_output.put_line('cotphi ' || ((cos(y0)*tanpsi2  - sin(y0)*cos(x1_0))/sin(x1_0)) );
         if cotphi <> 0.0 then
           bearingc := 90. -GZ_UTIL_ZONE.atan2(1./cotphi,1.) *rad2deg;
--           dbms_output.put_line('bc ' || bearingc );

--           dbms_output.put_line('bc ' ||(90.-atan2(1./cotphi,1.) *rad2deg));
--          dbms_output.put_line('bc ' || bearingc   || ' dy ' || round(dy,8) || ' dx ' || dx);
           if dy < 0. then --or (dy = 0. and dx < 0.0) then
             bearingc := bearingc +180.;
--             dbms_output.put_line('NOW bc ' || bearingc );
           end if;
            if abs(bearingc - bearing) > 1. then
                bearingc := bearingc - 180.;
--                dbms_output.put_line('NNNOW bc ' || bearingc );
            end if;
           if abs(bearingc - bearing) > 1. then
-- Very small dx offsets like this give positive cotphi and thus 0 degrees instead of 180. !!
--   x0 := -80.;
--   y0 := 40;
--   x1 := -80.000001; <<-- no good   --    x1 := -80.0000095 <-- this is ok
--   y1 := 40.;
               dbms_output.put_line('cotphi ' ||cotphi);
              dbms_output.put_line('bc ' || bearingc || ' b1 ' || bearing);
              dbms_output.put_line('x0 ' || px0 || ' py0 ' || py0);
              dbms_output.put_line('x1 ' || px1 || ' py1 ' || py1);
--              bearingc := bearing;
           end if;
           bearing := bearingc;
         else
-- Atan2 is very slow (remember 38 digits of precision) so use
-- a very accurate approximation and convert to degrees.
           bearing := GZ_UTIL_ZONE.atan2(dy,dx) * rad2deg;
         end if;
       end if;

      if bearing < 0. then
          bearing := 360. + bearing;
      end if;

      RETURN Bearing;

END GEO_BEARING;
--
FUNCTION ANGLE(x1 NUMBER,y1 NUMBER,x0 NUMBER, y0 NUMBER, x2 NUMBER, y2 NUMBER,bearing1 IN OUT NOCOPY NUMBER, bearing2 IN OUT NOCOPY NUMBER,SRID NUMBER default 8265.) 
RETURN NUMBER  Deterministic IS

-- Calculate the angle for 2 lines sharing a common vertex (x0,y0) using
-- geo bearings (initial azimuths of a great circle).
--
-- first pair of coordinates
--                      +  (x1,y1)
--                     /
--                    /   
--        (x0,y0)     +----------------+ (x2,y2)
--
--         apex of angle                  2nd leg

  rad2deg     CONSTANT NUMBER := 57.29577951308232087679815481410517033235;
  az1                NUMBER;
  az2                NUMBER;

  Included_angle     NUMBER;

BEGIN

  if Is_geodetic(SRID) then
    az1 := geo_bearing(x0,y0,x1,y1);
    az2 := geo_bearing(x0,y0,x2,y2);
  else
    az1 := GZ_MATH.new_arctan(y1-y0,x1-x0) * rad2deg;
    az2 := GZ_MATH.new_arctan(y2-y0,x2-x0) * rad2deg;  
  end if;
-- dbms_output.put_line('az1 ' || round(az1,9)  || ' az2 ' || round(az2,9));

  included_angle := az1-az2;
--     dbms_output.put_line('Aincluded ' || round(included_angle,9) );
  if included_angle < 0. then
    included_angle := az1-az2+360.;
--       dbms_output.put_line('included ' || round(included_angle,9) );
  end if;
  if included_angle > 180. then
     included_angle := 360. - included_angle;
  end if;
  bearing1 := az1;
  bearing2 := az2;
  
  RETURN  included_angle;
  
END ANGLE;
--
FUNCTION Is_Geodetic(SRID NUMBER) return BOOLEAN AS

-- Reports whether Geodetic coordinates (Degrees) are being used.

BEGIN
    if SRID = 8265.0 then Return TRUE; end if;
    Return FALSE;
END;
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
--              (3 times faster than the built in functions that take 5 seconds
--              for 200000 tan values) with about 14 digits of accuracy.
--
-- Accuracy: Over the range - two pi to two pi by 1/10000 the maximum errors are:
--
-- Reference: http://cache-www.intel.com/cd/00/00/29/37/293747_293747.pdf
--     ?originalurl=http://www.intel.com/cd/ids/developer/asmo-na/eng/293747.htm
--  The title of the pdf is:
-- "Slerping Clock Cycles" JMP van Waveren, Id Software, Inc
-- For cosine code
-- http://www.gansle.com/approx/approx.pdf
--            "A Guide to Approximations" by Jack G Ganssle

--Dependencies: None.   Also see c_test_tan for rigorous testing.
***************************************************************************************
*/

  X         NUMBER             := InX;

  twopi     CONSTANT NUMBER             := 6.2831853071795864769252867665590057684;
  pi        CONSTANT NUMBER             := 3.1415926535897932384626433832795028842;
  piByTwo   CONSTANT NUMBER             := 1.5707963267948966192313216916397514421;
  piBy4     CONSTANT NUMBER             := 0.78539816339744830961566084581987572105;
  fourbypi  CONSTANT NUMBER             := 1.27323954473516268615107010698011489627;
  pi3By2    CONSTANT NUMBER             := 4.7123889803846898576939650749192543263;
  recip_piby4 CONSTANT NUMBER           := 1.273239544735162686151070106980114896275;
  tanx      NUMBER;
  x2        NUMBER;
  octant    INTEGER;
  big       NUMBER := 1.E120;
  diff      NUMBER;

  -- these coefficients are optimized to give accurate results throughout
  -- the range [-2pi to 2pi]
    c1      CONSTANT NUMBER             := -34287.4662577359568109624;
    c2      CONSTANT NUMBER             := 2566.7175462315050423295;
    c3      CONSTANT NUMBER             := -26.5366371951731325438;
    c4      CONSTANT NUMBER             := -43656.1579281292375769579;
    c5      CONSTANT NUMBER             := 12244.4839556747426927793;
    c6      CONSTANT NUMBER             :=  -336.611376245464339493;


BEGIN

  if abs(x) >= twopi then          
   x := mod(x,twopi);
  end if;

  if x < 0.0 then
    x := x + twopi;
  end if;

  octant := TRUNC(x * recip_piby4); --trunc(x/piby4);

  if x > pi then
    diff := pibytwo - (x-pi); --mod(x,pi);
  else
    diff := pibytwo -x;
  end if;

  if abs(diff) < 1.E-20 then
  
-- Should return infinity but this will have to do..
     If diff >= 0.0 and octant <> 6 then
       RETURN BIG;
     Else
       RETURN -BIG;
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

END;
--
FUNCTION ROTATE_GEOM(geom IN MDSYS.SDO_GEOMETRY,angle NUMBER,px0 NUMBER default NULL,py0 NUMBER default NULL) RETURN MDSYS.SDO_GEOMETRY AS

-- Rotates a geometry  counter clockwise about the point (x0,y0) by the
-- angle specified in degrees.

    Xys             MDSYS.SDO_ORDINATE_ARRAY :=geom.sdo_ordinates;
    deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
    Rotated_Xys     MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    New_geom        MDSYS.SDO_GEOMETRY;
    n               PLS_INTEGER := TRUNC(Xys.count/2);
    jj              PLS_INTEGER;
    x0              NUMBER;
    y0              NUMBER;
    x               NUMBER;
    y               NUMBER;
    sin0            NUMBER := sin(angle*deg2rad);
    cos0            NUMBER := cos(angle*deg2rad);
BEGIN

  New_geom := geom;
  Rotated_Xys.extend(Xys.count);
  if px0 is NULL then
    x0 := Xys(1);
    y0 := Xys(2);  
  end if;
  
  For ii in 1..n Loop
    jj := ii*2;
-- Use Rotation matrix-- read theta for the zero below
--
--           [ cos0    -sin0 ]  [x]
--           |               |  | |
--           [ sin0     cos0 ]  [y]

    x := (Xys(jj-1)-x0);
    y := (Xys(jj)  -y0);
    Rotated_Xys(jj-1) := cos0*x - sin0*y + x0;
    Rotated_Xys(jj)   := sin0*x + cos0*y + y0;
  End Loop;

  New_geom.sdo_ordinates := Rotated_Xys;
  RETURN New_geom;

END ROTATE_GEOM;
--
--
function make_a_geom(closed varchar2 default 'N',reverse_it number default 0)  return mdsys.sdo_geometry as
x  number;
y  number;
xc number;
yc  number;
theta number;
no_to_do pls_integer := 100;
istart pls_integer :=2;
deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
Ord Mdsys.sdo_ordinate_array := Mdsys.sdo_ordinate_array();
Info  Mdsys.sdo_elem_info_array := Mdsys.sdo_elem_info_array(1,2,1);
keep_it mdsys.sdo_list_type := mdsys.sdo_list_type();
geom  Mdsys.sdo_geometry;
begin
--dbms_random.initialize(123459);   --123459
--    ord := Mdsys.sdo_ordinate_array(-100,40,-100.0000001,40,-100.00001,40,-100.00001,40.000000001,-100,40.01,-100,40);
    Ord.extend(no_to_do*2);
    keep_it.extend(no_to_do);
    keep_it(1) := 1.;
    keep_it(no_to_do) := 1.;
    ord(1) := -100.;
    ord(2) := 40.;
    xc := ord(1);
    yc := ord(2);
    theta := (360./no_to_do)*deg2rad;
    if closed = 'Y' then
      istart := 1;
    else
      x := xc;
      y := yc;
    end if;
    for ii in istart..no_to_do loop
        keep_it(ii) := 0.;
        if closed = 'N' then
        if mod(ii,43) = 0 then
        x := x ; -- - dbms_random.value(0,0.000004)*1.6;
        y := y ; -- - dbms_random.value(0,0.000001)*.3*ii;
        else
        x := x ; --+ dbms_random.value(0,0.000002);
        y := y ; --+ dbms_random.value(0,0.0000005);
        end if;
        else
          x := xc + cos(theta*(ii-1))*0.0005 ; --+dbms_random.value(0.,0.00002);
          y := yc + sin(theta*(ii-1))*0.0005 ; ---dbms_random.value(0.,0.00004);
        end if;
--        dbms_output.put_line('x ' || round(x,9) ||','||round(y,10));
        ord(ii*2-1) := round(x,12);
        ord(ii*2) := round(y,12);
    end loop;
    
    keep_it(50) := 1.;
    if closed = 'Y' then
       ord(no_to_do*2-1) := ord(1);
      ord(no_to_do*2) := ord(2);
    end if;
    ord.trim(140);
    if reverse_it <> 0 then
       reverse_ordinates(Ord);
    end if;
    geom := mdsys.sdo_geometry(2002,8265,null,Info,ord);
    return geom;
end;
--
function test_reverse_ordinates(ifrom pls_integer default 1,ito number default null,InXYord Mdsys.sdo_ordinate_array default NULL) RETURN Mdsys.sdo_ordinate_array AS

  XyOrd Mdsys.sdo_ordinate_array := Mdsys.sdo_ordinate_array(1,2,3,4,5,6,7,8,9,10);
  
begin
    If InXYOrd is not NULL then
      XYOrd := INXYOrd;
    end if;
    reverse_ordinates(XYOrd,ifrom,ito);
    for ii in 1..TRUNC(XYOrd.count/2) loop
    dbms_output.put_line('ii ' ||ii || '        X ' || Xyord(ii*2-1) || ' Y ' || XYord(ii*2));
    end loop;
     reverse_ordinates(XYOrd,3,8);
    for ii in 1..TRUNC(XYOrd.count/2) loop
    dbms_output.put_line('ii ' ||ii || '        X ' || Xyord(ii*2-1) || ' Y ' || XYord(ii*2));
    end loop;
    Return XYOrd;
end;
--
function test_remove_close_xys(Ingeom mdsys.sdo_geometry default NULL,ptolerance number default 0.05,closed varchar2 default 'N',reverse_it number default 0)  return mdsys.sdo_geometry as
x  number;
y  number;
Ord Mdsys.sdo_ordinate_array := Mdsys.sdo_ordinate_array(1,1,1.25,1,1.75,3,2.24,1,2.65,1,3.6,1);
Info  Mdsys.sdo_elem_info_array := Mdsys.sdo_elem_info_array(1,2,1);
geom  Mdsys.sdo_geometry := Ingeom;
keep_it mdsys.sdo_list_type := mdsys.sdo_list_type();
  status  number;
  tolerance number := ptolerance;
  n       pls_integer;

begin

--    ord := Mdsys.sdo_ordinate_array(-100,40,-100.0000001,40,-100.00001,40,-100.00001,40.000000001,-100,40.01,-100,40);
Ord := Mdsys.sdo_ordinate_array(0,1,1,1,1.25,1,1.75,1.5,2.25,1,2.65,1,3.6,1);
--    geom := make_a_geom(closed,reverse_it);
    geom := mdsys.sdo_geometry(2002,NULL,NULL,mdsys.sdo_elem_info_array(1,2,1),ord);
    n := TRUNC(geom.sdo_ordinates.count/2);
    keep_it.extend(n);
       for ii in 1..n loop
        keep_it(ii) := -2.;
       end loop;
    keep_it(1) := 1.;
    keep_it(n) := 1.;
    tolerance :=1.;
    dbms_output.put_line('Before ' || geom.sdo_ordinates.count);
    status := remove_close_xys(geom,keep_it,1.0/0.000508,0.0,tolerance);
    dbms_output.put_line('status ' || status);
    return geom;
end;
--
procedure test_accurate_length as

 deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
   x1  number;
   y1  number;
   x2  number;
   y2  number;
--   geometry    mdsys.sdo_geometry;

   d     number;
   d_K   number;
   s     number;
   az1   number;
   az2   number;
   m12   number;
   rms   number :=0.0;
   maxim number :=0.0;
   where_maxim VARCHAR2(20);
   ii    pls_integer;
   jj    pls_integer;
   loops pls_integer :=0;
   param number := 0.1251;
begin

 
   for j in 0..7 loop 
    jj := 10*j+3;
--    jj := 33.5;
   for i in -18..17 loop
      ii := i*10;
   
      x1 := 0.+param * cos(ii*deg2rad);
      y1 := jj+param * sin(ii*deg2rad);
      x2 := x1+param;
      y2 := jj+param; 

    d := accurate_gcd(x1,y1,x2,y2);
--    d := gz_qa.distance_fcn(x1,y1,x2,y2);
    
    s := GZ_geodesic.inverse(y1,x1,y2,x2,d_K,az1,az2,m12);
 
--    dbms_output.put_line('LAT ' || jj || ' AZ ' || II ||' Karney ' || ROUND(d_K,8) || ' d ' || round(d,8) || ' Diff ' || round(d_K-d,7));
    loops := loops + 1;
    rms := rms + (d_k-d)*(d_k-d);
    if abs(d_k-d) > maxim then
       maxim := abs(d_k-d);
       where_maxim := ' At Lat ' ||j || ' long ' || ii;
    end if;
   end loop;
   end loop;
   rms := sqrt(rms/loops);
   dbms_output.put_line('RMS ' || round(rms,10) || ' maxim ' || round(maxim,10)|| where_maxim);
END;
--
procedure test_triangle_area as
   x0  number := -100.;
   y0  number := 40;
   x1  number := -99.999;
   y1  number := 40.0005;
   x2  number := -99.999;
   y2  number := 40.001;
   geometry    mdsys.sdo_geometry;
   area number;
   oarea number;
   a     number;
   b     number;
   c     number;
   dx    number;
   dy    number;
   s     number;
   az1   number;
   az2   number;
   m12   number;
begin
  x0 := -117.271112000000002240085450466722249985;
  y0 := 34.02076300000000230738805839791893959045;
  x1 :=-117.270978999999996972292137797921895981;
  y1:= 34.02078399999999902547642705030739307404;
  x2 := -117.270581000000007065864338073879480362;
  y2:=    34.02084700000000339059624820947647094727;
  
  for ii in 1..10000 loop
   y0 := y0 + 1.E-20;
   area := triangle_area(x0,y0,x1,y1,x2,y2);
 -- oarea := sdo_geom.sdo_area(sdo_geometry(2003,8265,null,SDO_ELEM_INFO_ARRAY(1,1003,1),SDO_ORDINATE_ARRAY(x0,y0,x1,y1,x2,y2,x0,y0)),0.05,'unit=sq_meter');
  end loop;
  return;
  dx := x0-x1;
  dy := y0-y1;
  
  a := sqrt(dx*dx + dy*dy);
  dx := x2-x1;
  dy := y2-y1;
  b := sqrt(dx*dx + dy*dy);
  dx := x2-x0;
  dy := y2-y0;
  c := sqrt(dx*dx + dy*dy);
--  y2:=y0+0.00009;
--  y1:=y2;
  a := accurate_gcd(x0,y0,x1,y1);
  b := accurate_gcd(x2,y2,x1,y1);
  c := accurate_gcd(x0,y0,x2,y2);
   dbms_output.put_line('SIDEY  a ' || ROUND(a,10) || ' b ' || round(b,10) || 'c ' || round(c,10));
  s := GZ_geodesic.inverse(y0,x0,y1,x1,a,az1,az2,m12);
  s := GZ_geodesic.inverse(y2,x2,y1,x1,b,az1,az2,m12);
  s := GZ_geodesic.inverse(y2,x2,y0,x0,c,az2,az1,m12);
  dbms_output.put_line('KARNEY a ' || round(a,10) || ' b ' || round(b,10) || 'c ' || round(c,10));

  area := triangle_area(x0,y0,x1,y1,x2,y2);
  
  oarea := sdo_geom.sdo_area(sdo_geometry(2003,8265,null,SDO_ELEM_INFO_ARRAY(1,1003,1),SDO_ORDINATE_ARRAY(x0,y0,x1,y1,x2,y2,x0,y0)),0.05,'unit=sq_meter');
  dbms_output.put_line('area ' || round(area,6) || ' oarea ' || round(oarea,6));
  a := sdo_geom.sdo_length(sdo_geometry(2002,8265,null,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_ORDINATE_ARRAY(x0,y0,x1,y1)),0.05,'unit=meter');
  b := sdo_geom.sdo_length(sdo_geometry(2002,8265,null,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_ORDINATE_ARRAY(x1,y1,x2,y2)),0.05,'unit=meter');
  c := sdo_geom.sdo_length(sdo_geometry(2002,8265,null,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_ORDINATE_ARRAY(x0,y0,x2,y2)),0.05,'unit=meter');
  dbms_output.put_line('ORACLEa ' || round(a,10) || ' b ' || round(b,10) || 'c ' || round(c,10));

end;
procedure test_angle(in_angle number default 45.) as
deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
xys      mdsys.sdo_ordinate_array := mdsys.sdo_ordinate_array(0,0,0,0,0,0);
Rot_xys      mdsys.sdo_ordinate_array;
   x1 number := 1.;
   y1 number := -1.;
   x2 number := -120.;
   y2 number := 40.;
   x3 number := 0.5;
   y3 number := -0.7;
   dx number;
   dy number;
   b1 number;
   b2 number;
   s                     number;
   m12                   number;
   az1                   number;
   az2                   number;
   az3                   number;
   az4                   number;
   gcd_K                 number;
   big                   number:=0.0;
   old_b       number;
   rot_angle   number;
   alt_angle   number;
   angel       number :=45;
   included_angle number;

begin
--    dbms_random.initialize(123459);
    for jk in 0..70 loop
--     dbms_random.initialize(123459);
    for ii in 1..2 loop
       if ii = 1 then
        y1 := jk;
       else
        y1 := -jk;
       end if;
--       x1 := round(x2 + dbms_random.value(0,1)*0.05+ii*dbms_random.value(0,0.01),6);
--       y1 := round(y2 -dbms_random.value(0,1.)*0.05,6);
       x1 := round(x2 + 0.05);
       y2 := y1+0.0001;
       x3 := round(x1 + cos(angel*deg2rad)*0.15,6);
       y3 := round(y1 + sin(angel*deg2rad)*0.15,6);
--       x1 := -119.9999;
--       y1 := 40.0003;
--       x3 := -119.9999;
--       y3 := 40.;
--       dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
--       dbms_output.put_line('x3 ' || x3 || ' y3 ' || y3);
--    included_angle := GZ_QA.angle(x1,y1,x2,y2,x3,y3)*rad2deg;
    included_angle := angle(x1,y1,x2,y2,x3,y3,b1,b2);
    dbms_output.put_line('b1 ' || round(b1,5) || ' b2 '|| round(b2,5));

        s := GZ_geodesic.inverse(y2,x2,y1,x1,gcd_K,az1,az2,m12);
        s := GZ_geodesic.inverse(y2,x2,y3,x3,gcd_K,az2,az4,m12);
 
        if az1 < 0.0 then
          az1 := -az1+90.;
        elsif az1 <= 90. then
           az1 := 90.-az1;
        else
           az1 := 270+(180.-az1);
        end if;
 

        if az2 < 0.0 then
          az2 := -az2+90.;
        elsif az2 <= 90. then
           az2 := 90.-az2;
        else
           az2 := 270+(180.-az2);                 
        end if;
 dbms_output.put_line('az1 ' || round(az1,5)  || ' az2 ' || round(az2,5));
--az1 := geo_bearing(x2,y2,x1,y1);
--az2 := geo_bearing(x2,y2,x3,y3);
-- dbms_output.put_line('az1 ' || round(az1,9)  || ' az2 ' || round(az2,9));

--      dbms_output.put_line('Az1 ' || round(az1,9)  || ' Az2 ' || round(az2,9));  
    alt_angle := az1-az2;
--     dbms_output.put_line('AAlt ' || round(alt_angle,9) );
    if alt_angle < 0. then
      alt_angle := az1-az2+360.;
--       dbms_output.put_line('Alt ' || round(alt_angle,9) );
      if alt_angle > 180. then
        alt_angle := 360. - alt_angle;
      end if;
    else
       if alt_angle > 180. then
          alt_angle := 360. - alt_angle;
       end if;
--        dbms_output.put_line('alt ' || round(alt_angle,9) );
    end if;

    xys(1) := x2;
    xys(2) := y2;
    xys(3) := x1;
    xys(4) := y1;
    xys(5) := x3;
    xys(6) := y3;
    dy := y1-y2;
    dx := x1-x2;
--    dbms_output.put_line('x1 ' || round(xys(3),6) || ',' || round(xys(4),6));
--    dbms_output.put_line('x2 ' || round(xys(1),6) || ',' || round(xys(2),6));
--    dbms_output.put_line('x3 ' || round(xys(5),6) || ',' || round(xys(6),6));

    rot_angle := -(atan2(dy,dx)*rad2deg);
--     dbms_output.put_line('rotation  ' || round(rot_angle,4));
    rot_xys := Gz_qa.ROTATE_COORDINATES(Xys,rot_angle,x2,y2);
    dy := Rot_xys(6)-Rot_xys(2);
    dx := Rot_xys(5)-Rot_xys(1);
/*
    dbms_output.put_line('X1 ' || round(rot_xys(3),6) || ',' || round(rot_xys(4),6));
    dbms_output.put_line('X2 ' || round(rot_xys(1),6) || ',' || round(rot_xys(2),6));
    dbms_output.put_line('X3 ' || round(rot_xys(5),6) || ',' || round(rot_xys(6),6));
    rot_angle := angle(rot_xys(3),rot_xys(4),rot_xys(1),rot_xys(2),rot_xys(5),rot_xys(6),b1,b2);
*/
--    b1 := gz_qa.geo_bearing(x1,y1,x2,y2,old_b)*rad2deg;
--    dbms_output.put_line('bearing '|| b1);
--    dbms_output.put_line('Angle           ' || round(included_angle,4) || ' expected angle ' || round(alt_angle,4));

    if abs(alt_angle -included_angle) > big then
       big := abs(alt_angle -included_angle);
    end if;
--    dbms_output.put_line('Angle           ' || round(included_angle,4) || ' expected angle ' || round(abs(atan2(dy,dx))*rad2deg,4));
--    dbms_output.put_line('Geodesic Angle  ' || round(alt_angle,4));
--    rot_angle := GZ_qa.angle(rot_xys(3),rot_xys(4),rot_xys(1),rot_xys(2),rot_xys(5),rot_xys(6))*rad2deg;
 --   dbms_output.put_line('GZ_QA.Angle ' || round(rot_angle,4) || ' alternative angle ' || round(alt_angle,4)); 
    end loop;
      dbms_output.put_line('AT latitude ' || jk || ' Biggest           ' || round(big,4) );
      big := 0.0;
    end loop;
end;
procedure test_centroid as
xc  number;
yc  number;
Ord Mdsys.sdo_ordinate_array;
Info  Mdsys.sdo_elem_info_array;
tag VARCHAR2(100);
area  number;
areas  Mdsys.sdo_list_type;
begin
-- Now set up a rectangle with a hole - a diamond chosen symmetrically so
-- the centroid does not move.
     For ii in 1..4 loop
       if ii = 1 then
         Info := MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1); --(2,0,10,4,10,4,8,8,8,8,0,4,2,0);
-- add -100 to x here
         Ord  := MDSYS.SDO_ORDINATE_ARRAY(-98,0,-90,4,-90,4,-92,8,-92,8,-100,4,-98,0);
         tag := 'area of whole polygon ';
          area := centroid(Ord,xc,yc);
       elsif ii = 2 then
          Ord  := MDSYS.SDO_ORDINATE_ARRAY(3,3,5.4,3.2,7,5,7,5,4.6,4.8,3,3,3,3);
          tag := 'area of hole ';
           area := centroid(Ord,xc,yc);
       elsif ii = 3 then
          Info := MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1,11,1003,1);
          Ord  := MDSYS.SDO_ORDINATE_ARRAY(2,0,10,4,8,8,0,4,2,0);
          tag := 'area of polygon with hole ';
           area := centroid(Ord,xc,yc);
           Ord  := MDSYS.SDO_ORDINATE_ARRAY(3,3,4.6,4.8,7,5,5.4,3.2,3,3);
           area := area + centroid(Ord,xc,yc);
       else
          Info := MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1,11,2003,1,21,1003,1);
          Ord  := MDSYS.SDO_ORDINATE_ARRAY(2,0,10,4,8,8,0,4,2,0);
          tag := 'area of polygon with hole, and hole filled ';
           area := centroid(Ord,xc,yc);
           
           Ord  := MDSYS.SDO_ORDINATE_ARRAY(3,3,4.6,4.8,7,5,5.4,3.2,3,3);
           area := area + centroid(Ord,xc,yc);
           Ord  := MDSYS.SDO_ORDINATE_ARRAY(3,3,5.4,3.2,7,5,4.6,4.8,3,3);
           area := area + centroid(Ord,xc,yc);
       end if;
 

    
      dbms_output.put_line(tag || area || ' xc ' || xc || ' yc ' || yc);
      end loop;
end;
function test_bearings(geom mdsys.sdo_geometry) return number as
deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
   bearings          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   rvbearings          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   xys     mdsys.sdo_ordinate_array := mdsys.sdo_ordinate_array();
   mapto_vertex   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   B_to_V      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   measure number := 11.112;
   Inn_start pls_integer :=1;
   Inn_end   pls_integer := 40;
   pred_bcount   number;
   x             number := -100.;
   y             number := 0.;
   d             number;
   b             number;
   m             pls_integer;
begin

  if geom is NOT NULL then
    measure := 164.9023298; --139.491451;
    measure := 100.;
    xys := geom.sdo_ordinates;
    Inn_end := xys.count;
  else
  measure := measure * 0.5;
   xys.extend(40);
--   dbms_random.initialize(123459);
   for ii in 1..20 loop
      xys(ii*2-1) := -100 +(ii-1)*0.001; --*dbms_random.value(0,1);
      xys(ii*2) := y + 0.001*sin(2*(ii-1)*deg2rad); --dbms_random.value(-1,1);
      d := round(accurate_gcd(x,y,xys(ii*2-1),xys(ii*2)),4);
      b := round(geo_bearing(x,y,xys(ii*2-1),xys(ii*2)),4);
      x := xys(ii*2-1);
      y := xys(ii*2);
      
      dbms_output.put_line('x ' || round(xys(ii*2-1),9) || ' y ' || round(xys(ii*2),9) || ' d ' || d || ' b ' || b);
   end loop;
  end if; 
--   geom := sdo_geometry(2003,8265,null,SDO_ELEM_INFO_ARRAY(1,1003,1),Xys);
   
--    bearings := Bearings_Along_aline(measure,Inn_Start, Inn_End,XYs,b_to_v,pred_bcount);
    gz_topofix.reverse_ordinates(xys);
--    rvbearings := Bearings_Along_aline(measure,Inn_Start, Inn_End,XYs,b_to_v,pred_bcount);
    m := rvbearings.count;
    for ii in 1..bearings.count loop
      dbms_output.put_line('ii ' || ii || ' B ' || round(bearings(ii),4) || ' R ' || round(rvbearings(m+1-ii)+180.,4) || ' D ' || round(bearings(ii)-(rvbearings(m+1-ii)+180),4));
    end loop;
    dbms_output.put_line('pred_bcount ' || pred_bcount);
    return pred_bcount;
end;
--
END GZ_UTIL_ZONE;
/
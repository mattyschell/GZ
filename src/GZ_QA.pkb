CREATE OR REPLACE PACKAGE BODY GZ_QA AS

--==============================================================================
-- A package to compute quality assurance values for comparable polygon entities in
-- two separate tables and compare the differences. (The tables are usually FSL
-- tables but could just be any polygon entities build from the underlying edge$ tables.)
-- Compute_QA_values is usually run after line simplification to ensure that the
-- shape and area have not changed too much.
-- With 2007 geometries, this has to be done on a polygon on polygon basis. So a
-- major problem is figuring out which rings match - especially for figure "8"
-- polygons. It appears that some FSLs have many such polygons that become
-- ordinary polygons after generalization (get_geometry must dissolve an edge
-- because the 2 faces on either side have the same attribute).

-- Two other entry points, check_all_vertices and flag_affected_entities work
-- on edges and 1) check to see if an edge is ungeneralized and then if so 2)
-- flag affected polygon entities with a QA flag and a status showing the
-- reason for flagging.
--==============================================================================


PROCEDURE COMPUTE_QA_VALUES( UnGen_Schema VARCHAR2, UnGenView VARCHAR2, UnGenColumn VARCHAR2, Gen_Schema VARCHAR2, GenView VARCHAR2, GenColumn VARCHAR2, compare_table_name VARCHAR2,target_scale NUMBER,CompareGenColumn VARCHAR2 default 'RING_GEOM',parea_check NUMBER default NULL, shape_check NUMBER default 1.5) AS
/*
********************************************************************************
--Program Name: Compute_QA_Values
--Author: Sidey Timmins
--Creation Date: 6/25/2010
--Modification Date:
--                    03/23/12  Fixed xc (centroid) sign was sometimes wrong.
--                    04/27/11  New Code to find/get Pipes. Utilities for
--                              trig, padding, getting angles, distances.
--                    04/08/11  Fixed Boyce and compare table sdogeometry column parameterized
--                    03/29/11  Maintenance stuff for 2007s
--                    03/16/11  Deprecated Eigenvectors. Set Area threshold
--                              for noticeable difference in area to .0125 inches
--                              squared so at target scale (say 500k) this is 42046 m^2.
--                   03/08/11   Revised Moments and Eigenvectors for numerical
--                              robustness.
--                  07/02/2010  Made eigenvectors numerically stable by
--                              changing moments to not divide the mu_matrix by 6*area.
--
--Usage: (u = ungeneralzed, g = generalized)
--
-- SQL> exec gz_qa.compute_qa_values('uSchema','uTable','ugeometry',
--                                   'gSchema','gTable','gGeometry',
--                                   'Output_table',target_scale);

  -- Call this function from inside another PL/SQL program.  This function
  -- has 8 required parameters:
  --
  --      INPUT
  --      Ungen_Schema        = the ungeneralized schema
  --      UnGenView           - the ungeneralized table or view to process
  --      UnGenColumn         - the geometry column in UnGenview
  --      Gen_Schema          - generalized schema
  --      GenView             - the generalized table or view to process
  --      genColumn           - the geometry column in Genview
  --      compare_table_name  - the output table name to fill with QA values.
  --                            The table will be dropped first if it already exists.
  --      target_scale        - the target scale (used to check whether the area
  --                            changes are significant.
  --
  -- These parameters are thresholds that are used together to set the QA_FLAG.
  --      area_check          - this length input in inches that will be squared and
  --                            converted to square meters at the target scale.
  --                            When the shape check is exceeded and the
  --                            generalized area is less than the
  --                            (ungeneralized area - area_check), the QA_flag is set.
  --      shape_check         - A threshold for the shape index (usually 1.5).
  --                            A range of 1.25 to 1.75 probably appropriate, with
  --                            1.25 being very sensitive to changes and 1.75 allowing
  --                            more changes before the QA_Flag is set.
--
-- Purpose: Compute fundamental measures of polygon fidelity: area, perimeter,
-- the minimum bounding rectangle (MBR), the lengths and attitudes of the major
-- and minor axes and then sets a QA_FLAG when there are significant differences.
-- The output table contains a generalized geometry column with rows for each
-- 2003 and 2007 polygons.
-- Also there are extra rows for each outer ring in 2007 generalized polygons.
-- This is the "main" entry point to measure Quality Assurance values.

--  ************************************************************************
--  * At the moment this package does not really compute an absolute shape *
--  * index, it tries to measure differences in shape.                             *
--  ************************************************************************
-- This diagram shows the simple attributes of a shape that are easily measured:
--  area, perimeter, the lengths and attitude angle of the major/minor axes
-- (measured anti-clockwise from the x-axis) and the polygon extent.
--                      ^
--                      | minor axis
--                      |
--             <---Major length --> (xUR,yUR)
--             +-------------------+
--             |                   |      <----------------> major axis
--             +-------------------+
--       (xLL,yLL)   perimeter
--
--
--         Here we show the angle (attitude) of the major axis for an airbitary
--         polygon: note the vector +...>  (8,4) goes from (2,0) to (10,4). (To
--                  make a vector from 2 points, subtract the start coordinates
--                  from the end ones!)
--
---                                           + (8,8)
--                                     .       \
--         y axis     ^          .              \
--                    |     .                    > (10,4)
--                (0,4) +                  .
--                    |  \           .
--                    |   \     .     \ angle: anti-clockwise from x-axis to vector (8,4)
--                    -----+----------------------> x axis
--                          (2,0)

-- QA computes basic attributes that describe a shape and then derived values of:

-- 1) the area ratio (always in the order of generalized divided by ungeneralized),
-- 2) the length (perimeter) ratio,
-- 3) Chorley and Haggets shape descriptor (shape)  = 4/pi*area/(major axis squared).

--    It is not unique and can have same result for different inputs. This
--    demonstrates the difficulty in reducing many coordinates to a single number
--    examples:  polygon         shape           area       elongated length squared
--                circle           1             pi*r^2          2r
--                square          1.27..           1              1
--          equilateral triangle  0.3183           1              2
--          rectangle 4:1         0.3183           1              2

-- 4) a form ratio based upon this shape descriptor:
--       generalized shape/ungeneralized shape
-- 5) the angle difference between the attitude of the principle axis before
--    generalization versus afterwards.
-- 6) Sidey's shape difference: a composite of the absolute area difference,
--    the angle difference of the major axes and Chorley and Hagget's shape descriptor:
--    before minus after.
--
-- References: "Measuring and Comparing Two-dimensional Shapes" by Robert F Austin
--    in Spatial Statistics and Models, 292-312, 1984 Gaile and Wilmott (eds)
--    On the Calculation of Moments of Polygons. Carsten Steger.
---- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.29.8765&rep=rep1&type=pdf
--    CARTER, H. (1976). "The Geographical Approach, The Plans and Topography of
--                       Medieval Towns in England and Wales" (Barley, M.W., Ed.)
--( BA Research Report No. ...
-- excerpt:   ads.ahds.ac.uk/catalogue/adsdata/cbaresrep/pdf/014/01402003.pdf
-- Haggett, P and Chorley, RJ 1969 "Network Analysis in Geography"
--   New York: St. Martins's Press
--
-- Calls: Measure_Elongate_Length
********************************************************************************
*/
   type              cursor_type is REF CURSOR;
   TYPE "GEOID_STRING_ARRAY" IS VARRAY(1048576) OF VARCHAR2(60);
   axes              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   gaxes             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   xys               mdsys.sdo_ordinate_array;
   rot_xys           mdsys.sdo_ordinate_array;
   table_cursor      cursor_type ;
   geometry          MDSYS.SDO_GEOMETRY;
   Ugeometry         MDSYS.SDO_GEOMETRY;
   Ggeometry         MDSYS.SDO_GEOMETRY;
   ungeom            MDSYS.SDO_GEOMETRY;
   gengeom           MDSYS.SDO_GEOMETRY;
   geoids            GEOID_STRING_ARRAY := GEOID_STRING_ARRAY();
   gradii            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   uradii            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   qas_2007          MDSYS.SDO_LIST_TYPE;
   ids_2007          GEOID_STRING_ARRAY;
   gXys              MDSYS.SDO_ORDINATE_ARRAY;
   uXys              MDSYS.SDO_ORDINATE_ARRAY;
   UInfo             MDSYS.SDO_ELEM_INFO_ARRAY;
   kount             PLS_INTEGER;
   ulen              NUMBER;
   glen              NUMBER;
   garea             NUMBER;
   uarea             NUMBER;
   gshape            NUMBER;
   ushape            NUMBER;
   uangle            NUMBER;
   gangle            NUMBER;
   gmajor            NUMBER;
   umajor            NUMBER;
   gminor            NUMBER;
   uminor            NUMBER;
   gshape_index      NUMBER;
   ushape_index      NUMBER;
   shape_fidelity    NUMBER;
   area              NUMBER;
   area_ratio        NUMBER;
   area_check        NUMBER := parea_check;
   QA_FLAG           NUMBER;
   big_diff          NUMBER;
   xc                NUMBER;
   yc                NUMBER;
   gxc               NUMBER;
   gyc               NUMBER;
   delta             NUMBER;
   SSQ               NUMBER;
   sum_radii         NUMBER;
   gsum_radii        NUMBER;
   boyce_shape       NUMBER;
   gboyce_shape      NUMBER;
   temp              NUMBER;
   bias              NUMBER;
   gbias             NUMBER;
   angle             NUMBER;
   bad_ring          NUMBER;
   outer_ring        NUMBER;
   try_area_ratio    NUMBER;
   bad_area_ratio    NUMBER;
   ring              NUMBER;
   gring             NUMBER;
   usum_now          NUMBER;
   grings            PLS_INTEGER;
   next              PLS_INTEGER;
   jj                PLS_INTEGER;
   xLL               NUMBER;
   xUR               NUMBER;
   yLL               NUMBER;
   yUR               NUMBER;
   gxLL              NUMBER;
   gxUR              NUMBER;
   gyLL              NUMBER;
   gyUR              NUMBER;

   Ok                VARCHAR2(5);
   geo_id            VARCHAR2(60);
   sql_stmt          VARCHAR2(4000);
   sql_stmt2         VARCHAR2(4000);
   sql_stmt3         VARCHAR2(4000);
BEGIN

   if area_check <= 0.0 or area_check is NULL then
--     area_check := target_scale*0.000410105;   -- meters = 1/80 inch at target scale
       area_check := target_scale*0.000656168;   -- meters = 1/50 inch at target scale
   else
       area_check := area_check*target_scale;
   end if;
   area_check := area_check * area_check;    -- square meters at target scale
   dbms_output.put_line('AREA check ' || round(area_check,4) || ' sq meters');

   if Table_Exists(Compare_Table_name) then
     sql_stmt := 'DROP TABLE ' || Compare_Table_name;
     EXECUTE Immediate sql_stmt;
   end if;

-- Add a QA flag to indicate the entities that need a visual check
   sql_stmt := 'CREATE TABLE '|| Compare_Table_name || ' AS SELECT g.geo_id,0. QA_FLAG,0. shape_fidelity,0. form_ratio,' ||
              '0.0 garea,0.0 uarea,0.0 glength,0.0 ulength,'||
              '0. area_ratio,0. len_ratio,0. gshape,0. ushape,0. GBSI, 0. BSI,'||
              '0. gmajor,0. umajor,0. gminor,0. uminor,0. gangle,0. uangle,0. angle_diff,'||
              '0. gxLL,0.gyLL,0. gxUR,0.gyUR,0. xLL,0.yLL,0. xUR,0.yUR '||
              'from '||Gen_Schema||'.' || GenView || ' g, '||UnGen_Schema||'.' || UnGenView ||
              ' u where g.geo_id=u.geo_id';

  execute immediate sql_stmt;

   sql_stmt := 'GRANT SELECT on ' || Compare_table_name || ' TO PUBLIC';
   execute immediate sql_stmt;

   sql_stmt := 'SELECT geo_id from ' || Compare_table_name;
   execute immediate sql_stmt BULK COLLECT into Geoids;

-- Now add the sdogeometry

   sql_stmt := 'ALTER TABLE ' || Compare_table_name || ' ADD ('||CompareGenColumn ||' sdo_geometry)';
   execute immediate sql_stmt;

   sql_stmt := 'UPDATE ' || Compare_table_name || ' t SET t.'||CompareGenColumn||'=(select s.'||GenColumn ||' from '|| Gen_Schema ||'.'||GenView ||' s where s.geo_id=t.geo_id)';
   execute immediate sql_stmt;

   sql_stmt := 'SELECT g.' || GenColumn || ',u.' || UnGenColumn || ' from ' ||Gen_Schema||'.' || GenView || ' g, '||UnGen_Schema||'.' || UnGenView || ' u where g.geo_id=u.geo_id and g.geo_id=:1';
   sql_stmt2 := 'UPDATE ' || Compare_table_name || ' set QA_FLAG=:1,shape_fidelity=:2,garea=:3,uarea=:4,glength=:5,ulength=:6,area_ratio=:7,gshape=:8,ushape=:9,BSI=:10,GBSI=:11,gangle=:12,uangle=:13,gmajor=:14,umajor=:15,gminor=:16,uminor=:17,' ||
               'gxLL=:18,gyLL=:19,gxUR=:20,gyUR=:21,xLL=:22,yLL=:23,xUR=:24,yUR=:25 where QA_FLAG=0. and geo_id=:26';
   sql_stmt3 := 'SELECT SUM(Gangle * garea)/SUM(garea),SUM(Uangle * uarea)/SUM(uarea) from ' || Compare_table_name || ' where Geo_id=:1 and gmajor <> 0.';


   For ii in 1..geoids.count Loop
     geo_id := geoids(ii);
dbms_output.put_line(geo_id);
--    geo_id := '1500000US530530607004';
--     geo_id := '1500000US530330039001';

--     geo_id := '1500000US530610527071';
-- geo_id := '1500000US530330294051';
--geo_id :='1500000US530330303142';
--geo_id := '1500000US530619901000';  --sqrt(-ve area)
--geo_id := '1500000US530330232023';
--geo_id:='1500000US530299922010';
--geo_id:='1500000US530439601001';
--geo_id:='1500000US530179502001';
--dbms_output.put_line(geo_id);
--geo_id:='1500000US530319503004';
--geo_id := '1500000US530579901000';
--geo_id:='1500000US530579901000';
--geo_id:='1500000US530099901000';
--geo_id:='1500000US530359901000';
--geo_id:='1600000US5370560';
--geo_id:='1600000US5336045';
--geo_id:='1600000US5307170';
--geo_id :='7000000US53029WVSARP';
--geo_id := '2500000US3625';
--geo_id:='1500000US530110414002';
--geo_id := '1500000US530090017002';
--geo_id:= '1500000US530530726023';
--geo_id := '1500000US530559901000';
--geo_id := '1500000US530330096005';
--  geo_id := '9700000US0103180';
--  geo_id := '9700000US0102310';
--   geo_id := '9700000US0103430';
-- geo_id := '1500000US300619645001';

     execute immediate sql_stmt into Ggeometry,Ugeometry using geo_id;


     ok := Pack_geometry(Ggeometry);
     ok := Pack_Geometry(Ugeometry);

--   ugeometry := SDO_GEOMETRY(2003, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 1003,1), SDO_ORDINATE_ARRAY( -122.38324, 48.29759, -122.45833, 48.297592, -122.40575, 48.252177, -122.38324, 48.29759));
--   xys := SDO_ORDINATE_ARRAY( -122.38324, 48.29759, -122.420785,48.37259,-122.45833, 48.29759, -122.38324, 48.29759);
--   xys := mdsys.sdo_ordinate_array(0.,0.00000,6.00,0.,6.00,6.000000,0.0,6.000000,0.,0.0);
--   angle:= 55;
--   xys := SDO_ORDINATE_ARRAY(2,0,10,4,8,8,0,4,2,0);
--    dbms_random.seed(181457802);
--   for ii in 3..xys.count-2 loop
--    dbms_output.put_line(round(rot_xys(ii-1),7) || ',' ||round(rot_xys(ii),7));
--      if mod(ii,2) = 0 then
--      xys(ii) := xys(ii) +  1.E-8*dbms_random.value -0.5E-8;
--      end if;
--   end loop;

/*

   rot_xys := ROTATE_COORDINATES(Xys,angle,xys(1),xys(2));
   rot_xys := circulate_ordinates(rot_xys,1);
 for ii in 1..xys.count loop
    if mod(ii,2) = 0 then
     dbms_output.put_line(round(rot_xys(ii-1),7) || ',' ||round(rot_xys(ii),7));
    end if;
   end loop;
  ugeometry := SDO_GEOMETRY(2003, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 1003,1),rot_xys);
  */
--    xys := ugeometry.sdo_ordinates;


--   for ii in 1..xys.count loop
--    if mod(ii,2) = 0 then
--      xys(ii-1) := xys(ii-1) + 122.32;
--      xys(ii) := xys(ii) -47.30;
--    end if;
--   end loop;
--   for ii in 3..xys.count-2 loop
--      if mod(ii,2) = 0 then
--      xys(ii) := xys(ii) +  1.E-8*dbms_random.value -0.5E-8;
--     end if;
--   end loop;
--  ggeometry := SDO_GEOMETRY(2003, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 1003,1),xys);
-- First do the areas in square meters
--    Measure elongate length does this for the biggest polygon

-- just for testing
--      ggeometry := sdo_util.simplify(ugeometry,15000.);


--==============================================================================
-- Check that the geometries are ok
-- Not good, it destroys polygons and removes essential vertices
--     ok := check_and_rectify(Ugeometry);
--     ok := check_and_rectify(Ggeometry);

-- Get major axis length and attitude for the ungeneralized
     xc := NULL;
     yc := NULL;
-- dbms_output.put_line('Calling measure elongate with ' || ugeometry.sdo_ordinates.count);
     axes := measure_elongate_length(Ugeometry,xc,yc,uradii,geo_id);
-- Extract from axes the measurements
     uangle := ROUND(axes(1),4);
--dbms_output.put_line('uangle ' || uangle || ' g ' || gangle);
     umajor := ROUND(axes(2),4);
     uminor := ROUND(axes(3),4);
     xLL := axes(4);
     yLL := axes(5);
     xUR := axes(6);
     yUR := axes(7);
     uarea := ROUND(axes(8),4);

     ulen := ROUND(sdo_geom.sdo_length(Ugeometry,0.05,'unit=meter'),4);
     xc := axes(9);
     yc := axes(10);

-- Get major axis length and attitude for the generalized
-- Note here we pass the ungeneralized centroid
--dbms_output.put_line('calling measure elongate with ' || ggeometry.sdo_ordinates.count);
     gaxes := measure_elongate_length(Ggeometry,xc,yc,gradii,geo_id);

     gangle := ROUND(gaxes(1),4);
     gmajor := ROUND(gaxes(2),4);
     gminor := ROUND(gaxes(3),4);
     gxLL := gaxes(4);
     gyLL := gaxes(5);
     gxUR := gaxes(6);
     gyUR := gaxes(7);
     garea := ROUND(gaxes(8),4);
     glen := ROUND(sdo_geom.sdo_length(Ggeometry,0.05,'unit=meter'),4);
--   Store shape index based upon the mu_matrix:
--                       (mu_yy * mu_xx - mu_xy*muxy)/ (area in degrees^6);

--     gshape_index := ROUND(abs(axes(10)),6);
     gxc := gaxes(9);
     gyc := gaxes(10);


-- Occasionaly, a triangular shaped polygon can flip its axes (major becomes
-- minor and vice versa). In that case we ignore this flipping.
--dbms_output.put_line('umin ' || uminangle || ' g ' || gminangle);
--dbms_output.put_line('diff ' || (uangle-gminangle) || ' g ' || (gangle-uminangle));
--     if abs(uangle - gminangle) < 2. and abs(gangle - uminangle) < 2. then
--        temp := gangle;
--        gangle := gminangle;
--        gminangle := temp;
--     end if;

-- For a multipolygon (2007) check each polygon for the generalized against its
-- original 2003 polygon(s).
-- Note carefully: many FSLs have figure of '8' polygons that touch at 1 point
-- in the ungeneralized that become single polygons in the generalized.

     bad_ring := 1.0;
     if Ugeometry.sdo_gtype = 2007 then
        area_ratio := Compute_QA_Values_2007(geo_id,Ugeometry,Ggeometry,GenColumn,Compare_table_name,target_scale,area_check,shape_check);
-- For a 2007 average the angles need to be calculated by weighting the values for the rings
        execute immediate sql_stmt3 into gangle,uangle using geo_id;
-- dbms_output.put_line('uangle ' || uangle || ' g ' || gangle);
        uangle := ROUND(uangle,4);
        gangle := ROUND(gangle,4);
     end if;

-- We don't care about the bearing of the line, if one axis goes west and the
-- other east, it is the same axis.

-- This seems to get it
     if uangle > 170 and gangle <10. then
          gangle := gangle + 180.;
     elsif gangle > 170. and uangle < 10. then
          uangle  := uangle + 180.;
     end if;
--dbms_output.put_line('u00 ' || uangle || ' g  ' || gangle);
-- But this is better
     if abs(uangle-gangle) > 90 and uangle < gangle then
         uangle := uangle + 180.;
         if uangle >= 360. then
           uangle := uangle -360.;
         end if;
     elsif abs(uangle-gangle) > 90 and gangle < uangle then
         gangle := gangle + 180.;
         if gangle >= 360. then
           gangle := gangle -360.;
         end if;
     end if;
-- dbms_output.put_line('u ' || uangle || ' g  ' || gangle);
-- Flip major and minor when the difference in the angles > 79 degrees
-- then the orientations are presumed to be the same
    if abs(uangle-gangle) > 79.  then
       if uangle < gangle then
         gangle := gangle -90.;
       else
         gangle := gangle +90.;
       end if;
       temp := gmajor;
       gmajor := gminor;
       gminor := temp;
--       dbms_output.put_line('uu ' || uangle || ' g  ' || gangle || ' geo id ' || geo_id);
    end if;
--dbms_output.put_line('u ' || uangle || ' g  ' || gangle);

-- Compute Chorley and Hagget's Shape index using the Major axis (elongated length):
--                  4/pi A/L^2   where L = Major axis length

     gshape := ROUND(1.273239544*garea/(gmajor*gmajor),6);
     ushape := ROUND(1.273239544*uarea/(umajor*umajor),6);


--      dbms_output.put_line('garea ' || round(garea,4) || ' uarea ' || round(uarea,4) || ' ratio ' || round(garea/uarea,6));


--  Check the shape radii. Made up example:
--rad -14414.9685 angles 0
--rad -16258.525 angles 22.5
--rad -21628.8974 angles 45
--rad -19434.0496 angles 67.5
--rad -18804.2764 angles 90
--rad -20101.139 angles 112.5
--rad -22862.5934 angles 135
--rad -16420.7667 angles 157.5
--rad -14738.4637 angles 180
--rad -16447.1008 angles 202.5
--rad -22941.629 angles 225
--rad -17061.5526 angles 247.5
--rad -15946.5987 angles 270   <- start of 270 is marked negative
--     25000      ANGLES 270   <-- positive distance means 270 continues
--rad -20091.9057 angles 292.5
--rad -21765.7849 angles 315
--rad -15884.875 angles 337.5

     big_diff := 0.;
     ssq := 0.0;
     sum_radii := 0.0;
     boyce_shape := 0.0;
     gboyce_shape := 0.0;
     For ii in 1.. uradii.count Loop
       sum_radii := sum_radii + abs(uradii(ii));
--       dbms_output.put_line('ii ' || ii || ' uR ' ||round(uradii(ii),8));
     End Loop;
     gsum_radii := 0.0;
     For ii in 1.. gradii.count Loop
       gsum_radii := gsum_radii + abs(gradii(ii));
--       dbms_output.put_line('ii ' || ii || ' gR ' ||round(gradii(ii),8));
     End Loop;

--     dbms_output.put_line('sum ' || round(sum_radii,8));
--     dbms_output.put_line('sum ' || round(gsum_radii,8));
     if uradii.count = 0 or gradii.count = 0 then
       boyce_shape := -1.;  -- not calculated
       gboyce_shape := -1.;
     else
     bias := 100./uradii.count;   -- 6.25 for 16
     gbias := 100./gradii.count;
     if uradii.count <> gradii.count then
     dbms_output.put_line('u ' ||uradii.count || ' g ' || gradii.count || ' ' || geo_id);
     end if;
     next := 1;
     jj := 1;
     For ii in 1.. uradii.count Loop
     exit when jj > uradii.count or next > gradii.count;
       delta := abs(uradii(jj) - gradii(next));
       boyce_shape := boyce_shape + abs(abs(uradii(jj))/sum_radii*100.-bias);
--       dbms_output.put_line('ii ' || ii || ' now b ' || round(boyce_shape,8));
       gboyce_shape := gboyce_shape + abs(abs(gradii(next))/gsum_radii*100.-gbias);
--       dbms_output.put_line('ii ' || ii || ' now g ' || round(gboyce_shape,8));
       ssq := ssq + delta*delta;
       IF next <= gradii.count THEN
       if abs(uradii(jj) - gradii(next)) > big_diff then
          big_diff := abs(uradii(jj) - gradii(next));
          next := next + 1;
-- Make sure the radii stay in sync. Negative distance means the start of the
-- next angle.
          if next < gradii.count and gradii(next+1) < 0.0 then
              jj := jj+1;
              while uradii(jj) > 0.0 loop
                 boyce_shape := boyce_shape + abs(abs(uradii(jj))/sum_radii*100.-bias);
                 jj := jj + 1;
              end loop;
          else
             jj := jj+1;
          end if;
       else
          jj := jj + 1;
          next := next+1;
       end if;
       END IF;

     End Loop;

     SSQ := ROUND(SSQ,2);
     Big_diff := ROUND(Big_diff,2);
     Boyce_shape := ROUND(boyce_shape,4);
     GBoyce_shape := ROUND(GBoyce_shape,4);
     end if;
--     dbms_output.put_line('boyce ' || boyce_shape  || 'gboyce ' || gboyce_shape);
--==============================================================================
--
-- Set the QA FLAG

      qa_flag := NULL;


--      if ugeometry.sdo_gtype = 2007 then
--        NULL;   -- worst area ratio computed above
--      else
        area_ratio := ROUND(garea/uarea,6);
--      end if;

-- This index weights both the area differences and the attitude differences
-- of the major axis computed from the 2nd moments of inertia. So the area difference
-- is grounded in geometry and the attitude difference in physics.

-- Compute Sideys shape fidelity: 1 + ABS(1-area_generalized/area_ungeneralized) +
--                             ABS(ungeneralized_attitude-generalized_attitude)/100.
--
      shape_fidelity := 1.+ROUND(abs(1.-area_ratio) + 0.02*abs(uangle-gangle) +ABS(gshape-ushape),4);
      if shape_fidelity > shape_check and abs(1.-area_ratio)*uarea > area_check then
        qa_flag := bad_ring;
      end if;
      execute immediate sql_stmt2 using qa_flag,shape_fidelity,garea,uarea,glen,ulen,area_ratio,gshape,ushape,GBoyce_shape,Boyce_shape,gangle,uangle,gmajor,umajor,gminor,uminor,
                       gxLL,gyLL,gxUR,gyUR,xLL,yLL,xUR,yUR,geo_id;
      commit;
  End Loop;


-- Set form ratio, perimeter ratio, angle difference
   sql_stmt := 'UPDATE ' || Compare_table_name || ' set form_ratio=1.+ROUND(ABS(gshape-ushape),4),len_ratio=ROUND(glength/ulength,6),angle_diff=ROUND(uangle-gangle,4) where ulength <> 0.0';
   execute immediate sql_stmt;

-- For 2007's just put the "bad" geometry ring into the column
--   sql_stmt := 'SELECT GEO_ID,QA_FLAG from '|| Compare_table_name || ' WHERE QA_FLAG is not NULL';
--   execute immediate sql_stmt BULK COLLECT into ids_2007,qas_2007;

--   for ii in 1..ids_2007.count loop
--        sql_stmt := 'UPDATE ' || Compare_table_name || ' t SET t.'||GenColumn||'=sdo_util.extract(t.'||GenColumn||',:1,0) WHERE t.geo_id=:2';
--        ring := qas_2007(ii);
--        execute immediate sql_stmt using ring,ids_2007(ii);
--   end loop;
commit;


--EXCEPTION
--   WHEN OTHERS THEN
--   Generate_Error_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

END COMPUTE_QA_VALUES;
--
FUNCTION PACK_GEOMETRY(geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY) RETURN VARCHAR2 AS
   Xys MDSYS.SDO_ORDINATE_ARRAY := geometry.sdo_ordinates;
   info         MDSYS.SDO_ELEM_INFO_ARRAY := geometry.sdo_elem_info;
   n            PLS_INTEGER;
   k            PLS_INTEGER;
   next        PLS_INTEGER :=1;
   ring_next    PLS_INTEGER :=0;
   inext        PLS_INTEGER :=3;
   next_save    PLS_INTEGER := 1;

   result   VARCHAR2(1) := 'N';
BEGIN

-- We allow for the nasty case of an inside ring making a figure of eight
-- with the outside ring
     if inext < info.count then
        inext := inext +3;
        ring_next := info(inext-2)+1;
     end if;

   n := Xys.count;
   For ii in 2..TRUNC(n/2) loop
      k := ii*2;
      If k <> ring_next and (Xys(k-3) = Xys(k-1) AND Xys(k-2) = Xys(k)) then
         NULL;
      Else
         next := next + 1;
         Xys(next*2-1) := Xys(k-1);
         Xys(next*2)   := Xys(k);
         if k = ring_next then
            Info(inext-2) := next*2-1;
            if inext < info.count then
               inext := inext +3;
               ring_next := info(inext-2)+1;
           end if;
         end if;
      End if;
   End Loop;


   if next*2 <> n then
      result := 'Y';
      Xys.trim(n-next*2);
      geometry.sdo_ordinates := Xys;
      geometry.sdo_elem_info := Info;
   end if;

   RETURN result;
END;
--
FUNCTION COMPUTE_QA_VALUES_2007( geo_id VARCHAR2, Ugeometry  MDSYS.SDO_GEOMETRY, Ggeometry MDSYS.SDO_GEOMETRY, GenColumn VARCHAR2, compare_table_name VARCHAR2,target_scale NUMBER,parea_check NUMBER default NULL, shape_check NUMBER default 1.5) RETURN NUMBER AS
/*
********************************************************************************
--Program Name: Compute_QA_Values_2007
--Author: Sidey Timmins
--Creation Date: 3/23/2011

  -- Call this function from inside another PL/SQL program.  This function
  -- has 8 required parameters:
  --
  --      INPUT
  --      geo_id              = the geography id
  --      Ugeometry           - the ungeneralized 2007 geometry
  --      GGeometry           - the generalized geometry
  --      Gencolumn           - the column name for the generalized geometry in the output table
  --      compare_table_name  - the output table name to fill with QA values
  --      target_scale        - the target scale (used to check whether the area
  --                            changes are significant
  --      area_check          - a length in inches that will be squared and
  --                            converted to square meters at the target scale.
  --                            When the generalized area is less than the
  --                            ungeneralized area by this amount, the QA_flag is set.
  --      shape_check         - A threshold for the shape index (usually 1.5)
--
-- Purpose: Compute fundamental measures of polygon fidelity: area, perimeter,
-- the MBR, the lengths and attitudes of the major and minor axes and sets a
-- QA_FLAG when there are significant differences.

-- Calls: Measure_Elongate_Length
********************************************************************************
*/
   axes              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   gaxes             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   gradii            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   uradii            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   uring_nos         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   UMBR              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   GMBR              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   outer_rings       MDSYS.SDO_LIST_TYPE;
   xys               mdsys.sdo_ordinate_array;
   geometry          MDSYS.SDO_GEOMETRY;
   ungeom            MDSYS.SDO_GEOMETRY;
   gengeom           MDSYS.SDO_GEOMETRY;
   NULL_geom         MDSYS.SDO_GEOMETRY;
   gXys              MDSYS.SDO_ORDINATE_ARRAY;
   uXys              MDSYS.SDO_ORDINATE_ARRAY;
   UInfo             MDSYS.SDO_ELEM_INFO_ARRAY;
   gInfo             MDSYS.SDO_ELEM_INFO_ARRAY;
   kount             PLS_INTEGER;
   garea             NUMBER;
   uarea             NUMBER;
   gshape            NUMBER;
   ushape            NUMBER;
   uangle            NUMBER;
   gangle            NUMBER;
   gmajor            NUMBER;
   umajor            NUMBER;
   gminor            NUMBER;
   uminor            NUMBER;
   ulen              NUMBER;
   glen              NUMBER;
   gshape_index      NUMBER;
   ushape_index      NUMBER;
   shape_fidelity    NUMBER;
   area_ratio        NUMBER;
   area_check        NUMBER := parea_check;
   QA_FLAG           NUMBER;

   xc                NUMBER;
   yc                NUMBER;
   boyce_shape       NUMBER :=-1.;
   gboyce_shape      NUMBER :=-1.;
   temp              NUMBER;
   angle             NUMBER;
   bad_ring          NUMBER;
   outer_ring        NUMBER;
   try_area_ratio    NUMBER;
   bad_area_ratio    NUMBER;
   ring              NUMBER;
   gring             NUMBER;
   usum_now          NUMBER;

   grings            PLS_INTEGER;
   urings            PLS_INTEGER;
   sql_stmt3         VARCHAR2(4000);
BEGIN

    sql_stmt3 := 'INSERT INTO ' || Compare_table_name || ' VALUES(:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20,:21,:22,:23,:24,:25,:26,:27,:28,:29,:30)';

-- For a mulitpolygon (2007) check each polygon for the generalized against its
-- original 2003 polygon
    bad_ring := 1.0;
    area_ratio := 0.0;
    usum_now :=0.0;
    urings := Getnumrings(Ugeometry,'outer');
    uring_nos.extend(urings);
    for jj in 1..urings loop
      uring_nos(jj) := 0.;
    end loop;
    grings := Getnumrings(Ggeometry,'outer');
    bad_area_ratio := 1000.; -- just in case generalized area > ungen area
    UXys := Ugeometry.sdo_ordinates;
    UInfo := Ugeometry.sdo_elem_info;
    UMBR.trim(UMBR.count);
    ring := 1;
    for jj in 1..grings loop

        gring := ring;
        gengeom := GET_OUTER_RING(Ggeometry,ring);

         garea := sdo_geom.sdo_area(gengeom,0.05,'unit=sq_meter');
         garea := ROUND(garea,4);
         glen := ROUND(sdo_geom.sdo_length(gengeom,0.05,'unit=meter'),4);
         gXys := gengeom.sdo_ordinates;

         outer_rings := find_xes_and_yes(gXys,UXys,Uinfo,UMBR);
         outer_ring := outer_rings(1);
         qa_flag := NULL;
--   dbms_output.put_line('OR ' || outer_ring || ' K ' || uring_nos.count);
         if outer_ring <> 0. then
             uring_nos(outer_ring) := outer_ring;
             geometry := SDO_util.extract(Ugeometry,outer_ring,0);
             ulen := ROUND(sdo_geom.sdo_length(geometry,0.05,'unit=meter'),4);
-- In very rare circumstances 2 ungeneralized rings can become just 1 generalized
-- ring. A case was found with geo_id:='1600000US5336045' in Z653CL_FSL160v
-- where the ungeneralized has 3 rings, of which two were a figure of 8.
-- So find_xes_and_yes got the wrong ring
             if ulen < 0.5 * glen and outer_rings.count > 1 then
               outer_ring := outer_rings(2);

               geometry := SDO_util.extract(Ugeometry,outer_ring,0);
               ulen := sdo_geom.sdo_length(geometry,0.05,'unit=meter');
             end if;
--             dbms_output.put_line('calling measure elongate with ' || geometry.sdo_ordinates.count || ' oring ' || outer_ring);
             axes := measure_elongate_length(geometry,xc,yc,uradii,geo_id);
             uarea := sdo_geom.sdo_area(geometry,0.05,'unit=sq_meter');

             uarea := ROUND(uarea,4);
             uangle := ROUND(axes(1),4);
             umajor := ROUND(axes(2),4);
             uminor := ROUND(axes(3),4);

             gaxes := measure_elongate_length(gengeom,xc,yc,gradii,geo_id);
             try_area_ratio := ROUND(garea/uarea,6);
             gangle := ROUND(gaxes(1),4);
             gmajor := ROUND(gaxes(2),4);
             gminor := ROUND(gaxes(3),4);

--             dbms_output.put_line('jj ' || jj || ' garea ' || garea || ' gangle ' || gangle || ' uarea ' || uangle || ' outer ' || outer_ring);
-- We don't care about the bearing of the line, if one axis goes west and the
-- other east, it is the same axis.

-- This seems to get it
             if uangle > 170 and gangle <10. then
                 gangle := gangle + 180.;
             elsif gangle > 170. and uangle < 10. then
                 uangle  := uangle + 180.;
             end if;

-- But this is better
            if abs(uangle-gangle) > 90 and uangle < gangle then
                uangle := uangle + 180.;
                if uangle >= 360. then
                   uangle := uangle -360.;
                end if;
            elsif abs(uangle-gangle) > 90 and gangle < uangle then
                gangle := gangle + 180.;
                if gangle >= 360. then
                  gangle := gangle -360.;
                end if;
            end if;
--dbms_output.put_line('u ' || uangle || ' g  ' || gangle);
-- Flip major and minor when the difference in the angles > 79 degrees
-- then the orientations are presumed to be the same
            if abs(uangle-gangle) > 79.  and gmajor < gminor then
               if uangle < gangle then
                 gangle := gangle -90.;
               else
                 gangle := gangle +90.;
               end if;
              temp := gmajor;
              gmajor := gminor;
              gminor := temp;
           end if;
--dbms_output.put_line('u ' || uangle || ' g  ' || gangle);

-- Compute Chorley and Hagget's Shape index using the Major axis (elongated length):
--                  4/pi A/L^2   where L = Major axis length

          gshape := ROUND(1.273239544*garea/(gmajor*gmajor),6);
          ushape := ROUND(1.273239544*uarea/(umajor*umajor),6);

-- Set the QA FLAG

-- This index weights both the area differences and the attitude differences
-- of the major axis computed from the 2nd moments of inertia. So the area difference
-- is grounded in geometry and the attitude difference in physics.

-- Compute Sideys shape index: 1 + ABS(1-area_generalized/area_ungeneralized) +
--                             ABS(ungeneralized_attitude-generalized_attitude)/100.
--
--      dbms_output.put_line('area check ' || round(area_check,4) || 'try ' || try_area_ratio || ' abs ' || round(abs(1.-try_area_ratio)*uarea,4));
              shape_fidelity := 1.+ROUND(abs(1.-try_area_ratio) + 0.02*abs(uangle-gangle) +ABS(gshape-ushape),4);
              if shape_fidelity > shape_check and abs(1.-try_area_ratio)*uarea > area_check then
                  qa_flag := gring;
              end if;

              execute immediate sql_stmt3 using geo_id,qa_flag,shape_fidelity,1.,garea,uarea,glen,ulen,try_area_ratio,1.,gshape,ushape,gboyce_shape,boyce_shape,gmajor,umajor,gminor,uminor,gangle,uangle,0.,
                       gaxes(4),gaxes(5),gaxes(6),gaxes(7),axes(4),axes(5),axes(6),axes(7),gengeom;

             area_ratio := area_ratio + garea;   -- sum generalized areas
             usum_now := usum_now + uarea;        -- and their corresponding ungen areas
             if try_area_ratio < bad_area_ratio then
                bad_ring := gring;  -- Tell user the ungeneralized ring number
                bad_area_ratio := try_area_ratio;   -- save worst area ratio
--              dbms_output.put_line('area ratio ' || try_area_ratio || ' ring ' || bad_ring || ' ' || geo_id);
             end if;
           end if;
        end loop;

        gXys := Ggeometry.sdo_ordinates;
        gInfo := Ggeometry.sdo_elem_info;
        gMBR.trim(gMBR.count);

        For outer_ring in 1..urings loop
-- This is a ncessary condition
          if uring_nos(outer_ring)  = 0.0 then

-- But it is not  sufficient since the Line simplified input may have combined
-- rings. So if any coordinates match the ungeneralized then this ring has been processed
             geometry := SDO_util.extract(Ugeometry,outer_ring,0);
             uXys := geometry.sdo_ordinates;
             outer_rings := find_xes_and_yes(uXys,GXys,ginfo,GMBR);
-- So these coordinates were not found..

             if outer_rings(1) = 0 then
               qa_flag := NULL;
               ulen := ROUND(sdo_geom.sdo_length(geometry,0.05,'unit=meter'),4);
               axes := measure_elongate_length(geometry,xc,yc,uradii,geo_id);
               uarea := sdo_geom.sdo_area(geometry,0.05,'unit=sq_meter');
               uarea := ROUND(uarea,4);
               if uarea > area_check then
                 qa_flag := -outer_ring;
               end if;
               uangle := ROUND(axes(1),4);
               ushape := ROUND(1.273239544*uarea/(umajor*umajor),6);

-- For geometries which perhaps should have been generalized, set shape_fidelity
--to the threshold: shape_check
               if qa_flag is not NULL then
                  execute immediate sql_stmt3 using geo_id,qa_flag,shape_check,1.,0.0,uarea,0.0,ulen,0.0,1.,0.0,ushape,-1.,-1.,0.0,umajor,0.0,uminor,0.0,uangle,0.,
                       0.0,0.0,0.0,0.0,axes(4),axes(5),axes(6),axes(7),geometry;
               else
-- Nothing to compare it to and it is small at the target scale.
                  execute immediate sql_stmt3 using geo_id,qa_flag,0.0,1.,0.0,uarea,0.0,ulen,0.0,1.,0.0,ushape,-1.,-1.,0.0,umajor,0.0,uminor,0.0,uangle,0.,
                       0.0,0.0,0.0,0.0,axes(4),axes(5),axes(6),axes(7),NULL_Geom;
               end if;
             end if;
          end if;
        End Loop;
        commit;
        if usum_now > 0.0 then
          area_ratio := ROUND(area_ratio/usum_now,6);      -- so we can compute a weighted area_ratio
          RETURN area_ratio;
        else
          RETURN 0.;
        end if;


END COMPUTE_QA_VALUES_2007;
--
FUNCTION CHECK_AND_RECTIFY(Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,tolerance NUMBER default 0.05) RETURN VARCHAR2 AS
-- Check and verify a geometry, returns TRUE when its ok or 13356, 13349 when it
-- is not ok. Written as a procedure so as to just update the existing geometry;

   OK           VARCHAR2(5) := 'TRUE';
   Msg          VARCHAR2(2000) :='FALSE';
   loops        PLS_INTEGER := 0;
BEGIN
   While loops < 4 loop
     loops := loops + 1;
     Msg := sdo_geom.validate_geometry_with_context(Geometry,0.05);
     if SUBSTR(msg,1,4) <> 'TRUE' then
        Geometry := sdo_util.rectify_geometry(Geometry,tolerance);
     else
        RETURN Ok;
     end if;
   End Loop;
   RETURN SUBSTR(Msg,1,5);
END;
--
FUNCTION OLDFIND_XES_AND_YES(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,UXYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Uinfo MDSYS.SDO_ELEM_INFO_ARRAY,round_it NUMBER default 6) RETURN MDSYS.SDO_LIST_TYPE AS

--Author: Sidey Timmins
--Creation Date: 3/16/2011
--Updated: 3/23/2011 To get the outer ring right when there are holes
-- Find 2 pairs of coordinates (2 vertices) from one outer ring in another
-- 2007 geometry's coordinates and return the outer ring number (NOT the ring
-- itself) in the 2nd geometry (the outer ring --can used to call sdo_util.extract).
-- Designed just for line generalization where the Xys always came from the
-- Ungeneralized Xys with the only difference being rounding.

  j           PLS_INTEGER := 2;
  k           PLS_INTEGER;
  n           PLS_INTEGER := TRUNC(Xys.count/2);
  m           PLS_INTEGER;
  mring       PLS_INTEGER := TRUNC(Uinfo.count/3);
  istart      PLS_INTEGER;
  iend        PLS_INTEGER;
  kount       PLS_INTEGER;
  confirm     PLS_INTEGER :=0;
  x           NUMBER;
  y           NUMBER;
  x2          NUMBER;
  y2          NUMBER;
  epsilon     NUMBER := 1.E-6;
  found       NUMBER := 0.0;
  outer_ring  NUMBER;
BEGIN

-- Straightforward search for XY values in another coordinate array
    m := n-2;
    if Mod(m,2) = 1 then
      m := m + 1;
    end if;

    For ii in 1..n-2 Loop -- We skip the 1st vertex (node)
      j := j + 2;
      m := m + 2;
      x := ROUND(Xys(j-1),round_it);
      y := ROUND(Xys(j),round_it);
-- Check a coordinate near the middle too
      if m < Xys.count then
        x2 := ROUND(Xys(m-1),round_it);
        y2 := ROUND(Xys(m),round_it);
      end if;
      outer_ring := 0.0;


-- Search all outer (1003) loops
      FOR loops in 1..mring LOOP
        istart := loops*3+1;

        If UInfo(istart-2) = 2003 Then   -- Ignore the holes (2003)
          NULL;
        Else
-- Setup to set the start and end coordinates for each ring
          outer_ring := outer_ring+1;
          if loops*3 = UInfo.count then
            iend := UXys.count-2;
          else
            iend := UInfo(istart)-3;
          end if;
          k := UInfo(istart-3);       -- Start at start of ring     --
          kount := TRUNC((iend-k+1)/2);  -- Ignore last vertex (same as start)

          For kk in 1..kount Loop
             k := k + 2;  -- But we ignore 1st vertex since it is at a node
-- Zone rounded the coordinates so rounding makes more sense
--             if (ABS(UXys(k) -x) < epsilon and ABS(UXys(k+1)- y) < epsilon) or
--                (ABS(UXys(k)- x2)< epsilon and ABS(UXys(k+1)-y2) < epsilon) then
                if (ROUND(UXys(k),round_it) =x and ROUND(UXys(k+1),round_it)= y)  then
                  confirm := confirm + 1;
                end if;
                if (ROUND(UXys(k),round_it)= x2 and ROUND(UXys(k+1),round_it)=y2) then
                  confirm := confirm + 10000;
                end if;
                if (ROUND(UXys(k),round_it) =x and ROUND(UXys(k+1),round_it)= y) or
                    (ROUND(UXys(k),round_it)= x2 and ROUND(UXys(k+1),round_it)=y2) then
                -- We have found 1 vertex of the generalized polygon

                if found <> 0.0 then
-- We have found > 2 vertices of the generalized polygon
                    if found <> outer_ring and confirm > 10000 then
                      RETURN MDSYS.SDO_LIST_TYPE(outer_ring,found);
                    else
                      RETURN MDSYS.SDO_LIST_TYPE(found);
                    end if;
               end if;
               found := outer_ring;
               end if;
          End Loop;
        End If;
      End Loop;
    END LOOP;

    RETURN MDSYS.SDO_LIST_TYPE(0.0);


END OLDFind_Xes_and_Yes;
--
PROCEDURE CHECK_ALL_VERTICES( pInTable VARCHAR2, pGeometry_column VARCHAR2, pInTable2 VARCHAR2, pnewGeometry_column VARCHAR2, pOutput_table VARCHAR2, match_length PLS_INTEGER default 4, area_check NUMBER, percent_ungen NUMBER default 20., max_consecutive_match NUMBER default 20., pInclude_state_edges VARCHAR2 default 'NO', pprint VARCHAR2 default 'NO') AS
/**
--##############################################################################
--Program Name: Check_all_vertices
--Author: Sidey Timmins
--Creation Date:
--Updates: 08/05/2011 Code to write QA_flag ='Y' when an edge could have been
--                    generalized and is not visible at target scale.
--                    (However, the edge may not have been generalizable because
--                    another edge is in the way. Package GZ_QA has no knowledge
--                    of nearby edges in the way.)
--Usage:
--  This PL/SQL procedure has 5 required parameters:
--     PinTable:          -  an input table containing an ungeneralized
--                           geometry to check. This table must also contain
--                           an EDGE_ID column and LEFT_FACE_ID and
--                           RIGHT_FACE_ID columns.
--     pGeometry_column   -  the ungeneralized sdo_geometry column
--     PinTable2:         -  a second table to check (or the same as 1st one)
--     pnewGeometry_column - a generalized sdo_geometry column in the 2nd table
--     pOutput_Table:     -  a table name to create and write a report to.
--                           If it already exists it wil be dropped.
--     match_length       -  When this number of consecutive vertices are the
--                           same, an ungeneralized section is deemed to exist.
--     area_check         -  the minimum area in an ungeneralized "L" shaped portion
--                            of an edge which is allowed to be ungeneralized because
--                            removal of the L vertex will remove area_check square meters.

--     pInclude_state_edges -  'NO' does not include them, anything else does
--     pprint:             -   whether to print the matching section(s) for each edge_id
--
--Purpose: This procedure checks edges before and after generalization looking
--         for sections of consecutive ungeneralized vertices that match. An
--         ungenralized section is deemed to exist when 4 (the match length) or
--         more consecutive vertices are unchanged. This procedure generates
--         a report and a table describing these sections.
--
--Method: Compares input and output geometries.
--Dependencies: Compare_Edge_Coordinates
--##############################################################################
*/

    TYPE TblCursorType IS REF CURSOR;
    TYPE GEOM_ARRAY  IS  VARRAY(1048576) OF MDSYS.SDO_GEOMETRY;

    Table_cursor         TblCursorType;
    Geometries           GEOM_ARRAY := GEOM_ARRAY();
    New_Geometries       GEOM_ARRAY := GEOM_ARRAY();
    matches              MDSYS.SDO_LIST_TYPE;

    XYOrd                MDSYS.SDO_ORDINATE_ARRAY;
    NewXYOrd             MDSYS.SDO_ORDINATE_ARRAY;
    Info_Array           MDSYS.SDO_ELEM_INFO_ARRAY;
    Edge_ids             MDSYS.SDO_LIST_TYPE;
    Bad_matches          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

    geometry             MDSYS.SDO_GEOMETRY;

    InTable               VARCHAR2(30) := UPPER(pInTable);
    InTable2              VARCHAR2(30) := UPPER(pInTable2);
    Output_Table          VARCHAR2(30) := UPPER(pOutput_Table);
    Geometry_column       VARCHAR2(30) := UPPER(pGeometry_column);
    NewGeometry_column    VARCHAR2(30) := UPPER(pnewGeometry_column);
    Include_state_edges   VARCHAR2(3) := UPPER(pInclude_state_edges);
    sql_stmt              VARCHAR2(4000);
    sql_stmt2             VARCHAR2(4000);

    row_limit             NUMBER := 100;
    GTYPE                 NUMBER;
    new_GTYPE             NUMBER;
    sum_bad               NUMBER;
    avg_bad               NUMBER;
    median_bad            NUMBER;
    max_bad               NUMBER;
    bad_count             NUMBER;
    state_edge_count      NUMBER;
    state_vertex_count    NUMBER;
    vertices              NUMBER;
    max_area              NUMBER;
    max_new_area          NUMBER;
    qa_count              NUMBER;
    ptr                   PLS_INTEGER := 0;
    bad_match_sections    PLS_INTEGER := 0;  -- 3 or more vertices the same
    bad_edge_count        PLS_INTEGER := 0;
    bad_excluding_count   PLS_INTEGER := 0;
    whole_edge_count      PLS_INTEGER := 0;
    Total_bad             PLS_INTEGER := 0;
    Total_match_count     PLS_INTEGER := 0;
    match_count           PLS_INTEGER;
    ungen_count           PLS_INTEGER := 0;
    two_count             PLS_INTEGER := 0;
    three_count           PLS_INTEGER := 0;
    four_count            PLS_INTEGER := 0;
    five_count            PLS_INTEGER := 0;
    kount                 PLS_INTEGER := 0;
    bad_length            PLS_INTEGER := match_length;
    decim_digits          PLS_INTEGER := 6;
    print                 VARCHAR2(5) := UPPER(pprint);
    status                VARCHAR2(100);
    qa_flag               VARCHAR2(4);
    percent              NUMBER;
    bad                   NUMBER;
    area_diff             NUMBER;
    way_point             NUMBER;
    edge_len              NUMBER;

    closed                BOOLEAN;
    once                  BOOLEAN;
    written               BOOLEAN;
BEGIN

   if bad_length < 3 then
     bad_length := 3;
   end if;

   sql_stmt := 'SELECT count(1),sum(sdo_util.getnumvertices(' || Geometry_Column  ||
              ')) from '|| InTable || ' where (left_face_id=-1 or right_face_id=-1)';
   execute immediate sql_stmt into state_edge_count,state_vertex_count;

   if Intable = InTable2 then
     sql_stmt := 'SELECT a.Edge_id,a.' ||Geometry_Column  ||',a.' || NewGeometry_Column  ||
          ' FROM '|| InTable || ' a ';
     if Include_state_edges = 'NO' then
      sql_stmt := sql_stmt || ' WHERE NOT(a.left_face_id=-1 or a.right_face_id=-1)';
     end if;
   else
      sql_stmt := 'SELECT a.Edge_id,a.' ||Geometry_Column  ||',b.' || NewGeometry_Column  ||
          ' FROM '|| InTable || ' a,' || InTable2 ||' b  where a.edge_id=b.edge_id ';
      if Include_state_edges = 'NO' then
        sql_stmt := sql_stmt || ' and NOT(a.left_face_id=-1 or a.right_face_id=-1)';
      end if;
   end if;

   sql_stmt := sql_stmt || ' order by a.edge_id';

   OPEN Table_cursor FOR  sql_stmt;
   if Include_state_edges = 'NO' then
     dbms_output.put_line('This report does not include State boundary edges');
   else
     dbms_output.put_line('This report does include State boundary edges');
   end if;
   dbms_output.put_line('However the edge count and percentages are based on total edges and vertices');
   dbms_output.put_line('..');
   dbms_output.put_line('Ungeneralized edges (if any) with > 5 vertices follow:');

-- Make an output table to report each edge
   If Table_Exists(Output_Table) = TRUE then
     execute immediate 'DROP TABLE ' || Output_Table;
   end if;
   execute immediate 'CREATE TABLE ' || Output_Table ||' (EDGE_ID NUMBER,QA_FLAG VARCHAR2(4), PERCENT_UNGEN NUMBER,STATUS VARCHAR2(100),LENGTH NUMBER,MATCHING_VERTICES NUMBER,SECTIONS NUMBER,VERTEX_COUNT NUMBER, ORIG_VERTEX_COUNT NUMBER) NOLOGGING';
   sql_stmt2 := 'INSERT INTO ' || Output_Table ||' VALUES(:1,:2,:3,:4,:5,:6,:7,:8,:9)';

  LOOP

   FETCH Table_cursor BULK COLLECT INTO Edge_Ids,Geometries,New_Geometries LIMIT Row_limit;
--   dbms_output.put_line('count ' || Geometries.count);
     EXIT WHEN Geometries.COUNT = 0;
     kount := kount + geometries.count;
     if kount = 0 then
      Bad_matches.extend(kount);
     end if;

     For ij in 1..Geometries.COUNT Loop
--       if edge_ids(ij) = 49932then -- =68985 then
       geometry := Geometries(ij);
       Gtype:= geometry.SDO_GTYPE;
-- Detect dups because most edges are ok
--dbms_output.put_line('ID ' || edge_ids(ij));
       Info_Array := geometry.SDO_ELEM_INFO;
       XYOrd := geometry.SDO_Ordinates;
       closed := FALSE;
       if XYOrd(1) = XYord(XYord.count-1) and XYOrd(2) = XYord(XYord.count) then
          closed := TRUE;
       end if;
--       if edge_ids(ij) >= 64000  then
--        dbms_output.put_line( edge_ids(ij));
--        end if;
       ungen_count := ungen_count + Xyord.count;
       newXYOrd := new_geometries(ij).SDO_Ordinates;
       new_Gtype:= geometries(ij).SDO_GTYPE;
       edge_len := ROUND(Perimeter(newXYord),3);

--     Compare the 2 edge geometries and look for matching sections of coordinates
       bad_match_sections :=0;
       If gtype = 2002 and new_gtype = 2002 then
          matches := Compare_Edge_Coordinates(newXYOrd,XYOrd,match_count,bad_match_sections,decim_digits,bad_length);
--          dbms_output.put_line('M ' || matches(1) || ' matches ' || matches(3));

          Total_bad := Total_bad + bad_match_sections;
          qa_flag := NULL;
        if newXyOrd.count = XyOrd.count then

-- Next 5 lines are NEW code to turn off bad matching sections when
-- "L" shaped portions of the line are necessary to preserve visible areas
         if NOT closed then
           max_new_area := AREA_OF_LINE(NewXYOrd);
--           max_area := AREA_OF_LINE(XYOrd);
         else
           way_point := NULL;
           max_new_area := AREA_OF_LOOP(NewXYOrd,TRUE,way_point,8265.);
--           max_area := AREA_OF_LOOP(XYOrd);
         end if;

         if max_new_area = 0.0 then
           area_diff := area_check;
         else
           area_diff := max_new_area;
         end if;
         qa_flag := 'Y';

         if area_diff >= area_check and XYOrd.count > 4 then -- (match_count/2) <= bad_length then
 --          dbms_output.put_line('ID ' || edge_ids(ij) || ' Area ' || round(max_new_area,3) || ' area check ' || round(area_check,3));
           bad_match_sections := 0;
           qa_flag := NULL;
         elsif XyOrd.count = 4 then
            two_count := two_count + 1;
         elsif XyOrd.count = 6 then
            three_count := three_count + 1;
         elsif XyOrd.count = 8 then
            four_count := four_count + 1;
         elsif XyOrd.count = 10 then
            five_count := five_count + 1;
         end if;
--         if bad_match_sections <> 0 then
--           dbms_output.put_line('ID ' || edge_ids(ij) || ' Area ' || round(max_new_area,3) || ' area check ' || round(area_check,3));
--         end if;
        end if;
        vertices := XYOrd.count/2;
        written := FALSE;
        if bad_match_sections > 0 then
         if XYord.count > 10 and newXyOrd.count = XyOrd.count then
           status := 'Ungeneralized';
--           dbms_output.put_line('ID ' || edge_ids(ij) || ' vertex count ' || (match_count/2) || ' original ' || (xyord.count/2) || ' sections ' ||bad_match_sections);
           written := TRUE;
           execute immediate sql_stmt2 using edge_ids(ij),'Y',100.,status,edge_len,match_count/2,1,vertices,vertices;
--   for ii in 1..match_count loop
--     dbms_output.put_line('M ' || matches(ii));
--   end loop;
         end if;
        end if;

            ptr := 0;
            once := TRUE;
            bad := 0.0;
            While ptr < match_count loop
              ptr := ptr + 1;
              if matches(ptr) > 0 then
                bad_count := (matches(ptr) - TRUNC(matches(ptr))) * 1000000.;
                if once and bad_count >= bad_length then
                  bad_edge_count := bad_edge_count +1;
--                 dbms_output.put_line('ID ' || edge_ids(ij));
                  if bad_edge_count > bad_matches.count then
                    bad_matches.extend(row_limit);
                  end if;
                  bad_matches(bad_edge_count) := 0.0;
                  once := FALSE;
                end if;
                if bad_count > 1 then
                   if bad_count >= bad_length then
                     bad_matches(bad_edge_count) := bad_matches(bad_edge_count) + bad_count;
                     bad := bad + bad_count;
-- We might want to exclude the end points of an edge since they cannot be generalized
                     bad_excluding_count := bad_excluding_count + bad_count;
                     if TRUNC(matches(ptr)) = 1 then
                        bad_excluding_count := bad_excluding_count -1;
                        if abs(matches(ptr+bad_count*2-1)) +1 = XyOrd.count then
                          bad_excluding_count := bad_excluding_count -1;
                        end if;
                     end if;
                   end if;
                   ptr := ptr + (bad_count*2) -2;
                end if;
              end if;
              ptr := ptr + 1;
            end loop;

            if once = FALSE then
              qa_flag := NULL;
              percent := round(100.* bad/(XYOrd.count*0.5),1);
              if percent < 100. then
                status := 'Portion ungeneralized';
              else
                status := 'Ungeneralized';
              end if;
              if written = FALSE then
              execute immediate sql_stmt2 using edge_ids(ij),qa_flag,percent,status,edge_len,bad,bad_match_sections,match_count/2,vertices;
              end if;
            end if;
          end if;

          Total_match_count := total_match_count + newXYOrd.count;
          if newXYord.count = XYord.count and XYord.count > 4 then
             whole_edge_count := whole_edge_count + 1;
          end if;
--     end if;
    End Loop;
  END LOOP;

  dbms_output.put_line('.');

  if two_count > 0 and bad_length = 2 then
    dbms_output.put_line('Edges with only 2 vertices that were not generalized ' || two_count);
  end if;
  if three_count > 0  and bad_length <=3 then
    dbms_output.put_line('Edges with only 3 vertices that were not generalized ' || three_count);
  end if;
  if four_count > 0  and bad_length <= 4 then
    dbms_output.put_line('Edges with only 4 vertices that were not generalized ' || four_count);
  end if;
  if five_count > 0  and bad_length <= 5 then
    dbms_output.put_line('Edges with only 5 vertices that were not generalized ' || five_count);
  end if;
  bad_matches.trim(bad_matches.count-bad_edge_count);

  execute immediate 'select sum(column_value) from TABLE(:1)' into sum_bad using bad_matches;
  execute immediate 'select median(column_value) from TABLE(:1)' into median_bad using bad_matches;
  execute immediate 'select avg(column_value) from TABLE(:1)' into avg_bad using bad_matches;
  execute immediate 'select max(column_value) from TABLE(:1)' into max_bad using bad_matches;

  commit;
  ungen_count := ungen_count/2 + state_vertex_count;
  kount := kount + state_edge_count;
  Total_match_count := Total_match_count*0.5;
  dbms_output.put_line('Edge count ' || kount);
  dbms_output.put_line('>> Generalized vertex count ' || (Total_match_count+state_vertex_count) || ', Ungeneralized vertex count ' || ungen_count || ' compression: ' || round(ungen_count*1.0/Total_match_count,3));
  dbms_output.put_line('This procedure looks for sections with '||bad_length ||' or more consecutive vertices in the generalized edge matching the ungeneralized edge.');
  dbms_output.put_line('.');
  dbms_output.put_line('>> A Total of ' ||Total_bad || ' Matching sections occur in ' || bad_edge_count  || ' edges i.e. ' || round(100.*bad_edge_count/kount,2)  || ' percent of all edges');
  dbms_output.put_line('>> Total number of Matching vertices ' || sum_bad || ' i.e. '||round(100.*sum_bad/(Total_match_count+state_vertex_count),2) || ' percent (of generalized vertex count)');
  dbms_output.put_line('>> Total number of Matching vertices (excluding nodes) ' || bad_excluding_count);
  dbms_output.put_line('>> Median Matching section length (vertices) ' || median_bad);
  dbms_output.put_line('>> Average Matching section length (vertices) ' || round(avg_bad,2));
  dbms_output.put_line('>> Maximum Matching section length (vertices) ' || max_bad);
  dbms_output.put_line('>> Ungeneralized edges (more than 2 vertices) ' || whole_edge_count || ' i.e. ' || round(100.*whole_edge_count/kount,2)  || ' percent of all edges');
  dbms_output.put_line('>> Ungeneralized STATE edges  ' || state_edge_count || ' i.e. ' || round(100.*state_edge_count/kount,2)  || ' percent of all edges');


---------------------------------------------------------------------------------------------------------
-- How successful was the generalization?

  execute immediate 'UPDATE '|| Output_Table ||' set QA_FLAG = ''Y'' where PERCENT_UNGEN>:1 AND ' ||
                       'MATCHING_VERTICES>:2' using percent_ungen,max_consecutive_match;
  commit;
  execute immediate 'SELECT count(1) from '|| Output_Table ||' WHERE QA_FLAG is not NULL' into qa_count;
  if qa_count <> 0 then
     dbms_output.put_line('...');
     dbms_output.put_line('>>>There were '||qa_count || ' edges flagged.<<<');
  else
     dbms_output.put_line('...');
     dbms_output.put_line('There were NO edges flagged.');
  end if;


END Check_All_Vertices;
--
PROCEDURE FLAG_AFFECTED_ENTITIES(pReport_Table VARCHAR2,pTopology VARCHAR2,pSchemaName VARCHAR2,pBad_Entities_table VARCHAR2 default NULL) AS

-- Updated: Nov 17,2011 to handle "No data found error".

-- A Procedure to flag affected polygon entities in FSL tables when an edge is
-- deemed ungeneralized or partly (insufficiently) ungeneralized.

   TYPE VCHAR30_ARRAY  IS VARRAY(100000) of VARCHAR2(30);
   TYPE VCHAR100_ARRAY IS VARRAY(100000) of VARCHAR2(100);
   bad_edges    MDSYS.SDO_LIST_TYPE;
   bad_edge     NUMBER;
   left_face    NUMBER;
   right_face   NUMBER;
   face         NUMBER;
   tolerance    NUMBER:=0.05;
   kount        PLS_INTEGER;

   Geom                MDSYS.SDO_GEOMETRY;
   Edge_Geom           MDSYS.SDO_GEOMETRY;
   Intersection        MDSYS.SDO_GEOMETRY;

   Report_Table        VARCHAR2(30) := UPPER(pReport_table);
   Bad_Entities_Table  VARCHAR2(30) := UPPER(pBad_Entities_table);
   SchemaName          VARCHAR2(30) := UPPER(pSchemaName);
   Topology            VARCHAR2(30) := UPPER(pTopology);
   Relation_Table      VARCHAR2(100);
   Face_table          VARCHAR2(100);
   Edge_table          VARCHAR2(100);
   Table_Names         VCHAR30_ARRAY;
   Statuses            VCHAR100_ARRAY;
   Geo_Id              VARCHAR2(100);
   sql_stmt            VARCHAR2(4000);
   sql_stmt2           VARCHAR2(4000);
   sql_stmt3           VARCHAR2(4000);
   statuz              VARCHAR2(100);
   check_it            VARCHAR2(2000);

BEGIN

  Relation_Table := SchemaName || '.'||Topology ||'_RELATION$ ';
  Face_Table := SchemaName || '.'||Topology ||'_FACE$ ';
  Edge_Table := SchemaName || '.'||Topology ||'_EDGE$ ';

  sql_stmt := 'SELECT EDGE_ID,Status from '|| Report_Table;
  EXECUTE IMMEDIATE sql_stmt BULK COLLECT into Bad_edges,Statuses;

  if Bad_Entities_Table is NOT NULL and not Table_exists(Bad_Entities_Table) then
    sql_stmt := 'CREATE TABLE '|| Bad_Entities_Table || ' (EDGE_ID NUMBER, GEO_Id VARCHAR2(100), ENTITY VARCHAR2(30),STATUS VARCHAR2(100))';
     EXECUTE IMMEDIATE sql_stmt;
  end if;



  For ii in 1..Bad_edges.count Loop
    bad_edge := Bad_Edges(ii);

    sql_stmt := 'SELECT Geometry from '||Edge_Table || ' WHERE EDGE_ID=:1';
    EXECUTE IMMEDIATE sql_stmt into Edge_Geom using bad_edge;

    statuz   := Statuses(ii);
    sql_stmt := 'SELECT LEFT_FACE_ID,RIGHT_FACE_ID FROM ' || Topology||'_EDGE$ WHERE EDGE_ID=:1';
    EXECUTE IMMEDIATE sql_stmt into left_face,right_face using bad_edge;

--      dbms_output.put_line('edge_id ' || bad_edge);

    sql_stmt := 'SELECT TABLE_NAME from all_sdo_topo_metadata ' ||
                  ' where owner = '''|| SchemaName ||'''' ||
                   ' and topology = ''' || Topology ||''' and tg_layer_id IN ' ||
                   ' (select distinct rr.tg_layer_id from '||
                   Relation_table || ' r,' ||
                   Relation_table || ' rr where (r.topo_id =:1 or r.topo_id =:2)' ||
                   ' and rr.topo_id = r.tg_layer_id and rr.topo_type = r.tg_id)';

   EXECUTE IMMEDIATE sql_stmt BULK COLLECT into Table_Names using left_face,right_face;


    For jj in 1..Table_names.count Loop


      if NOT field_exists(Table_Names(jj),'QA_FLAG', SchemaName) then
--      dbms_output.put_line('sql ' ||'ALTER TABLE ' || Table_names(jj) || ' ADD  (QA_FLAG VARCHAR2(4))');
          EXECUTE IMMEDIATE 'ALTER TABLE ' || Table_names(jj) || ' ADD (QA_FLAG VARCHAR2(4))';
      end if;
      if NOT field_exists(Table_Names(jj),'STATUS', SchemaName) then
--      dbms_output.put_line('sql ' || 'ALTER TABLE ' || Table_names(jj) || ' ADD (STATUS VARCHAR2(100))');
          EXECUTE IMMEDIATE 'ALTER TABLE ' || Table_names(jj) || ' ADD (STATUS VARCHAR2(100))';
      end if;



     for kk in 1..2 loop
      sql_stmt3 := 'SELECT count(1) from ' || Table_Names(jj) || ' a, ' ||
                   Relation_table || ' r,' ||
                   Relation_table || ' rr,'||
                   Face_table || ' f ' ||
                   ' where a.topogeom.tg_id = r.tg_id ' ||
                   ' and a.topogeom.tg_layer_id = r.tg_layer_id ' ||
                   ' and r.topo_id = rr.tg_layer_id ' ||
                   ' and r.topo_type = rr.tg_id ' ||
                   ' and rr.topo_id = f.face_id ' ||
                   ' and f.face_id =:1';

      sql_stmt2 := 'SELECT a.geo_id,a.sdogeometry from ' || Table_Names(jj) || ' a, ' ||
                   Relation_table || ' r,' ||
                   Relation_table || ' rr,'||
                   Face_table || ' f ' ||
                   ' where a.topogeom.tg_id = r.tg_id ' ||
                   ' and a.topogeom.tg_layer_id = r.tg_layer_id ' ||
                   ' and r.topo_id = rr.tg_layer_id ' ||
                   ' and r.topo_type = rr.tg_id ' ||
                   ' and rr.topo_id = f.face_id ' ||
                   ' and f.face_id =:1';
      if kk = 1 then
        EXECUTE IMMEDIATE sql_stmt3 into kount using left_face;
        if kount <> 0 then
        EXECUTE IMMEDIATE sql_stmt2 into Geo_Id,Geom using left_face;
        end if;
      else
         EXECUTE IMMEDIATE sql_stmt3 into kount using right_face;
        if kount <> 0 then
         EXECUTE IMMEDIATE sql_stmt2 into Geo_Id,Geom using right_face;
        end if;
      end if;



-- Check if the edge is inside the Entity. If so we are not interested.

     if kount <> 0 and (kk=1 or (kk= 2 and right_face <> -1)) then
       check_it := SDO_GEOM.RELATE(Edge_geom,'determine',geom,0.05);

       if check_it <> 'INSIDE'  then
--         dbms_output.put_line('Geo id intersects' || Geo_Id || ' edge ' || bad_edge);

-- Either write to the output table or too the FSL table directly.

         if Bad_Entities_Table is NOT NULL then

          EXECUTE IMMEDIATE
          'INSERT INTO /*+ APPEND */ ' ||  Bad_Entities_Table ||
          '(Edge_id,Geo_id,Entity,Status) VALUES (:1,:2,:3,:4)'
                USING bad_edge,Geo_Id,Table_Names(jj),statuz;
                commit;
         else

           EXECUTE IMMEDIATE
          'UPDATE ' ||  Table_Names(jj) || ' set QA_FLAG=''Y'',STATUS=:1 where GEO_ID =:2'
                USING statuz,Geo_Id;

         end if;
       end if;
     end if;
    end loop;
   End Loop;
   END LOOP;
   commit;


END FLAG_AFFECTED_ENTITIES;
--
PROCEDURE INTERIOR_POINT( PolyXYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Info MDSYS.SDO_ELEM_INFO_ARRAY, xc IN OUT NOCOPY NUMBER, yc IN OUT NOCOPY NUMBER) AS

-- Find an interior point in a polygon:
--   1) Try centroid
--   2) If that fails calculate the principal axis and then move from the
--      centroid to a place on this axis near the largest center of mass

    deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
    mu_matrix       MDSYS.SDO_LIST_TYPE;
    eigens          MDSYS.SDO_LIST_TYPE;
    areas           MDSYS.SDO_LIST_TYPE;
    xcs             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    ycs             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    area            NUMBER;
    xLL             NUMBER;
    xUR             NUMBER;
    yLL             NUMBER;
    yUR             NUMBER;
    x               NUMBER;
    y               NUMBER;
    dx              NUMBER;
    dy              NUMBER;
    angle           NUMBER;
    sin0            NUMBER;
    cos0            NUMBER;
    delta           NUMBER := 0.1;
    found           PLS_INTEGER := 0;
    last_found      PLS_INTEGER := 0;
    inside          PLS_INTEGER;
    no_of_loops     PLS_INTEGER;
BEGIN
    areas := Centroid(PolyXys,info,xc,yc);

    inside := POINT_IN_POLY_CC(xc,yc,Polyxys);
--      dbms_output.put_line('inside ' || inside || ' xc ' || xc || ' yc ' || yc);
    If inside = 1 then
       RETURN;
    end if;

-- We have a problem, the centroid falls outside the polygon.
-- Begin by finding the principal axis
--    dbms_output.put_line('calling moments from interior point' || polyxys.count);
    mu_matrix := Moments(PolyXys,Info,12,'TRUE');
-- Now use the mu_matrix to find the principal axis
--    eigens := Eigenvectors(mu_matrix);
    angle := mu_matrix(7);
    GZ_UTIL_ZONE.Set_MBR(PolyXys,xLL,yLL,xUR,yUR);
--    if xc > (xLL+xUR)*0.5 then
--       delta := 1.;  -- move towards mass
--    else
--       delta := -1.;
--    end if;
--    dbms_output.put_line('delta ' || delta || ' ' ||angle);
    sin0 := GZ_UTIL_ZONE.sincos(angle*deg2rad,cos0);

    for loups in 1..2 loop
      delta := delta * 0.1;
      dx := delta*(xUR-xLL)*cos0;
      dy := delta*(YUR-yLL)*sin0;
--      dbms_output.put_line('dx ' || round(dx,6) || ' dy ' || round(dy,6));
      no_of_loops := TRUNC(1./delta);
      For ii in 0..no_of_loops Loop
        if ii = 0 then  -- average alphax and alphaxx
          x := xc + ABS((mu_matrix(8))*0.25*cos0);
                      -- average alphay and alphayy
          y := yc + ABS((mu_matrix(9))*0.25*sin0);
        else
          x := xc + dx*ii;
          y := yc + dy*ii;
        end if;
        inside := POINT_IN_POLY_CC(x,y,Polyxys);
--        dbms_output.put_line(ii ||'Inside ' || inside || ' xc ' || round(x,5) || ' yc ' || round(y,5));
        If inside = 1 then
           found := found + 1.;
           if found > xcs.count then
             xcs.extend(10);
             ycs.extend(10);
           end if;
           last_found := ii;
           xcs(found) := x;
           ycs(found) := y;
        end if;
        x := xc - dx*ii;
        y := yc - dy*ii;
        inside := POINT_IN_POLY_CC(x,y,Polyxys);
--        dbms_output.put_line(ii||'IInside ' || inside || ' x ' || round(x,5) || ' y ' || round(y,5));
        If inside = 1 then
           found := found + 1.;
           if found > xcs.count then
             xcs.extend(10);
             ycs.extend(10);
           end if;
           last_found := ii;
           xcs(found) := x;
           ycs(found) := y;
        end if;
        exit when found > 0 and ii > last_found;
    END Loop;
    exit when found > 2;
    found := 0;
    End Loop;

    if found > 0 then
      xc := 0.0;
      yc := 0.0;
      for ii in 1..found loop
         xc := xc + xcs(ii);
         yc := yc + ycs(ii);
      end loop;
      xc := xc/found;
      yc := yc/found;
    end if;
END Interior_Point;
--
FUNCTION CENTROID( XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Info MDSYS.SDO_ELEM_INFO_ARRAY, Xc IN OUT NUMBER, Yc IN OUT NUMBER,Positive BOOLEAN default TRUE) RETURN MDSYS.SDO_LIST_TYPE AS
/*
********************************************************************************
--Program Name: Centroid
--Author: Sidey Timmins
--Creation Date: 8/04/2008
--Updated:  03/23/2012 To calculate centroid correctly - was getting x positive
--                     for negative longitudes! Area calculation was wrong.
--          03/11/2011 To Handle more than one ring. Note this function shows
-- how to step simply thru the coordinates when there are multiple rings
--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 4 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT

  --      XYs          -  Array of X,Y coordinates
  --      Info         -  Element Information Array corresponding to the Xys.
  --      OUTPUT
  --       Xc           - Centroid X (usually of the biggest area)
  --       Yc           - Centroid Y
--
-- Purpose: Find a centroid of a single closed polygon and return the area(s) of
--         the polygon(s) or a non_self_intersecting edge. Area values are in
--         square input units. Accepts redundant coordinates. Centroid is
--         incorrect if the area is zero.

-- Reference: DH Maling, "Measurements from Maps"
----            http://en.wikipedia.org/wiki/Centroid
********************************************************************************
*/

   Area        NUMBER := 0.0;
   Areas       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

   delta       NUMBER;
   kount       PLS_INTEGER;
   ii          PLS_INTEGER;
   istart      PLS_INTEGER;
   iend        PLS_INTEGER;
   n           PLS_INTEGER;
   next        PLS_INTEGER :=1;
   last_area   NUMBER := 0.0;
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


-- Compute area(s) of a polygon (with optional holes) or a multipolygon
-- (with optional holes).
        n := TRUNC(Info.count/3);
        areas.extend(n+1);
        areas(1) := 0.0;
        xc := 0.0;
        yc := 0.0;

        FOR loops in 1..n LOOP
          istart := loops*3+1;   -- is 4
          if loops*3 = Info.count then
            iend := Xys.count-2;  -- iend points to the y coordinate
          else
            iend := Info(istart)-3;  -- of vertex n-1 (vertex n =1)
          end if;

          ii := Info(istart-3);  -- is Info(1)
          kount := TRUNC((iend-ii)/2)+1;  -- Ignore last vertex (same as start)

          -- To handle a hole, we just keep adding (but since the coordinates are
          -- stored clockwise, their area contribution is negative).

          for i in 1..kount loop

            -- area is the sum of:   (current x * y ahead - x ahead * current y)
            -- we avoid the factoring of the formula because differencing can
            -- lead to error... area = sum of current x * (y ahead - ybehind) !!!
            -- delta is impervious to repeated ordinates
            
            delta := (XYs(iend-1)*XYs(ii+1) - XYs(ii)*XYs(iend));
            
            Area := Area + delta;
            
            Xc := Xc + (XYs(iend-1)   + XYs(ii)) * delta;            
            Yc := Yc + (XYs(iend)     + XYs(ii+1)) * delta;
            
            ii := ii+2;
            iend := ii-1;           
          end loop;
 
-- keep track of individual areas
          if loops*3 = Info.count or Info(istart+1) = 1003 then
              next := next+1;
-- This will store hole areas negative
--
--  say you have a square area 100 with a hole area 16. Then area is now
-- (100-16) = 84 and area of the hole should be stored as -16
--                if area < last_area then
--                   areas(next) := (area - last_area)*0.5;
--                else
-- whereas an island should be store as 116 -100 = 16
              areas(next) := (area - last_area)*0.5;
--                end if;
              last_area := area;
           end if;
        END LOOP;

        areas.trim(areas.count-next);
        Area := (Area * 0.5);

        if Area <> 0.0 then
           Xc := Xc /(area*6.);
           Yc := Yc /(area*6.);
        end if;
-- Store the total area.
        if positive then
          areas(1) := abs(Area);
        else
          areas(1) := Area;
        end if;
  RETURN Areas;


END CENTROID;
--
FUNCTION COMPARE_EDGE_COORDINATES (XYs_Gen IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,match_count IN OUT NOCOPY PLS_INTEGER,bad_match_sections IN OUT NOCOPY PLS_INTEGER,decim_digits PLS_INTEGER default 6,bad_length PLS_INTEGER default 4) RETURN MDSYS.SDO_LIST_TYPE AS
/*
********************************************************************************
--Program Name: Compare_Edge_Coordinates
--Author: Sidey Timmins
--Creation Date: 11/02/2010

--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 6 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      XYs_Gen     - Array of generalized ordinates to compare with Xys
  --      XYs         - Array of ungeneralized ordinates to search
  --      decim_digits -- The number of decimal digits to round coordinates to
  --                      compare values. Default 6
  --      bad_length  -- number of vertices that must match to define a
  --                     "bad" section . Default 4
   --     OUTPUT
  --       match_count -- returned length of matches array
  --       bad_match_sections -- the number of bad sections
--
-- Purpose: Returns the matches between different vertex pairs in 2 different
--          coordinate arrays
-- Ungen       Gen
-- 1.000002    1    Coordinate 1 matches coordinate 1 and does so for 2 vertices
-- 3           3    coordinate 3 (vertex 2) matches coordinate 3 (vertex 2)
-- 5           7    coordinate 5 (vertex 3) matches coordinate 7 (vertex 4)
-- 7           9
-- 11          13
--
-- Method: Exhaustive search for each Xy vertex pair from a derived (generalized)
--         geometry from a source (ungeneralized geometry);
--
-- Called by:
-- Dependencies: None
********************************************************************************
*/
   matches   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   n         PLS_INTEGER := TRUNC(Xys_gen.count/2);
   m         PLS_INTEGER := TRUNC(Xys.count/2);
   x         NUMBER;
   y         NUMBER;
   match     NUMBER := 1;
   decim     NUMBER := 0.000001;   -- allow for a million segments

   mstart    PLS_INTEGER := 2;
   j         PLS_INTEGER := 1;
   k         PLS_INTEGER := 1;
   last      PLS_INTEGER := 1;
   klast     PLS_INTEGER :=1;
   new_section   PLS_INTEGER;

BEGIN

     matches.extend(Xys_gen.count);
-- We presume that the first vertices of each array match.
     matches(1) := 1; -- Generalized segment 1 matches ungeneralized segment 1
     matches(2) := 1;
     new_section := 1;
     last := 1;
  -- We always check the generalized (with fewer vertices) against the
  -- ungeneralized edge (with more vertices).
     FOR ii in 2..n-1 LOOP
        j := j + 2;
        x := ROUND(Xys_gen(j),decim_digits);
        y := ROUND(Xys_gen(j+1),decim_digits);
-- Now search for the next generalized vertex
        For jj in mstart..m-1 Loop
         k := k + 2;
--      dbms_output.put_line('k ' || k ||' ' || xys.count || ' jj ' || jj || ' mstart ' || mstart || ' m ' || m);
         if x = ROUND(Xys(k),decim_digits) and y = ROUND(Xys(k+1),decim_digits) then
            match := match+1;
            matches(match*2-1) := j;  -- Generalized segment j matches ungeneralized segment k
            if  k = klast + 2 then
                matches(match*2) := k;
                matches(last) := matches(last) + decim;  -- record count of consecutive as a decimal
                new_section := new_section + 1;
                if new_section = bad_length then -- count 4 consecutive matches
                  bad_match_sections := bad_match_sections + 1;
                elsif new_section = 2 then
                  matches(last) := matches(last) + decim;
                end if;
                if new_section >= 2 then
                  matches(match*2-1) := -j;  -- mark continuing with a negative
                end if;
                klast := k;
            else
                matches(match*2) := k;
                last := match*2-1;
                new_section := 1;
                klast := k;
            end if;
            exit;
         else
           new_section :=0;
         end if;
        End Loop;
-- Next time we search we can start further into the ungeneralized array
        mstart := TRUNC((k+3)/2);
     END LOOP;
-- We presume the last 2 vertices match each other
-- This ensures we close off the match array for this call.
      match := match+1;
      k := k + 2;
      if  k = klast + 2 then
          matches(last) := matches(last) + decim;
          new_section := new_section + 1;
          if new_section = bad_length then -- count 4 consecutive matches
              bad_match_sections := bad_match_sections + 1;
          end if;
      end if;
      matches(match*2-1) := xys_gen.count-1;
      matches(match*2) := xys.count-1;
      match_count := match*2;
  RETURN matches;


END COMPARE_EDGE_COORDINATES;
--
FUNCTION Find_Attitude(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY) RETURN NUMBER AS

   rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   xLL         NUMBER;
   xUR         NUMBER;
   yLL         NUMBER;
   yUR         NUMBER;
   angle       NUMBER;
   pos         NUMBER;
   pos1        NUMBER;
   pos2        NUMBER;
   pos3        NUMBER;
   xpos        NUMBER;
   ypos        NUMBER;
   tan0        NUMBER;
   tan1        NUMBER;
BEGIN

-- Grass roots way of finding the orientation
   GZ_UTIL_ZONE.Set_MBR(Xys,xLL,yLL,xUR,yUR);

   pos := Find_xory(NULL,yLL,Xys);
   pos1 := Find_xory(xLL,NULL,Xys);
   pos2 := Find_xory(xUR,NULL,Xys);
   pos3 := Find_xory(NULL,yUR,Xys);
   xpos := Find_xory(Xys(pos3),NULL,XYs);
   ypos := Find_xory(XYs(pos2),NULL,XYs);
--   dbms_output.put_line('pos ' ||pos || ' pos1 ' || pos1 || ' pos2 ' || pos2 || ' pos3 ' || pos3 || ' xpos ' || xpos || ' ypos ' || ypos);
--dbms_output.put_line('xyspos ' ||Xys(pos+1) || ' xysxpos1 ' || xys(xpos+1));
--dbms_output.put_line('xyspos2 ' ||Xys(pos2) || ' xysxpos3 ' || xys(pos3));
    if (xUR-xLL > yUR-yLL and Xys(pos+1) = Xys(ypos+1)) then
       tan0 := 0.0;
    elsif (xUR-xLL < yUR-yLL and Xys(pos+1) = Xys(ypos+1)) then
       tan0 := NULL;
    elsif XYS(pos+1) = XYS(pos2+1) then
       tan0 := NULL;
    elsif XYS(pos) <> XYS(pos2) then
      tan0 :=  (Xys(pos2+1)-Xys(Pos+1))/(Xys(Pos2)-Xys(pos));
--      dbms_output.put_line('ypos ' || round(xys(pos1+1),8));
--      dbms_output.put_line('ypos ' || round(xys(pos2+1),8));
--      dbms_output.put_line('ypos ' || (xys(pos1+1) - xys(pos2+1)));
--      dbms_output.put_line('xpos ' || (xys(pos1) - xys(pos2)));
--    dbms_output.put_line('tan ' || round(tan0,8));
      if XYS(pos) <> XYS(pos1) then
      tan1 :=  ((Xys(pos+1)-Xys(Pos1+1))/(Xys(Pos)-Xys(pos1)));
--       dbms_output.put_line('tan1 ' || round(tan1,8));
--        dbms_output.put_line('tan0 ' || round(tan0,8));
--      if abs(tan1) < abs(tan0) then
--         tan0 := tan1;
--      end if;
      end if;
   elsif XYS(pos) <> XYS(pos3) then
      tan0 :=  ((Xys(pos3+1)-Xys(Pos+1))/(Xys(Pos3)-Xys(pos)));
--       dbms_output.put_line('tan00 ' || round(tan0,8));
   elsif XYS(pos) <> XYS(pos1) then
     tan0 :=  ((Xys(pos+1)-Xys(Pos1+1))/(Xys(Pos1)-Xys(pos)));
--      dbms_output.put_line('tan000 ' || round(tan0,8));
   else
     tan0 := 0.0;
   end if;

   if tan0 is not NULL then
      angle := GZ_UTIL_ZONE.atan2(tan0,1.)*rad2deg;
   else
     angle := 90.0;
   end if;
   RETURN angle;
   
END Find_attitude;
--
FUNCTION Radial_distances( XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Xc IN OUT NUMBER, Yc IN OUT NUMBER, no_of_radii PLS_INTEGER default 16,SRID NUMBER default 8265.)

RETURN MDSYS.SDO_LIST_TYPE AS
/*
********************************************************************************
--Program Name: Radial_distances
--Author: Sidey Timmins
--Creation Date: 8/04/2008

--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 3 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT

  --      XYs         - Array of X,Y coordinates
  --      Xc,Yc       - Interior point ( usually centroid) X,Y -- must not be
  --                    NULL and must be inside the polygon
  --      no_of_radii - Number of radial distances to compute (16 default)
--
-- Purpose: Returns radial distances for the Boyce shape descriptor. At present
-- may circumnavigate a non-convex polygon more than once and return 2 or more
-- times the no_of_radii. It is not clear if it fails to find the desired radii
-- correctly (it has trouble with a pair of quads such as 15 and 1 when it is
-- trying to go from quad 1 to quad 2) .
-- Reference: "The Concept of Shape in Geography" Ronald R Boyce, W A V Clark
----      Geographical review, Vol 54, #4, (Oct 1964) pp 561-572.
********************************************************************************
*/

   rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   n               PLS_INTEGER := XYs.count;
   Angle            NUMBER;
   angle1           NUMBER;
   angle2           NUMBER;
   x                NUMBER;
   y                NUMBER;
   dy               NUMBER;
   dx               NUMBER;
   s                NUMBER;
   angle_next       NUMBER;
   last_angle       NUMBER;
   inc              NUMBER;
   xlast            NUMBER;
   ylast            NUMBER;
   angle_tolerance  NUMBER := 0.001; -- 0.001;  -- degrees
   angle_low        NUMBER;
   angle_hi         NUMBER;
   angle11          NUMBER;
   angle22          NUMBER;
   x1               NUMBER;
   y1               NUMBER;
   x2               NUMBER;
   y2               NUMBER;
   temp             NUMBER;
   angle_diff       NUMBER;
   swap             NUMBER;
   delta            NUMBER := 360./no_of_radii;
   tl               NUMBER := 0.0;
   loops            PLS_INTEGER;
   loups            PLS_INTEGER := 0;
   Radii            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Angles           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Distances        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   next             PLS_INTEGER := 0;
   nm               PLS_INTEGER := TRUNC(n/2) ;
   ii               PLS_INTEGER := 2;
   kount            PLS_INTEGER := 0;
   quads            PLS_INTEGER;
   dist             NUMBER;
   bias             NUMBER := 0.0;
   quad1            PLS_INTEGER;
   quad2            PLS_INTEGER;

BEGIN

--        Interior point is +
--
--                n-1 ____________  vertices
--                    |           |
--                    |           |
--                    |     +     |
--                    |           |
--                    |___________|
--                   1=n          2

 -- Measure radial distance every delta degrees

  distances.extend(no_of_radii*4);
  angles.extend(no_of_radii*4);
--  dbms_output.put_line('xc ' || xc || ' yc ' || yc);

     x1 := Xys(1);
     y1 := Xys(2);
     dx := x1 - xc;
     dy := y1 - yc;
     angle11 := GZ_UTIL_ZONE.atan2(dy,dx)*rad2deg;
     angle22 := angle11;
  WHILE ii < Xys.count  loop
     loups := loups + 1;
     x1 := Xys(ii-1);
     y1 := Xys(ii);
     angle11 := angle22;
     angle1 := angle22;
     if angle1 < 0.0 then
       angle1 := angle1 + 360;
     end if;
     ii := ii + 2;
     x2 := Xys(ii-1);
     y2 := Xys(ii);
     dx := x2- xc;
     dy := y2 - yc;
     angle2 := GZ_UTIL_ZONE.atan2(dy,dx)*rad2deg;
     angle22 := angle2;
--     dbms_output.put_line(' 1 ' || round(angle11,4) || ' 2 ' || round(angle22,4));
--     dbms_output.put_line('x1 ' || x1  ||  ' y1 ' || y1 || ' x2 ' || x2 || ' y2 ' || y2);
--      dbms_output.put_line('RAW angle1 ' || round(angle1,4) || ' angle 2 '||  round(angle2,4));

     if angle2 < 0. then
        angle2 := angle2 + 360.;
     end if;

     swap := 0.0;
--     if angle1 > angle2 then
--       temp := angle1;
--       angle1 := angle2;
--       angle2 := temp;
--       swap := 1.0;
--     end if;
     bias := 0.0;
--        if loups = 1 or (angle11 >= angle_next and angle2 > angle_next) then
 --    if angle1 < 0. then
 --       bias := 360.;
--        angle1 := angle1 + bias;
--        angle2 := angle2 + bias;
--     end if;
--if angle1 < 2 and angle1 > 0. then
--dbms_output.put_line('Angle1 ' || round(angle1,4) || ' angle 2 '||  round(angle2,4));
--dbms_output.put_line('angle11 ' || round(angle1,4) || ' angle 22 '||  round(angle22,4));
--end if;
      if angle2 < angle1 and angle11 < angle22 and angle2 < 360 then
        bias := 360.;
        angle2 := angle2 + 360.;
      end if;

--         dbms_output.put_line('angle1 ' || round(angle1,4) || ' angle 2 '||  round(angle2,4));

     if loups = 1 then
       angle_next := TRUNC(angle1/delta) * delta;

--          dbms_output.put_line('next_angle ' || angle_next);
       if angle_next < angle1 then
         angle_next := angle_next + delta;
       end if;
     else
       angle_next := angle_next + delta;
       if angle_next < 360. and NOT(angle_next >= angle1 and angle_next <= angle2) then
         angle_next := angle_next + 360.;
       end if;
     end if;
--      dbms_output.put_line('Angle_next ' || angle_next );
   if angle1 < angle2 and ((angle1 <= angle_next and angle2 >= angle_next)) and (angle2 - angle1 <=180.) then

--       dbms_output.put_line('Next_angle ' || angle_next );

     loops := 0;

     x := Xys(ii-1);
     y := Xys(ii);
     angle_low := angle1;
     angle_hi := angle2;
     angle := angle1;
     angle_diff := 0.0;
     last_angle := angle_next;
--     dbms_output.put_line('lOW_angle ' || round(angle_low,12) || ' hi ' || round(angle_hi,8));
     WHILE abs(angle - angle_next) > angle_tolerance and loops < 15 and angle_hi <> angle_low Loop
         loops := loops + 1;
--         dbms_output.put_line('LOW_angle ' || round(angle_low,12) || ' hi ' || round(angle_hi,8));
--         dbms_output.put_line('angle ' || round(angle,8) || ' NA ' || round(angle_next,8));
         s := (angle_next-angle_low)/(angle_hi-angle_low);
--         if (angle_diff > 1.5 and (s <0.05 or s > .95 ) and loops < 3) or loops > 10 then
--           s := 0.5;
--         end if;
         if s > 1.0 then
           s := 1.0;
         elsif s < 0.0 then
           s := 0.0;
         elsif s < 0.2 and (loops =2  or (loops > 7 and loops < 12)) then
           s := s * 5.;
         elsif s > 0.8 and (loops =2  or (loops > 7 and loops < 12)) then
           s := s - (1.-s) * 4.;
         elsif angle_diff < 0.5 and abs(angle - angle_next) > 0.5  then
           s := s*0.5;
         end if;

--         if (angle_diff > 1.5 and (s <0.05 or s > .95 ) and loops < 3) or loops > 10 then
--           s := 0.5;
--         end if;
         if swap <> 0.0 then
            s := 1.0 - s;
         end if;
         x := (1.-s) * x1 + s*x2;
         y := (1.-s) * y1 + s*y2;
--         dbms_output.put_line('x ' || round(x,10) || ' y ' || round(y,10));
         dy := y - yc;
         dx := x - xc;
         angle := GZ_UTIL_ZONE.atan2(dy,dx)*rad2deg;
         if angle < 0. then --or NOT(angle >= angle1 and angle <= angle2) then
          angle := angle + 360.;
         end if;
--         dbms_output.put_line('aaangle ' || ROUND(angle,8) || ' s ' || round(s,10) || ' bias ' || bias);
         if angle > 0. and NOT(angle >= angle_low and angle <= angle_hi) then
           angle := angle + bias;
         end if;
--  dbms_output.put_line('AAangle ' || ROUND(angle,8) || ' s ' || round(s,10));
         if angle > angle_next and swap = 0 then
            angle_hi := angle;
            x2 := x;
            y2 := y;
--            dbms_output.put_line('changed x2 ' || round(x2,6) || ' y ' || round(y2,6));
         else
            angle_low := angle;
            x1 := x;
            y1 := y;
--            dbms_output.put_line('changed x1 ' || round(x1,6) || ' y ' || round(y1,6));
         end if;
        angle_diff := abs(angle - last_angle);
        last_angle := angle;
       End Loop;
--       dbms_output.put_line('Loops ' || loops || ' angle ' || round(MOD(angle,360),12));
       tl := tl + loops;
--       dbms_output.put_line('xc '|| xc || ' yc ' || yc || ' x ' || x || ' y ' || y);
       if SRID is NULL or SRID <> 8265 then
         dist := sqrt((xc-x)*(xc-x) + (yc-y)*(yc-y));
       else
         dist := distance_fcn(xc,yc,x,y);
       end if;
       next := next + 1;
       if next > distances.count then
          distances.extend(no_of_radii*4);
          angles.extend(no_of_radii*4);
       end if;
       distances(next) :=dist;
--       dbms_output.put_line(' angles ' ||(angle_next + delta) || ' ang2  ' || round(angle2,4));
       if angle_next +delta < angle2 then
          ii := ii - 2;
          angle22 := angle11;
--           dbms_output.put_line('reset 1 ' || round(angle11,4) || ' 2 ' || round(angle22,4));
       end if;
       angles(next) := MOD(angle_next,360.);
    ELSE

       angle_next := angle_next - delta;
    END IF;
    angle_next := MOD(angle_next,360.);
    exit when next = no_of_radii;
--       dbms_output.put_line(' angles ' || round(angles(next),8) || ' d ' || round(distances(next),3));
    End Loop;


--  for ii in 1..distances.count loop
--     if distances(ii) > 1000000. then
--         dbms_output.put_line('ii ' || ii || ' ' ||ROUND(distances(ii),4));
--     end if;
--  end loop;
  Shellsort2(angles,distances,1,next);
  radii.extend(next);
  last_angle := -1;
  for ii in 1..next loop
     kount := kount + 1;
     if angles(ii) <> last_angle then
        radii(kount) := -Distances(ii);
     else
        radii(kount) := Distances(ii);
     end if;
     last_angle := angles(ii);
--     dbms_output.put_line('rad ' || ROUND(radii(kount),4) || ' angles ' || angles(ii) || ' d' || distances(ii));
  end loop;
--  dbms_output.put_line('total loops '|| tl);
  RETURN radii;

END RADIAL_DISTANCES;
--
FUNCTION EIGENVECTORS( mu_matrix MDSYS.SDO_LIST_TYPE,id VARCHAR2 default NULL) RETURN MDSYS.SDO_LIST_TYPE AS
/*
********************************************************************************
--Program Name: eigenvectors
--Author: Sidey Timmins
--Creation Date: 6/25/2010
--Updated: 7/06/2010 Made stable when root would be negative.
--Usage:
  --   REQUIRED Parameters:
  --      mu_matrix          - matrix of 2nd order moments
  --
  --                         |  muyy    -muxy  |
  --                         | -muxy     muxx  |
  --   Returns:
  --       The eigenvectors (vectors that describe the attitude of the
--         major and minor axes). See diagram below for example below.

-- Purpose: Finds eigenvectors from a mumatrix calculated by moments.

-- References: 
-- 1) "On the Calculation of Moments of Polygons". Carsten Steger.:
--            http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.29.8765&rep=rep1&type=pdf
-- 2) "Matrix Tutorial 3: Eigenvalues and Eigenvectors".  Dr E Garcia: Very clear!
--            http://www.miislita.com/information-retrieval-tutorial/matrix-tutorial-3-eigenvalues-eigenvectors.html
--
-- Called by: Measure_Elongate_Length
********************************************************************************
*/
   rad2deg     CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   lambda1     NUMBER;   -- eigenvalue
   lambda2     NUMBER;   -- eigenvalue
   muyy        NUMBER;
   muxx        NUMBER;
   muxy        NUMBER;
   b           NUMBER;
   c           NUMBER;
   trace_      NUMBER;
   scaler      NUMBER;
   b2m4c       NUMBER;
   det         NUMBER;
   temp        NUMBER;
   angle       NUMBER;
   oangle      NUMBER;
   area_sqrt   NUMBER;
   eigens      MDSYS.SDO_LIST_TYPE :=  MDSYS.SDO_LIST_TYPE();

BEGIN
--     For test example, area is 40, alphax =5 alphay = 4
--                       alphaxx = 30.6667, alphayy = 18.6667, alphaxy=22
--
--        Vertex is +                         + (8,8)
--                                     .       \
--                               .              \
--                    |     .                    + (10,4)
--                (0,4) +                  .
--                    |  \           .
--                    |   \     .
--                    |    +
--                          (2,0)

  muyy := mu_matrix(1);
  muxy := -mu_matrix(2);
  muxx := mu_matrix(4);
  area_sqrt := sqrt(mu_matrix(5));
--  b2m4c := mu_matrix(6);
  det := muyy*muxx - muxy*muxy;
-- dbms_output.put_line('muyy ' ||to_char(muyy,'99.99999999EEEE'));
--  dbms_output.put_line('muxy ' || to_char(muxy,'99.99999999EEEE'));
--  dbms_output.put_line('muxx ' || to_char(muxx,'99.99999999EEEE'));
--  scaler := 1./(4.*muxx*muyy-muxy*muxy);

--  Now solve the quadratic equation ax2 + bx + c =0
  b := -(muyy + muxx);
  c := muxx * muyy - muxy*muxy;

--  dbms_output.put_line('b2m4c ' || to_char(b2m4c,'99.99999999EEEE'));
--  dbms_output.put_line('b2o4c ' || to_char((b*b-4.*c),'99.99999999EEEE'));
--  dbms_output.put_line('bb' || to_char(b,'99.99999999EEEE'));
--  dbms_output.put_line('cc ' || to_char(c,'99.99999999EEEE'));
--  if b2m4c <= 1.E-10 then
--    b2m4c := 0.0;
--  end if;
  b2m4c := b*b-4.*c;
  if (b2m4c) > 0.0 then
--    lambda1 := c/ (0.5*b - sqrt(b*.25*b -c));
--    dbms_output.put_line('Lambda1 ' || to_char(lambda1,'99.99999999EEEE'));
    lambda1 := (-b + sqrt(b2m4c))/(2.);
    lambda2 := (-b - sqrt(b2m4c))/(2.);
--    dbms_output.put_line('equation ' || (lambda1 * lambda1 + lambda1*b +c) );
--    dbms_output.put_line('equation ' || (lambda2 * lambda2 + lambda2*b +c) );


    if lambda1 > lambda2 then
       temp := lambda1;
       lambda1 := lambda2;
       lambda2 := temp;
    end if;
  else
     lambda1 := -b*0.5; ---b*0.5;
     lambda2 := 0.0; --lambda1;
  end if;
--  dbms_output.put_line('lambda1 ' || to_char(lambda1,'99.99999999EEEE'));
--  dbms_output.put_line('lambda2 ' || to_char(lambda2,'99.99999999EEEE'));
  trace_ := lambda1 + lambda2;

--       The 2 eigenvectors: End Vertex is v
--
--
--                    |                         v (2,1)
--                    |                   .
--                    |             .
--                    |     .
--                    *___________________________
--                    | .
--                    |  .
--                    |   .
--                    |     .
--                    |      .
--                    |       .
--                    |        v (1,-2)

--  dbms_output.put_line('roots ' || to_char((muyy-lambda1),'99.99999999EEEE') || ' muxy ' || to_char(muxy,'99.99999999EEEE'));
--  dbms_output.put_line('roots ' || to_char(muxy,'99.99999999EEEE') || ' muxx  ' || to_char((muxx-lambda1),'99.99999999EEEE'));
  eigens := MDSYS.SDO_LIST_TYPE(1.,1.,1.,1.,1.,1.,0.);
--  if abs(muyy-lambda1) > abs(muxy) then
--  dbms_output.put_line('X ' || (muxy-1.E-16));
    if abs(muxy) > 1.E-30 then   -- was 1.E-18
      eigens(2) := (muyy-lambda1)/muxy;
      eigens(5) := 4.*sqrt(abs(lambda1));
      dbms_output.put_line('a');
    elsif b2m4c < 1.E-15 and muxy <> 0.0 then
       eigens(2) := mu_matrix(7); --1./sqrt(abs(muyy-lambda1));
--      dbms_output.put_line('b');
    else
      if abs(mu_matrix(7)) > 1.0 then
        eigens(1) := 0.0;
      else
      eigens(2) := 0.0;
      end if;
--      dbms_output.put_line('c ' || mu_matrix(7));
--    elsif b2m4c = 0.0 then
--      eigens(2) := lambda1/muyy; -- mu_matrix(7)/mu_matrix(8);
--      eigens(5) := 4.*sqrt(area_sqrt);
    end if;
 --   dbms_output.put_line('a ' || eigens(5));
--  else
--  dbms_output.put_line('z ');
--    if abs(muyy-lambda1) > 1.E-15 then
--      eigens(1) := muxy/(muyy-lambda1);
--      eigens(5) := 4.*sqrt(abs(lambda1));
--      dbms_output.put_line('d ');
--    elsif b2m4c < 1.E-15 then
--    dbms_output.put_line('e ');
--      eigens(2) := 1.; --1./sqrt(abs(muyy-lambda1));
--      eigens(2) := mu_matrix(7);
--    else
--      eigens(2) := 0.0;
--      dbms_output.put_line('f');
--    end if;
--    dbms_output.put_line('b aa ' ||eigens(5));
--  end if;
   if abs(muyy-lambda2) > abs(muxy) then
--   dbms_output.put_line('gg');

    if abs(muxy) > 1.E-30 then
      eigens(4) := (muyy-lambda2)/muxy;
      eigens(6) := 4.*sqrt(abs(lambda2));
--      dbms_output.put_line('g');
      elsif b2m4c < 1.E-15 then
      if mu_matrix(7) <> 0. then
      eigens(3) := -1./mu_matrix(7); --mu_matrix(7)/mu_matrix(8);
      end if;
      eigens(6) := 4.*sqrt(area_sqrt);
--      dbms_output.put_line('h');
    end if;
--    dbms_output.put_line('c  bb ' || eigens(6));
  else
--     dbms_output.put_line('det ' || det);
    if muyy <> lambda2 then
      eigens(3) := muxy/(muyy-lambda2);
      eigens(6) := 4.*sqrt(abs(lambda2));
--      dbms_output.put_line('i');
    end if;
--    dbms_output.put_line('d bb ' || eigens(6));
  end if;

-- This is the attitude of the major axis
--  dbms_output.put_line('E ' || eigens(1) || ' E ' || eigens(2));
--  dbms_output.put_line('E ' || eigens(3) || ' E ' || eigens(4));

  angle := GZ_UTIL_ZONE.atan2(eigens(2),eigens(1)) * rad2deg;
  dbms_output.put_line('raw angle ' || round(angle,12));
  if muxx <> muyy and muxy <> 0.0 then
--    dbms_output.put_line('muxy ' || muxy);
  oangle := rad2deg*0.5*GZ_UTIL_ZONE.atan2((2.*(muxy)/(muxx-muyy)),1.);
  if (muxx-muyy) < 0.0 then
    oangle := oangle + 90.;
  end if;
  if oangle < 0.0 then
    oangle := oangle + 180.;
  end if;
  else
--   dbms_output.put_line('raw O angle ' || mu_matrix(7));
    oangle := mu_matrix(7);
  end if;
--   dbms_output.put_line('raw oangle ' || round(oangle,12));



  if angle < 0.0 then
    eigens(7) := 180.0 + angle;
  else
    eigens(7) := angle;
  end if;
--    if ABS(oangle - eigens(7)) > 0.001 then
    dbms_output.put_line('ID ' || id);
  dbms_output.put_line('other ' || to_char(muxy,'99.99999999EEEE') || ' muxx-muyy  ' || to_char((muxx-muyy),'99.99999999EEEE'));
  dbms_output.put_line('angle ' || round(eigens(7),12) || ' other angle ' || round(oangle,12));
--  end if;
--dbms_output.put_line('Angle ' || round(eigens(7),6));
--  dbms_output.put_line('lambda1 ' || lambda1 || ' lambda2 ' || lambda2);
--  dbms_output.put_line('T ' || trace_);
--  dbms_output.put_line('eq 1 ' || ROUND((muyy-lambda1),18) || ' ' ||round(muxy,18));
--  dbms_output.put_line('eq 2 ' || ROUND((muxy),19) || ' ' ||round(muxx-lambda1,18));
--  dbms_output.put_line('Eq 1 ' || ROUND((muyy-lambda2),18) || ' ' ||round(muxy,18));
--  dbms_output.put_line('Eq 2 ' || ROUND((muxy),18) || ' ' ||round(muxx-lambda2,18));

--

  RETURN eigens;

END EIGENVECTORS;
--
FUNCTION Find_Distances(x IN OUT NOCOPY NUMBER,y IN OUT NOCOPY NUMBER,XYS IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,GC_distances  IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,pstart PLS_INTEGER default 1,pend PLS_INTEGER default 0,pnvert_outer PLS_INTEGER default 0,forwards VARCHAR2 default 'ASC') RETURN MDSYS.SDO_LIST_TYPE IS
/*
**************************************************************************************
--Program Name: Find_distances
--Author: Sidey Timmins
--Creation Date: 08/10/2009
--Usage:
  --   REQUIRED Parameters: 
  --            (x,y):         x,y coordinates or the reference point
  --            Xys:          coordinates of the lake geometry to measure to
  --            Gc_distances: array to store the distances in
  --            pstart:       vertex to start in XY (defaults to 1)
  --            pend:         vertex to end in XY. Note pstart may be greater than
  --                          pend to enable searching from say vertex 90 thru 100 (= 
  --                          vertex 1) to pend = 10.
  --            nvert_outer:  vertex to end or the # vertices in the outer ring
  --            forwards:     if true, the vertex order and distances are near to far
  --                           and vice versa.
  --
--Purpose:      Finds distances and their order (from close to far) from the  
--              input reference point (x,y) to each vertex of the Xys. 
--              Returns: vertices ordered by increasing distance.
--
-- Method:      Determines the distance to each vertex and then sorts.
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
   jj             PLS_INTEGER :=0;
   ii             PLS_INTEGER;
   ij             PLS_INTEGER;
BEGIN

    if n = 0 then n := Xys.count; end if;
 
    -- Setup the range which may or may not go through the last vertex   
--            example     20 .. 50
    if pend >= pstart then
      kount := iend-istart+1;
    else      
--            example     90 to 100 and then 1 .. 10  goes through last vertex
--              
      kount := iend + n -istart +1;
--      if n =10 then
--      dbms_output.put_line('kount ' || kount || ' iend ' || iend || ' n ' || n || 'istart ' || istart);
--      end if;
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
FUNCTION find_nearest_edge_seg(px0 NUMBER,py0 NUMBER,Poly_Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,poly_MBR IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,allowed_range MDSYS.SDO_LIST_TYPE,near_x IN OUT NOCOPY NUMBER,near_y IN OUT NOCOPY NUMBER ,check_cosine NUMBER default 0.,nvert_outer PLS_INTEGER) RETURN PLS_INTEGER AS

/*
**************************************************************************************
--Program Name: find_nearest_edge_seg
--Author: Sidey Timmins
--Creation Date: 05/17/2011
--Usage:
  -- There are 6 parameters:
  --
  --   Parameters: 
  --        INPUT
  --        (px0,py0):        coordinates of test point to measure from to the
  --                          edges of the polygon
  --        Poly_Xys:         the Xys for the polygon
  --        Poly_MBR:         the polygons MBR
  --        OUTPUT
  --            (near_x,near_y) the found nearest (or furthest point).
  --
  -- Once the near point has been found, to find a point across the lake from
  -- the near_x and near_y, input these additional parameters
  --        (near_x,near_y) : the previously found near point coordinates.
  --         check_cosine: the cosine of the desired angle from the near point.
  --                       Ideally we might desire -1 (corresponding to 180 degrees)
  --                       but in practice the cosine of 145 or a greater angle
  --                       should be input.
  --            
--Purpose:      Can be used to find the nearest point on the side of a lake to 
--              the test point (px1,py1) in the lake. Also can be used to find 
--              the furthest point, the point across the lake from the
--              nearest point. Thus we can determine the channel width.
--              Returns a segment number.
-- Method:      Searches for overlap between a rectangle around the point and
--              the polygon's MBRs. When it locates an overlapping segment,
--              it mesasure the distance and then scans all other coordinates
--              in other MBRs to ensure it did not miss any nearby segement.
--
--Dependencies: Find_distances, Perpendicular
***************************************************************************************
*/
  Vertex_order       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  SEGs_bot           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  SEGs_top           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  gc_distances       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  x0              NUMBER := px0;
  y0              NUMBER := py0;
  x1              NUMBER := near_x;
  y1              NUMBER := near_y;
  x2              NUMBER;
  y2              NUMBER;
  x3              NUMBER;
  y3              NUMBER;
  x22             NUMBER;
  y22             NUMBER;
  x2_found        NUMBER;
  y2_found        NUMBER;
  xA              NUMBER;
  yA              NUMBER;
  xB              NUMBER;
  yB              NUMBER;
  xLL             NUMBER;
  yLL             NUMBER;
  xUR             NUMBER;
  yUR             NUMBER;
  dist_so_far     NUMBER := 1.E10;
  cos_angle       NUMBER;
  dx              NUMBER := 0.0;
  dy              NUMBER := 0.0;
  eps             NUMBER := 0.00001;
  xAB             NUMBER;
  yAB             NUMBER;
  dist_close      NUMBER;
  seg             PLS_INTEGER;
  seg2            PLS_INTEGER;
  m               PLS_INTEGER;
  n               PLS_INTEGER := TRUNC(poly_Xys.count/2);
  kk              PLS_INTEGER;
  istart          PLS_INTEGER;
  iend            PLS_INTEGER;
  seg_found       PLS_INTEGER;
  loup            PLS_INTEGER := 0;
  done            NUMBER :=0.0;
  ok              PLS_INTEGER;
BEGIN
-- Scan a polygon's MBR to find overlapping portions and then search for closest
-- point on the polygon boundary

-- First set up the MBRs to use and which to skip based upon the allowed range(s)

  m := poly_MBR(5);
  SEGs_bot.extend(m);
  SEGs_top.extend(m);
--  dbms_output.put_line('in nearest_edge_seg'); -- || 'all ' || allowed_range(1) || ' ' || allowed_range(2) || ' ' || allowed_range(3) || ' ' || allowed_range(4));
  for k in 1..m loop
  
        kk := k*6;
        istart := TRUNC((poly_MBR(kk+5)+1)/2);
        iend := TRUNC(poly_MBR(kk+6)/2);
        SEGs_bot(k) := istart;
        SEGs_top(k) := iend;
-- dbms_output.put_line('kk ' || k || ' ist ' || istart || ' iend ' || iend );
-- user may set an allowed range to force which side of the polygon to pick
        if (allowed_range is not NULL and allowed_range(1) <> 0.0) then
--               dbms_output.put_line('ist ' || istart || ' iend ' || iend || 'all ' || allowed_range(1) || ' ' || allowed_range(2) || ' ' || allowed_range(3) || ' ' || allowed_range(4));
 
 -- Mark whole range as unuseable
           if (allowed_range(2) < istart and allowed_range(3) > iend) or
              (allowed_range(2) < istart and allowed_range(4) < istart) or
              (allowed_range(1) > iend and allowed_range(3) > iend) or
              (allowed_range(4) < istart and allowed_range(1) > iend) then
              SEGs_bot(k) :=0.0;
           else
                                
           if (allowed_range(1) >= istart and allowed_range(1) <= iend) then
              SEGs_bot(k) := allowed_range(1);
           end if;
           if (allowed_range(3) >= istart and allowed_range(3) <= iend) then
              SEGs_bot(k) := allowed_range(3);
           end if;
           if (allowed_range(2) >= istart and allowed_range(2) <= iend) then
              SEGs_top(k) := allowed_range(2);
           end if;
           if allowed_range(4) >= istart and allowed_range(4) <= iend then
              SEGs_top(k) := allowed_range(4);
           end if;
           end if;
--           dbms_output.put_line('k ' || k || ' bot ' || SEGs_bot(k) || ' top ' || SEGs_top(k)); 
        end if;
  end loop;
     
  LOOP
     loup := loup +1;
     eps := eps*2.;
     xLL := x0 - eps;
     yLL := y0 - eps;
     xUR := x0 + eps;
     yUR := y0 + eps;
--   Loop over each MBR
     For k in 1..m Loop
       kk := k*6;
       xA := poly_MBR(kk+1) - dx;
       yA := poly_MBR(kk+2) - dy;
       xB := poly_MBR(kk+3) + dx;
       yB := poly_MBR(kk+4) + dy;
       if dx = 0.0 and dy = 0.0 then
         xAB := (xA+xB)*0.5;
         yAB := (yA+yB)*0.5;
       end if;
--        dbms_output.put_line('K ' || k || ' bot ' || SEGs_bot(k) || ' top ' || SEGs_top(k));
--       dbms_output.put_line('k ' || k || ' istart ' || TRUNC((poly_MBR(kk+5)+1)/2) || ' iend ' ||TRUNC(poly_MBR(kk+6)/2) );
-- Let the ranges be >= on the lower bound and < on the upper
       IF (yUR < yA or yLL > yB) OR (xUR < xA or xLL > xB) THEN
--       dbms_output.put_line('k ignored ' || k ); --|| ' istart ' || TRUNC((poly_MBR(kk+5)+1)/2) || ' iend ' ||TRUNC(poly_MBR(kk+6)/2) || ' dx ' || dx); -- xa ' || round(xa,8) || ' xUR ' || round(xUR,8));
          NULL;
       ELSIF SEGs_bot(k) <> 0.0 THEN
         istart := SEGs_bot(k);
         iend := SEGs_top(k);

         Vertex_order :=  find_distances(x0,y0,poly_XYs,gc_distances,istart,iend,nvert_outer);

-- If the user wants to find a point across the lake, then we need to
-- make sure it is "away" from the previously found point.
--
-- Calculate the dot product between 3 points:
--       (x1,y1) a previously found near point on the lake edge
--       (x0,y0) the reference point in the lake
--       (x2,y2) the point under test on the far side of the lake

-- cos(angle) = (x2-x0) * (x1-x0) + (y2-y0) * (y1-y0)
--          ______________________________________________________
--         sqrt((x2-x0)^2 + (y2-y0)^2) * sqrt((x1-x0)^2 + (y1-y0)^2)


        for j in 1..Vertex_order.count loop
       
         seg := Vertex_order(j);
--         dbms_output.put_line('J ' || J || ' sEG ' || seg || ' dx ' || dx);
         x2 := poly_XYs(seg*2-1);
         y2 := poly_Xys(seg*2);
         seg2 := seg +1;
         if seg2 >= nvert_outer then
           seg2 := 2;
         end if;
         x3 := poly_XYs(seg2*2-1);
         y3 := poly_Xys(seg2*2);
          dist_close := GZ_SUPER.perpendicular(x0,y0,x2,y2,x3,y3,x22,y22);
         if dist_close = 1.E10 then
             dist_close := gc_distances(j);
             x22 := x2;
             y22 := y2;
         end if;
         if x1 is not NULL and (x1 <> x2 or y1 <> y2) then 
             cos_angle := ( (x22-x0) * (x1-x0) + (y22-y0) * (y1-y0) ) /
             (sqrt((x22-x0)**2. + (y22-y0)**2.) * sqrt((x1-x0)**2. + (y1-y0)**2.));
--             if istart = 50 then
--               dbms_output.put_line('seg ' || seg || ' V ' || Vertex_order(j) || ' x ' || round(x2,6) || ' y ' || round(y2,6));
--               dbms_output.put_line('cos ' || round(cos_angle,6) ||  ' x ' || round(x1,6) || ' y ' || round(y1,6));
--               dbms_output.put_line('cos ' || round(cos_angle,6) ||  ' x ' || round(x0,6) || ' y ' || round(y0,6));
 --              dbms_output.put_line('Cos ' || round(cos_angle,6) ||  ' x ' || round(x22,6) || ' y ' || round(y22,6));
--              end if;
         elsif  x1 is not NULL and (x1 = x2 and y1 = y2) then 
             cos_angle := 0.0;
             dist_close := 1.E10;
         else
             cos_angle := -1.;   -- set found cosine to 180 degrees
         end if;
--         if j < 3 then
--           dbms_output.put_line('seg ' || seg || ' dclsose ' || round(dist_close,6) ||  ' distsofar ' || round(dist_so_far,6) || ' checkcos ' || check_cosine);
--  dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2 || ' x3 ' || x3 || ' y3 ' || y3);
--   dbms_output.put_line('x22 ' || x22 || ' y22 ' || y22 || ' x0 ' || x0 || ' y0 ' || y0);
--         end if;
         if dist_close < dist_so_far and cos_angle <= check_cosine then
--         dbms_output.put_line('seg ' || seg || ' dclsose ' || round(dist_close,6) ||  ' distsofar ' || round(dist_so_far,6));
--         dbms_output.put_line('x22 ' || x22 || ' y22 ' || y22 || ' x0 ' || x0 || ' y0 ' || y0);
            dist_so_far := dist_close;
            seg_found := seg;
            x2_found := x22;
            y2_found := y22;
            done := loup;
            if x0 < xAB then
              dx := ABS(x0-xA);
            else
              dx := ABS(x0-xB);
            end if;
            if y0 < yAB then
              dy := ABS(y0-yA);
            else
              dy := ABS(y0-yB);
            end if;
            if dx > dy then
               dy := dx;
            else
               dx := dy;
            end if;

         end if;
         end loop;
         SEGs_bot(k) := 0.0;
         END IF;

       exit when loup > done and done <> 0.0;
   end Loop;
   exit when loup > done and done <> 0.0;
   exit when loup = 16;  -- corresponds to > 0.4 degrees or 50000 meters
   END Loop;

   near_x := x2_found;
   near_y := y2_found;
--   dbms_output.put_line('leaving ***'||seg_found);
   RETURN seg_found;

END find_nearest_edge_seg;
--
FUNCTION FIND_XES_AND_YES(GXYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,UXYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Uinfo IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,UMBR IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,round_it NUMBER default 6) RETURN MDSYS.SDO_LIST_TYPE AS

--Author: Sidey Timmins
--Creation Date: 3/16/2011
--Updated: 3/23/2011 To get the outer ring right when there are holes
-- Find 2 pairs of coordinates (2 vertices) from one outer ring in another
-- 2007 geometry's coordinates and return the outer ring number (NOT the ring
-- itself) in the 2nd geometry (the outer ring --can be used to call sdo_util.extract).
-- Note this ring number ignores the holes (2003s)
-- Designed just for line generalization where the Xys always came from the
-- Ungeneralized Xys with the only difference being rounding.

  j           PLS_INTEGER := 2;
  kk          PLS_INTEGER;
  n           PLS_INTEGER := TRUNC(GXys.count/2);
  m           PLS_INTEGER;
  mring       PLS_INTEGER := TRUNC(Uinfo.count/3);
  istart      PLS_INTEGER;
  iend        PLS_INTEGER;
  mm          PLS_INTEGER;
  ring        PLS_INTEGER := 0;
  found       PLS_INTEGER;
  nn          PLS_INTEGER;
  loops       PLS_INTEGER;

    GMBR           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    old_ring       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    rings          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    GTest          MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    UxLL            NUMBER;
    UyLL            NUMBER;
    UxUR            NUMBER;
    UyUR            NUMBER;
    gxLL            NUMBER;
    gyLL            NUMBER;
    gxUR            NUMBER;
    gyUR            NUMBER;
    axLL            NUMBER;
    ayLL            NUMBER;
    axUR            NUMBER;
    ayUR            NUMBER;
    pdim            PLS_INTEGER := 8;
    next            PLS_INTEGER := 0;

  big_ring    NUMBER :=0;
  x           NUMBER;
  y           NUMBER;
  x2          NUMBER;
  y2          NUMBER;

  epsilon     NUMBER := 1.E-6;
  outer_ring  NUMBER;


  aptr        PLS_INTEGER;
  LB        PLS_INTEGER;
  actual_ring    PLS_INTEGER;

  Function Get_Ring(pLB PLS_INTEGER,Info_array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,last_coord PLS_INTEGER,pactual_ring IN OUT NOCOPY PLS_INTEGER) Return Number as
     last_pos     PLS_INTEGER;
     ring_counter  PLS_INTEGER := 0;
  Begin
      For ii in 1..mring Loop
        If UInfo(ii*3-1) = 2003 Then   -- Ignore the holes (2003)
         Exit when Uinfo(ii*3-2) >= pLB;

        Else
        ring_counter := ring_counter + 1;
         if ii <> mring then
          last_pos := Info_array(ii*3+1)-1;
         else
          last_pos := last_coord;
         end if;
--         dbms_output.put_line('looking for ' ||pLB || '  info ' || info_array(ii*3-2) || ' last ' || last_pos);
         if pLB >= Info_array(ii*3-2) and pLB < last_pos then
            pactual_ring := ii;
--            dbms_output.put_line('found ' || ring_counter || ' actual ' ||pactual_ring);
            Return ring_counter;
         end if;
        End If;
     End loop;
     Return 0.0;
  End;

BEGIN


    GZ_TOPOFIX.SET_MANY_MBR(UXYs,UInfo,UMBR,UxLL,UyLL,UxUR,UyUR);
    GZ_TOPOFIX.SET_MBR(GXYs,gxLL,gyLL,gxUR,gyUR);
    m := UMBR(5);

-- Make a set of test coordinates for the Generalized.
    nn := sqrt(TRUNC(Gxys.count/2));
    if nn > 20 then
      nn := 20 + TRUNC((nn-20)/2);
    end if;
    nn := nn*2;
    kk := -nn+2;
    Gtest.extend(nn);
    for ii in 1..TRUNC(nn/2) Loop
      kk := kk+nn;
      Gtest(ii*2-1) := ROUND(GXys(kk-1),round_it);
      Gtest(ii*2)   := ROUND(GXys(kk  ),round_it);
    end loop;


 -- Straightforward search for XY values in another coordinate array
 -- First find the Ungeneralized ring!


    For ii in 1..m Loop
       aptr := ii*pdim;          -- ptr into the MBR array
       axLL := UMBR(aptr+1)-epsilon;
       ayLL := UMBR(aptr+2)-epsilon;
       axUR := UMBR(aptr+3)+epsilon;
       ayUR := UMBR(aptr+4)+epsilon;

 -- Check for no overlap
      if ( (axUR  <= gxLL) or (axLL  >= gxUR) or (ayUR  <= gyLL) or (ayLL  >= gyUR)) then
         NULL;
      else
         LB := UMBR(aptr+5);
         outer_ring := Get_Ring(LB,UInfo,Uxys.count,actual_ring);
--         dbms_output.put_line('Outer ' || outer_ring || ' A ' || actual_ring);
         if outer_ring <> 0 then
         istart := Uinfo(actual_ring*3-2);
         if actual_ring <> mring then
           iend := Uinfo(actual_ring*3+1);
         else
           iend := Uxys.count+1;
         end if;
         kk := istart-1;
--         dbms_output.put_line('istart ' || istart || ' iend ' || iend || ' kk ' || kk);
         loops := TRUNC(iend-istart)/2;
         for jj in 1..loops Loop
           kk := kk+2;
           mm := 0;
           For ll in 1..TRUNC(nn/2) loop
             mm := mm+2;

             if ROUND(Uxys(kk-1),round_it) = Gtest(mm-1) and ROUND(Uxys(kk),round_it) = Gtest(mm) then

                  found := 1;
                  While found <= rings.count and rings(found) <> outer_ring loop
                     found := found + 1;
                  End Loop;
                  if found > rings.count then
                    rings.extend(1);
                    next := next + 1;
                    rings(next) := outer_ring;
                  end if;

             end if;
           End Loop;

         end loop;
         end if;
      end if;
    End Loop;
    old_ring := oldFind_Xes_and_Yes(GXys,UXys,Uinfo);

    if rings.count <> 0  then
      if rings(1) <> old_ring(1) then
         dbms_output.put_line('R ' || rings(1) || ' Old ' || old_ring(1));
         if rings.count > 1 and old_ring.count > 1 then
             dbms_output.put_line('R ' || rings(2) || ' Old ' || old_ring(2));
         elsif rings.count > 1 and old_ring.count = 1 then
             dbms_output.put_line('R ' || rings(2));
          end if;

    end if;
      RETURN rings;
    end if;
    RETURN MDSYS.SDO_LIST_TYPE(0.0);


END Find_Xes_and_Yes;
--
FUNCTION FIND_XORY(X NUMBER, Y NUMBER,XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,epsilon NUMBER default 0.0) RETURN NUMBER AS

-- Find a coordinate (either the x or the y or both) ALWAYS returning the 
--      x coordinate position. Search is either exact (searching either for x or y) 
--      or with a tolerance epsilon (searching for both x and y). 
--      When epsilon is non-zero, this function finds the interpolated point on 
--      a segment and returns segment_number +line_parameter.
--      Thus if it found the desired point 30 % along the 7th segment, it would 
--      return 7.3

  j           PLS_INTEGER := -1;
  n           PLS_INTEGER := TRUNC(Xys.count/2);
  s           NUMBER;
  pos         NUMBER;
  pos_save    NUMBER;
BEGIN


  -- Straightforward search for a value in an array

  IF epsilon = 0.0 THEN
       if x is not NULL then
          For ii in 1..n Loop   -- Search for x
            j := j + 2;
            if Xys(j) = x then
               pos := j;
               RETURN pos;
            end if;
          End Loop;
       elsif y is not NULL then
          j := 0;
          For ii in 1..n Loop   -- Search for y
            j := j + 2;
            if Xys(j) = y then
               pos := j-1.;
               RETURN pos;
            end if;
          End Loop;
       end if;
  ELSE

-- Search for either a vertex or a position on a segment
    For ii in 1..n Loop
      j := j + 2;
      if j > Xys.count then
         RETURN pos_save;
      end if;
--      dbms_output.put_line('ii ' || ii || ' j ' || j || ' xys ' || xys.count );
      if abs(Xys(j) - x) <= epsilon and
          abs(Xys(j+1)-y) <= epsilon then
           pos := j;
           RETURN pos;
      else
         pos_save := j-1;
-- Now search for a segment to interpolate on
         j := j -2;
         For jj in ii..n-1 Loop
            j := j + 2;
            if j > Xys.count then
             RETURN pos_save;
            end if;
--             dbms_output.put_line('j ' || j || ' xys ' || xys.count);
            if (Xys(j+1) <= y and Xys(j+3) > y) then
               s := (y-Xys(j+1))/(Xys(j+3) - Xys(j+1));
--               dbms_output.put_line('s ' || s);
               pos := j + s;
               RETURN pos;
            elsif
               (Xys(j+1) > y and Xys(j+3) <= y) then
               s := (y-Xys(j+3))/(Xys(j+1) - Xys(j+3));
--               dbms_output.put_line('S ' || s);
               pos := j + (1.-s);
               RETURN pos;
            elsif (Xys(j+1) = y and Xys(j+3) = y) then
               pos := j;
--             dbms_output.put_line('y ' || y || ' xys ' || xys(j+1) || ' ' || xys(j+3));
            end if;

         End Loop;

      end if;
    End Loop;


  END IF;
  RETURN 0;

END FIND_XORY;
--

FUNCTION MEASURE_ELONGATE_LENGTH(ingeometry MDSYS.SDO_GEOMETRY,Xc_in IN OUT NUMBER,Yc_in IN OUT NUMBER,radii IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,id VARCHAR2 default NULL) RETURN MDSYS.SDO_LIST_TYPE AS
/*
********************************************************************************
--Program Name: measure_elongate_length
--Author: Sidey Timmins
--Creation Date: 6/25/2010

--Usage:
  --      INPUT
  --      geometry      - a polygon to measure major axis length and attitude.
  --                      May have more than 1 ring (2007 geometry).
  --
  --      OUTPUT
  --       Major and minor axes and the attitude (angle) of the major axis wrt to x-axis
  --
-- Purpose: Find major axes and attitude (angle) from eigenvectors calculated by
--          functions moments and eigenvectors.

-- Reference: 1) On the Calculation of Moments of Polygons. Carsten Steger.:
--  http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.29.8765&rep=rep1&type=pdf
-- 2) Dr E Garcia: "Matrix Tutorial 3: Eigenvalues and Eigenvectors". Very clear see
---- http://www.miislita.com/information-retrieval-tutorial/matrix-tutorial-3-eigenvalues-eigenvectors.html
--
-- Calls: GZ_UTIL_ZONE.Set_MBR, Moments,   (Eigenevectors - NO deprecated),
--        Rotate_Coordinates,Find_xory
********************************************************************************
*/
    rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
    axes            AXES_VTYPE := AXES_VTYPE();

    PolyXys         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    PolyXys2003     MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Rotated_Xys     MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Axis_Xys        MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Rotated_Axis_Xys  MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Info            MDSYS.SDO_ELEM_INFO_ARRAY;
    Info2003        MDSYS.SDO_ELEM_INFO_ARRAY;
    angles          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    lengths         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    mu_matrix       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    eigens          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    pars            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    segments        MDSYS.SDO_LIST_TYPE;
    areas           MDSYS.SDO_LIST_TYPE;
    geometry        MDSYS.SDO_GEOMETRY;
    geom            MDSYS.SDO_GEOMETRY;
    sql_stmt        VARCHAR2(4000);
    xLL             NUMBER;
    xUR             NUMBER;
    yLL             NUMBER;
    yUR             NUMBER;
    x1              NUMBER;
    y1              NUMBER;
    x2              NUMBER;
    y2              NUMBER;
    x3              NUMBER;
    y3              NUMBER;
    xc              NUMBER;
    yc              NUMBER;
    xca             NUMBER;
    yca             NUMBER;
    area            NUMBER;
    true_area       NUMBER;
    angle_12        NUMBER;
    angle_13        NUMBER;
    s               NUMBER;
    xlength         NUMBER :=0.0;
    ylength         NUMBER := 0.0;
    last_elength    NUMBER :=0.0;
    len             NUMBER := 0.0;
    elongated_length NUMBER := 0.0;
    short_length    NUMBER :=1E10;
    other_length    NUMBER;
    angle           NUMBER := 0.;
    angle_start     NUMBER := 0.0;
    epsilon         NUMBER := 0.000002;
    temp            NUMBER;
    n               PLS_INTEGER;
    loops           PLS_INTEGER := 0;
    inside          PLS_INTEGER;
    pos             NUMBER;
    kount           PLS_INTEGER;
    inc             PLS_INTEGER;
    best_angle      NUMBER := 0.0;
    n_rotations     PLS_INTEGER := 9;
    next            PLS_INTEGER :=0;
    choose          PLS_INTEGER := 2;
    delta           NUMBER := 90./n_rotations;
    ring            NUMBER := 1.;
    ir              NUMBER;
    rings           NUMBER;
    biggest_area    NUMBER := 0.0;
    total_area      NUMBER := 0.0;
    wt              NUMBER;
BEGIN

  radii := MDSYS.SDO_LIST_TYPE();
  rings :=Getnumrings(ingeometry,'outer');
  axes.extend(rings);
  PolyXys := ingeometry.sdo_ordinates;
  Info := ingeometry.sdo_elem_info;
  GZ_UTIL_ZONE.Set_MBR(PolyXys,xLL,yLL,xUR,yUR);
  pars.extend(10);  -- Store output parameters
  pars(4) := xLL;
  pars(5) := yLL;
  pars(6) := xUR;
  pars(7) := yUR;


  WHILE ring <> 0 LOOP
    ir := ring;
--  This function keeps track and tells us via the sentinel, when to quit
    geometry := GET_OUTER_RING(ingeometry,ring);
    Info2003 := geometry.sdo_elem_info;
    PolyXys2003 := geometry.sdo_ordinates;

    GZ_UTIL_ZONE.Set_MBR(PolyXys2003,xLL,yLL,xUR,yUR);
    axes(ir).xll := xLL;
    axes(ir).yLL := yLL;
    axes(ir).xUR := xUR;
    axes(ir).yUR := yUR;
--
-- First do the areas in square meters
    true_area := sdo_geom.sdo_area(geometry,0.05,'unit=sq_meter');
    axes(ir).area := true_area;
    total_area := total_area + true_area;

--  segments := Check_for_figure_8(PolyXys);
-- next 7 lins Just for testing
--  angle := 150.;
--  GZ_UTIL_ZONE.Set_MBR(PolyXys,xLL,yLL,xUR,yUR);
--  Rotated_Xys := Rotate_coordinates(PolyXys,angle,xLL,yLL);
--  PolyXys := Rotated_Xys;
--  geom := MDSYS.SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),PolyXys);
--   sql_stmt := 'INSERT INTO POINTS (ID,GEOMETRY) VALUES (:1,:2)';
--   execute immediate sql_stmt using 1000, geom;

--  dbms_output.put_line('CALLING M polyxys count ' || polyxys.count);
--  if polyxys.count = 10 then
--     for ji in 1..5 loop
--        dbms_output.put_line(polyxys(ji*2-1) || ' ' || polyxys(ji*2));
--     end loop;
--  end if;

-- Whereas Moments is set up to handle multiple rings, we just give it
-- one ring at a time.
--    dbms_output.put_line('IR ' || ir || ' poly ' || polyxys2003.count);
    mu_matrix := Moments(PolyXys2003,Info2003,12,id);
    n := TRUNC(Polyxys2003.count/2);

-- We don't need to find the eigenvectors any more since the attitude angle
-- is found by Moments (without eigenvectors) from 0.5*atan2(2.*mu_xy/(muxx-muyy))

--  eigens := Eigenvectors(mu_matrix,id);


    axes(ir).angle := mu_matrix(7); -- eigens(7); -- angle or attitude of major axis
    angle := -angle;


    areas := Centroid(PolyXys2003,Info2003,xc,yc);
    area := areas(1);
    axes(ir).xc := xc;
    axes(ir).yc := yc;

    if ir = 1 then
      areas := Centroid(PolyXys,Info,xca,yca);
      for kk in 2..areas.count loop
         if areas(kk) > biggest_area then
            choose := kk-1;
            biggest_area := areas(kk);
         end if;
      end loop;

    end if;
    inside := 0;
    if ir = choose then
    --      dbms_output.put_line('doing areas' || polyxys.count);
      inside := POINT_IN_POLY_CC(xc,yc,Polyxys2003);

      if inside = 0 then
--      dbms_output.put_line('doing interior point' || polyxys2003.count);
        Interior_point(PolyXys2003,Info2003,xc,yc);
      end if;
      inside := POINT_IN_POLY_CC(xc,yc,Polyxys2003);
    end if;
    if xc_in is not NULL then
      xc := xc_in;
      yc := yc_in;
      inside := POINT_IN_POLY_CC(xc,yc,Polyxys2003);
    end if;


--    dbms_output.put_line('Xc ' || xc || ' yc ' || yc || ' inside ' || inside || ' choose ' || choose);
--    dbms_output.put_line('Xc ' || xc_in || ' yc ' || yc_in || ' inside ' || inside);
      if inside = 0 and rings = 1 then
        radii := MDSYS.SDO_LIST_TYPE();
      elsif xc_in is not NULL and inside = 1 then
        radii := Radial_Distances(Polyxys2003,xc_in,yc_in);
      elsif inside = 1 then
        radii := Radial_Distances(Polyxys2003,xc,yc);
      end if;

-- To measure the major axis we rotate the polygon till its major axis
-- (not necessarily the longest side) is aligned with the x - axis.

    Rotated_Xys := Rotate_coordinates(PolyXys2003,angle,xLL,yLL);
    GZ_UTIL_ZONE.Set_MBR(Rotated_Xys,xLL,yLL,xUR,yUR);
--  geom :=MDSYS.SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),Rotated_Xys);
--  sql_stmt := 'INSERT INTO POINTS (ID,GEOMETRY) VALUES (:1,:2)';
--  execute immediate sql_stmt using 100, geom;

 -- Then we set up two segments along the rotated major and minor axes, rotate
 -- them back and measure their lengths. The segments are drawn midway between
 -- the MBR extreme points horizontally and vertically. When they are rotated back
 -- they lie in the eigenvector orientation.

    axis_Xys.extend(8);
    axis_Xys(1) := xLL;
    axis_Xys(2) := yc; --(yLL+yUR)*0.5;
    axis_Xys(3) := xUR;
    axis_Xys(4) := yc; --(yLL+yUR)*0.5;
--  geom :=MDSYS.SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_Ordinate_ARRAY(xLL,(yLL+yUR)*0.5,xUR,(yLL+yUR)*0.5));
--  sql_stmt := 'INSERT INTO POINTS (ID,GEOMETRY) VALUES (:1,:2)';
--  execute immediate sql_stmt using 10, geom;
--  commit;

    x1 := xLL;
    y1 := (yLL+yUR)*0.5;
    pos := Find_xory(x1,y1,Rotated_Xys,epsilon);
--  dbms_output.put_line('pos ' || pos);
    if pos <> 0.0 then
      s := pos - TRUNC(pos);
      pos := TRUNC(pos);
      if s = 0.0 and MOD(pos,2) = 0 then
        pos := pos-1.;
      end if;
      x1 := s*PolyXys(pos) + (1.- s) * PolyXys(pos+2);
      y1 := s*PolyXys(pos+1) + (1.- s) * PolyXys(pos+3);
    end if;
    axis_Xys(1) := x1;
    axis_Xys(2) := y1;
    axis_Xys(3) := xUR + (x1-xLL);
    axis_Xys(4) := y1;
    axis_Xys(5) := (xLL + xUR)*0.5;
    axis_Xys(6) := yLL;
    axis_Xys(7) := axis_Xys(5);
    axis_Xys(8) := yUR;

    Rotated_Axis_Xys := Rotate_coordinates(Axis_Xys,-angle,Axis_Xys(1),Axis_Xys(2));
    x1 := Rotated_Axis_Xys(1);
    y1 := Rotated_Axis_Xys(2);
    x2 := Rotated_Axis_Xys(3);
    y2 := Rotated_Axis_Xys(4);
  -- geom :=MDSYS.SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_Ordinate_ARRAY(x1,y1,x2,y2));
  -- sql_stmt := 'INSERT INTO POINTS (ID,GEOMETRY) VALUES (:1,:2)';
  -- execute immediate sql_stmt using 1, geom;
    axes(ir).major_axis := Distance_fcn(x1,y1,x2,y2);
--  elongated_length := sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1));

    x1 := Rotated_Axis_Xys(5);
    y1 := Rotated_Axis_Xys(6);
    x2 := Rotated_Axis_Xys(7);
    y2 := Rotated_Axis_Xys(8);
    axes(ir).minor_axis := Distance_fcn(x1,y1,x2,y2);

--    dbms_output.put_line('ir ' || ir || ' major ' || axes(ir).major_axis || ' minor ' || axes(ir).minor_axis);
--  short_length := sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1));
    if axes(ir).major_axis < axes(ir).minor_axis then
      temp := axes(ir).major_axis;
      axes(ir).major_axis := axes(ir).minor_axis;
      axes(ir).minor_axis := temp;
--    dbms_output.put_line('Axes 2 ' || axes(ir).major_axis || ' minor ' || temp);
--    axes(1) := eigens(7) +90.;
    end if;

  END LOOP;

  pars(1) := 0.0;
  pars(2) := 0.0;
  pars(3) := 0.0;
  biggest_area := 0.0;

  For ir in 1..rings loop
    wt := axes(ir).area/total_area;
    dbms_output.put_line('wt ' || wt || ' angle ' || axes(ir).angle || ' area ' || round(axes(ir).area,3) || ' tot ' || round(total_area,3));
    pars(2) := pars(2) + axes(ir).major_axis * wt;
    pars(3) := pars(3) + axes(ir).minor_axis * wt;
    if axes(ir).area > biggest_area then
      biggest_area := axes(ir).area;
       pars(1) := axes(ir).angle;  -- we cannot weight angles easily
       pars(9) := axes(ir).xc;
       pars(10) := axes(ir).yc;
    end if;
  end loop;
  pars(8) := total_area;
  RETURN pars;

  if abs(angle) > 45. then
     y1 := yLL;
  else
     x1 := xLL;
  end if;
  pos := Find_xory(x1,y1,Rotated_Xys);
  x1 := PolyXys(pos);
  y1 := PolyXys(pos+1);
  geom :=MDSYS.SDO_GEOMETRY(2001,8265,NULL,SDO_ELEM_INFO_ARRAY(1,1,1),SDO_Ordinate_ARRAY(x1,y1));
      sql_stmt := 'INSERT INTO POINTS (ID,GEOMETRY) VALUES (:1,:2)';
      execute immediate sql_stmt using 1, geom;
  if abs(angle) > 45. then
     y2 := yUR;
  else
     x2 := xUR;
  end if;
  pos := Find_xory(x2,y2,Rotated_Xys);
  x2 := PolyXys(pos);
  y2 := PolyXys(pos+1);
  angle_12 := atan2(y2-y1,x2-x1) * rad2deg;
--  dbms_output.put_line('angle12 ' || round(angle_12,6));
  angle_13 := angle_12;
  x3 := x2;
  y3 := y2;
  inc :=  2;
  if angle_12 > angle then
    inc := -2;
  end if;
  While abs(angle - angle_12) > 0.001 and loops < n Loop
    loops := loops + 1;
    pos := pos +inc;
    if pos <= 1 then
      pos := PolyXys.count -3;
    end if;
    if pos > PolyXys.count then
      pos := 3;
    end if;
    x2 := x3;
    y2 := y3;
    x3 := PolyXys(pos);
    y3 := PolyXys(pos+1);
    angle_12 := angle_13;
--    dbms_output.put_line('p ' || pos);
exit when x3 = x1;
    angle_13 := atan2(y3-y1,x3-x1) * rad2deg;
    if (inc < 0 and angle_13 < angle) or (inc > 0 and angle_13 > angle) then
      For i in 1..4 loop
      s := (angle - angle_13)/(angle_12 - angle_13);
      if s <0. then
        s:=0.;
      elsif s > 1. then
        s:=1.;
      end if;
      x2 := (1.-s)*x3 + s * x2;
      y2 := (1.-s)*y3 + s * y2;
      angle_12 := atan2(y2-y1,x2-x1) * rad2deg;
      exit when abs(angle_12-angle_13) < 0.00001 or s >= 1. or s <=0.;
      End Loop;
--      dbms_output.put_line('Angle12 ' || round(angle_12,6) || ' s ' ||round(s,6));
      geom :=MDSYS.SDO_GEOMETRY(2001,8265,NULL,SDO_ELEM_INFO_ARRAY(1,1,1),SDO_Ordinate_ARRAY(x2,y2));
      sql_stmt := 'INSERT INTO POINTS (ID,GEOMETRY) VALUES (:1,:2)';
      execute immediate sql_stmt using 2, geom;
      exit;
    end if;

  End Loop;

  len := Distance_fcn(x1,y1,x2,y2);

--  axes(2) := len;
--  RETURN axes;
  Angles.extend(n_rotations+2);
  Lengths.extend(n_rotations+2);



  Angles(1) := 0.;
  Angles(n_rotations+2) := 90.;
 -- Lengths(1) := elength;
  Lengths(n_rotations+2) := (yUR - yLL);
  xlength := (xUR-xLL);
  yLength := (yUR - yLL);
  elongated_length := Distance_fcn(xLL,yLL,xUR,yLL);
  short_length := Distance_fcn(xLL,yLL,xLL,yUR);
  dbms_output.put_line('Angle ' || round(angle,6) || ' elongated ' || round(elongated_length,5) || ' short ' || round(short_length,5));

  if short_length > elongated_length then
 --   Lengths(1) := elength;
    Lengths(n_rotations+2) := (xUR - xLL);
    temp := short_length;
    short_length := elongated_length;
    elongated_length := temp;
    best_angle := 90.0;
  end if;


  next := next + 1;

  For ii in 1..1 Loop
  angle := angle_start;
  For ii in 1..n_rotations Loop
    angle := angle + delta;
    Rotated_Xys := Rotate_coordinates(PolyXys,angle,xLL,yLL);
    GZ_UTIL_ZONE.Set_MBR(Rotated_Xys,xLL,yLL,xUR,yUR);
    len := Distance_Fcn(xLL,yLL,xUR,yLL);

    other_length := Distance_fcn(xLL,yLL,xLL,yUR);
    if len < other_length then
      temp := other_length;
      other_length := len;
      len := temp;
    end if;
    dbms_output.put_line('angle ' || round(angle,6) || ' elongated ' || round(len,5) || ' other ' || other_length);
    if len > elongated_length or other_length < short_length then
      elongated_length := len;
      short_length := other_length;
      best_angle := angle;
      dbms_output.put_line('angle ' || round(angle,6) || ' elongated ' || round(elongated_length,5));
    end if;


    next := next + 1;
    Angles(next) := angle;
--    Lengths(next) := elength;

  END  LOOP;
  delta := delta *0.1;
  next := 0;
  angle_start := best_angle - 5.*delta;
  END LOOP;
--  RETURN elongated_length;



END MEASURE_ELONGATE_LENGTH;
--
FUNCTION MOMENTS( inXYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Info MDSYS.SDO_ELEM_INFO_ARRAY, round_it PLS_INTEGER default 12, debug VARCHAR2 default 'FALSE') RETURN MDSYS.SDO_LIST_TYPE AS
/*
********************************************************************************
--Program Name: Moments
--Author: Sidey Timmins
--Creation Date: 6/25/2010
--Updated:       12/27/2012 To accumulate results for a 2007 and return the
--                         composite attitude and angle and other results.
--                         This update makes the sum of the mu_matrix 2003 results
--                         equal the 2007 and the composite attitude angle correct.
--                         This can be tested with any figure of eight polygon,
--                         once as 2 polygons and once as an "8"
--               3/18/2011 To use a pragmatic way when the moment formula fails.
--               3/16/2011 To calculate the attitude angle directly: Ref 3.
--Usage:
  -- Call this function from inside another PL/SQL program.  This function
  -- has 2 required parameter2:
  --
  --      INPUT
  --      XYs          - Array of a polygon's X,Y coordinates
  --      Info         - The sdo_elem_info array for these Xys
  --      round_it     - the number of decimals to round coordinates to (12).
  --                     Leave this parameter alone!
  --      OUTPUT:
  --      mu_matrix             1) muyy 2) muxy  3) muxy 4)  muxx
  --      extra results         5) area 6) b*b-4ac 7) theta (attitude angle)
  --      inputs to mu_matrix   8) alphax 9) alphay 10) alphaxy 11) alphaxx 12) alphayy
--
-- Purpose: Find the 2nd order moments of a 2-D polygon to be used to
--          calculate the length and attitude of the major and minor axes and
--          creates a mu_matrix: A 2 x 2 matrix containing the centralized 2nd
--                            order moments, uxx,uxy and uyy (read mu for u)
--
-- Referencea: 1) On the Calculation of Moments of Polygons. Carsten Steger.
----           http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.29.8765&rep=rep1&type=pdf
--  2) "A General Approach to Moment Calculation for Polygons and Line Segments"
--      Pattern Recognition Vol 26, #7 pp 1019-1028
--  3) "Measuring Linearity of a Finite Set of Points" , Milos Stojmenovic, Amiya Nayak, Jovisa Zunic
--      http://www.site.uottawa.ca/~mstoj075/Publications_files/P0386.pdf
--   4) "A survey of shape feature extraction techniques", Yang Mingqiang, Kpalma Kidiyo, Ronsin Joseph
--       http://hal.archives-ouvertes.fr/docs/00/44/60/37/PDF/ARS-Journal-SurveyPatternRecognition.pdf
-- Called by: Measure_Elongate_Length
********************************************************************************
*/
   rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   XYs         MDSYS.SDO_ORDINATE_ARRAY := inXYs;
   mu_matrix   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

   n           PLS_INTEGER := XYs.count-2;
   Area        NUMBER := 0.0;
   alphax      NUMBER := 0.0;
   alphay      NUMBER := 0.0;
   alphaxx     NUMBER := 0.0;
   alphayy     NUMBER := 0.0;
   alphaxy     NUMBER := 0.0;
   alphax2     NUMBER;
   alphay2     NUMBER;
   Nalphaxx    NUMBER;
   Nalphayy    NUMBER;
   b2m4c       NUMBER := 0.0;
   area6       NUMBER;
   xLL         NUMBER;
   xUR         NUMBER;
   yLL         NUMBER;
   yUR         NUMBER;
   alphai      NUMBER;
   angle       NUMBER;
   dI2dalpha2  NUMBER;
   nn          PLS_INTEGER := TRUNC(n/2) ;
   nring       PLS_INTEGER := TRUNC(Info.count/3);
   ii          PLS_INTEGER := 1;
   istart      PLS_INTEGER;
   iend        PLS_INTEGER;
   kount       PLS_INTEGER;

   top         NUMBER;

BEGIN

--     For this rectangle, area is 40, alphax =5 alphay = 4
--                          alphaxx = 30.6667, alphayy = 18.6667, alphaxy = 22.0
--
--        Vertex is +                         + (8,8)
--                                     .       \
--                               .              \
--                    |     .                    + (10,4)
--                (0,4) +                  .
--                    |  \           .
--                    |   \     .
--                    |    +
--                          (2,0)

-- The rounding here is critical since a 38 digit multiply will yield a 76
-- digit result. Removing the value of the first point helps usually removing
-- at least 4 digits (nn.nn). So rounding with 6 or 12 decimal places (suggested)
-- (take away 2 decimal places from removing the 1st point) still leaves lots of
-- room for Oracle to calculate differences and not run out of precision.

-- In actual tests, I noticed differences in the alphas in the 6th significant
-- digit using this simple geometry: Ord2 :=  MDSYS.SDO_ORDINATE_ARRAY
--(-122.657878656454,47.9311406639453,-122.657802, 47.931107,-122.657702404526, 47.9311260697458,-122.657722, 47.931156,-122.657878656454, 47.9311406639453);

  if round_it is not NULL then
    ii := 1;
    Xys(1) := ROUND(Xys(1),round_it);
    Xys(2) := ROUND(Xys(2),round_it);
    For i in 1..nn loop
      ii := ii+2;
      XYs(ii) := ROUND(Xys(ii),round_it) -Xys(1);
      XYs(ii+1) := ROUND(Xys(ii+1),round_it) -Xys(2);
    End loop;
    Xys(1) := 0.0;
    Xys(2) := 0.0;

  end if;

-- Now implement the first and 2nd order moments using equations 33 to 39 (Ref 1)

  mu_matrix.extend(12);
       FOR loops in 1..nring LOOP
 
          istart := loops*3+1;
          if loops*3 = Info.count then
                iend := Xys.count-2;
          else
                iend := Info(istart)-3;
          end if;
          area := area*2.0;   -- This is to enable area accumulation
          ii := Info(istart-3);
          kount := TRUNC((iend-ii)/2)+1;  -- Ignore last vertex (same as start)


          for i in 1..kount loop
--            The 0th moment - area integral of 1dxdy
              alphai := (XYs(ii)*XYs(ii+3) - XYs(ii+2)*XYs(ii+1));
              area := area + alphai;
--            The first moments:  Integral of xdxdy and ydxdy

--             Sum of (xi  +  xi+1) (xi * yi+1 - xi+1 * yi)

               alphax := alphax + (XYs(ii) +  XYs(ii+2)) * alphai;

--             Sum of (yi  +  yi+1) (xi * yi+1 - xi+1 * yi)
               alphay := alphay + (XYs(ii+1) +  XYs(ii+3)) * alphai;

--             Now the 2nd moments: integrals of x^2, xy and y^2

--             Sum of (xi^2  +  xi*xi+1 + xi+1^2) (xi * yi+1 - xi+1 * yi)
               alphaxx := alphaxx + (XYs(ii)*XYs(ii) + Xys(ii)* XYs(ii+2) + XYs(ii+2)*Xys(ii+2)) * alphai;
--             Sum of (yi^2  +  yi*yi+1 + yi+1^2) (xi * yi+1 - xi+1 * yi)
               alphayy := alphayy + (XYs(ii+1)*XYs(ii+1) + Xys(ii+1)* XYs(ii+3) + XYs(ii+3)*Xys(ii+3)) * alphai;
--             Sum of (2xiyi  +  xi*yi+1 + xi+1*yi +2xi+1*yi+1) (xi * yi+1 - xi+1 * yi)
               alphaxy := alphaxy + (2.*XYs(ii)*XYs(ii+1) + Xys(ii)* XYs(ii+3) + XYs(ii+2)*Xys(ii+1) + 2.*Xys(ii+2)*XYs(ii+3)) * alphai;
               ii := ii+2;
          end loop;
--          dbms_output.put_line('area ' || area);


      area6 := area* 3.0;  -- 6 A
      area := area * 0.5;   -- adjust area

--dbms_output.put_line('Area ' || area);


-- Now setup mumatrix
--  mu_matrix.extend(4);
--  mu_matrix(1) := alphayy - alphay * alphay;  -- muyy
--  mu_matrix(2) := -(alphaxy- alphax*alphay);  --  - muxy (NOTE negative sign !!)
--  mu_matrix(3) := mu_matrix(3);               --  - muxy
--  mu_matrix(4) := alphaxx - alphax*alphax;    -- muxx

-- Now implement equations
--  alphax := alphax/(6.*area);  -- equation 33
--  alphay := alphay/(6.*area);  -- equation 34
--  alphaxx := alphaxx/(12.*area);  -- equation 36
--  alphayy := alphayy/(12.*area);  -- equation 37
--  alphaxy := alphaxy/(24.*area);  -- equation 39
  ---

-- To handle these equation multiply the equations by 6area * 6area
-- These are numerically stable.

--  muxx := alphaxx*3.*area - alphax*alphax;
--  muyy := alphayy*3.*area - alphay * alphay;
--  muxy := alphaxy*1.5*area - alphax*alphay;

-- ugly work around for polygons with zero area  (ideally these sorts of polygons
-- are not passed to this program.
  if area = 0.0 then
    area := 1.0;
  end if;

-- Now setup mu matrix

  mu_matrix(1) := (alphayy*3.*area - alphay * alphay);  -- muyy
  mu_matrix(2) := -(alphaxy*1.5*area - alphax*alphay);  -- -muxy
  mu_matrix(3) := mu_matrix(2);              -- - muxy
  mu_matrix(4) := (alphaxx*3.*area - alphax*alphax);     -- muxx
  mu_matrix(5) := area;
  top := (alphay - alphax) * (alphax+alphay);
  top := 3.*area*(alphaxx-alphayy) + top;
  alphax2 := alphax*alphax;
  alphay2 := alphay*alphay;
-- This seems to be the best factoring so far of b*b- 4ac used in Eigenvectors
-- but it can still go negative. (But was that before removing Xys(1),Xys(2)?)
--  b2m4c := area*area*9.*((alphaxx-alphayy)*(alphaxx-alphayy) + alphaxy*alphaxy)
--          -12.*area*alphaxy*alphax*alphay
--          + (alphax2+alphay2)*(alphax2+alphay2)
--          + area6*(alphaxx-alphayy) *(alphay2-alphax2);
--  if b2m4c < 0.0 then
--    b2m4c := 0.0;
--  end if;

  mu_matrix(6) := b2m4c;

  mu_matrix(8) := alphax/(6.*area);  -- equation 33
  mu_matrix(9) := alphay/(6.*area);  -- equation 34
  mu_matrix(10) := alphaxx/(12.*area);  -- equation 36
  mu_matrix(11) := alphayy/(12.*area);  -- equation 37
  mu_matrix(12) := alphaxy/(24.*area);  -- equation 39

--   dbms_output.put_line('area ' || round(area,12));
--   dbms_output.put_line('alphax ' || round(mu_matrix(8),12) || ' alphay ' || round(mu_matrix(9),12));
--   dbms_output.put_line('alphaxx ' || round(mu_matrix(10),12) || ' alphayy ' || round(mu_matrix(11),12) ||' alphaxy ' || round(mu_matrix(12),8));
--  dbms_output.put_line('Nalphaxx ' || round(mu_matrix(10)/area,12) || ' Nalphayy ' || round(mu_matrix(11)/area,12));


-- Calculate Normalized alphaxx and alphayy
   Nalphaxx := mu_matrix(10)/area;
   Nalphayy := mu_matrix(11)/area;
--dbms_output.put_line('mu4-1 ' || (mu_matrix(4)-mu_matrix(1)) || ' mu4  ' || round(mu_matrix(4),12) ||' mu1 ' || round(mu_matrix(1),12) || ' mu2 ' || round(mu_matrix(2),12));

-- Calculated the attidue angle as zero when the normalized alphaxx and alphayy are close
--   if ABS(Nalphaxx-Nalphayy) <0.0005 then
--       angle := 0.0;
--dbms_output.put_line('top  ' || top);

-- Calculated the attidue angle using a pragmatic way based upon the MBR
-- when the mu_values in the formula are close to zero
--  elsif ABS( mu_matrix(4) - mu_matrix(1)) < 1.e-20 OR ABS(mu_matrix(2)) < 1.E-5 then
--    angle := find_attitude(Xys);
-- Calculated the attidue angle using the mu_formula = (0.5*atan2*(2.*muxy,(muxx-muyy))

    if mu_matrix(4) <> mu_matrix(1) and mu_matrix(2) <> 0.0 then
--    dbms_output.put_line('muxy/muxx-muyy ' || round(mu_matrix(2)/(mu_matrix(4) - mu_matrix(1)),12));
-- angle := rad2deg*0.5*GZ_UTIL_ZONE.atan2((2.*(-mu_matrix(2)))/(mu_matrix(4)-mu_matrix(1)),1. );
        angle := GZ_UTIL_ZONE.atan2((2.*(-mu_matrix(2))),(mu_matrix(4)-mu_matrix(1)) );
-- dbms_output.put_line('working angle' || round(angle,12));

-- "A Survey of Shape Feature Extraction Techniques" : page 4
        dI2dalpha2 := (mu_matrix(4)-mu_matrix(1)) * cos(angle) - mu_matrix(2)*sin(angle);
        angle := angle*rad2deg*0.5;
        if dI2dalpha2 < 0.0 then   -- (mu_matrix(4)-mu_matrix(1)) < 0.0 then
        dbms_output.put_line('working angle' || round(angle,12) || ' ' || debug);
          angle :=  angle + 90.;
          dbms_output.put_line('Working angle' || round(angle,12));
        end if;
        if angle < 0.0 then
          angle :=  angle + 180.;
        end if;

    elsif abs(xLL-xUR) > abs(yLL-yUR) then
        angle := 0.0;
    else

        angle := 90.0;
    end if;
--  dbms_output.put_line('angle ' || round(angle,12));
   mu_matrix(7) := angle;
   END LOOP;
--dbms_output.put_line('mu_matrix(1)++' || (mu_matrix(1) + 0.5*mu_matrix(2)));
--dbms_output.put_line('mu_matrix(14)+' || (0.5*mu_matrix(1) + 0.5*mu_matrix(4)));
  RETURN mu_matrix;

END MOMENTS;
--
FUNCTION POINT_IN_POLY_CC( Xc     NUMBER,
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
FUNCTION CIRCULATE_GEOMETRY(Geometry MDSYS.SDO_GEOMETRY,shift number) RETURN MDSYS.SDO_GEOMETRY AS

 New_Geometry MDSYS.SDO_GEOMETRY;
 Xys MDSYS.SDO_ORDINATE_ARRAY := GEOMETRY.SDO_ORDINATES;
 New_Xys MDSYS.SDO_ORDINATE_ARRAY;
 
BEGIN
   New_Xys := Circulate_Coordinates(Xys,shift);
   New_Geometry := Geometry;
   New_Geometry.sdo_ordinates := New_Xys;
   RETURN New_Geometry;
END;

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
  
END CIrculate_Coordinates;
--
FUNCTION ROTATE_COORDINATES(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,angle NUMBER,x0 NUMBER,y0 NUMBER) RETURN MDSYS.SDO_ORDINATE_ARRAY AS

-- Rotates a coordinate array counter clockwise about the point (x0,y0) by the
-- angle in degrees.

    deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
    Rotated_Xys     MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    n               PLS_INTEGER := TRUNC(Xys.count/2);
    jj              PLS_INTEGER;
    x               NUMBER;
    y               NUMBER;
    sin0            NUMBER := sin(angle*deg2rad);
    cos0            NUMBER := cos(angle*deg2rad);
BEGIN

  Rotated_Xys.extend(Xys.count);

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

  RETURN Rotated_Xys;


END ROTATE_COORDINATES;
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
FUNCTION AREA_ALONG_LINE(In_start PLS_INTEGER, In_End PLS_INTEGER,
                         Xys     IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         In_Degrees Boolean default TRUE,
                         pInUseProjection Boolean default FALSE
                                ) RETURN MDSYS.SDO_LIST_TYPE
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
  ---     In_start, In_End : Start and end elements of the polygons in the
  --                        Xes, Yes arrays.
  --      Xes            - an array of X coordinates
  --      Yes            - an array of Y coordinates
  --      In_Degrees     - whether Xes, yes are in degrees
  --      pInUseProjection - whether to use a Lambert Azimuthal Equal Area projection

  --      OUTPUT
  --       Returns areas of simple (closed and not intersecting) polygons
  --       in square meters. Areas(1) contains total. If a projection
  --       is not used and the input is in degrees the result is multiplied by
  --       the cos of the latitude (to correct for the meridians getting closer
  --       towards the pole) and better match the result from SDO_GEOM.SDO_AREA .

--Purpose:
  --  Calculates area using the herringbone method. See area_of_line to
  --  get the input to this function constructed.

--Reference:  "Measurements from Maps" DH Maling. Pergamon Press 1989, page 338
--Dependencies:  GZ_UTIL_ZONE.sincos
--
********************************************************************************
*/


  poly_geometry     MDSYS.SDO_GEOMETRY;
  Areas             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
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

  deg2rad      CONSTANT NUMBER    :=0.0174532925199432957692369076848861271344;
  sql_stmt          VARCHAR2(4000);
  total_area        NUMBER := 0.0;
  area              NUMBER := 0.0;
  R                 NUMBER := 6378137.0;
  to_meters         NUMBER := 6173827200.;
  g                 NUMBER;
  ksp               NUMBER;
  factor            NUMBER;
  xstart            NUMBER;
  ystart            NUMBER;
  pa                PLS_INTEGER := 1;
  px                PLS_INTEGER := 0;
  oarea             NUMBER := 0.;
  pos_area          NUMBER := 0.0;
  yfactor           NUMBER;
  dist_factor       NUMBER := 6196014515.236907424901819538; --111319.490793274^2 * 0.5;
BEGIN


   If Xys.count = 0 then
      RETURN areas;
   End If;

   pos := In_Start;
   areas.extend(1);
   areas(1) := 0.0;
   x0 := XYs(pos);
   y0 := XYs(pos+1);
   xstart := x0;
   ystart := y0;

   pos := pos + 2;
   y := ABS(XYs(In_Start+1)+XYs(In_End))* 0.5;
   yc := y* deg2rad ;
   sinyc := GZ_UTIL_ZONE.sincos(yc,cosyc);

   IF pInUseProjection = TRUE THEN
-- Convert to radians and then do a Lambert Azimuthal Equal Area projection
      x0 := x0* deg2rad;
      y0 := y0 * deg2rad;
      xstart := x0;
      ystart := y0;
      xc := (x0+XYs(In_End-1)* deg2rad) * 0.5;
      yc := (y0+XYs(In_End)* deg2rad) * 0.5;
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

   WHILE pos < In_End LOOP
    IF pInUseProjection = TRUE THEN

-- Convert to radians and then do a Lambert Azimuthal Equal Area projection
        x1 := XYs(pos) * deg2rad;
        y1 := XYs(pos+1) * deg2rad;
        x1_c := (x1-xc);

        siny1 := GZ_UTIL_ZONE.sincos(y1,cosy1);
        sinx1_c := GZ_UTIL_ZONE.sincos(x1_c,cosx1_c);

-- Project X1, Y1
-- X = ksp * cos(lat) * sin(dlon)
-- Y = ksp * ( cos_center_lat * sin(lat) - sin_center_lat * cos(lat) * cos(dlon)

        X11 := ksp*cosy1 * sinx1_c;
        Y11 := ksp*(cosyc * siny1 - sinyc * cosy1 * cosx1_c);
-- Calculate areas correctly using the herringbone method (Maling page 338).
        area := area + (x00 * y11   - x11 * y00);
--        dbms_output.put_line('pos ' || pos || ' area ' || round(area,4));
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
       x1 := XYs(pos);
       y1 := XYs(pos+1);
       area := area + (x0 * y1   - x1 * y0 );
     end if;

-- Have we finished the current polygon?

     if x1 = xstart and y1 = ystart then
          total_area := total_area + area;
          Areas.extend(1);
          pa := pa + 1;
--          dbms_output.put_line(' storing area ' || round(total_area,10));
          Areas(pa) := area;
          area := 0.0;
          pos := pos + 2;

-- move to the start of the next polygon

          if pos < in_end then
             x1 := XYs(pos);
             y1 := XYs(pos+1);

             IF pInUseProjection = TRUE THEN
                x0 := x0* deg2rad;
                y0 := y0 * deg2rad;
                x1 := x1* deg2rad;
                y1 := y1* deg2rad;
             END IF;
             xstart := x1;
             ystart := y1;
          end if;
       end if;

     x0 := x1;
     y0 := y1;
     pos := pos + 2;

  END LOOP;

--

  For ii in 2..Areas.count Loop
   IF pInUseProjection = TRUE THEN
       areas(ii) := 0.5*areas(ii);
   ELSE
      IF In_degrees = FALSE THEN
           areas(ii) := 0.5*areas(ii);
      ELSE
        y := ABS(XYs(In_Start+1)+XYs(In_End))* 0.5;
        yc := y* deg2rad ;
        sinyc := GZ_UTIL_ZONE.sincos(yc,cosyc);
-- This empirical factor was found by comparing the results with Oracle's area
-- function SDO_GEOM.SDO_AREA. Results are amazing
--        factor := 0.997 + Yes(in_Start) *  Yes(in_Start) * 3.5E-06 - abs(yes(In_Start)) *0.9E-6;
--        factor := 0.9968 + Yes(in_Start) *  Yes(in_Start) * 3.47E-06 - abs(yes(In_Start)) *2.2E-6;
---        factor := 0.9969 + Yes(in_Start) *  Yes(in_Start) * 3.485E-06 - abs(yes(In_Start)) *2.4E-6;
        factor := 0.9965 + XYs(in_Start+1) *  XYs(in_Start+1) * 1.6E-06 + abs(XYs(In_Start+1)) *7.6E-5;
         areas(ii)  := factor * to_meters * cosyc*areas(ii);

      End If;
   END IF;
   areas(1) := areas(1) + ABS(areas(ii));
   End Loop;
-- Anti-Clockwise areas are positive. Area will be negative if the order of
-- traversal is clockwise.

  RETURN Areas;


END AREA_ALONG_LINE;
--
FUNCTION FIND_WAY_POINT(NEWXYs IN MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER) RETURN NUMBER AS
-- Find furthest waypoint (from the start) along an edge
  x1          NUMBER;
  y1          NUMBER;
  x2          NUMBER;
  y2          NUMBER;
  dist        NUMBER :=0;
  best_dist   NUMBER :=0.0;
  way_point   PLS_INTEGER;
  n           PLS_INTEGER := TRUNC(NewXys.count/2);
BEGIN
   x1 := NewXys(1);
   y1 := NewXys(2);
   For ii in 2..n Loop
     x2 := NewXys(ii*2-1);
     y2 := NewXys(ii);
     dist := Distance_fcn(x1,y1,x2,y2,SRID);
     if dist > best_dist then
        best_dist := dist;
        way_point := ii;
     end if;
   End Loop;
   Return way_point;

END FIND_WAY_POINT;
--
FUNCTION AREA_OF_LOOP(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      In_Degrees Boolean default TRUE,
                      way_point IN OUT NOCOPY PLS_INTEGER, SRID NUMBER,
                      pInUseProjection Boolean default FALSE) RETURN NUMBER AS

  partXys            MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();

  max_area           NUMBER := 0.0;
  area2              NUMBER;
  pos                PLS_INTEGER;

BEGIN

  IF Xys.count = 8 THEN
    max_area := Polygon_area(Xys);

  ELSE
     if way_point is NULL then
       way_point := Find_Way_point(Xys,SRID);
       if way_point*2 > Xys.count - 4 then
         way_point := TRUNC((Xys.count-4)/2);
       end if;
     end if;
     partXys := Xys;
     partXys.trim(partXys.count - way_point*2-2);
     PartXys(Partxys.count-1) := Xys(1);
     PartXys(Partxys.count) := Xys(2);
--     dbms_output.put_line('callin area of line' || xys.count || ' way ' || way_point || 'part ' || partxys.count);
     max_area := Area_of_Line(partXys);
     partXys.trim(partXys.count);
     partXys.extend(Xys.count -way_point*2+4);
     pos := way_point*2-2;
     For ii in 1..partXys.count-2 Loop
       pos := pos + 1;
       partXys(ii) := Xys(pos);
     End Loop;
     PartXys(Partxys.count-1) := partXys(1);
     PartXys(Partxys.count) := partXys(2);
--     dbms_output.put_line('partxys' || partxys.count);
--     dbms_output.put_line('calling area of line' || pos);
     area2 := Area_of_Line(partXys);
     if area2 > max_area then
         max_area := area2;
     end if;
   END IF;

   RETURN max_area;


END AREA_OF_LOOP;
--
FUNCTION AREA_OF_LINE(XYs IN MDSYS.SDO_ORDINATE_ARRAY,
                      In_Degrees Boolean default TRUE,
                      pInUseProjection Boolean default FALSE) RETURN NUMBER AS
/*******************************************************************************
--Program Name: area_of_line
--Author: Sidey Timmins
--Creation Date: 06/7/2011
--Updated:

--Usage: area := area_of_line([0,0,5,5,10,-5,10,0],FALSE,FALSE);
--  This function has 3 parameters:

 --   REQUIRED Parameters:
  --      INPUT
  --      Xys              - an array of Y coordinates
  --      In_Degrees       - whether Xys are in degrees
  --      pInUseProjection - whether to use a Lambert Azimuthal Equal Area projection
  --                         or just estimate the area. The estimate matches
  --                         the SDO_GEOM.SDO_Area value better than the projected area.

  --      OUTPUT
  --       Returns area of a simple (closed and not self-intersecting) polygon
  --       in square meters. If a projection is not used and the input is in
  --       degrees the result is multiplied by the cos of the latitude
  --       (to correct for the meridians getting closer towards the pole) and
  --       better match the result from SDO_GEOM.SDO_AREA .

--Purpose:
  --  Calculates area using the herringbone method.

--Reference:  "Measurements from Maps" DH Maling. Pergamon Press 1989, page 338
--Dependencies:  GZ_UTIL_ZONE.sincos
--
********************************************************************************
*/

    geom1           MDSYS.SDO_GEOMETRY;
    geom2           MDSYS.SDO_GEOMETRY;
    poly_geometry   MDSYS.SDO_GEOMETRY;
    intersection    MDSYS.SDO_GEOMETRY;
    xy_intersect    MDSYS.SDO_ORDINATE_ARRAY;
    Poly_xys        MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Areas           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Area_sign       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    nseg            PLS_INTEGER;
    n               PLS_INTEGER;
    pos             PLS_INTEGER := 0;
    vertex_count    PLS_INTEGER;

    pa              PLS_INTEGER := 1;
    xstart          NUMBER;
    ystart          NUMBER;
    xend            NUMBER;
    yend            NUMBER;
    x1              NUMBER;
    y1              NUMBER;
    x2              NUMBER;
    y2              NUMBER;
    xi              NUMBER;
    yi              NUMBER;
    xc              NUMBER;
    yc              NUMBER;
    pos_area        NUMBER;
    area_above      NUMBER := 0.0;
    area_below      NUMBER := 0.0;
    det             NUMBER;
    tolerance       NUMBER := 0.00000005;
    area            NUMBER := 0.0;
    max_area        NUMBER := 0.0;


FUNCTION ORIENT2D(paX NUMBER, paY NUMBER,
                  pbX NUMBER, pbY NUMBER,
                  pcX NUMBER, pcY NUMBER)

            RETURN NUMBER AS

/*
********************************************************************************
--Program Name: c_orient2D
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
  --             in clockwise order; and zero if they are collinear.
********************************************************************************
*/
  det          NUMBER;
BEGIN
   det   := (paX - pcX) * (pbY - pcY) - (paY - pcY) * (pbX - pcX);
   RETURN det;
END;
BEGIN

-- Calculate the intersections between the straight line going from the start
-- vertex to the end vertex and the edge itself.

  n := XYs.count;
  xstart := Xys(1);
  ystart := Xys(2);
  xend := Xys(n-1);
  yend := Xys(n);
--  dbms_output.put_line(' xs ' || xstart || ' ys ' || ystart || ' xe ' || xend || ' ye ' || yend);
-- A closed loop has to be subdivided first into 2 shorter
  if n <= 4  or (xstart = xend and ystart = yend) then
     RETURN NULL;
  end if;


--  for ii in 1..Xys.count loop
--     if mod(ii,2) = 0 then
--        dbms_output.put_line('ii ' || ii || ' x ' || xys(ii-1) || ' y ' || xys(ii));
--     end if;
--  end loop;
  nseg := TRUNC(n/2);
  Poly_Xys.extend(nseg*8);
  Area_sign.extend(1);
-- Begin by adding the start vertex
  pos := pos + 2;

  Poly_Xys(pos-1) := xstart;
  Poly_Xys(pos) := ystart;
  geom1 := MDSYS.SDO_GEOMETRY(2002,NULL,NULL,mdsys.sdo_elem_info_array(1,2,1),
                                            mdsys.sdo_ordinate_array(xstart,ystart,xend,yend));
  x2 := Xys(3);
  y2 := Xys(4);
  xi := xstart;
  yi := ystart;
--
-- Build triangles from consecutive counter clockwise vertices like this.
-- The + are existing vertices and the * are intersected points along the way:
-- (xi,yi) and (xI,yI).
-- The triangles can be specified as a vertex list: example 1,i,2,1 stands for
-- (x1,y1,xi,yi,x2,y2,x1,y1).
-- The first triangle is 1,i,2,1; the 2nd is i,3,I,i and the 3rd is i,5,4,I.
--
--              + (x2,y2)          (x4,y4)
--             /   \             +
--           /      \ (xi,yi)  /  \
--          + (x1,y1) *       *     +  (x5,y5)
--                     \    / (xI,yI)
--                      \ /
--                       + (x3,y3)
--
-- Check from the 2nd to the 2nd to last
  for ii in 2..nseg-1 loop
    x1 := x2;
    y1 := y2;
    x2 := Xys(ii*2+1);
    y2 := Xys(ii*2+2);
-- Add the start of the current segment
-- except when coincidentally the last intersection point is one of the
-- vertexes. ( try testing this procedure with this data:
--  xys mdsys.sdo_ordinate_array := mdsys.sdo_ordinate_array(1,1,2,2,4,0,4.5,0.5,5,1,6,2,7,1);
--  area := area_of_line(xys);
-- We don't want the vertex 5,1 because it lies on the line 1,1 to 7,1.
--dbms_output.put_line('pos ' || pos || ' x1 ' || x1 || ' y1 ' || y1 || ' x2 ' || x2 || ' y2 ' || y2);
    IF x1 = xi and y1 = yi THEN
       NULL;
    ELSE
    pos := pos + 2;
--    dbms_output.put_line('pos ' || pos || ' x1 ' || x1 || ' y1 ' || y1 || ' x2 ' || x2 || ' y2 ' || y2);
    Poly_Xys(pos-1) := x1;
    Poly_Xys(pos) := y1;

    geom2 := MDSYS.SDO_GEOMETRY(2002,NULL,NULL,mdsys.sdo_elem_info_array(1,2,1),
                                            mdsys.sdo_ordinate_array(x1,y1,x2,y2));
     if x2 = xend and y2 = yend then
       intersection := NULL;
     else
     intersection := SDO_GEOM.SDO_INTERSECTION(geom1,geom2,tolerance);
     end if;
     if intersection is not NULL then
        pos := pos + 2;
        xy_intersect := intersection.sdo_ordinates;
-- Add the intersection and then complete by adding the start vertex
        xi := xy_intersect(1);
        yi := xy_intersect(2);

        Poly_Xys(pos-1) := xi;
        Poly_Xys(pos) := yi;
        pos := pos + 2;
        Poly_Xys(pos-1) := xstart;
        Poly_Xys(pos) := ystart;
        Poly_xys.trim(poly_xys.count-pos);
--        dbms_output.put_line('calling pA'|| poly_xys.count);
        area := ABS(Polygon_area(Poly_xys));
        if area > max_area then
          max_area := area;
        end if;
--        dbms_output.put_line('a ' || area);
-- We know that the orientation of the polygon can be determined just from
-- 3 vertices, the end(start) of the polygon, the intersection point and the
-- last vertex
--                                 +  (x1,y1)
--                                  \
--                                   \
--         (xend,yend) +-----------------+ (xi,yi)

        det := Orient2D(x1,y1,xi,y1,Poly_Xys(pos-1),Poly_Xys(pos));
        pa := pa + 1;
        Area_sign.extend(1);
        area_sign(pa) := 1.;
        if det < 0.0 then
          area_sign(pa) := -1.;
          GZ_UTIL_ZONE.reverse_ordinates(Poly_Xys,1,pos);
        end if;
-- Begin the next polygon area by changing xstart, ystart
        xstart := xi;
        ystart := yi;
--        dbms_output.put_line('xi ' || xi || ' yi ' || yi);
        if xstart <> xend or ystart <> yend  then
          Poly_xys.trim(Poly_xys.count);
          Poly_Xys.extend(nseg*8);
          pos := 2;

          Poly_Xys(pos-1) := xstart;
          Poly_Xys(pos) := ystart;
--          dbms_output.put_line('setting xstart ' );
        end if;
     elsif ii = nseg-1 and x2 = xstart and y2 = ystart then
        pos := pos + 2;
        Poly_Xys(pos-1) := xstart;
        Poly_Xys(pos) := ystart;
        Poly_xys.trim(poly_xys.count-pos);
--        dbms_output.put_line('calling pa' || poly_xys.count);
        area := ABS(Polygon_area(Poly_xys));
        if area > max_area then
          max_area := area;
        end if;
--       dbms_output.put_line('aa ' || area);
        xstart := xend;
        ystart := yend;
     end if;
     END IF;

  end loop;

-- Add the current segment's end vertex, then the end of the line
-- and then the start of the current area
  if xstart <> xend or ystart <> yend  then
    pos := pos + 2;
    Poly_Xys(pos-1) := x2;
    Poly_Xys(pos) := y2;
    pos := pos + 2;
    Poly_Xys(pos-1) := xend;
    Poly_Xys(pos) := yend;
    pos := pos + 2;
    Poly_Xys(pos-1) := xstart;
    Poly_Xys(pos) := ystart;

--    dbms_output.put_line('x1 ' || round(poly_xys(1),6) || ' y1 ' || round(poly_xys(2),6));
--    dbms_output.put_line('x2 ' || round(poly_xys(3),6) || ' y2 ' || round(Poly_xys(4),6));
--    dbms_output.put_line('x3 ' || round(xend,6) || ' y3 ' || round(yend,6));
--    dbms_output.put_line('xs ' || round(xstart,6) || ' ys ' || round(ystart,6));
    det := Orient2D(x2,y2,xend,yend,xstart,ystart);
--    dbms_output.put_line('completing polygon ' || pos);
    pa := pa + 1;
    Area_sign.extend(1);
    area_sign(pa) := 1.;
    if det < 0.0 then
          area_sign(pa) := -1.;
          GZ_UTIL_ZONE.reverse_ordinates(Poly_Xys,1,pos);
    end if;
  end if;
  Poly_xys.trim(poly_xys.count-pos);
--  dbms_output.put_line('calling ppA'|| poly_xys.count);
  area := ABS(Polygon_area(Poly_xys));
--  dbms_output.put_line('A' || area);
  if area > max_area then
     max_area := area;
   end if;
-- for ii in 1..pos loop
--     if mod(ii,2) = 0 then
--        dbms_output.put_line('ii ' || ii || ' x ' || poly_xys(ii-1) || ' y ' || poly_xys(ii));
--     end if;
--  end loop;
-- Then pass the coordinates to area along a line to calculate the areas
--  areas := area_along_line(1,pos,Poly_Xys,In_Degrees,pInUseProjection);

-- areas(1) contains the total.

--  dbms_output.put_line('A' || areas.count || ' as ' || area_sign.count || ' xys ' || xys.count);
--  max_area := 0.0;
--  For ii in 2..areas.count Loop
--   if area_sign(ii) < 0.0 then
--      area_above := area_above + areas(ii);
--   else
--      area_below := area_below + areas(ii);
--   end if;
--   if areas(ii) > max_area then
--      max_area := areas(ii);
--   end if;
--  End Loop;

  RETURN max_area;


END AREA_OF_LINE;
--
FUNCTION POLYGON_AREA(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER DEFAULT 8265.)
           RETURN NUMBER DETERMINISTIC IS

-- Calculate Polygon area using spherical triangles.
-- Error (compared to Oracle) using this spheroidal approximation is < 0.01%

/*
********************************************************************************
--Program Name: polygon_area
--Author: Sidey Timmins
--Creation Date: 7/25/2011

--Usage:  Xys := Geometry.sdo_ordinates;
--        Area (in meters ) := Polygon_Area(Xys);

  --  Parameters:
  --      Xys           - the ordinates of a single closed polygon
  --      SRID          - The Oracle spatial reference id (projection)
  --
-- Purpose: Computes area on a sphere by dividing the ordinates into spherical
  --        triangles. Remembering the earth is actually an ellipsoid with 1/273 
  --        flattening the results are extremely accurate on a percentage basis.
********************************************************************************
*/
  
  n            PLS_INTEGER := TRUNC(XYs.count/2);
  next        PLS_INTEGER := 2;
  ii           PLS_INTEGER := 1;
  iend         PLS_INTEGER:= XYs.count;
  
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

  R2           NUMBER := 40680631590769.; --40589732742451.84;
  -- Calculate factor = pi/(180.*dist_factor) 
  -- dist_factor NUMBER := 111319.490793274;
  factor       NUMBER := .0000001567855942887391978298823437439754779984;
  
  Area         NUMBER := 0.0;

FUNCTION ORIENT2D(paX NUMBER, paY NUMBER,
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
  --      paX,paY           - a point (x,y)
  --      pbX,pbY           - a 2nd point (x,y)
  --      pcX,pcY           - a 3rd point (x,y)
-- Purpose:
  -- Return a positive value if the points a, b, and c occur
  --             in counterclockwise order; a negative value if they occur
  --             in clockwise order; and zero if they are collinear.
********************************************************************************
*/
  det          NUMBER;
BEGIN
   det   := (paX - pcX) * (pbY - pcY) - (paY - pcY) * (pbX - pcX);
   RETURN det;
END;

BEGIN

    x2 := Xys(1);
    y2 := Xys(2);
    x3 := Xys(n*2-3);
    y3 := Xys(n*2-2);

    if x2 <> xys(n*2-1) or y2 <> xys(n*2) then
      dbms_output.put_line('>>> ITS NOT a valid polygon <<<');
    end if;
    
    iend := Xys.count-2;  -- points to y coordinate of n-1 vertex
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
    
    b := distance_fcn(x3,y3,x2,y2)*factor;

--  Do the triangles consecutively: 1,2,4 and then 2,3,4
--
--        +  4
--       /  \   -
--      /    \      -
--     /      \          +  3
--    /         \     .
--   +-----------+ .
--   1           2


    While next <= Xys.count-6 Loop
        x1 := x2;
        y1 := y2;
        next := next + 2;
        x2 := Xys(next-1);
        y2 := Xys(next);
        c := b;
  
        a := distance_fcn(x1,y1,x2,y2)*factor;
  
        b := distance_fcn(x2,y2,x3,y3)*factor;
--        dbms_output.put_line('a' || a || ' b ' || b || ' c ' || c);
  
        s := 0.5*(a+b+c);
--        dbms_output.put_line('s' ||s);
        if (s-a) > 0. and (s-b) > 0. and (s-c) > 0. then
          sph_tri_area := new_arctan(sqrt(c_tan(s*0.5)*c_tan((s-a)*0.5)*c_tan((s-b)*0.5)*c_tan((s-c)*0.5)),1.);
        end if;
        
        if Orient2d(x1,y1,x2,y2,x3,y3) < 0.0 then
          area := area - sph_tri_area;
        else
          area := area + sph_tri_area;
        end if;
  
    End Loop;
  
    area := area * 4. * R2 ;
    RETURN AREA;

END;
--
FUNCTION Perimeter(XYs IN MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER default 8265.) RETURN NUMBER DETERMINISTIC AS

-- Return Perimeter (edge) length

  perimeter_len    NUMBER := 0.0;
  x1               NUMBER;
  y1               NUMBER;
  x2               NUMBER;
  y2               NUMBER;
 
BEGIN

   If Xys is NULL then
      RETURN 0.0;
   End If;
   
   x2 := Xys(1);
   y2 := Xys(2);
   For ii in 2..TRUNC(Xys.count/2) Loop
     x1 := x2;
     y1 := y2;
     x2 := XYs(ii*2-1);
     y2 := Xys(ii*2);
     perimeter_len := perimeter_len + distance_fcn(x1,y1,x2,y2,SRID);
   End Loop;

  RETURN perimeter_len;

END Perimeter;
--
FUNCTION SPHTRI_AREA(x1 IN NUMBER, y1 IN NUMBER,
                        x2 IN NUMBER, y2 IN NUMBER,
                        x3 IN NUMBER, y3 IN NUMBER,SRID NUMBER default 8265.)
   RETURN NUMBER DETERMINISTIC IS

-- Calculate area of a spherical triangle in square meters. Note the earth
-- is an ellipsoid, not a sphere, so this function uses an approximate conversion
-- factor to increase the result.

  area        NUMBER;
  a           NUMBER;
  b           NUMBER;
  c           NUMBER;
  s           NUMBER;
  approx_convert NUMBER :=1.000001187;   -- oracle area/sphtri_area for triangle with 0.01 sides

  R2          NUMBER := 40680631590769.;  -- 6378137 squared
  -- Calculate factor = pi/(180.*dist_factor) 
  -- dist_factor NUMBER := 111319.490793274;
  factor      NUMBER := .0000001567855942887391978298823437439754779984;
BEGIN

  a := distance_fcn(x1,y1,x2,y2)*factor;
  b := distance_fcn(x1,y1,x3,y3)*factor;
  c := distance_fcn(x3,y3,x2,y2)*factor;
  s := 0.5 *(a+b+c);
--  dbms_output.put_line('a ' || round(a,6));
--  dbms_output.put_line('b ' || round(b,6));
--  dbms_output.put_line('c ' || round(c,6));
--  dbms_output.put_line('s ' || round(s,6));

  area := 4.*faster_atan2(sqrt(c_tan(s*0.5)*c_tan((s-a)*0.5)*c_tan((s-b)*0.5)*c_tan((s-c)*0.5)),1)*R2*0.99999876 * approx_convert;

  RETURN AREA;

END;
--
FUNCTION FAST_DISTANCE( x1 IN OUT NOCOPY NUMBER, y1 IN OUT NOCOPY NUMBER,
                        x2 IN OUT NOCOPY NUMBER, y2 IN OUT NOCOPY NUMBER,SRID NUMBER default 8265.)
                       RETURN NUMBER DETERMINISTIC IS
/*
********************************************************************************
--Program Name: fast_distance
--Author: Sidey Timmins
--Creation Date: 03/17/2010
--Updated: 10/29/2010 To calculate its own sincos and to better match Charles
--         Karney's results from his excellent code.
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
--              Fast_distance returns the great circle distance between 2 points
--              quickly and accurately.
--              The great circle approximation is twenty times the speed of
--              sdo_geom.sdo_length and good to less than a few mm of error at
--              0.1 degrees difference in the coordinates.
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
********************************************************************************
*/
  deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
  gcd        NUMBER := 0.;
   b         NUMBER;
   t         NUMBER;

-- Jack Ganssle Cosine coefficients good to 12 digits
--   cc1       NUMBER :=  0.999999953464;
--   cc2       NUMBER := -0.4999999053455;
--   cc3       NUMBER :=  0.0416635846769;
--   cc4       NUMBER := -0.0013853704264;
--   cc5       NUMBER :=  0.000023233;
   cc1       NUMBER :=  0.99999999999925182;
   cc2       NUMBER := -0.49999999997024012;
   cc3       NUMBER :=  0.041666666473384543;
   cc4       NUMBER := -0.001388888418000423;
   cc5       NUMBER :=  0.0000248010406484558;
   cc6       NUMBER := -0.0000002752469638432;
   cc7       NUMBER :=  0.0000000019907856854;

   a         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(
            0.9933056431085355608716884491956183391161,
            0.009973972089654263505971285993829439678765,
            0.00008445133481869081114188534958332938725393,
           -0.0000000207345260587865496039317049547208138981);
   a1          NUMBER;
   a2          NUMBER;
   c1          NUMBER;
   c2          NUMBER;
   tt          NUMBER;
   y           NUMBER := abs(y1);
   yy          NUMBER;
   yy2         NUMBER;
   dx          NUMBER;
   dy          NUMBER;
   siny        NUMBER;
   sinysq      NUMBER;
   cosy        NUMBER;
   cos3y       NUMBER;
   cos6y       NUMBER;
   cosyy       NUMBER;
   sinyy       NUMBER;
   siny2       NUMBER;
   cosy2       NUMBER;
   cosdy       NUMBER;
   sindy       NUMBER;
   length      NUMBER;
   one6th      NUMBER := 0.1666666666666666666666666666666666667;
   delta       NUMBER;
   dist_factor NUMBER := 111319.490793274;
   sindelta    NUMBER;

BEGIN

-- Handle case when data is projected.

      if SRID is NULL or SRID <> 8265. then
         length := sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1));
         RETURN length;
      end if;

-- Handle large angles (> 0.1 degrees)
      dx := x2-x1;
      dy := y2-y1;

      if (abs(dx) > 0.1 or abs(dy) > 0.1) then
        gcd := GZ_SUPER.fast_vincenty_gcd(x1,y1,x2,y2);
         RETURN gcd;
      end if;

-- Use a very accurate approximation (good to a few mm in 10,000 meters)

      yy := y*deg2rad;
      yy2 := yy*yy;

-- First generate sine and cosine of one latitude.
      IF yy < 1.5E-3 THEN  -- this is .0015 radians
-- SQL> select sin(.0015) a from dual;
--      .00149999943750006328124661
-- SQL> select 0.0015 - 0.0015*0.0015*0.0015/6 a from dual;
--      .00149999943750000000000000

--       siny :=  yy*(yy2*(yy2*(yy2*(yy2 *(-2.39E-08 * x2 + 2.7526E-06) - 1.98409E-04) + 8.3333315E-03)
--           -1.666666664E-01) + 1.0);
       siny := yy - yy*yy2*one6th;
       cosyy := sqrt(1.-siny*siny);

      ELSE

  -- Evaluate the cosine with Horner's rule
 --      cosyy := (cc1 + yy2*(cc2 + yy2*(cc3 + yy2*(cc4 + cc5*yy2))));
       cosyy := (cc1 + yy2*(cc2 + yy2*(cc3 + yy2*(cc4 + yy2*(cc5 + yy2*(cc6 + cc7*yy2))))));
       siny := sqrt(1.-cosyy*cosyy);

      END IF;

      delta := (y2-y1)*0.5*deg2rad;           -- calculate sin((y1+y2)*0.5)
      cosdy := 1. - delta*delta*0.5;           -- using sin(a+eps) = sin(a)cos(eps) + cos(a)sin(eps)
      sindelta := delta - delta*delta*delta*one6th;
      if y1 < 0.0 then
        siny := -siny;
      end if;
      siny2 := siny*cosdy + cosyy*sindelta;      -- small angle approximation for formula above



   if y < 34.5 then
      if y >= 27.5 then
--        p := 5;  -- for 27.5 .. 34.4999 degrees
        a1 := 0.9932996344181;
        a2 := 0.0100190915085;
        c1 := 0.99999882342768;
        c2 := 0.00335602313364;
        b := 1.99383172928565e-008;
        t := 31.;   -- centerpoint
      elsif y >= 20.5 then
--        p := 4;  -- for 20.5.. 27.4999 degrees
        a1 := 0.9933032652183;
        a2 := 0.0100024008612;
        c1 := 0.99999954254499;
        c2 := 0.00335267920716;
        b := 1.41304231339307e-008;
        t := 24.;
      elsif y >= 13.5 then
--        p := 3;  -- for 13.5 .. 20.4999 degrees
        a1 := 0.99330497373465;
        a2 := 0.00998904720322;
        c1 := 0.99999987773583;
        c2 := 0.00335000490016;
        b := 8.02456497494140e-009;
        t := 17.;
      elsif y >= 6.5 then
--        p := 2;  -- for 6.5 .. 13.4999 degrees
        a1 := 0.99330553341082;
        a2 := 0.00997977946635;
        c1 := 0.99999998463761;
        c2 := 0.00334815672;
        b := 3.00739642940506e-009;
        t := 10.;
      else
--        p := 1;  --for 0 .. 6.4999 degrees
        a1 := 0.99330562068398;
        a2 := 0.0099751554093;
        c1 := 0.99999999981136;
        c2 := 0.00334723822659;
        b := 2.60227919461295e-010;
        t := 3.;
      end if;
   elsif y < 52.5 then
      if y >= 45.5 then
--        p := 8;  --for 45.5 .. 52.4999 degrees
        a1 := 0.9932783482104 ;
        a2 := 0.0100699787885;
        c1 := 0.99999456031055;
        c2 := 0.00336625111507;
        b := 2.50221540058326e-008;
        t := 49.;
      elsif y >= 41.5 then
--        p := 7;  --for 41.5 .. 45.4999 degrees This range is different
        a1 := 0.9932845136377 ;
        a2 := 0.0100584521647;
        c1 := 0.99999581110019;
        c2 := 0.00336390796249;
        b := 2.56283586223214e-008;
        t := 45.;      -- nominal centerpoint for this unusual range
      else
--        p := 6;  --for 34.5 .. 41.4999 degrees
        a1 := 0.9932934620093 ;
        a2 := 0.0100381481123;
        c1 := 0.99999759542303;
        c2 := 0.00335984120472;
        b := 2.40962083085169e-008;
        t := 38.;
      end if;
   elsif y < 59.5 then
--     p := 9;  --for 52.5 .. 59.4999 degrees
     a1 := 0.9932657810331;
     a2 := 0.0100899846107;
     c1 := 0.99999207002972;
     c2 := 0.00337022115208;
     b := 2.20706708206713e-008;
     t := 56.;
   elsif y < 66.5 then
--      p := 10;  -- for 59.5 .. 66.4999 degrees
      a1 := 0.993252463959;
      a2 := 0.0101079774347;
      c1 := 0.99998940805689;
      c2 := 0.00337382242390;
      b := 1.68099737740735e-008;
      t := 63.;
   else
--      p := 11;  -- 66.5 .. 90 degrees
      a1 := 0.9932398462514;
      a2 := 0.0101230460033;
      c1 := 0.99998688314480;
      c2 :=0.00337683963728;
      b := 1.05932343670463e-008;
      t := 70.;
   end if;

   cosy := cosyy* (c1+ siny*siny*c2);
   tt := (y-t);
--   dy := dy*(a1+ a2*siny2*siny2 + tt*tt*b);  -- see comment below
--------------------------------------------------------------------------------
-- NEW The 4 a coefficents replace all of the code above for a1,a2
   sinysq := siny2*siny2;
   cos3y := cosyy*(1.-4.*sinysq);
   cos6y := 2.*cos3y*cos3y -1.;
   dy := dy * (a(1)+ a(2)*sinysq + a(3)*sinysq*sinysq + a(4)*cos6y);
-- The variation of dy with distance when dy >= 0.1 is not known yet
--   if dy >= .099 then
--      dy := dy * (1.- (35-y)*0.00000002);
--   end if;
--------------------------------------------------------------------------------
--    dbms_output.put_line('dx  ' || dx || ' dy ' || round(dy,10) || ' cosy ' || ROUND(cosy,10) || ' cosy2 ' || round(cosy2,10));
--    dbms_output.put_line('siny2 ' || siny2 || 'a2sysy ' || (a(1)+a(2)*sinysq));
   if abs(dy) >=  0.0001 then
      cosy2 := cosy - siny * dy*deg2rad; -- small angle approximation for cos(y2)
      gcd := sqrt(dy*dy + dx*dx*cosy*cosy2) * dist_factor;
   else
      gcd := sqrt(dy*dy + dx*dx*cosy*cosy) * dist_factor;
   end if;

   RETURN gcd;


END FAST_DISTANCE;
--
FUNCTION FASTER_ATAN2(YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER AS

   pi          NUMBER             := 3.1415926535897932384626433832795028842;
   piByTwo     NUMBER             := 1.5707963267948966192313216916397514421;
   piBy6       NUMBER             := 0.5235987755982988730771072305465838140;
   piBy12    CONSTANT NUMBER    := 0.2617993877991494365385536152732919070167;
   tanpiby6  CONSTANT NUMBER    := 0.5773502691896257645091487805019574556545;
   tanpiby12   NUMBER             := 0.2679491924311227064725536584941276331;
--   tanpiby24 CONSTANT NUMBER    := 0.1316524975873958534715264574097171035924;
--   tanpiby10  NUMBER            := 0.3249196962329063261558714122151344649574;
--   piby5      NUMBER :=            0.62831853071795864769252867665590057684;
--   tanpiby5   NUMBER :=           0.7265425280053608858954667574806187496074;
   tanpiby32 CONSTANT NUMBER    := 0.3249196962329063261558714122151344649574;
   rad2deg     NUMBER :=           57.29577951308232087679815481410517033235;
--   piby10      NUMBER :=           0.31415926535897932384626433832795028842;
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
  ELSIF ABS(XIN) >= ABS(YIN) THEN
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
  ELSIF ABS(YIN) > ABS(XIN) THEN
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
     if x > 1.E15 then
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
FUNCTION NEW_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER AS

/*
**************************************************************************************
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
***************************************************************************************
*/

   pi        NUMBER             := 3.1415926535897932384626433832795028842;
   piByTwo   NUMBER             := 1.5707963267948966192313216916397514421;
--   piby4plus NUMBER             := 0.7853981634974483096156608458198757210573;
   piBy6     NUMBER             := 0.5235987755982988730771072305465838140;
--   piBy12    CONSTANT NUMBER   := 0.2617993877991494365385536152732919070167;
--   piby24    CONSTANT NUMBER  := 0.1308996938995747182692768076366459535083;
   tanpiby6  CONSTANT NUMBER   := 0.5773502691896257645091487805019574556545;
--   tanpiby12 NUMBER             := 0.2679491924311227064725536584941276331;
--   tanpiby24 CONSTANT NUMBER   := 0.1316524975873958534715264574097171035924;
   rad2deg   NUMBER :=           57.29577951308232087679815481410517033235;

--   one_plus    NUMBER := 1.00000000020000000002000000000266666667;

   x           NUMBER;
   xx          NUMBER;
   result     NUMBER;
   complement  BOOLEAN           := FALSE;
   region      NUMBER := 0.;
   region2     NUMBER := 0.;
   region3     NUMBER := 0.;
   region4     NUMBER := 0.0;
   u           NUMBER;



-- Reference:
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
--      dbms_output.put_line('< 0.5 ' || x);
    end if;
   end if;

--   dbms_output.put_line('x now is ' || x);
/*
   if x > 0.85  then
      x :=  (1.0 +x*one_plus)/(x-one_plus);
      region := region+ piby4plus;
--      dbms_output.put_line('now x ' || x);
   elsif (x > 0.719)  THEN
      x :=  (1.0 +0.718*x)/(x-0.718);
      region := region + 0.6227046274143364651920141450962125645545;
--      dbms_output.put_line('nnow x ' || x);
--   elsif (x > 0.419) and x <> tanpiby6 THEN
--      x :=  (1.0 +tanpiby6*x)/(x-tanpiby6);
   elsif (x > 0.619)  THEN
      x :=  (1.0 +0.618*x)/(x-0.618);
      region := region +0.5535497640327316544572642343482671646305;
--      dbms_output.put_line('nnow x ' || x);
   elsif (x > 0.519)  THEN
      x :=  (1.0 +0.518*x)/(x-0.518);
      region := region +0.4779436961450838681775529862951774456587;
--      dbms_output.put_line('nnow x ' || x);
  elsif (x > 0.419)  THEN
      x :=  (1.0 +0.418*x)/(x-0.418);
      region := region +0.3959266763443836112761731320589412635438;
 --     dbms_output.put_line('nnow x ' || x);
--  elsif (x > tanpiby12) THEN --and x <> tanpiby12 THEN
-- reduce arg to under tan(pi/24)
--     x := (1.0 +tanpiby24*x)/(x-tanpiby24);
--     region4 := 1.;
--     dbms_output.put_line('nnnow x ' || x);
   elsif (x > 0.319)  THEN
      x :=  (1.0 +0.318*x)/(x-0.318);
      region := region + 0.3078876691619894502497709376934849823398;
 --     dbms_output.put_line('nnow x ' || x);
   elsif (x > 0.219)  THEN
      x :=  (1.0 +0.218*x)/(x-0.218);
      region := region +0.2146418375042067463803722632762100557514;
--      dbms_output.put_line('nnow x ' || x);
   elsif (x > 0.099) tHEN --tanpiby24) THEN
-- reduce arg to under tan(pi/24)
     x := (1.0 +0.098*x)/(x-0.098);  --(1.0 +tanpiby12*x)/(x-tanpiby12);
     region := region + 0.0976880648650501706150197637332474027458;
--     dbms_output.put_line('nnnnow x ' || x);
   elsif x <> 0.0 then
      x := 1./x;
--      dbms_output.put_line('Nnnnow x ' || x);
   end if;
*/

--  IF Yin <> 0. and ABS(YIn) < ABS(XIn) THEN
--     x   := ABS(XIn)/ABS(YIn);
--  ELSE
--     x   := ABS(YIn/XIN);
    if yin <> 0.0 and ((ABS(YIn) >= ABS(XIn) and u <= 1.) or (ABS(YIn) < ABS(XIn) and u > 1.)) then
     complement := TRUE;
     end if;
--  END IF;

     if x > 1.E15 then
      result := 1./x + region;
    else

--     dbms_output.put_line('x ' || round(x,8) || ' yin ' || yin);
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
FUNCTION GET_PIPES(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., minimum_len NUMBER default 1000.,aspect_ratio NUMBER default 2.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY  AS
-- SQL callable function to get pipes. Pipes are narrow peninsulas or river estuary shapes.
   Pipe_Geom          MDSYS.SDO_GEOMETRY;
   XYOrd              MDSYS.SDO_ORDINATE_ARRAY := Geom.Sdo_ordinates;
   PipesXYOrd         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   PipeXYOrd          MDSYS.SDO_ORDINATE_ARRAY;
   Info               MDSYS.SDO_ELEM_INFO_ARRAY := Geom.sdo_Elem_Info;
   Info_Arr           MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY();
   PSL                MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   PSL_All            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Widths             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Pipe_Pars          MDSYS.SDO_LIST_TYPE;
   from_vertex        PLS_INTEGER;
   to_vertex          PLS_INTEGER;
   n                  PLS_INTEGER;
   out_index          PLS_INTEGER := 0;
   next               PLS_INTEGER :=0;
   which              PLS_INTEGER;
   Big                NUMBER := 1.E10;
   area               NUMBER;
   avg_width          NUMBER;
   pipe_perim         NUMBER;
   pipe_len           NUMBER;
   accum_length       NUMBER;
   avg_width2         NUMBER;
   avg_width3         NUMBER := Big;
   hi_width3          NUMBER;
   narrow_width       NUMBER;
   wide_width         NUMBER;
   B2_4AC             NUMBER;

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
--       dbms_output.put_line('jj ' || kk || '  '||round(widths(kk),3));
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
--      dbms_output.put_line('jj ' || jj || '  '||round(widths(jj),3));
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
Procedure Copy_Xys(XY_Arr MDSYS.SDO_ORDINATE_ARRAY,from_aa PLS_INTEGER, to_bb PLS_INTEGER,Out_Arr IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,out_idx IN OUT PLS_INTEGER)
                   AS
-- Straightforward function to copy part of a single loop's coordinates


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
   Return ;
end;
--==============================================================================
BEGIN
   Pipe_Pars := Find_Get_Pipes(Geom,pipe_type,max_width,minimum_len,aspect_ratio,max_angle_diff);
--   dbms_output.put_line('PPP ' || pipe_pars.count);
   If Pipe_pars is NOT NULL then
      If Pipe_pars.count = 6 then
        pipe_len := Pipe_pars(1);
        from_vertex := Pipe_pars(2);
        to_vertex := Pipe_pars(3);
        which := TRUNC(Pipe_pars(4)*0.1);
 
--        dbms_output.put_line('pp ' || pipe_pars(1) || ',' || pipe_pars(2) ||','||pipe_pars(3));
        Pipe_geom := get_Pipe_Geom(XyOrd,Info,from_vertex,to_vertex,which,max_width,PSL,avg_width,accum_length);

        If pipe_geom is NOT NULL THEN
        
--      Approximate the pipe_length in meters 
        PipeXYord := Pipe_geom.sdo_ordinates;
        pipe_perim := Perimeter(PipeXyOrd);
--        dbms_output.put_line('AW ' || avg_width || ' pipexy ' || pipexyord.count);
        Widths := Get_widths(Pipe_geom,max_width*1.5,2,20,FALSE);
        if Widths is NOT NULL then

        Get_low_high_width;
        
--ignore        pipe_len := pipe_perim*0.5 - avg_width;
--        dbms_output.put_line('AWW ' || avg_width || ' PL ' ||pipe_len);
        area := ABS(polygon_area(PipeXyord));
        
        -- Use quadratic equation from L * W = Area and 2L + 2W = Perimeter
        -- W = (Perim/2 - sqrt((Perim/2)**2 -4*Area))/2
        
        
        avg_width3 := Big;
        B2_4AC := (pipe_perim*0.5)*(pipe_perim*0.5) - 4.*area;
        if b2_4AC >=0.0 then
          avg_width3 :=  (pipe_perim*0.5- sqrt(B2_4AC))*0.5;
 --         dbms_output.put_line('HI Width3 ' || hi_width3 || ' max ' || max_width);
        end if;
        
 --       if avg_width2 > avg_width and avg_width2 > avg_width3 and (1.- avg_width/avg_width2) < (1.-avg_width3/avg_width2) then
 --          avg_width := (avg_width + avg_width2)*0.5;
 --           dbms_output.put_line('SHOSE AWW1 ' || avg_width);
 --       elsif avg_width2 <= avg_width and avg_width > avg_width3 and (1.- avg_width2/avg_width) < (1.-avg_width3/avg_width) then
 --          avg_width := (avg_width + avg_width2)*0.5;
 --           dbms_output.put_line('SHOSE AWW2 ' || avg_width);
 --       else
--         -- Check that we have a rectangular like object
 --           if wide_width < max_width then
 --             avg_width := avg_width3;
 --           else
--               avg_width := Wide;
--            end if;
 --           dbms_output.put_line('SHOSE AWW3 ' || avg_width);
 --       end if;
--ignore        pipe_len := area/avg_width;
--       dbms_output.put_line('pipe len ' || round(pipe_len,3) || ' accum_len ' || round(accum_length,3) || ' perim ' || round(pipe_perim,3) || ' Area ' || round(area,3) || 'Estimate ' || round(avg_width*pipe_len,3));
  
        if pipe_len >= minimum_len and pipe_len/avg_width >= aspect_ratio then
           RETURN pipe_geom;
        else
           pipe_geom := NULL;
        end if;
        end if;
        END IF;
      Else
        Info_arr.extend((pipe_pars.count/10)*3);
        For ii in 1..Pipe_pars.count Loop
          If MOD(ii,10) = 1 then
            pipe_len :=  Pipe_pars(ii);
            from_vertex := Pipe_pars(ii+1);
            to_vertex := Pipe_pars(ii+2);
            which := TRUNC(Pipe_pars(ii+3)*0.1);

-- dbms_output.put_line('PSL for '|| pipe_pars(ii) || ',' || pipe_pars(ii+1) ||','||pipe_pars(ii+2) );
--          for kk in 1..psl_all.count loop
--            dbms_output.put_line('psl ' || psl_all(kk));
--          end loop;
            if NOT Check_PSL_pairs()  then
--            dbms_output.put_line('PPP ' || pipe_pars(ii) || ',' || pipe_pars(ii+1) ||','||pipe_pars(ii+2) || ' which ' || which);
            Pipe_geom := get_Pipe_Geom(XyOrd,Info,from_vertex,to_vertex,which,max_width,PSL,avg_width,accum_length);

           
            if pipe_geom is NOT NULL then
 
            --      Approximate the pipe_length in meters 
        PipeXYord := Pipe_geom.sdo_ordinates;
        pipe_perim := Perimeter(PipeXyOrd);
--        dbms_output.put_line('pipexy ' || pipexyord.count);
--               dbms_output.put_line('AW ' || avg_width || ' PP ' || pipe_perim);
        Widths := Get_widths(Pipe_geom,max_width*1.5,2,20,FALSE);
--             for kk in 1..trunc(pipexyord.count/2) loop
--               dbms_output.put_line('ii ' || kk || ' xy ' ||round(pipexyord(kk*2-1),7) ||','||round(pipexyord(kk*2),7));
--             end loop;
        if Widths is NULL then 
           Widths := MDSYS.SDO_LIST_TYPE(Pipe_pars(ii+4), Pipe_pars(ii+5));
        end if;
--             dbms_output.put_line('WWW ' || widths(1));
        if Widths is NOT NULL then
          
        Get_low_high_width;
--        dbms_output.put_line('AWW ' || avg_width);
--ignore        pipe_len := pipe_perim*0.5 - avg_width;
        area := ABS(polygon_area(PipeXyord));
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
--        dbms_output.put_line('pipe len ' || round(pipe_len,3) || ' pipe_len/avg_width ' || round((pipe_len/avg_width),3) || ' perim ' || round(pipe_perim,3) || ' Area ' || round(area,3) || 'Estimate ' || round(avg_width*pipe_len,3));
 
        if pipe_len >= minimum_len and pipe_len/avg_width >= aspect_ratio then
--dbms_output.put_line('>>> got a pipe');
  -- Only mark those that are successful as done           
             n := PSL_All.count;
            PSL_All.extend(PSL.count);
            for jj in 1..PSL.count loop
              PSL_All(n+jj) := PSL(jj);
            end loop;
            
            
            Info_Arr(next+1) := out_index+1;
            Info_Arr(next+2) := 2;
            Info_Arr(next+3) := 1;
            next := next+3;
            Copy_Xys(Pipe_Geom.sdo_ordinates,1,Pipe_Geom.sdo_ordinates.count,PipesXyOrd,out_index);
--            Copy_Xys(Geom.sdo_ordinates,from_index,to_index,XyOrd,out_index);
         End if;
            End If;
            End If;
          End If;
          End if;
        End Loop;
        Info_Arr.trim(Info_Arr.count-next);
      Pipe_geom :=  MDSYS.SDO_GEOMETRY(2006,Geom.sdo_SRID,NULL,Info_Arr,PipesXYOrd);
      End if;
   End if;
   if Info_Arr.count = 0 then
      RETURN NULL;
   end if;
   RETURN Pipe_Geom;
END GET_PIPES;
--
FUNCTION FIND_PIPES(Geom IN MDSYS.SDO_GEOMETRY,pipe_type VARCHAR2 default 'A',max_width NUMBER default 150., minimum_len NUMBER default 1000.,aspect_ratio NUMBER default 2.,max_angle_diff NUMBER default 35.) RETURN NUMBER  AS

-- SQL callable function to find pipes. Pipes are narrow peninsulas or river estuary shapes.
   Pipe_Pars          MDSYS.SDO_LIST_TYPE;
   Dist    NUMBER :=0.0;   -- Length of the pipe
   Max_Dist NUMBER :=0.0;
   Width   NUMBER := 0.0;
   pos     PLS_INTEGER;
   ID      VARCHAR2(1);
BEGIN
   Pipe_pars := Find_Get_Pipes(Geom,pipe_type,max_width,minimum_len,aspect_ratio);
   If Pipe_pars is NOT NULL then
      For ii in 1..TRUNC(Pipe_pars.count/10) loop
      pos := (ii-1)*10;
      Width := Pipe_pars(pos+5);
      Dist := Pipe_Pars(pos+1);
      if Dist > Max_Dist then
         Max_Dist := Dist;
      end if;
      End Loop;
   End if;

   RETURN max_Dist;
END FIND_PIPES;
--
FUNCTION FIND_GET_PIPES(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150.,  minimum_len NUMBER default 1000.,aspect_ratio NUMBER default 2.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_LIST_TYPE AS

-- SQL callable function to find pipes. A pipe is defined as a narrow part on
-- a polygon where either the angle is <  angle check or the distance to the next
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
  Pipe_Pars         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Xys               MDSYS.SDO_ORDINATE_ARRAY;
 
  Info_array        MDSYS.SDO_ELEM_INFO_ARRAY;
  Codes             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Matches           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Subject_Segs      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Widths_p          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Widths_q          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Lengths           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  
  MBRs              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Xps               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Yps               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Xqs               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Yqs               MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Accum_lengths     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Order_array       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
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

  peninsula_dist              NUMBER :=0.0;
  dist2             NUMBER :=0.0;
  SRID              NUMBER := Geom.SDO_SRID;
  checkit           NUMBER;
  width             NUMBER := 1.;
  found_width       NUMBER;
  cutoff_width      NUMBER := 2.* max_width;
  length_match      NUMBER;
  length_ss         NUMBER;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;
  x3                NUMBER;
  y3                NUMBER;
  x4                NUMBER;
  y4                NUMBER;
  projector         NUMBER;
  last_ss           PLS_INTEGER;
  pos               PLS_INTEGER;
  n                 PLS_INTEGER;
  next              PLS_INTEGER := 0;

  pipe_types        VARCHAR2(2) := 'A'||Upper(pipe_type);
  sql_stmt          VARCHAR2(4000);

  
--==============================================================================
BEGIN

    Xys := geom.sdo_ordinates;
    Info_Array := geom.sdo_elem_info;
    GZ_SUPER.Set_Many_Mbr(XYs,Info_Array,MBRs,XLL,YLL,XUR,YUR);
    
--    for ii in 1..50 loop
--       dbms_output.put_line('ii ' || ii || ' mbr ' || mbrs(ii));
--    end loop;


--  Check segments that are near to each other to see if they are semiparallel.
-- Generate segment numbers (Subject_Segs and Matches) that are always odd
--    XY(1),Xy(2)              XY(3),Xy(4)                   XY(5),Xy(6)
--    segment: 1               3                               5 etc
--    +-------------------------+-------------------------------+

 checkit := CHECK_NEARBY(XYs,Info_Array,MBRs,XYs,Info_Array,MBRs,Codes,Subject_Segs,Matches,Widths_p,Widths_q,Lengths,
                         Xps,Yps,Xqs,Yqs,SRID,max_angle_diff,max_width,2,pipe_types,TRUE);
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

    -- Sort the arrays so the distances can be accumulated.
    
    n := Codes.count;
    if n = 0 then 
       RETURN NULL;
    end if;
  
    Order_array.extend(n);
    Accum_Lengths.extend(n);
    for ii in 1..n loop
      Subject_Segs(ii) := 1+TRUNC(Subject_Segs(ii)/2);
      Matches(ii) := 1 + TRUNC(Matches(ii)/2);
      Accum_lengths(ii) := 0.0;
      Order_array(ii) := ii;
    end loop;

   
    Shellsort4(Subject_segs,Matches,Widths_p,Order_array,1,n);
    

    Sort_also(Lengths,Order_array);
    Sort_also(Widths_q,Order_array);
    Sort_also(Codes,Order_array);
    Sort_also(Xps,Order_array);
    Sort_also(Yps,Order_array);
    Sort_also(Xqs,Order_array);
    Sort_also(Yqs,Order_array);

-- Accumulate the distances to get a pipe length
    pos := 1;
    last_ss := Subject_segs(1);
   
    for ii in 2..n loop 
  
-- If this piece belongs to the current pipe part, then include it
        if (Subject_segs(ii) -last_ss <= 1 or 
           (Subject_segs(ii) -last_ss <= 2 and Matches(ii-1) -Matches(ii) =1))  then
            last_ss := Subject_segs(ii);
            if Lengths(ii) <> Lengths(ii-1) then
            Accum_Lengths(pos) := Accum_Lengths(pos) + Lengths(ii);
            end if;
--dbms_output.put_line('adding ' ||ii || ' to ' || pos);
        else
--        dbms_output.put_line('at ' ||ii || ' ' || ' SS ' || subject_segs(ii) || ' ' ||(Subject_segs(ii) -last_ss) || ' and ' || (Matches(ii-1) -Matches(ii)));
           pos := ii;
           last_ss := Subject_segs(ii);
           Accum_Lengths(ii) :=  Lengths(ii);
        end if; 
     end loop;
     if Accum_lengths(1) = 0.0 then
        Accum_Lengths(1) :=  Lengths(1);
     end if;

  for ii in 1..n loop
--     dbms_output.put_line('SSS ' || subject_segs(ii) || ' match ' || matches(ii) || ' W ' || round(Widths_p(ii),2) || ' W ' || round(Widths_q(ii),2) || ' Code ' || codes(ii) || ' AL ' || ROUND(Accum_lengths(ii),3) || ' LL ' || round(Lengths(ii),1));
--   dbms_output.put_line('SS ' || (subject_segs(ii)) || ' match ' || (matches(ii)) || ' D ' || round(distances(ii),8));

-- Sidey Friday 25 new code next line and the length lines below
     found_width := Widths_p(ii);
     if Widths_q(ii) > found_width then
       found_width := Widths_q(ii);
     end if;
--     if Widths_q(ii) > 1.5 * max_width or Widths_p(ii) > 1.5*max_width then
--        found_width := max_width + 1.;
--     end if;
     projector := (Codes(ii) - TRUNC(Codes(ii)))*100.;
--     dbms_output.put_line('SSS ' ||round(found_width,3) || ',' || max_width || ' PJ ' || projector);
     if found_width <= max_width and (projector = 12 or projector = 43 or projector = 42 or projector = 13) 
        then --accum_lengths(ii) > 0.0 then
--dbms_output.put_line('OK');
      Pipe_pars.extend(10);
      if Accum_lengths(ii) <> 0.0 then
      Pipe_pars(next+1) := Accum_Lengths(ii); 
      else
        Pipe_pars(next+1) := Lengths(ii); 
      end if;
      seg :=  Subject_Segs(ii);
      Pipe_pars(next+2) := seg;  -- start vertex
      mseg := Matches(ii);
      Pipe_pars(next+3) := mseg;       -- matching segment

      -- save the projecting vertex 1-> 2 or 2->4
      Pipe_pars(next+4) := projector;
      /*
      x1 := Xys(seg*2-1);
      y1 := Xys(seg*2);
      if projector < 3 then  -- unfortunately find_nearby reverses the convention
       x2 := Xps(ii);     -- of 1 and 2 refer to Subject segs and 3 and 4 refer to matches
       y2 := Yps(ii);
      else
       x1 := Xps(ii);
       y1 := Yps(ii);
       x2 := Xys(seg*2+1);
       y2 := Xys(seg*2+2);
      end if;
      
      
      length_ss := Distance_fcn(x1,y1,x2,y2,SRID);
      Pipe_pars(next+5) := length_ss;
      x3 := Xys(mseg*2-1);
      y3 := Xys(mseg*2);
      if projector > 2 then   -- ditto as above
       x4 := Xps(ii);
       y4 := Yps(ii);
      else
       x4 := Xys(mseg*2+1);
       y4 := Xys(mseg*2+2);
      end if;
      length_match := Distance_fcn(x3,y3,x4,y4,SRID);
      */
--      dbms_output.put_line('stored projector ' || projector || 'ss ' || seg || ' m ' || mseg  || ' w ' || round(widths_p(ii),3) || ' w ' || round(widths_q(ii),3)); -- ||' x1 ' || round(x1,7) || 'y1 ' || round(y1,7) );
--     dbms_output.put_line(' x2 ' || round(x2,7) || 'y2 ' || round(y2,7)  );
      Pipe_pars(next+5) := Widths_p(ii);
      Pipe_pars(next+6) := Widths_q(ii); --length_match;
      Pipe_pars(next+7) := Xps(ii);
      Pipe_pars(next+8) := Yps(ii); 
      Pipe_pars(next+9) := Xqs(ii);
      Pipe_pars(next+10) := Yqs(ii); 
      next := next + 10;
     end if;
  end loop;
  if next > 0 then
     RETURN Pipe_pars;
  else
     RETURN NULL;
  end if;

END FIND_GET_PIPES;
--
FUNCTION OLDFIND_GET_PIPES(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150.,  minimum_len NUMBER default 1000.,aspect_ratio NUMBER default 2.,max_angle_diff NUMBER default 25.) RETURN MDSYS.SDO_LIST_TYPE AS

-- SQL callable function to find pipes. A pipe is defined as a narrow part on
-- a polygon where either the angle is <  angle check or the distance to the next
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
  Pipe_Pars         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Xys               MDSYS.SDO_ORDINATE_ARRAY;
  partxyord         MDSYS.SDO_ORDINATE_ARRAY;
  Info_array        MDSYS.SDO_ELEM_INFO_ARRAY;
  Codes             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Matches           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Subject_Segs      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Distances         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  MBRs              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Xnears            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Ynears            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Xalts             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Yalts             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  XLL               NUMBER;
  YLL               NUMBER;
  XUR               NUMBER;
  YUR               NUMBER;
  checkit           PLS_INTEGER;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;
  x3                NUMBER;
  y3                NUMBER;
  x4                NUMBER;
  y4                NUMBER;
  angle_check       NUMBER;
  straight_angle_check NUMBER;

  offset            PLS_INTEGER;
  offset2           PLS_INTEGER;
  no_to_do          PLS_INTEGER;
  j                 PLS_INTEGER;
  k                 PLS_INTEGER;
  kk                PLS_INTEGER;
  jksave            PLS_INTEGER;
  angel             NUMBER;
  peninsula_dist    NUMBER := 0.0;
  try_distance      NUMBER := 0.0;
  d2                NUMBER;
  dist              NUMBER :=0.0;
  dist2             NUMBER :=0.0;
  SRID              NUMBER := Geom.SDO_SRID;
  xend              NUMBER;
  yend              NUMBER;
  to_find           NUMBER;
  last_match        NUMBER := 0;
  width             NUMBER := 1.;
  x22               NUMBER;
  y22               NUMBER;
  xi                NUMBER;
  yi                NUMBER;
 
  projector         NUMBER;
  next              PLS_INTEGER := 0;
  from_a            PLS_INTEGER;
  to_b              PLS_INTEGER;
  found_count       PLS_INTEGER;
  angle_fail        PLS_INTEGER;
  n                 PLS_INTEGER;
  m                 PLS_INTEGER := 20;
  pipe_types        VARCHAR2(2) := 'A'||Upper(pipe_type);
  sql_stmt          VARCHAR2(4000);
--==============================================================================
Function Search_array(find_it NUMBER, Arr MDSYS.SDO_LIST_TYPE) Return PLS_INTEGER AS
-- Straightforward function to confirm a value exists in an Array
Begin
   for i in 1..Arr.count Loop
      if Arr(i) = find_it then
        Return 1;
      end if;
   end loop;
   Return 0;
end;
--==============================================================================
BEGIN

-- First check Angles
  Angles := Get_Angles(Geom,1000.);
--  dbms_output.put_line('ANGLE ' || angles(1));
--    dbms_output.put_line('new program');

  If angles is not NULL then
      Xys := geom.sdo_ordinates;
      Info_Array := geom.sdo_elem_info;
      n := TRUNC(XYs.count/2);
      if m > n-1 then
        m := n-1;
      end if;
    if Angles(1) < angle_check then

      offset := Angles(2);
      x2 := Xys(offset*2-1);
      y2 := Xys(offset*2);
      x4 := x2;
      y4 := y2;
     for ii in 1..m loop
        x1 := x2;
        y1 := y2;
        offset2 := offset+ii;
        if offset2 >= n then
           offset2 := Mod(offset2,n)+1;
        end if;
        if offset2 >= TRUNC(xys.count/2) or offset2 < 1 then
       dbms_output.put_line('offset2 ' || offset2 || ' n ' || n || ' ii ' || ii || ' offset ' || offset);
       end if;
        x2 := Xys(offset2*2-1);
        y2 := Xys(offset2*2);
        if Angles(offset2+2) > straight_angle_check then
        dist := dist + distance_fcn(x1,y1,x2,y2,SRID);
--          dbms_output.put_line('dist ' || round(dist,3));
        if dist > minimum_len AND dist2 > minimum_len then
--          RETURN offset; ---abc
            NULL;
        end if;
        offset2 := offset-ii;
        if offset2 < 1 then
           offset2 := n+offset2 -1;
        end if;
        else ---
           angle_fail := angle_fail +1;
--          dbms_output.put_line('fail ' || round(angles(offset2+2),3));
          exit;
        end if;
        x3 := x4;
        y3 := y4;
        x4 := Xys(offset2*2-1);
        y4 := Xys(offset2*2);
        if Angles(offset2+2) > straight_angle_check then
        dist2 := dist2 + distance_fcn(x3,y3,x4,y4,SRID);
--        dbms_output.put_line('dist2 ' || round(dist2,3));
        if dist2 > minimum_len and dist > minimum_len then
--          RETURN offset;
          NULL;
        end if;
        else
           angle_fail := angle_fail +1;
--           dbms_output.put_line('Fail ' || round(angles(offset2+2),3));
           exit;
        end if;
     end loop;
    end if;


    GZ_SUPER.Set_Many_Mbr(XYs,Info_Array,MBRs,XLL,YLL,XUR,YUR);
    

--dbms_output.put_line('calling check_nearby ' || XYS.count );

--  Check segments that are near to each other to see if they are semiparallel.
-- Generate segment numbers (Subject_Segs and Matches) that are always odd
--    XY(1),Xy(2)              XY(3),Xy(4)                   XY(5),Xy(6)
--    segment: 1               3                               5 etc
--    +-------------------------+-------------------------------+

-- checkit := CHECK_NEARBY(XYs,Info_Array,MBRs,XYs,Info_Array,MBRs,Codes,Subject_Segs,Matches,Distances,
--                         Xnears,Ynears,Xalts,Yalts,SRID,max_angle_diff,max_width,2,pipe_types,TRUE);
--dbms_output.put_line('>>>>>>>>>>>>>>>BACK FROM CHECK NEARBYcheckit ' || checkit);

-- Find peninsula Length
-- 4 cases       1)                          2)
--              --   shorter           ---------------- longer      Match
--          ------------                       --                Subject_seg
--               3)                          4)
--               ------  ahead                ----  behind          Match
--          -------                             -------           Subject_seg


  for ii in 1..subject_segs.count loop
--   dbms_output.put_line('SS ' || (1+ TRUNC(subject_segs(ii)/2)) || ' match ' || (1+TRUNC(matches(ii)/2)) || ' D ' || round(distances(ii),8));
 --  dbms_output.put_line('SS ' || (subject_segs(ii)) || ' match ' || (matches(ii)) || ' D ' || round(distances(ii),8));
    j := Subject_segs(ii); -- begin at the start of the segment
    k := Matches(ii) ;  --    go to the start of match
    if j > last_match then
      last_match := Matches(ii);
    -- We have found the parallel segments which may have intervening
    -- connecting segments (2nd picture).
    -- find shorter segment and then set the peninsula distance as either
    -- 1) the length of the shorter edge or 
    -- 2) the distance from the start of the shorter segment to the furthest point
    --    before arriving at the start of the longer segment
    --
    -- Picture                    +---<----+ (x1,y1) (start of short one)
    --                                     |
    --                       <    Length  >|
    --                       +--------->---------+     (longer one)
    --
    -- Picture 2 (uses .. to mean
    --           more segments)
    --                                +---<----+ (x1,y1) (start of short one)
    --                ..                       |
    --         +<               Length        >|
    --                 ..    +--------->---------+     (longer one)
    x1 := Xys(j);
    y1 := Xys(j+1);
    x22 := Xys(j+2);
    y22 := Xys(j+3);
    jksave := j+3;
    xend := x22;
    yend := y22;
    projector := (Codes(ii) - TRUNC(Codes(ii)))*10.;
-- try and project vertex 1 onto the matching segment
    dist := GZ_SUPER.Perpendicular(x1,y1,Xys(k),XYs(k+1),Xys(k+2),Xys(k+3),xi,yi,FALSE, TRUE);
    if xi is NULL then
      dist := GZ_SUPER.Perpendicular(Xys(k+2),Xys(k+3),x1,y1,x22,y22,xi,yi,FALSE, TRUE);
    end if;
    peninsula_dist := Distance_fcn(x1,y1,x22,y22,SRID);

    try_distance := Distance_fcn(Xys(k),Xys(k+1),Xys(k+2),Xys(k+3),SRID);
    if try_distance < peninsula_dist then
      peninsula_dist := try_distance;
      x1 := Xys(k);
      y1 := Xys(k+1);
      x22 := Xys(k+2);
      y22 := Xys(k+3);
      jksave := k+3;
      xend := x22;
      yend := y22;
     
    end if;
--    dbms_output.put_line('first PD ' || round(peninsula_dist,4)); 
--            dbms_output.put_line('X1 ' || round(x1,7) ||','||round(y1,7));
--           dbms_output.put_line('Xe ' || round(xend,7) ||','||round(yend,7));
    j := TRUNC((j+1)/2);
    k := TRUNC((k+1)/2);
    if k >= n then
      k := 1;
    end if;
    no_to_do := k-j;
    if k < j then
      no_to_do := k+n-j;
    end if;
    found_count := 0;
--  Find largest
    for jj in 1..no_to_do loop
      kk := j + jj;
      if kk = n then
        NULL;
      else
        kk := MOD(kk,n)*2-1;

        x2 := Xys(kk);
        y2 := Xys(kk+1);
        
        found_count := found_count + Search_array(kk,Matches);
--        if found_count <> 0 then
--        dbms_output.put_line('found ' || found_count || ' kk ' || kk);
--        end if;
        found_count := found_count + Search_array(kk,Subject_segs);
        try_distance := Distance_fcn(x1,y1,x2,y2,SRID);
--        dbms_output.put_line('X2 ' || round(x2,7) ||','||round(y2,7)|| ' D ' || round(try_distance,3));
--        if found_count <> 0 then
--        dbms_output.put_line('Found ' || found_count || ' kk ' || kk);
--        end if;
        angel := angle(x22,y22,x1,y1,x2,y2,TRUE);
        if try_distance > peninsula_dist and angel < 30. then
           peninsula_dist := try_distance;
           xend := x2;
           yend := y2;
           jksave := kk+1;
--                   dbms_output.put_line('x1 ' || round(x1,7) ||','||round(y1,7));
--           dbms_output.put_line('Xe ' || round(xend,7) ||','||round(yend,7));
        end if;
      end if;
    end loop;

   
   dbms_output.put_line('PD ' || round(peninsula_dist,4) );

    width := Distances(ii);
    if peninsula_dist > minimum_len and width > 0. and peninsula_dist/width >= aspect_ratio then -- and found_count > TRUNC((no_to_do+1)/2) then
--    dbms_output.put_line('xend ' || round(xend,10) || ' yend '|| round(yend,10));


      from_a := j*2-1;
      to_b := last_match+3;
--dbms_output.put_line('from a ' || from_a || ' to_b '||to_b || ' jksave ' || jksave || ' PD ' || round(peninsula_dist,4)|| ' width ' || round(width,3));
      Pipe_pars.extend(6);
      Pipe_pars(next+1) := peninsula_dist;
      if jksave = to_b   then
        Pipe_pars(next+2) := to_b-3;
        Pipe_pars(next+3) := from_a+3;
      else
        Pipe_pars(next+2) := from_a;
        Pipe_pars(next+3) := to_b;
--        dbms_output.put_line('here!');
      end if;
      -- save the projecting vertex 1-> 2 or 2->4
      Pipe_pars(next+4) := projector;
      Pipe_pars(next+5) := xi;
      Pipe_pars(next+6) := yi;
      next := next + 6;
    end if;
--    last_match := k;
    end if;
  end loop;
  if next > 0 then
     RETURN Pipe_pars;
  else
     RETURN NULL;
  end if;


    RETURN NULL;

  else
    RETURN NULL;
  end if;


END OLDFIND_GET_PIPES;
--
FUNCTION ANGLE(x0 NUMBER,y0 NUMBER,x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,degrees Boolean default FALSE) RETURN NUMBER DETERMINISTIC IS

/*
**************************************************************************************
--Program Name: Angle
--Author: Sidey Timmins
--Creation Date: 08/01/2009
--Usage:
  -- Call this function from inside another PL/SQL program.  There are 6 parameters:
  --
  --   REQUIRED Parameters:
  --           (x0,y0): coordinates in degrees of a vertex
  --           (x1,y1): coordinates in degrees of center vertex
  --           (x2,y2): coordinates in degrees of 3rd vertex
--
--                    (x1,y1) +---------------+ (x0,y0)
--                            |   included angle
--                            |
--                    (x2,y2) +
--
--Purpose:  Measure angles on the earth's surface between nearby points.
--          Returned result is in radians!!
-- Method:  Computes bearings and then calculates difference.
-- Accuracy: typically less than ?? degrees.
--Dependencies: geo_bearing
***************************************************************************************
*/
   twopi     CONSTANT NUMBER := 6.2831853071795864769252867665590057684;
   pi        CONSTANT NUMBER := 3.1415926535897932384626433832795028842;
   rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;

   bearing1              NUMBER;
   bearing2              NUMBER;
   included_angle        NUMBER;
   oldb                  NUMBER;
BEGIN

        bearing2 := geo_bearing(x1,y1,x2,y2,oldb);

        if bearing2 < 0. then
          bearing2 := twopi + bearing2;
        end if;

-- Work out bearings all going towards x2,y2
        bearing1 := geo_bearing(x0,y0,x1,y1,oldb);
--        dbms_output.put_line('Bb1 ' || round(bearing1,3));
        if bearing1 < 0. then
          bearing1 := twopi + bearing1;
        end if;
-- Now adding 180 here reverses bearing1 to its real sense (from point 1 to point 0)
        included_angle := abs(mod(bearing1+pi,twopi) - bearing2);
-- dbms_output.put_line('B1 ' || round(bearing1,3) || ' B2 ' || round(bearing2,3) || ' ia ' ||round(included_angle,3));
-- dbms_output.put_line('x0 ' || round(x0,6) || ' y0 ' || round(y0,6) );
-- dbms_output.put_line('x1 ' || round(x1,6) || ' y1 ' || round(y1,6) );
-- dbms_output.put_line('x2 ' || round(x2,6) || ' y2 ' || round(y2,6) );
        if included_angle > pi then
          included_angle := twopi - included_angle;
        end if;

        if degrees then
           included_angle := included_angle*rad2deg;
        end if;
        
        RETURN included_angle;

END ANGLE;
--
FUNCTION GEO_BEARING(px0 NUMBER,py0 NUMBER,px1 NUMBER,py1 NUMBER,old_bearing IN OUT NOCOPY NUMBER)
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
-- Method:  Computes x and y and then calculates bearing. Always computes
--          going North or East and then reverses direction if necessary.
--
-- Accuracy: Typically less than 1.e-12 degrees compared to the azimuth formula.
-- This is the initial bearing (sometimes referred to as forward azimuth) which
--if followed in a straight line along a great-circle arc will take you from the
--start point to the end point:
-- ¿ = atan2( sin(¿long).cos(lat2),cos(lat1).sin(lat2) ¿ sin(lat1).cos(lat2).cos(¿long) )

--Reference: http://www.movable-type.co.uk/scripts/latlong.html
--   Personal communication from Peter H. Dana
--   http://www.colorado.edu/geography/gcraft/notes/mapproj/mapproj_f.html
--
--Dependencies:
--   GEN_UTIL_ZONE.atan2,GEN_UTIL_ZONE.sincos
***************************************************************************************
*/
   twopi     CONSTANT NUMBER := 6.2831853071795864769252867665590057684;
   pi        CONSTANT NUMBER := 3.1415926535897932384626433832795028842;
   piby2     CONSTANT NUMBER := 1.5707963267948966192313216916397514421;
   deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.


--   a2          NUMBER   := 40680631590769.;               -- ellipsoid radius ^2
--   b2          NUMBER   := 40408299983328.76931725432420763255772;
   e2          NUMBER := 0.006694380022900787625359114703055206936; --1. - b2/a2;
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
   flip                  PLS_INTEGER := 0;

BEGIN

-- To get consistent bearings independent of direction,always choose the smallest y
     if ABS(y1) < ABS(y0) then
        flip := 1;
        x0 := x1;
        y0 := y1;
        x1 := px0;
        y1 := py0;
     end if;

-- Here we setup the change in x and y from the most recent vertex
     dx := x1 - x0;
     dy := y1 - y0;

     if dx = 0. and dy = 0. then
        RETURN NULL;
     end if;

    -- Convert to radians and then do a local secant projection
--        dbms_output.put_line('x0 ' || round(x0,7) || ' y0 ' || round(y0,7));
--         dbms_output.put_line('x1 ' || round(x1,7) || ' y1 ' || round(y1,7));
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

        siny0 := GZ_UTIL_ZONE.sincos(y0,cosy0);
        siny1 := GZ_UTIL_ZONE.sincos(y1,cosy1);
        sinx1_0 := GZ_UTIL_ZONE.sincos(x1_0,cosx1_0);

        dx := cosy1 * sinx1_0;
        dy := (cosy0 * siny1 - siny0 * cosy1 * cosx1_0);

-- Maintain the way the line was going, from zero to 1 to 2;
       if flip = 1 then
         dy := -dy;
         dx := -dx;
       end if;

-- Cunningham's formula (page 120 from G. Bomford)

       bearing := faster_atan2(dy,dx);
       if sinx1_0 = 0.0 or cosy1 = 0.0  then
          bearing := faster_atan2(dy,dx);
--          dbms_output.put_line('= zero  ' || bearing);
       else
--         tany0 := tan(y0);
--         lambda12 := (1.- e2) * tan(y1)/tany0 + e2 * (cosy0/cosy1);
--         cotphi := (lambda12  - cosx1_0) * siny0/sinx1_0;
         tanpsi2 := (1.- e2) * tan(y1) + e2 * (siny0/cosy1);
         cotphi := (cosy0*tanpsi2  - siny0*cosx1_0)/sinx1_0;
         if cotphi <> 0.0 then
           bearingc := piby2 -faster_atan2(1./cotphi,1.);
           if dy < 0. then
             bearingc := bearingc -pi;
           end if;
           if abs(bearingc - bearing) > 0.2 then
              dbms_output.put_line('bc ' || round(bearingc,6) || ' b1 ' || round(bearing,6));
           end if;
           if bearing < 0. then
             bearing := twopi + bearing;
           end if;
           old_bearing := bearing;
           bearing := bearingc;
         else
-- Atan2 is very slow (remember 38 digits of precision) so use
-- a very accurate approximation and convert to degrees.
           bearing := faster_atan2(dy,dx);
         end if;
       end if;

      if bearing < 0. then
          bearing := twopi + bearing;
      end if;

      RETURN Bearing;

END GEO_BEARING;
--
FUNCTION Get_Small_Angle(Geom IN MDSYS.SDO_GEOMETRY, angle NUMBER default 5.) RETURN NUMBER  AS

  Angles            MDSYS.SDO_LIST_TYPE;

BEGIN

  Angles := Get_Angles(Geom,angle);

  If angles is not NULL then
    RETURN Angles(1);
  else
    RETURN NULL;
  end if;

END Get_Small_Angle;
--
FUNCTION Get_Small_Angle_Geom(Geom IN MDSYS.SDO_GEOMETRY,angle NUMBER default 5.) RETURN MDSYS.SDO_GEOMETRY  AS

  Angles            MDSYS.SDO_LIST_TYPE;
  VGeom             MDSYS.SDO_GEOMETRY;
  XYs               MDSYS.SDO_ORDINATE_ARRAY := Geom.SDO_ORDINATES;
  VXYs              MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  Info              MDSYS.SDO_ELEM_INFO_ARRAY := GEOM.SDO_ELEM_INFO;
  Vinfo             Mdsys.sdo_Elem_Info_Array:= Mdsys.sdo_Elem_Info_Array();

  back              PLS_INTEGER;
  ii                PLS_INTEGER;

  current_info      PLS_INTEGER :=0;
  next              PLS_INTEGER :=0;
  seg               PLS_INTEGER;
  GTYPE             NUMBER := 2002.;
  
BEGIN 

  Angles := Get_Angles(Geom,angle);

  If angles is not NULL then
  
    For i in 2..TRUNC(Angles.count/2)  loop  -- 1 and 2 are the smallest and its position
    
      ii := Angles(i*2)*2+1;         -- index is second
      
 
      for current_info in 1..TRUNC(Info.count/3) Loop
       
        if current_info+1 > TRUNC(Info.count/3) then
          back :=  Xys.count-3;
        else
          back := Info(current_info*3+1)-4;
        end if;
        if ii = Info(current_info*3-2) then
            exit;
        elsif ii < back then
          back := ii-2;
          exit;
        end if;
      end loop;
 
        Vxys.extend(6);
        next := next +2;
        
--dbms_output.put_line('back ' || back || ' ii ' || ii);
        Vxys(next-1) := Xys(back);
        Vxys(next) :=   Xys(back+1);
        for jj in 1..4 loop
          next := next +1;
          Vxys(next) := Xys(ii+jj-1);
        end loop;
  
    End Loop;
    
    If Vxys.count = 6 then
    VGeom := MDSYS.SDO_GEOMETRY(GTYPE,Geom.SDO_SRID,NULL,Mdsys.sdo_elem_info_array(1,2,1),
                                 VXys);
    Else
      Gtype := 2006.;
      Vinfo.extend(TRUNC(next/2));
      
      for i in 1..TRUNC(next/6) loop
         Vinfo(i*3-2) := (i-1)*6+1;
         Vinfo(i*3-1) := 2;
         Vinfo(i*3) := 1;
      end loop;
      VGeom := MDSYS.SDO_GEOMETRY(GTYPE,Geom.SDO_SRID,NULL,Vinfo,VXys);
    End if;
    RETURN VGeom;
  else
    RETURN NULL;
  end if;

END Get_Small_Angle_Geom;
--
FUNCTION GET_PIPE_Segs(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., minimum_len NUMBER default 1000.,aspect_ratio NUMBER default 2.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY  AS
-- SQL callable function to get pipes. Pipes are narrow peninsulas or river estuary shapes.

  VGeom             MDSYS.SDO_GEOMETRY;
  XYs               MDSYS.SDO_ORDINATE_ARRAY := Geom.SDO_ORDINATES;
  VXYs              MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  Info              MDSYS.SDO_ELEM_INFO_ARRAY := GEOM.SDO_ELEM_INFO;
  Vinfo             Mdsys.sdo_Elem_Info_Array:= Mdsys.sdo_Elem_Info_Array();
  Pipe_pars         Mdsys.sdo_list_type;

  ii                PLS_INTEGER;
  next              PLS_INTEGER :=0;
  seg               PLS_INTEGER;
  mseg              PLS_INTEGER;
  Big               NUMBER := 1.E10;
  GTYPE             NUMBER := 2002.;
  projector         NUMBER;
  pipe_len          NUMBER;
 Begin


 Pipe_Pars := Find_Get_Pipes(Geom,pipe_type,max_width,minimum_len,aspect_ratio,max_angle_diff);
--   dbms_output.put_line('PPP ' || pipe_pars.count);
   If Pipe_pars is NOT NULL then

  
   For i in 1..TRUNC(Pipe_pars.count/10) Loop
       ii := (i-1)*10;

       if Pipe_pars(ii+5) < max_width and Pipe_pars(ii+6) < max_width then
        pipe_len := Pipe_pars(ii+1);
        if pipe_len = 0.0 and pipe_len < minimum_len then
           NULL;
        else
 
        projector := Pipe_pars(ii+4);    ---TRUNC(Pipe_pars(ii+4)))*100.;
        seg := Pipe_pars(ii+2);
        mseg := Pipe_pars(ii+3);
        Vxys.extend(8);
        next := next +4;
        if projector = 12 then
          Vxys(next-3) := Xys(seg*2-1);
          Vxys(next-2) := Xys(seg*2);     
          Vxys(next-1) := Xys(seg*2+1);
          Vxys(next) :=   Xys(seg*2+2);
          next := next + 4;
          Vxys(next-3) := Pipe_pars(ii+7);
          Vxys(next-2) := Pipe_pars(ii+8);
          Vxys(next-1) := Pipe_pars(ii+9);
          Vxys(next) := Pipe_pars(ii+10);
        elsif projector = 13 then
          Vxys(next-3) := Xys(seg*2-1);
          Vxys(next-2) := Xys(seg*2);
          Vxys(next-1) := Pipe_pars(ii+9);
          Vxys(next) := Pipe_pars(ii+10);
          next := next + 4;
          Vxys(next-3) := Xys(mseg*2-1);
          Vxys(next-2) := Xys(mseg*2);
          Vxys(next-1) := Pipe_pars(ii+7);
          Vxys(next) :=   Pipe_pars(ii+8);
        end if;
-- Do matching (parallel) segment        
     
        if projector = 43 then
          Vxys(next-3) := Pipe_pars(ii+7);
          Vxys(next-2) := Pipe_pars(ii+8);
          Vxys(next-1) := Pipe_pars(ii+9);
          Vxys(next) := Pipe_pars(ii+10);
          next := next + 4;
          Vxys(next-3) := Xys(mseg*2-1);
          Vxys(next-2) := Xys(mseg*2);      
          Vxys(next-1) := Xys(mseg*2+1);
          Vxys(next) :=   Xys(mseg*2+2);
        elsif projector = 42 then
          Vxys(next-3) := Pipe_pars(ii+7);
          Vxys(next-2) := Pipe_pars(ii+8); 
          Vxys(next-1) := Xys(seg*2+1);
          Vxys(next) :=   Xys(seg*2+2);
          next := next+4;
          Vxys(next-3) := Pipe_pars(ii+9);
          Vxys(next-2) := Pipe_pars(ii+10); 
          Vxys(next-1) := Xys(mseg*2+1);
          Vxys(next) :=   Xys(mseg*2+2);
        end if;
        end if;
        End If;
    End Loop;
    
    If Vxys.count = 0 then
       RETURN NULL;
    Elsif Vxys.count = 4 then
    VGeom := MDSYS.SDO_GEOMETRY(GTYPE,Geom.SDO_SRID,NULL,Mdsys.sdo_elem_info_array(1,2,1),
                                 VXys);
    Else
      Gtype := 2006.;
      Vinfo.extend(3*TRUNC(next/4));
      
      for i in 1..TRUNC(next/4) loop
         Vinfo(i*3-2) := (i-1)*4+1;
         Vinfo(i*3-1) := 2;
         Vinfo(i*3) := 1;
      end loop;
      VGeom := MDSYS.SDO_GEOMETRY(GTYPE,Geom.SDO_SRID,NULL,Vinfo,VXys);
    End if;
    RETURN VGeom;
  end if;
  
  RETURN NULL;
   
END GET_Pipe_Segs;
--
FUNCTION GET_Angles(Geom IN MDSYS.SDO_GEOMETRY,smallest NUMBER default 0.0) RETURN MDSYS.SDO_LIST_TYPE  AS

-- Function callable from PL/SQL to measure Angles  and
-- return the smallest angle in a geometry, its position in the coordinates and
-- then the angle list (A1,, A2,..)

--SQL> select gz_qa.get_angles(sdogeometry) angle_list from cmp_150v8 where geo_id='1500000US530330303141';

--ANGLE_LIST
----------------smallest position A1    A2        A3        A4      A5        ..
--SDO_LIST_TYPE(82.4369, 17, 167.2396, 158.949, 102.9733, 173.7558, 150.9714, 164.
--4053, 179.298, 165.4775, 117.1999, 171.4071, 89.8185, 179.8077, 179.9497, 89.948
--4, 122.2755, 113.4667, 82.4369, 164.1819, 173.8649)


  Angles            MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();
  Xys               MDSYS.SDO_ORDINATE_ARRAY:= Geom.SDO_ORDINATES;
  Info              MDSYS.SDO_ELEM_INFO_ARRAY := Geom.SDO_ELEM_INFO;
  n                 PLS_INTEGER;
  no_info           PLS_INTEGER;
  istart            PLS_INTEGER;
  iend              PLS_INTEGER;
  ii                PLS_INTEGER;
  kount             PLS_INTEGER;
  loops             PLS_INTEGER;
  next             PLS_INTEGER :=2;
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
  where_is          NUMBER := 0;
  GTYPE             NUMBER := Geom.sdo_gtype;
  Interpretation    NUMBER;
BEGIN


-- Ignore point, multipoint and hetereogenous collections
  if geom is NULL or gtype = 2001 or gtype=2004 or gtype = 2005 then
     RETURN NULL;
  end if;

  interpretation := Info(3);
  if interpretation > 1 then
     RETURN NULL;
  end if;

  no_info := TRUNC(Info.count/3);
  n := Xys.count/2;
  Angles.extend(n+4 - no_info); -- smallest comes first, then its offset, then
  --                               all angles. Note there are n-1 vertices when
  --                               the xys.count/2 = n
 
  
  FOR loops in 1..no_info LOOP
    istart := loops*3+1;   -- is 4
    if loops*3 = Info.count then
      iend := Xys.count-2;  -- iend points to the y coordinate of 2nd to last vertex
    else
      iend := Info(istart)-3;  -- of vertex n-1 (vertex n =1)
    end if;

    ii := Info(istart-3);  -- is Info(1)
    kount := TRUNC((iend-ii)/2)+1;  -- Ignore last vertex (same as start)
 
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
      x3 := Xys(ii);
      y3 := Xys(ii+1);
  
  --          (x1,y1)  +          + (x3,y3)  future (x2,y2)
  --                     \       /   \
  --                       \   /       \
  --               (x2,y2)  +           + future (x3,y3)
  --                     future (x1,y1)
      dx := x1-x2;
      dy := y1-y2;
      dx2 := x3-x1;
      dy2 := y3-y1;
      dx3 := x3-x2;
      dy3 := y3-y2;
      abs_dx2_dy2 := ABS(dx2) + ABS(dy2);
      IF abs_dx2_dy2 < ABS(dx) + ABS(dy) or
         abs_dx2_dy2 < ABS(dx3) + ABS(dy3) then
  
        Angle1 := Faster_atan2(dy,dx,TRUE);
  --      Angle1 := New_arctan(dy,dx,TRUE);
  --angle1 := atan2(dy,dx) * 57.29577951308232087679815481410517033235;
  
        Angle2 := Faster_atan2(dy3,dx3,TRUE);
  --      Angle2 := New_arctan(dy3,dx3,TRUE);
  --      angle2 := atan2(dy2,dx2) * 57.29577951308232087679815481410517033235;
  --    if ii = 6 then
  --      dbms_output.put_line('ii ' || ii || ' angle1 ' || round(angle1,3) || ' ' ||round(angle2,3));
  --    end if;
        Angle := ABS(angle1-angle2);
  
        if angle > 180. then
          angle := 360. -angle;
        end if;
  --           dbms_output.put_line('ii ' || ii || ' angle ' || round(angle,4));
        if angle < small then
          small := angle;
          Angles(1) := small;
          Angles(2) := TRUNC(ii/2)-1.;  -- segment number
        end if;
        if smallest = 0. then
        next := next+1;
        Angles(next) := ROUND(angle,4);
        elsif angle < smallest then
        next := next+1;
        Angles(next) := ROUND(angle,4); 
        next := next+1;
        Angles(next) := TRUNC(ii/2)-1.;
--        dbms_output.put_line('ii ' || ii || ' angle ' || round(angle,3) || ' x2 ' || round(x2,7) || ' y2 ' ||round(y2,7) || ' 709 ' || round(xys(709),7));
        end if;
     End if;
    End Loop;
  End Loop;

  Angles.trim(angles.count-next);

  RETURN Angles;


END GET_AngleS;
--
FUNCTION GET_WIDTH(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 500.,choose VARCHAR2 default 'SMALL',pgap PLS_INTEGER default 2) RETURN NUMBER  AS
  Widths            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Distances         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Where_is          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Matches           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  
  avg_width              NUMBER:=0.0;
BEGIN
If Geom is NULL then
  RETURN NULL;
End If;
    Widths := Get_Pipe_Widths(Geom,Where_is,Matches,Distances,cutoff_width);
    if Widths is NOT NULL then
      if UPPER(choose)='SMALL' then
      RETURN Widths(1);
      else  -- return the average
        for ii in 1..widths.count loop
           avg_width := avg_width + Widths(ii);
--           dbms_output.put_line('ii '||ii || ' ' || widths(ii));
        end loop;
        avg_width := avg_width/Widths.count;
        RETURN avg_width;
      end if;
    else
      RETURN NULL;
    end if;
END GET_WIDTH;
--
FUNCTION GET_WIDTHS(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 500.,pgap PLS_INTEGER default 2,no_to_find PLS_INTEGER default 20,return_where BOOLEAN default TRUE) RETURN MDSYS.SDO_LIST_TYPE  AS
  Widths            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  WW                MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Distances         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Where_is          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Matches           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
BEGIN
    Widths := Get_Pipe_Widths(Geom,Where_is,Matches,Distances,cutoff_width,pgap,no_to_find);
    if Widths is NULL then
--    dbms_output.put_line('>>>>>>>>>>>>>>>>>>>>returning NULL');
       RETURN NULL;
    end if;
    if Widths.count > 0 and return_where then
      WW.extend(widths.count*2);
         for ii in 1..Widths.count loop
            WW(ii*2-1) := Widths(ii);
            WW(ii*2) := Where_is(ii);
         end loop;
      RETURN WW;
    elsif Widths.count > 0 and NOT return_where then
--       dbms_output.put_line('>>>>>>>>>>>>>>>>>>>>returning ' || Widths(1));
       RETURN Widths;
    else
      RETURN NULL;
    end if;
END GET_WIDTHS;
--
    function simple_intersect( x1 number,y1 number,x2 number,y2 number,
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
   
    end simple_intersect;  
    --
FUNCTION GET_Pipe_WIDTHS(Geom IN MDSYS.SDO_GEOMETRY,Where_is IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Matches IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Distances IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                          cutoff_width NUMBER default 500.,pgap PLS_INTEGER default 2,no_to_find PLS_INTEGER default 20) RETURN MDSYS.SDO_LIST_TYPE  AS

--Find shortest gap between coordinates which are gap apart.
--Example, gap=1 find vertices too close, gap=2 find a sliver triangle

  deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
  Xys               MDSYS.SDO_ORDINATE_ARRAY:= Geom.SDO_ORDINATES;
  Widths            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(); 
  N                 PLS_INTEGER;
  j                 PLS_INTEGER := 0;
  apart             PLS_INTEGER ;
  last_where        PLS_INTEGER;

  last_match        NUMBER;
  last_dist         NUMBER;
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
  distance          NUMBER :=0.0;
  distance_try      NUMBER :=0.0;
  dist_save         NUMBER :=0.0;
  pos               NUMBER;
  a_1               NUMBER := 0.9933056431085355608716884491956183391161;
  a_2               NUMBER := 0.009973972089654263505971285993829439678765;
  a_3               NUMBER := 0.00008445133481869081114188534958332938725393;
  a_4               NUMBER :=-0.0000000207345260587865496039317049547208138981;
  c1                NUMBER;
  c2                NUMBER;
  projector         NUMBER;
  Big               NUMBER := 1.E10;
  SRID              NUMBER := Geom.sdo_SRID;
  dist_factor       NUMBER := 111319.490793274;
   
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

    function perpendicular(xin number,yin number,x1 number,y1 number,x2 number,y2 number,
                            xL number,yL number,xR number, yR number) return number as
 
    -- Measure perpendicular distance in meters accurately between a point and a line
    
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
     
     x11           number :=x1;
     y11           number := y1;
     x22           number := x2;
     y22           number := y2;
    begin
--     dbms_output.put_line('xin ' || xin || ' yin ' || yin);

     u := ((Xin - X1) * dx + (Yin-Y1) * dy);

     If u >= 0. then 
        length_sq :=  dx*dx + dy*dy;
--        dbms_output.put_line('UU ' ||u || ' lensq ' || round(length_sq,6));
        if u <= length_sq  and length_sq > 0. then

           u := u/length_sq;

           xnear := X1 + u * dx;
           ynear := Y1 + u * dy;
           
           -- We have a perpendicular but does it intersect either the segment
           -- before or the segment after. That is not allowed.
           -- Caller has set up xb,yb (back) and xf,yf (forward)
           
           if xb is NOT NULL and seg_intersect(xin,yin,xnear,ynear,xb,yb,x1,y1) then
              Return 0.0;  -- A zero width is interpreted as a failure
           end if;
           if xf is NOT NULL and seg_intersect(xin,yin,xnear,ynear,x2,y2,xf,yf) then
              Return 0.0;  -- zero width => failure
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

        end if;
     end if;
     RETURN approx_width;
    end;
    
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
BEGIN

 
  n := Xys.count/2;
  Widths.extend(n*n);
  Where_is.extend(n*n);
  Matches.extend(n*n);
  Distances.extend(n*n);
  ya := ABS(Xys(2));
  cosy_a := cos(ya*deg2rad);
  siny_a := sqrt(1.-cosy_a*cosy_a);
  set_c1c2(ya);
           
  
  
  FOR gap in  pgap..n-1 LOOP
    x1 := NULL;
    y1 := NULL;
    x2 := Xys(1);
    y2 := Xys(2);
    apart := gap*2+2;
--    dbms_output.put_line('gap ' || gap || ' pos ' || apart);
    xnext := Xys(apart-1);
    ynext := Xys(apart);
    For ii in 2..n-gap Loop
--    dbms_output.put_line('ii ' || ii || ' n ' || n);
--                      Back                                       Forward
--   vertex order is:   (xb,yb)   (x1,y1)           (x2,y2)        (xf,yf)
--                or    (xb,yb)   (xlast,ylast)     (xnext,ynext)  (xf,yf)
      x0 := x1;
      y0 := y1;
      x1 := x2;
      y1 := y2;
      x2 := Xys(ii*2-1);
      y2 := Xys(ii*2);
      if ii < n then
       x3 := Xys(ii*2+1);  y3 := Xys(ii*2+2);
      else
        x3 := NULL; y3 := NULL;
      end if;
      pos := (ii-1)*2 + apart;
      xlast := xnext;
      ylast := ynext;
      xnext := Xys(pos-1);
      ynext := Xys(pos);
  
-- Begin by projecting the current segment onto the matching segment
--
        width := 0.0;
        xb := x0; yb := y0;
        xf := x3; yf := y3;
        width3 := Perpendicular(xlast,ylast,x1,y1,x2,y2,x1,y1,NULL,NULL);
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
        
        width1 := Perpendicular(x1,y1,xlast,ylast,xnext,ynext,x1,y1,NULL,NULL);
 
        
--      Test for case 13       
        if width1 <> 0.0 and width1 <> Big and width3 <> 0. and width3 <> Big then
          projector := 3.;
          distance := distance_try;
          width := width1;
        elsif width3 <> 0. and width3 < cutoff_width then
--        if width <> Big and ii=58 then
--        dbms_output.put_line('WWW ' || round(width,3) || ' segment ' || (ii-1)|| ' with  vertex ' || trunc((pos-1)/2));
--        end if;
          xb := x0; yb := y0;
          xf := x3; yf := y3;
          width4 := Perpendicular(xnext,ynext,x1,y1,x2,y2,xlast,ylast,xnext,ynext);
          projector := 3.;
-- Test for Case 43
          if width4 <> 0. and width4 < cutoff_width then
           distance := distance_try;
           width := width4;                    
          end if;
--          if width <> Big and ii=58 then
--           dbms_output.put_line('WW ' || round(width,3) || ' segment ' || trunc((pos-1)/2)|| ' with vertex ' || (ii-1));
--          end if;
        elsif width1 <> 0.0 and width1 < cutoff_width then
          width2 := Perpendicular(x2,y2,xlast,ylast,xnext,ynext,x1,y1,x2,y2);
-- Test for Case 12
          if width2 <> 0. and width2 < cutoff_width then
           projector := 1.;
           distance := distance_try;
           width := width2;                    
          end if;
-- Both 1 and 3 failed to project, so try 2 and 4
        elsif width1 = Big and Width3 = Big then
          width2 := Perpendicular(x2,y2,xlast,ylast,xnext,ynext,x1,y1,x2,y2);
          xb := x0; yb := y0;
          xf := x3; yf := y3;
          width4 := Perpendicular(xnext,ynext,x1,y1,x2,y2,x2,y2,NULL,NULL);
-- Test for Case 24
          if width2 <> 0. and width2 < cutoff_width and width4 <> 0. and width4 < cutoff_width then
           projector := 2.;
           distance := distance_try;
           width := width2;                    
          end if;
        end if;

--          if width <> Big and ii=58 then
--           dbms_output.put_line('W ' || round(width,3) || ' segment ' || trunc((pos-1)/2)|| ' with  ' || ii);       
--          end if;

        
        if width < cutoff_width and width <> 0. then
--        dbms_output.put_line('WW' || round(width,3) ||' D ' || round(distance,3) );
--        dbms_output.put_line('X ' || round(xnext,7) ||','||round(ynext,7) );
          j := j+1;
         Widths(j) := ROUND(width,3);
         Distances(j) := ROUND(distance,3);
         where_is(j) := ii-1.;
         Matches(j) := TRUNC((pos-1)/2) +0.01*projector;  -- meaning the vertex projects
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
  END LOOP;
  Shellsort4(Where_is,Matches,Widths,Distances,1,j);
  
  Where_is.trim(Where_is.count-j);
  Matches.trim(Matches.count-j);
  Widths.trim(Widths.count-j);
  Distances.trim(Distances.count-j);
  if Widths.count=0 then
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
FUNCTION GET_SHORT_GCD(Geom IN MDSYS.SDO_GEOMETRY) RETURN NUMBER DETERMINISTIC AS
   shorts  MDSYS.SDO_LIST_TYPE;
BEGIN
   shorts := get_gcds(geom,'SHORT');
   RETURN(shorts(1));
END GET_SHORT_GCD;
--
FUNCTION GET_GCDS(Geom IN MDSYS.SDO_GEOMETRY,psmallest VARCHAR2 default 'SHORT',coords VARCHAR2 default 'YES') RETURN MDSYS.SDO_LIST_TYPE  AS

-- Function callable from PL/SQL to measure Great Circle Distances (GCDs)  and
-- return the shortest distance in a geometry, its position in the coordinates
-- and the actual segment coordinates:

--SQL> select gz_qa.get_gcds(sdogeometry) from cmp_150v8 where geo_id='1500000US530330303141';

--GZ_QA.GET_GCDS(SDOGEOMETRY)
-----------position, distance (meters), and segment coordinates (x1,y1, x2,y2)
--SDO_LIST_TYPE(18, 12.3314, -122.32394, 47.308806, -122.3241, 47.308776)


  Distances         MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();
  Xys               MDSYS.SDO_ORDINATE_ARRAY:= Geom.SDO_ORDINATES;
  Info              MDSYS.SDO_ELEM_INFO_ARRAY := Geom.SDO_ELEM_INFO;
  N                 PLS_INTEGER;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;
  fnd_x1            NUMBER;
  fnd_y1            NUMBER;
  fnd_x2            NUMBER;
  fnd_y2            NUMBER;
  gcd               NUMBER := 0.0;
  seg12             NUMBER;
  s12               NUMBER;
  az0               NUMBER;
  az1               NUMBER;
  m12               NUMBER;
  dist_found        NUMBER;
  small             NUMBER := 1.E20;
  where_is          NUMBER := 0;
  GTYPE             NUMBER := Geom.sdo_gtype;
  SRID              NUMBER := Geom.sdo_SRID;
  Interpretation    NUMBER;
  smallest          VARCHAR2(10) := psmallest;
  j                 PLS_INTEGER :=0;
BEGIN

-- Ignore point, multipoint and hetereogenous collections
  if gtype = 2001 or gtype=2004 or gtype = 2005 then
     RETURN NULL;
  end if;

  interpretation := Info(3);
  if interpretation > 1 then
     RETURN NULL;
  end if;

  n := Xys.count/2;
  Distances.extend(n+1);

  x2 := Xys(1);
  y2 := Xys(2);

  if smallest = 'LONG' then  -- If caller wants the longest distance, then flip
    small := 0.0;

-- If user wants to find all distances less than a certain amount

  elsif SUBSTR(smallest,1,1) = '<' then

    small := SUBSTR(smallest,2,20);
    smallest := 'RANGE';
     Distances.extend(n+1);
  end if;

  For ii in 2..n Loop
    x1 := x2;
    y1 := y2;

    x2 := Xys(ii*2-1);
    y2 := Xys(ii*2);

--    seg12 := geodesic.inverse(y1,x1,y2,x2,s12,az0,az1,m12);
--    Distances(ii) := s12;

    dist_found := round(distance_fcn(x1,y1,x2,y2,SRID),4);
--     dist_found := sdo_geom.sdo_length(mdsys.sdo_geometry(2002,8265,null,mdsys.sdo_elem_info_array(1,2,1),mdsys.sdo_ordinate_array(x1,y1,x2,y2)),0.05,'unit=meter');
--    dbms_output.put_line('ii ' || ii || ' x2 ' || round(x2,9) || ' y2 ' || round(y2,9));
    if (smallest = 'SHORT' and dist_found < small) OR    -- return smallest
       (smallest = 'LONG' and dist_found > small ) then  -- or biggest
       small := dist_found;
       where_is := ii-1.;

       fnd_x1 := x1;
       fnd_y1 := y1;
       fnd_x2 := x2;
       fnd_y2 := y2;
--      dbms_output.put_line('ii ' || (ii-1) || ' ' || round(small,4) || ' xx1 ' || xx1 || ' yy1 ' || yy1 || ' ' || xx2 || ' ' || yy2);
    elsif smallest = 'RANGE' and  dist_found < small then
       j:=j+2;
        Distances(j) := ii-1;
        Distances(j-1) := dist_found;
    elsif smallest = 'TOTAL' then
        Distances(ii+1) := dist_found;
    end if;
    gcd := gcd + dist_found;
  End Loop;

   if smallest <> 'RANGE' then
  Distances(1) := gcd;
  end if;
  if smallest = 'RANGE' then
     Distances.trim(distances.count-j);
  elsif smallest = 'SHORT' or smallest = 'LONG' then
    Distances(2) := where_is;
    Distances(1) := small;
    if coords = 'YES' then
      if Distances.count < 6 then
        Distances.extend(4);
      end if;
      Distances(3) := fnd_x1;
      Distances(4) := fnd_y1;
      Distances(5) := fnd_x2;
      Distances(6) := fnd_y2;
      Distances.trim(distances.count-6);
    else
      Distances.trim(distances.count-2);
    end if;
  end if;
  RETURN Distances;


END GET_GCDS;
--
FUNCTION GET_XYS(
   Geom IN MDSYS.SDO_GEOMETRY,
   pring1 PLS_INTEGER,
   pseg1_to_find PLS_INTEGER,
   pring2 PLS_INTEGER DEFAULT NULL,
   pseg2_to_find PLS_INTEGER default NULL,
   convert VARCHAR2 default '8265'
) RETURN sdo_geometry AS
/**
--###############################################################################
--Program Name: GetXys
--Author: Sidey + Matt!
--Creation Date:  June? 2009
--Updated:      09/11/2013 To handle a range going around the end/start of ring
--              4/1/20111 To allow absolute segments when ring is zero
--              Feb 5/2010 to return a range of segments and preserve SRID: Sidey
--
--Usage:
-- validate gives : 13349 [Element <1>] [Ring <1>][Edge <20028>][Edge <20040>]
-- select MATTOOL.get_xys(a.sdogeometry,1,20028,NULL,20040) from
-- j12802711008001_srcwrk a where a.oid = 4453141
--
--Purpose: 1) Return a segment, 2 segments or a range of segments from a geometry
--   2) Prints all digits when convert = 'PRINT' or
--   3) sets SRID to NULL when convert <> '8265' and <> 'PRINT'

--Method: Makes a new geometry
--Dependencies: None
--
--##############################################################################
*/

    XYOrd             MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    New_XYs           MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
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
    pos       PLS_INTEGER;
    kount     PLS_INTEGER :=0;
    next      PLS_INTEGER;
    n         PLS_INTEGER;

    GTYPE     NUMBER:= geom.SDO_GTYPE;
    SRID      NUMBER:= geom.SDO_SRID;

    retval   SDO_GEOMETRY;

    procedure print_all_digits as
    
    ij      pls_integer;
    begin
         -- See full precision of coordinates in all their detail

     if UPPER(convert) = 'PRINT' then
        for ii in TRUNC((LB-1)/2)..TRUNC((LB+kount-2)/2) loop
           ij := MOD(ii*2,XYOrd.count) +1;
           dbms_output.put_line(XYOrd(ij)||','||XYOrd(ij+1)||',');
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

     Info := geom.SDO_ELEM_INFO;
     XYORD := geom.SDo_Ordinates;
     rings := TRUNC(Info.count/3);
     
       next := 1;
       While next+3 < Info.count and 2*Abs(seg1_to_find) > Info(next+3) loop
          next := next +3;
       End loop;

     if ring1 = 0 then
       ring1 := TRUNC(next/3) +1;    
       seg1_to_find := seg1_to_find - TRUNC(Info(next)/2);
     end if;
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
                              
-- Return a range: the range maygo around the end of a polygon like this:
-- select gz_qa2.get_xys(sdogeometry,1,-408,1,4) from z640sp_clip_face where face_id=56

     elsif (seg1_to_find < 0 or seg2_to_find < 0) and ring1 = ring2 then

        if abs(seg2_to_find) < abs(seg1_to_find) then
           n := XyOrd.count;
           if next <> 1 then
              n := Info(next)-1;
           end if;
           
           kount := abs(seg2_to_find)*2;
           if kount > XyOrd.count then
              kount := XyOrd.count;
           end if;
           kount := n-LB+1 +kount;
           New_Xys.extend(kount);
           For ii in LB..n Loop
             New_Xys(ii-LB+1) := XYOrd(ii); 
           End Loop;
-- Skip 1st coordinate           
           kount := abs(seg2_to_find)*2+2;
           For ii in 3..kount Loop            
             New_Xys(ii+n-LB-1) := XYOrd(ii);
           End Loop;
           
           LB := 1;
           XYOrd := New_Xys;
           
        else
        kount := ((abs(seg2_to_find) - abs(seg1_to_find)) +2) *2;
        if LB+kount-1 > XyOrd.count then
          kount := XyOrd.count-LB+1;
        end if;
        
        For ii in 1..kount Loop
           XYOrd(ii) := XYOrd(LB+ii-1);
        End Loop;

        LB :=1;
        XYOrd.trim(XYOrd.count-kount);
        end if;
        kount := XYOrd.count;
        
        if xyord(1) = xyord(Xyord.count-1) and xyord(2) = xyord(xyord.count) then
          retval := sdo_geometry (2003, SRID, null,
                              sdo_elem_info_array (1,1003,1), XYOrd);
        else
        retval := sdo_geometry (2002, SRID, null,
                              sdo_elem_info_array (1,2,1), XYOrd);
        end if;
     else

        LB := Info(j) + (seg2_to_find -1) *2;    --  ( I presume he gives the relative segment number)
        kount := 4;
        retval := sdo_geometry (2006, SRID, null,
                              sdo_elem_info_array (1,2,1, 5,2,1),
                              sdo_ordinate_array (xlast,ylast, xnew,ynew,
                              XYOrd(LB),XYOrd(LB+1),XYOrd(LB+2),XYOrd(LB+3)));
                  
     end if;
     
     print_all_digits;

     RETURN retval;

END Get_Xys;
--
FUNCTION GET_SHORT_EDGES( pTopology VARCHAR2, face_id NUMBER, pInSchema VARCHAR2 default 'GZDEC10ST', tolerance NUMBER default 0.05, decim_digits NUMBER default 7) RETURN MDSYS.SDO_LIST_TYPE AS

-- Returns a list of short edges, Positive means they are on the state line,
-- negative means they are not!

-- Note the incomplete schmema name in the input arguments! Get short edges
-- will complete the schema name from the Topology name!

-- example:
--SQL> select get_short_edges('Z615LS',4640) from dual;

--GET_SHORT_EDGES('Z615LS',4640)
-------------------------------------------------------
--SDO_LIST_TYPE(12016)


   InSchema         VARCHAR2(30)     := UPPER(NVL(pInSchema,USER));
   Topology         VARCHAR2(30)     := UPPER(pTopology);
   Distances        MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();

   Edge_ids         MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();
   Bad_ids          MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();
   geometry         MDSYS.SDO_GEOMETRY;
   geom2            MDSYS.SDO_GEOMETRY;
   Xys              MDSYS.SDO_ORDINATE_ARRAY;
   sql_stmt         VARCHAR2(4000);
   state            VARCHAR2(2);
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;
  xx1               NUMBER;
  yy1               NUMBER;
  xx2               NUMBER;
  yy2               NUMBER;
  edge_id           NUMBER;
  left_face         NUMBER;
  right_face        NUMBER;
  found             NUMBER :=0.0;
  j                 PLS_INTEGER;
  len               PLS_INTEGER;
  next              PLS_INTEGER :=0;
BEGIN

-- Make the schema name from a partial schema name and the state FIPS in
-- the Topology  z6nn

--   len := length(Inschema);
--   state := SUBSTR(Topology,3,2);
--   if SUBSTR(Inschema,len-1,2) <> state then
--     Inschema := Inschema || state;
--   end if;
   Topology := Inschema ||'.' ||Topology;

--   dbms_output.put_line('state ' || state || ' ' ||Inschema);
   sql_stmt := 'select GZ_QA.get_GCds(t.sdogeometry) from '||Topology ||'_clip_face t where t.face_id=:1';
   execute immediate sql_stmt into Distances using face_id;

   if Distances.count = 0 then
      RETURN bad_ids;
   else
      Bad_ids.extend(10);
   end if;

   sql_stmt := 'select left_face_id,right_face_id,geometry from '||Topology ||'_EDGE$ t where t.edge_id=:1';

   xx1 := ROUND(Distances(3),decim_digits);
   yy1 := ROUND(Distances(4),decim_digits);
   xx2 := ROUND(Distances(5),decim_digits);
   yy2 := ROUND(Distances(6),decim_digits);
  -- Get the edge ids
   edge_ids := sdo_topo.get_face_boundary(Topology,face_id);

   for ij in 1..edge_ids.count loop
      edge_id := edge_ids(ij);
      execute immediate sql_stmt into left_face,right_face,geometry using abs(edge_id);

      dbms_output.put_line('edge_id ' || edge_id ||' ' || edge_ids.count);
      if edge_id > 0 then
         Xys := geometry.sdo_ordinates;
      else
         geom2 := sdo_util.reverse_linestring(geometry);
         Xys := geom2.sdo_ordinates;
         edge_id := -edge_id;
      end if;

      x2 := ROUND(Xys(1),decim_digits);
      y2 := ROUND(Xys(2),decim_digits);

      for ii in 2..TRUNC(xys.count/2) loop
        x1 := x2;
        y1 := y2;
        j := ii*2;
        x2 := ROUND(xys(j-1),decim_digits);
        y2 := ROUND(xys(j),decim_digits);

        if x1 = xx1 and y1 = yy1 then
          found := found + 1;
--          dbms_output.put_line('f ' || found);
        end if;
        if x2 = xx2 and y2 = yy2 then
          found := found + 10;
--          dbms_output.put_line('F ' || found);
        end if;
      end loop;
-- Have we found the short edge (or edge with the short segment
      if found > 10 then
           next := next + 1;
          if next > bad_ids.count then
             bad_ids.extend(10);
          end if;
          if left_face < 0 or right_face < 0 then
            bad_ids(next) := edge_id;
          else
            bad_ids(next) := -edge_id;
          end if;
      end if;
      exit when found > 10;
--dbms_output.put_line('Edge_id ' || edge_id || ' ' || ij);
   end loop;
   bad_ids.trim(bad_ids.count-next);
   RETURN bad_ids;


END GET_SHORT_EDGES;
--
PROCEDURE Build_QA_TBLS_VIEWS(
   pGenSchema VARCHAR2,
   pGenTopology VARCHAR2,
   pGenSdoGeomCol VARCHAR2,
   pUnGenSchema VARCHAR2,
   pUnGenTopology VARCHAR2,
   pUnGenSdoGeomCol VARCHAR2,
   pOutPutTableSuffix VARCHAR2,
   pTARGET_SCALE NUMBER,
   pCOMPARE_GEN_COLUMN VARCHAR2,
   pAREA_CHECK NUMBER, -- default NULL,
   pSHAPE_CHECK NUMBER, -- default 1.5,
   pEntireTopology VARCHAR2, -- default 'Y',
   pGenTableName VARCHAR2, -- default NULL,
   pUnGenTableName VARCHAR2, -- default NULL,
   pMissingRecTBL  VARCHAR2,
   pMissingRecKey  VARCHAR2,
   pMissingRecFeatTbl VARCHAR2
   )
IS
/*
   pGenSchema VARCHAR2(50) := 'GZDEC10ST01';
   pGenTopology VARCHAR2(50):=  'Z901LS';
   pGenSdoGeomCol VARCHAR2(50):= 'GEOMETRY';
   pUnGenSchema VARCHAR2(50):= 'GZDEC10ST01';
   pUnGenTopology VARCHAR2(50):= 'MT901';
   pUnGenSdoGeomCol VARCHAR2(50):= 'GEOMETRY';
   pOutPutTableSuffix VARCHAR2(50):= 'CMP_TABLES';
   pEntireTopology VARCHAR2(50) := 'Y';
   pGenTableName VARCHAR2(50);
   pUnGenTableName VARCHAR2(50);
*/

   vGenSchema VARCHAR2(50) := pGenSchema;
   vGenTopology VARCHAR2(50) := pGenTopology;
   vGenSdoGeomCol VARCHAR2(50) := pGenSdoGeomCol;
   vUnGenSchema VARCHAR2(50) := pUnGenSchema;
   vUnGenTopology VARCHAR2(50) := pUnGenTopology;
   vUnGenSdoGeomCol VARCHAR2(50) := pUnGenSdoGeomCol;
   vOutPutTableSuffix VARCHAR2(50) := pOutPutTableSuffix;
   vEntireTopology VARCHAR2(50) := pEntireTopology;
   vGenTableName VARCHAR2(50) := pGenTableName;
   vUnGenTableName VARCHAR2(50) := pUnGenTableName;
   vOutPutTable VARCHAR2(50);
   vFSL  VARCHAR2(50);
   vQAFSL_Vw  VARCHAR2(50);

   TYPE cTable IS TABLE OF VARCHAR2(50);
   vArrUnGenTab  cTable := cTable();
   vArrUnGenFSL  cTable := cTable();
   vArrGenTab  cTable := cTable();
   vArrGenFSL  cTable := cTable();

   vArrTmp  cTable := cTable();

   vArrFSLViewCols   cTable := cTable();
   vArrOutPutTableCols   cTable := cTable();
   vArrQACols cTable := cTable();
   vArrQAViewCols   cTable := cTable();

   vQaReportTable  varchar2(32);

   vMissingRecTBL varchar2(30) := pMissingRecTBL;
   vMissingRecKey varchar2(30) := pMissingRecKey;
   vMissingRecFeatTbl varchar2(30) := pMissingRecFeatTbl;

   SQL1 varchar2(4000);
   SQL2 varchar2(4000);
   rcdCount number;
   tmpSRID number;

   SkipDu2Err varchar2(1);
Begin
   vArrQACols.extend(4);
   vArrQACols(1) := 'B.QC'; --'cast(NULL AS VARCHAR2(1)) QC';
   vArrQACols(2) := 'B.QA_FLAG_CNT';
   vArrQACols(3) := 'cast(NULL AS VARCHAR2(10)) QCSTATUS';
   vArrQACols(4) := 'cast(NULL AS VARCHAR2(4000)) QCCOMMENTS';

   -- Validate input
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   DBMS_APPLICATION_INFO.SET_ACTION('QA: Validate Input');
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO(' ');
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   IF vEntireTopology = 'Y' THEN
      -- 1) UnGen Topology should exist
      SQL1 := 'Select count(distinct topology) from all_sdo_topo_info ';
      SQL1 := SQL1 || ' Where owner = :1 and topology = :2';

      Execute immediate SQL1 into rcdCount using vUnGenSchema, vUnGenTopology;

      If rcdCount = 1 then
         dbms_output.put_line('Topology ' || vUnGenSchema || '.' || vUnGenTopology || ' exists');
      else
         RAISE_APPLICATION_ERROR(-20001,'Topology ' || vUnGenSchema || '.' || vUnGenTopology || ' does not exist');
      end if;

      -- 2) Gen Topology should exist

      Execute immediate SQL1 into rcdCount using vGenSchema, vGenTopology;

      If rcdCount = 1 then
         dbms_output.put_line('Topology ' || vGenSchema || '.' || vGenTopology || ' exists');
      else
         RAISE_APPLICATION_ERROR(-20001,'Topology ' || vGenSchema || '.' || vGenTopology || ' does not exist');
      end if;


      -- 3) UnGen FSL tables should match Gen FSL tables
      --       If tables don't match raise warning
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_ACTION('QA: Check FSL table counts');
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO(' ');
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--

      SQL1 := 'select table_name, substr(table_name,length(topology)+2) From all_sdo_topo_info ';
      SQL1 := SQL1 || ' where owner = :1 ';      --'GZDEC10ST01'
      SQL1 := SQL1 || '  and topology = :2 '; -- 'Z901LS'
      SQL1 := SQL1 || '  and table_name like :3 '; -- topology || '_FSL%'
      SQL1 := SQL1 || '  Order by table_name';

      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
      EXECUTE IMMEDIATE SQL1 BULK COLLECT INTO vArrUnGenTab, vArrUnGenFSL USING vUnGenSchema, vUnGenTopology, vUnGenTopology || '_FSL%';

      SQL1 := 'select table_name, substr(table_name,length(topology)+2)  From all_sdo_topo_info ';
      SQL1 := SQL1 || '  where owner = :1 '; -- 'GZDEC10ST01'
      SQL1 := SQL1 || '  and topology = :2 '; --'MT901'
      SQL1 := SQL1 || '  and table_name like :3'; --topology || '_FSL%'
      SQL1 := SQL1 || '  Order by table_name';

      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
      EXECUTE IMMEDIATE SQL1 BULK COLLECT INTO vArrGenTab, vArrGenFSL USING vGenSchema, vGenTopology, vGenTopology || '_FSL%';

      vArrTmp := vArrUnGenFSL
                    MULTISET EXCEPT
                 vArrGenFSL;

      If vArrTmp.Count > 0 then
         DBMS_OUTPUT.PUT_LINE('The following FSLs do not exist in the Generalized Topology');
         FOR i IN vArrTmp.First..vArrTmp.LAST
         Loop
            DBMS_OUTPUT.PUT_LINE(vArrTmp(i));
         End Loop;
         RAISE_APPLICATION_ERROR(-20001,'UnGenFSLs are more than GenFSLs');
      End if;

      vArrTmp := vArrGenFSL
                    MULTISET EXCEPT
                 vArrUnGenFSL;

      If vArrTmp.Count > 0 then
         DBMS_OUTPUT.PUT_LINE('The following FSLs do not exist in the Un Generalized Topology');
         FOR i IN vArrTmp.First..vArrTmp.LAST
         Loop
            DBMS_OUTPUT.PUT_LINE(vArrTmp(i));
         End Loop;
         RAISE_APPLICATION_ERROR(-20001,'GenFSLs are more than UnGenFSLs');
      End if;

      FOR i IN vArrUnGenTab.First..vArrUnGenTab.LAST
      Loop
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_ACTION('QA: Processing FSL: ' || vArrGenTab(i));
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO(' ');
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_OUTPUT.PUT_LINE('Processing FSLs ' || vArrUnGenTab(i) || ' and ' || vArrGenTab(i));
         vOutPutTable := vArrGenTab(i) || '_' || vOutPutTableSuffix;

            -- 4) SDOGeomCol should exist in both gen and ungen tables


         SQL1 := 'select count(1) ';
         SQL1 := SQL1 || '   from all_tab_columns ';
         SQL1 := SQL1 || '  where owner = :1'; -- 'GZDEC10ST01'
         SQL1 := SQL1 || '  and table_name = :2'; -- 'MT901_FSL050'
         SQL1 := SQL1 || '  and column_name = :3'; --'GEOMETRY'

         Execute immediate SQL1 into rcdCount using vGenSchema, vArrGenTab(i), vGenSdoGeomCol;

         If rcdCount <> 1 Then
            DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1 || ' rcdCount: ' || rcdCount);
            DBMS_OUTPUT.PUT_LINE('vGenSchema: ' || vGenSchema ||' vArrGenTab(i): ' || vArrGenTab(i) || ' vGenSdoGeomCol: ' || vGenSdoGeomCol || '*');
            DBMS_OUTPUT.PUT_LINE('Column ' || vGenSdoGeomCol || ' does not exist in ' || vGenSchema || '.' || vArrGenTab(i));
            SkipDu2Err := 'Y';
            --RAISE_APPLICATION_ERROR(-20001,'Column ' || vGenSdoGeomCol || ' does not exist in ' || vGenSchema || '.' || vArrGenTab(i));
         End If;

         SQL1 := 'select count(1) ';
         SQL1 := SQL1 || '   from all_tab_columns ';
         SQL1 := SQL1 || '  where owner = :1'; -- 'GZDEC10ST01'
         SQL1 := SQL1 || '  and table_name = :2'; -- 'MT901_FSL050'
         SQL1 := SQL1 || '  and column_name = :3'; --'GEOMETRY'

         Execute immediate SQL1 into rcdCount using vUnGenSchema, vArrUnGenTab(i), vUnGenSdoGeomCol;

         If rcdCount <> 1 Then
            DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1 || ' rcdCount: ' || rcdCount);
            DBMS_OUTPUT.PUT_LINE('vUnGenSchema: ' || vUnGenSchema ||' vArrUnGenTab(i): ' || vArrUnGenTab(i) || ' vUnGenSdoGeomCol: ' || vUnGenSdoGeomCol || '*');
            DBMS_OUTPUT.PUT_LINE('Column ' || vUnGenSdoGeomCol || ' does not exist in ' || vUnGenSchema || '.' || vArrUnGenTab(i));
            SkipDu2Err := 'Y';
            --RAISE_APPLICATION_ERROR(-20001,'Column ' || vUnGenSdoGeomCol || ' does not exist in ' || vUnGenSchema || '.' || vArrUnGenTab(i));
         End If;

        If SkipDu2Err = 'Y' then
           GOTO CONT;
        end if;

         -- 5) Check SRID
         SQL1 := 'Select A.' || vGenSdoGeomCol || '.sdo_srid, count(1) '; --GEOMETRY.sdo_srid, count(1) ';
         SQL1 := SQL1 || '   From ' || vGenSchema || '.' || vArrGenTab(i) || ' a '; --GZDEC10ST01.mt901_fsl050 a
         SQL1 := SQL1 || '   group by a.' || vGenSdoGeomCol || '.sdo_srid'; --A.GEOMETRY.sdo_srid

         --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
         Begin

            Execute immediate SQL1 into tmpSRID, rcdCount; -- using vUnGenSchema, vArrUnGenTab(i), vGenSdoGeomCol;

         Exception
         WHEN NO_DATA_FOUND THEN
             DBMS_OUTPUT.PUT_LINE(vGenSchema || '.' || vArrGenTab(i) || ' does not have any records.... Skipping SRID check.');
         WHEN OTHERS THEN
              Raise;
         End;

         If (rcdCount >= 1 and tmpSRID = 8265) Then
            -- Everything looks good.  Do nothing.
            Null;
         Else
            -- Either the SRID is not equal to 8265 or Multiple SRIDs exist
            DBMS_OUTPUT.PUT_LINE('Total Number of SRIDs in ' || vGenSchema || '.' || vArrGenTab(i) || ' = ' || rcdCount || ' and SRID = ' || tmpSRID );
            SkipDu2Err := 'Y';
            --RAISE_APPLICATION_ERROR(-20001,'Please verify the SRIDs in ' || vGenSchema || '.' || vArrGenTab(i) );
         End If;


         SQL1 := 'Select A.' || vUnGenSdoGeomCol || '.sdo_srid, count(1) '; 
         SQL1 := SQL1 || '   From ' || vUnGenSchema || '.' || vArrUnGenTab(i) || ' a '; 
         SQL1 := SQL1 || '   Where A. ' || vUnGenSdoGeomCol || '.sdo_srid  IS NOT NULL ';      
         SQL1 := SQL1 || '   group by a.' || vUnGenSdoGeomCol || '.sdo_srid'; 

         Begin

            Execute immediate SQL1 into tmpSRID, rcdCount; -- using vUnGenSchema, vArrUnGenTab(i), vUnGenSdoGeomCol;
         Exception
         WHEN NO_DATA_FOUND THEN
             DBMS_OUTPUT.PUT_LINE(vUnGenSchema || '.' || vArrUnGenTab(i) || ' does not have any records.... Skipping SRID check.');
         WHEN OTHERS THEN
              Raise;
         End;

         If (rcdCount >= 1 and tmpSRID = 8265) Then
            -- Everything looks good.  Do nothing.
            Null;
         Else
            -- Either the SRID is not equal to 8265 or Multiple SRIDs exist
            DBMS_OUTPUT.PUT_LINE('Total Number of SRIDs in ' || vUnGenSchema || '.' || vArrUnGenTab(i) || ' = ' || rcdCount || ' and SRID = ' || tmpSRID );
            SkipDu2Err := 'Y';
            --RAISE_APPLICATION_ERROR(-20001,'Please verify the SRIDs in ' || vUnGenSchema || '.' || vArrUnGenTab(i) );
         End If;

        If SkipDu2Err = 'Y' then
           GOTO CONT;
        end if;

         -- Drop if vOutPutTable exists
         SQL1 := ' Select count(1) ';
         SQL1 := SQL1 || '   from user_tables ';
         SQL1 := SQL1 || '  where table_name = :1'; --'MT901_FSL050'

         Execute immediate SQL1 into rcdCount using vOutPutTable;

         If rcdCount = 1 Then
            SQL1 := 'Drop Table ' || vOutPutTable || ' purge';
            Execute Immediate SQL1;
         End If;

     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Compute_QA_Values');
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--

            -- Modify Sidey's code to make SdoGeom col diff
            compute_qa_values(vUnGenSchema, vArrUnGenTab(i), vUnGenSdoGeomCol, vGenSchema, vArrGenTab(i), vGenSdoGeomCol, vOutPutTable, pTARGET_SCALE,  pCOMPARE_GEN_COLUMN, pAREA_CHECK, pSHAPE_CHECK);

/*  Un necessary code.  The call to gz_topo_helper.fsl_views is commented out anyway.
            -- Create FSL View  if it doesn't exist
            SQL1 := ' select count(1) from user_views where view_name = :1';
            Execute Immediate SQL1 into rcdCount using vArrGenTab(i) || 'V';
            If rcdCount = 0 then
               DBMS_OUTPUT.PUT_LINE('Creating View ' || vArrGenTab(i) || 'V');
--               gz_topo_helper.fsL_views(vArrGenTab(i));
            End If;

*/

           -- Now create the QA View based on FSL View, QA Output table and QA Columns
           SQL1 := 'select ''A.''' || ' || column_name From user_tab_columns ';
           SQL1 := SQL1 || '  where table_name =  :1';
           SQL1 := SQL1 || '  order by column_id ';

            EXECUTE IMMEDIATE SQL1 BULK COLLECT INTO vArrFSLViewCols USING vArrGenTab(i) || 'V';


/*        2011/11/18 Sreeni:  Add QC and QA_FLAG_CNT to vArrQACols instead.

           SQL2 := 'select ''B.''' || ' || column_name From user_tab_columns ';
           SQL2 := SQL2 || '  where table_name =  :1';
           SQL2 := SQL2 || '  and column_name <>  :2';
           SQL2 := SQL2 || '  order by column_id ';

            EXECUTE IMMEDIATE SQL2 BULK COLLECT INTO vArrOutPutTableCols USING vOutPutTable, 'GEO_ID';
*/

--            vArrQAViewCols :=   vArrFSLViewCols   MULTISET UNION DISTINCT  vArrQACols MULTISET UNION DISTINCT vArrOutPutTableCols;
            vArrQAViewCols :=   vArrFSLViewCols   MULTISET UNION DISTINCT  vArrQACols; -- MULTISET UNION DISTINCT vArrOutPutTableCols;

            SQL1 := 'Select substr(''' || vArrGenTab(i) || ''',instr(''' || vArrGenTab(i) || ''',''_'')+1) from dual';
            Execute Immediate SQL1 into vFSL;
            vQAFSL_Vw := vGenTopology || '_QA_' || vFSL || 'V';

         --Dbms_Output.Put_line('QAViewCols.Count: ' || vArrQAViewCols.count);
         -- 2011-11-18 Sreeni:

        /*
            Create a view by joining the FSL View and QA output table.
            * FSL View has unique geo_id records, QA output table has multiple geo_id records (one for each ring)
            * Select only one record for each geo_id in the QA output table
            ** For a geo_id if the qa_flag is not null set the corresponding QC flag to Y, also get the number of records where qa_flag is not null
            ** If the qa_flag is null for a geo_id, set the QC flag to N.

        */

     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Create QA View: ' || vQAFSL_Vw);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--

         SQL1 := 'Create or Replace View ' || vQAFSL_Vw || ' AS Select ';
         FOR j IN vArrQAViewCols.First..vArrQAViewCols.LAST
         Loop
              --DBMS_OUTPUT.PUT_LINE('j: ' || j || ' ' || vArrQAViewCols(j));
            /* Create the QA View */

            if j = vArrQAViewCols.count then
                 SQL1 := SQL1 || vArrQAViewCols(j) || ' From ' || vArrGenTab(i) || 'V A, '
                                   || ' (Select geo_id, ''Y'' QC, count(*) QA_FLAG_CNT '
                                   || ' from ' || vOutPutTable
                                   || ' where geo_id in ( '
                                   || ' select distinct geo_id '
                                   || ' From ' || vOutPutTable
                                   || ' where decode(nvl(to_char(qa_flag),''N''), ''N'', ''N'', ''Y'') = ''Y'') '
                                   || ' and qa_flag is not null '
                                   || ' Group by geo_id, ''Y'' '
                                   || ' union '
                                   || ' select distinct geo_id, ''N''  QC, 0 QA_FLAG_CNT '
                                   || ' From ' || vOutPutTable
                                   || ' where decode(nvl(to_char(qa_flag),''N''), ''N'', ''N'', ''Y'') = ''N'' '
                                   || ' and geo_id not in ( '
                                   || ' select distinct geo_id '
                                   || ' From ' || vOutPutTable
                                   || ' where decode(nvl(to_char(qa_flag),''N''), ''N'', ''N'', ''Y'') = ''Y'' '
                                   || ' )   '
                                   || ' ) B '
                     || ' where a.geo_id = b.geo_id ';

            else
                  SQL1 := SQL1 || vArrQAViewCols(j) || ',';

            end if;
         End Loop;

         --DBMS_OUTPUT.PUT_LINE('Create View SQL: ' || SQL1);
         EXECUTE IMMEDIATE SQL1;
         DBMS_OUTPUT.PUT_LINE('Created QA View: ' || vQAFSL_Vw);

         SQL1 := 'Grant Select on ' || vQAFSL_Vw || ' to public';
         EXECUTE IMMEDIATE SQL1;

      <<CONT>>
          If SkipDu2Err = 'Y' Then
             DBMS_OUTPUT.PUT_LINE('****************************************************');
             DBMS_OUTPUT.PUT_LINE('Unable to process FSLs ' || vArrUnGenTab(i) || ' and ' || vArrGenTab(i));
             DBMS_OUTPUT.PUT_LINE('****************************************************');
             DBMS_OUTPUT.PUT_LINE(' ');
             SkipDu2Err := '';
          End If;

      End Loop;

      DBMS_APPLICATION_INFO.SET_CLIENT_INFO(' ');

      vArrUnGenTab.DELETE;
      vArrUnGenFSL.DELETE;
      vArrGenTab.DELETE;
      vArrGenFSL.DELETE;
      vArrTmp.DELETE;
      vArrFSLViewCols.DELETE;
      vArrOutPutTableCols.DELETE;
      vArrQACols.DELETE;
      vArrQAViewCols.DELETE;

      -- Sreeni 2011/12/07 Moved flag_affected_entities and find_missing_records outside the loop
      -- Call FLAG_AFFECTED_ENTITIES
       vQaReportTable :=  vGenTopology || '_REPORT';
       --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
       DBMS_APPLICATION_INFO.SET_ACTION('QA: FLAG_AFFECTED_ENTITIES');
       DBMS_APPLICATION_INFO.SET_CLIENT_INFO('vQAReportTable ' || vQaReportTable);
       --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
       FLAG_AFFECTED_ENTITIES(vQaReportTable, vGenTopology, vGenSchema, NULL);

       -- Call find_missing_records('Z601IN','Z601FS','Z601_MISSING','GEO_ID');
       --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
       DBMS_APPLICATION_INFO.SET_ACTION('QA: find_missing_records');
       DBMS_APPLICATION_INFO.SET_CLIENT_INFO('vMissingRecTBL: ' || vMissingRecTBL);
       --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
       find_missing_records(vUnGenTopology, vGenTopology, vMissingRecTBL, vMissingRecKey, vMissingRecFeatTbl);



   ELSIF (pGenTableName IS NOT NULL and pUnGenTableName IS NOT NULL)THEN
      -- 1) UnGen FSL table should exist
      -- 2) Gen FSL table should exist

      -- 3) SDOGeomCol should exist  Check SRID
      DBMS_OUTPUT.PUT_LINE('*****  Not Implemented Yet!  Please call "compute_qa_values" instead *****');

   ELSE

         -- Dont process anything
      NULL;
   END IF;

     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_ACTION('QA: Done with Build_QA_TBLS_Views');
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO(' ');
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--


END Build_QA_TBLS_VIEWS;
--
PROCEDURE CHECKFOR_CLOSE_XYS(pInTable VARCHAR2,pId_column VARCHAR2,PGeometry_column VARCHAR2,pOutput_Table VARCHAR2 default 'SHORT_EDGES',pprint VARCHAR2 default 'NO',ptolerance NUMBER default 0.05) AS
/**
--##############################################################################
--Program Name: Checkfor_close_Xys
--Author: Sidey Timmins
--Creation Date: 7/22/2008
--Updates: 03/08/2011 To handle GEO_IDs which are VARCHAR2 instead of number and
--                    allowed 2007 geometries.
--Usage:
--  This PL/SQL PROCEDURE has 2 required parameters:
--                  PinTable: Table to check
--                  pGeometry_column : an sdo_geometry
--    (optional)    ptolerance: Tolerance in meters - vertices closer than this
--                              are noted.
--
--Purpose: This procdure checks for vertices which are too close.
--
--Method: Filters vertices to determine the ones to report.
--Dependencies: fast_distance
--##############################################################################
*/

   TYPE TblCursorType IS REF CURSOR;
   Table_cursor       TblCursorType;
   TYPE GEOM_ARRAY    IS VARRAY(1048576) OF MDSYS.SDO_GEOMETRY;
   TYPE VCHAR30_ARRAY IS VARRAY(100000) of VARCHAR2(30);
   Geometries         GEOM_ARRAY := GEOM_ARRAY();

    XYOrd             MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Info_Array        MDSYS.SDO_ELEM_INFO_ARRAY;
    Edge_ids          VCHAR30_ARRAY;
    geometry          MDSYS.SDO_GEOMETRY;

    InTable           VARCHAR2(30) := UPPER(pInTable);
    Out_Table         VARCHAR2(30) := UPPER(pOutput_Table);
    Id_Column         VARCHAR2(30) := UPPER(pId_column);
    Geometry_column   VARCHAR2(30) := UPPER(pGeometry_column);
    ii              PLS_INTEGER;
    xlast           NUMBER;
    ylast           NUMBER;
    xnew            NUMBER;
    ynew            NUMBER;
    k               PLS_INTEGER := -1;
    ycheck          NUMBER;
    xcheck          NUMBER;
    distance        NUMBER := 0.;
    rings           PLS_INTEGER;
    LB              PLS_INTEGER;
    UB              PLS_INTEGER;
    j               PLS_INTEGER;

    row_limit       NUMBER := 100;
    tolerance       NUMBER := ptolerance;
    tolerance2      NUMBER;
    oracle_tol      NUMBER := 0.05;    -- usual Oracle tolerance for geometries
    epsilon         NUMBER := 0.0000005;
    GTYPE           NUMBER ;
    SRID            NUMBER ;
    short_edges     PLS_INTEGER := 0;
    short_segments  PLS_INTEGER := 0;
    short_seg_edges PLS_INTEGER := 0;
    print           VARCHAR2(5) := UPPER(pprint);

BEGIN

   if Table_Exists(Out_table) = TRUE then
     execute immediate 'TRUNCATE TABLE ' || Out_Table;
   else
     execute immediate 'CREATE TABLE ' || Out_Table || '(EDGE_ID VARCHAR2(30),DISTANCE NUMBER,VERTEX NUMBER)';
   end if;

   If ptolerance <= 0. then
       tolerance := 0.05;
   End if;
   tolerance2 := tolerance *1.01;

   OPEN Table_cursor FOR 'SELECT '||Id_column||',' ||Geometry_Column  || ' FROM '|| InTable;


  LOOP

     FETCH Table_cursor BULK COLLECT INTO Edge_Ids,Geometries LIMIT Row_limit;
--   dbms_output.put_line('count ' || Geometries.count);
     EXIT WHEN Geometries.COUNT = 0;


     For ij in 1..Geometries.COUNT Loop
       geometry := Geometries(ij);
       Gtype:= geometry.SDO_GTYPE;
       SRID := geometry.SDO_SRID;
-- Detect dups because most edges are ok

       Info_Array := geometry.SDO_ELEM_INFO;
       XYORD := geometry.SDO_Ordinates;

       If NOT(gtype = 2002 or gtype = 2003 or gtype = 2007) then
           NULL;
       Else


-- These parameters were tested from 0 to 72 degrees North with random coordinates
-- to check that this PROCEDURE got the same results as the Oracle function
-- SDO_UTIL.REMOVE_DUPLICATE_VERTICES. Note that the Oracle function may change the
-- coordinates!


    If SRID = 8265 THEN
      if XYOrd(2) > 50. then               -- 20 = 1./0.05
        xcheck   := 0.000032 * tolerance;  -- 20. * 0.0000016
        ycheck   := 0.00001 * tolerance;   -- 20. * 0.0000005
      else
        xcheck   := 0.000015 * tolerance;  -- 20. * 0.00000075
        ycheck   := 0.000013 * tolerance;  -- 20. * 0.00000065
      End If;
    ELSE
        oracle_tol := 0.000000000005;
        xcheck := tolerance;
        ycheck := tolerance;
    END IF;

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

     For jj in LB..UB LOOP
       ii := jj*2-1;
       xnew := XYOrd(ii);
       ynew := XYOrd(ii+1);

-- Empirical set of comparisons so we rarely calculate the difference.

       If abs(xnew - xlast) < xcheck and abs(ynew - ylast) < ycheck then

-- Possible candidate for removal
           distance := distance_fcn(xnew,ynew,xlast,ylast,SRID);

           if distance < tolerance2 then
                  geometry := SDO_GEOMETRY(2002,SRID,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),
                             SDO_ORDINATE_ARRAY(xnew,ynew,xlast,ylast));
                  distance := SDO_GEOM.SDO_LENGTH(geometry,oracle_tol,'unit=meter');
           End If;

-- drop it if within tolerance (equal or less)
          if distance <= tolerance then
            distance := round(distance,6);
            if xyord.count = 4 then
              if print <> 'NO' then
                 dbms_output.put_line('distance ' || distance || ' too short for ID ' || edge_ids(ij));
              end if;
              short_edges:= short_edges+1;
            else
              if print <> 'NO' then
                 dbms_output.put_line('distance ' || distance || ' bad at vertex ' || (jj-1) || ' for ID ' || Edge_ids(ij));
              end if;
              short_segments:= short_segments+1;
              execute immediate
              'INSERT into ' || Out_Table || ' VALUES(:1,:2,:3)' using edge_ids(ij),distance,jj;
              short_seg_edges := short_seg_edges +1;
            end if;
          else

-- keep it and store coordinates
           xlast := xnew;
           ylast := ynew;
--  dbms_output.put_line('keeping at end');
          End If;
       Else
-- usual case - wide apart.
         xlast := xnew;
         ylast := ynew;
--dbms_output.put_line('usual case ');
       End If;

    End Loop;

    END LOOP;
    END If;
    End Loop;
  END LOOP;

  dbms_output.put_line('..');
  commit;
  If short_edges <> 0 then
   dbms_output.put_line('>> WARNING: there were '|| short_edges || 'Short edges  (with nodes too close) found');
  end if;
  If short_segments <> 0 then
    dbms_output.put_line('>> WARNING: a total of ' || short_segments || ' short line segments (vertices too close) were found in ' || short_seg_edges || ' edges');
  else
    dbms_output.put_line('SUCCESSFUL : no short edges or line segments');
  end if;


END Checkfor_close_Xys;
--
FUNCTION CHECK_NEARBY(Subject_XYs         IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                        Subject_Info_Array  IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                        Subject_MBR         IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         XYs                IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         Info_Array         IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                         MBR                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
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
  -- This PL/SQL function has 6 parameters:
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
     mstart     PLS_INTEGER := 1;
     pstart     PLS_INTEGER := 1;
     pend       PLS_INTEGER;
   start_point      PLS_INTEGER;
   npoints          PLS_INTEGER;
   try_end          PLS_INTEGER;
   mpairs           PLS_INTEGER := TRUNC(XYs.count/2)-1;
   inc              PLS_INTEGER;
   istart           PLS_INTEGER :=1;
   modulus          PLS_INTEGER;
   loupes           PLS_INTEGER := 2;
   checkit          NUMBER;
   pos              PLS_INTEGER;
   no_of_mbrs       PLS_INTEGER := TRUNC(MBR.count/4)-1;
   mbr_to_do        PLS_INTEGER;
   no_to_do         PLS_INTEGER;

   touch            PLS_INTEGER := 0;
   next             PLS_INTEGER := 0;
   nxinfo           PLS_INTEGER := 1;
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
       y2 := abs(MBR(4));    -- x needs more with increasing latitude (use yUR)
       xbuffer := meters_to_degrees('X',dist_tolerance,y2);
       y1 := abs(MBR(2));    -- y needs more with decreasing latitudes  (use yLL)
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

    mbr_to_do :=MBR(6);
    no_to_do := Subject_MBR(6);

    if Subject_Info_Array.count > 3 then
        nxinfo := 4;
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



    ii := 1 - no_to_do;
    j := 1;
    no_of_mbrs := Subject_MBR(5);
--dbms_output.put_line('no of mbrs ' || no_of_mbrs);
    For i in 1..no_of_mbrs LOOP
-- Check bounding box of the "Subject" segment in question to see
        -- if the test segment overlaps

-- First MBR is the overall one and the rest start at 7;
        j := j + 6;

-- We might have a horizontal or vertical line which almost touches
        xLL := Subject_MBR(j)   - xbuffer;
        yLL := Subject_MBR(j+1) - ybuffer;
        xUR := Subject_MBR(j+2) + xbuffer;
        yUR := Subject_MBR(j+3) + ybuffer;
        try  := Subject_MBR(j+4);
        try_end := Subject_MBR(j+5);
        if try_end = (Subject_Info_Array(nxinfo) -1) then
          try_end := Subject_MBR(j+5) -4;
        end if;
  

        mpairs := MBR(5);
        if the_same then
           istart := i;
        end if;
        pos := 1 + (istart-1) * 6;
--        dbms_output.put_line('istart ' || istart || 'mpairs ' || mpairs);
        For ij in istart..mpairs Loop
           pos := pos + 6;
--           dbms_output.put_line('trying MBR '||i || ' with MBR '|| ij);
           x1 := MBR(pos);
           y1 := MBR(pos+1);
           x2 := MBR(pos+2);
           y2 := MBR(pos+3);
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

              pstart := MBR(pos+4);
              pend := MBR(pos+5);
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
              found := find_nearby(try,try_end,pstart,pend,xbuffer,ybuffer,Subject_XYs,Subject_Info_Array,Subject_MBR,XYs,Info_Array,Codes,Subject_Segs,Matches,Widths_p,Widths_q,Lengths,Xps,Yps,Xqs,Yqs,next,SRID,angle_tolerance,dist_tolerance,find,the_same);
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
                         Subject_MBR        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
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
                         dist_tolerance     NUMBER,
                         pfind              VARCHAR2 default 'B',
                         the_same           BOOLEAN default FALSE)
RETURN PLS_INTEGER AS
/*
********************************************************************************
--Program Name: find_nearby
--Author: Sidey Timmins
--Creation Date: 11/24/08

--Usage:
  -- This PL/SQL function has 6 parameters:
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
deg2rad              NUMBER   :=0.0174532925199432957692369076848861271344;
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
     overlap_length NUMBER;
     sin_angle_tolerance NUMBER;
     cos_angle_tolerance NUMBER;
     i          PLS_INTEGER;
     ii         PLS_INTEGER;
     inext      PLS_INTEGER;

     jpos       PLS_INTEGER;

     check_value1     PLS_INTEGER := 1324;  --Parallel
     check_value2     PLS_INTEGER := 1423;  --Antiarallel
     start_point      PLS_INTEGER;
     npoints          PLS_INTEGER;
     inc              PLS_INTEGER;
     inside           PLS_INTEGER;

     code             NUMBER;
     xc               NUMBER;
     yc               NUMBER;
     distance         NUMBER;

     checkit          NUMBER;
     find             VARCHAR2(1) := SUBSTR(pfind,1,1);
     same             VARCHAR2(1) := NVL(SUBSTR(pfind,2,1),'N');
     pos              PLS_INTEGER;
     pend             PLS_INTEGER := Inpend-3;
     no_of_mbrs       PLS_INTEGER;
     no_to_do         PLS_INTEGER;
     touch            PLS_INTEGER := 0;
     ok               BOOLEAN;
  BEGIN
--         parallel                  anti-parallel
--      1 +--------+ 2             1 +--------+ 2
--          3+---------+4      4+----------+3

       if find = 'B' then   -- find segments going both in same direction and opposite
          NULL;
       elsif find ='A' then   -- find just antiparallel
          check_value1 := 1423;
       elsif find = 'P' then   -- find just parallel
          check_value2 := 1324;
       end if;

       sin_angle_tolerance := GZ_UTIL_ZONE.sincos(angle_tolerance*deg2rad,cos_angle_tolerance);
       npoints := Subject_XYs.count;
       inc := 2;
       start_point := try -inc;

       If start_point < (1-inc) then  -- USED TO BE <= 1
           start_point := 1;
       End if;
       i := start_point;
--dbms_output.put_line('start ' || i || ' try_end ' || try_end || ' pstart ' || pstart );
--       dbms_output.put_line(' mstart ' || mstart || ' XYs ' || Xys.count);

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
--  if ii >= 791  and jpos = 803 then
--  dbms_output.put_line('>>>>>>>>>>>>>>>>>>>testing ' || ii || ' pos ' ||pos);
--  end if;
-- Check to see if the line is semiparallel since one end of the line is within the window
           checkit := Line_parallel(x1,y1,x2,y2,x3,y3,x4,y4,
                      xp,yp,xq,yq,distance_p,distance_q,SRID,cos_angle_tolerance,dist_tolerance);
-- if ii >= 181  and jpos = 183 then
--  dbms_output.put_line('>>>>checkit ' || checkit);
--  end if;

--    if ii =85  and jpos = 93 then
--  dbms_output.put_line('Checkit ' || checkit || check_value1||check_value2||' pos ' || pos || ' ii ' || ii || ' D ' || distance || ' CV ' || check_value1 || ' pfind ' || pfind);
--  dbms_output.put_line('x1 ' || ROUND(x1,7) || ' y1 ' || ROUND(y1,7));
--        dbms_output.put_line('x2 ' || ROUND(x2,7) || ' y2 ' || ROUND(y2,7));
 
--  dbms_output.put_line('x3 ' || ROUND(x3,7) || ' , ' || ROUND(y3,7));
--        dbms_output.put_line('x4 ' || ROUND(x4,7) || ' , ' || ROUND(y4,7));
--      dbms_output.put_line('xp ' || ROUND(xp,7) || ' yp ' || ROUND(yp,7));
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
                elsif code = 34 or code= 43 then
                  overlap_length := Distance_fcn(x3,y3,x4,y4,SRID);
                elsif code = 14 then
                  overlap_length := Distance_fcn(x1,y1,xq,yq,SRID);
                elsif code = 32 then
                  overlap_length := Distance_fcn(xp,yp,x2,y2,SRID);
                elsif code = 42 then
                  overlap_length := Distance_fcn(xp,yp,x2,y2,SRID);
                elsif code= 13 then
                  overlap_length := Distance_fcn(x1,y1,xq,yq,SRID);
                else
                  overlap_length := 0.0;
--                   dbms_output.put_line('>>>for ii ' || ii || ' jpos ' || jpos || ' CODE was unexpected ' || code  || ' checkit ' || checkit);
                end if;
--                dbms_output.put_line('>>>for ii ' || ii || ' jpos ' || jpos || ' CODE was ' || code || ' OL ' || round(overlap_length,3));
                if SUBSTR(pfind,2,1) = 'P' or SUBSTR(pfind,2,1) = 'E' then
 
                  if projector = 1 then
                    x := x1; y := y1;
                  elsif projector = 2 then
                    x := x2; y := y2;
                  elsif projector = 3 then
                    x := x3; y := y3;
                  elsif projector = 4 then
                    x := x4; y := y4;
                  end if;

                  if TRUNC(checkit) = 1423 and (projector = 1 or projector = 4) then
                    xc := (x + xp)*0.5;
                    yc := (y + yp)*0.5;
                  else
                    xc := (x + xq)*0.5;
                    yc := (y + yq)*0.5;
                  end if;
                  inside := POINT_IN_POLY_CC(xc,yc,Subject_xys);
                   if (SUBSTR(pfind,2,1) = 'P' and inside = 1 ) or
                      (SUBSTR(pfind,2,1) = 'E' and inside = 0 ) then
                      ok := TRUE;
--                      dbms_output.put_line('OK is TRUe' || round(xc,7) || ' yc ' || round(yc,7));
--                        dbms_output.put_line('ok is TRUe' || round(xi,7) || ' yi ' || round(yi,7));
--                         dbms_output.put_line('ok is TRUe' || round(xalt,7) || ' ya ' || round(yalt,7));
                   else
                      ok := FALSE;
--                       dbms_output.put_line('OK is faalse' || round(xc,7) || ' yc ' || round(yc,7));
--                        dbms_output.put_line('ok is faalse' || round(xi,7) || ' yi ' || round(yi,7));
--                         dbms_output.put_line('ok is faalse' || round(xalt,7) || ' ya ' || round(yalt,7));
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
Function Distance_Fcn(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER IS

   deg2rad         NUMBER   :=0.0174532925199432957692369076848861271344;
   dx              NUMBER;
   dy              NUMBER;
   y               NUMBER := ABS(y1);
   yrad            NUMBER;
   ysin            NUMBER;
   ycos            NUMBER;
   ycos2           NUMBER;
   ysin2           NUMBER;
   cosdy           NUMBER;
   sinysq          NUMBER;
   cos3y           NUMBER;
   cos6y           NUMBER;
--                 Coefficients to calculate the y factor
   a0        CONSTANT NUMBER := 0.993305622509018;
   a1              NUMBER :=  0.99330564310853556;
   a2              NUMBER :=  0.0099739720896542635;
   a3              NUMBER :=  0.0000844513348186908111;
   a4              NUMBER := -0.0000000207345260587865496;
   delta           NUMBER;
   sindelta        NUMBER;
   yfactor         NUMBER;  --   0.9933;
   xfactor         NUMBER;  --  1.0
   dist_in_meters  NUMBER;
   dist_factor     NUMBER := 111319.490793274;

Begin

    dx := x2-x1;
    dy := y2-y1;
  
    -- Pythagoras distance for 2-D projections
  
    If SRID is NULL or SRID <> 8265.0 Then
       dist_in_meters := sqrt(dx*dx+ dy*dy);
  
    -- Vincenty's formula is used for "large" angles (large means > 0.1 degrees here)
  
    Elsif (abs(dx) > 0.1 or abs(dy) > 0.1) then
      dist_in_meters := GZ_SUPER.fast_vincenty_gcd(x1,y1,x2,y2);
      
    -- Use a remarkable approximation
    Else

    -- This code is derived from Accurate_GCD and has been tested against
    -- Charles Karney'code in the geodesic package. The results are amazing.
    -- For lat/long differences < 0.1 degrees, the error is < 0.01 meters.
    -- For differences < 0.02 degrees, the distance error is < 0.0004 meters.
    -- Provided lat/long differences are < 0.1 degrees, the relative error is
    -- less than 1 part in 1 million.

          ysin := quick_sincos(y1*deg2rad,ycos);
          delta := (y2-y1)*0.5*deg2rad;           -- calculate sin((y1+y2)*0.5)
          ycos2 := ycos - ysin * delta*2.;        -- small angle approximation for cos(y2)
          cosdy := 1. - delta*delta*0.5;          -- using sin(a+eps) = sin(a)cos(eps) + cos(a)sin(eps)
          sindelta := delta - delta*delta*delta/6.; -- make sin of delta using Taylor
          ysin2 := ysin*cosdy + ycos*sindelta;    -- small angle approximation for formula above
          delta := dy*deg2rad;
          ycos2 := ycos - ysin * delta;           -- small angle approximation for cos(y2)
          ycos2 := ycos*ycos2;

-- this code is just to handle user error when y > 90
          if ycos2 < 0. then
            ysin2 := quick_sincos(y2*deg2rad,ycos2);
          end if;
--------------------------------------------------------------------------------
--        NEW The 4 a coefficents replace all of the code in fast_distance for a1,a2
          sinysq := ysin2*ysin2;
--          cos3y := ycos*(1.-4.*sinysq);
--          cos6y := 2.*cos3y*cos3y -1.;
--          dy := dy * (a1+ a2*sinysq + a3*sinysq*sinysq + a4*cos6y);
          
   if y < 24. then
      a1 := 9.75337212373260e-003;
      a2 := 2.21455186315726e-004;
      a3 := 0.0;
   elsif y < 49. then
      a1 := 9.89284559059283e-003;
      a2 := 1.12348547934459e-004;
      a3 := -9.79441140263943e-006;
   elsif y < 71. then
      a1 := 1.00701149393983e-02;
      a2 := 4.56418031074841e-05;
      a3 :=-7.12726995829831e-05;
   else
      a1 := 1.01352148520020e-02;
      a2 := 3.94447272940481e-05;
      a3 := -1.10363060190724e-04;
   end if;
   yrad := y*deg2rad;
   dy := dy*(a0 + a1*sinysq + yrad*yrad*a2 + yrad*a3);           
--------------------------------------------------------------------------------
--          yfactor := 0.9933 + 0.010025*ysin2*ysin2;
--          dy := (y2-y1)*yfactor;
--          xfactor := (1. + 0.00335*ysin*ysin);

          xfactor := 0.00335;
          if abs(y1) > 25. then
             xfactor := xfactor + 2.8E-07*(abs(y1)-25.);
          else
             xfactor := xfactor - 1.5E-07*(25.-abs(y1));
          end if;
          
--          if abs(x2-x1) > 0.04 then
--               xfactor := xfactor + (y2-y1)/abs(x2-x1) * 7.E-06;
--          end if;
          xfactor := (1. + xfactor*ysin*ysin);
          dx := dx*xfactor;
          dist_in_meters := sqrt(dx*dx*ycos2 + dy*dy) * dist_factor;

    End if;
  
    Return dist_in_meters;

End;
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
                              perp_angle_tolerance NUMBER default 0.707107)
            RETURN NUMBER Deterministic IS
/*
********************************************************************************
--Program Name: line_parallel
--Author: Sidey Timmins
--Creation Date: 2/13/2009 from line intersect to find demi-parallel lines
--Updated: 12/02/2008 To better handle parallel lines and report differently
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
  --      cos_angle_tolerance - the cosine of the maximum angle between the lines:
  --                            for example for 30 degrees, cos = .8660254
  --                   Lines with a larger angle are not considered. When the
  --                   caller calls line_parallel thousands of times, it is
  --                   obviously wasteful to keep figuring out the cos(angle_tolerance).
  --      OUTPUT
  --      xni,yni      -- the nearest point if the lines are demi-parallel or
  --                      the intersection point (if the lines are not parallel)
  --      xalt,yalt    -- the far point
  --      distance        the distance between parallel (or near_parallel) lines
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
  twopi     NUMBER             := 6.2831853071795864769252867665590057684;
   deg2rad              NUMBER   :=0.0174532925199432957692369076848861271344;
   rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
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
 
   tt       NUMBER;
   X21      NUMBER := X2-X1;
   Y21      NUMBER := Y2-Y1;
   X43      NUMBER := X4-X3;
   Y43      NUMBER := Y4-Y3;
   X31      NUMBER := X3-X1;
   Y31      NUMBER := Y3-Y1;
   X32      NUMBER := X3-X2;
   Y32      NUMBER := Y3-Y2;
   X41      NUMBER := X4-X1;
   Y41      NUMBER := Y4-Y1;
   xii      NUMBER;
   yii      NUMBER;
   x        NUMBER;
   y        NUMBER;
   len_across NUMBER;

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
--   dotp2    NUMBER;
   dxp      NUMBER;
   dyp      NUMBER;

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
   ok              BOOLEAN;
   result         NUMBER := 0.;

 

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
--dbms_output.put_line('cos theta ' || round(costheta,6) || ' cosat ' || round(cos_angle_tolerance,6));
--   if costheta > 1.0 then
--      costheta := 1.0;
--   end if;
--   sintheta := sqrt(1.-costheta*costheta);


-- Check the angle the two lines make with each other.
-- Cosine(zero) is 1.0 so any angle with a cosine greater than the tolerance
-- is a smaller angle.
--if costheta > 0.866025035 then
--dbms_output.put_line('B1 is :' ||round(bearing1*rad2deg,7) || ' B2 ' || round(bearing2*rad2deg,7));
--dbms_output.put_line('costheta is :' ||round(costheta,7) || ' angle2 ' || round(angle2,7) || 'angle ' || round((acos(costheta)* rad2deg),4) || ' cosat ' || ROUND(cos_angle_tolerance,5));
--   dbms_output.put_line('x1 ' || round(x1,7) || ' y1 ' || round(y1,7) );
--   dbms_output.put_line('x2 ' || round(x2,7) || ' y2 ' || round(y2,7) );
--   dbms_output.put_line('x3 ' || round(x3,7) || ' y3 ' || round(y3,7) );
--   dbms_output.put_line('x4 ' || round(x4,7) || ' y4 ' || round(y4,7) );

--end if;
   IF costheta >= cos_angle_tolerance  THEN

      denom := X21 * X21 + Y21 * Y21;  -- square of length: line segment 1 
--------------------------------------------------------------------------------
--          dbms_output.put_line('projecting x3 onto x1,y1 to x2,y2');
      tt := (X21*X31 + Y21*Y31)/denom;
      xii := X1 + tt * X21;
      yii := Y1 + tt * Y21;
      ok := FALSE;
-- dbms_output.put_line('at 3'|| tt);
      if tt >= 0. and tt <= 1.0 then
         ok := TRUE;
      elsif perp_angle_tolerance > 0.0 then
         dxp := x3 - xii;
         dyp := y3 - yii;
         lenp := sqrt(dxp*dxp+dyp*dyp);

         if dotp > 0. then
            x := x1;
            y := y1;
         else
            x := x2;
            y := y2;
         end if;
         dxp := x3 - x;
         dyp := y3 - y;
         len_across := sqrt(dxp*dxp+dyp*dyp);
         dot_perp := abs(X21*dxp + Y21*dyp) /(len1*len_across);
--         dbms_output.put_line('dot_perp ' ||dot_perp|| ',' || perp_angle_tolerance); 
         if dot_perp < perp_angle_tolerance then
            ok := TRUE;
            xii := x; yii := y;
         end if;
           
      end if;
      if ok then
          
--   dbms_output.put_line('at 3'|| tt);
--          dbms_output.put_line('xii ' || round(xii,7) || ',' || round(yii,7));
--          dbms_output.put_line('x2 ' || round(x2,7) || ',' || round(y2,7));
--          dbms_output.put_line('x3 ' || round(x3,7) || ',' || round(y3,7));
--          dbms_output.put_line('xni ' || round(xni,7) || ',' || round(yni,7));
          distance1 := Distance_fcn(xii,yii,x3,y3,SRID);


-- We want to build the following projector codes going in the direction of line 1 -> 2: 
-- Lines in Same direction: 12,34,14 or 32
-- in opposite direction: 12,43,42,13
-- Use component parts of 10,30, 2 and 4 for same direction
-- and 10,40,2,3 in opposite directions

          if dotp > 0. then      -- same direction
            xp := xii; yp := yii;
            distance_p := distance1;
            projector := 30.;
          else                   -- opposite, 
            xq := xii; yq := yii;
            distance_q := distance1;
            projector := 3.;
          end if;

          if tt = 1. then result := 23.; end if;
          if tt = 0. then result := 13.; end if;

      end if;
--------------------------------------------------------------------------------
--            dbms_output.put_line('projecting x1 onto x3,y3 to x4,y4');
      tt := -(X31*X43 + Y31*Y43)/denom2;

      xii := X3 + tt * X43;
      yii := Y3 + tt * Y43;
 --     dbms_output.put_line('tt ' || tt || ' xii ' || xii || ' yii ' || yii);
      ok := FALSE;

      if tt >= 0. and tt <= 1.0 then
         ok := TRUE;
      elsif perp_angle_tolerance > 0.0 then
         dxp := x1 - xii;
         dyp := y1 - yii;
         lenp := sqrt(dxp*dxp+dyp*dyp);

         if dotp > 0. then
            x := x3;
            y := y3;
         else
            x := x4;
            y := y4;
         end if;
         dxp := x1 - x;
         dyp := y1 - y;
         len_across := sqrt(dxp*dxp+dyp*dyp);
         dot_perp := abs(X43*dxp + Y43*dyp) /(len2*len_across);
--         dbms_output.put_line('dot_perp ' ||dot_perp|| ',' || perp_angle_tolerance);
         if dot_perp < perp_angle_tolerance then
            ok := TRUE;
            xii := x; yii := y;
         end if;         
 
      end if;
      if ok then
          
          distance2 := Distance_fcn(xii,yii,x1,y1,SRID);
--          dbms_output.put_line('Distance2 ' ||round(distance2,8) || ',' || round(distance_p,8));
          if dotp > 0. then   -- they go in the same direction
             if distance2 < distance_p then
             xp := xii; yp := yii;
             distance_p := Distance2;
             projector := 10.;  -- this replace 30 above if any
             end if;
          else
             xp := xii; yp := yii;
             distance_p := Distance2;
             projector := 10.+ projector;  -- this builds 13 if 3 projected, else 10
          end if;
--          dbms_output.put_line('Projector is ' || projector);
          
          if  distance2 < distance1 then
             distance1 := distance2;
             
             if tt = 1. then result := 14.; end if;
             if tt = 0. then result := 13.; end if;
          end if;
      end if;


--      dotp2 := (X43*X32 +Y43*Y32);
--------------------------------------------------------------------------------
--          dbms_output.put_line('projecting x4 onto x1,y1 to x2,y2');
      tt := (X21*X41 + Y21*Y41)/denom;
      
      xii := X1 + tt * X21;
      yii := Y1 + tt * Y21;
      ok := FALSE;
--      dbms_output.put_line('TT ' || tt );
      if tt >= 0. and tt <= 1.0 then
         ok := TRUE;
      elsif perp_angle_tolerance > 0.0 then
         dxp := x4 - xii;
         dyp := y4 - yii;
         lenp := sqrt(dxp*dxp+dyp*dyp);
         
         if dotp > 0. then
            x := x2;
            y := y2;
         else
            x := x1;
            y := y1;
         end if;
         dxp := x4 - x;
         dyp := y4 - y;
         len_across := sqrt(dxp*dxp+dyp*dyp);

         dot_perp := abs(X21*dxp + Y21*dyp) /(len1*len_across);
--          dbms_output.put_line('dot_perp ' ||dot_perp|| ',' || perp_angle_tolerance);
         if dot_perp < perp_angle_tolerance then
            ok := TRUE;
            xii := x; yii := y;
         end if;

      end if;
      
      if ok then
            
          distance2 := Distance_fcn(xii,yii,x4,y4,SRID);
          
          if dotp > 0. then
             xq := xii; yq := yii;
             distance_q := distance2;
             projector := projector + 4.;  -- this builds 14 or 34
--              dbms_output.put_line('CHanged projector ' || projector);
          else
--          dbms_output.put_line('distance2 ' ||round(distance2,8) || ',' || round(distance_p,8));
             if distance2 < distance_p then
             xp := xii; yp := yii;
             distance_p := distance2;     -- this makes 43 or 40
             projector := 40. + projector - TRUNC(projector*0.1)*10.;
--             dbms_output.put_line('changed projector ' || projector);
             end if;
          end if;
          
       
          if distance2 < distance1 then
            distance1 := distance2;
            if tt = 1. then result := 24.; end if;
            if tt = 0. then result := 14.; end if;
          end if;
      end if;

--------------------------------------------------------------------------------
--             dbms_output.put_line('projecting x2 onto x3,y3 to x4,y4');
      tt := -(X32*X43 + Y32*Y43)/denom2;
      xii := X3 + tt * X43;
      yii := Y3 + tt * Y43;
      ok := FALSE;
-- dbms_output.put_line('at 2 '|| tt);
      if tt >= 0. and tt <= 1.0 then
         ok := TRUE;
      elsif perp_angle_tolerance > 0.0 then
         dxp := x2 - xii;
         dyp := y2 - yii;
         lenp := sqrt(dxp*dxp+dyp*dyp);
        
         if dotp > 0. then
            x := x4;
            y := y4;
         else
            x := x3;
            y := y3;
         end if;
         dxp := x2 - x;
         dyp := y2 - y;
         len_across := sqrt(dxp*dxp+dyp*dyp);
         dot_perp := abs(X43*dxp + Y43*dyp) /(len2*len_across);
--  dbms_output.put_line('dot_perp '|| round(dot_perp,8));       
         if dot_perp < perp_angle_tolerance then
            ok := TRUE;
            xii := x; yii := y;
         end if;
                     
      end if;
      if ok then
 

          distance2 := Distance_fcn(xii,yii,x2,y2,SRID);
          
          if dotp > 0. then
             if distance2 < distance_q then
             xq := xii; yq := yii;
             distance_q := distance2;
             projector := 2.+TRUNC(projector*0.1)*10.;  -- this makes 12 or 32
             end if;
          else
             if distance2 < distance_q then
             xq := xii; yq := yii;
             distance_q := distance2;
             projector := 2.+TRUNC(projector*0.1)*10.;  -- this makes 12 or 42
             end if;
          end if;
 
--   dbms_output.put_line('projector 2 ' || projector);
--dbms_output.put_line('dD' || distance2 );
          if distance2 < distance1 then

--            dbms_output.put_line('at 2');
            distance1 := distance2;
 
            if tt = 1. then result := 24.; end if;
            if tt = 0. then result := 23.; end if;
 
          end if;
      end if;

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
/*
if distance < dist_tolerance then
      geometry := SDO_GEOMETRY(2002, 8265, NULL,SDO_ELEM_INFO_ARRAY(1, 2, 1), SDO_ORDINATE_ARRAY(x1,y1,x2,y2));
      geometry2 :=  SDO_GEOMETRY(2002, 8265, NULL,SDO_ELEM_INFO_ARRAY(1, 2, 1), SDO_ORDINATE_ARRAY(x3,y3,x4,y4));
--        EXECUTE IMMEDIATE  'SELECT SDO_GEOM.RELATE(:1,''determine'',:2,0.05) from dual'
--                      into relationship using geometry,geometry2;
-- EXECUTE IMMEDIATE  'SELECT SDO_GEOM.SDO_LENGTH(:1,0.000000005,''unit=meter'') from dual' into len1 using geometry;
-- EXECUTE IMMEDIATE  'SELECT SDO_GEOM.SDO_LENGTH(:1,0.000000005,''unit=meter'') from dual' into len2 using geometry2;

      EXECUTE IMMEDIATE  'SELECT SDO_GEOM.SDO_DISTANCE(:1,:2,0.000000005,''unit=meter'') from dual' into odist using geometry,geometry2;
dbms_output.put_line('Odist ' || odist || ' len1 ' || distance1 || ' len2 ' || distance2);
end if;
*/
-- To measure the distance between the lines, we check to see if the far end is
-- also not too far away.
--dbms_output.put_line('DOTP ' || dotp);
--        if dot_perp = 1. then
--          xp := x2; yp:= y2; xq := x3; yq := y3;
--          distance_p := 0.0;
--          distance_q := 0.0;
--        elsif dot_perp = -1. then
--          xp := x2; yp:= y2; xq := x4; yq := y4;
--           distance_p := 0.0;
--          distance_q := 0.0;
        if distance1 > dist_tolerance then
          result := 0.;
        end if;

      if (result >= 1324. and distance1 < dist_tolerance ) or result > 0. then
--      if (result >= 1324. and distance < dist_tolerance and (approx_length*sintheta + distance) < dist_tolerance) or result > 0. then
         RETURN result;
      else
         RETURN 0.;
      end If;

   END IF;
   IF det <> 0.0 THEN
      s := (X43*Y31 - Y43*X31)/det; --((X4 - X3) * (Y3 - Y1) - (Y4 - Y3) * (X3 - X1)) /det;
      t := (X21*Y31 - Y21*X31)/det; --((X2 - X1) * (Y3 - Y1) - (Y2 - Y1) * (X3 - X1)) /det;
--        dbms_output.put_line('SS is :' || s || ' T ' || t);
      If s >= 0 and s <= 1.0 and t >= 0.0 and t <= 1.0 then

        xp := X1 + s * X21;
        yp := Y1 + s * Y21;

        If (t < tolerance2) or (t > 1.0 - tolerance2) then
-- The lines touch
           RETURN -1.;
        End if;
--dbms_output.put_line('the lines intersect' || xi || ' ' || yi);
        RETURN 1.;
      Else
        RETURN -approx_distance;
      End if;


   END IF;
-- Lines are parallel (or coincident)
   RETURN -approx_distance;


END LINE_PARALLEL;
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
  --      (x1,y1)     - Coordinates for one end of the line to test.            
  --      (x2,y2)     - Coordinates for other end of the line to test.
  --      OUTPUT
  --      (xnear,ynear): Calculated nearest point on line.
  --          Returns the perpendicular distance in meters when the point
  --          (xin,yin) projects onto the given line or 1.E10 when it does not.
-- Purpose:  Find whether a point projects onto aline, and if so, the distance.  
                          
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
             Distance := fast_distance(xin,yin,xnear,ynear,8265);
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
               Distance := fast_distance(xin,yin,x,y,8265);
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

  rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
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
--           |    |                           Extent: (2) on a
--      -------<--------             
--             b
--
--                  a         Case 2) = 43.   b3 and b4 project
-- 2)      --------------------              neither a1 or a2 project but both b's do
--               |    |                       Extent: projection of (3) onto a
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
   cpleft        pls_integer;

   side1         PLS_INTEGER;
   next1         PLS_INTEGER := side1_vertex;
   side2         PLS_INTEGER ;
   next2         PLS_INTEGER := side2_vertex; 
   next_side1    PLS_INTEGER;
   next_side2    PLS_INTEGER;
   pick          PLS_INTEGER;
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
--     dbms_output.put_line('Success at ' || (side_1) || ' with ' || side_2 || ' CASE: ' || case_is || ' dc ' || round(dist_close,3) || ' dist_f ' || round(dist_far,3));

 
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
       
-- For the Subject Segments (SS) and the matches (M) note the way the 
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

-- check side_1  <> side_2
         if (((angle_found < success and (dist_close < cutoff or dist_far < cutoff)) or ((dist_close < cutoff and dist_far < cutoff) and angle_found < 90.)) AND --(180.-success))) AND
                side_1 >= Subject_segs(cp) and side_2 <= Matches(cp) and ABS(side_1 - side_2) > 1) and case_is > 4 then
             
--                 dbms_output.put_line('success at cp ' || (cp-1) || ' ,' ||(side_1) || ' with ' || (side_2) || ' ANGLE ' || round(results(1),3)|| ' Cases: ' || case_is);

--          we don't want a with b and then b with a

            exit when side_1 = Matches(cplast) and side_2 = Subject_segs(cplast);
            cp := cp-1;
              
            Cases(cp) := case_is;         
            Scores(cp) := score;
            Angles_apart(cp) := angle_apart;
--            dbms_output.put_line('cpx ' || current_posx || ' cpy ' || current_posy);
            
-- We indicated which segment is a projected result with a trailing 0.1.
-- This means we don't want the beginning of the segment, we want a point
-- projected onto from the other segment

           
          if case_is = 12 or case_is = 42 then
              Subject_segs(cp) := side_1;                   
              Matches(cp) := side_2;
              Widths(cp) := results(3);
              Distances(cp) := results(5);
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
          results := trip(Xys,start_of_ring1,end_of_ring1,start_of_ring2,end_of_ring2,
                          side_1,inc1,side_2,inc2,current_posx,current_posy,cutoff,
                          case_is,score, angle_apart, width, dist_found,
                          x1,y1,x2,y2,x3,y3,x4,y4,
                       xpa,ypa,xqa,yqa,xpb,ypb,xqb,yqb);
        
         angle_found  := results(1);
         dist_close := results(3);
         dist_far   := results(4);
         IF case_is > 4 THEN
--         dbms_output.put_line('II ' || ii || ' case ' || case_is || ' trying side ' ||(side_1) || ' at side2 ' || (side_2) || ' RES1 ' || round(results(1),4) || ' rees ' || round(results(3),4) || ' Res ' || round(results(4),4) || ' rr ' || round(results(5),4) || ' r  ' || round(results(6),4));
          if (((angle_found < success and (dist_close < cutoff or dist_far < cutoff)) or ((dist_close < cutoff and dist_far < cutoff) and angle_found < 90.)) AND --(180.-success))) AND
              side_1 <= Subject_segs(cp) and side_2 >= Matches(cp) and ABS(side_1 - side_2) > 1)  and case_is > 4 then

--          we don't want a with b and then b with a

            exit when side_1 = Matches(cplast) and side_2 = Subject_segs(cplast);
            
--              dbms_output.put_line('Success at cp ' || (cp+1) || ' ,' ||(side_1) || ' with ' || (side_2) || ' ANGLE ' || round(results(1),3) || ' case: ' || case_is);
--               dbms_output.put_line('close ' ||round(dist_close,3) ||' far ' || round(dist_far,3) || ' cutoff ' || cutoff);
            cp := cp+1;
           
            Cases(cp) := case_is;
            Scores(cp) := score;
            Angles_apart(cp) := angle_apart;



            if case_is = 12 or case_is = 13 then
               Subject_segs(cp) := side_1;                
               Matches(cp) := side_2;    
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
Function Get_pipe_Geom(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Info MDSYS.SDO_ELEM_INFO_ARRAY,start_vertex PLS_INTEGER,across_vertex PLS_INTEGER,
                        
                        which PLS_INTEGER,cutoff NUMBER,PSL IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,average_width IN OUT NOCOPY NUMBER, accum_length IN OUT NOCOPY NUMBER ) RETURN MDSYS.SDO_GEOMETRY AS

-- Traverse amd Measure the widths of a pipe starting from the start and locating
-- the end.

-- The Processed segment list, contains either 1 range (a pair of sgement numbers) or
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
       Widths := Navigate(Xys,sv,av,which,start1,end1,start2,end2,cutoff,
                           Subject_segs,Matches,Scores,Angles_apart,Cases,Distances,Xes,Yes,Xps,Yps,Xfs,Yfs,Xos,Yos);

    n := Subject_Segs.count;
--dbms_output.put_line('NN ' || n);
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
--   dbms_output.put_line('xys ' || xys.count || ' SS ' || subject_segs(n)|| ' M ' || matches(n));

   if Subject_segs(n) = Matches(n)+2 or Subject_segs(1) = Matches(1)+3 or Subject_segs(1) = Matches(1)+4 then   
     ibeg := Matches(1)*2-1;
     istop := Subject_segs(1)*2+2;
 
     PSL(1) := TRUNC((ibeg+1)/2);
     PSL(2) := TRUNC(istop/2)-1;
--    dbms_output.put_line('ibeg ' || ibeg || ' istop ' || istop|| 'K ' || kount);
    
     copy_xys(ibeg,istop);
--dbms_output.put_line('nedt ' || next || ' new ' || new_xys.count);

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
--    dbms_output.put_line('ibeg ' || ibeg || ' istop ' || istop|| 'K ' || kount);

    copy_xys(ibeg,istop);
--dbms_output.put_line('nedt ' || next || ' new ' || new_xys.count);
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
--   dbms_output.put_line('2 ended pipe ' || n || ' case ' ||cases(n)|| ' SS ' || subject_segs(1));
   ibeg := Subject_segs(n)*2-1;
   istop := Subject_segs(1)*2+2;

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
--     dbms_output.put_line('hhhere ' || ibeg || ' istop ' || istop );
     copy_xys(ibeg,istop); 
 
-- Replace     
      if Cases(n) = 12 or Cases(n) = 13 then
--    dbms_output.put_line('hhHere');
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
-- function of latitude.

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
-- check now
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
procedure sort_also(Arr in out nocopy Mdsys.sdo_list_type,Order_array in out nocopy mdsys.sdo_list_type) as
      temp   Mdsys.sdo_list_type := Mdsys.sdo_list_type();
  begin
      temp.extend(Arr.count);
      for ii in 1..Arr.count loop
        temp(ii) := Arr(ii);
      end loop;
      
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
  -- throughout the range [-2pi to 2pi]
  c1        NUMBER             := -34287.4662577359568109624;
  c2        NUMBER             := 2566.7175462315050423295;
  c3        NUMBER             := -26.5366371951731325438;
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
Function QUICK_SINCOS(Xin NUMBER,cosX IN OUT NOCOPY NUMBER,in_degrees VARCHAR2 default 'N') RETURN NUMBER Deterministic IS
/*
********************************************************************************
--Program Name: quick_sincos
--Author: Sidey Timmins using Robin Green's source coefficients.
--Creation Date: 04/18/11
--Updated: 04/26/11 Newer coefficients from his paper - cannot find original ones!
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
--         accurately enough for most GIS purposes (< 1 millionth of a degree).
--         Minmax absolute error is <= 2.5E-9.
--         Maximum relative error is 3.8E-9 (1 - quick_sin/sine);
--         More accurate than Jack Ganssle's five cosine coefficients (from Hart)
--         About 5 times faster than combined sin and cos calls.
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
--  Some earlier results below
--  0   0            0            0
-- .1  .0998334166   .0998334166  0
-- .2  .1986693305   .1986693308  .0000000003
-- .3  .2955202054   .2955202067  .0000000013
-- .4  .3894183393   .3894183423  .0000000031
-- .5  .4794255346   .4794255386  .0000000040
-- .6  .5646424728   .5646424734  .0000000006
-- .7  .6442176928   .6442176872  -.0000000056
-- .8  .7173560921   .7173560909  -.0000000012
-- .9  .7833269063   .7833269096  .0000000034
-- 1.0  .8414709862  .8414709848  -.0000000014
-- 1.1  .8912073621  .8912073601  -.0000000020
-- 1.2  .9320390869  .932039086   -.0000000010
-- 1.3  .9635581857  .9635581854  -.0000000002
-- 1.4  .98544973    .98544973    0
-- Called by:
-- Dependencies: none
********************************************************************************
*/
   deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
   twopi     CONSTANT NUMBER := 6.2831853071795864769252867665590057684;
   pi        CONSTANT NUMBER := 3.1415926535897932384626433832795028842;
   piby2     CONSTANT NUMBER := 1.5707963267948966192313216916397514421;
   piby4     CONSTANT NUMBER := 0.78539816339744830961566084581987572105;
   pi3by2    CONSTANT NUMBER := 4.7123889803846898576939650749192543263;
-- One has to be careful with Robin Green's pdf. There must be 2 versions
-- because the left set of coefficients is better than the right. The test
-- x>pi/4 is also critical since these coefficients are only for -1 ..0..1
--       From Faster Math Functions pdf        From Blue Slide pdf
   s1        NUMBER := -0.166666546;  --      -0.166666686534881591796875;
   s2        NUMBER := 0.00833216076; --       0.00833282412;
   s3        NUMBER := -0.000195152832; --    -0.000195878418;
   x         NUMBER := Abs(Xin);
   xx        NUMBER;
   x2        NUMBER;
   sinx      NUMBER;
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

-- Calculate an accurate approximation to the sin: max error of 5.7E-9 radians
   x2 := x*x;
   sinx := x*(1.0 + x2*(s1 + x2*(s2 + s3*x2)));
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
--==============================================================================
-- A set of procedures and functions to histogram a column from a table
-- and draw a graph that can be viewed with MapViewer.
--==============================================================================
PROCEDURE HISTOGRAM_TABLE(pInTable VARCHAR2, pInColumn VARCHAR2,
                      InBottom NUMBER default NULL,
                      InTop NUMBER default NULL,
                      cells  NUMBER default 10,Graph_Table VARCHAR2 default 'HTABLE',mark number default 2.) AS

    Hgram_Array        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Range_Array        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Length_Array       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    sql_stmt           VARCHAR2(4000);
    average            NUMBER;
    st_dev             NUMBER;
    marker             NUMBER;

BEGIN

   Hgram_Array  := HISTOGRAM(pinTable, pInColumn, Length_Array,Range_Array,InBottom,Intop,cells);
--   Range_Array  := CREATE_EQUAl_RANGES(pinTable, pInColumn, Hgram_Array,InBottom,Intop,cells);
--  for ii in 1..Hgram_Array.count Loop
--     dbms_output.put_line('range : ' || Range_Array(ii) || ' to ' || Range_Array(ii+1) ||
--     ' Count was : ' || Hgram_Array(ii) );
--  End Loop;
   sql_stmt := 'SELECT AVG('|| pInColumn || '),STDDEV(' || pInColumn || ') FROM ' || pInTable;
   EXECUTE IMMEDIATE sql_stmt into average,st_dev;

-- Mark the 2nd standard deviation
   dbms_output.put_line('AVERAGE ' || round(average,4) || ' standard-dev ' || round(st_dev,4));
   marker := ROUND(mark *st_dev + average,4);
   Draw_Graph(Graph_Table,'HIST',pInTable,pInColumn,Range_array,Hgram_Array,marker);
   sql_stmt := 'GRANT SELECT ON ' || Graph_Table ||' TO PUBLIC';

   EXECUTE IMMEDIATE sql_stmt;


END HISTOGRAM_TABLE;
--
FUNCTION HISTOGRAM(pInTable VARCHAR2, pInColumn VARCHAR2,
                      Length_Array IN OUT NOCOPY  MDSYS.SDO_LIST_TYPE,
                      Range_Array IN OUT NOCOPY  MDSYS.SDO_LIST_TYPE,
                      InBottom NUMBER default NULL,
                      InTop NUMBER default NULL,
                      cells NUMBER default 10)
RETURN MDSYS.SDO_LIST_TYPE AS

   InTable            VARCHAR2(100) := TRIM(UPPER(pInTable));
   InColumn           VARCHAR2(100) := TRIM(UPPER(pInColumn));

   Hgram_Array        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Xbot               NUMBER;
   Xtop               NUMBER;
   XMed               NUMBER :=0.;
   sql_stmt           VARCHAR2(4000);
   loops              PLS_INTEGER;
   kount              NUMBER;
   len                NUMBER;
   current_bottom     NUMBER;
   current_top        NUMBER;
   counter            PLS_INTEGER := 0;
   cell_size1         NUMBER :=0.;
   cell_size          NUMBER :=0.;
   Less_than          VARCHAR2(3) :=' < ';
   the_total          NUMBER;
   total_so_far       NUMBER := 0.;
BEGIN

   Length_Array := MDSYS.SDO_LIST_TYPE();
   Range_Array := MDSYS.SDO_LIST_TYPE();

    If InBottom is  NULL or Intop  is NULL then
     sql_stmt := 'SELECT MIN('|| InColumn || '),MAX('|| InColumn ||
               '),MEDIAN(' || InColumn || '),COUNT(1) FROM ' || InTable;
     EXECUTE IMMEDIATE sql_stmt into Xbot,Xtop,XMed,the_total;
    ELSE
          sql_stmt := 'SELECT COUNT(1) FROM ' || InTable || ' WHERE ' ||
           InColumn || ' >=:1 AND ' || InColumn || ' <=:2';
     EXECUTE IMMEDIATE sql_stmt into the_total using InBottom,Intop;
    dbms_output.put_line('THE totla ' || the_total);
   End If;
-- Typically the data is log normally distributed and the top minus median
-- is a better estimate of the cell size

   If InBottom is not NULL then
     Xbot := InBottom;
   End If;
   If InTop is not NULL then
     Xtop := Intop;
   End If;


     If Xmed <> 0. then
     cell_size1 := TRUNC((Xtop - Xmed)/cells*2.);
     cell_size := cell_size1;
     Else
      cell_size := TRUNC((Xtop - Xbot)/cells);
     End if;

   cell_size := 0.01;

   loops := 1 + (Xtop - Xbot)/cell_size;
   If Loops < 1 then
     loops := 1;
   End if;
   Hgram_Array := MDSYS.SDO_LIST_TYPE();
   Length_Array := MDSYS.SDO_LIST_TYPE();
   Hgram_Array.extend(loops);
   Length_Array.extend(loops);
   Range_Array.extend(loops+1);
   current_top := Xbot;


--  dbms_output.put_line('range : ' || Xbot || ' median ' || XMed || ' top ' || Xtop || ' loops ' || loops);

-- Range Array contains the bottoms
   For ii in 1..loops Loop
        current_bottom := current_top;
        Range_Array(ii) := current_bottom;
        current_top := current_top + cell_size;
        If ii = loops then
          less_than := ' <=';
          current_top := Xtop;
        End If;

--        sql_stmt := 'SELECT COUNT(1),AVG(' || InColumn || ') FROM ' || InTable ||
        sql_stmt := 'SELECT COUNT(1),MEDIAN(' || InColumn || ') FROM ' || InTable ||
                    ' WHERE '|| InColumn ||
                    ' >=:1 AND ' || InColumn || less_than ||':2' ;
        EXECUTE IMMEDIATE sql_stmt into kount,len using current_bottom,current_top;
        total_so_far  := total_so_far + kount;
        Hgram_Array(ii) := kount;
        Length_Array(ii) := len;
        counter := counter + 1;
--        dbms_output.put_line('so far ' || total_so_far || ' top ' || current_top || ' the total ' || the_total);
        Exit when total_so_far = the_total;
   End Loop;
   Range_Array(counter+1) := Xtop;
   Hgram_Array.trim(loops-counter);
   Length_Array.trim(loops-counter);
   Range_Array.trim(loops-counter);

--    For ii in 1..counter Loop
--   dbms_output.put_line('range : ' || Range_Array(ii) || ' to ' || Range_Array(ii+1) || ' Median ' || Length_Array(ii) || ' Amount ' || Hgram_Array(ii));
--   end loop;
   RETURN Hgram_Array;

END HISTOGRAM;
--
PROCEDURE DRAW_GRAPH(Graph_table VARCHAR2,pgraph_type VARCHAR2,Title VARCHAR2,SubTitle VARCHAR2,Xin IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Yin IN OUT NOCOPY MDSYS.SDO_LIST_TYPE
                   ,marker NUMBER default NULL) AS
-- Draws a graph (about 10 inches square) that can be viewed with MapViewer

   sql_stmt        VARCHAR2(4000);
   graph_type      VARCHAR2(10) := UPPER(pgraph_type);
   xscale          NUMBER;
   yscale          NUMBER;
   xo              NUMBER := 1.E10;
   yo              NUMBER := 1.E10;
   xmax            NUMBER := -1.E10;
   ymax            NUMBER := -1.E10;

   x1              NUMBER;
   y1              NUMBER;
   x2              NUMBER;
   y2              NUMBER;
   xinc            NUMBER;
   yinc            NUMBER;
   ylast           NUMBER;
   ybig            NUMBER;
   ytotal          NUMBER := 0.0;
   kount           NUMBER := 0.0;
   xx              VARCHAR2(5);
   yy              VARCHAR2(5);
BEGIN

--Setup an output table to be viewed
    If Table_Exists(Graph_Table) = TRUE then
       sql_stmt := 'TRUNCATE TABLE ' || Graph_Table ;
       execute immediate sql_stmt;
    else
       sql_stmt := 'CREATE TABLE ' || Graph_Table || ' (ID VARCHAR2(100),COLOR NUMBER,LINE MDSYS.SDO_GEOMETRY)';
       execute immediate sql_stmt;
    end if;

-- Title: is in a fixed place

     Draw_rectangle(Graph_Table,2.,8.,10.,9.,NULL,1);

     Draw_line(Graph_Table,2.,8.6,10.,8.6,Title,0);
     Draw_line(Graph_Table,2.,8.2,10.,8.2,subTitle,0);

-- Find leftmost point and xmax,ymax
     For ii in 1..Yin.count Loop
       if Xin(ii) < xo then
         xo := Xin(ii);
       end if;
      if Yin(ii) < yo then
         yo := Yin(ii);
      end if;

      if Xin(ii) > xmax then
         xmax := Xin(ii);
      end if;
      if Yin(ii) > ymax then
         ymax := Yin(ii);
      end if;
      ytotal := ytotal + Yin(ii);
      if Yin(ii) <> 0. then
          kount := kount + 1.;
       end if;
     End Loop;

-- Setup scales
    xscale := 10./(xmax-xo)* 0.99;
    yscale := 10./ymax;

-- Label Y axis
   ybig := ymax;
    if SUBSTR(graph_type,1,4) = 'HIST' then
 -- Automatic y scaling
      if yscale * ytotal/kount < 2. then
        yscale := 5./(ytotal/kount);
        ybig := ymax*0.5;
--      dbms_output.put_line('ybig ' || ybig);
      end if;
    end if;
    yinc := 1.;
    y1 := 0.;
    y2 := 0.;
    while ybig > 10 * yinc Loop
      yinc := yinc * 10.;
    end loop;
-- dbms_output.put_line('yinc ' || yinc);
-- Draw xticks
    while y2 < 10.  Loop
       y1 := y1 + yinc;
       y2 := y1/yinc *yscale;
       yy := round(y1/yinc,2);
--  dbms_output.put_line('y1 ' || y1 || ' y2 ' || y2 || ' yy ' || yy);
       if mod(yy,10) = 0 then
         Draw_line(Graph_Table,-0.6,y2,0.,y2,yy,1);
       else
         Draw_line(Graph_Table,-0.6,y2,0.,y2,yy,0);
       end if;
    end Loop;

-- Automatic x scaling
    xinc := 0.1;
    x1 := 0.;
    x2 := 0.;
    while xmax > xo+10 * xinc Loop
      xinc := xinc * 10.;
    end loop;
-- dbms_output.put_line('xinc ' || xinc || ' xscale ' || xscale);
    x1 := xo-xinc;
    x2 := 0.0;
    while x2 < 10.  Loop
      x1 := x1 + xinc;
      x2 := (x1-xo) *xscale;
      xx := round(x1,2);
---  dbms_output.put_line('x1 ' || x1 || ' x2 ' || x2 || ' xx ' || xx || ' xo ' || xo);
      if mod(xx,10) = 0.0 then
         Draw_line(Graph_Table,x2,-0.8,x2,0.,xx,1);
      else
         Draw_line(Graph_Table,x2,-0.8,x2,0.,xx,0);
       end if;
    end Loop;

-- Draw a special tick like a standard deviation
    if marker is not NULL then
      x2 := (marker-xo)*xscale;
      dbms_output.put_line('x2 ' || x2 || ' marker ' || marker || ' scale ' || xscale);
      Draw_line(Graph_Table,x2,-1.4,x2,0.0,marker,1);
    end if;

-- dbms_output.put_line('xmax ' || xmax || ' ymax ' || ymax);
 -- Draw a Histogram

 if SUBSTR(graph_type,1,4) = 'HIST' then
 -- Automatic y scaling
   if yscale * ytotal/kount < 2. then
      yscale := 5./(ytotal/kount);
   end if;
   For ii in 1..Yin.count Loop

      if ylast <> 0. then
        x2 := (Xin(ii)-xo) *xscale;
        Draw_line(Graph_Table,x2,y1,x2,ylast,NULL,2);
        yy := Yin(ii-1);
        Draw_line(Graph_Table,x1,ylast,x2,ylast,yy,2);
      end if;

-- Draw the LHS of the box:
--                             {
--                             {
--_____________________________{______________________ x axis
      if Yin(ii) <> 0. then
        x1 := (Xin(ii)-xo) *xscale;
        x2 := x1;
        y1 := 0.;
        y2 := (Yin(ii)-yo) *yscale;
        if y2 > 10.5 then
          y2 := 10.5;
        end if;
        ylast := y2;
        if x1 <> 0. then
          Draw_line(Graph_Table,x1,y1,x2,y2,NULL,2);
        end if;
      else
        ylast := 0.0;
      end if;

--      dbms_output.put_line('ii ' || ii || ' ' || ylast);
   End Loop;

-- Complete the last box like an inverted "L":  --
--                                             {  |
--                                             {  |
--_____________________________________________{__|___________________ x axis
   if ylast <> 0. then
        x2 := (Xin(Xin.count)-xo) *xscale;
        Draw_line(Graph_Table,x2,y1,x2,ylast,NULL,2);    -- RHS
        yy := Yin(Yin.count);
        Draw_line(Graph_Table,x1,ylast,x2,ylast,yy,2);   -- top
   end if;
 end if;

-- Draw a set of simple line graph: a line between unmarked points
 if graph_type = 'LINE' then

   For ii in 1..Xin.count Loop
      x1 := (Xin(ii)-xo) *xscale;
      x2 := x1;
      y1 := 0.;
      y2 := (Yin(ii)-yo) *yscale;
      Draw_line(Graph_Table,x1,y1,x2,y2,NULL,2);
   End Loop;
 end if;

-- Draw a set of discrete points with axes
  if graph_type = 'POINT' then

   For ii in 1..Xin.count Loop
      x1 := (Xin(ii)-xo) *xscale;
      y1 := (Yin(ii)-yo) *yscale;
      Draw_point(Graph_Table,x1,y1,ii);
   End Loop;
 end if;

 -- Draw axes
 Draw_line(Graph_Table,0.,0.,10.,0.,' X ');

 Draw_line(Graph_Table,0.,0.,0.,10.,' Y ');
 commit;

END DRAW_GRAPH;
--
PROCEDURE DRAW_RECTANGLE(Graph_Table VARCHAR2,x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,id VARCHAR2,color number default 1) AS

-- Draws a rectangle from (x1,y1) to (x2,y2) with the specified color
-- Part of set of procedures to draw a graph that can be viewed with MapViewer
BEGIN
     Draw_line(Graph_Table,x1,y1,x2,y1,NULL,1);  -- bottom
     Draw_line(Graph_Table,x1,y2,x2,y2,NULL,1);  -- right side
     Draw_line(Graph_Table,x1,y1,x1,y2,NULL,1);  -- left side
     Draw_line(Graph_Table,x2,y1,x2,y2,NULL,1);  -- top

END DRAW_RECTANGLE;
--
PROCEDURE DRAW_LINE(Graph_Table VARCHAR2,x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,id VARCHAR2,color number default 1) AS

-- Draws a line from (x1,y1) to (x2,y2) with the given color
-- Part of set of procedures to draw a graph that can be viewed with MapViewer
   sql_stmt       VARCHAR2(4000);
   line           MDSYS.SDO_GEOMETRY;

BEGIN

    sql_stmt := 'INSERT INTO ' || GRAPH_TABLE || ' (ID,LINE,COLOR) VALUES(:1,:2,:3)';

    line := MDSYS.SDO_GEOMETRY(2002,NULL,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),
                              MDSYS.SDO_ORDINATE_ARRAY(x1,y1,x2,y2));

    execute immediate sql_stmt using id,line,color;
    commit;

END DRAW_LINE;
--
PROCEDURE Draw_Point(Graph_Table VARCHAR2,x1 NUMBER,y1 NUMBER,id VARCHAR2) AS

-- Draws a point at (x1,y1) with text label id
-- Part of set of procedures to draw a graph that can be viewed with MapViewer
   sql_stmt        VARCHAR2(4000);
   point           MDSYS.SDO_GEOMETRY;

BEGIN

    sql_stmt := 'INSERT INTO ' || GRAPH_TABLE || ' (ID,LINE) VALUES(:1,:2)';

    point := MDSYS.SDO_GEOMETRY(2001,NULL,NULL,SDO_ELEM_INFO_ARRAY(1,1,1),
                              MDSYS.SDO_ORDINATE_ARRAY(x1,y1));

    execute immediate sql_stmt using id,point;
    commit;

END DRAW_POINT;
--
FUNCTION CREATE_EQUAL_RANGES(pInTable VARCHAR2, pInColumn VARCHAR2,
                      Amounts IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      InBottom NUMBER default NULL,
                      InTop NUMBER default NULL,
                      Incells  NUMBER default 20,
                      Maximum_range_count NUMBER default 0)
RETURN  MDSYS.SDO_LIST_TYPE  AS

-- A Function to
    InTable       VARCHAR2(100) := UPPER(pinTable);
    Hgram_Array   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Range_Array   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Ranges        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Length_Array  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Hgram2_Array  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Range2_Array  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Length2_Array MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    kount         NUMBER;
    cells         NUMBER;
    about         NUMBER;
    bottom        NUMBER;
    top           NUMBER;
    amount        NUMBER := 0.;
    grand         NUMBER := 0.;
    sql_stmt      VARCHAR2(4000);
    the_total     NUMBER;

    next          PLS_INTEGER := 0;
    i             PLS_INTEGER;
    ii            PLS_INTEGER;
    j             PLS_INTEGER;
BEGIN



     sql_stmt := 'SELECT COUNT(1) from ' || InTable;
     EXECUTE IMMEDIATE sql_stmt into kount;


  If Maximum_range_count <> 0 then
     Cells := TRUNC(kount/Maximum_range_count);
     If kount <> Cells * Maximum_range_count then
       Cells := Cells + 1;
     End If;
     about := TRUNC(Maximum_range_count);
  Else
     Cells := InCells;
     about := TRUNC(kount/cells);
  End If;

 dbms_output.put_line('about ' || about || ' cells ' || cells || ' Toatal ' || kount);

  Hgram_Array  := HISTOGRAM(inTable, pInColumn, Length_Array,Range_Array,InBottom,Intop,cells);

  Amounts := MDSYS.SDO_LIST_TYPE();
  Amounts.extend(Hgram_Array.count+10);
  Ranges.extend(Hgram_Array.count+1+10);
  Ranges(1) := Range_Array(1);
  bottom := Ranges(1);

  ii := 1;
  While ii <= Hgram_Array.count Loop

    IF Hgram_Array(ii) > 0 THEN
    amount := amount + Hgram_Array(ii);
    top := Range_Array(ii+1);
-- We have too much in this range - so reduce the range
dbms_output.put_line('Next ' || (next+1) || ' amount ' || amount || ' ii ' || ii || ' median ' || length_array(ii));
    If amount > about then
      amount := amount - Hgram_Array(ii) + TRUNC(Hgram_Array(ii)/2);


      if (about - amount) >= 0 and (about - amount) < about/10 then
      dbms_output.put_line('Good enough ' ||(next+1) || ' amount ' || amount);
      If TRUNC(Length_Array(ii)) <> Length_Array(ii) then
        top := TRUNC(Length_Array(ii))+1;
      else
         top := TRUNC(Length_Array(ii));
      End if;
-- we have just the right amount
        next := next + 1;
        Ranges(next+1) := top;
        bottom := top+1;
        Amounts(next) := amount;
        amount := TRUNC(Hgram_Array(ii)/2);
      Else
--        cells := TRUNC(about/(about -amount));
--        if cells > 10 then
           cells := 10;
--        End If;
        If amount > about then
           amount := amount - TRUNC(Hgram_Array(ii)/2);
           bottom :=  Range_Array(ii);
        Else
           If TRUNC(Length_Array(ii)) <> Length_Array(ii) then
           bottom :=  TRUNC(Length_Array(ii))+1 ;
           else
              bottom :=  TRUNC(Length_Array(ii));
           End If;
        End if;
        dbms_output.put_line('bottom ' || bottom || ' top ' || top);
        Hgram2_Array  := HISTOGRAM(inTable, pInColumn, Length2_Array,Range2_Array,Bottom,top,cells);
        i := 0;
        dbms_output.put_line('cells ' || cells || ' hgram2.count ' || Hgram2_array.count || ' R ' ||Range2_Array.count );
        While i < Hgram2_Array.count Loop
           i := i + 1;
           If amount + Hgram2_Array(i) < about then
           top := Range2_Array(i+1);
           amount := amount + Hgram2_Array(i);
           dbms_output.put_line('adding ' || Hgram2_Array(i) || ' amount ' || amount || ' top ' || top);
           ElsIf amount + TRUNC(Hgram2_Array(i)/2) < about then

              if  TRUNC(Length2_Array(i)) <> Length2_Array(i) then
               top := TRUNC(Length2_Array(i))+1;
              else
                top := TRUNC(Length2_Array(i));
              End If;
           amount := amount + TRUNC(Hgram2_Array(i)/2);
           dbms_output.put_line('Adding ' || Hgram2_Array(i) || ' amount ' || amount || ' TOP ' || top);
           next := next + 1;
           Ranges(next+1) := top;
           bottom := top+1;
           Amounts(next) := amount;
           amount := Hgram2_Array(i) - TRUNC(Hgram2_Array(i)/2);

           Else
             next := next + 1;
             Ranges(next+1) := top;
             bottom := top+1;
             Amounts(next) := amount;
            amount := Hgram2_Array(i);
             top := Range2_Array(i+1);
           End If;
        End Loop;

      End If;
    End If;
    END IF;
    ii := ii+1;
  End Loop;

  If amount > 0 then
    next := next + 1;
    Ranges(next+1) := top;
    Amounts(next) := amount;
  End If;

  Ranges.trim(Ranges.count-1-next);
  Amounts.trim(Amounts.count-next);
  Ranges(Ranges.count) :=  Ranges(Ranges.count) +1;

  for ii in 1..Amounts.count Loop
     grand := grand + Amounts(ii);
     bottom := Ranges(ii);
     top := Ranges(ii+1);
                 sql_stmt := 'SELECT COUNT(1) FROM ' || pInTable || ' WHERE ' ||
           pInColumn || ' >=:1 AND ' || pInColumn || ' <:2';
     EXECUTE IMMEDIATE sql_stmt into the_total using Bottom,top;
     dbms_output.put_line('range : ' || Ranges(ii) || ' to ' || Ranges(ii+1) ||
     ' Count was : ' || Amounts(ii)  || ' oracle ' || the_total);
     Amounts(ii) := the_total;

  End Loop;
  Ranges(Ranges.count) :=  Ranges(Ranges.count) -1;
  dbms_output.put_line('GRAND ' || grand);
  RETURN RANGES;

END CREATE_EQUAL_RANGES;
--
Function GetNumRings( p_geometry in mdsys.sdo_geometry, p_element_type VARCHAR2 default NULL)
  Return Number Deterministic Is

-- Function to get the count of different types of rings from a geometry
--
-- Parameters:
--            p_geometry  - Input geometry
--            p_element_type - What to find. Maybe 'outer','inner', or a
--                            comma separated list such as '1005,2005'
-- Author Simon Greener:
-- Updated: Sidey Timmins to enable the element type to be passed in.
-- Returns: the total number of rings (default or 'ALL')
--         or the number of outer or inner rings
--         or using a comma separated list some combination of 1003, 2003,1005,2005
--
-- Reference: http://www.spatialdbadvisor.com/oracle_spatial_tips_tricks/89/sdo_utilgetnumrings-an-alternative

  etype          VARCHAR2(30) := UPPER(p_element_type);
  sql_stmt       VARCHAR2(4000);
  pad            VARCHAR2(30);
  v_ring_count   NUMBER := 0;

Begin
   IF etype is NULL or etype ='ALL' then
    SELECT count(*) as ringcount
      INTO v_ring_count
      FROM (SELECT e.id,
                   e.etype,
                   e.offset,
                   e.interpretation
              FROM (SELECT trunc((rownum - 1) / 3,0) as id,
                           sum(case when mod(rownum,3) = 1 then sei.column_value else null end) as offset,
                           sum(case when mod(rownum,3) = 2 then sei.column_value else null end) as etype,
                           sum(case when mod(rownum,3) = 0 then sei.column_value else null end) as interpretation
                      FROM TABLE(p_geometry.sdo_elem_info) sei
                     GROUP BY trunc((rownum - 1) / 3,0)
                    ) e
           ) i
     WHERE i.etype in (1003,1005,2003,2005);
   ELSE
  
     if etype = 'OUTER' then
        pad := '1003)';
     elsif etype = 'INNER' then
        pad := '2003)';
     else
        pad := etype||')';
     end if;
  
     sql_stmt :=
     'SELECT count(*) FROM ' ||
                  '(SELECT e.id, e.etype,e.offset,' ||
                  'e.interpretation FROM (SELECT trunc((rownum - 1) / 3,0) as id,' ||
                           'sum(case when mod(rownum,3) = 1 then sei.column_value else null end) as offset,' ||
                           'sum(case when mod(rownum,3) = 2 then sei.column_value else null end) as etype,' ||
                           'sum(case when mod(rownum,3) = 0 then sei.column_value else null end) as interpretation' ||
                      ' FROM TABLE(:1) sei ' ||
                     ' GROUP BY trunc((rownum - 1) / 3,0)' ||
                    ') e' ||
           ') i WHERE i.etype in ('|| pad;
     EXECUTE immediate sql_stmt into v_ring_count using p_geometry.sdo_elem_info;
  
   END IF;

  Return v_ring_count;
  
End GetNumRings;
--
FUNCTION GET_BIG_RING(ingeom MDSYS.SDO_GEOMETRY) return MDSYS.SDO_GEOMETRY AS

-- Simple function to return the biggest ring from a geometry (like the big island
-- from Hawaii - since an Oracle 2007 geometry may have many separate rings)

    no_of_rings   NUMBER := Getnumrings(ingeom,'outer');
    this_ring     NUMBER;
    ring          NUMBER := 1;
    big_ring      NUMBER := 1;
    ring_geom     MDSYS.SDO_GEOMETRY;
    area          NUMBER;
    big_area      NUMBER := 0.0;
    
BEGIN

    WHILE ring <= no_of_rings and ring > 0 loop
       this_ring := ring;
       ring := ring + 1;
  --                       Just to take care of an error when the person
  --                       makes an extra 1003 but forgets to put 2007 as gtype
       if this_ring = no_of_rings or ingeom.sdo_gtype= 2003 then
         ring := 0; -- we are done
       end if;
  -- Get the next outer ring
  
       ring_geom := sdo_util.extract(ingeom,this_ring,0);
       area := sdo_geom.sdo_area(ring_geom,0.05,'unit=sq_meter');
       if area > big_area then
         big_ring := this_ring;
         big_area := area;
       end if;
    End loop;
  
    if no_of_rings > 0 then
--      dbms_output.put_line('Big ring is: '|| big_ring);
       RETURN sdo_util.extract(ingeom,big_ring,0);
    else
      RETURN NULL;
    end if;
    
END GET_BIG_RING;
--
FUNCTION GET_OUTER_RING(ingeom MDSYS.SDO_GEOMETRY,ring IN OUT NOCOPY NUMBER) return MDSYS.SDO_GEOMETRY AS

-- Simple function to return the next outer ring from a geometry (with its holes if any).
-- (like the next island from the Hawaian islands)

    no_of_rings   NUMBER := Getnumrings(ingeom,'outer');
    this_ring     NUMBER;

BEGIN

    if ring is NULL or ring <=0 then  ring := 1; end if;
  
  -- Get the next polygon
    if ring <= no_of_rings and no_of_rings>0 then
       this_ring := ring;
       ring := ring + 1;
  --                       Just to take care of an error when the person
  --                       makes an extra 1003 but forgets to put 2007 as gtype
  
       if this_ring = no_of_rings or ingeom.sdo_gtype= 2003 then
         ring := 0; -- we are done
       end if;
  
       RETURN sdo_util.extract(ingeom,this_ring,0);
    else
       RETURN NULL;
    end if;
    
END GET_OUTER_RING;
--
--==============================================================================
-- Utilities
--==============================================================================
FUNCTION TABLE_EXISTS ( pInTable  IN VARCHAR2, pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN AS

   /**Taken from the cartodb
    Author: Nick Padfield
    Creation Date: 9/6/2006
   **/
   -- Updated to not place NVL in SQL statement

   InTable       VARCHAR2(30)     := UPPER(pInTable);
   InSchema      VARCHAR2(30)     := UPPER(NVL(pInSchema,USER));
   RecordCount   NUMBER(22)       := 0.;
   sql_stmt      VARCHAR2(4000);
   
   BEGIN
      --------------------------------------------------------------------------
      -- Check to see if the table exists!
      sql_stmt := 'SELECT COUNT(1) FROM all_objects WHERE object_type = ''TABLE'' AND owner =:1 AND object_name =:2';
      EXECUTE IMMEDIATE sql_stmt INTO RecordCount USING InSchema,InTable;

      -- If table exists, return TRUE, if not, return FALSE.
      RETURN (RecordCount > 0.);

   END TABLE_EXISTS;
--
FUNCTION field_exists(pTableName VARCHAR2, pFieldName VARCHAR2, pSchemaName VARCHAR2 DEFAULT USER) RETURN BOOLEAN AS
 
 -- Check if a column exists in a Table.
 
   vTableName             VARCHAR2(30)             := UPPER(SUBSTR(pTableName,1,30));
   vFieldName             VARCHAR2(30)             := UPPER(SUBSTR(pFieldName,1,30));
   vSchemaName            VARCHAR2(30)             := UPPER(SUBSTR(pSchemaName,1,30));
   sql_stmt               VARCHAR2(4000);
   vCnt                   PLS_INTEGER;
   vResult                BOOLEAN                   := FALSE;
   
BEGIN
   sql_stmt := 'SELECT count(*) FROM all_tab_cols WHERE owner = :1 AND table_name = :2 AND column_name = :3';
   EXECUTE IMMEDIATE sql_stmt INTO vCnt USING vSchemaName,vTableName,vFieldName;
   
   IF (vCnt=1) THEN
      vResult := TRUE;
   END IF;
   
  RETURN vResult;

END field_exists;
--
PROCEDURE  find_missing_records (
                                   pTopo1    VARCHAR2,
                                   pTopo2     VARCHAR2,
                                   pOutputTable     VARCHAR2,
                                   pKeyField   VARCHAR2,
                                   pFeatTable   VARCHAR2 default NULL
                                ) AS
    psql    VARCHAR2(4000);
    vTopo1  VARCHAR2(20);
    vTopo2  VARCHAR2(20);
    vOutputTable  VARCHAR2(30);
    vKeyField   VARCHAR2(20);
    vFeatTable    VARCHAR2(20);
    topo1Count   NUMBER := 0;
    topo2Count   NUMBER := 0;
    note    VARCHAR2(4000);
    note2    VARCHAR2(4000);
    note1    VARCHAR2(4000);
    topo1FSL    NUMBER := 0;
    topo2FSL    NUMBER := 0;
    tableCount1  NUMBER := 0;
    tableCount2  NUMBER := 0;
    missing_fsl_topo2  GZ_TYPES.stringarray;
    missing_fsl_topo1  GZ_TYPES.stringarray;
    lnote2   VARCHAR2(4000);
    lnote1   VARCHAR2(4000);
    fsl_array   GZ_TYPES.stringarray;
    missing_from_2  GZ_TYPES.stringarray;
    missing_from_1  GZ_TYPES.stringarray;

BEGIN

    vTopo1 := UPPER(pTopo1);
    vTopo2 := UPPER(pTopo2);
    vOutputTable := UPPER(pOutputTable);
    vKeyField := UPPER(pKeyField);
    vFeatTable := UPPER(pFeatTable);

    --check if topologies exist
    psql := 'select count(distinct topology) from user_sdo_topo_info where topology = :p1';
    EXECUTE IMMEDIATE psql INTO topo1Count USING vTopo1;
    EXECUTE IMMEDIATE psql INTO topo2Count USING vTopo2;

    IF topo1Count = 0 AND topo2Count = 0 THEN
      note := 'The topologies ' || vTopo1 || ' and ' || vTopo2 || ' do not exist in this schema.  Terminating execution.';
      dbms_output.put_line ('ERROR - ' || note);
      RAISE_APPLICATION_ERROR(-20001, note);
    ELSIF topo1Count = 0 THEN
      note := 'Topology ' || vTopo1 || ' does not exist in this schema.  Terminating execution.';
      dbms_output.put_line ('ERROR - ' || note);
      RAISE_APPLICATION_ERROR(-20001, note);
    ELSIF topo2Count = 0 THEN
      note := 'Topology ' || vTopo2 || ' does not exist in this schema.  Terminating execution.';
      dbms_output.put_line ('ERROR - ' || note);
      RAISE_APPLICATION_ERROR(-20001, note);
    END IF;

   IF pFeatTable IS NULL THEN
        --count FSL tables in input topology
        psql := 'select count(table_name) from user_sdo_topo_info where topology = :p1 and table_name like :p2';
        EXECUTE IMMEDIATE psql INTO topo1FSL USING vTopo1, '%FSL%';

        --count FSL tables in final topology
        psql := 'select count(table_name) from user_sdo_topo_info where topology = :p1 and table_name like :p2';
        EXECUTE IMMEDIATE psql INTO topo2FSL USING vTopo2, '%FSL%';

        --check if input and final topologies both have FSL tables
        IF topo1FSL = 0 AND topo2FSL = 0 THEN
            note := 'Topology ' || vTopo1 || ' and topology ' ||  vTopo2 || ' do not have any FSL tables associated with them. Terminating execution.';
            dbms_output.put_line ('ERROR - ' || note);
            RAISE_APPLICATION_ERROR(-20001, note);
        ELSIF topo1FSL = 0 THEN
            note := 'Topology ' || vTopo1 || ' does not have any FSL tables associated with it. Terminating execution.';
            dbms_output.put_line ('ERROR - ' || note);
            RAISE_APPLICATION_ERROR(-20001, note);
        ELSIF topo2FSL = 0 THEN
            note := 'Topology ' || vTopo2 || ' does not have any FSL tables associated with it. Terminating execution.';
            dbms_output.put_line ('ERROR - ' || note);
            RAISE_APPLICATION_ERROR(-20001, note);
        END IF;

       --see which FSL tables exist in input topology that do not in final topology
       psql := 'select regexp_replace(table_name, ''' || vTopo1 || '_'','''') from user_sdo_topo_info where topology = :p1 and table_name like :p2';
       psql := psql || ' MINUS';
       psql := psql || ' select regexp_replace(table_name, ''' || vTopo2 || '_'','''') from user_sdo_topo_info where topology = :p3 and table_name like :p4';

       EXECUTE IMMEDIATE psql BULK COLLECT INTO missing_fsl_topo2 USING vTopo1, '%FSL%', vTopo2, '%FSL%';

      --see which FSL tables exist in final topology that do not in input topology
      psql := 'select regexp_replace(table_name, ''' || vTopo2 || '_'','''') from user_sdo_topo_info where topology = :p1 and table_name like :p2';
      psql := psql || ' MINUS';
      psql := psql || ' select regexp_replace(table_name, ''' || vTopo1 || '_'','''') from user_sdo_topo_info where topology = :p3 and table_name like :p4';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO missing_fsl_topo1 USING vTopo2, '%FSL%', vTopo1, '%FSL%';

       IF missing_fsl_topo2.COUNT > 0 THEN
        note2 := 'Topology ' || vTopo2 || ' is missing the following FSL tables that are in topology ' || vTopo1 || ': ';
        FOR i in 1..missing_fsl_topo2.COUNT
        LOOP
            lnote2 := lnote2 || ' ' || missing_fsl_topo2(i) || ',';
        END LOOP;
        lnote2 := lnote2 || ' so we can''t compare their records. ';
       END IF;

       IF missing_fsl_topo1.COUNT > 0 THEN
        note1 := 'Topology ' || vTopo1 || ' is missing the following FSL tables that are in topology ' || vTopo2 || ': ';
        FOR i in 1..missing_fsl_topo1.COUNT
        LOOP
            lnote1 := lnote1 || ' ' || missing_fsl_topo1(i) || ',';
        END LOOP;
         lnote1 := lnote1 || ' so we can''t compare their records. ';
       END IF;

       --if either of the topologies contains FSL tables missing from the other one, raise error
       IF missing_fsl_topo2.COUNT > 0 AND missing_fsl_topo1.COUNT > 0 THEN
        dbms_output.put_line ('ERROR - ' || note2 || lnote2 || note1 || lnote1);
        RAISE_APPLICATION_ERROR(-20001, note2 || lnote2 || note1 || lnote1);
       ELSIF missing_fsl_topo2.COUNT > 0 THEN
        dbms_output.put_line ('ERROR - ' || note2 || lnote2);
        RAISE_APPLICATION_ERROR(-20001, note2 || lnote2);
       ELSIF missing_fsl_topo1.COUNT > 0 THEN
        dbms_output.put_line ('ERROR - ' || note1 || lnote1);
        RAISE_APPLICATION_ERROR(-20001, note1 || lnote1);
       --if both topologies contain the same set of FSL tables, make an array of the table suffixes
       ELSIF missing_fsl_topo1.COUNT = 0 AND missing_fsl_topo2.COUNT = 0 THEN
        psql := 'select regexp_replace(table_name, ''' || vTopo1 || '_'','''') from user_sdo_topo_info where topology = :p1 and table_name like :p2';
        EXECUTE IMMEDIATE psql BULK COLLECT INTO fsl_array USING vTopo1, '%FSL%';
       --ELSE
        --something else is wrong that is not accounted for above...can't think of anything
       END IF;

       --reset lnotes
       lnote1 := '';
       lnote2 := '';
   ELSE
       --if a specific table suffix is passed in, just make sure the table exists in both topologies
       psql := 'select count(table_name) from user_sdo_topo_info where topology = :p1 and table_name = :p2';
       EXECUTE IMMEDIATE psql INTO tableCount1 USING vTopo1, vTopo1 || '_' || vFeatTable;
       EXECUTE IMMEDIATE psql INTO tableCount2 USING vTopo2, vTopo2 || '_' || vFeatTable;

       IF tableCount1 = 0 and tableCount2 = 0 THEN
          note := 'The feature layer tables ' || vTopo1 || '_' || vFeatTable || ' and ' || vTopo2 || '_' || vFeatTable || ' do not exist in your schema!';
          dbms_output.put_line ('ERROR - ' || note);
          RAISE_APPLICATION_ERROR(-20001, note);
       ELSIF tableCount1 = 0 THEN
          note := 'The feature layer table ' || vTopo1 || '_' || vFeatTable || ' does not exist in your schema!';
          dbms_output.put_line ('ERROR - ' || note);
          RAISE_APPLICATION_ERROR(-20001, note);
       ELSIF tableCount2 = 0 THEN
          note := 'The feature layer table ' || vTopo2 || '_' || vFeatTable || ' does not exist in your schema!';
          dbms_output.put_line ('ERROR - ' || note);
          RAISE_APPLICATION_ERROR(-20001, note);
       END IF;
   END IF;

   --now check to see if output table already exists in schema...
   IF GZ_BUSINESS_UTILS.gz_table_exists(vOutputTable) THEN
    psql := 'truncate table ' || vOutputTable || '';
    EXECUTE IMMEDIATE psql;
   ELSE
    --call table creator procedure
    GZ_QA.create_gz_missing_geoids(vOutputTable);
   END IF;

   IF pFeatTable IS NULL THEN
       --if checking all FSL tables, loop through them and check values in key field for each
       FOR i in 1..fsl_array.COUNT
       LOOP

       --check to see if there are any records in input topology FSL table that aren't in final topology FSL table (this could be legit)
       --fsl_array(i) - current FSL table name
       psql := 'select ' || vKeyField || ' from ' || vTopo1 || '_' || fsl_array(i) ||'';
       psql := psql || ' MINUS';
       psql := psql || ' select ' || vKeyField || ' from ' || vTopo2 || '_' || fsl_array(i) ||'';

       BEGIN
        EXECUTE IMMEDIATE psql BULK COLLECT INTO missing_from_2;
       EXCEPTION
             WHEN OTHERS
             THEN
                IF SQLCODE = -904
                THEN
                   --ORA-009404: "<FIELD>": invalid identifier
                   --make error message more wordy
                   note := 'Sorry, the field ' || vKeyField || ' does not exist in one or both of the tables ending with ' || fsl_array(i) || '.';
                   dbms_output.put_line ('ERROR - ' || note);
                   RAISE_APPLICATION_ERROR(-20001, note);
                ELSE
                   RAISE;
                END IF;
       END;

       --put records missing from final topology into output table, if any exist
       IF missing_from_2.COUNT > 0 THEN
        FOR j in 1..missing_from_2.COUNT
        LOOP
             psql := 'insert into ' || vOutputTable || ' (TOPOLOGY1, TOPOLOGY2, FEATURE_TABLE, MISSING_FROM_2) ';
             psql := psql || 'values (:p1, :p2, :p3, :p4)';
             EXECUTE IMMEDIATE psql USING vTopo1, vTopo2, fsl_array(i), missing_from_2(j);
        END LOOP;
       END IF;

       --now check to see if there are any records in final topology FSL table that aren't in input topology FSL table (this would be bad, right?)
       psql := 'select ' || vKeyField || ' from ' || vTopo2 || '_' || fsl_array(i) ||'';
       psql := psql || ' MINUS';
       psql := psql || ' select ' || vKeyField || ' from ' || vTopo1 || '_' || fsl_array(i) ||'';

       EXECUTE IMMEDIATE psql BULK COLLECT INTO missing_from_1;
       --no need for exception here, since any problems would have been caught in the one above

       --put records missing from input topology into output table, if any exist
       IF missing_from_1.COUNT > 0 THEN
        FOR j in 1..missing_from_1.COUNT
        LOOP
             psql := 'insert into ' || vOutputTable || ' (TOPOLOGY1, TOPOLOGY2, FEATURE_TABLE, MISSING_FROM_1) ';
             psql := psql || 'values (:p1, :p2, :p3, :p4)';
             EXECUTE IMMEDIATE psql USING vTopo1, vTopo2, fsl_array(i), missing_from_1(j);
        END LOOP;
       END IF;

       END LOOP;
   ELSE
       --just compare table in both topologies
       psql := 'select ' || vKeyField || ' from ' || vTopo1 || '_' || vFeatTable ||'';
       psql := psql || ' MINUS';
       psql := psql || ' select ' || vKeyField || ' from ' || vTopo2 || '_' || vFeatTable ||'';

       BEGIN
       EXECUTE IMMEDIATE psql BULK COLLECT INTO missing_from_2;
       EXCEPTION
                 WHEN OTHERS
                 THEN
                    IF SQLCODE = -904
                    THEN
                       --ORA-009404: "<FIELD>": invalid identifier
                       --make error message more wordy
                       note := 'Sorry, the field ' || vKeyField || ' does not exist in one or both of the tables ending with ' || vFeatTable || '.';
                       dbms_output.put_line ('ERROR - ' || note);
                       RAISE_APPLICATION_ERROR(-904, note);
                    ELSE
                       RAISE;
                    END IF;
       END;

       --put records missing from final topology into output table, if any exist
       IF missing_from_2.COUNT > 0 THEN
        FOR i in 1..missing_from_2.COUNT
        LOOP
                 psql := 'insert into ' || vOutputTable || ' (TOPOLOGY1, TOPOLOGY2, FEATURE_TABLE, MISSING_FROM_2) ';
                 psql := psql || 'values (:p1, :p2, :p3, :p4)';
                 EXECUTE IMMEDIATE psql USING vTopo1, vTopo2, vFeatTable, missing_from_2(i);
        END LOOP;
       END IF;

       --now check to see if there are any records in final topology FSL table that aren't in input topology FSL table (this would be bad, right?)
       psql := 'select ' || vKeyField || ' from ' || vTopo2 || '_' || vFeatTable ||'';
       psql := psql || ' MINUS';
       psql := psql || ' select ' || vKeyField || ' from ' || vTopo1 || '_' || vFeatTable ||'';

       EXECUTE IMMEDIATE psql BULK COLLECT INTO missing_from_1;
       --no need for exception here, since any problems would have been caught in the one above

       --put records missing from input topology into output table, if any exist
       IF missing_from_1.COUNT > 0 THEN
        FOR i in 1..missing_from_1.COUNT
            LOOP
                 psql := 'insert into ' || vOutputTable || ' (TOPOLOGY1, TOPOLOGY2, FEATURE_TABLE, MISSING_FROM_1) ';
                 psql := psql || 'values (:p1, :p2, :p3, :p4)';
                 EXECUTE IMMEDIATE psql USING vTopo1, vTopo2, vFeatTable, missing_from_1(i);
            END LOOP;
       END IF;
   END IF;

END find_missing_records;
--
PROCEDURE create_gz_missing_geoids (
      p_table_name     IN VARCHAR2
   )
   AS
      --Elise copied from GZ_UTILITIES.CREATE_GEN_XTEND_TRACKING_LOG on 8/10/11
      --Creates empty table of type GZ_MISSING_GEOIDS for Find_Missing_Records procedure, drops if already exists
      psql          VARCHAR2(4000);

   BEGIN

      psql := 'CREATE TABLE ' || p_table_name || ' ';
      psql := psql || ' NOPARALLEL NOLOGGING AS '
                   || 'SELECT * FROM TABLE(GZ_QA.NEW_GZ_MISSING_GEOIDS ) ';
      BEGIN
         EXECUTE IMMEDIATE psql;
      EXCEPTION
         WHEN OTHERS
         THEN
            IF SQLCODE = -60
            OR SQLCODE = -18014
            THEN
               --ORA-00060: deadlock detected while waiting for resource
               --Usually bogus, just heavy production
               --can't use dbms_lock, it's not available on all databases (yet)
               --DBMS_LOCK.sleep(5);
               --just try again like fools
               EXECUTE IMMEDIATE psql;
            ELSIF SQLCODE = -955
            THEN
               --table already exists
               EXECUTE IMMEDIATE 'DROP TABLE ' || UPPER(p_table_name) || ' PURGE';
               EXECUTE IMMEDIATE psql;
            ELSE
               RAISE;
            END IF;
      END;

      EXECUTE IMMEDIATE 'GRANT SELECT ON ' || p_table_name || ' TO "PUBLIC" ';
END create_gz_missing_geoids;
--
FUNCTION NEW_GZ_MISSING_GEOIDS RETURN GZ_TYPES.GZ_MISSING_GEOIDS PIPELINED
AS
BEGIN
    NULL;
END NEW_GZ_MISSING_GEOIDS;
--
--==============================================================================
-- Unit testing procedures that check the functions and procedures above.
-- They are usually written so that an input argument Geometry may be NULL.
-- The code will then use simple canned data from within. When the caller supplies
-- a real geometry, then that is used in the test. !!
--==============================================================================
FUNCTION INERTIA_PARAM(geometry MDSYS.SDO_GEOMETRY default NULL,choice NUMBER default 1.)  RETURN NUMBER AS

rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
radii            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
axes             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
geom             mdsys.sdo_geometry;
xys              mdsys.sdo_ordinate_array;
rot_xys          mdsys.sdo_ordinate_array := mdsys.sdo_ordinate_array();
xc           NUMBER;
yc           NUMBER;
angle        NUMBER;

BEGIN/*
   geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1),mdsys.sdo_ordinate_array(2.,0.,10.,4.,8.,8.,0.,4.,2.,0.));

geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1,11,2003,1),mdsys.sdo_ordinate_array(2.,0.,10.,4.,8.,8.,0.,4.,2.,0.,   3,2,2,4,4,5,5,3,3,2));
geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1),mdsys.sdo_ordinate_array(2.,0.,6.,4.,4.,6.,0.,0.,2.,0.));
--    geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1),
--    mdsys.sdo_ordinate_array(2.,0.,10.0,4.,6.000000,12.,-2.,8.000,2.,0.000000));
--    geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1),
--    mdsys.sdo_ordinate_array(2.,0.,-2,8.,-10.000000,4.,-6.,-4.000,2.,0.000000));
--    geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1),
--    mdsys.sdo_ordinate_array(0.,0.,5.5,0.,5.5,4.55,0.35,5.15,0.,0.0));
  xys := geom.sdo_ordinates;
   angle := -atan2(0.5,1.)*rad2deg;
--   rot_xys := ROTATE_COORDINATES(Xys,angle,xys(1),xys(2));
   angle := 30.;

--   geom := mdsys.sdo_geometry(2003,8265,NULL,mdsys.sdo_elem_info_array(1,1003,1),rot_xys);
--    angle := 50.;
--    xys := mdsys.sdo_ordinate_array(1.123456,1.123456,2.123456,1.123456,2.123456,2.123456,1.123456,2.1234569,1.123456,1.123456);
  xys := mdsys.sdo_ordinate_array(0,0,10,0,10,100,0,100,0,0);
  rot_xys := ROTATE_COORDINATES(Xys,angle,xys(1),xys(2));
    for ii in 1..5 loop
       dbms_output.put_line('x ' || round(rot_xys(ii*2-1),8) || ' y ' || round(rot_xys(ii*2),8));
    end loop;
 geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1),rot_xys);
*/
   geom := geometry;
   axes := measure_elongate_length(geom,xc,yc,radii);
   for ii in 1..axes.count loop
     dbms_output.put_line('ii ' || ii || ' axes ' || ROUND(axes(ii),8));
   end loop;

   return axes(choice);



END Inertia_Param;
--
PROCEDURE try_inertia_param(ingeom MDSYS.SDO_GEOMETRY default NULL) as
ugeometry        MDSYS.SDO_GEOMETRY;
xys              MDSYS.sdo_ordinate_array;
axes             NUMBER;
choice           NUMBER :=1;
BEGIN
-- These 3 geometries illustrate the difficulty of computing a rotation angle
-- and its sensitivity to slight mass movements
--1) closest to the original

   xys := SDO_ORDINATE_ARRAY(-122.32118, 47.315167, -122.32339, 47.315169, -122.32801, 47.315165, -122.32758,
 47.310322, -122.32913, 47.309425, -122.327,47.3086,    -122.32439, 47.308752, -122.3241, 47.308776,
-122.32394, 47.308806, -122.32038, 47.310589,   -122.3212, 47.3128,
-122.3223, 47.313404, -122.32118,
47.315167);

  ugeometry := SDO_GEOMETRY(2003, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 1003,1),xys);
   xys := SDO_ORDINATE_ARRAY(-122.32118, 47.315167, -122.32339, 47.315169, -122.32801, 47.315165, -122.32758,
 47.310322, -122.32913, 47.309425,               -- -122.327,47.3086,
 -122.32439, 47.308752, -122.3241, 47.308776,
-122.32394, 47.308806, -122.32038, 47.310589,     -122.3212, 47.3128,
-122.3223, 47.313404, -122.32118,
47.315167);

  ugeometry := SDO_GEOMETRY(2003, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 1003,1),xys);

   xys := SDO_ORDINATE_ARRAY(-122.32118, 47.315167, -122.32339, 47.315169, -122.32801, 47.315165, -122.32758,
 47.310322, -122.32913, 47.309425, -122.327,47.3086,    -122.32439, 47.308752, -122.3241, 47.308776,
-122.32394, 47.308806, -122.32038, 47.310589,    -- -122.3212, 47.3128,
-122.3223, 47.313404, -122.32118,
47.315167);
 xys := SDO_ORDINATE_ARRAY(-122.40576, 48.2521933, -122.40575, 48.252177, -122.4015, 48.254234, -122.40576, 48.2521933);
 xys := MDSYS.SDO_ORDINATE_ARRAY(-122.405757331362,48.2521932631296,-122.405747,48.252177,-122.401495316615,48.2542339864937,-122.405757331362,48.2521932631296);
 ugeometry := SDO_GEOMETRY(2003, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 1003,1),xys);

   axes := inertia_param(ingeom,choice);
END try_inertia_param;
--
FUNCTION try_interior_point(ingeom MDSYS.SDO_GEOMETRY default NULL,px number default NULL,py number default NULL) RETURN MDSYS.SDO_GEOMETRY as
-- Set up the xys so there is a U-shaped polygon with the centroid outside the polygon
--   Ord       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(1,1,3.5,1,3.5,2.5,3,2.5,3,1.5,2,1.5,2,2.5,1,2.5,1,1);
--      Ord       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(1,1,2,1,2.,2.499,3,2.499,3,1,3.8,1,3.8,2.5,1.,2.5,1,1);
--         Ord       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(1,1,2,1,2.,2.499,10,2.499,10,1,13.8,1,13.8,2.5,1.,2.5,1,1);
   Ord       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(2,2.5,3,2.5,3,5,0,5,0,4,1,4,1,4.99,2.99,4.99,2.99,3,2,3,2,2.5);
   PolyXys   MDSYS.SDO_ORDINATE_ARRAY;
   rot_Xys   MDSYS.SDO_ORDINATE_ARRAY;
   Info      MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1);
   geom      MDSYS.SDO_GEOMETRY;
   dist      MDSYS.SDO_LIST_TYPE;
   inside    PLS_INTEGER;
   x         NUMBER;
   y         NUMBER;
   angle     NUMBER := 20.;
BEGIN

   if ingeom is not NULL then
      geom := ingeom;
      Info := geom.sdo_elem_info;
   else
      geom := SDO_GEOMETRY(2003, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 1003,1),Ord);
   end if;

   if px is not NULL then
     x := px;
     y := py;
   end if;
   PolyXys := geom.sdo_ordinates;
--   rot_xys := ROTATE_COORDINATES(PolyXys,angle,Polyxys(1),Polyxys(2));
--     for ii in 1..trunc(polyxys.count/2) loop
--     dbms_output.put_line('vertex ' || ii || ' x ' || ROUND(rot_xys(ii*2-1),8) || ' y ' || ROUND(rot_xys(ii*2),8));
--  end loop;
   Interior_point(Polyxys,Info,x,y);
   RETURN MDSYS.SDO_GEOMETRY(2001,8265,MDSYS.SDO_POINT_TYPE(x,y,null),null,null);
END try_interior_point;
--
PROCEDURE try_line_parallel(ingeom MDSYS.SDO_GEOMETRY default NULL) as
-- Set up the xys so there is a simple rectangle with a nasty zigzag on the top
   Ord       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(1,1,5,1,5,10,4.9,10,4.9,9.9,4.9,5,1,5,1,1);
   PolyXys   MDSYS.SDO_ORDINATE_ARRAY;
   geom      MDSYS.SDO_GEOMETRY;
   distance   NUMBER;
   result     NUMBER;
   angle_tolerance NUMBER := 10.;
   deg2rad              NUMBER   :=0.0174532925199432957692369076848861271344;
   cos_angle_tolerance NUMBER;
   sin_angle_tolerance NUMBER;

   dist_tolerance  NUMBER := 1000.;
   x1         NUMBER := 5;
   y1         NUMBER := 10;
   x2         NUMBER := 6;
   y2         NUMBER := 1;
   x3         NUMBER := 4.9999;
   y3         NUMBER := 9.999;
   x4         NUMBER := 5.9999;
   y4         NUMBER := 0.998;
   xi         NUMBER;
   yi         NUMBER;
   xalt         NUMBER;
   yalt         NUMBER;
   distance_q   NUMBER;
BEGIN

   if ingeom is not NULL then
      geom := ingeom;
   else
    x1 := 70971.6838873964;
    y1   :=  184331.932613249;
    x2   :=  71241.4748303597;
    y2  :=  184666.358957743;
     x3 :=71241.4748303597;
     y3 :=184666.358957741;
      x4 :=70439.6838592369;
     y4 :=183672.478212245;
      geom := SDO_GEOMETRY(2003, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 1003,1),Ord);
   end if;

   sin_angle_tolerance := GZ_UTIL_ZONE.sincos(angle_tolerance*deg2rad,cos_angle_tolerance);
   PolyXys := geom.sdo_ordinates;
   x1 := -99.7;
   y1 := 30.2;
   x2 := -99.9;
   y2 := 30.;
   x3:= -100.1;
   y3 := 30;
   x4 := -99.9;
   y4 := 29.8;
   x1 := -86.482312;
   y1 := 34.755425; 
   x2 := -86.482376;
   y2 := 34.751743;
   x3 := -86.479327;
   y3 := 34.755393;
   x4 := -86.479296;
   y4 := 34.755908;
   
   cos_angle_tolerance := 0.0;
   result := line_parallel(x1,y1,x2,y2,x3,y3,x4,y4,xi,yi,xalt,yalt,distance,distance_q,8265.,cos_angle_tolerance,dist_tolerance);
   dbms_output.put_line('Result ' || result || ' D ' || round(distance,8) || ' , ' || round(distance_q,8));
   dbms_output.put_line('xii ' ||xi || ',' ||yi || ' alt ' || xalt || ' , ' || yalt);
END try_line_parallel;
--
PROCEDURE try_point_in_poly_cc(ingeom MDSYS.SDO_GEOMETRY default NULL,px number default NULL,py number default NULL) as
-- Set up the xys so there is a simple rectangle with a nasty zigzag on the top
   Ord       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(2,0,10,4.0125,8,8,3,5.6,4,6,0,4,2,0);
   PolyXys   MDSYS.SDO_ORDINATE_ARRAY;
   geom      MDSYS.SDO_GEOMETRY;
   dist      MDSYS.SDO_LIST_TYPE;
   inside    PLS_INTEGER;
   x         NUMBER;
   y         NUMBER;
BEGIN

   if ingeom is not NULL then
      geom := ingeom;
   else
      geom := SDO_GEOMETRY(2003, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 1003,1),Ord);
   end if;

   if px is not NULL then
     x := px;
     y := py;
   else
     x := 9.9; --3.5;   30 points needed and 15 for 2nd
     y := 3.92; -- 5.73;
   end if;
   PolyXys := geom.sdo_ordinates;
   for ii in 1..30 loop
      inside := POINT_IN_POLY_CC(x,y,Polyxys);
      dbms_output.put_line('inside ' || inside || ' y ' || y);
      y := y + 0.01;
   end loop;
END try_point_in_poly_cc;
--
FUNCTION try_boyce(ingeom MDSYS.SDO_GEOMETRY default NULL) RETURN NUMBER as
   polyXys      MDSYS.SDO_ORDINATE_ARRAY;
   polyXys2003      MDSYS.SDO_ORDINATE_ARRAY;
-- This is a skinny rectangle
--   Xys2     MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(1,1,2,1,2,5,1,5,1,1);
 --  Xys2     MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(1,1,2,1,2,5,1,5,1,1);
      Xys2     MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(1,1,6,1,20,5,1.5,20,1,5.0,1,1);
   Info      MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1);
   Info2003      MDSYS.SDO_ELEM_INFO_ARRAY;
   radii    MDSYS.SDO_LIST_TYPE;
   mu_matrix  MDSYS.SDO_LIST_TYPE;
   axes           MDSYS.SDO_LIST_TYPE;
   areas     mdsys.sdo_list_type;
   geometry     MDSYS.SDO_GEOMETRY;
   xc       NUMBER;
   yc       NUMBER;
   ring     NUMBER :=1;
   inside   PLS_INTEGER;


   boyce_shape NUMBER;
   sum_radii NUMBER;
   bias      NUMBER;
   rings     NUMBER;
   jj        PLS_INTEGER;
BEGIN

    rings :=Getnumrings(ingeom,'outer');
    PolyXys := ingeom.sdo_ordinates;
    for ir in 1..rings loop
    geometry := GET_OUTER_RING(ingeom,ring);
    Info2003 := geometry.sdo_elem_info;
    PolyXys2003 := geometry.sdo_ordinates;

    areas := Centroid(PolyXys2003,Info2003,xc,yc);
--  if ir = 1 then
--      areas := Centroid(PolyXys,Info,xc,yc);
      inside := POINT_IN_POLY_CC(xc,yc,Polyxys2003);

      if inside = 0 then
        Interior_point(PolyXys2003,Info,xc,yc);
--        dbms_output.put_line('after interior point xc ' || xc || ' yc ' || yc );
      end if;
--    end if;
    inside := POINT_IN_POLY_CC(xc,yc,Polyxys2003);

--    dbms_output.put_line('xc ' || xc || ' yc ' || yc || ' inside ' || inside);
--    mu_matrix := Moments(PolyXys2003,Info2003,12,'TRUE');

--    axes := measure_elongate_length(ingeom,xc,yc,radii);
--   for ii in 1..axes.count loop
--     dbms_output.put_line('ii ' || ii || ' axes ' || ROUND(axes(ii),8));
--   end loop;


--  if inside = 0 then
--    RETURN NULL;
--  end if;
  radii := radial_distances(PolyXys2003,xc,yc,16,NULL);
  for ii in 1..radii.count loop
     dbms_output.put_line(ii || ' D ' || ROUND(radii(ii),4));
  end loop;
  end loop;
  sum_radii := 0.0;
  boyce_shape := 0.0;

  For ii in 1.. radii.count Loop
    sum_radii := sum_radii + abs(radii(ii));
  End Loop;

--     dbms_output.put_line('sum ' || round(sum_radii,8));

  boyce_shape := -1.;  -- not calculated
  bias := 100./radii.count;   -- 6.25 for 16


     jj := 1;
     For ii in 1.. radii.count Loop
       boyce_shape := boyce_shape + abs(abs(radii(jj))/sum_radii*100.-bias);
       jj := jj + 1;
     End Loop;
     Boyce_shape := ROUND(boyce_shape,4);

  RETURN Boyce_shape;
END try_boyce;
--
PROCEDURE try_moments(ingeom MDSYS.SDO_GEOMETRY default NULL) as
-- This uses a rectangle that is oriented at 26.565.. degrees so tan0 - 0.5.
-- This rectangle is useful because it is possible to intersect the sides at
-- a point with just a few decimals.
   Ord       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(2,0,10,4,8,8,0,4,2,0); --,2,0);
   Ord2      MDSYS.SDO_ORDINATE_ARRAY;
   geom      MDSYS.SDO_GEOMETRY;
   next_geom MDSYS.SDO_GEOMETRY;
   Info      MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1);
   mus       mdsys.sdo_list_type;
   eigens    mdsys.sdo_list_type;
   angle     NUMBER;
BEGIN

   if ingeom is not NULL then
      geom := ingeom;
      Info := geom.sdo_elem_info;
   end if;

    dbms_output.put_line('..');

    Ord2 :=  MDSYS.SDO_ORDINATE_ARRAY(-122.657878656454,47.9311406639453,-122.657802, 47.931107,-122.657702404526, 47.9311260697458,-122.657722, 47.931156,-122.657878656454, 47.9311406639453, -122.65,47.93,-122.649,47.93,-122.649,47.931,-122.65,47.931,-122.65,47.93);
    Ord2 := mdsys.sdo_ordinate_array(-97.492266, 44.543884, -96.885504, 44.543884,  -96.885504, 45.150224,  -97.492266, 45.150224, -97.492266, 44.543884);
-- caused problems without removal of xys(1),xys(2)
-- xys   mdsys.sdo_ordinate_array(-97.492266, 44.543884, -96.885504, 44.543884,  -96.885504, 45.150224,  -97.492266, 45.151631, -97.492266, 44.543884);

    Ord2  := MDSYS.SDO_ORDINATE_ARRAY(2,0,6,4,4,6,0,4,2,0);
   for ii in 1..5 loop
       dbms_output.put_line('x ' || round(ord2(ii*2-1),8) || ' y ' || round(ord2
       (ii*2),8));
    end loop;
    dbms_output.put_line('..');
    angle := 45.;
    Ord := rotate_coordinates(Ord2,angle,Ord2(1),Ord2(2));
    for ii in 1..5 loop
       dbms_output.put_line('x ' || round(ord(ii*2-1),8) || ' y ' || round(ord(ii*2),8));
    end loop;
    angle := find_attitude(Ord);
    dbms_output.put_line('found attitude ' || ROUND(angle,4));
--    mus := moments(Ord,Info,12);
--    for ii in 1..mus.count loop
--       dbms_output.put_line('ii ' || ii || ' muz ' || mus(ii));
--    end loop;
--    eigens := Eigenvectors(mus);
--    angle := eigens(7);
END;
--
PROCEDURE try_centroid(ingeom MDSYS.SDO_GEOMETRY default NULL) as
-- This uses a rectangle that is oriented at 26.565.. degrees so tan0 - 0.5.
-- This rectangle is useful because it is possible to intersect the sides at
-- a point with just a few decimals.
   Ord       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(2,0,10,4,8,8,0,4,2,0); --,2,0);
   Ord2      MDSYS.SDO_ORDINATE_ARRAY;
   geom      MDSYS.SDO_GEOMETRY;
   next_geom MDSYS.SDO_GEOMETRY;
   Info      MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1);
   lt        mdsys.sdo_list_type;
   areas     mdsys.sdo_list_type;
   area      NUMBER;
   xc        NUMBER;
   yc        NUMBER;
   rings     NUMBER;
   gtype     NUMBER := 2003.;
   elem_no   NUMBER := 1;
BEGIN

   if ingeom is not NULL then
      geom := ingeom;
      Info := geom.sdo_elem_info;
   else
-- Preamble: Show area of a triangle first:
    dbms_output.put_line('We all know that the base of a triangle is 1/2bh');
    dbms_output.put_line('So let us test the triangle (0,0 6,0, 0,12 0,0)');
    dbms_output.put_line('expected area = 36, centroid at 2,4');
    Ord2 :=  MDSYS.SDO_ORDINATE_ARRAY(6,0,0,12,0,0,6,0);
    areas := centroid(Ord2,Info,xc,yc);
    lt := moments(Ord,Info);
-- The expected area is 0.5 * base * height
     dbms_output.put_line('area ' || areas(2) || ' xc ' || xc || ' yc ' || yc);
-- Now set up a rectangle with a hole
     For ii in 1..3 loop
       if ii = 2 then
--          Info := MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1,11,2003,1);
          Ord  := MDSYS.SDO_ORDINATE_ARRAY(1,1,2,1,2,2,3,2,3,3,1,3,1,1);
--          Ord  := MDSYS.SDO_ORDINATE_ARRAY(2,0,10,4,8,8,0,4,2,0, 3,2,2,4,6,6,7,4,3,2);
--            Ord  := MDSYS.SDO_ORDINATE_ARRAY(1,1,5,1,5,3,1,3,1,1, 2,1.5,2,2.5,4.5,2.5,4.5,1.5,2,1.5);
       elsif ii = 3 then
                Info := MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1,11,2003,1);
       Ord  := MDSYS.SDO_ORDINATE_ARRAY(1,1,3,1,3,3,1,3,1,1, 2,1,2,2,3,2,3,1,2,1);
          Info := MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1,11,2003,1,21,1003,1);
          Ord  := MDSYS.SDO_ORDINATE_ARRAY(2,0,10,4,8,8,0,4,2,0, 3,2,2,4,6.8,6.4,7.8,4.4,3,2, 8,0.5,10,0.5,10,2,8,2,8,0.5);
--         gtype := 2007.;
         geom := mdsys.sdo_geometry(2003,8265,null,mdsys.sdo_elem_info_array(1,1003,1,11,2003,1),mdsys.sdo_ordinate_array(2.,0.,10.,4.,8.,8.,0.,4.,2.,0.,   3,2,2,4,4,5,5,3,3,2));
         ord := geom.sdo_ordinates;
       end if;
--     geom := SDO_GEOMETRY(gtype, 8265, NULL, Info, Ord);

     areas := centroid(Ord,Info,xc,yc);
-- The expected area is 40, xc=5, yc=4
--       dbms_output.put_line('expected area = 40, centroid at 5,4');
       dbms_output.put_line('area ' || areas(1) || ' xc ' || xc || ' yc ' || yc);
     End Loop;

   end if;


   rings := getnumrings(geom);
   dbms_output.put_line('rings ' || rings);
   While elem_no <> 0 Loop
      next_geom := GET_OUTER_RING(geom,elem_no);

      Info := next_geom.sdo_elem_info;
      Ord := next_geom.sdo_ordinates;
      areas := centroid(Ord,Info,xc,yc);
-- The expected area is 40, xc=5, yc=4
      if ingeom is NULL then
       dbms_output.put_line('expected area = 40, centroid at 5,4');
      end if;
      dbms_output.put_line('area ' || areas(1) || ' xc ' || xc || ' yc ' || yc);
   end loop;
END try_centroid;
--
PROCEDURE TRY_FASTER_ATAN2(yy NUMBER,XX NUMBER) AS
  deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
  rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
  x          NUMBER;
  y          NUMBER;
  big        NUMBER := 0.0;
  bigrel     NUMBER := 0.0;
  bad        PLS_INTEGER;
  badrel     PLS_INTEGER;
  Oatan      NUMBER;
  atanx      NUMBER;
  bias       NUMBER := 0.0;
  err        NUMBER := 0.0;
  time1      date;
  ij         NUMBER;
  tot        NUMBER := 0.0;
--==============================================================================
    Function RPAD(input NUMBER,width NUMBER) RETURN VARCHAR2 AS
-- Rigth pad input with blanks (if necessary) so it has a certain width
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
-- Rigth pad input with blanks (if necessary) so it has a certain width
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
-- dbms_output.put_line('ANGLE        TANGENT    FAST ATAN2           ATAN2       DIFF (DEGREES)');
 For J in 1..2 Loop
  for ii in 0..180 loop
       ij := ii*0.5;
       if j = 2 then
         ij := ii*0.5 + 180.;
       end if;
--       y := abs(tan(ij*deg2rad));
y := ij;
       x := 1.;
       if j = 1 then
          if ii > 90. then
             x := -1.;
          end if;
       else
         y := -abs(y);
         if ij < 270 then
           x := -1.;
         else
          x := 1.;
         end if;
         bias := 180.;
       end if;
--       dbms_output.put_line('y ' || y || ' x ' || x);
       atanx := GZ_QA.faster_atan2(y,x);
--       Oatan := atan2(y,x);
--      Just to test degrees = TRUE
/*       atanx := GZ_qa.faster_atan2(y,x,TRUE); --
       dbms_output.put_line(atanx);
       atanx := atanx *deg2rad;
       if j = 2 then
          atanx := atanx - 2*acos(-1);
       end if;
*/
--       atanx := GZ_UTIL_ZONE.fast_ATAN2(x,y);
       Oatan := atan2(y,x);
--      sinx := gz_util_zone.sincos(x,cosx);
      err := err+ (abs(Oatan-atanx));
      dbms_output.put_line('err ' || err || ' y ' || y || ' x ' || x);
      if abs(atanx- Oatan) > big then
        big := abs(atanx-Oatan);
        bad := ii;
     end if;

      if Oatan <> 0. and abs(1. -atanx/Oatan) > big then
        bigrel := abs(1. -atanx/Oatan);
        badrel := ii;
     end if;
 --    if Oatan <> 0. then
 --    dbms_output.put_line('ang ' || RPAD(ROUND(ij+bias,6),6)||' ii '  || Rpad(y,8) || ' Q ' || RPAD(round(atanx,11),13) || '  a ' || RPAD(round(Oatan,11),13)  || ' diff ' || RPADV(TO_CHAR(round((Oatan-atanx)*rad2deg,20),'99.999EEEE'),23));
 --    end if;

 end loop;
END Loop;
 err := err /3602.;
 dbms_output.put_line('Average error' || To_char(round(err,22),'999.99999EEEE'));
 dbms_output.put_line('biggest absolute error' || RPADV(To_char(round(big,22),'999.99999EEEE'),24) || ' bad ' || bad);
 dbms_output.put_line('biggest relative error' || RPADV(To_char(round(bigrel,22),'999.99999EEEE'),24) || ' bad ' || badrel);
 dbms_output.put_line((current_timestamp-time1));

END TRY_FASTER_ATAN2;
--
PROCEDURE try_get_GCds(ingeom MDSYS.SDO_GEOMETRY default NULL,smallest VARCHAR2 default 'SHORT') as
-- Set up the xys so they are fairly recognisable, .1 degree apart
-- Get_GCds (distances) get either the total 'TOTAL', or the shortest 'SHORT' or the longest 'LONG'
   Ord       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(-120.,0.,-119.99,0.,-119.99,0.01);
   geom      MDSYS.SDO_GEOMETRY;
   dist      MDSYS.SDO_LIST_TYPE;
   Big_or_small VARCHAR2(8) := 'Smallest';
BEGIN

   if ingeom is not NULL then
      geom := ingeom;
   else
      geom := SDO_GEOMETRY(2002, 8265, NULL, SDO_ELEM_INFO_ARRAY(1,2,1),Ord);
       dbms_output.put_line('Distance 1 is along the equator (0.1) degrees and ' ||
                            'Distance 2 is along the Meridian (0.1) degrees');
       dbms_output.put_line('Total distance is the sum of the Great circle distances from beginning to end');
   end if;

   execute immediate 'Select GZ_qa.get_GCds(:1,''TOTAL'') from dual' into dist using geom;
-- First get all the distances
   For ii in 1..dist.count loop
      If ii = 1 then
        dbms_output.put_line('Total of GC distances ' ||dist(ii));
      else
        dbms_output.put_line('distance ' || (II-1) || ' ' || dist(ii));
      end if;
   end loop;
dbms_output.put_line('----------------');
if smallest = 'SHORT' then
   dbms_output.put_line('Now get the shortest distance along the edge');
elsif smallest = 'LONG' then
   dbms_output.put_line('Now get the longest distance along the edge');
   Big_or_small := 'Longest';
else
   dbms_output.put_line('Now get the distances ' || smallest || ' along the edge');
end if;
   execute immediate 'Select GZ_qa.get_gcds(:1,:2) from dual' into dist using geom,smallest;
   if SUBSTR(smallest,1,1) = '<' then
    For ii in 2..dist.count loop
      If MOD(ii,2) = 1 then
        dbms_output.put_line('Segment ' ||dist(ii));
      else
        dbms_output.put_line('Distance ' || dist(ii));
      end if;
   end loop;

   else
 -- Then get the shortest one and report its location and coordinates
   For ii in 1..dist.count loop
      If ii = 2 then
        dbms_output.put_line('Segment ' ||dist(ii));
      elsif ii = 1 then
        dbms_output.put_line(Big_or_small|| ' distance ' ||  dist(ii));
      else
        dbms_output.put_line('coordinate ' || (II-2) || ' ' || dist(ii));
      end if;
   end loop;
   end if;
END try_get_gcds;
--
PROCEDURE try_find_xory(Geom MDSYS.SDO_GEOMETRY default NULL, px NUMBER default NULL,py NUMBER default NULL)
AS
     Xys  MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(-122.40576, 48.2521933, -122.40575, 48.252177, -122.4015, 48.254234, -122.40576, 48.2521933);
     pos  NUMBER;
     x    NUMBER;
     y    NUMBER;
     s    NUMBER := 0.1;
BEGIN
     if px is NULL then
        x := -122.40575*s + (1.-s) *-122.4015 ;
        y := 48.252177*s  + (1.-s) * 48.254234 ;
        x := -122.4015*s + (1.-s) *-122.40576 ;
        y := 48.254234*s  + (1.-s) * 48.2521933 ;
     end if;
     pos := find_xory(x,y,Xys,1.E-6);
     dbms_output.put_line('pos ' || pos);
END try_find_xory;
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
  for ii in 0..200 loop
       if ii = -126 then
          x := -4.*acos(-1.);
       elsif ii = 126 then
          x := 4*acos(-1.);
       else
          x := ii/100.;
       end if;
       sinx := GZ_qa.quick_sincos(x,cosx);

--      sinx := gz_util_zone.sincos(x,cosx);

      if abs(sinx- sin(x)) > big then
        big := abs(sinx-sin(x));
        bad := ii;
     end if;

      if sin(x) <> 0. and abs(1. -sinx/sin(x)) > big then
        bigrel := abs(1. -sinx/sin(x));
        badrel := ii;
     end if;
     if sin(x) <> 0. then
     dbms_output.put_line('angle ' || RPAD(ROUND(x*rad2deg,1),6) ||' ii '  || Rpad(x,4) || ' Qsine ' || RPAD(round(sinx,10),12) || '  sin ' || RPAD(round(sin(x),10),12)  || ' diff ' || RPADV(TO_CHAR(round(1.-sinx/sin(x),20),'99.999EEEE'),23));
     end if;
 end loop;
 dbms_output.put_line('biggest absolute error' || RPADV(To_char(round(big,22),'999.99999EEEE'),24) || ' bad ' || bad);
 dbms_output.put_line('biggest relative error' || RPADV(To_char(round(bigrel,22),'999.99999EEEE'),24) || ' bad ' || badrel);
 dbms_output.put_line((current_timestamp-time1));

END TRY_QUICK_SINCOS;
--
PROCEDURE try_rotate_coordinates(angle NUMBER default 90.) AS
-- Unit_test for rotate_coordinates that shows how coordinates
-- Rotate a rectangle oriented at 26.565..... degrees counter clockwise by
-- 90 degrees anticlockwise and back and check differences.
  Arr      MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(2,0,10,4,8,8,0,4,2,0);
  New_Arr  MDSYS.SDO_ORDINATE_ARRAY;
  n        PLS_INTEGER ;
BEGIN

 dbms_output.put_line('START');
 dbms_output.put_line('If you take a quick look at these coordinates you will see' ||
                      ' a box lying on its side twice as long as it is high');
dbms_output.put_line('.');
dbms_output.put_line('.   Vertex shown as +                        + (8,8)');
dbms_output.put_line('.                                     .       \');
dbms_output.put_line('.                               .              \');
dbms_output.put_line('.                    |     .                    \');
dbms_output.put_line('.               (0,4)+                           + (10,4)');
dbms_output.put_line('.                    | \                  .');
dbms_output.put_line('.                    |  \           .');
dbms_output.put_line('.                    |   \     .');
dbms_output.put_line('.                    |____+_________');
dbms_output.put_line('.                          (2,0)');
 dbms_output.put_line('SELECT MDSYS.SDO_GEOMETRY(2003,8265,NULL,MDSYS,SDO_ELEM_INFO_ARRAY(1,1003,1),' ||
                      'MDSYS.SDO_ORDINATE_ARRAY(2,0,10,4,8,8,0,4,2,0)) FROM DUAL;');
  n := TRUNC(Arr.count/2);
  for ii in 1..n loop
     dbms_output.put_line('vertex ' || ii || '  x ' || ROUND(Arr(ii*2-1),8) || ' y ' || ROUND(Arr(ii*2),8));
  end loop;

  New_arr := rotate_coordinates(Arr,angle,Arr(1),Arr(2));

  dbms_output.put_line('AFTER');

  for ii in 1..n loop
     dbms_output.put_line('vertex ' || ii || '  x ' || ROUND(new_arr(ii*2-1),8) || ' y ' || ROUND(New_arr(ii*2),8));
  end loop;

  New_arr := rotate_coordinates(New_Arr,-angle,New_Arr(1),New_Arr(2));
  dbms_output.put_line('ROTATED BACK');
  for ii in 1..n loop
     dbms_output.put_line('vertex ' || ii || ' x ' || ROUND(new_arr(ii*2-1),8) || ' y ' || ROUND(New_arr(ii*2),8));
  end loop;

    dbms_output.put_line('ERRORS');
  for ii in 1..n loop
     dbms_output.put_line('vectors ' || ii || '  x ' || to_char(Arr(ii*2-1)-new_arr(ii*2-1),'9999.9999999EEEE') || ' y ' || To_char(Arr(ii*2)-New_arr(ii*2),'9999.9999999EEEE'));
  end loop;
END try_rotate_coordinates;
--
PROCEDURE try_circulate_coordinates(shift PLS_INTEGER default 2) AS

-- Unit_test for circulate_coordinates (changes the start, end of a loop).
-- User may supply the (vertex) shift and then the printout shows the new
-- positions of the coordinates relative to original start which are
-- conveniently numbered  1,2,3,4,.. So a vertex that has coordinates (3,4) was
-- the second, for example.                         + (5,6)
--                                                 /
--                 + (1,2) ---------------------- + (3.4)

    Arr      MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(1,2,3,4,5,6,7,8,9,10,11,12);
    New_Arr  MDSYS.SDO_ORDINATE_ARRAY;
    n        PLS_INTEGER := TRUNC(Arr.count/2);
BEGIN

 dbms_output.put_line('Input array is:  1,2 3,4 5,6 7,8 9,10 11,12 (=1,2)');
    for ii in 1..n loop
       dbms_output.put_line('Before ' || ii || ' x ' || arr(ii*2-1) || ' y ' || arr(ii*2));
    end loop;
    
    New_arr := circulate_coordinates(Arr,shift);
    dbms_output.put_line('Shift by ' || shift);
    
    for ii in 1..n loop
       dbms_output.put_line('After ' || ii || ' x ' || new_arr(ii*2-1) || ' y ' || New_arr(ii*2));
    end loop;
  
END try_circulate_coordinates;
--
PROCEDURE try_shellsort2 AS
-- Unit_test for shellsort2:
--   Sorts 2 arrays simultaneously using the first array as the key.
--   Then test whether the array values increase or decrease.

  Arr        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  arr2       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  time1      date;
BEGIN

  Arr.extend(100000);
  Arr2.extend(100000);
  for i in 1..Arr.count loop
     Arr(i) := Mod(Arr.count - i + 1,37813); --Mod(i,3713);
     Arr2(i) := Mod(i,5); --Mod(i,3713);
  end loop;
 time1 := current_timestamp;

 shellsort2(Arr,Arr2,1,Arr.count,'ASC');

  dbms_output.put_line('Time for sort' ||(current_timestamp-time1));
  dbms_output.put_line(Arr(1));
  for i in 2..Arr.count loop
     If i < 6 then
     dbms_output.put_line(Arr(i));
     end if;
-- Just test sorting of the first array, this shellsort does not sort the 2nd array
     if Arr(i) < Arr(i-1) then --or (Arr(i) = Arr(i-1) and Arr2(i) < Arr2(i-1)) then
       dbms_output.put_line('sorting error');
       dbms_output.put_line(arr(i) || ' ' || arr(i-1));
       dbms_output.put_line(arr2(i) || ' ' || arr2(i-1));
       exit;
     end if;
  end loop;
  dbms_output.put_line('...');
  dbms_output.put_line(Arr(Arr.count));
  for i in 1..Arr.count loop
      Arr(i) := Mod(Arr.count - i + 1,6); --Mod(i,3713);
      Arr2(i) := Mod(i,5); --Mod(i,3713);
  end loop;
  time1 := current_timestamp;
  shellsort2(Arr,Arr2,1,Arr.count,'DES');

  dbms_output.put_line('Time for sort' || (current_timestamp-time1));
  for i in 2..Arr.count loop
     if Arr(i) > Arr(i-1) then --or (Arr(i) = Arr(i-1) and Arr2(i) > Arr2(i-1)) then
       dbms_output.put_line('sorting error');
       exit;
     end if;
  end loop;
END try_shellsort2;
--
PROCEDURE try_stablesort3 AS
-- Unit_test for shellsort2:
--   Sorts 2 arrays simultaneously using the first array as the key.
--   Then test whether the array values increase or decrease.
  Arr        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  arr2       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  arr3       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  time1      date;
BEGIN

  Arr.extend(100000);
  Arr2.extend(100000);
  Arr3.extend(100000);
  for i in 1..Arr.count loop
     Arr(i) := Mod(Arr.count - i + 1,37813); --Mod(i,3713);
     Arr2(i) := Mod(i,2); --Mod(i,3713);
     Arr3(i) := i;
  end loop;
 time1 := current_timestamp;

 stablesort3(Arr,Arr2,Arr3,1,Arr.count);

  dbms_output.put_line('Time for sort' ||(current_timestamp-time1));
  dbms_output.put_line(Arr(1));
  for i in 2..Arr.count loop
     If i < 6 then
     dbms_output.put_line(Arr(i));
     end if;
-- Just test sorting of the first array, this shellsort does not sort the 2nd array
     if Arr(i) < Arr(i-1) or (Arr(i) = Arr(i-1) and Arr2(i) < Arr2(i-1)) then
       dbms_output.put_line('sorting error');
       dbms_output.put_line(arr(i) || ' ' || arr(i-1));
       dbms_output.put_line(arr2(i) || ' ' || arr2(i-1));
       exit;
     end if;
  end loop;
  dbms_output.put_line('...');
  dbms_output.put_line(Arr(Arr.count));
  for i in 1..Arr.count loop
      Arr(i) := Mod(Arr.count - i + 1,3713); --Mod(i,3713);
      Arr2(i) := Mod(i,3); --Mod(i,3713);
  end loop;
  time1 := current_timestamp;
  stablesort3(Arr,Arr2,Arr3,1,Arr.count,FALSE);

  dbms_output.put_line('Time for sort' || (current_timestamp-time1));
  for i in 2..Arr.count loop
     if Arr(i) > Arr(i-1) or (Arr(i) = Arr(i-1) and Arr2(i) > Arr2(i-1)) then
       dbms_output.put_line('sorting error');
       exit;
     end if;
  end loop;
END try_stablesort3;
--
PROCEDURE try_area_of_line(Geom MDSYS.SDO_GEOMETRY default NULL)
AS
--     Xys  MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(-120.9, 45, -121, 45, -121, 45.1, -121.2, 45.2,-121.3,45.3);
--     Xys  MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(-120.9, 45, -121, 45, -121, 45.1, -121.2, 45.2,-121.3,45.3,-121.2,45.,-120.9,45);
--  Xys  MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(-148.130235999999996465703588910400867462,61.8179310000000015179466572590172290802,-148.12684100000001308217179030179977417,61.8238940000000027907844923902302980423,-148.128167999999988069248502142727375031,61.82728199999999674218997824937105178833,-148.135468000000003030436346307396888733,61.82871899999999953934093355201184749603);
  Xys  MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(-134.583965000000006284608389250934123993,58.39127299999999820556695340201258659363,-134.583956999999998060957295820116996765,58.39136599999999788224158692173659801483,-134.583402000000006637492333538830280304,58.39137199999999694455254939384758472443);
     area_found  NUMBER;
     way_point   NUMBER;
BEGIN
     if Geom is NOT NULL then
       Xys := Geom.sdo_ordinates;
     end if;
    Xys := MDSYS.SDO_ORDINATE_ARRAY(0,0,.005,.005,.015,-.005,.020,0);
     area_found := area_of_line(Xys);
--     area_found := area_of_loop(Xys,TRUE,way_point,8265.);
     dbms_output.put_line('area ' || round(area_found,3));
--     area_found := area_of_loop(Xys,TRUE,way_point,8265.);
--     dbms_output.put_line('area ' || area_found);
END try_area_of_line;
--
PROCEDURE TRY_FASTER_ATAN2 AS
  x    NUMBER;
  xx   NUMBER;
  big NUMBER :=0.0;
  bad PLS_INTEGER;
  time1      date;

BEGIN

 time1 := current_timestamp;

--   x := new_arctan(0.5773502691896257645091487805019574556545,1.);
--   x := new_arctan(0.5773502691996257645091487805019574556545,1.);
 --  dbms_output.put_line('x ' || x);
--   dbms_output.put_line('A ' || atan2(0.5773502691896257645091487805019574556545,1.));
--   x := new_arctan(0.2679491924311227064725536584941276331,1.);
--   dbms_output.put_line('x ' || x );
--   dbms_output.put_line('A ' || atan2(0.2679491924311227064725536584941276331,1.));
 for ii in 1..10000 loop

--      X := GZ_UTIL_ZONE.fast_ATAN2(1.,ii/1000.);
         xx := ii/10000.;
       X := gz_qa.faster_ATAN2(xx,1.);

--        X := new_arctan(1.,xx);
--        x := atan2(1.,xx);

      if abs(1.- x/atan2(xx,1.)) > big then
        big := abs(1.- x/atan2(xx,1.));
        bad := ii;
      end if;
     if mod(ii,100) = 0 then
     dbms_output.put_line('ii '  || (xx) || ' uhk ' || round(x,20) || '  atan ' || round(atan2(xx,1.),20)  || ' diff ' || to_char(round(1.-x/atan2(xx,1.),24),'999.99EEEE')); -- || ' diff ' || round((atan2(ii/100.,1.)-x)/(ii*ii),10));
     end if;

   end loop;
 dbms_output.put_line('biggest ' || to_char(round(big,30),'999.99999EEEE') || ' bad  ' || bad);
 dbms_output.put_line((current_timestamp-time1));
END TRY_FASTER_ATAN2;
--
PROCEDURE try_angle AS

rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   angle_found number;
   x0          number :=0.0;
   y0          number :=0.0;
   x1          number :=-1.0;
   y1          number :=0.0;
   x2          number :=-0.0001;
   y2          number :=0.1;
   xp          number;
   yp          number;
   xq          number;
   yq          number;
   cpx         number;
   cpy         number;
   result     MDSYS.SDo_LIST_TYPE;
BEGIN
    result := divergence(x0,y0,x1,y1, x0+1,y0+1,x2+1,y2+1,cpx, cpy,100.,xp,yp,xq,yq);
    angle_found := result(1);
    dbms_output.put_line('AF ' || round(angle_found,4) || ' D1 ' || round(result(2),4) || ' D2 ' || round(result(3),4));
END;
FUNCTION try_Get_pipe_Geom(width number default 100.) return mdsys.sdo_geometry AS

   Widths  mdsys.sdo_list_type;
   PSL     mdsys.sdo_list_type := mdsys.sdo_list_type();
   geom    mdsys.sdo_geometry;
   Xys     mdsys.sdo_ordinate_array;
   Info    mdsys.sdo_elem_info_array;
   
   start_vertex   pls_integer := 98; --144;
   across_vertex  pls_integer := 107; --8;
   sql_stmt VARCHAR2(4000);
   
   avg_width       number;
   accum_length    number;
BEGIN

   sql_stmt := 'SELECT sdogeometry from GZCPB2.V699TM_MERGE_FACE where face_id =910'; --435'; --2161';
   execute immediate sql_stmt into geom;
   
   Xys := geom.sdo_ordinates;
   Info := geom.sdo_elem_info;
   
   start_vertex := 27; --114; --144;
   across_vertex :=29; --151; --8;
   
--   start_vertex := 18;
--   across_vertex :=20;
   start_vertex := 29; --98;
   across_vertex := 31; --107;
    start_vertex :=8; --218;
   across_vertex :=144; --220;
   start_vertex := 50;
   across_vertex :=52;
    start_vertex := 97;
   across_vertex :=99;
  geom := get_Pipe_Geom(Xys,Info,start_vertex,across_vertex,4,width,PSL,avg_width,accum_length);

  RETURN geom;
END;
END GZ_QA;
/

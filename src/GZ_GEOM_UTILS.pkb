CREATE OR REPLACE PACKAGE BODY GZ_GEOM_UTILS
AS

   --
--
PROCEDURE RESHAPE_LEDGE(State VARCHAR2,ptopology VARCHAR2, face_no NUMBER, pUS_STATE_table VARCHAR2 default 'ACS09_SL040',pEntityfp VARCHAR2 default 'STATEFP') AS

-- A procedure to reshape an "L" shaped edge that touches another edge in the same face.
--                     edge_id tobe reshaped
--            \      /
--  --------   \    /-------------   nearby edge
--              \  /
--                +

-- Use this call: SQL> exec gz_utilities.reshape_Ledge('04','Z604LS3',face_no);
-- where face number is an actual face with a 13349 error.

  sql_stmt                        VARCHAR2(4000);

  vstate                          VARCHAR2(2) := State;
  Topology                        VARCHAR(20) := UPPER(pTopology);
  US_State_table                  VARCHAR2(30) := UPPER(pUS_STATE_table);
  status                          VARCHAR2(4000);
  new_geometry                    MDSYS.SDO_GEOMETRY;
  xLL                             NUMBER;
  yLL                             NUMBER;
  xUR                             NUMBER;
  yUR                             NUMBER;
  edge_id                         NUMBER;
  delta                           NUMBER := 0.001;
  mbr_geom                        SDO_GEOMETRY;

  topo_entity_count               NUMBER := 100000;
  cache_name                      VARCHAR2(20) := 'my_topo_map_cache';
BEGIN


    sdo_topo_map.set_max_memory_size (2147483648);
    BEGIN
        sdo_TOPO_MAP.CREATE_TOPO_MAP(Topology, cache_name, topo_entity_count,topo_entity_count,topo_entity_count);
         EXCEPTION
         WHEN OTHERS THEN

            IF SQLCODE = -29532
            AND Instr(sqlerrm,' a TopoMap by the same name already exists') != 0
            THEN
               sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);
               sdo_TOPO_MAP.CREATE_TOPO_MAP(Topology, cache_name, topo_entity_count,topo_entity_count,topo_entity_count);
            END IF;
    END;

        dbms_output.put_line(' processing state ' || vstate);

        sql_stmt := 'SELECT /*+ PARALLEL 4 */ sdo_geom.sdo_mbr(sdo_geom.sdo_buffer(sdo_aggr_mbr(sdogeometry), 500, .05))
                     FROM '||US_State_Table||' a WHERE a.'|| pEntityfp ||' = :1';

        Execute Immediate sql_stmt into mbr_geom using vstate;


        xLL := mbr_geom.sdo_ordinates(1)-delta;
        yLL := mbr_geom.sdo_ordinates(2)-delta;
        xUR := mbr_geom.sdo_ordinates(3)+delta;
        yUR := mbr_geom.sdo_ordinates(4)+delta;


        sdo_TOPO_MAP.LOAD_TOPO_MAP(cache_name,xLL,yLL,xUR,yUR, 'true');


        status := SDO_TOPO_MAP.VALIDATE_TOPO_MAP(cache_name);
        dbms_output.put_line('VALIDATE TOPO MAP:' ||status);

--  Get an improved "L" shaped edge that does not intersect the nearby edge

        new_geometry := get_nearest_edge(Topology,face_no,edge_id);

        if new_geometry is NULL then
          status := 'No close edge found';
        else
          status := swap_edge_coords(Topology,edge_id,new_geometry);
        end if;

        if status <> 'TRUE' then
          dbms_output.put_line('SWAP EDGE FAILED ' || status);
        else
          sdo_TOPO_MAP.COMMIT_TOPO_MAP();
          dbms_output.put_line('TOPO MAP COMMITTED');
        COMMIT;
        end if;
        sdo_TOPO_MAP.CLEAR_TOPO_MAP(cache_name);

        sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);

        sql_stmt := 'SELECT sdo_geom.validate_geometry_with_context(t.topogeom.get_geometry(),0.05) from '||
                     Topology||'_clip_face t where t.face_id=:1';

        execute immediate sql_stmt into status using face_no;
        dbms_output.put_line('FACE VALIDATION STATUS IS:' || status);

        return;


END RESHAPE_LEDGE;
--
FUNCTION SWAP_EDGE_COORDS(Topology VARCHAR2,Edge_id NUMBER, Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY) RETURN VARCHAR2 AS
/**
 ################################################################################
 # Program Name: swap_edge_coords
 # Author: Sidey Timmins
 # Creation Date: 2010
 #
 # Usage:
 #   This program has 3 required parameters :
 #
 #     REQUIRED Parameter:
 #        Topology            - The name of the Topology.
 #        Edge_id             - The edge_id to reshape
 #        Geometry            - the revised geometry to replace the existing one
 # Purpose:
 #        Update a topology by reshaping a single edge
 # Modification History:
 #        10/21/2010          Niranjan Reddy suggested using a different entry point
 #                            with 3 extra arguments to avoid Oracle bug SR 3-2169673431
 #        01/2011             Changed allow_iso_moves to TRUE to avoid yet another Oracle bug
 #        Feb 2011            BACK to false!!
 ################################################################################
*/
     status            VARCHAR2(4000);
     v_errm            VARCHAR2(4000);
     v_code            NUMBER;
     moved_iso_nodes   SDO_NUMBER_ARRAY;
     moved_iso_edges   SDO_NUMBER_ARRAY;
     Xys              MDSYS.SDo_ordinate_array;
     TopoXys          MDSYS.SDo_number_array;
     allow_iso_moves   VARCHAR2(5) := 'FALSE';

 -- In the specified topology, replace one edge's geometry with the input geometry.
 -- Called by simplify_region, swap_one_edge

BEGIN

 --  Perform the swap within an exception begin ..end clause
      BEGIN
--      Without theses extra parameters, we got Oracle errors
        SDO_TOPO_MAP.CHANGE_EDGE_COORDS(NULL,EDGE_ID,Geometry,moved_iso_nodes,moved_iso_edges,allow_iso_moves);


--dbms_output.put_line(' CHANGED edge coordinate ' || edge_id);
-- If there is an error then return the status
        RETURN 'TRUE';
        EXCEPTION
          WHEN OTHERS THEN
                IF (INSTR(sqlerrm, 'with an edge ID that does not exist in cache') != 0) or
                (INSTR(sqlerrm, 'Attempted change edge operation outside the update window') !=0)
                THEN
                  DBMS_OUTPUT.PUT_LINE('in handler' || substr(sqlerrm,1,100));
                ELSE
                  v_code := SQLCODE;
                  v_errm :=  SQLERRM;

                END IF;
                RETURN SUBSTR(sqlerrm,1,300);
       END;

END SWAP_EDGE_COORDS;
--
FUNCTION ACCURATE_GCD( x1 IN OUT NOCOPY NUMBER, y1 IN OUT NOCOPY NUMBER,
                       x2 IN OUT NOCOPY NUMBER, y2 IN OUT NOCOPY NUMBER)
                       RETURN NUMBER DETERMINISTIC IS
/*
********************************************************************************
--Program Name: accurate_gcd
--Author: Sidey Timmins
--Creation Date: 03/17/2010
--Updated: 10/29/2010 To calculate its own sincos and to better match Charles
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


-- Jack Ganssle Cosine coefficients good to 14.7 digits
   cc1       NUMBER := 0.99999999999999806767;
   cc2       NUMBER :=-0.4999999999998996568;
   cc3       NUMBER := 0.04166666666581174292;
   cc4       NUMBER :=-0.001388888886113613522;
   cc5       NUMBER := 0.000024801582876042427;
   cc6       NUMBER :=-0.0000002755693576863181;
   cc7       NUMBER := 0.0000000020858327958707;
   cc8       NUMBER :=-0.000000000011080716368;

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
  cosdy       NUMBER;
  sindy       NUMBER;
  p           PLS_INTEGER :=1;
  delta       NUMBER;
  yfactor     NUMBER :=1.;
  dist_factor NUMBER := 111319.490793274;
  sindelta    NUMBER;
BEGIN

      dx := x2-x1;
      dy := y2-y1;
      yy := y*deg2rad;
      yy2 := yy*yy;
      IF yy < 1.5E-3 THEN

 -- Use Horner's rule to evaluate the sine

       siny :=  yy*(yy2*(yy2*(yy2*(yy2 *(-2.39E-08 * x2 + 2.7526E-06) - 1.98409E-04) + 8.3333315E-03)
           -1.666666664E-01) + 1.0);
       cosyy := sqrt(1.-siny*siny);
     ELSE
  -- Evaluate the cosine
       cosyy := cc1 + yy2*(cc2 + yy2*(cc3 + yy2*(cc4 + yy2*(cc5 +yy2*(cc6 + yy2*(cc7 + yy2*cc8))))));
       siny := sqrt(1.-cosyy*cosyy);
     END IF;

      delta := (y2-y1)*0.5*deg2rad;           -- calculate sin((y1+y2)*0.5)
      cosdy := 1. - delta*delta*0.5;           -- using sin(a+eps) = sin(a)cos(eps) + cos(a)sin(eps)
      sindelta := delta - delta*delta*delta/6.;
      siny2 := siny*cosdy + cosyy*sindelta;      -- small angle approximation for formula above
--      dbms_output.put_line('siny2 ' || siny2 || ' y2 ' || y2);
 --     dbms_output.put_line('Siny2 ' || (sin((y1+y2)*0.5*deg2rad)) || ' y2 ' || y2);

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
   tt := (y-t);
   dy := dy*(a1+ a2*siny2*siny2 + tt*tt*b);

-- Looks like Pythagoras but actually is a great-circle formula modified
-- for small angles (whereby sin(theta) ~= theta ~= arcsin(theta)).
--  See reference
-- for 2 points: (lambda1,theta1) and (lambda2,theta2) (longitude,latitude respectively)
--                dlambda apart and dtheta apart then
--
-- length = radius * 2*arcsin(sqrt( sin(dtheta*0.5) * sin(dtheta*0.5) +
--                 cos(theta1)*cos(theta2) * sin(dlambda*0.5)* sin(dlambda*0.5))

-- Since the input is in degrees, then the dist_factor := radius * pi/180.

       if abs(dy) >=  0.0001 then
--          delta := (y2-y1)*deg2rad;  -- use delta from above which alter next line
--          cosdy := 1. - delta*delta*2.;
--          sindy := sqrt(1-cosdy*cosdy);
          cosy2 := cosy - siny * dy*deg2rad; -- small angle approximation for cos(y2)

-- Very bad Oracle error: ORA-03113: end-of-file on communication channel
          if  abs(dx) < 1.E-20 then
            dx :=0.0;
          end if;
--          cosy2 := cosyy*cosdy - siny * sindy;  -- this doesn't help
          gcd := sqrt(dy*dy + dx*dx*cosy*cosy2) * dist_factor;
       else
          if abs(dy) < 1.E-20 then
            dy := 0.0;
          end if;
          gcd := sqrt(dy*dy + dx*dx*cosy*cosy) * dist_factor;

       end if;

   RETURN gcd;


END ACCURATE_GCD;
--
FUNCTION FIND_CLOSEST_EDGE(pTopology VARCHAR2,face_id NUMBER) RETURN MDSYS.SDO_LIST_TYPE AS

-- Find closest edge. Note X,y should not be a node but a vertex on the edge

   Topology           VARCHAR2(30) := UPPER(pTopology);
   Edge_IDS           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Nearbys            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Edge_Xys           MDSYS.SDO_ORDINATE_ARRAY;
   Nearby_Xys         MDSYS.SDO_ORDINATE_ARRAY;
   Geom               MDSYS.SDO_GEOMETRY;
   Point              MDSYS.SDO_GEOMETRY;
   Seg                MDSYS.SDO_GEOMETRY;
   NearbyGeom         MDSYS.SDO_GEOMETRY;
   x0                 NUMBER ;
   y0                 NUMBER ;
   x1                 NUMBER ;
   y1                 NUMBER ;
   xtest1             NUMBER;
   ytest1             NUMBER;
   xtest2             NUMBER;
   ytest2             NUMBER;
   xnear              NUMBER;
   ynear              NUMBER;
   oxnear             NUMBER;
   oynear             NUMBER;
   pu                 NUMBER;
   edge_id            NUMBER;
   near_edge_id       NUMBER;
   dist               NUMBER;
   check_dist         NUMBER := 2.;
   last_dist          NUMBER := 10.;
   pos1               PLS_INTEGER;
   pos2               PLS_INTEGER;
   choice1            PLS_INTEGER;
   choice2            PLS_INTEGER;
   sql_stmt           VARCHAR2(4000);
   s   number;
   az1 number;
   az2 number;
   m12 number;
   s12 number;
   gcd_K number;
   status             VARCHAR2(4000);

BEGIN

   sql_stmt := 'select t.sdogeometry from '||Topology ||'_clip_face t where t.face_id=:1';
   execute immediate sql_stmt into Geom using face_id;

   status := sdo_geom.validate_geometry_with_context(geom,0.05);
   dbms_output.put_line(status);

-- Get the particular edges Oracle complains about
--13349 [Element <1>] [Ring <1>][Edge <7>][Edge <11>]

   pos1 := INSTR(status,'[Edge <');
   pos2 := INSTR(status,'>',pos1);
--   dbms_output.put_line('pos1:' ||pos1);
 --  dbms_output.put_line('pos2:' ||pos2);
 --  dbms_output.put_line('stuff:' || (substr(status,pos1+7,pos2-pos1-7)));
   choice1 := substr(status,pos1+7,pos2-pos1-7);
   pos1 := INSTR(status,'[Edge <',pos1+1);
   pos2 := INSTR(status,'>',pos1);
   choice2 := substr(status,pos1+7,pos2-pos1-7);

-- Hurrah. An ordered list of edges surrounding a face. negative means the
-- edge direction must be reversed to build a counter clockwise geoemtry.
   edge_ids := sdo_topo.get_face_boundary(Topology,face_id);

-- test we the problem edge second

   sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';

-- Assume we have an edge made of a number of segments
   dbms_output.put_line('choice1:'|| choice1 || ' choice2 ' || choice2);
   For ii in 1..Edge_ids.count Loop
--     if ii = choice1 or ii = choice2 then
     edge_id := ABS(Edge_ids(ii));
     dbms_output.put_line('ii ' || ii || ' edge ' || edge_id);
     EXECUTE IMMEDIATE sql_stmt INTO Geom using edge_id;
     Edge_Xys := Geom.Sdo_Ordinates;

     For ij in 1.. Edge_ids.count Loop
--       if ij = choice1 or ij = choice2 then
       near_edge_id := ABS(Edge_ids(ij));
-- Dont compare the subject edge with itself

       if edge_id <> near_edge_id then
       EXECUTE IMMEDIATE sql_stmt INTO NearbyGeom using near_edge_id;
       Nearby_Xys := NearbyGeom.Sdo_Ordinates;
       xtest2 := Nearby_Xys(1);
       ytest2 := Nearby_Xys(2);
--       check_dist := 0.000001;
--       if ytest2 > 50.0 then
--         check_dist := check_dist*3.5;
--       end if;
--       dbms_output.put_line('Nearby edge ' || near_edge_id);

       For jj in 2..TRUNC(Nearby_XYs.count/2) Loop
         xtest1 := xtest2;
         ytest1 := ytest2;

         xtest2 := Nearby_Xys(jj*2-1);
         ytest2 := Nearby_Xys(jj*2);

         x1 := Edge_xys(1);
         y1 := Edge_xys(2);
         for kk in 2..Trunc(Edge_xys.count/2) loop
           x0 := x1;
           y0 := y1;
           x1 := Edge_xys(kk*2-1);
           y1 := Edge_xys(kk*2);
           if (x0 <> xtest1 or y0 <> ytest1) and (x0 <> xtest2 or y0 <> ytest2) then
           dist := Is_pt_on_Segment(x0,y0,xtest1,ytest1,xtest2,ytest2,xnear,ynear,pu);
--       dbms_output.put_line('edge_id ' ||edge_id ||'  d ' || ROUND(dist,16) || ' dist2 ' || ROUND(dist2,16));
/*
            point := SDO_GEOMETRY(2002, NULL, SDO_POINT_TYPE(x0,y0,NULL),NULL, NULL);
            seg := SDO_GEOMETRY(2002, NULL, NULL, SDO_ELEM_INFO_ARRAY(1, 2, 1), SDO_ORDINATE_ARRAY(xtest1,ytest1,xtest2,ytest2));
            geom := gz_project_pt(point,seg);
            oxnear := geom.sdo_point.x;
            oynear := geom.sdo_point.y;
            dbms_output.put_line(' Oxn ' || round(oxnear,16));
            dbms_output.put_line(' Oyn ' || round(oynear,16));
            */
            dist := accurate_gcd(x0,y0,xnear,ynear);

--            s := geodesic.inverse(y0,x0,ynear,xnear,gcd_K,az1,az2,m12);
--            dbms_output.put_line('kk ' || kk ||'  d ' || ROUND(dist,8) || ' karney ' || round(gcd_k,10));
          if dist < last_dist then
          /*
            dbms_output.put_line('edge_id ' ||near_edge_id ||'  d ' || ROUND(dist,8) || ' found at ' || kk);
            dbms_output.put_line(' dist ' || dist);
            dbms_output.put_line(' x0 ' || round(x0,16));
            dbms_output.put_line(' y0 ' || round(y0,16));
            dbms_output.put_line(' xn ' || round(xnear,16));
            dbms_output.put_line(' yn ' || round(ynear,16));
            dbms_output.put_line(' xt ' || round(xtest1,10));
            dbms_output.put_line(' yt ' || round(ytest1,10));
            dbms_output.put_line(' xT ' || round(xtest2,10));
            dbms_output.put_line(' yT ' || round(ytest2,10));
            */
--            xnear := x0;
--            ynear := 47.790927;
--            xnear := x0+0.000004;
--            ynear := 47.790925;
            dist := accurate_gcd(x0,y0,xnear,ynear);
--            s := geodesic.inverse(y0,x0,ynear,xnear,gcd_K,az1,az2,m12);
--            dbms_output.put_line('red   d ' || ROUND(dist,10) || ' karney ' || round(gcd_k,10));


              Nearbys :=MDSYS.SDO_LIST_TYPE(near_edge_id,(jj-1.),xnear,ynear,edge_id,(kk-1.),dist);
              last_dist := dist;
           end if;
           end if;
         end loop;
       End Loop;
       End If;
--       End If;
    End Loop;
--    End if;
   End Loop;

   If Nearbys.count >0 then
     RETURN Nearbys;
   End If;
   RETURN NULL;


END FIND_CLOSEST_EDGE;

FUNCTION GET_NEAREST_EDGE (pTopology VARCHAR2,face_id NUMBER,edge_id IN OUT NOCOPY NUMBER,pInSchema VARCHAR2 default 'GZDEC10ST',tolerance NUMBER default 0.05,decim_digits NUMBER default 7) RETURN MDSYS.SDO_GEOMETRY AS


-- Returns an edge geometry that is improved by increasing the distance away
-- from a close nearby edge.

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

   NearBys          MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();
   Edge_ids         MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();

   geometry         MDSYS.SDO_GEOMETRY;
   new_geometry     MDSYS.SDO_GEOMETRY;
   nearby_geometry  MDSYS.SDO_GEOMETRY;
   geom             MDSYS.SDO_GEOMETRY;
   geometry1  MDSYS.SDO_GEOMETRY;
   geometry2  MDSYS.SDO_GEOMETRY;
   intersections    MDSYS.SDO_GEOMETRY;
   Xys              MDSYS.SDO_ORDINATE_ARRAY;
   nearby_Xys       MDSYS.SDO_ORDINATE_ARRAY;
   they             MDSYS.SDO_ORDINATE_ARRAY;
   sql_stmt         VARCHAR2(4000);
   state            VARCHAR2(2);
  xv                NUMBER;
  yv                NUMBER;
  xtry                NUMBER;
  ytry              NUMBER;
  xnear              NUMBER;
  ynear             NUMBER;
  dist              NUMBER;
  t                 NUMBER;
  left_face         NUMBER;
  right_face        NUMBER;

  x0                NUMBER;
  y0                NUMBER;
  x3                NUMBER;
  y3                NUMBER;
  x4                NUMBER;
  y4                NUMBER;
  xi                NUMBER;
  yi                NUMBER;
  xlast             NUMBER;
  ylast             NUMBER;
  nearby_id         NUMBER;
  check_it          NUMBER;
  try               NUMBER :=0.;
  way1              NUMBER;
  way2              NUMBER;
  dist_found        NUMBER;
  near_seg          PLS_INTEGER;
  vertex            PLS_INTEGER;
  loops             PLS_INTEGER := 0;
  j                 PLS_INTEGER;
  len               PLS_INTEGER;
  next              PLS_INTEGER :=0;
BEGIN

-- Make the schema name from a partial schema name and the state FIPS in
-- the Topology  z6nn

   len := length(Inschema);
   state := SUBSTR(Topology,3,2);
   if SUBSTR(Inschema,len-1,2) <> state then
     Inschema := Inschema || state;
   end if;
   Topology := Inschema ||'.' ||Topology;

   nearbys := FIND_CLOSEST_EDGE(Topology,face_id);

   if nearbys is NULL then
   dbms_output.put_line('no close nearby edge found');
     RETURN NULL;
   end if;

   edge_id := nearbys(5);
   sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';
   EXECUTE IMMEDIATE sql_stmt INTO Geometry using edge_id;
   Xys := geometry.sdo_ordinates;
--   dbms_output.put_line('x1 ' || xys(1));
--   dbms_output.put_line('x2 ' || xys(2));
--   dbms_output.put_line('x3 ' || xys(3));
--   dbms_output.put_line('x4 ' || xys(4));
--   dbms_output.put_line('x5 ' || xys(5));
--   dbms_output.put_line('x6 ' || xys(6));


   vertex := nearbys(6);
   dbms_output.put_line('VV ' || vertex || ' edge ' || edge_id || ' nearby ' || nearby_id);
   xv := Xys(vertex*2-1);
   yv := Xys(vertex*2);
--   dbms_output.put_line('X ' || round(xv,10));
--   dbms_output.put_line('Y ' || round(yv,10));
   xnear := nearbys(3);
   ynear := nearbys(4);
   nearby_id := nearbys(1);
   near_seg := nearbys(2);
   dist_found := Nearbys(7);
   sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';
   EXECUTE IMMEDIATE sql_stmt INTO nearby_Geometry using nearby_id;
   nearby_xys := nearby_geometry.sdo_ordinates;
   dbms_output.put_line('nearby ' || nearby_id || ' nearseg is ' || (near_seg) || ' count ' || nearby_xys.count);


   dist := accurate_gcd(xnear,ynear,xv,yv);
   dbms_output.put_line('OLD dist ' || round(dist,10) || ' dist_found ' || round(dist_found,10));
--   dbms_output.put_line('Xn ' || round(xnear,10));
--   dbms_output.put_line('Yn ' || round(ynear,10));
   dist := 1.2;
   if yv < 50. then
      t := dist/0.11;
   else
      t := dist/(10.*0.33);
   end if;

   if t > 2 then
      t := 2.;
  elsif t < 0.2 then
      t := 0.2;
  end if;
  dbms_output.put_line('TT ' || t);
   x3 := nearby_Xys(near_seg*2-1);
   y3 := nearby_Xys(near_seg*2);
   x4 := nearby_Xys(near_seg*2+1);
   y4 := nearby_Xys(near_seg*2+2);

   x0 := Xys(vertex*2-3);
   y0 := Xys(vertex*2-2);
   xtry := Xys(vertex*2-1);
   ytry := Xys(vertex*2  );

-- Check to see if it crosses the other line
   check_it := simple_intersect(x0,y0,xtry,ytry,x3,y3,x4,y4,xi,yi);
   way1 := orient2d(x3,y3,x4,y4,xtry,ytry);
   geometry1 := MDSYS.SDO_GEOMETRY(2002,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),mdsys.sdo_ordinate_array(x3,y3,x4,y4));
   geometry2 := MDSYS.SDO_GEOMETRY(2002,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),mdsys.sdo_ordinate_array(x0,y0,xtry,ytry));
   intersections := SDO_GEOM.SDO_INTERSECTION(geometry1,geometry2,1.e-10);

--   execute immediate 'insert into points values(:1,:2,:3)' using 34,geometry1,'ABC';
--   execute immediate 'insert into points values(:1,:2,:3)' using 1,geometry2,'ABC';
--   execute immediate 'insert into points values(:1,:2,:3)' using 100,intersections,'ABC';
--   commit;
   if intersections is not null then
      they := intersections.sdo_ordinates;
     dbms_output.put_line('it intersects!!!' || round(they(1),10) || ' ' || round(they(2),10));
   end if;
   dbms_output.put_line('check_it ' ||check_it);
   way2 := orient2d(x3,y3,x4,y4,x0,y0);
-- The lines intersect if the clockwiseness is different
   dbms_output.put_line('ways ' ||way1 || ' ' || way2);
   if (way2 > 0. and way1 < 0.) or (way1 >0. and way2 < 0.) then
     check_it:=1;
   end if;
   while try >=0. and loops < 4 loop
         loops := loops + 1;
-- If the lines are oriented so we can just march away with t increasing wrt
-- (xv,yv):
--                |       + t=3
--                |   +   t=2
--       \        + ---------
--         \       (xv,yv) t=1
--           . (xnear,ynear)
--             \
     if check_it < 0 then    -- It doesn't intersect
       if t < 0.5 then
          t := 1.-t;
       end if;
       xtry := xnear*(1.-t) + t * xv;
       ytry := ynear*(1.-t) + t * yv;

     else
--    The line cross over
--                           +  (xv,yv)  t = 1
--                         /   \
--              _____________._t=0____________  .= (xnear,ynear)
--                       /        \
--                      /    + t=-1\
--
--                           + t=-2

       xtry := xnear*(1.-t) + t * xv;
       ytry := ynear*(1.-t) + t * yv;
     end if;
   dbms_output.put_line('t ' || round(t,10));
   dbms_output.put_line('X ' || round(xtry,10));
   dbms_output.put_line('Y ' || round(ytry,10));
   dbms_output.put_line('Xn ' || round(xnear,10));
   dbms_output.put_line('Yn ' || round(ynear,10));
   dbms_output.put_line('Xv ' || round(xv,10));
   dbms_output.put_line('Yv ' || round(yv,10));
   dist := accurate_gcd(xnear,ynear,xtry,ytry);
   dbms_output.put_line('loop ' || loops || ' NEW dist ' || round(dist,10));

   Xys(vertex*2-1) := xtry;
   Xys(vertex*2  ) := ytry;

    try := simple_intersect(x0,y0,xtry,ytry,x3,y3,x4,y4,xi,yi);

-- We had a case in Alaska where the face (3601 in Topology Z602LS) had an apparent separation of
-- 0.854 meters but Oracle said they intersected.
    if loops = 1 and try < 0. and dist < 2.*dist_found then
       try := 0.0;
    elsif yv >= 50. and try < 0. and dist < 4. then
       try := 0.0;
    end if;
--    dbms_output.put_line('try ' || try);
      geometry2 := MDSYS.SDO_GEOMETRY(2002,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),mdsys.sdo_ordinate_array(x0,y0,xtry,ytry));
   intersections := SDO_GEOM.SDO_INTERSECTION(geometry1,geometry2,1.e-10);
   if intersections is not null then
    they := intersections.sdo_ordinates;
     dbms_output.put_line('it intersects!!!' || round(they(1),10) || ' ' || round(they(2),10));
   end if;
--    dbms_output.put_line('Xi ' || round(xi,10));
--    dbms_output.put_line('Yi ' || round(yi,10));

---  intersections := SDO_GEOM.SDO_INTERSECTION(geometry1,geometry2,small_tolerance);
/*
   xys.trim(xys.count-4);
   xys(1) := x0;
   xys(2) := y0;
   xys(3) := xtry;
   xys(4) := ytry;

   dbms_output.put_line('Xv ' || round(xv,10));
    dbms_output.put_line('Yv ' || round(yv,10));
    dbms_output.put_line('Xn ' || round(xnear,10));
    dbms_output.put_line('Yn ' || round(ynear,10));
    */
--   new_geometry := MDSYS.SDO_GEOMETRY(2002,8265,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),Xys);
--   execute immediate 'insert into points values(:1,:2,:3)' using loopS*10,new_geometry,'ABC';
--   commit;
--
      t := t*5.;
   end loop;
--    xys(1) := x3;
--   xys(2) := y3;
--   xys(3) := x4;
--   xys(4) := y4;

   new_geometry := MDSYS.SDO_GEOMETRY(2002,8265,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),Xys);
--   execute immediate 'insert into points values(:1,:2,:3)' using 2,new_geometry,'ABC';
--   commit;

   RETURN new_geometry;

END GET_NEAREST_EDGE;
--
FUNCTION OLDGET_NEAREST_EDGE (pTopology VARCHAR2,face_id NUMBER,edge_id IN OUT NOCOPY NUMBER,pInSchema VARCHAR2 default 'GZDEC10ST',tolerance NUMBER default 0.05,decim_digits NUMBER default 7) RETURN MDSYS.SDO_GEOMETRY AS


-- Returns an edge geometry that is improved by increasing the distance away
-- from a close nearby edge.

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

   NearBys          MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();
   Edge_ids         MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();

   geometry         MDSYS.SDO_GEOMETRY;
   new_geometry     MDSYS.SDO_GEOMETRY;
   nearby_geometry  MDSYS.SDO_GEOMETRY;
   geometry1  MDSYS.SDO_GEOMETRY;
   geometry2  MDSYS.SDO_GEOMETRY;
   intersections    MDSYS.SDO_GEOMETRY;
   Xys              MDSYS.SDO_ORDINATE_ARRAY;
   nearby_Xys       MDSYS.SDO_ORDINATE_ARRAY;
   they             MDSYS.SDO_ORDINATE_ARRAY;
   sql_stmt         VARCHAR2(4000);
   state            VARCHAR2(2);
  xv                NUMBER;
  yv                NUMBER;
  xtry                NUMBER;
  ytry              NUMBER;
  xnear              NUMBER;
  ynear             NUMBER;
  dist              NUMBER;
  t                 NUMBER;
  left_face         NUMBER;
  right_face        NUMBER;

  x0                NUMBER;
  y0                NUMBER;
  x3                NUMBER;
  y3                NUMBER;
  x4                NUMBER;
  y4                NUMBER;
  xi                NUMBER;
  yi                NUMBER;
  xlast             NUMBER;
  ylast             NUMBER;
  nearby_id         NUMBER;
  check_it          NUMBER;
  try               NUMBER :=0.;
  way1              NUMBER;
  way2              NUMBER;
  dist_found        NUMBER;
  near_seg          PLS_INTEGER;
  vertex            PLS_INTEGER;
  loops             PLS_INTEGER := 0;
  j                 PLS_INTEGER;
  len               PLS_INTEGER;
  next              PLS_INTEGER :=0;
BEGIN

-- Make the schema name from a partial schema name and the state FIPS in
-- the Topology  z6nn

   len := length(Inschema);
   state := SUBSTR(Topology,3,2);
   if SUBSTR(Inschema,len-1,2) <> state then
     Inschema := Inschema || state;
   end if;
   Topology := Inschema ||'.' ||Topology;

   nearbys := FIND_CLOSEST_EDGE(Topology,face_id);

   if nearbys is NULL then
   dbms_output.put_line('no close nearby edge found');
     RETURN NULL;
   end if;

   edge_id := nearbys(5);
   sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';
   EXECUTE IMMEDIATE sql_stmt INTO Geometry using edge_id;
   Xys := geometry.sdo_ordinates;
--   dbms_output.put_line('x1 ' || xys(1));
--   dbms_output.put_line('x2 ' || xys(2));
--   dbms_output.put_line('x3 ' || xys(3));
--   dbms_output.put_line('x4 ' || xys(4));
--   dbms_output.put_line('x5 ' || xys(5));
--   dbms_output.put_line('x6 ' || xys(6));


   vertex := nearbys(6);
   dbms_output.put_line('VV ' || vertex || ' edge ' || edge_id || ' nearby ' || nearby_id);
   xv := Xys(vertex*2-1);
   yv := Xys(vertex*2);
--   dbms_output.put_line('X ' || round(xv,10));
--   dbms_output.put_line('Y ' || round(yv,10));
   xnear := nearbys(3);
   ynear := nearbys(4);
   nearby_id := nearbys(1);
   near_seg := nearbys(2);
   dist_found := Nearbys(7);
   sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';
   EXECUTE IMMEDIATE sql_stmt INTO nearby_Geometry using nearby_id;
   nearby_xys := nearby_geometry.sdo_ordinates;
   dbms_output.put_line('nearby ' || nearby_id || ' nearseg is ' || (near_seg) || ' count ' || nearby_xys.count);


   dist := accurate_gcd(xnear,ynear,xv,yv);
--   dbms_output.put_line('OLD dist ' || round(dist,10));
--   dbms_output.put_line('Xn ' || round(xnear,10));
--   dbms_output.put_line('Yn ' || round(ynear,10));
   if yv < 50. then
      t := 0.11/dist;
   else
      t := 10.*0.33/dist;
   end if;

   x3 := nearby_Xys(near_seg*2-1);
   y3 := nearby_Xys(near_seg*2);
   x4 := nearby_Xys(near_seg*2+1);
   y4 := nearby_Xys(near_seg*2+2);

   x0 := Xys(vertex*2-3);
   y0 := Xys(vertex*2-2);
   xtry := Xys(vertex*2-1);
   ytry := Xys(vertex*2  );

-- Check to see if it crosses the other line
   check_it := simple_intersect(x0,y0,xtry,ytry,x3,y3,x4,y4,xi,yi);
   way1 := orient2d(x3,y3,x4,y4,xtry,ytry);
   geometry1 := MDSYS.SDO_GEOMETRY(2002,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),mdsys.sdo_ordinate_array(x3,y3,x4,y4));
   geometry2 := MDSYS.SDO_GEOMETRY(2002,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),mdsys.sdo_ordinate_array(x0,y0,xtry,ytry));
   intersections := SDO_GEOM.SDO_INTERSECTION(geometry1,geometry2,1.e-10);

--   execute immediate 'insert into points values(:1,:2,:3)' using 34,geometry1,'ABC';
--   execute immediate 'insert into points values(:1,:2,:3)' using 1,geometry2,'ABC';
--   execute immediate 'insert into points values(:1,:2,:3)' using 100,intersections,'ABC';
--   commit;
   if intersections is not null then
      they := intersections.sdo_ordinates;
     dbms_output.put_line('it intersects!!!' || round(they(1),10) || ' ' || round(they(2),10));
   end if;
   dbms_output.put_line('check_it ' ||check_it);
   way2 := orient2d(x3,y3,x4,y4,x0,y0);
-- The lines intersect if the clockwiseness is different
   dbms_output.put_line('ways ' ||way1 || ' ' || way2);
   if (way2 > 0. and way1 < 0.) or (way1 >0. and way2 < 0.) then
     check_it:=1;
   end if;
   while try >=0. and loops < 4 loop
         loops := loops + 1;
-- If the lines are oriented so we can just march away with t increasing wrt
-- (xv,yv):
--                |       + t=3
--                |   +   t=2
--       \        + ---------
--         \       (xv,yv) t=1
--           . (xnear,ynear)
--             \
     if check_it < 0 then    -- It doesn't intersect
       t := abs(t);
       xtry := xnear*(1.-t) + t * xv;
       ytry := ynear*(1.-t) + t * yv;
     else
--    The line cross over
--                           +  (xv,yv)  t = 1
--                         /   \
--              _____________._t=0____________  .= (xnear,ynear)
--                       /        \
--                      /    + t=-1\
--
--                           + t=-2

       xtry := xnear*(1.-t) + t * xv;
       ytry := ynear*(1.-t) + t * yv;
     end if;
   dbms_output.put_line('t ' || round(t,10));
   dbms_output.put_line('X ' || round(xtry,10));
   dbms_output.put_line('Y ' || round(ytry,10));
   dist := accurate_gcd(xnear,ynear,xtry,ytry);
   dbms_output.put_line('loop ' || loops || ' NEW dist ' || round(dist,10));

   Xys(vertex*2-1) := xtry;
   Xys(vertex*2  ) := ytry;

    try := simple_intersect(x0,y0,xtry,ytry,x3,y3,x4,y4,xi,yi);

-- We had a case in Alaska where the face (3601 in Topology Z602LS) had an apparent separation of
-- 0.854 meters but Oracle said they intersected.
    if loops = 1 and try < 0. and dist < 2.*dist_found then
       try := 0.0;
    end if;
--    dbms_output.put_line('try ' || try);
      geometry2 := MDSYS.SDO_GEOMETRY(2002,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),mdsys.sdo_ordinate_array(x0,y0,xtry,ytry));
   intersections := SDO_GEOM.SDO_INTERSECTION(geometry1,geometry2,1.e-10);
   if intersections is not null then
    they := intersections.sdo_ordinates;
     dbms_output.put_line('it intersects!!!' || round(they(1),10) || ' ' || round(they(2),10));
   end if;
--    dbms_output.put_line('Xi ' || round(xi,10));
--    dbms_output.put_line('Yi ' || round(yi,10));

---  intersections := SDO_GEOM.SDO_INTERSECTION(geometry1,geometry2,small_tolerance);
/*
   xys.trim(xys.count-4);
   xys(1) := x0;
   xys(2) := y0;
   xys(3) := xtry;
   xys(4) := ytry;

   dbms_output.put_line('Xv ' || round(xv,10));
    dbms_output.put_line('Yv ' || round(yv,10));
    dbms_output.put_line('Xn ' || round(xnear,10));
    dbms_output.put_line('Yn ' || round(ynear,10));
    */
--   new_geometry := MDSYS.SDO_GEOMETRY(2002,8265,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),Xys);
--   execute immediate 'insert into points values(:1,:2,:3)' using loopS*10,new_geometry,'ABC';
--   commit;
--
      t := t*5.;
   end loop;
--    xys(1) := x3;
--   xys(2) := y3;
--   xys(3) := x4;
--   xys(4) := y4;

   new_geometry := MDSYS.SDO_GEOMETRY(2002,8265,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),Xys);
--   execute immediate 'insert into points values(:1,:2,:3)' using 2,new_geometry,'ABC';
--   commit;

   RETURN new_geometry;


END OLDGET_NEAREST_EDGE;
--
FUNCTION Is_pt_on_Segment(xtest IN OUT NOCOPY NUMBER,ytest IN OUT NOCOPY NUMBER, -- test point
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,  -- the line
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                       xnear IN OUT NOCOPY NUMBER,ynear IN OUT NOCOPY NUMBER,
                       pu IN OUT NOCOPY NUMBER) RETURN NUMBER DETERMINISTIC AS
/*
********************************************************************************
--Program Name: Is_pt_on_Segment
--Author: Sidey Timmins
--Creation Date: 10/22/2008

--Usage:  --
  --   REQUIRED Parameters:
  --      INPUT
  --      (xtest,ytest)   - A point to check its proximity to a line.
  --      (x1,y1)     - Coordinates for one end of the segment to be tested.
  --      (x2,y2)     - Coordinates for other end of the segment to be tested.
  --      OUTPUT
  --      (xnear,ynear): Calculated nearest point on segment. The are NULL when
  --                     the point is not on line.
  --      pu             Returned line parameter which will vary from
  --                     about 0 to about 1 showing the fractional distance
  --                     from (x1,y1) to (x2,y2).
--
-- Purpose:  Find whether a point is on a line segment, and returns the
--           distance in the input coordinate units. Also returns the
--           near point on the line and its position as a line parameter.

-- Reference: Paul Bourke: "Minimum Distance between a point and a line".
--             http://local.wasp.uwa.edu.au/~pbourke/geometry/pointline/
********************************************************************************
*/
     BIG           NUMBER := 1.E10;
     u             NUMBER;            -- line parameter
     distance      NUMBER := BIG;
     length_sq     NUMBER;
     dx            NUMBER := x2-x1;
     dy            NUMBER := y2-y1;
     epsilon       NUMBER := 1.E-7;

BEGIN

     u := ((Xtest - X1) * dx + (Ytest-Y1) * dy);
     xnear := NULL;
     ynear := NULL;
--     dbms_output.put_line('dx ' || round(dx,20));
--     dbms_output.put_line('xtest-x1 ' || round((Xtest-x1),10));
--     dbms_output.put_line('dy ' || round(dy,20));
--     dbms_output.put_line('ytest-y1 ' || round((ytest-y1),10));
-- The line parameter varies from 0. to 1. exactly. Here we use from
--  -epsilon to 1.+ epsilon where epsilon is a very small number.
     If u >= -epsilon then
        length_sq :=  dx*dx + dy*dy;
-- way it was    if u <= length_sq and length_sq > 0. then
        if u <= length_sq +epsilon*length_sq and length_sq > 0. then
           u := u/length_sq;

           xnear := X1 + u * dx;
           ynear := Y1 + u * dy;
--           dbms_output.put_line('U ' || u);
--           dbms_output.put_line('y1 ' || y1);
--           dbms_output.put_line('Yn ' || ynear);
           pu := 1. - u;   -- fraction from X1 to X2
--           dbms_output.put_line('pu ' || round(pu,10));
           Distance := sqrt((Xtest - xnear) * (Xtest - xnear) + (Ytest - ynear) * (Ytest - ynear));
           RETURN Distance;
        end if;
      End if;

    Return BIG;


END IS_PT_ON_SEGMENT;
--
FUNCTION ORIENT2D (paX   NUMBER,  paY   NUMBER,
                      pbX   NUMBER,  pbY   NUMBER,
                      pcX   NUMBER,  pcY   NUMBER)

            RETURN          NUMBER
            Deterministic IS
/*
**************************************************************************************
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
***************************************************************************************
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

  RETURN det;

END ORIENT2D;
--
FUNCTION SIMPLE_INTERSECT( X1 IN OUT NOCOPY NUMBER,
                              Y1 IN OUT NOCOPY NUMBER,
                              X2 IN OUT NOCOPY NUMBER,
                              Y2 IN OUT NOCOPY NUMBER,
                              X3 IN OUT NOCOPY NUMBER,
                              Y3 IN OUT NOCOPY NUMBER,
                              X4 IN OUT NOCOPY NUMBER,
                              Y4 IN OUT NOCOPY NUMBER,
                              Xi IN OUT NOCOPY NUMBER,
                              Yi IN OUT NOCOPY NUMBER
                              )
            RETURN NUMBER Deterministic IS
/*
********************************************************************************
--Program Name: line_interesect
--Author: Sidey Timmins
--Creation Date: 4/24/2007

--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 11 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT

  --      X1,Y1      - start point (x,y) of line 1 (point A)
  --      X2,Y2      - end point (x,y) of line 1 (point B)
  --      X3,Y3      - start point  (x,y) of line 2
  --      X4,Y4      - end point (x,y) of line 2
  --
  --      OUTPUT
  --      xi,yi      - the intersection point
  --      det        - The determinant.
  --      Not returned int this version
  --      R         - the (returned) line parameter along line 1 (A to B)
  --                     will be from 0 to 1 if the interesection point C
  --                     is on the line.
  --
  --
  --                   C
  --            A+-----.------+B
  --          zero   prout    1.0
  --
-- Purpose: Determine where 2 lines intersect
  -- Return:      zero to 1.:  intersection point is on line
  --                      -1:  no intersection
  --                      -2:  parallel
  --                   -3,-4:  concident
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
--  http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
-- Dependencies: none
********************************************************************************
*/
   s           NUMBER;
   t           NUMBER;
   X21         NUMBER := X2-X1;
   Y21         NUMBER := Y2-Y1;
   X43         NUMBER := X4-X3;
   Y43         NUMBER := Y4-Y3;
   X31         NUMBER := X3-X1;
   Y31         NUMBER := Y3-Y1;
   length_2    NUMBER;
   d           NUMBER;
   xcheck      NUMBER;
   ycheck      NUMBER;
   det         NUMBER;

BEGIN
-- Check parametric equations for two lines: s is on line 1 from (x1,y1) to (x2,y2)
--                                           t is on line 2 from (x3,y3) to (x4,y4)
-- For example xx = (1.-s) * x1 + s*x2
--             yy = (1.-s) * y1 + s*y2

   det :=  X21 * Y43 - X43 * Y21;


   IF det <> 0.0 THEN

      s := (-X43 * Y31 + Y43 * X31) /det;
      t := (-X21 * Y31 + Y21 * X31) /det;

--     dbms_output.put_line(' SS ' || round(s,8) || ' t ' || round(t,8));


      If s >= 0.0 and s <= 1.0 and t >= 0.0 and t <= 1.0 then

        xi := X1 + s * X21;
        yi := Y1 + s * Y21;

        RETURN s;

-- A near miss and the intersection point is off the line
-- For this test, the first line is the subject line.
--                1st line
--      +---------------------------+
--                   +
--                     \   this is the 2nd line for this test
--                      +
/*
      Elsif check_near_miss = 'TRUE' and (s >= 0.0 and s <= 1.0) then
        xi := X1 + s * X21;
        yi := Y1 + s * Y21;

-- pretend near glances intersect
        if y1 > 50. then  -- used tolerance = 2 meters hers
          xcheck   := 0.0000016 * near_ratio;
          ycheck   := 0.0000005 * near_ratio;
        else
          xcheck   := 0.00000075 * near_ratio;
          ycheck   := 0.00000065 * near_ratio;
        End If;

        if t < 0.0 and abs(x3-xi) < xcheck and abs(y3-yi) < ycheck then
          d := fast_distance(xi,yi,x3,y3,8265);
--          dbms_output.put_line('found ' || round(d,6));
          if d < near_distance then
           dist_found := d;
           RETURN 0.5;
          end if;
        end if;
        if t > 1.0 and abs(x4-xi) < xcheck and abs(y4-yi) < ycheck then
          d := fast_distance(xi,yi,x4,y4,8265);
--          dbms_output.put_line('Found ' || round(d,6));
          if d < near_distance then
            dist_found := d;
            RETURN 0.5;
          end if;
        end if;

        RETURN -1.;

-- check point is on line
         length_2 := ((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1));

         IF length_2 <> 0 THEN
              d := abs((X1 - xi) * (Y2 - Y1) - (Y1 - yi) * (X2 - X1))  / sqrt(length_2);
              dbms_output.put_line(' CHECK d is :' || round(d,10) || ' for intersection ' || xi || ' ' || yi);
          END IF;
  */
      Else
        RETURN  -1.;
      End if;


   END IF;  -- end of if det <> 0.0
   if x1 = x3 and y1 = y3 and x2 = x4 and y2 = y4 then
       RETURN -3.;
   elsif x1 = x4 and y1 = y4 and x2 = x3 and y2 = y3 then
       RETURN -4.;
   end if;
-- Lines are parallel (but not coincident)
   RETURN -2.;

END SIMPLE_INTERSECT;

   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------------------------
   FUNCTION before(pString VARCHAR2,pSearchString VARCHAR2,pAppearance NUMBER DEFAULT 1) RETURN VARCHAR2 AS
/**
-------------------------------------------------------------------------------------------------------------
- Program Name: after
- Author: Nick Padfield
- Creation Date: 09/14/2009
- Usage:
-   Call this function from inside a PL/SQL program or directly from SQL.
-   There are two required parameters and one optional parameter.
-
-     REQUIRED Parameters:
-        pString        - String you want to search
-        pSearchString  - String you want to find
-
-     OPTIONAL Parameters:
-        pAppearance    - after how many appearances do you want to find the search string?
-
-
- Purpose:
-    To return the string that is before your pSearchString.
-
- Dependencies:
-    None
-
-------------------------------------------------------------------------------------------------------------
*/
   vString                VARCHAR2(4000)              := pString;
   vSearchString          VARCHAR2(4000)              := pSearchString;
   vAppearance            NUMBER                      := pAppearance;
   vSearchIndex           NUMBER;
   vStringLength          NUMBER;
   vSearchLength          NUMBER;
   vStringBefore          VARCHAR2(4000);
BEGIN
   --vStringLength := LENGTH(vString);
   --vSearchLength := LENGTH(vSearchString);
   vSearchIndex := INSTR(vString,vSearchString,1,vAppearance);
   IF (vSearchIndex <> 0) THEN
      vStringBefore := SUBSTR(vString,1,vSearchIndex-1);
   ELSE
      vStringBefore := vString;
   END IF;
   RETURN vStringBefore;
END before;


------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------


    PROCEDURE INSERT_SDOGEOM_METADATA (
      p_table_name      IN VARCHAR2,
      p_column_name     IN VARCHAR2,
      p_srid            IN NUMBER,
      p_tolerance       IN NUMBER DEFAULT .05
   )
   AS

    --Matt! 3/08/10
    --ouch, switched OR to AND in first IF.  3/8/11

    psql    VARCHAR2(4000);
    psql2   VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('INSERT_SDOGEOM_METADATA');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_srid > 999999
      AND p_srid < 1000100
      THEN
         --CPB ALBERS

         psql := 'INSERT INTO user_sdo_geom_metadata a '
              || '(a.table_name, a.column_name, a.srid, a.diminfo) '
              || 'VALUES '
              || '(:p1,:p2,:p3, '
              || 'SDO_DIM_ARRAY (SDO_DIM_ELEMENT (''X'',-100000000,100000000,:p4), '
              || 'SDO_DIM_ELEMENT (''Y'',-100000000,100000000,:p5) )) ';

      ELSIF p_srid = 8265
      THEN
         --GEODETIC
         --Somebody else should go in here too, 42-- something?

         psql := 'INSERT INTO user_sdo_geom_metadata a '
              || '(a.table_name, a.column_name, a.srid, a.diminfo) '
              || 'VALUES '
              || '(:p1,:p2,:p3, '
              || 'SDO_DIM_ARRAY (SDO_DIM_ELEMENT (''Longitude'',-180,180,:p4), '
              || 'SDO_DIM_ELEMENT(''Latitude'',-90,90,:p5) ))';

      ELSIF p_srid IS NULL
      THEN

         --special NULLed out geodetic to cartesian working coordinate system
         --same SQL as geodetic, but separated for clarity

         psql := 'INSERT INTO user_sdo_geom_metadata a '
              || '(a.table_name, a.column_name, a.srid, a.diminfo) '
              || 'VALUES '
              || '(:p1,:p2,:p3, '
              || 'SDO_DIM_ARRAY (SDO_DIM_ELEMENT (''Longitude'',-180,180,:p4), '
              || 'SDO_DIM_ELEMENT(''Latitude'',-90,90,:p5) ))';


      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Sorry, no one taught me what to do with srid ' || p_srid);

      END IF;

      BEGIN

         EXECUTE IMMEDIATE psql USING p_table_name,
                                      p_column_name,
                                      p_srid,
                                      p_tolerance,
                                      p_tolerance;

      EXCEPTION
         WHEN OTHERS
         THEN

         psql2 := 'DELETE FROM user_sdo_geom_metadata a '
               || 'WHERE a.table_name = :p1 '
               || 'AND a.column_name = :p2 ';
         EXECUTE IMMEDIATE psql2 USING p_table_name,
                                       p_column_name;

         EXECUTE IMMEDIATE psql USING p_table_name,
                                      p_column_name,
                                      p_srid,
                                      p_tolerance,
                                      p_tolerance;

      END;

   END INSERT_SDOGEOM_METADATA;

    ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE ADD_SPATIAL_INDEX (
      p_table_name      IN VARCHAR2,
      p_column_name     IN VARCHAR2,
      p_srid            IN NUMBER,
      p_tolerance       IN NUMBER,
      p_local           IN VARCHAR2 DEFAULT NULL,
      p_parallel        IN NUMBER DEFAULT NULL,
      p_idx_name        IN VARCHAR2 DEFAULT NULL
   )
   AS

    --Matt! 3/08/10


    psql          VARCHAR2(4000);
    psql2         VARCHAR2(4000);
    table_name    VARCHAR2(4000) := UPPER(p_table_name);
    column_name   VARCHAR2(4000) := UPPER(p_column_name);
    index_name    VARCHAR2(4000);

   BEGIN

      IF INSTR(p_table_name,'.') <> 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry database hero, I can''t index tables in remote schemas like ' || p_table_name);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_SPATIAL_INDEX');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      GZ_GEOM_UTILS.INSERT_SDOGEOM_METADATA(table_name,column_name,p_srid,p_tolerance);

      IF p_idx_name IS NULL
      THEN
         index_name := p_table_name || '_SIDX';
      ELSE
         index_name := p_idx_name;
      END IF;


      IF length(index_name) > 30
      THEN
         index_name := substr(index_name,1,30);
      END IF;

      psql := 'CREATE INDEX '
           || index_name
           || ' ON ' || table_name || '(' || column_name || ')'
           || ' INDEXTYPE IS MDSYS.SPATIAL_INDEX ';

      IF p_local IS NOT NULL
      THEN
         psql := psql || 'LOCAL ';
      END IF;

      IF p_parallel IS NOT NULL
      THEN
         psql := psql || 'PARALLEL ' || TO_CHAR(p_parallel) || ' ';
      ELSE
         psql := psql || 'NOPARALLEL ';
      END IF;

      --unpublished oracle bug requires this parm
      psql := psql || 'PARAMETERS (''SDO_DML_BATCH_SIZE=1'') ';

      BEGIN
         --dbms_output.put_line(psql);
         EXECUTE IMMEDIATE psql;

      EXCEPTION
         WHEN OTHERS
         THEN

         psql2 := 'DROP INDEX ' || index_name;
         EXECUTE IMMEDIATE psql2;

         EXECUTE IMMEDIATE psql;

      END;

   END ADD_SPATIAL_INDEX;

 -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION REMOVE_HOLES (
      p_sql          IN VARCHAR2,
      p_area         IN NUMBER,
      p_tolerance    IN NUMBER
   )
   RETURN SDO_GEOMETRY
   AS

      --Matt! 9/29/10
      --This is the backwards-compatible wrapper for Salman, and also maybe for debugging
      --The real code is in the overloaded remove_holes below
      --No logging or geom validation in the Salman version

      --I dont recommend this version unless you are really confident about your inputs
      --Mainly: all are valid and rectified
      --Sample usage:
      --   select GZ_UTILITIES.REMOVE_HOLES(
      --                        'select SDO_UTIL.RECTIFY_GEOMETRY(sdogeometry,.05) from gz_sdrp10_z6.diss_out330 where csafp = 304',
      --                        1000,
      --                        .05) from dual

      sdogeometry       SDO_GEOMETRY;

   BEGIN

      EXECUTE IMMEDIATE p_sql INTO sdogeometry;

      RETURN GZ_GEOM_UTILS.REMOVE_HOLES(sdogeometry,
                                        NULL,
                                        p_area,
                                        p_tolerance,
                                        'N');

   END REMOVE_HOLES;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION REMOVE_HOLES (
      geom_in           IN SDO_GEOMETRY,
      p_id              IN VARCHAR2,
      p_area            IN NUMBER,
      p_tolerance       IN NUMBER,
      p_log             IN VARCHAR2 DEFAULT 'N',
      p_logtable_name   IN VARCHAR2 DEFAULT NULL,
      p_validate_out    IN VARCHAR2 DEFAULT 'N'
   )
   RETURN SDO_GEOMETRY
   AS

      --Matt! 9/15/10
      --Salman promised me that he will polish this off
      --Revised and fixed bugs, added logging and validating Matt! 9/30/10

      --Assumptions
      --1. The input geometry outer rings should be valid.
      --      If not and validate_out is set to Y, we will throw an error
      --2. The input geometry inner rings may be invalid sliver type things.
      --     If they are invalid, we will temporarily rectify them and then toss the tiny ones
      --3. If you call without the validate option all bets are off.
      --      The output may be invalid, and in some cases sliver holes may remain.
      --      (SDO_AREA on polygons sometimes returns giant areas for invalid polys)

      --The logging business is just a phoney Matt placeholder
      --I know nothing about how production actually works at this stage of the GZprocess

      --Sample call, production-like, with logging and validation:
      -- noslivergeom := GZ_UTILITIES.REMOVE_HOLES(geom,'10162',1000,.05,'Y','REMOVE_HOLES_LOG','Y');

      --Sample call, quickly testing or debugging, no logging allowed in select stmt
      -- select GZ_UTILITIES.REMOVE_HOLES(a.sdogeometry, '2430',1000,.05,'N') from
      --    gz_sdrp10_z6.diss_out250 a where a.aiannhce = 2430

      --Phoney log table phoney setup phoney Matt testing:
      -- create table remove_holes_log
      -- (id VARCHAR2(4000), message VARCHAR2(4000), sdo_dump SDO_GEOMETRY)

      geom_temp      SDO_GEOMETRY;
      inner_ring     SDO_GEOMETRY;
      geom_out       SDO_GEOMETRY;
      eleminfo       SDO_ELEM_INFO_ARRAY := SDO_ELEM_INFO_ARRAY();
      tempinfo       SDO_ELEM_INFO_ARRAY := SDO_ELEM_INFO_ARRAY();
      ordinates      SDO_ORDINATE_ARRAY :=  SDO_ORDINATE_ARRAY();
      humpty         SDO_GEOMETRY;
      keptkount      PLS_INTEGER := 0;
      ordstart       PLS_INTEGER;
      ordend         PLS_INTEGER;
      ordkounter     PLS_INTEGER := 0;
      logsql         VARCHAR2(4000);
      rectified      PLS_INTEGER := 0;

   BEGIN


      IF geom_in.SDO_GTYPE != 2003
      AND geom_in.SDO_GTYPE != 2007
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Got gtype ' || geom_in.SDO_GTYPE || '. Whats the deal?');
      END IF;

      IF p_log = 'Y'
      THEN


         logsql := 'INSERT INTO ' || UPPER(p_logtable_name) || ' a '
                || 'VALUES (:p1,:p2,:p3) ';

         EXECUTE IMMEDIATE logsql USING p_id, 'START: Remove_Holes', geom_in;
         COMMIT;

      END IF;



      FOR i IN 1 .. SDO_UTIL.GETNUMELEM(geom_in)
      LOOP

         --Get each outer ring
         geom_temp := SDO_UTIL.EXTRACT(geom_in,i);


         --Check for valid outer ring if the caller cares
         --We never touch the outer ring, all we can do is bomb

         IF p_validate_out = 'Y'
         AND SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT( SDO_UTIL.EXTRACT(geom_temp,1,1), p_tolerance) != 'TRUE'
         THEN

            IF p_log = 'Y'
            THEN

               EXECUTE IMMEDIATE logsql USING p_id,
                                              'Failed: Outer ring number ' || i || ' is not valid',
                                              SDO_UTIL.EXTRACT(geom_temp,1,1);
               COMMIT;

            END IF;

            RAISE_APPLICATION_ERROR(-20001,'Failed: Outer ring number ' || i || ' is not valid');

         END IF;


         eleminfo := geom_temp.SDO_ELEM_INFO;

         IF eleminfo.COUNT = 3
         THEN

            --nothing to do
            IF i = 1
            THEN
               geom_out := geom_temp;
            ELSE
               geom_out := SDO_UTIL.APPEND(geom_out,geom_temp);
            END IF;

         ELSE

            --we have inner rings
            --in this section we will attempt to build a valid 2003, with our without holes
            -- (based on the areas of the holes)
            --then append the result to the running geom_out

            --ELEM info is in triplets

            FOR j IN 1 .. (eleminfo.COUNT)/3
            LOOP


               IF j = 1
               THEN

                  --can always pass along the outer ring
                  --no need to test validation, already did it above

                  --technically this one isnt an "inner"
                  inner_ring := SDO_UTIL.EXTRACT(geom_temp,1,j);

                  --move the info array along
                  tempinfo.EXTEND(3);
                  tempinfo := inner_ring.SDO_ELEM_INFO;

                  --move the ordinates along
                  ordinates.EXTEND(inner_ring.sdo_ordinates.COUNT);
                  ordinates := inner_ring.SDO_ORDINATES;

                  --start this counter up for later
                  ordkounter := ordinates.COUNT;

               ELSE

                  --get the inner ring
                  --element is always 1 since the EXTRACT up above guarantees a 2003
                  --ring is 2 or higher
                  inner_ring := SDO_UTIL.EXTRACT(geom_temp,1,j);


                  --test for validation and rectify if necessary
                  --so we can get a true area next

                  IF p_validate_out = 'Y'
                  AND SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(inner_ring, p_tolerance) != 'TRUE'
                  THEN

                     inner_ring := SDO_UTIL.RECTIFY_GEOMETRY(inner_ring, p_tolerance);

                     --note that we did this
                     --if this guy doesnt get tiny tossed, then we have an invalid
                     --big inner ring headed for the output
                     rectified := 1;

                  END IF;



                  IF SDO_GEOM.SDO_AREA(inner_ring, p_tolerance) < p_area
                  OR SDO_GEOM.SDO_AREA(inner_ring, p_tolerance) IS NULL --Sometimes rectify rectifies a sliver out of existence
                  THEN

                     --we dont want this bad boy
                     ------------------------------
                     --WE SHOULD LOG SOMETHING HERE
                     --Like: Hello, we dropped an inner ring with area of SDO_GEOM.SDO_AREA(inner_ring, tolerance)
                     ------------------------------

                     IF p_log = 'Y'
                     THEN

                        EXECUTE IMMEDIATE logsql USING p_id,
                                                       'Dropping an inner ring with area ' || NVL(SDO_GEOM.SDO_AREA(inner_ring, p_tolerance),0),
                                                       inner_ring;

                        COMMIT;

                     END IF;

                  ELSE


                     IF p_validate_out = 'Y'
                     AND rectified = 1
                     THEN

                        --doh we just had to rectify an inner ring
                        --but hes not valid, this is a fail

                        IF p_log = 'Y'
                        THEN

                           EXECUTE IMMEDIATE logsql USING p_id,
                                                          'Failed: Inner ring number ' || j || ' of outer ring ' || i || ' is not valid',
                                                          --inner_ring;
                                                          SDO_UTIL.RECTIFY_GEOMETRY(inner_ring, p_tolerance);
                           COMMIT;

                        END IF;

                        RAISE_APPLICATION_ERROR(-20001,'Failed: Inner ring number ' || j || ' of outer ring ' || i || ' is not valid');

                     END IF;


                     keptkount := keptkount + 1;

                     --we need him

                     --extend silly arrays
                     tempinfo.EXTEND(3);

                     --keeper starts at current ordinate kount + 1

                     tempinfo((keptkount * 3) + 1) := ordinates.COUNT + 1;
                     --inner ring is always a 2003
                     tempinfo((keptkount * 3) + 2) := 2003;
                     --etype is always a 1 (I think)
                     tempinfo((keptkount * 3) + 3) := 1;

                     --the extracted ordinates are actually reversed
                     --so go back to the geom_temp where the inner ring ordinates are still inners

                     --(1, 1003, 1, 2105, 2003, 1, 2113, 2003, 1, 2123, 2003, 1)
                     --For example start at ordinates 2105 go to 2112
                     ordstart := eleminfo((j*3) - 2);
                     ordend   := ordstart + inner_ring.sdo_ordinates.COUNT - 1;

                     ordinates.EXTEND(inner_ring.sdo_ordinates.COUNT);

                     FOR ii IN ordstart .. ordend
                     LOOP

                        IF ii = 1
                        THEN
                           ordkounter := ordinates.COUNT;
                        END IF;

                        ordkounter := ordkounter + 1;

                        ordinates(ordkounter) :=  geom_temp.sdo_ordinates(ii);


                     END LOOP;


                  END IF; --end if on keep or nokeep


               END IF;

               --no matter what, set this back to be safe
               rectified := 0;

            END LOOP;   --end loop over inner rings

            --we have put humpty back together again without any small inner rings
            --add this bit back to the full geometry

            humpty := SDO_GEOMETRY(2003,
                                   geom_in.SDO_SRID,
                                   NULL,
                                   tempinfo,
                                   ordinates);

            IF i = 1
            THEN
               geom_out := humpty;
            ELSE
               geom_out := SDO_UTIL.APPEND(geom_out,humpty);
            END IF;


            --some day Ill know which of these is correct
            tempinfo.DELETE;
            tempinfo := SDO_ELEM_INFO_ARRAY();
            ordinates.DELETE;
            ordinates := SDO_ORDINATE_ARRAY();
            keptkount := 0; --doh


         END IF;

      END LOOP; --end loop over this piece of the original


      --should do some checks here - make sure what we have is a polygon,
      --is valid, etc


      IF geom_out.SDO_GTYPE != 2003
      AND geom_out.SDO_GTYPE != 2007
      THEN

         IF p_log = 'Y'
         THEN

            EXECUTE IMMEDIATE logsql USING p_id,
                                           'Failed: Got gtype ' || geom_out.SDO_GTYPE,
                                           geom_out;
            COMMIT;

         END IF;

         RAISE_APPLICATION_ERROR(-20001,'Ended with gtype ' || geom_out.SDO_GTYPE || '. Whats the deal?');

      END IF;


      IF p_validate_out = 'Y'
      AND SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(geom_out,p_tolerance) != 'TRUE'
      THEN


         IF p_log = 'Y'
         THEN

            EXECUTE IMMEDIATE logsql USING p_id,
                                           'Failed: Got an invalid output geometry ',
                                           geom_out;
            COMMIT;

         END IF;

         RAISE_APPLICATION_ERROR(-20001,'Yo, geom out is not valid!');

      END IF;


      IF p_log = 'Y'
      THEN

         EXECUTE IMMEDIATE logsql USING p_id,
                                        'Finished: Remove_Holes',
                                        geom_out;
         COMMIT;

      END IF;


      RETURN geom_out;

   END REMOVE_HOLES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION EXTRACT_HOLES (
      geom_in           IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 5/11/11
      --Takes a geometry with inner rings (aka holes)
      --And returns just the inner rings as a new geometry

      --Sample call: select gz_utilities.extract_holes(a.sdogeometry)
      --from TAB10SL040_CF_AGGR a

      geom_temp      SDO_GEOMETRY;
      eleminfo       SDO_ELEM_INFO_ARRAY := SDO_ELEM_INFO_ARRAY();
      geom_out       SDO_GEOMETRY;
      inner_ring     SDO_GEOMETRY;


   BEGIN


      IF geom_in IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Hey buddy, EXTRACT_HOLES input geometry is NULL');

      END IF;


      IF geom_in.SDO_GTYPE != 2003
      AND geom_in.SDO_GTYPE != 2007
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Got gtype ' || geom_in.SDO_GTYPE || '. Whats the deal?');
      END IF;


      FOR i IN 1 .. SDO_UTIL.GETNUMELEM(geom_in)
      LOOP


         --Get each outer ring
         geom_temp := SDO_UTIL.EXTRACT(geom_in,i);

         eleminfo := geom_temp.SDO_ELEM_INFO;

         IF eleminfo.COUNT = 3
         THEN

            --no inner rings
            NULL;

         ELSE

            --we have inner rings
            --ELEM info is in triplets

            FOR j IN 1 .. (eleminfo.COUNT)/3
            LOOP


               IF j = 1
               THEN

                  --outer ring
                  NULL;

               ELSE

                  --get the inner ring
                  --element is always 1 since the EXTRACT up above guarantees a 2003
                  --ring is 2 or higher
                  inner_ring := SDO_UTIL.EXTRACT(geom_temp,1,j);

                  IF geom_out IS NULL
                  THEN

                     geom_out := inner_ring;

                  ELSE

                     geom_out := SDO_UTIL.APPEND(geom_out, inner_ring);

                  END IF;

               END IF; --end if on keep or nokeep

            END LOOP;   --end loop over inner rings

         END IF;

      END LOOP; --end loop over this piece of the original


      IF geom_out.SDO_GTYPE != 2003
      AND geom_out.SDO_GTYPE != 2007
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Got some sort of weird output ');

      ELSIF geom_out IS NULL
      THEN

         RETURN NULL;

      ELSE

         RETURN geom_out;

      END IF;


   END EXTRACT_HOLES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION MEASURE_SLIVER_WIDTH (
      geom_in           IN SDO_GEOMETRY,
      p_sample_kount    IN PLS_INTEGER DEFAULT 100,
      p_tolerance       IN NUMBER DEFAULT .00000005, --kinda expecting NULL srid from geodetic
      p_debug           IN NUMBER DEFAULT 0
   ) RETURN NUMBER DETERMINISTIC
   AS

      --Matt! 5/11/11
      --Takes as input a single outer ring
      --Assumes that the ring is very sliver-like.  Height to width ratio to the max
      --Attempts to find the max width line across the sliver
      --Then bisects the sliver into two halves based on the long axis
      --Measures the distance across, from one side to the other in the geom's units
      --increase p_sample_kount to get more accurate results, or lower for less accurate, faster performance
      --Tolerance must be granular enough to distinguish the sliver width.



      --sample call: select gz_utilities.MEASURE_SLIVER_WIDTH(SDO_GEOMETRY
      --                                                     (2003,NULL,NULL,SDO_ELEM_INFO_ARRAY(1,1003,3),SDO_ORDINATE_ARRAY(1,1, 5,7)),
      --                                                      100, .0000005) from dual
      --returns 4 since the rectangle gets bisected diagonally, and the horizontal width is 5-1

      geom_line         SDO_GEOMETRY;
      geom_pts          GZ_TYPES.geomarray;
      max_dist          NUMBER := 0;
      current_dist      NUMBER;
      measure           NUMBER := 0;
      bisect_pt1        PLS_INTEGER;
      bisect_pt2        PLS_INTEGER;
      ordinates_lower   SDO_ORDINATE_ARRAY :=  SDO_ORDINATE_ARRAY();
      ordinate_kount    PLS_INTEGER := 0;
      lower_geom        SDO_GEOMETRY;
      sliver_distance   NUMBER;
      project_pt        SDO_GEOMETRY;


   BEGIN

      IF geom_in.SDO_GTYPE != 2003
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Input gtype is ' || geom_in.sdo_gtype || ', expecting a 2003 ');

      END IF;

      geom_line := SDO_UTIL.POLYGONTOLINE(geom_in);

      IF p_debug = 1
      THEN
         dbms_output.put_line('verify polygton to line');
         dbms_output.put_line('SELECT ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(geom_line)) || ' FROM DUAL');
      END IF;


      FOR i in 1 .. p_sample_kount
      LOOP

         --GZ_LOCATE_PT uses a measure of 1000

         measure := measure + (1000/p_sample_kount);

         geom_pts(i) := GZ_CLIP.GZ_LOCATE_PT(geom_line,measure);

      END LOOP;

      FOR i in 1 .. geom_pts.COUNT
      LOOP

         --ex 1 (2 3 4 5 6 7 8 9 10)
         --   2 (3 4 5 6 7 8 9 10) ...
         --   9 (10)
         FOR j IN (i+1) .. geom_pts.COUNT
         LOOP

            current_dist := SDO_GEOM.SDO_DISTANCE(geom_pts(i),
                                                  geom_pts(j),
                                                  p_tolerance);

            IF current_dist > max_dist
            THEN

               max_dist := current_dist;
               bisect_pt1 := i;  --this is just a position in the geom_pts array
               bisect_pt2 := j;  --not a measure or a geom

            END IF;

         END LOOP;

      END LOOP;


      IF max_dist <= 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Got a max width of ' || max_dist || ' check ya tolerance and ya hole ');

      END IF;


      --Build just one geom for the lower side of the sliver
      --bisect_pt1 is a positive integer, and it stretches around to bisect_pt2
      --to form the "upper" side of the sliver.  We will just use those points as starters

      --bisect_pt2 to geom_pts.COUNT, then 1 to bisect_pt1
      --this is the lower side of the sliver
      --its ok that our two sides overlap at the shared endpoints
      --This will just be a distance of 0 and better be tossed

      ordinate_kount := 0;

      FOR ii IN bisect_pt2 .. geom_pts.COUNT
      LOOP

         ordinates_lower.EXTEND(2);

         ordinate_kount := ordinate_kount + 1;
         ordinates_lower(ordinate_kount) := geom_pts(ii).sdo_point.X;
         ordinate_kount := ordinate_kount + 1;
         ordinates_lower(ordinate_kount) := geom_pts(ii).sdo_point.Y;

      END LOOP;

      FOR ii IN 1 .. bisect_pt1
      LOOP

         ordinates_lower.EXTEND(2);

         ordinate_kount := ordinate_kount + 1;
         ordinates_lower(ordinate_kount) := geom_pts(ii).sdo_point.X;
         ordinate_kount := ordinate_kount + 1;
         ordinates_lower(ordinate_kount) := geom_pts(ii).sdo_point.Y;

      END LOOP;


      lower_geom := geom_line;
      lower_geom.sdo_ordinates := ordinates_lower;

      --we gots a series of points representing the upper half of the sliver
      --And a line representing the lower half of the sliver
      --Both geoms are likely densified from the original sliver geometry. For the best

      max_dist := 0;
      current_dist := 0;

      IF p_debug = 1
      THEN

         dbms_output.put_line('bisecting points are ');
         dbms_output.put_line('SELECT ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(geom_pts(bisect_pt1))) || ' FROM DUAL');
         dbms_output.put_line('SELECT ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(geom_pts(bisect_pt2))) || ' FROM DUAL');

      END IF;

      FOR ii IN bisect_pt1 .. bisect_pt2
      LOOP

         --for each ordinate in the upper set
         --project_pt onto the lower line and measure the distance
         --yes I know that the actual project_pt command has a distance IN OUT

         project_pt := GZ_CLIP.GZ_PROJECT_PT(geom_pts(ii),
                                             lower_geom,
                                             p_tolerance);

         current_dist := SDO_GEOM.SDO_DISTANCE(project_pt,
                                               geom_pts(ii),
                                               p_tolerance);

         IF current_dist > max_dist
         THEN

            max_dist := current_dist;

            IF p_debug = 1
            THEN

               dbms_output.put_line('got a max ' || current_dist || ' for top bisect point ' || ii);
               dbms_output.put_line('heres the two pts ');
               dbms_output.put_line('SELECT ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(project_pt)) || ' FROM DUAL');
               dbms_output.put_line('SELECT ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(geom_pts(ii))) || 'FROM DUAL');

            END IF;

         END IF;

      END LOOP;

      RETURN max_dist;

   END MEASURE_SLIVER_WIDTH;

   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------------------------

   FUNCTION GZ_UNION (
      geom1_in             IN SDO_GEOMETRY,
      geom2_in             IN SDO_GEOMETRY,
      p_tolerance          IN NUMBER DEFAULT .00000005,
      p_debug              IN NUMBER DEFAULT 1,
      p_recursive          IN NUMBER DEFAULT 0
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 6/25/13
      --Wrapper for buggy sdo_geom.sdo_union
      --Hopefully will be patched and wont have to use it

      output            SDO_GEOMETRY;
      deadman           PLS_INTEGER := 0;
      tolerance         NUMBER;

   BEGIN

      IF geom1_in IS NULL
      OR geom2_in IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Input geometry is NULL');

      END IF;

      IF geom1_in.sdo_srid IS NOT NULL
      OR geom2_in.sdo_srid IS NOT NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry, an input srid is not null.  This embarrassment only works on graph paper');

      END IF;

      IF (geom1_in.sdo_gtype <> 2003 AND geom1_in.sdo_gtype <> 2007)
      OR (geom2_in.sdo_gtype <> 2003 AND geom2_in.sdo_gtype <> 2007)
      THEN

         RAISE_APPLICATION_ERROR(-20001,'One of the inputs isnt a polygon.  Sorry, only wrote this mess for polys');

      END IF;

      IF p_tolerance IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Need a real tolerance, got null');

      ELSE

         tolerance := p_tolerance;

      END IF;


      LOOP

         IF p_debug = 1
         THEN

            dbms_output.put_line('using tolerance ' || tolerance || ' on try ' || (deadman + 1));

         END IF;

         BEGIN

            output := SDO_GEOM.SDO_UNION(geom1_in,
                                         geom2_in,
                                         tolerance);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%unable to construct spatial object%'
            THEN

               --Lazarus time
               --ORA-13050: unable to construct spatial object
               --ORA-06512: at "MDSYS.SDO_3GL", line 1606
               --ORA-06512: at "MDSYS.SDO_3GL", line 1646
               --ORA-06512: at "MDSYS.SDO_3GL", line 1750
               --ORA-06512: at "MDSYS.SDO_3GL", line 1844
               --ORA-06512: at "MDSYS.SDO_GEOM", line 1408
               --ORA-06512: at "MDSYS.SDO_GEOM", line 1493

               IF p_debug = 1
               THEN

                  dbms_output.put_line(SQLERRM || ' on try ' || (deadman + 1));

               END IF;

            ELSE

               RAISE;

            END IF;

         END;

         IF output IS NOT NULL
         AND output.sdo_gtype IN (2003,2007)
         AND sdo_geom.validate_geometry_with_context(output,p_tolerance) = 'TRUE'  --orig tolerance
         THEN

            EXIT;

         ELSE

            IF output IS NOT NULL
            AND output.sdo_gtype NOT IN (2003,2007)
            AND p_debug = 1
            THEN
               dbms_output.put_line('Got a geometry but a bad gtype');
            END IF;

            IF output IS NOT NULL
            AND sdo_geom.validate_geometry_with_context(output,p_tolerance) <> 'TRUE'
            AND p_debug = 1
            THEN
               dbms_output.put_line('Got a geometry but its not valid');
            END IF;

            deadman := deadman + 1;

            tolerance := tolerance / 10; --no idea

         END IF;

         IF deadman = 10
         AND p_recursive = 0
         THEN

            IF p_debug = 1
            THEN
               dbms_output.put_line('Going into the rabbit hole.  Rounding ordinates at ' || length(p_tolerance));
            END IF;

            --Recursive call with rounding to tolerance + 1 digits
            output := GZ_GEOM_UTILS.GZ_UNION(GZ_GEOM_UTILS.ORDINATE_ROUNDER(geom1_in, length(p_tolerance)), --decimal pt = +1
                                             GZ_GEOM_UTILS.ORDINATE_ROUNDER(geom2_in, length(p_tolerance)),
                                             p_tolerance,
                                             p_debug,
                                             (p_recursive+1));

            IF output IS NOT NULL
            THEN

               --for logging
               tolerance := p_tolerance;

               IF p_debug = 1
               THEN
                  dbms_output.put_line('Got an answer rounding ordinates');
               END IF;

               EXIT;

            END IF;

         ELSIF deadman > 10
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Out of ideas');

         END IF;

      END LOOP;

      IF p_debug = 1
      THEN

         dbms_output.put_line('Final tolerance used: ' || tolerance || ' on try ' || (deadman + 1));

      END IF;

      RETURN output;

   END GZ_UNION;

   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------------------------

   FUNCTION GZ_DIFFERENCE (
      geom1_in             IN SDO_GEOMETRY,
      geom2_in             IN SDO_GEOMETRY,
      p_tolerance          IN NUMBER DEFAULT .00000005,
      p_debug              IN NUMBER DEFAULT 1,
      p_recursive          IN NUMBER DEFAULT 0
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 6/25/13
      --Wrapper for buggy sdo_geom.sdo_difference
      --Desperate attempt to avoid failures in align_edges where output of difference is null

      output            SDO_GEOMETRY;
      deadman           PLS_INTEGER := 0;
      tolerance         NUMBER;

   BEGIN

      IF geom1_in IS NULL
      OR geom2_in IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Input geometry is NULL');

      END IF;

      IF geom1_in.sdo_srid IS NOT NULL
      OR geom2_in.sdo_srid IS NOT NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry, an input srid is not null.  This embarrassment only works on graph paper');

      END IF;

      IF (geom1_in.sdo_gtype <> 2003 AND geom1_in.sdo_gtype <> 2007)
      OR (geom2_in.sdo_gtype <> 2003 AND geom2_in.sdo_gtype <> 2007)
      THEN

         RAISE_APPLICATION_ERROR(-20001,'One of the inputs isnt a polygon.  Sorry, only wrote this mess for polys');

      END IF;

      IF p_tolerance IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Need a real tolerance, got null');

      ELSE

         tolerance := p_tolerance;

      END IF;


      LOOP

         IF p_debug = 1
         THEN

            dbms_output.put_line('using tolerance ' || tolerance || ' on try ' || (deadman + 1));

         END IF;

         BEGIN

            output := SDO_GEOM.SDO_DIFFERENCE(geom1_in,
                                              geom2_in,
                                              tolerance);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%unable to construct spatial object%'
            THEN

               --Lazarus time
               --ORA-13050: unable to construct spatial object
               --ORA-06512: at "MDSYS.SDO_3GL", line 1606
               --ORA-06512: at "MDSYS.SDO_3GL", line 1646
               --ORA-06512: at "MDSYS.SDO_3GL", line 1750
               --ORA-06512: at "MDSYS.SDO_3GL", line 1844
               --ORA-06512: at "MDSYS.SDO_GEOM", line 1408
               --ORA-06512: at "MDSYS.SDO_GEOM", line 1493

               IF p_debug = 1
               THEN

                  dbms_output.put_line(SQLERRM || ' on try ' || (deadman + 1));

               END IF;

            ELSE

               RAISE;

            END IF;

         END;

         IF output IS NOT NULL
         AND output.sdo_gtype IN (2003,2007)
         AND sdo_geom.validate_geometry_with_context(output,p_tolerance) = 'TRUE'  --orig tolerance
         THEN

            EXIT;

         ELSE

            IF output IS NOT NULL
            AND output.sdo_gtype NOT IN (2003,2007)
            AND p_debug = 1
            THEN
               dbms_output.put_line('Got a geometry but a bad gtype');
            END IF;

            IF output IS NOT NULL
            AND sdo_geom.validate_geometry_with_context(output,p_tolerance) <> 'TRUE'
            AND p_debug = 1
            THEN
               dbms_output.put_line('Got a geometry but its not valid');
            END IF;

            deadman := deadman + 1;

            tolerance := tolerance / 10; --no idea

         END IF;

         IF deadman = 10
         AND p_recursive = 0
         THEN

            IF p_debug = 1
            THEN
               dbms_output.put_line('Going into the rabbit hole.  Rounding ordinates at ' || length(p_tolerance));
            END IF;

            --Recursive call with rounding to tolerance + 1 digits
            output := GZ_GEOM_UTILS.GZ_DIFFERENCE(GZ_GEOM_UTILS.ORDINATE_ROUNDER(geom1_in, length(p_tolerance)), --decimal pt = +1
                                                  GZ_GEOM_UTILS.ORDINATE_ROUNDER(geom2_in, length(p_tolerance)),
                                                  p_tolerance,
                                                  p_debug,
                                                  (p_recursive+1));

            IF output IS NOT NULL
            THEN

               --for logging
               tolerance := p_tolerance;

               IF p_debug = 1
               THEN
                  dbms_output.put_line('Got an answer rounding ordinates');
               END IF;

               EXIT;

            END IF;

         ELSIF deadman > 10
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Out of ideas');

         END IF;

      END LOOP;

      IF p_debug = 1
      THEN

         dbms_output.put_line('Final tolerance used: ' || tolerance || ' on try ' || (deadman + 1));

      END IF;

      RETURN output;

   END GZ_DIFFERENCE;

   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------------------------

   FUNCTION GET_XYS(
      Geom IN MDSYS.SDO_GEOMETRY,
      ring1_to_find PLS_INTEGER,
      edge1_to_find PLS_INTEGER,
      ring2_to_find PLS_INTEGER DEFAULT NULL,
      edge2_to_find PLS_INTEGER DEFAULT NULL
   ) RETURN sdo_geometry
   AS


      /**
      ---
      -- Program Name: GetXys
      -- Author: Sidey + Matt!
      -- Creation Date:  June? 2009
      -- Updated: Feb 5/2010 to return a range of segments and preserve SRID: Sidey
      -- 11/17/11: Moved to utilities, calling it from several spots in production code
      --
      -- Usage:
      --  validate gives : 13349 [Element <1>] [Ring <1>][Edge <20028>][Edge <20040>]
      --  select MATTOOL.get_xys(a.sdogeometry,1,20028,NULL,20040) from
      --  j12802711008001_srcwrk a where a.oid = 4453141
      --
      -- Purpose: Return a segment, 2 segments or a range of segments from a geometry
      --
      -- Method: Makes a new geometry
      -- Dependencies: None
      --
      --
      */

       XYOrd             MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
       Info_Array        MDSYS.SDO_ELEM_INFO_ARRAY;

       geometry          MDSYS.SDO_GEOMETRY;



       xlast     NUMBER;
       ylast     NUMBER;
       xnew      NUMBER;
       ynew      NUMBER;
       x2last    NUMBER;
       y2last    NUMBER;
       x2new     NUMBER;
       y2new     NUMBER;
       k         PLS_INTEGER := -1;

       rings     PLS_INTEGER;
       LB        PLS_INTEGER;
       UB        PLS_INTEGER;
       j         PLS_INTEGER;
       kount     PLS_INTEGER;

       GTYPE     NUMBER:= geom.SDO_GTYPE;
       SRID      NUMBER:= geom.SDO_SRID;

       retval   SDO_GEOMETRY;


   BEGIN

        If Gtype = 2001 or Gtype = 2004 then

         dbms_output.put_line('forget about it');

        End If;

        Info_Array := geom.SDO_ELEM_INFO;
        XYORD := geom.SDo_Ordinates;
        rings := TRUNC(Info_Array.count/3);
        j := (ring1_to_find-1) *3 + 1;

        dbms_output.put_line('j is ' || j);
        LB := Info_Array(j) + (abs(edge1_to_find) -1) *2;    --  ( I presume he gives the relative segment number)

        dbms_output.put_line('LB is ' || LB);
        dbms_output.put_line('ord count is ' || XYORD.count);

        xlast := XYOrd(LB);
        ylast := XYOrd(LB+1);
        xnew := XYOrd(LB+2);
        ynew := XYOrd(LB+3);

        IF ring2_to_find IS NOT NULL
        THEN

           j := (ring2_to_find-1) *3 + 1;

        END IF;

        if edge2_to_find is NULL then
          retval := sdo_geometry (2002, SRID, null,
                                 sdo_elem_info_array (1,2,1),
                                 sdo_ordinate_array (xlast,ylast, xnew,ynew));

         -- Return a range:
        elsif (edge1_to_find < 0 or edge2_to_find < 0) and ring1_to_find = ring2_to_find then

           kount := ((abs(edge2_to_find) - abs(edge1_to_find)) +2) *2;
           if kount > XyOrd.count then
             kount := XyOrd.count;
           end if;

         For ii in 1..kount Loop
            XYOrd(ii) := XYOrd(LB);
            LB := LB + 1;
         End Loop;

         XYOrd.trim(XYOrd.count-kount);
        retval := sdo_geometry (2002, SRID, null,
                                 sdo_elem_info_array (1,2,1),
                                 XYOrd);

        else

          LB := Info_Array(j) + (edge2_to_find -1) *2;    --  ( I presume he gives the relative segment number)

          x2last := XYOrd(LB);
          y2last := XYOrd(LB+1);
          x2new  := XYOrd(LB+2);
          y2new  := XYOrd(LB+3);
          retval := sdo_geometry (2006, SRID, null,
                                 sdo_elem_info_array (1,2,1, 5,2,1),
                                 sdo_ordinate_array (xlast,ylast, xnew,ynew,
                                                     x2last,y2last, x2new,y2new));
        end if;

        RETURN retval;

   END Get_Xys;

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE GZ_ALIGN_EDGES (
      p_tab_name              IN VARCHAR2,
      p_geom_col_name         IN VARCHAR2,
      p_out_tab_name          IN VARCHAR2,
      p_tolerance             IN NUMBER DEFAULT .0000005,  --8265 NULL SRID .05 tol
      p_drop_out_tab          IN VARCHAR2 DEFAULT 'N',
      p_use_gz_difference     IN VARCHAR2 DEFAULT 'N'
   )
   AS

      --Matt! 5/13/11
      --This fella is still kind of in progress.  Not sure what I want here exactly
      --Wrapper for Dan Geringer align_edges
      --Input is expected to be a normal 8265 table with no spatial index (ie a work table)
      --6/10/11 Switched to "NULL" srid tolerance and expect NULL srid input
      --6/25/13 Moved to GZ_GEOM_UTILS and added "use gz difference" option

      --NB: There is no checking of output results in here. Caller beware of 2004s and other junk

      --Goal is to simply update that table with aligned sdo_geometries
      -- 1. creates special NULL srid, updates metadata and sidx on input table
      -- 2. also creates special Dan G required temp rowid + geom table
      -- 3. still outputs that temp table though now you can drop it, and also control the sdogeometry col
      -- 4. then updates the geom col of the input table
      -- 5. Rebuilds input table srid, metadata, and index back to real world
      --   ***Make a backup of the original geom if you need it***

      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;
      tolerance      NUMBER := p_tolerance;
      srid           NUMBER;


   BEGIN


      --Need SRID no matter what
      psql := 'SELECT a.' || p_geom_col_name || '.sdo_srid '
           || 'FROM '
           ||  p_tab_name || ' a '
           || 'WHERE rownum = 1 ';

      EXECUTE IMMEDIATE psql INTO srid;

      IF srid IS NOT NULL
      THEN

         --update input table to NULL srid
         psql := 'UPDATE ' || p_tab_name || ' a '
              || 'SET '
              || 'a.' || p_geom_col_name || '.sdo_srid  = NULL ';

         EXECUTE IMMEDIATE psql;
         COMMIT;

      END IF;


      --must update input table (or create for first time) to NULL SRID in both metadata and SIDX
      GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(UPPER(p_tab_name),
                                     UPPER(p_geom_col_name),
                                     NULL,
                                     p_tolerance); --original real tolerance in metadata for spatial queries



      --create table expected by Dan G align edges
      --order does matter due to insert statements
      --do not like this naked create table
      psql := 'CREATE TABLE ' || p_out_tab_name || ' '
           || '(sdo_rowid ROWID, ' || p_geom_col_name || ' SDO_GEOMETRY)';

      BEGIN
         EXECUTE IMMEDIATE psql;
      EXCEPTION
      WHEN OTHERS THEN
         EXECUTE IMMEDIATE 'DROP TABLE ' || p_out_tab_name;
         EXECUTE IMMEDIATE psql;
      END;


      --the bad boy
      GZ_GEOM_UTILS.ALIGN_EDGES(UPPER(p_tab_name),
                               UPPER(p_geom_col_name),
                               UPPER(p_out_tab_name),
                               tolerance,              --special cartesian tolerance
                               p_use_gz_difference);


      --now have output table with rowid and sdogeometry
      --update input table name with new geom

      psql := 'UPDATE ' || p_tab_name || ' a '
           || 'SET '
           || 'a.' || p_geom_col_name || ' = '
           ||    '(SELECT b.' || p_geom_col_name || ' '
           ||    'FROM '
           ||    p_out_tab_name || ' b '
           ||    'WHERE a.rowid = b.sdo_rowid) ';

      EXECUTE IMMEDIATE psql;
      COMMIT;


      --update back to original SRID and no bogus sidx

      --not robust chump
      psql := 'DROP INDEX ' || p_tab_name || '_SIDX ';
      EXECUTE IMMEDIATE psql;

      IF srid IS NOT NULL
      THEN

         psql := 'UPDATE ' || p_tab_name || ' a '
              || 'SET '
              || 'a.' || p_geom_col_name || '.sdo_srid  = :p1 ';

         EXECUTE IMMEDIATE psql USING srid;
         COMMIT;

      END IF;


      IF p_drop_out_tab = 'Y'
      THEN

         EXECUTE IMMEDIATE 'DROP TABLE ' || p_out_tab_name || ' ';

      END IF;

   END GZ_ALIGN_EDGES;

   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------


   PROCEDURE ALIGN_EDGES (
      geom_table_name         VARCHAR2,
      geom_column_name        VARCHAR2,
      output_table_name       VARCHAR2,
      tolerance               NUMBER,
      use_gz_difference       VARCHAR2 DEFAULT 'N'
   )
   AS

      --Donated to CPB by Dan Geringer
      --Added here 12/16/2010 Matt!
      --See http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_Questions_For_Dan_Geringer
      --  for sample usages
      --Matt! Updated to parameterize output geom column name 5/13/11
      --    See gz_geom_utils.GZ_ALIGN_EDGES for wrapper

      --Matt! 6/25/13 added option to use gz_difference since sdo_geom.sdo_difference is flaky as flak
      --              Its returning NULL differences.  May be patched soonish, not sure

      -- You can control the output table name.
      --
      -- The output table name must contain two columns,
      --   with the following column_names,
      --   declared in the following order:
      --
      --   sdo_rowid ROWID
      --   <geom_column_name>     SDO_GEOMETRY



     a_rowid        ROWID;
     b_rowid        ROWID;
     update_window  BOOLEAN;
     update_next    BOOLEAN;
     window_geom    SDO_GEOMETRY;
     next_geom      SDO_GEOMETRY;

     type        cursor_type is REF CURSOR;
     query_crs   cursor_type;
     query_str   VARCHAR2(1000) :=
       'SELECT /*+ ordered use_nl (a b) use_nl (a c) */ a.rowid1 a_rowid, a.rowid2 b_rowid ' ||
       'FROM TABLE (SDO_JOIN ('''  || geom_table_name || ''',''' || geom_column_name || ''',' ||
                              '''' || geom_table_name || ''',''' || geom_column_name || ''')) a, ' ||
            geom_table_name || ' b,' ||
            geom_table_name || ' c ' ||
       'WHERE a.rowid1=b.rowid '     ||
         'AND a.rowid2=c.rowid '     ||
         'AND a.rowid1 < a.rowid2 '  ||
         'AND sdo_geom.relate (b.' || geom_column_name || ' , ''anyinteract'',' ||
                              'c.' || geom_column_name || ' , ' || tolerance || ') = ''TRUE''';

   BEGIN

     OPEN query_crs FOR query_str;

     LOOP
       FETCH query_crs INTO a_rowid, b_rowid;
       EXIT WHEN query_crs%NOTFOUND;

       BEGIN
         EXECUTE IMMEDIATE ' SELECT '|| geom_column_name ||
                           ' FROM ' || output_table_name ||
                           ' WHERE sdo_rowid = ''' || a_rowid || ''''
                           INTO window_geom;

         update_window := true;
       EXCEPTION
         WHEN NO_DATA_FOUND THEN
           EXECUTE IMMEDIATE ' SELECT ' || geom_column_name ||
                             ' FROM '   || geom_table_name  ||
                             ' WHERE rowid = ''' || a_rowid || ''''
                             INTO window_geom;
           update_window := false;
       END;


       BEGIN
         EXECUTE IMMEDIATE ' SELECT ' || geom_column_name ||
                           ' FROM ' || output_table_name ||
                           ' WHERE sdo_rowid = ''' || b_rowid || ''''
                           INTO next_geom;
         update_next := true;
       EXCEPTION
         WHEN NO_DATA_FOUND THEN
           EXECUTE IMMEDIATE ' SELECT ' || geom_column_name ||
                             ' FROM '   || geom_table_name  ||
                             ' WHERE rowid = ''' || b_rowid || ''''
                             INTO next_geom;
           update_next := false;
       END;

       IF use_gz_difference = 'N'
       THEN

          window_geom := SDO_GEOM.SDO_DIFFERENCE (window_geom, next_geom, tolerance);
          next_geom := SDO_GEOM.SDO_DIFFERENCE (next_geom, window_geom, tolerance);

       ELSE

          --dbms_output.put_line(gz_geom_utils.dump_sdo(window_geom));
          --dbms_output.put_line(gz_geom_utils.dump_sdo(next_geom));
          window_geom := GZ_GEOM_UTILS.GZ_DIFFERENCE (window_geom, next_geom, tolerance);
          next_geom := GZ_GEOM_UTILS.GZ_DIFFERENCE (next_geom, window_geom, tolerance);

       END IF;

       IF update_next = true
       THEN
         EXECUTE IMMEDIATE ' UPDATE ' || output_table_name   ||
                           ' SET ' || geom_column_name || ' = :next_geom '         ||
                           ' WHERE sdo_rowid = ''' || b_rowid || ''''
                             USING next_geom;
       ELSE
         EXECUTE IMMEDIATE ' INSERT INTO ' || output_table_name ||
                           ' vALUES (:b_rowid, :next_geom)'
                           USING b_rowid, next_geom;
       END IF;


       IF update_window = true
       THEN

         EXECUTE IMMEDIATE ' UPDATE ' || output_table_name   ||
                           ' SET ' || geom_column_name || ' = :window_geom '         ||
                           ' WHERE sdo_rowid = ''' || a_rowid || ''''
                             USING window_geom;

       ELSE
         EXECUTE IMMEDIATE ' INSERT INTO ' || output_table_name ||
                           ' vALUES (:a_rowid, :window_geom)'
                           USING a_rowid, window_geom;
       END IF;

       COMMIT;
     END LOOP;

     CLOSE query_crs;

     -- Also, include islands that don't share boundaries with any other feature
     EXECUTE IMMEDIATE ' INSERT INTO ' || output_table_name ||
                       ' SELECT rid, ' || geom_column_name ||
                       ' FROM (SELECT rowid rid' ||
                             ' FROM ' || geom_table_name ||
                             ' MINUS' ||
                             ' SELECT sdo_rowid rid' ||
                             ' FROM ' || output_table_name || ') a,' ||
                              geom_table_name || ' b'||
                       ' WHERE a.rid = b.rowid';
     COMMIT;

   END ALIGN_EDGES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------
   PROCEDURE GZ_DUMP_SDO_EDGE_ARRAY (
      p_input         IN MDSYS.SDO_EDGE_ARRAY
   )
   AS

      --Matt! 1/22/11
      --sdo edge array is so wack
      --debugger helper to look at it

      testnumarray      SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY();

   BEGIN

      FOR i in 1 .. p_input.COUNT
      LOOP

         dbms_output.put_line(' ');
         dbms_output.put_line('EDGE ' || i);

         testnumarray := p_input(i);

         FOR j IN 1 .. testnumarray.COUNT
         LOOP

            dbms_output.put_line('   ' || testnumarray(j));

         END LOOP;

      END LOOP;


   END GZ_DUMP_SDO_EDGE_ARRAY;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------


   FUNCTION REMOVE_CLOSE_ORDINATES (
      p_geom          IN SDO_GEOMETRY,
      p_distance      IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 2/7/11
      --Called from an optional error handler in gz_change_edge_coords

      new_ordinates     SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();
      output            SDO_GEOMETRY;
      ordkount          PLS_INTEGER := 0;
      got_dupe          PLS_INTEGER := 0;
      distance          NUMBER;
      x1                NUMBER;
      y1                NUMBER;
      x2                NUMBER;
      y2                NUMBER;


   BEGIN

      IF p_geom.sdo_gtype != 2002
      THEN

         RAISE_APPLICATION_ERROR(-20001,'We only crush 2002s, got gtype ' || p_geom.sdo_gtype);

      END IF;

      IF p_geom.sdo_ordinates.COUNT = 4
      THEN

         --Only calling this in an exception handler where I expect this to fix my problem
         RAISE_APPLICATION_ERROR(-20001,'Cant help ya, ordinate count is ' || p_geom.sdo_ordinates.COUNT);

      END IF;

      FOR i IN 1 .. (p_geom.sdo_ordinates.COUNT)/2
      LOOP

        IF i = 1
        OR i = (p_geom.sdo_ordinates.COUNT)/2
        THEN


            --transfer ordinates unchanged
            new_ordinates.EXTEND(2);
            ordkount := ordkount + 1;

            new_ordinates(ordkount) := p_geom.sdo_ordinates((i * 2) - 1);
            ordkount := ordkount + 1;
            new_ordinates(ordkount) := p_geom.sdo_ordinates(i * 2);


        ELSE

           x1 := p_geom.sdo_ordinates((i * 2) - 1);
           y1 := p_geom.sdo_ordinates(i * 2);
           x2 := p_geom.sdo_ordinates((i * 2) - 3);
           y2 := p_geom.sdo_ordinates((i * 2) - 2);

           distance := GZ_GEOM_UTILS.ACCURATE_GCD(x1,y1,x2,y2);


           IF distance > p_distance
           THEN

              --want you too, distance is good
              new_ordinates.EXTEND(2);
              ordkount := ordkount + 1;
              new_ordinates(ordkount) := p_geom.sdo_ordinates((i * 2) - 1);
              ordkount := ordkount + 1;
              new_ordinates(ordkount) := p_geom.sdo_ordinates(i * 2);

           ELSE

              --dont want you
              got_dupe := 1;

           END IF;

        END IF;


      END LOOP;

      IF got_dupe = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Didnt find a dupe to remove!');

      END IF;


      output := p_geom;
      output.sdo_ordinates := new_ordinates;

      RETURN output;


   END REMOVE_CLOSE_ORDINATES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION GZ_LOCATE_PT_DISTANCE (
      p_edge_geom       IN SDO_GEOMETRY,
      p_distance        IN NUMBER,
      p_tip             IN VARCHAR2,
      p_tolerance       IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 1/22/11
      --Kinda like gz_clip.gz_locate_pt
      --This version uses a distance input instead of a measure
      --Called from REMOVE_DUPE_XPAND_V currently



      output         SDO_GEOMETRY;
      measure        NUMBER;
      line           SDO_GEOMETRY;

   BEGIN



      --get the percent of the total length of the edge
      measure := ( (p_distance/SDO_GEOM.SDO_LENGTH(p_edge_geom, p_tolerance)) * 100 );

      line := SDO_LRS.CONVERT_TO_LRS_GEOM(p_edge_geom, 0, 100);

      IF p_tip = 'START'
      THEN

         output := SDO_LRS.CONVERT_TO_STD_GEOM(SDO_LRS.LOCATE_PT(line, measure));

      ELSIF p_tip = 'END'
      THEN

         output := SDO_LRS.CONVERT_TO_STD_GEOM(SDO_LRS.LOCATE_PT(line, (100 - measure)));

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Whats a tip of ' || p_tip || '?');

      END IF;


      RETURN output;


   END GZ_LOCATE_PT_DISTANCE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION WHERE_GEOM (
      g              in sdo_geometry
   ) RETURN varchar2 deterministic
   AS

      --Matt! 4/6/12
      --Donated to CPB by Sreeni "Benedict Arnold" Karpurapu
      --Determines if a geometry migrated from 10G to 11G manifests the leading zeroes jdbc issue
      --Key is to_char(geom) returns null
      --If TRUE see ID_GEOM or IMPORT_SDOTOPO11G below

      --sample
      --select gz_utilities.where_geom(a.sdogeometry) from tab10st10.county a
      --where rownum = 1

   begin

     if g is null then

       return 'FALSE';

     end if;

     if g.sdo_point is not NULL then

       if g.sdo_point.x is not NULL and to_char(g.sdo_point.x) is null then
           return 'TRUE';
       end if;

       if g.sdo_point.y is not NULL and to_char(g.sdo_point.y) is null then
           return 'TRUE';
       end if;

       if g.sdo_point.z is not NULL and to_char(g.sdo_point.z) is null then
           return 'TRUE';
       end if;

     end if;

     if g.sdo_ordinates is not null then

       for idx in 1..g.sdo_ordinates.count loop

         if g.sdo_ordinates(idx) is not null and
         to_char(g.sdo_ordinates(idx)) is null then

           return 'TRUE';

         end if;

       end loop;

     end if;

     return 'FALSE';

   end WHERE_GEOM;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION ID_GEOM (
      g        IN SDO_GEOMETRY
   )
   RETURN SDO_GEOMETRY deterministic
   AS

      --Matt! 3/30/11
      --Donated by Siva Ravada via Oracle Spatial discussion forum
      --http://forums.oracle.com/forums/thread.jspa?threadID=2199241&tstart=0
      --Corrects geometries exported from 10g and imported into 11g
      --See IMPORT_SDOTOPO11G wrapper below


      g1 sdo_geometry ;
      idx number;
      ords sdo_ordinate_array;
      x number;
      y number;
      z number;
      point_t sdo_point_type;

   BEGIN

      if (g.sdo_point is not NULL)
      then

         x := NULL;
         y := NULL;
         z := NULL;


         if (g.sdo_point.x is not NULL)
         then

            x := g.sdo_point.x * 10;
            x := x / 10;

         end if;

         if (g.sdo_point.y is not NULL)
         then

            y := g.sdo_point.y * 10;
            y := y / 10;

         end if;

         if (g.sdo_point.z is not NULL)
         then

            z := g.sdo_point.z * 10;
            z := z / 10;

         end if;

         point_t := SDO_POINT_TYPE(x,y,z);

      else

         point_t := NULL;

      end if;

      if (g.sdo_ordinates is not NULL)
      then

         ords := sdo_ordinate_array();
         ords.extend(g.sdo_ordinates.count);

         for idx in 1 .. ords.count
         loop

            x := g.sdo_ordinates(idx);
            x := (x*10.0);
            x := x/10.0;
            ords(idx) := x;

         end loop;

         g1 := sdo_geometry(g.sdo_gtype, g.sdo_srid,point_t,g.sdo_elem_info,ords);

      else

         g1 := sdo_geometry(g.sdo_gtype, g.sdo_srid,point_t,g.sdo_elem_info, NULL);

      end if;

      RETURN g1;

   END ID_GEOM;

    ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

    FUNCTION DUMP_STRING_ENDPOINTS (
      geom1   IN SDO_GEOMETRY,
      geom2   IN SDO_GEOMETRY DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      FUNCTION DUMP_IT (
         geom SDO_GEOMETRY
      ) RETURN VARCHAR2
      AS
         p PLS_INTEGER;
      BEGIN
         p := geom.SDO_ORDINATES.COUNT();
         RETURN geom.SDO_ORDINATES(1) || ' , '
             || geom.SDO_ORDINATES(2) || ' <-> '
             || geom.SDO_ORDINATES(p-1) || ' , '
             || geom.SDO_ORDINATES(p);
      END DUMP_IT;
   BEGIN

      IF geom1.SDO_GTYPE != 2002
      THEN
         RAISE_APPLICATION_ERROR(-20001,'expected 2002 but got ' || geom1.SDO_GTYPE || '!');
      END IF;

      IF geom2 IS NOT NULL
      THEN
         RETURN DUMP_IT(geom1) || Chr(10) || DUMP_IT(geom2);
      ELSE
         RETURN DUMP_IT(geom1);
      END IF;

   END DUMP_STRING_ENDPOINTS;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION DUMP_SDO_SUBELEMENTS (
      geom    IN SDO_GEOMETRY,
      indent  IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB
   AS
      output    CLOB;
   BEGIN

      IF geom.SDO_GTYPE = 2004
      OR geom.SDO_GTYPE = 2006
      OR geom.SDO_GTYPE = 2007
      THEN

         FOR i IN 1 .. SDO_UTIL.GETNUMELEM(geom)
         LOOP
            IF output IS NULL
            THEN
               output := DUMP_SDO(SDO_UTIL.EXTRACT(geom,i),indent);
            ELSE
               output := output || Chr(10) || DUMP_SDO(SDO_UTIL.EXTRACT(geom,i),indent);
            END IF;
         END LOOP;

         RETURN output;

      ELSE

         RETURN DUMP_SDO(geom,indent);

      END IF;

   END DUMP_SDO_SUBELEMENTS;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------


   FUNCTION DUMP_SDO_SRID (
      srid    IN NUMBER,
      indent  IN VARCHAR2 DEFAULT ''
   ) RETURN VARCHAR2

   AS

      --wasnt handling NULL srids till 5/12/11

   BEGIN

      IF srid IS NULL
      THEN

         RETURN 'NULL';

      ELSE

         RETURN TO_CHAR(srid);

      END IF;


   END DUMP_SDO_SRID;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION DUMP_SDO (
      geom    IN SDO_GEOMETRY,
      indent  IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB
   AS
      output CLOB := '';

   BEGIN
      output := indent || 'SDO_GEOMETRY' || Chr(10)
             || indent || '('            || Chr(10)
             || indent || '   ' || TO_CHAR(geom.SDO_GTYPE) || ',' || Chr(10)
             || indent || '   ' || dump_sdo_srid(geom.SDO_SRID,indent)  || ',' || Chr(10)
             || indent ||       dump_sdo_point(geom.SDO_POINT,indent)    || ',' || Chr(10)
             || indent ||       dump_sdo_elem(geom.SDO_ELEM_INFO,indent) || ',' || Chr(10)
             || indent ||       dump_sdo_ords(geom.SDO_ORDINATES,indent) || Chr(10)
             || indent || ')' || Chr(10);

      RETURN output;

   END DUMP_SDO;



   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------


   FUNCTION DUMP_SDO_POINT (
      geom    IN SDO_POINT_TYPE,
      indent  IN VARCHAR2 DEFAULT ''
   ) RETURN VARCHAR2

   AS
      output VARCHAR2(4000) := '';
      X VARCHAR2(64);
      Y VARCHAR2(64);
      Z VARCHAR2(64);

   BEGIN
      IF geom IS NULL
      THEN
         RETURN indent || '   NULL';
      END IF;

      IF geom.X IS NULL
      THEN
         X := 'NULL';
      ELSE
         X := TO_CHAR(geom.X);
      END IF;

      IF geom.Y IS NULL
      THEN
         Y := 'NULL';
      ELSE
         Y := TO_CHAR(geom.Y);
      END IF;

      IF geom.Z IS NULL
      THEN
         Z := 'NULL';
      ELSE
         Z := TO_CHAR(geom.Z);
      END IF;

      output := indent || '   SDO_POINT_TYPE'    || Chr(10)
             || indent || '   ('            || Chr(10)
             || indent || '      ' || X  || ',' || Chr(10)
             || indent || '      ' || Y  || ',' || Chr(10)
             || indent || '      ' || Z  || Chr(10)
             || indent || '   )';

      RETURN output;

   END DUMP_SDO_POINT;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION DUMP_SDO_ELEM (
      geom    IN SDO_ELEM_INFO_ARRAY,
      indent  IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB

   AS
      output CLOB := '';

   BEGIN
      IF geom IS NULL
      THEN
         RETURN indent || '   NULL';
      END IF;

      output := indent || '   SDO_ELEM_INFO_ARRAY' || Chr(10)
             || indent || '   ('                   || Chr(10);

      FOR i IN geom.FIRST .. geom.LAST
      LOOP
         output := output || indent || '      ' || TO_CHAR(geom(i));
         IF i != geom.LAST
         THEN
            output := output || indent || ',' || Chr(10);
         END IF;
      END LOOP;

      RETURN output || Chr(10) || indent || '   )';

   END DUMP_SDO_ELEM;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION DUMP_SDO_ORDS (
      geom    IN SDO_ORDINATE_ARRAY,
      indent  IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB

   AS
      output CLOB := '';

   BEGIN
      IF geom IS NULL
      THEN
         RETURN indent || '   NULL';
      END IF;

      output := indent || '   SDO_ORDINATE_ARRAY' || Chr(10)
             || indent || '   ('                  || Chr(10);

      FOR i IN geom.FIRST .. geom.LAST
      LOOP
         output := output || indent || '      ' || TO_CHAR(geom(i));
         IF i != geom.LAST
         THEN
            output := output || indent || ',' || Chr(10);
         END IF;
      END LOOP;

      RETURN output || Chr(10) || indent || '   )';

   END DUMP_SDO_ORDS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION FIX_INVALID_GEOMETRIES (
      p_schema          IN VARCHAR2,
      p_table_name      IN VARCHAR2,
      p_pkccol          IN VARCHAR2,
      p_gtype           IN NUMBER,
      p_topo            IN VARCHAR2,
      p_column_name     IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_flag_col        IN VARCHAR2 DEFAULT 'QC',
      p_intersect_size  IN NUMBER DEFAULT NULL,  --clip only
      p_intersect_ratio IN NUMBER DEFAULT NULL   --clip only
   ) RETURN VARCHAR2
   AS

      --Matt! 7/20/10
      --Update 11/12/10 with guess at codes for QC
      --8/5/11 moved from Clip to Utilities.  Shared by merge now

      --BY ANY MEANS NECESSARY
      --Basic process:
      -- 1. Get bad ids
      -- 2. Flag with low qc val
      -- 3. Attempt to fix with simplest available technique
      -- 4. Flag with higher qc val any not fixed
      -- 5. Repeat from 3 with list of remaining bad ids

      --Remove duplicate vertices first
      --Still bad?  Self-union
      --Still bad?  Try rectify
      --Still bad?  <??>
      --Maybe this should go in GZ_Utilities?

      --Not really documented, but NB
      --validate will return 13356 (dups) first because it cant calc self intersects with dups
      --Geoms without dups but with self intersect get 13349


      --QC Values--FACE table------------
      --0 - Fixed in SDO: Dup vertices
      --1 - Unfixed in SDO: Dup vertices
      --2 - Fixed in SDO: Self intersect below clip_intersect_size:
      --4 - Fixed in SDO: Self intersect touching universal face with large perimeter to area ratio
      --5 - Fixed in SDO: Otherself intersects
      --6 - Unfixed in SDO: Self intersect bordering universal face
      --7 - Unfixed in SDO: Other self intersects
      --8 - Unfixed in SDO: Wrong GTYPE
      --9 - Unfixed in SDO: Other validation error
      -----------------------------------

      duppsql           VARCHAR2(4000);
      intpsql           VARCHAR2(4000);
      psql              VARCHAR2(4000);
      badkount          PLS_INTEGER;
      dup_ids           GZ_TYPES.stringarray;
      post_dups         GZ_TYPES.stringarray;
      current_list      GZ_TYPES.stringarray;
      int_ids           GZ_TYPES.stringarray;
      int_ids2          GZ_TYPES.stringarray;
      int_ids3          GZ_TYPES.stringarray;
      int_fixed         GZ_TYPES.stringarray;
      small_fixed       GZ_TYPES.stringarray;
      ratio_fixed       GZ_TYPES.stringarray;
      other_bads        GZ_TYPES.stringarray;
      final_bad_ids     NUMBER;
      bad_gtypes        GZ_TYPES.stringarray;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FIX_INVALID_GEOMETRIES: Start');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_gtype NOT IN (2002,2003,2007)
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry dude, I never gave a thought to gtype ' || p_gtype);

      END IF;

      --DUPS

      duppsql := 'SELECT a.' || p_pkccol ||  ' '
              || 'FROM ' || p_table_name || ' a '
              || 'WHERE SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.' || p_column_name || ',:p1) LIKE :p2 ';

      EXECUTE IMMEDIATE duppsql BULK COLLECT INTO dup_ids USING p_tolerance,
                                                                '13356%';


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FIX_INVALID_GEOMETRIES: Dups');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF dup_ids.COUNT > 0
      THEN

         --Flag with 0 under assumption we will fix them all
         --These are guys that have at least dup vertices, or something worse

         FORALL ii IN 1..dup_ids.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_table_name || ' a '
                           || 'SET a.' || p_flag_col || ' = :p1 '
                           || 'WHERE a.' || p_pkccol || ' = :p2 '
            USING 0,
                  dup_ids(ii);

         COMMIT;

      END IF;


      --Attempt to remove duplicate vertices first
      --(least invasive first)

      IF dup_ids.COUNT > 0
      THEN

         psql := 'UPDATE ' || p_table_name || ' a '
              || 'SET '
              || 'a.' || p_column_name || ' = SDO_UTIL.REMOVE_DUPLICATE_VERTICES(a.sdogeometry,:p1) '
              || 'WHERE a.' || p_pkccol || ' IN (SELECT * FROM TABLE(:p2)) '
              || 'AND SDO_UTIL.GETNUMVERTICES(a.' || p_column_name || ') > :p3 ';

         IF p_gtype IN (2003,2007) --eh, should really bust any 2007s up into components
         THEN

            EXECUTE IMMEDIATE psql USING p_tolerance,
                                         GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(dup_ids),
                                         8;

            COMMIT;

         ELSIF p_gtype = 2002
         THEN

            EXECUTE IMMEDIATE psql USING p_tolerance,
                                         GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(dup_ids),
                                         6;

            COMMIT;

         END IF;


         --CHECK if we are good
         --duppsql saved above, just look at the ones in play as dups
         duppsql := duppsql || ' AND a.' || p_pkccol || ' IN (SELECT * FROM TABLE(:p3)) ';


         EXECUTE IMMEDIATE duppsql BULK COLLECT INTO post_dups USING p_tolerance,
                                                                     '13356%',
                                                                     GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(dup_ids);

         --post_dups are unfixable as far as we are concerned
         --Update them to 1
         --0s are now either fixed, or they are about to get updated to 2+ with subsequent checks

         FORALL ii IN 1..post_dups.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_table_name || ' a '
                           || 'SET a.' || p_flag_col || ' = :p1 '
                           || 'WHERE a.' || p_pkccol || ' = :p2 '
            USING 1,
                  post_dups(ii);

         COMMIT;


      END IF; --we have some dups

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FIX_INVALID_GEOMETRIES: Self Intersects');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --now find the ones that are self intersecting
      --We cant get the area of polys that are self-intersecting, results are wack


      intpsql := 'SELECT a.' || p_pkccol ||  ' '
              || 'FROM ' || p_table_name || ' a '
              || 'WHERE SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.' || p_column_name || ',:p1) LIKE :p2 ';

      EXECUTE IMMEDIATE intpsql BULK COLLECT INTO int_ids USING p_tolerance,
                                                                '13349%';

      IF int_ids.COUNT > 0
      THEN

         --Second choice, self union
         --This can produce 2002s for slivers
         FORALL ii IN 1 .. int_ids.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_table_name || ' a '
                           || 'SET a.' || p_column_name || ' = '
                           || '(SELECT SDO_GEOM.SDO_UNION(a.sdogeometry, b.sdogeometry, :p1) '
                           || 'FROM '
                           || p_table_name || ' a, '
                           || p_table_name || ' b '
                           || 'WHERE '
                           || 'a.face_id = :p2 AND '
                           || 'b.face_id = :p3 '
                           || ') '
                           || 'WHERE a.face_id = :p4 '
            USING p_tolerance,
                  int_ids(ii),
                  int_ids(ii),
                  int_ids(ii);

         COMMIT;

         --CHECK if we are good, at least in terms of validation
         --intpsql saved above
         intpsql := intpsql || ' AND a.' || p_pkccol || ' IN (SELECT * FROM TABLE(:p3)) ';

         EXECUTE IMMEDIATE intpsql BULK COLLECT INTO int_ids2 USING p_tolerance,
                                                                    '13349%',
                                                                    GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(int_ids);

      END IF;  --we have some intersecting

      IF int_ids2.COUNT > 0
      THEN

         --third choice, rectify

         FORALL ii IN 1 .. int_ids2.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_table_name || ' a '
                           || 'SET '
                           || 'a.' || p_column_name || ' = SDO_UTIL.RECTIFY_GEOMETRY(a.' || p_column_name || ',:p1) '
                           || 'WHERE a.' || p_pkccol || ' = :p2 '

            USING p_tolerance,
                  int_ids2(ii);

         COMMIT;

         --CHECK if we are good at least in terms of validation
         --intpsql saved above

         EXECUTE IMMEDIATE intpsql BULK COLLECT INTO int_ids3 USING p_tolerance,
                                                                    '13349%',
                                                                    GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(int_ids2);

      END IF; --we still have some intersecting


      IF int_ids3.COUNT > 0
      THEN

         --Update unfixed self intersects to code 7

         FORALL ii IN 1..int_ids3.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_table_name || ' a '
                           || 'SET a.' || p_flag_col || ' = :p1 '
                           || 'WHERE a.' || p_pkccol || ' = :p2 '
            USING 7,
                  int_ids3(ii);

         COMMIT;

         --Now re-update any to 6 if they touch universal face

         FORALL ii IN 1..int_ids3.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_table_name || ' a '
                           || 'SET a.' || p_flag_col || ' = :p1 '
                           || 'WHERE a.' || p_pkccol || ' = :p2 '
                           || 'AND  a.' || p_pkccol || ' IN ( '
                           || 'SELECT e.right_face_id FROM ' || p_topo || '_edge$ e '
                           || 'WHERE e.left_face_id = :p3 '
                           || 'UNION '
                           || 'SELECT ee.left_face_id FROM ' || p_topo || '_edge$ ee '
                           || 'WHERE ee.right_face_id = :p4 '
                           || ') '
            USING 6,
                  int_ids3(ii),
                  -1,
                  -1;

         COMMIT;


      END IF; --we still x2 have some intersecting

      --Now update fixed guys if requested. Smallies first

      IF int_ids.COUNT > 0
      THEN

         psql := 'SELECT * FROM TABLE(:p1) '
              || 'MINUS '
              || 'SELECT * FROM TABLE(:p2) ';

         EXECUTE IMMEDIATE psql bulk collect into int_fixed USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(int_ids),  --initial self intersects
                                                                  GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(int_ids3); --never fixed intersects

      END IF;

      IF p_intersect_size IS NOT NULL
      AND int_fixed.COUNT > 0
      THEN

         psql := 'SELECT a.' || p_pkccol || ' '
              || 'FROM ' || p_table_name || ' a '
              || 'WHERE a.' || p_pkccol || ' IN (SELECT * FROM TABLE(:p1)) '
              || 'AND SDO_GEOM.SDO_AREA(a.' || p_column_name || ', :p2) < :p3 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO small_fixed USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(int_fixed),
                                                                    p_tolerance,
                                                                    p_intersect_size;

         FORALL ii IN 1..small_fixed.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_table_name || ' a '
                           || 'SET a.' || p_flag_col || ' = :p1 '
                           || 'WHERE a.' || p_pkccol || ' = :p2 '
            USING 2,
                  small_fixed(ii);

         COMMIT;

      END IF;

      --Update or re-update fixed guys that border universal face and have perim to area issues

      IF p_intersect_ratio IS NOT NULL
      AND int_fixed.COUNT > 0
      THEN

         psql := 'SELECT a.' || p_pkccol || ' '
              || 'FROM ' ||  p_table_name || ' a '
              || 'WHERE a.' || p_pkccol || ' IN (SELECT * FROM TABLE(:p1)) '
              || 'AND a.' || p_pkccol || ' IN ('
              || 'SELECT e.right_face_id FROM ' || p_topo || '_edge$ e '
              || 'WHERE e.left_face_id = :p2 '
              || 'UNION '
              || 'SELECT ee.left_face_id FROM ' || p_topo || '_edge$ ee '
              || 'WHERE ee.right_face_id = :p3 '
              || ') AND '
              || 'SQRT(SDO_GEOM.SDO_AREA(a.' || p_column_name || ', :p4))/SDO_GEOM.SDO_LENGTH(a.' || p_column_name || ', :p5 ) < :p6 ';


         EXECUTE IMMEDIATE psql BULK COLLECT INTO ratio_fixed USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(int_fixed),
                                                                    -1,
                                                                    -1,
                                                                    p_tolerance,
                                                                    p_tolerance,
                                                                    p_intersect_ratio;
         FORALL ii IN 1..ratio_fixed.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_table_name || ' a '
                           || 'SET a.' || p_flag_col || ' = :p1 '
                           || 'WHERE a.' || p_pkccol || ' = :p2 '
            USING 4,
                  ratio_fixed(ii);

         COMMIT;


      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FIX_INVALID_GEOMETRIES: Gtypes');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_gtype = 2003
      THEN


         psql := 'SELECT a.' || p_pkccol || ' '
              || 'FROM ' || p_table_name || ' a '
              || 'WHERE (a.sdogeometry.sdo_gtype != :p1 AND '
              || 'a.sdogeometry.sdo_gtype != :p2) ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO bad_gtypes USING 2003,
                                                                   2007;

         --Frack any other QC values, if you are the wrong GTYPE thats a deal breaker
         FORALL ii IN 1..bad_gtypes.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_table_name || ' a '
                           || 'SET a.' || p_flag_col || ' = :p1 '
                           || 'WHERE a.' || p_pkccol || ' = :p2 '
            USING 8,
                  bad_gtypes(ii);

         COMMIT;

      ELSIF p_gtype = 2002
      THEN

         psql := 'SELECT a.' || p_pkccol || ' '
              || 'FROM ' || p_table_name || ' a '
              || 'WHERE a.sdogeometry.sdo_gtype != :p1 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO bad_gtypes USING 2002;

         FORALL ii IN 1..bad_gtypes.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_table_name || ' a '
                           || 'SET a.' || p_flag_col || ' = :p1 '
                           || 'WHERE a.' || p_pkccol || ' = :p2 '
            USING 8,
                  bad_gtypes(ii);

         COMMIT;

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FIX_INVALID_GEOMETRIES: Others');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --lastly tag any other validation errors
      psql := 'SELECT aa.myid FROM ( '
           || 'SELECT a.' || p_pkccol || ' myid, SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.' || p_column_name || ', :p1) val '
           || 'FROM ' || p_table_name || ' a '
           || 'WHERE SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.' || p_column_name || ', :p2) != :p3 '
           || ') aa '
           || 'WHERE (aa.val NOT LIKE :p4 AND aa.val NOT LIKE :p5) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO other_bads USING p_tolerance,
                                                                p_tolerance,
                                                                'TRUE',
                                                                '13356%',
                                                                '13349%';

      FORALL ii IN 1..other_bads.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_table_name || ' a '
                           || 'SET a.' || p_flag_col || ' = :p1 '
                           || 'WHERE a.' || p_pkccol || ' = :p2 '
            USING 9,
                  other_bads(ii);

      COMMIT;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FIX_INVALID_GEOMETRIES: Peace Out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------



       RETURN 'See the QC field in ' || p_table_name || '. We attempted to fix '
            || 'some invalid geoms ';



   END FIX_INVALID_GEOMETRIES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION ORDINATE_ROUNDER (
      p_geometry               IN SDO_GEOMETRY,
      p_places                 IN PLS_INTEGER DEFAULT 6
   ) RETURN SDO_GEOMETRY DETERMINISTIC

   AS

    --Matt! 6/19/10
    --8/5/11 Moved from clip to utilities to share with myself


     geom          SDO_GEOMETRY;
     ordinates     SDO_ORDINATE_ARRAY;


   BEGIN

      IF p_geometry.sdo_gtype = 2001
      THEN

         geom := p_geometry;
         geom.sdo_point.X := ROUND(p_geometry.sdo_point.X,p_places);
         geom.sdo_point.Y := ROUND(p_geometry.sdo_point.Y,p_places);

      ELSIF (p_geometry.sdo_gtype = 2002)
      OR (p_geometry.sdo_gtype = 2003)
      OR (p_geometry.sdo_gtype = 2007)
      THEN

         ordinates := p_geometry.SDO_ORDINATES;

         FOR i in 1 .. ordinates.COUNT
         LOOP

            ordinates(i) := ROUND(ordinates(i),p_places);

         END LOOP;

         geom := p_geometry;
         geom.sdo_ordinates := ordinates;

      ELSE

          RAISE_APPLICATION_ERROR(-20001,'Dude, I have no idea what to do with a ' ||p_geometry.sdo_gtype );

      END IF;

      RETURN geom;

   END ORDINATE_ROUNDER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GZ_FIND_MEASURE (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY
   ) RETURN NUMBER DETERMINISTIC
   AS

      --Matt! 8/23/10
      --copied from gz_clip 11/29/11 for sharing with topofix

      p_line               SDO_GEOMETRY;
      output               NUMBER;

   BEGIN

      --convert to LRS, 0 to 100
      p_line := SDO_LRS.CONVERT_TO_LRS_GEOM(p_edge, 0, 1000);


      output := SDO_LRS.FIND_MEASURE(p_line,p_point);

      RETURN output;

   END GZ_FIND_MEASURE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GZ_LOCATE_PT (
      p_edge           IN SDO_GEOMETRY,
      p_measure        IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 8/28/10
      --copied from gz_clip 11/29/11 for sharing with topofix

      output         SDO_GEOMETRY;
      line           SDO_GEOMETRY;

   BEGIN

      line := SDO_LRS.CONVERT_TO_LRS_GEOM(p_edge, 0, 1000);

      output := SDO_LRS.CONVERT_TO_STD_GEOM(SDO_LRS.LOCATE_PT(line, p_measure));

      RETURN output;


   END GZ_LOCATE_PT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GZ_PROJECT_PT (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY,
      p_tolerance      IN NUMBER DEFAULT 0.00000001
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 8/23/10
      --Copied from gz_clip 11/30/11 for sharing with topofix

      p_line               SDO_GEOMETRY;
      output               SDO_GEOMETRY;

   BEGIN

      --convert to LRS, 0 to 100
      p_line := SDO_LRS.CONVERT_TO_LRS_GEOM(p_edge, 0, 1000);

      output := SDO_LRS.CONVERT_TO_STD_GEOM(SDO_LRS.PROJECT_PT(p_line,
                                                               p_point,
                                                               p_tolerance)
                                                               );

      RETURN output;

   END GZ_PROJECT_PT;

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION REMOVE_DUPE_VERTEX (
      XYs                     IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY
   ) RETURN PLS_INTEGER
   AS

      --Sidey!  (+Matt) 1/19/12

      where_is  PLS_INTEGER:= 0;

   BEGIN

      For ii in 1..TRUNC(Xys.count/2)-1 Loop
         if Xys(ii*2-1) = Xys(ii*2+1) and Xys(ii*2) = Xys(ii*2+2) then
            where_is := ii;
            exit;
         end if;
       End Loop;

       If where_is <> 0 then
         where_is := where_is*2+3;
         For ii in where_is..Xys.count loop
           Xys(ii-2) := Xys(ii);
         End Loop;
         Xys.trim(2);
       End if;

       Return where_is;

   END REMOVE_DUPE_VERTEX;


------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

     FUNCTION VALIDATE_LINES_WITH_CONTEXT (
      p_line1                  IN SDO_GEOMETRY,
      p_line2                  IN SDO_GEOMETRY,
      p_tolerance             IN NUMBER DEFAULT .05
   ) RETURN VARCHAR2
   AS

      --Sidey 07/11/2012

      --sample usage
      --select e.edge_id, gz_utilities.validate_lines_with_context(e.geometry,b.geometry .05)
      --from z955ls_edge$ e, some_other_table b
      --where gz_utilities.validate_lines_with_context(e.geometry,b.geometry .05) != 'TRUE'

      geom_both            MDSYS.SDO_GEOMETRY;
      result              VARCHAR2(4000);



   BEGIN


   Geom_both := Concatenate_Lines_Sans(p_line1,p_line2,p_tolerance);


   result := validate_lines_with_context(geom_both, p_tolerance);

   RETURN result;

  END VALIDATE_LINES_WITH_CONTEXT;
  
   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION VALIDATE_LINES_WITH_CONTEXT (
      p_line                  IN SDO_GEOMETRY,
      p_tolerance             IN NUMBER DEFAULT .05,
      p_which_check           IN VARCHAR2 DEFAULT 'BOTH'
   ) RETURN VARCHAR2
   AS

      --Matt! 1/26/11

      --sample usage
      --select e.edge_id, gz_utilities.validate_lines_with_context(e.geometry, .05)
      --from z955ls_edge$ e
      --where gz_utilities.validate_lines_with_context(e.geometry, .05) != 'TRUE'
      
      --For intersections see GZ_INTERACTIVE_UTILS.SHOW_ME_EDGE_INTERSECTION fpr a viewer

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;


   BEGIN

      IF p_line IS NULL
      THEN

         RETURN 'SDOGEOMETRY is NULL';

      END IF;

      IF p_line.sdo_gtype = 2002 or p_line.sdo_gtype = 2006
      THEN
      
         NULL;
         
      ELSE
      
         RETURN 'Gtype is ' || p_line.sdo_gtype;

      END IF;
      
      IF p_which_check = 'BOTH'
      OR p_which_check = '13356'
      THEN

         IF sdo_geom.validate_geometry_with_context(p_line, p_tolerance) != 'TRUE'
         THEN

            RETURN sdo_geom.validate_geometry_with_context(p_line, p_tolerance);

         END IF;
         
      END IF;
      
      IF p_which_check = 'BOTH'
      OR p_which_check = '13349'
      THEN

         psql := 'SELECT COUNT(*) FROM ( '
              || 'SELECT t.x, t.y, COUNT(*) kount '
              || 'FROM '
              || 'TABLE(SDO_UTIL.GETVERTICES(:p1)) t '
              || 'GROUP BY t.x, t.y '
              || ') WHERE kount > 1';

         EXECUTE IMMEDIATE psql INTO kount USING sdo_geom.sdo_intersection(p_line, p_line, p_tolerance);

         IF kount > 1
         THEN

            RETURN kount || ' self intersections ';

         ELSIF kount = 1
         THEN

            --This could be a ring!

            IF  p_line.sdo_ordinates(1) = p_line.sdo_ordinates(p_line.sdo_ordinates.COUNT - 1)
            AND p_line.sdo_ordinates(2) = p_line.sdo_ordinates(p_line.sdo_ordinates.COUNT)
            THEN

               RETURN 'TRUE';

            ELSE

               RETURN kount || ' self intersections ';

            END IF;

         END IF;
         
      END IF;

      RETURN 'TRUE';

   END VALIDATE_LINES_WITH_CONTEXT;
   
   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

FUNCTION Concatenate_lines_Sans(pline1 IN MDSYS.SDO_GEOMETRY,pline2 IN MDSYS.SDO_GEOMETRY,p_tolerance NUMBER DEFAULT 0.05) RETURN MDSYS.SDO_GEOMETRY AS

-- Concatenate 2 nearby geometries into a single geometry -
-- WITHOUT the touching vertex (if any).

  Geometry_sans_touch  MDSYS.SDO_GEOMETRY;
  Geom2                MDSYS.SDO_GEOMETRY := pline2;

  Xys1  MDSYS.SDO_ORDINATE_ARRAY := pline1.sdo_ordinates;
  Xys2  MDSYS.SDO_ORDINATE_ARRAY := pline2.sdo_ordinates;

  x1   NUMBER;
  y1   NUMBER;

  x3   NUMBER;  --we will modify the 2nd geometry using these coordinates
  y3   NUMBER;
  x4   NUMBER;
  y4   NUMBER;

  x    NUMBER;
  y    NUMBER;

  t    NUMBER := 0.5; -- take 80% of 1st or last vertex
  n    PLS_INTEGER;
  m    PLS_INTEGER;
BEGIN


-- Check to see if there are any intersections at the ends

  n  := Xys1.count;
  x1 := Xys1(1);
  y1 := Xys1(2);

   m :=1;
  x4 := Xys2(1);
  y4 := Xys2(2);
  x3 := Xys2(3);
  y3 := Xys2(4);

  for ii in 1..4 loop

--  1+-------------+2
--  3+-------------+4

-- Initial test is 1 with start of 2nd line


-- try 1 with 4 (end of 2nd line)
      if ii = 3 then
         m := Xys2.count-1;
         x3 := Xys2(m-2);
         x3 := Xys2(m-1);
         x4 := Xys2(m);
         y4 := Xys2(m+1);

         x1 := Xys1(1);
         y1 := Xys1(2);
-- try end of 1st line with both ends of 2nd line
       elsif ii =2 or ii=4 then
         x1 := Xys1(n-1);
         y1 := Xys1(n);
      end if;

      if x1 = x4 and y1 = y4 then
         x := x3*t + (1.-t)*x4;
         y := y3*t + (1.-t)*y4;
         Xys2(m) := x;
         Xys2(m+1) := y;

-- we don't exit because there may be 2 problems !!
      end if;

  end loop;


  geom2.sdo_ordinates :=Xys2;

  Geometry_sans_touch := sdo_util.append(pline1,geom2);

  RETURN Geometry_sans_touch;

END;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION VALIDATE_TOUCHING_LINES (
      p_line1                    IN SDO_GEOMETRY,
      p_line2                    IN SDO_GEOMETRY,
      p_tolerance                IN NUMBER DEFAULT .05,
      p_round_digits             IN NUMBER DEFAULT 6
   ) RETURN VARCHAR2
   AS

      --Matt! 10/31/13 Original version attempted to use gz_pipes.get_widths 
      --! 12/26/13 Rewrote to avoid gz_pipes

      --returns 'TRUE' or 'FALSE'
      --TRUE if the two lines do not come within p_tolerance of each other
      --FALSE if the two lines do come almost into contact with each other (other than shared point)
      --   Goal is to ID stuff like
      --
      --  x-----1------------
      --   \      /\
      --    \__2_/  \________

      --Lines must touch at one or 2 vertices, assumption is that these are next/previous edge$ edges


      --Specimen:
      --select gz_geom_utils.VALIDATE_TOUCHING_LINES(e.geometry, ee.geometry, .05)
      --from z699tm_edge$ e,
      --z699tm_edge$ ee
      --where e.edge_id = 46935
      --and ee.edge_id = 46937
      
      --side note on why rounding is necessary
      --heres an example of a loop x position for 2 edges that share a node
      -- -80.43940281608109899025293998420238494873
      -- -80.43940299999999865576683077961206436157
      --might be better to put the points in a sdo_geometry and use sdo_relate. Have done this elsewhere
      --adds overhead however

      val_result        VARCHAR2(4000);
      x1head            NUMBER := ROUND(p_line1.sdo_ordinates(1), p_round_digits);
      y1head            NUMBER := ROUND(p_line1.sdo_ordinates(2), 6);
      x1tail            NUMBER := ROUND(p_line1.sdo_ordinates(p_line1.sdo_ordinates.COUNT - 1), p_round_digits);
      y1tail            NUMBER := ROUND(p_line1.sdo_ordinates(p_line1.sdo_ordinates.COUNT), p_round_digits);
      x2head            NUMBER := ROUND(p_line2.sdo_ordinates(1), p_round_digits);
      y2head            NUMBER := ROUND(p_line2.sdo_ordinates(2), p_round_digits);
      x2tail            NUMBER := ROUND(p_line2.sdo_ordinates(p_line2.sdo_ordinates.COUNT - 1), p_round_digits);
      y2tail            NUMBER := ROUND(p_line2.sdo_ordinates(p_line2.sdo_ordinates.COUNT), p_round_digits);
      loop_allow_kount  PLS_INTEGER;
      ix_kount          PLS_INTEGER;

   BEGIN

      --Check for head-head, head-tail, tail-head, or tail-tail

      IF (x1head = x2head AND y1head = y2head)
      OR (x1head = x2tail AND y1head = y2tail)
      OR (x1tail = x2head AND y1tail = y2head)
      OR (x1tail = x2tail AND y1tail = y2tail)
      THEN

         --dont even make a copy of the concat geom in a local var
         val_result := GZ_GEOM_UTILS.VALIDATE_LINES_WITH_CONTEXT(SDO_UTIL.CONCAT_LINES(p_line1,
                                                                                       p_line2),
                                                                 p_tolerance,
                                                                 '13349'); --only check for self-intersection
            
         --theres potentially more info here if we want it
         --dbms_output.put_line(val_result);
                                                                    
         IF val_result = 'TRUE'
         THEN
            
            RETURN 'TRUE';
               
         ELSIF val_result LIKE '1 self intersections%'
         OR    val_result LIKE '2 self intersections%'
         OR    val_result LIKE '3 self intersections%'
         THEN
            
            --check that one of the edges isn't a ring attached to the other edge
            --that spot would appear as a self-intersection in the concatenated geom
            
            --   ---1--    x is a node in the original topo
            --   |    |      and also the reported intersection point in the concatenated validation
            --   |    |
            --   -----x----2-----
            
            --and also there's an unfortunate wrinkle regarding the *2* and *3* self intersections
            --in cases like the one diagrammed above Oracle appears to always output (in the validate subroutine sdo_intersection)
            --a new start point for the looping edge somewhere on the loop.  
            --I guess this makes it a little more valid or something
            
            --   x--1--        <---Bonus intersection point, start-end loop of the concatenated intersection
            --   |    |      
            --   |    |
            --   -----x----2-----   <--True sensible intersection of the input geom
               
            IF  (x1head <> x1tail AND y1head <> y1tail)  
            AND (x2head <> x2tail AND y2head <> y2tail)
            THEN               
               
               IF (x1head = x2head AND y1head = y2head AND x1tail = x2tail AND y1tail = y2tail) --1head/2head + 1tail/2tail
               OR (x1head = x2tail AND y1head = y2tail AND x1tail = x2head AND y1tail = y2head) --1head/2tail + 1tail/2head
               THEN
               
                  --However check that the two edges dont join at a node on both ends
                  --this results in 1 false positive intersection 
                  --    __1__
                  --   /     \
                  --  x       x
                  --   \__2__/
               
                  ix_kount := TO_NUMBER(SUBSTR(val_result,1,1));
                  
                  IF ix_kount = 1
                  THEN
                  
                     RETURN 'TRUE';
                  
                  ELSE
                  
                     RETURN 'FALSE';
                  
                  END IF;
                  

               ELSE
               
                  --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
                  --No looping involved. Got a real intersection at tolerance. 
                  --legit FALSE return values 99.99% go here, everything else is
                  --false positive loops burrowing toward a TRUE
                  RETURN 'FALSE';
                  --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
                  
               END IF;
               
            ELSE
               
               --false alarm, most likely, got loops in the picture
               
               --1, 2, or 3
               ix_kount := TO_NUMBER(SUBSTR(val_result,1,1));
               
               --start with one point for there being a loop, 
               --always an intersection at the junction 
               loop_allow_kount := 1;
               
               --next allow 1 or 2 more depending on how many self loops we have
               
               IF  x1head = x1tail 
               AND y1head = y1tail
               THEN
               
                  loop_allow_kount := loop_allow_kount + 1;
                  
               END IF;
               
               IF  x2head = x2tail 
               AND y2head = y2tail
               THEN
               
                  loop_allow_kount := loop_allow_kount + 1;
                  
               END IF;  

               
               IF ix_kount <= loop_allow_kount
               THEN
               
                  RETURN 'TRUE';
               
               ELSE
               
                  RETURN 'FALSE';
               
               END IF;
               
            END IF;
               
         ELSE
            
            --4 intersections?  Hopefully never
            RETURN 'FALSE';
            
         END IF;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Input lines dont touch buster');

      END IF;

   END VALIDATE_TOUCHING_LINES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION SUBDIVIDE_TILE (
      p_geom               IN SDO_GEOMETRY,
      p_subdivisions       IN NUMBER
   ) RETURN GZ_TYPES.geomarray
   AS

      --Matt! 2/10/12
      --Moved from gz_build_source to gz_utilities 9/28/12

      output            GZ_TYPES.geomarray;
      tile_size         NUMBER;
      llx               NUMBER;
      lly               NUMBER;
      urx               NUMBER;
      ury               NUMBER;
      current_window    SDO_GEOMETRY;
      deadman           PLS_INTEGER := 0;

   BEGIN

      IF p_geom.SDO_ORDINATES.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, gots ta be an optimized rectangle');

      END IF;

      IF  ROUND(ABS(p_geom.sdo_ordinates(1) - p_geom.sdo_ordinates(3)),6)
       != ROUND(ABS(p_geom.sdo_ordinates(2) - p_geom.sdo_ordinates(4)),6)
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Danger danger, I dont think your tile is squarish');

      END IF;

      tile_size := ABS(p_geom.sdo_ordinates(1) - p_geom.sdo_ordinates(3))/p_subdivisions;

      current_window := p_geom;

      --start at lower left
      llx := p_geom.sdo_ordinates(1);
      lly := p_geom.sdo_ordinates(2);
      urx := p_geom.sdo_ordinates(1);
      ury := p_geom.sdo_ordinates(2);

      LOOP

         --go up one grid in y dimension
         lly := ury;
         ury := ury + tile_size;

         LOOP

            --travel along the X dimension ---->
            llx := urx;
            urx := urx + tile_size;

            --we now have a tile position
            current_window.sdo_ordinates(1) := llx;
            current_window.sdo_ordinates(2) := lly;
            current_window.sdo_ordinates(3) := urx;
            current_window.sdo_ordinates(4) := ury;

            output(output.COUNT + 1) := current_window;


            --always go past the X end before exiting
            IF urx >= p_geom.sdo_ordinates(3)
            THEN

              --exit inner X loop
              EXIT;

            END IF;

            --check for deadman here, inner loop

            IF deadman < 10000
            THEN

               deadman := deadman + 1;

            ELSE

               RAISE_APPLICATION_ERROR(-20001,'Deadman switch');

            END IF;

         END LOOP;

         --carriage return the X
         llx := p_geom.sdo_ordinates(1);
         urx := p_geom.sdo_ordinates(1);


         IF ury >= p_geom.sdo_ordinates(4)
         THEN

            --exit the top right hopefully
            EXIT;

         END IF;

      END LOOP;

      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Oopsie');

      END IF;

      RETURN output;

   END SUBDIVIDE_TILE;

   --------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-
   --Private------------------------------------------------------------------------

   FUNCTION GEODETIC_MBR_CONSIDERATIONS (
      p_tile                IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 5/01/12
      --Moved from gz_build_source to gz_utilities 9/28/12

      --MBR of US data tends to go from -179 to 179 (or worse) if we have just one tile
      --I'm just gonna hard code the longitude to 140 -60, Guam to US Virgin Islands to make closer
      --Shift_at_dateline will make it all negative, if necessary
      --GZ_TILE_TABLE will split into multiple tiles if necessary

      --1. When an optimized rectangle spans more than 119 degrees in longitude, it is
      --internally divided into three rectangles; and as a result, these three rectangles
      --share an edge that is the common boundary between them. If you validate the geometry
      --of such an optimized rectangle, error code 13351 is returned because the internal
      --rectangles have a shared edge. You can use such an optimized rectangle for queries
      --with only the following: SDO_ANYINTERACT operator, SDO_RELATE operator with the
      --ANYINTERACT mask, or SDO_GEOM.RELATE function with the ANYINTERACT mask.
      --(Any other queries on such an optimized rectangle may return incorrect results.)
      --2. No polygon element can have an area larger than or equal to one-half the surface of the Earth.


      output         SDO_GEOMETRY;

   BEGIN

      IF p_tile.sdo_srid != 8265
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, srid is ' || p_tile.sdo_srid);

      END IF;

      IF p_tile.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Well isnt this embarassing?');

      END IF;

      --typically we get
      -- -179.281086,
      -- -14.651813,
      -- 179.909681,
      -- 71.491059

      output := p_tile;

      IF p_tile.sdo_ordinates(1) < -175
      AND p_tile.sdo_ordinates(3) > 0
      THEN

         output.sdo_ordinates(1) := 140;  --144 Guam
         output.sdo_ordinates(3) := -60;  --64 US Virgin

      END IF;


      RETURN output;

   END GEODETIC_MBR_CONSIDERATIONS;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private-------------------------------------------------------------------------

   FUNCTION SHIFT_AT_DATELINE (
      p_tile                IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 3/5/12 Shift any tiles at the dateline
      --Moved from gz_build_source to gz_utilities 9/28/12

      --to whatever magic style works best
      --This is a mess

      output         SDO_GEOMETRY;

   BEGIN

      IF p_tile.sdo_srid != 8265
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, srid is ' || p_tile.sdo_srid);

      END IF;

      IF p_tile.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Well this is embarassing.');

      END IF;

      output := p_tile;

      IF (p_tile.sdo_ordinates(1) < 0 AND
          p_tile.sdo_ordinates(3) < 0 AND
          p_tile.sdo_ordinates(1) <= -180 AND
          p_tile.sdo_ordinates(3) >= -180)
      THEN

         -- -182  -178   <---Earth---
         --anything?
         RETURN output;

      ELSIF (p_tile.sdo_ordinates(1) > 0 AND
             p_tile.sdo_ordinates(3) > 0 AND
             p_tile.sdo_ordinates(1) <= 180 AND
             p_tile.sdo_ordinates(3) >= 180)
      THEN

         -- 178   182   ---Earth-->
         --anything?
         RETURN output;

      ELSIF (p_tile.sdo_ordinates(1) > 0 AND
             p_tile.sdo_ordinates(3) < 0 AND
             p_tile.sdo_ordinates(1) <= 180 AND
             p_tile.sdo_ordinates(3) >= -180)
      THEN

         -- 177   -178    --->180<---
         --make it entirely negative
         output.sdo_ordinates(1) := -180 - (180 - p_tile.sdo_ordinates(1));

         RETURN output;

      ELSE

         --doesnt cross or concern us

         RETURN output;

      END IF;


   END SHIFT_AT_DATELINE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private---------------------------------------------------------------------------

   FUNCTION TIDY_TILES (
      p_tiles                IN GZ_TYPES.geomarray
   ) RETURN GZ_TYPES.geomarray
   AS

      output            GZ_TYPES.geomarray;

      --Moved! from gz_build_source to gz_utilities 9/28/12

   BEGIN


      FOR i IN 1 .. p_tiles.COUNT
      LOOP

         --Cant cross more than 119 degrees longitude
         --or have an area larger than or equal to one-half the surface of the Earth

         output(i) := GZ_GEOM_UTILS.GEODETIC_MBR_CONSIDERATIONS(p_tiles(i));

      END LOOP;

      FOR i IN 1 .. output.COUNT
      LOOP

         output(i) := GZ_GEOM_UTILS.SHIFT_AT_DATELINE(output(i));

      END LOOP;

      --anything else?


      RETURN output;

   END TIDY_TILES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private-------------------------------------------------------------------------

   FUNCTION GET_GEODETIC_MBR_WIDTH (
     p_mbr              IN SDO_GEOMETRY,
     p_tolerance        IN NUMBER DEFAULT .05
   ) RETURN NUMBER DETERMINISTIC
   AS

      --Matt! 5/2/12
      --Southern side this is just a rough estimate


      output            NUMBER;
      pt1               SDO_GEOMETRY;
      pt2               SDO_GEOMETRY;

   BEGIN

      IF p_mbr.sdo_ordinates.COUNT <> 4
      OR p_mbr.sdo_srid <> 8265
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Well this is embarassing');

      END IF;

      pt1 := SDO_GEOMETRY(2001,
                          p_mbr.sdo_srid,
                          SDO_POINT_TYPE(p_mbr.sdo_ordinates(1), p_mbr.sdo_ordinates(2), NULL),
                          NULL,
                          NULL);

      pt2 := SDO_GEOMETRY(2001,
                          p_mbr.sdo_srid,
                          SDO_POINT_TYPE(p_mbr.sdo_ordinates(3), p_mbr.sdo_ordinates(2), NULL),
                          NULL,
                          NULL);

      output := SDO_GEOM.SDO_DISTANCE(pt1,pt2,p_tolerance);

      IF output < 0
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Cheezburgers, width is ' || output);
      END IF;

      output := output/111000; --very rough, at equator

      --return in degrees longitude at the equator
      RETURN output;


   END GET_GEODETIC_MBR_WIDTH;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------


   FUNCTION GZ_TILE_TABLE (
      p_table           IN VARCHAR2,                     --can be schema.table
      p_tile_target     IN NUMBER,
      p_geom_col        IN VARCHAR2 DEFAULT 'GEOMETRY',
      p_whereclause     IN VARCHAR2 DEFAULT NULL,        --use a.xx syntax
      p_sdo_filter      IN SDO_GEOMETRY DEFAULT NULL,
      p_log_type        IN VARCHAR2 DEFAULT NULL,        --BUILD for me here
      p_log_tag         IN VARCHAR2 DEFAULT NULL,        --ex topology
      p_debug           IN NUMBER DEFAULT NULL
   ) RETURN GZ_TYPES.geomarray
   AS

      --Matt! 2/10/12
      --Moved from gz_build_source to gz_utilities 9/28/12

      --Attempt at an all-purpose tiler. Returns optimized rectangles (actually squares)
      --For use in topo validation in pieces
      --Or creating topomaps to blanket a topology
      --Best to use a shortcut extent layer like state.  Edge$ is a bit of a snoozer for the MBR step

      --sample1: Give me 50 tiles covering the states
      --tiles := gz_utilities.gz_tile_table('acs11.state',50,'SDOGEOMETRY','a.vintage = ''90'' ',NULL,'BUILD','Z210IN');


      --tile target can be any number
      --Resulting tile count will be "fairly close"(TM) to the input following the childish steps below
      --Code will do an input battleship tiling based on 1/4 the length of the short side of the input universe MBR
      --It will then mark the tiles in play (TIP)
      --If the number of tiles is less than the target kount, it will determine a number to subdivide each
      --   TIP to get close to the target (x4, x9, x16, etc)
      --If the number of tiles is greater than the target kount it will
      --   Start over with battleship tiling based on 1/3, 1/2 the length of the short side of the input universe MBR
      --   If the number of new TIPs is less than the target kount it will subdivide (x4 x9 x16...) to get close
      --For the 56 US states (ie with island areas) the number of tiles returned will be way lower than the number
      --   requested.  Several of the TIPs over the isaland areas will return many fewer
      --   tiles than the algorithm (cough) predicts


      psql                 VARCHAR2(4000);
      x_sql                VARCHAR2(4000);
      loop_sql             VARCHAR2(4000);
      output               GZ_TYPES.geomarray;
      big_mbr              SDO_GEOMETRY;
      tile_size            NUMBER;
      test_tiles           GZ_TYPES.stringarray;
      deadman              PLS_INTEGER := 0;
      llx                  NUMBER;
      lly                  NUMBER;
      urx                  NUMBER;
      ury                  NUMBER;
      test_window          SDO_GEOMETRY;
      relatekount          PLS_INTEGER;
      trukount             PLS_INTEGER;
      tile_divider         PLS_INTEGER := 4;
      best_i               PLS_INTEGER;
      diff_i               PLS_INTEGER;
      best_diff            NUMBER;
      subdivided_tile      GZ_TYPES.geomarray;
      subdivided_output    GZ_TYPES.geomarray;
      tile_target          NUMBER := p_tile_target;

   BEGIN

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_TILE_TABLE: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      IF p_log_type IS NOT NULL
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                 'Starting tiling of ' || p_table );

      END IF;

      IF tile_target <= 0
      OR tile_target > 100000
      THEN

         --just a guess
         RAISE_APPLICATION_ERROR(-20001,'Sorry boss, I dont think a tile target of ' || tile_target || ' is gonna work ');

      END IF;

      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_table)
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Table ' || p_table || ' no exist');

      END IF;


      --first, set up our reusable psql starting at FROM

      psql := 'FROM ' || p_table || ' a ';

      IF p_whereclause IS NOT NULL
      THEN

         --where a.edge_id IN (select edge_id from myedgeids)
         psql := psql || 'WHERE (' || p_whereclause || ') '; --parens in case dumb Matt passes in 'X OR Y'

      END IF;

      IF p_sdo_filter IS NOT NULL
      THEN

         IF p_whereclause IS NOT NULL
         THEN

            psql := psql || 'AND ';

         ELSE

            psql := psql || 'WHERE ';

         END IF;

         --ick on the bind variables going forward
         psql := psql || 'SDO_ANYINTERACT(a.' || p_geom_col || ',:p1) = :p2 ';

      END IF;


      --get our big mbr

      x_sql := 'SELECT SDO_AGGR_MBR(a.' || p_geom_col || ') ' || psql;

      IF p_log_type IS NOT NULL
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                 'Getting MBR of ' || p_table,NULL,NULL,NULL,x_sql);

      END IF;

      IF p_sdo_filter IS NOT NULL
      THEN

         EXECUTE IMMEDIATE x_sql INTO big_mbr USING p_sdo_filter,
                                                   'TRUE';

      ELSE

         EXECUTE IMMEDIATE x_sql INTO big_mbr;

      END IF;

      --dont trust him, probably went all the way around the world
      --dont do this, around the world is best for the U.S. MBR. Or at least the best we are gonna get
      --big_mbr := GZ_BUILD_SOURCE.SHIFT_AT_DATELINE(big_mbr);

      IF big_mbr.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Doh, big mbr has ' || big_mbr.sdo_ordinates.COUNT || ' ordinates. WTF?');

      END IF;

      IF p_debug = 1
      THEN
         dbms_output.put_line('BIG mbr is ');
         dbms_output.put_line(gz_geom_utils.dump_sdo(big_mbr));
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                               'BIG MBR-> ',NULL,NULL,NULL,NULL,NULL,NULL,big_mbr);
      END IF;


      IF tile_target = 1
      THEN

         --If just one tile possibly get out before we do any damage
         --potentially no spatial tests here

         subdivided_output(1) := big_mbr;

         IF subdivided_output(1).sdo_srid = 8265  --ugh
         THEN

            --Shift any tiles that cross the dateline to be all negative
            --load_topo_map is a big baby and can't handle this
            --and constrain any tiles that are huge
            --sdo operators cant handle this

            subdivided_output := GZ_GEOM_UTILS.TIDY_TILES(subdivided_output);

            --all negative after tidy tiles
            IF GZ_GEOM_UTILS.GET_GEODETIC_MBR_WIDTH(subdivided_output(1)) < 119
            THEN

               RETURN subdivided_output;

            ELSE

               --This single tile is bigger than 119 degrees, the magic limit for geodetic mbrs
               --Will likely run out of memory with a tile this size too
               --User probably input default tile target of 1
               --Override to 4 to be safe
               tile_target := 4;

            END IF;

         ELSE

            RETURN subdivided_output;

         END IF;

      END IF;


      <<go_logo_turtle>> -- Grade = F

      trukount := 0;
      output.DELETE;

      --divide the short side by 4 (first time at least)

      IF ABS(big_mbr.sdo_ordinates(1) - big_mbr.sdo_ordinates(3)) <
         ABS(big_mbr.sdo_ordinates(2) - big_mbr.sdo_ordinates(4))
      THEN

         --x is shorty
         tile_size := ABS(big_mbr.sdo_ordinates(1) - big_mbr.sdo_ordinates(3))/tile_divider;

      ELSE

         --y is shorty
         tile_size := ABS(big_mbr.sdo_ordinates(2) - big_mbr.sdo_ordinates(4))/tile_divider;

      END IF;

      IF p_debug = 1
      THEN
         dbms_output.put_line('tile size is ' || tile_size);
      END IF;



     loop_sql := 'SELECT COUNT(1) ' || psql || ' ';

      IF p_whereclause IS NULL
      AND p_sdo_filter IS NULL
      THEN

          loop_sql := loop_sql || 'WHERE ';

      ELSE

         loop_sql := loop_sql || 'AND ';

      END IF;

      --switch to anyinteract here - otherwise we get empty tiles for reals
      loop_sql := loop_sql || 'SDO_ANYINTERACT(a.' || p_geom_col || ',:p9) = :p8 ';

      --set up test window
      test_window := big_mbr;

      --start at lower left. Suck at this
      llx := big_mbr.sdo_ordinates(1);
      lly := big_mbr.sdo_ordinates(2);
      urx := big_mbr.sdo_ordinates(1);
      ury := big_mbr.sdo_ordinates(2);

      IF p_log_type IS NOT NULL
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                 'Starting loop over battleship tiles ' || p_table,NULL,NULL,NULL,loop_sql);

      END IF;

      LOOP

         --go up one grid in y dimension
         lly := ury;
         ury := ury + tile_size;

         LOOP

            --travel along the X dimension ---->
            llx := urx;
            urx := urx + tile_size;

            --we now have a tile position
            test_window.sdo_ordinates(1) := llx;
            test_window.sdo_ordinates(2) := lly;
            test_window.sdo_ordinates(3) := urx;
            test_window.sdo_ordinates(4) := ury;

            --test it

            IF p_debug = 1
            THEN
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                      'Testing battleship->',NULL,NULL,NULL,NULL,NULL,NULL,test_window);
            END IF;


            IF p_sdo_filter IS NOT NULL
            THEN

               EXECUTE IMMEDIATE loop_sql INTO relatekount USING p_sdo_filter,
                                                                 'TRUE',
                                                                 test_window,
                                                                 'TRUE';

            ELSE

               EXECUTE IMMEDIATE loop_sql INTO relatekount USING test_window,
                                                                 'TRUE';

            END IF;

            IF relatekount = 0
            THEN

               NULL;
               --output(output.COUNT + 1) := NULL;
               --test_tiles(test_tiles.COUNT + 1) := 'FALSE';
               IF p_debug = 1
               THEN
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                      'Tossing battleship->',NULL,NULL,NULL,NULL,NULL,NULL,test_window);
               END IF;

            ELSE

               trukount := trukount + 1;
               output(output.COUNT + 1) := test_window;
               --test_tiles(test_tiles.COUNT + 1) := 'TRUE';

               IF p_debug = 1
               THEN
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                         'Keeping battleship->',NULL,NULL,NULL,NULL,NULL,NULL,test_window);
               END IF;

            END IF;


            --always go past the X end before exiting
            IF urx >= big_mbr.sdo_ordinates(3)
            THEN

              --exit inner X loop
              EXIT;

            END IF;

            --check for deadman here, inner loop

            IF deadman < 10000
            THEN

               deadman := deadman + 1;

            ELSE

               RAISE_APPLICATION_ERROR(-20001,'Deadman switch');

            END IF;

         END LOOP;

         --carriage return the X
         llx := big_mbr.sdo_ordinates(1);
         urx := big_mbr.sdo_ordinates(1);

         --always go past the Y end before exit
         --ensures final tile hangs off the universe

         IF ury >=  big_mbr.sdo_ordinates(4)
         THEN

            --exit the top right hopefully
            EXIT;

         END IF;

      END LOOP;


      IF trukount > (tile_target * 1.25) --12 on target 10 is cool with me
      AND tile_divider > 1
      THEN

         IF p_log_type IS NOT NULL
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                   'Too many tiles, going back: ' || p_table,NULL,NULL,NULL,NULL);

         END IF;

         --too many tiles  ..4..3..2..1
         tile_divider := tile_divider - 1;

         GOTO go_logo_turtle;

      ELSIF trukount <= (tile_target * 1.25)
      AND   trukount >= (tile_target * .75)
      THEN

         IF p_log_type IS NOT NULL
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                   'Good enough, going with : ' || trukount || 'tiles',NULL,NULL,NULL,NULL);

         END IF;

         --good enough for me
         RETURN output;

      ELSE

         --we need to make more tiles
         --get a target multiplier

         IF p_debug = 1
         THEN
            dbms_output.put_line('starting with ' || output.COUNT || ' tiles before multiplier');
         END IF;

         FOR i IN 1 .. 1000
         LOOP

            --can take our current tile set and subdivide if thats better for you
            --Any 1 tile becomes... 4,9,16,25,36,49,64,81,100,...

            diff_i := ABS(tile_target - (trukount * (i*i)));

            IF best_i IS NULL
            OR diff_i < best_diff
            THEN

               best_diff := diff_i;
               best_i := (i*i);

            END IF;

         END LOOP;

         IF p_debug = 1
         THEN
            dbms_output.put_line('best i is ' || best_i);
         END IF;

         IF p_log_type IS NOT NULL
         AND SQRT(best_i) != 1
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                   'Multiplying ' || trukount || ' tiles by ' || SQRT(best_i) ,NULL,NULL,NULL,NULL);

         END IF;


         FOR i IN 1 .. output.COUNT
         LOOP

            subdivided_tile := GZ_GEOM_UTILS.SUBDIVIDE_TILE(output(i),SQRT(best_i));

            IF p_debug = 1
            AND i = 1
            THEN
               dbms_output.put_line('Got ' || subdivided_tile.COUNT || ' tiles from subdivider');
            END IF;

            FOR j IN 1 .. subdivided_tile.COUNT
            LOOP

               subdivided_output(subdivided_output.COUNT + 1) := subdivided_tile(j);

            END LOOP;

         END LOOP;

         output.DELETE; --trash this guy
         subdivided_tile.DELETE;


         IF subdivided_output(1).sdo_srid = 8265  --ugh
         THEN

            --Shift any tiles that cross the dateline to be all negative
            --and constrain any tiles that are huge
            --load_topo_map is a big baby and can't handle this

            subdivided_output := GZ_GEOM_UTILS.TIDY_TILES(subdivided_output);

         END IF;

         --dont forget, some of these subdivided tiles may be over dead space too

         IF p_debug = 1
         THEN
            dbms_output.put_line('Sending ' || subdivided_output.COUNT || ' tiles to spatial tests');
         END IF;

         IF p_log_type IS NOT NULL
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                   'Sending ' || subdivided_output.COUNT || ' subdivided tiles to spatial checks ',NULL,NULL,NULL,NULL);

         END IF;

         FOR i IN 1 .. subdivided_output.COUNT
         LOOP


            IF p_sdo_filter IS NOT NULL
            THEN

               EXECUTE IMMEDIATE loop_sql INTO relatekount USING p_sdo_filter,
                                                                 'TRUE',
                                                                 subdivided_output(i),
                                                                 'TRUE';

            ELSE

               EXECUTE IMMEDIATE loop_sql INTO relatekount USING subdivided_output(i),
                                                                 'TRUE';

            END IF;

            IF relatekount = 0
            THEN

               NULL;

               IF p_debug = 1
               THEN
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                         'Tossing tile->',NULL,NULL,NULL,NULL,NULL,NULL,subdivided_output(i));
               END IF;

            ELSE

               output(output.COUNT + 1) := subdivided_output(i);

               IF p_debug = 1
               THEN
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                         'Keeping tile->',NULL,NULL,NULL,NULL,NULL,NULL,subdivided_output(i));
               END IF;

            END IF;


         END LOOP;

         IF p_log_type IS NOT NULL
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                   'Returning ' || output.COUNT || ' tiles to caller ',NULL,NULL,NULL,NULL);

         END IF;


         IF output.COUNT = 0
         THEN

            RAISE_APPLICATION_ERROR(-20001,'oops code 3121');

         ELSE

            RETURN output;

         END IF;


      END IF;

      --shouldnt get here
      RAISE_APPLICATION_ERROR(-20001,'oops');


   END GZ_TILE_TABLE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION EXPAND_MBR_PERCENT (
      p_mbr             IN SDO_GEOMETRY,
      p_percent         IN NUMBER DEFAULT 1 --can be negative
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 12/14/12

      --Useful in dealing with topomap issues like edge/face not found in cache
      --Where fudging the size of the topomap helps resolve bugs
      --Expands both dimensions separately by the indicated pct
      --should probably replace all those hard coded deltas in topomap work with this some day too

      --tester sample
      --select gz_utilities.expand_mbr_percent(sdo_geom.sdo_mbr(sdogeometry),25) from z699tm_merge_Face
      --where face_id = 1

      output               SDO_GEOMETRY;
      horizontal_stretch   NUMBER;
      vertical_stretch     NUMBER;

   BEGIN

      IF p_mbr.sdo_ordinates.COUNT <> 4
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry famalam, only works for optimized rectangles. Feel free to add new code');

      END IF;

      IF p_mbr.sdo_ordinates(3) < p_mbr.sdo_ordinates(1)
      OR p_mbr.sdo_ordinates(4) < p_mbr.sdo_ordinates(2)
      AND p_mbr.sdo_srid <> 8265  --allow dateline crossing
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry famalam, upper left coordinates should be greater than lower left');

      END IF;

      IF p_percent <= -100
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry famalam, cant shrink by more than 100 percent');

      END IF;

      IF p_mbr.sdo_srid = 8265
      THEN

         --cant hurt, wring it through. will just returned unchanged
         output := GZ_GEOM_UTILS.SHIFT_AT_DATELINE(p_mbr);

      ELSE

         output := p_mbr;

      END IF;

      horizontal_stretch := (ABS(output.sdo_ordinates(3) - output.sdo_ordinates(1)) * ABS(p_percent)/100)/2;
      vertical_stretch   := (ABS(output.sdo_ordinates(4) - output.sdo_ordinates(2)) * ABS(p_percent)/100)/2;

      output := p_mbr;

      IF p_percent > 0
      THEN

         output.sdo_ordinates(1) := output.sdo_ordinates(1) - horizontal_stretch;
         output.sdo_ordinates(2) := output.sdo_ordinates(2) - vertical_stretch;
         output.sdo_ordinates(3) := output.sdo_ordinates(3) + horizontal_stretch;
         output.sdo_ordinates(4) := output.sdo_ordinates(4) + vertical_stretch;

      ELSE

         output.sdo_ordinates(1) := output.sdo_ordinates(1) + horizontal_stretch;
         output.sdo_ordinates(2) := output.sdo_ordinates(2) + vertical_stretch;
         output.sdo_ordinates(3) := output.sdo_ordinates(3) - horizontal_stretch;
         output.sdo_ordinates(4) := output.sdo_ordinates(4) - vertical_stretch;

      END IF;


      RETURN output;

   END EXPAND_MBR_PERCENT;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION DO_ORDINATES_MATCH (
      p_sdogeometry1       IN SDO_GEOMETRY,
      p_sdogeometry2       IN SDO_GEOMETRY,
      p_round              IN NUMBER DEFAULT NULL
   ) RETURN PLS_INTEGER DETERMINISTIC
   AS
   
      --Matt! 2/7/14
      --Not just are the geometries equal within tolerance, 
      --but are the ordinates at each position the same
      --1 for match, 0 for mismatch
      
      --sample
      --select gz_geom_utils.do_ordinates_match(a.sdogeometry, b.sdogeometry)
      --from gzcpb1.z899in_face a,
      --schel010.z899in_face b
      --where a.face_id = 1017
      --and b.face_id = 1017
      
         psql              VARCHAR2(4000);
         kount             PLS_INTEGER;

   BEGIN

      IF p_sdogeometry1.sdo_gtype NOT IN (2002, 2003, 2007)
      OR p_sdogeometry2.sdo_gtype NOT IN (2002, 2003, 2007)
      THEN
         
         RAISE_APPLICATION_ERROR(-20001, 'Unexpected gtype ' || p_sdogeometry1.sdo_gtype || ' ' || p_sdogeometry2.sdo_gtype);
         
      END IF;
      
      psql := 'SELECT COUNT(*) FROM '
           || '(select a.column_value coordinate, rownum position '
           || 'FROM '
           || 'TABLE(:p1) a '
           || ') aa, '
           || '(SELECT b.column_value coordinate, rownum position '
           || 'FROM '
           || 'TABLE(:p2) b '
           || ') bb '
           || 'WHERE aa.position = bb.position AND ';
           
      IF p_round IS NULL
      THEN
      
         psql := psql
           || 'aa.coordinate <> bb.coordinate ';
           
         EXECUTE IMMEDIATE psql INTO kount USING p_sdogeometry1.sdo_ordinates,
                                                 p_sdogeometry2.sdo_ordinates;
           
      ELSE
      
         psql := psql
           || 'ROUND(aa.coordinate, :p3) <> ROUND(bb.coordinate, :p4) ';
           
         EXECUTE IMMEDIATE psql INTO kount USING p_sdogeometry1.sdo_ordinates,
                                                 p_sdogeometry2.sdo_ordinates,
                                                 p_round,
                                                 p_round;
      
      END IF;
             
                                  
      IF kount = 0
      THEN
         
         RETURN 1;
            
      ELSE
         
         RETURN 0;
         
      END IF;
         
      
   END DO_ORDINATES_MATCH;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------    
      
   FUNCTION SHOW_FACE_ORDINATE_DIFFERENCES (
      p_sdogeometry1       IN SDO_GEOMETRY,
      p_sdogeometry2       IN SDO_GEOMETRY,
      p_round              IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS
   
      --Matt! 2/7/14
      --Not just are the geometries equal within tolerance like sdo_equals, 
      --but are the ordinates at each position the same down to the last digit (maybe round param?)
      --Return a line geometry of the coordinate mismatch zone using the first geometries' coordinates
      --Lazy Executive Decision: Outer ring of the face only.  Any inner rings are the domain of those faces
      --Overall totally sloppy since any string of segments can go in and out of match and mismatch
      --   but here just calling the whole range a mismatch
      
      --Sample
      --select gz_geom_utils.show_ordinate_differences(a.sdogeometry, b.sdogeometry)
      --from z899in_face a,
      --radiusstudio_face b
      --where a.face_id = 768 and b.face_id = 768
      
      psql                 VARCHAR2(4000);
      min_position         NUMBER;
      max_position         NUMBER;
      output               SDO_GEOMETRY;  
      outer_ring           SDO_GEOMETRY; 
      kounter              PLS_INTEGER := 0;         
   
   BEGIN
   
      IF p_sdogeometry1.sdo_gtype <> 2003
      OR p_sdogeometry2.sdo_gtype <> 2003
      THEN
         
            RAISE_APPLICATION_ERROR(-20001, 'Unexpected face gtype ' || p_sdogeometry1.sdo_gtype || ' ' || p_sdogeometry2.sdo_gtype);
         
      END IF;
   
      IF GZ_GEOM_UTILS.DO_ORDINATES_MATCH(p_sdogeometry1,
                                          p_sdogeometry2,
                                          p_round) = 1
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Why you bugging me, ordinates match');
      
      END IF;
      
      psql := 'SELECT MIN(aa.position), MAX(aa.position) FROM '  
           || '(select a.column_value coordinate, rownum position '
           || 'FROM '
           || 'TABLE(:p1) a '
           || ') aa, '
           || '(SELECT b.column_value coordinate, rownum position '
           || 'FROM '
           || 'TABLE(:p2) b '
           || ') bb '
           || 'WHERE aa.position = bb.position AND ';
           
      IF p_round IS NULL
      THEN
      
         psql := psql
           || 'aa.coordinate <> bb.coordinate ';
           
         EXECUTE IMMEDIATE psql INTO min_position,
                                     max_position USING p_sdogeometry1.sdo_ordinates,
                                                        p_sdogeometry2.sdo_ordinates;
           
      ELSE
      
         psql := psql
           || 'ROUND(aa.coordinate, :p3) <> ROUND(bb.coordinate, :p4) ';
           
         EXECUTE IMMEDIATE psql INTO min_position,
                                     max_position USING p_sdogeometry1.sdo_ordinates,
                                                        p_sdogeometry2.sdo_ordinates,
                                                        p_round,
                                                        p_round;
      
      END IF;
                                         
      IF MOD(min_position, 2) = 0
      THEN
      
         --starts on a Y, back that junk up to get the full x,y in play
         min_position := min_position - 1;
      
      END IF;
      
      IF MOD(max_position, 2) = 1
      THEN
      
         --ends on an X, go 1 past to get our Y
         max_position := max_position + 1;
      
      END IF;
      
      IF (max_position - min_position) = 1
      THEN
      
         --just a point
         output := SDO_GEOMETRY(2001,
                                p_sdogeometry1.sdo_srid,
                                SDO_POINT_TYPE
                                (
                                    p_sdogeometry1.sdo_ordinates(min_position),
                                    p_sdogeometry1.sdo_ordinates(max_position),
                                    NULL
                                 ),
                                 NULL,
                                 NULL
                              );

         RETURN output;
      
      ELSIF p_sdogeometry1.sdo_elem_info.COUNT = 3
      OR max_position <= p_sdogeometry1.sdo_elem_info(4)
      THEN
      
         --single ring, simple line output
         --or bad spot is on initial outer ring and we can simply roll through
      
         output := SDO_GEOMETRY(2002, 
                                p_sdogeometry1.sdo_srid, 
                                NULL,
                                SDO_ELEM_INFO_ARRAY(1,2,1),
                                SDO_ORDINATE_ARRAY()
                                );
                                
         output.sdo_ordinates.EXTEND(max_position - min_position + 1);
         
         FOR i IN 1 .. (max_position - min_position + 1)
         LOOP
            
            output.sdo_ordinates(kounter + 1) := p_sdogeometry1.sdo_ordinates(min_position + kounter);
            
            kounter := kounter + 1;
         
         END LOOP;
      
         RETURN output;
         
      ELSE

         --this guy has at least 1 inner ring
         --and the bad spot is at least partially on an inner ring
         
         outer_ring := SDO_UTIL.EXTRACT(p_sdogeometry1, 1, 1);
         
         IF min_position > outer_ring.sdo_ordinates.COUNT
         THEN
         
            --meh I dunno, gonna use interactively for starts
            RAISE_APPLICATION_ERROR(-20001, 'Only difference is on inner rings');
            --RETURN NULL;
            
         ELSE
         
            output := SDO_GEOMETRY(2002, 
                                   p_sdogeometry1.sdo_srid, 
                                   NULL,
                                   SDO_ELEM_INFO_ARRAY(1,2,1),
                                   SDO_ORDINATE_ARRAY()
                                   );
                                
            output.sdo_ordinates.EXTEND(outer_ring.sdo_ordinates.COUNT - min_position + 1);
            
            FOR i IN 1 .. (outer_ring.sdo_ordinates.COUNT - min_position + 1)
            LOOP   
               
               output.sdo_ordinates(kounter + 1) := outer_ring.sdo_ordinates(min_position + kounter);
               
               kounter := kounter + 1;
            
            END LOOP;
      
            RETURN output;
         
         END IF;
      
      END IF;
   
   END SHOW_FACE_ORDINATE_DIFFERENCES;
      
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

END GZ_GEOM_UTILS;
/

CREATE OR REPLACE PACKAGE BODY GZ_UTILITIES
AS

PROCEDURE CREATE_PROJECTION_SEQ(seq_name VARCHAR2 default 'GZ_PROJECT_SEQ') AS

    sql_stmt    VARCHAR2(4000);

BEGIN
-- seq_name := Topology||seq_name ||'Seq4_2007';

   sql_stmt := 'CREATE SEQUENCE ' || seq_name;
   EXECUTE IMMEDIATE sql_stmt;
END CREATE_PROJECTION_SEQ;
--
PROCEDURE CREATE_GZ_PROJECTION_WRK2003(
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_PROJECT_WRK2003'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_PROJECTION_WRK2003: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_PROJECTION_WRK2003',
                               'Y');                     --always drop.  Why else are we here?

      --Add constraints

--      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
--              || 'ADD ('
--              || '   CONSTRAINT ' || p_table_name || 'PKC '
--              || '      PRIMARY KEY(RELEASE, LAYER) '
--              || ')';


      --Add triggers

      --some day would be nice to check that SOURCE layers are in gz_layers_in

      --Add indexes. None

      --Manage privvies
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_PROJECT_WRK2003: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_PROJECTION_WRK2003;
   --
   PROCEDURE CREATE_GZ_PROJECTION_WRKAREA(
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_PROJECT_WRKAREA'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_PROJECTION_WRKAREA: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_PROJECTION_WRKAREA',
                               'Y');                     --always drop.  Why else are we here?

      --Add constraints

--      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
--              || 'ADD ('
--              || '   CONSTRAINT ' || p_table_name || 'PKC '
--              || '      PRIMARY KEY(RELEASE, LAYER) '
--              || ')';


      --Add triggers

      --some day would be nice to check that SOURCE layers are in gz_layers_in

      --Add indexes. None

      --Manage privvies
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_PROJECTION_WRK2003: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_PROJECTION_WRKAREA;
   PROCEDURE CREATE_GZ_PROJECTION_WRKOUT(
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_PROJECT_WRKOUT'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_PROJECTION_WRKOUT: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_PROJECTION_WRKOUT',
                               'Y');                     --always drop.  Why else are we here?

      --Add constraints

--      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
--              || 'ADD ('
--              || '   CONSTRAINT ' || p_table_name || 'PKC '
--              || '      PRIMARY KEY(RELEASE, LAYER) '
--              || ')';


      --Add triggers

      --some day would be nice to check that SOURCE layers are in gz_layers_in

      --Add indexes. None

      --Manage privvies
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_PROJECTION_WRK2003: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_PROJECTION_WRKOUT;
    PROCEDURE CREATE_GZ_PROJECTION_OUTPUT(
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_PROJECT_OUTPUT'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_PROJECTION_OUTPUT: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_PROJECTION_OUTPUT',
                               'Y');                     --always drop.  Why else are we here?

      --Add constraints

--      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
--              || 'ADD ('
--              || '   CONSTRAINT ' || p_table_name || 'PKC '
--              || '      PRIMARY KEY(RELEASE, LAYER) '
--              || ')';


      --Add triggers

      --some day would be nice to check that SOURCE layers are in gz_layers_in

      --Add indexes. None

      --Manage privvies
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_PROJECT_WRK2003: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_PROJECTION_Output;
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
--
FUNCTION TABLE_EXISTS ( pInTable  IN VARCHAR2, pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN AS

   /**Taken from the cartodb
    Author: Nick Padfield
    Creation Date: 9/6/2006
    See GZ_UTILITIES.GZ_TABLE_EXISTS for an alternative implementation
   **/
   -- Updated to not place NVL in SQL statement
   -- Updated to strip off schema name if accidentally begins table name (06/25/2012)
   -- Updated to call Matt's GZ_Table_Exists.
   InTable       VARCHAR2(30)     := UPPER(pInTable);
   InSchema      VARCHAR2(30)     := UPPER(NVL(pInSchema,USER)); --UPPER(pInSchema);
--   RecordCount   NUMBER(22)       := 0.;
--   sql_stmt      VARCHAR2(4000);
--   p             PLS_INTEGER;
   BEGIN


-- Strip off schema if passed in table name (accidentally)??

   If INSTR(Intable,'.') = 0 then
       Intable := Inschema||'.'|| InTable;
   end if;
   RETURN GZ_Table_exists(Intable,TRUE);
/*
-- Strip off schema if passed in table name (accidentally)??

   If INSTR(Intable,'.') > 0 then
       p := INSTR(InTable,'.')+1;
       Intable := SUBSTR(Intable,p,Length(Intable)-p+1);
   end if;
      ------------------------------------------------------
      -- Check to see if the table exists!
      sql_stmt := 'SELECT COUNT(1) FROM all_objects WHERE object_type = ''TABLE'' AND owner = ''' || InSchema || ''' AND object_name = ''' || InTable || '''';
      EXECUTE IMMEDIATE sql_stmt INTO RecordCount;
      ------------------------------------------------------
      -- If table exists, return TRUE, if not, return FALSE.
      RETURN (RecordCount > 0.);
*/
   END TABLE_EXISTS;
--
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
--

/* Quarantined .....

FUNCTION get_avail_temptable_name(pInSchema VARCHAR2, pTableSuffix VARCHAR2 DEFAULT 'TEMP',pReturnSchemaPortion VARCHAR2 DEFAULT 'NO',pCallingProc VARCHAR2 DEFAULT 'UNKNOWN') RETURN VARCHAR2 AS

---------------------------------------------------------------------------------------------------------------------
--Program Name: get_avail_temptable_name
--- Author: Nick Padfield
--- Creation Date: 7/20/2007
---
--- Usage:
---   Call this program from inside another PL/SQL program.  This program
---   has 3 OPTIONAL parameters:
---     pInSchema            - The Schema that you want to check to see if a temp table already
---                            exists in.  It defaults to the current schema.
---     pTableSuffix         - A table suffix that you want appended to the end of the name.
---                            The default is 'TEMP'.
---     pReturnSchemaPortion - Indicates whether you want the returned value to be prefixed
---                            with the schema name.  Only two valid strings may be passed:
---                            'YES' or 'NO'.  Defaults to 'No'.
---     pCallingProc         - The name of the calling procedure to be inserted into the
---                            CDB_TMPTABLE_TRACKING table.  Defaults to 'Unknown'.
---
---
--- Purpose:
---   The purpose of this program is to return a unique valid name for a temporary table.
---   This function was created so that anyone can grab a temporary table name and GAURANTEE
---   that it does not previously exist.  The structure of the table name is as follows:
---   X<Sequence Number>_<TableSuffix>
---
---   Assume that temp table X27_TEMP exists.  When this function is called (with no parameters passed),
---   it gets the next value in the CDB_TMPTABLE_SEQ sequence.  Most likeley this sequence will return a number
---   greater than 27 if this function was used to create the X27_TEMP table but in the unlikely event that the
---   the sequence returns the number 27, this function will check to see if a table named X27_TEMP already exists.
---   It will sense that this table DOES exist and therefore grab the next sequence nmber (in this case 28).  It will
---   then check to see if a table named X28_TEMP exists.  It will sense that it DOES NOT exist and return
---   the value of X28_TEMP.  This way one can be gauranteed that unique temp table names will always be
---   returned.
---
---
--- Dependencies:
---   CDB_UTIL.table_exists
---   CDB_UTIL.before
---   cdb_tmptable_seq Sequence
---   cdb_chains_seq Sequence
---   cdb_dissolve_seq Sequence
---
--- Modification Histroy:
---   06/29/2009, Nick Padfield - added the pCallingProc parameter and edited this procedure to insert
---                               information into the CDB_TMPTABLE_TRACKING table.
---   12/18/2009, Nick Padfield - Modified procedure to call different sequences for CDB_CHAINS and
---                               CDB_DISSOLVE
---
---------------------------------------------------------------------------------------------------------------------

   InSchema             VARCHAR2(30)            := UPPER(pInSchema);
   TableSuffix          VARCHAR2(16)            := SUBSTR(UPPER(pTableSuffix),1,16);
   ReturnSchemaPortion  VARCHAR2(3)             := UPPER(pReturnSchemaPortion);
   ExitStatus           BOOLEAN                 := FALSE;
   TablePrefix          VARCHAR2(15);
   CurrentName          VARCHAR2(30);
   ReturnValue          VARCHAR2(61);
   vSeqNum              NUMBER;
   vRecCount            NUMBER;
   vCallingProc         VARCHAR2(100)           := SUBSTR(UPPER(pCallingProc),1,100);
   vTrackingTable       VARCHAR2(30)            := 'CDB_TMPTABLE_TRACKING';
   sql_stmt             VARCHAR2(4000);
BEGIN
   --------------------------------------------------------------------------------

   --Refers to a global variable in the CDB code, wont work in generalization
   -- Validate parameters
--   IF (InSchema = 'CURRENT') THEN
--      InSchema := g_cs;
--   END IF;
   ----------------
   IF (LENGTH(pTableSuffix) > 16) THEN
      dbms_output.put_line('WARNING in CDB_UTIL.get_avail_temptable_name:');
      dbms_output.put_line('The pTableSuffix parameter that was supplied was greater than 16 digits in length');
      dbms_output.put_line('The pTableSuffix value will be truncated to 16 digits: ' || TableSuffix);
      dbms_output.put_line('Please make a note of this...');
   END IF;
   ----------------
   IF ((ReturnSchemaPortion <> 'YES') AND (ReturnSchemaPortion <> 'NO')) THEN
      dbms_output.put_line('ERROR in CDB_UTIL.get_avail_temptable_name: Invalid pReturnSchemaPortion parameter.');
      dbms_output.put_line('The only valid values are: ''YES'' -or- ''NO''');
      dbms_output.put_line('Returning NULL...');
      RETURN NULL;
   END IF;
   ----------------
   SELECT count(*) INTO vRecCount FROM all_sequences WHERE sequence_name = 'CDB_TMPTABLE_SEQ';
   IF (vRecCount < 1) THEN
      dbms_output.put_line('ERROR in CDB_UTIL.get_avail_temptable_name: Sequence (CDB_TMPTABLE_SEQ) Not Found!');
      dbms_output.put_line('This function cannot successfully operate without this sequence');
      dbms_output.put_line('Returning NULL...');
      RETURN NULL;
   END IF;
   --------------------------------------------------------------------------------
   -- Set TablePrefix and Sequence appropriately
   IF TableSuffix IN ('TOCHAIN_ED','REL_OUT','GEOM_TEMP','REL_TEMP','FREQ_TEMP','PROCESS_TEMP','PASS_TEMP','ID_TEMP','TILE_TEMP','MTE2TMP','CROSS_IDS_TEMP') THEN  -- CDB_CHAINS
      LOOP
         SELECT cdb_chains_seq.nextval INTO vSeqNum FROM dual;
         CurrentName := TO_CHAR('Y'||vSeqNum||TableSuffix);
         IF NOT (CDB_UTIL.table_exists(CurrentName,InSchema)) THEN
            ExitStatus := TRUE;
         END IF;
         EXIT WHEN (ExitStatus = TRUE);
      END LOOP;
   ELSIF (UPPER(CDB_UTIL.before(vCallingProc,'.')) = 'CDB_DISSOLVE') THEN --CDB_DISSOLVE
      LOOP
         SELECT cdb_dissolve_seq.nextval INTO vSeqNum FROM dual;
         CurrentName := TO_CHAR('Z'||vSeqNum||TableSuffix);
         IF NOT (CDB_UTIL.table_exists(CurrentName,InSchema)) THEN
            ExitStatus := TRUE;
         END IF;
         EXIT WHEN (ExitStatus = TRUE);
      END LOOP;
   ELSE
      LOOP
         SELECT cdb_tmptable_seq.nextval INTO vSeqNum FROM dual;
         CurrentName := TO_CHAR('X'||vSeqNum||'_'||TableSuffix);
         IF NOT (CDB_UTIL.table_exists(CurrentName,InSchema)) THEN
            ExitStatus := TRUE;
         END IF;
         EXIT WHEN (ExitStatus = TRUE);
      END LOOP;
   END IF;
   --------------------------------------------------------------------------------
   -- Return The result
   IF (ReturnSchemaPortion = 'YES') THEN
      ReturnValue := UPPER(InSchema)||'.'||UPPER(CurrentName);
   ELSE
      ReturnValue := UPPER(CurrentName);
   END IF;
   -- write to the Tracking Table
   IF CDB_UTIL.table_exists(vTrackingTable) THEN
         sql_stmt := 'INSERT INTO '||vTrackingTable||' (TMPTABLE_NAME,CALLING_PROC,DATETIME) VALUES (:1,:2,:3)';
         EXECUTE IMMEDIATE sql_stmt USING ReturnValue,vCallingProc,CURRENT_TIMESTAMP;
         COMMIT;
   END IF;
   RETURN ReturnValue;
   --------------------------------------------------------------------------------
EXCEPTION
   WHEN OTHERS THEN
      CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
END get_avail_temptable_name;

*/
--

/* COMMENTING OUT BECAUSE IT RELIES ON CDB_UTIL - PLEASE UPDATE IF YOU USE THIS FUNCTION!
/* otherwise, I'm going to delete it all together.

FUNCTION table_exists(pInTable IN VARCHAR2, pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN AS

-------------------------------------------------------------------------------------------------------------
- Program Name: table_exists
- Author: Nick Padfield
- Creation Date: 9/6/2006
-
- Usage: (How do I call this program and what parameters does it need?)
-   Call this program from inside another PL/SQL program.  This program
-   has one required parameter and one optional parameter:
-
-     REQUIRED Parameter:
-        pInTable              - The name of a table that you want to check existance of.
-
-     OPTIONAL Parameters:
-        pInSchema             - The Schema that pInTable resides in.  If not supplied, it will default
-                              to the current Schema
-
- Purpose: (What does this program do or what values does it return?)
-   The purpose of this procedure is to check for an existance of a table. The function will
-   return TRUE if a table exists and FALSE if the table does not exist.
-
- Dependencies: (Do other programs need to be in place for this program to run?)
-   There are no known dependencies.
-
- Modification History:
-   Nick Padfield, 11/3/2006 - Code modified to be included in a package.
-------------------------------------------------------------------------------------------------------------

   InTable       VARCHAR2(100)     := UPPER(pInTable);
   InSchema      VARCHAR2(100)     := UPPER(pInSchema);
   RecordCount   NUMBER(22)        := 0;
   sql_stmt      VARCHAR2(4000);
BEGIN
   ------------------------------------------------------
   -- Check to see if the table exists!
   sql_stmt := 'SELECT COUNT(*) FROM all_objects WHERE object_type = ''TABLE'' AND owner = ''' || NVL (InSchema, USER) || ''' AND object_name = ''' || InTable || '''';
   EXECUTE IMMEDIATE sql_stmt INTO RecordCount;
   ------------------------------------------------------
   -- If table exists, return TRUE, if not, return FALSE.
   IF RecordCount > 0 THEN
      dbms_output.put_line(NVL (InSchema, USER) || '.' || InTable || ' exists.');
      RETURN TRUE;
   ELSE
      dbms_output.put_line(NVL (InSchema, USER) || '.' || InTable || ' does NOT exist!');
      RETURN FALSE;
   END IF;
EXCEPTION
   WHEN OTHERS THEN
      CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
END table_exists;
--
*/



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   PROCEDURE CREATE_GEN_XTEND_TRACKING_LOG (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 6/03/11
      --Creates empty logging table that is compatible with GEN_EXTENDED_TRACKING_LOG

      psql          VARCHAR2(4000);
      v_object_root VARCHAR2(4000) := p_table_name;  --??


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Create the empty table as an empty pipelined custom type');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      psql := 'CREATE TABLE ' || p_table_name || ' ';


      psql := psql || ' NOPARALLEL NOLOGGING AS '
                   || 'SELECT * FROM TABLE(' || p_schema || '.GZ_TYPES.NEW_GEN_EXTENDED_TRACKING_LOG ) ';

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



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Grant privileges on ' || p_table_name);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS', p_table_name);

      --special public select in case debugging from non GZ schema
      EXECUTE IMMEDIATE 'GRANT SELECT ON ' || p_table_name || ' TO "PUBLIC" ';



   END CREATE_GEN_XTEND_TRACKING_LOG;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   PROCEDURE GEN_TRACKING_LOG (
     p_process        IN VARCHAR2,
     p_table_name     IN VARCHAR2 DEFAULT NULL,
     p_step           IN VARCHAR2 DEFAULT NULL,
     p_start_time     IN TIMESTAMP DEFAULT NULL,
     p_end_time       IN TIMESTAMP DEFAULT NULL,
     p_release        IN VARCHAR2 DEFAULT NULL,
     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
     p_deploy         IN VARCHAR2 DEFAULT NULL
   )
   AS PRAGMA AUTONOMOUS_TRANSACTION;

      --3/2/10
      --Call like:
      --  GZ_UTILITIES.GEN_TRACKING_LOG('MYSTEP',NULL,NULL,start_time);
      --5/01/10 I dont think anyone is actually calling this
      --Matt!s jumping ship to GEN_CLIP_TRACKING_LOG (below)
      --6/3/11 Looks like some of the remove_obs_nodes code uses this fella
      --3/27/2012 Removed dependency on this in remove_obs_nodes procedures.
      --          I think we can drop this unless Salman uses it. (Stephanie)

      psql           VARCHAR2(4000);
      v_end_time     TIMESTAMP;
      elapsed_time   interval DAY(5) TO second (2);

   BEGIN

      IF p_start_time IS NOT NULL
      AND p_end_time IS NULL
      THEN
         v_end_time := systimestamp;
      ELSE
         v_end_time := p_end_time;
      END IF;

      IF p_start_time IS NOT NULL
      THEN
         elapsed_time := v_end_time - p_start_time;
      END IF;


      psql := 'INSERT /*+ APPEND */ INTO GEN_TRACKING '
           || 'VALUES(:p1,:p2,:p3,:p4,:p5,:p6,:p7,:p8,:p9) ';

      EXECUTE IMMEDIATE psql USING p_table_name,
                                   p_process,
                                   p_step,
                                   p_start_time,
                                   v_end_time,
                                   elapsed_time,
                                   p_release,
                                   p_sqlstmt,
                                   p_deploy;
      COMMIT;

   END GEN_TRACKING_LOG;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   PROCEDURE GEN_CLIP_TRACKING_LOG (
     p_jobrun         IN VARCHAR2,
     p_process        IN VARCHAR2,
     p_table_name     IN VARCHAR2 DEFAULT NULL,
     p_step           IN VARCHAR2 DEFAULT NULL,
     p_start_time     IN TIMESTAMP DEFAULT NULL,
     p_end_time       IN TIMESTAMP DEFAULT NULL,
     p_release        IN VARCHAR2 DEFAULT NULL,  -- I dont know what this is
     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
     p_deploy         IN VARCHAR2 DEFAULT NULL,  -- ?Also
     p_error_msg      IN VARCHAR2 DEFAULT NULL,
     p_sdo_dump       IN SDO_GEOMETRY DEFAULT NULL
   )
   AS

      --5/1/10 Matt!
      --6/3/11 ...
      --This is now just a backwards compatible wrapper for the clipper
      --It calls gen_extended_tracking_log
      --When I have time I will get rid of it, make clip calls directly to generic extended logger



   BEGIN

      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('CLIP',
                                             p_jobrun,
                                             p_process,
                                             p_table_name,
                                             p_step,
                                             p_start_time,
                                             p_end_time,
                                             p_release,
                                             p_sqlstmt,
                                             p_deploy,
                                             p_error_msg,
                                             p_sdo_dump);


   END GEN_CLIP_TRACKING_LOG;


    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   PROCEDURE GEN_EXTENDED_TRACKING_LOG (
     p_module         IN VARCHAR2,
     p_jobrun         IN VARCHAR2,
     p_process        IN VARCHAR2,
     p_table_name     IN VARCHAR2 DEFAULT NULL,
     p_step           IN VARCHAR2 DEFAULT NULL,
     p_start_time     IN TIMESTAMP DEFAULT NULL,
     p_end_time       IN TIMESTAMP DEFAULT NULL,
     p_release        IN VARCHAR2 DEFAULT NULL,  -- I dont know what this is
     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
     p_deploy         IN VARCHAR2 DEFAULT NULL,  -- ?Also
     p_error_msg      IN VARCHAR2 DEFAULT NULL,
     p_sdo_dump       IN SDO_GEOMETRY DEFAULT NULL
   )
   AS PRAGMA AUTONOMOUS_TRANSACTION;

      --6/03/11 Matt! Copied and renamed from gen_clip_tracking_log

      --11/15/11 ! Removed autonomous transaction pragma
      --11/15/11 ! Added topofix to list of acceptable callers

      --If the caller is tracking start and end times and both are relevant, pass them both in
      --   Or just pass in the start and we'll calculate the current time as the end
      --If not really tracking elapsed time, dont pass in either and we'll put the current time
      --   in both the start time and the end time, no elapsed

      --12/21/11 Continued waffling on autonomous transaction pragma

      --02/06/12 Added build tracking
      --5/3/12 added output tracking
      --10/12/12 added MERGEFACE tracking, only expected to be used in standalone work



      psql           VARCHAR2(4000);
      v_start_time   TIMESTAMP;
      v_end_time     TIMESTAMP;
      elapsed_time   interval DAY(5) TO second (2);

   BEGIN

      IF UPPER(p_module) NOT IN ('CLIP','MERGE','TOPOFIX','BUILD','OUTPUT','SUPER','ZONE','LS', 'SP','PROJECTION','MERGEFACE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Yo buddy, whats module ' || p_module || '? Add it here please ');

      END IF;

      IF p_start_time IS NOT NULL
      AND p_end_time IS NULL
      THEN
         v_start_time := p_start_time;
         v_end_time := systimestamp;
      ELSE
         v_start_time := p_start_time;
         v_end_time := p_end_time;
      END IF;

      IF p_start_time IS NOT NULL
      THEN
         elapsed_time := v_end_time - p_start_time;
      END IF;

      IF p_start_time IS NULL
      THEN
         v_start_time := systimestamp;
         v_end_time := systimestamp;
      END IF;


      psql := 'INSERT /*+ APPEND */ INTO ' || p_jobrun || '_' || p_module || '_TRACKING '
           || 'VALUES(:p1,:p2,:p3,:p4,:p5,:p6,:p7,:p8,:p9,:p10,:p11,:p12) ';


      EXECUTE IMMEDIATE psql USING p_table_name,
                                   p_process,
                                   p_step,
                                   v_start_time,
                                   v_end_time,
                                   elapsed_time,
                                   p_release,
                                   p_sqlstmt,
                                   p_deploy,
                                   USER,
                                   p_error_msg,
                                   p_sdo_dump;
      COMMIT;

   END GEN_EXTENDED_TRACKING_LOG;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

  PROCEDURE GEN_SP_TRACKING_LOG (
     p_jobrun         IN VARCHAR2,
     p_process        IN VARCHAR2,
     p_table_name     IN VARCHAR2 DEFAULT NULL,
     p_step           IN VARCHAR2 DEFAULT NULL,
     p_start_time     IN TIMESTAMP DEFAULT NULL,
     p_end_time       IN TIMESTAMP DEFAULT NULL,
     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
     p_message      IN VARCHAR2 DEFAULT NULL  -- SK Changed to p_message from p_err_msg
   )
   AS PRAGMA AUTONOMOUS_TRANSACTION;
      --COPIED from Matt's Gen Clip Tracking Log
      -- DO NOT USE!!!!
      -- NOT FINISHED!!!! -------------------------
      --If the caller is tracking start and end times and both are relevant, pass them both in
      --   Or just pass in the start and we'll calculate the current time as the end
      --If not really tracking elapsed time, dont pass in either and we'll put the current time
      --   in both the start time and the end time, no elapsed
      psql           VARCHAR2(4000);
      v_start_time   TIMESTAMP;
      v_end_time     TIMESTAMP;
      elapsed_time   interval DAY(5) TO second (2);
   BEGIN

     -- DBMS_OUTPUT.PUT_LINE('Begin Insert');
      IF p_start_time IS NOT NULL
      AND p_end_time IS NULL
      THEN
         v_start_time := p_start_time;
         v_end_time := systimestamp;
      ELSE
         v_start_time := p_start_time;
         v_end_time := p_end_time;
      END IF;
      IF p_start_time IS NOT NULL
      THEN
         elapsed_time := v_end_time - p_start_time;
      END IF;
      IF p_start_time IS NULL
      THEN
         v_start_time := systimestamp;
         v_end_time := systimestamp;
      END IF;

      psql := 'INSERT /*+ APPEND */ INTO ' || p_jobrun || '_SP_TRACKING '
           || 'VALUES(:p1,:p2,:p3,:p4,:p5,:p6,:p7,:p8,:p9,:p10) ';
      EXECUTE IMMEDIATE psql USING p_jobrun,
                                   p_process,
                                   p_step,
                                   v_start_time,
                                   v_end_time,
                                   elapsed_time,
                                   p_sqlstmt,
                                   USER,
                                   p_message,
                                   USER;
      COMMIT;
   END GEN_SP_TRACKING_LOG;

------------------------------------------------------------------------------
  PROCEDURE GEN_FSL_TRACKING_LOG (
     p_jobrun         IN VARCHAR2,
     p_process        IN VARCHAR2,
     p_table_name     IN VARCHAR2 DEFAULT NULL,
     p_step           IN VARCHAR2 DEFAULT NULL,
     p_start_time     IN TIMESTAMP DEFAULT NULL,
     p_end_time       IN TIMESTAMP DEFAULT NULL,
     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
     p_message      IN VARCHAR2 DEFAULT NULL  -- SK Changed to p_message from p_err_msg
   )
   AS PRAGMA AUTONOMOUS_TRANSACTION;
      --COPIED from Sreeni's gen_sp_tracking log, but the notes there say it is bogus!
      psql           VARCHAR2(4000);
      v_start_time   TIMESTAMP;
      v_end_time     TIMESTAMP;
      elapsed_time   interval DAY(5) TO second (2);
   BEGIN

     -- DBMS_OUTPUT.PUT_LINE('Begin Insert');
      IF p_start_time IS NOT NULL
      AND p_end_time IS NULL
      THEN
         v_start_time := p_start_time;
         v_end_time := systimestamp;
      ELSE
         v_start_time := p_start_time;
         v_end_time := p_end_time;
      END IF;
      IF p_start_time IS NOT NULL
      THEN
         elapsed_time := v_end_time - p_start_time;
      END IF;
      IF p_start_time IS NULL
      THEN
         v_start_time := systimestamp;
         v_end_time := systimestamp;
      END IF;

      psql := 'INSERT /*+ APPEND */ INTO ' || p_jobrun || '_FSL_TRACKING '
           || 'VALUES(:p1,:p2,:p3,:p4,:p5,:p6,:p7,:p8,:p9,:p10) ';
      EXECUTE IMMEDIATE psql USING p_jobrun,
                                   p_process,
                                   p_step,
                                   v_start_time,
                                   v_end_time,
                                   elapsed_time,
                                   p_sqlstmt,
                                   USER,
                                   p_message,
                                   USER;
      COMMIT;
   END GEN_FSL_TRACKING_LOG;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION GET_SDO_TOPO_METADATA_CHAR (
      p_topo                     IN VARCHAR2,
      p_table                    IN VARCHAR2,
      p_what                     IN VARCHAR2,
      p_column                   IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN VARCHAR2
   AS
   
      --Matt! 2/14/13  @}--^--^--
      
      --p_topo can be schema.topo or just topo
      --   if just topo, then I will assume the topo is local
      --p_table can be schema.table or just table
      --   I will never use the schema part of that one
      --p_what is any column in user/all_sdo_topo_metadata that is defined as a varchar
      
      psql                 VARCHAR2(4000);
      topo                 VARCHAR2(32);
      owner                VARCHAR2(32);
      table_name           VARCHAR2(32);
      dict                 VARCHAR2(4);
      column_name          VARCHAR2(32);
      output               VARCHAR2(64);
   
   BEGIN
   
      IF p_table LIKE '%$'
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Sorry cuz, right idea, but $ tables like ' || p_table || ' arent registered in the metadata');
      
      END IF; 
   
      IF GZ_UTILITIES.GET_X_OF_THA_DOT(p_topo, 'LEFT') IS NOT NULL
      THEN
      
         dict  := 'ALL';
         owner := GZ_UTILITIES.GET_X_OF_THA_DOT(p_topo, 'LEFT');
         topo  := GZ_UTILITIES.GET_X_OF_THA_DOT(p_topo, 'RIGHT');         
      
      ELSE
      
         dict  := 'USER';
         owner := SYS_CONTEXT('USERENV','CURRENT_USER');
         topo  := UPPER(p_topo);      
      
      END IF;
   
      IF GZ_UTILITIES.GET_X_OF_THA_DOT(p_table, 'RIGHT') IS NOT NULL
      THEN
      
         table_name := GZ_UTILITIES.GET_X_OF_THA_DOT(p_table, 'RIGHT');         
      
      ELSE
      
         table_name := p_table;     
      
      END IF;
   
      column_name := UPPER(p_column);
      
      psql := 'SELECT a.' || p_what || ' ' 
           || 'FROM ' || dict || '_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.owner = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.table_name = :p3 AND '
           || 'a.column_name = :p4 ';
        
      BEGIN
         
         EXECUTE IMMEDIATE psql INTO output USING owner,
                                                  topo,
                                                  table_name,
                                                  column_name;
                                                  
      EXCEPTION
      WHEN OTHERS
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'ERROR> ' || SQLERRM || ' < on ' || psql || ' with binds- '
                                 || owner || ' ' || topo || ' ' || table_name || ' ' || column_name);
      
      END;
      
      RETURN output;

   
   END GET_SDO_TOPO_METADATA_CHAR;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION GET_SDO_TOPO_METADATA_NUM (
      p_topo                     IN VARCHAR2,
      p_table                    IN VARCHAR2,
      p_what                     IN VARCHAR2,
      p_column                   IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN NUMBER
   AS
   
      --Matt! 2/14/13  @}--^--^--
    
      --p_topo can be schema.topo or just topo
      --   if just topo, then I will assume the topo is local
      --p_table can be schema.table or just table
      --   I will never use the schema part of that one
      --p_what is any column in user/all_sdo_topo_metadata that is defined as a varchar
      
      psql                 VARCHAR2(4000);
      topo                 VARCHAR2(32);
      owner                VARCHAR2(32);
      table_name           VARCHAR2(32);
      dict                 VARCHAR2(4);
      column_name          VARCHAR2(32);
      output               NUMBER;
   
   BEGIN
   
      IF p_table LIKE '%$'
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Sorry cuz, right idea, but $ tables like ' || p_table || ' arent registered in the metadata');
      
      END IF; 
   
      IF GZ_UTILITIES.GET_X_OF_THA_DOT(p_topo, 'LEFT') IS NOT NULL
      THEN
      
         dict  := 'ALL';
         owner := GZ_UTILITIES.GET_X_OF_THA_DOT(p_topo, 'LEFT');
         topo  := GZ_UTILITIES.GET_X_OF_THA_DOT(p_topo, 'RIGHT');         
      
      ELSE
      
         dict  := 'USER';
         owner := SYS_CONTEXT('USERENV','CURRENT_USER');
         topo  := UPPER(p_topo);      
      
      END IF;
   
      IF GZ_UTILITIES.GET_X_OF_THA_DOT(p_table, 'RIGHT') IS NOT NULL
      THEN
      
         table_name := GZ_UTILITIES.GET_X_OF_THA_DOT(p_table, 'RIGHT');         
      
      ELSE
      
         table_name := p_table;     
      
      END IF;
   
      column_name := UPPER(p_column);
      
      psql := 'SELECT a.' || p_what || ' ' 
           || 'FROM ' || dict || '_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.owner = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.table_name = :p3 AND '
           || 'a.column_name = :p4 ';
        
      BEGIN
         
         EXECUTE IMMEDIATE psql INTO output USING owner,
                                                  topo,
                                                  table_name,
                                                  column_name;
                                                  
      EXCEPTION
      WHEN OTHERS
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'ERROR> ' || SQLERRM || ' < on ' || psql || ' with binds- '
                                 || owner || ' ' || topo || ' ' || table_name || ' ' || column_name);
      
      END;
      
      RETURN output;
   
   END GET_SDO_TOPO_METADATA_NUM;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION GET_X_OF_THA_DOT (
      p_input                    IN VARCHAR2,
      p_x                        IN VARCHAR2 DEFAULT 'LEFT'
   ) RETURN VARCHAR2 DETERMINISTIC
   AS
   
      --Matt! 2/12/2013
   
   BEGIN
   
      IF INSTR(p_input,'.') = 0
      THEN
      
         --executive decision.  Theres nothing to the side of nothing
         RETURN NULL;
      
      END IF;
   
      IF UPPER(p_x) = 'LEFT'
      THEN
            
         RETURN UPPER(SUBSTR(p_input, 0, (INSTR(p_input,'.') - 1) ));
         
      ELSIF UPPER(p_x) = 'RIGHT'
      THEN
      
          RETURN UPPER(SUBSTR(p_input, (INSTR(p_input,'.') + 1) ));
                   
      ELSE
      
         RAISE_APPLICATION_ERROR(-20001, 'What the X is a ' || p_x || '?');
      
      END IF;
   
   
   END GET_X_OF_THA_DOT;


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


      GZ_UTILITIES.INSERT_SDOGEOM_METADATA(table_name,column_name,p_srid,p_tolerance);

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

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------


   PROCEDURE ADD_INDEX (
    p_table_name      IN VARCHAR2,
    p_index_name      IN VARCHAR2,
    p_column_name     IN VARCHAR2,
    p_type            IN VARCHAR2 DEFAULT NULL,
    p_parallel        IN NUMBER DEFAULT NULL,
    p_logging         IN VARCHAR2 DEFAULT NULL,
    p_local           IN VARCHAR2 DEFAULT NULL
   )
   AS

   --Matt!

    psql          VARCHAR2(4000);
    psql2         VARCHAR2(4000);
    table_name    VARCHAR2(4000) := UPPER(p_table_name);
    column_name   VARCHAR2(4000) := UPPER(p_column_name);

   BEGIN
   
      IF INSTR(p_table_name,'.') <> 0
      THEN
      
         RAISE_APPLICATION_ERROR(-20001,'Sorry database hero, I can''t index tables in remote schemas like ' || p_table_name);
      
      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_INDEX');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      psql := 'CREATE ';

      IF p_type IS NOT NULL
      THEN
         psql := psql || UPPER(p_type) || ' ';
      END IF;

      --we like to name our indexes like J150032490103_SRC01C
      psql := psql || 'INDEX '
                   || p_index_name
                   || ' ON ' || table_name || '(' || column_name || ') ';

      IF p_parallel IS NOT NULL
      THEN
         psql := psql || 'PARALLEL ' || TO_CHAR(p_parallel) || ' ';
      ELSE
         psql := psql || 'NOPARALLEL ';
      END IF;

      IF p_logging IS NOT NULL
      THEN
         psql := psql || 'LOGGING ';
      ELSE
         psql := psql || 'NOLOGGING ';
      END IF;

      IF p_local IS NOT NULL
      THEN
         psql := psql || 'LOCAL ';
      END IF;

      BEGIN

         --dbms_output.put_line(psql);
         EXECUTE IMMEDIATE psql;

      EXCEPTION
         WHEN OTHERS
         THEN

         psql2 := 'DROP INDEX ' || p_index_name;
         EXECUTE IMMEDIATE psql2;

         EXECUTE IMMEDIATE psql;

      END;


   END ADD_INDEX;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE GATHER_TABLE_STATS (
    p_table_name      IN VARCHAR2
   )
   AS

   --Matt! 8/5/10
   --Any new advice on these mystery parameters welcome
   --Added option 2 for topo tables 10/12/11 !
   --see related GATHER_TOPO_STATS below


   BEGIN


      IF UPPER(p_table_name) NOT LIKE '%$'
      THEN

         --standard tables
         --this is the version Matt tweaked out over time for CAMPS tables, histograms being the main problem

         DBMS_STATS.GATHER_TABLE_STATS(ownname => USER,
                                          tabname => p_table_name,
                                          granularity => 'AUTO',                 --Oracle determine what partition-level stats to get (default)
                                          degree => 1,                           --no parallelism on gather
                                          cascade => DBMS_STATS.AUTO_CASCADE, --Oracle determine whether stats on idxs too
                                          method_opt => 'FOR ALL COLUMNS SIZE 1' --This prevents histograms
                                          );

      ELSE

         --topo primitives and relation$ table
         --This is straight from Subu via Sreeni as the way the benchmarking team does relation$ stats

         DBMS_STATS.GATHER_TABLE_STATS(ownname => USER,
                                          tabname => p_table_name,
                                          degree => 1,              --no parallelism on gather, changed from SK 16
                                          cascade => TRUE,          --yes, always gather stats on idxs too
                                          estimate_percent=>20      --20 percent sample
                                          );

      END IF;



   END GATHER_TABLE_STATS;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE GATHER_TOPO_STATS (
      p_topo_name       IN VARCHAR2
   )
   AS

      --Matt! 10/14/11
      --pass in a topology name, get stats gathered on all registered feature and dollar tables

      psql        VARCHAR2(4000);
      p_topo      VARCHAR2(4000) := UPPER(p_topo_name);
      kount       PLS_INTEGER;
      tabs        GZ_TYPES.stringarray;

   BEGIN

      --cant log, but better do this so we can at least see if hung up
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GATHER_TOPO_STATS: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      psql := 'SELECT a.table_name '
           || 'FROM user_sdo_topo_info a '
           || 'WHERE a.topology = :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING p_topo;

      IF tabs.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Cant find a topology named ' || p_topo);

      ELSIF tabs(1) IS NULL
      THEN

         --this may be OK if we are just carrying around primitives
         --but until I hear that it exists as a legit workflow lets call it out

         RAISE_APPLICATION_ERROR(-20001,'Topology ' || p_topo || ' exists but there are no registered feature tables ');

      END IF;

      --add the $ tables
      tabs(tabs.COUNT + 1) := p_topo || '_RELATION$';
      tabs(tabs.COUNT + 1) := p_topo || '_EDGE$';
      tabs(tabs.COUNT + 1) := p_topo || '_NODE$';
      tabs(tabs.COUNT + 1) := p_topo || '_FACE$';
      --history$? Who cares

      FOR i IN 1 .. tabs.COUNT
      LOOP

         GZ_UTILITIES.GATHER_TABLE_STATS(tabs(i));

      END LOOP;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GATHER_TOPO_STATS: Done ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


   END GATHER_TOPO_STATS;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------   
   
   FUNCTION INDEX_EXISTS (
      p_table_name        IN VARCHAR2,
      p_column_name       IN VARCHAR2 DEFAULT NULL,
      p_index_type        IN VARCHAR2 DEFAULT NULL
   ) RETURN BOOLEAN
   AS
   
      --Matt! 2/13/13
      
      --p_table_name may be simply a user table, or a remote <schema>.<table>
      
      
      --Usually best to just do what you want index-wise and trap errors
      --But sometimes I really do need a checker (like in a check_inputs function)   
      --Tempted to use dbms_sql but its too slow
      
      --SAMPLE usage
      --
      --IF NOT GZ_UTILITIES.INDEX_EXISTS('GZACS12.Z699IN_FACE','SDOGEOMETRY','DOMAIN')
      --THEN
      --   --do something
      
      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      whos_table        VARCHAR2(4);
      who               VARCHAR2(32);
      the_table         VARCHAR2(32);
      
   BEGIN
   
      IF INSTR(p_table_name, '.') = 0
      THEN
      
         --USER
         
         whos_table := 'USER';
         who        := SYS_CONTEXT('USERENV','CURRENT_USER');
         the_table  := UPPER(p_table_name);
         
      ELSE
      
         --ALL    
           
         whos_table := 'ALL';
         who        := GZ_UTILITIES.GET_X_OF_THA_DOT(p_table_name, 'LEFT');
         the_table  := GZ_UTILITIES.GET_X_OF_THA_DOT(p_table_name, 'RIGHT');
      
      END IF;
         
      psql := 'SELECT COUNT(1) '
           || 'FROM ' || whos_table || '_indexes a ';
           
      IF p_column_name IS NOT NULL
      THEN
      
         psql := psql || ', '
                      || whos_table || '_ind_columns b ';
         
      END IF;
      
      psql := psql || 'WHERE '
                   || 'a.table_name = :p1 AND '
                   || 'a.table_owner = :p2 AND '
                   || 'a.status = :p3 ';
           
           
      IF p_column_name IS NULL
      AND p_index_type IS NULL
      THEN
      
         EXECUTE IMMEDIATE psql INTO kount USING the_table, 
                                                 who,
                                                 'VALID';
                                                 
      ELSIF p_column_name IS NOT NULL
      AND p_index_type IS NULL
      THEN
           
         psql := psql || 'AND '
                      || 'b.column_name = :p4 AND '
                      || 'a.index_name = b.index_name ';
      
         EXECUTE IMMEDIATE psql INTO kount USING the_table, 
                                                 who,
                                                 'VALID',
                                                 UPPER(p_column_name);
         
      ELSIF p_column_name IS NULL
      AND p_index_type IS NOT NULL
      THEN
      
          psql := psql || 'AND '
                       || 'a.index_type = :p4 ';
                       
         IF UPPER(p_index_type) <> 'DOMAIN'
         THEN
                    
            EXECUTE IMMEDIATE psql INTO kount USING the_table, 
                                                    who,
                                                    'VALID',
                                                    UPPER(p_index_type);
                                                    
         ELSE
         
            --only domain indices we deal with are spatial 
            
            psql := psql || 'AND '
                         || 'a.domidx_opstatus = :p5 ';
            
            EXECUTE IMMEDIATE psql INTO kount USING the_table, 
                                                    who,
                                                    'VALID',
                                                    UPPER(p_index_type),
                                                    'VALID';
         
         END IF;
      
      ELSIF p_column_name IS NOT NULL
      AND p_index_type IS NOT NULL
      THEN
      
         psql := psql || 'AND '
                      || 'a.index_type = :p4 AND '
                      || 'b.column_name = :p5 AND '
                      || 'a.index_name = b.index_name ';
                
         
         IF UPPER(p_index_type) <> 'DOMAIN'
         THEN
             
            EXECUTE IMMEDIATE psql INTO kount USING the_table, 
                                                    who,
                                                    'VALID',
                                                    UPPER(p_index_type),
                                                    UPPER(p_column_name);
                                                    
         ELSE
         
            psql := psql || 'AND '
                         || 'a.domidx_opstatus = :p6 ';
                         
            EXECUTE IMMEDIATE psql INTO kount USING the_table, 
                                                    who,
                                                    'VALID',
                                                    UPPER(p_index_type),
                                                    UPPER(p_column_name),
                                                    'VALID';
         
         
         END IF;
               
      ELSE
      
         RAISE_APPLICATION_ERROR(-20001, 'Bad logic.  Bad!');
      
      END IF;      
      
      
      IF kount = 0
      THEN
      
         RETURN FALSE;
         
      ELSE
      
         RETURN TRUE;
      
      END IF;
      
   
   END INDEX_EXISTS;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION COLUMN_EXISTS (
      p_table_name        IN VARCHAR2,
      p_column_list       IN VARCHAR2,
      p_column_kount      IN PLS_INTEGER DEFAULT 1
   ) RETURN BOOLEAN
   AS
   
      --Matt! 2/13/13
      
      --p_table_name may be simply a user table, or a remote <schema>.<table>
      --p_column_list must be a fully quoted and double quoted string.  Sorry
      
      --SAMPLE
      --
      --IF NOT GZ_UTILITIES.COLUMN_EXISTS('GZACS12.Z699IN_FACE', '''ANRC'',''AIANNHCOMP'',''AIANNH''', 3)
      --THEN
      --   --columns missing!
      --
      
      --SAMPLE for q-quoting mensches
      --
      --IF NOT GZ_UTILITIES.COLUMN_EXISTS('Z699IN_FACE', q'^'ANRC','AIANNHCOMP','AIANNH'^', 3)
      --THEN
      --
      
      
      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      whos_table        VARCHAR2(4);
      who               VARCHAR2(32);
      the_table         VARCHAR2(32);
   
   BEGIN
   
      IF INSTR(p_table_name, '.') = 0
      THEN
      
         --USER
         
         whos_table := 'USER';
         the_table  := UPPER(p_table_name);
         
      ELSE
      
         --ALL    
           
         whos_table := 'ALL';
         who        := GZ_UTILITIES.GET_X_OF_THA_DOT(p_table_name, 'LEFT');
         the_table  := GZ_UTILITIES.GET_X_OF_THA_DOT(p_table_name, 'RIGHT');
      
      END IF;
   
   
      psql := 'SELECT COUNT(*) '
           || 'FROM ' || whos_table || '_tab_columns a '
           || 'WHERE '
           || 'a.table_name = :p1 AND '
           || 'a.column_name IN (' || p_column_list || ') ';
           
      IF whos_table = 'USER'
      THEN
      
         EXECUTE IMMEDIATE psql INTO kount USING the_table;
      
      ELSE
      
         psql := psql || 'AND '
                      || 'a.owner = :p2 ';
                      
         --dbms_output.put_line(psql);
         EXECUTE IMMEDIATE psql INTO kount USING the_table,
                                                 who;
      
      END IF;
            
      
      IF kount <> p_column_kount
      THEN
      
         RETURN FALSE;
         
      ELSE
      
         RETURN TRUE;
      
      END IF;
   
   END COLUMN_EXISTS;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION SPLIT (
      p_str   IN VARCHAR2,
      p_regex IN VARCHAR2 DEFAULT NULL,
      p_match IN VARCHAR2 DEFAULT NULL,
      p_end   IN NUMBER DEFAULT 0
   ) RETURN GZ_TYPES.stringarray DETERMINISTIC
   AS
      int_delim      PLS_INTEGER;
      int_position   PLS_INTEGER := 1;
      int_counter    PLS_INTEGER := 1;
      ary_output     GZ_TYPES.stringarray;
   BEGIN

      IF p_str IS NULL
      THEN
         RETURN ary_output;
      END IF;

      --Split byte by byte
      --split('ABCD',NULL) gives back A  B  C  D
      IF p_regex IS NULL
      OR p_regex = ''
      THEN
         FOR i IN 1 .. LENGTH(p_str)
         LOOP
            ary_output(i) := SUBSTR(p_str,i,1);
         END LOOP;
         RETURN ary_output;
      END IF;

      LOOP
         EXIT WHEN int_position = 0;
         int_delim  := REGEXP_INSTR(p_str,p_regex,int_position,1,0,p_match);
         IF  int_delim = 0
         THEN
            -- no more matches found
            ary_output(int_counter) := SUBSTR(p_str,int_position);
            int_position  := 0;
         ELSE
            IF int_counter = p_end
            THEN
               -- take the rest as is
               ary_output(int_counter) := SUBSTR(p_str,int_position);
               int_position  := 0;
            ELSE
               ary_output(int_counter) := SUBSTR(p_str,int_position,int_delim-int_position);
               int_counter := int_counter + 1;
               int_position := REGEXP_INSTR(p_str,p_regex,int_position,1,1,p_match);
               IF int_position > length(p_str)
               THEN
                  int_position := 0;
               END IF;
            END IF;
         END IF;
      END LOOP;

     RETURN ary_output;

    END SPLIT;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION STRINGARRAY_TO_VARRAY (
      p_input     IN GZ_TYPES.stringarray
   ) RETURN MDSYS.STRING_ARRAY DETERMINISTIC
   AS

      output     MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
      pcounter    PLS_INTEGER := 1;
      pkey        PLS_INTEGER;

   BEGIN

     output.EXTEND(p_input.COUNT);
     pkey := p_input.FIRST;
     LOOP

       EXIT WHEN NOT p_input.EXISTS(pkey);

       output(pcounter) := p_input(pkey);
       pcounter := pcounter + 1;

       pkey  := p_input.NEXT(pkey);

     END LOOP;

      RETURN output;

   END STRINGARRAY_TO_VARRAY;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION NUMARRAY_TO_VARRAY (
      p_input     IN GZ_TYPES.numberarray
   ) RETURN MDSYS.STRING_ARRAY DETERMINISTIC
   AS

      output      MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
      pcounter    PLS_INTEGER := 1;
      pkey        PLS_INTEGER;

   BEGIN

     output.EXTEND(p_input.COUNT);
     pkey := p_input.FIRST;
     LOOP

       EXIT WHEN NOT p_input.EXISTS(pkey);

       output(pcounter) := p_input(pkey);
       pcounter := pcounter + 1;

       pkey  := p_input.NEXT(pkey);

     END LOOP;

      RETURN output;

   END NUMARRAY_TO_VARRAY;


   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION NUMARRAY_TO_ORDARRAY (
      p_input     IN MDSYS.SDO_NUMBER_ARRAY
   ) RETURN MDSYS.SDO_ORDINATE_ARRAY DETERMINISTIC
   AS

     --Matt! 1/21/11
     --Change_edge_coordinates always uses this silly number array

     output      MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
     pcounter    PLS_INTEGER := 1;
     pkey        PLS_INTEGER;

   BEGIN

     output.EXTEND(p_input.COUNT);
     pkey := p_input.FIRST;

     LOOP
        EXIT WHEN NOT p_input.EXISTS(pkey);

       output(pcounter) := p_input(pkey);
       pcounter := pcounter + 1;

       pkey  := p_input.NEXT(pkey);
     END LOOP;

     RETURN output;

   END NUMARRAY_TO_ORDARRAY;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION STRINGARRAY_ADD (
     p_input_1   IN GZ_TYPES.stringarray,
     p_input_2   IN GZ_TYPES.stringarray
   ) RETURN GZ_TYPES.stringarray DETERMINISTIC
   AS
      output     GZ_TYPES.stringarray;
     pcounter    PLS_INTEGER := 1;
     pkey        PLS_INTEGER;
   BEGIN

     pkey := p_input_1.FIRST;
     LOOP
        EXIT WHEN NOT p_input_1.EXISTS(pkey);

       output(pcounter) := p_input_1(pkey);
       pcounter := pcounter + 1;

       pkey  := p_input_1.NEXT(pkey);
     END LOOP;

     pkey := p_input_2.FIRST;
     LOOP
        EXIT WHEN NOT p_input_2.EXISTS(pkey);

       output(pcounter) := p_input_2(pkey);
       pcounter := pcounter + 1;

       pkey  := p_input_2.NEXT(pkey);
     END LOOP;

      RETURN output;

   END STRINGARRAY_ADD;
   
   
   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------
   
   FUNCTION CLOB_TO_VARRAY (
      p_input           IN CLOB,
      p_delimiter       IN VARCHAR2 DEFAULT ','
   ) RETURN MDSYS.STRING_ARRAY DETERMINISTIC
   AS
   
      --Matt! 2/1/13

      output         MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
      end_pos        NUMBER;
      start_pos      NUMBER;
      elementx       VARCHAR2(256);     --string_array element length
      kount          NUMBER := 0;
      deadman        NUMBER := 1048576; --string_array max size

   BEGIN
 
      start_pos := 1;
      end_pos := INSTR(p_input,p_delimiter,start_pos);
   
      LOOP
       
         IF end_pos <> 0
         THEN
         
            elementx := SUBSTR(p_input,start_pos,(end_pos - start_pos));
            
         ELSE
         
            --last element, no length
            elementx := SUBSTR(p_input,start_pos);
         
         END IF;
         
         IF kount = deadman
         THEN
         
            RAISE_APPLICATION_ERROR(-20001, 'Exceeded string array size limit');
         
         END IF;
         
         --tuck
         output.EXTEND(1);
         kount := kount + 1;
         output(kount) := elementx;
         
         IF end_pos <> 0
         THEN
                  
            start_pos := end_pos + 1;
            end_pos := INSTR(p_input,p_delimiter,start_pos);
            
         ELSE
         
            EXIT;
            
         END IF;

      END LOOP;

      RETURN output;

   END CLOB_TO_VARRAY;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION CONVERT_COL_TO_CLOB (
      p_sql             IN VARCHAR2,
      p_delimiter       IN VARCHAR2 DEFAULT ','      
   ) RETURN CLOB
   AS
   
      --Matt! 1/25/12
      --Helper for use in populating the gz_layers_subset.oid_clob column
      
      --Sample:
      --update gz_layers_subset
      --set oid_clob = gz_utilities.CONVERT_COL_TO_CLOB('select oid_base from gzcpb1.z699tm_fsl061v')
      --where release = 'ACS122' and gen_project_id = 'Z6' and layer = '061'
   
      output            CLOB := empty_clob();  --initialize object      
      my_cursor         SYS_REFCURSOR;
      valz              GZ_TYPES.stringarray;
      
   
   BEGIN
      
      OPEN my_cursor FOR p_sql;

      LOOP

         FETCH my_cursor BULK COLLECT INTO valz LIMIT 10000; 
         EXIT WHEN valz.COUNT = 0;

         FOR i in 1 .. valz.COUNT
         LOOP
            
            output := output || valz(i) || p_delimiter;            
            
         END LOOP;
            
      END LOOP;
   
   
      RETURN output;
   
   END CONVERT_COL_TO_CLOB;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   PROCEDURE BUILD_TOPO_FROM_SPATIAL (
      p_toponame           IN VARCHAR2,
      p_featuretable       IN VARCHAR2,
      p_topo_type          IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_validate_geom      IN VARCHAR2 DEFAULT 'NO',
      p_validate_tol       IN NUMBER DEFAULT .05,
      p_fix_geom           IN VARCHAR2 DEFAULT 'NO',
      p_topo_col           IN VARCHAR2 DEFAULT 'TOPOGEOM',
      p_geom_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_oid_col            IN VARCHAR2 DEFAULT NULL,
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_val         IN VARCHAR2 DEFAULT NULL
   ) AS

      --This is Matt's original create_feature wrapper
      --See ADD_TOPO_FROM_SPATIAL for a non-create_feature solution
      --I dont call this guy any more 4/27/12
      --Salman may still use it, he was at one point

      --Matt! 3/10/10
      --updated to handle and drop any other updatable topomaps attached to the calling session
      --   Matt! 5/25/10
      --Reworked updatable topomaps dropping 6/18/10
      --Added oid population option 7/10/10
      --Added options to build topo for subset of records 12/10/10

      --Expected Inputs
      --   1. A table (p_featuretable) with
      --      1a. Some sort of primary key column (p_featuretable_pkc) for ex edge_id
      --      1b. A column with geometry, populated and indexed (p_geom_col)
      --      1c. An empty topo column (p_topo_col)
      --   2. A pre-existing topology (p_toponame)
      --Output
      --   Populates the topo column and all the usual $ tables
      --   Optionally populates an oid-like column with the tg_id

      --ex: GZ_UTILITIES.BUILD_TOPO_FROM_SPATIAL('STATEFP10','GEN_ST_EDGES_HI_10','LINE','ID','YES',.05);

      --ex Feature table exists, I just want to populate some of the topo records
      --   GZ_UTILITIES.BUILD_TOPO_FROM_SPATIAL('STATEFP10','GEN_ST_EDGES_HI_10','LINE','ID','YES',.05, 'NO', 'TOPOGEOM,
      --                                        'SDOGEOMETRY', oid, 'DANGLE', 'T');  --only populate topo where DANGLE = T


      valresult            VARCHAR2(4000);
      psql                 VARCHAR2(4000);
      psql2                VARCHAR2(4000);
      kount                PLS_INTEGER;
      toponame             VARCHAR2(32) := UPPER(p_toponame);
      featuretable         VARCHAR2(32) := UPPER(p_featuretable);
      topo_type            VARCHAR2(32) := UPPER(p_topo_type);
      featuretable_pkc     VARCHAR2(32) := UPPER(p_featuretable_pkc);
      topo_col             VARCHAR2(32) := UPPER(p_topo_col);
      geom_col             VARCHAR2(32) := UPPER(p_geom_col);
      newtopomap           VARCHAR2(4000) := toponame || '_TOPOMAP';  --ever need to parameterize?
      my_cursor            SYS_REFCURSOR;
      TYPE toporec         IS RECORD (
                           id NUMBER,
                           sdogeometry SDO_GEOMETRY );
      TYPE topot           IS TABLE OF toporec;
      topotab              topot;
      topogeom_arr         GZ_TYPES.SDO_TOPO_GEOMETRY_ARRAY_PLSINT;
      featuretable_ids     GZ_TYPES.stringarray;
      featuretable_oids    GZ_TYPES.stringarray;
      topomaps             GZ_TYPES.stringarray;
      ez_topo_mgr          NUMBER;

   BEGIN



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Verify Inputs ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_validate_geom != 'NO'
      THEN

         --validate geometry, adapted from advice in topo documentation
         --http://download.oracle.com/docs/html/B14256_01/sdo_topo_concepts.htm#CIHBGDEC

         psql := 'SELECT count(*) '
              || 'FROM ' || featuretable || ' a '
              || 'WHERE '
              || 'SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.' || geom_col || ', :p1) != :p2 ';

         IF p_subset_col IS NOT NULL
         THEN

            psql := psql || ' AND ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

         END IF;

         EXECUTE IMMEDIATE psql INTO kount USING p_validate_tol,
                                                 'TRUE';

         IF kount != 0
         THEN

            --Most likely duplicate vertices

            IF p_fix_geom = 'NO'
            THEN

               RAISE_APPLICATION_ERROR(-20001,'Geometries in ' || featuretable || ' are not valid ');

            ELSE

               --Let the grasping at straws begin

               psql2 := 'UPDATE ' || featuretable || ' a '
                     || 'SET '
                     || 'a.' || geom_col || ' = SDO_UTIL.REMOVE_DUPLICATE_VERTICES(a.' || geom_col || ' , ' || p_validate_tol || ' ) ';

               EXECUTE IMMEDIATE psql2;

               EXECUTE IMMEDIATE psql INTO kount USING p_validate_tol,
                                                       'TRUE';

               IF kount != 0
               THEN

                  RAISE_APPLICATION_ERROR(-20001,'Geometries in ' || featuretable || ' are not valid even after vertex removal ');

               END IF;

            END IF;


         END IF;

      END IF;


      IF topo_type NOT IN ('POINT', 'LINE', 'CURVE', 'POLYGON', 'COLLECTION')
      THEN

         --valid inputs to SDO_TOPO.add_topo_geometry_layer
         RAISE_APPLICATION_ERROR(-20001, 'What the heck is a topo type of ' || topo_type);

      END IF;


      --???????????????????????????
      --What else should we check??
      --???????????????????????????


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Add topo layer ' || featuretable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_subset_col IS NULL
      THEN

         SDO_TOPO.add_topo_geometry_layer(toponame,featuretable,topo_col,topo_type);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Initialize topo map ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      -- Mike Z Voodoo? Salman also rceommends this
      --But Sreeni points out that if you use sensibly sized topo maps you shouldnt need it
      -- Set sdo_topo_map maximum Java memory size
      --EZ_TOPOMAP_MANAGER handles this now.  EZ!
      --SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(2147483648);


      ez_topo_mgr := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(newtopomap,toponame,2);


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Create features for ' || featuretable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      psql := 'SELECT ' || featuretable_pkc || ', ' || geom_col || ' '
           || 'FROM ' || featuretable || ' ';

           IF p_subset_col IS NOT NULL
           THEN

              psql := psql || ' WHERE ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

           END IF;


      BEGIN

         OPEN my_cursor FOR psql;

         LOOP

            FETCH my_cursor BULK COLLECT INTO topotab LIMIT 100; --parameterize LIMIT?
            EXIT WHEN topotab.COUNT = 0;

            FOR i in 1 .. topotab.COUNT
            LOOP


               topogeom_arr(i) := SDO_TOPO_MAP.CREATE_FEATURE(toponame,featuretable,topo_col,topotab(i).sdogeometry);
               featuretable_ids(i) := topotab(i).id;


               IF p_oid_col IS NOT NULL
               THEN

                  --if desired, build a separate array of tg_ids
                  --to populate the source table oid-like field
                  featuretable_oids(i) := topogeom_arr(i).tg_id;

               END IF;


            END LOOP;

            --Update this bucket's worth
            --1 transaction Yo

            IF p_oid_col IS NULL
            THEN

               FORALL ii IN 1 .. featuretable_ids.COUNT
                  EXECUTE IMMEDIATE 'UPDATE ' || featuretable || ' a '
                                 || 'SET a.' || topo_col || ' = :p1 '
                                 || 'WHERE a.' || featuretable_pkc || ' = :p2 '
                  USING topogeom_arr(ii),
                        featuretable_ids(ii);

            ELSE

               FORALL ii IN 1 .. featuretable_ids.COUNT
                  EXECUTE IMMEDIATE 'UPDATE ' || featuretable || ' a '
                                 || 'SET a.' || topo_col || ' = :p1, '
                                 || 'a.' || p_oid_col || ' = :p2 '
                                 || 'WHERE a.' || featuretable_pkc || ' = :p3 '
                  USING topogeom_arr(ii),
                        featuretable_oids(ii),
                        featuretable_ids(ii);


            END IF;


            COMMIT;

            --Tidy up
            featuretable_ids.DELETE;
            topogeom_arr.DELETE;

            IF p_oid_col IS NOT NULL
            THEN
               featuretable_oids.DELETE;
            END IF;


         END LOOP;

      END;

      CLOSE my_cursor;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Commit n Drop topomap ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      SDO_TOPO_MAP.COMMIT_TOPO_MAP();
      SDO_TOPO_MAP.DROP_TOPO_MAP(newtopomap);


      --Initialize metadata here? Maybe we should parameterize this Y or N
      SDO_TOPO.INITIALIZE_METADATA(toponame);


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Complete ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Anything to return?


   END BUILD_TOPO_FROM_SPATIAL;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

    PROCEDURE ADD_TOPO_FROM_SPATIAL (
      p_toponame           IN VARCHAR2,
      p_featuretable       IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_topo_type          IN VARCHAR2,
      p_log_type           IN VARCHAR2,
      p_geom_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_val         IN VARCHAR2 DEFAULT NULL,
      p_allow_splits       IN VARCHAR2 DEFAULT 'Y',
      p_new_layer          IN VARCHAR2 DEFAULT 'N',
      p_topomap_mbr        IN SDO_GEOMETRY DEFAULT NULL
   ) AS

      --Matt! 4/27/12
      --Generic replacement for create feature
      -- *ADD*_TOPO_FROM.. because it calls the ADD_xxx interfaces
      --Calls ADD_POLYGON/LINEAR/POINT_GEOMETRY
      --Then constructs the topogeom column

      --Sample call:
      --GZ_UTILITIES.ADD_TOPO_FROM_SPATIAL('T699LS','T699LS_CUTTER','CUTTER_ID','POLYGON',
      --                                   'MERGE','SDOGEOMETRY',NULL,NULL,'Y','Y');


      psql                 VARCHAR2(4000);
      kount                PLS_INTEGER;
      toponame             VARCHAR2(32) := UPPER(p_toponame);
      featuretable         VARCHAR2(32) := UPPER(p_featuretable);
      featuretable_pkc     VARCHAR2(32) := UPPER(p_featuretable_pkc);
      geom_col             VARCHAR2(32) := UPPER(p_geom_col);
      newtopomap           VARCHAR2(4000) := toponame || '_TOPOMAP';  --ever need to parameterize?
      my_cursor            SYS_REFCURSOR;
      TYPE toporec         IS RECORD (
                           id NUMBER,
                           sdogeometry SDO_GEOMETRY );
      TYPE topot           IS TABLE OF toporec;
      topotab              topot;
      ez_topo_mgr          NUMBER;
      stupid_number_array  SDO_NUMBER_ARRAY;
      TYPE stupidrec       IS RECORD (
                           feat_id NUMBER,
                           prim_ids SDO_TOPO_OBJECT_ARRAY );
      TYPE stupid_arr      IS TABLE OF stupidrec INDEX BY PLS_INTEGER;
      stupid_array         stupid_arr;
      kurrent_kount        PLS_INTEGER := 0;
      tg_layer_id          NUMBER;
      me_tg_type           NUMBER;


   BEGIN


      --verify inputs?

      IF p_topo_type NOT IN ('POINT', 'LINE', 'POLYGON')
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'What the heck is a topo type of ' || p_topo_type || ' ?');

      END IF;

      IF p_topo_type = 'POINT'
      THEN
         me_tg_type := 1;
      ELSIF p_topo_type = 'LINE'
      THEN
         me_tg_type := 2;
      ELSIF p_topo_type = 'POLYGON'
      THEN
         me_tg_type := 3;
      END IF;

      --table exists?

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_TOPO_FROM_SPATIAL: Initialize topo map ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF p_topomap_mbr IS NULL
      THEN

         --entire topology topomap, can be expensive unless empty (empty for for clip)
         ez_topo_mgr := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(newtopomap,toponame,2);

      ELSE

         --window topomap

         IF p_topomap_mbr.sdo_ordinates.COUNT != 4
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Whoa buddy, input mbr has ' || p_topomap_mbr.sdo_ordinates.COUNT || ' ordinates ');

         END IF;

         ez_topo_mgr := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(newtopomap,
                                                        toponame,
                                                        2,
                                                        p_topomap_mbr.sdo_ordinates(1),
                                                        p_topomap_mbr.sdo_ordinates(2),
                                                        p_topomap_mbr.sdo_ordinates(3),
                                                        p_topomap_mbr.sdo_ordinates(4)
                                                        );

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_TOPO_FROM_SPATIAL: Create features for ' || featuretable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      psql := 'SELECT ' || featuretable_pkc || ', ' || geom_col || ' '
           || 'FROM ' || featuretable || ' ';

      IF p_subset_col IS NOT NULL
      THEN

         psql := psql || ' WHERE ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

      END IF;

      IF p_log_type IS NOT NULL
      THEN
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                toponame,
                                                'ADD_TOPO_FROM_SPATIAL',
                                                featuretable,
                                                'Opening cursor to call add_xxxx_geometry',
                                                NULL,NULL,NULL,psql);
      END IF;


      BEGIN

         OPEN my_cursor FOR psql;

         LOOP

            FETCH my_cursor BULK COLLECT INTO topotab LIMIT 25; --parameterize LIMIT?
            EXIT WHEN topotab.COUNT = 0;

            FOR i in 1 .. topotab.COUNT
            LOOP


               BEGIN


                  --THE CALL----
                  --**********--

                  IF p_topo_type = 'POLYGON'
                  THEN

                     stupid_number_array := SDO_TOPO_MAP.ADD_POLYGON_GEOMETRY(NULL,topotab(i).sdogeometry);

                  ELSIF p_topo_type = 'LINE'
                  THEN

                     stupid_number_array := SDO_TOPO_MAP.ADD_LINEAR_GEOMETRY(NULL,topotab(i).sdogeometry);

                  ELSIF p_topo_type = 'POINT'
                  THEN

                     --never tried this. Just return a single number
                     stupid_number_array(1) := SDO_TOPO_MAP.ADD_POINT_GEOMETRY(NULL,topotab(i).sdogeometry);

                  END IF;

                  --------------
                  --------------

                  IF p_allow_splits = 'N'
                  AND stupid_number_array.COUNT > 1
                  THEN

                     IF p_log_type IS NOT NULL
                     THEN
                        GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                               toponame,
                                                               'ADD_TOPO_FROM_SPATIAL',
                                                               featuretable,
                                                               'ADD_TOPO_FROM_SPATIAL error message on id ' || topotab(i).id,
                                                               NULL,NULL,NULL,NULL,NULL,
                                                               'You said no splits, but we got ' || stupid_number_array.COUNT || ' primitives',
                                                               topotab(i).sdogeometry);
                     END IF;

                     RAISE_APPLICATION_ERROR(-20001,'You said no splits, but we got ' || stupid_number_array.COUNT || ' primitives');

                  END IF;


               EXCEPTION
               WHEN OTHERS
               THEN

                  IF p_log_type IS NOT NULL
                  THEN
                     GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                            toponame,
                                                            'ADD_TOPO_FROM_SPATIAL',
                                                            featuretable,
                                                            'ADD_XXXX_GEOMETRY error message--> ',
                                                            NULL,NULL,NULL,NULL,NULL,
                                                            SQLERRM
                                                            );
                  END IF;

                  IF (UPPER(SQLERRM) LIKE '%OUTOFMEMORYERROR%'
                  OR UPPER(SQLERRM) LIKE '%JAVA OUT OF MEMORY CONDITION%')  --WTF causes this instead of the first?
                  THEN

                     --nothing in this handler has ever succeeded
                     --attempting to avoid this ahead of time in the caller

                     IF p_log_type IS NOT NULL
                     THEN
                        GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                               toponame,
                                                               'ADD_TOPO_FROM_SPATIAL',
                                                               featuretable,
                                                               'Memory error, gonna call the magick java_memory_manager ');

                     END IF;

                     BEGIN

                        --this will probably just choke too
                        --SDO_TOPO_MAP.COMMIT_TOPO_MAP();
                        --SDO_TOPO_MAP.DROP_TOPO_MAP(newtopomap);

                        --this is just a placeholder, not expecting it to work at present

                        GZ_UTILITIES.JAVA_MEMORY_MANAGER(featuretable,
                                                         'SDOGEOMETRY',
                                                         SQLERRM);  --<-- handle it wizard!

                     EXCEPTION
                     WHEN OTHERS
                     THEN

                        IF p_log_type IS NOT NULL
                        THEN
                           GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                                  toponame,
                                                                  'ADD_TOPO_FROM_SPATIAL',
                                                                  featuretable,
                                                                  'JAVA_MEMORY_MANAGER failed to clean house-->',
                                                                  NULL,NULL,NULL,NULL,NULL,
                                                                  SQLERRM);
                        END IF;

                        RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

                     END;


                  ELSE

                     RAISE;

                  END IF;

               END;

               --*************---
               --TUCK whats wes haves gots
               ------------------

               --counter
               kurrent_kount := kurrent_kount + 1;

               --place the feature id in the outer record id
               stupid_array(kurrent_kount).feat_id := topotab(i).id;
               --stupid extend the nested object
               stupid_array(kurrent_kount).prim_ids := SDO_TOPO_OBJECT_ARRAY();
               stupid_array(kurrent_kount).prim_ids.EXTEND(stupid_number_array.COUNT);

               --stupid objects
               FOR jj IN 1 .. stupid_number_array.COUNT
               LOOP

                  --prim_ids: SDO_TOPO_OBJECT_ARRAY is VARRAY (1000000) of SDO_TOPO_OBJECT
                  stupid_array(kurrent_kount).prim_ids(jj) := SDO_TOPO_OBJECT(stupid_number_array(jj), me_tg_type);

               END LOOP;

               -----------------
               --End tucking the results of this poly/edge/point into our nested objects
               --Back for another
               ------------------


            END LOOP;


         END LOOP;

      END;

      CLOSE my_cursor;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_TOPO_FROM_SPATIAL: Commit n Drop topomap ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_log_type IS NOT NULL
      THEN
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                toponame,
                                                'ADD_TOPO_FROM_SPATIAL',
                                                 featuretable,
                                                 'Commit and drop topo map');
      END IF;



      SDO_TOPO_MAP.COMMIT_TOPO_MAP();
      SDO_TOPO_MAP.DROP_TOPO_MAP(newtopomap);

      IF p_log_type IS NOT NULL
      THEN
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                toponame,
                                                'ADD_TOPO_FROM_SPATIAL',
                                                 featuretable,
                                                 'Completed commit and drop topo map');
      END IF;




      --after topo map commit...

      IF p_new_layer = 'Y'
      THEN

         SDO_TOPO.add_topo_geometry_layer(toponame,featuretable,'TOPOGEOM',p_topo_type);

      END IF;

      tg_layer_id := GZ_UTILITIES.GET_TG_LAYER_ID(toponame,
                                                  featuretable,
                                                  'TOPOGEOM',
                                                  p_topo_type);

      psql := 'UPDATE ' || featuretable || ' a '
           || 'SET '
           || 'a.topogeom = SDO_TOPO_GEOMETRY(:p1,:p2,:p3,:p4) '
           || 'WHERE a.' || featuretable_pkc || ' = :p5 ';

      IF p_log_type IS NOT NULL
      THEN
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                toponame,
                                                'ADD_TOPO_FROM_SPATIAL',
                                                featuretable,
                                                'Constructorfest on ' || stupid_array.COUNT || ' features',
                                                NULL,NULL,NULL,psql);
      END IF;

      FORALL ii IN 1 .. stupid_array.COUNT
         EXECUTE IMMEDIATE psql
         USING toponame,
               me_tg_type,
               tg_layer_id,
               stupid_array(ii).prim_ids,
               stupid_array(ii).feat_id;

      COMMIT;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_TOPO_FROM_SPATIAL: Complete ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_log_type IS NOT NULL
      THEN
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                toponame,
                                                'ADD_TOPO_FROM_SPATIAL',
                                                featuretable,
                                                'Done');
      END IF;

      --Anything to return?


   END ADD_TOPO_FROM_SPATIAL;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION MAKE_ARROWHEAD(
      p_geom            IN SDO_GEOMETRY,
      p_scalefactor     IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY
   AS

     --Sidey!! and Matt! 3/5/10

     --Just a helper, not called in production
     --use like:
     --select GZ_utilities.make_arrowhead(a.geom) from
     --   newfeaturetype a where a.st99_hi_edges_ = 1

     --Way too big, or small?  Add the scalefactor parameter
     --select GZ_utilities.make_arrowhead(a.geom,5) from
     --   newfeaturetype a where a.st99_hi_edges_ = 1

     --Color and fill in mapviewer

      twopi     CONSTANT NUMBER  := 6.2831853071795864769252867665590057684; -- pi*2

      angle            NUMBER;
      sin_angle        NUMBER;
      cos_angle        NUMBER;
      x1new            NUMBER;
      y1new            NUMBER;
      x2new            NUMBER;
      y2new            NUMBER;
      xmid             NUMBER;
      ymid             NUMBER;
      xnew             NUMBER;
      ynew             NUMBER;
      x3               NUMBER;
      y3               NUMBER;
      x4               NUMBER;
      y4               NUMBER;
      x1               NUMBER;
      y1               NUMBER;
      x2               NUMBER;
      y2               NUMBER;
      delta            NUMBER := 1;

      ordinates       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
      kount           PLS_INTEGER;
      mystart         PLS_INTEGER;
      triangle        SDO_GEOMETRY;


   BEGIN



      IF p_geom.sdo_gtype != '2002'
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Sorry, gtype is ' || p_geom.sdo_gtype);
      END IF;

      IF p_scalefactor IS NOT NULL
      THEN

         IF p_scalefactor <= 0
         THEN
            RAISE_APPLICATION_ERROR(-20001,'Scale factor must be greater than zero');
         END IF;

         delta := delta * (p_scalefactor / 100);

      END IF;

      --get roughly the middle segment
      kount := p_geom.sdo_ordinates.COUNT;
      --Dont let Sidey see this
      mystart := (kount/2 - 1);
      IF mod(mystart,2) = 0
      THEN
         --must start on an X
         mystart := mystart + 1;
      END IF;

      x1 := p_geom.sdo_ordinates(mystart);
      --dbms_output.put_line('kount is ' || kount);
      --dbms_output.put_line('x1 ' || x1);
      y1 := p_geom.sdo_ordinates(mystart+1);
      --dbms_output.put_line('y1 ' || y1);
      x2 := p_geom.sdo_ordinates(mystart+2);
      --dbms_output.put_line('x2 ' || x2);
      y2 := p_geom.sdo_ordinates(mystart+3);
      --dbms_output.put_line('y2 ' || y2);

      angle :=  atan2(y2-y1,x2-x1);
      if angle < 0. then
         angle :=  (twopi + angle);
      end if;
      sin_angle := sin(angle);
      cos_angle := cos(angle);
      --dbms_output.put_line('sin ' || sin_angle || ' cos ' || cos_angle);
      ymid := x1;
      xmid := y1;

       -- Fist rotate x2,y2 clockwise about (x1,y1)
       x2new := ROUND((y2 -ymid),8) * sin_angle + ROUND((x2 -xmid),8)* cos_angle;
       y2new :=  ROUND((y2 -ymid),8) * cos_angle - ROUND((x2 -xmid),8)* sin_angle;

       --dbms_output.put_line('x1new ' || x1new || ' y1new ' || y1new);
       --dbms_output.put_line('x2new ' || x2new || ' y2new ' || y2new);
       -- then rotate counterclockwise about x1,y1 and add back on the translation
       x3 := (x2new -delta) * cos_angle - (y2new+delta) * sin_angle + xmid;
       y3 := (x2new -delta) * sin_angle + (y2new+delta) * cos_angle + ymid;
       x4 := (x2new -delta) * cos_angle - (y2new-delta) * sin_angle + xmid;
       y4 := (x2new -delta) * sin_angle + (y2new-delta) * cos_angle + ymid;
       --dbms_output.put_line('x3 ' || x3 || ' y3 ' || y3);
       --dbms_output.put_line('x4 ' || x4 || ' y4 ' || y4);

       triangle := SDO_GEOMETRY(2003,
                                p_geom.SDO_SRID,
                                NULL,
                                SDO_ELEM_INFO_ARRAY (1,1003,1),
                                SDO_ORDINATE_ARRAY (x1,y1,
                                                    x3,y3,
                                                    x4,y4,
                                                    x1,y1)
                                );

      --This doesn't work too well since the scaling may move the triangle
      --completely off the line
      --IF p_scalefactor IS NOT NULL
      --THEN
         --triangle := GZ_CLIP.scale2percent(triangle, p_scalefactor);
      --END IF;


      RETURN triangle;

   END MAKE_ARROWHEAD;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CLOSED_LOOPS_HELPER (
      p_schema          IN VARCHAR2,
      p_table_name      IN VARCHAR2,
      p_log_table       IN VARCHAR2,
      p_state_code      IN VARCHAR2 DEFAULT NULL,
      p_srid            IN NUMBER DEFAULT 4269,
      p_tolerance       IN NUMBER DEFAULT .05,
      p_tidy_topo       IN VARCHAR2 DEFAULT 'N'
   )
   AS

      --Matt! 1/20/11
      --Swithced to add_linear_geometry 5/23/12
      --Helper to wrap the closed loop checker
      --Meant for standalone investigations
      --Written kinda on the cheap, not ready for prime time
      --sample: check all states
      --   BEGIN
      --   GZ_UTILITIES.CLOSED_LOOPS_HELPER('GZCPB_1','GZDEC10ST99.STATE_EDGES_Z6_V2','LOOPZ_TRACKER20100120');
      --   END;
      --   -----> See table LOOPZ_TRACKER20100120 for results
      --
      --sample: check one state, and clean up the temp topology
      --   BEGIN
      --   GZ_UTILITIES.CLOSED_LOOPS_HELPER('GZCPB_1','GZDEC10ST99.STATE_EDGES_Z6_V2','LOOPZ_TRACKER44','44',4269,.05,'Y');
      --   END;
      --   -----> See table LOOPZ_TRACKER44 for results


      ptopo       varchar2(32);
      ptabname    varchar2(32);
      states      gz_types.stringarray;
      psql        varchar2(4000);
      retval      varchar2(4000);

   BEGIN

      --set up logger
      BEGIN
         psql := 'create table ' || p_log_table || ' (state varchar2(4000), message varchar2(4000)) ';
         execute immediate psql;

         EXCEPTION
            when others then
            execute immediate 'drop table ' || p_log_table || ' ';
            execute immediate psql;

      END;

      IF p_state_code IS NULL
      THEN

         --get universe of states from source
         psql := 'select distinct state from ('
            || 'select l_statefp state from ' || p_table_name || ' '
            || 'union all '
            || 'select r_statefp state from ' || p_table_name || ' '
            || ') '
            || 'where state is not null '
            || 'order by state ';

         execute immediate psql bulk collect into states;

      ELSE

         states(1) := p_state_code;

      END IF;



      --start loop on states

      for i in 1 .. states.count
      loop

         --   IF i = 3
         --   THEN
         --      raise_application_error(-20001, 'peppahs!');
         --   end if;


         ptopo := 'LOOPZ_' || states(i);
         ptabname := 'LOOPZ_TAB' || states(i);


         --blow away anything topology related if rerunning
         BEGIN
            gz_topo_util.purge_topology(p_schema ,ptopo);

            EXCEPTION
            WHEN OTHERS THEN
               IF UPPER(SQLERRM) LIKE '%DOES NOT EXIST IN%'
               THEN
                  NULL;
               ELSE
                  RAISE;
               END IF;
         END;

         --drop the work table if there
         BEGIN
            execute immediate 'drop table ' || ptabname || ' ';

            EXCEPTION
               when others then
               NULL;
         END;


         --create table for this state
         psql := 'create table ' || ptabname || ' as select '
               || 'CAST(rownum AS NUMBER) ID, '
               || 'sdogeometry, '
               || 'CAST(NULL AS SDO_TOPO_GEOMETRY) TOPOGEOM '
               || 'from ' || p_table_name || ' '
               || 'where l_statefp = ' || states(i) || ' or r_statefp = ' || states(i) || ' ';
         execute immediate psql;

         --create topology
         SDO_TOPO.create_topology(ptopo,
                                   p_tolerance,
                                   p_srid,
                                   NULL,
                                   NULL,
                                   NULL,
                                   NULL,
                                   16);

         psql := 'INSERT INTO ' || ptopo || '_FACE$ '
              || 'VALUES (:p1, null, :p2, :p3, null)';
         EXECUTE IMMEDIATE psql USING -1,
                                            sdo_list_type(),
                                            sdo_list_type();
         commit;


         --build topogeom for this state

         --generic add_linear_geometry + constructors call

         GZ_UTILITIES.ADD_TOPO_FROM_SPATIAL(ptopo,
                                            ptabname,
                                            'ID',
                                            'LINE',
                                            NULL,      --no logging
                                            'SDOGEOMETRY',
                                            NULL,         --no subset
                                            NULL,
                                            'N',          --no allow splits. Should intersect nothing at this point
                                            'Y');         --yes new layer

         /* BEAT
         GZ_UTILITIES.BUILD_TOPO_FROM_SPATIAL(ptopo,
                                               ptabname,
                                                  'LINE',
                                                  'ID',
                                                  'YES',
                                                  .05,
                                                  'YES',
                                                  'TOPOGEOM',
                                                  'SDOGEOMETRY',
                                                  NULL);
         */

         --see what the loop checker thinks
         retval := '';

         BEGIN
            retval :=  gz_utilities.closed_loops(ptabname, ptopo);

            IF retval != 'Y'
            THEN
               psql := 'INSERT INTO ' || p_log_table || ' VALUES(:p1,:p2) ';
               EXECUTE IMMEDIATE psql USING states(i), retval;
               commit;
            ELSE
               psql := 'INSERT INTO ' || p_log_table || ' VALUES(:p1,:p2) ';
               EXECUTE IMMEDIATE psql USING states(i), 'OK';
               commit;
            END IF;

            --lets clean up if successful?
            --gz_topo_util.purge_topology(pschema ,ptopo);

         EXCEPTION
            WHEN OTHERS THEN

               psql := 'INSERT INTO ' || p_log_table || ' VALUES(:p1,:p2) ';
               execute immediate psql using states(i), SQLERRM;
               commit;
         END;

         IF p_tidy_topo = 'Y'
         THEN

            gz_topo_util.purge_topology(p_schema ,ptopo);

         END IF;



      end loop;



   END CLOSED_LOOPS_HELPER;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION CLOSED_LOOPS (
      p_tablename       IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_edge_id         IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2
   AS

   --Matt! 3/12/10
   --Fix! for negative/reversed topo_ids 5/19/10
   --Bug workaround! Set "_OPTIMIZER_CONNECT_BY_COST_BASED" = false 5/20/10


   --Pass in a linear feature table and topology name
   --Returns 'Y' when the feature table contains only closed loops
   --Returns 'N edge_id:xxx' when not, xxx is any edge_id encountered
   --                        at either a T intersection or a gap

   --Caveat: I suppose some geographies (ex Incplaces) can't be modeled as closed loops

   --Assumption: We've got a standard topology edge type table
   --            containing edges, start nodes, and end nodes

   --Positive reinforcement: Repeat after me: "Carrying around buckets of edge
   --                        ids and nodes for any reasonable (sub million)
   --                        number of edges is a cakewalk for a computer"

   --Future: Could be modified to return some sort of linked list of edges

   --IN pseudocode
   --1. Grab an edge from table at a direction reversal point (attempt start to start) and call it the "root"
   --   Save the root edge id and nodes
   --   If there is any reversal point in a loop then there is always a start to start and
   --   an end to end.  We are choosing start to start as code start for no particular reason
   --   If there is no start to start, the loop loops entirely without a leaf and any edge works
   --2. Tunnel forward, connecting end to start, as far as possible
   --   Save edges and nodes along the way
   --3. Check that we have only one leaf along the way (the end)
   --4. Check that our end "leaf" is not actually the root, if yes, loop closed
   --5. Get the next edge, reversing connection type
   --6. Tunnel forward, connecting in reverse fashion
   --    Repeat from step 2
   --7. When we've closed that loop, find another edge not on the loop
   --   Start at step 2 for that loop
   --8. If weve traversed all edges successfully, Y


      tablename      VARCHAR2(32) := UPPER(p_tablename);
      topology       VARCHAR2(32) := UPPER(p_topology);
      psql           VARCHAR2(4000);
      psql2          VARCHAR2(4000);
      ouroboros      VARCHAR2(4000);
      wrapsql        VARCHAR2(4000);
      subquery       VARCHAR2(4000);
      edge_id        NUMBER;
      currentedgez   GZ_TYPES.stringarray;
      currentstartz  GZ_TYPES.stringarray;
      currentendz    GZ_TYPES.stringarray;
      leafz          GZ_TYPES.stringarray;
      kount          PLS_INTEGER;
      badedge        NUMBER;
      alledgez       GZ_TYPES.stringarray;
      allstartz      GZ_TYPES.stringarray;
      allendz        GZ_TYPES.stringarray;
      multiloopedgez GZ_TYPES.stringarray;
      singlestart    NUMBER;
      singleend      NUMBER;
      singleflag     PLS_INTEGER;
      infinitechek   PLS_INTEGER := 0;
      altersession   VARCHAR2(4000);


   BEGIN

      --Bug 6521934  May be patched in 11.2
      altersession := 'ALTER SESSION Set "_OPTIMIZER_CONNECT_BY_COST_BASED" = false';
      EXECUTE IMMEDIATE altersession;
      --store the text away so we can set it back at the various return points
      altersession := 'ALTER SESSION Set "_OPTIMIZER_CONNECT_BY_COST_BASED" = true';

      --store subquery
      subquery := 'SELECT e.edge_id, e.start_node_id, e.end_node_id '
               || 'FROM '
               || tablename || ' a, '
               || topology || '_RELATION$ r, '
               || topology || '_EDGE$ e '
               || 'WHERE '
               || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
               || 'r.tg_id = a.topogeom.tg_id AND '
               || 'e.edge_id = ABS(r.topo_id) ';  --ABS for negative topo ids
                                                  --Their original SDO direction
                                                  -- is reversed from edge$ direction


      IF p_edge_id IS NULL
      THEN

         ouroboros := 'SELECT y.edge_id FROM ( '
                   || subquery
                   || ') z, '
                   || '( '
                   || subquery
                   || ') y '
                   || 'WHERE z.start_node_id = y.start_node_id AND '
                   || 'z.edge_id != y.edge_id AND '
                   || 'rownum = 1 ';


         BEGIN

            --if this works, we have found a loop composed of at least
            --2 chains, at least one facing the other
            --    0--->0---><---0<---0
            EXECUTE IMMEDIATE ouroboros INTO edge_id;

         EXCEPTION
            WHEN NO_DATA_FOUND THEN

               --The laws I sketched on my notebook suggest that
               --There are no facing chains here, and any edge will work
               --Because it will Ouroboros on itself and we will find that munching point

               psql := 'SELECT edge_id '
                    || 'FROM  ('
                    || subquery || ') '
                    || 'WHERE '
                    || 'rownum = 1 ';
               EXECUTE IMMEDIATE psql INTO edge_id;

             WHEN OTHERS THEN
               RAISE;
         END;

      ELSE

         --This should only be used in debugging mode
         edge_id := p_edge_id;

      END IF;



      <<logoturtle>>

      --This code LOOP traverses a single physical loop

      LOOP

         --Loops with no guaranteed exit give me the fantods
         IF infinitechek > 100000
         THEN
            RAISE_APPLICATION_ERROR(-20001,'yo, we have looped this code ' || infinitechek || ' times!');
         ELSE
            infinitechek := infinitechek + 1;
         END IF;



         --First, check if our edge id is a looped edge with no other edges
         psql := 'SELECT a.start_node_id, a.end_node_id '
              || 'FROM ' || topology || '_EDGE$ a '
              || 'WHERE a.edge_id = :p1 ';
         EXECUTE IMMEDIATE psql INTO singlestart, singleend USING edge_id;


         IF singlestart = singleend
         THEN
            singleflag := 1;
         ELSE
            singleflag := 0;
         END IF;


         --tunnel as far as we can get
         --end2start (reading left to right)
         --  0--edge_id--->0---->0--->
         psql := 'SELECT q.edge_id, q.start_node_id, q.end_node_id, CONNECT_BY_ISLEAF leaf '
              || 'FROM ( '
              || subquery || ' '
              || ') q '
              || 'START WITH q.edge_id = :p1 '
              || 'CONNECT BY NOCYCLE PRIOR q.end_node_id = q.start_node_id ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO currentedgez,
                                                  currentstartz,
                                                  currentendz,
                                                  leafz USING edge_id;


         --check that only one of the leafs is a 1
         --more would indicate a "T" or "X" or more intersection

         psql2 := 'SELECT count(*) FROM '
               || 'TABLE(:p1) t '
               || 'WHERE t.column_value = :p2 ';

         EXECUTE IMMEDIATE psql2 INTO kount
                                 USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(leafz),
                                 '1';  --varchar?

         IF kount > 1
         THEN

            --Get the first bad edge that comes back as a leaf and exit

            wrapsql := 'SELECT qq.edge_id FROM ( '
                     || psql || ' '
                     || ') qq '
                     || 'WHERE qq.leaf = :p1 '
                     || 'and rownum = 1 ';
            EXECUTE IMMEDIATE wrapsql INTO badedge USING 1;

                  psql := 'ALTER SESSION Set "_OPTIMIZER_CONNECT_BY_COST_BASED" = false';
      EXECUTE IMMEDIATE psql;

            EXECUTE IMMEDIATE altersession;
            RETURN 'N edge_id:' || badedge;

         END IF;

         IF singleflag = 1
         THEN
            --dont forget to add this edge to our bucket dummy
            alledgez := GZ_UTILITIES.stringarray_add(alledgez,currentedgez);

            --we checked our single edge loop to make sure he connected to no one else
            --he is good
            EXIT;
         END IF;


         --stash what weve got
         alledgez := GZ_UTILITIES.stringarray_add(alledgez,currentedgez);
         allstartz := GZ_UTILITIES.stringarray_add(allstartz,currentstartz);
         allendz := GZ_UTILITIES.stringarray_add(allendz,currentendz);
         --Delete the temps
         currentedgez.DELETE;
         currentstartz.DELETE;
         currentendz.DELETE;
         leafz.DELETE; --No need to stash

         --get the next edge
         --end2start means switch to end2end node in whereclause
         --  0---->0---->0--above--><--below--0
         psql := 'SELECT q.edge_id '
              || 'FROM ( '
              || subquery || ' '
              || ') q '
              || 'WHERE q.end_node_id = :p1 '  --reverse
              || 'AND q.edge_id != :p2 ';


         BEGIN

            EXECUTE IMMEDIATE psql INTO edge_id USING allendz(allendz.COUNT),
                                                      alledgez(alledgez.COUNT);
           EXCEPTION
            WHEN NO_DATA_FOUND THEN

              --We may have fully looped on this try
              psql := 'SELECT q.edge_id '
                   || 'FROM ( '
                   || subquery || ' '
                   || ') q '
                   || 'WHERE q.start_node_id = :p1 '  --reverse
                   || 'AND q.edge_id != :p2 ';

              --dbms_output.put_line(psql);
              --dbms_output.put_line(allendz(allendz.COUNT));
              --dbms_output.put_line(alledgez(alledgez.COUNT));

              BEGIN
                 EXECUTE IMMEDIATE psql INTO edge_id USING allendz(allendz.COUNT),
                                                           alledgez(alledgez.COUNT);

                 IF edge_id = alledgez(1)
                 THEN
                    -- We looped a loop, this is good
                    -- Exit the current loop and get a new edge, if applicable
                    EXIT;
                 ELSE
                    RAISE_APPLICATION_ERROR(-20001,'I, a computer, have no idea whats going on here');
                 END IF;

              EXCEPTION
                 WHEN NO_DATA_FOUND
                 THEN

                    RAISE_APPLICATION_ERROR(-20001,'Loop stuck! Check node ' || allendz(allendz.COUNT) );

              END;

            WHEN OTHERS THEN
               RAISE;
         END;

         --Check that this next edge_id isnt already in our list
         --We need to check more than just whether its the root (alledgez(1))
         --because a chain could link back on itself at a point other than the root
         --   if shaped like, for ex, a "P"

         psql := 'SELECT count(*) FROM '
              || 'TABLE(:p1) '
              || 'WHERE column_value = :p2 ';

         EXECUTE IMMEDIATE psql INTO kount USING
                                           GZ_UTILITIES.STRINGARRAY_TO_VARRAY(alledgez),
                                           edge_id;

         IF kount = 1
         AND edge_id != alledgez(1)
         THEN
            EXECUTE IMMEDIATE altersession;
            RETURN 'N edge_id:' || edge_id;
         ELSIF kount = 1
         AND edge_id = alledgez(1)
         THEN
            -- We looped a loop, this is good
            -- Exit the current loop and get a new edge
            EXIT;
         ELSE
            --We are just partway thru a chain
            NULL;
         END IF;


         --Do we need to check for a looped single edge here?
         --Dont think so

         --tunnel as far as we can get
         --Now START2END (reading Left to Right)
         --  <--edge_id--0<---0<---<---0
         psql := 'SELECT q.edge_id, q.start_node_id, q.end_node_id, CONNECT_BY_ISLEAF leaf '
              || 'FROM ( '
              || subquery || ' '
              || ') q '
              || 'START WITH q.edge_id = :p1 '
              || 'CONNECT BY NOCYCLE PRIOR q.start_node_id = q.end_node_id ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO currentedgez,
                                                  currentstartz,
                                                  currentendz,
                                                  leafz USING edge_id;


         --check that only one of the leafs is a 1
         --more would indicate a "T" or "X" or more intersection

         psql2 := 'SELECT count(*) FROM '
               || 'TABLE(:p1) t '
               || 'WHERE t.column_value = :p2 ';

         EXECUTE IMMEDIATE psql2 INTO kount USING
                                            GZ_UTILITIES.STRINGARRAY_TO_VARRAY(leafz),
                                            '1';  --varchar?


         IF kount > 1
         THEN

            --Get the first bad edge that comes back as a leaf and exit

            wrapsql := 'SELECT qq.edge_id FROM ( '
                     || psql || ' '
                     || ') qq '
                     || 'WHERE qq.leaf = :p1 '
                     || 'and rownum = 1 ';
            EXECUTE IMMEDIATE wrapsql INTO badedge USING 1;

            EXECUTE IMMEDIATE altersession;
            RETURN 'N edge_id:' || badedge;

         END IF;


         --stash what weve got
         alledgez := GZ_UTILITIES.stringarray_add(alledgez,currentedgez);
         allstartz := GZ_UTILITIES.stringarray_add(allstartz,currentstartz);
         allendz := GZ_UTILITIES.stringarray_add(allendz,currentendz);
         --Delete the temp
         currentedgez.DELETE;
         currentstartz.DELETE;
         currentendz.DELETE;
         leafz.DELETE; --No need to stash


         --get the next edge
         --start2end means switch to start to start node in where
         --  <---0<---<--last_edge---00---->
         psql := 'SELECT q.edge_id '
              || 'FROM ( '
              || subquery || ' '
              || ') q '
              || 'WHERE q.start_node_id = :p1 '  --reverse
              || 'AND q.edge_id != :p2 ';

         BEGIN

            EXECUTE IMMEDIATE psql INTO edge_id USING allstartz(allstartz.COUNT),
                                                      alledgez(alledgez.COUNT);

            EXCEPTION
            WHEN NO_DATA_FOUND THEN

              --We may have fully looped on this try
              psql := 'SELECT q.edge_id '
                   || 'FROM ( '
                   || subquery || ' '
                   || ') q '
                   || 'WHERE q.end_node_id = :p1 '  --reverse
                   || 'AND q.edge_id != :p2 ';

                   --dbms_output.put_line(psql);
                   --dbms_output.put_line(allstartz(allstartz.COUNT));
                   --dbms_output.put_line(alledgez(alledgez.COUNT));

              BEGIN

                 EXECUTE IMMEDIATE psql INTO edge_id USING allstartz(allstartz.COUNT),
                                                           alledgez(alledgez.COUNT);
                 EXCEPTION
                 WHEN OTHERS THEN
                    RAISE_APPLICATION_ERROR(-20001,'Our loop is hitting a dead end at edge ' || alledgez(alledgez.COUNT));

              END;

              IF edge_id = alledgez(1)
              THEN
                 -- We looped a loop, this is good
                 -- Exit the current loop and get a new edge, if applicable
                 EXIT;
              ELSE
                 RAISE_APPLICATION_ERROR(-20001,'I, a computer, have no idea whats going on here');
              END IF;

            WHEN OTHERS THEN
               RAISE;
         END;

         --Check that this next edge_id isnt already in our list
         --We need to check more than just whether its the root (alledgez(1))
         --because a chain could link back on itself at a point other than the root
         --   if shaped like, for ex, a "P"

         psql := 'SELECT count(*) FROM '
              || 'TABLE(:p1) '
              || 'WHERE column_value = :p2 ';

         EXECUTE IMMEDIATE psql INTO kount USING
                                           GZ_UTILITIES.STRINGARRAY_TO_VARRAY(alledgez),
                                           edge_id;

         IF kount = 1
         AND edge_id != alledgez(1)
         THEN
            EXECUTE IMMEDIATE altersession;
            RETURN 'N edge_id:' || edge_id;
         ELSIF kount = 1
         AND edge_id = alledgez(1)
         THEN
            -- We looped a loop, this is good
            -- Exit the current loop and get a new edge
            EXIT;
         ELSE
            --We are just partway thru a chain
            NULL;
         END IF;

         --Now we return to the start of the loop, and we are again on end2start

      END LOOP;



      --We've closed a physical loop
      --See if there are any other edges out there

      --add our loops worth of edges to the total stash
      multiloopedgez := GZ_UTILITIES.stringarray_add(multiloopedgez,alledgez);
      alledgez.DELETE;

      --Attempt to get another edge id from the bucket
      --That is at a start-start faceoff

      psql := ouroboros --see beginning of fn
           || ' AND y.edge_id NOT IN (SELECT * FROM TABLE(:p1)) ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO edge_id
                                USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(multiloopedgez);

         EXCEPTION
            WHEN NO_DATA_FOUND
            THEN

               --no start-starts left, check for an ouroboros
               psql := 'SELECT edge_id '
                    || 'FROM  (' || subquery || ') a '
                    || 'WHERE '
                    || 'a.edge_id NOT IN (SELECT * FROM TABLE(:p1)) '
                    || 'AND rownum = 1 ';

               BEGIN

                  --Are nested BEGINS with exceptions considered sloppy?

                  EXECUTE IMMEDIATE psql INTO edge_id
                                         USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(multiloopedgez);

                  EXCEPTION
                     WHEN NO_DATA_FOUND
                     THEN
                        --Goody,we are done
                        EXECUTE IMMEDIATE altersession;
                        RETURN 'Y';
                     WHEN OTHERS
                     THEN
                        RAISE;
               END;


            WHEN OTHERS
            THEN
               RAISE;
      END;


      --We now have an edge_id that we believe exists on a loop or chain
      --   totally independent from the loop(s) we just closed

      --Keeping it real.  Second grade real
      GOTO logoturtle;

      --If we took computer science past second grade I guess
      --we would return the array of edges to a wrapping caller


   END CLOSED_LOOPS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION COUNT_OBSOLETE_NODES (
      p_topology       IN VARCHAR2
   ) RETURN NUMBER AS

   /*

   Matt! 9/12/12 Pulled the logic here into GET_OBSOLETE_NODES (see below)
                 Changed COUNT_OBSOLETE_NODES to call GET_OBSOLETE_NODES and flip just the count

   Purpose:  Return the number of obsolete nodes in a topology based on NODE$ and EDGE$
   only.

   Author: Stephanie

   Date: 20100426

   Arguments:
   p_topology - the name of a topology in the current schema, or a schema
       name followed by a topology name whose primitive tables you have select
       access to.
       examples...
          MYTOPO
          MT
          ACS09GM.MT

   Explanation:
       Loops through each node in NODE$ and checks to see how many
       different lines are connected to it.  Single connections are okay in the
       case of islands, nodes with three connections are required for the
       topology, nodes with two and only two connections are counted.

       This function does not take feature layers into account, only
       node$ and edge$.  An obsolete node may be required to support a
       feature layer (say a road name change for example).  This function was
       written to support the generalized boundary files where all two-connected
       nodes are obsolete.

   */


    v_nodearray      GZ_TYPES.NUMBERARRAY;

   BEGIN

      v_nodearray := GZ_UTILITIES.GET_OBSOLETE_NODES(p_topology);

      dbms_output.put_line('*******************************************');
      dbms_output.put_line('I found '||v_nodearray.COUNT|| ' obsolete nodes!');
      dbms_output.put_line('*******************************************');

     RETURN v_nodearray.COUNT;

   END COUNT_OBSOLETE_NODES;

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GET_OBSOLETE_NODES(
      p_topology       IN VARCHAR2
   ) RETURN GZ_TYPES.NUMBERARRAY
   AS

   --Matt! 9/12/12
   --Stole and modified the guts of Stephanie's count_obsolete_nodes (above) and put it in here
   --Made original count_obsolete_nodes call this guy so no code duplication
   --Returns the node ids of obsolete nodes in a topology

   --Loops through each node in NODE$ and checks to see how many
   --different lines are connected to it.  Single connections are okay in the
   --case of islands, nodes with three connections are required for the
   --topology, nodes with two and only two connections are counted.

   --This function does not take feature layers into account, only
   --node$ and edge$.  An obsolete node may be required to support a
   --feature layer (say a road name change for example).  This function was
   --written to support the generalized boundary files where all two-connected
   --nodes are obsolete.


   vsql           VARCHAR2(4000);
   v_total        NUMBER;
   v_islands      NUMBER;
   v_obs_count    NUMBER := 0;
   v_nodearray    GZ_TYPES.numberarray;
   output         GZ_TYPES.numberarray;

   BEGIN

      vsql := 'SELECT node_id from '||p_topology||'_node$ order by node_id';
      EXECUTE IMMEDIATE vsql BULK COLLECT INTO v_nodearray;

      FOR i IN 1..v_nodearray.COUNT
      LOOP

          vsql := 'select count(*) from '||p_topology||'_edge$ a '||
                  'where '|| v_nodearray(i)
                ||'in (a.start_node_id,a.end_node_id)';

          EXECUTE IMMEDIATE vsql INTO v_total;

          --dbms_output.put_line('NODE = '||v_nodearray(i));
          --dbms_output.put_line('  Total edges = '||v_total);

          vsql := 'select count(*) from '||p_topology||'_edge$ a '||
                  'where a.start_node_id  = :p1 '||
                  'AND a.end_node_id = :p2';

          EXECUTE IMMEDIATE vsql INTO v_islands USING v_nodearray(i),
                                                      v_nodearray(i);

          --dbms_output.put_line('  Total islands = '||v_islands);

          --IF v_total = 1 AND v_islands = 1 THEN
             --dbms_output.put_line('  This node is on an isolated island = OK');
          --END IF;

         IF v_total = 2
         AND v_islands = 0
         THEN

            --dbms_output.put_line('  Node '||v_nodearray(i)||' is OBSOLETE!');

            v_obs_count := v_obs_count + 1;

            output(v_obs_count) := v_nodearray(i);

         END IF;

      END LOOP;


     RETURN output;

   END GET_OBSOLETE_NODES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

PROCEDURE UPDATE_FACE_MEASUREMENTS(
   p_table      IN VARCHAR2,
   p_join_key   IN VARCHAR2 DEFAULT 'FACE_ID',
   p_list       IN VARCHAR2 DEFAULT NULL

) AS
/*
This program updates the following fields in a topological polygon feature
table based on the current topo geometry.

   SDOGEOMETRY
   MBR
   AREATOTAL
   PERIMETER
   LLX
   LLY
   URX
   URY

Prerequisites
   1) The fields listed above must already exist on the feature table you want
      to update.

   2) The table you want to update must be a polygon table

Parameters
   1) p_table (REQUIRED) - the name of the table to update

   2) p_join_key (OPTIONAL) - A field on p_table that contains a unique
                              identifier for each polygon (usually face_id).
                              This is also the column used when joining the
                              p_table with the p_list.
                              DEFAULTS to FACE_ID.

   3) p_list  (OPTIONAL) - the name of another table which stores a list
                           of polygon identifiers to update.  This table must
                           have a field called the same as p_join_key.
                           This is typically
                           just a single field table with a column called
                           "FACE_ID" that includes a record for each face
                           in p_table that requires updated measurements.
                           DEFAULTS to NULL, and processes all records in
                           p_table.

*/

sql_stmt VARCHAR2(4000);
v_list VARCHAR2(32) := p_list;

BEGIN

 -- Unless the user has a specific list of faces to update in a separate
 -- table, update all faces in the input table.

 IF v_list IS NULL THEN
    v_list := p_table;
 END IF;

 -- update SDO Geometry

 sql_stmt := 'UPDATE /*+ PARALLEL 4 */ ' || p_table ||
             ' a set a.sdogeometry = a.topogeom.get_geometry() '||
             ' where '||p_join_key||
             ' IN (select '||p_join_key||' from '||v_list||')';
 EXECUTE immediate sql_stmt;
 COMMIT;

 -- update MBRs

 sql_stmt := 'UPDATE /*+ PARALLEL 4 */  ' || p_table ||
             ' set MBR = SDO_GEOM.SDO_MBR(SDOGEOMETRY)'||
             ' where '||p_join_key||
             ' IN (select '||p_join_key||' from '||v_list||')';
 EXECUTE IMMEDIATE sql_stmt;
 COMMIT;

 -- Update Perimeter, Area, and extract MBR ordinates
 sql_stmt := 'UPDATE /*+ PARALLEL 4 */  ' || p_table
             || ' set areatotal = sdo_geom.sdo_area(sdogeometry,0.05,''unit=sq_meter''),'
             ||'PERIMETER = SDO_GEOM.SDO_LENGTH(SDOGEOMETRY,0.05,''unit=meter''),'
             ||'LLX = SDO_GEOM.SDO_MIN_MBR_ORDINATE(MBR,1),'
             ||'LLY = SDO_GEOM.SDO_MIN_MBR_ORDINATE(MBR,2),'
             ||'URX = SDO_GEOM.SDO_MAX_MBR_ORDINATE(MBR,1),'
             ||'URY = SDO_GEOM.SDO_MAX_MBR_ORDINATE(MBR,2) '
             ||'where '||p_join_key||
             ' IN (select '||p_join_key||' from '||v_list||')';

 EXECUTE IMMEDIATE sql_stmt;
 COMMIT;

 -- update perimeter/area ratio

 sql_stmt := 'UPDATE /*+ PARALLEL 4 */ ' || p_table
             ||' set PA_RATIO = PERIMETER/SQRT(AREATOTAL)'
             ||'where '||p_join_key||
             ' IN (select '||p_join_key||' from '||v_list||')';

 EXECUTE IMMEDIATE sql_stmt;
 COMMIT;

END UPDATE_FACE_MEASUREMENTS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION SHOW_ME_THE_VERTICES (
      p_geom            IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 5/21/10

      --This is just a helper, not actually called by anything in production
      --I want to see every x,y in mapviewer that makes up my geometry
      --Ex
      --select GZ_UTILITIES.show_me_the_vertices(e.geometry)
      --       FROM statefp02_edge$ e
      --       WHERE e.edge_id = 1

      output            SDO_GEOMETRY;
      out_ordinates     SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();

   BEGIN

      --Lets be complete simpletons
      --Make a multipoint (dont tell) and put the ordinates in it

      output := SDO_GEOMETRY(2005,
                             p_geom.sdo_srid,
                             NULL,
                             SDO_ELEM_INFO_ARRAY(1,1,p_geom.sdo_ordinates.COUNT/2),
                             NULL);


      out_ordinates.EXTEND(p_geom.sdo_ordinates.COUNT);

      FOR i in 1 .. p_geom.sdo_ordinates.COUNT
      LOOP

         out_ordinates(i) := p_geom.sdo_ordinates(i);

      END LOOP;

      output.sdo_ordinates := out_ordinates;

      RETURN output;


   END SHOW_ME_THE_VERTICES;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION SHOW_ME_THE_FACE_DUPES (
      p_geom            IN SDO_GEOMETRY,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 2/11/11
      --sick this on a face that you know has duplicate vertices
      --it will return just the segment bounded by the dupes

      --sample:
      -- select GZ_UTILITIES.SHOW_ME_THE_FACE_DUPES(a.sdogeometry)
      -- from Z920LS_clip_face a
      -- where a.face_id = 152

      output            SDO_GEOMETRY;
      first_carrot      NUMBER;
      second_carrot     NUMBER;
      vertex_loc        NUMBER;
      validate_msg      VARCHAR2(4000);
      my_segment        SDO_GEOMETRY;

   BEGIN

      IF p_geom.sdo_gtype != 2003
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Sorry bubkins, this FN is face friendly only.  Got gtype ' || p_geom.sdo_gtype);
      END IF;

      --13356 [Element <1>] [Coordinate <220>][Ring <1>]
      validate_msg := SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(p_geom,
                                                              p_tolerance);

      IF validate_msg NOT LIKE '%13356%'
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bubkins, this FN expects duplicate vertices, got ' || validate_msg);

      END IF;

      --Where is my fracking validate parser?
      --location of second <
      first_carrot := regexp_instr(validate_msg, '<',1,2);
      --location of second >
      second_carrot := regexp_instr(validate_msg, '>',1,2);
      vertex_loc := substr(validate_msg, (first_carrot + 1), (second_carrot - first_carrot - 1));

      my_segment := GZ_CLIP.GET_XYS(p_geom,1,vertex_loc);

      IF my_segment.SDO_ORDINATES.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Something went wrong, got ' || my_segment.SDO_ORDINATES.COUNT || ' ordinates');

      END IF;

      --Evil 2004 in the house.  May require an exorcism
      --can get away with this because I know exactly what the segment looks like
      --and this is just a helper fn
      output := SDO_GEOMETRY(2004,
                             my_segment.sdo_srid,
                             NULL,
                             SDO_ELEM_INFO_ARRAY(1,1,1, 3,1,1, 7,2,1),
                             SDO_ORDINATE_ARRAY(my_segment.sdo_ordinates(1), my_segment.sdo_ordinates(2),
                                                my_segment.sdo_ordinates(3), my_segment.sdo_ordinates(4),
                                                my_segment.sdo_ordinates(1),
                                                my_segment.sdo_ordinates(2),
                                                my_segment.sdo_ordinates(3),
                                                my_segment.sdo_ordinates(4)
                                                )
                             );

      RETURN output;


   END SHOW_ME_THE_FACE_DUPES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION SHOW_ME_EDGE_INTERSECTION (
      p_geom            IN SDO_GEOMETRY,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 7/12/12

      output            SDO_GEOMETRY;
      psql              VARCHAR2(4000);
      xes               GZ_TYPES.stringarray;
      yes               GZ_TYPES.stringarray;
      kounts            GZ_TYPES.stringarray;
      this_pt           SDO_GEOMETRY;

   BEGIN

      IF p_geom.sdo_gtype != 2002
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Sorry bubkins, this FN is edge friendly only.  Got gtype ' || p_geom.sdo_gtype);
      END IF;

      psql := 'SELECT xx, yy, kount FROM ( '
           || 'SELECT t.x xx, t.y yy, COUNT(*) kount '
           || 'FROM '
           || 'TABLE(SDO_UTIL.GETVERTICES((select sdo_geom.sdo_intersection(:p1, :p2, :p3) FROM DUAL '
           || '))) t '
           || 'GROUP BY t.x, t.y '
           || ') WHERE kount > 1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO xes, yes, kounts USING p_geom,
                                                                      p_geom,
                                                                      p_tolerance;

      IF xes.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'No intersection at ' || p_tolerance);

      END IF;

      --Evil 2004 in the house.  May require an exorcism
      --and this is just a helper fn

      output := p_geom;

      FOR i IN 1 .. xes.COUNT
      LOOP

         this_pt := SDO_GEOMETRY(2001,
                                 p_geom.sdo_srid,
                                 SDO_POINT_TYPE(xes(i),
                                                yes(i),
                                                NULL),
                                 NULL,NULL);

         output := SDO_UTIL.APPEND(output, this_pt);


      END LOOP;

      RETURN output;


   END SHOW_ME_EDGE_INTERSECTION;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION DROP_UPDATABLE_TOPOMAPS(
     p_current_topomap    IN VARCHAR2,
     p_which              IN VARCHAR2 DEFAULT 'UPDATABLE'
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt!  5/25/10
      --Called from GZ_UTILITIES.build_topo_from_spatial as well as from
      --    several spots in GZ_CLIP
      --Drops all topo maps associated with the current session or just updatable ones
      --   so we can get on with our freakin lives
      --Returns the names of the dropped topo maps in case it matters (currently does not to me)
      --added p_which and p_current_topomap parameters 5/28/10
      --fixed logic for ALL parameter 6/18/10

      --N.B.!!!
      --Though the error that makes one want to call this function occurs on load topomap
      --One must take a step back and recreate the topomap too after calling this guy
      --SDO_TOPO_MAP.CREATE_TOPO_MAP(toponame, newtopomap);
      --SDO_TOPO_MAP.LOAD_TOPO_MAP(newtopomap, 'TRUE','TRUE');

      --SAMPLE USAGE
      --Im working on a topomap called STATEFP25_TOPOMAP and just found out that
      --Some bomb left another (or several) topomap(s) out in the ether

      --DECLARE
      --topomaps GZ_TYPES.stringarray;
      --BEGIN
      --topomaps := GZ_UTILITIES.DROP_UPDATABLE_TOPOMAPS('STATEFP25_TOPOMAP','ALL');
      --END;

      topomaps          VARCHAR2(4000);
      topostring        VARCHAR2(4000);
      current_trifecta  GZ_TYPES.stringarray;
      output            GZ_TYPES.stringarray;
      kount             PLS_INTEGER := 1;
      kount2            PLS_INTEGER := 1;


   BEGIN

      topomaps := SDO_TOPO_MAP.LIST_TOPO_MAPS();

      --If nothing, get out
      --probably an error, but seems like a diff scope
      IF LENGTH(topomaps) = 0
      THEN
         RETURN output;
      END IF;

      --Annoyingly, looks like this
      --(STATEFP25_TOPOMAP, STATEFP25, read-only), (STATEFP24_TOPOMAP, STATEFP24, updatable)

      --Parens must be escaped in regexps
      --Find me the first occurrence of a ( followed by non () text, then a )
      topostring := regexp_substr(topomaps,'\([^\)^\(]+\)',1,1);

      WHILE topostring IS NOT NULL
      LOOP

         --now like: (STATEFP25_TOPOMAP, STATEFP25, read-only)
         --replace parens
         topostring := regexp_replace(topostring,'\(','');
         topostring := regexp_replace(topostring,'\)','');

         --now like STATEFP25_TOPOMAP, STATEFP25, read-only
         current_trifecta := GZ_UTILITIES.split(topostring,',');


         IF p_which = 'UPDATABLE'
         AND UPPER(current_trifecta(3)) LIKE '%UPDATABLE%'
         AND p_current_topomap != current_trifecta(1)
         THEN

               output(kount2) := current_trifecta(1);
               SDO_TOPO_MAP.DROP_TOPO_MAP(current_trifecta(1));

               kount2 := kount2 + 1;

         ELSIF p_which = 'ALL'
         THEN

               output(kount2) := current_trifecta(1);
               SDO_TOPO_MAP.DROP_TOPO_MAP(current_trifecta(1));

               kount2 := kount2 + 1;

         END IF;

         kount := kount + 1;
         topostring := regexp_substr(topomaps,'\([^\)^\(]+\)',1,kount);

         IF kount > 1000
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Yo, we''ve just attempted to drop 1000 topo maps, thats topowack. Here they are: ' || topomaps);

         END IF;

      END LOOP;


      IF output.COUNT > 0
      THEN

         RETURN output;

      ELSE

         --This error assumes we did not call this guy without reason
         --We think there is an updatable topomap somewhere out there
         RAISE_APPLICATION_ERROR(-20001,'Inexplicably, failed to drop any of these topo maps: ' || topomaps);

      END IF;


   END DROP_UPDATABLE_TOPOMAPS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION EZ_TOPOMAP_MANAGER (
      p_topomap_name       IN VARCHAR2,
      p_topology_name      IN VARCHAR2,
      p_create_it          IN NUMBER,
      p_xLL                IN NUMBER DEFAULT NULL,
      p_yLL                IN NUMBER DEFAULT NULL,
      p_xUR                IN NUMBER DEFAULT NULL,
      p_yUR                IN NUMBER DEFAULT NULL,
      p_delta              IN NUMBER DEFAULT 0.0001,
      p_memory_size        IN NUMBER DEFAULT NULL
   ) RETURN NUMBER
   AS

      --Matt! 6/23/10
      --Matt! 3/17/11 Added memory management

      --p_create_it
      -- 1: Create (only) topomap
      -- 2: Create and load, if any topomaps already exist nuke them
      -- 3: Commit and drop

      --Sample usage
      -- create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(newtopo,topomap,2);
      --Then do some work, occasionally call SDO_TOPO_MAP.UPDATE_TOPO_MAP etc
      -- create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(newtopo,topomap,create_it);

      topomap_name      VARCHAR2(4000);
      topology_name     VARCHAR2(4000);
      topomaps          GZ_TYPES.stringarray;
      pXMin             NUMBER;
      pYMin             NUMBER;
      pXMax             NUMBER;
      pYMax             NUMBER;
      create_it         NUMBER;
      raise_the_roof    NUMBER;


   BEGIN

      IF p_memory_size IS NULL
      THEN
         raise_the_roof := 2147483648;
      ELSE
         raise_the_roof := p_memory_size;
      END IF;


      --Just in case
      topomap_name := UPPER(p_topomap_name);
      topology_name := UPPER(p_topology_name);

      IF p_create_it NOT IN (1,2,3)
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Sorry pal, p_create_it must be 1 2 or 3 and you chucked in ' || p_create_it);
      END IF;

      IF p_xLL IS NOT NULL
      AND p_yLL IS NOT NULL
      AND p_xUR IS NOT NULL
      AND p_yUR IS NOT NULL
      THEN

         --Set up window
         pXmin := p_xLL - p_delta;
         pYmin := p_yLL - p_delta;
         pXMax := p_xUR + p_delta;
         pYmax := p_yUR + p_delta;

      ELSIF p_xLL IS NOT NULL
      AND (p_yLL IS NULL OR
           p_xUR IS NULL OR
           p_yUR IS NULL)
      THEN

         RAISE_APPLICATION_ERROR(-20001,'We have some but not all of the bounding box coordinates');

      ELSE

         --Full topology topomap load
         NULL;

      END IF;


      IF p_create_it IN (1,2)
      THEN

         BEGIN

            SDO_TOPO_MAP.CREATE_TOPO_MAP(topology_name,topomap_name);
            create_it := 2;

            IF p_create_it = 2
            THEN

               IF p_xLL IS NULL
               THEN

                  SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, 'TRUE','TRUE');
                  create_it := 3;

               ELSE

                  SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, pXmin, pYmin, pXMax, pYmax, 'TRUE');
                  create_it := 3;

               END IF;

            END IF;

         EXCEPTION
         WHEN OTHERS THEN

            IF UPPER(SQLERRM) LIKE UPPER('%an updatable TopoMap object already exists%') OR
            UPPER(SQLERRM) LIKE UPPER('%a TopoMap by the same name already exists%')
            THEN

               --ex:
               --ORA-29532: Java call terminated by uncaught Java exception:
               --oracle.spatial.topo.TopoDataException:
               --an updatable TopoMap object already exists: STATEFP24_TOPOMAP

               topomaps := GZ_UTILITIES.DROP_UPDATABLE_TOPOMAPS(topomap_name,'ALL');

               SDO_TOPO_MAP.CREATE_TOPO_MAP(topology_name,topomap_name);
               create_it := 2;

               IF p_create_it = 2
               THEN

                  IF p_Xll IS NULL
                  THEN
                     SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, 'TRUE','TRUE');
                  ELSE
                     SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, pXmin, pYmin, pXMax, pYmax, 'TRUE');
                  END IF;

                  create_it := 3;

                  RETURN create_it;

               END IF;

            ELSIF UPPER(SQLERRM) LIKE '%OUTOFMEMORYERROR%'
            THEN

               --Sidey voodoo first
               dbms_session.free_unused_user_memory;

               SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(raise_the_roof);

               IF p_create_it = 2
               THEN

                  IF p_xLL IS NULL
                  THEN

                     SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, 'TRUE','TRUE');
                     create_it := 3;

                  ELSE

                     SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, pXmin, pYmin, pXMax, pYmax, 'TRUE');
                     create_it := 3;

                  END IF;

               END IF;

            ELSE

               ---------------------------------------------------------
               --More errors as we learn and wish to handle them go here
               ---------------------------------------------------------

               RAISE;

            END IF;

         END;


         RETURN create_it;

      END IF;


      IF p_create_it = 3
      THEN

         BEGIN

            SDO_TOPO_MAP.COMMIT_TOPO_MAP();

         EXCEPTION
         WHEN OTHERS THEN

            --ORA-29532: Java call terminated by uncaught Java exception:
            --oracle.spatial.topo.TopoDataException: no updatable TopoMap object has been created
            IF SQLCODE = -29532
            AND UPPER(SQLERRM) LIKE UPPER('%no updatable TopoMap object has been created%')
            THEN

               create_it := 1;
               RETURN create_it;

            ELSE

               ---------------------------------------------------------
               --More errors as we learn and wish to handle them go here
               ---------------------------------------------------------
               --here
               RAISE;

            END IF;

         END;


         BEGIN

            SDO_TOPO_MAP.DROP_TOPO_MAP(p_topomap_name);

         EXCEPTION
         WHEN OTHERS THEN

            --ORA-29532: Java call terminated by uncaught Java exception:
            --oracle.spatial.topo.TopoDataException: the specified TopoMap does not exist
            IF SQLCODE = -29532
            AND UPPER(SQLERRM) LIKE UPPER('%the specified TopoMap does not exist%')
            THEN

               create_it := 1;
               RETURN create_it;

            ELSE

               ---------------------------------------------------------
               --More errors as we learn and wish to handle them go here
               ---------------------------------------------------------
               RAISE;

            END IF;

         END;

         create_it := 1;
         RETURN create_it;

      END IF;


   END EZ_TOPOMAP_MANAGER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

PROCEDURE remove_obs_nodes_one_state (
topo_name VARCHAR2,
state_table VARCHAR2,
v_state VARCHAR2
)
AS
   --Matt! 9/12/12 Making SP not call this any more
   --      Not sure if anything calls it

---
-- Does not remove nodes where they are the start and end of a single edge.
--
v_topo_name VARCHAR2(4000) := UPPER(topo_name);
v_topo_map VARCHAR2(4000) := v_topo_name ||'_TOPOMAP';
v_ez_topo_mgr NUMBER := 0;

BEGIN
--------------------------------------------------------------------------------
-- 3/27/2012 - We will need to change the parameters and call in GZ_SMPOLY
-- all we really need now is the topology name.
-- At some point we should add an optional window.
-- We should change the whole procedure to call it - 'remove obsolete nodes from
-- whole topology' or something similarly descriptive

-- No logging is needed, the caller will check obs node counts and log as
-- needed.  If there are any obsolete nodes left, then that means the
-- procedure did not work.

    -- call ez_topomap_manager to open a new topomap
    -- for the whole topology, (CODE 2 to EZ_TOPOMAP_MANAGER).
    --(defaults to a buffer of 0.0001)

    v_ez_topo_mgr := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(v_topo_map,v_topo_name,2);

    -- edit the topo map - remove obsolete nodes (pseudo nodes)...

    SDO_TOPO_MAP.REMOVE_OBSOLETE_NODES(NULL);

    -- commit the topomap and drop it  (CODE 3 to EZ_TOPOMAP_MANAGER).

    v_ez_topo_mgr := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(v_topo_map,v_topo_name,3);

END remove_obs_nodes_one_state ;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GZ_REMOVE_OBSOLETE_NODES (
      p_topo          IN VARCHAR2
   ) RETURN NUMBER
   AS

      --Matt! 9/12/12
      --Update! 10/26/12 to deal with face not found in cache error
      --SP module having java memory blowouts calling remove_obs_nodes_one_state above
      --Gonna try calling with node by node topomap, see how the performance goes
      --I have only done cursory performance checks.  In general, would expect this guy to be
      --   slower for big topos with lots of obsolete nodes.  For topos with few obsolete nodes
      --   this approach may be faster since lots of untouched territory doesn't have to enter cache

      --Should return 0, for 0 obsolete nodes
      --If the number is greater than 0 the caller should decide what to do

      nodez          GZ_TYPES.numberarray;
      create_it      NUMBER;
      node_geomz     GZ_TYPES.geomarray;
      psql           VARCHAR2(4000);
      my_cursor      SYS_REFCURSOR;
      output         NUMBER;

   BEGIN

      nodez := GZ_UTILITIES.GET_OBSOLETE_NODES(p_topo);

      psql := 'SELECT n.geometry '
           || 'FROM '
           || p_topo || '_node$ n '
           || 'WHERE '
           || 'n.node_id IN (SELECT * FROM TABLE(:p1)) ';

      OPEN my_cursor FOR psql USING GZ_UTILITIES.numarray_to_varray(nodez);

      LOOP

         FETCH my_cursor BULK COLLECT INTO node_geomz LIMIT 1000;
         EXIT WHEN node_geomz.COUNT = 0;

         FOR i IN 1 .. node_geomz.COUNT
         LOOP

            --dbms_output.put_line('NODE ' || gz_utilities.dump_sdo(node_geomz(i)));

            create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || '_MAP',
                                                         p_topo,
                                                         2,
                                                         node_geomz(i).sdo_point.X,
                                                         node_geomz(i).sdo_point.Y,
                                                         node_geomz(i).sdo_point.X, --allow the delta to form the window
                                                         node_geomz(i).sdo_point.Y);

            --may need an exception handler here
            --Yep. Put some voodoo on it

            BEGIN

               SDO_TOPO_MAP.REMOVE_OBSOLETE_NODES(NULL);

            EXCEPTION
            WHEN OTHERS
            THEN

               IF SQLERRM LIKE '%not found in cache%'
               THEN

                  --Java call terminated by uncaught Java exception: oracle.spatial.topo.TopoEntityNotFoundException:
                  --Face ID 67 not found in cache
                  --The usual topomap error.  Think this is a bug but haven't done SR (yet 10/26/12)
                  --Stretch the topomap a little and we have the funkiest horn section in the metropolis again

                  create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || '_MAP',
                                                               p_topo,
                                                               2,
                                                               node_geomz(i).sdo_point.X,
                                                               node_geomz(i).sdo_point.Y,
                                                               node_geomz(i).sdo_point.X, --allow the delta to form the window
                                                               node_geomz(i).sdo_point.Y,
                                                               .0004);  --quadruple delta!

                  SDO_TOPO_MAP.REMOVE_OBSOLETE_NODES(NULL);


               ELSE

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

               END IF;

            END;


            create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || '_MAP',
                                                         p_topo,
                                                         3);

         END LOOP;

      END LOOP;

      output := GZ_UTILITIES.COUNT_OBSOLETE_NODES(p_topo);

      RETURN output;

   END GZ_REMOVE_OBSOLETE_NODES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

PROCEDURE remove_isolated_nodes
(p_topology VARCHAR2)
AS

TYPE mynumbers IS TABLE of NUMBER
   INDEX BY PLS_INTEGER;

v_node_list mynumbers;

v_sql VARCHAR2(4000);

BEGIN

v_sql := 'SELECT node_id FROM '||p_topology||'_node$ where edge_id = 0 or edge_id IS NULL';

EXECUTE IMMEDIATE v_sql BULK COLLECT INTO v_node_list;

FOR i IN 1..v_node_list.count LOOP

    SDO_TOPO_MAP.REMOVE_NODE(p_topology,v_node_list(i));

    commit;

END LOOP;

END remove_isolated_nodes;
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

      RETURN GZ_UTILITIES.REMOVE_HOLES(sdogeometry,
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
   -----------------------------------------------------------------------------------------

   PROCEDURE DEREGISTER_FEATURE_TABLES (
      p_schema             IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_drop_tabs          IN VARCHAR2 DEFAULT 'Y',
      p_min_tg_layer_level IN NUMBER DEFAULT 0,
      p_likeclause         IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 10/28/10
      --Deregisters all (or some) feature tables registered with a topology
      --Deletes the deregistered tables if requested
      --Leaves topology and primitives intact

      --Sample, deregister and drop all
      -- GZ_UTILITIES.DEREGISTER_FEATURE_TABLES('GZCPB1','DEC09');

      --Deregister and drop all greater than 0-level feature tabs
      -- GZ_UTILITIES.DEREGISTER_FEATURE_TABLES('GZCPB1','DEC09','Y',1);

      --Deregister FSL tables, but keep them
      -- GZ_UTILITIES.DEREGISTER_FEATURE_TABLES('GZCPB1','DEC09','N',0,'FSL%');



      tabs                 GZ_TYPES.stringarray;
      cols                 GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);
      tabstash             GZ_CLIP.stringhash;
      topo                 VARCHAR2(32) := UPPER(p_topo);
      pschema              VARCHAR2(32) := UPPER(p_schema);

   BEGIN

      psql := 'SELECT a.table_name, a.column_name '
           || 'FROM '
           || 'user_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.owner = :p2 AND '
           || 'a.tg_layer_level >= :p3 AND '
           || 'a.table_name IS NOT NULL ';

      IF p_likeclause IS NULL
      THEN

         psql := psql || 'ORDER BY a.tg_layer_id DESC ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs, cols USING topo,
                                                                   pschema,
                                                                   p_min_tg_layer_level;

      ELSIF p_likeclause IS NOT NULL
      THEN

         psql := psql || 'AND a.table_name LIKE :p4 '
                      || 'ORDER BY a.tg_layer_id DESC ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs, cols USING topo,
                                                                   pschema,
                                                                   p_min_tg_layer_level,
                                                                   p_likeclause;

      END IF;

      IF tabs.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Couldnt find any tables to deregister for topo ' || topo);

      END IF;


      FOR i in 1 .. tabs.COUNT
      LOOP

         BEGIN

            SDO_TOPO.DELETE_TOPO_GEOMETRY_LAYER(topo,tabs(i),cols(i));

         EXCEPTION
         WHEN OTHERS THEN

            --Add handled errors here

            RAISE;

         END;

      END LOOP;

      IF UPPER(p_drop_tabs) = 'Y'
      THEN

         FOR i in 1 .. tabs.COUNT
         LOOP

            IF NOT tabstash.EXISTS(tabs(i))
            THEN

               tabstash(tabs(i)) := '1';
               psql := 'DROP TABLE ' || tabs(i) || ' PURGE ';

               BEGIN

                  EXECUTE IMMEDIATE psql;

               EXCEPTION
               WHEN OTHERS THEN

                  --Add handled errors here
                  RAISE;

               END;

            END IF;

         END LOOP;

      END IF;

   END DEREGISTER_FEATURE_TABLES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE REFRESH_REMOTE_PROD_SCHEMA (
      p_prod_schema        IN VARCHAR2,
      p_prod_db            IN VARCHAR2,
      p_prod_pwd           IN VARCHAR2,
      p_likeclause         IN VARCHAR2 DEFAULT 'REF%'
   )
   AS

      --Matt! 11/01/10
      --Cheap little utility to take all the tables in GZDEC10ST99
      --  (or wherever the production schema parameter tables live) and
      -- mirror them on Devbench (or wherever)

      --As of 8/1/11 we no longer have privvies to create db links
      --Need to revise this guy to expect an input database link name, pre-created

      --Usage Sample. Logged into GZCPB1@Devbench
      --BEGIN
      --GZ_UTILITIES.REFRESH_REMOTE_PROD_SCHEMA('GZDEC10ST99','PRODBNCH','XXXXX');
      --END;

      prod_schema    VARCHAR2(32) := UPPER(p_prod_schema);
      prod_db        VARCHAR2(32) := UPPER(p_prod_db);
      prod_pwd       VARCHAR2(32) := p_prod_pwd;
      tabs           GZ_TYPES.stringarray;
      psql           VARCHAR2(4000);

   BEGIN

      --create DB link

      psql := 'CREATE DATABASE LINK ' || prod_schema || ' CONNECT TO ' || prod_schema
           || ' IDENTIFIED BY ' || prod_pwd || ' USING ''' || prod_db || '''';

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS THEN

         IF SQLCODE = -02011
         THEN
            --ORA-02011: duplicate database link name
            NULL;
         ELSE
            RAISE;
         END IF;

      END;


      --get all tables
      psql := 'SELECT table_name FROM '
           || 'USER_TABLES@' || prod_schema || ' '
           || 'WHERE table_name LIKE :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING p_likeclause;

      FOR i IN 1 .. tabs.COUNT
      LOOP

         psql := 'CREATE TABLE ' || tabs(i) || ' AS '
              || 'SELECT * FROM ' || prod_schema || '.' || tabs(i) || '@' || prod_schema || ' ';

         BEGIN

            EXECUTE IMMEDIATE psql;

         EXCEPTION

            WHEN OTHERS THEN

               IF SQLCODE = -00955
               THEN

                  --ORA-00955: name is already used by an existing object
                  EXECUTE IMMEDIATE 'DROP TABLE ' || tabs(i) || ' PURGE ';

                  EXECUTE IMMEDIATE psql;

               ELSE

                  RAISE;

               END IF;

         END;

         EXECUTE IMMEDIATE 'GRANT SELECT ON ' || tabs(i) || ' TO "PUBLIC" ';

      END LOOP;


      --clean up
      EXECUTE IMMEDIATE 'ALTER SESSION CLOSE DATABASE LINK ' || prod_schema;

      psql := 'DROP DATABASE LINK ' || prod_schema || ' ';
      EXECUTE IMMEDIATE psql;


   END REFRESH_REMOTE_PROD_SCHEMA;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION GET_REF_SCHEMAS (
      p_schema             IN VARCHAR2,
      p_ref_schema_tab     IN VARCHAR2 DEFAULT 'REFERENCE_SCHEMAS'
   ) RETURN GZ_TYPES.stringhash
   AS

      --Matt! 11/4/10
      --Dont really like selecting from a table, but it is what it is

      psql              VARCHAR2(4000);
      retval            GZ_TYPES.stringhash;
      schemas           GZ_TYPES.stringarray;
      privvies          GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT UPPER(a.schema_name), UPPER(a.grants) '
           || 'FROM '
           || p_schema || '.' || p_ref_schema_tab || ' a '
           || 'WHERE a.schema_name IN '
           || '(SELECT username FROM ALL_USERS '   --only get the reals
           || ' UNION ALL '
           || ' SELECT ''PUBLIC'' FROM DUAL) ';  --hard code in public.
                                                 --Not sure where to see roles except DBA roles and not all users can see

      BEGIN

         EXECUTE IMMEDIATE psql BULK COLLECT INTO schemas, privvies;

      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Error. No records found in ' || p_ref_schema_tab);
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Table ' || p_ref_schema_tab || ' does not exist');
            ELSE
               RAISE;
            END IF;
      END;

      FOR i IN 1 .. schemas.COUNT
      LOOP

         retval(schemas(i)) := privvies(i);

      END LOOP;


      RETURN retval;

   END GET_REF_SCHEMAS;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GET_REFERENCE_FACE_FIELDS (
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_type           IN VARCHAR2 DEFAULT 'ATTRIBUTE', --or MEASUREMENT
      p_ref_table      IN VARCHAR2 DEFAULT 'REFERENCE_FACE_FIELDS'
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 11/5/10
      --! 9/30/11 moved to GZ_UTILITIES and added project_id awareness

      psql              VARCHAR2(4000);
      output            GZ_TYPES.stringarray;

   BEGIN


      psql := 'SELECT a.field '
           || 'FROM ' || p_ref_table || ' a '
           || 'WHERE '
           || 'UPPER(a.field_use) = :p1 AND '
           || 'UPPER(a.gen_project_id) = :p2 AND '
           || 'UPPER(a.release) = :p3 '
           || 'ORDER BY a.field ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_type),
                                                            UPPER(p_project_id),
                                                            UPPER(p_release);

      IF output.COUNT = 0
      THEN

          --this may be legit but more likely is a mistake in the table
          --or a convention change that the code here is oblivious to
          RAISE_APPLICATION_ERROR(-20001,'Didnt get back any ' || p_type || ' from ' || p_ref_table || ' ');

      END IF;

      --Add geoid, at least for now. Not in reference table by convention/decision
      IF UPPER(p_type) = 'ATTRIBUTE'
      THEN
         output(output.COUNT + 1) := 'GEOID';
      END IF;

      --Got similar convention/decision for QC
      IF UPPER(p_type) = 'MEASUREMENT'
      THEN
         output(output.COUNT + 1) := 'QC';
      END IF;


      RETURN output;

   END;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE GZ_PRIV_GRANTER (
      p_ref_schema_table   IN VARCHAR2 DEFAULT 'REFERENCE_SCHEMAS',
      p_like_clause        IN VARCHAR2 DEFAULT NULL
   )
   AS

         --Matt! 12/09/10
         --This mirrors the standalone procedure GZ_PRIV_GRANTER
         --This version is in the package with invokers rights, and is meant to be called by code
         --   ie the code in this schema
         --The standalone version is a helper tool for a user logged into a remote schema

         --This procedure grants privvies on the schema from where it is called

         --Sample 1: Grant privs specified in reference table on all tables in the Z609 topology
         --BEGIN
         --GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS','Z609%');
         --END;

         --Sample 2, Grant privs on all tables in my schema
         --Unlock it all
         --BEGIN
         --GZ_UTILITIES.GZ_PRIV_GRANTER();
         --END;


         legal_users           GZ_TYPES.stringhash;
         bunched_users         GZ_TYPES.stringhash;
         v_hashindex           VARCHAR2(4000);
         ref_table             VARCHAR2(4000);
         like_clause           VARCHAR2(4000);
         v_session_user        VARCHAR2(4000);
         v_current_user        VARCHAR2(4000);
         psql                  VARCHAR2(4000);
         tabs                  GZ_TYPES.stringarray;


   BEGIN

      ref_table   := UPPER(p_ref_schema_table);

      IF p_like_clause IS NOT NULL
      THEN

         like_clause := UPPER(p_like_clause);

      END IF;


      --commented this in case someone is testing from a bond schema
      --the standalone procedure that allows a remote user to unlock a schema is more strict

      --get caller
      --v_session_user := SYS_CONTEXT('USERENV', 'SESSION_USER');


      --IF v_session_user NOT LIKE 'GZ%'
      --THEN

         --RAISE_APPLICATION_ERROR(-20001,'Sorry, Im pretty fast and loose with permissions, but you have to be a GZ to use this ');

      --END IF;

      --get code and table owner
      v_current_user := SYS_CONTEXT('USERENV', 'CURRENT_USER');

      psql := 'SELECT a.table_name '
           || 'FROM user_tables a '
           || 'WHERE '
           || 'a.table_name NOT LIKE :p1 ';

      IF p_like_clause IS NULL
      THEN

         EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING 'MD%';

      ELSE

         psql := psql || ' AND '
                      || 'a.table_name LIKE :p2 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING 'MD%',
                                                              like_clause;

      END IF;

      IF tabs.COUNT = 0
      THEN

         --I think this is fatal (usually a typo) and I want to hear about it, but I could be convinced otherwise
         RAISE_APPLICATION_ERROR(-20001, 'Yo, cant find any tables to grant privvies to');

      END IF;


      --Get ref schemas filters out schemas that dont exist on this DB
      --Ref schemas filters out all but real schemas.  No roles, including public past this point
      legal_users := GZ_UTILITIES.GET_REF_SCHEMAS(v_current_user, ref_table);

      --legal_users looks something like
      --(GZDEC10ST99) SELECT
      --(GZCPB_1)     SELECT, INSERT, UPDATE, DELETE, INDEX

      --For improved performance, lets bunch up similar grant types into a single call
      --Ex GRANT SELECT on xx to GZDECST01, GZDECST02, ....

      v_hashindex := legal_users.FIRST;

      LOOP

         EXIT WHEN NOT legal_users.EXISTS(v_hashindex);

         --switch keys and values as we bunch ala
         --(SELECT) GZDEC10ST01, GZDEC10ST02,...
         --(SELECT, INSERT, UPDATE) GZCPB_1, GZCPB_2


         IF bunched_users.EXISTS(legal_users(v_hashindex))
         AND v_hashindex != v_current_user
         THEN

            --            SELECT, INSERT, UPDATE                     GZCPB_1                   , GZCPB_2
            bunched_users(legal_users(v_hashindex)) := bunched_users(legal_users(v_hashindex)) || ', ' || v_hashindex || ' ';

         ELSIF v_hashindex != v_current_user
         THEN

            --            SELECT, INSERT, UPDATE       GZCPB_1
            bunched_users(legal_users(v_hashindex)) := v_hashindex;

         ELSE

            --no grants to self
            NULL;

         END IF;

         v_hashindex := legal_users.NEXT(v_hashindex);

      END LOOP;


      v_hashindex := bunched_users.FIRST;

      LOOP

         EXIT WHEN NOT bunched_users.EXISTS(v_hashindex);

         FOR i IN 1 .. tabs.COUNT
         LOOP

            EXECUTE IMMEDIATE 'GRANT ' || v_hashindex || ' ON ' || tabs(i) || ' TO ' || bunched_users(v_hashindex) || ' WITH GRANT OPTION ';

         END LOOP;

         v_hashindex := bunched_users.NEXT(v_hashindex);

      END LOOP;

   END GZ_PRIV_GRANTER;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_TABLE (
      p_table_type        IN VARCHAR2,
      p_table_name        IN VARCHAR2,
      p_global_temp       IN VARCHAR2 DEFAULT 'N'
   )
   AS

    --Matt! Stoled from CAMPS and modified 11/4/10

    psql     VARCHAR2(8000);
    v_schema VARCHAR2(4000);



   BEGIN

      psql := 'CREATE ';

      IF p_global_temp = 'N'
      THEN

         psql := psql || 'TABLE ' || p_table_name || ' ';

      ELSE

         psql := psql || 'GLOBAL TEMPORARY TABLE ' || p_table_name || ' ';

      END IF;

      --No partitions in GZ (yet)
      --IF p_partition IS NOT NULL
      --AND p_partition_type IS NULL
      --THEN

         --psql := psql || 'PARTITION BY LIST(' || p_partition_field || ') ' || PARSE_PARTITIONS(p_partition) || ' ';

      --ELSIF p_partition IS NOT NULL
      --AND p_partition_type = 'RANGE'
      --THEN

         --psql := psql || 'PARTITION BY RANGE(' || p_partition_field || ') ' || PARSE_PARTITIONS(p_partition,p_partition_type) || ' ';

      --END IF;

      IF p_global_temp = 'N'
      THEN

         psql := psql || 'NOLOGGING AS ';

      ELSE

         psql := psql || 'AS ';

      END IF;

      psql := psql || 'SELECT * FROM TABLE(GZ_TYPES.NEW_' || p_table_type || ') ';



      BEGIN

         --dbms_output.put_line(psql);
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
               BEGIN

                  EXECUTE IMMEDIATE 'DROP TABLE ' || UPPER(p_table_name) || ' PURGE';
                  EXECUTE IMMEDIATE psql;

               EXCEPTION
                  WHEN OTHERS THEN
                     RAISE;
               END;



            ELSE
               RAISE;
            END IF;


      END;

      --Anyone who thinks grant select to public is a bad idea prepare to battle
      psql := 'GRANT SELECT ON ' || p_table_name || ' TO "PUBLIC" ';
      EXECUTE IMMEDIATE psql;


   END CREATE_TABLE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GZ_TABLE_EXISTS (
      p_table_name        IN VARCHAR2,p_empty_exists BOOLEAN default FALSE
   ) RETURN BOOLEAN
   AS

      --Matt! 8/1/11
      --This is a specific implementation of, as Bill Clinton might say, the definition of EXISTS
      --It (implicitly) checks that the object passed in exists, and there is at least one row in it
      --Synonyms passed in should also return TRUE
      --Empty tables return FALSE when empty_exists is FALSE
      -- When empty_exists is TRUE, empty tables return TRUE  (Sidey 06/25/2010)
      --See GZ_UTILTIES.TABLE_EXISTS for the Nick Padfield implementation.  Doesn't really work for me

      --sample
      -- IF NOT GZ_UTILITIES.GZ_TABLE_EXISTS(my_important_table)
      -- THEN
      --   <problem>


      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;

   BEGIN

      --kick out junk right away
      IF LENGTH(p_table_name) = 0
      THEN
         RETURN FALSE;
      END IF;


      psql := 'SELECT count(*) '
           || 'FROM ' || UPPER(p_table_name) || ' a '
           || 'WHERE rownum = 1 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO kount;

      EXCEPTION
      WHEN OTHERS THEN

         IF SQLCODE = -942
         THEN
            --ORA-00942: table or view does not exist
            RETURN FALSE;
         ELSE
            --What else is possible?
            RETURN FALSE;
         END IF;

      END;
-- Empty tables exist when empty_exists is TRUE
      IF kount = 0 AND NOT p_empty_exists
      THEN

         RETURN FALSE;

      END IF;

      RETURN TRUE;

   END GZ_TABLE_EXISTS;

   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------


   PROCEDURE ALIGN_EDGES (
      geom_table_name         VARCHAR2,
      geom_column_name        VARCHAR2,
      output_table_name       VARCHAR2,
      tolerance               NUMBER
   )
   AS

      --Donated to CPB by Dan Geringer
      --Added here 12/16/2010 Matt!
      --See http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_Questions_For_Dan_Geringer
      --  for sample usages
      --Matt! Updated to parameterize output geom column name 5/13/11
      --    See gz_utilities.GZ_ALIGN_EDGES for wrapper

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

       window_geom := SDO_GEOM.SDO_DIFFERENCE (window_geom, next_geom, tolerance);
       next_geom := SDO_GEOM.SDO_DIFFERENCE (next_geom, window_geom, tolerance);

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

           distance := GZ_UTILITIES.ACCURATE_GCD(x1,y1,x2,y2);


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

   PROCEDURE GZ_CHANGE_EDGE_COORDS (
      p_topo          IN VARCHAR2,
      p_edge_id       IN NUMBER,
      p_edge_geom     IN SDO_GEOMETRY,
      p_clean_edge    IN NUMBER DEFAULT NULL,
      p_debug         IN NUMBER DEFAULT NULL
   )
   AS

      --Matt! 2/07/11
      --This is meant to be a general purpose wrapper for SDO_TOPO_MAP.CHANGE_EDGE_COORDS
      --Like topo functions, pass in NULL for the topology if an appropriate topomap is already open
      --   If an actual topology comes in, will build a window topomap here
      --11/08/11 Changed p_clean_edge to NUMBER default .05.  Passed along as the tolerance
      --12/14/11 Attempt to manage "edge_id X not found in cache" oracle bug


      psql                 VARCHAR2(4000);
      create_it            NUMBER;
      window_geom          SDO_GEOMETRY;
      getoutofjail         PLS_INTEGER := 0;
      moved_iso_nodes      SDO_NUMBER_ARRAY;
      moved_iso_edges      SDO_NUMBER_ARRAY;
      allow_iso_moves      VARCHAR2(5) := 'FALSE';
      clean_geom           SDO_GEOMETRY;
      topotest             VARCHAR2(4000);


   BEGIN


     <<jailbreak>>

      IF p_topo IS NOT NULL
      THEN

         --Make a window for the topomap
         --got a topology name passed in

         IF getoutofjail = 0
         THEN

            --SOP
            psql := 'SELECT SDO_GEOM.SDO_MBR(e.geometry) '
                 || 'FROM '
                 || p_topo || '_edge$ e '
                 || 'WHERE '
                 || 'e.edge_id = :p1 ';

            EXECUTE IMMEDIATE psql INTO window_geom USING p_edge_id;

         ELSE

            --maybe need to load the entire topology to work around Oracle shenanigans

            psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
                 || 'FROM '
                 || p_topo || '_edge$ e ';

            EXECUTE IMMEDIATE psql INTO window_geom;

         END IF;


         IF window_geom.sdo_ordinates.COUNT != 4
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Bad window. Bad. ');

         END IF;

         --create the topomap with window

         create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                      p_topo,
                                                      2,
                                                      window_geom.sdo_ordinates(1),
                                                      window_geom.sdo_ordinates(2),
                                                      window_geom.sdo_ordinates(3),
                                                      window_geom.sdo_ordinates(4)
                                                      );

      END IF;



      BEGIN

         --always a topomap in the session at this point

         --This is Sidey's current best understanding of
         --the signature to change_edge_coords that calls up the fewest bugs

         SDO_TOPO_MAP.CHANGE_EDGE_COORDS(NULL,
                                         p_edge_id,
                                         p_edge_geom,
                                         moved_iso_nodes,
                                         moved_iso_edges,
                                         allow_iso_moves);

      EXCEPTION
      WHEN OTHERS
      THEN

         IF UPPER(SQLERRM) LIKE '%CHANGED EDGE COORDINATE STRING SELF-INTERSECTS%'
         THEN

            --This is bogus, current workaround is manual in ADE
            --Should automate that workaround right here

            --however, just in case the error isnt bogus
            --feel free to try this. Have to ask for it with a value like .05
            IF p_clean_edge IS NOT NULL
            THEN

               clean_geom := GZ_UTILITIES.REMOVE_CLOSE_ORDINATES(p_edge_geom,
                                                                 p_clean_edge);

               SDO_TOPO_MAP.CHANGE_EDGE_COORDS(NULL,
                                               p_edge_id,
                                               clean_geom,
                                               moved_iso_nodes,
                                               moved_iso_edges,
                                               allow_iso_moves);

            ELSE

               IF p_topo IS NOT NULL
               THEN

                  --commit and drop temp topomap
                  create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

               END IF;

               --tell the caller to work around this bug error
               RAISE_APPLICATION_ERROR(-20001,'ADD FAKE FACE');

            END IF;


         ELSIF UPPER(SQLERRM) LIKE '%NOT FOUND IN CACHE%'
         THEN

            --this is a bug
            --Adding the edge returned to the window just
            --   chases the error to a new edge ad infinitum
            --Automated workaround here: Make a topomap that includes the whole topology

            IF getoutofjail = 0
            THEN

               getoutofjail := 1;
               GOTO jailbreak;

            ELSE

               --out of ideas

                --GZ_UTILITIES.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
                RAISE_APPLICATION_ERROR(-20001, 'Got the not found in cache error 2x! '|| SQLERRM);

            END IF;


         ELSE     --what else gets raised here?

            IF p_topo IS NOT NULL
            THEN

               --commit and drop temp topomap
               create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

            END IF;

            RAISE;

         END IF;

      END;



      IF p_topo IS NOT NULL
      THEN

         --commit and drop temp topomap
         create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

      END IF;



   END GZ_CHANGE_EDGE_COORDS;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   PROCEDURE GZ_MOVE_NODE (
      p_topo          IN VARCHAR2,
      p_node_id       IN NUMBER,
      p_move_point    IN SDO_GEOMETRY,
      p_debug         IN NUMBER DEFAULT NULL,
      p_topology      IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 1/22/11
      --This is meant to be a general purpose wrapper for sdo_topo_map.move_node
      --   Which is sdo_topo_WACK to use
      --Standard usage: pass in first arg p_topo. If this comes in, will build a window topomap here
      --Like topo functions, pass in NULL for p_topo if an appropriate topomap is already open
      --   NB: Be sure to then also include the optional parameter 5, so we know what topology this actually is

      --10/06/11 Added p_topology option.  Dont use it. Use p_topo and build topomap on the fly


      edge_ids          GZ_TYPES.stringarray;
      pos_edge_ids      GZ_TYPES.stringarray;
      edge_geoms        GZ_TYPES.geomarray;
      psql              VARCHAR2(4000);
      newnumarray       SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY();
      ordkount          PLS_INTEGER := 0;
      wack_edge_array   SDO_EDGE_ARRAY := SDO_EDGE_ARRAY();
      create_it         NUMBER;
      window_geom       SDO_GEOMETRY;
      getoutofjail      PLS_INTEGER := 0;
      passed_topo       VARCHAR2(4000);


   BEGIN

      IF p_topology IS NULL
      THEN

         --standard usage. Pass in a topology, we will build a topomap
         --use this guy for SQL
         passed_topo := p_topo;

      ELSE

         --p_topology is not null
         --nonstandard usage. Topomap exists
         passed_topo := p_topology;

      END IF;


      --get our bucket, must maintain the order
      psql := 'SELECT tt.edge_id, ABS(tt.edge_id), e.geometry '
           || 'FROM '
           || '  (SELECT rownum myorder, t.column_value edge_id '
           || '   FROM TABLE(SDO_TOPO_MAP.GET_NODE_STAR(:p1, NULL, :p2)) t '
           || '  ) tt, '
           || passed_topo || '_edge$ e '
           || 'WHERE '
           || 'ABS(tt.edge_id) = e.edge_id '
           || 'ORDER BY tt.myorder ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_ids,
                                               pos_edge_ids,
                                               edge_geoms USING passed_topo,
                                                                p_node_id;



      --edge ids are in correct order for move_node
      --positive edge_ids mean the geometry runs from our changing spot --> end
      --negative edge_ids mean the geometry runs from start node --> our change

      FOR i IN 1 .. edge_ids.COUNT
      LOOP

         newnumarray.EXTEND(edge_geoms(i).sdo_ordinates.COUNT);

         FOR j IN 1 .. (edge_geoms(i).sdo_ordinates.COUNT)/2
         LOOP


            IF edge_ids(i) > 0
            AND j = 1
            THEN

               --move start
               ordkount := ordkount + 1;
               newnumarray(ordkount) := p_move_point.sdo_point.X;
               ordkount := ordkount + 1;
               newnumarray(ordkount) := p_move_point.sdo_point.Y;

            ELSIF edge_ids(i) < 0
            AND j = (edge_geoms(i).sdo_ordinates.COUNT)/2
            THEN

               --move end
               ordkount := ordkount + 1;
               newnumarray(ordkount) := p_move_point.sdo_point.X;
               ordkount := ordkount + 1;
               newnumarray(ordkount) := p_move_point.sdo_point.Y;

            ELSE

               --transfer ordinates unchanged
               ordkount := ordkount + 1;
               newnumarray(ordkount) := edge_geoms(i).sdo_ordinates((j * 2) - 1);
               ordkount := ordkount + 1;
               newnumarray(ordkount) := edge_geoms(i).sdo_ordinates(j * 2);

            END IF;

         END LOOP;

            --this edge has a num array with an altered tip
            wack_edge_array.EXTEND;
            wack_edge_array(i) := newnumarray;

            --reset for next loop
            newnumarray.DELETE;
            ordkount := 0;

      END LOOP;


      IF p_debug = 1
      THEN

         GZ_UTILITIES.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);

      END IF;


      <<jailbreak>>

      IF p_topo IS NOT NULL
      THEN

         --Make a window for the topomap

         IF getoutofjail = 0
         THEN

            --SOP
            psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
                 || 'FROM '
                 || p_topo || '_edge$ e '
                 || 'WHERE '
                 || 'e.edge_id IN (SELECT * FROM TABLE(:p1)) ';

            EXECUTE IMMEDIATE psql INTO window_geom
                                        USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(pos_edge_ids);

         ELSE

            --need to load the entire topology to work around move_node bug

            psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
                 || 'FROM '
                 || p_topo || '_edge$ e ';

            EXECUTE IMMEDIATE psql INTO window_geom;

         END IF;


         IF window_geom.sdo_ordinates.COUNT != 4
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Bad window. Bad. ');

         END IF;

         --create the topomap with window

         create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                      p_topo,
                                                      2,
                                                      window_geom.sdo_ordinates(1),
                                                      window_geom.sdo_ordinates(2),
                                                      window_geom.sdo_ordinates(3),
                                                      window_geom.sdo_ordinates(4)
                                                      );

      END IF;



      BEGIN


         --there will always be a topomap at this point
         SDO_TOPO_MAP.MOVE_NODE(NULL,
                                p_node_id,
                                wack_edge_array);

         --consider other signature for bug management as well
                                --moved_iso_nodes,
                                --moved_iso_edges,
                                --'TRUE'); or FALSE


      EXCEPTION
         WHEN OTHERS
         THEN

            --cleanliness is something
            IF p_topo IS NOT NULL
            THEN

               --commit and drop topomap
               create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

            END IF;

            IF SQLERRM LIKE '%Moved node has not moved%'
            THEN

               --probably a tolerance issue.  Maybe ignore it?
               GZ_UTILITIES.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
               RAISE;

            ELSIF SQLERRM LIKE '%An edge does not maintain the position of the opposite end node%'
            THEN

               --Fracked up the order probably
               GZ_UTILITIES.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
               RAISE;

            ELSIF SQLERRM LIKE '%not found in cache%'
            THEN

               --oracle.spatial.topo.TopoEntityNotFoundException: Edge ID 337 not found in cache

               --This appears to be a bug
               --Get it when not using a topomap.  Manual workaround: use a topomap

               --Also get it when using a topomap.  Adding the edge returned to the window just
               --   chases the error to a new edge ad infinitum
               --Automated workaround here: Make a topomap that includes the whole topology

               IF p_topo IS NOT NULL
               THEN

                  --We made a topomap to work with so we may get out of jail here

                  IF getoutofjail = 0
                  THEN

                     getoutofjail := 1;
                     GOTO jailbreak;

                  ELSE

                     GZ_UTILITIES.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
                     RAISE_APPLICATION_ERROR(-20001, 'Got the not found in cache error 2x! '
                                       || 'SDO_TOPO_MAP.MOVE_NODE(NULL,' || p_node_id || ', <edge array>);'
                                       || SQLERRM);

                  END IF;

               ELSE

                  GZ_UTILITIES.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
                  RAISE_APPLICATION_ERROR(-20001, 'Got the not found in cache error, consider using a topomap '
                                       || 'SDO_TOPO_MAP.MOVE_NODE(NULL,' || p_node_id || ', <edge array>);'
                                       || SQLERRM);

               END IF;

            ELSE

               GZ_UTILITIES.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
               RAISE;

            END IF;

      END;

      IF p_debug = 1
      THEN

         dbms_output.put_line('jail is ' || getoutofjail);

      END IF;

      IF p_topo IS NOT NULL
      THEN

         --commit and drop topomap
         create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

      END IF;


   END GZ_MOVE_NODE;



   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION ADD_FAKE_FACE (
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER,
      p_dupe_geom       IN SDO_GEOMETRY,
      p_dupe_i          IN PLS_INTEGER,
      p_face_tab        IN VARCHAR2,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN NUMBER
   AS

      --Matt! 2/14
      --Change edge coordinates has raised the bogus "changed edge coordinate string self-intersects"
      --  error at us.  The workardound is to build a face into the ether around the spot

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      edge_geom         SDO_GEOMETRY;
      new_edge_geom     SDO_GEOMETRY;
      new_ordinates     SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();
      new_node_1        SDO_GEOMETRY;
      new_node_1_id     NUMBER;
      new_node_2        SDO_GEOMETRY;
      new_node_2_id     NUMBER;
      new_pt_x          NUMBER;
      new_pt_y          NUMBER;
      new_pt_geom       SDO_GEOMETRY;
      new_edge_id       NUMBER;

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_FAKE_FACE: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --verify that we are on the edge of the universe
      psql := 'SELECT count(*) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 AND '
           || '(e.right_face_id = :p2 or e.left_face_id = :p3) ';

      EXECUTE IMMEDIATE psql INTO kount USING p_edge_id,
                                              -1,
                                              -1;

      IF kount != 1
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Yo, edge ' || p_edge_id || ' isnt on the clip outline ');

      END IF;

      --get the geometry of the edge
      psql := 'SELECT e.geometry '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO edge_geom USING p_edge_id;

      new_node_1 := p_dupe_geom;
      new_node_2 := p_dupe_geom;

      IF edge_geom.sdo_ordinates.EXISTS((p_dupe_i * 2) + 4)
      THEN

         --go two vertexes over for node1

         --geom will be ignored
         new_node_1.sdo_point.X :=  edge_geom.sdo_ordinates((p_dupe_i * 2) + 3);
         new_node_1.sdo_point.Y :=  edge_geom.sdo_ordinates((p_dupe_i * 2) + 4);

         new_node_1_id := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                                p_edge_id,
                                                new_node_1,
                                                p_dupe_i + 1,  --two coord indexes, +1 how Im counting vtxs
                                                'FALSE');



      ELSE

         --just get the end node
         psql := 'SELECT e.end_node_id '
              || 'FROM '
              || p_topo || '_edge$ e '
              || 'WHERE e.edge_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO new_node_1_id USING p_edge_id;

          psql := 'SELECT n.geometry '
              || 'FROM '
              || p_topo || '_node$ n '
              || 'WHERE n.node_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO new_node_1 USING new_node_1_id;

      END IF;


      IF edge_geom.sdo_ordinates.EXISTS((p_dupe_i * 2) - 5)
      THEN

         --go two vertexes in the opposite direction for number 2

         --geom will be ignored
         new_node_2.sdo_point.X :=  edge_geom.sdo_ordinates((p_dupe_i * 2) - 5);
         new_node_2.sdo_point.Y :=  edge_geom.sdo_ordinates((p_dupe_i * 2) - 4);

         new_node_2_id := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                                p_edge_id,
                                                new_node_2,
                                                p_dupe_i - 3, --two coord indexes, -3 how Im counting vtxs
                                                'FALSE');



      ELSE

         --just get the end node
         psql := 'SELECT e.start_node_id '
              || 'FROM '
              || p_topo || '_edge$ e '
              || 'WHERE e.edge_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO new_node_2_id USING p_edge_id;

         psql := 'SELECT n.geometry '
              || 'FROM '
              || p_topo || '_node$ n '
              || 'WHERE n.node_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO new_node_2 USING new_node_2_id;

      END IF;


      --make up a new point out in space
      --amateur-style

      GZ_UTILITIES.ADD_SPATIAL_INDEX(p_face_tab,
                                     'SDOGEOMETRY',
                                     new_node_1.sdo_srid,
                                     p_tolerance);


      IF ( ABS(new_node_1.sdo_point.X - new_node_2.sdo_point.X) > ABS(new_node_1.sdo_point.Y - new_node_2.sdo_point.Y) )
      THEN

         --more longish

         --avg the Xs
         new_pt_x := (new_node_1.sdo_point.X + new_node_2.sdo_point.X)/2;

         --move Y off an X sized distance in positive direction
         --   X will be stretchy a bit since its longitude converted to latitude
         new_pt_y := ((new_node_1.sdo_point.Y + new_node_2.sdo_point.Y)/2) + ABS(new_node_1.sdo_point.X - new_node_2.sdo_point.X);

         --check if this is in outer space
         new_pt_geom := new_node_1;
         new_pt_geom.sdo_point.X := new_pt_x;
         new_pt_geom.sdo_point.Y := new_pt_y;

         psql := 'SELECT count(*) FROM '
               || p_face_tab || ' a '
               || 'WHERE SDO_RELATE(a.sdogeometry, :p1, :p2) = :p3 ';

         EXECUTE IMMEDIATE psql INTO kount USING new_pt_geom,
                                                 'mask=ANYINTERACT',
                                                 'TRUE';

         IF kount > 0
         THEN

            --we are in the faces, go the other way
            new_pt_y := ((new_node_1.sdo_point.Y + new_node_2.sdo_point.Y)/2) - ABS(new_node_1.sdo_point.X - new_node_2.sdo_point.X);

            new_pt_geom.sdo_point.Y := new_pt_y;

            EXECUTE IMMEDIATE psql INTO kount USING new_pt_geom,
                                                    'mask=ANYINTERACT',
                                                    'TRUE';

            IF kount > 0
            THEN

               RAISE_APPLICATION_ERROR(-20001, 'Cant find a spot for the new point');

            END IF;

         END IF;


      ELSE

         --more tallish

         --avg the ys
         new_pt_y := (new_node_1.sdo_point.Y + new_node_2.sdo_point.Y)/2;

         --move x off a Y sized distance in positive direction
         new_pt_x := ((new_node_1.sdo_point.X + new_node_2.sdo_point.X)/2) + ABS(new_node_1.sdo_point.Y - new_node_2.sdo_point.Y);

         --check if this is in outer space
         new_pt_geom := new_node_1;
         new_pt_geom.sdo_point.X := new_pt_x;
         new_pt_geom.sdo_point.Y := new_pt_y;

         psql := 'SELECT count(*) FROM '
               || p_face_tab || ' a '
               || 'WHERE SDO_RELATE(a.sdogeometry, :p1, :p2) = :p3 ';

         EXECUTE IMMEDIATE psql INTO kount USING new_pt_geom,
                                                 'mask=ANYINTERACT',
                                                 'TRUE';

         IF kount > 0
         THEN

            --we are in the faces, go the other way
            new_pt_x := ((new_node_1.sdo_point.X + new_node_2.sdo_point.X)/2) - ABS(new_node_1.sdo_point.Y - new_node_2.sdo_point.Y);


            new_pt_geom.sdo_point.X := new_pt_x;

            EXECUTE IMMEDIATE psql INTO kount USING new_pt_geom,
                                                    'mask=ANYINTERACT',
                                                    'TRUE';

            IF kount > 0
            THEN

               RAISE_APPLICATION_ERROR(-20001, 'Cant find a spot for the new point');

            END IF;

         END IF;


      END IF;


      new_edge_geom := edge_geom;
      new_ordinates.EXTEND(6);
      new_ordinates(1) := new_node_1.sdo_point.X;
      new_ordinates(2) := new_node_1.sdo_point.Y;
      new_ordinates(3) := new_pt_geom.sdo_point.X;
      new_ordinates(4) := new_pt_geom.sdo_point.Y;
      new_ordinates(5) := new_node_2.sdo_point.X;
      new_ordinates(6) := new_node_2.sdo_point.Y;

      new_edge_geom.sdo_ordinates := new_ordinates;

      new_edge_id := SDO_TOPO_MAP.ADD_EDGE(p_topo,
                                           new_node_1_id,
                                           new_node_2_id,
                                           new_edge_geom);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_FAKE_FACE: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN new_edge_id;

   END ADD_FAKE_FACE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------


   FUNCTION GET_TEMP_EDGE(
      p_topo            IN VARCHAR2,
      p_dupepoint_geom  IN SDO_GEOMETRY,
      p_edge_id         IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 2/14/11

      psql           VARCHAR2(4000);
      edge_id        NUMBER;

   BEGIN

      psql := 'SELECT e.edge_id FROM ' || p_topo || '_edge$ e '
           || 'WHERE e.start_node_id '
           || 'IN '
           || '   (SELECT start_node_id '
           || '   FROM ' || p_topo || '_edge$ e '
           || '   WHERE e.edge_id = :p1 '
           || '   UNION ALL '
           || '   SELECT end_node_id '
           || '   FROM ' || p_topo || '_edge$ e '
           || '   WHERE e.edge_id = :p2) '
           || 'AND e.end_node_id IN '
           || '   (SELECT start_node_id '
           || '   FROM ' || p_topo || '_edge$ e '
           || '   WHERE e.edge_id = :p3 '
           || '   UNION ALL '
           || '   SELECT end_node_id '
           || '   FROM ' || p_topo || '_edge$ e '
           || '   WHERE e.edge_id = :p4) '
           || 'AND e.edge_id != :p5 ';

      EXECUTE IMMEDIATE psql INTO edge_id USING p_edge_id,
                                                p_edge_id,
                                                p_edge_id,
                                                p_edge_id,
                                                p_edge_id;

      RETURN edge_id;


   END GET_TEMP_EDGE;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   PROCEDURE REMOVE_FAKE_FACE (
      p_topo            IN VARCHAR2,
      p_face_feat_tab   IN VARCHAR2
   )
   AS

      --Matt! 2/14/11

      psql        VARCHAR2(4000);
      face_id     NUMBER;
      edje        NUMBER;
      nodes       GZ_TYPES.stringarray;
      nodekounts  GZ_TYPES.stringarray;
      kount       PLS_INTEGER;

   BEGIN


      psql := 'SELECT a.face_id FROM ' || p_topo || '_face$ a '
           || 'WHERE a.face_id != :p1 '
           || 'MINUS '
           || 'select b.face_id FROM ' || p_face_feat_tab || '  b ';

      EXECUTE IMMEDIATE psql INTO face_id USING -1;


      psql := 'SELECT e.edge_id FROM '
           || p_topo || '_edge$ e '
           || 'WHERE (e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
           || '(e.left_face_id = :p3 OR e.right_face_id = :p4) ';

      EXECUTE IMMEDIATE psql INTO edje USING face_id, face_id,
                                             -1, -1;

      psql := 'SELECT e.start_node_id FROM ' || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 '
           || 'UNION ALL '
           || 'SELECT e.end_node_id FROM ' || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO nodes USING edje, edje;


      FOR i in 1 .. nodes.COUNT
      LOOP

         psql := 'SELECT count(*) FROM '
              || p_topo || '_edge$ '
              || 'WHERE start_node_id = :p1 or end_node_id = :p2 ';

         EXECUTE IMMEDIATE psql INTO nodekounts(i) USING nodes(i),
                                                         nodes(i);


      END LOOP;

      --must remove the edge before the nodes
      SDO_TOPO_MAP.REMOVE_EDGE(p_topo,edje);


      FOR i IN 1 .. nodes.COUNT
      LOOP

         IF TO_NUMBER(nodekounts(i)) = 3
         THEN

            --just connected to the clip outline and fake edge
            SDO_TOPO_MAP.REMOVE_NODE(p_topo,nodes(i));

         END IF;

      END LOOP;


   END REMOVE_FAKE_FACE;



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

   FUNCTION GZ_GET_FSL_FACES (
      p_fsl_tab       IN VARCHAR2,
      p_fsl_oid       IN NUMBER,
      p_fsl_oid_col   IN VARCHAR2 DEFAULT 'OID',
      p_topo_col      IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt!  11/15/11
      --Quickie for Sidey, not fully tested or thought through
      --
      --DECLARE
      -- face_ids gz_types.stringarray;
      --BEGIN
      -- face_ids := GZ_UTILITIES.GZ_GET_FSL_FACES('Z601LS_FSL050',27590310671100);
      --END;

      psql              VARCHAR2(4000);
      topology          VARCHAR2(4000);
      tg_layer_level    NUMBER;
      output            GZ_TYPES.stringarray;


   BEGIN

      IF NOT (GZ_UTILITIES.GZ_TABLE_EXISTS(p_fsl_tab))
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Partner, table ' || p_fsl_tab || ' doesnt exist ');

      END IF;

      psql := 'SELECT a.topology, a.tg_layer_level '
           || 'FROM '
           || 'user_sdo_topo_Info a '
           || 'WHERE a.table_name = :p1 ';

      EXECUTE IMMEDIATE psql INTO topology,
                                  tg_layer_level USING UPPER(p_fsl_tab);

      CASE tg_layer_level

         WHEN 0 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         WHEN 1 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_relation$ r2, '
                 || topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         WHEN 2 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_relation$ r2, '
                 || topology || '_relation$ r3, '
                 || topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         WHEN 3 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_relation$ r2, '
                 || topology || '_relation$ r3, '
                 || topology || '_relation$ r4, '
                 || topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         WHEN 4 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_relation$ r2, '
                 || topology || '_relation$ r3, '
                 || topology || '_relation$ r4, '
                 || topology || '_relation$ r5, '
                 || topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = r5.tg_layer_id AND '
                 || 'r4.topo_type = r5.tg_id AND '
                 || 'r5.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         WHEN 5 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_relation$ r2, '
                 || topology || '_relation$ r3, '
                 || topology || '_relation$ r4, '
                 || topology || '_relation$ r5, '
                 || topology || '_relation$ r6, '
                 || topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = r5.tg_layer_id AND '
                 || 'r4.topo_type = r5.tg_id AND '
                 || 'r5.topo_id = r6.tg_layer_id AND '
                 || 'r5.topo_type = r6.tg_id AND '
                 || 'r6.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Hierarchy level ' ||tg_layer_level || ' is yet to be implemented!');

      END CASE;

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING p_fsl_oid;


      --is output of nothing OK?  Lets say no for now

      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Didnt find any faces for ' || p_fsl_tab || ' ' || p_fsl_oid_col || ' ' || p_fsl_oid);

      END IF;

      RETURN output;


   END GZ_GET_FSL_FACES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION GZ_GET_FSL_BDYEDGES (
      p_fsl_tab       IN VARCHAR2,
      p_fsl_oid       IN NUMBER,
      p_fsl_oid_col   IN VARCHAR2 DEFAULT 'OID',
      p_topo_col      IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt!  11/15/11
      --Quickie for Sidey, not fully tested or thought through
      --Probably super slow, turns out we dont need this so I didnt really finish it
      --I also didnt really test extensively

      --See GZ_BUILD_SOURCE.GZ_GET_BDYEDGES for a much faster and more generic utility

      --DECLARE
      -- edges gz_types.stringarray;
      --BEGIN
      -- edges := gz_utilities.GZ_GET_FSL_BDYEDGES('Z601LS_FSL050',27590310671100);
      --END;

      psql              VARCHAR2(4000);
      output            GZ_TYPES.stringarray;
      topology          VARCHAR2(4000);
      tg_layer_level    NUMBER;
      faces             GZ_TYPES.stringarray;

   BEGIN


      IF NOT (GZ_UTILITIES.GZ_TABLE_EXISTS(p_fsl_tab))
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Partner, table ' || p_fsl_tab || ' doesnt exist ');

      END IF;

      psql := 'SELECT a.topology, a.tg_layer_level '
           || 'FROM '
           || 'user_sdo_topo_Info a '
           || 'WHERE a.table_name = :p1 ';

      EXECUTE IMMEDIATE psql INTO topology,
                                  tg_layer_level USING UPPER(p_fsl_tab);

      faces := GZ_UTILITIES.GZ_GET_FSL_FACES(p_fsl_tab,
                                             p_fsl_oid,
                                             p_fsl_oid_col,
                                             p_topo_col);


      --This is atrocious
      --Should be rewritten for performance

      psql := 'SELECT e.edge_id FROM '
           || topology || '_edge$ e '
           || 'WHERE '
           || 'e.left_face_id IN (SELECT * FROM TABLE(:p1)) AND '
           || 'e.right_face_id NOT IN (SELECT * FROM TABLE(:p2)) '
           || 'UNION ALL '
           || 'SELECT e.edge_id FROM '
           || topology || '_edge$ e '
           || 'WHERE '
           || 'e.right_face_id IN (SELECT * FROM TABLE(:p3)) AND '
           || 'e.left_face_id NOT IN (SELECT * FROM TABLE(:p4)) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING GZ_UTILITIES.stringarray_to_varray(faces),
                                                            GZ_UTILITIES.stringarray_to_varray(faces),
                                                            GZ_UTILITIES.stringarray_to_varray(faces),
                                                            GZ_UTILITIES.stringarray_to_varray(faces);

      --error?
      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Couldnt find any edges for ' || p_fsl_tab || ' ' || p_fsl_oid_col || ' ' || p_fsl_oid);

      END IF;

      RETURN output;


   END GZ_GET_FSL_BDYEDGES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------
   
   FUNCTION GZ_COUNT_FSL_OVERLAPS (
      p_topology        IN VARCHAR2,
      p_fsl_tab         IN VARCHAR2,
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN NUMBER
   AS
   
      --Matt 1/14/13
      --Postulate: For any GZ topology
      --   A 0 level feature table contains no spatial overlap if no topo_ids are duplicated in relation$
      --   Any 1+ level feature table contains no spatial overlap if both
      --      No topo_types (ie pointers to lower relation$ records) are duplicated in relation$ and
      --      All layer(s) on which its built, down to 0-level, contain no overlaps

   
      psql                 VARCHAR2(4000);
      kount                NUMBER;
      tg_layer_id          NUMBER;
      tg_layer_level       NUMBER;
      group_col            VARCHAR2(32);
   
   
   BEGIN
   
      tg_layer_id := GZ_UTILITIES.GET_TG_LAYER_ID(p_topology,
                                                  p_fsl_tab,
                                                  p_topo_col,
                                                  'POLYGON');
                                                  
      tg_layer_level := GZ_UTILITIES.GET_TG_LAYER_LEVEL(p_topology,
                                                        p_fsl_tab,
                                                        p_topo_col,
                                                        'POLYGON');
                                                                                                   
      IF tg_layer_level = 0
      THEN
      
         group_col := 'TOPO_ID';
         
      ELSE
      
         group_col := 'TOPO_TYPE';
         
      END IF;
      
      --ex1 REGION in MT
      --select count(*) from bas13.mt_relation$
      --where tg_layer_id = 2721
      --group by topo_type
      --having count(topo_type) > 1
      
      --ex2 FACE feature in GZ
      --select count(*) from t699in_relation$
      --where tg_layer_id = 1
      --group by topo_id
      --having count(topo_id) > 1
      
      --Need an outer select count(*)
      --When the group by having return no results the inner query return no rows
      --at all, which is uncountable, its not even null
      
      psql := 'SELECT COUNT(*) FROM ('
           || 'SELECT COUNT(*) '
           || 'FROM ' || p_topology || '_relation$ '
           || 'WHERE '
           || 'tg_layer_id = :p1 '
           || 'GROUP BY ' || group_col || ' '
           || 'HAVING COUNT(' || group_col || ') > :p2 '
           || ')';

      EXECUTE IMMEDIATE psql INTO kount USING tg_layer_id,
                                              1;
           
                                         
      RETURN kount;
   
   END GZ_COUNT_FSL_OVERLAPS;
   
   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------
   
   FUNCTION GZ_COUNT_TOPO_OVERLAPS (
      p_topology        IN VARCHAR2,
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN NUMBER
   AS
   
      --Matt! 1/14/13
      --All-feature-table-in-topo wrapper for GZ_COUNT_FSL_OVERLAPS
      --Basically you want to check a topology and get back 0 
      
      psql                 VARCHAR2(4000);
      tabs                 GZ_TYPES.stringarray;
      total_overlaps       NUMBER := 0;
      overlap              NUMBER;
   
   
   BEGIN
   
      psql := 'SELECT table_name FROM user_sdo_topo_info a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.column_name = :p2 '
           || 'ORDER BY tg_layer_level ASC ';
           
      EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING UPPER(p_topology),
                                                          UPPER(p_topo_col);
                                                          
                                     
      FOR i IN 1 .. tabs.COUNT
      LOOP

      
         overlap := GZ_UTILITIES.GZ_COUNT_FSL_OVERLAPS(p_topology,
                                                       tabs(i),
                                                       p_topo_col);
                                          
         total_overlaps := total_overlaps + overlap;        
      
      END LOOP;
      
      RETURN total_overlaps;
   
   
   END GZ_COUNT_TOPO_OVERLAPS;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------
   
   PROCEDURE GZ_POPULATE_MEASUREMENTS (
      p_face_tab      IN VARCHAR2,
      p_pkc_col       IN VARCHAR2,
      p_column        IN VARCHAR2 DEFAULT 'ALL',
      p_records       IN VARCHAR2 DEFAULT 'ALL', --or NULL, meaning sdo null
      p_tolerance     IN NUMBER DEFAULT .05,
      p_to_srid       IN NUMBER DEFAULT NULL,
      p_subset_col    IN VARCHAR2 DEFAULT NULL,
      p_subset_val    IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 1/29/11
      --Called from clipper and topology fixer
      --8/18/11 Added subset_col and subset_val. Much better than clueing off of SDO being NULL
      --11/8/11 Added edge$ monkeying for null geoms and bad gtypes

      --Sample 1 update every measurement known
      --BEGIN
      --GZ_UTILITIES.GZ_POPULATE_MEASUREMENTS('Z644CL_CLIP_FACE','FACE_ID');
      --END;

      --Sample 2 update just the records where the sdogeometry is nulled out -- topo fixer calls like this
      --BEGIN
      --GZ_UTILITIES.GZ_POPULATE_MEASUREMENTS('Z644CL_CLIP_FACE','FACE_ID','ALL','NULL');
      --END;

      --Sample 3 just update the PA_RATIO -- clipper calls like this, one by one
      --BEGIN
      --GZ_UTILITIES.GZ_POPULATE_MEASUREMENTS('Z644CL_CLIP_FACE','FACE_ID','PA_RATIO','ALL');
      --END;

      psql              VARCHAR2(4000);
      face_id           VARCHAR2(4000);
      face_ids          GZ_TYPES.stringarray;
      updategeompsql    VARCHAR2(4000);
      nullgeompsql      VARCHAR2(4000);
      mycolumn          VARCHAR2(4000) := UPPER(p_column);
      nullgeomkount     PLS_INTEGER;
      badrecs           GZ_TYPES.stringarray;
      badedges          GZ_TYPES.stringarray;
      badedgegeoms      GZ_TYPES.geomarray;
      deadman           PLS_INTEGER := 0;
      topology          VARCHAR2(256);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_POPULATE_MEASUREMENTS: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --we should probably check a few things first to be safe
      --how about that table exists and has records for starters

      BEGIN

         psql := 'SELECT a.' || p_pkc_col || ' '
              || 'FROM '
              || p_face_tab || ' a '
              || 'WHERE ';

         IF p_subset_col IS NOT NULL
         THEN

            psql := psql || 'a.' || p_subset_col || ' = :p1 AND '
                         || 'rownum = 1 ';

            EXECUTE IMMEDIATE psql INTO face_id USING p_subset_val;

         ELSE

            psql := psql || 'rownum = 1 ';

            EXECUTE IMMEDIATE psql INTO face_id;

         END IF;


      EXCEPTION

         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Error. No records found in ' || p_face_tab);
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Table ' || p_face_tab || ' does not exist');
            ELSE
               RAISE;
            END IF;

      END;

      --dont really trust no_data_found
      IF face_id IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Error, no records found in ' || p_face_tab);

      END IF;

      --Better check some of the parms too
      IF mycolumn NOT IN ('ALL','SDOGEOMETRY','AREATOTAL','PERIMETER','LLX','LLY','URX','URY','MBR','PA_RATIO')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Never heard of the column to update ' || p_column);

      END IF;

      IF p_records NOT IN ('NULL','ALL')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Weird input p_records option ' || p_records);

      END IF;



      IF p_records = 'NULL'
      AND p_subset_val IS NULL
      THEN

         --Updating all records where sdogeometry is null

         psql := 'SELECT a.' || p_pkc_col || ' '
              || 'FROM '
              || p_face_tab || ' a '
              || 'WHERE '
              || 'a.sdogeometry IS NULL ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO face_ids;

      ELSIF p_subset_col IS NOT NULL
      AND p_subset_val IS NOT NULL
      AND p_records = 'ALL'
      THEN

         --updating all records based on a subset of values in some column

         psql := 'SELECT a.' || p_pkc_col || ' '
              || 'FROM '
              || p_face_tab || ' a '
              || 'WHERE '
              || 'a.' || p_subset_col || ' = :p1 ';

         --This is a little sloppy.  Shouldnt ever be bigger than ~50-75k records
         EXECUTE IMMEDIATE psql BULK COLLECT INTO face_ids USING p_subset_val;

         --should we check if there are any face ids here? Yes, for now, but maybe bad idea
         IF face_ids.COUNT = 0
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Didnt find any records to update ');

         END IF;

      ELSIF p_subset_col IS NOT NULL
      AND p_subset_val IS NOT NULL
      AND p_records = 'NULL'
      THEN

         --even more limited set
         --updating a subset of values in some column and also within that set only where sdogeom is null

         psql := 'SELECT a.' || p_pkc_col || ' '
              || 'FROM '
              || p_face_tab || ' a '
              || 'WHERE '
              || 'a.' || p_subset_col || ' = :p1 AND '
              || 'a.sdogeometry IS NULL ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO face_ids USING p_subset_val;

         --should we check if there are any face ids here? Yes, for now, but maybe bad idea
         IF face_ids.COUNT = 0
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Didnt find any records to update ');

         END IF;

      END IF;


      --update sdogeometry first

      IF mycolumn = 'SDOGEOMETRY'
      OR mycolumn = 'ALL'
      THEN

         updategeompsql := 'UPDATE ' || p_face_tab || ' a '
                        || 'SET ';

         IF p_to_srid IS NULL
         THEN

            --no transform necessary
            updategeompsql := updategeompsql || 'a.sdogeometry = a.topogeom.get_geometry() ';

         ELSE

            --transform from working SRID to final srid
            updategeompsql := updategeompsql || 'a.sdogeometry = SDO_CS.TRANSFORM(a.topogeom.get_geometry(),'
                                             || p_tolerance || ',' || p_to_srid || ') ';

         END IF;


         IF p_records = 'ALL'
         AND p_subset_col IS NULL
         THEN

            EXECUTE IMMEDIATE updategeompsql;

         ELSIF p_records = 'NULL'
         OR p_subset_col IS NOT NULL
         THEN

            EXECUTE IMMEDIATE updategeompsql || 'WHERE a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p1)) '
                                             USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

         END IF;

         COMMIT;


         --check to make sure that we got a geometry everywhere
         --invalid topo $ geometries sometimes gives back null
         --or gives back wacky etch-a-sketched 2004s

         --Note that at this point we are fixing NULL or Bad GTYPE SDO in our feature table
         --  by removing dupes from edge$
         --The caller is responsible for checking success and
         --   if we can get non-NULL 2003s out of the topology (even if they are slivers and total junk)
         --   attempting a second correction if desired on the feature SDO itself

         nullgeompsql := 'SELECT a.' || p_pkc_col || ' '
                      || 'FROM ' || p_face_tab || ' a '
                      || 'WHERE (a.sdogeometry IS NULL '
                      || 'OR (a.sdogeometry.sdo_gtype != :p1 AND a.sdogeometry.sdo_gtype != :p2)) ';

         IF p_records = 'ALL'
         AND p_subset_col IS NULL
         THEN

            EXECUTE IMMEDIATE nullgeompsql BULK COLLECT INTO badrecs USING 2003,
                                                                           2007;

         ELSIF p_records = 'NULL'
         OR p_subset_col IS NOT NULL
         THEN

            nullgeompsql := nullgeompsql || 'AND a.' || p_pkc_col || ' IN '
                                         || '(SELECT * FROM TABLE(:p3)) ';

            EXECUTE IMMEDIATE nullgeompsql BULK COLLECT INTO badrecs USING 2003,
                                                                           2007,
                                                                           GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

         END IF;

         WHILE badrecs.COUNT > 0
         LOOP

            --most of the time we never enter this loop
            --and looping more than once may never happen

            psql := 'SELECT a.topology '
                 || 'FROM '
                 || 'user_sdo_topo_info a '
                 || 'WHERE a.table_name = :p1 ';

            EXECUTE IMMEDIATE psql INTO topology USING UPPER(p_face_tab);

            --scary while loop catch
            IF deadman > 5
            THEN

                --IF SDO at this stage is just a helpful thing to have,
                --this may not be fatal
                --IT IS UP TO THE CALLER TO DECIDE
                EXIT;

            ELSE

               deadman := deadman + 1;

            END IF;

            --remove dups from any edges attached to our suspect geometry
            --I have this in a loop on an unsubstantiated hunch
            --that there can be some sort of domino effect.  I think I saw this once?  Wouldnt write a loop for nothin


            psql := 'SELECT ee.edge_id, ee.geometry FROM '
                 || topology || '_EDGE$ ee, '
                 || p_face_tab || ' a '
                 || 'WHERE '
                 || 'a.' || p_pkc_col || ' IN (SELECT * FROM TABLE(:p1)) '
                 || 'AND (ee.left_face_id = a.face_id OR ee.right_face_id = a.face_id) '
                 || 'AND SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(ee.geometry, :p2 ) != :p3 ';

            --there should never be more than 1 or 2 of these, not gonna worry about array sizes
            EXECUTE IMMEDIATE psql BULK COLLECT INTO badedges,
                                                     badedgegeoms USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(badrecs),
                                                                        p_tolerance,
                                                                        'TRUE';

            FOR i IN 1 .. badedges.COUNT
            LOOP

               --is there a justification for using this one (with Sidey's GCS)
               --vs oracle remove_duplicate_vertices?
               badedgegeoms(i) := GZ_UTILITIES.REMOVE_CLOSE_ORDINATES(badedgegeoms(i), p_tolerance);

               --this will properly update edge$
               --and should also make other appropriate changes, like face$ mbrs
               GZ_UTILITIES.GZ_CHANGE_EDGE_COORDS(topology,
                                                  badedges(i),
                                                  badedgegeoms(i));

            END LOOP;


            --lets now take another crack at updating NULLys

            --note reuse of updategeompsql stem from above

            EXECUTE IMMEDIATE updategeompsql || ' WHERE a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p1)) ' USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(badrecs);

            COMMIT;

            --see if we succeeded
            --note reuse of nullgeompsql - its fully formed, just bind var switches

            IF p_records = 'ALL'
            AND p_subset_col IS NULL
            THEN

               EXECUTE IMMEDIATE nullgeompsql BULK COLLECT INTO badrecs USING 2003,
                                                                              2007;

            ELSIF p_records = 'NULL'
            OR p_subset_col IS NOT NULL
            THEN

               EXECUTE IMMEDIATE nullgeompsql BULK COLLECT INTO badrecs USING 2003,
                                                                              2007,
                                                                              GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

            END IF;

            --new badrecs count is either 0 and we are out
            --or back for another loop

            --I would really like to log some clues here but we have no shared logging method

         END LOOP;


         --CALLER BEWARE
         --check to make sure that we got a geometry everywhere requested
         --invalid topo sometimes gives back null
         --or gives back wacky etch-a-sketched 2004s
         --Also, no guarantee of validity


      END IF;


      --update MBR 2nd

      IF mycolumn = 'MBR'
      OR mycolumn = 'ALL'
      THEN

          updategeompsql := 'UPDATE ' || p_face_tab || ' a set a.mbr = SDO_GEOM.SDO_MBR(a.SDOGEOMETRY) ';


         IF p_records = 'ALL'
         AND p_subset_col IS NULL
         THEN

            EXECUTE IMMEDIATE updategeompsql;

         ELSIF p_records = 'NULL'
         OR p_subset_col IS NOT NULL
         THEN

            updategeompsql := updategeompsql || 'WHERE a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p1)) ';

            EXECUTE IMMEDIATE updategeompsql USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

         END IF;

         COMMIT;

         --CALLER BEWARE
         --These MBRs can be invalid or degenerate to points and lines

      END IF;

      --update area

      IF mycolumn = 'AREATOTAL'
      OR mycolumn = 'ALL'
      THEN

          updategeompsql := 'UPDATE ' || p_face_tab || ' a '
                         || 'SET '
                         || 'a.areatotal = SDO_GEOM.sdo_area(a.SDOGEOMETRY,:p1)';


         IF p_records = 'ALL'
         AND p_subset_col IS NULL
         THEN

            EXECUTE IMMEDIATE updategeompsql USING p_tolerance;

         ELSIF p_records = 'NULL'
         OR p_subset_col IS NOT NULL
         THEN

            updategeompsql := updategeompsql || 'WHERE a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p1)) ';

            EXECUTE IMMEDIATE updategeompsql USING p_tolerance,
                                                   GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

         END IF;

         COMMIT;

         --Sometimes invalid geometries that are very small slivers make areas < 0

         updategeompsql := 'UPDATE ' || p_face_tab || ' a '
                        || 'SET a.areatotal = :p1 '
                        || 'WHERE '
                        || 'a.areatotal < :p2 ';

         IF p_records = 'ALL'
         AND p_subset_col IS NULL
         THEN

            EXECUTE IMMEDIATE updategeompsql USING 0, 0;

         ELSIF p_records = 'NULL'
         OR p_subset_col IS NOT NULL
         THEN

            updategeompsql := updategeompsql || 'AND a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p3)) ';

            EXECUTE IMMEDIATE updategeompsql USING 0, 0,
                                                   GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

         END IF;

         COMMIT;

      END IF;


      --update perimeter

      IF mycolumn = 'PERIMETER'
      OR mycolumn = 'ALL'
      THEN

          updategeompsql := 'UPDATE ' || p_face_tab || ' a '
                         || 'SET '
                         || 'a.perimeter = SDO_GEOM.SDO_LENGTH(a.SDOGEOMETRY,:p1,:p2)';


         IF p_records = 'ALL'
         AND p_subset_col IS NULL
         THEN

            EXECUTE IMMEDIATE updategeompsql USING p_tolerance,
                                                   'unit=meter';  --should parameterize this

         ELSIF p_records = 'NULL'
         OR p_subset_col IS NOT NULL
         THEN

            updategeompsql := updategeompsql || 'WHERE a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p3)) ';

            EXECUTE IMMEDIATE updategeompsql USING p_tolerance,
                                                   'unit=meter',
                                                   GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

         END IF;

         COMMIT;

      END IF;


      --update pa_ratio

      IF mycolumn = 'PA_RATIO'
      OR mycolumn = 'ALL'
      THEN

          --DECODE when area is 0 to avoid divide by 0

          updategeompsql := 'UPDATE ' || p_face_tab || ' a '
                         || 'SET '
                         || 'a.pa_ratio = DECODE(a.areatotal, 0, 0, '
                         || '                                 a.PERIMETER/SQRT(a.AREATOTAL)) ';



         IF p_records = 'ALL'
         AND p_subset_col IS NULL
         THEN

            EXECUTE IMMEDIATE updategeompsql;

         ELSIF p_records = 'NULL'
         OR p_subset_col IS NOT NULL
         THEN

            updategeompsql := updategeompsql || 'WHERE a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p3)) ';

            EXECUTE IMMEDIATE updategeompsql USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

         END IF;

         COMMIT;

      END IF;


      --update LLx

      IF mycolumn = 'LLX'
      OR mycolumn = 'ALL'
      THEN

         updategeompsql := 'UPDATE ' || p_face_tab || ' a set a.llx = SDO_GEOM.SDO_MIN_MBR_ORDINATE(a.MBR,:p1) ';


         IF p_records = 'ALL'
         AND p_subset_col IS NULL
         THEN

            EXECUTE IMMEDIATE updategeompsql USING 1;

         ELSIF p_records = 'NULL'
         OR p_subset_col IS NOT NULL
         THEN

            updategeompsql := updategeompsql || 'WHERE a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p2)) ';

            EXECUTE IMMEDIATE updategeompsql USING 1, GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

         END IF;

         COMMIT;

      END IF;

      --update LLY

      IF mycolumn = 'LLY'
      OR mycolumn = 'ALL'
      THEN

         updategeompsql := 'UPDATE ' || p_face_tab || ' a set a.lly = SDO_GEOM.SDO_MIN_MBR_ORDINATE(a.MBR,:p1) ';


         IF p_records = 'ALL'
         AND p_subset_col IS NULL
         THEN

            EXECUTE IMMEDIATE updategeompsql USING 2;

         ELSIF p_records = 'NULL'
         OR p_subset_col IS NOT NULL
         THEN

            updategeompsql := updategeompsql || 'WHERE a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p2)) ';

            EXECUTE IMMEDIATE updategeompsql USING 2, GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

         END IF;

         COMMIT;

      END IF;


      --update URX

      IF mycolumn = 'URX'
      OR mycolumn = 'ALL'
      THEN

         updategeompsql := 'UPDATE ' || p_face_tab || ' a set a.urx = SDO_GEOM.SDO_MAX_MBR_ORDINATE(a.MBR,:p1) ';


         IF p_records = 'ALL'
         AND p_subset_col IS NULL
         THEN

            EXECUTE IMMEDIATE updategeompsql USING 1;

         ELSIF p_records = 'NULL'
         OR p_subset_col IS NOT NULL
         THEN

            updategeompsql := updategeompsql || 'WHERE a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p2)) ';

            EXECUTE IMMEDIATE updategeompsql USING 1, GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

         END IF;

         COMMIT;

      END IF;

      --update URY

      IF mycolumn = 'URY'
      OR mycolumn = 'ALL'
      THEN

         updategeompsql := 'UPDATE ' || p_face_tab || ' a set a.ury = SDO_GEOM.SDO_MAX_MBR_ORDINATE(a.MBR,:p1) ';


         IF p_records = 'ALL'
         AND p_subset_col IS NULL
         THEN

            EXECUTE IMMEDIATE updategeompsql USING 2;

         ELSIF p_records = 'NULL'
         OR p_subset_col IS NOT NULL
         THEN

            updategeompsql := updategeompsql || 'WHERE a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p2)) ';

            EXECUTE IMMEDIATE updategeompsql USING 2, GZ_UTILITIES.STRINGARRAY_TO_VARRAY(face_ids);

         END IF;

         COMMIT;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_POPULATE_MEASUREMENTS: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END GZ_POPULATE_MEASUREMENTS;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   PROCEDURE POPULATE_EDIT_MEASUREMENTS (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_face_id         IN NUMBER,
      p_validate_sdo    IN VARCHAR2 DEFAULT 'Y',
      p_validate_topo   IN VARCHAR2 DEFAULT 'N', --or Y
      p_tolerance       IN NUMBER DEFAULT .05,
      p_pkc_col         IN VARCHAR2 DEFAULT 'FACE_ID',
      p_column          IN VARCHAR2 DEFAULT 'ALL',
      p_to_srid         IN NUMBER DEFAULT NULL,
      p_debug           IN NUMBER DEFAULT NULL
   )
   AS

      --Matt! 2/15/11
      --Was only checking first ring of edits. Fixed 3/10/11 Matt!

      --Use case: Some face is bad. We edit it and some of its neighbors in ADE
      --          No other action required, validation-wise or update-wise
      --          Use this procedure to validate topo and sdogeom and to
      --          update measurements in the face feature table
      --3 Key assumptions: 1. All edited faces are either the original face id
      --                      or connect to the original face id in a chain of edited faces
      --                   2. One-to-one correspondence between feature face_ids and primitive face_ids
      --                   3. Best to call this procedure after each single bad face fixing session
      --                      otherwise 2 faces fixed separately could get connected through a chain
      --                      No real harm in it, just when calling this procedure to fix face #2 it will
      --                      error out with "nothing to fix" type errors

      --Sample Usage:
      --GZ_UTILITIES.POPULATE_EDIT_MEASUREMENTS('Z955LS','Z955LS_CLIP_FACE',1407);
      --Sample usage 2, Ive been fixing stuff and I want the face table updated, but
      --                I have no confidence that the faces are valid
      --GZ_UTILITIES.POPULATE_EDIT_MEASUREMENTS('Z955LS','Z955LS_CLIP_FACE',1407,'N');

      psql              VARCHAR2(4000);
      psql2             VARCHAR2(4000);
      validstr          VARCHAR2(4000);
      kount             PLS_INTEGER;
      neighbor_faces    GZ_TYPES.stringarray;
      temp_faces        GZ_TYPES.stringarray;
      edited_faces      GZ_TYPES.stringarray;
      checked_faces     GZ_TYPES.stringhash;
      mask              VARCHAR2(4000);
      p_key             PLS_INTEGER;
      p_key_done        PLS_INTEGER;
      topo_sdo          SDO_GEOMETRY;
      old_sdo           SDO_GEOMETRY;
      vboolean          BOOLEAN;


   BEGIN

      --RAISE_APPLICATION_ERROR(-20001,'pickles');

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_EDIT_MEASUREMENTS: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      ------------------------
      --VERIFICATION REGIMEN--
      ------------------------

      --verify that input face exists
      psql := 'SELECT count(*) '
           || 'FROM '
           || p_face_tab || ' a '
           || 'WHERE a.' || p_pkc_col || ' = :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_face_id;

      IF kount != 1
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Cant find a ' || p_pkc_col || ' ' || p_face_id || ' in ' || p_face_tab || ' ');

      END IF;

      --validate the topology if requested. Slower

      IF p_validate_topo = 'Y'
      THEN

         validstr := GZ_UTILITIES.VALIDATE_TOPOLOGY(p_topo);

         --should raise an error, but just in case
         IF validstr != 'TRUE'
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Topology ' || p_topo || ' isnt valid ');

         END IF;

         --sreenis validator for matching pointers
         vboolean := GZ_TOPO_UTIL.validate_feature_tables(SYS_CONTEXT('USERENV', 'CURRENT_USER'), p_topo);

         IF vboolean
         THEN

            NULL;

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Validate_feature_tables says ' || p_topo || ' isnt valid ');

         END IF;

      END IF;

      --theres a nicely packaged procedure GZ_SMPOLY.ARE_MY_POLY_FEATURES_VALID
      --that does a lot of this. Should rewrite with a tolerance

      IF p_validate_sdo = 'Y'
      THEN

         --verify that face in question is actually valid and a polygon now
         --and grab both geoms while we are at it
         --(need to consider possibility that this will be false if face has problems in several spots)
         psql := 'SELECT '
              || 'SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.topogeom.get_geometry(), :p1), '
              || 'a.sdogeometry, '
              || 'a.topogeom.get_geometry() '
              || 'FROM '
              ||  p_face_tab || ' a '
              || 'WHERE a.' || p_pkc_col || ' = :p2 ';

         EXECUTE IMMEDIATE psql INTO validstr,
                                     old_sdo,
                                     topo_sdo USING p_tolerance,
                                                    p_face_id;

         IF validstr != 'TRUE'
         THEN

            RAISE_APPLICATION_ERROR(-20001,'The topogeom for face ' || p_face_id || ' isnt valid ');

         END IF;

         IF topo_sdo.sdo_gtype != 2003
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Face ' || p_face_id || ' has a gtype of ' || topo_sdo.sdo_gtype || ' ');

         END IF;


         --check that topogeom is not the same as sdogeom for our edited face
         --cant do use SQL call to sdo_geom.relate

         topo_sdo.sdo_srid := NULL;
         old_sdo.sdo_srid := NULL;

         --dbms_output.put_line(to_char(mattool.dump_sdo(topo_sdo)));
         --dbms_output.put_line(to_char(mattool.dump_sdo(old_sdo)));

         --Holy boy, what tolerance is a guarantee here?  How close can two vertexes be?
         --I guess we have 16 digits on the topology for create feature snapping
         --And it could go even lower. I dunno

         IF SDO_GEOM.RELATE(topo_sdo, 'mask=determine', old_sdo, .00000000000000000000005) = 'EQUAL'
         THEN

            RAISE_APPLICATION_ERROR(-20001,'This error shouldnt be possible, check the tolerance.  Topogeom and sdogeom are equal, but only one is valid ');

         END IF;

      END IF;

      --------------------
      --OK now real work--
      --------------------

      --put base face in list of edited and checked faces.  We did that above
      --or, since hes the focus here, we'll just update his SDO no matter what

      kount := 1;
      edited_faces(kount) := p_face_id;
      kount := kount + 1;
      checked_faces(TO_CHAR(p_face_id)) := '1';

      --Work our way out from each face, checking for edited neighbors
      --tempted for 1/2 second to use sdo_relate, but it means checking every face

      --set up initial list of neighbors
      psql := 'SELECT face_id FROM ( '
           || 'SELECT e.left_face_id face_id FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.right_face_id = :p1 '
           || 'UNION '
           || 'SELECT e.right_face_id face_id FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.left_face_id = :p2 '
           || ') WHERE face_id != :p3 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO neighbor_faces USING p_face_id,
                                                                    p_face_id,
                                                                    -1;


      --set up checker sql for use in loop
      psql2 := 'SELECT a.sdogeometry, a.topogeom.get_geometry() '
            || 'FROM '
            || p_face_tab || ' a '
            || 'WHERE '
            || 'a.face_id = :p1 ';


      p_key := neighbor_faces.FIRST;  --ie 1

      --dbms_output.put_line('start key is ' || p_key);

      LOOP

         EXIT WHEN NOT neighbor_faces.EXISTS(p_key);

         IF checked_faces.EXISTS(TO_CHAR(neighbor_faces(p_key)))
         THEN

            --we can get in here because Im sloppy below
            NULL;

            IF p_debug = 1
            THEN
               dbms_output.put_line('skipping face ' || neighbor_faces(p_key));
            END IF;

         ELSE

            IF p_debug = 1
            THEN
               dbms_output.put_line('checking face ' || neighbor_faces(p_key));
            END IF;

            checked_faces(TO_CHAR(neighbor_faces(p_key))) := '1';

            EXECUTE IMMEDIATE psql2 INTO old_sdo,
                                         topo_sdo USING neighbor_faces(p_key);

            topo_sdo.sdo_srid := NULL;
            old_sdo.sdo_srid := NULL;

            IF SDO_GEOM.RELATE(topo_sdo, 'mask=equal', old_sdo, .00000000000000000000005) = 'FALSE'
            THEN

               IF p_debug = 1
               THEN
                  dbms_output.put_line('adding face ' || neighbor_faces(p_key) || ' to list ');
               END IF;

               --weve never checked this face till just now and
               --this face is in our edit list

               edited_faces(kount) := neighbor_faces(p_key);
               kount := kount + 1;

               --get all of his neighbors too
               EXECUTE IMMEDIATE psql BULK COLLECT INTO temp_faces USING neighbor_faces(p_key),
                                                                         neighbor_faces(p_key),
                                                                         -1;

               --spin through this new neighbor list and add any newbies to our running list
               FOR i IN 1 .. temp_faces.COUNT
               LOOP

                  IF checked_faces.EXISTS(TO_CHAR(temp_faces(i)))
                  THEN

                     --we just backtracked over an edge we already checked
                     --no add to list
                     IF p_debug = 1
                     THEN
                        dbms_output.put_line('already checked ' || temp_faces(i));
                     END IF;

                     NULL;

                  ELSE

                     IF p_debug = 1
                     THEN
                        dbms_output.put_line('adding face ' || temp_faces(i) || ' to list. Not checked yet ');
                        dbms_output.put_line('neighbor faces key for the add is ' || (neighbor_faces.COUNT + 1));
                     END IF;

                     --note that there will be duplicate UNCHECKED neighbors added at this point
                     --but as soon as one gets checked the others will be skipped and deleted
                     --as they are encountered at the top of the loop
                     neighbor_faces(neighbor_faces.COUNT + 1) := temp_faces(i);

                  END IF;

               END LOOP;


            END IF;

            --IF any other mask we dont care, just leave this IF and delete the face from our neighbors

         END IF;


         p_key := neighbor_faces.NEXT(p_key);

         --WHY? !Digame!
         --p_key_done := p_key;

         --p_key := neighbor_faces.NEXT(p_key_done);

         --neighbor_faces.DELETE(p_key_done);

      END LOOP;


      --edit_faces is now a chain of all edited faces

      --update sdo to NULL since populate measurements keys off of this

      psql := 'UPDATE ' || p_face_tab || ' a '
           || 'SET '
           || 'a.sdogeometry = NULL '
           || 'WHERE '
           || 'a.face_id IN (SELECT * FROM TABLE(:p1)) ';

      IF p_debug = 1
      THEN
         dbms_output.put_line('edited faces kount is ' || edited_faces.COUNT);
      END IF;

      EXECUTE IMMEDIATE psql USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(edited_faces);

      COMMIT;

      GZ_UTILITIES.GZ_POPULATE_MEASUREMENTS(p_face_tab,
                                            p_pkc_col,
                                            'ALL',
                                            'NULL',
                                            p_tolerance,
                                            p_to_srid);



      IF p_validate_sdo = 'Y'
      THEN

         --verify that at least the sdo is now valid

         psql := 'SELECT SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.sdogeometry, :p1) '
              || 'FROM '
              || p_face_tab || ' a '
              || 'WHERE a.' || p_pkc_col || ' = :p2 ';

         EXECUTE IMMEDIATE psql INTO validstr USING p_tolerance,
                                                    p_face_id;

         IF validstr != 'TRUE'
         THEN

            RAISE_APPLICATION_ERROR(-20001,'HEY. We think we updated everything.  We are at the end of the procedure.  But the '
                                        || 'sdogeometry for face id ' || p_face_id || ' is not valid ');

         END IF;

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_EDIT_MEASUREMENTS: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END POPULATE_EDIT_MEASUREMENTS;

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

   PROCEDURE IMPORT_SDOTOPO11G (
      p_topo            IN VARCHAR2,
      p_feature_tabs    IN VARCHAR2 DEFAULT NULL
   )
   AS

    --Matt! 3/30/11
    --Wrapper for Sivas ID_GEOM
    --Corrects 10g topogeom geometries imported into 11g to avoid
    --ORA-29532: Java call terminated by uncaught Java exception: java.lang.NumberFormatException: empty String
    --ORA-06512: at "MDSYS.SDO_TOPO_MAP", line 189
    --Call this after running standard import into 11g
    --Not clear to me (yet) if feature tables also have to be "touched" so its an option

    topo             VARCHAR2(4000) := UPPER(p_topo);
    psql             VARCHAR2(4000);
    kount            PLS_INTEGER;
    TYPE strarray    IS TABLE OF VARCHAR2(4000) INDEX BY PLS_INTEGER;
    feattabs         strarray;
    featcols         strarray;
    featidxs         strarray;

   BEGIN

      --checks

      psql := 'SELECT COUNT(*) FROM '
           || 'user_sdo_topo_info a '
           || 'WHERE a.topology = :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING topo;

      IF kount = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry buddy, cant find topology ' || topo || ' in this schema ');

      END IF;


      psql := 'SELECT COUNT(*) FROM '
           || 'user_tables a '
           || 'WHERE a.table_name IN (:p1,:p2,:p3) ';

      EXECUTE IMMEDIATE psql INTO kount USING topo || '_EDGE$',  --geometry
                                              topo || '_NODE$',  --geometry
                                              topo || '_FACE$';  --mbr_geometry

      IF kount != 3
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry buddy, cant find edge$ or node$ or face$ for topology ' || topo || ' in this schema ');

      END IF;

      psql := 'SELECT b.table_name, b.column_name, c.index_name '
           || 'FROM '
           || 'user_tab_columns b, '
           || 'user_indexes c '
           || 'WHERE '
           || 'b.data_type = :p1 AND '
           || 'c.index_type = :p2 AND '
           || 'b.table_name IN (:p3, :p4, :p5) AND '
           || 'b.table_name = c.table_name ';

      --Get feature tables too?
      IF p_feature_tabs IS NOT NULL
      THEN

         psql := psql || 'UNION '
              || 'SELECT a.table_name, b.column_name, c.index_name '
              || 'FROM '
              || 'user_sdo_topo_info a, '
              || 'user_tab_columns b, '
              || 'user_indexes c '
              || 'WHERE '
              || 'a.topology = :p6 AND '
              || 'b.data_type = :p7 AND '
              || 'c.index_type = :p8 AND '
              || 'a.table_name = b.table_name AND '
              || 'a.table_name = c.table_name ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO feattabs,
                                                  featcols,
                                                  featidxs USING 'SDO_GEOMETRY',
                                                                 'DOMAIN',
                                                                 topo || '_EDGE$',
                                                                 topo || '_FACE$',
                                                                 topo || '_NODE$',
                                                                 topo,
                                                                 'SDO_GEOMETRY',
                                                                 'DOMAIN';

      ELSE

         EXECUTE IMMEDIATE psql BULK COLLECT INTO feattabs,
                                                  featcols,
                                                  featidxs USING 'SDO_GEOMETRY',
                                                                 'DOMAIN',
                                                                 topo || '_EDGE$',
                                                                 topo || '_FACE$',
                                                                 topo || '_NODE$';


      END IF;


      FOR i in 1 .. feattabs.COUNT
      LOOP


         EXECUTE IMMEDIATE 'ALTER INDEX ' || featidxs(i) || ' UNUSABLE ';

         psql := 'UPDATE ' || feattabs(i) || ' a '
              || 'SET a.' || featcols(i) || ' = GZ_UTILITIES.ID_GEOM(a.' || featcols(i) || ')';

         EXECUTE IMMEDIATE psql;
         COMMIT;

         EXECUTE IMMEDIATE 'ALTER INDEX ' || featidxs(i) || ' REBUILD ';


      END LOOP;


   END IMPORT_SDOTOPO11G;




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

   FUNCTION DUMP_TEST_CASE (
      p_topo            IN VARCHAR2,
      p_feature_tab     IN VARCHAR2,
      p_edge_sql        IN VARCHAR2
   ) RETURN DUMPTAB PIPELINED
   AS

      --Matt! 2/18/11 No more picking apart a topology piece by piece to build test cases
      --This is for simple test case builds from small to medium-sized edge$ records
      --It will produce a script, which, if possible, is best for test cases
      --See GZ_UTILITIES.DUMP_TEST_CASE_PRC for polys and more complex geoms

      --sample usage: make a topo with all of the edges in the state clip outline
      --select dumptext from
      --table(mattool.dump_test_case('DUMPTEST2','DUMPTESTTAB2',
      --      'select e.geometry from Z955LS_STATE_EDGES_Z9_V1 a, Z955LS_relation$ r, Z955LS_edge$ e where a.topogeom.tg_id = r.tg_id and a.topogeom.tg_layer_id = r.tg_layer_id and r.topo_id = e.edge_id'))
      --Then right click on TOAD and export to flat file

      output         CLOB;
      kount          PLS_INTEGER := 0;

      TYPE sdoarray  IS TABLE OF SDO_GEOMETRY
                     INDEX BY PLS_INTEGER;
      my_sdoarray    sdoarray;

      varpipe        DUMPREC;
      startpt        NUMBER;
      endpt          NUMBER;
      lengthpt       NUMBER;
      my_cursor      SYS_REFCURSOR;
      itstheend      PLS_INTEGER := 0;
      ordklump       PLS_INTEGER := 0;



   BEGIN

      varpipe.dumptext :=                     '--This is a test of ??' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--SR ??' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--Submitted by Matt Schell' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--Contact Matt Schell (matthew.c.schell@census.gov)' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--Create topology' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO.CREATE_TOPOLOGY(''' || p_topo || ''' ,.05,8265,NULL,NULL,NULL,NULL,16); ' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--insert universal face' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'INSERT INTO ' || p_topo || '_FACE$ VALUES (-1, null, sdo_list_type(), sdo_list_type(), null); ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'commit;' || chr(10);

      PIPE ROW(varpipe);

      varpipe.dumptext := Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--create dummy feature table' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'CREATE TABLE ' || p_feature_tab || '(id NUMBER, topogeom MDSYS.sdo_topo_geometry);' || chr(10);
      varpipe.dumptext := varpipe.dumptext || '--add table to topology' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO.add_topo_geometry_layer(''' || p_topo || ''',''' || p_feature_tab || ''',''TOPOGEOM'',''LINE'');' || chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--load a topomap' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO_MAP.DROP_TOPO_MAP(''' || p_topo || '_MAP'');' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXCEPTION ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'WHEN OTHERS THEN NULL; ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END;' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO_MAP.CREATE_TOPO_MAP(''' || p_topo || ''',''' || p_topo || '_MAP'');' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO_MAP.LOAD_TOPO_MAP(''' || p_topo || '_MAP'', ''TRUE'',''TRUE'');' || chr(10);

      PIPE ROW(varpipe);


      output := '';

      --get edge geometries


      OPEN my_cursor FOR p_edge_sql;
      LOOP

         FETCH my_cursor BULK COLLECT INTO my_sdoarray LIMIT 50;
         EXIT WHEN my_sdoarray.COUNT = 0;

         --insert lines

         FOR i IN 1 .. my_sdoarray.COUNT
         LOOP

            kount := kount + 1;

            --varpipe.dumptext := Chr(10);
            varpipe.dumptext := 'INSERT INTO ' || p_feature_tab
                             || ' VALUES(' || kount ||',SDO_TOPO_MAP.CREATE_FEATURE(''' || p_topo || ''',''' || p_feature_tab || ''',''TOPOGEOM'','; --no NL
            PIPE ROW(varpipe);


            --manage CLOBs here

            output := GZ_UTILITIES.DUMP_SDO(my_sdoarray(i)); --no NL
            output := output || '));' || Chr(10);
            output := output || 'commit;' || Chr(10);


            endpt := 0;
            ordklump := 0;

            LOOP


               startpt := endpt + 1;

               --jump ahead 3000 chars, find a comma
               endpt := REGEXP_INSTR(output,',', (startpt + 3000));


               IF endpt = 0
               THEN

                  itstheend := 1;
                  --endpt := DBMS_LOB.GETLENGTH(output);
                  endpt := length(output);

               END IF;

               lengthpt := endpt - startpt + 1;


               varpipe.dumptext := TO_CHAR(substr(output,startpt,lengthpt));

               IF ordklump > 0
               THEN

                  --remove newline character at the start (after comma) in next klump
                  varpipe.dumptext := substr(varpipe.dumptext,2);

               END IF;

               PIPE ROW(varpipe);

               ordklump := ordklump + 1;

               IF itstheend = 1
               THEN

                  itstheend := 0;
                  EXIT;

               END IF;

            END LOOP;

            output := '';


         END LOOP;

      END LOOP;


      --clean up
      varpipe.dumptext := Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--clean up and initialize ' ||  Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO_MAP.COMMIT_TOPO_MAP(); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO_MAP.DROP_TOPO_MAP(''' || p_topo || '_MAP''); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO.INITIALIZE_METADATA(''' || p_topo || '''); ' || Chr(10);

      PIPE ROW(varpipe);



   END DUMP_TEST_CASE;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

    FUNCTION DUMP_TEST_CASE_PRC (
      p_topo            IN VARCHAR2,
      p_proc_name       IN VARCHAR2,
      p_type            IN VARCHAR2 DEFAULT 'LINE',
      p_geom_sql        IN VARCHAR2
   ) RETURN DUMPTAB PIPELINED
   AS

      --Matt! 05/06/11 No more picking apart a topology piece by piece to build test cases
      --This is the version that builds a procedure that you then compile and execute
      --This one is intended for bigger geometries (> 999 arguments in insert statement, ie 999 ordinates, I think)
      --See GZ_UTILITIES.DUMP_TEST_CASE for a simpler version that produces a script


      --sample usage: make a procedure that builds a topology from the SDO in the select statement
      --select dumptext from
      --table(gz_utilities.dump_test_case_prc('CFTEST2','CFTESTPROC','POLYGON','select a.sdogeometry from tab10_sl040 a')) --Then right click on TOAD and export to flat file


      output         CLOB;
      kount          PLS_INTEGER := 0;

      TYPE sdoarray  IS TABLE OF SDO_GEOMETRY
                     INDEX BY PLS_INTEGER;
      my_sdoarray    sdoarray;

      varpipe        DUMPREC;
      startpt        NUMBER;
      endpt          NUMBER;
      lengthpt       NUMBER;
      my_cursor      SYS_REFCURSOR;
      itstheend      PLS_INTEGER := 0;
      ordklump       PLS_INTEGER := 0;
      v_type         VARCHAR2(32) := UPPER(p_type);



   BEGIN

      --checks
      IF v_type NOT IN ('LINE','POLYGON')
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Sorry, p_type must be either LINE or POLYGON, you entered: ' || v_type);
      END IF;


      --As the kids say, this is so meta....

      varpipe.dumptext := 'CREATE OR REPLACE PROCEDURE ' || p_proc_name || ' (p_table_name VARCHAR2) ' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || 'AUTHID CURRENT_USER AS  ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   table_name     VARCHAR2(32) := UPPER(p_table_name); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   psql           VARCHAR2(4000); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   geom           SDO_GEOMETRY; ' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '  --This is a test of ??' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '  --SR ??' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '  --Submitted by ??' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '  --Contact ?? (??.?.??@census.gov)' || Chr(10);

      PIPE ROW(varpipe);

      varpipe.dumptext := Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || Chr(10);


      varpipe.dumptext := varpipe.dumptext || '--deregister table if it already exists from a previous run' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   SDO_TOPO.DELETE_TOPO_GEOMETRY_LAYER(''' || p_topo || ''',table_name,''TOPOGEOM'' ); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXCEPTION ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'WHEN OTHERS THEN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   NULL; ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END; ' || Chr(10);


      varpipe.dumptext := varpipe.dumptext || '--drop topo too if it already exists from a previous run' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   SDO_TOPO.DROP_TOPOLOGY(''' || p_topo || '''); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXCEPTION ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'WHEN OTHERS THEN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   NULL; ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END; ' || Chr(10);


      varpipe.dumptext := varpipe.dumptext || '--drop table if it already exists from a previous run' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'psql := ''DROP TABLE '' || table_name ;' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   EXECUTE IMMEDIATE psql; ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXCEPTION ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'WHEN OTHERS THEN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   NULL; ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END; ' || Chr(10);



      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--Create topology' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO.CREATE_TOPOLOGY(''' || p_topo || ''' ,.05,8265,NULL,NULL,NULL,NULL,16); ' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--insert universal face' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXECUTE IMMEDIATE ''INSERT INTO ' || p_topo || '_FACE$ VALUES (-1, null, sdo_list_type(), sdo_list_type(), null) ''; ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'commit;' || chr(10);

      PIPE ROW(varpipe);

      varpipe.dumptext := Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--create dummy feature table' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXECUTE IMMEDIATE ''CREATE TABLE '' || table_name || ''(id NUMBER, topogeom MDSYS.sdo_topo_geometry) ''; ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || '--add table to topology' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO.add_topo_geometry_layer(''' || p_topo || ''',table_name,''TOPOGEOM'',''' || v_type || ''');' || chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--load a topomap' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || '   SDO_TOPO_MAP.DROP_TOPO_MAP(''' || p_topo || '_MAP'');' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXCEPTION ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || '   WHEN OTHERS THEN NULL; ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END;' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO_MAP.CREATE_TOPO_MAP(''' || p_topo || ''',''' || p_topo || '_MAP'');' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO_MAP.LOAD_TOPO_MAP(''' || p_topo || '_MAP'', ''TRUE'',''TRUE'');' || chr(10);

      PIPE ROW(varpipe);


      output := '';

      --get geometries

      OPEN my_cursor FOR p_geom_sql;
      LOOP

         FETCH my_cursor BULK COLLECT INTO my_sdoarray LIMIT 25;
         EXIT WHEN my_sdoarray.COUNT = 0;

         --insert geoms

         FOR i IN 1 .. my_sdoarray.COUNT
         LOOP

            kount := kount + 1;

            --varpipe.dumptext := Chr(10);
            --varpipe.dumptext := 'INSERT INTO ' || p_feature_tab
                             --|| ' VALUES(' || kount ||',SDO_TOPO_MAP.CREATE_FEATURE(''' || p_topo || ''',''' || p_feature_tab || ''',''TOPOGEOM'','; --no NL

            varpipe.dumptext := 'geom := '; --no newline I think
            PIPE ROW(varpipe);


            --manage CLOBs here

            output := GZ_UTILITIES.DUMP_SDO(my_sdoarray(i)) || ';';
            --output := output || '));' || Chr(10);
            --output := output || 'commit;' || Chr(10);


            endpt := 0;
            ordklump := 0;

            LOOP


               startpt := endpt + 1;

               --jump ahead 3000 chars, find a comma
               endpt := REGEXP_INSTR(output,',', (startpt + 3000));


               IF endpt = 0
               THEN

                  itstheend := 1;
                  --endpt := DBMS_LOB.GETLENGTH(output);
                  endpt := length(output);

               END IF;

               lengthpt := endpt - startpt + 1;


               varpipe.dumptext := TO_CHAR(substr(output,startpt,lengthpt));

               IF ordklump > 0
               THEN

                  --remove newline character at the start (after comma) in next klump
                  varpipe.dumptext := substr(varpipe.dumptext,2);

               END IF;

               PIPE ROW(varpipe);

               ordklump := ordklump + 1;

               IF itstheend = 1
               THEN

                  itstheend := 0;
                  EXIT;

               END IF;

            END LOOP;

            output := '';

            varpipe.dumptext := Chr(10);

            varpipe.dumptext := varpipe.dumptext || 'psql := ''INSERT INTO '' || table_name || '' VALUES('' ' || Chr(10);
            varpipe.dumptext := varpipe.dumptext || '   || '':p1,SDO_TOPO_MAP.CREATE_FEATURE(:p2,:p3,:p4,:p5)) ''; ' || Chr(10);
            varpipe.dumptext := varpipe.dumptext || 'EXECUTE IMMEDIATE psql USING ' || kount || ',''' || p_topo || ''',table_name,''TOPOGEOM'',geom; ' || Chr(10);
            varpipe.dumptext := varpipe.dumptext || 'COMMIT; ' || Chr(10);

            PIPE ROW(varpipe);

         END LOOP;

      END LOOP;


      --clean up
      varpipe.dumptext := Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--clean up and initialize ' ||  Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO_MAP.COMMIT_TOPO_MAP(); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO_MAP.DROP_TOPO_MAP(''' || p_topo || '_MAP''); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO.INITIALIZE_METADATA(''' || p_topo || '''); ' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END ' || p_proc_name || '; ' || Chr(10);

      PIPE ROW(varpipe);



   END DUMP_TEST_CASE_PRC;

  -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

function FACE_DIFFS (
      p_face1            IN number,
      p_face2            IN number,
      p_face_table_name  IN varchar2,
      p_reference_face_fields_tab IN varchar2, 
      p_release_name     IN VARCHAR2 DEFAULT NULL,
      p_gen_project_id   IN VARCHAR2 DEFAULT NULL
   ) RETURN ty_attr_diff_tab PIPELINED AS

   -- Stephanie:  July 29, 2011

   -- Interactive function designed to help figure out what attributes are different
   -- in a generalized topology's face feature table if you know two face ids
   -- and have a list of attributes to compare (in a REFERENCE_FACE_FIELDS...
   -- style table.)

   -- Parameters:
   --  p_face1: face_id for a face in p_face_table_name
   --  p_face2: face_id for a face in p_face_table_name
   --  p_face_table_name: A face feature table with "face_id" column and some attribute fields
   --  p_reference_face_fields_tab: a "reference_face_fields.." style table that contains a list of
   --                               attribute field names that exist in your p_face_table_name.
   --                               needs a field called "FIELD", and a field called "FIELD_USE"
   --                               For each column name in 'FIELD' that where 'FIELD_USE' = 'attribute'
   --                               this function will compare the values for that column between the two faces.
   --                               See the Generalization wiki for the layout of this table.
   
   --                   Duplicate field names in this table are not allowed.  If you are using a
   --                   reference_face_fields table that has duplicate records for a field name,
   --                   you will need to pass the optional "p_release_name" and "gen_project_id" 
   --                   values (and columns called 'release' and 'gen_project_id' will need to be
   --                   in the reference_face_fields table
   --                   in order to get the correct result from the function.  If you are using a 
   --                   reference face_fields table from production, pass in the last two parameters.

   --  p_release_name: The name in the "release" column of the
   --                  reference_face_fields_tab to select by.  For example,
   --                  "ACS12", or "DEC10W6"
   --                  if you enter this parameter, you MUST also enter a gen_project_id
   
   --  p_gen_project_id: The gen_project_id in the "gen_project_id" column of
   --                    the reference_face_fields_tab to select by. For 
   --                    example 'z6' or 'z9'  In ptoduction reference face_fields
   --                    tables this is usually the lowercase version of the
   --                    resolution code.  This parameter is case sensitive.
   --                  if you enter this parameter, you MUST also enter a release name
   
   -- example calls...

   -- get a report for all fields...
      -- select * from table(GZ_UTILTITIES.FACE_DIFFS(123,456,'Z899PB_FACE','REFERENCE_FACE_FIELDS_Z8', 'ACS12W3','z6')) ;

   -- how many match and how many are mismatches...
      -- select count(*),STATUS from table(GZ_UTILTITIES.FACE_DIFFS(123,456,'Z899PB_FACE','REFERENCE_FACE_FIELDS_Z8','ACS12W3','z6')) GROUP BY STATUS;

   -- just give me the fields that do not match...
      -- select * from table(GZ_UTILTITIES.FACE_DIFFS(123,456,'Z899PB_FACE','REFERENCE_FACE_FIELDS_Z8','ACS12W3','z6')) where status = 'MISMATCH' ;

   -- February 21, 2013:  Updated to allow use of the new reference
   --                     face fields format and added "release_name" and
   --                     "gen_project_id" are passed as arguments)       
      
   -- variable declarations section
   vsql             varchar2(4000);
   vattribute_list  gz_types.stringarray;
   vresult          gz_utilities.ty_attr_diff_rec;
   vrelease         varchar2(4000) := UPPER(p_release_name);
   -- currently reference face fields is forced to lowercase in the production ref face fields talbe
   vgpid            varchar2(4000) := (p_gen_project_id); 

   -- Do the work in this block...
   BEGIN

   -- get a list of fields from the reference table...
   IF vrelease IS NOT NULL
   THEN
      IF vgpid IS NULL THEN
          RAISE_APPLICATION_ERROR (
               -20001,
               'If p_release_name is passed as a parameter, you must also pass p_gen_project_id.');
      END IF;
      -- This works with a release name and gen_project ID
      vsql :=
         'select FIELD from ' || p_reference_face_fields_tab
         || ' where FIELD_USE = :p1 and release = :p2 and gen_project_id = :p3';

      BEGIN
         EXECUTE IMMEDIATE vsql
            BULK COLLECT INTO vattribute_list
            USING 'attribute', vrelease, vgpid;
      EXCEPTION
         WHEN OTHERS
         THEN
            RAISE_APPLICATION_ERROR (
               -20001,
               'Trouble selecting a list of attribute fields from reference_face_fields table, '
               || p_reference_face_fields_tab
               || '.  SQL = '
               || vsql
               || ' USING ''attribute'','''||vrelease||''''||vgpid||'''.');
      END;
   ELSE
        -- This works without a release name and gen_project_id  
      vsql :=
            'select FIELD from '
         || p_reference_face_fields_tab
         || ' where FIELD_USE = :p1';

      BEGIN
         EXECUTE IMMEDIATE vsql
            BULK COLLECT INTO vattribute_list
            USING 'attribute';
      EXCEPTION
         WHEN OTHERS
         THEN
            RAISE_APPLICATION_ERROR (
               -20001,
               'Trouble selecting a list of attribute fields from reference_face_fields table, '
               || p_reference_face_fields_tab
               || '.  SQL = '
               || vsql
               || ' USING ''attribute''.');
      END;
   END IF;

 

   vresult.ATTR := 'FACE_ID';
   vresult.FACE1 := p_face1;
   vresult.FACE2 := p_face2;
   vresult.STATUS := NULL;

   PIPE ROW(vresult);

       for i IN 1..vattribute_list.COUNT LOOP
           vresult.ATTR := NULL;
           vresult.FACE1 := NULL;
           vresult.FACE2 := NULL;
           vresult.STATUS := NULL;

           -- First caputure the FACE_IDs...

           vresult.ATTR := vattribute_list(i);
           vsql := 'select '||vresult.ATTR||' from '||p_face_table_name||
                   ' where face_id = :p1';
           BEGIN
              EXECUTE IMMEDIATE vsql INTO vresult.FACE1 USING p_face1;
                EXCEPTION
                WHEN OTHERS THEN
                   RAISE_APPLICATION_ERROR(-20001,'Trouble selecting attribute value ('||vresult.ATTR||') for face '||p_face1||' from '||p_face_table_name||'.  Are you sure that face_id exists? SQL = '||vsql||' USING '||p_face1);
           END;

           BEGIN
              EXECUTE IMMEDIATE vsql INTO vresult.FACE2 USING p_face2;
                EXCEPTION
                WHEN OTHERS THEN
                   RAISE_APPLICATION_ERROR(-20001,'Trouble selecting attribute value ('||vresult.ATTR||') for face '||p_face2||' from '||p_face_table_name||'.  Are you sure that face_id exists?  SQL = '||vsql||' USING '||p_face2);
           END;

           IF vresult.FACE1 = vresult.FACE2 THEN
              vresult.STATUS := 'OK';
           ELSE
              vresult.STATUS := 'MISMATCH';
           END IF;

           IF ( vresult.FACE1 IS NULL AND vresult.FACE2 IS NULL) THEN
              vresult.STATUS := 'OK';
           END IF;

           PIPE ROW(vresult);

       END LOOP;

   END FACE_DIFFS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION GET_TG_LAYER_ID (
      p_topology                    IN VARCHAR2,
      p_table_name                  IN VARCHAR2,
      p_column_name                 IN VARCHAR2,
      p_tg_layer_type               IN VARCHAR2 --point, line, polygon
   ) RETURN NUMBER
   AS

      --Matt! 4/27/10
      --Surely there are a million versions of this fn floating about?
      --8/3/11 moved this from GZ_CLIP to GZ_UTILITIES so I can call it from elsewhere, like merge

      psql           VARCHAR2(4000);
      tg_layer_id    NUMBER;

   BEGIN

      IF UPPER(p_tg_layer_type) NOT IN ('POINT','LINE','POLYGON')
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'What the heck is a tg_layer_type of ' || p_tg_layer_type);

      END IF;

      psql := 'SELECT a.tg_layer_id FROM '
           || 'user_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.table_name = :p2 AND '
           || 'a.column_name = :p3 AND '
           || 'a.tg_layer_type = :p4 ';

      BEGIN
         EXECUTE IMMEDIATE psql INTO tg_layer_id USING UPPER(p_topology),
                                                       UPPER(p_table_name),
                                                       UPPER(p_column_name),
                                                       UPPER(p_tg_layer_type);


      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Error! No matching records found in user_sdo_topo_metadata');
         WHEN TOO_MANY_ROWS THEN
            RAISE_APPLICATION_ERROR(-20001,'Error! Found more than one record in user_sdo_topo_metadata');
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Table user_sdo_topo_metadata does not exist! How wack is that?');
            ELSE
               RAISE;
            END IF;
      END;

      RETURN tg_layer_id;

   END GET_TG_LAYER_ID;


 -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION GET_TG_LAYER_LEVEL (
      p_topology                    IN VARCHAR2,
      p_table_name                  IN VARCHAR2,
      p_column_name                 IN VARCHAR2,
      p_tg_layer_type               IN VARCHAR2 --point, line, polygon
   ) RETURN NUMBER
   AS

      --Matt! 1/14/13
      --Should combine with get_tg_layer_id into something generic
      
      psql              VARCHAR2(4000);
      tg_layer_level    NUMBER;

   BEGIN

      IF UPPER(p_tg_layer_type) NOT IN ('POINT','LINE','POLYGON')
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'What the heck is a tg_layer_type of ' || p_tg_layer_type);

      END IF;

      psql := 'SELECT a.tg_layer_level FROM '
           || 'user_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.table_name = :p2 AND '
           || 'a.column_name = :p3 AND '
           || 'a.tg_layer_type = :p4 ';

      BEGIN
         EXECUTE IMMEDIATE psql INTO tg_layer_level USING UPPER(p_topology),
                                                          UPPER(p_table_name),
                                                          UPPER(p_column_name),
                                                          UPPER(p_tg_layer_type);


      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Error! No matching records found in user_sdo_topo_metadata');
         WHEN TOO_MANY_ROWS THEN
            RAISE_APPLICATION_ERROR(-20001,'Error! Found more than one record in user_sdo_topo_metadata');
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Table user_sdo_topo_metadata does not exist! How wack is that?');
            ELSE
               RAISE;
            END IF;
      END;

      RETURN tg_layer_level;

   END GET_TG_LAYER_LEVEL;

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
                                         GZ_UTILITIES.STRINGARRAY_TO_VARRAY(dup_ids),
                                         8;

            COMMIT;

         ELSIF p_gtype = 2002
         THEN

            EXECUTE IMMEDIATE psql USING p_tolerance,
                                         GZ_UTILITIES.STRINGARRAY_TO_VARRAY(dup_ids),
                                         6;

            COMMIT;

         END IF;


         --CHECK if we are good
         --duppsql saved above, just look at the ones in play as dups
         duppsql := duppsql || ' AND a.' || p_pkccol || ' IN (SELECT * FROM TABLE(:p3)) ';


         EXECUTE IMMEDIATE duppsql BULK COLLECT INTO post_dups USING p_tolerance,
                                                                     '13356%',
                                                                     GZ_UTILITIES.STRINGARRAY_TO_VARRAY(dup_ids);

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
                                                                    GZ_UTILITIES.STRINGARRAY_TO_VARRAY(int_ids);

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
                                                                    GZ_UTILITIES.STRINGARRAY_TO_VARRAY(int_ids2);

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

         EXECUTE IMMEDIATE psql bulk collect into int_fixed USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(int_ids),  --initial self intersects
                                                                  GZ_UTILITIES.STRINGARRAY_TO_VARRAY(int_ids3); --never fixed intersects

      END IF;

      IF p_intersect_size IS NOT NULL
      AND int_fixed.COUNT > 0
      THEN

         psql := 'SELECT a.' || p_pkccol || ' '
              || 'FROM ' || p_table_name || ' a '
              || 'WHERE a.' || p_pkccol || ' IN (SELECT * FROM TABLE(:p1)) '
              || 'AND SDO_GEOM.SDO_AREA(a.' || p_column_name || ', :p2) < :p3 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO small_fixed USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(int_fixed),
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


         EXECUTE IMMEDIATE psql BULK COLLECT INTO ratio_fixed USING GZ_UTILITIES.STRINGARRAY_TO_VARRAY(int_fixed),
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

   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------------

   FUNCTION VALIDATE_TOPOLOGY_TILE (
      p_topo               IN VARCHAR2,
      p_tiles              IN GZ_TYPES.geomarray,
      p_delta              IN NUMBER DEFAULT 0.0001,
      p_edge_count         IN NUMBER DEFAULT 100000,
      p_memory_size        IN NUMBER DEFAULT NULL,
      p_log_type           IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt!  3/2/12
      --Tile-based wrapper for VALIDATE_TOPOLOGY (see just below)
      --Caller has tiles in her hands. Compare to GZ_VALIDATE_TOPOLOGY (also below)
      --   where caller has an extent table, no tiles

      --Like sdo_topo_map.validate_topology either return TRUE or raise an error
      --Tiles must be optimized rectangles
      --This function will add additional just-to-be-safe overlap based on the delta input
      --If the caller passes in overlapping tiles, like the MBRs of states, oh well
      --Expects the caller to know something about tiles - how many, extents, etc

      retval               VARCHAR2(4000);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VALIDATE_TOPOLOGY_TILE: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_log_type IS NOT NULL
      THEN

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'VALIDATE_TOPOLOGY_TILE',
                                                p_topo,
                                                'Starting validation using ' || p_tiles.COUNT || ' tiles ');

      END IF;

      --validate our tiles

      IF p_tiles.COUNT = 0
      OR p_tiles(1).sdo_gtype IS NULL --topos have to have srids right?
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Problem with tiles - either none passed or they are empty');

      END IF;

      FOR i IN 1 .. p_tiles.COUNT
      LOOP

         IF p_tiles(i).sdo_ordinates.COUNT != 4
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Yo, I expect optimized rectangles for tiles, Im reading ordinate count '
                                            || p_tiles(i).sdo_ordinates.COUNT);

         END IF;

      END LOOP;


      --validate by tile

      FOR i IN 1 .. p_tiles.COUNT
      LOOP

         IF p_log_type IS NOT NULL
         THEN

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   'VALIDATE_TOPOLOGY_TILE',
                                                   p_topo,
                                                   'Validating tile ' || i,
                                                   NULL,NULL,NULL,NULL,NULL,NULL,
                                                   p_tiles(i));

         END IF;

         BEGIN

            retval := GZ_UTILITIES.VALIDATE_TOPOLOGY(p_topo,
                                                     p_edge_count,
                                                     p_memory_size,
                                                     (p_tiles(i).sdo_ordinates(1) - p_delta),
                                                     (p_tiles(i).sdo_ordinates(2) - p_delta),
                                                     (p_tiles(i).sdo_ordinates(3) + p_delta),
                                                     (p_tiles(i).sdo_ordinates(4) + p_delta));

         EXCEPTION
         WHEN OTHERS
         THEN

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   'VALIDATE_TOPOLOGY_TILE',
                                                   p_topo,
                                                   'Validation failure or error, see error_msg for dets ',
                                                   NULL,NULL,NULL,NULL,NULL,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

            RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);


         END;

         IF retval != 'TRUE'
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Huhs? validate_topology returned ' || retval || ' without error');

         END IF;

      END LOOP;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VALIDATE_TOPOLOGY_TILE: Peace Out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN retval;

   END VALIDATE_TOPOLOGY_TILE;

   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------------

   FUNCTION VALIDATE_TOPOLOGY (
      p_topo               IN VARCHAR2,
      p_edge_count         IN NUMBER DEFAULT NULL,
      p_memory_size        IN NUMBER DEFAULT NULL,
      p_xmin               IN NUMBER DEFAULT NULL,
      p_ymin               IN NUMBER DEFAULT NULL,
      p_xmax               IN NUMBER DEFAULT NULL,
      p_ymax               IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! and GZteam  1/12/10
      --Matt! 8/8/11 added signature for x,y bounds
      --Modifications! 9/28/12 related to memory

      --This is the lowest level topo validator
      --Knows nothing of tiles
      --Does all the java memory management voodoo

      --Should always use this wrapper in place of sdo_topo_map.validate_topology
      --sample
      --    returnval := GZ_UTILITIES.VALIDATE_TOPOLOGY('MT');

      retval         VARCHAR2(4000);
      edgekount      NUMBER;
      raise_the_roof NUMBER;
      already_tried  PLS_INTEGER := 0;
      psql           VARCHAR2(4000);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VALIDATE_TOPOLOGY: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_memory_size IS NULL
      THEN
         raise_the_roof := 2147483648;
      ELSE
         raise_the_roof := p_memory_size;
      END IF;

      IF p_edge_count IS NOT NULL
      THEN

         --check edge count to decide whether or not to raise the roof
         --I dont really bother with this any more

         psql := 'SELECT count(*) '
              || 'FROM '
              || p_topo || '_edge$ ';

         EXECUTE IMMEDIATE psql INTO edgekount;

      ELSE

         --NULLS = scary
         edgekount := 0;

      END IF;

      IF edgekount > p_edge_count
      OR p_memory_size IS NOT NULL
      THEN

         --We either have buku edges
         --or the user has an override memory setting

         SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(raise_the_roof);

         --if not, go with the default, for now

      END IF;


      BEGIN

         --return TRUE or raises and error
         IF p_xmin IS NULL
         THEN

            --all of topology
            retval := SDO_TOPO_MAP.VALIDATE_TOPOLOGY(p_topo);

         ELSE

            retval := SDO_TOPO_MAP.VALIDATE_TOPOLOGY(p_topo,
                                                     p_xmin,
                                                     p_ymin,
                                                     p_xmax,
                                                     p_ymax);

         END IF;

      EXCEPTION
      WHEN OTHERS
      THEN

         --cant use real codes for java errors

         IF UPPER(SQLERRM) LIKE '%MEMORY%'
         THEN

            --Call the java memory manager with just the error message
            --he'll pull some java rabbits out of java hats
            --freeing unused memory, clearing session states, who knows what else

            GZ_UTILITIES.JAVA_MEMORY_MANAGER(p_topo || '_edge$', --table not actually used
                                             NULL,               --sdo col not needed
                                             SQLERRM);

            --try again

            BEGIN

               IF p_xmin IS NULL
               THEN

                  retval := SDO_TOPO_MAP.VALIDATE_TOPOLOGY(p_topo);

               ELSE

                  retval := SDO_TOPO_MAP.VALIDATE_TOPOLOGY(p_topo,
                                                           p_xmin,
                                                           p_ymin,
                                                           p_xmax,
                                                           p_ymax);

               END IF;

            EXCEPTION
            WHEN OTHERS
            THEN

               IF UPPER(SQLERRM) LIKE '%MEMORY%'
               THEN

                  --caller is gonna get the memory error and will need to deal
                  RAISE;

               ELSE

                   --not valid, probably
                   --just raise, to mimic oracle topo validator
                   RAISE;

               END IF;

            END;


         ELSE

             --invalid topology
             --raise to mimic validator
             RAISE;

         END IF;


      END;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VALIDATE_TOPOLOGY: Peace Out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --valid, should return 'TRUE'
      RETURN retval;

   END VALIDATE_TOPOLOGY;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

  FUNCTION GZ_VALIDATE_TOPOLOGY (
      p_topo            IN VARCHAR2,
      p_outline_table   IN VARCHAR2,
      p_column          IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_delta           IN NUMBER DEFAULT 0.0001,
      p_log_type        IN VARCHAR2 DEFAULT NULL,
      p_tile_target     IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2
   AS


      --Matt! 8/8/11
      --Matt! 9/28/12 Made tiley and better able to handle memory errors
      --Matt! 11/15/12 added explicit tile target option.

      --This is the validator for a caller who has no tiles, or known tile count parameters
      --But does have some sort of an extent table, like a table of states

      --Called from Merge (maybe more), national validation. I have a "cookie cutter" table representing states (usually states)
      --Called from clip too, state based

      --Originally based on GZ_TOPO_BUILD.valtopo2 which may still be used by GZother modules
      --weird dependency in valtopo2 on MTUTIL schema and fsl040 table weird me out

      --sample, merge-like call
      --retval := GZ_UTILITIES.GZ_VALIDATE_TOPOLOGY('Z699LM', 'Z699LM_CUTTER');


      output            NUMBER := 1;
      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      my_cursor         SYS_REFCURSOR;
      tiles             GZ_TYPES.geomarray;
      subdivided_tiles  GZ_TYPES.geomarray;
      pxmin             NUMBER;
      pymin             NUMBER;
      pxmax             NUMBER;
      pymax             NUMBER;
      validstr          VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_VALIDATE_TOPOLOGY: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF NOT GZ_UTILITIES.GZ_TABLE_EXISTS(p_outline_table)
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Table ' || p_outline_table || ' doesnt exist ');

      END IF;

      IF p_tile_target IS NULL
      THEN

         psql := 'SELECT COUNT(*) '
              || 'FROM ' || p_outline_table || ' ';

         EXECUTE IMMEDIATE psql INTO kount;  --usually something like 50 (for a national topo in the merge)

      ELSE

         kount := p_tile_target;

      END IF;

      --gz_tile_table almost always falls short, so request 2x what we want
      --to minimize the likelihood of memory errors

      tiles := GZ_UTILITIES.GZ_TILE_TABLE(p_outline_table,
                                          (kount * 2),
                                          p_column,
                                          NULL,            --no whereclause
                                          NULL,            --no sdo filter,
                                          p_log_type,      --ex MERGE
                                          p_topo);         --log tag

      IF p_log_type IS NOT NULL
      THEN

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'GZ_VALIDATE_TOPOLOGY',
                                                p_topo,
                                                'Validating ' || tiles.COUNT || ' tiles');

      END IF;

      FOR i IN 1 .. tiles.COUNT
      LOOP

         IF tiles(i).sdo_ordinates.COUNT != 4
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Wtf, tile ordinate count is ' || tiles(i).sdo_ordinates.COUNT);

         END IF;

         --delta is in units of the data, usually degrees
         --this is the only spot where the overlap is intentionally added
         --Nothing in the tiler, or in lower level VALIDATE_TOPOLOGY

         pxmin := tiles(i).sdo_ordinates(1) - p_delta;
         pymin := tiles(i).sdo_ordinates(2) - p_delta;
         pxmax := tiles(i).sdo_ordinates(3) + p_delta;
         pymax := tiles(i).sdo_ordinates(4) + p_delta;

         IF p_log_type IS NOT NULL
         THEN

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   'GZ_VALIDATE_TOPOLOGY',
                                                   p_topo,
                                                   'Validating tile ' || i,
                                                   NULL,NULL,NULL,NULL,NULL,NULL,
                                                   tiles(i));

         END IF;


         BEGIN

            --use the memory management wrapped and rewrapped validate_topology
            --All basic memory trickery happens in this sub
            --If it cant handle memory issues, then up here at the caller we need smaller tiles


            validstr := GZ_UTILITIES.VALIDATE_TOPOLOGY(p_topo,
                                                       NULL,   --edge count, dont even bother any more
                                                       NULL,   --default memory size will override
                                                       pxmin,
                                                       pymin,
                                                       pxmax,
                                                       pymax);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%MEMORY%'
            THEN

               --Our tile is too big.  Or java is too bloated.  Its Rashomon

               --Why not use validate_topology_tile here, or in the first place?
               --This guy (GZ_VALIDATE_TOPOLOGY) has no tile info
               --He is free to tile and subdivide at will
               --validate_topology_tile is in a framework (build + output) where tile
               --counts come in as parameters

               IF p_log_type IS NOT NULL
               THEN

                  GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                         p_topo,
                                                         'GZ_VALIDATE_TOPOLOGY',
                                                         p_topo,
                                                         'Memory error (see message->) validating tile ' || i || '. Gonna subdivide this bad boy',
                                                         NULL,NULL,NULL,NULL,NULL,SQLERRM,
                                                         tiles(i));

               END IF;

               --try 25 (5x5) for now.  Desire is to do this rarely, but nail the problem when it happens
               subdivided_tiles := GZ_UTILITIES.SUBDIVIDE_TILE(tiles(i),
                                                               25);

               FOR j IN 1 .. subdivided_tiles.COUNT
               LOOP


                  pxmin := tiles(j).sdo_ordinates(1) - p_delta;
                  pymin := tiles(j).sdo_ordinates(2) - p_delta;
                  pxmax := tiles(j).sdo_ordinates(3) + p_delta;
                  pymax := tiles(j).sdo_ordinates(4) + p_delta;

                  IF p_log_type IS NOT NULL
                  THEN

                     GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                            p_topo,
                                                            'GZ_VALIDATE_TOPOLOGY',
                                                            p_topo,
                                                            'Validating tile ' || i || ' subdivided tile ' || j,
                                                            NULL,NULL,NULL,NULL,NULL,NULL,
                                                            subdivided_tiles(j));

                  END IF;

                  --allow to raise whatever, memory or invalid
                  validstr := GZ_UTILITIES.VALIDATE_TOPOLOGY(p_topo,
                                                             NULL,   --edge count, dont even bother any more
                                                             NULL,   --default memory size will override
                                                             pxmin,
                                                             pymin,
                                                             pxmax,
                                                             pymax);


               END LOOP;

               --made it
               subdivided_tiles.DELETE;


            ELSE

               RAISE_APPLICATION_ERROR(-20001,'Validation failed: ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;


         END;


         --should raise an error if theres a problem, this is just in case
         IF validstr != 'TRUE'
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Topology ' || p_topo || ' isnt valid ');

         END IF;

      END LOOP;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_VALIDATE_TOPOLOGY: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN 'TRUE';

   END GZ_VALIDATE_TOPOLOGY;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION QUERY_DELIMITED_LIST (
     p_input     IN GZ_TYPES.stringarray,
     p_query     IN VARCHAR2
   ) RETURN NUMBER DETERMINISTIC
   AS

   BEGIN

      FOR i IN 1 .. p_input.COUNT
      LOOP

         IF p_input(i) = p_query
         THEN
            RETURN i;
          END IF;

      END LOOP;

     RETURN 0;

   END QUERY_DELIMITED_LIST;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION QUERY_DELIMITED_LIST (
     p_input     IN GZ_TYPES.stringarray,
     p_query     IN NUMBER
   ) RETURN NUMBER DETERMINISTIC
   AS

   BEGIN

      FOR i IN 1 .. p_input.COUNT
      LOOP

         IF TO_NUMBER(p_input(i)) = p_query
         THEN
            RETURN i;
         END IF;

      END LOOP;

      RETURN 0;

   END QUERY_DELIMITED_LIST;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------


   PROCEDURE JAVA_MEMORY_MANAGER (
      p_table_name        IN VARCHAR2,
      p_sdo_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_error_msg         IN VARCHAR2 DEFAULT NULL,
      p_filter            IN SDO_GEOMETRY DEFAULT NULL
    ) AS

       --Matt! 10/14/11
       --Ive got a smattering of java voodoo throughout my code
       --goal is to consolidate it all here. Its all very discomforting
       --Maybe avoid java memory errors up front

       --p_table_name  Name of the table with geoms about to go into a topomap
       --p_sdo_col     Name of the geom column in p_table_name
       --p_error_msg   If you are in an error handler, pass in SQLERRM and maybe the manager can handle it
       --p_filter      If the topomap being applied to p_table_name is window-based, pass that window here


       psql             VARCHAR2(4000);
       kount            PLS_INTEGER;
       raise_the_roof   NUMBER := 2147483648;  --silly java
       myvarchar        VARCHAR2(4000);

    BEGIN


       --take a quick estimate of the total number of vertices in play


       IF p_filter IS NULL
       AND p_error_msg IS NULL
       THEN

          --if no window, take a sample
          --Get here from topo validation, assume entire edge$ table going into the black box
          --Get here from clip, where we take the entire input state table, add edges to topomap

          psql := 'SELECT SUM(SDO_UTIL.GETNUMVERTICES(' || p_sdo_col || ')) * :p1 '
               || 'FROM '
               || p_table_name || ' '
               || 'SAMPLE(5) ';   --cant use bind variable

          EXECUTE IMMEDIATE psql INTO kount USING 20;

       ELSIF p_filter IS NOT NULL
       AND p_error_msg IS NULL
       THEN

          --use the window, no sample
          --Get here from merge.  Have an entire nation of sdo, add to topology state by state

          psql := 'SELECT SUM(SDO_UTIL.GETNUMVERTICES(a.' || p_sdo_col || ')) '
               || 'FROM '
               || p_table_name || ' a '
               || 'WHERE '
               || 'SDO_FILTER(a.' || p_sdo_col || ', :p1) = :p2 ';

          EXECUTE IMMEDIATE psql INTO kount USING p_filter,
                                                  'TRUE';

       END IF;


       --XXXXcurrent evidence: default java memory runs out at around 3 million verticesXXX
       --XXXto be safe, lets raise the roof at 2 millXXX

       --In 11G Ive seen the memory error thrown at really low numbers.  Something other than
       --vertex counts may be involved
       --To be safe, Im changing this to call set_max_memory_size for just about everything but Delaware

       IF kount <= 10000
       AND p_error_msg IS NULL
       THEN

          --Nothing to do here yet
          NULL;

       ELSIF kount > 10000
       AND p_error_msg IS NULL
       THEN

          SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(raise_the_roof);

       ELSIF UPPER(p_error_msg) LIKE '%OUTOFMEMORYERROR%'
       THEN

          --this is potentially recoverable
          --this crapshoot first
          DBMS_SESSION.FREE_UNUSED_USER_MEMORY;
          --then this
          SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(raise_the_roof);

       ELSIF UPPER(p_error_msg) LIKE '%JAVA OUT OF MEMORY CONDITION%'
       THEN

          --In 11G can give this a try:
          --based on the new "two-tier" duration for java session state
          --Then raise the roof and retry

          myvarchar := DBMS_JAVA.ENDSESSION_AND_RELATED_STATE;

          IF myvarchar LIKE '%java session and permanent state ended%'
          THEN

             SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(raise_the_roof);

          ELSE

             RAISE_APPLICATION_ERROR(-20001,'Sorry bub, ENDSESSION_AND_RELATED_STATE returned this ' || myvarchar);

          END IF;

          --May want to return to this instead
          --we are probably hosed, any attempt to recover and call java code will result in:
          --"java session state cleared"
          --All tests suggest we must disconnect and reconnect

          --RAISE_APPLICATION_ERROR(-20001,'Sorry bub, we are hosed as far as I know ');


       ELSIF p_error_msg IS NOT NULL
       THEN

          --add more here smarties

          RAISE_APPLICATION_ERROR(-20001, 'Sorry, I dont know what to do with ' || p_error_msg || ' yet');

       END IF;



    END JAVA_MEMORY_MANAGER;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_REFERENCE_SCHEMAS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'REFERENCE_SCHEMAS'
   )
   AS

      --Matt! 10/21/11
      --Creates empty REFERENCE_SCHEMAS table

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_REFERENCE_SCHEMAS: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,                   --table name can be variable
                               'GZ_TYPES.REFERENCE_SCHEMAS',   --type and function fixed
                               'Y');                           --always drop.  Why else are we here?

      --no table creation garbage and error handling... all in workflow



      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(SCHEMA_NAME) '
              || ')';


      --Add triggers

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || p_table_name || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || '   IF :NEW.schema_name IS NOT NULL '
                     || '   THEN '
                     || '      :NEW.schema_name := UPPER(:NEW.schema_name); '
                     || '   END IF; '
                     || 'END;';


      --Manage privvies

      --hmm this is a bit of an existential pickle
      --lets stick public in there for now
      --I dont like having column names in this proc

      psql := 'INSERT INTO ' || p_table_name || ' '
           || '(SCHEMA_NAME, GRANTS) VALUES '
           || '(:p1,:p2) ';

      EXECUTE IMMEDIATE psql USING 'PUBLIC','SELECT';
      COMMIT;

      --execute grants on self to self
      GZ_UTILITIES.GZ_PRIV_GRANTER(p_table_name,p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_REFERENCE_SCHEMAS: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_REFERENCE_SCHEMAS;

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_REFERENCE_FACE_FIELDS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'REFERENCE_FACE_FIELDS'
   )
   AS

      --Matt! 10/21/11
      --Creates empty REFERENCE_FACE_FIELDS table

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_REFERENCE_FACE_FIELDS: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,                       --table name can be variable
                               'GZ_TYPES.REFERENCE_FACE_FIELDS',   --type and function fixed
                               'Y');                               --always drop.  Why else are we here?

      --no table creation garbage and error handling... all in workflow package



      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(RELEASE, GEN_PROJECT_ID, FIELD) '
              || ')';


      --Add triggers

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || p_table_name || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || '   IF :NEW.gen_project_id IS NOT NULL '
                     || '   THEN '
                     || '      :NEW.gen_project_id := LOWER(:NEW.gen_project_id); '  --somebody expects this, i forget who
                     || '   END IF; '
                     || 'END;';



      --Manage privvies

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_REFERENCE_FACE_FIELDS: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_REFERENCE_FACE_FIELDS;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_STATE_EDGES (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 10/21/11
      --Creates empty state edges type of table
      --MUST give it a name, theres no convention for these guys

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_STATE_EDGES: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,                       --table always variable
                               'GZ_TYPES.STATE_EDGES',             --type and function fixed
                               'Y');                               --always drop.  Why else are we here?

      --no table creation garbage and error handling... all in workflow package



      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(STEDGE_ID) '                  --this right?
              || ')';


      --Add triggers
      --NONE


      --Add indexes
      --spatial index seems polite
      --But I guess I'm unsure of the srid and tolerance at this low level


      --Manage privvies

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_STATE_EDGES: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_STATE_EDGES;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GEN_CLIP_PARAMETERS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_CLIP_PARAMETERS'
   )
   AS

      --Matt! 7/13/12
      --Backwards compatibility dealie

   BEGIN

       GZ_CLIP.CREATE_GEN_CLIP_PARAMETERS(p_schema,
                                          p_table_name);

   END CREATE_GEN_CLIP_PARAMETERS;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GEN_MERGE_PARAMETERS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_MERGE_PARAMETERS'
   )
   AS

      --Matt! 7/13/12
      --Backwards compatibility dealie

   BEGIN

       GZ_TOPO_MERGE.CREATE_GEN_MERGE_PARAMETERS(p_schema,
                                                 p_table_name);

   END CREATE_GEN_MERGE_PARAMETERS;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_LUT_LSAD (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'LUT_LSAD'
   )
   AS

      --Matt! 10/24/11
      --THIS IS NOT AN OFFICIAL decision on making this table
      --just to help me get something into the build script

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_LUT_LSAD: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,                       --table always variable
                               'GZ_TYPES.LUT_LSAD',              --type and function fixed
                               'Y');                               --always drop.  Why else are we here?

      --no table creation garbage and error handling... all in workflow package

      --Add constraints. None

      --Add triggers. None

      --Add indexes. None

      --Manage privvies

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_STATE_EDGES: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_LUT_LSAD;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_LAYERS_IN (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_IN'
   )
   AS

      --Matt! 2/06/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_IN: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,                       --table always variable
                               'GZ_TYPES.GZ_LAYERS_IN',               --type and function fixed
                               'Y');                               --always drop.  Why else are we here?

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(RELEASE, GEN_PROJECT_ID, LAYER), '
              || '   CONSTRAINT ' || p_table_name || 'TEL '
              || '      CHECK(TOPO_EXTENT_LYR IN (''Y'')) '
              || ')';


      --Add triggers

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || p_table_name || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || '   :NEW.release := UPPER(:NEW.release); '
                     || '   :NEW.gen_project_id := UPPER(:NEW.gen_project_id); '
                     || 'END;';

      --Add indexes. None

      --Manage privvies
      --workflow does to public

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_LAYERS_IN: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_IN;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_LAYER_INFO (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 2/06/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYER_INFO: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      IF p_table_name IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bud, need a table name');

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,                       --table always variable
                               'GZ_TYPES.GZ_LAYERS_IN_INFO',           --type and function fixed
                               'Y');                               --always drop.  Why else are we here?

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(LAYER) '
              || ')';


      --No triggers

      --Add indexes. None

      --Manage privvies
      --workflow does to public

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYER_INFO;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_BUILD_POLY (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 2/07/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_BUILD_POLY: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      IF p_table_name IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bud, need a table name');

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,                       --table always variable
                               'GZ_TYPES.GZ_BUILD_POLY',           --type and function fixed
                               'Y');                               --always drop.  Why else are we here?

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(SOURCE_KEY,EDGE_ID,LAYER) '
              || ')';


      --No triggers

      --Add indexes. Lets index the heck out of this table for now

      GZ_UTILITIES.ADD_INDEX(p_table_name, p_table_name || '_LYR', 'LAYER', 'BITMAP');
      GZ_UTILITIES.ADD_INDEX(p_table_name, p_table_name || '_EDG', 'EDGE_ID');

      --Manage privvies
      --workflow does to public

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_BUILD_POLY: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_BUILD_POLY;

    ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_BUILD_EDGE (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 2/13/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_BUILD_EDGE: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      IF p_table_name IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bud, need a table name');

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,                       --table always variable
                               'GZ_TYPES.GZ_BUILD_EDGE',           --type and function fixed
                               'Y');                               --always drop.  Why else are we here?

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(EDGE_ID) '
              || ')';


      --No triggers

      --Add indexes. Lets index the heck out of this table for now

      GZ_UTILITIES.ADD_INDEX(p_table_name, p_table_name || 'TN', 'TILE_NUMBER', 'BITMAP');
      GZ_UTILITIES.ADD_INDEX(p_table_name, p_table_name || 'SEI', 'SOURCE_EDGE_ID','UNIQUE');

      --Manage privvies
      --workflow does to public

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_BUILD_EDGE: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_BUILD_EDGE;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_BUILD_TILE (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 3/14/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_BUILD_TILE: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      IF p_table_name IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bud, need a table name');

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,                       --table always variable
                               'GZ_TYPES.GZ_BUILD_TILE',           --type and function fixed
                               'Y');                               --always drop.  Why else are we here?

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(tile_number) '
              || ')';


      --No triggers

      --Manage privvies
      --workflow does to public

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_BUILD_TILE: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_BUILD_TILE;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_BUILD_GEOM (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 3/19/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_BUILD_GEOM: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      IF p_table_name IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bud, need a table name');

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,                       --table always variable
                               'GZ_TYPES.GZ_BUILD_GEOM',           --type and function fixed
                               'Y');                               --always drop.  Why else are we here?

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(edge_id) '
              || ')';


      --No triggers

      --Manage privvies
      --workflow does to public

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_BUILD_GEOM: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_BUILD_GEOM;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_LAYERS_OUT (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_OUT'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_OUT: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_LAYERS_OUT',
                               'Y');                     --always drop.  Why else are we here?

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(RELEASE, GEN_PROJECT_ID, LAYER), '
              || '   CONSTRAINT ' || p_table_name || '05C '
              || '      CHECK(UPPER(LAYER_TYPE) IN (''HIERARCHICAL'',''INITIAL'',''SPLIT'',''AGGREGATE'',''SUBSET'')) '
              || ')';


      --Add triggers

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || p_table_name || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || '   :NEW.release := UPPER(:NEW.release); '
                     || '   :NEW.gen_project_id := UPPER(:NEW.gen_project_id); '
                     || '   :NEW.layer_type := UPPER(:NEW.layer_type); '
                     || '   :NEW.add_to_face := UPPER(:NEW.add_to_face); '
                     || 'END;';

      --Add indexes. None

      --Manage privvies

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_OUT: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_OUT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_LAYERS_AGGREGATE (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_AGGREGATE'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_AGGREGATE: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_LAYERS_AGGREGATE',
                               'Y');                     --always drop.  Why else are we here?

      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(RELEASE, LAYER) '
              || ')';


      --Add triggers

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || p_table_name || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || '   :NEW.release := UPPER(:NEW.release); '
                     || '   :NEW.source := UPPER(:NEW.source); '
                     || 'END;';

      --some day would be nice to check that SOURCE layers are in gz_layers_in

      --Add indexes. None

      --Manage privvies
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_AGGREGATE: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_AGGREGATE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_LAYERS_SPLIT (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_SPLIT'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_SPLIT: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_LAYERS_SPLIT',
                               'Y');                     --always drop.  Why else are we here?

      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(RELEASE, LAYER), '
              || '   CONSTRAINT ' || p_table_name || '05C '
              || '      CHECK(UPPER(CREATE_REMAINDERS) IN (''Y'',''N'')) '
              || ')';


      --Add triggers

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || p_table_name || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || '   :NEW.release := UPPER(:NEW.release); '
                     || '   :NEW.base_layer := UPPER(:NEW.base_layer); '   --can be a GZ_LAYERS_IN, ex SDUNI
                     || 'END;';

      --some day would be nice to check that some of these values have matches in related tables
      --foreign keys? never heard of them

      --Add indexes. None

      --Manage privvies
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_SPLIT: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_SPLIT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

    PROCEDURE CREATE_GZ_LAYERS_HIERARCHICAL (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_HIERARCHICAL'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_HIERARCHICAL: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_LAYERS_HIERARCHICAL',
                               'Y');                     --always drop.  Why else are we here?

      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(RELEASE, GEN_PROJECT_ID, LAYER) '
              || ')';


      --Add triggers

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || p_table_name || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || '   :NEW.release := UPPER(:NEW.release); '
                     || '   :NEW.gen_project_id := UPPER(:NEW.gen_project_id); '
                     || '   :NEW.nesting_layer := UPPER(:NEW.nesting_layer); '   --can be a GZ_LAYERS_IN, ex COUNTY
                     || 'END;';

      --some day would be nice to check that some of these values have matches in related tables
      --foreign keys? never heard of them

      --Add indexes. None

      --Manage privvies
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_HIERARCHICAL: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_HIERARCHICAL;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_LAYERS_SUBSET (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_SUBSET'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_SUBSET: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_LAYERS_SUBSET',
                               'Y');                     --always drop.  Why else are we here?

      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(RELEASE, GEN_PROJECT_ID, LAYER) '
              || ')';


      --Add triggers

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || p_table_name || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || '   :NEW.release := UPPER(:NEW.release); '
                     || '   :NEW.gen_project_id := UPPER(:NEW.gen_project_id); '
                     || '   :NEW.source := UPPER(:NEW.source); '   --can be a GZ_LAYERS_IN, ex COUNTY
                     || 'END;';

      --some day would be nice to check that some of these values have matches in related tables
      --foreign keys? never heard of them

      --Add indexes. None

      --Manage privvies
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_SUBSET: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_SUBSET;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_LAYERS_FIELDS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_FIELDS'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_SUBSET: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_LAYERS_FIELDS',
                               'Y');                     --always drop.  Why else are we here?

      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(RELEASE, LAYER), '
              || '   CONSTRAINT ' || p_table_name || '04C '
              || '      CHECK (INSTR(update_fields, update_fields_delimiter) > 0) '
              || ')';


      --Add triggers

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || p_table_name || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || '   :NEW.release := UPPER(:NEW.release); '
                     || 'END;';

      --some day would be nice to check that some of these values have matches in related tables
      --foreign keys? Sounds scary

      --Add indexes. None

      --Manage privvies
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_FIELDS: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_FIELDS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_LAYERS_CROSSWALK (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_CROSSWALK'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_CROSSWALK: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_LAYERS_CROSSWALK',
                               'Y');                     --always drop.  Why else are we here?

      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(RELEASE, OUTPUT_FIELD) '
              || ')';


      --Add triggers

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || p_table_name || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || '   :NEW.release := UPPER(:NEW.release); '
                     || 'END;';

      --some day would be nice to check that some of these values have matches in related tables
      --foreign keys? Sounds kinda spicy

      --Add indexes. None

      --Manage privvies
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_CROSSWALK: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_CROSSWALK;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   PROCEDURE CREATE_GZ_LAYERS_GEOID (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_GEOID'
   )
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_GEOID: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_LAYERS_GEOID',
                               'Y');                     --always drop.  Why else are we here?

      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(RELEASE, SUM_LEV) '
              || ')';


      --Add triggers

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || p_table_name || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || '   :NEW.release := UPPER(:NEW.release); '
                     || 'END;';

      --some day would be nice to check that some of these values have matches in related tables
      --foreign keys? No lo habla


      --Manage privvies
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_GEOID: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_GEOID;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_LAYERS_OUT_INFO (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 5/03/12
      --Work table for OUTPUT module

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_OUT_INFO: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      IF p_table_name IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bud, need a table name');

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_LAYERS_OUT_INFO',
                               'Y');

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(LAYER) '
              || ')';


      --No triggers

      --Add indexes. None

      --Manage privvies
      --workflow does to public

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_OUT_INFO: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_OUT_INFO;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_LAYERS_OUT_GEOM (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2,
      p_srid           IN NUMBER
   )
   AS

      --Matt! 01/xx/13
      --Work table for OUTPUT module

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_OUT_GEOM: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      IF p_table_name IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bud, need a table name');

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_LAYERS_OUT_GEOM',
                               'Y');

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PK '
              || '      PRIMARY KEY(oid) '
              || ')';


      --No triggers

      --Add indexes.

      GZ_UTILITIES.ADD_SPATIAL_INDEX(p_table_name,
                                     'SDOGEOMETRY',
                                     p_srid,
                                     .05);

      --Manage privvies
      --workflow does to public

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_OUT_INFO: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_OUT_GEOM;
   
    ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_LAYERS_OUT_HELP (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 01/xx/13
      --Work helper table for OUTPUT module, hierarchical layers and split layers

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_OUT_HELP: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      IF p_table_name IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bud, need a table name');

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_LAYERS_OUT_HELP',
                               'Y');

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      /*
      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PK '
              || '      PRIMARY KEY(oid_child) '
              || ')'; */


      --No triggers

      --Add indexes. Col not unique in split usage
      GZ_UTILITIES.ADD_INDEX(p_table_name, p_table_name || 'idx1', 'OID_CHILD');

      --Manage privvies
      --workflow does to public

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_LAYERS_OUT_INFO: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_LAYERS_OUT_HELP;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_FACE_MERGE (
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 11/01/12
      --Helper table for face merge

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(32);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_FACE_MERGE: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      IF p_table_name IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bud, need a table name');

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_FACE_MERGE',
                               'Y');

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(release, gen_project_id, layer), '
              || '   CONSTRAINT ' || p_table_name || '04C '
              || '      CHECK (DONT_CHECK = ''Y'') '
              || ')';


      --No triggers
      --No indexes

      --Manage privvies
      --workflow does to public

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_FACE_MERGE: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_FACE_MERGE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

    PROCEDURE GZ_RELEASE_COPIER (
      p_src_schema     IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_new_release    IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 4/24/12
      --WIP, for now just a personal helper for working on alternate topo build and alternate output

      release              VARCHAR2(4000) := UPPER(p_release);
      new_release          VARCHAR2(4000);
      legalreleasetabs     GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);
      kount                PLS_INTEGER;
      missing_stash        GZ_TYPES.stringarray;
      missing_string       VARCHAR2(4000);
      layer_type           VARCHAR2(64);
      ezcounter            PLS_INTEGER := 1;
      ezpick               PLS_INTEGER := 0;
      staging_table        VARCHAR2(64);



   BEGIN

      --checks checks checks first

      IF p_new_release IS NULL
      THEN

         new_release := UPPER(release);

      ELSE

         new_release := UPPER(p_new_release);

      END IF;

      IF LENGTH(new_release) > 64
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Check length of ' || p_release || '; elsewhere GZ code has a 64 char limit ');

      END IF;

      IF p_src_schema = SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA')
      AND release = new_release
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry boss, cant replace a release with itself in your schema');

      END IF;

      --Check to make sure that previous release exists, and if copying to new release, new release does not

      legalreleasetabs := GZ_TYPES.LEGAL_GZ_RELEASE_TABLES();

      --check source schema

      FOR i IN 1 .. legalreleasetabs.COUNT
      LOOP

         --meh, clever this up later
         IF legalreleasetabs(i) LIKE '%HIERARCHICAL'
         OR legalreleasetabs(i) LIKE '%SUBSET'
         OR legalreleasetabs(i) LIKE '%SPLIT'
         OR legalreleasetabs(i) LIKE '%AGGREGATE'
         THEN

            --These tables only have records for the release if GZ_LAYERS_OUT says so

            --gz_layers_aggregate --> AGGREGATE
            layer_type := REGEXP_REPLACE(legalreleasetabs(i),'GZ_LAYERS_','');


            --sum should be 0 for neither, or 2 for both

            psql := 'SELECT SUM(kount) FROM ('
                 || 'SELECT count(*) kount '
                 || 'FROM ' || p_src_schema || '.gz_layers_out a '
                 || 'WHERE ';

            IF layer_type = 'SUBSET'
            THEN

               psql := psql || '(a.layer_type = ''INITIAL'' OR a.layer_type = :p1) AND ';

            ELSE

               psql := psql || 'a.layer_type = :p1 AND ';

            END IF;

            psql := psql
                 || 'a.release = :p2 AND '
                 || 'rownum = 1 '
                 || 'UNION ALL '
                 || 'SELECT COUNT(*) kount '
                 || 'FROM ' || p_src_schema || '.' || legalreleasetabs(i) || ' a '
                 || 'WHERE '
                 || 'a.release = :p3 AND '
                 || 'rownum = 1 '
                 || ')';


            EXECUTE IMMEDIATE psql INTO kount USING layer_type,
                                                    release,
                                                    release;

            IF kount <> 0
            AND kount <> 2
            THEN

               missing_stash(missing_stash.COUNT + 1) := legalreleasetabs(i);

            END IF;

         ELSIF legalreleasetabs(i) IN ('QA_PARAMETERS')
         THEN

            --these tables are feral critters
            NULL;

         ELSE

            --These tables should always have a record or more for a release

            psql := 'SELECT COUNT(*) '
                 || 'FROM ' || p_src_schema || '.' || legalreleasetabs(i) || ' a '
                 || 'WHERE a.release = :p1 AND '
                 || 'rownum = 1 ';

            BEGIN

               EXECUTE IMMEDIATE psql INTO kount USING release;

            EXCEPTION
            WHEN OTHERS THEN

               IF SQLERRM LIKE '%table or view does not exist%'
               THEN

                  missing_stash(missing_stash.COUNT + 1) := legalreleasetabs(i);

               ELSE

                  RAISE;

               END IF;

            END;

            IF kount != 1
            THEN

               --check them all before reporting back
               missing_stash(missing_stash.COUNT + 1) := legalreleasetabs(i);

            END IF;

         END IF;

      END LOOP;


      IF missing_stash.COUNT > 0
      THEN

         FOR i IN 1 .. missing_stash.COUNT
         LOOP

            missing_string := missing_string || ' ' || missing_stash(i);

         END LOOP;

         RAISE_APPLICATION_ERROR(-20001,'Dude, release ' || release || ' '
                                     || 'doesnt exist or is extraneous in ' || p_src_schema || ' table(s) ' || missing_string || '.');

      END IF;

      missing_stash.DELETE;
      missing_string := '';

      --check destination schema

      FOR i IN 1 .. legalreleasetabs.COUNT
      LOOP

         psql := 'SELECT COUNT(*) '
              || 'FROM ' || legalreleasetabs(i) || ' a '
              || 'WHERE a.release = :p1 AND '
              || 'rownum = 1 ';

         BEGIN

            EXECUTE IMMEDIATE psql INTO kount USING new_release;

         EXCEPTION
         WHEN OTHERS THEN

            IF SQLERRM LIKE '%table or view does not exist%'
            THEN

               RAISE_APPLICATION_ERROR(-20001, 'Dude, you dont have a ' || legalreleasetabs(i) || ' table ');

            ELSE

               RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' from ' || psql);

            END IF;

         END;

         IF kount != 0
         THEN

            --check them all before reporting back
            missing_stash(missing_stash.COUNT + 1) := legalreleasetabs(i);

         END IF;


      END LOOP;

      IF missing_stash.COUNT > 0
      THEN

         FOR i IN 1 .. missing_stash.COUNT
         LOOP

            missing_string := missing_string || ' ' || missing_stash(i);

         END LOOP;

         RAISE_APPLICATION_ERROR(-20001,'Dude, release ' || new_release || ' '
                                     || 'already exists in these tables of yours-> ' || missing_string || ' <- '
                                     || 'Use the GZ_RELEASE_DELETER ');

      END IF;

      ------------------
      --any more checks?
      ------------------

      FOR i IN 1 .. legalreleasetabs.COUNT
      LOOP

         IF release <> new_release
         THEN

            --make temp staging table
            --this is either a remote schema or own schema

            ezcounter := 1;
            FOR j in 1 .. 99
            LOOP

               EXIT WHEN ezcounter = 0;

               psql := 'SELECT count(*) FROM USER_TABLES '
                    || 'WHERE TABLE_NAME = :p1';
               EXECUTE IMMEDIATE psql INTO ezcounter USING legalreleasetabs(i) || '_EZ' || j;

               IF ezcounter = 0
               THEN
                  ezpick := j;
               END IF;

            END LOOP;

            staging_table := legalreleasetabs(i) || '_EZ' || ezpick;

            --create temp table from source. Could be own schema if changing release codes

            psql := 'CREATE TABLE ' || staging_table || ' '
                 || 'AS SELECT * FROM ' || p_src_schema || '.' || legalreleasetabs(i) || ' a '
                 || 'WHERE a.release = ''' || release || ''' ';   --no binds in DDL

            EXECUTE IMMEDIATE psql;

            psql := 'UPDATE ' || staging_table || ' a '
                 || 'SET a.release = :p1 ';

            EXECUTE IMMEDIATE psql USING new_release;
            COMMIT;

         ELSE

            --this is ALWAYS a remote schema with no release code change
            staging_table :=  p_src_schema || '.' || legalreleasetabs(i);

         END IF;


         psql := 'INSERT INTO ' || legalreleasetabs(i) || ' '
              || 'SELECT * FROM ' || staging_table || ' '
              || 'WHERE release = ''' || new_release || ''' ';

         dbms_output.put_line(psql);

         BEGIN

            EXECUTE IMMEDIATE psql;
            COMMIT;

         EXCEPTION
         WHEN OTHERS
         THEN

            RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' on ' || psql);

         END;

         IF release <> new_release
         THEN

            EXECUTE IMMEDIATE 'DROP TABLE ' || staging_table || ' PURGE ';

         END IF;


      END LOOP;


   END GZ_RELEASE_COPIER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE GZ_RELEASE_DELETER (
      p_release        IN VARCHAR2
   )
   AS

      --Matt! 4/26/12
      --WIP, for now just a personal helper for working on alternate topo build and alternate output

      legalreleasetabs     GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);
      kount                PLS_INTEGER;

   BEGIN

      --checks

      --should at least be in gz_layers_out and gz_layers_in right?

      psql := 'SELECT SUM(kount) FROM ( '
           || 'SELECT COUNT(*) kount FROM gz_layers_out '
           || 'WHERE release = :p1 AND '
           || 'rownum = 1 '
           || 'UNION ALL '
           || 'SELECT COUNT(*) kount FROM gz_layers_in '
           || 'WHERE release = :p2 AND '
           || 'rownum = 1 '
           || ') ';

      EXECUTE IMMEDIATE psql INTO kount USING UPPER(p_release),
                                              UPPER(p_release);

      IF kount <> 2
      THEN

         RAISE_APPLICATION_ERROR(-20001,'I dont see release ' || p_release || ' in either gz_layers_in or gz_layers_out '
                                     || 'Maybe you just want to wipe out some other layers junk?  Lets remove this error ');

      END IF;

      legalreleasetabs := GZ_TYPES.LEGAL_GZ_RELEASE_TABLES();


      FOR i IN 1 .. legalreleasetabs.COUNT
      LOOP

         psql := 'DELETE FROM ' || legalreleasetabs(i) || ' a '
              || 'WHERE a.release = :p1 ';

         EXECUTE IMMEDIATE psql USING UPPER(p_release);
         COMMIT;

      END LOOP;


   END GZ_RELEASE_DELETER;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   PROCEDURE GZ_LAYER_COPIER (
      p_src_schema         IN VARCHAR2,
      p_src_release        IN VARCHAR2,
      p_src_project_id     IN VARCHAR2,
      p_src_layer          IN VARCHAR2,
      p_dest_release       IN VARCHAR2,
      p_dest_project_id    IN VARCHAR2,
      p_dest_layer         IN VARCHAR2,
      p_src_x_out          IN VARCHAR2 DEFAULT NULL,
      p_clean_house        IN VARCHAR2 DEFAULT 'N'
   )
   AS

      --Matt! 9/04/12

      --p_clean_house means copier has permission to delete any conflicts in the destination

      --WIP WIP WIP WIP

      psql              VARCHAR2(4000);
      psql2             VARCHAR2(4000);
      temp_tab_out      VARCHAR2(30);
      temp_tab_type     VARCHAR2(30);
      temp_tab_geoid    VARCHAR2(30);
      temp_tab_fields   VARCHAR2(30);
      temp_tab_xwalk    VARCHAR2(30);
      layer_type        VARCHAR2(64);
      fieldz            GZ_TYPES.stringarray;
      kount             PLS_INTEGER;
      new_kount         PLS_INTEGER := 0;


   BEGIN

      ----------------------
      --Cleanup
      ----------------------


      temp_tab_out    := 'GZ_LAYERS_OUT_' || p_dest_layer;
      temp_tab_type   := 'GZ_LAYERS_TYPE_' || p_dest_layer;
      temp_tab_geoid  := 'GZ_LAYERS_GEOID_' || p_dest_layer;
      temp_tab_fields := 'GZ_LAYERS_FIELDS_' || p_dest_layer;
      temp_tab_xwalk  := 'GZ_LAYERS_XWALK_' || p_dest_layer;


      BEGIN
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || temp_tab_out;
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || temp_tab_type;
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || temp_tab_geoid;
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || temp_tab_fields;
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || temp_tab_xwalk;
      EXCEPTION
      WHEN OTHERS
      THEN

         NULL;

      END;


      ----------------------
      --GZ_LAYERS_OUT
      ----------------------

      psql := 'CREATE GLOBAL TEMPORARY TABLE ' || temp_tab_out || ' '
           || 'ON COMMIT PRESERVE ROWS '
           || 'AS '
           || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_OUT a '
           || 'WHERE '
           || 'a.release = ''' || p_src_release || p_src_x_out || ''' AND '  --assumption x out only here
           || 'a.gen_project_id = ''' || p_src_project_id || ''' AND '
           || 'a.layer = ''' || p_src_layer || ''' ';

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -955
         THEN

            psql := 'INSERT INTO ' || temp_tab_out || ' '
                 || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_OUT a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || p_src_x_out || ''' AND '  --assumption x out only here
                 || 'a.gen_project_id = ''' || p_src_project_id || ''' AND '
                 || 'a.layer = ''' || p_src_layer || ''' ';

            EXECUTE IMMEDIATE psql;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ON ' || psql);

         END IF;

      END;

      --verify
      psql2 := 'SELECT COUNT(*) FROM ' || temp_tab_out;
      EXECUTE IMMEDIATE psql2 INTO kount;

      IF kount = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Didnt get any records from ' || psql);

      END IF;

      --update all for simplicity

      psql := 'UPDATE ' || temp_tab_out || ' a '
           || 'SET '
           || 'a.release = :p1, '
           || 'a.gen_project_id = :p2, '
           || 'a.layer = :p3 ';

      EXECUTE IMMEDIATE psql USING p_dest_release,
                                   p_dest_project_id,
                                   p_dest_layer;


      psql := 'SELECT layer_type FROM ' || temp_tab_out;
      EXECUTE IMMEDIATE psql INTO layer_type;

      IF p_clean_house = 'Y'
      THEN

         psql := 'DELETE FROM gz_layers_out a '
              || 'WHERE '
              || 'a.release = :p1 AND '
              || 'a.gen_project_id = :p2 AND '
              || 'a.layer = :p3 ';

         EXECUTE IMMEDIATE psql USING p_dest_release,
                                      p_dest_project_id,
                                      p_dest_layer;

      END IF;

      psql := 'INSERT INTO gz_layers_out '
           || 'SELECT * FROM ' || temp_tab_out || ' ';

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLERRM LIKE ('%unique constraint%')
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Primary key violation inserting into gz_layers_out. '
                                        || ' Consider the p_clean_house flag. ' || SQLERRM || ' on ' || psql);

         ELSE

            RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' on ' || psql);

         END IF;

      END;

      COMMIT;


      IF layer_type NOT IN ('INITIAL','SUBSET','SPLIT','HIERARCHICAL','AGGREGATE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'WTW is layer type ' || layer_type);

      END IF;

      ----------------------
      --GZ_LAYERS_<LAYER_TYPE>
      ----------------------

      IF layer_type = 'INITIAL'
      THEN

         layer_type := 'SUBSET';

      END IF;

      psql := 'CREATE GLOBAL TEMPORARY TABLE ' || temp_tab_type || ' '
           || 'ON COMMIT PRESERVE ROWS '
           || 'AS '
           || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_' || layer_type || ' a '
           || 'WHERE '
           || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
           || 'a.layer = ''' || p_src_layer || ''' ';

           IF layer_type NOT IN ('SPLIT','AGGREGATE')
           THEN

              psql := psql || 'AND a.gen_project_id = ''' || p_src_project_id || ''' ';

           END IF;

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -955
         THEN

            psql := 'INSERT INTO ' || temp_tab_type || ' '
                 || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_' || layer_type || ' a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                 || 'a.layer = ''' || p_src_layer || ''' ';

            IF layer_type NOT IN ('SPLIT','AGGREGATE')
            THEN

               psql := psql || 'AND a.gen_project_id = ''' || p_src_project_id || ''' ';

            END IF;

            EXECUTE IMMEDIATE psql;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ON ' || psql);

         END IF;

      END;

      --update all for simplicity

      psql := 'UPDATE ' || temp_tab_type || ' a '
           || 'SET '
           || 'a.release = :p1, '
           || 'a.layer = :p2 ';

      IF layer_type NOT IN ('SPLIT','AGGREGATE')
      THEN

         psql := psql || ', a.gen_project_id = ''' || p_dest_project_id || ''' ';

      END IF;


      EXECUTE IMMEDIATE psql USING p_dest_release,
                                   p_dest_layer;

      IF p_clean_house = 'Y'
      THEN

         psql := 'DELETE FROM gz_layers_' || layer_type || ' a '
              || 'WHERE '
              || 'a.release = :p1 AND '
              || 'a.layer = :p2 ';

         IF layer_type NOT IN ('SPLIT','AGGREGATE')
         THEN

            psql := psql || 'AND a.gen_project_id = ''' || p_dest_project_id || ''' ';

         END IF;

         EXECUTE IMMEDIATE psql USING p_dest_release,
                                      p_dest_layer;

      END IF;

      psql := 'INSERT INTO  GZ_LAYERS_' || layer_type || ' '
           || 'SELECT * FROM ' || temp_tab_type;

      EXECUTE IMMEDIATE psql;
      COMMIT;


      ----------------------
      --GZ_LAYERS_GEOID
      ----------------------

      psql := 'CREATE GLOBAL TEMPORARY TABLE ' || temp_tab_geoid || ' '
           || 'ON COMMIT PRESERVE ROWS '
           || 'AS '
           || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_GEOID a '
           || 'WHERE '
           || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
           || 'a.sum_lev = ''' || p_src_layer || ''' ';

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -955
         THEN

            psql := 'INSERT INTO ' || temp_tab_geoid || ' '
                 || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_GEOID a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                 || 'a.sum_lev = ''' || p_src_layer || ''' ';

            EXECUTE IMMEDIATE psql;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ON ' || psql);

         END IF;

      END;

      psql := 'UPDATE ' || temp_tab_geoid || ' a '
           || 'SET '
           || 'a.release = :p1, '
           || 'a.sum_lev = :p2 ';

      EXECUTE IMMEDIATE psql USING p_dest_release,
                                   p_dest_layer;

      IF p_clean_house = 'Y'
      THEN

         psql := 'DELETE FROM gz_layers_geoid a '
              || 'WHERE '
              || 'a.release = :p1 AND '
              || 'a.sum_lev = :p2 ';


         EXECUTE IMMEDIATE psql USING p_dest_release,
                                      p_dest_layer;

      END IF;

      psql := 'INSERT INTO GZ_LAYERS_GEOID '
           || 'SELECT * FROM ' || temp_tab_geoid;

      EXECUTE IMMEDIATE psql;
      COMMIT;


      ----------------------
      --GZ_LAYERS_FIELDS
      ----------------------

      psql := 'CREATE GLOBAL TEMPORARY TABLE ' || temp_tab_fields || ' '
           || 'ON COMMIT PRESERVE ROWS '
           || 'AS SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_FIELDS a '
           || 'WHERE '
           || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
           || 'a.layer = ''' || p_src_layer || ''' ';

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -955
         THEN

            psql := 'INSERT INTO ' || temp_tab_fields || ' '
                 || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_GEOID a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                 || 'a.sum_lev = ''' || p_src_layer || ''' ';

            EXECUTE IMMEDIATE psql;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ON ' || psql);

         END IF;

      END;

      psql := 'UPDATE ' || temp_tab_fields || ' a '
           || 'SET '
           || 'a.release = :p1, '
           || 'a.layer = :p2 ';

      EXECUTE IMMEDIATE psql USING p_dest_release,
                                   p_dest_layer;

      IF p_clean_house = 'Y'
      THEN

         psql := 'DELETE FROM gz_layers_fields a '
              || 'WHERE '
              || 'a.release = :p1 AND '
              || 'a.layer = :p2 ';

         EXECUTE IMMEDIATE psql USING p_dest_release,
                                      p_dest_layer;

      END IF;

      psql := 'INSERT INTO GZ_LAYERS_FIELDS '
           || 'SELECT * FROM ' || temp_tab_fields;

      EXECUTE IMMEDIATE psql;
      COMMIT;


      ----------------------
      --GZ_LAYERS_CROSSWALK - only add the missing
      ----------------------

      fieldz := GZ_OUTPUT.GET_FIELDS(p_dest_release,
                                     p_dest_layer);


      FOR i IN 1 .. fieldz.COUNT
      LOOP

         psql := 'SELECT COUNT(*) FROM '
              || 'GZ_LAYERS_CROSSWALK a '
              || 'WHERE '
              || 'a.release = :p1 AND '
              || 'a.output_field = :p2 ';

         EXECUTE IMMEDIATE psql INTO kount USING p_dest_release,
                                                 fieldz(i);

         IF kount = 0
         AND new_kount = 0
         THEN

            new_kount := new_kount + 1;

            psql := 'CREATE GLOBAL TEMPORARY TABLE ' || temp_tab_xwalk || ' '
                 || 'ON COMMIT PRESERVE ROWS '
                 || 'AS SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_CROSSWALK a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                 || 'a.output_field = ''' || fieldz(i) || ''' ';

            BEGIN

               EXECUTE IMMEDIATE psql;

            EXCEPTION
            WHEN OTHERS
            THEN

               IF SQLCODE = -955
               THEN

                  psql := 'INSERT INTO ' || temp_tab_xwalk || ' '
                       || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_CROSSWALK a '
                       || 'WHERE '
                       || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                       || 'a.output_field = ''' || fieldz(i) || ''' ';

                 EXECUTE IMMEDIATE psql;

               ELSE

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ON ' || psql);

               END IF;

            END;

         ELSIF kount = 0
         AND new_kount > 0
         THEN

            psql := 'INSERT INTO ' || temp_tab_xwalk || ' '
                 || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_CROSSWALK a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                 || 'a.output_field = ''' || fieldz(i) || ''' ';

            EXECUTE IMMEDIATE psql;

         END IF;

      END LOOP;

      IF new_kount > 0
      THEN

         psql := 'UPDATE ' || temp_tab_xwalk || ' a '
              || 'SET a.release = :p1 ';

         EXECUTE IMMEDIATE psql USING p_dest_release;

         psql := 'INSERT INTO GZ_LAYERS_CROSSWALK '
              || 'SELECT * FROM ' || temp_tab_xwalk;

         EXECUTE IMMEDIATE psql;
         COMMIT;

      END IF;


   END GZ_LAYER_COPIER;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION COPY_TO_X (
      p_tablename     IN VARCHAR2,
      p_xxx           IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

    --Matt! Added and modified this guy from the original 10/24/11

    psql     VARCHAR2(4000);
    pcounter NUMBER;
    xspot     PLS_INTEGER;

   BEGIN

      IF p_xxx IS NOT NULL
      AND LENGTH(p_xxx) > 7
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Length of character string ' || p_xxx || ' is too long. Sorry, 7 char limit');
      END IF;

      psql := 'SELECT COUNT(*) '
           || 'FROM USER_TABLES a '
           || 'WHERE '
           || 'a.table_name = :p1 ';

       EXECUTE IMMEDIATE psql INTO pcounter USING UPPER(p_tablename);

       IF pcounter = 1
       THEN

          xspot := 1;
          <<topper>>

          IF xspot > 99
          THEN
             RAISE_APPLICATION_ERROR(-20001,'PROBLEM CANNOT GET A PROPER TEMP TABLE NAME FOR PROCESS!');
          END IF;

          psql := 'SELECT COUNT(*) '
               || 'FROM USER_TABLES a '
               || 'WHERE '
               || 'a.table_name = :p1 ';

          IF p_xxx IS NULL
          THEN
             EXECUTE IMMEDIATE psql INTO pcounter USING UPPER(p_tablename) || '_X' || TO_CHAR(xspot);
          ELSE
             EXECUTE IMMEDIATE psql INTO pcounter USING UPPER(p_tablename) || '_' || p_xxx || '_X' || TO_CHAR(xspot);
          END IF;

          IF pcounter > 0
          THEN
             xspot := xspot + 1;
             GOTO topper;
          END IF;

          IF p_xxx IS NULL
          THEN

             psql := 'CREATE TABLE ' || p_tablename || '_X' || TO_CHAR(xspot) || ' '
                  || 'AS SELECT * FROM ' || p_tablename;

             EXECUTE IMMEDIATE psql;

             RETURN p_tablename || '_X' || TO_CHAR(xspot);

          ELSE

             psql := 'CREATE TABLE ' || p_tablename || '_' || p_xxx || '_X' || TO_CHAR(xspot) || ' '
                  || 'AS SELECT * FROM ' || p_tablename;

             EXECUTE IMMEDIATE psql;

             RETURN p_tablename || '_' || p_xxx || '_X' || TO_CHAR(xspot);

           END IF;

       ELSE

          RETURN NULL;

       END IF;

   END COPY_TO_X;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE COPY_TO_X (
      p_tablename     IN VARCHAR2,
      p_xxx           IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 3/23/12
      tabname_whatevs VARCHAR2(32);

   BEGIN

      tabname_whatevs := COPY_TO_X(p_tablename, p_xxx);

   END;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------



   PROCEDURE BACKUP_GZ_TABLES (
      p_schema          IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 10/24/11

      tabs              GZ_TYPES.stringarray;
      newtab            VARCHAR2(4000);

   BEGIN

      --get table list
      tabs := GZ_TYPES.LEGAL_GZ_ALL_TABLES;

      FOR i IN 1 .. tabs.COUNT
      LOOP

         BEGIN

            newtab := GZ_UTILITIES.COPY_TO_X(tabs(i));

         EXCEPTION
         WHEN OTHERS
         THEN

            --no confidence in this system at this stage
            --user may be missing a few
            NULL;

         END;

      END LOOP;


   END BACKUP_GZ_TABLES;



   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_TABLES (
      p_drop            IN VARCHAR2 DEFAULT 'Y',
      p_new_schema      IN VARCHAR2 DEFAULT 'N'   --Y = fresh "other" (non-release-aware) tables too
   )
   AS

      --Matt! 10/24/11
      --Another attempt! 7/13/12

      -- 1. Drop all GZ release-aware tables if requested
      -- 2. Then create ALL (release-aware, others, setup, etc) GZ Tables that don't exist


      release_tables       GZ_TYPES.stringarray;
      other_tables         GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);


   BEGIN

      IF p_drop = 'N' AND p_new_schema = 'Y'
      THEN

         RAISE_APPLICATION_ERROR(-20001,'I kinda know what you mean, but N drop tables and Y wipe out a fresh schema '
                                     || 'doesnt compute in the procedure ');

      END IF;


      release_tables := GZ_TYPES.LEGAL_GZ_RELEASE_TABLES();
      other_tables   := GZ_TYPES.LEGAL_GZ_OTHER_TABLES();

      IF p_drop = 'Y'
      THEN

         FOR i IN 1 .. release_tables.COUNT
         LOOP

            GZ_UTILITIES.GZ_DROP_TABLE(release_tables(i));

         END LOOP;

         IF p_new_schema = 'Y'
         THEN

            --only kill off the special tables if we know this a brand new schema
            --and we are building up from scratch
            --most importantly, our reference_schemas permissions on the new tables will be missing

            FOR i IN 1 .. other_tables.COUNT
            LOOP

               GZ_UTILITIES.GZ_DROP_TABLE(other_tables(i));

            END LOOP;

         END IF;


      END IF;

      ---------------------------------------------------------------
      --make the "others" first
      --reference_schemas must exist for the "release" table builders
      ---------------------------------------------------------------

      FOR i IN 1 .. other_tables.COUNT
      LOOP

         IF (p_drop = 'Y' AND p_new_schema = 'Y')
         OR NOT GZ_UTILITIES.GZ_TABLE_EXISTS(other_tables(i))
         THEN

            psql := 'BEGIN GZ_UTILITIES.CREATE_' || other_tables(i) || '(); END; ';

            --dbms_output.put_line(psql);

            EXECUTE IMMEDIATE psql;

         END IF;

      END LOOP;


      ---------------------------------------------------------------
      --make the release-aware tables second
      --First pass: Attempt to build all tables using CREATE_<TABLE_NAME>
      --reference_schemas must exist for the "release" table builders
      ---------------------------------------------------------------

      FOR i IN 1 .. release_tables.COUNT
      LOOP

         IF p_drop = 'Y'
         OR NOT GZ_UTILITIES.GZ_TABLE_EXISTS(release_tables(i))
         THEN

             psql := 'BEGIN GZ_UTILITIES.CREATE_' || release_tables(i) || '(); END; ';

             --dbms_output.put_line(psql);

             BEGIN

                EXECUTE IMMEDIATE psql;

             EXCEPTION
             WHEN OTHERS
             THEN

                IF SQLERRM LIKE '%must be declared%'
                OR SQLERRM LIKE '%too long%'
                THEN

                   --PLS-00302: component 'CREATE_GZ_JOB_SETUP' must be declared
                   --PLS-00114: identifier 'CREATE_GZ_BUILD_SOURCE_PARAMETERS' too long
                   --NP, these should all be tables not built gz_utilities-style.  We will catch them in a minute
                   NULL;

                ELSE

                   RAISE_APPLICATION_ERROR(-20001, 'Creating table ' || release_tables(i) || ' with-  ' || psql
                                                || '  -threw ' || SQLERRM);

                END IF;

             END;

         END IF;

      END LOOP;


      --Create setup tables
      --no drop option, if they exist already they will be unharmed
      GZ_WORKFLOW.CREATE_EMPTY_SETUP_TBLS();

      --create all other empty parameter tables
      --no drop flag, will not touch the ones created above if same same
      GZ_WORKFLOW.CREATE_EMPTY_PRM_TBLS();

   END CREATE_GZ_TABLES;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE REBUILD_GZ_TABLES
   AS

      --Matt! 7/13/12

      all_tables           GZ_TYPES.stringarray;
      backup_tables        GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);

   BEGIN

      all_tables := GZ_TYPES.LEGAL_GZ_ALL_TABLES;

      FOR i IN 1 .. all_tables.COUNT
      LOOP

         BEGIN

            backup_tables(i) := GZ_UTILITIES.COPY_TO_X(all_tables(i));

         EXCEPTION
         WHEN OTHERS
         THEN

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' on GZ_UTILITIES.COPY_TO_X ' || all_tables(i));

         END;

      END LOOP;

      GZ_UTILITIES.CREATE_GZ_TABLES('Y','Y');  --all flags to drop

      FOR i IN 1 .. backup_tables.COUNT
      LOOP

         --reference schemas is special, it autofills itself with public
         IF all_tables(i) = 'REFERENCE_SCHEMAS'
         THEN

            EXECUTE IMMEDIATE 'DELETE FROM ' || all_tables(i);

         END IF;

         psql := 'INSERT INTO ' || all_tables(i) || ' '
              || 'SELECT * FROM ' || backup_tables(i) || ' ';

         BEGIN

            EXECUTE IMMEDIATE psql;

         EXCEPTION
         WHEN OTHERS THEN

            IF SQLCODE = -947 --ORA-00947: not enough values
            THEN

               RAISE_APPLICATION_ERROR(-20001, 'Not enough values on ' || psql );

            ELSE

               RAISE_APPLICATION_ERROR(-20001, 'Error ' || SQLERRM || ' on ' || psql);

            END IF;

         END;


         COMMIT;

         GZ_UTILITIES.GZ_DROP_TABLE(backup_tables(i));

      END LOOP;

      --we may have lost our privvies when the reference_schemas table was rebuilt
      --Grant privs on all based on whats in the final rebuilt reference_schemas

      FOR i IN 1 .. all_tables.COUNT
      LOOP

         GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',all_tables(i));

      END LOOP;


   END REBUILD_GZ_TABLES;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE COPY_GZ_TABLES (
      p_srcschema       IN VARCHAR2,
      p_myschema        IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 10/24/11
      --Copy the contents of some other schemas worth of parameter tables to me
      --All known tables

      psql              VARCHAR2(4000);
      tabs              GZ_TYPES.stringarray;
      v_schema          VARCHAR2(4000);

   BEGIN

      IF p_myschema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_myschema);

      END IF;

      tabs := GZ_TYPES.LEGAL_GZ_ALL_TABLES;

      FOR i IN 1 .. tabs.COUNT
      LOOP

         --assumption1 is that the source schema has this table, and its populated
         --we will call this assumption a hope, and allow it to slide if false
         --assumption2 is that my schema has the table and its empty - lets enforce this

         --have no fear, we backed this guy up
         --also need to do this for reference_schemas which autofills with public
         psql := 'DELETE FROM ' || tabs(i);

         BEGIN

            EXECUTE IMMEDIATE psql;

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -942
            THEN

               --table or view does not exist
               RAISE_APPLICATION_ERROR(-20001,'Table ' || tabs(i) || ' doesnt exist in your schema. Cant copy from other schema ');

            ELSE

               RAISE;

            END IF;

         END;

         COMMIT;

         psql := 'INSERT /*+ APPEND */ INTO '
               || v_schema || '.' || tabs(i) || ' '
               || 'SELECT * FROM ' || p_srcschema || '.' || tabs(i) || ' ';

         BEGIN

            EXECUTE IMMEDIATE psql;

         EXCEPTION
         WHEN OTHERS
         THEN
            --meh. for now
            NULL;
         END;

         COMMIT;

      END LOOP;

   END COPY_GZ_TABLES;

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
   --Public---------------------------------------------------------------------------------

   FUNCTION NEW_SMALL_POLY_RSUV RETURN GZ_TYPES.SMALL_POLY_RSUV PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_SMALL_POLY_RSUV;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE CREATE_SMALL_POLY_RSUV (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 1/24/12

      psql          VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_SMALL_POLY_RSUV: Start');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      psql := 'CREATE TABLE ' || p_table_name || ' ';

      --RSUV is relegated to the utilities wilderness

      psql := psql || 'NOPARALLEL NOLOGGING AS '
                   || 'SELECT * FROM TABLE(' || p_schema || '.GZ_UTILITIES.NEW_SMALL_POLY_RSUV ) ';

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


      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_SMALL_POLY_RSUV: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


   END CREATE_SMALL_POLY_RSUV;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION PIPE_EDGE_VERTICES (
      p_tab_name           IN VARCHAR2,
      p_sdo_col            IN VARCHAR2 DEFAULT 'SDOGEOMETRY'
   ) RETURN GZ_TYPES.SMALL_POLY_RSUV PIPELINED
   AS

      --Matt! 12/14/11
      --WIP, not polished, not production-ready, etc
      --buddy to REMOVE_SP_UNIVERSAL_VERTICES below

      output         GZ_TYPES.RSUV_REC;
      kounter        PLS_INTEGER := 0;
      my_cursor      SYS_REFCURSOR;
      psql           VARCHAR2(4000);
      sdo_recs       GZ_TYPES.geomarray;
      this_pt        SDO_GEOMETRY;

   BEGIN

      psql := 'SELECT a.' || p_sdo_col || ' '
           || 'FROM '
           || p_tab_name || ' a ';

      OPEN my_cursor FOR psql;

      LOOP

         FETCH my_cursor BULK COLLECT INTO sdo_recs LIMIT 25;
         EXIT WHEN sdo_recs.COUNT = 0;

         FOR i IN 1 .. sdo_recs.COUNT
         LOOP

            FOR j IN 1 .. sdo_recs(i).sdo_ordinates.COUNT/2
            LOOP

               --build and pipe the ordinate
               this_pt := SDO_GEOMETRY(2001,
                                       sdo_recs(i).sdo_srid,
                                       SDO_POINT_TYPE(sdo_recs(i).sdo_ordinates((j * 2) - 1),
                                                      sdo_recs(i).sdo_ordinates((j * 2)),
                                                      NULL),
                                       NULL,NULL);

               kounter := kounter + 1;

               output.id := kounter;
               output.sdogeometry := this_pt;

               PIPE ROW(output);

            END LOOP;

         END LOOP;

      END LOOP;

      CLOSE my_cursor;

   END PIPE_EDGE_VERTICES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION AXE_UNMATCHED_VTXS (
      p_topo               IN VARCHAR2,
      p_match_tab          IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN NUMBER
   AS

      --Matt! 12/14/11
      --WIP, not polished, not production-ready, etc
      --buddy to REMOVE_SP_UNIVERSAL_VERTICES below

      --returns the total number of vertices it thinks it removed on this edge

      vtx_kount            PLS_INTEGER := 0;
      edge_geom            SDO_GEOMETRY;
      temp_pt              SDO_GEOMETRY;
      new_ordinates        MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
      new_edge_geom        SDO_GEOMETRY;
      ordkount             PLS_INTEGER := 0;
      psql                 VARCHAR2(4000);
      relate_kount         PLS_INTEGER;

   BEGIN


      --switch package
      edge_geom := GZ_CLIP.GET_EDGE_GEOMETRY(p_topo,
                                            p_edge_id);


      --I guess its easier to do the entire edge at once?

      FOR i IN 1 .. edge_geom.sdo_ordinates.COUNT/2
      LOOP

        IF i = 1
        OR i = edge_geom.sdo_ordinates.COUNT/2
        THEN

           --dont touch the nodes

           new_ordinates.EXTEND(2);

           ordkount := ordkount + 1;
           new_ordinates(ordkount) := edge_geom.sdo_ordinates((i * 2) - 1);
           ordkount := ordkount + 1;
           new_ordinates(ordkount) := edge_geom.sdo_ordinates((i * 2));

        ELSE

           temp_pt := SDO_GEOMETRY(2001,
                                   edge_geom.sdo_srid,
                                   SDO_POINT_TYPE(edge_geom.sdo_ordinates((i * 2) - 1),
                                                  edge_geom.sdo_ordinates((i * 2)),
                                                  NULL),
                                   NULL,NULL);

           psql := 'SELECT COUNT(*) '
                || 'FROM '
                || p_match_tab || ' a '
                || 'WHERE '
                || 'SDO_RELATE(a.sdogeometry, :p1, :p3 ) = :p4 ';

           EXECUTE IMMEDIATE psql INTO relate_kount USING temp_pt,
                                                          'mask=ANYINTERACT',
                                                          'TRUE';

           IF relate_kount > 0
           THEN

              --this is a keeper
              new_ordinates.EXTEND(2);

              ordkount := ordkount + 1;
              new_ordinates(ordkount) := edge_geom.sdo_ordinates((i * 2) - 1);
              ordkount := ordkount + 1;
              new_ordinates(ordkount) := edge_geom.sdo_ordinates((i * 2));

           ELSE

              --this is a chucker
              vtx_kount := vtx_kount + 1;

           END IF;


        END IF;

      END LOOP;

      --change edge coords if necessary

      IF vtx_kount > 0
      THEN

         new_edge_geom := SDO_GEOMETRY(2002,
                                       edge_geom.sdo_srid,
                                       NULL,
                                       SDO_ELEM_INFO_ARRAY(1,2,1),
                                       new_ordinates
                                       );


         BEGIN

            GZ_UTILITIES.GZ_CHANGE_EDGE_COORDS(p_topo,
                                               p_edge_id,
                                               new_edge_geom);

         EXCEPTION
         WHEN OTHERS
         THEN

            --one option here would be to go through each vertex one by one
            --not gonna bother yet unless this is happening too much

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                                p_sqlstmt=>'Change edge ' || p_edge_id || ' error ',
                                                p_error_msg=>'FAILED with ' || SQLERRM);
            vtx_kount := 0;

         END;

      END IF;

      RETURN vtx_kount;

   END AXE_UNMATCHED_VTXS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION REMOVE_SP_UNIVERSAL_VERTICES (
      p_topo            IN VARCHAR2,
      p_clip_tab        IN VARCHAR2,
      p_drop_work_tab   IN VARCHAR2 DEFAULT 'N',
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN NUMBER
   AS

      --Matt! 12/14/11
      --Returns total number of vertices I think we axed
      --1/24/11 ! Attempt at making production-ready

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      srid              NUMBER;
      universal_edges   GZ_TYPES.stringarray;
      axed_vtxs         NUMBER := 0;
      total_axed        NUMBER := 0;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('REMOVE_SP_UNIVERSAL_VERTICES: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --basic verification

      IF NOT GZ_UTILITIES.TABLE_EXISTS(p_clip_tab)
      THEN

          RAISE_APPLICATION_ERROR(-20001,'Cuz, table ' || p_clip_tab || ' doesnt exist');

      END IF;


      --kind of want to check for valid sdo at real tolerance, but maybe thats overkill

      --Check that all state edges inputs are 2002s
      psql := 'SELECT count(*) '
           || 'FROM ' || p_clip_tab || ' a '
           || 'WHERE '
           || 'a.sdogeometry.sdo_gtype != :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING 2002;

      IF kount != 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Cuz, I expect 2002 gtype geometries in ' || p_clip_tab);

      END IF;


      --make a table with all of the important vertices around the universal face
      --source is the state edges table

      GZ_UTILITIES.CREATE_SMALL_POLY_RSUV( SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA') ,
                                           p_topo || '_RSUV'
                                         );

      --insert each vertex from the state edges table into the work table
      --as its own sdo_geometry

      psql := 'INSERT /*+ APPEND */ '
           || 'INTO ' || p_topo || '_RSUV '
           || 'SELECT * FROM TABLE(GZ_UTILITIES.PIPE_EDGE_VERTICES(:p1)) ';

      EXECUTE IMMEDIATE psql USING p_clip_tab;
      COMMIT;

      --check that this worked
      psql := 'SELECT COUNT(*) '
           || 'FROM '
           || p_topo || '_RSUV ';

      EXECUTE IMMEDIATE psql INTO kount;

      IF kount = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry cuz, failed to extract vertices from ' || p_clip_tab);

      END IF;

      --get an srid
      psql := 'SELECT a.sdogeometry.sdo_srid '
           || 'FROM '
           || p_topo || '_rsuv a '
           || 'WHERE rownum = 1 ';

      EXECUTE IMMEDIATE psql INTO srid;

      --spatial index the new work table
      GZ_UTILITIES.ADD_SPATIAL_INDEX(p_topo || '_RSUV',
                                    'SDOGEOMETRY',
                                    srid,
                                    p_tolerance);

      --Get all the unversal faces and throw each at a sub to remove vertices
      --distinct just in case, should never really happen I think
      psql := 'SELECT DISTINCT e.edge_id '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.left_face_id = :p1 OR '
           || 'e.right_face_id = :p2 ';

      --maybe should cursor, not sure how big this array could get
      EXECUTE IMMEDIATE psql BULK COLLECT INTO universal_edges USING -1, -1;

      FOR i IN 1 .. universal_edges.COUNT
      LOOP

         axed_vtxs := GZ_UTILITIES.AXE_UNMATCHED_VTXS(p_topo,
                                                      p_topo || '_RSUV ',
                                                      universal_edges(i),
                                                      p_tolerance);

         total_axed := total_axed + axed_vtxs;

      END LOOP;

      IF UPPER(p_drop_work_tab) != 'N'
      THEN

         psql := 'DELETE FROM user_sdo_geom_metadata WHERE table_name = :p1 ';
         EXECUTE IMMEDIATE psql USING UPPER(p_topo) || '_RSUV';
         COMMIT;

         psql := 'DROP TABLE ' || p_topo || '_RSUV ';
         EXECUTE IMMEDIATE psql;

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('REMOVE_SP_UNIVERSAL_VERTICES: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN total_axed;

   END REMOVE_SP_UNIVERSAL_VERTICES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE UPDATE_SP_THINNED_FACES (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_tolerance       IN NUMBER DEFAULT .05
   )
   AS

      --Matt! 1/24/12

      psql              VARCHAR2(4000);

   BEGIN

      --update all geometries touching the universal face to NULL
      --some of these may not have been edited, but many have

      psql := 'UPDATE ' || p_face_tab || ' a '
           || 'SET a.sdogeometry = NULL '
           || 'WHERE a.face_id IN '
           || '(SELECT DISTINCT e.right_face_id face_id '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.left_face_id = :p1 '
           || 'UNION '
           || 'SELECT DISTINCT e.left_face_id face_id '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.right_face_id = :p2) ';

      EXECUTE IMMEDIATE psql USING -1, -1;
      COMMIT;


      GZ_UTILITIES.GZ_POPULATE_MEASUREMENTS(p_face_tab,
                                            'FACE_ID',
                                            'SDOGEOMETRY',  --just update sdo.  Not sure if this should be ALL instead
                                            'NULL');        --only where face sdo is null

   END UPDATE_SP_THINNED_FACES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION SP_VERTEX_THINNER (
      p_topo            IN VARCHAR2,
      p_clip_tab        IN VARCHAR2,
      p_face_tab        IN VARCHAR2 DEFAULT NULL,
      p_validate_sdo    IN VARCHAR2 DEFAULT 'N',
      p_validate_topo   IN VARCHAR2 DEFAULT 'N',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_drop_work_tab   IN VARCHAR2 DEFAULT 'N'
   ) RETURN NUMBER
   AS

      --Matt! 12/15/11
      --Entry point for state outline vertex thinning
      --Wrapper for REMOVE_SP_UNIVERSAL_VERTICES
      --Returns number of vertices removed
      --throws error if any faces are invalid or topo is invalid

      --1/24/12 ! Updates to incorporate into small poly production

      --Anticipated sample call from Small Poly
      --vtxkount := GZ_UTILITIES.SP_VERTEX_THINNER('Z666SP','Z666CL_STEDZ6V4','Z666SP_CLIP_FACE')
      --Though some of the flags, esp validate sdo, depend on workflow decisions

      output            NUMBER;
      val_ret           VARCHAR2(4000);
      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('SP_VERTEX_THINNER: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --log template
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                          p_sqlstmt=>'SP_VERTEX_THINNER',
                                          p_error_msg=>'Starting');

      --check for existence of feature face table if caller has one
      --existence of p_clip_tab is checked in sub

      IF p_face_tab IS NOT NULL
      THEN

         IF NOT GZ_UTILITIES.TABLE_EXISTS(p_face_tab)
         THEN

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                                p_sqlstmt=>'Input verification',
                                                p_error_msg=>'Failed, ' || p_face_tab || ' doesnt exist');

             RAISE_APPLICATION_ERROR(-20001,'Cuz, table ' || p_face_tab || ' doesnt exist');

         END IF;

      END IF;

      --------------------
      --do the actual work
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                          p_sqlstmt=>'Calling REMOVE_SP_UNIVERSAL_VERTICES on ' || p_topo,
                                          p_error_msg=>'Starting');

      output := GZ_UTILITIES.REMOVE_SP_UNIVERSAL_VERTICES(p_topo,
                                                          p_clip_tab,
                                                          p_drop_work_tab,
                                                          p_tolerance);

      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                          p_sqlstmt=>'REMOVE_SP_UNIVERSAL_VERTICES removed ' || output || ' vertices',
                                          p_error_msg=>'Complete');

      --/all the actual work
      ----------------------

      IF p_validate_topo = 'Y'
      THEN

         --will throw an error and kill us if invalid. Allow this

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                             p_sqlstmt=>'Validating ' || p_topo,
                                             p_error_msg=>'Starting');
         BEGIN

            val_ret := GZ_UTILITIES.VALIDATE_TOPOLOGY(p_topo);

         EXCEPTION
            WHEN OTHERS
            THEN

               GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                                   p_sqlstmt=>'Validating ' || p_topo,
                                                   p_error_msg=>'FAILED with ' || SQLERRM);

               RAISE_APPLICATION_ERROR(-20001,'Validation failed: ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

         END;

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                             p_sqlstmt=>'Validating ' || p_topo,
                                             p_error_msg=>'Finished - Sweet ' || p_topo || ' is valid');

      END IF;

      IF p_face_tab IS NOT NULL
      AND output > 0
      THEN

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                             p_sqlstmt=>'Updating sdogeometry in ' || p_face_tab,
                                             p_error_msg=>'Starting');
         GZ_UTILITIES.UPDATE_SP_THINNED_FACES(p_topo,
                                              p_face_tab,
                                              p_tolerance);

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                             p_sqlstmt=>'Updating sdogeometry in ' || p_face_tab,
                                             p_error_msg=>'Finished');

      ELSIF p_face_tab IS NOT NULL
      AND output = 0
      THEN

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                             p_sqlstmt=>'Not updating sdogeometry in ' || p_face_tab,
                                             p_error_msg=>'0 faces changed. ');

      END IF;

      IF p_validate_sdo = 'Y'
      THEN

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                             p_sqlstmt=>'Validating sdogeometry in ' || p_face_tab,
                                             p_error_msg=>'Starting');

         psql := 'SELECT COUNT(*) '
              || 'FROM '
              || p_face_tab || ' a '
              || 'WHERE '
              || 'sdo_geom.validate_geometry_with_context(a.sdogeometry, :p1) != :p2 ';

         EXECUTE IMMEDIATE psql INTO kount USING p_tolerance,
                                                 'TRUE';

         IF kount > 0
         THEN

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                                p_sqlstmt=>'Validating sdogeometry in ' || p_face_tab,
                                                p_error_msg=>'Failure - ' || kount || ' are invalid');

            RAISE_APPLICATION_ERROR(-20001,'Sorry cuz, ' || kount || ' faces in ' || p_face_tab || ' are invalid ');

         END IF;

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                             p_sqlstmt=>'Validating sdogeometry in ' || p_face_tab,
                                             p_error_msg=>'Finished - all valid');

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('SP_VERTEX_THINNER: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo,'SP_VERTEX_THINNER',
                                          p_sqlstmt=>'SP_VERTEX_THINNER',
                                          p_error_msg=>'Complete, returning ' || output);


      RETURN output;


   END SP_VERTEX_THINNER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE GZ_ADD_TOPO_GEOMETRY_LAYER (
      p_topo            IN VARCHAR2,
      p_table_name      IN VARCHAR2,
      p_column_name     IN VARCHAR2 DEFAULT 'TOPOGEOM',
      p_layer_type      IN VARCHAR2 DEFAULT 'POLYGON',
      p_child_tab       IN VARCHAR2 DEFAULT NULL
   )
   AS

     --Matt! 12/20/11 Wrapper for SDO_TOPO.ADD_TOPO_GEOMETRY_LAYER
     --Not sure why I didnt write this until now

      child_tg_layer_id          NUMBER;

   BEGIN

      IF p_layer_type NOT IN ('POINT', 'LINE', 'CURVE', 'POLYGON', 'COLLECTION')
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Cousin, what is a layer type of ' || p_layer_type || '?');

      END IF;


      IF p_child_tab IS NULL
      THEN

         child_tg_layer_id := NULL;

      ELSE

         child_tg_layer_id := GZ_UTILITIES.GET_TG_LAYER_ID(p_topo,
                                                           p_child_tab,
                                                           p_column_name,  --assume same as parent
                                                           p_layer_type);

      END IF;


      BEGIN

         SDO_TOPO.add_topo_geometry_layer(p_topo,
                                          p_table_name,
                                          p_column_name,
                                          p_layer_type,
                                          NULL,               --never mess with relation$ storage
                                          child_tg_layer_id); --child layer id likely NULL


      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLERRM LIKE '%Need use delete_topo_geometry_layer%'  --yes, really written like that
         THEN

            SDO_TOPO.DELETE_TOPO_GEOMETRY_LAYER(p_topo,p_table_name,p_column_name);

            --try again
            SDO_TOPO.add_topo_geometry_layer(p_topo,
                                          p_table_name,
                                          p_column_name,
                                          p_layer_type,
                                          NULL,               --never mess with relation$ storage
                                          child_tg_layer_id); --child layer id

         ELSE

            --what else can we catch here?

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

         END IF;

      END;


   END GZ_ADD_TOPO_GEOMETRY_LAYER;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE GZ_COPY_ATTR_FROM_FACE_TO_FACE (
      p_release                IN VARCHAR2,
      p_projectid              IN VARCHAR2,
      p_face_table_name        IN VARCHAR2,
      p_src_face               IN VARCHAR2,
      p_destination_face       IN VARCHAR2
   )
   AS

     --Suzanne 1/3/12 When updating face attributes to merge sliver faces into their larger counterparts, we should
     --change ALL attributes on the sliver face so let's standardize how we do that

     --p_src_face is large face
     --p_destination_face is sliver face

        vsql varchar2(4000);
        vsql2 varchar2(4000);
        vsql3 varchar2(4000);

        vcolumn_list GZ_TYPES.stringarray;
        vCount NUMBER;

   BEGIN

        vcolumn_list := GZ_UTILITIES.GET_REFERENCE_FACE_FIELDS(p_release, p_projectid,'ATTRIBUTE','REFERENCE_FACE_FIELDS');

        vSql := 'update '||p_face_table_name||' set ';
        vSql3:= 'where face_id = '||p_destination_face;

        vCount := vcolumn_list.COUNT;

        FOR i in 1..vcolumn_list.COUNT LOOP
            IF i = vCount THEN
                vSql2 := vSql2 ||vcolumn_list(i)||' = (select '||vcolumn_list(i)||' from '||p_face_table_name||' where face_id = '||p_src_face||') ';
            ELSE
                vSql2 := vSql2 ||vcolumn_list(i)||' = (select '||vcolumn_list(i)||' from '||p_face_table_name||' where face_id = '||p_src_face||'),';
            END IF;
        END LOOP;

        vsql := vsql ||vSql2||vsql3;

        execute immediate vsql;
        commit;
        --dbms_output.put_line(vsql);

   END GZ_COPY_ATTR_FROM_FACE_TO_FACE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE GZ_COPY_FACE_ATTR_FROM_TABLE (
      p_projectid                      IN VARCHAR2,
      p_faceid                        IN VARCHAR2,
      p_face_src_table_name           IN VARCHAR2,
      p_face_destination_table_name   IN VARCHAR2
   )
   AS

     --Suzanne 1/4/12 A helper tool to restore the attributes of a face from a previous version of a face_table
     --in case manual work requires restoring the original face attributes; no measurements are copied over

        vsql varchar2(4000);
        vsql2 varchar2(4000);
        vsql3 varchar2(4000);

        vcolumn_list GZ_TYPES.stringarray;
        vCount NUMBER;

   BEGIN

        vcolumn_list := GZ_UTILITIES.GET_REFERENCE_FACE_FIELDS(p_projectid,'ATTRIBUTE','REFERENCE_FACE_FIELDS');

        vSql := 'update '||p_face_destination_table_name||' set ';
        vSql3:= 'where face_id = '||p_faceid;

        vCount := vcolumn_list.COUNT;

        FOR i in 1..vcolumn_list.COUNT LOOP
            IF i = vCount THEN
                vSql2 := vSql2 ||vcolumn_list(i)||' = (select '||vcolumn_list(i)||' from '||p_face_src_table_name||' where face_id = '||p_faceid||') ';
            ELSE
                vSql2 := vSql2 ||vcolumn_list(i)||' = (select '||vcolumn_list(i)||' from '||p_face_src_table_name||' where face_id = '||p_faceid||'),';
            END IF;
        END LOOP;

        vsql := vsql ||vSql2||vsql3;

        execute immediate vsql;
        commit;
        --dbms_output.put_line(vsql);

   END GZ_COPY_FACE_ATTR_FROM_TABLE;

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

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE REMOVE_EXACT_TOPO_DUPES (
      p_topo                  IN VARCHAR2
   )
   AS

     --Matt! 1/19/12

     psql                     VARCHAR2(4000);
     my_cursor                SYS_REFCURSOR;
     edge_ids                 GZ_TYPES.stringarray;
     edge_geoms               GZ_TYPES.geomarray;
     got_dupe                 PLS_INTEGER;
     xys                      MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();

   BEGIN

      psql := 'SELECT e.edge_id, e.geometry '
           || 'FROM '
           || p_topo || '_edge$ e ';

      OPEN my_cursor FOR psql;

      LOOP

         FETCH my_cursor BULK COLLECT INTO edge_ids, edge_geoms LIMIT 25;
         EXIT WHEN edge_ids.COUNT = 0;

            FOR i in 1 .. edge_ids.COUNT
            LOOP

               xys := edge_geoms(i).sdo_ordinates;

               --xys is in out nocopy
               --returns greater than 0 if a dupe
               got_dupe := GZ_UTILITIES.REMOVE_DUPE_VERTEX(xys);

               IF got_dupe != 0
               THEN

                  --dbms_output.put_line('removing a dupe!');

                  edge_geoms(i).sdo_ordinates := xys;

                  GZ_UTILITIES.GZ_CHANGE_EDGE_COORDS(p_topo,
                                                     edge_ids(i),
                                                     edge_geoms(i));

               END IF;

               xys.DELETE;

            END LOOP;

      END LOOP;


   END REMOVE_EXACT_TOPO_DUPES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE REBUILD_TOPO_DOMAIN_INDEXES (
      p_topo                  IN VARCHAR2
   )
   AS

      --Matt! 1/19/12

      psql              VARCHAR2(4000);
      baddies           GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT index_name '
           || 'FROM '
           || 'user_indexes '
           || 'WHERE '
           || 'table_name LIKE :p1 AND '
           || 'index_type = :p2 AND '
           || '(status != :p3 OR domidx_status != :p4 OR domidx_opstatus != :p5) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO baddies USING p_topo || '%',
                                                             'DOMAIN',
                                                             'VALID',
                                                             'VALID',
                                                             'VALID';

      FOR i IN 1 .. baddies.COUNT
      LOOP

         EXECUTE IMMEDIATE 'ALTER INDEX ' || baddies(i) || ' REBUILD ';

      END LOOP;


   END REBUILD_TOPO_DOMAIN_INDEXES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE REBUILD_REL_DOLLAR_INDEXES (
      p_topo                  IN VARCHAR2
   )
   AS

      --Matt! 1/18/12
      --Some bug is causing relation$ index <topo>_REL_IDX$ to be marked unuseable
      --also sometimes the same for the <topo>_REL_LID$ index
      --Rebuild them

      psql              VARCHAR2(4000);
      baddies           GZ_TYPES.stringarray;

   BEGIN


      psql := 'SELECT index_name '
           || 'FROM user_indexes '
           || 'WHERE '
           || 'table_name = :p1 AND '
           || 'status = :p2 AND '
           || 'index_type = :p3 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO baddies USING UPPER(p_topo) || '_RELATION$',
                                                             'UNUSABLE', --not unusEable
                                                             'NORMAL';

      FOR i IN 1 .. baddies.COUNT
      LOOP


         EXECUTE IMMEDIATE 'ALTER INDEX ' || baddies(i) || ' REBUILD ';

      END LOOP;

      --I have a weird suspicion that these can cascade

      IF baddies.COUNT > 0
      THEN

         EXECUTE IMMEDIATE psql BULK COLLECT INTO baddies USING UPPER(p_topo) || '_RELATION$',
                                                                'UNUSABLE', --not unusEable
                                                                'NORMAL';
         FOR j IN 1 .. baddies.COUNT
         LOOP

            EXECUTE IMMEDIATE 'ALTER INDEX ' || baddies(j) || ' REBUILD ';

         END LOOP;

      END IF;


   END REBUILD_REL_DOLLAR_INDEXES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE GZ_TOPO_TUNE_UP (
      p_topo                  IN VARCHAR2
   ) AS

      --Matt! 1/18/12

      --Consolidate all of the topo fixes and polishing into one procedure
      --Idea is that it should work for both fresh empty topologies and full final topologies
      --Whenever we come to realize something new just add it here instead of 20 places
      --Open to suggestions and feel free to add or remove to/from it

   BEGIN

      -- creates the Unique relation$ index if it is missing.
      -- creates the _REL_LID$ index if it does not exist.
      -- drops and creates the _REL_LID$ if has the wrong number of fields
      GZ_TOPO_UTIL.FIX_RELATION_DOLLAR_INDEXES(UPPER(p_topo));

      --rebuild any relation$ indexes that have become unusable
      GZ_UTILITIES.REBUILD_REL_DOLLAR_INDEXES(UPPER(p_topo));

      --rebuild all domain indexes on a topo table that are not valid
      GZ_UTILITIES.REBUILD_TOPO_DOMAIN_INDEXES(UPPER(p_topo));

      --stats gathered on all registered topo feature and dollar tables
      GZ_UTILITIES.GATHER_TOPO_STATS(UPPER(p_topo));

      --grant privileges as specd in reference schemas on all tables named like this topology
      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',UPPER(p_topo) || '%');

      --Remove exactly matched consecutive duplicate vertices in edge$?
      --Im not 100 percent convinced that this is a good idea.  We will see
      GZ_UTILITIES.REMOVE_EXACT_TOPO_DUPES(UPPER(p_topo));


      --what about isolated nodes?  Or is that getting carried away?


      --What about finding missing indexes after a copy?  I dont really know how to do that



   END GZ_TOPO_TUNE_UP;

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

   FUNCTION VALIDATE_LINES_WITH_CONTEXT (
      p_line                  IN SDO_GEOMETRY,
      p_tolerance             IN NUMBER DEFAULT .05
   ) RETURN VARCHAR2
   AS

      --Matt! 1/26/11

      --sample usage
      --select e.edge_id, gz_utilities.validate_lines_with_context(e.geometry, .05)
      --from z955ls_edge$ e
      --where gz_utilities.validate_lines_with_context(e.geometry, .05) != 'TRUE'

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

      IF sdo_geom.validate_geometry_with_context(p_line, p_tolerance) != 'TRUE'
      THEN

         RETURN sdo_geom.validate_geometry_with_context(p_line, p_tolerance);

      END IF;

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

      RETURN 'TRUE';


   END VALIDATE_LINES_WITH_CONTEXT;

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
--
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE UPDATE_HARD_CODED_TABLES (
      p_table_list            IN VARCHAR2,
      p_type                  IN VARCHAR2
   )
   AS

      --Matt! 1/30/12
      --1/26/12 Added source_location_tbls
      --Helper for production build script
      --Note that order of operations is install packages, imp dump file, then this

      --DEPLOY_TBLS=ACS11_TU_Z6_IN,ACS11_TH_Z6_IN
      --GEN_SCHEMA_TBLS=GZ_TOPOBUILD_SETUP,GZ_QA_SETUP
      --UNGEN_SCHEMA_TBLS=GZ_QA_SETUP
      --SOURCE_LOCATION_TBLS=TOPO_UNIVERSE


      psql        VARCHAR2(4000);
      tabs        GZ_TYPES.stringarray;

   BEGIN

      IF UPPER(p_type) NOT IN ('DEPLOY_TBLS','GEN_SCHEMA_TBLS','UNGEN_SCHEMA_TBLS','SOURCE_LOCATION_TBLS')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, I dont know what ' || p_type || ' is ');

      END IF;

      tabs := GZ_UTILITIES.SPLIT(p_table_list,',');

      FOR i IN 1 .. tabs.COUNT
      LOOP

         psql := 'UPDATE ' || TRIM(tabs(i)) || ' a '
              || 'SET a.';

         IF UPPER(p_type) = 'DEPLOY_TBLS'
         THEN

            psql := psql || 'deploy';

         ELSIF UPPER(p_type) = 'GEN_SCHEMA_TBLS'
         THEN

            psql := psql || 'gen_schema';

         ELSIF UPPER(p_type) = 'UNGEN_SCHEMA_TBLS'
         THEN

            psql := psql || 'ungen_schema';

         ELSIF UPPER(p_type) = 'SOURCE_LOCATION_TBLS'
         THEN

            psql := psql || 'source_location';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Lost here');

         END IF;

         IF UPPER(p_type) != 'SOURCE_LOCATION_TBLS'
         THEN

            --first three just update to schema
            psql := psql || ' = :p1 ';

            --dbms_output.put_line(psql);

            EXECUTE IMMEDIATE psql USING SYS_CONTEXT('USERENV','CURRENT_SCHEMA');

         ELSE

            --ex TAB10ST60.COUNTY@DEVPCRAC
            --to TAB10ST60.COUNTY@PRODBNCH

            psql := psql || ' = REGEXP_REPLACE(a.source_location, :p1 , :p2) ';

            --dbms_output.put_line(psql);

            EXECUTE IMMEDIATE psql USING '@(.*)',
                                         '@' || UPPER(SYS_CONTEXT('USERENV', 'DB_NAME'));

         END IF;


         COMMIT;

      END LOOP;


   END UPDATE_HARD_CODED_TABLES;



   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE PRIV_GRANTS_ON_LIST (
      p_table_list            IN VARCHAR2
   )
   AS

      --Matt! 1/30/12
      --Helper for production install script
      --wrapper for gz_utilities.gz_priv_granter

      tabs     GZ_TYPES.stringarray;

   BEGIN

      tabs := GZ_UTILITIES.SPLIT(p_table_list,',');

      FOR i IN 1 .. tabs.COUNT
      LOOP

         --dbms_output.put_line(TRIM(UPPER(tabs(i))));

         BEGIN

            GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',TRIM(UPPER(tabs(i))));

         EXCEPTION
         WHEN OTHERS
         THEN

            RAISE_APPLICATION_ERROR(-20001,'PRIV_GRANTS_ON_LIST table ' || TRIM(UPPER(tabs(i))) || ' threw ' || SQLERRM);

         END;

      END LOOP;


   END PRIV_GRANTS_ON_LIST;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------

   PROCEDURE GZ_DROP_TABLE (
      p_table                  IN VARCHAR2
   )
   AS

      --Matt! 3/6/12
      --preferred practice for gz table dropping goes here

   BEGIN

      BEGIN

         EXECUTE IMMEDIATE 'DROP TABLE ' || UPPER(p_table) || ' PURGE '; --sure on the purge?

      EXCEPTION
      WHEN OTHERS
      THEN

         --anything we expect to encounter and want to handle or die?
         NULL;

      END;

   END GZ_DROP_TABLE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE DROP_MODULE_WORK_TABLES (
      p_topo                  IN VARCHAR2,            --ex Z201IN
      p_module                IN VARCHAR2,            --ex BUILD
      p_drop_tracking         IN VARCHAR2 DEFAULT 'N'
   )
   AS

      --Matt! 3/6/12
      --I plan to share this in clip, merge, build, and topofix
      --Feel free to join the party

      psql                 VARCHAR2(4000);
      tabz                 GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT a.table_name '
           || 'FROM user_tables a '
           || 'WHERE '
           || 'a.table_name LIKE :p1 AND '
           || 'a.table_name LIKE :p2 AND '
           || 'a.table_name NOT LIKE :p3 AND '
           || 'NOT EXISTS ('
           || 'SELECT * FROM user_sdo_topo_info b where b.table_name = a.table_name'
           || ') ';

      IF p_drop_tracking = 'Y'
      THEN

         EXECUTE IMMEDIATE psql BULK COLLECT INTO tabz USING '%' || UPPER(p_module) || '%',
                                                             UPPER(p_topo) || '%',
                                                             '%$';

      ELSE

         --usually in here for me, I want to keep mines tracking table

         psql := psql || 'AND a.table_name NOT LIKE :p4 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO tabz USING '%' || UPPER(p_module) || '%',
                                                             UPPER(p_topo) || '%',
                                                             '%$',
                                                             '%TRACKING%';
      END IF;

      FOR i IN 1 .. tabz.COUNT
      LOOP

         GZ_DROP_TABLE(tabz(i));

      END LOOP;


   END DROP_MODULE_WORK_TABLES;

    ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE DROP_MODULE_SEQUENCES (
      p_topo                  IN VARCHAR2,
      p_module                IN VARCHAR2
   )
   AS

      --Matt! 8/28/12
      --So far only sequence cleanup is the output builder cleaning up
      --   Sidey's 1 sequence in GZ_PROJECTION.project_To_albers

      psql                 VARCHAR2(4000);
      seqz                 GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT sequence_name '
           || 'FROM user_sequences '
           || 'WHERE '
           || 'sequence_name LIKE :p1 AND '
           || 'sequence_name LIKE :p2 AND '
           || 'sequence_name NOT LIKE :p3 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO seqz USING '%' || UPPER(p_module) || '%',
                                                          UPPER(p_topo) || '%',
                                                          '%$';

      FOR i IN 1 .. seqz.COUNT
      LOOP

         --any error handling here?  Not for now I guess
         EXECUTE IMMEDIATE 'DROP SEQUENCE ' || seqz(i) || ' ';

      END LOOP;


   END DROP_MODULE_SEQUENCES;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

    PROCEDURE CREATE_VERTEX_TABLE (
       pGeom           SDO_GEOMETRY,
       pOutputTable    VARCHAR2,
       pDropTable      VARCHAR2 DEFAULT 'N')
    AS
       /*

       Stephanie 4/2/2012

       Creates a table with a vertex id and point sdo geometry from an
       edge sdo geometry.  (primarily for labeling vertexes in mapviewer).

       pgeom = an edge sdo geometry
       poutputtable = the name of a new table to store the vertexes in
       pDrop Table = 'Y' allows the program to drop a pre-exisitng table
                      with the same name as Output Table.

       */

       vSql                  VARCHAR2 (4000);
       vDropSql              VARCHAR2 (4000);
       vVertex_count         NUMBER;
       vlon                  NUMBER;
       vlat                  NUMBER;
       vPointGeom            SDO_GEOMETRY;
       vDropFlag             VARCHAR2 (1) := UPPER (pDropTable);
       eTableAlreadyExists   EXCEPTION;
       PRAGMA EXCEPTION_INIT (eTableAlreadyExists, -00955);
       vmsg                  VARCHAR2 (4000);

    BEGIN
           vVertex_count := 0;
           vlon := 0;
           vlat := 0;

           vsql :=
                 'Create table '
              || pOutputTable
              || ' (vid number, geom sdo_geometry) '
              || 'noparallel nologging';

           BEGIN
              EXECUTE IMMEDIATE vsql;
           EXCEPTION
              WHEN eTableAlreadyExists
              THEN
                 IF vDropFlag = 'Y'
                 THEN
                    DBMS_OUTPUT.put_line (
                       'Dropped exisitng table, ' || pOutputTable || '.');
                    vdropsql :=
                       'DROP table ' || pOutputTable || ' cascade constraints purge';

                    EXECUTE IMMEDIATE vDropSQL;

                    EXECUTE IMMEDIATE vSql;
                 ELSE
                    vmsg :=
                          'A table called '
                       || pOutputTable
                       || ' already exists. Call with pDropTable = ''Y'' to '
                       || 'drop this table.';
                    raise_application_error (-20101, vMsg);
                 END IF;
              WHEN OTHERS
              THEN
                 RAISE;
           END;


        -- loop though each ordinate pair

        FOR i IN 1 .. (pGeom.sdo_ordinates.COUNT/2)
           LOOP

             -- create sdogeometry for the vertex point

              vVertex_count := i;
              vlon := pGeom.sdo_ordinates (i * 2 -1);
              vlat := pGeom.sdo_ordinates (i * 2);

              vPointGeom :=
                       sdo_geometry (2001,
                                     pGeom.sdo_srid,
                                     SDO_POINT_TYPE (vlon, vlat, NULL),
                                     NULL,
                                     NULL);

                    vSQL :=
                          'INSERT INTO '
                       || pOutputTable
                       || ' (vid,geom) values (:p1,:p2)';

                    BEGIN
                       EXECUTE IMMEDIATE vSQL USING vVertex_count, vPointGeom;
                    EXCEPTION
                       WHEN OTHERS
                       THEN
                          RAISE;
                    END;

                    COMMIT;

        END LOOP;

    END CREATE_VERTEX_TABLE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE CPB_SLEEP (
      p_time_seconds    IN NUMBER
   )
   AS

      --Matt! 7/25/11
      --DBMS_LOCK.SLEEP is locked down, need an alternative
      --This is a horrible idea, CPU-wise
      --Also not too accurate
      --Otherwise, this is the awesome

      the_time             DATE;
      psql                 VARCHAR2(4000);

   BEGIN


      psql := 'SELECT SYSDATE FROM DUAL ';

      EXECUTE IMMEDIATE psql INTO the_time;

      LOOP

         EXIT WHEN the_time + (p_time_seconds * (1/86400)) = SYSDATE;

      END LOOP;


   END CPB_SLEEP;


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

         output(i) := GZ_UTILITIES.GEODETIC_MBR_CONSIDERATIONS(p_tiles(i));

      END LOOP;

      FOR i IN 1 .. output.COUNT
      LOOP

         output(i) := GZ_UTILITIES.SHIFT_AT_DATELINE(output(i));

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

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                 'Starting tiling of ' || p_table );

      END IF;

      IF tile_target <= 0
      OR tile_target > 100000
      THEN

         --just a guess
         RAISE_APPLICATION_ERROR(-20001,'Sorry boss, I dont think a tile target of ' || tile_target || ' is gonna work ');

      END IF;

      IF NOT GZ_UTILITIES.GZ_TABLE_EXISTS(p_table)
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

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
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
         dbms_output.put_line(gz_utilities.dump_sdo(big_mbr));
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
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

            subdivided_output := GZ_UTILITIES.TIDY_TILES(subdivided_output);

            --all negative after tidy tiles
            IF GZ_UTILITIES.GET_GEODETIC_MBR_WIDTH(subdivided_output(1)) < 119
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

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
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
               GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
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
                  GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                      'Tossing battleship->',NULL,NULL,NULL,NULL,NULL,NULL,test_window);
               END IF;

            ELSE

               trukount := trukount + 1;
               output(output.COUNT + 1) := test_window;
               --test_tiles(test_tiles.COUNT + 1) := 'TRUE';

               IF p_debug = 1
               THEN
                  GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
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

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
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

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
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

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                   'Multiplying ' || trukount || ' tiles by ' || SQRT(best_i) ,NULL,NULL,NULL,NULL);

         END IF;


         FOR i IN 1 .. output.COUNT
         LOOP

            subdivided_tile := GZ_UTILITIES.SUBDIVIDE_TILE(output(i),SQRT(best_i));

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

            subdivided_output := GZ_UTILITIES.TIDY_TILES(subdivided_output);

         END IF;

         --dont forget, some of these subdivided tiles may be over dead space too

         IF p_debug = 1
         THEN
            dbms_output.put_line('Sending ' || subdivided_output.COUNT || ' tiles to spatial tests');
         END IF;

         IF p_log_type IS NOT NULL
         THEN

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
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
                  GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                         'Tossing tile->',NULL,NULL,NULL,NULL,NULL,NULL,subdivided_output(i));
               END IF;

            ELSE

               output(output.COUNT + 1) := subdivided_output(i);

               IF p_debug = 1
               THEN
                  GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
                                                         'Keeping tile->',NULL,NULL,NULL,NULL,NULL,NULL,subdivided_output(i));
               END IF;

            END IF;


         END LOOP;

         IF p_log_type IS NOT NULL
         THEN

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_log_tag,'GZ_TILE_TABLE',NULL,
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
         output := GZ_UTILITIES.SHIFT_AT_DATELINE(p_mbr);

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
   
   PROCEDURE DBMS_SQL_HELPER (
      p_sql             IN CLOB
   )
   AS
   
      --Matt! 12/17/12
      --Starter wrapper.  Called from gz_workflow pre_process code
      --More to come
      
      --In Oracle Database 11g, native dynamic SQL now supports statements bigger 
      --   than 32K characters by allowing a CLOB argument
      
      cursor_id    BINARY_INTEGER;
      v_dummy      INTEGER;
      
   BEGIN
   
      cursor_id := DBMS_SQL.OPEN_CURSOR;
      
      DBMS_SQL.PARSE(cursor_id, p_sql, DBMS_SQL.NATIVE);
      
      v_dummy := DBMS_SQL.EXECUTE(cursor_id);
                  
      DBMS_SQL.CLOSE_CURSOR(cursor_id);
      
   
   END DBMS_SQL_HELPER;
   
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

END GZ_UTILITIES;
/

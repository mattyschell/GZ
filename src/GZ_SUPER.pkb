CREATE OR REPLACE PACKAGE BODY GZ_SUPER AS
-- Updates:
--    06/07/12    Tracking and parameter checking updates.
--    01/24/2012  Minor change to SET_MANY_MBR to make m and no_to_do more even
--    01/18/2011  Improvements to Find_Segment and Check_Clock_Wiseness
--                for speed and accuracy respectively
--    01/13/2011  Fix to Check_Clock_Wiseness sometimes segment off by 1.
--                This caused infinite loops on edges that were thought to
--                be on the wrong side of another edge - no matter how much
--                the scale was reduced
--    02/10/2011  Check for divisions and NOT SWAPPED edges caught in swap_edge
--    11/23/2010  Avoid Self intersections
--    10/20/2010 To avoid going on the wrong side of islands and to use
--               other SRIDs besides 8265.
--     9/14/2010 To avoid going on the wrong side of islands
--     8/27/2010 To avoid "An island of face nn has an edge coincident with
--                   outer boundary" we add the 2nd and next to last vertices
--                   to figure 8 polygons.
--     8/16/2010 To drop Topomap if last run died. Also to name Edge Table
-- A package to supervise generalization of edges in a topology.
-- Supervision (hence SUPER) has one overall goal: - maintain relationships
-- that are unaffected by stretching the surface - so called topological
-- relationships. So if we reshape an edge by stretching the region around it,
-- the relationships with other edges must not change. So intersecting itself or
-- other edges must be avoided by modifying the scale where necessary for parts
-- of the edge being generalized. If you imagine a river edge with two banks on
-- either side, you can foresee that generalization of the river edge requires
-- care to avoid the banks. To accomplish  this, the procedure Simplify requires
-- a work table (Edges_toprocess) which has a nearby edge list for each edge and
-- a new or current geometry which acts as state variable to describe the current
-- shape of each edge and thus the state of the topology.  It will make this table
-- automatically if it has not been already made by calling Run_Make_Edge_Table.
--
-- Besides updating the topology with the new geometries, Simplify writes 2 status
-- tables: Edges_done and Bad_edges. The 1st table is queried to avoid processing
-- an edge twice. Since this table has the entity (state) listed it is convenient
-- to monitor progress. Bad_edges has edge_ids and the status messages for edges
-- that could not be inserted into the topology.
--
-- Assumptions: Code assumes you will run from within the schema. This only
-- affects the call to Index_exists.

-- So the entry points are:

--SQL>  exec simplify('10','Z610LS3','Z610LS3','ZONE',500000.,1.,'ACS10_SL040','edges_toprocess','Z610LS3EDGES_TOSKIP','edges_done','bad_edges','state_edge_table','STATEFP','Z610LS3_EDGE_ATT','Z610LS3_CLIP_FACE');
-- In this usage statement all INPUTS are capitalized and all work and tracking tables are in lower case. These lower case table names will be prefixed with the run flag, 'Z610LS3' in this case.

-- This call  processes Delaware using ZONE at 1:500,000 scale using the topology 'Z610LS3' and the same run_flag:

FUNCTION VALIDATE_TOPOLOGY_WITH_TILES(v_topology VARCHAR2,face VARCHAR2,vstate VARCHAR2) return VARCHAR2 AS

    result        VARCHAR2(1000);
    error_msg      VARCHAR2(4000);
    v_row_count    PLS_INTEGER;
    v_tile_guess   PLS_INTEGER;
    sql_stmt       VARCHAR2(4000);

    --Matt! as of 12/31/13 no longer called


BEGIN

  BEGIN
       -- old call to validation can run into a memory error if the topology is
       -- large.
       -- sql_stmt := 'select  sdo_topo_map.validate_topology('''||v_topology||''',''N'') from dual';
       -- EXECUTE IMMEDIATE sql_stmt into result;

       -- estimate topology size

       v_row_count := 0;
       sql_stmt := 'select count(*) from '||v_topology||'_EDGE$';
       EXECUTE IMMEDIATE sql_stmt into v_row_count;

       result := 'FALSE';

       if v_row_count > 50000
       THEN
          -- we guess this is a large topology that will need to be tiled
          v_tile_guess := 20;
       ELSIF v_row_count > 10000
       THEN
          -- we guess this is a medium sized topology
          v_tile_guess := 10;
       ELSE
          -- this topology can probably be handled without tiling
          v_tile_guess := 1;

       END IF;

       -- run validation...

       result := GZ_TOPO_UTIL.GZ_VALIDATE_TOPOLOGY(v_topology,face,'SDOGEOMETRY', p_log_type => 'LS', p_tile_target => v_tile_guess);


           EXCEPTION
        WHEN OTHERS  then

         error_msg := 'Fail on Validate Topology:Line_Simplify:'||
                       'For entity' || vstate|| ' Validate Topology Failed '||sqlerrm;

         dbms_output.put_line(sqlerrm);

         GZ_TOPOFIX.Track_App_Error(error_msg,v_topology,'LS');

        END;



    RETURN result;
--------------------------------------------------------------------------------


END VALIDATE_TOPOLOGY_WITH_TILES;
--

PROCEDURE SIMPLIFY (pstate VARCHAR2,pTopology VARCHAR2, run_flag VARCHAR2 default NULL,pmethod VARCHAR2 default 'ZONE',ptarget_scale NUMBER DEFAULT 500000., pnice NUMBER default 1.,
pEntity_Table VARCHAR2 default 'ACS09_SL040',pedges_table VARCHAR2 default 'EDGES_TOPROCESS',  pskip_edges_table VARCHAR2 default NULL, pDone_edges_table VARCHAR2 default 'EDGES_DONE',pBad_Table VARCHAR2 default 'BAD_EDGES',pState_Edge_Table VARCHAR2 default 'STATE_EDGE_TABLE',pEntityfp VARCHAR2 default 'STATEFP',pEdge_attribute_table VARCHAR2 default 'EDGE',pclip_face_table VARCHAR2 default 'FACE',drop_work_tables VARCHAR2 default 'TRUE') AS

/*
********************************************************************************
--Program Name: Simplify
--Author: Sidey Timmins
--Creation Date: 07/09/2010

--Usage:
     1) exec simplify('01','Z601SP3','AA','ZONE',500000,1.,'ACS09_SL040',
                 'EDGES_TOPROCESS',NULL,'EDGES_DONE','BAD_EDGES','STATE_EDGE_TABLE','STATEFP','EDGE_ATT','CLIP_FACE');
--   2) exec Simplify(NULL,'MT','AA','ZONE',500000.);
--   exec GZCPB_8.GZ_SUPER.simplify('06','Z606SP','Z606SP','ZONE',500000.,1.,'ACS11_SL040', 'EDGES_TOPROCESS','Z606SPEDGES_TOSKIP','edges_done','bad_edges','state_edge_table','STATEFP','Z606SP_EDGE_ATT','Z606SP_CLIP_FACE');

  -- This procedure has 2 required parameters:
  --
  -- INPUT
  --  pstate            - default NULL to do entire Topology or statefp or countyfp
  --                      code when you wish to do just part of the Topology.
  --  pTopology         - the Topology to update with generalized edges
  --  prun_flag         - a short identifier to prepend to table names to make them unique.
  --  pedge_table       - 'EDGE' the name of the edge table (Topology prefix will NOT be prefixed)
  --  face              - 'FACE' the name of the face table (Topology will not be prefixed)
  --  pmethod           - the method either 'ZONE' or 'DP' for Douglas_Peuker.
  --  target_scale      - the scale ratio, typically 500000, 5000000 or 20000000.
  --  nice              - a parameter (default 1.) to pass to the Zone generalizer
  --                     or a threshold in meters to pass to the DP generalizer.
  --  pUSStates         - an input table of 52 US states with statefp and sdogeoemtry
  --                      columns (default 'ACS09_SL040')
  --  pEdges_Table      - a work table made once listing the edges to process
  --                      (default 'EDGES_TO_PROCESS').
  --  pskip_edges_table  - a table of edge_ids to skip (default NULL)
  --                       If this table is empty/does not exist it sis ignored.
  --  pDone_edges_table  - a work progress table used by the program default 'EDGES_DONE'
  --  pState_Edge_Table  - a work table used by the program default 'STATE_EDGE_INPUT'
  --  pEntityfp          - either 'STATEFP' or 'COUNTYFP'

--
-- Purpose: Simplifies and edits the topology for an entity or entities (usually States)

-- Reference: An original method devised by the author.
-- Calls: simplify_region, Table_exists
--
********************************************************************************
*/
sql_stmt                        VARCHAR2(4000);
vstate                          VARCHAR2(2);
state                           VARCHAR2(2) := NVL(pstate,'0');
Topology                        VARCHAR(20) := UPPER(pTopology);
array_state                     MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
Edge_table                      VARCHAR2(100) := UPPER(Topology) || '_'||UPPER(pEdge_attribute_Table);
Edge_id                         NUMBER;
xLL                             NUMBER;
yLL                             NUMBER;
xUR                             NUMBER;
yUR                             NUMBER;
quad_xLL                        NUMBER;
quad_yLL                        NUMBER;
quad_xUR                        NUMBER;
quad_yUR                        NUMBER;
temp                            NUMBER;
delta                           NUMBER := 0.001;  --used to be 0.001
dec_digits                      NUMBER :=6; -- rounds shape points (not nodes) using this

status                          VARCHAR2(4000);
v_topology                      VARCHAR2(20)  := UPPER(pTopology);
Edges_table                     VARCHAR2(100) := UPPER(run_flag) ||UPPER(pEdges_table);
Done_table                      VARCHAR2(100) := UPPER(run_flag) ||UPPER(pDone_edges_table);
Bad_table                       VARCHAR2(100) := UPPER(run_flag) ||UPPER(pbad_table);
pSkip_Table                     VARCHAR2(100) := UPPER(pSkip_Edges_Table);
US_State_table                  VARCHAR2(100) := UPPER(pEntity_table);
face                            VARCHAR2(100) := UPPER(pclip_face_table);
State_Edge_table                VARCHAR2(100) := UPPER(run_flag) ||UPPER(pstate_edge_table);

target_scale                    NUMBER := ptarget_scale;
nice                            NUMBER := pnice;
tolerance                       NUMBER := 0.5;
kount                           NUMBER :=0;
edges_todo                      NUMBER;
ratio                           NUMBER;
sum_vertices                    NUMBER;
progress                        PLS_INTEGER :=0;
no_done                         PLS_INTEGER;
tile                            PLS_INTEGER := 0;

method                         VARCHAR2(4) := UPPER(NVL(pmethod,'ZONE'));
entityfp                        VARCHAR2(8) := UPPER(pentityfp);
found_entityfp                  VARCHAR2(8);
result                         VARCHAR2(2000);

mbr_geom                        SDO_GEOMETRY;
cache_name                      VARCHAR2(20) := Topology||'topomapcache';
error_msg                       VARCHAR2(4000);
msg                             VARCHAR2(4000);
track_name                      VARCHAR2(20) := Topology||'_LS_TRACKING';
ok                              BOOLEAN;
subdivide                       BOOLEAN;
done                            PLS_INTEGER := 0;
the_time     timestamp;

    Procedure Track_App(msg VARCHAR2) as

    begin
      GZ_TOPOFIX.Track_App(msg,Topology,'LS');
    end;

    Procedure Track_App_Error(error_msg VARCHAR2) as

    begin
      GZ_TOPOFIX.Track_App_Error(error_msg,Topology,'LS');
    end;

    Procedure Set_Overall_MBR as
      begin
      
        IF GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(US_State_Table)
        THEN
           
           --original code using pretty much all the time Z<x>99IN_FSL040V
           
           sql_stmt := 'SELECT  sdo_geom.sdo_mbr(sdo_geom.sdo_buffer(sdo_aggr_mbr(sdogeometry), 500, .05))
                        FROM '||US_State_Table||' a WHERE a.'|| Entityfp ||'= :1';
           Execute Immediate sql_stmt into mbr_geom using vstate;          
        
        END IF;
        
        IF mbr_geom IS NULL
        OR NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(US_State_Table)
        THEN
        
           --Matt! 1/24/14
           --After getting an unhelpful reference to uninitialized collection for not having the state table
           --or the state table with the required states in it
           --on the 50th such time I added this
           --note that I have no idea why 500 meters is the buffer, or why a buffer at all.  Twilight zone to me.
        
           sql_stmt := 'SELECT SDO_GEOM.SDO_MBR(SDO_GEOM.SDO_BUFFER(SDO_AGGR_MBR(e.geometry), :p1, :p2)) '
                    || 'FROM ' || pTopology || '_edge$ e '
                    || 'WHERE '
                    || 'e.left_face_id = :p3 OR e.right_face_id = :p4 ';
                    
           EXECUTE IMMEDIATE sql_stmt INTO mbr_geom USING 500,
                                                          .05,
                                                          -1,
                                                          -1;
           
        END IF;
        
        xLL := mbr_geom.sdo_ordinates(1);
        yLL := mbr_geom.sdo_ordinates(2);
        xUR := mbr_geom.sdo_ordinates(3);
        yUR := mbr_geom.sdo_ordinates(4);

        if xUR < xLL then   -- Check 180 degree daeline.
          temp := xUR;
          xUR := xLL;
          xLL := temp;
        end if;
     end;
BEGIN

-- Protect this package from previous programs that may have used much memory
   dbms_session.free_unused_user_memory;

-- For testing it is useful to have automatic generation of the Tracking table
-- So put a undocumented hook in so this will not happen in production. When the
-- method is 'ZoNe' create the tracking work table if necessary.

   if pmethod = 'ZoNe' and NOT TABLE_Exists(Track_name) then

     GZ_business_utils.CREATE_GEN_XTEND_TRACKING_LOG(USER,Track_name);

   elsif NOT TABLE_Exists(Track_name) then

     dbms_output.put_line('FATAL Table ERROR: Line_Simplify: Tracking table does not exist');
     RAISE_APPLICATION_ERROR(-20001,'Tracking table:'||Track_name||': does not exist');
   end if;


-- Begin by printing the parameters !!

   msg := ':Begin:Line_Simplify:Parameters - for entity='||pEntityfp ||' set to=' ||pstate ||
          ' with 1) Topology='||Topology||
              ', 2) run_flag='||run_flag||
              ', 3) method='||pmethod||
              ', 4) target_scale='||target_scale||
              ', 5) nice='||nice||
   ', and tables 1) Entity_table='|| pEntity_Table||
              ', 2) temporary output Edges_table='||edges_table||
              ', 3) input Skip_edges_table (may be NULL)='||pSkip_Edges_table||
              ', 4) temporary output Done_edges_table='||Done_table||
              ', 5) deprecated parameter='||pBad_Table||
              ', 6) State_Edge_Table='||State_Edge_Table||
              ', 7) input Edge_Attribute_Table='||pEdge_attribute_table||
              ', 8) input Clip_face_table='||pclip_face_table||
   ', and parameter to drop the work tables='|| drop_work_tables;

   GZ_TOPOFIX.Track_App(SUBSTR(msg,1,455),Topology,'LS');

   if pEntityFp is NULL or pstate is NULL or Topology is NULL or run_flag is NULL or target_scale is NULL or nice is NULL then
      Track_App_Error(':FATAL Parameter ERROR:Line_Simplify:'||
                      'NULL Input parameter to SUPER, '||
                      'Entity:'||pEntityFp||': is:'||pstate||':scale:' ||target_scale || ':nice:' || nice);
   end if;

   if target_scale <=1 or nice <=0. then
      Track_App_Error(':FATAL Parameter ERROR:Line_Simplify:'||
                      'BAD Input value to SUPER, scale:' ||target_scale || ':nice:' || nice);
   end if;


--  Check all the tables

   CHECK_TABLES(state,run_flag,EntityFP,Topology,pEdge_attribute_table,pclip_face_table,pEntity_table,pSkip_Table,Edges_Table,Done_Table,State_Edge_table,drop_work_tables);

   if Table_Exists(pclip_face_table) = FALSE then
      Track_App_Error(':FATAL Table ERROR:Line_Simplify:'||
                      'Clip face Table ('||pclip_face_table||') does not exist/may not be NULL');
   end if;


-- Simplify map process many entities (usually states)

   if pstate = 0  then
     sql_stmt := 'SELECT distinct '|| Entityfp ||' FROM '||US_State_Table||' order by ' || Entityfp;
     EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO array_state;
   else

  -- or just a single entity
     array_state.extend(1);
     array_state(1) := pstate;
   end if;


   sdo_topo_map.set_max_memory_size (2147483648);
--------------------------------------------------------------------------------
-- This is a big loop but usually just does just 1 iteration

   FOR c in 1..array_state.COUNT LOOP -- Loop over states or other entities
      vstate := array_state(c);

      EXECUTE IMMEDIATE 'SELECT count(*),sum(vertices) from '||Edges_table||
                          ' WHERE STATE=:1'
        into edges_todo,sum_vertices using vstate;

      Track_App(':Processing Entity:Line_Simplify:Beginning entity ' || vstate || ' which has ' || edges_todo || ' edges');

      Set_Overall_MBR;

      Track_App(':Set MBR:Line_Simplify:For this entity ' || vstate ||
                ' using this MBR, LL:'|| XLL || ' : ' || yLL||' UR: '|| XUR || ' : ' || yUR);

--------------------------------------------------------------------------------
-- Loop over either the whole state or parts thereof using automatically generated
-- quads. Done is a flag: 0 = not done, 1=done and -1 = not done and subdivision necessary.
--

      WHILE  DONE <> 1 LOOP

-- We have to reset these each time for the program to load the correct quad
        quad_xLL := xLL;
        quad_yLL := yLL;
        quad_xUR := xUR;
        quad_yUR := yUR;

-- Create and Load a topomap
-- There is a complication. We need to ensure we get the aggregate MBR of all the edges that are
-- in a topomap.

        done := LOAD_TOPO_MAP_BY_QUAD(cache_name,vstate,Topology,quad_xLL,quad_yLL,quad_xUR,quad_yUR,tile,delta,pskip_Table, Done_table,Edges_Table);

--        the_time := current_timestamp;
        no_done := simplify_region(v_topology,cache_name,target_scale,nice,vstate,quad_xLL,quad_yLL,quad_xUR,quad_yUR,tolerance,Bad_Table,
                        pskip_Table, Done_table,Edges_Table,method,dec_digits);
--        dbms_output.put_line('Elapsed time : ' || (current_timestamp - the_time));
--dbms_output.put_line('ready to commit');
        -- Commit TOPO map


        BEGIN
           Track_App(':Simplify Region Complete:Line_Simplify:'||
                     no_done||' edges simplified for entity ' || vstate);

           sdo_TOPO_MAP.COMMIT_TOPO_MAP();
           Track_App(':Commit TopoMap:Line_Simplify:'||
                     'Topology commited for entity ' || vstate);

           COMMIT;
           EXCEPTION
             WHEN OTHERS THEN
             IF (INSTR(sqlerrm, 'no updatable TopoMap object has been created') != 0) THEN
                 NULL;
             ELSE
                dbms_output.put_line(SUBSTR(sqlerrm,1,200));
             END IF;
        END;

        sdo_TOPO_MAP.CLEAR_TOPO_MAP(cache_name);
        sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);
      END LOOP;

    END LOOP; -- end of entities loop

--------------------------------------------------------------------------------


    --Matt! 12/31/13
    --Commented this because
    --1. There's a topo validation in the GZ_LINESIM wrapper 2 seconds after this spot
    --2. The topology is implicitly validated on commit 2 seconds before this spot

    -- Now validate using Mathews Tile Validator

    --result := VALIDATE_TOPOLOGY_WITH_TILES(v_topology,face,vstate);


    --If result = 'TRUE' then
      --Track_App(':Success Validate TopoMap:Line_Simplify:'||
      --          'Final Topology validated for entity ' || vstate);
    --End if;
--------------------------------------------------------------------------------
-- And print completion messages

    msg := ':Finish:Line_Simplify:Completed. For entity ' || vstate;

    sql_stmt := 'SELECT SUM(sdo_util.getnumvertices(geometry))/' ||
                       'SUM(sdo_util.getnumvertices(new_geometry)) from '|| Edges_Table;
    execute immediate sql_stmt into ratio;

    msg := msg ||'. Input/output generalization ratio '  || ROUND(ratio,3);

    if ratio <= 2. then
        msg := msg ||' suggests that the input data was already generalized to the target scale';
    end if;
    Track_App(msg);
--------------------------------------------------------------------------------
--  CHECK_ALL_VERTICES(Edges_table,'GEOMETRY',Edges_table,'NEW_GEOMETRY',Report_table);
--  EXCEPTION
--   WHEN OTHERS THEN
--   Generate_Error_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
END SIMPLIFY;
--

FUNCTION LOAD_TOPO_MAP_BY_QUAD(cache_name VARCHAR2,vstate VARCHAR2,Topology VARCHAR2,
                   pxLL IN OUT NOCOPY NUMBER,pyLL IN OUT NOCOPY NUMBER,
                   pxUR IN OUT NOCOPY NUMBER,pyUR IN OUT NOCOPY NUMBER,tile IN OUT NOCOPY PLS_INTEGER,delta NUMBER,
                   pskip_Table     VARCHAR2 default NULL,
                   pDone_Table     VARCHAR2 default 'EDGES_DONE',
                   pEdges_Table    VARCHAR2 default 'EDGES_TOPROCESS') RETURN PLS_INTEGER AS

  tiles     MDSYS.SDO_LIST_TYPE;
  MBR       MDSYS.SDO_ORDINATE_ARRAY;
  current_tile  PLS_INTEGER := tile;

  xLL       NUMBER := pxLL;
  yLL       NUMBER := pyLL;
  xUR       NUMBER := pxUR;
  yUR       NUMBER := pyUR;
  temp      NUMBER;
  topo_entity_count   NUMBER := 100000;
  progress  PLS_INTEGER;
  last_digit PLS_INTEGER;
  error_msg VARCHAR2(4000);
  done      PLS_INTEGER :=-1;
  loops     PLS_INTEGER := 0;
  subdivide BOOLEAN := FALSE;

BEGIN

-- manipulate the current_tile number to get the next tile and its MBR

   WHILE done = -1 and loops < 20 LOOP
     loops := loops + 1;

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

-- Have to reset the extent for each invocation
     xLL      := pxLL;
     yLL      := pyLL;
     xUR      := pxUR;
     yUR      := pyUR;
     tiles := quad_tree(xLL,yLL,xUR,yUR,tile,subdivide);

-- Caller Forces subdivision of a large state (just for testing).

     if tile < 0 then
        tile :=0;
        xLL      := pxLL;
        yLL      := pyLL;
        xUR      := pxUR;
        yUR      := pyUR;
        subdivide := TRUE;
        tiles := quad_tree(xLL,yLL,xUR,yUR,tile,subdivide);
        subdivide := FALSE;
      end if;


      tile := tiles(2);
      error_msg := ':QUAD Tree:Line_Simplify:Topology loaded for entity ' || vstate ||' tile ' || tiles(1) || ' with MBR, LL:'|| xLL || ' : ' || yLL||' UR: '|| xUR || ' : ' || yUR;
      GZ_TOPOFIX.Track_App(error_msg,Topology,'LS');
      done := 0;

-- We have to get the aggregate MBR we want to use in sync with the Topomap MBR

      MBR := BUILD_MBR(Topology,vstate,xLL,yLL,xUR,yUR,0.05,pskip_Table,pDone_Table,pEdges_Table);

-- Get the aggregate of the quadtree desired rectangle and the MBR of the
-- edges that may interact with this rectangle

      for ii in 1..4 loop
        if ii < 3 then
--        Fix the 180 degree dateline
          if ii=1 and MBR(3) < MBR(1) then
            temp := MBR(1); MBR(1) := MBR(3); MBR(3) := temp;
          end if;
          MBR(ii) := MBR(ii)-delta;
          if ii=1 and xLL < MBR(1) then MBR(1) := xLL; end if;
          if ii=2 and yLL < MBR(2) then MBR(2) := yLL; end if;
        else
          MBR(ii) := MBR(ii)+delta;
          if ii=3 and xUR > MBR(3) then MBR(3) := xUR; end if;
          if ii=4 and yUR > MBR(4) then MBR(4) := yUR; end if;
        end if;
      end loop;


      BEGIN
          progress :=0;
-- Note we don't pass these deltas to simplify_region
          sdo_TOPO_MAP.LOAD_TOPO_MAP(cache_name,MBR(1),MBR(2),MBR(3),MBR(4), 'TRUE');
          progress :=1;
          error_msg := ':Load TopoMap:Line_Simplify:Topology loaded for entity ' || vstate ||' tile ' || tiles(1) || ' with MBR, LL:'|| MBR(1) || ' : ' || MBR(2)||' UR: '|| MBR(3) || ' : ' || MBR(4);
          GZ_TOPOFIX.Track_App(error_msg,Topology,'LS');
          subdivide := FALSE;
-- At present don't validate on input
--          sql_stmt := 'select  sdo_topo_map.validate_topology('''||v_topology||''',''N'') from dual';
--          EXECUTE IMMEDIATE sql_stmt into result;
          progress :=2;
--          error_msg := ':Validate TopoMap:Line_Simplify:Topology validated for entity ' || vstate ;
--          GZ_TOPOFIX.Track_App(error_msg,Topology,'LS');

-- If we fail we ask to subdivide the current tile.
-- Example:
--ii 1 quad now 11 X 104 y 30 xUR 108 yUR 32 next 11
--ii 2 quad now 12 X 100 y 30 xUR 104 yUR 32 next 12
--ii 3 quad now 121 X 102 y 30 xUR 104 yUR 31 next 121
--ii 4 quad now 122 X 100 y 30 xUR 102 yUR 31 next 122
--ii 5 quad now 1221 X 101 y 30 xUR 102 yUR 30.5 next 1221
--ii 6 quad now 1222 X 100 y 30 xUR 101 yUR 30.5 next 1222
--ii 7 quad now 1223 X 101 y 30.5 xUR 102 yUR 31 next 1223
--ii 8 quad now 1224 X 100 y 30.5 xUR 101 yUR 31 next 122
--ii 9 quad now 12241 X 100.5 y 30.5 xUR 101 yUR 30.75 next 12241
--ii 10 quad now 12242 X 100 y 30.5 xUR 100.5 yUR 30.75 next 12242
--ii 11 quad now 12243 X 100.5 y 30.75 xUR 101 yUR 31 next 12243
--ii 12 quad now 12244 X 100 y 30.75 xUR 100.5 yUR 31 next 122
--ii 13 quad now 123 X 102 y 31 xUR 104 yUR 32 next 123
--ii 14 quad now 124 X 100 y 31 xUR 102 yUR 32 next 12
--ii 15 quad now 1241 X 101 y 31 xUR 102 yUR 31.5 next 1241
--ii 16 quad now 1242 X 100 y 31 xUR 101 yUR 31.5 next 1242
--ii 17 quad now 1243 X 101 y 31.5 xUR 102 yUR 32 next 1243
--ii 18 quad now 1244 X 100 y 31.5 xUR 101 yUR 32 next 12
--ii 19 quad now 13 X 104 y 32 xUR 108 yUR 34 next 13
--ii 20 quad now 14 X 100 y 32 xUR 104 yUR 34 next 1


        EXCEPTION
        WHEN OTHERS  then
          sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);

          IF UPPER(SQLERRM) LIKE '%MEMORY%' THEN
             done := -1;
             error_msg := ':Load TopoMap:Line_Simplify:Topology load fail for entity ' || vstate ||' quad ' || tiles(1) || ' with MBR, LL:'|| XLL || ' : ' || yLL||' UR: '|| XUR || ' : ' || yUR;
          GZ_TOPOFIX.Track_App(error_msg,Topology,'LS');
             subdivide := TRUE;
             tile := tiles(1);
          ELSE
          dbms_output.put_line(sqlerrm);   -- but this does!!!
          if progress = 0 then
            error_msg := 'FATAL ERROR:Line_Simplify: Problem Loading TopoMap:' || sqlerrm;
          elsif progress = 1 then
            error_msg := 'FATAL ERROR:Line_Simplify: Problem Validating Topology:' || sqlerrm;
          end if;

          GZ_TOPOFIX.Track_App_Error(error_msg,Topology,'LS');
          END IF;
        END;


    END LOOP;

-- Have to return the extent to process from the topomap
    pxLL      := xLL;
    pyLL      := yLL;
    pxUR      := xUR;
    pyUR      := yUR;

    if tile = 1 then
      done := 1;
    end if;
    dbms_output.put_line('done ' || done || ' tile ' || tile);
    RETURN done;
END;
--
Function get_x_or_y(ptile NUMBER,pLLxy NUMBER,pURxy NUMBER,get_x BOOLEAN,delta IN OUT NOCOPY NUMBER) return number as

-- From the quad tree til number construnct either an x or a y
    xy        number  := pLLxy;
    tile      number  := ptile;
    subtile   number;
    halfxy    number  := ABS(pURxy-pLLxy);
    divisor   number  := 1.;
    levels   pls_integer := 1;
  Begin
-- Start with tile=143 for example

     While divisor*10. < tile loop        -- 10,100,1000
        divisor := divisor * 10.;          -- 10,100
        levels := levels+1;               -- 2,3
     End loop;

     For ii in 2..levels loop             -- divisor is now 100
        halfxy := halfxy*0.5;
        tile := MOD(tile,divisor);           -- Change say 143 to 43, and then to 3
        divisor := divisor*0.1;              -- 10,1
        subtile := TRUNC(tile/divisor);      -- 4
        if get_x then
          xy := xy + MOD(subtile,2)*halfxy;
        else
          xy := xy + TRUNC(subtile/3)*halfxy;
        end if;
     End loop;
     delta := halfxy;
     Return xy;    -- This is the lower left and the UR is + delta
  End get_x_or_y;
  --
Function quad_tree(xLL IN OUT NOCOPY NUMBER,yLL IN OUT NOCOPY NUMBER,
                    xUR IN OUT NOCOPY NUMBER,yUR IN OUT NOCOPY NUMBER,ptile PLS_INTEGER,subdivide BOOLEAN) RETURN MDSYS.SDO_LIST_TYPE AS


--          ___________               _______________
--         |           |             |       |       |
--         |           |             |  14   |  13   |     Y := TRUNC(# / 3)
--         |     1     |             |_______|_______|
--         |           |             |       |       |     X := MOD(#,2)
--         |           |             |  12   |  11   |
--         |___________|             |______ |_______|
--
--
--          ___________               _______________
--         |           |             |144|143|134|133|
--         |           |             |---|---|---|---|
--         |     1     |             |142|141|132|131|
--         |           |             --------|-------|
--         |           |             |       |       |
--         |           |             |  12   |  11   |
--         |___________|             |______ |_______|
--
-- Just recursively set an extent using a current tile number. Thus:
--     zero -> 1 which meand the whole extent
--     11   -> means the NW corner and there are 3 more to do, 12,13 and 14
--     111  -> means the NW corner of the NW corner and there are 3 more to do
--             112,113 and 114 and then we go to 12, 13 and 14
-- So whenever we hit a 4 we drop a digit and move up to the next level

  current_tile   PLS_INTEGER := ptile;
  tile_to_pass   PLS_INTEGER;
  deltax         NUMBER;
  deltay         NUMBER;
  last_digit     PLS_INTEGER;
  Begin

    If current_tile is NULL then
      current_tile :=0;
    End if;
     current_tile := current_tile+1;
     if current_tile = 1 and NOT subdivide then        -- Whole extent
        RETURN MDSYS.SDO_LIST_TYPE(current_tile,current_tile);
     elsif current_tile = 1 and subdivide then
     current_tile :=2;
     end if;

-- If we are to subdivide bump down to a smaller square
     if subdivide then
        current_tile := (current_tile-1)*10 + 1;
     end if;

-- Using the current tile we have to subdivide
     xLL := get_x_or_y(current_tile,xLL,xUR,TRUE,deltax);
     xUR := xLL+deltax;
     yLL := get_x_or_y(current_tile,yLL,yUR,FALSE,deltay);
     yUR := yLL+deltay;

-- Now setup the tile for next time. So for example, change 134 to 13.

     last_digit := MOD(current_tile,10);
     tile_to_pass := current_tile;
     WHILE last_digit = 4 loop
       tile_to_pass := TRUNC(tile_to_pass/10);
       last_digit := MOD(tile_to_pass,10);
     End Loop;

-- When we return a current tile of 1 we are done
  RETURN MDSYS.SDO_LIST_TYPE(current_tile,tile_to_pass);
  End quad_tree;
--
PROCEDURE CREATE_GZ_SUPER_DONE(
      p_schema         VARCHAR2 DEFAULT NULL,
      p_table_name     VARCHAR2) AS

  BEGIN
      Create_GZ_SUPER_Table(p_schema,p_table_name,'DONE');
  END CREATE_GZ_SUPER_DONE;
--
PROCEDURE CREATE_GZ_SUPER_STATE(
      p_schema         VARCHAR2 DEFAULT NULL,
      p_table_name     VARCHAR2) AS

  BEGIN
      Create_GZ_SUPER_Table(p_schema,p_table_name,'STATE');
  END CREATE_GZ_SUPER_STATE;
  --
PROCEDURE CREATE_GZ_SUPER_EDGES(
      p_schema         VARCHAR2 DEFAULT NULL,
      p_table_name     VARCHAR2) AS

  BEGIN
      Create_GZ_SUPER_Table(p_schema,p_table_name,'EDGES');
  END CREATE_GZ_SUPER_EDGES;

--
PROCEDURE CREATE_GZ_SUPER_TABLE(p_schema IN VARCHAR2 DEFAULT NULL, p_table_name IN VARCHAR2,Table_Abbrev VARCHAR2)
   AS

      --Matt! 4/24/12

      psql          VARCHAR2(4000);
      v_schema      VARCHAR2(4000);

   BEGIN

      dbms_output.put_line('Tabbrev ' || Table_Abbrev );
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1 of 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_SUPER_'||Table_Abbrev||': Starting ');
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
                               'GZ_TYPES.GZ_SUPER_'||Table_Abbrev,
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
      GZ_business_utils.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_SUPER_'||Table_Abbrev||  ': Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_SUPER_TABLE;
   --
PROCEDURE CHECK_TABLES(pstate VARCHAR2,run_flag VARCHAR2,pEntityFP VARCHAR2,Topology VARCHAR2,pEdge_attribute_table VARCHAR2,pclip_face_table VARCHAR2,pEntity_table VARCHAR2,pSkip_Table IN OUT NOCOPY VARCHAR2,pEdges_Table VARCHAR2,pDone_Table VARCHAR2,pState_Edge_table VARCHAR2,drop_work_tables VARCHAR2)  AS

   error_msg         VARCHAR2(4000);
   sql_stmt          VARCHAR2(4000);
   found_entityfp    VARCHAR2(8);
   Skip_Table        VARCHAR2(100) := UPPER(run_flag)|| UPPER(pskip_table);
   ok                BOOLEAN;
   p                 PLS_INTEGER;

    Procedure Track_App_Error(error_msg VARCHAR2) as

    begin
      GZ_TOPOFIX.Track_App_Error(error_msg,Topology,'LS');
    end;
BEGIN

-- Check the entityFp matches the clip_face table

   if pstate is not NULL and pstate <> 0 then

      ok := Column_exists(pEntityFP,pclip_face_table);

      If NOT ok then
        Track_App_Error(':FATAL ERROR:Line_Simplify:'||
                        'Entity -'||pEntityFP||'- was not found in the face attribute table ('||pclip_face_table||')');

      Else
      sql_stmt := 'SELECT ' || pEntityFP || ' from ' || pclip_face_table || ' WHERE rownum < 2';
      EXECUTE IMMEDIATE sql_stmt into found_entityfp;
        if found_entityfp <> pstate then
           Track_App_Error(':FATAL ERROR:Line_Simplify:'||
                           'Entity -'||pEntityFP||'- does not match the FIPS code ('||pstate||') specified in the first parameter pstate');
        end if;
      End If;
   end if;

-- New. Help the user - ignore empty Skip tables and don't error off.
   p := INSTR(pSkip_table,'(');
   if pSkip_Table is NOT NULL then
      p := INSTR(pSkip_table,'(');
      if p = 0 and Table_Exists(Skip_table) = FALSE then
        pSkip_Table := NULL;
      elsif p <> 1 then
        pSkip_table := Skip_table;
      end if;
  end if;

    if Table_Exists(pEntity_table) = FALSE then
      Track_App_Error(':FATAL_ERROR: Line_Simplify:'||
                      'Entity Table ('||pEntity_Table||') does not exist/may not be NULL');
   end if;

   if Table_Exists(pEdge_attribute_table) = FALSE then
      Track_App_Error(':FATAL ERROR:Line_Simplify:'||
                      'Edge attribute Table ('||pEdge_attribute_table||') does not exist/may not be NULL');
   end if;

-- First make sure caller has made our precursor tables
-- The done_table with drop work tables parameter = FALSE enables restarts.

  --   if drop_work_tables = 'TRUE' and Table_Exists(pState_Edge_table) = TRUE then
 --    EXECUTE IMMEDIATE 'DROP TABLE ' || pState_Edge_table;
     CREATE_GZ_SUPER_STATE(NULL,pState_Edge_table);
 --  end if;
   if pEdges_Table is NOT NULL then

        CREATE_GZ_SUPER_EDGES(NULL,pEdges_table);

      Run_Make_Edge_Table(Topology,run_flag,pEdge_attribute_Table,pclip_face_table,pEdges_Table,pState_Edge_table,pEntityfp);

   elsif pEdges_Table is NULL then
       Track_app_Error(':FATAL ERROR:Line_Simplify:'||
                       'The parameter Edges_Table ('||pEdges_table||') may not be NULL');
   end if;


   CREATE_GZ_SUPER_DONE(NULL,pDone_table);



END CHECK_TABLES;
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
  geom      MDSYS.SDO_GEOMETRY;
  sql_stmt  VARCHAR2(4000);
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


   a_1       NUMBER := 0.9933056431085355608716884491956183391161;
   a_2       NUMBER := 0.009973972089654263505971285993829439678765;
   a_3       NUMBER := 0.00008445133481869081114188534958332938725393;
   a_4       NUMBER :=-0.0000000207345260587865496039317049547208138981;
   recip6    NUMBER := 0.1666666666666666666666666666666666666667;
   recip120  NUMBER := 0.008333333333333333333333333333333333333333;
   s1        NUMBER := 0.00019841269840709526914091841438;  -- 0-12 max_error is 3.04 e-22
   s2        NUMBER := 0.00000275573139043280323218428038;
   s3        NUMBER := 0.00000002503585871796003296498373;
   a1          NUMBER;
   a2          NUMBER;
   c1          NUMBER;
   c2          NUMBER;
   tt          NUMBER;
   y           NUMBER := abs(y1);
   yy          NUMBER;
   yy2         NUMBER;
   yy3         NUMBER;
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
   bad         NUMBER;

BEGIN

-- Handle case when data is projected.

      if SRID is NULL or SRID <> 8265. then
--  eof maybe ?
         dx := ROUND(x2-x1,9);
         dy := ROUND(y2-y1,9);
         length := sqrt(dx*dx+dy*dy);
         RETURN length;
      end if;

-- Handle large angles (> 0.1 degrees)
      dx := x2-x1;
      dy := y2-y1;

      if (abs(dx) +  abs(dy) > 0.1) then
        gcd := fast_vincenty_gcd(x1,y1,x2,y2);
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
       yy3 := yy*yy2;
       siny := yy -yy3*(recip6 + yy2*(recip120 - yy2*(s1 + yy2*(s2 - yy2*s3))));
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
        c1 := 0.99999882342768;
        c2 := 0.00335602313364;
--        t := 31.;   -- centerpoint
      elsif y >= 20.5 then
--        p := 4;  -- for 20.5.. 27.4999 degrees
        c1 := 0.99999954254499;
        c2 := 0.00335267920716;
--        t := 24.;
      elsif y >= 13.5 then
--        p := 3;  -- for 13.5 .. 20.4999 degrees
        c1 := 0.99999987773583;
        c2 := 0.00335000490016;
--        t := 17.;
      elsif y >= 6.5 then
--        p := 2;  -- for 6.5 .. 13.4999 degrees
        c1 := 0.99999998463761;
        c2 := 0.00334815672;
--        t := 10.;
      else
--        p := 1;  --for 0 .. 6.4999 degrees
        c1 := 0.99999999981136;
        c2 := 0.00334723822659;
--        t := 3.;
      end if;
   elsif y < 52.5 then
      if y >= 45.5 then
--        p := 8;  --for 45.5 .. 52.4999 degrees
        c1 := 0.99999456031055;
        c2 := 0.00336625111507;
--        t := 49.;
      elsif y >= 41.5 then
--        p := 7;  --for 41.5 .. 45.4999 degrees This range is different
        c1 := 0.99999581110019;
        c2 := 0.00336390796249;
--        t := 45.;      -- nominal centerpoint for this unusual range
      else
--        p := 6;  --for 34.5 .. 41.4999 degrees
        c1 := 0.99999759542303;
        c2 := 0.00335984120472;
--        t := 38.;
      end if;
   elsif y < 59.5 then
--     p := 9;  --for 52.5 .. 59.4999 degrees
     c1 := 0.99999207002972;
     c2 := 0.00337022115208;
--     t := 56.;
   elsif y < 66.5 then
--      p := 10;  -- for 59.5 .. 66.4999 degrees
      c1 := 0.99998940805689;
      c2 := 0.00337382242390;
--      t := 63.;
   else
--      p := 11;  -- 66.5 .. 90 degrees
      c1 := 0.99998688314480;
      c2 :=0.00337683963728;
--      t := 70.;
   end if;

   cosy := cosyy* (c1+ siny*siny*c2);
--   tt := (y-t);
--   dy := dy*(a1+ a2*siny2*siny2 + tt*tt*b);  -- see comment below
--------------------------------------------------------------------------------
-- NEW The 4 a coefficents replace all of the code above for a1,a2
   sinysq := siny2*siny2;
   cos3y := cosyy*(1.-4.*sinysq);
   cos6y := 2.*cos3y*cos3y -1.;
   dy := dy * (a_1+ sinysq*(a_2 + a_3*sinysq) + a_4*cos6y);

-- The variation of dy with distance when dy >= 0.1 is not known yet
--   if dy >= .099 then
--      dy := dy * (1.- (35-y)*0.00000002);
--   end if;
--------------------------------------------------------------------------------
   if abs(dy) >=  0.0001 then
      cosy2 := cosy - siny * dy*deg2rad; -- small angle approximation for cos(y2)
-- Very bad Oracle error: ORA-03113: end-of-file on communication channel
      if  abs(dx) < 1.E-20 then
         dx :=0.0;
      end if;
      gcd := sqrt(dy*dy + dx*dx*cosy*cosy2) * dist_factor;
   else
      if abs(dy) < 1.E-20 then
         dy := 0.0;
      end if;
      gcd := sqrt(dy*dy + dx*dx*cosy*cosy) * dist_factor;
   end if;

   RETURN gcd;

END FAST_DISTANCE;
--
PROCEDURE COPY_GEOM_COORDINATES(XYORD IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,pstart PLS_INTEGER, Iend PLS_INTEGER,toXYORD IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,next IN OUT NOCOPY PLS_INTEGER) AS

--   Copy XY coordinates from one ordinate array to another.
--   Next is the last coordinate position in the output array. So it begins
--   at zero. It is updated by this procedure.
   istart   PLS_INTEGER := pstart;

BEGIN

-- Ensure we don't make a duplicate vertex
--     if next > 1  and istart < XYord.count and
--                      toXYord(next-1) = Xyord(istart+1) and
--                      toXYord(next) = Xyord(istart+2) then
--        istart := istart + 2;
--     end if;
     For ii in istart..iend loop
       next := next +1;
       if next > toXYOrd.count then
           toXYOrd.extend(100);
       end if;
       toXYORD(next) := XYOrd(ii);
     End loop;

END;
--
FUNCTION ASSEMBLE_EDGE(Status IN OUT NOCOPY VARCHAR2, Edge_id NUMBER,start_node NUMBER,end_node NUMBER,Segments IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,pscale NUMBER,nice NUMBER,nearby_edges MDSYS.SDO_LIST_TYPE,orig_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
     gen_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,Edges_Table VARCHAR2,method VARCHAR2 default 'ZONE',Topology VARCHAR2,target_scale NUMBER) RETURN NUMBER AS

-- Assemble pieces of an edge at different scales together into a single
-- linestring that doesn't break topology.
--
-- Get a list of triplets: a) new segments that fail
--                         b) old corresponding start segment,
--                         c) old corresponding end segment
-- assemble an edge from new generalized portions at different scales.
--
  XYORD         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  GenXYORD      MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  origXYORD     MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  partXYORD     MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  newXYORD      MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  checkXYORD    MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
  new_Geometry  MDSYS.SDO_GEOMETRY;
  part_Geometry MDSYS.SDO_GEOMETRY;
  Geometry      MDSYS.SDO_GEOMETRY;
  Geom          MDSYS.SDO_GEOMETRY;
  check_geometry MDSYS.SDO_GEOMETRY;
  check_geometry2 MDSYS.SDO_GEOMETRY;

  ISegments     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  Self_segs     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

  loops         PLS_INTEGER :=0;
  last          PLS_INTEGER := 0;
  next          PLS_INTEGER := 0;
  place         PLS_INTEGER;
  istart        PLS_INTEGER;
  iend          PLS_INTEGER;
  pos           PLS_INTEGER;
  gseg          NUMBER;
  lo_v          NUMBER;
  hi_v          NUMBER :=0;
  lo2           NUMBER;
  hi2           NUMBER;
  scale         NUMBER := pscale;
  tolerance     NUMBER := 0.05;
  threshold     NUMBER := nice;
  sql_stmt      VARCHAR2(4000);
  minimum_len   NUMBER :=0.;
  edgelen       NUMBER :=0.;
  bad_id        NUMBER;
  vertice_count NUMBER := 0.0;
  thousand      NUMBER := 100000.;
  last_loop     PLS_INTEGER := 15;
  loop_counter  PLS_INTEGER := 0;
  last_count    PLS_INTEGER := 4;
  new_count     PLS_INTEGER := 4;
  last_hi       PLS_INTEGER := 0;
  seg1          PLS_INTEGER;
  seg2          PLS_INTEGER;
  looper        PLS_INTEGER;
  j             PLS_INTEGER;
  k             PLS_INTEGER;
  n             PLS_INTEGER;
  temp          PLS_INTEGER;
  ibeg          PLS_INTEGER := 1;
  ifin          PLS_INTEGER;
  pos_save      PLS_INTEGER;
  ungen_seg     PLS_INTEGER;
  gen_seg       PLS_INTEGER;
  new_segs      PLS_INTEGER;
  ignore_these  PLS_INTEGER;
  gseg1         PLS_INTEGER;
  gseg2         PLS_INTEGER;
  total_self    PLS_INTEGER :=0;
  total_near    PLS_INTEGER :=0;
  segs_so_far   PLS_INTEGER := TRUNC(Segments.count/6);
  len           NUMBER;
  effective_scale NUMBER := 0.0;
  SRID          NUMBER := Orig_geometry.SDO_SRID;
  done          BOOLEAN := FALSE;
  backup_once   BOOLEAN := TRUE;
  once          BOOLEAN := TRUE;
  error_msg     VARCHAR2(1000);
  time1         date;
  no_of_coords  PLS_INTEGER;

BEGIN

   status  := 'TRUE';
   XYOrd.extend(100);
   OrigXYord := Orig_geometry.sdo_ordinates;
   n := OrigXyord.count;

-- Generalized at target scale with one of more intersections with nearby edges
   GenXyOrd := Gen_geometry.sdo_ordinates;
--   dbms_output.put_line('IN Assemble edge' || scale || ' gen count ' || genxyord.count);

   WHILE loops < Segments.count LOOP
      new_count := 4;
      last_count := 4;   -- last coordinate count
      gseg := Segments(loops+1);  -- get the generalized segment that is the problem
      last_hi := hi_v;
      lo_v := Segments(loops+2);  -- the low vertex or segment in the original data
                                  -- that corresponds to the generalized segment
--                              are
--                          ____________
--             these       /             \    the original ungeneralized segments
--                       /                 \
--               lo_v   +--------------------+ hi_v
--                              gseg (generalized segment)
--
      hi_v := Segments(loops+3);  -- the high vertex or segment in the original data
      bad_id := Segments(loops+4);  -- nearby id that is in the way, seg2 for self interesect
      lo2 := Segments(loops+5);
      hi2 := Segments(loops+6);
      loops := loops + 6;
--      dbms_output.put_line('loops ' || loops || ' NEXT ' || next || ' gseg ' || gseg ||  ' lo ' || lo_v || 'last hi ' || last_hi || ' hi_v ' || hi_v || ' scale ' || scale || ' last ' || last || ' bad_id ' || bad_id);

      IF lo_v >= last_hi THEN

-- Copy and append the generalized coordinates before the first intersection
-- into the outbuf buffer. Note we may just copy a vertex.

        istart := last+1;
        iend := gseg*2;
        if iend > istart  then
          copy_Geom_coordinates(GenXYord,istart,iend,XYord,next);
--         dbms_output.put_line('copying a portion from ' || istart || ' to ' || iend || ' gen ' || genxyord.count || ' next ' || next);
          ifin := 2*lo_v;
          ibeg := ifin + 1;
        end if;

        last := iend+2;

-- Now generalize a portion that breaks topology by intersecting (itself or another edge).

        partXYOrd.trim(partXYOrd.count);
        place := lo_v*2-2;    -- The coordinate to start is 2*vertex -1

        if hi_v*2 < place then     -- this would be an error
          dbms_output.put_line('id ' || edge_id || ' lo ' || lo_v || ' hi ' || hi_v || ' place ' || place);
        end if;
        partXYOrd.extend(hi_v*2-place);
-- Extract the problem region of the edge that needs to be generalized at a larger scale

        For ii in place+1..hi_v*2 loop
          partXyord(ii-place) := origXYOrd(ii);
--          dbms_output.put_line('ii ' || ii || ' ' || origXYOrd(ii));
        End Loop;
--        dbms_output.put_line('PART now is a from ' || (place+1) || ' to ' ||(hi_v*2));
        part_geometry :=  MDSYS.SDO_GEOMETRY(2002,SRID,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),partXYORD);
--        sql_stmt := 'INSERT into points values(:1,:2)';
--        execute immediate sql_stmt using lo_v*100,part_geometry;
--        commit;

        scale := pscale;
        loop_counter := 0;
        last_count := 4; --new_count:= partXYORD.count;
        edgelen := GZ_UTIL_ZONE.accurate_length(partXYOrd);
        total_near :=0;
--    dbms_output.put_line('entering outer loop'|| new_count || ' ISEGZ ' || isegments.count);
        WHILE (loop_counter = 0 or ISegments.count > 0 or self_segs.count > 0)  LOOP
--dbms_output.put_line('OUTER loop: scale now is ' || scale || ' part ccount ' || new_count || 'total self ' || total_self);
--dbms_output.put_line('iseg count ' || isegments.count || ' self ' || self_segs.count);
-- This loop is just to ensure the vertex count increases.
          While loop_counter < last_loop Loop

              loop_counter := loop_counter + 1;
--        Keep reducing the scale including the 1st time because
--        Assemble edge was called because the target scale failed.
--              if once or loop_counter <> 1 then  -- not sure about this
              get_scale(threshold,scale,target_scale,edgelen);
--              once := FALSE;
--              end if;
--dbms_output.put_line('scale now is ' || scale || ' next ' || next || ' target was ' || target_scale|| ' loop ' || loop_counter ||  ' LL ' || last_loop);




              if loop_counter < last_loop then

                if UPPER(method)='DP' then

                   new_geometry := sdo_util.simplify(part_geometry,threshold,tolerance);
                else

                   new_geometry := GZ_UTIL_ZONE.line_simplify(part_geometry,scale,nice,minimum_len,edgelen,edge_id,-1.,Topology,target_scale);
                end if;

                newXYord := new_geometry.sdo_ordinates;

                new_count := newXYOrd.count;

--                dbms_output.put_line('NEW_count ' || new_count|| ' last ' || last_count);
--                for jk in 1..trunc(newxyord.count/2) loop
--                   dbms_output.put_line('x ' || round(newxyord(jk*2-1),7) || ' y ' || round(newxyord(jk*2),7));
--                end loop;

                exit when new_count <> last_count ;
                last_count := new_count;
              else
--                dbms_output.put_line('set new geom to original data for ' || edge_id || ' ' || scale || ' ' || loop_counter);
-- use original data if it fails at lowest scale
                new_geometry := part_geometry;
                newXYord := new_geometry.sdo_ordinates;

              end if;
          End Loop;
--dbms_output.put_line('exited from inner loop');
-- Check for nearby edges that we encroach upon.

          ISegments := GET_SPLIT_VERTICES(edge_id,start_node,end_node,new_geometry,orig_geometry,
                                          nearby_edges,Edges_Table,part_geometry);
--          for pp in 1..isegments.count loop
--            dbms_output.put_line('ppsegments ' || isegments(pp));
--          end loop;

          if isegments.count > 0 then
            total_near := total_near+1;
          end if;
-- Try and fix a difficult situation when this part of the edge is essentially
-- straight and we need either the 2nd or 2nd to last vertex.
--
--               + Under very high magnification (*50 or more) , trying to generalize
--               |  this vertical segment, a straight line is expected
--               |                            but the arrowed vertex is needed
--               | +-----------------+        to miss the corner.
--         --->  + |
--                \|
--                 +

-- try adding vertex 2 first
/*
          if isegments.count > 0 and new_count =4 and PartXYord.count > 4 and once then
             newXYord.extend(2);
             newXYOrd(6) := NewXyord(4);
             newXYOrd(5) := NewXyord(3);
             newXYOrd(4) := partXYOrd(4);
             newXYOrd(3) := partXYOrd(3);
             new_Geometry.sdo_ordinates := newXYOrd;
             ISegments := GET_SPLIT_VERTICES(edge_id,start_node,end_node,new_geometry,orig_geometry,
                                          nearby_edges,Edges_Table,part_geometry);
--             dbms_output.put_line('>>>Now isegments count ' || isegments.count || ' new ' || newxyord.count);

--And now try the second to last instead

          if isegments.count > 0  then
             newXYOrd(4) := partXYOrd(partXYOrd.count-2);
             newXYOrd(3) := partXYOrd(partXYOrd.count-3);
             new_Geometry.sdo_ordinates := newXYOrd;
             ISegments := GET_SPLIT_VERTICES(edge_id,start_node,end_node,new_geometry,orig_geometry,
                                          nearby_edges,Edges_Table,part_geometry);
--             dbms_output.put_line('>>>now isegments count ' || isegments.count);
          end if;
          once := FALSE;
          end if;
*/
-- We have to construct the whole geometry for checking
-- Topologically there are 3 or 4 pieces to copy.
-- In most cases there is the before, the intersectors and the after - 3 pieces.
-- But in some cases there is an in_between between the intersectors.

-- Bypass the common vertex that we already have, but copy to the end.
             istart := 3;
             iend := newXYOrd.count;
             new_segs := Trunc((iend-2)/2) ;
             pos := next;
             if checkxyord.count < pos then
               checkxyord.extend(pos);
             end if;
             for ii in 1..pos loop
                checkxyord(ii) := Xyord(ii);
             end loop;
--             DBMS_OUTPUT.PUT_LINE('ccopying from 3 ' || istart || ' iend ' || iend || ' pos ' || pos);
             copy_Geom_coordinates(newXYord,istart,iend,checkXYord,pos);
--             temp := checkXYOrd.count-pos;
--             checkXYord.trim(temp);

--             check_geometry := MDSYS.SDO_GEOMETRY(2002,SRID,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),checkXYOrd);
--  sql_stmt := 'INSERT into points values(:1,:2,:3)';
--        execute immediate sql_stmt using 1,check_geometry,'before';
--        commit;
--             checkXYord.extend(temp);
--   DBMS_OUTPUT.PUT_LINE('pos is ' || pos || ' new count ' || checkXYord.count);

-- Copy and append remaining generalized coordinates

             istart := 2*gseg+3;
             iend := GenXYOrd.count;
--            DBMS_OUTPUT.PUT_LINE('ccopying from istart ' || istart || ' iend ' || iend || ' pos ' || pos);
             pos_save := pos;
             copy_Geom_coordinates(GenXYord,istart,iend,checkXYord,pos);
             temp := checkXYOrd.count-pos;
             checkXYord.trim(temp);
  --           for jp in 1..TRUNC(checkXYOrd.count/2) loop
  --              dbms_output.put_line('x ' || round(checkxyord(jp*2-1),7)||','|| round(checkxyord(jp*2),7)||',');
  --           end loop;
             check_geometry := MDSYS.SDO_GEOMETRY(2002,SRID,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),checkXYOrd);
             self_segs := check_for_self_intersect(check_Geometry);
--             dbms_output.put_line('after check_for_self_intersect ' || self_segs.count ||' scale now is ' || scale);

-- Now if the segments that we just created are not in this list we have succeeded.
-- They begin at gseg1 := TRUNC(next/2);
-- They end at gseg2 := TRUNC(pos_save/2).
             if self_segs.count > 0 then
                gseg1 := TRUNC(next/2);
                gseg2 := TRUNC(pos_save/2);
                ignore_these := 0;
                for ij in 1..self_segs.count loop
                seg1 := TRUNC(self_segs(ij)/thousand);
                seg2 := self_segs(ij) - seg1*thousand;
--                DBMS_OUTPUT.PUT_LINE('intersecting segs are :' || seg1 || ' ' || seg2);
--                   if seg1 = gseg1 or seg2 = gseg1 or seg2 = gseg1 or seg2 = gseg2 then
                   if seg1 = gseg1 or seg2 = gseg1 or seg2 = gseg1 or seg2 = gseg2 or (seg1 = bad_id and bad_id < gseg) then
                      ignore_these := ignore_these + 1;
                   end if;
                end loop;
                if ignore_these = 0 then
                   self_segs.trim(self_segs.count);
                end if;
                if self_segs.count > 0 then
                   total_self := total_self+1;
                end if;
             end if;
             checkXYord.extend(temp);

--          DBMS_OUTPUT.PUT_LINE('Segments ' || ISegments.count ||  ' self seg count ' || self_segs.count || ' loop counter ' || loop_counter);

-- If we fail completely,(even no generalization doesn't work) then a prior edge
-- must have been generalized incorrectly.
--dbms_output.put_line('>>>>Loop counter ' || loop_counter || ' LAST ' || last_loop || ' iseg ' || isegments.count || ' self ' || self_segs.count);
          if loop_counter = last_loop then
             if ISegments.count > 0 then
               error_msg := 'WARNING'||edge_id ||':Line Simplify:Failed with no generalization intersects edge ' || Isegments(4);
               GZ_TOPOFIX.Track_App(error_msg,Topology,'LS');

--               RAISE_APPLICATION_ERROR(-20001,'BAD');
               RETURN NULL;
             end if;

          end if;

          if (Isegments.count = 0 and self_segs.count =0) or loop_counter = last_loop  then

             newXYord := new_geometry.sdo_ordinates;
-- Bypass the common vertex that we already have, but copy the end.
             istart := 3;
             iend := newXYOrd.count;
             ifin := 2*hi_v;
             ibeg := ifin + 1;
             copy_Geom_coordinates(newXYord,istart,iend,XYord,next);
--                DBMS_OUTPUT.PUT_LINE('Copying from 3 ' || istart || ' iend ' || iend || ' next ' || next);
--             temp := xyord.count -next;
--             xyord.trim(temp);
--check_geometry := MDSYS.SDO_GEOMETRY(2002,SRID,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),XYOrd);
--                     sql_stmt := 'INSERT into points values(:1,:2)';
--        execute immediate sql_stmt using -2,check_geometry;
--        commit;
--             xyord.extend(temp);
--                DBMS_OUTPUT.PUT_LINE('now next ' || next || ' loops ' || loops || ' seg count ' || segments.count||' last ' || last || ' gen ' || genXyOrd.count);
-- Copy and append remaining coordinates already generalized at target scale
               if loops = Segments.count then
                  if XYord(next-1) <> origXYord(n-1) or XYord(next) <> origXYord(n) then
                  istart := last+1;
                  iend := genXYOrd.count;
                  copy_Geom_coordinates(genXYord,istart,iend,XYord,next);
--                  DBMS_OUTPUT.PUT_LINE('Copying from istart ' || istart || ' iend ' || iend || ' next ' || next);

                  end if;
                 done := TRUE;
               end if;

-- We have completed generalizing this portion;
               exit;

            end if;

            exit when  loop_counter = last_loop;

-- we are having trouble with a self intersection problem so ask to be
-- called again with a smaller scale
--               if NOT done and (total_self > 4 or (loop_counter = last_loop-1 ) or total_near > 10) then
--                   error_msg := '>>>>>Id: '||edge_id ||' Returning early from ASSEMBLE_EDGE for a smaller scale<<<';
--                  GZ_TOPOFIX.Track_App(error_msg,Topology,'LS');
--                  RETURN 0;
--               end if;
        END LOOP;
        effective_scale := effective_scale + scale * new_count;
        vertice_count := vertice_count + new_count;

        exit when done;
      END IF;
   END LOOP;

--   dbms_output.put_line('exited from outer loop'|| next);
-- make the generalized geometry;
   XYOrd.trim(XYord.count-next);
   gen_geometry := MDSYS.SDO_GEOMETRY(2002,SRID,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),XYORD);
--   sql_stmt := 'INSERT into points values(:1,:2)';
--execute immediate sql_stmt using -9,gen_geometry;
--commit;
--  dbms_output.put_line('returning from Assemble edge ' || 'next ' || next || ' xycount ' || xyord.count);
   if vertice_count <> 0. and effective_scale <> 0. then
     scale := effective_scale/vertice_count;
   end if;
-- This is the normal (successful) return.
   RETURN XYord.count;

END ASSEMBLE_EDGE;
--
FUNCTION CENTROID(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Xc IN OUT NUMBER,Yc IN OUT NUMBER,SRID NUMBER)

RETURN NUMBER AS
/*
********************************************************************************
--Program Name: centroid
--Author: Sidey Timmins
--Creation Date: 8/04/2008
--Updated:   11/08/2012 with code from GZ_QA.Centroid
--           10/21/2010 To accomodate SRID <> 8265
--           8/31/2010 To calculate approximate area in square meters.
--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 4 required parameter:
  --
  --   REQUIRED Parameters:
  --      INPUT

  --      XYs          - Array of X,Y coordinates
  --      SRID         - The Spatial reference ID 8265
  --      OUTPUT
  --       Xc           - Centroid X
  --       Yc           - Centroid Y
--
-- Purpose: Find a centroid of a single closed polygon and return the area of
--         the polygon.

-- Reference: DH Maling, "Measurements from Maps"
----            http://en.wikipedia.org/wiki/Centroid
********************************************************************************
*/
   deg2rad     CONSTANT NUMBER    :=0.0174532925199432957692369076848861271344;
   iend        PLS_INTEGER := XYs.count-2;
   Area        NUMBER :=0.0;

   nm          PLS_INTEGER := TRUNC(iend/2) ;
   ii          PLS_INTEGER := 1;
   delta       NUMBER;
   y           NUMBER;
   yy          NUMBER;
   cosy        NUMBER;
   siny        NUMBER;
   factor      NUMBER;
   to_meters   NUMBER := 12347654400.0;

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
--

    for i in 1..nm loop

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

  if Area > 0. then
    Xc := Xc/(6.*Area);
    Yc := Yc/(6.*Area);
  end if;

  if SRID = 8265.0 then
    y := ABS(Xys(2)+Xys(Xys.count))* 0.5 ;
    yy := y *deg2rad;

    siny := GZ_UTIL_ZONE.sincos(yy,cosy);

-- This empirical factor was found by comparing the results with Oracle's area
-- function SDO_GEOM.SDO_AREA. Results are amazing - 4 to 6 digits.

    factor := 0.9968658 + 0.0134185* siny*siny + 0.0095E-6*y*y -0.6E-7*(y-40) - 1.e-5*sin(5*(y-18)*deg2rad);

    area := factor * to_meters * cosy*area;
--    dbms_output.put_line('NEW area ' || area );
  end if;
  RETURN area;

END CENTROID;
--
FUNCTION CHECK_POLYLR(geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,nearby_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,new_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY)
RETURN MDSYS.SDO_LIST_TYPE AS

-- Check the clockwiseness of points on the new and old geometries with a segment
-- on a nearby polygon to ensure that the new and old geometries stay on the
-- same side of the polygon.

   LR_original  NUMBER;
   LR_now       NUMBER;
   iseg         PLS_INTEGER :=0;
   iseg_old     PLS_INTEGER :=0;
   test_point   PLS_INTEGER :=1;
   allow_loops  PLS_INTEGER := 1000000;

   Segments     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
BEGIN
-- Check the more complicated old geometry first
   LR_original := Check_Clock_Wiseness(geometry,nearby_geometry,iseg_old,test_point,allow_loops);

-- If (and only if) the detailed MBR does not overlap the MBR of the loop,
-- we ignore the test.
   If LR_original is NOT NULL THEN
--  dbms_output.put_line('Iseg was ' || ISEG_old || ' TP ' || test_point);
   LR_now := Check_Clock_Wiseness(new_geometry,nearby_geometry,iseg,test_point,1);

--  dbms_output.put_line('Iseg now ' || ISEG);
-- dbms_output.put_line('LR Orig ' || lr_original || ' LR now ' || LR_now);
-- Ignore collinearity here if it was not resolved
   if (LR_original <= 0. and LR_now <= 0.) or (LR_original > 0. and LR_now > 0.) then
     NULL;
   else
--   dbms_output.put_line('LR Orig ' || lr_original || ' LR now ' || LR_now);
     Segments.extend(1);
     Segments(1) := iseg;
   end if;
   END IF;

   RETURN Segments;
END CHECK_POLYLR;
--
FUNCTION CHECK_CLOCK_WISENESS(geom1 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,polygeom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,iseg IN OUT NOCOPY PLS_INTEGER,test_point IN OUT NOCOPY PLS_INTEGER,allow_loops PLS_INTEGER) RETURN NUMBER AS

-- Updated: 10/26/2012 To use segments that are near perpendicular to test point
--          1/12/2012 to ensure we use segments that are most parallel.
--          1/31/2011 to use a different point when the cross product is zero.
--          10/20/2010 To use a point from the polygon and nearest segments from
--          the old and new geometries.
--          09/27/2010 To use a segment from the polygon and nearest points from
--          the old and new geometries.
--          09/20/2010 to handle collinear case of test point and horizontal
--          or vertical line segment

-- Method: If we assert we are on the coastline and we are observing zig-zagging
-- ships sailing North, then if they never cross-over their previous paths,
-- on their closest approach they are going North. Furthermore, if we choose
-- a segment on the coastline (going in any direction) then the closest point
-- on one ship's path has a particular clockwise relationship wrt this segment.
-- The simplified version of this path must have the same clockwise relationship.
--
--
  piBy2     CONSTANT NUMBER  := 1.5707963267948966192313216916397514421;
  rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
  Xys          MDSYS.SDO_ORDINATE_ARRAY := geom1.sdo_ordinates;
  Info_Array   MDSYS.SDO_ELEM_INFO_ARRAY := geom1.sdo_elem_info;
  PolyXys      MDSYS.SDO_ORDINATE_ARRAY := polygeom.sdo_ordinates;
  PolyMBR      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  MBR          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  SRID         NUMBER := geom1.sdo_srid;
  xtest        NUMBER := PolyXys(test_point);
  ytest        NUMBER := PolyXys(test_point+1);

  angle        NUMBER;
  angle2       NUMBER;

  x0           NUMBER;
  y0           NUMBER;
  x1           NUMBER;
  y1           NUMBER;
  x2           NUMBER;
  y2           NUMBER;
  xt1          NUMBER;
  yt1          NUMBER;
  x21          NUMBER;
  y21          NUMBER;
  xLL          NUMBER;
  yLL          NUMBER;
  xUR          NUMBER;
  yUR          NUMBER;
  xLL2         NUMBER;
  yLL2         NUMBER;
  xUR2         NUMBER;
  yUR2         NUMBER;
  xnear        NUMBER;
  ynear        NUMBER;
  epsilon      NUMBER := 0.001;
  Is_left      NUMBER := 0.0;
  distance_found NUMBER;
  dist           NUMBER;
  best_distance  NUMBER := 1.E10;


  angle_check  NUMBER;
  best_point   PLS_INTEGER := test_point;
  best_seg     PLS_INTEGER :=1;
  before_seg   PLS_INTEGER;
  ok_to_loop   PLS_INTEGER := allow_loops;
  loops        PLS_INTEGER := 0;
  try          PLS_INTEGER := test_point -2;
  round_it     PLS_INTEGER := 10;
  no_to_try    PLS_INTEGER;
  closed       BOOLEAN := FALSE;


  xc           NUMBER;
  yc           NUMBER;
  ok           BOOLEAN;
  geom         mdsys.sdo_geometry;
  sql_stmt     VARCHAR2(4000);

BEGIN

--  the_time := current_timestamp;
--   dbms_output.put_line('in Check polyLR');
  if xys.count=2 then
--   dbms_output.put_line('returning zero' ||  xys.count || ' poly ' ||polyxys.count);
    RETURN 0.0;
  end if;

-- When a loop is far away we may end up with segments nearly parallel to
-- the direction of the test point to either end of the segment.

-- So if the polygon is outside the MBR of the segment, just always return
-- a positive number.

   SET_MANY_MBR(PolyXYs,polygeom.sdo_elem_info,PolyMBR,xLL2,yLL2,xUR2,yUR2);

-- First setup the MBRs for the edge to narrow the search
-- Not implemented yet.

  SET_MANY_MBR(XYs,Info_Array,MBR,xLL,yLL,xUR,yUR);

-- Return only if the detailed MBR doesn not overlap the loop MBR

  if ok_to_loop > 1 and (xUR < xLL2 or xLL> xUR2 or yUR < yLL2 or yLL > yUR2) then
--     dbms_output.put_line('returning NULL '|| xuR || ' ' || xLl2);
     RETURN NULL; --Its outside the MBR so ignore the test
  end if;
-- Check if the line is closed.
  if Xys(1) = Xys(XYs.count-1) and Xys(2) = Xys(Xys.count) then
    closed := TRUE;
  end if;



  iseg := 0;

  if MOD(try,2) =0 then
    try := try+1;
  end if;
  if try >= PolyXys.count-1 then
     try := -1;
  end if;

--  For the original geometry, loop over the polygon coordinates and search for
--  the closest distance to the polygon. Save best_point on the polygon.

--    dbms_output.put_line('TRY ' || try || ' poly ' || polyxys.count);
    While try < PolyXys.count-4 and ok_to_loop > 0 loop
      try := try+2;
      xtest := PolyXys(try);
      ytest := PolyXys(try+1);
-- Find segment returns the segment

--dbms_output.put_line('Trying polygon vertex ' || try);
/*
        iseg  := find_distances(xtest,ytest,Xys,distance_found,FALSE);
-- Now choose the segment that is most perpendicular to the test point
    x1 := ROUND(Xys(iseg*2-1),round_it);
     y1 := ROUND(Xys(iseg*2),round_it);

     x2 := ROUND(Xys(iseg*2+1),round_it);
     y2 := ROUND(Xys(iseg*2+2),round_it);
         dist := Perpendicular(xtest,ytest,x1,y1,x2,y2,xnear,ynear,FALSE, TRUE);
      if dist < distance_found then
         distance_found := dist;
      else
      if iseg > 1 then
        x2 := ROUND(Xys(iseg*2-3),round_it);
       y2 := ROUND(Xys(iseg*2-2),round_it);
         dist := Perpendicular(xtest,ytest,x2,y2,x1,y1,xnear,ynear,FALSE, TRUE);
         dbms_output.put_line('Trying next segment ' || round(dist,5) || ' x ' ||round(x1,7) || ' ,' || round(y1,7));
         if dist <= distance_found then
         distance_found := dist;
         iseg := iseg-1;
         end if;
        end if;
         if iseg*2+3 < Xys.count then
        x1 := ROUND(Xys(iseg*2+3),round_it);
       y1 := ROUND(Xys(iseg*2+4),round_it);
         dist := Perpendicular(xtest,ytest,x2,y2,x1,y1,xnear,ynear,FALSE, TRUE);
         dbms_output.put_line('Trying next segment ' || round(dist,5) || ' x ' ||round(x1,7) || ' ,' || round(y1,7));
         if dist <= distance_found then
         distance_found := dist;
         iseg := iseg+1;
         end if;
         end if;
      end if;
*/
      iseg := Find_Segment(xtest,ytest,MBR,Xys,NULL,distance_found,epsilon);
      ok := FALSE;
--      dbms_output.put_line('iseg ' || iseg || ' Dd ' ||round(distance_found,3) || ' best ' || round(best_distance,3)|| ' poly count ' || polyxys.count);

-- Our convention is to associate a vertex with the segment it start with
-- even thought there are 2 segments that share this vertex.

      if distance_found <> 0.0 and distance_found < best_distance then
        best_distance := distance_found;
        best_point := try;
        best_seg := iseg;
--        dbms_output.put_line('xtest ' ||round(xtest,6) || ' ' || round(ytest,6));
--        dbms_output.put_line('found seg ' || iseg || ' D ' ||round(distance_found,6) || ' try ' || try);
--        dbms_output.put_line('iiseg ' || iseg);
      end if;
      ok_to_loop := ok_to_loop -1;
   End Loop;
-- Now the vertex that we have belongs to 2 segments

   if allow_loops <> 1 then
      test_point := best_point;
   end if;

-- Find the closest segment to the test point, WITH the angle closest to
-- 90 degrees.  try better than 75 degrees, 60, 45, 30,15

--dbms_output.put_line('starting' || try || ' poly ' || (PolyXys.count-4) || ' ok ' || ok_to_loop);
--  FOR ii in 1..5 LOOP
--    angle_check := 15. + (ii-1)*15.;   -- this is 90 - angles of 75,60,45,30 etc
    iseg := best_seg;

--    dbms_output.put_line('BEST SEG  ' || iseg || ' BEST TP ' || best_point || ' best seg ' || best_seg);
--    if iseg > 1 then
     -- Don't know if this is what causes eof on communication channel
--      x0 := ROUND(Xys(iseg*2-3),round_it);
--      y0 := ROUND(Xys(iseg*2-2),round_it);
--      before_seg := iseg-1;
--    elsif closed then
--      x0 := ROUND(Xys(Xys.count-3),round_it);
--      y0 := ROUND(Xys(xys.count-2),round_it);
--      before_seg := TRUNC(XYs.count/2)-1;
--    end if;
--    if iseg >= TRUNC(Xys.count) then
--      iseg := 1;
--    end if;
--    dbms_output.put_line('>>> iseg ' || iseg);
     x1 := ROUND(Xys(iseg*2-1),round_it);
     y1 := ROUND(Xys(iseg*2),round_it);

     x2 := ROUND(Xys(iseg*2+1),round_it);
     y2 := ROUND(Xys(iseg*2+2),round_it);
--      dbms_output.put_line('before loop iseg ' || iseg || ' test_pt ' || test_point);

-- We try points behind and points ahead, finding the 1st one closest to the
-- test point.
/*
     IF allow_loops <> 1 THEN

        no_to_try := TRUNC(PolyXys.count/8);
        try := test_point+2;

     For jj in 1..no_to_try*2 Loop
       if jj <= no_to_try then
         try := try -2;
         if try <=0 then
           try := PolyXys.count-3;
         end if;
       else
         try := test_point + (jj-no_to_try)*2;
         if try >= polyXys.count-1 then
            try := try - Polyxys.count+2;
         end if;
       end if;

       xtest := ROUND(PolyXys(try),round_it);
       ytest := ROUND(PolyXys(try+1),round_it);
--      dbms_output.put_line('xtest ' || xtest || ' ytest ' || ytest);
--      dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
--      dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2);

      dist := Perpendicular(xtest,ytest,x1,y1,x2,y2,xnear,ynear,FALSE, TRUE);
      if dist < 1.E10 then
         angle :=0.;
         exit;
      end if;
-- We really have 2 different scenarios if the line find_segment pick is nearly
-- perpendicular to the test point:

-- 1)               |                   2)          |
--                  ^  segment picked    or         v
--                  |                               |
--          ---------                                -------------
--
--                 + (xtest,ytest)                 + (xtest,ytest)


-- Make some bearings and measure the angle that the segment subtends at the
-- test point. We really want the test point more or less perpendicular to
-- the segment. End on is not good since an "L" shaped edge can be generalized
-- in different ways so that a point that is

--              +                                +
--              |                                 \
--              |                                   \     generalized
--              |                                     \
--              +-->--+                                +
--
--
--                        + test point to the right        + same test point now to left

-- So if the test point is more perpendicular

--              +                                +
--              |                                 \
--              |                                   \     generalized
--              |                                     \
--              +-->--+                                +
--
--
--                 + test point to the right        + same test point still to the right


  if fast_distance(xtest,ytest,x1,y1) < fast_distance(xtest,ytest,x2,y2) then
    angle := ABS(90.-GZ_QA.angle(xtest,ytest,x1,y1,x2,y2)*rad2deg);
  else
    angle := ABS(90.-GZ_QA.angle(xtest,ytest,x2,y2,x1,y1)*rad2deg);
  end if;


 -- We use a segment number that is really a vertex number, since another
 -- segment shares that vertex.

  if fast_distance(xtest,ytest,x0,y0) < fast_distance(xtest,ytest,x1,y1) then
    angle2 := ABS(90.-GZ_QA.angle(xtest,ytest,x0,y0,x1,y1)*rad2deg);
  else
    angle2 := ABS(90.-GZ_QA.angle(xtest,ytest,x1,y1,x0,y0)*rad2deg);
  end if;
-- dbms_output.put_line(' angle2 ' || round(angle2,4));
  if angle2 < angle and angle2 < angle_check then
    iseg := before_seg;
    angle := angle2;
  end if;
--  dbms_output.put_line(' angle ' || round(angle,4));
  if angle < angle_check then
    test_point := try;
  end if;
   exit when angle < angle_check;

  End loop;
  END IF;
  exit when angle < angle_check;
  */
 -- End Loop;

  xtest := ROUND(PolyXys(test_point),round_it);
  ytest := ROUND(PolyXys(test_point+1),round_it);

  --  dbms_output.put_line('ok to loop ' || (1E6 - ok_to_loop));
-- Now we hold the polygon point constant and try different nearest
-- test segments on the old and new geometries
--
--                        + (x2,y2)
--                       /             * (xtest,ytest)
--                     /
--                   +  (x1,y1)

--    dbms_output.put_line('best_point ' || best_point);

-- Check that the 3 points are not collinear. If they are, choose another segment
-- on the line.

   While loops < 3 and is_left = 0.0 Loop

   loops := loops + 1;
--   dbms_output.put_line('>>> in loop 5 ' || loops);
   if (x1 = x2 and xtest = x1) or (y1 = y2 and ytest = y1) or (loops >1  and Is_left = 0.0) then

-- Move the edge segment,

        iseg := iseg + 1;
--        dbms_output.put_line('ISEG now ' || iseg || ' xys ' || xys.count);
        if iseg*2 > Xys.count then
-- for closed we can go round the loop
           if closed then
             iseg := 2;
--             dbms_output.put_line('closed now ' || iseg );
           else
-- for not closed we try the segment before best if we reach the end of the line
             iseg := best_seg -1;
--             dbms_output.put_line('best SEG now ' || iseg );
             if iseg < 1 then
               iseg := 1;
             end if;
             x2 := ROUND(Xys(iseg*2-1),round_it);
             y2 := ROUND(Xys(iseg*2),round_it);
             iseg := iseg + 1;
           end if;
        end if;
        x1 := x2;
        y1 := y2;
        x2 := ROUND(Xys(iseg*2-1),round_it);
        y2 := ROUND(Xys(iseg*2),round_it);

   end if;
--     dbms_output.put_line('xst ' || xtest || ' yt ' || ytest);
--     dbms_output.put_line('x11 ' || x1 || ' y1 ' || y1);
--     dbms_output.put_line('x22 ' || x2 || ' y2 ' || y2);


-- Finally formulate a crossproduct. Must be careful since A X B <> B X A.

-- Returns > 0  for point p2 left of line p0 to p1
--         = 0 for p2 on line
--         < 0 for p2 right of line
--dbms_output.put_line('test point on poly ' || test_point || ' iseg ' || iseg);

     Is_left := (x2-x1)* (ytest-y1) - (xtest-x1)*(y2-y1);  -- crossproduct

 --  Is_left := (xtest-x1)*(y2-y1) -(x2-x1)* (ytest-y1) ;  -- crossproduct

--   dbms_output.put_line('is left ' || is_left || ' test pt ' || test_point || ' iseg ' || iseg);

   End Loop;

--  dbms_output.put_line('After check clock Elapsed time : ' || (current_timestamp - the_time));
  RETURN Is_left;

END CHECK_CLOCK_WISENESS;
--
FUNCTION CHECK_FOR_SELF_INTERSECT(Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,dec_digits NUMBER default 7,thousand NUMBER default 100000.) RETURN MDSYS.SDO_LIST_TYPE AS
/*
********************************************************************************
--Program Name: Check_for_Self_Intersect
--Author: Sidey Timmins
--Creation Date: 05/26/2010
--Updated: 07/21/2010 To ignore intersection at the last vertex.
--
--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 2 required parameter:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      geometry      - A geometry to check

  --      dec_digits    - decimal digits to round to. Some of the geometries have
  --                       trailing zeroes. Oracle cannot multiply these numbers
  --                       and it just disconnects!
--
-- Purpose: Self intersection means segment n intersects segment n+m where m
--          is greater than 1. Returns an array of intersecting segments or an empty array.

-- Method: Checks each pair of segments using Cartesian geometry. But does not
--         make unnecessaary checks when the MBR of a group of segments does
--         overlap the current segment being considered.
-- Calls: line_intersect
--
********************************************************************************
*/
    Geom           MDSYS.SDO_GEOMETRY;
    XYOrd          MDSYS.SDO_ORDINATE_Array;
    segments       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    MBR            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Info_Array     MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1);
    xLL            NUMBER;
    yLL            NUMBER;
    xUR            NUMBER;
    yUR            NUMBER;
    axLL           NUMBER;
    ayLL           NUMBER;
    axUR           NUMBER;
    ayUR           NUMBER;
    bxLL           NUMBER;
    byLL           NUMBER;
    bxUR           NUMBER;
    byUR           NUMBER;
    xi             NUMBER;
    yi             NUMBER;
    x1             NUMBER;
    y1             NUMBER;
    x2             NUMBER;
    y2             NUMBER;
    x3             NUMBER;
    y3             NUMBER;
    x4             NUMBER;
    y4             NUMBER;
    xt             NUMBER;
    yt             NUMBER;
    det            NUMBER;
    dist12         NUMBER;
    dist13         NUMBER;
    dist1t         NUMBER;
    s              NUMBER;
    seg            NUMBER;
    x33            NUMBER;
    y33            NUMBER;
    x44            NUMBER;
    y44            NUMBER;
    overlap        BOOLEAN;
    ia             PLS_INTEGER;
    ib             PLS_INTEGER;
    ii             PLS_INTEGER;
    astart         PLS_INTEGER;
    bstart         PLS_INTEGER;
    aend           PLS_INTEGER;
    bend           PLS_INTEGER;
    lastm          PLS_INTEGER := 1;
    sql_stmt       VARCHAR2(4000);
    t              NUMBER := 0.99;
    closed         BOOLEAN := FALSE;
    n              PLS_INTEGER;
    m              PLS_INTEGER;
    ak             PLS_INTEGER;
    bk             PLS_INTEGER;
    iend           PLS_INTEGER;
    jj             PLS_INTEGER;
    aend_seg       PLS_INTEGER;
    bend_seg       PLS_INTEGER;
    next           PLS_INTEGER := 0;

BEGIN


  XYOrd := Geometry.sdo_ordinates;
  n := TRUNC(XYOrd.count/2);

  if n < 3 then
    RETURN segments;
  end if;

-- Checking for self interesection is costly and not necessary everywhere
  iend :=n;
  segments.extend(10);


-- We want to set up checking segment i with segment i+2
-- Note that segment 2 touches 1 and can never intersect 1.
--                            3
--                     +-------------+                            +
--                    /                                         / | 2
--          1        /  2                                  1   /  |
--    +------------+                                +-------------+
--                                                           / 3
--                                                          +

--  Preload the loops with vertices and segments
--  For a closed loop we back up around the loop and start at the
--  the last segment
--             5
--      +-----------+
--      |         4   \   3
--   6  |               +---+
--      |                   |  2
--      +-------------------+
--               1

  if XYOrd(1) = XYord(XyOrd.count-1) and XYOrd(2) = XyOrd(XyOrd.count) then
    closed := TRUE;
  end if;
-- For a normal edge not closed.
--                             3
--           (x3,y3)    +-------------+ (x4,y4)
--                    /
--          1        /  2
--    +------------+
--   (x1,y1)       (x2,y2)
--
    x2 := ROUND(XYord(1),dec_digits);     -- these are all off by 1 as we are going to do
    y2 := ROUND(XyOrd(2),dec_digits);     --  1 = 2,  2 = 3, 3 = 4
    x3 := ROUND(XYord(3),dec_digits);
    y3 := ROUND(XyOrd(4),dec_digits);
--    astart := 4;
--    seg := 0;           --seg becomes 1 after we add 1 in the loop below
--  end if;               -- to be compared with segment 3 which ends at vertex 4
--  dbms_output.put_line('calling set many from checkfor self');
  SET_MANY_MBR(XYord,Info_Array,MBR,xLL,yLL,xUR,yUR);

  m := MBR(5);
  seg := 0;
  aend_seg := n-3;
  bend_seg := n;
  /*
   for ii in 1..m loop   -- Just to visulaize the MBRs
       kk := ii*6;
       xLL := MBR(kk+1);
       yLL := MBR(kk+2);
       xUR := MBR(kk+3);
       yUR := MBR(kk+4);
     geom :=  MDSYS.SDO_GEOMETRY(2003,8265,NULL,SDO_ELEM_INFO_ARRAY(1,1003,3),
                    MDSYS.sdo_ordinate_Array(xLL,yLL,xUR,yUR));
                sql_stmt := 'INSERT into points values(:1,:2,:3)';
                    execute immediate sql_stmt using ii,geom,'MBR geom';
                    commit;
   end loop;
   */
--  dbms_output.put_line('m is ' || m || ' MBR ' || mbr.count);

-- We have divided the edge into sections using Set_many_MBR
-- In order to check if any 2 segments intersect it is necessary to
-- check if segment ii interesects segment ii+2. But this is not necessary if
-- the MBR for segments (ii+2) to say (ii+p) don't overlap (x1,y1) to (x2,y2)
--  dbms_output.put_line('istart ' || istart || ' iend ' ||iend);

  astart := 1;
  ia := 0;
  While seg < aend_seg Loop
    x1 := x2;
    y1 := y2;
    seg := seg + 1;
--    dbms_output.put_line('seg is ' ||seg);

    x2 := ROUND(XYord(seg*2+1),dec_digits); --seg 1 for example begins at coordinates 3 and 4
    y2 := ROUND(XYord(seg*2+2),dec_digits);
--    execute immediate 'insert into xys values(:1,:2,:3,:4,:5) ' using x1,y1,x2,y2,ii;
--    commit;
    dist12 := (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1);
-- reset the set number
    if seg >= astart then
      ia := ia + 1;
      ak := ia*6;
      astart := TRUNC((MBR(ak+5)+1)/2);  -- Turn coord # into segment # or vertex numbers
      aend := TRUNC(MBR(ak+6)/2);        -- Note start at 2nd MBR
--      dbms_output.put_line('astart ' || astart || ' aend ' || aend);
      astart := aend;
      axLL := MBR(ak+1);
      ayLL := MBR(ak+2);
      axUR := MBR(ak+3);
      ayUR := MBR(ak+4);
    end if;
    ib := ia-1;
    ii := seg+1;

    bend := 0;

    While ii < bend_seg Loop
      ii := ii + 1;
--      if seg = 53 then
--      dbms_output.put_line('II ' ||ii || ' bend ' || bend);
--      end if;
      if ii > bend then
        ib := ib + 1;
        exit when ib > m;
        bk := ib*6;
        bstart := TRUNC((MBR(bk+5)+1)/2);  -- Turn coord # into segment # or vertex numbers
        bend := TRUNC(MBR(bk+6)/2);        -- Note start at 2nd MBR
        exit when bstart >= bend;
--        dbms_output.put_line('bstart ' || bstart || ' bend ' || bend);

        overlap := TRUE;
--        if seg =2 then
--         dbms_output.put_line('ia ' || ia || ' ib ' || ib  || ' ii ' || ii || ' seg ' || seg);
--         end if;
        if ib = ia then                    -- The 2 sets overlap
          x4 := ROUND(XYord(ii*2-1),dec_digits);
          y4 := ROUND(Xyord(ii*2),dec_digits);
          ii := ii+1;
--          if seg =2 and ii=5 then
--          dbms_output.put_line('Ii ' || ii  || ' bstart ' || bstart || ' bend ' || bend);
--          end if;
          exit when ii > bend_seg;

        else                               -- check the MBRs
          bxLL := MBR(bk+1);
          byLL := MBR(bk+2);
          bxUR := MBR(bk+3);
          byUR := MBR(bk+4);
          if  (axUR  < bxLL) or (axLL  > bxUR) or
              (ayUR  < byLL) or (ayLL  > byUR) then
            overlap := FALSE;
            ii := bend;                               -- they don't overlap
--            if seg >= 19 then
 --           dbms_output.put_line('ii ' || ii  || ' bstart ' || bstart || ' bend ' || bend);
--            end if;
          else
            x4 := ROUND(XYord(bstart*2-1),dec_digits);  -- they overlap
            y4 := ROUND(Xyord(bstart*2),dec_digits);
            ii := bstart+1;
--            if seg =2 and (ii = 5 or II = 6) then
--           dbms_output.put_line('II ' || ii  || ' bstart ' || bstart || ' bend ' || bend);
--           end if;
        end if;
      end if;
      End If;

-- Loop over vertices, starting at vertex 4 (x4,y4) to the last vertex n.
-- When we conside vertex 4 we are checking segment 3:  (x3,y3) to (x4,y4)
-- However this loop is over just the vertices described by the current MBR

          x3 := x4;
          y3 := y4;

          jj := ii*2;
--          if jj >= Xyord.count then
--          dbms_output.put_line('ii ' || ii || ' Xyord ' || xyord.count);
--          end if;
            x4 := ROUND(XYord(jj-1),dec_digits);
            y4 := ROUND(XYord(jj),dec_digits);
 -- If the 2 sets overlap then we compare each pair of segments in the set
-- dbms_output.put_line('ii ' ||  ' iilast ' || iilast || ' bend ' || bend);
      IF overlap THEN
-- Make a test point
              xt := x3*t + (1.-t)*x4;
              yt := y3*t + (1.-t)*y4;
--    if seg = 39 and (ii = 42 or ii = 43) then
--    dbms_output.put_line(' x1 ' || x1 || ' y1 ' || y1 || ' x2 ' || x2 || ' y2 ' || y2);
--    dbms_output.put_line(' x3 ' || x3 || ' y3 ' || y3 || ' x4 ' || x4 || ' y4 ' || y4);
--    end if;
--execute immediate 'insert into xys values(:1,:2,:3,:4,:5) ' using x1,y1,x3,y3,-ii;
--    commit;
              dist13 := (x3-x1)*(x3-x1) + (y3-y1)*(y3-y1);
              dist1t := (xt-x1)*(xt-x1) + (yt-y1)*(yt-y1);
--              if seg = 2 and ii = 6 then
--      dbms_output.put_line('ii ' || ii ||  ' d3 ' || round(dist13,8) || ' ' || round(dist1t,8));
--              end if;
              If dist1t < dist13 or dist1t < dist12 Then
                 s := Line_intersect(x1,y1,x2,y2,x3,y3,x4,y4,xi,yi,det);
--        if s <> 0 and s <> -1. then
--          if seg = 2 and ii = 6 then
--        dbms_output.put_line('ii ' || ii || ' seg ' || seg || ' n ' || n || ' s ' || round(s,10) || ' xi ' || xi || ' yi ' || yi);
--         end if;
                if s >= 0. and s < 1. then

-- Off course a loop intersects itself and we are not interested in
-- intersections between consecutive segments
                   if (closed = TRUE and s = 0.0 and ii = n and seg = 1) or
                                        (s = 0.0 and ii=seg) then
                       NULL;
                   else
                       next := next +1;
                       if next >= segments.count then
                          segments.extend(10);
                       end if;
                       if seg > (ii-1) then
                          segments(next) := (ii-1.)*thousand + seg;
                          next := next + 1;
                          segments(next) := seg*thousand + (ii-1.);
--                          dbms_output.put_line('found ' || (ii-1) ||' and ' || seg );
                       else
--                          dbms_output.put_line('Found ' || seg ||' and ' || (ii-1));
                         segments(next) := seg*thousand + (ii-1.);
                         next := next + 1;
                         segments(next) := (ii-1.)*thousand + seg;
                       end if;
                   end if;
                end if;
              End if;

      End If;
    End Loop;
  End Loop;

--  dbms_output.put_line('leafing self');
  If next = 0 then
    segments.trim(segments.count);
    RETURN segments;
  else
    segments.trim(segments.count-next);
    RETURN segments;
  end if;

END CHECK_FOR_SELF_INTERSECT;
--
FUNCTION CHECK_MATCHING_XYS(tested IN OUT NOCOPY PLS_INTEGER,XyOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_Array,origXyOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_Array) RETURN PLS_INTEGER AS

    kount          PLS_INTEGER := 0;
    jj             PLS_INTEGER := -1;
    start_loop     PLS_INTEGER := 1;
    end_loop       PLS_INTEGER := origXYOrd.count-1;

BEGIN

-- Find the number of consecutive matches between the current (XYOrd) edge and
-- the original (origXYOrd) edge testing from coordinate 1 on the 1st edge to XYOrd.count
-- and comparing with coordinate start2 to coordinate end2 on the 2nd edge.
--
--   We also return tested which is the range from 1 to where the 1st edge matches
--   the original edge (marked here with a *).
--
--                 n coordinates
--         +------------*--------+    Original edge
--         start                 end
--                 m matching coordinates out of p total coordinates in 1st edge
--         +------------+             1st edge
--         1            XyOrd.count

-- We always have the start and end vertices match somewhere but we want the
-- number of consecutive matches.

-- Now test each element of the current XyOrdinate array


   WHILE jj <= end_loop Loop
         jj := jj + 2;   --- 1,3,5,7,.. n-1 where n = origXYord.count
         If jj-start_loop+1 < XyOrd.count then
            If XyOrd(jj-start_loop+1) <> origXyOrd(jj) or
               XyOrd(jj-start_loop+2) <> origXyOrd(jj+1) then
                 exit;
            Else
                 kount := kount + 2;
            End if;
        End If;
    End Loop;

    tested := end_loop-start_loop +2;

   RETURN kount;

END CHECK_MATCHING_XYS;
--
FUNCTION COMPARE_EDGE_COORDINATES (XYs_Gen IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,match_count IN OUT NOCOPY PLS_INTEGER,bad_match_sections IN OUT NOCOPY PLS_INTEGER,decim_digits PLS_INTEGER default 6,bad_length PLS_INTEGER default 4) RETURN MDSYS.SDO_LIST_TYPE AS
/*
********************************************************************************
--Program Name: Compare_Edge_Coordinates
--Author: Sidey Timmins
--Creation Date: 11/02/2010

--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 3 required parameter:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      XYs_Gen     - Array of generalized ordinates to compare with Xys
  --      XYs         - Array of ungeneralized ordinates to search
   --      OUTPUT
  --      match_count -- returned length of matches array

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
FUNCTION EDGE_SIMPLIFY(Topology VARCHAR2,Edge_id NUMBER,geometry  MDSYS.SDO_GEOMETRY,
pscale NUMBER default 500000.,nice NUMBER default 1.,tolerance NUMBER default 0.05,dec_digits PLS_INTEGER default 6,method VARCHAR2 default 'ZONE',Edges_Table VARCHAR2 DEFAULT 'EDGES_TOPROCESS')  RETURN MDSYS.SDO_GEOMETRY AS

   orig_geometry  MDSYS.SDO_GEOMETRY := geometry;
   new_geom    MDSYS.SDO_GEOMETRY;

   Edge_table  VARCHAR2(100) := UPPER(Topology) || UPPER(Edges_Table);
   Status      VARCHAR2(4000);
   sql_stmt    VARCHAR2(4000);
   elength     NUMBER;
   uniq        NUMBER;
   scale       NUMBER :=pscale;

-- Just a SQL callable function that returns the geometry that SUPER will make.


BEGIN

--   dbms_output.put_line(edge_table);
   sql_stmt := 'SELECT MT_LENGTH,UNIQ FROM '||Edge_Table ||' WHERE EDGE_ID =:1';
--   dbms_output.put_line(sql_stmt);
   execute immediate sql_stmt into elength,uniq using Edge_id;

   new_geom := EDGE_SIMPLIFY(Status,Edge_id,orig_geometry,elength,uniq,scale,nice,tolerance,dec_digits,method,Edge_Table,Topology);
--   dbms_output.put_line('Status was ' || status);
   RETURN new_geom;
END EDGE_SIMPLIFY;
--
FUNCTION EDGE_SIMPLIFY(Status IN OUT NOCOPY VARCHAR2,Edge_id NUMBER,orig_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,elength NUMBER,uniq NUMBER,
pscale IN OUT NOCOPY NUMBER,nice NUMBER,tolerance NUMBER,dec_digits PLS_INTEGER default 6,method VARCHAR2 default 'ZONE',Edges_Table VARCHAR2 DEFAULT 'EDGES_TOPROCESS',topology VARCHAR2)  RETURN MDSYS.SDO_GEOMETRY AS

/*
********************************************************************************
--Program Name: Edge_Simplify
--Author: Sidey Timmins
--Creation Date: 05/26/2010
--Updates: 8/27/2010 To avoid "An island of face nn has an edge coincident with
--                   outer boundary" we add the 2nd and next to last vertices
--                   to figure 8 polygons.
--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 8 required parameter:
  --
  --   REQUIRED Parameters:
  --      INPUT

  --      Status       - A returned status
  --      Edge_id      - edge_id of the edge
  --      geometry     - A geometry to simplify
  --      elength      - edge length in meters
  --      uniq         - uniq>=100 means the edge has a companion that completes a loop,
  --                     0 mean no, 2 or 8 means closed loops.
  --      scale        - Target scale for generalization
  --                     On output, the actual effective scale used =
  --                     sum of (coordinate_count(i)* scale_used(i))/ total_coordinate_count
  --      nice         - For ZONE a modifier to scale usually 1. Smaller numbers
  --                     are "nicer" and give more generalization, bigger numbers
  --                     are not so nice. Allowable range 0.25 to 2.
  --                     For DP  (Douglas-Peuker), the threshold in meters.
  --      tolerance    - Oracle tolerance to pass to SDO_INTERSECTION.
  --      dec_digits   - Rounds all coordinates except the end points to this
  --                     precision.
  --      method       - Either 'DP' or 'ZONE'
  --      Edges_table  - A specially prepared work table previously made by
  --                     Make_edge_table

  --      OUTPUT
--                        A generalized geometry is returned
-- Purpose: Simplifies an edge and checks for self intersections or with other
--          edges.

-- Reference:
-- Calls: GZ_UTIL_ZONE.line_simplify, GZ_UTIL_ZONE.reverse_ordinates,
--        check_for_self_intersect
-- SDO_GEOM.SDO_INTERSECTION
********************************************************************************
*/
   sql_stmt          VARCHAR2(4000);
   sql_stmt1         VARCHAR2(4000);
   sql_stmt2         VARCHAR2(4000);
   sql_stmt4         VARCHAR2(4000);
   sql_stmt6         VARCHAR2(4000);

   new_geometry      MDSYS.SDO_GEOMETRY;
   nearby_geometry   MDSYS.SDO_GEOMETRY;
   poly_geometry     MDSYS.SDO_GEOMETRY;

   better_geometry   MDSYS.SDO_GEOMETRY;
   intersections     MDSYS.SDO_LIST_TYPE;
   xyord             mdsys.sdo_ordinate_array;
   origxyord         mdsys.sdo_ordinate_array := orig_geometry.sdo_ordinates;
   partxyord         mdsys.sdo_ordinate_array := mdsys.sdo_ordinate_array();
   badXYOrd          mdsys.sdo_ordinate_array := mdsys.sdo_ordinate_array();
   NearbyOrd         mdsys.sdo_ordinate_array;
   polyXys           mdsys.sdo_ordinate_array;
   new_Xys           mdsys.sdo_ordinate_array;
   old_Xys           mdsys.sdo_ordinate_array;
   segments          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

   isegments         MDSYS.SDO_LIST_TYPE;
   nearby_edges      MDSYS.SDO_LIST_TYPE;
   self_segs         MDSYS.SDO_LIST_TYPE;
   id_list           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();


   area1             NUMBER;
   area2             NUMBER;
   threshold         NUMBER := nice;
   xc                NUMBER;
   yc                NUMBER;
   area              NUMBER;
   area_after        NUMBER;
   start_node        NUMBER;
   end_node          NUMBER;
   nearby_edge_id    NUMBER;
   vertex_removed    NUMBER;

   xstart            NUMBER;
   ystart            NUMBER;
   xend              NUMBER;
   yend              NUMBER;
   new_co_count      NUMBER;
   target_scale      NUMBER := pscale;
   scale            NUMBER := pscale;
   SRID              NUMBER := orig_geometry.sdo_srid;
   thousand          NUMBER := 100000.;
   closed            BOOLEAN := FALSE;
   seg1              PLS_INTEGER;
   seg2              PLS_INTEGER;

   offset            PLS_INTEGER :=0;
   j                 PLS_INTEGER;
   geom_is_valid     VARCHAR2(4000) := 'TRUE';
   nlast             PLS_INTEGER := 8;
   n                 PLS_INTEGER;
   m                 PLS_INTEGER;

   check1            PLS_INTEGER;
   check2            PLS_INTEGER;

   loop_counter      PLS_INTEGER := 0;
   last_loop         PLS_INTEGER := 20;
   loops             PLS_INTEGER := 0;
   istart            PLS_INTEGER := 0;
   iend              PLS_INTEGER := 0;
   inc               PLS_INTEGER := 0;
   jj                PLS_INTEGER;
   seg_found         PLS_INTEGER;
   temp              PLS_INTEGER;
   done              BOOLEAN := TRUE;
   swop              PLS_INTEGER;
   jrow              PLS_INTEGER;
   ok                BOOLEAN;
   loop_end          PLS_INTEGER;
   jjj               NUMBER :=0.0;
   is_a_connection   VARCHAR2(5);
   error_msg         VARCHAR2(1000);
   time1         date;

BEGIN
--   dbms_output.put_line('pscale is ' || pscale);
   Status := 'TRUE';

   m := origXYord.count;
-- If the edge has 2 vertices (4 coordinates it cannot be generalized)
-- About 6% (63348 out of 1.01 million) have just 2 vertices.

   if m <= 4 then
     RETURN orig_geometry;
   end if;


   sql_stmt1 := 'Select new_geometry from ' || Edges_Table|| ' where edge_id=:1';

-- For polygons, it would be nice if the ordinates were in counter clockwise order.
-- However, some loops are doughnut holes so better to leave the order unchanged
-- and let Zone deal with it.

   if origxyOrd(1) = origxyOrd(m-1) and origxyOrd(2) = origxYord(m) then
     area := Centroid(origXyord,xc,yc,SRID);
     closed := TRUE;
   else    -- we can just try the target scale for an edge which is not a loop
     last_loop := 1;
   end if;

-- Ensure we get a polygon with at least 8 coordinates if the edge is closed,
-- otherwise if not closed just generalize once.
   Loop

      loop_counter := loop_counter + 1;

--    Keep reducing the scale
--    The sequence of scale depend upon the target scale and are "nice"

      if loop_counter <> 1 then
        get_scale(threshold,scale,target_scale,elength);
      end if;
--      dbms_output.put_line('>>>' || loop_counter || ' <<<<<<<<<<<<<<<<<<<<<<<<');
--      dbms_output.put_line('>>>scale now is ' || scale || ' loop_counter ' || loop_counter);


      if UPPER(method) = 'DP' then --    Generalize the line either by Douglas-Peuker
         new_geometry := sdo_util.simplify(orig_geometry,threshold,tolerance);
      else  -- or by ZONE
         new_geometry := GZ_UTIL_ZONE.line_simplify(orig_geometry,scale,nice,0.0,elength,edge_id,-1.,Topology,target_scale);
      end if;

-- Ensure that there are not any vertices too close;


     if (closed = TRUE and nlast >= 8) or (closed = FALSE and nlast >=4) then
       ok := REMOVE_CLOSE_XYS(new_geometry);
     end if;

     XyOrd := new_geometry.sdo_ordinates;
     if closed = TRUE then
       area_after :=  Centroid(Xyord,xc,yc,SRID);
       if area*area_after < 0. and loop_counter < last_loop then
         continue;
       end if;
     end if;
     n := XYord.count;

-- Disallow Removing close xys on last loop if there have been problems

     if loop_counter = last_loop-1 and ((closed = TRUE and nlast >= 8) or (closed = FALSE and nlast >=4)) then
        nlast := n;
     else
        nlast := 0;
     end if;

     exit when (closed = TRUE and n >= 8) or (closed = FALSE and n >=4) or loop_counter >= last_loop;
   End loop;

--dbms_output.put_line('done first zone simplification' || (current_timestamp-time1));

-- This is just to handle a gross error returning too few vertices. DP may do this.

   if n < 4 then
      new_geometry := Robust_line_Gen(orig_geometry,3);
      xyOrd := new_geometry.sdo_ordinates;
   end if;



   IF m > 4 THEN

-- If the edge has another companion that makes a loop (uniq >=100) then
-- we have to be careful not to generalize it to a straight line because that will
-- break topology.
--
--              + ------------- +
--              |               |
--              |               /  this edge must not generalized to a straight line
--               \-------------/   no matter how short it is.


      if closed = TRUE then
--   dbms_output.put_line(' its closed ');

-- Just for figure 8 polygons
         if uniq = 8 or uniq = 2 then
--         dbms_output.put_line(' calling FIX_FIGURE8 ');
           FIX_FIGURE8(origXYord,XYord,dec_digits);
           new_geometry.sdo_ordinates := Xyord;
         end if;

         poly_Geometry := new_geometry;
         poly_Geometry.sdo_gtype :=2003;
         poly_Geometry.sdo_elem_info := MDSYS.sdo_elem_info_array(1,1003,1);

--   use an Oracle function to check to see if the loop self intersects
         geom_is_valid := SDO_GEOM.Validate_Geometry_with_context(poly_Geometry,0.05);
--dbms_output.put_line('geom is valid ::: ' || substr(geom_is_valid,1,100));
         if (INSTR(geom_is_valid, '13367 [Element') != 0) then
                 polyXys := poly_geometry.sdo_ordinates;
                 GZ_UTIL_ZONE.reverse_ordinates(PolyXys);
                 poly_geometry.sdo_ordinates := PolyXys;
                 geom_is_valid := sdo_geom.validate_geometry_with_context(poly_geometry,0.05);
--                  dbms_output.put_line(edge_id || geom_is_valid);
         end if;

      else
--   dbms_output.put_line('not closed');
--   Use a function to check to see if the line self intersects
--dbms_output.put_line('checking for self intersection');

         self_segs := check_for_self_intersect(new_Geometry,thousand);

         if self_segs.count > 0 then
         seg2 := MOD(self_segs(1),thousand);
         seg1 := TRUNC((self_segs(1)-seg2)/thousand);
         geom_is_valid := '13349 [Element <1>] [Ring <1>][Edge <'||seg1||'>][Edge <'||seg2||'>]';
         end if;
      end if;
--    dbms_output.put_line(substr(geom_is_valid,1,100) || ' count ' || new_geometry.sdo_ordinates.count);

-- Now fix an intersecting geometries
      if SUBSTR(geom_is_valid,1,5) = '13349' then
        new_Geometry.sdo_gtype :=2002;
        new_Geometry.sdo_elem_info := MDSYS.sdo_elem_info_array(1,2,1);
       end if;

      sql_stmt := 'Select start_node_id,end_node_id,nearby_edges,new_geometry from ' || Edges_Table|| ' where edge_id=:1';
      Execute immediate sql_stmt into start_node,end_node,nearby_edges,nearby_geometry using edge_id;

-- Now see if there are any nearby edges which may interfere with the new
-- changed edge coordinates.

      IF nearby_edges is NULL THEN
        done := TRUE;
        nearby_edges := MDSYS.SDO_LIST_TYPE();
--        dbms_output.put_line('no nearby edges');
      ELSE

        For jj in 1..nearby_edges.count loop
          nearby_edge_id := ABS(nearby_edges(jj));
--dbms_output.put_line('checking ' || nearby_edge_id);
          Execute immediate sql_stmt1 into nearby_geometry using nearby_edge_id;
-- Oracle will include end on (node) intersections which we don't want
             intersections := FIND_INTERSECTION_SEGMENT(new_geometry,nearby_geometry);
--          intersections := SDO_GEOM.SDO_INTERSECTION(new_geometry,nearby_geometry,tolerance);

          if intersections.count > 0 then -- is NOT NULL then
--          dbms_output.put_line('intersections is not null');
            done := FALSE;
            exit;
          end if;
        End Loop;
      END IF;

-- Check nodestar is good.
-- A is to the right of edge B before generalization.
-- After A moves to poition C and is now to the left. The area of the loop AB
-- is positive whereas the area of CB is not

--          +______
--          | \    |
--          |  |   |
--    C=new |  | B |  A = old edge
--          |  |   |
--          |  /   |
--          +______

     if XYOrd.count = 4 then
          is_a_connection := Is_Connected(edge_id,start_node,end_node,nearby_edges,Edges_Table,id_list);
--          dbms_output.put_line('ffor id : ' || edge_id || ' is a connection ' || is_a_connection);
-- We want the first edge to always maintain its direction

          if is_a_connection='TRUE' then
            new_xys := get_loop_xys(XYOrd,Edges_table,id_list);
            old_xys := get_loop_xys(OrigXYOrd,Edges_table,id_list);
            area2 := centroid(new_xys,xc,yc,8265.);
            area1 := centroid(old_xys,xc,yc,8265.);
--            dbms_output.put_line('area1 ' || round(area1,6) || ' area2 ' || round(area2,6));
-- dbms_output.put_line('area1 ' || area1 || ' area2 ' || area2);
  -- We have a nodestar situation. Make straight line into a bent one.
  -- We can have a very straight line that creates a needle triangle and it
  -- is not clear that we are maintaining the clockwiseness

            if area2*area1 <= 0. or round(area1,3) = 0. or round(area2,3)=0. then
              done := FALSE;
              new_geometry := Robust_line_gen(orig_geometry,3);
              XYord :=new_geometry.sdo_ordinates;
            end if;
          end if;
      end if;

      IF done = TRUE THEN
--      dbms_output.put_line('CHECKING SELF intersect' );
        self_segs := check_for_self_intersect(new_Geometry);
        if self_segs is NOT NULL then
          done := FALSE;
        end if;
      END IF;
-- Are we done? (no nearby edges or no intersections).
      IF done = TRUE THEN
       dbms_output.put_line('done is true');
         NULL;
-- We are not done so we must modify our representation by changing the scale
-- for a portion of the line

      ELSE
--    dbms_output.put_line('done is false');

--   We use the new_geometry as the state variable for the spatial data

--dbms_output.put_line ('before loop ' ||new_geometry.sdo_ordinates.count);
-- For each nearby edge, get its current geometry and test the new generalized
-- geometry to see it it intersects anywhere except on the ends (nodes).


        While done = FALSE and loops < 8 and new_geometry is not NULL LOOP
           loops := loops + 1;
           done := TRUE;


           loop_end := nearby_edges.count+1;

           For jj in 1..loop_end loop

            XYOrd := new_geometry.sdo_ordinates;
--          if edge_id = 970204 then

--           end if;
             if jj < loop_end then
               nearby_edge_id := nearby_edges(jj);
--                dbms_output.put_line ('jj loop ' || jj || ' c ' ||new_geometry.sdo_ordinates.count || ' near id ' || nearby_edge_id || ' end ' || loop_end);
               if nearby_edge_id <> 0 then
                 Execute immediate sql_stmt1 into nearby_geometry using nearby_edge_id;
               end if;
               NearbyOrd := nearby_geometry.sdo_ordinates;
               xstart  := NearbyOrd(1);
               ystart := NearbyOrd(2);
               xend   := NearbyOrd(NearbyOrd.count-1);
               yend := NearbyOrd(NearbyOrd.count);

               segments := FIND_INTERSECTION_SEGMENT(new_geometry,nearby_geometry);

--              dbms_output.put_line ('Checking ' || nearby_edge_id || ' Seg count ' || segments.count);

-- For nearby loops we have to check we stay on the same side of them.
-- but don't if the current geometry is a closed loop !!
--               if origXYord(1) = origXYord(m-1) and origXYord(2) = origXYord(m) then
--                  NULL;

               if segments.count = 0 and -- ((nearbyOrd.count <> 4 and XYord.count = 4) or
                 (xstart = xend and ystart = yend and
                 (xstart <> origXYord(1) or ystart <> origXYord(2)) and
                 (xstart <> origXYord(m-1) or ystart <> origXYord(m))) then

--               dbms_output.put_line('calling polyLR from edge_simplify ' || new_geometry.sdo_ordinates.count || ' orig ' || orig_geometry.sdo_ordinates.count);

                 segments := CHECK_POLYLR(orig_geometry,nearby_geometry,new_geometry);

--               dbms_output.put_line ('checking polyLR ' || nearby_edge_id || ' seg count ' || segments.count  );
               end if;


             elsif loops >= 1 and loops < 8 then


               Segments.trim(Segments.count);

--  dbms_output.put_line ('Calling self intersect check'); -- || polyxys.count); -- || ' x ' || polyxys(1) || '  xend ' || polyxys(polyxys.count-1));
               self_segs := check_for_self_intersect(new_Geometry,thousand);
--   dbms_output.put_line ('self_segs count'|| self_segs.count);
-- Build an array describing the intersections, the generalized segments and
-- their corresponding ungeneralzed segments.

               if self_segs.count > 0  then
--               dbms_output.put_line ('self segs ' || self_segs(1) || ' ' || self_segs(2));
                 isegments:= Process_self_segs(self_segs,OrigXyord,XYord);
--   this is just a flag, information is in isegments
                 Segments.extend(1);
             end if;
             else
                segments.trim(segments.count);
             end if;
--dbms_output.put_line ('>>>segs COUNT ' || segments.count);
             if segments.count > 0 then
               done := FALSE;
               if jj < loop_end then

--                 dbms_output.put_line ('calling get_split from edge-simplify');
                 isegments := GET_SPLIT_VERTICES(edge_id,start_node,end_node,
                              new_geometry,orig_geometry,nearby_edges,Edges_Table,orig_geometry,TRUE);
--    for pp in 1..isegments.count loop
--       dbms_output.put_line('pp ' || isegments(pp));
--    end loop;
-- Ignore a bad result from GET_SPLIT_VERTICES
                 if isegments.count > 0 and isegments(3) =0 then
                   isegments :=MDSYS.SDO_LIST_TYPE();
                 end if;
 --                 dbms_output.put_line ('AFTER get_split ' || isegments.count);
               end if;
               if isegments.count > 0 then
 --              dbms_output.put_line ('calling assemble with isegments '|| isegments.count || ' scale ' || scale);

                    new_co_count := ASSEMBLE_EDGE(Status,edge_id,start_node,end_node,iSegments,scale,nice,nearby_edges,orig_geometry,
                                    new_geometry,Edges_Table,method,topology,target_scale);
-- dbms_output.put_line ('>>>> BACK FROM assemble');
-- A problem with too many self intersections has occurred so redo the whole edge.
-- Example, a very sharp "V" exists (segments 2 and 3) with segment 2 hitting
-- a nearby but when we attempt to reshape segment 2 into 2 or more pieces
-- a self intersection will occur since segment 3 crosses the original crescent
-- shaped segments that segment 2 has to generalize.

--        segment 3     \           .  original dotted
--                       \      .    /
--                         \  .     /  segment 2
--                          .\     /
--                           . \  /
--                             .\/
--                               .

-- Assemble is supposed to be building a bent line so if it returns a segment
-- that is not useful so it is often best to reduce the scale.

                    if new_co_count <= 4 then
--dbms_output.put_line('new_co_count was ' || new_co_count ||' scale ' || scale);
                       get_scale(threshold,scale,target_scale,elength);

             --    Generalize the line either by Douglas-Peuker
                      if UPPER(method) = 'DP' then
                         new_geometry := sdo_util.simplify(orig_geometry,threshold,tolerance);
                      else  -- or by ZONE
                         new_geometry := GZ_UTIL_ZONE.line_simplify(orig_geometry,scale,nice,0.0,elength,edge_id,-1.,Topology,target_scale);
                      end if;
                      exit ;
                    end if;
-- Ensure that there are any vertices too close;
                 if new_co_count > 4 then
--                   dbms_output.put_line('calling remove ' || new_co_count);
                   ok := REMOVE_CLOSE_XYS(new_geometry);
                 end if;
               end if;
             end if;

           end Loop;

           exit when done or loop_counter > 1;
        END LOOP;



      END IF;
   END IF;


   if new_geometry is not null then
--       dbms_output.put_line('its good');

-- Don't round the start and end node !!
     if dec_digits <> 0 then
       xyOrd := new_geometry.sdo_ordinates;
       xyOrd(1) := origxyOrd(1);
       xyOrd(2) := origXYord(2);
       n := XYord.count;
       xyOrd(n-1) := origxyOrd(origXYOrd.count-1);
       xyOrd(n) := origXYord(origXYOrd.count);
       for ii in 3..n-2 Loop
         Xyord(ii) := ROUND(Xyord(ii),dec_digits);
       end loop;

       new_geometry.sdo_ordinates := XYord;

     end if;

-- Return the new Geometry

     RETURN new_geometry;
  else

-- Return the original Geometry

     RETURN orig_geometry;

  end if;

  RETURN NULL;

END EDGE_SIMPLIFY;
--
PROCEDURE FIX_FIGURE8(origXYord IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,XYord IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,dec_digits PLS_INTEGER) AS

-- Add 2nd and 2nd to last vertex to array of coordinates to fix some weird Oracle
-- bug that only affects figure of 8 polygons.

    polyXys   MDSYS.SDO_ORDINATE_ARRAY := XYord;
    inc       PLS_INTEGER;
    istart    PLS_INTEGER;
    iend      PLS_INTEGER;
    n         PLS_INTEGER := XYord.count;
    m         PLS_INTEGER := origXYord.count;
BEGIN
-- Just for figure 8 polygons
-- 4 cases:   1)  1 2                   3? 4
--            2)  1 2 missing           3? 4
--            3)  1 2?                  3 missing 4
--            4)  1 2?                  3  4

           istart :=5;
           iend := n;
           inc := 0;
--dbms_output.put_line('*******in FIX figure 8 !!!');
-- case 2 same, no extend, loop below does nothing
           if ROUND(Xyord(3),dec_digits) = ROUND(origXyOrd(3),dec_digits) and
              ROUND(Xyord(4),dec_digits) = ROUND(origXyOrd(4),dec_digits) then
              NULL;
           else
-- case 1 insert, extend, loop moves over by 2
             xyOrd.extend(2);
             inc :=-2;
           end if;

           Xyord(3) := origXyOrd(3);
           Xyord(4) := origXyOrd(4);
           for jj in istart..iend loop
             Xyord(jj) := PolyXys(jj+inc);
           end loop;
--           dbms_output.put_line('case 1');
-- case 4 same, store both vertices (3 is already there)
           if ROUND(polyXys(n-3),dec_digits) = ROUND(origXyOrd(m-3),dec_digits) and
              ROUND(polyXys(n-2),dec_digits) = ROUND(origXyOrd(m-2),dec_digits) then
               NULL;
-- case 3 insert,store both the 2nd to last and the last
           else
             xyOrd.extend(2);
           end if;
--           dbms_output.put_line('case 3');
           Xyord(xyOrd.count-1) := origXyOrd(m-1);
           Xyord(xyOrd.count)   := origXyOrd(m);
           Xyord(xyOrd.count-3) := origXyOrd(m-3);
           Xyord(xyOrd.count-2) := origXyOrd(m-2);

END;
--
Function estimate_area(x0 number,y0 number,x1 number,y1 number,x2 number,y2 number
                           ) return number as
   sinyc number;
   cosyc number;
   sin5yc number;
   factor number;
   approx_convert number := 1.000001;  --1.0000006;
begin
   return estimate_area(x0,y0,x1,y1,x2,y2,sinyc,cosyc,sin5yc,factor);
end;
--
Function estimate_area(x0 number,y0 number,x1 number,y1 number,x2 number,y2 number,
                            sinyc in out nocopy number,cosyc in out nocopy number,sin5yc in out nocopy number,factor in out nocopy number) return number as

-- Estimate the area of a triangle good to 5 or even 6 digits for angular
-- differences between coordinates of 0.002 degrees or less.
-- This function is expected to be called many times for evaluating the
-- significance of each vertex by measuring the triangular
-- area of the current point and its 2 adjacent neighbors.

 deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.

     estimated_area    number;                -- 111120 is 60 * nautical mile
     to_meters         NUMBER := 6173827200.; -- = 0.5*111120*111120
     yc                NUMBER;
     Begin

-- These factors are expected to be very slowly varying and need only be
-- calculated once for small polygons.

      if factor is NULL then
        yc := (y0+y1+y2)/3.;
        cosyc := cos(yc*deg2rad);
        sinyc := sin(yc*deg2rad);
        sin5yc := sin(5.*(yc-18.)*deg2rad);

-- This factor is generally optimized from the equator to latitude 50.

        factor := 0.9968658 + 0.0134185* sinyc*sinyc + 0.0095E-6*yc*yc -0.6E-7*(yc-40) - 1.e-5*sin5yc;

      end if;


      estimated_area := factor * to_meters * cosyc*(x0*y1 - y0*x1 + x1*y2 - y1*x2 + x2*y0 - y2*x0);

    return estimated_area;
    end;
--
FUNCTION ROBUST_LINE_GEN( geom MDSYS.SDO_GEOMETRY,cut NUMBER default 5) RETURN MDSYS.SDO_GEOMETRY AS

-- A short robust? approach to choosing best VIPs.
--
-- Cutoff is the number of distinct vertices (at least 3 for a polygon, or 2
-- for an unclosed edge.

   deg2rad     CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
   rad2deg     CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   New_geom    MDSYS.SDO_Geometry;
   Xys         MDSYS.SDO_ORDINATE_ARRAY := geom.sdo_ordinates;
   New_Xys     MDSYS.SDO_ORDINATE_ARRAY := geom.sdo_ordinates;
   Angles      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Areas       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Sort_Areas  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Ids         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Sort_Ids    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Orders      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Bad_list    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   bad         number;
   bad2        number;
   x0          number;
   y0          number;
   x1          number;
   y1          number;
   x2          number;
   y2          number;
   x3          number;
   y3          number;
   angle       number;
   cosyc       number;
   sinyc       number;
   sin5yc      number;
   factor      number;

   sum_area    number :=0.0;
   million     number := 1000000.;
   add_one     pls_integer :=0;
   cutoff_area number := cut;
   cutoff      pls_integer :=2;
   cutoff_start pls_integer := 2;
   n           pls_integer;
   next        pls_integer :=1;
   istart      pls_integer :=1;
   loops       pls_integer;
   nv          pls_integer;
   no_to_do    pls_integer;
   pos         pls_integer;
   shift       pls_integer;
   cutoff_save pls_integer;
   intersects_itself number :=1.;
   closed      boolean := FALSE;

   procedure Build_an_edge(Bad_list mdsys.sdo_list_type) as

-- Build an edge from a sorted area array and a sorted id array
     cutoff_to_use  pls_integer := cutoff;
     place          pls_integer;
     diff           number;
     least_diff     number := 1000000.;
     ij             pls_integer;
     ok             boolean;

   begin

   cutoff_to_use := cutoff_to_use+bad_list.count;

-- Typical list of vertices (ids) and areas. Note how the vertices are out of
-- order so to build a meaningful edge, we must go in vertex order.

--    Areas          Ids
--Area 10000000000  Id 1
--Area 21007.063726 Id 49
--Area 15451.86113  Id 47
--Area 11706.250133 Id 26
--Area 10431.369117 Id 29
--Area 9185.977502  Id 28
--Area 4713.557327  Id 42
--Area 4208.339749  Id 33
-- ...

-- we need to keep track of the "bad" vertices so we can ignore one or more.
-- Bad are marked negative. Caller must keep track of
      for ii in 1..Orders.count loop
        Orders(ii) :=ii;
      end loop;
      Sort_Areas := Areas;
      Sort_ids := Ids;   -- Need some sorted Ids so we know the vertex at the
                         -- end of each segment

      GZ_QA.stablesort3(sort_ids,Orders,Sort_areas,1,cutoff_to_use);
--for ii in 1..sort_ids.count loop

--  dbms_output.put_line('Vtx ' || sort_ids(ii) || ' A ' || round(sort_areas(ii),5));
--end loop;

      -- Always get start of edge or loop
      New_Xys(1) := Xys(1);
      New_Xys(2) := Xys(2);
      next :=2;
      for ii in 2..cutoff_to_use loop
          ok := TRUE;
          for jj in 1..bad_list.count loop
            if Sort_ids(ii) = bad_list(jj) then
               ok := FALSE;
              exit;
            end if;
          end loop;
          -- if there is a vertex to drop, it is a relative one
          if ok then
             ij := Sort_Ids(ii);
             next := next+2;
--             dbms_output.put_line('next ' || next || ' ij ' || ij || ' ids ' || sort_ids(ii));
             new_xys(next-1) := xys(ij*2-1);
             new_xys(next) := xys(ij*2);
--              dbms_output.put_line('CHOOSIing ' || round(xys(ij*2-1),7) ||','|| round(xys(ij*2),7));

          else
           ij := Sort_Ids(ii);
--             dbms_output.put_line('excluding ' || ij  ||' ' ||round(xys(ij*2-1),7) ||','|| round(xys(ij*2),7));
          end if;

      end loop;

  -- Always get end of edge or loop
      next := next+2;
      new_xys(next-1) := Xys(n*2-1);
      new_xys(next) := Xys(n*2);

   end;

   function find_intersections(check_angle number default 3.) RETURN NUMBER AS

 -- Exhaustive combinatorial search of every segment intersecting any
 -- other segment.That is, 1 with 3, 1 with 4,.., 2 with 4, 2 with 5 ..
 -- Just returns the first intersection it finds.
 --
      m        pls_integer := TRUNC(next/2);
      x11      number;
      y11      number;
   begin

      x1 := New_xys(1);
      y1 := New_Xys(2);


-- Do 2 things here, check for line intersections and needle like angles.
-- The intersections require 2 segment, the angle measurement just
-- 3 consecutive vertices.

      for ii in 2..m-1 loop
        x0 := x1;
        y0 := y1;
        x1 := New_Xys(ii*2-1);
        y1 := New_Xys(ii*2);
        x3 := New_Xys(ii*2+1);
        y3 := New_Xys(ii*2+2);

               -- or a skinny angle st the subject vertex that is just as bad

        if  GZ_QA.angle(x0,y0,x1,y1,x3,y3)*rad2deg < check_angle then
--            dbms_output.put_line('Found ' || ii );
--            dbms_output.put_line('x0 ' || round(x0,7) || ','||round(y0,7) ||' x2 ' || round(x2,7) ||','||round(y2,7));
--            dbms_output.put_line('x2 ' || round(x2,7) || ','||round(y2,7) ||' x3 ' || round(x3,7) ||','||round(y3,7));
            -- return end vertices of line segment
            return (ii+1)*million + ii;
        end if;

        for jj in ii+2..m loop

            x2 := x3;
            y2 := y3;
            x3 := New_Xys(jj*2-1);
            y3 := New_Xys(jj*2);
--            dbms_output.put_line('checking ' || (ii-1) || ' with ' || (jj-1) || ' angle ' || round( GZ_QA.angle(x11,y11,x2,y2,x3,y3)*rad2deg,3));

            -- check for an intersection between line segments

            if simple_intersect(x0,y0,x1,y1,x2,y2,x3,y3) then
--               dbms_output.put_line('found ' || ii || ' and ' || jj);
--               dbms_output.put_line('x0 ' || round(x0,7) || ','||round(y0,7) ||' x1 ' || round(x1,7) ||','||round(y1,7));
--               dbms_output.put_line('x2 ' || round(x2,7) || ','||round(y2,7) ||' x3 ' || round(x3,7) ||','||round(y3,7));
               -- return end vertices of line segment
               return ii*million + jj;
            end if;

        end loop;
      end loop;
      return 0.;
   end;


BEGIN

-- We always keep the first and last vertices

    n := TRUNC(xys.count/2);
--    Angles.extend(n);
--    Xys := remove_obtuse_angles(170.,Xys);
--    n := TRUNC(xys.count/2);

    if Xys(1) = Xys(Xys.count-1) and Xys(2) = Xys(Xys.count) then
      closed  := TRUE;
      nv :=  n-1;
      no_to_do := nv;
      x1 := Xys(n*2-3);
      y1 := Xys(n*2-2);
    else
      nv := n;
      no_to_do := nv-1;
      x1 := Xys(1);
      y1 := Xys(2);
    end if;
    Angles.extend(no_to_do);
    Areas.extend(no_to_do);
    Ids.extend(no_to_do);

    For ii in 1..no_to_do loop
      x0 := x1;
      y0 := y1;
      pos := ii*2;
      x1 := Xys(pos-1);
      y1 := Xys(pos);
      if NOT closed or ii < nv then
        x2 := Xys(pos+1);
        y2 := Xys(pos+2);
      else
        x2 := Xys(1);
        y2 := Xys(2);
      end if;
      ids(ii) :=ii;
      if NOT closed and ii=1 then -- no angle available for 1st point and we have to keep it
        areas(ii) := 1.E10;
      else
        Angles(ii) := GZ_QA.angle(x0,y0,x1,y1,x2,y2);
        areas(ii) := ABS(estimate_area(x0,y0,x1,y1,x2,y2,sinyc,cosyc,sin5yc,factor))*sin(angles(ii));
        sum_area := sum_area + areas(ii);
      end if;
--dbms_output.put_line('ii ' || ii || 'Area ' || round(areas(ii),6) || ' sin ' || round(sin(angles(ii)),8));
--  dbms_output.put_line('ii ' || ii ||' x0 ' || x0||','||y0 || ',' || x1||','||y1 || ',' || x2 ||','||y2);
    End Loop;
--    dbms_output.put_line('sum Area ' || round(sum_area,6));
    if closed then
      cutoff_area := cutoff_area*sum_area;
      cutoff_start := 4;
    else
      cutoff_area := cutoff_area*sum_area;
    end if;
--    dbms_output.put_line('sum Area ' || round(cutoff_area,6) || ' nv ' || nv);
    gz_qa.shellsort2(areas,ids,1,no_to_do,'DESC');

--    for ii in 1..20 loop
--      dbms_output.put_line('Area ' || round(areas(ii),6) || ' Id ' || ids(ii));
--    end loop;
-- User just wants 1 extra vertex
    if cut <=0.0 then
      if closed then
-- When the user specified cutoff is zero, circulate the original geometry
        if cut=0.0 then
          shift := ids(1)-1;
          new_Xys := GZ_QA.circulate_coordinates(Xys,shift);
          new_geom := geom;
          new_geom.sdo_ordinates := new_Xys;
          return new_geom;
        else
          cutoff := 3;  --with 1st vertex you get a triangle
        end if;
      else
        cutoff := 1;  -- with 1st vertex you get straight line
      end if;
    elsif cut >=1 then
       cutoff := cut-1;

         for ii in 1..cut loop
         if closed and ids(ii) = 1 then
            cutoff := cutoff+1;
           exit;
         end if;
         end loop;

--         dbms_output.put_line('Cutoff ' || cutoff );
    else
--    we always include vertex 1

--       dbms_output.put_line('cutoff ' || cutoff);
      cutoff := nv;
      for ii in 1..areas.count loop
         if closed and ids(ii) = 1 then
           add_one :=+1;
         end if;
--        dbms_output.put_line('sum ' || round(cutoff_area,6) || ' area ' || round(areas(ii),6) || ' id ' || ids(ii));
         if areas(ii) < cutoff_area then
            cutoff := ii-1 +add_one;
--             dbms_output.put_line('cccutoff ' || cutoff);
            exit;
         end if;
      end loop;
    end if;
--     for ii in 1..areas.count loop
--      dbms_output.put_line('area ' || round(areas(ii),6) || ' id ' || ids(ii)|| ' sin ' || round((angles(ids(ii)))/deg2rad,4));
--      end loop;
-- Ensure we get at least a triangle
    if closed then   -- we have 2 vertices, 1 distinct
      if cutoff < 3 then
      cutoff := 3;
      elsif cut <= 1. then
        cutoff := cutoff-1;
      end if;
    end if;
--    dbms_output.put_line('cutoff ' || cutoff || ' nv ' || nv);
--------------------------------------------------------------------------------
--  Ensure segment does not self intersect


    Orders.extend(ids.count);
    Sort_areas.extend(ids.count);
    cutoff_save := cutoff;
    New_xys.extend(Xys.count);

    WHILE cutoff <= n and intersects_itself <> 0.0 LOOP

      Build_an_edge(bad_list);

-- Some loops can be problematic and so we want to try fewer coordinates
-- before we have to use more.

--      if cutoff > cutoff_start then
--         cutoff := cutoff -1;
--      else
--        if cutoff <= cutoff_start then
--           cutoff := cutoff_save;
--        end if;

--      end if;
      intersects_itself := find_intersections;

-- Deal with any intersections
-- Loops or near loops can be problematic so if we just added a point we may
-- need to skip that one.
      loops :=0;

      while loops < 2 and  intersects_itself <> 0 and cutoff >= cutoff_start loop
        loops := loops +1;

--        dbms_output.put_line('II ' || intersects_itself);

        bad  := TRUNC(intersects_itself/million);
        bad2 := intersects_itself - bad*million ;
        if bad =TRUNC(next/2) then
          bad := bad-1;
        end if;
        if bad2 =TRUNC(next/2) then
          bad2 := bad2-1;
        end if;

        if Sort_Areas(bad2) < Sort_Areas(bad) then
        bad := bad2;
--        dbms_output.put_line('bbad ' || bad || ' bad2 ' || bad2);
        end if;
        -- If its already in the list, ignore 2nd occurrence
        for ii in 1..bad_list.count loop
           if bad = bad_list(ii) then
              bad :=0;
              exit;
           end if;
        end loop;
        -- else add it to the list.
        if bad <> 0 then
          Bad_list.extend(1);
          bad_list(Bad_list.count) := Sort_ids(bad);
--          dbms_output.put_line('trying to drop vertex ' || Sort_ids(bad) || ' bad count ' || bad_list.count);
        end if;
        Build_an_edge(bad_list);
        intersects_itself := find_intersections;
--        dbms_output.put_line('i itself ' || intersects_itself || ' next was ' || next);

      end loop;
      cutoff := cutoff+1;

    END LOOP;
    new_xys.trim(new_xys.count-next);
--    dbms_output.put_line('FINAL ' ||new_xys.counT);
    new_geom := geom;
    new_geom.sdo_ordinates := new_xys;

  RETURN new_geom;

END Robust_Line_Gen;
--
FUNCTION Fast_Vincenty_gcd(x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,units VARCHAR2 DEFAULT 'm') RETURN NUMBER DETERMINISTIC AS
/**
--###################################################################################################################
 # Program Name: fast_vincenty_gcd
 # Author: Sidey Timmins
 # Creation Date: 8/16/2006
 #
 # Usage:
 #    Call this program from inside another PL/SQL program.  This program
 #    has 4 required parameters:
 #
 #    REQUIRED Parameters:
 #        x1,y1           - 1st point longitude,latitude) (degrees)
 #        x2,y2           - 2nd point longitude,latitude  (degrees)
 #
 #    OPTIONAL Parameter:
 #        unit         - 'USM' : US Miles
 #                     - 'm' : meters (default)
 #                     - 'ft': feet (US)
 # Purpose:
 #    Calculates Great circle distance (shortest line on the ellipsoid - a model
 #    of the earth - a sphere with flattening at the poles) very accurately. The
 #    line is between 2 points in geodetic coordinates (degrees). The trace of
 #    this line on the elipsoid is called a geodesic. Uses GRS 80 ellipsoid.
 #
 #    This function exceeds the Oracle sdo_length accuracy (
 #    set length = sdo_geom.sdo_length(geometry,0.5,'unit=meter') and matches to
 #    3 decimal digits (or more) the superlative accuracy of Charles Karney's code in the
 #    Geodesic package for great circles up to half way round the world!
 #    It (like Oracle) does not work for antipodal and near antipodal points.
 #    This function is about 1.5 faster than sdo_length.
 #
 #    Returns: Great circle distance in meters
 #
 # Reference: T.Vincenty "Direct and Inverse Solutions of Geodesics on the
 #            Ellipsoid with applications of nested equations", Survey Review,
 #            Vol XXII, 176, April 1975
 #            http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf
 #            http://www.moveable-type.co.uk/scripts/LatLongVincenty.html
 #            for ellipsoids see: http://www.pcigeomatics.com/cgi-bin/pcihlp/PROJ%7CEARTH+MODELS%7CELLIPSOIDS%7CELLIPSOID+CODES
 # Dependencies:
 #    GZ_UTIL_ZONE.c_sincos
 #    GZ_UTIL_ZONE.fast_atan2
 #
 # Limits: This function does not work correctly for antipodal points and points
 #         within about 0.6 degrees longitude of antipodicity.
 #         Use geodesic.inverse instead.
 #
 # Modification History:
 #    09/14/2007, Nick Padfield - Ported code into the CDB_NN Package
 #
--###################################################################################################################
*/
  twopi       NUMBER   :=6.2831853071795864769252867665590057684;     -- 2 * pi
  deg2rad     NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
  a           NUMBER   := 6378137.;        -- GRS-80  ellipsoid radius in meters
  b           NUMBER   := 6356752.31414035584785210686153; --minor radius

  a2          NUMBER   := 40680631590769.;               -- ellipsoid radius ^2
  b2          NUMBER   := 40408299983328.76931725432420763849393114;  -- minor radius ^2
--  e2          NUMBER   := 0.00669438002290078762535911470306; -- (a2-b2)/b2 = (1./f) *(2.-1./f)
-- This is eprime squared
  e2          NUMBER   := 0.0067394967754789582381665683977103296775;
  F           NUMBER   := 0.003352810681182318935434146126128510783;
  lambda      NUMBER ;
  u1          NUMBER ;
  u2          NUMBER ;
  one_ov5040  NUMBER := 0.0001984126984126984126984126984126984127;
  one_ov120   NUMBER := 0.0083333333333333333333333333333333333333;
  one_ov6     NUMBER := 0.1666666666666666666666666666666666666667;
  tantheta1   NUMBER ;
  tantheta2   NUMBER ;
  b_over_a    NUMBER  := 0.996647189318817681064565853873944695763;    -- b/a
  temp        NUMBER;

  sinU1       NUMBER ;
  sinU2       NUMBER ;
  cosU1       NUMBER ;
  cosU2       NUMBER ;
  sinU1CosU2  NUMBER ;
  sinU2CosU1  NUMBER ;
  sinU1sinU2  NUMBER ;
  cosU1cosU2  NUMBER ;
  lambdaP     NUMBER ;
  lambda2     NUMBER ;
  lambda3     NUMBER ;
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
  dtan        NUMBER;
  del         NUMBER;
  factor      NUMBER := -2.25;  -- empirically derived
  threshold   NUMBER := 1.E-13;

BEGIN

-- F :=  (a-b)/a;   -- 1./ 298.257222101;           -- flattening

 tantheta1 := b_over_a*tan(y1 * deg2rad) ;     -- theta is latitude
 tantheta2 := b_over_a*tan(y2 * deg2rad) ;

 L := (x2 - x1) * deg2rad ; -- Difference in longitude
 temp :=b_over_a;
-- differences are in .01 mm over a 111000 meter distance using fast_atan2
 u1 := GZ_UTIL_ZONE.fast_atan2(Tantheta1,1.) ;
 u2 := GZ_UTIL_ZONE.fast_atan2(Tantheta2,1.) ;


 sinU1 := GZ_UTIL_ZONE.sincos(u1,cosU1) ;
 sinU2 := GZ_UTIL_ZONE.sincos(u2,cosU2) ;


 sinU1CosU2 := sinU1 * cosU2;
 sinU2CosU1 := sinU2 * cosU1;
 sinU1sinU2 := sinU1 * sinU2;
 cosU1cosU2 := cosU1 * cosU2;

 lambda := L ; --* 1.0021;  -- empirical factor to reduce the number of iterations
 lambdaP := twopi ;

 FOR iter IN 1 .. 20 LOOP

 --   itercount := itercount + 1;
 -- Calculate the sine from the Taylor series (truncated) for small sines
 -- The Vincenty method is not limited by either the sine/cosine or atan2
 -- values calculated below.

    if abs(lambda) < .04 then   -- good to 2 degrees  -> this .04 constant
      lambda2 := lambda*lambda;
      lambda3 := lambda2*lambda;
      sinLambda := lambda - lambda3*one_ov6 + lambda3*lambda2*one_ov120;
      coslambda := sqrt(1.-sinlambda*sinLambda);
    else
      sinLambda := GZ_UTIL_ZONE.sincos(lambda,cosLambda) ;
    end if;


-- Note: exponentiation is not as accurate.
    sinSigma := Sqrt((cosU2 * sinLambda) * (cosU2 * sinLambda) +
   (sinU2cosU1 - sinU1cosU2 * cosLambda)* (sinU2cosU1 - sinU1cosU2 * cosLambda)) ;
    If (sinSigma = 0.) Then
        gcd := 0. ;
        RETURN gcd;                --  co-incident points
    End If ;
    cosSigma := sinU1sinU2 + cosU1cosU2 * cosLambda ;
    Sigma    := GZ_UTIL_ZONE.fast_atan2(sinSigma,cosSigma) ;

    sinAlpha := cosU1cosU2 * sinLambda / sinSigma ;
    cosSqAlpha := 1. - sinAlpha * sinAlpha ;
    If (cosSqAlpha = 0.) Then
      gcd := Abs(a * L) ;      -- two points on equator
      RETURN gcd ;
    End If ;
    cos2sigmaM := cosSigma - 2. * sinU1sinU2 / cosSqAlpha ;
    cos22 := 2. * cos2sigmaM * cos2sigmaM;
    c          := F *0.0625 * cosSqAlpha * (4. + F * (4. - 3. * cosSqAlpha)) ;
    lambdaP    := lambda ;
    temp := (Sigma + c * sinSigma *(cos2sigmaM + c * cosSigma *(cos22 -1.))) ;

-- Accelerate the computation by reducing the number of iterations
-- This factor is empirical.

    lambda     := L + (1. - c*factor) * F * sinAlpha * temp ;
--    dbms_output.put_line('lambdap ' || round(lambdap,14) || ' lambda ' || round(lambda,14) || ' lambdap/lambda ' || round(lambdap/lambda,12));
    factor := 1.0;
--    g_iter := g_iter +1.;
    EXIT WHEN Abs(lambda - lambdaP) < threshold ;

/*    if lambdap > 3.0 and (lambdap/lambda > 1.002 or lambdap/lambda < 0.998) then
      if lambdap < lambda then
        lambda := lambda*0.05+ lambdap*0.95;
      else
        lambda := lambda*0.05+ lambdap*0.95;
      end if;
--      lambda := lambda*0.2+ lambdap*0.8;
    elsif lambdap > 3.0 then
       lambda := lambda *0.25 + lambdap*0.75;
--       lambda := 3.14159265;
--      lambda := lambda*0.22223+ lambdap*0.77777;
   end if;
   */
  END LOOP ;


--  If Abs(lambda - lambdaP) > threshold Then
--      lambda := lambdap;
--    gcd := -1. ;
--    RETURN gcd ;                       -- formula failed to converge
--  End If ;

  uSq  := cosSqAlpha * e2 ;

  temp := (4096. + uSq * (-768. + uSq * (320. - 175. * uSq)))  ;
  aa   := 1. + uSq * temp * 0.00006103515625;
  BB   := uSq * 0.0009765625  * (256. + uSq * (-128. + uSq * (74. - 47. * uSq))) ;
  deltaSigma := BB * sinSigma * (cos2sigmaM + BB *0.25 * (cosSigma *
                (cos22 -1.) - BB / 6. * cos2sigmaM *
                (-3. + 4. * sinSigma * sinSigma) * (2.*cos22 -3.))) ;

 -- Final Vincenty Great circle distance in meters
  gcd  := b * aa * (Sigma - deltaSigma) ;

 -- result is already in meters

   IF (units = 'ft') THEN       -- US statutory feet
     gcd := gcd * 39.37/12.;
   ELSIF (units = 'USM') THEN   -- US Miles
     gcd := gcd * 39.37/63360.;
   END IF ;

   RETURN ROUND(gcd,6);

END fast_vincenty_gcd;
--
FUNCTION FIND_COORDINATE_WITHIN_EDGE(x NUMBER,y NUMBER,XYOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_Array,tt IN OUT NOCOPY NUMBER,ptolerance NUMBER default 0.05)  RETURN NUMBER AS

-- Locate a pair of coordinates (either vertex within the edge or a new point near
-- or on the edge) within an edge and return the particular segment that it
-- falls within.

--        * (x,y)  falls within 1 to 2
--      +-----------------+-----------------+------------+  XyOrd
--      1                 2                 3            4
--         Segment 1         Segment 2         Segment 3

/*
********************************************************************************
--Program Name: Find_Coordinate_within_edge
--Author: Sidey Timmins
--Creation Date: 05/26/2010

--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 3 required parameter:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      (x,y)       - A point to find
  --      XYOrd       - Array of ordinates to search
  --      tt          - Position found as a fraction (line parameter)
  --      ptolerance  - Oracle tolerance to pass to SDO_INTERSECTION.
  --      OUTPUT

--
-- Purpose: Returns the segment that a particular (x,y) pair fall within.

-- Called by: Find_Intersection_Segment
-- Dependencies: None
********************************************************************************
*/
deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
    found             NUMBER :=0.0;
    n                 PLS_INTEGER := TRUNC(XYord.count/2);
    jj                PLS_INTEGER;
    x0                NUMBER;
    y0                NUMBER;
    x1                NUMBER;
    y1                NUMBER;
    xhalf             NUMBER;
    xdiff             NUMBER;
    xLL               NUMBER;
    yLL               NUMBER;
    xUR               NUMBER;
    yUR               NUMBER;
    t                 NUMBER;
    xp                NUMBER;
    yp                NUMBER;
    delta             NUMBER;
    eps               NUMBER;
    ytolerance        NUMBER;
    m                 PLS_INTEGER;
BEGIN

  x1 := XYOrd(1);
  y1 := XYOrd(2);
  tt := 0.0;
  if x = x1 and y = y1 then
     found := 1.0;
     n :=0;
  elsif x = XYOrd.count-1 and y = XYOrd.count then
     found := n;
     n :=0;
     tt := 1.0;
  end if;

-- Convert 0.05 meters to degrees

   if delta is NULL then
     delta := ptolerance/(111319.490793274*cos(y1*deg2rad));
   end if;

   m := n;
   if MOD(m,2) = 0 then
     m := m + 1;
   end if;
   xhalf := XYOrd(m);
   xdiff := abs(xhalf-x1);
--   ytolerance := 0.00005; --.0011;
   ytolerance := 0.0000004;
--   dbms_output.put_line('ytol ' || ytolerance);
   eps := 0.0011;
   if xdiff > 1. then
       eps :=  eps* xdiff*xdiff;
   end if;

  For ii in 2..n Loop
     x0 := x1;
     y0 := y1;
     jj := ii*2;
     x1 := XYOrd(jj-1);
     y1 := XYOrd(jj);
     if x = x1 and y = y1 then
       found := ii;
       exit;
     end if;
     IF x0 <> x1 or y0 <> y1 THEN
       xLL := x0;
       yLL := y0;
       if x1 < x0 then
         xLL := x1-delta;
         xUR := x0+delta;
       else
         xLL := xLL-delta;
         xUR := x1+delta;
       end if;

       if y1 < y0 then
         yLL := y1-eps;
         yUR := y0+eps;
       else
        yLL := yLL-eps;
         yUR := y1+eps;
       end if;
--     'NOT( (( m.XUR <:1)  OR (m.YUR <:2)) OR  (( m.XLL >:3) OR (m.YLL > :4)))';
--   The x,y pair fall withing the range of this window.
-- Now, are they on the line?
/*
       if ii < 0 then
       dbms_output.put_line('x ' || x || ' xLL ' || xll);
       dbms_output.put_line('y ' || y || ' yLL ' || yLL);
       dbms_output.put_line('X ' || x || ' xUR ' || xur);
       dbms_output.put_line('Y ' || y || ' yUR ' || yur);

       dbms_output.put_line('ii ' || ii);
       dbms_output.put_line('X0 ' || x0 || ' y0 ' || y0);
       dbms_output.put_line('X1 ' || x1 || ' y1 ' || y1);
       end if;
*/
       if NOT ((xUR < x OR yUR < y) OR (xLL > x OR yLL > y)) then
-- Calculate a line parameter  (LRS value)
--     x = t.x0 + (1-t).x1

        if x0 <> x1 then
          t := (x-x1) /(x0-x1);
--          if ii < 4 then
--            dbms_output.put_line('t ' || t);
--            dbms_output.put_line('x-x0 ' || (x-x0) || ' ' || (x0-x1));
--          end if;
          if t >= 0. and t <= 1. then
            yp := t*y0 + (1.-t)*y1;
--          if ii < 4 then
--            dbms_output.put_line('yp ' || yp || ' diff ' || abs(y-yp));
--          end if;
--dbms_output.put_line('yp ' || (y-yp) || ' ytol ' || ytolerance);
          if abs(y-yp) < ytolerance then
             found := (ii-1);
             tt := 1.-t;
             exit;
          end if;
          end if;
        else
          t := (y-y1) /(y0-y1);
--          dbms_output.put_line('T ' || t);
          if t >= 0. and t <= 1. then
          xp := t*x0 + (1.-t)*x1;
          if abs(x-xp) < ytolerance then
            found := (ii-1);
            tt := 1.-t;
            exit;
          end if;
          end if;
        end if;

       end if;
     END IF;
  End Loop;

  RETURN found;

END FIND_COORDINATE_WITHIN_EDGE;
--
FUNCTION FIND_INTERSECTION_SEGMENT(geometry1 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                                   geometry2 IN OUT NOCOPY MDSYS.SDO_GEOMETRY
                                  ) RETURN MDSYS.SDO_LIST_TYPE AS
--******************************************************************************
-- Using 2 geometries, determine the segment in line string1 that intersects
-- line string2.
-- Can be used to check if a generalized geometry intersects a nearby geometry.
-- Note that any shared node (the start and end of geometry1) are not reported.
/*
********************************************************************************
--Program Name: Find_Intersection_Segment
--Author: Sidey Timmins
--Creation Date: 05/20/2010
--Update: July 2010 to use Cartesian space per Dan Geringer info about how
-- Topomaps work.
--        09/01/2010 Handle 2 identical segments.
--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 3 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      geometry1     - A subject geometry
  --      geometry2     - A 2nd geometry

--
-- Purpose: In Cartesian space, returns an array of intersecting segments.

-- Dependencies: GZ_UTIL_ZONE.accurate_gcd, find_coordinate_within_edge
--   SDO_GEOM.SDO_INTERSECTION, SDO_LRS.convert_to_lrs_geom,SDO_LRS.FIND_MEASURE
********************************************************************************
*/
     XYOrd             MDSYS.SDO_ORDINATE_Array;
     XYOrd2            MDSYS.SDO_ORDINATE_Array;
     intersections     MDSYS.SDO_GEOMETRY;
     point             MDSYS.SDO_GEOMETRY;
     lrsgeom           MDSYS.SDO_GEOMETRY;
     seg_geometry      MDSYS.SDO_GEOMETRY;
     sql_stmt          VARCHAR2(4000);
     sql_stmt2         VARCHAR2(4000);
     n                 PLS_INTEGER;
     m                 PLS_INTEGER;
     gtype             NUMBER;
     srid              NUMBER := geometry1.sdo_srid;
     x                 NUMBER;
     y                 NUMBER;
     x1                NUMBER;
     y1                NUMBER;
     x2                NUMBER;
     y2                NUMBER;
     x3                NUMBER;
     y3                NUMBER;
     x4                NUMBER;
     y4                NUMBER;
     seg               NUMBER;
     measure           NUMBER;
     tt                NUMBER;
     distance          NUMBER;
     small_tolerance   NUMBER := 1.E-20;
     Segments          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
     kount             PLS_INTEGER := 0;
     loops             PLS_INTEGER :=0;
     bad               PLS_INTEGER;
BEGIN

    IF geometry1 is NULL then
      RETURN Segments;
    END IF;

    XYOrd := geometry1.sdo_ordinates;
--     bad := check_for_dups(XYord);
--      if bad <> 0 then
--       dbms_output.put_line('bad vertex ' || bad);
--    end if;
    x1 := XYOrd(1);
    y1 := XYOrd(2);
    x2 := XYOrd(XYOrd.count-1);
    y2 := XYOrd(XYOrd.count);
    n := TRUNC(XYOrd.count/2);
--    for pp in 1..n loop
--       dbms_output.put_line('XX ' || round(xyord(pp*2-1),7) || ',' || round(xyord(pp*2),7));
--    end loop;
-- Check to find the number of intersections;
    XYOrd2 := geometry2.sdo_ordinates;
--    bad := check_for_dups(XYord2);
--    if bad <> 0 then
--       dbms_output.put_line('Bad vertex ' || bad);
--    end if;
--    for pp in 1..8 loop
--       dbms_output.put_line('X2 ' || xyord2(pp*2-1) || ',' || xyord2(pp*2));
--    end loop;
    geometry1.SDO_SRID := NULL;
    geometry2.SDO_SRID := NULL;
    intersections := SDO_GEOM.SDO_INTERSECTION(geometry1,geometry2,small_tolerance);
    geometry1.SDO_SRID := SRID;
    geometry2.SDO_SRID := SRID;
    if intersections is NULL then
--       dbms_output.put_line('intersect is null' || tolerance);
       RETURN Segments;
    end if;


--    sql_stmt := 'INSERT into points values(:1,:2)';
--    execute immediate sql_stmt using 100,intersections;
--    sql_stmt := 'INSERT into points values(:1,:2)';
--    execute immediate sql_stmt using 1,geometry1;
-- commit;
    gtype := intersections.sdo_gtype;
--    dbms_output.put_line('GTYPE: ' || gtype);

    if gtype = 2001. or gtype=2002. or gtype = 2005. or gtype = 2004. then
       m := TRUNC(intersections.sdo_ordinates.count/2);
--       dbms_output.put_line('MM ' || m || ' gtype ' || gtype);
       Segments.extend(m);

       for ii in 1..m loop
       x := intersections.sdo_ordinates(ii*2-1);
       y := intersections.sdo_ordinates(ii*2);
--     dbms_output.put_line('intersection ' || ii || ' x ' || x || ' y ' || y);
-- Handle a very unusal case - a bent edge that we generalize straight intersecting
-- and overlaying a nearby edge.
--                              -----+
--                 edge 1      |    /   edge 2
--                             |   /  This is both an edge and the generalization of 1
--                             |  /
--                             |/
--                             +

      if (ROUND(x,7) = ROUND(x1,7) and ROUND(y,7) = ROUND(y1,7)) then

         if ii = 1 and gtype=2002. and n=2 and ROUND(intersections.sdo_ordinates(3),7) = ROUND(x2,7) and ROUND(intersections.sdo_ordinates(4),7) = ROUND(y2,7) then
           segments(1) := 1;
           kount := 1;
--           dbms_output.put_line('here');
           exit;
         end if;
      elsif (ROUND(x,7) = ROUND(x2,7) and ROUND(y,7) = ROUND(y2,7)) then

          if ii = 1 and gtype=2002. and n=2 and ROUND(intersections.sdo_ordinates(3),7) = ROUND(x1,7) and ROUND(intersections.sdo_ordinates(4),7) = ROUND(y1,7) then
           segments(1) := 1;
           kount := 1;
--            dbms_output.put_line('Here');
           exit;
         end if;
--       dbms_output.put_line('x1y1 ' || ii || ' x1 ' || x1 || ' y ' || y1);
--       dbms_output.put_line('x2y2 ' || ii || ' x2' || x2 || ' y ' || y2);
      elsif gtype <> 2002 and ((ROUND(x,7) = ROUND(x1,7) and ROUND(y,7) = ROUND(y1,7)) or (ROUND(x,7) = ROUND(x2,7) and ROUND(y,7) = ROUND(y2,7))) then
--       dbms_output.put_line('at round 7 null');
          NULL;
      else
        distance := GZ_UTIL_ZONE.accurate_gcd(x1,y1,x,y);
--        dbms_output.put_line('seg ' || seg || ' n ' || n || ' tt ' || round(tt,10) || ' dist ' || distance);
        if distance > 1.E-6 then
--       if x1 <> x or y1 <> y then
-- This is a cartesian approximation
       Seg := find_coordinate_within_edge(x,y,XYOrd,tt);
--       dbms_output.put_line('seg ' || seg || ' n ' || n || ' tt ' || round(tt,10) || ' dist ' || round(distance,8));
--       if (tt = 0.0 and seg = 1.0) or (tt = 1.0 and seg = n) then
--         seg := 0.0;
--       end if;
--       if tt =1.0 then
--          Seg := seg+1;
--       end if;
--      dbms_output.put_line('seg ' || seg); -- || ' n ' || n || ' x ' || x || ' y ' || y);
       loops := 0;
-- Verify using srid = 8265
       WHILE seg <> 0 and seg <> n and loops < 3 Loop
        loops := loops + 1;
        point := SDO_GEOMETRY(2001,NULL,
                                  SDO_POINT_TYPE(x,y,NULL),NULL,NULL);
         x1 := XYOrd(seg*2-1);
         y1 := XYOrd(seg*2);
         x2 := XYOrd(seg*2+1);
         y2 := XYOrd(seg*2+2);

-- Enforce the following rule: 0 belongs to the current segment and 1 belongs to
-- the next segment
--        0             1
--        +------------|+-------------

         if x1 = x and y1 = y then
           exit;
         elsif x2 = x and y2 = y then

           seg := seg + 1;  -- enforce that intersection at end => n
            dbms_output.put_line('SEG now two ' ||seg);
           exit;
         else
           seg_geometry := SDO_GEOMETRY(2002,NULL,NULL,SDO_ELEM_INFO_Array(1,2,1),
                                      SDO_ORDINATE_ARRAY(x1,y1,x2,y2));

          lrsgeom := sdo_lrs.convert_to_lrs_geom(seg_geometry,0.,1.);
          measure := SDO_LRS.FIND_MEASURE(lrsgeom,point);

          exit when measure >= 0. and measure < 1.;

          if measure = 1. then

            seg := seg + 1;
            exit;
          end if;
         end if;
         seg := seg + 1;
       End Loop;
       If Seg <> 0 and seg <> n then
         kount := kount + 1;
         Segments(kount) := Seg;
       End if;
       End If;
--       if measure >=0. and measure <= 1. then
--       dbms_output.put_line('SEG ' || round(measure,6)  || ' seg ' || seg || ' tt ' || round(tt,6));
--       end if;
       END IF;
       end loop;
    end if;


    Segments.trim(Segments.count-kount);

--dbms_output.put_line('returning ' || kount || ' seg ' || segments.count);
  RETURN Segments;

END FIND_INTERSECTION_SEGMENT;
--
FUNCTION FIND_MATCHING_SEGMENT(seg PLS_INTEGER, XYOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,GenXYOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,epsilon NUMBER default 1.E-6) RETURN PLS_INTEGER AS

/*
********************************************************************************
--Program Name: Find_Matching_Segment
--Author: Sidey Timmins
--Creation Date: 05/26/2010

--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 4 required parameter:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      seg         - A segment in GenXyord to find
  --      XYOrd       - Array of ungeneralized ordinates to search
  --      GenXYOrd    - Array of generalized ordinates that corresponds to Xyord
  --      epsilon     - a small difference in degrees to measure equality
--                      between the point to find and the nearest ordinate.

-- Purpose: Searches for a segment in the ungeneralized ordinates and returns
--          corresponding generalized segment number.

-- Dependencies: None
********************************************************************************
*/
  found        PLS_INTEGER := 0;
  found2       PLS_INTEGER;
  ii           PLS_INTEGER := -1;
  x            NUMBER;
  y            NUMBER;
  genseg       PLS_INTEGER;

BEGIN

  For jj in 1..TRUNC(GenXYOrd.count/2)-1 loop
      ii := ii + 2;               -- ii is odd
      x := GenXYord(ii);
      y := GenXYord(ii+1);
      found := find_matching_xy(x,y,XYOrd,epsilon);
      x := GenXYord(ii+2);
      y := GenXYord(ii+3);
      found2 := find_matching_xy(x,y,XYOrd,epsilon);
-- We adopt the [...) rule. A segment begins where it starts.
      if seg >= found and seg < found2 then
         genseg := TRUNC(ii+1)/2;
         RETURN genseg;
      end if;
  End Loop;

  RETURN 0;

END FIND_MATCHING_SEGMENT;
--
FUNCTION FIND_MATCHING_XY(x NUMBER,y NUMBER, XYOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,epsilon NUMBER default 1.E-6,istart PLS_INTEGER default 1) RETURN PLS_INTEGER AS

/*
********************************************************************************
--Program Name: Find_Matching_XY
--Author: Sidey Timmins
--Creation Date: 05/26/2010

--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 4 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      (x,y)       - A pair of ordinates (point) to find
  --      XYOrd       - Array of ordinates to search
  --      epsilon     - a small difference in degrees to measure equality
--                      between the point to find and the nearest ordinate.

-- Purpose: Searches for an (x,y) pair and returns matching vertex number

-- Dependencies: None
********************************************************************************
*/
  found        PLS_INTEGER := 0;
  ii           PLS_INTEGER := istart*2-3;

BEGIN

--  dbms_output.put_line('XX ' ||x || ' YY' ||y);
  For jj in istart..TRUNC(XYOrd.count/2) loop
      ii := ii + 2;
--      dbms_output.put_line('ii ' || ii ||' xx ' ||xyord(ii) || ' y ' ||Xyord(ii+1));
      if ABS(x - XYord(ii)) <= epsilon  and ABS(y - XYOrd(ii+1)) <= epsilon then
        found := TRUNC((ii+1)/2);   -- return the segment number.
--        dbms_output.put_line('found ' || found || ' x ' || x || ' y ' || y);
--        dbms_output.put_line('Found ' || found || ' x ' || xyord(ii) || ' y ' || xyord(ii+1));
        exit;
      end if;
  End Loop;

  RETURN found;

END FIND_MATCHING_XY;
--
FUNCTION Find_Segment(X NUMBER,Y NUMBER,PolyMBR IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,XYS IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER,distance_found IN OUT NOCOPY NUMBER,epsilon NUMBER default 0.0) RETURN PLS_INTEGER Deterministic As

-- Search Xys to find whether a point is in the vicinity of a particular segment
-- and return the segment number. The point may be just within the MBR of the
-- segment or on it.

 rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   Distances          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   xnear              NUMBER;
   ynear              NUMBER;
   xLL                NUMBER;
   yLL                NUMBER;
   xUR                NUMBER;
   yUR                NUMBER;
   xtemp              NUMBER;
   ytemp              NUMBER;
   xlast              NUMBER;
   ylast              NUMBER;
   x1                 NUMBER;
   y1                 NUMBER;
   x2                 NUMBER;
   y2                 NUMBER;
   xb                 NUMBER;
   yb                 NUMBER;
   angleb             NUMBER :=0.0;
   angle              NUMBER;
   check_distance     NUMBER;

   xtest              NUMBER := X;
   ytest              NUMBER := Y;
   dist               NUMBER;
   dist2              NUMBER;
   big                NUMBER := 1.E10;
   last_dist          NUMBER := 1.E10;
   mult               NUMBER := 1.E7;
   rmult              NUMBER := 1.E-7;
   m                  PLS_INTEGER;
   kk                 PLS_INTEGER;
   ibehind            PLS_INTEGER;
   istart             PLS_INTEGER;
   iend               PLS_INTEGER;
   mloops             PLS_INTEGER;
   seg                PLS_INTEGER :=0;
   meters             BOOLEAN := TRUE;
   got_perpendicular  BOOLEAN;
BEGIN


  if SRID is NULL or SRID <> 8265. then
    meters := FALSE;
  end if;
  m := polyMBR(5);

  IF epsilon = 0. then
     For k in 1..m Loop
       kk := k*6;
       xLL := polyMBR(kk+1);
       yLL := polyMBR(kk+2);
       xUR := polyMBR(kk+3);
       yUR := polyMBR(kk+4);

       if (Y < yLL or Y > yUR) OR (X < xLL or X > xUR) then
          NULL;
       else
--  Check to locate the point within the range of this MBR
         istart := polyMBR(kk+5);
         iend := polyMBR(kk+6);
         xlast := XYs(istart);
         ylast := XYs(istart+1);
         For i in istart+2..iend Loop
           if MOD(i,2) = 1 THEN
           xLL  := xlast;
           yLL  := ylast;
           xUR := XYs(i);
           yUR := Xys(i+1);
           xlast := xUR;
           ylast := yUR;
           if xUR < xLL then
              xtemp := xLL;
              xLL := xUR;
              xUR := xtemp;
           end if;
           if yUR < yLL then
              ytemp := yLL;
              yLL := yUR;
              yUR := ytemp;
           end if;
          if (Y < yLL or Y > yUR) OR (X < xLL or X > xUR) then
            NULL;
          else
            seg := TRUNC((i-1)/2);
            RETURN seg;
          end if;
          END IF;
         End Loop;
       end if;
      End Loop;

   ELSE


      Distances.extend(m);
      For k in 1..m Loop
        kk := k*6;
        xLL := polyMBR(kk+1);
        yLL := polyMBR(kk+2);
        xUR := polyMBR(kk+3);
        yUR := polyMBR(kk+4);
-- Find closest corner to the point
        Dist := ROUND(fast_distance(xLL,yLL,xtest,ytest,SRID),3);
        Dist2 := ROUND(fast_distance(xLL,yUR,xtest,ytest,SRID),3);
        if Dist2 < Dist then
          Dist := Dist2;
        end if;
        Dist2 := ROUND(fast_distance(xUR,yUR,xtest,ytest,SRID),3);
        if Dist2 < Dist then
          Dist := Dist2;
        end if;
        Dist2 := ROUND(fast_distance(xUR,yLL,xtest,ytest,SRID),3);
        if Dist2 < Dist then
          Dist := Dist2;
        end if;
        Distances(k) := Dist + k * rmult;  -- encode k in the distances
      end loop;                            -- so we sort only 1 array but know k

      shellsort(Distances);
      mloops := 4;
      if Distances.count >= 3 then
        if Distances.count = 3 then
        mloops := 3;
        end if;
        check_distance := Distances(3);
      elsif Distances.count >= 2 then
      mloops := 2;
        check_distance := Distances(2);
      else
        mloops := 1;
        check_distance := Distances(1);
      end if;

         For k in 1..mloops Loop
           kk  := Distances(k)*mult - TRUNC(Distances(k)*mult/1000.)*1000.;
--             dbms_output.put_line('kk is ' || (kk*6) || ' k ' || k  || ' d ' || distances(k) || ' m ' || m); --pmbr count ' || polyMBR.count);
        kk := kk*6;

--  Check to locate the point within the range of this MBR

         istart := polyMBR(kk+5);
         if istart+2 > iend then
            istart := istart-2;
         end if;
         iend := polyMBR(kk+6);
         ibehind := istart-2;
         if ibehind < 1 then
           ibehind := Xys.count-3;
         end if;
         x1 := Xys(ibehind);
         y1 := Xys(ibehind+1);
         x2 := XYs(istart);
         y2 := XYs(istart+1);
--    dbms_output.put_line('start is ' || istart || ' iend ' || iend || ' kk ' || kk || ' distances ' || distances(k));
         For i in istart+2..iend Loop
           IF MOD(i,2) = 1 THEN
            xb := x1;
            yb := y1;
            x1 := x2;
            y1 := y2;

            x2 := Xys(i);
            y2 := Xys(i+1);

-- Work out perpendicular distance from point to line -> (xtest,ytest) to the
-- near point (xnear,ynear).
-- If Point does not project onto line then measure distance from (xtest,ytest) to (x1,y1)

            dist := Perpendicular(xtest,ytest,x1,y1,x2,y2,xnear,ynear,FALSE, meters);
            got_perpendicular := TRUE;
            if dist = big then
              Dist := fast_distance(x1,y1,xtest,ytest,SRID);
              got_perpendicular := FALSE;
--            if dist < 0.002 then
--              if i >=63 and i <=79 then
--            dbms_output.put_line('Perpend dist ' || round(dist,6) || ' seg ' || trunc((i-1)/2) ||' xnear ' || round(x1,7) || ',' || round(y1,7)  || ' xt ' || round(xtest,7) || ',' || round(ytest,7));
--            end if;
            end if;

--dbms_output.put_line('dist ' || round(dist,6) || ' I ' || i || ' seg ' || trunc((i-1)/2) || ' last  ' || round(last_dist,3));
            if dist <= last_dist then

-- We need to ensure we get the segment on the subject edge thatis most parallel to
-- the segment on the loop.

             if xb is not NULL then
             angleb := ABS(90.-GZ_QA.angle(xtest,ytest,x1,y1,xb,yb)*rad2deg);
             end if;
             angle := ABS(90.-GZ_QA.angle(xtest,ytest,x1,y1,x2,y2)*rad2deg);
--             dbms_output.put_line('angleb ' || round(angleb,6) || ' angle ' || round(angle,6) || ' seg ' || seg || ' ii ' || i);
             if got_perpendicular = FALSE and angleb < angle and i > 3 then
               seg := TRUNC((i-1)/2) -1;
             else
               seg := TRUNC((i-1)/2);
             end if;
               last_dist := dist;
--               if seg = 23 then
--                   dbms_output.put_line('angleb ' || round(angleb,6) || ' angle ' || round(angle,6) || ' seg ' || seg || ' ii ' || i);
--               dbms_output.put_line('dist ' || round(dist,6) || ' seg ' || seg || ' last  ' || round(last_dist,6));
--               dbms_output.put_line('xtest ' || xtest || ' ytest ' || ytest);
--               dbms_output.put_line('x1 ' || round(x1,6) || ' y1 ' || round(y1,6));
--               dbms_output.put_line('x2 ' || round(x2,6) || ' y2 ' || round(y2,6));
--               end if;
          end if;
          END IF;
         End Loop;
         exit when dist <> big and distances(k) > 2.* check_distance; --last_dist;

      End Loop;
      distance_found := last_dist;
--      dbms_output.put_line('returning seg ' || seg);
      RETURN seg;
      END IF;
  RETURN 0;

END FIND_SEGMENT;
--
FUNCTION Find_Distances(x IN OUT NOCOPY NUMBER,y IN OUT NOCOPY NUMBER,XYS IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,short_distance  IN OUT NOCOPY NUMBER,exclude BOOLEAN default FALSE,pstart PLS_INTEGER default 1,pnvert_outer PLS_INTEGER default 0,forwards BOOLEAN default TRUE) RETURN NUMBER IS
/*
**************************************************************************************
--Program Name: Find_distances
--Author: Sidey Timmins
--Creation Date: 08/10/2009
--Usage:
  -- Call this function from inside another PL/SQL program.  There are 5 parameters:
  --
  --   REQUIRED Parameters:
  --            x,y:  x,y coordinates of the reference point
  --            Xys: coordinates of the lake geometry to measure to
  --            exclude: When exclude is TRUE, exclude the first and last point
  --               since they are the entrance and exit of the art path.
  --            pstart: vertex to start in XY (defaults to 1)
  --            nvert_outer: vertex to end or the # vertices in the outer ring
  --
--Purpose:      Finds distances and their order (from close to far) from a
--              reference point x,y to each vertex of (another) geometries Xys.
--              Returns the vertices ordered by increasing distance.
-- Method:      Determines the distance to each vertex and then sorts.
-- Dependencies: distance_fcn
***************************************************************************************
*/
   Vertex_order   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   GC_distances   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   big            NUMBER := 1E10;
   xtest          NUMBER;
   ytest          NUMBER;
   istart         PLS_INTEGER := pstart*2-1;
   n              PLS_INTEGER := pnvert_outer;
   n2             PLS_INTEGER;
   jj             PLS_INTEGER;
   ii             PLS_INTEGER;
BEGIN

    if n = 0 then
      n := TRUNC(Xys.count/2);
    end if;
    n2 := n*2-1;
    Vertex_Order.extend(n-pstart+1);
    gc_distances.trim(gc_distances.count);
    GC_distances.extend(n-pstart+1);
    if Vertex_order.count = n then
      Vertex_order(1) := 1;   -- just to exclude 1st and last vertex
      Vertex_order(n) := n;
      gc_distances(1) := big;
      gc_distances(n) := big;
    end if;

    ii := istart-2;
    While ii <> n2 loop
      ii := ii+2;
      If exclude and (ii = 1 or ii = (Xys.count-1)) then
         NULL;
      Else
         xtest := Xys(ii);
         ytest := Xys(ii+1);
         jj := TRUNC((ii+1)/2) -pstart + 1;
-- Use this empirical function to get a very accurate approximation to
-- the great circle distance.
         gc_distances(jj) := fast_distance(x,y,xtest,ytest);
         Vertex_order(jj) := jj -1 + pstart;
      End if;
    End Loop;

-- Now sort from near to far
    Stablesort2(gc_distances,Vertex_order,1,Vertex_order.count,forwards);
--     if vertex_order.count < 100 then
    for ii in 1..1 loop --Vertex_order.count loop
        dbms_output.put_line('ii ' || ii || ' vertex order ' || vertex_order(ii) || ' d ' || round(gc_distances(ii),8));
      end loop;
--    end if;
    short_distance := GC_distances(1);
    Return Vertex_order(1);

END Find_Distances;
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
--Updated:  12/01/2010 To be more efficient when the order_Array has duplicates.
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
PROCEDURE GET_SCALE(threshold IN OUT NOCOPY NUMBER,scale IN OUT NOCOPY NUMBER,target_scale NUMBER,Length NUMBER) AS

iscale        PLS_INTEGER;
iround       PLS_INTEGER;
BEGIN
-- Keep reducing the scale, starting with 10% and then getting a larger percentage
-- The sequence of scales are 4.5M,4M,3.5M,3M,2.5M,2M,1.5M,1M,500K,250K,100K,50K,...none!

               if threshold > 1000. then
                 threshold := threshold - 100.;
               elsif threshold > 500. then
                 threshold := threshold - 50.;
               else
                 threshold := threshold - 25.;
               end if;
               if threshold < 1. then
                 threshold := 1.;
               end if;

-- Above 10 million we go down by 2M: so 20M,18M,16M,14M,12M,10M,8M,6M,4M,2M
               if target_scale > 15000000. and scale >= 4000000. then
                  scale := scale - 2000000.;
-- At and above 10 million we go down by 1M for target scale of say 10M
               elsif target_scale > 5000000. and target_scale <= 15000000. and scale > 1000000. then
                  scale := scale - 1000000.;
-- At and above million we go down by 500K: so 5M,4.5M,4M,3.5M,3M,2.5M,2M,1.5M,1M
               elsif target_scale > 1000000. and target_scale <= 10000000. and scale >= 1000000. then
                  scale := scale - 500000.;
-- At and above 1 million we go down by 100K for target scale of say 10M
               elsif target_scale > 500000. and target_scale <= 1000000. and scale > 100000. then
                  scale := scale - 100000.;
-- At and above 500K we go down by 50K: so 500K,450K,400K,350K,300K,250K,200K,150K,100K
               elsif target_scale <= 1500000. and scale >= 100000. then
                  scale := scale - 50000.;
-- Below a million we halve more or less: 500K,250K,100K,50K,25K,10K,5K,2.5K,1K,500,250,125
               elsif scale <= 1000000. or (target_scale >= 10000000. and scale <= 4000000.) then
                 scale := TRUNC(scale / 2.);
                 if scale < 1. then
                    scale := 1.;
                 end if;
                 if scale = 125000. then
                   scale := 100000.;
                 end if;
                 if scale = 12500. then
                   scale := 10000.;
                 end if;
                 if scale = 1250. then
                   scale := 1000.;
                 end if;
              end if;
/*
 iscale := (Length/(1.+ Length/scale));
 if scale >= 5000000. then
   iround := 500000;
 elsif scale > 500000. then
   iround := 100000;
 else
    iround := 10000;
 end if;
 if iscale >= iround then
   scale := TRUNC(iscale/iround)*iround;
 end if;
 scale := iscale;
*/
END GET_SCALE;
--
FUNCTION GET_LOOP_XYS(Edge_xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Edges_Table VARCHAR2,ID_LIST IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN MDSYS.SDO_ORDINATE_ARRAY AS

   n          PLS_INTEGER := ID_LIST.count;
   start_xy   PLS_INTEGER := 3;
   direction  PLS_INTEGER;
   sql_stmt   VARCHAR2(4000);
   geom  MDSYS.SDO_GEOMETRY;
   xys        MDSYS.SDO_ORDINATE_ARRAY;
   loop_xys   MDSYS.SDO_ORDINATE_ARRAY := Edge_xys;


   procedure concatenate_xys(loup_xys in out nocopy mdsys.sdo_ordinate_array, next_xys in out nocopy mdsys.sdo_ordinate_array, way number) as

    m         pls_integer := loup_xys.count;
    begin
         loup_xys.extend(next_xys.count+1-start_xy);
         if way < 0 then
            gz_util_zone.reverse_ordinates(next_xys);
         end if;
         for i in start_xy..next_xys.count loop
            loup_xys(m+i+1-start_xy) := next_xys(i);
--            dbms_output.put_line('next_xys ' || round(next_xys(i),6));
         end loop;
    end;

BEGIN
--          for i in 1..loop_xys.count loop
--            dbms_output.put_line('loop_xys ' || round(loop_xys(i),6));
--         end loop;
--   dbms_output.put_line('id: ' || id_list(1) || ' n ' || n);
   for ii in 2..n Loop

-- Get either the old or new geometry for the current edge so we have a polygon
-- built with the desired edge. A is to the right of edge B before generalization.
-- After A moves to position C and is now to the left. The area of the loop AB
-- is positive whereas the area of CB is not

--          +______
--          | \    |
--          |  |   |
--    C=new |  | B |  A = old edge
--          |  |   |
--          |  /   |
--          +______


     sql_stmt := 'SELECT NEW_GEOMETRY FROM ' ||EDGES_TABLE ||' WHERE EDGE_ID=:1';
--     dbms_output.put_line( 'selecting ' ||id_list(ii));
     EXECUTE IMMEDIATE sql_stmt into geom using ABS(id_list(ii));
-- dbms_output.put_line( id_list(ii));
       if id_list(ii) > 0 then
          direction := 1;
       else
          direction :=-1;
       end if;
       xys := geom.sdo_ordinates;
--       dbms_output.put_line('concat');
       concatenate_xys(loop_xys,xys,direction);
--       dbms_output.put_line('back from concat');
   end loop;
--  dbms_output.put_line('ready to return');
   Return loop_xys;
END GET_LOOP_XYS;
--
FUNCTION IS_CONNECTED(edge_id NUMBER,start_node NUMBER,end_node NUMBER,
                       nearby_edges MDSYS.SDO_LIST_TYPE,
                       Edges_Table VARCHAR2,ID_LIST IN OUT NOCOPY MDSYS.SDO_LIST_TYPE ) RETURN VARCHAR2 AS


-- simple function to "discover" continuity from a small set of edges as in
--   2,3,4 connect start_node to target
--
--         1   2           3         4    5
--    +-------+---+---------------+----+------+
--          start                       target


-- Returns an id_list of the edge_ids that connect, negative meaning the
-- order is backwards thru the edge.

 Connected    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
 Edge_ids     MDSYS.SDO_LIST_TYPE;
 Starts       MDSYS.SDO_LIST_TYPE;
 Ends         MDSYS.SDO_LIST_TYPE;
 sql_stmt     VARCHAR2(4000);
 nearby_list  VARCHAR2(4000) :='(';
 loop_counter PLS_INTEGER :=0;
 next         PLS_INTEGER :=1;
 current_node NUMBER := start_node;
 last_node    NUMBER :=0;
 last2_node   NUMBER :=0;
 temp         NUMBER;
 new          NUMBER := start_node;
 target       NUMBER := end_node;

 n           PLS_INTEGER := nearby_edges.count;

      function connect_it(node number,last number) return number as

-- simple function to "discover" continuity of one edge from a small set of edges
-- as in 3 connects from node c of edge 2 and sets the current node to d.
-- Note how each edge can only be used once so we can keep track with an is
-- connected (already) array.
--
--         1   2           3         4    5
--    +-------+---+---------------+----+------+
--    a       b   c               d    e      f     nodes
--          node                       target
         new   number;

      begin
         new := node;
         for ii in 1..n loop
            if connected(ii) = 0 then
            -- we have to avoid going back to where we just were so
            -- we would end up with a 2 edge loop. Usually this is not desired.
              if starts(ii) = new and ends(ii) <> last then
                 new := ends(ii);
                 next := next +1;
                 id_list(next) := edge_ids(ii);
                 connected(ii) :=1;
--                 dbms_output.put_line('.......connected edge:'||edge_ids(ii) ||' from node ' || node ||' to node '|| new);
                 return new;
              elsif ends(ii) = new and starts(ii) <> last then
                 new := starts(ii);
                 next := next +1;
                 id_list(next) := -edge_ids(ii);
                 connected(ii) :=1;
--                 dbms_output.put_line('.......connected edge:'||edge_ids(ii) ||' from node ' || node ||' to node '|| new);
                 return new;
            end if;
            end if;
         end loop;
--           dbms_output.put_line('this route failed');
         return 0;
      end;
BEGIN

    id_list.extend(n);
    id_list(1) := edge_id;
--    dbms_output.put_line('START ' || edge_id);
    for ii in 1..n loop
       if ii <> n then
         nearby_list := nearby_list || nearby_edges(ii) ||',';
       else
         nearby_list := nearby_list || nearby_edges(ii) ||')';
       end if;
    end loop;
--    dbms_output.put_line('list ' || nearby_list);
     sql_stmt := 'SELECT EDGE_ID,START_NODE_ID,END_NODE_ID FROM ' ||EDGES_TABLE ||' WHERE EDGE_ID in '|| nearby_list;

     EXECUTE IMMEDIATE sql_stmt BULK COLLECT into edge_ids,starts,ends;
     Connected.extend(n);
--     dbms_output.put_line('Pool of edges');
--     dbms_output.put_line('edge_id start end');
     for ii in 1..n loop
--     dbms_output.put_line(edge_ids(ii) || ' ' || starts(ii) || ' ' || ends(ii));
        Connected(ii) := 0;
     end loop;
     last_node :=0;
-- Here we go from start to end node
     WHILE new <> target and loop_counter < n LOOP
        loop_counter := loop_counter +1;
        current_node := start_node;
        new := current_node;
        next :=1;
--        dbms_output.put_line('...');
--        dbms_output.put_line('>>Starting with start node ' || new );

        while new <> 0 and new <> target loop
        last2_node := last_node;
        last_node := current_node;
          new := connect_it(current_node,last2_node);
          current_node := new;
        end loop;
     END LOOP;

     if new = target then
-- we went backwards thru the first edge, so change order to appear we went forwards
     id_list.trim(id_list.count-next);
     for i in 2..TRUNC(next/2)+1 loop
       temp := id_list(i);
       id_list(i) := -id_list(next+2-i);
       id_list(next+2-i) := -temp;
     end loop;
--     dbms_output.put_line('>>>Found target ' || new);
       RETURN 'TRUE';
     end if;

-- Here we try from end to start node
    for ii in 1..n loop
        Connected(ii) := 0;
     end loop;
     new := end_node;
     target := start_node;
     last_node :=0;
     next :=1;
--     dbms_output.put_line('...');
--     dbms_output.put_line('>>Starting with end node ' || new);
     WHILE new <> target and new <> target and loop_counter < n LOOP
        loop_counter := loop_counter +1;
        current_node := end_node;
        new := current_node;
        while new <> 0 loop
        last2_node := last_node;
        last_node := current_node;
          new := connect_it(current_node,last2_node);
          current_node := new;
        end loop;
     END LOOP;

     if new = target then
       id_list.trim(id_list.count-next);
--     dbms_output.put_line('>>>Found target ' || new);
       RETURN 'TRUE';
     end if;
       RETURN 'FALSE';
END;
--
FUNCTION GET_SPLIT_VERTICES(edge_id NUMBER,start_node NUMBER,end_node NUMBER,
                            new_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                            geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                            nearby_edges MDSYS.SDO_LIST_TYPE,
                            Edges_Table VARCHAR2 default NULL,
                            part_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,whole BOOLEAN default FALSE) RETURN MDSYS.SDO_LIST_TYPE AS

/*
********************************************************************************
--Program Name: Get_Split_Vertices
--Author: Sidey Timmins
--Creation Date: 05/26/2010

--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 7 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --       edge_id      - id of the edge
  --       start_node   - start node from MT_EDGE$ for the edge_id
  --       end_node     - end node from MT_EDGE$ for the edge_id
  --       new_geometry - the current geometry
  --       geometry     - the original geometry
  --       nearby_edge  - the edges nearby that intersect edge_id
  --       Edges_Table  - the Edges_toprocess table

-- Purpose: Each generalized edge may intersect other edges. if so, this function
--          makes a list of places where to split the edge (quadraplets):
--             1.  bad generalized segment #,
--             2. orig start segment, (starting at the same vertex as the generalized segment)
--             3. original end segment (starting at the same vertex as the generalized segment ended)
--             4. nearby edge id that this edge hits

-- Dependencies: Find_intersection_segment, Check_matching_Xys,
--               Find_matching_xy
********************************************************************************
*/

    nearby_geometry  MDSYS.SDO_GEOMETRY;
    intersections    MDSYS.SDO_GEOMETRY;

    XYOrd            MDSYS.SDO_ORDINATE_Array;
    newXYOrd         MDSYS.SDO_ORDINATE_Array;
    old_xys          MDSYS.SDO_ORDINATE_Array;
    new_xys          MDSYS.SDO_ORDINATE_Array;
    Int_XYOrd        MDSYS.SDO_ORDINATE_Array;
    nearbyOrd        MDSYS.SDO_ORDINATE_Array;
    ISegments        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Segments         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Seg_copy         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Seg_ids          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    id_list          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    area1            NUMBER;
    area2            NUMBER;
    seg_low          NUMBER;
    seg_hi           NUMBER;
    nearby_edge_id   NUMBER;
    vertex           NUMBER := 0.;
    x                NUMBER;
    y                NUMBER;
    xstart           NUMBER;
    ystart           NUMBER;
    xend             NUMBER;
    yend             NUMBER;
    xc               NUMBER;
    yc               NUMBER;
    place            PLS_INTEGER;
    place2           PLS_INTEGER;
    sql_stmt         VARCHAR2(4000);
    temp             NUMBER;
    small            NUMBER := 0.0000001;
    recip            NUMBER;
    matching         PLS_INTEGER;
    tested           PLS_INTEGER;
    low_segment      PLS_INTEGER;
    final_segment    PLS_INTEGER;
    vertex_count     PLS_INTEGER;
    next             PLS_INTEGER;
    vcount           PLS_INTEGER;
    n                PLS_INTEGER;
    m                PLS_INTEGER;
    seg_pos          PLS_INTEGER;
    loops            PLS_INTEGER;
    seg_count        PLS_INTEGER := 0;
    found            BOOLEAN;
    is_a_connection  VARCHAR2(5) := 'FALSE';

BEGIN

   recip := 1./small;
   XyOrd := geometry.sdo_ordinates;
   m := XYOrd.count;
   n := TRUNC(XYOrd.count/2);
   newXYOrd := new_geometry.sdo_ordinates;
   vertex_count := TRUNC(newXYord.count/2);

   Segments.extend(12);

--   dbms_output.put_line('NEW ' || newxyord(1) || ' Y  ' || newxyord(2));
--   dbms_output.put_line('new ' || newxyord(3) || ' Y  ' || newxyord(4));
-- Check the count. If different then the lines may be different - depending
-- on how many consecutive vertices are the same?

    matching := Check_matching_Xys(tested,newXYOrd,XyOrd);



-- if ii < 1000 then
--    dbms_output.put_Line('id ' || edge_id || ' match ' || matching || ' tested ' || tested);
--  end if;

-- If there are any nearby edges check to see if there are any intersections

    if nearby_edges.count > 0 and matching <> tested then

-- We use the new_geometry as the state variable for the spatial data
       sql_stmt := 'Select new_geometry from ' || Edges_Table|| ' where edge_id=:1';

-- For each nearby edge, get its current geometry and test the new generalized
-- geometry to see it it intersects anywhere except on the ends (nodes).

       For jj in 1..nearby_edges.count loop
       -- First one is the closest
          nearby_edge_id := nearby_edges(jj);


            Execute immediate sql_stmt into nearby_geometry using nearby_edge_id;

-- dbms_output.put_line(Edges_Table||'checking ' || nearby_edge_id || ' nearby count ' || nearby_geometry.sdo_ordinates.count || ' new count ' || new_geometry.sdo_ordinates.count);

               NearbyOrd := nearby_geometry.sdo_ordinates;
               xstart  := NearbyOrd(1);
               ystart := NearbyOrd(2);
               xend   := NearbyOrd(NearbyOrd.count-1);
               yend := NearbyOrd(NearbyOrd.count);

               ISegments := FIND_INTERSECTION_SEGMENT(new_geometry,nearby_geometry);
--dbms_output.put_line(nearby_edge_id||' has INTERSECTIONS ' || isegments.count);
-- Check to see if the segment stays on the same side of a nearby closed loop OR
-- if and only if the segment is the whole geometry we check to to avoid a
-- nodestar situation when the generalized is a straight  segment and it crosses
-- over another edge with the same starting and ending nodes.

               if isegments.count = 0 and -- ((is_a_connection='TRUE') or
                 ((xstart = xend and ystart = yend) and
                 (xstart <> XYord(1) or ystart <> XYord(2)) and
                 (xstart <> XYord(m-1) or ystart <> XYord(m))) then
--               dbms_output.put_line('Isegments count ' || Isegments.count);
--               if Isegments.count=0 and jj =1 and xstart = xend and ystart = yend and
--               if Isegments.count=0 and xstart = xend and ystart = yend and
--                 (xstart <> XYord(1) or ystart <> XYord(2)) and
--                 (xstart <> XYord(m-1) or ystart <> XYord(m)) then
--                dbms_output.put_line('CCCalling polyLR with ' || part_geometry.sdo_ordinates.count || ' new ' || new_geometry.sdo_ordinates.count);

                 ISegments := CHECK_POLYLR(part_geometry,nearby_geometry,new_geometry);
--                 dbms_output.put_line(nearby_edge_id||' has Isegments count after check polyLR ' || Isegments.count || ' nearby ' || nearby_edge_id);
               end if;
--          dbms_output.put_line('Isegments count ' || Isegments.count);

          If Isegments.count <> 0 then
--            dbms_output.put_line('ID intersects ' || nearby_edge_id || ' at ' || ISegments(1) || ' isegments count ' || Isegments.count);

            For ii in 1..Isegments.count Loop
              seg_pos := seg_count;
              found := FALSE;
              For ij in 1..seg_count Loop
                 if Mod(ij,6) = 1 and Isegments(ii) = Segments(ij) then
                   seg_pos := ij-1;
                   found := TRUE;
                   exit;
                 end if;
              End Loop;
            seg_pos := seg_pos+1;
            if seg_pos > Segments.count then
              Segments.extend(12);
            end if;

-- Make a list of sextuplets: bad generalized segment #,
--                          orig start segment, original end segment
--                          nearby edge id that is the problem,
--                          zero,zero
            Segments(seg_pos) := Isegments(ii);
            seg_low := ISegments(ii);
--            dbms_output.put_line('seg low ' || seg_low || ' count ' ||vertex_count);
            x := newXYord(seg_low*2-1);
            y := newXYord(seg_low*2);
            low_segment := Find_matching_xy(x,y,XYord);
--            dbms_output.put_line('x ' || x || ' y ' || y);
--            dbms_output.put_line('low_segment ' || low_segment);
            if (found = TRUE and Segments(seg_pos+1) > low_segment) or found = FALSE then
              Segments(seg_pos+1) := low_segment;
--              dbms_output.put_line('SET low_segment ' || low_segment);
            end if;
            seg_hi := seg_low+1;
--            dbms_output.put_line('seg hi ' || seg_hi  || 'vc ' || vertex_count);
            if seg_hi > vertex_count then
              seg_hi := vertex_count;
            end if;
--            dbms_output.put_line('seg hi ' || seg_hi || ' count ' ||vertex_count);
            x := newXYord(seg_hi*2-1);
            y := newXYord(seg_hi*2);

            final_segment := Find_matching_xy(x,y,XYord);

            if final_segment <= low_segment then
              final_segment := Find_matching_xy(x,y,XYord,1.E-6,low_segment+1);
            end if;

            if XYOrd(1) = XYOrd(XYOrd.count-1) and XYOrd(2) = XYOrd(XYOrd.count) and
               x = XYord(1) and y = XYOrd(2) then
               final_segment := TRUNC(XYOrd.count/2);
            end if;
--            dbms_output.put_line('Final ' || final_segment);
            if final_segment <> 0 then
            if (found = TRUE and Segments(seg_pos+2) < final_segment) or found = FALSE then
            Segments(seg_pos+2) := final_segment;
--            dbms_output.put_line('FFinal ' || final_segment|| ' ' || seg_count || ' nearby ' || nearby_edge_id);
            end if;
--            dbms_output.put_line('storing ' || nearby_edge_id);
            if Segments(seg_pos+3) is NOT NULL and xstart= xend and ystart=yend then
              NULL;
            else
            Segments(seg_pos+3) := nearby_edge_id;
            end if;
            Segments(seg_pos+4) := 0;
            Segments(seg_pos+5) := 0;
            if found = TRUE then
               NULL;
            else
              seg_count := seg_count+6;
            end if;
            end if;
            End Loop;
          end if;
      End Loop;
    END IF;

-- Check nodestar is good.
-- A is to the right of edge B before generalization.
-- After A moves to poition C and is now to the left. The area of the loop AB
-- is positive whereas the area of CB is not

--          +______
--          | \    |
--          |  |   |
--    C=new |  | B |  A = old edge
--          |  |   |
--          |  /   |
--          +______

    if whole and seg_count=0 and vertex_count=2 then
          is_a_connection := Is_Connected(edge_id,start_node,end_node,nearby_edges,Edges_Table,id_list);
--          dbms_output.put_line('for id : ' || edge_id || ' is a connection ' || is_a_connection);


          if is_a_connection='TRUE' then
            new_xys := get_loop_xys(newXYOrd,Edges_table,id_list);
            old_xys := get_loop_xys(XYOrd,Edges_table,id_list);
            area2 := centroid(new_xys,xc,yc,8265.);
            area1 := centroid(old_xys,xc,yc,8265.);
  --          dbms_output.put_line('Area1 ' || round(area1,6) || ' area2 ' || round(area2,6));
            if area2*area1 <= 0. then
               Segments := MDSYS.SDO_LIST_TYPE(1,1,TRUNC(old_xys.count/2),ABS(id_list(2)));
               RETURN Segments;
            end if;
          end if;
      end if;

--           for ij in 1..seg_count loop
--    dbms_output.put_line('list was ' || segments(ij));
--    end loop;
    Segments.trim(Segments.count-seg_count);
--
    if seg_count > 6 then
-- We have to have the segment list sorted for Assemble_edge
       Seg_copy := Segments;
       Seg_ids.extend(segments.count/6);
       For ii in 1..Seg_ids.count Loop
         Seg_ids(ii) := Segments((ii-1)*6+1) + ii*small;
       End Loop;
-- sort them
       shellsort(Seg_ids);
--        dbms_output.put_line('sorted seg_ids');
       For ii in 1..Seg_ids.count Loop
         place := ((Seg_ids(ii) - TRUNC(Seg_ids(ii)))*recip -1.) *6.;
         place2 := (ii-1)*6;
         Segments(place2+1) := Seg_copy(place+1);
         Segments(place2+2) := Seg_copy(place+2);
         Segments(place2+3) := Seg_copy(place+3);
         Segments(place2+4) := Seg_copy(place+4);
       End Loop;

-- Avoid a gross mistake.
     end if;
--   for ij in 1..segments.count loop
--    dbms_output.put_line('LIST ' || segments(ij));
--    end loop;
    RETURN Segments;

END GET_SPLIT_VERTICES;
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
                              Yi IN OUT NOCOPY NUMBER,
                              det IN OUT NOCOPY NUMBER,debugit boolean default false)
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
--  http://local.wasp.uwa.edu.au/~pbourke/geometry/pointline/
-- Dependencies: none
********************************************************************************
*/
   s    NUMBER;
   t    NUMBER;

   length_2 NUMBER;
   d        NUMBER;
   xcheck   NUMBER;
   ycheck   NUMBER;
BEGIN
-- Check parametric equations for two lines: s is on line 1
--                                           t is on line 2

   det := (X4 - X3) * (Y2 - Y1)  - (X2 - X1) * (Y4 - Y3) ;


   IF det <> 0.0 THEN
      s := ((X4 - X3) * (Y3 - Y1) - (Y4 - Y3) * (X3 - X1)) /det;
      t := ((X2 - X1) * (Y3 - Y1) - (Y2 - Y1) * (X3 - X1)) /det;

--      if debugit = TRUE then
--      dbms_output.put_line(' SS ' || s || ' t ' || t);
--      end if;
      If s >= 0.0 and s <= 1.0 and t >= 0.0 and t <= 1.0 then

        xi := X1 + s * (X2 - X1);
        yi := Y1 + s * (Y2 - Y1);

        RETURN s;

/*
      Elsif (s >= 0.0 and s <= 1.0) or (t >= 0.0 and t <= 1.0) then
        xi := X1 + s * (X2 - X1);
        yi := Y1 + s * (Y2 - Y1);

-- pretend near glances intersect
        if y1 > 50. then  -- used tolerance = 2 meters hers
          xcheck   := 0.000064; --0.0000016 * tolerance/0.05;
          ycheck   := 0.00002; --0.0000005 * tolerance/0.05;
        else
          xcheck   := 0.00003; --0.00000075 * tolerance/0.05;
          ycheck   := 0.000026; --0.00000065 * tolerance/0.05;
        End If;
        if abs(x3-xi) < xcheck and abs(y3-yi) < ycheck then
          d := fast_distance(xi,yi,x3,y3,8265);
          if d < 2.0 then
           RETURN 0.5;
          end if;
        elsif abs(x4-xi) < xcheck and abs(y4-yi) < ycheck then
          d := fast_distance(xi,yi,x4,y4,8265);
          if d < 2.0 then
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


   END IF;
   if x1 = x3 and y1 = y3 and x2 = x4 and y2 = y4 then
       RETURN -3.;
   elsif x1 = x4 and y1 = y4 and x2 = x3 and y2 = y3 then
       RETURN -4.;
   end if;
-- Lines are parallel (but not coincident)
   RETURN -2.;

END LINE_INTERSECT;
--
FUNCTION PROCESS_SELF_SEGS(self_segs IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,origXYORD IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,XYORD IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY) RETURN MDSYS.SDO_LIST_TYPE
AS

   isegments         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   seg1              PLS_INTEGER;
   seg2              PLS_INTEGER;
   last_seg2         PLS_INTEGER :=0;
   last_seg1         PLS_INTEGER :=0;
   j                 PLS_INTEGER;
   low_segment       PLS_INTEGER;
   hi_segment        PLS_INTEGER;
   seg_found         PLS_INTEGER;
   temp              PLS_INTEGER;
   x                 NUMBER;
   y                 NUMBER;
   thousand          NUMBER := 100000.;
BEGIN


  if self_segs.count > 0  then
-- We need to process the segments in order from low to high along the edge
    shellsort(self_segs);
--  dbms_output.put_line ('after self intersect check' || self_segs.count);
    j := 0;
    for kk in 1..self_segs.count loop
        last_seg2 := seg2;
        last_seg1 := seg1;
        seg2 := MOD(self_segs(kk),thousand);
        seg1 := TRUNC((self_segs(kk)-seg2)/thousand);
        if kk > 1 and seg2 = last_seg2 and seg1 = last_seg1 then
          self_segs(kk) := 0;
--          NULL;
        else
          j := j + 1;
        end if;
--        dbms_output.put_line ('self ' || self_segs(kk));
    end loop;
    isegments := MDSYS.SDO_LIST_TYPE();
    isegments.extend(6*j);


    j := -6;
    for kk in 1..self_segs.count loop
        if self_segs(kk) <> 0 then
           seg2 := MOD(self_segs(kk),thousand);
           seg1 := TRUNC((self_segs(kk)-seg2)/thousand);

--         dbms_output.put_line ('>>>>>>>>seg1 ' || seg1 || ' seg2 ' || seg2);

-- keep each segment once
           if kk = 1 or (kk> 1 and isegments(j+1) <> seg1)  then

-- Self intersections can be very complicated
-- Find lowest segment (8) that intersects the very lowest segment 3
--              /  \
--         9  /      \   8
--      ---- /--------\----
--              3
               j := j+6;
               isegments(j+4) := seg2;
               x := Xyord(seg2*2-1);
               y := Xyord(seg2*2);
               seg_found := Find_matching_xy(x,y,origXYord);
               isegments(j+5) := seg_found;
-- Because self intersection is complicated
               x := Xyord(seg2*2+1);
               y := Xyord(seg2*2+2);
               seg_found := Find_matching_xy(x,y,origXYord);
               isegments(j+6) := seg_found;
--                       dbms_output.put_line ('>>>>nowseg1 ' || seg1 || ' seg2 ' || seg2 || ' Low ' || isegments(7) || ' Hi ' || isegments(8));
/*
               if seg2 = TRUNC(XYord.count/2)-1 then
                  seg1 := seg2;
                  seg2 := seg2 + 1;
               elsif seg1 = TRUNC(XYord.count/2)-1 then
                  seg2 := seg1+1;
--                  elsif seg1 > seg2 then
--                      seg2 := seg1+1;
--                       dbms_output.put_line ('made seg2 1 larger!! ' || seg2);
--                      seg1 := MOD(self_segs(kk),thousand);
               end if;
*/
               x := Xyord(seg1*2-1);
               y := Xyord(seg1*2);

               low_segment := Find_matching_xy(x,y,origXYord);
               x := Xyord(seg1*2+1);
               y := Xyord(seg1*2+2);

               hi_segment := Find_matching_xy(x,y,origXYord);
               if hi_segment = 1 then
                  hi_segment := TRUNC(origXYOrd.count/2);
               end if;
               if hi_segment <= low_segment then
                  hi_segment := Find_matching_xy(x,y,origXYord,1.E-6,low_segment+1);
               end if;
--                  dbms_output.put_line ('>>>>>>>>seg1 ' || seg1 || ' seg2 ' || seg2 || ' low ' || low_segment || ' hi ' || hi_segment);

               isegments(j+1) := seg1;    -- the generalized segment that intersects itself

--      Each section (generalized segment) has a corresponding start vertex
--      and end vertex in the ungeneralized data. This vertex (or segment since
-- you wll note segment 1 starts at vertex 1 and ends at vertex 2,
--                 "    2    "   "     "   2 "    "   "    "    3,  and so on)
--
--                          original ungeneralized segments
--                          ____________
--                         /             \
--      low segment       /                \   hi segment
--               lo_v   +------------------+ hi_v
--                        generalized segment
--
               isegments(j+2) := low_segment;     -- the low ungeneralized segment
               isegments(j+3) := hi_segment;        -- the high generalized segment
            end if;

          end if;
        end loop;
        isegments.trim(isegments.count-(j+6));
--             dbms_output.put_line ('iseg1 '|| (isegments(1)) || ' iseg2 ' || (isegments(2)) || ' iseg3 ' || (isegments(3)) || ' iseg4 ' || (isegments(4)) || ' iseg5 ' || (isegments(5)));
--             dbms_output.put_line ('Iseg6 '|| (isegments(6)) || ' iseg2 ' || (isegments(7)) || ' iseg3 ' || (isegments(8)) || ' iseg4 ' || (isegments(9)) || ' iseg5 ' || (isegments(10)));

    end if;
    RETURN isegments;

END PROCESS_SELF_SEGS;
--
PROCEDURE RUN_MAKE_EDGE_TABLE(Topology VARCHAR2 default 'MT',run_flag VARCHAR2 default NULL,pedge_table VARCHAR2 default 'EDGE',pface VARCHAR2 default 'FACE',pedges_toprocesstable VARCHAR2 default 'EDGES_TOPROCESS',pState_Edge_Table VARCHAR2 default 'STATE_EDGE_TABLE',pEntityfp VARCHAR2 default 'STATEFP') AS

  --   REQUIRED Parameters:
  --      INPUT

  --      TOPOLOGY      - Topology name
  --      run_flag       - a prefix to add to the output work tables
  --      Edge_table    - name of EDGE table
  --      Edges_toprocesstable   - A work table name to create.
  --      State_Edge_table - A work table name to create listing the state for each edge

-- SQL> exec Run_Make_edge_Table('MT','AA','EDGE',EDGES_TOPROCESS','State_Edge_Table');

-- Makes the custom edges to process work table needed by simplify which needs
-- a current geometry and a list of nearby edges. Also makes the State_Edge_input
-- work table needed by Make_edge_table.

  Edges_table                     VARCHAR2(100) := UPPER(pEdges_toprocesstable);
  Edge_table                      VARCHAR2(100) := UPPER(pEdge_table);
  State_Edge_table                VARCHAR2(100) := UPPER(pState_Edge_table);
  sql_stmt                        VARCHAR2(4000);
  schema                          VARCHAR2(30);
  face                            VARCHAR2(100) := UPPER(pface);
  n  NUMBER;
BEGIN


-- First make sure caller has made our precursor tables

      sql_stmt := 'SELECT USER from dual';
      Execute immediate sql_stmt into schema;

-- None of these auxiliary tables (Face and Edge) can be depended upon to have indexes !!
      if INDEX_Exists(FACE,'FACE_ID',schema) = FALSE then
         sql_stmt := 'CREATE INDEX '|| FACE ||'_IDX on '|| FACE||'(FACE_ID)';
         Execute immediate sql_stmt;
      end if;
-- Make a state table which has the lowest state for each edge. We read all
-- edges including boundary edges!

   sql_stmt := 'INSERT INTO ' || State_Edge_table ||
           ' select ed.edge_id, fr.'|| pEntityfp ||' as right_state, fl.'|| pEntityfp ||' as left_state, ' ||
           'least(fr.'||pEntityfp||',fl.'||pEntityfp||') AS lowest_state from '|| Topology ||
           '_edge$ ed, '||face ||' fr,' ||face ||' fl' ||
           ' where ed.left_face_id = fl.face_id AND ed.right_face_id = fr.face_id ';
    Execute immediate sql_stmt;


    sql_stmt := 'INSERT INTO ' || State_Edge_table ||
           ' select ed.edge_id, -1 as right_state, fl.'|| pEntityfp ||' as left_state, ' ||
           ' fl.'||pEntityfp||' AS lowest_state from '|| Topology ||
           '_edge$ ed, '||face ||' fl where (ed.left_face_id = fl.face_id AND ed.right_face_id = -1)';
    Execute immediate sql_stmt;

 -- Yhis never happens
      sql_stmt := 'INSERT INTO ' || State_Edge_table ||
           ' select ed.edge_id, fl.'|| pEntityfp ||' as right_state, -1 as left_state,' ||
           ' fl.'||pEntityfp||' AS lowest_state from '|| Topology ||
           '_edge$ ed, '||face ||' fl where (ed.right_face_id = fl.face_id AND ed.left_face_id = -1)';
    Execute immediate sql_stmt;
    commit;
    sql_stmt := 'CREATE INDEX '||State_Edge_Table ||'_EIDX on '||State_Edge_Table||'(EDGE_ID)';
    Execute immediate sql_stmt;


  n := MAKE_EDGE_TABLE(Topology,'EDGE$',Edge_table,Edges_Table,NULL,State_Edge_table);

  dbms_output.put_line('edge count ' || n);
END RUN_MAKE_EDGE_TABLE;
--
PROCEDURE GET_BAD_IDS(Topology VARCHAR2 default 'MT',state_code VARCHAR2,Edges_Table VARCHAR2,validate_error VARCHAR2,BAD_IDS IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) AS

-- Gets the edges of a face when a validate error occurs like this:

--ORA-29532: Java call terminated by uncaught Java exception:
--oracle.spatial.topo.TopoValidationException: An island of face 33576 has an
--edge coincident with outer boundary
--ORA-06512: at "MDSYS.SDO_TOPO_MAP", line 92
--ORA-06512: at "ACS09GH3.GZ_SUPER", line 173
--ORA-06512: at line 1

  sql_stmt        VARCHAR2(4000);
  bad_ones        MDSYS.SDO_LIST_TYPE;
  pos2            PLS_INTEGER :=0;
  face            VARCHAR2(10);
  pos             PLS_INTEGER;
BEGIN

  pos := INSTR(validate_error,': An island of face');
  pos := pos + 19;

  for ii in 1..10 loop
--  dbms_output.put_line(SUBSTR(validate_error,pos+ii,1));
    if SUBSTR(validate_error,pos+ii,1) = ' ' then
      pos2 := pos+ii;
      face := SUBSTR(validate_error,pos,pos2-pos);
      exit;
    end if;
  end loop;
--  dbms_output.put_line('face' || face);

  sql_stmt := 'select e.edge_id from ' ||edges_Table ||' e,'||Topology||
              '_FACE$ f where sdo_relate(f.mbr_geometry,e.geometry,''mask=ANYINTERACT'') = ''TRUE'' and e.state=:1 and f.face_id=:2';
  execute immediate sql_stmt BULK COLLECT into bad_ones using state_code,face;
 -- Append to the array
  For ii in 1..bad_ones.count Loop
    Bad_ids.extend(1);
    bad_ids(bad_ids.count) := Bad_ones(ii);
  End Loop;

END GET_BAD_IDS;
--
FUNCTION MAKE_EDGE_TABLE(Topology VARCHAR2 default 'MT',mt_Edge VARCHAR2 default 'EDGE$',pedge_table VARCHAR2 default 'EDGE',Edges_toprocesstable VARCHAR2,Ignore_Table VARCHAR2 default NULL,State_Edge_table VARCHAR2) RETURN NUMBER AS

/*
********************************************************************************
--Program Name: Make_Edge_Table
--Author: Sidey Timmins
--Creation Date: 05/20/2010
--Updated: 10/20/2010 To handle other SRIDs
--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 5 required parameter:
  --
  --   REQUIRED Parameters:
  --      INPUT

  --      TOPOLOGY      - Topology name
  --      MT_EDGE       - MT_edge$ name
  --      Edge_table    - name of EDGE table
  --      Edges_toprocesstable   - A work table name to create.
  --      Ignore_Table  - Table of edges to ignore default NULL
  --      State_Edge_table - A work table name to create listing the state for each edge
--
-- Purpose: <<This function is automatically run by Simplify but since it is
--          slow to run (about 3 hours for a million edges) it is best to run it
--          separately. See Run_make_edge_table>>
--          It makes a custom edge table used by the Simplify function to generalize
--          each edge and update the topology with the new edge coordinates.
--          This edge table has edge, node and face info, MBRs and the original
--          geometry, and the nearby edges. These nearby edges are the key to
--          to simplify generalizing in the presence of nearby edges.
--          The new_geometry column (which starts out as the original geometry)
--          is used as a state variable to represent the state of each geometry
--          in MT_EDGE$ as Simplify updates the edge_coordinates in the topology.

-- Called by: Run_Make_Edge_table
-- Calls: Table_Exists, Get_Nearby_Edges
********************************************************************************
*/
   TYPE TblCursorType  IS REF CURSOR;

   Edges_table       VARCHAR2(100) := UPPER(Edges_toprocessTable);
   Table_cursor      TblCursorType;
   Edge_ids          MDSYS.SDO_LIST_TYPE;
   Loop_ids          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   Nearby_ids        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   nearby_edges      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   XLLs              MDSYS.SDO_LIST_TYPE;
   YLLs              MDSYS.SDO_LIST_TYPE;
   XURs              MDSYS.SDO_LIST_TYPE;
   YURs              MDSYS.SDO_LIST_TYPE;
   Start_nodes       MDSYS.SDO_LIST_TYPE;
   End_nodes         MDSYS.SDO_LIST_TYPE;
   schema            VARCHAR2(30);

   xLL               NUMBER;
   yLL               NUMBER;
   xUR               NUMBER;
   yUR               NUMBER;
   SRID              NUMBER := 8265;
   Edge_id           NUMBER;
   edge_kount        NUMBER := 0.;
   sql_stmt          VARCHAR2(4000);
   sql_stmt2         VARCHAR2(4000);
   distance          NUMBER;
   best_dist         NUMBER := 1000000.;
   ROW_LIMIT         NUMBER := 200;
   nearby_id         NUMBER;
   closest           PLS_INTEGER;
   n                 PLS_INTEGER;
   pos               PLS_INTEGER := 0.0;
   temp              NUMBER;
   the_time          timestamp;

BEGIN

-- Create a tablr of edges to process. Get them all (including the ones that ar
--on the coast) to make an accurate nearby list



   sql_stmt := 'SELECT USER from dual';
   Execute immediate sql_stmt into schema;

    -- This table holds the current geometry and act as a state variable
   sql_stmt := 'INSERT INTO  ' || Edges_Table ||  -- ' NOLOGGING ' ||
                 ' SELECT t.edge_id,t.start_node_id,t.end_node_id,t.left_face_id,t.right_face_id,s.lowest_state state,0. uniq,0. mt_length,' ||
                 ' SDO_GEOM.SDO_MIN_MBR_ORDINATE(t.geometry,1) XLL,' ||
                 ' SDO_GEOM.SDO_MIN_MBR_ORDINATE(t.geometry,2) YLL,' ||
                 ' SDO_GEOM.SDO_MAX_MBR_ORDINATE(t.geometry,1) XUR,' ||
                 ' SDO_GEOM.SDO_MAX_MBR_ORDINATE(t.geometry,2) YUR,' ||
                 ' CAST(NULL AS MDSYS.SDO_LIST_TYPE) NEARBY_EDGES,sdo_util.getnumvertices(t.geometry) vertices,0. Ignore,t.geometry,t.geometry new_geometry from ' ||
                 Topology||'_'||MT_edge || ' t,' || State_Edge_table || ' s where s.edge_id= t.edge_id';

   EXECUTE Immediate sql_stmt;

-- Get the SRID
   sql_stmt := 'SELECT t.geometry.SDO_SRID  from ' || Edges_table ||' t WHERE rownum=1';
   EXECUTE Immediate sql_stmt into SRID;

--Set the ones to ignore and not generalize
   sql_stmt := 'UPDATE ' || Edges_table ||' e set e.Ignore=1 WHERE e.vertices=2 or e.left_face_id = -1 or e.right_face_id = -1';
   EXECUTE Immediate sql_stmt;

   If Ignore_Table is NOT NULL then
     sql_stmt := 'UPDATE ' || Edges_table || ' e set e.Ignore= (SELECT 1 from ' || Ignore_Table ||' i where i.edge_id=e.edge_id) ' ||
                 'WHERE EXISTS (SELECT 1 from ' || Ignore_Table ||' i where i.edge_id=e.edge_id)';
     EXECUTE Immediate sql_stmt;
   end if;

-- Make an index on the edge_att table

   if INDEX_Exists(pEDGE_Table,'EDGE_ID',schema) = FALSE then
     sql_stmt := 'CREATE INDEX ' ||pEDGE_Table ||'_EIDX on '||pEDGE_Table ||'(EDGE_ID)';
     Execute immediate sql_stmt;
   end if;

   sql_stmt := 'UPDATE ' || Edges_table || ' e set e.mt_length= (SELECT d.edgelen from '||pEDGE_Table ||'  d where d.edge_id=e.edge_id)'||
                      ' WHERE EXISTS (SELECT d.edgelen from '||pEDGE_table ||' d where d.edge_id=e.edge_id)';
   EXECUTE Immediate sql_stmt;

   sql_stmt := 'CREATE INDEX '||Edges_Table ||'_EIDX on '||Edges_Table||'(EDGE_ID)';
   Execute immediate sql_stmt;
   sql_stmt := 'CREATE INDEX ' || Edges_Table ||'_XLLIDX ON ' || Edges_Table ||'(XLL)';
   EXECUTE IMMEDIATE sql_stmt;
   sql_stmt := 'CREATE INDEX ' || Edges_Table ||'_YLLIDX ON ' || Edges_Table ||'(YLL)';
   EXECUTE IMMEDIATE sql_stmt;
   sql_stmt := 'CREATE INDEX ' || Edges_Table ||'_XURIDX ON ' || Edges_Table ||'(XUR)';
   EXECUTE IMMEDIATE sql_stmt;
   sql_stmt := 'CREATE INDEX ' || Edges_Table ||'_YURIDX ON ' || Edges_Table ||'(YUR)';
   EXECUTE IMMEDIATE sql_stmt;

   sql_stmt := 'CREATE INDEX '||Edges_Table ||'_SIDX on '||Edges_Table||'(START_NODE_ID)';
   Execute immediate sql_stmt;

   sql_stmt := 'CREATE INDEX '||Edges_Table ||'_NIDX on '||Edges_Table||'(END_NODE_ID)';
   Execute immediate sql_stmt;

   sql_stmt := 'CREATE INDEX '||Edges_Table ||'_STIDX on '||Edges_Table||'(STATE)';
   Execute immediate sql_stmt;

   sql_stmt := 'CREATE INDEX '||Edges_Table ||'_STGIDX on '||Edges_Table||'(STATE,IGNORE)';
   Execute immediate sql_stmt;

 -- Calculate the node frequency of the node for closed loops. We actually count
 -- two less than the frequency.

   sql_stmt := 'UPDATE '|| Edges_Table || ' e set e.uniq=(select count(1) from ' || edges_table ||' b where  '||
                                             ' b.edge_id= e.edge_id and (e.start_node_id=e.end_node_id))';
   EXECUTE Immediate sql_stmt;

   sql_stmt := 'UPDATE '|| Edges_Table || ' e set e.uniq = (select count(1) from '|| Edges_Table ||
   ' b where e.uniq = 1 and e.edge_id <> b.edge_id and (e.start_node_id=b.start_node_id or e.start_node_id = b.end_node_id)) ' ||
   ' where exists (select count(1) from '|| Edges_Table ||
   ' b where e.uniq = 1 and e.edge_id <> b.edge_id and (e.start_node_id=b.start_node_id or e.start_node_id = b.end_node_id))';
  EXECUTE Immediate sql_stmt;


-- This marks the edges that have another edge that completes them with 100,200 etc
     sql_stmt := 'UPDATE '|| Edges_Table || ' e set e.uniq= e.uniq + 100*(select count(1) from ' || edges_table ||' b where b.edge_id <> e.edge_id and '||
        '((e.start_node_id=b.end_node_id and e.end_node_id = b.start_node_id) or(e.start_node_id=b.start_node_id and e.end_node_id = b.end_node_id))) ' ||
        'where exists(select count(1) from '|| Edges_Table || ' b where b.edge_id <> e.edge_id and ((e.start_node_id=b.end_node_id and e.end_node_id = b.start_node_id) or(e.start_node_id=b.start_node_id and e.end_node_id = b.end_node_id)))';
    EXECUTE Immediate sql_stmt;
   commit;

-- Mark figure of eights with an 8
     sql_stmt := 'UPDATE '|| Edges_Table || ' e set e.uniq=(select 8 from ' || edges_table ||' b where b.edge_id <> e.edge_id and '||
        '(e.start_node_id=e.end_node_id and e.end_node_id = b.start_node_id and e.end_node_id=b.end_node_id)) ' ||
        'where exists(select 8 from '|| Edges_Table || ' b where b.edge_id <> e.edge_id and (e.start_node_id=e.end_node_id and e.end_node_id = b.start_node_id and e.end_node_id=b.end_node_id))';
    EXECUTE Immediate sql_stmt;
   commit;

   if SRID = 8265 then
     add_geom_metadata(Edges_Table,'GEOMETRY',schema,SRID);
   else
     -- Get the extent of the data
     sql_stmt := 'SELECT MIN(XLL),MIN(YLL),MAX(XUR),MAX(YUR) from ' || Edges_table;
     EXECUTE Immediate sql_stmt into xLL,yLL,xUR,yUR;
     add_geom_metadata(Edges_Table,'GEOMETRY',schema,SRID,xLL,yLL,xUR,yUR,0.001);
   end if;

   sql_stmt := 'CREATE INDEX '||Edges_Table ||'_SPIDX on '||Edges_Table||'(GEOMETRY) ' ||
              'INDEXTYPE IS MDSYS.SPATIAL_INDEX';
   Execute immediate sql_stmt;

   sql_stmt := 'Select Edge_id from ' || Edges_Table || ' where start_node_id = end_node_id';
   Execute immediate sql_stmt BULK COLLECT into Loop_ids;

   n := Loop_ids.count;
   Nearby_ids.extend(n);

-- Get each edges nearby edges  (the ones that intersect the subject edges MBR)
   sql_stmt := 'Select edge_id,start_node_id,end_node_id,xLL,yLL,xUR,yUR from ' || Edges_Table ||
               ' order by edge_id';

   sql_stmt2 := 'SELECT sdo_geom.sdo_distance(t.geometry,s.geometry,0.05,''unit=meter'') from ' ||
                 Edges_table || ' t,' || Edges_table || ' s where t.edge_id=:1 and s.edge_id=:2';

  OPEN Table_cursor FOR sql_stmt;

   LOOP
       FETCH Table_cursor BULK COLLECT INTO Edge_ids,Start_nodes,End_nodes,xLLs,yLLs,xURs,yURs LIMIT ROW_LIMIT;
       Exit when Edge_ids.count=0;

       edge_kount := edge_kount + Edge_ids.count;

       For ii in 1..Edge_ids.count Loop
         Edge_id := Edge_ids(ii);

-- Get Nearby Edges uses x and y epsilons around the border of the edge MBR to
-- ensure it finds all nearby edges.
         xLL := XLLs(ii);
         yLL := yLLs(ii);
         xUR := xURs(ii);
         yUR := yURs(ii);
         Nearby_Edges := Get_Nearby_edges(Edges_Table,Edge_id,xLL,yLL,xUR,yUR,SRID);
         temp := 0.0;
         if Nearby_Edges.count = 1 then
           temp := Nearby_edges(1);
         end if;
-- Mark the closest when the edge is a closed loop.
         if Start_nodes(ii) = End_nodes(ii) and Nearby_Edges.count > 1 then
            best_dist := 1000000.;
            closest := 0;
--            the_time := current_timestamp;
            for jj in 1..Nearby_edges.count Loop
              nearby_id := Nearby_edges(jj);
              Execute immediate sql_stmt2 into distance using edge_id,nearby_id;
              if distance < best_dist then
                closest := jj;
                best_dist := distance;
              end if;
            end loop;
            if closest <> 0 then
              temp := Nearby_edges(1);
              Nearby_edges(1) := Nearby_edges(closest);
              Nearby_edges(closest) := temp;
            else
              temp := 0.0;
            end if;
        end if;
-- Now keep track and store the closest loop to each edge
         if Start_nodes(ii) = End_nodes(ii) then
            pos := pos +1;
            Nearby_ids(pos) := temp;
            Loop_ids(pos) := edge_id;
         end if;
--            dbms_output.put_line('Elapsed time : ' || (current_timestamp - the_time));


-- Insert the nearby edges for current edge into the table
         If Nearby_Edges.count <> 0 then
               sql_stmt := 'Update ' || Edges_Table|| ' set NEARBY_EDGES=:1 where edge_id=:2';
               Execute immediate sql_stmt using Nearby_Edges,edge_id;
         End if;

       End Loop;
       commit;

   END LOOP;
   CLOSE Table_cursor;
   commit;
-- Now mark the closest loop for each edge that has a nearby loop.

   for ii in 1..n loop
     if Nearby_ids(ii) <> 0.0 then
         closest := 0;
         Edge_id := Nearby_ids(ii);
         sql_stmt := 'Select Nearby_edges from ' || Edges_Table|| ' where edge_id=:1';
         Execute immediate sql_stmt into Nearby_edges using edge_id;
         if Nearby_edges is NULL then
            Nearby_edges := MDSYS.SDO_LIST_TYPE();
            Nearby_edges.extend(1);
            Nearby_edges(1) := Loop_ids(ii);
            closest := 1;
         else
          for jj in 1..Nearby_edges.count loop
             if Nearby_edges(jj) = Loop_ids(ii) then
                closest := jj;
                temp := nearby_edges(1);
-- Store the closest loop as the 1st edge in Nearby edges for this edge_id
                Nearby_edges(1) := Loop_ids(ii);
                Nearby_edges(jj) := temp;
                exit;
             end if;
          end loop;
         end if;
         if closest <> 0 then
           sql_stmt2 := 'Update ' || Edges_Table|| ' set NEARBY_EDGES=:1 where edge_id=:2';
           Execute immediate sql_stmt2 using Nearby_Edges,edge_id;
         end if;
     end if;
   end loop;
-- Note that we cannot delete any edges because we need them for nearby checks
-- (for example the ones with 2 vertices)
   commit;


  RETURN edge_kount;

END MAKE_EDGE_TABLE;
--
FUNCTION GET_NEARBY_EDGES(Edges_Table VARCHAR2,Edge_id NUMBER,pxLL NUMBER,pyLL NUMBER,pxUR NUMBER,pyUR NUMBER,SRID NUMBER,pxepsilon NUMBER default NULL,pyepsilon NUMBER default NULL,ptolerance NUMBER default 0.05) RETURN MDSYS.SDO_LIST_TYPE AS
/*
**************************************************************************************
--Program Name: Get_Nearby_edges
--Author: Sidey Timmins
--Creation Date: 05/06/2010
--Updated:
--Usage:
  -- Call this function from inside another PL/SQL program.
  --
  --   REQUIRED Parameters:
  --           Edges_table -- a table name of a table that has columns
  --                           xLL,yLL,xUR,yUR for each geometry.
  --           Edge_id     - The edge_id in question to get the nearby edges for
  --           (pxLL,pyLL): coordinates in degrees of lower left
  --           (pxUR,pyUR): coordinates in degrees of upper right
  --           pxepsilon: a distance to search nearby in degrees.
  --               When pxepsilon is NULL then this function calculates it
  --               using the tolerance (usually 0.05 meters).
  --           pyepsilon: a distance to search nearby in degrees
  --               When pyepsilon is NULL then this function calculates it.
  --               It is suggested you leave this NULL.
  --           ptolerance: the tolerance usually 0.05 meters
--
-- Purpose:  Gets the edge_ids of nearby edges that are within (wholly or partially)
--           the extent (MBR) of the edge in question. No geometries are pulled.

-- Method:  Uses standard computer graphics code for clipping to a window
--          moderated slightly by borders to allow for geodetic coordinates.
--          This function may be slightly conservative and find a few edges which
--          are not within the subject's MBR but so far, I have not found any
--          edges missed.
--
-- Reference: See Wiki for Sideys approximation to great circle distances.
-- Dependencies: Requires the Edges_toprocess table exists with MBRs for each edge.
-- Called by: Make_Edge_Table
***************************************************************************************
*/

    deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
    Nearby_Edges       MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();
    sql_stmt    VARCHAR2(4000);

    xLL         NUMBER := pXLL;
    yLL         NUMBER := pYLL;
    xUR         NUMBER := pXUR;
    yUR         NUMBER := pyUR;
    epsilon     NUMBER := pyepsilon;
    delta       NUMBER := pxepsilon;
    xdiff       NUMBER;
    n           PLS_INTEGER := 0;

BEGIN

-- This function fails badly when these are NULL.
   If xLL is NULL or yLL is NULL or xUR is NULL or yUR is NULL then
      dbms_output.put_line('ERROR: Get nearby edges has null mbrs <<<');
      RETURN NULL;
   end if;

-- Convert 0.05 meters to degrees
   if delta is NULL and SRID = 8265. then
       delta := ptolerance/(111319.490793274*cos(yLL*deg2rad));
   elsif delta is NULL then
       delta := ptolerance;
   end if;

   xLL := xLL - delta;
   xUR := xUR + delta;

-- This vertical offset was found by checking a horizontal line 1 degree long
-- at all latitudes and finding the max vertical offset (.001091) and finding
-- that the y offset varied with latitude and as the square of the length.
--lat 0 diff 0
--lat 10 diff .000373
--lat 20 diff .000701
--lat 30 diff .000945
--lat 40 diff .001074
--lat 45 diff .001091
--lat 50 diff .001074
--lat 60 diff .000945
--lat 70 diff .000701
--lat 80 diff .000373
--lat 45 max diff .001091 per degree .001091

   if epsilon is NULL and SRID = 8265. then
         epsilon := .0011;
         xdiff := xUR-xLL;
         if xdiff > 1. then
           epsilon := epsilon * xdiff*xdiff;
         end if;
   elsif epsilon is NULL then
     epsilon := ptolerance;
   end if;
   yLL := yLL - epsilon;
   yUR := yUR + epsilon;

 -- Get nearby edges. We exclude the current Edge_id by ignoring it in the results.

   sql_stmt := 'SELECT m.edge_id FROM '|| Edges_Table  || ' m WHERE ' ||
               'SDO_FILTER(m.geometry,MDSYS.sdo_geometry(2003,:1,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,3),'||
               'MDSYS.SDO_ORDINATE_ARRAY(:2,:3,:4,:5))) = ''TRUE''';
-- SDO_FILTER is much faster than these range checks on a large table
--   'NOT( (( m.XUR <:1)  OR (m.YUR <:2)) OR  (( m.XLL >:3) OR (m.YLL > :4)))';
    EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO Nearby_Edges USING SRID,xLL,yLL,xUR,yUR;

-- we may return an empty array it would seem.

    IF Nearby_Edges is NULL THEN
       nearby_Edges := MDSYS.SDO_LIST_TYPE();
    ELSE
       For ii in 1..Nearby_Edges.count Loop
         If nearby_Edges(ii) = edge_id then
           NULL; -- skip the edge in question (ourself)
         else
           n := n+1;
           Nearby_Edges(n) := Nearby_Edges(ii);
         end if;
        End Loop;

       Nearby_Edges.trim(Nearby_Edges.count-n);
    END IF;

  RETURN Nearby_Edges;    -- We may return an empty array

END GET_NEARBY_EDGES;
--
FUNCTION Perpendicular(xin IN OUT NOCOPY NUMBER,yin IN OUT NOCOPY NUMBER,  -- the point
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,  -- the line
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                       xnear IN OUT NOCOPY NUMBER,ynear IN OUT NOCOPY NUMBER,  -- the perpendicular
                       always BOOLEAN default FALSE,  -- always return a distance
                       meters BOOLEAN default FALSE)  -- when true return meters
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
     dx            NUMBER := x2-x1;
     dy            NUMBER := y2-y1;

BEGIN
--     dbms_output.put_line('xin ' || xin || ' yin ' || yin);

     u := ((Xin - X1) * dx + (Yin-Y1) * dy);

--     dbms_output.put_line('U ' ||U || ' ' || (u/(dx*dx + dy*dy)));
--     dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
--     dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2);
     xnear := NULL;
     ynear := NULL;
     If u >= 0. or Always then
        length_sq :=  dx*dx + dy*dy;
        if (u <= length_sq or Always) and length_sq > 0. then

           u := u/length_sq;

           xnear := X1 + u * dx;
           ynear := Y1 + u * dy;

           if meters then
             Distance := fast_distance(xin,yin,xnear,ynear,8265);
           else   -- eof on communication channel ?? Added these 2 lines
             dx := ROUND(Xin-xnear,9);
             dy := ROUND(Yin-ynear,9);
             Distance := sqrt((dx*dx + dy*dy));
           end if;
--        dbms_output.put_line('distance ' || distance);
        elsif length_sq = 0. then  -- zero length line
           NULL;
        end if;
     end if;
     RETURN Distance;

END Perpendicular;
--
FUNCTION REMOVE_CLOSE_XYS(Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,ptolerance NUMBER default 0.05)
 RETURN BOOLEAN Deterministic IS
/**
 ################################################################################
 # Program Name: Remove_dupXys
 # Author: Sidey Timmins
 # Creation Date: 7/22/2008
 # Updates: 10/22/2010 To leave 2 vertex edges alone.
 # Usage:
 #   This PL/SQL procedure has 2 parameters:
 #                   Geom: the Geometry to check and fix.
 #                   ptolerance: Tolerance in meters - vertices closer than this
 #                               will have one removed.
 #   Returns TRUE only when the geometry is changed;
 #
 # Purpose: This procedure removes vertices which are too close.
 #          The resolution for our geodetic coordinates is 1 millionth of a degree.
 #          A longitude difference of 1.E-06 degrees at 64 degrees North with the
 #          same latitude is 0.047 meters and less than the 0.05 meter tolerance.
 #
 # Method: Filters vertices to determine the ones to remove.
 # Dependencies:
 #  CDB_UTIL.fast_vincenty_gcd
 ################################################################################
*/

    XYOrd             MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Info_Array        MDSYS.SDO_ELEM_INFO_ARRAY;

    geometry          MDSYS.SDO_GEOMETRY;

    ii         PLS_INTEGER;
    xlast      NUMBER;
    ylast      NUMBER;
    xnew       NUMBER;
    ynew       NUMBER;
    k          PLS_INTEGER := -1;
    minimum_no PLS_INTEGER := 4;
    ycheck     NUMBER;
    xcheck     NUMBER;
    distance   NUMBER := 0.;
    rings      PLS_INTEGER;
    LB         PLS_INTEGER;
    UB         PLS_INTEGER;
    j          PLS_INTEGER;
    n          PLS_INTEGER;

    tolerance  NUMBER := ptolerance;
    tolerance2 NUMBER;
    oracle_tol NUMBER := 0.05;    -- usual Oracle tolerance for geometries
    epsilon    NUMBER := 0.0000005;
    GTYPE      NUMBER := geom.SDO_GTYPE;
    SRID       NUMBER := geom.SDO_SRID;


-- Written as a procedure so as NOT to create new geometry objects and waste memory

BEGIN

-- Detect dups because most edges are ok

    Info_Array := geom.SDO_ELEM_INFO;
    XYORD := geom.SDO_Ordinates;
    n := XYOrd.count;
-- Cannot shorten an edge with 2 vertices;
--dbms_output.put_line('IN remove ' || XYord.count);
    If XYord.count = 4 or NOT(gtype = 2002 or gtype = 2003) then
      RETURN FALSE;
    end if;
    If gtype = 2003 or (XYOrd(1) = XYOrd(n-1) and XYord(2) = XYOrd(n)) then
       minimum_no := 8;  -- minimum number of coordinates in loop
    End if;

    If ptolerance <= 0. then
       tolerance := 0.05;
    End if;
-- These parameters were tested from 0 to 72 degrees North with random coordinates
-- to check that this procedure got the same results as the Oracle function
-- SDO_UTIL.REMOVE_DUPLICATE_VERTICES. Note that the Oracle function may change the
-- coordinates!

    tolerance2 := tolerance *1.01;
    If SRID = 8265 THEN
      ylast := XYOrd(2);
      if ylast > 50. then
        xcheck   := 0.0000016 * tolerance*20.;  -- = /0.05
        ycheck   := 0.0000005 * tolerance*20.;
      else
        xcheck   := 0.00000075 * tolerance*20.;
        ycheck   := 0.00000065 * tolerance*20.;
      End If;
    ELSE
        oracle_tol := 0.0005;
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
     k := k + 2;
     Info_Array(j) := k;
     XYOrd(k) := xlast;
     XYOrd(k+1) := ylast;

     For jj in LB..UB LOOP
       ii := jj*2-1;
       xnew := XYOrd(ii);
       ynew := XYOrd(ii+1);

-- Empirical set of comparisons so we rarely calculate the difference.
       If k + (UB-jj)*2 > minimum_no and abs(xnew - xlast) < xcheck and abs(ynew - ylast) < ycheck then
--          If abs(xnew - xlast) <= 0.00000125 and abs(ynew - ylast) <= 0.00000125 then

-- Possible candidate for removal
           distance := fast_distance(xnew,ynew,xlast,ylast,SRID);
           if distance < tolerance2 then
                  geometry := SDO_GEOMETRY(2002,SRID,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),
                             SDO_ORDINATE_ARRAY(xnew,ynew,xlast,ylast));
                  distance := SDO_GEOM.SDO_LENGTH(geometry,oracle_tol,'unit=meter');
           End If;

-- drop it if within tolerance (equal or less)
          if distance <= tolerance and jj <>UB then
--          dbms_output.put_line('distance ' || round(distance,6) || ' dropping ' || k);
--            dbms_output.put_line('distance ' || distance || ' xlast ' || xlast || ' xnew ' || xnew || ' ylast  ' || ylast || ' ynew ' || ynew);
--            dbms_output.put_line('xdiff ' ||abs(xnew - xlast) || ' ydiff ' || abs(ynew - ylast));
            NULL;
          else
-- drop the previous vertex at the end
            if distance > tolerance then
              k := k+2;
            End If;

-- keep it and store coordinates
           XYOrd(k) := xnew;
           XYOrd(k+1) := ynew;
           xlast := xnew;
           ylast := ynew;
--  dbms_output.put_line('keeping at end');
          End If;
       Else
-- usual case - wide apart.
         k := k+2;
-- Store coordinates
         XYOrd(k) := xnew;
         XYOrd(k+1) := ynew;
         xlast := xnew;
         ylast := ynew;
--dbms_output.put_line('usual case ' || k || ' xnew ' || xnew || ' ynew ' || ynew);
       End If;

    End Loop;
    END LOOP;

    k := k + 1;

    If k <> XYOrd.count then
       XYOrd.trim(XYOrd.count-k);
       geom := MDSYS.SDO_GEOMETRY(GTYPE,SRID,NULL,Info_Array,XYOrd);
--       dbms_output.put_line('changed ' || XYord.count);
       RETURN TRUE;
    ELSE
       RETURN FALSE;
    End If;

END Remove_close_Xys;
--
FUNCTION remove_obtuse_angles(check_angle number, In_Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY) return MDSYS.SDO_ORDINATE_ARRAY as

--   Choose vertices (besides 1st and last) that subtend an angle < check angle
--   with their neighbors. Check_angle typically should be 160-170 degrees.

 rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
     Out_Xys       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
     nof     pls_integer :=1;
     pos     pls_integer :=2;
     next   pls_integer :=2;
     n       pls_integer := TRUNC(In_Xys.count/2);
     x0      number;
     y0      number;
     x1      number;
     y1      number;
     x2      number;
     y2      number;
     angle   number;

BEGIN

   Out_Xys := In_Xys;
   x1 := In_Xys(1);
   y1 := In_Xys(2);


   For ii in 2..n-1 loop
      x0 := x1;
      y0 := y1;
      pos := pos+2;
      x1 := In_Xys(pos-1);
      y1 := In_Xys(pos);
      x2 := In_Xys(pos+1);
      y2 := In_Xys(pos+2);

      angle := GZ_QA.angle(x0,y0,x1,y1,x2,y2)*rad2deg;
--      dbms_output.put_line('A ' || round(angle,3));
      if angle < check_angle then
        next := next+2;
        Out_Xys(next-1) := x1;
        Out_Xys(next)   := y1;
      end if;
   End loop;
   next := next+2;
   Out_xys(next-1) := x2;
   Out_Xys(next) := y2;
   Out_Xys.trim(Out_Xys.count-next);
   Return Out_Xys;
END;
--
FUNCTION SIMPLE_INTERSECT(   X1 IN OUT NOCOPY NUMBER,
                              Y1 IN OUT NOCOPY NUMBER,
                              X2 IN OUT NOCOPY NUMBER,
                              Y2 IN OUT NOCOPY NUMBER,
                              X3 IN OUT NOCOPY NUMBER,
                              Y3 IN OUT NOCOPY NUMBER,
                              X4 IN OUT NOCOPY NUMBER,
                              Y4 IN OUT NOCOPY NUMBER )  RETURN BOOLEAN IS
   s           NUMBER;
   t           NUMBER;
   X21         NUMBER := X2-X1;
   Y21         NUMBER := Y2-Y1;
   X43         NUMBER := X4-X3;
   Y43         NUMBER := Y4-Y3;
   X31         NUMBER := X3-X1;
   Y31         NUMBER := Y3-Y1;

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


      If s > 0.0 and s < 1.0 and t > 0.0 and t < 1.0 then
            RETURN TRUE;
      End if;

   END IF;
   Return FALSE;

END Simple_Intersect;
--
PROCEDURE CHECK_ALL_VERTICES(pInTable VARCHAR2,PGeometry_column VARCHAR2,pInTable2 VARCHAR2,PnewGeometry_column VARCHAR2,pOutput_table VARCHAR2,pInclude_state_edges VARCHAR2 default 'NO',pprint VARCHAR2 default 'NO') AS
BEGIN
  GZ_QA.check_all_vertices(pInTable,PGeometry_column,pInTable2,PnewGeometry_column,pOutput_table,pInclude_state_edges,pprint);
END CHECK_ALL_VERTICES;
--
PROCEDURE SHELLSORT( InArray      IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       InLB         PLS_INTEGER default 1,
                       InUB         PLS_INTEGER default 0,
                       pInDirection    VARCHAR2 default 'ASC')
 AS
/*******************************************************************************
--Program Name: c_shellsort
--Author: Sidey Timmins
--Creation Date: 01/04/2007

--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 1 required parameter:
  --
  --   REQUIRED Parameters:
 --          InArray           - Input number Array to sort. Sort is ascending.
 --                            c_shellsort(Data_Array,1,n);

 --          LB              - lower bound of Arr to sort, defaults to 1
 --          UB              - upper bound of Arr to sort, defaults to Arr.count
--
--Purpose:
  -- Sorts an array ascending or descending.

    -- example of input                             sorted output
  --    unsorted Nodes         Order         Node                sorted Order
--     216000003385025             1         8185025                   4
--     216000008785025             2         8185025                   3
--             8185025             3         8385025                   5
--             8185025             4         216000003385025           1
--             8385025             5         216000008785025           2
--     216000008785025             6         216000008785025           6

-- Sort in place with no additional storage. Note Shellsort is not stable (that
-- is it may move equal values that are already sorted as shown above for
-- elements 3 and 4).

-- Reference: Robert Sedgewick "Algorithms" Addison Wesley 1984
--            DL Shell. "A High-Speed Sorting Procedure"
--                       Communications of the ACM, 2,7 30-32 (1959)
--Dependencies: none
--Updates:
--          01/04/07 Added trunc to fix SQL divide bug. No change in behaviour.
********************************************************************************
*/
  Direction        VARCHAR2(10)              := SUBSTR(UPPER(pInDirection),1,3);
  LB               PLS_INTEGER               := InLB;
  UB               PLS_INTEGER               := InUB;
  LBh              PLS_INTEGER;
  h                PLS_INTEGER := 4;
  i                PLS_INTEGER;
  j                PLS_INTEGER;

  valu             NUMBER;
  n                PLS_INTEGER;

  BEGIN

  IF UB = 0 THEN
     UB := InArray.count;
  END IF;

  n := UB - LB +1;
  IF (n <= 1) THEN
     RETURN;
  END IF;

 -- compute largest increment: h. NOTE h=4 best for short arrays

-- Successive values of h are 1, 4, 14, 40, 121,..
-- see references to understand this loop
   IF (n > 14) THEN

     WHILE (h <= n) LOOP
       h := 3 * h + 1;
     END LOOP;
 -- After this line, have the largest increment <= n
     h := trunc(h / 3);

   END IF;

   IF Direction = 'ASC' THEN

   While h > 0  LOOP

-- Sort by insertion in increments of h
     LBh := LB+h;
      For i IN LBh..UB LOOP
        valu := InArray(i);
        j := i;
        WHILE ( j >= LBh) and valu < InArray(j-h) LOOP
           InArray(j) := InArray(j-h);
           j := j - h;
        END LOOP;
        InArray(j) := valu;
      END LOOP;
     h := trunc(h / 3);
  End Loop;

-- Array is now sorted ascending
  ELSE
-- Descending sort

     While h > 0  LOOP

       LBh := LB+h;
       For i IN LBh..UB LOOP
        valu := InArray(i);
        j := i ;
        WHILE ( j >= LBh) and (valu > InArray(j-h))  LOOP
          InArray(j) := InArray(j-h);
          j := j - h;
        END LOOP;
        InArray(j) := valu;
      END LOOP;
      h := trunc(h / 3);
    End Loop;
  END IF;

End Shellsort;
--
FUNCTION BUILD_MBR(topology     VARCHAR2,
                    state_code      VARCHAR2,
                    min_x           NUMBER default NULL,
                    min_y           NUMBER default NULL,
                    max_x           NUMBER default NULL,
                    max_y           NUMBER default NULL,
                    tolerance       NUMBER default 0.05,

                    pskip_Table     VARCHAR2 default NULL,
                    pDone_Table     VARCHAR2 default 'EDGES_DONE',
                    pEdges_Table    VARCHAR2 default 'EDGES_TOPROCESS') RETURN MDSYS.SDO_ORDINATE_ARRAY AS


    type        cursor_type is REF CURSOR;
    query_crs   cursor_type ;

    Done            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Edge_ids        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Skip_Table      VARCHAR2(100) := UPPER(pskip_table);
    Done_Table      VARCHAR2(100) := UPPER(pdone_table);
    Edges_Table     VARCHAR2(100) := UPPER(pEdges_table);
    MBR_GEOM        MDSYS.SDO_GEOMETRY;
    MBR             MDSYS.SDO_ORDINATE_ARRAY;
    max_id        NUMBER;
    edge_id       NUMBER;
    no_on_UF      NUMBER;
    no_done       NUMBER;
    no_skip       NUMBER;
    SRID          NUMBER :=8265.;
    temp          NUMBER;
    p             PLS_INTEGER;
    row_limit     NUMBER :=  10000;
    sql_stmt      VARCHAR2(4000);
    query_str     VARCHAR2(4000);
    error_msg     VARCHAR2(1000);
    minus_statement    VARCHAR2(4000) := NULL;
    and_statement      VARCHAR2(4000) := NULL;
BEGIN

--------------------------------------------------------------------------------
    query_str := 'SELECT max(edge_id) FROM ' || topology || '_edge$';
    execute immediate query_str into max_id;
    Done.extend(max_id);

--------------------------------------------------------------------------------
-- Find out how many on Universal Face
    EXECUTE IMMEDIATE 'SELECT count(1) FROM ' || Edges_table || ' WHERE IGNORE=1' into no_on_UF;

-- Skip edges in Done Table. Zero means not done, 1 means done (or do not process).
    for ii in 1..max_id loop
      Done(ii) := 0.;
    end loop;

    query_str := 'SELECT  edge_id FROM ' || Done_Table;


    OPEN query_crs FOR query_str;

    LOOP
      FETCH query_crs BULK COLLECT INTO edge_ids LIMIT Row_Limit;
      EXIT WHEN edge_ids.count = 0;
      for ii in 1..Edge_ids.count loop
        edge_id := Edge_ids(ii);
        Done(edge_id) := 1.;
      end loop;
      no_done := no_done + Edge_ids.count;
    END LOOP;

    CLOSE query_crs;

--------------------------------------------------------------------------------

-- Skip edges in Skip Table
    IF Skip_Table is NOT NULL then
      p := INSTR(Skip_table,'(');
      if p = 0 Then
      query_str := 'SELECT  Edge_id FROM ' || Skip_Table;
      dbms_output.put_line(query_str);
       GZ_TOPOFIX.Track_App(query_str,Topology,'LS');
      OPEN query_crs FOR query_str;

      LOOP

        FETCH query_crs BULK COLLECT INTO edge_ids LIMIT Row_limit;
        EXIT WHEN edge_ids.count=0;
        for ii in 1..Edge_ids.count loop
          edge_id := Edge_ids(ii);
          Done(edge_id) := 1.;
        end loop;
        no_skip := no_skip + Edge_ids.count;
      END LOOP;

      CLOSE query_crs;

      and_statement := 'e.edge_id NOT in (' || query_str || ') and ';

-- It is a skip List so get rid of the Topology name at the beginning
      else

          minus_statement := ' and e.edge_id NOT in ' || Skip_Table;
          p := INSTR(Skip_table,')');
          if p = 0 then
            minus_statement := minus_statement || ')';
          end if;
      end if;
    END IF;


    sql_stmt := 'SELECT SDO_AGGR_MBR(e.geometry) FROM ' ||
                Edges_Table || ' e WHERE e.state=:1 and e.Ignore=0 and ' ||  and_statement ||
                  'SDO_ANYINTERACT(e.geometry,MDSYS.sdo_geometry(2003,:2,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,3),'||
                 'MDSYS.SDO_ORDINATE_ARRAY(:3,:4,:5,:6))) = ''TRUE''' || minus_statement;

    EXECUTE IMMEDIATE sql_stmt INTO MBR_Geom USING state_code,SRID,min_x,min_y,max_x,max_y;
    MBR := MBR_GEOM.sdo_ordinates;

    RETURN MBR;
END BUILD_MBR;
--
FUNCTION SIMPLIFY_REGION (topology     VARCHAR2,
                                        topomap_name    VARCHAR2,
                                        pscale          NUMBER,
                                        nice            NUMBER,
                                        state_code      VARCHAR2,
                                        min_x           NUMBER default NULL,
                                        min_y           NUMBER default NULL,
                                        max_x           NUMBER default NULL,
                                        max_y           NUMBER default NULL,
                                        tolerance       NUMBER default 0.05,
                                        pbad_Table      VARCHAR2 default 'BAD_EDGES',
                                        pskip_Table     VARCHAR2 default NULL,
                                        pDone_Table     VARCHAR2 default 'EDGES_DONE',
                                        pEdges_Table    VARCHAR2 default 'EDGES_TOPROCESS',
                                        method          VARCHAR2 default 'ZONE',
                                        dec_digits      NUMBER default 6.
                                        ) RETURN NUMBER AS
--                                        context_info     SDO_KEYWORDARRAY) IS

    type        cursor_type is REF CURSOR;
    query_crs   cursor_type ;

    Done            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Edge_ids        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    State_Edge_ids  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Bad_ids         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Skip_ids        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

    Bad_Table       VARCHAR2(100) := UPPER(pbad_table);
    Skip_Table      VARCHAR2(100) := UPPER(pskip_table);
    Done_Table      VARCHAR2(100) := UPPER(pdone_table);
    Edges_Table     VARCHAR2(100) := UPPER(pEdges_table);
    PolyInfo_Array  MDSYS.SDO_ELEM_INFO_ARRAY:= MDSYS.SDO_ELEM_INFO_ARRAY();
    PolyXys         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    EndXys          MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    self_segs       MDSYS.SDO_LIST_TYPE;


    edge_id         NUMBER;
    max_id          NUMBER;
    scale           NUMBER := pscale;
    row_limit       NUMBER := 10000;
    vertex          NUMBER;
    seg1            NUMBER;
    seg2            NUMBER;
    start_node      NUMBER;
    end_node        NUMBER;
    uniq            NUMBER;
    elength         NUMBER;
    vertices        NUMBER;
    area            NUMBER;
    short_dist      NUMBER;
    perim           NUMBER;
    length_cutoff   NUMBER := pscale/40.;
    delta           NUMBER := 0.001;
    xLL2            NUMBER;
    yLL2            NUMBER;
    xUR2            NUMBER;
    yUR2            NUMBER;


    no_done         PLS_INTEGER := 0;
    no_skip         PLS_INTEGER := 0;
    loop_counter    PLS_INTEGER := 0;
    sem_count       PLS_INTEGER := 0;   -- single edge_mode count
    idx             PLS_INTEGER := 0;
    last_idx        PLS_INTEGER := 0;
    batch           PLS_INTEGER := 500;
    m               PLS_INTEGER;
    loup_count      PLS_INTEGER := 0;
    geometry        MDSYS.SDO_GEOMETRY;
    new_geometry    MDSYS.SDO_GEOMETRY;
    poly_geometry   MDSYS.SDO_GEOMETRY;
    XYOrd           MDSYS.SDO_ORDINATE_ARRAY;
    origXYOrd       MDSYS.SDO_ORDINATE_ARRAY;

    status           VARCHAR2(10000);
    result           VARCHAR2(4000);
    minus_statement  VARCHAR2(4000) :=NULL;
    OK               BOOLEAN;
    sql_stmt         VARCHAR2(4000);
    sql_stmt2        VARCHAR2(4000);
    sql_stmt3        VARCHAR2(4000);
    sql_stmt4        VARCHAR2(4000);
    sql_stmt5        VARCHAR2(4000);
    sql_stmt6        VARCHAR2(4000);
    sql_stmt7        VARCHAR2(4000);
    query_str        VARCHAR2(4000);
    error_msg        VARCHAR2(1000);
    once             VARCHAR2(5) := 'TRUE';
    low_id           NUMBER;
    hi_id            NUMBER;
    bad_count        NUMBER;
    no_on_UF         NUMBER;
    SRID             NUMBER :=8265.;
    loops            PLS_INTEGER := 0;
    kount            PLS_INTEGER;
    p                PLS_INTEGER;
    window_geom      SDO_GEOMETRY;
    single_edge_mode BOOLEAN := FALSE;
    problem_edge     BOOLEAN := FALSE;
    clear_it         PLS_INTEGER := 0;
    the_time         timestamp;
--------------------------------------------------------------------------------
    function is_digit(cvalue varchar2) return boolean as

-- Is a character a digit? (or a sign?)
    begin
        for ii in -1..9 loop
           if (ii=-1 and (cvalue='-' or cvalue='+')) then
             return true;
           elsif (ii <=9 and cvalue=ii )then
             return true;
           end if;
        end loop;
        return FALSE;
    end;

    function is_digits(cvalue varchar2) return boolean as

-- Are all the characters digits?
    begin
        for ii in 1..length(cvalue) loop
           if NOT is_digit(SUBSTR(cvalue,ii,1)) then
             return false;
           end if;
        end loop;
        return TRUE;
    end;

    function split(input VARCHAR2) return mdsys.sdo_list_type as

-- Split a comma delimited list into an array of values.

       pos     pls_integer :=1;
       next    pls_integer:= 0;
       nof     pls_integer :=0;
       valus   mdsys.sdo_list_type := mdsys.sdo_list_type();
       cval    varchar2(100);
    begin
-- Parse comma separated integers

       while pos < Length(input) loop
          next := INSTR(input,',',pos+1);
          if next = 0 then
            next := INSTR(input,')',pos+1);
          end if;
          if next = 0 then
            next := Length(input);
            pos := Length(input);
          end if;
          cval := SUBSTR(input,pos+1,next-pos-1);

          if is_digits(cval) then
            nof := nof+1;
            valus.extend(1);
            valus(nof) := cval;

          end if;
          pos := next;
       end loop;

       return valus;
    end;
--------------------------------------------------------------------------------
  BEGIN

    the_time := current_timestamp;
    EndXys.extend(4);
    PolyInfo_Array.extend(3);
    PolyInfo_Array(1) := 1;
    PolyInfo_Array(2) := 1003;
    PolyInfo_Array(3) := 1;


    --Matt! Added this 9/14/12
    --seeing cases in production where the sdo_TOPO_MAP.LOAD_TOPO_MAP below is erroring with
    --   java out of memory
    --Maybe theres a higher level one-time-only spot where it belongs?
    SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(2147483648);


--------------------------------------------------------------------------------
    query_str := 'SELECT max(edge_id) FROM ' || topology || '_edge$';
    execute immediate query_str into max_id;
    Done.extend(max_id);

--------------------------------------------------------------------------------
   EXECUTE IMMEDIATE 'SELECT count(1) FROM ' || Edges_table into kount;

-- Find out how many on Universal Face
    EXECUTE IMMEDIATE 'SELECT count(1) FROM ' || Edges_table || ' WHERE IGNORE=1' into no_on_UF;

-- Skip edges in Done Table. Zero means not done, 1 means done (or do not process).
    for ii in 1..max_id loop
      Done(ii) := 0.;
    end loop;

    query_str := 'SELECT  edge_id FROM ' || Done_Table;


    OPEN query_crs FOR query_str;

    LOOP
      FETCH query_crs BULK COLLECT INTO edge_ids LIMIT Row_Limit;
      EXIT WHEN edge_ids.count = 0;
      for ii in 1..Edge_ids.count loop
        edge_id := Edge_ids(ii);
        Done(edge_id) := 1.;
      end loop;
      no_done := no_done + Edge_ids.count;
    END LOOP;

    CLOSE query_crs;

--------------------------------------------------------------------------------

-- Skip edges in Skip Table
    IF Skip_Table is NOT NULL then
-- User passed a comm delimited list of edge_ids to skip in parentheses.
-- Example (1,3)
      p := INSTR(Skip_Table,'(');
      if p <> 0 then
-- We have 2 cases, a list to skip (positive)
        Skip_ids := split(Skip_Table);
        if Skip_ids(1) > 0 then
          for ii in 1..Skip_ids.count loop
            edge_id := Skip_ids(ii);
            Done(edge_id) := 1.;
          end loop;
          no_skip := no_skip + Skip_ids.count;
          query_str := 'SELECT column_value from TABLE(:7)';
          minus_statement := ' minus ' || query_str;
        else
-- or 2nd case is a list to select (negative)
          for ii in 1..Done.count loop
            Done(ii) := 1.;
          end loop;
          query_str := '(';
          for ii in 1..Skip_ids.count loop
            Skip_ids(ii) := ABS(Skip_ids(ii));
            query_str := query_str|| Skip_ids(ii);
            if ii <> Skip_ids.count then
              query_str := query_str||',';
            else
              query_str := query_str||')';
            end if;
            edge_id := Skip_ids(ii);
            Done(edge_id) :=0.;
          end loop;
          no_skip := kount- Skip_ids.count;
          minus_statement := ' AND e.edge_id in '||query_str;
          p:=0;
        end if;
-- User passed a table name
      else
      query_str := 'SELECT  Edge_id FROM ' || Skip_Table;
       GZ_TOPOFIX.Track_App(query_str,Topology,'LS');
      OPEN query_crs FOR query_str;

      LOOP

        FETCH query_crs BULK COLLECT INTO Skip_ids LIMIT Row_limit;
        EXIT WHEN edge_ids.count=0;
        for ii in 1..Skip_ids.count loop
          edge_id := Skip_ids(ii);
          Done(edge_id) := 1.;
        end loop;
        no_skip := no_skip + Skip_ids.count;
      END LOOP;

      CLOSE query_crs;

      minus_statement := ' minus ' || query_str;
      end if;
    END IF;

      error_msg := ':Edges to Skip:Line_Simplify:Edge counts '||no_on_UF||' on univ face';
    if no_skip > 0 then
      error_msg := error_msg || ', and user requests '|| no_skip || ' to skip';
    end if;

    if no_done > 0 then
      error_msg := error_msg || ', '|| no_done || ' already simplified.';
    end if;

 --   dbms_output.put_line( substr(error_msg,1,100));

    GZ_TOPOFIX.Track_App(error_msg,Topology,'LS');

    dbms_output.put_line( 'Number of edges done: '|| no_done  || ' from ' || done_table);
    dbms_output.put_line( 'Number to skip: '|| no_skip);
    dbms_output.put_line( 'Number on Universal Face: '|| no_on_UF);
--------------------------------------------------------------------------------
/*
    window_geom := SDO_GEOMETRY (2003,NULL,NULL,
                                 SDO_ELEM_INFO_ARRAY(1,1003,3),
                                 SDO_ORDINATE_ARRAY(min_x,min_y,max_x,max_y));
    IF max_x IS NULL
    THEN
      query_str := 'SELECT edge_id, geometry ' ||
                   'FROM ' || topology || '_edge$';
    ELSE
      query_str := 'SELECT edge_id, geometry ' ||
                   'FROM ' || topology || '_edge$ a, ' ||
                         'TABLE (SDO_TOPO_MAP.SEARCH_EDGE_RTREE_TOPO_MAP(''' ||
                                   topomap_name || ''',' ||
                                   min_x || ','        ||
                                   min_y || ','        ||
                                   max_x || ','        ||
                                   max_y || ',-1)) b ' ||
                   'WHERE a.edge_id = b.column_value';
    END IF;
*/


    sql_stmt2 := 'INSERT INTO ' || Done_Table || ' (EDGE_ID,STATE) VALUES(:1,:2)';


   if p <> 0 then
     sql_stmt := 'SELECT e.edge_id ' ||
                   'FROM ' || Edges_Table || ' e WHERE e.state=:1 and e.Ignore=0 and ' ||
                  'SDO_RELATE(e.geometry,MDSYS.sdo_geometry(2003,:2,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,3),'||
                 'MDSYS.SDO_ORDINATE_ARRAY(:3,:4,:5,:6)),''MASK=INSIDE'') = ''TRUE''' || minus_statement;
    dbms_output.put_line('minus ' || minus_statement);
    EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO Edge_ids USING state_code,SRID,min_x,min_y,max_x,max_y,Skip_ids;
   else
    sql_stmt := 'SELECT e.edge_id ' ||
                   'FROM ' || Edges_Table || ' e WHERE e.state=:1 and e.Ignore=0 and ' ||
                  'SDO_RELATE(e.geometry,MDSYS.sdo_geometry(2003,:2,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,3),'||
                 'MDSYS.SDO_ORDINATE_ARRAY(:3,:4,:5,:6)),''MASK=INSIDE'') = ''TRUE''' || minus_statement;

    EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO Edge_ids USING state_code,SRID,min_x,min_y,max_x,max_y;
   end if;


    If Edge_ids.count = 0 then
      RETURN 0;
    End If;
-- Sort the edge_ids ascending. We must have a repeatable order that is defined.
-- For example:
-- If we process 2 curved river edges, then if we process the top one first, it must
-- not interfere or intersect with the lower one. So the shape is well preserved.
-- If we process the lower one first, it is free to cut across missing the top
-- one altogether. Then both edges can have much straighter shapes.

    shellsort(Edge_ids);


--    sql_stmt4 := 'INSERT INTO ' ||BAD_Table || ' VALUES(:1,:2,:3)';
    sql_stmt3 := 'UPDATE ' || Edges_Table || ' set new_geometry=:1 where edge_id=:2';

    sql_stmt := 'SELECT e.new_geometry,m.start_node_id,m.end_node_id,e.uniq,e.mt_length,e.vertices ' ||
                   'FROM ' || Topology ||'_edge$ m,' || Edges_table || ' e where m.edge_id=:1 and m.edge_id=e.edge_id';


--    dbms_output.put_line('edge ids count ' || edge_ids.count);
--------------------------------------------------------------------------------
--dbms_output.put_line('Elapsed time : ' || (current_timestamp - the_time));
  no_done := 0;
   FOR Loups in 1..2 LOOP
  -- On the 2nd loop we process the "bad" ids individually
--     If Edge_ids.count < 500 then
     batch := Edge_ids.count;
--     end if;
     If loups = 2 then
        Edge_ids := Bad_ids;
        shellsort(Edge_ids);
        batch := Bad_ids.count;
        -- Mark these as not done
--        for jj in 1..Bad_ids.count Loop
--            Done(bad_ids(jj)) := 0.;
--        end loop;
        idx := 0;
        single_edge_mode := TRUE;
        if Bad_ids.count > 0 then
          error_msg := ':SingleEdge Mode:Line_Simplify: Bad ids to process '|| Bad_ids.count;
          GZ_TOPOFIX.Track_App(error_msg,Topology,'LS');
        end if;
     end if;
-- Loop over all edges that are not done

    loup_count := 0;

    WHILE loup_count < batch LOOP   -- we do batches of edge_ids
      if loup_count = 0 then
        if clear_it = 0 and single_edge_mode = FALSE then
--          dbms_output.put_line('loading whole state' );
          if once <> 'TRUE' then
            error_msg := ':Load TopoMap:Line_Simplify:Topology reloading with MBR, LL:'|| min_x || ' : ' || min_y||' UR: '|| max_x || ' : ' || max_y;
            GZ_TOPOFIX.Track_App(error_msg,Topology,'LS');
            BEGIN
            sdo_TOPO_MAP.LOAD_TOPO_MAP(topomap_name,min_x,min_y,max_x,max_y, 'true');

            EXCEPTION
            WHEN OTHERS  then
              sdo_TOPO_MAP.DROP_TOPO_MAP(topomap_name);

              IF UPPER(SQLERRM) LIKE '%MEMORY%' THEN
                 error_msg := ':Load TopoMap:Line_Simplify:Topology reload fail with MBR, LL:'|| min_x || ' : ' || min_y||' UR: '|| max_x || ' : ' || max_y;
                 GZ_TOPOFIX.Track_App(error_msg,Topology,'LS');

              ELSE
              dbms_output.put_line(sqlerrm);   -- but this does!!!

                error_msg := 'FATAL ERROR:Line_Simplify: Problem Loading TopoMap:' || sqlerrm;
                GZ_TOPOFIX.Track_App_Error(error_msg,Topology,'LS');
              END IF;
           END;
          end if;
          once := 'FALSE';
          clear_it := 1;
        end if;
        last_idx := idx;
        if idx + batch > Edge_ids.count then
          batch := Edge_ids.count - idx;
        end if;
        low_id := Edge_ids(idx+1);
        hi_id := Edge_ids(idx+batch);
--         dbms_output.put_line('L ' || low_id || ' H ' || hi_id);
      end if;
      loup_count := loup_count +1;
      idx := idx + 1;
      edge_id := Edge_ids(idx);
--      if mod(idx,200) =1 then
--      if edge_id < 224 then
--      dbms_output.put_line('L ' || loup_count || ' ' || edge_id || ' D ' || done(edge_id));
--      end if;
--      edge_id :=1760;
--     edge_id := 1554569; --1760; --1129513; --765744; --11381; --1220221; --2109193; --85113; --624950; --970887; --1298; --970058; --1219656; --62781; --1138750; --1997295; --772282; --2165828; --472958;

      geometry := NULL;
      execute immediate sql_stmt into geometry,start_node,end_node,uniq,elength,vertices using edge_id;

--  Two vertices cannot be generalized, so the edge is done.
      IF vertices = 2 THEN
          execute immediate sql_stmt2 using edge_id,state_code;

-- If the Edge has not been already generalized and it has more than 2 vertices
--  then process it

      ELSIF Done(edge_id) = 0 THEN

           if single_edge_mode then
--           dbms_output.put_line(edge_id);

             sql_stmt5 := 'SELECT xLL,yLL,xUR,yUR from '|| Edges_Table || ' where edge_id=:1';
             Execute Immediate sql_stmt5 into xLL2,yLL2,xUR2,yUR2 using edge_id;

             xLL2 := xLL2-delta;
             yLL2 := yLL2-delta;
             xUR2 := xUR2+delta;
             yUR2 := yUR2+delta;
             if clear_it = 1 then
               sdo_TOPO_MAP.CLEAR_TOPO_MAP(topomap_name);
               clear_it := 0;
             end if;
             error_msg := ':Load TopoMap:Line_Simplify:Single Edge reloading with MBR, LL:'|| xLL2 || ' : ' || yLL2||' UR: '|| xUR2 || ' : ' || yUR2;
            GZ_TOPOFIX.Track_App(error_msg,Topology,'LS');
             sdo_TOPO_MAP.LOAD_TOPO_MAP(topomap_name,xLL2,yLL2,xUR2,yUR2, 'true');
             clear_it := 1;
           end if;

-- First Simplify the edge, and then test if its good.

           execute immediate sql_stmt2 using edge_id,state_code;
           commit;
           origXyord := geometry.sdo_ordinates;
           scale := pscale;


-- Treat very small polygons or edges that are part of a small polygon
-- differently since they may not generalize at all. For example, how do you
-- generalize a square at some arbitrary scale? The chance you can do better
-- is not good and sometimes it is easier to just use the ungeneralized or
-- a very mildly generalized version of the geometry.

           if elength = 0. then
             elength := GZ_UTIL_ZONE.accurate_length(origXYOrd);
           end if;
           if elength < length_cutoff then
             m := origXYord.count;
             if origxyord(1) = origxyord(m-1) and
               origxyord(2) = origxyord(m) then
               scale := ROUND(elength*200.,0);
               if scale < 0.8*pscale then
                 scale := pscale*0.8;
               end if;
--               dbms_output.put_line('scale ' || scale);
             else
/*
             PolyXys.trim(Polyxys.count);
             PolyXys.extend(m+2);
             For jj in 1..origXyord.count Loop
               PolyXys(jj) := origXYord(jj);
             end loop;
             PolyXys(m+1) := Polyxys(1);
             PolyXys(m+2) := Polyxys(2);
             area := area_perim(PolyXys,perim);
--  A triangle with 2 sides L and included angle of 90 degrees has area L*L/2
--  Perimeter is 2L. so sqrt(area/perim) = sqrt(L*L/2)/2L.
             dbms_output.put_line('a2/P ' || (sqrt(area)/perim) || ' a ' ||area || ' p ' || perim);
             if perim > 0. and sqrt(area)/perim > 0.35355 then
*/
               EndXys(1) := origxyord(1);
               EndXys(2) := origxyord(2);
               EndXys(3) := origxyord(m-1);
               EndXys(4) := origxyord(m);
               short_dist := GZ_UTIL_ZONE.accurate_length(EndXys);
--             dbms_output.put_line('elen ' || elength || ' short ' || short_dist);
-- Here we reset the scale for very short edges that will not
               if short_dist < 0.3 * elength or uniq >= 8 then
                 scale := ROUND(elength*200.,0);
               end if;
               if scale < 0.8*pscale and uniq < 8 then
                 scale := pscale*0.8;
               end if;
--                dbms_output.put_line('scale now' || scale);
             end if;

             if scale > pscale then
               scale := pscale;
             end if;
 --             dbms_output.put_line('scale final ' || scale);
           end if;

           OK := TRUE;
           result := 'BAD';
           loops := 0;
           new_geometry := NULL;
           WHILE result <> 'TRUE' and loops < 1 LOOP

--             dbms_output.put_line('calling edge simplify scale is ' || scale || ' elength ' || (200*round(elength,4)));
             new_geometry := EDGE_SIMPLIFY(Status,Edge_id,geometry,elength,uniq,scale,nice,tolerance,
                                      dec_digits,method,Edges_Table,topology);

             IF new_geometry is not NULL THEN
               xyord := geometry.sdo_ordinates;
               SRID := Geometry.sdo_srid;
--        dbms_output.put_line('vertex kount ' || xyord.count || ' new count ' || new_geometry.sdo_ordinates.count);

-- If the edge closes then we test to see if it self intersects using ORACLE
               if xyord(1) = xyord(XYord.count-1) and xyord(2) = xyord(XYord.count) then

                  poly_geometry := MDSYS.SDO_GEOMETRY(2003,SRID,NULL,SDO_ELEM_INFO_ARRAY(1,1003,1),
                               new_geometry.sdo_ordinates);
                  result := sdo_geom.validate_geometry_with_context(poly_geometry,0.05);

                  if (INSTR(result, '13367 [Element') != 0) then
                    polyXys := new_geometry.sdo_ordinates;
                    GZ_UTIL_ZONE.reverse_ordinates(PolyXys);
                    poly_geometry.sdo_ordinates := PolyXys;
                    result := sdo_geom.validate_geometry_with_context(poly_geometry,tolerance);
--                dbms_output.put_line('new result is' || result);
                  end if;

               else
-- Check for self intersection.
                  self_segs := check_for_self_intersect(new_geometry);
--           if edge_id = 2220515 then
--      dbms_output.put_line(edge_id || ' new ' || new_geometry.sdo_ordinates.count);
--      end if;
--           dbms_output.put_line('SEG is ' || seg1);

-- If it does make a similar 13349 error message.
                 if self_segs.count > 0 then
                   seg2 := MOD(self_segs(1),100000.);
                   seg1 := TRUNC((self_segs(1)-seg2)/100000.);
                   result := '13349 [Element <1>] [Ring <1>][Edge <'||seg1||'>][Edge <'||seg2||'>]';
                   dbms_output.put_line(edge_id || ' self intersected ' || result);
--                   sql_stmt := 'INSERT into points values(:1,:2,:3)';
--                   execute immediate sql_stmt using -19,new_geometry,'generalized geom';
--                   commit;
                 else
-- We are done, its good.
                   result := 'TRUE';
                 end if;
               end if;

             END IF;
             loops := loops+1;
           END LOOP;

--------------------------------------------------------------------------------
           IF new_geometry is not NULL THEN
              xyord := new_geometry.sdo_ordinates;
             if result <> 'TRUE' then
               OK := FALSE;

               error_msg := ':WARNING'||edge_id||':Line_Simplify:Updated edge failed insertion into topology with ERROR:'||result;
               GZ_TOPOFIX.Track_App(error_msg,Topology,'LS',new_geometry);
--               for pp in 1..TRUNC(Xyord.count/2) loop
--                 dbms_output.put_line('X:' || Xyord(pp*2-1) ||','||Xyord(pp*2));
--               end loop;
--               execute immediate sql_stmt4 using edge_id,state_code,result;
             end if;

--   dbms_output.put_line(result);


--  Record that the edge is done.



             IF OK = TRUE THEN
               status := swap_edge_coords(Topology,edge_id,new_geometry);
--               dbms_output.put_line(edge_id || ' ' ||SUBSTR(status,1,100));
               if status <> 'TRUE' then
                 dbms_output.put_line(SUBSTR(status,1,100));
               else
                 no_done := no_done+1;
               end if;
-- If we short-circuit a very small polygon with a straight edge and get the Oracle error
-- "Changing edge coordinates attempts to reorder node star at node nnnnnn"
--or "Changing a loop's coordinates reverses the sense of the loop"

-- then just leave the original edge!

               if (INSTR(status, 'Changing edge coordinates attempts to reorder node star') != 0 or INSTR(status, 'coordinates reverses the sense of the loop') != 0)  then
-- We mark it as done even though it was not generalized..
                  error_msg := ':WARNING'||edge_id||':Line_Simplify:Updated edge failed insertion into topology with ERROR:'||status;
                 GZ_TOPOFIX.Track_App(error_msg,Topology,'LS',new_geometry);
                 Done(edge_id) := 1.;
                 execute immediate sql_stmt2 using edge_id,state_code;
                 commit;
               elsif SUBSTR(status,1,4)  <> 'TRUE' then

                 DBMS_OUTPUT.PUT_LINE('Unable to Change Edge Coords: ' || Edge_id || ': ' || status);
                 if status <> 'NOT SWAPPED' then
                  error_msg := ':WARNING'||edge_id||':Line_Simplify:Updated edge failed insertion into topology with ERROR:'||status;
                 GZ_TOPOFIX.Track_App(error_msg,Topology,'LS',new_geometry);
--                 execute immediate sql_stmt4 using edge_id,state_code,status;
                 commit;
                 dbms_output.put_line(status);
                 else
-- Record an edge that Oracle would not swap as complete but has a NOT SWAPPED status in Bad_Edges
                   Done(edge_id) := 1.;
                   execute immediate sql_stmt2 using edge_id,state_code;
                   error_msg := ':WARNING'||edge_id||':Line_Simplify:Updated edge failed insertion into topology with ERROR:'||status;
                 GZ_TOPOFIX.Track_App(error_msg,Topology,'LS',new_geometry);
--                   execute immediate sql_stmt4 using edge_id,state_code,status;
                   commit;
                 end if;

               else
-- This is sucess
-- This is the geometry we placed into the Topology
                 sql_stmt3 := 'UPDATE ' || Edges_Table || ' set new_geometry=:1 where edge_id=:2';
                 execute immediate sql_stmt3 using new_geometry,edge_id;
                 Done(edge_id) := 1.;
                 execute immediate sql_stmt2 using edge_id,state_code;
                 commit;
               end if;



             END IF;  -- end of edge was 'OK'

         END IF;  -- end of if new_geometry is not null
      END IF;   -- end of when there are > 2 vertices and edge is not done
               if (single_edge_mode and idx <> Edge_ids.count) or
                  (single_edge_mode = FALSE and loup_count=batch) then
--                  dbms_output.put_line('readdy to committt');
               BEGIN
--               dbms_output.put_line('calling commit_topo_map');
                 sdo_TOPO_MAP.COMMIT_TOPO_MAP();
--                 dbms_output.put_line('committed');
                 COMMIT;
--                 sql_stmt7 := 'SELECT count(1) from '||Topology||'_EDGE$ a, GZ_SDRP10_Z6.Z641SP1_edge$ b where a.edge_id = b.edge_id and (a.left_face_id <> b.left_face_id or a.right_face_id <> b.right_face_id)';
--                 execute immediate sql_stmt7 into bad_count;
--                 dbms_output.put_line('Bad count ' || bad_count || ' at id ' || edge_id);
--                 if bad_count > 0 then
--                   raise_application_error(-20001,'BAD');
--                 end if;
                 EXCEPTION
                 WHEN OTHERS THEN
                 dbms_output.put_line('at ' || edge_id );
                 dbms_output.put_line(sqlerrm);
                 IF (INSTR(sqlerrm, 'has an edge coincident with outer boundary') != 0) THEN
                  Get_bad_ids(Topology,state_code,Edges_table,sqlerrm,Bad_ids);

-- Mark those done as not done
                  for jj in last_idx+1..idx Loop
                    Done(Edge_ids(jj)) := 0.;
                  end loop;
                  sql_stmt6 := 'Delete from ' || Done_table || ' WHERE STATE=:1 and EDGE_ID >=:1 and EDGE_ID <=:2';
                  Execute Immediate sql_stmt6 using state_code,low_id,hi_id;
                  sql_stmt6 := 'UPDATE '||Edges_Table || ' SET new_geometry = geometry WHERE state=:1 and edge_id >=:2 and edge_id <=:3';
                  Execute Immediate sql_stmt6 using state_code,low_id,hi_id;
                  dbms_output.put_line('Reset geometries back to original for ids: ' || low_id || ' to ' || hi_id);
                  commit;
-- Mark these as done
                  for jj in 1..Bad_ids.count Loop
                    Done(bad_ids(jj)) := 1.;
                  end loop;
                  loup_count := batch;
                  idx := last_idx;
                  status := sqlerrm;
                   error_msg := ':WARNING'||edge_id||':Line_Simplify:Updated edge failed insertion into topology with ERROR:'||status;
                  GZ_TOPOFIX.Track_App(error_msg,Topology,'LS',new_geometry);
--                  execute immediate sql_stmt4 using edge_id,state_code,status;
                  COMMIT;
                 END IF;
                END;
--                dbms_output.put_line('cleared topo map');
                sdo_TOPO_MAP.CLEAR_TOPO_MAP(topomap_name);
                clear_it := 0;
               end if;
      if Mod(loup_count,5000) = 1 then
         dbms_session.free_unused_user_memory;
      end if;
      if loup_count = batch and idx < Edge_ids.count then
         loup_count := 0;
      end if;

    END LOOP;
    END LOOP;   -- end of Loop over loops

    RETURN no_done;

  END SIMPLIFY_REGION;
  --
PROCEDURE SET_MBR(   XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0) AS
--==============================================================================
--Program Name: SET_MBR
--Author: Sidey Timmins
--Creation Date: 7/25/08
--Updated: 11/04/2010 To go through the array in different directions

--Usage: Set_MBR(Xys,xLL,yLL,xUR,yUR);

  --             XYs:             the input array of xy coordinates
  --             pLB, pUB  :      elements caller may specify describing the
  --                              range in the ordinate array to use. (Set Many MBR
  --                              uses these parameters to set up many extents
  --                              for a single edge. These are used to sudivide it.)
--   Output      xLL,yLL,xUR,yUR:  the returned MBR (Lower left, Upper right).
--
--                ---------+ (xUR,yUR)
--                |        |
--                |        |
--      (xLL,yLL) +--------+
--
--Purpose: Determines the minimum bounding rectangle (MBR) of a geometry. It is
--         about 10% faster than Oracle sdo_geom.sdo_mbr.
-- Method: May loop through the coordinates twice using a coarse comb to first
-- check values far apart bedore checking every vertex.
--==============================================================================

   n       PLS_INTEGER := Xys.count;
   ii      PLS_INTEGER;
   LB      PLS_INTEGER := pLB;
   UB      PLS_INTEGER := pUB;
   inc     PLS_INTEGER;
   mid     PLS_INTEGER;
   loops   PLS_INTEGER :=2;
   m       PLS_INTEGER;

BEGIN
--------------------------------------------------------------------------------
-- Setup to get initial values for the returned extent.

   If UB = 0 then UB := n; End if;

-- We want the loop below to just do comparisons and not unnecessary stores.
-- Since we start at the beginning (below), check the midpoint and the end here.

   mid := TRUNC((LB+UB)/2);
   if TRUNC(mid/2) *2 <> mid then mid := mid +1; end if;

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

   if n <=4 then RETURN; end if;  -- Done when we have a 2 vertex edge.
--------------------------------------------------------------------------------
-- Loop twice using a comb to avoid a lot of wasted loads and stores. Usually
-- begind with a coarse comb before checking every value.
-- Set thiscomb increment

   inc := sqrt(UB - LB +1);

   if TRUNC(inc/2)*2 <> inc then inc := inc + 1; end if;

-- for less than 400 coordinates, one loop (brute force) seems fastest
   if inc <= 20 then
      inc := 2;
      loops := 1;
   elsif inc > 40 then
      inc := inc * 2;
   End if;

--------------------------------------------------------------------------------
-- Begin 2 or 1 loops (sort of like Shellsort). First loop uses coarse comb
-- and second uses a fine comb to check every vertex.


   For jj in 1..loops Loop
     m := TRUNC((UB-LB+1)/inc);
     inc := inc -1;
     if loops = 2 and jj = 1 then
       ii := UB + inc-1;
       inc := -inc;
     else
       ii := pLB  - inc;
     end if;
--dbms_output.put_line('inc was ' ||inc);
--   Loop of the coordinates. Most of the time this loop doesn't do anything
--   except additions and tests.

     For i in 1..m Loop
       ii := ii + inc;

       If Xys(ii) < xLL then          -- it is believed that this if .. elsif
          xLL := Xys(ii);
       ElsIf Xys(ii) > xUR then
          xUR := Xys(ii);
       End If;
       ii := ii +1;

       If Xys(ii) < yLL then          -- structure is missing from the comparable Oracle code!
         yLL := Xys(ii);
       ElsIf Xys(ii) > yUR then
         yUR := Xys(ii);
       End If;
     End Loop;
     inc := 2;
--   dbms_output.put_line('LL ' || xLL || ' y ' || yLL || ' UR ' || xUR || ' y ' || yUR);
   End Loop;
--------------------------------------------------------------------------------
END SET_MBR;
--
PROCEDURE SET_MANY_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                      MBRs IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0,
                      pbeg PLS_INTEGER default 0)  AS

--==============================================================================
--Program Name: set_many_mbr
--Author: Sidey Timmins
--Creation Date: 11/03/08
--Updated: 12/02/2008 To handle holes in polygons
--Usage:

  --             XYs:      the input array of xy coordinates
  --             Info_Array: the Elem Info Array from the geometry
  --             MBRs:     an output array to be filled with MBRs
  --                       (groups of 6, xLL,yLL,xUR,yUR followed by
  --                       the range for each MBR - start/stop elements)
  --             pLB, pUB  :     (lower bound and upper bound)- elements caller
  --                              must specify to start and stop at in the
  --                              ordinate array. pLB defaults to 1
  --                              and pUB defaults to Xys.count
  --             pbeg: where to start storing in MBR minus 1 (usually zero). Only
  --                   non-zero when you want to store mbrs for multiple
  --                   polygons within the same MBR array. You keep track
  --                   of where they begin and end!
  --
--   Output        xLL,yLL,xUR,yUR  the returned extent (Lower left, Upper right).
--
--                ---------+ (xUR,yUR)
--                |        |
--                |        |
--      (xLL,yLL) +--------+              NOTE: Extents are a pair of points.

--
--Purpose:
--          Takes an XY ordinate array and produces many MBRs (Minimum Bounding
--          Rectangles or boxes) and the overall extent (xLL,yLL) to (xUR,yUR)
--          spanning the coordinates. These MBRS enable a geometry to be subdivided.
--                              MBRs                    geometry
--                        ___________________         ___________
--                       |   |___________|   |       /           \
--                       |   |           |   |      /             \
--                       |___|___________|___|     /               \
--                       |________|          |     --------         |
--                                |__________|              --------
--         This function determines many touching MBRs around the perimeter of
--         a geometry as shown. These MBRs can be used to limit xy searches.
--         very efficiently If the LB is not equal to 1 then the Info Array is
--         ignored and MBRs are determined using pLB and pUB.

--Dependencies: SET_MBR
--==============================================================================

     xLL2            NUMBER;
     yLL2            NUMBER;
     xUR2            NUMBER;
     yUR2            NUMBER;
     LB              PLS_INTEGER := pLB;
     UB              PLS_INTEGER := pUB;
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

--------------------------------------------------------------------------------
--  Setup the number of MBRs to do and their subscript ranges

    If UB = 0 then UB := XYs.count; end If;
    InUB := UB;
    If LB = 1 then no_holes := TRUNC(Info_Array.count/3) -1; End if;
    If no_holes > 0 then
       next := 4;
       UB := Info_Array(4) -1;
    End if;
    m := TRUNC(sqrt((InUB-LB+1)/2));

    if m <= 0 then
       m := 1;
    end if;
-- To get MBRs rectangles touching we have to reuse the last vertex as the
-- next start vertex (hence -2).
        no_to_do := TRUNC((InUB-LB+1)/m);
--  dbms_output.put_line('noto do' || no_to_do || ' inub ' ||inub || ' lb ' || LB);
    if TRUNC(no_to_do/2)*2 <> no_to_do then
      no_to_do := no_to_do +1;
--          dbms_output.put_line('NOW noto do' || no_to_do );
    End if;

    if no_to_do = 2 then
      m := 1;
    else
      if m*(no_to_do-2)+2 < InUB-LB+1 then   -- this is new - and next 2 lines
        no_to_do := no_to_do + 2;
      end if;
----          m := TRUNC((InUB-LB+1)/(no_to_do -2));
      If (m* (no_to_do-2)) +2 < (InUB-LB+1) then  -- +2 is new
        no_to_do := no_to_do + 2; -- m := m + 1;
      End If;
    end if;

    mm := m + no_holes;

    len_required := mm*6 +6;
    If (MBRs.count - pbeg) < len_required then
      If len_required < 1000 then
        len_required := 1000;
      End if;
      MBRs.extend(len_required);
    End If;
    LBB := LB;
--         dbms_output.put_line('MM ' || mm || ' holes ' || no_holes || ' no to do ' || no_to_do || ' count ' || Xys.count || 'inub ' || InUB);
--------------------------------------------------------------------------------
--      For each partial MBR along/around the edge

      For ii in 1..mm Loop
        UBB := LBB + no_to_do-1;
        If UBB > UB then
           UBB := UB;
        End if;
--      Do a partial MBR and then save its extent and the range irt covers
        Set_MBR(XYs,xLL2,yLL2,xUR2,yUR2,LBB,UBB);

--          dbms_output.put_line('II ' || ii || ' LBB ' || LBB || ' UBB ' || UBB || ' count ' ||Xys.count || ' UB ' || UB);
--          dbms_output.put_line('II ' || ii || ' xLL ' || XLL2 || ' yLL ' || YLL2);
--          dbms_output.put_line('II ' || ii || ' xUR ' || XUR2 || ' yLL ' || YUR2);
        jj := ii*6+pbeg;
        MBRs(jj+1) := xLL2; -- partial extent
        MBRs(jj+2) := yLL2;
        MBRs(jj+3) := xUR2;
        MBRs(jj+4) := yUR2;
        MBRs(jj+5) := LBB;   -- start range
        MBRs(jj+6) := UBB;   -- end range
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
--            exit when UBB >= UB;
          LBB := UBB-1;
        End if;

      End Loop;
      -- Save at the beginning, the overall MBR and no of MBRs/group size
      MBRs(1+pbeg) := xLL;        -- Overall MBR (extent) of this data
      MBRs(2+pbeg) := yLL;
      MBRs(3+pbeg) := xUR;
      MBRs(4+pbeg) := yUR;
      MBRS(5+pbeg) := m;          -- the number of MBRs
      MBRS(6+pbeg) := no_to_do;   -- the number of coordinates for each MBR

END SET_MANY_MBR;
--
FUNCTION SWAP_EDGE_COORDS(Topology VARCHAR2,Edge_id NUMBER, Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY) RETURN VARCHAR2 AS

--------------------------------------------------------------------------------
-- Program Name: swap_edge_coords
-- Author: Sidey Timmins
-- Creation Date: 2010
--
-- Usage:
--     REQUIRED Parameter:
--        Topology            - The name of the Topology.
--        Edge_id             - The edge_id to reshape
--        Geometry            - the revised geometry to replace the existing one
-- Purpose:       Update a topology by reshaping a single edge
-- Modification History:
--       10/21/2010 Niranjan Reddy suggested a different entry point with 3 extra
--                   arguments to avoid Oracle bug SR 3-2169673431
--------------------------------------------------------------------------------

     status            VARCHAR2(4000);
     v_errm            VARCHAR2(4000);
     v_code            NUMBER;
     moved_iso_nodes   SDO_NUMBER_ARRAY;
     moved_iso_edges   SDO_NUMBER_ARRAY;
     allow_iso_moves   VARCHAR2(5) := 'FALSE';

BEGIN
--------------------------------------------------------------------------------
 --   In the specified topology, replace one edge's geometry with the input geometry.
 --   Perform the swap within an exception begin ..end clause

      BEGIN

        SDO_TOPO_MAP.CHANGE_EDGE_COORDS(NULL,EDGE_ID,Geometry,moved_iso_nodes,moved_iso_edges,allow_iso_moves);

--      Very rarely Oracle reports a moved iso nodes count > 1

        if moved_iso_nodes.count > 0 or moved_iso_edges.count > 0 then
           RETURN 'NOT SWAPPED: moved iso nodes or moved iso_edges > 0';
        end if;

--      If we get here the edge was swapped.

        RETURN 'TRUE';

        EXCEPTION       --      If there is an error then return the status
          WHEN OTHERS THEN
                IF (INSTR(sqlerrm, 'with an edge ID that does not exist in cache') != 0) THEN
                  DBMS_OUTPUT.PUT_LINE('in handler' || substr(sqlerrm,1,100));
                ELSE
                  v_code := SQLCODE;
                  v_errm :=  SQLERRM;

                END IF;
                RETURN SUBSTR(sqlerrm,1,300);  -- error message can be long
       END;
--------------------------------------------------------------------------------
END SWAP_EDGE_COORDS;
--
PROCEDURE Add_geom_metadata(pTableName VARCHAR2,pColumnName VARCHAR2,pInSchema VARCHAR2 default NULL,SRID NUMBER default 8265.,xLL NUMBER default -180.,yLL NUMBER default -90.,xUR NUMBER default 180.,yUR NUMBER default 90.,tolerance NUMBER default 0.05) AS

--==============================================================================
-- Program Name: add_geom_metadata_8265
-- Author: Nick Padfield. Creation Date: 5/31/2006
-- Updated: 10/20/2010 Sidey Timmins 1) to handle other SRIDs when caller knows the extent
--                     and 2) not add dummy entries for geometry columns that don't exist!!
--                     Revised error messages and tolerance.

-- Usage: exec Add_geom_metadata('My_Table','GEOMETRY');

     --   pTableName      - a table name
     --   pColumnName     - a column name in that table to be registered with
     --                     the USER_SDO_GEOM_METADATA table
     --   pInSchema       - the Schema name (defaults to user)
     --   SRID            - the Spatial reference id
     --   xLL,yLL,xUR,yUR - the maximum extent of coordinates for this SRID
     --   tolerance       - Oracle tolerance

-- Purpose: This procedure registers an existing spatial column with
--          the USER_SDO_GEOM_METADATA table.

-- Dependencies: Table_exists, Column_exists
--==============================================================================

   RowsSelected   PLS_INTEGER := 0;
   InSchema       VARCHAR2(30) := UPPER(NVL(pInSchema,USER));
   TableName      VARCHAR2(30) := UPPER(pTableName);
   ColumnName     VARCHAR2(30) := UPPER(pColumnName);
   sql_stmt       VARCHAR2(4000);
   xname          VARCHAR2(20) :='X';
   yname          VARCHAR2(20) :='Y';
   RecordCount    NUMBER;
BEGIN
 -------------------------------------------------------------------------------

-- Check to see if the COLUMN exists!

   IF Table_exists( Tablename , InSchema) then

      If NOT Column_exists(ColumnName,TableName,InSchema) then
        dbms_output.put_line('WARNING: the '||ColumnName||' column does not exist in the '||InSchema||'.'||TableName||' table');
        RETURN;
      End if;

   -- Check to see if this geometry is already registered

     SELECT COUNT(rownum)
             INTO RowsSelected
             FROM user_sdo_geom_metadata
             WHERE table_name = TableName AND column_name = ColumnName;

     IF (RowsSelected > 0) THEN
        dbms_output.put_line('There were ' || RowsSelected || ' pre-existing records found in the USER_SDO_GEOM_METADATA table.');
        DELETE FROM user_sdo_geom_metadata
                WHERE table_name = TableName AND column_name = ColumnName;
        COMMIT;
        dbms_output.put_line('These pre-existing records were deleted');
     END IF;

   -----------------------------------------------------------------------------
   -- Add a record to the system metadata table
   --Additional notes about insertion:
   --If v_SRID is the geodetic coordinate system (ie. 8265) then v_Xmin must equal -180,
   --v_Xmax must equal 180, v_Ymin must equal -90, and v_Ymax must equal 90.  Additionally,
   --the units for the tolerance for geodetic SRID's is METERS, so a tolerance value of
   --0.05 is precise to 5 centimeters.

     RowsSelected := 0;
     IF SRID = 8265 THEN
        xname := 'Longitude';
        yname := 'Latitude';
     END IF;
     INSERT INTO USER_SDO_GEOM_METADATA
        (TABLE_NAME, COLUMN_NAME, DIMINFO, SRID)
        VALUES (
                 TableName,
                 ColumnName,
                 MDSYS.SDO_DIM_ARRAY (
                            MDSYS.SDO_DIM_ELEMENT(xname,xLL,xUR,tolerance),
                            MDSYS.SDO_DIM_ELEMENT(yname,yLL,yUR,tolerance)),
                            SRID);
     COMMIT;
   -----------------------------------------------------------------------------
   -- Check to see if the geometry was registered correctly
     SELECT COUNT(rownum)
             INTO RowsSelected
             FROM user_sdo_geom_metadata
             WHERE table_name = TableName AND column_name = ColumnName;

     IF (RowsSelected = 1) THEN
        dbms_output.put_line(TableName || '.' || ColumnName || ' was successfully registered!');
     ELSE
        dbms_output.put_line('WARNING: ' || TableName || '.' || ColumnName || ' was NOT successfully registered!');
     END IF;
   End If;  -- Do nothing if the Table did not exist
   -----------------------------------------------------------------------------
END Add_geom_metadata;
--
FUNCTION Index_Exists(pInTable VARCHAR2, pInColumn VARCHAR2, pInSchema VARCHAR2 DEFAULT NULL) RETURN BOOLEAN AS
/**
 ################################################################################
 # Program Name: Index_Exists
 # Author: Nick Padfield
 # Creation Date: 2/23/2008
 #
 # Usage:
 #     REQUIRED Parameter:
 #        pInTable              - Table name to check to see if it has an index.
 #        pInColumn             - Column name to check if it is indexed.
 #
 #     OPTIONAL Parameters:
 #        pInSchema             - The Schema that pInTable resides in.
 #                                This value is used to query the TABLE_OWNER column
 #                                of table ALL_IND_COLUMNS.  If pInschema is NULL,
 #                                then the current schema will be used.
 #
 # Purpose:  Checks to see if a particular column in a given table is indexed.
 #           Returns TRUE:  if the table/column is indexed and
 #                   FALSE  otherwise.
 #
 # Dependencies:
 #   Dependent upon the ALL_USERS,ALL_OBJECTS (Table Exists) ,
 #                      ALL_TAB_COLUMNS (Column_Exists)ALL_IND_COLUMNS tables.
 #
 # Modification History: Updated to call Table_Exists and Column_Exists
 #
 ################################################################################
*/
   InTable       VARCHAR2(30)     := UPPER(pInTable);
   InColumn      VARCHAR2(30)     := UPPER(pInColumn);
   InSchema      VARCHAR2(30)     := UPPER(NVL(pInSchema,USER));
   RecordCount   NUMBER(22)       := 0.;
   Warning       VARCHAR2(44)     := 'WARNING: '||InSchema;
   sql_stmt      VARCHAR2(4000);
BEGIN
   ------------------------------------------------------

   -- Check to see if: the USER exists

   sql_stmt := 'SELECT COUNT(*) FROM all_users WHERE username = :1';
   EXECUTE IMMEDIATE sql_stmt INTO RecordCount USING InSchema;
   IF (RecordCount = 0.) THEN
      dbms_output.put_line(Warning||' schema does not exist');

   --                 the TABLE exists!

   ELSIF NOT Table_exists( InTable , InSchema) then
      dbms_output.put_line(Warning||'.'||InTable||' table does not exist');

   --                the COLUMN exists!

   ELSIF NOT Column_exists( InColumn, InTable , InSchema) then
      dbms_output.put_line(Warning ||'.'||InTable|| ' does not have a column named '||InColumn);

   --            and if the INDEX exists!
   ELSE
      sql_stmt := 'SELECT COUNT(*) FROM all_ind_columns WHERE table_owner = :1 AND table_name = :2 AND column_name = :3';
      EXECUTE IMMEDIATE sql_stmt INTO RecordCount USING InSchema,InTable,InColumn;
   END IF;

   RETURN (RecordCount > 0.);
  ------------------------------------------------------
END Index_exists;
--
FUNCTION COLUMN_EXISTS ( pInColumn  IN VARCHAR2, pInTable  IN VARCHAR2, pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN AS

-- Purpose: Checks to see if a table contains a particular column.
-- Author: Nick Padfield
-- Method: Checks to see if the specified column exists in ALL_TAB_COLUMNS.

   InColumn      VARCHAR2(30)     := UPPER(pInColumn);
   InTable       VARCHAR2(30)     := UPPER(pInTable);
   InSchema      VARCHAR2(30)     := UPPER(NVL(pInSchema,USER));
   RecordCount   NUMBER(22)       := 0.;
   sql_stmt      VARCHAR2(4000);

BEGIN
  ------------------------------------------------------
   IF Table_exists( pInTable , pInSchema) then
     sql_stmt := 'SELECT COUNT(*) FROM all_tab_columns WHERE owner = :1 AND table_name = :2 AND column_name = :3';
     EXECUTE IMMEDIATE sql_stmt INTO RecordCount USING InSchema,InTable,InColumn;
   END IF;

   RETURN (RecordCount > 0.);  -- If column exists, return TRUE, if not, return FALSE.
  ------------------------------------------------------
END COLUMN_EXISTS;
--
FUNCTION TABLE_EXISTS ( pInTable  IN VARCHAR2, pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN AS

-- Purpose: Checks to see if a table exists (not that it contains records!)
-- Author:  Nick Padfield. Creation Date: 9/6/2006  Taken from the cartodb
-- Method:  Checks to see if the Table specified exists in ALL_OBJECTS.
--          Updated to not place NVL in SQL statement and use bind variables.

   InTable       VARCHAR2(30)     := UPPER(pInTable);
   InSchema      VARCHAR2(30)     := UPPER(NVL(pInSchema,USER));
   RecordCount   NUMBER(22)       := 0.;
   sql_stmt      VARCHAR2(4000);
   BEGIN

      If pInTable is NULL then   -- Table name may not be NULL. new 06/06/2012 Sidey
         RETURN FALSE;
      end if;
      ------------------------------------------------------
      -- Check to see if the table exists in ALL_OBJECTS

      sql_stmt := 'SELECT COUNT(*) FROM all_objects WHERE object_type = :1 AND owner = :2 AND object_name = :3';
      EXECUTE IMMEDIATE sql_stmt INTO RecordCount USING 'TABLE',InSchema,InTable;

      RETURN (RecordCount > 0.);  -- If table exists, return TRUE, if not, return FALSE.
      ------------------------------------------------------
   END TABLE_EXISTS;
--
  procedure test_find_intersection_segment as

  geom1    mdsys.sdo_geometry := mdsys.sdo_geometry(2002,8265,null,mdsys.sdo_elem_info_array(1,2,1),
                                                       mdsys.sdo_ordinate_array(-100,30,-101,31));
  geom2    mdsys.sdo_geometry := mdsys.sdo_geometry(2002,8265,null,mdsys.sdo_elem_info_array(1,2,1),
                                                       mdsys.sdo_ordinate_array(-100,30,-101,31));

  segments mdsys.sdo_list_type;
  begin
    segments := find_intersection_segment(geom1,geom2);
    for ii in 1..segments.count loop
       dbms_output.put_line('seg ' ||segments(ii));
    end loop;

  end;
procedure test_check_PolyLR  as

  geom  mdsys.sdo_geometry;
  Poly_geom   mdsys.sdo_geometry;
  new_geom    mdsys.sdo_geometry;
  xys         mdsys.sdo_ordinate_array := mdsys.sdo_ordinate_array(-100,30,-99.998,30.0003,-99.996,30.002,-99.994,30.004,-100,30);
  result     mdsys.sdo_list_type;
  xc          number;
  yc          number;
  area number;
  oarea number;
  begin

    execute immediate 'select geometry from  GZCPB3.Z647LS_EDGE$ where edge_id=:1' into geom using 15085; --18511;
    execute immediate 'select geometry from  GZCPB3.Z624LS_EDGE$ where edge_id=:1' into Poly_geom using 6014; --22843; --11872;
     new_geom := GZ_UTIL_ZONE.line_simplify(geom,5000000.);
--    result := CHECK_POLYLR(geom,Poly_geom,new_geom);

--    xys := poly_geom.sdo_ordinates;
--    GZ_UTIL_ZONE.reverse_ordinates(Xys,1,xys.count);
    area := centroid(xys,xc,yc,8265);
    poly_geom.sdo_ordinates := xys;
    poly_geom := mdsys.sdo_geometry(2003,8265.,null,mdsys.sdo_elem_info_array(1,1003,1),xys);
    oarea := sdo_geom.sdo_area(poly_geom,0.05,'unit=sq_meter');
    dbms_output.put_line('oarea ' || oarea);
end;
procedure test_find_distances  as

  geom  mdsys.sdo_geometry;
  Poly_geom   mdsys.sdo_geometry;
  new_geom    mdsys.sdo_geometry;
  xys         mdsys.sdo_ordinate_array;
  Poly_xys    mdsys.sdo_ordinate_array;
  info_array   mdsys.sdo_elem_info_array;
  dist        number;
  vertex      number;
  xc          number;
  yc          number;
  MBR     mdsys.sdo_list_type := mdsys.sdo_list_type();
  xLL number;
  yLL number;
  xur number;
  yur  number;
  epsilon     number := 0.001;
  best        number := 1.E10;
  area number;
  oarea number;
  iseg      pls_integer;
  begin

    execute immediate 'select gz_qa.get_xys(geometry,1,-114,1,165) from  GZCPB3.Z624LSEDGES_TOPROCESS where edge_id=:1' into geom using 6108; --15085; --18511;
    execute immediate 'select new_geometry from  GZCPB3.Z624LSEDGES_TOPROCESS where edge_id=:1' into Poly_geom using 6063; --22843; --11872;
--    new_geom := GZ_UTIL_ZONE.line_simplify(geom,5000000.);
    Xys := geom.sdo_ordinates;
    Poly_xys := poly_geom.sdo_ordinates;
    info_array := geom.sdo_elem_info;
    SET_MANY_MBR(XYs,Info_Array,MBR,xLL,yLL,xUR,yUR);

    for ii in 1..TRUNC(Poly_Xys.count/2)-1 loop
       xc := Poly_Xys(ii*2-1);
       yc := Poly_XYs(ii*2);
       dbms_output.put_line('Measuring from vertex ' || ii || 'sc ' || round(xc,8) || ' yc ' || round(yc,8));
 --      vertex  := find_distances(xc,yc,Xys,dist);
        iseg := Find_Segment(xc,yc,MBR,Xys,8265.,dist,epsilon);
       if dist < best then
         best := dist;
         dbms_output.put_line('>>>>>>>found ' || round(best,5) || ' at vertex ' ||iseg);
       end if;
    end loop;

end;
procedure test_set_mbr as
  xys       sdo_ordinate_array:= sdo_ordinate_array();
  geom      mdsys.sdo_geometry := mdsys.sdo_geometry(2002,8265.,null,sdo_elem_info_array(1,2,1),
                                   mdsys.sdo_ordinate_array(1,2,3,4));
  mbr       mdsys.sdo_geometry;
  xll       number;
  yll       number;
  xur       number;
  yur       number;
  the_time  timestamp := current_timestamp;
begin
  xys.extend(8998);

  for jj in 1..3000 loop
  for ii in 1..xys.count loop
     xys(ii) := Mod(ii+200+jj,8997)*0.01;
  end loop;

  geom.sdo_ordinates := xys;
  mbr := sdo_geom.sdo_mbr(geom);
--  set_mbr(Xys,xll,yll,xur,yur);
  end loop;
  Xys := Mbr.sdo_ordinates;
  xll := xys(1);
  yll := xys(2);
  xur := xys(3);
  yur := xys(4);
  dbms_output.put_line('xll ' || xLL || ' yll ' || yll || ' xur  ' || xur  || ' yur ' || yur);
dbms_output.put_line('Elapsed time : ' || (current_timestamp - the_time));
end;
procedure test_quad_tree as

   xLL    number := 100.;
   yll    number := 30.;
   xUR    number := 108;
   yUR    number := 34;
   tile   pls_integer :=0;
   current_tile pls_integer;
   tiles  mdsys.sdo_list_type := mdsys.sdo_list_type(1,1);
   subdivide   boolean := FALSE;
begin
  dbms_output.put_line('quad was ' || tile);
  for ii in 1..20 loop
  if ii=1 or ii = 2 then
  subdivide := TRUE;
  tile := tiles(1);
  else
   subdivide := FALSE;
  end if;
  xLL    := 100.;
   yll    := 30.;
   xUR    := 108;
   yUR    := 34;
   xLL := -107.392532222355;
   yLL := 25.8326614491579;
   xUR := -93.5024306963926;
   yUR := 36.5052161428592;

-- tile(1) is the current quad
-- tile(2) is the next tile to pass UNLESS you want to subdivide this tile
-- in which case reset tile to tile(1) - the current quad you want subdivided
  tiles := quad_tree(xLL,yLL,xUR,yUR,tile,subdivide);
  current_tile := tiles(1);
  tile := tiles(2);
  dbms_output.put_line('ii ' || ii || ' quad now ' || current_tile || ' X ' || xLL|| ' y ' || yLL || ' xUR ' || xur || ' yUR '|| yUR|| ' next ' || tile);
  exit when tile=1;
  end loop;
end;
function test_robust_line_gen(pgeom mdsys.sdo_geometry default NULL,no_of_vert pls_integer default 3) return mdsys.sdo_geometry as

 geom  mdsys.sdo_geometry := pgeom;
begin
   if geom is NULL then
      geom := SDO_GEOMETRY(2002, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 2, 1),
      SDO_ORDINATE_ARRAY(
-93.887954, 43.626488, -93.91519, 43.626589, -93.915841, 43.625022, -93.927866,
43.626054, -93.927835, 43.630185, -93.942826, 43.630228, -93.942898, 43.604825,
-93.967888, 43.604887, -93.967916, 43.594998, -93.959311, 43.592548, -93.956764,
 43.591993, -93.938198, 43.586693, -93.987835, 43.586758, -93.988112, 43.557935,
 -93.968104, 43.557859, -93.96811, 43.543333, -93.988033, 43.543398));

   end if;

   return robust_line_gen(geom,no_of_vert);
end;
END GZ_SUPER;
/

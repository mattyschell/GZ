CREATE OR REPLACE PACKAGE BODY GZ_FSL AS

----------------------------------------
-- PROCEDURES
----------------------------------------
PROCEDURE CREATE_FSL_TRACKING (
   p_schema         IN VARCHAR2,
   p_table_name     IN VARCHAR2 DEFAULT 'GEN_FSL_TRACKING'
) AS
   --Stephanie
   --Copied from GZ_SMPOLY
   --Created the FSL tracking table.
   --This will drop one if it already exists.

   psql          VARCHAR2(4000);
   v_object_root VARCHAR2(4000) := p_table_name;  --??
BEGIN
   ----------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Create the empty table as an empty pipelined custom type');
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ----------------------------------------------------------------------------------
   psql := 'CREATE TABLE ' || p_table_name || ' NOPARALLEL NOLOGGING AS '
           || 'SELECT * FROM TABLE(GZ_FSL.NEW_GEN_FSL_TRACKING)';
   -- was this ... || 'SELECT * FROM TABLE('||p_schema||'GZ_FSL.NEW_GEN_FSL_TRACKING ) ';
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
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Grant privileges on ' || p_table_name);
   ----------------------------------------------------------------------------------
   EXECUTE IMMEDIATE 'GRANT SELECT ON ' || p_table_name || ' TO PUBLIC ';

END CREATE_FSL_TRACKING;
--------------------------------------------------------------------------------
PROCEDURE GRANT_FSL_VIEW_PERMISSIONS (
          p_topology_name IN VARCHAR2,
          p_to_whom IN VARCHAR2
         ) IS

-- helper to grant public select on FSL views (should be in process FSL)
-- in the schema you are logged into
vtopo varchar2(4000) := UPPER(p_topology_name);
vsql varchar2(4000);
vviews GZ_TYPES.stringarray;
vGrantee varchar2(4000) := UPPER(p_to_whom);

BEGIN

   DBMS_APPLICATION_INFO.SET_ACTION('');
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GRANT_FSL_VIEW_PERMISSIONS: Starting...');

   dbms_output.put_line('GRANT_FSL_VIEW_PERMISSIONS: running the following SQL...');

   vsql := 'select view_name from user_views where view_name like '''||
           vtopo ||'_FSL%''';

   dbms_output.put_line(vsql);

   EXECUTE IMMEDIATE vsql BULK COLLECT INTO vviews;

   dbms_output.put_line('GRANT_FSL_VIEW_PERMISSIONS: Number of views to grant permissions to = '||vviews.count);
   dbms_output.put_line('GRANT_FSL_VIEW_PERMISSIONS: running the following SQL...');

   FOR i in 1..vviews.COUNT LOOP

     vsql := 'GRANT SELECT ON '||vviews(i)||' TO '||vGrantee;
     dbms_output.put_line(vsql);
     EXECUTE IMMEDIATE vsql;

   END LOOP;

   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GRANT_FSL_VIEW_PERMISSIONS: Complete.');

END GRANT_FSL_VIEW_PERMISSIONS;
-------------------------------------------------------------------------------
PROCEDURE GRANT_FSL_TABLE_PERMISSIONS (
          p_topology_name IN VARCHAR2,
          p_to_whom IN VARCHAR2
         ) IS

-- helper to grant public select on FSL tables (should be in process FSL)
-- in the schema you are logged into
-- This grabs everything that starts with TOPO_FSL (so you migth set the
-- permissions for tables you are not intending here, which should be changed
-- by keeping a record of all tables we make as we go along in a
-- work table

vtopo varchar2(4000) := UPPER(p_topology_name);
vsql varchar2(4000);
vtables GZ_TYPES.stringarray;
vGrantee varchar2(4000) := UPPER(p_to_whom);

BEGIN

   DBMS_APPLICATION_INFO.SET_ACTION('');
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GRANT_FSL_TABLE_PERMISSIONS: Starting...');

   dbms_output.put_line('GRANT_FSL_TABLE_PERMISSIONS: running the following SQL...');

   vsql := 'select table_name from user_tables where table_name like '''||
           vtopo ||'_FSL%''';

   dbms_output.put_line(vsql);

   EXECUTE IMMEDIATE vsql BULK COLLECT INTO vtables;

   dbms_output.put_line('GRANT_FSL_TABLE_PERMISSIONS: Number of tables to grant permissions to = '||vtables.count);
   dbms_output.put_line('GRANT_FSL_TABLE_PERMISSIONS: running the following SQL...');

   FOR i in 1..vtables.COUNT LOOP

     vsql := 'GRANT SELECT ON '||vtables(i)||' TO '||vGrantee;
     dbms_output.put_line(vsql);
     EXECUTE IMMEDIATE vsql;

   END LOOP;

   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GRANT_FSL_VIEW_PERMISSIONS: Complete.');

END GRANT_FSL_TABLE_PERMISSIONS;

-------------------------------------------------------------------------------
PROCEDURE CHECK_FSLS(
         p_schema IN VARCHAR2,
         p_topology IN VARCHAR2
)
IS

-- DRAFT PROCEDURE TO RUN A BUNCH oF validation steps on FSL tables.
-- Stephanie 1/16/2011

-- Revise me!

vschema VARCHAR2(4000) := UPPER(p_schema);
vtopo VARCHAR2(4000) := UPPER(p_topology);
vsql VARCHAR2(4000);
vcount number := 0;
vprocess VARCHAR2(4000) := 'CHECK_FSLS';
vmsg VARCHAR2(4000);
vmsg1 VARCHAR2(4000) := 'CHECK_FSLS: ';
vtracking varchar2(4000);
vnote varchar2(4000);
vLogTime TIMESTAMP;
vCheckNum Number := 0;
vTableList GZ_TYPES.stringarray;
vBadGtypes NUMBER;
vFlag boolean := FALSE;
vIsValid varchar2(4000);
vStatus varchar2(4000) := 'PASSED';
vStep varchar2(4000);

BEGIN



   -- create log file if it does not exist...
   vtracking := vtopo||'_FSL_TRACKING';
   vsql := 'Select count(*) FROM user_tables WHERE table_name = :p1';
   EXECUTE IMMEDIATE vsql INTO vCount USING vtracking;
   IF (vCount = 0) THEN
       GZ_FSL.CREATE_FSL_TRACKING(vschema, vtracking);
   END IF;

   -- log 1st step
   GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,
                                     vProcess,vTracking,p_message=>'Starting FSL Checks');
   -----------------------------------------------------------------------------
   -- CHECK 1: TABLE COUNT
   -- check registered table count
   vCheckNum := 1;
   vStep := 'Check '||vCheckNum;
   vmsg := vmsg1 ||' Check Number '||vCheckNum||': ';
   vsql := 'SELECT COUNT(*) FROM ALL_SDO_TOPO_INFO '||
           'WHERE owner = :p1 AND topology = :p2';

   EXECUTE IMMEDIATE vsql INTO vcount USING vschema,vtopo;

   vnote := vmsg||'Found '||vcount||' registered tables in '||vtopo||'.';

   GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,
                                     vProcess,vTracking,p_step=>vStep,
                                     p_sqlstmt=>vsql,p_message=>vNote);

   dbms_output.put_line(vNote);

   IF vcount > 2 THEN
       vNote := 'PASSED';
       GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,
                                     vProcess,vTracking,
                                     p_step=>vStep,
                                     p_message=>vNote);
   dbms_output.put_line(vNote);
   ELSE
       vNote := 'FAILED';
       vStatus := 'FAILED';
       GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess,vTracking,
                                     p_step=>vStep,p_message=>vNote) ;
   END IF;

   dbms_output.put_line(vmsg||vNote);

   -----------------------------------------------------------------------------
   -- CHECK 1.5: Get a list of FSL tables to look at.
   vCheckNum := 1.5;
   vStep := 'Check '||vCheckNum;
   vmsg := vmsg1 ||' Check Number '||vCheckNum||': ';

   vNote := 'Begin Individual Table Checks.';

   dbms_output.put_line(vmsg1||vNote);

   GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess,vTracking,
             p_step=>vStep,p_message=>vNote) ;

   vsql := 'SELECT table_name FROM user_sdo_topo_info '||
           'WHERE topology = :p1 AND table_name like :p2 '||
           'AND table_name NOT LIKE :p3 order by table_name';

   GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess,vTracking,
             p_step=>vStep,p_sqlstmt=>vsql,
             p_message=>'Starting Loop through tables (Checks 2 and 3)');

   EXECUTE IMMEDIATE vsql
           BULK COLLECT INTO vTableList
           USING vtopo,'%FSL%','%TRACKING';

   FOR i IN 1..vTableList.COUNT LOOP
   -----------------------------------------------------------------------------
   -- CHECK 2: Look for bad Gtypes in all FSL tables in the topology
       vCheckNum := 2;
       vStep := 'Check '||vCheckNum;
       vmsg := vmsg1 ||' Check Number '||vCheckNum||': ';

       vNote := vTableList(i)||' - Running Search for bad GTypes.';
       GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess,vTracking, p_step=>vStep,
                                     p_message=>vNote);

       dbms_output.put_line(vmsg||vNote);

       vBadGtypes := GZ_FSL.COUNT_BAD_GTYPES(vTableList(i),'SDOGEOMETRY');
       vNote :=  vTableList(i)||' - Found '||vBadGtypes||' bad gtypes.';

       GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess, vTracking,p_step=>vStep,
       p_sqlstmt=>'GZ_FSL.COUNT_BAD_GTYPES('||vTableList(i)||',''SDOGEOMETRY'');',
       p_message=>vNote);

       dbms_output.put_line(vmsg||vNote);
       IF vBadGtypes > 0 THEN
          vNote := 'FAILED';
          vStatus := 'FAILED';
       ELSE
          vNote := 'PASSED';
       END IF;
       GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess, vTracking,p_step=>vStep,
                                         p_message=>vNote);

       dbms_output.put_line(vmsg||vTableList(i)||' - '||vNote);

   -----------------------------------------------------------------------------
   -- CHECK 3: Look for invalid Geometries in all FSL tables in the topology
      vCheckNum := 3;
      vStep := 'Check '||vCheckNum;
      vmsg := vmsg1 ||' Check Number '||vCheckNum||': ';
      vNote := vTableList(i)||' - Running Validate Geometry with Context Check.';
      dbms_output.put_line(vmsg||vNote);
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess, vTracking,p_step=>vStep,

                                         p_message=>vNote);

      -- This way calls topogeom.get_geometry() and checks the result.  Use
      -- this line instead of the next one if you don't have an SDOGEOMETRY
      -- calculated.
      -- vFlag := GZ_SMPOLY.are_my_poly_features_valid(vTableList(i),'GEO_ID');

      -- This way checks the SDO geometry values in an sdogeometry column you provide.
      vFlag := GZ_SMPOLY.ARE_MY_SDO_GEOMETRIES_VALID(vTableList(i),'GEO_ID','SDOGEOMETRY', 0.05);

      IF vFlag THEN
         vNote := 'PASSED';
      ELSE
         vNote := 'FAILED';
         vStatus := 'FAILED';
      END IF;

      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess,vTracking, p_step=>vStep,
                                         p_message=>vNote);

      dbms_output.put_line(vmsg||vTableList(i)||' - '||vNote);

      dbms_output.put_line(vmsg||vTableList(i)||' - Check Complete ---------------------------');
   END LOOP;

   -----------------------------------------------------------------------------
   -- CHECK 4: Validate the topology
   dbms_output.put_line(vmsg||'------------------------------------------------');
   vCheckNum := 4;
   vStep := 'Check '||vCheckNum;
   vmsg := vmsg1 ||' Check Number '||vCheckNum||': ';
   vNote := 'Begin Validate Geometry - Sreeni style.';
   dbms_output.put_line(vmsg||vNote);
   GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess,vTracking,
                                     p_step=>vStep,p_message=>vNote);

   vFlag := FALSE;
   vFlag := GZ_TOPO_UTIL.validate_feature_tables(vSchema,vTopo);
   IF vFlag THEN
      vNote := 'PASSED';
   ELSE
      vNote := 'FAILED';
      vStatus := 'FAILED';
   END IF;

   GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess, vTracking,p_step=>vStep,
                                    p_message=>vNote);
   dbms_output.put_line(vmsg||vNote);

   -----------------------------------------------------------------------------
   -- CHECK 5: Validate the topology with Oracle's validator
   vCheckNum := 5;
   vStep := 'Check '||vCheckNum;
   vmsg := vmsg1 ||' Check Number '||vCheckNum||': ';
   vNote := 'Begin Validate Geometry - Oracle style.';
   dbms_output.put_line(vmsg||vNote);
   GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess, vTracking,p_step=>vStep,
                                      p_message=>vNote);

   vcount := 0;
   vsql := 'SELECT COUNT(*) FROM '||vtopo||'_EDGE$';
   EXECUTE IMMEDIATE vsql INTO vCount;
   GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess, vTracking,p_step=>vStep,
                                     p_sqlstmt=>vsql);

   vIsValid := 'FALSE';
   vIsValid := GZ_UTILITIES.VALIDATE_TOPOLOGY(vtopo,vCount);
   IF vIsValid = 'TRUE' THEN
      vNote := 'PASSED';
   ELSE
      vNote := 'FAILED';
     vStatus := 'FAILED';
   END IF;
   GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,vProcess,vTracking, p_step=>vStep,
                                     p_message=>vNote);
   dbms_output.put_line(vmsg||vNote);

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -- Report Final Status:
   vNote := 'Completed: '||vStatus;
   GZ_UTILITIES.GEN_FSL_TRACKING_LOG(vTopo,
                                     vProcess,vTracking,p_message=>vNote);
   dbms_output.put_line(vmsg||vNote);

END CHECK_FSLS;

PROCEDURE FIX_ALL_CONNECT_BY_ERRORS(
         p_topology IN VARCHAR2,
         p_feature_table_name IN VARCHAR2,
         p_PrimaryKeyColumn IN VARCHAR2,
         p_GeometryColumn IN VARCHAR2 DEFAULT 'TOPOGEOM'
) IS

-- This check for the connect by loop error and if it finds it,
-- it corrects it, using Sreeni's fixer, then tries again if there are more
--         p_topology:  the name of the topology to check
--                    you will need select access to the
--                    <topology>.relation$ table
--         p_feature_table_name: the  name of the feature table
--                               that could cause the problem.  This is
--                               usually the FACE feature table, but I
--                               think we might be able to use this for other
--                               feature tables at some point.
--         p_PrimaryKeyColumn: The primary key of the feature table
--                            (FACE_ID for faces),
--         p_GeometryColumn: The name of the geometry column in the topology
--                           for most of our stuff this is 'TOPOGEOM'

-- For now - assume this only works when you have a single feature table
-- in the topology.  It is not tested for anything else.

vsql varchar2(4000);
vtopo varchar2(4000) := UPPER(p_topology);
vtable varchar2(4000) := UPPER(p_feature_table_name);
vHasError BOOLEAN := FALSE;
vNote varchar2(4000);
vContinue boolean := TRUE;
vPKC varchar2(4000) := UPPER(p_PrimaryKeyColumn);
vGeomColumn varchar2(4000) := UPPER(p_GeometryColumn);
vmsg varchar2(4000) := 'FIX_ALL_CONNECT_BY_ERRORS: ';
vPKList gz_types.stringarray;
vLoopCount NUMBER := 0;

BEGIN

-- Does the PKValue have to be a number ?  Can I use FACE_ID, OR GEO_ID?

   WHILE vContinue LOOP

      vHasError := GZ_FSL.HAS_CONNECT_BY_ERROR(vtopo);

      IF vHasError THEN
         -- figure out which entity has the problem...
         vsql := 'SELECT b.'||vPKC||
                 ' FROM '||vtopo||'_relation$ a, '||
                         vtable||' b '||
                 'WHERE a.tg_layer_id = a.topo_id AND a.tg_id = a.topo_type '||
                 'AND b.'||vGeomColumn||'.TG_ID = a.tg_id '||
                 'AND a.tg_layer_id = b.'||vGeomColumn||'.tg_layer_id' ;

         dbms_output.put_line(vmsg||'A Connect By Loop Error was found.');
         dbms_output.put_line(vmsg||'Looking for an entity to fix.');
         dbms_output.put_line(vmsg||vSQL);
         EXECUTE IMMEDIATE vsql BULK COLLECT INTO vPKList;

         -- Fix the problem you found.
         BEGIN
            dbms_output.put_line(vmsg||'GZ_TOPO_UTIL.FIX_CNNCTBYLP_ERR('''
                                ||vtopo||''', '''||vtable||''', '''||
                                vPKC||''', '''||vPKList(1)||''', '''||
                                vGeomColumn||''');');
            GZ_TOPO_UTIL.FIX_CNNCTBYLP_ERR(vtopo, vtable, vPKC, vPKList(1), vGeomColumn);
            -- add error handling
         END;
      ELSE
         dbms_output.put_line(vmsg||'No Connect By Loop Error found.');
         vContinue := FALSE;
      END IF;

      -- Bail out if you loop more than 100 times.

      vLoopCount := vLoopCount + 1;

      -- debugging message
      IF vLoopCount > 100 THEN
        dbms_output.put_line(vmsg||'Loop Count over 100, Bailing out of loop!');
      END IF;

      EXIT WHEN vLoopCount > 100;

   END LOOP;

vNote := 'Connect By Loop error should be fixed, double checking...';
dbms_output.put_line(vMsg||vNote);

vHasError := TRUE;
vHasError := GZ_FSL.HAS_CONNECT_BY_ERROR(vtopo);

IF vHasError THEN
   vNote := 'Connect By Loop error IS NOT fixed!';
ELSE
   vNote := 'Connect By Loop Fix was Successful.';
END IF;
dbms_output.put_line(vmsg||vNote);

END FIX_ALL_CONNECT_BY_ERRORS;
------------------------------------------------------------------------------
Procedure CREATE_STATETOPO_VIEWS(
         Schema_Name VARCHAR2,
         Topology VARCHAR2 default NULL
) AS

-- set up for DEC10. Since we are processing in multiple state-based
-- schemas and we would like to create all the state shape files from one
-- single schema, we create views of the state-based FSL views in
-- the single national schema GZDEC10ST99.

-- Depending on the production environment in the future,
-- we may need to change this

-- Written by Salman, Mid January, 2011

-- Parameters
   -- Schema_Name = the name of the schema where the state-based views live
   -- Topology  = the name of the topology to create the views for


vtopology   VARCHAR2(32)            := Topology;
vSchema     VARCHAR2(50)            := Schema_Name;
vViewList   MDSYS.STRING_ARRAY;
vCurView    VARCHAR2(32);

sql_stmt    VARCHAR2(4000);     -- Dynamic SQL Statement

Begin

    IF (vtopology IS NULL)
     THEN
        sql_stmt := 'select view_name from all_views where owner = :1 and view_name like :2 order by view_name';
        EXECUTE IMMEDIATE sql_stmt bulk collect INTO vViewList USING vSchema, '%_FSL%';
    ELSE
        sql_stmt := 'select view_name from all_views where owner = :1 and view_name like :2 order by view_name';
        EXECUTE IMMEDIATE sql_stmt bulk collect INTO vViewList USING vSchema, vtopology||'_FSL%';
    END IF;

    FOR i IN vViewList.FIRST..vViewList.LAST
     LOOP

        vCurView := vViewList(i);

        sql_stmt := 'create or replace view '||vCurView||' AS select * from '||vSchema||'.'||vCurView||'';
        EXECUTE IMMEDIATE sql_stmt;

        dbms_output.put_line('View Created: '||vCurView||'');

     END LOOP;

End Create_StateTopo_Views;

----------------------------------------
-- FUNCTIONS
----------------------------------------
FUNCTION NEW_GEN_FSL_TRACKING RETURN GZ_TYPES.GEN_FSL_TRACKING PIPELINED
AS
BEGIN
  NULL;
END NEW_GEN_FSL_TRACKING;
------------------------------------------------------------------------------
FUNCTION COUNT_BAD_GTYPES(
    p_polygon_table_name IN VARCHAR2,
    p_geometry_column_name IN VARCHAR2
) RETURN NUMBER
AS

-- returns the number of sdogeometries in your table that
-- either 1) don't look like polygons (2003) or
--        2) don't look like multi-polygons (2004)

vsql varchar2(4000);
vtable varchar2(4000) := UPPER(p_polygon_table_name);
vsdogeometry_column varchar2(4000) := UPPER(p_geometry_column_name);
vCount number;

BEGIN

   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('COUNT_BAD_GTYPES: Starting...');

   vsql := 'SELECT COUNT(*) '||
           'FROM '|| vtable || ' a '||
           'WHERE SDO_GEOMETRY.get_gtype(a.'||vsdogeometry_column||') <> 3 '||
           'AND SDO_GEOMETRY.get_gtype(a.'||vsdogeometry_column||') <> 7';

   EXECUTE IMMEDIATE vsql INTO vCOUNT;

   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('COUNT_BAD_GTYPES: Compelte ('||vCount||').');

   return vCOUNT;

END COUNT_BAD_GTYPES;
------------------------------------------
FUNCTION HAS_CONNECT_BY_ERROR(
   p_topology   varchar2
) RETURN boolean AS
-- runs the query to check for the potential of the Oracle Connect By Loop
-- Error.  Returns TRUE if it find the potential and FALSE if not.
-- ptopology : the name of the topology to check
-- you will need access to the <topology>_relation$ table
vsql varchar2(4000);
vtopo varchar2(4000) := UPPER(p_topology);
vcount NUMBER;
vreturn BOOLEAN := FALSE;
BEGIN

vsql := 'select count(*) '||
        'from '||vtopo||'_relation$ '||
        'where tg_layer_id = topo_id '||
        '  and tg_id = topo_type';

EXECUTE IMMEDIATE vsql INTO vcount;

IF vcount > 0 THEN
   vreturn := TRUE;
END IF;

RETURN vreturn;

END HAS_CONNECT_BY_ERROR;

PROCEDURE FSL_BUILD(
   pSchema IN VARCHAR2,
   pTopo_In IN VARCHAR2,
   pTopoType IN VARCHAR2,
   pFace_Tab_Ext IN VARCHAR2,
   pTopo_Universe IN VARCHAR2,
   pTopo_Hierarchy IN VARCHAR2,
   -- pHas_Mirrors IN VARCHAR2,
   pProject_Id IN VARCHAR2,
   pProject_Z9 IN VARCHAR2,
   pRelease In VARCHAR2,
   pSt_Edges_Tbl in VARCHAR2,
   pJobID in VARCHAR2 default NULL
   ) as
-- Input parameters

   vSchema VARCHAR2(30) := upper(pSchema);
   vTopo_In VARCHAR2(20) := upper(pTopo_In);
   vFace_Tab_Ext VARCHAR2(30) := upper(pFace_Tab_Ext);
   vTopo_Universe VARCHAR2(30) := upper(pTopo_Universe);
   vTopo_Hierarchy VARCHAR2(30) := upper(pTopo_Hierarchy);
   --vHas_Mirrors VARCHAR2(10) := upper(pHas_Mirrors);
   vProject_Id VARCHAR2(10) := upper(pProject_Id);
   vProject_Z9 VARCHAR2(1) := upper(pProject_Z9);
   vRelease VARCHAR2(10) := upper(pRelease);
   vSt_Edges_Tbl VARCHAR2(30) := upper(pSt_Edges_Tbl);

   APP_INFO_MODULE VARCHAR2(48);
   APP_INFO_ACTION VARCHAR2(32);
   APP_INFO_CLIENT_INFO VARCHAR2(64);

   TrackingTable VARCHAR2(30) := vTopo_In || '_FSL_TRACKING';

   SQL1 VARCHAR2(4000);
   rowCnt NUMBER;

   Face_Table VARCHAR2(30) := vTopo_In || '_' || vFace_Tab_ext;
   vNote VARCHAR2(1000);

   TYPE nested_type IS TABLE OF VARCHAR2(30);
   inpTbls nested_type;

   inpTblErr boolean := FALSE;

   XTABLE_ARR MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();

Begin

   -- Pre processing
   ------------------------------------------------------------------------------
   -- Tag the batch job
   APP_INFO_MODULE := 'GZ_FSL.FSL_BUILD-'||vSchema||','||vProject_ID||','||vTopo_In;
   APP_INFO_ACTION  := 'Begin';
   DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
   ------------------------------------------------------------------------------

   -- 0. Create FSL_tracking table if it doesn't exist
   -- Create FSL_Tracking table
   SQL1 := 'Select Count(*) From User_Tables Where Table_Name = :1';
   Execute Immediate SQL1 into rowCnt using TrackingTable;

   If rowCnt = 1 Then
      SQL1 := 'Truncate table ' || trackingTable;
      Execute Immediate SQL1;
   Else
     dbms_output.put_line('Create trackingTable: ' || TrackingTable);
      Create_FSL_Tracking(vSchema, TrackingTable);
   End If;


   --Verify if Input Tables exist
   --1. Face table
   --2. Topo Universe
   --3. Topo Hierarchy
   --4. St Edges Tbl

   Face_Table := vTopo_In || '_' || vFace_Tab_ext;

   --inpTbls := nested_type(Face_table, vTopo_Universe, vTopo_Hierarchy, vSt_Edges_Tbl);
   -- Add LUT_LSAD to the parameter table
   inpTbls := nested_type(Face_table, vTopo_Universe, vTopo_Hierarchy, 'LUT_LSAD');

   SQL1 := 'Select Count(*) From User_Tables Where Table_Name = :1';

   If inpTbls IS NULL THEN
      dbms_output.put_line('Error reading input table names');
      RAISE_APPLICATION_ERROR(-20001, 'Error reading input table names');
   else

      --dbms_output.put_line('Input Tbl Count: ' || inpTbls.Count);

      FOR i IN inpTbls.FIRST .. inpTbls.LAST
      LOOP
         Execute Immediate SQL1 into rowCnt using inpTbls(i);
         If rowCnt = 0 Then
            vNote := 'ERROR: Table: ' || inpTbls(i) || ' does not exist!';
            dbms_output.put_line(vNote);
            inpTblErr := TRUE;
         Elsif rowCnt = 1 then
            vNote := 'Table: ' || inpTbls(i) || ' exists';
         End If;
         --DBMS_OUTPUT.PUT_LINE('TrackingTable: ' || TrackingTable);
         GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'FSL_BUILD', TrackingTable, p_message=>vNote);

      END LOOP;

   End If;

   If inpTblErr = True Then
      RAISE_APPLICATION_ERROR(-20001, 'Input Table(s) don''t exist... Please verify');
   End If;


  --GZ_FSL.CREATE_FSL_TRACKING('$user', '$tracking');
  -- Handled above

  -- 1. Cleanup_FSLs
  CLEANUP_FSLS(vTopo_In, vSchema, vSt_Edges_Tbl);
  CLEANUP_FSLS_QA(pJobID, vTopo_In, vSchema);

  
  -- Uncomment the following IF block after updating SQL1 with the correct projection table list.
  -- 2. Cleanup_tmp_projection_tbls
  --If (vProject_Z9 = 'Y') Then
     -- Cleanup_tmp_projection_tbls(....);
     -- drop any projection related tables (Topology || 'X')
      --SQL1 := 'SELECT TABLE_NAME FROM USER_TABLES WHERE UPPER(TABLE_NAME) LIKE :1';
      -- EXECUTE IMMEDIATE SQL1 BULK COLLECT INTO XTABLE_ARR USING 'XX2003ONLY XXTMPOUT AREA';
      -- 
      --
      --If XTABLE_ARR.Count > 0 Then
      --   For i in XTABLE_ARR.First .. XTABLE_ARR.Last
      --      Loop
      --         SQL1 := 'DROP TABLE ' || XTABLE_ARR(i) || ' PURGE';
      --         GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'FSL_BUILD',TrackingTable,p_message=>SQL1);
      --         EXECUTE IMMEDIATE SQL1;
      --
      --      End Loop;
      --
      --End If;
      --
      --XTABLE_ARR.Delete;
      --
      --vNote := 'Cleanup of tmp projection tables is complete!';
      --GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'FSL_BUILD',TrackingTable,p_message=>vNote);
      --
      --
  --End If;
  
  -- 3. Build_fsls
  --Build_FSLs(pTOPO_IN,vFace_Tab_ext,Face_Table,pTopo_Universe,pTopo_Hierarchy,pHas_Mirrors,pProject_Id,pProject_Z9,pRelease,pSchema);
  Build_FSLs(pTOPO_IN,pTopoType,vFace_Tab_ext,Face_Table,pTopo_Universe,pTopo_Hierarchy,pProject_Id,pProject_Z9,pRelease,pSchema, pJobID);

  vNote := 'FSL_BUILD Module completed successfully';
  GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'FSL_BUILD',TrackingTable,p_message=>vNote);


   ------------------------------------------------------------------------------
   -- Tag the batch job
   APP_INFO_MODULE := 'GZ_FSL.FSL_BUILD-'||vSchema||','||vProject_ID||','||vTopo_In;
   APP_INFO_ACTION  := 'END';
   DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
   ------------------------------------------------------------------------------

End;

Procedure CLEANUP_FSLS(
   pTOPO_IN IN VARCHAR2,
   pSCHEMA IN VARCHAR2,
   --pFACE_TAB IN VARCHAR2, -- Why do we need this?
   pST_EDGES_TBL IN VARCHAR2
 ) as

   SQL1 VARCHAR2(4000);
   rowCnt NUMBER;

   vNote VARCHAR2(1000);

   APP_INFO_MODULE VARCHAR2(48);
   APP_INFO_ACTION VARCHAR2(32);
   APP_INFO_CLIENT_INFO VARCHAR2(64);

   TrackingTable VARCHAR2(32) := upper(pTOPO_IN) ||'_FSL_TRACKING';

Begin

   ------------------------------------------------------------------------------
   -- Tag the batch job
   APP_INFO_MODULE := 'GZ_FSL.CLEANUP_FSLS-'||pSchema||','||pTopo_In;
   APP_INFO_ACTION  := 'Begin';
   DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
   ------------------------------------------------------------------------------

   --- Tuncate FSL Tracking table  * Not necessary this is already done *


   --- De-register existing FSL tables (GZ_TOPO_HELPER.DEREGISTER_FSL)
   SQL1 := 'Select count(*) From user_sdo_topo_info Where topology = :1 and table_name like :2';
   Execute Immediate SQL1 into rowCnt using pTOPO_IN, pTOPO_IN || '_FSL%' ;

   If rowCnt > 0 Then
      vNote := 'Found ' || rowCnt || ' registered FSL tables.  Goto next step';
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'CLEANUP_FSLS',TrackingTable,p_message=>vNote);

      GZ_TOPO_HELPER.DEREGISTER_FSL(pTOPO_IN, 'FSL', pSCHEMA); -- % to deregister all FSLs
      --- Drop existing FSL tables (GZ_TOPO_UTIL.DROP_TABLES_WITH_PREFIX)
      -- Per Stephanie we should only drop registered tables 2011/07/26
      -- gz_topo_util.drop_tables_with_prefix(pTOPO_IN || '_FSL', pSCHEMA);

      --- Verify
      -- if the number of registered tables does not exactly equal 2, there is a problem.
      Execute Immediate SQL1 into rowCnt using pTOPO_IN,  pTOPO_IN || '_FSL%' ;
      If rowCnt > 0 THEN
         vNote := 'CLEANUP_FSLS: ERROR cleaning up FSL tables.  Still found ' || rowCnt || ' registered tables. Aborting...';
         GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'CLEANUP_FSLS',TrackingTable,p_message=>vNote);
         RAISE_APPLICATION_ERROR(-20001, vNote);
      Else
         vNote := 'Successful FSL Cleanup!';
         GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'CLEANUP_FSLS',TrackingTable,p_message=>vNote);
      End If;

   End If;

   ------------------------------------------------------------------------------
   -- Tag the batch job
   APP_INFO_MODULE := 'GZ_FSL.CLEANUP_FSLS-'||pSchema||','||pTopo_In;
   APP_INFO_ACTION  := 'End';
   DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
   ------------------------------------------------------------------------------
End;

PROCEDURE CLEANUP_FSLS_QA (pJOBID_IN   IN VARCHAR2,
                           pTOPO_IN    IN VARCHAR2,
                           pSCHEMA     IN VARCHAR2)
AS
   SQL1                   VARCHAR2 (4000);
   rowCnt                 NUMBER;
   vTbl_subfix          VARCHAR2 (30);
   vNote                  VARCHAR2 (1000);
   array_table_name    MDSYS.string_array;

   APP_INFO_MODULE        VARCHAR2 (48);
   APP_INFO_ACTION        VARCHAR2 (32);
   APP_INFO_CLIENT_INFO   VARCHAR2 (64);

   TrackingTable          VARCHAR2 (32)
                             := UPPER (pTOPO_IN) || '_FSL_TRACKING';
BEGIN
   ------------------------------------------------------------------------------
   -- Tag the batch job
   APP_INFO_MODULE := 'GZ_FSL.CLEANUP_FSLS_QA-' || pSchema || ',' || pTopo_In;
   APP_INFO_ACTION := 'Begin';
   DBMS_APPLICATION_INFO.SET_MODULE (APP_INFO_MODULE, APP_INFO_ACTION);
   ------------------------------------------------------------------------------
   IF (pJOBID_IN IS NOT NULL) THEN
      SQL1 := 'SELECT COUNT(1) FROM GZ_QA_SETUP WHERE jobid = :1';
      EXECUTE IMMEDIATE SQL1 INTO rowCnt USING pJOBID_IN;
   ELSE
      SQL1 := 'SELECT COUNT(1) FROM GZ_QA_SETUP 
                    WHERE gen_topo = :1 AND rownum = 1 ';
      EXECUTE IMMEDIATE SQL1 INTO rowCnt USING pTOPO_IN;
   END IF;   
   
   IF (rowCnt > 0) THEN
      IF (pJOBID_IN IS NOT NULL) THEN
          SQL1 := 'SELECT out_tbl_suffix FROM GZ_QA_SETUP WHERE jobid = :1';
          EXECUTE IMMEDIATE SQL1 INTO vTbl_subfix USING pJOBID_IN;
      ELSE
          SQL1 := 'SELECT out_tbl_suffix FROM GZ_QA_SETUP 
                        WHERE gen_topo = :1 AND rownum = 1 ';
          EXECUTE IMMEDIATE SQL1 INTO vTbl_subfix USING pTOPO_IN;
       END IF;   
       SQL1 := 'SELECT table_name FROM user_tables WHERE table_name like :1 ';

       EXECUTE IMMEDIATE SQL1 BULK COLLECT INTO array_table_name
          USING pTOPO_IN || '_FSL%'||vTbl_subfix;
       IF (array_table_name.COUNT > 0) THEN
          --
          FOR i IN 1 .. array_table_name.LAST
          LOOP
             SQL1 := 'DROP TABLE  ' || array_table_name (i)  ||' PURGE';
             EXECUTE IMMEDIATE SQL1;

          END LOOP;
       END IF;
   END IF;
   
   ------------------------------------------------------------------------------
   -- Tag the batch job
   APP_INFO_MODULE := 'GZ_FSL.CLEANUP_FSLS_QA-' || pSchema || ',' || pTopo_In;
   APP_INFO_ACTION := 'End';
   DBMS_APPLICATION_INFO.SET_MODULE (APP_INFO_MODULE, APP_INFO_ACTION);
------------------------------------------------------------------------------
END CLEANUP_FSLS_QA;

Procedure BUILD_FSLS(
   pTOPO_IN IN VARCHAR2,
   pTopo_Type IN VARCHAR2,
   pFACE_TABLE_EXT IN VARCHAR2,
   pFACE_TABLE IN VARCHAR2,
   pTopo_Universe IN VARCHAR2,
   pTopo_Hierarchy IN VARCHAR2,
   --pHas_Mirrors IN VARCHAR2,
   pProject_Id IN VARCHAR2,
   pProject_Z9 IN VARCHAR2,
   pRelease In VARCHAR2,
   pSchema IN VARCHAR2,
   pJobID in VARCHAR2 default NULL   
) as

   SQL1 VARCHAR2(4000);
   rowCnt NUMBER;

   -- Assumptions pFace_Table has Face_ID and TOPOGEOM columns
   FSL_BUILD_ERR BOOLEAN := FALSE;

   vNote VARCHAR2(1000);

   TrackingTable VARCHAR2(32) := upper(pTOPO_IN) ||'_FSL_TRACKING';

   APP_INFO_MODULE VARCHAR2(48);
   APP_INFO_ACTION VARCHAR2(32);
   APP_INFO_CLIENT_INFO VARCHAR2(64);


     PROCEDURE Update_FSLBUILD_Comments(
     pJobID IN VARCHAR2,
     pMessage IN VARCHAR2
     )
     AS PRAGMA AUTONOMOUS_TRANSACTION;

        SQL1 VARCHAR2(1000);

     Begin


           SQL1 := 'Update GZ_FSLBUILD_SETUP SET Comments = comments || '' ' || pMessage
                       || ''' Where jobid = :1';
           EXECUTE IMMEDIATE SQL1 USING pJobID;
        
           commit;

     Exception
       When Others then
           DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
           RAISE_APPLICATION_ERROR(-20001, SQLCODE || ' ' || SQLERRM);

     END;


Begin

   ------------------------------------------------------------------------------
   -- Tag the batch job
   APP_INFO_MODULE := 'GZ_FSL.BUILD_FSLS-'||pSchema||','||pProject_ID||','||pTopo_In;
   APP_INFO_ACTION  := 'Begin';
   DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
   ------------------------------------------------------------------------------
   -- 1. Create the log/tracking table if it doesn't exist
   -- GZ_FSL.CREATE_FSL_TRACKING('$user', '$tracking');
   -- Already done in Calling Procedure (FSL_BUILD)

    vNote := 'Begin Build_FSLs!';
    GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);

   -- 2. Check for Connect By Loop Error AND fix it if it exists...
   GZ_FSL.FIX_ALL_CONNECT_BY_ERRORS(pTOPO_IN,pFACE_TABLE,'FACE_ID','TOPOGEOM');

   -- Verify if the above fix worked
   sql1 := 'SELECT count(*) from ' || pTopo_In || '_relation$ ' || ' where tg_layer_id = topo_id and tg_id = topo_type';
   Execute Immediate SQL1 into rowCnt;

   If rowCnt > 0 Then
      vNote := 'Unable to fix all connect by loop errors... Aborting';
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);
      RAISE_APPLICATION_ERROR(-20001, vNote);
   Else
      vNote := 'Connect By Loop Double Check Looks All Right';
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);
   End If;

    vNote := 'FIX_ALL_CONNECT_BY_ERRORS -- Done';
    GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);

   -- 3. Fix permissions?  I don't think we need to fix any permissions.  Double check this.
   -- GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS','$topo_in%')

   -- 4. Run FSL Build

   --GZ_TOPO_HELPER.PROCESS_FSL(pTopo_IN, pFACE_TABLE_EXT, pTopo_Universe, pTopo_Hierarchy, pHas_Mirrors, pProject_Z9, pRelease, pSchema);
   --PROCESS_FSL (Topology VARCHAR2, Face_tbl VARCHAR2, Topo_universe VARCHAR2, Topo_hierarchy VARCHAR2, Mirrors VARCHAR2 default 'N', Projection VARCHAR2 default 'N', Release VARCHAR2, Deploy VARCHAR2)
   GZ_TOPO_HELPER.PROCESS_FSL(pTopo_IN, pFACE_TABLE_EXT, pTopo_Universe, pTopo_Hierarchy, pProject_Z9, pRelease, pSchema);

    vNote := 'GZ_TOPO_HELPER.PROCESS_FSL -- Done';
    GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);

   -- 5. Grants
   GZ_FSL.GRANT_FSL_VIEW_PERMISSIONS(pTopo_IN,'PUBLIC');
   GZ_FSL.GRANT_FSL_TABLE_PERMISSIONS(pTopo_IN,'PUBLIC');

   -- 6. Check FSLs
   GZ_FSL.CHECK_FSLS(pSchema,pTopo_IN);

   -- 7. Check for bad gtypes
   sql1 := 'SELECT COUNT(*) FROM ' || pTopo_in || '_FSL_TRACKING WHERE process = :1 AND message = :2';
   Execute Immediate SQL1 into rowCnt using 'FAILED', 'Check 2';

   If rowCnt > 0 Then
      vNote := 'Failed the GTYPE check. (' || rowCnt || ' tables are bad)';
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);
      FSL_BUILD_ERR := TRUE;
      --RAISE_APPLICATION_ERROR(-20001, vNote);

   Else
      vNote := 'GTYPE Check OK';
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);
   End If;


   -- 8. Check for "validate with context" errors
   sql1 := 'SELECT COUNT(*) FROM ' || pTopo_in || '_FSL_TRACKING WHERE step = :1 AND message = :2';
   Execute Immediate SQL1 into rowCnt using 'Check 3', 'FAILED';

   If rowCnt > 0 Then
      vNote := 'Failed Validate with Context check. (' || rowCnt || ' tables are bad)';
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);
      FSL_BUILD_ERR := TRUE;
      --RAISE_APPLICATION_ERROR(-20001, vNote);

   Else
      vNote := 'Validate with Context check OK';
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);
   End If;

   -- 9. Use Oracle topology validator
   sql1 := 'SELECT COUNT(*) FROM ' || pTopo_in || '_FSL_TRACKING WHERE step = :1 AND message = :2';
   Execute Immediate SQL1 into rowCnt using 'Check 4', 'PASSED';
   If rowCnt = 1 Then
      vNote := 'Sreeni Style Validate Feature Tables check OK';
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);

   Else
      vNote := 'Failed Sreeni Style Validate Feature Tables check';
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);
      FSL_BUILD_ERR := TRUE;
      --RAISE_APPLICATION_ERROR(-20001, vNote);

   End If;

   Execute Immediate SQL1 into rowCnt using 'Check 5', 'PASSED';
   If rowCnt = 1 Then
      vNote := 'Oracle Style Validate Feature Tables check OK';
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);

   Else
      vNote := 'Failed Oracle Style Validate Feature Tables check';
      GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);
      FSL_BUILD_ERR := TRUE;
      --RAISE_APPLICATION_ERROR(-20001, vNote);

   End If;

   If FSL_BUILD_ERR = TRUE THEN
      IF    pTopo_Type = 'UG' Then
            -- This is the ungeneralized topology.  It might be ok.
            vNote := 'UnGen Topology ' || pTopo_IN || ' failed some validation checks.';
            GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);
            If    pJobID IS NOT NULL Then
                  Update_FSLBUILD_Comments(pJobID, vNote);
            
            End If;

      Else
         vNote := 'Errors encountered during FSL BUILD. Failed some validation checks.  Aborting...';
         GZ_UTILITIES.GEN_FSL_TRACKING_LOG(pTopo_IN,'BUILD_FSLS',TrackingTable,p_message=>vNote);
         RAISE_APPLICATION_ERROR(-20001, vNote);      

      End If;
   
   

   END IF;

   ------------------------------------------------------------------------------
   -- Tag the batch job
   APP_INFO_MODULE := 'GZ_FSL.BUILD_FSLS-'||pSchema||','||pProject_ID||','||pTopo_In;
   APP_INFO_ACTION  := 'Begin';
   DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
   ------------------------------------------------------------------------------

End;

--------------------------------------------------------------------------------
END GZ_FSL;
/
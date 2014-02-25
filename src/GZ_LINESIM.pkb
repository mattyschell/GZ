CREATE OR REPLACE PACKAGE BODY GZ_LINESIM
AS
   -- package to drive line simplification and work-sround issues
   -- (like bug in change edge coordinates)
   -- dependencies (not a complete list!)
   -- the rest fo the Generalization programs and especially
   -- GZ_SUPER
   -- GEN_UTIL_ZONE
   -------------------------------------------------------------------------------
   -------------------------------------------------------------------------------
   PROCEDURE PRINT_EDGES_TOSKIP (SOURCE_TOPOLOGY      VARCHAR2,
                                 DERIVED_TOPOLOGY     VARCHAR2,
                                 OUTPUT_TABLE_NAME    VARCHAR2)
   AS
      tplgy1             VARCHAR2 (100) := UPPER (Derived_topology); -- << derived topology : 'Z605LS1'
      tplgy2             VARCHAR2 (100) := UPPER (Source_Topology); -- << source topology  : 'Z605SP'
      -- where the edges to skip during line simplification will be stored.
      -- if the table does not exist, it will be created by the script.  If it already
      -- exists it will be appended
      skip_table         VARCHAR2 (100) := UPPER (OUTPUT_TABLE_NAME);

      TYPE RefCursorType IS REF CURSOR;

      Cur1               RefCursorType;
      sql1               VARCHAR2 (4000);
      sql2               VARCHAR2 (4000);
      island_edge_list   MDSYS.SDO_LIST_TYPE;
      faceid             NUMBER;
      nlei               NUMBER;
      plei               NUMBER;
      nrei               NUMBER;
      prei               NUMBER;
   BEGIN
      IF GZ_SUPER.Table_Exists (skip_table) = FALSE
      THEN
         sql1 :=
            'CREATE TABLE ' || skip_table || ' (EDGE_ID NUMBER) NOLOGGING';

         EXECUTE IMMEDIATE sql1;
      END IF;

      sql1 :=
            'select face_id, island_edge_id_list from '
         || tplgy1
         || '_face$ '
         || ' where face_id in (select distinct a.right_face_id '
         || ' from '
         || tplgy1
         || '_edge$ a, '
         || tplgy2
         || '_edge$ b '
         || ' where a.edge_id=b.edge_id '
         || ' and ((a.left_face_id <> b.left_face_id) '
         || ' or (a.right_face_id <> b.right_face_id)))';
      DBMS_OUTPUT.put_line (sql1);

      OPEN cur1 FOR sql1;

      LOOP
         FETCH cur1
         INTO faceid, island_edge_list;

         EXIT WHEN cur1%NOTFOUND;
         DBMS_OUTPUT.put_line ('Faceid: ' || faceid);

         IF island_edge_list.COUNT > 0
         THEN
            FOR i IN island_edge_list.FIRST .. island_edge_list.LAST
            LOOP
               DBMS_OUTPUT.put_line (
                     'Island Edge List('
                  || i
                  || '): '
                  || ABS (island_edge_list (i)));
               sql1 :=
                  'select abs(next_left_edge_id), abs(prev_left_edge_id), abs(next_right_edge_id), abs(prev_right_edge_id) '
                  || '  from '
                  || tplgy1
                  || '_edge$ where edge_id = :1';

               EXECUTE IMMEDIATE sql1
                  INTO nlei, plei, nrei, prei
                  USING ABS (island_edge_list (i));

               DBMS_OUTPUT.put_line (
                     'Possible edges to eliminate: '
                  || nlei
                  || ', '
                  || plei
                  || ', '
                  || nrei
                  || ', '
                  || prei);
               DBMS_OUTPUT.put_line ('Edge to eliminate: ' || nrei);
               sql2 := 'INSERT INTO ' || skip_table || ' VALUES(:1)';

               EXECUTE IMMEDIATE sql2 USING nrei;

               IF nlei <> nrei
               THEN
                  EXECUTE IMMEDIATE sql2 USING nlei;
               END IF;

               DBMS_OUTPUT.put_line ('---');
            END LOOP;
         ELSE
            DBMS_OUTPUT.put_line (
               'Island Edge List: ' || island_edge_list.COUNT);
         END IF;
      END LOOP;

      CLOSE cur1;

      COMMIT;

      BEGIN
         island_edge_list.delete;
      EXCEPTION
         WHEN OTHERS
         THEN
            DBMS_OUTPUT.put_line (SQLCODE || ': ' || SQLERRM);
      END;
   END PRINT_EDGES_TOSKIP;

   -------------------------------------------------------------------------------
   -------------------------------------------------------------------------------
   FUNCTION START_LOGGING (pSchema IN VARCHAR2, pJobRun IN VARCHAR2)
      RETURN VARCHAR2
   AS
      --Copied from Matt's clipper
      --Create small polygon logging table for this topology
      --The jobrun = the topology you are working on
      vSchema   VARCHAR2 (32) := UPPER (pSchema);
      vJobRun   VARCHAR2 (20) := UPPER (pJobrun);         -- the topology name
   BEGIN
      GZ_BUSINESS_UTILS.CREATE_GEN_XTEND_TRACKING_LOG (
         SYS_CONTEXT ('USERENV', 'CURRENT_USER'),
         pJobRun || '_LS_TRACKING');

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
         'LS',
         pJobRun,
         'START_LS_LOGGING',
         NULL,
         'STARTING JOB: ' || pJobRun);
      RETURN '0';
   END START_LOGGING;

   -----------------------------------------------------------------------------------
   ----------------------------------------------------------------------------------------
   ----------------------------------------------------------------------------------------
   PROCEDURE CREATE_GEN_LS_PARAMETERS (
      p_schema       IN VARCHAR2,
      p_table_name   IN VARCHAR2 DEFAULT 'LINE_SIM_PARAMETERS' -- should change to 'GEN_LS_PARAMETERS'
                                                              )
   AS
      -- Copied from Matt (GZ_CLIP) with minor modifications for small poly parm table.
      -- Stephanie 12/16/2010
      --Creates empty clip parameters table
      --Clip parameters table is a child of the SCHEMA. Theres just one per schema
      -- NOTE:  NEED TO CHANGE DEFAULT NAME to 'GEN_LS_PARAMETERS', but rest of code expects LINE_SIM_PARAMETERS

      psql            VARCHAR2 (4000);
      v_object_root   VARCHAR2 (4000) := p_table_name;                    --??
   BEGIN
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO (
         'Create the empty table as an empty pipelined custom type');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      psql := 'CREATE TABLE ' || p_table_name || ' ';


      psql :=
            psql
         || ' NOPARALLEL NOLOGGING AS '
         || 'SELECT * FROM TABLE('
         || p_schema
         || '.GZ_LINESIM.NEW_LINE_SIM_PARAMETERS ) ';

      BEGIN
         EXECUTE IMMEDIATE psql;
      EXCEPTION
         WHEN OTHERS
         THEN
            IF SQLCODE = -60 OR SQLCODE = -18014
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
               EXECUTE IMMEDIATE
                  'DROP TABLE ' || UPPER (p_table_name) || ' PURGE';

               EXECUTE IMMEDIATE psql;
            ELSE
               RAISE;
            END IF;
      END;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO (
         'Add constraints to ' || p_table_name);

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      EXECUTE IMMEDIATE
            'ALTER TABLE '
         || p_table_name
         || ' '
         || 'ADD ('
         || '   CONSTRAINT '
         || v_object_root
         || 'PKC '
         || '      PRIMARY KEY(GEN_PROJECT_ID) '
         || ')';


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO (
         'Add triggers to ' || p_table_name);

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --only do date, user last modified, and release triggers for permanent parameter tables
      EXECUTE IMMEDIATE
            'CREATE OR REPLACE TRIGGER '
         || v_object_root
         || 'TRG '
         || 'BEFORE INSERT OR UPDATE ON '
         || p_table_name
         || ' '
         || 'FOR EACH ROW '
         || 'BEGIN '
         || '   :NEW.date_last_modified := CURRENT_DATE; '
         || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
         || '   IF :NEW.gen_project_id IS NOT NULL '
         || '   THEN '
         || '      :NEW.gen_project_id := UPPER(:NEW.gen_project_id); '
         || '   END IF; '
         || 'END;';


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO (
         'Grant privileges on ' || p_table_name);

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      EXECUTE IMMEDIATE 'GRANT SELECT ON ' || p_table_name || ' TO "PUBLIC" ';
   END CREATE_GEN_LS_PARAMETERS;

   ----------------------------------------------------------------------------------------
   ----------------------------------------------------------------------------------------
   ----------------------------------------------------------------------------------------


   PROCEDURE CREATE_GEN_LS_TRACKING (
      p_schema       IN VARCHAR2,
      p_table_name   IN VARCHAR2 DEFAULT 'GEN_LS_TRACKING')
   AS
      --Sreeni
      --Using Matt and Stephanie's Log table logic
      --Creates empty sp tracking table
      --There is one for each state
      psql            VARCHAR2 (4000);
      v_object_root   VARCHAR2 (4000) := p_table_name;                    --??
   BEGIN
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      --DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO (
         'Create the empty table as an empty pipelined custom type');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------
      psql := 'CREATE TABLE ' || p_table_name || ' ';
      psql :=
            psql
         || ' NOPARALLEL NOLOGGING AS '
         || 'SELECT * FROM TABLE('
         || p_schema
         || '.GZ_LINESIM.NEW_GEN_LS_TRACKING ) ';

      BEGIN
         EXECUTE IMMEDIATE psql;
      EXCEPTION
         WHEN OTHERS
         THEN
            IF SQLCODE = -60 OR SQLCODE = -18014
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
               EXECUTE IMMEDIATE
                  'DROP TABLE ' || UPPER (p_table_name) || ' PURGE';

               EXECUTE IMMEDIATE psql;
            ELSE
               RAISE;
            END IF;
      END;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      --DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO (
         'Grant privileges on ' || p_table_name);

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------
      EXECUTE IMMEDIATE 'GRANT SELECT ON ' || p_table_name || ' TO "PUBLIC" ';
   END CREATE_GEN_LS_TRACKING;

   --------------------------------------------------------------------------------
   FUNCTION NEW_GEN_LS_TRACKING
      RETURN GZ_TYPES.GEN_LS_TRACKING
      PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_LS_TRACKING;

   -------------------------------------------------------------------------------
   -- need to change default table name and TYPE name
   FUNCTION NEW_LINE_SIM_PARAMETERS
      RETURN GZ_TYPES.LINE_SIM_PARAMETERS
      PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_LINE_SIM_PARAMETERS;

   --------------------------------------------------------------------------------

   PROCEDURE GEN_LS_TRACKING_LOG (p_jobrun       IN VARCHAR2,
                                  p_process      IN VARCHAR2,
                                  p_table_name   IN VARCHAR2 DEFAULT NULL,
                                  p_step         IN VARCHAR2 DEFAULT NULL,
                                  p_start_time   IN TIMESTAMP DEFAULT NULL,
                                  p_end_time     IN TIMESTAMP DEFAULT NULL,
                                  p_sqlstmt      IN VARCHAR2 DEFAULT NULL,
                                  p_message      IN VARCHAR2 DEFAULT NULL -- SK Changed to p_message from p_err_msg
                                                                         )
   AS
      PRAGMA AUTONOMOUS_TRANSACTION;
      --COPIED from Matt's Gen Clip Tracking Log
      -- DO NOT USE!!!!
      -- NOT FINISHED!!!! -------------------------
      --If the caller is tracking start and end times and both are relevant, pass them both in
      --   Or just pass in the start and we'll calculate the current time as the end
      --If not really tracking elapsed time, dont pass in either and we'll put the current time
      --   in both the start time and the end time, no elapsed
      psql           VARCHAR2 (4000);
      v_start_time   TIMESTAMP;
      v_end_time     TIMESTAMP;
      elapsed_time   INTERVAL DAY (5) TO SECOND (2);
   BEGIN
      -- DBMS_OUTPUT.PUT_LINE('Begin Insert');
      IF p_start_time IS NOT NULL AND p_end_time IS NULL
      THEN
         v_start_time := p_start_time;
         v_end_time := SYSTIMESTAMP;
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
         v_start_time := SYSTIMESTAMP;
         v_end_time := SYSTIMESTAMP;
      END IF;

      psql :=
            'INSERT /*+ APPEND */ INTO '
         || p_jobrun
         || '_LS_TRACKING '
         || 'VALUES(:p1,:p2,:p3,:p4,:p5,:p6,:p7,:p8,:p9,:p10) ';

      EXECUTE IMMEDIATE psql
         USING p_jobrun,
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
   END GEN_LS_TRACKING_LOG;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   PROCEDURE LINE_SIM (pSchema              IN VARCHAR2,
                       pProjectId           IN VARCHAR2,
                       pJobId               IN VARCHAR2,
                       pTopo                IN VARCHAR2,
                       pModules             IN VARCHAR2,
                       pFromTopo            IN VARCHAR2,
                       pTopoBk              IN VARCHAR2,
                       pStateOutlineTable   IN VARCHAR2,
                       pStateFP             IN VARCHAR2,
                       pSkipEdgesTable      IN VARCHAR2,
                       p_validate_topo      IN VARCHAR2 DEFAULT 'Y',
                       p_fix_edge           IN VARCHAR2 DEFAULT 'Y',
                       p_fix_2edge          IN VARCHAR2 DEFAULT 'Y',
                       p_topofix_qa         IN VARCHAR2 DEFAULT 'Y' 
                       )
   AS
      --Matt! 12/20/12 added skip edges input sources from gz_linesim_setup instead of
      --               line_sim_parameters source within here and pass it through to GZ_SUPER.simplify
      --Matt! 6/7/13 added topofix_qa management
      --M@! 8/21/13 added topofix_qa management to fix_face results.  Oversight in 6/7 update
      --Matt! 12/4/13 added fix edge and close edge nonsense
      --M! 12/30/13 Rearranged the topo validation and topofix deck chairs again

      vSchema                VARCHAR2 (4000) := UPPER (pSchema); --     := 'GZ_SDRP10_Z6';
      vProjectID             VARCHAR2 (4) := UPPER (pProjectId);
      vTopo                  VARCHAR2 (4000) := UPPER (pTopo);
      vModules               VARCHAR2 (20) := UPPER (pModules);
      vFromTopo              VARCHAR2 (20) := UPPER (pFromTopo);
      vTopoBk                VARCHAR2 (20) := UPPER (pTopoBk);
      pEntity_Table          VARCHAR2 (4000) := UPPER (pStateOutlineTable); --    := 'ACS10_SL040';
      vStateFP               VARCHAR2 (20) := UPPER (pStateFP);
      vSrcTopo               VARCHAR2 (4000) := UPPER (pFromTopo);
      vRunFlag               VARCHAR2 (4000) := vTopo;
      vJobID                 VARCHAR2 (20) := UPPER (pJobId);
      vProcess               VARCHAR2 (100)
         := 'Line Simplification ' || vProjectID || ' ' || vStateFP;
      vStepNum               NUMBER;
      vStep                  VARCHAR2 (4000);
      vLSModules             GZ_TYPES.stringarray;
      vReRun                 VARCHAR2 (10);

      /*
        The following block will be read/processed based on the linsim_parameter table
      */
      pmethod                VARCHAR2 (4000);            --         := 'ZONE';
      ptarget_scale          NUMBER;               --              := 500000.;
      pnice                  NUMBER;                 --                 := 1.;
      pedges_table           VARCHAR2 (4000);   --       := 'EDGES_TOPROCESS';
      pDone_edges_table      VARCHAR2 (4000);             --  := 'EDGES_DONE';
      pBad_Table             VARCHAR2 (4000);       --         := 'BAD_EDGES';
      pState_Edge_Table      VARCHAR2 (4000);       --  := 'STATE_EDGE_TABLE';
      pEntityfp              VARCHAR2 (4000);         --         := 'STATEFP';
      pEdge_table            VARCHAR2 (4000); --        := vTopo||'_EDGE_ATT';
      pSrcEdge_table         VARCHAR2 (4000); --     := vSrcTopo||'_EDGE_ATT';
      pface                  VARCHAR2 (4000); --         := vTopo||'_CLIP_FACE';
      vUpdateLog             VARCHAR2 (4000);
      vLogNote               VARCHAR2 (4000);
      vLogTime               TIMESTAMP;
      vSql                   VARCHAR2 (4000);
      --vStates GZ_TYPES.stringarray;
      vRes                   BOOLEAN;
      vOutput                VARCHAR2 (10);
      vCheckCount            NUMBER;
      APP_INFO_MODULE        VARCHAR2 (48);
      APP_INFO_ACTION        VARCHAR2 (32);
      APP_INFO_CLIENT_INFO   VARCHAR2 (64);
      vNote                  VARCHAR2 (4000) := NULL;
      retval                 VARCHAR2 (8000);
      sql1                   VARCHAR2 (4000);
      rowCount               NUMBER;
      tblName                VARCHAR2 (100);
      TABLE_ARR              MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY ();
      lineSimPrmRec          GZ_TYPES.LINE_SIM_PARAMETERS_REC;

      vArea_Check            NUMBER;
      vPercent_Ungen         NUMBER;
      vMax_Consec_Match      NUMBER;

      vCAVMatchLength        INTEGER;
      vCAVIncStEdges         VARCHAR2 (3);
      vCAVPPrint             VARCHAR2 (3);

      vQaTolerance           NUMBER;
      vQaEdgesTable          VARCHAR2 (100);
      vEdgeDollarTable       VARCHAR2 (100);
      vQaReportTable         VARCHAR2 (100);
      vReportGeomColumn      VARCHAR2 (100);
      vUnGenEdgeCount        NUMBER;
      vUnGenPCT              NUMBER;
      vEdgeCount             NUMBER;

      Success_Flag           CHAR := 'Y';
      edgefix_val            VARCHAR2 (1);
      QA_Note                VARCHAR2 (2000) := 'QA_Note: ';
      vRelease               VARCHAR2 (10);
      topofix_qa             VARCHAR2 (1);

      v_state_tab_srid       VARCHAR2 (4000);
      v_topo_srid            VARCHAR2 (4000);

      -----------------------------------
      -- Added by Stephanie to check for topologies with no internal edges
      -- so we can skip line simplification if that happens

      vInternal_Edge_Count   NUMBER;
      vInternal_Edge_Sql     VARCHAR2 (4000);
   -----------------------------------


   BEGIN
      APP_INFO_MODULE :=
            'GZ_LINESIM.LINE_SIM-'
         || vSchema
         || ','
         || vProjectID
         || ','
         || vTopo;
      APP_INFO_ACTION := 'Step 0';
      DBMS_APPLICATION_INFO.SET_MODULE (APP_INFO_MODULE, APP_INFO_ACTION);
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO ('Line Sim Work: Begin');


      vLSModules := GZ_BUSINESS_UTILS.split (vModules);

      IF vLSModules.COUNT <> 10
      THEN
         vNote := 'ERROR - wrong number of modules to run';
         DBMS_OUTPUT.put_line (vNote);
         RAISE_APPLICATION_ERROR (
            -20001,
            'Wrong number of modules (LSModules) passed');
      END IF;

      IF vLSModules (3) = 'N'
      THEN
         vReRun := 'Y';
      ELSE
         vReRun := 'N';
      END IF;

      --vUpdateLog := 'INSERT INTO GEN_LS_LOG VALUES(:p1,:p2)';
      --FOR i in 1..vStates.COUNT
      --LOOP
      --SK  vSrcTopo := 'Z6' ||vStates(i)|| 'SP5';
      --SK  vTopo    := 'Z6' ||vStates(i)|| 'LS1';
      --SK  vRunFlag := vTopo;
      --SK  pEdge_table := vTopo||'_EDGE_ATT';
      --SK  pSrcEdge_table := vSrcTopo||'_EDGE_ATT';
      --SK  pface := vTopo||'_CLIP_FACE';
      DBMS_OUTPUT.put_line (
         '--------------------------------------------------------');

      /*
         =================================================================================
         Description of all steps that can be controlled by setting the Modules parameter.
         =================================================================================
         SubStr    Step  Descr
          1          10      copy topology to new topology before running.
          2          20      Create New Edge Attribute table
          3          30      Run line simplification
          4          40      QA: Check for un generalized edges
          5          50      Validate Feature tables
          6          60      QA1: Check count with Sidey's query
          7          70      QA2: Check for L/R face changes
          8          80      Update Face Measurments
          9          90      Validate ALL FACES to see if it is ready for FSL build.
         10        100      Validate Topology (Oracle)
         =================================================================================
      */

      ----------------------------------------------------------------------------
      IF vReRun = 'N'
      THEN
         retval := GZ_LINESIM.START_LOGGING (vSchema, vTopo);
         DBMS_OUTPUT.PUT_LINE ('RetVal: ' || retVal);

         IF retval != '0'
         THEN
            RAISE_APPLICATION_ERROR (
               -20001,
               'Problem with START_LOGGING: ' || retval);
         ELSE
            DBMS_OUTPUT.PUT_LINE (
               'Created Log Table ' || vTopo || '_LS_TRACKING');
         END IF;
      ELSE
         -- Check for tracking table
         sql1 := 'Select count(*) from user_tables where table_name =   :1';

         EXECUTE IMMEDIATE sql1 INTO rowCount USING vTopo || '_LS_TRACKING';

         IF rowCount = 0
         THEN
            RAISE_APPLICATION_ERROR (
               -20001,
               'Tracking table must exist for a re-run');
         END IF;
      END IF;

      vNote :=
            'Input Parameters: '
         || ' pSchema: '
         || pSchema
         || ' pProjectId: '
         || pProjectId
         || ' pJobId: '
         || pJobId
         || ' pTopo: '
         || pTopo
         || ' pModules: '
         || pModules
         || ' pFromTopo: '
         || pFromTopo
         || ' pTopoBk: '
         || pTopoBk
         || ' pStateOutlineTable: '
         || pStateOutlineTable
         || ' pStateFP: '
         || pStateFP
         || ' pSkipEdgesTable: '
         || pSkipEdgesTable
         || ' p_validate_topo: '
         || p_validate_topo
         || ' p_fix_edge: '
         || p_fix_edge
         || ' p_fix_2edge: '
         || p_fix_2edge
         || ' p_topofix_qa: '
         || p_topofix_qa;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                   vTopo,
                                                   vProcess,
                                                   p_step        => vStepNum,
                                                   p_sqlstmt     => vStep,
                                                   p_error_msg   => vNote);

      DBMS_OUTPUT.put_line (vNote);

      IF p_topofix_qa IS NULL OR p_topofix_qa = 'Y'
      THEN
      
         --SOP. We want to fail after LS if there are too close edges or invalid faces
         topofix_qa := 'Y';
         
      ELSE
      
         --dont plan to let qa slide at LS module
         topofix_qa := 'N';
         
      END IF;

      sql1 :=
         'SELECT target_scale, release FROM gz_job_setup WHERE jobid =   :1';

      EXECUTE IMMEDIATE sql1 INTO ptarget_scale, vRelease USING vJobID;

      lineSimPrmRec := GET_LINESIM_PROJECT_PARAMETERS (vProjectID, vRelease);
      pmethod := lineSimPrmRec.METHOD;
      pnice := lineSimPrmRec.NICE;
      pedges_table := lineSimPrmRec.EDGES_TABLE;
      pDone_edges_table := lineSimPrmRec.DONE_EDGES_TABLE;
      pBad_Table := lineSimPrmRec.BAD_TABLE;
      pState_Edge_Table := lineSimPrmRec.STATE_EDGE_TABLE;
      pEntityfp := lineSimPrmRec.ENTITYFP;
      pEdge_table := vTopo || lineSimPrmRec.EDGE_TABLE;
      pSrcEdge_table := vSrcTopo || lineSimPrmRec.SRCEDGE_TABLE;
      pface := vTopo || lineSimPrmRec.FACETABLE;

      --QA variables used in step 40
      vQaTolerance := lineSimPrmRec.UnGenQaLimitPct;

      vArea_Check := lineSimPrmRec.AREA_CHECK;
      vPercent_Ungen := lineSimPrmRec.PERCENT_UNGEN;
      vMax_Consec_Match := lineSimPrmRec.MAX_CONSEC_MATCH;

      vCAVMatchLength := lineSimPrmRec.CAV_MATCH_LENGTH;
      vCAVIncStEdges := lineSimPrmRec.CAV_INC_ST_EDGES;
      vCAVPPrint := lineSimPrmRec.CAV_PPRINT;

      vQaEdgesTable := vTopo || pedges_table;
      vEdgeDollarTable := vTopo || '_EDGE$';
      vQaReportTable := vTopo || '_REPORT';
      vReportGeomColumn := 'GEOMETRY';



      -- Validate Parameters
      -- Check if vFromTopo exists
      sql1 := 'Select count(*) from user_sdo_topo_info where topology = :1';

      EXECUTE IMMEDIATE sql1 INTO rowCount USING vFromTopo;

      IF rowCount = 0
      THEN
         vNote :=
               'Topology '
            || vFromTopo
            || ' does not exist.  Terminating execution';
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         DBMS_OUTPUT.put_line ('ERROR - ' || vNote);
         RAISE_APPLICATION_ERROR (-20001, vNote);
      END IF;

      -- Check if vTopo exists
      sql1 := 'Select count(*) from user_sdo_topo_info where topology = :1';

      EXECUTE IMMEDIATE sql1 INTO rowCount USING vTopo;

      IF rowCount > 0
      THEN
         IF vReRun = 'N'
         THEN
            vNote :=
                  'Rerun flag is set to '
               || vReRun
               || ' Topology '
               || vTopo
               || ' exists.  Terminating execution';
            vNote := 'Topology ' || vTopo || ' exists.  It will be purged';
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);

            DBMS_OUTPUT.put_line (vNote);
         --RAISE_APPLICATION_ERROR(-20001, vNote);
         ELSE
            vNote :=
                  'Rerun flag is set to '
               || vReRun
               || ' Topology '
               || vTopo
               || ' exists.  Continue execution';
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
         END IF;
      END IF;

      -- Check if vTopobk exists
      sql1 := 'Select count(*) from user_sdo_topo_info where topology = :1';

      EXECUTE IMMEDIATE sql1 INTO rowCount USING vTopoBk;

      IF rowCount > 0
      THEN
         vNote := 'Topology ' || vTopoBk || ' exists.';
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         DBMS_OUTPUT.put_line (vNote);
      -- RAISE_APPLICATION_ERROR(-20001, vNote);
      END IF;

      -- vStateOutlineTable table should exist
      -- Check for existance  ----------------------------------------
      sql1 := 'Select count(*) from user_tables where table_name = :1';

      EXECUTE IMMEDIATE sql1 INTO rowCount USING pEntity_Table;

      IF rowCount = 0
      THEN
         vNote :=
               'State Outline Table '
            || pEntity_Table
            || ' does not exist.  Terminating execution';
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         DBMS_OUTPUT.put_line ('ERROR - ' || vNote);
         RAISE_APPLICATION_ERROR (-20001, vNote);
      END IF;

      --------------------------------------------------
      -- vStateOutlineTable should have the same SRID as the src TOPO edge table.
      --------------------------------------------------
      -- Stephanie added 8/5/2013 to identify a problem that sometimes
      -- doesn't cause a line sim failure, but always should.
      -- pre check the STATE table to make sure the SRID of the
      -- sdogeometry column matches the one for the edges in the topology we
      -- are generalizing.  If not, fail and explain in the log...

      -- using the primitive "GOTO" and I know I am not supposed to,
      -- but if it is good enough for William, it's good enough for me.

      sql1 :=
            'SELECT DISTINCT st.sdogeometry.sdo_srid FROM '
         || pEntity_Table
         || ' st';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                   vTopo,
                                                   vProcess,
                                                   p_step      => vStepNum,
                                                   p_sqlstmt   => sql1);

      EXECUTE IMMEDIATE sql1 INTO v_state_tab_srid;

      sql1 :=
            'SELECT DISTINCT e.geometry.sdo_srid FROM '
         || vSrcTopo
         || '_edge$ e';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                   vTopo,
                                                   vProcess,
                                                   p_step      => vStepNum,
                                                   p_sqlstmt   => sql1);

      EXECUTE IMMEDIATE sql1 INTO v_topo_srid;

      IF v_topo_srid <> v_state_tab_srid
      THEN
         vNote :=
               'Can not run linesim.  The SRID for '
            || vSrcTopo
            || ' is '
            || v_topo_srid
            || ' and the SRID for the state table ('
            || pEntity_Table
            || ') is '
            || v_state_tab_srid
            || '.  Terminating execution.  Maybe you need to change the state table you are using to help line sim get state MBRs.';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => 'FAILED',
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         DBMS_OUTPUT.put_line (
               '---------------------------------'
            || CHR (10)
            || 'ERROR - '
            || vNote
            || CHR (10)
            || '---------------------------------');

         RAISE_APPLICATION_ERROR (-20001, vNote);
      END IF;

      --------------------------------------------------
      -- Check and drop if tmp wrk tables (EDGES_TOPROCESS, and EDGES_DONE) exist
      vNote :=
            'Now check for tmp tbls pEdges_table: '
         || vTopo
         || pedges_table
         || ' pDone_edges_table: '
         || vTopo
         || pDone_edges_table;
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                   vTopo,
                                                   vProcess,
                                                   p_step        => vStepNum,
                                                   p_sqlstmt     => vStep,
                                                   p_error_msg   => vNote);

      sql1 := 'Select count(*) from user_tables where table_name in (:1, :2)';

      EXECUTE IMMEDIATE sql1
         INTO rowCount
         USING vTopo || pedges_table, vTopo || pDone_edges_table;

      IF rowCount > 0
      THEN
         vNote := 'Found tables temp work tables. Drop them';
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         -- Add code to purge tables
         sql1 := 'Select TABLE_NAME as table_name From user_tables ';
         sql1 := sql1 || ' where table_name in (:1, :2)';
         DBMS_OUTPUT.put_line (sql1 || tblname);

         EXECUTE IMMEDIATE sql1
            BULK COLLECT INTO table_arr
            USING vTopo || pedges_table, vTopo || pDone_edges_table;

         FOR i IN table_arr.FIRST .. table_arr.LAST
         LOOP
            sql1 :=
               'Drop table ' || vschema || '.' || table_arr (i) || ' PURGE';
            DBMS_OUTPUT.put_line (sql1);

            EXECUTE IMMEDIATE sql1;
         END LOOP;
      ELSE
         vNote :=
               vTopo
            || pedges_table
            || ' and '
            || vTopo
            || pDone_edges_table
            || ' tables dont exist.  Need not drop them';
         DBMS_OUTPUT.put_line (vNote);
      END IF;

      --------------------------------------------------------------------------------
      -- copy topology to new topology before running.
      vStepNum := 10;
      vStep :=
            'Copy topology from '
         || vSchema
         || '.'
         || vSrcTopo
         || ' to '
         || vSchema
         || '.'
         || vTopo;

      IF vLSModules (1) = 'Y'
      THEN
         DBMS_APPLICATION_INFO.SET_ACTION (vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO ('Copy topology');
         vNote := 'Begin ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         gz_topo_util.copy_topology (vSchema,
                                     vSrcTopo,
                                     vSchema,
                                     vTopo,
                                     'Y',
                                     'N');
         vNote := 'End ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      ELSE
         vNote := 'Step ' || vStepNum || ' - SKIPPING ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      END IF;

      --------------------------------------------------------------------------------
      -- check to see if the topology has an generalizable edges  Edges on the topology
      -- boundary are not generalizable.

      vInternal_Edge_Sql :=
            'select count(edge_id) from '
         || vEdgeDollarTable
         || ' where left_face_id != -1 AND right_face_id != -1';

      EXECUTE IMMEDIATE vInternal_Edge_Sql INTO vInternal_Edge_Count;

      IF vInternal_Edge_Count > 0
      THEN
         vNote :=
            vInternal_Edge_Count
            || ' internal Edges were found, continuing with line simplification.';
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      ELSE
         vNote :=
            'There are no internal edges in the ' || vtopo
            || ' topology.  Line Simplification will not change anything, so we can skip it.';
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      END IF;

      --------------------------------------------------------------------------------
      -- add edge attribute table
      vStepNum := 20;
      vStep := 'Create New Edge Attribute table for ' || vTopo;

      IF vLSModules (2) = 'Y' AND vInternal_Edge_Count > 0
      THEN
         DBMS_APPLICATION_INFO.SET_ACTION (vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO ('Create Edge Attr Tbl');
         vNote := 'Begin ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);


         gz_smpoly.LOAD_EDGE_SIMPLE (vSchema, vTopo);
         vNote := 'End ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      ELSE
         vNote := 'Step ' || vStepNum || ' - SKIPPING ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      END IF;

      DBMS_OUTPUT.put_line ('----------Begin Step 30 -------------------');



      --------------------------------------------------------------------------------
      -- run line simplification (skip if you already decided to fail)
      vStepNum := 30;
      vStep := 'Line Simplification';

      IF     vLSModules (3) = 'Y'
         AND vInternal_Edge_Count > 0
         AND Success_Flag <> 'N'
      THEN
         DBMS_APPLICATION_INFO.SET_ACTION (vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO ('Line Simplification');
         vNote := 'Begin ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);


         vNote :=
               vStateFP
            || ';'
            || vTopo
            || ';'
            || vRunFlag
            || ';'
            || pmethod
            || ';'
            || ptarget_scale
            || ';'
            || pnice
            || ';'
            || pEntity_Table
            || ';'
            || pedges_table
            || ';'
            || pSkipEdgesTable
            || ';'
            || pDone_edges_table
            || ';'
            || pBad_Table
            || ';'
            || pState_Edge_Table
            || ';'
            || pEntityfp
            || ';'
            || pEdge_table
            || ';'
            || pface;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         GZ_SUPER.simplify (vStateFP,                           -- vStates(i),
                            vTopo,
                            vRunFlag,
                            pmethod,
                            ptarget_scale,
                            pnice,
                            pEntity_Table,
                            pedges_table,
                            pSkipEdgesTable,
                            pDone_edges_table,
                            pBad_Table,
                            pState_Edge_Table,
                            pEntityfp,
                            pEdge_table,
                            pface);
         vNote := 'End ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      ELSE
         vNote := 'Step ' || vStepNum || ' - SKIPPING ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      END IF;

      DBMS_OUTPUT.put_line (
         '--------------------Done with Step 30-------------------------------');
      --------------------------------------------------------------------------------
      -- QA Step:  Check All Vertices -- Check for un generalized edges
      vStepNum := 40;
      vStep := 'QA Step -- Check All Vertices';

      IF vLSModules (4) = 'Y' AND vInternal_Edge_Count > 0
      THEN
         DBMS_APPLICATION_INFO.SET_ACTION (vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO (vStep);
         vNote := 'Begin ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         DBMS_OUTPUT.PUT_LINE (
               'vQaEdgesTable: '
            || vQaEdgesTable
            || 'vReportGeomColumn: '
            || vReportGeomColumn
            || 'vEdgeDollarTable: '
            || vEdgeDollarTable
            || 'vReportGeomColumn: '
            || vReportGeomColumn
            || 'vQaReportTable: '
            || vQaReportTable);
         -- GZ_QA.CHECK_ALL_VERTICES( pInTable VARCHAR2, pGeometry_column VARCHAR2, pInTable2 VARCHAR2,
         --                                              pnewGeometry_column VARCHAR2, pOutput_table VARCHAR2, match_length PLS_INTEGER default 4,
         --                                              area_check NUMBER, percent_ungen NUMBER default 20., max_consecutive_match NUMBER default 20.,
         --                                              pInclude_state_edges VARCHAR2 default 'NO', pprint VARCHAR2 default 'NO') AS

         DBMS_OUTPUT.PUT_LINE (
               'GZ_QA.CHECK_ALL_VERTICES('
            || vQaEdgesTable
            || ','
            || vReportGeomColumn
            || ','
            || vEdgeDollarTable
            || ','
            || vReportGeomColumn
            || ','
            || vQaReportTable
            || ','
            || vCAVMatchLength
            || ','
            || vArea_Check
            || ','
            || vPercent_Ungen
            || ','
            || vMax_Consec_Match
            || ','
            || vCAVIncStEdges
            || ','
            || vCAVPPrint
            || ');');


         GZ_QA.CHECK_ALL_VERTICES (vQaEdgesTable,
                                   vReportGeomColumn,
                                   vEdgeDollarTable,
                                   vReportGeomColumn,
                                   vQaReportTable,
                                   vCAVMatchLength,
                                   vArea_Check,
                                   vPercent_Ungen,
                                   vMax_Consec_Match,
                                   vCAVIncStEdges,
                                   vCAVPPrint);

         vNote :=
               'vQaReportTable: '
            || vQaReportTable
            || ' vEdgeDollarTable: '
            || vEdgeDollarTable;
         DBMS_OUTPUT.put_line (vNote);

         sql1 :=
               'Select count(*) from '
            || vQaReportTable
            || '  where percent_ungen = 100';

         EXECUTE IMMEDIATE sql1 INTO vUnGenEdgeCount; -- using vQaReportTable;


         sql1 := 'Select count(*) from ' || vEdgeDollarTable;

         EXECUTE IMMEDIATE sql1 INTO vEdgeCount;

         sql1 := 'select (:1/:2)*100 from dual';

         EXECUTE IMMEDIATE sql1
            INTO vUnGenPCT
            USING vUnGenEdgeCount, vEdgeCount;

         vNote :=
               'UnGenEdgeCount = '
            || vUnGenEdgeCount
            || ' Total Edges = '
            || vEdgeCount
            || ' unGenPCT = '
            || vUnGenPCT
            || '  QA Tolerance = '
            || vQaTolerance;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         IF vUnGenPCT > vQaTolerance
         THEN
            vNote := 'Too many un generalized edges....  stop execution!';
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);

            Success_Flag := 'N';
            QA_Note :=
               QA_Note || ' Failed Step 40. Too many ungeneralized edges. ';
         --RAISE_APPLICATION_ERROR(-20001,vNote);

         ELSE
            vNote :=
               'Un Generalized edges are within limit...  can continue to next step';
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
         END IF;

         vNote := 'End ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      ELSE
         vNote := 'Step ' || vStepNum || ' - SKIPPING ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      END IF;



      --------------------------------------------------------------------------------
      -- validate topology...
      vStepNum := 50;
      vStep := 'Validate Feature tables';

      IF vLSModules (5) = 'Y' AND vInternal_Edge_Count > 0
      THEN
         DBMS_APPLICATION_INFO.SET_ACTION (vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO (vStep);
         vNote := 'Begin ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         vRes := gz_topo_util.validate_feature_tables (vSchema, vtopo);

         IF vRes
         THEN
            vNote := 'VALID: All Feature Tables are valid in ' || vtopo;
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
         ELSE
            vNote :=
               'INVALID: Some Feature Table records are not valid in '
               || vtopo;
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
            RAISE_APPLICATION_ERROR (-20001, vNote);
         END IF;

         vNote := 'End ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      ELSE
         vNote := 'Step ' || vStepNum || ' - SKIPPING ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      END IF;

      ---- Check count with Sidey's query.  If this is zero, its perfect.

      vStepNum := 60;
      vStep := 'QA1: Sidey''s Query';

      IF vLSModules (6) = 'Y' AND vInternal_Edge_Count > 0
      THEN
         DBMS_APPLICATION_INFO.SET_ACTION (vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO (vStep);
         vNote := 'Begin ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         vCheckCount := 0;
         -- if this result comes out 0, then all is right with the world,
         -- otherwise, we have to get Sidey to QA it.


         -- !Stephanie -- 20101217 removed hard coded edges to process table name.

         --vsql := 'select count(1) from '||vTopo||'edges_toprocess a,'||
         --     vTopo||'_EDGE$ t '||
         --     'where sdo_util.getnumvertices(a.new_geometry) <> '||
         --     'sdo_util.getnumvertices(t.geometry) and a.edge_id=t.edge_id '||
         --     'and a.ignore <> 1';
         vsql :=
               'select count(1) from '
            || vTopo
            || pedges_table
            || ' a,'
            || vTopo
            || '_EDGE$ t '
            || 'where sdo_util.getnumvertices(a.new_geometry) <> '
            || 'sdo_util.getnumvertices(t.geometry) and a.edge_id=t.edge_id '
            || 'and a.ignore <> 1';

         EXECUTE IMMEDIATE vsql INTO vCheckCount;

         IF vCheckCount = 0
         THEN
            vNote := 'QA Step60 - GOOD. Sidey''s SQL count query = 0.   ';
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
         ELSE
            vNote :=
               'QA Step60 - BAD. Sidey''s SQL count query = ' || vCheckCount
               || '. This is an informational message only.  Pay more attention to other QA checks';
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
         --  RAISE_APPLICATION_ERROR(-20001,vNote);
         END IF;

         vNote := 'End ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      ELSE
         vNote := 'Step ' || vStepNum || ' - SKIPPING ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      END IF;

      -- when QAing - you can ignore the 13356 oracle errors, and self
      -- intersecting errors, but make sure there are no duplicate edge_ids in the
      -- bad edges table, because that is a big problem
      ----------------------------------------------------------------------------
      -- Also Check for left/right face changes.  This is a bug!
      -- If it happens, you'll have to skip the edges involved, or wait for
      -- Sidey's new code before running again.
      vStepNum := 70;
      vCheckCount := 0;
      vStep := 'QA2: Check for L/R face changes';

      IF vLSModules (7) = 'Y' AND vInternal_Edge_Count > 0
      THEN
         DBMS_APPLICATION_INFO.SET_ACTION (vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO (vStep);
         vNote := 'Begin ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         vSql :=
               'select count(*) '
            || 'from '
            || vTopo
            || '_edge$ a,'
            || vSrcTopo
            || '_edge$ b '
            || 'where a.edge_id = b.edge_id and '
            || '(a.left_face_id <> b.left_face_id or a.right_face_id <> b.right_face_id)';

         EXECUTE IMMEDIATE vSql INTO vCheckCount;

         IF vCheckCount <> 0
         THEN
            -- log an error
            vNote :=
                  'FAILED! Found a mismatch on l/r faces between '
               || vSrcTopo
               || ' and '
               || vTopo;
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);

            Success_Flag := 'N';
            QA_Note := QA_Note || ' Failed QA Step 70';
         --RAISE_APPLICATION_ERROR(-20001,vNote);
         ELSE
            vNote := 'PASSED! No l/r face mismatched found. ' || vTopo;
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
         END IF;

         vNote := 'End ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      ELSE
         vNote := 'Step ' || vStepNum || ' - SKIPPING ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      END IF;

      ----------------
      -- Check and fix conflicting edges...
      vStepNum := 75;
      vStep := 'Check and fix conflicting edges ' || vTopo || '_EDGE$';

      IF vLSModules (8) = 'Y' AND vInternal_Edge_Count > 0
      AND p_fix_edge = 'Y'
      THEN
      
         DBMS_APPLICATION_INFO.SET_ACTION (vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO (
            'Check and fix conflict edges');
         vNote := 'Begin ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         edgefix_val :=
            GZ_TOPOFIX.GZ_FIX_EDGE (pJobId,
                                    vTopo,
                                    'LS',
                                    'Y',         --Yes, hold universal face
                                    0.05,        --no tolerance in this module?
                                    NULL,        --No fixed loop count. Continue as long as theres progress
                                    p_fix_2edge); --check for close pairs of edges, expensive but this is the best chance
                                                  --for thinned coordinates and parallel processing (in state step 2 runs)


         IF edgefix_val = '0'
         THEN
         
            vNote := 'Step ' || vStepNum || ' - Finished Successful ' || vStep;
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
               
         ELSIF edgefix_val = '1' 
         AND topofix_qa = 'Y'
         THEN
         
            --Failed fix edge and we do want to fail the module

            vNote := 'Step ' || vStepNum || ' - Fail ' || vStep;
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);

            Success_Flag := 'N';
            
         ELSIF edgefix_val = '1' 
         AND topofix_qa = 'N'
         THEN
         
            --Failed fix edge but we want to just continue.  Could be we have a good
            --reason for this and are overriding via the gz_linesim_setup.topofix_qa value

            vNote :=
                  'Step '
               || vStepNum
               || ' - Fail '
               || vStep
               || ' but we wont fail LS for it ';
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
               
         ELSE
         
            vNote :=
               'Step ' || vStepNum || ' - Unknown edgefix fail ' || vStep;
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
            Success_Flag := 'N';
            RAISE_APPLICATION_ERROR (-20001, vNote);
            
         END IF;
      ELSE
      
         vNote := 'Step ' || vStepNum || ' - SKIPPING ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      END IF;

      DBMS_OUTPUT.put_line (
         '--------------------------------------------------------');

      -- update face measurements...
      vStepNum := 80;
      vStep := 'Update Face Measurments for ' || pFace;

      IF vLSModules (8) = 'Y' AND vInternal_Edge_Count > 0
      THEN
         DBMS_APPLICATION_INFO.SET_ACTION (vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO ('Update Face Measurements');
         vNote := 'Begin ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         GZ_BUSINESS_UTILS.UPDATE_FACE_MEASUREMENTS (pface);
         vNote := 'End ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      ELSE
         vNote := 'Step ' || vStepNum || ' - SKIPPING ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      END IF;

      DBMS_OUTPUT.put_line (
         '--------------------------------------------------------');


      -- Validate ALL FACES to see if it is ready for FSL build.
      vStepNum := 90;
      vStep := 'Validate all faces';

      IF vLSModules (9) = 'Y' AND 
      vInternal_Edge_Count > 0
      THEN
      
         DBMS_APPLICATION_INFO.SET_ACTION (vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO (vStep);
         vNote := 'Begin ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         vRes := FALSE;
         -- Sreeni 2012/01/26 Adding vTopo to the following call.
         vOutput :=
            GZ_TOPOFIX.check_face_tab (pJobId,
                                       vTopo,
                                       pFace,
                                       'FACE_ID',
                                       'LS');

         IF (vOutput = '0')
         THEN
         
            --DBMS_OUTPUT.put_line ('TRUE');
            
            vNote :=
                  'PASS - '
               || vTopo
               || ' ready for FSL build if steps 60 and 70 passed.';
               
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
               
         ELSIF VOutput <> '0'
         AND topofix_qa = 'Y'
         THEN
         
            --DBMS_OUTPUT.put_line ('FALSE');
            
            vNote :=
                  'FAIL - in '
               || vTopo
               || ' some faces didn''t validate.  Fix them before FSL build.';
               
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);

            Success_Flag := 'N';
            QA_Note :=
               QA_Note || ' Failed Step 90. Unable to validate all faces. ';
         
         ELSIF VOutput <> '0'
         AND topofix_qa = 'N'
         THEN
         
            vNote :=
                  'FAIL - in '
               || vTopo
               || ' some faces didn''t validate.  But topofix_qa is set to N, so we wont fail the job.';
               
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);
         
         END IF;

         vNote := 'End ' || vStep;
         
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
                                                      
      ELSE
      
         vNote := 'Step ' || vStepNum || ' - SKIPPING ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
      END IF;

      -- Validate Topology
      vStepNum := 100;
      vStep := 'Validate Topology ' || vTopo;

      IF vLSModules (10) = 'Y' 
      AND vInternal_Edge_Count > 0
      AND p_validate_topo = 'Y'
      THEN
      
         DBMS_APPLICATION_INFO.SET_ACTION (vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO ('Validate Topology');
         
         vNote := 'Step ' || vStepNum || ' - Begin ' || vStep;
         
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);

         BEGIN
         
            rowCount := 0;
            sql1 := 'Select count(*) From ' || vTopo || '_EDGE$';

            EXECUTE IMMEDIATE sql1 INTO rowCount;

            retVal := 'FALSE';
            --retVal := SDO_TOPO_MAP.VALIDATE_TOPOLOGY( vTopo, 'TRUE', 1);
            retVal := GZ_TOPO_UTIL.VALIDATE_TOPOLOGY (vtopo, rowCount);

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);

            IF retVal <> 'TRUE'
            THEN
               DBMS_OUTPUT.put_line (
                  'Source Topology ' || vTopo || ' is not valid');
               RAISE_APPLICATION_ERROR (-20001, retVal);
            END IF;


            vNote := 'Step ' || vStepNum || ' Successfully validated topology';
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               'LS',
               vTopo,
               vProcess,
               p_step        => vStepNum,
               p_sqlstmt     => vStep,
               p_error_msg   => vNote);

            DBMS_OUTPUT.put_line (vNote);
         EXCEPTION
            WHEN OTHERS
            THEN
               QA_Note :=
                  QA_Note || ' Failed Step 100. Unable to Validate Topology';
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
                  'LS',
                  vTopo,
                  vProcess,
                  p_step        => vStepNum,
                  p_sqlstmt     => vStep,
                  p_error_msg   => vNote);

               retVal :=
                  'Validation Failed due to ' || SQLCODE || '-' || SQLERRM;
               vNote := retVal;
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
                  'LS',
                  vTopo,
                  vProcess,
                  p_step        => vStepNum,
                  p_sqlstmt     => vStep,
                  p_error_msg   => vNote);

               DBMS_OUTPUT.put_line (vNote);
               RAISE_APPLICATION_ERROR (-20001, vNote);
         END;

         vNote := 'Step ' || vStepNum || ' - Finished ' || vStep;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
                                                      
      ELSE
      
         vNote := 'Step ' || vStepNum || ' - SKIPPING ' || vStep;
         
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_step        => vStepNum,
                                                      p_sqlstmt     => vStep,
                                                      p_error_msg   => vNote);
                                                      
      END IF;

     --END LOOP;

     -- label in case the SRID is off from the start.
     <<EndLS>>
      IF (Success_Flag = 'Y')
      THEN
         vNote := 'Complete - Successful';
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_error_msg   => vNote);
      ELSE
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
            'LS',
            vTopo,
            vProcess,
            p_error_msg   => QA_Note);
         vNote := 'Complete - Failed Processing. Please check QA messages.';
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG ('LS',
                                                      vTopo,
                                                      vProcess,
                                                      p_error_msg   => vNote);
         RAISE_APPLICATION_ERROR (-20001, QA_Note);
      END IF;

      -- Clear the session tag
      DBMS_APPLICATION_INFO.SET_MODULE (NULL, NULL);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO (NULL);
   END LINE_SIM;

   FUNCTION GET_LINESIM_PROJECT_PARAMETERS (p_project_id   IN VARCHAR2,
                                            p_release      IN VARCHAR2)
      RETURN GZ_TYPES.LINE_SIM_PARAMETERS_REC
   AS
      -- Copied from Matt's clipper
      psql     VARCHAR2 (4000);
      output   GZ_TYPES.LINE_SIM_PARAMETERS_REC;
   BEGIN
      psql :=
            'SELECT a.* FROM LINE_SIM_PARAMETERS a '
         || 'WHERE '
         || 'a.gen_project_id = :p1 AND a.release = :p2';

      BEGIN
         EXECUTE IMMEDIATE psql
            INTO output
            USING UPPER (p_project_id), p_release;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            RAISE_APPLICATION_ERROR (
               -20001,
               'Line_Sim_Parameter record not found for project id '
               || p_project_id);
         WHEN TOO_MANY_ROWS
         THEN
            RAISE_APPLICATION_ERROR (
               -20001,
               'Found more than one Line_Sim_Parameter record for project id '
               || p_project_id);
         WHEN OTHERS
         THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR (
                  -20001,
                  'Table LINE_SIM_PARAMETERS does not exist!');
            ELSE
               RAISE;
            END IF;
      END;

      RETURN output;
   END GET_LINESIM_PROJECT_PARAMETERS;
END GZ_LINESIM;
/

CREATE OR REPLACE PACKAGE BODY GZ_QA_BASICS
AS
   -- Stephanie 8/01/2013

   -----------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------
   --See
   --GENERALIZATION_RUN_BASIC_QA;
   --For main entry point to this package from gz_workflow.
   -----------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------


   -----------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------

   PROCEDURE START_QA_LOGGING (p_jobid             IN VARCHAR2,
                               p_output_topology   IN VARCHAR2)
   AS
   --Stephanie 08/01/2013
   --Create logging table for this job

   BEGIN
      GZ_BUSINESS_UTILS.CREATE_GEN_XTEND_TRACKING_LOG (
         SYS_CONTEXT ('USERENV', 'CURRENT_SCHEMA'),
         p_output_topology || '_QA_TRACKING');

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
         'QA',
         p_output_topology,
         'START_QA_LOGGING',
         NULL,
         'STARTING QA:' || p_jobid || ': ' || p_output_topology);
   END START_QA_LOGGING;

   -----------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------

   FUNCTION VERTEX_COUNT_COMPARISON (
      p_module        IN VARCHAR2,
      p_topo          IN VARCHAR2,
      p_gen_tbl       IN VARCHAR2,
      p_ungen_tbl     IN VARCHAR2,
      p_geom_column   IN VARCHAR2 DEFAULT 'SDOGEOMETRY')
      RETURN VARCHAR2
   AS
      -- Stephanie 8/2/2013
      -- M@! Added tracking record for failed SQLs 9/4/13

      -- Simplistic vertex count compare tool.

      -- Purpose: function to return a string describing the difference
      -- between the total vertex count in two tables with an
      -- sdo_geometry column (table A vs B).

      -- Stephanie 8/5/2013

      -- Added p_module and p_topo to allow logging (see Matt's example in
      -- CHECK_STATE_AGREEMENT).
      -- Added SQL logging.

      -- Added check for internal edges if the vertex counts are equal.
      -- Prevents failure for DC if you just run state and county layers
      -- for example.

      v_sql              VARCHAR2 (4000);
      detailed_kount     PLS_INTEGER;
      gz_kount           PLS_INTEGER;
      returnval          VARCHAR2 (4000);
      v_judgement        VARCHAR2 (100);
      v_function         VARCHAR2 (4000) := 'VERTEX_COUNT_COMPARISON';
      v_internal_edges   NUMBER;
   BEGIN
      v_sql :=
            'select sum(sdo_util.getnumvertices('
         || p_geom_column
         || ')) from '
         || p_ungen_tbl;

      -- log SQL...

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
         p_module,
         p_topo,
         v_function,
         NULL,
         'Getting ungeneralized vertex count (see SQL)',
         p_sqlstmt   => v_sql);

      -- execute SQL

      BEGIN
         EXECUTE IMMEDIATE v_sql INTO detailed_kount;
      EXCEPTION
         WHEN OTHERS
         THEN
         
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (p_module,
                                                         p_topo,
                                                         v_function,
                                                         NULL,
                                                         'ERROR Getting ungeneralized vertex count (see sql and error msg)',
                                                         p_sqlstmt   => v_sql,
                                                         p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
                                                      
            RAISE_APPLICATION_ERROR (
               -20001,
                  SQLERRM || ' '
               || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE
               || ' on '
               || v_sql);
      END;

      v_sql :=
            'select sum(sdo_util.getnumvertices('
         || p_geom_column
         || ')) from '
         || p_gen_tbl;

      -- log SQL...

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
         p_module,
         p_topo,
         v_function,
         NULL,
         'Getting generalized vertex count (see SQL)',
         p_sqlstmt   => v_sql);

      -- execute SQL

      BEGIN
         EXECUTE IMMEDIATE v_sql INTO gz_kount;
      EXCEPTION
         WHEN OTHERS
         THEN
         
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (p_module,
                                                         p_topo,
                                                         v_function,
                                                         NULL,
                                                         'ERROR Getting ungeneralized vertex count (see sql and error msg)',
                                                         p_sqlstmt   => v_sql,
                                                         p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
            RAISE_APPLICATION_ERROR (
               -20001,
                  SQLERRM || ' '
               || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE
               || ' on '
               || v_sql);
      END;

      -- if either topology is empty fail

      IF (detailed_kount <= 0 OR gz_kount <= 0)
      THEN
         returnval :=
               'Something is suspicious!  '
            || 'The generalized or detailed vertex count = 0. '
            || CHR (10)
            || '   Detailed ('
            || p_ungen_tbl
            || '): '
            || detailed_kount
            || CHR (10)
            || '   Generalized ('
            || p_gen_tbl
            || '): '
            || gz_kount;

         RETURN returnval;
      END IF;

      -- if the gen and detailed are equal, check for internal edges
      -- if there are none, don't fail, just warn user in log.
      -- if internal edges are found, fail.

      IF (detailed_kount = gz_kount)
      THEN
         -- check for internal edges

         v_sql :=
               'select count(*) '
            || ' from '
            || p_topo
            || '_edge$ '
            || 'where left_face_id <> -1 and right_face_id <> -1 ';

         BEGIN
            EXECUTE IMMEDIATE v_sql INTO v_internal_edges;
         EXCEPTION
            WHEN OTHERS
            THEN
            
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (p_module,
                                                            p_topo,
                                                            v_function,
                                                            NULL,
                                                            'ERROR Getting ungeneralized vertex count (see sql and error msg)',
                                                            p_sqlstmt   => v_sql,
                                                            p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
                                                            
               RAISE_APPLICATION_ERROR (
                  -20001,
                     SQLERRM || ' ' 
                  || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE
                  || ' on '
                  || v_sql);
         END;

         IF v_internal_edges = 0
         THEN
            -- if no internal edges are found, it is probably OK.

            returnval :=
               'Warning: Not generalized!  '
               || 'The generalized vertex count equals the detailed vertex count. '
               || CHR (10)
               || 'This is probably okay because it looks like there are no internal edges to generalize.'
               || CHR (10)
               || 'This may be correct especially for small states like DC where there is only one entity from the input layers in the state.'
               || CHR (10)
               || '   Detailed ('
               || p_ungen_tbl
               || '): '
               || detailed_kount
               || CHR (10)
               || '   Generalized ('
               || p_gen_tbl
               || '): '
               || gz_kount;

            RETURN returnval;
         ELSE
            -- if internal edges are found, this probably is not right.
            -- Fail the job.

            returnval :=
               'Warning: Not generalized!  This looks suspicious!  '
               || 'The generalized vertex count equals the detailed vertex count. '
               || CHR (10)
               || 'This is probably NOT okay because it looks like there are internal edges to generalize. '
               || '   Detailed ('
               || p_ungen_tbl
               || '): '
               || detailed_kount
               || CHR (10)
               || '   Generalized ('
               || p_gen_tbl
               || '): '
               || gz_kount;

            RETURN returnval;
         END IF;
      END IF;

      IF (gz_kount > (detailed_kount / 2))
      THEN
         v_judgement := 'suspicious';
      ELSE
         v_judgement := 'generalized';
      END IF;

      returnval :=
            'generalized to detailed ratio: '
         || ROUND (gz_kount / detailed_kount, 2)
         || CHR (10)
         || '   Looks '
         || v_judgement
         || ' to me.'
         || CHR (10)
         || '   Detailed ('
         || p_ungen_tbl
         || '): '
         || detailed_kount
         || CHR (10)
         || '   Generalized ('
         || p_gen_tbl
         || '): '
         || gz_kount;

      RETURN returnval;
   END VERTEX_COUNT_COMPARISON;

   -----------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------

   FUNCTION CHECK_STATE_AGREEMENT (p_module           IN VARCHAR2,
                                   p_topo             IN VARCHAR2,
                                   p_gen_face_tab     IN VARCHAR2,
                                   p_release          IN VARCHAR2,
                                   p_gen_project_id   IN VARCHAR2)
      RETURN VARCHAR2
   AS
      -- Stephanie 8/2/2013

      --Matt 8/2/13 replaced some lines with suggestions
      --            added module and topo to inputs to allow logging

      -- given a release, gen_project_id, and topology name
      -- looks in the source schema and table for all input layers
      -- which have a 'STATEFP' in the source table
      -- when they do, compares the value for STATE on the face feature table to
      -- the value in the benchmark table.

      -- Check only runs if STATE has been copied down the the face feature
      -- table.
      -- Checks gz_layers_out for an ADD_TO_FACE column value
      -- xx= 'STATE'xx --> Not null

      -- returns a varchar that it skipped the check, found mismatches,
      -- or found no mismathces.

      v_sql                 VARCHAR2 (4000);
      v_source_schema       VARCHAR2 (4000);
      v_layer_list          GZ_TYPES.GZ_LAYERS_IN;                      --Matt
      v_rec_count           NUMBER;
      v_mismatch_count      NUMBER;
      v_total_mismatched    NUMBER := 0;
      v_mismatched_layers   VARCHAR2 (4000);
      v_add_to_face         VARCHAR2 (32);                              --Matt
   BEGIN
      -- make sure STATE (or something statelike) has been copied to
      -- face feature table

      v_sql :=
            'SELECT add_to_face '
         || '  FROM gz_layers_out '
         || 'WHERE release = :p1 '
         || 'AND gen_project_id = :p2 '                  --better use this too
         || 'AND add_to_face IS NOT NULL ';

      --- Matt's comment...
      --instead of STATE, why not Not Null?
      --could be anything.  statefp is possible for sure
      --lets get the value too, for the checker SQL below
      --I dont think we'd ever add anything other than a state-like value,
      --right?
      --No reason to add NATION to the face table for runs without clipping

      --- Stephanie's comment...
      -- But if it isn't 'STATE' and we comapre it against statefp in
      -- the loop below, it will not match.  If we hard code it there,
      -- seems like we should hard code it here, too.  Maybe we could make
      -- it more robust by allowing multiple values, but I think we would
      -- need to limit it somehow because comparing countyfp to a county
      -- value copied down to face would not work without also using
      -- statefp in the query.


      EXECUTE IMMEDIATE v_sql
         INTO v_add_to_face
         USING p_release, p_gen_project_id;

      IF v_add_to_face IS NULL
      THEN
         RETURN 'Skipped check.  State value was not copied to the face table.';
      END IF;

      -- select the source schema name from gz_job_parameters

      v_sql :=
            'select source_schema '
         || '  from gz_job_parameters '
         || ' where release = :p1 and gen_project_id = :p2';

      EXECUTE IMMEDIATE v_sql
         INTO v_source_schema
         USING p_release, p_gen_project_id;

      -- get input layers, source tables, and keys

      v_layer_list := GZ_BUILD_SOURCE.GET_LAYERS (p_release, p_gen_project_id);

      -- foreach input layer, check the value of STATE on the face table vs
      -- the value of STATEFP on the benchmark table

      v_rec_count := 0;

      FOR i IN 1 .. v_layer_list.COUNT
      LOOP
         --source table and key built into GZ_TYPES.GZ_LAYERS_IN

         --Probably a little faster to just throw the SQL and catch
         --errors when columns are NA
         --all_tab_columns can be contentious

         v_mismatch_count := 0;

         -- if v_add_to_face int he SQL below
         -- isn't STATE, there could be trouble.

         v_sql :=
               'select count(*) '
            || ' from '
            || p_gen_face_tab
            || ' a, '
            || v_source_schema
            || '.'
            || v_layer_list (i).source_table
            || ' b '
            || ' where a.'
            || v_layer_list (i).layer
            || ' = b.'
            || v_layer_list (i).source_key
            || ' AND a.'
            || v_add_to_face
            || ' <> b.statefp';

         IF i = 1
         THEN
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               p_module,
               p_topo,
               'CHECK_STATE_AGREEMENT',
               NULL,
                  'Checking '
               || v_layer_list.COUNT
               || ' input layers with SQL like -->',
               p_sqlstmt   => v_sql);
         END IF;

         BEGIN
            EXECUTE IMMEDIATE v_sql INTO v_mismatch_count;
         EXCEPTION
            WHEN OTHERS
            THEN
               IF SQLCODE = -904
               THEN
                  --ORA-00904: "B"."STATEFP": invalid identifier
                  --hopefully aiannh or something
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
                     p_module,
                     p_topo,
                     'CHECK_STATE_AGREEMENT',
                     NULL,
                        'Didnt check '
                     || v_layer_list (i).layer
                     || '. No STATEFP column',
                     p_sqlstmt   => v_sql);
               ELSE
                  RAISE_APPLICATION_ERROR (
                     -20001,
                        SQLERRM
                     || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE
                     || ' on '
                     || v_sql);
               END IF;
         END;

         IF v_mismatch_count > 0
         THEN
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
               p_module,
               p_topo,
               'CHECK_STATE_AGREEMENT',
               NULL,
                  'Found '
               || v_mismatch_count
               || ' mismatched records with SQL -->',
               p_sqlstmt   => v_sql);

            v_total_mismatched := v_total_mismatched + 1;

            IF v_total_mismatched = 1
            THEN
               v_mismatched_layers := v_layer_list (i).layer;
            ELSE
               v_mismatched_layers :=
                  v_mismatched_layers || ', ' || v_layer_list (i).layer;
            END IF;
         END IF;
      END LOOP;

      -- report PASS or FAIL

      IF v_total_mismatched > 0
      THEN
         RETURN    'FAILED.  Found '
                || v_total_mismatched
                || ' mismatched layer(s): '
                || v_mismatched_layers;
      ELSE
         RETURN 'PASSED.  No mismatches found between source statefp values and face feature table state values.';
      END IF;
   END CHECK_STATE_AGREEMENT;

   -----------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------

   -- MAIN ENTRY POINT for GZ software

   FUNCTION GENERALIZATION_RUN_BASIC_QA (p_jobid             IN VARCHAR2,
                                         p_output_topology   IN VARCHAR2,
                                         p_ungen_topology    IN VARCHAR2,
                                         p_gen_tbl           IN VARCHAR2,
                                         p_ungen_tbl         IN VARCHAR2)
      RETURN VARCHAR2
   AS
      v_module                  VARCHAR2 (20);
      v_process                 VARCHAR2 (100);
      v_step                    VARCHAR2 (4000);
      v_sql                     VARCHAR2 (4000);
      v_result                  VARCHAR2 (4000);
      v_vertex_count_result     VARCHAR2 (4000);
      v_state_mismatch_result   VARCHAR2 (4000);
      v_retval                  VARCHAR2 (1) := '0';
      v_release                 VARCHAR2 (4000);
      v_gen_project_id          VARCHAR2 (4000);
      v_jobid                   VARCHAR2 (4000);
      v_input_string            VARCHAR2 (4000);
      v_final_result            VARCHAR2 (4000) := 'QA Results: ';
      v_status                  VARCHAR2 (4000) := 'Passed';
   BEGIN
      -- Stephanie 8/02/2013

      --------------------------------------------------------------------------
      -- Steps
      -- 10. Start logging
      -- 20. Run basic vertex check
      -- 30. Run state mismatch check
      -- 40. Return result to workflow ('0' = pass, '1' = Fail)
      --------------------------------------------------------------------------

      -- set up variable constants

      v_module := 'QA';
      v_process := 'RUN_BASIC_QA';
      v_jobid := UPPER (p_jobid);

      -- Step 10 - start logging

      --------------------------------------------------------------------------
      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO (
         'GENERALIZATION_QA_BASICS: Begin QA Logging');
      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      --------------------------------------------------------------------------

      GZ_QA_BASICS.START_QA_LOGGING (p_jobid, p_output_topology);

      --
      -- log start time and inputs
      --

      v_step := 'Starting Basic QA check for ' || p_output_topology || '.';
      v_sql :=
            'Inputs are GENERALIZATION_RUN_BASIC_QA('
         || p_jobid
         || ','
         || p_output_topology
         || ','
         || p_ungen_topology
         || ','
         || p_gen_tbl
         || ','
         || p_ungen_tbl
         || ')';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (v_module,
                                                   p_output_topology,
                                                   v_process,
                                                   NULL,
                                                   v_step,
                                                   p_sqlstmt   => v_sql);

      -- Step 20 - run vertex check

      --------------------------------------------------------------------------
      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO (
         'GENERALIZATION_QA_BASICS: Vertex Check');
      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      --------------------------------------------------------------------------

      --
      -- run vertex check
      --

      v_step := 'Vertex count check for ' || p_output_topology || '.';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (v_module,
                                                   p_output_topology,
                                                   v_process,
                                                   NULL,
                                                   v_step || ' Begin.');

      v_result :=
         GZ_QA_BASICS.VERTEX_COUNT_COMPARISON (v_module,
                                               p_output_topology,
                                               p_gen_tbl,
                                               p_ungen_tbl,
                                               'SDOGEOMETRY');

      -- log result string from vertex count check

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (v_module,
                                                   p_output_topology,
                                                   v_process,
                                                   NULL,
                                                   v_step || ' Complete.',
                                                   p_error_msg   => v_result);

      IF v_result LIKE '%suspicious%'
      THEN
         v_final_result :=
            v_final_result || ' VERTEX_COUNT_COMPARISON: Failed. ';

         -- Log reason and fail the job

         v_vertex_count_result := 'Vertex count check failed.';

         v_retval := '1';
      ELSIF v_result LIKE '%Warning%'
      THEN
         v_final_result :=
            v_final_result
            || ' VERTEX_COUNT_COMPARISON: Passed with warning. ';

         -- don't fail the job

         v_vertex_count_result := 'Vertex count check passed with warning.';
      ELSE
         v_final_result :=
            v_final_result || ' VERTEX_COUNT_COMPARISON: Passed. ';

         -- don't fail the job

         v_vertex_count_result := 'Vertex count check passed.';
      END IF;

      -- Log vertex count result

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (v_module,
                                                   p_output_topology,
                                                   v_process,
                                                   NULL,
                                                   v_vertex_count_result);

      -- Step 30 - check for state mismatches.

      --------------------------------------------------------------------------
      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO (
         'GENERALIZATION_QA_BASICS: State mismatch check');
      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      --------------------------------------------------------------------------

      --
      -- run state mismatch check
      --

      v_step := 'State mismatch check for ' || p_output_topology || '.';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (v_module,
                                                   p_output_topology,
                                                   v_process,
                                                   NULL,
                                                   v_step);


      -- look up release and gen_project_id for the job...

      v_sql :=
            'select release, gen_project_id '
         || '  from gz_job_setup '
         || ' where jobid = :p1';

      EXECUTE IMMEDIATE v_sql INTO v_release, v_gen_project_id USING v_jobid;

      --Matt! added module and topo for tracking
      v_result :=
         CHECK_STATE_AGREEMENT (v_module,
                                p_output_topology,
                                p_gen_tbl,
                                v_release,
                                v_gen_project_id);

      -- log result string from state mismatch check

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (v_module,
                                                   p_output_topology,
                                                   v_process,
                                                   NULL,
                                                   v_step,
                                                   p_error_msg   => v_result);

      IF v_result LIKE '%FAIL%'
      THEN
         v_final_result :=
            v_final_result || ' CHECK_STATE_AGREEMENT: Failed. ';
         -- Log reason and fail the job

         v_state_mismatch_result := 'State mismatch check failed.';
         v_retval := '1';
      ELSE
         v_final_result :=
            v_final_result || ' CHECK_STATE_AGREEMENT: Passed. ';
         -- don't fail the job
         v_state_mismatch_result := 'State mismatch check passed.';
      END IF;

      -- Log vertex count result

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (v_module,
                                                   p_output_topology,
                                                   v_process,
                                                   NULL,
                                                   v_state_mismatch_result);


      -- Step 40 - retrun result

      --------------------------------------------------------------------------
      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO (
         'GENERALIZATION_QA_BASICS: Completed Returning result');

      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      --------------------------------------------------------------------------

      -- return 0 PASS or 1 FAIL to the caller

      -- log the overall result of GZ_QA_BASICS too.

      IF v_retval = 1
      THEN
         v_status := 'Failed';
      END IF;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG (
         v_module,
         p_output_topology,
         v_process,
         NULL,
         'QA Complete! ' || v_status || '.',
         p_sqlstmt   => v_final_result);

      RETURN v_retval;
   END GENERALIZATION_RUN_BASIC_QA;
END GZ_QA_BASICS;
/

CREATE OR REPLACE PACKAGE BODY GZ_TOPO_MERGE
AS
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   --SEE
   --GENERALIZATION_TOPO_MERGE
   --For entry point to this package
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   PROCEDURE START_MERGE_LOGGING (
      p_schema         IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_gz_jobid       IN VARCHAR2
   )
   AS

      --Matt! 6/3/11
      --Create logging table for this job

   BEGIN

      GZ_BUSINESS_UTILS.CREATE_GEN_XTEND_TRACKING_LOG(p_schema,
                                                 p_topo_out || '_MERGE_TRACKING');

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',
                                             p_topo_out,
                                             'START_MERGE_LOGGING',
                                             'STARTING JOB: ' || p_gz_jobid);


   END START_MERGE_LOGGING;

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   PROCEDURE SET_UP (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_face_out       IN VARCHAR2
   )
   AS

      --Matt! 6/10/11
      --Create all work tables and drop any topology (gotta do this before table creation)
      --Still shaking the process out, not sure if I like this approach

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('SET_UP: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --drop topology

      BEGIN
         GZ_TOPO_UTIL.purge_topology(p_schema, p_topo_out);
      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLERRM LIKE '%does not exist in%'
         THEN

            --ORA-20001: Topology JUNK does not exist in GZCPB1
            NULL;

         ELSE

            RAISE;

         END IF;

      END;

      --create and drop (if they exist) work tables

      --state outline table
      GZ_TOPO_MERGE.CREATE_GEN_CUTTER(p_schema, p_topo_out || '_CUTTER');

      --aggregation and gap testing work table
      GZ_TOPO_MERGE.CREATE_GEN_AGGR(p_schema, p_topo_out || '_AGGR');

      --face alignment processing table
      GZ_TOPO_MERGE.CREATE_GEN_ALFACE(p_schema, p_topo_out || '_ALFACE');

      --final face table
      GZ_TOPO_MERGE.CREATE_MERGE_FACE(p_schema,
                                      p_release,
                                      p_project_id,
                                      p_topo_out || '_' || p_face_out);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('SET_UP: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END SET_UP;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION GZ_TOPO_EXISTS (
      p_topo_name        IN VARCHAR2
   ) RETURN BOOLEAN
   AS

      --Matt! 8/8/11

      psql     VARCHAR2(4000);
      kount    PLS_INTEGER;

   BEGIN

      --kick out junk right away
      IF LENGTH(p_topo_name) = 0
      THEN
         RETURN FALSE;
      END IF;


      psql := 'SELECT count(*) '
           || 'FROM user_sdo_topo_info a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.table_name IS NOT NULL AND '
           || 'rownum = 1 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO kount USING UPPER(p_topo_name);

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

      IF kount = 0
      THEN

         RETURN FALSE;

      END IF;

      RETURN TRUE;

   END GZ_TOPO_EXISTS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION GET_GALACTIC_TOPOS_SRC (
      p_topo_in_table         IN VARCHAR2,
      p_column                IN VARCHAR2 DEFAULT 'TOPO_NAME' --or FACE_TABLE
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 5/25/11

      psql        VARCHAR2(4000);
      output      GZ_TYPES.stringarray;

   BEGIN

      IF p_column NOT IN ('TOPO_NAME','FACE_TABLE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Whats a ' || p_column || ' in ' || p_topo_in_table || '? ');

      END IF;


      psql := 'SELECT a.' || p_column || ' '
           || 'FROM '
           || p_topo_in_table || ' a '
           || 'ORDER BY a.topo_name ';

      BEGIN

         EXECUTE IMMEDIATE psql BULK COLLECT INTO output;

      EXCEPTION

         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Yo, topo_name record not found in table ' || p_topo_in_table );
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table ' || p_topo_in_table || ' does not exist!');
            ELSE
               RAISE;
            END IF;

      END;

      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Yo, couldnt find any ' || p_column || ' to work with in ' || p_topo_in_table);

      END IF;

      RETURN output;

   END GET_GALACTIC_TOPOS_SRC;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------------

   FUNCTION GET_GALACTIC_TOPOS (
      p_topo_out              IN VARCHAR2,
      p_column                IN VARCHAR2 DEFAULT 'TOPO_NAME', --or FACE_TABLE
      p_is_dead_flag          IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 5/25/11

      psql        VARCHAR2(4000);
      output      GZ_TYPES.stringarray;

   BEGIN

      IF p_column NOT IN ('TOPO_NAME','FACE_TABLE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Whats a ' || p_column || ' in ' || p_topo_out || '_CUTTER? ');

      END IF;


      psql := 'SELECT a.' || p_column || ' '
           || 'FROM '
           || p_topo_out || '_CUTTER a '
           || 'WHERE ';

      IF p_is_dead_flag IS NULL
      THEN

         --usual
         psql := psql
           || 'a.is_dead_flag != :p1 '
           || 'ORDER BY a.topo_name ';

      ELSE

         --in a few cases we want the dead topos
         psql := psql
           || 'a.is_dead_flag = :p1 '
           || 'ORDER BY a.topo_name ';

      END IF;


      BEGIN

         EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING 'Y';

      EXCEPTION

         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Yo, topo_name record not found in table ' || p_topo_out || '_CUTTER ' );
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table ' || p_topo_out || '_CUTTER does not exist!');
            ELSE
               RAISE;
            END IF;

      END;

      IF output.COUNT = 0
      AND p_is_dead_flag IS NULL --ok to return an empty array if we are asking for just dead topos
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Yo, couldnt find any ' || p_column || ' to work with in ' || p_topo_out || '_CUTTER');

      END IF;

      RETURN output;

   END GET_GALACTIC_TOPOS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   PROCEDURE DEADEN_GALACTIC_TOPO (
      p_topo_out              IN VARCHAR2,
      p_topo_name             IN VARCHAR2,
      p_resurrect_flag        IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 8/11/11

      psql        VARCHAR2(4000);

   BEGIN


      psql := 'UPDATE ' || p_topo_out || '_cutter a '
           || 'SET a.is_dead_flag = :p1 '
           || 'WHERE '
           || 'a.topo_name = :p2 ';

      BEGIN

         IF p_resurrect_flag IS NULL
         THEN

            EXECUTE IMMEDIATE psql USING 'Y',
                                          UPPER(p_topo_name);

         ELSE

            EXECUTE IMMEDIATE psql USING 'N',
                                          UPPER(p_topo_name);

         END IF;

      EXCEPTION

         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table ' || p_topo_out || '_CUTTER does not exist!');
            ELSE
               RAISE;
            END IF;

      END;

      COMMIT;

   END DEADEN_GALACTIC_TOPO;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION VERIFY_MERGE_INPUTS (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_gz_jobid           IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_top_layer_tab      IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt 8/8/11

      psql              VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      output            VARCHAR2(4000);
      ref_tables        GZ_TYPES.stringarray;
      galactic_topos    GZ_TYPES.stringarray;
      galactic_faces    GZ_TYPES.stringarray;
      face_fields       GZ_TYPES.stringarray;
      column_str        VARCHAR2(4000);
      kount             PLS_INTEGER;

   BEGIN

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'VERIFY_MERGE_INPUTS',NULL,'STARTING ' || p_gz_jobid);

      --get input parms
      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id_in);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_MERGE_INPUTS: Check tables exist');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --reference face fields
      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(merge_parms.gen_merge_face_fields)
      THEN

         output := output || 'Dude, ' || merge_parms.gen_merge_face_fields || ' doesnt exist. Check gen_merge_parameters.gen_merge_face_fields | ';

      END IF;

      --any other reference tables. Just reference_schemas at the moment
      ref_tables := GZ_TYPES.LEGAL_REFERENCE_TABLES();

      FOR i IN 1 .. ref_tables.COUNT
      LOOP

         IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(ref_tables(i))
         THEN

            output := output || 'Dude, reference table ' || ref_tables(i) || ' doesnt exist | ';

         END IF;

      END LOOP;

      --topo in table
      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_topo_in_table)
      THEN

         output := output || 'Dude, ' || p_topo_in_table || ' doesnt exist. Check input p_topo_in_table | ';

         --Call it a day if this is the case
         RETURN output;

      END IF;

      --go to preliminary SRC get galactic fn
      galactic_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS_SRC(p_topo_in_table, 'TOPO_NAME');
      galactic_faces := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS_SRC(p_topo_in_table, 'FACE_TABLE');

      --loop through the topos and faces
      --1 check for topology
      --2 Check for topology_face
      --3 Check for topology_p_top_layer_tab
      --4 Check that ALL sdogeometry faces are valid and polygons
      --5 Check for no gaps or overlaps

      FOR i IN 1 .. galactic_topos.COUNT
      LOOP

         IF NOT GZ_TOPO_MERGE.GZ_TOPO_EXISTS(galactic_topos(i))
         THEN

            output := output || 'Dude, topology ' || galactic_topos(i) || ' doesnt exist. Check ' || p_topo_in_table|| ' | ';

         END IF;

         IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(galactic_faces(i))
         THEN

            output := output || 'Dude, table ' || galactic_faces(i) || ' doesnt exist. Check ' || p_topo_in_table|| ' | ';

         END IF;

         IF p_top_layer_tab IS NOT NULL
         THEN

            --top layer (state) may not exist if no FSLs have been built
            --we will build it on the fly in a hot minute

            IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(galactic_topos(i) || '_' || p_top_layer_tab)
            THEN

               output := output || 'Dude, table ' || galactic_topos(i) || '_' ||  p_top_layer_tab || ' doesnt exist. Check ' || p_topo_in_table|| ' | ';

            END IF;

         END IF;


         --check for valid sdo at real tolerance
         psql := 'SELECT COUNT(*) '
              || 'FROM '
              || galactic_faces(i) ||  ' a '
              || 'WHERE '
              || 'SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.sdogeometry, :p1) != :p2 OR '
              || 'a.sdogeometry.sdo_gtype NOT IN (:p3,:p4) ';


         EXECUTE IMMEDIATE psql INTO kount USING merge_parms.gen_merge_tolerance,
                                                 'TRUE',
                                                 2003,
                                                 2007;  --really?

         IF kount != 0
         THEN

            output := output || 'Dude, table ' || galactic_faces(i) || ' contains ' || kount || ' invalid sdogeometries |';

         END IF;

         psql := 'SELECT COUNT(1) '
              || 'FROM '
              || 'user_ind_columns a, '
              || 'user_indexes b '
              || 'WHERE '
              || 'a.column_name = :p1 AND '
              || 'b.table_name = :p2 AND '
              || 'b.index_type = :p3 AND '
              || 'b.status = :p4 AND '
              || 'a.index_name = b.index_name ';

         EXECUTE IMMEDIATE psql INTO kount USING 'FACE_ID',
                                                  UPPER(galactic_faces(i)),
                                                  'NORMAL',
                                                  'VALID';

         IF kount = 0
         THEN

            --taking matters into my own hands here

            GZ_BUSINESS_UTILS.ADD_INDEX(galactic_faces(i),
                                   galactic_faces(i) || '_FA',
                                   'FACE_ID',
                                   'UNIQUE');

            --verify
            EXECUTE IMMEDIATE psql INTO kount USING 'FACE_ID',
                                                     UPPER(galactic_faces(i)),
                                                     'NORMAL',
                                                     'VALID';

            IF kount = 0
            THEN

               output := output || 'Dude, I tried but table ' || galactic_faces(i) || ' has no face_id index |';

            END IF;

         END IF;

      END LOOP;

      --if bad at this point, lets quit
      IF output IS NOT NULL
      THEN

         RETURN output;

      END IF;


      --this is probably overkill
      --but I want to check that all of the reference_face_fields 'attributes'
      --correspond to columns in the input face tables

      --return includes geoid
      face_fields := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                            p_project_id_in,
                                                            'ATTRIBUTE',
                                                            merge_parms.gen_merge_face_fields);

      FOR i IN 1 .. face_fields.COUNT
      LOOP

         --Build the list of columns once
         --and column_name IN ('AIANNHCE','ARTLI','CDFP','GEOID')

         IF i != face_fields.COUNT
         THEN

            column_str := column_str || '''' || face_fields(i) || ''',';

         ELSE

            column_str := column_str || '''' || face_fields(i) || '''';

         END IF;

      END LOOP;


      psql := 'SELECT count(*) FROM '
           || 'user_tab_columns a '
           || 'WHERE '
           || 'a.table_name = :p1 AND '
           || 'a.column_name IN (' || column_str || ')';

      FOR i IN 1 .. galactic_faces.COUNT
      LOOP

         EXECUTE IMMEDIATE psql INTO kount USING UPPER(galactic_faces(i));

         IF kount != face_fields.COUNT
         THEN

            output := output || 'Table ' || galactic_faces(i) || ' '
                             || 'is missing attributes requested in ' || merge_parms.gen_merge_face_fields || '|';

         END IF;

      END LOOP;

      --Check for gaps and overlaps

      FOR i IN 1 .. galactic_topos.COUNT
      LOOP


         kount := GZ_TOPO_MERGE.COUNT_TOPO_GAPS(galactic_topos(i),
                                                galactic_faces(i));

         IF kount != 0
         THEN

            output := output || 'Input topo ' || galactic_topos(i) || ' '
                             || 'has ' || kount || ' gaps |';

         END IF;

         kount := GZ_TOPO_MERGE.COUNT_TOPO_OVERLAPS(galactic_topos(i),
                                                    galactic_faces(i));

         IF kount != 0
         THEN

            output := output || 'Input topo ' || galactic_topos(i) || ' '
                             || 'has ' || kount || ' ' || galactic_faces(i) || ' face overlaps |';

         END IF;

      END LOOP;

      --What else should be checked?

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_MERGE_INPUTS: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'VERIFY_MERGE_INPUTS',NULL,'FINISHED ' || p_gz_jobid);

      IF output IS NULL
      THEN
         output := '0';
      END IF;

      RETURN output;

   END VERIFY_MERGE_INPUTS;

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------


   FUNCTION TIDY_EXIT (
      p_schema            IN VARCHAR2,
      p_release           IN VARCHAR2,
      p_project_id_in     IN VARCHAR2,
      p_gz_jobid          IN VARCHAR2,
      p_topo_in_table     IN VARCHAR2,
      p_top_layer_tab     IN VARCHAR2,
      p_topo_out          IN VARCHAR2,
      p_face_out          IN VARCHAR2,
      p_restart_flag      IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 8/18/11
      --What else to do here?


      retval             VARCHAR2(4000) := '0';
      dead_topos         GZ_TYPES.stringarray;
      live_topos         GZ_TYPES.stringarray;

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 5');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('TIDY EXIT: Starting ' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --tune the topo no matter what
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TIDY_EXIT',NULL,
                                              'Calling topo tune up ',
                                              NULL,NULL,NULL,NULL,NULL,NULL );

      GZ_TOPO_UTIL.GZ_TOPO_TUNE_UP(p_topo_out);

      dead_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out,
                                                     'TOPO_NAME',
                                                     'Y');

      IF dead_topos.COUNT = 0
      THEN

         --this is a success. A+

         --included in here is totally successful restarts

         --drop work tables
         --might we want to keep them for investigating max gaps or overlaps in inputs?
         --eh, not on a success, I guess

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TIDY_EXIT',NULL,
                                                'Dropping AGGR, CUTTER, ALFACE work tables ',
                                                 NULL,NULL,NULL,NULL,NULL,NULL );

         BEGIN

            GZ_BUSINESS_UTILS.GZ_DROP_TABLE(p_topo_out || '_AGGR');
            GZ_BUSINESS_UTILS.GZ_DROP_TABLE(p_topo_out || '_CUTTER');
            GZ_BUSINESS_UTILS.GZ_DROP_TABLE(p_topo_out || '_ALFACE');


         EXCEPTION
         WHEN OTHERS
         THEN
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TIDY_EXIT',NULL,
                                                'Error dropping work tables: ' || SQLERRM,
                                                 NULL,NULL,NULL,NULL,NULL,NULL );
         END;


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TIDY_EXIT',NULL,
                                                'WOOT: Successful completion for ' || p_gz_jobid || '. Return code 0 ',
                                                 NULL,NULL,NULL,NULL,NULL,NULL );

         RETURN '0';

      ELSIF dead_topos.COUNT > 0
      AND p_restart_flag = 'N'
      THEN

         --This is very unlikely these days, since I now try to kill the job after the module that fails
         --instead of attempting to bull in a tea shop ahead to completion

         --this is an I for incomplete.  Started at the beginning and left some topos behind

         FOR i IN 1 .. dead_topos.COUNT
         LOOP

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TIDY_EXIT',NULL,
                                                   'Input topology ' || dead_topos(i) || ' croaked along the way. Check the log ',
                                                    NULL,NULL,NULL,NULL,NULL,NULL );

         END LOOP;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TIDY_EXIT',NULL,
                                                'Semi-Woot: Complete, but not a total success for ' || p_gz_jobid || '. Return code 1 ',
                                                 NULL,NULL,NULL,NULL,NULL,NULL );

         RETURN '1';

      ELSIF dead_topos.COUNT > 0
      AND p_restart_flag = 'Y'
      THEN

         live_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out,
                                                        'TOPO_NAME');

         --This is unlikely in the current version of the code
         --We cant say too much about this, just that we restarted and didnt die
         --But somehow didnt end up with all living topos

         FOR i IN 1 .. live_topos.COUNT
         LOOP

            --this made more sense when the user was resetting lots of topos to N on restarts
            --and the code only processed the one or two live topos

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TIDY_EXIT',NULL,
                                                   'Input topology ' || live_topos(i) || ' is now complete ',
                                                    NULL,NULL,NULL,NULL,NULL,NULL );

         END LOOP;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TIDY_EXIT',NULL,
                                                'Questionable WOOT: Restart yielded successful completion for ' || live_topos.COUNT || ' source topos. Return code 1 to request eyeballs ',
                                                 NULL,NULL,NULL,NULL,NULL,NULL );


         RETURN '1';

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Peace Out ' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      RETURN retval;

   END TIDY_EXIT;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION STRINGARRAY2HASH (
      p_input       IN GZ_TYPES.stringarray
   )
   RETURN GZ_TYPES.stringhash DETERMINISTIC
   AS

     splithash     GZ_TYPES.stringhash;

   BEGIN

        FOR i IN 1 .. p_input.COUNT
        LOOP
           splithash(p_input(i)) := '1';
        END LOOP;

        RETURN splithash;

   END STRINGARRAY2HASH;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION CREATE_EMPTY_TOPOLOGY (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_keep_tabs      IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! 5/25/11

      psql           VARCHAR2(4000);
      merge_parms    GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      output         VARCHAR2(4000) := '0';
      start_time     TIMESTAMP;
      newtopo        VARCHAR2(32);
      kount          PLS_INTEGER;


   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_EMPTY_TOPOLOGY',NULL,'STARTING ' || p_jobid);

      --get input parms
      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id);

      newtopo := UPPER(p_topo_out);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EMPTY_TOPOLOGY: Drop topo ' || newtopo || ' if it exists');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Sreeni purger will error if topo doesnt exist

      psql := 'SELECT COUNT(*) '
           || 'FROM user_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'rownum = 1 ';

      EXECUTE IMMEDIATE psql INTO kount USING newtopo;

      IF kount = 1
      AND p_keep_tabs IS NULL
      THEN

         --first time thru, burn it all
         GZ_TOPO_UTIL.PURGE_TOPOLOGY(p_schema,newtopo);

      ELSIF kount = 1
      AND p_keep_tabs IS NOT NULL
      THEN

         --dropping states, burn the topo but keep the cutter table
         GZ_TOPO_UTIL.PURGE_TOPOLOGY(p_schema,newtopo,'Y');

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EMPTY_TOPOLOGY: Create topology ' || newtopo);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --Can't bring this lower than 6 digits or else loops and edges degenerate
      SDO_TOPO.create_topology(newtopo,
                               merge_parms.gen_merge_tolerance,
                               merge_parms.gen_merge_srid,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               merge_parms.gen_merge_snapping_digits);


      --insert universal face
      psql := 'INSERT INTO ' || newtopo || '_FACE$ '
           || 'VALUES (:p1, NULL, :p2, :p3, NULL)';

      EXECUTE IMMEDIATE psql USING -1,
                                   sdo_list_type(),
                                   sdo_list_type();

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',newtopo,'CREATE_EMPTY_TOPOLOGY',newtopo,
                                             'COMPLETED ',start_time);


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EMPTY_TOPOLOGY: Grant public select on ' || newtopo || '$s');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --relation$ doesnt exist at this point

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',newtopo || '_EDGE$');

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',newtopo || '_FACE$');

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',newtopo || '_NODE$');


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EMPTY_TOPOLOGY: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END CREATE_EMPTY_TOPOLOGY;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   PROCEDURE BUILD_PHONY_FSL (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_jobid              IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_face_tab           IN VARCHAR2,
      p_table_ext          IN VARCHAR2 DEFAULT 'PHONY'
   )
   AS

      --Matt! 12/19/11
      --we have no FSLs so we need to make a phony top level one
      --this table will get deregistered and dropped a few steps after calling this proc

      psql                 VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_PHONY_FSL: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --deregister in case of rerun
      BEGIN
         GZ_TOPO_UTIL.DEREGISTER_FEATURE_TABLES(p_schema,
                                                p_topo,
                                                'Y',                            --Yes drop the table
                                                1,                              --tg_layer_level 1
                                                p_topo || '_' || p_table_ext);  --like clause for just this table
      EXCEPTION
      WHEN OTHERS
      THEN
         NULL;
      END;


      --only cols in this tab are sdogeometry, topogeom
      GZ_TOPO_MERGE.CREATE_GEN_MERGE_PHONY_FSL(p_schema, p_topo || '_' || p_table_ext);

      --add to topo
      GZ_TOPO_UTIL.GZ_ADD_TOPO_GEOMETRY_LAYER(p_topo,
                                              p_topo || '_' || p_table_ext,
                                              'TOPOGEOM',
                                              'POLYGON',
                                              p_face_tab);


      --should probably write a utility for this form of create_feature
      --create topogeom pointers to all the faces in the face feature table
      psql := 'INSERT INTO '
            || p_topo || '_' || p_table_ext || ' (sdogeometry, topogeom) '
            || 'VALUES '
            || '(NULL, '
            || ' SDO_TOPO_MAP.CREATE_FEATURE(:p1,:p2,:p3,:p4)) ';

      EXECUTE IMMEDIATE psql USING p_topo,
                                   p_topo || '_' || p_table_ext,
                                   'TOPOGEOM',
                                   'face_id>0';  --all face features

      COMMIT;

      --cant use GZ_POPULATE_MEASUREMENTS no pkc
      psql := 'UPDATE '
            || p_topo || '_' || p_table_ext || ' a '
            || 'SET '
            || 'a.sdogeometry = a.topogeom.get_geometry() ';

      EXECUTE IMMEDIATE psql;
      COMMIT;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_PHONY_FSL: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END BUILD_PHONY_FSL;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION CREATE_COOKIE_CUTTERS (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_jobid_in           IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_top_layer_tab      IN VARCHAR2,
      p_topo_out           IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/25/11
      --2 parts, for now
      --build the cookie cutter table ie get all states into one table
      --assess gaps and overlaps
      --! 12/19/11 Added option to create phony top layer FSL when p_top_layer_tab is null

      psql              VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      output            VARCHAR2(4000) := '0';
      start_time        TIMESTAMP;
      galactic_topos    GZ_TYPES.stringarray;
      galactic_faces    GZ_TYPES.stringarray;
      kount             PLS_INTEGER;
      srid              NUMBER;
      cutter_tab        VARCHAR2(4000);
      aggr_tab          VARCHAR2(4000);
      max_gap           NUMBER;
      max_overlap       NUMBER;
      top_layer_ext     VARCHAR2(32);

   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,'STARTING ' || p_jobid_in);

      --get input parms
      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id_in);

      --get galactic topos from source topo_in table - after this always from misnamed cutter
      galactic_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS_SRC(p_topo_in_table, 'TOPO_NAME');

      galactic_faces := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS_SRC(p_topo_in_table, 'FACE_TABLE');

      cutter_tab := p_topo_out || '_CUTTER';
      aggr_tab   := p_topo_out || '_AGGR';

      IF p_top_layer_tab IS NULL
      THEN

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_COOKIE_CUTTERS: Build top FSL on the fly ');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         --optional NULL gen_merge_top_layer
         --no fsls have been built for these topologies
         --build a top layer equivalent on the fly

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,
                                                'Making ' || galactic_topos.COUNT || ' phony FSLs on the fly');

         top_layer_ext := 'PHONY';

         FOR i IN 1 .. galactic_topos.COUNT
         LOOP

            GZ_TOPO_MERGE.BUILD_PHONY_FSL(p_schema,
                                          p_project_id_in,
                                          p_jobid_in,
                                          galactic_topos(i),
                                          galactic_faces(i),
                                          top_layer_ext);

         END LOOP;

      ELSE

         top_layer_ext := p_top_layer_tab;

      END IF;



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_COOKIE_CUTTERS: Populate table ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,
                                             'Looping ' || galactic_topos.COUNT || ' inserts into ' || cutter_tab || ' based on ' || p_topo_in_table);


      FOR i in 1 .. galactic_topos.COUNT
      LOOP


         --CHECK for count of 1. Expected of a top level layer

         psql := 'SELECT count(*) '
              || 'FROM ' || galactic_topos(i) || '_' ||  top_layer_ext ;

         EXECUTE IMMEDIATE psql INTO kount;


         IF kount != 1
         THEN

             --LOG
             GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,
                                                    'Error: Record count of  ' || kount || ' in ' || galactic_topos(i) || '_' ||  top_layer_ext,
                                                    NULL,NULL,NULL,NULL,NULL,NULL,NULL);

             output := output || 'Problem with ' || galactic_topos(i) || '_' || '_' || top_layer_ext || '. It has ' || kount || ' records instead of 1 |';
             RETURN output;

         END IF;

         --INSERT record source topo and sdo into cutter table

         --SRID check. Note that it could potentially be different for each input
         psql := 'SELECT a.sdogeometry.sdo_srid '
              || 'FROM '
              || galactic_topos(i) || '_' || top_layer_ext || ' a ';

         EXECUTE IMMEDIATE psql INTO srid;


         psql := 'INSERT INTO '
                 || cutter_tab
                 || '(cutter_id, source_topo, topo_name, face_table, unaligned_sdo, sdogeometry, is_dead_flag) VALUES '
                 || '(:p1, :p2, :p3, :p4,';

         IF srid = merge_parms.gen_merge_srid
         THEN

            psql := psql
                || '(select a.sdogeometry FROM ' || galactic_topos(i) || '_' || top_layer_ext || ' a), '
                || '(select a.sdogeometry FROM ' || galactic_topos(i) || '_' || top_layer_ext || ' a), :p5 ) '; --sue me

            EXECUTE IMMEDIATE psql USING i,
                                         galactic_topos(i),
                                         galactic_topos(i),
                                         galactic_faces(i),
                                         'N';


         ELSE

            psql := psql
                 || '(select SDO_CS.TRANSFORM(a.sdogeometry,:p5) FROM ' || galactic_topos(i) || '_' || top_layer_ext || ' a), '
                 || '(select SDO_CS.TRANSFORM(a.sdogeometry,:p6) FROM ' || galactic_topos(i) || '_' || top_layer_ext || ' a), :p7 ) ';

            EXECUTE IMMEDIATE psql USING i,
                                         galactic_topos(i),
                                         galactic_topos(i),
                                         galactic_faces(i),
                                         merge_parms.gen_merge_srid,
                                         merge_parms.gen_merge_srid,
                                         'N';

         END IF;



      END LOOP;

      COMMIT;

      --check that geoms are valid
      psql := 'SELECT count(*) '
           || 'FROM '
           || cutter_tab || ' a '
           || 'WHERE '
           || 'sdo_geom.validate_geometry_with_context(a.unaligned_sdo, :p1) != :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING merge_parms.gen_merge_tolerance,
                                              'TRUE';

      IF kount != 0
      THEN

         --LOG
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,
                                                'Error: ' || kount || ' invalid geometries  in ' || cutter_tab,
                                                NULL,NULL,NULL,psql,NULL,NULL,NULL);

         output := output || 'Problem with ' || cutter_tab || ' It has ' || kount || ' invalid geoms |';

         RETURN output;

      END IF;


      --update SRID to NULL from here until Topo creation 3 modules from now

      psql := 'UPDATE ' || cutter_tab || ' a '
           || 'SET '
           || 'a.sdogeometry.sdo_srid = NULL, '
           || 'a.unaligned_sdo.sdo_srid = NULL ';

      EXECUTE IMMEDIATE psql;
      COMMIT;

      IF p_top_layer_tab IS NULL
      THEN

         --we have what we want from the phony FSLs we built
         --deregister and drop

         FOR i IN 1 .. galactic_topos.COUNT
         LOOP

            GZ_TOPO_UTIL.DEREGISTER_FEATURE_TABLES(p_schema,
                                                   galactic_topos(i),
                                                   'Y',                                  --Yes drop the table
                                                   1,                                    --tg_layer_level 1 and up
                                                   galactic_topos(i) || '_' || 'PHONY'); --like clause

         END LOOP;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_COOKIE_CUTTERS: Gap evaluation on ' || cutter_tab);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,
                                             'Gap evaluation on ' || cutter_tab);


      max_gap := GZ_TOPO_MERGE.MAX_GAP_EVALUATION_2(p_topo_out,
                                                    cutter_tab,
                                                    cutter_tab || '_GAP_EVALUATION',
                                                    aggr_tab,
                                                    'UNALIGNED_SDO', --unaligned_sdo col is equivalent at the moment actually
                                                    merge_parms.gen_merge_null_tolerance,
                                                    p_gz_union =>'N');  --override to use backup gz_union if sdo_union buggy


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,
                                             'Finished gap evaluation on ' || cutter_tab || ' Max gap = ' || max_gap);

      IF max_gap > merge_parms.gen_merge_error_chasm
      THEN

          --LOG
          GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,
                                                 'ERROR: Max gap in unaligned input is ' || max_gap,
                                                  NULL,NULL,NULL,NULL,NULL,NULL,NULL);

          output := output || 'Problem with input ' || top_layer_ext || 's. Max gap is ' || max_gap
                           || '. Greater than allowed ' || merge_parms.gen_merge_error_chasm || ' |';

          RETURN output;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_COOKIE_CUTTERS: Overlap evaluation on ' || cutter_tab);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      max_overlap := GZ_TOPO_MERGE.MAX_OVERLAP_EVALUATION(p_topo_out,
                                                          cutter_tab,
                                                          cutter_tab || '_GAP_EVALUATION',
                                                          aggr_tab,
                                                          'UNALIGNED_SDO', --unaligned_sdo col is equivalent now, no diff
                                                          merge_parms.gen_merge_null_tolerance,
                                                          p_gz_intersect=>'N');  --possible bug workaround

       GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,
                                             'Finished overlap evaluation on ' || cutter_tab || ' Max overlap = ' || max_overlap);

      IF max_overlap > merge_parms.gen_merge_error_chasm
      THEN

          --LOG
          GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,
                                                 'ERROR: Max overlap in unaligned input is ' || max_overlap,
                                                  NULL,NULL,NULL,NULL,NULL,NULL,NULL);

          output := output || 'Problem with input ' || top_layer_ext || 's. Max overlap is ' || max_overlap
                           || '. Greater than allowed ' || merge_parms.gen_merge_error_chasm || ' |';

          RETURN output;

      END IF;

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_COOKIE_CUTTERS: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      RETURN output;


   END CREATE_COOKIE_CUTTERS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION ALIGN_COOKIE_CUTTERS (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_jobid_in           IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_top_layer_tab      IN VARCHAR2,
      p_topo_out           IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 6/22/11


      psql              VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      output            VARCHAR2(4000) := '0';
      start_time        TIMESTAMP;
      kount             PLS_INTEGER;
      cutter_tab        VARCHAR2(4000);


   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,'STARTING ' || p_jobid_in);

      --get input parms
      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id_in);

      cutter_tab := p_topo_out || '_CUTTER';


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ALIGN_COOKIE_CUTTERS: Align ' || cutter_tab);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_COOKIE_CUTTERS',NULL,
                                             'Aligning ' || cutter_tab);


      --This will align almost all true gaps and overlaps, which are usually no more than
      --coordinate rounding differences

      GZ_GEOM_UTILS.GZ_ALIGN_EDGES(cutter_tab,
                                   'SDOGEOMETRY',
                                   cutter_tab || '_AL',
                                   merge_parms.gen_merge_null_tolerance,
                                   'Y',
                                   'N'); --switch if sdo_difference is f'd

      --occasionally still leaves a sliver or overlap or 2
      --easiest just to take a second crack at it

      --why not 3?  Dont ask

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_COOKIE_CUTTERS',NULL,
                                             'Aligning: Pass two ' || cutter_tab);

      GZ_GEOM_UTILS.GZ_ALIGN_EDGES(cutter_tab,
                                   'SDOGEOMETRY',
                                   cutter_tab || '_AL',
                                   merge_parms.gen_merge_null_tolerance,
                                   'Y',
                                   'N'); --switch if sdo_difference is f'd



      psql := 'SELECT count(*) FROM '
           || cutter_tab || ' a '
           || 'WHERE '
           || 'a.sdogeometry.sdo_gtype NOT IN (:p1,:p2)';

      EXECUTE IMMEDIATE psql INTO kount USING 2003, 2007;

      IF kount != 0
      THEN

         --cant build a polygon topology from garbage gtypes
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_COOKIE_CUTTERS',NULL,
                                                 'ERROR: Aligned ' || cutter_tab || ' has ' || kount || ' non-polygon geometries',
                                                  NULL,NULL,NULL,psql,NULL,NULL,NULL);

         output := output || ' Problem with aligned ' || cutter_tab || '. It has ' || kount || ' non-polygon geometries|';

         RETURN output;

      END IF;


      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ALIGN_COOKIE_CUTTERS: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      RETURN output;

   END ALIGN_COOKIE_CUTTERS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION BUILD_CUTTER_TOPO (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_jobid_in           IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_top_layer_tab      IN VARCHAR2,
      p_topo_out           IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 6/22/11
      --4/27/12 Switch to add_topo_from_spatial


      psql              VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      output            VARCHAR2(4000) := '0';
      start_time        TIMESTAMP;
      kount             PLS_INTEGER;
      cutter_tab        VARCHAR2(4000);


   BEGIN


      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_CUTTER_TOPO',NULL,'STARTING ' || p_jobid_in);

      --get input parms
      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id_in);

      cutter_tab := p_topo_out || '_CUTTER';


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_CUTTER_TOPO: Build topology ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_CUTTER_TOPO',NULL,
                                             'Build topo for ' ||  cutter_tab);

      --UPDATE sdo back to real SRID dummy!
      --NULLed it out starting at the max gap eval

      psql := 'UPDATE ' || cutter_tab || ' a '
           || 'SET '
           || 'a.sdogeometry.sdo_srid = :p1 ';

      EXECUTE IMMEDIATE psql USING merge_parms.gen_merge_srid;
      COMMIT;

      --Check that the only problem with the cutter table is duplicate vertices
      --we are going off the grid in terms of validation inside build topo from spatial
      psql := 'SELECT count(*) FROM '
           || cutter_tab || ' a '
           || 'WHERE '
           || 'sdo_geom.validate_geometry_with_context(a.sdogeometry, :p1) != :p2 AND '
           || 'sdo_geom.validate_geometry_with_context(a.sdogeometry, :p3) NOT LIKE :p4 ';

      EXECUTE IMMEDIATE psql INTO kount USING merge_parms.gen_merge_tolerance,
                                              'TRUE',
                                              merge_parms.gen_merge_tolerance,
                                              '13356%';

      IF kount != 0
      THEN

         --too risky
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_CUTTER_TOPO',NULL,
                                                 'ERROR: Aligned ' || cutter_tab || ' has ' || kount || ' invalid (non dupe) geometries',
                                                  NULL,NULL,NULL,psql,NULL,NULL,NULL);

         output := output || ' Problem with aligned ' || cutter_tab || '. It has ' || kount || '  invalid (non dupe) geometries|';

         RETURN output;

      END IF;



      GZ_BUSINESS_UTILS.JAVA_MEMORY_MANAGER(cutter_tab,
                                       'SDOGEOMETRY');

      --generic add_polygon_geometry + constructors call

      GZ_TOPO_UTIL.ADD_TOPO_FROM_SPATIAL(p_topo_out,
                                         cutter_tab,
                                         'CUTTER_ID',
                                         'POLYGON',
                                         'MERGE',      --generic logger
                                         'SDOGEOMETRY',
                                         NULL,         --no subset
                                         NULL,
                                         'Y',          --allow splits, thats what we want to learn about
                                         'Y');         --yes new layer

      --have a relation$ now
      --but we will drop and recreate the topology in a few steps
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS', p_topo_out || '_RELATION$');

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_CUTTER_TOPO: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END BUILD_CUTTER_TOPO;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION ASSESS_CUTTER_TOPO (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_jobid_in           IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_top_layer_tab      IN VARCHAR2,
      p_topo_out           IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 6/22/11


      psql              VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      output            VARCHAR2(4000) := '0';
      start_time        TIMESTAMP;
      kount             PLS_INTEGER;
      cutter_tab        VARCHAR2(4000);


   BEGIN


      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_CUTTER_TOPO',NULL,'STARTING ' || p_jobid_in);

      --get input parms
      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id_in);

      cutter_tab := p_topo_out || '_CUTTER';


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ASSESS_CUTTER_TOPO: Check it out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --an overlap is a face shared by two cutter records
      --subtract distinct faces in the group from total faces

      psql := 'SELECT '
           || '( COUNT(f.face_id) - COUNT(distinct f.face_id) ) FROM '
           || cutter_tab || ' a, '
           || p_topo_out || '_relation$ r, '
           || p_topo_out || '_face$ f '
           || 'WHERE '
           || 'a.topogeom.tg_id = r.tg_id AND '
           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'r.topo_id = f.face_id ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_CUTTER_TOPO',NULL,
                                             'Check topo in ' ||  cutter_tab || ' for overlaps ',
                                             NULL,NULL,NULL,psql,NULL,NULL,NULL);

      EXECUTE IMMEDIATE psql INTO kount;

      IF kount != 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_CUTTER_TOPO',NULL,
                                                'ERROR: Aligned ' || cutter_tab || ' topogeom has ' || kount || ' overlapping faces ',
                                                NULL,NULL,NULL,psql,NULL,NULL,NULL);

         output := output || ' Problem with aligned ' || cutter_tab || ' topogeom. It has ' || kount || '  overlapping faces|';

         RETURN output;

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_CUTTER_TOPO',NULL,
                                                'No overlaps in ' || cutter_tab || ' topogeom ',
                                                NULL,NULL,NULL,NULL,NULL,NULL,NULL);

      END IF;


      --a gap is a face id that isnt associated with one of the cutter features

      psql := 'SELECT COUNT(ff.face_id) FROM '
           || p_topo_out || '_face$ ff '
           || 'WHERE ff.face_id != :p1 AND '
           || 'ff.face_id NOT IN '
           || '('
           || 'SELECT DISTINCT f.face_id FROM '
           || cutter_tab || ' a, '
           || p_topo_out || '_relation$ r, '
           || p_topo_out || '_face$ f '
           || 'WHERE '
           || 'a.topogeom.tg_id = r.tg_id AND '
           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'r.topo_id = f.face_id '
           || ') ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_CUTTER_TOPO',NULL,
                                             'Check topo in ' ||  cutter_tab || ' for gaps  ',
                                             NULL,NULL,NULL,psql,NULL,NULL,NULL);

      EXECUTE IMMEDIATE psql INTO kount USING -1;


      IF kount != 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_CUTTER_TOPO',NULL,
                                                'ERROR: Aligned ' || cutter_tab || ' topogeom has ' || kount || ' gap faces ',
                                                NULL,NULL,NULL,psql,NULL,NULL,NULL);

         output := output || ' Problem with aligned ' || cutter_tab || ' topogeom. It has ' || kount || '  gap faces|';

         RETURN output;

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_CUTTER_TOPO',NULL,
                                                'No gaps in ' || cutter_tab || ' topogeom ',
                                                NULL,NULL,NULL,NULL,NULL,NULL,NULL);


      END IF;


      --Thats all to check for now
      --Maybe more later


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ASSESS_CUTTER_TOPO: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END ASSESS_CUTTER_TOPO;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public --------------------------------------------------------------------------------

   FUNCTION PIPE_UNALIGNED_RECS (
      p_project_id         IN VARCHAR2,
      p_job_id             IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_topo               IN VARCHAR2,  --source topo
      p_face_tab           IN VARCHAR2,
      p_cutter_tab         IN VARCHAR2,
      p_srid               IN NUMBER
   )
   RETURN GZ_TYPES.GEN_ALFACE PIPELINED
   AS

      --Matt! 6/27/11

      output         GZ_TYPES.GEN_ALFACE_REC;
      my_cursor      SYS_REFCURSOR;
      source_topos   GZ_TYPES.stringarray;
      source_geoms   GZ_TYPES.geomarray;
      psql           VARCHAR2(4000);
      face_ids       GZ_TYPES.stringarray;
      face_ids_2     GZ_TYPES.stringarray;
      face_geoms     GZ_TYPES.geomarray;
      srid           NUMBER;
      neighbors      PLS_INTEGER := 0;  --assume no neighbors



   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('PIPE_UNALIGNED_RECS: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --No neighbor state topos 8/24/11
      --Not nailed down 100 percent tight yet

      --get neighbor topos. The sdogeometry column should be perfectly aligned
      --but be prepared to add OVERLAPBDYINTERSECT to the mask
      --srid in whereclause is already in working srid, no transform
      --unaligned_sdo is likely still null

      /*

      psql := 'SELECT a.source_topo, a.unaligned_sdo '
           || 'FROM '
           || p_cutter_tab || ' a '
           || 'WHERE '
           || 'SDO_RELATE(a.sdogeometry, (SELECT sdogeometry FROM ' || p_cutter_tab || ' WHERE source_topo = :p1), :p2) = :p3 '
           || 'ORDER BY a.source_topo ';

      OPEN my_cursor FOR psql USING p_topo,
                                    'MASK=TOUCH',
                                    'TRUE';

     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'PIPE_UNALIGNED_RECS',NULL,
                                            'Getting neighbor topos ',
                                            NULL,NULL,NULL,psql,NULL,NULL,NULL);

      LOOP

         FETCH my_cursor BULK COLLECT INTO source_topos, source_geoms LIMIT 25;
         EXIT WHEN source_topos.COUNT = 0;

         FOR i IN 1 .. source_topos.COUNT
         LOOP

            output.face_id       := source_topos(i);  --phony face id of source topo
            output.source_topo   := source_topos(i);

            source_geoms(i).sdo_srid := NULL;

            output.unaligned_sdo := source_geoms(i);  --put the unaligned sdo
            output.sdogeometry   := source_geoms(i);  --in both columns

            --got here at least 1 time
            neighbors := 1;

            PIPE ROW(output);

         END LOOP;

      END LOOP;

      CLOSE my_cursor;

      */

      neighbors := 1;

      IF neighbors = 1
      THEN

         --only do this next step if we have neighbor topos to align to
         --skip Alaska and HI and PR

         --now get faces that touch the state outline of the working state
         --this should include faces that touch at a point
         --start by collecting just the face ids

         --reference
         /*SELECT DISTINCT face_id
           FROM (SELECT a.face_id face_id
                   FROM z635ls_clip_face a,
                        z635ls_relation$ r,
                        z635ls_face$ f,
                        z635ls_edge$ e
                  WHERE     a.topogeom.tg_id = r.tg_id
                        AND a.topogeom.tg_layer_id = r.tg_layer_id
                        AND r.topo_id = f.face_id
                        AND (e.left_face_id = f.face_id OR e.right_face_id = f.face_id)
                        AND e.start_node_id IN
                               (SELECT n.node_id
                                  FROM z635ls_node$ n
                                 WHERE n.node_id IN (SELECT e.start_node_id
                                                       FROM z635ls_edge$ e
                                                      WHERE e.right_face_id = -1
                                                     UNION
                                                     SELECT e.start_node_id
                                                       FROM z635ls_edge$ e
                                                      WHERE e.left_face_id = -1))
                 UNION
                 SELECT a.face_id face_id
                   FROM z635ls_clip_face a,
                        z635ls_relation$ r,
                        z635ls_face$ f,
                        z635ls_edge$ e
                  WHERE     a.topogeom.tg_id = r.tg_id
                        AND a.topogeom.tg_layer_id = r.tg_layer_id
                        AND r.topo_id = f.face_id
                        AND (e.left_face_id = f.face_id OR e.right_face_id = f.face_id)
                        AND e.end_node_id IN
                               (SELECT n.node_id
                                  FROM z635ls_node$ n
                                 WHERE n.node_id IN (SELECT e.end_node_id
                                                       FROM z635ls_edge$ e
                                                      WHERE e.right_face_id = -1
                                                     UNION
                                                     SELECT e.end_node_id
                                                       FROM z635ls_edge$ e
                                                      WHERE e.left_face_id = -1)))
         */

         psql := 'SELECT DISTINCT face_id FROM ('
              || 'SELECT a.face_id face_id FROM '  --should be one to one from feature face
              || p_face_tab || ' a, '               --but we will go through topo to be safe
              || p_topo || '_relation$ r, '
              || p_topo || '_face$ f, '
              || p_topo || '_edge$ e '
              || 'WHERE a.topogeom.tg_id = r.tg_id AND '
              || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
              || 'r.topo_id = f.face_id AND '
              || '(e.left_face_id = f.face_id or e.right_face_id = f.face_id) AND ' --limit to faces with these edges
              || 'e.start_node_id IN ( '                                            --which edges are in the following
              ||    'SELECT n.node_id FROM '                                        --start node list
              ||    p_topo || '_node$ n '                                           --gets us nodes/edges/faces that touch state outline at a point also
              ||    'WHERE n.node_id IN ('
              ||        'SELECT e.start_node_id FROM ' || p_topo || '_edge$ e '     --Nodes on state outline
              ||        'WHERE e.right_face_id = -1 '
              ||        'UNION '
              ||        'SELECT e.start_node_id FROM ' || p_topo || '_edge$ e '
              ||        'WHERE e.left_face_id = -1 '
              ||      ') '
              ||   ') '
              || 'UNION '
              || 'SELECT a.face_id face_id FROM '                                   --REPEAT
              || p_face_tab || ' a, '
              || p_topo || '_relation$ r, '
              || p_topo || '_face$ f, '
              || p_topo || '_edge$ e '
              || 'WHERE a.topogeom.tg_id = r.tg_id AND '
              || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
              || 'r.topo_id = f.face_id AND '
              || '(e.left_face_id = f.face_id or e.right_face_id = f.face_id) AND '
              || 'e.end_node_id IN ( '                                              --end node list
              ||    'SELECT n.node_id FROM '
              ||    p_topo || '_node$ n '
              ||    'WHERE n.node_id IN ('
              ||        'SELECT e.end_node_id FROM ' || p_topo || '_edge$ e '
              ||        'WHERE e.right_face_id = -1 '
              ||        'UNION '
              ||        'SELECT e.end_node_id FROM ' || p_topo || '_edge$ e '
              ||        'WHERE e.left_face_id = -1 '
              ||      ') '
              ||   ') '
              || ') ';

          GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'PIPE_UNALIGNED_RECS',NULL,
                                                  'Getting faces on ' || p_topo || ' outline ',
                                                  NULL,NULL,NULL,psql,NULL,NULL,NULL);

          EXECUTE IMMEDIATE psql BULK COLLECT INTO face_ids;


          --SRID checker
          psql := 'SELECT a.sdogeometry.sdo_srid '
               || 'FROM '
               || p_face_tab || ' a '
               || 'WHERE rownum = 1 ';

          EXECUTE IMMEDIATE psql INTO srid;


         --cursor for faces
         psql := 'select a.face_id, a.sdogeometry '
              || 'FROM '
              || p_face_tab || ' a '
              || 'WHERE a.face_id IN (SELECT * FROM TABLE(:p1)) ';


         OPEN my_cursor FOR psql USING GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(face_ids);

         LOOP

            FETCH my_cursor BULK COLLECT INTO face_ids_2, face_geoms LIMIT 25;
            EXIT WHEN face_ids_2.COUNT = 0;

            FOR i IN 1 .. face_ids_2.COUNT
            LOOP


               output.face_id       := face_ids_2(i);
               output.source_topo   := p_topo;

               IF srid != p_srid
               THEN

                  --transform then NULL out
                  face_geoms(i) := SDO_CS.TRANSFORM(face_geoms(i),p_srid);
                  face_geoms(i).sdo_srid := NULL;

               ELSE

                  --just NULL srid
                  face_geoms(i).sdo_srid := NULL;

               END IF;


               output.unaligned_sdo := face_geoms(i);
               output.sdogeometry   := face_geoms(i);

               PIPE ROW(output);

            END LOOP;

         END LOOP;

         CLOSE my_cursor;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('PIPE_UNALIGNED_RECS: Peace Out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


   END PIPE_UNALIGNED_RECS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------


  FUNCTION ALIGN_FACES (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_jobid_in           IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_top_layer_tab      IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_face_out           IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 6/27/11


      psql              VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      galactic_topos    GZ_TYPES.stringarray;
      galactic_faces    GZ_TYPES.stringarray;
      output            VARCHAR2(4000) := '0';
      start_time        TIMESTAMP;
      kount             PLS_INTEGER;
      cutter_tab        VARCHAR2(4000);
      face_altab        VARCHAR2(4000);
      merge_face_tab    VARCHAR2(4000);
      alignkount        PLS_INTEGER;
      neighbor_kount    PLS_INTEGER;


   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_FACES',NULL,'STARTING ' || p_jobid_in);


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ALIGN_FACES: Get inputs ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --get input parms
      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id_in);

      cutter_tab     := p_topo_out || '_CUTTER';
      face_altab     := p_topo_out || '_ALFACE';
      merge_face_tab := p_topo_out || '_' || p_face_out;

      galactic_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out, 'TOPO_NAME');
      galactic_faces := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out, 'FACE_TABLE');

      --create index on aligned cutter sdogeom field

      GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(cutter_tab,
                                     'SDOGEOMETRY',
                                     merge_parms.gen_merge_srid,
                                     merge_parms.gen_merge_tolerance);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ALIGN_FACES: Insert internal galactic topo faces into ' || face_altab);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --loop over each state to get internal faces

      FOR i IN 1 .. galactic_topos.COUNT
      LOOP

         --first, find out if this is an island topo, like AK or HI
         --this is both to save time, and because running align edges on AK tends to result in invalid geoms
         --I know this is kinda cheating

         psql := 'SELECT COUNT(*) '
              || 'FROM ' || cutter_tab || ' a '
              || 'WHERE '
              || 'a.source_topo != :p1 AND '
              || 'SDO_RELATE(a.sdogeometry, '
              ||            '(SELECT sdogeometry FROM ' || cutter_tab || ' WHERE source_topo = :p2), :p3) = :p4 ';

         EXECUTE IMMEDIATE psql INTO neighbor_kount USING galactic_topos(i),
                                                          galactic_topos(i),
                                                          'mask=ANYINTERACT',
                                                          'TRUE';

         IF neighbor_kount != 0
         THEN


            psql := 'INSERT /*+ APPEND */ INTO ' || face_altab || ' a '
                 || 'SELECT * FROM TABLE(GZ_TOPO_MERGE.PIPE_UNALIGNED_RECS('
                 || ':p1,:p2,:p3,:p4,:p5,:p6,:p7))';

            EXECUTE IMMEDIATE psql USING p_project_id_in,
                                         p_jobid_in,
                                         p_topo_out,
                                         galactic_topos(i),
                                         galactic_faces(i),
                                         cutter_tab,
                                         merge_parms.gen_merge_srid;

            COMMIT;

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_FACES',NULL,
                                                   'Skipping alignment for island topo ' || galactic_topos(i) );

         END IF;

      END LOOP;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ALIGN_FACES: Align all faces in ' || face_altab);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --in the XX_ALFACE table (ex Z699LS_ALFACE) we now have, in order
      --all faces along the exterior of all states. Unaligned sdo

      --check that we have something to align, and this isnt AK or PR or HI running solo
      psql := 'SELECT count(*) '
           || 'FROM '
           || face_altab || ' '
           || 'WHERE rownum = 1 ';

      EXECUTE IMMEDIATE psql INTO alignkount;


      IF alignkount != 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_FACES',NULL,
                                                'Aligning all faces in ' || face_altab || ' ' ||
                                                'with tolerance ' || merge_parms.gen_merge_null_tolerance );

         --This will align almost all true gaps and overlaps, which are usually no more than
         --coordinate rounding differences

         GZ_GEOM_UTILS.GZ_ALIGN_EDGES(face_altab,
                                      'SDOGEOMETRY',
                                      face_altab || '_AL',
                                      merge_parms.gen_merge_null_tolerance,
                                      'Y',
                                      'N'); --switch if sdo_difference is f'd

         --occasionally still leaves a sliver or overlap or 2
         --easiest just to take a second crack at it
         --why not 3?  Dont ask


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_FACES',NULL,
                                                'Pass two: Aligning all faces in ' || face_altab || ' ' ||
                                               'with tolerance ' || merge_parms.gen_merge_null_tolerance );

         GZ_GEOM_UTILS.GZ_ALIGN_EDGES(face_altab,
                                      'SDOGEOMETRY',
                                      face_altab || '_AL',
                                      merge_parms.gen_merge_null_tolerance,
                                      'Y',
                                      'N'); --switch if sdo_difference is f'd


         psql := 'SELECT count(*) FROM '
              || face_altab || ' a '
              || 'WHERE '
              || 'a.sdogeometry.sdo_gtype NOT IN (:p1,:p2) OR '
              || 'a.sdogeometry IS NULL ';

         EXECUTE IMMEDIATE psql INTO kount USING 2003, 2007;

         IF kount != 0
         THEN

            --cant build a polygon topology from garbage gtypes
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_FACES',NULL,
                                                   'ERROR: Aligned ' || face_altab || ' has ' || kount || ' non-polygon or NULL geometries',
                                                   NULL,NULL,NULL,psql,NULL,NULL,NULL);

            output := output || ' Problem with aligned ' || face_altab || '. It has ' || kount || ' non-polygon or NULL geometries|';

            RETURN output;

         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ALIGN_FACES: Insert aligned faces into ' || merge_face_tab);
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         --insert just the aligned faces
         --into the xx_merge_face table (ex Z699FSL_MERGE_FACE)


         psql := 'INSERT /*+ APPEND */ INTO ' || merge_face_tab || ' a '
              || '(a.source_face_id, a.source_topo, a.sdogeometry) '
              || '   SELECT b.face_id, b.source_topo, b.sdogeometry '
              || '   FROM ' ||  face_altab || ' b ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_FACES',NULL,
                                                'Insert aligned faces into ' || merge_face_tab,
                                                NULL,NULL,NULL,psql,NULL,NULL,NULL);


         EXECUTE IMMEDIATE psql;

         COMMIT;

      ELSE  --end skip over islands


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_FACES',NULL,
                                                'No neighbors to align ');


      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ALIGN_FACES: Insert interior faces into ' || merge_face_tab);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      /* option 1?
         select face_id, CAST('Z604LS' AS VARCHAR2(4000)), sdogeometry from Z604LS_CLIP_FACE
         where face_id IN (
         select face_id from Z604LS_CLIP_FACE
         MINUS
         select face_id from Z699FSL_merge_face
         where source_topo = 'Z604LS' )
      */

      --option 2 best?

      FOR i IN 1 .. galactic_topos.COUNT
      LOOP

         psql := 'INSERT /*+ APPEND */ INTO ' || merge_face_tab || ' a '
              || '(a.source_face_id, a.source_topo, a.sdogeometry) '
              ||    'SELECT a.face_id, CAST(:p1 AS VARCHAR2(4000)), a.sdogeometry '
              ||    'FROM ' ||  galactic_faces(i) || ' a '
              ||    'WHERE NOT EXISTS '
              ||       '(SELECT b.source_face_id FROM '
              ||       merge_face_tab || ' b '
              ||       'WHERE '
              ||       'b.source_topo = :p2 AND '
              ||       'a.face_id = b.source_face_id '
              ||       ') ';

         EXECUTE IMMEDIATE psql USING galactic_topos(i),
                                      galactic_topos(i);


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_FACES',NULL,
                                                'Insert ' || galactic_topos(i) || ' interior faces into ' || merge_face_tab,
                                                NULL,NULL,NULL,psql,NULL,NULL,NULL);

         COMMIT;

      END LOOP;


      --mix of NULL and real SRIDs.  Make consistent, will need real SRID to build topo
      --also give us a unique key
      --this is all faces, not by source topo.  All faces are in the table now

      --First, some geometries may have become NULL

      psql := 'UPDATE ' || merge_face_tab || ' a '
           || 'SET '
           || 'a.meaningless_id = rownum, '   --legal I guess. gut says bad idea
           || 'a.sdogeometry.sdo_srid = :p1 ';


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_FACES',NULL,
                                             'Update SRID and meaningless ID in ' || merge_face_tab,
                                             NULL,NULL,NULL,psql,NULL,NULL,NULL);

      EXECUTE IMMEDIATE psql USING merge_parms.gen_merge_srid;
      COMMIT;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ALIGN_FACES: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END ALIGN_FACES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   FUNCTION BUILD_TOPO_FROM_TOPO (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_jobid              IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_table              IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_topo_type          IN NUMBER,
      p_topo_col           IN VARCHAR2 DEFAULT 'TOPOGEOM',
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_value       IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! 10/28/10
      --8/9/11 Copied from GZ_CLIP, not sure if this one will be different

      psql              VARCHAR2(4000);
      tg_layer_type     VARCHAR2(32);
      tg_layer_id       NUMBER;

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_TOPO: Verify Inputs ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_topo_type NOT IN (1,2,3)
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'What the heck is a topo type of ' || p_topo_type);

      ELSE

         IF p_topo_type = 1
         THEN
            tg_layer_type := 'POINT';
         ELSIF p_topo_type = 2
         THEN
            tg_layer_type := 'LINE';
         ELSIF p_topo_type = 3
         THEN
            tg_layer_type := 'POLYGON';
         END IF;

      END IF;

      --???????????????????????????
      --What else should we check??
      --???????????????????????????

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_TOPO: Add topo layer ' || p_table);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_subset_col IS NULL
      THEN

         --building topo for the entire feature layer
         SDO_TOPO.add_topo_geometry_layer(p_topo,p_table,p_topo_col,tg_layer_type);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_TOPO: Construct features for ' || p_table);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_topo,
                                                  p_table,
                                                  p_topo_col,
                                                  tg_layer_type);

      psql := 'UPDATE ' || p_table || ' a '
           || 'SET '
           || 'a.topogeom = SDO_TOPO_GEOMETRY(:p1,:p2,:p3,SDO_TOPO_OBJECT_ARRAY(SDO_TOPO_OBJECT (a.' || p_featuretable_pkc || ',:p4))) ';


      BEGIN

         IF p_subset_col IS NOT NULL
         THEN

            psql := psql || 'WHERE '
                         || 'a.' || p_subset_col || ' = :p5 ';

            EXECUTE IMMEDIATE psql uSING p_topo,
                                         p_topo_type,
                                         tg_layer_id,
                                         p_topo_type,
                                         p_subset_value;

         ELSE

            EXECUTE IMMEDIATE psql USING p_topo,
                                         p_topo_type,
                                         tg_layer_id,
                                         p_topo_type;

         END IF;

         COMMIT;

      EXCEPTION
      WHEN OTHERS
      THEN

         RETURN DBMS_UTILITY.format_error_backtrace;

      END;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_TOPO: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN '0';

   END BUILD_TOPO_FROM_TOPO;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   FUNCTION BUILD_FACE_TOPO (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_jobid_in           IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_top_layer_tab      IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_face_out           IN VARCHAR2,
      p_add_poly           IN VARCHAR2 DEFAULT NULL,
      p_restart_flag       IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2
   AS

      --Matt! 7/08/11
      --This is the long running module that adds each galactic topo face to the merged topo
      --Using add_polygon_geometry then constructors on merged face feature table

      --12/14/12 Change to return a failed output for the module when a topo fails
      --         Also modified to automatically process all is_dead topos on restarts
      --         And change them to not is dead on successful restarts
      --         Added handler for not found in cache bug. Tested in phony test only

      --6/17/13 Minor improved logging and handling of "Attempted to add linear geometry outside the update window"


      psql              VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      output            VARCHAR2(4000) := '0';
      start_time        TIMESTAMP;
      kount             PLS_INTEGER;
      merge_face_tab    VARCHAR2(4000);
      cutter_tab        VARCHAR2(4000);
      galactic_topos    GZ_TYPES.stringarray;
      stack             VARCHAR2(4000);
      errm              VARCHAR2(8000);
      retval            VARCHAR2(4000);
      topo_mbr          SDO_GEOMETRY;
      stowed_mbr        SDO_GEOMETRY;
      invalid_topos     GZ_TYPES.stringarray;
      raise_the_roof    NUMBER := 2147483648;  --silly java


   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,'STARTING ' || p_jobid_in);


      --get input parms
      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id_in);

      cutter_tab     := p_topo_out || '_CUTTER';
      merge_face_tab := p_topo_out || '_' || p_face_out;

      IF p_restart_flag = 'N'
      THEN

         --SOP
         galactic_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out, 'TOPO_NAME');

      ELSE

         --Restart. Get the is_dead topos only
         --We will use this same logic below as well
         --however, if any topos die during a restart this wont work since we are always getting the dead topos,
         --restarts will have to simply die on the spot
         galactic_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out, 'TOPO_NAME', 'Y');

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_FACE_TOPO: Assess inputs ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                             'Assess inputs (validate geometry check) for ' ||  merge_face_tab );


      --Check that the only problem with the face table is duplicate vertices
      --we are going off the grid in terms of validation inside build topo from spatial
      psql := 'SELECT DISTINCT a.source_topo FROM '
           || merge_face_tab || ' a '
           || 'WHERE '
           || 'sdo_geom.validate_geometry_with_context(a.sdogeometry, :p1) != :p2 AND '
           || 'sdo_geom.validate_geometry_with_context(a.sdogeometry, :p3) NOT LIKE :p4 AND '
           || 'a.source_topo IN (SELECT * FROM TABLE(:p5)) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO invalid_topos USING merge_parms.gen_merge_tolerance,
                                                                   'TRUE',
                                                                   merge_parms.gen_merge_tolerance,
                                                                   '13356%',
                                                                   GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(galactic_topos);

      --!!!!!!!!!!!!!!!!!!!
      --!!!!!!!!!!!!!!!!!!!
      --13349s are possible here.  Need to decide what to do
      --probably safe, but rude, to let them thru
      --for now lets fail the state



      IF invalid_topos.COUNT != 0
      THEN

         --too risky
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_CUTTER_TOPO',NULL,
                                                 'ERROR: Aligned ' || merge_face_tab || ' has invalid (non dupe) geometries but we will continue',
                                                  NULL,NULL,NULL,psql,NULL,NULL,NULL);

            FOR i IN 1 .. invalid_topos.COUNT
            LOOP

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                      'Killing source topo ' || invalid_topos(i),
                                                      NULL,NULL,NULL,NULL,NULL,NULL );

               --this topo is dead until we can investigate, but our goal is to carry on without it
               GZ_TOPO_MERGE.DEADEN_GALACTIC_TOPO(p_topo_out,
                                                  invalid_topos(i));

               --but only carry on through the end of this module, its too messy otherwise
               output := output || 'BUILD_FACE_TOPO failure: ' || invalid_topos(i) || ' ';

               IF p_restart_flag = 'Y'
               THEN

                  --restarts die on any error
                  RETURN output;

               END IF;

            END LOOP;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 15');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_FACE_TOPO: Purge and create fresh topo ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_restart_flag = 'N'
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                'Purge and recreate topo ' ||  p_topo_out );

         --this is called the first time straight from generalization_topo_merge
         --this time it will purge before creating
         retval := GZ_TOPO_MERGE.CREATE_EMPTY_TOPOLOGY(p_schema,
                                                       p_release,
                                                       p_project_id_in,
                                                       p_jobid_in,
                                                       p_topo_out,
                                                       'Y');  --keep the cutter table

         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                   'Failed to purge and recreate topo ' ||  p_topo_out );

            output := output || 'Error creating topology ' || retval;

            --cant continue
            RETURN output;

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                   'Finished purge and recreate topo ' ||  p_topo_out );

         END IF;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_FACE_TOPO: Add spatial index to ' || merge_face_tab);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_restart_flag = 'N'
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                'Add spatial index to ' || merge_face_tab );

         --this is only necessary for the java memory manager, really
         GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(merge_face_tab,
                                        'SDOGEOMETRY',
                                        merge_parms.gen_merge_srid,
                                        merge_parms.gen_merge_tolerance);

      END IF;


      --get a new set of topos in case we killed a few above

      IF p_restart_flag = 'N'
      THEN

         --SOP
         galactic_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out, 'TOPO_NAME');

      ELSE

         --Restart. This isnt necessary since restarts die on any error
         NULL;  --claritee
         --galactic_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out, 'TOPO_NAME', 'Y');

      END IF;

      FOR i IN 1 .. galactic_topos.COUNT
      LOOP

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_FACE_TOPO: Call build topo or poly from spatial ');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------


         IF p_add_poly IS NULL
         THEN

            --never use this method, saved for posteriority and reference
            --just in case some mess in my caller accidentally passes it

            RAISE_APPLICATION_ERROR(-20001,'One way street here, BUILD_TOPO_FROM_SPATIAL method not supported');


         ELSE

            --the winna - add_poly method

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_FACE_TOPO: Call gz_add_polygon_geometry ');
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                   'Add poly for ' ||  merge_face_tab ||
                                                   ' source topo ' || galactic_topos(i));

            IF i = 1
            AND p_restart_flag = 'N'
            THEN

              --have to get sidx on face$ in order to use limited topomap
              --on add_poly
              SDO_TOPO.INITIALIZE_METADATA(p_topo_out);

            END IF;

            --get the MBR to limit our topomap
            psql := 'SELECT SDO_GEOM.SDO_MBR(a.sdogeometry) '
                 || 'FROM ' || cutter_tab || ' a '
                 || 'WHERE a.source_topo = :p1 ';

            EXECUTE IMMEDIATE psql INTO topo_mbr USING galactic_topos(i);

            <<logoturtle>>

            --call the java memory manager
            --should call set_max_memory_size for the biggies
            --remember, we have a big table with all states in it
            --calling add polys state by state on the same table
            GZ_BUSINESS_UTILS.JAVA_MEMORY_MANAGER(merge_face_tab,
                                             'SDOGEOMETRY',
                                             NULL,
                                             topo_mbr);


            BEGIN

               --THE BIG CALL HERE:
               --will populate the face_id column
               --will not populate topogeom column yet

               GZ_TOPO_MERGE.ADD_POLYS_FROM_SPATIAL(p_topo_out,
                                                    merge_face_tab,
                                                    'MEANINGLESS_ID',
                                                    'FACE_ID',  --update with face id here
                                                    'NO',       --cant validate, we have duplicate vertices from align_edges and we know this
                                                    merge_parms.gen_merge_tolerance,  --real tolerance, we're back in real SRID now
                                                    'SDOGEOMETRY',
                                                    'SOURCE_TOPO',       --on this loop just do where source_topo
                                                    galactic_topos(i),  -- is this one state
                                                    topo_mbr);
            EXCEPTION
            WHEN OTHERS
            THEN

               IF UPPER(SQLERRM) LIKE '%NOT FOUND IN CACHE%'
               OR UPPER(SQLERRM) LIKE '%OUTSIDE THE UPDATE WINDOW%'
               THEN

                  --attempt to handle. This is an oracle bug, may be patched some day
                  --usually fudging with the topomap size will fix it
                  --since its an oracle error I reserve the right to act like a petulant child

                  IF stowed_mbr IS NULL
                  THEN

                     --store for re-re-try (havent needed to program this yet)
                     stowed_mbr := topo_mbr;

                     topo_mbr := GZ_GEOM_UTILS.EXPAND_MBR_PERCENT(topo_mbr, 2);

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                            'Retry with new window (see dump->). ' || SQLERRM || ' on Add poly for ' ||  merge_face_tab ||
                                                            ' source topo ' || galactic_topos(i), p_sdo_dump=>topo_mbr);


                     GOTO logoturtle;

                  ELSE

                     --Could try other stuff here

                     output := output || 'BUILD_FACE_TOPO failure: ' || galactic_topos(i) || ' ';

                     errm := SQLERRM;
                     stack := DBMS_UTILITY.format_error_backtrace;

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                            'FAIL AGAIN in build topo for ' ||  merge_face_tab || ' source topo ' || galactic_topos(i),
                                                            NULL,NULL,NULL,NULL,NULL,substr(errm || ' | ' || stack , 1, 4000) );

                     RETURN output;

                  END IF;

               END IF;

               --unhandled errors

               --most importantly, continue through the topos but do not continue after this module in the caller
               output := output || 'BUILD_FACE_TOPO failure: ' || galactic_topos(i) || ' ';

               errm := SQLERRM;
               stack := DBMS_UTILITY.format_error_backtrace;

               --what should we catch here?  Lets at least log and continue

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                      'FAIL in build topo for ' ||  merge_face_tab || ' source topo ' || galactic_topos(i),
                                                       NULL,NULL,NULL,NULL,NULL,substr(errm || ' | ' || stack , 1, 4000) );



               IF SQLERRM LIKE '%we expect non overlapping inputs%'
               THEN

                  --This isnt a valid error I dont think.  Pre mitosis management

                  --this is my error in utilities
                  --this topo is dead until we can investigate, but our goal is to carry on without it
                  --shouldnt ever trip this, its an ADD_A_POLY_FROM_SPATIAL  error
                  GZ_TOPO_MERGE.DEADEN_GALACTIC_TOPO(p_topo_out,
                                                     galactic_topos(i));

               ELSE

                  GZ_TOPO_MERGE.DEADEN_GALACTIC_TOPO(p_topo_out,
                                                     galactic_topos(i));

               END IF;

               IF p_restart_flag = 'Y'
               THEN

                  --restarts die immediately on errors
                  RETURN output;

               END IF;


            END;


         END IF;

      END LOOP;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_FACE_TOPO: Call BUILD_TOPO_FROM_TOPO ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF p_add_poly IS NOT NULL  --always this
      THEN

         --call the constructor, have to do it state by state for error handling

         --get a new set of galactics, we may have killed a few above

         IF p_restart_flag = 'N'  --though only on first time through kill topos and continue
         THEN

            galactic_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out, 'TOPO_NAME');

            --cant do this in build_topo_from_topo since we are calling it over and over
            SDO_TOPO.add_topo_geometry_layer(p_topo_out,
                                             merge_face_tab,
                                             'TOPOGEOM',
                                             'POLYGON');

            GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_topo_out || '_RELATION$');

         END IF;



         FOR i IN 1 .. galactic_topos.COUNT
         LOOP

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                   'Calling build_topo_from_topo for ' ||  galactic_topos(i));

            retval := GZ_TOPO_MERGE.BUILD_TOPO_FROM_TOPO(p_schema,
                                                         p_project_id_in,
                                                         p_jobid_in,
                                                         p_topo_out,
                                                         merge_face_tab,
                                                         'FACE_ID',
                                                         3,
                                                         'TOPOGEOM',
                                                         'SOURCE_TOPO',        --only update where this column
                                                         galactic_topos(i));   --is this value

            IF retval != '0'
            THEN

               --some sort of failure
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                      'FAIL in build topo from topo on ' ||  merge_face_tab || ' source topo ' || galactic_topos(i),
                                                          NULL,NULL,NULL,NULL,NULL,substr(retval , 1, 4000) );

               GZ_TOPO_MERGE.DEADEN_GALACTIC_TOPO(p_topo_out,
                                                  galactic_topos(i));

               output := output || 'BUILD_FACE_TOPO BUILD_TOPO_FROM_TOPO failure on : ' || galactic_topos(i) || ' ';

               IF p_restart_flag = 'Y'
               THEN

                  RETURN output;

               END IF;

            END IF;

         END LOOP;


      END IF;

      --Moved NULL of sdogeometry from here to after assessment, next module

       ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 35');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_FACE_TOPO: Gather ' || p_topo_out || '_relation$ statistics ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                             'Calling topo tune up for stats and index tidying ');

      GZ_TOPO_UTIL.GZ_TOPO_TUNE_UP(p_topo_out);

      --All below managed in topo_tune_up
      --first fix any missing or broken relation$ indexes
      --GZ_TOPO_UTIL.FIX_RELATION_DOLLAR_INDEXES(p_topo_out);

      --We are now almost fully populated, topo-wise
      --Lets gather stats on relation$
      --Why do I do this?  Is it in the documentation anywhere? Dont think so
      --I think just because I see the DBAs doing it
      --GZ_UTILITIES.GATHER_TABLE_STATS(p_topo_out || '_RELATION$');

      IF p_restart_flag = 'Y'
      AND output = '0'
      THEN

         --if restarting and all galactic topos run now were a success
         --update all galactic topos in the cutter table to active again
         FOR i IN 1 .. galactic_topos.COUNT
         LOOP

            GZ_TOPO_MERGE.DEADEN_GALACTIC_TOPO(p_topo_out,
                                               galactic_topos(i),
                                               'Y');  --resurrect flag

         END LOOP;

      END IF;



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_FACE_TOPO: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END BUILD_FACE_TOPO;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   PROCEDURE DELETE_A_FACE (
      p_topo           IN VARCHAR2,
      p_feature_table  IN VARCHAR2,
      p_feature_face   IN NUMBER,
      p_source_face    IN NUMBER,
      p_tg_layer_id    IN NUMBER,
      p_retain_feature IN VARCHAR2 DEFAULT 'N'
   )
   AS

      --Matt! 8/29/11
      --This guy disassociates a primitive face
      --Manages the fallout on the feature face table
      --Deletes the edge from the topo if possible or requested



      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      my_tg_id          NUMBER;


   BEGIN

      --tg id is too dynamic to track in the caller
      --this is the only safe place to grab it

      psql := 'SELECT a.topogeom.tg_id '
           || 'FROM '
           || p_feature_table || ' a '
           || 'WHERE '
           || 'a.face_id = :p1 AND '
           || 'a.source_face_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO my_tg_id USING p_feature_face,
                                                 p_source_face;

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_topo || '_RELATION$ r '
           || 'WHERE '
           || 'r.tg_id = :p1 AND '
           || 'r.tg_layer_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING my_tg_id,
                                              p_tg_layer_id;



      IF kount > 1
      THEN

         --We may use the constructor, leaving at least one face with the feature other
         --than the one we are deleting
         --This is my current understanding of best practice
         --in merge usage, should never be in here
         --each face record in the merge face table has a 1 to 1 relationship with a primitive face

         psql := 'UPDATE ' ||  p_feature_table || ' a '
              || 'SET '
              || 'a.topogeom = '  --Topology name, topo geom type, tg_layer_id, no topo to add, topo to delete (face_id, type)
              || 'SDO_TOPO_GEOMETRY(:p1, :p2, :p3, NULL, SDO_TOPO_OBJECT_ARRAY(SDO_TOPO_OBJECT(:p4,:p5))) '
              || 'WHERE '
              || 'a.face_id = :p6 AND '
              || 'a.source_face_id = :p7 ';

         EXECUTE IMMEDIATE psql USING p_topo,
                                      3,
                                      p_tg_layer_id,
                                      p_feature_face,
                                      3,
                                      p_feature_face,
                                      p_source_face;

         COMMIT;

      ELSIF kount = 1
      AND p_retain_feature = 'N'
      THEN


         psql := 'DELETE FROM ' || p_feature_table || ' a '
              || 'WHERE '
              || 'a.face_id = :p1 AND '
              || 'a.source_face_id = :p2 ';

         EXECUTE IMMEDIATE psql USING p_feature_face,
                                      p_source_face;

         COMMIT;

      ELSIF kount = 1
      AND p_retain_feature = 'Y'
      THEN

         --we want this feature record to stick around, but disassociate the primitive
         --confirmed that updating to NULL does delete the relation$ pointers

         psql := 'UPDATE ' || p_feature_table || ' a '
              || 'SET a.topogeom = NULL '
              || 'WHERE '
              || 'a.face_id = :p1 AND '
              || 'a.source_face_id = :p2 ';

         EXECUTE IMMEDIATE psql USING p_feature_face,
                                      p_source_face;
         COMMIT;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'No matching tg_id(' || my_tg_id || ')  for this tg_layer_id(' || p_tg_layer_id || ') ');

      END IF;


      --no primitives to delete in this flavor


   END DELETE_A_FACE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC--------------------------------------------------------------------------------

   FUNCTION COUNT_TOPO_GAPS (
      p_topo                 IN VARCHAR2,
      p_feature_face_tab     IN VARCHAR2
   ) RETURN NUMBER
   AS

      --Matt! 9/14/11
      --a gap is a face primitive with no face feature

      psql     VARCHAR2(4000);
      kount    NUMBER;

   BEGIN

      psql := 'SELECT COUNT(ff.face_id) '
           || 'FROM '
           || p_topo || '_face$ ff '
           || 'WHERE ff.face_id != :p1 '
           || 'AND ff.face_id NOT IN '
           || '(SELECT DISTINCT f.face_id '
           || 'FROM '
           || p_feature_face_tab || ' a, '
           || p_topo || '_relation$ r, '
           || p_topo || '_face$ f '
           || 'WHERE '
           || 'a.topogeom.tg_id = r.tg_id AND '
           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'r.topo_id = f.face_id) ';

      EXECUTE IMMEDIATE psql INTO kount USING -1;

      RETURN kount;

   END COUNT_TOPO_GAPS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC--------------------------------------------------------------------------------

   FUNCTION GET_TOPO_GAPS (
      p_topo                 IN VARCHAR2,
      p_feature_face_tab     IN VARCHAR2
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 9/14/11
      --a gap is a face primitive with no face feature
      --see count_topo_gaps above for same sql

      psql     VARCHAR2(4000);
      gaps     GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT ff.face_id '
           || 'FROM '
           || p_topo || '_face$ ff '
           || 'WHERE ff.face_id != :p1 '
           || 'AND ff.face_id NOT IN '
           || '(SELECT DISTINCT f.face_id '
           || 'FROM '
           || p_feature_face_tab || ' a, '
           || p_topo || '_relation$ r, '
           || p_topo || '_face$ f '
           || 'WHERE '
           || 'a.topogeom.tg_id = r.tg_id AND '
           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'r.topo_id = f.face_id) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO gaps USING -1;

      RETURN gaps;

   END GET_TOPO_GAPS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------------

   FUNCTION COUNT_TOPO_OVERLAPS (
      p_topo                 IN VARCHAR2,
      p_feature_face_tab     IN VARCHAR2
   ) RETURN NUMBER
   AS

      --Matt! 8/29/11
      --an overlap is a face primitive shared by two face feature records
      --subtract distinct faces in the group from total faces

      psql     VARCHAR2(4000);
      kount    NUMBER;

   BEGIN

      psql := 'SELECT '
           || '( COUNT(f.face_id) - COUNT(distinct f.face_id) ) FROM '
           || p_feature_face_tab || ' a, '
           || p_topo || '_relation$ r, '
           || p_topo || '_face$ f '
           || 'WHERE '
           || 'a.topogeom.tg_id = r.tg_id AND '
           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'r.topo_id = f.face_id ';

      EXECUTE IMMEDIATE psql INTO kount;

      RETURN kount;

   END COUNT_TOPO_OVERLAPS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------------

   FUNCTION GET_SIMPLE_FACE_GEOMETRY (
      p_topo                 IN VARCHAR2,
      p_face_id              IN NUMBER,
      p_tolerance            IN NUMBER DEFAULT .05,
      p_valid_out            IN VARCHAR2 DEFAULT 'Y'
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 8/31/11
      --Simple fn to return the geom of a simple face
      --Not 100 pct confident it is robust


      psql           VARCHAR2(4000);
      lines          SDO_GEOMETRY;
      output         SDO_GEOMETRY;
      kount          PLS_INTEGER;
      valstr         VARCHAR2(4000);
      outords        SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();

   BEGIN

      --validate existence of face
      psql := 'SELECT COUNT(*) '
           || 'FROM '
           || p_topo || '_face$ f '
           || 'WHERE '
           || 'f.face_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_face_id;

      IF kount != 1
      THEN

         RAISE_APPLICATION_ERROR(-20001,'No face id ' || p_face_id || ' in ' || p_topo || '_face$');

      END IF;

      --first check that this is a simple face with nothing interior
      --I dont wanna deal with that mess

      psql := 'SELECT COUNT(t.column_value) '
           || 'FROM '
           || p_topo || '_face$ f, '
           || 'TABLE(f.island_edge_id_list) t '
           || 'WHERE f.face_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_face_id;

      IF kount > 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry dude, ' || p_face_id || ' is too complex. ' || kount || ' islands ');

      END IF;

      --get linear geom

      psql := 'SELECT SDO_AGGR_CONCAT_LINES(e.geometry) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.left_face_id = :p1 OR '
           || 'e.right_face_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO lines USING  p_face_id,
                                               p_face_id;


      --fingers crossed

      output := SDO_GEOMETRY
                (2003,
                 lines.sdo_srid,
                 NULL,
                 SDO_ELEM_INFO_ARRAY(1, 1003, 1),
                 NULL
                 );

      output.sdo_ordinates := lines.sdo_ordinates;


      --just once
      valstr := sdo_geom.validate_geometry_with_context(output, p_tolerance);

      IF valstr != 'TRUE'
      THEN

         IF valstr LIKE '13367%'
         THEN

            --reversed orientation. An innie on some other face

            --set up
            output.sdo_ordinates := NULL;
            kount := 0;
            outords.EXTEND(lines.sdo_ordinates.COUNT);

            FOR i IN REVERSE 1 .. lines.sdo_ordinates.COUNT
            LOOP

               --cluephone: the first ordinate in this loop is a Y, not an X
               --or more generally, switch ya odds and ya evens

               kount := kount + 1;

               IF MOD(i,2) = 0
               THEN

                  outords(kount) := lines.sdo_ordinates(i - 1);

               ELSE

                  outords(kount) := lines.sdo_ordinates(i + 1);

               END IF;


            END LOOP;

            output.sdo_ordinates := outords;

            valstr := sdo_geom.validate_geometry_with_context(output, p_tolerance);

            IF valstr LIKE '13367%'
            THEN

               --this is never good
               RAISE_APPLICATION_ERROR(-20001,'FAIL, attempt at simple face geometry is still clockwise');

            END IF;

            IF valstr = 'TRUE'
            OR p_valid_out = 'N'
            THEN

               RETURN output;

            ELSE

               RAISE_APPLICATION_ERROR(-20001, 'Doh, output is invalid');

            END IF;


         ELSIF p_valid_out = 'N'
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Doh, output is invalid');

         END IF;

      END IF;

      RETURN output;

   END GET_SIMPLE_FACE_GEOMETRY;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION OVERLAP_MANAGER (
      p_topo                 IN VARCHAR2,
      p_feature_face_tab     IN VARCHAR2
   ) RETURN NUMBER
   AS

      --Matt! 8/26/11

      --Reminder of what these things usually are:
      --   - We have a sliver face in the input
      --   - After alignment of faces along the galactic topo outline a neighboring
      --   - face flops over on top of the sliver
      --   - NB, the alternative, adding the sliver to the alignment is also tricky - may disappear
      --   - Simple enough here to throw the fatty face off of the overlapped tiny face

      psql                 VARCHAR2(4000);
      overlapkount         NUMBER;
      primitive_faces      GZ_TYPES.stringarray;
      kount_feat_face      PLS_INTEGER;
      dist_feat_face       PLS_INTEGER;
      kount_prim_face      PLS_INTEGER;
      source_faces         GZ_TYPES.stringarray;
      prim_faces           GZ_TYPES.stringarray;
      dist_source          PLS_INTEGER;
      dist_face            PLS_INTEGER;
      dup_src_face         NUMBER;
      tg_layer_id          NUMBER;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('OVERLAP_MANAGER: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --get list of faces that are overlaps

      psql := 'SELECT f.face_id '
           || 'FROM '
           || p_feature_face_tab || ' a, '
           || p_topo || '_relation$ r, '
           || p_topo || '_face$ f '
           || 'WHERE '
           || 'a.topogeom.tg_id = r.tg_id AND '
           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'r.topo_id = f.face_id '
           || 'GROUP BY f.face_id '
           || 'HAVING COUNT(f.face_id) > :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO primitive_faces USING 1;

      --get this just one time
      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_topo,
                                                  p_feature_face_tab,
                                                  'TOPOGEOM',
                                                  'POLYGON');


      FOR i IN 1 .. primitive_faces.COUNT
      LOOP

          --figure out what kind of overlap we have for this face

          psql := 'SELECT a.source_face_id, f.face_id '
               || 'FROM '
               || p_feature_face_tab || ' a, '
               || p_topo || '_relation$ r, '
               || p_topo || '_face$ f '
               || 'WHERE '
               || 'a.topogeom.tg_id = r.tg_id AND '
               || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
               || 'r.topo_id = f.face_id AND '
               || '(a.source_face_id, a.source_topo) IN '
               || '( '
               ||    'select a.source_face_id, a.source_topo '
               ||    'FROM '
               ||    p_feature_face_tab || ' a, '
               ||    p_topo || '_relation$ r, '
               ||    p_topo || '_face$ f '
               ||    'WHERE '
               ||    'a.topogeom.tg_id = r.tg_id AND '
               ||    'a.topogeom.tg_layer_id = r.tg_layer_id AND '
               ||    'r.topo_id = f.face_id AND '
               ||    'f.face_id = :p1 '
               || ') ';

           GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo,'OVERLAP_MANAGER',NULL,
                                                  'Figuring out what to do with overlap face ' || primitive_faces(i),
                                                  NULL,NULL,NULL,psql,NULL,NULL,NULL);

           EXECUTE IMMEDIATE psql BULK COLLECT INTO source_faces,
                                                    prim_faces USING primitive_faces(i);

           psql := 'SELECT COUNT(DISTINCT column_value) from TABLE(:p1)';

           EXECUTE IMMEDIATE psql INTO dist_source USING GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(source_faces);

           EXECUTE IMMEDIATE psql INTO dist_face USING GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(prim_faces);

           IF source_faces.COUNT = 3
           AND prim_faces.COUNT = 3
           AND dist_source = 2
           AND dist_face = 2
           THEN

              --This is the most likely outcome
              --One big feature face has split and taken over some itty bitty neighbor sliver face
              --need to remove that itty bitty face from the hungry hippo

              psql := 'SELECT column_value FROM TABLE(:p1) '
                   || 'GROUP BY column_value '
                   || 'HAVING COUNT(column_value) = :p2 ';

              EXECUTE IMMEDIATE psql INTO dup_src_face USING GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(source_faces),
                                                             2;

              GZ_TOPO_MERGE.DELETE_A_FACE(p_topo,
                                          p_feature_face_tab,
                                          primitive_faces(i), --feature face matches primitive face
                                          dup_src_face,
                                          tg_layer_id);



           ELSE

              RAISE_APPLICATION_ERROR(-20001,'I dont know how to handle ' || dist_face || ' faces for ' || dist_source ||
                                            ' features. Face id is ' || primitive_faces(i));

           END IF;



      END LOOP;


      --see what we have now and return to caller

      overlapkount := GZ_TOPO_MERGE.COUNT_TOPO_OVERLAPS(p_topo,
                                                        p_feature_face_tab);



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('OVERLAP_MANAGER: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN overlapkount;

   END OVERLAP_MANAGER;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   PROCEDURE ADD_A_FACE (
      p_topo                 IN VARCHAR2,
      p_feature_face_tab     IN VARCHAR2,
      p_face_id              IN NUMBER,
      p_feature_id           IN NUMBER,
      p_feature_pkc          IN VARCHAR2 DEFAULT 'FACE_ID',
      p_topo_col             IN VARCHAR2 DEFAULT 'TOPOGEOM'
   )
   AS

      --Matt! 9/20/11
      --Adds a primitive face to a feature topogeom record using constructor

      psql           VARCHAR2(4000);
      tg_layer_id    NUMBER;


   BEGIN

      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_topo,
                                                  p_feature_face_tab,
                                                  p_topo_col,
                                                  'POLYGON');

      psql := 'UPDATE '
            || p_feature_face_tab || ' a '
            || 'SET '
            || 'a.' || p_topo_col || ' = SDO_TOPO_GEOMETRY('
            || ':p1, ' -- Topology name
            || ':p2, ' -- Topology geometry type (3)
            || ':p3, ' -- TG_LAYER_ID
            || 'SDO_TOPO_OBJECT_ARRAY ('
            || 'SDO_TOPO_OBJECT (:p4, :p5)), ' --face_id, topo_type
            || 'NULL '
            || ') ' -- No topological elements to be deleted
            || 'WHERE a.' || p_feature_pkc || ' = :p6 ';


      BEGIN

         EXECUTE IMMEDIATE psql USING UPPER(p_topo),
                                      3,
                                      tg_layer_id,
                                      p_face_id,
                                      3,
                                      p_feature_id;

      EXCEPTION
      WHEN OTHERS
      THEN

         --what to expect here?
         RAISE;

      END;

      COMMIT;

   END ADD_A_FACE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION GAP_MANAGER (
      p_topo                 IN VARCHAR2,
      p_feature_face_tab     IN VARCHAR2,
      p_null_tolerance       IN NUMBER,
      p_max_chasm            IN NUMBER,
      p_tolerance            IN NUMBER DEFAULT .05,
      p_log_type             IN VARCHAR2 DEFAULT 'MERGE'
   ) RETURN NUMBER
   AS

      --Matt! 9/20/11
      --Made public and optional logging 4/4/12

      --Reminder of what these things usually are:
      --   We have a "glancing V" at the state outline
      --   Align_edges pinches the neck of the V into two polygons lifting one side of the V
      --      up and away from the interior neighbor face
      --   The interior neighbor face never gets touched by align_edges, it isnt on the state outline
      --      (we could start aligning these, but then we'd be mending gaps and overlaps in the interior too)
      --   The gap is generally entirely interior to one of the aligned faces
      --   But since it's so pointless and miniscule, we will just add it to whatever face shares its longests edge


      psql                 VARCHAR2(4000);
      gapkount             NUMBER;
      gap_faces            GZ_TYPES.stringarray;
      max_length           NUMBER;
      merge_face           NUMBER;
      nuke_edge            NUMBER;
      remaining_face       GZ_TYPES.stringarray;
      gap_geom             SDO_GEOMETRY;
      gap_width            NUMBER;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GAP_MANAGER: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      gap_faces := GZ_TOPO_MERGE.GET_TOPO_GAPS(p_topo,
                                               p_feature_face_tab);


      --Deal with each gap

      FOR i IN 1 .. gap_faces.COUNT
      LOOP

         --better set these to NULL in case we flub loop 2+
         max_length := NULL;
         merge_face := NULL;
         nuke_edge  := NULL;

         IF p_log_type IS NOT NULL
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topo,'GAP_MANAGER',NULL,
                                                   'Figuring out what to do with overlap face ' || gap_faces(i),
                                                    NULL,NULL,NULL,NULL,NULL,NULL,NULL);

         END IF;

         --get the width of the gap

         gap_geom := GZ_TOPO_MERGE.GET_SIMPLE_FACE_GEOMETRY(p_topo,gap_faces(i));

         gap_geom.sdo_srid := NULL;

         gap_width := GZ_GEOM_UTILS.MEASURE_SLIVER_WIDTH(gap_geom,
                                                         100,  --sample kount
                                                         p_null_tolerance * p_null_tolerance);  --hopped up tolerance since likely to be bad

         IF gap_width > p_max_chasm
         THEN

            --cant mess with this guy, he is meaningful

            IF p_log_type IS NOT NULL
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topo,'GAP_MANAGER',NULL,
                                                      'Bailing on face ' || gap_faces(i) || ', face width is ' || gap_width,
                                                       NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            END IF;

         ELSE

            IF p_log_type IS NOT NULL
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topo,'GAP_MANAGER',NULL,
                                                      'Ok to manage face ' || gap_faces(i) || ', face width is ' || gap_width,
                                                       NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            END IF;

            --find the longest edge next door.  Gotta be a better way than the two step

            psql := 'SELECT MAX(SDO_GEOM.SDO_LENGTH(e.geometry,:p1)) '
                 || 'FROM ' || p_topo || '_edge$ e '
                 || 'WHERE '
                 || 'e.left_face_id = :p2 OR '
                 || 'e.right_face_id = :p3 ';

            EXECUTE IMMEDIATE psql INTO max_length USING p_tolerance,
                                                         gap_faces(i),
                                                         gap_faces(i);


            --this is the face we want to add to and the edge in between to nuke

            psql := 'SELECT e.left_face_id, e.edge_id '
                 || 'FROM '
                 || p_topo || '_edge$ e '
                 || 'WHERE e.right_face_id = :p1 AND '
                 || 'sdo_geom.sdo_length(e.geometry,:p2) = :p3 '
                 || 'UNION ALL '
                 || 'SELECT e.right_face_id, e.edge_id '
                 || 'FROM '
                 || p_topo || '_edge$ e '
                 || 'WHERE e.left_face_id = :p4 AND '
                 || 'sdo_geom.sdo_length(e.geometry,:p5) = :p6 ';

            EXECUTE IMMEDIATE psql INTO merge_face,
                                        nuke_edge USING gap_faces(i),
                                                        p_tolerance,
                                                        max_length,
                                                        gap_faces(i),
                                                        p_tolerance,
                                                        max_length;

             --there is a 1 to 1 relationship between primitive faces
             --and merge face table face ids.  Even those experiencing mitosis
             --are 1 to 1 on this side of the equation

             --add the gap face to the to be merged feature face using constructor

              IF p_log_type IS NOT NULL
              THEN

                 GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topo,'GAP_MANAGER',NULL,
                                                        'Adding primitive face ' || gap_faces(i) || ' to '
                                                        || 'feature face ' || merge_face,
                                                        NULL,NULL,NULL,NULL,NULL,NULL,NULL);

             END IF;

             GZ_TOPO_MERGE.ADD_A_FACE(p_topo,
                                      p_feature_face_tab,
                                      gap_faces(i),
                                      merge_face);


            --remove the edge

            IF p_log_type IS NOT NULL
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topo,'GAP_MANAGER',NULL,
                                                      'Nukeing (sp?) edge ' || nuke_edge || ' between gap face and feature face ',
                                                      NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            END IF;

            --should have a sub for this I think
            SDO_TOPO_MAP.REMOVE_EDGE(p_topo,
                                     nuke_edge);

            --check what we have now in primitive faces
            --after deleting the edge oracle can leave either face behind

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_topo || '_face$ f '
                 || 'WHERE f.face_id IN (:p1,:p2) ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO remaining_face USING merge_face,
                                                                          gap_faces(i);

            IF remaining_face.COUNT > 1
            THEN

               RAISE_APPLICATION_ERROR(-20001,'Didnt delete the gao face? ' || psql);

            ELSIF remaining_face(1) = merge_face
            THEN

               --successfully merged and face id is unchanged

               IF p_log_type IS NOT NULL
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topo,'GAP_MANAGER',NULL,
                                                         'Gap appears to be gone, face ' || remaining_face(1) || ' remains ',
                                                          NULL,NULL,NULL,psql,NULL,NULL,NULL);

               END IF;

            ELSIF remaining_face(1) = gap_faces(i)
            THEN

               --update remaining face feature face_id
               --NB source_face_id and source_topo are unchanged, we can still track back to source

               psql := 'UPDATE '
                     || p_feature_face_tab || ' a '
                     || 'SET '
                     || 'a.face_id = :p1 '
                     || 'WHERE '
                     || 'a.face_id = :p2 ';

               EXECUTE IMMEDIATE psql USING remaining_face(1),
                                            merge_face;

               COMMIT;

               IF p_log_type IS NOT NULL
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topo,'GAP_MANAGER',NULL,
                                                         'Gap appears to be gone, updated ' || p_feature_face_tab
                                                         || ' face_id ' || merge_face || ' to ' || ' ' || remaining_face(1),
                                                         NULL,NULL,NULL,psql,NULL,NULL,NULL);

               END IF;


            ELSE

               RAISE_APPLICATION_ERROR(-20001, 'No possible');

            END IF;

         END IF; --end if on size of gap being acceptable


      END LOOP;


      --howd we do?

      gapkount := GZ_TOPO_MERGE.COUNT_TOPO_GAPS(p_topo,
                                                p_feature_face_tab);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GAP_MANAGER: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      RETURN gapkount;

   END GAP_MANAGER;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION ASSESS_FACE_TOPO (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_jobid_in           IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_top_layer_tab      IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_face_out           IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 7/08/11


      psql              VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      output            VARCHAR2(4000) := '0';
      start_time        TIMESTAMP;
      kount             PLS_INTEGER;
      merge_face_tab    VARCHAR2(4000);
      overlapkount      NUMBER;
      gapkount          NUMBER;
      galactic_topos    GZ_TYPES.stringarray;


   BEGIN


      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_CUTTER_TOPO',NULL,'STARTING ' || p_jobid_in);

      --get input parms
      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id_in);

      merge_face_tab := p_topo_out || '_' || p_face_out;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ASSESS_FACE_TOPO: Check out overlaps ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --an overlap is a face primitive shared by two face feature records
      --subtract distinct faces in the group from total faces

      overlapkount := GZ_TOPO_MERGE.COUNT_TOPO_OVERLAPS(p_topo_out,
                                                        merge_face_tab);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                             overlapkount || ' overlaps in ' ||  merge_face_tab || ' ',
                                             NULL,NULL,NULL,NULL,NULL,NULL,NULL);

      IF overlapkount != 0
      THEN


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                                'Calling overlap manager ',
                                                NULL,NULL,NULL,NULL,NULL,NULL,NULL);

         overlapkount := GZ_TOPO_MERGE.OVERLAP_MANAGER(p_topo_out,
                                                       merge_face_tab);

         --check again

         IF overlapkount != 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                                   'ERROR: Aligned ' || merge_face_tab || ' topogeom has ' || kount || ' overlapping faces ',
                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            output := output || ' Problem with aligned ' || merge_face_tab || ' topogeom. It has ' || kount || '  overlapping faces|';

            RETURN output;

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                                   'Snuffed overlaps in ' || merge_face_tab || ' topogeom ',
                                                    NULL,NULL,NULL,NULL,NULL,NULL,NULL);


         END IF;

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                                'Continuing, no overlaps in ' || merge_face_tab || ' topogeom ',
                                                NULL,NULL,NULL,NULL,NULL,NULL,NULL);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ASSESS_FACE_TOPO: Check out gaps ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --a gap is a face id that isnt associated with one of the face features

      gapkount := GZ_TOPO_MERGE.COUNT_TOPO_GAPS(p_topo_out,
                                                merge_face_tab);


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                             gapkount || ' gaps in ' ||  merge_face_tab || ' ',
                                             NULL,NULL,NULL,NULL,NULL,NULL,NULL);

      IF gapkount != 0
      THEN


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                                'Calling gap manager ',
                                                NULL,NULL,NULL,NULL,NULL,NULL,NULL);

         gapkount := GZ_TOPO_MERGE.GAP_MANAGER(p_topo_out,
                                               merge_face_tab,
                                               merge_parms.gen_merge_null_tolerance,
                                               merge_parms.gen_merge_error_chasm,
                                               merge_parms.gen_merge_tolerance);

         --check again

         IF gapkount != 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                                   'ERROR: Aligned ' || merge_face_tab || ' topogeom has ' || gapkount || ' gap faces ',
                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            output := output || ' Problem with aligned ' || merge_face_tab || ' topogeom. It has ' || gapkount || '  gap faces|';

            RETURN output;

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                                   'Snuffed gaps in ' || merge_face_tab || ' topogeom ',
                                                    NULL,NULL,NULL,NULL,NULL,NULL,NULL);


         END IF;

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                                'Continuing, no gaps in ' || merge_face_tab || ' topogeom ',
                                                NULL,NULL,NULL,NULL,NULL,NULL,NULL);

      END IF;


      --Thats all to check for now
      --Maybe more later

       ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ASSESS_FACE_TOPO:: NULL successful sdogeometries ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --At this point we have some or all topos with topogeom and face_id
      --If a face has split, his sdogeometry no longer matches his topo
      --And if a topo failed, we just have aligned but dead SDO
      --NULL the sdo for successes, since we will recalc it anyway
      --And to avoid any possible confusion
      --However, if we just discovered a problem, leave them all there for easier investigation

      IF output = '0'  --actually should have exited if any non-0 exit
      THEN

         --get funky fresh
         galactic_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out, 'TOPO_NAME');

         psql := 'UPDATE ' || merge_face_tab || ' a '
              || 'SET '
              || 'a.sdogeometry = NULL '
              || 'WHERE '
              || 'a.source_topo IN (SELECT * FROM TABLE(:p1)) ';

         EXECUTE IMMEDIATE psql USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(galactic_topos);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ASSESS_FACE_TOPO: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      RETURN output;

   END ASSESS_FACE_TOPO;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION TRANSFER_FACE_ATTRIBUTES (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_jobid_in           IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_top_layer_tab      IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_face_out           IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 7/08/11


      psql              VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      output            VARCHAR2(4000) := '0';
      start_time        TIMESTAMP;
      kount             PLS_INTEGER;
      merge_face_tab    VARCHAR2(4000);
      galactic_faces    GZ_TYPES.stringarray;
      galactic_topos    GZ_TYPES.stringarray;
      geogs             GZ_TYPES.stringarray;
      psql_2            VARCHAR2(4000);


   BEGIN


      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TRANSFER_FACE_ATTRIBUTES',NULL,'STARTING ' || p_jobid_in);

      --get input parms
      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id_in);

      merge_face_tab := p_topo_out || '_' || p_face_out;

      galactic_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out, 'TOPO_NAME');
      galactic_faces := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out, 'FACE_TABLE');

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('TRANSFER_FACE_ATTRIBUTES: Populate face feature face ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --if we got this far there is a 1 to 1 relationship between face features and primitive faces
      --but we have yet to actually update the face column on the face feature table
      --doesnt really belong here but not sure where else to put it

      --add polys from spatial handles this

      /* reference
         update Z699LM_MERGE_FACE a
         set a.face_id = (
         select f.face_id
         from Z699LM_MERGE_FACE aa,
         z699lm_relation$ r,
         z699lm_face$ f
         where aa.topogeom.tg_id = r.tg_id
         and aa.topogeom.tg_layer_id = r.tg_layer_id
         and f.face_id = r.topo_id
         and a.meaningless_id = aa.meaningless_id)
      */

      /*
      psql := 'UPDATE ' || merge_face_tab || ' a '
           || 'SET a.face_id = ( '
           || 'SELECT f.face_id '
           || 'FROM '
           || merge_face_tab || ' aa, '
           || p_topo_out || '_relation$ r, '
           || p_topo_out || '_face$ f '
           || 'WHERE '
           || 'aa.topogeom.tg_id = r.tg_id AND '
           || 'aa.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'f.face_id = r.topo_id AND '
           || 'a.meaningless_id = aa.meaningless_id) ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TRANSFER_FACE_ATTRIBUTES',NULL,
                                             'Populating ' || merge_face_tab || ' feature face_ids ',
                                             NULL,NULL,NULL,psql,NULL,NULL,NULL);

      EXECUTE IMMEDIATE psql;

      COMMIT;

      */

      --verify, only check live topos

      psql := 'SELECT count(*) '
           || 'FROM '
           ||  merge_face_tab || ' a '
           || 'WHERE '
           || 'a.face_id IS NULL AND '
           || 'a.source_topo IN (SELECT * FROM TABLE(:p1))';

      EXECUTE IMMEDIATE psql INTO kount USING GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(galactic_topos);

      IF kount != 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TRANSFER_FACE_ATTRIBUTES',NULL,
                                                'We have ' || kount || ' NULL face_ids in ' || merge_face_tab,
                                                NULL,NULL,NULL,psql,NULL,NULL,NULL);

         output := output || 'We have ' || kount || ' NULL face_ids in ' || merge_face_tab || ' |';

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('TRANSFER_FACE_ATTRIBUTES: Populate face geogs ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Roll through each source topo and call the update

      geogs := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                      p_project_id_in,
                                                      'ATTRIBUTE',
                                                      merge_parms.gen_merge_face_fields);


      --get the SQL string started

          /*reference
            update Z699LM_MERGE_FACE a
            set (a.aiannhce, a.anrcfp, a.artli, a.blkgrpce, a.cbsafp) = (
            SELECT b.aiannhce, b.anrcfp, b.artli, b.blkgrpce, b.cbsafp
            FROM Z623LS_CLIP_FACE b
            WHERE b.face_id = a.source_face_id
            )
            WHERE a.source_topo = 'Z623LS'
         */

      psql := 'UPDATE ' || merge_face_tab || ' a '
           || 'SET '
           || '(';

      FOR i IN 1 .. geogs.COUNT
      LOOP

         IF i != geogs.COUNT
         THEN

            psql := psql || 'a.' || geogs(i) || ',';

         ELSE

            psql := psql || 'a.' || geogs(i);

         END IF;

      END LOOP;

      psql := psql || ') = (SELECT ';

      FOR i IN 1 .. geogs.COUNT
      LOOP

         IF i != geogs.COUNT
         THEN

            psql := psql || 'b.' || geogs(i) || ',';

         ELSE

            psql := psql || 'b.' || geogs(i);

         END IF;

      END LOOP;

      psql := psql || ' FROM ';


      FOR i IN 1 .. galactic_faces.COUNT
      LOOP

         --execute one time per source topo face table

         psql_2 := psql || galactic_faces(i) || ' b '
                || 'WHERE '
                || 'b.face_id = a.source_face_id '
                || ') '
                || 'WHERE a.source_topo = :p1 ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TRANSFER_FACE_ATTRIBUTES',NULL,
                                                'Transfer attributes from ' || galactic_faces(i) || ' to ' || merge_face_tab,
                                                NULL,NULL,NULL,psql_2,NULL,NULL,NULL);

         EXECUTE IMMEDIATE psql_2 USING galactic_topos(i);  --same kounter

         COMMIT;


      END LOOP;


      --anything to check here?  how about geoid

      psql := 'SELECT count(*) '
           || 'FROM '
           || merge_face_tab || ' a '
           || 'WHERE '
           || 'a.geoid is NULL AND '
           || 'a.source_topo IN (SELECT * FROM TABLE(:p1))';

      EXECUTE IMMEDIATE psql INTO kount USING GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(galactic_topos);

      IF kount != 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TRANSFER_FACE_ATTRIBUTES',NULL,
                                                'We have ' || kount || ' NULL geoids in ' || merge_face_tab || ' |',
                                                NULL,NULL,NULL,psql,NULL,NULL,NULL);

         output := output || 'We have ' || kount || ' NULL geoids in ' || merge_face_tab || ' |';

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('TRANSFER_FACE_ATTRIBUTES: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END TRANSFER_FACE_ATTRIBUTES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION POPULATE_MEASUREMENTS_ETC (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_jobid_in           IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_top_layer_tab      IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_face_out           IN VARCHAR2,
      p_validate_topo      IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge           IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge          IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa         IN VARCHAR2 DEFAULT 'Y'    
   ) RETURN VARCHAR2
   AS

      --Matt! 8/03/11
      --The ETC is because lots happens in here other than measurements specifically
      --1/23/12 Updates for auto topo fix


      psql              VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      output            VARCHAR2(4000) := '0';
      start_time        TIMESTAMP;
      kount             PLS_INTEGER;
      merge_face_tab    VARCHAR2(4000);
      measurements      GZ_TYPES.stringarray;
      measurehash       GZ_TYPES.stringhash;
      tg_layer_id       NUMBER;
      tg_ids            GZ_TYPES.stringarray;
      tg_layer_ids      GZ_TYPES.stringarray;
      nullgeompsql      VARCHAR2(4000);
      nullgeomkount     NUMBER;
      badgeompsql       VARCHAR2(4000);
      badfaces          GZ_TYPES.stringarray;
      fix_val           VARCHAR2(4000);
      validstr          VARCHAR2(4000);
      galactic_topos    GZ_TYPES.stringarray;
      invalidfacekount  PLS_INTEGER := 0;
      edgefix_val       VARCHAR2(1);


   BEGIN


      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,'STARTING ' || p_jobid_in);

      --get input parms
      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id_in);

      merge_face_tab := p_topo_out || '_' || p_face_out;

      measurements := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                             p_project_id_in,
                                                            'MEASUREMENT',
                                                            merge_parms.gen_merge_face_fields);

      --note that there is a QC in this assoc array but we will never touch it below
      measurehash := GZ_TOPO_MERGE.STRINGARRAY2HASH(measurements);

      --just successful
      galactic_topos := GZ_TOPO_MERGE.GET_GALACTIC_TOPOS(p_topo_out, 'TOPO_NAME');

      IF p_validate_topo = 'Y'
      THEN
      
         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Validate topology');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         BEGIN

            validstr := GZ_TOPO_UTIL.GZ_VALIDATE_TOPOLOGY(p_topo => p_topo_out,
                                                          p_outline_table => p_topo_out || '_CUTTER',
                                                          p_column => 'SDOGEOMETRY',
                                                          p_log_type => 'MERGE');

         EXCEPTION
         WHEN OTHERS
         THEN

            --this is a FAIL but we'll continue
            output := output || ' GZ_VALIDATE_TOPOLOGY threw ' || SQLERRM || ' |';

         END;

         IF validstr = 'TRUE'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Sweet: ' || p_topo_out || ' is valid ',
                                                    NULL,NULL,NULL,NULL,NULL,NULL,NULL);

         END IF;
         
      ELSE
      
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                      'Skipping topo validation since validate_topology is ' || p_validate_topo,
                                                       NULL,NULL,NULL,NULL,NULL,NULL,NULL);
                                                    
      END IF;



      --lets do this too, mimic the copier validation

      --usually 2
      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_topo_out,
                                                  merge_face_tab,
                                                  'TOPOGEOM',
                                                  'POLYGON');

      --mimic SQL in topo copier

      psql := 'SELECT f.topogeom.tg_id, f.topogeom.tg_layer_id '
           || 'FROM '
           || merge_face_tab || ' f '
           || 'WHERE f.source_topo IN (SELECT * FROM TABLE(:p1)) '
           || 'MINUS '
           || 'SELECT tg_id, tg_layer_id '
           || 'FROM '
           || p_topo_out || '_relation$ '
           || 'WHERE tg_layer_id = :p2 ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                             'Check if face tg_ids match',
                                             NULL,NULL,NULL,psql,NULL,NULL,NULL);

      EXECUTE IMMEDIATE psql BULK COLLECT INTO tg_ids,
                                               tg_layer_ids USING GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(galactic_topos),
                                                                  tg_layer_id;

      IF tg_ids.COUNT > 0
      THEN

         --this is a FAIL but we'll continue
         output := output || ' We have tg_ids with no match in relation$ |';

      END IF;


      IF measurehash.EXISTS('SDOGEOMETRY')
      THEN


         IF p_fix_edge = 'Y'
         THEN
         
            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 19');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Call gz_fix_edge ');
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Calling gz_fix_edge on all of ' || p_topo_out,
                                                    NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            edgefix_val := GZ_TOPOFIX.GZ_FIX_EDGE(p_jobid_in,
                                                  p_topo_out,
                                                  'MERGE', --log type
                                                  'N', --hold univeral who cares now. Prob no diff in the underlying code
                                                   merge_parms.gen_merge_tolerance,
                                                   NULL, --loop as long as progress is being made
                                                   p_fix_2edge); --expensive check and fix on edge pairs.  Should be N here

            IF edgefix_val = '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                      'GZ_FIX_EDGE success');

            ELSIF edgefix_val = '1'
            AND p_topofix_qa = 'Y'
            THEN

               --SOP when we have a failure in merge.  Want to fail and investigate
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                      'GZ_FIX_EDGE not successful, we will fail the merge job to be safe');


               output := output || '|Failed to fix all edges using gz_topofix.gz_fix_edge';

            ELSIF edgefix_val = '1'
            AND p_topofix_qa = 'N'
            THEN

               --Overriding the failure based on gz_merge_setup.topofix_qa
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                      'GZ_FIX_EDGE not successful, but we we wont fail the merge job ' ||
                                                      'because topofix_qa is N');

            ELSE

                RAISE_APPLICATION_ERROR(-20001,'Unknown edgefix result');

            END IF;


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Not calling gz_fix_edge on all of ' || p_topo_out,
                                                    NULL,NULL,NULL,NULL,NULL,NULL,NULL);

         END IF;

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Calc ' || merge_face_tab || ' geometry ');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------


         --no allowance to change SRIDs here

         --loop over each source topo to try to avoid 24 hour hang up
         --not efficient on small jobs, but avocado son

         FOR i IN 1 .. galactic_topos.COUNT
         LOOP

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Calc geometry ' || galactic_topos(i),
                                                    NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(merge_face_tab,
                                                  'FACE_ID',
                                                  'SDOGEOMETRY',
                                                  'ALL',  --we did NULL our successes but this isnt our game
                                                  merge_parms.gen_merge_tolerance,
                                                  NULL, --TO SRID is null, no translate
                                                  'SOURCE_TOPO',
                                                  galactic_topos(i));


            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Check for NULL or bad gtype geometries in ' || merge_face_tab );
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            --this is still in the loop over topos
            --I guess thats ok

            nullgeompsql := 'SELECT a.meaningless_id '
                         || 'FROM ' || merge_face_tab || ' a '
                         || 'WHERE '
                         || 'a.source_topo = :p1 AND '
                         || '( a.sdogeometry IS NULL '
                         || '  OR (a.sdogeometry.sdo_gtype != :p1 AND a.sdogeometry.sdo_gtype != :p2) ) ';

            EXECUTE IMMEDIATE nullgeompsql BULK COLLECT INTO badfaces USING galactic_topos(i),
                                                                            2003,
                                                                            2007;

            IF badfaces.COUNT > 0
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                      'Got ' || badfaces.COUNT || ' NULL sdogeometry or bad gtypes in ' || galactic_topos(i),
                                                       NULL,NULL,NULL,nullgeompsql,NULL,NULL,NULL);

               --update these to QC 2. Auto fixer doesnt handle

               psql := 'UPDATE ' || merge_face_tab || ' a '
                    || 'SET '
                    || 'a.qc = :p1 '
                    || 'WHERE a.meaningless_id IN (SELECT * FROM TABLE(:p2)) ';

               EXECUTE IMMEDIATE psql USING '2',
                                             GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(badfaces);

               output := output || '|From source topo ' || galactic_topos(i) || 'we have ' || badfaces.COUNT || ' NULL or wrong gtype sdogeometries ';

               badfaces.DELETE;

            ELSE

               ----------------------------------------------------------------------------------
               --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
               DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
               DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Clean up ' || merge_face_tab || ' sdogeometry ');
               --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
               ----------------------------------------------------------------------------------

               --This just makes for a reasonable (ahem hard coded) precision

               psql := 'UPDATE ' || merge_face_tab || ' a '
                    || 'SET '
                    || 'a.sdogeometry =  GZ_GEOM_UTILS.ORDINATE_ROUNDER(a.sdogeometry, :p1 ) '
                    || 'WHERE '
                    || 'a.source_topo = :p2 ';

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                      'Rolling sdogeometry precision back to 8 digits ' || galactic_topos(i),
                                                      NULL,NULL,NULL,psql,NULL,NULL,NULL);


               EXECUTE IMMEDIATE psql USING 8,  --parameterize buddy. Switch to 7 1/4/12. Now 8! Lesson though is do this before validation
                                            galactic_topos(i);

               COMMIT;

            END IF;



         END LOOP; --end loop over topos

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Index ' || merge_face_tab || ' sdogeometry');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         --seems polite to do this no matter what
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                'Index ' || merge_face_tab || ' sdogeometry ',
                                                NULL,NULL,NULL,NULL,NULL,NULL,NULL);

         --this is actually a drop and rebuild
         GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(merge_face_tab,
                                        'SDOGEOMETRY',
                                        merge_parms.gen_merge_srid,
                                        merge_parms.gen_merge_tolerance);

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Call topofix on ' || merge_face_tab);
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                'Calling topofix on  ' || merge_face_tab,
                                                NULL,NULL,NULL,NULL,NULL,NULL,NULL);

         --will update QC for all that it can't fix
         --and update sdo for those it does fix
         --Returns '0' for success, '1' for something bad
         --QC 1 is 13349, 13356 or some other validate_Geometry_with_context error
         --QC 2 is NULL sdogeometry
         --QC 3 is non-2003 gtype

          fix_val := GZ_TOPOFIX.CHECK_FACE_TAB(p_jobid_in,
                                               p_topo_out,
                                               merge_face_tab,
                                               'FACE_ID',
                                               'MERGE',
                                               merge_parms.gen_merge_tolerance,
                                               merge_parms.gen_merge_srid);

         IF fix_val <> '0'
         AND p_topofix_qa = 'Y'
         THEN

            --SOP when a failure, we want to investigate any problems at merge before building final shapes and shapefiles
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'We cant get a valid geometry in ' || merge_face_tab || ' for some faces',
                                                       NULL,NULL,NULL,NULL,NULL,NULL,NULL);


            output := output || '| We have some invalid face sdogeometries ';

         ELSIF fix_val <> '0'
         AND p_topofix_qa = 'N'
         THEN

            --Overriding the failure via gz_merge_setup.topofix_qa
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'We cant get a valid geometry in ' || merge_face_tab || ' for some faces ' ||
                                                   'but wont fail the job since topofix QA is N',
                                                    NULL,NULL,NULL,NULL,NULL,NULL,NULL);


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                    'Good: We have a valid geometry in ' || merge_face_tab || ' for all of our faces',
                                                     NULL,NULL,NULL,badgeompsql,NULL,NULL,NULL);

         END IF;

      END IF; --BIG IF on if we have sdogeometry


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Calc ' || merge_face_tab || ' MBR and and index it');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --FROM HERE on we calc all fields if we are good

      FOR i IN 1 .. galactic_topos.COUNT
      LOOP

         IF measurehash.EXISTS('MBR')
         AND measurehash.EXISTS('SDOGEOMETRY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Calc MBR and index it ' || galactic_topos(i),
                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);



            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(merge_face_tab,
                                                  'FACE_ID',
                                                  'MBR',
                                                  'ALL',
                                                   merge_parms.gen_merge_tolerance,
                                                   NULL,
                                                   'SOURCE_TOPO',
                                                   galactic_topos(i));

           --Does anyone give a fudge if the MBRs are invalid?

            badgeompsql := 'select a.meaningless_id '
                        || 'FROM ' || merge_face_tab || ' a '
                        || 'WHERE SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.mbr, :p1) != :p2 AND '
                        || 'a.source_topo = :p2 ';

            EXECUTE IMMEDIATE badgeompsql BULK COLLECT INTO badfaces USING merge_parms.gen_merge_tolerance,
                                                                           'TRUE',
                                                                           galactic_topos(i);

            IF badfaces.COUNT > 0
            THEN

               --nobody gives a pudding
               --just log and update QC val of 3

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                      'We cant get a valid geometry in ' || galactic_topos(i) || ' for ' || badfaces.COUNT || 'of our MBRs',
                                                      NULL,NULL,NULL,badgeompsql,NULL,NULL,NULL);

                psql := 'UPDATE ' || merge_face_tab || ' a '
                     || 'SET '
                     || 'a.qc = :p1 '
                     || 'WHERE a.meaningless_id IN (SELECT * FROM TABLE(:p2)) AND '
                     || 'a.qc IS NULL ';

                EXECUTE IMMEDIATE psql USING '4',
                                              GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(badfaces);

                COMMIT;

                badfaces.DELETE;

            END IF;


         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 70');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Calc ' || merge_face_tab || ' areatotal');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         IF measurehash.EXISTS('AREATOTAL')
         AND measurehash.EXISTS('SDOGEOMETRY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Calc areatotal ' || galactic_topos(i),
                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(merge_face_tab,
                                                  'FACE_ID',
                                                  'AREATOTAL',
                                                  'ALL',
                                                   merge_parms.gen_merge_tolerance,
                                                   NULL,
                                                   'SOURCE_TOPO',
                                                   galactic_topos(i));

            --Sometimes invalid geometries that are very small slivers make areas < 0
            --utility handles this

         END IF;

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 80');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Calc ' || merge_face_tab || ' perimeter');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         IF measurehash.EXISTS('PERIMETER')
         AND measurehash.EXISTS('SDOGEOMETRY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Calc perimeter ' || galactic_topos(i),
                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            -- 'unit=meter' is hard coded into utility  !

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(merge_face_tab,
                                                  'FACE_ID',
                                                  'PERIMETER',
                                                  'ALL',
                                                   merge_parms.gen_merge_tolerance,
                                                   NULL,
                                                   'SOURCE_TOPO',
                                                   galactic_topos(i));


         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 90');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Calc ' || merge_face_tab || ' pa_ratio');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         IF measurehash.EXISTS('PA_RATIO')
         AND measurehash.EXISTS('SDOGEOMETRY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Calc pa_ratio ' || galactic_topos(i),
                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);


            --DECODEs when area is 0 to avoid divide by 0
            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(merge_face_Tab,
                                                  'FACE_ID',
                                                  'PA_RATIO',
                                                  'ALL',
                                                   merge_parms.gen_merge_tolerance,
                                                   NULL,
                                                   'SOURCE_TOPO',
                                                   galactic_topos(i));

         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 100');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Calc ' || merge_face_tab || ' LLX');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------


         IF measurehash.EXISTS('MBR')
         AND measurehash.EXISTS('SDOGEOMETRY')
         AND measurehash.EXISTS('LLX')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Calc llX ' || galactic_topos(i),
                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(merge_face_tab,
                                                  'FACE_ID',
                                                  'LLX',
                                                  'ALL',
                                                   merge_parms.gen_merge_tolerance,
                                                   NULL,
                                                   'SOURCE_TOPO',
                                                   galactic_topos(i));

         END IF;

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 110');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Calc ' || merge_face_tab || ' LLY');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         IF measurehash.EXISTS('MBR')
         AND measurehash.EXISTS('SDOGEOMETRY')
         AND measurehash.EXISTS('LLY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Calc lly ' || galactic_topos(i),
                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(merge_face_tab,
                                                  'FACE_ID',
                                                  'LLY',
                                                  'ALL',
                                                   merge_parms.gen_merge_tolerance,
                                                   NULL,
                                                   'SOURCE_TOPO',
                                                   galactic_topos(i));

         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 120');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Calc ' || merge_face_tab || ' URX');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         IF measurehash.EXISTS('MBR')
         AND measurehash.EXISTS('SDOGEOMETRY')
         AND measurehash.EXISTS('URX')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Calc urx ' || galactic_topos(i),
                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(merge_face_tab,
                                                  'FACE_ID',
                                                  'URX',
                                                  'ALL',
                                                   merge_parms.gen_merge_tolerance,
                                                   NULL,
                                                   'SOURCE_TOPO',
                                                   galactic_topos(i));

         END IF;

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 120');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Calc ' || merge_face_tab || ' URY');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         IF measurehash.EXISTS('MBR')
         AND measurehash.EXISTS('SDOGEOMETRY')
         AND measurehash.EXISTS('URY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Calc ury ' || galactic_topos(i),
                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(merge_face_tab,
                                                  'FACE_ID',
                                                  'URY',
                                                  'ALL',
                                                   merge_parms.gen_merge_tolerance,
                                                   NULL,
                                                   'SOURCE_TOPO',
                                                   galactic_topos(i));

         END IF;

      END LOOP;  --end loop over topos calcing secondary measurements


      --index the MBR
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                             'Calc sidx on ' || merge_face_tab || ' MBR ',
                                             NULL,NULL,NULL,NULL,NULL,NULL,NULL);

      GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(merge_face_tab,
                                     'MBR',
                                     merge_parms.gen_merge_srid,
                                     merge_parms.gen_merge_tolerance,
                                     NULL,
                                     NULL,
                                     merge_face_tab || '_MIDX');



      --what am I forgetting?


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END POPULATE_MEASUREMENTS_ETC;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GENERALIZATION_TOPO_MERGE (
      p_schema             IN VARCHAR2,
      p_gz_jobid           IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_face_out           IN VARCHAR2,
      p_modules            IN VARCHAR2 DEFAULT 'YYYYYYYYYY',
      p_restart_flag       IN VARCHAR2 DEFAULT 'N',
      p_validate_topo      IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge           IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge          IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa         IN VARCHAR2 DEFAULT 'Y' 
   ) RETURN VARCHAR2
   AS

      --Matt! 5/25/11
      --Matt! 6/10/13 added topofix_qa option
      --M! 12/30/13 More rearranging of the topofix and validate topo deck chairs

      psql              VARCHAR2(4000);
      retval            VARCHAR2(4000) := '1';  --set to fail, must set to pass in modules
      tidy_retval       VARCHAR2(4000);
      stack             VARCHAR2(4000);
      errm              VARCHAR2(8000) := 'ERROR:'; --default for line 1 in log error message, if no SQLERRM
      p_top_layer_tab   VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      topofix_qa        VARCHAR2(1);


   BEGIN

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Let the logging begin');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      BEGIN

         --get input parms
         merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id_in);
         --these were input parms but switched to parameter table values
         --just switch them out here to be lazy
         p_top_layer_tab := merge_parms.gen_merge_top_layer;
         --back to input
         --p_face_out      := merge_parms.gen_merge_face_out;

      EXCEPTION
      WHEN OTHERS
      THEN
          --send special exception to generic handler at the end
          RAISE_APPLICATION_ERROR(-20002,'Failed before we could even create a log.  Check the basics, like schema name');
      END;


      IF p_restart_flag = 'N'
      THEN

         BEGIN

            --We are starting from the beginning for this job
            GZ_TOPO_MERGE.START_MERGE_LOGGING(p_schema,
                                              p_project_id_in,
                                              p_topo_out,
                                              p_gz_jobid);

         EXCEPTION
         WHEN OTHERS
         THEN
            --send special exception to generic handler at the end
            RAISE_APPLICATION_ERROR(-20002,'Failed before we could even create a log.  Check the basics, like schema name');
         END;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'SET_UP',NULL,
                                                'Inputs are (' || p_schema || ',' || p_gz_jobid || ',' || p_release || ','
                                                || p_project_id_in || ',' || p_topo_in_table || ','
                                                || p_topo_out || ',' || p_face_out || ',' || p_modules || ','
                                                ||  p_restart_flag || ',' || p_validate_topo || ','
                                                || p_fix_edge || ',' || p_fix_2edge || ','
                                                || p_topofix_qa || ')');


         --make work tables
         --need to give this more thought later

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'SET_UP',NULL,
                                                     'Starting table set up for ' || p_gz_jobid,
                                                     NULL,NULL,NULL,NULL,NULL,NULL );
                                                     
         GZ_TOPO_MERGE.SET_UP(p_schema,
                              p_release,
                              p_project_id_in,
                              p_topo_out,
                              p_face_out);

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'SET_UP',NULL,
                                                 'Finished table set up for ' || p_gz_jobid,
                                                 NULL,NULL,NULL,NULL,NULL,NULL );

      END IF;

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Verify and collect merge inputs');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      retval := GZ_TOPO_MERGE.VERIFY_MERGE_INPUTS(p_schema,
                                                  p_release,
                                                  p_project_id_in,
                                                  p_gz_jobid,
                                                  p_topo_in_table,
                                                  p_topo_out,
                                                  p_top_layer_tab);

      IF retval != '0'
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'VERIFY_MERGE_INPUTS',NULL,
                                                'UNRECOVERABLE ERROR: Problem in verify merge_inputs: ' || retval,
                                                 NULL,NULL,NULL,NULL,NULL,substr(retval , 1, 4000) );

         --kick it down to the generic error handler
         RAISE_APPLICATION_ERROR(-20001,'Problem with merge inputs: ' || retval);

      END IF;

      IF p_topofix_qa IS NULL
      OR p_topofix_qa = 'Y'
      THEN

         --Default is to fail for QA when faces or edges are invalid after the merge
         --we are too close to final shapefiles, last chance
         topofix_qa := 'Y';

      ELSE

         topofix_qa := 'N';

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Create and assess cookie cutters ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF substr(p_modules,1,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_TOPO_MERGE.CREATE_COOKIE_CUTTERS(p_schema,
                                                          p_release,
                                                          p_project_id_in,
                                                          p_gz_jobid,
                                                          p_topo_in_table,
                                                          p_top_layer_tab,
                                                          p_topo_out);

         EXCEPTION
         WHEN OTHERS
            THEN

               IF SQLCODE = -01013
               THEN
                  --ORA-01013: user requested cancel of current operation
                  RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
               ELSE
                  errm := SQLERRM;
                  stack := DBMS_UTILITY.format_error_backtrace;
               END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

               --currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);
            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_COOKIE_CUTTERS',NULL,
                                                   'Complete for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

            --GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '1');

         END IF;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Align cutters ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF substr(p_modules,2,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_TOPO_MERGE.ALIGN_COOKIE_CUTTERS(p_schema,
                                                         p_release,
                                                         p_project_id_in,
                                                         p_gz_jobid,
                                                         p_topo_in_table,
                                                         p_top_layer_tab,
                                                         p_topo_out);

         EXCEPTION
         WHEN OTHERS
            THEN

               IF SQLCODE = -01013
               THEN
                  --ORA-01013: user requested cancel of current operation
                  RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
               ELSE
                  errm := SQLERRM;
                  stack := DBMS_UTILITY.format_error_backtrace;
               END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_COOKIE_CUTTERS',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

               --currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);
            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_COOKIE_CUTTERS',NULL,
                                                   'Complete for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

            --GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '1');

         END IF;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Create topology ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF substr(p_modules,3,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_TOPO_MERGE.CREATE_EMPTY_TOPOLOGY(p_schema,
                                                          p_release,
                                                          p_project_id_in,
                                                          p_gz_jobid,
                                                          p_topo_out);

         EXCEPTION
         WHEN OTHERS
            THEN

               IF SQLCODE = -01013
               THEN
                  --ORA-01013: user requested cancel of current operation
                  RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
               ELSE
                  errm := SQLERRM;
                  stack := DBMS_UTILITY.format_error_backtrace;
               END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_EMPTY_TOPOLOGY',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'CREATE_EMPTY_TOPOLOGY',NULL,
                                                   'Complete for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Build Cutter Topology ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF substr(p_modules,4,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_TOPO_MERGE.BUILD_CUTTER_TOPO(p_schema,
                                                      p_release,
                                                      p_project_id_in,
                                                      p_gz_jobid,
                                                      p_topo_in_table,
                                                      p_top_layer_tab,
                                                      p_topo_out);

         EXCEPTION
         WHEN OTHERS
            THEN

               IF SQLCODE = -01013
               THEN
                  --ORA-01013: user requested cancel of current operation
                  RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
               ELSE
                  errm := SQLERRM;
                  stack := DBMS_UTILITY.format_error_backtrace;
               END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_CUTTER_TOPO',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_CUTTER_TOPO',NULL,
                                                   'Complete for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );


         END IF;

      END IF;



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 70');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Assess Cutter Topology ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF substr(p_modules,5,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_TOPO_MERGE.ASSESS_CUTTER_TOPO(p_schema,
                                                       p_release,
                                                       p_project_id_in,
                                                       p_gz_jobid,
                                                       p_topo_in_table,
                                                       p_top_layer_tab,
                                                       p_topo_out);

         EXCEPTION
         WHEN OTHERS
            THEN

               IF SQLCODE = -01013
               THEN
                  --ORA-01013: user requested cancel of current operation
                  RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
               ELSE
                  errm := SQLERRM;
                  stack := DBMS_UTILITY.format_error_backtrace;
               END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_CUTTER_TOPO',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_CUTTER_TOPO',NULL,
                                                   'Complete for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );


         END IF;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 80');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Align Faces ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF substr(p_modules,6,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_TOPO_MERGE.ALIGN_FACES(p_schema,
                                                p_release,
                                                p_project_id_in,
                                                p_gz_jobid,
                                                p_topo_in_table,
                                                p_top_layer_tab,
                                                p_topo_out,
                                                p_face_out);

         EXCEPTION
         WHEN OTHERS
            THEN

               IF SQLCODE = -01013
               THEN
                  --ORA-01013: user requested cancel of current operation
                  RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
               ELSE
                  errm := SQLERRM;
                  stack := DBMS_UTILITY.format_error_backtrace;
               END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_FACES',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ALIGN_FACES',NULL,
                                                   'Complete for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );


         END IF;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 90');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Build Face Topology ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF substr(p_modules,7,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_TOPO_MERGE.BUILD_FACE_TOPO(p_schema,
                                                    p_release,
                                                    p_project_id_in,
                                                    p_gz_jobid,
                                                    p_topo_in_table,
                                                    p_top_layer_tab,
                                                    p_topo_out,
                                                    p_face_out,
                                                    'Y',   --add poly method
                                                    p_restart_flag);   --first module supported for real restart

         EXCEPTION
         WHEN OTHERS
            THEN

               IF SQLCODE = -01013
               THEN
                  --ORA-01013: user requested cancel of current operation
                  RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
               ELSE
                  errm := SQLERRM;
                  stack := DBMS_UTILITY.format_error_backtrace;
               END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'BUILD_FACE_TOPO',NULL,
                                                   'Complete for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 100');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Assess Face Topology ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF substr(p_modules,8,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_TOPO_MERGE.ASSESS_FACE_TOPO(p_schema,
                                                     p_release,
                                                     p_project_id_in,
                                                     p_gz_jobid,
                                                     p_topo_in_table,
                                                     p_top_layer_tab,
                                                     p_topo_out,
                                                     p_face_out);

         EXCEPTION
         WHEN OTHERS
            THEN

               IF SQLCODE = -01013
               THEN
                  --ORA-01013: user requested cancel of current operation
                  RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
               ELSE
                  errm := SQLERRM;
                  stack := DBMS_UTILITY.format_error_backtrace;
               END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'ASSESS_FACE_TOPO',NULL,
                                                   'Complete for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );


         END IF;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 110');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Transfer Face Attributes ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF substr(p_modules,9,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_TOPO_MERGE.TRANSFER_FACE_ATTRIBUTES(p_schema,
                                                             p_release,
                                                             p_project_id_in,
                                                             p_gz_jobid,
                                                             p_topo_in_table,
                                                             p_top_layer_tab,
                                                             p_topo_out,
                                                             p_face_out);

         EXCEPTION
         WHEN OTHERS
            THEN

               IF SQLCODE = -01013
               THEN
                  --ORA-01013: user requested cancel of current operation
                  RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
               ELSE
                  errm := SQLERRM;
                  stack := DBMS_UTILITY.format_error_backtrace;
               END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TRANSFER_FACE_ATTRIBUTES',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TRANSFER_FACE_ATTRIBUTES',NULL,
                                                   'Complete for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );


         END IF;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 120');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_MERGE: Calculate Measurements ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF substr(p_modules,10,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_TOPO_MERGE.POPULATE_MEASUREMENTS_ETC(p_schema,
                                                              p_release,
                                                              p_project_id_in,
                                                              p_gz_jobid,
                                                              p_topo_in_table,
                                                              p_top_layer_tab,
                                                              p_topo_out,
                                                              p_face_out,
                                                              p_validate_topo,
                                                              p_fix_edge,
                                                              p_fix_2edge,
                                                              topofix_qa);

         EXCEPTION
         WHEN OTHERS
            THEN

               IF SQLCODE = -01013
               THEN
                  --ORA-01013: user requested cancel of current operation
                  RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
               ELSE
                  errm := SQLERRM;
                  stack := DBMS_UTILITY.format_error_backtrace;
               END IF;

         END;


         IF retval != '0'
         THEN

            --almost always invalid face sdogeometry gets us right here
            --at least until automated fixer gets incorporated

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                        'UNRECOVERABLE ERROR: Ending processing for ' || p_gz_jobid,
                                                        NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'POPULATE_MEASUREMENTS_ETC',NULL,
                                                   'Complete for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );


         END IF;

      END IF;


      --Cleanliness is
      --If we get here we are in at least decent shape, return bad codes if major failures above
      --should not return total success however if we ditched a couple of source topos along the way

      BEGIN

         --assume fail
         tidy_retval := '1';

         tidy_retval := GZ_TOPO_MERGE.TIDY_EXIT(p_schema,
                                                p_release,
                                                p_project_id_in,
                                                p_gz_jobid,
                                                p_topo_in_table,
                                                p_top_layer_tab,
                                                p_topo_out,
                                                p_face_out,
                                                p_restart_flag);

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -01013
         THEN
            --ORA-01013: user requested cancel of current operation
            RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
         ELSE
            errm := SQLERRM;
            stack := DBMS_UTILITY.format_error_backtrace;

            --NB: Different here
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'TIDY_EXIT',NULL,
                                                   'Weird: Tidy Exit UNRECOVERABLE ERROR: Ending processing for ' || p_gz_jobid,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || tidy_retval || chr(10) || stack , 1, 4000) );
         END IF;

      END;


      IF tidy_retval != '0'
      THEN

         --return the exit handler code
         --nothing else to do here just yet
         RETURN tidy_retval;

      ELSE

         RETURN tidy_retval;

      END IF;


   --This is the general unhandled exception area
   --usually a totally flubbed up input near the very beginning of the job

   EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -20002  --my made up code
         THEN

            errm := SQLERRM || DBMS_UTILITY.format_error_backtrace;
            RAISE_APPLICATION_ERROR(-20001, 'Doh! Unhandled exception.  We dont even have a log to write to. ' || errm);

         ELSE

            -- Word up to "good practice"

            errm := SQLERRM || DBMS_UTILITY.format_error_backtrace;
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'EXCEPTION HANDLER',NULL,
                                                   'UNRECOVERABLE ERROR: Topo merge caught this exception and has no clue ',
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm, 1, 4000) );

            RETURN 1;


         END IF;



   END GENERALIZATION_TOPO_MERGE;

   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION MAX_GAP_EVALUATION_2 (
      p_topo_out           IN VARCHAR2,
      p_tab                IN VARCHAR2,
      p_step               IN VARCHAR2,
      p_work_tab           IN VARCHAR2,
      p_sdo_col            IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_null_tol           IN NUMBER DEFAULT .0000005,
      p_sample_count       IN NUMBER DEFAULT 100,
      p_gz_union           IN VARCHAR2 DEFAULT 'N'
   ) RETURN NUMBER
   AS

      --Matt! 6/7/11
      --Replaces problematic MAX_GAP_EVALUATION
      --12/27/11 Fiddle with tolerance, no longer user tol * tol in the straight oracle calls
      --         Hopped up tolerance was mysteriously producing giant incorrect gaps
      --6/25/13  Added gz_union option in case bugs proliferate

      --Goal is to be total troglodytes
      --No fancy sdo_aggr_union, just union each piece with nieghbors
      --Hopefully will take us through the same Oracle code that does sdo_difference in align_edges

      --Takes an input table with geometries expected to be contiguous and continuous, mas o menos
      --Aggregates input with neighbors and measures the largest inner ring in the result polygon
      --If input is geodetic with .05 tolerance limitation then the SRID should be NULLed by the caller

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      work_geom         SDO_GEOMETRY;
      hole_geom         SDO_GEOMETRY;
      result            VARCHAR2(4000);
      ring              SDO_GEOMETRY;
      width             NUMBER;
      max_width         NUMBER := 0;
      min_width         NUMBER;
      all_outlines      GZ_TYPES.stringarray;
      buddy_outlines    GZ_TYPES.stringarray;
      second_cousins    GZ_TYPES.stringarray;
      third_cousins     GZ_TYPES.stringarray;
      second_geom       SDO_GEOMETRY;
      third_geom        SDO_GEOMETRY;
      max_step          VARCHAR2(4000);
      hull_geom         SDO_GEOMETRY;

   BEGIN


      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_GAP_EVALUATION: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                              'Starting', NULL,NULL,NULL,NULL,NULL,NULL,NULL);



      --validate input w null SRID
      psql := 'SELECT count(*) '
           || 'FROM '
           || p_tab || ' a '
           || 'WHERE '
           || 'sdo_geom.validate_geometry_with_context(a.' || p_sdo_col || ', :p1) != :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_null_tol,
                                              'TRUE';

      IF kount != 0
      THEN

          GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                                'Yo, ' || p_tab || ' has invalid sdogeometry at tolerance ' || p_null_tol,
                                                 NULL,NULL,NULL,psql,NULL,NULL,NULL);

          RAISE_APPLICATION_ERROR(-20001,'Yo, ' || p_tab || ' has invalid sdogeometry at tolerance ' || p_null_tol );

      END IF;

      --need SIDX for this guy. Should be on unaligned_sdo column

      GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(p_tab,
                                     p_sdo_col,
                                     NULL,
                                     p_null_tol);


      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_GAP_EVALUATION: Cycle through each piece ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      psql := 'SELECT a.source_topo '
           || 'FROM ' || p_tab || ' a ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO all_outlines;


      FOR i IN 1 .. all_outlines.COUNT
      LOOP

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                               'Starting ' || all_outlines(i), NULL,NULL,NULL,NULL,NULL,NULL,NULL);

         --get buddies

         --F it, I don't even care if we do this work 2x
         --any way we can merge and get a gap, A:B or B:A I want to know about
         --I know it shouldnt matter, but who knows

         psql := 'select a.source_topo '
              || 'FROM '
              || p_tab || ' a, '
              || p_tab || ' b '
              || 'WHERE '
              || 'SDO_RELATE(a.' || p_sdo_col || ', b.' || p_sdo_col || ', :p1) = :p2 AND '
              || 'a.source_topo != :p3 AND '
              || 'b.source_topo = :p4 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO buddy_outlines USING 'mask=ANYINTERACT',
                                                                       'TRUE',
                                                                       all_outlines(i),
                                                                       all_outlines(i);

         --loop through buddies, if any

         FOR j IN 1 .. buddy_outlines.COUNT
         LOOP


            IF j = 1
            THEN

               IF p_gz_union = 'N'
               THEN

                  psql := 'SELECT SDO_GEOM.SDO_UNION(a.' || p_sdo_col || ', b.' || p_sdo_col || ', :p1) ';

               ELSE

                  psql := 'SELECT GZ_GEOM_UTILS.GZ_UNION(a.' || p_sdo_col || ', b.' || p_sdo_col || ', :p1) ';

               END IF;

               psql := psql || 'FROM '
                       || p_tab || ' a, '
                       || p_tab || ' b '
                       || 'WHERE '
                       || 'a.source_topo = :p1 AND '
                       || 'b.source_topo = :p2 ';




               EXECUTE IMMEDIATE psql INTO work_geom USING p_null_tol,
                                                           all_outlines(i),
                                                           buddy_outlines(j);

            ELSE

               IF p_gz_union = 'N'
               THEN

                  psql := 'SELECT SDO_GEOM.SDO_UNION(a.' || p_sdo_col || ', :p1, :p2) ';

               ELSE

                  psql := 'SELECT GZ_GEOM_UTILS.GZ_UNION(a.' || p_sdo_col || ', :p1, :p2) ';

               END IF;

               psql := psql || 'FROM '
                    || p_tab || ' a '
                    || 'WHERE '
                    || 'a.source_topo = :p3 ';

               EXECUTE IMMEDIATE psql INTO work_geom USING work_geom,
                                                           p_null_tol,
                                                           buddy_outlines(j);

            END IF;

         END LOOP;



         --Next section handles 3 cases using the blob we have at this point
         --1. Another poly that is fully enclosed by the resulting blob but doesnt touch the seed all_outline
         --   This is DC.  2003 or 2007 + poly INSIDE hull + poly touch blob
         --   Unfortunately we also get added some extra that are inside the convex hull and touch but arent really holes
         --2. A piece of another poly where the piece is fully enclosed by the resulsting blob
         --   This is KY. 2007 + poly OVERLAPBDYINTERSECT hull + piece touch blob
         --3. A piece of another poly where the piece is fully enclosed by the resulsting blob but the hull no touch the poly
         --   This is an unseen case with states. 2007 + poly OVERLAPBDYDISJOINT hull + piece touch blob

         IF buddy_outlines.COUNT > 0
         THEN

            --1. Another poly that is fully enclosed by the resulting blob but doesnt touch the seed all_outline

            --There should really be multiple passes here, but this works for our current state outlines.
            --I should be fired...

            hull_geom := SDO_GEOM.SDO_CONVEXHULL(work_geom,p_null_tol);

            psql := 'select a.source_topo '
                 || 'FROM '
                 || p_tab || ' a '
                 || 'WHERE '
                 || 'SDO_RELATE(a.' || p_sdo_col || ', :p1, :p2) = :p3 AND '
                 || 'a.source_topo NOT IN (SELECT * FROM TABLE(:p4)) AND '
                 || 'a.source_topo != :p5 ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO second_cousins USING hull_geom,
                                                                          'mask=INSIDE',
                                                                          'TRUE',
                                                                          GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(buddy_outlines),
                                                                          all_outlines(i);

            IF second_cousins.COUNT > 0
            THEN

               FOR j IN 1 .. second_cousins.COUNT
               LOOP

                  psql := 'SELECT a.' || p_sdo_col || ' '
                       || 'FROM '
                       || p_tab || ' a '
                       || 'WHERE '
                       || 'a.source_topo = :p1 ';

                  EXECUTE IMMEDIATE psql INTO second_geom USING second_cousins(j);



                  IF second_geom.sdo_gtype = 2003
                  THEN

                     IF SDO_GEOM.RELATE(work_geom, 'mask=ANYINTERACT', second_geom, p_null_tol) != 'FALSE'
                     THEN

                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                                               'Adding second cousin ' ||  second_cousins(j) || ' to ' || all_outlines(i),
                                                               NULL,NULL,NULL,NULL,NULL,NULL,NULL);

                        IF p_gz_union = 'N'
                        THEN

                           work_geom := SDO_GEOM.SDO_UNION(work_geom,
                                                           second_geom,
                                                           p_null_tol);

                        ELSE

                           work_geom := GZ_GEOM_UTILS.GZ_UNION(work_geom,
                                                               second_geom,
                                                               p_null_tol);
                        END IF;

                     END IF;

                  ELSIF second_geom.sdo_gtype = 2007
                  THEN

                     FOR k IN 1 .. SDO_UTIL.GETNUMELEM(second_geom)
                     LOOP

                        --only add if it touches the actual blob, save one millisecond
                        --and avoid adding in lots of islands and junk
                        IF SDO_GEOM.RELATE(work_geom, 'mask=ANYINTERACT', SDO_UTIL.EXTRACT(second_geom,k), p_null_tol) != 'FALSE'
                        THEN

                           GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                                                  'Adding piece of second cousin ' ||  second_cousins(j) || ' to ' || all_outlines(i),
                                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);

                           IF p_gz_union = 'N'
                           THEN

                              work_geom := SDO_GEOM.SDO_UNION(work_geom,
                                                              SDO_UTIL.EXTRACT(second_geom,k),
                                                              p_null_tol);

                           ELSE

                              work_geom := GZ_GEOM_UTILS.GZ_UNION(work_geom,
                                                                  SDO_UTIL.EXTRACT(second_geom,k),
                                                                  p_null_tol);


                           END IF;

                        END IF;

                     END LOOP;

                  ELSE

                     RAISE_APPLICATION_ERROR(-20001, 'weird gtype ' || second_geom.sdo_gtype);

                  END IF;

               END LOOP;

            END IF;


             --2. A piece of another poly where the piece is fully enclosed by the resulsting blob
             --3. A piece of another poly where the piece is fully enclosed by the resulting blob but the hull no touch the poly


            --What follows is the basically the same code as above, except for the 2007 requirement and an additional relate
            --also it allows for the INSIDE second cousins to rework the convex hull for the third cousins

            hull_geom := SDO_GEOM.SDO_CONVEXHULL(work_geom,p_null_tol);

            psql := 'select a.source_topo '
                 || 'FROM '
                 || p_tab || ' a '
                 || 'WHERE '
                 || 'SDO_RELATE(a.' || p_sdo_col || ', :p1, :p2) = :p3 AND '
                 || 'a.source_topo NOT IN (SELECT * FROM TABLE(:p4)) AND '
                 || 'a.source_topo NOT IN (SELECT * FROM TABLE(:p5)) AND '
                 || 'a.source_topo != :p6 AND '
                 || 'a.' || p_sdo_col || '.sdo_gtype = :p7 ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO third_cousins USING hull_geom,
                                                                         'mask=OVERLAPBDYINTERSECT+OVERLAPBDYDISJOINT',
                                                                          'TRUE',
                                                                          GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(buddy_outlines),
                                                                          GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(second_cousins),
                                                                          all_outlines(i),
                                                                          2007;

            --third cousins: Not fully inside the hull
            --but a piece of them IS fully inside, and touches the blob
            --piece req means 2007

            IF third_cousins.COUNT > 0
            THEN

               FOR j IN 1 .. third_cousins.COUNT
               LOOP


                  psql := 'SELECT a.' || p_sdo_col || ' '
                       || 'FROM '
                       || p_tab || ' a '
                       || 'WHERE '
                       || 'a.source_topo = :p1 ';

                  EXECUTE IMMEDIATE psql INTO third_geom USING third_cousins(j);

                  FOR k IN 1 .. SDO_UTIL.GETNUMELEM(third_geom)
                  LOOP

                     IF SDO_GEOM.RELATE(SDO_UTIL.EXTRACT(third_geom,k), 'mask=INSIDE', hull_geom, p_null_tol) != 'FALSE'
                     AND SDO_GEOM.RELATE(work_geom, 'mask=ANYINTERACT', SDO_UTIL.EXTRACT(third_geom,k), p_null_tol) != 'FALSE'
                     THEN

                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                                               'Adding piece of third cousin ' || third_cousins(j) || ' to ' || all_outlines(i),
                                                               NULL,NULL,NULL,NULL,NULL,NULL,NULL);


                        --piece is entirely inside hull and it touches, add it

                        IF p_gz_union = 'N'
                        THEN

                           work_geom := SDO_GEOM.SDO_UNION(work_geom,
                                                           SDO_UTIL.EXTRACT(third_geom,k),
                                                           p_null_tol);

                        ELSE

                           work_geom := GZ_GEOM_UTILS.GZ_UNION(work_geom,
                                                               SDO_UTIL.EXTRACT(third_geom,k),
                                                               p_null_tol);

                        END IF;


                     END IF;


                  END LOOP;


               END LOOP;

            END IF;


         END IF;


         IF buddy_outlines.COUNT > 0
         THEN

            --now have the blob for one state and its neighbors

            --save result
            psql := 'DELETE FROM ' || p_work_tab ||  ' a '
                 || 'WHERE a.step = :p1 ';

            EXECUTE IMMEDIATE psql USING p_step || '.' || all_outlines(i) || '.AGGR_GEOM';

            psql := 'INSERT INTO ' || p_work_tab || ' a '
                 || 'VALUES(:p1,:p2)';

            EXECUTE IMMEDIATE psql USING p_step || '.' || all_outlines(i) || '.AGGR_GEOM',
                                         work_geom;

            COMMIT;


            IF work_geom IS NOT NULL
            AND sdo_geom.validate_geometry_with_context(work_geom, p_null_tol) != 'TRUE'
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                                   'Result of aggregation in ' || p_work_tab ||
                                                   ', step ' || p_step || '.' || all_outlines(i) || '.AGGR_GEOM is not valid');

               RAISE_APPLICATION_ERROR(-20001,'Result of aggregation at ' || p_step || '.' || all_outlines(i) || '.AGGR_GEOM' || ' is not valid');

            ELSIF work_geom IS NULL
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                                   'Result of aggregation in ' || p_work_tab ||
                                                   ', step ' || p_step || '.' || all_outlines(i) || '.AGGR_GEOM is NULL');

               RAISE_APPLICATION_ERROR(-20001,'Result of aggregation at ' || p_step || '.' || all_outlines(i) || '.AGGR_GEOM' || ' is NULL');

               --One ridiculous backup option is just to let this slide
               --this is an Oracle bug that was introduced at some point in spring 2013
               --where unions of valid geoms result in null output
               --I have no consistent workaround, can only ignore it and hope for the best
               --GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                 --                                          'WARNING: Result of aggregation in ' || p_work_tab ||
                   --                                        ', step ' || p_step || '.' || all_outlines(i) || '.AGGR_GEOM is NULL. ' ||
                     --                                      'No gap evaluation can happen');


            END IF;


            --extract holes cycles through the inner rings, if any
            --and turns them inside out, into a 2007

            IF work_geom IS NOT NULL
            THEN

               hole_geom := GZ_GEOM_UTILS.EXTRACT_HOLES(work_geom);

            END IF;

            --done with you for now
            work_geom := NULL;

            --save result
            psql := 'DELETE FROM ' || p_work_tab ||  ' a '
                 || 'WHERE a.step = :p1 ';

            EXECUTE IMMEDIATE psql USING p_step || '.' || all_outlines(i) || '.HOLE_GEOM';

            psql := 'INSERT INTO ' || p_work_tab || ' a '
                 || 'VALUES(:p1,:p2)';

            EXECUTE IMMEDIATE psql USING p_step || '.' || all_outlines(i) || '.HOLE_GEOM',
                                         hole_geom;

            COMMIT;

            IF hole_geom IS NOT NULL
            THEN

               --should be true at the tolerance we aggregated
               IF sdo_geom.validate_geometry_with_context(hole_geom, p_null_tol) != 'TRUE'
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                                         'Result of hole geom ' || p_step || '.' || all_outlines(i) || '.HOLE_GEOM is not valid',
                                                          NULL,NULL,NULL,NULL,NULL,NULL,NULL);


                  RAISE_APPLICATION_ERROR(-20001,'Result of hole geom in ' || p_step || '.' || all_outlines(i) || '.HOLE_GEOM is not valid');

               END IF;

               FOR j in 1 .. SDO_UTIL.GETNUMELEM(hole_geom)
               LOOP

                  ring := SDO_UTIL.EXTRACT(hole_geom,j);

                  width := GZ_GEOM_UTILS.MEASURE_SLIVER_WIDTH(ring,
                                                              p_sample_count,
                                                              (p_null_tol * p_null_tol));

                  IF width > max_width
                  THEN

                     max_width := width;
                     max_step  := p_step || '.' || all_outlines(i) || '.HOLE_GEOM Element ' || j;

                  END IF;

                  --min_width is just an FYI
                  --note the smallest non zero gap we encounter

                  IF min_width IS NULL
                  AND width != 0
                  THEN

                     --first time in here
                     min_width := width;

                  ELSIF width < min_width
                  AND width != 0
                  THEN

                     min_width := width;

                  END IF;

               END LOOP;

            END IF; -- has holes

         ELSE

            --state with no buddies, like Alaska
            NULL; --for now I guess

         END IF;

      END LOOP;  --end loop over each state



      --lets log some junk that isnt 100% germane to the test

      IF min_width IS NULL
      THEN
         min_width := 0;
      END IF;

      IF max_step IS NULL
      THEN
         max_step := 'NULL';
      END IF;


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                             'Finished gap evaluation on ' || p_tab || ' Just FYI, min gap = ' || min_width);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                             'Max gap size is ' || max_width);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                             'Max gap at step ' || max_step);

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_GAP_EVALUATION: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      RETURN max_width;

   END MAX_GAP_EVALUATION_2;

   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION MAX_GAP_EVALUATION (
      p_topo_out      IN VARCHAR2,
      p_tab           IN VARCHAR2,
      p_step          IN VARCHAR2,
      p_work_tab      IN VARCHAR2,
      p_sdo_col       IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_null_tol      IN NUMBER DEFAULT .0000005,
      p_sample_count  IN NUMBER DEFAULT 100
   ) RETURN NUMBER
   AS

      --This function is producing invalid geometries in the output
      --Results of sdo_aggr_union on a large mess of geoms is different from
      --   aggregating a smaller subset
      --Seems buggy to me, ditching for MAX_GAP_EVALUATION_2

      --Matt! 6/7/11
      --Takes an input table with geometries expected to be contiguous and continuous, mas o menos
      --Aggregates the input and measures the largest inner ring in the result polygon
      --If input is geodetic with .05 tolerance limitation then the SRID should be NULLed by the caller

      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;
      work_geom      SDO_GEOMETRY;
      result         VARCHAR2(4000);
      ring           SDO_GEOMETRY;
      width          NUMBER;
      max_width      NUMBER := 0;
      min_width      NUMBER := 0;


   BEGIN


      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_GAP_EVALUATION: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                              'Starting', NULL,NULL,NULL,NULL,NULL,NULL,NULL);



      --validate input w null SRID
      psql := 'SELECT count(*) '
           || 'FROM '
           || p_tab || ' a '
           || 'WHERE '
           || 'sdo_geom.validate_geometry_with_context(a.' || p_sdo_col || ', :p1) != :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_null_tol,
                                              'TRUE';

      IF kount != 0
      THEN

          GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                                'Yo, ' || p_tab || ' has invalid sdogeometry at tolerance ' || p_null_tol,
                                                 NULL,NULL,NULL,psql,NULL,NULL,NULL);

          RAISE_APPLICATION_ERROR(-20001,'Yo, ' || p_tab || ' has invalid sdogeometry at tolerance ' || p_null_tol );

      END IF;


      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_GAP_EVALUATION: Aggregating ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      work_geom := GZ_TOPO_MERGE.GZ_AGGR_UNION(p_tab,
                                               p_sdo_col,
                                               p_null_tol * p_null_tol);  --right?  ensure no closing of gaps


      --save result
      psql := 'DELETE FROM ' || p_work_tab ||  ' a '
           || 'WHERE a.step = :p1 ';

      EXECUTE IMMEDIATE psql USING p_step || '.AGGR_GEOM';

      psql := 'INSERT INTO ' || p_work_tab || ' a '
           || 'VALUES(:p1,:p2)';

      EXECUTE IMMEDIATE psql USING p_step || '.AGGR_GEOM',
                                    work_geom;

      COMMIT;

      --validate result
      psql := 'SELECT sdo_geom.validate_geometry_with_context(a.sdogeometry, :p1) '
           || 'FROM ' || p_work_tab || ' a '
           || 'WHERE a.step = :p1 ';

      EXECUTE IMMEDIATE psql INTO result USING (p_null_tol * p_null_tol),
                                                p_step || '.AGGR_GEOM';


      IF result != 'TRUE'
      THEN

         --I was thinking this should be true at the tolerance we aggregated
         --But apparently if two input polys have a tiny, sub-tolerance, sliver gap between them
         --sometimes the output doesnt merge this sliver
         --I consider this to be buggy behavior of sdo_aggr_union, but it is what it is

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                                'As usual, result of aggregation in ' || p_work_tab || ' is not valid',
                                                 NULL,NULL,NULL,psql,NULL,NULL,NULL);

         --RAISE_APPLICATION_ERROR(-20001,'Result of aggregation in ' || p_work_tab || ' is not valid');

      END IF;


      work_geom := NULL;

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_GAP_EVALUATION: Extract holes ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      --extract holes cycles through the inner rings, if any
      --and turns them inside out, into a 2007
      psql := 'SELECT GZ_TOPO_MERGE.EXTRACT_HOLES(a.sdogeometry) sdogeometry '
           || 'FROM '
           || p_work_tab || ' a '
           || 'WHERE a.step = :p1 ';

      EXECUTE IMMEDIATE psql INTO work_geom USING p_step || '.AGGR_GEOM';

      --save result
      psql := 'DELETE FROM ' || p_work_tab ||  ' a '
           || 'WHERE a.step = :p1 ';

      EXECUTE IMMEDIATE psql USING p_step || '.HOLE_GEOM';

      psql := 'INSERT INTO ' || p_work_tab || ' a '
           || 'VALUES(:p1,:p2)';

      EXECUTE IMMEDIATE psql USING p_step || '.HOLE_GEOM',
                                   work_geom;

      COMMIT;

      IF work_geom IS NULL
      THEN

         --this means there are no holes
         --seems fishy, unless we are just playing, but lets log and keep rolling
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                                'FISHY WARNING: Result of aggregation in ' || p_work_tab || ' has no holes',
                                                 NULL,NULL,NULL,NULL,NULL,NULL,NULL);

         RETURN 0;

      END IF;

      --validate result
      psql := 'SELECT sdo_geom.validate_geometry_with_context(a.sdogeometry, :p1) '
           || 'FROM ' || p_work_tab || ' a '
           || 'WHERE a.step = :p1 ';

      EXECUTE IMMEDIATE psql INTO result USING (p_null_tol * p_null_tol),
                                              p_step || '.HOLE_GEOM';

      --should be true at the tolerance we aggregated
      IF result != 'TRUE'
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                                'Result of hole geom in ' || p_work_tab || ' is not valid',
                                                 NULL,NULL,NULL,psql,NULL,NULL,NULL);


         --RAISE_APPLICATION_ERROR(-20001,'Result of hole geom in ' || p_work_tab || ' is not valid');

      END IF;

      --keep work geom
      --we just saved it for posterity

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_GAP_EVALUATION: Measure holes ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      FOR i in 1 .. SDO_UTIL.GETNUMELEM(work_geom)
      LOOP

         ring := SDO_UTIL.EXTRACT(work_geom,i);

         width := GZ_GEOM_UTILS.MEASURE_SLIVER_WIDTH(ring,
                                                     p_sample_count,
                                                     p_null_tol * p_null_tol);

         IF width > max_width
         THEN

            max_width := width;

         END IF;

         IF i = 1
         THEN

            min_width := width;

         ELSIF width < min_width
         THEN

            min_width := width;

         END IF;

      END LOOP;

      --lets log some junk that isnt 100% germane to the test

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                             'Finished gap evaluation on ' || p_tab || ' Just FYI, min gap = ' || min_width);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_GAP_EVALUATION',NULL,
                                             'Finished gap evaluation on ' || p_tab || ' Just FYI, total gap count = ' || SDO_UTIL.GETNUMELEM(work_geom));


      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_GAP_EVALUATION: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      RETURN max_width;

   END MAX_GAP_EVALUATION;


   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION MAX_OVERLAP_EVALUATION (
      p_topo_out      IN VARCHAR2,
      p_tab           IN VARCHAR2,
      p_step          IN VARCHAR2,
      p_work_tab      IN VARCHAR2,
      p_sdo_col       IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_null_tol      IN NUMBER DEFAULT .0000005,
      p_sample_count  IN NUMBER DEFAULT 100,
      p_gz_intersect  IN VARCHAR2 DEFAULT 'N'
   ) RETURN NUMBER
   AS

      --Matt! 6/14/11
      --12/28/11 Fiddle with tolerance, no longer using tol * tol in the straight oracle calls
      --         Hopped up tolerance was mysteriously producing giant intersections
      --6/26/13 Added p_gz_intersect option to call GZ_MERGE_INTERSECTION which further wraps sdo_intersection
      --           to deal with buggy NULL output.  Change to Y to pull the trigger

      --Takes an input table with geometries expected to be contiguous and continuous, mas o menos
      --Cycles through each geometry and finds neighbors
      --Intersects geom with neighbors to find overlaps
      --If input is geodetic with .05 tolerance limitation the SRID should be NULLed by the caller

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      overlap           NUMBER;
      max_overlap       NUMBER := 0;
      min_overlap       NUMBER := 0;
      max_step          VARCHAR2(4000);
      max_ring          NUMBER;
      all_outlines      GZ_TYPES.stringarray;
      buddy_outlines    GZ_TYPES.stringarray;
      touch_buddy       VARCHAR2(4000);
      ringgeom          SDO_GEOMETRY;
      stepz             GZ_TYPES.stringarray;
      geom              SDO_GEOMETRY;
      ring              SDO_GEOMETRY;
      big_ring          SDO_GEOMETRY;
      small_ring        SDO_GEOMETRY;


   BEGIN

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_OVERLAP_EVALUATION: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_OVERLAP_EVALUATION',NULL,
                                              'Starting', NULL,NULL,NULL,NULL,NULL,NULL,NULL);


      --validate input w null SRID
      psql := 'SELECT count(*) '
           || 'FROM '
           || p_tab || ' a '
           || 'WHERE '
           || 'sdo_geom.validate_geometry_with_context(a.' || p_sdo_col || ', :p1) != :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_null_tol,
                                              'TRUE';

      IF kount != 0
      THEN

          GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_OVERLAP_EVALUATION',NULL,
                                                'Yo, ' || p_tab || ' has invalid sdogeometry at tolerance ' || p_null_tol,
                                                 NULL,NULL,NULL,psql,NULL,NULL,NULL);

          RAISE_APPLICATION_ERROR(-20001,'Yo, ' || p_tab || ' has invalid sdogeometry at tolerance ' || p_null_tol );

      END IF;

      --need SIDX for this guy. Should be on unaligned_sdo column

      GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(p_tab,
                                     p_sdo_col,
                                     NULL,
                                     p_null_tol);

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_OVERLAP_EVALUATION: Cycle through each state');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      psql := 'SELECT a.source_topo '
           || 'FROM ' || p_tab || ' a ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO all_outlines;


      FOR i IN 1 .. all_outlines.COUNT
      LOOP

         --get buddies

         psql := 'select a.source_topo '
              || 'FROM '
              || p_tab || ' a, '
              || p_tab || ' b '
              || 'WHERE '
              || 'SDO_RELATE(a.' || p_sdo_col || ', b.' || p_sdo_col || ', :p1) = :p2 AND '
              || 'a.source_topo != :p3 AND '
              || 'b.source_topo = :p4 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO buddy_outlines USING 'mask=ANYINTERACT',
                                                                       'TRUE',
                                                                       all_outlines(i),
                                                                       all_outlines(i);

         --loop through buddies, if any

         FOR j IN 1 .. buddy_outlines.COUNT
         LOOP

            psql := 'SELECT '
                 || 'SDO_GEOM.RELATE(a.' || p_sdo_col || ', :p1, b.' || p_sdo_col || ', :p2) '
                 || 'FROM '
                 || p_tab || ' a, '
                 || p_tab || ' b '
                 || ' WHERE '
                 || 'a.source_topo = :p3 AND '
                 || 'b.source_topo = :p4 ';

            EXECUTE IMMEDIATE psql INTO touch_buddy USING 'mask=determine',
                                                          p_null_tol,
                                                          all_outlines(i),
                                                          buddy_outlines(j);

            IF touch_buddy != 'TOUCH'
            THEN

               --some other relationship

               --check if we've already intersected these two
               psql := 'SELECT count(*) FROM '
                     || p_work_tab || ' a '
                     || 'WHERE a.step = :p1 ';

               EXECUTE IMMEDIATE psql INTO kount USING p_step || '.OVERLAP.' || buddy_outlines(j) || '.' || all_outlines(i);

               IF kount = 0
               THEN

                  --log for funs
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_OVERLAP_EVALUATION',NULL,
                                                         'FYI, relationship between ' || all_outlines(i) || ' and '
                                                         || buddy_outlines(j) || ' is ' || touch_buddy);


                  --intersect buddies
                  psql := 'SELECT ';

                  IF p_gz_intersect = 'N'
                  THEN

                     --standard intersection wrapper
                     psql := psql
                       || 'GZ_TOPO_MERGE.GZ_INTERSECTION(a.' || p_sdo_col || ', b.' || p_sdo_col || ', :p1) ';

                  ELSE

                     --bug workaround wrapper. wraps gz_intersection
                     psql := psql
                        || 'GZ_TOPO_MERGE.GZ_MERGE_INTERSECTION(a.' || p_sdo_col || ', b.' || p_sdo_col || ', :p1) ';

                  END IF;

                  psql := psql
                       || 'FROM '
                       || p_tab || ' a, '
                       || p_tab || ' b '
                       || 'WHERE '
                       || 'a.source_topo = :p3 AND '
                       || 'b.source_topo = :p4 ';

                  EXECUTE IMMEDIATE psql INTO ringgeom USING p_null_tol,
                                                             all_outlines(i),
                                                             buddy_outlines(j);


                  --should now have one or more slivers (2003 or 2007) representing the overlap
                  --stick them in the AGGR table (doh on name)

                  --save result
                  psql := 'DELETE FROM ' || p_work_tab ||  ' a '
                       || 'WHERE a.step = :p1 ';

                  EXECUTE IMMEDIATE psql USING p_step || '.OVERLAP.' || buddy_outlines(j) || '.' || all_outlines(i);

                  psql := 'INSERT INTO ' || p_work_tab || ' a '
                       || 'VALUES(:p1,:p2)';

                  EXECUTE IMMEDIATE psql USING p_step || '.OVERLAP.' || buddy_outlines(j) || '.' || all_outlines(i),
                                               ringgeom;

                  COMMIT;

               END IF;

            END IF;

         END LOOP;

      END LOOP;

      ringgeom := NULL;

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_OVERLAP_EVALUATION: Cycle through each sliver');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      --we now have all the overlap slivers in our output work table
      --calculate max width

      psql := 'SELECT a.step '
           || 'FROM ' || p_work_tab || ' a '
           || 'WHERE a.step LIKE :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO stepz USING p_step || '.OVERLAP.' || '%';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_OVERLAP_EVALUATION',NULL,
                                             'Measuring the width of ' || stepz.COUNT || ' overlaps ');

      FOR i IN 1 .. stepz.COUNT
      LOOP

         psql := 'SELECT a.sdogeometry '
              || 'FROM ' || p_work_tab || ' a '
              || 'WHERE a.step = :p1 ';

         EXECUTE IMMEDIATE psql INTO geom USING stepz(i);


         -- Validate to be safe. Use hopped up tol
         -- This shouldnt happen, lets just throw an error
         IF geom IS NOT NULL
         AND SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(geom, p_null_tol) != 'TRUE'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_OVERLAP_EVALUATION',NULL,
                                                        'Got an invalid overlap geometry (see sqlstmt) on step ' || stepz(i),
                                                        p_sqlstmt=>'SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(geom, ' || p_null_tol || ')',
                                                        p_sdo_dump=>geom);

            RAISE_APPLICATION_ERROR(-20001,'Got an invalid overlap geometry on step ' || stepz(i));

         ELSIF geom IS NULL
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_OVERLAP_EVALUATION',NULL,
                                                        'Got a NULL overlap geometry (see sqlstmt) on step ' || stepz(i),
                                                        p_sqlstmt=>'SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(geom, ' || p_null_tol || ')',
                                                        p_sdo_dump=>geom);

            RAISE_APPLICATION_ERROR(-20001,'Got a NULL overlap geometry on step ' || stepz(i));

            --One absurd option is just to let this slide
            --This is an oracle bug, dont have much of an alternative
            --GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_OVERLAP_EVALUATION',NULL,
              --                                          'WARNING: Got a NULL overlap geometry (see sqlstmt) on step ' || stepz(i),
                --                                        p_sqlstmt=>'SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(geom, ' || p_null_tol || ')',
                  --                                      p_sdo_dump=>geom);

         END IF;

         FOR j IN 1 .. SDO_UTIL.GETNUMELEM(geom)
         LOOP

            ring    := SDO_UTIL.EXTRACT(geom,j);

            overlap := GZ_GEOM_UTILS.MEASURE_SLIVER_WIDTH(ring,
                                                          p_sample_count,
                                                          (p_null_tol * p_null_tol));

            IF overlap > max_overlap
            THEN

               max_overlap := overlap;
               big_ring    := ring;

               --these two for logging
               max_step    := stepz(i);
               max_ring    := j;

            END IF;

            IF i = 1
            THEN

               min_overlap := overlap;
               small_ring  := ring;

            ELSIF overlap < min_overlap
            THEN

               min_overlap := overlap;
               small_ring  := ring;

            END IF;

         END LOOP;


      END LOOP;


      --lets log some junk that isnt 100% germane to the test

      IF min_overlap IS NULL
      THEN
         min_overlap := 0;
      END IF;

      IF max_step IS NULL
      THEN
         max_step := 'NULL';
      END IF;

      IF max_ring IS NULL
      THEN
         max_ring := 0;
      END IF;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_OVERLAP_EVALUATION',NULL,
                                             'Finished overlap evaluation on ' || p_tab || ' Just FYI, total overlap count = ' || stepz.COUNT,
                                             NULL,NULL,NULL,NULL,NULL,NULL,NULL);


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_OVERLAP_EVALUATION',NULL,
                                             'Min overlap = ' || min_overlap,
                                             NULL,NULL,NULL,NULL,NULL,NULL,small_ring);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_OVERLAP_EVALUATION',NULL,
                                             'Max overlap = ' || max_overlap || ' (see sdo-->) ',
                                             NULL,NULL,NULL,NULL,NULL,NULL,big_ring);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_topo_out,'MAX_OVERLAP_EVALUATION',NULL,
                                             'Max overlap at step ' || max_step || ' ring ' || max_ring,
                                             NULL,NULL,NULL,NULL,NULL,NULL,big_ring);


      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MAX_OVERLAP_EVALUATION: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      RETURN max_overlap;

   END MAX_OVERLAP_EVALUATION;

   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------



   FUNCTION GZ_AGGR_UNION (
      p_tab_name      IN VARCHAR2,
      p_col_name      IN VARCHAR2,
      p_tolerance     IN NUMBER
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 6/10/11
      --This is just a placeholder until I have time to finish writing it

      output      SDO_GEOMETRY;
      psql        VARCHAR2(4000);
      kount       PLS_INTEGER;

   BEGIN

      --CYA
      psql := 'SELECT count(*) FROM '
           || p_tab_name || ' a '
           || 'WHERE '
           || 'a.' || p_col_name || '.sdo_gtype NOT IN (:p1,:p2) ';

      EXECUTE IMMEDIATE psql INTO kount USING 2003,2007;

      IF kount != 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Yo, ' || p_tab_name || ' contains weird non-polygon geometries ');

      END IF;

      --need to do spatial grouping, first, right here



      psql := 'SELECT sdo_aggr_union(mdsys.sdoaggrtype(aggr_geom,:p1)) aggr_geom '
            || 'FROM (SELECT sdo_aggr_union(mdsys.sdoaggrtype(aggr_geom,:p2)) aggr_geom '
            || '  FROM (SELECT sdo_aggr_union(mdsys.sdoaggrtype(aggr_geom,:p3)) aggr_geom '
            || '      FROM (SELECT sdo_aggr_union(mdsys.sdoaggrtype(aggr_geom,:p4)) aggr_geom '
            || '             FROM (SELECT sdo_aggr_union(mdsys.sdoaggrtype(' || p_col_name || ',:p5)) aggr_geom '
            || '                  FROM ' || p_tab_name || ' '
            || '                  GROUP BY mod(rownum,16) '
            || '                 ) '
            || '            GROUP BY mod (rownum, 8) '
            || '           ) '
            || '      GROUP BY mod (rownum, 4) '
            || '     ) '
            || '  GROUP BY mod (rownum, 2) '
            || ' ) ';

      EXECUTE IMMEDIATE psql INTO output USING p_tolerance, p_tolerance,
                                               p_tolerance, p_tolerance,
                                               p_tolerance;



      IF output.sdo_gtype NOT IN (2003,2007)
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Code to manage this junk isnt written yet');

      END IF;


      RETURN output;


   END GZ_AGGR_UNION;

   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------------------------

   FUNCTION GZ_MERGE_INTERSECTION (
      geom1_in             IN SDO_GEOMETRY,
      geom2_in             IN SDO_GEOMETRY,
      p_tolerance          IN NUMBER DEFAULT .00000005,
      p_debug              IN NUMBER DEFAULT 1,
      p_recursive          IN NUMBER DEFAULT 0
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 6/25/13
      --Wrapper to standard gz_intersection (which in turn is a wrapper to sdo_geom.sdo_intersection)
      --This version is only to be used if sdo_geom.sdo_intersection is known to be buggy
      --    in typical merge usage.  Returns NULL sometimes

      --Slightly different order of operations from the bug wrappers for difference and union

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

            output := GZ_TOPO_MERGE.GZ_INTERSECTION(geom1_in,
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
         AND sdo_geom.validate_geometry_with_context(output,p_tolerance) = 'TRUE'
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
               dbms_output.put_line('Got a geometry but its not valid at ' || p_tolerance);
            END IF;


            --Recursive call with rounding to tolerance + 1 digits
            IF p_recursive = 0
            THEN

               --Always try rounding in intersection
               IF p_debug = 1
               THEN
                  dbms_output.put_line('Going into the rabbit hole.  Rounding ordinates at ' || (length(p_tolerance) - deadman));
               END IF;

               --only call on the surface, never in the hole
               --decimal pt = +1 (usually 9 to start) then subtract a digit on each loop
               output := GZ_TOPO_MERGE.GZ_MERGE_INTERSECTION(GZ_GEOM_UTILS.ORDINATE_ROUNDER(geom1_in, length(p_tolerance) - deadman),
                                                             GZ_GEOM_UTILS.ORDINATE_ROUNDER(geom2_in, length(p_tolerance) - deadman),
                                                             p_tolerance,  --use original target tolerance
                                                             p_debug,
                                                            (p_recursive+1));

               IF output IS NOT NULL
               AND output.sdo_gtype IN (2003,2007)
               AND sdo_geom.validate_geometry_with_context(output,p_tolerance) = 'TRUE'
               THEN

                  EXIT;

               END IF;

            END IF;

            --If no exit, another loop

            deadman := deadman + 1;

            tolerance := tolerance / 10; --no idea

         END IF;


         IF p_recursive > 0
         THEN

            --just one call in recursive
            RETURN output;

         END IF;


         IF deadman > 10
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Out of ideas');

         END IF;

      END LOOP;

      IF p_debug = 1
      THEN

         dbms_output.put_line('Final tolerance used: ' || tolerance || ' on try ' || (deadman + 1));

      END IF;

      RETURN output;


   END GZ_MERGE_INTERSECTION;

   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private------------------------------------------------------------------------------------------

   FUNCTION SDO_ARRAY_TO_SDO (
      p_sdo_array   IN  GZ_TYPES.geomarray
   ) RETURN SDO_GEOMETRY
   AS

      output SDO_GEOMETRY;

   BEGIN

      FOR i IN 1 .. p_sdo_array.COUNT
      LOOP
         IF output IS NULL
         THEN
            output := p_sdo_array(i);
         ELSE
            output := SDO_UTIL.APPEND(output,p_sdo_array(i));
         END IF;

      END LOOP;

      RETURN output;

   END SDO_ARRAY_TO_SDO;

   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private------------------------------------------------------------------------------------------

   FUNCTION GZ_SCRUB_LINE (
      p_incoming    IN SDO_GEOMETRY
   ) RETURN GZ_TYPES.geomarray
   AS

      subelement   SDO_GEOMETRY;
      output       GZ_TYPES.geomarray;
      pcounter     PLS_INTEGER;

   BEGIN

      -- These outcomes are all garbage
      -- for lines
      IF p_incoming IS NULL
      OR p_incoming.SDO_GTYPE = 2001
      OR p_incoming.SDO_GTYPE = 2003
      OR p_incoming.SDO_GTYPE = 2005
      OR p_incoming.SDO_GTYPE = 2007
      THEN

         RETURN output;

      ELSIF p_incoming.SDO_GTYPE = 2002
      THEN

         output(1) := p_incoming;
         RETURN output;

      ELSIF p_incoming.SDO_GTYPE = 2004
      OR    p_incoming.SDO_GTYPE = 2006
      THEN

         pcounter := 1;
         FOR i IN 1 .. SDO_UTIL.GETNUMELEM(p_incoming)
         LOOP
            subelement := SDO_UTIL.EXTRACT(p_incoming,i);
            IF subelement.SDO_GTYPE = 2002
            THEN
               output(pcounter) := subelement;
               pcounter := pcounter + 1;
            END IF;
         END LOOP;

         RETURN output;

      ELSE
         RAISE_APPLICATION_ERROR(-20001,'incoming SDO_GEOMETRY cannot be processed!');
      END IF;

   END GZ_SCRUB_LINE;


   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private------------------------------------------------------------------------------------------

   FUNCTION GZ_SCRUB_POLY (
      p_incoming    IN SDO_GEOMETRY
   ) RETURN GZ_TYPES.geomarray
   AS

      subelement   SDO_GEOMETRY;
      output       GZ_TYPES.geomarray;
      pcounter     PLS_INTEGER;

   BEGIN

      -- These outcomes are all garbage
      -- for polygons
      IF p_incoming IS NULL
      OR p_incoming.SDO_GTYPE = 2001
      OR p_incoming.SDO_GTYPE = 2002
      OR p_incoming.SDO_GTYPE = 2005
      OR p_incoming.SDO_GTYPE = 2006
      THEN

         RETURN output;

      ELSIF p_incoming.SDO_GTYPE = 2003
      THEN

         output(1) := p_incoming;
         RETURN output;

      ELSIF p_incoming.SDO_GTYPE = 2004
      OR    p_incoming.SDO_GTYPE = 2007
      THEN

         pcounter := 1;
         FOR i IN 1 .. SDO_UTIL.GETNUMELEM(p_incoming)
         LOOP
            subelement := SDO_UTIL.EXTRACT(p_incoming,i);
            IF subelement.SDO_GTYPE = 2003
            THEN
               output(pcounter) := subelement;
               pcounter := pcounter + 1;
            END IF;
         END LOOP;

         RETURN output;

      ELSE
         RAISE_APPLICATION_ERROR(-20001,'incoming SDO_GEOMETRY cannot be processed!');
      END IF;

   END GZ_SCRUB_POLY;


   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private------------------------------------------------------------------------------------------

   FUNCTION GZ_POLY_SDO (
      p_incoming      IN SDO_GEOMETRY,
      p_clipper       IN SDO_GEOMETRY,
      p_tolerance     IN NUMBER,
      p_operation     IN VARCHAR2
   ) RETURN GZ_TYPES.geomarray
   AS

      sdo_temp     SDO_GEOMETRY;
      output       GZ_TYPES.geomarray;

   BEGIN

      IF p_operation = 'INTERSECTION'
      THEN

         IF p_incoming.SDO_GTYPE = 2003
         OR p_incoming.SDO_GTYPE = 2007
         THEN

            RETURN GZ_SCRUB_POLY(SDO_GEOM.SDO_INTERSECTION(p_incoming,p_clipper,p_tolerance));

         ELSE
            RAISE_APPLICATION_ERROR(-20001,'ERROR GZ_POLY_SDO only works on 2003 and 2007 SDO_GTYPEs!');
         END IF;

      ELSE
         RAISE_APPLICATION_ERROR(-20001,'Unknown operation ' || p_operation || '!');
      END IF;

   END GZ_POLY_SDO;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

    FUNCTION GZ_INTERSECTION (
      p_incoming      IN SDO_GEOMETRY,
      p_clipper       IN SDO_GEOMETRY,
      p_tolerance     IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt!  5/20/11 Copied and modified camps clipper

   BEGIN

      IF p_incoming.SDO_GTYPE = 2001 OR p_incoming.SDO_GTYPE = 2005
      THEN

         -- Just do a regular intersection on points
         RETURN SDO_GEOM.SDO_INTERSECTION(p_incoming,
                                          p_clipper,
                                          p_tolerance);

      ELSIF p_incoming.SDO_GTYPE = 2002 OR p_incoming.SDO_GTYPE = 2006
      THEN

         -- use the special GZ_LINE_INTERSECTION function
         RETURN GZ_TOPO_MERGE.GZ_LINE_INTERSECTION(p_incoming,
                                                   p_clipper,
                                                   p_tolerance);

      ELSIF p_incoming.SDO_GTYPE = 2003 OR p_incoming.SDO_GTYPE = 2007
      THEN

           -- use the special GZ_POLY_INTERSECTION function
         RETURN GZ_TOPO_MERGE.GZ_POLY_INTERSECTION(p_incoming,
                                                   p_clipper,
                                                   p_tolerance);

      ELSE
         RAISE_APPLICATION_ERROR(-20001,'Dude, I have no idea what to do with SDO_GTYPE ' || p_incoming.SDO_GTYPE || '!');
      END IF;


   END GZ_INTERSECTION;

   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------------------------

   FUNCTION GZ_LINE_INTERSECTION (
      p_incoming      IN SDO_GEOMETRY,
      p_clipper       IN SDO_GEOMETRY,
      p_tolerance     IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS
   BEGIN

      IF p_incoming.SDO_GTYPE != 2002 AND p_incoming.SDO_GTYPE != 2006
      THEN
         RAISE_APPLICATION_ERROR(-20001,'GZ_LINE_INTERSECTION: input_line sdo geometry is not 2002 or 2006 but ' || p_incoming.SDO_GTYPE || '!');
      END IF;

      IF p_clipper.SDO_GTYPE != 2003 AND p_clipper.SDO_GTYPE != 2007
      THEN
         RAISE_APPLICATION_ERROR(-20001,'GZ_LINE_INTERSECTION: clip_polygon sdo geometry is not 2003 or 2007 but ' || p_clipper.SDO_GTYPE || '!');
      END IF;

      RETURN SDO_ARRAY_TO_SDO(GZ_LINE_SDO(p_incoming,p_clipper,p_tolerance,'INTERSECTION'));

   END GZ_LINE_INTERSECTION;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION GZ_LINE_SDO (
      p_incoming      IN SDO_GEOMETRY,
      p_clipper       IN SDO_GEOMETRY,
      p_tolerance     IN NUMBER,
      p_operation     IN VARCHAR2
   ) RETURN GZ_TYPES.geomarray DETERMINISTIC
   AS

      output       GZ_TYPES.geomarray;

   BEGIN

      IF p_operation = 'INTERSECTION'
      THEN
         output := GZ_SCRUB_LINE(SDO_GEOM.SDO_INTERSECTION(p_incoming,p_clipper,p_tolerance));

      ELSIF p_operation = 'DIFFERENCE'
      THEN
         output := GZ_SCRUB_LINE(SDO_GEOM.SDO_DIFFERENCE(p_incoming,p_clipper,p_tolerance));
      ELSE
         RAISE_APPLICATION_ERROR(-20001,'GZ_2002_SDO: unknown operation ' || p_operation || '!');
      END IF;


      RETURN output;

   END GZ_LINE_SDO;


   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------------------------

   FUNCTION GZ_POLY_INTERSECTION (
      p_incoming    IN SDO_GEOMETRY,
      p_clipper     IN SDO_GEOMETRY,
      p_tolerance   IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS


   BEGIN

      IF p_incoming.SDO_GTYPE != 2003 AND p_incoming.SDO_GTYPE != 2007
      THEN
         RAISE_APPLICATION_ERROR(-20001,'GZ_POLY_INTERSECTION: input_line sdo geometry is not 2003 or 2007 but ' || p_incoming.SDO_GTYPE || '!');
      END IF;

      IF p_clipper.SDO_GTYPE != 2003 AND p_clipper.SDO_GTYPE != 2007
      THEN
         RAISE_APPLICATION_ERROR(-20001,'GZ_POLY_INTERSECTION: clip_polygon sdo geometry is not 2003 or 2007 but ' || p_clipper.SDO_GTYPE || '!');
      END IF;

      RETURN SDO_ARRAY_TO_SDO(GZ_POLY_SDO(p_incoming,p_clipper,p_tolerance,'INTERSECTION'));

   END GZ_POLY_INTERSECTION;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GET_MERGE_PARAMETERS (
      p_release                  IN VARCHAR2,
      p_project_id               IN VARCHAR2
   ) RETURN GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC
   AS

      --Matt! 06/06/11

      psql     VARCHAR2(4000);
      output   GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;

   BEGIN

      psql := 'SELECT a.* FROM GEN_MERGE_PARAMETERS a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING UPPER(p_release),
                                                  UPPER(p_project_id);

      EXCEPTION

         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Yo, Merge Parameter record not found for project id ' || p_project_id );
         WHEN TOO_MANY_ROWS THEN
            RAISE_APPLICATION_ERROR(-20001,'Yo, found more than one Merge Parameter record for project id ' || p_project_id);
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table GEN_MERGE_PARAMETERS does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if GEN_TOPO_MERGE_PARAMETERS_REC matches GEN_MERGE_PARAMETERS');
            ELSE
               RAISE;
            END IF;

      END;

      RETURN output;

   END GET_MERGE_PARAMETERS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION NEW_GEN_CUTTER RETURN GZ_TYPES.GEN_CUTTER PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_CUTTER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION NEW_GEN_ALFACE RETURN GZ_TYPES.GEN_ALFACE PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_ALFACE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION NEW_GEN_MERGE_PARAMETERS RETURN GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_MERGE_PARAMETERS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION NEW_GEN_AGGR RETURN GZ_TYPES.GEN_TOPO_MERGE_AGGR PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_AGGR;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION NEW_GEN_MERGE_PHONY_FSL RETURN GZ_TYPES.GEN_TOPO_MERGE_PHONY_FSL PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_MERGE_PHONY_FSL;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GEN_CUTTER (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 5/25/11

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
                   || 'SELECT * FROM TABLE(' || p_schema || '.GZ_TOPO_MERGE.NEW_GEN_CUTTER ) ';

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
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add constraints to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------

       EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || v_object_root || 'PKC '
              || '      PRIMARY KEY(SOURCE_TOPO) '
              || ')';

     ----------------------------------------------------------------------------------
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add privvies to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------


      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);




   END CREATE_GEN_CUTTER;




   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GEN_AGGR (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 6/10/11

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
                   || 'SELECT * FROM TABLE(' || p_schema || '.GZ_TOPO_MERGE.NEW_GEN_AGGR ) ';

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
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add constraints to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------

       EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || v_object_root || 'PKC '
              || '      PRIMARY KEY(STEP) '
              || ')';


     ----------------------------------------------------------------------------------
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add privvies to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------


      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);



   END CREATE_GEN_AGGR;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------


   PROCEDURE CREATE_GEN_ALFACE (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 6/27/11

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
                   || 'SELECT * FROM TABLE(' || p_schema || '.GZ_TOPO_MERGE.NEW_GEN_ALFACE ) ';

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
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add constraints to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------

      --NO 8/24/11  We will be putting all source faces in one alignment table
      --May want to revisit a composite key if any SQL operates on subsets of this table

      -- EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
      --                || 'ADD ('
      --                || '   CONSTRAINT ' || v_object_root || 'PKC '
      --                || '      PRIMARY KEY(FACE_ID) '
      --                || ')';

     ----------------------------------------------------------------------------------
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add privvies to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------


      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);



   END CREATE_GEN_ALFACE;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   PROCEDURE CREATE_GEN_MERGE_PHONY_FSL (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 12/20/11

      psql          VARCHAR2(4000);
      v_object_root VARCHAR2(4000) := p_table_name;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Create the empty table as an empty pipelined custom type');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      psql := 'CREATE TABLE ' || p_table_name || ' ';


      psql := psql || ' NOPARALLEL NOLOGGING AS '
                   || 'SELECT * FROM TABLE(' || p_schema || '.GZ_TOPO_MERGE.NEW_GEN_MERGE_PHONY_FSL ) ';

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
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add privvies to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------


      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

   END CREATE_GEN_MERGE_PHONY_FSL;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_MERGE_FACE (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 6/27/11

      psql              VARCHAR2(4000);
      merge_parms       GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      measurements      GZ_TYPES.stringarray;
      geogs             GZ_TYPES.stringarray;
      v_object_root     VARCHAR2(4000) := p_table_name;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Create the empty table as an empty pipelined custom type');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      merge_parms := GZ_TOPO_MERGE.GET_MERGE_PARAMETERS(p_release, p_project_id);

      geogs := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                      p_project_id,
                                                      'ATTRIBUTE',
                                                      merge_parms.gen_merge_face_fields);



      measurements := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                             p_project_id,
                                                             'MEASUREMENT',
                                                             merge_parms.gen_merge_face_fields);

      --Build SQL string for face table
      psql := 'CREATE TABLE ' || p_table_name || ' ('
           || 'FACE_ID NUMBER, '
           || 'SOURCE_TOPO VARCHAR2(4000), '
           || 'SOURCE_FACE_ID NUMBER, '
           || 'MEANINGLESS_ID NUMBER, '
           || 'TOPOGEOM SDO_TOPO_GEOMETRY, ';

      FOR i in 1 .. geogs.COUNT
      LOOP

         psql := psql || geogs(i) || ' VARCHAR2(4000), ';

      END LOOP;

      FOR i in 1 .. measurements.COUNT
      LOOP


         IF measurements(i) LIKE '%GEOMETRY%' --ICK
         OR measurements(i) LIKE '%MBR%'      --Double ICK
         THEN

            psql := psql || measurements(i) || ' SDO_GEOMETRY';

         ELSE

            psql := psql || measurements(i) || ' NUMBER';

         END IF;

         IF i != measurements.COUNT
         THEN

            psql := psql || ',';

         ELSE

            psql := psql || ' ) NOPARALLEL NOLOGGING ';

         END IF;


      END LOOP;

      --add hard coded QC field.  Maybe it should be marked a measurement in the reference_face_fields table?
      --now QC is returned by GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS
      --psql := psql || ' QC NUMBER ) NOPARALLEL NOLOGGING ';


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
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add constraints to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------

     /* Cant do this, will be NULL for a while

       EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || v_object_root || 'PKC '
              || '      PRIMARY KEY(FACE_ID) '
              || ')';

     */

     EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || v_object_root || 'UQC '
              || '      UNIQUE(MEANINGLESS_ID) '
              || ')';


     ----------------------------------------------------------------------------------
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add indexes to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------

     --bitmap index on handful (50ish usually) source topos

     GZ_BUSINESS_UTILS.ADD_INDEX(p_table_name,
                            p_table_name || '_ST',
                            'SOURCE_TOPO',
                            'BITMAP');

     --source face ids arent unique.
     GZ_BUSINESS_UTILS.ADD_INDEX(p_table_name,
                            p_table_name || '_SF',
                            'SOURCE_FACE_ID');

     --output face ids are almost unique
     --unless they experience mitosis
     GZ_BUSINESS_UTILS.ADD_INDEX(p_table_name,
                            p_table_name || '_FA',
                            'FACE_ID');


     ----------------------------------------------------------------------------------
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add privvies to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------


      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);


   END CREATE_MERGE_FACE;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GEN_MERGE_PARAMETERS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_MERGE_PARAMETERS'
   )
   AS

      --Matt! 06/06/11
      --Creates empty topo merge parameters table

      psql          VARCHAR2(4000);
      v_object_root VARCHAR2(4000) := p_table_name;  --??
      v_schema      VARCHAR2(32);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Create the empty table as an empty pipelined custom type');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_schema IS NULL
      THEN

         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      ELSE

         v_schema := UPPER(p_schema);

      END IF;

      psql := 'CREATE TABLE ' || p_table_name || ' ';


      psql := psql || ' NOPARALLEL NOLOGGING AS '
                   || 'SELECT * FROM TABLE(' || v_schema || '.GZ_TOPO_MERGE.NEW_GEN_MERGE_PARAMETERS ) ';

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
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add constraints to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------

       EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || v_object_root || 'PKC '
              || '      PRIMARY KEY(RELEASE,GEN_PROJECT_ID) '
              || ')';


    ----------------------------------------------------------------------------------
    --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
    DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
    DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add triggers to ' || p_table_name);
    --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
    ----------------------------------------------------------------------------------

       --only do date, user last modified, and release triggers for permanent parameter tables
       EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || v_object_root || 'TRG '
                      || 'BEFORE INSERT OR UPDATE ON ' || p_table_name || ' '
                      || 'FOR EACH ROW '
                      || 'BEGIN '
                      || '   :NEW.date_last_modified := CURRENT_DATE; '
                      || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                      || '   IF :NEW.gen_project_id IS NOT NULL '
                      || '   THEN '
                      || '      :NEW.gen_project_id := UPPER(:NEW.gen_project_id); '
                      || '   END IF; '
                      || '   IF :NEW.release IS NOT NULL '
                      || '   THEN '
                      || '      :NEW.release := UPPER(:NEW.release); '
                      || '   END IF; '
                      || 'END;';


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Grant privileges on ' || p_table_name);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);



   END CREATE_GEN_MERGE_PARAMETERS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE ADD_A_POLY_FROM_SPATIAL (
      p_toponame           IN VARCHAR2,
      p_featuretable       IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_featuretable_id    IN VARCHAR2,
      p_validate_geom      IN VARCHAR2 DEFAULT 'NO',
      p_validate_tol       IN NUMBER DEFAULT .05,
      p_geom_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_val         IN VARCHAR2 DEFAULT NULL,
      p_topomap_mbr        IN SDO_GEOMETRY DEFAULT NULL
   ) AS

      --Matt! 8/9/11
      --This one is hard core about only adding one face for each feature
      --See add_polys_from_spatial where mitosis is allowed

      --Expected Inputs
      --   1. A table (p_featuretable) with
      --      1a. Some sort of primary key column (p_featuretable_pkc) for ex edge_id
      --      1b. A column with geometry, populated and indexed (p_geom_col), NO OVERLAPS
      --      1c. An updateable column (p_featuretable_id) to receive the new face id for each add
      --   2. A pre-existing topology (p_toponame) with nothing in it
      --   3. If using the p_topomap_mbr option, must initialize metadata (for the face$ sidx) before calling

      --Output
      --   Uses add_polygon_geometry to create face primitives
      --   Populates the plain vanilla p_featuretable_id with the face_id returned

      --EXAMPLE usage
      --
      --SDO_TOPO.INITIALIZE_METADATA('MY_MT');
      --
      --GZ_UTILITIES.ADD_A_POLY_FROM_SPATIAL('MY_MT',
      --                                   'Z699LM_MERGE_FACE',
      --                                   'MEANINGLESS_ID',
      --                                   'FACE_ID',          --update this column with the face id
      --                                   'NO',               --cant validate, we have duplicate vertices from align_edges and we know this
      --                                   .05,  --real tolerance, we're back in real SRID now
      --                                   'SDOGEOMETRY',
      --                                   'SOURCE_TOPO',       --on this loop just do where column source_topo...
      --                                   'Z623LS',            --is this one state
      --                                    topo_mbr);          --gots me an MBR for the state
      --
      ----Then call a constructor, like GZ_UTILITIES.BUILD_TOPO_FROM_TOPO

      psql                 VARCHAR2(4000);
      psql2                VARCHAR2(4000);
      not_sql              VARCHAR2(4000);
      kount                PLS_INTEGER;
      toponame             VARCHAR2(32) := UPPER(p_toponame);
      featuretable         VARCHAR2(32) := UPPER(p_featuretable);
      featuretable_pkc     VARCHAR2(32) := UPPER(p_featuretable_pkc);
      featuretable_id      VARCHAR2(32) := UPPER(p_featuretable_id);
      geom_col             VARCHAR2(32) := UPPER(p_geom_col);
      newtopomap           VARCHAR2(4000) := toponame || '_TOPOMAP';  --ever need to parameterize?
      my_cursor            SYS_REFCURSOR;
      TYPE toporec         IS RECORD (
                           id NUMBER,
                           sdogeometry SDO_GEOMETRY );
      TYPE topot           IS TABLE OF toporec;
      topotab              topot;
      face_ids             GZ_TYPES.stringarray;
      featuretable_ids     GZ_TYPES.stringarray;
      ez_topo_mgr          NUMBER;
      stupid_number_array  SDO_NUMBER_ARRAY;
      ordertry             PLS_INTEGER := 0;
      special_ids          GZ_TYPES.stringarray;

      p_debug              NUMBER := 1;


   BEGIN



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_A_POLY_FROM_SPATIAL: Verify Inputs ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_validate_geom != 'NO'
      THEN

         --validate geometry, adapted from advice in topo documentation
         --http://download.oracle.com/docs/html/B14256_01/sdo_topo_concepts.htm#CIHBGDEC

         psql := 'SELECT count(*) '
              || 'FROM ' || featuretable || ' a '
              || 'WHERE '
              || '(SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.' || geom_col || ', :p1) != :p2 OR '
              || 'a.' || geom_col || '.sdo_gtype != :p3) ';

         IF p_subset_col IS NOT NULL
         THEN

            psql := psql || ' AND ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

         END IF;

         EXECUTE IMMEDIATE psql INTO kount USING p_validate_tol,
                                                 'TRUE',
                                                 2003;

         IF kount != 0
         THEN

            --Most likely duplicate vertices
            RAISE_APPLICATION_ERROR(-20001,'Geometries in ' || featuretable || ' are not valid ');

         END IF;

      END IF;


      --???????????????????????????
      --What else should we check??
      --???????????????????????????


      --Just say no: SDO_TOPO.ADD_TOPO_GEOMETRY_LAYER

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_A_POLY_FROM_SPATIAL: Initialize topo map ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      <<logoturtle>>


      IF p_topomap_mbr IS NULL
      THEN

         --entire topology topomap, can be expensive
         ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(newtopomap,toponame,2);

      ELSE

         --window topomap

         IF p_topomap_mbr.sdo_ordinates.COUNT != 4
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Whoa buddy, input mbr has ' || p_topomap_mbr.sdo_ordinates.COUNT || ' ordinates ');

         END IF;

         ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(newtopomap,
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
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_A_POLY_FROM_SPATIAL: Create features for ' || featuretable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      psql := 'SELECT ' || featuretable_pkc || ', ' || geom_col || ' '
           || 'FROM ' || featuretable || ' ';

      IF p_subset_col IS NOT NULL
      THEN

         psql := psql || ' WHERE ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

      END IF;



      IF ordertry > 0
      THEN

         --special 2th thru 5rd try, we want to do these special faces firth
         not_sql := '';
         psql2   := '';

         FOR i IN 1 .. special_ids.COUNT
         LOOP

            --not just putting these in an IN ( ) list, the order should be maintained

            psql2 := psql2
                  || 'SELECT ' || featuretable_pkc || ', ' || geom_col || ' '
                  || 'FROM ' || featuretable || ' '
                  || 'WHERE ' || featuretable_pkc || ' = ' || special_ids(i) || ' '
                  || 'UNION ALL ';

            --build this while we are looping
            IF i != special_ids.COUNT
            THEN

               not_sql := not_sql || special_ids(i) || ',';

            ELSE

               not_sql := not_sql || special_ids(i);

            END IF;

         END LOOP;

         psql2 := psql2 || psql || ' ';  --original sql above, then remove special id

         IF p_subset_col IS NOT NULL
         THEN

            psql2 := psql2 || 'AND ' || featuretable_pkc || ' NOT IN (' || not_sql || ') ';

         ELSE

            psql2 := psql2 || 'WHERE ' || featuretable_pkc || ' NOT IN (' || not_sql || ') ';

         END IF;


         --switch out for regular sql
         psql := psql2;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_A_POLY_FROM_SPATIAL',NULL,
                                                'Special try ' || ordertry,
                                                NULL,NULL,NULL,psql,NULL,NULL,NULL);


      END IF;

      --dbms_output.put_line(psql);

      BEGIN

         OPEN my_cursor FOR psql;

         LOOP

            FETCH my_cursor BULK COLLECT INTO topotab LIMIT 100; --parameterize LIMIT?
            EXIT WHEN topotab.COUNT = 0;

            FOR i in 1 .. topotab.COUNT
            LOOP


               --THE CALL----
               stupid_number_array := SDO_TOPO_MAP.ADD_POLYGON_GEOMETRY(NULL,topotab(i).sdogeometry);
               --------------


               IF stupid_number_array.COUNT > 1
               AND ( ordertry > 5
                     OR GZ_BUSINESS_UTILS.QUERY_DELIMITED_LIST(special_ids,topotab(i).id) != 0 )
               THEN

                  --SOL.
                  --Calling it quits after either 5 tries or because the same face is coming back at us


                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_A_POLY_FROM_SPATIAL',NULL,
                                                         'Yo, we expect non overlapping inputs but we got ' || stupid_number_array.COUNT
                                                      || ' faces for ' || featuretable_pkc || ' record ' || topotab(i).id,
                                                         NULL,NULL,NULL,NULL,NULL,NULL,NULL);

                  RAISE_APPLICATION_ERROR(-20001,'Yo, we expect non overlapping inputs but we got ' || stupid_number_array.COUNT
                                              || ' faces for ' || featuretable_pkc || ' record ' || topotab(i).id );

               ELSIF stupid_number_array.COUNT > 1
               AND GZ_BUSINESS_UTILS.QUERY_DELIMITED_LIST(special_ids,topotab(i).id) = 0
               THEN


                  --try this first
                  --we'll try to put this spoiled baby into the topology first
                  ordertry := ordertry + 1;

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_A_POLY_FROM_SPATIAL',NULL,
                                                         'going back for try ' || ordertry || 'adding ' || topotab(i).id || ' to the shoot list',
                                                         NULL,NULL,NULL,NULL,NULL,NULL,NULL);


                  --kill the topomap
                  SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;

                  --set this guy aside
                  special_ids(ordertry) := topotab(i).id;

                  --clean up, just in case
                  stupid_number_array.DELETE;
                  featuretable_ids.DELETE;
                  face_ids.DELETE;

                  GOTO logoturtle;

               ELSE

                  --SOP

                  face_ids(i) := stupid_number_array(1);
                  stupid_number_array.DELETE;

               END IF;

               featuretable_ids(i) := topotab(i).id;

            END LOOP;

            --Update this bucket's worth
            --1 transaction Yo

            FORALL ii IN 1 .. featuretable_ids.COUNT
               EXECUTE IMMEDIATE 'UPDATE ' || featuretable || ' a '
                              || 'SET a.' || featuretable_id || ' = :p1 '
                              || 'WHERE a.' || featuretable_pkc || ' = :p2 '
                  USING face_ids(ii),
                        featuretable_ids(ii);


            COMMIT;

            --Tidy up
            featuretable_ids.DELETE;
            face_ids.DELETE;


         END LOOP;

      END;

      CLOSE my_cursor;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_A_POLY_FROM_SPATIAL: Commit n Drop topomap ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      SDO_TOPO_MAP.COMMIT_TOPO_MAP();
      SDO_TOPO_MAP.DROP_TOPO_MAP(newtopomap);

      --JUST SAY NO
      --SDO_TOPO.INITIALIZE_METADATA(toponame);


      --verify that all of the faces we think we got above
      --are actually in the topology and not phantoms

      psql := 'SELECT count(*) FROM ( '
           || 'SELECT ' || p_featuretable_id || ' '
           || 'FROM ' || featuretable || ' ';

      IF p_subset_col IS NOT NULL
      THEN

         psql := psql || ' WHERE ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

      END IF;


      psql := psql || 'MINUS '
                   || 'SELECT face_id FROM '
                   || p_toponame || '_face$ '
                   || ') ';

      EXECUTE IMMEDIATE psql INTO kount;

      IF kount != 0
      THEN


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_A_POLY_FROM_SPATIAL',NULL,
                                                 'Yo, ' || kount || ' faces in ' || featuretable || ' have no matching face in face$ ',
                                                 NULL,NULL,NULL,psql,NULL,NULL,NULL);

         RAISE_APPLICATION_ERROR(-20001,'Yo, ' || kount || ' faces in ' || featuretable || ' have no matching face in face$ ');

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_A_POLY_FROM_SPATIAL: Complete ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------



      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_A_POLY_FROM_SPATIAL',NULL,
                                             'Done ', NULL,NULL,NULL,NULL,NULL,NULL,NULL);
      --Anything to return?


   END ADD_A_POLY_FROM_SPATIAL;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   PROCEDURE MITOSIS_MANAGER (
      p_toponame           VARCHAR2,
      p_featuretable       VARCHAR2,
      p_featuretable_pkc   VARCHAR2,         --meaningless_id
      p_featuretable_id    VARCHAR2,         --face_id
      p_subset_col         VARCHAR2,         --source_topo
      p_subset_val         VARCHAR2,         --eg Z606LS
      distinct_ids         GZ_TYPES.stringarray,
      special_ids          GZ_TYPES.stringarray,
      mitosis_face_ids     GZ_TYPES.stringarray,
      p_mitosis_col        VARCHAR2          --eg SOURCE_FACE_ID
   )
   AS

      --Matt! 8/16/11
      --take some feature face records that produced more than one face
      --duplicate them, increment primary key, and populate the face_ids
      --brutal and slow but theres usually just one or two of these

      psql              VARCHAR2(4000);
      pkc_kounter       NUMBER;
      curr_src_face     NUMBER;
      first_rec         PLS_INTEGER := 0;


   BEGIN

      --get the max primary key
      --must enforce that its always a number now

      psql := 'SELECT MAX(a.' || p_featuretable_pkc || ') + 1 '
           || 'FROM '
           || p_featuretable || ' a ';

      EXECUTE IMMEDIATE psql INTO pkc_kounter;


      FOR i IN 1 .. distinct_ids.COUNT
      LOOP

         --get the one other col we want to bring along (source_face_id)
         --kinda hard coded, if there are more we are in trouble

         psql := 'SELECT a.' || p_mitosis_col || ' '
              || 'FROM ' || p_featuretable || ' a '
              || 'WHERE a.' || p_featuretable_pkc || ' = :p1 ';

         EXECUTE IMMEDIATE psql INTO curr_src_face USING distinct_ids(i);


         --duplicate the row as many times as necessary

         FOR j IN 1 .. special_ids.COUNT
         LOOP

            IF special_ids(j) = distinct_ids(i)
            THEN

               --should match here at least twice per outer loop
               --possibly more if the face splits into 3+ faces, but I havent seen this

               IF first_rec = 0
               THEN

                  first_rec := 1;

                  --update the parent record
                  psql := 'UPDATE ' || p_featuretable || ' a '
                       || 'SET a.' || p_featuretable_id || ' = :p1 '
                       || 'WHERE a.' || p_featuretable_pkc || ' = :p2 ';

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'MITOSIS_MANAGER',NULL,
                                                         'Updating parent record. p1,p2 are ' || mitosis_face_ids(j) || ',' || special_ids(j),
                                                          NULL,NULL,NULL,psql,NULL,NULL,NULL);

                  EXECUTE IMMEDIATE psql USING mitosis_face_ids(j),
                                               special_ids(j);

                  COMMIT;

               ELSIF first_rec = 1
               THEN

                  --hit the parent, this is a child

                  psql := 'INSERT INTO ' || p_featuretable || ' '
                       || '(' || p_featuretable_id || ',' || p_subset_col || ',' || p_mitosis_col || ',' || p_featuretable_pkc || ') '
                       || 'VALUES(:p1,:p2,:p3,:p4) ';

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'MITOSIS_MANAGER',NULL,
                                                         'Duplicating record ' || distinct_ids(i) ||
                                                         ' giving it ' || p_featuretable_id || ' ' || mitosis_face_ids(j),
                                                          NULL,NULL,NULL,psql,NULL,NULL,NULL);

                  EXECUTE IMMEDIATE psql USING mitosis_face_ids(j),
                                               p_subset_val,
                                               curr_src_face,
                                               pkc_kounter;

                  COMMIT;

                  --increment pkc since we added one
                  pkc_kounter := pkc_kounter + 1;

               END IF;

            END IF;


         END LOOP;  --end loop over splitters for this id


         --reset record counter for next distinct id
         IF first_rec = 1
         THEN

            first_rec := 0;

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Didnt find a parent match for distinct id ' || distinct_ids(i));

         END IF;

      END LOOP; --end looping over all splitters


   END MITOSIS_MANAGER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE ADD_POLYS_FROM_SPATIAL (
      p_toponame           IN VARCHAR2,
      p_featuretable       IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_featuretable_id    IN VARCHAR2,
      p_validate_geom      IN VARCHAR2 DEFAULT 'NO',
      p_validate_tol       IN NUMBER DEFAULT .05,
      p_geom_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_val         IN VARCHAR2 DEFAULT NULL,
      p_topomap_mbr        IN SDO_GEOMETRY DEFAULT NULL,
      p_mitosis_col        IN VARCHAR2 DEFAULT 'SOURCE_FACE_ID'
   ) AS

      --Matt! 8/16/11
      --! 10/13/11 Updates for out of memory catch and retries. Havent actually seen this in add_poly
      --! 10/19/11 Removed memory management, all caller now
      --! 12/14/12 Rudimentary handling for not found in cache bug error
      --! 11/25/13 Added obsolete node removal

      --This one will handle input geoms being split into multiple faces
      --See add_a_poly_from_spatial for more hard core in your faces face code

      --Expected Inputs
      --   1. A table (p_featuretable) with
      --      1a. Some sort of primary key column (p_featuretable_pkc) for ex edge_id
      --      1b. A column with geometry, populated and indexed (p_geom_col), NO OVERLAPS
      --      1c. An updateable column (p_featuretable_id) to receive the new face id for each add
      --   2. A pre-existing topology (p_toponame) with nothing in it
      --   3. If using the p_topomap_mbr option, must initialize metadata (for the face$ sidx) before calling

      --Output
      --   Uses add_polygon_geometry to create face primitives
      --   Populates the plain vanilla p_featuretable_id with the face_id returned
      --   Duplicates (or triplicates, etc) any records that result in 2+ primitive faces

      --EXAMPLE usage
      --
      --SDO_TOPO.INITIALIZE_METADATA('MY_MT');
      --
      --GZ_UTILITIES.ADD_POLYS_FROM_SPATIAL('MY_MT',
      --                                   'Z699LM_MERGE_FACE',
      --                                   'MEANINGLESS_ID',
      --                                   'FACE_ID',          --update this column with the face id
      --                                   'NO',               --cant validate, we have duplicate vertices from align_edges and we know this
      --                                   .05,  --real tolerance, we're back in real SRID now
      --                                   'SDOGEOMETRY',
      --                                   'SOURCE_TOPO',       --on this loop just do where column source_topo...
      --                                   'Z623LS',            --is this one state
      --                                    topo_mbr);          --gots me an MBR for the state
      --
      ----Then call a constructor, like BUILD_TOPO_FROM_TOPO

      psql                 VARCHAR2(4000);
      psql2                VARCHAR2(4000);
      not_sql              VARCHAR2(4000);
      kount                PLS_INTEGER;
      toponame             VARCHAR2(32) := UPPER(p_toponame);
      featuretable         VARCHAR2(32) := UPPER(p_featuretable);
      featuretable_pkc     VARCHAR2(32) := UPPER(p_featuretable_pkc);
      featuretable_id      VARCHAR2(32) := UPPER(p_featuretable_id);
      geom_col             VARCHAR2(32) := UPPER(p_geom_col);
      newtopomap           VARCHAR2(4000) := toponame || '_TOPOMAP';  --ever need to parameterize?
      my_cursor            SYS_REFCURSOR;
      TYPE toporec         IS RECORD (
                           id NUMBER,
                           sdogeometry SDO_GEOMETRY );
      TYPE topot           IS TABLE OF toporec;
      topotab              topot;
      face_ids             GZ_TYPES.stringarray;
      featuretable_ids     GZ_TYPES.stringarray;
      ez_topo_mgr          NUMBER;
      stupid_number_array  SDO_NUMBER_ARRAY;
      special_ids          GZ_TYPES.stringarray;
      distinct_ids         GZ_TYPES.stringarray;
      mitosis_face_ids     GZ_TYPES.stringarray;
      update_id            PLS_INTEGER := 0;



   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_POLYS_FROM_SPATIAL: Verify Inputs ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_validate_geom != 'NO'
      THEN

         --validate geometry, adapted from advice in topo documentation
         --http://download.oracle.com/docs/html/B14256_01/sdo_topo_concepts.htm#CIHBGDEC

         psql := 'SELECT count(*) '
              || 'FROM ' || featuretable || ' a '
              || 'WHERE '
              || '(SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.' || geom_col || ', :p1) != :p2 OR '
              || 'a.' || geom_col || '.sdo_gtype != :p3) ';

         IF p_subset_col IS NOT NULL
         THEN

            psql := psql || ' AND ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

         END IF;

         EXECUTE IMMEDIATE psql INTO kount USING p_validate_tol,
                                                 'TRUE',
                                                 2003;

         IF kount != 0
         THEN

            --Most likely duplicate vertices
            RAISE_APPLICATION_ERROR(-20001,'Geometries in ' || featuretable || ' are not valid ');

         END IF;

      END IF;


      --???????????????????????????
      --What else should we check??
      --???????????????????????????


      --Just say no: SDO_TOPO.ADD_TOPO_GEOMETRY_LAYER

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_POLYS_FROM_SPATIAL: Initialize topo map ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF p_topomap_mbr IS NULL
      THEN

         --entire topology topomap, can be expensive
         ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(newtopomap,toponame,2);

      ELSE

         --window topomap

         IF p_topomap_mbr.sdo_ordinates.COUNT != 4
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Whoa buddy, input mbr has ' || p_topomap_mbr.sdo_ordinates.COUNT || ' ordinates ');

         END IF;

         ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(newtopomap,
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
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_POLYS_FROM_SPATIAL: Create features for ' || featuretable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --in case of restarts, set all the face ids to null

      psql := 'UPDATE ' || featuretable || ' a '
           || 'SET a.' || featuretable_id || ' = NULL ';

      IF p_subset_col IS NOT NULL
      THEN

         psql := psql || ' WHERE ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

      END IF;

      EXECUTE IMMEDIATE psql;
      COMMIT;


      psql := 'SELECT ' || featuretable_pkc || ', ' || geom_col || ' '
           || 'FROM ' || featuretable || ' ';

      IF p_subset_col IS NOT NULL
      THEN

         psql := psql || ' WHERE ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

      END IF;


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                             'Opening cursor to call add_polygon_geometry ',
                                             NULL,NULL,NULL,psql,NULL,NULL,NULL);


      BEGIN

         OPEN my_cursor FOR psql;

         LOOP

            FETCH my_cursor BULK COLLECT INTO topotab LIMIT 100; --parameterize LIMIT?
            EXIT WHEN topotab.COUNT = 0;

            FOR i in 1 .. topotab.COUNT
            LOOP

               BEGIN


                  --THE CALL----
                  --------------
                  stupid_number_array := SDO_TOPO_MAP.ADD_POLYGON_GEOMETRY(NULL,topotab(i).sdogeometry);
                  --------------
                  --------------

               EXCEPTION
               WHEN OTHERS
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                                         'ADD_POLYS_FROM_SPATIAL error on id ' || topotab(i).id || ' message--> ',
                                                          NULL,NULL,NULL,NULL,NULL,SQLERRM,topotab(i).sdogeometry);


                  --The topomap is hosed and uncommitted, none of the primitives are actually added to the topo
                  --but we have potentially updated some feature record oids
                  --but but but we didn't save their ids, and they are about to be overwritten on some future pass
                  --but so could probably let it slide, however that seems fishy

                  psql := 'UPDATE ' || featuretable || ' a '
                       || 'SET a.' || featuretable_id || ' = NULL '
                       || 'WHERE a.' || featuretable_id || ' IS NOT NULL ';

                  IF p_subset_col IS NOT NULL
                  THEN

                     psql := psql || 'AND a.' || p_subset_col || ' = ''' || p_subset_val || ''' ';

                  END IF;

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                                         'NULLing out any ' || featuretable_id || 's in ' || featuretable
                                                         || ' that we updated before the error' ,
                                                         NULL,NULL,NULL,psql,NULL,NULL,NULL);


                  EXECUTE IMMEDIATE psql;
                  COMMIT;

                  IF (UPPER(SQLERRM) LIKE '%OUTOFMEMORYERROR%'
                  OR UPPER(SQLERRM) LIKE '%JAVA OUT OF MEMORY CONDITION%')  --WTF causes this instead of the first?
                  THEN

                     --nothing in this handler has ever succeeded
                     --attempting to avoid this ahead of time in the caller


                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                                            'Memory error, just gonna call the magick java memory manager ',
                                                            NULL,NULL,NULL,NULL,NULL,NULL);


                     BEGIN

                        --this is just a placeholder, not expecting it to work at present
                        --Caller should call this same procedure before we get here

                        GZ_BUSINESS_UTILS.JAVA_MEMORY_MANAGER(featuretable,
                                                         'SDOGEOMETRY',
                                                         SQLERRM);  --<-- handle it wizard!
                     EXCEPTION
                     WHEN OTHERS
                     THEN

                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                                               'JAVA_MEMORY_MANAGER failed to clean house--> ',
                                                                NULL,NULL,NULL,NULL,NULL,SQLERRM);

                        --Return the error to the caller
                        --It can decide whether or not to retry or use other magic
                        RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

                     END;

                  ELSIF UPPER(SQLERRM) LIKE '%NOT FOUND IN CACHE%'
                  THEN

                     --bogus oracle bug
                     --ORA-29532: Java call terminated by uncaught Java exception:
                     --oracle.spatial.topo.TopoEntityNotFoundException: Edge ID 483909 not found in cache

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                                            'F''ing edge id X not found in cache error on '
                                                            || featuretable_pkc ||  ' ' || topotab(i).id,
                                                             NULL,NULL,NULL,NULL,NULL,SQLERRM);

                     RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

                  ELSE

                     RAISE;

                  END IF;

               END;


               IF stupid_number_array.COUNT = 1
               THEN

                  --SOP

                  --gotta keep a separate counter in here
                  --since we may be skipping elements from the main loop
                  update_id := update_id + 1;

                  face_ids(update_id) := stupid_number_array(1);
                  stupid_number_array.DELETE;
                  featuretable_ids(update_id) := topotab(i).id;

               ELSIF stupid_number_array.COUNT > 1
               THEN

                   GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                                          'Warning: Got ' || stupid_number_array.COUNT || ' ' ||
                                                          'faces for ' || featuretable || ' ' || featuretable_pkc || ': ' || topotab(i).id,
                                                           NULL,NULL,NULL,NULL,NULL,NULL,NULL);

                  --bummer, this input face is creating 2+ output faces
                  --set this bad boy aside for separate treatment

                  distinct_ids(distinct_ids.COUNT + 1) := topotab(i).id;

                  FOR jj IN 1 .. stupid_number_array.COUNT
                  LOOP

                     --NB: We arent updating this one. His face_id is NULL until mitosis mgr
                     special_ids(special_ids.COUNT + 1) :=  topotab(i).id;
                     mitosis_face_ids(mitosis_face_ids.COUNT + 1) := stupid_number_array(jj);

                  END LOOP;

               ELSE

                  RAISE_APPLICATION_ERROR(-20001,'Huh, we got ' || stupid_number_array.COUNT || ' faces from ' || topotab(i).id);

               END IF;


            END LOOP;

            --Update this bucket's worth of singles
            --1 transaction Yo

            FORALL ii IN 1 .. featuretable_ids.COUNT
               EXECUTE IMMEDIATE 'UPDATE ' || featuretable || ' a '
                              || 'SET a.' || featuretable_id || ' = :p1 '
                              || 'WHERE a.' || featuretable_pkc || ' = :p2 '
                  USING face_ids(ii),
                        featuretable_ids(ii);


            COMMIT;

            --Tidy up
            featuretable_ids.DELETE;
            face_ids.DELETE;
            update_id := 0;


         END LOOP;

      END;

      CLOSE my_cursor;
      
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_POLYS_FROM_SPATIAL: Remove obsolete nodes from ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                                  'Remove any obsolete nodes from topomap ' || newtopomap,
                                                   NULL,NULL,NULL,NULL,NULL,NULL,NULL);
                                              
      --From the docs
      --"Obsolete nodes can result when the SDO_TOPO_MAP.ADD_POLYGON_GEOMETRY function is used repeatedly to build a topology"
      --This is generally harmless in the merged topologies, but sloppy
      --is harmful when topofix stuff is called where an obsolete exists
      
      --unfortunately gz_topo_util.REMOVE_OBSOLETE_NODES expects no topomap open, full topo
      --error handler?
      --b4 and after counts? No care
      SDO_TOPO_MAP.REMOVE_OBSOLETE_NODES(NULL);      

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                             'Complete: remove any obsolete nodes from topomap ' || newtopomap,
                                              NULL,NULL,NULL,NULL,NULL,NULL,NULL);
                                              

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_POLYS_FROM_SPATIAL: Commit n Drop topomap ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                             'Commit and drop topo map ',
                                              NULL,NULL,NULL,NULL,NULL,NULL,NULL);

      
      SDO_TOPO_MAP.COMMIT_TOPO_MAP();
      SDO_TOPO_MAP.DROP_TOPO_MAP(newtopomap);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_POLYS_FROM_SPATIAL: Deal with mitosis ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF distinct_ids.COUNT > 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                                 'Calling mitosis manager for ' || distinct_ids.COUNT || ' input records ',
                                                 NULL,NULL,NULL,NULL,NULL,NULL,NULL);



         GZ_TOPO_MERGE.MITOSIS_MANAGER(p_toponame,
                                       p_featuretable,
                                       p_featuretable_pkc,
                                       p_featuretable_id,
                                       p_subset_col,
                                       p_subset_val,
                                       distinct_ids,
                                       special_ids,
                                       mitosis_face_ids,
                                       p_mitosis_col);

      END IF;



      --verify that all of the faces we think we got above
      --are actually in the topology and not phantoms

      psql := 'SELECT count(*) FROM ( '
           || 'SELECT ' || p_featuretable_id || ' '
           || 'FROM ' || featuretable || ' ';

      IF p_subset_col IS NOT NULL
      THEN

         psql := psql || ' WHERE ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

      END IF;


      psql := psql || 'MINUS '
                   || 'SELECT face_id FROM '
                   || p_toponame || '_face$ '
                   || ') ';

      EXECUTE IMMEDIATE psql INTO kount;

      IF kount != 0
      THEN


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                                 'Yo, ' || kount || ' faces in ' || featuretable || ' have no matching face in face$ ',
                                                 NULL,NULL,NULL,psql,NULL,NULL,NULL);

         RAISE_APPLICATION_ERROR(-20001,'Yo, ' || kount || ' faces in ' || featuretable || ' have no matching face in face$ ');

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_POLYS_FROM_SPATIALL: Complete ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------



      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('MERGE',p_toponame,'ADD_POLYS_FROM_SPATIAL',NULL,
                                             'Done ', NULL,NULL,NULL,NULL,NULL,NULL,NULL);
      --Anything to return?


   END ADD_POLYS_FROM_SPATIAL;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

END GZ_TOPO_MERGE;
/

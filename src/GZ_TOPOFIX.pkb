CREATE OR REPLACE PACKAGE BODY GZ_TOPOFIX
AS

   -- Face Fixing
   -- Best entry point: Wrapper gz_topofix.check_face_tab
   -- Main logical entry: gz_topofix.gz_fix_face

   --For edge fixing
   --GZ_FIX_EDGE
  
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   PROCEDURE START_TOPOFIX_LOGGING (
      p_gz_jobid       IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_log_type       IN VARCHAR2 --TOPOFIX if standalone, could also add to CLIP, MERGE, etc
   )
   AS

      --Matt! 11/15/11
      --Create logging table for this topofix

   BEGIN

      IF p_log_type = 'TOPOFIX'
      THEN

         --new topofix job, make a log
         GZ_BUSINESS_UTILS.CREATE_GEN_XTEND_TRACKING_LOG(SYS_CONTEXT('USERENV', 'CURRENT_USER'),
                                                    p_topo_out || '_TOPOFIX_TRACKING');

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo_out,
                                                'START_TOPOFIX_LOGGING',
                                                NULL,
                                                'STARTING JOB: ' || p_gz_jobid);
      ELSE

         --insert the first record

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo_out,
                                                'START_TOPOFIX_LOGGING',
                                                NULL,
                                                'STARTING JOB: ' || p_gz_jobid);

      END IF;

   END START_TOPOFIX_LOGGING;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   PROCEDURE TIDY_EXIT (
      p_gz_jobid       IN VARCHAR2,
      p_topo           IN VARCHAR2,
      p_badkount       IN NUMBER,
      p_badedgekount   IN NUMBER,
      p_log_type       IN VARCHAR2 DEFAULT 'TOPOFIX'
   )
   AS

      --Matt! 11/16/11

   BEGIN

      --Work tables to drop on success or failure?

      IF p_badkount = 0
      AND p_badedgekount = 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'TIDY_EXIT',
                                                NULL,
                                                'Total Success! Peace out on ' || p_gz_jobid);


      ELSIF p_badkount > 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'TIDY_EXIT',
                                                NULL,
                                                'Returning failed exit code. ' || p_badkount || ' faces are not fixed');

      ELSIF p_badkount = 0
      AND p_badedgekount > 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'TIDY_EXIT',
                                                NULL,
                                                'Returning failed exit code. ' || p_badedgekount || ' edges are invalid');

      END IF;

   END TIDY_EXIT;

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION GET_EDGE_GEOMETRY (
      p_topo            IN VARCHAR2,
      p_edge_id         IN VARCHAR2
   ) RETURN SDO_GEOMETRY
   AS


      psql     VARCHAR2(4000);
      output   SDO_GEOMETRY;

   BEGIN

      psql := 'SELECT e.geometry '
           || 'FROM ' || p_topo || '_EDGE$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO output USING p_edge_id;


      RETURN output;


   END GET_EDGE_GEOMETRY;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private---------------------------------------------------------------------------

   FUNCTION GET_NODE_GEOMETRY (
      p_topo            IN VARCHAR2,
      p_node_id         IN VARCHAR2
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 7/10/10
      --Got tired of typing this over and over

      psql     VARCHAR2(4000);
      output   SDO_GEOMETRY;

   BEGIN

      psql := 'SELECT n.geometry '
           || 'FROM ' || p_topo || '_NODE$ n '
           || 'WHERE n.node_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO output USING p_node_id;


      RETURN output;


   END GET_NODE_GEOMETRY;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION GZ_FIX_FACE (
      p_gz_jobid        IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_log_type        IN VARCHAR2 DEFAULT 'TOPOFIX',
      p_hold_universal  IN VARCHAR2 DEFAULT 'Y',
      p_calc_sdo        IN VARCHAR2 DEFAULT 'N',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_qc_col          IN VARCHAR2 DEFAULT 'QC',
      p_sdo_col         IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_sdo_srid        IN NUMBER DEFAULT 8265,
      p_face_pkc        IN VARCHAR2 DEFAULT 'FACE_ID',
      p_valid_edges     IN VARCHAR2 DEFAULT 'N',   --no longer used
      p_debug           IN NUMBER DEFAULT 0
   ) RETURN VARCHAR2
   AS

      --Matt! 11/10/11
      --Main entry point for face fixer
      --Called from wrapper GZ_TOPOFIX.CHECK_FACE_TAB

      --Matt! 8/9/13  Decided that the face table QC value won't be touched by anything but
      --              face fixing code.
      --              This function now updates all face QC values to NULL before flagging problems.
      --              This function only sets baddies to 1.  CHECK_FACE_TAB handles 2s and 3s
      --              So in summary
      --              1 - Invalid face
      --              2 - Null sdo
      --              3 - Not a polygon
      --              NULL - No known issues


      --In practice this code doesn't get a lot of calls these days
      --since GZ_FIX_EDGE is called first (before calculating sdogeometries)
      --and most fixes occur in there

      --declare
      --output varchar2(4000);
      --begin
      --output := GZ_TOPOFIX.GZ_FIX_FACE('jobbyjob','Z899LM','z899lm_merge_face');
      --end;
      --
      --see http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_Detailed_Steps_-_Topology_Fix

      psql              VARCHAR2(4000);
      ids_13349         GZ_TYPES.stringarray;
      ids_13356         GZ_TYPES.stringarray;
      ids_other         GZ_TYPES.stringarray;
      deadman           PLS_INTEGER;
      deadman_tries     PLS_INTEGER := 10; --parameterize more? Have seen 4 dupes on island areas faces. Now 9
                                           --think dummy think
      sub_retval        VARCHAR2(4000);
      edge_ids          GZ_TYPES.stringhash;
      p_key             VARCHAR2(4000);
      fixed_something   PLS_INTEGER;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 5');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_TOPOFIX: Let the logging begin ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --log just so we arent hanging here
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                             p_topo,
                                             'GZ_FIX_FACE',
                                             p_face_tab,
                                             'Beginning input verification regimen ');

      --any problems, this guy will log the problem details then raise an error
      GZ_TOPOFIX.VERIFY_TOPOFIX_INPUTS(p_topo,
                                       p_face_tab,
                                       p_log_type,
                                       p_calc_sdo,
                                       p_qc_col,
                                       p_sdo_col,
                                       p_face_pkc);


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_TOPOFIX: Calculate sdogeometry ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_calc_sdo = 'Y'
      THEN

         --this guy will populate all sdogeometry using get_geometry()
         --it will attempt its best to get something back, even going so far
         --   as to remove duplicate vertices from edge$ if some geoms come back NULL
         --   or the wrong gtype
         --It will not correct any invalid sdogeometries, or flag anything for QC

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'GZ_POPULATE_MEASUREMENTS',
                                                p_face_tab,
                                                'Calculating sdogeometry');

         GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_face_tab,
                                               p_face_pkc,
                                               p_sdo_col,
                                               'ALL',
                                               p_tolerance,
                                               p_sdo_srid);


      END IF;


      --Update everything to NULL
      --so we know that any set to our QA values at the end were set by us

      psql := 'UPDATE ' || p_face_tab || ' a '
           || 'SET a.' || p_qc_col || ' = NULL ';

      EXECUTE IMMEDIATE psql;

      COMMIT;

      --log just so we arent hanging here
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                             p_topo,
                                             'GZ_FIX_FACE',
                                             p_face_tab,
                                             'Getting our initial list of invalid ' || p_face_pkc || '(s) ');

      --first time only setups

      ids_13349 := GZ_TOPOFIX.GET_INVALID_IDS(p_topo,
                                              p_face_tab,
                                              '13349',
                                              p_face_pkc,
                                              p_sdo_col,
                                              p_tolerance);

      IF ids_13349.COUNT > 0
      THEN

         --log just so we arent hanging here
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'GZ_FIX_FACE',
                                                p_face_tab,
                                                'Updating ' || p_face_tab || ' ' || p_qc_col || ' to NULL for '
                                                || ids_13349.COUNT || ' 13349s ');

      END IF;



      ids_13356 := GZ_TOPOFIX.GET_INVALID_IDS(p_topo,
                                              p_face_tab,
                                              '13356',
                                              p_face_pkc,
                                              p_sdo_col,
                                              p_tolerance);

      IF ids_13356.COUNT > 0
      THEN

         --log just so we arent hanging here
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'GZ_FIX_FACE',
                                                p_face_tab,
                                                'Updating ' || p_face_tab || ' ' || p_qc_col || ' to NULL for '
                                                || ids_13356.COUNT || ' 13356s ');

      END IF;

      --initialize
      deadman := 0;


      IF  ids_13349.COUNT = 0
      AND ids_13356.COUNT = 0
      THEN

         --better log if theres nothing to do. Reduce confusion

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'GZ_FIX_FACE',
                                                p_face_tab,
                                                'Found nothing to fix ');

      END IF;

      --the big loop

      WHILE (ids_13349.COUNT > 0
      OR     ids_13356.COUNT > 0 )
      LOOP

         --reset this. Dont want to loop over and over again if theres no progress
         fixed_something := 0;

         IF deadman < deadman_tries --10 now
         THEN

            deadman := deadman + 1;

         ELSE

            --out of tries
            EXIT;

         END IF;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                '13349 Fixes',
                                                p_face_tab,
                                                'On loop ' || deadman || ' we have ' || ids_13349.COUNT || ' 13349 errors ');


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_TOPOFIX: Work on 13349 ');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         FOR i IN 1 .. ids_13349.COUNT
         LOOP


            --may have been fixed on a previous loop if in a paired relationship

            sub_retval := NULL;

            IF GZ_TOPOFIX.IS_FACE_INVALID(p_topo,
                                          p_face_tab,
                                          ids_13349(i),
                                          '13349',
                                          p_face_pkc,
                                          p_sdo_col,
                                          p_tolerance)
            THEN


               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                      p_topo,
                                                      'FIX_13349',
                                                      p_face_tab,
                                                      'Calling fixer on ' || ids_13349(i));

               BEGIN

                  IF p_hold_universal = 'Y'
                  THEN

                     --standard, do not allow universal face reshape

                     sub_retval := GZ_TOPOFIX.FIX_13349(ids_13349(i),
                                                        p_face_tab,
                                                        p_sdo_col,
                                                        SYS_CONTEXT('USERENV', 'CURRENT_USER'),
                                                        p_topo,
                                                        p_tolerance,'FALSE',p_log_type);

                  ELSE

                     --rare, allow shape changes to universal face if no other options

                     sub_retval := GZ_TOPOFIX.FIX_13349(ids_13349(i),
                                                        p_face_tab,
                                                        p_sdo_col,
                                                        SYS_CONTEXT('USERENV', 'CURRENT_USER'),
                                                        p_topo,
                                                        p_tolerance,
                                                        'TRUE',p_log_type);

                  END IF;

               EXCEPTION
               WHEN OTHERS
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                         p_topo,
                                                         'FIX_13349',
                                                         p_face_tab,
                                                         'Fixer error (see error_msg-->) on ' || ids_13349(i),
                                                         NULL,NULL,NULL,NULL,NULL,
                                                         SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

               END;

               IF UPPER(sub_retval) = 'TRUE'
               OR sub_retval = '0'
               THEN

                  fixed_something := 1;

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                         p_topo,
                                                         'FIX_13349',
                                                         p_face_tab,
                                                         'Success, fixer returned --> ' || sub_retval || ' <-- on ' || ids_13349(i));

               ELSE

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                         p_topo,
                                                         'FIX_13349',
                                                         p_face_tab,
                                                         'Possible issue, fixer returned --> ' || sub_retval || ' <-- on ' || ids_13349(i));

               END IF;


            ELSE

               fixed_something := 1;

               --got fixed somehow
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                       p_topo,
                                                       'FIX_13349',
                                                       p_face_tab,
                                                       'No call, face ' || ids_13349(i) || ' is now valid ');

            END IF;


         END LOOP; --13349 loop

         --check again (if we actually did something)

         IF ids_13349.COUNT > 0
         THEN

            ids_13349 := GZ_TOPOFIX.GET_INVALID_IDS(p_topo,
                                                   p_face_tab,
                                                   '13349',
                                                   p_face_pkc,
                                                   p_sdo_col,
                                                   p_tolerance);

             GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   '13349 Fixes',
                                                   p_face_tab,
                                                   'After loop ' || deadman || ' we now have ' || ids_13349.COUNT || ' 13349 errors ');

         END IF;
         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_TOPOFIX: Work on 13356 ');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         --always get fresh
         ids_13356 := GZ_TOPOFIX.GET_INVALID_IDS(p_topo,
                                                 p_face_tab,
                                                 '13356',
                                                 p_face_pkc,
                                                 p_sdo_col,
                                                 p_tolerance);

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                '13356 Fixes',
                                                p_face_tab,
                                                'On loop ' || deadman || ' we have ' || ids_13356.COUNT || ' 13356 errors ');

         FOR i IN 1 .. ids_13356.COUNT
         LOOP

            --may have been fixed on a previous loop if in a paired relationship

            sub_retval := NULL;

            IF GZ_TOPOFIX.IS_FACE_INVALID(p_topo,
                                          p_face_tab,
                                          ids_13356(i),
                                          '13356',
                                          p_face_pkc,
                                          p_sdo_col,
                                          p_tolerance)
            THEN


               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                      p_topo,
                                                      'FIX_13356',
                                                      p_face_tab,
                                                      'Calling fixer on ' || ids_13356(i));

               BEGIN

                  sub_retval := GZ_TOPOFIX.FIX_13356(p_topo,
                                                     p_face_tab,
                                                     ids_13356(i),
                                                     p_hold_universal,
                                                     p_tolerance,
                                                     p_sdo_col,
                                                     p_face_pkc,
                                                     p_log_type,
                                                     p_debug);

               EXCEPTION
               WHEN OTHERS
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                         p_topo,
                                                         'FIX_13356',
                                                         p_face_tab,
                                                         'Fixer error (see error_msg-->) on ' || ids_13356(i),
                                                         NULL,NULL,NULL,NULL,NULL,
                                                         SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

               END;

               IF UPPER(sub_retval) = 'TRUE'
               OR sub_retval = '0'
               THEN

                  fixed_something := 1;

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                         p_topo,
                                                         'FIX_13356',
                                                         p_face_tab,
                                                         'Success, fixer returned --> ' || sub_retval || ' <-- on ' || ids_13356(i));

               ELSE

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                         p_topo,
                                                         'FIX_13356',
                                                         p_face_tab,
                                                         'Possible issue, fixer returned --> ' || sub_retval || ' <-- on ' || ids_13356(i));

               END IF;


            ELSE

               fixed_something := 1;

               --got fixed somehow
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                       p_topo,
                                                       'FIX_13356',
                                                       p_face_tab,
                                                       'No call, face ' || ids_13356(i) || ' is now valid ');

            END IF;


         END LOOP; --13356 loop


          --check again (only if we did something)

         IF ids_13356.COUNT > 0
         THEN

            ids_13356 := GZ_TOPOFIX.GET_INVALID_IDS(p_topo,
                                                   p_face_tab,
                                                   '13356',
                                                   p_face_pkc,
                                                   p_sdo_col,
                                                   p_tolerance);

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   '13356 Fixes',
                                                   p_face_tab,
                                                   'After loop ' || deadman || ' we now have ' || ids_13356.COUNT || ' 13356 errors ');

            --if we have any errors of either type here the loop will cycle
            --if we have a clean bill of health, however, after 13349 fix and now here at 13356
            --better check that the 13356 fixes didnt generate new 13349s

            ids_13349 := GZ_TOPOFIX.GET_INVALID_IDS(p_topo,
                                                    p_face_tab,
                                                    '13349',
                                                    p_face_pkc,
                                                    p_sdo_col,
                                                    p_tolerance);

         END IF;

         IF fixed_something = 0
         THEN

            --no progress on this loop.  Get out
            EXIT;

         END IF;

      END LOOP;  --all loop


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_TOPOFIX: Sort out the results ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --check again
      --not sure what values to do here

       GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                              p_topo,
                                              'All Fixes',
                                              p_face_tab,
                                              'Making one last check for invalid ' || p_face_pkc ||'s with any validation problem ' );

      --this actually gets all faces invalid at this point
      ids_other := GZ_TOPOFIX.GET_INVALID_IDS(p_topo,
                                              p_face_tab,
                                              '1',           --matches to validation like '1%' ie 13356, 13349 etc
                                              p_face_pkc,
                                              p_sdo_col,
                                              p_tolerance);

      IF ids_other.COUNT > 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                     p_topo,
                                                     'All Fixes',
                                                     p_face_tab,
                                                     'Oh noes!  Invalid faces remain. May be invalid for other than 13349/13356');


         FOR i IN 1 .. ids_other.COUNT
         LOOP

            --log each, no harm, usually none and when 1 its confusing enough
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        p_topo,
                                                        'All Fixes',
                                                        p_face_tab,
                                                        'Updating ' || p_qc_col || ' to 1 for '
                                                        || p_face_pkc || ' ' || ids_other(i));

         END LOOP;

         FORALL ii IN 1 .. ids_other.COUNT
            EXECUTE IMMEDIATE 'UPDATE ' || p_face_tab || ' a '
                           || 'SET a.' || p_qc_col || ' = :p1 '
                           || 'WHERE a.' || p_face_pkc || ' = :p2 '
            USING '1',
                  ids_other(ii);

         COMMIT;

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'All Fixes',
                                                p_face_tab,
                                                'Didnt find any invalid ' || p_face_pkc || 's' );

      END IF;



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_TOPOFIX: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --final logging and possible work table cleanup

      GZ_TOPOFIX.TIDY_EXIT(p_gz_jobid,
                           p_topo,
                           ids_other.COUNT,
                           edge_ids.COUNT,
                           p_log_type);


      IF ids_other.COUNT = 0
      AND edge_ids.COUNT = 0
      THEN

         RETURN '0';

      ELSE

         RETURN '1';

      END IF;

   END GZ_FIX_FACE;


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE VERIFY_TOPOFIX_INPUTS (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_log_type        IN VARCHAR2,
      p_calc_sdo        IN VARCHAR2,
      p_qc_col          IN VARCHAR2,
      p_sdo_col         IN VARCHAR2,
      p_face_pkc        IN VARCHAR2
   )
   AS

      --Matt! 1/5/12
      --All verification in one procedure
      --log and raise an error if nec

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_TOPOFIX_INPUTS: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --existence of face table and 3 face table columns

      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_face_tab)
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'Verification failure',
                                                p_face_tab,
                                                'Table ' || p_face_tab || ' doesnt exist ');

         RAISE_APPLICATION_ERROR(-20001, 'Sonny, table ' || p_face_tab || ' doesnt exist ');

      END IF;

      psql := 'SELECT COUNT(1) '
           || 'FROM user_tab_columns a '
           || 'WHERE '
           || 'a.table_name = :p1 AND '
           || '(a.column_name = :p2 OR '
           || 'a.column_name = :p3 OR '
           || 'a.column_name = :p4) ';

      EXECUTE IMMEDIATE psql INTO kount USING UPPER(p_face_tab),
                                              UPPER(p_qc_col),
                                              UPPER(p_sdo_col),
                                              UPPER(p_face_pkc);

      IF kount != 3
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'Verification failure',
                                                p_face_tab,
                                                'Cant find 3 expected columns in table ' || p_face_tab);

         RAISE_APPLICATION_ERROR(-20001, 'Dude, cant find 3 expected columns in table ' || p_face_tab);

      END IF;

      --existence of topology and level 0 face table
      --and only face table registered in topology

      psql := 'SELECT COUNT(1) '
           || 'FROM user_sdo_topo_info a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.table_name = :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING UPPER(p_topo),
                                              UPPER(p_face_tab);

      IF kount = 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'Verification failure',
                                                p_face_tab,
                                                'Cant find a feature table named ' || p_face_tab || ' registered in ' || p_topo);

         --allow for >1, like boundaryedges
         RAISE_APPLICATION_ERROR(-20001, 'Dude, cant find a feature table named ' || p_face_tab || ' registered in ' || p_topo);

      END IF;


      --Now allowing this to be called in output build, with full complement of feature tables
      --the purpose of this check is to avoid registered edge work tables and state edges tables
      --they can prevent edits

      psql := 'SELECT COUNT(1) '
           || 'FROM user_sdo_topo_info a '
           || 'WHERE a.topology = :p1 AND '
           || 'a.tg_layer_type != :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING UPPER(p_topo),
                                              'POLYGON';

      IF kount != 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'Verification failure',
                                                p_face_tab,
                                                'There are ' || kount || ' linear feature tables '
                                                 || ' registered in ' || p_topo);

         RAISE_APPLICATION_ERROR(-20001, 'Yo, there are ' || kount || ' linear feature tables '
                                      || 'registered in ' || p_topo);

      END IF;


      --one to one face feature table to face$ table

      --face has no more than face $
      psql := 'SELECT count(*) FROM ('
           || 'SELECT ' || p_face_pkc || ' FROM ' || p_face_tab || ' '
           || 'MINUS '
           || 'SELECT face_id FROM ' || p_topo || '_FACE$ '
           || 'WHERE face_id != :p1 '
           || ')';

      EXECUTE IMMEDIATE psql INTO kount USING -1;

      IF kount != 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'Verification failure',
                                                p_face_tab,
                                                p_face_tab || ' has more faces than ' || p_topo || '_face$ ');

         RAISE_APPLICATION_ERROR(-20001, 'Yo! ' || p_face_tab || ' has more faces than ' || p_topo || '_face$ ');

      END IF;


      --face$ has no more than face
      psql := 'SELECT count(*) FROM ('
           || 'SELECT face_id FROM ' || p_topo || '_FACE$ '
           || 'WHERE face_id != :p1 '
           || 'MINUS '
           || 'SELECT ' || p_face_pkc || ' FROM ' || p_face_tab || ' '
           || ')';

      EXECUTE IMMEDIATE psql INTO kount USING -1;

      IF kount != 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'Verification failure',
                                                p_face_tab,
                                                p_topo || '_face$ has more faces than ' || p_face_tab);

         RAISE_APPLICATION_ERROR(-20001, 'Yo! ' || p_topo || '_face$ has more faces than ' || p_face_tab);

      END IF;

      --if no sdo calc requirement, make sure not null
      IF p_calc_sdo = 'N'
      THEN

         psql := 'SELECT count(*) '
              || 'FROM '
              || p_face_tab || ' a '
              || 'WHERE a.' || p_sdo_col || ' IS NULL ';

         EXECUTE IMMEDIATE psql INTO kount;

         IF kount > 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   'Verification failure',
                                                   p_face_tab,
                                                   kount || ' sdogeometry records are NULL ');

            RAISE_APPLICATION_ERROR(-20001, 'Buddy, you said not to calculate sdogeometry but '
                                          || kount || ' records arent populated ');

         END IF;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_TOPOFIX_INPUTS: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


   END VERIFY_TOPOFIX_INPUTS;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

    FUNCTION GET_INVALID_EDGES (
      p_topo            IN VARCHAR2,
      p_sdo_col         IN VARCHAR2 DEFAULT 'GEOMETRY',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_pkc_col         IN VARCHAR2 DEFAULT 'EDGE_ID'
   ) RETURN GZ_TYPES.stringhash
   AS

      --Matt! 3/19/12

      edge_ids          GZ_TYPES.stringarray;
      validations       GZ_TYPES.stringarray;
      psql              VARCHAR2(4000);
      output            GZ_TYPES.stringhash;

   BEGIN


      psql := 'SELECT e.' || p_pkc_col || ', '
           || 'GZ_GEOM_UTILS.VALIDATE_LINES_WITH_CONTEXT(e.' || p_sdo_col || ', :p1) '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE '
           || 'GZ_GEOM_UTILS.VALIDATE_LINES_WITH_CONTEXT(e.' || p_sdo_col || ', :p2) != :p3 ';


      EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_ids,
                                               validations USING p_tolerance,
                                                                 p_tolerance,
                                                                 'TRUE';

      FOR i IN 1 .. edge_ids.COUNT
      LOOP

         output(edge_ids(i)) := validations(i);

      END LOOP;

      RETURN output;

   END GET_INVALID_EDGES;


  ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

    FUNCTION GET_INVALID_IDS (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_error           IN VARCHAR2,
      p_face_pkc        IN VARCHAR2 DEFAULT 'FACE_ID',
      p_sdo_col         IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 11/14/11

      output            GZ_TYPES.stringarray;
      psql              VARCHAR2(4000);

   BEGIN


      psql := 'SELECT a.' || p_face_pkc || ' '
           || 'FROM '
           || p_face_tab || ' a '
           || 'WHERE '
           || 'SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.' || p_sdo_col || ', :p1) LIKE :p2 ';


      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING p_tolerance,
                                                            p_error || '%';


      RETURN output;

   END GET_INVALID_IDS;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------

    FUNCTION GET_13356_SEGMENT (
      p_geom         IN SDO_GEOMETRY,
      p_message      IN VARCHAR2
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 11/17/11

      output         SDO_GEOMETRY;
      coordinate     NUMBER;

   BEGIN

      IF p_message NOT LIKE '13356%'
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Not a 13356 error ');

      END IF;

      IF p_geom.sdo_gtype != 2003
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Yo, this guy was written for faces, gtype is ' || p_geom.sdo_gtype);

      END IF;

      coordinate := GZ_TOPOFIX.PARSE_VALIDATION_STRING(p_message,
                                                       'COORDINATE');

      output := GZ_GEOM_UTILS.GET_XYS(p_geom, 1, coordinate);

      IF output.sdo_gtype != 2002
      OR output.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Doh, something wrong with get xys segment');

      ELSE

         RETURN output;

      END IF;


   END GET_13356_SEGMENT;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------

   FUNCTION PARSE_VALIDATION_STRING (
      p_message         IN VARCHAR2,
      p_token           IN VARCHAR2
   ) RETURN NUMBER DETERMINISTIC
   AS

      --Matt! 11/17/11
      --This is kind of a joke placeholder.  Seems like finishing it is inevitable though

      --select GZ_TOPOFIX.PARSE_VALIDATION_STRING('13356 [Element <1>] [Coordinate <220>][Ring <1>]', 'Ring')
      --from dual
      -- --> 1

      first_carrot      NUMBER;
      second_carrot     NUMBER;
      output            NUMBER;

   BEGIN


      IF p_message LIKE '13356%'
      THEN

         --13356 [Element <1>] [Coordinate <220>][Ring <1>]

         --Should probably be looking for Element in the string, then next carrots XML style

         IF UPPER(p_token) = 'ELEMENT'
         THEN

            first_carrot  := REGEXP_INSTR(p_message, '<',1,1);
            second_carrot := REGEXP_INSTR(p_message, '>',1,1);

            output := SUBSTR(p_message, (first_carrot + 1), (second_carrot - first_carrot - 1));

         ELSIF UPPER(p_token) = 'COORDINATE'
         THEN

            first_carrot  := REGEXP_INSTR(p_message, '<',1,2);
            second_carrot := REGEXP_INSTR(p_message, '>',1,2);

            output := SUBSTR(p_message, (first_carrot + 1), (second_carrot - first_carrot - 1));

         ELSIF UPPER(p_token) = 'RING'
         THEN

            first_carrot  := REGEXP_INSTR(p_message, '<',1,3);
            second_carrot := REGEXP_INSTR(p_message, '>',1,3);

            output := SUBSTR(p_message, (first_carrot + 1), (second_carrot - first_carrot - 1));

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Computer says: No one taught me how to chomp token ' || p_token);

         END IF;

      ELSIF p_message LIKE '13349%'
      THEN

         --lazy
         RAISE_APPLICATION_ERROR(-20001,'Computer says: Lazy humans didnt teach me how to parse 13349 errors');

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Computer says: No one taught me how to parse ' || substr(p_message,1,5));

      END IF;


      RETURN output;

   END PARSE_VALIDATION_STRING;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE----------------------------------------------------------------------------

   FUNCTION GET_EDGE_SEGMENT_FROM_LIST (
      p_topo            IN VARCHAR2,
      p_segment         IN SDO_GEOMETRY,
      p_edges           IN GZ_TYPES.stringarray,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN NUMBER
   AS

      --Matt! 1/17/12

      psql                 VARCHAR2(4000);
      candidate_geoms      GZ_TYPES.geomarray;
      loop_edges           GZ_TYPES.stringarray;
      output               NUMBER;
      my_cursor            SYS_REFCURSOR;
      runoff_edges         GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT e.edge_id, e.geometry '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.edge_id IN (SELECT * FROM TABLE(:p1))';

      OPEN my_cursor FOR psql USING GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(p_edges);

      LOOP

         FETCH my_cursor BULK COLLECT INTO loop_edges, candidate_geoms LIMIT 10;
         EXIT WHEN loop_edges.COUNT = 0;

         FOR i in 1 .. loop_edges.COUNT
         LOOP

            --Why does it seem to be OK that one or both of these geoms is invalid at tolerance?

            IF SDO_GEOM.RELATE(p_segment, 'mask=CONTAINS+EQUAL+INSIDE+COVEREDBY', candidate_geoms(i), p_tolerance) != 'FALSE'
            AND SDO_GEOM.RELATE(p_segment, 'mask=CONTAINS+EQUAL+INSIDE+COVEREDBY', candidate_geoms(i), p_tolerance) NOT LIKE 'UNKNOWN MASK%'
            THEN

               --unknown mask is the only addition not in the callers masks
               --I think unknown mask is only returned for an edge we dont want when p_segment is really tiny
               --Serves me right for using invalid geoms

               runoff_edges(runoff_edges.COUNT + 1) := loop_edges(i);

            END IF;


         END LOOP;

      END LOOP;


      IF runoff_edges.COUNT = 1
      THEN

         RETURN runoff_edges(1);

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'New code to walk the edges goes here');

      END IF;


   END GET_EDGE_SEGMENT_FROM_LIST;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION GET_EDGE_FROM_SEGMENT (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_face_id         IN NUMBER,
      p_segment         IN SDO_GEOMETRY,
      p_face_pkc        IN VARCHAR2 DEFAULT 'FACE_ID',
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN NUMBER
   AS

      --Matt! 11/17/11
      --We have a topology and a face id that has a duplicate vertex
      --Segment is the little duplicate vertex bittty bit
      --What edge$ is it's most likely source?
      --try sdo_relate to locate it
      --if that fails, take more drastic measures

      psql                 VARCHAR2(4000);
      candidate_edges      GZ_TYPES.stringarray;
      candidate_geoms      GZ_TYPES.geomarray;
      loop_edges           GZ_TYPES.stringarray;
      output               NUMBER;
      my_cursor            SYS_REFCURSOR;
      runoff_edges         GZ_TYPES.stringarray;
      final_edge           NUMBER;



   BEGIN

      --Lets put this piece of garbage to use
      candidate_edges := GZ_TOPO_UTIL.GZ_GET_FSL_BDYEDGES(p_face_tab,
                                                          p_face_id,
                                                          p_face_pkc);

      psql := 'SELECT e.edge_id, e.geometry '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.edge_id IN (SELECT * FROM TABLE(:p1))';

      OPEN my_cursor FOR psql USING GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(candidate_edges);

      LOOP

         FETCH my_cursor BULK COLLECT INTO loop_edges, candidate_geoms LIMIT 10;
         EXIT WHEN loop_edges.COUNT = 0;

         FOR i in 1 .. loop_edges.COUNT
         LOOP

            --Why does it seem to be OK that one or both of these geoms is invalid at tolerance?

            IF SDO_GEOM.RELATE(p_segment, 'mask=CONTAINS+EQUAL+INSIDE+COVEREDBY', candidate_geoms(i), p_tolerance) != 'FALSE'
            THEN

               runoff_edges(runoff_edges.COUNT + 1) := loop_edges(i);

            END IF;


         END LOOP;

      END LOOP;


      IF runoff_edges.COUNT = 1
      THEN

         --just got one? We are done
         RETURN runoff_edges(1);

      ELSE

         --this is where we could either test LRS style
         --or check for short segments in edge$
         --get_edge_segment_from_list will only ever return 1
         final_edge := GZ_TOPOFIX.GET_EDGE_SEGMENT_FROM_LIST(p_topo,
                                                             p_segment,
                                                             runoff_edges,
                                                             p_tolerance);

         RETURN final_edge;

      END IF;


   END GET_EDGE_FROM_SEGMENT;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION GET_SHORT_SEGMENT_INDEX (
      p_topo               IN VARCHAR2,
      p_edge_id            IN VARCHAR2
   ) RETURN NUMBER
   AS

      --Matt! 11/2/11
      --Returns coordinate index ( X--0--x--1--x--2--X ) for the shortest segment in an edge

      geom                 SDO_GEOMETRY;
      current_seg          SDO_GEOMETRY;
      current_ordinates    SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();
      current_dist         NUMBER;
      shorty_dist          NUMBER;
      shorty_index         NUMBER;

   BEGIN

      geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo, p_edge_id);


      IF geom.sdo_ordinates.COUNT = 4
      THEN
         --get out for single segments
         RETURN 0;
      END IF;

      --just set it up
      current_seg := geom;
      current_ordinates.EXTEND(4);

      --No srid.  All distances will be relative, just want the shortest
      current_seg.sdo_srid := NULL;

      FOR i IN 1 .. (geom.sdo_ordinates.COUNT/2 - 1)
      LOOP

         current_ordinates(1) := geom.sdo_ordinates( (i * 2) - 1 );
         current_ordinates(2) := geom.sdo_ordinates( (i * 2) );
         current_ordinates(3) := geom.sdo_ordinates( (i * 2) + 1 );
         current_ordinates(4) := geom.sdo_ordinates( (i * 2) + 2 );

         current_seg.sdo_ordinates := current_ordinates;

         current_dist := SDO_GEOM.SDO_LENGTH(current_seg, .000000000000000005); --tolerance meh

         IF current_dist < shorty_dist
         OR i = 1
         THEN

            shorty_dist := current_dist;
            shorty_index := i - 1;

         END IF;

      END LOOP;

      RETURN shorty_index;

   END GET_SHORT_SEGMENT_INDEX;
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------

   FUNCTION IS_EDGE_UNIVERSAL (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER
   ) RETURN BOOLEAN
   AS

      --MATT! 11/22/11

      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(1) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || '(e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
           || 'e.edge_id = :p3 ';

      EXECUTE IMMEDIATE psql INTO kount USING -1, -1,
                                              p_edge_id;

      IF kount = 0
      THEN

         RETURN FALSE;

      ELSIF kount = 1
      THEN

         RETURN TRUE;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Topology is wack');

      END IF;


   END IS_EDGE_UNIVERSAL;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------

   FUNCTION IS_NODE_UNIVERSAL (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   ) RETURN BOOLEAN
   AS

      --MATT! 11/22/11

      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(1) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || '(e.start_node_id = :p1 OR e.end_node_id = :p2) AND '
           || '(e.left_face_id = :p3 or e.right_face_id = :p4) ';

      EXECUTE IMMEDIATE psql INTO kount USING p_node_id, p_node_id,
                                              -1, -1;

      IF kount = 0
      THEN

         RETURN FALSE;

      ELSIF kount = 2
      THEN

         RETURN TRUE;

      ELSIF kount = 1
      THEN

         --This is an island edge that forms a full ring
         --Any outlier cases to worry about here?
         RETURN TRUE;

      ELSIF kount > 2
      THEN

         RAISE_APPLICATION_ERROR(-20001,'You have a bowtie topology! Remove this error if thats cool');


      END IF;


   END IS_NODE_UNIVERSAL;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------

   FUNCTION IS_NODE_OBSOLETE (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   ) RETURN BOOLEAN
   AS

      --MATT! 11/28/11
      --Duh, edges fully encircling island fix 9/27/13

      --This is not an accurate definition of obsolete
      --Here I take obsolete to mean bounding 2 edges without
      --   regard to registered feature layers

      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(1) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.start_node_id = :p1 OR '
           || 'e.end_node_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_node_id,
                                              p_node_id;

      IF kount = 2
      THEN

         RETURN TRUE;

      ELSIF kount > 2
      THEN

         RETURN FALSE;

      ELSIF kount = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Node ' || p_node_id || ' is isolated ');

      ELSE

         IF kount = 1
         THEN

            --check for an island
            psql := 'SELECT COUNT(1) '
                 || 'FROM '
                 || p_topo || '_edge$ e '
                 || 'WHERE '
                 || '(e.start_node_id = :p1 OR e.end_node_id = :p2) AND '
                 || 'e.start_node_id = e.end_node_id ';

            EXECUTE IMMEDIATE psql INTO kount USING p_node_id,
                                                    p_node_id;

            IF kount = 1
            THEN

               RETURN TRUE;

            END IF;

         END IF;

         RAISE_APPLICATION_ERROR(-20001,'Node ' || p_node_id || ' bounds ' || kount || ' edges.  Whats the deal?');

      END IF;


   END IS_NODE_OBSOLETE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION GET_SEGMENT_GEOM_FROM_INDEX (
      p_edge           IN SDO_GEOMETRY,
      p_index          IN PLS_INTEGER
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 11/22/11 stole from gz_clip

      output            SDO_GEOMETRY;

   BEGIN


      output := SDO_GEOMETRY(2002,
                             p_edge.sdo_srid,
                             NULL,
                             SDO_ELEM_INFO_ARRAY(1,2,1),
                             SDO_ORDINATE_ARRAY (p_edge.sdo_ordinates( (2*p_index) + 1),
                                                 p_edge.sdo_ordinates( (2*p_index) + 2),
                                                 p_edge.sdo_ordinates( (2*p_index) + 3),
                                                 p_edge.sdo_ordinates( (2*p_index) + 4)
                                                 )
                             );

      RETURN output;

   END GET_SEGMENT_GEOM_FROM_INDEX;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION DUMP_CONSTELLATION (
      p_constellation      IN GZ_TYPES.TOPOFIX_CONSTELLATION
   ) RETURN VARCHAR2
   AS

      --Matt! 11/28/11

      output            VARCHAR2(4000);

   BEGIN

      output := Chr(10)
             || 'CLASS ' || Chr(10)
             || ' fix_class: ' || p_constellation.fix_class || Chr(10)
             || ' fix_constellation: ' || p_constellation.fix_constellation || Chr(10)
             || 'INFO ' || Chr(10)
             || ' edge_id: ' || p_constellation.edge_id || Chr(10)
             || ' edge_start_node_id: ' || p_constellation.edge_start_node_id || Chr(10)
             || ' edge_end_node_id: ' || p_constellation.edge_end_node_id || Chr(10)
             || 'CONFIGURATION ' || Chr(10)
             || ' edge_vertex_count: ' || p_constellation.edge_vertex_count || Chr(10)
             || ' edge_nodes_univ_count: ' || p_constellation.edge_nodes_univ_count || Chr(10)
             || ' shorty_index: ' || p_constellation.shorty_index || Chr(10)
             || ' edge_universal: ' || p_constellation.edge_universal || Chr(10)
             || ' segment_obsolete_nodes: ' || p_constellation.segment_obsolete_nodes;

     RETURN output;

   END DUMP_CONSTELLATION;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GET_SEGMENT_OBS_NODES (
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER,
      p_index           IN NUMBER,
      p_start_node_id   IN NUMBER,
      p_end_node_id     IN NUMBER,
      p_vertex_count    IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 11/28/11
      --For the shorty segment of an edge (may only be one segment on the edge)
      --Tell me if 0, 1, or 2 endpoints of that segment are obsolete nodes
      --Where *obsolete* in this case means bounded by only 2 edges
      --   If the node divides 2 linear features (like the state feature table)
      --   we will still call it obsolete for this purpose

      -- EDGE DIRECTION ------->
      --
      -- 0--0      ords = 4
      --           check both nodes
      --
      -- 0-X----0  ords > 4
      --           index = 0
      --           check start node
      --
      -- 0----X-0  ords > 4
      --           index = ords/2 - 2
      --           check end node
      --
      -- 0--X-X--0 ords > 4
      --           other index
      --           count = zero

      output            NUMBER;
      edge_geom         SDO_GEOMETRY;


   BEGIN


      IF p_vertex_count = 2
      THEN

         --check both

         IF IS_NODE_OBSOLETE(p_topo, p_start_node_id)
         THEN
            output := 1;
         ELSE
            output := 0;
         END IF;

         IF IS_NODE_OBSOLETE(p_topo, p_end_node_id)
         THEN
            output := output + 1;
         ELSE
            NULL;
         END IF;

      ELSIF p_vertex_count > 2
      AND p_index = 0
      THEN

         --check start

         IF IS_NODE_OBSOLETE(p_topo, p_start_node_id)
         THEN
            output := 1;
         ELSE
            output := 0;
         END IF;

      ELSIF p_vertex_count > 2
      AND p_index = (p_vertex_count - 2)
      THEN

         --check end

         IF IS_NODE_OBSOLETE(p_topo, p_end_node_id)
         THEN
            output := 1;
         ELSE
            output := 0;
         END IF;

      ELSIF p_vertex_count > 2
      AND p_index > 0
      THEN

         output := 0;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Unsupported config. vertex count: ' || p_vertex_count || 'index: ' || p_index);

      END IF;

      RETURN output;

   END GET_SEGMENT_OBS_NODES;


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

    FUNCTION GET_EDGE_CONSTELLATION (
      p_topo               IN VARCHAR2,
      p_edge_id            IN VARCHAR2
   ) RETURN GZ_TYPES.TOPOFIX_CONSTELLATION
   AS

      --Matt! 11/28/11
      --This function determines the type of 13356 error
      --All subsequent fix work is based on the determination here


      psql              VARCHAR2(4000);
      edge_geom         SDO_GEOMETRY;
      output            GZ_TYPES.TOPOFIX_CONSTELLATION;
      start_universal   PLS_INTEGER := 0;
      end_universal     PLS_INTEGER := 0;

   BEGIN

      ------
      --INFO.
      --Basic info about the edge
      ------

      output.edge_id := p_edge_id;

      psql := 'SELECT e.start_node_id, e.end_node_id '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO output.edge_start_node_id,
                                  output.edge_end_node_id USING p_edge_id;

      --------
      --CONFIG
      --Info about the edge configuration. Determines classification
      --------

      --shorty index
      output.shorty_index := GZ_TOPOFIX.GET_SHORT_SEGMENT_INDEX(p_topo,
                                                                p_edge_id);

      edge_geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo, p_edge_id);

      --vertex count
      output.edge_vertex_count := edge_geom.sdo_ordinates.COUNT/2;

      --universal node count
      --just call these 1x then sort it out

      IF IS_NODE_UNIVERSAL(p_topo,output.edge_start_node_id)
      THEN
         start_universal := 1;
      ELSE
         start_universal := 0;
      END IF;

      IF IS_NODE_UNIVERSAL(p_topo,output.edge_end_node_id)
      THEN
         end_universal := 1;
      ELSE
         end_universal := 0;
      END IF;

      output.edge_nodes_univ_count := start_universal + end_universal;

      --edge universal

      IF IS_EDGE_UNIVERSAL(p_topo, output.edge_id)
      THEN
         output.edge_universal := 'Y';
      ELSE
         output.edge_universal := 'N';
      END IF;

      --obs node count
      output.segment_obsolete_nodes := GZ_TOPOFIX.GET_SEGMENT_OBS_NODES(p_topo,
                                                                        p_edge_id,
                                                                        output.shorty_index,
                                                                        output.edge_start_node_id,
                                                                        output.edge_end_node_id,
                                                                        output.edge_vertex_count);

      --------
      --CLASSify this bad boy
      --------

      IF output.edge_universal = 'Y'
      THEN

         ----------
         --1 thru 6
         --edge on universal
         ----------

         --This could be more efficiently written, but I like to see it spelled out

         IF  output.edge_vertex_count = 2
         AND output.edge_nodes_univ_count = 2
         AND output.shorty_index = 0
         AND output.edge_universal = 'Y'
         AND output.segment_obsolete_nodes = 2
         THEN

            output.fix_class := 1;
            output.fix_constellation := 'Virgo';

         ELSIF output.edge_vertex_count > 2
         AND   output.edge_nodes_univ_count = 2
         AND   (output.shorty_index = 0 OR output.shorty_index = (output.edge_vertex_count - 2) )
         AND   output.edge_universal = 'Y'
         AND   output.segment_obsolete_nodes = 1
         THEN

            output.fix_class := 2;
            output.fix_constellation := 'Aries';

         ELSIF output.edge_vertex_count > 2
         AND   output.edge_nodes_univ_count = 2
         AND   (output.shorty_index != 0 AND output.shorty_index != (output.edge_vertex_count - 2) )
         AND   output.edge_universal = 'Y'
         AND   output.segment_obsolete_nodes = 0
         THEN

            output.fix_class := 3;
            output.fix_constellation := 'Libra';

         ELSIF output.edge_vertex_count = 2
         AND   output.edge_nodes_univ_count = 2
         AND   output.shorty_index = 0
         AND   output.edge_universal = 'Y'
         AND   output.segment_obsolete_nodes = 1
         THEN

            output.fix_class := 4;
            output.fix_constellation := 'UrsaMinor';

         ELSIF output.edge_vertex_count = 2
         AND   output.edge_nodes_univ_count = 2
         AND   output.shorty_index = 0
         AND   output.edge_universal = 'Y'
         AND   output.segment_obsolete_nodes = 0
         THEN

            output.fix_class := 5;
            output.fix_constellation := 'Lepus';

         ELSIF output.edge_vertex_count > 2
         AND   output.edge_nodes_univ_count = 2
         AND   (output.shorty_index = 0 OR output.shorty_index = (output.edge_vertex_count - 2) )
         AND   output.edge_universal = 'Y'
         AND   output.segment_obsolete_nodes = 0
         THEN

            output.fix_class := 6;
            output.fix_constellation := 'Andromeda';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Unclassified universal fix ' || GZ_TOPOFIX.DUMP_CONSTELLATION(output));

         END IF;

      ELSIF output.edge_universal = 'N'
      AND output.edge_nodes_univ_count > 0
      THEN

         ----------
         --7 thru 10 (+ one rare 14)
         --semi-interior
         ----------

         IF  output.edge_vertex_count > 2
         AND output.edge_nodes_univ_count = 1
         AND (output.shorty_index = 0 OR output.shorty_index = (output.edge_vertex_count - 2) )
         AND output.edge_universal = 'N'
         AND output.segment_obsolete_nodes = 0
         THEN

            IF output.edge_start_node_id = output.edge_end_node_id
            THEN

               output.fix_class := 7;
               output.fix_constellation := 'Gemini';

            ELSE

               output.fix_class := 8;
               output.fix_constellation := 'Capricorn';

            END IF;

         ELSIF  output.edge_vertex_count > 2
         AND    output.edge_nodes_univ_count = 2
         AND    (output.shorty_index = 0 OR output.shorty_index = (output.edge_vertex_count - 2) )
         AND    output.edge_universal = 'N'
         AND    output.segment_obsolete_nodes = 0
         THEN

            output.fix_class := 9;
            output.fix_constellation := 'CanisMinor';

         ELSIF  output.edge_vertex_count = 2
         AND    output.edge_nodes_univ_count = 1
         AND    output.shorty_index = 0
         AND    output.edge_universal = 'N'
         AND    output.segment_obsolete_nodes = 0
         THEN

            output.fix_class := 10;
            output.fix_constellation := 'Cancer';

         ELSIF  output.edge_vertex_count = 2
         AND    output.edge_nodes_univ_count = 1
         AND    output.shorty_index = 0
         AND    output.edge_universal = 'N'
         AND    output.segment_obsolete_nodes = 1
         THEN

            --This is a messed up topology, an extra-headed edge with a node in the universal void
            --added later to handle
            output.fix_class := 11;
            output.fix_constellation := 'Hydra';

         ELSIF  output.edge_vertex_count > 2
         AND    output.edge_nodes_univ_count = 1
         AND    (output.shorty_index != 0 AND output.shorty_index != (output.edge_vertex_count - 2) )
         AND    output.edge_universal = 'N'
         AND    output.segment_obsolete_nodes = 0
         THEN

            --This is a rare post-merge Cassiopeia that just happens to be connected to
            --the universal face at one of its far ends
            --added to handle the weird zig-zag in the MD-VA delmarva area that we always run in to
            output.fix_class := 14;
            output.fix_constellation := 'Cassiopeia';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Unclassified semi-interior fix ' || GZ_TOPOFIX.DUMP_CONSTELLATION(output));

         END IF;

      ELSIF output.edge_universal = 'N'
      AND output.edge_nodes_univ_count = 0
      THEN

         ----------
         --11 thru 14
         --interior
         ----------

         IF  output.edge_vertex_count = 2
         AND output.edge_nodes_univ_count = 0
         AND output.shorty_index = 0
         AND output.edge_universal = 'N'
         AND output.segment_obsolete_nodes = 0
         THEN

            output.fix_class := 12;
            output.fix_constellation := 'Crux';

         ELSIF output.edge_vertex_count > 2
         AND   output.edge_nodes_univ_count = 0
         AND   (output.shorty_index = 0 OR output.shorty_index = (output.edge_vertex_count - 2) )
         AND   output.edge_universal = 'N'
         AND   output.segment_obsolete_nodes = 0
         THEN

            output.fix_class := 13;
            output.fix_constellation := 'Cygnus';

         ELSIF output.edge_vertex_count > 2
         AND   output.edge_nodes_univ_count = 0
         AND   (output.shorty_index != 0 AND output.shorty_index != (output.edge_vertex_count - 2) )
         AND   output.edge_universal = 'N'
         AND   output.segment_obsolete_nodes = 0
         THEN

            output.fix_class := 14;
            output.fix_constellation := 'Cassiopeia';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Unclassified interior fix ' || GZ_TOPOFIX.DUMP_CONSTELLATION(output));

         END IF;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Unclassified general fix ' || GZ_TOPOFIX.DUMP_CONSTELLATION(output));

      END IF;


      RETURN output;


   END GET_EDGE_CONSTELLATION;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE REMOVE_SHORTY_VERTEX (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_index              IN NUMBER
   )
   AS

      --Matt! 11/28/11
      --We have a problem with shorty index p_index on this edge
      --Reshape the edge to remove the vertex we dislike

      old_edge_geom        SDO_GEOMETRY;
      new_edge_geom        SDO_GEOMETRY;
      new_ordinates        SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();
      ordkount             PLS_INTEGER := 0;
      skip_i               PLS_INTEGER;

   BEGIN

      old_edge_geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo,
                                                    p_edge_id);

      new_ordinates.EXTEND(old_edge_geom.sdo_ordinates.COUNT - 2);

      --rule
      --if index is 0, skip vertex 2
      --otherwise, this procedure will skip the vertex before (index +1) the index
      --most callers with first or last vertex removal will see this as transparent
      --if choosing a vertex in the middle caller must know this rule

      IF p_index = 0
      THEN

        skip_i := 2;

      ELSE

         skip_i :=  p_index + 1;

      END IF;

      FOR i IN 1 .. old_edge_geom.sdo_ordinates.COUNT/2
      LOOP

         IF i != skip_i
         THEN

            ordkount := ordkount + 1;
            new_ordinates(ordkount) := old_edge_geom.sdo_ordinates((i*2) - 1);
            ordkount := ordkount + 1;
            new_ordinates(ordkount) := old_edge_geom.sdo_ordinates(i*2);

         END IF;

      END LOOP;

      new_edge_geom := old_edge_geom;
      new_edge_geom.sdo_ordinates := new_ordinates;

      --allow this guy to throw errors back on failure
      GZ_TOPO_UTIL.GZ_CHANGE_EDGE_COORDS(p_topo,
                                         p_edge_id,
                                         new_edge_geom);


   END REMOVE_SHORTY_VERTEX;


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION PROJECT_V_TIP_VALID (
      p_edge_geom          IN SDO_GEOMETRY,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_debug              IN NUMBER DEFAULT 0
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 11/29/11
      --THIS FUNCTION DOES NOT WORK WITH REAL INVALID-type 8265 DATA
      --Problem seems to be related to precision and scale and the LRS offset fn
      --Not being used for now.  Does seem to work with a normal, valid and pretty, V shape

      --we have an edge shaped something like a V
      --Find a point that is "tolerance" away from the V tip on the obtuse side
      --
      --   \
      --    \___
      --
      --   X    <--that fella


      --This is probably still kinda WIP, I'm not getting brownie points for accuracy

      edge_orientation           NUMBER;
      vertex                     SDO_GEOMETRY;
      vertex_measure             NUMBER;
      rounded_bit                SDO_GEOMETRY;
      output_pt                  SDO_GEOMETRY;


   BEGIN

      IF p_edge_geom.sdo_ordinates.COUNT != 6
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, I only know how to handle 2 segment edges');

      END IF;

      IF p_edge_geom.sdo_srid != 8265
      THEN

         --this can be worked out but not today
         RAISE_APPLICATION_ERROR(-20001,'So sorry, but handling multiple SRIDs here is gonna require more work. '
                                     || 'See the OFFSET_GEOM_SEGMENT inputs for what I mean');

      END IF;

      --Use Sidey power to determine if V shape is oriented clockwise or counterclockwise
      --returns positive for counterclockwise
      --negative for clockwise

      edge_orientation := GZ_MATH.ORIENT2D(p_edge_geom.sdo_ordinates(1),p_edge_geom.sdo_ordinates(2),
                                           p_edge_geom.sdo_ordinates(3),p_edge_geom.sdo_ordinates(4),
                                           p_edge_geom.sdo_ordinates(5),p_edge_geom.sdo_ordinates(6));

      --If counterclockwise, the offset is to the right.  This is a negative offset in LRS-speak
      --If clockwise, offset is to the left.  This is a positive offset in LRS-speak

      --first, find the measure of the vertex in the middle

      vertex := SDO_GEOMETRY(2001,
                             p_edge_geom.sdo_srid,
                             SDO_POINT_TYPE(p_edge_geom.sdo_ordinates(3),
                                            p_edge_geom.sdo_ordinates(4),
                                            NULL),
                             NULL,NULL);


      vertex_measure := GZ_GEOM_UTILS.GZ_FIND_MEASURE(vertex, p_edge_geom);

      IF p_debug = 1
      THEN
         dbms_output.put_line('vertex measure: ' || vertex_measure);
      END IF;

      --use LRS offset to find the rounded bit off of the point of the V
      --lets use vertex_measure+.5 and vertex_measure+.5 or basically .1 percent

      IF edge_orientation > 0
      THEN

         --positive Sidey
         --counterclockwise
         --negative offset

         --some of this mess should be subbed out into more reuseable parts

         rounded_bit := SDO_LRS.CONVERT_TO_STD_GEOM(
                           SDO_LRS.OFFSET_GEOM_SEGMENT(
                              SDO_LRS.CONVERT_TO_LRS_GEOM(p_edge_geom,1,1000),    --segment
                              (vertex_measure - .5),                               --start measure
                              (vertex_measure + .5),                               --end measure
                              -(p_tolerance),                                      --offset
                              p_tolerance,                                         --tolerance
                              'unit=meter arc_tolerance=.05'                       --units. Tolerance * 20 is default in other fns
                              )
                           );

      ELSE

         --negative Sidey
         --clockwise
         --positive offset

         rounded_bit := SDO_LRS.CONVERT_TO_STD_GEOM(
                           SDO_LRS.OFFSET_GEOM_SEGMENT(
                              SDO_LRS.CONVERT_TO_LRS_GEOM(p_edge_geom,1,1000),    --segment
                              (vertex_measure - .5),                               --start measure
                              (vertex_measure + .5),                               --end measure
                              p_tolerance,                                         --offset
                              p_tolerance,                                         --tolerance
                              'unit=meter arc_tolerance=.05'    --units. Tolerance * 20 is default in other fns
                              )
                           );

      END IF;

      IF p_debug = 1
      THEN

         dbms_output.put_line('heres the rounded bit');
         dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(rounded_bit)));

      END IF;

      --now that we have a rounded arc around the tip, get its midpoint

      output_pt := GZ_GEOM_UTILS.GZ_LOCATE_PT(rounded_bit, 500);

      IF output_pt.sdo_gtype != 2001
      OR output_pt IS NULL
      THEN

         --what else to check?
         RAISE_APPLICATION_ERROR(-20001,'V tip failure ');

      ELSE

         RETURN output_pt;

      END IF;

   END PROJECT_V_TIP_VALID;


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION PROJECT_V_TIP_GARBAGE (
      p_edge_geom          IN SDO_GEOMETRY,
      p_projection_dist    IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_debug              IN NUMBER DEFAULT 0
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 11/30/11
      --This crud also doesnt work correctly about 1/2 the time
      --Sometimes it works in 8265, sometimes SRID has to be NULL.
      --Sometimes the buffer call return a nice buffer, sometimes its a point.
      --I cant discern enough of a pattern to follow through

      --we have an edge shaped something like a V
      --Find a point that is "tolerance" away from the V tip on the obtuse side
      --
      --   \
      --    \___
      --
      --   X    <--that fella

      --TESTER
      --select GZ_GEOM_UTILS.DUMP_SDO(gz_topofix.project_v_tip(SDO_GEOMETRY
      -- (2002, 8265, NULL, SDO_ELEM_INFO_ARRAY (1,2,1),
      --    SDO_ORDINATE_ARRAY
      --       (  -90.92424699999999404553818749263882637024,46.98920700000000039153746911324560642242,
      --          -90.92427200000000198087946046143770217896,46.9892830000000003565219230949878692627,
      --          -90.92427194317646410581801319494843482971,46.98928310417648646080124308355152606964
      --    )),.05,.05,1)) from dual

      work_edge_geom          SDO_GEOMETRY := p_edge_geom;
      edge_buffer             SDO_GEOMETRY;
      vertex                  SDO_GEOMETRY;
      projection_pt           SDO_GEOMETRY;
      edge_orientation        NUMBER;
      new_edge_orientation    NUMBER;

   BEGIN

      IF p_edge_geom.sdo_ordinates.COUNT != 6
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, I only know how to handle 2 segment edges');

      END IF;

      IF p_edge_geom.sdo_srid != 8265
      THEN

         --this can be worked out but not today
         RAISE_APPLICATION_ERROR(-20001,'So sorry, but handling multiple SRIDs here is gonna require more work');

      END IF;

      work_edge_geom.sdo_srid := NULL;

      --not sure if polygontoline is necessary
      --NB buffering an invalid geom with .05 geodetic tolerance
      --so far so good, seems to always return a valid buffer, even if the thing is just a .05x.05 box around the edge

      edge_buffer := SDO_UTIL.POLYGONTOLINE(SDO_GEOM.SDO_BUFFER(work_edge_geom,
                                                                .0000005,
                                                                .0000005
                                                                ));
      IF p_debug = 1
      THEN
         dbms_output.put_line('buffer shape: ');
         dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(edge_buffer)));
      END IF;


      --mid point vertex
      vertex := SDO_GEOMETRY(2001,
                             work_edge_geom.sdo_srid,
                             SDO_POINT_TYPE(work_edge_geom.sdo_ordinates(3),
                                            work_edge_geom.sdo_ordinates(4),
                                            NULL),
                             NULL,NULL);

      IF p_debug = 1
      THEN
         dbms_output.put_line('vertex (start projection point): ');
         dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(vertex)));
      END IF;

      --Use NULL SRID for LRS functions
      --edge_buffer.sdo_srid := NULL;
      --vertex.sdo_srid := NULL;

      projection_pt := GZ_GEOM_UTILS.GZ_PROJECT_PT(vertex,
                                                  edge_buffer);

      --restore SRID of result
      projection_pt.sdo_srid := p_edge_geom.sdo_srid;

      IF p_debug = 1
      THEN
         dbms_output.put_line('projection point: ');
         dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(projection_pt)));
      END IF;

      --Verify that our projection point is on the correct "side" of the V
      --In other words, the three original edge points vs  1 .. NEW .. 3
      --are still at least are the same clockwise wise

      --Sidey power returns positive for counterclockwise, negative for clockwise

      edge_orientation := GZ_MATH.ORIENT2D(p_edge_geom.sdo_ordinates(1),p_edge_geom.sdo_ordinates(2),
                                           p_edge_geom.sdo_ordinates(3),p_edge_geom.sdo_ordinates(4),
                                           p_edge_geom.sdo_ordinates(5),p_edge_geom.sdo_ordinates(6));


      new_edge_orientation := GZ_MATH.ORIENT2D(p_edge_geom.sdo_ordinates(1),p_edge_geom.sdo_ordinates(2),
                                               projection_pt.sdo_point.X,projection_pt.sdo_point.Y,
                                               p_edge_geom.sdo_ordinates(5),p_edge_geom.sdo_ordinates(6));

      IF edge_orientation > 0 AND new_edge_orientation < 0
      OR edge_orientation < 0 AND new_edge_orientation > 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Oops, went around on the wrong side ');

      ELSE

         RETURN projection_pt;

      END IF;


   END PROJECT_V_TIP_GARBAGE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --VERY private----------------------------------------------------------------------

   FUNCTION EXTEND_SEGMENT_GARBAGE (
      p_segment            IN SDO_GEOMETRY,
      p_distance           IN NUMBER
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 12/1/11
      --Dont let Sidey see this
      --10/10/12 fixed precision error
      --Actually on further review I think this garbage works pretty well
      --But extend_segment below is much cleaner and has the Sidey Stamp of approval

      x1                NUMBER;
      y1                NUMBER;
      x2                NUMBER;
      y2                NUMBER;
      dx                NUMBER;
      dy                NUMBER;
      hypotenuse        NUMBER;
      hypotenuse_prime  NUMBER;
      theta             NUMBER;
      dx_prime          NUMBER;
      dy_prime          NUMBER;
      x3                NUMBER;
      y3                NUMBER;
      add_x             NUMBER;
      add_y             NUMBER;
      output            SDO_GEOMETRY;


   BEGIN

      IF p_segment.sdo_gtype != 2002
      OR p_segment.sdo_ordinates.COUNT != 4
      OR p_distance <= 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Bad inputs. Bad');

      END IF;

      --I diagramed this out on scratch paper, FWIW

      --tan(angle) := dx/dy

      x1 := p_segment.sdo_ordinates(1);
      y1 := p_segment.sdo_ordinates(2);
      x2 := p_segment.sdo_ordinates(3);
      y2 := p_segment.sdo_ordinates(4);

      dx := ABS(x1 - x2);
      dy := ABS(y1 - y2);

      --dbms_output.put_line('dx ' || dx);
      --dbms_output.put_line('dy ' || dy);

      IF dx < .000000001
      THEN
         --Sidey is always right!
         dx := .000000001;
      END IF;


      theta := ATAN(dy/dx);

      --dbms_output.put_line('theta is ' || theta);

      hypotenuse := SQRT( (dx * dx) + (dy * dy) );

      hypotenuse_prime := hypotenuse + p_distance;

      --cos(angle) := x'/(hypotenuse')
      dx_prime := hypotenuse_prime * cos(theta);

      --sin(angle) := y'/(hypotenuse')
      dy_prime := hypotenuse_prime * sin(theta);

      --add_x and add_y are the additions to x and y to make the big triangle
      add_x := dx_prime - dx;
      add_y := dy_prime - dy;


      IF x2 < x1
      THEN

         x3 := x2 - add_x;

      ELSE

         x3 := x2 + add_x;

      END IF;

      IF y2 < y1
      THEN

         y3 := y2 - add_y;

      ELSE

         y3 := y2 + add_y;

      END IF;


      output := p_segment;

      output.sdo_ordinates(3) := x3;
      output.sdo_ordinates(4) := y3;

      RETURN output;

   END EXTEND_SEGMENT_GARBAGE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION EXTEND_SEGMENT (
      p_segment            IN SDO_GEOMETRY,
      p_percent            IN NUMBER       --typically 200, meaning double the little guy
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 10/11/12
      --Use barycentric coods
      --See http://www.farinhansford.com/dianne/teaching/cse470/materials/BarycentricCoords.pdf
      --    for something I understand
      -- for point1 p1 and point2 p2 find new point P using t
      -- P = (1-t)p1 + tp2
      -- t between 0 and 1 will be on the segment
      -- t of 1.x will be on the extension

      x1                NUMBER;
      y1                NUMBER;
      x2                NUMBER;
      y2                NUMBER;
      x3                NUMBER;
      y3                NUMBER;
      output            SDO_GEOMETRY;


   BEGIN

      IF p_segment.sdo_gtype != 2002
      OR p_segment.sdo_ordinates.COUNT != 4
      OR p_percent <= 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Bad inputs. Bad');

      END IF;


      x1 := p_segment.sdo_ordinates(1);
      y1 := p_segment.sdo_ordinates(2);
      x2 := p_segment.sdo_ordinates(3);
      y2 := p_segment.sdo_ordinates(4);


      x3 := ( (1 - p_percent/100) * x1 ) + ( p_percent/100 * x2 );
      y3 := ( (1 - p_percent/100) * y1 ) + ( p_percent/100 * y2 );


      output := p_segment;

      output.sdo_ordinates(3) := x3;
      output.sdo_ordinates(4) := y3;

      RETURN output;

   END EXTEND_SEGMENT;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION PROJECT_V_TIP (
      p_edge_geom          IN SDO_GEOMETRY,
      p_index              IN NUMBER,
      p_percent            IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_debug              IN NUMBER DEFAULT 0
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 12/01/11
      --Math!

      --we have an edge shaped something like a V
      --Find a point that is "tolerance" away from the V tip on the obtuse side
      --
      --   \
      --    \___
      --   X            <--that fella

      --take the short segment and just lenghten him by .05 meters

      --TESTER 1
      --select GZ_GEOM_UTILS.DUMP_SDO(gz_topofix.project_v_tip(SDO_GEOMETRY
      -- (2002, 8265, NULL, SDO_ELEM_INFO_ARRAY (1,2,1),
      --    SDO_ORDINATE_ARRAY
      --       (  -90.92424699999999404553818749263882637024,46.98920700000000039153746911324560642242,
      --          -90.92427200000000198087946046143770217896,46.9892830000000003565219230949878692627,
      --          -90.92427194317646410581801319494843482971,46.98928310417648646080124308355152606964
      --    )),1,.0000005,.05,1)) from dual

      --TESTER2 Mapviewer lies
      --select gz_topofix.PROJECT_V_TIP(SDO_GEOMETRY
      --(   2002,   8265,   NULL,   SDO_ELEM_INFO_ARRAY   (  1,  2,  1 ),
      --   SDO_ORDINATE_ARRAY   (  -87.51341334813450600904616294428706169128,44.86891914434845318737643538042902946472,
      --      -87.5134129999999998972270986996591091156,44.86891899999999822057361598126590251923,
      --      -87.51341291345184458805306348949670791626,44.86891897414795238319129566662013530731
      --   )),1,.0000005,.05,1) from dual


      shorty_segment          SDO_GEOMETRY;
      extended_segment        SDO_GEOMETRY;
      projection_pt           SDO_GEOMETRY;
      edge_orientation        NUMBER;
      new_edge_orientation    NUMBER;
      tolerance               NUMBER;

   BEGIN

      IF p_edge_geom.sdo_ordinates.COUNT != 6
      OR (p_index != 0 AND p_index != 1)
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, I only know how to handle 2 segment edges');

      END IF;

      IF p_edge_geom.sdo_srid != 8265
      THEN

         --this can be worked out but not today
         RAISE_APPLICATION_ERROR(-20001,'So sorry, but handling multiple SRIDs here is gonna require more work');

      END IF;

      IF p_index = 0
      THEN

         --continue in same direction, extending the first segment

         shorty_segment := SDO_GEOMETRY (
                              2002,
                              p_edge_geom.sdo_srid,
                              NULL,
                              SDO_ELEM_INFO_ARRAY (1,2,1),
                              SDO_ORDINATE_ARRAY (
                                 p_edge_geom.sdo_ordinates(p_index * 2 + 1),
                                 p_edge_geom.sdo_ordinates(p_index * 2 + 2),
                                 p_edge_geom.sdo_ordinates(p_index * 2 + 3),
                                 p_edge_geom.sdo_ordinates(p_index * 2 + 4)
                              )
                           );

      ELSE

         --reverse direction of the ordinates
         --projecting backward toward the center of the V

         shorty_segment := SDO_GEOMETRY (
                              2002,
                              p_edge_geom.sdo_srid,
                              NULL,
                              SDO_ELEM_INFO_ARRAY (1,2,1),
                              SDO_ORDINATE_ARRAY (
                                 p_edge_geom.sdo_ordinates(p_index * 2 + 3),
                                 p_edge_geom.sdo_ordinates(p_index * 2 + 4),
                                 p_edge_geom.sdo_ordinates(p_index * 2 + 1),
                                 p_edge_geom.sdo_ordinates(p_index * 2 + 2)

                              )
                           );

      END IF;

      IF p_debug = 1
      THEN
         dbms_output.put_line('passing this segment in to be extended');
         dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(shorty_segment)));
      END IF;

      extended_segment := GZ_TOPOFIX.EXTEND_SEGMENT(shorty_segment,
                                                    p_percent);

      projection_pt := SDO_GEOMETRY (
                          2001,
                          p_edge_geom.sdo_srid,
                          SDO_POINT_TYPE(extended_segment.sdo_ordinates(3),
                                         extended_segment.sdo_ordinates(4),
                                         NULL),
                          NULL,
                          NULL);

      RETURN projection_pt;

   END PROJECT_V_TIP;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION GET_NEXT_DIRECTED_SEGMENT (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_node_id            IN NUMBER,
      p_direction          IN VARCHAR2,           --not really direction, just where on the edge we are
      p_ignore_edge        IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 12/06/11
      --For an edge (must be on universal face) get the closest segment
      --on the neighboring edge (x1,y1,x2,y2)
      --If the neighboring edge runs the opposite direction of the edge
      --   we are interested in, reverse the ordinates in the output

      psql                    VARCHAR2(4000);
      neighbor_edge_id        NUMBER;
      neighbor_edge_geom      SDO_GEOMETRY;
      neighbor_start_node_id  NUMBER;
      neighbor_end_node_id    NUMBER;
      neighbor_edge_seg       SDO_GEOMETRY;
      ring_flag               PLS_INTEGER := 0;


   BEGIN

      psql := 'SELECT e.edge_id, e.geometry, e.start_node_id, e.end_node_id '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id != :p1 AND '
           || '(e.left_face_id = :p2 OR e.right_face_id = :p3) AND '
           || '(e.start_node_id = :p4 OR e.end_node_id = :p5) ';

      IF p_ignore_edge IS NULL THEN

         BEGIN

            EXECUTE IMMEDIATE psql INTO neighbor_edge_id,
                                        neighbor_edge_geom,
                                        neighbor_start_node_id,
                                        neighbor_end_node_id USING p_edge_id,
                                                                   -1,
                                                                   -1,
                                                                   p_node_id,
                                                                   p_node_id;

         EXCEPTION
         WHEN NO_DATA_FOUND
         THEN

            --probably a rare edge that fully loops an island and has the shorty bit next to the node
             psql := 'SELECT e.edge_id, e.geometry, e.start_node_id, e.end_node_id '
                  || 'FROM '
                  || p_topo || '_edge$ e '
                  || 'WHERE '
                  || 'e.edge_id = :p1 AND '                                  --now equals
                  || '(e.left_face_id = :p2 OR e.right_face_id = :p3) AND '  --same as above
                  || '(e.start_node_id = :p4 OR e.end_node_id = :p5) AND '   --same as above
                  || '(e.start_node_id = e.end_node_id)';                    --added

            EXECUTE IMMEDIATE psql INTO neighbor_edge_id,
                                        neighbor_edge_geom,
                                        neighbor_start_node_id,
                                        neighbor_end_node_id USING p_edge_id,
                                                                   -1,
                                                                   -1,
                                                                   p_node_id,
                                                                   p_node_id;

             ring_flag := 1;

         WHEN OTHERS
         THEN

            RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

         END;

      ELSE

         --special handling when weve added a fake face and edge to ignore

         psql := psql || ' AND e.edge_id != :p6 ';

         EXECUTE IMMEDIATE psql INTO neighbor_edge_id,
                                     neighbor_edge_geom,
                                     neighbor_start_node_id,
                                     neighbor_end_node_id USING p_edge_id,
                                                                -1,
                                                                -1,
                                                                p_node_id,
                                                                p_node_id,
                                                                p_ignore_edge;

      END IF;

      IF neighbor_edge_id IS NULL
      THEN
         RAISE_APPLICATION_ERROR(-20001, 'Bad inputs, edge_id ' || p_edge_id || ' likely isnt on universal ');
      END IF;

      IF neighbor_start_node_id = p_node_id
      AND ring_flag = 0
      THEN

         --first segment
         neighbor_edge_seg := GZ_TOPOFIX.GET_SEGMENT_GEOM_FROM_INDEX(neighbor_edge_geom,
                                                                     0);

         IF p_direction = 'START'
         THEN
            --reverse it
            neighbor_edge_seg := SDO_UTIL.REVERSE_LINESTRING(neighbor_edge_seg);
         END IF;

      ELSIF neighbor_end_node_id = p_node_id
      AND ring_flag = 0
      THEN

         --last segment
         neighbor_edge_seg := GZ_TOPOFIX.GET_SEGMENT_GEOM_FROM_INDEX(neighbor_edge_geom,
                                                                     (neighbor_edge_geom.sdo_ordinates.COUNT/2 - 2));

         IF p_direction = 'END'
         THEN
            --reverse it
            neighbor_edge_seg := SDO_UTIL.REVERSE_LINESTRING(neighbor_edge_seg);
         END IF;

      ELSIF ring_flag = 1
      THEN

         --base entirely on p_direction
         IF p_direction = 'END'
         THEN

            --get the first/start, no need to reverse
            neighbor_edge_seg := GZ_TOPOFIX.GET_SEGMENT_GEOM_FROM_INDEX(neighbor_edge_geom,
                                                                        0);

         ELSE

            --get the last/end, no need to reverse
            neighbor_edge_seg := GZ_TOPOFIX.GET_SEGMENT_GEOM_FROM_INDEX(neighbor_edge_geom,
                                                                       (neighbor_edge_geom.sdo_ordinates.COUNT/2 - 2));

         END IF;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Wtf?');

      END IF;

      RETURN neighbor_edge_seg;

   END GET_NEXT_DIRECTED_SEGMENT;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION CALC_SEGMENT_ANGLE (
      p_geom1              IN SDO_GEOMETRY,
      p_geom2              IN SDO_GEOMETRY
   ) RETURN NUMBER DETERMINISTIC
   AS

      --Matt! 12/6/11
      --What it looks like. Wrapper for Sidey util in GZ_MATH
      --must be joint simple segments with same direction

      theta                NUMBER;

   BEGIN

      IF p_geom1.sdo_ordinates.COUNT != 4
      OR p_geom2.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Not simple segments');

      END IF;

      IF p_geom1.sdo_ordinates(3) != p_geom2.sdo_ordinates(1)
      OR p_geom1.sdo_ordinates(4) != p_geom2.sdo_ordinates(2)
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Segments dont touch or direction is not right');

      END IF;

      theta := GZ_MATH.ANGLE_DOTP(p_geom1.sdo_ordinates(1),
                                  p_geom1.sdo_ordinates(2),
                                  p_geom1.sdo_ordinates(3),
                                  p_geom1.sdo_ordinates(4),
                                  p_geom2.sdo_ordinates(3),
                                  p_geom2.sdo_ordinates(4));

      IF theta IS NULL
      OR theta > 180
      OR theta < 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Wierd result, angle is: ' || theta);

      END IF;

      RETURN theta;


   END CALC_SEGMENT_ANGLE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION GET_NODE_ID (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_tip                IN VARCHAR2
   ) RETURN NUMBER
   AS

      --Matt! 12/12/11  Surely this fn exists in 20 places?

      psql              VARCHAR2(4000);
      output            NUMBER;

   BEGIN

     IF p_tip NOT IN ('START','END')
     THEN
        RAISE_APPLICATION_ERROR(-20001,'Tip must be start or end');
     END IF;

     psql := 'SELECT e.' || p_tip || '_node_id '
          || 'FROM '
          || p_topo || '_edge$ e '
          || 'WHERE '
          || 'e.edge_id = :p1 ';

     EXECUTE IMMEDIATE psql INTO output USING p_edge_id;

     IF output IS NULL
     THEN
        RAISE_APPLICATION_ERROR(-20001,'Didnt get a node, does edge ' || p_edge_id || ' exist?');
     END IF;

     RETURN output;

   END GET_NODE_ID;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE RECONNECT_NODE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_shorty_index       IN NUMBER,
      p_tip                IN VARCHAR2
   )
   AS

      --Matt! 12/12/11

      --Looks like one of these three

      --1 0--X------
      --2 ------X--0

      --Turns into, respectively

      --1 ---0-----
      --2 ------0--

      node_id              NUMBER;
      move_pt              SDO_GEOMETRY;
      edge_geom            SDO_GEOMETRY;

   BEGIN


      node_id := GZ_TOPOFIX.GET_NODE_ID(p_topo,
                                        p_edge_id,
                                        p_tip);

      edge_geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo,
                                                p_edge_id);

      IF p_tip = 'START'
      THEN

         move_pt := SDO_GEOMETRY (
                                  2001,
                                  edge_geom.sdo_srid,
                                  SDO_POINT_TYPE(edge_geom.sdo_ordinates(3),
                                                 edge_geom.sdo_ordinates(4),
                                  NULL),
                                  NULL,
                                  NULL);

      ELSE --Remove end

         move_pt := SDO_GEOMETRY (
                                  2001,
                                  edge_geom.sdo_srid,
                                  SDO_POINT_TYPE(edge_geom.sdo_ordinates(edge_geom.sdo_ordinates.COUNT - 3),
                                                 edge_geom.sdo_ordinates(edge_geom.sdo_ordinates.COUNT - 2),
                                  NULL),
                                  NULL,
                                  NULL);

      END IF;


      --remove the vertex we just tucked away as move_pt
      GZ_TOPOFIX.REMOVE_SHORTY_VERTEX(p_topo,
                                      p_edge_id,
                                      p_shorty_index);

      --now move the node to that sweet spot

      GZ_TOPO_UTIL.GZ_MOVE_NODE(p_topo,
                                node_id,
                                move_pt);


   END RECONNECT_NODE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE ZAP_NODE (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   )
   AS

      --Matt! 12/12/11
      --Removes a node from the topology. Node must be attached to two edges
      --Then also removes all trace of the ghost vertex

      node_geom            SDO_GEOMETRY;
      edge_ids             GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);
      window_geom          SDO_GEOMETRY;
      create_it            NUMBER;
      remaining_edge_id    NUMBER;
      remaining_edge_geom  SDO_GEOMETRY;
      reshaped_edge_geom   SDO_GEOMETRY;
      new_ordinates        SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();
      ordkount             PLS_INTEGER := 0;
      skipvertex           PLS_INTEGER := 0;

   BEGIN

      node_geom := GET_NODE_GEOMETRY(p_topo,
                                     p_node_id);

      psql := 'SELECT e.edge_id '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.start_node_id = :p1 OR e.end_node_id = :p2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_ids USING p_node_id,
                                                              p_node_id;

      IF edge_ids.COUNT != 2
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Zap node expects node to be bound by 2 edges ');

      END IF;

      --set up topomap to cover the extent of both edges. This necessary?

      psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 OR '
           || 'e.edge_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO window_geom USING edge_ids(1),
                                                    edge_ids(2);

      --create the topomap with window

      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                   p_topo,
                                                   2,
                                                   window_geom.sdo_ordinates(1),
                                                   window_geom.sdo_ordinates(2),
                                                   window_geom.sdo_ordinates(3),
                                                   window_geom.sdo_ordinates(4)
                                                   );

      BEGIN

         SDO_TOPO_MAP.REMOVE_NODE(NULL,
                                  p_node_id);

      EXCEPTION
      WHEN OTHERS
      THEN

         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

         RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

      END;

      --commit and drop temp topomap
      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

      --one of the edges should now be gone, find which edge remains
      psql := 'SELECT e.edge_id '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 OR '
           || 'e.edge_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO remaining_edge_id USING edge_ids(1),
                                                          edge_ids(2);

      --Reshape the remaining edge to not include the position we saved

      remaining_edge_geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo,
                                                          remaining_edge_id);

      new_ordinates.EXTEND(remaining_edge_geom.sdo_ordinates.COUNT - 2);

      FOR i IN 1 .. remaining_edge_geom.sdo_ordinates.COUNT/2
      LOOP

         IF remaining_edge_geom.sdo_ordinates( (i*2) - 1 ) != node_geom.SDO_POINT.X
         AND remaining_edge_geom.sdo_ordinates(i*2) != node_geom.SDO_POINT.Y
         THEN

            ordkount := ordkount + 1;
            new_ordinates(ordkount) := remaining_edge_geom.sdo_ordinates( (i*2) - 1 );
            ordkount := ordkount + 1;
            new_ordinates(ordkount) := remaining_edge_geom.sdo_ordinates(i*2);

         ELSE

           skipvertex := skipvertex + 1;

         END IF;

      END LOOP;

      IF skipvertex != 1
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Didnt find a match');

      END IF;

      reshaped_edge_geom := remaining_edge_geom;
      reshaped_edge_geom.sdo_ordinates := new_ordinates;

      GZ_TOPO_UTIL.GZ_CHANGE_EDGE_COORDS(p_topo,
                                         remaining_edge_id,
                                         reshaped_edge_geom);


   END ZAP_NODE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE GZ_MOVE_EDGE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_from_node_id       IN NUMBER,
      p_to_node_id         IN NUMBER
   )
   AS

      --Matt! 12/12/11
      --Wrapper for sdo_topo_map.move_edge

      old_edge_geom        SDO_GEOMETRY;
      tip                  VARCHAR2(32);
      to_node_geom         SDO_GEOMETRY;
      newnumarray          SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY();
      ordkount             PLS_INTEGER := 0;
      psql                 VARCHAR2(4000);
      window_geom          SDO_GEOMETRY;
      create_it            NUMBER;


   BEGIN

      psql := 'SELECT ''START'' '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.start_node_id = :p1 AND '
           || 'e.edge_id = :p2 '
           || 'UNION ALL '
           || 'SELECT ''END'' '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.end_node_id = :p3 AND '
           || 'e.edge_id = :p4 ';

      EXECUTE IMMEDIATE psql INTO tip USING p_from_node_id, p_edge_id,
                                            p_from_node_id, p_edge_id;



      old_edge_geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo,
                                                    p_edge_id);

      to_node_geom := GZ_TOPOFIX.GET_NODE_GEOMETRY(p_topo,
                                                   p_to_node_id);

      newnumarray.EXTEND(old_edge_geom.sdo_ordinates.COUNT);

      FOR i IN 1 .. old_edge_geom.sdo_ordinates.COUNT/2
      LOOP

         IF tip = 'START'
         AND i = 1
         THEN

            --move start
            ordkount := ordkount + 1;
            newnumarray(ordkount) := to_node_geom.sdo_point.X;
            ordkount := ordkount + 1;
            newnumarray(ordkount) := to_node_geom.sdo_point.Y;

         ELSIF tip = 'END'
         AND i = old_edge_geom.sdo_ordinates.COUNT/2
         THEN

            --move end. I know, its the same
            ordkount := ordkount + 1;
            newnumarray(ordkount) := to_node_geom.sdo_point.X;
            ordkount := ordkount + 1;
            newnumarray(ordkount) := to_node_geom.sdo_point.Y;

         ELSE

            --pass thru
            ordkount := ordkount + 1;
            newnumarray(ordkount) := old_edge_geom.sdo_ordinates((i * 2) - 1);
            ordkount := ordkount + 1;
            newnumarray(ordkount) := old_edge_geom.sdo_ordinates(i * 2);

         END IF;

      END LOOP;

      --FOR i in 1 .. newnumarray.COUNT
      --LOOP
         --dbms_output.put_line(newnumarray(i));
      --END LOOP;

      psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || '(e.start_node_id = :p1 OR e.end_node_id = :p2) OR'
           || '(e.start_node_id = :p3 OR e.end_node_id = :p4) ';

      EXECUTE IMMEDIATE psql INTO window_geom
                                        USING p_from_node_id, p_from_node_id,
                                              p_to_node_id, p_to_node_id;

      IF window_geom.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Bad window. Bad. ');

      END IF;

      --create the topomap with window

      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                   p_topo,
                                                   2,
                                                   window_geom.sdo_ordinates(1),
                                                   window_geom.sdo_ordinates(2),
                                                   window_geom.sdo_ordinates(3),
                                                   window_geom.sdo_ordinates(4)
                                                   );

      BEGIN


         SDO_TOPO_MAP.MOVE_EDGE(NULL,
                                p_edge_id,
                                p_from_node_id,
                                p_to_node_id,
                                newnumarray);

      EXCEPTION
      WHEN OTHERS
      THEN

         --what do we expect here?
         --most likely "Moved edge coordinate string has an intersection with another edge"
         --also "Move edge cannot create loop edge"

         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

         RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

      END;

      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

   END GZ_MOVE_EDGE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE MERGE_NODES (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_zap_tip            IN VARCHAR2
   )
   AS

      --Matt! 12/12/11

      --begin with

      --1 --0---0--

      --2 --0---0--
      --        |

      --3 --0---0--
      --    |   |

      --end with, respectively

      --1 --0------

      --2 ---0---
      --     |

      --3 ---0---
      --     /\

      start_node_id     NUMBER;
      end_node_id       NUMBER;
      psql              VARCHAR2(4000);
      interior_edge_id  NUMBER;


   BEGIN


      start_node_id := GZ_TOPOFIX.GET_NODE_ID(p_topo,
                                              p_edge_id,
                                              'START');

      end_node_id := GZ_TOPOFIX.GET_NODE_ID(p_topo,
                                            p_edge_id,
                                            'END');

      --if the node to remove is not interior connected
      --we can just zap it

      IF p_zap_tip = 'START'
      AND IS_NODE_OBSOLETE(p_topo, start_node_id)
      THEN

         --expect error here if this node is registered as a segment-segment divide
         --in the state outline layer
         GZ_TOPOFIX.ZAP_NODE(p_topo,
                             start_node_id);

         --removing dead vertex taken care of by zap node

      ELSIF p_zap_tip = 'END'
      AND IS_NODE_OBSOLETE(p_topo, end_node_id)
      THEN

         --expect error here if this node is registered as a segment-segment divide
         --in the state outline layer
         GZ_TOPOFIX.ZAP_NODE(p_topo,
                             end_node_id);

         --removing dead vertex taken care of by zap node

      ELSE

         --more complicated
         --get the interior edge that is going to be moved
         --hopefully is the only edge connected to the zap node not on universal

         psql := 'SELECT e.edge_id '
              || 'FROM '
              || p_topo || '_edge$ e '
              || 'WHERE '
              || '(e.start_node_id = :p1 OR e.end_node_id = :p2) AND '
              || 'e.left_face_id != :p3 AND e.right_face_id != :p4 ';


         IF p_zap_tip = 'START'
         THEN

            EXECUTE IMMEDIATE psql INTO interior_edge_id USING start_node_id,
                                                               start_node_id,
                                                               -1,
                                                               -1;

            --move the interior edge to the keep node
            GZ_TOPOFIX.GZ_MOVE_EDGE(p_topo,
                                    interior_edge_id,
                                    start_node_id,
                                    end_node_id);

            --now essentially looks like our simple cases above
            --remove the node and vertex left behind in the dead spot
            GZ_TOPOFIX.ZAP_NODE(p_topo,
                                start_node_id);

         ELSIF p_zap_tip = 'END'
         THEN

            EXECUTE IMMEDIATE psql INTO interior_edge_id USING end_node_id,
                                                               end_node_id,
                                                               -1,
                                                               -1;

            --move the interior edge to the keep node
            GZ_TOPOFIX.GZ_MOVE_EDGE(p_topo,
                                    interior_edge_id,
                                    end_node_id,
                                    start_node_id);

            --now essentially looks like our simple cases above
            --move the interior edge to the keep node
            GZ_TOPOFIX.ZAP_NODE(p_topo,
                                end_node_id);

         END IF;

      END IF;


   END MERGE_NODES;


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE KEEP_MOST_ANGULAR (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_shorty_index       IN NUMBER,
      p_start_node_id      IN NUMBER,
      p_end_node_id        IN NUMBER,
      p_edge_vertex_count  IN NUMBER,
      p_ignore_edge        IN NUMBER DEFAULT NULL
   )
   AS

      --Matt! 12/06/11
      --For some edge on the universal face we have IDd a short index
      --The short segment could be node-node, node-vertex, vertex-vertex
      --Remove whichever one is least angular
      --If least angular is a node and the keeper is a vertex, or both are nodes, reattach
      --If both are nodes and reattach causes an error, error out and return error to caller for a slide
      --Added ignore_edge 12/14/11 for fake_face workaround

      old_edge_geom           SDO_GEOMETRY;
      short_segment           SDO_GEOMETRY;
      a_segment               SDO_GEOMETRY;
      b_segment               SDO_GEOMETRY;
      a_angle                 NUMBER;
      b_angle                 NUMBER;
      ring_flag               PLS_INTEGER := 0;


   BEGIN

      --figure out our segments for angle measure

      old_edge_geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo,
                                                    p_edge_id);

      short_segment := GZ_TOPOFIX.GET_SEGMENT_GEOM_FROM_INDEX(old_edge_geom,
                                                              p_shorty_index);

      --4 subcases within this case

      IF  p_shorty_index != 0
      AND p_shorty_index != (p_edge_vertex_count - 2)
      THEN

         --this is a vertex-vertex segment in the middle of an edge
         --all geometries run in the same direction

         a_segment := GZ_TOPOFIX.GET_SEGMENT_GEOM_FROM_INDEX(old_edge_geom,
                                                             (p_shorty_index - 1));

         b_segment := GZ_TOPOFIX.GET_SEGMENT_GEOM_FROM_INDEX(old_edge_geom,
                                                             (p_shorty_index + 1));

      ELSIF p_shorty_index = 0
      AND   p_edge_vertex_count = 2
      THEN

         --this is a node-node edge with no vertices, go to neighbors
         --will reverse segment geom if necessary to match edge

         a_segment := GZ_TOPOFIX.GET_NEXT_DIRECTED_SEGMENT(p_topo,
                                                           p_edge_id,
                                                           p_start_node_id,
                                                           'START',
                                                           p_ignore_edge);

         b_segment := GZ_TOPOFIX.GET_NEXT_DIRECTED_SEGMENT(p_topo,
                                                           p_edge_id,
                                                           p_end_node_id,
                                                           'END',
                                                           p_ignore_edge);

      ELSIF p_shorty_index = 0
      AND   p_edge_vertex_count > 2
      AND   p_start_node_id <> p_end_node_id
      THEN

         --this is a node-vertex edge with shorty at the start

         a_segment := GZ_TOPOFIX.GET_NEXT_DIRECTED_SEGMENT(p_topo,
                                                           p_edge_id,
                                                           p_start_node_id,
                                                           'START',
                                                           p_ignore_edge);

         b_segment := GZ_TOPOFIX.GET_SEGMENT_GEOM_FROM_INDEX(old_edge_geom,
                                                             1);


      ELSIF p_shorty_index = (p_edge_vertex_count - 2)
      AND   p_edge_vertex_count > 2
      AND   p_start_node_id <> p_end_node_id
      THEN

         --this is a vertex-node edge with shorty at the end

         a_segment := GZ_TOPOFIX.GET_SEGMENT_GEOM_FROM_INDEX(old_edge_geom,
                                                             (p_shorty_index - 1));

         b_segment := GZ_TOPOFIX.GET_NEXT_DIRECTED_SEGMENT(p_topo,
                                                           p_edge_id,
                                                           p_end_node_id,
                                                           'END',
                                                           p_ignore_edge);

      ELSIF p_start_node_id = p_end_node_id
      THEN

         --ring true
         --on an island ring the node is always the winner regardless of angle
         ring_flag := 1;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Unknown keep_most_angular configuration');

      END IF;


      IF ring_flag = 0
      THEN

         --order of inputs must be a_segment then short_segment
         a_angle := GZ_TOPOFIX.CALC_SEGMENT_ANGLE(a_segment,
                                                  short_segment);

         --order of inputs must be short_segment then b_segment
         b_angle := GZ_TOPOFIX.CALC_SEGMENT_ANGLE(short_segment,
                                                  b_segment);

      END IF;


      --2 paths from here
      --either remove a vertex
      --or move a node to a vertex, deleting its ghost

      IF ( p_shorty_index != 0 AND p_shorty_index != (p_edge_vertex_count - 2) )                          --vertex-vertex always
      OR ( p_shorty_index = 0 AND p_edge_vertex_count > 2 AND b_angle < a_angle )                         --node-vertex, remove vertex
      OR ( p_shorty_index = (p_edge_vertex_count - 2) AND p_edge_vertex_count > 2 AND a_angle > b_angle ) --vertex node, vertex goes poof cause its more straight
      THEN

         --remove a vertex, pretty simple

         IF p_shorty_index != 0
         AND p_shorty_index != (p_edge_vertex_count - 2)
         AND b_angle > a_angle
         THEN

            --REMOVE_SHORTY_VERTEX will skip the vertex before (index +1) the index
            --so in this one case advance the index to the next to nuke the "before" vertex
            GZ_TOPOFIX.REMOVE_SHORTY_VERTEX(p_topo,
                                            p_edge_id,
                                            (p_shorty_index+1));

         ELSE

            --everybody else should work as is
            GZ_TOPOFIX.REMOVE_SHORTY_VERTEX(p_topo,
                                            p_edge_id,
                                            p_shorty_index);

         END IF;

      ELSIF p_shorty_index = 0
      AND p_edge_vertex_count = 2
      THEN

         --this is node-node
         --requires special care. Most likely will be moving an edge, not a node, in Oracle topo speak

         IF a_angle < b_angle
         THEN

            GZ_TOPOFIX.MERGE_NODES(p_topo,
                                   p_edge_id,
                                   'START');  --nuke start

         ELSE

            GZ_TOPOFIX.MERGE_NODES(p_topo,
                                   p_edge_id,
                                   'END');  --nuke end

         END IF;

      ELSE

         --some sort of vertex-node combo. Move the node to the vertex

         --IF a_angle < b_angle --what does angle have to do with the decision?

         IF p_shorty_index = 0
         AND ring_flag = 0
         THEN

            GZ_TOPOFIX.RECONNECT_NODE(p_topo,
                                      p_edge_id,
                                      p_shorty_index,
                                      'START');

         ELSIF ring_flag = 0
         THEN

            GZ_TOPOFIX.RECONNECT_NODE(p_topo,
                                      p_edge_id,
                                      p_shorty_index,
                                      'END');

         ELSIF ring_flag = 1
         THEN

             --vertex-node combo but the node can't, and shouldn't, be moved
             --just remove the vertex
             GZ_TOPOFIX.REMOVE_SHORTY_VERTEX(p_topo,
                                             p_edge_id,
                                             p_shorty_index);

         END IF;

      END IF;


   END KEEP_MOST_ANGULAR;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE SLIDE_NODE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_node_id            IN NUMBER,
      p_distance           IN NUMBER DEFAULT .05
   )
   AS

      --Matt! 12/12/11
      --Assumption is that this is a universal facing face and node

      psql                    VARCHAR2(4000);
      neighbor_edge_id        NUMBER;
      neighbor_edge_geom      SDO_GEOMETRY;
      neighbor_start_node_id  NUMBER;
      neighbor_end_node_id    NUMBER;
      tucked_srid             NUMBER;
      move_pt                 SDO_GEOMETRY;

   BEGIN

      --get the next edge on the universal face
      --we will be taking a neighborly chunk of his geometric flesh

      psql := 'SELECT e.edge_id, e.geometry, e.start_node_id, e.end_node_id '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id != :p1 AND '
           || '(e.start_node_id = :p2 OR e.end_node_id = :p3) AND '
           || '(e.left_face_id = :p4 OR e.right_face_id = :p5)';

      EXECUTE IMMEDIATE psql INTO neighbor_edge_id,
                                  neighbor_edge_geom,
                                  neighbor_start_node_id,
                                  neighbor_end_node_id USING p_edge_id,
                                                             p_node_id,
                                                             p_node_id,
                                                             -1,
                                                             -1;

      IF neighbor_edge_id IS NULL
      THEN

          RAISE_APPLICATION_ERROR(-20001,'Couldnt find a neighbor');

      END IF;

      --Still cant trust any LRS in geodetic?
      --Actually move node is only working in geodetic LRS calculations
      --Using the LRS NULL srid location for the move point is throwing
      --Node is not moved to a position inside one of the original adjoining faces
      --   Since the node is slightly outside the universal face



      IF neighbor_start_node_id = p_node_id
      THEN

         move_pt := GZ_GEOM_UTILS.GZ_LOCATE_PT_DISTANCE(neighbor_edge_geom,
                                                       p_distance,  --distance
                                                       'START',
                                                       p_distance); --tolerance

      ELSIF neighbor_end_node_id = p_node_id
      THEN

         move_pt := GZ_GEOM_UTILS.GZ_LOCATE_PT_DISTANCE(neighbor_edge_geom,
                                                       p_distance,  --distance
                                                       'END',
                                                       p_distance); --tolerance

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Wtf? Check SQL');

      END IF;


      GZ_TOPO_UTIL.GZ_MOVE_NODE(p_topo,
                                p_node_id,
                                move_pt);

      --what else?


   END SLIDE_NODE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE SLIDE_LEAST_ANGULAR_NODE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_start_node_id      IN NUMBER,
      p_end_node_id        IN NUMBER,
      p_distance           IN NUMBER DEFAULT .05
   )
   AS

      --Matt! 12/12/11
      --We are here on a degenerating fix case
      --backup plan is to slide the nodes apart
      --At present the only caller to this code throws in a node-node (vertex-free) edge
      --   so will enforce that for now

      edge_geom               SDO_GEOMETRY;
      a_segment               SDO_GEOMETRY;
      b_segment               SDO_GEOMETRY;
      a_angle                 NUMBER;
      b_angle                 NUMBER;

   BEGIN

      edge_geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo,
                                                p_edge_id);

      IF edge_geom.sdo_ordinates.COUNT != 4
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Unexpected inputs, should be a simple node-node edge');
      END IF;


      --will reverse segment geom if necessary to match edge

      a_segment := GZ_TOPOFIX.GET_NEXT_DIRECTED_SEGMENT(p_topo,
                                                        p_edge_id,
                                                        p_start_node_id,
                                                        'START');

      b_segment := GZ_TOPOFIX.GET_NEXT_DIRECTED_SEGMENT(p_topo,
                                                        p_edge_id,
                                                        p_end_node_id,
                                                        'END');

      --order of inputs must be a_segment then short_segment
      a_angle := GZ_TOPOFIX.CALC_SEGMENT_ANGLE(a_segment,
                                               edge_geom);

      --order of inputs must be short_segment then b_segment
      b_angle := GZ_TOPOFIX.CALC_SEGMENT_ANGLE(edge_geom,
                                               b_segment);

      --slide the node
      IF a_angle < b_angle
      THEN

         GZ_TOPOFIX.SLIDE_NODE(p_topo,
                               p_edge_id,
                               p_start_node_id,
                               p_distance);

      ELSE

         GZ_TOPOFIX.SLIDE_NODE(p_topo,
                               p_edge_id,
                               p_end_node_id,
                               p_distance);

      END IF;


   END SLIDE_LEAST_ANGULAR_NODE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE LENGTHEN_SHORTY (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_index              IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05
   )
   AS

      --Matt! 11/28/11
      --We have a problem with index p_index on this edge
      --This edge is just like 0--X-------0   3 total vertexes
      --But we cant just remove the offending vertex, the edge/face would degenerate
      --so instead stretch the offending shorty segment until it is less shorty

      old_edge_geom        SDO_GEOMETRY;
      new_edge_geom        SDO_GEOMETRY;
      new_ordinates        SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();
      ordkount             PLS_INTEGER := 0;
      skip_i               PLS_INTEGER;
      new_vertex           SDO_GEOMETRY;
      percent_stretch      NUMBER;

   BEGIN

      old_edge_geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo,
                                                    p_edge_id);

      IF (p_index != 0 AND p_index != 1)
      OR old_edge_geom.sdo_ordinates.COUNT != 6
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, I only know how to handle 2 segment edges');

      END IF;

      IF old_edge_geom.sdo_srid != 8265
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, 8265  only');

      ELSE

         percent_stretch := 200;

      END IF;

      --lets just call a sub to return the new point

      new_vertex := GZ_TOPOFIX.PROJECT_V_TIP(old_edge_geom,
                                             p_index,
                                             percent_stretch,   --amount we want to stretch. 200 percent, double the shorty
                                             p_tolerance);  --tolerance, not really used


      new_ordinates.EXTEND(old_edge_geom.sdo_ordinates.COUNT);

      FOR i IN 1 .. old_edge_geom.sdo_ordinates.COUNT/2
      LOOP

         IF i != 2
         THEN

            ordkount := ordkount + 1;
            new_ordinates(ordkount) := old_edge_geom.sdo_ordinates((i*2) - 1);
            ordkount := ordkount + 1;
            new_ordinates(ordkount) := old_edge_geom.sdo_ordinates(i*2);

         ELSE

            --switcherino

            ordkount := ordkount + 1;
            new_ordinates(ordkount) := new_vertex.sdo_point.X;
            ordkount := ordkount + 1;
            new_ordinates(ordkount) := new_vertex.sdo_point.Y;

         END IF;

      END LOOP;

      new_edge_geom := old_edge_geom;
      new_edge_geom.sdo_ordinates := new_ordinates;

      GZ_TOPO_UTIL.GZ_CHANGE_EDGE_COORDS(p_topo,
                                         p_edge_id,
                                         new_edge_geom);

   END LENGTHEN_SHORTY;

    ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

    FUNCTION ADD_FAKE_FACE (
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER,
      p_index           IN NUMBER,
      p_face_tab        IN VARCHAR2,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN NUMBER
   AS

      --Matt! 2/14
      --Copied into gz_topofix and modified 12/14/11
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
      edge_geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo, p_edge_id);


      IF edge_geom.sdo_ordinates.EXISTS(((p_index + 2) * 2) + 6)
      THEN

         new_node_1 := SDO_GEOMETRY(2001,
                                    edge_geom.sdo_srid,
                                    SDO_POINT_TYPE(1,  --garbage
                                                   2,
                                                   NULL),
                                    NULL,NULL);

         --we will go two vertexes forward for node1 --> (((p_index + 2) * 2) + 3)
         --but must check 3 vertices forward, can't add a node on the final vertex --> (((p_index + 2) * 2) + 6)

         new_node_1.sdo_point.X :=  edge_geom.sdo_ordinates(((p_index + 2) * 2) + 3);
         new_node_1.sdo_point.Y :=  edge_geom.sdo_ordinates(((p_index + 2) * 2) + 4);

         --Gots a wrapper for add_node in gz_clip but its messy and complex

         new_node_1_id := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                                p_edge_id,
                                                new_node_1,
                                                p_index + 2,  --two coord indexes
                                                'FALSE');



      ELSE

         --just get the end node
         psql := 'SELECT e.end_node_id '
              || 'FROM '
              || p_topo || '_edge$ e '
              || 'WHERE e.edge_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO new_node_1_id USING p_edge_id;

         new_node_1 := GZ_TOPOFIX.GET_NODE_GEOMETRY(p_topo, new_node_1_id);

      END IF;



      IF edge_geom.sdo_ordinates.EXISTS(((p_index + 1) * 2) - 7)
      THEN

         new_node_2 := SDO_GEOMETRY(2001,
                                    edge_geom.sdo_srid,
                                    SDO_POINT_TYPE(1,  --garbage
                                                   2,
                                                   NULL),
                                    NULL,NULL);

         --go two vertices in the opposite direction for number 2
         --check three vertices over however


         new_node_2.sdo_point.X :=  edge_geom.sdo_ordinates(((p_index + 1) * 2) - 5);
         new_node_2.sdo_point.Y :=  edge_geom.sdo_ordinates(((p_index + 1) * 2) - 4);

         new_node_2_id := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                                p_edge_id,
                                                new_node_2,
                                                (p_index - 2),
                                                'FALSE');



      ELSE

         --just get the start node
         psql := 'SELECT e.start_node_id '
              || 'FROM '
              || p_topo || '_edge$ e '
              || 'WHERE e.edge_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO new_node_2_id USING p_edge_id;

         new_node_2 := GZ_TOPOFIX.GET_NODE_GEOMETRY(p_topo, new_node_2_id);


      END IF;


      --make up a new point out in space
      --amateur-style

      dbms_output.put_line('node1: ');
      dbms_output.put_line(to_char(GZ_GEOM_UTILS.DUMP_SDO(new_node_1)));
      dbms_output.put_line('node2: ');
      dbms_output.put_line(to_char(GZ_GEOM_UTILS.DUMP_SDO(new_node_2)));

      --this feels wrong
      GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(p_face_tab,
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


      --get the node geoms again, cant trust that HE put them in as we requested
      new_node_1 := GZ_TOPOFIX.GET_NODE_GEOMETRY(p_topo, new_node_1_id);
      new_node_2 := GZ_TOPOFIX.GET_NODE_GEOMETRY(p_topo, new_node_2_id);

      new_edge_geom := edge_geom;
      new_ordinates.EXTEND(6);
      new_ordinates(1) := new_node_1.sdo_point.X;
      new_ordinates(2) := new_node_1.sdo_point.Y;
      new_ordinates(3) := new_pt_geom.sdo_point.X;
      new_ordinates(4) := new_pt_geom.sdo_point.Y;
      new_ordinates(5) := new_node_2.sdo_point.X;
      new_ordinates(6) := new_node_2.sdo_point.Y;

      new_edge_geom.sdo_ordinates := new_ordinates;

      --wrapper?
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

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

    FUNCTION GET_TEMP_EDGE(
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 2/14/11
      --copied and modified from gz_utilities 12/14/12
      --This is called in the add_fake_face code workaround for the oracle edge self-intersects bug
      --We have added a new edge into the universal face and we know its id
      --The new edge forms a cone overtop of the original edge we were trying to fix
      --We need to figure out the edge_id of that edge to fix, it may have changed

      --          x
      --         / \
      --        /   \
      --       /     \
      --      /       \
      --  ---0---x-x---0--   <--- ? edge_id

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
   --Public-------------------------------------------------------------------------

   PROCEDURE REMOVE_FAKE_FACE (
      p_topo            IN VARCHAR2,
      p_face_feat_tab   IN VARCHAR2
   )
   AS

      --Matt! 2/14/11
      --Copied from gz_utilities 12/14/11
      --we are cleaning up from a successful or failed add_fake_face
      --workaround due to the oracle self intersects bug

      psql        VARCHAR2(4000);
      face_id     NUMBER;
      edje        NUMBER;
      nodes       GZ_TYPES.stringarray;
      nodekounts  GZ_TYPES.stringarray;
      kount       PLS_INTEGER;

   BEGIN


      --our new fake face has no feature face
      psql := 'SELECT a.face_id FROM ' || p_topo || '_face$ a '
           || 'WHERE a.face_id != :p1 '
           || 'MINUS '
           || 'select b.face_id FROM ' || p_face_feat_tab || '  b ';

      EXECUTE IMMEDIATE psql INTO face_id USING -1;


      --new edge is the only edge on the universal for this face

      psql := 'SELECT e.edge_id FROM '
           || p_topo || '_edge$ e '
           || 'WHERE (e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
           || '(e.left_face_id = :p3 OR e.right_face_id = :p4) ';

      EXECUTE IMMEDIATE psql INTO edje USING face_id, face_id,
                                             -1, -1;

      --get start and end nodes, dont care which is which
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

         --otherwise, leave him

      END LOOP;


   END REMOVE_FAKE_FACE;


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE SELF_INTERSECTS_WORKAROUND (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_index              IN NUMBER,
      p_face_tab           IN VARCHAR2,
      p_tolerance          IN NUMBER DEFAULT .05
   )
   AS

      --Matt! 12/14/11
      --Trying to get this add_Fake_face workaround for the oracle self-intersects bug
      --into a single procedure
      --could potentially be called standalone by and ADE-user
      --That said, this is extremely klugey and unpolished

      fake_edge_id         NUMBER;
      temp_edge_id         NUMBER;
      temp_start_node_id   NUMBER;
      temp_end_node_id     NUMBER;
      psql                 VARCHAR2(4000);
      temp_index           NUMBER;
      temp_edge_geom       SDO_GEOMETRY;

   BEGIN

      --add the cone
      fake_edge_id := GZ_TOPOFIX.ADD_FAKE_FACE(p_topo,
                                               p_edge_id,
                                               p_index,
                                               p_face_tab,
                                               p_tolerance);

      --get the edge id to work on
      temp_edge_id := GZ_TOPOFIX.GET_TEMP_EDGE(p_topo,
                                               fake_edge_id);

      temp_edge_geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo,
                                                     temp_edge_id);

      psql := 'SELECT e.start_node_id, e.end_node_id '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO temp_start_node_id,
                                  temp_end_node_id USING temp_edge_id;

      temp_index := GZ_TOPOFIX.GET_SHORT_SEGMENT_INDEX(p_topo, temp_edge_id);



      --try again, with new edge id
      BEGIN

         GZ_TOPOFIX.KEEP_MOST_ANGULAR(p_topo,
                                      temp_edge_id,
                                      temp_index,
                                      temp_start_node_id,
                                      temp_end_node_id,
                                      temp_edge_geom.sdo_ordinates.COUNT/2,
                                      fake_edge_id);  --special override

      EXCEPTION
      WHEN OTHERS THEN

         --clean up our cone of junk no matter what
         GZ_TOPOFIX.REMOVE_FAKE_FACE(p_topo,
                                     p_face_tab);


         RAISE_APPLICATION_ERROR(-20001,'Self intersects workaround failure: '
                                         || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

      END;

      GZ_TOPOFIX.REMOVE_FAKE_FACE(p_topo,
                                  p_face_tab);

      --we dont care about any of the remaining face ids, whatevs


   END SELF_INTERSECTS_WORKAROUND;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE REMOVE_OBSOLETE_NODE (
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER
   )
   AS

      --Matt! 12/27/11
      --Called from Hydra.  Wrapper for zap_node
      --Expect just one node to be obsolete on this edge

      psql           VARCHAR2(4000);
      start_node_id  NUMBER;
      end_node_id    NUMBER;

   BEGIN

      psql := 'SELECT e.start_node_id, e.end_node_id '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO start_node_id,
                                  end_node_id USING p_edge_id;

      IF IS_NODE_OBSOLETE(p_topo, start_node_id)
      AND NOT IS_NODE_OBSOLETE(p_topo, end_node_id)
      THEN

         --zap_node will remove the node and reshape the edge without the vtx

         GZ_TOPOFIX.ZAP_NODE(p_topo,
                             start_node_id);

      ELSIF NOT IS_NODE_OBSOLETE(p_topo, start_node_id)
      AND IS_NODE_OBSOLETE(p_topo, end_node_id)
      THEN

         GZ_TOPOFIX.ZAP_NODE(p_topo,
                             end_node_id);

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Unknown config, only one node should be obsolete');

      END IF;


   END REMOVE_OBSOLETE_NODE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION CLOCKWISE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_face_id            IN NUMBER
   ) RETURN BOOLEAN
   AS

      psql                 VARCHAR2(4000);
      clockw               VARCHAR2(32);

   BEGIN

      --Get direction of our bad edge relative to our passed in face

      psql := 'SELECT ''COUNTERCLOCKWISE'' FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 AND '
           || 'e.left_face_id = :p2 '
           || 'UNION ALL '
           || 'SELECT ''CLOCKWISE'' FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p3 AND '
           || 'e.right_face_id = :p4 ';

      EXECUTE IMMEDIATE psql INTO clockw USING p_edge_id,
                                               p_face_id,
                                               p_edge_id,
                                               p_face_id;

      IF clockw = 'CLOCKWISE'
      THEN

         RETURN TRUE;

      ELSIF clockw = 'COUNTERCLOCKWISE'
      THEN

         RETURN FALSE;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Messed up inputs or topio');

      END IF;

   END CLOCKWISE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE FIX_INTERIOR_DUPE (
      p_topo                     IN VARCHAR2,
      p_edge_id                  IN NUMBER,
      p_start_node_id            IN NUMBER,
      p_end_node_id              IN NUMBER
   )
   AS

      --Matt! 1/3/12

      psql                 VARCHAR2(4000);
      face_id              NUMBER;
      change_edge_id       NUMBER;
      keep_edge_id         NUMBER;
      change_edge_id_bak   NUMBER;
      keep_edge_id_bak     NUMBER;
      silly_num_array      SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY();
      shorty_edge_geom     SDO_GEOMETRY;
      ordkount             PLS_INTEGER := 0;


   BEGIN

      shorty_edge_geom := GZ_TOPOFIX.GET_EDGE_GEOMETRY(p_topo, p_edge_id);

      IF shorty_edge_geom.SDO_ORDINATES.COUNT != 4
      THEN

         --standard crux, expect a shorty edge
         RAISE_APPLICATION_ERROR(-20001, 'Hey shorty, thats not a shorty!');

      END IF;

      --The logic below about which to keep and which to move isnt really important.
      --What is important is getting the correct primitives to choose from for keep and move
      --since in this interior universe there are edges attached to the bad edge that are NOT in play at all

      --Get the face on left side of the bad edge, could just as easily do the right

      psql := 'SELECT e.left_face_id FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO face_id USING p_edge_id;

      --left face id means we are on this face running counterclockwise
      --I drew this so its T R U E
      --relative to this edge and face, we will call the remove edge its "next right edge"
      --the keep edge is the "next left edge"


      psql := 'SELECT '
           || 'ABS(e.next_right_edge_id), ABS(e.next_left_edge_id), '
           || 'ABS(e.prev_left_edge_id), ABS(e.prev_right_edge_id) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 ';

      --oops not using the keeps at the moment
      EXECUTE IMMEDIATE psql INTO change_edge_id,
                                  keep_edge_id,
                                  change_edge_id_bak,
                                  keep_edge_id_bak USING p_edge_id;


      BEGIN

         GZ_TOPOFIX.GZ_MOVE_EDGE(p_topo,
                                 change_edge_id,
                                 p_start_node_id,
                                 p_end_node_id);

      EXCEPTION
      WHEN OTHERS
      THEN

         IF UPPER(SQLERRM) LIKE '%HAS AN INTERSECTION WITH ANOTHER EDGE%'
         THEN

            --If we chose to start with the edge that results in an intersection at the interim
            --move edge stage, then just use the other edge

            --Heres a horrible pic attempt
            --
            --      |                              |                       |/----
            --    --0--0---  good interim-->  --0--0---  bad interim-->  --0--0
            --         |                           |                          |


            GZ_TOPOFIX.GZ_MOVE_EDGE(p_topo,
                                    change_edge_id_bak,
                                    p_start_node_id,
                                    p_end_node_id);
         ELSE

            RAISE;

         END IF;

      END;

      GZ_TOPOFIX.ZAP_NODE(p_topo, p_start_node_id);


   END FIX_INTERIOR_DUPE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE REMOVE_INTERIOR_VERTEX (
      p_topo                     IN VARCHAR2,
      p_edge_id                  IN NUMBER,
      p_shorty_index             IN NUMBER
   )
   AS

      --Matt! 1/3/11
      --Much simpler than the exterior guys
      --no concerns about angles and nodes here, weve IDd and interior node-vtx combo
      --just go straight to the low level vertex removal utility


   BEGIN

      GZ_TOPOFIX.REMOVE_SHORTY_VERTEX(p_topo,
                                      p_edge_id,
                                      p_shorty_index);



   END REMOVE_INTERIOR_VERTEX;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GET_LINEAR_FEATURE_TABS (
      p_topo                  IN VARCHAR2,
      p_tg_layer_level        IN NUMBER DEFAULT 0
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 11/08/12
      --Build in some GZ assumptions

      psql              VARCHAR2(4000);
      output            GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT a.table_name FROM user_sdo_topo_info a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.column_name = :p2 AND '
           || 'a.tg_layer_type = :p3 AND '
           || 'a.tg_layer_level = :p4 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_topo),
                                                            'TOPOGEOM',
                                                            'LINE',
                                                            p_tg_layer_level;

      --Going to allow this to return empty. Caller beware
      RETURN output;

   END GET_LINEAR_FEATURE_TABS;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   PROCEDURE ANNIHILATE_LINEAR_FEATURES (
      p_topo                  IN VARCHAR2,
      p_edge_id               IN VARCHAR2
   )
   AS

      --Matt! 11/08/12

      --no counting of how many edges per feature
      --Just delete from the feature table where it has the edge
      --The state outline table should always just delete nothing
      --Not doing it this way for a good reason
      --But because I know nothing about the feature tables in here - column
      --   names, relationship to the edges, etc
      --Making assumptions from the caller that this is OK

      line_feature_tables           GZ_TYPES.stringarray;
      kount                         PLS_INTEGER;
      psql                          VARCHAR2(4000);

   BEGIN

      line_feature_tables := GZ_TOPOFIX.GET_LINEAR_FEATURE_TABS(p_topo);

      FOR i IN 1 .. line_feature_tables.COUNT
      LOOP


         psql := 'DELETE FROM ' || line_feature_tables(i) || ' a '
              || 'WHERE EXISTS '
              || '(SELECT * FROM '
              || p_topo || '_relation$ r, '
              || p_topo || '_edge$ e '
              || 'WHERE '
              || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
              || 'r.tg_id = a.topogeom.tg_id AND '
              || 'e.edge_id = r.topo_id AND '
              || 'e.edge_id = :p1 '
              || ')';

         EXECUTE IMMEDIATE psql USING p_edge_id;

         COMMIT;

      END LOOP;


   END ANNIHILATE_LINEAR_FEATURES;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   PROCEDURE GZ_TOPOFIX_REMOVE_EDGE (
      p_topo                  IN VARCHAR2,
      p_edge_id               IN VARCHAR2,
      p_delete_features       IN VARCHAR2 DEFAULT 'N',
      p_depth                 IN NUMBER DEFAULT 0
   )
   AS

      --Matt! 11/06/12
      --Need a wrapper for edge removal
      --In order to deal with clip topos where edges are features
      --Forget it, this is not a generic wrapper
      --Assumes that if the edge we want to delete has an assigned linear feature
      --we may simply delete the feature. This is the case in the cancer configurion,
      --which is the only caller right now

   BEGIN

      IF p_depth > 1
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Shouldnt be more than 1 recursion');

      END IF;

      IF p_delete_features = 'Y'
      THEN

         GZ_TOPOFIX.ANNIHILATE_LINEAR_FEATURES(p_topo,
                                               p_edge_id);

      END IF;

      BEGIN

         SDO_TOPO_MAP.REMOVE_EDGE(p_topo,
                                  p_edge_id);

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLERRM LIKE '%has assigned feature%'
         THEN



            --This can happen in clip when the _ewrk feature table is still registered
            --Java call terminated by uncaught Java exception: oracle.spatial.topo.InvalidTopoOperationException:
            --Edge ID 43213 has assigned feature(s) and cannot be removed

            GZ_TOPOFIX.GZ_TOPOFIX_REMOVE_EDGE(p_topo,
                                              p_edge_id,
                                              'Y',
                                              (p_depth + 1));


         ELSE

            RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

         END IF;

      END;

   END GZ_TOPOFIX_REMOVE_EDGE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE DROP_Y_SHAPE (
      p_topo                     IN VARCHAR2,
      p_edge_id                  IN NUMBER,
      p_start_node_id            IN NUMBER,
      p_end_node_id              IN NUMBER,
      p_debug                    IN NUMBER DEFAULT NULL
   )
   AS

      --Matt! 1/23/12
      --! 11/08/12 Added handling for CL module registered feature tables on edges
      --Constellation cancer looks like a Y up against the universal face
      --Move each arm of the Y to the universal
      --Remove the dangling edge
      --Remove the dangling node
      --Its similar to GZ_CLIP.extend_dangling_v_nodes except the first couple of steps are already done

      interior_node_id        NUMBER;
      universal_node_id       NUMBER;
      interior_edge_ids       GZ_TYPES.stringarray;
      psql                    VARCHAR2(4000);
      fail_move_kount         PLS_INTEGER := 0;

   BEGIN

      --figure out which node is at the Y

      IF IS_NODE_UNIVERSAL(p_topo,p_start_node_id)
      AND NOT IS_NODE_UNIVERSAL(p_topo, p_end_node_id)
      THEN

         universal_node_id := p_start_node_id;
         interior_node_id  := p_end_node_id;

      ELSIF IS_NODE_UNIVERSAL(p_topo, p_end_node_id)
      AND NOT IS_NODE_UNIVERSAL(p_topo, p_start_node_id)
      THEN

         universal_node_id := p_end_node_id;
         interior_node_id  := p_start_node_id;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Yo, config is messed up');

      END IF;


      --get the two edges connected

      psql := 'SELECT e.edge_id '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE '
           || '(e.start_node_id = :p1 OR e.end_node_id = :p2) AND '
           || 'e.edge_id != :p3 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO interior_edge_ids USING interior_node_id,
                                                                       interior_node_id,
                                                                       p_edge_id;

      IF interior_edge_ids.COUNT != 2
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Y shape should only have 2 arms ');

      END IF;

      FOR i IN 1 .. interior_edge_ids.COUNT
      LOOP


         BEGIN

            GZ_TOPOFIX.GZ_MOVE_EDGE(p_topo,
                                    interior_edge_ids(i),
                                    interior_node_id,
                                    universal_node_id);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF p_debug = 1
            THEN
               dbms_output.put_line('edge ' || interior_edge_ids(i) || ' '||  SQLERRM);
            END IF;

            IF SQLERRM LIKE '%MOVED EDGE COORDINATE STRING HAS AN INTERSECTION%'
            THEN

               fail_move_kount := fail_move_kount + 1;

            ELSE

               --what the heck
               fail_move_kount := fail_move_kount + 1;

            END IF;

         END;

      END LOOP;


      IF fail_move_kount = 2
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Fail, couldnt move either interior edge to outline');

      ELSIF fail_move_kount = 1
      THEN

         --should be able to zap the node since now just 1 edge uses it
         --Will remove the original dupe problem
         GZ_TOPOFIX.ZAP_NODE(p_topo, interior_node_id);

      ELSE

         --moved both
         --call our special remove edge code
         --will delete the _ewrk feature if we are in clip
         GZ_TOPOFIX.GZ_TOPOFIX_REMOVE_EDGE(p_topo,p_edge_id);

         --zap_node wont work, expects an edge on each side and a vtx to remove
         SDO_TOPO_MAP.REMOVE_NODE(p_topo,interior_node_id);

      END IF;



   END DROP_Y_SHAPE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION FIX_CONSTELLATION (
      p_topo               IN VARCHAR2,
      p_face_tab           IN VARCHAR2,   --NONE means call from the edge fixer
      p_constellation      IN GZ_TYPES.TOPOFIX_CONSTELLATION,
      p_hold_universal     IN VARCHAR2 DEFAULT 'Y',
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN VARCHAR2
   AS

      --Matt! 11/28/11
      --return TRUE or an Error Message

      output               VARCHAR2(32) := 'FALSE';



   BEGIN

      IF p_constellation.fix_class = 1
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'There should be no Virgos in generalization');

      ELSIF p_constellation.fix_class = 2
      THEN

         --Aries
         --Never encountered or tested

         BEGIN

            GZ_TOPOFIX.KEEP_MOST_ANGULAR(p_topo,
                                         p_constellation.edge_id,
                                         p_constellation.shorty_index,
                                         p_constellation.edge_start_node_id,
                                         p_constellation.edge_end_node_id,
                                         p_constellation.edge_vertex_count);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF UPPER(SQLERRM) LIKE '%ADD FAKE FACE%'
            AND p_face_tab <> 'NONE'
            THEN

               --RAISE_APPLICATION_ERROR(-20001,'add fake face on edge id ' || p_constellation.edge_id);

               --this is my error for the oracle bug
               --'%CHANGED EDGE COORDINATE STRING SELF-INTERSECTS%'
               --on the universal face
               --Call our special handler

               GZ_TOPOFIX.SELF_INTERSECTS_WORKAROUND(p_topo,
                                                     p_constellation.edge_id,
                                                     p_constellation.shorty_index,
                                                     p_face_tab,
                                                     p_tolerance);

            ELSE


               RAISE_APPLICATION_ERROR(-20001,p_constellation.fix_constellation
                                           || 'Aries unhandled exception ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;

      ELSIF p_constellation.fix_class = 3 --Libra
      THEN


         BEGIN

            GZ_TOPOFIX.KEEP_MOST_ANGULAR(p_topo,
                                         p_constellation.edge_id,
                                         p_constellation.shorty_index,
                                         p_constellation.edge_start_node_id,
                                         p_constellation.edge_end_node_id,
                                         p_constellation.edge_vertex_count);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF UPPER(SQLERRM) LIKE '%ADD FAKE FACE%'
            AND p_face_tab <> 'NONE'
            THEN

               --RAISE_APPLICATION_ERROR(-20001,'add fake face on edge id ' || p_constellation.edge_id);

               --this is my error for the oracle bug
               --'%CHANGED EDGE COORDINATE STRING SELF-INTERSECTS%'
               --on the universal face
               --Call our special handler

               GZ_TOPOFIX.SELF_INTERSECTS_WORKAROUND(p_topo,
                                                     p_constellation.edge_id,
                                                     p_constellation.shorty_index,
                                                     p_face_tab,
                                                     p_tolerance);

            ELSE


               RAISE_APPLICATION_ERROR(-20001,p_constellation.fix_constellation
                                           || 'Libra unhandled exception ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;

      ELSIF p_constellation.fix_class = 4
      THEN

          --UrsaMinor
          --Never seen or tested this code


         BEGIN

            GZ_TOPOFIX.KEEP_MOST_ANGULAR(p_topo,
                                         p_constellation.edge_id,
                                         p_constellation.shorty_index,
                                         p_constellation.edge_start_node_id,
                                         p_constellation.edge_end_node_id,
                                         p_constellation.edge_vertex_count);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF UPPER(SQLERRM) LIKE '%ADD FAKE FACE%'
            AND p_face_tab <> 'NONE'
            THEN

               --RAISE_APPLICATION_ERROR(-20001,'add fake face on edge id ' || p_constellation.edge_id);

               --this is my error for the oracle bug
               --'%CHANGED EDGE COORDINATE STRING SELF-INTERSECTS%'
               --on the universal face
               --Call our special handler

               GZ_TOPOFIX.SELF_INTERSECTS_WORKAROUND(p_topo,
                                                     p_constellation.edge_id,
                                                     p_constellation.shorty_index,
                                                     p_face_tab,
                                                     p_tolerance);

            ELSE


               RAISE_APPLICATION_ERROR(-20001,p_constellation.fix_constellation
                                           || 'Ursa Minor unhandled exception ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;

      ELSIF p_constellation.fix_class = 5 --Lepus
      THEN


         BEGIN

            GZ_TOPOFIX.KEEP_MOST_ANGULAR(p_topo,
                                         p_constellation.edge_id,
                                         p_constellation.shorty_index,
                                         p_constellation.edge_start_node_id,
                                         p_constellation.edge_end_node_id,
                                         p_constellation.edge_vertex_count);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF UPPER(SQLERRM) LIKE '%MOVE EDGE CANNOT CREATE LOOP EDGE%'
            OR UPPER(SQLERRM) LIKE '%MOVED EDGE COORDINATE STRING HAS AN INTERSECTION WITH ANOTHER EDGE%'
            THEN

               --slide the nodes apart
               GZ_TOPOFIX.SLIDE_LEAST_ANGULAR_NODE(p_topo,
                                                   p_constellation.edge_id,
                                                   p_constellation.edge_start_node_id,
                                                   p_constellation.edge_end_node_id,
                                                   p_tolerance);


            ELSE

               RAISE_APPLICATION_ERROR(-20001,p_constellation.fix_constellation
                                           || 'Lepus unhandled exception ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;

      ELSIF p_constellation.fix_class = 6
      THEN

         --Andromeda

         BEGIN

            GZ_TOPOFIX.KEEP_MOST_ANGULAR(p_topo,
                                         p_constellation.edge_id,
                                         p_constellation.shorty_index,
                                         p_constellation.edge_start_node_id,
                                         p_constellation.edge_end_node_id,
                                         p_constellation.edge_vertex_count);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF UPPER(SQLERRM) LIKE '%ADD FAKE FACE%'
            AND p_face_tab <> 'NONE'
            THEN

               --RAISE_APPLICATION_ERROR(-20001,'add fake face on edge id ' || p_constellation.edge_id);

               --this is my error for the oracle bug
               --'%CHANGED EDGE COORDINATE STRING SELF-INTERSECTS%'
               --on the universal face
               --Call our special handler

               GZ_TOPOFIX.SELF_INTERSECTS_WORKAROUND(p_topo,
                                                     p_constellation.edge_id,
                                                     p_constellation.shorty_index,
                                                     p_face_tab,
                                                     p_tolerance);

            ELSE


               --what to handle here?

               RAISE_APPLICATION_ERROR(-20001,p_constellation.fix_constellation
                                        || 'Andromeda unhandled exception ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;


         END;

      ELSIF p_constellation.fix_class = 7
      THEN

         --Gemini
         --NEVER ENCOUNTERED OR TESTED

         BEGIN

            GZ_TOPOFIX.REMOVE_SHORTY_VERTEX(p_topo,
                                            p_constellation.edge_id,
                                            p_constellation.shorty_index);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%changed edge coordinate string has an intersection with another edge%'
            THEN

               --Something like LENGTHEN SHORTY should be implemented here
               --but lenghten shorty expects a 3 vertex edge, this guy has 4, first and last the same

               RAISE_APPLICATION_ERROR(-20001,'Gemini unimplemented degenerate case: ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);
               --GZ_TOPOFIX.LENGTHEN_SHORTY(p_topo,
                                          --p_constellation.edge_id,
                                          --p_constellation.shorty_index,
                                          --p_tolerance);

            ELSE


               RAISE_APPLICATION_ERROR(-20001,'Gemini Unhandled exception ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;


      ELSIF p_constellation.fix_class = 8
      THEN

         --capricorn

         BEGIN

            GZ_TOPOFIX.REMOVE_SHORTY_VERTEX(p_topo,
                                            p_constellation.edge_id,
                                            p_constellation.shorty_index);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%changed edge coordinate string has an intersection with another edge%'
            AND p_constellation.edge_vertex_count = 3
            THEN

               --THIS HAS NEVER BEEN TESTED FOR constellation 8

               GZ_TOPOFIX.LENGTHEN_SHORTY(p_topo,
                                          p_constellation.edge_id,
                                          p_constellation.shorty_index,
                                          p_tolerance);

            ELSE


               RAISE_APPLICATION_ERROR(-20001,'Capricorn Unhandled ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;


      ELSIF p_constellation.fix_class = 9
      THEN

         --Canis Minor

         BEGIN

            IF p_constellation.edge_vertex_count > 3
            THEN

               GZ_TOPOFIX.REMOVE_SHORTY_VERTEX(p_topo,
                                               p_constellation.edge_id,
                                               p_constellation.shorty_index);

            ELSE

               GZ_TOPOFIX.LENGTHEN_SHORTY(p_topo,
                                          p_constellation.edge_id,
                                          p_constellation.shorty_index,
                                          p_tolerance);


            END IF;

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%changed edge coordinate string has an intersection with another edge%'
            AND p_constellation.edge_vertex_count = 3
            THEN

               --this is expected a certain percent of the time
               --probably a 3 vertex edge V up against the universe poly

               GZ_TOPOFIX.LENGTHEN_SHORTY(p_topo,
                                          p_constellation.edge_id,
                                          p_constellation.shorty_index,
                                          p_tolerance);

            ELSE

               RAISE_APPLICATION_ERROR(-20001,'CanisMinor Unhandled ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;

      ELSIF p_constellation.fix_class = 10
      THEN

         --Cancer

         BEGIN

            GZ_TOPOFIX.DROP_Y_SHAPE(p_topo,
                                    p_constellation.edge_id,
                                    p_constellation.edge_start_node_id,
                                    p_constellation.edge_end_node_id);

         EXCEPTION
         WHEN OTHERS
         THEN

            --there should be a backup option here to back the Y up so the 3-connect
            --is further from the universal

            RAISE_APPLICATION_ERROR(-20001,'Cancer Unhandled ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

         END;


      ELSIF p_constellation.fix_class = 11
      THEN

         --hydra, late add
         --semi interior

         BEGIN

            GZ_TOPOFIX.REMOVE_OBSOLETE_NODE(p_topo,
                                            p_constellation.edge_id);


         EXCEPTION
         WHEN OTHERS
         THEN

             RAISE_APPLICATION_ERROR(-20001,'Hydra unhandled ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

         END;

      ELSIF p_constellation.fix_class = 12
      THEN

         --crux
         --Majority of interior dupes

         BEGIN

            GZ_TOPOFIX.FIX_INTERIOR_DUPE(p_topo,
                                         p_constellation.edge_id,
                                         p_constellation.edge_start_node_id,
                                         p_constellation.edge_end_node_id);


         EXCEPTION
         WHEN OTHERS
         THEN

             RAISE_APPLICATION_ERROR(-20001,'Crux unhandled ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

         END;

      ELSIF p_constellation.fix_class = 13
      THEN

         --Cygnus

         BEGIN

            GZ_TOPOFIX.REMOVE_INTERIOR_VERTEX(p_topo,
                                              p_constellation.edge_id,
                                              p_constellation.shorty_index);


         EXCEPTION
         WHEN OTHERS
         THEN

             RAISE_APPLICATION_ERROR(-20001,'Cygnus unhandled ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

         END;

      ELSIF p_constellation.fix_class = 14
      THEN

         --Cassiopeia
         --HAVE NEVER ACTUALLY SEEN THIS OR TESTED THIS CODE but simple fix, so whatevs

         BEGIN

            GZ_TOPOFIX.REMOVE_INTERIOR_VERTEX(p_topo,
                                              p_constellation.edge_id,
                                              p_constellation.shorty_index);


         EXCEPTION
         WHEN OTHERS
         THEN

             RAISE_APPLICATION_ERROR(-20001,'Cassiopeia unhandled ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

         END;

      ELSE

         RETURN 'FALSE';

      END IF;


      -------------------------
      --made it here equals good
      RETURN 'TRUE';
      -------------------------

   EXCEPTION
   WHEN OTHERS
   THEN

       --return the error message
       RETURN SQLERRM || CHR(10) || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

   END FIX_CONSTELLATION;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION RESHAPE_EDGE_WITH_DUPLICATE (
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER,
      p_tolerance       IN NUMBER DEFAULT .05,
      p_hold_universal  IN VARCHAR2 DEFAULT 'Y',
      p_log_type        IN VARCHAR2 DEFAULT 'TOPOFIX'
   ) RETURN VARCHAR2
   AS

      --Matt! 5/29/12
      --Entry point for edge-based duplicate vertice fixer
      --See FIX_13356 for the face-based 13356 fixer
      --Both are wrappers to the same underlying fixing code

      --Here the input is an edge, and there is no face table that concerns us
      --1. Determine the constellation
      --2. Call various types of fixers
      --   Do not populate or update any measurements
      --3. Follow Sidey's lead in RESHAPE_TOO_CLOSE and return 'TRUE' or 'FALSE'

      --sample: output := GZ_TOPOFIX.RESHAPE_EDGE_WITH_DUPLICATE('Z609CL',589,.05,'Y','CLIP');


      psql                    VARCHAR2(4000);
      validate_msg            VARCHAR2(4000);
      og_validate_coordinate  NUMBER;
      bad_edge_constellation  GZ_TYPES.TOPOFIX_CONSTELLATION;
      bad_edge_fix            VARCHAR2(4000);
      output                  VARCHAR2(32) := 'FALSE';
      p_debug                 NUMBER := 0;


   BEGIN


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('RESHAPE_EDGE_WITH_DUPLICATE: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      -----
      --any initial verification checks? Caller should do most right?
      ----


      --get validate string

      psql := 'SELECT '
           || 'GZ_GEOM_UTILS.VALIDATE_LINES_WITH_CONTEXT(e.geometry, :p1) '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';

      --13356 [Element <1>] [Coordinate <1>]

      BEGIN

         EXECUTE IMMEDIATE psql INTO validate_msg USING p_tolerance,
                                                        p_edge_id;

      EXCEPTION
      WHEN OTHERS
      THEN


         IF SQLERRM LIKE '%no data found%'
         THEN

            --ORA-01403: no data found
            --If the caller is dipping in for a bucket of edges
            --And calls this utility repeatedly, an edge might disappear during a neighbor fix
            --Decide how to handle if it happens
            RAISE_APPLICATION_ERROR(-20001, 'Edge ' || p_edge_id || ' doesnt exist. This may be expected depending on caller');

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

         END IF;

      END;





      IF validate_msg IS NULL
      OR validate_msg NOT LIKE '13356%'
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Why you pestering me, validation returns ' || validate_msg);

      ELSE

         NULL;
         --tuck this away. Meh Lets skip it. Used it to compare post-fix string
         --og_validate_coordinate := GZ_TOPOFIX.PARSE_VALIDATION_STRING(validate_msg, 'COORDINATE');

      END IF;


      --get list of info about our constellation of badness

      bad_edge_constellation := GZ_TOPOFIX.GET_EDGE_CONSTELLATION(p_topo,
                                                                  p_edge_id);

      IF p_debug = 1
      THEN
         dbms_output.put_line(GZ_TOPOFIX.DUMP_CONSTELLATION(bad_edge_constellation));
      END IF;


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                             p_topo,
                                             'RESHAPE_EDGE_WITH_DUPLICATE',
                                             p_topo || '_edge$',
                                             'Decided edge ' || p_edge_id || ' is ' || bad_edge_constellation.fix_constellation);

      --And here is the actual work--
      bad_edge_fix := GZ_TOPOFIX.FIX_CONSTELLATION(p_topo,
                                                   'NONE',  --special flag for no face table.  Only needed for some weirdo workarounds
                                                   bad_edge_constellation,
                                                   p_hold_universal,
                                                   p_tolerance);
      --------------------------------


      --howd we do?

      IF bad_edge_fix = 'TRUE'
      THEN

         --we think we are rock stars.
         --We cant really verify our awesomeness though, since sometimes we remove
         --the very edge we were working on


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'RESHAPE_EDGE_WITH_DUPLICATE',
                                                p_topo || '_edge$',
                                                'Success (we think). Fixed ' || p_edge_id);

         output := 'TRUE';


      ELSE

         --fix_constellation has returned an error message instead of true
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                 p_topo,
                                                 'RESHAPE_EDGE_WITH_DUPLICATE',
                                                 p_topo || '_edge$',
                                                 'Failure to fix edge ' || p_edge_id || '. See error msg->',
                                                 NULL,NULL,NULL,NULL,NULL,
                                                 bad_edge_fix);

         output := 'FALSE';

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('RESHAPE_EDGE_WITH_DUPLICATE: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END RESHAPE_EDGE_WITH_DUPLICATE;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION FIX_13356 (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_face_id         IN NUMBER,
      p_hold_universal  IN VARCHAR2 DEFAULT 'Y',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_sdo_col         IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_face_pkc        IN VARCHAR2 DEFAULT 'FACE_ID',
      p_log_type        IN VARCHAR2 DEFAULT 'TOPOFIX',
      p_debug           IN NUMBER DEFAULT 0
   ) RETURN VARCHAR2
   AS

      --Matt! 11/16/11
      --This is the SUPERvisor of lower level 13356 fixers

      --Steps
      --1. Get the validation string and segment
      --2. Determine what primitive edge owns the dupe
      --3. Determine which constellation of invalidity this is based on
      --     Edge vertex count
      --     Edge nodes on universal face
      --     Index of shorty segment on the edge
      --     Edge borders the universal or is interior
      --     Count of obsolete nodes on the segment
      --4. Fix based on the constellation type
      --5. For all paths populate measurements for the face and any neighbor faces we took a chomp out of
      --6. Verify that rebuilt sdogeoms are now valid
      --7. Return 'TRUE' or 'FALSE' like Sidey

      psql                    VARCHAR2(4000);
      validate_msg            VARCHAR2(4000);
      og_validate_coordinate  NUMBER;
      badgeom                 SDO_GEOMETRY;
      shorty_segment          SDO_GEOMETRY;
      bad_edge_id             NUMBER;
      bad_edge_segment        NUMBER;
      bad_edge_constellation  GZ_TYPES.TOPOFIX_CONSTELLATION;
      bad_edge_fix            VARCHAR2(4000);
      output                  VARCHAR2(32) := 'FALSE';


   BEGIN


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FIX_13356: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      -----
      --any initial verification checks? Caller should do most right?
      ----


      --get validate string
      --and geom.  Going back to topo to be absolutely certain

      psql := 'SELECT '
           || 'a.topogeom.get_geometry(), SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.topogeom.get_geometry() , :p1) '
           || 'FROM '
           || p_face_tab || ' a '
           || 'WHERE a.' || p_face_pkc || '  = :p2 ';

      --13356 [Element <1>] [Coordinate <220>][Ring <1>]
      EXECUTE IMMEDIATE psql INTO badgeom, validate_msg USING p_tolerance,

                                                           p_face_id;
      IF validate_msg = 'TRUE'
      THEN

         --this is sloppy but...
         --sometimes rounding brings an sdogeometry into an invalid 13356 segment
         --the sdogeometry seen by the caller is bad
         --the topogeom seen here is ok
         --just update the sdo to match the topo if thats the case and then get out

          GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                  p_topo,
                                                  'FIX_13356',
                                                  p_face_tab,
                                                  p_face_pkc || ' ' || p_face_id || ' is valid in topogeom. Probably rounding. '
                                                  || 'Just gonna update the sdo ',NULL,NULL,NULL,NULL,NULL,NULL);

          GZ_BUSINESS_UTILS.POPULATE_EDIT_MEASUREMENTS(p_topo,
                                                  p_face_tab,
                                                  p_face_id,
                                                  'N');  --Dont check face sdo validity.  We have no confidence

          RETURN 'TRUE';

      ELSIF validate_msg != 'TRUE'
      AND validate_msg NOT LIKE '13356%'
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Why you pestering me, validation returns ' || validate_msg);

      ELSE

         shorty_segment := GZ_TOPOFIX.GET_13356_SEGMENT(badgeom, validate_msg);

         --tuck this away too
         og_validate_coordinate := GZ_TOPOFIX.PARSE_VALIDATION_STRING(validate_msg, 'COORDINATE');

      END IF;


      --shorty_segment is a gtype 2002 with just 2 vertices
      --its a direct extract from the face sdogeometry
      --Find out what primitive edge is the source

      bad_edge_id := GZ_TOPOFIX.GET_EDGE_FROM_SEGMENT(p_topo,
                                                      p_face_tab,
                                                      p_face_id,
                                                      shorty_segment,
                                                      p_face_pkc,
                                                      p_tolerance);


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                              p_topo,
                                              'GET_EDGE_FROM_SEGMENT',
                                              p_face_tab,
                                              'Decided face ' || p_face_id || ' bad edge is edge_id ' || bad_edge_id);



      --get list of info about our constellation of badness

      bad_edge_constellation := GZ_TOPOFIX.GET_EDGE_CONSTELLATION(p_topo,
                                                                  bad_edge_id);

      IF p_debug = 1
      THEN
         dbms_output.put_line(GZ_TOPOFIX.DUMP_CONSTELLATION(bad_edge_constellation));
      END IF;


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                              p_topo,
                                              'GET_EDGE_CONSTELLATION',
                                              p_face_tab,
                                              'Decided edge ' || bad_edge_id || ' is ' || bad_edge_constellation.fix_constellation);

      bad_edge_fix := GZ_TOPOFIX.FIX_CONSTELLATION(p_topo,
                                                   p_face_tab,
                                                   bad_edge_constellation,
                                                   p_hold_universal,
                                                   p_tolerance);


      --howd we do?

      IF bad_edge_fix = 'TRUE'
      THEN

         --we think we are rock stars. Verify our awesomeness

         EXECUTE IMMEDIATE psql INTO badgeom, validate_msg USING p_tolerance,
                                                                 p_face_id;

         IF validate_msg = 'TRUE'
         THEN

             GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                    p_topo,
                                                    'FIX_13356',
                                                    p_face_tab,
                                                    'Success. Fixed ' || p_face_id);

            output := 'TRUE';

         ELSIF validate_msg LIKE '13356%'
         AND og_validate_coordinate != GZ_TOPOFIX.PARSE_VALIDATION_STRING(validate_msg, 'COORDINATE')
         THEN

            --just rolled on to some other dupe on the same face

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                    p_topo,
                                                    'FIX_13356',
                                                    p_face_tab,
                                                    'Semi-success. We fixed the first dupe on face id ' || p_face_id,
                                                    NULL,NULL,NULL,NULL,NULL,
                                                    validate_msg);

            output := 'FALSE';

         ELSIF validate_msg LIKE '13356%'
         AND og_validate_coordinate = GZ_TOPOFIX.PARSE_VALIDATION_STRING(validate_msg, 'COORDINATE')
         THEN

            --Fail. We think we did something but got the same 13356 error back
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                    p_topo,
                                                    'FIX_13356',
                                                    p_face_tab,
                                                    'Did something but failed to fix the dupe on face id ' || p_face_id,
                                                    NULL,NULL,NULL,NULL,NULL,
                                                    validate_msg);

            output := 'FALSE';

         ELSE

            --Fail. We think we did something but got the same 13356 error back
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                    p_topo,
                                                    'FIX_13356',
                                                    p_face_tab,
                                                    'Failed to fix the dupe on face id ' || p_face_id,
                                                    NULL,NULL,NULL,NULL,NULL,
                                                    validate_msg);

            output := 'FALSE';

         END IF;

      ELSE

         --fix_constellation has returned an error message instead of true
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                 p_topo,
                                                 'FIX_13356',
                                                 p_face_tab,
                                                 'Known failure on face id ' || p_face_id || '. See error msg',
                                                 NULL,NULL,NULL,NULL,NULL,
                                                 bad_edge_fix);

         output := 'FALSE';

      END IF;


      --no matter what, update the face sdo and measurements
      --even if we failed, we owe it to whoever comes after us (ex an ADE user)
      --to update all SDO, especially neighbors, that we may have munged up
      --this fn checks face id for differences between topogeom.get_geometry and geometry
      --then works its way outward to neighboring faces, doing the same

      GZ_BUSINESS_UTILS.POPULATE_EDIT_MEASUREMENTS(p_topo,
                                              p_face_tab,
                                              p_face_id,
                                              'N');  --Dont check face sdo validity.  We have no confidence

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FIX_13356: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END FIX_13356;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

    FUNCTION IS_FACE_INVALID (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_face_id         IN NUMBER,
      p_error           IN VARCHAR2,
      p_face_pkc        IN VARCHAR2 DEFAULT 'FACE_ID',
      p_sdo_col         IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN BOOLEAN
   AS

      --Matt! 11/14/11

      --Should we also check topogeom.get_geometry() or is this a relationship of Trust?

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(*) '
           || 'FROM '
           || p_face_tab || ' a '
           || 'WHERE '
           || 'SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.' || p_sdo_col || ', :p1) LIKE :p2 AND '
           || 'a.' || p_face_pkc || ' = :p3 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_tolerance,
                                              p_error || '%',
                                              p_face_id;

      IF kount = 0
      THEN

         RETURN FALSE;

      ELSIF kount = 1
      THEN

         RETURN TRUE;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'wtf?');

      END IF;

   END IS_FACE_INVALID;


   ----^------^-----------------------------------------------------^------^----------
   --  | MATT | ++++++++++++++++++++++++++++++++++++++++++++++++++++| MATT |++++++++--
   -----------------------------------------------------------------------------------

   ------------------------------------------------------------------------------------
   --   | SHARED | +++++++++++++++++++++++++++++++++++++++++++++++++ | SIDEY |++++++++-
   ----\/------\/---------------------------------------------------\/------\/---------



   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   FUNCTION check_face_tab (p_gz_jobid         IN VARCHAR2,
                            p_topo             IN VARCHAR2,
                            p_face_tab         IN VARCHAR2,
                            p_face_pkc         IN VARCHAR2 DEFAULT 'FACE_ID',
                            p_log_type         IN VARCHAR2 DEFAULT 'TOPOFIX',
                            p_tolerance        IN NUMBER   DEFAULT .05,
                            p_sdo_srid         IN NUMBER   DEFAULT 8265)
      RETURN VARCHAR2
   AS

      --WWU added this. Its a wrapper to GZ_TOPOFIX.GZ_FIX_FACE
      --Plan is to call this wrapper from topo build, clip, sp, ls, merge, etc
      --Matt! 4/17/12 Added and passed thru log, tolerance, and srid inputs
      --Matt! 10/1/12 changed to not look at the QC column for the final return value
      --              The QC column gets out of whack.  GZ_FIX_FACE only touches QC for
      --              the faces on which it works.  Sometimes old values will hang around, or
      --              most annoyingly, SMPOLY will merge a QC face into a nice face, leaving a
      --              QC value on a perfectly valid face
      --Matt! 8/9/13  No change to this code, but now the QC column is IN whack.  GZ_FIX_FACE
      --              now updates all face QC values to NULL before flagging problems.
      --              We decided that no other code shares xx_face.qc any more.  So in summary
      --              1 - Invalid face
      --              2 - Null sdo
      --              3 - Not a polygon
      --              NULL - No known issues

      psql            VARCHAR2 (4000);
      output          NUMBER;
      fix_face_return VARCHAR2(1);

   BEGIN
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 1');
      DBMS_APPLICATION_INFO. SET_CLIENT_INFO ('TOPOFIX: run GZ_FIX_FACE ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --any problems, this guy will log the problem details then raise an error
      fix_face_return := GZ_TOPOFIX.GZ_FIX_FACE ( p_gz_jobid,
                                                  p_topo,
                                                  p_face_tab,
                                                  p_face_pkc=>p_face_pkc,
                                                  p_log_type=>p_log_type,
                                                  p_tolerance=>p_tolerance,
                                                  p_sdo_srid=>p_sdo_srid);
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 2');
      DBMS_APPLICATION_INFO.
      SET_CLIENT_INFO ('CHECK_FACE_TAB: Check if any null geometry ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------
      psql := 'UPDATE ' ||p_face_tab || ' a '
           || ' SET a.qc = :p1 '
           || 'WHERE a.sdogeometry IS NULL AND a.qc IS NULL';
      EXECUTE IMMEDIATE  psql USING 2;
      COMMIT;
      ---------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 3');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO ('CHECK_FACE_TAB: Check gtype != 2003 ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------
      psql := 'UPDATE ' ||p_face_tab||' a '
           || 'SET a.qc = :p1 '
           || 'WHERE a.sdogeometry.sdo_gtype <> :p2 AND a.qc IS NULL';
      EXECUTE IMMEDIATE  psql USING 3, 2003;
      COMMIT;
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION ('Step 4');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO ('CHECK_FACE_TAB: Final check and return the result ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------
      psql := 'SELECT COUNT(1) FROM ' ||p_face_tab
           || ' WHERE qc IN (:p1,:p2) ';

      --Im not worried about false failures in these totally bad faces
      EXECUTE IMMEDIATE  psql INTO output USING 2, 3;

      IF output = 0 AND
      fix_face_return = '0'
      THEN

         --Yes
         RETURN '0';

      ELSE

         --No
         RETURN '1';

      END IF;

   END check_face_tab;


   ------------------------------------------------------------------------------------
   --   | SIDEY | +++++++++++++++++++++++++++++++++++++++++++++++++ | SIDEY |++++++++--
   ----\/------\/---------------------------------------------------\/------\/---------


--  A package to provide various capabilities to validate geometries and find
--  potential problems such as segments too close to each other.

--  a) find segments in each geometry that intersect or could "snap" together is
--     performed by:
--     >>check_for_self_intersect.<<

--  b) find nearby segments in adjacent geometries (two or more geometries)
--     >>check_for_nearby_segments<<  (nearly finished)

-- work so far: 1) adapted set_many_mbr to handle multiple geometries
--              2) to avoid modifying check_for_self_intersect to check one
--                  geometry for nearby segments in another geometry - it was
--                  easiest to just concatenate XY arrays and the Info arrays
--              3) used get nearby edges from GZ_SUPER to find teh second geometry
--
--  I just realized that the problem of finding nearbys may be construed as
-- two separate problems. From an effciciency point of view this might be important
-- a) just find segments from "a" that are to near to "b"

--or what this package is doing
-- b) finding any sort of near glances.

PROCEDURE FIX_FACE_13349(face_id NUMBER,Intable VARCHAR2,InGeom_Column VARCHAR2,schema VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05,UFS VARCHAR default 'FALSE',p_log_type VARCHAR2) AS

-- Alternate main entry point to fix an Oracle 13349 face error
--     (polygon boundary crosses itself):
--example: 13349 [Element <1>] [Ring <1>][Edge <22>][Edge <21>]

/**
--##############################################################################
-- Program Name: Fix_face_13349
-- Author: Sidey Timmins
-- Creation Date: 10/28/2011

-- Usage:
--   This PL/SQL procedure has 7 parameters:
--          face_id  - the problem face which has an identified error
--          Intable  - the clip_face table containing the face geometry
--          InGeom_column - the Geometry column
--          schema   - the working schema
--          topology - the topology to edit
--          ptolerance: tolerance in meters which the face fails at.
--          UFS      - Universal face switch: 'TRUE' allows changes on the UF,
--                     'FALSE' does not.

-- Purpose: This procedure repairs faces which either:
--          1) have too small angles - requires a node or vertex move
--      or  2) have self- intersecting or nearly self-intersecting edges
--
-- Method:
-- Dependencies:
--
--##############################################################################
*/
   status     VARCHAR2(4000);
BEGIN

   status := FIX_13349(face_id,Intable,InGeom_Column,schema,topology,tolerance,UFS,p_log_type);



END FIX_FACE_13349;
--
FUNCTION ID_EDGE_IN_13349(status VARCHAR2,face_id NUMBER,Intable VARCHAR2,InGeom_Column VARCHAR2,schema VARCHAR2,topology VARCHAR2) RETURN MDSYS.SDO_LIST_TYPE AS

-- Identify the edge in the 13349 message and specify the error (bad) segments
-- --13349 [Element <1>] [Ring <1>][Edge <7>][Edge <11>]
--
/**
--##############################################################################
-- Program Name: Fix_face_13349
-- Author: Sidey Timmins
-- Creation Date: 10/28/2011

-- Usage:
--   This PL/SQL procedure has 7 parameters:
--          status   = the input 13349 error message
--          face_id  - the problem face which has an identified error
--          Intable  - the clip_face table containing the face geometry
--          InGeom_column - the Geometry column
--          schema   - the working schema
--          topology - the topology to edit
--          ptolerance: tolerance in meters which the face fails at.

-- Purpose:
--          Returns an array containing the true edge_id, and the 2 overlapping segments
--
-- Method:
-- Dependencies:
--
--##############################################################################
*/


  edge_ids    MDSYS.SDO_LIST_TYPE;
  edge_nos    MDSYS.SDO_LIST_TYPE;
  seg_nos     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  edge_geom   MDSYS.SDO_GEOMETRY;
  poly_geom   MDSYS.SDO_GEOMETRY;

  pos         PLS_INTEGER :=0;
  last        PLS_INTEGER :=0;
  edge_id     NUMBER;
  choice1     NUMBER;
  choice2     NUMBER;
  x           NUMBER;
  y           NUMBER;
  len         NUMBER;
  sql_stmt    VARCHAR2(4000);

BEGIN

   choice1 := pull(status,'[Edge ');
   choice2 := pull(status,'[Edge ',2);
   sql_stmt := 'SELECT t.' || InGeom_Column ||' from ' || schema ||'.'||InTable ||
               ' t WHERE t.FACE_ID =:2';
--   dbms_output.put_line(sql_stmt);
   execute IMMEDIATE sql_stmt into poly_geom using face_id;

   edge_ids := sdo_topo.get_face_boundary(Topology,face_id);

   seg_nos.extend(3*edge_ids.count);
   sql_stmt := 'SELECT geometry from ' || schema||'.' || Topology || '_EDGE$ where edge_id=:1';

   For ii in 1..edge_ids.count Loop
     edge_id := abs(edge_ids(ii));
     execute IMMEDIATE sql_stmt into edge_geom using edge_id;
     edge_id := edge_ids(ii);
     seg_nos(pos+1) := edge_id;
     edge_nos := match_edge(poly_geom,edge_id,edge_geom,x,y,len,0.00000002);
-- Fix any obvious mistakes
     if edge_nos(1) < last then
        edge_nos(1) := last;
     end if;
     seg_nos(pos+2) := edge_nos(1);
     seg_nos(pos+3) := edge_nos(2);
     dbms_output.put_line('edge_no ' || edge_nos(1) || ' ' || edge_nos(2) || ' id ' || edge_id);
     pos := pos +3;
     last := edge_nos(2);
   End Loop;

   RETURN seg_nos;
END;
--
FUNCTION Wise_count(m NUMBER,n NUMBER,nvert NUMBER,clockwise VARCHAR2 default 'UNKNO') RETURN NUMBER AS
/*
**************************************************************************************
--Program Name: Wise_Count
--Author: Sidey Timmins
--Creation Date: 08/01/2009
--Usage:
  -- Call this function from inside another PL/SQL program.  There are 4 parameters:
  --
  --   REQUIRED Parameters:
  --            n:  a vertex
  --            m:  another vertex
  --            nvert: the # of vertices in the outer ring of the polygon
  --            clockwise: direction ('ANTI' or 'CLOCK') to count
--Purpose:      Determines either the smallest count of vertices between m and n
--              or the clockwise or anticlockwise count from m to n.
-- Method:      Determines the order of v1 and v2 (vertices increment anticlockwise
--              around the outer ring) to check their vertex spacing.
--Dependencies: None
***************************************************************************************
*/
   kount    NUMBER;
   BIG      NUMBER := 1.E10;
BEGIN
-- Get the shortest number of vertices between start and end
   IF clockwise = 'UNKNO' THEN
    if n < 0 or m < 0 then
       dbms_output.put_line('returning 1E10' || ' n ' || n || ' m ' || m);
      RETURN BIG;
    elsif n = m then
-- Cannot tell the direction
       kount := 0;
    elsif m > n then
    --                          12 n
--                 m   1            11 M
--               m  2                  10  M
--               m  3                      9  M
--                m  4                  8  M
--                  m    5            7  M
--                        mm    6
       if n + nvert - m < (m-n) then
         kount := n + nvert - m;
       else
         kount := m - n;
       end if;
    else
    --                          12 m
--                n    1            11  N
--              n   2                  10   N
--              n  3                      9  N
--               n    4                  8  N
--                 n    5            7  N
--                              6  NN
       if m + nvert - n  < (n-m) then
         kount := m + nvert - n;
       else
         kount := n - m;
       end if;
    end if;

   ELSE
-- Get the anti clockwise count
--       dbms_output.put_line(clockwise || ' M ' || m || ' N ' || n);
     if clockwise = 'ANTI' then
        if m < n then
         kount := n - m;
       else
         kount := n + nvert - m;
       end if;
    else
-- Get the clockwise count
       if m > n then
         kount := m - n;
       else
         kount := m + nvert - n;
       end if;
    end if;
   END IF;
  RETURN kount;


END Wise_count;
--
FUNCTION FIX_13349(face_id NUMBER,Intable VARCHAR2,InGeom_Column VARCHAR2,schema VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05,pUFS VARCHAR default 'FALSE',p_log_type VARCHAR2) RETURN VARCHAR2 AS

-- Main entry point to fix a face with an Oracle 13349 error -
--     (polygon boundary crosses itself),
--     including the Element, Ring, and Edge numbers where self-intersection occurs
-- example: 13349 [Element <1>] [Ring <1>][Edge <22>][Edge <21>]

/*
--##############################################################################
--Program Name: Fix_13349
--Author: Sidey Timmins
--Creation Date: 10/xx/2011
--
--Usage:
  -- Call this function from inside another PL/SQL program.  This function
  -- has 7 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      face_id        - the face_id from face$ which has the 13349 error
  --      Intable        - the input clip_table
  --      InGeomcolumn   - the geometry column for the face
  --      schema         - the schema we are in
  --      Topology       - the topology name
  --      tolerance      - the Oracle tolerance the face failed at
  --      UFS            - the universal face switch,. 'TRUE' allows
  --                       edge updates on the universal face (UF), 'FALSE' does not.
--
-- Purpose: This function attempts to fix face 13349 errors. A polygon  may
==          actually (or more usually almost) cross over itself. There are a
--          a number of different types:
--          1) an "L" shaped edge that comes to close to another part of the
--             boundary usually off the UF.
--          2) sliver triangles with 2 very small angles (< 10 degrees) usually
--             on the UF.
-- Method:
--         It checks whether the change has been accepted in the topology by
--         checking to see if the edge length has increased. Return a status
--         of 'TRUE' for success or an Oracle error message.
--##############################################################################
*/


  edge_geom   MDSYS.SDO_GEOMETRY;
  poly_geom   MDSYS.SDO_GEOMETRY;
  small_geom  MDSYS.SDO_GEOMETRY;
  edge_ids    MDSYS.SDO_LIST_TYPE;
  edge_nos    MDSYS.SDO_LIST_TYPE;
  seg_nos     MDSYS.SDO_LIST_TYPE;
  angles      MDSYS.SDO_LIST_TYPE;
  shortest    NUMBER := 1.E10;
  edge_id     NUMBER;
  len         NUMBER;
  short_edge  NUMBER;
  choice1     NUMBER;
  choice2     NUMBER;
  start_node  NUMBER;
  end_node    NUMBER;
  kount1      NUMBER;
  kount2      NUMBER;

  x           NUMBER;
  y           NUMBER;
  left_face   NUMBER;
  right_face  NUMBER;
  bad_edge    NUMBER;
  short_dist  NUMBER;
  longest     NUMBER := 0.0;
  area        NUMBER;

  UFS         VARCHAR2(5) := UPPER(pUFS);
  istart      PLS_INTEGER;
  iend        PLS_INTEGER;
  loops       PLS_INTEGER;
  vtx_count   PLS_INTEGER;
  vtx1        PLS_INTEGER;
  vtx2        PLS_INTEGER;
  Edge_table  VARCHAR2(30);
  sql_stmt    VARCHAR2(4000);
  sql_stmt2   VARCHAR2(4000);
  status      VARCHAR2(4000);
  n           PLS_INTEGER;
  small_angle_dist BOOLEAN:= FALSE;

BEGIN

   sql_stmt := 'SELECT t.' || InGeom_Column ||',SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(t.' ||
               InGeom_Column ||',:1) from ' || schema ||'.'||InTable || ' t WHERE t.FACE_ID =:2';

   execute IMMEDIATE sql_stmt into poly_geom,status using tolerance,face_id;

   if SUBSTR(status,1,5) = '13356' then
     sql_stmt := 'SELECT t.' || InGeom_Column ||',SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(t.' ||
               InGeom_Column ||',:1) from ' || schema ||'.'||InTable || ' t WHERE t.FACE_ID =:2';

     execute IMMEDIATE sql_stmt into poly_geom,status using 0.05,face_id;
  end if;

   if status = 'TRUE' or SUBSTR(status,1,5) <> '13349' then
     dbms_output.put_line(status);
     RETURN status;
   else
-- Get the particular edges Oracle complains about
--13349 [Element <1>] [Ring <1>][Edge <7>][Edge <11>]


     choice1 := pull(status,'[Edge ');
     choice2 := pull(status,'[Edge ',2);
     seg_nos := ID_EDGE_IN_13349(status,face_id,Intable,InGeom_Column,schema,topology);
--
     for ii in 1..TRUNC(seg_nos.count/3) loop
        istart := seg_nos(ii*3-1);
        iend   := seg_nos(ii*3);
        if choice1 >= istart and choice2 <= iend then
           bad_edge := abs(seg_nos(ii*3-2));   -- can be negative
           dbms_output.put_line('bad edge ' || bad_edge || ' Istart ' || istart || ' iend ' || iend);
           exit;
        end if;
     end loop;
   end if;


   edge_ids := sdo_topo.get_face_boundary(Topology,face_id);
   sql_stmt := 'SELECT geometry from ' || schema||'.' || Topology || '_EDGE$ where edge_id=:1';

-- First check to find the shortest edge on the face. If it is very short then
-- the error may be caused by nodes being too close.

   For ii in 1..edge_ids.count Loop
      edge_id := abs(edge_ids(ii));
      execute IMMEDIATE sql_stmt into edge_geom using edge_id;
      len := sdo_geom.sdo_length(edge_geom,0.05,'unit=meter');
      if len > longest then
         longest := len;
      end if;
        dbms_output.put_line('len ' || round(len,10) || ' ID ' || edge_ids(ii));
      if len < shortest then
         shortest := len;
         short_edge := edge_id;
      end if;

   End Loop;
--    dbms_output.put_line('length ' || round(shortest,10) || ' id ' || short_edge);

-- If the face is a small triangle like feature we move a node



-- The tolerance that if passed in is the length so this message is invalid,
-- it still probably fails at 0.05


   n := poly_geom.sdo_ordinates.count;

   vtx1 := choice1;
   vtx2 := choice2;
   dbms_output.put_line('vtx1 ' || vtx1 || ' vtx2 ' || vtx2 || ' pg ' || n);
   if choice2 < vtx1 then
     vtx1 := choice2;
     vtx2 := choice1;
   end if;
   if vtx1 > 1 then
      vtx1 := vtx1 -1;
   end if;
   if vtx2 < TRUNC(n/2)-1 then
      vtx2 := vtx2+1;
   end if;

  dbms_output.put_line('vtx1 ' || vtx1 || ' vtx2 ' || vtx2 || ' pg ' || n);

   small_geom := GZ_QA.get_Xys(poly_geom,1,-vtx1,1,vtx2);

   angles :=  get_angles(small_geom);
   short_dist := gz_qa.get_short_gcd(small_geom);
   area := sdo_geom.sdo_area(poly_geom,0.05,'unit=sq_meter');
   vtx_count := wise_count(choice1,choice2,n);

   dbms_output.put_line('vertex count ' ||vtx_count || ' area ' || round(area,2) || 'short length ' || short_dist ||' smallest angle ' || round(angles(1),2));

-- Decide if the problem involves a short edge or small angle
-- The vertex count between the problems is usually low when the problem
-- involves an "L" shaped edge.

   if angles(1) < 6 or short_dist < 1. or area < 10. then
      small_angle_dist := TRUE;
   end if;

-- If the face is a small triangle like feature we move a vertex or a node
-- Choose a method: 1) move a node or a vertex

--    dbms_output.put_line('n ' || n || ' shortest ' || shortest || ' for face_id ' || face_id);
    sql_stmt2 := 'SELECT left_face_id,right_face_id,start_node_id,end_node_id from ' || schema||'.' || Topology || '_EDGE$ where edge_id=:1';
    execute IMMEDIATE sql_stmt2 into left_face,right_face,start_node,end_node using edge_id;


    sql_stmt2 := 'SELECT count(1) from ' || schema||'.' || Topology ||
            '_EDGE$ where (start_node_id=:1 OR end_node_id=:2) AND (left_face_id=-1 OR right_face_id=-1)';
    execute IMMEDIATE sql_stmt2 into kount1 using start_node,start_node;

    sql_stmt2 := 'SELECT count(1) from ' || schema||'.' || Topology ||
          '_EDGE$ where (start_node_id=:1 OR end_node_id=:2) AND (left_face_id=-1 OR right_face_id=-1)';
    execute IMMEDIATE sql_stmt2 into kount2 using end_node,end_node;


--  Fix an "A" shaped triangle on the UF
    dbms_output.put_line('kount1 ' || kount1 || ' kount2 ' || kount2 || ' longest ' || longest);

    If kount1 <> 0 and kount2 <> 0 and longest < 0.5 then
       TRACK_APP('FIX_13349: Fix_A_triangle',Intable,p_log_type);
       status := FIX_A_TRIANGLE(short_edge,start_node,end_node,face_id,Intable,InGeom_Column,topology,schema,tolerance,p_log_type);
    ElsIf small_angle_dist  AND (left_face =-1 or right_face = -1 or shortest < 0.15) then
dbms_output.put_line('<<<<<<<<<<<<<<<<<<<<<<'||UFS);
      TRACK_APP('FIX_13349: Check_face_For_13349',Intable,p_log_type);
      status := CHECK_FACE_FOR_13349(face_id,Intable,InGeom_Column,schema,topology,tolerance,UFS);

    else

-- OR 2) Reshape an "L" shaped edge which in a large polygon usually is not
--       on the UF (universal face).

dbms_output.put_line('>>>>>>>>>>>>>>>>>>');

       Edge_table := Topology || '_EDGE$';
--       For ii in 1..edge_ids.count Loop
--        edge_id := abs(edge_ids(ii));
--        execute IMMEDIATE sql_stmt into edge_geom using edge_id;
--        edge_id := edge_ids(ii);
--        edge_nos := match_edge(poly_geom,edge_id,edge_geom,x,y,len,0.00000002);
--        dbms_output.put_line('edge_no ' || edge_nos(1) || ' ' || edge_nos(2) ||' id ' || edge_id);
--        Check_for_nearby_segments(Edge_Table,'EDGE_ID','GEOMETRY', tolerance,edge_id);
--       End Loop;

-- Sometimes the reshaping has to be repeated to fix the error
       loops := 0;
       WHILE status <> 'TRUE' and SUBSTR(status,1,5) <> '13356' and loops < 2 LOOP
         loops := loops + 1;
         TRACK_APP('FIX_13349: Reshape_Ledge: Loop '|| loops,Intable,p_log_type);
         status := Reshape_Ledge(topology, face_id, tolerance,Schema,Intable,InGeom_Column,UFS);
         dbms_output.put_line('loop ' || loops || ' ' || status);
       END Loop;
    End if;

      --no matter what, update the face sdo and measurements
      --even if we failed, we owe it to whoever comes after us (ex an ADE user)
      --to update all SDO, especially neighbors, that we may have munged up
      --this fn checks face id for differences between topogeom.get_geometry and geometry
      --then works its way outward to neighboring faces, doing the same

      GZ_BUSINESS_UTILS.POPULATE_EDIT_MEASUREMENTS(topology,
                                              InTable,
                                              face_id,
                                              'N');  --Dont check face sdo validity.  We have no confidence



   RETURN status;

EXCEPTION
   WHEN OTHERS THEN
     RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
END FIX_13349;
--
FUNCTION FIX_A_TRIANGLE(edge_id NUMBER,start_node NUMBER, end_node NUMBER,face_no NUMBER,pClip_face_table VARCHAR2,InGeom_Column VARCHAR2 default 'SDOGEOMETRY',ptopology VARCHAR2, pSchema VARCHAR2,tolerance NUMBER,p_log_type VARCHAR2)  RETURN VARCHAR2 AS

-- Fix a triangle on the Universal Face (UF) which has a crossbar edge like a
-- figure "A" and 2 edges leading away like the "A". Move both nodes at the
-- the ends of the NUF (non universal face) edge - the crossbar.

   point_to_move      MDSYS.SDO_GEOMETRY;
   point_geom         MDSYS.SDO_GEOMETRY;
   edge_geom          MDSYS.SDO_GEOMETRY;
   lrs_geom           MDSYS.SDO_GEOMETRY;
   nearby_geom        MDSYS.SDO_GEOMETRY;
   ids                MDSYS.SDO_LIST_TYPE;

   new_x              NUMBER;
   new_y              NUMBER;
   start_node_id      NUMBER;
   end_node_id        NUMBER;
   start_edge         NUMBER;
   end_edge           NUMBER;
   nearby_id1         NUMBER;
   nearby_id2         NUMBER;

   node_to_move       NUMBER;
   measure            NUMBER;
   edge_len           NUMBER;
   kount1             NUMBER;
   kount2             NUMBER;

   sql_stmt           VARCHAR2(4000);
   result            VARCHAR2(4000);

BEGIN


--   Drop a vertex on the crossbar if it exists - it wills cause problems -
--  intersections !!

   sql_stmt := 'SELECT geometry from ' || pschema||'.' || pTopology ||
               '_EDGE$ where edge_id = :1';
   execute immediate sql_stmt into edge_geom using edge_id;

   if edge_geom.sdo_ordinates.count > 4 then
     drop_edge_vertex(pTopology,edge_id,2);
   end if;

-- Begin by getting the id(s) of the top part of the "A".

   sql_stmt := 'SELECT count(1) from ' || pschema||'.' || pTopology ||
            '_EDGE$ where (start_node_id=:1 OR end_node_id=:2) AND (left_face_id=-1 OR right_face_id=-1)';

   execute IMMEDIATE sql_stmt into kount1 using start_node,start_node;

   if kount1 <> 0 then
     sql_stmt := 'SELECT edge_id from '||pschema||'.' || pTopology ||
               '_EDGE$ where edge_id <> :1 AND (start_node_id=:2 OR end_node_id=:3)' ||
               ' ORDER by sdo_geom.sdo_length(geometry,0.05,''unit=meter'')';
     execute IMMEDIATE sql_stmt BULK COLLECT into ids using edge_id,start_node,start_node;
     start_edge := ids(1);
     nearby_id1 := ids(2);
   end if;

   sql_stmt := 'SELECT count(1) from ' || pschema||'.' || pTopology ||
          '_EDGE$ where (start_node_id=:1 OR end_node_id=:2) AND (left_face_id=-1 OR right_face_id=-1)';
   execute IMMEDIATE sql_stmt into kount2 using end_node,end_node;

   if kount2 <> 0 then
     sql_stmt := 'SELECT edge_id from '||pschema||'.' || pTopology ||
               '_EDGE$ where edge_id <> :1 AND (start_node_id=:2 OR end_node_id=:3)' ||
               ' ORDER by sdo_geom.sdo_length(geometry,0.05,''unit=meter'')';
     execute IMMEDIATE sql_stmt BULK COLLECT into ids using edge_id,end_node,end_node;
     end_edge := ids(1);
     nearby_id2 := ids(2);
   end if;

-- Ready to move a node: get the other edge and its direction

   sql_stmt := 'SELECT start_node_id,geometry from ' || pschema||'.' || pTopology ||
               '_EDGE$ where edge_id = :1';
   execute immediate sql_stmt into start_node_id,edge_geom using start_edge;

   edge_len := sdo_geom.sdo_length(edge_geom,0.05,'unit=meter');


-- These measures can get out of hand if the edge length is much different from
-- a meter. (Example: half of a 1000 meters is 500 meters - we dont want to make
-- any node moves that big!)

   if start_edge= end_edge then
     edge_len := edge_len*0.5;    -- incorporate a factor of half
   end if;

   if start_node_id = start_node then
     measure := -1. * (0.05/edge_len);  -- factors of a half are incorporated.
   else
     measure := 1. + (0.05/edge_len);
   end if;
--    dbms_output.put_line('new ' || TRUNC(measure,10) || ' start ' || start_node || ' start n id ' || start_node_id);
   point_geom := LRS_locate_pt(measure,edge_geom,'N');
   new_x := point_geom.sdo_point.x;
   new_y := point_geom.sdo_point.y;

   point_to_move := MDSYS.sdo_geometry(2001,8265,MDSYS.SDO_POINT_TYPE(new_x,new_y,NULL),NULL,NULL);

--   dbms_output.put_line('new ' || new_x || ' y ' || new_y);

-- Move start node
   node_to_move := start_node;
   TRACK_APP('Fix_A_triangle: moving node '||node_to_move,pClip_face_table,p_log_type);
   move_edge_node(ptopology,start_edge,start_node,measure,NULL);
 --  GZ_TOPO_UTIL.GZ_MOVE_NODE(ptopology,node_to_move,point_to_move);


-- Now work on the other edge (often the same edge).

   sql_stmt := 'SELECT start_node_id,geometry from ' || pschema||'.' || pTopology ||
               '_EDGE$ where edge_id = :1';
   execute immediate sql_stmt into start_node_id,edge_geom using end_edge;

   edge_len := sdo_geom.sdo_length(edge_geom,0.05,'unit=meter');

   point_to_move := MDSYS.sdo_geometry(2001,8265,MDSYS.SDO_POINT_TYPE(new_x,new_y,NULL),NULL,NULL);

-- Ready to move a node
    if start_node_id = end_node then
     measure := -1. * (0.05/edge_len);
   else
     measure := 1. + (0.05/edge_len);
   end if;
   point_geom := LRS_locate_pt(measure,edge_geom,'N');
   new_x := point_geom.sdo_point.x;
   new_y := point_geom.sdo_point.y;
--    dbms_output.put_line('New ' || new_x || ' y ' || new_y);

-- Move end node
   node_to_move := end_node;
   TRACK_APP('Fix_A_triangle: moving node '||node_to_move,pClip_face_table,p_log_type);
   move_edge_node(ptopology,end_edge,end_node,measure,NULL);
 --  GZ_TOPO_UTIL.GZ_MOVE_NODE(ptopology,node_to_move,point_to_move);

-- More SUCCESS checking
-- Now check the face this edge supposedly fixed

    sql_stmt := 'SELECT sdo_geom.validate_geometry_with_context(a.topogeom.get_geometry(),:1) from '||pSchema||
                '.' || pClip_Face_table ||' a where a.face_id=:2';
    EXECUTE IMMEDIATE sql_stmt into result using tolerance,face_no;
--    dbms_output.put_line(result);


-- COMPLETE SUCCESS
-- Update all of the geometries that surround node1. Remember the faces done, so we only do them once
    if result = 'TRUE' then

       TRACK_APP('Fix_A_triangle: Update_faces_around: '||node_to_move,pClip_face_table,p_log_type);
       UPDATE_FACES_AROUND(node_to_move,pClip_face_table, InGeom_column,pSCHEMA,ptopology,tolerance);
    end if;

--==============================================================================

  RETURN result;

END FIX_A_TRIANGLE;
--
FUNCTION RESHAPE_LEDGE(ptopology VARCHAR2, face_no NUMBER, tolerance NUMBER,pSchema VARCHAR2, pClip_face_table VARCHAR2,InGeom_Column VARCHAR2 default 'SDOGEOMETRY',UFS VARCHAR2 default 'FALSE')  RETURN VARCHAR2 AS

-- A procedure to reshape an "L" shaped edge that touches another edge in the same face.
--                     edge_id to be reshaped
--            \      /
--  --------   \    /-------------   nearby edge
--              \  /
--                +

-- Use this call: SQL> exec gz_utilities.reshape_Ledge('04','Z604LS3',face_no);
-- where face number is an actual face with a 13349 error.

/*
--##############################################################################
--Program Name: Reshape_Ledge
--Author: Sidey Timmins
--Creation Date: 10/xx/2011
--
--Usage:
  -- Call this function from inside another PL/SQL program.  This function
  -- has 6 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      Topology       - the topology name
  --      face_id        - the face_id from face$ which has the 13349 error
  --      tolerance      - the Oracle tolerance the face failed at
  --      schema         - the schema we are in
  --      Intable        - the input clip_table
  --      InGeomcolumn   - the geometry column for the face
  --      UFS            - Universal Face switch, 'TRUE' allows edits on
  --                       the UF. 'FALSE' does not.
--
-- Purpose: This function attempts to fix face 13349 errors. A polygon  may
==          actually (or more usually) almost cross over itself. One type is
--          1) an "L" shaped edge that comes to close to another part of the
--             boundary usually off the UF.

-- Method:
--         It checks whether the change has been accepted in the topology by
--         checking to see if the edge length has increased. Return a status
--         of 'TRUE' for success or an Oracle error message.
--##############################################################################
*/

  sql_stmt                        VARCHAR2(4000);
  Schema                         VARCHAR(30) := UPPER(pSchema);
  Topology                        VARCHAR(20) := UPPER(pTopology);
  Clip_face_table                 VARCHAR(30) := UPPER(pClip_face_table);
  status                          VARCHAR2(4000);
  new_geometry                    MDSYS.SDO_GEOMETRY;
  new_xys                         MDSYS.SDO_ORDINATE_ARRAY;
  xLL                             NUMBER;
  yLL                             NUMBER;
  xUR                             NUMBER;
  yUR                             NUMBER;
  coord1                          NUMBER;
  edge_id                         NUMBER;
  left_face                       NUMBER;
  right_face                      NUMBER;
  face_id                         NUMBER := face_no;
  delta                           NUMBER := 0.001;
  mbr_geom                        SDO_GEOMETRY;

  topo_entity_count               NUMBER := 100000;
  ok                              BOOLEAN;
  cache_name                      VARCHAR2(20) := Topology||'topomapcache';
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



        sql_stmt := 'SELECT sdo_geom.sdo_mbr(sdo_aggr_mbr(t.topogeom.get_geometry())) FROM ' ||
                    Schema || '.'||Clip_Face_table || ' t WHERE t.face_id = :1';

        Execute Immediate sql_stmt into mbr_geom using face_no;


        xLL := mbr_geom.sdo_ordinates(1)-delta;
        yLL := mbr_geom.sdo_ordinates(2)-delta;
        xUR := mbr_geom.sdo_ordinates(3)+delta;
        yUR := mbr_geom.sdo_ordinates(4)+delta;


        sdo_TOPO_MAP.LOAD_TOPO_MAP(cache_name,xLL,yLL,xUR,yUR, 'true');


--        status := SDO_TOPO_MAP.VALIDATE_TOPO_MAP(cache_name);
--        dbms_output.put_line('VALIDATE TOPO MAP:' ||status);

--  Get an improved "L" shaped edge (edge_id) that does not intersect the nearby
--  edge. It locates the edges from the face_id.

        new_geometry := get_better_edge(Topology,face_id,edge_id,pschema,Clip_face_table,InGeom_Column,tolerance);

 -- This puts this face back:
 -- exec reshape_ledge('Z606LS4',17171,0.4,'GZCPB_3','Z606LS4_clIP_FACE');
 --        execute immediate 'select geometry from GZCPB_3.Z606SP1_EDGE$ where edge_id=42329' into new_geometry;
--      new_geometry := SDO_GEOMETRY(2002, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 2, 1), SDO_ORDINATE_ARRAY( -124.258393167077471730408433359116315842,40.18427196394051748029596637934446334839, -124.258407, 40.184276, -124.26031, 40.185742, -124.26124,
--40.186223, -124.26174, 40.186795, -124.26195, 40.186887, -124.26294, 40.18778, -124.26499, 40.189405, -124.26628, 40.190022, -124.26777, 40.191373, -124.2709, 40.194234, -124.27176, 40.195379, -124.27212, 40.195745, -124.27313, 40.196454, -124.27555, 40.197508, -124.2763, 40.19808, -124.27695, 40.198378, -124.27934, 40.198973, -124.2828, 40.200164, -124.28307, 40.200416, -124.28337, 40.200439, -124.283852482412385143106803297996520996,40.20067100162546580577327404171228408813 ));
--         edge_id:=42329;
        if new_geometry is NULL then
          status := '************************************No close edge found';
        else
--         new_xys := new_geometry.sdo_ordinates;
--dbms_output.put_line('swapping ID ' || edge_id);
--         for ii in 1..new_xys.count loop
--            dbms_output.put_line('II ' || II || ' ' || new_xys(ii));
--         end loop;
         status := swap_edge_coords(Topology,edge_id,new_geometry);



        if status <> 'TRUE' then
          dbms_output.put_line('SWAP EDGE FAILED ' || status);
        else
          sdo_TOPO_MAP.COMMIT_TOPO_MAP();
          dbms_output.put_line('TOPO MAP COMMITTED for edge ' || edge_id);
          COMMIT;
        end if;

        end if;
        sdo_TOPO_MAP.CLEAR_TOPO_MAP(cache_name);
        sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);

        sql_stmt := 'SELECT sdo_geom.validate_geometry_with_context(t.topogeom.get_geometry(),:1) from '||
                     Schema||'.'|| Clip_face_table ||' t where t.face_id=:2';

        execute immediate sql_stmt into status using tolerance,face_no;
        dbms_output.put_line('FACE VALIDATION STATUS IS:' || status);

        if SUBSTR(status,1,5) = '13356' then
          ok := REMOVE_CLOSE_XYS(New_Geometry,tolerance);
          coord1 := pull(status,'[Coordinate ');
          dbms_output.put_line('cd ' || coord1);
          if ok and coord1 <> 1 then
          sdo_TOPO_MAP.CREATE_TOPO_MAP(Topology, cache_name, topo_entity_count,topo_entity_count,topo_entity_count);
          sdo_TOPO_MAP.LOAD_TOPO_MAP(cache_name,xLL,yLL,xUR,yUR, 'true');
           status := swap_edge_coords(Topology,edge_id,new_geometry);

           if status <> 'TRUE' then
             dbms_output.put_line('SWAP EDGE FAILED ' || status);
           else
             sdo_TOPO_MAP.COMMIT_TOPO_MAP();
            dbms_output.put_line('TOPO MAP COMMITTED for edge ' || edge_id);
            COMMIT;
           end if;

           sdo_TOPO_MAP.CLEAR_TOPO_MAP(cache_name);
           sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);
           execute immediate sql_stmt into status using tolerance,face_no;
          dbms_output.put_line('FACE VALIDATION STATUS IS:' || status);
        end if;
      end if;
  -- Update the 2 geometries on either side of the edge

      if status = 'TRUE' and new_geometry is NOT NULL then

        UPDATE_FACES_EACH_SIDE(edge_id,Clip_face_table,topology,tolerance);

      end if;
      return status;

END RESHAPE_LEDGE;
--
PROCEDURE DROP_EDGE_VERTEX(Topology VARCHAR2,edge_id NUMBER,vertex NUMBER) AS

-- Reshapes an edge by dropping a vertex (not the first or last !).
BEGIN
-- Just drop the vertex by specifying an illogical move of zero.
    Add_Move_drop_vertex(Topology,edge_id,vertex,0.0);
END;
--
PROCEDURE ADD_MOVE_DROP_VERTEX(Topology VARCHAR2,edge_id NUMBER,pvertex NUMBER, pmeasure NUMBER default NULL,new_point_geom MDSYS.SDO_GEOMETRY default NULL,Add_new_vertex VARCHAR2 default NULL) AS

-- Capabilities: Add, Move or Drop a vertex.

--              Move vertex on edge_id to a new position specified by measure
--             (goes from >zero to 1) or by a new node geometry
--
-- or           Drop the vertex altogether when measure is set to zero.

-- If measure is specified, this procedure will calculate the new_edge_geometry.
-- Just get the direction right (a measure of 0.1 is near the beginning of the
-- edge and a measure of 0.9 is near the end) so measure corresponds to the
-- direction you want vertex to move.
-- Example: vertex = 2 on an edge having 3 vertices with measure= 0.25 moves the
--          the middle vertex towards the end slightly(25% of distance 2 to 3).
--
--                  +----------------+-----------------+  Before
--                  +---------------------+------------+  After


  xLL                             NUMBER;
  yLL                             NUMBER;
  xUR                             NUMBER;
  yUR                             NUMBER;
  xx                              NUMBER;
  yy                              NUMBER;
  new_x                           NUMBER;
  new_y                           NUMBER;
  measure                         NUMBER;
  vertex                          NUMBER := pvertex;
  delta                           NUMBER := 0.001;

  topo_entity_count               NUMBER := 100000;
  cache_name                      VARCHAR2(20) := Topology||'topomapcache';
  tolerance                       NUMBER := 0.05;
  mbr_geom                        SDO_GEOMETRY;
  edge_geom                       SDO_GEOMETRY;
  seg_geom                        SDO_GEOMETRY;
  geom                            SDO_GEOMETRY;
  point_geom                      SDO_GEOMETRY;
  lrsgeom                         SDO_GEOMETRY;
  Point                           MDSYS.SDO_POINT_TYPE;

  Xys                             SDO_ORDINATE_ARRAY;
  Edge_Xys                        SDO_ORDINATE_ARRAY;
  sql_stmt                        VARCHAR2(4000);
  status                          VARCHAR2(4000);
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

 -- Verify the point is on or near the edge
--    If the caller specified a measure we just use that. If a new_node_geometry
--    was supplied, use that and then check the LRS.
      sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';
      execute immediate sql_stmt into edge_geom using edge_id;
      Edge_Xys := edge_geom.sdo_ordinates;

      if new_point_geom is NOT NULL then
         Point := new_point_geom.sdo_point;
         new_x := Point.x;
         new_y := Point.y;

-- We will find the new node using measure below
      elsif pmeasure is NOT NULL then
         measure := pmeasure;
      end if;

-- Drop the vertex if requested..

      if measure is NOT NULL and measure = 0.0 then
-- Check up on the caller
         if vertex <= 1 or vertex >= TRUNC(Edge_Xys.count/2) then
            sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);
            TRACK_APP_ERROR('MOVE_EDGE_VERTEX: vertex='||vertex||
                      ' Cannot drop start or end vertex!!');
         end if;
         for ii in vertex*2-1..Edge_xys.count-2 loop
           Edge_xys(ii) := Edge_xys(ii+2);
         end loop;
         Edge_xys.trim(2);

-- If the point is supposed to be on the edge then find an acceptable
-- LRS point with Oracle. But use LRS_locate_pt instead as Oracle
-- doesn't work for short edges.

      elsif measure is NOT NULL and measure <> 0.0 then
--        lrsgeom := sdo_lrs.convert_to_lrs_geom(edge_geom,0.,1.);
--        point_geom := sdo_lrs.Locate_pt(lrsgeom,measure);
--        Xys := point_geom.sdo_ordinates;
        measure := pmeasure - TRUNC(measure);
        if measure = 0.0 then
           measure := 0.5;
        end if;
--        dbms_output.put_line('pm ' || pmeasure || ' meas ' || measure || ' vtx ' || vertex);
        seg_geom := edge_geom;
        seg_geom.sdo_ordinates := MDSYS.SDO_ORDINATE_ARRAY(Edge_Xys(vertex*2-1),Edge_Xys(vertex*2),
                                     Edge_Xys(vertex*2+1),Edge_Xys(vertex*2+2));
        point_geom := LRS_locate_pt(measure,seg_geom,'N');
        new_x := point_geom.sdo_point.x; -- Xys(1);
        new_y := point_geom.sdo_point.y; --Xys(2);
        If Add_new_vertex ='Y' then
          Edge_xys.extend(2);
          for ii in REVERSE vertex*2-1..Edge_xys.count-2 loop
             Edge_xys(ii+2) := Edge_xys(ii);
          end loop;
          vertex := vertex+1;
        End if;
        Edge_Xys(vertex*2-1) := ROUND(new_x,9);
        Edge_Xys(vertex*2) :=   ROUND(new_y,9);

      else
        Edge_Xys(vertex*2-1) := ROUND(new_x,9);
        Edge_Xys(vertex*2) :=   ROUND(new_y,9);
      end if;

      edge_geom.sdo_ordinates := Edge_Xys;


      mbr_geom := sdo_geom.sdo_mbr(edge_geom);

      xLL := mbr_geom.sdo_ordinates(1)-delta;
      yLL := mbr_geom.sdo_ordinates(2)-delta;
      xUR := mbr_geom.sdo_ordinates(3)+delta;
      yUR := mbr_geom.sdo_ordinates(4)+delta;


      sdo_TOPO_MAP.LOAD_TOPO_MAP(cache_name,xLL,yLL,xUR,yUR, 'true');

      status := swap_edge_coords(Topology,edge_id,edge_geom);

      if status <> 'TRUE' then
          dbms_output.put_line('SWAP EDGE FAILED ' || status);
      else
        sdo_TOPO_MAP.COMMIT_TOPO_MAP();
        dbms_output.put_line('TOPO MAP COMMITTED for edge ' || edge_id);
        COMMIT;
      end if;

     sdo_TOPO_MAP.CLEAR_TOPO_MAP(cache_name);
     sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);

END;
--
FUNCTION LRS_LOCATE_PT(measure NUMBER, edge_geom MDSYS.SDO_GEOMETRY, limit_measure VARCHAR2 default 'Y') RETURN MDSYS.SDO_GEOMETRY AS

-- Locate a point along (or beyond) an edge's extent. Return the point geometry
-- at a certain measure.
-- For very short edges, Oracle LRS locate point function returns steps
-- rather than discrete interpolated values.

-- Method: calculates line parameter using a distance measure along the edge.
--         Caller specifies a measure in the range [0,1] to ensure the
--         new point falls along the extent of the original edge.
--         When the measure is outside this range the resulting point will be
--         beyond the original extent of the edge. Typical values outside the
--         range might be -0.1 or 1.1.
--         Caller must set limit_measure to 'N' to allow this behaviour.

   Edge_xys        MDSYS.SDO_ORDINATE_ARRAY := edge_geom.sdo_ordinates;
   gcds            MDSYS.SDO_LIST_TYPE;
   Point           MDSYS.SDO_GEOMETRY;

   x1              NUMBER;
   y1              NUMBER;
   x2              NUMBER;
   y2              NUMBER;
   total           NUMBER;
   t               NUMBER;
   dist_along      NUMBER;
   inter_vtx_len   NUMBER;
   dist_so_far     NUMBER := 0.0;
   n               PLS_INTEGER := TRUNC(Edge_xys.count/2);

   function limit(pval number,range_1 number,range_2 number) return number as
      val number := pval;
      begin
      if val < range_1 then
         val := range_1;
      elsif val > range_2 then
         val := range_2;
      end if;
      return val;
   end;
BEGIN

-- This function return the total length of an edge and the intervertex distances
-- starting in position 3. Position 2 is reserved for the index when you
-- request the longest segment length etc.

      gcds := GZ_QA.get_gcds(Edge_geom,'TOTAL');

      total := gcds(1);
      dist_along := measure*total;
--      dbms_output.put_line('total ' || total || ' dist-along ' || dist_along);
      x2 := Edge_Xys(1);
      y2 := Edge_Xys(2);

      Point := MDSYS.SDO_GEOMETRY(2001,edge_geom.SDO_SRID,
                  SDO_Point_type(Edge_Xys(n*2-1),Edge_xys(n*2),NULL),NULL,NULL);

      For ii in 2..n loop
       x1 := x2;
       y1 := y2;
       x2 := Edge_xys(ii*2-1);
       y2 := Edge_Xys(ii*2);
       inter_vtx_len := gcds(ii+1); -- see comment above

-- Locate the particular line segment that the measure falls within
-- to calculate a point along that segment. However, we allow the user to go
-- outside the range [0.1] when limit_measure='N'

--dbms_output.put_line('dist along ' || dist_along || ' compare ' || (dist_so_far + inter_vtx_len) );
       if (measure <= 0. and ii=1) or (measure >= 1. and ii=n) or
          dist_along <= (dist_so_far + inter_vtx_len) then
          t := (dist_along-dist_so_far)/inter_vtx_len;  -- line parameter on segment

-- We may limit t to ensure we don't go beyond the range of the current segment or the edge.

          if UPPER(limit_measure) = 'Y' then
            t := limit(t,0.0,1.0);
          end if;
          Point.SDO_Point := MDSYS.SDO_POINT_TYPE((1.-t)*x1 + t*x2,(1.-t)*y1 + t*y2,NULL);
          exit;
       end if;

       dist_so_far := dist_so_far + gcds(ii+1);
      end loop;

      RETURN Point;
END LRS_LOCATE_PT;
--
PROCEDURE MOVE_EDGE_NODE(Topology VARCHAR2,edge_id NUMBER,node_id NUMBER, pmeasure NUMBER default NULL,new_node_geom MDSYS.SDO_GEOMETRY) AS

-- Move a node on an edge. If measure is specified, this procedure will calculate
-- the new_node_geometry. Measure is specificed from zero to 1 to move within
-- the current extent of the edge. Measure varies from zero at the start node to
-- 1 near the end node. When using measure get the correspondence right (a measure
-- of 0.1 is near the beginning of the edge and a measure of 0.9 is near the end)
-- so measure corresponds to the node use specify to move.
-- Typical calls for the start node 272 are:
--    exec GZ_TOPOFIX.move_edge_node('Z634SC',437,272,0.1,NULL);
-- or exec GZ_TOPOFIX.move_edge_node('Z634SC',437,272,-0.1,NULL);
-- and for the end node 273
-- exec GZ_TOPOFIX.move_edge_node('Z634SC',437,273,0.9,NULL);
--exec GZ_TOPOFIX.move_edge_node('Z634SC',437,272,1.1,NULL);


  xLL                             NUMBER := 1E10;
  yLL                             NUMBER := 1E10;
  xUR                             NUMBER := -1E10;
  yUR                             NUMBER := -1E10;

  xx                              NUMBER;
  yy                              NUMBER;
  new_x                           NUMBER;
  new_y                           NUMBER;
  anedge_id                       NUMBER;
  measure                         NUMBER := pmeasure;
  n                               PLS_INTEGER;
  m                               PLS_INTEGER;
  Xys                             SDO_ORDINATE_ARRAY;
  Edges_Xys                       SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY();
  edges_around                    SDO_NUMBER_ARRAY;
  EDGE_COORDS                     SDO_EDGE_ARRAY := SDO_EDGE_ARRAY();
  Point                           MDSYS.SDO_POINT_TYPE;

  delta                           NUMBER := 0.001;
  epsilon                         NUMBER := 0.00000002;
  mbr_geom                        SDO_GEOMETRY;
  edge_geom                       SDO_GEOMETRY;
  geom                            SDO_GEOMETRY;
  old_node_geom                   SDO_GEOMETRY;
  Edge_Xys                        SdO_ORDINATE_ARRAY;
  lrsgeom                         MDSYS.SDO_GEOMETRY;
  Point_geom                      MDSYS.SDO_GEOMETRY;
  topo_entity_count               NUMBER := 100000;
  cache_name                      VARCHAR2(20) := Topology||'topomapcache';
  tolerance                       NUMBER := 0.05;
  sql_stmt                        VARCHAR2(4000);

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


      edges_around := sdo_topo_map.get_node_star(Topology,NULL,node_id);

      n := edges_around.count;
      edge_coords.extend(n);

      sql_stmt := 'SELECT geometry from ' || Topology || '_NODE$ where node_id=:1';
      execute immediate sql_stmt into geom using node_id;

      Point := geom.sdo_point;
      xx := Point.x;
      yy := Point.y;

-- Verify the point is on or near the edge
--    If the caller specified a measure we just use that. If a new_node_geometry
--    was supplied, use that and then check the LRS.
      sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';
      execute immediate sql_stmt into edge_geom using edge_id;
      Edge_Xys := edge_geom.sdo_ordinates;

      if new_node_geom is NOT NULL then
         Point := new_node_geom.sdo_point;
         new_x := Point.x;
         new_y := Point.y;

-- First find the points position along the path from start node to end_node


         m := Edge_Xys.count;
         if pmeasure is NULL then
            if Edge_Xys(1) <> Edge_Xys(m-1) then
              measure := (new_x-Edge_xys(1))/(Edge_Xys(m-1) - Edge_xys(1));
            else
              measure := (new_y-Edge_xys(2))/(Edge_Xys(m) - Edge_xys(2));
            end if;

         end if;
-- We will find the new node using measure below
      elsif pmeasure is NOT NULL then
         measure := pmeasure;
      end if;

-- If the point is supposed to be on the edge then find an acceptable
-- LRS locate_point.
      if measure is NOT NULL then

--        dbms_output.put_line('measure   ' ||TRUNC(measure,10));
--        dbms_output.put_line('xx   ' || xx);
--        dbms_output.put_line('yy   ' ||yy);
--        lrsgeom := sdo_lrs.convert_to_lrs_geom(edge_geom,0.,100.);
--        point_geom := sdo_lrs.Locate_pt(lrsgeom,ii*10.);
--        Xys := point_geom.sdo_ordinates;
        point_geom := LRS_locate_pt(measure,edge_geom,'N');
        new_x := point_geom.sdo_point.x; --Xys(1);
        new_y := point_geom.sdo_point.y; --Xys(2);
      end if;
--      dbms_output.put_line('xx   ' || xx);
--      dbms_output.put_line('newx ' || new_x);
--      dbms_output.put_line('yy   ' || yy);
--      dbms_output.put_line('newy ' || new_y);

      sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';
      For ii in 1..n loop

        anedge_id := abs(edges_around(ii));
        execute immediate sql_stmt into geom using anedge_id;

-- Build up a composite MBR for all the edges

        mbr_geom := sdo_geom.sdo_mbr(geom);
        if mbr_geom.sdo_ordinates(1) < xLL then
          xLL := mbr_geom.sdo_ordinates(1);
        end if;
        if mbr_geom.sdo_ordinates(3) > xUR then
          xUR := mbr_geom.sdo_ordinates(3);
        end if;
        if mbr_geom.sdo_ordinates(2) < yLL then
          yLL := mbr_geom.sdo_ordinates(2);
        end if;
        if mbr_geom.sdo_ordinates(4) > xUR then
          yUR := mbr_geom.sdo_ordinates(4);
        end if;
        Xys := geom.sdo_ordinates;
        Edges_Xys.trim(Edges_xys.count);
        Edges_Xys.extend(Xys.count);
        For ij in 1.. Xys.count loop
           Edges_Xys(ij) := Xys(ij);
-- And replace the start or end vertex with the new position
           if MOD(ij,2)=1 and ABS(Xys(ij) - xx) < epsilon and ABS(Xys(ij+1) - yy) < epsilon then
              Edges_Xys(ij) := new_x; --+epsilon;
              Edges_Xys(ij+1) := new_y; --+epsilon;
              dbms_output.put_line('updated edge coordinates for' || anedge_id);
           end if;
        End Loop;
        edge_coords(ii) := Edges_Xys;
      End loop;

      xLL := xLL-delta;
      yLL := yLL-delta;
      xUR := xUR+delta;
      yUR := yUR+delta;


      sdo_TOPO_MAP.LOAD_TOPO_MAP(cache_name,xLL,yLL,xUR,yUR, 'true');

      SDO_TOPO_MAP.MOVE_NODE(NULL,node_id,edge_coords);

      sdo_TOPO_MAP.COMMIT_TOPO_MAP();
      dbms_output.put_line('TOPO MAP COMMITTED for node ' || node_id);
      COMMIT;


      sdo_TOPO_MAP.CLEAR_TOPO_MAP(cache_name);
      sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);


END MOVE_EDGE_NODE;
--
FUNCTION ADD_NODE_TO_EDGE(Topology VARCHAR2,edge_id NUMBER, measure NUMBER, new_node_geom MDSYS.SDO_GEOMETRY default NULL,p_is_new_shape_point VARCHAR2 default 'TRUE') RETURN NUMBER AS

-- Add a node to an edge. Caller may specify its position using measure instead
-- of giving a point geometry. In this case the new vertex will lie
-- at measure where measure is a number in the range [0 to number of existing
-- vertices]. So for example, 0,5 lies halfway between vertex 1 and vertex 2, and
-- similarly 3.25 lies a quarter of the distance between vertices 3 and 4.

   new_node_id    NUMBER;
  xLL                             NUMBER;
  yLL                             NUMBER;
  xUR                             NUMBER;
  yUR                             NUMBER;
  x                               NUMBER;
  y                               NUMBER;
  coord_index                     NUMBER;

  delta                           NUMBER := 0.001;
  point_geom                      SDO_GEOMETRY := new_node_geom;
  edge_geom                       SDO_GEOMETRY;
  mbr_geom                        SDO_GEOMETRY;
  Xys                             MDSYS.SDO_ORDINATE_ARRAY;
  topo_entity_count               NUMBER := 100000;
  cache_name                      VARCHAR2(20) := Topology||'topomapcache';
  tolerance                       NUMBER := 0.05;
  sql_stmt                        VARCHAR2(4000);
  is_new_shape_point              VARCHAR2(5) := UPPER(p_is_new_shape_point);

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

-- If the point is supposed to be on the edge then find an acceptable
-- point with LRS_locate_pt.
      sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';
      execute immediate sql_stmt into edge_geom using edge_id;
      if measure is NOT NULL then

--        dbms_output.put_line('measure   ' ||TRUNC(measure,10));

--        lrsgeom := sdo_lrs.convert_to_lrs_geom(edge_geom,0.,100.);
--        point_geom := sdo_lrs.Locate_pt(lrsgeom,ii*10.);
--        Xys := point_geom.sdo_ordinates;
        point_geom := LRS_locate_pt(measure,edge_geom,'N');
        is_new_shape_point := 'TRUE';
        coord_index := TRUNC(measure);
      elsif is_new_shape_point ='FALSE' and new_node_geom is NOT NULL then

        x := new_node_geom.sdo_point.x;
        y := new_node_geom.sdo_point.y;
        Xys := edge_geom.sdo_ordinates;
        for ii in 1..TRUNC(XYs.count/2) loop
          if x =  Xys(ii*2-1) and y = Xys(ii*2) then
             coord_index := TRUNC(ii/2);  -- this makes it zero based
             exit;
          end if;
        end loop;
      end if;
    mbr_geom := sdo_geom.sdo_mbr(point_geom);

    xLL := mbr_geom.sdo_ordinates(1)-delta;
    yLL := mbr_geom.sdo_ordinates(2)-delta;
    xUR := mbr_geom.sdo_ordinates(1)+delta;
    yUR := mbr_geom.sdo_ordinates(2)+delta;


    sdo_TOPO_MAP.LOAD_TOPO_MAP(cache_name,xLL,yLL,xUR,yUR, 'true');

    new_node_id := SDO_TOPO_MAP.ADD_NODE(NULL,edge_id,point_geom,coord_index,is_new_shape_point);

    sdo_TOPO_MAP.COMMIT_TOPO_MAP();
    dbms_output.put_line('TOPO MAP COMMITTED for node ' || new_node_id);
    COMMIT;


    sdo_TOPO_MAP.CLEAR_TOPO_MAP(cache_name);
    sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);

    DBMS_OUTPUT.PUT_LINE('New node created: ' ||new_node_id);
    RETURN new_node_id;
END ADD_NODE_TO_EDGE;
--
PROCEDURE ADD_EDGE_VERTEX(Topology VARCHAR2,edge_id NUMBER, measure NUMBER, new_vertex_geom MDSYS.SDO_GEOMETRY default NULL) AS

-- Add a vertex to an edge. Caller may specify its position using measure instead
-- of giving a point geometry. In this case the new vertex will lie
-- at measure where measure is a number in the range [0 to number of existing
-- vertices]. So for example, 0,5 lies halfway between vertex 1 and vertex 2, and
-- similarly 3.25 lies a quarter of the distance between vertices 3 and 4.

   vertex                        NUMBER;

BEGIN

     vertex := TRUNC(measure);
     Add_Move_drop_vertex(Topology,edge_id,vertex,measure,new_vertex_geom,'Y');

END ADD_EDGE_VERTEX;
--
PROCEDURE MOVE_EDGE_VERTEX(Topology VARCHAR2,edge_id NUMBER, vertex NUMBER,measure NUMBER, new_vertex_geom MDSYS.SDO_GEOMETRY default NULL) AS

-- Move a vertex on an edge. Caller may specify its position using measure instead
-- of giving a point geometry. In this case the new vertex will lie
-- at measure where measure is a number in the range [0 to number of existing
-- vertices]. So for example, 0,5 lies halfway between vertex 1 and vertex 2, and
-- similarly 3.25 lies a quarter of the distance between vertices 3 and 4.


BEGIN

     Add_Move_drop_vertex(Topology,edge_id,vertex,measure,new_vertex_geom);

END MOVE_EDGE_VERTEX;
--
PROCEDURE DROP_EDGE(Topology VARCHAR2,p_edge_id NUMBER) AS

-- Code to remove an edge from the topology.

  xLL                             NUMBER;
  yLL                             NUMBER;
  xUR                             NUMBER;
  yUR                             NUMBER;

  delta                           NUMBER := 0.001;
  edge_geom                       SDO_GEOMETRY;
  mbr_geom                        SDO_GEOMETRY;
  topo_entity_count               NUMBER := 100000;
  cache_name                      VARCHAR2(20) := Topology||'topomapcache';
  sql_stmt                        VARCHAR2(4000);

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

    sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';
    execute immediate sql_stmt into edge_geom using p_edge_id;

    mbr_geom := sdo_geom.sdo_mbr(edge_geom);

    xLL := mbr_geom.sdo_ordinates(1)-delta;
    yLL := mbr_geom.sdo_ordinates(2)-delta;
    xUR := mbr_geom.sdo_ordinates(3)+delta;
    yUR := mbr_geom.sdo_ordinates(4)+delta;


    sdo_TOPO_MAP.LOAD_TOPO_MAP(cache_name,xLL,yLL,xUR,yUR, 'true');

    SDO_TOPO_MAP.REMOVE_EDGE(NULL, p_edge_id);

    sdo_TOPO_MAP.COMMIT_TOPO_MAP();
    dbms_output.put_line('TOPO MAP COMMITTED');
    COMMIT;


    sdo_TOPO_MAP.CLEAR_TOPO_MAP(cache_name);
    sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);

    DBMS_OUTPUT.PUT_LINE('Edge dropped: ' ||p_edge_id);

END DROP_EDGE;
--
PROCEDURE DROP_NODE(Topology VARCHAR2,p_node_id NUMBER) AS

-- Code to drop a node from the topology.

  xLL                             NUMBER := 1.E10;
  yLL                             NUMBER := 1.E10;
  xUR                             NUMBER := -1.E10;
  yUR                             NUMBER := -1.E10;
  anedge_id                       NUMBER;
  n                               PLS_INTEGER;
  Edges_around                    SDO_NUMBER_ARRAY;

  delta                           NUMBER := 0.001;
  geom                            SDO_GEOMETRY;
  mbr_geom                        SDO_GEOMETRY;
  topo_entity_count               NUMBER := 100000;
  cache_name                      VARCHAR2(20) := Topology||'topomapcache';
  sql_stmt                        VARCHAR2(4000);

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

    edges_around := sdo_topo_map.get_node_star(Topology,NULL,p_node_id);
    n := edges_around.count;

    sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';

    For ii in 1..n loop

        anedge_id := abs(edges_around(ii));
        execute immediate sql_stmt into geom using anedge_id;

-- Build up a composite MBR for all the edges

        mbr_geom := sdo_geom.sdo_mbr(geom);
        if mbr_geom.sdo_ordinates(1) < xLL then
          xLL := mbr_geom.sdo_ordinates(1);
        end if;
        if mbr_geom.sdo_ordinates(3) > xUR then
          xUR := mbr_geom.sdo_ordinates(3);
        end if;
        if mbr_geom.sdo_ordinates(2) < yLL then
          yLL := mbr_geom.sdo_ordinates(2);
        end if;
        if mbr_geom.sdo_ordinates(4) > xUR then
          yUR := mbr_geom.sdo_ordinates(4);
        end if;

      End loop;

      xLL := xLL-delta;
      yLL := yLL-delta;
      xUR := xUR+delta;
      yUR := yUR+delta;


    sdo_TOPO_MAP.LOAD_TOPO_MAP(cache_name,xLL,yLL,xUR,yUR, 'true');

    SDO_TOPO_MAP.REMOVE_NODE(NULL, p_node_id);

    sdo_TOPO_MAP.COMMIT_TOPO_MAP();
    dbms_output.put_line('TOPO MAP COMMITTED');
    COMMIT;


    sdo_TOPO_MAP.CLEAR_TOPO_MAP(cache_name);
    sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);

    DBMS_OUTPUT.PUT_LINE('Node removed: ' ||p_node_id);

END DROP_NODE;
--
PROCEDURE ADD_EDGE_BETWEEN_NODES(Topology VARCHAR2,p_old_node_id NUMBER, p_new_node_id NUMBER, new_edge_geom MDSYS.SDO_GEOMETRY) AS

-- Just add an edge between two existing nodes.

  new_edge_id                     NUMBER;
  xLL                             NUMBER;
  yLL                             NUMBER;
  xUR                             NUMBER;
  yUR                             NUMBER;

  delta                           NUMBER := 0.001;
  mbr_geom                        SDO_GEOMETRY;
  topo_entity_count               NUMBER := 100000;
  cache_name                      VARCHAR2(20) := Topology||'topomapcache';
  tolerance                       NUMBER := 0.05;

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



    mbr_geom := sdo_geom.sdo_mbr(new_edge_geom);

    xLL := mbr_geom.sdo_ordinates(1)-delta;
    yLL := mbr_geom.sdo_ordinates(2)-delta;
    xUR := mbr_geom.sdo_ordinates(3)+delta;
    yUR := mbr_geom.sdo_ordinates(4)+delta;


    sdo_TOPO_MAP.LOAD_TOPO_MAP(cache_name,xLL,yLL,xUR,yUR, 'true');

    new_edge_id := SDO_TOPO_MAP.ADD_EDGE(NULL, p_old_node_id,p_new_node_id,new_edge_geom);

    sdo_TOPO_MAP.COMMIT_TOPO_MAP();
    dbms_output.put_line('TOPO MAP COMMITTED for edge ' || new_edge_id);
    COMMIT;


    sdo_TOPO_MAP.CLEAR_TOPO_MAP(cache_name);
    sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);

    DBMS_OUTPUT.PUT_LINE('New edge created: ' ||new_edge_id);

END ADD_EDGE_BETWEEN_NODES;
--
FUNCTION SWAP_EDGE_COORDS(Topology VARCHAR2,Edge_id NUMBER, Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY) RETURN VARCHAR2 AS
/**
--##############################################################################
-- Program Name: swap_edge_coords
-- Author: Sidey Timmins
-- Creation Date: 2010
--
-- Usage:
--   This program has 3 required parameters :
--
--     REQUIRED Parameter:
--        Topology            - The name of the Topology.
--        Edge_id             - The edge_id to reshape
--        Geometry            - the revised geometry to replace the existing one
-- Purpose:
--        Update a topology by reshaping a single edge. Returns 'TRUE' or an
--        Oracle error message.

-- Modification History:
--        10/21/2010          Niranjan Reddy suggested using a different entry point
--                            for CHANGE_EDGE_COORDS with 3 extra arguments to avoid
--                            Oracle bug SR 3-2169673431
--        01/2011             Changed allow_iso_moves to TRUE to avoid yet another Oracle bug
--        Feb 2011            BACK to false!!
--###############################################################################
*/
     status            VARCHAR2(4000);
     v_errm            VARCHAR2(4000);
     v_code            NUMBER;
     moved_iso_nodes   SDO_NUMBER_ARRAY;
     moved_iso_edges   SDO_NUMBER_ARRAY;

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
FUNCTION TEST_BETTER_EDGE(Topology VARCHAR2,face_id NUMBER,Clip_face_table VARCHAR2, InGeom_Column VARCHAR2,tolerance NUMBER default 0.05) RETURN MDSYS.SDO_LIST_TYPE AS
seg1 number;
seg2 number;
  nearbys   MDSYS.SDO_LIST_TYPE;
BEGIN
   nearbys := FIND_CLOSEST_EDGES(Topology,face_id,Clip_face_table,InGeom_column,seg1,seg2,tolerance);
END;
--
FUNCTION GET_BETTER_EDGE (pTopology VARCHAR2,face_id NUMBER,edge_id IN OUT NOCOPY NUMBER,pInSchema VARCHAR2,Clip_face_table VARCHAR2,InGeom_Column VARCHAR2 default 'SDOGEOMETRY',tolerance NUMBER default 0.05,decim_digits NUMBER default 7) RETURN MDSYS.SDO_GEOMETRY AS


-- Returns an edge geometry that is improved by increasing the distance away
-- from a close nearby edge.

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
   poly_geom        MDSYS.SDO_GEOMETRY;
   nearby_geometry  MDSYS.SDO_GEOMETRY;
   geom             MDSYS.SDO_GEOMETRY;
   geometry1  MDSYS.SDO_GEOMETRY;
   geometry2  MDSYS.SDO_GEOMETRY;
   intersections    MDSYS.SDO_GEOMETRY;
   Xys              MDSYS.SDO_ORDINATE_ARRAY;
   nearby_Xys       MDSYS.SDO_ORDINATE_ARRAY;
   they             MDSYS.SDO_ORDINATE_ARRAY;
   edge_nos         MDSYS.SDO_LIST_TYPE;
   sql_stmt         VARCHAR2(4000);
   state            VARCHAR2(2);
  xv                NUMBER;
  yv                NUMBER;
  xtry              NUMBER;
  ytry              NUMBER;
  xnear             NUMBER;
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
  where_is          NUMBER;
  seg1              NUMBER;
  seg2              NUMBER;
  SRID              NUMBER;
  low               NUMBER;
  hi                NUMBER;


  near_seg          PLS_INTEGER;
  vertex            PLS_INTEGER;
  loops             PLS_INTEGER := 0;
  j                 PLS_INTEGER;
  len               PLS_INTEGER;
  next              PLS_INTEGER :=0;
  n                 PLS_INTEGER;
  thousand          NUMBER := 1000000.;
BEGIN


-- Begin by finding the closest edges in the face that are the problem

   Topology := Inschema ||'.' ||Topology;


   nearbys := FIND_CLOSEST_EDGES(Topology,face_id,Clip_face_table,InGeom_column,seg1,seg2,tolerance);



   if nearbys is NULL then
   dbms_output.put_line('no close nearby edges for face ' || face_id);

     sql_stmt := 'SELECT ' || Ingeom_column||' from ' || Clip_face_table || ' where face_id=:1';
     execute IMMEDIATE sql_stmt into Poly_geom using face_id;

     edge_ids := sdo_topo.get_face_boundary(Topology,face_id);
     sql_stmt := 'SELECT geometry from ' ||  Topology || '_EDGE$ where edge_id=:1';


--dbms_output.put_line('edge_id count ' || edge_ids.count);

      For ii in 1..edge_ids.count Loop
        edge_id := abs(edge_ids(ii));
--        dbms_output.put_line('ID ' || edge_id);
        execute IMMEDIATE sql_stmt into geometry using edge_id;
        edge_nos := match_edge(poly_geom,edge_id,geometry,x0,y0,len);
        low := edge_nos(1);
        hi :=  edge_nos(2);
        if low > hi then
          low := hi;
          hi := edge_nos(1);
        end if;
--        dbms_output.put_line('seg1 ' || seg1 || ' seg2 ' || seg2 || ' low ' || low || ' hi ' || hi);

        if seg1 >= low and seg1 <= hi and
           seg2 >= low and seg2 <= hi
        then
        Xys := geometry.sdo_ordinates;
        dist := Get_Short_dist(geometry,2,where_is);
--        dbms_output.put_line('D ' || dist || ' id ' ||edge_id || ' tol ' || tolerance);
        if dist < tolerance+0.02 then
-- drop a vertex
         where_is := where_is*2+3;

         For ii in where_is..Xys.count loop
            Xys(ii-2) := Xys(ii);
         End Loop;
         Xys.trim(2);
         end if;
         new_geometry := MDSYS.SDO_GEOMETRY(2002,8265,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),Xys);
         RETURN new_geometry;
        end if;
      End Loop;
      RETURN NULL;
   end if;

-- Here we process the nearby edge which we want to alter.
-- We have its edge_id, its problem vertex

   edge_id := nearbys(5);
      sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';
   EXECUTE IMMEDIATE sql_stmt INTO Geometry using edge_id;
   Xys := geometry.sdo_ordinates;
   SRID := geometry.sdo_srid;
--   dbms_output.put_line('x1 ' || xys(1));
--   dbms_output.put_line('x2 ' || xys(2));
--   dbms_output.put_line('x3 ' || xys(3));
--   dbms_output.put_line('x4 ' || xys(4));
--   dbms_output.put_line('x5 ' || xys(5));
--   dbms_output.put_line('x6 ' || xys(6));


   vertex := nearbys(6);
   if vertex = 1 and Xys.count > 4 then
     vertex := 2;
   end if;
--   dbms_output.put_line('VV ' || vertex || ' edge ' || edge_id || ' nearby ' || nearby_id);

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
--   dbms_output.put_line('nearby ' || nearby_id || ' nearseg is ' || (near_seg) || ' count ' || nearby_xys.count);


   dist := distance_fcn(xnear,ynear,xv,yv,SRID);
--   dbms_output.put_line('OLD dist ' || round(dist,10) || ' dist_found ' || round(dist_found,10));
--   dbms_output.put_line('Xn ' || round(xnear,10));
--   dbms_output.put_line('Yn ' || round(ynear,10));

   if yv < 50. then
      t := dist/0.11 * tolerance/0.05;
   else
      t := dist/(10.*0.33) * tolerance / 0.05;
   end if;

   if t > 2 then
      t := 2.;
  elsif t < 0.2 then
      t := 0.2;
  end if;
--  dbms_output.put_line('TT ' || t || ' vertex ' || vertex || ' xys ' || xys.count);
   x3 := nearby_Xys(near_seg*2-1);
   y3 := nearby_Xys(near_seg*2);
   x4 := nearby_Xys(near_seg*2+1);
   y4 := nearby_Xys(near_seg*2+2);
   if x3 = x4 and y3 = y4 then
    x4 := nearby_Xys(near_seg*2+3);
    y4 := nearby_Xys(near_seg*2+4);
   end if;



   x0 := Xys(vertex*2-1);
   y0 := Xys(vertex*2);
   xtry := Xys(vertex*2+1);
   ytry := Xys(vertex*2+2);


--   dbms_output.put_line('X0 ' || round(x0,10));
--   dbms_output.put_line('Y0 ' || round(y0,10));
--   dbms_output.put_line('Xt ' || round(xtry,10));
--   dbms_output.put_line('Yt ' || round(ytry,10));
--   dbms_output.put_line('X3 ' || round(x3,10));
--   dbms_output.put_line('Y3 ' || round(y3,10));
--   dbms_output.put_line('X4 ' || round(x4,10));
--   dbms_output.put_line('Y4 ' || round(y4,10));
-- Check to see if it crosses the other line

   check_it := simple_intersect(x0,y0,xtry,ytry,x3,y3,x4,y4,xi,yi);

   if check_it > 0.999999999999 or check_it < 0.000000000001 then
      check_it := -1;
   end if;
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
--     dbms_output.put_line('it intersects!!!' || round(they(1),10) || ' ' || round(they(2),10));
   end if;
--   dbms_output.put_line('check_it ' ||check_it);
   way2 := orient2d(x3,y3,x4,y4,x0,y0);
-- The lines intersect if the clockwiseness is different
--   dbms_output.put_line('ways ' ||way1 || ' ' || way2);
   if (way2 > 0. and way1 < 0.) or (way1 >0. and way2 < 0.) then
     check_it:=1;
   end if;
   while try >=0. and loops < 8 loop
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

--   dbms_output.put_line('checkit ' || check_it ||' t ' || round(t,10) || ' vtx ' ||vertex);
--   dbms_output.put_line('Xtry ' || round(xtry,10));
--   dbms_output.put_line('Ytry ' || round(ytry,10));
--   dbms_output.put_line('Xn ' || round(xnear,10));
--   dbms_output.put_line('Yn ' || round(ynear,10));
--   dbms_output.put_line('Xv ' || round(xv,10));
--   dbms_output.put_line('Yv ' || round(yv,10));

   dist := distance_fcn(xnear,ynear,xtry,ytry,SRID);
--   dbms_output.put_line('loop ' || loops || ' NEW dist ' || round(dist,10));

   Xys(vertex*2-1) := xtry;
   Xys(vertex*2  ) := ytry;

   if (x0 = x3 and y0 = y3) then
    try  := -1;
--    dbms_output.put_line('SET try ' || try);
   else
    try := simple_intersect(x0,y0,xtry,ytry,x3,y3,x4,y4,xi,yi);
   end if;
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
--     dbms_output.put_line('it intersects!!!' || round(they(1),10) || ' ' || round(they(2),10));
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

END GET_BETTER_EDGE;
--
FUNCTION FIND_CLOSEST_EDGES(pTopology VARCHAR2,face_id NUMBER,Clip_face_table VARCHAR2, InGeom_Column VARCHAR2,seg1 IN OUT NOCOPY NUMBER,seg2 IN OUT NOCOPY NUMBER,tolerance NUMBER) RETURN MDSYS.SDO_LIST_TYPE AS

-- Find closest edges for a face.

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
   left_face          NUMBER;
   right_face         NUMBER;
   pu                 NUMBER;
   edge_id            NUMBER;
   near_edge_id       NUMBER;
   dist               NUMBER;
   SRID               NUMBER;
   check_dist         NUMBER := 2.;
   last_dist          NUMBER := 10.;


   ok                 PLS_INTEGER;
   sql_stmt           VARCHAR2(4000);
   s   number;
   az1 number;
   az2 number;
   m12 number;
   s12 number;
   gcd_K number;
   status             VARCHAR2(4000);

-- Remove a duplicate vertex since this muddles things up

  FUNCTION Remove_DUP_vertex(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY) RETURN PLS_INTEGER AS
     where_is  PLS_INTEGER:= 0;
  Begin
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
  End;

BEGIN

--  get the face geometry and validate it

   sql_stmt := 'select t.'||InGeom_Column ||' from '||Clip_face_table || ' t where t.face_id=:1';
   execute immediate sql_stmt into Geom using face_id;

   status := sdo_geom.validate_geometry_with_context(geom,tolerance);
   dbms_output.put_line(status);

   if status = 'TRUE' then
     RETURN NULL;
   end if;

-- Get the particular edges Oracle complains about
--13349 [Element <1>] [Ring <1>][Edge <7>][Edge <11>]
--
-- It doesn't actually give the edges - it specifies the segments !!

   dbms_output.put_line(status);
   SRID := geom.sdo_srid;
   seg1 := pull(status,'[Edge ');
   seg2 := pull(status,'[Edge ',2);


-- Get an ordered list of edges surrounding a face. Negative means the
-- edge direction must be reversed to build a counter clockwise geometry.

   edge_ids := sdo_topo.get_face_boundary(Topology,face_id);

-- Flag the edges that touch the universal face


    sql_stmt := 'SELECT left_face_id,right_face_id from '|| Topology || '_edge$ where edge_id=:1';


-- the edge ids are negative when they go backwards

   For ii in 1..Edge_ids.count Loop
    edge_id := ABS(Edge_ids(ii));
    EXECUTE IMMEDIATE sql_stmt INTO left_face,right_face using edge_id;

-- Now make them negative if they touch the universal face

    if left_face = -1 or right_face = -1 then
      edge_id := -edge_id;
    end if;
    Edge_ids(ii) := edge_id;
   End loop;

-- Use two loops to compare edges and find the closest approach

   sql_stmt := 'SELECT geometry from ' || Topology || '_EDGE$ where edge_id=:1';


   For ii in 1..Edge_ids.count Loop

     edge_id := ABS(Edge_ids(ii));
--     dbms_output.put_line('ii ' || ii || ' edge ' || edge_ids(ii));
     EXECUTE IMMEDIATE sql_stmt INTO Geom using edge_id;

     Edge_Xys := Geom.Sdo_Ordinates;
     dbms_output.put_line('id ' || edge_id);
     For ij in 1.. Edge_ids.count Loop

-- This is the nearby we expect to alter

       near_edge_id := ABS(Edge_ids(ij));

-- Dont compare the subject edge with itself and don't move edges on universal face

       if edge_id <> near_edge_id and Edge_ids(ij) > 0 then

         EXECUTE IMMEDIATE sql_stmt INTO NearbyGeom using near_edge_id;

         Nearby_Xys := NearbyGeom.Sdo_Ordinates;
-- We can run into a problem when we drop a node onto segment one of a line

         xtest2 := Nearby_Xys(1);
         ytest2 := Nearby_Xys(2);

--         dbms_output.put_line('Nearby edge ' || edge_ids(ij) || ' count ' || nearby_xys.count);
         ok := Remove_DUP_vertex(Nearby_Xys);

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
--dbms_output.put_line('edge ' || edge_ids(ii) || ' count ' || edge_xys.count ||  ' kk ' || kk);
-- we cannot move the end point
--  ORA-29532: Java call terminated by uncaught Java exception:
---oracle.spatial.topo.InvalidTopoOperationException: Changed-edge coordinates
---cannot move endpoints

             if kk >= 2 and (x1 <> x0 or y1 <> y0) and ((x0 <> xtest1 or y0 <> ytest1) and (x0 <> xtest2 or y0 <> ytest2)) then
             dist := Is_pt_on_Segment(x0,y0,xtest1,ytest1,xtest2,ytest2,xnear,ynear,pu);


              IF xnear is NOT NULL THEN
--               dbms_output.put_line('edge_id ' ||edge_id ||'  d ' || ROUND(dist,16) || ' xnear ' || ROUND(xnear,8) || ' ynear ' || ROUND(ynear,8));
              dist := distance_fcn(x0,y0,xnear,ynear,SRID);
--       dbms_output.put_line('edge_id ' ||edge_id ||'  d ' || ROUND(dist,16) || ' LAST ' || ROUND(last_dist,16));
--              s := geodesic.inverse(y0,x0,ynear,xnear,gcd_K,az1,az2,m12);
--              dbms_output.put_line('kk ' || kk ||'jj ' || jj|| '  d ' || ROUND(dist,8) || ' karney ' || round(gcd_k,8) || ' Error ' || round(gcd_k-dist,8));
            if dist < last_dist then

--              dbms_output.put_line('edge_id ' ||near_edge_id ||'  d ' || ROUND(dist,8) || ' found at ' || kk);
--              dbms_output.put_line(' dist ' || dist);
--              dbms_output.put_line(' x0 ' || round(x0,16));
--              dbms_output.put_line(' y0 ' || round(y0,16));
--              dbms_output.put_line(' xn ' || round(xnear,16));
--              dbms_output.put_line(' yn ' || round(ynear,16));
--              dbms_output.put_line(' xt ' || round(xtest1,10));
--              dbms_output.put_line(' yt ' || round(ytest1,10));
--              dbms_output.put_line(' xT ' || round(xtest2,10));
--              dbms_output.put_line(' yT ' || round(ytest2,10));


-- Build a list, the nearby edge to be altered, its problem segment, the near point
-- on that segment, the other edge, its vertex that is the problem and the distance
-- between them.
                if kk > 2 then
                Nearbys :=MDSYS.SDO_LIST_TYPE(near_edge_id,(jj-1.),xnear,ynear,edge_id,(kk-1.),dist);
                else
                 Nearbys :=MDSYS.SDO_LIST_TYPE(edge_id,(kk-1.),xnear,ynear,near_edge_id,(jj-1),dist);
                end if;
                last_dist := dist;
             end if;
             END IF;
             end if;
              if jj > 2 and (xtest1 <> xtest2 or ytest1 <> ytest2) and ((xtest1 <> x0 or ytest1 <> y0) and (xtest1 <> x1 or ytest1 <> y1)) then
             dist := Is_pt_on_Segment(xtest1,ytest1,x0,y0,x1,y1,xnear,ynear,pu);
                  dist := Is_pt_on_Segment(xtest1,ytest1,x0,y0,x1,y1,xnear,ynear,pu);

                 if xnear is NOT NULL then
                 dist := distance_fcn(xtest1,ytest1,xnear,ynear,SRID);
                 if dist < last_dist then
                  Nearbys :=MDSYS.SDO_LIST_TYPE(edge_id,(kk-1.),xnear,ynear,near_edge_id,(jj-1.),dist);
                  last_dist := dist;
                  end if;
                 end if;

           end if;
         end loop;
       End Loop;
       End If;

    End Loop;

   End Loop;

--   for ii in 1..Nearbys.count loop
--     dbms_output.put_line('ii ' || ii || ' value ' || nearbys(ii));
--   end loop;
   If Nearbys.count >0 then
     RETURN Nearbys;
   End If;
   RETURN NULL;

END FIND_CLOSEST_EDGES;
--
FUNCTION GET_SHORT_DIST(Geom IN MDSYS.SDO_GEOMETRY,pgap NUMBER,where_is IN OUT NOCOPY NUMBER) RETURN NUMBER  AS

--Find shortest gap between coordinates which are gap apart.
--Example, gap=1 find vertices too close, gap=2 find a sliver triangle

  Xys               MDSYS.SDO_ORDINATE_ARRAY:= Geom.SDO_ORDINATES;
  N                 PLS_INTEGER;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;
  xnext             NUMBER;
  ynext             NUMBER;
  gap               NUMBER := pgap*2-2;
  seg12             NUMBER;
  s12               NUMBER;
  az0               NUMBER;
  az1               NUMBER;
  m12               NUMBER;
  distance          NUMBER;
  pos               NUMBER;
  small             NUMBER := 1.E20;
  SRID              NUMBER := Geom.sdo_SRID;
BEGIN

  n := Xys.count/2;
  x2 := Xys(1);
  y2 := Xys(2);
  where_is := 0;

  For ii in 2..n-gap Loop
    x1 := x2;
    y1 := y2;
    x2 := Xys(ii*2-1);
    y2 := Xys(ii*2);
    pos := (ii-1)*2 + gap;
    xnext := Xys(pos+1);
    ynext := Xys(pos+2);
--    seg12 := geodesic.inverse(y1,x1,ynext,xnext,s12,az0,az1,m12);
--    Distance := s12;
      distance := distance_fcn(x1,y1,xnext,ynext,SRID);

    if distance < small then
       small := distance;
       where_is := ii-1.;
--       dbms_output.put_line('ii ' || (ii-1) || ' ' || round(small,4));
    end if;

  End Loop;
  RETURN small;

END GET_SHORT_DIST;
--
FUNCTION Is_pt_on_Segment(xtest IN OUT NOCOPY NUMBER,ytest IN OUT NOCOPY NUMBER, -- test point
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,  -- the line
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                       xnear IN OUT NOCOPY NUMBER,ynear IN OUT NOCOPY NUMBER,
                       pu IN OUT NOCOPY NUMBER) RETURN NUMBER DETERMINISTIC AS
/*
--##############################################################################
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
--##############################################################################
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
--##############################################################################
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
--##############################################################################
*/

   ccwerrboundA  NUMBER:= 1.76324152623343126195310480583336851684E-38;

   detleft               NUMBER;
   detright              NUMBER;
   det                   NUMBER;
   detsum                NUMBER;
   errbound              NUMBER;


BEGIN


  detleft  := (TRUNC(paX,10) - TRUNC(pcX,10)) * (TRUNC(pbY,10) - TRUNC(pcY,10));
  detright := (TRUNC(paY,10) - TRUNC(pcY,10)) * (TRUNC(pbX,10) - TRUNC(pcX,10));
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
-- However, we could use factoring or exact arithmetic to compute the
-- determinant more exactly.
--  det := c_orient2d_adapt(pax, pAy, pbx, pby, pcx, pcy, detsum);

  RETURN det;

END ORIENT2D;
--
FUNCTION GET_Angles(Geom IN MDSYS.SDO_GEOMETRY,smallest VARCHAR2 default 'NO') RETURN MDSYS.SDO_LIST_TYPE  AS

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

  n := Xys.count/2;
  Angles.extend(n+1); -- smallest comes first, then its offset, then
  --                      all angles. Note there are n-1 vertices when
  --                      the xys.count/2 = n

  x2 := Xys(n*2-3);
  y2 := Xys(n*2-2);

  x3 := Xys(1);
  y3 := Xys(2);


  For ii in 1..n-1 Loop
    x1 := x2;
    y1 := y2;
    x2 := x3;
    y2 := y3;
    x3 := Xys(ii*2+1);
    y3 := Xys(ii*2+2);

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
       abs_dx2_dy2 < ABS(dx3) + ABS(dy3) or smallest <> 'YES' then

      Angle1 := GZ_QA.Faster_atan2(dy,dx,TRUE);
--      Angle1 := New_arctan(dy,dx,TRUE);
--angle1 := atan2(dy,dx) * 57.29577951308232087679815481410517033235;

      Angle2 := GZ_QA.Faster_atan2(dy3,dx3,TRUE);
--      Angle2 := New_arctan(dy3,dx3,TRUE);
--      angle2 := atan2(dy2,dx2) * 57.29577951308232087679815481410517033235;
--    if ii = 6 then
--      dbms_output.put_line('ii ' || ii || ' angle1 ' || angle1 || ' ' ||angle2);
--    end if;
      Angle := ABS(angle1-angle2);

      if angle > 180. then
        angle := 360. -angle;
      end if;
--           dbms_output.put_line('ii ' || ii || ' angle ' || round(angle,4));
      if angle < small then
        small := angle;
        Angles(1) := small;
        Angles(2) := (ii);
      end if;
      Angles(ii+2) := ROUND(angle,4);
   End if;
  End Loop;


  RETURN Angles;


END GET_AngleS;
--
FUNCTION CHECK_FACE_FOR_13349(face_id NUMBER,Intable VARCHAR2,InGeom_Column VARCHAR2,schema VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05,UFS VARCHAR default 'FALSE') RETURN NUMBER AS

/*
--##############################################################################
--Program Name: Check_face_for_13349
--Author: Sidey Timmins
--Creation Date: 11/xx/2011
--
--Usage:
  -- Call this function from inside another PL/SQL program.  This function
  -- has 7 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      face_id        - the face_id from face$ which has the 13349 error
  --      Intable        - the input clip_table
  --      InGeomcolumn   - the geometry column for the face
  --      schema         - the schema we are in
  --      Topology       - the topology name
  --      tolerance      - the Oracle tolerance the face failed at
  --      UFS            - the universal face switch,. 'True' allows
  --                       edge updates on the universal face (UF), 'FALSE' does not.
--
-- Purpose: This function reshapes an edge by adding (if necessary) a vertex
--          or moving the existing middle vertex and updates the Topology.
--          Then updates the polygons on either side of the edge.

-- Method: It moves the vertex about 1/8 of the length of the segment it selects
--         as being the longest. Movement is perpendicular to the segment.
--         It checks whether the change has been accepted in the topology by
--         checking to see if the edge length has increased. Return a status
--         of 1 for failure, 0 for OK.
--##############################################################################
*/

-- SQL> 'select face_id,sdo_geom.validate_geometry_with_context(t.sdogeometry,0.05) '
--      'from GZDEC10ST24.Z624CL_CLIP_face t where '
--      'SUBSTR(sdo_geom.validate_geometry_with_context(t.sdogeometry,0.05),1,5) = '13349';

  edges       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  edge_ids    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  nuf_edge    MDSYS.SDO_LIST_TYPE;
  angles      MDSYS.SDO_LIST_TYPE;
  start_nodes MDSYS.SDO_LIST_TYPE;
  end_nodes   MDSYS.SDO_LIST_TYPE;
  vertx_nos    MDSYS.SDO_LIST_TYPE;
  dists       MDSYS.SDO_LIST_TYPE;
  edge_angles MDSYS.SDO_LIST_TYPE;

  edges_around SDO_NUMBER_ARRAY;
  vertices    MDSYS.SDO_ORDINATE_ARRAY:= MDSYS.SDO_ORDINATE_ARRAY();
  Xys         MDSYS.SDO_ORDINATE_ARRAY;
  Ord         MDSYS.SDO_ORDINATE_ARRAY;
  poly_geom   MDSYS.SDO_GEOMETRY;
  edge_geom   MDSYS.SDO_GEOMETRY;
  anedge_geom MDSYS.SDO_GEOMETRY;
  lrsgeom     MDSYS.SDO_GEOMETRY;
  point       MDSYS.SDO_GEOMETRY;
  geom        MDSYS.SDO_GEOMETRY;
  point_to_move MDSYS.SDO_GEOMETRY;

  sql_stmt    VARCHAR2(4000);
  sql_stmt2   VARCHAR2(4000);

  result     VARCHAR2(400);
  edge1       NUMBER;
  edge2       NUMBER;

  left_face   NUMBER;
  right_face  NUMBER;
  edge_angle  NUMBER;
  x           NUMBER;
  y           NUMBER;

  x1          NUMBER;
  y1          NUMBER;
  x2          NUMBER;
  y2          NUMBER;

  new_x       NUMBER;
  new_y       NUMBER;
  node1       NUMBER;
  node2       NUMBER;
  node1_angle NUMBER;
  node2_angle NUMBER;
  angle1      NUMBER;
  angle2      NUMBER;
  shortest    NUMBER;
  short_edge  NUMBER;
  edge_id     NUMBER;
  edge_len    NUMBER;

  edge_id1    NUMBER :=0.;
  edge_id2    NUMBER :=0.;
  len         NUMBER;
  kount1      NUMBER;
  kount2      NUMBER;
  count1      NUMBER;
  edge_to_move          NUMBER;
  t           NUMBER;
  delta       NUMBER;
  short1      NUMBER := 1000000.;
  short2      NUMBER := 1000000.;
  short_tol   NUMBER := 0.08;
  short_id1   NUMBER;
  short_id2   NUMBER;
  status      NUMBER := 0;
  start_node  NUMBER;
  end_node    NUMBER;
  node_to_move NUMBER;
  measure     NUMBER;
  universal_count   NUMBER;
  universal_count2  NUMBER;

  old_len     NUMBER;
  new_len     NUMBER;

  next        PLS_INTEGER := 0;
  fpos        PLS_INTEGER := 0;
  pos         PLS_INTEGER;

BEGIN




 if short_tol < tolerance then
   short_tol := tolerance * 1.6;
 end if;

-- Process these kind of errors
-- 13349 [Element <1>] [Ring <1>][Edge <1>][Edge <2>]

-- Get the poly geometry and its validation status

  sql_stmt := 'SELECT t.' || InGeom_Column ||',SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(t.' || InGeom_Column ||',:1) from ' || schema ||
        '.'||InTable || ' t WHERE t.FACE_ID =:2';
--  dbms_output.put_line(sql_stmt);
  execute IMMEDIATE sql_stmt into poly_geom,result using tolerance,face_id;


-- it still probably fails at 0.05
--  IF SUBSTR(result,1,5) <> '13349' THEN
--     dbms_output.put_line('Face ' || face_id || ' does not have a 13349 error');
--     RETURN status;
--  END IF;

-- These are the straddling sides (possibly for an angle that is too small)
-- that straddle a problem edge. The problem edge (side 3) may have 3
-- vertices and thus have 1 segment too short or more usually the whole edge
-- is too short.

--           +                                 +
--          / \                               / \
--  side1  /   \  side2                      /   \
--        /     \                           /face \
--                                          -------
--             and we want to figure out    side 3
--             and the nodes on this edge

   edge1 := pull(result,'[Edge ');
   edge2 := pull(result,'[Edge ',2);

  sql_stmt2 := 'SELECT count(*) from ' || schema ||'.'||Topology ||
                 '_EDGE$ t WHERE t.START_NODE_ID =:1 OR t.END_NODE_ID =:2';
  execute IMMEDIATE sql_stmt2 into kount1 using node1,node1;

   sql_stmt2 := 'SELECT count(*) from ' || schema ||'.'||Topology ||
                 '_EDGE$ t WHERE t.START_NODE_ID =:1 OR t.END_NODE_ID =:2';

   sql_stmt := 'SELECT edge_id,start_node_id,end_node_id from ' || schema ||
        '.'||Topology || '_EDGE$ t WHERE t.LEFT_FACE_ID =:2 OR t.RIGHT_FACE_ID =:2';

-- Now get each of the edges that bound this face

    execute IMMEDIATE sql_stmt BULK COLLECT into edge_ids,start_nodes,end_nodes using face_id,face_id;

    sql_stmt := 'SELECT geometry,left_face_id,right_face_id from ' || schema||'.' || Topology || '_EDGE$ where edge_id=:1';

    vertices.extend(4);
    shortest := 1.E10;

-- Now gather as much information as possible about the edges, their lengths
-- and the polygon angles. Not all of this information may be used but what the..

    angles :=  get_angles(poly_geom);

    For ii in 1.. edge_ids.count Loop
        edge_id := edge_ids(ii);
        execute IMMEDIATE sql_stmt into anedge_geom,left_face,right_face using edge_id;

-- Here we match an edge's coordinates to the polygon. The vertex nos we get
-- back are in the polygon and may not match the edge.

        vertx_nos := match_edge(poly_geom,edge_id,anedge_geom,x,y,len);
--dbms_output.put_line('side ' || vertx_nos(1) || ' side two ' || vertx_nos(2) || ' len ' || ROUND(len,6));

        dists := GZ_QA.get_gcds(anedge_geom,'TOTAL');
        for ij in 3..dists.count loop
          if dists(ij) < len then
             len := dists(ij);
             dbms_output.put_line('shortest len ' || ROUND(len,6));
          end if;
        end loop;
-- The search here is for the edge of the face with the shortest segment distance.
-- It could be for the side1 and side2 that Oracle complained about

        if len < shortest then
           shortest := len;
           short_edge := edge_id;
           pos := abs(vertx_nos(1));

           angle1 := angles(pos+2);
--            dbms_output.put_line('set angel 1 ' || angle1  || ' side ' || pos);
           if pos = angles.count-2 then
              angle2 := angles(3);
           else
--              dbms_output.put_line('angles.count ' || angles.count || ' pos ' || pos);
              angle2 := angles(pos+3);
--               dbms_output.put_line('set angel 2 ' || angle2);
           end if;
           if vertx_nos(1) < vertx_nos(2) then  -- edge goes in the same direction as polygon
              node1_angle := angle1;
              node2_angle := angle2;
           else
              node2_angle := angle1;
              node1_angle := angle2;
           end if;
           node1 := start_nodes(ii);
           node2 := end_nodes(ii);
           edge_geom := anedge_geom;
           edge_angles := get_angles(anedge_geom);
           edge_angle := edge_angles(1);
           edge_len := sdo_geom.sdo_length(edge_geom,0.05,'unit=meter');
--           dbms_output.put_line('edge Id' || short_edge || ' len ' || ROUND(len,6));
-- Get the multiplicity
           execute IMMEDIATE sql_stmt2 into kount1 using node1,node1;
           execute IMMEDIATE sql_stmt2 into kount2 using node2,node2;

-- Now we have an edge with 2 nodes, node1 and node2, it length edge_len and we
-- know the angles at these nodes that the other sides make:
-- node1_angle and node2_angle

           vertices := edge_geom.sdo_ordinates;

--           dbms_output.put_line('side_no ' || vertx_nos(1) || ' side2 ' || vertx_nos(2) || ' id ' || edge_id || ' node1  ' || node1 || ' node2 ' || node2 || ' count ' || edge_geom.sdo_ordinates.count);
--   dbms_output.put_line('node1_angle ' || node1_angle || ' node2_angel ' || node2_angle);
        end if;
    End Loop;


-- Check edges that touch node1 and node2 and see if any of those are also short
-- and also if any edge touches the universal face (UF =-1)
-- First the nodestar for node1

   edges_around := sdo_topo_map.get_node_star(Topology,NULL,node1);

   for ii in 1..edges_around.count loop
      edge_id := abs(edges_around(ii));
      if edge_id <> short_edge then
        execute IMMEDIATE sql_stmt into anedge_geom,left_face,right_face using edge_id;
        len := sdo_geom.sdo_length(anedge_geom,0.05,'unit=meter');
        if len < short1 then
           short1 := len;
           short_id1 := edge_id;
        end if;

-- Since picture may look like this:
--         -1 universal face
--
--            node1
--            +--------------
--           /|
--          / |
--         /  |
-- node2   +__+
--        /
--       /
  -- Keep track of any faces that are on the universal face that touch node1

        if left_face = -1 or right_face =-1 then
           edge_id1 := edge_id;
        end if;
      end if;
   end loop;

-- Then the nodestar for node2

   edges_around := sdo_topo_map.get_node_star(Topology,NULL,node2);

   for ii in 1..edges_around.count loop
      edge_id := abs(edges_around(ii));
      if edge_id <> short_edge then
        execute IMMEDIATE sql_stmt into anedge_geom,left_face,right_face using edge_id;
        len := sdo_geom.sdo_length(anedge_geom,0.05,'unit=meter');
        if len <> short2 then
           short2 := len;
           short_id2 := edge_id;
        end if;
    -- Keep track of any faces that are on the universal face that touch node2

        if left_face = -1 or right_face =-1 then
           edge_id2 := edge_id;
        end if;
      end if;
   end loop;



--  Now count all the edges that come to node1 and node2 and find out if we are
--  restricted by the universal face

    sql_stmt := 'SELECT count(*) from ' || schema ||
        '.'||Topology || '_EDGE$ t WHERE (t.START_NODE_ID =:1 OR t.END_NODE_ID =:2)' ||
                         ' AND (t.LEFT_FACE_ID =-1 OR t.RIGHT_FACE_ID =-1)';

    execute IMMEDIATE sql_stmt into universal_count using node1,node1;
    execute IMMEDIATE sql_stmt into universal_count2 using node2,node2;

--     dbms_output.put_line('node1 ' || node1 || ' node2 ' || node2);

--    dbms_output.put_line('Short edge ' || short_edge || ' D ' || ROUND(shortest,6));
--    dbms_output.put_line('kount1 ' || kount1 || ' kount2 ' || kount2);
--    dbms_output.put_line('ukount1 ' || universal_count || ' ukount2 ' ||universal_count2);

    edges.extend(3);
    edges(1) := short_edge;

-- Begin making choices. Assume for the present the UFS (Universal Face switch)
-- is off (FALSE) so we must avoid altering edges on it.

-- Here we are interior so it is moot

    IF universal_count = 0 and universal_count2 = 0 THEN
      if kount1 <= kount2 and short1 > short_tol then
        edges(2) := node1;   -- preferred move comes first
        edges(3) := node2;
      elsif kount2 <= kount1 and (short2 > short_tol or short2 > short1) then
        edges(2) := node2;
        edges(3) := node1;
      else
        edges(2) := node1;
        edges(3) := node2;
      end if;

    ELSE

-- Universal face is involved. Try to avoid moving edges on it.
-- UFS setting is applicable but even if it is allowable to move nodes on the
-- UF, be as little disruptive as possible.

-- These tests are just to ensure we don't move a node on an edge that is
-- itself short thereby creating another problem. Short_tol is chosen
-- considerably bigger than 0.05 (0.08) so one can hopefully move about
-- 0.05 towards an edge that is itself just a bit more than 0.05.

      if universal_count = 0 and short1 > short_tol then
        edges(2) := node1;   -- preferred move comes first
        edges(3) := node2;
--        dbms_output.put_line('a');
      elsif universal_count2 = 0 and (short2 > short_tol or short2 >short1) then
        edges(2) := node2;
        edges(3) := node1;
--        dbms_output.put_line('b');
      elsif universal_count = 0 then
        edges(2) := node1;   -- preferred move comes first
        edges(3) := node2;
--        dbms_output.put_line('c');
      elsif universal_count2 = 0 then
        edges(2) := node2;
        edges(3) := node1;
--        dbms_output.put_line('d');

      else

--     Both ends of the shortest edge are on the universal face.
--     We have 2 choices: either 1) reshape the NUF edge if it exists
--                                  (there may be 2 or more edges making up
--                                   this connection from node1 to node2)
--                        or     2) slide a node on one of the UF edges


 -- Is there another edge going between node1 and node2?
--                     Universal face
--               ----------------------
--              /                      \
--     node1   +------------------------+ node2
--            /           NUF  edge      \
-- UF edge 1 /                            \  UF edge2

         sql_stmt2 := 'SELECT count(*) from ' || schema ||'.'||Topology ||
                       '_EDGE$ t WHERE (t.LEFT_FACE_ID <> -1 AND t.RIGHT_FACE_ID =:1)'||
                              ' OR (t.LEFT_FACE_ID =:1 AND t.RIGHT_FACE_ID <> -1)';
         execute IMMEDIATE sql_stmt2 into count1 using face_id,face_id;
--         dbms_output.put_line('count 1 ' || count1 || ' FACE ' || face_id);

         if count1 <> 0 then
           sql_stmt2 := 'SELECT t.EDGE_ID from ' || schema ||'.'||Topology ||
                       '_EDGE$ t WHERE (t.LEFT_FACE_ID <> -1 AND t.RIGHT_FACE_ID =:1)'||
                              ' OR (t.LEFT_FACE_ID =:1 AND t.RIGHT_FACE_ID <> -1)';
          execute IMMEDIATE sql_stmt2 BULK COLLECT into nuf_edge using face_id,face_id;

      -- Add a vertex or reshape the bend in the NUF edge
          if count1 = 1 then   -- BUG should be 1
            edge_to_move := nuf_edge(1);

--            Dbms_output.put_line('reshaping bend ' || node_to_move || ' for edge ' || edge_to_move);
            status := Add_Reshape_Bend(edge_to_move,Poly_Geom, face_id,Intable,InGeom_column,schema,topology,tolerance);
            RETURN status;

          end if;
-- This elsif is the only untested branch, although add_reshape_bend is tested.
         elsif UFS = 'FALSE' then
-- Finally we use the Universal Face switch. The code outside this if will
-- move a node on the outside face but it is not allowed here.

-- Use the same algorithm though to reshape an edge. Reshape the one with the
-- smallest angle.

          if node1_angle > node2_angle then
            node_to_move := node1;
            edge_to_move := edge_id1;
            edges(2) := node1;
          else
            node_to_move := node2;
            edge_to_move := edge_id2;
            edges(2) := node2;
          end if;
--          Dbms_output.put_line('RESHAPING bend ' || node_to_move || ' for edge ' || edge_to_move);
          status := Add_Reshape_Bend(edge_to_move,Poly_Geom, face_id,Intable,InGeom_column,schema,topology,tolerance);
          RETURN status;
         end if;

 -- We are now going to move a node on the universal face and we choose the
 -- one with the largest angle inside the polygon at either node1 or node2

--                     Universal face
--               ---------------------
--              / node angles         \
--             / 1        and         2\
--      node1  +------------------------+ node2
--            /        NUF  edge         \
--  edge_id1 /                            \ edge_id2


-- This code is not to be executed so QUIT!!

        RETURN 'FALSE';

        if node1_angle > node2_angle then
          node_to_move := node1;
          edge_to_move := edge_id1;
          edges(2) := node1;
        else
          node_to_move := node2;
          edge_to_move := edge_id2;
          edges(2) := node2;
        end if;
--   Dbms_output.put_line('MOVING node ' || node_to_move || ' nod1 angel ' || node1_angle || ' nod2 angle ' || node2_angle);
        edges(1) := edge_to_move;

-- we can move either end of the edge but don't want a boomarang situation
-- where the edge we shorten is very short already.


        sql_stmt := 'SELECT geometry,start_node_id,end_node_id from ' || schema||'.' || Topology ||
                   '_EDGE$ where edge_id=:1';
        execute IMMEDIATE sql_stmt into edge_geom,start_node,end_node using edge_to_move;
        len := sdo_geom.sdo_length(edge_geom,0.05,'unit=meter');
--        dbms_output.put_line('len ' || len || ' id ' || edge_id);

        vertices := edge_geom.sdo_ordinates;
      end if;    -- end of the universal face involved

    END IF;

    edge_to_move := edges(1);
--  dbms_output.put_line('node to move ' || node_to_move || ' edge to move ' || edge_to_move);

   if universal_count = 0 or universal_count2 = 0 then
  --  dbms_output.put_line('v1 ' || vertices(1) || ' v2 ' || vertices(2));
      node_to_move := edges(2);
      if edges(2) = node1 then
        x1 := vertices(1);
        y1 := vertices(2);
        x2 := vertices(3);
        y2 := vertices(4);
      else
        x2 := vertices(vertices.count-3);
        y2 := vertices(vertices.count-2);
        x1 := vertices(vertices.count-1);
        y1 := vertices(vertices.count);
      end if;

      len := shortest;
--      dbms_output.put_line('len ' || len || ' Tol ' || tolerance);
      if len > tolerance then -- it failed
        delta := len*1.5;      -- so we are generous 0.5   -> to 0.75
      else                     -- its < tolerance
  --      if abs(len-tolerance) < 0.01 then
         delta := tolerance * 1.5;     -- again we are generous
  --      else
  --        delta := (tolerance/len -1) * 1.5;
  --      end if;
      end if;

    else
      if edges(2) = start_node then
        x1 := vertices(1);
        y1 := vertices(2);
        x2 := vertices(3);
        y2 := vertices(4);
      else
        x2 := vertices(vertices.count-3);
        y2 := vertices(vertices.count-2);
        x1 := vertices(vertices.count-1);
        y1 := vertices(vertices.count);
      end if;
      delta := tolerance * 1.5;     -- again we are generous
    end if;
    --
-- we want to move beyond node (x1,y1) from (x2,y2) so t > 1
--    dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
--    dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2);



-- When we are moving a node on the UF
-- Use LRS to insure we don't choose a position slightly into the universal face.
--dbms_output.put_line('edge_id1 ' || edge_id1 || ' edge_id2 ' || edge_id2);
    if edge_to_move = edge_id1 or edge_to_move = edge_id2 then

    t := 0.95;
    measure := t;
--    dbms_output.put_line('t ' || t || 'len ' || len ||  ' delta ' || delta);
    new_x := t*x1 + (1.-t)*x2;
    new_y := t*y1 + (1.-t)*y2;
--    dbms_output.put_line('new x ' || new_x || ' new_y ' || new_y);

      geom := mdsys.sdo_geometry(2002,8265,NULL,sdo_elem_info_array(1,2,1),
                   mdsys.sdo_ordinate_array(x2,y2,x1,y1));
      lrsgeom := sdo_lrs.convert_to_lrs_geom(geom,0.,1.);
      point := sdo_lrs.Locate_pt(lrsgeom,measure);
      Ord := point.sdo_ordinates;
      new_x := Ord(1);
      new_y := Ord(2);

    else
      t := 1.0 +delta/len;

--    dbms_output.put_line('t ' || t || 'len ' || len ||  ' delta ' || delta);
    new_x := t*x1 + (1.-t)*x2;
    new_y := t*y1 + (1.-t)*y2;

    end if;


--   dbms_output.put_line('new x ' || new_x || ' new_y ' || new_y);
--   dbms_output.put_line('node_to_move ' || node_to_move  || ' edge_to_move ' || edge_to_move);
--==============================================================================

 -- Ready to move a node

  point_to_move := MDSYS.sdo_geometry(2001,8265,MDSYS.SDO_POINT_TYPE(new_x,new_y,NULL),NULL,NULL);


  sql_stmt := 'SELECT geometry from ' || schema||'.' || Topology || '_EDGE$ where edge_id=:1';
  execute IMMEDIATE sql_stmt into edge_geom using edge_to_move;
  Xys := edge_geom.sdo_ordinates;

  old_len := Distance_fcn(Xys(1),Xys(2),xys(xys.count-1),Xys(Xys.count));
  --
  --  Now check if it moved by checking the edge length
  --

  GZ_TOPO_UTIL.GZ_MOVE_NODE(topology,node_to_move,point_to_move);

 -- Now get a new length


  execute IMMEDIATE sql_stmt into edge_geom using edge_to_move;
  Xys := edge_geom.sdo_ordinates;

  new_len := Distance_fcn(Xys(1),Xys(2),xys(xys.count-1),Xys(Xys.count));
--  dbms_output.put_line('old_len ' || old_len || ' new_len ' || new_len);

--==============================================================================
-- If the length has not changed, the node has NOT been moved!


  if old_len = new_len then
    status := 1;                       -- it did not move, so don't update any more geometries
  else

-- More SUCCESS checking
-- Now check the face this edge supposedly fixed

    sql_stmt := 'SELECT sdo_geom.validate_geometry_with_context(a.topogeom.get_geometry(),:1) from '||Schema||
                '.' || Intable ||' a where a.face_id=:2';
    EXECUTE IMMEDIATE sql_stmt into result using tolerance,face_id;
    dbms_output.put_line(result);


-- COMPLETE SUCCESS
-- Update all of the geometries that surround node1. Remember the faces done, so we only do them once
    if result = 'TRUE' then
          UPDATE_FACES_AROUND(node_to_move,Intable, InGeom_column,SCHEMA,topology,tolerance);
    end if;
  end if;
--==============================================================================

  RETURN status;

END CHECK_FACE_FOR_13349;
--
FUNCTION ADD_RESHAPE_BEND(edge_to_move NUMBER, poly_geom MDSYS.SDO_GEOMETRY,face_id NUMBER,Intable VARCHAR2,InGeom_Column VARCHAR2,schema VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05) RETURN NUMBER AS

/*
--##############################################################################
--Program Name: Add_Reshape_Bend
--Author: Sidey Timmins
--Creation Date: 01/06/2012
--
--Usage:
  -- Call this function from inside another PL/SQL program.  This function
  -- has 8 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      edge_to_move   - edge to be updated
  --      poly_geom      - the face geometry
  --      face_id        - the face_id from face$
  --      Intable        - the input clip_table
  --      InGeomcolumn   - the geometry column for the face
  --      schema         - the schema we are in
  --      Topology       - the topology name
  --      tolerance      - the Oracle tolerance at which the face failed.
--
-- Purpose: This function reshapes an edge by adding (if necessary) a vertex
--          or moving the existing middle vertex and updates the Topology.
--          Then updates the polygons on either side of the edge.

-- Method: It moves the vertex about 1/8 of the length of the segment it selects
--         as being the one to alter. Movement is perpendicular to the segment.
--         It checks whether the change has been accepted in the topology by
--         checking to see if the edge length has increased. Return a status
--         of 1 for failure, 0 for OK.
--##############################################################################
*/


  Xys         MDSYS.SDO_ORDINATE_ARRAY;
  edge_geom   MDSYS.SDO_GEOMETRY;
  areas       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  point_to_move MDSYS.SDO_GEOMETRY;
  PolyXys     MDSYS.SDO_ORDINATE_ARRAY;
  PolyInfo    MDSYS.SDO_ELEM_INFO_ARRAY;

  start_node      NUMBER;
  end_node        NUMBER;
  xm              NUMBER;
  ym              NUMBER;
  x1              NUMBER;
  y1              NUMBER;
  dist            NUMBER;
  xnew            NUMBER;
  ynew            NUMBER;
  xnew2           NUMBER;
  ynew2           NUMBER;
  xc              NUMBER;
  yc              NUMBER;
  factor          NUMBER;
  old_len         NUMBER;
  new_len         NUMBER;
  node_to_move    NUMBER;
  m               PLS_INTEGER;
  drop_it         PLS_INTEGER :=0;
  result         VARCHAR2(400);
  status          NUMBER := 0.0;
  sql_stmt        VARCHAR2(4000);
  sql_stmt3       VARCHAR2(4000);



BEGIN


  sql_stmt3 := 'SELECT geometry,start_node_id,end_node_id from ' || schema||'.' || Topology ||
               '_EDGE$ where edge_id=:1';
   execute IMMEDIATE sql_stmt3 into edge_geom,start_node,end_node using edge_to_move;

   Xys := edge_geom.sdo_ordinates;
   old_len := sdo_geom.sdo_length(edge_geom,0.05,'unit=meter');
   dbms_output.put_line('edge_to_move ' ||edge_to_move || ' xy '|| xys.count);
   PolyXys := Poly_Geom.sdo_ordinates;
   PolyInfo := Poly_geom.sdo_elem_info;
   areas := GZ_QA.Centroid(PolyXys,PolyInfo,xc,yc);

--   dbms_output.put_line('Area ' || areas(1) );
--  dbms_output.put_line('xc ' || xc || ' yc ' || yc);
--                   +
--                  /|
--                 / |
--                /  |
--        0.05   /   |
--              /    |  d = sqrt(0.05*0.05 - x*x)
--             /     |
--            /      |
--           /       |
--          *--------+-----*
--               x

   if Xys.count = 4 then
      Xys.extend(2);
      Xys(5) := Xys(3);
      Xys(6) := Xys(4);
      xm := 0.5*(Xys(1)+Xys(3));
      ym := 0.5*(Xys(2)+Xys(4));
--      dbms_output.put_line('AV x ' || xys(3) || ' AV_y ' || xys(4));
      x1 := Xys(1);
      y1 := Xys(2);
--      dist := Short_Oracle_dist(x1,y1,xm,ym);
      factor := 0.125; --sqrt(0.06*0.06 -dist*dist)/dist;
      xnew := xm+ (y1-ym)*factor;
      ynew := ym + (xm-x1)*factor;
      xnew2 := xm + (ym-y1)*factor;
      ynew2 := ym + (x1-xm)*factor;
      if Short_Oracle_dist(xc,yc,xnew,ynew) < Short_Oracle_dist(xc,yc,xnew2,ynew2)  then
         xnew := xnew2;
         ynew := ynew2;
      end if;

      Xys(3) := xnew;
      Xys(4) := ynew;
--       dbms_output.put_line('New x ' || xys(3) || ' new_y ' || xys(4));
    elsif xys.count > 4 then

      m := Xys.count;

 -- First check if we can drop a vertex.

      xm := Xys(1);
      ym := Xys(2);
      For ii in 2..TRUNC((Xys.count-2)/2) loop
        x1 := xm;
        y1 := ym;
        xm := Xys(ii*2-1);
        ym := Xys(ii*2);
        dist := Distance_fcn(x1,y1,xm,ym);
        if dist < 0.05 then
           drop_it := ii*2-1;
           exit;
        end if;
      End Loop;

-- This code works but we may still have to add a vertex midway because
-- the resulting edge is >0.05 meters but Oracle reports a 13349 error.
-- Example face 318 on Z634CL edge 626
--      drop_it :=0;
      if drop_it <> 0 then

         For ii in drop_it..Xys.count-2 loop
          Xys(ii) := Xys(ii+2);
         End Loop;
         Xys.trim(2);

      elsif Distance_fcn(Xys(1),Xys(2),Xys(3),Xys(4)) >
         Distance_fcn(Xys(m-1),Xys(m),Xys(m-3),Xys(m-2)) then
        xm := 0.5*(Xys(1)+Xys(3));
        ym := 0.5*(Xys(2)+Xys(4));
        x1 := Xys(1);
        y1 := Xys(2);
--        dist := Distance_fcn(x1,y1,xm,ym);
        factor := 0.125; --sqrt(0.06*0.06 -dist*dist)/dist;
--         dbms_output.put_line('FACTOR ' || factor || ' dist  ' || dist);
        xnew := xm + (y1-ym)*factor;
        ynew := ym + (xm-x1)*factor;
--        dbms_output.put_line('New xx ' || xnew || ' new_y ' || ynew);
        xnew2 := xm + (ym-y1)*factor;
        ynew2 := ym + (x1-xm)*factor;
        if Distance_fcn(xc,yc,xnew,ynew) < Distance_fcn(xc,yc,xnew2,ynew2)  then
           xnew := xnew2;
           ynew := ynew2;
--            dbms_output.put_line('CHOSE New x2 ' || xnew2 || ' new_y2 ' || ynew2);
        end if;
         Xys(3) := xnew;
         Xys(4) := ynew;

      else
--        dbms_output.put_line('m-1 ' || xys(m-1) || ' m_y ' || xys(m));
--        dbms_output.put_line('m-3 ' || xys(m-3) || ' m_y ' || xys(m-2));
        xm := 0.5*(Xys(m-1)+Xys(m-3));
        ym := 0.5*(Xys(m)+Xys(m-2));
--        dbms_output.put_line('AV x ' || xys(m-3) || ' av_y ' || xys(m-2));
        x1 := XYs(m-1);
        y1 := Xys(m);
        dist := Distance_fcn(x1,y1,xm,ym);
        if dist < 0.05 then
          factor := sqrt(0.06*0.06 -dist*dist)/dist;
        else
        factor := 0.125;
        end if;
--         dbms_output.put_line('factor ' || factor);
        xnew := xm+ (y1-ym)*factor;
        ynew := ym + (xm-x1)*factor;
--        dbms_output.put_line('New x ' || xnew || ' new_y ' || ynew);
        xnew2 := xm + (ym-y1)*factor;
        ynew2 := ym + (x1-xm)*factor;
--        dbms_output.put_line('New2 x ' || xnew2 || ' new_y ' || ynew2);
--        dbms_output.put_line('DNew ' || Distance_fcn(xc,yc,xnew,ynew));
--        dbms_output.put_line('DNew2 ' || Distance_fcn(xc,yc,xnew2,ynew2));
        if Distance_fcn(xc,yc,xnew,ynew) < Distance_fcn(xc,yc,xnew2,ynew2)  then
           xnew := xnew2;
           ynew := ynew2;
        end if;
--        dbms_output.put_line('DNew ' || Distance_fcn(xc,yc,xnew,ynew));
        Xys(m-3) := xnew;
        Xys(m-2) := ynew;
      end if;

  end if;

--      dbms_output.put_line('xc ' || xc || ' yc ' || yc);

--      dbms_output.put_line('New x ' || xnew || ' new_y ' || ynew);
--==============================================================================
 -- Incorporate the bend into the geometry and change the edge in the topology

     edge_geom.sdo_ordinates := XYs;
     GZ_TOPO_UTIL.GZ_CHANGE_EDGE_COORDS(Topology,edge_to_move,edge_geom,tolerance);

     sql_stmt3 := 'SELECT geometry,start_node_id,end_node_id from ' || schema||'.' || Topology ||
               '_EDGE$ where edge_id=:1';
     execute IMMEDIATE sql_stmt3 into edge_geom,start_node,end_node using edge_to_move;
     new_len := sdo_geom.sdo_length(edge_geom,0.05,'unit=meter');
--     dbms_output.put_line('old ' || old_len || ' new ' || new_len);
     node_to_move := start_node;
--==============================================================================
-- If the length has not changed, the node has NOT been moved!


  if old_len = new_len then
    status := 1;                       -- it did not move, so don't update any more geometries
  else

-- More SUCCESS checking
-- Now check the face this edge supposedly fixed

    sql_stmt := 'SELECT sdo_geom.validate_geometry_with_context(a.topogeom.get_geometry(),:1) from '||Schema||
                '.' || Intable ||' a where a.face_id=:2';
    EXECUTE IMMEDIATE sql_stmt into result using tolerance,face_id;
    dbms_output.put_line(result);


-- COMPLETE SUCCESS
-- Update all of the geometries that surround node1. Remember the faces done, so we only do them once
    if result = 'TRUE' then
          UPDATE_FACES_AROUND(node_to_move,Intable, InGeom_column,SCHEMA,topology,tolerance);
    end if;
  end if;
--==============================================================================

  RETURN status;

END;
--
PROCEDURE UPDATE_FACES_EACH_SIDE(edge_id NUMBER,Clip_face_table VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05) AS

-- Procedure to update the face geometries on either side of an edge
-- Calls Matthew's GZ_populate_Measurements to do the work

  left_face      NUMBER;
  right_face     NUMBER;
  sql_stmt       VARCHAR2(4000);

BEGIN

    sql_stmt := 'SELECT left_face_id,right_face_id from '|| Topology || '_edge$ where edge_id=:1';
    EXECUTE IMMEDIATE sql_stmt into left_face,right_face using edge_id;

    if left_face <> -1. then
      GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(Clip_face_table,'FACE_ID','ALL','ALL',tolerance,NULL,'FACE_ID',left_face);
    end if;
    if right_face <> -1. then
      GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(Clip_face_table,'FACE_ID','ALL','ALL',tolerance,NULL,'FACE_ID',right_face);
    end if;

END UPDATE_FACES_EACH_SIDE;
--
PROCEDURE UPDATE_FACES_AROUND(node_to_move NUMBER,Intable VARCHAR2, InGeom_column VARCHAR2,SCHEMA VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05) AS

-- Procedure to update the face geometries around a node
-- Calls Matthew's GZ_populate_Measurements to do the work but keeps track of which
-- faces are already done,

  faces_done  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  edges_around SDO_NUMBER_ARRAY;
  left_face      NUMBER;
  right_face     NUMBER;
  id             NUMBER;
  fpos           PLS_INTEGER := 0;
  sql_stmt       VARCHAR2(4000);

BEGIN

    edges_around := sdo_topo_map.get_node_star(Topology,NULL,node_to_move);
    faces_done.extend(edges_around.count);

    sql_stmt := 'SELECT left_face_id,right_face_id from '|| Topology || '_edge$ where edge_id=:1';

    for ii in 1..edges_around.count loop
      id := ABS(edges_around(ii));
      EXECUTE IMMEDIATE sql_stmt into left_face,right_face using id;

-- Only do each face once .. so mark it as minus 1 to skip code below

      for jj in 1..fpos loop
         if left_face = faces_done(jj) then left_face := -1.;   end if;
         if right_face = faces_done(jj) then  right_face := -1.;  end if;
      end loop;

      if left_face <> -1. then

          GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(Intable,'FACE_ID','ALL','ALL',tolerance,NULL,'FACE_ID',left_face);

          fpos := fpos + 1;
          faces_done(fpos) := left_face;
          dbms_output.put_line('done ' || left_face);
      end if;

      if right_face <> -1. then

          GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(Intable,'FACE_ID','ALL','ALL',tolerance,NULL,'FACE_ID',right_face);

          fpos := fpos + 1;
          faces_done(fpos) := right_face;
          dbms_output.put_line('done ' || right_face);
      end if;
    end loop;

END UPDATE_FACES_AROUND;
--
FUNCTION SHORT_ORACLE_DIST(x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER)
                                 RETURN NUMBER DETERMINISTIC AS

-- This function returns the correct Oracle distance for SRID 8265 for
-- differences in latitude and longitude both <= 0.0000057 degrees.
-- RMS error is 2E-9 meters (2 nanometers) and maximum error is 6 nanometers.
--
-- According to Siva Ramada, Oracle uses the authalic sphere for calculations
-- less than 1 meters or thereabouts. The authalic radius used here is larger
-- than the accepted value because it matches Oracle values better!
--https://user.gs.rmit.edu.au/rod/files/publications/Geometric%20Geodesy%20A(2013).pdf
-- but according to this reference it should 6371007.181
  authalic       NUMBER := 6371007.181;
  authalic_perd  NUMBER := 111195.051975;  -- Authalic radius * acos(-1.)/180.
  deg2rad        NUMBER := 0.017453292519943295;  -- For IEEE accuracy for 15-16 digits

  cosin NUMBER;
  yy             NUMBER;
  dist_x         NUMBER;
  sin2lat        NUMBER;
  sin2lon        NUMBER;
BEGIN
  yy := (y1+y2)*0.5*deg2rad;

  dist_x :=   (x2-x1)*cos(yy);

  RETURN sqrt(dist_x*dist_x + (y2-y1)*(y2-y1))*authalic_perd;

END SHORT_ORACLE_DIST;
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
--##############################################################################
--Program Name: line_intersect
--Author: Sidey Timmins
--Creation Date: 4/24/2007

--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 10 required parameters:
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
-- After adding 4 million meters to x and 2 million meters to y the new
-- tolerances shown below enabled x1,y1 (and/or x2,y2) to be changed by 1.E-7

-- Reference: A Programmer's Geometry: Adrian Bowyer and John Woodwark
--  http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
-- Dependencies: none
--##############################################################################
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
--
FUNCTION PULL(err_msg VARCHAR2,find VARCHAR2, instance PLS_INTEGER default 1) return number AS

-- pull a string from an Oracle error mesage like this:

-- 13349 [Element <1>] [Ring <1>][Edge <1>][Edge <2>]
-- 13356 [Element <1>] [Coordinate <3>][Ring <1>]

-- Caller has to know what he/she is looking for:
-- result := pull('err_msg,'[Element ');

   pos     PLS_INTEGER;
   pos2    PLS_INTEGER;
   value  NUMBER;
BEGIN
   pos := INSTR(err_msg,find,1,instance);

   if pos <> 0 then
     pos := INSTR(err_msg,'<',pos);
     pos2 := INSTR(err_msg,'>',pos);
     value := to_number(SUBSTR(err_msg,pos+1,pos2-pos-1));
     return value;
   else
     return 0;
   end if;

END PULL;
--
FUNCTION MATCH_EDGE(poly_geom   MDSYS.SDO_GEOMETRY, edge_id NUMBER, edge_geom   MDSYS.SDO_GEOMETRY,x IN OUT NOCOPY NUMBER, y IN OUT NOCOPY NUMBER,len IN OUT NOCOPY NUMBER,
epsilon number default 0.0000005) return MDSYS.SDO_LIST_TYPE AS

-- Check an edge (should be a segment) to see where it fits in a poly geom
-- Restrictions:
--                1) the edge does match the polygon somewhere on the nodes (endpoints)
--                   or zero will be returned
--                2) the edge has no duplicate vertices!!

-- Return:   matches(2): 1) poly_segment matching edge_segment(1)
--                       2) poly_segment matching edge_segment(2)
--   so if matches(2) < matches(1) then edge goes backwards relative to the polygon

   XYs        MDSYS.SDO_ORDINATE_ARRAY := edge_geom.sdo_ordinates;
   Poly_Xys   MDSYS.SDO_ORDINATE_ARRAY := poly_geom.sdo_ordinates;
   try_Xys    MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   try_Xys2   MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
   try_geom   MDSYS.SDO_GEOMETRY;
   x1         NUMBER;
   y1         NUMBER;
   x2         NUMBER;
   y2         NUMBER;
   delta      NUMBER;
   len1       NUMBER;
   len2       NUMBER;
   matches    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(0.0,0.0);
   v1         NUMBER := -1;
   v2         NUMBER := -1;

   n          PLS_INTEGER := Trunc(Poly_Xys.count/2) -1;
   m          PLS_INTEGER := Trunc(Xys.count/2) -1;
   ii         PLS_INTEGER;
   ik         PLS_INTEGER := 1;
   loops      PLS_INTEGER;

   Function Match_vertex(x0 NUMBER,y0 NUMBER, XYs IN OUT NOCOPY MDSYS.sdo_ordinate_array,tiny NUMBER,v2 NUMBER default 0.0) RETURN NUMBER AS
     ip   PLS_INTEGER := -1;
   Begin
     For jj in 1..TRUNC(XYs.count/2) Loop
      ip := ip + 2;
      if ABS(x0 - Xys(ip)) <= tiny and ABS(y0 - Xys(ip+1)) <= tiny then
          if jj <> v2 then
            RETURN jj;   -- return a segment number
          end if;
      end if;
     End Loop;
     RETURN 0.0;
   End;
   Procedure CopyXys(XYin IN OUT NOCOPY MDSYS.sdo_ordinate_array,a number,b number,XYout IN OUT NOCOPY MDSYS.sdo_ordinate_array) AS
   next PLS_INTEGER := 0;

   Begin
   Xyout.extend(b-a+1);
     For ii in a..b Loop
       next := next + 1;
       Xyout(next) := Xyin(ii);
     End Loop;
   End;
   Function Assemble(XYs IN OUT NOCOPY MDSYS.sdo_ordinate_array,v1 NUMBER,v2 NUMBER,way NUMBER) RETURN MDSYS.sdo_ordinate_array AS
    part_Xys MDSYS.sdo_ordinate_array := MDSYS.sdo_ordinate_array();
   Begin
-- When we assemble it is implied you may want the clockwise or anticlockwise way
 --  dbms_output.put_line('v1 ' || v1 || ' v2 ' || v2);
        if v2 > v1 then CopyXys(Xys,v1*2-1,v2*2,Part_xys); end if;
        if v1 > v2 then CopyXys(Xys,v2*2-1,v1*2,Part_xys); end if;
     if way < 0 then    -- anticlockwise - polygon order
        if v1 > v2 then Reverse_ordinates(Part_xys); end if;
     elsif way > 0 then
        if v2 > v1 then Reverse_Ordinates(Part_xys); end if;
     End if;
     RETURN Part_Xys;
   End;
BEGIN

 len := sdo_geom.sdo_length(edge_geom,0.05,'unit=meter');
 -- Find the 2 Edge end points (the nodes)

  x1 := Xys(1);
  y1 := Xys(2);
  x2 := Xys(Xys.count-1);
  y2 := Xys(Xys.count);

  if edge_id < 0 then
    x2 := Xys(1);
    y2 := Xys(2);
    x1 := Xys(Xys.count-1);
    y1 := Xys(Xys.count);
  end if;
  loops := 0;
  delta := epsilon*0.25;
--  dbms_output.put_line('x1 ' || x1 || ' y1 '|| y1);
--   dbms_output.put_line('x2 ' || x2 || ' y2 '|| y2);
  While (v1 <= 0.0 or v2 <=0.0) and loops < 4 Loop
    loops := loops + 1;
    delta := delta*2.;
-- Ensure v1 <> v2
    if v1 <= 0.0 then v1 := Match_vertex(x1,y1,Poly_Xys,delta,v2); end if;
    if v2 <= 0.0 then v2 := Match_vertex(x2,y2,Poly_Xys,delta,v1); end if;
--    dbms_output.put_line('VV1 ' || v1 || ' v2 '|| v2);
  End Loop;
  If v1 = 0.0 or v2 = 0.0 then
     RETURN matches;
  End if;


  try_Xys := Assemble(Poly_Xys,v1,v2,-1);
--  dbms_output.put_line('XYs count ' || try_xys.count);
  try_geom := MDSYS.SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1, 2, 1), try_Xys);
  len1 := sdo_geom.sdo_length(try_geom,0.05,'unit=meter');
  try_Xys2 := Assemble(Poly_Xys,v1,v2,1);
  try_geom := MDSYS.SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1, 2, 1), try_Xys2);
  len2 := sdo_geom.sdo_length(try_geom,0.05,'unit=meter');

  matches(1) := v1;
  matches(2) := v2;
RETURN matches;

/*
  For kk in 1..m Loop
    x1 := x2;
    y1 := y2;
    ik := ik + 2;
    x2 := Xys(ik);
    y2 := Xys(ik+1);

    ii:= -1;
    For jj in 1..n Loop
      ii := ii + 2;
      if ABS(x1 - Poly_Xys(ii)) < epsilon and ABS(y1 - Poly_Xys(ii+1)) < epsilon then
        if ABS(x2 - poly_Xys(ii+2)) < epsilon and ABS(y2 - Poly_Xys(ii+3)) < epsilon then
          x := x1;
          y := y1;
          RETURN jj +  thousand*kk;
        End if;
      end if;
-- Edges can go backwards..
      if ABS(x2 - Poly_Xys(ii)) < epsilon and ABS(y2 - Poly_Xys(ii+1)) < epsilon then
        if ABS(x1 - Poly_Xys(ii+2)) < epsilon and ABS(y1 - Poly_Xys(ii+3)) < epsilon then
          x := x2;
          y := y2;
          RETURN -jj - thousand*kk;
        End if;
      end if;

   End Loop;
  End Loop;
*/


END MATCH_EDGE;
--
PROCEDURE CHECK_FOR_NEAR_SEGMENTS(pTopology VARCHAR2,pInput_Table VARCHAR2,EDGE_ID_COLUMN VARCHAR2, GEOMETRY_COLUMN VARCHAR2, ptolerance NUMBER default 0.05,edge_id NUMBER default 0.0,plog_type VARCHAR2 default 'LS') AS

  TYPE TblCursorType IS REF CURSOR;
  TYPE GEOM_ARRAY  IS  VARRAY(1048576) OF MDSYS.SDO_GEOMETRY;

  deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.

    Table_cursor         TblCursorType;
    Geometries           GEOM_ARRAY := GEOM_ARRAY();
    Geometry1            MDSYS.SDO_GEOMETRY;
    Geometry2            MDSYS.SDO_GEOMETRY;
    Geom1                MDSYS.SDO_GEOMETRY;
    Geom2                MDSYS.SDO_GEOMETRY;
    Xys                  MDSYS.SDO_ORDINATE_ARRAY;


    Oids                 MDSYS.SDO_LIST_TYPE;
    Nearby_Oids          MDSYS.SDO_LIST_TYPE;


    Input_Table          VARCHAR2(30) := UPPER(pInput_Table);
    Topology             VARCHAR2(30) := UPPER(pTopology);
    sql_stmt             VARCHAR2(4000);
    sql_stmt2            VARCHAR2(4000);
    Row_limit            NUMBER := 100;

    Oid                  NUMBER;
    Oid2                 NUMBER;
    xLL                  NUMBER;
    yLL                  NUMBER;
    xUR                  NUMBER;
    yUR                  NUMBER;


    tolerance            NUMBER := ptolerance * 1.02;

    xdelta               NUMBER;  -- tolerances in degrees
    ydelta               NUMBER;
    xdiff                NUMBER;
    dist                 NUMBER;
    SRID                 NUMBER;
    kount                NUMBER;
    self_kount           NUMBER;


    pos                  PLS_INTEGER;
    msg                  VARCHAR2(4000);
    result              VARCHAR2(1000);
    n                    PLS_INTEGER;
    ii                   PLS_INTEGER;
     greater              VARCHAR2(4):= 'NO';

BEGIN


    sql_stmt := 'SELECT '||EDGE_ID_COLUMN||','||GEOMETRY_COLUMN||' from ' || Input_table;
    if edge_id <> 0 then

      sql_stmt := sql_stmt || ' WHERE '||EDGE_ID_COLUMN|| ' =:1';
      OPEN Table_cursor FOR  sql_stmt using edge_id;
    else
       greater := 'YES';
      OPEN Table_cursor FOR  sql_stmt;
    end if;
    sql_stmt2 := 'SELECT '||GEOMETRY_COLUMN||' from ' || Input_table || ' WHERE ' || EDGE_ID_COLUMN ||'=:1';


 LOOP

   FETCH Table_cursor BULK COLLECT INTO OIds,Geometries LIMIT Row_limit;
--   dbms_output.put_line('count ' || Geometries.count);
     EXIT WHEN Geometries.COUNT = 0;

    For jj in 1..Geometries.count Loop
      Geometry1 := Geometries(jj);
      Oid := Oids(jj);

-- This is a little messy here because we need the extent of the geometry to get
-- the neighbors.
-- Best to have a table of MBRs?

      Xys := Geometry1.sdo_ordinates;



-- We need to set a tolerance in degrees no meters
 -- Convert 0.05 meters to degrees

    SRID := Geometry1.SDO_SRID;
     if SRID = 8265. then
         xdelta := tolerance/(111319.490793274*cos(xys(2)*deg2rad));
     else
         xdelta := tolerance;
     end if;



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

   Set_MBR(Xys,xLL,yLL,xUR,yUR);
   if SRID = 8265. then
         ydelta := .0011;
         xdiff := xUR-xLL;
         if xdiff > 1. then
           ydelta := ydelta * xdiff*xdiff;
         end if;
   else
     ydelta := tolerance;
   end if;




      Nearby_oids := GET_NEARBY_EDGES(Input_table,EDGE_ID_COLUMN,GEOMETRY_COLUMN,Oid,xLL,yLL,xUR,yUR,SRID,greater);

-- We want to check for self intersections when there are no neighbors

-- Loop over all Nearbors
      For kk in 0..Nearby_oids.count LOOP



--        IF oid2 = 1124691 then  -- just for debugging
        IF kk <> 0 THEN
          Oid2 := Nearby_Oids(kk);

--        dbms_output.put_line('oid2 ' || oid2);

          EXECUTE IMMEDIATE sql_stmt2 into Geometry2 using Oid2;


--dbms_output.put_line('ID ' ||oid || ' nearby ' || oid2 || ' XY count ' || Xys.count);

          result := GZ_GEOM_UTILS.validate_lines_with_context(Geometry1,Geometry2,tolerance);

          kount :=0;
          if result <> 'TRUE'then
          pos := INSTR(result,'s');
          if pos > 1 then
            kount := to_number(SUBSTR(result,1,pos-1));
          end if;
          end if;
          if result <> 'TRUE' and kount >  self_kount then
           dbms_output.put_line('ID ' || OID || ' has '|| (kount-self_kount) ||' intersections with ' || oid2);
        end if;

-- Check for self_intersections first
        ELSE
           result := GZ_GEOM_UTILS.validate_lines_with_context(Geometry1,tolerance);
           self_kount :=0;
           if result <> 'TRUE' then
           pos := INSTR(result,'s');

           if pos > 1 then
             dbms_output.put_line(SUBSTR(result,1,pos-1));
            self_kount := to_number(SUBSTR(result,1,pos-1));
           end if;
           end if;
           if result <> 'TRUE' then
             dbms_output.put_line('ID ' || OID || ' has ' || result);
           end if;
        END IF;

    End Loop;

    END LOOP;
  END LOOP;
  dbms_output.put_line('Oracle found ' || kount);

END CHECK_FOR_NEAR_SEGMENTS;
--
PROCEDURE CHECK_FOR_NEARBY_SEGMENTS(pTopology VARCHAR2,pInput_Table VARCHAR2,EDGE_ID_COLUMN VARCHAR2, GEOMETRY_COLUMN VARCHAR2, ptolerance NUMBER default 0.05,edge_id NUMBER default 0.0,plog_type VARCHAR2 default 'LS') AS

  TYPE TblCursorType IS REF CURSOR;
  TYPE GEOM_ARRAY  IS  VARRAY(1048576) OF MDSYS.SDO_GEOMETRY;

  deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.

    Table_cursor         TblCursorType;
    Geometries           GEOM_ARRAY := GEOM_ARRAY();
    Geometry1            MDSYS.SDO_GEOMETRY;
    Geometry2            MDSYS.SDO_GEOMETRY;
    Geom1                MDSYS.SDO_GEOMETRY;
    Geom2                MDSYS.SDO_GEOMETRY;
    Geom3                MDSYS.SDO_GEOMETRY;
    XYs                  MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    XY2s                 MDSYS.SDO_ORDINATE_ARRAY;
    XYOrd                MDSYS.SDO_ORDINATE_ARRAY;
    Info_array           MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1);
    Oids                 MDSYS.SDO_LIST_TYPE;
    Nearby_Oids          MDSYS.SDO_LIST_TYPE;
    result              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    List_within          MDSYS.SDO_LIST_TYPE;
    Start_end            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    Input_Table          VARCHAR2(30) := UPPER(pInput_Table);
    Topology             VARCHAR2(30) := UPPER(pTopology);
    sql_stmt             VARCHAR2(4000);
    sql_stmt2            VARCHAR2(4000);
    Row_limit            NUMBER := 100;

    Oid                  NUMBER;
    Oid2                 NUMBER;
    xLL                  NUMBER;
    yLL                  NUMBER;
    xUR                  NUMBER;
    yUR                  NUMBER;
    SRID                 NUMBER;
    x1                   NUMBER;
    y1                   NUMBER;
    x2                   NUMBER;
    y2                   NUMBER;
    xdiff                NUMBER;
    segments             NUMBER;
    thousand             NUMBER := 10000000.;

    tolerance            NUMBER := ptolerance * 1.02;
    seg1                 NUMBER;
    seg2                 NUMBER;
    xdelta               NUMBER;  -- tolerances in degrees
    ydelta               NUMBER;
    dist                 NUMBER;
    kount                NUMBER := 0.0;
    vertex               NUMBER;
    seg1save             PLS_INTEGER;
    seg2save             PLS_INTEGER;
    range               NUMBER;
    dec_digits           PLS_INTEGER :=9;
    kj                   PLS_INTEGER;
    nn                   PLS_INTEGER;
    nv                   PLS_INTEGER;
    mm                   PLS_INTEGER;
    m                    PLS_INTEGER;
    pos                  PLS_INTEGER;
    msg                  VARCHAR2(4000);
    n                    PLS_INTEGER;
    ii                   PLS_INTEGER;
    last_result          NUMBER :=0.0;
    greater              VARCHAR2(4):= 'NO';

    ok                  BOOLEAN;
    skip                BOOLEAN;
    the_time            timestamp;
BEGIN

--    the_time := current_timestamp;
    Start_end.extend(4);
    sql_stmt := 'SELECT '||EDGE_ID_COLUMN||','||GEOMETRY_COLUMN||' from ' || Input_table;
    if edge_id <> 0 then

      sql_stmt := sql_stmt || ' WHERE '||EDGE_ID_COLUMN|| ' =:1';
      OPEN Table_cursor FOR  sql_stmt using edge_id;
    else
      greater := 'YES';   -- Show all relationships
      OPEN Table_cursor FOR  sql_stmt;
    end if;
    sql_stmt2 := 'SELECT '||GEOMETRY_COLUMN||' from ' || Input_table || ' WHERE ' || EDGE_ID_COLUMN ||'=:1';


 LOOP

   FETCH Table_cursor BULK COLLECT INTO OIds,Geometries LIMIT Row_limit;
--   dbms_output.put_line('count ' || Geometries.count);
     EXIT WHEN Geometries.COUNT = 0;


    For jj in 1..Geometries.count Loop
--     dbms_output.put_line('current ' || (current_timestamp - the_time));
      Geometry1 := Geometries(jj);
      Oid := Oids(jj);
--dbms_output.put_line('OID ' || oid );
-- This is a little messy here because we need the extent of the geometry to get
-- the neighbors.
-- Best to have a table of MBRs?




      Xys := Geometry1.sdo_ordinates;
--      dbms_output.put_line('XYS ' || xys.count);
--        reverse_ordinates(Xys);             --- JUST FOR TESTING
--       geometry1.sdo_ordinates := Xys;
--       dbms_output.put_line('XYS ' || xys.count);
      Info_array := Geometry1.sdo_elem_info;
      nn :=Xys.count;
      n := TRUNC(Xys.count/2) -1;  -- the highest segment number in Geometry1
      nv := n+1;
      Start_end(1) := 1.;
      Start_end(2) := n;    -- last possible segment (vertex is 1 higher).


-- We need to set a tolerance in degrees NOT meters
 -- Convert 0.05 meters to degrees

    SRID := Geometry1.SDO_SRID;
     if SRID = 8265. then
         xdelta := ROUND(tolerance/(111319.490793274*cos(xys(2)*deg2rad)),dec_digits);
     else
         xdelta := tolerance;
     end if;



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

   Set_MBR(Xys,xLL,yLL,xUR,yUR);
   if SRID = 8265. then
         ydelta := .0011;
         xdiff := xUR-xLL;
         if xdiff > 1. then
           ydelta := ydelta * xdiff*xdiff;
         end if;
   else
     ydelta := tolerance;
   end if;




      Nearby_oids := GET_NEARBY_EDGES(Input_table,EDGE_ID_COLUMN,GEOMETRY_COLUMN,Oid,xLL,yLL,xUR,yUR,SRID,greater);

-- We want to check for self intersections when there are no neighbors
      if Nearby_oids.count =0 then
         Nearby_oids.extend(1);
         Nearby_Oids(1) :=0.;
      end if;

-- Loop over all Nearbors
      For kk in 1..Nearby_oids.count LOOP


-- We only check for self intersections on geometry1 on the first loop.
        skip := FALSE;
        if kk <> 1 then
          skip := TRUE;
        end if;
        Oid2 := Nearby_Oids(kk);
--        dbms_output.put_line('OID ' || oid ||'nn ' || nn ||' oid2 ' || oid2);


--      If we have a nearby geometry, get it and concatenate it to Geometry1.

--        IF oid2 = 6639496 then  -- just for debugging
        IF oid2 <> 0 THEN

          EXECUTE IMMEDIATE sql_stmt2 into Geometry2 using Oid2;


          Xy2s := Geometry2.sdo_ordinates;
--          reverse_ordinates(Xy2s);
--          geometry2.sdo_ordinates := Xy2s;

          mm := Xy2s.count;

--        We get a list of ordinates from Oid2 (the nearby Oid) that are within
--        the MBR of the current edge (Oid).
--        We use this below to concatenate just the needed portions of Oid2
--        that may interact with Oid.

          List_within := Get_Within_MBR(XY2s,xLL,yLL,xUR,yUR);

--   for ij in 1..list_within.count loop
--      dbms_output.put_line('ij ' || ij || ' LW ' || list_within(ij));
--    end loop;

--          if list_within.count = 0 then
--             dbms_output.put_line('LW is zero');
--          end if;
-- Make a segment start/end array (the start and end of each geometry) so
-- Check_geom_self_intersect knows whether an edge to edge intersection is to be considered.
-- We just want something like 1,n,n+1,n+m when all vertices of edge 2 are present
-- and variations like this when the last vertex is present but there are gaps
-- created by Get_within_MBR ignoring parts of edge 2. For example, consider 2 edges
-- each with 100 vertices. If all of edge 1 is present and edge 2 has just verticies
-- 49 to 100 (coordinates 97 to 200) then we build this array:
--   1,50,-52,151. Here the negative sign means we don't have segment 1 of edge2.


         if List_within.count > 0 then

             Start_end(3) := n+2;
           if List_within(1) <> 1 then
             Start_end(3) := -Start_end(3);
           end if;


            Start_end(4) := n;
  --          dbms_output.put_line('SEND : ' || start_end(4));
            kj := 1;
            While kj < List_within.count Loop

              Start_end(4):= Start_end(4) + TRUNC((List_within(kj+1) - List_within(kj)+1)/2);
  --            dbms_output.put_line('SEND : ' || start_end(2) || ' ' ||start_end(3)||' '||start_end(4));
              kj := kj+2;
            End loop;

            if List_within(list_within.count) <> mm then
             Start_end(4) := -Start_end(4);
           end if;
         end if;
--      dbms_output.put_line('send : ' || start_end(2) || ' ' ||start_end(3)||' '||start_end(4));

    -- Make a concatenated XY array as it is easier for checking for intersections
    -- to handle just a single set of Xys and a single Info array. We can determine
    -- which geometry an intersection comes from because at present we have
    -- the entire set of oordinates for Geometry1 (Oid).

          Xys := Concatenate_Xys_Info(Geometry1,Geometry2,Info_Array,List_within);

        END IF;
--dbms_output.put_line('ID ' ||oid || ' nearby ' || oid2 || ' XY count ' || Xys.count  || ' xdelta ' || xdelta || ' ydelta ' || ydelta);

        result := CHECK_GEOM_SELF_INTERSECT(XYs,Info_Array,FALSE,Start_end,tolerance,xdelta,ydelta,dec_digits,10000000.,oid2,skip);

  -- Now when we interpret the result we just need to realize that segments
  -- beyond the count of the first geometry are in the second geometry.

        if result.count <> 0 then
--dbms_output.put_line('id ' ||oid || ' nearby ' || oid2 || ' XY count ' || Xys.count);

--  dbms_output.put_line('found ' || result.count);
        for ij in 1..TRUNC(result.count/5) loop

          ii := ij*5-4;
--          dbms_output.put_line('result ' || result(ii) || ' ' || result.count);

          ok := TRUE;

          for ik in 1..ij-1 loop
--          dbms_output.put_line('ik ' || ik ||' Result ' || result(ik*5-4));
             if TRUNC(result(ii)) = TRUNC(result(ik*5-4)) THEN
                ok := FALSE;
             end if;
          end loop;
-- Format is segment2 * million + segment1 + the vertex involved *0.1
-- Vertex 1 and 2 refer to segment 1, Vertex 3 and 4 refer to segment 2.
          if ok THEN

-- Here we don't use vertex which could be 23 or 24 or 13 or 14 when for
-- example 23 means 2 and 3 too close :   1+------+2   3+-------+4
-- The vertex is actually in result(ii+3).

          vertex := (result(ii) -TRUNC(result(ii)))*100.;

          segments := result(ii)- vertex*0.01;
-- vertex=3 or 4,  2nd segment is too close to segment 1

-- vertex=1 or 2: 1st segment is to close to segment 2

          seg2 := TRUNC(segments/thousand);
          seg1 := segments-thousand*seg2;


          dist := ROUND(result(ii+4),4);
--          vertex := result(ii+3);
-- If we composites 2 geometries above,
-- we have to work out the true segment number in Xys2
--dbms_output.put_line('seg1 ' || seg1 || ' seg2 ' || seg2 || ' count ' || xys.count);
      seg2save := seg2;
      seg1save := seg1;
          if seg1 > n then
            seg1 := seg1 -1;
          end if;
          x1 := Xys(seg1*2-1);
          y1 := Xys(seg1*2);
          x2 := Xys(seg1*2+1);
          y2 := Xys(seg1*2+2);
--          dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
--          dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2);
          geom1 := MDSYS.SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1, 2, 1),
                   SDO_ORDINATE_ARRAY(x1,y1,x2,y2));

          if seg2*2= Xys.count then
             seg2 := seg2-1;
          end if;
          x1 := Xys(seg2*2-1);
          y1 := Xys(seg2*2);
          x2 := Xys(seg2*2+1);
          y2 := Xys(seg2*2+2);

--                 dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
--          dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2);
          geom2 := MDSYS.SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1, 2, 1),
                   SDO_ORDINATE_ARRAY(x1,y1,x2,y2));

      pos :=0;
      seg2 := seg2save;
      if oid2 <> 0 and List_within.count >= 2 and seg2 > nv then
      pos := TRUNC(List_within(1)/2);
--      dbms_output.put_line('seg2 ' || seg2 || ' nn ' || nn || ' count ' || xys.count);

           seg2 := seg2 - nv;

           m := TRUNC(List_within.count/2);
           for i in 1..m Loop
             range := TRUNC((List_within(i*2) -List_within(i*2-1)+1)/2);
--             dbms_output.put_line('range ' || range || ' seg2 ' || seg2);
             if seg2 <= range then
                seg2 := seg2 + pos;
                exit;
             elsif i <> m then
               seg2 := seg2 - range;
               pos := TRUNC(List_within(i*2+1)/2);
             end if;
           end loop;
      end if;

-- Ignore for the present  reports of edges intersecting butt on.
      IF vertex <= 4 THEN

--          dbms_output.put_line('Checking ' || seg1 || ' with ' || seg2 || ' seg2save ' || seg2save || ' mm ' || mm || ' n ' || n);

          geom3 := sdo_geom.sdo_intersection(geom1,geom2,tolerance);

-- Did Oracle find an intersection between the 2 segments at the tolerance specified?
          if geom3 is NOT NULL then
           kount := kount+1;

-- A very sharp needle may end at the node so we have segment n-2 appearing to
-- intersect another edge at segment 1 or segment m-1 for that edge. But what that
-- really means is the last vertex projects on segment n-2 and the 1st edge has
-- a self intersection.

-- Check if it intersects itself or the second geom
--           if seg1 = TRUNC(nn/2)-2 and (seg2save =1 or seg2save= TRUNC(mm/2)-1) then
--              msg := ':Intersection '||dist||': Check_Nearby: '||'oooid ' || Oids(jj) || ' Seg ' || seg1 ||' intersects itself with vertex ' || TRUNC(nn/2);
--            TRACK_APP(msg,Input_Table,plog_type);
--           elsif seg1 = 1 and (seg2save < TRUNC(nn/2) and  seg2save= TRUNC(mm/2)-1) then
--             msg := ':Intersection '||dist||': Check_Nearby: '||'OOid ' || Oids(jj) || ' Seg ' || seg1 ||' intersects itself with vertex 1';
--            TRACK_APP(msg,Input_Table,plog_type);
           if seg2save > Start_end(2) then
             if vertex <= 2 then
               msg :=':Intersection '||dist||': Check_Nearby: '||'Oid ' || Oids(jj) || ' Vertex ' || seg1save ||' intersects seg ' || seg2 || ' from Oid ' || Oid2;
             else
               msg :=':Intersection '||dist||': Check_Nearby: '||'Oid ' || Oids(jj) || ' Segment ' || seg1save ||' intersects vertex ' || seg2 || ' from Oid ' || Oid2;
             end if;
             TRACK_APP(msg,Input_Table,plog_type);
           else
           if vertex <= 2 then
            msg := ':Intersection '||dist||': Check_Nearby: '||'Oid ' || Oids(jj) || ' Vertex ' || seg1save ||' intersects itself at seg ' || seg2;
           else
            msg := ':Intersection '||dist||': Check_Nearby: '||'Oid ' || Oids(jj) || ' Segment ' || seg1 ||' intersects itself at vertex ' || seg2;
           end if;
            TRACK_APP(msg,Input_Table,plog_type);
           end if;
          end if;
          END IF;
          End if;
          last_result := TRUNC(result(ii));
        end loop;

--      end if;
    END IF;

    End Loop;

    END LOOP;
  END LOOP;
  dbms_output.put_line('Oracle found ' || kount);

END CHECK_FOR_NEARBY_SEGMENTS;
--
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
  dist NUMBER;

  t    NUMBER := 0.5; -- take 50% of 1st or last vertex
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
         y3 := Xys2(m-1);
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
         dist := short_oracle_dist(x,y,x1,y1);
--         dbms_output.put_line('distance apart now is ' || dist);
-- we don't exit because there may be 2 problems !!
      end if;

  end loop;



  geom2.sdo_ordinates :=Xys2;

  Geometry_sans_touch := sdo_util.append(pline1,geom2);

  RETURN Geometry_sans_touch;

END;
--
FUNCTION CONCATENATE_XYS_INFO(Geometry1 IN MDSYS.SDO_GEOMETRY,Geometry2 IN MDSYS.SDO_GEOMETRY,
INFO_ARRAY IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,List_within MDSYS.SDO_LIST_TYPE default NULL) RETURN MDSYS.SDO_ORDINATE_ARRAY AS

/*
--##############################################################################
--Program Name: Concatenate_Xys_Info
--Author: Sidey Timmins
--Creation Date: 10/xx/2011
--
--Usage:
  -- Call this function from inside another PL/SQL program.  This program
  -- has 3 required parameter:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      geometry1      - A geometry for which the entire Xy list is used
  --      geometry2      - A 2nd geometry, of which part(s) are to be used
  --
  --      Info_array    - Info array to be updated
  --      List_within   - optional list to direct which part(s) of geometry2 to use.
  --                    - start, stop coordinate pairs
--
-- Purpose: This function concatenates two geometries using a list for the
--          second geometry. Why? Some software handles an XY array using an
--          Info array to separate the various rings. If one has 2 geometries
--          it is easier to concatenate them and just keep track of where
--          you are accessing to determine which of the 2 geometries you are
--          processing.

-- Method: Processes XY data optionally using List_within.
--
--
--##############################################################################
*/


   XYOrd1          MDSYS.SDO_ORDINATE_Array := Geometry1.SDO_ORDINATES;
   XYOrd2          MDSYS.SDO_ORDINATE_Array := Geometry2.SDO_ORDINATES;
   Xys             MDSYS.SDO_ORDINATE_Array := MDSYS.SDO_ORDINATE_Array();
   Info1_Array     MDSYS.SDO_ELEM_INFO_ARRAY := Geometry1.SDO_ELEM_INFO;
   Info2_Array     MDSYS.SDO_ELEM_INFO_ARRAY := Geometry2.SDO_ELEM_INFO;

  n                PLS_INTEGER;
  m                PLS_INTEGER;
  p                PLS_INTEGER;
  q                PLS_INTEGER;
  r                PLS_INTEGER;
  low              PLS_INTEGER;
  hi               PLS_INTEGER;
  next            PLS_INTEGER;

BEGIN
   n := XYord1.count;
   m := XYord2.count;

   Info_array.trim(Info_array.count);

   p := Info1_array.count;
   Info_array := Info1_array;

   q := Info2_array.count;
   Xys := XYOrd1;

-- Here we concantenate all the coordinates with a single new Info triplet
   if List_within is NULL then

     Info_array.extend(q);
     for ii in 1..q loop
        if MOD(ii,3) = 1 then
          Info_array(ii+p) := Info2_array(ii) + n;
        else
          Info_array(ii+p) := Info2_array(ii);
        end if;
     end loop;

     XYs.extend(m);
     for ii in 1..m Loop
       Xys(ii+n) := XYord2(ii);
     end loop;

   else

-- Use the ranges in List_within to build an Info array for the
-- Xys array we build below.

     r := TRUNC(List_within.count/2);
     m := 0;
     for j in 1..r loop
         m := m+ (List_within(j*2) - List_within(j*2-1) +1);
     end loop;
     XYs.extend(m);
     Info_array.extend(3*r);
     next := n;

-- Build composite Info array and composite XY array

     for j in 1..r loop

       Info_array(p+j*3-2) := next+1;
       Info_array(p+j*3-1) := 2;
       Info_array(p+j*3) := 1;

-- Store chosen ranges of Xys

       for ii in List_within(j*2-1)..List_within(j*2) Loop
         next := next + 1;
         Xys(next) := XYord2(ii);
       end loop;
     end loop;
   end if;
   RETURN Xys;

END CONCATENATE_XYS_INFO;
--
FUNCTION CHECK_GEOM_SELF_INTERSECT(XYord IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
allow_touches BOOLEAN,Start_end IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,tolerance NUMBER,xdelta NUMBER,ydelta NUMBER,dec_digits NUMBER default 18,thousand NUMBER default 10000000.,oid number default 0,skip BOOLEAN default FALSE) RETURN MDSYS.SDO_LIST_TYPE AS
/*
--##############################################################################
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
  --      XYord      - An xy coordinate array to check
  --      Info_array - The corresponding Info array
  --      allow_touches - whether to allow touches 1) for loops and 2)
  --                      for 2 edges end on.
  --      Start_end     -- A range of segments for both edges. If the entire set of
  --                       of vertices (n for the first and m for the 2nd) are
  --                       present in XYord, this is: [1,n-1,n+1,n+m]
  --      tolerance     --
  --      xdelta        --
  --      ydelta        --
  --      loop1_count
  --      dec_digits    - decimal digits to round to. Some of the geometries have
  --                       trailing zeroes. Oracle cannot multiply these numbers
  --                       and it just disconnects!

  --      skip          - Tells this function to skip checking for self
  --                      intersections in the range 1 to Info_array(4)-1.
  --                      Just for effciency, caller should set it TRUE on
  --                      2nd and later calls for id1 with id3, id1 with id4, etc
  --                      (but not for id1 with id2). After the 1st call we know
  --                      all of the self intersections.
--
-- Purpose: Self intersection means segment n intersects segment n+m where m
--          is greater than 1. Returns an array of intersecting segments or an empty array.

-- Method: Checks each pair of segments using Cartesian geometry. But does not
--         make unnecessary checks when the MBR of a group of segments does
--         overlap the current segment being considered.
-- Calls: Set_many_MBR, line_intersect
--
--##############################################################################
*/
    Geom           MDSYS.SDO_GEOMETRY;

    segments       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    MBR            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

    ring_st_end    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
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
    dist2t         NUMBER;
    dist23         NUMBER;
    dist3t         NUMBER;
    dist4t         NUMBER;
    dist24         NUMBER;
    dist1s         NUMBER;

    s              NUMBER;

    x33            NUMBER;
    y33            NUMBER;
    x44            NUMBER;
    y44            NUMBER;
    dx31           NUMBER;
    dy31           NUMBER;
    dx21           NUMBER;
    dy21           NUMBER;
    dx42           NUMBER;
    dy32           NUMBER;
    dx32           NUMBER;
    dy42           NUMBER;
    dist2s         NUMBER;
    distance       NUMBER;
    dxt            NUMBER;
    dyt            NUMBER;

    denom          NUMBER;
    angle          NUMBER;
    d              NUMBER;
    u              NUMBER;
    lensq          NUMBER;
    lencheck       NUMBER;
    vertex         NUMBER;
    metre_indegrees NUMBER := .00029; -- 1 meter longitude (worst case at 72 deg North)
    meter_indeg_sq  NUMBER := 8.4E-08; -- squared
    xtol           NUMBER := xdelta*2.; -- This seems a little excessive
    ytol           NUMBER := ydelta*2.;
    overlap        BOOLEAN;
    a_set          PLS_INTEGER;
    b_set          PLS_INTEGER;
    aseg           PLS_INTEGER;
    bseg           PLS_INTEGER :=1;
    astore         PLS_INTEGER;
    bstore         PLS_INTEGER;

    bstart         PLS_INTEGER;
    aend           PLS_INTEGER;
    bend           PLS_INTEGER;
    pdim           PLS_INTEGER := 8;
    pdim1          PLS_INTEGER := 7;
    lastm          PLS_INTEGER := 1;
    sql_stmt       VARCHAR2(4000);
    t              NUMBER := 0.99999;
    one_t          NUMBER := 1.-t;
    closed         NUMBER :=0;
    n              PLS_INTEGER;
    nxy            PLS_INTEGER; -- # of xy coordinates
    m              PLS_INTEGER :=0;
    p              PLS_INTEGER :=0;
    q              PLS_INTEGER;
    aptr           PLS_INTEGER;
    bptr           PLS_INTEGER;
    lo             PLS_INTEGER;
    hi             PLS_INTEGER;
    begin_at       PLS_INTEGER :=0;

    jj             PLS_INTEGER;
    kk             PLS_INTEGER;
    astart         PLS_INTEGER;
    aend_seg       PLS_INTEGER;
    bend_seg       PLS_INTEGER;
    a_start_set    PLS_INTEGER := 0;
    a_end_set      PLS_INTEGER;
    b_start_set    PLS_INTEGER := 0;
    kountera       PLS_INTEGER := 0;
    kounterb       PLS_INTEGER := 0;
    kounterb2      PLS_INTEGER := 0;
    flag           PLS_INTEGER;
    rings          PLS_INTEGER := 1;
    second_set     PLS_INTEGER;
    next           PLS_INTEGER := 0;
    pos            PLS_INTEGER;
    last_pos       PLS_INTEGER;
    end_of_ring    BOOLEAN := FALSE;
    ok             BOOLEAN := TRUE;

    FUNCTION     maxof(in1 NUMBER,in2 NUMBER) return number as
    Begin
       if in2 > in1 then
         return in2;
       end if;
       return in1;
    end;
BEGIN


  n := TRUNC(XYOrd.count/2);

  if n <= 2 then
    RETURN segments;
  end if;

-- Checking for self intersection is costly and not necessary everywhere

  one_t := 1.-t;

-- We want to set up checking segment i with segment i+2
-- Note that segment 2 touches 1 and can never intersect 1.
--                            3
--                     +-------------+                            +
--                    /                                         / | 2
--          1        /  2                                  1   /  |
--    +------------+                                +-------------+
--                                                           / 3
--                                                          +

-- However, Oracle may imply that segment 2's end vertex is within 0.05
-- of segment 1 (distance d <= 0.05).

--              +
--              | 3
--              |
--              +
--              ^\
--              d \  2
--              v  \
--    +------------+
--            1

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

  nxy := XYOrd.count;
  q := nxy;
  if Info_array.count > 3 then
    nxy := Info_Array(4)-1;
    p := nxy;
  end if;

--  dbms_output.put_line('n ' || n || ' xys ' || xyord.count || ' p ' || p || ' nxy ' || nxy || ' q ' ||q);


   if XYOrd(1) = XYord(nxy-1) and XYOrd(2) = XyOrd(nxy) then
     closed :=1.;
  end if;
  if XYOrd(p+1) = XYord(q-1) and XYOrd(p+2) = XyOrd(q) then
    closed := closed + 2.;
   end if;
   if closed <> 0. then
    rings := Info_Array.count/3;
    ring_st_end.extend(rings+1);
    ring_st_end(1) := 1.0;
-- Setup a list of segment ring starts and ends for each ring
    for kk in 1..rings loop
      if kk <> rings then
        ring_st_end(kk+1) := TRUNC((Info_Array(kk*3+1)-1)/2)+1;
      else
        ring_st_end(kk+1) := TRUNC(XYOrd.count/2)+1; -- this is right!!
      end if;
    end loop;
  end if;

--  for ii in 1..info_array.count loop
--     dbms_output.put_line('ii ' || ii || ' info ' || info_array(ii));
--  end loop;
  if skip then
    begin_at := ABS(Start_end(3))*2-1;
  end if;
  GZ_TOPOFIX.SET_MANY_MBR(XYord,Info_Array,MBR,xLL,yLL,xUR,yUR,begin_at,xdelta);

--for ii in 1..MBR.count loop
--   dbms_output.put_line('ii ' || ii || '   ' ||mbr(ii));
--end loop;

-- Setup: m # of MBRS sets, aseg the segment # for the "a" set,
  m := MBR(5);
  aseg := 1;
  aend_seg := n-2;  -- last segment for the "a" set
  bend_seg := n-1;  -- this is the last segment for the "b" set
  if aend_seg <= aseg then aend_seg := aseg+1; end if;
  if bend_seg <= aseg then bend_seg := aseg+2; end if;
--                     so if there are 20 coordinates, 10 vertices, 9 segments
--  dbms_output.put_line('m is ' || m || ' N ' ||n || ' asend seg ' || aend_seg || ' bend ' || bend_seg);


-- Decide when to skip

    for ii in 2..MBR(7)+1 loop
--       dbms_output.put_line('II ' || ii || ' '|| MBR((ii-1)*pdim+5));
          If ABS(MBR((ii-1)*pdim+5)) >= ABS(Start_end(3))*2-1 then  -- Start_end(3) is start of 2nd edge
                b_start_set := ii-1;
--               dbms_output.put_line('set bstart ' || b_start_set || ' ST_END ' || start_end(3));
              exit;
          end if;
    end loop;
 a_end_set := b_start_set;
 if skip then
-- Now find if the a_set overlap the b_set at all.
     for ii in MBR(6)..MBR.count loop
          If MBR(ii) >= b_start_set then  -- now find an a_set that
             for kk in 2..b_start_set loop
                 lo := MBR((kk-1)*pdim+7);
                 hi := MBR((kk-1)*pdim+8);

                 If lo >= ii or hi >= ii  then
                 if a_start_set = 0 then
                    a_start_set := kk-1;
                    end if;
                    if MBR(lo) >= b_start_set or MBR(hi) >= b_start_set then
                    aend_seg := TRUNC((MBR((kk-1)*pdim+6) -1)/2);
                    if aend_seg <= aseg then aend_seg := aseg+1; end if;
--                    dbms_output.put_line('set astart ' || a_start_set || ' aend_seg  ' || aend_seg || ' ii ' || ii);
                    end if;
                 end if;
             end loop;
              exit;
          end if;
    end loop;

    if a_start_set =0 or b_start_set =0 then
       RETURN segments;
    end if;

  else
    b_start_set :=0;
  end if;


/*
   for ii in 1..m loop   -- Just to visualize the MBRs
       kk := ii*pdim;
       xLL := MBR(kk+1);
       yLL := MBR(kk+2);
       xUR := MBR(kk+3);
       yUR := MBR(kk+4);
--dbms_output.put_line('LB ' || MBR(kk+5) || ' UB ' || MBR(kk+6));
       geom :=  MDSYS.SDO_GEOMETRY(2003,8265,NULL,SDO_ELEM_INFO_ARRAY(1,1003,3),
                    MDSYS.sdo_ordinate_Array(xLL,yLL,xUR,yUR));
                   sql_stmt := 'INSERT into points values(:1,:2,:3)';
                     execute immediate sql_stmt using ii,geom,'MBR geom';
   end loop;
   commit;
   */
--  dbms_output.put_line('m is ' || m || ' MBR ' || mbr.count);

-- We have divided the edge into sections using Set_many_MBR.   MBRS are
-- rectangles than may overlap or may just touch along a line or may just
-- touch at a point as shown
--
--                      |----------------+ (x5,y5)
--                      |               /|
--                      |       3      / |
--    ----------------- +-------------+--|
--    |                /| (x3,y3)      (x4,y4)
--    |     1         /2|
--    +--------------+--|
--   (x1,y1)        (x2,y2)

-- In order to check if any 2 segments intersect it is only necessary to
-- check if segment ii intersects segment ii+2.
-- But this is not necessary if the MBR for segments (ii+2) to say (ii+p) don't
-- overlap (x1,y1) to (x2,y2)


-- Set some triggers to force action at front of loop.
  aend := 0;     -- end segment for "A" set
  a_set := 0;    -- "A" set
--dbms_output.put_line('SEG is ' ||aseg  || ' aendseg ' || aend_seg || ' aend ' || aend || ' B ' || b_start_set);

  While aseg < aend_seg Loop

    aseg := aseg + 1;
    kountera := kountera + 1;
--    dbms_output.put_line('SEG is ' ||aseg  || ' aendseg ' || aend_seg || ' aend ' || aend || ' B ' || b_start_set);

-- Forcing: increment the set number
    if aseg > aend then
       a_set := a_set + 1;          -- becomes 1 at beginning
       if a_set=1 and skip then    -- When skip is set,
          a_set := a_start_set;     -- start at 1st set that overlaps a b_set
       end if;
--       dbms_output.put_line('ASET is ' ||a_set|| ' aseg ' || aseg);
       exit when a_set > m;        -- m is the number of sets of MBRs
       aptr := a_set*pdim;          -- ptr into the MBR array

-- Set start and end segments for this set

       astart := TRUNC((ABS(MBR(aptr+5))+1)/2);  -- start seg #: Turn coord # into segment # or vertex numbers
       aseg := astart;
--       dbms_output.put_line('SEG is NOW ' ||aseg  );
       aend := TRUNC(ABS(MBR(aptr+6))/2) -1;  -- aend:  last segment # for this set
                                         -- and set forcing to advance to next "A" set
--       end_of_ring:= TRUNC(MBR(aptr+6)) <> MBR(aptr+6);
      -- For a normal edge not closed.
--                             3
--           (x3,y3)    +-------------+ (x4,y4)
--                    /
--          1        /  2
--    +------------+
--   (x1,y1)       (x2,y2)
--

       x2 := ROUND(XYord(aseg*2-1),dec_digits);   -- these are all off by 1 as we are going to do
       y2 := ROUND(XyOrd(aseg*2),dec_digits);     --  1 = 2,  2 = 3, 3 = 4


      -- For a closed edge we have 4 segments but we cannot intersect 4 with anything
--                    3
--    (x4,y4)  +-------------+ (x3,y3)
--        4   /             /
--           /             /  2
--           +------------+
--       (x1,y1)       (x2,y2)
--               1



       axLL := MBR(aptr+1);
       ayLL := MBR(aptr+2);
       axUR := MBR(aptr+3);
       ayUR := MBR(aptr+4);
       last_pos := MBR(aptr+8);
    end if;

-- Set up an MBR for this segment

    x1 := ROUND(XYord(aseg*2-1),dec_digits);
    y1 := ROUND(XYord(aseg*2  ),dec_digits);
    x2 := ROUND(XYord(aseg*2+1),dec_digits);
    y2 := ROUND(XYord(aseg*2+2),dec_digits);
    dx21 := (x2-x1);
    dy21 := (y2-y1);
    lensq := (dx21*dx21 + dy21*dy21);

    if x1 > x2 then
      xLL := x2-xtol;
      xUR := x1+xtol;
    else
      xLL := x1-xtol;
      xUR := x2+xtol;
    end if;
    if y1 > y2 then
      yLL := y2-ytol;
      yUR := y1+ytol;
    else
      yLL := y1-ytol;
      yUR := y2+ytol;
    end if;

-- Now set up the 'B" set with Forcing

    bend := 0;               -- force b_set to be incremented at loop start

    if skip then            -- except when skip is set and we can avoid
      b_set := b_start_set;  -- checking all of the a_set again.

    else
       b_set := a_set-1;

-- Set_Many_MBR figured out if the MBR has the potential to overlap itself

       bptr :=(b_set+1)*pdim;
       if MBR(bptr+5) < 0 then   -- it does not overlap itself
         b_set := b_set +1;
--         dbms_output.put_line('BSETTT ' || b_set || ' bptr ' || bptr || ' mbr ' || mbr(bptr+5));
       end if;
    end if;
    pos := MBR(aptr + 7) -1; -- position in the list of overlapping MBRs
    bseg := aseg+1;  -- "B" segment number: we want to check 1 with 3

--dbms_output.put_line('now aseg ' || aseg);
--dbms_output.put_line('ENTERING bseg ' || bseg || ' bend ' || bend_seg || ' bset ' || b_set || ' MM ' || m || ' aseg ' || aseg || ' pos ' || pos || ' last ' || last_pos);
    While b_set <= m and bseg <= bend_seg Loop

      kounterb := kounterb + 1;
      bseg := bseg + 1;
      flag :=0;
--      if aseg >= 60 and bseg <= 61 then
--      dbms_output.put_line('>>bseg ' || bseg || ' aseg ' || aseg || ' bend ' || bend_seg || ' aendseg ' ||aend_seg || ' bset ' || b_set);
--   end if;
-- Handle the forcing to the next "B" set

      if bseg > bend then
        b_set := b_set + 1;
--        if a_set=23 or b_set=23 then
--       dbms_output.put_line('>>>>>>now  BSET ' || b_set || ' aset ' || a_set || ' aend ' || aend);
--        end if;
--      Short circuit a lot of loops here by check

-- Now get the other MBRs that overlap the "A" set
        if b_set <> a_set then

           pos := pos + 1;
           if pos > last_pos or last_pos <= 0 then
             b_set := m+1;
--              dbms_output.put_line('>>>>>>SET  BSET ' || b_set || ' pos ' || pos || ' last ' || last_pos);
           else
              b_set := maxof(MBR(pos),b_start_set);
--               dbms_output.put_line('>>>>>>set  BSET ' || b_set);
           end if;

           if b_set > m then
             bend :=0;
--             dbms_output.put_line('exiting'|| b_set);
             exit;
           end if;
        end if;


        bptr := b_set*pdim;
        bstart := TRUNC((ABS(MBR(bptr+5))+1)/2);  -- Turn coord # into segment # or vertex numbers
        bend := TRUNC(ABS(MBR(bptr+6))/2)-1;      -- Last segment for this set
        bseg := bstart;

        if bseg < aseg+1 then
            bseg := aseg+1;
        end if;
--      At the beginning of an edge we do want to check segment 1 with 2
        if aseg = 1 and a_set = b_set and bseg = 3 then
            bseg :=2;
            flag :=11;
        end if;
 --        dbms_output.put_line('ST ' || start_end(2) || ' bend ' || bend || ' aseg ' || aseg );
 --      And similarly at the end
 --       if flag =0 and aseg = start_end(2)-1 and (a_set = b_set or (b_set = a_set+1 and bstart=bend)) then
 --           bseg := start_end(2);
 --           flag := 11;
 --       end if;

 --         dbms_output.put_line('bstart ' || bstart || ' bseg ' || bseg || ' aseg ' || aseg || ' aend_sseg ' || aend_seg ||' bend ' || bend || 'bset '||b_set || ' a_set ' ||a_set);

--         dbms_output.put_line('Bset ' || b_set ||' bptr ' || bptr || ' aset ' || a_set);
--        dbms_output.put_line('Bset ' || b_set ||' BBstart '|| bstart ||' bend ' || bend || ' bseg ' || bseg);
--        overlap := TRUE;

-- On the first "B" pass we want to check if the "A" segment overlaps the "B" MBR at all -
-- if not then we can exit
                                     -- check the MBRs
          bxLL := MBR(bptr+1);
          byLL := MBR(bptr+2);
          bxUR := MBR(bptr+3);
          byUR := MBR(bptr+4);

-- The equals are removed from these tests because one may have a segment that
-- exactly matches the MBR
--        if b_set <> a_set then             -- The 2 sets are different
--          if  (axUR  < bxLL) or (axLL  > bxUR) or
--              (ayUR  < byLL) or (ayLL  > byUR) then
--            overlap := FALSE;
--            bseg := bend;                  -- they don't overlap, so force to next set
--          end if;
--        end if;
      End If;


-- If the 2 sets DONT overlap  or the MBR of the "A" segment is outside "B" MBR
-- then nothing to do...
-- Also if we are at the end of a range then bseg > bend and we dont want to draw
-- a segment between 2 ranges.

--      dbms_output.put_line('Aseg ' || aseg || ' bseg ' || bseg || ' bend ' || bend || ' b_set ' || b_set || ' a_set  ' || a_set);

      IF flag =0 and (bseg > bend or  (xUR  < bxLL) or (xLL  > bxUR) or
              (yUR  < byLL) or (yLL  > byUR)) THEN

           NULL;
      ELSE
-- If the 2 sets overlap then we compare each pair of segments in the set

-- Loop over vertices, starting at vertex 4 (x4,y4) and proceeding to the last vertex n.
-- When we consider vertex 4 we are checking segment 3:  (x3,y3) to (x4,y4)
-- However this loop is over just the vertices described by the current MBR

--dbms_output.put_line('bseg ' || bseg || ' bend ' || bend || ' xyord ' || xyord.count);
          jj := bseg*2;
          x3 := ROUND(XYord(jj-1),dec_digits);
          y3 := ROUND(XYord(jj),dec_digits);
          jj := (bseg+1)*2;
          x4 := ROUND(XYord(jj-1),dec_digits);
          y4 := ROUND(XYord(jj),dec_digits);
--if aseg=11 and bseg=12 then
--dbms_output.put_line('aseg ' || aseg || ' x1 ' ||x1 || ' y1 ' || y1 || ' x2 ' || x2 || ' y2 ' || y2);
--dbms_output.put_line('bseg ' || bseg || ' x3 ' ||x3 || ' y3 ' || y3 || ' x4 ' || x4 || ' y4 ' || y4);
--end if;
-- When we don't have doubling back we have this sort of picture and later
-- segments like 3-4  walk away from 1-2.

--                            3        4
--              2 +           +-*------+
--               /              test
--              /               point
--           1 +

-- Make a test point (near 3) to see if the line is doubling back and check if
--                                             dist1t < dist13
--             2  +                         or dist1t < dist12
--               /     + 3
--              /       *   test point (t)
--           1 +         \
--                        + 4

              xt := x3*t + one_t*x4;
              yt := y3*t + one_t*y4;
              dx31 := x3 - x1;
              dy31 := y3 - y1;
              dist13 := dx31*dx31 + dy31*dy31;
              dxt := xt-x1;
              dyt := yt-y1;
              dist1t := dxt*dxt + dyt*dyt;

-- NEW CODE

              if dist1t > dist13 then
                dx42 := x4 - x2;
                dy42 := y4 - y2;
                dist24 := dx42*dx42 + dy42*dy42;
                xt := x4*t + one_t*x3;   -- point near x4 (coming back towards 1-2)
                yt := y4*t + one_t*y3;
                dxt := xt-x2;
                dyt := yt-y2;
                dist2s := dxt*dxt + dyt*dyt;
              else
                dist24 := 1.;
                dist2s := 0.;
              end if;
-- OLD CODE most commented
--              dxt := xt-x2;
--              dyt := yt-y2;
--              dist2t := dxt*dxt + dyt*dyt;
              dx32 := x3 - x2;
              dy32 := y3 - y2;
              dist23 := dx32*dx32 + dy32*dy32;


-- Make a 2nd test point (near 1) and see if
--                                             dist3t < dist13
--              4 +                         or dist3t < dist34
--               /     + 2
--              /       \
--           3 +         *  test point (t)
--                        + 1

--              xt := x1*t + one_t*x2;
--              yt := y1*t + one_t*y2;

--              dist3t := (xt-x3)*(xt-x3) + (yt-y3)*(yt-y3);

-- Make a 2nd test point (near 2) and see if
--                                             dist1s < dist12
--              3 +   test point s          or dist23 < tolerance squared
--               /    * + 2
--              /        \
--           4 +          \
--                         + 1

--              xt := x2*t + one_t*x3;
--              yt := y2*t + one_t*y3;

--              dist1s := (xt-x1)*(xt-x1) + (yt-y1)*(yt-y1);
--              if aseg >=9 and bseg <=12 then
--            dbms_output.put_line('distlt ' || dist1t || ' dist2s ' || dist2s);
--             dbms_output.put_line('dist13 ' || dist13 || ' dist24 ' || dist24);
--  dbms_output.put_line('dist23 ' || dist23 ||'lensq ' || lensq);
--end if;

              If dist1t <= dist13 or dist1t < lensq or
              dist2s >= dist24 or (dist23 <> 0.0 and dist23 < 1.E-11) then
--              If dist1t <= dist13 or dist1t < lensq or
--              dist2t < dist23 or dist1s < lensq or dist23 < 1.E-11 then --dist3t < dist13 Then

                 kounterb2 := kounterb2 + 1;

-- We set a flag to indicate projecting vtx 1 onto segment 2 or
        if flag =0 and ((aseg+1 = bseg) or (aseg = start_end(2)-1 and (a_set = b_set or (b_set = a_set+1 and b_set = a_end_set and bstart=bend)))) then

            flag := 11;
        end if;
-- dbms_output.put_line('flag' ||flag || ' aseg ' || aseg || ' bseg ' || bseg || ' x3 ' || x3 || ' x4 ' || x4);
                 WHILE flag >= 0 LOOP
                 s := Line_intersect(x1,y1,x2,y2,x3,y3,x4,y4,xi,yi,vertex,distance,tolerance,flag);
                if flag = 22 then
                  flag := vertex;
                end if;
--        dbms_output.put_line('TESTING aseg ' || aseg || ' seg ' || bseg || ' n ' || n || ' s ' || s || ' send ' || start_end(2) || ' xtol ' || xtol); --|| ' xi ' || xi || ' yi ' || yi || ' vertx ' || vertex); -- || ' xi ' || xi || ' yi ' || yi);



                if (s >= 0. and s <= 1.) or s = -13. or s = -14. or (s=-23. and aseg +1 <> bseg) or s = -24. then
--                dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1 || ' x2 ' || x2 || ' y2 ' || y2 || ' toler ' || tolerance);
--         dbms_output.put_line('x3 ' || x3 || ' y3 ' || y3 || ' x4 ' || x4 || ' y4 ' || y4 || ' distance ' || distance);
--   dbms_output.put_line('ii ' || bseg || ' seg ' || aseg || ' n ' || n || ' s ' || round(s,10) || ' loop1 ' || loop1_count);
-- Off course a loop intersects itself and we are not interested in
-- intersections between consecutive segments
--dbms_output.put_line('aseg ' || aseg || ' seg ' || bseg || ' n ' || n || ' s ' || s );

                   if (closed = 1. or closed = 3.) then
 --                  dbms_output.put_line('closed');

--                 Allow touches for a loop means that we allow the closed edge to touch
--                 itself like a figure "8". This is ok in Oracle
                      if allow_touches and s=0. and aseg <= Start_end(2) and (bseg-1) <= Start_end(2) then
                              ok := FALSE;

                      else
--                      dbms_output.put_line('aseg ' || aseg || ' se2 ' || start_end(2) || ' se3 ' || start_end(3));
                         if s=0. and (aseg = 1 or aseg = start_end(2) or aseg=ABS(start_end(3))) and (bseg = start_end(2) or bseg = ABS(start_end(3)) or bseg= ABS(start_end(4))) then
                            ok := FALSE;
                         elsif s=1. and aseg = start_end(2) and (bseg = start_end(2) or bseg = ABS(start_end(3)) or bseg=ABS(start_end(4))) then
                         ok := FALSE;
                       end if;
                       end if;

-- Ignore end to end touches between 2 edges
-- If edge1 has n vertices then it goes from segment 1 to segment (n-1)
--
                    elsif closed <> 1. and NOT allow_touches and ( (s=0. and (aseg=1 or aseg = ABS(start_end(3))) and
                    (bseg =ABS(Start_end(3)) or bseg = ABS(Start_end(4))))

                    or (s=1. and aseg= Start_end(2)  and (bseg = ABS(Start_end(3)) or bseg = ABS(Start_end(4))))) then
                       ok := FALSE;
                    end if;

                    if ok then
                       next := next +1;
                       if next > segments.count then
                          segments.extend(20);
                       end if;
                       astore := aseg;
                       if vertex = 2. or s = -23. or s = -24. then astore := astore +1; end if;
                       bstore := bseg;
                       if (vertex = 4.) or s = -24. then bstore := bstore+1; end if;

--                       dbms_output.put_line('>>>storing ' || (astore) || ' with ' || bstore || ' vtx ' || vertex || ' bs ' || bstart || ' bend ' || bend || ' bptr ' || bptr);
--             dbms_output.put_line('xi ' || xi || ' yi ' || yi);
--dbms_output.put_line('st end ' || start_end(2) || ' 3 ' || start_end(3) || ' 4 ' || start_end(4));
--dbms_output.put_line('aseg ' || aseg || ' seg ' || bseg  || ' S' || s || ' v ' ||vertex );
--                       dbms_output.put_line( ' x1 ' || x1 || ' y1 ' || y1 || ' x2 ' || x2 || ' y2 ' || y2);
--                       dbms_output.put_line('x3 ' || x3 || ' y3 ' || y3 || ' x4 ' || x4 || ' y4 ' || y4 || ' toler ' || tolerance);
                       if s = -13 or s = -14 or s = -23 or s=-24 then  -- vertex bstore is too close to segment astore
                           segments(next) := bstore*thousand + astore +abs(s)*0.01 ;
                       elsif vertex > 0  then -- vertex astore (or bstore) is too close to segment bstore (or astore)
                          segments(next) := bstore*thousand + astore + vertex*0.01;
                       else
                          segments(next) := bstore*thousand + astore;
                       end if;

-- We can have situation where vertex 4 projects onto 1-2
-- and then on the next comparison, 1 projects onto 3-4.
-- We keep both but avoid duplicates.
                       if next > 5 and abs(segments(next)) = abs(segments(next-5)) then

                         if distance = segments(next-1) then
                           next := next-1;  -- drop this group
                           end if;
                       else
--                          dbms_output.put_line('>>>storing ' || (astore) || ' with ' || bstore || ' vtx ' || vertex || ' s  ' || s|| ' distance ' || ROUND(distance,6));
--                        dbms_output.put_line('>>>Storing ' || a_set || ' b_set ' || b_set || ' D ' || round(distance,6));
                          segments(next+1) := xi;
                          segments(next+2) := yi;
-- Vertex is -13 or -14 or -23 or -24 for butting lines
                          segments(next+3) := vertex;
-- pass back distance when we have nearly intersecting segments or butting lines that are within tolerance
                          segments(next+4) := distance;
                         next := next+4;
                       end if;

                    else
                      ok := TRUE;
                    end if;
                end if;

                END LOOP;
              End if;


           END iF;

    End Loop;

  End Loop;

--  dbms_output.put_line('leaving self Counter A '|| kountera || 'B ' || kounterb || ' B2 ' || kounterb2 ||' next ' || next);
  If next = 0 then
    segments.trim(segments.count);
    RETURN segments;
  else
    segments.trim(segments.count-next);
    RETURN segments;
  end if;


END CHECK_GEOM_SELF_INTERSECT;
--
FUNCTION RESHAPE_TOO_CLOSE(pschema VARCHAR2,ptopology VARCHAR2, pedge_id1 NUMBER, pedge_id2 NUMBER, tolerance NUMBER,universal_flag VARCHAR2, log_type VARCHAR2) RETURN VARCHAR2 AS

/*
--##############################################################################
--Program Name: Reshape_Too_Close
--Author: Sidey Timmins
--Creation Date: 05/21//2012
--
--Usage:
  -- Call this function from inside another PL/SQL program.  This function
  -- has 7 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT
  --      Schema         - the Schema name
  --      Topology       - the topology name
  --      edge_id1       - the first edge_id
  --      edge_id2       - the 2nd edge_id (or same as the first one).
  --      tolerance      - the Oracle tolerance the face failed at
  --      universal_flag - 'Y' allows reshaping an edge on the UF.
  --      log_type       -  ??
--
-- Purpose: This function move 2 edges apart at one vertex (or reshapes
--          a single edge that goes too close to itself at one place).

-- Method:
--         It checks whether the change has been accepted in the topology.
--         Return a status of 'TRUE' for success or an Oracle error message.
--##############################################################################
*/


  geom1           mdsys.sdo_geometry;
  geom2           mdsys.sdo_geometry;


  New_one         NUMBER;

  sql_stmt                        VARCHAR2(4000);
  Schema                         VARCHAR(30) := UPPER(pSchema);
  Topology                        VARCHAR(20) := UPPER(pTopology);

  status                          VARCHAR2(4000);
  new_geom                        MDSYS.SDO_GEOMETRY;
  new_xys                         MDSYS.SDO_ORDINATE_ARRAY;
  xLL                             NUMBER;
  yLL                             NUMBER;
  xUR                             NUMBER;
  yUR                             NUMBER;
  coord1                          NUMBER;
  edge_id1                        NUMBER := pedge_id1;
  edge_id2                        NUMBER := pedge_id2;
  left_face                       NUMBER;
  right_face                      NUMBER;
  UFS                             NUMBER :=0.0;
  delta                           NUMBER := 0.001;
  mbr_geom                        SDO_GEOMETRY;

  topo_entity_count               NUMBER := 100000;
  ok                              BOOLEAN;
  same                            BOOLEAN := FALSE;
  cache_name                      VARCHAR2(20) := Topology||'topomapcache';

     procedure swap_geometries as
        temp_geom       mdsys.sdo_geometry;
     begin
          UFS := 1.;
          temp_geom := geom1;
          geom1 := geom2;
          geom2 := temp_geom;
          edge_id1 := edge_id2;
          edge_id2 := pedge_id1;
      end;
BEGIN

--  Move apart may move either geometry if the UF switch is not set (UFS=0)
--  but only geometry 2 if the UFS is set, because we set up geometry 1
--  to be the one on the Universal face (UF).

--  Keep track of the geometry(ies) wrt the Universal face and arrange
--  the situation so if either of them are on the UF, it is geometry 1.

    sql_stmt := 'SELECT GEOMETRY,LEFT_FACE_ID,RIGHT_FACE_ID from '||Topology||'_EDGE$ WHERE EDGE_ID=:1';
    EXECUTE IMMEDIATE sql_stmt into Geom1,left_face,right_face using EDGE_ID1;

    if left_face < 0. or right_face < 0. then
          UFS := 1.;
    end if;
--==============================================================================
-- There are 2 cases: a) 1 edge (a possible self intersection)
-- or                 b) 2 edges nearly intersecting

-- Case a) Find closest approach with 1 edge (a near self intersection)

dbms_output.put_line('ID1: '|| edge_id1 || ' Id2 ' || edge_id2);
    if edge_id1 = edge_id2 then
       same := TRUE;
       new_geom := move_apart_2edges(geom1,geom1,same,new_one,UFS,tolerance);
       if new_one = 0.0 then
          RETURN 'FAILURE, no self intersections for edge_id ' || edge_id1 ||' at this tolerance:' ||tolerance;
       end if;
       new_one := edge_id1;
--==============================================================================
-- Case b) Find closest approach with 2 edges
    else
       EXECUTE IMMEDIATE sql_stmt into Geom2,left_face,right_face using EDGE_ID2;

      -- If this edge is on the Universal face, if possible, swap the geometries so
      -- only geom1 is on the UF.

      if (left_face < 0. or right_face < 0) then
          if UFS = 1.  then  -- have a problem, both edges on universal face.
             RETURN 'FAILURE, both edges '||edge_id1 || ' and ' || edge_id2 || ' are on Universal Face';
          end if;
          swap_geometries;  -- now edge_id2 is swapped with edge_id1 so it is on the UF
       end if;

-- We have made it easy for move_apart. If either edge is on the Universal Face,
-- it is edge 1.

       new_geom := move_apart_2edges(geom1,geom2,same,new_one,UFS,tolerance);

-- This situation is bad +------------------------+
---              this is a very skinny angle d  /  D
--               and d << D                   /
--                                   node    *------------------+
--                                                   seg2

-- Since move_apart cannot move geom2, tell it to move geom1
       if UFS =0. and new_one = -1. then
          new_one :=0.;
          swap_geometries;

          new_geom := move_apart_2edges(geom1,geom2,same,new_one,UFS,tolerance);

       end if;

-- Interpret results: a) zero -> don't intersect at tolerance
--                    b) < 0  -> butting edges (so no projection possible) don't intersect
--                    c) 1    -> edge_id1 is the updated geometry
--                    d) 2)   -> edge_id2 is updated

      if new_one = 0.0 then
          RETURN 'FAILURE, no intersections for '||edge_id1 || ' and ' || edge_id2 || ' at this tolerance:'|| tolerance;
      elsif new_one < 0.0 then
-- When we have nearly butting edges then New_one contains minus their distance apart.
          RETURN 'FAILURE, : close butting edges '||edge_id1 ||' and ' || edge_id2 ||' dont intersect: '|| round(abs(new_one),6) || ' meters apart';
      elsif new_one = 1. then
          new_one := edge_id1;
      else
          new_one := edge_id2;
      end if;
    end if;
--==============================================================================
--  Update the topology with the new_geometry for new_one

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


        mbr_geom := Sdo_geom.sdo_mbr(new_geom);

        xLL := mbr_geom.sdo_ordinates(1)-delta;
        yLL := mbr_geom.sdo_ordinates(2)-delta;
        xUR := mbr_geom.sdo_ordinates(3)+delta;
        yUR := mbr_geom.sdo_ordinates(4)+delta;


        sdo_TOPO_MAP.LOAD_TOPO_MAP(cache_name,xLL,yLL,xUR,yUR, 'true');


   dbms_output.put_line('swapping geoemtry for ID ' || new_one);

        status := swap_edge_coords(Topology,new_one,new_geom);


        if status <> 'TRUE' then
          dbms_output.put_line('SWAP EDGE FAILED ' || status);
        else
          sdo_TOPO_MAP.COMMIT_TOPO_MAP();
          dbms_output.put_line('TOPO MAP COMMITTED for edge ' || new_one);
          COMMIT;
        end if;

        sdo_TOPO_MAP.CLEAR_TOPO_MAP(cache_name);
        sdo_TOPO_MAP.DROP_TOPO_MAP(cache_name);

        RETURN status;
END RESHAPE_TOO_CLOSE;
--
FUNCTION MOVE_APART_2EDGES(geom1 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                geom2 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,same BOOLEAN,New_one IN OUT NOCOPY NUMBER,pUFS NUMBER,ptolerance NUMBER default 0.05) RETURN MDSYS.SDO_GEOMETRY AS

-- A function to move apart a zinger and return an updated geometry.
-- Input either two geometries (same = FALSE) and New_one returned tells you
-- which was updated or a single geometry (same=TRUE and geom2=geom1).

-- Locates the problem segments (seg1 and seg2), and the vertex to move. Then if
-- the vertex is not on the Universal Face, it is moved. If not the segment in the
-- the 2nd geometry is adjusted by altering slightly the position of one
-- vertex on seg2. The vertex chosen is has the smallest movement of the end
-- vertices of seg2.

   deg2rad  CONSTANT      NUMBER   :=0.0174532925199432957692369076848861271344;
   new_Geom     MDSYS.SDO_GEOMETRY := Geom1;
   test_Geom    MDSYS.SDO_GEOMETRY := Geom1;
   Xys1         MDSYS.SDO_ORDINATE_ARRAY := Geom1.sdo_ordinates;
   Xys2         MDSYS.SDO_ORDINATE_ARRAY;
   Xys          MDSYS.SDO_ORDINATE_ARRAY;
   Info_array   MDSYS.SDO_ELEM_INFO_ARRAY := MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1);
   List_within  MDSYS.SDO_LIST_TYPE;
   Start_end   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(1,2,1000000,1000000);
   result      MDSYS.SDO_LIST_TYPE;
   dist_apart   NUMBER := 0.0;
   dist_try1    NUMBER;
   dist_try2    NUMBER;
   where_it_is  NUMBER;
   where_it_is2  NUMBER;
   seg1         NUMBER;
   seg2         NUMBER;
   xfound       NUMBER;
   yfound       NUMBER;
   xp           NUMBER;
   yp           NUMBER;
   x1           NUMBER;
   y1           NUMBER;
   x2           NUMBER;
   y2           NUMBER;
   x3           NUMBER;
   y3           NUMBER;
   x4           NUMBER;
   y4           NUMBER;
   xi           NUMBER;
   yi           NUMBER;
   det          NUMBER;
   temp         NUMBER;
   new_x        NUMBER;
   new_y        NUMBER;
   xLL          NUMBER;
   yLL          NUMBER;
   xUR          NUMBER;
   yUR          NUMBER;
   vtx          NUMBER;
   UFS          NUMBER := pUFS;

   xLL2         NUMBER;
   yLL2         NUMBER;
   xUR2         NUMBER;
   yUR2         NUMBER;

   L            NUMBER;
   d            NUMBER;
   dx           NUMBER;
   dy           NUMBER;
   desired_tol  NUMBER := ptolerance + 0.01;
   tolerance    NUMBER := ptolerance*1.05;
   delta        NUMBER := 0.05;
   s            NUMBER := 1.0; -- line parameter
   t            NUMBER;
   oid          NUMBER;
   SRID         NUMBER := GEOM1.SDO_SRID;
   million      NUMBER := 10000000.;
   epsilon      NUMBER;
   xdelta       NUMBER;
   ydelta       NUMBER;
   xdiff        NUMBER;
   vertex       NUMBER;
   new_d        NUMBER;
   n            PLS_INTEGER;
   nn           PLS_INTEGER;
   m            PLS_INTEGER;
   mm           PLS_INTEGER;
   kj           PLS_INTEGER;
   range       PLS_INTEGER;
   pos          PLS_INTEGER :=0;
   ptr          PLS_INTEGER := 1;
   ptr_save     PLS_INTEGER;
   loop_count   PLS_INTEGER :=0;

   status       VARCHAR2(1000);
BEGIN
--==============================================================================
-- Setup.

-- Where_it_is tells us which edge has the close vertex, >0 =1, <0 = 2
   Xys := Xys1;
   Xys2  := XYs1;
   n := XYs.count;
   nn := TRUNC(n/2)-1;
-- dbms_output.put_line('1st Xys ' || n);

      Start_end(1) := 1.;
      Start_end(2) := TRUNC(Xys.count/2) -1;    -- last possible segment (vertex is 1 higher).

 -- We need to set a tolerance in degrees NOT meters
 -- Convert 0.05 meters to degrees

    SRID := Geom1.SDO_SRID;
     if SRID = 8265. then
         xdelta := tolerance/(111319.490793274*cos(xys(2)*deg2rad));
     else
         xdelta := tolerance;
     end if;

-- This vertical deflection was found by checking a horizontal line 1 degree long
-- at all latitudes and finding the max vertical offset (.001091 at 45 degres),
-- It was also found this y offset varies as the square of the length.
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
--lat 45 max diff .001091 per degree

   Set_MBR(Xys,xLL,yLL,xUR,yUR);
   if SRID = 8265. then
         ydelta := .0011;
         xdiff := xUR-xLL;
         if xdiff > 1. then
           ydelta := ydelta * xdiff*xdiff;
         end if;
   else
     ydelta := tolerance;
   end if;

-- If the two geometries are different we concatenate the nearby parts of
-- one geometry to the other using Info. This sets up a single geoemtry for
-- Check_geom_self_interesect which can navigate the separate pieces using Info.

   if same = FALSE then
--     dbms_output.put_line('SAME is false');
     epsilon := desired_tol*2.;
     Set_MBR(Xys,xLL,yLL,xUR,yUR);
     xLL := xLL-xdelta;
     yLL := yLL-ydelta;
     xUR := xUR + xdelta;
     yUR := yUR + ydelta;
     Xys2 := Geom2.sdo_ordinates;
     mm := Xys2.count;

     List_within := Get_Within_MBR(XYs2,xLL,yLL,xUR,yUR);
--     for ii in 1..List_within.count loop
--       dbms_output.put_line('LW ' || list_within(ii));
--     end loop;

-- Make a segment start/end array (the start and end of each geometry) so
-- Check_geom_self_intersect knows whether an edge to edge intersection is to be considered.
-- We just want something like 1,n,n+1,n+m when all vertices of edge 2 are present
-- and variations like this when the last vertex is present but there are gaps
-- created by Get_within_MBR ignoring parts of edge 2. For example, consider 2 edges
-- each with 100 vertices. If all of edge 1 is present and edge 2 has just vertcies
-- 49 to 100 (coordinates 97 to 200) then we build this array:
--   1,100,1000000,151. Here the million means we don't have segment 1 of edge2.


         if List_within.count > 0 then
           if List_within(1) = 1 then
             Start_end(3) := nn+2;
           end if;

           if List_within(list_within.count) = mm then
            Start_end(4) := nn;
  --          dbms_output.put_line('SEND : ' || start_end(4));
            kj := 1;
            While kj < List_within.count Loop

              Start_end(4):= Start_end(4) + TRUNC((List_within(kj+1) - List_within(kj)+1)/2);
  --            dbms_output.put_line('SEND : ' || start_end(2) || ' ' ||start_end(3)||' '||start_end(4));
              kj := kj+2;
            End loop;
           else
             Start_end(4) := 1000000;
           end if;
         end if;

  -- Make a concatenated XY array as it is easier to handle just a single
  -- set of Xys and a single Info array in Check_geom_self_interseect
  -- (We already have to parse an Info array, so adding more coordinates is no big issue
  -- - just have to keep track of the segment number and determine which geometry
  -- is referred to!)

     Xys := Concatenate_Xys_Info(Geom1,Geom2,Info_Array,List_within);
--      for ii in 1..XYS.count loop
--       dbms_output.put_line('XYS ' || XYS(ii));
--     end loop;
--dbms_output.put_line('XYS ' || xys.count);
   end if;

-- dbms_output.put_line('calling check_geom_self ' || tolerance);
--==============================================================================

-- Result is the vertex or vertices

  result := CHECK_GEOM_SELF_INTERSECT(XYs,Info_Array,FALSE,Start_end,tolerance,xdelta,ydelta,18,million,oid);
--for ii in 1..INFO_array.count loop
--       dbms_output.put_line('Info ' || Info_array(ii));
--     end loop;

   if result.count = 0 then
      new_one := 0.0;
      RETURN NULL;

-- If there is no intersection between almost butting lines very close,
-- result contains: 23 or 24 or 13 or 14 (depends on the geometric arrangement).

-- Example 23 :   1+----+2   3+-----+4 means vertices 2 and 3 too close

   elsif (result(ptr)  - TRUNC(result(ptr))) *100. > 4 then

      new_one := result(ptr+4);  -- pass back the distance
      RETURN NULL;
   end if;
-- At present we just want apparent intersections, not actual ones.

-- If there are 2 apparent intersections, get the closest one.

   d := 1E10;
   for ii in 1..TRUNC(result.count/5) Loop
      new_d := result(ptr+4);
      if new_d <= d then
         d := new_d;
         ptr_save :=ptr;
      end if;
      ptr := ptr+5;
   end loop;

   ptr := ptr_save;
   While (dist_apart = 0.0 or dist_apart = 10000000000.) and ptr < result.count Loop

     where_it_is := TRUNC(result(ptr));
     vtx := result(ptr+3);
--     dbms_output.put_line('result ' ||where_it_is || ' x ' || round(result(ptr+1),9) || ' y ' || round(result(ptr+2),9)|| ' vtx ' || result(ptr+3));

-- This function is too slow!!
--  dist_apart := get_closest(geom1,geom2,same,xfound,yfound,where_it_is2);
--    dbms_output.put_line('result ' ||where_it_is2);



-- Seg1 corresponds to Geom1
-- Seg2 corresponds to Geom2

      if vtx <= 2.0 then        -- Geom1 has a point too close to geom2
        seg1 := ABS(MOD(where_it_is,million));
        seg2 := (ABS(where_it_is) -seg1)/million;

      else                       -- Geom2 has a point too close to geom1
        seg1 := ABS(MOD(where_it_is,million));
        seg2 := (ABS(where_it_is) -seg1)/million;

      end if;

-- If its a ring and we have a scissorcut set a special flag to force moving
-- other that start/end node

      if vtx = 4 and same = TRUE and seg2 = TRUNC(XYs1.count/2) and
         (XYs1(1) = Xys1(XYs1.count-1) and Xys1(2) = XYs1(Xys1.count)) then
        vtx := -4;
        temp := seg1;
        seg1 := seg2;
        seg2 := temp;
      end if;

-- If we composited 2 geometries above,
-- then work out the true segment number in Xys2

      pos :=0;
      if same = FALSE and List_within.count >= 2 then
           pos := TRUNC(List_within(1)/2);
           seg2 := seg2 - TRUNC(n/2);
           m := TRUNC(List_within.count/2);
           for ii in 1..m Loop

             range := TRUNC((List_within(ii*2) -List_within(ii*2-1)+1)/2);
             if seg2 <= range then
                seg2 := seg2 + pos;
             elsif ii <> m then
               seg2 := seg2 - range;
               pos := TRUNC(List_within(ii*2+1)/2);
             end if;
           end loop;
      end if;
--           dbms_output.put_line('NOW seg1 ' || seg1 || ' seg2 ' || seg2);

-- This situation is bad +------------------------+
---              this is a very skinny angle    /
--                                            /
--                                   node    *------------------+
--                                                   seg2
--
--
      if (vtx = 3.0 and seg2=1) or (vtx = 4.0 and seg2= TRUNC(mm/2)) then
-- Force caller to call us again with the geometries reversed
-- and the UFS set so we move
         New_one :=-1.;
         RETURN NULL;
      end if;

-- Seg1 and Seg2 cannot be used as vertex numbers if we get the last
-- vertex of the edge. But it may be used as a vertex number as shown here.
-- Below when we want to set up segments for perpendicular, we go
-- backwards when we reach the end of the edge.

      if vtx <= 2.0 then        -- Geom1 has a point too close to geom2
        xfound := XYs1(seg1*2-1);
        yfound := XYs1(seg1*2);
--      elsif vtx = 4.0 then                       -- Geom2 has a point too close to geom1

--        xfound := XYs2(seg2*2+1);
--        yfound := XYs2(seg2*2+2);
      else                       -- Geom2 has a point too close to geom1

        xfound := XYs2(seg2*2-1);
        yfound := XYs2(seg2*2);
      end if;

--        dbms_output.put_line('seg1 ' || seg1 || ' seg2 ' || seg2);
--        dbms_output.put_line('xf ' || ROUND(xfound,8) || ' yf ' || ROUND(yfound,8));

--       dbms_output.put_line('seg1 ' || seg1 || ' xys1 ' || xys1.count ||' seg2 ' || seg2 || ' xys2 ' || xys2.count);

    -- When where_it_is is positive, Geom1 has a point which is too close too Geom2
--        dbms_output.put_line('WHERE ' || where_it_is );



      if vtx <= 2.0 and UFS <> 1.0 then

-- We want to move (xfound,yfound) on Geom1 slightly further away from the segment
-- on Geom2. Work out the perpendicular point (xp,yp) on Geom2.
-- If the UFs is set (to 1) then we will move Geom2 instead and effectively
-- move (xp,yp).

        x1 := Xys2(seg2*2-1);
        y1 := Xys2(seg2*2);
        if seg2*2 = Xys2.count then
          x2 := XYs2(seg2*2-3);
          y2 := Xys2(seg2*2-2);
        else
          x2 := XYs2(seg2*2+1);
          y2 := Xys2(seg2*2+2);
        end if;

        x3 := XYs1(seg1*2-1);
        y3 := XYs1(seg1*2);
        if seg1*2 = Xys1.count then
          x4 := Xys1(seg1*2-3);
          y4 := Xys1(seg1*2-2);
        else
        x4 := Xys1(seg1*2+1);
        y4 := Xys1(seg1*2+2);
        end if;
        New_one := 1.;

        dist_apart := perpendicular(xfound,yfound,x1,y1,x2,y2,xp,yp,FALSE,SRID=8265.);

--       dbms_output.put_line('xp ' || round(xp,9) || ' yp ' || round(yp,9) || ' Dist apart ' || round(dist_apart,6));
      else
-- We want to move (xfound,yfound) on Geom2 slightly further away from the segment
-- on Geom1. Work out the perpendicular point (xp,yp) on Geom1. If the UFS is set, we
-- will not move Geom1 because the caller has set the call so that Geom2 is
-- not on the Universal face.


        x1 := Xys1(seg1*2-1);
        y1 := Xys1(seg1*2);
        if seg1*2 = Xys1.count then
          x2 := XYs1(seg1*2-3);
          y2 := Xys1(seg1*2-2);
        else
        x2 := XYs1(seg1*2+1);
        y2 := Xys1(seg1*2+2);
        end if;

        x3 := XYs2(seg2*2-1);
        y3 := XYs2(seg2*2);
        if seg2*2 = Xys2.count then
          x4 := Xys2(seg2*2-3);
          y4 := Xys2(seg2*2-2);
        else
          x4 := Xys2(seg2*2+1);
          y4 := Xys2(seg2*2+2);
        end if;
        New_one := 2.;
        dist_apart := perpendicular(xfound,yfound,x1,y1,x2,y2,xp,yp,FALSE,SRID=8265.);

--        dbms_output.put_line('Xp ' || round(xp,9) || ' yp ' || round(yp,9)|| ' Dist apart ' || round(dist_apart,6));
      end if;
      if dist_apart > 1. then   -- used to be 0.1
         dist_apart := 10000000000.;
      end if;
      ptr := ptr + 5;
  End Loop;

  new_x := x3;
  new_y := y3;
-- We want to move slightly away from the other geometry;
  if dist_apart <> 0.0 then
    delta := desired_tol*1.01/dist_apart;
  else
    delta := desired_tol*2.;
  end if;

-- However if we crossed over it we want to go past it.
    t := GZ_SUPER.Line_intersect(x1,y1,x2,y2,x3,y3,x4,y4,xi,yi,det);
--        dbms_output.put_line(' t ' || round(t,10) || ' xi ' || xi || ' yi ' || yi);
    if t > 0. and t < 1. then
      delta := -(delta);
      s := 0.0;
      dist_apart := 0.0;
    end if;


-- The Universal face switch is set to zero or 1, never 2.
-- So there are 2 cases, we may move geometry 1 (UFS =0 and New_one=1)
--                                or geometry 2 (UFS =0 and New_one=2)
--                                or UFS is set to 1, and we move geometry 2.

    if (UFS = 0. or UFS <> New_one) and vtx <> -4 THEN

      While loop_count < 20 and dist_apart < desired_tol Loop
        loop_count := loop_count+1;
        s := s + delta;
--        dbms_output.put_line('S ' || s || ' xf ' || xfound || '  xp ' || xp);
-- Calculate how to move (xfound,yfound) away from (xp,yp)
        new_x := s*xfound + (1.-s) * xp;
        new_y := s*yfound + (1.-s) * yp;
        test_geom := MDSYS.SDO_GEOMETRY(2002,8265.,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),
                                          MDSYS.SDO_ORDINATE_ARRAY(new_x,new_y,xp,yp));
        dist_apart := SDO_GEOM.SDO_LENGTH(test_geom,0.05,'unit=meter');
        dbms_output.put_line('now DISTANCE ' || round(dist_apart,6)|| ' diff ' || (desired_tol-dist_apart));
      End Loop;

-- Make the new geometry !!
      If New_one = 1. then  -- UFS =0 and we move edge 1
--       dbms_output.put_line('moving vertex ' || seg1 || ' '||new_x ||','||new_y);
        Xys1(seg1*2-1) := new_x;
        Xys1(seg1*2)   := new_y;
        New_Geom.SDO_Ordinates := Xys1;
      Else                  -- New_one=2 and (UFS = 0 or UFS=1) so we move edge 2
--      dbms_output.put_line('Moving vertex ' || seg2 || ' ' ||new_x ||','||new_y);
        Xys2(seg2*2-1) := new_x;
        Xys2(seg2*2)   := new_y;
        New_Geom.SDO_Ordinates := Xys2;
      End If;

   ELSE

-- Just 1 case: UFS =1 and New_one = 1
-- and we are not allowed to disturb Geom1.
-- We have to move either (x1,y1) or (x2,y2) from geometry 2.
--dbms_output.put_line('Before loop DISTANCE apart ' || round(dist_apart,6) );
      While loop_count < 20 and dist_apart < desired_tol Loop
        loop_count := loop_count+1;
        s := s + delta;
-- Calculate a potential new placement for (xp,yp)
        new_x := (1.-s)*xfound + s * xp;
        new_y := (1.-s)*yfound + s * yp;
        test_geom := MDSYS.SDO_GEOMETRY(2002,8265.,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),
                                          MDSYS.SDO_ORDINATE_ARRAY(new_x,new_y,xfound,yfound));
        dist_apart := SDO_GEOM.SDO_LENGTH(test_geom,0.05,'unit=meter');
--        dbms_output.put_line('now DISTANCE ' || round(dist_apart,6) || ' dif ' || (desired_tol-dist_apart));
      End Loop;

-- Ok we know how to move xp,yp. Now figure out using similar triangle how to
-- move either (x1,y1) or (x2,y2) instead. We leave (xfound,yfound) alone.

--
--                                                        + (x4,y4)
--                            + (xfound,yfound)           |
--                            |                           |
--  (x1,y1)+------------------+---------------------------+ (x2,y2)
--         <--       d     -->(xp,yp)
--         <                       L                    -->
--                            |
--                            v direction of movement

      test_geom.sdo_Ordinates := MDSYS.SDO_ORDINATE_ARRAY(x1,y1,new_x,new_y);
      d := SDO_GEOM.SDO_LENGTH(test_geom,0.05,'unit=meter');
      test_geom.sdo_Ordinates := MDSYS.SDO_ORDINATE_ARRAY(x1,y1,x2,y2);
      L := SDO_GEOM.SDO_LENGTH(test_geom,0.05,'unit=meter');

      dx := (new_x-xp);
      dy := (new_y-yp);
      if d <> 0. and (L/d) < 20. then
        dx := (L/d) * (new_x-xp);
        dy := (L/d) * (new_y-yp);
      end if;

      new_x := x1 + dx;
      new_y := y1 + dy;

      dist_try1 := perpendicular(xfound,yfound,new_x,new_y,x2,y2,xp,yp,FALSE,SRID=8265.);

      new_x := x2 + dx;
      new_y := y2 + dy;
      dist_try2 := perpendicular(xfound,yfound,x1,y1,new_x,new_y,xp,yp,FALSE,SRID=8265.);

-- The aim here is just to move one vertex as little as possible
-- yet accomplish our goal of perturbing the (xfound,yfound) to (xp,yp) distance

      if dist_try1 > desired_tol and dist_try1 < dist_try2 then
        new_x := x1 + dx;
        new_y := y1 + dy;
        Xys2(seg2*2-1) := new_x;
        Xys2(seg2*2) := new_y;
--        dbms_output.put_line('dx ' || round(dx,12) || ' dy '|| round(dy,12));
      else

        new_x := x2 + dx;
        new_y := y2 + dy;
        Xys2(seg2*2+1) := new_x;
        Xys2(seg2*2+2) := new_y;
--        dbms_output.put_line('Dx ' || round(dx,12) || ' dy '|| round(dy,12) || ' seg2 ' || seg2);
      end if;
      New_Geom.SDO_Ordinates := Xys2;

   END IF;

-- Check for failure

   if dist_apart < tolerance then
      New_one :=0.0;
   end if;

--   status := GZ_GEOM_UTILS.validate_lines_with_context(new_geom,tolerance);
--   dbms_output.put_line('HELLO'||status);
---   If New_one = 1 then
--    dist_apart := get_closest(new_geom,geom2,same,xfound,yfound,where_it_is);
--  Else
--    dist_apart := get_closest(geom1,new_geom,same,xfound,yfound,where_it_is);
--  End If;
  dbms_output.put_line('New_one ' || new_one || ' dist apart now ' || round(dist_apart,4) || ' where ' || where_it_is);
  RETURN New_geom;

END MOVE_APART_2EDGES;
--
FUNCTION GET_CLOSEST_SEGMENT(
  px NUMBER,py NUMBER,Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,pSRID NUMBER,where_it_is IN OUT NOCOPY NUMBER) RETURN NUMBER AS

-- A function to get the closest segment from a point to the geometry.
-- (The point is presumed to NOT be on the geometry.)
-- To find the closest between 2 edges, call this function with each point in
-- the first geometry. Then repeat with the geometries reversed.

rad2deg     CONSTANT NUMBER := 57.29577951308232087679815481410517033235;
  nvert   PLS_INTEGER;

  b1      NUMBER;
  b2      NUMBER;
  x       NUMBER := px;
  y       NUMBER := py;
  x1      NUMBER;
  y1      NUMBER;
  x2      NUMBER;
  y2      NUMBER;
  az1     NUMBER;
  az2     NUMBER;
  dist    NUMBER;
  xp      NUMBER;
  yp      NUMBER;
  included_angle NUMBER;
  best_dist NUMBER := 1.E10;

  SRID    NUMBER := NVL(pSRID,0.);
  last_brng_diff     NUMBER :=1000.;
  distO     NUMBER;

BEGIN

  nvert := TRUNC(Xys.count/2);
  x2 := Xys(1);
  y2 := XYs(2);
  For ii in 2.. nvert Loop
     x1 := x2;
     y1 := y2;
     x2 := Xys(ii*2-1);
     y2 := Xys(ii*2);


     if (x <> x1 or y <> y1) and (x <> x2 or y <> y2) then
     if SRID = 8265. then
       az1 := GZ_UTIL_ZONE.geo_bearing(x,y,x1,y1);
       az2 := GZ_UTIL_ZONE.geo_bearing(x,y,x2,y2);
     else
       az1 := GZ_MATH.new_arctan(y1-y,x1-x) * rad2deg;
       az2 := GZ_MATH.new_arctan(y2-y,x2-x) * rad2deg;
     end if;
     included_angle := GZ_UTIL_ZONE.angle(x1,y1,x,y,x2,y2,b1,b2,SRID);

-- If the two lines have similar bearing, then (x,y) must be very close
-- to the line.
-- Now check to see if the perpendicular projects onto it?


--     If ABS(b1-az1) < last_brng_diff or ABS(b2-az2) < last_brng_diff  then
       dist := perpendicular(x,y,x1,y1,x2,y2,xp,yp,FALSE,SRID=8265.);

       if xp is not NULL then
--       dbms_output.put_line('x ' || x || ' y ' || y);
--       dbms_output.put_line('X ' || xp || ' Y ' || yp);
      distO := sdo_geom.sdo_length( mdsys.sdo_geometry(2002,8265.,NULL,
      mdsys.sdo_elem_info_array(1,2,1),mdsys.sdo_ordinate_array(x,y,xp,yp)),0.05,'unit=meter');
--      dbms_output.put_line('dist ' || round(dist,10) || ' distO ' || round(disto,10));
      end if;
       if dist < best_dist and dist <> 0.0 then
         best_dist := dist;
         where_it_is := (ii-1);  -- label segments with the start vertex
       end if;
        If ABS(b1-az1) < last_brng_diff then
          last_brng_diff := ABS(b1-az1);
        else
          last_brng_diff := ABS(b2-az2);
        end if;
 --    End if;
 End if;
  End Loop;

  RETURN best_dist;
END GET_CLOSEST_SEGMENT;
--
FUNCTION GET_CLOSEST(geom1 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                geom2 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,same BOOLEAN,xfound IN OUT NOCOPY NUMBER,yfound IN OUT NOCOPY NUMBER,where_it_is IN OUT NOCOPY NUMBER) RETURN NUMBER AS


  Xys1     MDSYS.SDO_ORDINATE_ARRAY := Geom1.sdo_ordinates;
  Xys2     MDSYS.SDO_ORDINATE_ARRAY := Geom2.sdo_ordinates;

  nvert1   PLS_INTEGER;
  nvert2   PLS_INTEGER;

   x       NUMBER;
   y       NUMBER;
   dist1   NUMBER;
   dist2   NUMBER;
   million NUMBER := 1000000.;

   where_it NUMBER;
   best_dist NUMBER := 1.E10;
BEGIN

   nvert1 := TRUNC(Xys1.count/2);
   for ii in 1..nvert1 Loop
      x := Xys1(ii*2-1);
      y := Xys1(ii*2);

      dist1 := Get_Closest_Segment(x,y,Xys2,Geom2.SDO_SRID,where_it);
      if dist1 < best_dist and dist1 <> 0. then
         where_it_is := where_it*million +ii;
         best_dist := dist1;
         xfound := x;
         yfound := y;
      end if;
   end loop;
--   if same = FALSE then
   nvert2 := TRUNC(Xys2.count/2);
   for ii in 1..nvert2 Loop
      x := Xys2(ii*2-1);
      y := Xys2(ii*2);

      dist2 := Get_Closest_Segment(x,y,Xys1,Geom1.SDO_SRID,where_it);
      if dist2 < best_dist and dist2 <> 0. then
         where_it_is := -where_it*million -ii;  -- flag to indicate geom2 is near geom1
         best_dist := dist2;
         xfound := x;
         yfound := y;
      end if;
   end loop;
 --  end if;
  RETURN best_dist;
END GET_CLOSEST;
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

rad2deg     CONSTANT NUMBER := 57.29577951308232087679815481410517033235;

     u             NUMBER;
     distance      NUMBER := 1.E10;
     length_sq     NUMBER;
     dx            NUMBER := x2-x1;
     dy            NUMBER := y2-y1;

     cosy          NUMBER;
     new_x         NUMBER;
     new_y         NUMBER;
     measure       NUMBER;
BEGIN
--     dbms_output.put_line('xin ' || xin || ' yin ' || yin);

     u := ((Xin - X1) * dx + (Yin-Y1) * dy);

--     dbms_output.put_line('U is ' ||U || ' ' || (u/(dx*dx + dy*dy)));
--     dbms_output.put_line('x1 ' || x1 || ' y1 ' || y1);
--     dbms_output.put_line('x2 ' || x2 || ' y2 ' || y2);
     xnear := NULL;
     ynear := NULL;
     If u >= 0. or Always then
        length_sq :=  dx*dx + dy*dy;
        if (u <= length_sq or Always) and length_sq > 0. then

           if meters then
--           cosy := cos((y1+y2)*0.5*rad2deg);
--           dx := dx*cosy;
--           length_sq := dx*dx + dy*dy;
--           u := (cosy*(Xin - X1) * dx + (Yin-Y1) * dy);
           u := u/length_sq;


           xnear := X1 + u * dx; -- X1 + u * dx/cosy;
           ynear := Y1 + u * dy;

--           dbms_output.put_line('>U ' || U || ' xn ' || xnear || ' yn ' || ynear);
--           dbms_output.put_line('xN ' || xin || ' Yn ' || yin);
             Distance := GZ_UTIL_ZONE.accurate_gcd(xin,yin,xnear,ynear,8265.);
--              dbms_output.put_line('ddddistance ' || round(distance,8) || ' U ' || round(u,8));

---  dbms_output.put_line('xnear ' || round(xnear,10) || ' ynear  ' || round(ynear,10));
           else   -- eof on communication channel ?? Added these 2 lines
             u := u/length_sq;
             xnear := X1 + u * dx;
             ynear := Y1 + u * dy;
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

END;
--
Function karney(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER IS

 gcd_k NUMBER;
 az1   NUMBER;
 az2   NUMBER;
 m12   NUMBER;
 s     NUMBER;
BEGIN
--  s := geodesic.inverse(y1,x1,y2,x2,gcd_K,az1,az2,m12);
  RETURN gcd_k;
END KARNEY;
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
-- May not want this discontinuity in GZ_TOPOFIX
--  Elsif dist_in_meters <= 0.0000057 then
--    dist_in_meters := GZ_TOPOFIX.short_oracle_dist(x1,y1,x2,y2);

  Elsif (abs(dx) + abs(dy) > 0.1) then
    dist_in_meters := fast_vincenty_gcd(x1,y1,x2,y2);
  Else

-- This code is derived from Accurate_GCD and has been tested against
-- Charles Karney'code in the geodesic package. The results are amazing.
-- For lat/long differences(total) < 0.1 degrees, the error is < 0.001 meters.
-- For differences < 0.04 degrees, the distance error is < 0.0003 meters.
-- Provided lat/long differences are < 0.1 degrees, the relative error is
-- less than 1 part in 1 million.

    ysin := GZ_MATH.sincos(y1*deg2rad,ycos);
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

--------------------------------------------------------------------------------

          if abs(dy) >=  0.0001 then
             cosy2 := cosy - ysin * dy*deg2rad; -- small angle approximation for cos(y2)
             dist_in_meters := sqrt(dy*dy + dx*dx*cosy*cosy2) * dist_factor;
          else
             dist_in_meters := sqrt(dx*dx*cosy*cosy + dy*dy) * dist_factor;
          end if;
  End if;

  Return dist_in_meters;

End DISTANCE_FCN;
--
Function OLDDistance_Fcn(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER IS
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

  Elsif (abs(dx) + abs(dy) > 0.1) then
    dist_in_meters := fast_vincenty_gcd(x1,y1,x2,y2);
  Else

-- This code is derived from Accurate_GCD and has been tested against
-- Charles Karney'code in the geodesic package. The results are amazing.
-- For lat/long differences(total) < 0.1 degrees, the error is < 0.001 meters.
-- For differences < 0.04 degrees, the distance error is < 0.0003 meters.
-- Provided lat/long differences are < 0.1 degrees, the relative error is
-- less than 1 part in 1 million.

    ysin := GZ_MATH.sincos(y1*deg2rad,ycos);
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

--------------------------------------------------------------------------------

          if abs(dy) >=  0.0001 then
             cosy2 := cosy - ysin * dy*deg2rad; -- small angle approximation for cos(y2)
             dist_in_meters := sqrt(dy*dy + dx*dx*cosy*cosy2) * dist_factor;
          else
             dist_in_meters := sqrt(dx*dx*cosy*cosy + dy*dy) * dist_factor;
          end if;
  End if;

  Return dist_in_meters;

End OLDDISTANCE_FCN;
--
FUNCTION Fast_Vincenty_gcd(x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,units VARCHAR2 DEFAULT 'm') RETURN NUMBER DETERMINISTIC AS
/**
--##############################################################################
-- Program Name: fast_vincenty_gcd
-- Author: Sidey Timmins
-- Creation Date: 8/16/2006
-- Modified: Oct 13/2011 Sidey Timmins Fixed the constants so it matches the
--                       geodesic package inverse calculation. Wow!
-- Usage:
--    Call this program from inside another PL/SQL program.  This program
--    has 4 required parameters:
--
--    REQUIRED Parameters:
--        x1,y1           - 1st point longitude,latitude) (degrees)
--        x2,y2           - 2nd point longitude,latitude  (degrees)
--
--    OPTIONAL Parameter:
--        units        - 'USM' : US Miles
--                     - 'm' : meters (default)
--                     - 'ft': feet (US)
-- Purpose:
--    Calculates Great circle distance (shortest line on the ellipsoid - a model
--    of the earth - a sphere with flattening at the poles) very accurately. The
--    line is between 2 points in geodetic coordinates (degrees). The trace of
--    this line on the elipsoid is called a geodesic. Uses GRS 80 ellipsoid.
--
--    This function exceeds the Oracle sdo_length accuracy (
--    set length = sdo_geom.sdo_length(geometry,0.5,'unit=meter') and matches to
--    3 decimal digits (or more) the superlative accuracy of Charles Karney's code in the
--    Geodesic package for great circles up to half way round the world!
--    It (like Oracle) does not work for antipodal and near antipodal points.
--    This function is about 1.5 faster than sdo_length.
--
--    Returns: Great circle distance in meters
--
-- Reference: T.Vincenty "Direct and Inverse Solutions of Geodesics on the
--            Ellipsoid with applications of nested equations", Survey Review,
--            Vol XXII, 176, April 1975
--            http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf
--            http://www.movable-type.co.uk/scripts/latlong-vincenty.html
--            for ellipsoids see: http://www.pcigeomatics.com/cgi-bin/pcihlp/PROJ%7CEARTH+MODELS%7CELLIPSOIDS%7CELLIPSOID+CODES
-- Dependencies:
--    sincos
--    new_arctan
--
-- Limits: This function does not work correctly for antipodal points and points
--         within about 0.6 degrees longitude of antipodicity.
--         Use geodesic.inverse instead.
--
-- Modification History:
--    09/14/2007, Nick Padfield - Ported code into the CDB_NN Package
--
--##############################################################################
*/
  twopi       NUMBER   :=6.2831853071795864769252867665590057684;     -- 2 * pi
  deg2rad     NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
  a           NUMBER   := 6378137.;        -- GRS-80  ellipsoid radius in meters
  b           NUMBER   := 6356752.31414034724399572718485828; --   3141403558478521068615295331; --minor radius

  a2          NUMBER   := 40680631590769.;               -- ellipsoid radius ^2
  b2          NUMBER   := 40408299983328.65993208642012581835048446; --    76931725432420763255799582; 849393114;  -- minor radius ^2
--  e2          NUMBER   := 0.00669438002290078762535911470306; -- (a2-b2)/a2
  eprime2     NUMBER   := 0.0067394967754816834794891652411525; -- (a2-b2)/b2
  F           NUMBER   := 0.0033528106811836678961698086983268;  -- 0.003352810681182318935434146126128510783424;
  lambda      NUMBER ;
  u1          NUMBER ;
  u2          NUMBER ;
  one_ov5040  NUMBER := 0.000198412698412698412698412698412698412698;
  one_ov120   NUMBER := 0.008333333333333333333333333333333333333333;
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
 u1 := GZ_MATH.new_arctan(Tantheta1,1.) ;
 u2 := GZ_MATH.new_arctan(Tantheta2,1.) ;


 sinU1 := GZ_MATH.sincos(u1,cosU1) ;
 sinU2 := GZ_MATH.sincos(u2,cosU2) ;


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
      sinLambda := GZ_MATH.sincos(lambda,cosLambda) ;
    end if;


-- Note: exponentiation is not as accurate.
    sinSigma := Sqrt((cosU2 * sinLambda) * (cosU2 * sinLambda) +
   (sinU2cosU1 - sinU1cosU2 * cosLambda)* (sinU2cosU1 - sinU1cosU2 * cosLambda)) ;
    If (sinSigma = 0.) Then
        gcd := 0. ;
        RETURN gcd;                --  co-incident points
    End If ;
    cosSigma := sinU1sinU2 + cosU1cosU2 * cosLambda ;
    Sigma    := GZ_MATH.new_arctan(sinSigma,cosSigma) ;

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

  uSq  := cosSqAlpha * eprime2 ;

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

   RETURN ROUND(gcd,8);

END fast_vincenty_gcd;
--
FUNCTION GET_NEARBY_EDGES(Edges_Table VARCHAR2,EDGE_ID_COLUMN VARCHAR2, GEOMETRY_COLUMN VARCHAR2,Edge_id NUMBER,pxLL NUMBER,pyLL NUMBER,pxUR NUMBER,pyUR NUMBER,SRID NUMBER,GREATER VARCHAR2 DEFAULT 'NO',pxepsilon NUMBER default NULL,pyepsilon NUMBER default NULL,ptolerance NUMBER default 0.05) RETURN MDSYS.SDO_LIST_TYPE AS
/*
--##############################################################################
--Program Name: Get_Nearby_edges
--Author: Sidey Timmins
--Creation Date: 05/06/2010
--Updated: 06/19/2012 To only get edges > input edge_id
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
--           the nearby extent (MBR) of the edge in question. No geometries are pulled.

-- Method:  Uses standard computer graphics code for clipping to a window
--          moderated slightly by borders to allow for geodetic coordinates.
--          This function may be slightly conservative and find a few edges which
--          are not within the subject's MBR but so far, I have not found any
--          edges missed.
--
-- Reference:
-- Dependencies: Requires the input table exists with a spatial index.
-- Called by: Check_for_nearby_segments
--##############################################################################
*/

    deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
    Nearby_Edges       MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();
    rectangle          MDSYS.SDO_GEOMETRY;
    sql_stmt    VARCHAR2(4000);

    xLL         NUMBER := pXLL;
    yLL         NUMBER := pYLL;
    xUR         NUMBER := pXUR;
    yUR         NUMBER := pyUR;
    epsilon     NUMBER := pyepsilon;
    delta       NUMBER := pxepsilon;
    xdiff       NUMBER;


BEGIN

-- This function fails badly when one of these is NULL.

   If xLL is NULL or yLL is NULL or xUR is NULL or yUR is NULL then
      dbms_output.put_line('ERROR: Get nearby edges has null mbrs <<<');
      RETURN NULL;
   end if;


-- Setup a rectangle with different tolerances for meridians and small circles.

-- Convert 0.05 meters to degrees
   if delta is NULL and SRID = 8265. then
       delta := ptolerance/(111319.490793274*cos(yUR*deg2rad));
   elsif delta is NULL then
       delta := ptolerance;
   end if;

   xLL := xLL - delta;
   xUR := xUR + delta;

-- This vertical offset was found by checking a horizontal line (a small circle)
-- 1 degree long at all latitudes and finding the max vertical offset (.001091).
-- It was also found that this offset varied with latitude and as the square of the length.

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
         if xdiff > 1. then   -- use squaring when widht > 1 degree
           epsilon := epsilon * xdiff*xdiff;
         end if;
   elsif epsilon is NULL then
     epsilon := ptolerance;
   end if;
   yLL := yLL - epsilon;
   yUR := yUR + epsilon;

   rectangle := MDSYS.sdo_geometry(2003,SRID,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,3),
                                    MDSYS.SDO_ORDINATE_ARRAY(xLL,yLL,xUR,yUR));

-- When Greater = 'YES', only get edge_ids that are greater than subject id.
-- (Check_for_nearby_segments gets all edges when the user specifies a single edge.)

   if Greater <> 'YES' then
     sql_stmt := 'SELECT m.'||EDGE_ID_COLUMN||' FROM '|| Edges_Table  || ' m WHERE m.' ||
                 EDGE_ID_COLUMN||'<>:1 AND SDO_FILTER(m.'|| GEOMETRY_COLUMN||',:2) = ''TRUE''';
   else
     sql_stmt := 'SELECT m.'||EDGE_ID_COLUMN||' FROM '|| Edges_Table  || ' m WHERE m.' ||
                 EDGE_ID_COLUMN||' >:1 AND SDO_FILTER(m.'|| GEOMETRY_COLUMN||',:2) = ''TRUE''';
   end if;

-- SDO_FILTER is much faster than these range checks on a large table
--   'NOT( (( m.XUR <:1)  OR (m.YUR <:2)) OR  (( m.XLL >:3) OR (m.YLL > :4)))';


-- Get nearby edges and exclude Edge_id itself

    EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO Nearby_Edges USING Edge_id,rectangle;

-- we may return an empty array it would seem.

    IF Nearby_Edges is NULL THEN
       nearby_Edges := MDSYS.SDO_LIST_TYPE();
    END IF;

  RETURN Nearby_Edges;    -- We may return an empty array but not NULL

END GET_NEARBY_EDGES;
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
                              vertex IN OUT NOCOPY NUMBER,
                              distance IN OUT NOCOPY NUMBER,
                              tolerance NUMBER,
                              flag_2way IN OUT NOCOPY PLS_INTEGER)
            RETURN NUMBER Deterministic IS
/*
--##############################################################################
--Program Name: line_intersect
--Author: Sidey Timmins
--Creation Date: 4/24/2007

--Usage:
  -- Call this program from inside another PL/SQL program.  This program
  -- has 13 required parameters:
  --
  --   REQUIRED Parameters:
  --      INPUT

  --      X1,Y1      - start point (x,y) of line 1 (point A)
  --      X2,Y2      - end point (x,y) of line 1 (point B)
  --      X3,Y3      - start point  (x,y) of line 2
  --      X4,Y4      - end point (x,y) of line 2
  --      tolerance  - tolerance to check for near intersections
  --      OUTPUT
  --      xi,yi      - the intersection point
  --      vertex     - 1,2,3 or 4 indicate vertex 1 or 2 project onto line 3,4
  --                   or vertex 3 or 4 project onto the line 1,2.
  --      distance   - distance between butting lines (zero for intersecting lines)
  --      tolerance  - number in meters
  --      flag_2way  - a two-way flag - usually zero. Here are the possible
  --                   values:
  --                        zero on input        : first call
  --                        1,2,3,4 on input     : vertex already found
  --                        11 on input          : ignore vertex intersection
  --                                               and look for a projection from
  --                                               vtx 1 to seg 2 or vtx n to seg n-2
  --                        -1 on output         : don't call me again.
  --                        22 on output         : there are 2 intersections

  --                   Usually there is only one intersection,
  --                   but when there are 2, line intersect returns the
  --                   closest intersection. If there are 2 intersections found,
  --                   on the 2nd call specify the vertex already found to get
  --                   the intersection that is further apart.
  --                   This example has 2 potential intersections:
  --                 +-------------------------------------+   edge 1
  --                              +--------------------------------+   edge2
  --
-- Purpose: Determine where 2 lines intersect
  -- Return:      zero to 1.:  intersection exists. The value returned is the measure
  --                           (line parameter) on line 1-2.
  --                      -1:  no intersection
  --                      -2:  parallel
  --                   -3,-4:  concident lines
  --                   if the lines are within tolerance but a perpendicular
  --                   cannot be dropped from one line to the other
  --                    -13: vertices 1 and 3 are distance apart.
  --                    -14: vertices 1 and 4 are distance apart.
  --                    -23: vertices 2 and 3 are distance apart.
  --                    -24: vertices 2 and 4 are distance apart.
   --      The line parameter along line 1 (A to B) will be from 0 to 1 if there
   --      is an intersection point C at (xi,yi).
  --
  --                   C
  --            A+-----.------+B
  --          zero     R      1.0
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
-- After adding 4 million meters to x and 2 million meters to y the new
-- tolerances shown below enabled x1,y1 (and/or x2,y2) to be changed by 1.E-7

-- Reference: A Programmer's Geometry: Adrian Bowyer and John Woodwark
--  http://local.wasp.uwa.edu.au/~pbourke/geometry/pointline/
-- Dependencies: none
--##############################################################################
*/
   s    NUMBER;
   t    NUMBER;
   try  NUMBER;

   len_sq   NUMBER;
   d        NUMBER;
   xt       NUMBER;
   yt       NUMBER;
   dx21     NUMBER := x2-x1;
   dy21     NUMBER := y2-y1;
   dx31     NUMBER := x3-x1;
   dy31     NUMBER := y3-y1;
   dx43     NUMBER := x4-x3;
   dy43     NUMBER := y4-y3;

   dist      NUMBER;
   lastd     NUMBER := tolerance;

   oneplus  CONSTANT NUMBER := 1.00000000000000000000000000000000000001;
   det      NUMBER;
   u        NUMBER;
   xcheck   NUMBER;
   ycheck   NUMBER;
   pflag    PLS_INTEGER := flag_2way;
   kount    PLS_INTEGER :=0;

BEGIN

   vertex :=0.0;
   distance := 0.0;
   flag_2way :=-1;
-- Check parametric equations for two lines: s is on line 1
--                                           t is on line 2

   det := dx43 * dy21  - dx21 * dy43 ;

   IF det <> 0.0 THEN
      s := (dx43 * dy31 - dy43 * dx31) /det;
      t := (dx21 * dy31 - dy21 * dx31) /det;

-- If both line parameters are within [0,1] then the lines intersect
--dbms_output.put_line('SS ' || s || ' t ' || t);


      If pflag <> 11 and s >= 0.0 and s < oneplus and t >= 0.0 and t < oneplus then

        xi := X1 + s * dx21;
        yi := Y1 + s * dy21;

        RETURN s;

      Else

-- Set up latitude and longitude checks.

        ycheck := 0.00009*tolerance;
        if ABS(y1) > 50. then
           xcheck := .00014*tolerance;
        else
           xcheck := 0.00029*tolerance;
        end if;



        len_sq := dx21*dx21 + dy21*dy21;
        FOR ii in 3..4 LOOP
        if ii=3 then
          u := dx31*dx21 + dy31*dy21;
--        dbms_output.put_line('U' || u);
-- drop perpendicular to x1,y1 _> x2,y2 from x3,y3 or x4,y4
          try :=3.;  --from x3,y3
          xt := x3;
          yt := y3;
        else
          u := (x4-x1) * dx21 + (y4-y1)*dy21;  -- try x4,y4
--           dbms_output.put_line('Uu' || u || ' len sq ' || len_sq);
          try := 4;
          xt := x4;
          yt := y4;
        end if;
--         dbms_output.put_line('U ' || u ||  'lensq ' || len_sq);
        IF u >= 0.0 and u < len_sq THEN
          u := u / len_sq;
          xi := X1 + u * dx21;
          yi := Y1 + u * dy21;
--dbms_output.put_line('ii ' || ii || ' U ' || u || ' dx21 ' || dx21 || ' dy21 ' || dy21);

  -- pretend near glances intersect
          if ABS(y1) > 50. and (abs(xt-xi) > xcheck  --.0006*tolerance
                            or abs(yt-yi) > ycheck) then -- .00052*tolerance)  then

             NULL;
          elsif ABS(y1) <= 50. and (abs(xt-xi) > xcheck -- .00032*tolerance
                                 or abs(yt-yi) > ycheck) then -- .0004*tolerance)  then  -- used tolerance = 2 meters hers
             NULL;
--              dbms_output.put_line('abs st - xi ' || abs(xt-xi) || ' xcheck ' || xcheck);
--               dbms_output.put_line('abs st - yi ' || abs(yt-yi) || ' xcheck ' || ycheck);
          else
            d := short_oracle_dist(xi,yi,xt,yt);
--            dbms_output.put_line('DD' || round(d,8) || ' tol ' || tolerance);

--            dbms_output.put_line('Oracel d ' || round(sdo_geom.sdo_length(mdsys.sdo_geometry(2002,8265.,null,mdsys.sdo_elem_info_array(1,2,1),mdsys.sdo_ordinate_array(x3,y3,xi,yi)),0.05,'unit=meter'),8));
--dbms_output.put_line('Xt ' || xt || ' yt ' || yt || ' xi ' || xi || ' yi ' || yi);
-- Dont exit here because the other segment may project a shorter distance
            if d <> 0. and d <= tolerance then
--            dbms_output.put_line('RETURNING >> ' || try );
--            dbms_output.put_line('X1 ' || x1 || ' y1 ' || y1 || ' x2 ' || x2 || ' y2 ' || y2);
--            dbms_output.put_line('X3 ' || x3 || ' y3 ' || y3 || ' x4 ' || x4 || ' y4 ' || y4);
             vertex := try;
             distance :=d;
             lastd := d;
             if s > 1.0 then
              s := 1.0;
             elsif s < 0.0 then
              s := 0.0;
             end if;
             kount := kount+1;
             if (pflag =1 or pflag = 2) and pflag <> try then
                RETURN 0.5;
             end if;
            end if;
          end if;

        END IF;
        END LOOP;

      -- drop perpendicular to x3,y3 _> x4,y4 from x1,y1 or x2,y2


       len_sq := dx43*dx43 + dy43*dy43;
       FOR ii in 1..2 LOOP
--        dbms_output.put_line('u' || u);
       if ii = 1 then
          u := -dx31 * dx43 -dy31*dy43;
          try := 1.;  -- try x1,y1
          xt := x1;
          yt := y1;
        else
          u := (x2-x3) * dx43 + (y2-y3)*dy43;
--           dbms_output.put_line('uu' || u);
          try := 2.;   -- try x2,y2
          xt := x2;
          yt := y2;
        end if;
--           dbms_output.put_line('Uu ' || u || ' lensq ' || len_sq);
        IF u >= 0 and u <= len_sq THEN

        u := u / len_sq;
        xi := X3 + u * dx43;
        yi := Y3 + u * dy43;
--        if ABS(y1) > 50. then  -- used tolerance = 2 meters hers
--          xcheck   := 0.000064; --0.0000016 * tolerance/0.05;
--          ycheck   := 0.00002; --0.0000005 * tolerance/0.05;
--        else
 --         xcheck   := 0.00003; --0.00000075 * tolerance/0.05;
--          ycheck   := 0.000026; --0.00000065 * tolerance/0.05;
--        end if;


        if ABS(y1) > 50. and (abs(xt-xi) > xcheck
                          or abs(yt-yi) > ycheck) then
           NULL;
        elsif ABS(y1) <= 50. and (abs(xt-xi) > xcheck
                               or abs(yt-yi) > ycheck)  then  -- used tolerance = 2 meters hers
           NULL;

        else
--dbms_output.put_line('oracel d ' || sdo_geom.sdo_length(mdsys.sdo_geometry(2002,8265.,NULL,mdsys.sdo_elem_info_array(1,2,1),mdsys.sdo_ordinate_array(xt,yt,xi,yi)),0.05,'unit=meter'));

-- dbms_output.put_line('xt ' || xt || ' yt ' || yt || ' xi ' || xi || ' yi ' || yi);
          d := short_oracle_dist(xi,yi,xt,yt);
--         dbms_output.put_line('DDD' || round(d,15) || ' tol ' || tolerance || ' lastd ' || lastd);
          if d <> 0. and d <= lastd then
           lastd := d;
           if s > 1.0 then
              s := 1.0;
           elsif s < 0.0 then
              s := 0.0;
           end if;
--           dbms_output.put_line('retURNING >> ' || try || ' tol ' || tolerance);
           vertex := try;
           distance :=d;
           kount := kount+1;
          end if;
        End if;

       End If;
       END LOOP;
-- If we found a vertex above that projects onto another segment return it.

       if vertex <> 0.0 then
           if kount = 2 and pflag =0 then  -- we found two intersections
             flag_2way :=22;
           end if;
           RETURN 0.5;
        end if;
-- Check if the lines are nearly butting (close) but not intersecting

       if abs(dx31) > xcheck or abs(dy31) > ycheck  then
          NULL;
       else
          distance := short_oracle_dist(x1,y1,x3,y3);
          vertex := -13.;
       end if;
       if abs(x4-x1) > xcheck or abs(y4-y1) > ycheck  then
          NULL;
       else
         dist := short_oracle_dist(x1,y1,x4,y4);
         if dist < distance then
           distance := dist;
           vertex := -14.;
         end if;
       end if;
       if abs(x4-x2) > xcheck or abs(y4-y2) > ycheck  then
          NULL;
       else
          dist := short_oracle_dist(x2,y2,x4,y4);
          if dist < distance then
            distance := dist;
            vertex := -24.;
          end if;
       end if;
       if abs(x3-x2) > xcheck or abs(y3-y2) > ycheck  then
          NULL;
       else
          dist := short_oracle_dist(x2,y2,x3,y3);
          if dist < distance then
            distance := dist;
            vertex := -23.;
          end if;
       end if;

-- Very close segments (within tolerance)
--dbms_output.put_line('returning -1 '  || ' distance ' || distance);
       if vertex <> 0 and distance < tolerance then
--          dbms_output.put_line('returning ' || vertex || ' distance ' || distance);
         RETURN vertex;
       end if;
        RETURN -1.;  -- _1 means they don't intersect and are not even close at tolerance

      End if;


   END IF;

-- Check if the two lines are coincident

   if x1 = x3 and y1 = y3 and x2 = x4 and y2 = y4 then
       RETURN -3.;
   elsif x1 = x4 and y1 = y4 and x2 = x3 and y2 = y3 then
       RETURN -4.;
   end if;
-- Lines are parallel (but not coincident)
   RETURN -2.;

END LINE_INTERSECT;
--
FUNCTION MEASURE_ANGLE (X1  NUMBER,  Y1 NUMBER,
                           XC  NUMBER,  YC  NUMBER,
                           X2  NUMBER,  Y2 NUMBER)
               RETURN          NUMBER Deterministic IS
/*
--##############################################################################
--Program Name: mk_measure_angle
--Author: Sidey Timmins
--Creation Date: 01/04/2007
--Usage:
  -- This PL/SQL program has 6 parameters:
  --
  --   REQUIRED Parameters:
  --            X1,Y1:  end point of one line
  --            XC,YC:  the common vertex, x,y
  --            X2,Y2:  end point of 2nd line

--Purpose:   -- Calculates the angle between two vectors. The result is always
--              between zero and 180. Note the direction of the vectors on
--              the drawing and note that the acute angle is returned (never the
--              the obtuse angle).
--                                       + (X2,Y2)
--                                      ^
--                                     /
--                                    /
--                  -----------------/-------------->+ (X1,Y1)
--                                   (XC,YC)
-- Method:     Calculates dot product of 2 vectors and converts radians to degrees.

-- Accuracy:  The maximum error in the range - two pi to two pi < 1.E-38

-- Reference: http://en.wikipedia.org/wiki/Dot_product
--
--Dependencies: GZ_MATH.new_arccos
--##############################################################################
*/

  cf           CONSTANT NUMBER    := 57.29577951308232087679815481410517033240;

  output         NUMBER;
  denom          NUMBER;
  X1C            NUMBER          := X1-XC;
  X2C            NUMBER          := X2-XC;
  Y1C            NUMBER          := Y1-YC;
  Y2C            NUMBER          := Y2-YC;

   BEGIN

    denom := SQRT( X1C*X1C + Y1C*Y1C ) * SQRT( X2C*X2C + Y2C*Y2C );

    if denom = 0.0 then
      TRACK_APP_ERROR('MEASURE_ANGLE: denom is zero in measure_angle ');
    end if;

    output :=  GZ_MATH.new_arccos( (X1C*X2C + Y1C*Y2C) /denom) *cf;

    RETURN output;

END MEASURE_ANGLE;
--
FUNCTION  GET_WITHIN_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0) RETURN MDSYS.SDO_LIST_TYPE AS
/*
--##############################################################################
--Program Name: get_within_mbr
--Author: Sidey Timmins
--Creation Date: 09/14/11
--Updated:
--Usage:
  -- This PL/SQL function has 7 parameters:
  --             XYs:             the input array of xy coordinates
  --             pLB, pUB  :      a range [pLB,pUB] to use from the ordinate array.
  --                              pLB must be odd and pUB must be even.
               xLL,yLL,xUR,yUR    the MBR to check XYs against (Lower left, Upper right).
--
--                ---------+ (xUR,yUR)
--                |        |
--                |        |
--      (xLL,yLL) +--------+
--
--   Ouput: returns a list of pairs (ranges of the XY ordinates) - within the MBR

--Purpose: Checks a geometry to find just the portions of an edge within the
--         user specified MBR and returns a list of pairs (start,stop):
--         ordinate ranges of the geometry's Xys within the input MBR.
--Dependencies: None

--##############################################################################
*/

  List_within       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  trap              BOOLEAN := FALSE;
  i                 PLS_INTEGER;
  next              PLS_INTEGER :=0;
  low               PLS_INTEGER :=1;
  hi                PLS_INTEGER;
  LB                PLS_INTEGER :=pLB;
  n                 PLS_INTEGER :=pUB;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;

BEGIN

   if LB <= 0 then    LB := 1;  end if;

   if n <= 0 then  n := Xys.count;  end if;
   n := TRUNC((n-LB+1)/2) -1;
   List_within.extend(10);

   x2 := Xys(LB);
   y2 := XYs(LB+1);
   i := LB;

   if n = 1 then trap := TRUE; end if;

   For ii in 1..n+1 Loop  -- extra loop is to store values when trap is
   --                         set on the n'th loop
      if ii <= n then
      i := i + 2;
      x1 := x2;
      y1 := y2;
      x2 := Xys(i);
      y2 := XYs(i+1);
      end if;
-- This is the correct way to formulate this test (see any graphics book
 -- for checking whether a line segment overlaps a window)

      If (x1 < xLL and x2 < xLL) or (x1 > xUR and x2 > xUR) or
          (y1 < yLL and y2 < yLL) or (y1 > yUR and y2 > yUR) or
          (trap and ii>=n) then

-- Its completely outside the window, so if we already have a low, hi range
-- we store that range.

          if trap then
             if ii = n then hi := i+1; end if;
             trap := FALSE;
             next := next + 1;
             if next > List_within.count then  List_within.extend(10);  end if;
             List_within(next) := low;
             next := next+1;
             List_within(next) := hi;
          end if;
      else
-- its partially or wholly within so set low

        if NOT trap then
          trap := TRUE;
          low := i-2;
        end if;
        hi := i+1;    -- and continually set hi
      end if;

   End Loop;

   List_within.trim(List_within.count-next);
-- Return a list of pairs of ordinates within the MBR.
   RETURN List_within;

End GET_WITHIN_MBR;
--
PROCEDURE     SET_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0) AS
/*
--##############################################################################
--Program Name: set_mbr
--Author: Sidey Timmins
--Creation Date: 7/25/08
--Updated: 11/04/2010 To go through the array in different directions
--Usage:
  -- This PL/SQL function has 7 parameters:
  --             XYs:      the input array of xy coordinates
  --             pLB, pUB  :      elements caller must specify to start and stop
  --                              at in the ordinate array
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

-- for less than 400 coordinates, brute force seems fastest
     if inc < 20 then
        inc := 2;
     elsif inc > 40 then
        inc := inc * 2;
     end if;
     If inc = 2 then  loops := 1; end if;

     For jj in 1..loops Loop
       m := TRUNC((UB-LB+1)/inc);
       inc := inc -1;
       if loops = 2 and jj = 1 then
         ii := UB + inc-1;
         inc := -inc;
       else
         ii := pLB  - inc;
       end if;
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
       inc := 2;   -- now we check every coordinate pair
--   dbms_output.put_line('LL ' || xLL || ' y ' || yLL || ' UR ' || xUR || ' y ' || yUR);
     End Loop;
  END IF;

END SET_MBR;
--
PROCEDURE SET_MANY_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                      MBRs IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
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
--Updated:     02/26/2013 Fixed situation where 2 MBRs share a vertex and
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
--                           ____________
--                       ____|___________|____     Example: House shaped polygon
--                       |   |           |   |              might have these 5 MBRs
--                       |___|___________|___|              placed around the perimeter
--                       |________|__________|
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
     accum_angle     NUMBER :=0.0;
     angle_turned    NUMBER;
     old_bearing     NUMBER;
     pdim            PLS_INTEGER := 8;
     LB              PLS_INTEGER := pLB;
     UB              PLS_INTEGER := pUB;
     found           PLS_INTEGER;
     InUB            PLS_INTEGER;
     iend            PLS_INTEGER;
     LBB             PLS_INTEGER;
     UBB             PLS_INTEGER := 0;
     m               PLS_INTEGER;
     mm              PLS_INTEGER;
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
     may_overlap     BOOLEAN := TRUE;
BEGIN

   if UB = 0 then UB := XYs.count; end if;

   InUB := UB;

   if LB = 1 then no_holes := TRUNC(Info_Array.count/3) -1; end if;

    If no_holes > 0 then
       next := 4;

       for ii in 1..no_holes loop
         if Info_array(ii*3+1) > UB then
           dbms_output.put_line('UB ' || UB || ' info ' || (Info_array(ii*3+1)));
           TRACK_APP_ERROR('SET_MANY_MBR: INFO array does not match Xys');
         end if;
       end loop;
       UB := Info_Array(4) -1;
    End if;
    m := TRUNC(sqrt((InUB-LB+1)/2));
--        dbms_output.put_line('m ' || m || 'inub ' || inub || ' ' ||lb);
    if m > 60 then
       m := m * TRUNC(m/20) ;  -- m := m*8;    -- This is new  08/18/2011
    end if;
    if m <= 0 then  m := 1; end if;

-- To get MBRs rectangles touching we have to reuse the last vertex as the
-- next start vertex (hence -2).

    no_to_do := TRUNC((InUB-LB+1)/m);
--  dbms_output.put_line('noto do' || no_to_do || ' inub ' ||inub || ' lb ' || LB || ' m ' || m);
    if TRUNC(no_to_do/2)*2 <> no_to_do then no_to_do := no_to_do +1; end if;
--          dbms_output.put_line('NOW noto do' || no_to_do );

    if no_to_do <= 2 then
      m := 1;
    else
      m := TRUNC((InUB-LB+1)/(no_to_do -2));
      If (m* (no_to_do-2)) < (InUB-LB+1) then m := m + 1; End If;
    end if;
    no_to_do2 := TRUNC(no_to_do/2);

-- if we have 26/2 => 13 then we want at least 14 so 2*no_to_do2 >= no_to_do
    if MOD(no_to_do2,2) =1 then no_to_do2 := no_to_do2+1; end if;


    mm := m + no_holes;

    len_required := mm*pdim +pdim;
    If (MBRs.count - pbeg) < len_required then
      If len_required < 1000 then
        len_required := 1000;
      End if;

      MBRs.extend(len_required);
    End If;
    LBB := LB;
--    dbms_output.put_line('M ' || m || ' mm ' || mm || ' UB ' || UB);
--         dbms_output.put_line('M ' || m || ' holes ' || no_holes || ' no to do ' || no_to_do || ' count ' || Xys.count || 'inub ' || InUB);
    WHILE UBB <> UB LOOP --For ii in 1..mm Loop
        UBB := LBB + no_to_do-1;
        If UBB > UB then UBB := UB;  End if;
        ii := ii + 1;
        Set_MBR(XYs,xLL2,yLL2,xUR2,yUR2,LBB,UBB);
-- This is new below 08/18/2011

        if ii <> 1 and no_to_do > 20 and UBB-LBB > 20 then
          if ABS(xUR2-xLL2) > 2. * average_Xextent/ii or
             ABS(yUR2-yLL2) > 2. * average_Yextent/ii then
             UBB := LBB + no_to_do2-1;
             If UBB > UB then
               UBB := UB;
             End if;
             Set_MBR(XYs,xLL2,yLL2,xUR2,yUR2,LBB,UBB);
             mm := mm + 1;
--               dbms_output.put_line('II ' || ii || ' LBB ' || LBB || ' UBB ' || UBB || ' count ' ||Xys.count || ' UB ' || UB);
          end if;
        end if;
--          dbms_output.put_line('II ' || ii || ' LBB ' || LBB || ' UBB ' || UBB || ' count ' ||Xys.count || ' UB ' || UB);
--          dbms_output.put_line('II ' || ii || ' xLL ' || XLL2 || ' yLL ' || YLL2);
--          dbms_output.put_line('II ' || ii || ' xUR ' || XUR2 || ' yLL ' || YUR2);


        jj := ii*pdim+pbeg;
        if jj+pdim > MBRS.count then MBRs.extend(100); end if;
        MBRs(jj+1) := xLL2;
        MBRs(jj+2) := yLL2;
        MBRs(jj+3) := xUR2;
        MBRs(jj+4) := yUR2;
        MBRs(jj+5) := LBB;

-- It is very useful when skip is FALSE to find out if the segments themselves
-- have any potential to overlap.  We just want to know if the segments just
-- walk away from each other and never loop back. If they do so the angle
-- between the segments will be >= 90.

        found :=0;
        IF skip =0 or LBB < skip THEN

          xlast := Xys(LBB);
          ylast := Xys(LBB+1);
          jpos := LBB+1;
          may_overlap := FALSE;

-- Two MBRs can end and next one begin on a vertex. So check for this situation:
--
--          +-_-_-_-_-_-_-_-_
--                  Angle close to zero
--
          iend := TRUNC((UBB-LBB+1)/2);
          if UBB = Xys.count then
             iend := iend -1;
          end if;

          for kk in 2..iend loop

 -- Check do product which is in the range 1 to zero for
 --                                 angles 0 to 90.
 -- (when divided by the lengths of the vectors).
             jpos := jpos +1;
             dx1 := xlast- XYs(jpos);
             dx2 := XYs(jpos+2) -Xys(jpos);
             xlast := Xys(jpos);
             jpos := jpos+1;
             dy1 := ylast -XYs(jpos);
             dy2 := XYs(jpos+2) -Xys(jpos);
             ylast := Xys(jpos);

-- "Forget" to divide by lengths and range is positive to zero instead.
             if dx1*dx2 + dy1*dy2 > 0. then
                  may_overlap := TRUE;

                  found := jpos;
                  exit;
            end if;
          end loop;

-- Flag to say no potential for intersection within this MBR
          if NOT may_overlap then

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


-- Now adding 180 here reverses bearing to its real sense (from point 1 to point 0)
               accum_angle :=  accum_angle + angle_turned;
               if kk = 2 then
                 accum_angle := 0.;
               end if;
--               if accum_angle > 180. then
--                  accum_angle := 360.-accum_angle;
--               end if;
--  dbms_output.put_line('jpos ' || jpos ||' A ' ||   round(accum_angle,6)  || ' B ' || round(bearing,6));
             if ABS(accum_angle) >= 91. then
                   may_overlap := TRUE;
                   exit;
             end if;
             end loop;
             if NOT may_overlap then
               MBRs(jj+5) := -LBB;
--              dbms_output.put_line('NO overlap for  ' || jj);
             end if;
          elsif found < UBB-2 then
               found := 0;            -- set semaphore to ignore
          end if;
        END IF;

        average_Xextent := average_Xextent + ABS(XUR2-xLL2);
        average_Yextent := average_Yextent + ABS(YUR2-yLL2);
-- NEW   If we are at the end of a ring set a flag not to use the non existent
--       segment between the 2 rings
        if UBB = UB then
          MBRs(jj+6) := UBB+.01;  -- This fraction is transparent
        else
          MBRs(jj+6) := UBB;
        end if;
        if found <> 0 then
           MBRs(jj+6) := -MBRs(jj+6);   -- use semaphore that  that there is a V a vertex between MBRs
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
    MBRs(1) := xLL;        -- Overall MBR of this data set
    MBRs(2) := yLL;
    MBRs(3) := xUR;
    MBRs(4) := yUR;
    MBRS(5) := m;          -- the number of MBRs for this data set
    if pbeg = 0 then
       MBRS(7) := m;
       MBRS(8) := 0.0;
    else
       MBRS(7) :=  MBRS(7) + m;  -- Total Number of mbrs
    end if;

    MBRs.trim(MBRs.count-(jj+pdim));

--    dbms_output.put_line('m ' || m || ' mm ' || mm);
-- Work out the touching relationships between MBRs and record them
-- We already know that MBR n touches MBR n-1 and n+1.
-- Place pointers to the ones that touch in positions 7 through 8 and then an
-- individual list at the end

    pos := MBRs.count;
    MBRS(6+pbeg) := pos+1;   -- the start of the overlapping MBRs list
    MBRs.extend(1000);

    For ii in 1..m Loop
      jj:= ii*pdim +pbeg;
      xLL2 := MBRs(jj+1);
      yLL2 := MBRs(jj+2);
      xUR2 := MBRs(jj+3);
      yUR2 := MBRs(jj+4);
      MBRs(jj+7) := 0;  --start ptr: one zero means no overlapping
      MBRs(jj+8) := 0;  --end ptr: ditto
      for kk in ii+1..m Loop
        jk := kk*pdim+pbeg;

-- Test for Overlap

        if  (MBRs(jk+3)  < xLL2) or (MBRs(jk+1)  > xUR2) or
            (MBRs(jk+4)  < yLL2) or (MBRs(jk+2)  > yUR2) then
          NULL;   -- none
        elsif skip = 0 or MBRs(jk+5) >= skip then

-- Figure out if the coordinates really touch at a tolerance.
-- The MBRs touch either in the X direction or the Y.

-- First check the X's
--            _______ _______
--            |      |       |     sideways overlapping
--            ----------------
--

            may_overlap := FALSE;
            LBB := ABS(MBRs(jk+5));
            UBB := ABS(MBRs(jk+6));
            jpos := LBB+2;
--            dbms_output.put_line('checkin ' || ii || ' with ' || kk ||' mbr ' ||MBRs(jk+6)  || ' xys ' || (xys(jpos)+tolerance));
            if kk=ii+1 and MBRs(jj+6) < 0 then
              MBRs(jj+6) := ABS(MBRs(jj+6));

              may_overlap := TRUE;
            elsif  (MBRs(jk+3)  = xLL2) then
            While jpos < UBB loop
               if Xys(jpos)+tolerance > xLL2 then
                  may_overlap := TRUE;
                  exit;
               end if;
               jpos := jpos +2;
            end loop;
          elsif  (MBRs(jk+1)  = xUR2) then
            While jpos < UBB loop
               if Xys(jpos)-tolerance < xUR2 then
                  may_overlap := TRUE;
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
        elsif (MBRs(jk+4)  = yLL2)  then

            jpos := LBB+1;
            While jpos < UBB-1 loop
               if Xys(jpos)+tolerance > yLL2  then
                  may_overlap := TRUE;
                  exit;
               end if;
               jpos := jpos +2;
            end loop;
            elsif (MBRs(jk+4)  = yLL2) or (MBRs(jk+2)  = yUR2) then

            jpos := LBB+1;
            While jpos < UBB-1 loop
               if Xys(jpos)-tolerance < yUR2 then
                  may_overlap := TRUE;
                  exit;
               end if;
               jpos := jpos +2;
            end loop;
        else
-- If they completely overlap then the checking above is unnecessary.
          may_overlap := TRUE;
        end if;

        if may_overlap then

          pos := pos + 1;
          if MBRs(jj+7) = 0 then
            MBRs(jj+7) := pos;  --start pointer
          end if;
          MBRs(jj+8) := pos;   -- end pointer
          if pos > MBRs.count then MBRs.extend(1000);  end if;
-- Store overlapping MBRs for the subject MBR
            MBRs(pos) := kk;
          end if;
        end if;
      end loop;
    End Loop;
    MBRs.trim(MBRS.count-pos);

--        for ii in 1..Mbrs.count loop
--          dbms_output.put_line('ii ' || ii || ' ' ||mbrs(ii));
--       end loop;
-- This just prints out when our list exceeded our available space
--        for ii in 1..m loop
--             jj:= ii*pdim;
--           if mbrs(jj+pdim) <> TRUNC(mbrs(jj+pdim)) then
--           for j in 1..pdim loop
--           dbms_output.put_line('ii ' || j || ' MBR ' || mbrs(jj+j));
--           end loop;
--           end if;
--        end loop;

END SET_MANY_MBR;
--
FUNCTION GET_DIST(Geom IN MDSYS.SDO_GEOMETRY,smallest VARCHAR2 default 'YES',coords VARCHAR2 default 'YES') RETURN MDSYS.SDO_LIST_TYPE  AS

-- Function to return either 1) the shortest distance and its location
--                        or 2) all the intervertex distances.
-- example:
--SQL> select get_ds(sdogeometry) from z653ls_clip_face where face_id=17430;
--     returns the position, distance, pair of vertex coordinates as:
--GET_DS(SDOGEOMETRY)
--------------------------------------------------------------------------------
--SDO_LIST_TYPE(4, .0426, -123.86331, 46.951009, -123.86331, 46.9510089)

  Distances         MDSYS.SDO_LIST_TYPE:= MDSYS.SDO_LIST_TYPE();
  Xys               MDSYS.SDO_ORDINATE_ARRAY:= Geom.SDO_ORDINATES;
  N                 PLS_INTEGER;
  x1                NUMBER;
  y1                NUMBER;
  x2                NUMBER;
  y2                NUMBER;
  xx1               NUMBER;
  yy1               NUMBER;
  xx2               NUMBER;
  yy2               NUMBER;
  gcd               NUMBER := 0.0;
  seg12             NUMBER;
  s12               NUMBER;
  az0               NUMBER;
  az1               NUMBER;
  m12               NUMBER;
  small             NUMBER := 1.E20;
  where_is          NUMBER := 0;
BEGIN
  n := Xys.count/2;
  Distances.extend(n);
  x2 := Xys(1);
  y2 := Xys(2);
  For ii in 2..n Loop
    x1 := x2;
    y1 := y2;
    x2 := Xys(ii*2-1);
    y2 := Xys(ii*2);
--    seg12 := geodesic.inverse(y1,x1,y2,x2,s12,az0,az1,m12);
--    Distances(ii) := s12;
    Distances(ii) := round(distance_fcn(x1,y1,x2,y2),4);
    if distances(ii) < small then
       small := distances(ii);
       where_is := ii-1.;
       xx1 := x1;
       yy1 := y1;
       xx2 := x2;
       yy2 := y2;
--       dbms_output.put_line('ii ' || (ii-1) || ' ' || round(small,4));
    end if;
    gcd := gcd + Distances(ii);
  End Loop;
  Distances(1) := gcd;
  if smallest = 'YES' then
    Distances(1) := where_is;
    Distances(2) := small;
    if coords = 'YES' then
      if Distances.count < 6 then
        Distances.extend(4);
      end if;
      Distances(3) := xx1;
      Distances(4) := yy1;
      Distances(5) := xx2;
      Distances(6) := yy2;
      Distances.trim(Distances.count-6);
    else
      Distances.trim(Distances.count-2);
    end if;
  end if;
  RETURN Distances;
END GET_DIST;
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
 #                               will have the 2nd one removed.
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

    If tolerance <= 0. then tolerance := 0.05; end if;


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
           distance := distance_fcn(xnew,ynew,xlast,ylast,SRID);
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
--dbms_output.put_line('usual case ' || k);
       End If;

    End Loop;
    END LOOP;

    k := k + 1;
--    dbms_output.put_line('exiting with ' || k);
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
PROCEDURE REVERSE_ORDINATES(XY IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                             LB  PLS_INTEGER default 1,
                             InUB  PLS_INTEGER default 0)  AS
/*******************************************************************************
--Program Name: mk_reverse_ordinates
--Author: Sidey Timmins
--Updated: 01/09/12 To catch bad LB or UB and raise application error
--Creation Date: 03/27/2007

--Usage:
  --   REQUIRED Parameters:
  --         INPUT/OUTPUT
  --         XY              - SDO_ORDINATE_ARRAY to be reversed;
  --         LB              - place at which to begin reversing (lower limit
  --                            - must be odd)
  --         UB              - place at which to end reversing (upper limit -
  --                            - must be even)
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
   UB        PLS_INTEGER := inUB;

BEGIN
   If UB <= 0 then     UB := XY.count;     End If;

  If LB < 1 or UB > Xy.count or MOD(LB,2) <> 1 or MOD(UB,2) <> 0 then
    TRACK_APP_ERROR('REVERSE_ORDINATES: Bad LB '||LB || ' or UB ' || UB);
  End if;

  n := UB - LB + 1;

  If n <= 2 then
      NULL;
  Else

 -- Calculate the start n1, the end n2, and the swing point m

     n1 := trunc(LB/2)+1;
     m := UB + (n1 -1)*2;
     n2 := trunc(trunc(n/2)/2) + n1 -1;

-- Go throught the xyarray reversing coordinates, two pairs at a time
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
PROCEDURE TRACK_APP_ERROR(msg VARCHAR2,p_table_name VARCHAR2 default NULL,p_log_type VARCHAR2 default NULL,geom MDSYS.SDO_GEOMETRY default NULL) AS

-- These are the parameters we could pass
--     p_module         IN VARCHAR2,   'TOPOFIX'
--     p_jobrun         IN VARCHAR2,   'Z634CL'
--     p_process        IN VARCHAR2,   procedure/function name
--     p_table_name     IN VARCHAR2 DEFAULT NULL,   - table if any
--     p_step           IN VARCHAR2 DEFAULT NULL,  - what you are doing
--     p_start_time     IN TIMESTAMP DEFAULT NULL,
--     p_end_time       IN TIMESTAMP DEFAULT NULL,
--     p_release        IN VARCHAR2 DEFAULT NULL,  -- I dont know what this is
--     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
--     p_deploy         IN VARCHAR2 DEFAULT NULL,  -- ?Also
--     p_error_msg      IN VARCHAR2 DEFAULT NULL,
--     p_sdo_dump       IN SDO_GEOMETRY DEFAULT NULL

  p_process      VARCHAR2(30);
  p_topo         VARCHAR2(30);
  p_step         VARCHAR2(25);
  pos            PLS_INTEGER :=1;
  pos2           PLS_INTEGER:=1;
  len            PLS_INTEGER;
  error_msg      VARCHAR2(4000) := msg;
  prmsg          VARCHAR2(4000);

  BEGIN

  dbms_output.put_line(' ');

  prmsg := REPLACE(SUBSTR(msg,1,4000),':',' ');

  For ii in 1..TRUNC((Length(prmsg)+99)/100) loop
    dbms_output.put_line(SUBSTR(prmsg,pos,pos+99));
    pos := pos + 100;
  End Loop;

  pos := INSTR(msg,':');

-- For SUPER and ZONE enable the edge_id to be pushed into the step
  if pos = 1 then
   pos := INSTR(msg,':',2);
   len := pos-2;
   if len > 25 then len :=25; end if;
   p_step := SUBSTR(msg,2,len);
   error_msg := SUBSTR(msg,pos+1,LENGTH(msg)-pos);
   pos := INSTR(error_msg,':');
  end if;
  if pos = 0 then
    pos := 15;
    if pos > LENGTH(error_msg) then pos := LENGTH(error_msg); end if;
  end if;
  if pos > 30 then
     pos := 30;
  else
     pos2 := pos;
  end if;
  p_process := SUBSTR(error_msg,1,pos-1);
  error_msg := SUBSTR(error_msg,pos2+1,LENGTH(error_msg)-pos2);

  pos := INSTR(p_table_name,'_') -1;
  if pos <= 0  then pos := 6; end if;
  p_topo := SUBSTR(p_table_name,1,pos);



  if p_log_type is NOT NULL then
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topo,p_process,p_table_name,p_step,
                                          NULL,NULL,NULL,NULL,NULL,error_msg
                                          || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE,geom);
  end if;
  RAISE_APPLICATION_ERROR(-20001,msg|| Chr(10) || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
END;
--
PROCEDURE TRACK_APP(pmsg VARCHAR2,p_table_name VARCHAR2 default NULL,p_log_type VARCHAR2,geom MDSYS.SDO_GEOMETRY default NULL) AS

-- These are the parameters we could pass
--     p_module         IN VARCHAR2,   'TOPOFIX'
--     p_jobrun         IN VARCHAR2,   'Z634CL'
--     p_process        IN VARCHAR2,   procedure/function name
--     p_table_name     IN VARCHAR2 DEFAULT NULL,   - table if any
--     p_step           IN VARCHAR2 DEFAULT NULL,  - what you are doing
--     p_start_time     IN TIMESTAMP DEFAULT NULL,
--     p_end_time       IN TIMESTAMP DEFAULT NULL,
--     p_release        IN VARCHAR2 DEFAULT NULL,  -- I dont know what this is
--     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
--     p_deploy         IN VARCHAR2 DEFAULT NULL,  -- ?Also
--     p_error_msg      IN VARCHAR2 DEFAULT NULL,
--     p_sdo_dump       IN SDO_GEOMETRY DEFAULT NULL

 -- msg            VARCHAR2(100);
  p_process      VARCHAR2(30);
  p_topo         VARCHAR2(30);
  p_step         VARCHAR2(25);
  pos            PLS_INTEGER :=1;
  pos2           PLS_INTEGER :=1;
  len            PLS_INTEGER;
  error_msg      VARCHAR2(4000) := pmsg;
  msg            VARCHAR2(4000);

  BEGIN

  dbms_output.put_line(chr(10));

  msg := REPLACE(SUBSTR(pmsg,1,4000),':',' ');
  For ii in 1..TRUNC((Length(msg)+99)/100) loop
    dbms_output.put_line(SUBSTR(msg,pos,pos+99));
    pos := pos + 100;
  End Loop;


  if p_table_name is NOT NULL then
    pos := INSTR(pmsg,':');

-- For SUPER and ZONE enable the edge_id to be pushed into the step
    if pos = 1 then
     pos := INSTR(pmsg,':',2);
     len := pos-2;
     if len > 25 then len :=25; end if;
     p_step := SUBSTR(pmsg,2,len);
     error_msg := SUBSTR(pmsg,pos+1,LENGTH(pmsg)-pos);
     pos := INSTR(error_msg,':');
    end if;

    if pos <= 0 then
      pos := 15;
      if pos > LENGTH(error_msg) then pos := LENGTH(error_msg); end if;
    end if;

    if pos > 30 then
       pos := 30;
    else
     pos2 := pos;
    end if;
    p_process := SUBSTR(error_msg,1,pos-1);
    error_msg := SUBSTR(error_msg,pos2+1,LENGTH(error_msg)-pos2);

    pos := INSTR(p_table_name,'_') -1;
    if pos <= 0 then pos := 6; end if;
    p_topo := SUBSTR(p_table_name,1,pos);

-- write to a tracking table

    GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topo,p_process,p_table_name,p_step,
                                            NULL,NULL,NULL,NULL,NULL,error_msg,geom);
  end if;

END;
--
--
FUNCTION GET_XY(Geom MDSYS.SDO_GEOMETRY, pvtx PLS_INTEGER) RETURN MDSYS.SDO_GEOMETRY AS

  XYS    MDSYS.SDO_ORDINATE_ARRAY := Geom.sdo_ordinates;
  x      NUMBER;
  y      NUMBER;
  vtx    PLS_INTEGER := pvtx*2;
BEGIN

  if vtx > XYs.count then
     RETURN NULL;
  end if;
  x := Xys(vtx-1);
  y := Xys(vtx);
  RETURN MDSYS.SDO_GEOMETRY(2001,geom.sdo_srid,mdsys.sdo_point_type(x,y,NULL),NULL,NULL);
END;
--
FUNCTION GET_SEG(Geom MDSYS.SDO_GEOMETRY, pvtx PLS_INTEGER) RETURN MDSYS.SDO_GEOMETRY AS

  XYS    MDSYS.SDO_ORDINATE_ARRAY := Geom.sdo_ordinates;
  x1      NUMBER;
  y1      NUMBER;
  x2      NUMBER;
  y2      NUMBER;
  vtx    PLS_INTEGER := pvtx*2;
BEGIN

  if vtx+2 > XYs.count then
     RETURN NULL;
  end if;
  x1 := Xys(vtx-1);
  y1 := Xys(vtx);
  x2 := Xys(vtx+1);
  y2 := Xys(vtx+2);
  RETURN MDSYS.SDO_GEOMETRY(2002,geom.sdo_srid,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),MDSYS.SDO_ORDINATE_ARRAY(x1,y1,x2,y2));
END;
--
FUNCTION CIRCULATE_COORDINATES(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,shift PLS_INTEGER,pstart PLS_INTEGER default 1, pend PLS_INTEGER default 0) RETURN MDSYS.SDO_ORDINATE_ARRAY AS

-- Updated: Oct  27,2011 to handle multiple rings
-- Note shift is in vertices and the start and end are ordinate positions so pstart
-- is always odd and pend always even.
--
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
    istart          PLS_INTEGER := pstart;
    iend            PLS_INTEGER := pend;

BEGIN

-- Handle rings
  if istart < 0 then istart := 1; end if;


  if istart <> 1 or iend <> Xys.count then
     if iend = 0 or iend > Xys.count then iend := Xys.count; end if;
     n := TRUNC((iend-istart+1)/2);
  end if;

  if MOD(istart,2) <> 1 or MOD(iend,2) <> 0 then
    TRACK_APP_ERROR('CIRCULATE_COORDINATES: Bad istart '|| istart || ' or iend ' || iend);
  End if;

  if ishift > n-1 or ishift < 0 then
    ishift := MOD(ishift,n-1);  -- handle relative backup too.
  end if;

--  dbms_output.put_line('istart ' || istart || ' iend ' || iend || ' n ' || n);
  if ishift <> 0 then
    ishift := ishift*2;
    For ii in 1..n loop
       jj := ii*2+istart-1;
       if jj + ishift > iend then
          ishift := -(jj - istart -3);
--          dbms_output.put_line('IS ' || ishift);
       elsif jj+ishift <= (istart-1) then
          ishift := iend -jj +ishift;
--          dbms_output.put_line('is ' || ishift);
       end if;
--       dbms_output.put_line('ii ' || ii || ' jj ' || jj || ' to ' || (jj-1+ishift) || ' ishift ' || ishift);
       New_xys(jj-1) := Xys(jj-1+ishift);
       New_xys(jj) := Xys(jj+ishift);

    End loop;
  end if;
  RETURN New_Xys;


END Circulate_Coordinates;
--
FUNCTION TABLE_EXISTS ( pInTable  IN VARCHAR2, pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN AS

   /**Taken from the cartodb
    Author: Nick Padfield
    Creation Date: 9/6/2006
   **/
   -- Updated to not place NVL in SQL statement

   InTable       VARCHAR2(30)     := UPPER(pInTable);
   InSchema      VARCHAR2(30)     := UPPER(NVL(pInSchema,USER)); --UPPER(pInSchema);
   RecordCount   NUMBER(22)       := 0.;
   sql_stmt      VARCHAR2(4000);
   BEGIN
      ------------------------------------------------------
      -- Check to see if the table exists!
      sql_stmt := 'SELECT COUNT(1) FROM all_objects WHERE object_type = ''TABLE'' AND owner = ''' || InSchema || ''' AND object_name = ''' || InTable || '''';
      EXECUTE IMMEDIATE sql_stmt INTO RecordCount;
      ------------------------------------------------------
      -- If table exists, return TRUE, if not, return FALSE.
      RETURN (RecordCount > 0.);

   END TABLE_EXISTS;
--
PROCEDURE TRY_DISTANCE_FCN AS

deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
geom MDSYS.SDO_GEOMETRY;
   x1  number;
   y1  number;
   x2  number;
   y2  number;
  s   number;
   az1 number;
   az2 number;
   m12 number;
   s12 number;
   gcd_K number;
   gcd_O number;
   gcd_fast number;
   delta  number;
   difs   number :=0.0;
   time1  date;
BEGIN

-- time1 := current_timestamp;
-- dbms_random.initialize(1286);
dbms_output.put_line('To run this test the GEODESIC package must be available');
dbms_output.put_line('and the code must be compiled');
    delta := 0.04;
    x1 := -100.0;
    for ii in 0..7200 loop -- 0..7200 loop --0..7200 loop
     x2 := -100.+ delta; --dbms_random.value *delta;
     y1 := ii*0.01;
     y2 := y1 + delta;   --*0.66666666; -- dbms_random.value *delta;

      gcd_fast := distance_fcn(x1,y1,x2,y2);
--      dbms_output.put_line(' Gcd ' || round(gcd_fast,6));
--      s := geodesic.inverse(y1,x1,y2,x2,gcd_K,az1,az2,m12);
 --     geom := MDSYS.SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),
 --                    MDSYS.sdo_ordinate_Array(x1,y1,x2,y2));
 --     gcd_O := sdo_geom.sdo_length(geom,0.05,'unit=meter');
      if abs(gcd_K - gcd_fast) > 0.0003 then -- 0.0004 then -- 0.00001 then
         difs := difs+1;
           dbms_output.put_line('CK ' || round(gcd_K,6) || ' gcd ' || round(gcd_fast,6) || ' x ' ||round(x1,6) || ' y ' || round(y1,6) || ' x ' || round(x2,6) || ' y ' || round(y2,6) || ' ii ' || ii);
      end if;
    end loop;
--       dbms_output.put_line((current_timestamp-time1));
    dbms_output.put_line(difs);
END TRY_DISTANCE_FCN;
--
PROCEDURE try_circulate_coordinates(shift PLS_INTEGER default 2) AS
-- Unit_test for circulate_coordinates (changes the start, end of a loop).
-- User may supply the (vertex) shift and then the printout shows the new
-- positions of the coordinates relative to original start which are
-- conveniently numbered  1,2,3,4,.. So a vertex that has coordinates (3,4) was
-- the second, for example.                         + (5,6)
--                                                 /
--                 + (1,2) ---------------------- + (3.4)

  Arr      MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(-1,-2,-3,-4,1,2,3,4,5,6,7,8,9,10,11,12,13,14);
  New_Arr  MDSYS.SDO_ORDINATE_ARRAY;
  n        PLS_INTEGER;
BEGIN

  New_arr := circulate_coordinates(Arr,shift,5,16);
  n := TRUNC(new_arr.count/2);
  for ii in 1..n loop
     dbms_output.put_line('vertex ' || ii || ' x ' || new_arr(ii*2-1) || ' y ' || New_arr(ii*2));
  end loop;
END try_circulate_coordinates;
--
PROCEDURE try_check_for_nearby_segments(ingeom MDSYS.SDO_GEOMETRY default NULL) as
  geometry        MDSYS.SDO_GEOMETRY;
  xys              MDSYS.sdo_ordinate_array;
  Info_array       MDSYS.SDo_ELEM_INFO_array := MDSYS.SDo_ELEM_INFO_array(1,2,1);
  result          MDSYS.SDO_LIST_TYPE;
  Start_end       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(1,7);
BEGIN

  geometry :=
      SDO_GEOMETRY (2002,
                    8265,
                    NULL,
                    SDO_ELEM_INFO_ARRAY (1, 2, 1),
                    SDO_ORDINATE_ARRAY (0,
                                        0,
                                        0,
                                        1,
                                        .5,
                                        1,
                                        -.5,
                                        1));

  Xys := geometry.sdo_ordinates;

  result := CHECK_GEOM_SELF_INTERSECT(Xys,Info_array,FALSE,Start_end,0.05,0.05,0.05);

  for ii in 1..result.count loop
    dbms_output.put_line('re ' || result(ii));
  end loop;
end;--
PROCEDURE try_check_for_self_intersect(ingeom MDSYS.SDO_GEOMETRY default NULL) as
  geometry        MDSYS.SDO_GEOMETRY;
  xys              MDSYS.sdo_ordinate_array;
  new_xys          MDSYS.sdo_ordinate_array;
  Info_array       MDSYS.SDo_ELEM_INFO_array;
  result          MDSYS.SDO_LIST_TYPE;
  Start_end       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  MBR          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  status           VARCHAR2(1000);
BEGIN

   xys := SDO_ORDINATE_ARRAY(-122.32118, 47.315167, -122.32339, 47.315169, -122.32801, 47.315165, -122.32758,
 47.310322, -122.322707, 47.309425, -122.327,47.3086,    -122.32439, 47.308752, -122.3241, 47.308776,
-122.32394, 47.308806, -122.32038, 47.310589,   -122.3212, 47.3128,
-122.3223, 47.313404, -122.32118,
47.315167, -122.3,47.31,-122.29,47.31,-122.29,47.32,-122.3,47.32,-122.3,47.31);
---122.32270403,
  geometry := mdsys.sdo_geometry(2007, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 1003,1,11,2003,1,37,1003,1),SDO_ORDINATE_ARRAY(-122.33,47.3,-122.32,47.3,-122.32,47.32,-122.33,47.32,-122.33,47.3,-122.32118, 47.315167, -122.32339, 47.315169, -122.32801, 47.315165, -122.32758,
 47.310322, -122.322704016, 47.309425, -122.327,47.3086,    -122.32439, 47.308752, -122.3241, 47.308776,
 -122.32394, 47.308806, -122.32038, 47.310589,   -122.3212, 47.3128,
 -122.3223, 47.313404, -122.32118,
 47.315167,-122.324,47.311, -122.323,47.311,-122.323,47.312,-122.324,47.312,-122.324,47.311));
--  geometry := SDO_GEOMETRY(2003, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 1003,1,27,1003,1),xys);

  geometry :=
      SDO_GEOMETRY (2002,
                    8265,
                    NULL,
                    SDO_ELEM_INFO_ARRAY (1, 2, 1),
                    SDO_ORDINATE_ARRAY (0,
                                        0,
                                        0,
                                        1,
                                        .5,
                                        1,
                                        -.5,
                                        1));

   geometry := SDO_GEOMETRY
(
   2002,
   8265,
   NULL,
   SDO_ELEM_INFO_ARRAY
   (
      1,
      2,
      1
   ),
   SDO_ORDINATE_ARRAY
   (
      -82.79344600000000298223312711343169212341,
      37.79923900000000003274180926382541656494,
      -82.7933620000000018990249373018741607666,
      37.79935600000000306408765027299523353577,
      -82.79284499999999979991116560995578765869,
      37.80010999999999654619387001730501651764,
      -82.79242000000000700765667716041207313538,
      37.80061599999999799592842464335262775421,
      -82.79187199999999791089067002758383750916,
      37.80135099999999681585904909297823905945,
      -82.79171800000000303043634630739688873291,
      37.80155700000000251748133450746536254883,
      -82.79167599999999538340489380061626434326,
      37.80162899999999837064024177379906177521,
      -82.79157899999999870033207116648554801941,
      37.80173899999999775900505483150482177734,
      -82.79144100000000605632521910592913627625,
      37.80194199999999682404450140893459320068,
      -82.79135800000000244835973717272281646729,
      37.80202400000000295676727546378970146179,
      -82.79131599999999480132828466594219207764,
      37.80209599999999880992618273012340068817,
      -82.79110799999999414922058349475264549255,
      37.8023540000000011218617146369069814682,
      -82.79110199999999508690962102264165878296,
      37.80237600000000242062014876864850521088,
      -82.79094200000000114414433483034372329712,
      37.8025459999999995375219441484659910202,
      -82.79088000000000135969457915052771568298,
      37.80264999999999986357579473406076431274,
      -82.79076200000000085310603026300668716431,
      37.80279900000000026238922146148979663849,
      -82.79074799999999356714397436007857322693,
      37.80283099999999762985680717974901199341,
      -82.79069300000000453110260423272848129272,
      37.8028860000000008767528925091028213501,
      -82.79067899999999724514054832980036735535,
      37.80292500000000188720150617882609367371,
      -82.79065799999999342162482207641005516052,
      37.80293600000000253658072324469685554504,
      -82.79044399999999370720615843310952186584,
      37.80321099999999745477907708846032619476,
      -82.79027000000000668933353153988718986511,
      37.80347400000000135378286358900368213654,
      -82.79012500000000329691829392686486244202,
      37.80372700000000207865014090202748775482,
      -82.7900489999999962265064823441207408905,
      37.80387000000000341515260515734553337097,
      -82.79004199999999968895281199365854263306,
      37.80390299999999825786289875395596027374,
      -82.79001399999999932788341538980603218079,
      37.80397399999999663577909814193844795227,
      -82.78997599999999579267750959843397140503,
      37.8040850000000006048139766789972782135,
      -82.78935300000000552245182916522026062012,
      37.80396999999999962938090902753174304962,
      -82.78624899999999797728378325700759887695,
      37.80339599999999933288563624955713748932,
      -82.78631300000000692307366989552974700928,
      37.80261200000000343379724654369056224823,
      -82.78633800000000064756022766232490539551,
      37.80199999999999960209606797434389591217,
      -82.78622400000000425279722549021244049072,
      37.80093600000000009231371222995221614838,
      -82.78643599999999480587575817480683326721,
      37.80023200000000116460796562023460865021,
      -82.78658900000000642194208921864628791809,
      37.79987400000000263844412984326481819153,
      -82.78675499999999942701833788305521011353,
      37.79929700000000281079337582923471927643,
      -82.78685600000000022191670723259449005127,
      37.79871099999999728424882050603628158569,
      -82.78767799999999965621100272983312606812,
      37.79820500000000293994162348099052906036,
      -82.78858999999999923602445051074028015137,
      37.79767300000000318505044560879468917847,
      -82.78892899999999599458533339202404022217,
      37.79705799999999982219378580339252948761,
      -82.7890830000000050858943723142147064209,
      37.79638299999999873080014367587864398956,
      -82.78918600000000083127815742045640945435,
      37.79601999999999861756805330514907836914,
      -82.78931799999999441297404700890183448792,
      37.79567999999999727833710494451224803925,
      -82.78929300000000068848748924210667610168,
      37.79530599999999651572579750791192054749,
      -82.78998500000000149157131090760231018066,
      37.79476400000000069212546804919838905334,
      -82.79014999999999702140485169366002082825,
      37.79403899999999794090399518609046936035,
      -82.79086700000000575982994632795453071594,
      37.79407299999999736428435426205396652222,
      -82.79108700000000453655957244336605072021,
      37.79415900000000050340531743131577968597,
      -82.79130899999999826377461431547999382019,
      37.79452599999999762303559691645205020905,
      -82.79142400000000634463503956794738769531,
      37.79448800000000119325704872608184814453,
      -82.79180399999999906412995187565684318542,
      37.79459899999999805686456966213881969452,
      -82.79204799999999409010342787951231002808,
      37.79392299999999949022821965627372264862,
      -82.79266800000000614545569987967610359192,
      37.79393699999999967076291795819997787476,
      -82.792501999999998929524736013263463974,
      37.79391700000000042791725718416273593903,
      -82.79318700000000319505488732829689979553,
      37.79399899999999945521267363801598548889,
      -82.79363200000000233558239415287971496582,
      37.79413000000000266709321294911205768585,
      -82.79398899999999628107616445049643516541,
      37.79432099999999650208337698131799697876,
      -82.79426200000000335421646013855934143066,
      37.79457800000000133877620100975036621094,
      -82.79441500000000075942807598039507865906,
      37.79481499999999982719600666314363479614,
      -82.79452299999999809188011568039655685425,
      37.79515200000000163527147378772497177124,
      -82.79460699999999917508830549195408821106,
      37.79556999999999788997229188680648803711,
      -82.79466999999999643478076905012130737305,
      37.79603900000000038517100620083510875702,
      -82.79467400000000054660631576552987098694,
      37.7964389999999994529389368835836648941,
      -82.7945590000000066766006057150661945343,
      37.79705799999999982219378580339252948761,
      -82.79442899999999383453541668131947517395,
      37.7974999999999994315658113919198513031,
      -82.79418400000000133331923279911279678345,
      37.79805999999999954752638586796820163727,
      -82.79379099999999880310497246682643890381,
      37.79876699999999800638761371374130249023,
      -82.79344600000000298223312711343169212341,
      37.79923900000000003274180926382541656494
   )
);



  Info_array := geometry.sdo_elem_info;
  Xys := geometry.sdo_ordinates;
--   for ii in 11..36 loop
--    dbms_output.put_line('XY ' || xys(ii));
--  end loop;
--  new_xys := circulate_coordinates(Xys,7,11,36);    -- shift was 6
--   for ii in 11..36 loop
--      xys(ii) := new_xys(ii-10);
--  end loop;
--  for ii in 11..36 loop
--    new_xys(ii-10) := new_xys(ii);
--    dbms_output.put_line('xy ' || new_xys(ii-10));
--  end loop;
--  new_xys.trim(new_xys.count-26);
--  info_array := SDO_ELEM_INFO_ARRAY(1, 1003,1);
  dbms_output.put_line('count ' || xys.count);

  start_end := mdsys.sdo_list_type(1,3,1,3);
  result := CHECK_GEOM_SELF_INTERSECT(Xys,Info_array,FALSE,Start_end,0.05,0.05,0.05);

  for ii in 1..result.count loop
    dbms_output.put_line('re ' || result(ii));
  end loop;
--  geometry := mdsys.sdo_geometry(2003, 8265, NULL, INFO_ARRAY,New_xys);

--  for ii in 1..26 loop
--    dbms_output.put_Line('ii ' || ii || ' xy ' || new_xys(ii));
--  end loop;

  status := sdo_geom.validate_geometry_with_context(geometry,0.05);
  dbms_output.put_line('status ' || status);
end;
procedure try_move AS

   new_geom   MDSYS.SDO_GEOMETRY;
   point_geom1  MDSYS.SDO_GEOMETRY;
   point_geom2  MDSYS.SDO_GEOMETRY;
   point_geom  MDSYS.SDO_GEOMETRY;
   lrsgeom     MDSYS.SDO_GEOMETRY;
   lrsgeom2     MDSYS.SDO_GEOMETRY;
   geom1      MDSYS.SDO_GEOMETRY;
   geom2      MDSYS.SDO_GEOMETRY;
--   xys        MDSYS.SDO_ordinate_array :=
--     MDSYS.SDO_ordinate_array(-77.219, 43.039846, -77.215847, 43.039863,
--                               -77.215847, 43.039864, -77.215839, 43.039844);
   xys        MDSYS.SDO_ordinate_array :=
             MDSYS.SDO_ordinate_array(-77.219, 43.039846, -77.215847, 43.0398638,
                              -77.215847, 43.0398631, -77.21581, 43.039844, -77.2158, 43.039847);
   same       BOOLEAN := FALSE;
   new_one    NUMBER;
   sql_stmt   VARCHAR2(4000);
   UFS                             NUMBER :=0.0;

   x1         NUMBER;
   y1         NUMBER;
   x2         NUMBER;
   y2         NUMBER;
   x3         NUMBER;
   y3         NUMBER;
   x4         NUMBER;
   y4         NUMBER;

   xt         NUMBER;
   yt         NUMBER;
   xi         NUMBER;
   yi         NUMBER;
   vertex     NUMBER;
   t          NUMBER;
   s          NUMBER;
   d          NUMBER;
   Od         NUMBER;
   new_x      NUMBER;
   new_y      NUMBER;
   new_x2      NUMBER;
   new_y2      NUMBER;
   best       NUMBER := 1000.;
   dist       NUMBER;
    tolerance   NUMBER :=0.1;

   flag       PLS_INTEGER :=0;

BEGIN
-- was 1,14 and 239,250
--  sql_stmt := 'SELECT geometry from GZCPB1.SDT99IN_EDGE$ where edge_id=:1';
-- sql_stmt := 'SELECT GZ_qa.get_xys(geometry,1,-9,1,10) from GZCPB1.SDT99IN_EDGE$ where edge_id=:1';
-- execute immediate sql_stmt into geom1 using 657425;
-- dbms_output.put_line('XYs ' || sdo_util.getnumvertices(geom1));
-- sql_stmt := 'SELECT GZ_qa.get_xys(geometry,1,-1,1,243) from GZCPB1.SDT99IN_EDGE$ where edge_id=:1';
-- execute immediate sql_stmt into geom2 using  657753;

--  sql_stmt := 'SELECT GZ_qa.get_xys(geometry,1,-1,1,2) from GZCPB3.B424ST_EDGE$ where edge_id=:1';
-- execute immediate sql_stmt into geom1 using  125;
--  sql_stmt := 'SELECT GZ_qa.get_xys(geometry,1,-1,1,1) from GZCPB3.B424ST_EDGE$ where edge_id=:1';
-- execute immediate sql_stmt into geom2 using  127;

 --sql_stmt := 'SELECT GZ_qa.get_xys(geometry,1,-1,1,93) from GZCPB3.Z636CL_EDGE$ where edge_id=:1';
 --execute immediate sql_stmt into geom2 using  2936;
 sql_stmt := 'SELECT geometry from GZCPB3.Z636CL_EDGE$ where edge_id=:1';
 execute immediate sql_stmt into geom1 using  4094;
--  sql_stmt := 'SELECT geometry from GZCPB3.Z636CL_EDGE$ where edge_id=:1';
-- execute immediate sql_stmt into geom2 using  4095;
-- xys := geom1.sdo_ordinates;
--  x1 := xys(1);
--  y1 := xys(2);
--  x2 := xys(3);
--  y2 := xys(4);
--  reverse_ordinates(xys);
-- geom1.sdo_ordinates := xys;
--  xys := geom2.sdo_ordinates;
-- x3 := xys(1);
-- y3 := xys(2);
-- x4 := xys(3);
-- y4 := xys(4);
-- reverse_ordinates(xys);
-- geom2.sdo_ordinates := xys;
 x1:= -73.713674;
 y1:= 40.870099;
 x2:= -73.675573;
 y2:= 40.856999;
x3:= -73.672758447;
y3:= 40.857984758;
x4:= -73.737024029;
y4:= 40.844171942;
  s := Line_intersect(x1,y1,x2,y2,x3,y3,x4,y4,xi,yi,vertex,dist,tolerance,flag);
  dbms_output.put_line('S' || s || ' xi ' || xi);
  RETURN;
--xys := geom2.sdo_ordinates;

-- for ii in 1..xys.count loop
--   if mod(ii,2) = 0 then
--      xys(ii) := xys(ii) + 0.000018;
--   end if;
-- end loop;
-- geom2.sdo_ordinates := xys;

 xys := geom2.sdo_ordinates;
 x3 := xys(1);
 y3 := xys(2);
 x4 := xys(3);
 y4 := xys(4);

 t := 0.0;
  lrsgeom := sdo_lrs.convert_to_lrs_geom(geom1,0.,100.);
  lrsgeom2 := sdo_lrs.convert_to_lrs_geom(geom2,0.,100.);
 for ii in 1..100 loop
   xi := x1*(1.-t) + t * x2;
   yi := y1*(1.-t) + t * y2;
   t := t + 0.01;
   s :=0.0;
   for jj in 1..100 loop
      xt := x3*(1.-s) + s * x4;
      yt := y3*(1.-s) + s * y4;

      d := sdo_geom.sdo_distance(sdo_geometry(2001,8265,mdsys.sdo_point_type(xi,yi,NULL),NULL,NULL),
                        sdo_geometry(2001,8265,mdsys.sdo_point_type(xt,yt,NULL),NULL,NULL),0.05,'unit=meter');
      if d < best and d < 1. then
           dbms_output.put_line('d ' || round(d,9) || ' dist ' || round(short_oracle_dist(xt,yt,xi,yi),9) ||' t ' || t || ' s ' || s);
           best := d;

           point_geom1 := sdo_lrs.Locate_pt(lrsgeom,t*100.);
           point_geom2 := sdo_lrs.Locate_pt(lrsgeom2,s*100.);
           Od := sdo_geom.sdo_distance(point_geom1,point_geom2,0.05,'unit=meter');
           Xys := point_geom1.sdo_ordinates;
           new_x := Xys(1);
           new_y := Xys(2);
            Xys := point_geom2.sdo_ordinates;
           new_x2 := Xys(1);
           new_y2 := Xys(2);
           dbms_output.put_line('D ' || round(od,9) || ' short '||round(short_oracle_dist(new_x,new_y,new_x2,new_y2),9));

      end if;
      s := s + 0.01;
   end loop;
 end loop;


 dbms_output.put_line('XYs1 ' || sdo_util.getnumvertices(geom1));
-- dbms_output.put_line('XYs2 ' || sdo_util.getnumvertices(geom2));
 same := TRUE;
 new_geom := move_apart_2edges(geom1,geom1,same,new_one,UFS,tolerance);
 dbms_output.put_line('New one ' || new_one);
-- new_geom := move_apart_2edges(geom2,geom1,same,new_one,UFS,tolerance);
END;
FUNCTION measure_zinger(Geom MDSYS.SDO_GEOMETRY) RETURN NUMBER AS

geom1         MDSYS.SDO_GEOMETRY :=geom;
geom2         MDSYS.SDO_GEOMETRY :=geom;


point_geom1         MDSYS.SDO_GEOMETRY;
point_geom2         MDSYS.SDO_GEOMETRY;
lrsgeom1      MDSYS.SDO_GEOMETRY;
lrsgeom2      MDSYS.SDO_GEOMETRY;
xys        MDSYS.SDO_ordinate_array;

   x1         NUMBER;
   y1         NUMBER;
   x2         NUMBER;
   y2         NUMBER;
   x3         NUMBER;
   y3         NUMBER;
   x4         NUMBER;
   y4         NUMBER;
   new_x      NUMBER;
   new_y      NUMBER;
   new_x2     NUMBER;
   new_y2     NUMBER;
   xt         NUMBER;
   yt         NUMBER;
   xi         NUMBER;
   yi         NUMBER;
   od         NUMBER;
   dist       NUMBER;

   s          NUMBER;
   t          NUMBER;
   d          NUMBER;
   best       NUMBER := 1.E10;
BEGIN
 xys := geom.sdo_ordinates;
 x1 := xys(1);
 y1 := xys(2);
 x2 := xys(3);
 y2 := xys(4);

 geom1.sdo_ordinates := MDSYS.SDO_ordinate_array(x1,y1,x2,y2);

 x3 := xys(5);
 y3 := xys(6);
 x4 := xys(7);
 y4 := xys(8);
 geom2.sdo_ordinates := MDSYS.SDO_ordinate_array(x3,y3,x4,y4);
 t := 0.0;
  lrsgeom1 := sdo_lrs.convert_to_lrs_geom(geom1,0.,100.);
  lrsgeom2 := sdo_lrs.convert_to_lrs_geom(geom2,0.,100.);
 /*
 for ii in 1..2 loop
   xi := x1*(1.-t) + t * x2;
   yi := y1*(1.-t) + t * y2;

   s :=0.0;
   for jj in 1..200 loop
      xt := x3*(1.-s) + s * x4;
      yt := y3*(1.-s) + s * y4;

      d := sdo_geom.sdo_distance(sdo_geometry(2001,8265,mdsys.sdo_point_type(xi,yi,NULL),NULL,NULL),
                        sdo_geometry(2001,8265,mdsys.sdo_point_type(xt,yt,NULL),NULL,NULL),0.05,'unit=meter');
      if d < best and d < 1. then
           dbms_output.put_line('d ' || round(d,9) || ' dist ' || round(short_oracle_dist(xt,yt,xi,yi),9) ||' t ' || t || ' s ' || s);
           best := d;

           point_geom1 := sdo_lrs.Locate_pt(lrsgeom1,t*100.);
--           point_geom2 := sdo_lrs.Locate_pt(lrsgeom2,s*100.);
           Od := sdo_geom.sdo_distance(point_geom1,geom2,0.05,'unit=meter');
           Xys := point_geom1.sdo_ordinates;
           new_x := Xys(1);
           new_y := Xys(2);
--            Xys := point_geom2.sdo_ordinates;
           new_x2 := Xys(1);
           new_y2 := Xys(2);
           dbms_output.put_line('D ' || round(od,9) || ' short '||round(short_oracle_dist(new_x,new_y,new_x2,new_y2),9));

      end if;
      s := s + 0.005;
   end loop;
   t := t + 1.;
 end loop;

 */
best := 10000.;
 s :=0.0;
 for ii in 1..1 loop
   xi := x3*(1.-s) + s * x4;
   yi := y3*(1.-s) + s * y4;

   t :=0.43;
   for jj in 1..100 loop
 --   t:= .4379002246;
      xt := x1*(1.-t) + t * x2;
      yt := y1*(1.-t) + t * y2;

      d := sdo_geom.sdo_distance(sdo_geometry(2001,8265,mdsys.sdo_point_type(xi,yi,NULL),NULL,NULL),
                        sdo_geometry(2001,8265,mdsys.sdo_point_type(xt,yt,NULL),NULL,NULL),0.05,'unit=meter');
      dist := short_oracle_dist(xt,yt,xi,yi);
           dbms_output.put_line('t ' || t || ' d ' || round(d,9) || ' dist ' || round(short_oracle_dist(xt,yt,xi,yi),9) ||' t ' || t || ' s ' || s);
    if dist < best and d < 1. then
           best := dist;

           point_geom1 := sdo_lrs.Locate_pt(lrsgeom1,t*100.);
           point_geom2 := sdo_lrs.Locate_pt(lrsgeom2,s*100.);
           Od := sdo_geom.sdo_distance(point_geom1,point_geom2,0.05,'unit=meter');
           Xys := point_geom1.sdo_ordinates;
           new_x := Xys(1);
           new_y := Xys(2);
            Xys := point_geom2.sdo_ordinates;
           new_x2 := Xys(1);
           new_y2 := Xys(2);
           dbms_output.put_line('D ' || round(od,9) || ' short '||round(short_oracle_dist(new_x,new_y,new_x2,new_y2),9));

      end if;
      t := t  + 0.0001;
   end loop;
   s := s + 1.;
 end loop;

 RETURN od;

END;
--
PROCEDURE try_line_intersect(ingeom MDSYS.SDO_GEOMETRY default NULL) as

    geom           mdsys.sdo_geometry;
    xys            mdsys.sdo_ordinate_array;
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
    tol            NUMBER := 0.05;
    s              NUMBER;
    vertex         NUMBER;
    dist           NUMBER;
    flag           PLS_INTEGER :=0;
begin

   geom := SDO_GEOMETRY(2006, 8265, NULL, SDO_ELEM_INFO_ARRAY(1, 2, 1, 5, 2, 1), SDO_ORDINATE_ARRAY(-122.32394, 47.308806, -122.32038, 47.310589, -122.32270402, 47.309425, -122.327, 47.3086));

  xys := geom.sdo_ordinates;
  x1 := xys(1);
  y1 := xys(2);
  x2 := xys(3);
  y2 := xys(4);
  x3 := xys(5);
  y3 := xys(6);
  x4 := xys(7);
  y4 := xys(8);
  s := Line_intersect(x1,y1,x2,y2,x3,y3,x4,y4,xi,yi,vertex,dist,tol,flag);
  dbms_output.put_line('S' || s || ' xi ' || xi);
  s := Line_intersect(x3,y3,x4,y4,x1,y1,x2,y2,xi,yi,vertex,dist,tol,flag);
  dbms_output.put_line('B' || s || ' xi ' || xi);

EXCEPTION
   WHEN OTHERS THEN
     RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
end;
--
FUNCTION try_get_closest(pgeom1 MDSYS.SDO_GEOMETRY,pgeom2 MDSYS.SDO_GEOMETRY default NULL,tolerance NUMBER default 0.05) RETURN NUMBER DETERMINISTIC AS

  geom1     MDSYS.SDO_GEOMETRY := pgeom1;
  geom2     MDSYS.SDO_GEOMETRY := pgeom2;
  xys       MDSYS.SDo_ORDINATE_ARRAY := geom1.sdo_ordinates;
  Info_array mdsys.sdo_elem_info_array := pgeom1.sdo_elem_info;
  result          MDSYS.SDO_LIST_TYPE;
  Start_end          MDSYS.SDO_LIST_TYPE :=MDSYS.SDO_LIST_TYPE(1,2);
  xfound    NUMBER;
  yfound    NUMBER;
  where_it_is NUMBER;
  dist_apart NUMBER;
  seg1       NUMBER;
  seg2       NUMBER;
  dist       NUMBER;
  oid        NUMBER;

  million    NUMBER := 10000000.;
  same       BOOLEAN := FALSE;
  n          PLS_INTEGER;
  ij         PLS_INTEGER;
  res        VARCHAR2(1000);

BEGIN

if geom2 is NULL then
  Xys := geom1.sdo_ordinates;
  n := TRUNC(XYs.count/2);
  geom2 := geom1;
  same := TRUE;
end if;


--  res := GZ_GEOM_UTILS.validate_lines_with_context (geom1,tolerance);
  Start_end(2) := n+1;
  result := CHECK_GEOM_SELF_INTERSECT(XYs,Info_Array,TRUE,Start_end,tolerance,tolerance,tolerance,9,million,oid);

--dist_apart := get_closest(geom1,geom2,same,xfound,yfound,where_it_is);
  for ii in 1..TRUNC(result.count/5) loop
  ij := ii*5-4;
  where_it_is := result(ij);
  seg2 := ABS(MOD(where_it_is,million));
  seg1 := (ABS(where_it_is) -seg2)/million;

  dbms_output.put_line('seg1 ' || seg1 || ' seg2 ' || seg2 || ' Dist ' || round(dist,10));
  end loop;
  RETURN dist_apart;
  end;

PROCEDURE try_reverse_ordinates(xysin MDSYS.SDO_ORDINATE_ARRAY default NULL,lb pls_integer default 1,ub pls_integer default 0) AS

    xys MDSYS.SDO_ORDINATE_ARRAY;
begin
    xys := mdsys.sdo_ordinate_array(1,2,3,4,5,6,7,8,9,10);
    reverse_ordinates(xys,lb,ub);

    for ii in 1..xys.count loop
    dbms_output.put_Line('ii ' || ii || ' xy ' || xys(ii));
  end loop;
end;
Procedure try_check_intersect_edge(p_topo VARCHAR2,p_tolerance NUMBER default 0.05) as

   Geom mdsys.sdo_geometry;

   ids_intersect   MDSYS.sdo_LIST_TYPE;
   result  VARCHAR2(4000);
   sql_stmt VARCHAR2(4000);
BEGIN


  sql_stmt := 'SELECT a.edge_id FROM '
              || p_topo || '_EDGE$ a '
              || 'WHERE GZ_TOPOFIX.CHECK_INTERSECTIONS_EDGE(a.geometry, :p1) != ''TRUE'' ';
         EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO ids_intersect USING p_tolerance;

   for ii in 1..ids_intersect.count loop
       dbms_output.put_line('ID ' || ids_intersect(ii));
   end loop;
END try_check_intersect_edge;
Procedure try_check_close_edge(p_topo VARCHAR2,p_tolerance NUMBER default 0.05) as

   Geom mdsys.sdo_geometry;

   ids_edge1       MDSYS.sdo_LIST_TYPE;
   ids_edge2       MDSYS.sdo_LIST_TYPE;
   result  VARCHAR2(4000);
   sql_stmt VARCHAR2(4000);

BEGIN

sql_stmt := 'SELECT a.edge_id edge1, GZ_TOPOFIX.CHECK_CLOSE_EDGE(:p1, a.edge_id, :p2) edge2 FROM '
                 || p_topo || '_EDGE$ a '
                 || 'WHERE GZ_TOPOFIX.CHECK_CLOSE_EDGE(:p3, a.edge_id, :p4) > 0 ';
            EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO ids_edge1, ids_edge2
                     USING p_topo, p_tolerance, p_topo, p_tolerance;

   for ii in 1..ids_edge1.count loop
       dbms_output.put_line('ID ' || ids_edge1(ii) || ' intersects ' || ids_edge2(ii));
   end loop;
END try_check_close_edge;

    FUNCTION CHECK_INTERSECTIONS_EDGE (
      p_line                     IN SDO_GEOMETRY,
      p_tolerance                IN NUMBER DEFAULT .05,
      p_debug                    IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! 11/26/12 Modified to use Sidey power on looped edges
      --Standard code was working 99.9% of the time, and still works best for non-looping edges
      --   The problem with loops is that Oracle seems to be able to return the head-tail
      --   as a self-intersection spot. OR NOT.  When not, if there is a true self-intersection on a
      --   looped edge the bad spot looks like the single expected head-tail intersection
      --   and the edge falsely passes as OK
      --   Worse, Oracle does this inconsistently on the same geometry, so a looped edge can switch back and
      --   forth between bad and good

      psql              VARCHAR2(4000);
      output            VARCHAR2(4000);
      kount             PLS_INTEGER;

   BEGIN

      IF  p_line.sdo_ordinates(1) = p_line.sdo_ordinates(p_line.sdo_ordinates.COUNT - 1)
      AND p_line.sdo_ordinates(2) = p_line.sdo_ordinates(p_line.sdo_ordinates.COUNT)
      THEN

         --Call to special Sidey loop edge code wrapper

         IF GZ_TOPOFIX.IS_LOOP_EDGE_SELF_INTERSECTING(p_line, p_tolerance, p_debug)
         THEN

            RETURN '1 OR MORE SELF INTERSECTIONS';

         ELSE

            RETURN 'TRUE';

         END IF;

      ELSE

         --SOP edges
         --This is based on a post from Siva R on the oracle spatial discussion forum

         psql := 'SELECT COUNT(*) FROM ( '
              || 'SELECT t.x, t.y, COUNT(*) kount '
              || 'FROM '
              || 'TABLE(SDO_UTIL.GETVERTICES(:p1)) t '
              || 'GROUP BY t.x, t.y '
              || ') WHERE kount > 1';

         EXECUTE IMMEDIATE psql INTO kount USING sdo_geom.sdo_intersection(p_line, p_line, p_tolerance);


         IF kount > 0
         THEN

            RETURN kount || ' SELF INTERSECTIONS';

         ELSE

            RETURN 'TRUE';

         END IF;

      END IF;

   END CHECK_INTERSECTIONS_EDGE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION IS_LOOP_EDGE_SELF_INTERSECTING (
      p_line                     IN SDO_GEOMETRY,
      p_tolerance                IN NUMBER DEFAULT .05,
      p_debug                    IN NUMBER DEFAULT NULL
   ) RETURN BOOLEAN
   AS

      --Matt!  11/21/12
      --This is a streamlined wrapper to Sidey's gz_topofix.CHECK_GEOM_SELF_INTERSECT
      --I prefer to use CHECK_INTERSECTIONS_EDGE above, since it uses the Oracle black box to determine intersections
      --But the logic is breaking down for some closed loops (see the note in check_intersections_edge)
      --Use this instead for loops

      --I think you could also call this wrapper for non-loop edges but I haven't tested that

      result      MDSYS.SDO_LIST_TYPE;
      start_end   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE ();
      xys         MDSYS.SDO_ORDINATE_ARRAY;
      info        MDSYS.SDO_ELEM_INFO_ARRAY;
      deg2rad     CONSTANT NUMBER := 0.0174532925199432957692369076848861271344;
      dec_digits  PLS_INTEGER := 7;
      xdelta      NUMBER;
      ydelta      NUMBER;

   BEGIN

      --these values correspond to segments
      --In a 100 vertex edge example
      --Pass in 1,49, 1,49
      start_end.EXTEND (4);
      start_end (1) := 1;
      start_end (2) := trunc(p_line.sdo_ordinates.COUNT/2) - 1;

      start_end (3) := 1;
      start_end (4) := trunc(p_line.sdo_ordinates.COUNT/2) - 1;

      --put ordinates in once
      xys := p_line.sdo_ordinates;

      --put elem info in once
      info := p_line.sdo_elem_info;

      --Sidey says:
      --Note however xdelta and ydelta are in degrees (easy mistake to make).
      --That said, since you are doing self intersection you can (of course) get away with
      --putting any positive number for these since they are not buffers to another edge.

      IF p_line.sdo_srid = 8265.
      THEN

         xdelta := ROUND(p_tolerance/(111319.490793274 * cos(xys(2)*deg2rad)),dec_digits);

      ELSE

         xdelta := p_tolerance;

      END IF;

      IF p_line.sdo_srid = 8265.
      THEN

         ydelta := .0011;

      ELSE

         ydelta := p_tolerance;

      end if;

      result := gz_topofix.CHECK_GEOM_SELF_INTERSECT (xys,
                                                      info,
                                                      FALSE,         --allow loops head to tail
                                                      start_end,
                                                      p_tolerance,
                                                      xdelta,        --Deltas are for MBR stuff
                                                      ydelta);

      IF result.COUNT = 0
      THEN

         --No self intersect good
         RETURN FALSE;

      ELSE

         --yes self intersect bad

         IF p_debug IS NOT NULL
         THEN

            FOR i IN 1 .. result.COUNT
            LOOP

               --Sidey returns an array of values indicating details on the intersection
               dbms_output.put_line(i || ': ' || result(i));

            END LOOP;

         END IF;

         RETURN TRUE;

         --just for FYIs and giggles, heres what a self intersecting result array looks like

         --    530000052.02
         --    -82.79266801516128828929149971950776238631
         --    37.79393687334777465652832551386808250464
         --    2
         --    .0141459706307900125918190505261055596845

         --53rd and 52nd segment intersect.  Forget what the .02 is
         --The coordinate X
         --The coordinate Y
         --Forget what the 2 is
         --The distance apart

      END IF;

   END IS_LOOP_EDGE_SELF_INTERSECTING;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE CHECK_CLOSE_EDGE_MGR (
      p_topo               IN VARCHAR2,
      p_tolerance          IN NUMBER,
      p_edgeids1           IN OUT GZ_TYPES.numberarray,
      p_edgeids2           IN OUT GZ_TYPES.numberarray,
      p_chunk_size         IN NUMBER DEFAULT 100000
   )
   AS

      --Matt!  Wrapper to check_close_edge
      --       Divides the call into smaller chunks which seems to help with performance

      --Specimen, standalone use like checking a topo in a target schema
      --
      --declare
      --   p_edgeids1           GZ_TYPES.numberarray;
      --   p_edgeids2           GZ_TYPES.numberarray;
      --begin
      --   gz_topofix.CHECK_CLOSE_EDGE_MGR('GZCPB9.Z699IN',.05,p_edgeids1,p_edgeids2,100000);
      --   for i IN 1 .. p_edgeids1.COUNT
      --   LOOP
      --      dbms_output.put_line('Found ' || p_edgeids1(i) || ', ' || p_edgeids2(i));
      --   END LOOP;
      --end;

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      edge_min          NUMBER;
      edge_max          NUMBER;
      prev_max          NUMBER;
      loop_min          NUMBER := 0;
      loop_max          NUMBER;
      temp_edges1       GZ_TYPES.NUMBERARRAY;
      temp_edges2       GZ_TYPES.NUMBERARRAY;


   BEGIN

      psql := 'SELECT COUNT(*) FROM ' || p_topo || '_edge$';

      EXECUTE IMMEDIATE psql INTO kount;

      IF kount > (p_chunk_size * 2)
      THEN

         --assume pretty close to continuous edge ids

         psql := 'SELECT MIN(e.edge_id), MAX(e.edge_id) '
              || 'FROM ' || p_topo || '_edge$ e ';

         EXECUTE IMMEDIATE psql INTO edge_min, edge_max;

         loop_max := edge_min;

         WHILE loop_max < edge_max
         LOOP

            prev_max := loop_max;
            loop_max := prev_max + p_chunk_size;
            loop_min := prev_max;

            psql := 'SELECT a.edge_id edge1, GZ_TOPOFIX.CHECK_CLOSE_EDGE(:p1, a.edge_id, :p2) edge2 FROM '
                 || p_topo || '_edge$ a '
                 || 'WHERE GZ_TOPOFIX.CHECK_CLOSE_EDGE(:p3, a.edge_id, :p4) > :p5 '
                 || 'AND a.edge_id >= :p6 AND a.edge_id < :p7 ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO temp_edges1,
                                                     temp_edges2 USING p_topo,
                                                                       p_tolerance,
                                                                       p_topo,
                                                                       p_tolerance,
                                                                       0,
                                                                       loop_min,
                                                                       loop_max;

            IF temp_edges1.COUNT > 0
            THEN

               p_edgeids1 := GZ_BUSINESS_UTILS.NUMBERARRAY_ADD(p_edgeids1,
                                                               temp_edges1);

               p_edgeids2 := GZ_BUSINESS_UTILS.NUMBERARRAY_ADD(p_edgeids2,
                                                               temp_edges2);

            END IF;

         END LOOP;

      ELSE

         --no division, hit the whole table

         psql := 'SELECT a.edge_id edge1, GZ_TOPOFIX.CHECK_CLOSE_EDGE(:p1, a.edge_id, :p2) edge2 FROM '
              || p_topo || '_edge$ a '
              || 'WHERE GZ_TOPOFIX.CHECK_CLOSE_EDGE(:p3, a.edge_id, :p4) > :p5 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO p_edgeids1,
                                                  p_edgeids2 USING p_topo,
                                                                   p_tolerance,
                                                                   p_topo,
                                                                   p_tolerance,
                                                                   0;

      END IF;

      --No return, edge ids are in/out


   END CHECK_CLOSE_EDGE_MGR;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------


   FUNCTION CHECK_CLOSE_EDGE (
      pTopology        VARCHAR2,
      pedge_id         NUMBER,
      p_tolerance      NUMBER DEFAULT .05
   ) RETURN NUMBER
   AS

      --This function tells whether there are other edges close to the specified edge
      --It will return either -1, for nothing close, or the *first* close edge id it encounters
      --Note that for performance reasons it only checks for close edge ids that have an edge_id
      --   greater than the passed in edge id.  So the expected usage is in a call that checks
      --   an entire topology

      --See also gz_topofix.CHECK_CLOSE_EDGE_MGR for a wrapper to this

      --WWU!   2012 I think
      --Matt!  10/31/13 additional check of previous and next edges, plus formatting/comments
      --                rewrote to check all L/R edges in a single group, rather than 1x L, 1X R

      --Example:
      --select e.edge_id, gz_topofix.check_close_edge('Z699TM',e.edge_id,.05) outp from
      --z699tm_edge$ e
      --where gz_topofix.check_close_edge('Z699TM',e.edge_id,.05) <> -1

      Topology          VARCHAR2 (30) := UPPER (pTopology);
      edge_ids          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE ();
      next_edges        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE ();
      edge_id           NUMBER;
      sql_stmt          VARCHAR2 (4000);
      sql_stmt1         VARCHAR2 (4000);
      sql_stmt3         VARCHAR2 (4000);
      sql_stmt4         VARCHAR2 (4000);
      vmygeom           MDSYS.SDO_GEOMETRY;
      vgeom             MDSYS.SDO_GEOMETRY;
      vLface_id         NUMBER;
      vRface_id         NUMBER;
      vMySNode          NUMBER;
      vMyENode          NUMBER;
      vSNode            NUMBER;
      vENode            NUMBER;
      vDistance         NUMBER;
      vCount            NUMBER;

   BEGIN

      sql_stmt :=
            'SELECT geometry, left_face_id, right_face_id, start_node_id, end_node_id FROM '
         || Topology
         || '_EDGE$ where edge_id=:1';

      EXECUTE IMMEDIATE sql_stmt
         INTO vmygeom, vLface_id, vRface_id, vMySNode, vMyENode
         USING pedge_id;

      vDistance := p_tolerance * 2;

      --Get all edges that are kinda close
      --and have edge_id greater than current edge_id
      --NOTE: Does not check the next and previous edge (that share nodes with the current edge)
      sql_stmt := 'SELECT edge_id, geometry  FROM '  || Topology || '_EDGE$
                        WHERE SDO_WITHIN_DISTANCE (Geometry,:1, ''distance = '||vDistance||''') = ''TRUE''
                            AND edge_id > :3
                            AND start_node_id <> :4
                            AND start_node_id <> :5
                            AND end_node_id <> :6
                            AND end_node_id <> :7
                            AND edge_id IN (SELECT ABS(column_value) FROM TABLE(:8))
                            AND rownum = 1 ';

      --this is the initial filter, just ask if there are any edges at all
      --Thinking that this might be faster without sdo_within_distance.
      --    since it uses the index the plan will always call within_distance on the whole edge$ table
      --    before filtering out all of the other predicates
      sql_stmt1 := 'SELECT COUNT(1)  FROM '  || Topology || '_EDGE$
                        WHERE SDO_WITHIN_DISTANCE (Geometry,:1, ''distance = '||vDistance||''') = ''TRUE''
                            AND edge_id > :3
                            AND start_node_id <> :4
                            AND start_node_id <> :5
                            AND end_node_id <> :6
                            AND end_node_id <> :7
                            AND edge_id IN (SELECT ABS(column_value) FROM TABLE(:8))';

      --Also check the edges that share start and end nodes, most baddies are actually these
      --Matt! 10/31/13
      sql_stmt3 := 'SELECT e.edge_id FROM ' || Topology || '_EDGE$ e '
                || 'WHERE e.edge_id > :p1 '
                || 'AND ((e.start_node_id = :p2 OR e.start_node_id = :p3) OR '
                || '     (e.end_node_id = :p4 OR e.end_node_id = :p5)) '
                || 'AND GZ_GEOM_UTILS.VALIDATE_TOUCHING_LINES(e.geometry, :p6, :p7) <> :p8 '
                || 'AND e.edge_id IN (SELECT ABS(column_value) FROM TABLE(:p9)) ';

      IF  vLface_id > 0
      AND vRface_id > 0
      THEN

         --standard interior face

         sql_stmt4 := 'SELECT * FROM TABLE(SDO_TOPO.get_face_boundary(:p1, :p2)) '
                   || 'UNION ALL '
                   || 'SELECT * FROM TABLE(SDO_TOPO.get_face_boundary(:p3, :p4)) ';

         EXECUTE IMMEDIATE sql_stmt4 BULK COLLECT INTO edge_ids USING Topology, vLface_id,
                                                                      Topology, vRface_id;

      ELSIF vLface_id > 0
      THEN

         --universal face facing
          edge_ids := SDO_TOPO.get_face_boundary (Topology, vLface_id);

      ELSIF vRface_id > 0
      THEN

         edge_ids := SDO_TOPO.get_face_boundary (Topology, vRface_id);

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Something wrong here with left/right faces');

      END IF;

      --are there edges?
      EXECUTE IMMEDIATE sql_stmt1 INTO vCount
            USING vmygeom, pedge_id,
                      vMySNode, vMyENode,
                      vMySNode, vMyENode, edge_ids;

      IF (vCount > 0)
      THEN

         --if yeah, get edge_ids and geoms
         EXECUTE IMMEDIATE sql_stmt INTO edge_id, vgeom
            USING vmygeom, pedge_id,
                      vMySNode, vMyENode,
                      vMySNode, vMyENode, edge_ids;

         IF (SDO_GEOM.sdo_intersection (vmygeom, vgeom, p_tolerance) IS NOT NULL)   --this is the main thing, do they intersect at tol
         AND (GZ_TOPOFIX.EDGE_TOO_CLOSE(Topology, pedge_id, edge_id, 0.001) = 0)    --this has something to do with rejecting rings
         THEN

            --Too close
            RETURN  edge_id;

         END IF;

      END IF;

      --check next and previous edges
      EXECUTE IMMEDIATE sql_stmt3 BULK COLLECT INTO next_edges USING pedge_id,
                                                                     vMySNode,
                                                                     vMyENode,
                                                                     vMySNode,
                                                                     vMyENode,
                                                                     vmygeom,
                                                                     p_tolerance,
                                                                     'TRUE',
                                                                     edge_ids;

      IF next_edges.COUNT > 0
      THEN

         RETURN next_edges(1);

      END IF;

      --Didnt find anything, return -1
      RETURN -1;

   EXCEPTION
      WHEN OTHERS
      THEN

         RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

   END CHECK_CLOSE_EDGE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION EDGE_TOO_CLOSE (
      pTopology        VARCHAR2,
      pedge_id1        NUMBER,
      pedge_id2        NUMBER,
      p_tolerance      NUMBER DEFAULT .05)
      RETURN NUMBER
   AS
      Topology    VARCHAR2 (30) := UPPER (pTopology);
      sql_stmt    VARCHAR2 (4000);
      vSNode1      NUMBER;
      vENode1      NUMBER;
      vSNode2      NUMBER;
      vENode2      NUMBER;

      vSNode      NUMBER;
      vENode      NUMBER;
      vEDistance   NUMBER;
      vNDistance   NUMBER;
      vDistance1   NUMBER;
      vDistance2   NUMBER;
      vDistance3   NUMBER;
      vDistance4   NUMBER;

      vCount   NUMBER;

   BEGIN
      sql_stmt := 'SELECT start_node_id, end_node_id FROM '
         || Topology || '_EDGE$ where edge_id=:1';
       EXECUTE IMMEDIATE sql_stmt INTO vSNode1, vENode1
            USING pedge_id1;

       EXECUTE IMMEDIATE sql_stmt INTO vSNode2, vENode2
            USING pedge_id2;

      sql_stmt := 'SELECT SDO_GEOM.SDO_DISTANCE(a.geometry, b.geometry, :1)
                          FROM '|| Topology || '_NODE$ a, '||Topology || '_NODE$ b
                        WHERE a.node_id = :2 AND b.node_id = :3';
      EXECUTE IMMEDIATE sql_stmt INTO vDistance1
         USING p_tolerance, vSNode1, vSNode2;

      EXECUTE IMMEDIATE sql_stmt INTO vDistance2
         USING p_tolerance, vSNode1, vENode2;

      EXECUTE IMMEDIATE sql_stmt INTO vDistance3
         USING p_tolerance, vENode1, vSNode2;

      EXECUTE IMMEDIATE sql_stmt INTO vDistance4
         USING p_tolerance, vENode1, vENode2;

      IF (  vDistance1 = least(vDistance1, vDistance2, vDistance3, vDistance4)) THEN
         vSNode := vSNode1;
         vENode := vSNode2;
      ELSIF (  vDistance2 = least(vDistance1, vDistance2, vDistance3, vDistance4)) THEN
         vSNode := vSNode1;
         vENode := vENode2;
      ELSIF (  vDistance3 = least(vDistance1, vDistance2, vDistance3, vDistance4)) THEN
         vSNode := vENode1;
         vENode := vSNode2;
      ELSIF (  vDistance4 = least(vDistance1, vDistance2, vDistance3, vDistance4)) THEN
         vSNode := vENode1;
         vENode := vENode2;
      ELSE
         RETURN 0;
      END IF;

      sql_stmt := 'SELECT COUNT(1)
                          FROM '|| Topology || '_EDGE$
                        WHERE (start_node_id = :1 AND end_node_id = :2)
                                OR  (end_node_id = :1 AND  start_node_id= :2) ';
      EXECUTE IMMEDIATE sql_stmt INTO vCount
         USING vSNode, vENode, vSNode, vENode;

      IF (vCount > 0) THEN
         RETURN 1;
      ELSE
         RETURN 0;
      END IF;

   EXCEPTION
      WHEN OTHERS
      THEN
         DBMS_OUTPUT.put_line (SQLCODE || ' ' || SQLERRM);
         DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
   END EDGE_TOO_CLOSE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION GZ_FIX_EDGE (
      p_gz_jobid                 IN VARCHAR2,
      p_topo                     IN VARCHAR2,
      p_log_type                 IN VARCHAR2 DEFAULT 'TOPOFIX',
      p_hold_universal           IN VARCHAR2 DEFAULT 'Y',
      p_tolerance                IN NUMBER DEFAULT .05,
      p_repeat                   IN NUMBER DEFAULT 2,
      p_checkcloseedge           IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2
   AS

      --WWU + Matt!
      --10/1/12 Added! new logic when p_repeat is NULL.  This will mean continue repeating
      --   as long as 1 or more edges are bad and the count of bad edges is decreasing
      --Matt! 11/01/13 More tracking records, code cleanup
      --               Call to check_too_close wrapper that helps with performance
      --! 12/03/13     Added found_something to skip final check when no attempted fixes happened

      psql              VARCHAR2(4000);
      ids_13356         GZ_TYPES.stringarray;
      ids_intersec      GZ_TYPES.stringarray;
      ids_edge1         GZ_TYPES.numberarray;
      ids_edge2         GZ_TYPES.numberarray;

      sub_retval        VARCHAR2(4000);
      final_result      VARCHAR2(4000);
      too_close         VARCHAR2(4000);
      repeat            PLS_INTEGER;
      deadman           PLS_INTEGER := 0;
      r                 PLS_INTEGER;
      fixed_something   PLS_INTEGER := 0;
      found_something   PLS_INTEGER := 0;

   BEGIN

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                  p_topo,
                                                  'GZ_FIX_EDGE',
                                                  p_topo||'_EDGE$',
                                                  'Start  GZ_FIX_EDGE.GZ_FIX_EDGE!');

      IF p_repeat IS NULL
      THEN

         --ensure 1 loop
         r := 0;
         repeat := 1;

      ELSE

         --loop however many times the caller insists
         r := 0;
         repeat := p_repeat;

      END IF;

      WHILE r < repeat
      LOOP

         --reset this flag.  If any edge returns true then set to 1 below
         fixed_something := 0;

         --protect from infinite loops
         IF deadman > 10
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        p_topo,
                                                        'GZ_FIX_EDGE',
                                                        p_topo||'_EDGE$',
                                                        '10 Fix_Edge loops is fruit_loops. Something isnt working out. Exiting');

            EXIT;

         ELSE

            deadman := deadman + 1;

         END IF;

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_TOPOFIX: Work on edge 13356 ');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------
         psql := 'SELECT a.edge_id FROM '
              || p_topo || '_EDGE$ a '
              || 'WHERE SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.geometry, :p1) LIKE :p2 ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                     p_topo,
                                                     'GZ_FIX_EDGE',
                                                     p_topo||'_EDGE$',
                                                     'Calling 13356 VALIDATE_GEOMETRY_WITH_CONTEXT', p_sqlstmt=>psql);

         EXECUTE IMMEDIATE psql BULK COLLECT INTO ids_13356 USING p_tolerance, '13356 %';

         IF ids_13356.COUNT > 0
         THEN

            found_something := 1;

            --log just so we arent hanging here
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   'GZ_FIX_EDGE',
                                                   p_topo||'_EDGE$',
                                                   'On loop ' || deadman || ' there are '||ids_13356.COUNT||' 13356 error(s)!');
         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        p_topo,
                                                        'GZ_FIX_EDGE',
                                                        p_topo||'_EDGE$',
                                                        'Didnt find any 13356 errors');

         END IF;

         --Call RESHAPE_EDGE_WITH_DUPLICATE to fix 13356 problem
         FOR i IN 1 .. ids_13356.COUNT
         LOOP

            sub_retval := NULL;

            BEGIN

               sub_retval := GZ_TOPOFIX.RESHAPE_EDGE_WITH_DUPLICATE(
                                                           p_topo,
                                                           ids_13356(i),
                                                           p_tolerance,
                                                           p_hold_universal,
                                                           p_log_type);

            EXCEPTION
            WHEN OTHERS THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                           p_topo,
                                                           'RESHAPE_EDGE_WITH_DUPLICATE',
                                                           p_topo||'_EDGE$',
                                                           'Fixer error (see error_msg-->) on ' || ids_13356(i),
                                                           NULL,NULL,NULL,NULL,NULL,
                                                           SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            END;

            IF UPPER(sub_retval) = 'TRUE' THEN

               fixed_something := 1;

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                           p_topo,
                                                           'RESHAPE_EDGE_WITH_DUPLICATE',
                                                           p_topo||'_EDGE$',
                                                           'Success, fixer returned --> ' || sub_retval || ' <-- on ' || ids_13356(i));


            ELSE

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                           p_topo,
                                                          'RESHAPE_EDGE_WITH_DUPLICATE',
                                                           p_topo||'_EDGE$',
                                                           'Possible issue, fixer returned --> ' || sub_retval || ' <-- on ' || ids_13356(i));

            END IF;

         END LOOP; --13356 loop

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_TOPOFIX: Work on edge self intersections ');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------
         psql := 'SELECT a.edge_id FROM '
              || p_topo || '_EDGE$ a '
              || 'WHERE GZ_TOPOFIX.CHECK_INTERSECTIONS_EDGE(a.geometry, :p1) != ''TRUE'' ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                     p_topo,
                                                     'GZ_FIX_EDGE',
                                                     p_topo||'_EDGE$',
                                                     'Calling 13349 GZ_TOPOFIX.CHECK_INTERSECTIONS_EDGE', p_sqlstmt=>psql);

         EXECUTE IMMEDIATE psql BULK COLLECT INTO ids_intersec USING p_tolerance;

         IF ids_intersec.COUNT > 0
         THEN

            found_something := 1;

            --log just so we arent hanging here
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   'GZ_FIX_EDGE',
                                                   p_topo||'_EDGE$',
                                                   'On loop ' || deadman || ' there are '||ids_intersec.COUNT||' self intersection error(s)!');

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        p_topo,
                                                        'GZ_FIX_EDGE',
                                                        p_topo||'_EDGE$',
                                                        'Didnt find any self intersection errors');

         END IF;

         --Call RESHAPE_TOO CLOSE with a single edge (repeated) input to fix this type of 13349 problem

         FOR i IN 1 .. ids_intersec.COUNT
         LOOP

            sub_retval := NULL;

            BEGIN

               sub_retval := GZ_TOPOFIX.RESHAPE_TOO_CLOSE(
                                                           USER,
                                                           p_topo,
                                                           ids_intersec(i),
                                                           ids_intersec(i),
                                                           p_tolerance,
                                                           p_hold_universal,
                                                           p_log_type);

            EXCEPTION
            WHEN OTHERS THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                           p_topo,
                                                           'RESHAPE_TOO_CLOSE',
                                                           p_topo||'_EDGE$',
                                                           'Fixer error (see error_msg-->) on ' || ids_intersec(i),
                                                           NULL,NULL,NULL,NULL,NULL,
                                                           SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            END;

            IF UPPER(sub_retval) = 'TRUE' THEN

               fixed_something := 1;

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                      p_topo,
                                                      'RESHAPE_TOO_CLOSE',
                                                      p_topo||'_EDGE$',
                                                      'Success, fixer returned --> ' || sub_retval || ' <-- on ' || ids_intersec(i));


            ELSE
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                      p_topo,
                                                      'RESHAPE_TOO_CLOSE',
                                                      p_topo||'_EDGE$',
                                                      'Possible issue, fixer returned --> ' || sub_retval || ' <-- on ' || ids_intersec(i));
            END IF;

         END LOOP; --self intersection loop

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_TOPOFIX: Work on edges too close ');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------
         too_close := '';

         IF (p_checkcloseedge = 'Y')
         THEN

            --This check is very expensive since it must compare all edge geoms with their neighbors
            --We rarely call it.  Output is the only module that calls it at the moment since we can assume shapefiles shortly after
            --also processing tends to be faster with fewer coordinates in step 3 output

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        p_topo,
                                                        'GZ_FIX_EDGE',
                                                        p_topo||'_EDGE$',
                                                        'Calling expensive edge-to-edge CHECK_CLOSE_EDGE', p_sqlstmt=>psql);

            --Procedure wrapper to GZ_TOPOFIX.CHECK_CLOSE_EDGE
            GZ_TOPOFIX.CHECK_CLOSE_EDGE_MGR(p_topo,
                                            p_tolerance,
                                            ids_edge1,    --IN/OUT
                                            ids_edge2,    --IN/OUT
                                            100000);      --default chunk size 100k

            /*  original, non-managed version of check_close_edge, 2012 - 2013
            psql := 'SELECT a.edge_id edge1, GZ_TOPOFIX.CHECK_CLOSE_EDGE(:p1, a.edge_id, :p2) edge2 FROM '
                 || p_topo || '_EDGE$ a '
                 || 'WHERE GZ_TOPOFIX.CHECK_CLOSE_EDGE(:p3, a.edge_id, :p4) > 0 ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO ids_edge1, ids_edge2
                     USING p_topo, p_tolerance, p_topo, p_tolerance;
            */

            IF ids_edge1.COUNT > 0
            THEN

               found_something := 1;

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                           p_topo,
                                                           'GZ_FIX_EDGE',
                                                           p_topo||'_EDGE$',
                                                           'On loop ' || deadman || ' there are '||ids_edge1.COUNT||' edge pairs too close!');

            ELSE

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                           p_topo,
                                                           'GZ_FIX_EDGE',
                                                           p_topo||'_EDGE$',
                                                           'Didnt find any edge pairs too close');

            END IF;

            --Call RESHAPE_TOO_CLOSE with 2 edge ids to fix this type of 13349 problem

            FOR i IN 1 .. ids_edge1.COUNT
            LOOP

               sub_retval := NULL;

               BEGIN

                  sub_retval := GZ_TOPOFIX.RESHAPE_TOO_CLOSE(
                                                              USER,
                                                              p_topo,
                                                              ids_edge1(i),
                                                              ids_edge2(i),
                                                              p_tolerance,
                                                              p_hold_universal,
                                                              p_log_type);

               EXCEPTION
               WHEN OTHERS THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                              p_topo,
                                                              'RESHAPE_TOO_CLOSE',
                                                              p_topo||'_EDGE$',
                                                              'Fixer error (see error_msg-->) on ' || ids_edge1(i) ||', '||ids_edge2(i),
                                                              NULL,NULL,NULL,NULL,NULL,
                                                              SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

                  too_close :=  too_close || ids_edge1(i) ||', '||ids_edge2(i)||'|';

               END;

               IF UPPER(sub_retval) = 'TRUE'
               THEN

                  fixed_something := 1;

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                         p_topo,
                                                         'RESHAPE_TOO_CLOSE',
                                                         p_topo||'_EDGE$',
                                                         'Success, fixer returned --> ' || sub_retval || ' <-- on ' || ids_edge1(i) ||', '||ids_edge2(i));

               ELSE

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                         p_topo,
                                                         'RESHAPE_TOO_CLOSE',
                                                         p_topo||'_EDGE$',
                                                         'Possible issue, fixer returned --> ' || sub_retval || ' <-- on ' ||ids_edge1(i) ||', '||ids_edge2(i));

                  too_close :=  too_close || ids_edge1(i) ||', '||ids_edge2(i)||'|';

               END IF;

            END LOOP; --edges too close loop

         END IF;


         IF p_repeat IS NOT NULL
         THEN

            --increment loop counter for caller passing in an exact loop count
            r := r + 1;

         ELSIF p_repeat IS NULL
         AND fixed_something = 1
         THEN

            --something got fixed
            --and the caller wants us to keep fixing if we are still making progress
            --set r to artificial 0 value to force another loop.  repeat is 1
            --sometimes though we think we are fixing but the same edge is bad over and over, catch this above in deadman

            r := 0;

         ELSIF p_repeat IS NULL
         AND fixed_something = 0
         THEN

            --we are going nowhere
            r := 1;

         END IF;


      END LOOP;

      IF found_something = 1
      THEN

         psql := 'SELECT a.edge_id FROM '
              || p_topo || '_EDGE$ a '
              || 'WHERE SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.geometry, :p1) LIKE :p2 ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                     p_topo,
                                                     'GZ_FIX_EDGE',
                                                     p_topo||'_EDGE$',
                                                     'Making a final check for 13356 errors',
                                                     p_sqlstmt => psql);

         EXECUTE IMMEDIATE psql BULK COLLECT INTO ids_13356 USING p_tolerance, '13356 %';

         IF ids_13356.COUNT > 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        p_topo,
                                                        'GZ_FIX_EDGE',
                                                        p_topo||'_EDGE$',
                                                        'Oops, ' || ids_13356.COUNT || ' 13356 error(s) remain');

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        p_topo,
                                                        'GZ_FIX_EDGE',
                                                        p_topo||'_EDGE$',
                                                        'Groovy, no 13356 errors remain');

         END IF;

         psql := 'SELECT a.edge_id FROM '
              || p_topo || '_EDGE$ a '
              || 'WHERE GZ_TOPOFIX.CHECK_INTERSECTIONS_EDGE(a.geometry, :p1) <> ''TRUE'' ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                     p_topo,
                                                     'GZ_FIX_EDGE',
                                                     p_topo||'_EDGE$',
                                                     'Making a final check for 13349 errors',
                                                     p_sqlstmt => psql);

         EXECUTE IMMEDIATE psql BULK COLLECT INTO ids_intersec USING p_tolerance;


         IF ids_intersec.COUNT > 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        p_topo,
                                                        'GZ_FIX_EDGE',
                                                        p_topo||'_EDGE$',
                                                        'Oops, ' || ids_intersec.COUNT || ' 13349 error(s) remain');

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        p_topo,
                                                        'GZ_FIX_EDGE',
                                                        p_topo||'_EDGE$',
                                                        'Groovy, no 13349 errors remain');

         END IF;

      END IF;

      IF too_close IS NOT NULL
      THEN

         --track this, otherwise if this is the only problem
         --there are 13356 and 13349 Groovies and then a generic final result "fail" which is confusing
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                     p_topo,
                                                     'GZ_FIX_EDGE',
                                                     p_topo||'_EDGE$',
                                                     'Oops, ' || REGEXP_COUNT(too_close, '\|') || ' edge pairs too close (topofix_2edge) remain');

      END IF;


      IF found_something = 1 AND
      ((ids_13356.COUNT > 0) OR (ids_intersec.COUNT > 0) OR (too_close IS NOT NULL))
      THEN

         final_result := '1';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                     p_topo,
                                                     'GZ_FIX_EDGE',
                                                     p_topo||'_EDGE$',
                                                     'Finish  GZ_FIX_EDGE.GZ_FIX_EDGE: Fail');

      ELSE

         final_result := '0';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                     p_topo,
                                                     'GZ_FIX_EDGE',
                                                     p_topo||'_EDGE$',
                                                     'Finish GZ_FIX_EDGE.GZ_FIX_EDGE: Successful');

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_TOPOFIX: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN final_result;

   END GZ_FIX_EDGE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION CIRCULATE_NODE(Edge_id NUMBER,Topology VARCHAR2) RETURN NUMBER AS
     geom              MDSYS.SDO_GEOMETRY;
     new_geom          MDSYS.SDO_GEOMETRY;
     Xys               MDSYS.SDO_ORDINATE_ARRAY;
     node_id           NUMBER;
     node2             NUMBER;
     new_edge_id       NUMBER;
     x                 NUMBER;
     y                 NUMBER;
     sql_stmt          VARCHAR2(4000);
   BEGIN

     sql_stmt := 'SELECT geometry,start_node_id from '||Topology|| '_EDGE$ where Edge_id=:1';
     EXECUTE IMMEDIATE sql_stmt into geom,node_id using Edge_id;

     new_geom := GZ_SUPER.Robust_line_gen(geom,0);
     Xys := new_geom.sdo_ordinates;
     x := Xys(1);
     y := Xys(2);
     new_geom := MDSYS.SDO_GEOMETRY(2001,geom.SDO_SRID,MDSYS.SDO_POINT_TYPE(x,y,NULL),NULL,NULL);
     dbms_output.put_line('x ' || round(x,7) ||','|| round(y,7));
     node2 := add_node_to_edge(Topology,Edge_id,NULL,new_geom,'FALSE');
     dbms_output.put_line('old node ' || node_id);
     drop_node(Topology,node_id);
      dbms_output.put_line('dropped ' || node_id);
     sql_stmt := 'SELECT edge_id from '||Topology|| '_EDGE$ where Start_node_id=:1';
     EXECUTE IMMEDIATE sql_stmt into new_edge_id using node2;
     return new_edge_id;
   END;
   procedure test_circulate_node(Edge_id NUMBER,Topology VARCHAR2) as
   -- helper procedure to call circulate and avoid cannot performa a DML
   new_edge  number;
   begin
      new_edge := gz_topofix.circulate_node(edge_id,Topology);
   end;

procedure try_set_many_mbr as
  geom              MDSYS.SDO_GEOMETRY;
  XYOrd             MDSYS.SDO_ORDINATE_ARRAY;
  Info_array        MDSYS.SDO_ELEM_INFO_ARRAY:= MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1);
  MBRs              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  xLL               number;
  yLL               number;
  xUR               number;
  yUR               number;
begin

   geom  := mdsys.sdo_geometry(2002,8265.,NULL, mdsys.sdo_elem_info_array(1,2,1),
                mdsys.sdo_ordinate_array(-2,-0.5,-1,-0.3,0,0,0.5,0,1,0,1,1,2,1,2,2,1,2,0,2,0,0.1));
                Xyord := geom.sdo_ordinates;
   SET_MANY_MBR(XYord,Info_Array,MBRs,xLL,yLL,xUR,yUR,0,0.101);
   for ii in 1..MBRs.count loop
      dbms_output.put_line('ii ' || ii || ' MBR ' || MBRs(ii));
   end loop;
end;
--
procedure test_geom_self_intersect as
  deg2rad  CONSTANT      NUMBER   :=0.0174532925199432957692369076848861271344;
   geom  mdsys.sdo_geometry;
   xys       MDSYS.SDo_ORDINATE_ARRAY ;
  Info_array mdsys.sdo_elem_info_array := mdsys.sdo_elem_info_array(1,2,1);
  result          MDSYS.SDO_LIST_TYPE;
  Start_end        MDSYS.SDO_LIST_TYPE :=MDSYS.SDO_LIST_TYPE(1,2,1,2);
  srid         number;
  xdelta       number;
  ydelta       number;
  xdiff        number;
  oid          number := 9884;
  dec_digits   pls_integer :=7;
  tolerance    number :=0.05;
  million    NUMBER := 10000000.;
begin
  geom := SDO_GEOMETRY
(
   2002,
   8265,
   NULL,
   SDO_ELEM_INFO_ARRAY
   (
      1,
      2,
      1
   ),
   SDO_ORDINATE_ARRAY
   (
      -85.89924000000000603449734626337885856628,
      37.71633200000000130103217088617384433746,
      -85.89924399999999593546817777678370475769,
      37.71633500000000083218765212222933769226,
      -85.89763499999999396550265373662114143372,
      37.71766000000000218506102100946009159088,
      -85.89698900000000492127583129331469535828,
      37.71820600000000212048689718358218669891,
      -85.89549800000000345789885614067316055298,
      37.71701099999999939882400212809443473816,
      -85.89525600000000338241079589352011680603,
      37.71681900000000098316377261653542518616,
      -85.89478400000000135605660034343600273132,
      37.71644299999999816463969182223081588745,
      -85.89423999999999637111613992601633071899,
      37.71601100000000172940417542122304439545,
      -85.89421600000000012187229003757238388062,
      37.71600800000000219824869418516755104065,
      -85.89373299999999744613887742161750793457,
      37.71595700000000306317815557122230529785,
      -85.89371500000000025920599000528454780579,
      37.71593899999999877081791055388748645782,
      -85.8936460000000039372025639750063419342,
      37.71591699999999747205947642214596271515,
      -85.89347999999999672127160010859370231628,
      37.71592299999999653437043889425694942474,
      -85.89340400000000386171450372785329818726,
      37.71590100000000234103936236351728439331,
      -85.89307900000000017826096154749393463135,
      37.71574100000000129284671857021749019623,
      -85.89293999999999584815668640658259391785,
      37.71575200000000194222593563608825206757,
      -85.89287099999999952615326037630438804626,
      37.71569199999999710826159571297466754913,
      -85.89276099999999303236108971759676933289,
      37.71565300000000320324033964425325393677,
      -85.89264400000000421186996391043066978455,
      37.71564800000000161617208505049347877502,
      -85.89253999999999678038875572383403778076,
      37.7156250000000028421709430404007434845,
      -85.89247100000000045838532969355583190918,
      37.71560900000000060572347138077020645142,
      -85.89209700000000680120137985795736312866,
      37.71552299999999746660250821150839328766,
      -85.89194500000000687123247189447283744812,
      37.71548800000000056797944125719368457794,
      -85.89177999999999713054421590641140937805,
      37.71544999999999703277353546582162380219,
      -85.89172399999999640840542269870638847351,
      37.71546599999999926922100712545216083527,
      -85.89166099999999914871295914053916931152,
      37.71547900000000197451299754902720451355,
      -85.89159300000000030195224098861217498779,
      37.71538799999999724832377978600561618805,
      -85.89155499999999676674633519724011421204,
      37.71532499999999998863131622783839702606,
      -85.89130000000000109139364212751388549805,
      37.71489499999999850388121558353304862976,
      -85.89122799999999813280737726017832756042,
      37.71476799999999940382622298784554004669,
      -85.8911970000000053460098570212721824646,
      37.7147140000000007376002031378448009491,
      -85.89148099999999885767465457320213317871,
      37.71464399999999983492671162821352481842,
      -85.89154499999999359260982600972056388855,
      37.71462700000000012323653209023177623749,
      -85.89156499999999994088284438475966453552,
      37.71462300000000311683834297582507133484,
      -85.89165699999999503688741242513060569763,
      37.71460199999999929332261672243475914001,
      -85.89276800000000378076947527006268501282,
      37.71434699999999651254256605170667171478,
      -85.89373799999999903320713201537728309631,
      37.71412300000000072941475082188844680786,
      -85.89535399999999754072632640600204467773,
      37.71377600000000285263013211078941822052,
      -85.8958130000000039672158891335129737854,
      37.71406999999999953843143885023891925812,
      -85.89600099999999827105057192966341972351,
      37.71419699999999863848643144592642784119,
      -85.89709000000000571617420064285397529602,
      37.71494599999999763895175419747829437256,
      -85.89736700000000269028532784432172775269,
      37.71513000000000204181560548022389411926,
      -85.8976940000000013242242857813835144043,
      37.71535399999999782494342071004211902618,
      -85.89799700000000370891939383000135421753,
      37.71557500000000118234311230480670928955,
      -85.89843700000000126237864606082439422607,
      37.71587000000000244881448452360928058624,
      -85.89889999999999758983904030174016952515,
      37.71617799999999931515048956498503684998,
      -85.89911700000000394084054278209805488586,
      37.71627600000000057889337767846882343292));
--      -85.89924000000000603449734626337885856628,
--      37.71633200000000130103217088617384433746
--   )
--);
 xys := geom.sdo_ordinates;
  SRID := Geom.SDO_SRID;
     if SRID = 8265. then
         xdelta := ROUND(tolerance/(111319.490793274*cos(xys(2)*deg2rad)),dec_digits);
     else
         xdelta := tolerance;
     end if;
  if SRID = 8265. then
         ydelta := .0011;
 --        xdiff := xUR-xLL;
 --        if xdiff > 1. then
--           ydelta := ydelta * xdiff*xdiff;
--         end if;
   else
     ydelta := tolerance;
   end if;
    Start_end   :=MDSYS.SDO_LIST_TYPE(1,TRUNC(xys.count/2)-1,1,TRUNC(Xys.count/2)-1);
 result := CHECK_GEOM_SELF_INTERSECT(XYs,Info_Array,TRUE,Start_end,tolerance,xdelta,ydelta,9,million,oid);
 dbms_output.put_line('result ' || result.count || ' for  ' || xys.count);
 for ii in 1..result.count loop
    dbms_output.put_line(result(ii));
 end loop;
end;
END GZ_TOPOFIX;

/
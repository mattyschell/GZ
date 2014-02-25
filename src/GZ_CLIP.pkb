CREATE OR REPLACE PACKAGE BODY GZ_CLIP
AS

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   --SEE
   --GENERALIZATION_CLIP
   --For entry point to this package
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   FUNCTION START_CLIP_LOGGING (
      p_schema         IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/01/10
      --Create logging table for this JOB

   BEGIN

      GZ_CLIP.CREATE_GEN_CLIP_TRACKING_LOG(p_schema, p_project_id || p_jobid || '_CLIP_TRACKING');

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,
                                         'START_CLIP_LOGGING',
                                         'STARTING JOB');

      RETURN '0';

   END START_CLIP_LOGGING;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   FUNCTION SET_UP_JOBRUN (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_clip_edge_tab  IN VARCHAR2,              --ex DADSGEN.STEDZ6V6
      p_topo           IN VARCHAR2 DEFAULT NULL  --"national" if null
   ) RETURN VARCHAR2
   AS

      --Matt! 7/31/10
      --Make the <release>_CLIP_JOBRUNS table if it doesn't exist
      --Populate with this the parameters of this job

      --the <release>_clip_jobruns table and this function serve to allow input parameters
      --to morph and take on all kinds of new meanings over time
      --Then this set_up step performs whatever contortions of non-logic are necessary to
      --   make as much of the original clip code work as it was originally specd and written

      psql             VARCHAR2(4000);
      kount            PLS_INTEGER;
      proj_parms       GZ_TYPES.GEN_CLIP_PARAMETERS_REC;
      gen_clip_table   VARCHAR2(32);

   BEGIN

      psql := 'SELECT COUNT(*) FROM '
           || 'user_tables a '
           || 'WHERE '
           || 'a.table_name = :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING UPPER(p_release) || '_CLIP_JOBRUNS';

      IF kount = 0
      THEN

         --Create table for the entire PROJECT (this is the first job run for the project)
         GZ_CLIP.CREATE_GEN_CLIP_JOBRUNS(p_schema,
                                         p_release,
                                         p_project_id,
                                         p_jobid,
                                         p_release || '_CLIP_JOBRUNS');

      END IF;

      IF p_clip_edge_tab IS NOT NULL
      THEN

         --SOP
         --snap the schema name off of the clip edges table
         --for easy backwards compatibility use it as the base for naming the local
         --p_topo _ <table>
         --ie STEDZ6V6 here will be called Z609CL_STEDZ6V6 in code
         gen_clip_table := substr(p_clip_edge_tab,(instr(p_clip_edge_tab,'.')+1));

      ELSE

         --special attribute clip
         --there is no state outline type table, we will fake the state outline from the input topo
         --into this table
         --ie ATTRIBUTE here will be called Z609CL_ATTRIBUTE in code
         gen_clip_table := 'ATTRIBUTE';

      END IF;

      --Get the parms - this is the actual gen_clip_parameters table
      --after this the job will always go to the <release>_clip_jobruns

      proj_parms := GZ_CLIP.GET_CLIP_PROJECT_PARAMETERS(p_release, p_project_id);


      IF p_topo IS NULL
      THEN

         --single biggie topology from which we will be extracting parts.
         --one and only topology is in gen_clip_parameters.gen_topology_name

         --proj_parms.gen_topology_name := p_topo;  dont mess with this, its in there already

         --we giving the name to one and only <input_topo>_<edge work table>
         --will actually create this table in TRANSFER_ATTRIBUTES

         --Why does it take 10 lines of comments to explain 2 lines of code?
         --If the input topology is local, the value of proj_parms.edge_input table is a stub like EWRK
         --If the input topology is remote and edge input table already exists, user enters full, like gzcpb1.z699in_ewrk

         IF GZ_CLIP.GET_SOURCE_SCHEMA(proj_parms.edge_input_table) = UPPER(p_schema)
         THEN

            --returned current schema, SOP
            --so our edge input table will be created (or has been, step 1) in this work schema
            --and be named like MYSCHEMA.Z699IN_EWRK

            IF GZ_CLIP.GET_SOURCE_SCHEMA(proj_parms.gen_topology_name) = UPPER(p_schema)
            THEN

               --no special schema name on the front of the gen_topology_name parameter
               -- =clipping from topo build in local schema
               proj_parms.edge_input_table  := UPPER(p_schema) || '.' || proj_parms.gen_topology_name || '_' || proj_parms.edge_input_table;

            ELSE

               --some sort of schema name in front of gen_topology_name parameter, like GZCPB1.Z699IN
               -- =clipping from a remote schema topo build, but creating local edge attribute table

               --ex ME.z699in_EWRK. Add my local schema, strip the remote topo out, and append the edge stub
               proj_parms.edge_input_table  := UPPER(p_schema) || '.' ||
                                               GZ_CLIP.GET_SOURCE_TOPOLOGY(proj_parms.gen_topology_name) || '_' ||
                                               proj_parms.edge_input_table;

            END IF;

         ELSE

            --some other schema to the left of the dot, full remote table entered by user. Dont change it
            --REMOTESCHEMA.Z699IN_EWRK
            NULL;

            --also, leave face input table as is, ex gzcpb1.z699in_face

         END IF;

         --dont mess with this, its THE face input table for all jobs on this project
         --proj_parms.face_input_table  := p_topo || '_' || proj_parms.face_input_table;

      ELSIF p_topo IS NOT NULL
      THEN

         --state based topo input at runtime.  This is not common post-TAB10
         --I think this could also be a national topo where we are clipping the whole thing, no interior states

         proj_parms.gen_topology_name := p_topo;
         proj_parms.edge_input_table  := p_topo || '_' || proj_parms.edge_input_table;
         proj_parms.face_input_table  := p_topo || '_' || proj_parms.face_input_table;

         --Note that this flavor is not prepared to handle the remote schema step 1 business

      END IF;


      IF proj_parms.gen_clip_snapping_digits IS NULL
      AND p_topo IS NOT NULL
      THEN

         --state based topo input, we are copying it
         --sniff out the source topo snapping digits
         --when we are copying from state based topology instead of building our own

         psql := 'SELECT digits_right_of_decimal '
              || 'FROM user_sdo_topo_info '
              || 'WHERE topology = :p1 '
              || 'and rownum = 1 ';

         EXECUTE IMMEDIATE psql INTO proj_parms.gen_clip_snapping_digits USING p_topo;

      ELSIF proj_parms.gen_clip_snapping_digits IS NULL
      AND p_topo IS NULL
      THEN

         --not valid
         --we want to get our own parameterized snapping digits since we are making a new topo
         RETURN 'GEN_CLIP_PARAMETERS.gen_clip_snapping_digits should be populated for this national input ';

      ELSIF proj_parms.gen_clip_snapping_digits IS NOT NULL
      AND p_topo IS NOT NULL
      THEN

         --not valid
         --this is a state based input, we need to inherit the snapping digits from the source
         --its not really fatal, could run the SQL here
         --But then we'd have a misleading parameter in gen_clip_parameters being ignored
         RETURN 'GEN_CLIP_PARAMETERS.gen_clip_snapping_digits should not be populated for state based inputs.  We inherit from the source ';

      END IF;



      IF kount > 0
      THEN

         --may need to clear out a previous run
         psql := 'DELETE FROM ' || p_release || '_CLIP_JOBRUNS a '
              || 'WHERE '
              || 'a.gen_job_id = :p1 AND '
              || 'a.gen_project_id = :p2 ';

         EXECUTE IMMEDIATE psql USING UPPER(p_jobid),
                                      UPPER(p_project_id);
         COMMIT;

      END IF;

      --hmm this isnt very dynamic
      psql := 'INSERT /*+ APPEND */ INTO ' || p_release || '_CLIP_JOBRUNS a '
           || 'VALUES '
           || '(:p1,:p2,:p3,:p4,:p5,:p6,:p7,:p8,:p9,:p10,:p11,:p12,:p13,:p14,:p15,:p16,:p17,NULL,NULL) ';

      EXECUTE IMMEDIATE psql USING p_jobid,
                                   proj_parms.gen_project_id,
                                   proj_parms.gen_topology_name,
                                   proj_parms.edge_input_table,
                                   proj_parms.face_input_table,
                                   proj_parms.edge_output_table,
                                   p_project_id || p_jobid || '_' || proj_parms.face_output_table,
                                   proj_parms.face_output_measurements,
                                   proj_parms.left_right_attributes,
                                   proj_parms.face_feature_mask_col,
                                   proj_parms.clip_edge_mask_col,
                                   gen_clip_table,
                                   proj_parms.gen_clip_modules,
                                   proj_parms.gen_clip_tolerance,
                                   proj_parms.gen_clip_job_srid,
                                   proj_parms.gen_clip_output_srid,
                                   proj_parms.gen_clip_snapping_digits;

      COMMIT;


      RETURN '0';


   END SET_UP_JOBRUN;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------------

   FUNCTION GET_MASKS (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_clip_mask      IN VARCHAR2 DEFAULT NULL --optional state based
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 3/08/10



      psql           VARCHAR2(4000);
      clip_parms     GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      output         GZ_TYPES.stringarray;
      start_time     TIMESTAMP;


   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'GET_MASKS',NULL,'STARTING');


      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release,p_project_id,p_jobid);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GET_MASKS: Execute SQL');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --We just finished populating the edge input table with a new field
      --The clip mask field.  It should have all the possible clip masks
      --Current generalization: this just return a list of statefps
      --Or when state-based inputs, should just be one state

      --This is STUPID for national topology inputs
      --We are getting this list for all 50+ states and then
      --but only using it on the first job to fully populate the gen_clip_modules table

      psql := 'SELECT DISTINCT ' || clip_parms.face_feature_mask_col || ' '
           || 'FROM ' || clip_parms.edge_input_table || ' '
           || 'WHERE '
           || clip_parms.face_feature_mask_col || ' IS NOT NULL AND '
           || clip_parms.face_feature_mask_col || ' != :p1 '
           || 'ORDER BY ' || clip_parms.face_feature_mask_col || ' ';

      --We hard coded a 00 for edges that are clip mask boundaries
      --Do not pick it up here as a valid clip mask
      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING '00';

      IF output.COUNT = 0
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'GET_MASKS',NULL,
                                            'Nothing returned from query, going with mask ' || p_clip_mask);

         --if nothing is an interior edge, then we probably have DC or something
         --just a state outline.  Return the mask that came in
         output(1) := p_clip_mask;

      END IF;


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'GET_MASKS',clip_parms.edge_input_table,'COMPLETED',start_time,
                                          NULL,NULL,psql);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GET_MASKS: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END GET_MASKS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------------

   FUNCTION GET_MODULES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_modules        IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 5/12/10
      --Added command line override pass in of module string 7/26/11

      clip_parms     GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      output         GZ_TYPES.stringarray;
      start_time     TIMESTAMP;


   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'GET_MODULES','GEN_CLIP_PARMS','STARTING');

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release,p_project_id, p_jobid);

      IF p_modules IS NOT NULL
      THEN

         clip_parms.gen_clip_modules := p_modules;

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GET_MODULES: Populate output array');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF length(clip_parms.gen_clip_modules) != 10
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Wrong number of modules in gen_clip_modules: ' || clip_parms.gen_clip_modules);
      END IF;

      output := GZ_BUSINESS_UTILS.SPLIT(clip_parms.gen_clip_modules);

      IF output.COUNT != 10
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Problem with get_modules, we have a count of ' || output.COUNT);
      END IF;


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'GET_MODULES','GEN_CLIP_PARMS','COMPLETED',start_time);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GET_MODULES: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END GET_MODULES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION GET_EDGE_GEOMETRY (
      p_topo            IN VARCHAR2,
      p_edge_id         IN VARCHAR2
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 7/10/10
      --Got tired of typing this over and over

      psql     VARCHAR2(4000);
      output   SDO_GEOMETRY;

   BEGIN

      psql := 'SELECT e.geometry '
           || 'FROM ' || p_topo || '_EDGE$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO output USING p_edge_id;


      RETURN output;


   END GET_EDGE_GEOMETRY;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

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


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   --Table Creation and accessors below


   FUNCTION GET_CLIP_PARAMETERS (
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_jobid              IN VARCHAR2
   ) RETURN GZ_TYPES.GEN_CLIP_JOBRUNS_REC
   AS

      --Matt!
      --Object accessor for clip parameters for a generalization job
      --This actually access XXX_CLIP_JOBRUNS but too much typing to rename it

      psql     VARCHAR2(4000);
      output   GZ_TYPES.GEN_CLIP_JOBRUNS_REC;

   BEGIN

      psql := 'SELECT a.* FROM ' || p_release || '_CLIP_JOBRUNS a '
           || 'WHERE '
           || 'a.gen_job_id = :p1 AND '
           || 'a.gen_project_id = :p2 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING UPPER(p_jobid),
                                                  UPPER(p_project_id);

      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Yo, Clip_Parameter record not found for jobid ' || p_jobid || ' project id ' || p_project_id
                                        || ' in ' || p_release || '_CLIP_JOBRUNS');
         WHEN TOO_MANY_ROWS THEN
            RAISE_APPLICATION_ERROR(-20001,'Yo, found more than one Clip_Parameter record for jobid ' || p_jobid
                                         || ' in ' || p_release || '_CLIP_JOBRUNS');
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table ' || p_release || '_CLIP_JOBRUNS does not exist!');
            ELSE
               RAISE;
            END IF;
      END;

      RETURN output;

   END GET_CLIP_PARAMETERS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GET_CLIP_PROJECT_PARAMETERS (
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2
   ) RETURN GZ_TYPES.GEN_CLIP_PARAMETERS_REC
   AS

      --Matt!
      --Object accessor for clip parameters at project level for a generalization job

      psql     VARCHAR2(4000);
      output   GZ_TYPES.GEN_CLIP_PARAMETERS_REC;

   BEGIN

      psql := 'SELECT a.* FROM GEN_CLIP_PARAMETERS a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING UPPER(p_release),
                                                  UPPER(p_project_id);

      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Yo, Clip_Parameter record not found for release ' || p_release
                                        || ', project id ' || p_project_id );
         WHEN TOO_MANY_ROWS THEN
            RAISE_APPLICATION_ERROR(-20001,'Yo, found more than one Clip_Parameter record for release ' || p_release
                                        || 'project id ' || p_project_id);
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table GEN_CLIP_PARAMETERS does not exist!');
            ELSE
               RAISE;
            END IF;
      END;

      RETURN output;

   END GET_CLIP_PROJECT_PARAMETERS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GET_SOURCE_SCHEMA (
      p_input                    IN VARCHAR2
   ) RETURN VARCHAR2 DETERMINISTIC
   AS

      --Matt! 2/8/13
      --For some gen_clip_parameters.topology_name like input, return the schema name
      --Input could be either <schema>.<topo> --return--> schema
      --                   or      <topo>     --return--> current schema

   BEGIN

      IF INSTR(p_input,'.') = 0
      THEN

         --specific inplementation overrides get_x_of_tha_dot NULL
         RETURN SYS_CONTEXT('USERENV','CURRENT_SCHEMA');

      ELSE

         RETURN GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_input, 'LEFT');

      END IF;


   END GET_SOURCE_SCHEMA;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GET_SOURCE_TOPOLOGY (
      p_input                    IN VARCHAR2
   ) RETURN VARCHAR2 DETERMINISTIC
   AS

      --Matt! 2/8/13
      --For some gen_clip_parameters.topology_name like input, return the schema name
      --Input could be either <schema>.<topo> --return--> schema
      --                   or      <topo>     --return--> current schema

   BEGIN

      IF INSTR(p_input,'.') = 0
      THEN

         --specific inplementation overrides get_x_of_tha_dot NULL
         RETURN UPPER(p_input);

      ELSE

         RETURN GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_input, 'RIGHT');

      END IF;


   END GET_SOURCE_TOPOLOGY;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GET_EDGE_TABLE (
      p_project_id            IN VARCHAR2,
      p_jobid                 IN VARCHAR2,
      p_edge_input_table      IN VARCHAR2
   ) RETURN VARCHAR2 DETERMINISTIC
   AS

      --Matt! 2/15/13

      --This is meant to replace all of the
      --newedgetable := p_project_id || p_jobid || '_' || clip_parms.edge_input_table;
      --   shoo, love of mine

      --p_project_id            Z6
      --p_jobid                 01CL
      --p_edge_input_table      Z699IN_EWRK
      --                or      SCHEL010.Z699IN_EWRK
      --our working edge table  Z601CL_Z699IN_EWRK

   BEGIN

      IF GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_edge_input_table, 'RIGHT') IS NULL
      THEN

         --SOP
         RETURN p_project_id || p_jobid || '_' || p_edge_input_table;

      ELSE

         RETURN p_project_id || p_jobid || '_' || GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_edge_input_table, 'RIGHT');

      END IF;

   END GET_EDGE_TABLE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION PIPE_MODULES (
      p_masks           IN MDSYS.STRING_ARRAY
   ) RETURN GZ_TYPES.GEN_CLIP_MODULES PIPELINED
   AS

      --Matt! 5/04/10
      output     GZ_TYPES.GEN_CLIP_MODULES_REC;

   BEGIN

      FOR i in p_masks.FIRST .. p_masks.LAST
      LOOP

         output.gen_clip_mask := p_masks(i);

         output.module_1 := 'N';
         output.module_2 := 'N';
         output.module_3 := 'N';
         output.module_4 := 'N';
         output.module_5 := 'N';
         output.module_6 := 'N';
         output.module_7 := 'N';
         output.module_8 := 'N';
         output.module_9 := 'N';
         output.module_10 := 'N';

         PIPE ROW(output);

      END LOOP;


   END PIPE_MODULES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

    PROCEDURE UPDATE_MODULES (
      p_schema         IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_modules        IN VARCHAR2
   )
   AS

      --Matt! 5/12/10
      --Update a single successful Y for a mask+module
      --p_modules is either a single number module, like '1'
      --or a reset value string like 'NNNNYYYYYY'

      psql        VARCHAR2(4000);
      yandns      GZ_TYPES.stringarray;
      beenhere    PLS_INTEGER := 0;


   BEGIN

      IF p_modules LIKE 'N%'
      OR p_modules LIKE 'Y%'
      THEN

         yandns := GZ_BUSINESS_UTILS.SPLIT(p_modules);

         --Update all modules we are about to run back to N
         psql := 'UPDATE ' || p_project_id || '_CLIP_MODULES' || ' a '
              || 'SET ';

         --could be all clever with the bind variables
         --next lifetime

         FOR i IN 1 .. yandns.COUNT
         LOOP

            IF yandns(i) = 'Y'
            THEN

               IF beenhere = 0
               THEN

                  psql := psql || 'a.module_' || TO_CHAR(i) || ' = ''N'' ';

               ELSE

                  psql := psql || ', a.module_' || TO_CHAR(i) || ' = ''N'' ';

               END IF;

               beenhere := 1;

            END IF;

         END LOOP;

         psql := psql || 'WHERE '
                      || 'a.gen_clip_mask = :p1 ';

         EXECUTE IMMEDIATE psql USING p_mask;

         COMMIT;

      ELSE

         psql := 'UPDATE ' || p_project_id || '_CLIP_MODULES' || ' a '
              || 'SET '
              || 'a.module_' || p_modules || ' = :p1 '
              || 'WHERE '
              || 'a.gen_clip_mask = :p2 ';

         EXECUTE IMMEDIATE psql USING 'Y',
                                       p_mask;

         COMMIT;



      END IF;


   END UPDATE_MODULES;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------


    FUNCTION START_CLIP_MODULE_LOGGING (
      p_schema         IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_masks          IN GZ_TYPES.stringarray,   --If input is a national topo, we have 50+ masks here. if state, just the p_mask
      p_mask           IN VARCHAR2 DEFAULT NULL   --always the current mask
   ) RETURN VARCHAR2
   AS

      --Matt! 5/04/10
      --Create module logging table for this PROJECT if no exist
      --This thing is a mess.  Needs to go

      psql        VARCHAR2(4000);
      psql2       VARCHAR2(4000);
      kount       PLS_INTEGER;
      kount2      PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(*) FROM '
           || 'user_tables a '
           || 'WHERE '
           || 'a.table_name = :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING UPPER(p_project_id) || '_CLIP_MODULES';


      IF kount = 0
      THEN

         --FIRST JOB FOR THE PROJECT
         --create the xx_clip_modules table

         BEGIN

            GZ_CLIP.CREATE_GEN_CLIP_MODULE_LOG(p_schema, p_project_id || '_CLIP_MODULES');

         EXCEPTION
         WHEN OTHERS
         THEN

            IF UPPER(SQLERRM) LIKE UPPER('%resource busy%')
            THEN

               --some other job started simultaneously
               --this is some shameful stuff
               GZ_BUSINESS_UTILS.CPB_SLEEP(3);

               kount := 1;

            ELSE

               RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

            END IF;

         END;


         IF kount = 0
         THEN

            --fresh table? Insert all the masks in there for whatever our input edge work table is
            --if this is a state input topo, just one to insert
            --Not really sure if this universe (derived from the input edge table)
            --is necessarily true to production

            psql := 'INSERT /*+ APPEND */ INTO ' || p_project_id || '_CLIP_MODULES '
                 || 'SELECT * FROM TABLE(GZ_CLIP.PIPE_MODULES(:p1))';

            BEGIN

               EXECUTE IMMEDIATE psql USING GZ_CLIP.STRINGARRAY_TO_VARRAY(p_masks);
               COMMIT;

            EXCEPTION
               WHEN OTHERS
               THEN

                  IF SQLCODE = -60
                  OR SQLCODE = -18014
                  OR SQLCODE = -942 --table or view does not exist
                  THEN

                     --deadlock just try again ... in a sec
                     --I think there are conflicting job possibilities here
                     --1 created the table, 2 recreated the empty table, now both are in here

                     psql2 := 'SELECT count(*) '
                           || 'FROM ' || p_project_id || '_CLIP_MODULES';

                     EXECUTE IMMEDIATE psql2 INTO kount2;

                     IF kount2 = 0
                     THEN

                        EXECUTE IMMEDIATE psql USING GZ_CLIP.STRINGARRAY_TO_VARRAY(p_masks);
                        COMMIT;

                     END IF;

                  ELSE

                     RAISE;

                  END IF;


            END;

         END IF;

      END IF;

      IF kount > 0
      AND p_masks.COUNT = 1
      THEN

         --this is a new state input topo
         --table already exists

         psql := 'INSERT /*+ APPEND */ INTO ' || p_project_id || '_CLIP_MODULES '
              || 'SELECT * FROM TABLE(GZ_CLIP.PIPE_MODULES(:p1))';

         BEGIN

            EXECUTE IMMEDIATE psql USING GZ_CLIP.STRINGARRAY_TO_VARRAY(p_masks);
            COMMIT;

         EXCEPTION
         WHEN OTHERS
         THEN

               --If a rerun its already in there
               NULL;

         END;

      END IF;


      IF kount > 0
      THEN

         --If table does exist, this mask is either in there because we just inserted it above here
         --Or this is a restart
         --probably redundant, but
         --reset all Ys to Ns, we are restarting from the beginning if we are in here
         --Actually I think this is handled in the caller and with the above (what this mean?)

         GZ_CLIP.UPDATE_MODULES(p_schema,
                                p_project_id,
                                p_jobid,
                                p_mask,
                                'YYYYYYYYYY'); --Ys for run these, get reset to Ns


      END IF;

      RETURN '0';

   END START_CLIP_MODULE_LOGGING;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   FUNCTION VERIFY_CLIP_INPUTS (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_clip_edge_tab  IN VARCHAR2,
      p_edge_tab       IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2
   AS

      --Matt! 3/2/10
      --So far checks for table existence and that edge counts and face counts match
      --Corresponds to prerequisites of
      --  http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions


      psql           VARCHAR2(4000);
      clip_parms     GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      output         VARCHAR2(8000);
      start_time     TIMESTAMP;
      clip_gtype     NUMBER;
      face_fields    GZ_TYPES.stringarray;
      column_str     VARCHAR2(4000);
      column_str2    VARCHAR2(4000);
      badkount       PLS_INTEGER;
      ref_tables     GZ_TYPES.stringarray;


   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'VERIFY_CLIP_INPUTS',NULL,'STARTING');

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release,p_project_id,p_jobid);

       --return includes geoid
      face_fields := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                            p_project_id,
                                                            'ATTRIBUTE',
                                                            clip_parms.left_right_attributes);


       ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_CLIP_INPUTS: Check tables exist');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --1. usually <input_topology>_FACE
      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(clip_parms.face_input_table)
      THEN

         output := output || 'Dude, ' || clip_parms.face_input_table || ' doesnt exist. Check gen_clip_parameters.face_input_table | ';

      END IF;

      --2. Input topo edge$
      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(clip_parms.gen_topology_name || '_EDGE$')
      THEN

         output := output || 'Dude, ' || clip_parms.gen_topology_name || '_EDGE$ doesnt exist | ';

      END IF;

      --3. Input topo face$
      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(clip_parms.gen_topology_name || '_FACE$')
      THEN

         output := output || 'Dude, ' || clip_parms.gen_topology_name || '_FACE$ doesnt exist | ';

      END IF;

      --4. special reference tables, we expect them to exist

      ref_tables := GZ_TYPES.LEGAL_REFERENCE_TABLES();

      FOR i IN 1 .. ref_tables.COUNT
      LOOP

         IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(ref_tables(i))
         THEN

            output := output || 'Dude, reference table ' || ref_tables(i) || ' doesnt exist | ';

         END IF;

      END LOOP;


      --5. clipping table, ex 'DADSGEN.STEDZ6V5'
      IF p_clip_edge_tab IS NOT NULL AND
      NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_clip_edge_tab)
      THEN

         output := output || 'Dude, table ' || p_clip_edge_tab || ' doesnt exist. Check input parameters to generalization_clip | ';

      END IF;

      --6. left right attributes ref table, ex 'REFERENCE_FACE_FIELDS'
      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(clip_parms.left_right_attributes)
      THEN

         output := output || 'Dude, table ' || clip_parms.left_right_attributes || ' doesnt exist. Check gen_clip_parameters.left_right_attributes | ';

      END IF;

      --7. Edge work table has weird naming convention of <topo_in>_<topo_out>_clip_parms.edge_input_table
      --   At this stage we are looking at the jobrun version of edge_input_table, the "transfer attributes" table name
      --   Its name is like <topo_in>_clip_parms.edge_input_table
      --   This now effectively the table extension, which we limit to 14 characters

      IF GZ_TOPO_UTIL.GET_X_OF_THA_DOT(clip_parms.edge_input_table,'LEFT') IS NULL
      THEN

         --just a table name
         IF LENGTH(clip_parms.edge_input_table) > 14
         THEN

            output := output || 'Dude, this is my bad but the edge_input_table name (' || clip_parms.edge_input_table || ') is too long. | ';

         END IF;

      ELSE

         --strip just the table to tha right of tha dot
         IF LENGTH(GZ_TOPO_UTIL.GET_X_OF_THA_DOT(clip_parms.edge_input_table,'RIGHT')) > 14
         THEN

            output := output || 'Dude, this is my bad but the edge_input_table name (' || clip_parms.edge_input_table || ') is too long. | ';

         END IF;

      END IF;


      --if we have tables not existing at this step its pointless to continue, the other
      --checks will just give conflicting error messages

      IF output IS NOT NULL
      THEN

         RETURN output;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_CLIP_INPUTS: Check Face and Edge Counts');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --phace table phirst

      --This isn't supposed to happen but it always does (in classic topo build, not in alternative)
      --Delete the universal face
      --psql := 'DELETE FROM ' || clip_parms.face_input_table || ' a '
        --   || 'WHERE a.face_id = :p1 ';

      --I think its safe to now just check for it rather than assume its there

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || clip_parms.face_input_table || ' a '
           || 'WHERE a.face_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO badkount USING -1;

      IF badkount <> 0
      THEN

         output := output || clip_parms.face_input_table || ' has a face feature with face id -1 | ';

      END IF;


      --face has no more than face $
      psql := 'SELECT count(*) FROM ('
           || 'SELECT face_id FROM ' || clip_parms.face_input_table || ' '
           || 'MINUS '
           || 'SELECT face_id FROM ' || clip_parms.gen_topology_name || '_FACE$ '
           || 'WHERE face_id != :p1 '
           || ')';

      EXECUTE IMMEDIATE psql INTO badkount USING -1;

      IF badkount != 0
      THEN

         output := output || clip_parms.face_input_table || ' has more faces than '
                          || clip_parms.gen_topology_name || '_FACE$ | ';

      END IF;

      --face$ has no more than face
      psql := 'SELECT count(*) FROM ('
           || 'SELECT face_id FROM ' || clip_parms.gen_topology_name || '_FACE$ '
           || 'WHERE face_id != :p1 '
           || 'MINUS '
           || 'SELECT face_id FROM ' || clip_parms.face_input_table || ' '
           || ')';
      EXECUTE IMMEDIATE psql INTO badkount USING -1;

      IF badkount != 0
      THEN

         output := output || clip_parms.gen_topology_name || '_FACE$ has more faces than '
                          || clip_parms.face_input_table || ' | ';

      END IF;

      --only check edge tab related stuff if the caller says the edge table exists

      IF p_edge_tab = 'Y'
      THEN

         --1. usually <work_topology>_EWRK
         IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(clip_parms.edge_input_table)
         THEN

            output := output || 'Dude, ' || clip_parms.edge_input_table || ' doesnt exist. Check gen_clip_parameters.edge_input_table | ';

         END IF;

         --edge has no more than edge$
         psql := 'SELECT count(*) FROM ('
              || 'SELECT edge_id FROM ' || clip_parms.edge_input_table || ' '
              || 'MINUS '
              || 'SELECT edge_id FROM ' || clip_parms.gen_topology_name || '_EDGE$ '
              || ')';

         EXECUTE IMMEDIATE psql INTO badkount;

         IF badkount != 0
         THEN

            output := output || clip_parms.edge_input_table || ' has more edges than '
                             || clip_parms.gen_topology_name || '_EDGE$ | ';

         END IF;

         --edge$ has no more than edge
         psql := 'SELECT count(*) FROM ('
              || 'SELECT edge_id FROM ' || clip_parms.gen_topology_name || '_EDGE$ '
              || 'MINUS '
              || 'SELECT edge_id FROM ' || clip_parms.edge_input_table || ' '
              || ')';

         EXECUTE IMMEDIATE psql INTO badkount;

         IF badkount != 0
         THEN

            output := output || clip_parms.gen_topology_name || '_EDGE$ has more faces than '
                             || clip_parms.edge_input_table || ' | ';

         END IF;

         --after transfer attributes we should have built face_id index on the input face table
         --if it wasnt there.  Check now, this scenario can happen if the tranfer attributes step gets skipped

         IF NOT GZ_BUSINESS_UTILS.INDEX_EXISTS(clip_parms.face_input_table,
                                          'FACE_ID',
                                          'NORMAL')
         THEN

            output := output || ' ' || clip_parms.face_input_table || ' is missing an index on face_id | ';

         END IF;

         --check that the columns on the edge work table match the reference_face_fields requested
         --Added this now that edge work table could be in a remote schema.  Must ensure that the
         --reference_face_fields in the current job are the same or a subset of whatever ran for step 1 over yonder
         --also applies to local jobs too

         FOR i IN 1 .. face_fields.COUNT
         LOOP

            --and column_name IN ('L_AIANNHCE','R_AIANNHCE','L_CDFP',...)

            IF i <> face_fields.COUNT
            THEN

               column_str2 := column_str2 || '''L_' || face_fields(i) || ''',''R_' || face_fields(i) || ''',';

            ELSE

               column_str2 := column_str2 || '''L_' || face_fields(i) || ''',''R_' || face_fields(i) || '''';

            END IF;

         END LOOP;

         IF NOT GZ_BUSINESS_UTILS.COLUMN_EXISTS(clip_parms.edge_input_table,
                                           column_str2,
                                           (face_fields.COUNT * 2))
         THEN

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'VERIFY_CLIP_INPUTS',clip_parms.edge_input_table,
                                               'Table ' || clip_parms.edge_input_table ||
                                               ' is missing L_ or R_ attributes requested in ' || clip_parms.left_right_attributes);

            output := output || 'Table ' || clip_parms.edge_input_table ||
                                ' is missing L_ or R_ attributes requested in ' || clip_parms.left_right_attributes || ' ';

         END IF;


      END IF;


      --This should supposedly never happen, but we'll check for now
      --All faces should have a statefp, for the usual example of state clipping
      --If not, the universal face probably got in there, or we have
      --junk that isn't supposed to be part of the universe (ex Virgin Islands faces)
      psql := 'SELECT count(*) '
           || 'FROM ' || clip_parms.face_input_table || ' '
           || 'WHERE ' || clip_parms.face_feature_mask_col || ' IS NULL ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO badkount;

         IF badkount != 0
         THEN

            output := output || clip_parms.face_input_table || ' has faces without a '
                             || clip_parms.face_feature_mask_col || ' populated | ';

         END IF;

      EXCEPTION
      WHEN OTHERS
      THEN

         output := output || 'SQL to check face table failed -->' || psql || '<--|';

      END;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_CLIP_INPUTS: Check Clip Geometry');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --pretty basic.  More robust checking happens in the loop checker

      IF p_clip_edge_tab IS NOT NULL
      THEN

         psql := 'SELECT a.sdogeometry.sdo_gtype '
              || 'FROM '
              || p_clip_edge_tab || ' a '
              || 'WHERE rownum = 1 ';

         EXECUTE IMMEDIATE psql INTO clip_gtype;

         IF clip_gtype != 2002
         THEN

            output := output || '|Looks like we have a gtype of ' || clip_gtype || ' '
                             || 'in ' || p_clip_edge_tab || '.  I expect 2002 ';

         END IF;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_CLIP_INPUTS: Check Left Right attributes on face table');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Need to check and give a descriptive error if the REFERENCE_FACE_FIELDS_ZX
      --'attributes' dont match the columns in the face input table
      --Usually this is just an oversight on an initial or testing run

      --got face fields at the beginning

      FOR i IN 1 .. face_fields.COUNT
      LOOP

         --and column_name IN ('AIANNHCE','ARTLI','CDFP','GEOID')

         IF i != face_fields.COUNT
         THEN

            column_str := column_str || '''' || face_fields(i) || ''',';

         ELSE

            column_str := column_str || '''' || face_fields(i) || '''';

         END IF;

      END LOOP;

      IF NOT GZ_BUSINESS_UTILS.COLUMN_EXISTS(clip_parms.face_input_table,
                                        column_str,
                                        face_fields.COUNT)
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'VERIFY_CLIP_INPUTS',clip_parms.edge_input_table,
                                            'Table ' || clip_parms.face_input_table ||
                                            ' is missing attributes requested in ' || clip_parms.left_right_attributes);

         output := output || 'Table ' || clip_parms.face_input_table ||
                             ' is missing attributes requested in ' || clip_parms.left_right_attributes || ' ';

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_CLIP_INPUTS: Misc ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --check that input face table has a spatial index
      --it can get lost in copies


      IF NOT GZ_BUSINESS_UTILS.INDEX_EXISTS(clip_parms.face_input_table,
                                       'SDOGEOMETRY',
                                       'DOMAIN')
      THEN

         output := output || 'Table ' || clip_parms.face_input_table || ' is missing a spatial index on sdogeometry |';

      END IF;

      IF NOT GZ_BUSINESS_UTILS.INDEX_EXISTS(clip_parms.face_input_table,
                                       'TOPOGEOM',
                                       'DOMAIN')
      THEN

         output := output || 'Table ' || clip_parms.face_input_table || ' is missing a domain index on topogeom |';

      END IF;

      ----------------------------
      --What else should we check??



      IF output IS NULL
      THEN
         output := '0';
      END IF;

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'VERIFY_CLIP_INPUTS',clip_parms.edge_input_table,'COMPLETED',start_time);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_CLIP_INPUTS: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END VERIFY_CLIP_INPUTS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   FUNCTION TRANSFER_ATTRIBUTES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_topo           IN VARCHAR2  --expect a real input topology here, always. Could be local or remote with dot
   ) RETURN VARCHAR2
   AS

      --Matt! 3/2/10
      --Corresponds to step 1 of
      --   http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions
      -- Modifications for new edge attribute call 11/5/10


      psql           VARCHAR2(4000);
      clip_parms     GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      output         VARCHAR2(4000);
      start_time     TIMESTAMP;
      edge_table     VARCHAR2(32);


   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'TRANSFER_ATTRIBUTES',NULL,'STARTING');


      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release,p_project_id,p_jobid);

      IF INSTR(clip_parms.edge_input_table, '.') = 0
      THEN

         edge_table := clip_parms.edge_input_table;

      ELSE

         --Could be remote or local, whatever, get the schema off it
         --SCHEL010.Z699IN_EWRK
         edge_table := GZ_TOPO_UTIL.GET_X_OF_THA_DOT(clip_parms.edge_input_table, 'RIGHT');

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 5');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('TRANSFER_ATTRIBUTES: Add spatial index to face table');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --I think this step was only necessary in classic topo build where the face table spatial index was a crapshoot
      --Will switch now to assume that its there

      IF NOT GZ_BUSINESS_UTILS.INDEX_EXISTS(clip_parms.face_input_table,
                                       'SDOGEOMETRY',
                                       'DOMAIN')
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'TRANSFER_ATTRIBUTES',clip_parms.face_input_table,
                                             'Add spatial index to ' || clip_parms.face_input_table || ', this could take a while');

         BEGIN

            GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(clip_parms.face_input_table,
                                           'SDOGEOMETRY',
                                           clip_parms.gen_clip_job_srid,
                                           clip_parms.gen_clip_tolerance);

         EXCEPTION
            WHEN OTHERS
            THEN
               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'TRANSFER_ATTRIBUTES',clip_parms.face_input_table,
                                                  'Add spatial index failed, see SQLERRM-->',NULL,NULL,NULL,NULL,NULL,SQLERRM);

         END;

      ELSE

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'TRANSFER_ATTRIBUTES',clip_parms.face_input_table,
                                            'Looks like spatial index on ' || clip_parms.face_input_table || '.sdogeometry already exists');

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('TRANSFER_ATTRIBUTES: Call BUILD_EDGE_ATTRIBUTE_TABLE ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --For each dealie in left_right_attributes add columns L_DEALIE and R_DEALIE


      --All code here moved to GZ_Utilities
      --Edge table should not pre-exist
      --We will create it and populate left and right fields

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'TRANSFER_ATTRIBUTES',clip_parms.edge_input_table,
                                          'Build edge attribute table and transfer attributes. ZZZZzzzzz...',NULL,NULL,NULL,NULL);

      GZ_CLIP.BUILD_EDGE_ATTRIBUTE_TABLE(p_schema,
                                         p_release,
                                         p_project_id,
                                         edge_table,                       --edge table we are creating
                                         p_topo,
                                         'CLIP',                           --type switch, defunct option
                                         clip_parms.left_right_attributes, --name of reference table
                                         clip_parms.face_feature_mask_col, --usually statefp
                                         'N',                              --do not calculate edge length. No one uses it
                                         clip_parms.gen_clip_tolerance,
                                         clip_parms.face_input_table,
                                         p_project_id || p_jobid );        --log tag = topo



      IF output IS NULL
      THEN

         output := '0';

      END IF;

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'TRANSFER_ATTRIBUTES',clip_parms.edge_input_table,'COMPLETED',start_time);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('TRANSFER_ATTRIBUTES: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END TRANSFER_ATTRIBUTES;




   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------


   FUNCTION ZAP_MODULES (
      p_modules      IN   GZ_TYPES.stringarray
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 5/21/10
      --We encountered a fatal error
      --Reset all the modules to N so we skip over the rest of the processing for this mask

      output      GZ_TYPES.stringarray;


   BEGIN

      FOR i in 1 .. p_modules.COUNT
      LOOP

         output(i) := 'N';

      END LOOP;

      RETURN output;

   END ZAP_MODULES;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION CREATE_EMPTY_TOPOLOGY (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_topo           IN VARCHAR2 DEFAULT NULL,  --state based topo input
      p_validate       IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2
   AS

      --Matt! 3/08/10
      --Corresponds to step 3 of
      --   http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions


      psql           VARCHAR2(4000);
      clip_parms     GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      output         VARCHAR2(4000) := '0';
      start_time     TIMESTAMP;
      newtopo        VARCHAR2(32);
      kount          PLS_INTEGER;


   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_EMPTY_TOPOLOGY',NULL,'STARTING ' || p_mask);

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      -- ie SDZ1AL
      --newtopo := p_project_id || p_jobid;
      newtopo := p_topo_out;

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
      THEN

         GZ_TOPO_UTIL.PURGE_TOPOLOGY(p_schema,newtopo);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EMPTY_TOPOLOGY: Create topology ' || newtopo);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_topo IS NULL
      THEN

         --Can't bring this lower than 6 digits or else loops and edges degenerate
         SDO_TOPO.create_topology(newtopo,
                                  clip_parms.gen_clip_tolerance,
                                  clip_parms.gen_clip_job_srid,
                                  NULL,
                                  NULL,
                                  NULL,
                                  NULL,
                                  clip_parms.gen_clip_snapping_digits);


         --insert universal face
         psql := 'INSERT INTO ' || newtopo || '_FACE$ '
              || 'VALUES (:p1, null, :p2, :p3, null)';

         EXECUTE IMMEDIATE psql USING -1,
                                      sdo_list_type(),
                                      sdo_list_type();

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_EMPTY_TOPOLOGY',newtopo || '_XX',
                                             'COMPLETED ' || p_mask,start_time);

      ELSE


         --state based, copy entier topology

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_EMPTY_TOPOLOGY',
                                            NULL,'STARTING copy of ' || p_topo || ' to ' || newtopo);

         --topo already exists, we just wish to copy it
         BEGIN
            GZ_TOPO_UTIL.copy_topology(p_schema,
                                       p_topo,
                                       p_schema,
                                       newtopo,
                                       'Y', --redundant, should have cleaned up above
                                       p_validate); --Salman says this should never raise an error. Sometimes it does though

         EXCEPTION
            WHEN OTHERS
            THEN

               --cant use real codes for java errors
               IF UPPER(SQLERRM) LIKE '%OUTOFMEMORYERROR%'
               THEN

                  --voodoo java memory
                  GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_EMPTY_TOPOLOGY',
                                                     NULL,'Copy failed, increasing java heap size ');

                  SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(2147483648);

                  GZ_TOPO_UTIL.copy_topology(p_schema,
                                             p_topo,
                                             p_schema,
                                             newtopo,
                                             'Y', --redundant, should have cleaned up above
                                             p_validate); --Salman says this should never raise an error
               ELSE

                  RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

               END IF;

         END;


         --We copied over the face feature table
         --Get rid of it from the topo and drop tables (and any others should they exist)
         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_EMPTY_TOPOLOGY',NULL,
                                          'Removing the face table from ' || newtopo);

         GZ_TOPO_UTIL.DEREGISTER_FEATURE_TABLES(p_schema,newtopo);

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_EMPTY_TOPOLOGY',newtopo || '_XX',
                                            'Copied ' || p_topo,start_time);



         --This is exactly the same as the above, IF
         --Reworking order possibility

         --Can't bring this lower than 6 digits or else loops and edges degenerate
--         SDO_TOPO.create_topology(newtopo,
--                                  clip_parms.gen_clip_tolerance,
--                                  clip_parms.gen_clip_job_srid,
--                                  NULL,
--                                  NULL,
--                                  NULL,
--                                  NULL,
--                                  clip_parms.gen_clip_snapping_digits);
--
--
--         --insert universal face
--         psql := 'INSERT INTO ' || newtopo || '_FACE$ '
--              || 'VALUES (:p1, null, :p2, :p3, null)';
--
--         EXECUTE IMMEDIATE psql USING -1,
--                                      sdo_list_type(),
--                                      sdo_list_type();
--
--         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_EMPTY_TOPOLOGY',newtopo || '_XX',
--                                             'COMPLETED ' || p_mask,start_time);


      END IF;



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EMPTY_TOPOLOGY: Grant public select on ' || newtopo || '$s');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --relation$ doesnt exist at this point

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',newtopo || '_EDGE$');
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',newtopo || '_FACE$');
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',newtopo || '_HISTORY$');



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

   PROCEDURE CLIP_CREATE_TABLE (
      p_schema         IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_table_name     IN VARCHAR2,
      p_sql            IN VARCHAR2,
      p_pkc_col        IN VARCHAR2 DEFAULT NULL,
      p_uqc_col        IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 3/8/10
      --Added unique col parm for Oids 7/10/10

      psql             VARCHAR2(4000);
      v_object_root    VARCHAR2(4000) := p_table_name;  --??

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Create the table using passed sql');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      psql := p_sql;

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
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add constraints and keys to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------

      IF p_pkc_col IS NOT NULL
      AND p_uqc_col IS NULL
      THEN
         --Does EDGE_ID need to be parameterized?  Think this is ok
         EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
                        || 'ADD ('
                        || '   CONSTRAINT ' || v_object_root || 'PKC '
                        || '      PRIMARY KEY(' || p_pkc_col || ') '
                        || ')';



      END IF;

      IF p_uqc_col IS NOT NULL
      AND p_pkc_col IS NULL
      THEN

         EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
                        || 'ADD ('
                        || '   CONSTRAINT ' || v_object_root || 'UQC '
                        || '      UNIQUE(' || p_uqc_col || ') '
                        || ')';


      END IF;

      IF p_uqc_col IS NOT NULL
      AND p_pkc_col IS NOT NULL
      THEN

         EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
                        || 'ADD ('
                        || '   CONSTRAINT ' || v_object_root || 'PKC '
                        || '      PRIMARY KEY(' || p_pkc_col || '), '
                        || '   CONSTRAINT ' || v_object_root || 'UQC '
                        || '      UNIQUE(' || p_uqc_col || ') '
                        || ')';


      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Grant privileges on ' || p_table_name);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);



   END CLIP_CREATE_TABLE;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUblic---------------------------------------------------------------------------------

   FUNCTION BUILD_TOPO_FROM_TOPO (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_jobid              IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_table              IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_topo_type          IN NUMBER,
      p_topo_col           IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN VARCHAR2
   AS

      --I think this is defunct as of september 2012
      --Matt! 10/28/10

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

      SDO_TOPO.add_topo_geometry_layer(p_topo,p_table,p_topo_col,tg_layer_type);

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

      EXECUTE IMMEDIATE psql uSING p_topo,
                                   p_topo_type,
                                   tg_layer_id,
                                   p_topo_type;

      COMMIT;

      --RAISE_APPLICATION_ERROR(-20001,'Peppahs!');


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

   FUNCTION REMOVE_EXTERIOR_EDGES (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_jobid              IN VARCHAR2,
      p_topo               IN VARCHAR2
   ) RETURN VARCHAR2 AS

      --Matt! 10/28/10
      --When working with state based input topos
      --We copy the entire topo, then associate interior edges with our working edge table
      --Must also remove the state edges that got copied over before we add generalized state/shore
      --Dont have to use the more complex GZ_CLIP.delete_an_edge, no features are associated

      psql           VARCHAR2(4000);
      edges          GZ_TYPES.stringarray;
      ez_topo_mgr    NUMBER;
      topomap        VARCHAR2(4000);

   BEGIN

      topomap := p_topo || '_TOPOMAP';

      ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topomap,p_topo,2);

      psql := 'SELECT e.edge_id '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE '
           || '(e.left_face_id = :p1 OR e.right_face_id = :p2) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO edges USING -1, -1; --no limit, should
                                                                   --just be a few hundred edge ids (?)

      FOR i IN 1 .. edges.COUNT
      LOOP

         BEGIN

            SDO_TOPO_MAP.REMOVE_EDGE(NULL,edges(i));

         EXCEPTION
         WHEN OTHERS THEN

            IF SQLERRM LIKE '%is not on the boundary of one or two of the faces it links%'
            THEN
               --copied this handler from delete_an_edge
               --want to die here, cant leave this guys in

               --ORA-29532: Java call terminated by uncaught Java exception: oracle.spatial.topo.TopoValidationException:
               --Edge 2015 is not on the boundary of one or two of the faces it links
               --ORA-06512: at "MDSYS.SDO_TOPO_MAP", line 344
               --I dont know what this is all about

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'REMOVE_EXTERIOR_EDGES ' || p_topo, p_topo || '_EDGE$',
                                          'Got one of the weird ''is not on the boundary'' errors, lets ignore it ' ,
                                           NULL,NULL,NULL,'SDO_TOPO_MAP.REMOVE_EDGE(''' || p_topo || ''',' || edges(i)|| ');',
                                           NULL,SQLERRM);

               RAISE;

            ELSE

               RAISE;

            END IF;

         END;

      END LOOP;


      --clean out topomap and update the dollar tables
      ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topomap,p_topo,ez_topo_mgr);

      RETURN '0';

   END REMOVE_EXTERIOR_EDGES;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   FUNCTION REMOVE_TOUCHY_EDGES (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_jobid              IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_edge_table         IN VARCHAR2,
      p_clip_table         IN VARCHAR2
   ) RETURN VARCHAR2 AS

       --9/10/12 this is no longer called

      --Matt! 12/19/10
      --4/12/12 Added isolated node removal too.  They are interacting with the clip outline
      --   when working with state based input topos
      --9/10/12 Change to small window edge-by-edge topomap to avoid memory errors

      --We copy the entire topo, then associate interior edges with our working edge table
      --Now we take a step backward and remove the interior edges that touch the clip outline


      psql              VARCHAR2(4000);
      ez_topo_mgr       NUMBER;
      topomap           VARCHAR2(4000);
      feature_edges     GZ_TYPES.stringarray;
      primitive_edges   GZ_TYPES.stringarray;
      tg_layer_id       NUMBER;
      nodez             GZ_TYPES.stringarray;

   BEGIN

      topomap := p_topo || '_TOPOMAP';  --not used

      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_topo,
                                                  p_edge_table,
                                                  'TOPOGEOM',
                                                  'LINE');


      --Collect touching feature edges and corresponding primitive edges

      --Reference
      --SELECT /*+ ORDERED */ a.edge_id, e.edge_id
      --FROM
      --MT609CL_GEN_ST_EDGES_HI c,
      --MT609CL_MT609_EDGE a,
      --MT609CL_RELATION$ r,
      --MT609CL_EDGE$ e
      --WHERE
      --r.tg_layer_id = a.topogeom.tg_layer_id AND
      --r.tg_id = a.topogeom.tg_id AND
      --e.edge_id = r.topo_id and
      --sdo_relate(a.sdogeometry, c.sdogeometry, 'mask=ANYINTERACT') = 'TRUE'

      psql := 'SELECT /*+ ORDERED */ a.edge_id, e.edge_id '
           || 'FROM '
           || p_clip_table || ' c, '
           || p_edge_table || ' a, '
           || p_topo || '_RELATION$ r, '
           || p_topo || '_EDGE$ e '
           || 'WHERE '
           || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
           || 'r.tg_id = a.topogeom.tg_id AND '
           || 'e.edge_id = r.topo_id AND '
           || 'SDO_RELATE(a.sdogeometry, c.buf_sdogeometry, :p1) = :p2 '
           || 'GROUP BY a.edge_id, e.edge_id '; --Have to group by.  If a feature edge touches more then one
                                                --clip edge we get dupes


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'REMOVE_TOUCHY_EDGES ' || p_edge_table,
                                         p_edge_table,'Collect touchy edges ' ,
                                         NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql BULK COLLECT INTO feature_edges,
                                               primitive_edges
                                               USING 'mask=ANYINTERACT',
                                                     'TRUE';

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'REMOVE_TOUCHY_EDGES ' || p_edge_table,
                                         p_edge_table,'Removing edges for ' || feature_edges.COUNT || ' records in ' || p_edge_table ,
                                         NULL,NULL,NULL,NULL);

      FOR i in 1 .. feature_edges.COUNT
      LOOP



         GZ_CLIP.DELETE_AN_EDGE(p_schema,
                                p_project_id,
                                p_jobid,
                                p_topo,
                                p_edge_table,
                                feature_edges(i),
                                primitive_edges(i),
                                tg_layer_id,
                                topomap, --not used
                                'Y',   --retain feature, dont delete the feature record
                                'N');  --but do not retain the primitive edge

      END LOOP;


      --Dont forget, we need to mark these guys

      FORALL ii IN 1 .. feature_edges.COUNT
         EXECUTE IMMEDIATE 'UPDATE ' || p_edge_table || ' a '
                        || 'SET a.dangle = :p1 '
                        || 'WHERE a.edge_id = :p2 '
         USING 'T', feature_edges(ii);

         COMMIT;

      --topomap must be updated to key off of node$
      --Loads lots of small explicit topomaps
      GZ_TOPO_UTIL.REMOVE_ISOLATED_NODES_CACHE(p_topo);


      RETURN '0';

   END REMOVE_TOUCHY_EDGES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE ADD_LINES_FROM_SPATIAL (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_jobid              IN VARCHAR2,
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

      --Matt! 8/30/11
      --10/14/11! Nonfunctional updates for out of memory catch and retries (caller handles)

      --Expected Inputs
      --   1. A table (p_featuretable) with
      --      1a. Some sort of primary key column (p_featuretable_pkc) for ex edge_id
      --      1b. A column with geometry, populated and indexed (p_geom_col). No overlaps edge to edge
      --      1c. An updateable column (p_featuretable_id) to receive the new edge id for each add. Ex OID
      --   2. A pre-existing topology (p_toponame) with nothing in it
      --   3. If using the p_topomap_mbr option, must initialize metadata (for the face$ sidx) before calling

      --Output
      --   Uses add_linear_geometry to create edge and face primitives
      --   Populates the plain vanilla p_featuretable_id with the edge_id returned

      --EXAMPLE usage
      --

      ----Then call a constructor, like BUILD_TOPO_FROM_TOPO


      psql                 VARCHAR2(4000);
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
      update_id            PLS_INTEGER := 0;



   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_LINES_FROM_SPATIAL: Verify Inputs ');
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
                                                 2002;

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
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_LINES_FROM_SPATIAL: Initialize topo map ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF p_topomap_mbr IS NULL
      THEN

         --entire topology topomap, can be expensive unless empty (empty for for clip)
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
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_LINES_FROM_SPATIAL: Create features for ' || featuretable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      psql := 'SELECT ' || featuretable_pkc || ', ' || geom_col || ' '
           || 'FROM ' || featuretable || ' ';

      IF p_subset_col IS NOT NULL
      THEN

         psql := psql || ' WHERE ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

      END IF;

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_LINES_FROM_SPATIAL',
                                         featuretable,'Opening cursor to call add_linear_geometry ' ,
                                         NULL,NULL,NULL,psql);


      BEGIN

         OPEN my_cursor FOR psql;

         LOOP

            FETCH my_cursor BULK COLLECT INTO topotab LIMIT 100; --parameterize LIMIT?
            EXIT WHEN topotab.COUNT = 0;

            FOR i in 1 .. topotab.COUNT
            LOOP


               BEGIN


                  --THE CALL----
                  --**********--
                  stupid_number_array := SDO_TOPO_MAP.ADD_LINEAR_GEOMETRY(NULL,topotab(i).sdogeometry);
                  --------------
                  --------------


               EXCEPTION
               WHEN OTHERS
               THEN

                  GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_LINES_FROM_SPATIAL',
                                                     featuretable,'ADD_LINEAR_GEOMETRY error message--> ' ,
                                                     NULL,NULL,NULL,NULL,NULL,SQLERRM);

                  IF (UPPER(SQLERRM) LIKE '%OUTOFMEMORYERROR%'
                  OR UPPER(SQLERRM) LIKE '%JAVA OUT OF MEMORY CONDITION%')  --WTF causes this instead of the first?
                  THEN

                     --nothing in this handler has ever succeeded
                     --attempting to avoid this ahead of time in the caller

                     --The topomap is hosed and uncommitted, none of the primitives are actually added to the topo
                     --but we have potentially updated some feature record oids
                     --but but we didn't save their ids, and they are about to be overwritten on the next pass
                     --but so could probably let it slide, however that seems fishy

                     psql := 'UPDATE ' || featuretable || ' a '
                          || 'SET a.' || featuretable_id || ' = NULL '
                          || 'WHERE a.' || featuretable_id || ' IS NOT NULL ';

                     IF p_subset_col IS NOT NULL
                     THEN

                        psql := psql || 'AND a.' || p_subset_col || ' = ''' || p_subset_val || ''' ';

                     END IF;

                     GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_LINES_FROM_SPATIAL',
                                         featuretable,'NULLing out any ' || featuretable_id || 's we updated in ' || featuretable,
                                         NULL,NULL,NULL,psql);

                     EXECUTE IMMEDIATE psql;
                     COMMIT;


                     GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_LINES_FROM_SPATIAL',
                                                     featuretable,
                                                     'Memory error, gonna call the magick java_memory_manager ',
                                                     NULL,NULL,NULL,NULL,NULL,NULL);


                     BEGIN

                        --this will probably just choke too
                        --SDO_TOPO_MAP.COMMIT_TOPO_MAP();
                        --SDO_TOPO_MAP.DROP_TOPO_MAP(newtopomap);

                        --this is just a placeholder, not expecting it to work at present

                        GZ_BUSINESS_UTILS.JAVA_MEMORY_MANAGER(featuretable,
                                                         'SDOGEOMETRY',
                                                         SQLERRM);  --<-- handle it wizard!

                     EXCEPTION
                     WHEN OTHERS
                     THEN

                        GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_LINES_FROM_SPATIAL',
                                                     featuretable,'JAVA_MEMORY_MANAGER failed to clean house--> ' ,
                                                     NULL,NULL,NULL,NULL,NULL,SQLERRM);

                        RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

                     END;


                  ELSE

                     RAISE;

                  END IF;

               END;


               IF stupid_number_array.COUNT = 1
               THEN

                  --SOP

                  --gotta keep a separate counter in here
                  --since we may (in future rewrite) be skipping elements from the main loop
                  update_id := update_id + 1;

                  face_ids(update_id) := stupid_number_array(1);
                  stupid_number_array.DELETE;
                  featuretable_ids(update_id) := topotab(i).id;

               ELSIF stupid_number_array.COUNT > 1
               THEN

                   --bummer, this input line is creating 2+ output lines
                   --future rewrite: set this bad boy aside for separate treatment
                   --for now, shows over

                   RAISE_APPLICATION_ERROR(-20001,'Got ' || stupid_number_array.COUNT || ' ' ||
                                                  'edges for ' || featuretable || ' ' || featuretable_pkc || ': ' || topotab(i).id);


                   GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_LINES_FROM_SPATIAL',
                                                      featuretable,'Got ' || stupid_number_array.COUNT || ' ' ||
                                                      'edges for ' || featuretable || ' ' || featuretable_pkc || ': ' || topotab(i).id ,
                                                      NULL,NULL,NULL,NULL);

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
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_LINES_FROM_SPATIAL: Commit n Drop topomap ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_LINES_FROM_SPATIAL',
                                         featuretable,'Commit and drop topo map ',
                                         NULL,NULL,NULL,NULL);


      SDO_TOPO_MAP.COMMIT_TOPO_MAP();
      SDO_TOPO_MAP.DROP_TOPO_MAP(newtopomap);

      --JUST SAY NO
      --SDO_TOPO.INITIALIZE_METADATA(toponame);

      ---------------------
      --MITOSIS Manager?
      ---------------------


      --verify that all of the edges we think we got above
      --are actually in the topology and not phantoms

      psql := 'SELECT count(*) FROM ( '
           || 'SELECT ' || p_featuretable_id || ' '
           || 'FROM ' || featuretable || ' ';

      IF p_subset_col IS NOT NULL
      THEN

         psql := psql || ' WHERE ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

      END IF;


      psql := psql || 'MINUS '
                   || 'SELECT edge_id FROM '
                   || p_toponame || '_edge$ '
                   || ') ';

      EXECUTE IMMEDIATE psql INTO kount;

      IF kount != 0
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_LINES_FROM_SPATIAL',
                                         featuretable,'Yo, ' || kount || ' edges in ' || featuretable || ' have no matching edge in edge$ ',
                                         NULL,NULL,NULL,psql);

         RAISE_APPLICATION_ERROR(-20001,'Yo, ' || kount || ' faces in ' || featuretable || ' have no matching edge in edge$ ');

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_LINES_FROM_SPATIALL: Complete ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

       GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_LINES_FROM_SPATIAL',
                                         featuretable,'Done ',
                                         NULL,NULL,NULL,psql);

      --Anything to return?


   END ADD_LINES_FROM_SPATIAL;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public for debug-----------------------------------------------------------------------

   PROCEDURE CONNECT_DANGLE_TO_NN_NODE (
      p_topo               IN VARCHAR2,
      p_source_table       IN VARCHAR2,
      p_source_pkc         IN VARCHAR2,
      p_source_value       IN NUMBER,
      p_tolerance          IN NUMBER,
      p_how_desperate      IN NUMBER DEFAULT 10   --will jump 10 meters
   )
   AS

      --M@! 8/28/13
      --Conditional compiler flags used so that if, god forbid, any of this code
      --is still operational in 12 someone has to see the same buggy behavior happening
      --and come back in here and agree to turn this mess back on
      --dont do it. Heaven is high and the emperor is far away

      --Increasing levels of desperation to work around combinations of
      --"attempt to add an iso node that lies..." and also
      --"attempt to add an edge that ends in different faces"
      --In this case if all else has failed attempt to manually
      --connect a dangle to the nearest node on the state outline

      psql                 VARCHAR2(4000);
      uf_psql              VARCHAR2(4000);
      dangle_geom          SDO_GEOMETRY;
      start_pt             SDO_GEOMETRY;
      end_pt               SDO_GEOMETRY;
      start_node_id        NUMBER;
      end_node_id          NUMBER;
      start_dist           NUMBER;
      end_dist             NUMBER;
      start_node_geom      SDO_GEOMETRY;
      end_node_geom        SDO_GEOMETRY;

   BEGIN

      $IF dbms_db_version.ver_le_11_2
      $then

         psql := 'SELECT a.sdogeometry FROM '
              || p_source_table || ' a '
              || 'WHERE a.' || p_source_pkc || ' = :p1 ';

         EXECUTE IMMEDIATE psql INTO dangle_geom USING p_source_value;

         --1. From both ends of the dangle, find which is closest to a universal node
         --   Hard coded distance of 10m is as far as I want to stretch right now
         --   The case this is attempting to work around in ~9m away

         --start
         start_pt := SDO_GEOMETRY
                  (  2001,
                     dangle_geom.sdo_srid,
                     SDO_POINT_TYPE
                     (
                        dangle_geom.sdo_ordinates(1),
                        dangle_geom.sdo_ordinates(2),
                        NULL
                     ),
                     NULL,
                     NULL
                  );

         --other end
         end_pt := start_pt;
         end_pt.SDO_POINT.X := dangle_geom.sdo_ordinates(dangle_geom.sdo_ordinates.COUNT - 1);
         end_pt.SDO_POINT.Y := dangle_geom.sdo_ordinates(dangle_geom.sdo_ordinates.COUNT);

         uf_psql := 'SELECT node_id, geom, dist FROM ('
                 || 'SELECT /*+ FIRST_ROWS */ n.node_id, n.geometry geom, sdo_nn_distance(1) dist '
                 || 'FROM ' || p_topo || '_node$ n '
                 || 'WHERE '
                 || 'SDO_NN(n.geometry, :p1, ''sdo_batch_size=2 dist=' || p_how_desperate || ''', 1) = :p4 '
                 || 'ORDER BY DIST '
                 || ') WHERE rownum = 1 ';


         BEGIN

            EXECUTE IMMEDIATE uf_psql INTO start_node_id,
                                           start_node_geom,
                                           start_dist USING start_pt,
                                                            'TRUE';

         EXCEPTION
         WHEN NO_DATA_FOUND
         THEN

            start_node_id := NULL;

         WHEN OTHERS
         THEN

            RAISE;

         END;

         IF start_node_id IS NOT NULL
         AND (start_dist > p_how_desperate  --just in case sdo_nn wack
         OR NOT GZ_TOPOFIX.IS_NODE_UNIVERSAL(p_topo, start_node_id))
         THEN

            start_node_id := NULL;

         END IF;

         BEGIN

            EXECUTE IMMEDIATE uf_psql INTO end_node_id,
                                           end_node_geom,
                                           end_dist USING end_pt,
                                                          'TRUE';

         EXCEPTION
         WHEN NO_DATA_FOUND
         THEN

            end_node_id := NULL;

         WHEN OTHERS
         THEN

            RAISE;

         END;

         IF end_node_id IS NOT NULL
         AND (end_dist > p_how_desperate  --just in case sdo_nn wack
         OR NOT GZ_TOPOFIX.IS_NODE_UNIVERSAL(p_topo, end_node_id))
         THEN

            end_node_id := NULL;

         END IF;

         IF start_node_id IS NULL
         AND end_node_id IS NULL
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Investigate, cant find a close UF node');

         ELSIF start_node_id IS NOT NULL
         AND end_node_id IS NOT NULL
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Investigate, this BS workaround expects edges that have just one end near a line of latitude on the UF');

         ELSIF start_node_id IS NOT NULL
         THEN

            --replace start with exact node
            dangle_geom.sdo_ordinates(1) := start_node_geom.sdo_point.X;
            dangle_geom.sdo_ordinates(2) := start_node_geom.sdo_point.Y;

         ELSE

            dangle_geom.sdo_ordinates(dangle_geom.sdo_ordinates.COUNT - 1) := end_node_geom.sdo_point.X;
            dangle_geom.sdo_ordinates(dangle_geom.sdo_ordinates.COUNT)     := end_node_geom.sdo_point.Y;

         END IF;

         --dbms_output.put_line(gz_geom_utils.dump_sdo(dangle_geom));

         psql := 'UPDATE ' || p_source_table || ' a '
              || 'SET a.sdogeometry = :p1 '
              || 'WHERE a.' || p_source_pkc || ' = :p2 ';

         EXECUTE IMMEDIATE psql USING dangle_geom,
                                      p_source_value;

         COMMIT;

         --caller retries the edge add

      $else

         --version 12 and later
         RAISE_APPLICATION_ERROR(-20001, 'Connect_dangle_to_nn_node is intended to work around bugs in 11.2.04 only');

      $end


   END CONNECT_DANGLE_TO_NN_NODE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public for debug-----------------------------------------------------------------------

   PROCEDURE ADD_NODES_AROUND_DANGLE(
      p_topo               IN VARCHAR2,
      p_source_table       IN VARCHAR2,
      p_source_pkc         IN VARCHAR2,
      p_source_value       IN NUMBER,
      p_tolerance          IN NUMBER
   )
   AS

      --M@! 8/8/13
      --Basically a hard coded workaround for an add_linear_geometry interior edge
      --that throws:
      --"Attempted to add an iso node that lies on an existing edge or node"
      --This is the only workaround that I could come up with, after ~10 workarounds failed
      --I'm embrassed to be associated with this, hopefully no one ever looks at it, including me, after 8/8/13

      --Conditional compiler flags used so that if, god forbid, any of this code
      --is still operational in 12 someone has to see the same buggy behavior unfixed
      --and come back in here and agree to turn this mess back on

      --1. From both ends of the dangle, find which is closest to a universal edge
      --2. Verify that that distance is tiny.  If big bail out, dont want this called on anything untested
      --3. Use null geom srids and project from the dangle to the UF edge
      --4. Determine the coord index of that location
      --5. Turn a vtx into a node at the start of that segment
      --6. Commit topomap
      --7. Determine the new closest edge id and coord index
      --8. Should be 0 or last.  If 0, convert node to vtx at coord index 1. If last at ordkount/2-1

      psql                 VARCHAR2(4000);
      uf_psql              VARCHAR2(4000);
      dangle_geom          SDO_GEOMETRY;
      edge_geom            SDO_GEOMETRY;
      coord_index          NUMBER;
      start_pt             SDO_GEOMETRY;
      start_edge_id        NUMBER;
      start_dist           NUMBER;
      end_pt               SDO_GEOMETRY;
      end_edge_id          NUMBER;
      end_dist             NUMBER;
      the_uf_edge_id       NUMBER;
      the_uf_edge_geom     SDO_GEOMETRY;
      the_uf_edge_mbr      SDO_GEOMETRY;
      the_dangle_pt        SDO_GEOMETRY;
      tolerance            NUMBER;
      project_pt_geom      SDO_GEOMETRY;
      start_seg_pt         SDO_GEOMETRY;
      end_seg_pt           SDO_GEOMETRY;
      create_it            NUMBER;
      new_node_id          NUMBER;
      final_uf_edge_id     NUMBER;

   BEGIN

      $IF dbms_db_version.ver_le_11_2
      $then

         psql := 'SELECT a.sdogeometry FROM '
              || p_source_table || ' a '
              || 'WHERE a.' || p_source_pkc || ' = :p1 ';

         EXECUTE IMMEDIATE psql INTO dangle_geom USING p_source_value;

         --1. From both ends of the dangle, find which is closest to a universal edge

         --start
         start_pt := SDO_GEOMETRY
                  (  2001,
                     dangle_geom.sdo_srid,
                     SDO_POINT_TYPE
                     (
                        dangle_geom.sdo_ordinates(1),
                        dangle_geom.sdo_ordinates(2),
                        NULL
                     ),
                     NULL,
                     NULL
                  );

         --other end
         end_pt := start_pt;
         end_pt.SDO_POINT.X := dangle_geom.sdo_ordinates(dangle_geom.sdo_ordinates.COUNT - 1);
         end_pt.SDO_POINT.Y := dangle_geom.sdo_ordinates(dangle_geom.sdo_ordinates.COUNT);

         uf_psql := 'SELECT edge_id, dist FROM ('
                 || 'SELECT /*+ FIRST_ROWS */ e.edge_id, sdo_nn_distance(1) dist '
                 || 'FROM ' || p_topo || '_edge$ e '
                 || 'WHERE (e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
                 || 'SDO_NN(e.geometry, :p3, ''sdo_batch_size=2 dist=100'', 1) = :p4 '
                 || 'ORDER BY DIST '
                 || ') WHERE rownum = 1 ';

         --dbms_output.put_line(gz_geom_utils.dump_sdo(start_pt));
         --dbms_output.put_line(uf_psql);

         BEGIN

            EXECUTE IMMEDIATE uf_psql INTO start_edge_id,
                                           start_dist USING -1, -1,
                                                            start_pt,
                                                            'TRUE';

         EXCEPTION
         WHEN NO_DATA_FOUND
         THEN

            start_edge_id := NULL;
            start_dist    := 999999999999999999;

         WHEN OTHERS
         THEN

            RAISE;

         END;

         --dbms_output.put_line(gz_geom_utils.dump_sdo(end_pt));
         --dbms_output.put_line(uf_psql);

         BEGIN

            EXECUTE IMMEDIATE uf_psql INTO end_edge_id,
                                           end_dist USING -1, -1,
                                                          end_pt,
                                                          'TRUE';

         EXCEPTION
         WHEN NO_DATA_FOUND
         THEN

            end_edge_id := NULL;
            end_dist    := 999999999999999999;

         WHEN OTHERS
         THEN

            RAISE;

         END;



         IF start_edge_id IS NULL
         AND end_edge_id IS NULL
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Investigate, cant find a close edge');

         ELSIF start_dist > 10
         AND end_dist > 10
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Investigate, this workaround is expected for edges that throw errors because they touch lines of latitude');

         ELSIF start_dist < end_dist
         THEN

            the_dangle_pt := start_pt;
            the_uf_edge_id   := start_edge_id;

         ELSE

            the_dangle_pt  := end_pt;
            the_uf_edge_id := end_edge_id;

         END IF;

         psql := 'SELECT e.geometry, sdo_geom.sdo_mbr(e.geometry) FROM '
              || p_topo || '_edge$ e '
              || 'WHERE e.edge_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO the_uf_edge_geom,
                                     the_uf_edge_mbr USING the_uf_edge_id;

         --3. Use null geom srids and project from the dangle to the UF edge

         tolerance := GZ_CLIP.TOLERANCE_CONVERTER(p_tolerance,
                                                  TO_CHAR(the_uf_edge_geom.sdo_srid),
                                                  'NULL');

         the_uf_edge_geom.sdo_srid := NULL;
         the_dangle_pt.sdo_srid := NULL;

         project_pt_geom := GZ_CLIP.GZ_PROJECT_PT(the_dangle_pt, the_uf_edge_geom, tolerance);

         coord_index := GZ_CLIP.ACCURATE_FIND_COORD_INDEX(project_pt_geom,the_uf_edge_geom);

         the_uf_edge_geom.sdo_srid := dangle_geom.sdo_srid;

         --5. Turn a vtx into a node at the start of that segment
         --   save the other point while we've got it

         start_seg_pt := SDO_GEOMETRY
                        (  2001,
                           dangle_geom.sdo_srid,
                           SDO_POINT_TYPE
                           (
                              the_uf_edge_geom.sdo_ordinates((coord_index + 1) * 2 - 1),
                              the_uf_edge_geom.sdo_ordinates((coord_index + 1) * 2),
                              NULL
                           ),
                           NULL,
                           NULL
                        );

         end_seg_pt := SDO_GEOMETRY
                        (  2001,
                           dangle_geom.sdo_srid,
                           SDO_POINT_TYPE
                           (
                              the_uf_edge_geom.sdo_ordinates((coord_index + 2) * 2 - 1),
                              the_uf_edge_geom.sdo_ordinates((coord_index + 2) * 2),
                              NULL
                           ),
                           NULL,
                           NULL
                        );

         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                      p_topo,
                                                      2,
                                                      the_uf_edge_mbr.sdo_ordinates(1),
                                                      the_uf_edge_mbr.sdo_ordinates(2),
                                                      the_uf_edge_mbr.sdo_ordinates(3),
                                                      the_uf_edge_mbr.sdo_ordinates(4));

         new_node_id := SDO_TOPO_MAP.ADD_NODE(NULL,
                                              the_uf_edge_id, --edge id
                                              start_seg_pt,
                                              coord_index,
                                              'N'); --not new shape point

         --6. Commit topomap
         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

         --7. Determine the new closest edge id and coord index

         the_dangle_pt.sdo_srid := dangle_geom.sdo_srid;

         EXECUTE IMMEDIATE uf_psql INTO final_uf_edge_id,
                                        start_dist USING -1, -1,
                                                         the_dangle_pt,
                                                         'TRUE';

         psql := 'SELECT e.geometry, sdo_geom.sdo_mbr(e.geometry) FROM '
              || p_topo || '_edge$ e '
              || 'WHERE e.edge_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO the_uf_edge_geom,
                                     the_uf_edge_mbr USING final_uf_edge_id;

         --AGAIN Use null geom srids and project from the dangle to the UF edge

         the_uf_edge_geom.sdo_srid := NULL;
         the_dangle_pt.sdo_srid := NULL;

         project_pt_geom := GZ_CLIP.GZ_PROJECT_PT(the_dangle_pt, the_uf_edge_geom, tolerance);

         coord_index := GZ_CLIP.ACCURATE_FIND_COORD_INDEX(project_pt_geom,the_uf_edge_geom);

         --8. Should be 0 or last.  If 0, convert node to vtx at coord index 1. If last at ordkount/2-1

         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                      p_topo,
                                                      2,
                                                      the_uf_edge_mbr.sdo_ordinates(1),
                                                      the_uf_edge_mbr.sdo_ordinates(2),
                                                      the_uf_edge_mbr.sdo_ordinates(3),
                                                      the_uf_edge_mbr.sdo_ordinates(4));

         IF coord_index = 0
         THEN

            new_node_id := SDO_TOPO_MAP.ADD_NODE(NULL,
                                                 final_uf_edge_id, --edge id
                                                 end_seg_pt,
                                                 coord_index + 1,
                                                 'N'); --not new shape point

         ELSIF ((coord_index + 2) * 2) = the_uf_edge_geom.sdo_ordinates.COUNT
         THEN

            new_node_id := SDO_TOPO_MAP.ADD_NODE(NULL,
                                                 final_uf_edge_id, --edge id
                                                 end_seg_pt,
                                                 (the_uf_edge_geom.sdo_ordinates.COUNT/2 - 1),
                                                 'N'); --not new shape point

         ELSE

            RAISE_APPLICATION_ERROR(-20001, 'The only solution is Happy Hour');

         END IF;


         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

      $else

         --Oracle 12
         RAISE_APPLICATION_ERROR(-20001,'Add_nodes_around_dangle is intended to work around bugs in 11.2.0.4 only');

      $end


   END ADD_NODES_AROUND_DANGLE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------


   FUNCTION ADD_INTERIOR_LINES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_topo           IN VARCHAR2 DEFAULT NULL,  --State based input
      p_add_linear     IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2
   AS

      --Matt! 3/08/10
      --Corresponds to step 4 of
      --   http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions
      --Added state based steps 10/28/10


      psql              VARCHAR2(4000);
      psql2             VARCHAR2(4000);
      clip_parms        GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      output            VARCHAR2(4000) := '0';
      start_time        TIMESTAMP;
      newedgetable      VARCHAR2(32);
      newtopo           VARCHAR2(32);
      srid              NUMBER;
      layerkount        PLS_INTEGER;
      retval            VARCHAR2(1);
      newcliptable      VARCHAR2(32);
      shared_edges      GZ_TYPES.stringarray;
      kount             PLS_INTEGER;
      single_edges      GZ_TYPES.numberarray;
      edge_mbrs         GZ_TYPES.geomarray;

   BEGIN


      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',NULL,'STARTING ' || p_mask);

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      --set up basic names
      --ex Z601CL_Z699IN_EWRK
      newedgetable := GZ_CLIP.GET_EDGE_TABLE(p_project_id,p_jobid,clip_parms.edge_input_table);

      newcliptable := p_project_id || p_jobid || '_' || clip_parms.gen_clip_table;

      -- ie SDZ1AL
      newtopo := p_topo_out;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_INTERIOR_LINES: Create feature table ' || newedgetable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --First find out if job SRID matches input SRID

      srid := GZ_TOPO_UTIL.GET_SDO_TOPO_METADATA_NUM(clip_parms.gen_topology_name,
                                                     clip_parms.face_input_table,   --face is the only one we know exists
                                                     'srid');

     -- I think this is correct
     -- Create with both geometry and empty topo
     -- Topo gets populated in a bit

     GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,'STARTING: Create table ' || newedgetable);

      psql := 'CREATE TABLE ' || newedgetable || ' NOPARALLEL NOLOGGING '
           || 'AS '
           || 'SELECT '
           || 'CAST(NULL AS NUMBER) OID, '
           || 'e.*, ';

      IF srid = clip_parms.gen_clip_job_srid
      AND p_topo IS NULL
      THEN

         --building topo from spatial, national usually, bring over as, but lets round since we can
         --psql := psql || 'GZ_CLIP.ORDINATE_ROUNDER(t.geometry,6) SDOGEOMETRY, ';
         psql := psql || 't.geometry SDOGEOMETRY, ';

      ELSIF srid != clip_parms.gen_clip_job_srid
      AND p_topo IS NULL
      THEN

         --building topo from spatial, national.  Must project.  Round since we can
         --psql := psql || 'GZ_CLIP.ORDINATE_ROUNDER(SDO_CS.TRANSFORM(t.geometry,'
                      --|| clip_parms.gen_clip_tolerance || ',' || clip_parms.gen_clip_job_srid || ') ,6) SDOGEOMETRY, ';
         psql := psql || 'SDO_CS.TRANSFORM(t.geometry,'
                      || clip_parms.gen_clip_tolerance || ',' || clip_parms.gen_clip_job_srid || ') SDOGEOMETRY, ';

      ELSIF p_topo IS NOT NULL
      AND srid = clip_parms.gen_clip_job_srid
      THEN

         --Building topo from topo, state based. Cant change geom in any way, we are taking primitive values as is
         psql := psql || 't.geometry SDOGEOMETRY, ';

         --Forget this...
         --No more, back to create feature, round since we can
         --tecnhically we could probably project too
         --psql := psql || 'GZ_CLIP.ORDINATE_ROUNDER(t.geometry,6) SDOGEOMETRY, ';

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Yo, we cant (currently) copy a topology with SRID of ' || srid
                             || ' to this job with SRID of ' || clip_parms.gen_clip_job_srid || ' ');


      END IF;

      psql := psql
           || 'CAST(NULL AS SDO_TOPO_GEOMETRY) TOPOGEOM, '
           || 'CAST(NULL AS VARCHAR2(4000)) DANGLE '  --May need this later
           || 'FROM '
           || clip_parms.edge_input_table || ' e, '
           || clip_parms.gen_topology_name || '_edge$ t '
           || 'WHERE e.' || clip_parms.face_feature_mask_col || ' = ' || p_mask || ' AND '  --cant skip this on state based
           || 'e.edge_id = t.edge_id ';                                             --NULL statefp are edge of universe. No want

           --No bind vars allowed in DDL

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,'Create ' || newedgetable,
                                          NULL,NULL,NULL,psql);

      --private procedure
      CLIP_CREATE_TABLE(p_schema,
                        p_jobid,
                        newedgetable,
                        psql,
                        'EDGE_ID',  --primary key populated at the moment
                        'OID');     --unique key NULL till below.

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 15');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_INTERIOR_LINES: Remove duplicate vertices from ' || newedgetable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --remove dups
      --Do not round ordinates in this call though

      IF p_topo IS NULL
      THEN

         --we are building topo from this very geometry (national source topology usually)
         --so clean it up
         --I dont think this ever does anything, but just in case, CYA
         GZ_CLIP.CLEAN_UP_GEOMETRIES(p_schema, newedgetable, 2002);

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_INTERIOR_LINES: Spatial index ' || newedgetable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                         'Spatially index ' || newedgetable,NULL,NULL,NULL,psql);


      GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(newedgetable,
                                     'SDOGEOMETRY',
                                     clip_parms.gen_clip_job_srid,
                                     clip_parms.gen_clip_tolerance);


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_INTERIOR_LINES: Build topo for ' || newedgetable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_topo IS NULL
      AND p_add_linear IS NULL  --never
      THEN

         --THIS section is defunct, used create feature
         RAISE_APPLICATION_ERROR(-20001,'Bridge out ahead');

      ELSIF p_topo IS NULL   --topo would be populated with state input topology
      AND p_add_linear = 'Y'
      THEN

         --THIS IS SOP, 2012 going forward I hope
         --Use add_linear_geometry

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                                 'STARTING: Calling add_topo_from_spatial for ' || newedgetable);

         --Exadata, b*tches
         SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(2147483648);

         --Call the all purpose wrapper
         --will add geometries to the topo, register the table, and use constructors
         --Im not sure why this doesnt run out of java memory sometimes

         --if this throws memory errors consider either tiles, or
         -- an error handler with a dumb edge by edge "subset" loop. Would be slow for the unlucky ones

         BEGIN

            GZ_TOPO_UTIL.ADD_TOPO_FROM_SPATIAL(newtopo,
                                               newedgetable,
                                               'EDGE_ID',
                                               'LINE',
                                               'CLIP',      --generic logger
                                               'SDOGEOMETRY',
                                               NULL,         --no subset
                                               NULL,
                                               'Y',          --yes allow splits. Mad intersects with state outline, went in first
                                               'Y');         --yes new layer

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%SINGLE RECORD MODE%'
            THEN

               --there was an error hosing the topo map.  Its not recoverable
               --usually this happens due to Oracle bugs preventing the interior lines
               --from snapping correctly to lines of latitude
               --process line by line to allow for slow but nimble response

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                                       'Going into single record mode add_topo_from_spatial for ' || newedgetable);

               --add_topo_from_spatial subset mode assumes this is done
               SDO_TOPO.ADD_TOPO_GEOMETRY_LAYER(newtopo,newedgetable,'TOPOGEOM','LINE');
               SDO_TOPO.INITIALIZE_METADATA(newtopo);

               psql := 'SELECT edge_id, sdo_geom.sdo_mbr(sdogeometry) FROM ' || newedgetable;

               --hopefully not hundreds of thousands, and mbrs better than sdo
               EXECUTE IMMEDIATE psql BULK COLLECT INTO single_edges,
                                                        edge_mbrs;

               FOR i IN 1 .. single_edges.COUNT
               LOOP

                  --this will make for very verbose logging out of add_topo_from_spatial
                  --should be pretty noticeable
                  --also slow

                  BEGIN

                     GZ_TOPO_UTIL.ADD_TOPO_FROM_SPATIAL(newtopo,
                                                        newedgetable,
                                                        'EDGE_ID',
                                                        'LINE',
                                                        'CLIP',            --generic logger
                                                        'SDOGEOMETRY',
                                                        'EDGE_ID',         --primary key
                                                        single_edges(i),   --single edge
                                                        'Y',               --yes allow splits. Mad intersects with state outline, went in first
                                                        'N',               --not a new layer, added above
                                                         edge_mbrs(i));

                  EXCEPTION
                  WHEN OTHERS
                  THEN

                     IF SQLERRM LIKE '%SINGLE RECORD MODE%'  --attempt to add iso node
                     THEN

                        GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                                                'On ' || single_edges(i) || ' calling ADD_NODES_AROUND_DANGLE, '
                                                                || 'same error in single record mode add_topo_from_spatial for ' || newedgetable);

                        --expect to catch the same error in single edge mode. It like to got dealt with

                        --workaround number 1.  Convert vertices to nodes on the universal that are near this dangle
                        --this is harmless
                        GZ_CLIP.ADD_NODES_AROUND_DANGLE(newtopo,
                                                        newedgetable,
                                                        'EDGE_ID',
                                                        single_edges(i),
                                                        clip_parms.gen_clip_tolerance);

                         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                                                 'Trying edge ' || single_edges(i) || ' again ');

                        BEGIN

                           GZ_TOPO_UTIL.ADD_TOPO_FROM_SPATIAL(newtopo,
                                                              newedgetable,
                                                              'EDGE_ID',
                                                              'LINE',
                                                              'CLIP',            --generic logger
                                                              'SDOGEOMETRY',
                                                              'EDGE_ID',         --primary key
                                                              single_edges(i),   --single edge
                                                              'Y',               --yes allow splits. Mad intersects with state outline, went in first
                                                              'N',               --not a new layer, added above
                                                               edge_mbrs(i));

                        EXCEPTION
                        WHEN OTHERS
                        THEN

                           --It's turtles all the way down!
                           IF SQLERRM LIKE '%SINGLE RECORD MODE%'
                           OR SQLERRM LIKE '%add an edge that ends in different faces%'
                           THEN

                              --converting vtxs to nodes near the dangle didnt work
                              --next awful plan is to manually switch out the final coordinate pair
                              --in the dangle with the close node.  10 meters is the limit for stretching
                              --This is of course a terrible and potentially harmful idea, but slightly better
                              --than stopping production and editing vertices by hand

                              GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                                                      'On ' || single_edges(i) || ' got error ' || SQLERRM || ', '
                                                                      || 'Calling connect_dangle_to_nn_node ');

                              GZ_CLIP.CONNECT_DANGLE_TO_NN_NODE(newtopo,
                                                                newedgetable,
                                                                'EDGE_ID',
                                                                single_edges(i),
                                                                clip_parms.gen_clip_tolerance,
                                                                10); --willing to connect 10 meters away

                              GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                                                      'Trying edge ' || single_edges(i) || ' again ');

                              --one more time!  No handler
                              GZ_TOPO_UTIL.ADD_TOPO_FROM_SPATIAL(newtopo,
                                                              newedgetable,
                                                              'EDGE_ID',
                                                              'LINE',
                                                              'CLIP',            --generic logger
                                                              'SDOGEOMETRY',
                                                              'EDGE_ID',         --primary key
                                                              single_edges(i),   --single edge
                                                              'Y',               --yes allow splits. Mad intersects with state outline, went in first
                                                              'N',               --not a new layer, added above
                                                               edge_mbrs(i));

                           ELSE

                              RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

                           END IF;

                        END;

                     ELSE

                        RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

                     END IF;


                  END;

               END LOOP;

            ELSE

               RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;


         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                            'COMPLETE: Called ADD_TOPO_FROM_SPATIAL for ' || newedgetable);


         SDO_TOPO.INITIALIZE_METADATA(newtopo);


      ELSE

         --Stopped supporting state input topos fall 2012
         RAISE_APPLICATION_ERROR(-20001,'Unsupported configuration. No state-based topology inputs');


         --This is probably a state based source topology
         --we will use topology constructors to associate feature edges with their edge$ counterparts


         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                          'STARTING: Calling BUILD_TOPO_FROM_TOPO constructorfest for ' || newedgetable);

         --This call will add the new edge table to the topology (first one we want)
         --In the create table statement above we made sure to only include records for non-state outline feature edges

         --Contruct topogeom for all edge features we've got.  They are interior to the state only edges
         retval := GZ_CLIP.BUILD_TOPO_FROM_TOPO(p_schema,
                                                p_project_id,
                                                p_jobid,
                                                newtopo,
                                                newedgetable,
                                                'EDGE_ID',    --copy means edge id = edge id
                                                2);

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                            'COMPETE: Called BUILD_TOPO_FROM_TOPO constructorfest for ' || newedgetable);

         --this is where we delete exterior, universal face facing edges
         --They are not associated with a feature edge, we just inherited them when we copied the topo
         --Do not need them
         --Also do not want them to interfere with state outline

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                            'Starting: Calling remove_exterior_edges for ' || newedgetable);

         retval := GZ_CLIP.REMOVE_EXTERIOR_EDGES(p_schema,
                                                 p_project_id,
                                                 p_jobid,
                                                 newtopo);

          GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                             'Complete: Called remove_exterior_edges for ' || newedgetable);
         --We also need to remove all feature edges that touch the state outline
         --If they are entrenched in the topo the state outline shifts around, we need it to be exact

         --Actually lets wait till we have the geometry for the state outline created, next module



      END IF;



      --We are now basically fully populated
      --Lets gather stats on relation$
      --we probably also did this in the previous module, add_clip_mask, but what the what
      --GZ_UTILITIES.GATHER_TABLE_STATS(newtopo || '_RELATION$');

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                         'Calling GZ_TOPO_TUNE_UP on ' || newtopo);

      GZ_TOPO_UTIL.GZ_TOPO_TUNE_UP(newtopo);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_INTERIOR_LINES: Verify topo direction built from ' || newcliptable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      psql := 'SELECT e.edge_id '
           || 'FROM '
           || newcliptable || ' a, '
           || newtopo || '_RELATION$ r, '
           || newtopo || '_EDGE$ e '
           || 'WHERE '
           || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
           || 'r.tg_id = a.topogeom.tg_id AND '
           || 'e.edge_id = ABS(r.topo_id) AND '  --State edges may have negative topo_ids
           || 'e.edge_id IN ( '                  --since they went in second
           || '   SELECT ee.edge_id '
           || '   FROM '
           || '   ' || newedgetable || ' aa, '
           || '   ' || newtopo || '_RELATION$ rr, '
           || '   ' || newtopo || '_EDGE$ ee '
           || '   WHERE '
           || '   rr.tg_layer_id = aa.topogeom.tg_layer_id AND '
           || '   rr.tg_id = aa.topogeom.tg_id AND '
           || '   ee.edge_id = ABS(rr.topo_id) '      --I dont think the initial topo should have negative topo ids
           || '   ) ';                                --But just in case


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES ' || p_mask,newcliptable,'Find shared edges ',
                                          NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql BULK COLLECT INTO shared_edges;

      FOR i in 1 .. shared_edges.COUNT
      LOOP

         --lets log these in case we need them
         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES ' || p_mask,newcliptable ,
                                             'WARNING: Found this edge shared by ' || newcliptable
                                              || ' and ' ||  newedgetable || ': ' || shared_edges(i));

      END LOOP;

      --Maybe interior edges also changed direction when we added the clip to topo
      --When interior edges went into the topo first, this was never ever an issue.
      --If interior edges go into topo on top of the state clip they can get reversed
      --We can handle this, they will get tossed in module 5

      psql := 'SELECT count(*) FROM '
            || newedgetable || ' a, '
            || newtopo || '_RELATION$ r '
            || 'WHERE '
            || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
            || 'r.tg_id = a.topogeom.tg_id AND '
            || 'r.topo_id < :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING -1;

      IF kount > 0
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES ' || p_mask,newcliptable ,
                                          'WARNING: Found ' || kount || ' edges in ' || newedgetable ||
                                          ' with reversed direction ');
      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_INTERIOR_LINES: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',newedgetable,
                                         'Complete ADD_INTERIOR_LINES for ' || newtopo);

      --Not really sure what errors to be trapping and returning...?
      RETURN output;


   END ADD_INTERIOR_LINES;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------


   FUNCTION ADD_CLIP_MASK (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_clip_edge_tab  IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_topo_in        IN VARCHAR2 DEFAULT NULL  --state topo usually, if not null
   ) RETURN VARCHAR2
   AS

      --Matt! 3/10/10
      --Corresponds to step 5 of
      --   http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions
      --Now its module 2

      --Many updates for state based reordering Matt! 12/10
      --4/27/12 call to ADD_TOPO_FROM_SPATIAL generic utility
      --09/06/12 Increased tunnel buffer to 500M from 100M.  Some Z9 lines across long state outlines
      --         show no interaction in geodetic but add_linear_geometry sees it. PARAMETERIZE BUDDY
      --09/10/12 Overhaul for memory errors. Moved to module 2. Buffer not used any more
      --10/25/12 Added option for NULL clip edge table, meaning transfer input topo state outline edges here


      psql           VARCHAR2(4000);
      psql2          VARCHAR2(4000);
      clip_parms     GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      output         VARCHAR2(4000) := '0';
      start_time     TIMESTAMP;
      newcliptable   VARCHAR2(32);
      newedgetable   VARCHAR2(32);
      newtopo        VARCHAR2(32);
      newtopomap     VARCHAR2(4000);
      srid           NUMBER;
      clip_ids       GZ_TYPES.stringarray;
      --shared_edges   GZ_TYPES.stringarray;
      kount          PLS_INTEGER;
      loopcheck      VARCHAR2(4000);
      retval         VARCHAR2(4000);
      tunnel_buffer  NUMBER := 500;  -- buffer still created but not used
      clip_edge_tab  VARCHAR2(32);
      sdo_col        VARCHAR2(32);
      mask_col       VARCHAR2(32);


   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK',NULL,'STARTING ' || p_mask);

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      --ex Z601CL_Z699IN_EWRK
      newedgetable := GZ_CLIP.GET_EDGE_TABLE(p_project_id,p_jobid,clip_parms.edge_input_table);
      newcliptable := p_project_id || p_jobid || '_' || clip_parms.gen_clip_table; --our task at hand

      -- ie SDZ1AL
      newtopo := p_topo_out;
      newtopomap := newtopo || '_TOPOMAP';



      IF p_clip_edge_tab IS NOT NULL
      THEN

         --SOP
         clip_edge_tab := p_clip_edge_tab;

         psql := 'SELECT a.sdogeometry.sdo_srid '
              || 'FROM ' || clip_edge_tab || ' a '
              || 'WHERE rownum = 1 ';

         EXECUTE IMMEDIATE psql INTO srid;

         --source is a generalized state outline table
         sdo_col := 'SDOGEOMETRY';
         mask_col := clip_parms.clip_edge_mask_col;

      ELSE

         --Allow for special "attribute clip" where we get passed a null state edges table
         --We will use the xx_ewrk table as the source for the outline with special query
         clip_edge_tab := clip_parms.edge_input_table;

         psql := 'SELECT e.geometry.sdo_srid '
              || 'FROM ' || clip_parms.gen_topology_name || '_edge$ e '
              || 'WHERE rownum = 1 ';

         EXECUTE IMMEDIATE psql INTO srid;

         --source is an edge$ table from input topo and face input table
         sdo_col := 'GEOMETRY';
         mask_col := clip_parms.face_feature_mask_col;

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_CLIP_MASK: Create feature table ' || newcliptable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK',newcliptable,'STARTING: Create table ' || newcliptable);



      --I dont much like this method of table creation
      psql := 'CREATE TABLE ' || newcliptable || ' NOPARALLEL NOLOGGING '
           || 'AS SELECT '
           || 'CAST(NULL AS NUMBER) OID, '
           || 'CAST(rownum AS NUMBER) ID, ';

      IF clip_parms.gen_clip_job_srid != srid
      THEN

         --transform to working SRID

         --hard coded buffer guesses for now
         --must buffer fairly large, can't just do a few meters because
         --"When these functions are used on data with geodetic coordinates,
         --they internally perform the operations in an implicitly generated
         --local-tangent-plane Cartesian coordinate system and then transform
         --the results to the geodetic coordinate system"
         --100M is my best guess right now

         --9/10/12 buffer still created but not used. State outline goes into topo first. End of story. Final answer

         psql := psql || 'SDO_CS.TRANSFORM(t.' || sdo_col || ','
                      || clip_parms.gen_clip_tolerance || ',' || clip_parms.gen_clip_job_srid || ') SDOGEOMETRY, '
                      || 'CAST(NULL AS SDO_GEOMETRY) BUF_SDOGEOMETRY, ';

                      --kill buffer for now.  Was used in remove_touchy_edges
                      --|| 'SDO_GEOM.SDO_BUFFER(SDO_CS.TRANSFORM('
                      --|| 't.' || sdo_col || ', ' || clip_parms.gen_clip_tolerance || ', ' || clip_parms.gen_clip_job_srid || '),'|| tunnel_buffer || ',.05,''unit=m'') BUF_SDOGEOMETRY, ';

      ELSE

         psql := psql || 't.' || sdo_col || ' SDOGEOMETRY, '
                      || 'CAST(NULL AS SDO_GEOMETRY) BUF_SDOGEOMETRY, ';

                      --kill buffer for now.  Was used in remove_touchy_edges
                      --|| 'SDO_GEOM.SDO_BUFFER('
                      --|| 't.' || sdo_col || ',' || tunnel_buffer || ',.05,''unit=m'') BUF_SDOGEOMETRY, ';

      END IF;

      IF p_clip_edge_tab IS NOT NULL
      THEN

         psql := psql
              || 'CAST(t.l_' || mask_col || ' AS VARCHAR2(4000)) L_' || mask_col || ', '
              || 'CAST(t.r_' || mask_col || ' AS VARCHAR2(4000)) R_' || mask_col || ', '
              || 'CAST(NULL AS SDO_TOPO_GEOMETRY) TOPOGEOM '
              || 'FROM ';

      ELSE

         psql := psql
              || 'CAST(e.l_' || mask_col || ' AS VARCHAR2(4000)) L_' || mask_col || ', '  --2 tables, 2 aliases
              || 'CAST(e.r_' || mask_col || ' AS VARCHAR2(4000)) R_' || mask_col || ', '
              || 'CAST(NULL AS SDO_TOPO_GEOMETRY) TOPOGEOM '
              || 'FROM ';

      END IF;

      IF p_clip_edge_tab IS NOT NULL
      THEN

         psql := psql || clip_edge_tab || ' t '
                      || 'WHERE '
                      || 't.l_' || mask_col || ' = ' || p_mask || ' '
                      || 'OR '
                      || 't.r_' || mask_col || ' = ' || p_mask || ' ';

      ELSE

         --use the face feature column, not the clip edge column
         psql := psql || clip_parms.gen_topology_name || '_edge$ t, '
                      || clip_parms.edge_input_table || ' e '
                      || 'WHERE '
                      || '(e.l_' || mask_col || ' = ''' || p_mask || ''' OR e.r_' || mask_col || ' = ''' || p_mask || ''') '
                      || 'AND '
                      || '(   (e.l_' || mask_col || ' <> e.r_' || mask_col || ') '
                      || '  OR e.l_' || mask_col || ' IS NULL OR e.r_' || mask_col || ' IS NULL) '
                      || 'AND e.edge_id = t.edge_id ';


      END IF;

           --No bind vars allowed in DDL

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK',newcliptable,'SQL to create table ' || newcliptable,
                                          NULL,NULL,NULL,psql);

      --private procedure
      CLIP_CREATE_TABLE(p_schema,
                        p_jobid,
                        newcliptable,
                        psql,
                        'ID',   --pkc
                        'OID'); --unique key

      --verify that our new table is valid
      --custom utility checks for NULL, gtype, validate_geom_With_context, and custom self intersections

      psql := 'SELECT COUNT(*) '
           || 'FROM '
           || newcliptable || ' a '
           || 'WHERE GZ_GEOM_UTILS.validate_lines_with_context(a.sdogeometry, :p1) != :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING clip_parms.gen_clip_tolerance,
                                              'TRUE';

      IF kount > 0
      THEN

          GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK',newcliptable,
                                             'Error: validation problem with ' || newcliptable,
                                             NULL,NULL,NULL,psql);

          output := output || 'Problem with ' || newcliptable || ' validation. ' || kount || ' sdogeometries arent valid |';

      ELSE

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK',newcliptable,
                                            'Great, ' || newcliptable || ' sdogeometry validates ',
                                            NULL,NULL,NULL,psql);

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_CLIP_MASK: Register metadata and spatial index ' || newcliptable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK',newcliptable,
                                            'Spatially indexing  ' || newcliptable,
                                             NULL,NULL,NULL,NULL);

      GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(newcliptable,
                                     'SDOGEOMETRY',
                                     clip_parms.gen_clip_job_srid,
                                     clip_parms.gen_clip_tolerance);

      --not used AND ALSO was overwriting the sdogeometry spatial index
      --Do not resurrect without good reason
      --GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(newcliptable,
                                     --'BUF_SDOGEOMETRY',
                                     --clip_parms.gen_clip_job_srid,
                                     --clip_parms.gen_clip_tolerance);




      --CLEAR OUT TOUCHY EDGES is defunct fall 2012
      --Will always add the state outline to the topo first

      --For state based topo builds:
      --To get reasonable performance we have already copied the entire topology with these edges
      --For national topo builds:
      --We have added all interior edges to the topology, state outlines we left off
      --But we dont want the clip mask to go into the topology second and hit any of these (above)
      --The state gets shifted around and we get topology merge problems way down the line

      --So... remove any of the edges that conflict with the state outline
      --The interior edges that were state outline never got added to the interior edge feature table
      --And the primitive edges for those guys got nixed in the previous module
      --Now we need to do the same for the touchers, and mark them to add back in
      --   This is what's different about the _interior touchers_ we do ultimately want them

      /*
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK',newcliptable,
                                        'STARTING: Remove touchy edges from ' || newedgetable);



      --Their topogeom gets updated to NULL (meaning relation$ pointers gone)
      --And the edge primitive is removed
      --the column "DANGLE" on the feature edge table gets a T


      retval := GZ_CLIP.REMOVE_TOUCHY_EDGES(p_schema,
                                            p_project_id,
                                            p_jobid,
                                            newtopo,
                                            newedgetable,
                                            newcliptable);

      */

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_CLIP_MASK: Add topo layer ' || newcliptable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK',newcliptable,
                                         'STARTING: ADD_TOPO_FROM_SPATIAL on ' || newcliptable);


      --generic add_linear_geometry + constructors call

      GZ_TOPO_UTIL.ADD_TOPO_FROM_SPATIAL(newtopo,
                                         newcliptable,
                                         'ID',
                                         'LINE',
                                         'CLIP',      --generic logger
                                         'SDOGEOMETRY',
                                         NULL,         --no subset
                                         NULL,
                                         'N',          --no allow splits. Should intersect nothing at this point
                                         'Y');         --yes new layer


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_CLIP_MASK: Verify topology build on ' || newcliptable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Check that everything is closed loops of edges and nodes, no gaps or three-way intersections

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK',newcliptable,
                                         'STARTING: Closed loop check on topo of ' || newcliptable);


      --NOTE that this step can be extremely slow for an occasional call
      --Should look into this
      loopcheck := GZ_TOPO_UTIL.CLOSED_LOOPS(newcliptable,newtopo);

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id  || p_jobid,'ADD_CLIP_MASK',newcliptable,
                                          'FINISHED: Closed loop check on ' || newcliptable);


      IF loopcheck != 'Y'
      THEN

          output := output || 'Problem with ' || newcliptable || ' topo.  Check: ' || loopcheck || ' |';

      END IF;




      /*
      TOUCHY EDGES defunct fall 2012

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id  || p_jobid,'ADD_CLIP_MASK',newcliptable,
                                         'Adding touchy edges back into ' || newedgetable);

      --Use combined add_linear_geometry + constructor


      GZ_UTILITIES.ADD_TOPO_FROM_SPATIAL(newtopo,
                                         newedgetable,
                                         'EDGE_ID',
                                         'LINE',
                                         'CLIP',      --generic logger
                                         'SDOGEOMETRY',
                                         'DANGLE',
                                         'T',
                                         'Y',          --yes allow splits. Should intersect nothing at this point
                                         'N');         --no new layer




      --lets verify that this worked

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || newedgetable || ' a '
           || 'WHERE a.topogeom IS NULL ';

      EXECUTE IMMEDIATE psql INTO kount;

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id  || p_jobid,'ADD_CLIP_MASK',newcliptable,
                                         'Completed adding touchy edges back into ' || newedgetable ||
                                          '. ' || kount || ' feature edges have NULL topogeom');


      IF kount > 0
      THEN

         output := output || ' | Doh, we have ' || kount || ' features in ' || newedgetable || ' with NULL topogeom ';

      END IF;

      */

      --Now that we have something in the topology we can grant these selects

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',newtopo || '_RELATION$');
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',newtopo || '_NODE$');

      --now run tune up
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK',newedgetable,
                                         'Calling gz_topo_tune_up on ' || newtopo);

      GZ_TOPO_UTIL.GZ_TOPO_TUNE_UP(newtopo);


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK ' || p_mask,newcliptable ,'COMPLETED',start_time);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_CLIP_MASK: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --What other errors should we be trapping and returning...?

      RETURN output;


   END ADD_CLIP_MASK;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public overloaded ---------------------------------------------------------------------

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

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public overloaded ---------------------------------------------------------------------

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


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC for debug-----------------------------------------------------------------------

   FUNCTION ID_DANGLES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 3/10/10
      --Corresponds to step 6 of
      --   http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions
      --I'm not sure if this step is really necessary since it is coarsely identifying edge features that
      --  may be composed of multiple topological edges with different dangle conditions
      --  For example, an edge feature ID'd here as a "dangle" might contain a pure external dangling
      --  edge, and internal dangling edge, and some portion of it that is a good, non dangling edge
      --  True, DANGLE=Y not being used as of July 2010
      --Added DANGLE = '<original node id>' for a dangling multi-connected node

      --ID all possible V dangles and sort out details later 9/8/10



      psql           VARCHAR2(4000);
      clip_parms     GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      output         VARCHAR2(4000) := '0';
      start_time     TIMESTAMP;
      newedgetable   VARCHAR2(32);
      newtopo        VARCHAR2(32);
      nodes          GZ_TYPES.stringarray;
      edges          GZ_TYPES.stringarray;
      all_edges      GZ_TYPES.stringarray;
      kount          PLS_INTEGER;
      newcliptable   VARCHAR2(32);




   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ID_DANGLES ' || p_mask,NULL,'STARTING');

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      --set up basic names
      --ex Z601CL_Z699IN_EWRK
      newedgetable := GZ_CLIP.GET_EDGE_TABLE(p_project_id,p_jobid,clip_parms.edge_input_table);

      -- ie STATEFP02
      newtopo := p_topo_out;
      -- ie SDZ1AL_GEN_ST_EDGES_HI
      newcliptable := p_project_id || p_jobid || '_' || clip_parms.gen_clip_table; --our task at hand

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ID_DANGLES: Update ' || newedgetable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --First, update all to null
      --we may have hijacked this column for marking edges to be removed and restored on state outline

      psql := 'UPDATE ' || newedgetable || ' aa '
           || 'SET '
           || 'aa.dangle = NULL ';

      EXECUTE IMMEDIATE psql;
      COMMIT;


      --Not actually using this dangle flag
      --Note that it ids dangles that arent really dangles, may be connected to a non closed dangling system
      --EXTEND_INTERIOR_DANGLES takes care of the more complicated WHERE EXISTS whereclause
      psql := 'UPDATE ' || newedgetable || ' aa '
           || 'SET '
           || 'aa.dangle = :p1 '
           || 'WHERE aa.edge_id IN '
           || '( '
           || 'SELECT DISTINCT a.edge_id '
           || 'FROM '
           || newedgetable || ' a, '
           || newtopo || '_RELATION$ r, '
           || newtopo || '_EDGE$ e '
           || 'WHERE '
           || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
           || 'r.tg_id = a.topogeom.tg_id AND '
           || 'e.edge_id = r.topo_id AND '
           || '( (e.left_face_id = e.right_face_id) OR '
           || '  (e.left_face_id = -1) or (e.right_face_id = -1) ) '
           || ')';

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ID_DANGLES ' || p_mask,newedgetable,'Update ' || newedgetable || ' ' ,
                                          NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql USING 'Y';

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ID_DANGLES: Update ' || newedgetable || ' With state boundary V dangles');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --These are nodes that are multiconnected in the input topology and are on the state boundary, producing a V (or multi-V)

      --At this point they may be either
      /*
            |  <-- Some other (technically) non-dangle with BAD same L/R faces
            |
            ^   <--A "dangling" triangle like thing
            V
         _________ (new state/shoreline)



      Or actually ok because they bumped up against the new clipper

           |
           |
           ^
         --V-----


      Or, very rarely remaining intersecting precisely on the new clipper

           |
           |
           ^
           V
         --X--
      */

      --Start by finding candidate edges from our state based edge table
      --That were multiconnected to the original outline

      psql := 'SELECT z.nodes FROM '
           || '( '       --1st paren: all nodes on interior lines
           || 'select a.edge_id, e.start_node_id nodes FROM '
           || newedgetable || ' a, '
           || clip_parms.gen_topology_name || '_edge$ e '
           || 'WHERE '
           || 'a.edge_id = e.edge_id '
           || 'UNION '
           || 'select a.edge_id, e.end_node_id nodes FROM '
           || newedgetable || ' a, '
           || clip_parms.gen_topology_name || '_edge$ e '
           || 'WHERE '
           || 'a.edge_id = e.edge_id '
           || ') z where z.nodes IN '
           || '( ' --next up, nodes on states or shoreline (shoreline unlikely due to 3 mi limit thing)
           || 'SELECT e.start_node_id nodes FROM '
           || clip_parms.edge_input_table || ' a, '         --<-- this is the original EDGE input table
           || clip_parms.gen_topology_name || '_edge$ e '   --<-- this is the original topology
           || 'WHERE (a.' || clip_parms.face_feature_mask_col || ' = :p1 OR a.' || clip_parms.face_feature_mask_col || ' IS NULL) AND '
           || 'a.edge_id = e.edge_id '
           || 'UNION '
           || 'SELECT e.end_node_id nodes FROM '
           || clip_parms.edge_input_table || ' a, '
           || clip_parms.gen_topology_name || '_edge$ e '
           || 'WHERE (a.' || clip_parms.face_feature_mask_col || ' = :p2 OR a.' || clip_parms.face_feature_mask_col || ' IS NULL) AND '
           || 'a.edge_id = e.edge_id '
           || ') '
           || 'GROUP BY nodes '
           || 'HAVING COUNT(nodes) > 1 '; --Count greater than 1 meaning V against the orginal state

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ID_DANGLES ' || p_mask,newedgetable,'ID ' || newedgetable || ' V dangles' ,
                                          NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql BULK COLLECT INTO nodes USING '00', '00';

      --dbms_output.put_line('nodes count is ' || nodes.COUNT);

      FOR i in 1 .. nodes.COUNT
      LOOP


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ID_DANGLES: Update ' || newedgetable || ' With state boundary V dangles');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         --THIS NEXT SECTION, which initially went into production for school districts, IS NOT RIGHT
         --First, I am throwing out as V dangles any edge that shares a node with the new clip outline
         --   I assumed it was picture 2 above
         --Sometimes, however, the opposite side of the same feature edge connects with the state outline
         --Second, I am updating each edge feature with a single dangle node value
         --Sometimes, however, a feature edge can be a V dangle on both ends, which totally threw code out of whack

         /*
         --95% of the time we will never enter this loop
         --This section is poorly written, come back if performance becomes a problem

         --In english:
         --For the current edge features we are working with
         --Find those that were historically attached to the edge of a state with the node(s) from above
         --Find the actual topology edges for those edge features
         --Plus the universe of edges that make up the current state boundary
         --If they NOW share a node in the topology (edge feature + state outline), we will ignore them
         --If no shared nodes, suspicious edge <--> new state clip, we have a V dangle

         --Admittedly, its strange to be querying from a feature table to two topologies

         psql := 'select a.edge_id FROM '  --edge id for FEATURE edges
               || newedgetable || ' a, '
               || clip_parms.gen_topology_name || '_edge$ ee, '
               || newtopo || '_RELATION$ r, '
               || newtopo || '_EDGE$ e '
               || 'WHERE (ee.start_node_id = :p1 OR ee.end_node_id = :p2) '
               || 'AND a.edge_id = ee.edge_id '
               || 'AND a.topogeom.tg_id = r.tg_id '
               || 'AND a.topogeom.tg_layer_id = r.tg_layer_id '
               || 'AND r.topo_id = e.edge_id '
               || 'AND e.start_node_id '   --start node not in
               || 'NOT IN ( ' --nodes on the current state outline
               || 'SELECT e.start_node_id FROM '
               || newcliptable || ' a, '
               || newtopo || '_RELATION$ r, '
               || newtopo || '_EDGE$ e '
               || 'WHERE a.topogeom.tg_layer_id = r.tg_layer_id '
               || 'AND a.topogeom.tg_id = r.tg_id '
               || 'AND r.topo_id = e.edge_id '
               || 'UNION '
               || 'SELECT e.end_node_id FROM '
               || newcliptable || ' a, '
               || newtopo || '_RELATION$ r, '
               || newtopo || '_EDGE$ e '
               || 'WHERE a.topogeom.tg_layer_id = r.tg_layer_id '
               || 'AND a.topogeom.tg_id = r.tg_id '
               || 'AND r.topo_id = e.edge_id '
               || ') '
               || 'AND e.end_node_id '  --AND end node not in
               || 'NOT IN ( '  --nodes on the current state outline
               || 'SELECT e.start_node_id FROM '
               || newcliptable || ' a, '
               || newtopo || '_RELATION$ r, '
               || newtopo || '_EDGE$ e '
               || 'WHERE a.topogeom.tg_layer_id = r.tg_layer_id '
               || 'AND a.topogeom.tg_id = r.tg_id '
               || 'AND r.topo_id = e.edge_id '
               || 'UNION '
               || 'SELECT e.end_node_id FROM '
               || newcliptable || ' a, '
               || newtopo || '_RELATION$ r, '
               || newtopo || '_EDGE$ e '
               || 'WHERE a.topogeom.tg_layer_id = r.tg_layer_id '
               || 'AND a.topogeom.tg_id = r.tg_id '
               || 'AND r.topo_id = e.edge_id '
               || ') ';

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ID_DANGLES ' || p_mask,newedgetable,
                                                'Checking if node ' || nodes(i) || ' is a V dangle' ,
                                                 NULL,NULL,NULL,psql);

            EXECUTE IMMEDIATE psql BULK COLLECT INTO edges USING nodes(i),
                                                                 nodes(i);

            IF edges.COUNT > 0
            THEN

               FORALL ii IN 1 .. edges.COUNT
                  EXECUTE IMMEDIATE 'UPDATE ' || newedgetable || ' a '
                                 || 'SET a.dangle = :p1 '
                                 || 'WHERE edge_id = :p2 '
                  USING nodes(i), edges(ii);

               COMMIT;

            END IF;

            */


         --Playing it safe
         --ID each feature edge for every and all possible dangles
         --extend_dangling_V_nodes will sort it out later


         psql := 'SELECT a.edge_id '
              || 'FROM '
              || newedgetable || ' a, '
              || clip_parms.gen_topology_name || '_edge$ ee '
              || 'WHERE '
              || '(ee.start_node_id = :p1 OR ee.end_node_id = :p2) AND '
              || 'a.edge_id = ee.edge_id ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO edges USING nodes(i),
                                                                 nodes(i);

         IF i = 1
         THEN

           --first (or only) V dangle, update EDGE table with possible dangling node

            all_edges := edges;

            FORALL ii IN 1 .. edges.COUNT
               EXECUTE IMMEDIATE 'UPDATE ' || newedgetable || ' a '
                              || 'SET a.dangle = :p1 '
                              || 'WHERE edge_id = :p2 '
               USING nodes(i), edges(ii);

               COMMIT;

         ELSE


            FOR ii in 1 .. edges.COUNT
            LOOP


               IF GZ_CLIP.QUERY_DELIMITED_LIST(all_edges,edges(ii)) = 0
               THEN

                  --standard, just a possible V dangle
                  psql := 'UPDATE ' || newedgetable || ' a '
                       || 'SET a.dangle = :p1 '
                       || 'WHERE edge_id = :p2 ';

                  EXECUTE IMMEDIATE psql USING nodes(i),
                                               edges(ii);

                  COMMIT;

               ELSE

                  --rare, V dangle on both sides of the feature edge
                  --tag it as xxx,yyy
                  psql := 'UPDATE ' || newedgetable || ' a '
                       || 'SET a.dangle = a.dangle || :p1 '
                       || 'WHERE edge_id = :p2 ';

                  EXECUTE IMMEDIATE psql USING ',' || nodes(i),
                                               edges(ii);

                  COMMIT;

               END IF;


            END LOOP;

            --update all_edges for next time through
            all_edges := GZ_BUSINESS_UTILS.stringarray_add(all_edges,edges);

         END IF;  --end if on i = 1


      END LOOP;



      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ID_DANGLES ' || p_mask,newedgetable,'COMPLETED',start_time);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ID_DANGLES: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --What other errors should we be trapping and returning...?
      RETURN output;


   END ID_DANGLES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------


   PROCEDURE GZ_REMOVE_EDGE (
      p_topo           IN VARCHAR2,
      p_edge_id        IN VARCHAR2,
      p_delta          IN NUMBER DEFAULT NULL
   )
   AS

      --Matt! 9/10/12
      --Need to add this to utilities. Private in clip to simplify a CR

      psql              VARCHAR2(4000);
      window_geom       SDO_GEOMETRY;
      create_it         NUMBER;


   BEGIN

      psql := 'SELECT SDO_GEOM.SDO_MBR(e.geometry) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO window_geom USING p_edge_id;

      IF window_geom.sdo_ordinates.COUNT != 4
      THEN

        RAISE_APPLICATION_ERROR(-20001,'Bad window. Bad. ');

      END IF;

      --create the topomap with window

      IF p_delta IS NULL
      THEN

         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                      p_topo,
                                                      2,
                                                      window_geom.sdo_ordinates(1),
                                                      window_geom.sdo_ordinates(2),
                                                      window_geom.sdo_ordinates(3),
                                                      window_geom.sdo_ordinates(4)
                                                      );

      ELSE

         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                      p_topo,
                                                      2,
                                                      window_geom.sdo_ordinates(1),
                                                      window_geom.sdo_ordinates(2),
                                                      window_geom.sdo_ordinates(3),
                                                      window_geom.sdo_ordinates(4),
                                                      p_delta);   --bogus topomap errors, shift topomap

      END IF;


      --no error handler here, let the caller trap and release
      SDO_TOPO_MAP.REMOVE_EDGE(NULL,p_edge_id);

      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);


   END GZ_REMOVE_EDGE;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC--------------------------------------------------------------------------------

   PROCEDURE DELETE_AN_EDGE (
      p_schema         IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_topo           IN VARCHAR2,
      p_feature_table  IN VARCHAR2,
      p_feature_edge   IN NUMBER,
      p_edge_id        IN NUMBER,
      p_tg_layer_id    IN NUMBER,
      p_topomap        IN VARCHAR2,  --not used any more
      p_retain_feature IN VARCHAR2 DEFAULT 'N',
      p_retain_prim    IN VARCHAR2 DEFAULT 'N',
      p_reverse_flag   IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 8/14/10
      --This guy disassociates a primitive edge
      --Manages the fallout on the feature edge table
      --Deletes the edge from the topo if possible or requested

      --Added retain options and reverse flag 12/10 Matt!

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      my_tg_id          NUMBER;
      edge_id           NUMBER := p_edge_id;

   BEGIN

      --tg id is too dynamic to track in the caller if we are looping over primitive edges
      --this is the only safe place to grab it
      --performance be ****d

      psql := 'SELECT a.topogeom.tg_id '
           || 'FROM '
           || p_feature_table || ' a '
           || 'WHERE a.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO my_tg_id USING p_feature_edge;

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_topo || '_RELATION$ r '
           || 'WHERE '
           || 'r.tg_id = :p1 AND '
           || 'r.tg_layer_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING my_tg_id,
                                              p_tg_layer_id;



      IF kount > 1
      THEN

         --We may use the constructor, leaving at least one edge with the feature other
         --than the one we are deleting
         --This is my current understanding of best practice
         --Most of the time we are in here

         --UNDOCUMENTED! (that I know of) negative edge id in constructor when edge is reversed
         IF p_reverse_flag IS NOT NULL
         THEN
            edge_id := -edge_id;
         END IF;

         psql := 'UPDATE ' ||  p_feature_table || ' a '
              || 'SET '
              || 'a.topogeom = '  --Topology name, topo geom type, tg_layer_id, no topo to add, topo to delete (edge id, type)
              || 'SDO_TOPO_GEOMETRY(:p1, :p2, :p3, NULL, SDO_TOPO_OBJECT_ARRAY(SDO_TOPO_OBJECT(:p4,:p5))) '
              || 'WHERE a.edge_id = :p6 ';

         EXECUTE IMMEDIATE psql USING p_topo,
                                      2,
                                      p_tg_layer_id,
                                      edge_id,
                                      2,
                                      p_feature_edge;

         COMMIT;

         IF p_reverse_flag IS NOT NULL
         THEN
            edge_id := ABS(edge_id);
         END IF;

      ELSIF kount = 1
      AND p_retain_feature = 'N'
      THEN

         --Should not (though can) delete the last edge for a feature directly
         --It produces suspicious phony tg_ids in relation$

         psql := 'DELETE FROM ' || p_feature_table || ' a '
              || 'WHERE '
              || 'a.edge_id = :p1 ';

         EXECUTE IMMEDIATE psql USING p_feature_edge;
         COMMIT;

      ELSIF kount = 1
      AND p_retain_feature = 'Y'
      THEN

         --we want this feature record to stick around, but disassociate the primitive
         --confirmed that updating to NULL does delete the relation$ pointers

         psql := 'UPDATE ' || p_feature_table || ' a '
              || 'SET a.topogeom = NULL '
              || 'WHERE '
              || 'a.edge_id = :p1 ';

         EXECUTE IMMEDIATE psql USING p_feature_edge;
         COMMIT;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'No matching tg_id(' || my_tg_id || ')  for this tg_layer_id(' || p_tg_layer_id || ') ');

      END IF;



      IF p_retain_prim = 'N'
      THEN

          --Use implicit topomap
          --will probably need to add explicit topomap for various error handling

          BEGIN

             --We are done with this primitive edge no matter what
             --   (unless we are retaining the primitive. In this case we CANT delete it because its also a feature of the clip)

            GZ_CLIP.GZ_REMOVE_EDGE(p_topo,
                                   edge_id);


          EXCEPTION
          WHEN OTHERS THEN

               --I hope this wont happen now that we are using the constructor

               IF SQLERRM LIKE '%is not on the boundary of one or two of the faces it links%'
               OR SQLERRM LIKE '%links a face but is not on its boundary%'
               THEN
                  --ORA-29532: Java call terminated by uncaught Java exception: oracle.spatial.topo.TopoValidationException:
                  --Edge 2015 is not on the boundary of one or two of the faces it links
                  --ORA-06512: at "MDSYS.SDO_TOPO_MAP", line 344
                  --I dont know what this is all about
                  --The error identifies an edge that is not the edge being removed, and I dont see any problems
                  --This may cause chaos later, who the heck knows

                  --oracle.spatial.topo.TopoValidationException: Edge 97 links a face but is not on its boundary
                  --This is the bogus error where an edge appears to glance against the topomap window at a point
                  --especially topomaps covering lots of geodetic ground
                  --resolve by shifting the topomap a little

                  GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_EXTERIOR_DANGLES ' || p_topo, p_topo || '_EDGE$',
                                             'Got one of the weird ''is not on the boundary'' errors, lets shift the topomap ' ,
                                              NULL,NULL,NULL,'SDO_TOPO_MAP.REMOVE_EDGE(''' || p_topo || ''',' || edge_id|| ');',
                                              NULL,SQLERRM);

                  GZ_CLIP.GZ_REMOVE_EDGE(p_topo,
                                         edge_id,
                                         0.0002); --override ez_topomap_manager .0001 default delta


               ELSE

                  RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

               END IF;

          END;

      END IF;

   END DELETE_AN_EDGE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC--------------------------------------------------------------------------------

   FUNCTION DELETE_EXTERIOR_DANGLES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 3/15/10
      --Corresponds to step 7 of
      --   http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions

      --I dont think this is an issue any more, but leaving the gibberish just in case...
      --Note: In some cases this function will leave the dangling edges in the edge table
      --      No longer associated with the edge feature table so no worries I dont think
      --      If this is a problem the mysterious error preventing edge deletion can probably
      --      be avoided by using an explicit topomap.



      psql           VARCHAR2(4000);
      clip_parms     GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      output         VARCHAR2(4000) := '0';
      start_time     TIMESTAMP;
      newedgetable   VARCHAR2(32);
      newtopo        VARCHAR2(32);
      dangles        GZ_TYPES.stringarray;
      feature_edges  GZ_TYPES.stringarray;
      tg_ids         GZ_TYPES.stringarray;
      tg_layer_id    NUMBER;
      topomap        VARCHAR2(4000);
      ez_topo_mgr    NUMBER;



   BEGIN


      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_EXTERIOR_DANGLES ' || p_mask,NULL,'STARTING');

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      --set up basic names
      --ex Z601CL_Z699IN_EWRK
      newedgetable := GZ_CLIP.GET_EDGE_TABLE(p_project_id,p_jobid,clip_parms.edge_input_table);

      newtopo := p_topo_out;
      topomap := newtopo || '_TOPOMAP';

      --usually 1
      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(newtopo,
                                                  newedgetable,
                                                  'TOPOGEOM',
                                                  'LINE');

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('DELETE_EXTERIOR_DANGLES: Collect ' || newedgetable|| ' dangling edges ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Note to self: Must collect these edges before deleting any from relation$
      psql := 'SELECT a.edge_id, e.edge_id, r.tg_id '
           || 'FROM '
           || newedgetable || ' a, '
           || newtopo || '_RELATION$ r, '
           || newtopo || '_EDGE$ e '
           || 'WHERE '
           || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
           || 'r.tg_id = a.topogeom.tg_id AND '
           || 'e.edge_id = r.topo_id AND '
           || '( (e.left_face_id = :p1) and (e.right_face_id = :p2) ) ';



      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_EXTERIOR_DANGLES ' || p_mask,
                                          newedgetable,'Collect dangling edges ' ,
                                          NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql BULK COLLECT INTO feature_edges,
                                               dangles,
                                               tg_ids USING -1, -1;

      --Would like to do this in a cursor, but as long as dangles isnt ginormous
      --I think we will be ok
      --  Ive heard about enough from Spring 2010 Matt


      --We have features in the xx_EDGE_XX feature table that partially dangle outside of the clipper


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('DELETE_EXTERIOR_DANGLES: Delete ' || newedgetable || ' dangles ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --No topomap for the entire topology, too costly. Used to do this


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_EXTERIOR_DANGLES ' || p_mask,
                                          newedgetable,'Deleting ' || feature_edges.COUNT || ' dangling edges ',
                                          NULL,NULL,NULL,NULL);

      FOR i in 1 .. feature_edges.COUNT
      LOOP

         --Cleans out the edge, and feature edge if necessary

         GZ_CLIP.DELETE_AN_EDGE(p_schema,
                                p_project_id,
                                p_jobid,
                                newtopo,
                                newedgetable,
                                feature_edges(i),
                                dangles(i),
                                tg_layer_id,
                                topomap,   --not used
                                'N',       --No retain feature record if this is the last edge
                                'N');      --No retain primitive, delete the edge from the topology

      END LOOP;



      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_EXTERIOR_DANGLES ' || p_mask,newtopo || '_RELATION$',
                                          'COMPLETED',start_time);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('DELETE_EXTERIOR_DANGLES: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --What other errors should we be trapping and returning...?
      RETURN output;


   END DELETE_EXTERIOR_DANGLES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------


   FUNCTION DELETE_DANGLING_FACES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 3/16/10
      --Corresponds to step 8 of
      --   http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions
      --Rewrite to use topomap 6/10/10
      --Added catch and delete of edges on state boundary with reversed direction Matt! 12/09/10



      psql1          VARCHAR2(4000);
      psql           VARCHAR2(4000);
      clip_parms     GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      output         VARCHAR2(4000) := '0';
      start_time     TIMESTAMP;
      newedgetable   VARCHAR2(32);
      newtopo        VARCHAR2(32);
      newcliptable   VARCHAR2(32);
      topomap        VARCHAR2(4000);
      dangles        GZ_TYPES.stringarray;
      deadman        PLS_INTEGER := 0;
      topomaps       GZ_TYPES.stringarray;
      ez_topo_mgr    NUMBER;
      feature_edges  GZ_TYPES.stringarray;
      tg_ids         GZ_TYPES.stringarray;
      tg_layer_id    NUMBER;


   BEGIN


      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_DANGLING_FACES ' || p_mask,NULL,'STARTING');

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      --set up basic names
      --ex Z601CL_Z699IN_EWRK
      newedgetable := GZ_CLIP.GET_EDGE_TABLE(p_project_id,p_jobid,clip_parms.edge_input_table);

      newtopo := p_topo_out;
      topomap := newtopo || '_TOPOMAP';

      -- ie SDZ1AL_GEN_ST_EDGES_HI
      newcliptable := p_project_id || p_jobid || '_' || clip_parms.gen_clip_table; --our task at hand

      --usually 1
      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(newtopo,
                                                  newedgetable,
                                                  'TOPOGEOM',
                                                  'LINE');

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('DELETE_DANGLING_FACES:  Collect ' || newedgetable|| ' dangling faces ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Note to self: Must collect edge_ids before deleting from relation$
      --These are edges that stick outside of the clip mask
      --   but still connect back to the topo spaghetti - not pure dangles
      --This could probably be combined with DELETE_EXTERIOR_DANGLES

      --Select from edge feature table where left or right face is universal
      --and edge is not part of clip mask
      psql1 := 'SELECT a.edge_id, e.edge_id, r.tg_id FROM '
            || newedgetable || ' a, '
            || newtopo || '_RELATION$ r, '
            || newtopo || '_EDGE$ e '
            || 'WHERE '
            || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
            || 'r.tg_id = a.topogeom.tg_id AND '
            || 'e.edge_id = ABS(r.topo_id) AND '
            || '( (e.left_face_id = :p1) or (e.right_face_id = :p2) ) AND '
            || 'e.edge_id NOT IN ( '
            || '   SELECT ee.edge_id FROM '
            ||     newcliptable || ' aa, '
            ||     newtopo || '_RELATION$ rr, '
            ||     newtopo || '_EDGE$ ee '
            || '   WHERE '
            || '   rr.tg_layer_id = aa.topogeom.tg_layer_id AND '
            || '   rr.tg_id = aa.topogeom.tg_id AND '
            || '   ee.edge_id = ABS(rr.topo_id) '
            || ') ';


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_DANGLING_FACES ' || p_mask, newedgetable, 'Select dangling edges',
                                           NULL,NULL,NULL,psql1);

      EXECUTE IMMEDIATE psql1 BULK COLLECT INTO feature_edges,
                                                dangles,
                                                tg_ids USING -1, -1;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('DELETE_DANGLING_FACES:  Delete EDGES ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --do not open full topology topomap, too costly (used to)

      WHILE dangles.COUNT > 0
      LOOP

         --This style of WHILE loop scares me
         IF deadman < 100
         THEN

            deadman := deadman + 1;

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Yo, we have been deleting dangling faces for 100 loops! That is fruit loops :-)');

         END IF;



         FOR i in 1 .. feature_edges.COUNT
         LOOP

            --constructors and topomap (implicit at the moment) handled in sub

            GZ_CLIP.DELETE_AN_EDGE(p_schema,
                                   p_project_id,
                                   p_jobid,
                                   newtopo,
                                   newedgetable,
                                   feature_edges(i),
                                   dangles(i),
                                   tg_layer_id,
                                   topomap,
                                   'N',     --do not retain feature if last edge
                                   'N');    --do not retain the edge in the topology


         END LOOP;



         --Go back for another loop if necessary
         --Interior edges that were not touching the edge of the universe may be now
         --They get exposed by the previous loop
         --NOTE psql1, the stored text from way above
         EXECUTE IMMEDIATE psql1 BULK COLLECT INTO feature_edges,
                                                   dangles,
                                                   tg_ids USING -1, -1;



      END LOOP;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('DELETE_DANGLING_FACES:  Catch and delete reversed edges on Clip Outline ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      /* REFERENCE

      SELECT a.edge_id, e.edge_id, r.tg_id
        FROM MT609CL_MT609_EDGE a,
             MT609CL_RELATION$ r,
             MT609CL_EDGE$ e
       WHERE     r.tg_layer_id = a.topogeom.tg_layer_id
             AND r.tg_id = a.topogeom.tg_id
             AND e.edge_id = ABS(r.topo_id)
             AND r.topo_id < 0  --here it is, a reversed edge for our interior edges
             AND ( (e.left_face_id = -1) OR (e.right_face_id = -1))
             AND e.edge_id IN
                    (SELECT ee.edge_id
                       FROM MT609CL_GEN_ST_EDGES_HI aa,
                            MT609CL_RELATION$ rr,
                            MT609CL_EDGE$ ee
                      WHERE     rr.tg_layer_id = aa.topogeom.tg_layer_id
                            AND rr.tg_id = aa.topogeom.tg_id
                            AND ee.edge_id = ABS (rr.topo_id))
      */



      psql1 := 'SELECT a.edge_id, e.edge_id, r.tg_id FROM '
            || newedgetable || ' a, '
            || newtopo || '_RELATION$ r, '
            || newtopo || '_EDGE$ e '
            || 'WHERE '
            || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
            || 'r.tg_id = a.topogeom.tg_id AND '
            || 'e.edge_id = ABS(r.topo_id) AND '
            || 'r.topo_id < 0 AND '             -- a reversed edge for our interior edges
            || '( (e.left_face_id = :p1) or (e.right_face_id = :p2) ) AND '
            || 'e.edge_id IN ( '   -- IN IN IN !
            || '   SELECT ee.edge_id FROM '
            ||     newcliptable || ' aa, '
            ||     newtopo || '_RELATION$ rr, '
            ||     newtopo || '_EDGE$ ee '
            || '   WHERE '
            || '   rr.tg_layer_id = aa.topogeom.tg_layer_id AND '
            || '   rr.tg_id = aa.topogeom.tg_id AND '
            || '   ee.edge_id = ABS(rr.topo_id) '
            || ') ';



      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_DANGLING_FACES ' || p_mask, newedgetable,
                                         'Check for any edges on the clip outline that are reversed ' ,
                                          NULL,NULL,NULL,psql1);

      --just to be safe
      feature_edges.DELETE;
      dangles.delete;
      tg_ids.delete;

      EXECUTE IMMEDIATE psql1 BULK COLLECT INTO feature_edges,
                                                dangles,
                                                tg_ids USING -1, -1;

      FOR i in 1 .. feature_edges.COUNT
      LOOP

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_DANGLING_FACES ' || p_mask, newedgetable,
                                            'Jackpot! Edge ' || dangles(i) || ' of feature edge ' || feature_edges(i) || ' is reversed ',
                                            NULL,NULL,NULL,psql1);

         GZ_CLIP.DELETE_AN_EDGE(p_schema,
                                p_project_id,
                                p_jobid,
                                newtopo,
                                newedgetable,
                                feature_edges(i),
                                dangles(i),
                                tg_layer_id,
                                topomap,
                                'N',  --Do not retain the feature if this is its last edge
                                'Y',  --Must retain the primitive, its part of the state clip feature
                                'Y'); --reverse flag. Use negative edge id in constructor

      END LOOP;


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_DANGLING_FACES ' || p_mask, newedgetable,
                                         'Finished removing ' || feature_edges.COUNT || ' edges ',
                                          NULL,NULL,NULL,NULL);


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_DANGLING_FACES ' || p_mask,newedgetable,'COMPLETED',start_time);


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('DELETE_DANGLING_FACES: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --What other errors should we be trapping and returning...?
      RETURN output;


   END DELETE_DANGLING_FACES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC--------------------------------------------------------------------------------

   PROCEDURE UHAUL_NODE (
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_dangle_edge     IN NUMBER,
      p_dangle_node     IN NUMBER,
      p_percent         IN NUMBER DEFAULT .5
   ) AS

      --Matt! 6/2/10
      --Attempt to back our dangling node up from the clip outline
      --So we can add a new node on the clip outline without error
      --beep    beep      beep

      -- from
      -- 0--->--X------X-----X-------0
      -- to
      -- 0--->--X------X-----X---0


      psql           VARCHAR2(4000);
      danglegeom     SDO_GEOMETRY;
      startid        NUMBER;
      endid          NUMBER;
      movept         SDO_GEOMETRY;
      fixpt          SDO_GEOMETRY;
      moveptmeasure  NUMBER;
      fixptmeasure   NUMBER;
      newmeasure     NUMBER;
      newpt          SDO_GEOMETRY;
      newnumarray    SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY();


   BEGIN

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'UHAUL_NODE', p_topo || '_edge$',
                                          'Backing up node ' || p_dangle_node || ' on ' || p_dangle_edge);


      --get our geom plus node ids for comparison
      psql := 'SELECT e.start_node_id, e.end_node_id, e.geometry '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO startid, endid, danglegeom USING p_dangle_edge;


      IF p_dangle_node = startid
      THEN

         movept := SDO_GEOMETRY(
                                2001,
                                danglegeom.sdo_srid,
                                SDO_POINT_TYPE(danglegeom.sdo_ordinates(1),
                                               danglegeom.sdo_ordinates(2),
                                               NULL),
                                NULL,
                                NULL);

         fixpt := SDO_GEOMETRY(
                               2001,
                               danglegeom.sdo_srid,
                               SDO_POINT_TYPE(danglegeom.sdo_ordinates(3),
                                              danglegeom.sdo_ordinates(4),
                                              NULL),
                                NULL,
                                NULL);

      ELSIF p_dangle_node = endid
      THEN

         movept := SDO_GEOMETRY(
                                2001,
                                danglegeom.sdo_srid,
                                SDO_POINT_TYPE(danglegeom.sdo_ordinates(danglegeom.sdo_ordinates.COUNT - 1),
                                               danglegeom.sdo_ordinates(danglegeom.sdo_ordinates.COUNT),
                                               NULL),
                                NULL,
                                NULL);

         fixpt := SDO_GEOMETRY(
                               2001,
                               danglegeom.sdo_srid,
                               SDO_POINT_TYPE(danglegeom.sdo_ordinates(danglegeom.sdo_ordinates.COUNT - 3),
                                              danglegeom.sdo_ordinates(danglegeom.sdo_ordinates.COUNT- 2),
                                              NULL),
                                NULL,
                                NULL);
      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Dangle node ' || p_dangle_node || ' doesnt match dangle edge ' || p_dangle_edge);

      END IF;


      --Figure out our measures
      moveptmeasure := SDO_LRS.FIND_MEASURE( (SDO_LRS.convert_to_lrs_geom(danglegeom)), movept);
      fixptmeasure := SDO_LRS.FIND_MEASURE( (SDO_LRS.convert_to_lrs_geom(danglegeom)), fixpt);

      newmeasure := (moveptmeasure + fixptmeasure) * p_percent;


      --Find our new point based on new measure
      newpt := SDO_LRS.CONVERT_TO_STD_GEOM(SDO_LRS.LOCATE_PT((SDO_LRS.convert_to_lrs_geom(danglegeom)), newmeasure));



      --Use the bizarrely formatted MOVE_NODE
      --Third parm is SDO_EDGE_ARRAY
      --Which is an array of SDO_NUMBER_ARRAYS where
      --   "Each inner array consists of coordinates of each resulting attached edge, from start point to end point."
      --Thank santos cielos this is a dangle and only one inner array

      --set up our numarray first

      newnumarray.EXTEND(danglegeom.SDO_ORDINATES.COUNT);

      FOR i in 1 .. danglegeom.SDO_ORDINATES.COUNT
      LOOP

         --could combine these but this format looks so cute
         IF p_dangle_node = startid
         AND i = 1
         THEN

            newnumarray(i) := newpt.sdo_point.X;

         ELSIF p_dangle_node = startid
         AND i = 2
         THEN

            newnumarray(i) := newpt.sdo_point.Y;

         ELSIF p_dangle_node = endid
         AND i = (danglegeom.SDO_ORDINATES.COUNT - 1)
         THEN

            newnumarray(i) := newpt.sdo_point.X;

         ELSIF p_dangle_node = endid
         AND i = (danglegeom.SDO_ORDINATES.COUNT)
         THEN

            newnumarray(i) := newpt.sdo_point.Y;

         ELSE

            newnumarray(i) := danglegeom.SDO_ORDINATES(i);

         END IF;


      END LOOP;

      SDO_TOPO_MAP.MOVE_NODE(p_topo,
                             p_dangle_node,
                             SDO_EDGE_ARRAY(newnumarray));



      --Wheres the error handling here ?

   END UHAUL_NODE;


    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   PROCEDURE REAREND_NODE (
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_dangle_edge     IN NUMBER,
      p_dangle_node     IN NUMBER,
      p_percent         IN NUMBER DEFAULT .5
   ) AS

      --Matt! 6/30/10
      --More drastic measures to back our dangling node up from the clip outline
      --So we can add a new node on the clip outline without error
      --We already tried UHAUL_NODE but have backed our node up against the next vertex
      --So remove the vertex and keep backing up

      -- from
      -- 0--->--X------X-----X-------0
      -- After UHAUL_NODE
      -- 0--->--X------X-----X---0
      --After REAREND_NODE
      -- 0--->--X------X--0


      psql           VARCHAR2(4000);
      danglegeom     SDO_GEOMETRY;
      startid        NUMBER;
      endid          NUMBER;
      movept         SDO_GEOMETRY;
      fixpt          SDO_GEOMETRY;
      measurept      SDO_GEOMETRY;
      moveptmeasure  NUMBER;
      fixptmeasure   NUMBER;
      newmeasure     NUMBER;
      newpt          SDO_GEOMETRY;
      newnumarray    SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY();
      kount          PLS_INTEGER := 0;


   BEGIN

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'REAREND_NODE', p_topo || '_edge$',
                                          'Further backing up node ' || p_dangle_node || ' on ' || p_dangle_edge);


      --get our geom plus node ids for comparison
      psql := 'SELECT e.start_node_id, e.end_node_id, e.geometry '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO startid, endid, danglegeom USING p_dangle_edge;

      IF danglegeom.SDO_ORDINATES.COUNT < 6
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'REAREND_NODE', p_topo || '_edge$',
                                          'Exiting without action, edge ' || p_dangle_edge || ' only has '
                                          || danglegeom.SDO_ORDINATES.COUNT || ' ordinates ');
         RETURN;

      END IF;

      --dbms_output.put_line('startid ' || startid);
      --dbms_output.put_line('endid ' || endid);

      IF p_dangle_node = startid
      THEN

         --This is the node we are backing up
         movept := SDO_GEOMETRY(
                                2001,
                                danglegeom.sdo_srid,
                                SDO_POINT_TYPE(danglegeom.sdo_ordinates(1),
                                               danglegeom.sdo_ordinates(2),
                                               NULL),
                                NULL,
                                NULL);

         --measure from this one, it will go away
         measurept := SDO_GEOMETRY(
                                2001,
                                danglegeom.sdo_srid,
                                SDO_POINT_TYPE(danglegeom.sdo_ordinates(3),
                                               danglegeom.sdo_ordinates(4),
                                               NULL),
                                NULL,
                                NULL);


         --Then hold this one as fixed
         fixpt := SDO_GEOMETRY(
                               2001,
                               danglegeom.sdo_srid,
                               SDO_POINT_TYPE(danglegeom.sdo_ordinates(5),
                                              danglegeom.sdo_ordinates(6),
                                              NULL),
                                NULL,
                                NULL);


      ELSIF p_dangle_node = endid
      THEN

         --This is the node we are backing up
         movept := SDO_GEOMETRY(
                                2001,
                                danglegeom.sdo_srid,
                                SDO_POINT_TYPE(danglegeom.sdo_ordinates(danglegeom.sdo_ordinates.COUNT - 1),
                                               danglegeom.sdo_ordinates(danglegeom.sdo_ordinates.COUNT),
                                               NULL),
                                NULL,
                                NULL);

         --measure from this one, it will go away
         measurept := SDO_GEOMETRY(
                                2001,
                                danglegeom.sdo_srid,
                                SDO_POINT_TYPE(danglegeom.sdo_ordinates(danglegeom.sdo_ordinates.COUNT - 3),
                                               danglegeom.sdo_ordinates(danglegeom.sdo_ordinates.COUNT - 2),
                                               NULL),
                                NULL,
                                NULL);

         --Then hold the next one as fixed
         fixpt := SDO_GEOMETRY(
                               2001,
                               danglegeom.sdo_srid,
                               SDO_POINT_TYPE(danglegeom.sdo_ordinates(danglegeom.sdo_ordinates.COUNT - 5),
                                              danglegeom.sdo_ordinates(danglegeom.sdo_ordinates.COUNT- 4),
                                              NULL),
                                NULL,
                                NULL);
      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Dangle node ' || p_dangle_node || ' doesnt match dangle edge ' || p_dangle_edge);

      END IF;


      --Figure out our measures
      moveptmeasure := SDO_LRS.FIND_MEASURE( (SDO_LRS.convert_to_lrs_geom(danglegeom)), measurept);
      fixptmeasure := SDO_LRS.FIND_MEASURE( (SDO_LRS.convert_to_lrs_geom(danglegeom)), fixpt);

      newmeasure := (moveptmeasure + fixptmeasure) * p_percent;


      --Find our new point based on new measure
      newpt := SDO_LRS.CONVERT_TO_STD_GEOM(SDO_LRS.LOCATE_PT((SDO_LRS.convert_to_lrs_geom(danglegeom)), newmeasure));


      --Use the bizarrely formatted MOVE_NODE
      --Third parm is SDO_EDGE_ARRAY
      --Which is an array of SDO_NUMBER_ARRAYS where
      --   "Each inner array consists of coordinates of each resulting attached edge, from start point to end point."
      --Thank santos cielos this is a dangle and only one inner array

      --set up our numarray first
      -- -2 since we are dropping one
      newnumarray.EXTEND(danglegeom.SDO_ORDINATES.COUNT - 2);

      --dbms_output.put_line('count is ' || danglegeom.SDO_ORDINATES.COUNT);

      FOR i in 1 .. danglegeom.SDO_ORDINATES.COUNT
      LOOP


         --dbms_output.put_line('i is ' || i);

         --could combine these but this format looks so cute
         IF p_dangle_node = startid
         AND i = 1
         THEN

            kount := kount + 1;
            newnumarray(kount) := newpt.sdo_point.X;

         ELSIF p_dangle_node = startid
         AND i = 2
         THEN

            kount := kount + 1;
            newnumarray(kount) := newpt.sdo_point.Y;

         ELSIF p_dangle_node = startid
         AND i = 3
         THEN

            NULL;

         ELSIF p_dangle_node = startid
         AND i = 4
         THEN

            NULL;


         ELSIF p_dangle_node = endid
         AND i = (danglegeom.SDO_ORDINATES.COUNT - 3)
         THEN

            kount := kount + 1;
            newnumarray(kount) := newpt.sdo_point.X;

         ELSIF p_dangle_node = endid
         AND i = (danglegeom.SDO_ORDINATES.COUNT- 2)
         THEN

            kount := kount + 1;
            newnumarray(kount) := newpt.sdo_point.Y;

         ELSIF p_dangle_node = endid
         AND i = (danglegeom.SDO_ORDINATES.COUNT)
         THEN

            --dbms_output.put_line('skip');
            NULL;

         ELSIF p_dangle_node = endid
         AND i = (danglegeom.SDO_ORDINATES.COUNT - 1)
         THEN

            --dbms_output.put_line('skip');
            NULL;

         ELSE

            kount := kount + 1;
            newnumarray(kount) := danglegeom.SDO_ORDINATES(i);

         END IF;



      END LOOP;

      SDO_TOPO_MAP.MOVE_NODE(p_topo,
                             p_dangle_node,
                             SDO_EDGE_ARRAY(newnumarray));



      --Wheres the error handling here ?

   END REAREND_NODE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------


   FUNCTION FIND_CLOSEST_ENDPOINT (
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_dangle_node     IN NUMBER,
      p_edge            IN NUMBER,
      p_coord_index     IN NUMBER
   ) RETURN NUMBER
   AS

      --NOTHING CALLS THIS as of 7/19/20
      --Does work as documented
      --Matt! 7/10/10
      --We are forced to use an existing shapepoint
      --ADD_NODE thinks wherever we are attempting to add is too close
      --Find out which candidate on this coord index segment is the one hes crabbing about

      psql           VARCHAR2(4000);
      edge_geom      SDO_GEOMETRY;
      node_geom      SDO_GEOMETRY;
      test_point     SDO_GEOMETRY;
      dist1          NUMBER;
      dist2          NUMBER;

   BEGIN

      edge_geom := GZ_CLIP.GET_EDGE_GEOMETRY(p_topo,
                                             p_edge);

      node_geom := GZ_CLIP.GET_NODE_GEOMETRY(p_topo,
                                             p_dangle_node);


      test_point := SDO_GEOMETRY(
                                 2001,
                                 node_geom.sdo_srid,
                                 SDO_POINT_TYPE(
                                                edge_geom.sdo_ordinates((p_coord_index * 2) + 1),
                                                edge_geom.sdo_ordinates((p_coord_index * 2) + 2),
                                                NULL
                                                ),
                                 NULL,
                                 NULL);

      dist1 := SDO_GEOM.SDO_DISTANCE(node_geom,
                                     test_point,
                                     .05);

      test_point := SDO_GEOMETRY(
                                 2001,
                                 node_geom.sdo_srid,
                                 SDO_POINT_TYPE(
                                                edge_geom.sdo_ordinates((p_coord_index * 2) + 3),
                                                edge_geom.sdo_ordinates((p_coord_index * 2) + 4),
                                                NULL
                                                ),
                                 NULL,
                                 NULL);

      dist2 := SDO_GEOM.SDO_DISTANCE(node_geom,
                                     test_point,
                                     .05);

      IF dist1 <= dist2
      THEN

         --start is closest
         --stay with coord_index we have
         RETURN p_coord_index;

      ELSE

         --The end of the chosen segment is the troublemaker
         --increment to the next coord index for the caller
         RETURN p_coord_index + 1;

      END IF;



   END FIND_CLOSEST_ENDPOINT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC FOR DEBUG-----------------------------------------------------------------------


   PROCEDURE ADD_VERTEX_ON_EDGE (
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_edge            IN NUMBER,
      p_node            IN SDO_GEOMETRY,
      p_coord_index     IN NUMBER
   )
   AS

      --NOTHING CALLS THIS PRESENTLY 7/19/10
      --It does work as described
      --Matt! 7/16/10
      --We are having problems adding p_node to p_edge
      --So lets try adding a vertex at that spot first
      --Then when we add a node at the same spot he wont know whats what!

      edge_geom         SDO_GEOMETRY;
      new_geom          SDO_GEOMETRY;
      ordinates         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
      ord_kount         PLS_INTEGER := 0;

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_VERTEX_ON_EDGE: Start ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      edge_geom := GZ_CLIP.GET_EDGE_GEOMETRY(p_topo, p_edge);

      ordinates.EXTEND(edge_geom.SDO_ORDINATES.COUNT + 2);

      FOR i IN 1 .. edge_geom.SDO_ORDINATES.COUNT
      LOOP


         IF ((p_coord_index * 2) + 3) = i
         THEN

            --slip the new vertex in here
            ord_kount := ord_kount + 1;
            ordinates(ord_kount) := p_node.sdo_point.X;
            ord_kount := ord_kount + 1;
            ordinates(ord_kount) := p_node.sdo_point.Y;

            ord_kount := ord_kount + 1;
            ordinates(ord_kount) := edge_geom.SDO_ORDINATES(i);

         ELSE

            ord_kount := ord_kount + 1;
            ordinates(ord_kount) := edge_geom.SDO_ORDINATES(i);

         END IF;

      END LOOP;


      new_geom := edge_geom;
      new_geom.sdo_ordinates := ordinates;

      --dbms_output.put_line(to_char(mattool.dump_sdo(edge_geom)));

      SDO_TOPO_MAP.CHANGE_EDGE_COORDS(p_topo,
                                      p_edge,
                                      --new_geom);
                                      edge_geom);


    ----------------------------------------------------------------------------------
    --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
    DBMS_APPLICATION_INFO.SET_ACTION('');
    DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_VERTEX_ON_EDGE: Peace Out ');
    --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
    ----------------------------------------------------------------------------------



   END ADD_VERTEX_ON_EDGE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION GET_EDGE_MBR (
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_edge            IN NUMBER
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 9/6/10

      psql              VARCHAR2(4000);
      output            GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT '
           || 'SDO_GEOM.SDO_MIN_MBR_ORDINATE(e.geometry,1) llx, '
           || 'SDO_GEOM.SDO_MIN_MBR_ORDINATE(e.geometry,2) lly, '
           || 'SDO_GEOM.SDO_MAX_MBR_ORDINATE(e.geometry,1) urx, '
           || 'SDO_GEOM.SDO_MAX_MBR_ORDINATE(e.geometry,2) ury '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1';
      EXECUTE IMMEDIATE psql INTO output(1),
                                  output(2),
                                  output(3),
                                  output(4) USING p_edge;

      RETURN output;


   END GET_EDGE_MBR;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION ADD_NEW_NODES (
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_edge            IN NUMBER,
      p_coord_index     IN NUMBER,
      p_node            IN SDO_GEOMETRY,
      p_node_backup     IN SDO_GEOMETRY,
      p_is_new_shape_pt IN VARCHAR2,
      p_dangle_edge     IN NUMBER,
      p_dangle_node     IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 4/03/10
      --Add a node to the topology
      --Return the new node id
      --Handle errors in a series of inexplicably wilder guesses

      output            NUMBER;
      new_coord_index   NUMBER;
      topomap           VARCHAR2(4000);
      edge_mbr          GZ_TYPES.stringarray;
      create_it         NUMBER;

   BEGIN




      output := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                      p_edge,
                                      p_node,
                                      p_coord_index,
                                      p_is_new_shape_pt);


      RETURN output;

      EXCEPTION
      WHEN OTHERS
      THEN


         --Keep in mind that one option here is to make a second attempt with the backup Sidey point
         --So far I havent encountered a case where this would help
         --Actually yes, now have, see second exception block


         --log us a clue
         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_NEW_NODES attempt initial failure ' ,p_topo || '_XX',
                                            'Adding node to edge ' || p_edge || ', coord idx is ' || p_coord_index ||
                                            ',is new shape pt is ' || p_is_new_shape_pt,NULL,NULL,NULL,NULL,NULL,
                                            SQLERRM,p_node );

         IF SQLCODE = -29532
         THEN


            IF SQLERRM LIKE '%add node results in an intersection with another edge%'
            THEN

               --oracle.spatial.topo.InvalidTopoOperationException: add node results in an intersection with another edge
               --This frequently occurs when a dangle is within tolerance of the clip outline, The distance between vertices on
               --   the state outline is usually very long, resulting in shapepoints densifying the clip line
               --In my humble opinion this dangle should have been snapped to the clip outline when they were added to the topo


               --Lets try to back this bad boy up
               --Move the dangling node away from the clip outline
               GZ_CLIP.UHAUL_NODE(p_project_id,
                                  p_jobid,
                                  p_topo,
                                  p_dangle_edge,
                                  p_dangle_node);


               BEGIN

                  output := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                                  p_edge,
                                                  p_node,
                                                  p_coord_index,
                                                  p_is_new_shape_pt);

                  RETURN output;

                  EXCEPTION
                  WHEN OTHERS
                  THEN

                     --log us a brand new clue from the cluephone
                     GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_NEW_NODES attempt second failure ' ,p_topo || '_XX',
                                            'SIDEY POWER ENGAGE: Adding node to edge ' || p_edge || ', coord idx is ' || p_coord_index ||
                                            ',is new shape pt is ' || p_is_new_shape_pt,NULL,NULL,NULL,NULL,NULL,
                                            SQLERRM,p_node );

                     --What we probably have here is a very long edge where the location that LRS
                     --calculated was actually in cartesian space
                     --and now we are off the line in geodetic
                     --so try the backup location calculated by Sidey
                     --maybe should also undo the uhaul node thing. But on the other hand, this distance could help

                     --By the power of Sidey I command thee:

                     BEGIN

                        output := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                                        p_edge,
                                                        p_node_backup,
                                                        p_coord_index,
                                                        p_is_new_shape_pt);

                        RETURN output;

                     EXCEPTION
                        WHEN OTHERS
                        THEN


                        -------------------------------------------------------------------------
                        --WHAT WE SHOULD HAVE HERE IS SOME SORT OF WRAPPER THAT MANAGES THIS MESS
                        -------------------------------------------------------------------------

                        --This is getting ridiculous
                        --But we may not have been able to back the node up enough
                        --in uhaul node because we backed into a close vertex
                        --rearend node will kick that vertex out of the way and move further
                        --could probably have a WHILE loop on this too

                        --I dont like this, probably doesnt help
                        GZ_CLIP.REAREND_NODE(p_project_id,
                                             p_jobid,
                                             p_topo,
                                             p_dangle_edge,
                                             p_dangle_node);

                        BEGIN

                           GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_NEW_NODES attempt third failure ' ,p_topo || '_XX',
                                                               'FOURTH TRY: Adding node to edge ' || p_edge || ', coord idx is ' || new_coord_index ||
                                                               ',is new shape pt is FALSE',NULL,NULL,NULL,NULL,NULL,
                                                               SQLERRM,p_node );

                           output := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                                           p_edge,
                                                           p_node,
                                                           p_coord_index,
                                                           p_is_new_shape_pt);

                           RETURN output;



                        EXCEPTION
                        WHEN OTHERS
                        THEN

                              --One far-fetched possibility, has never helped
                              --GZ_CLIP.ADD_VERTEX_ON_EDGE(p_jobid,
                                                         --p_topo,
                                                        -- p_edge,
                                                        -- p_node,
                                                        -- p_coord_index);


                              --output := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                                              --p_edge,
                                                              --p_node,
                                                              --p_coord_index,
                                                              --'FALSE');  --Theres now a vertex right at our node spot





                              --More likely problem however...
                              --We have selected a new point that SDO_RELATE does not think is "EQUAL" to
                              --an existing vertex on the clip outline
                              --But ADD_NODE disagrees, its too close for it, whatever that means

                              --Or actually, more likely (7/19/10) because we are clueless, we have created something invalid in the topology
                              --For example, the edge on which we are adding a node doesnt accurately
                              --match its neighbors at its endpoints
                              --For now we want to die here and find out why

                              IF UPPER(SQLERRM) LIKE UPPER('%changed edge coordinate string has an intersection with another edge%')
                              THEN

                                 GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_NEW_NODES attempt fourth failure ' ,p_topo || '_XX',
                                                               'Error message suggests that ' || p_topo || '_edge$ edge ' || p_edge
                                                               || ' itself is overlapping a neighbor',NULL,NULL,NULL,
                                                               'SDO_TOPO_MAP.ADD_NODE(''' || p_topo || ''',''' || p_edge || ''','''
                                                               || 'geom-->,''' || p_coord_index || ''',''' || p_is_new_shape_pt || ''');',
                                                               NULL,SQLERRM,p_node );


                                 RAISE;

                              ELSE

                                 GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_NEW_NODES attempt fourth failure ' ,p_topo || '_XX',
                                                               'Probably the wrong coord index adding to ' || p_topo || '_edge$ edge ' || p_edge
                                                               ,NULL,NULL,NULL,
                                                               'SDO_TOPO_MAP.ADD_NODE(''' || p_topo || ''',''' || p_edge || ''','''
                                                               || 'geom-->,''' || p_coord_index || ''',''' || p_is_new_shape_pt || ''');',
                                                               NULL,SQLERRM,p_node );

                                 RAISE;

                              END IF;



                              --FOOLPROOF Solution
                              --Commented, do not want to use it
                              --If implemented, will need to note in the output that this edge is suspect
                              --Just connect to the closest existing node
                              --new_coord_index := GZ_CLIP.FIND_CLOSEST_ENDPOINT(p_jobid,
                                                                               --p_topo,
                                                                               --p_dangle_node,
                                                                               --p_edge,
                                                                               --p_coord_index);


                             --BEGIN

                                --GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_NEW_NODES attempt third failure ' ,p_topo || '_XX',
                                                                    --'FIFTH TRY: Adding node to edge ' || p_edge || ', coord idx is ' || new_coord_index ||
                                                                   -- ',is new shape pt is FALSE',NULL,NULL,NULL,NULL,NULL,
                                                                   -- SQLERRM,p_node );

                                --output := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                                                --p_edge,
                                                                --p_node,
                                                                --new_coord_index,
                                                                --'FALSE');

                                --RETURN output;

                             --EXCEPTION
                             --WHEN OTHERS
                            -- THEN

                                --RAISE;


                             --END;

                        END;

                     END;

                  END;


            ELSIF SQLERRM LIKE '%is not on the boundary of one or two of the faces it links%'
            THEN

               --This is a mystery error
               --using an explicit topomap seems to resolve it
               --maybe should be doing all of the node adding like this actually

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_NEW_NODES attempt 2 ' ,p_topo || '_XX',
                                            'Opening an explicit topomap and then adding node to edge ' || p_edge
                                            || ', coord idx is ' || p_coord_index ||
                                            ',is new shape pt is ' || p_is_new_shape_pt,
                                            NULL,NULL,NULL,NULL,NULL,NULL,p_node );

               topomap := p_topo || '_TOPOMAP';

               edge_mbr := GZ_CLIP.GET_EDGE_MBR(p_project_id,
                                                p_jobid,
                                                p_topo,
                                                p_edge);

               create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topomap,
                                                             p_topo,
                                                             2,
                                                             edge_mbr(1),
                                                             edge_mbr(2),
                                                             edge_mbr(3),
                                                             edge_mbr(4));

               output := SDO_TOPO_MAP.ADD_NODE(NULL,  --topomap
                                               p_edge,
                                               p_node,
                                               p_coord_index,
                                               p_is_new_shape_pt);

               create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topomap,
                                                             p_topo,
                                                             create_it);


               RETURN output;

            ELSIF SQLERRM LIKE '%Add node results in two pieces of split edge intersecting each other%'
            THEN

               --This means we probably are working with the wrong coord index
               --No error handling here per se (yet) requires fix in the caller

               RAISE;

            ELSE

               RAISE;

            END IF;


         ELSE

            IF UPPER(SQLERRM) LIKE UPPER('%is not on the boundary of one or two of the faces it links%')
            THEN

               -- ORA-29532: Java call terminated by uncaught Java exception: oracle.spatial.topo.TopoValidationException:
               -- Edge 27490 is not on the boundary of one or two of the faces it links

               --Do not have a handle on this
               --The edge complained about is usually a neighboring edge, not the one where we are adding the node
               RAISE;

            ELSE



               -- I dont know whats going on
               -- (even more than usual)
               RAISE;

            END IF;

         END IF;


   END ADD_NEW_NODES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------


   FUNCTION GET_EXISTING_NODE (
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_newtopo         IN VARCHAR2,
      p_edge            IN VARCHAR2,
      p_coord_index     IN NUMBER,
      p_dangle_edge     IN NUMBER,
      p_dangle_node     IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 7/09/10
      --We have an existing node where we want to connect our dangle
      --Figure out the node id based on the coord index
      --Then back the dangle up since its so close to our short edge that we
      --   are likely about to make a huge mess

      /*
      --Heres what we probably have

                      0
      X--------------X| <-- Very short edge
                      0

      OR
                     0
      X-------------X| <-- Very close sliver creator
                     |
                     |

      --Turn it into more like

                      0
      X--------X      |
                      0

      --Before we attempt to connect the dangle X to one of the node 0s
      */

      output     NUMBER;
      psql       VARCHAR2(4000);

   BEGIN

      psql := 'SELECT e.';

      IF p_coord_index = 0
      THEN
         psql := psql || 'start';
      ELSE
         psql := psql || 'end';
      END IF;

      psql := psql || '_node_id '
            || 'FROM ' || p_newtopo || '_edge$ e '
            || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO output USING p_edge;


      --Beep beep beep

      --This may also fail, if so keep going
      BEGIN
         GZ_CLIP.UHAUL_NODE(p_project_id,
                            p_jobid,
                            p_newtopo,
                            p_dangle_edge,
                            p_dangle_node);
      EXCEPTION
      WHEN OTHERS THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'GET_EXISTING_NODE' ,p_newtopo || '_NODE$',
                                             'Exception when we tried to back up ' || p_dangle_node ||
                                             ', we will just keep on going',NULL,NULL,NULL,NULL,NULL,SQLERRM);

      END;

      RETURN output;


   END GET_EXISTING_NODE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION EXTEND_EDGE(
      p_topo            IN VARCHAR2,
      p_edge            IN NUMBER,
      p_edge_geom       IN SDO_GEOMETRY,
      p_end_node        IN NUMBER,
      p_new_end_node    IN NUMBER,
      p_tip             IN VARCHAR2 --start or end
   ) RETURN VARCHAR2
   AS

   --NOTHING CALLS THIS 4/21/10
   --Matt! 4/19/10
   --extend an edge using sdo_topo_map.move_edge


      output            VARCHAR2(1) := '0';
      psql              VARCHAR2(4000);
      new_node_geom     SDO_GEOMETRY;
      silly_num_array   SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY();


   BEGIN

      silly_num_array.EXTEND(p_edge_geom.sdo_ordinates.count + 2);

      --We dont trust the geometry we used to create the new nodes
      --Get the oracle x y

      psql := 'select n.geometry '
           || 'FROM ' || p_topo || '_node$ n '
           || 'WHERE '
           || 'n.node_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO new_node_geom USING p_new_end_node;



      IF p_tip = 'START'
      THEN

         --we want direction of new node to old node
         silly_num_array(1) := new_node_geom.sdo_point.X;
         silly_num_array(2) := new_node_geom.sdo_point.Y;

         FOR i in 1 .. p_edge_geom.sdo_ordinates.COUNT
         LOOP

            silly_num_array(i+2) := p_edge_geom.sdo_ordinates(i);

         END LOOP;


      ELSIF p_tip = 'END'
      THEN

         --we want direction of old edge running toward new node

         FOR i in 1 .. p_edge_geom.sdo_ordinates.COUNT
         LOOP

            silly_num_array(i) := p_edge_geom.sdo_ordinates(i);

         END LOOP;

         silly_num_array(p_edge_geom.sdo_ordinates.count + 1) := new_node_geom.sdo_point.X;
         silly_num_array(p_edge_geom.sdo_ordinates.count + 2) := new_node_geom.sdo_point.Y;

      ELSE
         RAISE_APPLICATION_ERROR(-20001,'We are lost in the rough here');
      END IF;


      --Check to see if our new planned edge crosses some other edges
      --If we were using a topomap could we detect interactions between
      --   this edge and one that we already added in this function?

         /*
         psql := 'SELECT count(*) '
              || 'FROM ' || p_topo || '_EDGE$ e '
              || 'WHERE e.edge_id NOT IN (:p1,:p2) AND '
              || 'SDO_RELATE(e.geometry,:p3,:p4) = :p5 ';

         EXECUTE IMMEDIATE psql INTO kount USING p_edges(i),
                                                 p_connect_edges(i),
                                                 new_edge_geom,
                                                 'mask=ANYINTERACT',
                                                 'TRUE';

         IF kount > 0
         THEN
            RAISE_APPLICATION_ERROR(-20001,'Havent dealt with this yet');
         END IF;
         */

      SDO_TOPO_MAP.MOVE_EDGE(p_topo,
                             p_edge,
                             p_end_node,
                             p_new_end_node,
                             silly_num_array);

      /*
         IF p_tips(i) = 'START'
         THEN
           --we want direction of new node to old node
           output(i) := SDO_TOPO_MAP.ADD_EDGE(p_topo,
                                              p_new_node_ids(i),
                                              p_old_node_ids(i),
                                              new_edge_geom);

         ELSIF p_tips(i) = 'END'
         THEN
           --we want direction of old node to new node
           output(i) := SDO_TOPO_MAP.ADD_EDGE(p_topo,
                                              p_old_node_ids(i),
                                              p_new_node_ids(i),
                                              new_edge_geom);

         ELSE
            RAISE_APPLICATION_ERROR(-20001,'not possible, act like ya heard');
         END IF;
      */



      RETURN output;


   END EXTEND_EDGE;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

    FUNCTION ADD_NEW_EDGES (
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_old_node_id     IN NUMBER,
      p_new_node_id     IN NUMBER,
      p_tip             IN VARCHAR2,
      p_edge            IN NUMBER,
      p_connect_count   IN NUMBER DEFAULT NULL
      --p_connect_edge    IN NUMBER  --Dont have this anymore in caller
   ) RETURN NUMBER
   AS

      --Matt! 4/03/10
      --Add a new edge
      --Return the new edge ids
      --Handle errors somehow
      --Rewrite for single edge processing 4/21/10
      --Allow for backed up dangle nodes 6/3/10

      new_edge_id       NUMBER;
      psql              VARCHAR2(4000);
      new_node_geom     SDO_GEOMETRY;
      old_node_geom     SDO_GEOMETRY;
      new_edge_geom     SDO_GEOMETRY;
      kount             PLS_INTEGER;
      connect_edges_2   GZ_TYPES.stringarray;
      allow_kount       PLS_INTEGER;



   BEGIN



      --We dont trust the geometry we used to create the new nodes
      --Get the oracle x y and put in an array

      psql := 'select n.geometry '
           || 'FROM ' || p_topo || '_node$ n '
           || 'WHERE '
           || 'n.node_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO new_node_geom USING p_new_node_id;

      --We also no longer trust the dangle node since we may have backed
      --him up to resolve other issues
      --We could handle this scenario with error handlers, but I'm valuing
      --clean code over efficiency

      psql := 'select n.geometry '
           || 'FROM ' || p_topo || '_node$ n '
           || 'WHERE '
           || 'n.node_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO old_node_geom USING p_old_node_id;



      IF p_tip = 'START'
      THEN

         --we want direction of new node to old node
         new_edge_geom := SDO_GEOMETRY(2002,
                                       old_node_geom.SDO_SRID,
                                       NULL,
                                       SDO_ELEM_INFO_ARRAY(1, 2, 1),
                                       SDO_ORDINATE_ARRAY(new_node_geom.sdo_point.X,
                                                          new_node_geom.sdo_point.Y,
                                                          old_node_geom.sdo_point.X,
                                                          old_node_geom.sdo_point.Y
                                                          )
                                   );

      ELSIF p_tip = 'END'
      THEN

         --we want direction of old node to new node
         new_edge_geom := SDO_GEOMETRY(2002,
                                       old_node_geom.SDO_SRID,
                                       NULL,
                                       SDO_ELEM_INFO_ARRAY(1, 2, 1),
                                       SDO_ORDINATE_ARRAY(old_node_geom.sdo_point.X,
                                                          old_node_geom.sdo_point.Y,
                                                          new_node_geom.sdo_point.X,
                                                          new_node_geom.sdo_point.Y
                                                          )
                                   );


      ELSE

         RAISE_APPLICATION_ERROR(-20001,'We are lost in the rough here');

      END IF;

      --Find the edges in the topology that are supposed to touch this new edge
      --
      -- ? split edge   p_connect_edge (original, we new it outside this scope )
      --      -------*------
      --             |
      --             |  <-- New edge, doesnt exist in topo yet

      psql := 'SELECT e.edge_id FROM '
           || p_topo || '_edge$ e '
           || 'WHERE (e.start_node_id = :p1 '
           || 'OR e.end_node_id = :p2) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO connect_edges_2 USING p_new_node_id,
                                                                     p_new_node_id;

      IF connect_edges_2.COUNT != 2
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid, 'ADD_NEW_EDGES',p_topo || '_edge$','Extending ' || p_topo || '_EDGE$ edge ' || p_edge
                                          || ' between old node ' || p_old_node_id || ' and new node ' || p_new_node_id
                                          || '. New node splits these edges: ' || connect_edges_2(1) || ' , ' || connect_edges_2(2) || '.'
                                          || ' Tip is ' || p_tip,NULL,NULL,NULL,NULL,NULL,NULL,
                                             new_edge_geom);

         RAISE_APPLICATION_ERROR(-20001,'We should only have 2 edges initially connected to our new node. New node id is ' || p_new_node_id);

      END IF;


      --Check to see if our new planned edge crosses some other edges
      --If we were using a topomap could we detect interactions between
      --   this edge and one that we already added in this function?
      --Not clear from the documentation if spatial checks this automatically
      --   It definitely does when MOVING an edge


      psql := 'SELECT count(*) '
           || 'FROM ' || p_topo || '_EDGE$ e '
           || 'WHERE e.edge_id NOT IN (:p1,:p2,:p3) AND '
           || 'SDO_RELATE(e.geometry,:p4,:p5) = :p6 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_edge,
                                              connect_edges_2(1),
                                              connect_edges_2(2),
                                              new_edge_geom,
                                              'mask=ANYINTERACT',
                                              'TRUE';

      --We allow the V connected guy to slum in here too
      --ordinarily we want to touch to no one else
      --But in that case we allow one more (the anonymous other side of the V)
      IF p_connect_count IS NULL
      THEN

         allow_kount := 0;

      ELSE

         allow_kount := p_connect_count;

      END IF;

      IF kount > allow_kount
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid, 'ADD_NEW_EDGES',p_topo || '_edge$','Extending ' || p_topo || '_EDGE$ edge ' || p_edge
                                      || ' between old node ' || p_old_node_id || ' and new node ' || p_new_node_id
                                      || '. New node splits these edges: ' || connect_edges_2(1) || ' , ' || connect_edges_2(2) || '.'
                                      || ' Tip is ' || p_tip);

         RAISE_APPLICATION_ERROR(-20001,'Havent dealt with this yet, edge is ' || p_edge || ' connect edge is ' ||
                                 connect_edges_2(1) || ' ,' || connect_edges_2(2) || ' edge geom is in log '); --|| to_char(mattool.dump_sdo(new_edge_geom)) );
      END IF;


     --Lets write a book to the log before the inevitable errors
     GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid, 'ADD_NEW_EDGES',p_topo || '_edge$','Extending ' || p_topo || '_EDGE$ edge ' || p_edge
                                      || ' between old node ' || p_old_node_id || ' and new node ' || p_new_node_id
                                      || '. New node splits these edges: ' || connect_edges_2(1) || ' , ' || connect_edges_2(2) || '.'
                                      || ' Tip is ' || p_tip);




         IF p_tip = 'START'
         THEN
           --we want direction of new node to old node

            BEGIN

               new_edge_id := SDO_TOPO_MAP.ADD_EDGE(p_topo,
                                                    p_new_node_id,
                                                    p_old_node_id,
                                                    new_edge_geom);

            EXCEPTION
            WHEN OTHERS THEN

               IF SQLCODE = -29532
               THEN

                  GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_NEW_EDGES',p_topo || '_edge$','ERROR, check the call-->',NULL,NULL,NULL,
                                                      'SDO_TOPO_MAP.ADD_EDGE_ID('''||p_topo||''','||p_new_node_id||','||p_old_node_id||','||'<GEOM>)',
                                                      NULL,SQLERRM,new_edge_geom);
                  RAISE_APPLICATION_ERROR(-20001,'We know what this means: ' || SQLERRM);

               ELSE

                  RAISE;

               END IF;

            END;

         ELSIF p_tip = 'END'
         THEN

            BEGIN

               --we want direction of old node to new node
               new_edge_id := SDO_TOPO_MAP.ADD_EDGE(p_topo,
                                                    p_old_node_id,
                                                    p_new_node_id,
                                                    new_edge_geom);

           EXCEPTION

              WHEN OTHERS THEN

                 IF SQLCODE = -29532
                 THEN

                    GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_NEW_EDGES',p_topo || '_edge$','ERROR, check the call-->',NULL,NULL,NULL,
                                                        'new_edge_id := SDO_TOPO_MAP.ADD_EDGE('||p_topo||','||p_old_node_id||','||p_new_node_id||','||'<GEOM>);',
                                                        NULL,SQLERRM,new_edge_geom);
                    RAISE_APPLICATION_ERROR(-20001,'We know what this means: ' || SQLERRM);

                 ELSE

                    RAISE;

                 END IF;


           END;


         ELSE
            RAISE_APPLICATION_ERROR(-20001,'not possible, act like ya heard');
         END IF;


      RETURN new_edge_id;


   END ADD_NEW_EDGES;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION COMBINE_EDGES (
      p_topo            IN VARCHAR2,
      p_feature_table   IN VARCHAR2,
      p_feature_edge    IN NUMBER,
      p_new_edge        IN NUMBER
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 4/03/10
      --Create some geometries based on original features plus extensions
      --Rewrite for single edge processing 4/21/10
      --NOTHING CALLS THIS any more


      output         SDO_GEOMETRY;
      psql           VARCHAR2(4000);

   BEGIN


      --cant just get the geometry of the feature, it may have been edited
      --need to aggr_union the topo to get the current feature
      --add in the new edge too
      psql := 'SELECT SDO_AGGR_CONCAT_LINES(e.geometry) '
           || 'FROM ' || p_feature_table || ' a, '
           ||  p_topo || '_RELATION$ r, '
           ||  p_topo || '_EDGE$ e '
           || 'WHERE '
           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'a.topogeom.tg_id = r.tg_id AND '
           || '(r.topo_id = e.edge_id or e.edge_id = :p1) AND '
           || 'a.edge_id = :p2';

      EXECUTE IMMEDIATE psql INTO output USING p_new_edge,
                                               p_feature_edge;


      RETURN output;

   END COMBINE_EDGES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION UPDATE_SOME_TOPO (
      p_topo            IN VARCHAR2,
      p_feature_table   IN VARCHAR2,
      p_topo_col        IN VARCHAR2,
      p_feature_pkc     IN VARCHAR2,
      p_feature_edge    IN NUMBER,
      p_new_edge_id     IN NUMBER
   ) RETURN VARCHAR2
   AS

      --Matt! 4/27/10
      --For some dangling feature edge, we have created a new edge that extends
      --   from the dangle to the clip outline
      --Here we use the topogeom constructor to add the new edge to the feature
      --   so it is no longer dangling

      ret_val              VARCHAR2(4000) := '0';
      psql                 VARCHAR2(4000);
      tg_layer_id          NUMBER;

   BEGIN


      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_topo,
                                                  p_feature_table,
                                                  p_topo_col,
                                                  'LINE');


      psql := 'UPDATE ' || p_feature_table || ' a '
           || 'SET '
           || 'a.' || p_topo_col || ' = SDO_TOPO_GEOMETRY( '
           || ':p1, '
           || ':p2, '
           || ':p3, '
           || 'SDO_TOPO_OBJECT_ARRAY( '
           || 'SDO_TOPO_OBJECT (:p4,:p5)), '
           || 'NULL) '
           || 'WHERE '
           || 'a.' || p_feature_pkc || ' = :p6 ';

      EXECUTE IMMEDIATE psql USING p_topo,
                                   2,
                                   tg_layer_id,
                                   p_new_edge_id,
                                   2,
                                   p_feature_edge;
       COMMIT;


      --Thought about updating sdo while at it, but sdo col would be correct just for this
      --extend dangles edit.  Why bother?

      --will have more info on what to return later
      RETURN ret_val;

   END UPDATE_SOME_TOPO;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------


   PROCEDURE ZAP_NODE (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   )
   AS

      --Matt! Taken and modified 9/6/12 GZ_from topofix
      --Wrapper for remove_node, uses explicit topomap

      edge_ids             GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);
      window_geom          SDO_GEOMETRY;
      create_it            NUMBER;


   BEGIN


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

         --Expecting future errors here.  Consider changing topomap window, etc

         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

         RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

      END;

      --commit and drop temp topomap
      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

   END ZAP_NODE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION REMOVE_DANGLE_NODES (
      p_topo            IN VARCHAR2,
      p_node_ids        IN GZ_TYPES.stringarray
   ) RETURN VARCHAR2
   AS

      --Matt! 4/03/10
      --We've extended our dangles
      --The nodes that were previously the end of the dangles should be removed
      --9/6/12 Added error handler and call to bespoke zap_node

      ret_val        VARCHAR2(4000) := '0';

   BEGIN

      FOR i in 1 .. p_node_ids.COUNT
      LOOP

         IF p_node_ids(i) > 0 --sometimes we add fake negative nodes for silly purposes
         THEN

            BEGIN

               --This is the implicit topomap version, which generally works
               --Presumably Oracle knows efficiently what size topomap to use, but sometimes it will error
               SDO_TOPO_MAP.REMOVE_NODE(p_topo, p_node_ids(i));

            EXCEPTION
            WHEN OTHERS
            THEN

               IF SQLERRM LIKE '%not found in cache%'
               THEN

                  --ORA-29532: Java call terminated by uncaught Java exception: oracle.spatial.topo.TopoEntityNotFoundException:
                  --Face ID 193 not found in cache ORA-06512: at "MDSYS.SDO_TOPO_MAP", line 337

                  --Using my own (presumably too big) window generally resolves this.

                  GZ_CLIP.ZAP_NODE(p_topo,
                                   p_node_ids(i));


               ELSE

                  --Should we capture errors here?
                  --For now lets die and discover what could go wrong
                  --but ultimately leaving these nodes shouldn't really hurt

                  --here's one error:
                  --ORA-29532: Java call terminated by uncaught Java exception:
                  --   oracle.spatial.topo.InvalidTopoOperationException: Attempt to remove node ID 140 which bounds 3 or more edges
                  --   Not a real error, happened on an uncleaned up rerun
                  --ORA-29532: Java call terminated by uncaught Java exception:
                  --   oracle.spatial.topo.InvalidTopoOperationException: Node ID 14 has mismatched lineal features on each side and can't be removed

                  RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

               END IF;

            END;



         END IF;

      END LOOP;

      RETURN ret_val;


   END REMOVE_DANGLE_NODES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public --------------------------------------------------------------------------------

   FUNCTION NODE_EXISTS (
      p_schema         IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo           IN VARCHAR2,
      p_test_table     IN VARCHAR2,
      p_edge_id        IN NUMBER,
      p_tip            IN VARCHAR2
   ) RETURN PLS_INTEGER
   AS

      --Matt! 9/3/10
      --I have a node that I want to use on the clip outline
      --Its either the start or the end of this edge I fancy
      --But I want to make sure something in the interior isnt already using that node

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(e.edge_id) '
           || 'FROM '
           || p_test_table || ' a, '
           || p_topo || '_relation$ r, '
           || p_topo || '_edge$ e, '
           || p_topo || '_edge$ ee '
           || 'WHERE '
           || 'a.topogeom.tg_id = r.tg_id AND '
           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'r.topo_id = e.edge_id AND '
           || '(e.start_node_id = ee.' || p_tip || '_node_id OR e.end_node_id = ee.' || p_tip || '_node_id) AND '
           || 'ee.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_edge_id;

      IF kount >= 1
      THEN

         --Bad but TRUE, node exists
         RETURN 1;

      ELSE

         --Good, but FALSE node does not exist
         RETURN 0;

      END IF;



   END NODE_EXISTS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private -------------------------------------------------------------------------------

   FUNCTION GET_EDGE_NODES (
      p_schema         IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo           IN VARCHAR2,
      p_edge_id        IN NUMBER,
      p_null_srid      IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_CLIP.geomarray
   AS

      --Matt! 9/6/10

      psql              VARCHAR2(4000);
      output            GZ_CLIP.geomarray;
      startnode         NUMBER;
      endnode           NUMBER;

   BEGIN

      psql := 'SELECT e.start_node_id, e.end_node_id '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO startnode,
                                  endnode USING p_edge_id;

      output(1) := GZ_CLIP.GET_NODE_GEOMETRY(p_topo, startnode);
      output(2) := GZ_CLIP.GET_NODE_GEOMETRY(p_topo, endnode);

      IF p_null_srid IS NOT NULL
      THEN
         output(1).sdo_srid := NULL;
         output(2).sdo_srid := NULL;
      END IF;

      RETURN output;


   END GET_EDGE_NODES;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE -------------------------------------------------------------------------------

   FUNCTION sincos(InX NUMBER,COSX IN OUT NOCOPY NUMBER) RETURN NUMBER DETERMINISTIC AS
/**

 Program Name: sincos
 Author: Sidey Timmins
 Creation Date: 11/16/2006

 Usage:
    Call this program from inside another PL/SQL program.  This program
    has 2 parameters:

     REQUIRED Parameters:
              Input:            X:  X input value (in radians)
              Output:        cosx: will contain cosine of X on output.

 Purpose:
              Calculate simultaneously sin(x) and cos(x) -
              the sine and cosine functions as fast as possible (3 times faster
              than the built in functions that take 1 second for 20000 sine
              and cosine values) with about 19 digits of accuracy. Worst is
              15 digits of accuracy near zero and pi.

 Accuracy:
    Over the range - two pi to two pi by 1/10000 the maximum errors are:
           sine       -.001884954475928113753023025004501920291813 correct
           cosx       -.001884954475928117129897194923190991656619 very close
           error       .000000000000000003376874169918689071364806
           cosine      .8846460864518815658418479233550142026277  correct
           cosx        .8846460864518815658354808691145121305329  very close
           error       .0000000000000000000063670542405020720948
    Near pi
           sine       -.00150796390221547396236390642315296548 cos
           sincos     -.001507963902215478183896385939205188475162
           cosine      -.99999886302178844781346870679287144982
           cosx        -.999998863021788447807102780963812085428

 Reference:
    http://cache-www.intel.com/cd/00/00/29/37/293747_293747.pdf
    ?originalurl=http://www.intel.com/cd/ids/developer/asmo-na/eng/293747.htm
    The title of the pdf is:
    "Slerping Clock Cycles" JMP van Waveren, Id Software, Inc
    For cosine code
    http://www.ganssle.com/approx/approx.pdf
    "A Guide to Approximations" by Jack G Ganssle

 Updates:
    with better approximations for cosine on 11/22
    allowed range to be the real number line

 Dependencies:
    None but see c_test_sincos for rigorous testing.

 Modification History:
    11/28/2006, Sidey Timmins
    09/14/2007, Nick Padfield - Ported code into the CDB_POLYSIMPLIFY2 Package
    09/17/2007 Sidey Timmins Added decimal points to literals.

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

  -- This approximation is better for small angles (1st reference) and gets
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

 -- The cosine is an even function

  IF Abs(xx) > pibyTwo and Abs(xx) < (pi3by2) THEN
      cosx := -cosx;
  END IF;

  RETURN sinx;

EXCEPTION
   WHEN OTHERS THEN
    dbms_output.put_line('FIX SLIVERS ERROR: '||SQLERRM);
      -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
END sincos;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public --------------------------------------------------------------------------------

   FUNCTION APPROX_GC_DISTANCE (
      x1 IN OUT NOCOPY NUMBER,
      y1 IN OUT NOCOPY NUMBER,
      x2 IN OUT NOCOPY NUMBER,
      y2 IN OUT NOCOPY NUMBER
   ) RETURN NUMBER DETERMINISTIC
   AS


      --Program Name: approx_gc_distance
      --Author: Sidey Timmins
      --Creation Date: 06/29/2009
      --Usage:
        -- Call this function from inside a PL/SQL program.

        --   REQUIRED Parameters:
        --        INPUT
        --             x1,y1:  vertex coordinates in degrees.
        --             x2,y2:  2nd vertex coordinates in degrees.
        --
      --Purpose:   -- Calculate the approximate great circle distance between 2 vertices
      --              in meters quickly and very accurately from geodetic coordinates.
      --              Note it approximates the values given by Oracle length - not
      --              those given by the Vincenty formula - although they should be the same.
      --              Percentage accuracy which is <0.0002% at .1 degrees decreases
      --              to < 0.2% at 0.00001 degrees.
      -- Method:      Uses empirically derived constants to approximate the distance
      --              using the arcsin formula for the GCD. The constants were
      --              derived in part by Rosenbrock optimization and a HP15c calculator!
      -- References:   http://en.wikipedia.org/wiki/Great_circle_distance
      -- Interesting reading follows:
      --   http://local.wasp.uwa.edu.au/~pbourke/geometry/ellipsecirc/
      --   http://local.wasp.uwa.edu.au/~pbourke/geometry/ellipsecirc/Necat2.pdf
      --Dependencies: FIX_SLIVERS.sincos  (a fast sin and cos function)


     deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
     length      NUMBER := 0.;
     y           NUMBER := abs(y1);
     dx          NUMBER;
     dy          NUMBER;
     siny        NUMBER;
     cosy        NUMBER;
     cosyy       NUMBER;
     cosy2       NUMBER;

     yfactor     NUMBER;
     dist_factor number := 111120.;

   BEGIN

         siny := GZ_CLIP.sincos(y*deg2rad,cosyy);
         dx := x2-x1;
         dy := y2-y1;

   -- These new constants using siny ^2 give less than 0.03% error up to
   -- delta latitude  = delta longitude = 0.1 degrees

         if y < 16. then
   --        cosy := cosyy* (1.0017948 + siny*siny*0.00335903);
           cosy := cosyy* (1.001795272 + siny*siny*0.0033543); -- much better
           yfactor := 0.9950886 + (0.010041229*siny*siny);
           if abs(dy) < 0.00001 and dx = 0. then
               yfactor := 1.;
            end if;

         elsif y < 33. then

           cosy := cosyy* (1.0017937 + siny*siny*0.00336514);
           yfactor := 0.995085 + (0.010032*siny*siny);
           if abs(dy) < 0.000005 and dx = 0. then
               yfactor := 1.;
            end if;

         elsif y < 51. then
           cosy := cosyy* (1.0017937+ siny*siny*0.00336514);
           yfactor := 0.995072 + (0.010072174*siny*siny);
           if abs(dy) < 0.000005 and dx = 0. then
               yfactor := 1.;
            end if;

         else -- Alaska
              cosy := cosyy* (1.00179492 + siny*siny*0.00336508);
              yfactor := 0.995049 + (0.01011339*siny*siny);
            if abs(dy) < 0.000005 and dx = 0. then
               yfactor := 1.;
            end if;
         end if;
         yfactor := yfactor* yfactor;

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
             cosy2 := cosy - siny * dy*deg2rad; -- small angle approximation for cos(y2)
   --          if cosy2 < 0. then
   --            dbms_output.put_line('cosy2 ' || cosy2 || ' dy ' || dy || ' cosy ' || cosy || ' siny ' || siny);
   --          end if;
             length := sqrt(dy*dy*yfactor + dx*dx*cosy*cosy2) * dist_factor;
          else
             if abs(dx) < 0.0000065 then
               cosyy := (cosy + cosyy)*0.5;
             else
               cosyy := cosy;
             end if;

             cosy2 := cosyy - siny * dy*deg2rad; -- small angle approximation for cos(y2)
             length := sqrt(dy*dy*yfactor + dx*dx*cosyy*cosy2) * dist_factor;
          end if;
         RETURN Length;

   EXCEPTION
      WHEN OTHERS THEN
      RAISE;
       dbms_output.put_line('APPROX_GC_DISTANCE: '||SQLERRM);
      --CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
   END APPROX_GC_DISTANCE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public -------------------------------------------------------------------------------

   FUNCTION PERPENDICULAR (
      xin IN OUT NOCOPY NUMBER,
      yin IN OUT NOCOPY NUMBER,  -- the point
      x1 IN OUT NOCOPY NUMBER,
      y1 IN OUT NOCOPY NUMBER,  -- the line
      x2 IN OUT NOCOPY NUMBER,
      y2 IN OUT NOCOPY NUMBER,
      xnear IN OUT NOCOPY NUMBER,
      ynear IN OUT NOCOPY NUMBER,  -- the perpendicular
      always BOOLEAN default FALSE,  -- always return a distance
      meters BOOLEAN default FALSE
   )  -- when true return meters
   RETURN NUMBER deterministic
   AS
/*
********************************************************************************
--Program Name: Perpendicular
--Author: Sidey Timmins
--Creation Date: 10/16/2008

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
             Distance := GZ_CLIP.approx_gc_distance(xin,yin,xnear,ynear);
           else
             Distance := ((Xin - xnear) * (Xin - xnear) + (Yin - ynear) * (Yin - ynear));
           end if;
--        dbms_output.put_line('distance ' || distance);
        elsif length_sq = 0. then  -- zero length line
           NULL;
        end if;
     end if;
     RETURN Distance;
EXCEPTION
   WHEN OTHERS THEN
    dbms_output.put_line('FIX SLIVERS ERROR: '||SQLERRM);
    --CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
END;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE -------------------------------------------------------------------------------

   FUNCTION ADD_A_NODE (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_node_id        IN NUMBER,  --dangling node
      p_edge_id        IN NUMBER,  --dangling edge
      p_v_flag         IN NUMBER DEFAULT NULL,
      p_tolerance      IN NUMBER DEFAULT .05,
      p_debug          IN PLS_INTEGER DEFAULT NULL  --passed in for dbms
   ) RETURN NUMBER
   AS

      --Matt! 7/13/10
      --Pulled this core section of EXTEND_INTERIOR_DANGLES out
      --so it can get called by other spots (most notably extend dangling nodes uses same functionality)
      --Start with a dangling node id of some sort
      --End when we have a new node in the topology that completes it in some manner
      --Probably not tracking all updates!
      --IN LRS +10 -10 section check if the psql anyinteract edge_ids are just the two expected 2/2/11
      --Allow the shifty_measure section to shifty more then +1 or -1 coord indexes away 9/12/12
      --More rigorous touchkount kounting to deal with geodetic vs cartesian diffs 9/28/12
      --In desperate >20 code added check on number of intersections with the full target UF edge 8/26/13

      psql                 VARCHAR2(4000);
      psql2                VARCHAR2(4000);
      clip_parms           GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      newedgetable         VARCHAR2(32);
      newtopo              VARCHAR2(32);
      newcliptable         VARCHAR2(32);
      connectedge          VARCHAR2(4000);
      connectedgegeom      SDO_GEOMETRY;
      connectedgelrs       SDO_GEOMETRY;
      nodegeom             SDO_GEOMETRY;
      measure              NUMBER;
      coord_index          NUMBER;
      tuck_coord_index     NUMBER;
      the_segment          GZ_TYPES.stringarray;
      dist                 NUMBER;
      xnear                NUMBER;
      ynear                NUMBER;
      new_node_geom        SDO_GEOMETRY;
      sideypoint           SDO_GEOMETRY;
      extend_retval        VARCHAR2(4000);
      combined_edge        SDO_GEOMETRY;
      unfixed_dangles      GZ_TYPES.stringarray;
      orig_index           PLS_INTEGER;
      new_edge_id          NUMBER;
      new_node_id          NUMBER;    --RETURNed
      test_node            SDO_GEOMETRY;
      tucked_srid          NUMBER;
      shortedgeflag        PLS_INTEGER := 0;
      vtx_count            NUMBER;
      test_segment         SDO_GEOMETRY;
      touchkount           PLS_INTEGER;
      touch_edges          GZ_TYPES.stringarray;
      touchtimes           PLS_INTEGER := 0;
      rejected_edges       VARCHAR2(4000);
      rejected_edges_root  VARCHAR2(4000);
      shortykount          PLS_INTEGER;

      --FLAGS with default values
      existing_node_flag   PLS_INTEGER := 0;
      is_new_shape_pt      VARCHAR2(4000) := 'TRUE';
      allowkount           PLS_INTEGER;
      gz_measure           NUMBER;
      the_segment_geom     SDO_GEOMETRY;
      getoutofjail         PLS_INTEGER := 0;
      intersekt            SDO_GEOMETRY;
      target_full_geom     SDO_GEOMETRY;
      rejectededges        GZ_TYPES.stringarray;
      rejectededgesgeom    GZ_CLIP.geomarray;
      edgewalker           PLS_INTEGER := 0;
      edge_revisit         PLS_INTEGER := 0;
      maxindex             NUMBER;
      currentindex         PLS_INTEGER;
      edgenodes            GZ_CLIP.geomarray;
      deadman              PLS_INTEGER := 0;
      edge_geom            SDO_GEOMETRY;
      shifty_measure       NUMBER;
      shorty_length        NUMBER;
      too_long             PLS_INTEGER := 0;



   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_A_NODE: Set up basics ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      --set up basic names
      --ex Z601CL_Z699IN_EWRK
      newedgetable := GZ_CLIP.GET_EDGE_TABLE(p_project_id,p_jobid,clip_parms.edge_input_table);

      newtopo := p_topo_out;
      -- ie SDZ1AL_GEN_ST_EDGES_HI
      newcliptable := p_project_id || p_jobid || '_' || clip_parms.gen_clip_table; --our task at hand


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_A_NODE: Start working on dangling edge ' || p_edge_id);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Find closest edge for this dangling node
      --consider rewriting this to collect all nodes in a single sql
      --   instead of this kiddie one at a time business

      --actually, Matt, its necessary to do everything that follows in the loop one by one
      --when a new node gets added it potentially destroys the gathered edge_id
      --and position of some other dangle attempt

      --find closest edge and its geom
      --We will connect our dangle to this target

      --Dan Geringer says that with our current patchset at Census
      --Use just the first rows hint on SDO_NN

      --EVIL GOTO lOOP
      --Closest edge is a FAIL
      --Will try with a new one if we are in LOGOS mode
      <<logoturtle>>

      IF touchtimes <= 20
      THEN

         --SOP here

         psql := 'SELECT edge_id, edgegeom, nodegeom FROM ( '
              || 'SELECT '
            --|| '/*+ INDEX(e ' || newtopo || '_ED_SIDX$ NO_INDEX(e ' || newtopo || '_PKEID$) */ '  --NO !
              || '/*+ FIRST_ROWS */ '
              || 'e.edge_id, e.geometry edgegeom, n.geometry nodegeom, sdo_nn_distance(1) dist FROM '
              || newcliptable || ' a, '
              || newtopo || '_RELATION$ r, '
              || newtopo || '_EDGE$ e, '
              || newtopo || '_NODE$ n '
              || 'WHERE '
              || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
              || 'r.tg_id = a.topogeom.tg_id AND '
              || 'e.edge_id = r.topo_id AND '
              || 'n.node_id = :p1 AND ';

         IF touchtimes > 0
         THEN

            --Building a list of edges that cross and are rejected
            psql := psql || 'e.edge_id NOT IN ' || rejected_edges || ' AND ';

         END IF;

         psql := psql
              || 'SDO_NN(e.geometry, n.geometry,:p2,1) = :p3 '
              || 'ORDER BY dist '
              || ') WHERE '
              || 'rownum = 1 ';

          GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,'Get close edge for node id ' || p_node_id,
                                              NULL,NULL,NULL,psql);


         EXECUTE IMMEDIATE psql INTO connectedge,
                                     connectedgegeom,
                                     nodegeom USING p_node_id,   --till now we just had nodeid, get geom while we are at it
                                                    'sdo_batch_size=2',
                                                    'TRUE';

      ELSE

         --just noting this for sanity
         --if touchtimes > 20 we built connectedge and connectedgeom down below in the FAIL manager
         --nodegeom does not actually change


         --actually this can get out of hand if we are lost
         --need to catch near infinite loops
         IF deadman < 1000
         THEN

            deadman := deadman + 1;

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Yo, we just tried to connect to 1000 segments, this probably isnt gonna work out');

         END IF;


      END IF;


      -------------------
      --KILL SRID NOW
      -------------------

      IF touchtimes <= 20
      THEN

         --SOP
         tucked_srid := connectedgegeom.sdo_srid;
         connectedgegeom.sdo_srid := NULL;
         nodegeom.sdo_srid := NULL;

      ELSE
         --If we are logoturtling it, we have a phony connectedge geom in here
         --Upchucked from the desperate measures below
         --keep all tucked SRIDs and etc from normal processing
         NULL;
      END IF;




      --Find the segment along the edge that is closest
      --Follow the sdo_topo_map.add_node concept of "coord_index"
      --"if the edge coordinates are (2,2, 5,2, 8,3) the index of the second vertex (5,2) is 1"
      --1 for us here means we wish to add a node between 5,2 and 8,3

      new_node_geom := GZ_CLIP.GZ_PROJECT_PT(nodegeom, connectedgegeom, p_tolerance);

      IF touchtimes <= 20
      THEN

         --SOP in here
         coord_index := GZ_CLIP.ACCURATE_FIND_COORD_INDEX(new_node_geom,connectedgegeom,p_debug);

      ELSE

         --we are chewing segment by segment in a desperate attemtpt to connect
         --we know theres just one segment, lets not risk it
         coord_index := 0;

      END IF;

      --------
      --tuck these next three away for later klugefest
      --------

      orig_index := coord_index;
      vtx_count := SDO_UTIL.GETNUMVERTICES(connectedgegeom);  --using this for sure

      IF vtx_count = 2
      THEN
         shortedgeflag := 1;
      ELSE
         shortedgeflag := 0;
      END IF;

      -------------
      --End tucking
      ------------



      IF p_debug = 1
      THEN

         --dbms_output.put_line('heres connectedgegeom');
         --dbms_output.put_line(TO_CHAR(MATTOOL.dump_sdo(connectedgegeom)));
         dbms_output.put_line('edge is ' || connectedge);

         dbms_output.put_line('node is ' || p_node_id);
         dbms_output.put_line('coord index is ' || coord_index);

         dbms_output.put_line('node x is ' || nodegeom.sdo_point.X);
         dbms_output.put_line('node y is ' || nodegeom.sdo_point.Y);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 25');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_A_NODE: Test what we have for dangling edge ' || p_edge_id);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --AKA the tilting at windmills code


      --At this point we are either geometry geniuses, or we are about to insert an edge into the topology
      --That is going to break a whole gang of rules
      --Lets at least note possible tests and solutions for various error conditions


      --TEST 0 Forget it for now
      --IS our test_segment internal to the state outline?
      --   We don't have a polygon for the state to use
      --   Could either test if new line crosses another edge, or make a poly somewhere
      --   I'm a little concerned about accurately testing any of this given hair thin tolerances
      --   Return to test1 if this error repeats often


      --TEST1 Does our new proposed connection, dangle to outline, cross other edges?
      --If so, the closest edge is not the bestest edge
      --Theres a similar check in add_new_edges but at that point we're hosed
      --Weve added this proposed node already and its too late

      --Create a segment connecting the dangle node and the new node
      test_segment := SDO_GEOMETRY(2002,
                                   new_node_geom.SDO_SRID,   --Should be NULL
                                   NULL,
                                   SDO_ELEM_INFO_ARRAY(1, 2, 1),
                                   SDO_ORDINATE_ARRAY(new_node_geom.sdo_point.X,
                                                      new_node_geom.sdo_point.Y,
                                                      nodegeom.sdo_point.X,
                                                      nodegeom.sdo_point.Y
                                                     )
                                   );


      --Length checks
      --First time through the 20 edges only

      IF touchtimes = 0
      THEN

         --record the length of the 1st, preferred, and shortest test segment
         --this length is in decimal degrees
         --tolerance is meh
         shorty_length := SDO_GEOM.SDO_LENGTH(test_segment,p_tolerance);

         IF p_debug = 1
         THEN
            dbms_output.put_line('first segment length: ' || shorty_length);
         END IF;

         --just in case
         too_long := 0;

      ELSIF touchtimes <= 20
      THEN

         --we reserve the right to reject proposed long distance connections
         --if it seems like the guy we are working on is really right next to (relatively) the outline
         --prevents wild slivers and going around on the "wrong" side of the L/R relationships
         --actual ratio is hard coded here and I dunno

         IF  SDO_GEOM.SDO_LENGTH(test_segment,p_tolerance)/shorty_length > 75
         THEN

            --too long is true
            too_long := 1;

         ELSE

            too_long := 0;

         END IF;

      ELSE

         --just in case, for edge by edge processing, never too long, desperate
         too_long := 0;

      END IF;


      IF p_debug = 1
      THEN
         dbms_output.put_line('test segment: ');
         dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(test_segment)));
      END IF;


      IF p_v_flag IS NULL
      THEN

         --standard, only allowed to touch dangle and clip
         allowkount := 2;

      ELSE

         --special V dangle passed in the number of touches allowed
         --The V dangle typically allows for 3 touches, but sometimes pitchforks make 4... etc

         --Dangle1 + Dangle2
         --   \    /
         --    \  /
         --     \/
         --      |
         --      | <-- New proposed edge
         --      |
         --  --------- + State = 3 expected touches

         allowkount :=  p_v_flag;

      END IF;



      --must make test_segment back into real srid for operator. This ok?
      -- I guess I could filter everybody into local variables and take them on one by one
      test_segment.sdo_srid := tucked_srid;


      psql2 := 'SELECT e.edge_id '
            || 'FROM ' || newtopo || '_EDGE$ e '
            || 'WHERE '
            || 'SDO_RELATE(e.geometry,:p1,:p2) = :p3 ';

      EXECUTE IMMEDIATE psql2 BULK COLLECT INTO touch_edges USING test_segment,
                                                                  'mask=ANYINTERACT',
                                                                  'TRUE';

      --Because of cartesian vs geodetic issues here, the test_segment may connect to the connect edge
      --just fine in cartesian, but in geodetic appear to fall short on long lines of latitude (Z8, Z9 usually).
      --In and of itself this is not a problem
      --But must be careful that if the sdo_relate above sees just 2 interactions, those two are in fact the start and end
      --and not start, something else, and NO end

      --      -----X----------   Nuthin touch (long latidudey line)
      --           ^
      --           |
      --         --|------       2 touch BAD
      --           |
      --           |
      --    -------X---------    1 touch

      --Extra careful kounting as a result.

      touchkount := 0;

      FOR jjj IN 1 .. touch_edges.COUNT
      LOOP

         IF touch_edges(jjj) <> connectedge
         AND touch_edges(jjj) <> p_edge_id
         THEN

            touchkount := touchkount + 1;

         END IF;

      END LOOP;

      --Add the two that must touch
      touchkount := touchkount + 2;


      IF p_debug = 1
      THEN

         dbms_output.put_line('touchkount is ' || touchkount || ' allowkount is ' || allowkount);

         FOR jjj IN 1 .. touch_edges.COUNT
         LOOP
            dbms_output.put_line('touched edge ' || jjj || ' ' || touch_edges(jjj));
         END LOOP;

      END IF;

      --Back to NULL srid
      test_segment.sdo_srid := NULL;


      --TEST2: Does our proposed edge badly intersect the edge we are trying to extend
      --I guess this one should be back to NULL Srid (?)

      edge_geom := GZ_CLIP.GET_EDGE_GEOMETRY(newtopo, p_edge_id);
      edge_geom.sdo_srid := NULL;

      intersekt := SDO_GEOM.SDO_INTERSECTION(edge_geom, test_segment, p_tolerance);

      IF p_debug = 1
      THEN
         dbms_output.put_line(chr(10) || ' intersection of test segment with edge to extend is ');
         dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(intersekt)));
      END IF;

      IF touchtimes > 20
      AND intersekt.sdo_gtype = 2001
      THEN

         --Very special TEST2A: When processing segment by segment (connectedgegeom is just 1 phony segment)
         --in round 2 we aren't necessarily connecting to the closest point on the target edge
         --which introduces the possibility of intersecting the target edge at a
         --segment other than the one we are processing
         target_full_geom := GZ_CLIP.GET_EDGE_GEOMETRY(p_topo_out,
                                                       connectedge);

         target_full_geom.sdo_srid := NULL;

         --re-use intersekt.  Both must be good
         intersekt := SDO_GEOM.SDO_INTERSECTION(target_full_geom,
                                                test_segment,
                                                p_tolerance);

         IF p_debug = 1
         THEN
            dbms_output.put_line('Test 2A connect edge geom is ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(target_full_geom)));
            dbms_output.put_line(chr(10) || ' intersection of test segment with target full edge ' || connectedge || ' is ');
            dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(intersekt)));
         END IF;

      END IF;

      IF touchkount > allowkount
      OR intersekt.sdo_gtype <> 2001
      OR too_long = 1
      THEN

         IF p_debug = 1
         THEN
            dbms_output.put_line('touchkount is ' || touchkount ||', compare to allowkount ' || allowkount);
            dbms_output.put_line('intersekt gtype is ' || intersekt.sdo_gtype);
            dbms_output.put_line('too long is ' || too_long);
         END IF;

         --TEST1 and TEST2 management
         --BAD NEWS
         --we touch suspiciously too many edges with our new edge
         --or we self_intersect with the guy we are trying to extend

         IF touchkount = (allowkount + 2)
         AND intersekt.sdo_gtype = 2001
         AND too_long = 0
         THEN

            --we have a chance to get out of jail

            --This could be our best segment available
            --we just happen to be at its start or end point where it is already connected to an interior edge
            -- +2 because theres both the interior connector there, and also the neighboring outline edge
            --If thats the case, lets try to build a test segment to somewhere in the middle

            --get the little segment bit
            the_segment_geom := GZ_CLIP.GET_SEGMENT_GEOM_FROM_INDEX(connectedgegeom,coord_index);


            gz_measure := GZ_CLIP.GZ_FIND_MEASURE(new_node_geom,the_segment_geom);

            IF gz_measure = 0
            OR gz_measure = 1000
            THEN

                --our theory looks good, lets roll with it

                --note that this is the midpoint of the segment at the terminus of the edge
                --not the midpoint of the entire edge
                --Also note that if this segment is long we could be making a giant mess
                --   here without knowing it
                new_node_geom := GZ_LOCATE_PT(the_segment_geom,500);

                --Create a segment connecting the dangle node and the new node
                test_segment := SDO_GEOMETRY(2002,
                                             new_node_geom.SDO_SRID,  --should be NULL
                                             NULL,
                                             SDO_ELEM_INFO_ARRAY(1, 2, 1),
                                             SDO_ORDINATE_ARRAY(new_node_geom.sdo_point.X,
                                                                new_node_geom.sdo_point.Y,
                                                                nodegeom.sdo_point.X,
                                                                nodegeom.sdo_point.Y
                                                                )
                                              );

               --Back to real srid
               test_segment.sdo_srid := tucked_srid;

               IF p_debug = 1
               THEN
                  dbms_output.put_line('test segment is ' || gz_geom_utils.dump_sdo(test_segment));
               END IF;

               --psql saved from above
               EXECUTE IMMEDIATE psql2 BULK COLLECT INTO touch_edges USING test_segment,
                                                                           'mask=ANYINTERACT',
                                                                           'TRUE';
               --Back to NULL srid
               test_segment.sdo_srid := NULL;

               --Must do extra careful touch kounting here too, since connected geodetic edge may not relate
               touchkount := 0;

               FOR jjj IN 1 .. touch_edges.COUNT
               LOOP

                  IF touch_edges(jjj) <> connectedge
                  AND touch_edges(jjj) <> p_edge_id
                  THEN

                     touchkount := touchkount + 1;

                  END IF;

               END LOOP;

               --Add the two that must touch
               touchkount := touchkount + 2;

               IF touchkount <= allowkount
               THEN

                  GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                      'Going with the midpoint of coord_index ' || coord_index,
                                                      NULL,NULL,NULL,NULL,NULL,NULL,test_segment);
                  --got lucky
                  getoutofjail := 1;

               END IF;

            END IF;

         ELSIF touchkount = (allowkount + 1)
         AND intersekt.sdo_gtype = 2001
         AND too_long = 0
         THEN

            --This could just be the start or end of the edge
            --allowkount + 1 because the edge is connected to one other exterior edge
            --no interior edge concerns

            edgenodes := GZ_CLIP.GET_EDGE_NODES(p_schema,
                                                p_project_id,
                                                p_jobid,
                                                p_mask,
                                                newtopo,
                                                connectedge,
                                                'Y');  --return NULL SRIDs


            --used to have .00005 tolerance here with 8265 calls
            IF SDO_GEOM.RELATE(edgenodes(1),'mask=EQUAL',new_node_geom, p_tolerance) != 'FALSE'
            OR SDO_GEOM.RELATE(edgenodes(2),'mask=EQUAL',new_node_geom, p_tolerance) != 'FALSE'
            THEN

                  GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                      'Think we are at the end or beginning of the edge ',
                                                      NULL,NULL,NULL,NULL,NULL,NULL,new_node_geom);

                  --got lucky
                  --TEST3 and 4 will actually manage this
                  getoutofjail := 1;

            ELSE

               --another occasional possibility is that we are at some super-close double-dangle
               --the nearby dangle is whats causing the +1 touchkount
               --sometimes just going a smidge one way or the other from the calc'd project_pt will fix it
               gz_measure := GZ_CLIP.GZ_FIND_MEASURE(new_node_geom,connectedgegeom);

               --dont go past 1000
               IF (gz_measure + 10) > 1000
               THEN
                  shifty_measure := (gz_measure + 1000)/2;  --split the dif
               ELSE
                  shifty_measure := gz_measure + 10; --move 1%
               END IF;

               new_node_geom := GZ_LOCATE_PT(connectedgegeom,shifty_measure);

               --Create a segment connecting the dangle node and the new node
               test_segment := SDO_GEOMETRY(2002,
                                             new_node_geom.SDO_SRID,  --should be NULL
                                             NULL,
                                             SDO_ELEM_INFO_ARRAY(1, 2, 1),
                                             SDO_ORDINATE_ARRAY(new_node_geom.sdo_point.X,
                                                                new_node_geom.sdo_point.Y,
                                                                nodegeom.sdo_point.X,
                                                                nodegeom.sdo_point.Y
                                                                )
                                              );
               IF p_debug = 1
               THEN
                  dbms_output.put_line('test segment on shifty_measure +10: ');
                  dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(test_segment)));
               END IF;

               --psql saved from above
               --Back to real srid
               test_segment.sdo_srid := tucked_srid;
               EXECUTE IMMEDIATE psql2 BULK COLLECT INTO touch_edges USING test_segment,
                                                                           'mask=ANYINTERACT',
                                                                           'TRUE';
               touchkount := touch_edges.COUNT;

               --Having rare but annoying problems here where this count is returning one too few, and a failed node is passing
               --Bad workaround: Just for the more common standard (non V) node addition, check the actual edge Ids

               test_segment.sdo_srid := NULL;


               IF touchkount <= allowkount  --main test
               AND (  (p_v_flag IS NULL                                              --standard 2 connect
                       AND GZ_CLIP.QUERY_DELIMITED_LIST(touch_edges, p_edge_id) != 0 --dangle must be in the list of touches
                       AND GZ_CLIP.QUERY_DELIMITED_LIST(touch_edges, connectedge) != 0 ) --connector must be in the list too
                    OR ( p_v_flag IS NOT NULL )  -- Or 3+ connect, no rigorous check
                   )
               THEN

                  GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                      'Going with +10 project point of coord_index ' || coord_index,
                                                      NULL,NULL,NULL,NULL,NULL,NULL,test_segment);
                  --got lucky
                  getoutofjail := 1;

               ELSE

                  --try the other direction
                  --dont go past 0
                  --remember, have not changed the original gz_measure in the last ~15 lines
                  IF (gz_measure - 10) < 0
                  THEN
                     shifty_measure := (gz_measure)/2;  --split the dif
                  ELSE
                     shifty_measure := gz_measure - 10; --move 1%
                  END IF;

                  new_node_geom := GZ_LOCATE_PT(connectedgegeom,(shifty_measure));


                  --Create a segment connecting the dangle node and the new node
                  test_segment := SDO_GEOMETRY(2002,
                                               new_node_geom.SDO_SRID,
                                               NULL,
                                               SDO_ELEM_INFO_ARRAY(1, 2, 1),
                                               SDO_ORDINATE_ARRAY(new_node_geom.sdo_point.X,
                                                                  new_node_geom.sdo_point.Y,
                                                                  nodegeom.sdo_point.X,
                                                                  nodegeom.sdo_point.Y
                                                                  )
                                                );

                  IF p_debug = 1
                  THEN
                     dbms_output.put_line('test segment on shifty_measure -10: ');
                     dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(test_segment)));
                  END IF;

                  --psql saved from above
                  --Back to real srid
                  test_segment.sdo_srid := tucked_srid;
                  EXECUTE IMMEDIATE psql2 BULK COLLECT INTO touch_edges USING test_segment,
                                                                              'mask=ANYINTERACT',
                                                                              'TRUE';
                  touchkount := touch_edges.COUNT;

                  test_segment.sdo_srid := NULL;




                  IF touchkount <= allowkount
                  AND (  (p_v_flag IS NULL                                              --standard 2 connect
                          AND GZ_CLIP.QUERY_DELIMITED_LIST(touch_edges, p_edge_id) != 0 --dangle must be in the list of touches
                          AND GZ_CLIP.QUERY_DELIMITED_LIST(touch_edges, connectedge) != 0 ) --connector must be in the list too
                    OR ( p_v_flag IS NOT NULL )  -- Or 3+ connect, no rigorous check
                   )
                  THEN


                     GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                      'Going with -1 project point of coord_index ' || coord_index,
                                                      NULL,NULL,NULL,NULL,NULL,NULL,test_segment);
                     --got lucky
                     getoutofjail := 1;

                  END IF;

               END IF;

            END IF;


         END IF;


         IF getoutofjail = 0
         THEN


            touchtimes := touchtimes + 1;

            --store this away for emergencies
            rejectededges(touchtimes) := connectedge;
            rejectededgesgeom(touchtimes) := connectedgegeom;


            --Standard management
            --Try the next closest edge to our dangle
            IF touchtimes > 1
            AND touchtimes <= 20
            THEN

               rejected_edges_root := rejected_edges_root || ',' || connectedge;

            ELSIF touchtimes <= 20
            THEN

               --first time
               rejected_edges_root := '(' || connectedge;

            END IF;

            rejected_edges := rejected_edges_root || ')';


            --desperate, rare, measures
            IF touchtimes > 20
            THEN

               IF edge_revisit = 0
               THEN

                  --1st time in here: we mean business now
                  --Go back to the first edge we tried and get its first segment
                  --we will walk the edge segment by segment, then the next...
                  edge_revisit := edge_revisit + 1;
                  connectedge := rejectededges(edge_revisit);

                  --get the top index we may try for this guy
                  maxindex := SDO_UTIL.GETNUMVERTICES( rejectededgesgeom(edge_revisit) ) - 2;
                  currentindex := 0;

                  IF maxindex != 0
                  THEN

                     --Get first segment
                     connectedgegeom := GZ_CLIP.GET_SEGMENT_GEOM_FROM_INDEX(rejectededgesgeom(edge_revisit),currentindex);

                  ELSE

                     --if this edge is only two nodes we are just going to be hitting against the
                     --entire edge, or, in other words, get the same result as last time
                     --lets be morons and chop 10% off of the ends
                     --This is presumably where the most messy geometries are hanging out
                     connectedgegeom := GZ_CLIP.GZ_SEGMENT_RANGE(rejectededgesgeom(edge_revisit),100,900);


                  END IF;

               ELSE

                  --we are walking an edge, or about to move to the next one

                  currentindex := currentindex + 1;

                  IF currentindex > maxindex
                  THEN

                     --on to next edge
                     edge_revisit := edge_revisit + 1;
                     connectedge := rejectededges(edge_revisit);
                     --get the top index we may try for this guy
                     maxindex := SDO_UTIL.GETNUMVERTICES( rejectededgesgeom(edge_revisit) ) - 2;
                     currentindex := 0;


                     IF maxindex != 0
                     THEN

                        --Get first segment
                        connectedgegeom := GZ_CLIP.GET_SEGMENT_GEOM_FROM_INDEX(rejectededgesgeom(edge_revisit),currentindex);

                     ELSE

                        --if this edge is only two nodes we are just going to be hitting against the
                        --entire edge, or, in other words, get the same result as last time
                        --lets be morons and try the midpoint this time
                        connectedgegeom := GZ_CLIP.GZ_SEGMENT_RANGE(rejectededgesgeom(edge_revisit),100,900);


                     END IF;

                     --Get first segment
                     connectedgegeom := GZ_CLIP.GET_SEGMENT_GEOM_FROM_INDEX(rejectededgesgeom(edge_revisit),currentindex);

                  ELSE

                     --on to next segment of current edge
                     connectedgegeom := GZ_CLIP.GET_SEGMENT_GEOM_FROM_INDEX(rejectededgesgeom(edge_revisit),currentindex);

                  END IF;


               END IF;

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                'We cross an existing edge using 20 edges. Try edge ' || connectedge || ' segment ' || currentindex,
                                                 NULL,NULL,NULL,NULL,NULL,NULL,connectedgegeom);


            END IF;


            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                'We cross an existing edge using ' || connectedge || ', going back to try another one ',
                                                 NULL,NULL,NULL,psql,NULL,NULL,test_segment);



            --Are there any variables we need to reset?
            --xnear := NULL;
            --ynear := NULL;
            GOTO logoturtle;

         END IF;

      END IF;


      IF touchtimes > 20
      THEN

         --IF we got here we just escaped wacky wiggly dangle purgatory
         --weve been using a false coord index of 0 on a specific segment of the current edge
         --swap out its real coord index and geom before continuing
         coord_index := currentindex;
         connectedgegeom := rejectededgesgeom(edge_revisit);

      END IF;

      --TEST3
      --Is our new calc'd node perfectly coincident (within a fuzzylike tolerance) of one of the vertices on the edge
      --Then lets make that vertex our new node
      --Allowing ourselves to produce a new node that is within tolerance of an existing node
      --   is asking for zig-zags around the add point


      --get x1,y1,x2,y2 for this index value
      the_segment := GZ_CLIP.GET_SEGMENT_FROM_INDEX(connectedgegeom,coord_index);

      --dbms_output.put_line('the segment is ' || to_char(mattool.dump_sdo(the_segment)));

      --SOP now
      --tucked_srid := new_node_geom.sdo_srid;
      --new_node_geom.sdo_srid := NULL;


      test_node := SDO_GEOMETRY(2001,
                                --new_node_geom.sdo_srid,
                                NULL,
                                SDO_POINT_TYPE(the_segment(1), the_segment(2), NULL),
                                NULL,
                                NULL
                                );

      IF p_debug = 1
      THEN
         dbms_output.put_line('RELATE 1');
         dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(new_node_geom)));
         dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(test_node)));
         dbms_output.put_line('coord index is ' || coord_index);
         dbms_output.put_line('relate is ' || SDO_GEOM.RELATE(new_node_geom,'mask=EQUAL',test_node, p_tolerance));
      END IF;

      --Note voodoo tolerance attempting to mimic
      --The distance at which add_node sees two points as being on top of each other
      --This is the best guess I have so far
      --used to be .00005 now going with more like .0000005 (degrees!)

      IF SDO_GEOM.RELATE(new_node_geom,'mask=EQUAL',test_node, p_tolerance) != 'FALSE'
      THEN

         IF coord_index != 0
         THEN

            --usually in here
            new_node_geom := test_node;
            is_new_shape_pt := 'FALSE';

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                'Going with an existing shape point (beginning of segment) on ' || connectedge);


         ELSE

            --Note if we just picked a node (START in this case)

            --However we cant allow this if the node we are attaching to is attached to an interior line already
            IF NODE_EXISTS(p_schema,
                           p_project_id,
                           p_jobid,
                           p_mask,
                           newtopo,
                           newedgetable,
                           connectedge,
                           'START') = 1
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                   'We are really close to beginning of segment on ' || connectedge
                                                   || ' but we cant connect to that node. Gonna risk it');

            ELSE

               --its ok
               --probably a node on state outline from original built edges

               existing_node_flag := 1;

               new_node_geom := test_node;
               is_new_shape_pt := 'FALSE';

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                'Going with an existing shape point (node at beginning of segment) on ' || connectedge);


            END IF;


         END IF;


      END IF;


      --test the end of the edge we want to connect to
      test_node := SDO_GEOMETRY(2001,
                                --new_node_geom.sdo_srid,
                                NULL,
                                SDO_POINT_TYPE(the_segment(3), the_segment(4), NULL),
                                NULL,
                                NULL
                                );

      IF p_debug = 1
      THEN
         dbms_output.put_line('RELATE 2');
         dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(new_node_geom)));
         dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(test_node)));
      END IF;

      --tolerance switched to degrees
      IF SDO_GEOM.RELATE(new_node_geom,'mask=EQUAL',test_node, p_tolerance)  != 'FALSE'
      THEN

         coord_index := coord_index + 1; --dont forget
                                         --we are actually incrementing onto the next index

         vtx_count := SDO_UTIL.GETNUMVERTICES(connectedgegeom);

         IF (vtx_count - 1 ) != coord_index
         THEN

            --usually here
            new_node_geom := test_node;
            is_new_shape_pt := 'FALSE';

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                'Going with an existing shape point, next coord index, on ' || connectedge);




         ELSE   --ie (vtx_count - 1 ) = coord_index

            --We have actually incremented ourself from the final coord index onto the next edge

            --However we cant allow this if the node we are attaching to is attached to an interior line already
            IF NODE_EXISTS(p_schema,
                           p_project_id,
                           p_jobid,
                           p_mask,
                           newtopo,
                           newedgetable,
                           connectedge,
                           'END') = 1
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                   'We are really close to the end of of ' || connectedge
                                                   || ' but we cant connect to that node. Gonna risk it');


            ELSE

               --its ok
               --probably a node on state outline from original built edges

               existing_node_flag := 1;
               new_node_geom := test_node;
               is_new_shape_pt := 'FALSE';

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                   'Going with an existing shape point, NEXT EDGE AND NODE after ' || connectedge);

            END IF;


         END IF;

      END IF;

      --restore SRID
      --nope
      --new_node_geom.sdo_srid := tucked_srid;


      --TESTS 4 and 5
      --Verify our coord index

      IF p_debug = 1
      THEN
         dbms_output.put_line('is new shape point is ' || is_new_shape_pt);
         dbms_output.put_line('coord index is ' || coord_index);
      END IF;


      IF is_new_shape_pt = 'TRUE'
      THEN

         --only check this if we didnt already find out we were on the endpoints of the segment
         --pass in just the little segment

         the_segment_geom := GZ_CLIP.GET_SEGMENT_GEOM_FROM_INDEX(connectedgegeom,coord_index);


         IF p_debug = 1
         THEN
            dbms_output.put_line('is new shape point coord index ' || coord_index);
            dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(new_node_geom)));
            dbms_output.put_line(TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(the_segment_geom)));
         END IF;

         gz_measure := GZ_CLIP.GZ_FIND_MEASURE(new_node_geom,the_segment_geom);

         IF p_debug = 1
         THEN
            dbms_output.put_line('gz measure is ' || gz_measure);
         END IF;


         IF gz_measure = 1000
         OR gz_measure = 0
         THEN

            --this means we have picked an index that is past the point
            --or before it, on the original coord_index segment we started on
            --try on either side

            IF p_debug = 1
            THEN
               dbms_output.put_line('gz measure 1 is ' || gz_measure);
            END IF;

            coord_index := coord_index - 1;

            --if we were at 0 (now -1), dont back up
            IF coord_index >= 0
            THEN

               --Give it another check
               the_segment_geom := GZ_CLIP.GET_SEGMENT_GEOM_FROM_INDEX(connectedgegeom,coord_index);
               gz_measure := GZ_CLIP.GZ_FIND_MEASURE(new_node_geom,the_segment_geom);

            END IF;


            IF gz_measure = 1000
            OR gz_measure = 0
            THEN

               --try the other way
               coord_index := coord_index + 2;

               the_segment_geom := GZ_CLIP.GET_SEGMENT_GEOM_FROM_INDEX(connectedgegeom,coord_index);
               gz_measure := GZ_CLIP.GZ_FIND_MEASURE(new_node_geom,the_segment_geom);

               IF gz_measure = 1000
               OR gz_measure = 0
               THEN

                  --Getting deep in the weeds here, but sometimes the spot we picked skipped right past
                  --a hairball of shape points on the state outline, ie more than +1 or -1 the original segment
                  --cycle  through them all and see if we can find it

                  FOR jjj IN 1 .. (SDO_UTIL.GETNUMVERTICES(connectedgegeom) - 1)
                  LOOP

                     coord_index := (jjj - 1);

                     the_segment_geom := GZ_CLIP.GET_SEGMENT_GEOM_FROM_INDEX(connectedgegeom,coord_index);
                     gz_measure := GZ_CLIP.GZ_FIND_MEASURE(new_node_geom,the_segment_geom);

                     IF gz_measure < 1000
                     AND gz_measure > 0
                     THEN

                        --whew
                        GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                          'Had to go far afield, but going with coord index ' || coord_index);
                        EXIT;

                     END IF;

                     IF jjj = (SDO_UTIL.GETNUMVERTICES(connectedgegeom) - 1)
                     THEN

                        --hosed

                        GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                           'Cant get a grasp on the coord index on edge ' || connectedge,
                                                           NULL,NULL,NULL,NULL,NULL,NULL,new_node_geom);

                        RAISE_APPLICATION_ERROR(-20001,'Cant get a grasp on the coord index, see log');


                     END IF;

                  END LOOP;

               END IF;

            END IF;

         END IF;



         --TEST5
         --Verify our coord index once more
         --LRS just says we are within the range of the segment
         --We could be sitting in space directly above it, test we are actually ON it

         --both SRIDs should be null
         --how loose can we be on the tolerance? Used to have .05 for 8265

         IF SDO_GEOM.RELATE(new_node_geom,'mask=INSIDE',the_segment_geom,p_tolerance) = 'FALSE'
         THEN

            tuck_coord_index := coord_index;

            --We are off of the segment
            --most likely we are just on a neighboring segment, like an L shape

            --Force this guy through
            --High degree of confidence that the point is fine, just need the index

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                'Cant get a grasp on the coord index on edge ' || connectedge || '.'
                                             || ' We will loop through all the segments');

            FOR i IN 1 .. (vtx_count-1)
            LOOP

               --loop through all of the segments

               the_segment_geom := GZ_CLIP.GET_SEGMENT_GEOM_FROM_INDEX(connectedgegeom,(i-1));

               IF p_debug = 1
               THEN
                  dbms_output.put_line('relate is ' || SDO_GEOM.RELATE(new_node_geom,'mask=determine',the_segment_geom,.05));
                  dbms_output.put_line('relate is ' || SDO_GEOM.RELATE(new_node_geom,'mask=INSIDE',the_segment_geom,.05));
               END IF;

               --used to have .05 tolerance with 8265
               IF SDO_GEOM.RELATE(new_node_geom,'mask=INSIDE',the_segment_geom,p_tolerance) != 'FALSE'
               THEN

                  coord_index := (i-1);

                  GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newcliptable,
                                                    'For edge ' || connectedge || ' we used sdo_relate to chose coord index ' || coord_index);

                  EXIT;

               END IF;

               IF i = (vtx_count-1)
               THEN

                  --FAIL
                  --Just in case the original coord index was ok
                  --we didnt find anything on the edge, so just go back to initial best index
                  coord_index := tuck_coord_index;
                  --raise_application_error(-20001,'caught you');

               END IF;


            END LOOP;


         END IF;

      END IF;

      --RAISE_APPLICATION_ERROR(-20001,'peppahs');
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 29');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_A_NODE: Get backup Sidey point');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF existing_node_flag != 1
      THEN

         --dont go in here for existing nodes
         --we dont need it
         --and we may have picked a coord index that technically extends us past the end of the edge

         the_segment := GZ_CLIP.GET_SEGMENT_FROM_INDEX(connectedgegeom,coord_index);

         dist := GZ_CLIP.perpendicular(new_node_geom.sdo_point.X,
                                           new_node_geom.sdo_point.Y,
                                           the_segment(1),
                                           the_segment(2),
                                           the_segment(3),
                                           the_segment(4),
                                           xnear,  --INOUT
                                           ynear);   --INOUT


         sideypoint := SDO_GEOMETRY(2001,
                                    new_node_geom.sdo_srid,  --NULL
                                    SDO_POINT_TYPE(xnear,ynear,NULL),
                                    NULL,
                                    NULL);

         sideypoint.sdo_srid := tucked_srid;

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_A_NODE: Adding node');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------
      --Add the node, using a procedure that can handle errors and
      --unforseen circumstances

      ----
      --Consider creating a topo map for this next section (whole thing actually genius)
      ----

      --Add nodes and collect node ids

      --switch back to real srids
      new_node_geom.sdo_srid := tucked_srid;


      IF is_new_shape_pt = 'TRUE'                                --Just a totally new spot for this node
      OR (is_new_shape_pt = 'FALSE' AND existing_node_flag = 0)  --OR an existing vertex on an edge
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newtopo || '_XX',
                                             'Adding a node to edge ' || connectedge);

         new_node_id := GZ_CLIP.ADD_NEW_NODES(p_project_id,
                                              p_jobid,
                                              newtopo,
                                              connectedge,
                                              coord_index,
                                              new_node_geom,
                                              sideypoint,
                                              is_new_shape_pt,
                                              p_edge_id,       --rarely used
                                              p_node_id);      --rarely used

      ELSIF is_new_shape_pt = 'FALSE'
      AND existing_node_flag = 1
      THEN


         --We are connecting to either its start or end node

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES ' || p_mask,newtopo || '_XX',
                                             'Grabbing an existing node_id for edge ' || connectedge ||
                                             ' and backing up node ' || p_node_id);

         new_node_id := GZ_CLIP.GET_EXISTING_NODE(p_project_id,
                                                  p_jobid,
                                                  newtopo,
                                                  connectedge,
                                                  coord_index,
                                                  p_edge_id,
                                                  p_node_id);

         --Sometimes when we pick an existing node its actually just the opposite side of our dangle
         --This happens when the dangle is very short and "dangles inside" the state

         --In on left, out on right
         --     |
         --     |
         --  X--0 <-- Existing node, we are lost trying to connect the X to the 0
         --     |     When its actually already connected
         --     |

         psql := 'SELECT count(*) FROM '
              || newtopo || '_edge$ e '
              || 'WHERE '
              || 'e.edge_id = :p1 '
              || 'AND ( e.start_node_id = :p2 OR e.end_node_id = :p3 ) ';

         EXECUTE IMMEDIATE psql INTO shortykount USING p_edge_id,
                                                       new_node_id,
                                                       new_node_id;

         IF shortykount != 0
         THEN

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_A_NODE ' || p_mask,newtopo || '_XX',
                                             'Our best connection location is just he opposite end of ' || p_edge_id ||
                                             ' returning a fake negative node ');

            new_node_id := -1;

         END IF;

      END IF;

      RETURN new_node_id;




   END ADD_A_NODE;




   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE -------------------------------------------------------------------------------

   FUNCTION RESHAPE_EDGE (
      p_schema          IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_mask            IN VARCHAR2,
      p_newtopo         IN VARCHAR2,
      p_edge            IN NUMBER,
      p_new_node        IN NUMBER, --connect to this new node
      p_old_node        IN NUMBER  --The old tip of the V
   ) RETURN VARCHAR2
   AS

   --Matt!  7/14/10
   --We have an edge connected to a dangling V
   --Wish to reshape it so it bypasses the tip of the V and goes to the new node on the clip outline

   --10/28/11 Added check and handler for when the reshape edge is simple 0------0
   --         and the non-reshape edge is already on the state outline
   --         If the result of the reshape will result in intersections with state outline
   --         add a new midpoint vertex to the edge for a safer reshape

      psql              VARCHAR2(4000);
      retval            VARCHAR2(4000) := '0';
      edge_geom         SDO_GEOMETRY;
      start_node_id     NUMBER;
      end_node_id       NUMBER;
      silly_num_array   SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY();
      node_geom         SDO_GEOMETRY;
      kount             PLS_INTEGER;


   BEGIN

      --Get edge info for the guy we are working on
      psql := 'SELECT e.start_node_id, e.end_node_id, e.geometry '
           || 'FROM '
           || p_newtopo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO start_node_id,
                                  end_node_id,
                                  edge_geom USING p_edge;

      --Get geom of new node we are using for the reshaped edge
      node_geom := GZ_CLIP.GET_NODE_GEOMETRY(p_newtopo,
                                             p_new_node);

      --check if this is a scary short edge where the other side is connected
      --to the outline already

      IF edge_geom.sdo_ordinates.COUNT = 4
      THEN

         --scary.

         psql := 'SELECT COUNT(*) '
              || 'FROM ' || p_newtopo || '_edge$ e '
              || 'WHERE (e.start_node_id = :p1 OR e.end_node_id = :p2) AND '
              || '(e.left_face_id = :p3 OR e.right_face_id = :p4) ';

         IF start_node_id = p_old_node
         THEN

            --end node stays. Is it on the universal face?
            --divides at least two edges bounded by -1
            EXECUTE IMMEDIATE psql INTO kount USING end_node_id, end_node_id,
                                                    -1, -1;

         ELSE

            --start node stays
            EXECUTE IMMEDIATE psql INTO kount USING start_node_id, start_node_id,
                                                    -1, -1;

         END IF;

         IF kount = 2
         THEN

            --bad news.  Reshaping this edge is likely to lay it flat onto the state outline
            --Give it an extra vertex right in the middle so it can V
            --Idea from Sidey - would be safer if this wasnt a midpoint, but was smart
            --test if the other edge is going to intersect and drop it to 40%, 30%, etc as necessary
            edge_geom := GZ_CLIP.GZ_ADD_MIDPOINT(edge_geom);


         END IF;


      END IF;

      silly_num_array.EXTEND(edge_geom.sdo_ordinates.count);

      IF start_node_id = p_old_node
      THEN

         --Start at dead node, replace with new node

         FOR i in 1 .. edge_geom.sdo_ordinates.COUNT
         LOOP

            IF i = 1
            THEN

               silly_num_array(i) := node_geom.sdo_point.X;

            ELSIF i = 2
            THEN

               silly_num_array(i) := node_geom.sdo_point.Y;

            ELSE

               silly_num_array(i) := edge_geom.sdo_ordinates(i);

            END IF;

         END LOOP;

      ELSIF end_node_id = p_old_node
      THEN

         --start of edge is same, skip over dead node to new node at end

         FOR i in 1 .. edge_geom.sdo_ordinates.COUNT
         LOOP

            IF i = (edge_geom.sdo_ordinates.COUNT - 1)
            THEN

               silly_num_array(i) := node_geom.sdo_point.X;

            ELSIF i = (edge_geom.sdo_ordinates.COUNT)
            THEN

               silly_num_array(i) := node_geom.sdo_point.Y;

            ELSE

               silly_num_array(i) := edge_geom.sdo_ordinates(i);

            END IF;

         END LOOP;


      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Nodes no match');

      END IF;



      BEGIN

         SDO_TOPO_MAP.MOVE_EDGE(p_newtopo,
                                p_edge,
                                p_old_node,
                                p_new_node,
                                silly_num_array);

      EXCEPTION
         WHEN OTHERS
         THEN

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'RESHAPE_EDGE ' || p_mask,p_newtopo || '_edge$',
                                               'Failed to move edge ' || p_edge || ' from old node ' || p_old_node || ' to '
                                               || 'new node ' || p_new_node );


            RAISE;

      END;



      RETURN retval;

   END RESHAPE_EDGE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC FOR DEBUG ----------------------------------------------------------------------

   FUNCTION REMOVE_NODE_AND_RESHAPE (
      p_schema          IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_mask            IN VARCHAR2,
      p_newtopo         IN VARCHAR2,
      p_old_edge        IN NUMBER,
      p_new_edge        IN NUMBER,
      p_old_node        IN NUMBER,  --connect to this new node
      p_new_node        IN NUMBER,  --The old tip of the V
      p_tip             IN VARCHAR2 --either START or END
   ) RETURN VARCHAR2
   AS

      --Matt!  7/15/10
      --We have a feature edge that connects to the clip outline via a now obsolete node
      --p_old_node used to be the dangling tip of the V
      --Now we want to remove the dangle node, and reshape the resulting edge so it bypasses that spot

      retval            VARCHAR2(4000) := '0';
      old_edge_geom     SDO_GEOMETRY;
      new_edge_geom     SDO_GEOMETRY;
      reshaped_geom     SDO_GEOMETRY;
      ord_kount         PLS_INTEGER := 0;
      remaining_edge    NUMBER;
      psql              VARCHAR2(4000);
      ordinates         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();


   BEGIN


      --Must get edge geoms before we monkey with anything
      old_edge_geom := GZ_CLIP.GET_EDGE_GEOMETRY(p_newtopo,
                                                  p_old_edge);


      new_edge_geom := GZ_CLIP.GET_EDGE_GEOMETRY(p_newtopo,
                                                  p_new_edge);


      --p_tip refers to the dangle node
      --START means we go from the clip toward the interior
      --END means we are going from the interior toward the clip
      --Both edges run in the same direction, we enforced this when we added the new

      reshaped_geom := old_edge_geom;

      --silly varrays, why are you so old-timey?
      ordinates.EXTEND((old_edge_geom.sdo_ordinates.COUNT-2) +
                       (new_edge_geom.sdo_ordinates.COUNT-2));


      --Build the new geom, skipping the node we are removing
      IF p_tip = 'START'
      THEN

         --new to old
         FOR i in 1 .. new_edge_geom.SDO_ORDINATES.COUNT
         LOOP

            IF i = new_edge_geom.SDO_ORDINATES.COUNT
            OR i = (new_edge_geom.SDO_ORDINATES.COUNT - 1)
            THEN

               --skip shared node position
               NULL;

            ELSE

               ord_kount := ord_kount + 1;
               ordinates(ord_kount) := new_edge_geom.SDO_ORDINATES(i);

            END IF;


         END LOOP;

         FOR i in 1 .. old_edge_geom.SDO_ORDINATES.COUNT
         LOOP

            IF i = 1
            OR i = 2
            THEN

               --skip shared node position
               NULL;

            ELSE

               ord_kount := ord_kount + 1;
               ordinates(ord_kount) := old_edge_geom.SDO_ORDINATES(i);

            END IF;


         END LOOP;

      ELSIF p_tip = 'END'
      THEN

         --old to new
         FOR i in 1 .. old_edge_geom.SDO_ORDINATES.COUNT
         LOOP


            IF i = old_edge_geom.SDO_ORDINATES.COUNT
            OR i = (old_edge_geom.SDO_ORDINATES.COUNT - 1)
            THEN

               --skip shared node
               NULL;

            ELSE

               ord_kount := ord_kount + 1;
               ordinates(ord_kount) := old_edge_geom.SDO_ORDINATES(i);

            END IF;


         END LOOP;

         FOR i in 1 .. new_edge_geom.SDO_ORDINATES.COUNT
         LOOP


            IF i = 1
            OR i = 2
            THEN

               --skip shared node
               NULL;

            ELSE

               ord_kount := ord_kount + 1;
               ordinates(ord_kount) := new_edge_geom.SDO_ORDINATES(i);

            END IF;


         END LOOP;


      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Yo, tips are either starts or ends, you said ' || p_tip);

      END IF;


      reshaped_geom.sdo_ordinates := ordinates;

      --This should always work, shape stays the same, just removes node
      SDO_TOPO_MAP.REMOVE_NODE(p_newtopo, p_old_node);

      --CALL Change Edge coords
      --One of our edges flew the coop when we removed the node above
      --Find out whats left

      psql := 'SELECT e.edge_id FROM '
           || p_newtopo || '_edge$ e '
           || 'WHERE e.edge_id IN (:p1,:p2) ';

      EXECUTE IMMEDIATE psql INTO remaining_edge USING p_old_edge,
                                                       p_new_edge;

      BEGIN

         SDO_TOPO_MAP.CHANGE_EDGE_COORDS(p_newtopo,
                                         remaining_edge,
                                         reshaped_geom);

      EXCEPTION
         WHEN OTHERS
         THEN

         IF UPPER(SQLERRM) LIKE '%CHANGED EDGE COORDINATE STRING HAS AN INTERSECTION WITH ANOTHER EDGE%'
         THEN

            --Hopefully this is ok
            --We have an angled V where the far side of the V cant get backed up and still go around the close side
            --The resulting shape will not be pretty, but it probably wasn't pretty to begin with, so :-P to it

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newtopo || '_edge$',
                                               'Failed to change edge coords for edge ' || remaining_edge
                                               || ' but thats probably for the best ');

         ELSE

            --What we do?
            RAISE;

         END IF;

      END;

      RETURN TO_CHAR(remaining_edge);

   END REMOVE_NODE_AND_RESHAPE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public for Debug-----------------------------------------------------------------------

   FUNCTION FIND_FAR_EDGE (
      p_schema          IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_mask            IN VARCHAR2,
      p_newtopo         IN VARCHAR2,
      p_new_node        IN NUMBER,
      p_edge_1          IN NUMBER,
      p_edge_1_tip      IN VARCHAR2,
      p_edge_2          IN NUMBER,
      p_edge_2_tip      IN VARCHAR2,
      p_edge_3          IN NUMBER DEFAULT NULL,
      p_edge_3_tip      IN VARCHAR2 DEFAULT NULL
   ) RETURN NUMBER
   AS

      --Matt! 7/16/10
      --Find out which side of the V is farthest to the clip outline
      --Based on the vertices one back from the shared V point
      --Well not really "far" far, just far in the sense that if you drop a line from the
      --   -1 vertex to the new node does it intersect the other edge
      --Starting with the "far" side of the V guarantees both sides make it to the clip
      --All these calls to get geoms arent dumbfficient, but calls to this V section
      --   are fairly rare and overall a drop in the bhuket.  Should probably revisit though

      --Added edge 3 processing 9/7/10
      --Added TOUCH to second test 2/4/11
      --Added overlapbdyintersect to second test 11/3/11

      psql              VARCHAR2(4000);
      edge_1_geom       SDO_GEOMETRY;
      edge_2_geom       SDO_GEOMETRY;
      edge_3_geom       SDO_GEOMETRY;
      edge_1_test       SDO_GEOMETRY;
      edge_2_test       SDO_GEOMETRY;
      edge_3_test       SDO_GEOMETRY;
      node_pt           SDO_GEOMETRY;
      far_edge          NUMBER;
      distance_1        NUMBER;
      distance_2        NUMBER;
      p_debug           PLS_INTEGER := 0;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FIND_FAR_EDGE: Start ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      edge_1_geom := GET_EDGE_GEOMETRY(p_newtopo,p_edge_1);
      edge_2_geom := GET_EDGE_GEOMETRY(p_newtopo,p_edge_2);
      node_pt     := GET_NODE_GEOMETRY(p_newtopo,p_new_node);

      IF p_edge_3 IS NOT NULL
      THEN
         edge_3_geom := GET_EDGE_GEOMETRY(p_newtopo,p_edge_3);
      END IF;


      IF p_edge_1_tip = 'START'
      THEN

         --start means from the dangle toward the interior

          edge_1_test := SDO_GEOMETRY(
                                      2002,
                                      edge_1_geom.sdo_srid,
                                      NULL,
                                      SDO_ELEM_INFO_ARRAY(1,2,1),
                                      SDO_ORDINATE_ARRAY(edge_1_geom.sdo_ordinates(3),
                                                         edge_1_geom.sdo_ordinates(4),
                                                         node_pt.SDO_POINT.X,
                                                         node_pt.SDO_POINT.Y)
                                 );

      ELSE

         --interior toward clip
         edge_1_test := SDO_GEOMETRY(
                                     2002,
                                     edge_1_geom.sdo_srid,
                                     NULL,
                                     SDO_ELEM_INFO_ARRAY(1,2,1),
                                     SDO_ORDINATE_ARRAY(edge_1_geom.sdo_ordinates(edge_1_geom.sdo_ordinates.COUNT - 3),
                                                        edge_1_geom.sdo_ordinates(edge_1_geom.sdo_ordinates.COUNT - 2),
                                                        node_pt.SDO_POINT.X,
                                                        node_pt.SDO_POINT.Y)
                                    );

      END IF;

      IF p_edge_2_tip = 'START'
      THEN

         --start means from the dangle toward the interior

         edge_2_test := SDO_GEOMETRY(
                                     2002,
                                     edge_2_geom.sdo_srid,
                                     NULL,
                                     SDO_ELEM_INFO_ARRAY(1,2,1),
                                     SDO_ORDINATE_ARRAY(edge_2_geom.sdo_ordinates(3),
                                                        edge_2_geom.sdo_ordinates(4),
                                                        node_pt.SDO_POINT.X,
                                                        node_pt.SDO_POINT.Y)
                                 );

      ELSE

         --interior toward clip
         edge_2_test := SDO_GEOMETRY(
                                     2002,
                                     edge_2_geom.sdo_srid,
                                     NULL,
                                     SDO_ELEM_INFO_ARRAY(1,2,1),
                                     SDO_ORDINATE_ARRAY(edge_2_geom.sdo_ordinates(edge_2_geom.sdo_ordinates.COUNT - 3),
                                                        edge_2_geom.sdo_ordinates(edge_2_geom.sdo_ordinates.COUNT - 2),
                                                        node_pt.SDO_POINT.X,
                                                        node_pt.SDO_POINT.Y)
                                    );

      END IF;

      IF p_edge_3_tip IS NOT NULL
      AND p_edge_3_tip = 'START'
      THEN

         --start means from the dangle toward the interior

         edge_3_test := SDO_GEOMETRY(
                                     2002,
                                     edge_3_geom.sdo_srid,
                                     NULL,
                                     SDO_ELEM_INFO_ARRAY(1,2,1),
                                     SDO_ORDINATE_ARRAY(edge_3_geom.sdo_ordinates(3),
                                                        edge_3_geom.sdo_ordinates(4),
                                                        node_pt.SDO_POINT.X,
                                                        node_pt.SDO_POINT.Y)
                                 );

      ELSIF p_edge_3_tip IS NOT NULL
      THEN

         --interior toward clip
         edge_3_test := SDO_GEOMETRY(
                                     2002,
                                     edge_3_geom.sdo_srid,
                                     NULL,
                                     SDO_ELEM_INFO_ARRAY(1,2,1),
                                     SDO_ORDINATE_ARRAY(edge_3_geom.sdo_ordinates(edge_3_geom.sdo_ordinates.COUNT - 3),
                                                        edge_3_geom.sdo_ordinates(edge_3_geom.sdo_ordinates.COUNT - 2),
                                                        node_pt.SDO_POINT.X,
                                                        node_pt.SDO_POINT.Y)
                                    );

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FIND_FAR_EDGE: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Hey you
      --Cant use anyinteract because the edges in question may be so short that they
      --   connect already back at their interior start/end points
      --OVERLAPBDYDISJOINT means they X, not just 0---0---0  This, overlapbdydisjoint is the suspected issue
      --Touch means 0---A---0---B---0
      --Overlapbdyintersect (??) means   ___A__0____
      --                                       |
      --                                       B
      --                                       |
      --Hey one more thing
      --sdo_geom.relate return the name of the mask or FALSE.
      --   Dont even think about putting 'TRUE' in here

      IF p_edge_3 IS NULL
      THEN

         IF p_debug = 1
         THEN
             dbms_output.put_line('edge1 test:');
             dbms_output.put_line(GZ_GEOM_UTILS.DUMP_SDO(edge_1_test));
             dbms_output.put_line('edge2 test:');
             dbms_output.put_line(GZ_GEOM_UTILS.DUMP_SDO(edge_2_test));
         END IF;

         --This is working for 2 connects, keep it separate

         IF sdo_geom.relate(edge_1_test,'OVERLAPBDYDISJOINT',edge_2_geom,.05) = 'OVERLAPBDYDISJOINT'  --tolerance is meh
         THEN

            --edge1 will X intersect edge2 if we draw a line from its vertex-1 to the new node
            --So we must process it first, and then just bail on its reshape
            RETURN p_edge_1;

         ELSIF sdo_geom.relate(edge_2_test,'OVERLAPBDYDISJOINT',edge_1_geom,.05) = 'OVERLAPBDYDISJOINT'
         THEN

            --edge2 will intersect, it must be processed first
            RETURN p_edge_2;

         ELSE


            IF p_debug = 1
            THEN
               dbms_output.put_line('no basic interaction');
            END IF;

            --Sometimes the dangling tip is so close to the boundary that overlapbdydisjoint doesnt
            --see an actual X

            --Try again with no srid and mad silly tolerance to catch those rare cases
            edge_1_test.sdo_srid := NULL;
            edge_2_test.sdo_srid := NULL;
            edge_1_geom.sdo_srid := NULL;
            edge_2_geom.sdo_srid := NULL;

            --Add touch to this more precise test 2/4/11
            --in case the proposed edge skims the tip of the other side of the V

            IF sdo_geom.relate(edge_1_test,
                              'OVERLAPBDYDISJOINT+TOUCH+OVERLAPBDYINTERSECT',
                              edge_2_geom,
                              .00000005) = 'OVERLAPBDYDISJOINT+TOUCH+OVERLAPBDYINTERSECT'  --tolerance is not meh
            THEN                                                                                               --based on one case only however

               --edge1 will X intersect edge2 very near the tip if we draw a line from its vertex-1 to the new node
               --So we must process it first, and then just bail on its reshape
               RETURN p_edge_1;

            ELSIF sdo_geom.relate(edge_2_test,
                                  'OVERLAPBDYDISJOINT+TOUCH+OVERLAPBDYINTERSECT',
                                  edge_1_geom,
                                  .00000005) = 'OVERLAPBDYDISJOINT+TOUCH+OVERLAPBDYINTERSECT'
            THEN

               --edge2 will intersect, it must be processed first
               RETURN p_edge_2;

            ELSE

               --Is there some other rule here? distance? anything?
               --I think the result will be the same regardless of which we return here
               --Stay with the first for the feng shui
               RETURN p_edge_1;

            END IF;

         END IF;

      ELSE

         --First choice: we are looking for the far tine in the pitchfork
         --it should be the one that intersects both of the others if we have a troubling rotated pitchfork

         IF sdo_geom.relate(edge_1_test,'OVERLAPBDYDISJOINT',edge_2_geom,.05) = 'OVERLAPBDYDISJOINT'  --tolerance is meh
         AND sdo_geom.relate(edge_1_test,'OVERLAPBDYDISJOINT',edge_3_geom,.05) = 'OVERLAPBDYDISJOINT'
         THEN

            RETURN p_edge_1;

         ELSIF sdo_geom.relate(edge_2_test,'OVERLAPBDYDISJOINT',edge_1_geom,.05) = 'OVERLAPBDYDISJOINT'  --tolerance is meh
         AND sdo_geom.relate(edge_2_test,'OVERLAPBDYDISJOINT',edge_3_geom,.05) = 'OVERLAPBDYDISJOINT'
         THEN

            RETURN p_edge_2;

         ELSIF sdo_geom.relate(edge_3_test,'OVERLAPBDYDISJOINT',edge_1_geom,.05) = 'OVERLAPBDYDISJOINT'  --tolerance is meh
         AND sdo_geom.relate(edge_3_test,'OVERLAPBDYDISJOINT',edge_2_geom,.05) = 'OVERLAPBDYDISJOINT'
         THEN

            RETURN p_edge_3;

         ELSE

            --Nobody intersects both, so take the first one that inersects at least one
            --This should be a standard pitchfork middle tine with no weird orientation

            IF sdo_geom.relate(edge_1_test,'OVERLAPBDYDISJOINT',edge_2_geom,.05) = 'OVERLAPBDYDISJOINT'  --tolerance is meh
            OR sdo_geom.relate(edge_1_test,'OVERLAPBDYDISJOINT',edge_3_geom,.05) = 'OVERLAPBDYDISJOINT'
            THEN

               RETURN p_edge_1;

            ELSIF sdo_geom.relate(edge_2_test,'OVERLAPBDYDISJOINT',edge_1_geom,.05) = 'OVERLAPBDYDISJOINT'  --tolerance is meh
            OR sdo_geom.relate(edge_2_test,'OVERLAPBDYDISJOINT',edge_3_geom,.05) = 'OVERLAPBDYDISJOINT'
            THEN

               RETURN p_edge_2;

            ELSIF sdo_geom.relate(edge_3_test,'OVERLAPBDYDISJOINT',edge_1_geom,.05) = 'OVERLAPBDYDISJOINT'  --tolerance is meh
            OR sdo_geom.relate(edge_3_test,'OVERLAPBDYDISJOINT',edge_2_geom,.05) = 'OVERLAPBDYDISJOINT'
            THEN

               RETURN p_edge_3;

            ELSE

               RAISE_APPLICATION_ERROR(-20001,'Funky pitchfork, lets see him');

            END IF;

         END IF;

      END IF;



   END FIND_FAR_EDGE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC FOR DEBUG-----------------------------------------------------------------------

   FUNCTION EXTEND_DANGLING_V_NODES (
      p_schema          IN VARCHAR2,
      p_release         IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_mask            IN VARCHAR2,
      p_newtopo         IN VARCHAR2,
      p_oldtopo         IN VARCHAR2,
      p_newedgetable    IN VARCHAR2,
      p_oldedgetable    IN VARCHAR2,
      p_oldnode         IN NUMBER,
      p_srid            IN NUMBER,
      p_tolerance       IN NUMBER,
      p_debug           IN PLS_INTEGER
   ) RETURN NUMBER
   AS

      --Matt! 7/13/10
      --We have a V shape that used to be against the state/clip outline
      --Now its floating internally
      --Move the node at the V tip to the new clip outline (goal)

      --Steps, in Topoglish:
      --Add a new close node on the clip outline, this is SOP
      --Add a new edge from the tip of the V to the new clip outline, also SOP
      --Associate that edge with the feature on one side of the V
      --   Always use the "far" edge to guarantee that it gets "around" the V tip
      --Reshape the other side of the V so the tip of the V shifts down to the new node, bypassing the dangle node
      --Remove the node that formed the V tip on edge #1, if possible, causing it to shoot right to the clip
      --Salud!

      --Added edge3 processing 9/7/10
      --Modified processing to check all possible dangling Vs 9/8/10

      --Catch when V is still precisely on state outline 1/11/11

      psql              VARCHAR2(4000);
      clip_parms        GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      newcliptable      VARCHAR2(32);
      new_node_id       NUMBER;
      --special type
      TYPE              v_edges_rec IS RECORD (
      feature_edge      NUMBER,
      edge_id           NUMBER,
      start_node        NUMBER,
      end_node          NUMBER
      );
      TYPE v_edge_tab   IS TABLE OF v_edges_rec;
      edge_tab          v_edge_tab;
      node_kount        GZ_CLIP.numberhash;
      old_nodegeom      SDO_GEOMETRY;
      dup_nodes         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      pkey              VARCHAR2(4000);
      extend_kount      PLS_INTEGER := 0;
      dangling_node     NUMBER;
      dangling_edge_1   NUMBER;
      dangling_edge_2   NUMBER;
      dangling_edge_3   NUMBER;
      new_edge_id       NUMBER;
      edge1_tip         VARCHAR2(32);
      edge2_tip         VARCHAR2(32);
      edge3_tip         VARCHAR2(32);
      ret_val           VARCHAR2(4000);
      feature_edge_1    NUMBER;
      feature_edge_2    NUMBER;
      feature_edge_3    NUMBER;
      far_edge          NUMBER;
      edge1stash        NUMBER;
      tip1stash         VARCHAR2(32);
      feature1stash     NUMBER;
      kount             PLS_INTEGER;

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 5');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_DANGLING_V_NODES: Set up basics ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      --set up basic names
      -- ie SDZ1AL_GEN_ST_EDGES_HI
      newcliptable := p_project_id || p_jobid || '_' || clip_parms.gen_clip_table; --our task at hand



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_DANGLING_V_NODES: Start ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --we have two features that are joint at at least one node (possibly more)
      --we need to find the node where they connect that corresponds to the old V


      psql := 'SELECT a.edge_id, e.edge_id, e.start_node_id, e.end_node_id '
           || 'FROM '
           || p_newedgetable || ' a, '
           || p_newtopo || '_relation$ r, '
           || p_newtopo || '_edge$ e '
           || 'WHERE '
           || 'a.topogeom.tg_id = r.tg_id AND '
           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'r.topo_id = e.edge_id AND '
           || 'a.dangle LIKE :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_tab USING '%' || TO_CHAR(p_oldnode) || '%' ;


      IF edge_tab.COUNT < 2
      THEN

         --This is allowable if dangles were totally exterior and got deleted
         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newedgetable,
                                               'Decided that node ' || p_oldnode || ' is no longer in play as a dangling V',
                                                NULL,NULL,NULL,psql);

         RETURN '0';

      END IF;


      --This is a list of edges connected to the V node
      --They could be connected somewhere else too, like a diamond shape
      --But we know that one of the shared nodes is THE ONE
      --The features in question may also be made up of other unrelated edges and nodes

      --Put all the nodes in a list with their counts
      --Somehow I imagine this is faster than the SQL to do it above in one grab

      FOR i in 1 .. edge_tab.COUNT
      LOOP

         IF i = 1
         THEN

            node_kount(edge_tab(i).start_node) := 1;
            node_kount(edge_tab(i).end_node) := 1;

         ELSE

            --manage this start node
            IF node_kount.EXISTS(edge_tab(i).start_node)
            THEN
               node_kount(edge_tab(i).start_node) := node_kount(edge_tab(i).start_node) + 1;
            ELSE
               node_kount(edge_tab(i).start_node) := 1;
            END IF;

            --manage this end node
            IF node_kount.EXISTS(edge_tab(i).end_node)
            THEN
               node_kount(edge_tab(i).end_node) := node_kount(edge_tab(i).end_node) + 1;
            ELSE
               node_kount(edge_tab(i).end_node) := 1;
            END IF;


         END IF;

      END LOOP;



      --put them in a more manageable list
      --not using the sdo_nn where this was used
      --pkey := node_kount.FIRST;
      --LOOP

         --EXIT WHEN NOT node_kount.EXISTS(pkey);

         --IF node_kount(pkey) > 1
         --THEN

            --only trap the ones with node count of 2 or greater (3 will be a yikes)
            --extend_kount := extend_kount + 1;
            --dup_nodes.EXTEND(1);
            --dup_nodes(extend_kount) := pkey;

         --END IF;

         --pkey := node_kount.NEXT(pkey);

      --END LOOP;


      --One of the nodes in dup_nodes is the one
      --Get the original state outline node
      --Dont forget to transform to current projection!


      psql := 'SELECT SDO_CS.TRANSFORM(n.geometry,:p1) FROM '
            || p_oldtopo || '_node$ n '
            || 'WHERE n.node_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO old_nodegeom USING p_srid,
                                                     p_oldnode;

      --Now find the closest node in the list to this target point


      --NB this version doesnt work
      --something about subqueries in SDO_NN blah blah
      --psql := 'SELECT /*+ FIRST_ROWS */ n.node_id '
           --|| 'FROM '
           --|| p_newtopo || '_node$ n '
           --|| 'WHERE SDO_NN(n.geometry, :p1, :p2) = :p3 '
           --|| 'AND n.node_id IN (SELECT * FROM TABLE(:p4)) '
           --|| 'AND rownum < 2 ';

      --batch size = 2, we're not gonna find another node near this guy
      --They should be almost on top of each other
      --psql := 'SELECT node_id FROM ( '
           --|| 'SELECT /*+ FIRST_ROWS */ n.node_id, sdo_nn_distance(1) dist FROM '
           --|| p_newtopo || '_node$ n '
           --|| 'WHERE SDO_NN(n.geometry, :p1, :p2, 1) = :p3 '
           --|| 'AND n.node_id IN (SELECT * FROM TABLE(:p4)) '
           --|| 'order by dist '
           --|| ') '
           --|| 'WHERE rownum = 1 ';


      --EXECUTE IMMEDIATE psql INTO dangling_node USING old_nodegeom,
                                                     -- 'sdo_batch_size=2',
                                                     -- 'TRUE',
                                                      --dup_nodes;

      ----------
      --NEW PLAN
      ----------
      --get the closest node in all of the current topo
      --   to the original dangle node on the state outline. If this node is still there, they are practically on top of one another
      --If this sdo_nn nearest result guy isn't one of our dupes for the attached edges, then the V tip got clipped off
      psql := 'SELECT node_id FROM ( '
           || 'SELECT /*+ FIRST_ROWS */ n.node_id, sdo_nn_distance(1) dist FROM '
           || p_newtopo || '_node$ n '
           || 'WHERE SDO_NN(n.geometry, :p1, :p2, 1) = :p3 '
           || 'order by dist '
           || ') '
           || 'WHERE rownum = 1 ';

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newedgetable,
                                               'Find closest current node to old node: ' || p_oldnode ,
                                                NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql INTO dangling_node USING old_nodegeom,
                                                      'sdo_batch_size=2',
                                                      'TRUE';



      IF node_kount.EXISTS(dangling_node)
      THEN

         IF node_kount(dangling_node) < 2
         THEN

            --EXIT
            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newedgetable,
                                               'Decided that node ' || p_oldnode || ' is no longer in play as a dangling V',
                                                NULL,NULL,NULL,psql);
            RETURN '0';

         ELSE

            --Rare, but does happen
            --Check if the dangling V, by the grace of Descartes, ended up exactly on the state outline

            psql := 'WITH all_nodes AS '
                 || '( '
                 || 'SELECT e.start_node_id node_id FROM '
                 || newcliptable || ' a, '
                 || p_newtopo || '_relation$ r, '
                 || p_newtopo || '_edge$ e '
                 || 'WHERE a.topogeom.tg_id = r.tg_id '
                 || 'AND a.topogeom.tg_layer_id = r.tg_layer_id '
                 || 'AND r.topo_id = e.edge_id '
                 || 'UNION ALL '
                 || 'SELECT e.end_node_id node_id FROM '
                 || newcliptable || ' a, '
                 || p_newtopo || '_relation$ r, '
                 || p_newtopo || '_edge$ e '
                 || 'WHERE a.topogeom.tg_id = r.tg_id '
                 || 'AND a.topogeom.tg_layer_id = r.tg_layer_id '
                 || 'AND r.topo_id = e.edge_id '
                 || ') '
                 || 'SELECT count(*) FROM all_nodes '
                 || 'WHERE node_id = :p1 ';

            EXECUTE IMMEDIATE psql INTO kount USING dangling_node;

            IF kount > 0
            THEN

               --EXIT
               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newedgetable,
                                                'Decided that node ' || p_oldnode || ' is still on the clip outline so is no longer in play as a dangling V',
                                                NULL,NULL,NULL,psql);
               RETURN '0';

            ELSE

               ------------------
               --CONTINUE
               ------------------
               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newedgetable,
                                                  'Decided that node ' || dangling_node || ' is the current dangling V',
                                                   NULL,NULL,NULL,psql);


            END IF;

         END IF;


      ELSE

            --totally clipped off

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newedgetable,
                                               'Decided that node ' || p_oldnode || ' is no longer in play as a dangling V',
                                                NULL,NULL,NULL,psql);
            RETURN '0';


      END IF;


      --Worry that there is still a chance that the closest
      --candidate is some sort of interior triangle meeting point
      --Hopefully there would still be some closer on state node if
      --the true dangle got clipped off


      --Get our edges connected to this node
      FOR i in 1 ..  edge_tab.COUNT
      LOOP

         IF (edge_tab(i).start_node = dangling_node
         OR edge_tab(i).end_node = dangling_node)
         AND dangling_edge_1 IS NULL
         THEN

            dangling_edge_1 := edge_tab(i).edge_id;

         ELSIF (edge_tab(i).start_node = dangling_node
         OR edge_tab(i).end_node = dangling_node)
         AND dangling_edge_2 IS NULL
         THEN

            dangling_edge_2 := edge_tab(i).edge_id;

         ELSIF (edge_tab(i).start_node = dangling_node
         OR edge_tab(i).end_node = dangling_node)
         AND dangling_edge_3 IS NULL
         THEN

            dangling_edge_3 := edge_tab(i).edge_id;

         ELSIF (edge_tab(i).start_node = dangling_node
         OR edge_tab(i).end_node = dangling_node)
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Yo, I dont know what to do with a quadruple connected V. Thats not even a V any more!');

         END IF;

      END LOOP;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_DANGLING_V_NODES: Add a node near dangling node ' || dangling_node );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF dangling_edge_3 IS NULL
      THEN

         --Reuse this trusty mess
         new_node_id := GZ_CLIP.ADD_A_NODE(p_schema,
                                           p_release,
                                           p_project_id,
                                           p_jobid,
                                           p_mask,
                                           p_newtopo,
                                           dangling_node, --dangling node id
                                           dangling_edge_1, --dangling edge id, doesnt matter which one at this point
                                           3, --override the number of edges we allow to touch
                                           p_tolerance,
                                           p_debug);

      ELSE

         --Reuse this trusty mess
         new_node_id := GZ_CLIP.ADD_A_NODE(p_schema,
                                           p_release,
                                           p_project_id,
                                           p_jobid,
                                           p_mask,
                                           p_newtopo,
                                           dangling_node, --dangling node id
                                           dangling_edge_1, --dangling edge id, doesnt matter which one at this point
                                           4, --override +1 more the number of edges we allow to touch
                                           p_tolerance,
                                           p_debug);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 25');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_DANGLING_V_NODES: Find the far edge of the V ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Whether anything is a tip of START or END varies depending on which dangling edge we are working with
      --Take a quick spin through this guy to find out
      --Get the feature edge while we are at it
      --Should have just put all this in a hash with edge as the key
      --   Should just use get_node_star

      FOR i in 1 .. edge_tab.COUNT
      LOOP

         IF edge_tab(i).edge_id = dangling_edge_1
         AND edge_tab(i).start_node = dangling_node
         THEN

            edge1_tip := 'START';
            feature_edge_1 := edge_tab(i).feature_edge;

         ELSIF edge_tab(i).edge_id = dangling_edge_1
         AND edge_tab(i).end_node = dangling_node
         THEN

            edge1_tip := 'END';
            feature_edge_1 := edge_tab(i).feature_edge;

         END IF;

         IF edge_tab(i).edge_id = dangling_edge_2
         AND edge_tab(i).start_node = dangling_node
         THEN

            edge2_tip := 'START';
            feature_edge_2 := edge_tab(i).feature_edge;

         ELSIF edge_tab(i).edge_id = dangling_edge_2
         AND edge_tab(i).end_node = dangling_node
         THEN

            edge2_tip := 'END';
            feature_edge_2 := edge_tab(i).feature_edge;

         END IF;

         IF dangling_edge_3 IS NOT NULL
         THEN

            IF edge_tab(i).edge_id = dangling_edge_3
            AND edge_tab(i).start_node = dangling_node
            THEN

               edge3_tip := 'START';
               feature_edge_3 := edge_tab(i).feature_edge;

            ELSIF edge_tab(i).edge_id = dangling_edge_3
            AND edge_tab(i).end_node = dangling_node
            THEN

               edge3_tip := 'END';
               feature_edge_3 := edge_tab(i).feature_edge;

            END IF;

         END IF;

      END LOOP;


      far_edge := GZ_CLIP.FIND_FAR_EDGE(p_schema,
                                        p_project_id,
                                        p_jobid,
                                        p_mask,
                                        p_newtopo,
                                        new_node_id,
                                        dangling_edge_1,
                                        edge1_tip,
                                        dangling_edge_2,
                                        edge2_tip,
                                        dangling_edge_3,
                                        edge3_tip);



      --this works, dont mess with it
      IF far_edge = dangling_edge_2
      THEN

         --Switcheroo
         edge1stash       := dangling_edge_1;
         tip1stash        := edge1_tip;
         feature1stash    := feature_edge_1;

         dangling_edge_1 := dangling_edge_2;
         edge1_tip       := edge2_tip;
         feature_edge_1  := feature_edge_2;

         dangling_edge_2 := edge1stash;
         edge2_tip       := tip1stash;
         feature_edge_2  := feature1stash;

      ELSIF far_edge = dangling_edge_3
      THEN

         --Switcheroo
         edge1stash       := dangling_edge_1;
         tip1stash        := edge1_tip;
         feature1stash    := feature_edge_1;

         dangling_edge_1 := dangling_edge_3;
         edge1_tip       := edge3_tip;
         feature_edge_1  := feature_edge_3;

         dangling_edge_3 := edge1stash;
         edge3_tip       := tip1stash;
         feature_edge_3  := feature1stash;

      END IF;


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newedgetable,
                                          'Decided that edge ' || far_edge || ' on feature '
                                          || feature_edge_1 || ' should get processed first');



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_DANGLING_NODES: Add an edge to ' || p_newtopo || '_edge$ ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------



      --old faithful here
      IF dangling_edge_3 IS NULL
      THEN

         --works
         new_edge_id := GZ_CLIP.ADD_NEW_EDGES(p_project_id,
                                              p_jobid,
                                              p_newtopo,
                                              dangling_node,
                                              new_node_id,
                                              edge1_tip,
                                              dangling_edge_1,
                                              1 --override the check on the number of allowable connected edges
                                              );

      ELSE

         --works
         new_edge_id := GZ_CLIP.ADD_NEW_EDGES(p_project_id,
                                              p_jobid,
                                              p_newtopo,
                                              dangling_node,
                                              new_node_id,
                                              edge1_tip,
                                              dangling_edge_1,
                                              2 --override the check on the number of allowable connected edges
                                              );  --2 on state, one dangle are allowed already,  plus 2 more on pitchfork

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_DANGLING_V_NODES: Add new edge ' || new_edge_id
                                              || ' to the feature table ' || p_newedgetable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newedgetable,
                                          'Adding edge ' || new_edge_id || ' to ' || p_newedgetable);



      --Use topogeom constructor to add this new edge to our dangling feature "edge" (originial, not topo)
      --Also reusing code from regular dangle business
      ret_val := GZ_CLIP.UPDATE_SOME_TOPO(p_newtopo,
                                          p_newedgetable,
                                          'TOPOGEOM',    --parameterize this
                                          'EDGE_ID',     --this too
                                          feature_edge_1,
                                          new_edge_id);

      IF ret_val != '0'
      THEN
         --I want to know about this right here
         RAISE_APPLICATION_ERROR(-20001,'Couldnt add ' || new_edge_id || ' to ' || p_newedgetable);
      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_DANGLING_V_NODES: Reshape ' || dangling_edge_2);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newedgetable,
                                          'Reshaping edge ' || dangling_edge_2);

      ret_val := GZ_CLIP.RESHAPE_EDGE(p_schema,
                                      p_project_id,
                                      p_jobid,
                                      p_mask,
                                      p_newtopo,
                                      dangling_edge_2,  --reshape this edge
                                      new_node_id,      --connect to this new node
                                      dangling_node);   --bypass this old node, either start or end

      IF dangling_edge_3 IS NOT NULL
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newedgetable,
                                             'Reshaping edge ' || dangling_edge_3);

         ret_val := GZ_CLIP.RESHAPE_EDGE(p_schema,
                                         p_project_id,
                                         p_jobid,
                                         p_mask,
                                         p_newtopo,
                                         dangling_edge_3,  --reshape this edge
                                         new_node_id,      --connect to this new node
                                         dangling_node);   --bypass this old node, either start or end

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_DANGLING_V_NODES: Remove node ' || dangling_node
                                                 || ' and reshape edge ' || dangling_edge_1);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_DANGLING_V_NODES ' || p_mask,p_newedgetable,
                                          'Remove node ' || dangling_node || ' and reshape edge ' || dangling_edge_1
                                          ||' with edge ' || new_edge_id);


      --Could also call REMOVE_DANGLE_NODES on a bunch of these
      --If I ever do anything value added in there

      --return remaining edge, dont need it yet
      ret_val := GZ_CLIP.REMOVE_NODE_AND_RESHAPE(p_schema,
                                                 p_project_id,
                                                 p_jobid,
                                                 p_mask,
                                                 p_newtopo,
                                                 dangling_edge_1,  --original processed edge
                                                 new_edge_id,      --new edge goes to clip outline
                                                 dangling_node,    --remove this node
                                                 new_node_id,      --node on clip outline
                                                 edge1_tip
                                                 );



      --RAISE_APPLICATION_ERROR(-20001,'YO');

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_DANGLING_V_NODES: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN dangling_node;


   END EXTEND_DANGLING_V_NODES;

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE --------------------------------------------------------------------------------

   PROCEDURE REMOVE_SHORTY_EDGE (
      p_schema          IN VARCHAR2
      ,p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_featuretable    IN VARCHAR2,
      p_feature_edge    IN NUMBER,
      p_shorty_edge     IN NUMBER
   )
   AS

      tg_layer_id       NUMBER;
      tg_id             NUMBER;
      psql              VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('REMOVE_SHORTY_EDGE: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --SHAME SHAME do not do this

      --delete from relation$
--      psql := 'DELETE FROM ' || p_topo || '_relation$ rr '
--           || 'WHERE '
--           || '(rr.tg_layer_id, rr.tg_id, rr.topo_id, rr.topo_type) IN ( '
--           || 'SELECT r.tg_layer_id, r.tg_id, r.topo_id, r.topo_type '
--           || 'FROM '
--           || p_featuretable || ' a, '
--           || p_topo || '_RELATION$ r, '
--           || p_topo || '_EDGE$ e '
--           || 'WHERE '
--           || 'a.topogeom.tg_id = r.tg_id AND '
--           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
--           || 'r.topo_id = e.edge_id AND '
--           || 'a.edge_id = :p1 AND '
--           || 'e.edge_id = :p2 '
--           || ')';
--
--      EXECUTE IMMEDIATE psql USING p_feature_edge,
--                                   p_shorty_edge;

      --usually 1
      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_topo,
                                                  p_featuretable,
                                                 'TOPOGEOM',
                                                 'LINE');

      --Not too efficient this is rare though
      psql := 'SELECT a.topogeom.tg_id '
           || 'FROM ' || p_featuretable || ' a '
           || 'WHERE a.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO tg_id USING p_feature_edge;



      GZ_CLIP.DELETE_AN_EDGE(p_schema,
                             p_project_id,
                             p_jobid,
                             p_topo,
                             p_featuretable,
                             p_feature_edge,
                             p_shorty_edge,
                             tg_layer_id,
                             tg_id);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('REMOVE_SHORTY_EDGE: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END REMOVE_SHORTY_EDGE;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC FOR DEBUG-----------------------------------------------------------------------


   FUNCTION EXTEND_INTERIOR_DANGLES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_debug          IN PLS_INTEGER DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! 3/17/10
      --Corresponds to step 9 of
      --   http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions
      -- This is the most error-prone step in generalization clipping
      --Fine toothed comb on NULL SRIDs for all(?) work 12/10

      /*Typing saver
      declare
      retval varchar2(4000);
      begin
      retval := GZ_CLIP.EXTEND_INTERIOR_DANGLES('GZCPB1','TST','Z9','47CL','47','Z947CL',1);
      end;

      */


      psql              VARCHAR2(4000);
      psql2             VARCHAR2(4000);
      clip_parms        GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      output            VARCHAR2(4000) := '0';
      start_time        TIMESTAMP;
      newedgetable      VARCHAR2(32);
      newtopo           VARCHAR2(32);
      newcliptable      VARCHAR2(32);
      edges             GZ_TYPES.stringarray;
      edgesgeoms        GZ_CLIP.geomarray;
      nodes             GZ_TYPES.stringarray;
      tips              GZ_TYPES.stringarray;
      feature_edges     GZ_TYPES.stringarray;
      my_cursor         SYS_REFCURSOR;
      retval            VARCHAR2(4000);
      new_node_ids      GZ_TYPES.stringarray;
      nodekount         NUMBER;
      dangling_nodes    GZ_TYPES.stringarray;
      temp_nodes        GZ_TYPES.stringarray;
      distinct_dangles  GZ_TYPES.stringarray;
      kounter           PLS_INTEGER := 0;
      new_edge_id       NUMBER;
      ret_val           VARCHAR2(4000);
      unfixed_dangles   GZ_TYPES.stringarray;
      tolerance         NUMBER;

   BEGIN

      --RAISE_APPLICATION_ERROR(-20001,'HERE');

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_INTERIOR_DANGLES: Set up parms');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES ' || p_mask,NULL,'STARTING');

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      --set up basic names
      --ex Z601CL_Z699IN_EWRK
      newedgetable := GZ_CLIP.GET_EDGE_TABLE(p_project_id,p_jobid,clip_parms.edge_input_table);

      newtopo := p_topo_out;
      -- ie SDZ1AL_GEN_ST_EDGES_HI
      newcliptable := p_project_id || p_jobid || '_' || clip_parms.gen_clip_table; --our task at hand

      --voodooo
      --convert meter tolerance "degrees"
      --may want an Alaska check and call to fourth flag here
      tolerance := GZ_CLIP.TOLERANCE_CONVERTER(clip_parms.gen_clip_tolerance,
                                               TO_CHAR(clip_parms.gen_clip_job_srid),
                                               'NULL');

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_INTERIOR_DANGLES: Identify edges and nodes ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Not sure if I need to distinguish start nodes from end nodes
      --  Yes, when we add the new edge we will want it to flow in the same direction

      --This sql gets edges from our edge feature table that have the same face
      --   on the left and right, meaning they are dangling
      --And ALSO that have a node that connects to no other edge in the topology
      --   (thats the WHERE EXISTS clause)
      --The where exists is necessary because non-dangles can be connected to
      --   dangles.  The connected non-dangles can also have the same L/R
      --   faces.
      --    |          FACE A
      --   |
      --  |  0---Dangle---O---nondangle----0-----
      -- |
      --|             Also FACE A

      --Keep in mind that a dangle could be dangling on both ends
      --Such an edge will be collected twice in the sql below, with diff node IDs


      psql2 := 'SELECT a.edge_id, e.edge_id, e.geometry, e.start_node_id node, CAST(''START'' AS VARCHAR2(32)) TIP '
           || 'FROM '
           || newedgetable || ' a, '
           || newtopo || '_RELATION$ r, '
           || newtopo || '_EDGE$ e '
           || 'WHERE '
           || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
           || 'r.tg_id = a.topogeom.tg_id AND '
           || 'e.edge_id = r.topo_id AND '
           || 'e.left_face_id = e.right_face_id AND '
           || 'NOT EXISTS ( '
           || '   SELECT * FROM ' || newtopo || '_EDGE$ ee '
           || '   WHERE '
           || '   ee.edge_id != e.edge_id AND '
           || '   (e.start_node_id = ee.start_node_id or e.start_node_id = ee.end_node_id) '
           || ' ) '
           || 'UNION ALL '
           || 'SELECT a.edge_id, e.edge_id, e.geometry, e.end_node_id node, CAST(''END'' AS VARCHAR2(32)) TIP '
           || 'FROM '
           || newedgetable || ' a, '
           || newtopo || '_RELATION$ r, '
           || newtopo || '_EDGE$ e '
           || 'WHERE '
           || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
           || 'r.tg_id = a.topogeom.tg_id AND '
           || 'e.edge_id = r.topo_id AND '
           || 'e.left_face_id = e.right_face_id AND '
           || 'NOT EXISTS ( '
           || '   SELECT * FROM ' || newtopo || '_EDGE$ ee '
           || '   WHERE '
           || '   ee.edge_id != e.edge_id AND '
           || '   (e.end_node_id = ee.start_node_id or e.end_node_id = ee.end_node_id) '
           || ' ) ';

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES ' || p_mask,newedgetable,'Opening cursor' ,
                                           NULL,NULL,NULL,psql2);

      OPEN my_cursor FOR psql2;

      LOOP

         FETCH my_cursor BULK COLLECT INTO feature_edges, edges, edgesgeoms, nodes, tips LIMIT 200;
         EXIT WHEN edges.COUNT = 0;



         FOR i IN 1 .. edges.COUNT
         LOOP


            --get either one new node id added
            --or identify an existing node to connect to
            --This is the big one
            new_node_ids(i) := GZ_CLIP.ADD_A_NODE(p_schema,
                                                  p_release,
                                                  p_project_id,
                                                  p_jobid,
                                                  p_mask,
                                                  newtopo,
                                                  nodes(i), --dangling node id
                                                  edges(i), --dangling edge id
                                                  NULL,
                                                  tolerance,
                                                  p_debug);


            IF new_node_ids(i) > 0
            THEN

               --A standard node
               ----------------------------------------------------------------------------------
               --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
               DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
               DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_INTERIOR_DANGLES: Extending ' || newtopo || '_EDGE$ edge ' || edges(i));
               --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
               ----------------------------------------------------------------------------------

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES ' || p_mask,newtopo || '_EDGE$',
                                                   'Extending ' || newtopo || '_EDGE$ edge ' || edges(i));


               --Add new edges, connecting dangle to new node (or vice versa, depends on TIP)
               --   Must go through the step of adding the new edge, so oracle will split the face


               new_edge_id := GZ_CLIP.ADD_NEW_EDGES(p_project_id,
                                                    p_jobid,
                                                    newtopo,
                                                    nodes(i),
                                                    new_node_ids(i),
                                                    tips(i),
                                                    edges(i));



               ----------------------------------------------------------------------------------
               --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
               DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
               DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_INTERIOR_DANGLES: Add new edge ' || new_edge_id || ' to the feature table ' || newedgetable);
               --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
               ----------------------------------------------------------------------------------

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES ' || p_mask,newedgetable,
                                                   'Adding edge ' || new_edge_id || ' to ' || newedgetable);




               --Use topogeom constructor to add this new edge to our dangling feature "edge" (originial, not topo)
               --It might be faster to do a bunch of these at once ?
               ret_val := GZ_CLIP.UPDATE_SOME_TOPO(newtopo,
                                                   newedgetable,
                                                   'TOPOGEOM',    --parameterize this
                                                   'EDGE_ID',     --this too
                                                   feature_edges(i),
                                                   new_edge_id);

               IF ret_val != '0'
               THEN
                  --Sometimes update some topo fails
                  --Lets fail at the end of this section though, not here
                  output := output || ' ' || ret_val;
               END IF;
               ----------------------------------------------------------------------------------
               --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
               DBMS_APPLICATION_INFO.SET_ACTION('Step 70');
               DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_INTERIOR_DANGLES: Remove any duplicate vertices we made');
               --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
               ----------------------------------------------------------------------------------

                -- We may have created some invalid geoms above
                -- This causes the next sdo_nn to crap out finding the nearest edge
                -- If there are duplicate vertices sdo_nn tends to think that anything is 0 away from the dup

                --Switch to FIRST_ROWS hint has helped sdo_nn
                --I still think having duplicate vertices could be a problem on some sdo calls

               --Commented for now.  Living dangerously

               --GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES ' || p_mask,newtopo || '_edge$',
                                                   --'Removing duplicate vertices');

               --GZ_CLIP.CLEAN_UP_GEOMETRIES(p_schema,newtopo || '_EDGE$',2002,'GEOMETRY');

            ELSE

               --Add_a_node returned a negative, fake node id on purpose
               --This means the closest connect point to our dangle is just a node on its opposite side
                     --In state on left, out on right
                     --     |
                     --     |
                     --  X--0 <-- Existing node, we are lost trying to connect the X to the 0
                     --     |     When its actually already connected
                     --     |
               --This dangle is bogus, lets remove it and continue

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES ' || p_mask,newedgetable,
                                                   'Removing edge ' || edges(i) || ' from ' || newtopo);

               GZ_CLIP.REMOVE_SHORTY_EDGE(p_schema,
                                          p_project_id,
                                          p_jobid,
                                          newtopo,
                                          newedgetable,
                                          feature_edges(i),
                                          edges(i));




            END IF;


         END LOOP;  --End loop over this bunch of edges




         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 80');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_INTERIOR_DANGLES: Remove a bunch of extraneous nodes');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES ' || p_mask, newtopo || '_NODE$',
                                             'Removing a bunch of nodes');

         --Attempt to remove our added, now-extraneous nodes
         ret_val := GZ_CLIP.REMOVE_DANGLE_NODES(newtopo,
                                                 nodes); --not new nodes!  old ones, used to dangle

         --Do we have any reason to capture the edge that gets deleted when the node is removed?

         --maybe we can do these earlier
         new_node_ids.DELETE;


         IF ret_val != '0'
         THEN
            output := output || ' ' || ret_val;
         END IF;


      END LOOP; --end loop over dangling single edge cursor


      --eh, why not
      edges.DELETE;
      nodes.DELETE;
      tips.DELETE;
      CLOSE my_cursor;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 90');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_INTERIOR_DANGLES: Check for dangling NODES in ' || newedgetable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      /* Rework for more exacting checks
      psql :=  'SELECT a.dangle ancient_node_id '
            || 'FROM ' || newedgetable || ' a '
            || 'WHERE a.dangle != :p1 '
            || 'AND a.dangle IS NOT NULL '
            || 'GROUP BY a.dangle '
            || 'HAVING COUNT(a.dangle) > 1 ';  --There are occasionally singles at this point
                                               --They were paired up back when we id'd them
                                               --They were edge of universe shorelines outside the state
                                               --Harmless, have no topo pointers remaining if we see them here
                                               --(Im not as confident about this as this comment suggests)

      */

      psql := 'SELECT DISTINCT a.dangle '
           || 'FROM ' || newedgetable || ' a '
           || 'WHERE a.dangle != :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO dangling_nodes USING 'Y';

      FOR i IN 1 .. dangling_nodes.COUNT
      LOOP

         --get distinct nodes from the list
         --some, rarely are comma delimited
         temp_nodes := GZ_BUSINESS_UTILS.SPLIT(dangling_nodes(i),',');

         FOR j IN 1 .. temp_nodes.COUNT
         LOOP

            IF GZ_CLIP.QUERY_DELIMITED_LIST(distinct_dangles, temp_nodes(j)) = 0
            THEN

               kounter := kounter + 1;
               distinct_dangles(kounter) := temp_nodes(j);

            END IF;


         END LOOP;

      END LOOP;

      FOR i in 1 .. distinct_dangles.COUNT
      LOOP

         --Most of the time we do not enter this loop at all

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES ' || p_mask,newedgetable,
                                                'Moving node ' || distinct_dangles(i) || ' (node id from '
                                                || clip_parms.edge_input_table || ') to the new clip outline ');


         --nodes will contain the dangling node, current topo
         nodes(i) := GZ_CLIP.EXTEND_DANGLING_V_NODES(p_schema,
                                                     p_release,
                                                     p_project_id,
                                                     p_jobid,
                                                     p_mask,
                                                     newtopo,
                                                     clip_parms.gen_topology_name,
                                                     newedgetable,
                                                     clip_parms.edge_input_table,
                                                     distinct_dangles(i),
                                                     clip_parms.gen_clip_job_srid,
                                                     tolerance,
                                                     p_debug);





      END LOOP; --END LOOP over dangling original-topo nodes




      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 100');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_INTERIOR_DANGLES: Check for no dangles in ' || newedgetable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --Check for no dangles

      psql := 'SELECT e.edge_id '
           || 'FROM '
           || newedgetable || ' a, '
           || newtopo || '_RELATION$ r, '
           || newtopo || '_EDGE$ e '
           || 'WHERE '
           || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
           || 'r.tg_id = a.topogeom.tg_id AND '
           || 'e.edge_id = r.topo_id AND '
           || 'e.left_face_id = e.right_face_id ';

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES ' || p_mask,newedgetable,'Check for no dangles in ' || newedgetable,
                                           NULL,NULL,NULL,psql);


      EXECUTE IMMEDIATE psql BULK COLLECT INTO unfixed_dangles;

      IF unfixed_dangles.COUNT != 0
      THEN
         output := output || ' | Still have some dangles ';
      END IF;


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES ' || p_mask,newedgetable,'COMPLETED',start_time);

      --What other errors should we be trapping and returning...?
      --Maybe pop out the nodes from newedgetable and sdo_relate if any are outside of the clip mask?

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('EXTEND_INTERIOR_DANGLES: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      RETURN output;


   END EXTEND_INTERIOR_DANGLES;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------


   FUNCTION CREATE_NEW_FACE_TABLE (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/cinco/10
      --10/diecisiete/13 parameterized column lengths

     retval          VARCHAR2(4000) := '0';
     psql            VARCHAR2(4000);
     start_time      TIMESTAMP;
     facetable       VARCHAR2(32);
     geogs           GZ_TYPES.stringarray;
     temp_length     GZ_TYPES.stringarray;
     measurements    GZ_TYPES.stringarray;
     clip_parms      GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
     newtopo         VARCHAR2(32);
     face_ids        GZ_TYPES.stringarray;
     kount           PLS_INTEGER;


   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_NEW_FACE_TABLE',NULL,'STARTING ' || p_mask);

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      -- ie SDZ1AL
      newtopo := p_topo_out;

      -- ie SDZ1AL_GEN_FACES
      facetable := clip_parms.face_output_table;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_NEW_FACE_TABLE: Set up sql string for ' || facetable );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --includes GEOID
      geogs := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                           p_project_id,
                                                           'ATTRIBUTE',
                                                           clip_parms.left_right_attributes);  --REFERENCE_FACE_FIELDS

      measurements := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                                  p_project_id,
                                                                  'MEASUREMENT',
                                                                  clip_parms.face_output_measurements); --also REFERENCE_FACE_FIELDS


      --Build SQL string for face table
      psql := 'CREATE TABLE ' || facetable || ' ('
           || 'FACE_ID NUMBER, '
           || 'TOPOGEOM SDO_TOPO_GEOMETRY, ';

      FOR i in 1 .. geogs.COUNT
      LOOP
      
         IF geogs(i) <> 'GEOID'
         THEN
         
            --return a single length, as a character
            temp_length := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                                       p_project_id,
                                                                       'ATTRIBUTE',
                                                                       clip_parms.left_right_attributes,
                                                                       'FIELD_LENGTH',
                                                                       geogs(i));
                                                                      
         ELSE
         
            temp_length(1) := '4000';
         
         END IF;

         psql := psql || geogs(i) || ' VARCHAR2(' || temp_length(1) || '), ';

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
            psql := psql || ', ';
         ELSE
            psql := psql || ') NOPARALLEL NOLOGGING ';
         END IF;


      END LOOP;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_NEW_FACE_TABLE: Send sql string to generic table creator');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_NEW_FACE_TABLE ' || p_mask,facetable,'Create ' || facetable || ' ' ,
                                          NULL,NULL,NULL,psql);

      CLIP_CREATE_TABLE(p_schema, p_jobid, facetable, psql, 'FACE_ID');


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_NEW_FACE_TABLE: Add face ids to ' || facetable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      psql := 'SELECT a.face_id '
           || 'FROM ' || newtopo || '_face$ a '
           || 'WHERE a.face_id != :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO face_ids USING -1;

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_NEW_FACE_TABLE ' || p_mask,facetable,
                                          'Inserting ' || face_ids.COUNT || ' face_ids into ' || facetable,
                                          NULL,NULL,NULL,psql);

      FORALL ii IN 1 .. face_ids.COUNT
         EXECUTE IMMEDIATE 'INSERT INTO ' || facetable || ' (FACE_ID) '
                        || 'VALUES (:p1)'
         USING face_ids(ii);

      COMMIT;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_NEW_FACE_TABLE: Check output ' || facetable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      psql := 'SELECT count(*) FROM ('
           || 'SELECT face_id FROM ' || facetable || ' '
           || 'MINUS '
           || 'SELECT face_id FROM ' || newtopo || '_face$ '
           || 'WHERE face_id != :p1 '
           || ')';
      EXECUTE IMMEDIATE psql INTO kount USING -1;

      IF kount != 0
      THEN
         retval := retval || 'Problem with ' || facetable || '. '
                          || kount || ' more face_ids than ' ||  newtopo || '_face$ |';
      END IF;

      psql := 'SELECT count(*) FROM ('
           || 'SELECT face_id FROM ' || newtopo || '_face$ '
           || 'WHERE face_id != :p1 '
           || 'MINUS '
           || 'SELECT face_id FROM ' || facetable || ' '
           || ')';
      EXECUTE IMMEDIATE psql INTO kount USING -1;

      IF kount != 0
      THEN
         retval := retval || 'Problem with ' || facetable || '. '
                          || kount || ' fewer face_ids than ' ||  newtopo || '_face$ |';
      END IF;



      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_NEW_FACE_TABLE',facetable,
                                          'COMPLETED ' || p_mask,start_time);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_NEW_FACE_TABLE: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN retval;

   END CREATE_NEW_FACE_TABLE;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public for debug-----------------------------------------------------------------------

   FUNCTION FLIP_INVERTED_FACE (
      p_release         IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_topo            IN VARCHAR2, --working topo
      newedgetab        IN VARCHAR2, --working edge table with L/R
      intopo            IN VARCHAR2, --original input topo
      inedgetab         IN VARCHAR2, --edge table from transfer attributes
      badface           IN VARCHAR2, --the face we IDd
      left_right_atts   IN VARCHAR2,
      p_face_mask       IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 10/31/11
      --Very careful manager of mismatched faces
      --So far just one example in production has met this condition
      --And this code is totally tailored to fix it and it alone

      --The input topology edge cut off a tip of the state defining one small face on one of its sides
      --   |\
      --   |/
      --
      --Now its dangling and I connected it the opposite way, backwards. Its probably at a small angle
      --   ____
      --    \/
      --
      --Must flip the attributes so the little interior face is the original unique small face

      geoids               GZ_TYPES.stringarray;
      geoidkounts          GZ_TYPES.stringarray;
      badedge              NUMBER;
      psql                 VARCHAR2(4000);
      mid_sql              VARCHAR2(4000);
      geogs                GZ_TYPES.stringarray;
      start_node           NUMBER;
      end_node             NUMBER;
      kount                PLS_INTEGER;
      othergeoid           VARCHAR2(4000);
      badside              VARCHAR2(32);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FLIP_INVERTED_FACE: Starting ' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --ID the outlier. Use Geoid as shortcut

      --store this sql once
      mid_sql := '('
              || 'SELECT a.edge_id, a.l_geoid geoid '   -- Left geoid
              || 'FROM ' || newedgetab || ' a, '
              || p_topo || '_relation$ r, '
              || p_topo || '_edge$ e '
              || 'WHERE '
              || 'a.topogeom.tg_id = r.tg_id AND '
              || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
              || 'SDO_GEOM.SDO_LENGTH (e.geometry, .05) > :p1 AND '
              || 'e.left_face_id != :p2 AND '
              || 'e.left_face_id = :p3 AND '            -- Left face is problem
              || 'r.topo_id = e.edge_id '
              || 'UNION ALL '
              || 'SELECT a.edge_id, a.r_geoid geoid '   -- R geoid
              || 'FROM ' || newedgetab || ' a, '
              || p_topo || '_relation$ r, '
              || p_topo || '_edge$ e '
              || 'WHERE '
              || 'a.topogeom.tg_id = r.tg_id AND '
              || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
              || 'SDO_GEOM.SDO_LENGTH (e.geometry, .05) > :p4 AND '
              || 'e.left_face_id != :p5 AND '
              || 'e.right_face_id = :p6 AND '          -- R face is bad
              || 'r.topo_id = e.edge_id '
              || ') ';

      psql := 'WITH mid_sql AS '
           || mid_sql || ' '
           || 'SELECT geoid, count(geoid) FROM mid_sql '
           || 'GROUP BY geoid ORDER BY COUNT(geoid) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO geoids,
                                               geoidkounts USING .05,
                                                                 -1,
                                                                 badface,
                                                                 .05,
                                                                 -1,
                                                                 badface;

      --Check 1. In final topology the mismatched side is
      --         outvoted 2+ to 1

      --Only acceptable configuration to handle
      --One edge outvoted by 2+ others.

      --__,__,__,__,__,1,47 ... 016,54,__,__,__,__,__,54,972701   1
      --__,__,__,__,__,1,47 ... 016,54,__,__,__,__,__,51,972701   7

      IF geoids.COUNT != 2
      OR geoidkounts(1) != 1
      THEN

        RETURN 'Face ' || badface || ' has ' || ' an unmanageable set of L/R votes ';

      END IF;

      --Continue
      --get the bad edge

      psql := 'WITH mid_sql AS '
            || mid_sql || ' '
            || 'SELECT edge_id FROM mid_sql '
            || 'WHERE '
            || 'geoid = :p7 ';

      EXECUTE IMMEDIATE psql INTO badedge USING .05,
                                                -1,
                                                badface,
                                                .05,
                                                -1,
                                                badface,
                                                geoids(1);  --the outlier

      --check 2. Is the other side of the bad edge a match with the alternative votes?
      --         Suggests a flip
      psql := 'SELECT r_geoid othergeoid '
           || 'FROM ' || newedgetab || ' a '
           || 'WHERE '
           || 'a.edge_id = :p1 AND '
           || 'a.l_geoid = :p2 '
           || 'UNION ALL '
           || 'SELECT l_geoid othergeoid '
           || 'FROM ' || newedgetab || ' a '
           || 'WHERE '
           || 'a.edge_id = :p1 AND '
           || 'a.r_geoid = :p2 ';

      EXECUTE IMMEDIATE psql INTO othergeoid USING badedge,
                                                   geoids(1), --the outliner
                                                   badedge,
                                                   geoids(1);
      IF othergeoid != geoids(2)
      THEN

         RETURN 'Geoid on other side of edge ' || badedge || ' doesnt match the proposed flipperoo ';

      END IF;

      --Check 3. Make sure that this edge is one that we IDd as a dangle
      --This implies we may have inverted it

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || newedgetab || ' a '
           || 'WHERE '
           || 'a.edge_id = :p1 AND '
           || 'a.dangle IS NOT NULL ';  --either Y or node ids for dangles

      EXECUTE IMMEDIATE psql INTO kount USING badedge;

      IF kount != 1
      THEN

         RETURN 'Face ' || badface || ' edge ' || badedge || ' is not a dangle ';

      END IF;

      --Now back to the original input topo

      --Check 4. Get the start and end nodes in the original topo for this bad edge
      --         Ensure that both nodes are on the state outline, implying the edge closed of a section of the state

      psql := 'SELECT e.start_node_id, e.end_node_id '
           || 'FROM ' ||  intopo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO start_node,
                                  end_node USING badedge;


      --Count should be 2 - this node has at least 1 state outline edge on both sides of it
      --I think this could be greater than 2 if other states meet at the point, but lets not risk that until we see it

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || intopo || '_edge$ '
           || 'WHERE '
           || '(start_node_id = :p1 or end_node_id = :p2) AND '
           || 'edge_id IN '
           || '(SELECT edge_id FROM ' || inedgetab || ' '
           || 'WHERE ' || p_face_mask || ' = :p3 )';  --state or statefp

      EXECUTE IMMEDIATE psql INTO kount USING start_node,
                                              start_node,
                                              '00';       --flagged edges without same state on both sides as 00

      IF kount != 2
      THEN

         RETURN 'Face ' || badface || ' edge ' || badedge || ' doesnt appear to connect to the original topo clip outline ';

      END IF;

      --now check the other node
      EXECUTE IMMEDIATE psql INTO kount USING end_node,
                                              end_node,
                                              '00';

      IF kount != 2
      THEN

         RETURN 'Face ' || badface || ' edge ' || badedge || ' doesnt appear to connect to the original topo clip outline ';

      END IF;

      --Check 5. In initial topo, ensure that the other side of the bad edge is defined only by the bad edge

      --first, which side is the bad side?

      psql := 'SELECT ''left'' FROM '
           || inedgetab || ' '
           || 'WHERE edge_id = :p1 AND '
           || 'l_geoid = :p2 '
           || 'UNION ALL '
           || 'SELECT ''right'' FROM '
           || inedgetab || ' '
           || 'WHERE '
           || 'edge_id = :p3 AND '
           || 'r_geoid = :p4 ';

      EXECUTE IMMEDIATE psql INTO badside USING badedge,
                                                geoids(1),
                                                badedge,
                                                geoids(1);

      psql := 'SELECT COUNT(ee.edge_id) FROM '
           || inedgetab || ' e, '
           || inedgetab || ' ee '
           || 'WHERE '
           || 'e.edge_id = :p1 AND '
           || '(e.' || badside || '_face_id = ee.left_face_id OR '
           || ' e.' || badside || '_face_id = ee.right_face_id) AND '
           || 'ee.' || p_face_mask || ' != :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING badedge,
                                              '00';

      IF kount != 1
      THEN

         RETURN 'Face ' || badface || ' edge ' || badedge || ' bounds more than one face in the original topo ';

      END IF;


      --Done with checks
      --If we made it this far we will flip

      geogs := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                      p_project_id,
                                                     'ATTRIBUTE',
                                                     left_right_atts);

      /*Sample snippet
      update Z654CL_Z699IN_EWRK a
      set a.l_blkgrpce = a.r_blkgrpce,
          a.r_blkgrpce = a.l_blkgrpce,
          a.l_statefp_sl795 = a.r_statefp_sl795,
          a.r_statefp_sl795 = a.l_statefp_sl795
      where a.edge_id = 1801213
      */

      psql := 'UPDATE ' || newedgetab || ' a '
           || 'SET ';

      FOR i IN 1 .. geogs.COUNT
      LOOP

         IF i != geogs.COUNT
         THEN

            psql := psql || 'a.l_' || geogs(i) || ' = a.r_' || geogs(i) || ', '
                         || 'a.r_' || geogs(i) || ' = a.l_' || geogs(i) || ', ';

         ELSE

            psql := psql || 'a.l_' || geogs(i) || ' = a.r_' || geogs(i) || ', '
                         || 'a.r_' || geogs(i) || ' = a.l_' || geogs(i) || ' ';

         END IF;

      END LOOP;

      psql := psql || 'WHERE '
                   || 'a.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql USING badedge;
      COMMIT;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FLIP_INVERTED_FACE: Peace out ' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN badface;

   END FLIP_INVERTED_FACE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------



   FUNCTION POPULATE_FACE_TABLE (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/cinco/10
      --Populate topgeom and geographies on output face table
      --Also do some error checks
      --corresponds to step 12 here
      --   http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions
      --moved expelled checks to module 10 8/31/10 !
      --Added check on edge length being at least .05 meters
      --   prevents tiny slivers from popping out, facing the wrong direction, on the opposite side of the clip state
      --update to use new reference table for left right fields 11/5/10
      --Still update short (<.05 m) edges if they bound tiny state edge slivers 1/11/11
      --Improved performance for rights update sql 1/27/11

     retval          VARCHAR2(4000) := '0';
     psql            VARCHAR2(4000);
     psql2           VARCHAR2(4000);
     start_time      TIMESTAMP;
     facetable       VARCHAR2(32);
     newtopo         VARCHAR2(32);
     newedgetable    VARCHAR2(32);
     newcliptable    VARCHAR2(32);
     clip_parms      GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
     tg_layer_id     NUMBER;
     geogs           GZ_TYPES.stringarray;
     badfaces        GZ_TYPES.stringarray;
     badlefts        GZ_TYPES.stringarray;
     badrights       GZ_TYPES.stringarray;
     current_ids     GZ_TYPES.stringarray;
     fixedface       VARCHAR2(4000) := '0';
     kount           NUMBER := 0;




   BEGIN

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',NULL,'STARTING ' || p_mask);

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      --geogs := GZ_BUSINESS_UTILS.SPLIT(clip_parms.left_right_attributes, '\|');
      --now this parm is a table name
      geogs := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                      p_project_id,
                                                      'ATTRIBUTE',
                                                      clip_parms.left_right_attributes);


      -- ie SDZ1AL
      newtopo := p_topo_out;
      --ex Z601CL_Z699IN_EWRK
      newedgetable := GZ_CLIP.GET_EDGE_TABLE(p_project_id,p_jobid,clip_parms.edge_input_table);
      -- ie SDZ1AL_GEN_ST_EDGES_HI
      newcliptable := p_project_id || p_jobid || '_' || clip_parms.gen_clip_table;

      -- ie SDZ1AL_GEN_FACES
      facetable := clip_parms.face_output_table;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_FACE_TABLE: Register ' || facetable || ' with ' || newtopo );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,'Register ' || facetable || ' with ' || newtopo);


      --Register face feature table with topology

      BEGIN

         SDO_TOPO.add_topo_geometry_layer(newtopo,facetable,'TOPOGEOM','POLYGON');

      EXCEPTION
      WHEN OTHERS
      THEN

         IF UPPER(SQLERRM) LIKE '%CANNOT ADD TOPO_GEOMETRY LAYER%'
         THEN

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',
                                               facetable,'Deregistering ' || facetable || ' from ' || newtopo || ' first ');


            GZ_TOPO_UTIL.DEREGISTER_FEATURE_TABLES(p_schema,
                                                   newtopo,
                                                   'N',
                                                   0,
                                                   facetable);

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,'Register ' || facetable || ' with ' || newtopo);

            SDO_TOPO.add_topo_geometry_layer(newtopo,facetable,'TOPOGEOM','POLYGON');

         ELSE

            RAISE;

         END IF;

      END;
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_FACE_TABLE: Populate ' || facetable || ' topogeom' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,'Populate ' || facetable || ' topogeom');
      --Populate its topo with existing faces

      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(newtopo,
                                                 facetable,
                                                 'TOPOGEOM',
                                                 'POLYGON');


      psql := 'UPDATE ' || facetable || ' a '
           || 'SET '
           || 'a.topogeom = SDO_TOPO_GEOMETRY(:p1,:p2,:p3,SDO_TOPO_OBJECT_ARRAY(SDO_TOPO_OBJECT (a.face_id,:p4))) ';
      EXECUTE IMMEDIATE psql USING newtopo,
                                   3, --tg layer type
                                   tg_layer_id,
                                   3; --tg layer type again

      COMMIT;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_FACE_TABLE: Check for matching L/R attributes' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      /*reference

      select rights.right_face, rights.geogs, lefts.geogs from (
      select distinct e.right_face_id right_face,
      a.r_cdfp || '|' || a.r_countyfp || '|' || a.r_divisionce || '|' || a.r_nationfp || '|' || a.r_regionce || '|' || a.r_statefp || '|' || a.r_uace || '|' || a.r_geoid  geogs
      from edge_56 a,
      statefp56_relation$ r,
      statefp56_edge$ e
      where a.topogeom.tg_id = r.tg_id and
      a.topogeom.tg_layer_id = r.tg_layer_id and
      r.topo_id = e.edge_id
      ) rights,
      (
      select distinct e.left_face_id left_face,
      a.l_cdfp || '|' || a.l_countyfp || '|' || a.l_divisionce || '|' || a.l_nationfp || '|' || a.l_regionce || '|' || a.l_statefp || '|' || a.l_uace || '|' || a.l_geoid  geogs
      from edge_56 a,
      statefp56_relation$ r,
      statefp56_edge$ e
      where a.topogeom.tg_id = r.tg_id and
      a.topogeom.tg_layer_id = r.tg_layer_id and
      r.topo_id = e.edge_id
      ) lefts
      where rights.right_face = lefts.left_face
      and rights.geogs != lefts.geogs

      */

      psql := 'SELECT rights.right_face, rights.geogs, lefts.geogs FROM '
           || '('
           || 'SELECT DISTINCT e.right_face_id right_face, ';

      FOR i in 1 .. geogs.COUNT
      LOOP

         IF i != geogs.COUNT
         THEN
            psql := psql || 'a.r_' || geogs(i) || ' || ''|'' || ';
         ELSE
            psql := psql || 'a.r_' || geogs(i) || ' geogs ';
         END IF;

      END LOOP;

      psql := psql || ' FROM ' || newedgetable || ' a, '
           || newtopo || '_relation$ r, '
           || newtopo || '_edge$ e '
           || 'WHERE '
           || 'a.topogeom.tg_id = r.tg_id AND '
           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'SDO_GEOM.SDO_LENGTH(e.geometry,.05) > :p1 AND ' --avoid tiny guys popping out on wrong side
           || 'e.right_face_id != :p2 AND '  --May have some coincident with the clipper
           || 'r.topo_id = e.edge_id '        --    Will have mismatched attributes on universal face of course
           || ') rights, '                    --    But whatevs here, it doesnt go into the output face table
           || '('
           || 'SELECT DISTINCT e.left_face_id left_face, ';

      FOR i in 1 .. geogs.COUNT
      LOOP

         IF i != geogs.COUNT
         THEN
            psql := psql || 'a.l_' || geogs(i) || ' || ''|'' || ';
         ELSE
            psql := psql || 'a.l_' || geogs(i) || ' geogs ';
         END IF;

      END LOOP;

      psql := psql || ' FROM ' || newedgetable || ' a, '
           || newtopo || '_relation$ r, '
           || newtopo || '_edge$ e '
           || 'WHERE '
           || 'a.topogeom.tg_id = r.tg_id AND '
           || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
           || 'SDO_GEOM.SDO_LENGTH(e.geometry,.05) > :p3 AND '
           || 'e.left_face_id != :p4 AND '     --See note above
           || 'r.topo_id = e.edge_id '
           || ') lefts '
           || 'WHERE '
           || 'rights.right_face = lefts.left_face AND '
           || 'rights.geogs != lefts.geogs ';


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,'POPULATE_FACE_TABLE: Check for matching L/R attributes',
                                          NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql BULK COLLECT INTO badfaces,
                                               badlefts,
                                               badrights USING .05, -1,
                                                               .05, -1;

      IF badfaces.COUNT != 0
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                            'POPULATE_FACE_TABLE: We have ' || badfaces.COUNT || ' mismatched faces, see if they can be flipped',
                                            NULL,NULL,NULL,NULL);

         --see if we have a rare inverted face that can be flipped

         FOR i IN 1 .. badfaces.COUNT
         LOOP

            --return face id as character or text
            fixedface := GZ_CLIP.FLIP_INVERTED_FACE(p_release,
                                                    p_project_id,
                                                    newtopo,
                                                    newedgetable,
                                                    clip_parms.gen_topology_name,
                                                    clip_parms.edge_input_table,
                                                    badfaces(i),
                                                    clip_parms.left_right_attributes,
                                                    clip_parms.face_feature_mask_col);

            IF fixedface = TO_CHAR(badfaces(i))
            THEN

               --Woot
               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                                  'POPULATE_FACE_TABLE: Supposedly fixed inverted face ' || fixedface,
                                                  NULL,NULL,NULL,NULL);

            ELSE

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                                  'POPULATE_FACE_TABLE: Failed to fix because: ' || fixedface,
                                                  NULL,NULL,NULL,NULL);

            END IF;

         END LOOP;


         --Rerun L/R Checker SQL no matter what the subroutine thinks it did
         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,'POPULATE_FACE_TABLE: AGAIN Check for matching L/R attributes',
                                            NULL,NULL,NULL,psql);

         EXECUTE IMMEDIATE psql BULK COLLECT INTO badfaces,
                                                  badlefts,
                                                  badrights USING .05, -1,
                                                                  .05, -1;


      END IF;

      IF badfaces.COUNT != 0
      THEN

         --probably want to mark these somehow and continue, but for now lets die and investigate
         RAISE_APPLICATION_ERROR(-20001,'We have ' || badfaces.COUNT || ' mismatched faces.  Heres the first one: ' || badfaces(1) );


      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_FACE_TABLE: Execute update statements' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --This update statement with de-duplicating left right attributes into single fields is a brain teaser
      --I know theres a better way, but we'll get this one working first
      --First update all the lefts for all geogs
      --   Fewer nulls on the lefts I think since counterclockwise edge of state has lots o NULLs on right
      --Then go through each geog one by one updating lefts where the geog remains null
      --   Without the not null some of the previously calcd rights become null
      --   And I dont know how to check for the not null for all individually while updating them all
      --   I feel a WHERE EXISTS coming on but blah for now. Oh its on.

      --This is too slow, but I'm not gonna fix it right now
      --See the change to the right update SQL below - face table in subquery and where exists


      /* For reference
               UPDATE Z955CL_CLIP_FACE f
            SET (f.CBSAFP,
                 f.CDFP,
                 f.CNECTAFP,
                 f.COUNTYFP,
                 f.CSAFP,
                 f.DIVISIONCE,
                 f.NECTAFP,
                 f.REGIONCE,
                 f.STATEFP,
                 f.GEOID) =
                   (
         SELECT a.l_CBSAFP,
                             a.l_CDFP,
                             a.l_CNECTAFP,
                             a.l_COUNTYFP,
                             a.l_CSAFP,
                             a.l_DIVISIONCE,
                             a.l_NECTAFP,
                             a.l_REGIONCE,
                             a.l_STATEFP,
                             a.l_GEOID
                        FROM Z955CL_MT955_EWRK a, Z955CL_relation$ r, Z955CL_edge$ e
                       WHERE     a.topogeom.tg_id = r.tg_id
                             AND a.topogeom.tg_layer_id = r.tg_layer_id
                             AND r.topo_id = e.edge_id
                             AND (
                                   (SDO_GEOM.SDO_LENGTH (e.geometry, .05) > .05)
                                   OR
                                   (   e.end_node_id IN (select e.start_node_id from
                                                   Z955CL_STATE_EDGES_Z9_V1 a,
                                                   z955cl_relation$ r,
                                                   z955cl_edge$ e
                                                   where a.topogeom.tg_id = r.tg_id
                                                   and a.topogeom.tg_layer_id = r.tg_layer_id
                                                   and r.topo_id = e.edge_id
                                                   union all
                                                   select e.end_node_id from
                                                   Z955CL_STATE_EDGES_Z9_V1 a,
                                                   z955cl_relation$ r,
                                                   z955cl_edge$ e
                                                   where a.topogeom.tg_id = r.tg_id
                                                   and a.topogeom.tg_layer_id = r.tg_layer_id
                                                   and r.topo_id = e.edge_id)
                                     AND e.start_node_id IN (select e.start_node_id from
                                                   Z955CL_STATE_EDGES_Z9_V1 a,
                                                   z955cl_relation$ r,
                                                   z955cl_edge$ e
                                                   where a.topogeom.tg_id = r.tg_id
                                                   and a.topogeom.tg_layer_id = r.tg_layer_id
                                                   and r.topo_id = e.edge_id
                                                   union all
                                                   select e.end_node_id from
                                                   Z955CL_STATE_EDGES_Z9_V1 a,
                                                   z955cl_relation$ r,
                                                   z955cl_edge$ e
                                                   where a.topogeom.tg_id = r.tg_id
                                                   and a.topogeom.tg_layer_id = r.tg_layer_id
                                                   and r.topo_id = e.edge_id) )
                                  )
                             AND e.left_face_id = f.face_id
                    GROUP BY a.l_CBSAFP,
                             a.l_CDFP,
                             a.l_CNECTAFP,
                             a.l_COUNTYFP,
                             a.l_CSAFP,
                             a.l_DIVISIONCE,
                             a.l_NECTAFP,
                             a.l_REGIONCE,
                             a.l_STATEFP,
                             a.l_GEOID,
                             f.face_id)
      */


      psql := 'UPDATE ' || facetable || ' f '
           || 'SET (';

      FOR i IN 1 .. geogs.COUNT
      LOOP

         IF i != geogs.COUNT
         THEN
            psql := psql || 'f.' || geogs(i) || ', ';
         ELSE
            psql := psql || 'f.' || geogs(i) || ')';
         END IF;

      END LOOP;

      psql := psql || ' = '
                   || '(SELECT ';

      FOR i IN 1 .. geogs.COUNT
      LOOP

         IF i != geogs.COUNT
         THEN
            psql := psql || 'a.l_' || geogs(i) || ', ';
         ELSE
            psql := psql || 'a.l_' || geogs(i) || ' ';
         END IF;

      END LOOP;

      psql := psql || 'FROM '
                   || newedgetable || ' a, '
                   || newtopo || '_relation$ r, '
                   || newtopo || '_edge$ e '
                   || 'WHERE '
                   || 'a.topogeom.tg_id = r.tg_id AND '
                   || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
                   || 'r.topo_id = e.edge_id AND '
                   || '( '
                   || ' (SDO_GEOM.SDO_LENGTH (e.geometry, .05) > .05) '     --must be an edge of real length
                   || ' OR '                                                --Not a poke in | or poke in V
                   || ' (   e.end_node_id IN (select e.start_node_id from ' --OR define an entire itty bitty sliver on state outline
                                          || newcliptable || ' a, '
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id '
                                          || 'union all '
                                          || 'select e.end_node_id from '
                                          || newcliptable || ' a, '
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id) '
                    || '  AND '
                    || '    e.start_node_id IN (select e.start_node_id from '  --Want to use a WITH clause for this
                                          || newcliptable || ' a, '            --But theres a bug in multicolumn updates
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id '
                                          || 'union all '
                                          || 'select e.end_node_id from '
                                          || newcliptable || ' a, '
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id) '
                     || ' ) ' --end start AND end node
                     || ') '  --end length OR start/end
                     || 'AND '
                     || 'e.left_face_id = f.face_id '
                     || 'GROUP BY ';

      FOR i in 1 .. geogs.COUNT
      LOOP

         psql := psql || 'a.l_' || geogs(i) || ', ';

      END LOOP;


      psql := psql || ' f.face_id)';

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,'Populate all the lefts',
                                          NULL,NULL,NULL,psql);


      EXECUTE IMMEDIATE psql;

      COMMIT;

      --now populate the rights each geog by each geog

      FOR i in 1 .. geogs.COUNT
      LOOP

         /*for reference
         UPDATE Z655CL_CLIP_FACE f
          set f.artli =
                (select fff.r_artli from
          (SELECT a.r_ARTLI, ff.face_id
                        FROM Z655CL_MT655_EWRK a,
                             Z655CL_relation$ r,
                             Z655CL_edge$ e,
                             Z655CL_CLIP_FACE ff
                       WHERE     a.topogeom.tg_id = r.tg_id
                             AND a.topogeom.tg_layer_id = r.tg_layer_id
                             AND r.topo_id = e.edge_id
                                                 AND ( (SDO_GEOM.SDO_LENGTH (e.geometry, .05) > .05)
                                  OR (e.end_node_id IN
                                         (SELECT e.start_node_id
                                            FROM Z655CL_STATE_EDGES_Z6_V2 a,
                                                 Z655CL_relation$ r,
                                                 Z655CL_edge$ e
                                           WHERE a.topogeom.tg_id = r.tg_id
                                                 AND a.topogeom.tg_layer_id =
                                                        r.tg_layer_id
                                                 AND r.topo_id = e.edge_id
                                          UNION ALL
                                          SELECT e.end_node_id
                                            FROM Z655CL_STATE_EDGES_Z6_V2 a,
                                                 Z655CL_relation$ r,
                                                 Z655CL_edge$ e
                                           WHERE a.topogeom.tg_id = r.tg_id
                                                 AND a.topogeom.tg_layer_id =
                                                        r.tg_layer_id
                                                 AND r.topo_id = e.edge_id)
                                      AND e.start_node_id IN
                                             (SELECT e.start_node_id
                                                FROM Z655CL_STATE_EDGES_Z6_V2 a,
                                                     Z655CL_relation$ r,
                                                     Z655CL_edge$ e
                                               WHERE a.topogeom.tg_id = r.tg_id
                                                     AND a.topogeom.tg_layer_id =
                                                            r.tg_layer_id
                                                     AND r.topo_id = e.edge_id
                                              UNION ALL
                                              SELECT e.end_node_id
                                                FROM Z655CL_STATE_EDGES_Z6_V2 a,
                                                     Z655CL_relation$ r,
                                                     Z655CL_edge$ e
                                               WHERE a.topogeom.tg_id = r.tg_id
                                                     AND a.topogeom.tg_layer_id =
                                                            r.tg_layer_id
                                                     AND r.topo_id = e.edge_id)) )
                             AND e.right_face_id = ff.face_id
                             AND a.r_artli IS NOT NULL
                             AND ff.artli is null
                    GROUP BY a.r_ARTLI, ff.face_id) fff
                    WHERE fff.face_id = f.face_id)
           WHERE EXISTS
           (select fff.r_artli from
          (SELECT a.r_ARTLI, ff.face_id
                        FROM Z655CL_MT655_EWRK a,
                             Z655CL_relation$ r,
                             Z655CL_edge$ e,
                             Z655CL_CLIP_FACE ff
                       WHERE     a.topogeom.tg_id = r.tg_id
                             AND a.topogeom.tg_layer_id = r.tg_layer_id
                             AND r.topo_id = e.edge_id
                                                 AND ( (SDO_GEOM.SDO_LENGTH (e.geometry, .05) > .05)
                                  OR (e.end_node_id IN
                                         (SELECT e.start_node_id
                                            FROM Z655CL_STATE_EDGES_Z6_V2 a,
                                                 Z655CL_relation$ r,
                                                 Z655CL_edge$ e
                                           WHERE a.topogeom.tg_id = r.tg_id
                                                 AND a.topogeom.tg_layer_id =
                                                        r.tg_layer_id
                                                 AND r.topo_id = e.edge_id
                                          UNION ALL
                                          SELECT e.end_node_id
                                            FROM Z655CL_STATE_EDGES_Z6_V2 a,
                                                 Z655CL_relation$ r,
                                                 Z655CL_edge$ e
                                           WHERE a.topogeom.tg_id = r.tg_id
                                                 AND a.topogeom.tg_layer_id =
                                                        r.tg_layer_id
                                                 AND r.topo_id = e.edge_id)
                                      AND e.start_node_id IN
                                             (SELECT e.start_node_id
                                                FROM Z655CL_STATE_EDGES_Z6_V2 a,
                                                     Z655CL_relation$ r,
                                                     Z655CL_edge$ e
                                               WHERE a.topogeom.tg_id = r.tg_id
                                                     AND a.topogeom.tg_layer_id =
                                                            r.tg_layer_id
                                                     AND r.topo_id = e.edge_id
                                              UNION ALL
                                              SELECT e.end_node_id
                                                FROM Z655CL_STATE_EDGES_Z6_V2 a,
                                                     Z655CL_relation$ r,
                                                     Z655CL_edge$ e
                                               WHERE a.topogeom.tg_id = r.tg_id
                                                     AND a.topogeom.tg_layer_id =
                                                            r.tg_layer_id
                                                     AND r.topo_id = e.edge_id)) )
                             AND e.right_face_id = ff.face_id
                             AND a.r_artli IS NOT NULL
                             AND ff.artli is null
                    GROUP BY a.r_ARTLI, ff.face_id) fff
                    WHERE fff.face_id = f.face_id)
         */

         psql := 'UPDATE ' || facetable || ' f '
              || 'SET '
              || '(f.' || geogs(i) || ') = '
              || '(SELECT fff.r_' || geogs(i) || ' FROM '
              || '(SELECT a.r_' || geogs(i) || ', ff.face_id '
              || 'FROM '
              || newedgetable || ' a, '
              || newtopo || '_relation$ r, '
              || newtopo || '_edge$ e, '
              || facetable || ' ff '
              || 'WHERE '
              || 'a.topogeom.tg_id = r.tg_id AND '
              || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
              || 'r.topo_id = e.edge_id AND '
              || '( '
                   || ' (SDO_GEOM.SDO_LENGTH (e.geometry, .05) > .05) '     --must be an edge of real length
                   || ' OR '                                                --Not a poke in | or poke in V
                   || ' (   e.end_node_id IN (select e.start_node_id from ' --OR define an entire itty bitty sliver on state outline
                                          || newcliptable || ' a, '
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id '
                                          || 'union all '
                                          || 'select e.end_node_id from '
                                          || newcliptable || ' a, '
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id) '
                    || '  AND '
                    || '    e.start_node_id IN (select e.start_node_id from '  --Want to use a WITH clause for this
                                          || newcliptable || ' a, '            --But theres a bug in multicolumn updates
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id '
                                          || 'union all '
                                          || 'select e.end_node_id from '
                                          || newcliptable || ' a, '
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id) '
                     || ' ) ' --end start AND end node
                     || ') '  --end length OR start/end
              || 'AND '
              || 'e.right_face_id = ff.face_id AND '
              || 'a.r_' || geogs(i) || ' IS NOT NULL AND '
              || 'ff.' || geogs(i) || ' IS NULL '
              || 'GROUP BY '
              || 'a.r_' || geogs(i) || ', ff.face_id) fff '
              || 'WHERE fff.face_id = f.face_id ) '
              || 'WHERE EXISTS '          --BLASTED WHERE EXISTS!  Why must YOU exist?
              || '(SELECT fff.r_' || geogs(i) || ' FROM '
              || '(SELECT a.r_' || geogs(i) || ', ff.face_id '
              || 'FROM '
              || newedgetable || ' a, '
              || newtopo || '_relation$ r, '
              || newtopo || '_edge$ e, '
              || facetable || ' ff '
              || 'WHERE '
              || 'a.topogeom.tg_id = r.tg_id AND '
              || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
              || 'r.topo_id = e.edge_id AND '
              || '( '
                   || ' (SDO_GEOM.SDO_LENGTH (e.geometry, .05) > .05) '     --must be an edge of real length
                   || ' OR '                                                --Not a poke in | or poke in V
                   || ' (   e.end_node_id IN (select e.start_node_id from ' --OR define an entire itty bitty sliver on state outline
                                          || newcliptable || ' a, '
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id '
                                          || 'union all '
                                          || 'select e.end_node_id from '
                                          || newcliptable || ' a, '
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id) '
                    || '  AND '
                    || '    e.start_node_id IN (select e.start_node_id from '  --Want to use a WITH clause for this
                                          || newcliptable || ' a, '            --But theres a bug in multicolumn updates
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id '
                                          || 'union all '
                                          || 'select e.end_node_id from '
                                          || newcliptable || ' a, '
                                          || newtopo || '_relation$ r, '
                                          || newtopo || '_edge$ e '
                                          || 'where a.topogeom.tg_id = r.tg_id '
                                          || 'and a.topogeom.tg_layer_id = r.tg_layer_id '
                                          || 'and r.topo_id = e.edge_id) '
                     || ' ) ' --end start AND end node
                     || ') '  --end length OR start/end
              || 'AND '
              || 'e.right_face_id = ff.face_id AND '
              || 'a.r_' || geogs(i) || ' IS NOT NULL AND '
              || 'ff.' || geogs(i) || ' IS NULL '
              || 'GROUP BY '
              || 'a.r_' || geogs(i) || ', ff.face_id) fff '
              || 'WHERE fff.face_id = f.face_id ) ';



         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,'Populate the right for ' || geogs(i),
                                             NULL,NULL,NULL,psql);

         EXECUTE IMMEDIATE psql;
         COMMIT;


      END LOOP;



      --Moved check for expelled values here to final module






      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                          'COMPLETED ' || p_mask,start_time);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_FACE_TABLE: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN retval;

   END POPULATE_FACE_TABLE;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION POPULATE_MEASUREMENTS (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_validate_topo  IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge       IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge      IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa     IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2
   AS

      --Matt! 5/10/10
      --Populate measurements on output face table
      --corresponds to step 13 here
      --   http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions
      --Rewritten for new play on tolerance and geom validation 7/19/10
      --Moved all final checks on null and missing attributes into here 8/31/10 ! (Dont really belong)
      --Added new QA values and awareness of gen_clip_intersect_size and gen_clip_intersect_ratio 11/12/10
      --Add new populate face code for tiny slivers against state boundary 2/5/11
      --1/23/12 - auto topofix
      --6/07/12 - auto edge fix
      --11/09/12 - added isolated node removal and tile validator
      --11/20/12 - fudged with the final null attribute fixer
      --06/07/13 - Added p_topofix_qa parameter

     retval             VARCHAR2(4000) := '0';
     psql               VARCHAR2(4000);
     start_time         TIMESTAMP;
     facetable          VARCHAR2(32);
     newedgetable       VARCHAR2(32);
     newtopo            VARCHAR2(32);
     newcliptable       VARCHAR2(32);
     clip_parms         GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
     measurements       GZ_TYPES.stringarray;
     measurehash        GZ_CLIP.stringhash;
     topotest           VARCHAR2(4000);
     nullgeompsql       VARCHAR2(4000);
     updategeompsql     VARCHAR2(4000);
     badgeompsql        VARCHAR2(4000);
     badfacekount       NUMBER;
     badmbrkount        NUMBER;
     fix_val            VARCHAR2(4000);
     missnulls          GZ_TYPES.stringarray;
     badnulls           GZ_TYPES.stringarray;
     geogs              GZ_TYPES.stringarray;
     current_ids        GZ_TYPES.stringarray;
     kount              NUMBER := 0;
     tg_ids             GZ_TYPES.stringarray;
     tg_layer_ids       GZ_TYPES.stringarray;
     tg_layer_id        NUMBER;
     misskount          PLS_INTEGER;
     missgeogs          GZ_TYPES.stringarray;
     kount2             PLS_INTEGER;
     sliver_ids         GZ_TYPES.stringarray;
     sliver_sides       GZ_TYPES.stringarray;
     sliver_geoid       VARCHAR2(4000);
     sliver_kontinue    PLS_INTEGER;
     topofix_val        VARCHAR2(1);
     edgefix_val        VARCHAR2(1);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 5');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Starting ' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',NULL,'STARTING ' || p_mask);

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      measurements := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                             p_project_id,
                                                             'MEASUREMENT',
                                                             clip_parms.face_output_measurements);


      measurehash := GZ_CLIP.STRINGARRAY2HASH(measurements);

      -- ie SDZ1AL
      newtopo := p_topo_out;
      -- ie SDZ1AL_GEN_FACES
      facetable := clip_parms.face_output_table;
      --ex Z601CL_Z699IN_EWRK
      newedgetable := GZ_CLIP.GET_EDGE_TABLE(p_project_id,p_jobid,clip_parms.edge_input_table);
      newcliptable := p_project_id || p_jobid || '_' || clip_parms.gen_clip_table;



      geogs := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                      p_project_id,
                                                      'ATTRIBUTE',
                                                      clip_parms.left_right_attributes);


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Check tg_ids ' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      --Maybe consider parameterizing some parallelism here

      IF measurehash.EXISTS('SDOGEOMETRY')
      THEN

         IF p_fix_edge = 'Y'
         THEN

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 11');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Call gz_fix_edge' );
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,'Call gz_fix_edge ',
                                               NULL, NULL, NULL, NULL);


            --return char 1 for some still bad, 0 for all good

            edgefix_val := GZ_TOPOFIX.GZ_FIX_EDGE(p_jobid,
                                                  p_topo_out,
                                                  'CLIP', --log type
                                                  'Y', --hold univeral
                                                   clip_parms.gen_clip_tolerance,
                                                   NULL, --no insistence on loop counts. Continue as long as there is progress
                                                   p_fix_2edge); --check pairs of close edges.  Nooo. Very slow

            IF edgefix_val = '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,'GZ_FIX_EDGE success ',
                                                  NULL, NULL, NULL, NULL);

            ELSIF edgefix_val = '1'
            AND p_topofix_qa = 'Y'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                                  'GZ_FIX_EDGE not successful, we will fail the clip job to be safe ');

               retval := retval || '|Failed to fix all edges in gz_fix_edge';

            ELSIF edgefix_val = '1'
            AND p_topofix_qa = 'N'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                                  'GZ_FIX_EDGE not successful, but we wont fail since topofix QA is N ');

            ELSE

                RAISE_APPLICATION_ERROR(-20001,'Unknown edgefix result');

            END IF;

         ELSE

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                               'Not Calling gz_fix_edge ',
                                               NULL, NULL, NULL, NULL);

         END IF;

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 12');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || facetable || ' geometry' );
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------


         --switch to use utility for all of these
         --makes logging harder unfortunately. Need to incorporate shared logger

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,'Calc geometry ',
                                             NULL, NULL, NULL, NULL);


         IF clip_parms.gen_clip_job_srid = clip_parms.gen_clip_output_srid
         THEN

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(facetable,
                                                  'FACE_ID',
                                                  'SDOGEOMETRY',
                                                  'ALL',
                                                  clip_parms.gen_clip_tolerance,
                                                  NULL);

         ELSE

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(facetable,
                                                  'FACE_ID',
                                                  'SDOGEOMETRY',
                                                  'ALL',
                                                  clip_parms.gen_clip_tolerance,
                                                  clip_parms.gen_clip_output_srid);   --transform to new SRID

         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 12');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Check for NULL or bad gtype geometries in ' || facetable);
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         --gz populate measurements will work to get non-null 2003 or 2007 everywhere
         --may work a little too hard, even removes dupes from edge$
         --no response from it on failure though

         --check to make sure that we got a geometry everywhere
         --invalid topo sometimes gives back null
         --or gives back wacky etch-a-sketched 2004s


         nullgeompsql := 'SELECT a.face_id '
                      || 'FROM ' || facetable || ' a '
                      || 'WHERE a.sdogeometry IS NULL '
                      || 'OR (a.sdogeometry.sdo_gtype != :p1 AND a.sdogeometry.sdo_gtype != :p2) ';

         EXECUTE IMMEDIATE nullgeompsql BULK COLLECT INTO badnulls USING 2003,
                                                                         2007;
         IF badnulls.COUNT > 0
         THEN

            --bad gtype or NULL doesnt get into the fixing world below
            --it just handles sdo_geom.validate_Geometry_with_context results

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                              'Initial get_geometry(): We cant get a non-null or correct '
                                           || ' gtype for ' || badnulls.COUNT || ' of our faces ', NULL, NULL, NULL, nullgeompsql);

            retval := retval || '| (Also) We cant get a non-null or correct gtype for ' || badnulls.COUNT || ' of our faces ';

            --update these to QC 2. Auto fixer doesnt handle

            nullgeompsql := 'UPDATE ' || facetable || ' a '
                         || 'SET '
                         || 'a.qc = :p1 '
                         || 'WHERE a.face_id IN (SELECT * FROM TABLE(:p2)) ';

            EXECUTE IMMEDIATE psql USING '2',
                                          GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(badnulls);

         ELSE

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                              'Good: We have non-null correct gtype geometries for all of our faces ',
                                               NULL, NULL, NULL, nullgeompsql);

         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 15');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Index ' || facetable || ' geometry' );
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                             'Creating spatial index ',NULL, NULL, NULL);

         GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(facetable,
                                        'SDOGEOMETRY',
                                        clip_parms.gen_clip_output_srid,
                                        clip_parms.gen_clip_tolerance);


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 90');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Manage NULLS in ' || facetable );
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         --To do the following we need
         --newedgetable still registered in the topology
         --spatial index on face table
         --thats why it is right here

         -------------------------------------------
         --ALL THE REST SHOULD BE ITS OWN QA MODULE or call to a sub
         -------------------------------------------

         --Yes, must do this DOES NOT BELONG HERE THOUGH
         --For now, we will assume that we always have a geoid to work with
         --and if NULL, we have an island face that lost all of its identifying edges somehow

         psql := 'SELECT a.face_id '
              || 'FROM ' || facetable || ' a '
              || 'WHERE a.geoid IS NULL ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO missnulls;

         IF missnulls.COUNT > 0
         THEN

            --First crack, is it entirely contained within a face from the input topo

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                                'We have faces with no attributes, will try to fix ',
                                                NULL,NULL, NULL,NULL,NULL,NULL);


            psql := 'UPDATE ' || facetable || ' a '
                 || 'SET ( ';

            FOR i in 1 .. geogs.COUNT
            LOOP

               IF i != geogs.COUNT
               THEN
                  psql := psql || 'a.' || geogs(i) || ', ';
               ELSE
                  psql := psql || 'a.' || geogs(i) || ') ';
               END IF;

            END LOOP;

            psql := psql || ' = '
                         || '(SELECT  /*+ ORDERED */ ';

            FOR i in 1 .. geogs.COUNT
            LOOP

               IF i != geogs.COUNT
               THEN
                  psql := psql || 'ff.' || geogs(i) || ', ';
               ELSE
                  psql := psql ||  'ff.' || geogs(i) || ' ';
               END IF;

            END LOOP;

            psql := psql || ' FROM '
                 || facetable || ' f, '
                 || clip_parms.face_input_table || '  ff '
                 || 'WHERE f.face_id IN (SELECT * FROM TABLE(:p1)) '
                 || 'AND SDO_RELATE(ff.sdogeometry,f.sdogeometry,:p2) = :p3 '
                 || 'AND a.face_id = f.face_id) '
                 || 'WHERE a.face_id IN (SELECT * FROM TABLE(:p4)) ';

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                                'Fix wholely surrounded island null face attributes with this sql',
                                                NULL,NULL,NULL,psql);


            EXECUTE IMMEDIATE psql USING GZ_CLIP.stringarray_to_varray(missnulls),
                                         'mask=CONTAINS',
                                         'TRUE',
                                         GZ_CLIP.stringarray_to_varray(missnulls);
            COMMIT;

            missnulls.delete;

            --see what we have now

            psql := 'SELECT a.face_id '
                 || 'FROM ' || facetable || ' a '
                 || 'WHERE a.geoid IS NULL ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO missnulls;

         END IF;

         IF missnulls.COUNT > 0
         THEN

            --Previously we tried islands entirely within the state boundary
            --What if we have some that are crossing?  Try them next
            --Ellis Island is an example
            --If all the touching faces have the same combo of attributes, just stick that on the loser face

            --Will get a bomb, single-row subquery returns more than one row if the touching state vals dont match
            --   This is for the best

            psql := 'UPDATE ' || facetable || ' a '
                 || 'SET ( ';

            FOR i in 1 .. geogs.COUNT
            LOOP

               IF i != geogs.COUNT
               THEN
                  psql := psql || 'a.' || geogs(i) || ', ';
               ELSE
                  psql := psql || 'a.' || geogs(i) || ') ';
               END IF;

            END LOOP;

            psql := psql || ' = '
                         || '(SELECT  DISTINCT /*+ ORDERED */ ';  --DISTINCT!!

            FOR i in 1 .. geogs.COUNT
            LOOP

               IF i != geogs.COUNT
               THEN
                  psql := psql || 'ff.' || geogs(i) || ', ';
               ELSE
                  psql := psql ||  'ff.' || geogs(i) || ' ';
               END IF;

            END LOOP;

            psql := psql || ' FROM '
                 || facetable || ' f, '
                 || clip_parms.face_input_table || '  ff '
                 || 'WHERE f.face_id = :p1 '
                 || 'AND SDO_RELATE(ff.sdogeometry,f.sdogeometry,:p2) = :p3 '
                 || 'AND ff.' || clip_parms.face_feature_mask_col || ' = :p4 '
                 || 'AND a.face_id = f.face_id) '
                 || 'WHERE a.face_id = :p5 ';

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,'Fix null face attributes with another sql',
                                                NULL,NULL,NULL,psql);


            FOR i IN 1 .. missnulls.COUNT
            LOOP

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                                  'Attempt to fix partially covered island at state edge null face_id '
                                                  || missnulls(i) || ' attributes with this sql',
                                                   NULL,NULL,NULL,psql);

               BEGIN

                  --Used to have ANYINTERACT here but I dont want TOUCH. Thats too desperate
                  EXECUTE IMMEDIATE psql USING missnulls(i),
                                               'mask=INSIDE+COVEREDBY+CONTAINS+COVERS+OVERLAPBDYINTERSECT+EQUAL+OVERLAPBDYDISJOINT',
                                               'TRUE',
                                               p_mask,
                                               missnulls(i);

               EXCEPTION
                  WHEN OTHERS THEN

                     GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                                  'FAILED to fix partially covered island at state edge null face_id '
                                                  || missnulls(i) || '. The touching edges have different geoids',
                                                   NULL,NULL,NULL,psql);
                     --Will get a bomb, single-row subquery returns more than one row
                     --if the touching bits have non-matching geoids

               END ;

               COMMIT;


            END LOOP;

            missnulls.delete;

            --see what we have now

            psql := 'SELECT a.face_id '
                 || 'FROM ' || facetable || ' a '
                 || 'WHERE a.geoid IS NULL ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO missnulls;

         END IF;


         --Do we still have faces with no attributes?

         IF missnulls.COUNT > 0
         THEN

            --This is practically hard coded, but I have a rare case in Z6 where a tiny null face
            --   borders the state outline, and its interior is 2 edges
            --   It would get updated in the populate_face_table section exception all the
            --   IDifying edges are < .05 (that filter is for good reason) and it isn't a sliver bounded
            --   by a single interior edge
            --   The face is at the intersection of 4 different faces in the input topo, so none of
            --   the above works eiter

            --Let's call this a new "category" (good one)
            --1. Test if null face bounds the state
            --2. Test if it is bounded on the interior by 1+ short edges
            --3. Test if the appropriate side (L or R) of the edges has all the same attributes
            --4. If 1-3 pass muster, update the face with one of the edges


            FOR i IN 1 .. missnulls.COUNT
            LOOP

               --see if face is on state

               psql := 'SELECT count(*) FROM '
                    || newcliptable || ' a, '
                    || newtopo || '_relation$ r, '
                    || newtopo || '_edge$ e '
                    || 'WHERE '
                    || 'a.topogeom.tg_id = r.tg_id AND '
                    || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
                    || 'r.topo_id = e.edge_id AND '
                    || '(e.left_face_id = :p1 OR e.right_face_id = :p2) ';

               EXECUTE IMMEDIATE psql INTO kount2 USING missnulls(i),
                                                        missnulls(i);

               IF kount2 >= 1
               THEN

                  --check if bounded by two+ short interior edges

                  psql := 'SELECT count(e.edge_id) FROM '
                       || newedgetable || ' a, '
                       || newtopo || '_relation$ r, '
                       || newtopo || '_edge$ e '
                       || 'WHERE '
                       || 'a.topogeom.tg_id = r.tg_id AND '
                       || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
                       || 'r.topo_id = e.edge_id AND '
                       || '(e.left_face_id = :p1 or e.right_face_id = :p2) AND '
                       || 'SDO_GEOM.SDO_LENGTH(e.geometry, :p3) <= :p4 ';

                  EXECUTE IMMEDIATE psql INTO kount2 USING missnulls(i),
                                                           missnulls(i),
                                                           clip_parms.gen_clip_tolerance,
                                                           .1; --This needs to be parameterized. Yup
                                                               --Allow this to fudge a little higher than .05
                                                               --FIX edge (called above) tends to make these bigger
                                                               --Than when the original dont update L/R shorties ran

                  IF kount2 >= 2
                  THEN

                     --I dont think the sliver could be bounded by just one feature edge divided into two primitives
                     --   Technically its possible: A non-dangle T intersection <.05 m away from the state outline
                     --   Lets not think about this

                     --Loop through each feature edge
                     --verifying that the appropriate (L/R) geoid from one to the next is the same

                     psql := 'SELECT a.edge_id, ''R_'' side '
                          || 'FROM '
                          || newedgetable || ' a, '
                          || newtopo || '_relation$ r, '
                          || newtopo || '_edge$ e '
                          || 'WHERE '
                          || 'a.topogeom.tg_id = r.tg_id AND '
                          || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
                          || 'r.topo_id = e.edge_id AND '
                          || 'e.right_face_id = :p1 '
                          || 'UNION ALL '
                          || 'SELECT a.edge_id, ''L_'' side '
                          || 'FROM '
                          || newedgetable || ' a, '
                          || newtopo || '_relation$ r, '
                          || newtopo || '_edge$ e '
                          || 'WHERE '
                          || 'a.topogeom.tg_id = r.tg_id AND '
                          || 'a.topogeom.tg_layer_id = r.tg_layer_id AND '
                          || 'r.topo_id = e.edge_id AND '
                          || 'e.left_face_id = :p2 ';

                     EXECUTE IMMEDIATE psql BULK COLLECT INTO sliver_ids,
                                                              sliver_sides USING missnulls(i),
                                                                                 missnulls(i);

                     --looks like
                     --   9150   R_
                     --   11770  L_

                     IF sliver_ids.COUNT >= 2
                     THEN

                        sliver_kontinue := 1;

                        FOR j IN 1 .. (sliver_ids.COUNT - 1)
                        LOOP


                           psql := 'SELECT ' || sliver_sides(j) || 'geoid geoid '
                                || 'FROM '
                                || newedgetable || ' a '
                                || 'WHERE a.edge_id = :p1 '
                                || 'MINUS '
                                || 'SELECT ' || sliver_sides(j+1) || 'geoid geoid '
                                || 'FROM '
                                || newedgetable || ' b '
                                || 'WHERE b.edge_id = :p2 ';

                           BEGIN
                              EXECUTE IMMEDIATE psql INTO sliver_geoid USING sliver_ids(j),
                                                                             sliver_ids(j+1);

                           EXCEPTION
                              WHEN NO_DATA_FOUND
                              THEN
                                 sliver_geoid := NULL;
                              WHEN OTHERS
                              THEN
                                 RAISE;
                           END;

                           IF sliver_geoid IS NULL
                           AND sliver_kontinue = 1
                           THEN

                              sliver_kontinue := 1;

                           ELSE

                              --got a non matching geoid bounding the face
                              sliver_kontinue := 0;

                           END IF;

                        END LOOP;

                        IF sliver_kontinue = 1
                        THEN

                           --I hereby grant you permission to attribute this face
                           --which is the size of this comment at 1:1

                           --stripped down sample sql
                           --update Z651CL_CLIP_FACE a
                           --set (a.aiannhce, a.anrcfp, a.artli, a.countyfp) =
                           --(select e.r_aiannhce, e.r_anrcfp, e.r_artli, e.r_countyfp
                           --from Z651CL_MT651_EWRK e
                           --where e.edge_id = 9150)
                           --where a.face_id = 13333

                           psql := 'UPDATE ' || facetable || ' a '
                                || 'SET ( ';

                           FOR j in 1 .. geogs.COUNT
                           LOOP

                              IF j != geogs.COUNT
                              THEN
                                 psql := psql || 'a.' || geogs(j) || ', ';
                              ELSE
                                 psql := psql || 'a.' || geogs(j) || ') ';
                              END IF;

                           END LOOP;

                           psql := psql || ' = '
                                        || '(SELECT ';

                           --go with the first edge in the list.  Feng shui, of course

                           FOR j in 1 .. geogs.COUNT
                           LOOP

                              IF j != geogs.COUNT
                              THEN
                                 psql := psql || 'e.' || sliver_sides(1) || geogs(j) || ', ';
                              ELSE
                                 psql := psql ||  'e.' || sliver_sides(1) || geogs(j) || ' ';
                              END IF;

                           END LOOP;

                           psql := psql || 'FROM ' || newedgetable || ' e '
                                        || 'WHERE e.edge_id = :p1 ) '
                                        || 'WHERE a.face_id = :p2 ';



                           GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                                             'Fixing NULL attributed face id '
                                                             || missnulls(i) || ' with this query, using feature edge ' || sliver_ids(1),
                                                             NULL,NULL,NULL,psql);

                           --SUCCESS IS HERE
                           EXECUTE IMMEDIATE psql USING sliver_ids(1),
                                                        missnulls(i);

                           COMMIT;

                        ELSE

                              --FAIL
                              GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                                          'Cant fix NULL attributed face id '
                                                          || missnulls(i) || ' because its not bounded by 2 or more feature edges ',
                                                           NULL,NULL,NULL,psql);

                        END IF;

                     ELSE

                        --FAIL
                        GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                                          'Cant fix NULL attributed face id '
                                                          || missnulls(i) || ' because its not bounded by 2 or more feature edges ',
                                                           NULL,NULL,NULL,psql);

                     END IF;



                  ELSE

                     --FAIL
                     GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                                        'Cant fix NULL attributed face id '
                                                        || missnulls(i) || ' because its not bounded by 2 or more short edges ',
                                                        NULL,NULL,NULL,psql);

                  END IF;


               ELSE

                  --FAIL
                  GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',facetable,
                                                     'Cant fix NULL attributed face id '
                                                     || missnulls(i) || ' because its not on the edge of the universe ',
                                                     NULL,NULL,NULL,psql);

               END IF;


            END LOOP;



            --see what we have now after attempting to fix null sliver face(s) on unversal boundary

            psql := 'SELECT a.face_id '
                 || 'FROM ' || facetable || ' a '
                 || 'WHERE a.geoid IS NULL ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO missnulls;

         END IF;


         IF missnulls.COUNT > 0
         THEN

            --nothing in our bag of tricks worked

            retval := retval || ' | We have ' || missnulls.COUNT || ' faces with no attributes ';

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'INVESTIGATE expelled table ' || p_jobid , facetable,
                                                      'This SQL will show faces in ' || facetable || ' that have NULL geoids',
                                                      NULL,NULL,NULL,psql);


            --List these in output table GEN_CLIP_EXPELLED_XXX

            --must prep id first
            FOR k in 1 .. missnulls.COUNT
            LOOP

               kount := kount + 1;
               current_ids(k) := kount + 1;

            END LOOP;

            FORALL ii in 1 .. missnulls.COUNT
               EXECUTE IMMEDIATE 'INSERT INTO ' || p_project_id || p_jobid || '_CLIP_EXPELLED '
                              || ' VALUES (:p1,:p2,:p3,:p4,:p5,NULL)' USING current_ids(ii),
                                                                            p_mask,
                                                                            'NULL FACE ID:',
                                                                            missnulls(ii),
                                                                            psql;
            COMMIT;

            --reset ids
            current_ids.DELETE;

         END IF;

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 14');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: clean up ' || facetable || ' geometry' );
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         --Only thing that should be safe is precision, affects all equally

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                             'Rolling sdogeometry precision back to 8 digits ',
                                              NULL, NULL, NULL);

         --This just makes for a reasonable (ahem hard coded) precision
         --It is polishing of the face table for general usage, not an actual correction
         --or intended to be used in any topology cleanup inputs
         --Must do this first in case it causes something invalid. Fixer will fix it back

         GZ_CLIP.CLEAN_UP_GEOMETRIES(p_schema,
                                     facetable,
                                     2003,
                                     'YES',  --yes (skip dups)
                                     'YES',  --yes (precision)
                                     8);     --8 digits of precision, parameter?


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 16');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Correct topology if necessary' );
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------


         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                            'Deregistering ' || newedgetable || ' and ' || newcliptable,
                                            NULL, NULL, NULL);

         GZ_TOPO_UTIL.DEREGISTER_FEATURE_TABLES(p_schema,
                                                p_topo_out,
                                                'N',            --no drop in case we want to looksee
                                                0,              --0 level
                                                newedgetable);  --just this one table

         GZ_TOPO_UTIL.DEREGISTER_FEATURE_TABLES(p_schema,
                                                p_topo_out,
                                                'N',            --no drop
                                                0,              --0 level
                                                newcliptable);  --just this one table


         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                            'Calling topofix for ' || p_topo_out,
                                            NULL, NULL, NULL);

         --will update QC for all that it can't fix
         --and update sdo for those it does fix
         --Returns '0' for success, '1' for something bad
         --QC 1 is 13349, 13356 or some other validate_Geometry_with_context error
         --QC 2 is NULL sdogeometry
         --QC 3 is non-2003 gtype

         topofix_val := GZ_TOPOFIX.CHECK_FACE_TAB(p_jobid,
                                                  p_topo_out,
                                                  facetable,
                                                  'FACE_ID',
                                                  'CLIP',
                                                  clip_parms.gen_clip_tolerance,
                                                  clip_parms.gen_clip_output_srid);

         /*old
         topofix_val := GZ_TOPOFIX.GZ_FIX_FACE(p_jobid,
                                               p_topo_out,
                                               facetable,
                                               'CLIP',
                                               'Y',
                                               'N',
                                               clip_parms.gen_clip_tolerance,
                                               'QC',
                                               'SDOGEOMETRY',
                                               clip_parms.gen_clip_output_srid);

         */

         IF topofix_val <> '0'
         AND p_topofix_qa = 'Y'
         THEN

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                               'We cant get a valid geometry in ' || facetable || ' for some of our faces',
                                                NULL, NULL, NULL, NULL);

            retval := retval || '| (Also) We cant get a valid geometry in ' || facetable || ' for some of our faces';

         ELSIF topofix_val <> '0'
         AND p_topofix_qa = 'N'
         THEN

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                               'We cant get a valid geometry in ' || facetable || ' for some of our faces '
                                            || 'but wont fail since topofix QA is N',
                                                NULL, NULL, NULL, NULL);

         ELSE

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                               'Good: We have a valid geometry in ' || facetable || ' for all of our faces',
                                               NULL, NULL, NULL, badgeompsql);



         END IF;


      END IF;  --end IF we have sdogeometry in face table


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || facetable || ' MBR and and index it' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF measurehash.EXISTS('MBR')
      AND measurehash.EXISTS('SDOGEOMETRY')
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,'Calc MBR and and index it ',
                                            NULL, NULL, NULL, NULL);

         GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(facetable,
                                               'FACE_ID',
                                               'MBR',
                                               'ALL',
                                                clip_parms.gen_clip_tolerance);

         --No valid geom worries on an MBR?
         --Overabundance of caution
         --1/26/12 - Decided we want to flag these for QC in the face table
         --But this is not fail-worthy. In fact, no one uses the MBR column

         badgeompsql := 'select a.face_id '
                     || 'FROM ' || facetable || ' a '
                     || 'WHERE SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.mbr, :p1) != :p2 ';

         EXECUTE IMMEDIATE badgeompsql BULK COLLECT INTO badnulls USING clip_parms.gen_clip_tolerance,
                                                                        'TRUE';

         IF badnulls.COUNT > 0
         THEN

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                               'Warning: We cant get a valid geometry in ' || facetable || ' for ' || badnulls.COUNT || ' of our MBRs',
                                               NULL, NULL, NULL, badgeompsql);

            --no retval here

            nullgeompsql := 'UPDATE ' || facetable || ' a '
                         || 'SET '
                         || 'a.qc = :p1 '
                         || 'WHERE a.face_id IN (SELECT * FROM TABLE(:p2)) AND '
                         || 'a.qc IS NULL '; --dont want to overwrite something more important

            EXECUTE IMMEDIATE nullgeompsql USING '4',
                                                 GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(badnulls);
            COMMIT;

         ELSE

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                               'Good: All MBRs in ' || facetable || ' are valid ',
                                               NULL, NULL, NULL, badgeompsql);

         END IF;

         GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(facetable,
                                        'MBR',
                                        clip_parms.gen_clip_output_srid,
                                        clip_parms.gen_clip_tolerance,
                                        NULL,
                                        NULL,
                                        facetable || '_MBR_SIDX');

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || facetable || ' AREATOTAL');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF measurehash.EXISTS('AREATOTAL')
      AND measurehash.EXISTS('SDOGEOMETRY')
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,'Calc areatotal ',
                                            NULL, NULL, NULL, NULL);

         GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(facetable,
                                               'FACE_ID',
                                               'AREATOTAL',
                                               'ALL',
                                                clip_parms.gen_clip_tolerance);

         --Sometimes invalid geometries that are very small slivers make areas < 0
         --utility handles this now

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || facetable || ' PERIMETER');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF measurehash.EXISTS('PERIMETER')
      AND measurehash.EXISTS('SDOGEOMETRY')
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,'Calc perimeter ',
                                             NULL, NULL, NULL, NULL);

         -- 'unit=meter' is hard coded into utility
         GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(facetable,
                                               'FACE_ID',
                                               'PERIMETER',
                                               'ALL',
                                                clip_parms.gen_clip_tolerance);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || facetable || ' PA_RATIO');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF measurehash.EXISTS('PA_RATIO')
      AND measurehash.EXISTS('SDOGEOMETRY')
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,'Calc pa_ratio ',
                                             NULL, NULL, NULL, NULL);

         --DECODEs when area is 0 to avoid divide by 0
         GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(facetable,
                                               'FACE_ID',
                                               'PA_RATIO',
                                               'ALL',
                                                clip_parms.gen_clip_tolerance);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || facetable || ' LLX');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF measurehash.EXISTS('MBR')
      AND measurehash.EXISTS('SDOGEOMETRY')
      AND measurehash.EXISTS('LLX')
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,'Calc LLX ',
                                             NULL, NULL, NULL, NULL);

         GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(facetable,
                                               'FACE_ID',
                                               'LLX',
                                               'ALL',
                                                clip_parms.gen_clip_tolerance);

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || facetable || ' LLY');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF measurehash.EXISTS('MBR')
      AND measurehash.EXISTS('SDOGEOMETRY')
      AND measurehash.EXISTS('LLY')
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,'Calc LLY ',
                                             NULL, NULL, NULL, NULL);

         GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(facetable,
                                               'FACE_ID',
                                               'LLY',
                                               'ALL',
                                                clip_parms.gen_clip_tolerance);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 70');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || facetable || ' URX');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF measurehash.EXISTS('MBR')
      AND measurehash.EXISTS('SDOGEOMETRY')
      AND measurehash.EXISTS('URX')
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,'Calc URX ',
                                             NULL, NULL, NULL, NULL);

         GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(facetable,
                                               'FACE_ID',
                                               'URX',
                                               'ALL',
                                                clip_parms.gen_clip_tolerance);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 80');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || facetable || ' URY');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      IF measurehash.EXISTS('MBR')
      AND measurehash.EXISTS('SDOGEOMETRY')
      AND measurehash.EXISTS('URY')
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,'Calc URY ',
                                             NULL, NULL, NULL, NULL);

         GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(facetable,
                                               'FACE_ID',
                                               'URY',
                                               'ALL',
                                                clip_parms.gen_clip_tolerance);

      END IF;




      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 100');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Evaluate expelled values that never made it into ' || facetable );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --However some things, like UAs can be null
      --Lets check that everything from our input edge table is represented somewhere in the out face table

      /*reference
      select count(*) from (
      select distinct countyfp from gen_faces_56
      MINUS
      select distinct countyfp from (
      select l_countyfp countyfp from EDGE_56 e
      union all
      select r_countyfp countyfp from edge_56 e
      )
      UNION all
      select distinct countyfp from (
      select l_countyfp countyfp from EDGE_56 e
      union all
      select r_countyfp countyfp from edge_56 e
      )
      MINUS
      select distinct countyfp from gen_faces_56
      )
      */

      FOR i IN 1 .. geogs.COUNT
      LOOP

         --Note that if executing this 7x is a problem, it could be
         --rewritten into a single SQL
         psql := 'SELECT COUNT(*) FROM ( '
              || 'SELECT DISTINCT ' || geogs(i) || ' FROM ' || facetable || ' '
              || 'MINUS '
              || 'SELECT DISTINCT ' || geogs(i) || ' FROM ( '
              || 'SELECT l_' || geogs(i) || ' ' || geogs(i) || ' FROM ' || newedgetable || ' e '
              || 'UNION ALL '  --union all ok because of distinct
              || 'SELECT r_' || geogs(i) || ' ' || geogs(i) || ' FROM ' || newedgetable || ' e '
              || ') '
              || 'UNION ALL '
              || 'SELECT DISTINCT ' || geogs(i) || ' FROM ( '
              || 'SELECT l_' || geogs(i) || ' ' || geogs(i) || ' FROM ' || newedgetable || ' e '
              || 'UNION ALL '
              || 'SELECT r_' || geogs(i) || ' ' || geogs(i) || ' FROM ' || newedgetable || ' e '
              || ') '
              || 'MINUS '
              || 'SELECT DISTINCT ' || geogs(i) || ' FROM ' || facetable || ' '
              || ') ';

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                             'Check that all ' || geogs(i) || ' in ' || newedgetable || ' are also in ' || facetable,
                                             NULL,NULL,NULL,psql);

         EXECUTE IMMEDIATE psql INTO misskount;

         IF misskount > 0
         THEN

            retval := retval || ' | We have '
                             || misskount || ' attributes that got expelled, investigate ' || p_project_id || p_jobid || '_CLIP_EXPELLED ';

            --We have an error

            --First, who is in face but not in edge (rather unlikely)
            psql := 'SELECT DISTINCT ' || geogs(i) || ' FROM ' || facetable || ' '
                 || 'MINUS '
                 || 'SELECT DISTINCT ' || geogs(i) || ' FROM ( '
                 || 'SELECT l_' || geogs(i) || ' ' || geogs(i) || ' FROM ' || newedgetable || ' e '
                 || 'UNION ALL '
                 || 'SELECT r_' || geogs(i) || ' ' || geogs(i) || ' FROM ' || newedgetable || ' e '
                 || ') ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO missgeogs;

            IF missgeogs.COUNT > 0
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid, p_project_id || p_jobid || 'CLIP_EXPELLED ' , facetable,
                                                   'This SQL will show ' || geogs(i) || ' in ' || facetable || ' that arent in ' || newedgetable,
                                                   NULL,NULL,NULL,psql);

               --List these in output table xxx_CLIP_EXPELLED

               --must prep id first
               FOR k in 1 .. missgeogs.COUNT
               LOOP
                  kount := kount + 1;
                  current_ids(k) := kount + 1;
               END LOOP;

               FORALL ii in 1 .. missgeogs.COUNT

                  EXECUTE IMMEDIATE 'INSERT INTO ' || p_project_id || p_jobid || '_CLIP_EXPELLED '
                                 || ' VALUES (:p1,:p2,:p3,:p4,:p5,NULL)' USING current_ids(ii),
                                                                          p_mask,
                                                                          geogs(i),
                                                                          missgeogs(ii),
                                                                          psql;
                  COMMIT;

               --reset ids
               current_ids.DELETE;


            END IF;


            --Second, who is in edge but never made into face (main concern)
            psql := 'SELECT DISTINCT ' || geogs(i) || ' FROM ('
                 || 'SELECT l_' || geogs(i) || ' ' || geogs(i) || ' FROM ' || newedgetable || ' e '
                 || 'UNION ALL '
                 || 'SELECT r_' || geogs(i) || ' ' || geogs(i) || ' FROM ' || newedgetable || ' e '
                 || ') '
                 || 'MINUS '
                 || 'SELECT DISTINCT ' || geogs(i) || ' FROM ' || facetable || ' ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO missgeogs;

            IF missgeogs.COUNT > 0
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'INVESTIGATE ' || p_project_id || p_jobid || '_CLIP_EXPELLED', facetable,
                                                   'This SQL will show ' || geogs(i) || ' in ' || newedgetable || ' that arent in ' || facetable,
                                                   NULL,NULL,NULL,psql);

               --List these in output table GEN_CLIP_EXPELLED_XXX

               --must prep id first
               FOR k in 1 .. missgeogs.COUNT
               LOOP
                  kount := kount + 1;
                  current_ids(k) := kount + 1;
               END LOOP;

               FORALL ii in 1 .. missgeogs.COUNT

                  EXECUTE IMMEDIATE 'INSERT INTO ' || p_project_id || p_jobid || '_CLIP_EXPELLED '
                                 || ' VALUES (:p1,:p2,:p3,:p4,:p5,NULL)' USING current_ids(ii),
                                                                          p_mask,
                                                                          geogs(i),
                                                                          missgeogs(ii),
                                                                          psql;
                  COMMIT;

               --reset ids
               current_ids.DELETE;

            END IF;


         END IF;


      END LOOP;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 110');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Remove isolated nodes' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',newtopo || '_XXX$',
                                         'Remove isolated nodes');

      GZ_TOPO_UTIL.REMOVE_ISOLATED_NODES_CACHE(newtopo);

      IF GZ_BUILD_SOURCE.ISOLATED_NODES_EXIST(newtopo)
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',newtopo || '_XXX$',
                                            'Uh oh, isolated nodes remain. Im not gonna fail the job (topo validation might), '
                                         || 'but maybe take a peek');

      ELSE

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',newtopo || '_XXX$',
                                            'There are 0 isolated nodes.');


      END IF;

      IF p_validate_topo = 'Y'
      THEN

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 120');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Validate topology' );
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         BEGIN

             GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',newtopo || '_XXX$',
                                                'Check if topo is still valid');

            --return TRUE or raises and error
            --our version handles out of memory errors
            --topotest := GZ_UTILITIES.VALIDATE_TOPOLOGY(newtopo);  --memory management is not too good, all one tile

            --Clip is pre tile kounts, so this is a little funky
            --The state outline table provides a good count for tiles, ~5 for small states, ... ~20 or so for bigger ones
            --But it doesn't work for the tile table itself, since its "hollow" and interior tiles may be dropped
            --Must use the face table, which adds a little processing time

            psql := 'SELECT count(*) FROM ' || newcliptable;
            EXECUTE IMMEDIATE psql INTO kount;

            topotest := GZ_TOPO_UTIL.GZ_VALIDATE_TOPOLOGY(newtopo,
                                                          facetable,          --extent
                                                          'SDOGEOMETRY',
                                                          p_log_type => 'CLIP',
                                                          p_tile_target => kount);

         EXCEPTION
            WHEN OTHERS
            THEN

               --This means we fail the module, but go ahead and create all measurements anyway
               retval := retval || ' | SDO_TOPO_MAP.VALIDATE_TOPOLOGY(''' || newtopo || ''') returns ' || SQLERRM || ' ';

         END;
         
      ELSE
      
         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',newtopo || '_XXX$',
                                                 'Skipping topo validation since validate_topo is ' || p_validate_topo);

      
      END IF;


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',facetable,
                                          'COMPLETED ' || p_mask, start_time);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN retval;

   END POPULATE_MEASUREMENTS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------


   FUNCTION TIDY_EXIT (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_drop_tabs      IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2
   AS

      --Matt! 11/12/10
      --Deregister edge work table from topo
      --maybe more tidying in the future?


      retval             VARCHAR2(4000) := '0';
      clip_parms         GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      newedgetable       VARCHAR2(32);
      start_time         TIMESTAMP;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 5');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('TIDY EXIT: Starting ' );
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      start_time := systimestamp;
      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'TIDY EXIT',NULL,'STARTING ' || p_mask);

      --get input parms
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id, p_jobid);

      --this now has to happen before the auto topofix
      /*
      IF clip_parms.edge_output_table IS NULL
      THEN

         --would be Y if we wanted to keep the thing

         newedgetable := p_project_id || p_jobid || '_' || clip_parms.edge_input_table;

         GZ_TOPO_UTIL.DEREGISTER_FEATURE_TABLES(p_schema,
                                                p_topo_out,
                                                p_drop_tabs,    --usually no drop
                                                0,              --0 level
                                                newedgetable);  --just this one table

      END IF;
      */

      --topo tune
      GZ_TOPO_UTIL.GZ_TOPO_TUNE_UP(p_topo_out);


      GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'TIDY EXIT',newedgetable,
                                         'COMPLETED ' || p_mask, start_time);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('TIDY EXIT: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN retval;

   END TIDY_EXIT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC--MAIN ENTRY POINT---------------------------------------------------------------


   FUNCTION GENERALIZATION_CLIP (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_jobid_in           IN VARCHAR2,
      p_clip_edge_tab      IN VARCHAR2,  --ex DADSGEN.STEDZ6V6
      p_gen_clip_mask      IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_topo_in            IN VARCHAR2 DEFAULT NULL,
      p_drop_module_tab    IN VARCHAR2 DEFAULT 'Y',
      p_transfer_atts      IN VARCHAR2 DEFAULT 'Y',
      p_validate_input     IN VARCHAR2 DEFAULT 'Y',
      p_modules            IN VARCHAR2 DEFAULT NULL,
      p_validate_topo      IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge           IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge          IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa         IN VARCHAR2 DEFAULT 'N'    
   ) RETURN NUMBER
   AS

      --Matt! 3/2/10
      --PLSQL entry point for generalization clipping
      --Steps roughly correspond to (wiki page is getting out of date)
      --http://node101.csvd.census.gov/cpmb-bin/CAMPSwiki.pl?Generalization_-_How_To_Clip_Instructions

      --p_schema          ->The name of the schema where the work will occur
      --p_release         ->Corresponds to gen_clip_parameters
      --p_project_id_in   ->Corresponds to the gen project id in gen_clip_parameters
      --p_jobid_in        ->User defined. Used as p_project_id_in + p_jobid_in to form work table names.  Recommend making this = output topo
      --p_clip_edge_tab   ->The state outline table. Ex DADSGEN.STEDZ6V5
      --p_gen_clip_mask   ->The statefp to use when extracting edges from the state clipper table. When national topo inputs, the statefp to extract from the national topo.
      --p_topo_out        ->Name of output topology after clip
      --p_topo_in         ->Name of the input topology when the input is state-based. Leave NULL for national-based jobs.
      --p_drop_module_tab ->Recreate log tables or append
      --p_transfer_atts   ->Legal values - Y(es), N(o), and (O)nly. Only means only transfer attributes and then stop.
      --p_topofix_qa      ->(Y): Fail if topofix fails. (N): Continue regardless of topofix.  Default behavior is N for clip
      --p_fix_edge        ->Run fix_edge in addition to fix_face.  Not parameterized from the workflow, default behavior is a go
      --p_close_edge      ->In fix edge run, also check for close edges.  This is very slow, so best to not do it in clip with detailed coords

      --Returns
      -- 1: Failure
      -- 2: Success

      --Some sample usages
      --
      --
      --1. Process just one clip, state-based input topo
      --BEGIN
      --retval := GZ_CLIP.GENERALIZATION_CLIP('GZCPB1', 'Z6', '09CL', '09', 'Z609CL', 'Z609',  'Y', 'Y');
      --END;
      --
      --1a. Change some parameters or something from 1 above, rerun with new job and topo to compare
      --BEGIN
      --retval := GZ_CLIP.GENERALIZATION_CLIP('GZCPB1', 'Z6', '09CL2', '09', 'Z609CL2', 'Z609',  'Y', 'Y');
      --END;
      --
      --2. Process just one clip, nation-based, first one we're working on so transfer all attributes
      -- BEGIN
      -- retval := GZ_CLIP.GENERALIZATION_CLIP('GEN_ACS07_GH5', 'SDZ1', '01', '01', 'SDZ101', NULL, 'Y', 'Y');
      -- END;
      --
      --3. Run additional jobs, nation input topo, can usually skip attribute transfer (No change to input EDGE and FACE tables)
      -- BEGIN
      -- retval := GZ_CLIP.GENERALIZATION_CLIP('GEN_ACS07_GH5', 'SDZ1', '02', '02', 'SDZ102', NULL, 'Y', 'N');
      -- END;
      --
      --
      --4. Run from a mid-module save point
      --The N means dont kill the logs from the partial older run
      -- BEGIN
      -- retval := GZ_CLIP.GENERALIZATION_CLIP('GEN_ACS07_GH5', 'SDZ1', '02', '02', 'SDZ102', NULL, 'N', 'N');
      -- END;


      retval                VARCHAR2(8000);
      errm                  VARCHAR2(8000);
      clipmasks             GZ_TYPES.stringarray;
      clipmodules           GZ_TYPES.stringarray;
      currentmodules        GZ_TYPES.stringarray;
      v_schema              VARCHAR2(32);
      p_jobid               VARCHAR2(6);
      p_project_id          VARCHAR2(4) := UPPER(p_project_id_in);
      p_topo                VARCHAR2(32) := UPPER(p_topo_in);
      p_newtopo             VARCHAR2(32) := UPPER(p_topo_out);
      retcode               NUMBER := 0; --default success
      clip_parms            GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
      topofix_qa            VARCHAR2(1);

   BEGIN


      v_schema := UPPER(p_schema);

      IF length(p_jobid_in) > 6
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, job ids can only be 6 characters long');

      ELSE

         p_jobid := UPPER(p_jobid_in);

      END IF;

      IF p_topofix_qa IS NULL
      OR p_topofix_qa = 'N'
      THEN

         topofix_qa := 'N';

      ELSIF p_topofix_qa = 'Y'
      THEN

         topofix_qa := 'Y';

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Unknown topofix_qa value of ' || p_topofix_qa);

      END IF;


      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 5');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Let the logging begin');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      IF p_drop_module_tab = 'Y'
      THEN

         --We are starting from the beginning for this job
         --special exception handling since we rely on the log for, uh, logging

         BEGIN

            retval := GZ_CLIP.START_CLIP_LOGGING(v_schema, p_project_id, p_jobid);

         EXCEPTION
         WHEN OTHERS THEN
            --send special exception to generic handler at the end
            RAISE_APPLICATION_ERROR(-20002,'Failed in START_CLIP_LOGGING.  Check the basics, like schema name');
         END;

         IF retval != '0'
         THEN

            --cant log, there may not be a log
            RAISE_APPLICATION_ERROR(-20002,'Failed before we could even create a log');

         END IF;

      END IF;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('CLIP',p_topo_out,'GENERALIZATION_CLIP',NULL,
                                                   'Inputs are (' || p_schema || ',' || p_release || ','
                                                   || p_project_id_in || ',' || p_jobid_in || ',' || p_clip_edge_tab || ','
                                                   || p_gen_clip_mask || ',' || p_topo_out || ',' || p_topo_in || ','
                                                   || p_drop_module_tab || ',' || p_transfer_atts || ','
                                                   || p_validate_input || ',' || p_modules || ',' 
                                                   || p_validate_topo || ',' || p_fix_edge || ','
                                                   || p_fix_2edge || ',' || p_topofix_qa || ')');



      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 6');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Spin off job parameters');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      retval := GZ_CLIP.SET_UP_JOBRUN(v_schema,
                                      p_release,
                                      p_project_id,
                                      p_jobid,
                                      p_clip_edge_tab,
                                      p_topo);

      IF retval != '0'
      THEN

         --we have a log
         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'SET_UP_JOBRUN',NULL,
                                            'UNRECOVERABLE ERROR: Problem with SET_UP_JOBRUN: ' || retval,
                                            NULL,NULL,NULL,NULL,NULL,substr(retval, 1, 4000) );

         --kick it down to the generic error handler
         RAISE_APPLICATION_ERROR(-20001,'Problem with setting up job: ' || retval);

      END IF;

      --get the results back for a few uses
      clip_parms := GZ_CLIP.GET_CLIP_PARAMETERS(p_release, p_project_id,p_jobid);



      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Verify clip inputs');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      --switched order again.  Want to check these bad boys right away
      --Checks on edge work table will have to wait

      retval := GZ_CLIP.VERIFY_CLIP_INPUTS(v_schema,
                                           p_release,
                                           p_project_id,
                                           p_jobid,
                                           p_clip_edge_tab,
                                           'N'); --no edge work table yet

      IF retval != '0'
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'VERIFY CLIP INPUTS',NULL,
                                            'UNRECOVERABLE ERROR: Problem with verify clip inputs: ' || retval,
                                            NULL,NULL,NULL,NULL,NULL,substr(retval, 1, 4000) );

         --kick it down to the generic error handler
         RAISE_APPLICATION_ERROR(-20001,'Problem with clip inputs: ' || retval);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Transfer attributes to new edge table');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_transfer_atts = 'Y'
      OR p_transfer_atts = 'O'
      THEN

         --We may wish to skip this step
         --Possibly because we took care of it outside of the clipper, as a preprocess

         retval := GZ_CLIP.TRANSFER_ATTRIBUTES(v_schema,
                                               p_release,
                                               p_project_id,
                                               p_jobid,
                                               clip_parms.gen_topology_name);   --this is the input topology
                                                                                --cant be null in transfer_attributes

         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'TRANSFER ATTRIBUTES',NULL,
                                               'UNRECOVERABLE ERROR: Problem with transfer attributes: ' || retval,
                                               NULL,NULL,NULL,NULL,NULL,substr(retval, 1, 4000) );

            --kick it down to the generic error handler
            RAISE_APPLICATION_ERROR(-20001,'Problem with transfer attributes: ' || retval);


         END IF;


      END IF;


      IF p_transfer_atts = 'O'
      THEN

         --only transfer attributes and get out
         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'TRANSFER ATTRIBUTES',NULL,'Exiting generalization_clip after attribute transfer ' ,
                                            NULL,NULL,NULL,NULL,NULL,NULL );
         --this is cheating. LAME!
         GOTO the_end;

      END IF;



      --check again
      --kinda redundant, but we want to get the edge work table checkup too
      --Also the transfer atts step cleans up and adds missing indexes on input tabs.
      --   We want to check for them now

      retval := GZ_CLIP.VERIFY_CLIP_INPUTS(v_schema,
                                           p_release,
                                           p_project_id,
                                           p_jobid,
                                           p_clip_edge_tab,
                                           'Y'); --yeah edge work table

      IF retval != '0'
      THEN

         GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'VERIFY CLIP INPUTS',NULL,
                                            'UNRECOVERABLE ERROR: Problem with verify clip inputs: ' || retval,
                                            NULL,NULL,NULL,NULL,NULL,substr(retval, 1, 4000) );

         --kick it down to the generic error handler
         RAISE_APPLICATION_ERROR(-20001,'Problem with clip inputs: ' || retval);

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 25');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Set up looping on sub nation clip masks ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --still unclear on the order and logic on this next section

      --Get the universe of all clip masks for this job
      --If we have a state based input this is just one statefp
      clipmasks := GZ_CLIP.GET_MASKS(v_schema,
                                     p_release,
                                     p_project_id,
                                     p_jobid,
                                     p_gen_clip_mask);

      --Get the modules
      clipmodules := GZ_CLIP.GET_MODULES(v_schema,
                                         p_release,
                                         p_project_id,
                                         p_jobid,
                                         p_modules);  --pass in to override parm table if we got them


      IF p_drop_module_tab = 'Y'
      THEN

         --unless overridden because we are rerunning ('N' input)
         --create a new modules table to track our progress
         --one record for each mask
         retval := GZ_CLIP.START_CLIP_MODULE_LOGGING(v_schema,
                                                     p_project_id,
                                                     p_jobid,
                                                     clipmasks,
                                                     p_gen_clip_mask);

         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'START_CLIP_MODULE_LOGGING',NULL,
                                               'UNRECOVERABLE ERROR: Problem with clip module logging: ' || retval,
                                                NULL,NULL,NULL,NULL,NULL,substr(retval, 1, 4000) );

            --kick down to generic error handler
            RAISE_APPLICATION_ERROR(-20001,'Problem with START_CLIP_MODULE_LOGGING: ' || retval);

         END IF;


         --This is also our cue to create any output recording tables for this job
         --So far all we have is the expulsion table
         GZ_CLIP.CREATE_GEN_CLIP_EXPELLED(v_schema, p_project_id || p_jobid || '_CLIP_EXPELLED');

      ELSE

         --We are rerunning a partially complete mask
         --We should reset the modules log to N for anything we are about to rerun
         GZ_CLIP.UPDATE_MODULES(v_schema,
                                p_project_id,
                                p_jobid,
                                p_gen_clip_mask,
                                GZ_CLIP.STRINGARRAY_TO_VARCHAR(clipmodules,''));

      END IF;


      --override and just run one clip mask
      IF p_gen_clip_mask IS NOT NULL
      THEN

         --just running one
         --This is SOP now
         clipmasks.DELETE;
         clipmasks(1) := p_gen_clip_mask;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Start looping on sub nation clip masks ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --this loop is just some vestigial testing code
      --really just one loop, the current state

      FOR i in 1 .. clipmasks.COUNT
      LOOP


         --switch to temporary modules variable
         --we may need to override them on this loop for error handling purposes
         --then on next clip mask loop the module sun rises again
         currentmodules := clipmodules;


         IF currentmodules(1) = 'Y'
         THEN

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 35');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Create empty topology for ' || clipmasks(i));
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            BEGIN

               retval := GZ_CLIP.CREATE_EMPTY_TOPOLOGY(v_schema,
                                                       p_release,
                                                       p_project_id,
                                                       p_jobid,
                                                       clipmasks(i),
                                                       p_topo_out,
                                                       p_topo,           --this is the parm input to the fn.  May be NULL
                                                       p_validate_input);

            EXCEPTION
               WHEN OTHERS
               THEN
                  IF SQLCODE = -01013
                  THEN
                     --ORA-01013: user requested cancel of current operation
                     RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
                  ELSE
                     errm := SQLERRM;
                     retval := DBMS_UTILITY.format_error_backtrace;
                  END IF;
            END;


            IF retval != '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_EMPTY_TOPOLOGY',NULL,'UNRECOVERABLE ERROR: Ending processing for ' || clipmasks(i),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

               currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);

               retcode := 1;

            ELSE

               GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '1');

            END IF;

         END IF;


         IF currentmodules(2) = 'Y'
         THEN

          ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Add clipping mask for ' || clipmasks(i));
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            BEGIN


               retval := GZ_CLIP.ADD_CLIP_MASK(v_schema,
                                               p_release,
                                               p_project_id,
                                               p_jobid,
                                               clipmasks(i),
                                               p_clip_edge_tab,
                                               p_topo_out,
                                               p_topo);


            EXCEPTION
               WHEN OTHERS
               THEN
                  IF SQLCODE = -01013
                  THEN
                     --ORA-01013: user requested cancel of current operation
                     RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
                  ELSE
                     errm := SQLERRM;
                     retval := DBMS_UTILITY.format_error_backtrace;
                  END IF;
            END;


            IF retval != '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_CLIP_MASK',NULL,'UNRECOVERABLE ERROR: Ending processing for ' || clipmasks(i),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

               currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);

               retcode := 1;

            ELSE

               GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '2');

            END IF;

         END IF;



         IF currentmodules(3) = 'Y'
         THEN

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Add interior lines for ' || clipmasks(i));
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            BEGIN

               retval := GZ_CLIP.ADD_INTERIOR_LINES(v_schema,
                                                    p_release,
                                                    p_project_id,
                                                    p_jobid,
                                                    clipmasks(i),
                                                    p_topo_out,
                                                    p_topo);

            EXCEPTION
               WHEN OTHERS
               THEN
                  IF SQLCODE = -01013
                  THEN
                     --ORA-01013: user requested cancel of current operation
                     RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');
                  ELSE
                     errm := SQLERRM;
                     retval := DBMS_UTILITY.format_error_backtrace;
                  END IF;
            END;


            IF retval != '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ADD_INTERIOR_LINES',NULL,'UNRECOVERABLE ERROR: Ending processing for ' || clipmasks(i),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

               currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);

               retcode := 1;

            ELSE

               GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '3');

            END IF;




         END IF;



         IF currentmodules(4) = 'Y'
         THEN

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: ID Dangles for ' || clipmasks(i));
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            BEGIN

               retval := GZ_CLIP.ID_DANGLES(v_schema,
                                            p_release,
                                            p_project_id,
                                            p_jobid,
                                            clipmasks(i),
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
                     retval := DBMS_UTILITY.format_error_backtrace;
                  END IF;
            END;


            IF retval != '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'ID_DANGLES',NULL,'UNRECOVERABLE ERROR: Ending processing for ' || clipmasks(i),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

               currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);

               retcode := 1;

            ELSE

               GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '4');

            END IF;

         END IF;


         IF currentmodules(5) = 'Y'
         THEN

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 70');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Delete exterior dangles for ' || clipmasks(i));
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            BEGIN

               retval := GZ_CLIP.DELETE_EXTERIOR_DANGLES(v_schema,
                                                         p_release,
                                                         p_project_id,
                                                         p_jobid,
                                                         clipmasks(i),
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
                     retval := DBMS_UTILITY.format_error_backtrace;
                  END IF;
            END;


            IF retval != '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_EXTERIOR_DANGLES',NULL,'UNRECOVERABLE ERROR: Ending processing for ' || clipmasks(i),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

               currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);

               retcode := 1;

            ELSE

               GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '5');

            END IF;

         END IF;


         IF currentmodules(6) = 'Y'
         THEN

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 80');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Delete dangling faces for ' || clipmasks(i));
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            BEGIN

               retval := GZ_CLIP.DELETE_DANGLING_FACES(v_schema,
                                                       p_release,
                                                       p_project_id,
                                                       p_jobid,
                                                       clipmasks(i),
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
                     retval := DBMS_UTILITY.format_error_backtrace;
                  END IF;
            END;


            IF retval != '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'DELETE_DANGLING_FACES',NULL,'UNRECOVERABLE ERROR: Ending processing for ' || clipmasks(i),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

               currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);

               retcode := 1;

            ELSE

               GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '6');

            END IF;

         END IF;


         IF currentmodules(7) = 'Y'
         THEN

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 90');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Extend dangling edges for ' || clipmasks(i));
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            BEGIN

               retval := GZ_CLIP.EXTEND_INTERIOR_DANGLES(v_schema,
                                                         p_release,
                                                         p_project_id,
                                                         p_jobid,
                                                         clipmasks(i),
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
                     retval := DBMS_UTILITY.format_error_backtrace;
                  END IF;
            END;


            IF retval != '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'EXTEND_INTERIOR_DANGLES',NULL,'UNRECOVERABLE ERROR: Ending processing for ' || clipmasks(i),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

               currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);

               retcode := 1;

            ELSE

               GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '7');

            END IF;

         END IF;


         IF currentmodules(8) = 'Y'
         THEN

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 100');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Create new face table for '|| clipmasks(i));
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            BEGIN

               retval := GZ_CLIP.CREATE_NEW_FACE_TABLE(v_schema,
                                                       p_release,
                                                       p_project_id,
                                                       p_jobid,
                                                       clipmasks(i),
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
                     retval := DBMS_UTILITY.format_error_backtrace;
                  END IF;
            END;


            IF retval != '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'CREATE_NEW_FACE_TABLE',NULL,'UNRECOVERABLE ERROR: Ending processing for ' || clipmasks(i),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

               currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);

               retcode := 1;

            ELSE

               GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '8');

            END IF;

         END IF;


         IF currentmodules(9) = 'Y'
         THEN

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 110');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Populate new face table for '|| clipmasks(i));
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            BEGIN

               retval := GZ_CLIP.POPULATE_FACE_TABLE(v_schema,
                                                     p_release,
                                                     p_project_id,
                                                     p_jobid,
                                                     clipmasks(i),
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
                     retval := DBMS_UTILITY.format_error_backtrace;
                  END IF;
            END;


            IF retval != '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_FACE_TABLE',NULL,'UNRECOVERABLE ERROR: Ending processing for ' || clipmasks(i),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

               currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);

               retcode := 1;

            ELSE

               GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '9');

            END IF;

         END IF;


         IF currentmodules(10) = 'Y'
         THEN

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 120');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Populate measurements for '|| clipmasks(i));
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            BEGIN

               retval := GZ_CLIP.POPULATE_MEASUREMENTS(v_schema,
                                                       p_release,
                                                       p_project_id,
                                                       p_jobid,
                                                       clipmasks(i),
                                                       p_topo_out,
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
                     retval := DBMS_UTILITY.format_error_backtrace;
                  END IF;
            END;


            IF retval != '0'
            THEN


               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',NULL,'UNRECOVERABLE ERROR: Ending processing for ' || clipmasks(i),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

               currentmodules := GZ_CLIP.ZAP_MODULES(currentmodules);

               retcode := 1;

            ELSE

               GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '10');

            END IF;

         END IF;


         --Second module 10!!!!!
         --only executes if module 10 succeeded completely. Otherwise zapped to N
         IF currentmodules(10) = 'Y'
         THEN

            --Module 10 = Y means we want to be done with this job run
            --Use this to key any cleanup
            --So far just deregister edge work table from topo

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 130');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Generalization_Clip: Clean up for '|| clipmasks(i));
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            BEGIN

               retval := GZ_CLIP.TIDY_EXIT(v_schema,
                                           p_release,
                                           p_project_id,
                                           p_jobid,
                                           clipmasks(i),
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
                     retval := DBMS_UTILITY.format_error_backtrace;
                  END IF;
            END;


            IF retval != '0'
            THEN


               GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'POPULATE_MEASUREMENTS',NULL,'UNRECOVERABLE ERROR: Ending processing for ' || clipmasks(i),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

               retcode := 1;

            ELSE

               NULL;

               --No module update for fake module 10
               --GZ_CLIP.UPDATE_MODULES(v_schema, p_project_id, p_jobid, clipmasks(i), '10');

            END IF;

         END IF;




      END LOOP; --End loop over clip masks


      <<the_end>>


      ---------------------------------------------------------------
      --WE SHOULD PROBABLY RETURN SOMETHING TO A CALLER IN PRODUCTION
      --cool, as of late 2011 we now have a caller

      RETURN retcode;
      --1: Failure
      --0: Success
      ---------------------------------------------------------------
      ---------------------------------------------------------------


   EXCEPTION
      WHEN OTHERS
      THEN


         IF SQLCODE = -20002
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Doh! Unhandled exception.  We dont even have a log to write to. Check basic inputs like schema name: '
                                 || SQLERRM);

         ELSE

            -- Word up to "good practice"

            errm := SQLERRM || DBMS_UTILITY.format_error_backtrace;
            GZ_BUSINESS_UTILS.GEN_CLIP_TRACKING_LOG(p_project_id || p_jobid,'GENERALIZATION_CLIP',NULL,
                                               'UNRECOVERABLE ERROR: GENERALIZATION_CLIP caught this exception and has no clue ',
                                               NULL,NULL,NULL,NULL,NULL,substr(errm || ' ' || retval , 1, 4000) );

            --RAISE_APPLICATION_ERROR(-20001,'GENERALIZATION_CLIP caught this exception but has no clue: ' || errm);

            RETURN 1;


         END IF;




   END GENERALIZATION_CLIP;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION NEW_GEN_CLIP_PARAMETERS RETURN GZ_TYPES.GEN_CLIP_PARAMETERS PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_CLIP_PARAMETERS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION NEW_GEN_CLIP_MODULES_LOG RETURN GZ_TYPES.GEN_CLIP_MODULES PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_CLIP_MODULES_LOG;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION NEW_GEN_CLIP_EXPELLED RETURN GZ_TYPES.GEN_CLIP_EXPELLED PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_CLIP_EXPELLED;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION NEW_GEN_CLIP_JOBRUNS RETURN GZ_TYPES.GEN_CLIP_JOBRUNS PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_CLIP_JOBRUNS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GEN_CLIP_PARAMETERS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_CLIP_PARAMETERS'
   )
   AS

      --Matt! 3/2/10
      --Creates empty clip parameters table
      --Clip parameters table is a child of the SCHEMA. Theres just one per schema

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
                   || 'SELECT * FROM TABLE(' || v_schema || '.GZ_CLIP.NEW_GEN_CLIP_PARAMETERS ) ';

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
              || '      PRIMARY KEY(RELEASE, GEN_PROJECT_ID) '
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




   END CREATE_GEN_CLIP_PARAMETERS;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   PROCEDURE CREATE_GEN_CLIP_TRACKING_LOG (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2 DEFAULT 'CLIP_TRACKING'
   )
   AS

      --Matt! 5/01/10
      --Creates empty clip logging  table
      --Clip tracking table is a child of the job.  There may be several in a schema
      --6/3/11 switched to use shared GEN_EXTENDED_TRACKING_LOG type and creator

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);



   END CREATE_GEN_CLIP_TRACKING_LOG;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GEN_CLIP_MODULE_LOG (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_CLIP_MODULES_XXX'
   )
   AS

      --Matt! 5/01/10
      --Creates empty clip MODULE logging  table
      --Clip module logging table is a child of the job.  There may be several in a schema

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
                   || 'SELECT * FROM TABLE(' || p_schema || '.GZ_CLIP.NEW_GEN_CLIP_MODULES_LOG ) ';

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);


   END CREATE_GEN_CLIP_MODULE_LOG;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GEN_CLIP_JOBRUNS (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_table_name     IN VARCHAR2 DEFAULT 'CLIP_JOBRUNS'
   )
   AS

      --Matt! 5/01/10
      --Creates empty CLIP_JOBRUNS table
      --Clip jobruns is a child of a project

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
                   || 'SELECT * FROM TABLE(' || p_schema || '.GZ_CLIP.NEW_GEN_CLIP_JOBRUNS ) ';

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
              || '      PRIMARY KEY(GEN_JOB_ID, GEN_PROJECT_ID) '
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
                      || 'END;';

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Grant privileges on ' || p_table_name);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);



   END CREATE_GEN_CLIP_JOBRUNS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE CREATE_GEN_CLIP_EXPELLED (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_CLIP_EXPELLED_XXX'
   )
   AS

      --Matt! 5/13/10
      --Creates empty clip expulsion
      --Clip expulsion table is a child of the job.  There may be several in a schema

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
                   || 'SELECT * FROM TABLE(' || p_schema || '.GZ_CLIP.NEW_GEN_CLIP_EXPELLED ) ';

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);



   END CREATE_GEN_CLIP_EXPELLED;


   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------

   PROCEDURE CREATE_EDGE_ATTRIBUTE (
      p_schema             IN VARCHAR2,
      p_tab_name           IN VARCHAR2,
      p_type               IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 11/04/10
      --2/13/13 moved from utilities to clip since it was never shared.  Makes logging easier

      --Creates edge attribute table stub with no values

      --SAMPLE
      --BEGIN
      --GZ_CLIP.CREATE_EDGE_ATTRIBUTE('GZCPB1','Z109_EDGET');
      --END;

      legal_users           GZ_TYPES.stringhash;
      v_hashindex           VARCHAR2(4000);

   BEGIN


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EDGE_ATTRIBUTE table ' || p_tab_name);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_type != 'SIMPLIFY'
      OR p_type IS NULL
      THEN

         GZ_BUSINESS_UTILS.CREATE_TABLE('EDGE_ATTRIBUTE',p_tab_name);

      ELSE

         --simplify version has fixed type, but more cols
         GZ_BUSINESS_UTILS.CREATE_TABLE('EDGE_ATTRIBUTE_' || p_type, p_tab_name);

      END IF;


      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_tab_name || ' '
                     || 'ADD ('
                     || '   CONSTRAINT ' || p_tab_name || '_PKC '
                     || '      PRIMARY KEY(EDGE_ID) '
                     || ')';


      --This grant business could maybe go in CREATE_TABLE

      legal_users := GZ_BUSINESS_UTILS.GET_REF_SCHEMAS(p_schema);

      --legal_users looks like
      --(GZDEC10ST99) SELECT
      --(GZCPB_1)     SELECT, INSERT, UPDATE, DELETE, INDEX

      v_hashindex := legal_users.FIRST;

      LOOP
         EXIT WHEN NOT legal_users.EXISTS(v_hashindex);

         IF UPPER(v_hashindex) != UPPER(p_schema)
         THEN

            EXECUTE IMMEDIATE 'GRANT ' || legal_users(v_hashindex) || ' ON ' || p_tab_name || ' TO ' || v_hashindex || ' WITH GRANT OPTION ';

         END IF;

         v_hashindex := legal_users.NEXT(v_hashindex);

      END LOOP;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EDGE_ATTRIBUTE Peace Out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_EDGE_ATTRIBUTE;

   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private---------------------------------------------------------------------------------

   PROCEDURE CLIP_EDGE_ATTRIBUTE_TABLE (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_tab_name           IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_zref_table         IN VARCHAR2,
      p_clip_mask          IN VARCHAR2,
      p_face_tab           IN VARCHAR2 DEFAULT NULL,
      p_log_tag            IN VARCHAR2
   )
   AS

      --Matt!  11/04/10
      --! 10/03/11 added p_project_id for new reference_face_fields structure
      --2/13/13 moved from utilities to clip since it was never shared.  Makes logging easier
      --10/17/13 added column length parameterization

      --This takes the pre-created basic edge attribute table and adds
      --   clip specific (no one else needs these I dont think) left and right geog fields
      --   Then populates them

      psql                 VARCHAR2(4000);
      fields               GZ_TYPES.stringarray;
      leftyrighty          GZ_TYPES.stringarray;
      leftyrighty_lengths  GZ_TYPES.stringarray;
      temp_length          GZ_TYPES.stringarray;
      kount                PLS_INTEGER := 0;
      kount2               PLS_INTEGER;
      face_input_table     VARCHAR2(4000);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Starting CLIP_EDGE_ATTRIBUTE_TABLE: ' || p_tab_name);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_face_tab IS NULL
      THEN

         face_input_table := UPPER(p_topo) || '_FACE';

      ELSE

         face_input_table := UPPER(p_face_tab);

      END IF;


      --return includes geoid
      fields := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                            p_project_id,
                                                            'ATTRIBUTE',
                                                            p_zref_table);

      --we will use variable "fields" when referring to DML
      --we will use variable "leftyrighty" when referring to DDL. It contains the additional clip col
      --DDduh

      --simplify by adding the Ls and Rs up front
      FOR i in 1 .. fields.COUNT
      LOOP
      
         IF fields(i) <> 'GEOID'
         THEN
         
            --get the desired column length, just returns 1 element
            temp_length := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                                       p_project_id,
                                                                       'ATTRIBUTE',
                                                                       p_zref_table,
                                                                       'FIELD_LENGTH',
                                                                       fields(i));
          
         ELSE
         
            --geoid
            temp_length(1) := '4000';                             
                                                      
         END IF;

         kount := kount + 1;
         leftyrighty(kount) := 'L_' || fields(i);
         leftyrighty_lengths(kount) := temp_length(1);

         kount := kount + 1;
         leftyrighty(kount) := 'R_' || fields(i);
         leftyrighty_lengths(kount) := temp_length(1);

         --Add the special mask column too
         IF i = fields.COUNT
         THEN

            --get the desired column length, just returns 1 element
            temp_length := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                                       p_project_id,
                                                                       'ATTRIBUTE',
                                                                       p_zref_table,
                                                                       'FIELD_LENGTH',
                                                                       p_clip_mask);  --STATE usually
                                                                   
            kount := kount + 1;
            leftyrighty(kount) := p_clip_mask;
            leftyrighty_lengths(kount) := temp_length(1);

         END IF;

      END LOOP;


      --In initial clip code we dropped columns here, but this table should always be fresh!

      --For each dealie in reference table add L_DEALIE and R_DEALIE

      psql := 'ALTER TABLE ' || p_tab_name || ' '
           || 'ADD (';

      FOR i in 1 .. leftyrighty.COUNT
      LOOP

         IF i != leftyrighty.COUNT
         THEN

            psql := psql || leftyrighty(i) || ' VARCHAR2(' || leftyrighty_lengths(i) || '), ';

         ELSE

            psql := psql || leftyrighty(i) || ' VARCHAR2(' || leftyrighty_lengths(i) || ') )';

         END IF;

      END LOOP;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('CLIP',p_log_tag,'CLIP_EDGE_ATTRIBUTE_TABLE',NULL,
                                             'Adding l/r columns to ' || p_tab_name,NULL,NULL,NULL,psql);

      --error handling?
      EXECUTE IMMEDIATE psql;


      --In oldskool clip code we added index on edge_id here
      --But we now wouldn't be in here without going through the caller that guarantees it.  FRESH

      --What about face id though?
      --Commented code to create face index on face_input_table 2/13/13
      --Should be good in alternative topo build, and also verify_clip_inputs checks for this

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CLIP_EDGE_ATTRIBUTE_TABLE: ' || p_tab_name || ' Execute the L/R Update');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      /* For reference
      update edge e
      set
      (e.l_cdfp, e.l_countyfp, e.l_divisionce, e.l_nationfp, e.l_regionce, e.l_statefp, e.l_uace) =
      (select f.cdfp, f.countyfp, f.divisionce, f.nationfp, f.regionce, f.statefp, f.uace
      from
      face f,
      mt_edge$ t
      where t.left_face_id = f.face_id
      and t.edge_id = e.edge_id),
      (e.r_cdfp, e.r_countyfp, e.r_divisionce, e.r_nationfp, e.r_regionce, e.r_statefp, e.r_uace) =
      (select f.cdfp, f.countyfp, f.divisionce, f.nationfp, f.regionce, f.statefp, f.uace
      from
      face f,
      mt_edge$ t
      where t.right_face_id = f.face_id
      and t.edge_id = e.edge_id)
      */

      psql := 'UPDATE ' || p_tab_name || ' e '
           || 'SET (';

      FOR i in 1 .. fields.COUNT
      LOOP

         IF i != fields.COUNT
         THEN
            psql := psql || 'e.l_' || fields(i) || ', ';
         ELSE
            psql := psql || 'e.l_' || fields(i) || ') = ';
         END IF;

      END LOOP;

      psql := psql || '(SELECT ';

      FOR i in 1 .. fields.COUNT
      LOOP

         IF i != fields.COUNT
         THEN
            psql := psql || 'f.' || fields(i) || ', ';
         ELSE
            psql := psql || 'f.' || fields(i) || ' ';
         END IF;

      END LOOP;

      psql := psql || 'FROM '
                   || face_input_table || ' f, '
                   || p_topo  || '_edge$ t '
                   || 'WHERE '
                   || 't.left_face_id = f.face_id AND '
                   || 't.edge_id = e.edge_id), '  --end left update subquery
                   || '(';

      FOR i in 1 .. fields.COUNT
      LOOP

         IF i != fields.COUNT
         THEN
            psql := psql || 'e.r_' || fields(i) || ', ';
         ELSE
            psql := psql || 'e.r_' || fields(i) || ') = ';
         END IF;

      END LOOP;

      psql := psql || '(SELECT ';

      FOR i in 1 .. fields.COUNT
      LOOP

         IF i != fields.COUNT
         THEN
            psql := psql || 'f.' || fields(i) || ', ';
         ELSE
            psql := psql || 'f.' || fields(i) || ' ';
         END IF;

      END LOOP;

      psql := psql || 'FROM '
                   || face_input_table || ' f, '
                   || p_topo  || '_edge$ t '
                   || 'WHERE '
                   || 't.right_face_id = f.face_id AND '
                   || 't.edge_id = e.edge_id) ';  --end right update subquery

      --dbms_output.put_line(psql);


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('CLIP',p_log_tag,'CLIP_EDGE_ATTRIBUTE_TABLE',NULL,
                                             'Populating l/r columns on ' || p_tab_name,NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql;
      COMMIT;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CLIP_EDGE_ATTRIBUTE_TABLE: ' || p_tab_name || ' Set clip mask flag');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------
      /*reference
      update edge e
      set e.statefp =
      CASE
      when e.l_statefp = e.r_statefp THEN e.l_statefp
      when e.l_statefp != e.r_statefp THEN '00'
      END */

      psql := 'UPDATE ' || p_tab_name || ' e '
           || 'SET e.' || p_clip_mask || ' = '
           || 'CASE '
           || 'WHEN e.l_' || p_clip_mask || ' = e.r_' || p_clip_mask || ' '
           || 'THEN e.l_' || p_clip_mask || ' '
           || 'WHEN e.l_' || p_clip_mask || ' != e.r_' || p_clip_mask || ' '
           || 'THEN :p1 '
           || 'END ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('CLIP',p_log_tag,'CLIP_EDGE_ATTRIBUTE_TABLE',NULL,
                                             'Setting clip mask flag on ' || p_tab_name,NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql USING '00';

      COMMIT;

      --RIGHT HERE we are adding a fake clip mask, 00
      --Need to be sure we dont accidentally attempt to process it later

       ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CLIP_EDGE_ATTRIBUTE_TABLE: ' || p_tab_name || ' Verify results');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Check that we dont have any junk in the input topology
      --each edge should now either
      --NULL Statefp: Edge of the universe
      --00 Statefp: State border in the middle of the country (national inputs)
      --XX (populated) statefp: All other non state boundaries
      --should have been caught in verify_clip_inputs
      --but what do I know
      psql := 'SELECT count(*) '
           || 'FROM ' || p_tab_name || ' e '
           || 'WHERE '
           || 'l_' || p_clip_mask || ' IS NULL AND '
           || 'r_' || p_clip_mask || ' IS NULL ';

      EXECUTE IMMEDIATE psql INTO kount2;

      IF kount2 != 0 THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('CLIP',p_log_tag,'CLIP_EDGE_ATTRIBUTE_TABLE',NULL,
                                                'Oops, ' || p_tab_name || ' has records with no left or right ' || p_clip_mask,
                                                NULL,NULL,NULL,psql);

         RAISE_APPLICATION_ERROR(-20001, 'Oops, ' || p_tab_name || ' has records with no left or right ' || p_clip_mask);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CLIP_EDGE_ATTRIBUTE_TABLE: ' || p_tab_name || ' Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CLIP_EDGE_ATTRIBUTE_TABLE;


   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------


    PROCEDURE BUILD_EDGE_ATTRIBUTE_TABLE (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_tab_name           IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_type               IN VARCHAR2 DEFAULT NULL, --or CLIP or SIMPLIFY
      p_zref_table         IN VARCHAR2 DEFAULT NULL, --ex 'REFERENCE_FACE_FIELDS_Z6'
      p_special_use        IN VARCHAR2 DEFAULT NULL, --clip mask
      p_calc_atts          IN VARCHAR2 DEFAULT 'Y',
      p_tolerance          IN VARCHAR2 DEFAULT .05,
      p_face_tab           IN VARCHAR2 DEFAULT NULL,
      p_log_tag            IN VARCHAR2                --topo
   )
   AS

      --Matt! 11/4/10
      --! 10/03/11 Added p_project_id input for new reference_face_fields structure
      --2/13/13 moved from utilities to clip since it was never shared.  Makes logging easier too


      --Wrapper for create_edge_attribute
      --CREATE sets up empty table shell based on short type
      --BUILD here populates it
      -- Then, optionally, BUILD adds clip or simplification specific fields

      --SAMPLE, standard edge att
      --BEGIN
      --GZ_CLIP.BUILD_EDGE_ATTRIBUTE_TABLE('GZCPB1','Z109_EDGET','Z109');
      --END;

      --Sample, Matts clipping edge attribute table
      --BEGIN
      --GZ_CLIP.BUILD_EDGE_ATTRIBUTE_TABLE('GZCPB1','Z109_EDGET','Z109','CLIP','REFERENCE_FACE_FIELDS_Z6', 'STATEFP');
      --END;



      psql                 VARCHAR2(4000);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_EDGE_ATTRIBUTE_TABLE: ' || p_tab_name);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('CLIP',p_log_tag,'BUILD_EDGE_ATTRIBUTE_TABLE',NULL,
                                             'Calling create_edge_attribute',NULL,NULL,NULL,NULL);

      --Make it
      GZ_CLIP.CREATE_EDGE_ATTRIBUTE(p_schema,p_tab_name,p_type);

      IF p_calc_atts = 'Y'
      AND (p_type != 'SIMPLIFY' OR p_type IS NULL)
      THEN

         --not too dynamic, will need to update when type changes. Boo
         --standard stub edge attribute table.
         --Or clip version temporarily slumming as stub

         --I dont use this any more, length is fairly costly and unused

         psql := 'INSERT /*+ APPEND */ INTO '
             ||  p_tab_name || ' '
             || 'SELECT '
             || 'e.edge_id, '
             || 'SDO_GEOM.SDO_LENGTH(e.geometry,:p1), '  --ROUND parm?
             || 'CAST(NULL AS VARCHAR2(100)), '
             || 'e.left_face_id, '
             || 'e.right_face_id '
             || 'FROM ' || p_topo || '_edge$ e ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('CLIP',p_log_tag,'BUILD_EDGE_ATTRIBUTE_TABLE',NULL,
                                                'Inserting edges and l/r faces',NULL,NULL,NULL,psql);

         EXECUTE IMMEDIATE psql USING p_tolerance;
         COMMIT;

         --expand for clip  --whats this mean?


      ELSIF p_calc_atts != 'Y'
      AND (p_type != 'SIMPLIFY' or p_type IS NULL)
      THEN

        --same two types, just no attributes
        --THIS IS SOP

        psql := 'INSERT /*+ APPEND */ INTO '
             ||  p_tab_name || ' '
             || 'SELECT '
             || 'e.edge_id, '
             || 'CAST(NULL AS NUMBER), '
             || 'CAST(NULL AS VARCHAR2(100)), '
             || 'e.left_face_id, '
             || 'e.right_face_id '
             || 'FROM ' || p_topo || '_edge$ e ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('CLIP',p_log_tag,'BUILD_EDGE_ATTRIBUTE_TABLE',NULL,
                                                'Inserting edges and l/r faces',NULL,NULL,NULL,psql);

         EXECUTE IMMEDIATE psql;
         COMMIT;


      END IF;



      IF UPPER(p_type) = 'CLIP'
      THEN

         --clip type is expanded and expanded cols populated
         --with columns defined in a REFERENCE parameter table
         GZ_CLIP.CLIP_EDGE_ATTRIBUTE_TABLE(p_schema,
                                           p_release,
                                           p_project_id,
                                           p_tab_name,
                                           p_topo,
                                           p_zref_table,
                                           p_special_use,
                                           p_face_tab,
                                           p_log_tag);

      ELSIF UPPER(p_type) = 'SIMPLIFY'
      THEN

         --Sidey type is based on an expanded fixed type. Created empty above
         --but we've got a bunch more cols to calculate
         --Sidey never actually used this

         RAISE_APPLICATION_ERROR(-20001, 'Yo, 2010 wants its code back');

      ELSIF p_type IS NOT NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'What the heck is a ' || p_type || ' edge attribute table?');

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_EDGE_ATTRIBUTE_TABLE:  ' || p_tab_name || ' Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END BUILD_EDGE_ATTRIBUTE_TABLE;

   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------

   FUNCTION GET_XYS(
   Geom IN MDSYS.SDO_GEOMETRY,
   ring1_to_find PLS_INTEGER,
   edge1_to_find PLS_INTEGER,
   ring2_to_find PLS_INTEGER DEFAULT NULL,
   edge2_to_find PLS_INTEGER DEFAULT NULL
) RETURN sdo_geometry
   AS

  --Just a helper, not called in production

   /**
   ---
   -- Program Name: GetXys
   -- Author: Sidey + Matt!
   -- Creation Date:  June? 2009
   -- Updated: Feb 5/2010 to return a range of segments and preserve SRID: Sidey
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

   PROCEDURE MAKE_ARROWHEAD(
      x1 number default 0.,
      y1 number default 0.,
      x2 number default 1.,
      y2 number default 1.,
      delta number default 1.
   )
   AS

     --From Sidey, y4 doesnt seem to be correct
     --Just a helper, not called in production

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

   BEGIN

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


   END MAKE_ARROWHEAD;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION MAKE_ARROWHEAD(
      p_geom            IN SDO_GEOMETRY,
      p_scalefactor     IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY
   AS

     --Sidey!! and Matt! 3/5/10
     --Just a helper, not called in production
     --use like:
     --select GZ_CLIP.make_arrowhead(a.geom) from
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


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

    FUNCTION SCALE2PERCENT(
      p_geom            IN SDO_GEOMETRY,
      p_scalefactor     IN NUMBER
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 3/08/10
      --Just a helper, not called in production
      --scalefactor is a whole number percent.  Want 1/2 size, enter 50
      --Ex select GZ_CLIP.SCALE2PERCENT(a.sdogeometry,10) from face a
      --   where a.face_id = 2592

      output         SDO_GEOMETRY;
      ordinates      MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
      geom_bits      SDO_GEOMETRY;
      minx           NUMBER;
      miny           NUMBER;
      maxx           NUMBER;
      maxy           NUMBER;
      centerx        NUMBER;
      centery        NUMBER;
      newx           NUMBER;
      newy           NUMBER;
      scalefact      NUMBER;

   BEGIN

      IF p_scalefactor <= 0
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Scale factor must be greater than zero');
      END IF;

      scalefact := p_scalefactor / 100;


      ordinates := p_geom.sdo_ordinates;

      FOR i in 1 .. SDO_UTIL.getnumelem(p_geom)
      LOOP

         geom_bits := SDO_UTIL.extract(p_geom, i);
         ordinates := geom_bits.sdo_ordinates;

         minx := SDO_GEOM.SDO_MIN_MBR_ORDINATE(geom_bits,1);
         miny := SDO_GEOM.SDO_MIN_MBR_ORDINATE(geom_bits,2);
         maxx := SDO_GEOM.SDO_MAX_MBR_ORDINATE(geom_bits,1);
         maxy := SDO_GEOM.SDO_MAX_MBR_ORDINATE(geom_bits,2);

         centerx := minx + ((maxx - minx) / 2);
         centery := miny + ((maxy - miny) / 2);

         FOR j in 1 .. ordinates.COUNT
         LOOP

            IF mod(j,2) = 1
            THEN
               ordinates(j) := centerx + ( (ordinates(j) - centerx) * scalefact);
            ELSE
               ordinates(j) := centery + ( (ordinates(j) - centery) * scalefact);
            END IF;

         END LOOP;

         geom_bits.sdo_ordinates := ordinates;

         IF i = 1
         THEN

            output := geom_bits;

         ELSE

            output := SDO_UTIL.APPEND(output, geom_bits);

         END IF;

      END LOOP;

      --just in case
      output := SDO_UTIL.REMOVE_DUPLICATE_VERTICES(output,.05);

      RETURN output;

   END SCALE2PERCENT;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION FIRSTSEG (
      p_geom            IN SDO_GEOMETRY,
      p_last            IN VARCHAR2 DEFAULT NULL
   ) RETURN sdo_geometry
   AS

   --Matt! 3/5/10
   --Just a helper, not called in production
   --Dumb Dumb method to see the direction of a linear geom
   --Plot my edge
   --Then call this to see the first part of it.  Thats the start dummy!
   --And heres something else dumb: pass in a second argument and FIRSTseg gives
   --   a LAST segment!

   --ex select GZ_CLIP.firstseg(a.geom) from newfeaturetype a
   --   where a.l_state = '39' and st99_hi_edges_ = 294

   --Sometimes make_arrowhead is hard to use, or I dont believe it
   --Use this to confirm

      ordinates  MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
      mysegno    PLS_INTEGER;
      myseg      SDO_GEOMETRY;


  BEGIN

     IF p_geom.sdo_gtype != '2002'
     THEN
        RAISE_APPLICATION_ERROR(-20001,'Sorry, gtype is ' || p_geom.sdo_gtype);
     END IF;


     IF p_last IS NULL
     THEN
        myseg     := GZ_CLIP.get_xys(p_geom,1,1);
     ELSE
        ordinates := p_geom.SDO_ORDINATES;
        myseg := GZ_CLIP.get_xys(p_geom, 1, ( (ordinates.COUNT/2) - 1));
     END IF;


     RETURN myseg;

   END FIRSTSEG;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------


   FUNCTION FIND_COORD_INDEX (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY
   ) RETURN PLS_INTEGER DETERMINISTIC
   AS

      --Matt! 3/19/10
      --pass in a point and an edge
      --return the "coord_index" for the segment on the edge that is closest to the point
      --where "coord_index" matches the oracle concept
      --"if the edge coordinates are (2,2, 5,2, 8,3) the index of the second vertex (5,2) is 1"

      --MODEL! (and I use the term loosely)
      --We have a point that is near a roughly continous part of the edge
      --   Note that we are hosed if the point is near an L turn, come back to this later
      --Two possibilities as we walk past on our edge shapepoints (reading L to R)
      --   1.  Node is closer to start, relative to closest shapepoint
      --   2.  Node is closer to end, relative to closest shapepoint
      --   Vizually:
      --   1.  0--0-------i-------0--0
      --                X
      --
      --   2.  0--0-------0-------i--0
      --                    X
      --
      --   1. After i-1, on current i the previous low dist becomes runnerup, current becomes low, no more activity
      --      i-1 is index
      --   2. On current i we get a new runnerup, but no new low
      --      i-1 is index
      --
      --This is a total crock, though, since we could have:
      --
      --   3.  0--0-----------------i--0--0
      --                             x
      --We'll just use the returned index as a starting point for the caller
      --
      --consider more research for Matt here:
      --http://www.mcdonalds.com/usa/work/search.html

      pointx            NUMBER;
      pointy            NUMBER;
      x                 NUMBER;
      y                 NUMBER;
      curr_distance     NUMBER;
      low_distance      NUMBER := 40000000; --earth circumference
      runnerup_distance NUMBER := 40000000;
      output            PLS_INTEGER;


   BEGIN

      IF p_edge.sdo_gtype != 2002
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Buddy, edge gtype is ' || p_edge.sdo_gtype || ' !?');

      END IF;

      --Return if just one segment
      IF p_edge.sdo_ordinates.count = 4
      THEN

         RETURN 0;  --oracle coord_index

      END IF;

      --one time
      pointx := p_point.sdo_point.x;
      pointy := p_point.sdo_point.y;


      FOR i in 1 .. (p_edge.sdo_ordinates.count/2)
      LOOP


         --i, corresponding to x1,y1 = index of 0
         --we will subtract 1 from i when we are done

         x := p_edge.sdo_ordinates( i*2 - 1 );
         y := p_edge.sdo_ordinates (i*2 );


         --use the fast Sidey constants. Go HP15C calculator go!
         curr_distance := GZ_CLIP.APPROX_GC_DISTANCE(pointx,
                                                         pointy,
                                                         x,
                                                         y);


         IF i = 1
         THEN

            --initialize
            low_distance := curr_distance;
            output := i;

         ELSIF curr_distance < low_distance
         THEN


            --case 1 or 2
            runnerup_distance := low_distance;
            low_distance := curr_distance;
            output := i - 1;

         ELSIF ( (curr_distance > low_distance) AND (curr_distance < runnerup_distance) )
         THEN


            --we've just passed our break point
            --Will never hit this on case 1
            runnerup_distance := curr_distance;
            output := i-1;

         END IF;

         --I think we have to check all of the ordinates
         --We could get a closest on the far side of a big U for example, before
         --coming across the true closest

      END LOOP;


      RETURN (output-1);


   END FIND_COORD_INDEX;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------


   FUNCTION ACCURATE_FIND_COORD_INDEX (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY,
      p_debug          IN PLS_INTEGER DEFAULT 0  --This guy is gonna try our patience
   ) RETURN PLS_INTEGER DETERMINISTIC
   AS

      --Matt! 09/03/10
      -- This is the one.  This is the one.  This is the one.

      pt_measure        NUMBER;
      test_point        SDO_GEOMETRY;
      test_measure      NUMBER;


   BEGIN


      IF p_edge.sdo_gtype != 2002
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Buddy, edge gtype is ' || p_edge.sdo_gtype || ' !?');

      END IF;

      --Return if just one segment
      IF p_edge.sdo_ordinates.count = 4
      THEN

         RETURN 0;  --oracle coord_index

      END IF;

      --No SRID to mimic topology voodoo?
      --Now done in caller
      --p_edge.sdo_srid := NULL;
      --p_point.sdo_srid := NULL;


      pt_measure := GZ_CLIP.GZ_FIND_MEASURE(p_point,p_edge);

      --we are probably screwed, but we will give the caller a guess
      IF pt_measure >= 1000
      THEN

         IF p_debug = 1
         THEN
            dbms_output.put_line('off the line, measure is ' || pt_measure);
         END IF;

         RETURN ( SDO_UTIL.GETNUMVERTICES(p_edge) - 2 );

      ELSIF pt_measure <= 0
      THEN

         IF p_debug = 1
         THEN
            dbms_output.put_line('off the line, measure is ' || pt_measure);
         END IF;

         RETURN 0;

      END IF;


      IF p_debug = 1
      THEN
         dbms_output.put_line('pt_measure is ' || pt_measure);
      END IF;


      FOR i IN 1 .. SDO_UTIL.GETNUMVERTICES(p_edge)
      LOOP

         IF i = 1
         THEN

            test_measure := 0;

         ELSE

            test_point := SDO_GEOMETRY(2001,
                                       p_edge.sdo_srid,  --Should be NULL
                                       SDO_POINT_TYPE(p_edge.sdo_ordinates( (i*2) - 1 ),
                                                      p_edge.sdo_ordinates( (i*2) ),
                                                      NULL),
                                       NULL,
                                       NULL);

            test_measure := GZ_CLIP.GZ_FIND_MEASURE(test_point,p_edge);

         END IF;


         IF p_debug = 1
         THEN
            dbms_output.put_line('test_measure is ' || test_measure);
         END IF;


         IF test_measure > pt_measure
         THEN

            --we just passed our point
            RETURN (i-2);

         END IF;


      END LOOP;


   END ACCURATE_FIND_COORD_INDEX;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------


   FUNCTION ACCURATE_FIND_COORD_INDEX_OLD (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY,
      p_debug          IN PLS_INTEGER DEFAULT 0  --This guy is gonna try our patience
   ) RETURN PLS_INTEGER
   AS

      --Matt! and Rachel!! 4/27/10
      --Rewrote original find_coord_index based on advice from Rachel

      --The plan:
      --   Use sidey's fast great circle distance to find the closest shape point
      --   to our dangling point.
      --
      --    0--seg1---0---seg2---0
      --                x
      --
      --   Take the 2 segments on either side as candidates
      --   Use shortest distance equation to find which line is closest
      --      Check if calc'd point is on line.  If yes, done, thats the close segment
      --      If not  (ex)
      --
      --      \
      --      \
      --       0------
      --
      --        x  (point is closest to imaginary extension of segment, ie line's equation)
      --
      --      Probably we should calc the second point and see if its on the second line
      --      But I'm just gonna roll with the second one for now

      --Need some error handling if close point is the start or end node

      --Sample debug wrapper:
      /*
      DECLARE
         nodegeom sdo_geometry;
         edgegeom sdo_geometry;
         psql VARCHAR2(4000);
         coordindex pls_integer;
      begin
         psql := 'select geometry from statefp09_node$ where node_id = 147';
         execute immediate psql INTO nodegeom;
         psql := 'select geometry from statefp09_edge$ where edge_id = 1593';
         execute immediate psql INTO edgegeom;
         coordindex := GZ_CLIP.ACCURATE_FIND_COORD_INDEX(nodegeom,edgegeom,1);
      end;
      */




      pointx            NUMBER;
      pointy            NUMBER;
      x                 NUMBER;
      y                 NUMBER;
      curr_distance     NUMBER;
      low_distance      NUMBER;
      position          PLS_INTEGER;
      x1                NUMBER;
      y1                NUMBER;
      x2                NUMBER;
      y2                NUMBER;
      x3                NUMBER;
      y3                NUMBER;
      m1                NUMBER;
      m2                NUMBER;
      dist1             NUMBER;
      dist2             NUMBER;
      b1                NUMBER;
      b2                NUMBER;
      xon               NUMBER;
      yon               NUMBER;
      seg_geom          SDO_GEOMETRY;
      on_point          SDO_GEOMETRY;
      debug_pt          SDO_GEOMETRY;


   BEGIN


      IF p_edge.sdo_gtype != 2002
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Buddy, edge gtype is ' || p_edge.sdo_gtype || ' !?');

      END IF;

      --Return if just one segment
      --not sure about this. What if point is still off of the edge?
      IF p_edge.sdo_ordinates.count = 4
      THEN

         RETURN 0;  --oracle coord_index

      END IF;

      --one time, put point xy in variables
      pointx := p_point.sdo_point.x;
      pointy := p_point.sdo_point.y;

      IF p_debug = 1
      THEN

         dbms_output.put_line('point x is ' || pointx);
         dbms_output.put_line('point y is ' || pointy);

      END IF;


      FOR i in 1 .. (p_edge.sdo_ordinates.count/2)
      LOOP

         --find the closest shape point
         --the closest segment will be attached to it somehow

         x := p_edge.sdo_ordinates( i*2 - 1 );
         y := p_edge.sdo_ordinates (i*2 );


         IF p_edge.sdo_srid = 8265
         THEN

            --use the fast Sidey constants. Go HP15C calculator go!
            curr_distance := GZ_CLIP.APPROX_GC_DISTANCE(pointx,
                                                            pointy,
                                                            x,
                                                            y);
         ELSE

            --construct a point with the current x,y
            --and use sdo distance
            curr_distance := SDO_GEOM.SDO_DISTANCE(p_point,
                                                   SDO_GEOMETRY(2001,
                                                                p_edge.sdo_srid,
                                                                SDO_POINT_TYPE(x, y, NULL),
                                                                NULL,
                                                                NULL),
                                                   .05);  --should this tolerance be lower (or parameterized) ?


         END IF;

         IF p_debug = 1
         THEN

            dbms_output.put_line('i is ' || i);
            dbms_output.put_line('distance is ' || curr_distance);

         END IF;

         IF i = 1
         THEN
            --initialize
            low_distance := curr_distance;
            position := i;

         ELSIF curr_distance < low_distance
         THEN

            low_distance := curr_distance;
            position := i;

            IF p_debug = 1
            THEN

               dbms_output.put_line('new low, distance is ' || low_distance);
               dbms_output.put_line('new low, x is ' || x);
               dbms_output.put_line('new low, y is ' || y);

            END IF;

         END IF;

      END LOOP;

      IF p_debug = 1
      THEN

         dbms_output.put_line('low position was ' || position);
         dbms_output.put_line('heres the edge dude');
         --dbms_output.put_line(TO_CHAR(MATTOOL.DUMP_SDO(p_edge)));
         dbms_output.put_line('and heres the shapepoint we picked');
         debug_pt := SDO_GEOMETRY(2001,
                                  p_edge.sdo_srid,
                                  SDO_POINT_TYPE(p_edge.sdo_ordinates(position*2 - 1), p_edge.sdo_ordinates(position*2), NULL),
                                  NULL,
                                  NULL);

         --dbms_output.put_line(TO_CHAR(MATTOOL.DUMP_SDO(debug_pt)));

      END IF;

      IF position = (p_edge.sdo_ordinates.count/2)
      THEN

         --closest point was the end of the edge, no need to test further
         RETURN (position - 2);

      ELSIF position = 1
      THEN

         --closest point was at start of edge, no further
         RETURN 0;

      END IF;


      --We now have two segments, x1,y1--seg1--x2y2--seg2--x3y3
      x1 := p_edge.sdo_ordinates(position*2 - 3 );
      y1 := p_edge.sdo_ordinates(position*2 - 2 );
      x2 := p_edge.sdo_ordinates(position*2 - 1 );
      y2 := p_edge.sdo_ordinates(position*2 );
      x3 := p_edge.sdo_ordinates(position*2 + 1 );
      y3 := p_edge.sdo_ordinates(position*2 + 2 );

      --seg1 distance
      m1 := GZ_CLIP.slope_calculator(x1,y1,x2,y2);
      b1 := (y1 - (m1*x1));

      IF p_debug = 1
      THEN

         dbms_output.put_line('x1 ' || x1);
         dbms_output.put_line('y1 ' || y1);
         dbms_output.put_line('x2 ' || x2);
         dbms_output.put_line('y2 ' || y2);
         dbms_output.put_line('x3 ' || x3);
         dbms_output.put_line('y3 ' || y3);

         dbms_output.put_line('slope1 is ' || m1);
         dbms_output.put_line('intercept is ' || b1);

      END IF;

      dist1 := (abs(pointy - (m1 * pointx) - b1)) / (sqrt( (m1*m1) + 1));


      IF p_debug = 1
      THEN

         dbms_output.put_line('dist1 is ' || dist1);
         dbms_output.put_line('numerator is ' || (abs(y1 - (m1 * x1) - b1)));
         dbms_output.put_line('m1 * x1 is ' || (m1 * x1) );
         dbms_output.put_line('y1 - that is ' || (y1 - (m1 * x1)));
         dbms_output.put_line('denominator is ' || (sqrt( (m1*m1) + 1)));

      END IF;


      --seg2 distance
      --m2 := (y2-y3)/(x2-x3);
      m2 := GZ_CLIP.slope_calculator(x2,y2,x3,y3);
      b2 := (y2 - (m2*x2));
      dist2 := (abs(pointy - (m2 * pointx) - b2)) / (sqrt( (m2*m2) + 1));

      IF p_debug = 1
      THEN

         dbms_output.put_line('slope 2 is ' || m2);
         dbms_output.put_line('dist2 is ' || dist2);

      END IF;

      IF (m1 > 1000 OR m2 > 1000 OR m1 < -1000 OR m2 < -1000)
      THEN
         --Sidey is falling on his sword
         --One of the slopes is approaching vertical, we have no logic to handle this
         --Return a negative number to indicate to caller that we need to work this out
         --using oracle SDO piece by piece

         RETURN -1;

      END IF;

      IF dist1 < dist2
      THEN

         --calc point on seg1
         xon := ( (m1 * pointy) + pointx - (m1 * b1) ) / ( (m1*m1) + 1 );
         yon := ( ( (m1*m1) * pointy) + (m1*pointx) + b1 ) / ( (m1*m1) + 1 );

         --find out if its on the physical segment
         --lets not use an SRID since we just calc'd in cartesian
         --and we want to use a tolerance wider than 8265 .05
         seg_geom := SDO_GEOMETRY(
                                 2002,
                                 NULL,
                                 NULL,
                                 SDO_ELEM_INFO_ARRAY(1,2,1),
                                 SDO_ORDINATE_ARRAY(x1,y1, x2,y2)
                                 );

         on_point := SDO_GEOMETRY(
                                 2001,
                                 NULL,
                                 SDO_POINT_TYPE(xon, yon, NULL),
                                 NULL,
                                 NULL
                                 );

         IF p_debug = 1
         THEN
            NULL;
            --dbms_output.put_line('on point yall ' || TO_CHAR(MATTOOL.dump_sdo(on_point)));
            --dbms_output.put_line('seg geom ' || TO_CHAR(MATTOOL.dump_sdo(seg_geom)));
         END IF;

         --First check if the new point is actually right on top of our middle vertex
         --IF SDO_GEOM.RELATE(on_point,'mask=EQUAL',SDO_GEOMETRY(2001,
                                                               --NULL,
                                                               --SDO_POINT_TYPE(x2, y2, NULL),
                                                               --NULL,
                                                               --NULL
                                                               --), 0.5) = 'EQUAL'
         --THEN

            --NULL;

         --END IF;



         IF SDO_GEOM.RELATE(seg_geom,'mask=CONTAINS+TOUCH',on_point,0.5) != 'FALSE' --permissive tolerance
         THEN

            --The relationship between a point and the line on which it sits should be TOUCH
            --A point is only boundary.  Line and point then share only a boundary, no interior, = TOUCH
            --But I swear I saw CONTAINS in my initial testing, gonna leave it till proven otherwise

            --This (seg 1) is the one

            IF p_debug = 1
            THEN

               dbms_output.put_line('swell, the point is where we wanted it');

            END IF;

            RETURN (position - 2);

         ELSE

            IF p_debug = 1
            THEN

               dbms_output.put_line('oops, we calcd a point on the imaginary lines equation.  Go with 2nd segment');

            END IF;

            --Eh try the other one
            RETURN (position - 1);

         END IF;


      ELSIF dist1 > dist2
      THEN

         --calc point on seg2
         xon := ( (m2 * pointy) + pointx - (m2 * b2) ) / ( (m2*m2) + 1 );
         yon := ( ( (m2*m2) * pointy) + (m2*pointx) + b2 ) / ( (m2*m2) + 1 );

         --find out if its on the physical segment
         --lets not use an SRID since we just calc'd in cartesian
         --and we want to use a tolerance wider than 8265 .05
         seg_geom := SDO_GEOMETRY(
                                 2002,
                                 NULL,
                                 NULL,
                                 SDO_ELEM_INFO_ARRAY(1,2,1),
                                 SDO_ORDINATE_ARRAY(x2,y2, x3,y3)
                                 );

         on_point := SDO_GEOMETRY(
                                 2001,
                                 NULL,
                                 SDO_POINT_TYPE(xon, yon, NULL),
                                 NULL,
                                 NULL
                                 );

         IF p_debug = 1
         THEN
            NULL;
            --dbms_output.put_line('on point yall ' || TO_CHAR(MATTOOL.dump_sdo(on_point)));
            --dbms_output.put_line('seg geom ' || TO_CHAR(MATTOOL.dump_sdo(seg_geom)));
         END IF;

         IF SDO_GEOM.RELATE(seg_geom,'mask=CONTAINS+TOUCH',on_point,0.5) != 'FALSE'
         THEN
            --The relationship between a point and the line on which it sits should be TOUCH
            --A point is only boundary.  Line and point then share only a boundary, no interior, = TOUCH
            --But I swear I saw CONTAINS in my initial testing, gonna leave it till proven otherwise

            IF p_debug = 1
            THEN

               dbms_output.put_line('swell, the point is where we wanted it');

            END IF;

            --This (seg 2) is the one
            RETURN (position - 1);

         ELSE

            IF p_debug = 1
            THEN

               dbms_output.put_line('oops, we calcd a point on the imaginary lines equation.  Go with 2nd segment');

            END IF;

            --Eh try the other one
            RETURN (position - 2);

         END IF;

      ELSIF dist1 = dist2
      THEN

            IF p_debug = 1
            THEN

               dbms_output.put_line('V shape?');

            END IF;

         --V shape ?
         --pick either
         RETURN (position - 2);


      ELSE

         RAISE_APPLICATION_ERROR(-20001,'nopossible');

      END IF;


   END ACCURATE_FIND_COORD_INDEX_OLD;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public -------------------------------------------------------------------------------

   FUNCTION GZ_SEGMENT_RANGE (
      p_seg_geom             IN SDO_GEOMETRY,
      p_start                IN NUMBER,
      p_end                  IN NUMBER
   ) RETURN SDO_GEOMETRY
   AS

      --Matt!  11/2/11
      --For a segment (must be only 2 vertices) chop out a range of it
      --ex first_half_geom := GZ_SEGMENT_RANGE(full_geom, 0, 500)

      output            SDO_GEOMETRY;
      ordinates         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
      start_point       SDO_GEOMETRY;
      end_point         SDO_GEOMETRY;

   BEGIN

      --check for bad inputs
      --this could potentially be rewritten to allow more complicated lines
      IF SDO_UTIL.GETNUMVERTICES(p_seg_geom) != 2
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Must be a single segment, has ' || SDO_UTIL.GETNUMVERTICES(p_seg_geom) || ' vertices');

      ELSIF p_start >= 1000 OR p_start < 0
      OR p_end > 1000 OR p_end <= 0
      OR p_end <= p_start
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Bad range, must be 0-1000');

      END IF;

      output := p_seg_geom;

      start_point := GZ_LOCATE_PT(p_seg_geom, p_start);
      end_point   := GZ_LOCATE_PT(p_seg_geom, p_end);

      ordinates.EXTEND(4);

      ordinates(1) := start_point.sdo_point.X;
      ordinates(2) := start_point.sdo_point.Y;
      ordinates(3) := end_point.sdo_point.X;
      ordinates(4) := end_point.sdo_point.Y;

      output.sdo_ordinates := ordinates;

      RETURN output;


   END GZ_SEGMENT_RANGE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public --------------------------------------------------------------------------------

   FUNCTION GZ_ADD_MIDPOINT (
      p_geometry        IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 10/28/11
      --adds a midpoint vertex to any standard line or poly geometry
      --   (only tested for lines)
      --totally oblivious to tolerance

      midpoint          SDO_GEOMETRY;
      output            SDO_GEOMETRY;
      temppt            SDO_GEOMETRY;
      ordinates         MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
      shark             PLS_INTEGER := 0;

   BEGIN

      midpoint := GZ_LOCATE_PT(p_geometry, 500);

      output := p_geometry;

      --silly varrays, why are you so old-timey?
      ordinates.EXTEND(p_geometry.sdo_ordinates.COUNT + 2);


      --we will put each ordinate in this temporary point
      --to test each ordinates measure
      temppt := SDO_GEOMETRY(2001,
                             p_geometry.sdo_srid,
                             SDO_POINT_TYPE(1, 1, NULL), --dummy values
                             NULL,
                             NULL);

      FOR i IN 1 .. p_geometry.sdo_ordinates.COUNT/2
      LOOP


         temppt.sdo_point.X := p_geometry.sdo_ordinates( (i*2) - 1);
         temppt.sdo_point.Y := p_geometry.sdo_ordinates( (i*2) );

         IF shark = 0
         THEN

            --working our way toward the middle

            IF GZ_FIND_MEASURE(temppt, p_geometry) < 500
            THEN

               --right into the output
               ordinates((i*2) - 1) := p_geometry.sdo_ordinates( (i*2) - 1);
               ordinates((i*2))     := p_geometry.sdo_ordinates( (i*2) );

            ELSE

               --jumped the shark
               shark := 1;

               --insert the middle guy
               ordinates((i*2) - 1) := midpoint.sdo_point.X;
               ordinates((i*2))     := midpoint.sdo_point.Y;

               --continue with regularly scheduled programming
               ordinates((i*2) + 1) := p_geometry.sdo_ordinates( (i*2) - 1);
               ordinates((i*2) + 2) := p_geometry.sdo_ordinates( (i*2) );

            END IF;

         ELSE

            --continue, counter of new geom is one vertex ahead of input
            ordinates((i*2) + 1) := p_geometry.sdo_ordinates( (i*2) - 1);
            ordinates((i*2) + 2) := p_geometry.sdo_ordinates( (i*2) );

         END IF;


      END LOOP;


      output.sdo_ordinates := ordinates;

      RETURN output;


   END GZ_ADD_MIDPOINT;

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

   FUNCTION GZ_FIND_MEASURE (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY
   ) RETURN NUMBER DETERMINISTIC
   AS

      --Matt! 8/23/10

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
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 8/28/10

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


   FUNCTION GET_SEGMENT_FROM_INDEX (
      p_edge           IN SDO_GEOMETRY,
      p_index          IN PLS_INTEGER
   ) RETURN GZ_TYPES.stringarray DETERMINISTIC
   AS

      --Matt! 3/19/10
      --Perpendicular code needs to know the x1,y1  x2,y2 of the segment we have chosen
      --We have the "index" so return the coords
      --This is a little redundant since the index code knows the x1y1s, but who is gonna know?

      output      GZ_TYPES.stringarray;


   BEGIN

      output(1) := p_edge.sdo_ordinates( (2*p_index) + 1);
      output(2) := p_edge.sdo_ordinates( (2*p_index) + 2);
      output(3) := p_edge.sdo_ordinates( (2*p_index) + 3);
      output(4) := p_edge.sdo_ordinates( (2*p_index) + 4);

      RETURN output;

   END GET_SEGMENT_FROM_INDEX;




   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

    FUNCTION GET_SEGMENT_GEOM_FROM_INDEX (
      p_edge           IN SDO_GEOMETRY,
      p_index          IN PLS_INTEGER
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 8/23/10

      output            SDO_GEOMETRY;

   BEGIN


             output := SDO_GEOMETRY(2002,
                             p_edge.sdo_srid, --Should be null
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

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION MAKE_GEOM_FROM_SIDEY_SEGMENT (
      p_coords          IN GZ_TYPES.stringarray,
      p_srid            IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 4/8/10
      --This is just a helper for debugging, not actually called

      output      SDO_GEOMETRY;
      srid        NUMBER;

   BEGIN

      IF p_coords.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Yo, thats not a sidey segment!');

      END IF;

      IF p_srid IS NULL
      THEN

         srid := 8265;

      ELSE

         srid := p_srid;

      END IF;


      output := SDO_GEOMETRY(2002,
                             srid,
                             NULL,
                             SDO_ELEM_INFO_ARRAY(1, 2, 1),
                             SDO_ORDINATE_ARRAY(p_coords(1),
                                                p_coords(2),
                                                p_coords(3),
                                                p_coords(4)
                                                )
                             );

       RETURN output;

   END MAKE_GEOM_FROM_SIDEY_SEGMENT;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

  FUNCTION MAKE_GEOM_FROM_SIDEY_SEGMENT (
      p_x1              IN NUMBER,
      p_y1              IN NUMBER,
      p_x2              IN NUMBER,
      p_y2              IN NUMBER,
      p_srid            IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 4/8/10
      --This is just a helper for debugging, not actually called

      output      SDO_GEOMETRY;
      srid        NUMBER;

   BEGIN

      IF p_y2 = 8265
      THEN
         RAISE_APPLICATION_ERROR(-20001, 'You forgot a comma dude');
      END IF;

      IF p_srid IS NULL
      THEN
         srid := 8265;
      ELSE
         srid := p_srid;
      END IF;

      output := SDO_GEOMETRY(2002,
                             srid,
                             NULL,
                             SDO_ELEM_INFO_ARRAY(1, 2, 1),
                             SDO_ORDINATE_ARRAY(p_x1,
                                                p_y1,
                                                p_x2,
                                                p_y2
                                                )
                             );

       RETURN output;

   END MAKE_GEOM_FROM_SIDEY_SEGMENT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ----Public-------------------------------------------------------------------------------

   FUNCTION SLOPE_CALCULATOR (
      p_x1                          IN NUMBER,
      p_y1                          IN NUMBER,
      p_x2                          IN NUMBER,
      p_y2                          IN NUMBER
   ) RETURN NUMBER DETERMINISTIC
   AS

      --Matt! 5/03/10
      --Stop Snitchin'!  If anyone asks, you never saw this

      slope    NUMBER;

   BEGIN

      BEGIN

         slope := (p_y1 - p_y2)/(p_x1 - p_x2);


         EXCEPTION
         WHEN OTHERS THEN
            IF SQLCODE = -01476
            THEN

               --Divide by zero.  Just return a big number
               IF p_y1 > p_y2
               THEN
                  RETURN 999999999;
               ELSE
                  RETURN -999999999;
               END IF;

            ELSE
               RAISE;
            END IF;

      END;

      RETURN slope;

   END SLOPE_CALCULATOR;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION STRINGARRAY_TO_VARRAY (
      p_input     IN GZ_TYPES.stringarray
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

   END STRINGARRAY_TO_VARRAY;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION STRINGARRAY2HASH (
      p_input       IN GZ_TYPES.stringarray
   )
   RETURN GZ_CLIP.stringhash DETERMINISTIC
   AS

     splithash     GZ_CLIP.stringhash;

   BEGIN

        FOR i IN 1 .. p_input.COUNT
        LOOP
           splithash(p_input(i)) := '1';
        END LOOP;

        RETURN splithash;

   END STRINGARRAY2HASH;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION STRINGARRAY_TO_VARCHAR (
      p_input             IN GZ_TYPES.stringarray,
      p_delim             IN VARCHAR2
   ) RETURN VARCHAR2 DETERMINISTIC
   AS

      output   VARCHAR2(4000);
      pkey     PLS_INTEGER;

   BEGIN

     pkey := p_input.FIRST;
     LOOP
        EXIT WHEN NOT p_input.EXISTS(pkey);

       IF output IS NULL
       THEN
          output := p_input(pkey);
        ELSE
          output := output || p_delim || p_input(pkey);
       END IF;

       pkey  := p_input.NEXT(pkey);

     END LOOP;

     RETURN output;

   END STRINGARRAY_TO_VARCHAR;




   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION SHOW_ME_COORD_INDEX (
      p_index     IN NUMBER,
      p_input     IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY
   AS

      --this is a helper for debugging
      --not called in actual code
      --Matt! 6/1/10

      --sample usage:
      --select GZ_CLIP.SHOW_ME_COORD_INDEX(18,a.geometry)
      --       from statefp08_edge$ a where a.edge_id = 864


      out_ordinates     SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();
      output            SDO_GEOMETRY;

   BEGIN

      IF p_input.sdo_gtype != 2002
      THEN
         RAISE_APPLICATION_ERROR(-20001, 'Sorry dude, I only roll with edges, input gtype is ' || p_input.sdo_gtype);
      END IF;

      out_ordinates.EXTEND(4);
      out_ordinates(1) := p_input.sdo_ordinates((p_index * 2) + 1);
      out_ordinates(2) := p_input.sdo_ordinates((p_index * 2) + 2);
      out_ordinates(3) := p_input.sdo_ordinates((p_index * 2) + 3);
      out_ordinates(4) := p_input.sdo_ordinates((p_index * 2) + 4);

      output := SDO_GEOMETRY(2005,
                             p_input.sdo_srid,
                             NULL,
                             SDO_ELEM_INFO_ARRAY(1,1,2),
                             out_ordinates);


      RETURN output;


   END SHOW_ME_COORD_INDEX;




   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------


   FUNCTION ORDINATE_ROUNDER (
      p_geometry               IN SDO_GEOMETRY,
      p_places                 IN PLS_INTEGER DEFAULT 6
   ) RETURN SDO_GEOMETRY DETERMINISTIC

   AS

   --Matt! 6/19/10 Same as CAMPS


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

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------


   PROCEDURE CLEAN_UP_GEOMETRIES (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2,
      p_gtype          IN NUMBER,
      p_column_name    IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_skip_dups      IN VARCHAR2 DEFAULT 'NO',
      p_precision_too  IN VARCHAR2 DEFAULT 'NO',
      p_digits_right   IN NUMBER DEFAULT 6,
      p_tolerance      IN NUMBER DEFAULT .05
   )
   AS

      --Matt! 6/22/10
      --Tis a sin to output invalid geometries and silly precision
      --What else is a sin?

      --Ex
      -- BEGIN
      -- GZ_CLIP.CLEAN_UP_GEOMETRIES('GEN_ACS07_GH5','gen_faces_06');
      -- END;

      psql           VARCHAR2(4000);
      tabname        VARCHAR2(4000);
      colname        VARCHAR2(4000);

   BEGIN

      tabname := UPPER(p_table_name);
      colname := UPPER(p_column_name);


      IF UPPER(p_precision_too) = 'YES'
      THEN

         psql := 'UPDATE ' || tabname || ' a '
              || 'SET '
              || 'a.' || colname || ' =  GZ_CLIP.ORDINATE_ROUNDER(a.' || colname || ', :p1 ) ';

         EXECUTE IMMEDIATE psql USING p_digits_right;

         COMMIT;

      END IF;


      IF UPPER(p_skip_dups) = 'NO'
      THEN

         psql := 'UPDATE ' || tabname || ' a '
              || 'SET '
              || 'a.' || colname || ' =  SDO_UTIL.REMOVE_DUPLICATE_VERTICES(a.' || colname || ', :p1 ) '
              || 'WHERE '
              || 'SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.' || colname || ', :p2 ) != :p3 '
              || 'AND SDO_UTIL.GETNUMVERTICES(a.' || colname || ') > :p4 ';

         IF p_gtype = 2003
         THEN

            EXECUTE IMMEDIATE psql USING p_tolerance,
                                         p_tolerance,
                                         'TRUE',
                                         8;

         ELSIF p_gtype = 2002
         THEN

            EXECUTE IMMEDIATE psql USING p_tolerance,
                                         p_tolerance,
                                         'TRUE',
                                         4;

         ELSIF p_gtype = 2001
         THEN

            NULL;

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Dude, I dont know what to do with gtype ' || p_gtype);

         END IF;

      END IF;

   END CLEAN_UP_GEOMETRIES;



   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC-------------------------------------------------------------------------

   FUNCTION FIX_INVALID_GEOMETRIES (
      p_schema          IN VARCHAR2,
      p_table_name      IN VARCHAR2,
      p_pkccol          IN VARCHAR2,
      p_gtype           IN NUMBER,
      p_topo            IN VARCHAR2,
      p_column_name     IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_flag_col        IN VARCHAR2 DEFAULT 'QC',
      p_intersect_size  IN NUMBER DEFAULT NULL,
      p_intersect_ratio IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! 7/20/10
      --Update 11/12/10 with guess at codes for QC

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
                                         GZ_BUSINESS_UTILS.stringarray_to_varray(dup_ids),
                                         8;

            COMMIT;

         ELSIF p_gtype = 2002
         THEN

            EXECUTE IMMEDIATE psql USING p_tolerance,
                                         GZ_BUSINESS_UTILS.stringarray_to_varray(dup_ids),
                                         6;

            COMMIT;

         END IF;


         --CHECK if we are good
         --duppsql saved above, just look at the ones in play as dups
         duppsql := duppsql || ' AND a.' || p_pkccol || ' IN (SELECT * FROM TABLE(:p3)) ';


         EXECUTE IMMEDIATE duppsql BULK COLLECT INTO post_dups USING p_tolerance,
                                                                     '13356%',
                                                                     GZ_BUSINESS_UTILS.stringarray_to_varray(dup_ids);

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
                                                                    GZ_BUSINESS_UTILS.stringarray_to_varray(int_ids);

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
                                                                    GZ_BUSINESS_UTILS.stringarray_to_varray(int_ids2);

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

         EXECUTE IMMEDIATE psql bulk collect into int_fixed USING GZ_BUSINESS_UTILS.stringarray_to_varray(int_ids),  --initial self intersects
                                                                  GZ_BUSINESS_UTILS.stringarray_to_varray(int_ids3); --never fixed intersects

      END IF;

      IF p_intersect_size IS NOT NULL
      AND int_fixed.COUNT > 0
      THEN

         psql := 'SELECT a.' || p_pkccol || ' '
              || 'FROM ' || p_table_name || ' a '
              || 'WHERE a.' || p_pkccol || ' IN (SELECT * FROM TABLE(:p1)) '
              || 'AND SDO_GEOM.SDO_AREA(a.' || p_column_name || ', :p2) < :p3 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO small_fixed USING GZ_BUSINESS_UTILS.stringarray_to_varray(int_fixed),
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
              || ') AND '  --decode to avoid divide by zero.  Return zero when length is zero, else divide
              || 'DECODE(SDO_GEOM.SDO_LENGTH(a.' || p_column_name || ',:p4 ), :p5, :p6, '
              ||                            'SQRT(SDO_GEOM.SDO_AREA(a.' || p_column_name || ',:p7)) / SDO_GEOM.SDO_LENGTH(a.' || p_column_name || ',:p8 )) < :p9 ';

              --bad
              --|| 'SQRT(SDO_GEOM.SDO_AREA(a.' || p_column_name || ', :p4))/SDO_GEOM.SDO_LENGTH(a.' || p_column_name || ', :p5 ) < :p6 ';


         EXECUTE IMMEDIATE psql BULK COLLECT INTO ratio_fixed USING GZ_BUSINESS_UTILS.stringarray_to_varray(int_fixed),
                                                                    -1,
                                                                    -1,
                                                                    p_tolerance,
                                                                    0,
                                                                    0,
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

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION PARSE_CRSWKT (
      p_srid        NUMBER,
      p_item_no     NUMBER,
      p_keyword1    VARCHAR2,
      p_keyword2    VARCHAR2 DEFAULT NULL,
      p_keyword3    VARCHAR2 DEFAULT NULL,
      p_keyword4    VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --stoled from CAMPS! 12/16/10

      TYPE hash_table IS TABLE OF VARCHAR2(255)
      INDEX BY VARCHAR2(255);

      myhash     hash_table;

      TYPE array_table IS TABLE OF VARCHAR2(255)
      INDEX BY PLS_INTEGER;

      mylevel    array_table;
      myposition array_table;

      WKTEXT           VARCHAR2(4000);
      p_return         VARCHAR2(4000);
      p_key            VARCHAR2(255);
      p_match          VARCHAR2(255);
      p_wktext         VARCHAR2(1);

      p_word           VARCHAR2(4000) := '';
      p_level          PLS_INTEGER    := 0;
      p_check          PLS_INTEGER    := 0;
      p_delay          BOOLEAN        := FALSE;
      p_quote          BOOLEAN        := FALSE;

   BEGIN

      EXECUTE IMMEDIATE 'SELECT a.wktext '
                     || 'FROM mdsys.cs_srs a '
                     || 'WHERE srid = :p1 '
                     INTO WKTEXT USING TO_CHAR(p_srid);

      FOR i IN 1 .. LENGTH(WKTEXT)
      LOOP

         p_wktext := SUBSTR(WKTEXT,i,1);

         IF    p_wktext = '"'
         THEN
            IF p_quote = TRUE
            THEN
               p_quote := FALSE;
            ELSE
               p_quote := TRUE;
            END IF;

         ELSIF p_wktext = '['
            OR ( p_wktext = ',' AND p_delay = TRUE )
         THEN


            p_word := LTRIM(p_word);
            p_word := RTRIM(p_word);

            IF p_word = 'PARAMETER'
            THEN
               p_delay := TRUE;
               p_word  := p_word || ' ';
            ELSE
               p_delay := FALSE;
               p_level := p_level + 1;
               myposition(p_level) := 0;
               mylevel(p_level) := p_word;
               p_word := '';
            END IF;

         ELSIF p_wktext = ','
         THEN
            p_word := LTRIM(p_word);
            p_word := RTRIM(p_word);

            myposition(p_level) := myposition(p_level) + 1;

            p_key := '';
            FOR j IN 1 .. p_level
            LOOP
               p_key := p_key || ',' || mylevel(j);
            END LOOP;
            p_key := TO_CHAR(myposition(p_level)) || p_key;
            myhash(p_key) := p_word;
            p_word := '';

            p_level := p_level - p_check;
            p_check := 0;


         ELSIF p_wktext = ']'
         THEN
            p_check := p_check + 1;

         ELSIF p_wktext = '"'
         THEN
            NULL;

         ELSIF p_wktext = Chr(10)
         THEN
            NULL;

         ELSE

            p_word := p_word || p_wktext;
         END IF;


      END LOOP;

      p_word := LTRIM(p_word);
      p_word := RTRIM(p_word);

      myposition(p_level) := myposition(p_level) + 1;

      p_key := '';
      FOR j IN 1 .. p_level
      LOOP
         p_key := p_key || ',' || mylevel(j);
      END LOOP;
      p_key := TO_CHAR(myposition(p_level)) || p_key;
      myhash(p_key) := p_word;

      IF    p_item_no = 0
      THEN
         p_key := myhash.FIRST;
         LOOP
            -- Exit when the next value is null, i.e. does not exist
            EXIT WHEN NOT myhash.EXISTS(p_key);

            p_return := p_return || p_key || ' => ' || myhash(p_key) || Chr(10);

            p_key := myhash.NEXT(p_key);
         END LOOP;
         RETURN p_return;
      END IF;


      p_match := TO_CHAR(p_item_no) || ',' || p_keyword1;
      IF p_keyword2 IS NOT NULL
      THEN
         p_match := p_match || ',' || p_keyword2;
      END IF;
      IF p_keyword3 IS NOT NULL
      THEN
         p_match := p_match || ',' || p_keyword3;
      END IF;
      IF p_keyword4 IS NOT NULL
      THEN
         p_match := p_match || ',' || p_keyword4;
      END IF;

      p_key := myhash.FIRST;
      LOOP
         -- Exit when the next value is null, i.e. does not exist
         EXIT WHEN NOT myhash.EXISTS(p_key);

         IF p_match = p_key
         THEN
            RETURN myhash(p_key);
         END IF;

         p_key := myhash.NEXT(p_key);
      END LOOP;

      RETURN NULL;

   END PARSE_CRSWKT;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

    FUNCTION FULL_DIST_CONVERT (
      p_input        IN NUMBER,
      p_input_units  IN VARCHAR2,
      p_output_units IN VARCHAR2 DEFAULT NULL
   ) RETURN NUMBER DETERMINISTIC
   AS

      --ancient Mattility from CAMPS 12/16/10

      psql                VARCHAR2(4000);
      psql2                VARCHAR2(4000);
      v_input_conversion  NUMBER;
      v_output_conversion NUMBER;

   BEGIN
      --Enter just the input units, the result is meters

      BEGIN
         psql := 'SELECT conversion_factor '
              || 'FROM SDO_DIST_UNITS '
              || 'WHERE '
              || '(sdo_unit = :p1 OR unit_name = :p2 ) AND '
              || 'rownum = 1';

         EXECUTE IMMEDIATE psql INTO v_input_conversion USING UPPER(p_input_units),
                                                              p_input_units;

     EXCEPTION
        WHEN NO_DATA_FOUND THEN
           RAISE_APPLICATION_ERROR(-20001,'No match found in SDO_DIST_UNITS for ' || p_input_units || '!');
        WHEN TOO_MANY_ROWS THEN
           RAISE_APPLICATION_ERROR(-20001,'Multiple matches found in SDO_DIST_UNITS for ' || psql || '!');
     END;

     BEGIN
         IF p_output_units IS NULL
         THEN
            v_output_conversion := 1;
         ELSE
            psql2 := 'SELECT conversion_factor '
                  || 'FROM SDO_DIST_UNITS '
                  || 'WHERE '
                  || '(sdo_unit = :p1 OR unit_name = :p2) AND '
                  || 'rownum = 1';

            EXECUTE IMMEDIATE psql2 INTO v_output_conversion USING UPPER(p_output_units),p_output_units;

         END IF;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
           RAISE_APPLICATION_ERROR(-20001,'No match found in SDO_DIST_UNITS for ' || p_input_units || '!');
        WHEN TOO_MANY_ROWS THEN
           RAISE_APPLICATION_ERROR(-20001,'Multiple matches found in SDO_DIST_UNITS for ' || psql2|| '!');
      END;

      RETURN (p_input * v_input_conversion * (1/v_output_conversion));

   END FULL_DIST_CONVERT;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

    FUNCTION TOLERANCE_CONVERTER (
      p_input         IN NUMBER,
      p_input_srid    IN VARCHAR2 DEFAULT '8265',
      p_output_srid   IN VARCHAR2 DEFAULT 'NULL', --NULL = Special, no SRID flag
      p_alaska_flag   IN VARCHAR2 DEFAULT NULL
   ) RETURN NUMBER DETERMINISTIC
   AS

      --Matt! 12/17/10.
      --Problem: Have 8265 data, need to run the Oracle Fns with NULL SRIDs
      --Tolerance magical happy time changes mirrored here. Probably garbage
      --Currently code only runs through the hard coded exceptions

      output         NUMBER;
      input_units    VARCHAR2(4000);
      output_units   VARCHAR2(4000);


   BEGIN

      input_units := UPPER(GZ_CLIP.PARSE_CRSWKT(TO_NUMBER(p_input_srid),
                                                1,
                                                'PROJCS',
                                                'UNIT'));

      --cant parse Meter from 8265 WKT
      IF input_units IS NULL
      THEN

         input_units := 'METER';

      END IF;

      IF p_output_srid != 'NULL'
      THEN

         output_units := UPPER(GZ_CLIP.PARSE_CRSWKT(TO_NUMBER(p_output_srid),
                                                   1,
                                                   'PROJCS',
                                                   'UNIT'));

         --cant parse Meter from 8265 WKT
         IF output_units IS NULL
         THEN

            output_units := 'METER';

         END IF;

      ELSE

         output_units := 'DEGREE';  -- this is not a valid sdo dist units value

      END IF;


      IF output_units != 'DEGREE'
      THEN

         output := GZ_CLIP.FULL_DIST_CONVERT(p_input, input_units, output_units);

      ELSE

         IF input_units = 'METER'
         AND output_units = 'DEGREE'
         THEN

            --convert meters to something degree like
            --Learn: At the poles, even .05 meters becomes infinite in degrees longitude
            --In general we want to take the larger tolerance, to avoid any appearance of overlap or node on node
            --   action when back on the sphere

            IF p_alaska_flag IS NULL
            THEN

               --1 degree per 111320 meters at equator
               output := p_input * 1./111320.;

            ELSE

               --take a crack at Alaska
               output := p_input * 1./(111320. * cos(72.));  -- used to be 65 (Sidey T)

            END IF;


         ELSE

            RAISE_APPLICATION_ERROR(-20001, 'Sorry, I dont know how to convert ' || input_units || ' to ' || output_units);

         END IF;

      END IF;


      RETURN output;


   END TOLERANCE_CONVERTER;



   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

END GZ_CLIP;
/

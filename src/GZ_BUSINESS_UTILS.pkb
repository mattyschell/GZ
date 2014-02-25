CREATE OR REPLACE PACKAGE BODY GZ_BUSINESS_UTILS
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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_PROJECT_WRK2003: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_PROJECTION_Output;


--
FUNCTION TABLE_EXISTS ( pInTable  IN VARCHAR2, pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN AS

   /**Taken from the cartodb
    Author: Nick Padfield
    Creation Date: 9/6/2006
    See GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS for an alternative implementation
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
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------


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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS', p_table_name);

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
      --  GZ_BUSINESS_UTILS.GEN_TRACKING_LOG('MYSTEP',NULL,NULL,start_time);
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

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('CLIP',
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

      IF UPPER(p_module) NOT IN ('CLIP','MERGE','TOPOFIX','BUILD','OUTPUT','SUPER','ZONE','LS', 'SP','PROJECTION','MERGEFACE','QA')
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








   -----------------------------
   --All other table creators
   -----------------------------

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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER(p_table_name,p_table_name);

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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

      GZ_BUSINESS_UTILS.ADD_INDEX(p_table_name, p_table_name || '_LYR', 'LAYER', 'BITMAP');
      GZ_BUSINESS_UTILS.ADD_INDEX(p_table_name, p_table_name || '_EDG', 'EDGE_ID');

      --Manage privvies
      --workflow does to public

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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

      GZ_BUSINESS_UTILS.ADD_INDEX(p_table_name, p_table_name || 'TN', 'TILE_NUMBER', 'BITMAP');
      GZ_BUSINESS_UTILS.ADD_INDEX(p_table_name, p_table_name || 'SEI', 'SOURCE_EDGE_ID','UNIQUE');

      --Manage privvies
      --workflow does to public

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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

      GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(p_table_name,
                                     'SDOGEOMETRY',
                                     p_srid,
                                     .05);

      --Manage privvies
      --workflow does to public

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
      GZ_BUSINESS_UTILS.ADD_INDEX(p_table_name, p_table_name || 'idx1', 'OID_CHILD');

      --Manage privvies
      --workflow does to public

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

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
              || '      CHECK (SLIVER_EXEMPT = ''Y'') '
              || ')';


      --No triggers
      --No indexes

      --Manage privvies
      --workflow does to public

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_GZ_FACE_MERGE: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END CREATE_GZ_FACE_MERGE;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CREATE_GZ_FACE_SLIVERS (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   )
   AS

      --Matt! 8/2/13
      --Transaction table for coastal slivers

      v_schema      VARCHAR2(32);

   BEGIN

      v_schema := SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA');

      IF p_table_name IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bud, need a table name');

      END IF;

      GZ_WORKFLOW.CREATE_TABLE(v_schema,
                               p_table_name,
                               'GZ_TYPES.GZ_FACE_SLIVERS',
                               'Y');

      --no table creation garbage and error handling... all in workflow package


      --Add constraints

      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(sliver_id) '
              || ')';

      --No triggers

      --Index on face id. Not unique. Code will query by it a lot
      GZ_BUSINESS_UTILS.ADD_INDEX(p_table_name, p_table_name || 'idx1', 'FACE_ID');

      --Manage privvies
      --workflow does to public

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);


   END CREATE_GZ_FACE_SLIVERS;


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
                   || 'SELECT * FROM TABLE(' || p_schema || '.GZ_BUSINESS_UTILS.NEW_SMALL_POLY_RSUV ) ';

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


      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_SMALL_POLY_RSUV: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


   END CREATE_SMALL_POLY_RSUV;











   -----------------------------------------------------------------------------
   -- Database Objects
   -----------------------------------------------------------------------------

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
      --IF NOT GZ_BUSINESS_UTILS.INDEX_EXISTS('GZACS12.Z699IN_FACE','SDOGEOMETRY','DOMAIN')
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
         who        := GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_table_name, 'LEFT');
         the_table  := GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_table_name, 'RIGHT');

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
      --IF NOT GZ_BUSINESS_UTILS.COLUMN_EXISTS('GZACS12.Z699IN_FACE', '''ANRC'',''AIANNHCOMP'',''AIANNH''', 3)
      --THEN
      --   --columns missing!
      --

      --SAMPLE for q-quoting mensches
      --
      --IF NOT GZ_BUSINESS_UTILS.COLUMN_EXISTS('Z699IN_FACE', q'^'ANRC','AIANNHCOMP','AIANNH'^', 3)
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
         who        := GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_table_name, 'LEFT');
         the_table  := GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_table_name, 'RIGHT');

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

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GET_PRIMARY_KEY_COLS (
      p_table_name      IN VARCHAR2,
      p_schema          IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 6/12/13
      --Use p_schema if interrogating a remote schema.  May be slower

      -- mystringarray:= GZ_BUSINESS_UTILS.GET_PRIMARY_KEY_COLS('GZ_LAYERS_CROSSWALK')
      -- returns RELEASE, OUTPUT_FIELD

      psql        VARCHAR2(4000);
      output      GZ_TYPES.stringarray;
      which       VARCHAR2(4);

   BEGIN

      IF p_schema IS NOT NULL
      THEN

        which := 'all';

      ELSE

        which := 'user';

      END IF;

      psql := 'SELECT c.column_name FROM '
           || which || '_constraints a, '
           || which || '_cons_columns c '
           || 'WHERE '
           || 'a.table_name = :p1 AND '
           || 'a.constraint_type = :p2 AND '
           || 'c.constraint_name = a.constraint_name ';


      --error handling?

      IF p_schema IS NOT NULL
      THEN

         psql := psql || 'AND '
                      || 'a.owner = :p3 AND '
                      || 'c.owner = :p4 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_table_name),
                                                               'P',
                                                               UPPER(p_schema),
                                                               UPPER(p_schema);

      ELSE

         EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_table_name),
                                                               'P';

      END IF;

      --gonna let empty arrays ship back to the caller
      RETURN output;

   END GET_PRIMARY_KEY_COLS;


   ----------------------------------------------------------------------------------------
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













   --------------------
   --Module Callss
   --------------------



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
      p_ref_table      IN VARCHAR2 DEFAULT 'REFERENCE_FACE_FIELDS',
      p_ref_col        IN VARCHAR2 DEFAULT 'FIELD',
      p_ref_field      IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 11/5/10
      --! 9/30/11 moved to GZ_UTILITIES and added project_id awareness
      --! 9/30/13 added p_ref_col and p_ref_field

      --standard usage, give me all the measurements
      --measurements := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,p_project_id,'MEASUREMENT');

      --nonstandard usage, give me the desired length of column for the STATE layer
      --lengths := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,p_project_id,'ATTRIBUTE','REFERENCE_FACE_FIELDS','FIELD_LENGTH','STATE');


      psql              VARCHAR2(4000);
      output            GZ_TYPES.stringarray;

   BEGIN


      IF p_ref_field IS NULL
      THEN

         --SOP
         psql := 'SELECT TO_CHAR(a.' || p_ref_col || ') '
              || 'FROM ' || p_ref_table || ' a '
              || 'WHERE '
              || 'UPPER(a.field_use) = :p1 AND '
              || 'UPPER(a.gen_project_id) = :p2 AND '
              || 'UPPER(a.release) = :p3 '
              || 'ORDER BY a.field ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_type),
                                                               UPPER(p_project_id),
                                                               UPPER(p_release);

      ELSE

         psql := 'SELECT a.' || p_ref_col || ' '
              || 'FROM ' || p_ref_table || ' a '
              || 'WHERE '
              || 'UPPER(a.field_use) = :p1 AND '
              || 'UPPER(a.gen_project_id) = :p2 AND '
              || 'UPPER(a.release) = :p3 AND '
              || 'UPPER(a.field) = :p4 '
              || 'ORDER BY a.field ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_type),
                                                               UPPER(p_project_id),
                                                               UPPER(p_release),
                                                               UPPER(p_ref_field);

      END IF;

      IF output.COUNT = 0
      THEN

          --this may be legit but more likely is a mistake in the table
          --or a convention change that the code here is oblivious to
          RAISE_APPLICATION_ERROR(-20001,'Didnt get back any ' || p_type || ' from ' || p_ref_table || ' ');

      END IF;

      --Add geoid, at least for now. Not in reference table by convention/decision
      IF UPPER(p_type) = 'ATTRIBUTE'
      AND p_ref_field IS NULL
      THEN

         output(output.COUNT + 1) := 'GEOID';

      END IF;

      --Got similar convention/decision for QC
      IF UPPER(p_type) = 'MEASUREMENT'
      THEN

         output(output.COUNT + 1) := 'QC';

      END IF;


      RETURN output;

   END GET_REFERENCE_FACE_FIELDS;

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
         --GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS','Z609%');
         --END;

         --Sample 2, Grant privs on all tables in my schema
         --Unlock it all
         --BEGIN
         --GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER();
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
      legal_users := GZ_BUSINESS_UTILS.GET_REF_SCHEMAS(v_current_user, ref_table);

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
      -- IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(my_important_table)
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
      --GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS('Z644CL_CLIP_FACE','FACE_ID');
      --END;

      --Sample 2 update just the records where the sdogeometry is nulled out -- topo fixer calls like this
      --BEGIN
      --GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS('Z644CL_CLIP_FACE','FACE_ID','ALL','NULL');
      --END;

      --Sample 3 just update the PA_RATIO -- clipper calls like this, one by one
      --BEGIN
      --GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS('Z644CL_CLIP_FACE','FACE_ID','PA_RATIO','ALL');
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
                                             USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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
                                                                           GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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
                                                     badedgegeoms USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(badrecs),
                                                                        p_tolerance,
                                                                        'TRUE';

            FOR i IN 1 .. badedges.COUNT
            LOOP

               --is there a justification for using this one (with Sidey's GCS)
               --vs oracle remove_duplicate_vertices?
               badedgegeoms(i) := GZ_GEOM_UTILS.REMOVE_CLOSE_ORDINATES(badedgegeoms(i), p_tolerance);

               --this will properly update edge$
               --and should also make other appropriate changes, like face$ mbrs
               GZ_TOPO_UTIL.GZ_CHANGE_EDGE_COORDS(topology,
                                                  badedges(i),
                                                  badedgegeoms(i));

            END LOOP;


            --lets now take another crack at updating NULLys

            --note reuse of updategeompsql stem from above

            EXECUTE IMMEDIATE updategeompsql || ' WHERE a.' || p_pkc_col || ' IN '
                                             || '(SELECT * FROM TABLE(:p1)) ' USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(badrecs);

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
                                                                              GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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

            EXECUTE IMMEDIATE updategeompsql USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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
                                                   GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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
                                                   GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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
                                                   GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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

            EXECUTE IMMEDIATE updategeompsql USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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

            EXECUTE IMMEDIATE updategeompsql USING 1, GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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

            EXECUTE IMMEDIATE updategeompsql USING 2, GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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

            EXECUTE IMMEDIATE updategeompsql USING 1, GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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

            EXECUTE IMMEDIATE updategeompsql USING 2, GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(face_ids);

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
      --GZ_BUSINESS_UTILS.POPULATE_EDIT_MEASUREMENTS('Z955LS','Z955LS_CLIP_FACE',1407);
      --Sample usage 2, Ive been fixing stuff and I want the face table updated, but
      --                I have no confidence that the faces are valid
      --GZ_BUSINESS_UTILS.POPULATE_EDIT_MEASUREMENTS('Z955LS','Z955LS_CLIP_FACE',1407,'N');

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

         validstr := GZ_TOPO_UTIL.VALIDATE_TOPOLOGY(p_topo);

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

      EXECUTE IMMEDIATE psql USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(edited_faces);

      COMMIT;

      GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_face_tab,
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

            newtab := GZ_BUSINESS_UTILS.COPY_TO_X(tabs(i));

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

            GZ_BUSINESS_UTILS.GZ_DROP_TABLE(release_tables(i));

         END LOOP;

         IF p_new_schema = 'Y'
         THEN

            --only kill off the special tables if we know this a brand new schema
            --and we are building up from scratch
            --most importantly, our reference_schemas permissions on the new tables will be missing

            FOR i IN 1 .. other_tables.COUNT
            LOOP

               GZ_BUSINESS_UTILS.GZ_DROP_TABLE(other_tables(i));

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
         OR NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(other_tables(i))
         THEN

            psql := 'BEGIN GZ_BUSINESS_UTILS.CREATE_' || other_tables(i) || '(); END; ';

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
         OR NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(release_tables(i))
         THEN

             psql := 'BEGIN GZ_BUSINESS_UTILS.CREATE_' || release_tables(i) || '(); END; ';

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

            backup_tables(i) := GZ_BUSINESS_UTILS.COPY_TO_X(all_tables(i));

         EXCEPTION
         WHEN OTHERS
         THEN

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' on GZ_BUSINESS_UTILS.COPY_TO_X ' || all_tables(i));

         END;

      END LOOP;

      GZ_BUSINESS_UTILS.CREATE_GZ_TABLES('Y','Y');  --all flags to drop

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

         GZ_BUSINESS_UTILS.GZ_DROP_TABLE(backup_tables(i));

      END LOOP;

      --we may have lost our privvies when the reference_schemas table was rebuilt
      --Grant privs on all based on whats in the final rebuilt reference_schemas

      FOR i IN 1 .. all_tables.COUNT
      LOOP

         GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',all_tables(i));

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

      tabs := GZ_BUSINESS_UTILS.SPLIT(p_table_list,',');

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
      --wrapper for GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER

      tabs     GZ_TYPES.stringarray;

   BEGIN

      tabs := GZ_BUSINESS_UTILS.SPLIT(p_table_list,',');

      FOR i IN 1 .. tabs.COUNT
      LOOP

         --dbms_output.put_line(TRIM(UPPER(tabs(i))));

         BEGIN

            GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',TRIM(UPPER(tabs(i))));

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













   ---------------------------------------------
   --Parsing and logic utils
   ---------------------------------------------

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

   FUNCTION NUMBERARRAY_ADD (
     p_input_1   IN GZ_TYPES.numberarray,
     p_input_2   IN GZ_TYPES.numberarray
   ) RETURN GZ_TYPES.numberarray DETERMINISTIC
   AS

     --Matt! 7/2/13
     output     GZ_TYPES.numberarray;
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

   END NUMBERARRAY_ADD;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION NUMBERARRAY_ADD_UNIQUE (
     p_input_1   IN GZ_TYPES.numberarray,
     p_input_2   IN GZ_TYPES.numberarray
   ) RETURN GZ_TYPES.numberarray DETERMINISTIC
   AS

      --Matt! 10/16/12
      --7/2/13! Copied over from GZ_SMPOLY

      output      GZ_TYPES.numberarray;
      tester      GZ_TYPES.numberhash;
      pcounter    PLS_INTEGER := 1;
      pkey        PLS_INTEGER;

   BEGIN

      pkey := p_input_1.FIRST;
      LOOP

         EXIT WHEN NOT p_input_1.EXISTS(pkey);

         IF NOT tester.EXISTS(p_input_1(pkey))
         THEN

            output(pcounter) := p_input_1(pkey);
            pcounter := pcounter + 1;

            tester(p_input_1(pkey)) := 1;

         END IF;

         pkey  := p_input_1.NEXT(pkey);

      END LOOP;

      pkey := p_input_2.FIRST;
      LOOP

         EXIT WHEN NOT p_input_2.EXISTS(pkey);

         IF NOT tester.EXISTS(p_input_2(pkey))
         THEN

            output(pcounter) := p_input_2(pkey);
            pcounter := pcounter + 1;

            tester(p_input_2(pkey)) := 1;

         END IF;

         pkey  := p_input_2.NEXT(pkey);

      END LOOP;

      RETURN output;

   END NUMBERARRAY_ADD_UNIQUE;
   
   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------
   
    FUNCTION NUMBERARRAY_SUBTRACT (
      p_input_1           IN GZ_TYPES.numberarray,
      p_input_2           IN GZ_TYPES.numberarray
   ) RETURN GZ_TYPES.numberarray DETERMINISTIC
   AS
   
      --Matt! 12/24/13   
      -- (set 1) minus (set 2)
   
      output         GZ_TYPES.numberarray;
      tester         GZ_TYPES.numberhash;
      pcounter       PLS_INTEGER := 1;
      pkey           PLS_INTEGER;

   BEGIN

      --convert the array to subtract to a convenient hash so someone smarter
      --does the double dos dos
      
      pkey := p_input_2.FIRST;
      
      LOOP
        
         EXIT WHEN NOT p_input_2.EXISTS(pkey);

         tester(p_input_2(pkey)) := '1';

         pkey  := p_input_2.NEXT(pkey);
        
      END LOOP;

      --loop thru first array and add to output
      --only when no exist in the hash
      
      pkey := p_input_1.FIRST;
      
      LOOP
      
         EXIT WHEN NOT p_input_1.EXISTS(pkey);

         IF NOT tester.EXISTS(p_input_1(pkey))
         THEN
         
            output(pcounter) := p_input_1(pkey);
            pcounter := pcounter + 1;
         
         END IF;
         
         pkey  := p_input_1.NEXT(pkey);
      
      END LOOP;

     RETURN output;
   
   END NUMBERARRAY_SUBTRACT;
   
   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------
   
   FUNCTION NUMBERARRAY_SUBTRACT (
      p_input_1           IN GZ_TYPES.numberarray,
      p_input_2           IN NUMBER
   ) RETURN GZ_TYPES.numberarray DETERMINISTIC
   AS
   
      --Matt! 12/24/13
      --Convenience wrapper to numberarray_subtract above
      
      tempy             GZ_TYPES.numberarray;
   
   BEGIN
   
      tempy(1) := p_input_2;
   
      RETURN GZ_BUSINESS_UTILS.NUMBERARRAY_SUBTRACT(p_input_1,
                                                    tempy);
   
   END NUMBERARRAY_SUBTRACT;

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


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION QUERY_DELIMITED_LIST (
     p_input     IN MDSYS.STRINGLIST,
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
   -----------------------------------------------------------------------------------------

   FUNCTION QUERY_DELIMITED_LIST (
     p_input     IN MDSYS.SDO_LIST_TYPE,
     p_query     IN NUMBER
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
   -----------------------------------------------------------------------------------------

   FUNCTION DUMP_NESTED_HASH (
      p_input             IN GZ_TYPES.nestedhash
   ) RETURN VARCHAR2 DETERMINISTIC
   AS

      --M@!  8/9/13
      --Dumper for my stoopid nested hash used in coastal slivers (so far)

      -- TYPE nestedhash IS TABLE OF GZ_TYPES.numberhash
      -- INDEX BY VARCHAR2(4000);

      --typically "looks" like
      --T848LS_FSL050V
      --   0500000US48427 = 1
      --   0500000US48427 = 1
      --T848LS_FSL310V
      --   310M300US21340 = 1

      --tester
      --declare
      --   testhash gz_types.nestedhash;
      --   numhash gz_types.numberhash;
      --begin
      --   numhash('first') := 1;
      --   numhash('second') := 2;
      --   testhash('outerfirst') := numhash;
      --   numhash.delete;
      --   numhash('third') := 3;
      --   numhash('fourth') := 4;
      --   testhash('outersecond') := numhash;
      --   dbms_output.put_line(gz_business_utils.dump_nested_hash(testhash));
      --end;

      outer_key            VARCHAR2(4000);
      inner_hash           GZ_TYPES.NUMBERHASH;
      inner_key            VARCHAR2(4000);
      output               VARCHAR2(8000) := '';

   BEGIN

      outer_key := p_input.FIRST;

      LOOP

         EXIT WHEN NOT p_input.EXISTS(outer_key);

         output := output || outer_key || chr(10);

         inner_hash := p_input(outer_key);

         inner_key := inner_hash.FIRST;

         LOOP

            EXIT WHEN NOT inner_hash.EXISTS(inner_key);

            output := output || '    ' || inner_key || ' = ' || inner_hash(inner_key) || chr(10);

            inner_key := inner_hash.NEXT(inner_key);

         END LOOP;

         outer_key := p_input.NEXT(outer_key);

      END LOOP;

      --dbms_output.put_line(output);

      RETURN output;

   END DUMP_NESTED_HASH;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

END GZ_BUSINESS_UTILS;
/

CREATE OR REPLACE PACKAGE BODY GZ_TOPO_BUILD
AS
PROCEDURE load_source_for_nation (topology VARCHAR2, mt_nation_source_tbl_name VARCHAR2, mt_state_load_tbl_name VARCHAR2, vintage VARCHAR2, release VARCHAR2, deploy VARCHAR2) AS
/**
 ###################################################################################################################
 # Program Name: load_source_for_nation
 # Author: mz
 # Creation Date: 06/15/2009
 # Recent Revision: 08/13/2009 - mz - changed name of procedure; added tracking 
 #
 # Purpose:
 #   The purpose of this procedure is to load data into the source table for SL010 (Nation) for PEP09. 
 #   MAF/TIGER does not populate the SL010 (Nation) table, therefore the table must be populated through 
 #   code. SL010 (Nation) tables are part of the Generalization Medium and Low resolutions.
 #
 #   In the case of PEP09, data for Puerto Rico is included in SL050/FSL050 (County) and SL040/FSL040 (State) but not
 #   in eithere SL030/FL030 (Division) or SL020/FSL020 (Region).  Therefore the data fields to be aggregated are
 #   derived from SL040/FSL040 (State) not SL020/FSL020 (Region).
 #
 # Required paramenters:
 #   - mt_nation_source_tbl_name - MAF/TIGER source table name for NATION (SL010)
 #   - mt_state_source_tbl_name - MAF/TIGER source table name for STATE (SL040)
 #   - vintage - vintage of data
 #   for tracing purposes
 #   - deploy - Schema where the table is to reside
 #   - release - Name of deliverable
 #
 # Dependencies:
 #  - Relevant '010' and '020' tables
 #  - GEN_TRACKING
 #
 ###################################################################################################################
*/
--
-- Tracking variables
--
v_process VARCHAR2(100) := 'Populate SL010 (Nation) source table'; -- Process
v_step VARCHAR2(4000);                                      -- Step
v_start_time TIMESTAMP;                                     -- Start time
v_end_time TIMESTAMP;                                       -- End time
v_elapsed_time interval DAY(5) TO second (2);               -- Elapsed time
v_proc_start_time TIMESTAMP;                                -- Procedure Start Time
v_proc_step VARCHAR2(4000);                                 -- Procedure Step
v_proc_end_time TIMESTAMP;                                  -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2);          -- Procedure Elapsed time
--
sql_stmt VARCHAR2(4000);               -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000);              -- Dynamic SQL Statement #2 for GEN_TRACKING
array_source_col mdsys.string_array;   -- Table column array
v_col_name VARCHAR(32);                -- Table column name
BEGIN
 --
 -- Start tracking for procedure
 --
 -- start process timestamp
 v_proc_start_time := systimestamp;
 -- tracking title
 v_proc_step := 'Load source for SL010';
 --
 -- delete records in if they exist in mt_nation_source_tbl_name
 --
 sql_stmt := 'delete from ' || mt_nation_source_tbl_name;
 execute immediate sql_stmt;
 commit;
 --
 -- create single record
 --
 sql_stmt := 'insert into ' || mt_nation_source_tbl_name || ' (oid,nationfp,vintage,name) values (:1,:2,:3,:4)';  
 execute immediate sql_stmt using 1,'01',vintage,'United States';
 commit;
 --
 -- Get all columns from the MAF/TIGER NATION source table that have NUMBER datatype
 --
 sql_stmt := 'select column_name from cols where table_name = ''' || mt_nation_source_tbl_name || ''' and data_type = ''NUMBER'' and column_name <> ''OID''';
 EXECUTE immediate sql_stmt bulk collect INTO array_source_col;
 --
 -- update column data
 --
 FOR i IN 1..array_source_col.LAST
  LOOP
   v_col_name := array_source_col(i);
   sql_stmt   := 'update /*+ PARALLEL 4 */ ' || mt_nation_source_tbl_name || ' a set ' || v_col_name || ' = (select sum(b.' || v_col_name || ') from ' || mt_state_load_tbl_name || ' b)';
   EXECUTE immediate sql_stmt;
   COMMIT;
  END LOOP;
 --
 -- The aggregation of SDOGEOMETRY is not necessary for creating topology from hierarchical tables and therefore
 --   has been commented out
 --
 -- Aggregate union of SDOGEOMETRY from the records in MAF/TIGER REGION (SL020) table to populate the MAF/TIGER NATION (SL010) table
 --
 -- sql_stmt := 'update /*+ PARALLEL 4 */ ' || mt_nation_source_tbl_name || ' a set a.sdogeometry = (select sdo_aggr_union(mdsys.sdoaggrtype(b.sdogeometry,0.05)) from ' || mt_region_load_tbl_name || ' b)';  
 -- EXECUTE immediate sql_stmt;  
 -- COMMIT;
 --
 -- Given that there are no more than 5 records in the MAF/TIGER REGION (SL020) source table, complex aggregation is not necessary
 -- The following is an example of a complex aggregation:
 --
 -- sql_stmt := 'update /*+ parallel 4 */ ' || mt_nation_source_tbl_name || ' a set a.sdogeometry = (SELECT /*+ parallel 4 */ sdo_aggr_union(mdsys.sdoaggrtype(aggr_geom,0.05)) aggr_geom
 -- FROM (SELECT /*+ parallel 4 */ sdo_aggr_union(mdsys.sdoaggrtype(aggr_geom,0.05)) aggr_geom
 -- FROM (SELECT /*+ parallel 4 */ sdo_aggr_union(mdsys.sdoaggrtype(aggr_geom,0.05)) aggr_geom
 -- FROM (SELECT /*+ parallel 4 */ sdo_aggr_union(mdsys.sdoaggrtype(aggr_geom,0.05)) aggr_geom
 -- FROM (SELECT /*+ parallel 4 */ sdo_aggr_union(mdsys.sdoaggrtype(b.sdogeometry,0.05)) aggr_geom
 -- FROM ' || mt_region_load_tbl_name || ' b GROUP BY mod(rownum,16)) GROUP BY mod (rownum, 8)) GROUP BY mod (rownum, 4)) GROUP BY mod (rownum, 2)))';
 --
 -- end tracking
 --
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A','Populated MAF/TIGER Nation (SL010) source table','COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,release,sql_stmt,deploy);
 COMMIT;
 --
 -- generic exception handling
 --
 EXCEPTION
 WHEN OTHERS THEN
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  -- tracking
  v_process := v_process || ' FAILED';
  v_step    := v_step || ' FAILED';
  GZ_TOPO_BUILD.gen_tracking_log(topology,mt_nation_source_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
  COMMIT;
  -- RETURN;
  RAISE;
END load_source_for_nation;
--
PROCEDURE create_topo_tables (topology VARCHAR2,topo_universe VARCHAR2,release VARCHAR2,deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: create_topo_tables
 # Author: mz
 # Creation Date: 05/29/2009
 # Recent Revisions: 06/04/2009; 06/10/2009; 06/11/2009
 #
 # Purpose:
 #   The purpose of this procedure is to create the topology source and feature tables. This procedure
 #    should be executed from the schema that will contain the topology.
 #
 # Required parameters:
 #   - topology - Name of topology
 #   - deploy - Schema where the tables are to be built
 #   - release - Name of deliverable
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #  - GEN_TRACKING
 #
 ###################################################################################################################
*/
--
-- Tracking variables
--
v_process VARCHAR2(100) := 'Create Topology Source Tables'; -- Process
v_step VARCHAR2(4000);                                      -- Step
v_start_time TIMESTAMP;                                     -- Start time
v_end_time TIMESTAMP;                                       -- End time
v_elapsed_time interval DAY(5) TO second (2);               -- Elapsed time
v_proc_start_time TIMESTAMP;                                -- Procedure Start Time
v_proc_step VARCHAR2(4000);                                 -- Procedure Step
v_proc_end_time TIMESTAMP;                                  -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2);          -- Procedure Elapsed time
--
vtopo_universe VARCHAR2(32) := topo_universe;               -- Topo Universe Table
str1 VARCHAR2(4000); -- String1 for add_code
str2 VARCHAR2(4000); -- String2 for add_code
str3 VARCHAR2(4000); -- String3 for add_code
--
sql_stmt  VARCHAR2(4000); -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
--
array_table_name mdsys.string_array; -- table name array
array_source_col mdsys.string_array; -- column array
--
array_gtype mdsys.sdo_number_array;     -- GTYPE array
v_gtype NUMBER;                         -- GTYPE
array_gtype_cnt mdsys.sdo_number_array; -- GTYPE count array
v_gtype_cnt NUMBER;                     -- GTYPE count
--
tbl_check_cnt      NUMBER;                        -- table count
tbl_record_cnt     NUMBER;                        -- table record count
v_table_name       VARCHAR2(32);                  -- table name
v_tbl_type         VARCHAR2(1);                   -- table type
v_source_location  VARCHAR2(100);                 -- table location
v_where_clause     VARCHAR2(1000);                -- table where clause
v_tbl_keys         VARCHAR2(250);                 -- table keys
v_temp_vr1         VARCHAR2(32);                  -- geometry validation table name holder
v_vr1_cnt          NUMBER;                        -- number of records in first validation table
v_temp_vr2         VARCHAR2(32);                  -- geometry validation table name holder
v_spacol           VARCHAR2(11) := 'SDOGEOMETRY'; -- spatial column to be validated - fixed
v_code1            VARCHAR2(4000);                -- special code 1
v_code2            VARCHAR2(4000);                -- special code 2
v_except_cols      VARCHAR2(4000);                -- MAF/TIGER columns not wanted in source table
v_table_fields     VARCHAR2(4000);                -- source table fields
v_col              VARCHAR2(4000);                -- holds column names
v_hack_mt_local    NUMBER;                        -- checks if MT source is local to schema
v_hack_left        NUMBER;                        -- String manipulation -- splits info from v_source_location
v_hack_right       NUMBER;                        -- String manipulation -- splits info from v_source_location
v_mt_owner         VARCHAR2(32);                  -- info derived from v_source_location
v_mt_tbl_name      VARCHAR2(32);                  -- info derived from v_source_location
v_mt_instance_name VARCHAR2(100);                 -- oracle instance derived from v_source_location
--
v_index_name VARCHAR2(50); -- index name
BEGIN
 --
 -- Start tracking for procedure
 --
 -- start process timestamp
 v_proc_start_time := systimestamp;
 -- tracking title
 v_proc_step := 'Create Source tables from MAF/TIGER; Create empty SL/FSL feature tables';
 --
 -- Get source names from topo_universe
 --
 --
 -- Start tracking step
 --
 -- start step timestamp
 v_start_time := systimestamp;
 -- tracking title
 v_step := 'Step 1: Selecting source/feature table names from TOPO_UNIVERSE';
 --
 -- Select source/feature table names from TOPO_UNIVERSE
 --
 -- topology = name of topology
 -- deploy = schema where topology resides
 -- release = Generalization release (e.g., ACS09)
 sql_stmt := 'select /*+ PARALLEL 4 */ table_name from '||vtopo_universe||' where topology = :1 and deploy = :2 and release = :3 and table_type not like :4 order by exec_order';
 EXECUTE immediate sql_stmt bulk collect INTO array_table_name USING topology, deploy, release, 'D%';
 --
 -- End step tracking
 --
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
 --
 -- Process Each Source Table
 --
 FOR i IN 1..array_table_name.LAST
  LOOP
   -- step tracking
   v_start_time := systimestamp;
   v_table_name := array_table_name(i);
   v_step := 'Step 2: Select information for ' || v_table_name || ' from TOPO_UNIVERSE';
   --
   -- select all relevant data from the topo_universe table
   --
   sql_stmt := 'select source_location,where_clause,tbl_keys,except_cols,add_code1,add_code2,table_type from '||vtopo_universe||' where topology = :1 and deploy = :2 and table_name = :3 and release = :4';
   EXECUTE immediate sql_stmt INTO v_source_location,v_where_clause,v_tbl_keys,v_except_cols,v_code1,v_code2,v_tbl_type USING topology,deploy,v_table_name,release;
   --
   -- break out source source_location variable
   --
   -- Source tables can be either local to the current schema or in schemas in remote databases
   -- Some 'source_location' examples: MT_PEP09_SL050 (original MT source); PEP09_SL050 (subset of MT_PEP09_SL050 based on release requirements);
   --      ACS09.COUNTY@DEVBENCH (remote MT source)
   -- The following string manipulation commands derives specific information from 'source_location' variables
   --
   v_hack_mt_local := instr(v_source_location,'MT_',1,1); -- if 'MT_' exists in string then MAF/TIGER source table in local schema
   v_hack_left := instr(v_source_location,'.',1,1); -- if '.' exists in string then MAF/TIGER source table is in remote schema
   v_hack_right := instr(v_source_location,'@',1,1); -- if '@' exists in string then MAF/TIGER source table is in remote schema
   v_mt_owner := SUBSTR(v_source_location,1,v_hack_left-1); -- owner of source table
   v_mt_tbl_name := SUBSTR(v_source_location,v_hack_left+1,v_hack_right-v_hack_left-1); -- source table name
   v_mt_instance_name := SUBSTR(v_source_location,v_hack_right +1); -- Oracle instance of source
   --
   -- end step tracking
   --
   v_end_time := systimestamp;
   v_elapsed_time := v_end_time - v_start_time;
   GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
   --
   -- check table type
   --
   IF (v_tbl_type = 'F') THEN
    --
    -- set topology_tablename
    --
    v_table_name := topology||'_'||v_table_name;
   END IF;
   --
   -- remove source table if it exists in local schema
   --
   -- determine whether table exists
   sql_stmt := 'select count(*) from all_tables where table_name = :1 and owner = :2';
   EXECUTE immediate sql_stmt INTO tbl_check_cnt USING v_table_name, deploy;
   --
   -- if table exists then drop/purge table
   --
   IF tbl_check_cnt > 0 THEN
    --
    -- step tracking
    --
    v_start_time := systimestamp;
    v_step := 'Step 2a: Dropping previously created source/feature table ' || v_table_name;
    --
    -- drop/purge table
    --
    sql_stmt := 'drop table ' || v_table_name || ' purge';
    EXECUTE immediate sql_stmt;
    --
    -- step tracking
    --
    v_end_time := systimestamp;
    v_elapsed_time := v_end_time - v_start_time;
    
    GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
    
   END IF;
   --
   -- create the feature table from MAF/TIGER source or already existing feature table source
   --
   -- tables that do not have a location are tables that are loaded after the FACE table is populated
   IF v_tbl_type = 'S' AND v_source_location IS NOT NULL THEN
    --
    -- step tracking
    --
    v_start_time := systimestamp;
    v_step := 'Step 3a: Create source table ' || v_table_name || ' from either MAF/TIGER or existing source(s)';
    --
    -- Get column names from either the local schema or remote schema
    --
    IF v_hack_left > 0 THEN -- remote schema
     sql_stmt    := 'select column_name from all_tab_cols@' || v_mt_instance_name || ' where owner = :1 and table_name = :2 and column_name not in ' || v_except_cols || ' and column_name not like ''SYS%'' order by column_id';
     EXECUTE immediate sql_stmt bulk collect INTO array_source_col USING v_mt_owner,v_mt_tbl_name;
     END IF;
    IF v_hack_left = 0 -- local schema
     THEN
      IF v_hack_mt_local > 0 -- MAF/TIGER local schema source
       THEN
        sql_stmt := 'select column_name from user_tab_cols where table_name = :1 and column_name not in ' || v_except_cols || ' and column_name not like ''SYS%'' order by column_id';
        EXECUTE immediate sql_stmt bulk collect INTO array_source_col USING v_source_location;
       ELSE
        -- non-MAF/TIGER local schema source
        sql_stmt := 'select column_name from cols where table_name = :1 order by column_id';
        EXECUTE immediate sql_stmt bulk collect INTO array_source_col USING v_source_location;
      END IF;
    END IF;
    --
    -- process column names from multiple lines into a single line; this creates the column names for the feature tables
    --
    v_table_fields := '';
    FOR i IN 1..array_source_col.LAST
     LOOP
      v_col := array_source_col(i);
      v_table_fields := v_table_fields || ',' || v_col;
     END LOOP;
     -- remove first comma
     v_table_fields := SUBSTR(v_table_fields,2,3999);
     --
     -- create table from already existing feature table source
     --
     sql_stmt := 'create table ' || v_table_name || ' nologging as select /*+ parallel 4 */ ' || v_table_fields || ' from ' || v_source_location || ' where ' || v_where_clause;
     EXECUTE immediate sql_stmt;
     --
     -- end step tracking
     --
     v_end_time := systimestamp;
     v_elapsed_time := v_end_time - v_start_time;
     
     GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
     --
     -- gtype #1 check
     --
     -- Obtain a count of the GTYPEs (e.g, 2001,2002,2003,2004,etc), write results to the gtype_check table
     -- This is a check to insure no unexpected GTYPEs
     sql_stmt := 'select count(*) from ' || v_table_name || '';
     EXECUTE immediate sql_stmt INTO tbl_record_cnt;
     IF tbl_record_cnt > 0 THEN
      --
      -- Insert into gtype_check table
      --
      sql_stmt := 'select /*+ parallel 4 */ a.sdogeometry.sdo_gtype, count(*) from ' || v_table_name || ' a group by a.sdogeometry.sdo_gtype order by a.sdogeometry.sdo_gtype';
      EXECUTE immediate sql_stmt bulk collect INTO array_gtype,array_gtype_cnt;
      FOR i IN 1..array_gtype.LAST
       LOOP
        v_gtype := array_gtype(i);
        v_gtype_cnt := array_gtype_cnt(i);
        sql_stmt := 'insert into gtype_check (chk,tbl_name,gtype,cnt) values (:1,:2,:3,:4)';
        EXECUTE immediate sql_stmt USING 1,v_table_name,v_gtype,v_gtype_cnt;
        COMMIT;
       END LOOP;
     END IF; 
   END IF;
   --
   -- special code from the TOPO_UNIVERSE is executed; also feature tables, generally FSL tables 
   -- that do not have a source table, are created in this step
   --
   -- step tracking
   IF v_tbl_type = 'S' THEN
     v_start_time := systimestamp;
     v_step := 'Step 4: Special processing for S type ' || v_table_name;
     BEGIN
       -- select code to be executed #1
       IF v_code1 IS NOT NULL THEN
        sql_stmt := v_code1;
        EXECUTE immediate sql_stmt; -- execute first code set
        COMMIT;
       END IF;
       -- select code to be executed #2
       IF v_code2 IS NOT NULL THEN
        sql_stmt := v_code2;
        EXECUTE immediate sql_stmt; -- execute second code set
        COMMIT;
       END IF;
       -- step tracking
       v_end_time := systimestamp;
       v_elapsed_time := v_end_time - v_start_time;
       GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
       -- end step tracking
     END;    
   END IF;
   --
   -- create empty feature tables only if there is a source table defined
   --
   IF v_tbl_type = 'F' AND v_source_location IS NOT NULL THEN
    v_start_time := systimestamp;
    v_step := 'Step 3b: Create feature table ' || v_table_name || ' from existing source';
    --
    sql_stmt := 'create table ' || v_table_name || ' nologging as select /*+ parallel 4*/ * from ' || v_source_location || ' where ' || v_where_clause;
    EXECUTE immediate sql_stmt;
    --
    v_end_time := systimestamp;
    v_elapsed_time := v_end_time - v_start_time;
    
    GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
    
   END IF;
   --
   -- special code from the TOPO_UNIVERSE is executed; also feature tables, generally FSL tables 
   -- that do not have a source table, are created in this step
   --
   -- step tracking
   IF v_tbl_type = 'F' THEN 
    v_start_time := systimestamp;
    v_step := 'Step 4: Special processing for F type ' || v_table_name;
    BEGIN
     -- select code to be executed #1
     IF v_code1 IS NOT NULL THEN
      str1 := Substr(v_code1, 1, Instr(v_code1,' ',1,2));
      str2 := Regexp_replace(v_code1,'^.*table ([^ ]*) .*$','\1');
      str3 := Substr(v_code1, Instr(v_code1,' ',1,3));
      v_code1 := str1 || topology||'_'||str2 || str3;
      sql_stmt := v_code1;
      EXECUTE immediate sql_stmt; -- execute first code set
      COMMIT; 
     END IF;
     
     -- select code to be executed #2
     IF v_code2 IS NOT NULL THEN
      str1 := Substr(v_code2, 1, Instr(v_code2,' ',1,2));
      str2 := Regexp_replace(v_code2,'^.*table ([^ ]*) .*$','\1');
      str3 := Substr(v_code2, Instr(v_code2,' ',1,3));
      v_code2 := str1 || topology||'_'||str2 || str3;
      sql_stmt := v_code2;
      EXECUTE immediate sql_stmt; -- execute second code set
      COMMIT;
     END IF;
     -- step tracking
     v_end_time := systimestamp;
     v_elapsed_time := v_end_time - v_start_time;
     GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
     -- end step tracking
    END;
   END IF;
   --
   -- create unique index for source table where table_keys are not null
   --
   IF v_tbl_type = 'S' AND v_tbl_keys IS NOT NULL THEN
    v_start_time := systimestamp;
    v_step := 'Step 5: Create a unique index for source table ' || v_table_name;
    --
    sql_stmt := 'create index ' || v_table_name || '_uk on ' || v_table_name || '(' || v_tbl_keys || ') nologging';
    EXECUTE immediate sql_stmt;
    --
    v_end_time := systimestamp;
    v_elapsed_time := v_end_time - v_start_time;
    
    GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
    
   END IF;
   --
   -- Validate sdogeometry
   --
   IF v_tbl_type = 'S' THEN
    -- start tracking
    v_start_time := systimestamp;
    v_step := 'Step 6: Validate/Rectify SDOGEOMETRY for source table ' || v_table_name;
    --
    -- create entry in user_sdo_geom_metadata
    --
    sql_stmt := 'select count(rownum) from user_sdo_geom_metadata WHERE table_name = :1 AND column_name = :2';
    EXECUTE immediate sql_stmt INTO tbl_check_cnt USING v_table_name,'SDOGEOMETRY';
    IF tbl_check_cnt = 0 THEN
     BEGIN
      -- enter new record into user_sdo_geom_metadata
      sql_stmt := 'insert into user_sdo_geom_metadata (TABLE_NAME, COLUMN_NAME, DIMINFO, SRID) VALUES (''' || v_table_name || ''',''SDOGEOMETRY'',MDSYS.SDO_DIM_ARRAY(MDSYS.SDO_DIM_ELEMENT(''Longitude'',-180,180,0.05),MDSYS.SDO_DIM_ELEMENT(''Latitude'',-90,90,0.05)),8265)';
      EXECUTE immediate sql_stmt;
      COMMIT;
      tbl_check_cnt := 0;
     END;
    END IF;
    --
    -- SDOGEOMETRY validation/rectification
    --
    -- Validation/Rectification occurs in three steps: validate,rectify,validate new geometry
    --
    -- Select only local or remote MAF/TIGER source tables
    IF v_hack_left > 0 OR v_hack_mt_local > 0 THEN
     -- sdogeometry validation #1
     -- create table to store invalid geometries
     v_temp_vr1 := v_table_name || '_VR1';
     --
     -- drop validation table #1 if it exists
     sql_stmt := 'select count(*) from user_tables where table_name = :1';
     EXECUTE immediate sql_stmt INTO tbl_check_cnt USING v_temp_vr1;
     IF tbl_check_cnt > 0 THEN
      sql_stmt := 'drop table ' || v_temp_vr1 || ' purge';
      EXECUTE immediate sql_stmt;
     END IF;
     --
     -- drop geom validation table #1 if it exists
     sql_stmt := 'select count(*) from user_tables where table_name = :1';
     EXECUTE immediate sql_stmt INTO tbl_check_cnt USING v_temp_vr1 || '_GEOM';
     IF tbl_check_cnt > 0 THEN
      sql_stmt := 'drop table ' || v_temp_vr1 || '_geom purge';
      EXECUTE immediate sql_stmt;
     END IF;
     --
     -- create validation #1 tables
     sql_stmt := 'create table ' || v_temp_vr1 || ' (sdo_rowid ROWID, result varchar2(1000)) nologging';
     EXECUTE immediate sql_stmt;
     -- validate geometry layer with context
     sql_stmt := 'call sdo_geom.validate_layer_with_context(:1,''SDOGEOMETRY'',:2)';
     EXECUTE immediate sql_stmt USING v_table_name,v_temp_vr1;
     -- validate geometry with context
     sql_stmt := 'create table ' || v_temp_vr1 || '_geom nologging as select /*+ parallel 4 */ oid,sdo_geom.validate_geometry_with_context(a.sdogeometry,0.05) val from ' || v_table_name || ' a';
     EXECUTE immediate sql_stmt;
     -- elimate valid or TRUE records to leave only the invalid ones
     sql_stmt := 'delete from ' || v_temp_vr1 || '_geom where val = ''TRUE''';
     EXECUTE immediate sql_stmt;
     COMMIT;
     --
     -- Rectify invalid geometry and update source table
     --
     -- RECTIFY INVALID GEOMETRY AND UPDATE GEOMETRY IN THE SOURCE TABLE IS NOT CURRENTLY IN USE,
     -- THEREFORE, THE CODE FOR GTYPE #2 CHECK, AND SDOGEOMETRY VALIDATION #2 IS ALSO COMMENTED OUT
     --
     -- sql_stmt := 'update /*+ parallel 4 */ ' || v_table_name || ' a set a.sdogeometry = sdo_util.rectify_geometry (a.sdogeometry,0.05) where a.rowid = (select b.sdo_rowid from ' || v_table_name || '_VR1 b where b.sdo_rowid = a.rowid and result not like ''13349%'')';
     -- execute immediate sql_stmt;
     -- commit;
     -- re-check validity of sdogeometry
     -- gtype #2 check
     --
     -- sql_stmt := 'select a.sdogeometry.sdo_gtype, count(*) from ' || v_table_name || ' a group by a.sdogeometry.sdo_gtype order by a.sdogeometry.sdo_gtype';
     -- execute immediate sql_stmt bulk collect into array_gtype,array_gtype_cnt;
     --
     -- for i in 1..array_gtype.LAST 
     --  LOOP
     --   v_gtype := array_gtype(i);
     --   v_gtype_cnt := array_gtype_cnt(i);
     --   sql_stmt := 'insert into gtype_check (chk,tbl_name,gtype,cnt) values (:1,:2,:3,:4)';
     --   execute immediate sql_stmt using 2,v_table_name,v_gtype,v_gtype_cnt;
     --   commit;
     --  end loop;
     --
     -- sdogeometry validation #2
     --
     -- create table to store invalid geometries
     -- execute only if sdogeometry validation table #1 has more than 1 record
     --
     -- sql_stmt := 'select count(*) from ' || v_temp_vr1;
     -- execute immediate sql_stmt into v_vr1_cnt;
     -- 
     -- if v_vr1_cnt > 1 then
     --  v_temp_vr2 := v_table_name || '_VR2';
     --  -- drop validation table #2 if it exists
     --  sql_stmt := 'select count(*) from user_tables where table_name = :1';
     --  execute immediate sql_stmt into tbl_check_cnt using v_temp_vr2;
     --  if tbl_check_cnt > 0 then
     --   sql_stmt := 'drop table ' || v_temp_vr2 || ' purge';
     --   execute immediate sql_stmt;
     --  end if;
     --  -- create validation table #2 layer_with_context
     --  sql_stmt := 'create table ' || v_temp_vr2 || ' (sdo_rowid ROWID, result varchar2(1000))';
     --  execute immediate sql_stmt;
     --  -- validate layer_with_context
     --  sql_stmt := 'call sdo_geom.validate_layer_with_context(:1,''SDOGEOMETRY'',:2)';
     --  execute immediate sql_stmt using v_table_name,v_temp_vr2;
     --  -- special check to see if validation table #2 has more than 1 record
     --  sql_stmt := 'select count(*) from ' || v_temp_vr2;
     --  execute immediate sql_stmt into tbl_check_cnt;
     --
     --  if tbl_check_cnt > 1 then
     --   -- write to tracking table to highlight that there were sdogeometries that couldn't be rectified
     --   sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
     --   execute immediate sql_stmt2 using v_temp_vr2,v_process,'More than 1 record',v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy;
     --  end if;
     --  end tracking step
     v_end_time := systimestamp;
     v_elapsed_time := v_end_time - v_start_time;
     GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
     -- end if; <-- this completed the 'if v_vrl_cnt > 1 IF statement
   END IF;
   --
   -- Create a Spatial Index
   --
   v_start_time := systimestamp;
   v_step := 'Step 7: Create a Spatial Index for source table ' || v_table_name;
   --
   v_index_name := v_table_name || '_SIDX';
   sql_stmt := 'select count(rownum) from user_indexes where index_name = :1';
   EXECUTE immediate sql_stmt INTO tbl_check_cnt USING v_index_name;
   --
   IF tbl_check_cnt = 0 THEN
    BEGIN
     sql_stmt := 'create index ' || v_table_name || '_sidx on ' || v_table_name || '(' || v_spacol || ') INDEXTYPE IS MDSYS.SPATIAL_INDEX PARAMETERS (''sdo_non_leaf_tbl=TRUE'')';
     EXECUTE immediate sql_stmt;
    END;
   END IF;
   --
   v_end_time := systimestamp;
   v_elapsed_time := v_end_time - v_start_time;
   GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
  END IF;
  --
  -- grant select access to public
  --
  v_start_time := systimestamp;
  v_step := 'Step 8: Grant select access to public for table ' || v_table_name;
  --
  sql_stmt := 'grant select on ' || deploy || '.' || v_table_name || ' to public';
  EXECUTE immediate sql_stmt;
  --
  -- tracking
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
 END LOOP;
 -- end tracking
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A','Created Source tables from MAF/TIGER; created empty SL/FSL feature tables','COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,release,'N/A',deploy);
 --
 -- generic exception handling
 --
 EXCEPTION
  WHEN OTHERS THEN
   -- error output
   dbms_output.put_line (SQLERRM);
   dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
   dbms_output.put_line (sql_stmt);
   -- tracking
   v_process := v_process || ' FAILED';
   v_step    := v_step || ' FAILED';
   GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
   COMMIT;
   -- RETURN;
   RAISE;
END create_topo_tables;
--
PROCEDURE create_topology (topology VARCHAR2, topo_hierarchy VARCHAR2, release VARCHAR2, deploy VARCHAR2, tolerance NUMBER, srid NUMBER, grid VARCHAR2 DEFAULT '16', drop_ft VARCHAR2 DEFAULT 'N')
AS
/**
 ###################################################################################################################
 # Program Name: create_topology
 # Author: mz
 # Creation Date: 6/18/2009
 # Recent Revision:
 #
 # Purpose:
 #   The purpose of this procedure is to create an empty topology for subsequent loading
 #    using the procedures load_topo_sl_0_statefp, load_topo_sl_0, and load_topo_sl_1_up
 #
 # Required parameters:
 #  - topology - Name of project
 #  - deploy = schema deploy
 #  - release = release
 #  - tolerance = tolerance of topology
 #  - srid = srid of topology
 #  - grid = digits_right_of_decimal DEFAULT 16
 #  - drop_ft = drop feature table ('Y' or take default 'N') if the topology already exists
 #
 # Steps to create an empty topology
 #  1. Determine if project exists in TOPO_UNIVERSE table; if it doesn't exist then exit procedure with error
 #  2. Retrieve information to create an empty topology from information in the TOPO_UNIVERSE table
 #  3. Remove target topology if it exists (drop_topology procedure)
 #  4. Create new empty topology
 #  5. Register feature tables, both non-hierachical and hierarchical, in the new topology by obtaining
 #       information from both the TOPO_UNIVERSE and TOPO_HIERARCHY lookup tables
 #
 # Dependencies:
 #  - TOPO_UNIVERSE table
 #  - TOPO_HIERARCHY table
 #  - DROP_TOPOLOGY procedure
 #
 ###################################################################################################################
*/
--
-- Information from TOPO_UNIVERSE lookup table
--
v_process VARCHAR2(100) := 'Create Topology';      -- Process
array_topology mdsys.string_array;                 -- topology
v_topology VARCHAR2(32);
array_table_name mdsys.string_array;               -- table name
v_table_name VARCHAR2(32);
array_column_name mdsys.string_array;              -- column name
v_column_name VARCHAR2(32);
array_child_layer_tbl mdsys.string_array;          -- hierarchical position
v_child_layer_tbl VARCHAR2(32);
array_name mdsys.string_array;                     -- summary level name in TOPO_HIERARCHY
v_name VARCHAR2(250);
array_gtype mdsys.string_array;                    -- GTYPE
--
vtopo_hierarchy VARCHAR2(32) := topo_hierarchy;    -- Topo Hierarchy Table
tbl_check_cnt NUMBER;                              -- table count
--
v_gtype VARCHAR2(10);                              -- geometry type
v_face VARCHAR2(32);                               -- face$ name
v_tg_layer_id NUMBER;                              -- tg_layer_id
sql_stmt VARCHAR2(4000);                           -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000);                          -- Dynamic SQL Statement for tracking
v_step VARCHAR2(4000);                             -- Step
v_start_time TIMESTAMP;                            -- Start time
v_end_time TIMESTAMP;                              -- End time
v_elapsed_time interval DAY(5) TO second (2);      -- Elapsed time
v_proc_start_time TIMESTAMP;                       -- Procedure Start Time
v_proc_step VARCHAR2(4000);                        -- Procedure Step
v_proc_end_time TIMESTAMP;                         -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2); -- Procedure Elapsed time
v_topo_exists NUMBER;                              -- Variable to hold topology existence info
BEGIN
 --
 -- Check if topology exists in topo_hierarchy
 --
 BEGIN
  -- step tracking
  v_proc_start_time := systimestamp;
  v_proc_step := v_process;
  v_start_time := systimestamp;
  v_step := 'Step 1: Check the TOPO_HIERARCHY table to confirm that Topology information exists';
  --
  -- Verify topology related data exists in TOPO_HIERARCHY
  --
  sql_stmt := 'select distinct topology from '||vtopo_hierarchy||' where topology = :1';
  EXECUTE immediate sql_stmt INTO v_topology USING topology;
  -- if not found
  EXCEPTION
   WHEN NO_DATA_FOUND THEN
    dbms_output.put_line(DBMS_UTILITY.format_error_backtrace);
    dbms_output.put_line('Project ' || topology || ' does not exist in TOPO_HIERARCHY');
    -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
    v_end_time := systimestamp;
    v_elapsed_time := v_end_time - v_start_time;
    GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
    RETURN; -- exit procedure
 END;
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
 --
 -- Remove old Topology if it exists and drop feature tables
 --
 sql_stmt := 'select count(*) from all_sdo_topo_info where topology = :1 and owner = :2';
 EXECUTE immediate sql_stmt INTO v_topo_exists USING topology, deploy;
 -- Drop if old Topology exists
 IF v_topo_exists > 0 THEN
  -- tracking
  v_start_time := systimestamp;
  v_step := 'Step 2: Remove old Topology if it exists';
  -- call drop_topology procedure
  gz_topo_build.drop_topology (topology,deploy,release,deploy,drop_ft);
  -- tracking
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
  COMMIT;
 END IF;
 --
 -- Create topology
 --
 -- tracking
 v_start_time := systimestamp;
 v_step := 'Step 3: Create empty topology and insert the universal face$ record';
 --
 -- create topology - topology name, tolerance, and srid values
 --
 sdo_topo.create_topology(topology,tolerance,srid,digits_right_of_decimal => grid);
 --
 -- insert the universal face (F0). (id = -1, not 0)
 --
 v_face := topology || '_FACE$';
 sql_stmt := 'insert into ' || v_face || ' values (-1, null, sdo_list_type(), sdo_list_type(),null)';
 EXECUTE immediate sql_stmt;
 COMMIT;
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
 --
 -- Register feature tables with new topology
 --
 -- tracking
 v_start_time := systimestamp;
 v_step := 'Step 4: Register Feature Tables';
 --
 -- obtain relevant information from TOPO_HIERARCHY to register feature tables in sequence
 --
 sql_stmt := 'select topology,table_name,column_name,child_layer_tbl,name,gtype from '||vtopo_hierarchy||' where topology = :1 order by seq_num';
 EXECUTE immediate sql_stmt bulk collect INTO array_topology,array_table_name,array_column_name,array_child_layer_tbl,array_name,array_gtype USING topology;
 --
 -- register each feature table
 --
 FOR i IN 1..array_table_name.LAST 
 LOOP
  v_topology := array_topology(i);
  v_table_name := v_topology||'_'||array_table_name(i);
  v_column_name := array_column_name(i);
  v_child_layer_tbl := array_child_layer_tbl(i);
  v_name := array_name(i);
  v_gtype := array_gtype(i);
  --
  -- determine whether table exists
  --
  sql_stmt := 'select count(*) from all_tables where table_name = :1 and owner = :2';
  EXECUTE immediate sql_stmt INTO tbl_check_cnt USING v_table_name, deploy;
  --
  -- if table exists then register table
  --
  IF tbl_check_cnt > 0 THEN
   --
   -- adding topology prefix
   --
   IF v_child_layer_tbl IS NOT NULL THEN
    v_child_layer_tbl := v_topology||'_'||v_child_layer_tbl;
   END IF;
   --
   -- register tg_level_id = 0 feature tables
   --
   IF v_child_layer_tbl IS NULL THEN
    sdo_topo.add_topo_geometry_layer(v_topology,v_table_name,v_column_name,v_gtype);
   END IF;
   --
   -- register hierarchical feature tables (tg_level_id > 0)
   --
   IF v_child_layer_tbl IS NOT NULL THEN
    -- get the tg_layer_id for the parent from all_sdo_topo_info
    sql_stmt := 'select tg_layer_id from all_sdo_topo_info where topology = :1 and table_name = :2 and column_name = :3 and owner = :4';
    EXECUTE immediate sql_stmt INTO v_tg_layer_id USING v_topology, v_child_layer_tbl, v_column_name, deploy;
    -- register the hierarchical feature table using tg_layer_id
    sdo_topo.add_topo_geometry_layer(v_topology,v_table_name,v_column_name,v_gtype,child_layer_id => v_tg_layer_id);
   END IF;
   -- tracking
   v_end_time     := systimestamp;
   v_elapsed_time := v_end_time - v_start_time;
   GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
  END IF;
  --
 END LOOP;
 --
 -- Initialize Topology Metadata
 --
 -- The recommended technique is to initialize the topology metadata (create indexes and constraints on the $ tables and
 --  topo columns in the feature tables) after the initial load. In the case of CPB Generalization, the nature of the
 --  load programs suggests that there is little advantage to initializing the metadata after the first load due to the
 --  relative small of records loaded after a topo_map commit.  Therefore, we initialize the topology metadata
 --  at the end of the create_topology procedure
 --
 -- tracking
 v_step := 'Initialize Topology Metadata';
 v_start_time := systimestamp;
 --
 -- initialize topo
 --
 sdo_topo.initialize_metadata ('' || topology || '');
 --
 dbms_output.put_line ('++++++++++++++++++++++++++++++');
 dbms_output.put_line ('Topology ' || topology || ' created');
 dbms_output.put_line ('++++++++++++++++++++++++++++++');
 --
 --
 -- step tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
 --
 -- end tracking for procedure
 --
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A','Created Topology','COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,release,'N/A',deploy);
 --
 -- generic exception handling
 --
 EXCEPTION
  WHEN OTHERS THEN
   -- error output
   dbms_output.put_line (SQLERRM);
   dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
   dbms_output.put_line (sql_stmt);
   -- tracking
   v_process := v_process || ' FAILED';
   v_step    := v_step || ' FAILED';
   GZ_TOPO_BUILD.gen_tracking_log(topology,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
   --
   RAISE;
END create_topology;
--
PROCEDURE drop_topology (topo_name IN VARCHAR2, topo_owner IN VARCHAR2, release VARCHAR2, deploy VARCHAR2, delete_topo_tables IN VARCHAR2 DEFAULT 'N')
AS
/**
 #############################################################################################################
 # Program Name: drop_topology
 # Author: mz
 # Creation Date: 10/22/2008
 # Modification History: 10/27/2008 - mz
 #
 # Purpose:
 #   The purpose of this program is to delete a topology. First, the feature tables, if they exist, are de-registered
 #   from the topology. Second, the topology is dropped.  Optionally, the feature tables are dropped, if they
 #   were registered.
 #
 # Required Parameters:
 # - topo_name - Name of topology to be dropped
 # - topo_owner - Owner of topology to be dropped
 # - release - CPB release name
 # - deploy - schema name
 # - delete_topo_tables - Whether ('Y') or not ('N') to drop the feature tables - optional
 #
 # Dependencies: None
 #
 #############################################################################################################
*/
--
-- Traking variables
--
v_process VARCHAR2(100) := 'Create Topology Source Tables'; -- Process
v_step VARCHAR2(4000);                                      -- Step
v_start_time TIMESTAMP;                                     -- Start time
v_end_time TIMESTAMP;                                       -- End time
v_elapsed_time interval DAY(5) TO second (2);               -- Elapsed time
v_proc_start_time TIMESTAMP;                                -- Procedure Start Time
v_proc_step VARCHAR2(4000);                                 -- Procedure Step
v_proc_end_time TIMESTAMP;                                  -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2);          -- Procedure Elapsed time
--
v_topo_name VARCHAR2(100);              -- Name of topology
sql_stmt VARCHAR2(4000);                -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000);               -- Dynamic SQL Statement for tracking
array_table_name mdsys.string_array;    -- Array to hold table names
v_table_name VARCHAR2(64);              -- Variable to hold table name
array_table_name2 mdsys.string_array;   -- Array to hold distinct feature table names
v_table_name2 VARCHAR2(64);             -- Variable to hold distinct feature table name
array_column_name mdsys.string_array;   -- Array to hold column_names
v_column_name VARCHAR2(64);             -- Variable to hold column_name
array_tg_layer_type mdsys.string_array; -- Array to hold tg_layer_types
v_tg_layer_type VARCHAR2(64);           -- Variable to hold tg_layer_type
v_ft_exists NUMBER;                     -- Variable to hold feature table existence info
BEGIN
 --
 -- Check if topology exists
 --
 dbms_output.put_line ('++++++++++++++++++++++++++++++');
 dbms_output.put_line ('Dropping Topology if it exists');
 BEGIN -- begin/end to enclose specific error handling
   sql_stmt := 'select distinct topology from all_sdo_topo_info where topology = :1 and owner = :2';
   EXECUTE immediate sql_stmt INTO v_topo_name USING topo_name, topo_owner;
 EXCEPTION
 WHEN NO_DATA_FOUND THEN
   dbms_output.put_line(DBMS_UTILITY.format_error_backtrace);
   dbms_output.put_line('Topology ' || topo_name || ' does not exist');
   dbms_output.put_line ('++++++++++++++++++++++++++++++');
   -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
   RETURN; -- If topology not found then exit procedure
 END;
 --
 --  Check if registered feature tables exist and if they do exist then de-register
 --  them from the topology
 --
 sql_stmt := 'select count(*) from all_sdo_topo_info where topology = :1 and owner = :2 and table_name is not null';
 EXECUTE immediate sql_stmt INTO v_ft_exists USING topo_name, topo_owner;
 --
 -- If no feature tables registered
 --
 IF v_ft_exists = 0 THEN
  dbms_output.put_line ('No feature tables associated with this topology');
 END IF;
 --
 -- Get list of distinct table names to be used in dropping feature tables if delete_feature_tables = 'Y'
 --  before the information is deleted from all_sdo_topo_info
 --
 IF v_ft_exists > 0 THEN
  sql_stmt := 'select distinct table_name from all_sdo_topo_info where topology = :1 and owner = :2';
  EXECUTE immediate sql_stmt bulk collect INTO array_table_name2 USING topo_name,topo_owner;
 END IF;
 --
 -- If feature tables registered, then de-register them from the topology
 --
 IF v_ft_exists > 0 THEN
  sql_stmt := 'select table_name, column_name, tg_layer_type from all_sdo_topo_info where topology = :1 and owner = :2 and table_name is not null order by tg_layer_level desc';
  EXECUTE immediate sql_stmt bulk collect INTO array_table_name,array_column_name,array_tg_layer_type USING topo_name, topo_owner;
  FOR i IN 1..array_table_name.LAST
   LOOP
    v_table_name := array_table_name(i);
    v_column_name := array_column_name(i);
    v_tg_layer_type := array_tg_layer_type(i);
    sdo_topo.delete_topo_geometry_layer (topo_name,v_table_name,v_column_name);
    dbms_output.put_line ('Feature Table: ' || v_table_name || ' (' || v_column_name || ' ' || v_tg_layer_type || ') de-registered from topology ' || topo_name);
   END LOOP;
 END IF;
 --
 -- Drop features tables if delete_topo_tables = 'Y'
 --
 IF v_ft_exists > 0 AND delete_topo_tables = 'Y' THEN
  FOR j IN 1..array_table_name.LAST
   LOOP
    v_table_name := array_table_name(j);
    sql_stmt := 'drop table ' || topo_owner || '.' || v_table_name || ' purge';
    EXECUTE immediate sql_stmt;
    dbms_output.put_line ('Table: ' || v_table_name || ' dropped');
   END LOOP;
 END IF;
 --
 -- Drop Topology
 --
 sdo_topo.drop_topology(topo_name);
 --
 dbms_output.put_line ('Topology ' || topo_name || ' dropped');
 dbms_output.put_line ('++++++++++++++++++++++++++++++');
 --
 -- generic exception handling
 --
 EXCEPTION
  WHEN OTHERS THEN
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  -- tracking
  v_process := v_process || ' FAILED';
  v_step := v_step || ' FAILED';
  GZ_TOPO_BUILD.gen_tracking_log(topo_name,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
  --
  RAISE;
END drop_topology;
--
PROCEDURE drop_topology2 (topo_name IN VARCHAR2, topo_owner IN VARCHAR2, release VARCHAR2, deploy VARCHAR2, delete_topo_tables IN VARCHAR2 DEFAULT 'N', raise_errors IN VARCHAR2 DEFAULT NULL)
AS
/**
 #############################################################################################################
 # Program Name: drop_topology2 
 # Author: mz
 # Creation Date: 10/22/2008
 # Modification History: 10/27/2008 - mz
 # Added raise_errors option 03/16/2010 - Matt!
 # Continue and drop xxxx$ tables no matter what if raise_errors is populated 6/21/10 Matt!
 #
 # Purpose:
 #   The purpose of this program is to delete a topology. First, the feature tables, if they exist, are de-registered
 #   from the topology. Second, the topology is dropped.  Optionally, the feature tables are dropped, if they
 #   were registered.
 #
 # Required Parameters:
 # - topo_name - Name of topology to be dropped
 # - topo_owner - Owner of topology to be dropped
 # - release - CPB release name
 # - deploy - schema name
 # - delete_topo_tables - Whether ('Y') or not ('N') to drop the feature tables - optional
 #
 # Dependencies: None
 #
 #############################################################################################################
*/
--
-- Traking variables
--
   v_process VARCHAR2(100) := 'Create Topology Source Tables'; -- Process
   v_step VARCHAR2(4000);                                      -- Step
   v_start_time TIMESTAMP;                                     -- Start time
   v_end_time TIMESTAMP;                                       -- End time
   v_elapsed_time interval DAY(5) TO second (2);               -- Elapsed time
   v_proc_start_time TIMESTAMP;                                -- Procedure Start Time
   v_proc_step VARCHAR2(4000);                                 -- Procedure Step
   v_proc_end_time TIMESTAMP;                                  -- Procedure End time
   v_proc_elapsed_time interval DAY(5) TO second (2);          -- Procedure Elapsed time
   --
   v_topo_name VARCHAR2(100);              -- Name of topology
   sql_stmt VARCHAR2(4000);                -- Dynamic SQL Statement
   sql_stmt2 VARCHAR2(4000);               -- Dynamic SQL Statement for tracking
   array_table_name mdsys.string_array;    -- Array to hold table names
   v_table_name VARCHAR2(64);              -- Variable to hold table name
   array_table_name2 mdsys.string_array;   -- Array to hold distinct feature table names
   v_table_name2 VARCHAR2(64);             -- Variable to hold distinct feature table name
   array_column_name mdsys.string_array;   -- Array to hold column_names
   v_column_name VARCHAR2(64);             -- Variable to hold column_name
   array_tg_layer_type mdsys.string_array; -- Array to hold tg_layer_types
   v_tg_layer_type VARCHAR2(64);           -- Variable to hold tg_layer_type
   v_ft_exists NUMBER;                     -- Variable to hold feature table existence info
   mskount  PLS_INTEGER;                   -- Matts check on $ table drops
BEGIN
    --
    -- Check if topology exists
    --
    dbms_output.put_line ('++++++++++++++++++++++++++++++');
    dbms_output.put_line ('Dropping Topology if it exists');
    BEGIN -- begin/end to enclose specific error handling
      sql_stmt := 'select distinct topology from all_sdo_topo_info where topology = :1 and owner = :2';
      EXECUTE immediate sql_stmt INTO v_topo_name USING topo_name, topo_owner;
    EXCEPTION
    WHEN NO_DATA_FOUND THEN
      dbms_output.put_line(DBMS_UTILITY.format_error_backtrace);
      dbms_output.put_line('Topology ' || topo_name || ' does not exist');
      dbms_output.put_line ('++++++++++++++++++++++++++++++');
      -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
      IF raise_errors IS NULL
      THEN
         RETURN; -- If topology not found then exit procedure
      ELSE
         --Matt wants this procedure to be more strict
         --Keep going if no topo found
         NULL;
      END IF;
    END;
    --
    --  Check if registered feature tables exist and if they do exist then de-register
    --  them from the topology
    --
    sql_stmt := 'select count(*) from all_sdo_topo_info where topology = :1 and owner = :2 and table_name is not null';
    EXECUTE immediate sql_stmt INTO v_ft_exists USING topo_name, topo_owner;
    --
    -- If no feature tables registered
    --
    IF v_ft_exists = 0 THEN
     dbms_output.put_line ('No feature tables associated with this topology');
    END IF;
    --
    -- Get list of distinct table names to be used in dropping feature tables if delete_feature_tables = 'Y'
    --  before the information is deleted from all_sdo_topo_info
    --
    IF v_ft_exists > 0 THEN
     sql_stmt := 'select distinct table_name from all_sdo_topo_info where topology = :1 and owner = :2';
     EXECUTE immediate sql_stmt bulk collect INTO array_table_name2 USING topo_name,topo_owner;
    END IF;
    --
    -- If feature tables registered, then de-register them from the topology
    --
    IF v_ft_exists > 0 THEN
     sql_stmt := 'select table_name, column_name, tg_layer_type from all_sdo_topo_info where topology = :1 and owner = :2 and table_name is not null order by tg_layer_level desc';
     EXECUTE immediate sql_stmt bulk collect INTO array_table_name,array_column_name,array_tg_layer_type USING topo_name, topo_owner;
     FOR i IN 1..array_table_name.LAST
      LOOP
       v_table_name := array_table_name(i);
       v_column_name := array_column_name(i);
       v_tg_layer_type := array_tg_layer_type(i);
       sdo_topo.delete_topo_geometry_layer (topo_name,v_table_name,v_column_name);
       dbms_output.put_line ('Feature Table: ' || v_table_name || ' (' || v_column_name || ' ' || v_tg_layer_type || ') de-registered from topology ' || topo_name);
      END LOOP;
    END IF;
    --
    -- Drop features tables if delete_topo_tables = 'Y'
    --
    IF v_ft_exists > 0 AND delete_topo_tables = 'Y' THEN
     FOR j IN 1..array_table_name.LAST
      LOOP
       v_table_name := array_table_name(j);
       sql_stmt := 'drop table ' || topo_owner || '.' || v_table_name || ' purge';
       EXECUTE immediate sql_stmt;
       dbms_output.put_line ('Table: ' || v_table_name || ' dropped');
      END LOOP;
    END IF;
    --
    -- Drop Topology
    --
    sdo_topo.drop_topology(topo_name);
    --
    --drop topology just moves forward happily even if it fails to drop $ tables
    --Matt wants to know
    IF raise_errors IS NOT NULL
    THEN
       sql_stmt := 'select count(*) from user_tables '
                || 'where table_name IN (:p1,:p2,:p3,:p4) ';
       EXECUTE IMMEDIATE sql_stmt INTO mskount USING topo_name || '_EDGE$',
                                                     topo_name || '_NODE$',
                                                     topo_name || '_FACE$',
                                                     topo_name || '_HISTORY$';
       IF mskount > 0
       THEN
          RAISE_APPLICATION_ERROR(-20001,'Failed to drop one of the xx_$ tables');
       END IF;                                                  
    END IF;
    dbms_output.put_line ('Topology ' || topo_name || ' dropped');
    dbms_output.put_line ('++++++++++++++++++++++++++++++');
    --
    -- generic exception handling
    --
    EXCEPTION
     WHEN OTHERS THEN
     IF raise_errors IS NOT NULL
     THEN
        --Matt likey this way
        RAISE;
     ELSE
        --Mike Z prefers to choo choo
        -- error output
        dbms_output.put_line (SQLERRM);
        dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
        dbms_output.put_line (sql_stmt);
        -- tracking
        v_process := v_process || ' FAILED';
        v_step := v_step || ' FAILED';
        GZ_TOPO_BUILD.gen_tracking_log(topo_name,v_table_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
        --
        RAISE; 
     END IF;
END drop_topology2;
--
PROCEDURE load_topo_sl_0 (topology VARCHAR2, topo_universe VARCHAR2, topo_hierarchy VARCHAR2, release VARCHAR2, deploy VARCHAR2, load_statefp VARCHAR2 DEFAULT 'FALSE')
AS
/**
 ###################################################################################################################
 # Program Name: load_topo_sl_0
 # Author: mz
 # Creation Date: 06/30/2009
 # Recent Revisions: 07/16/2009
 #
 # Purpose:
 #   The purpose of this procedure is load the tg_level_id = 0 feature tables
 #    that do NOT contain a STATEFP field
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #  - TOPO_HIERARCHY
 #
 ###################################################################################################################
*/
v_process VARCHAR2(100) := 'Load SL Feature Tables that do NOT contain a STATEFP field'; -- Process
v_step VARCHAR2(4000); -- Step
v_start_time TIMESTAMP; -- Start time
v_end_time TIMESTAMP; -- End time
v_elapsed_time interval DAY(5) TO second (2); -- Elapsed time
v_proc_start_time TIMESTAMP; -- Procedure Start Time
v_proc_step VARCHAR2(4000); -- Procedure Step
v_proc_end_time TIMESTAMP; -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2); -- Procedure Elapsed time
--
array_tbl_name mdsys.string_array;
v_tbl_name VARCHAR2(32);
tbl_record_cnt NUMBER; -- table record count
array_source_location mdsys.string_array;
v_source_location VARCHAR2(250);
array_tbl_keys mdsys.string_array;
v_tbl_keys VARCHAR2(250);
v_keys VARCHAR2(50);
array_seq_num mdsys.sdo_number_array;
v_seq_num NUMBER;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp VARCHAR2(30);
statefp_exists NUMBER;
v_release VARCHAR2(30) := NULL;
v_deploy VARCHAR2(30) := NULL;
--
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
col_cnt NUMBER;
v_cnt NUMBER;
--
v_ord1 NUMBER; -- MBR ordinate 1
v_ord2 NUMBER; -- MBR ordinate 2
v_ord3 NUMBER; -- MBR ordinate 3
v_ord4 NUMBER; -- MBR ordinate 4
alpha  NUMBER(4,3) := 0.001; -- MBR Buffer 
--
v_table_fields      VARCHAR2(4000);
v_load_table_fields VARCHAR2(4000);
--
mbr_geom SDO_GEOMETRY; -- minimum bound rectangle
--
-- array to hold tbl_fp - table primary field (not related to primary key)
-- one option is to use the OID as the MBR for the topo map but generally
-- this is too small a MBR
array_tbl_pf mdsys.string_array;
v_tbl_pf VARCHAR2(32);                -- table primary field
array_tbl_pf_loop mdsys.string_array; -- stores values of tbl_pf
v_tbl_pf_loop VARCHAR2(32);           -- tbl_fp value
--
cntr NUMBER; -- counter
BEGIN
 -- Set sdo_topo_map maximum Java memory size
 sdo_topo_map.set_max_memory_size (2147483648);
 -- set variables
 v_release := release;
 v_deploy := deploy;
 --
 -- Create topo map
 --
 -- tracking
 v_proc_start_time := systimestamp;
 v_proc_step := v_process;
 v_step := 'Create Topo Map';
 v_start_time := systimestamp;
 -- create topo map
 sdo_topo_map.create_topo_map(topology,'MTMAP',8000,4000,1500);
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
 --
 -- Obtain release and deploy info
 --sql_stmt := 'select distinct release,deploy from '||topo_universe||'';
 --EXECUTE immediate sql_stmt INTO v_release,v_deploy;
 --
 -- Either load all the tables or only without STATEFP field
 --
 IF (load_statefp = 'TRUE')
  THEN
   -- 
   v_keys := '';
 END IF; 
 --
 IF (load_statefp = 'FALSE')
  THEN
   -- Set keys where not like 'STATEFP%'
   v_keys := 'and a.tbl_keys NOT like ''STATEFP%'''; 
 END IF; 
 --
 -- Obtain table_name, source_location, tbl_keys, seq_num, and tbl_pf from TOPO_UNIVERSE/TOPO_HIERARCHY
 sql_stmt := 'select /*+ PARALLEL 4 */ a.table_name,a.source_location,a.tbl_keys,b.seq_num,a.tbl_pf from '||topo_universe||' a, '||topo_hierarchy||' b where a.table_name like :1 and a.topology = :2 '||v_keys||' and b.child_layer_tbl is null and a.topology = b.topology and a.table_name = b.table_name order by b.seq_num';
 EXECUTE immediate sql_stmt bulk collect INTO array_tbl_name,array_source_location,array_tbl_keys,array_seq_num,array_tbl_pf USING 'SL%',topology;
 -- check if data exists
 IF array_seq_num.COUNT > 0 THEN
 -- loop thru each seq_num
 FOR i IN 1..array_seq_num.LAST
  LOOP
   v_tbl_name := topology||'_'||array_tbl_name(i);
   v_source_location := array_source_location(i);
   v_tbl_keys := array_tbl_keys(i);
   v_seq_num := array_seq_num(i);
   v_tbl_pf := array_tbl_pf(i);
   --
   sql_stmt := 'select count(*) from ' || v_source_location || '';
   EXECUTE immediate sql_stmt INTO tbl_record_cnt;
   -- 
   IF tbl_record_cnt > 0 
    THEN
     --
     -- get tbl_pf (table primary field) from source table (v_source_location)
     -- with or without OID column
     --
     sql_stmt := 'select count(*) from user_tab_columns where table_name = '''||v_source_location||''' and column_name = ''OID''';
     EXECUTE immediate sql_stmt INTO col_cnt;
     --
     IF col_cnt = 0
      THEN
       sql_stmt := 'select /*+ parallel 4 */ ' || TO_CHAR(v_tbl_pf) || ' from ' || v_source_location || ' order by ' || v_tbl_pf;
       EXECUTE immediate sql_stmt bulk collect INTO array_tbl_pf_loop;
     END IF;
     --
     IF col_cnt = 1
      THEN
       v_tbl_pf := 'OID';
       sql_stmt := 'select /*+ parallel 4 */ ' || TO_CHAR(v_tbl_pf) || ' from ' || v_source_location || ' order by ' || v_tbl_pf;
       EXECUTE immediate sql_stmt bulk collect INTO array_tbl_pf_loop;
     END IF;
     --
     -- loop through each key value for a particular table
     -- the purpose of lopping through each tbl_pf is to limit the size of the topology map in memory; given
     -- that there is not STATEFP a logical boundary, then each tbl_pf value was the logical alternative even
     -- though it is likely that the size of the topology map for a given tbl_fp value falls far below the maximum
     -- allowable size of the topo map
     --
     FOR j IN 1..array_tbl_pf_loop.LAST
     LOOP
      v_tbl_pf_loop := array_tbl_pf_loop(j);
      -- check to insure tbl_fp not already loaded
      sql_stmt := 'select count(*) from ' || v_tbl_name || ' WHERE ' || v_tbl_pf || ' = ''' || v_tbl_pf_loop || '''';
      EXECUTE immediate sql_stmt INTO v_cnt;
      -- if not loaded, then load
      IF v_cnt = 0 THEN
       --
       -- Get MBR for load_topo_map
       --
       -- tracking
       v_step := 'Get MBR for load_topo_map';
       v_start_time := systimestamp;
       -- get MBR
       sql_stmt := 'SELECT /*+ PARALLEL 4 */ sdo_geom.sdo_mbr(a.sdogeometry) FROM ' || v_source_location || ' a WHERE ' || v_tbl_pf || ' = ''' || v_tbl_pf_loop || ''' and rownum = 1';
       EXECUTE immediate sql_stmt INTO mbr_geom;
       -- split out ordinates
       v_ord1 := mbr_geom.sdo_ordinates(1);
       v_ord2 := mbr_geom.sdo_ordinates(2);
       v_ord3 := mbr_geom.sdo_ordinates(3);
       v_ord4 := mbr_geom.sdo_ordinates(4);
       -- mbr buffer (alpha) 
       v_ord1 := v_ord1 - alpha;
       v_ord2 := v_ord2 - alpha;
       v_ord3 := v_ord3 + alpha;
       v_ord4 := v_ord4 + alpha;
       -- tracking
       v_end_time := systimestamp;
       v_elapsed_time := v_end_time - v_start_time;
       GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
       --
       -- Load topo map
       --
       -- tracking
       v_step := 'Load Topo Map';
       v_start_time := systimestamp;
       -- load topo map
       sdo_topo_map.load_topo_map('MTMAP',v_ord1,v_ord2,v_ord3,v_ord4,'TRUE');
       -- tracking
       v_end_time := systimestamp;
       v_elapsed_time := v_end_time - v_start_time;
       GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
       --
       -- Load table
       --
       -- tracking
       v_step := 'Load feature table ' || v_tbl_name;
       -- get column names from source feature table; call function
       v_table_fields := create_ft_cols (v_source_location);
       -- create load_rec string; call function
       v_load_table_fields := create_ft_cols (v_tbl_name);
       -- append 'load_rec' to each column
       v_load_table_fields := 'load_rec.' || regexp_replace (v_load_table_fields,',',',load_rec.');
       -- tracking
       v_start_time := systimestamp;
       -- load tbl_pf using standard sdo_topo_map.create_feature call
       sql_stmt := 'Declare topo_geom sdo_topo_geometry; 
                    BEGIN FOR load_rec IN (SELECT /*+ PARALLEL 4 */ ' || v_table_fields || ' FROM ' || v_source_location || ' WHERE ' || v_tbl_pf || ' = ''' || v_tbl_pf_loop || ''')
                     LOOP
                        topo_geom := SDO_TOPO_MAP.CREATE_FEATURE(''' || topology || ''',''' || v_tbl_name || ''',''TOPOGEOM'',load_rec.sdogeometry,0);
                        INSERT /*+ APPEND */ INTO ' || v_tbl_name || ' VALUES (' || v_load_table_fields || ',topo_geom);
                     END LOOP; 
                    END;';
       EXECUTE immediate sql_stmt;
       -- tracking
       v_end_time := systimestamp;
       v_elapsed_time := v_end_time - v_start_time;
       GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
       COMMIT;
       --
       -- Commit TOPO map
       --
       -- tracking
       v_step := 'Commit Topo Map';
       v_start_time := systimestamp;
       -- commit TOPO map
       sdo_TOPO_MAP.COMMIT_TOPO_MAP();
       -- topology is now updated with new data
       -- COMMIT; -- not necessary
      END IF;
      -- tracking
      v_end_time := systimestamp;
      v_elapsed_time := v_end_time - v_start_time;
      GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
      --
      -- Clear TOPO map cache
      --
      -- tracking
      v_step := 'Clear Topo Map';
      v_start_time := systimestamp;
      -- clear topo map
      sdo_TOPO_MAP.CLEAR_TOPO_MAP('MTMAP');
      -- tracking
      v_end_time := systimestamp;
      v_elapsed_time := v_end_time - v_start_time;
      GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
     END LOOP;
   END IF;
  END LOOP;
  --
 END IF;
  --
  -- Drop TOPO map
  --
  -- tracking
  v_step       := 'Drop Topo Map';
  v_start_time := systimestamp;
  -- drop topo map
  sdo_TOPO_MAP.DROP_TOPO_MAP('MTMAP');
  -- tracking
  v_end_time     := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  -- end tracking
  v_proc_end_time     := systimestamp;
  v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_proc_step,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,v_release,'N/A',v_deploy);
  --
  -- generic exception handling
  --
EXCEPTION
WHEN OTHERS THEN
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  -- tracking
  v_process := v_process || ' FAILED';
  v_step := v_step || ' FAILED';
  GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
  --
  RAISE;
END load_topo_sl_0;
--
PROCEDURE load_topo_sl_0_statefp (topology VARCHAR2,tbl_type VARCHAR2,topo_universe VARCHAR2,topo_hierarchy VARCHAR2,state_tbl VARCHAR2,release VARCHAR2,deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: load_topo_sl_0_statefp
 # Author: mz
 # Creation Date: 05/15/2009
 # Recent Revisions: 06/06/2009; 06/08/2009; 07/15/2009
 #
 # Purpose:
 #   The purpose of this procedure is to load the tg_level_id = 0 feature tables that contain STATEFP
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #  - TOPO_HIERARCHY
 #
 ###################################################################################################################
*/
v_process VARCHAR2(100) := 'Load SL Feature Tables that have a STATEFP field'; -- Process
v_step VARCHAR2(4000); -- Step
v_start_time TIMESTAMP; -- Start time
v_end_time TIMESTAMP; -- End time
v_elapsed_time interval DAY(5) TO second (2); -- Elapsed time
v_proc_start_time TIMESTAMP; -- Procedure Start Time
v_proc_step VARCHAR2(4000); -- Procedure Step
v_proc_end_time TIMESTAMP; -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2); -- Procedure Elapsed time
array_tbl_name mdsys.string_array;
v_tbl_name VARCHAR2(32);
array_source_location mdsys.string_array;
v_source_location VARCHAR2(250);
array_tbl_keys mdsys.string_array;
v_tbl_keys VARCHAR2(250);
v_deploy VARCHAR2(250) := NULL;
v_release VARCHAR2(250) := NULL;
array_seq_num mdsys.sdo_number_array;
v_seq_num NUMBER;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp VARCHAR2(30);
statefp_exists  NUMBER;
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
v_cnt NUMBER;
v_cnt2 NUMBER;
v_cnt3 NUMBER;
v_table_fields VARCHAR2(4000);
v_load_table_fields VARCHAR2(4000);
v_ord1 NUMBER; -- MBR ordinate 1 for STATE
v_ord2 NUMBER; -- MBR ordinate 2 for STATE
v_ord3 NUMBER; -- MBR ordinate 3 for STATE
v_ord4 NUMBER; -- MBR ordinate 4 for STATE
array_state mdsys.string_array; -- STATEFPs
v_state VARCHAR2(2);
v_state_count NUMBER; -- Used to determine whether a particular STATEFP
v_state_data VARCHAR2(2); -- is loaded thus allowing restart capability
v_statefp_exists NUMBER;
v_where_clause VARCHAR2(30); -- used so insert command doesn't have to be changed
mbr_geom SDO_GEOMETRY; -- minimum bond rectangle
statefp_tbl VARCHAR2(32);
v_statefp_tbl VARCHAR2(32);
statefp_colname VARCHAR2(32);
BEGIN
 -- this load technique creates topogeom for all the tables with STATEFP 'xx' as part of a single
 -- map commit; then the next STATEFP is selected and topogeom is created for all tables containing
 -- the next STATEFP, etc.
 --
 -- set variables
 v_release := release;
 v_deploy := deploy;
 --
 -- get all the SL tables that have STATEFP columns
 sql_stmt := 'select a.table_name,a.source_location,a.tbl_keys,b.seq_num from '||topo_universe||' a, '||topo_hierarchy||' b where a.table_name like :1 and a.table_type = :2 and a.topology = :3 and a.tbl_keys like :4 and b.child_layer_tbl is null and a.topology = b.topology and a.table_name = b.table_name order by b.seq_num';
 EXECUTE immediate sql_stmt bulk collect INTO array_tbl_name,array_source_location,array_tbl_keys,array_seq_num USING 'SL%',tbl_type,topology,'STATEFP%';
 --
 -- Set sdo_topo_map maximum Java memory size
 sdo_topo_map.set_max_memory_size (2147483648);
 --
 -- Create topo map
 -- tracking
 v_proc_start_time := systimestamp;
 v_proc_step := v_process;
 v_step := 'Create Topo Map';
 v_start_time := systimestamp;
 --
 sdo_topo_map.create_topo_map(topology,'MTMAP',8000,4000,1500);
 --
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
 --
 -- set STATEFP table name
 v_statefp_tbl := state_tbl;
 --
 -- see if it exists; will error out if table not found
 sql_stmt := 'select /*+ PARALLEL 4 */ table_name from user_tables where table_name = :1';
 EXECUTE immediate sql_stmt INTO statefp_tbl USING v_statefp_tbl;
 -- 
 -- get the STATEFP data
 sql_stmt := 'select /*+ PARALLEL 4 */ statefp from ' || statefp_tbl || ' order by statefp';
 EXECUTE immediate sql_stmt bulk collect INTO array_state;
 --
 -- process each statefp
 FOR i IN 1..array_state.LAST
 LOOP
  -- select STATEFP
  v_state_data := array_state(i);
  -- Get MBR for load_topo_map
  v_step := 'Get MBR for load_topo_map';
  v_start_time := systimestamp;
  -- State MBRs and exceptions
  sql_stmt := 'select count(*) from mbr_exceptions where release = :1 and statefp = :2';
  EXECUTE immediate sql_stmt INTO v_cnt3 USING v_release, v_state_data;
  IF v_cnt3 = 1 
   THEN
    -- use statefp mbr exceptions if they exist
    sql_stmt := 'select ord1,ord2,ord3,ord4 from mbr_exceptions where release = :1 and statefp = :2';
    EXECUTE immediate sql_stmt INTO v_ord1,v_ord2,v_ord3,v_ord4 USING v_release,v_state_data;
   ELSE
    -- use normal statefp MBRs
    sql_stmt := 'SELECT /*+ PARALLEL 4 */ sdo_geom.sdo_mbr(a.sdogeometry) FROM ' || statefp_tbl || ' a WHERE a.statefp = ''' || v_state_data || ''' and rownum = 1';
    EXECUTE immediate sql_stmt INTO mbr_geom;
    v_ord1 := mbr_geom.sdo_ordinates(1);
    v_ord2 := mbr_geom.sdo_ordinates(2);
    v_ord3 := mbr_geom.sdo_ordinates(3);
    v_ord4 := mbr_geom.sdo_ordinates(4);
  END IF;
  --
  v_cnt3 := NULL;
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  -- Load topo map
  v_step := 'Load Topo Map';
  v_start_time := systimestamp;
  --
  sdo_topo_map.load_topo_map('MTMAP',v_ord1,v_ord2,v_ord3,v_ord4,'TRUE');
  --
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  -- Load table
  FOR i IN 1..array_seq_num.LAST
  LOOP
   v_tbl_name := topology||'_'||array_tbl_name(i);
   v_source_location := array_source_location(i);
   v_tbl_keys := array_tbl_keys(i);
   v_seq_num := array_seq_num(i);
   -- set statefp column name, either statefp or statefp_sl795
   IF (v_tbl_name = topology||'_SL795')
    THEN
     statefp_colname := 'statefp_sl795';
     v_where_clause := 'STATEFP_SL795 IS NOT NULL'; -- used so insert command doesn't have to be changed
    ELSE
     statefp_colname := 'statefp';
     v_where_clause := 'STATEFP IS NOT NULL'; -- used so insert command doesn't have to be changed
   END IF;
   -- gen_tracking 
   v_step := 'Load feature table ' || v_tbl_name;
   -- check to see if the source table has the specific STATEFP
   sql_stmt := 'select count(rownum) from ' || v_source_location || ' where '||statefp_colname||' = ''' || v_state_data || '''';
   EXECUTE immediate sql_stmt INTO v_cnt;
   IF v_cnt > 0 -- specific STATEFP exists in source table; if = 0 then it skips the load for the particular STATEFP
    THEN
     -- check if specific STATEFP is already loaded into feature table; ability to re-start an aborted load
     sql_stmt := 'select count(rownum) from ' || v_tbl_name || ' where '||statefp_colname||' = ''' || v_state_data || '''';
     EXECUTE immediate sql_stmt INTO v_cnt2;
     --
     IF v_cnt2 = 0 THEN
      -- get column names from source feature table
      v_table_fields := create_ft_cols (v_source_location);
      -- create load_rec string
      v_load_table_fields := create_ft_cols (v_tbl_name);
      v_load_table_fields := 'load_rec.' || regexp_replace (v_load_table_fields,',',',load_rec.');
      v_start_time := systimestamp;
      --
      -- create feature
      sql_stmt := 'Declare topo_geom sdo_topo_geometry; 
                   BEGIN FOR load_rec IN (SELECT /*+ PARALLEL 4 */ ' || v_table_fields || ' FROM ' || v_source_location || ' where '||statefp_colname||' = ''' || v_state_data || ''' and ' || v_where_clause || ')
                    LOOP
                     topo_geom := SDO_TOPO_MAP.CREATE_FEATURE(''' || topology || ''',''' || v_tbl_name || ''',''TOPOGEOM'',load_rec.sdogeometry,0);
                     INSERT /*+ APPEND */ INTO ' || v_tbl_name || ' VALUES (' || v_load_table_fields || ',topo_geom);
                    END LOOP; 
                   END;';
      EXECUTE immediate sql_stmt;
      --
      v_end_time := systimestamp;
      v_elapsed_time := v_end_time - v_start_time;
      GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
      COMMIT;
     END IF;
   END IF;
  END LOOP;
  --
  -- Commit TOPO map
  --
  -- commit topo map
  v_step := 'Commit Topo Map';
  v_start_time := systimestamp;
  --
  sdo_TOPO_MAP.COMMIT_TOPO_MAP();
  -- commit; -- likely redundant
  --
  -- tracking
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  --
  -- Clear TOPO map cache
  --
  -- tracking
  v_step := 'Clear Topo Map';
  v_start_time := systimestamp;
  --
  sdo_TOPO_MAP.CLEAR_TOPO_MAP('MTMAP');
  --
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  COMMIT;
 END LOOP;
 --
 -- Drop TOPO map
 --
 -- tracking
 v_step := 'Drop Topo Map';
 v_start_time := systimestamp;
 -- drop topo map
 sdo_TOPO_MAP.DROP_TOPO_MAP('MTMAP');
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
 --
 -- end tracking
 --
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_proc_step,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,v_release,'N/A',v_deploy);
 --
 -- generic exception handling
 --
 EXCEPTION
 WHEN OTHERS THEN
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  -- tracking
  v_process := v_process || ' FAILED';
  v_step    := v_step || ' FAILED';
  GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
  --
  RAISE;
END load_topo_sl_0_statefp;
--
PROCEDURE load_topo_sl_1_up (topology VARCHAR2, sl_type VARCHAR2, topo_universe VARCHAR2,topo_hierarchy VARCHAR2, release VARCHAR2, deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: load_topo_level_1_up
 # Author: mz
 # Creation Date: 06/24/2009
 # Recent Revisions:
 #
 # Purpose:
 #  The purpose of this procedure is to load the hiearchical feature tables, those feature tables
 #   registered with tg_layer_level = 1 and higher
 #
 #  A DML_CONDITION is created which aggregates the data from the child layer based on one or more table_keys
 #
 # Required parameters:
 #  - topology - Topology Name
 #  - sl_type - Summary Level Type - SL or FSL
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #  - TOPO_HIERARCHY
 #
 ###################################################################################################################
*/
v_process VARCHAR2(100) := 'Load Hierarchical Summary Level Feature Tables'; -- Process
v_step VARCHAR2(4000); -- Step
v_start_time TIMESTAMP; -- Start time
v_end_time TIMESTAMP; -- End time
v_elapsed_time interval DAY(5) TO second (2); -- Elapsed time
v_proc_start_time TIMESTAMP; -- Procedure Start Time
v_proc_step VARCHAR2(4000); -- Procedure Step
v_proc_end_time TIMESTAMP; -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2); -- Procedure Elapsed time
array_tbl_name mdsys.string_array;
v_tbl_name VARCHAR2(32);
tbl_record_cnt NUMBER; -- table record count
feature_tbl_cnt NUMBER; -- table feature count
array_source_location mdsys.string_array;
v_source_location VARCHAR2(250);
array_tbl_keys mdsys.string_array;
v_tbl_keys VARCHAR2(250);
array_deploy mdsys.string_array;
v_deploy VARCHAR2(250);
array_release mdsys.string_array;
v_release VARCHAR2(250) := NULL;
array_seq_num mdsys.sdo_number_array;
v_seq_num NUMBER;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp VARCHAR2(30);
statefp_exists NUMBER;
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
v_cnt NUMBER;
v_table_fields VARCHAR2(4000);
v_load_table_fields VARCHAR2(4000);
v_where_clause VARCHAR2(4000);
v_where_clause_full VARCHAR2(4000);
v_dml_condition VARCHAR2(4000);
mbr_geom SDO_GEOMETRY; -- minimum bond rectangle
statefp_tbl VARCHAR2(32);
v_statefp_tbl VARCHAR2(32);
v_tbl_keys2 VARCHAR2(1000);
array_key_data mdsys.string_array;
v_sl_type VARCHAR2(10); -- type of hierarchical feature table - SL or FSL
v_tbl_keys_data VARCHAR2(4000);
comma_cnt NUMBER;
v_len_keys VARCHAR2(4000);
BEGIN
 --
 -- tracking
 v_proc_start_time := systimestamp;
 v_proc_step := v_process;
 --
 -- get relevant information from TOPO_UNIVERSE and TOPO_HIERARCHY
 --
 -- set sl_type to query with a 'like' where clause
 v_sl_type := sl_type || '%';
 -- get relevant topo_universe and topo_hierarchy
 sql_stmt := 'select a.table_name,a.source_location,a.tbl_keys,a.deploy,a.release,b.seq_num from '||topo_universe||' a, '||topo_hierarchy||' b where a.table_name like :1 and a.table_type in (:2,:3) and a.topology = :4 and b.child_layer_tbl is not null and a.topology = b.topology and a.table_name = b.table_name order by b.seq_num';
 execute immediate sql_stmt bulk collect into array_tbl_name,array_source_location,array_tbl_keys,array_deploy,array_release,array_seq_num using v_sl_type,'F','DF',topology;
 -- check if data exists
 IF array_seq_num.COUNT > 0 THEN
 --
 FOR i IN 1..array_seq_num.LAST
  LOOP
   v_tbl_name := topology||'_'||array_tbl_name(i);
   v_source_location := array_source_location(i);
   v_tbl_keys := array_tbl_keys(i);
   v_deploy := array_deploy(i);
   v_release := array_release(i);
   v_seq_num := array_seq_num(i);
   --
   -- Set record counter = 0
   --
   tbl_record_cnt := 0;
   feature_tbl_cnt := 0;
   --
   -- Check if Topo_Universe has souce_location populated
   --
   IF (v_source_location is not null)
    THEN
     sql_stmt := 'select count(*) from ' || v_source_location || '';
     EXECUTE immediate sql_stmt INTO tbl_record_cnt;
     --
     sql_stmt := 'select count(*) from ' || v_tbl_name || '';
     EXECUTE immediate sql_stmt INTO feature_tbl_cnt;
   END IF;
   --
   -- if records exist
   -- 
   IF tbl_record_cnt > 0 AND feature_tbl_cnt = 0
    THEN
    --
    -- Load table
    --
    v_start_time := systimestamp;
    v_step := 'Load feature table ' || v_tbl_name;
    -- get column names from source feature table
    v_table_fields := create_ft_cols (v_source_location);
    --
    -- create load_rec string
    --
    v_load_table_fields := create_ft_cols (v_tbl_name);
    v_load_table_fields := 'load_rec.' || regexp_replace (v_load_table_fields,',',',load_rec.');
    v_start_time := systimestamp;
    --
    -- get table keys
    --
    comma_cnt := count_occurs (array_tbl_keys(i),',');
    -- one table key
    IF comma_cnt = 0 THEN
     v_tbl_keys2 := array_tbl_keys(i);
     sql_stmt := 'select distinct ' || v_tbl_keys2 || ' from ' || v_source_location || ' order by ' || v_tbl_keys2;
     EXECUTE immediate sql_stmt bulk collect INTO array_key_data;
    END IF;
    -- two or more table keys
    IF comma_cnt > 0 THEN
     v_tbl_keys2 := REPLACE(v_tbl_keys,',',' || ''|'' || ');
     sql_stmt := 'select distinct ' || v_tbl_keys2 || ' from ' || v_source_location || ' order by ' || v_tbl_keys2;
     EXECUTE immediate sql_stmt bulk collect INTO array_key_data;
    END IF;
    --
    FOR i IN 1..array_key_data.LAST
     LOOP
      v_tbl_keys_data := REPLACE(array_key_data(i),'|',',');
      --
      -- get dml_condition
      --
      v_dml_condition := create_dml_condition (v_tbl_keys,v_tbl_keys_data);
      --
      -- create source table where clause
      --
      SELECT LENGTH(v_dml_condition) INTO v_len_keys FROM dual;
      SELECT SUBSTR(v_dml_condition,2,v_len_keys) INTO v_where_clause FROM dual;
      SELECT SUBSTR(v_where_clause,1,v_len_keys-2) INTO v_where_clause_full FROM dual;
      SELECT REPLACE(v_where_clause_full,'''''','''') INTO v_where_clause_full FROM dual;
      --
      -- Load table
      --
      sql_stmt := 'BEGIN FOR load_rec IN (SELECT /*+ PARALLEL 4 */ ' || v_table_fields || ' FROM ' || v_source_location || ' where ' || v_where_clause_full || ')
                    LOOP
                     INSERT /*+ APPEND */ INTO ' || v_tbl_name || ' VALUES (' || v_load_table_fields || ',
                     SDO_TOPO_MAP.CREATE_FEATURE(''' || topology || ''',''' || v_tbl_name || ''',''TOPOGEOM'',' || v_dml_condition || '));
                    END LOOP; 
                   END;';
      EXECUTE immediate sql_stmt;
      COMMIT;
      --
     END LOOP;
    --
    v_end_time := systimestamp;
    v_elapsed_time := v_end_time - v_start_time;
    GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
    COMMIT;
   END IF;
  END LOOP;
  --
  v_proc_end_time := systimestamp;
  v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A', v_proc_step,'COMPLETED', v_proc_start_time,v_proc_end_time, v_proc_elapsed_time, v_release, 'N/A', v_deploy);
  --
 END IF;
  --
  -- generic exception handling
  --
 EXCEPTION
 WHEN OTHERS THEN
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  -- tracking
  v_process := v_process || ' FAILED';
  v_step := v_step || ' FAILED';
  GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
  -- RETURN;
  RAISE;
END load_topo_sl_1_up;
--
PROCEDURE load_topo_fsl_state (topology VARCHAR2, sl_type VARCHAR2, topo_universe VARCHAR2, topo_hierarchy VARCHAR2, release VARCHAR2, deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: load_topo_fsl_state (State Based FSLs) 
 # Author: Salman Mansoor 
 # Creation Date: 08/11/2010
 # Recent Revisions:
 #
 # Purpose:
 #  The purpose of this procedure is to load the hiearchical feature tables, those feature tables
 #   registered with tg_layer_level = 1 and higher
 #
 #  A DML_CONDITION is created which aggregates the data from the child layer based on one or more table_keys
 #
 # Required parameters:
 #  - topology - Topology Name
 #  - sl_type - Summary Level Type - FSL
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #  - TOPO_HIERARCHY
 #
 ###################################################################################################################
*/
v_process VARCHAR2(100) := 'Load Hierarchical Summary Level Feature Tables'; -- Process
v_step VARCHAR2(4000); -- Step
v_start_time TIMESTAMP; -- Start time
v_end_time TIMESTAMP; -- End time
v_elapsed_time interval DAY(5) TO second (2); -- Elapsed time
v_proc_start_time TIMESTAMP; -- Procedure Start Time
v_proc_step VARCHAR2(4000); -- Procedure Step
v_proc_end_time TIMESTAMP; -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2); -- Procedure Elapsed time
array_tbl_name mdsys.string_array;
v_tbl_name VARCHAR2(32);
array_source_location mdsys.string_array;
tbl_record_cnt NUMBER; -- table record count
feature_tbl_cnt NUMBER; -- table feature count
v_source_location VARCHAR2(250);
array_tbl_keys mdsys.string_array;
v_tbl_keys VARCHAR2(250);
array_deploy mdsys.string_array;
v_deploy VARCHAR2(250);
array_release mdsys.string_array;
v_release VARCHAR2(250) := NULL;
array_seq_num mdsys.sdo_number_array;
v_seq_num NUMBER;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp VARCHAR2(30);
statefp_exists NUMBER;
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
v_cnt NUMBER;
v_table_fields VARCHAR2(4000);
v_load_table_fields VARCHAR2(4000);
v_where_clause VARCHAR2(4000);
v_where_clause_full VARCHAR2(4000);
v_dml_condition VARCHAR2(4000);
mbr_geom SDO_GEOMETRY; -- minimum bond rectangle
statefp_tbl VARCHAR2(32);
v_statefp_tbl VARCHAR2(32);
v_tbl_keys2 VARCHAR2(1000);
array_key_data mdsys.string_array;
v_sl_type VARCHAR2(25); -- type of hierarchical feature table - SL or FSL
v_tbl_keys_data VARCHAR2(4000);
comma_cnt NUMBER;
v_len_keys VARCHAR2(4000);
array_face_tbl mdsys.string_array;
v_face_tbl VARCHAR2(250);
BEGIN
 --
 -- tracking
 v_proc_start_time := systimestamp;
 v_proc_step := v_process;
 --
 -- get relevant information from TOPO_UNIVERSE and TOPO_HIERARCHY
 --
 -- set sl_type to query with a 'like' where clause
 v_sl_type := sl_type || '%';
 -- get relevant topo_universe and topo_hierarchy
 sql_stmt := 'select a.table_name,a.source_location,a.tbl_keys,a.deploy,a.release,b.child_layer_tbl,b.seq_num from '||topo_universe||' a, '||topo_hierarchy||' b where a.table_name like :1 and a.table_type in (:2,:3) and a.topology = :4 and b.child_layer_tbl is not null and a.topology = b.topology and a.table_name = b.table_name order by b.seq_num';
 execute immediate sql_stmt bulk collect into array_tbl_name,array_source_location,array_tbl_keys,array_deploy,array_release,array_face_tbl,array_seq_num using v_sl_type,'F','DF',topology;
 -- check if data exists
 IF array_seq_num.COUNT > 0 THEN
 --
 FOR i IN 1..array_seq_num.LAST
  LOOP
   v_tbl_name := topology || '_' || array_tbl_name(i);
   v_source_location := array_source_location(i);
   v_tbl_keys := array_tbl_keys(i);
   v_deploy := array_deploy(i);
   v_release := array_release(i);
   v_face_tbl := topology || '_' || array_face_tbl(i);
   v_seq_num := array_seq_num(i);
   --
   -- Set record counter = 0
   --
   tbl_record_cnt := 0;
   feature_tbl_cnt := 0;
   --
   -- Check if Topo_Universe has souce_location populated
   --
   IF (v_source_location is not null)
    THEN
     sql_stmt := 'select count(*) from ' || v_source_location || '';
     EXECUTE immediate sql_stmt INTO tbl_record_cnt;
     --
     sql_stmt := 'select count(*) from ' || v_tbl_name || '';
     EXECUTE immediate sql_stmt INTO feature_tbl_cnt;
   END IF;
   --
   -- if records exist
   -- 
   IF tbl_record_cnt > 0 AND feature_tbl_cnt = 0
    THEN
    --
    -- Load table
    --
    v_start_time := systimestamp;
    v_step := 'Load feature table ' || v_tbl_name;
    -- get column names from source feature table
    v_table_fields := create_ft_cols (v_source_location);
    --
    -- create load_rec string
    --
    v_load_table_fields := create_ft_cols (v_tbl_name);
    v_load_table_fields := 'load_rec.' || regexp_replace (v_load_table_fields,',',',load_rec.');
    v_start_time := systimestamp;
    --
    -- get table keys
    --
    comma_cnt := count_occurs (array_tbl_keys(i),',');
    -- one table key
    IF comma_cnt = 0 THEN
     v_tbl_keys2 := array_tbl_keys(i);
     sql_stmt := 'select distinct ' || v_tbl_keys2 || ' from ' || v_face_tbl || ' order by ' || v_tbl_keys2;
     EXECUTE immediate sql_stmt bulk collect INTO array_key_data;
    END IF;
    -- two or more table keys
    IF comma_cnt > 0 THEN
     v_tbl_keys2 := REPLACE(v_tbl_keys,',',' || ''|'' || ');
     sql_stmt := 'select distinct ' || v_tbl_keys2 || ' from ' || v_face_tbl || ' order by ' || v_tbl_keys2;
     EXECUTE immediate sql_stmt bulk collect INTO array_key_data;
    END IF;
    --
    FOR i IN 1..array_key_data.LAST
     LOOP
      v_tbl_keys_data := REPLACE(array_key_data(i),'|',',');
      --
      -- get dml_condition
      --
      v_dml_condition := create_dml_condition (v_tbl_keys,v_tbl_keys_data);
      --dbms_output.put_line ('dml_statement ... ' || v_dml_condition);
      --
      -- create source table where clause
      --
      SELECT LENGTH(v_dml_condition) INTO v_len_keys FROM dual;
      SELECT SUBSTR(v_dml_condition,2,v_len_keys) INTO v_where_clause FROM dual;
      SELECT SUBSTR(v_where_clause,1,v_len_keys-2) INTO v_where_clause_full FROM dual;
      SELECT REPLACE(v_where_clause_full,'''''','''') INTO v_where_clause_full FROM dual;
      --
      -- Load table
      --
      sql_stmt := 'BEGIN FOR load_rec IN (SELECT /*+ PARALLEL 4 */ ' || v_table_fields || ' FROM ' || v_source_location || ' where ' || v_where_clause_full || ')
                    LOOP
                     INSERT /*+ APPEND */ INTO ' || v_tbl_name || ' VALUES (' || v_load_table_fields || ',
                     SDO_TOPO_MAP.CREATE_FEATURE(''' || topology || ''',''' || v_tbl_name || ''',''TOPOGEOM'',' || v_dml_condition || '));
                    END LOOP; 
                   END;';
      EXECUTE immediate sql_stmt;
      COMMIT;
      --
     END LOOP;
    --
    v_end_time := systimestamp;
    v_elapsed_time := v_end_time - v_start_time;
    GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
    COMMIT;
   END IF;
  END LOOP;
  --
  v_proc_end_time := systimestamp;
  v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A', v_proc_step,'COMPLETED', v_proc_start_time,v_proc_end_time, v_proc_elapsed_time, v_release, 'N/A', v_deploy);
  --
 END IF;
  --
  -- generic exception handling
  --
 EXCEPTION
 WHEN OTHERS THEN
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  -- tracking
  v_process := v_process || ' FAILED';
  v_step := v_step || ' FAILED';
  GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
  -- RETURN;
  RAISE;
END load_topo_fsl_state;
--
PROCEDURE add_poly_sl_0 (topology VARCHAR2, topo_universe VARCHAR2, topo_hierarchy VARCHAR2, release VARCHAR2, deploy VARCHAR2, load_statefp VARCHAR2 DEFAULT 'FALSE')
AS
/**
 ###################################################################################################################
 # Program Name: add_poly_sl_0
 # Author: Salman Mansoor
 # Creation Date: 06/09/2011
 # Recent Revisions:
 #
 # Purpose:
 #   The purpose of this procedure is load the tg_level_id = 0 feature tables
 #    that do NOT contain a STATEFP field
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #  - TOPO_HIERARCHY
 #
 ###################################################################################################################
*/
v_process VARCHAR2(100) := 'Add Polygon - SL Feature Tables that do NOT contain a STATEFP field'; -- Process
v_step VARCHAR2(4000); -- Step
v_start_time TIMESTAMP; -- Start time
v_end_time TIMESTAMP; -- End time
v_elapsed_time interval DAY(5) TO second (2); -- Elapsed time
v_proc_start_time TIMESTAMP; -- Procedure Start Time
v_proc_step VARCHAR2(4000); -- Procedure Step
v_proc_end_time TIMESTAMP; -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2); -- Procedure Elapsed time
--
tbl_record_cnt NUMBER; -- table record count
array_source_location mdsys.string_array;
v_source_location VARCHAR2(250);
array_seq_num mdsys.sdo_number_array;
v_seq_num NUMBER;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp VARCHAR2(30);
v_keys VARCHAR2(50);
statefp_exists NUMBER;
v_release VARCHAR2(30) := NULL;
v_deploy VARCHAR2(30) := NULL;
--
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
col_cnt NUMBER;
v_cnt NUMBER;
--
v_ord1 NUMBER; -- MBR ordinate 1
v_ord2 NUMBER; -- MBR ordinate 2
v_ord3 NUMBER; -- MBR ordinate 3
v_ord4 NUMBER; -- MBR ordinate 4
alpha  NUMBER(4,3) := 0.001; -- MBR Buffer 
--
v_table_fields VARCHAR2(4000) := 'SDOGEOMETRY';
--
mbr_geom SDO_GEOMETRY; -- minimum bound rectangle
--
-- array to hold tbl_fp - table primary field (not related to primary key)
-- one option is to use the OID as the MBR for the topo map but generally
-- this is too small a MBR
array_tbl_pf mdsys.string_array;
v_tbl_pf VARCHAR2(32);                -- table primary field
array_tbl_pf_loop mdsys.string_array; -- stores values of tbl_pf
v_tbl_pf_loop VARCHAR2(32);           -- tbl_fp value
--
cntr NUMBER; -- counter
BEGIN
 -- Set sdo_topo_map maximum Java memory size
 sdo_topo_map.set_max_memory_size (2147483648);
 -- set variables
 v_release := release;
 v_deploy := deploy;
 --
 -- Create topo map
 --
 -- tracking
 v_proc_start_time := systimestamp;
 v_proc_step := v_process;
 v_step := 'Create Topo Map';
 v_start_time := systimestamp;
 -- create topo map
 sdo_topo_map.create_topo_map(topology,'MTMAP',8000,4000,1500);
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
 --
 -- Either load all the tables or only without STATEFP field
 --
 IF (load_statefp = 'TRUE')
  THEN
   -- 
   v_keys := '';
 END IF; 
 --
 IF (load_statefp = 'FALSE')
  THEN
   -- Set keys where not like 'STATEFP%'
   v_keys := 'and a.tbl_keys NOT like ''STATEFP%'''; 
 END IF; 
 --
 -- Obtain release and deploy info
 -- set variables
 v_release := release;
 v_deploy := deploy;
 -- Obtain table_name, source_location, tbl_keys, seq_num, and tbl_pf from TOPO_UNIVERSE/TOPO_HIERARCHY
 sql_stmt := 'select /*+ PARALLEL 4 */ a.source_location,b.seq_num,a.tbl_pf from '||topo_universe||' a, '||topo_hierarchy||' b where a.table_name like :1 and a.topology = :2 '||v_keys||' and b.child_layer_tbl is null and a.topology = b.topology and a.table_name = b.table_name order by b.seq_num';
 EXECUTE immediate sql_stmt bulk collect INTO array_source_location,array_seq_num,array_tbl_pf USING 'SL%',topology;
 -- check if data exists
 IF array_seq_num.COUNT > 0 THEN
 -- loop thru each seq_num
 FOR i IN 1..array_seq_num.LAST
  LOOP
   v_source_location := array_source_location(i);
   v_seq_num := array_seq_num(i);
   v_tbl_pf := array_tbl_pf(i);
   --
   sql_stmt := 'select count(*) from ' || v_source_location || '';
   EXECUTE immediate sql_stmt INTO tbl_record_cnt;
   -- 
   IF tbl_record_cnt > 0 
    THEN
     --
     -- get tbl_pf (table primary field) from source table (v_source_location)
     -- with or without OID column
     --
     sql_stmt := 'select count(*) from user_tab_columns where table_name = '''||v_source_location||''' and column_name = ''OID''';
     EXECUTE immediate sql_stmt INTO col_cnt;
     --
     IF col_cnt = 0
      THEN
       sql_stmt := 'select /*+ parallel 4 */ ' || TO_CHAR(v_tbl_pf) || ' from ' || v_source_location || ' order by ' || v_tbl_pf;
       EXECUTE immediate sql_stmt bulk collect INTO array_tbl_pf_loop;
     END IF;
     --
     IF col_cnt = 1
      THEN
       v_tbl_pf := 'OID';
       sql_stmt := 'select /*+ parallel 4 */ ' || TO_CHAR(v_tbl_pf) || ' from ' || v_source_location || ' order by ' || v_tbl_pf;
       EXECUTE immediate sql_stmt bulk collect INTO array_tbl_pf_loop;
     END IF;
     --
     -- loop through each key value for a particular table
     -- the purpose of lopping through each tbl_pf is to limit the size of the topology map in memory; given
     -- that there is not STATEFP a logical boundary, then each tbl_pf value was the logical alternative even
     -- though it is likely that the size of the topology map for a given tbl_fp value falls far below the maximum
     -- allowable size of the topo map
     --
     FOR j IN 1..array_tbl_pf_loop.LAST
     LOOP
      v_tbl_pf_loop := array_tbl_pf_loop(j);
      --
      -- Get MBR for load_topo_map
      --
      -- tracking
      v_step := 'Get MBR for load_topo_map';
      v_start_time := systimestamp;
      -- get MBR
      sql_stmt := 'SELECT /*+ PARALLEL 4 */ sdo_geom.sdo_mbr(a.sdogeometry) FROM ' || v_source_location || ' a WHERE ' || v_tbl_pf || ' = ''' || v_tbl_pf_loop || ''' and rownum = 1';
      EXECUTE immediate sql_stmt INTO mbr_geom;
      -- split out ordinates
      v_ord1 := mbr_geom.sdo_ordinates(1);
      v_ord2 := mbr_geom.sdo_ordinates(2);
      v_ord3 := mbr_geom.sdo_ordinates(3);
      v_ord4 := mbr_geom.sdo_ordinates(4);
      -- mbr buffer (alpha) 
      v_ord1 := v_ord1 - alpha;
      v_ord2 := v_ord2 - alpha;
      v_ord3 := v_ord3 + alpha;
      v_ord4 := v_ord4 + alpha;
      -- tracking
      v_end_time := systimestamp;
      v_elapsed_time := v_end_time - v_start_time;
      GZ_TOPO_BUILD.gen_tracking_log(topology,v_source_location,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
      --
      -- Load topo map
      --
      -- tracking
      v_step := 'Load Topo Map';
      v_start_time := systimestamp;
      -- load topo map
      sdo_topo_map.load_topo_map('MTMAP',v_ord1,v_ord2,v_ord3,v_ord4,'TRUE');
      -- tracking
      v_end_time := systimestamp;
      v_elapsed_time := v_end_time - v_start_time;
      GZ_TOPO_BUILD.gen_tracking_log(topology,v_source_location,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
      --
      -- Add Polygon Geometry 
      --
      -- tracking
      v_step := 'Add polygon feature table ' || v_source_location;
      v_start_time := systimestamp;
      -- load tbl_pf using standard sdo_topo_map.create_feature call
      sql_stmt := 'Declare 
                     farray mdsys.sdo_number_array; 
                   BEGIN 
                     FOR load_rec IN (SELECT /*+ PARALLEL 4 */ ' || v_table_fields || ' FROM ' || v_source_location || ' WHERE ' || v_tbl_pf || ' = ''' || v_tbl_pf_loop || ''')
                      LOOP
                        farray := SDO_TOPO_MAP.ADD_POLYGON_GEOMETRY(null, load_rec.sdogeometry);
                     END LOOP; 
                   END;';
      EXECUTE immediate sql_stmt;
      -- tracking
      v_end_time := systimestamp;
      v_elapsed_time := v_end_time - v_start_time;
      GZ_TOPO_BUILD.gen_tracking_log(topology,v_source_location,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
      COMMIT;
      --
      -- Commit TOPO map
      --
      -- tracking
      v_step := 'Commit Topo Map';
      v_start_time := systimestamp;
      -- commit TOPO map
      sdo_TOPO_MAP.COMMIT_TOPO_MAP();
      -- topology is now updated with new data
      -- COMMIT; -- not necessary
      
      -- tracking
      v_end_time := systimestamp;
      v_elapsed_time := v_end_time - v_start_time;
      GZ_TOPO_BUILD.gen_tracking_log(topology,v_source_location,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
      --
      -- Clear TOPO map cache
      --
      -- tracking
      v_step := 'Clear Topo Map';
      v_start_time := systimestamp;
      -- clear topo map
      sdo_TOPO_MAP.CLEAR_TOPO_MAP('MTMAP');
      -- tracking
      v_end_time := systimestamp;
      v_elapsed_time := v_end_time - v_start_time;
      GZ_TOPO_BUILD.gen_tracking_log(topology,v_source_location,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
      COMMIT;
     END LOOP;
   END IF;
  END LOOP;
  --
 END IF;
  --
  -- Drop TOPO map
  --
  -- tracking
  v_step       := 'Drop Topo Map';
  v_start_time := systimestamp;
  -- drop topo map
  sdo_TOPO_MAP.DROP_TOPO_MAP('MTMAP');
  -- tracking
  v_end_time     := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,v_source_location,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  -- end tracking
  v_proc_end_time     := systimestamp;
  v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_proc_step,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,v_release,'N/A',v_deploy);
  --
  -- generic exception handling
  --
EXCEPTION
WHEN OTHERS THEN
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  -- tracking
  v_process := v_process || ' FAILED';
  v_step := v_step || ' FAILED';
  GZ_TOPO_BUILD.gen_tracking_log(topology,v_source_location,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
  --
  RAISE;
END add_poly_sl_0;
--
PROCEDURE add_poly_sl_0_statefp (topology VARCHAR2,tbl_type VARCHAR2,topo_universe VARCHAR2,topo_hierarchy VARCHAR2,state_tbl VARCHAR2,release VARCHAR2,deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: add_poly_sl_0_statefp
 # Author: Salman Mansoor
 # Creation Date: 05/15/2011
 # Recent Revisions: 
 #
 # Purpose:
 #   The purpose of this procedure is to load the tg_level_id = 0 feature tables that contain STATEFP
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #  - TOPO_HIERARCHY
 #
 ###################################################################################################################
*/
v_process VARCHAR2(100) := 'Add Polygon - SL Feature Tables that have a STATEFP field'; -- Process
v_step VARCHAR2(4000); -- Step
v_start_time TIMESTAMP; -- Start time
v_end_time TIMESTAMP; -- End time
v_elapsed_time interval DAY(5) TO second (2); -- Elapsed time
v_proc_start_time TIMESTAMP; -- Procedure Start Time
v_proc_step VARCHAR2(4000); -- Procedure Step
v_proc_end_time TIMESTAMP; -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2); -- Procedure Elapsed time
array_source_location mdsys.string_array;
v_source_location VARCHAR2(250);
v_deploy VARCHAR2(250) := NULL;
v_release VARCHAR2(250) := NULL;
array_seq_num mdsys.sdo_number_array;
v_seq_num NUMBER;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp VARCHAR2(30);
statefp_exists  NUMBER;
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
v_cnt NUMBER;
v_cnt2 NUMBER;
v_cnt3 NUMBER;
v_table_fields VARCHAR2(4000) := 'SDOGEOMETRY';
v_load_table_fields VARCHAR2(4000);
v_ord1 NUMBER; -- MBR ordinate 1 for STATE
v_ord2 NUMBER; -- MBR ordinate 2 for STATE
v_ord3 NUMBER; -- MBR ordinate 3 for STATE
v_ord4 NUMBER; -- MBR ordinate 4 for STATE
array_state mdsys.string_array; -- STATEFPs
v_state VARCHAR2(2);
v_state_count NUMBER; -- Used to determine whether a particular STATEFP
v_state_data VARCHAR2(2); -- is loaded thus allowing restart capability
v_statefp_exists NUMBER;
v_where_clause VARCHAR2(30); -- used so insert command doesn't have to be changed
mbr_geom SDO_GEOMETRY; -- minimum bond rectangle
statefp_tbl VARCHAR2(32);
v_statefp_tbl VARCHAR2(32);
statefp_colname VARCHAR2(32) := 'STATEFP';
BEGIN
 -- this load technique creates topogeom for all the tables with STATEFP 'xx' as part of a single
 -- map commit; then the next STATEFP is selected and topogeom is created for all tables containing
 -- the next STATEFP, etc.
 --
 -- set variables
 v_release := release;
 v_deploy := deploy;
 --
 -- get all the SL tables that have STATEFP columns
 sql_stmt := 'select a.source_location,b.seq_num from '||topo_universe||' a, '||topo_hierarchy||' b where a.table_name like :1 and a.table_type = :2 and a.topology = :3 and a.tbl_keys like :4 and b.child_layer_tbl is null and a.topology = b.topology and a.table_name = b.table_name order by b.seq_num';
 EXECUTE immediate sql_stmt bulk collect INTO array_source_location,array_seq_num USING 'SL%',tbl_type,topology,'STATEFP%';
 --
 -- Set sdo_topo_map maximum Java memory size
 sdo_topo_map.set_max_memory_size (2147483648);
 --
 -- Create topo map
 -- tracking
 v_proc_start_time := systimestamp;
 v_proc_step := v_process;
 v_step := 'Create Topo Map';
 v_start_time := systimestamp;
 --
 sdo_topo_map.create_topo_map(topology,'MTMAP',8000,4000,1500);
 --
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
 --
 -- set STATEFP table name
 v_statefp_tbl := state_tbl;
 --
 -- see if it exists; will error out if table not found
 sql_stmt := 'select /*+ PARALLEL 4 */ table_name from user_tables where table_name = :1';
 EXECUTE immediate sql_stmt INTO statefp_tbl USING v_statefp_tbl;
 -- 
 -- get the STATEFP data
 sql_stmt := 'select /*+ PARALLEL 4 */ statefp from ' || statefp_tbl || ' order by statefp';
 EXECUTE immediate sql_stmt bulk collect INTO array_state;
 --
 -- process each statefp
 FOR i IN 1..array_state.LAST
 LOOP
  -- select STATEFP
  v_state_data := array_state(i);
  -- Get MBR for load_topo_map
  v_step := 'Get MBR for load_topo_map';
  v_start_time := systimestamp;
  -- State MBRs and exceptions
  sql_stmt := 'select count(*) from mbr_exceptions where release = :1 and statefp = :2';
  EXECUTE immediate sql_stmt INTO v_cnt3 USING v_release, v_state_data;
  IF v_cnt3 = 1 
   THEN
    -- use statefp mbr exceptions if they exist
    sql_stmt := 'select ord1,ord2,ord3,ord4 from mbr_exceptions where release = :1 and statefp = :2';
    EXECUTE immediate sql_stmt INTO v_ord1,v_ord2,v_ord3,v_ord4 USING v_release,v_state_data;
   ELSE
    -- use normal statefp MBRs
    sql_stmt := 'SELECT /*+ PARALLEL 4 */ sdo_geom.sdo_mbr(a.sdogeometry) FROM ' || statefp_tbl || ' a WHERE a.statefp = ''' || v_state_data || ''' and rownum = 1';
    EXECUTE immediate sql_stmt INTO mbr_geom;
    v_ord1 := mbr_geom.sdo_ordinates(1);
    v_ord2 := mbr_geom.sdo_ordinates(2);
    v_ord3 := mbr_geom.sdo_ordinates(3);
    v_ord4 := mbr_geom.sdo_ordinates(4);
  END IF;
  --
  v_cnt3 := NULL;
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  -- Load topo map
  v_step := 'Load Topo Map';
  v_start_time := systimestamp;
  --
  sdo_topo_map.load_topo_map('MTMAP',v_ord1,v_ord2,v_ord3,v_ord4,'TRUE');
  --
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  -- Load table
  FOR i IN 1..array_seq_num.LAST
  LOOP
   v_source_location := array_source_location(i);
   -- gen_tracking 
   v_step := 'Add polygon feature table ' || v_source_location;
   -- check to see if the source table has the specific STATEFP
   sql_stmt := 'select count(rownum) from ' || v_source_location || ' where '||statefp_colname||' = ''' || v_state_data || '''';
   EXECUTE immediate sql_stmt INTO v_cnt;
   IF v_cnt > 0 -- specific STATEFP exists in source table; if = 0 then it skips the load for the particular STATEFP
    THEN
     --
     v_start_time := systimestamp;
     --
     -- add polygon geometry 
     sql_stmt := 'Declare 
                    farray mdsys.sdo_number_array;
                  BEGIN 
                    FOR load_rec IN (SELECT /*+ PARALLEL 4 */ ' || v_table_fields || ' FROM ' || v_source_location || ' where '||statefp_colname||' = ''' || v_state_data || ''')
                     LOOP
                      farray := SDO_TOPO_MAP.ADD_POLYGON_GEOMETRY(null, load_rec.sdogeometry);
                    END LOOP; 
                  END;';
     EXECUTE immediate sql_stmt;
     --
     v_end_time := systimestamp;
     v_elapsed_time := v_end_time - v_start_time;
     GZ_TOPO_BUILD.gen_tracking_log(topology,v_source_location,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
     COMMIT;
   END IF;
  END LOOP;
  --
  -- Commit TOPO map
  --
  -- commit topo map
  v_step := 'Commit Topo Map';
  v_start_time := systimestamp;
  --
  sdo_TOPO_MAP.COMMIT_TOPO_MAP();
  -- commit; -- likely redundant
  --
  -- tracking
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  --
  -- Clear TOPO map cache
  --
  -- tracking
  v_step := 'Clear Topo Map';
  v_start_time := systimestamp;
  --
  sdo_TOPO_MAP.CLEAR_TOPO_MAP('MTMAP');
  --
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  COMMIT;
 END LOOP;
 --
 -- Drop TOPO map
 --
 -- tracking
 v_step := 'Drop Topo Map';
 v_start_time := systimestamp;
 -- drop topo map
 sdo_TOPO_MAP.DROP_TOPO_MAP('MTMAP');
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
 --
 -- end tracking
 --
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_proc_step,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,v_release,'N/A',v_deploy);
 --
 -- generic exception handling
 --
 EXCEPTION
 WHEN OTHERS THEN
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  -- tracking
  v_process := v_process || ' FAILED';
  v_step    := v_step || ' FAILED';
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
  --
  RAISE;
END add_poly_sl_0_statefp;
--
PROCEDURE load_topo_single_state (topology VARCHAR2,tbl_type VARCHAR2,statefp VARCHAR2,topo_universe VARCHAR2,topo_hierarchy VARCHAR2,state_tbl VARCHAR2,release VARCHAR2,deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: load_topo_single_state
 # Author: Salman Mansoor
 # Creation Date: 06/16/20011
 # Recent Revisions:
 #
 # Purpose:
 #   The purpose of this procedure is to load the tg_level_id = 0 feature tables that contain STATEFP, one state at a time 
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #  - TOPO_HIERARCHY
 #
 ###################################################################################################################
*/
v_process VARCHAR2(100) := 'Load SL Feature Tables that have a STATEFP field'; -- Process
v_step VARCHAR2(4000); -- Step
v_start_time TIMESTAMP; -- Start time
v_end_time TIMESTAMP; -- End time
v_elapsed_time interval DAY(5) TO second (2); -- Elapsed time
v_proc_start_time TIMESTAMP; -- Procedure Start Time
v_proc_step VARCHAR2(4000); -- Procedure Step
v_proc_end_time TIMESTAMP; -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2); -- Procedure Elapsed time
array_tbl_name mdsys.string_array;
v_tbl_name VARCHAR2(32);
array_source_location mdsys.string_array;
v_source_location VARCHAR2(250);
array_tbl_keys mdsys.string_array;
v_tbl_keys VARCHAR2(250);
v_deploy VARCHAR2(250) := NULL;
v_release VARCHAR2(250) := NULL;
array_seq_num mdsys.sdo_number_array;
v_seq_num NUMBER;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp VARCHAR2(30);
statefp_exists  NUMBER;
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
v_cnt NUMBER;
v_cnt2 NUMBER;
v_cnt3 NUMBER;
v_table_fields VARCHAR2(4000);
v_load_table_fields VARCHAR2(4000);
v_ord1 NUMBER; -- MBR ordinate 1 for STATE
v_ord2 NUMBER; -- MBR ordinate 2 for STATE
v_ord3 NUMBER; -- MBR ordinate 3 for STATE
v_ord4 NUMBER; -- MBR ordinate 4 for STATE
array_state mdsys.string_array; -- STATEFPs
v_state VARCHAR2(2);
v_state_count NUMBER; -- Used to determine whether a particular STATEFP
v_state_data VARCHAR2(2); -- is loaded thus allowing restart capability
v_statefp_exists NUMBER;
v_where_clause VARCHAR2(30); -- used so insert command doesn't have to be changed
mbr_geom SDO_GEOMETRY; -- minimum bond rectangle
statefp_tbl VARCHAR2(32);
v_statefp_tbl VARCHAR2(32);
statefp_colname VARCHAR2(32);
BEGIN
 -- this load technique creates topogeom for all the tables with STATEFP 'xx' as part of a single
 -- map commit; then the next STATEFP is selected and topogeom is created for all tables containing
 -- the next STATEFP, etc.
 --
 -- set variables
 v_release := release;
 v_deploy := deploy;
 --
 -- get all the SL tables that have STATEFP columns
 sql_stmt := 'select a.table_name,a.source_location,a.tbl_keys,b.seq_num from '||topo_universe||' a, '||topo_hierarchy||' b where a.table_name like :1 and a.table_type = :2 and a.topology = :3 and a.tbl_keys like :4 and b.child_layer_tbl is null and a.topology = b.topology and a.table_name = b.table_name order by b.seq_num';
 EXECUTE immediate sql_stmt bulk collect INTO array_tbl_name,array_source_location,array_tbl_keys,array_seq_num USING 'SL%',tbl_type,topology,'STATEFP%';
 --
 -- Set sdo_topo_map maximum Java memory size
 sdo_topo_map.set_max_memory_size (2147483648);
 --
 -- Create topo map
 -- tracking
 v_proc_start_time := systimestamp;
 v_proc_step := v_process;
 v_step := 'Create Topo Map';
 v_start_time := systimestamp;
 --
 sdo_topo_map.create_topo_map(topology,statefp||'MTMAP',8000,4000,1500);
 --
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
 --
 -- set STATEFP table name
 v_statefp_tbl := state_tbl;
 --
 -- see if it exists; will error out if table not found
 sql_stmt := 'select /*+ PARALLEL 4 */ table_name from user_tables where table_name = :1';
 EXECUTE immediate sql_stmt INTO statefp_tbl USING v_statefp_tbl;
 -- 
 -- get the STATEFP data
 sql_stmt := 'select /*+ PARALLEL 4 */ statefp from ' || statefp_tbl || ' where statefp = ' || statefp || '';
 EXECUTE immediate sql_stmt bulk collect INTO array_state;
 --
 -- process each statefp
 FOR i IN 1..array_state.LAST
 LOOP
  -- select STATEFP
  v_state_data := array_state(i);
  -- Get MBR for load_topo_map
  v_step := 'Get MBR for load_topo_map';
  v_start_time := systimestamp;
  -- State MBRs and exceptions
  sql_stmt := 'select count(*) from mbr_exceptions where release = :1 and statefp = :2';
  EXECUTE immediate sql_stmt INTO v_cnt3 USING v_release, v_state_data;
  IF v_cnt3 = 1 
   THEN
    -- use statefp mbr exceptions if they exist
    sql_stmt := 'select ord1,ord2,ord3,ord4 from mbr_exceptions where release = :1 and statefp = :2';
    EXECUTE immediate sql_stmt INTO v_ord1,v_ord2,v_ord3,v_ord4 USING v_release,v_state_data;
   ELSE
    -- use normal statefp MBRs
    sql_stmt := 'SELECT /*+ PARALLEL 4 */ sdo_geom.sdo_mbr(a.sdogeometry) FROM ' || statefp_tbl || ' a WHERE a.statefp = ''' || v_state_data || ''' and rownum = 1';
    EXECUTE immediate sql_stmt INTO mbr_geom;
    v_ord1 := mbr_geom.sdo_ordinates(1);
    v_ord2 := mbr_geom.sdo_ordinates(2);
    v_ord3 := mbr_geom.sdo_ordinates(3);
    v_ord4 := mbr_geom.sdo_ordinates(4);
  END IF;
  --
  v_cnt3 := NULL;
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  -- Load topo map
  v_step := 'Load Topo Map';
  v_start_time := systimestamp;
  --
  sdo_topo_map.load_topo_map(statefp||'MTMAP',v_ord1,v_ord2,v_ord3,v_ord4,'TRUE');
  --
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  -- Load table
  FOR i IN 1..array_seq_num.LAST
  LOOP
   v_tbl_name := topology||'_'||array_tbl_name(i);
   v_source_location := array_source_location(i);
   v_tbl_keys := array_tbl_keys(i);
   v_seq_num := array_seq_num(i);
   -- set statefp column name, either statefp or statefp_sl795
   IF (v_tbl_name = topology||'_SL795')
    THEN
     statefp_colname := 'statefp_sl795';
     v_where_clause := 'STATEFP_SL795 IS NOT NULL'; -- used so insert command doesn't have to be changed
    ELSE
     statefp_colname := 'statefp';
     v_where_clause := 'STATEFP IS NOT NULL'; -- used so insert command doesn't have to be changed
   END IF;
   -- gen_tracking 
   v_step := 'Load feature table ' || v_tbl_name;
   -- check to see if the source table has the specific STATEFP
   sql_stmt := 'select count(rownum) from ' || v_source_location || ' where '||statefp_colname||' = ''' || v_state_data || '''';
   EXECUTE immediate sql_stmt INTO v_cnt;
   IF v_cnt > 0 -- specific STATEFP exists in source table; if = 0 then it skips the load for the particular STATEFP
    THEN
     -- check if specific STATEFP is already loaded into feature table; ability to re-start an aborted load
     sql_stmt := 'select count(rownum) from ' || v_tbl_name || ' where '||statefp_colname||' = ''' || v_state_data || '''';
     EXECUTE immediate sql_stmt INTO v_cnt2;
     --
     IF v_cnt2 = 0 THEN
      -- get column names from source feature table
      v_table_fields := create_ft_cols (v_source_location);
      -- create load_rec string
      v_load_table_fields := create_ft_cols (v_tbl_name);
      v_load_table_fields := 'load_rec.' || regexp_replace (v_load_table_fields,',',',load_rec.');
      v_start_time := systimestamp;
      --
      -- create feature
      sql_stmt := 'Declare topo_geom sdo_topo_geometry; 
                   BEGIN FOR load_rec IN (SELECT /*+ PARALLEL 4 */ ' || v_table_fields || ' FROM ' || v_source_location || ' where '||statefp_colname||' = ''' || v_state_data || ''' and ' || v_where_clause || ')
                    LOOP
                     topo_geom := SDO_TOPO_MAP.CREATE_FEATURE(''' || topology || ''',''' || v_tbl_name || ''',''TOPOGEOM'',load_rec.sdogeometry,0);
                     INSERT /*+ APPEND */ INTO ' || v_tbl_name || ' VALUES (' || v_load_table_fields || ',topo_geom);
                    END LOOP; 
                   END;';
      EXECUTE immediate sql_stmt;
      --
      v_end_time := systimestamp;
      v_elapsed_time := v_end_time - v_start_time;
      GZ_TOPO_BUILD.gen_tracking_log(topology,v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
      COMMIT;
     END IF;
   END IF;
  END LOOP;
  --
  -- Commit TOPO map
  --
  -- commit topo map
  v_step := 'Commit Topo Map';
  v_start_time := systimestamp;
  --
  sdo_TOPO_MAP.COMMIT_TOPO_MAP();
  -- commit; -- likely redundant
  --
  -- tracking
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  --
  -- Clear TOPO map cache
  --
  -- tracking
  v_step := 'Clear Topo Map';
  v_start_time := systimestamp;
  --
  sdo_TOPO_MAP.CLEAR_TOPO_MAP(statefp||'MTMAP');
  --
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  COMMIT;
 END LOOP;
 --
 -- Drop TOPO map
 --
 -- tracking
 v_step := 'Drop Topo Map';
 v_start_time := systimestamp;
 -- drop topo map
 sdo_TOPO_MAP.DROP_TOPO_MAP(statefp||'MTMAP');
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
 --
 -- end tracking
 --
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_proc_step,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,v_release,'N/A',v_deploy);
 --
 -- generic exception handling
 --
 EXCEPTION
 WHEN OTHERS THEN
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  -- tracking
  v_process := v_process || ' FAILED';
  v_step    := v_step || ' FAILED';
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
  --
  RAISE;
END load_topo_single_state;
--
PROCEDURE add_poly_single_state (topology VARCHAR2,tbl_type VARCHAR2,statefp VARCHAR2,topo_universe VARCHAR2,topo_hierarchy VARCHAR2,state_tbl VARCHAR2,release VARCHAR2,deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: add_poly_single_state
 # Author: Salman Mansoor
 # Creation Date: 06/16/2011
 # Recent Revisions: 
 #
 # Purpose:
 #   The purpose of this procedure is to load the tg_level_id = 0 feature tables that contain STATEFP, one state at a time 
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #  - TOPO_HIERARCHY
 #
 ###################################################################################################################
*/
v_process VARCHAR2(100) := 'Add Polygon - SL Feature Tables that have a STATEFP field'; -- Process
v_step VARCHAR2(4000); -- Step
v_start_time TIMESTAMP; -- Start time
v_end_time TIMESTAMP; -- End time
v_elapsed_time interval DAY(5) TO second (2); -- Elapsed time
v_proc_start_time TIMESTAMP; -- Procedure Start Time
v_proc_step VARCHAR2(4000); -- Procedure Step
v_proc_end_time TIMESTAMP; -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2); -- Procedure Elapsed time
array_source_location mdsys.string_array;
v_source_location VARCHAR2(250);
v_deploy VARCHAR2(250) := NULL;
v_release VARCHAR2(250) := NULL;
array_seq_num mdsys.sdo_number_array;
v_seq_num NUMBER;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp VARCHAR2(30);
statefp_exists  NUMBER;
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
sql_stmt2 VARCHAR2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
v_cnt NUMBER;
v_cnt2 NUMBER;
v_cnt3 NUMBER;
v_table_fields VARCHAR2(4000) := 'SDOGEOMETRY';
v_load_table_fields VARCHAR2(4000);
v_ord1 NUMBER; -- MBR ordinate 1 for STATE
v_ord2 NUMBER; -- MBR ordinate 2 for STATE
v_ord3 NUMBER; -- MBR ordinate 3 for STATE
v_ord4 NUMBER; -- MBR ordinate 4 for STATE
array_state mdsys.string_array; -- STATEFPs
v_state VARCHAR2(2);
v_state_count NUMBER; -- Used to determine whether a particular STATEFP
v_state_data VARCHAR2(2); -- is loaded thus allowing restart capability
v_statefp_exists NUMBER;
v_where_clause VARCHAR2(30); -- used so insert command doesn't have to be changed
mbr_geom SDO_GEOMETRY; -- minimum bond rectangle
statefp_tbl VARCHAR2(32);
v_statefp_tbl VARCHAR2(32);
statefp_colname VARCHAR2(32) := 'STATEFP';
BEGIN
 -- this load technique creates topogeom for all the tables with STATEFP 'xx' as part of a single
 -- map commit; then the next STATEFP is selected and topogeom is created for all tables containing
 -- the next STATEFP, etc.
 --
 -- set variables
 v_release := release;
 v_deploy := deploy;
 --
 -- get all the SL tables that have STATEFP columns
 sql_stmt := 'select a.source_location,b.seq_num from '||topo_universe||' a, '||topo_hierarchy||' b where a.table_name like :1 and a.table_type = :2 and a.topology = :3 and a.tbl_keys like :4 and b.child_layer_tbl is null and a.topology = b.topology and a.table_name = b.table_name order by b.seq_num';
 EXECUTE immediate sql_stmt bulk collect INTO array_source_location,array_seq_num USING 'SL%',tbl_type,topology,'STATEFP%';
 --
 -- Set sdo_topo_map maximum Java memory size
 sdo_topo_map.set_max_memory_size (2147483648);
 --
 -- Create topo map
 -- tracking
 v_proc_start_time := systimestamp;
 v_proc_step := v_process;
 v_step := 'Create Topo Map';
 v_start_time := systimestamp;
 --
 sdo_topo_map.create_topo_map(topology,statefp||'MTMAP',8000,4000,1500);
 --
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
 --
 -- set STATEFP table name
 v_statefp_tbl := state_tbl;
 --
 -- see if it exists; will error out if table not found
 sql_stmt := 'select /*+ PARALLEL 4 */ table_name from user_tables where table_name = :1';
 EXECUTE immediate sql_stmt INTO statefp_tbl USING v_statefp_tbl;
 -- 
 -- get the STATEFP data
 sql_stmt := 'select /*+ PARALLEL 4 */ statefp from ' || statefp_tbl || ' where statefp = ' || statefp || '';
 EXECUTE immediate sql_stmt bulk collect INTO array_state;
 --
 -- process each statefp
 FOR i IN 1..array_state.LAST
 LOOP
  -- select STATEFP
  v_state_data := array_state(i);
  -- Get MBR for load_topo_map
  v_step := 'Get MBR for load_topo_map';
  v_start_time := systimestamp;
  -- State MBRs and exceptions
  sql_stmt := 'select count(*) from mbr_exceptions where release = :1 and statefp = :2';
  EXECUTE immediate sql_stmt INTO v_cnt3 USING v_release, v_state_data;
  IF v_cnt3 = 1 
   THEN
    -- use statefp mbr exceptions if they exist
    sql_stmt := 'select ord1,ord2,ord3,ord4 from mbr_exceptions where release = :1 and statefp = :2';
    EXECUTE immediate sql_stmt INTO v_ord1,v_ord2,v_ord3,v_ord4 USING v_release,v_state_data;
   ELSE
    -- use normal statefp MBRs
    sql_stmt := 'SELECT /*+ PARALLEL 4 */ sdo_geom.sdo_mbr(a.sdogeometry) FROM ' || statefp_tbl || ' a WHERE a.statefp = ''' || v_state_data || ''' and rownum = 1';
    EXECUTE immediate sql_stmt INTO mbr_geom;
    v_ord1 := mbr_geom.sdo_ordinates(1);
    v_ord2 := mbr_geom.sdo_ordinates(2);
    v_ord3 := mbr_geom.sdo_ordinates(3);
    v_ord4 := mbr_geom.sdo_ordinates(4);
  END IF;
  --
  v_cnt3 := NULL;
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  -- Load topo map
  v_step := 'Load Topo Map';
  v_start_time := systimestamp;
  --
  sdo_topo_map.load_topo_map(statefp||'MTMAP',v_ord1,v_ord2,v_ord3,v_ord4,'TRUE');
  --
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  -- Load table
  FOR i IN 1..array_seq_num.LAST
  LOOP
   v_source_location := array_source_location(i);
   -- gen_tracking 
   v_step := 'Add polygon feature table ' || v_source_location;
   -- check to see if the source table has the specific STATEFP
   sql_stmt := 'select count(rownum) from ' || v_source_location || ' where '||statefp_colname||' = ''' || v_state_data || '''';
   EXECUTE immediate sql_stmt INTO v_cnt;
   IF v_cnt > 0 -- specific STATEFP exists in source table; if = 0 then it skips the load for the particular STATEFP
    THEN
     --
     v_start_time := systimestamp;
     --
     -- add polygon geometry 
     sql_stmt := 'Declare 
                    farray mdsys.sdo_number_array;
                  BEGIN 
                    FOR load_rec IN (SELECT /*+ PARALLEL 4 */ ' || v_table_fields || ' FROM ' || v_source_location || ' where '||statefp_colname||' = ''' || v_state_data || ''')
                     LOOP
                      farray := SDO_TOPO_MAP.ADD_POLYGON_GEOMETRY(null, load_rec.sdogeometry);
                    END LOOP; 
                  END;';
     EXECUTE immediate sql_stmt;
     --
     v_end_time := systimestamp;
     v_elapsed_time := v_end_time - v_start_time;
     GZ_TOPO_BUILD.gen_tracking_log(topology,v_source_location,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
     COMMIT;
   END IF;
  END LOOP;
  --
  -- Commit TOPO map
  --
  -- commit topo map
  v_step := 'Commit Topo Map';
  v_start_time := systimestamp;
  --
  sdo_TOPO_MAP.COMMIT_TOPO_MAP();
  -- commit; -- likely redundant
  --
  -- tracking
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  --
  -- Clear TOPO map cache
  --
  -- tracking
  v_step := 'Clear Topo Map';
  v_start_time := systimestamp;
  --
  sdo_TOPO_MAP.CLEAR_TOPO_MAP(statefp||'MTMAP');
  --
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
  COMMIT;
 END LOOP;
 --
 -- Drop TOPO map
 --
 -- tracking
 v_step := 'Drop Topo Map';
 v_start_time := systimestamp;
 -- drop topo map
 sdo_TOPO_MAP.DROP_TOPO_MAP(statefp||'MTMAP');
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy);
 --
 -- end tracking
 --
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_proc_step,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,v_release,'N/A',v_deploy);
 --
 -- generic exception handling
 --
 EXCEPTION
 WHEN OTHERS THEN
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  -- tracking
  v_process := v_process || ' FAILED';
  v_step    := v_step || ' FAILED';
  GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||v_state_data,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
  --
  RAISE;
END add_poly_single_state;
--
PROCEDURE update_division_regionce (tbl_name VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: update_division_regionce
 # Author: mz
 # Creation Date: 06/29/2009
 # Recent Revisions:
 #
 # Purpose:
 #  The purpose of this procedure is to populate the regionce field in the division (SL030) table.
 #  MAF/TIGER source tables do not have REGIONCE in the DIVISION table (SL030) which is required
 #  in order to build SL020 (REGION) hierarchically over SL030 (DIVISION).
 #
 # Required Parameters:
 #  - tbl_name = Table Name
 #
 # Dependencies:
 #  - Division (SL030) table
 #
 ###################################################################################################################
*/
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
BEGIN
 -- region 1
 sql_stmt := 'update ' || tbl_name || ' set regionce = ''1'' where divisionce in (''1'',''2'')';
 EXECUTE immediate sql_stmt;
 COMMIT;
 -- region 2
 sql_stmt := 'update ' || tbl_name || '  set regionce = ''2'' where divisionce in (''3'',''4'')';
 EXECUTE immediate sql_stmt;
 COMMIT;
 -- region 3
 sql_stmt := 'update ' || tbl_name || '  set regionce = ''3'' where divisionce in (''5'',''6'',''7'')';
 EXECUTE immediate sql_stmt;
 COMMIT;
 -- region 4
 sql_stmt := 'update ' || tbl_name || '  set regionce = ''4'' where divisionce in (''8'',''9'')';
 EXECUTE immediate sql_stmt;
 COMMIT;
 -- region 9
 sql_stmt := 'update ' || tbl_name || '  set regionce = ''9'' where divisionce in (''0'')';
 EXECUTE immediate sql_stmt;
 COMMIT;
END update_division_regionce;
--
FUNCTION count_occurs (str VARCHAR2,search VARCHAR2 := NULL) RETURN NUMBER
IS
/**
 ###################################################################################################################
 # Program Name: count_occurs
 # Author: mz
 # Creation Date: 06/23/2009
 # Revision Date:
 #
 # Purpose:
 #   The purpose of this function is to search for a pattern within a string
 #
 # Required parameters:
 #   - str - varchar2 which contains the string to search
 #   - string - what to search for in the string
 #
 # Dependencies: None
 #
 ###################################################################################################################
*/
BEGIN
 IF (search IS NULL ) THEN
   RETURN LENGTH(str);
 ELSE
   RETURN (LENGTH(str) - NVL(LENGTH(REPLACE(str,search,'')),0))/LENGTH(search);
 END IF;
END count_occurs;
--
FUNCTION create_dml_condition (tbl_keys IN VARCHAR2, tbl_keys_data IN VARCHAR2) RETURN VARCHAR2 IS dml_condition VARCHAR2(4000);
/**
 ###################################################################################################################
 # Program Name: create_dml_condition
 # Author: mz
 # Creation Date: 06/23/2009
 # Revision Date:
 #
 # Purpose:
 #   The purpose of this function is to create a DML_CONDITION for loading hierarchical feature tables
 #   based on a particular table's keys (defined in the TOPO_UNIVERSE table)
 #
 #   The DML_CONDITION results for 1, 2, 3, or 4 keys MUST match:
 #   Key1 - 'statefp = ''01'''
 #   Key2 - 'statefp = ''01'' and countyfp = ''001'''
 #   Key3 - 'statefp = ''01'' and countyfp = ''001'' and cousubfp = ''00001'''
 #   Key4 - 'statefp = ''01'' and countyfp = ''001'' and cousubfp = ''00001'' and placefp = ''00001'''
 #
 # Required parameters:
 #  - tbl_keys - Table Keys - e.g., STATEFP,COUNTYFP,COUSUBFP,PLACEFP
 #  - tbl_keys_data - Table Keys Data - e.g., 01,001,00001,00001
 #
 # Dependencies:
 #  TOPO_UNIVERSE table
 #
 ###################################################################################################################
*/
comma_cnt NUMBER; -- number of comma's in string
-- selects subset information from string
v_hack_left1  NUMBER; -- first table_key comma
v_hack_right1 NUMBER; -- second table_key comma
v_hack_right1a NUMBER; -- third table key comma
v_hack_left2  NUMBER; -- first table_key_data comma
v_hack_right2 NUMBER; -- second table_key_data comma
v_hack_right2a NUMBER; -- third table_key_data comma
-- key related
v_key1  VARCHAR2(1000);
v_key2  VARCHAR2(1000);
v_key3  VARCHAR2(1000);
v_key4  VARCHAR2(1000);
v_key1a VARCHAR2(1000);
v_key2a VARCHAR2(1000);
v_key3a VARCHAR2(1000);
v_key4a VARCHAR2(1000);
v_apos  VARCHAR2(1); -- apostrophies used in construction of dml_condition
BEGIN
 -- split table keys
 -- get count of commas
 comma_cnt := count_occurs (tbl_keys,',');
 v_apos := ''''; -- apostrophies used in construction of dml_condition
 --
 -- table_keys
 -- locate first comma
 v_hack_left1 := instr(tbl_keys,',',1,1);
 -- located second comma
 v_hack_right1 := instr(tbl_keys,',',v_hack_left1 + 1,1);
 -- locate third comma
 v_hack_right1a := instr(tbl_keys,',',v_hack_right1 + 1,1);
 --
 -- table_keys_data
 -- locate first comma
 v_hack_left2 := instr(tbl_keys_data,',',1,1);
 -- locate second comma
 v_hack_right2 := instr(tbl_keys_data,',',v_hack_left2 + 1,1);
 -- locate third comma
 v_hack_right2a := instr(tbl_keys_data,',',v_hack_right2 + 1,1);
 --
 -- if one table key
 IF comma_cnt = 0 THEN
  -- first key (only one key)
  v_key1 := tbl_keys;
  v_key1a := tbl_keys_data;
  -- initial apostrophy
  v_key1 := v_apos || v_key1;
  -- dml condition
  SELECT '' || v_key1 || ' = ''''' || v_key1a || '''''''' INTO dml_condition FROM dual;
 END IF;
 --
 -- two table keys
 IF comma_cnt = 1 THEN
  -- first key
  v_key1 := SUBSTR(tbl_keys,1,v_hack_left1-1);
  v_key1a := SUBSTR(tbl_keys_data,1,v_hack_left2-1);
  -- initial apostrophy
  v_key1 := v_apos || v_key1;
  -- second key
  v_key2 := SUBSTR(tbl_keys,v_hack_left1+1,15);
  v_key2a := SUBSTR(tbl_keys_data,v_hack_left2+1,15);
  -- dml comdition
  SELECT '' || v_key1 || ' = ''''' || v_key1a || ''''' and ' || v_key2 || ' = ''''' || v_key2a || '''''''' INTO dml_condition FROM dual;
 END IF;
 --
 -- three table keys
 IF comma_cnt = 2 THEN
  -- first key
  v_key1 := SUBSTR(tbl_keys,1,v_hack_left1-1);
  v_key1a := SUBSTR(tbl_keys_data,1,v_hack_left2-1);
  -- initial apostrophy
  v_key1 := v_apos || v_key1;
  -- second key
  v_key2 := SUBSTR(tbl_keys,v_hack_left1+1,v_hack_right1-v_hack_left1-1);
  v_key2a := SUBSTR(tbl_keys_data,v_hack_left2 +1,v_hack_right2-v_hack_left2-1);
  -- third key
  v_key3 := SUBSTR(tbl_keys,v_hack_right1+1);
  v_key3a := SUBSTR(tbl_keys_data,v_hack_right2+1);
  -- dml comdition
  SELECT '' || v_key1 || ' = ''''' || v_key1a || ''''' and ' || v_key2 || ' = ''''' || v_key2a || ''''' and ' || v_key3 || ' = ''''' || v_key3a || '''''''' INTO dml_condition FROM dual;
 END IF;
 --
 -- four table keys
 IF comma_cnt = 3 THEN
  -- first key
  v_key1 := SUBSTR(tbl_keys,1,v_hack_left1-1);
  v_key1a := SUBSTR(tbl_keys_data,1,v_hack_left2-1);
  -- add leading apostrophy
  v_key1 := v_apos || v_key1;
  -- second key
  v_key2 := SUBSTR(tbl_keys,v_hack_left1+1,v_hack_right1-v_hack_left1-1);
  v_key2a := SUBSTR(tbl_keys_data,v_hack_left2+1,v_hack_right2-v_hack_left2-1);
  -- third key
  v_key3 := SUBSTR(tbl_keys,v_hack_right1+1,v_hack_right1a-v_hack_right1-1);
  v_key3a := SUBSTR(tbl_keys_data,v_hack_right2+1,v_hack_right2a-v_hack_right2-1);
  -- fourth key
  v_key4 := SUBSTR(tbl_keys,v_hack_right1a+1);
  v_key4a := SUBSTR(tbl_keys_data,v_hack_right2a+1);
  -- dml comdition
  SELECT '' || v_key1 || ' = ''''' || v_key1a || ''''' and ' || v_key2 || ' = ''''' || v_key2a || ''''' and ' || v_key3 || ' = ''''' || v_key3a || ''''' and ' || v_key4 || ' = ''''' || v_key4a || '''''''' INTO dml_condition FROM dual;
 END IF;
 -- return dml_condition value
  RETURN dml_condition;
END create_dml_condition;
--
FUNCTION create_ft_cols (table_name IN VARCHAR2) RETURN VARCHAR2 IS v_table_fields VARCHAR2(4000);
/**
 ###################################################################################################################
 # Program Name: create_ft_cols
 # Author: Unknown (from internet)
 # Creation Date: Unknown
 # Revision Date: 06/22/2009 - mz
 #
 # Purpose:
 #   The purpose of this function is to extract a table's column names from the COLS view in the data dictionary
 #      and concatenate the column names so that they are in a single line separated by commas.
 #
 # Required parameters:
 #  - table_name - Name of feature table
 #
 # Dependencies: None
 #
 ###################################################################################################################
*/
array_col mdsys.string_array; -- array of columns
v_col VARCHAR2(32); -- column
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
BEGIN
 -- obtain column names
 sql_stmt := 'select column_name from cols where table_name = :1 and column_name <> ''TOPOGEOM'' order by column_id';
 EXECUTE immediate sql_stmt bulk collect INTO array_col USING table_name;
 -- process column names from multiple lines into a single line
 v_table_fields := '';
 FOR i IN 1..array_col.LAST
  LOOP
   v_col := array_col(i);
   v_table_fields := v_table_fields || ',' || v_col;
  END LOOP;
 -- remove leading comma
 v_table_fields := SUBSTR(v_table_fields,2,3999);
 RETURN v_table_fields;
END create_ft_cols;
--
FUNCTION create_dml_condition2 (tbl_keys IN VARCHAR2, tbl_keys_data IN VARCHAR2) RETURN VARCHAR2
IS
  dml_condition VARCHAR2(4000);
  /**
  ###################################################################################################################
  # Program Name: create_dml_condition
  # Author: Salman 
  # Creation Date: 02/25/2010 
  # Revision Date:
  #
  # Purpose:
  #   The purpose of this function is to create a DML_CONDITION for loading hierarchical feature tables
  #   based on a particular table's keys (defined in the TOPO_UNIVERSE table)
  #
  #   The DML_CONDITION result for 1, 2 or 3 keys MUST match:
  #   Key1 - 'statefp = ''01'''
  #   Key2 - 'statefp = ''01'' and countyfp = ''001'''
  #   Key3 - 'statefp = ''01'' and countyfp = ''001'' and cousubfp = ''00001'''
  #   Key4 - 'statefp = ''01'' and countyfp = ''001'' and cousubfp = ''00001'' and placefp = ''00001'''
  #   Key5 - 'statefp = ''01'' and countyfp = ''001'' and cousubfp = ''00001'' and placefp = ''00001'' and tractce = ''000001'''
  #
  # Required parameters:
  #  - tbl_keys - Table Keys - e.g., STATEFP,COUNTYFP,COUSUBFP,PLACEFP,TRACTCE 
  #  - tbl_keys_data - Table Keys Data - e.g., 01,001,00001,00001,000001 
  #
  # Dependencies:
  #  TOPO_UNIVERSE table
  #
  ###################################################################################################################
  */
  comma_cnt NUMBER; -- number of comma's in string
  -- selects subset information from string
  -- first table_key comma
  v_hack_left1  NUMBER;
  -- second table_key comma
  v_hack_right1 NUMBER;
  -- third table_key comma
  v_hack_right1a NUMBER;
  -- fourth table_key comma
  v_hack_right1b NUMBER;
  -- first table_key_data comma
  v_hack_left2  NUMBER;
  -- second table_key data comma
  v_hack_right2 NUMBER;
  -- third table_key data comma
  v_hack_right2a NUMBER;
  -- fourth table_key data comma
  v_hack_right2b NUMBER;
  -- key related
  v_key1  VARCHAR2(1000);
  v_key2  VARCHAR2(1000);
  v_key3  VARCHAR2(1000);
  v_key4  VARCHAR2(1000);
  v_key5  VARCHAR2(1000);
  v_key1a VARCHAR2(1000);
  v_key2a VARCHAR2(1000);
  v_key3a VARCHAR2(1000);
  v_key4a VARCHAR2(1000);
  v_key5a VARCHAR2(1000);
  v_apos  VARCHAR2(1); -- apostrophies used in construction of dml_condition
BEGIN
  -- split table keys
  -- get count of commas
  comma_cnt := count_occurs (tbl_keys,',');
  v_apos := ''''; -- apostrophies used in construction of dml_condition
  --
  -- table_keys
  -- locate first comma
  v_hack_left1  := instr(tbl_keys,',',1,1);
  -- located second comma
  v_hack_right1 := instr(tbl_keys,',',v_hack_left1 + 1,1);
  -- locate third comma
  v_hack_right1a := instr(tbl_keys,',',v_hack_right1 + 1,1);
  -- locate fourth comma
  v_hack_right1b := instr(tbl_keys,',',v_hack_right1a + 1,1);
  --
  -- table_keys_data
  -- locate first comma
  v_hack_left2  := instr(tbl_keys_data,',',1,1);
  -- locate second comma
  v_hack_right2 := instr(tbl_keys_data,',',v_hack_left2 + 1,1);
  -- locate third comma
  v_hack_right2a := instr(tbl_keys_data,',',v_hack_right2 + 1,1);
  -- locate fourth comma
  v_hack_right2b := instr(tbl_keys_data,',',v_hack_right2a + 1,1);
  -- one table key
  IF comma_cnt = 0 THEN
    -- first key (only key)
    v_key1    := tbl_keys;
    v_key1a   := tbl_keys_data;
    -- initial apostrophy
    v_key1    := v_apos || v_key1;
    -- dml condition
    SELECT '' || v_key1 || ' = ''''' || v_key1a || '''''''' INTO dml_condition FROM dual;
  END IF;
  -- two table keys
  IF comma_cnt = 1 THEN
    -- first key
    v_key1    := SUBSTR(tbl_keys,1,v_hack_left1-1);
    v_key1a   := SUBSTR(tbl_keys_data,1,v_hack_left2-1);
    -- initial apostrophy
    v_key1    := v_apos || v_key1;
    -- second key
    v_key2    := SUBSTR(tbl_keys,v_hack_left1+1,15);
    v_key2a   := SUBSTR(tbl_keys_data,v_hack_left2+1,15);
    -- dml comdition
    SELECT '' || v_key1 || ' = ''''' || v_key1a || ''''' and ' || v_key2 || ' = ''''' || v_key2a || '''''''' INTO dml_condition FROM dual;
  END IF;
  -- three table keys
  IF comma_cnt = 2 THEN
    -- first key
    v_key1    := SUBSTR(tbl_keys,1,v_hack_left1-1);
    v_key1a   := SUBSTR(tbl_keys_data,1,v_hack_left2-1);
    -- initial apostrophy
    v_key1    := v_apos || v_key1;
    -- second key
    v_key2    := SUBSTR(tbl_keys,v_hack_left1+1,v_hack_right1-v_hack_left1-1);
    v_key2a   := SUBSTR(tbl_keys_data,v_hack_left2 +1,v_hack_right2-v_hack_left2-1);
    -- third key
    v_key3    := SUBSTR(tbl_keys,v_hack_right1+1);
    v_key3a   := SUBSTR(tbl_keys_data,v_hack_right2+1);
    -- dml comdition
    SELECT '' || v_key1 || ' = ''''' || v_key1a || ''''' and ' || v_key2 || ' = ''''' || v_key2a || ''''' and ' || v_key3 || ' = ''''' || v_key3a || '''''''' INTO dml_condition FROM dual;
  END IF;
  -- four table keys
  IF comma_cnt = 3 THEN
    -- first key
    v_key1    := SUBSTR(tbl_keys,1,v_hack_left1-1);
    v_key1a   := SUBSTR(tbl_keys_data,1,v_hack_left2-1);
    -- add leading apostrophy
    v_key1    := v_apos || v_key1;
    -- second key
    v_key2    := SUBSTR(tbl_keys,v_hack_left1+1,v_hack_right1-v_hack_left1-1);
    v_key2a   := SUBSTR(tbl_keys_data,v_hack_left2+1,v_hack_right2-v_hack_left2-1);
    -- third key
    v_key3    := SUBSTR(tbl_keys,v_hack_right1+1,v_hack_right1a-v_hack_right1-1);
    v_key3a   := SUBSTR(tbl_keys_data,v_hack_right2+1,v_hack_right2a-v_hack_right2-1);
    -- fourth key
    v_key4    := SUBSTR(tbl_keys,v_hack_right1a+1);
    v_key4a   := SUBSTR(tbl_keys_data,v_hack_right2a+1);
    -- dml comdition
    SELECT '' || v_key1 || ' = ''''' || v_key1a || ''''' and ' || v_key2 || ' = ''''' || v_key2a || ''''' and ' || v_key3 || ' = ''''' || v_key3a || ''''' and ' || v_key4 || ' = ''''' || v_key4a || '''''''' INTO dml_condition FROM dual;
  END IF;
  -- five table keys
  IF comma_cnt = 4 THEN
    -- first key
    v_key1    := SUBSTR(tbl_keys,1,v_hack_left1-1);
    v_key1a   := SUBSTR(tbl_keys_data,1,v_hack_left2-1);
    -- add leading apostrophy
    v_key1    := v_apos || v_key1;
    -- second key
    v_key2    := SUBSTR(tbl_keys,v_hack_left1+1,v_hack_right1-v_hack_left1-1);
    v_key2a   := SUBSTR(tbl_keys_data,v_hack_left2+1,v_hack_right2-v_hack_left2-1);
    -- third key
    v_key3    := SUBSTR(tbl_keys,v_hack_right1+1,v_hack_right1a-v_hack_right1-1);
    v_key3a   := SUBSTR(tbl_keys_data,v_hack_right2+1,v_hack_right2a-v_hack_right2-1);
    -- fourth key
    v_key4    := SUBSTR(tbl_keys,v_hack_right1a+1,v_hack_right1b-v_hack_right1a-1);
    v_key4a   := SUBSTR(tbl_keys_data,v_hack_right2a+1,v_hack_right2b-v_hack_right2a-1);
    -- fifth key
    v_key5    := SUBSTR(tbl_keys,v_hack_right1b+1);
    v_key5a   := SUBSTR(tbl_keys_data,v_hack_right2b+1);
    -- dml comdition
    SELECT '' || v_key1 || ' = ''''' || v_key1a || ''''' and ' || v_key2 || ' = ''''' || v_key2a || ''''' and ' || v_key3 || ' = ''''' || v_key3a || ''''' and ' || v_key4 || ' = ''''' || v_key4a || ''''' and ' || v_key5 || ' = ''''' || v_key5a || '''''''' INTO dml_condition FROM dual;
  END IF;
 -- return dml_condition value
  RETURN dml_condition;
END create_dml_condition2;
--
PROCEDURE create_cousub_estimates (source_tbl_name VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: create_cousub_estimates
 # Author: mz
 # Creation Date: 11/18/2008
 # Recent Revision: 04/02/2009; 06/04/2009; 06/10/2009; 07/15/2009
 #
 # Purpose:
 #   The purpose of this procedure is to create cousub tables that require specialized filtering queries.
 #   This procedure works with ACS08 (ACS08_SL060), ACS09 (ACS09_SL060), PEP09 (PEP09_SL060/PEP09_SL070);
 #   it may have to be revised to work with other releases.
 #
 # Dependencies:
 #  - Pre-existing sl060 source table
 #
 ###################################################################################################################
*/
array_state2 mdsys.string_array;
v_state2 VARCHAR2(2);
array_county2 mdsys.string_array;
v_county2 VARCHAR2(3);
array_state mdsys.string_array;
v_statefp VARCHAR2(2);
array_county mdsys.string_array;
v_countyfp VARCHAR2(3);
array_cousub mdsys.string_array;
v_cousubfp VARCHAR2(5);
array_funcstat mdsys.string_array;
v_funcstat VARCHAR2(1);
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
exists_count NUMBER;
counter NUMBER := 0;
v_temp_tbl VARCHAR2(32);
BEGIN
 --
 -- create temp table
 --
 v_temp_tbl := source_tbl_name || '_MOD';
 -- determine if a table named <passed variable>_MOD exists
 sql_stmt := 'select count(*) from user_tables where table_name = :1';
 EXECUTE immediate sql_stmt INTO exists_count USING v_temp_tbl;
 -- if it exists, then drop it
 IF exists_count > 0 THEN
  BEGIN
   sql_stmt := 'drop table ' || v_temp_tbl || ' purge';
   EXECUTE immediate sql_stmt;
  END;
 END IF;
 exists_count := 0;
 -- rename the passed table name to the '_MOD' version
 sql_stmt := 'alter table ' || source_tbl_name || ' rename to ' || v_temp_tbl;
 EXECUTE immediate sql_stmt;
 --
 -- create cousub_list
 --
 -- check to see if cousub_list exists
 sql_stmt := 'select count(*) from user_tables where table_name = ''COUSUB_LIST''';
 EXECUTE immediate sql_stmt INTO exists_count;
 -- if it exists then drop it
 IF exists_count > 0 THEN
  sql_stmt := 'drop table cousub_list purge';
  EXECUTE immediate sql_stmt;
 END IF;
 exists_count := 0;
 -- create cousub_list
 sql_stmt := 'create table cousub_list (statefp varchar2(2), countyfp varchar2(3), cousubfp varchar2(5),sdogeometry mdsys.sdo_geometry, name varchar2(200)) nologging';
 EXECUTE immediate sql_stmt;
 --
 -- obtain relevant data from _MOD source table
 --
 sql_stmt := 'select /*+ PARALLEL 4 */ statefp,countyfp,cousubfp,funcstat from ' || v_temp_tbl || ' order by statefp,countyfp';
 EXECUTE immediate sql_stmt bulk collect INTO array_state2,array_county2,array_cousub,array_funcstat;
 --
 -- loop through arrays to obtain data
 --
 FOR i IN 1..array_state2.LAST
 LOOP
  v_state2 := array_state2(i);
  v_county2 := array_county2(i);
  v_cousubfp := array_cousub(i);
  v_funcstat := array_funcstat(i);
  -- consider data only if funcstat A,B,or C
  IF v_funcstat IN ('A','B','C') THEN
    counter := counter + 1;
  END IF;
  -- insure data not already loaded
  IF counter > 0 THEN
   sql_stmt := 'select /*+ PARALLEL 4 */ count(*) from cousub_list where statefp = :1 and countyfp = :2 and cousubfp = :3';
   EXECUTE immediate sql_stmt INTO exists_count USING v_state2,v_county2,v_cousubfp;
   --
   -- load ALL cousubs for a particular state,county if at least one cousub is either an A,B,or C
   -- process is redudant but executes quickly
   IF exists_count = 0 THEN
    sql_stmt := 'insert /*+ APPEND */ into cousub_list select /*+ PARALLEL 4 */ statefp,countyfp,cousubfp,sdogeometry,name from ' || v_temp_tbl || ' where statefp = :1 and countyfp = :2';
    EXECUTE immediate sql_stmt USING v_state2,v_county2;
    -- commit;
    sql_stmt := 'commit';
    EXECUTE immediate sql_stmt;
   END IF;
  END IF;
  -- reset counter
  counter := 0;
 END LOOP;
 sql_stmt := 'create unique index cousub_list_uk on cousub_list (statefp,countyfp,cousubfp) nologging';
 EXECUTE immediate sql_stmt;
 --
 -- create source table SL060/SL070 (COUSUB)
 --
 sql_stmt := 'select count(*) from user_tables where table_name = :1';
 EXECUTE immediate sql_stmt INTO exists_count USING source_tbl_name;
 IF exists_count > 0 THEN
  sql_stmt  := 'drop table :1 purge';
  EXECUTE immediate sql_stmt USING source_tbl_name;
 END IF;
 exists_count := 0;
 sql_stmt := 'create table ' || source_tbl_name || ' nologging as select /*+ PARALLEL 4 */ a.* from ' || v_temp_tbl || ' a, cousub_list b where (a.statefp = b.statefp and a.countyfp = b.countyfp and a.cousubfp = b.cousubfp)';
 EXECUTE immediate sql_stmt;
 EXCEPTION
  WHEN OTHERS THEN
   dbms_output.put_line (SQLERRM);
   dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
   -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
   -- RETURN;
   RAISE;
END create_cousub_estimates;
--
PROCEDURE valtopo (topology VARCHAR2, state_tbl VARCHAR2, release VARCHAR2, deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: valtopo
 # Author: J Sidhwaney
 # Creation Date: Fall 2008
 # Recent Revision: 07/07/2009 - mz - put into package; modified passed parameters
 #                  05/18/2011 - Salman - modified mbr selection query 
 #
 # Purpose: The purpose of this procedure is to validate the topology by state
 #
 # Dependencies:
 # - mt_util_topo_val.validate_topology
 #
 ###################################################################################################################
*/
v_process VARCHAR2(100) := 'Validate Topology';    -- Process
v_step VARCHAR2(4000);                             -- Step
v_start_time TIMESTAMP;                            -- Start time
v_end_time TIMESTAMP;                              -- End time
v_elapsed_time interval DAY(5) TO second (2);      -- Elapsed time
v_proc_start_time TIMESTAMP;                       -- Procedure Start Time
v_proc_step VARCHAR2(4000);                        -- Procedure Step
v_proc_end_time TIMESTAMP;                         -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2); -- Procedure Elapsed time
sql_stmt VARCHAR2(4000);                           -- Dynamic SQL Statement
vstate VARCHAR2(2);
array_state MDSYS.STRING_ARRAY;
v_ord1 NUMBER;
v_ord2 NUMBER;
v_ord3 NUMBER;
v_ord4 NUMBER;
vflag VARCHAR2(100);
vstatefp VARCHAR2(2);
mbr_geom SDO_GEOMETRY;
alpha  NUMBER(4,3) := 0.001;                      -- MBR Buffer 
BEGIN
 --
 -- Set Topo Map maximum memory size
 --
 sdo_topo_map.set_max_memory_size(2147483648);
 --
 -- tracking
 --
 v_proc_start_time := systimestamp;
 v_proc_step := v_process;
 --
 -- Get a list of STATEFPs from the STATE (SL040) table
 --
 -- If the topology does not contain any tables with STATEFP, then this procedure will have to
 --  be revised.  Also, you can obtain a list of STATEFPs from any table that contains STATEFPs
 --  by doing a DISTINCT STATEFP on the selection query
 --
 sql_stmt := 'select statefp from ' || state_tbl || ' order by statefp';
 EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO array_state;
 --
 -- Validate topology partitioned by each state's MBR (Minimum Bound Rectangle)
 --
 -- This technique may not validate the topology 100% it but will get close to that percentage
 --
 FOR c IN 1..array_state.COUNT
  LOOP
   -- get statefp
   vstate := array_state(c);
   dbms_output.put_line(' processing state ' || vstate);
   -- tracking
   v_step := 'Processing State ' || vstate;
   v_start_time := systimestamp;
   --
   -- get mbr ordinates for the particular state
   sql_stmt := 'SELECT /*+ PARALLEL 4 */ sdo_geom.sdo_mbr(a.sdogeometry) 
                     FROM '|| state_tbl ||' a WHERE a.statefp = :1';
   EXECUTE immediate sql_stmt INTO mbr_geom USING vstate;
   --
   -- Extracting all 4 ordinates
   --
   v_ord1 := mbr_geom.sdo_ordinates(1);
   v_ord2 := mbr_geom.sdo_ordinates(2);
   v_ord3 := mbr_geom.sdo_ordinates(3);
   v_ord4 := mbr_geom.sdo_ordinates(4);
   -- mbr buffer (alpha) 
   v_ord1 := v_ord1 - alpha;
   v_ord2 := v_ord2 - alpha;
   v_ord3 := v_ord3 + alpha;
   v_ord4 := v_ord4 + alpha;
   --
   -- call external package to validate the topology within the particular state's MBR
   sql_stmt := 'select mt_util_topo_val.validate_topology(:1, :2, :3, :4, :5, :6) FROM DUAL';
   EXECUTE IMMEDIATE sql_stmt INTO vflag USING deploy,topology,v_ord1,v_ord2,v_ord3,v_ord4;
   --
   -- At the end of the execution the following is displayed using dbms_output
   -- The value for vflag should always be TRUE or the topo is invalid
   dbms_output.put_line(' MBR Coordinates are:  ' || v_ord1||','||v_ord2||','||v_ord3||','||v_ord4||' TOPO '||vflag);
   -- tracking
   v_end_time := systimestamp;
   v_elapsed_time := v_end_time - v_start_time;
   GZ_TOPO_BUILD.gen_tracking_log(topology,'Statefp = '||vstate||' TOPO '||vflag,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,'MBR Coordinates: '||v_ord1||','||v_ord2||','||v_ord3||','||v_ord4||' TOPO '||vflag,deploy);
   --
  END LOOP;
 --
 -- end tracking for procedure
 --
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'N/A',v_proc_step,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,release,'N/A',deploy);
 -- error handling
 EXCEPTION
 WHEN OTHERS THEN
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (sql_stmt);
  dbms_output.put_line ('Release: ' || release);
  dbms_output.put_line ('Deploy: ' || deploy);
  dbms_output.put_line ('Topology: ' || topology);
  dbms_output.put_line ('State: ' || vstate);
  dbms_output.put_line (v_ord1 || ' ' || v_ord2 || ' ' || v_ord3 || ' ' || v_ord4);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
  -- RETURN;
  RAISE;
END valtopo;
--
PROCEDURE valtopo2 (topology VARCHAR2, state_table VARCHAR2, release VARCHAR2, deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: valtopo
 # Author: J Sidhwaney
 # Creation Date: Fall 2008
 # Revision History:
 # 07/07/2009 - mz - put into package; modified passed parameters
 # 02/22/2010 - ss - user must eneter the name of the table where the 
 #                   state SDO geometries can be found
 #
 # Purpose: The purpose of this procedure is to validate the topology by state
 #
 # Dependencies:
 # - mt_util_topo_val.validate_topology
 #
 # Created version 2:  2/22/2010 to allow user to eneter the state table where the 
 #         state SDO geometries can be found
 #
 ###################################################################################################################
*/
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
vstate VARCHAR2(2);
array_state MDSYS.STRING_ARRAY;
v_ord1 NUMBER;
v_ord2 NUMBER;
v_ord3 NUMBER;
v_ord4 NUMBER;
vflag VARCHAR2(100);
vstatefp VARCHAR2(2);
BEGIN
 --
 -- Set Topo Map maximum memory size
 --
 sdo_topo_map.set_max_memory_size(2147483648);
 --
 -- Get a list of STATEFPs from the STATE (SL040) table
 --
 -- If the topology does not contain any tables with STATEFP, then this procedure will have to
 --  be revised.  Also, you can obtain a list of STATEFPs from any table that contains STATEFPs
 --  by doing a DISTINCT STATEFP on the selection query
 --
 sql_stmt := 'select statefp from ' ||state_table|| ' order by statefp';
 EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO array_state;
 --
 -- Validate topology partitioned by each state's MBR (Minimum Bound Rectangle)
 --
 -- This technique may not validate the topology 100% it but will get close to that percentage
 --
 FOR c IN 1..array_state.COUNT
  LOOP
   -- get statefp
   vstate := array_state(c);
   dbms_output.put_line(' processing state ' || vstate);
   --
   -- get mbr ordinates for the particular state
   sql_stmt := 'select sdo_geom.sdo_min_mbr_ordinate(a.sdogeometry,1),sdo_geom.sdo_min_mbr_ordinate(a.sdogeometry,2),sdo_geom.sdo_max_mbr_ordinate(a.sdogeometry,1),sdo_geom.sdo_max_mbr_ordinate(a.sdogeometry,2) from ' ||state_table||' a where statefp = :1';
   EXECUTE immediate sql_stmt INTO v_ord1,v_ord2,v_ord3,v_ord4 USING vstate;
   --
   -- call external package to validate the topology within the particular state's MBR
   sql_stmt := 'select mt_util_topo_val.validate_topology(:1, :2, :3, :4, :5, :6) FROM DUAL';
   EXECUTE IMMEDIATE sql_stmt INTO vflag USING deploy,topology,v_ord1,v_ord2,v_ord3,v_ord4;
   --
   -- At the end of the execution the following is displayed using dbms_output
   -- The value for vflag should always be TRUE or the topo is invalid
   dbms_output.put_line(' MBR Coordinates are:  ' || v_ord1||','||v_ord2||','||v_ord3||','||v_ord4||' TOPO '||vflag);
   IF (vflag = 'FALSE') THEN
      dbms_output.put_line('  ');
      dbms_output.put_line('*****************************************************  ');
      dbms_output.put_line('FAILURE!!! State '||vstate||'has invalid topology !!!');
      dbms_output.put_line('*****************************************************  ');
      dbms_output.put_line('  ');
   END IF;
  END LOOP;
 -- error handling
 EXCEPTION
 WHEN OTHERS THEN
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (sql_stmt);
  dbms_output.put_line ('Release: ' || release);
  dbms_output.put_line ('Deploy: ' || deploy);
  dbms_output.put_line ('Topology: ' || topology);
  dbms_output.put_line ('State: ' || vstate);
  dbms_output.put_line (v_ord1 || ' ' || v_ord2 || ' ' || v_ord3 || ' ' || v_ord4);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
  -- RETURN;
  RAISE;
END valtopo2;
--
PROCEDURE update_data_length (topology VARCHAR2,topo_universe VARCHAR2,release VARCHAR2,deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: update_data_length
 # Author: mz
 # Creation Date: 07/08/2009
 # Recent Revisions:08/16/2011 (Salman)
 #
 # Purpose:
 #   The purpose of this procedure is modify the data_length in the SL/FSL tables to conform
 #   with the specification for a particular release
 #
 # Dependencies:
 #  - TOPO_FIELD_DEF
 #  - TOPO_UNIVERSE
 #
 ###################################################################################################################
*/
--
-- Tracking variables
--
v_process VARCHAR2(100) := 'Update Data Length';   -- Process
v_step    VARCHAR2(4000);                          -- Step
v_start_time TIMESTAMP;                            -- Start time
v_end_time TIMESTAMP;                              -- End time
v_elapsed_time interval DAY(5) TO second (2);      -- Elapsed time
v_proc_start_time TIMESTAMP;                       -- Procedure Start Time
v_proc_step VARCHAR2(4000);                        -- Procedure Step
v_proc_end_time TIMESTAMP;                         -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2); -- Procedure Elapsed time
sql_stmt VARCHAR2(4000);                           -- Dynamic SQL Statement
tbl_array mdsys.string_array;
tbl VARCHAR2(32);
col_array mdsys.string_array;
col VARCHAR2(32);
dt_array mdsys.string_array;
dt VARCHAR2(32);
dl_array mdsys.string_array;
dl VARCHAR2(32);
collen_array mdsys.sdo_number_array;
collen NUMBER;
BEGIN
 --
 -- purge recyclebin
 --
 -- On some of the tests problems were encountered related to the 'alter table' command when the recyclebin contained
 --   earlier versions of the same table definition.  Therefore, the 'purge recyclebin' command was added to the code
 --
 sql_stmt := 'purge recyclebin';
 EXECUTE immediate sql_stmt;
 --
 -- Get the data_lengths from the TOPO_FIELD_DEF table
 --
 --sql_stmt := 'select b.table_name,b.column_name,b.data_type,b.data_length,a.collen from topo_field_def a, cols b where a.mtcoldef = b.column_name and a.collen <> b.data_length and b.table_name not like ''TOPO%'' order by b.table_name,b.column_name';
 sql_stmt := 'select b.table_name,b.column_name,b.data_type,b.data_length,a.collen from topo_field_def a, cols b, '||topo_universe||' c where a.mtcoldef = b.column_name and a.collen <> b.data_length and b.table_name = '''||topology||'_''||c.table_name and b.table_name not like ''TOPO%'' order by b.table_name,b.column_name';
 EXECUTE immediate sql_stmt bulk collect INTO tbl_array,col_array,dt_array,dl_array,collen_array;
 --
 IF tbl_array.COUNT > 0 THEN
  --
  -- Cycle through each relevant table/column and alter the table definition where applicable
  --
  FOR i IN 1..tbl_array.LAST
  LOOP
   tbl := tbl_array(i);
   col := col_array(i);
   dt := dt_array(i);
   dl := dl_array(i);
   collen := collen_array(i);
   sql_stmt := 'alter table ' || tbl || ' modify ' || col || ' ' || dt || '(' || collen || ')';
   EXECUTE immediate sql_stmt;
  END LOOP;
  --
 END IF;
 --
 -- error handling
 --
 EXCEPTION
 WHEN OTHERS THEN
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (sql_stmt);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
  -- RETURN;
  RAISE;
END update_data_length;
--
PROCEDURE build_face_cols_mz (topology VARCHAR2,pFaceTable VARCHAR2,pFaceDollarTable VARCHAR2,topo_universe VARCHAR2)
AS
/**
 #############################################################################################################
 # Program Name: build_face_cols
 # Author: Nick.A.Padfield
 # Creation Date: 12/30/2008
 # Modification History: 07/14/2009 - mz - Automated
 #
 # Purpose: The purpose of this program is to create and update the geo_id and attribute (STATEFP,COUNTYFP, etc.)
 #          fields in FACE
 #
 # Required parameters:
 #   pFaceTable - Name of FACE table
 #   pFaceDollarTable - Name of FACE$ table
 #
 # Dependencies: None
 #
 #############################################################################################################
*/
vFaceTable VARCHAR2(30) := UPPER(SUBSTR(pFaceTable,1,30));
vFaceDollarTable VARCHAR2(30) := UPPER(SUBSTR(pFaceDollarTable,1,30));
sql_stmt VARCHAR2(4000);
vContinueStatus  BOOLEAN := TRUE;
vColumnList MDSYS.STRING_ARRAY;
vCurColumn VARCHAR2(30);
vTblList MDSYS.STRING_ARRAY;
vCurTbl VARCHAR2(30);
vTmpCount NUMBER;
vTmpCount2 NUMBER;
vTmpTable VARCHAR2(30) := 'X'||topology||'FACETMP';
vTmpTableIDX VARCHAR2(30) := 'X'||topology||'FACETMP_IDX';
vDataLength NUMBER;
vGeoId VARCHAR2(4000);
vCnt NUMBER;
vCnt2 NUMBER;
vCnt3 NUMBER;
BEGIN
 -----------------------------------------------------------------------------
 -- Validation
 --------------
 -- Check if FACE table exists
 sql_stmt := 'SELECT count(*) FROM user_tables WHERE table_name = :1';
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount USING vFaceTable;
 IF (vTmpCount <> 1) THEN
  vContinueStatus := FALSE;
  dbms_output.put_line('pFaceTable: '||vFaceTable||' does not exist in current schema');
 END IF;
 --------------
 -- Check if FACE$ table exists
 sql_stmt := 'SELECT count(*) FROM user_tables WHERE table_name = :1';
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount USING vFaceDollarTable;
 IF (vTmpCount <> 1) THEN
  vContinueStatus := FALSE;
  dbms_output.put_line('pFaceDollarTable: '||vFaceDollarTable||' does not exist in current schema');
 END IF;
 --------------
 -- Check if FACE_ID column exists
 sql_stmt := 'SELECT count(*) FROM user_tab_columns WHERE table_name = :1 AND column_name = :2';
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount USING vFaceTable,'FACE_ID';
 IF (vTmpCount <> 1) THEN
  vContinueStatus := FALSE;
  dbms_output.put_line('pFaceTable: '||vFaceTable||' does not contain a FACE_ID column');
 END IF;
 --------------
 -- Check if index exists on FACE_ID column of FACE
 sql_stmt := 'SELECT count(*) FROM user_ind_columns WHERE table_name = :1 AND column_name = :2';
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount USING vFaceTable,'FACE_ID';
 IF (vTmpCount <> 1) THEN
  vContinueStatus := FALSE;
  dbms_output.put_line('pFaceTable: '||vFaceTable||', FACE_ID column does not have an index');
 END IF;
 --------------
 -- Check to insure FACE count = FACE$ count
 sql_stmt := 'SELECT count(*) FROM '||vFaceTable;
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount;
 sql_stmt := 'SELECT count(*) FROM '||vFaceDollarTable;
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount2;
 IF (vTmpCount <> vTmpCount2) THEN
  vContinueStatus := FALSE;
  dbms_output.put_line('The record counts between '||vFaceTable||' and '||vFaceDollarTable||' are not equal');
 END IF;
 --------------
 -----------------------------------------------------------------------------
 -- If parameters are validated, then get into performing the process...
 IF vContinueStatus THEN
  --------------------------------------------------------------------------
  -- Insert Columns Into FACE table
  sql_stmt := 'select table_name,tbl_pf from '||topo_universe||' where topology = :1 and tbl_pf is not null order by tbl_pf';
  EXECUTE immediate sql_stmt bulk collect INTO vTblList,vColumnList USING topology;
  --
  -- loop thru table/column list
  --
  FOR i IN vTblList.FIRST..vTblList.LAST
  LOOP
   vCurTbl := topology||'_'||vTblList(i);
   vCurColumn := vColumnList(i);
   -----------------------------------------------------------------------
   -- Check to see if column pre-exists
   sql_stmt := 'SELECT count(*) FROM user_tab_columns WHERE table_name = :1 AND column_name = :2';
   EXECUTE IMMEDIATE sql_stmt INTO vTmpCount USING vFaceTable,vCurColumn;
   IF (vTmpCount = 0) THEN
    -- Add new column
    -- get data length
    sql_stmt := 'select data_length from user_tab_columns where table_name = :1 and column_name = :2';
    EXECUTE immediate sql_stmt INTO vDataLength USING vCurTbl,vCurColumn;
    --
    -- alter table add new column
    --
    sql_stmt := 'ALTER TABLE '||vFaceTable||' ADD '||vCurColumn||' VARCHAR2(' || vDataLength || ')';
    -- if the column is PLACEFP a default value of '00000' is added to the alter table statement
    -- because of issues related to loading FSL tables with null PLACEFPs derived from FACE table
    EXECUTE IMMEDIATE sql_stmt;
    --IF vCurColumn = 'PLACEFP' THEN
     --sql_stmt := 'ALTER TABLE '||vFaceTable||' MODIFY '||vCurColumn||' VARCHAR2(' || vDataLength || ') default ''00000''';
     --EXECUTE IMMEDIATE sql_stmt;
     -- set default '00000'
     --sql_stmt := 'UPDATE /*+ PARALLEL 4 */ '||vFaceTable||' f Set f.'||vCurColumn||' = ''00000''';
     --EXECUTE IMMEDIATE sql_stmt;
     --COMMIT;
    --END IF;
   END IF;
   -----------------------------------------------------------------------
   --
   -- create temp table
   sql_stmt := 'CREATE TABLE ' || vTmpTable || '  NOLOGGING AS SELECT t.topo_id AS face_id,TO_CHAR(sl.' || vCurColumn || ') AS ' || vCurColumn || ' FROM ' || vCurTbl || ' sl, TABLE(sl.topogeom.get_topo_elements()) t';
   EXECUTE IMMEDIATE sql_stmt;
   -- create index for temp table face_id
   sql_stmt := 'CREATE INDEX ' || vTmpTableIDX ||' ON ' || vTmpTable || '(face_id) nologging';
   EXECUTE IMMEDIATE sql_stmt;
   -- update face table
   sql_stmt := 'UPDATE /*+ PARALLEL 4 */ ' || vFaceTable || ' f SET f.' || vCurColumn || ' = (SELECT t.' || vCurColumn || ' FROM ' || vTmpTable || ' t WHERE t.face_id = f.face_id) WHERE EXISTS (SELECT t.' || VCurColumn || ' FROM ' || vTmpTable || ' t WHERE t.face_id = f.face_id)';
   EXECUTE IMMEDIATE sql_stmt;
   COMMIT;
   -- drop temp table
   sql_stmt := 'DROP TABLE '||vTmpTable||' PURGE';
   EXECUTE IMMEDIATE sql_stmt;
  END LOOP;
  --
  -- original code for geoid
  --
  -- sql_stmt := 'UPDATE /*+ PARALLEL 4 */ face f SET f.geoid = DECODE(f.aiannhce,NULL,''____'',f.aiannhce)||'',''||DECODE(f.anrcfp,NULL,''____'',f.anrcfp)||'',
  -- ''||DECODE(f.artli,NULL,''_'',f.artli)||'',''||DECODE(f.cbsafp,NULL,''_____'',f.cbsafp)||'',''||DECODE(f.cdfp,NULL,''__'',f.cdfp)||'',
  -- ''||DECODE(f.cnectafp,NULL,''___'',f.cnectafp)||'',''||DECODE(f.countyfp,NULL,''___'',f.countyfp)||'',''||DECODE(f.cousubfp,NULL,''_____'',f.cousubfp)||'',
  -- ''||DECODE(f.csafp,NULL,''___'',f.csafp)||'',''||DECODE(f.divisionce,NULL,''_'',f.divisionce)||'',''||DECODE(f.metdivfp,NULL,''_____'',f.metdivfp)||'',
  -- ''||DECODE(f.nation,NULL,''_'',f.nation)||'',''||DECODE(f.nectadivfp,NULL,''_____'',f.nectadivfp)||'',''||DECODE(f.nectafp,NULL,''_____'',f.nectafp)||'',''||DECODE(f.placefp,NULL,''_____'',f.placefp)||'',
  -- ''||'||'DECODE(f.puma5ce,NULL,''_____'',f.puma5ce)||'',''||DECODE(f.regionce,NULL,''_'',f.regionce)||'',''||DECODE(f.sddoe_e,NULL,''_____'',f.sddoe_e)||'',''||DECODE(f.sddoe_s,NULL,''_____'',f.sddoe_s)||'',
  -- ''||DECODE(f.sddoe_u,NULL,''_____'',f.sddoe_u)||'',''||DECODE(f.statefp,NULL,''__'',f.statefp)||'',''||DECODE(f.uace,NULL,''_____'',f.uace)';
  --
  -- get count of array cells
  vCnt := 0;
  FOR i IN vTblList.FIRST..vTblList.LAST
   LOOP
    vCnt := vCnt + 1;
   END LOOP;
  -- initialize variable
  vCnt2 := 0;
  --
  -- create geoid column
  --
  vCnt3 := 0;
  sql_stmt := 'SELECT count(*) FROM user_tab_columns WHERE table_name = :1 AND column_name = :2';
  EXECUTE IMMEDIATE sql_stmt INTO vCnt3 USING vFaceTable,'GEOID';
  if vCnt3 = 0
   then
    sql_stmt := 'alter table ' || vFaceTable || ' add geoid varchar2(250)';
    execute immediate sql_stmt;
  end if;
  --
  -- the following loop constructs the GeoId string dynamically
  --
  -- set the initial vGeoId value; the update statement
  vGeoId := 'UPDATE ' || vFaceTable || ' f SET f.geoid = ';
  --
  -- get current column from column list
  FOR i IN vTblList.FIRST..vTblList.LAST
   LOOP
    vCurColumn := vColumnList(i);
    vCnt2 := vCnt2 + 1;
    IF vCnt2 = 1 THEN
     -- beginning of string
     vGeoId := vGeoID || 'DECODE(f.' || vCurColumn || ',NULL,''__'',f.' || vCurColumn || ')||'',';
    END IF;
    IF vCnt2 > 1 AND vCnt2 < vCnt THEN
     -- middle of string
     vGeoId := vGeoId || '''||DECODE(f.' || vCurColumn || ',NULL,''__'',f.' || vCurColumn || ')||'',';
    END IF;
    IF vCnt2 = vCnt THEN
    -- end of string
     vGeoId := vGeoId || '''||DECODE(f.' || vCurColumn || ',NULL,''__'',f.' || vCurColumn || ')';
    END IF;
   END LOOP;
   -- an update statement
   EXECUTE immediate vGeoId;
   COMMIT;
   --
 END IF;
END build_face_cols_mz;
--
PROCEDURE build_face_cols (topology VARCHAR2,pFaceTable VARCHAR2,pFaceDollarTable VARCHAR2,topo_universe VARCHAR2)
AS
/**
 #############################################################################################################
 # Program Name: build_face_cols
 # Author: Nick.A.Padfield
 # Creation Date: 12/30/2008
 # Modification History: 07/14/2009 - mz - Automated, 08/18/2011 - Salman
 #
 # Purpose: The purpose of this program is to create and update the geo_id and attribute (STATEFP,COUNTYFP, etc.)
 #          fields in FACE
 #
 # Required parameters:
 #   pFaceTable - Name of FACE table
 #   pFaceDollarTable - Name of FACE$ table
 #
 # Dependencies: None
 #
 #############################################################################################################
*/
vFaceTable VARCHAR2(32) := UPPER(SUBSTR(pFaceTable,1,30));
vFaceDollarTable VARCHAR2(32) := UPPER(SUBSTR(pFaceDollarTable,1,30));
sql_stmt VARCHAR2(4000);
vContinueStatus  BOOLEAN := TRUE;
vColumnList MDSYS.STRING_ARRAY;
vCurColumn VARCHAR2(32);
vTblList MDSYS.STRING_ARRAY;
vCurTbl VARCHAR2(32);
vColumnList2 MDSYS.STRING_ARRAY;
vCurColumn2 VARCHAR2(32);
vTblList2 MDSYS.STRING_ARRAY;
vCurTbl2 VARCHAR2(32);
vTmpCount NUMBER;
vTmpCount2 NUMBER;
vTmpTable VARCHAR2(32) := 'X'||topology||'FACETMP';
vTmpTableIDX VARCHAR2(32) := 'X'||topology||'FACETMP_IDX';
vDataLength NUMBER;
vGeoId VARCHAR2(4000);
vCnt NUMBER;
vCnt2 NUMBER;
vCnt3 NUMBER;
BEGIN
 -----------------------------------------------------------------------------
 -- Validation
 --------------
 -- Check if FACE table exists
 sql_stmt := 'SELECT count(*) FROM user_tables WHERE table_name = :1';
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount USING vFaceTable;
 IF (vTmpCount <> 1) THEN
  vContinueStatus := FALSE;
  dbms_output.put_line('pFaceTable: '||vFaceTable||' does not exist in current schema');
 END IF;
 --------------
 -- Check if FACE$ table exists
 sql_stmt := 'SELECT count(*) FROM user_tables WHERE table_name = :1';
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount USING vFaceDollarTable;
 IF (vTmpCount <> 1) THEN
  vContinueStatus := FALSE;
  dbms_output.put_line('pFaceDollarTable: '||vFaceDollarTable||' does not exist in current schema');
 END IF;
 --------------
 -- Check if FACE_ID column exists
 sql_stmt := 'SELECT count(*) FROM user_tab_columns WHERE table_name = :1 AND column_name = :2';
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount USING vFaceTable,'FACE_ID';
 IF (vTmpCount <> 1) THEN
  vContinueStatus := FALSE;
  dbms_output.put_line('pFaceTable: '||vFaceTable||' does not contain a FACE_ID column');
 END IF;
 --------------
 -- Check if index exists on FACE_ID column of FACE
 sql_stmt := 'SELECT count(*) FROM user_ind_columns WHERE table_name = :1 AND column_name = :2';
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount USING vFaceTable,'FACE_ID';
 IF (vTmpCount <> 1) THEN
  vContinueStatus := FALSE;
  dbms_output.put_line('pFaceTable: '||vFaceTable||', FACE_ID column does not have an index');
 END IF;
 --------------
 -- Check to insure FACE count = FACE$ count
 sql_stmt := 'SELECT count(*) FROM '||vFaceTable;
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount;
 sql_stmt := 'SELECT count(*) FROM '||vFaceDollarTable;
 EXECUTE IMMEDIATE sql_stmt INTO vTmpCount2;
 IF (vTmpCount <> vTmpCount2) THEN
  vContinueStatus := FALSE;
  dbms_output.put_line('The record counts between '||vFaceTable||' and '||vFaceDollarTable||' are not equal');
 END IF;
 --------------
 -----------------------------------------------------------------------------
 -- If parameters are validated, then get into performing the process...
 IF vContinueStatus THEN
  --------------------------------------------------------------------------
  -- Insert Columns Into FACE table
  sql_stmt := 'select table_name,tbl_pf from '||topo_universe||' where topology = :1 and tbl_pf is not null order by tbl_pf';
  EXECUTE immediate sql_stmt bulk collect INTO vTblList,vColumnList USING topology;
  --
  -- loop thru table/column list
  --
  FOR i IN vTblList.FIRST..vTblList.LAST
  LOOP
   vCurTbl := topology||'_'||vTblList(i);
   vCurColumn := vColumnList(i);
   -----------------------------------------------------------------------
   -- Check to see if column pre-exists
   sql_stmt := 'SELECT count(*) FROM user_tab_columns WHERE table_name = :1 AND column_name = :2';
   EXECUTE IMMEDIATE sql_stmt INTO vTmpCount USING vFaceTable,vCurColumn;
   IF (vTmpCount = 0) THEN
    -- Add new column
    -- get data length
    sql_stmt := 'select data_length from user_tab_columns where table_name = :1 and column_name = :2';
    EXECUTE immediate sql_stmt INTO vDataLength USING vCurTbl,vCurColumn;
    --
    -- alter table add new column
    --
    sql_stmt := 'ALTER TABLE '||vFaceTable||' ADD '||vCurColumn||' VARCHAR2(' || vDataLength || ')';
    EXECUTE IMMEDIATE sql_stmt;
   END IF;
   -----------------------------------------------------------------------
   --
   -- create temp table
   sql_stmt := 'CREATE TABLE ' || vTmpTable || '  NOLOGGING AS SELECT t.topo_id AS face_id,TO_CHAR(sl.' || vCurColumn || ') AS ' || vCurColumn || ' FROM ' || vCurTbl || ' sl, TABLE(sl.topogeom.get_topo_elements()) t';
   EXECUTE IMMEDIATE sql_stmt;
   -- create index for temp table face_id
   sql_stmt := 'CREATE INDEX ' || vTmpTableIDX ||' ON ' || vTmpTable || '(face_id) nologging';
   EXECUTE IMMEDIATE sql_stmt;
   -- update face table
   sql_stmt := 'UPDATE /*+ PARALLEL 4 */ ' || vFaceTable || ' f SET f.' || vCurColumn || ' = (SELECT t.' || vCurColumn || ' FROM ' || vTmpTable || ' t WHERE t.face_id = f.face_id) WHERE EXISTS (SELECT t.' || VCurColumn || ' FROM ' || vTmpTable || ' t WHERE t.face_id = f.face_id)';
   EXECUTE IMMEDIATE sql_stmt;
   COMMIT;
   -- drop temp table
   sql_stmt := 'DROP TABLE '||vTmpTable||' PURGE';
   EXECUTE IMMEDIATE sql_stmt;
  END LOOP;
  --
  -- New Code: Addtional Face Columns (Salman) 
  --
  sql_stmt := 'select table_name,add_face_cols from '||topo_universe||' where topology = :1 and add_face_cols is not null order by add_face_cols';
  EXECUTE immediate sql_stmt bulk collect INTO vTblList2,vColumnList2 USING topology;
  --
  -- if data exists
  --
  IF vTblList2.COUNT > 0 THEN
   --
   -- loop thru table/column list
   --
   FOR i IN vTblList2.FIRST..vTblList2.LAST
   LOOP
    vCurTbl2 := topology||'_'||vTblList2(i);
    vCurColumn2 := vColumnList2(i);
    -----------------------------------------------------------------------
    -- Check to see if column pre-exists
    sql_stmt := 'SELECT count(*) FROM user_tab_columns WHERE table_name = :1 AND column_name = :2';
    EXECUTE IMMEDIATE sql_stmt INTO vTmpCount USING vFaceTable,vCurColumn2;
    IF (vTmpCount = 0) THEN
     -- Add new column
     -- get data length
     sql_stmt := 'select data_length from user_tab_columns where table_name = :1 and column_name = :2';
     EXECUTE immediate sql_stmt INTO vDataLength USING vCurTbl2,vCurColumn2;
     --
     -- alter table add new column
     --
     sql_stmt := 'ALTER TABLE '||vFaceTable||' ADD '||vCurColumn2||' VARCHAR2(' || vDataLength || ')';
     EXECUTE IMMEDIATE sql_stmt;
    END IF;
    -----------------------------------------------------------------------
    --
    -- create temp table
    sql_stmt := 'CREATE TABLE ' || vTmpTable || '  NOLOGGING AS SELECT t.topo_id AS face_id,TO_CHAR(sl.' || vCurColumn2 || ') AS ' || vCurColumn2 || ' FROM ' || vCurTbl2 || ' sl, TABLE(sl.topogeom.get_topo_elements()) t';
    EXECUTE IMMEDIATE sql_stmt;
    -- create index for temp table face_id
    sql_stmt := 'CREATE INDEX ' || vTmpTableIDX ||' ON ' || vTmpTable || '(face_id) nologging';
    EXECUTE IMMEDIATE sql_stmt;
    -- update face table
    sql_stmt := 'UPDATE /*+ PARALLEL 4 */ ' || vFaceTable || ' f SET f.' || vCurColumn2 || ' = (SELECT t.' || vCurColumn2 || ' FROM ' || vTmpTable || ' t WHERE t.face_id = f.face_id) WHERE EXISTS (SELECT t.' || VCurColumn2 || ' FROM ' || vTmpTable || ' t WHERE t.face_id = f.face_id)';
    EXECUTE IMMEDIATE sql_stmt;
    COMMIT;
    -- drop temp table
    sql_stmt := 'DROP TABLE '||vTmpTable||' PURGE';
    EXECUTE IMMEDIATE sql_stmt;
   END LOOP;
   --
  END IF;
  --
  -- get count of array cells
  vCnt := 0;
  FOR i IN vTblList.FIRST..vTblList.LAST
   LOOP
    vCnt := vCnt + 1;
   END LOOP;
  -- initialize variable
  vCnt2 := 0;
  --
  -- create geoid column
  --
  vCnt3 := 0;
  sql_stmt := 'SELECT count(*) FROM user_tab_columns WHERE table_name = :1 AND column_name = :2';
  EXECUTE IMMEDIATE sql_stmt INTO vCnt3 USING vFaceTable,'GEOID';
  if vCnt3 = 0
   then
    sql_stmt := 'alter table ' || vFaceTable || ' add geoid varchar2(250)';
    execute immediate sql_stmt;
  end if;
  --
  -- the following loop constructs the GeoId string dynamically
  --
  -- set the initial vGeoId value; the update statement
  vGeoId := 'UPDATE ' || vFaceTable || ' f SET f.geoid = ';
  --
  -- get current column from column list
  FOR i IN vTblList.FIRST..vTblList.LAST
   LOOP
    vCurColumn := vColumnList(i);
    vCnt2 := vCnt2 + 1;
    IF vCnt2 = 1 THEN
     -- beginning of string
     vGeoId := vGeoID || 'DECODE(f.' || vCurColumn || ',NULL,''__'',f.' || vCurColumn || ')||'',';
    END IF;
    IF vCnt2 > 1 AND vCnt2 < vCnt THEN
     -- middle of string
     vGeoId := vGeoId || '''||DECODE(f.' || vCurColumn || ',NULL,''__'',f.' || vCurColumn || ')||'',';
    END IF;
    IF vCnt2 = vCnt THEN
    -- end of string
     vGeoId := vGeoId || '''||DECODE(f.' || vCurColumn || ',NULL,''__'',f.' || vCurColumn || ')';
    END IF;
   END LOOP;
  --
  -- if data exists
  --
  IF vTblList2.COUNT > 0 THEN
   --
   -- get count of array cells
   vCnt := 0;
   FOR i IN vTblList2.FIRST..vTblList2.LAST
    LOOP
     vCnt := vCnt + 1;
    END LOOP;
   -- initialize variable
   vCnt2 := 0;
   --
   -- get current column from column list 2
   FOR i IN vTblList2.FIRST..vTblList2.LAST
    LOOP
     vCurColumn2 := vColumnList2(i);
     vCnt2 := vCnt2 + 1;
     IF vCnt2 = 1 THEN
      -- beginning of string
      vGeoId := vGeoID || '||'',''||DECODE(f.' || vCurColumn2 || ',NULL,''__'',f.' || vCurColumn2 || ')||'',';
     END IF;
     IF vCnt2 > 1 AND vCnt2 < vCnt THEN
      -- middle of string
      vGeoId := vGeoId || '''||DECODE(f.' || vCurColumn2 || ',NULL,''__'',f.' || vCurColumn2 || ')||'',';
     END IF;
     IF vCnt2 = vCnt THEN
     -- end of string
      vGeoId := vGeoId || '''||DECODE(f.' || vCurColumn2 || ',NULL,''__'',f.' || vCurColumn2 || ')';
     END IF;
    END LOOP;
   --
  END IF;  
  -- an update statement
  EXECUTE immediate vGeoId;
  COMMIT;
  --
 END IF;
 --
END build_face_cols;
--
PROCEDURE load_face (topology VARCHAR2,face_tbl VARCHAR2,face_dollar_tbl VARCHAR2,topo_universe VARCHAR2,release VARCHAR2,deploy VARCHAR2)
AS
/**
 #############################################################################################################
 # Program Name: load_face
 # Author: J. Sidhwaney
 # Creation Date: 12/30/2008
 # Modification History:
 #  07/16/2009 - mz - Automated code which was previously contained in a script and in a procedure named fill_face
 #
 # Purpose: Populates FACE table
 #
 # Required parameters:
 #  - topology - Name of topology
 #  - face_tbl - Name of face table
 #  - release - Release name
 #  - deploy - Deployment schema
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #
 #############################################################################################################
*/
array_faceid MDSYS.sdo_number_array;
sql_stmt VARCHAR2(4000);                                -- Dynamic SQL Statement
vface VARCHAR2(100);
topo_face_tbl VARCHAR2(100) := face_tbl;
vface_tbl VARCHAR2(100) := topology||'_'||face_tbl;
v_tg_layer_id NUMBER;
v_tg_layer_type VARCHAR2(10);
v_tg_lt NUMBER;
v_tbl_keys VARCHAR2(250);
vRowList MDSYS.STRING_ARRAY;
vRow ROWID;
v_process VARCHAR2(100) := 'Load Face Feature Table';   -- Process
v_step VARCHAR2(4000);                                  -- Step
v_start_time TIMESTAMP;                                 -- Start time
v_end_time TIMESTAMP;                                   -- End time
v_elapsed_time interval DAY(5) TO second (2);           -- Elapsed time
v_proc_start_time TIMESTAMP;                            -- Procedure Start Time
v_proc_step VARCHAR2(4000);                             -- Procedure Step
v_proc_end_time TIMESTAMP;                              -- Procedure End time
v_proc_elapsed_time interval DAY(5) TO second (2);      -- Procedure Elapsed time
cnt NUMBER;
BEGIN
 -- step tracking
 v_proc_start_time := systimestamp;
 v_proc_step := v_process;
 v_start_time := systimestamp;
 v_step := 'Step 1: Insert Face_IDs from Face$ and Calculate Topogeom';
 cnt := 0;
 --
 -- get tg_layer_id and tg_layer_type for particular topology and face_tbl
 --
 sql_stmt := 'select tg_layer_id,tg_layer_type from user_sdo_topo_info where topology = :1 and table_name = :2';
 EXECUTE immediate sql_stmt INTO v_tg_layer_id,v_tg_layer_type USING topology,vface_tbl;
 -- identify the tg_layer_type
 CASE
  WHEN v_tg_layer_type = 'POINT' THEN v_tg_lt := 1;
  WHEN v_tg_layer_type = 'LINE' THEN v_tg_lt := 2;
  WHEN v_tg_layer_type = 'POLYGON' THEN v_tg_lt := 3;
  WHEN v_tg_layer_type = 'COLLECTION' THEN v_tg_lt := 4;
  ELSE v_tg_lt := 0; -- this will generate an error
 END CASE;
 --
 -- bulk fetch face_id from face$
 sql_stmt := 'SELECT face_id FROM ' || face_dollar_tbl;
 EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO array_faceid;
 --
 -- insert values into face table; face_id and topogeom
 FOR i IN 1..array_faceid.COUNT
 LOOP
  vface := array_faceid(i);
  --
  sql_stmt := 'INSERT INTO ' || vface_tbl || ' VALUES (:1,SDO_TOPO_GEOMETRY(:2,:3,:4,SDO_TOPO_OBJECT_ARRAY(SDO_TOPO_OBJECT (:5,:6))))';
  EXECUTE immediate sql_stmt USING vface,topology,v_tg_lt,v_tg_layer_id,vface,v_tg_lt;
  COMMIT;
  --
 END LOOP;
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,vface_tbl,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
 --
 -- get face table keys; generally just face_id
 sql_stmt := 'select tbl_keys from '||topo_universe||' where topology = :1 and table_name = :2';
 EXECUTE immediate sql_stmt INTO v_tbl_keys USING topology,topo_face_tbl;
 --
 -- create index on tbl_keys
 sql_stmt := 'create unique index ' || vface_tbl || '_' || v_tbl_keys || '_uk on ' || vface_tbl || '(' || v_tbl_keys || ') nologging';
 EXECUTE immediate sql_stmt;
 -- tracking
 v_start_time := systimestamp;
 v_step := 'Step 2: Build Face Feature Attribute Columns';
 --
 -- call procedure to build attribute and geo_id columns in FACE table
 GZ_TOPO_BUILD.build_face_cols (topology, vface_tbl, face_dollar_tbl, topo_universe);
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,vface_tbl,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,'GZ_TOPO_BUILD.build_face_cols();',deploy);
 -- tracking
 v_step := 'Step 3: Calculate Sdogeometry Column';
 v_start_time := systimestamp;
 --
 -- add sdogeometry field
 sql_stmt := 'alter table ' || vface_tbl || ' add sdogeometry mdsys.sdo_geometry';
 EXECUTE immediate sql_stmt;
 --
 -- update sdogeometry field from topogeom
 sql_stmt := 'select rowid from '|| vface_tbl ||' order by rowid';
 EXECUTE immediate sql_stmt bulk collect INTO vRowList; 
 --
 -- Check to see if any data exists  
 IF (vRowList.COUNT > 0) 
  THEN
  --
  FOR j IN vRowList.FIRST..vRowList.LAST
   LOOP
   --
   vRow := vRowList(j);
   cnt := cnt + 1;
   --
   -- Update single row
   sql_stmt:= 'UPDATE /*+ PARALLEL 4 */ '|| vface_tbl ||' a set a.sdogeometry = a.topogeom.get_geometry() where a.rowid = :1';
   EXECUTE IMMEDIATE sql_stmt USING vRow;
   --
   IF (MOD(cnt,500) = 0) 
    THEN -- Commits every 500 records
     COMMIT;
   END IF;
   --
  END LOOP;
  --
  COMMIT;
  --
 END IF;
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,vface_tbl,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
 --
 cnt := 0; 
 -- tracking
 v_start_time := systimestamp;
 v_step := 'Step 4: Add Spatial Indexes';
 --
 -- insert entry into user_sdo_geom_metadata and create spatial index, if not previously completed
 sql_stmt := 'select count(*) from user_sdo_geom_metadata where table_name = :1 and column_name = ''SDOGEOMETRY''';
 EXECUTE immediate sql_stmt INTO cnt USING vface_tbl;
 IF cnt = 0 THEN
  sql_stmt := 'insert into user_sdo_geom_metadata values (:1,''SDOGEOMETRY'',SDO_DIM_ARRAY (SDO_DIM_ELEMENT (''X'',-180,180,0.05),SDO_DIM_ELEMENT(''Y'',-90,90,0.05)), 8265)';
  EXECUTE immediate sql_stmt USING vface_tbl;
  COMMIT;
 END IF;
 --
 sql_stmt := 'create index ' || vface_tbl || '_SIDX on ' || vface_tbl || ' (SDOGEOMETRY) indextype is mdsys.spatial_index';
 EXECUTE immediate sql_stmt;
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,vface_tbl,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
 --
 -- add columns to face
 sql_stmt := 'alter table ' || vface_tbl || ' add (areatotal number, perimeter number, llx number, lly number, urx number, ury number, pa_ratio number, mbr mdsys.sdo_geometry,qc varchar2(100))';
 EXECUTE immediate sql_stmt;
 -- tracking
 v_start_time := systimestamp;
 v_step := 'Step 5: Calculate Measurements';
 --
 -- add data to area total,perimeter,llx,lly,pa_ratio,mbr,urx,ury
 -- define perimeter and perimeter/area ratio; define first an MBR and then from it the Lower Left and Upper Right of the geometry
 sql_stmt := 'UPDATE /*+ PARALLEL 4 */  ' || vface_tbl || ' set MBR = SDO_GEOM.SDO_MBR(SDOGEOMETRY)';
 EXECUTE IMMEDIATE sql_stmt;
 COMMIT;
 sql_stmt := 'UPDATE /*+ PARALLEL 4 */  ' || vface_tbl || ' set areatotal = sdo_geom.sdo_area(sdogeometry,0.05),PERIMETER = SDO_GEOM.SDO_LENGTH(SDOGEOMETRY,0.05,''unit=meter''),LLX = SDO_GEOM.SDO_MIN_MBR_ORDINATE(MBR,1),LLY = SDO_GEOM.SDO_MIN_MBR_ORDINATE(MBR,2),URX = SDO_GEOM.SDO_MAX_MBR_ORDINATE(MBR,1),URY = SDO_GEOM.SDO_MAX_MBR_ORDINATE(MBR,2)';
 EXECUTE IMMEDIATE sql_stmt;
 COMMIT;
 sql_stmt := 'UPDATE /*+ PARALLEL 4 */  ' || vface_tbl || ' set PA_RATIO = PERIMETER/SQRT(AREATOTAL)';
 EXECUTE IMMEDIATE sql_stmt;
 COMMIT;
 -- tracking
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,vface_tbl,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
 --
 -- remove universal face
 sql_stmt := 'delete from ' || vface_tbl || ' where face_id = -1';
 EXECUTE immediate sql_stmt;
 Commit;
 --
 -- end tracking for procedure
 --
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,vface_tbl,v_proc_step,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,release,'N/A',deploy);
 --
 -- create index on basic fields - statefp,countyfp,cousubfp,placefp
 -- NOTE: The code to create the following index needs to be made generic and therefore it is commented out
 -- sql_stmt := 'create index face_idx on ' || vface_tbl || ' (statefp,countyfp) nologging';
 -- EXECUTE immediate sql_stmt;
END load_face;
--
PROCEDURE load_edge (topology VARCHAR2,edge_tbl VARCHAR2,edge_dollar_tbl VARCHAR2,release VARCHAR2,deploy VARCHAR2)
AS
/**
 #############################################################################################################
 # Program Name: load_edge
 # Author: mz
 # Creation Date: 07/16/2009
 # Modification History:
 #
 # Purpose: Inserts data into the EDGE table from topology_EDGE$
 #
 # Required parameters:
 #  - topology - Name of topology
 #  - deploy - Deployment schema
 #  - edge_tbl - Name of edge table
 #  - edge_dollar_tbl - Name of EDGE$ table
 #
 # Dependencies: None
 #
 #############################################################################################################
*/
sql_stmt   VARCHAR2(4000); -- Dynamic SQL Statement
v_tbl_keys VARCHAR2(250);
BEGIN
 -- obtain info from topo_universe table
 sql_stmt := 'select tbl_keys from topo_universe where topology = :1 and table_name = :2 and deploy = :3';
 EXECUTE immediate sql_stmt INTO v_tbl_keys USING topology,edge_tbl,deploy;
 --
 -- load edges
 sql_stmt := 'insert into ' || edge_tbl || ' (' || v_tbl_keys || ') select ' || v_tbl_keys || ' from ' || edge_dollar_tbl;
 EXECUTE immediate sql_stmt;
 COMMIT;
 --
 -- create index
 sql_stmt := 'create unique index ' || edge_tbl || '_' || v_tbl_keys || '_uk on ' || edge_tbl || '(' || v_tbl_keys || ') nologging';
 EXECUTE immediate sql_stmt;
 --
 -- load edgelen; field 'edgelen' is hardcoded assuming no changes to the edge table description
 sql_stmt := 'update /*+ parallel 4 */ ' || edge_tbl || ' a set a.edgelen = (select sdo_geom.sdo_length(b.geometry,0.05) from ' || edge_dollar_tbl || ' b where a.' || v_tbl_keys || ' = b.edge_id)';
 EXECUTE immediate sql_stmt;
 COMMIT;
END load_edge;
--
PROCEDURE gen_tracking_log (topology IN VARCHAR2, table_name IN VARCHAR2 DEFAULT NULL, process IN VARCHAR2 DEFAULT NULL, step IN VARCHAR2 DEFAULT NULL, start_time IN TIMESTAMP DEFAULT NULL, end_time IN TIMESTAMP DEFAULT NULL, elapsed_time IN INTERVAL DAY TO SECOND DEFAULT NULL, release IN VARCHAR2 DEFAULT NULL, sqlstmt IN VARCHAR2 DEFAULT NULL, deploy IN VARCHAR2 DEFAULT NULL) 
AS
/**
 #############################################################################################################
 # Program Name: gen_tracking_log
 # Author: Salman Mansoor
 # Creation Date: 03/07/2012
 # Modification History:
 #
 # Purpose: Insert logs into gen_tracking table 
 #
 # Dependencies: <topo>_gen_tracking
 #
 #############################################################################################################
*/
tbl_check_cnt NUMBER;
sql_stmt  VARCHAR2(4000); 
--
BEGIN
 --
 -- GEN_TRACKING - Tracking table
 sql_stmt := 'select count(*) from all_tables where table_name = :1 and owner = :2';
 EXECUTE immediate sql_stmt INTO tbl_check_cnt USING topology||'_GEN_TRACKING', deploy;
 --
 -- gen_tracking check 
 --
 IF tbl_check_cnt > 0 
  THEN
   --
   sql_stmt := 'INSERT /*+ APPEND */ INTO '||topology||'_GEN_TRACKING VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
   EXECUTE IMMEDIATE sql_stmt USING table_name,process,step,start_time,end_time,elapsed_time,release,sqlstmt,deploy;
   --
   COMMIT;
   --
 ELSE
   --
   sql_stmt := 'CREATE TABLE '||topology||'_GEN_TRACKING (TABLE_NAME VARCHAR2(30),PROCESS VARCHAR2(100),STEP VARCHAR2(4000),START_TIME TIMESTAMP (6) WITH TIME ZONE,END_TIME TIMESTAMP (6) WITH TIME ZONE, ELAPSED_TIME INTERVAL DAY (5) TO SECOND (6),RELEASE VARCHAR2(100),SQLSTMT VARCHAR2(4000),DEPLOY VARCHAR2(32)) nologging';
   EXECUTE immediate sql_stmt;
   --
   sql_stmt := 'INSERT /*+ APPEND */ INTO '||topology||'_GEN_TRACKING VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
   EXECUTE IMMEDIATE sql_stmt USING table_name,process,step,start_time,end_time,elapsed_time,release,sqlstmt,deploy;
   --
   COMMIT;
   --
 END IF;
 --
END gen_tracking_log;
--
PROCEDURE create_lookup_output_tables (topology VARCHAR2, release VARCHAR2, deploy VARCHAR2)
AS
/**
 #############################################################################################################
 # Program Name: create_lookup_output_tables
 # Author: mz / Salman Mansoor
 # Creation Date: 08/13/2009
 # Modification History:06/15/2011
 #
 # Purpose: Creates lookup_tables for Generalization releases. This procedure assumes that these
 #            tables do not exist. If they already exist in another release/deploy then use
 #            copy_lookup_output_tables procedure instead since it easier to modify data from
 #            an existing release/deploy than it is to start from the beginning
 #
 # Dependencies: None
 #
 #############################################################################################################
*/
tbl_check_cnt NUMBER;
sql_stmt VARCHAR2(4000); -- SQL Statement
BEGIN
 --
 -- GEN_TRACKING - Tracking table
 sql_stmt := 'select count(*) from all_tables where table_name = :1 and owner = :2';
 EXECUTE immediate sql_stmt INTO tbl_check_cnt USING topology||'_GEN_TRACKING', deploy;
 --
 -- create gen_tracking 
 --
 IF tbl_check_cnt > 0 
  THEN
   sql_stmt := 'TRUNCATE TABLE '||topology||'_GEN_TRACKING';
   EXECUTE immediate sql_stmt;
 ELSE
   sql_stmt := 'CREATE TABLE '||topology||'_GEN_TRACKING (TABLE_NAME VARCHAR2(30),PROCESS VARCHAR2(100),STEP VARCHAR2(4000),START_TIME TIMESTAMP (6) WITH TIME ZONE,END_TIME TIMESTAMP (6) WITH TIME ZONE, ELAPSED_TIME INTERVAL DAY (5) TO SECOND (6),RELEASE VARCHAR2(100),SQLSTMT VARCHAR2(4000),DEPLOY VARCHAR2(32)) nologging';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on table '||topology||'_gen_tracking is ''Event Tracking Table''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column '||topology||'_gen_tracking.table_name is ''Name of Table''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column '||topology||'_gen_tracking.process is ''Name of process, e.g., create topology''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column '||topology||'_gen_tracking.step is ''Step within a process; may not applicable for every process''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column '||topology||'_gen_tracking.start_time is ''Start time''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column '||topology||'_gen_tracking.end_time is ''End time''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column '||topology||'_gen_tracking.elapsed_time is ''Elapsed time''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column '||topology||'_gen_tracking.release is ''Specific CPB deliverable''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column '||topology||'_gen_tracking.sqlstmt is ''SQL statement being executed''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column '||topology||'_gen_tracking.deploy is ''Default Schema''';
   EXECUTE immediate sql_stmt;
 END IF;
 --
 -- GTYPE - Geometry type check - Stores table name and number of records per geometry type. For example,
 --          Table name: PEP09_SL050; Geometry Type: 2003; Number of records: 3,168
 sql_stmt := 'select count(*) from all_tables where table_name = :1 and owner = :2';
 EXECUTE immediate sql_stmt INTO tbl_check_cnt USING 'GTYPE_CHECK', deploy;
 --
 -- if table exists then truncate
 --
 IF tbl_check_cnt > 0 
  THEN
   sql_stmt := 'TRUNCATE TABLE GTYPE_CHECK';
   EXECUTE immediate sql_stmt;
 ELSE
   sql_stmt := 'CREATE TABLE GTYPE_CHECK (CHK NUMBER,TBL_NAME VARCHAR2(32),GTYPE NUMBER,CNT NUMBER) nologging';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on table gtype_check is ''Geometry Type Check''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column gtype_check.chk is ''Check number in sequence for a particular table''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column gtype_check.tbl_name is ''Name of table''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column gtype_check.gtype is ''SDOGEOMETRY type: 2001,2002,2003, etc.''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column gtype_check.cnt is ''Number per SDOGEOMETRY types''';
   EXECUTE immediate sql_stmt;
 END IF;  
 --
 -- MBR Exceptions - The MBRs from some STATEFPs exceed the "normal" MBR boundaries and therefore the boundaries are artifically extended
 sql_stmt := 'select count(*) from all_tables where table_name = :1 and owner = :2';
 EXECUTE immediate sql_stmt INTO tbl_check_cnt USING 'MBR_EXCEPTIONS', deploy;
 --
 -- if table does not exists then create
 --
 IF tbl_check_cnt = 0 
  THEN
   sql_stmt := 'CREATE TABLE MBR_EXCEPTIONS (RELEASE VARCHAR2(10),STATEFP VARCHAR2(2),ORD1 NUMBER,ORD2 NUMBER,ORD3 NUMBER,ORD4 NUMBER) nologging';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on table mbr_exceptions is ''MBR Exceptions: Occasionally the MBRs for a particular state failed topology loading with an "out of bounds" error and the MBR had to be extended. This table contains a list of states and their substitution MBRs''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column mbr_exceptions.release is ''Name of CPB release''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column mbr_exceptions.statefp is ''STATEFP code''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column mbr_exceptions.ord1 is ''Coordinate 1''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column mbr_exceptions.ord2 is ''Coordinate 2''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column mbr_exceptions.ord3 is ''Coordinate 3''';
   EXECUTE immediate sql_stmt;
   sql_stmt := 'comment on column mbr_exceptions.ord4 is ''Coordinate 4''';
   EXECUTE immediate sql_stmt;
   --
   -- Insert data
   --
   sql_stmt := 'Insert into MBR_EXCEPTIONS (RELEASE,STATEFP,ORD1,ORD2,ORD3,ORD4) Values (:1,''04'',-115.299697,30.859011,-108.219077,37.594643)';
   EXECUTE immediate sql_stmt USING release;
   sql_stmt := 'Insert into MBR_EXCEPTIONS (RELEASE,STATEFP,ORD1,ORD2,ORD3,ORD4) Values (:1,''08'',-109.076811,36.982424,-102.030878,41.013444)';
   EXECUTE immediate sql_stmt USING release;
   sql_stmt := 'Insert into MBR_EXCEPTIONS (RELEASE,STATEFP,ORD1,ORD2,ORD3,ORD4) Values (:1,''20'',-103.795819,35.718441,-92.345107,42.217976)';
   EXECUTE immediate sql_stmt USING release;
   sql_stmt := 'Insert into MBR_EXCEPTIONS (RELEASE,STATEFP,ORD1,ORD2,ORD3,ORD4) Values (:1,''24'',-79.49765,37.876605,-74.976282,39.733037)';
   EXECUTE immediate sql_stmt USING release;
   sql_stmt := 'Insert into MBR_EXCEPTIONS (RELEASE,STATEFP,ORD1,ORD2,ORD3,ORD4) Values (:1,''39'',-85.557691,38.138699,-79.329917,42.368064)';
   EXECUTE immediate sql_stmt USING release;
   sql_stmt := 'Insert into MBR_EXCEPTIONS (RELEASE,STATEFP,ORD1,ORD2,ORD3,ORD4) Values (:1,''46'',-105.14221,41.598741,-95.407526,47.019994)';
   EXECUTE immediate sql_stmt USING release;
   sql_stmt := 'Insert into MBR_EXCEPTIONS (RELEASE,STATEFP,ORD1,ORD2,ORD3,ORD4) Values (:1,''54'',-82.654591,37.19154,-77.709662,40.648801)';
   EXECUTE immediate sql_stmt USING release;
   --
   Commit;
   --
 END IF;  
 --
END create_lookup_output_tables;
--
PROCEDURE add_sde_column_to_table (topology VARCHAR2,table_type VARCHAR2,release varchar2,deploy VARCHAR2,pNewColumnName VARCHAR2 DEFAULT 'SDESEQ')
AS
/**
 #############################################################################################################
 # Program Name: add_sde_column_to_table
 # Author: Nick A. Padfield
 # Creation Date: 2009
 # Modification History:
 #   08/13/2009 - mz - Changed code for bulk and integrated procedure into package
 #
 # Purpose: Adds SDE column to table
 #
 # Dependencies: None
 #
 #############################################################################################################
*/
-- vInTable VARCHAR2(30) := UPPER(SUBSTR(pInTable,1,30));
vNewColumnName VARCHAR2(30) := UPPER(SUBSTR(pNewColumnName,1,30));
sql_stmt VARCHAR2(4000);
TmpCount NUMBER;
ContinueStatus BOOLEAN := TRUE;
v_tbl_array MDSYS.STRING_ARRAY;
v_table_name VARCHAR2(32);
tblcnt NUMBER;
BEGIN
 -----------------------------------------------------------------------------
 -- Retrieve relevant tables
 sql_stmt := 'select table_name from topo_universe where topology = :1 and table_type = :2 and release = :3 and deploy = :4';
 EXECUTE immediate sql_stmt INTO v_tbl_array USING topology,table_type,release,deploy;
 -- loop through each table, add SDE sequence number, and populate field
 FOR i IN 1..v_tbl_array.COUNT
  LOOP
   v_table_name := v_tbl_array(i);
   -- determine whether the table has an SDESEQ field that is populated
   sql_stmt := 'SELECT count(*) FROM user_tab_columns WHERE table_name = :1 AND column_name = :2';
   EXECUTE IMMEDIATE sql_stmt INTO TmpCount USING v_table_name,
   vNewColumnName;
   IF TmpCount = 0 THEN
    -- Add the new column
    sql_stmt := 'ALTER TABLE ' || v_table_name || ' ADD (' || vNewColumnName || ' INTEGER)';
    EXECUTE IMMEDIATE sql_stmt;
    -- Populate the new column with ROWNUM values
    sql_stmt := 'UPDATE /*+ PARALLEL 4 */ ' || v_table_name || ' t SET t.' || vNewColumnName || ' = rownum';
    EXECUTE IMMEDIATE sql_stmt;
    COMMIT;
   END IF;
   IF TmpCount > 0 THEN
    -- check to see if there is data in SDE Sequence field
    sql_stmt := 'select count(*) from ' || v_table_name || ' where ' || vNewColumnName || ' is not null and rownum = 1';
    EXECUTE immediate sql_stmt INTO TmpCount;
    -- if the SDE Sequence field is empty then populate it
    IF TmpCount = 0 THEN
     sql_stmt := 'UPDATE /*+ PARALLEL 4 */ ' || v_table_name || ' t SET t.' || vNewColumnName || ' = rownum';
      EXECUTE IMMEDIATE sql_stmt;
      COMMIT;
    END IF;
   END IF;
  END LOOP;
END add_sde_column_to_table;
--
PROCEDURE copy_lookup_output_tables (source_schema VARCHAR2,target_schema VARCHAR2)
AS
/**
 #############################################################################################################
 # Program Name: copy_lookup_output_tables
 # Author: mz
 # Creation Date: 08/19/2009
 # Modification History: None
 #
 # Purpose: Copy lookup and output tables from source schema to target schema. Data in source lookup tables
 #            is retained for modification (easier than re-entering the data).  Data in source output tables
 #            is eliminated during create statement (initially the tables should be blank)
 #
 # Dependencies:
 #   - Tables in Source Schema must be set to PUBLIC SELECT
 #   - For potential priv issues this procedure should be run from the target schema
 #
 #############################################################################################################
*/
sql_stmt VARCHAR2(4000);
BEGIN
 -- gen_tracking
 sql_stmt := 'create table ' || target_schema || '.gen_tracking nologging as select * from ' || source_schema || '.gen_tracking where rownum < 1';
 EXECUTE immediate sql_stmt;
 --
 --- gtype_check
 sql_stmt := 'create table ' || target_schema || '.gtype_check nologging as select * from ' || source_schema || '.gtype_check where rownum < 1';
 EXECUTE immediate sql_stmt;
 --
 --- mbr exceptions
 sql_stmt := 'create table ' || target_schema || '.mbr_exceptions nologging as select * from ' || source_schema || '.mbr_exceptions';
 EXECUTE immediate sql_stmt;
 --
 -- topo_field_def
 sql_stmt := 'create table ' || target_schema || '.topo_field_def nologging as select * from ' || source_schema || '.topo_field_def';
 EXECUTE immediate sql_stmt;
 --
 -- topo_hiearchy
 sql_stmt := 'create table ' || target_schema || '.topo_hierarchy nologging as select * from ' || source_schema || '.topo_hierarchy';
 EXECUTE immediate sql_stmt;
 --
 -- topo_universe
 sql_stmt := 'create table ' || target_schema || '.topo_universe nologging as select * from ' || source_schema || '.topo_universe';
 EXECUTE immediate sql_stmt;
END copy_lookup_output_tables;
--
PROCEDURE execution_script (topology VARCHAR2,topo_universe VARCHAR2,topo_hierarchy VARCHAR2,statefp VARCHAR2 DEFAULT '99',state_tbl VARCHAR2 DEFAULT 'SL040',feature_mbr_only VARCHAR2 DEFAULT 'FALSE',validate VARCHAR2 DEFAULT 'Y',build_option NUMBER DEFAULT 2,cleanup VARCHAR2 DEFAULT 'N',release VARCHAR2,deploy VARCHAR2)
AS
/**
 #############################################################################################################
 # Program Name: execution_script
 # Author: Salman Mansoor    
 # Creation Date: 06/14/2011
 # Modification History:
 #
 # Purpose: Execution script for a particular release.
 #
 # Required Parameters:
 #  - topology - Name of topology
 #  - topo_universe - Name of topo_universe table
 #  - topo_hierarchy - Name of topo_hierarchy table
 #  - statefp - Statefp code (99 National)
 #  - state_tbl - Name of state table (SL040)
 #  - feature_mbr_only - State based mbr or feature by feature mbr (True or False)
 #  - validate - Validate topology flag ('Y' or 'N')
 #  - build_option - Create_feature only or add_polygon/create_feature combination (1 or 2)
 #  - cleanup - Drop topology for a re-run ('Y' or 'N')
 #  - release - Release name
 #  - deploy - Deployment schema
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #  - TOPO_HIERARCHY
 #
 #############################################################################################################
*/
-- Tracking Variables 
v_process VARCHAR2(100) := 'Execution Script'; -- Process
v_step VARCHAR2(4000); -- Step
v_start_time TIMESTAMP; -- Start time
v_end_time TIMESTAMP; -- End time
v_elapsed_time interval DAY(5) TO second (2); -- Elapsed time
-- Local Variables 
vtopology VARCHAR2(32) := topology;                         -- Topology Name
vtopo_universe VARCHAR2(32) := topo_universe;           -- Topo Universe Table
vtopo_hierarchy VARCHAR2(32) := topo_hierarchy;       -- Topo Hierarchy Table
vstatefp VARCHAR2(2) := statefp;                              -- Statefp default to '99' (National)
vstate_tbl VARCHAR2(32) := state_tbl;                            -- State Table Name default 'SL040'
vfeature_mbr_only VARCHAR2(5) := feature_mbr_only;    -- Run load by feature mbr, not by state mbr
vbuild_option Number := build_option;                       -- Create_feature only or add_polygon/create_feature combination
vface_tbl VARCHAR2(32);                                         -- Face feature table name
array_table_name mdsys.string_array;                      -- table name array
vtable_name VARCHAR2(32);                                   -- table name
--
sql_stmt VARCHAR2(4000);                                -- Dynamic SQL Statement
topo_cnt NUMBER;                                            -- Topology Counter 
table_cnt NUMBER;                                           -- Table Counter 
tbl_cnt2 NUMBER;                                            -- Table Counter 2
face_cnt NUMBER;                                           -- Face$ Counter 
edge_cnt NUMBER;                                          -- Edge$ Counter 
ContinueStatus BOOLEAN := FALSE;                 -- Flag
v_topo_exists NUMBER;                                   -- Variable to hold topology existence info
--
BEGIN
 --
 dbms_output.put_line ('Execution Script: Begin Build Option '||vbuild_option);
 dbms_output.put_line ('---');
 --
 -- check for state mbr table in current schema 
 --
 sql_stmt := 'select count(*) from user_tables where table_name = :1';
 EXECUTE immediate sql_stmt INTO table_cnt USING vstate_tbl;
 --
 -- check for state mbr table in topo_universe table
 --
 sql_stmt := 'select count(*) from '||vtopo_universe||' a where a.table_name = :1 and a.topology = :2 and a.table_type = :3';
 EXECUTE immediate sql_stmt INTO tbl_cnt2 USING vstate_tbl,vtopology,'S';
 
 IF (table_cnt = 1) 
  THEN
   ContinueStatus := TRUE;
 ELSIF (table_cnt < 1 AND tbl_cnt2 = 1) 
  THEN
   ContinueStatus := TRUE;
 ELSIF (table_cnt < 1 AND tbl_cnt2 < 1) 
  THEN
   ContinueStatus := FALSE;
   dbms_output.put_line('State MBR Table does not exist in current schema or in topo_universe table. Aborting now...');    
 END IF;
--
IF ContinueStatus THEN
 --
 -- If topology already exists, drop topology
 --
 IF cleanup = 'Y'
  THEN
  --
  -- Remove old Topology if it exists and drop feature tables
  --
  sql_stmt := 'select count(*) from all_sdo_topo_info where topology = :1 and owner = :2';
  EXECUTE immediate sql_stmt INTO v_topo_exists USING vtopology, deploy;
  -- Drop if old Topology exists
  IF v_topo_exists > 0 THEN
   -- call drop_topology procedure
   gz_topo_build.drop_topology (vtopology,deploy,release,deploy,'Y');
   --
  END IF;
  --
 END IF;
 --
 -- get count for a particular topology
 --
 sql_stmt := 'select count(*) from user_sdo_topo_info where topology = :1';
 EXECUTE immediate sql_stmt INTO topo_cnt USING vtopology;
 --
 -- If topology does not exist, create new 
 --
 IF topo_cnt = 0
  THEN
   --
   GZ_TOPO_BUILD.create_lookup_output_tables (vtopology,release,deploy);    
   --
   GZ_TOPO_BUILD.create_topo_tables (vtopology,vtopo_universe,release,deploy);
   --
   GZ_TOPO_BUILD.update_data_length (vtopology,vtopo_universe,release,deploy);
   --
   GZ_TOPO_BUILD.create_topology (vtopology,vtopo_hierarchy,release,deploy,0.05,8265);
   --
 END IF;
 --
 -- Grant privileges 
 --
 GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',vtopology||'%');
 --
 -- Check if national run or state based 
 --
 IF (vstatefp = '99')
  THEN
   --   
   -- State MBR or Feature MBR 
   --  
   IF (vfeature_mbr_only = 'FALSE')
    THEN
     --  
     IF (vbuild_option = 1) 
      THEN
       -- tracking
       v_start_time := systimestamp;
       --
       -- Add Polygon
       GZ_TOPO_BUILD.add_poly_sl_0_statefp (vtopology,'F',vtopo_universe,vtopo_hierarchy,state_tbl,release,deploy);
       --
       GZ_TOPO_BUILD.add_poly_sl_0 (vtopology,vtopo_universe,vtopo_hierarchy,release,deploy);
       --
       -- tracking 
       v_end_time := systimestamp;
       v_elapsed_time := v_end_time - v_start_time;
       --
       v_step := 'Count of FACE$';
       sql_stmt := 'select count(*) from '||vtopology||'_FACE$';
       EXECUTE immediate sql_stmt INTO face_cnt;   
       GZ_TOPO_BUILD.gen_tracking_log(vtopology,'FACE$',v_process,v_step||' : '||face_cnt,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
       --
       v_step := 'Count of EDGE$';
       sql_stmt := 'select count(*) from '||vtopology||'_EDGE$';
       EXECUTE immediate sql_stmt INTO edge_cnt;
       GZ_TOPO_BUILD.gen_tracking_log(vtopology,'EDGE$',v_process,v_step||' : '||edge_cnt,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
       --
       -- Copy of Edge$ Table 
       table_cnt := 0;
       sql_stmt := 'select count(*) from user_tables where table_name = :1';
       EXECUTE immediate sql_stmt INTO table_cnt USING vtopology||'_EDGE_ADDPOLY';
       
       IF table_cnt > 0
        THEN
          sql_stmt := 'drop table '||vtopology||'_edge_addpoly purge';
          EXECUTE immediate sql_stmt;
          Commit;
       END IF;
       
       sql_stmt := 'Create table '||vtopology||'_edge_addpoly as select * from '||vtopology||'_edge$';
       EXECUTE immediate sql_stmt;
       --
       -- insert entry into user_sdo_geom_metadata and create spatial index, if not previously created 
       table_cnt := 0;
       sql_stmt := 'select count(*) from user_sdo_geom_metadata where table_name = :1 and column_name = ''GEOMETRY''';
       EXECUTE immediate sql_stmt INTO table_cnt USING vtopology||'_EDGE_ADDPOLY';
       IF table_cnt = 0 
        THEN
          sql_stmt := 'insert into user_sdo_geom_metadata values (:1,''GEOMETRY'',SDO_DIM_ARRAY (SDO_DIM_ELEMENT (''X'',-180,180,0.05),SDO_DIM_ELEMENT(''Y'',-90,90,0.05)), 8265)';
          EXECUTE immediate sql_stmt USING vtopology||'_EDGE_ADDPOLY';
          COMMIT;
       END IF;
       --
       sql_stmt := 'create index '||vtopology||'_EDGE_ADDPOLY_SIDX on '||vtopology||'_EDGE_ADDPOLY (GEOMETRY) indextype is mdsys.spatial_index';
       EXECUTE immediate sql_stmt;
       --
       sql_stmt := 'create index '||vtopology||'_EID on '||vtopology||'_EDGE_ADDPOLY (EDGE_ID) nologging';
       EXECUTE immediate sql_stmt;
       --
       sql_stmt := 'Grant Select on '||vtopology||'_EDGE_ADDPOLY to PUBLIC';
       EXECUTE immediate sql_stmt;
       --
       -- Create Feature 
       GZ_TOPO_BUILD.load_topo_sl_0_statefp (vtopology,'F',vtopo_universe,vtopo_hierarchy,state_tbl,release,deploy);
       --
       GZ_TOPO_BUILD.load_topo_sl_0 (vtopology,vtopo_universe,vtopo_hierarchy,release,deploy);
       --
       GZ_TOPO_BUILD.load_topo_sl_1_up (vtopology,'SL',vtopo_universe,vtopo_hierarchy,release,deploy);
       --
     ELSIF (vbuild_option = 2)
      THEN
       --
       GZ_TOPO_BUILD.load_topo_sl_0_statefp (vtopology,'F',vtopo_universe,vtopo_hierarchy,state_tbl,release,deploy);
       --
       GZ_TOPO_BUILD.load_topo_sl_0 (vtopology,vtopo_universe,vtopo_hierarchy,release,deploy);
       --
       GZ_TOPO_BUILD.load_topo_sl_1_up (vtopology,'SL',vtopo_universe,vtopo_hierarchy,release,deploy);
       --
     END IF;
     -- 
   ELSIF (vfeature_mbr_only = 'TRUE')
    THEN
     --  
     IF (vbuild_option = 1) 
      THEN
       -- tracking
       v_start_time := systimestamp;
       --
       -- Add Polygon 
       GZ_TOPO_BUILD.add_poly_sl_0 (vtopology,vtopo_universe,vtopo_hierarchy,release,deploy,vfeature_mbr_only);
       --
       -- tracking 
       v_end_time := systimestamp;
       v_elapsed_time := v_end_time - v_start_time;
       --
       v_step := 'Count of FACE$';
       sql_stmt := 'select count(*) from '||vtopology||'_FACE$';
       EXECUTE immediate sql_stmt INTO face_cnt;   
       GZ_TOPO_BUILD.gen_tracking_log(vtopology,'FACE$',v_process,v_step||' : '||face_cnt,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
       --
       v_step := 'Count of EDGE$';
       sql_stmt := 'select count(*) from '||vtopology||'_EDGE$';
       EXECUTE immediate sql_stmt INTO edge_cnt;
       GZ_TOPO_BUILD.gen_tracking_log(vtopology,'EDGE$',v_process,v_step||' : '||edge_cnt,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt,deploy);
       --
       -- Copy of Edge$ Table 
       table_cnt := 0;
       sql_stmt := 'select count(*) from user_tables where table_name = :1';
       EXECUTE immediate sql_stmt INTO table_cnt USING vtopology||'_EDGE_ADDPOLY';
       
       IF table_cnt > 0
        THEN
          sql_stmt := 'drop table '||vtopology||'_edge_addpoly purge';
          EXECUTE immediate sql_stmt;
          Commit;
       END IF;
       
       sql_stmt := 'Create table '||vtopology||'_edge_addpoly as select * from '||vtopology||'_edge$';
       EXECUTE immediate sql_stmt;
       --
       -- insert entry into user_sdo_geom_metadata and create spatial index, if not previously created 
       table_cnt := 0;
       sql_stmt := 'select count(*) from user_sdo_geom_metadata where table_name = :1 and column_name = ''GEOMETRY''';
       EXECUTE immediate sql_stmt INTO table_cnt USING vtopology||'_EDGE_ADDPOLY';
       IF table_cnt = 0 
        THEN
          sql_stmt := 'insert into user_sdo_geom_metadata values (:1,''GEOMETRY'',SDO_DIM_ARRAY (SDO_DIM_ELEMENT (''X'',-180,180,0.05),SDO_DIM_ELEMENT(''Y'',-90,90,0.05)), 8265)';
          EXECUTE immediate sql_stmt USING vtopology||'_EDGE_ADDPOLY';
          COMMIT;
       END IF;
       --
       sql_stmt := 'create index '||vtopology||'_EDGE_ADDPOLY_SIDX on '||vtopology||'_EDGE_ADDPOLY (GEOMETRY) indextype is mdsys.spatial_index';
       EXECUTE immediate sql_stmt;
       --
       sql_stmt := 'create index '||vtopology||'_EID on '||vtopology||'_EDGE_ADDPOLY (EDGE_ID) nologging';
       EXECUTE immediate sql_stmt;
       --
       sql_stmt := 'Grant Select on '||vtopology||'_EDGE_ADDPOLY to PUBLIC';
       EXECUTE immediate sql_stmt;
       --
       -- Create Feature 
       GZ_TOPO_BUILD.load_topo_sl_0 (vtopology,vtopo_universe,vtopo_hierarchy,release,deploy,vfeature_mbr_only);
       --
       GZ_TOPO_BUILD.load_topo_sl_1_up (vtopology,'SL',vtopo_universe,vtopo_hierarchy,release,deploy);
       --
     ELSIF (vbuild_option = 2)
      THEN
       --
       GZ_TOPO_BUILD.load_topo_sl_0 (vtopology,vtopo_universe,vtopo_hierarchy,release,deploy,vfeature_mbr_only);
       --
       GZ_TOPO_BUILD.load_topo_sl_1_up (vtopology,'SL',vtopo_universe,vtopo_hierarchy,release,deploy);
       --
     END IF;
     --
   END IF;
   --
   -- Loading Face feature table
   --
   sql_stmt := 'select table_name from '||vtopo_universe||' where table_name like :1 and topology = :2 and release = :3 and deploy = :4';
   EXECUTE immediate sql_stmt INTO vface_tbl USING '%FACE',vtopology,release,deploy;
   --
   GZ_TOPO_BUILD.load_face (vtopology,vface_tbl,vtopology||'_'||vface_tbl||'$',vtopo_universe,release,deploy);
   --
   -- Create source tables for FSL build
   --
   GZ_TOPO_HELPER.create_scr_tables (vtopology,vface_tbl,vtopo_universe,release,deploy);
   --
   -- Gather stats on entire topology
   --
   GZ_TOPO_UTIL.GATHER_TOPO_STATS(vtopology);
   --
   -- Grant privileges 
   --
   GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',vtopology||'%');
   --
   -- Validating the Topology 
   -- 
   IF validate = 'Y'
    THEN
     GZ_TOPO_BUILD.valtopo (vtopology,state_tbl,release,deploy);
   END IF;
   --
 ELSE
   --
   IF (vbuild_option = 1) 
    THEN
     --
     GZ_TOPO_BUILD.add_poly_single_state (vtopology,'F',vstatefp,vtopo_universe,vtopo_hierarchy,state_tbl,release,deploy);
     --
     GZ_TOPO_BUILD.load_topo_single_state (vtopology,'F',vstatefp,vtopo_universe,vtopo_hierarchy,state_tbl,release,deploy);
     --
   ELSIF (vbuild_option = 2)
    THEN
     --
     GZ_TOPO_BUILD.load_topo_single_state (vtopology,'F',vstatefp,vtopo_universe,vtopo_hierarchy,state_tbl,release,deploy);
     --
   END IF;
   --
 END IF; -- statefp 99 or state based
-- 
END IF; -- ContinueStatus Flag
--
 dbms_output.put_line ('---');
 dbms_output.put_line ('Execution Script: End Build Option '||vbuild_option);
--
-- generic exception handling
--
EXCEPTION
 WHEN OTHERS THEN
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  --
  -- RETURN;
  RAISE;
  --  
END execution_script;
--
PROCEDURE execution_script_mz
AS
/**
 #############################################################################################################
 # Program Name: execution_script
 # Author: mz
 # Creation Date: 08/18/2009
 # Modification History:
 #
 # Purpose: Creates the execution script for a particular release. Having the execution script
 #            as part of the PL/SQL package insures that it won't be misplaced.
 #
 # Required Parameters: None
 #
 # Dependencies: None
 #
 #############################################################################################################
*/
BEGIN
 dbms_output.put_line ('Execution Script:');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Create lookup and output tables');
 dbms_output.put_line ('-- exec pep09mh.TOPO_BUILD_2.create_lookup_output_tables');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Copy lookup and output tables from source schema to target schema');
 dbms_output.put_line ('exec copy_lookup_output_tables (''PEP09MH'',''PEP09MH'')');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Create the source tables; create the empty SL and FSL tables according to specification');
 dbms_output.put_line ('exec pep09mh.TOPO_BUILD_2.create_topo_tables(''MT'',''PEP09MH'',''PEP09'')');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Reset the column data lengths to conform with whatever release is being created');
 dbms_output.put_line ('exec pep09mh.TOPO_BUILD_2.update_data_length');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Create the topology and register the feature tables - SL and FSL');
 dbms_output.put_line ('exec pep09mh.TOPO_BUILD_2.create_topology (''MT'',''PEP09MH'',''PEP09'',0.05,8265)');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Load SL feature trables that have a STATEFP column');
 dbms_output.put_line ('exec pep09mh.TOPO_BUILD_2.load_topo_sl_0_statefp (''MT'',''F'',''PEP09'',''PEP09MH'')');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Load SL feature tables that do not have a STATEFP column');
 dbms_output.put_line ('exec pep09mh.TOPO_BUILD_2.load_topo_sl_0 (''MT'',''PEP09'',''PEP09MH'')');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Load Hierarchical SL feature tables');
 dbms_output.put_line ('exec pep09mh.TOPO_BUILD_2.load_topo_sl_1_up (''MT'',''SL'',''PEP09'',''PEP09MH'')');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Load Hierarchical SL feature table SL010');
 dbms_output.put_line ('-- exec pep09mh.TOPO_BUILD_2.load_sl010 (''MT'',''PEP09MM'',''PEP09'')');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Validate Topology');
 dbms_output.put_line ('exec pep09mh.TOPO_BUILD_2.valtopo (''MT'',''PEP09'',''PEP09MH'')');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Load Face');
 dbms_output.put_line ('exec pep09mh.TOPO_BUILD_2.load_face (''MT'',''FACE'',''MT_FACE$'',''PEP09MH'')');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Load Edge');
 dbms_output.put_line ('exec pep09mh.TOPO_BUILD_2.load_edge (''MT'',''EDGE'',''MT_EDGE$'',''PEP09MH'')');
 dbms_output.put_line ('---');
 dbms_output.put_line ('---');
 dbms_output.put_line ('---');
 dbms_output.put_line ('-- Load Hierarchical FSL feature tables');
 dbms_output.put_line ('exec pep09mh.TOPO_BUILD_2_load_sl_1_up (''MT'',''FSL'')');
 dbms_output.put_line ('---');
END execution_script_mz;
--
PROCEDURE export_import_topo (topology VARCHAR2,release VARCHAR2,deploy VARCHAR2,orcl_instance VARCHAR2,target_topology VARCHAR2,target_deploy VARCHAR2,orcl_instance2 VARCHAR2)
AS
/**
 #############################################################################################################
 # Program Name: export_import_topo
 # Author: mz
 # Creation Date: 08/24/2009
 # Modification History:
 #
 # Purpose: Creates the script that exports and imports a topology from one schema to another.
 #
 # Required Parameters: topology, deploy (schema), release (CPB release name), orcl_instance (source oracle instance),
 #                      target_topology, target_deploy (schema), orcl_instance2 (target oracle instance)
 #
 # Dependencies: TOPO_UNIVERSE
 #
 # NOTE: As of 08/24/2009 this procedure does not execute the EXP and IMP commands thru any external call
 #       mechanism but simply creates the EXP and IMP command line. This code will have to be modified
 #       to run exp/imp as external processes. Therefore, this procedure cannot be executed as is since it will
 #       fail at "modify topology_EXP$ settings".  The code to initialize topology after import has been
 #       commented out.
 #
 #############################################################################################################
*/
sql_stmt VARCHAR2(4000);
v_tbl_array mdsys.string_array;
v_tbl_array2 mdsys.string_array;
v_table_name VARCHAR2(32);
v_table_name2 VARCHAR2(32);
v_other_tbls VARCHAR2(4000) := 'GEN_TRACKING,GEO_ID_SL,GTYPE_CHECK,MBR_EXCEPTIONS,TOPO_FIELD_DEF,TOPO_HIERARCHY,TOPO_UNIVERSE';
v_exp_str VARCHAR2(4000) := 'exp ';
v_imp_str VARCHAR2(4000) := 'imp ';
dt VARCHAR2(100);
BEGIN
 -- prepare topology for export
 -- a topology_EXP$ file is created which is the equivalent of the view user_sdo_topo_info
 sdo_topo.prepare_for_export (topology);
 --
 -- create exp command
 --
 -- create initial part of export string
 --
 -- get date/time and make it part of the file containing the exported data
 SELECT TO_CHAR (sysdate, 'YYYYMMDD_HH24MISS') INTO dt FROM dual;
 -- add to export string
 v_exp_str := v_exp_str || deploy || '@' || orcl_instance || ' file=' || deploy || '_' || topology || '_' || dt || '.exp tables=(';
 --
 -- obtain feature table names from TOPO_UNIVERSE
 sql_stmt := 'select table_name from topo_universe where topology = :1 and deploy = :2 and release = :3 and table_type = :4';
 EXECUTE immediate sql_stmt bulk collect INTO v_tbl_array USING topology,deploy,release,'F';
 -- build table list for export
 FOR i IN 1..v_tbl_array.LAST
  LOOP
   v_table_name := v_tbl_array(i);
   v_exp_str := v_exp_str || v_table_name || ',';
  END LOOP;
 --
 -- obtain topology names from ALL_TABLES
 sql_stmt := 'select table_name from all_tables where owner = :1 and table_name like :2';
 EXECUTE immediate sql_stmt bulk collect INTO v_tbl_array2 USING deploy,'MT_%$';
 --
 -- build exp string
 FOR i IN 1..v_tbl_array2.LAST
 LOOP
  v_table_name := v_tbl_array2(i);
  v_exp_str := v_exp_str || v_table_name || ',';
 END LOOP;
 -- add any other tables to be exported
 v_exp_str := v_exp_str || v_other_tbls || ') indexes=N statistics=NONE';
 -- output export string
 dbms_output.put_line (v_exp_str);
 --
 -- create import command
 --
 v_imp_str := v_imp_str || target_deploy || '@' || orcl_instance2 || ' file=' || deploy || '_' || topology || '_' || dt || '.exp fromuser=' || deploy || ' touser=' || target_deploy;
 -- output import string
 dbms_output.put_line (v_imp_str);
 --
 -- initalize topology after import
 --
 -- modify topology_EXP$ settings
 --sql_stmt := 'update ' || topology || '_' || 'EXP$ set owner = :1 , table_schema = :2';
 --execute immediate sql_stmt using target_deploy,target_deploy;
 --commit;
 --dbms_output.put_line (sql_stmt);
 --
 -- initalize new topology after import
 --SDO_TOPO.INITIALIZE_AFTER_IMPORT(target_topology);
 --
END export_import_topo;
--
PROCEDURE create_fsl070_source (topology varchar2, face_tbl varchar2, release varchar2, deploy varchar2)
AS
/**
 ################################################################################################################### 
 # Program Name: create_fsl070_source
 # Author: mz 
 # Creation Date: 08/26/2009
 # Recent Revisions:
 #
 # Purpose: 
 #  The purpose of this procedure is to load table FSL070 for PEP09
 #  From FACE:
 #   1) build one record for each distinct statefp || countyfp || cousubfp || placefp combination where
 #        cousubfp is not null and place is not null
 #   2) for each distinct statefp || countyfp || cousubfp combination in the above set, add a record for
 #        each distinct statefp || countyfp || cousubfp || placefp where placefp is null (the balance of
 #        county subdivision)
 # 
 # Required parameters: topology, release, deploy (schema)
 #
 # Dependencies: None
 #
 ################################################################################################################### 
*/
v_process varchar2(100) := 'Load FSL070'; -- Process
v_step varchar2(4000); -- Step
v_start_time timestamp; -- Start time
v_end_time timestamp; -- End time
v_elapsed_time interval day(5) to second (2); -- Elapsed time
v_proc_start_time timestamp; -- Procedure Start Time
v_proc_step varchar2(4000); -- Procedure Step
v_proc_end_time timestamp; -- Procedure End time
v_proc_elapsed_time interval day(5) to second (2); -- Procedure Elapsed time
--
array_tbl_name mdsys.string_array;
v_tbl_name varchar2(32);
array_source_location mdsys.string_array;
v_source_location varchar2(250);
array_tbl_keys mdsys.string_array;
v_tbl_keys varchar2(250);
array_deploy mdsys.string_array;
v_deploy varchar2(250);
array_release mdsys.string_array;
v_release varchar2(250) := null;
array_seq_num mdsys.sdo_number_array;
v_seq_num number;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp varchar2(30);
statefp_exists number;
--
sql_stmt varchar2(4000); -- Dynamic SQL Statement 
sql_stmt2 varchar2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
v_cnt number;
--
v_table_fields varchar2(4000);
v_load_table_fields varchar2(4000);
--
v_where_clause varchar2(4000);
v_where_clause_full varchar2(4000);
--
v_dml_condition varchar2(4000);
--
mbr_geom SDO_GEOMETRY; -- minimum bond rectangle
--
statefp_tbl varchar2(32);
v_statefp_tbl varchar2(32);
--
v_tbl_keys2 varchar2(1000);
--
array_key_data mdsys.string_array;
--
v_tbl_keys_data varchar2(4000);
--
comma_cnt number;
--
v_len_keys varchar2(4000);
--
array_statefp mdsys.string_array;
v_statefp varchar2(2);
array_countyfp mdsys.string_array;
v_countyfp varchar2(3);
array_cousubfp mdsys.string_array;
v_cousubfp varchar2(5);
v_keys varchar2(15);
BEGIN
 v_start_time := systimestamp;
 v_step := 'Create FSL070 source' || v_tbl_name;
 --
 -- create temp table
 --
 -- table existence check
 sql_stmt := 'select count(*) from user_tables where table_name = :1';
 execute immediate sql_stmt into v_cnt using 'FSL070_S';
 if v_cnt = 1 
  then
   -- if table exists then truncate table
   sql_stmt := 'truncate table fsl070_s';
   execute immediate sql_stmt;
 end if;
 if v_cnt = 0
  then
   -- if table doesn't exist, then create table
   sql_stmt := 'create table fsl070_s (geo_id number, statefp varchar2(2), countyfp varchar2(3), cousubfp varchar2(5), placefp varchar2(5), name varchar2(90), lsad varchar2(2), topogeom mdsys.sdo_topo_geometry, keys varchar2(15)) nologging';
   execute immediate sql_stmt;
 end if;
 --
 -- Load into temporary table one record for each distinct statefp || countyfp || cousubfp || placefp from face
 --
 -- NOTE: As of 09/09/2009 the PLACEFP field in the FACE table is populated with '00000' instead of NULL. An issue related to DML_CONDITIION
 sql_stmt := 'insert /*+ append */ into fsl070_s (keys) select distinct statefp || countyfp || cousubfp || placefp from '||face_tbl||' where cousubfp is not null and placefp <> ''00000''';
 -- original sql_stmt
 --  sql_stmt := 'insert /*+ append */ into fsl070_s (keys) select distinct statefp || countyfp || cousubfp || placefp from face where cousubfp is not null and placefp is not null';
 execute immediate sql_stmt;
 commit;
 --
 -- put statefp,countyfp,cousubfp into arrays
 sql_stmt := 'select distinct substr(keys,1,2),substr(keys,3,3),substr(keys,6,5) from fsl070_s';
 execute immediate sql_stmt bulk collect into array_statefp,array_countyfp,array_cousubfp;
 --
 -- loop through the arrays and select a record from FACE that match statefp,countyfp,cousubfp where placefp is not null
 FOR i in 1..array_statefp.LAST
  LOOP
   v_statefp := array_statefp(i);
   v_countyfp := array_countyfp(i);
   v_cousubfp := array_cousubfp(i);
   -- NOTE: As of 09/09/2009 the PLACEFP field in the FACE table is populated with '00000' instead of NULL. An issue related to DML_CONDITIION
   sql_stmt := 'insert /*+ append */ into fsl070_s (keys) select distinct statefp || countyfp || cousubfp || placefp from '||face_tbl||' where statefp = :1 and countyfp = :2 and cousubfp = :3 and placefp = ''00000''';
   -- original sql_stmt
   --  sql_stmt := 'insert /*+ append */ into fsl070_s (keys) select distinct statefp || countyfp || cousubfp || placefp from face where statefp = :1 and countyfp = :2 and cousubfp = :3 and placefp is null';
   execute immediate sql_stmt using v_statefp,v_countyfp,v_cousubfp;
   commit;
  end loop;
 --
 -- fill statefp field
 sql_stmt := 'update /*+ parallel 4 */ fsl070_s set statefp = substr(keys,1,2)';
 execute immediate sql_stmt;
 commit;
 --
 -- fill countyfp field
 sql_stmt := 'update /*+ parallel 4 */ fsl070_s set countyfp = substr(keys,3,3)';
 execute immediate sql_stmt;
 commit;
 --
 -- fill cousubfp field
 sql_stmt := 'update /*+ parallel 4 */ fsl070_s set cousubfp = substr(keys,6,5)';
 execute immediate sql_stmt;
 commit;
 -- 
 -- fill placefp field
 sql_stmt := 'update /*+ parallel 4 */ fsl070_s set placefp = substr(keys,11,5)';
 execute immediate sql_stmt; 
 commit;
 --
 -- determine whether index exists or not
 sql_stmt := 'select count(*) from user_indexes where index_name = :1';
 execute immediate sql_stmt into v_cnt using 'FSL070_S_IDX';
 if v_cnt = 1
  then
   -- drop index if it exists
   sql_stmt := 'drop index fsl070_s_idx';
   execute immediate sql_stmt;
 end if;
 if v_cnt = 0
  then
   -- create index if it doesn't exist
   sql_stmt := 'create index fsl070_s_idx on fsl070_s(statefp,countyfp,cousubfp,placefp) nologging';
   execute immediate sql_stmt;
 end if; 
 --
 -- drop column keys since it is not part of the specification
 sql_stmt := 'alter table fsl070_s drop column keys';
 execute immediate sql_stmt;
 -- count records 
 sql_stmt := 'select count(*) from fsl070_s';
 execute immediate sql_stmt into v_cnt;
 dbms_output.put_line('Count: ' || v_cnt);
 -- tracking
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'FSL070_S',v_process,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,v_release,'N/A',v_deploy);
 --
 -- generic exception handling
 --
 EXCEPTION
  WHEN OTHERS THEN
   -- error output
   dbms_output.put_line (SQLERRM);
   dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
   dbms_output.put_line (sql_stmt);
   -- tracking
   v_process := v_process || ' FAILED';
   v_step := v_step || ' FAILED';
   GZ_TOPO_BUILD.gen_tracking_log(topology,'FSL070_S',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
   -- RETURN;
   RAISE;
END create_fsl070_source;
--
PROCEDURE create_fsl080_source (topology varchar2, face_tbl varchar2, release varchar2, deploy varchar2)
AS
/**
 ################################################################################################################### 
 # Program Name: create_fsl080_source
 # Author: Salman 
 # Creation Date: 02/24/2010
 # Recent Revisions:
 #
 # Purpose: 
 #  The purpose of this procedure is to load table FSL080 for ACS09
 #  From FACE:
 #   1) build one record for each distinct statefp || countyfp || cousubfp || tractce || placefp combination where
 #        cousubfp is not null and place is not null
 #   2) for each distinct statefp || countyfp || cousubfp || tractce combination in the above set, add a record for
 #        each distinct statefp || countyfp || cousubfp || tractce || placefp where placefp is null (the balance of
 #        county subdivision)
 # 
 # Required parameters: topology, release, deploy (schema)
 #
 # Dependencies: None
 #
 ################################################################################################################### 
*/
v_process varchar2(100) := 'Load FSL080'; -- Process
v_step varchar2(4000); -- Step
v_start_time timestamp; -- Start time
v_end_time timestamp; -- End time
v_elapsed_time interval day(5) to second (2); -- Elapsed time
v_proc_start_time timestamp; -- Procedure Start Time
v_proc_step varchar2(4000); -- Procedure Step
v_proc_end_time timestamp; -- Procedure End time
v_proc_elapsed_time interval day(5) to second (2); -- Procedure Elapsed time
--
array_tbl_name mdsys.string_array;
v_tbl_name varchar2(32);
array_source_location mdsys.string_array;
v_source_location varchar2(250);
array_tbl_keys mdsys.string_array;
v_tbl_keys varchar2(250);
array_deploy mdsys.string_array;
v_deploy varchar2(250);
array_release mdsys.string_array;
v_release varchar2(250) := null;
array_seq_num mdsys.sdo_number_array;
v_seq_num number;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp varchar2(30);
statefp_exists number;
--
sql_stmt varchar2(4000); -- Dynamic SQL Statement 
sql_stmt2 varchar2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
v_cnt number;
--
v_table_fields varchar2(4000);
v_load_table_fields varchar2(4000);
--
v_where_clause varchar2(4000);
v_where_clause_full varchar2(4000);
--
v_dml_condition varchar2(4000);
--
mbr_geom SDO_GEOMETRY; -- minimum bond rectangle
--
statefp_tbl varchar2(32);
v_statefp_tbl varchar2(32);
--
v_tbl_keys2 varchar2(1000);
--
array_key_data mdsys.string_array;
--
v_tbl_keys_data varchar2(4000);
--
comma_cnt number;
--
v_len_keys varchar2(4000);
--
array_statefp mdsys.string_array;
v_statefp varchar2(2);
array_countyfp mdsys.string_array;
v_countyfp varchar2(3);
array_cousubfp mdsys.string_array;
v_cousubfp varchar2(5);
array_tractce mdsys.string_array;
v_tractce varchar2(6);
v_keys varchar2(25);
BEGIN
 v_start_time := systimestamp;
 v_step := 'Create FSL080 source' || v_tbl_name;
 --
 -- create temp table
 --
 -- table existence check
 sql_stmt := 'select count(*) from user_tables where table_name = :1';
 execute immediate sql_stmt into v_cnt using 'FSL080_S';
 if v_cnt = 1 
  then
   -- if table exists then truncate table
   sql_stmt := 'truncate table fsl080_s';
   execute immediate sql_stmt;
 end if;
 if v_cnt = 0
  then
   -- if table doesn't exist, then create table
   sql_stmt := 'create table fsl080_s (geo_id number, statefp varchar2(2), countyfp varchar2(3), cousubfp varchar2(5), tractce varchar2(6), placefp varchar2(5), name varchar2(90), lsad varchar2(2), topogeom mdsys.sdo_topo_geometry, keys varchar2(25)) nologging';
   execute immediate sql_stmt;
 end if;
 --
 -- Load into temporary table one record for each distinct statefp || countyfp || cousubfp || tractce || placefp from face
 --
 -- NOTE: As of 09/09/2009 the PLACEFP field in the FACE table is populated with '00000' instead of NULL. An issue related to DML_CONDITIION
 sql_stmt := 'insert /*+ append */ into fsl080_s (keys) select distinct statefp || countyfp || cousubfp || tractce || placefp from '||face_tbl||' where cousubfp is not null and placefp <> ''00000''';
 -- original sql_stmt
 --  sql_stmt := 'insert /*+ append */ into fsl070_s (keys) select distinct statefp || countyfp || cousubfp || placefp from face where cousubfp is not null and placefp is not null';
 execute immediate sql_stmt;
 commit;
 --
 -- put statefp,countyfp,cousubfp into arrays
 sql_stmt := 'select distinct substr(keys,1,2),substr(keys,3,3),substr(keys,6,5),substr(keys,11,6) from fsl080_s';
 execute immediate sql_stmt bulk collect into array_statefp,array_countyfp,array_cousubfp,array_tractce;
 --
 -- loop through the arrays and select a record from FACE that match statefp,countyfp,cousubfp,tractce where placefp is null
 FOR i in 1..array_statefp.LAST
  LOOP
   v_statefp := array_statefp(i);
   v_countyfp := array_countyfp(i);
   v_cousubfp := array_cousubfp(i);
   v_tractce := array_tractce(i); 
   -- NOTE: As of 09/09/2009 the PLACEFP field in the FACE table is populated with '00000' instead of NULL. An issue related to DML_CONDITIION
   sql_stmt := 'insert /*+ append */ into fsl080_s (keys) select distinct statefp || countyfp || cousubfp || tractce || placefp from '||face_tbl||' where statefp = :1 and countyfp = :2 and cousubfp = :3 and tractce = :4 and placefp = ''00000''';
   -- original sql_stmt
   --  sql_stmt := 'insert /*+ append */ into fsl070_s (keys) select distinct statefp || countyfp || cousubfp || placefp from face where statefp = :1 and countyfp = :2 and cousubfp = :3 and placefp is null';
   execute immediate sql_stmt using v_statefp,v_countyfp,v_cousubfp,v_tractce;
   commit;
  end loop;
 --
 -- fill statefp field
 sql_stmt := 'update /*+ parallel 4 */ fsl080_s set statefp = substr(keys,1,2)';
 execute immediate sql_stmt;
 commit;
 --
 -- fill countyfp field
 sql_stmt := 'update /*+ parallel 4 */ fsl080_s set countyfp = substr(keys,3,3)';
 execute immediate sql_stmt;
 commit;
 --
 -- fill cousubfp field
 sql_stmt := 'update /*+ parallel 4 */ fsl080_s set cousubfp = substr(keys,6,5)';
 execute immediate sql_stmt;
 commit;
 -- 
 -- fill tractce field
 sql_stmt := 'update /*+ parallel 4 */ fsl080_s set tractce = substr(keys,11,6)';
 execute immediate sql_stmt; 
 commit;
 -- 
 -- fill placefp field
 sql_stmt := 'update /*+ parallel 4 */ fsl080_s set placefp = substr(keys,17,5)';
 execute immediate sql_stmt; 
 commit;
 --
 -- determine whether index exists or not
 sql_stmt := 'select count(*) from user_indexes where index_name = :1';
 execute immediate sql_stmt into v_cnt using 'FSL080_S_IDX';
 if v_cnt = 1
  then
   -- drop index if it exists
   sql_stmt := 'drop index fsl080_s_idx';
   execute immediate sql_stmt;
 end if;
 if v_cnt = 0
  then
   -- create index if it doesn't exist
   sql_stmt := 'create index fsl080_s_idx on fsl080_s(statefp,countyfp,cousubfp,placefp,tractce) nologging';
   execute immediate sql_stmt;
 end if; 
 --
 -- drop column keys since it is not part of the specification
 sql_stmt := 'alter table fsl080_s drop column keys';
 execute immediate sql_stmt;
 -- count records 
 sql_stmt := 'select count(*) from fsl080_s';
 execute immediate sql_stmt into v_cnt;
 dbms_output.put_line('Count: ' || v_cnt);
 -- tracking
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'FSL080_S',v_process,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,v_release,'N/A',v_deploy);
 --
 -- generic exception handling
 --
 EXCEPTION
  WHEN OTHERS THEN
   -- error output
   dbms_output.put_line (SQLERRM);
   dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
   dbms_output.put_line (sql_stmt);
   -- tracking
   v_process := v_process || ' FAILED';
   v_step := v_step || ' FAILED';
   GZ_TOPO_BUILD.gen_tracking_log(topology,'FSL080_S',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
   -- RETURN;
   RAISE;
END create_fsl080_source;
--
PROCEDURE create_fsl157_source (topology varchar2, face_tbl varchar2, release varchar2, deploy varchar2)
AS
/**
 ################################################################################################################### 
 # Program Name: create_fsl157_source
 # Author: mz 
 # Creation Date: 08/26/2009
 # Recent Revisions:
 #
 # Purpose: 
 #  The purpose of this procedure is to load table FSl157 for PEP09
 #  From FACE:
 #   1) build one record for each distinct statefp||countyfp||placefp combination where placefp is NOT NULL
 #   2) build one record for each statefp||countyfp in the above set where placefp is NULL (the balance of the county)
 # 
 # Required parameters: topology, release, deploy (schema)
 #
 # Dependencies: 
 #
 ################################################################################################################### 
*/
v_process varchar2(100) := 'Load FSL157'; -- Process
v_step varchar2(4000); -- Step
v_start_time timestamp; -- Start time
v_end_time timestamp; -- End time
v_elapsed_time interval day(5) to second (2); -- Elapsed time
v_proc_start_time timestamp; -- Procedure Start Time
v_proc_step varchar2(4000); -- Procedure Step
v_proc_end_time timestamp; -- Procedure End time
v_proc_elapsed_time interval day(5) to second (2); -- Procedure Elapsed time
--
array_tbl_name mdsys.string_array;
v_tbl_name varchar2(32);
array_source_location mdsys.string_array;
v_source_location varchar2(250);
array_tbl_keys mdsys.string_array;
v_tbl_keys varchar2(250);
array_deploy mdsys.string_array;
v_deploy varchar2(250);
array_release mdsys.string_array;
v_release varchar2(250) := null;
array_seq_num mdsys.sdo_number_array;
v_seq_num number;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp varchar2(30);
statefp_exists number;
--
sql_stmt varchar2(4000); -- Dynamic SQL Statement 
sql_stmt2 varchar2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
v_cnt number;
--
v_table_fields varchar2(4000);
v_load_table_fields varchar2(4000);
--
v_where_clause varchar2(4000);
v_where_clause_full varchar2(4000);
--
v_dml_condition varchar2(4000);
--
mbr_geom SDO_GEOMETRY; -- minimum bond rectangle
--
statefp_tbl varchar2(32);
v_statefp_tbl varchar2(32);
--
v_tbl_keys2 varchar2(1000);
--
array_key_data mdsys.string_array;
--
v_tbl_keys_data varchar2(4000);
--
comma_cnt number;
--
v_len_keys varchar2(4000);
--
array_statefp mdsys.string_array;
v_statefp varchar2(2);
array_countyfp mdsys.string_array;
v_countyfp varchar2(3);
v_keys varchar2(10);
BEGIN
 v_start_time := systimestamp;
 v_step := 'Create FSL157 source' || v_tbl_name;
 --
 -- create temp table
 --
 sql_stmt := 'select count(*) from user_tables where table_name = :1';
 execute immediate sql_stmt into v_cnt using 'FSL157_S';
 if v_cnt = 1 
  then
   -- truncate exiting FSL157_S table
   sql_stmt := 'truncate table fsl157_s';
   execute immediate sql_stmt;
 end if;
 if v_cnt = 0
  then
   -- create FSL157_S table
   sql_stmt := 'create table fsl157_s (geo_id number, statefp varchar2(2), countyfp varchar2(3), placefp varchar2(5), name varchar2(90), lsad varchar2(2), topogeom mdsys.sdo_topo_geometry, keys varchar2(10)) nologging';
   execute immediate sql_stmt;
 end if;
 --
 -- Load into temporary table one record for each distinct statefp || countyfp || placefp from face
 -- NOTE: As of 09/09/2009 the PLACEFP field in the FACE table is populated with '00000' instead of NULL. An issue related to DML_CONDITIION
 sql_stmt := 'insert /*+ append */ into fsl157_s (keys) select distinct statefp || countyfp || placefp from '||face_tbl||' where placefp <> ''00000''';
 -- Original sql_stmt
 --  sql_stmt := 'insert /*+ append */ into fsl157_s (keys) select distinct statefp || countyfp || placefp from face where placefp is not null';
 execute immediate sql_stmt;
 commit;
 --
 -- place statefp and countyfp into arrays
 sql_stmt := 'select /*+ parallel 4 */ distinct substr(keys,1,2),substr(keys,3,3) from fsl157_s';
 execute immediate sql_stmt bulk collect into array_statefp,array_countyfp;
 --
 -- loop through the arrays and select a record from FACE where statefp and countyfp match and placefp is null
 FOR i in 1..array_statefp.LAST
  LOOP
   v_statefp := array_statefp(i);
   v_countyfp := array_countyfp(i);
   -- NOTE: As of 09/09/2009 the PLACEFP field in the FACE table is populated with '00000' instead of NULL. An issue related to DML_CONDITIION
   sql_stmt := 'insert into fsl157_s (keys) select distinct statefp || countyfp || placefp from '||face_tbl||' where statefp = :1 and countyfp = :2 and placefp = ''00000''';
   -- Original sql_stmt
   --  sql_stmt := 'insert into fsl157_s (keys) select distinct statefp || countyfp || placefp from face where statefp = :1 and countyfp = :2 and placefp is null';
   execute immediate sql_stmt using v_statefp,v_countyfp;
   commit;
  END LOOP;
 --
 -- fill statefp field
 sql_stmt := 'update /*+ parallel 4 */ fsl157_s set statefp = substr(keys,1,2)';
 execute immediate sql_stmt;
 commit;
 --
 -- fill countyfp field
 sql_stmt := 'update /*+ parallel 4 */ fsl157_s set countyfp = substr(keys,3,3)';
 execute immediate sql_stmt;
 commit;
 --
 -- fill placefp field
 sql_stmt := 'update /*+ parallel 4 */ fsl157_s set placefp = substr(keys,6,5)';
 execute immediate sql_stmt;
 commit;
 -- 
 -- check if index exists
 sql_stmt := 'select count(*) from user_indexes where index_name = :1';
 execute immediate sql_stmt into v_cnt using 'FSL157_S_IDX';
 if v_cnt = 1
  then
   -- drop index if it exists
   sql_stmt := 'drop index fsl157_s_idx';
   execute immediate sql_stmt;
 end if;
 if v_cnt = 0
  then
   -- create index if it doesn't exist
   sql_stmt := 'create index fsl157_s_idx on fsl157_s(statefp,countyfp,placefp) nologging';
   execute immediate sql_stmt;
 end if; 
 --
 -- drop column keys since it is not part of the specification
 sql_stmt := 'alter table fsl157_s drop column keys';
 execute immediate sql_stmt;
 -- count records in fsl157_s 
 sql_stmt := 'select count(*) from fsl157_s';
 execute immediate sql_stmt into v_cnt;
 dbms_output.put_line('Count: ' || v_cnt);
 -- tracking 
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'FSL157_S',v_process,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,v_release,'N/A',v_deploy);
 --
 -- generic exception handling
 --
 EXCEPTION
  WHEN OTHERS THEN
   -- error output
   dbms_output.put_line (SQLERRM);
   dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
   dbms_output.put_line (sql_stmt);
   -- tracking
   v_process := v_process || ' FAILED';
   v_step := v_step || ' FAILED';
   GZ_TOPO_BUILD.gen_tracking_log(topology,'FSL157_S',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
   -- RETURN;
   RAISE;
END create_fsl157_source;
--
PROCEDURE post_fsl172
AS
/**
 ################################################################################################################### 
 # Program Name: post_fsl172
 # Author: mz 
 # Creation Date: 09/08/2009
 # Recent Revisions: 09/28/2009 - Salman 
 # Modification History:
 #   Salman - Added code to de-associate faces 
 #
 # Purpose: 
 #  The purpose of this procedure is to populate the CONCITYFP field in FSL172 after FSL172 is loaded from FACE
 # 
 # Required parameters: None
 #
 # Dependencies: 
 #
 ################################################################################################################### 
*/
sql_stmt varchar2(4000);
array_face_id   MDSYS.SDO_NUMBER_ARRAY;
vface_id        NUMBER;
vftgid            NUMBER;
vftglayer       NUMBER;
vplace           VARCHAR2(5);
vfsllayer       NUMBER;
BEGIN
 -- add concityfp field to fsl172
 sql_stmt := 'alter table fsl172 add concityfp varchar2(5)';
 execute immediate sql_stmt;
 -- populate concityfp field in fsl172
 sql_stmt := 'update fsl172 a set a.concityfp = (select distinct b.concityfp from face b where a.statefp = b. statefp and a.placefp_sl172 = b.placefp_sl172 and b.statefp is not null and b.placefp_sl172 is not null and b.concityfp is not null)';
 execute immediate sql_stmt;
 commit;
 sql_stmt := 'CREATE TABLE Fsl172_temp NOLOGGING AS
              Select f.face_id,
                     f.topogeom.tg_id as face_tg_id,
                     f.topogeom.tg_layer_id as face_tg_layer_id, 
                     a.placefp_sl172 as fsl172_placefp, 
                     f.concityfp as face_concityfp, 
                     a.topogeom.tg_layer_id as fsl172_tg_layer_id, 
                     A.TOPOGEOM.tg_id as fsl172_tg_id, 
                     b.topo_id as relation_topo_id, 
                     b.topo_type as relation_topo_type 
              From fsl172 a, 
                   mt_relation$ b, 
                   face f
              Where f.CONCITYFP is null 
              And a.topogeom.tg_layer_id = b.tg_layer_id 
              And A.TOPOGEOM.tg_id = b.tg_id 
              And b.topo_type = f.topogeom.tg_id
              Order By a.placefp_sl172';
 EXECUTE IMMEDIATE sql_stmt;
 sql_stmt := 'SELECT a.face_id FROM Fsl172_temp a ORDER BY a.face_id';
 EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO array_face_id; 
 FOR i in 1..array_face_id.COUNT LOOP
    vface_id := array_face_id(i);
    sql_stmt := 'SELECT b.face_tg_id, b.face_tg_layer_id, b.fsl172_placefp, b.fsl172_tg_layer_id FROM Fsl172_temp b WHERE b.face_id = :1';
    EXECUTE IMMEDIATE sql_stmt INTO vftgid, vftglayer, vplace, vfsllayer USING vface_id;
    sql_stmt := 'UPDATE fsl172 b SET b.topogeom = SDO_TOPO_GEOMETRY(
                                                  ''MT'',
                                                    3,
                                                    :1, 
                                                    null,
                                                    SDO_TGL_OBJECT_ARRAY (
                                                        SDO_TGL_OBJECT (:2,:3))
                                                    )
                WHERE b.placefp_sl172 = :4';
    EXECUTE IMMEDIATE sql_stmt USING vfsllayer, vftglayer, vftgid, vplace;
 END LOOP;
 Commit;
END post_fsl172;
--
PROCEDURE process_fsl155_fsl312 (topology varchar2, release varchar2, deploy varchar2)
AS
/**
 ################################################################################################################### 
 # Program Name: process_fsl155_fsl312
 # Author: mz 
 # Creation Date: 09/17/2009
 # Recent Revisions:
 #
 # Purpose: 
 #  The purpose of this procedure is to process fsl155/fsl312 for ACS08/ACS09/ACS10
 # 
 # Required parameters:
 #
 # Dependencies: 
 #
 ################################################################################################################### 
*/
sql_stmt varchar2(4000);
begin
 -- drop source fsl155 table if it exists
 sql_stmt := 'drop table ' || release || '_fsl155';
 execute immediate sql_stmt;
 --
 -- create fsl155
 -- sdogeometry mdsys.sdo_geometry was previously included but no longer
 -- sql_stmt := 'create table ACS09_fsl155 (oid number,statefp varchar2(2),placefp varchar2(5),countyfp varchar2(3),name varchar2(100),lsad varchar2(2),cbsafp varchar2(5),pciflag varchar2(1),catkey varchar2(12),topogeom mdsys.sdo_topo_geometry)';
 --
 -- load ACS09_fsl155
 sql_stmt := 'insert into ACS09_fsl155 (statefp,placefp,countyfp) select distinct statefp,placefp,countyfp from face where placefp is not null';
 execute immediate sql_stmt;
 commit;
 -- load CBSAFP into ACS09_FSL155 from FACE
 sql_stmt := 'update ACS09_fsl155 a set a.cbsafp = (select distinct b.cbsafp from face b where a.statefp = b.statefp and a.placefp = b.placefp and a.countyfp = b.countyfp and b.cbsafp is not null and face_id <> -1)';
 execute immediate sql_stmt;
 commit;
 --
 -- load name and lsad
 sql_stmt := 'update /*+ parallel */ ACS09_fsl155 a set a.name = (select b.name from ' || release || '_sl160 b where a.statefp = b.statefp and a.placefp = b.placefp)';
 commit;
 sql_stmt := 'update /*+ parallel */ ACS09_fsl155 a set a.lsad = (select b.lsad from ' || release || '_sl160 b where a.statefp = b.statefp and a.placefp = b.placefp)';
 commit;
 -- the following code is not being executed
 -- *****
 -- update ACS09_fsl155
 -- drop table coucoll;
 -- create table coucoll as select oid,placefp,name,lsad,sdogeometry,b.* from gen_acs07_mh.ACS09_sl160_cdp a, table(a.pcistcoucoll) b where a.pcicbsa = 'Y';
 -- alter table coucoll add statefp varchar2(2);
 -- alter table coucoll add countyfp varchar2(3);
 -- update coucoll set statefp = substr(column_value,1,2);
 -- commit;
 -- update coucoll set countyfp = substr(column_value,3,3);
 -- commit;
 -- insert into ACS09_fsl155 (oid,statefp,placefp,countyfp,name,lsad,sdogeometry) select oid,statefp,placefp,countyfp,name,lsad,sdogeometry from coucoll;
 -- commit;
 -- *****
 -- update catkey in ACS09_FSL155
 sql_stmt := 'update ACS09_fsl155 set catkey = statefp || placefp || cbsafp';
 commit;
 --
 -- add catkey to face
 sql_stmt := 'alter table face add catkey varchar2(12)';
 execute immediate sql_stmt;
 --
 -- update catkey in face
 sql_stmt := 'update /*+ parallel */ face set catkey = statefp || placefp || cbsafp where statefp is not null and placefp is not null and cbsafp is not null';
 commit;
 --
 -- create fsl155 - check if in topo_universe
 -- sql_stmt := 'create table fsl155 as select * from ACS09_fsl155 where rownum < 1;
 -- execute immediate sql_stmt;
 -- load fsl155
 --exec load_fsl155_master;
 --create or replace procedure load_fsl155_master
 --as
 --array_spc mdsys.string_array;
 --v_spc varchar2(10);
 --sql_stmt varchar2(4000);
 --begin
 --sql_stmt := 'select distinct (statefp || placefp || countyfp) from acs08_fsl155 where placefp is not null order by statefp || placefp || countyfp';
 --execute immediate sql_stmt bulk collect into array_spc;
 --for i in 1..array_spc.LAST loop
 -- v_spc := array_spc(i);
 -- load_fsl155 (v_spc);
 --end loop;
 --end;
-- change code 
--create or replace procedure load_fsl155 (v_spc varchar2)
--as
----topo_geom sdo_topo_geometry;
--v_table_fields varchar2(200) := 'STATEFP,PLACEFP,COUNTYFP';
--v_feature_source varchar2(32) := 'ACS08_FSL155';
--v_new_table_name varchar2(32) := 'FSL155';
--v_where_clause varchar2(1000) := 'PLACEFP is not null';
--v_load_table_fields varchar2(1000) := 'load_rec.statefp,load_rec.placefp,load_rec.countyfp';
--v_topo_target_topology varchar2(32) := 'ACS09MH';
--v_dml_condition varchar2(1000);
--sql_stmt varchar2(4000);
--v_state varchar2(2);
--v_place varchar2(5);
--v_county varchar2(3);
--begin
--v_state := substr(v_spc,1,2);
--v_place := substr(v_spc,3,5);
--v_county := substr(v_spc,8,3);
--select '''statefp = ''''' || v_state || ''''' and placefp = ''''' || v_place || ''''' and countyfp = ''''' || v_county || '''''''' into v_dml_condition from dual;
--sql_stmt := 'BEGIN FOR load_rec IN (SELECT ' || v_table_fields || ' FROM ' || v_feature_source || ' where statefp = ''' || v_state || ''' and placefp = ''' || v_place || ''' and countyfp = ''' || v_county || ''' and ' || v_where_clause || ')
--LOOP
-- INSERT INTO ' || v_new_table_name || ' VALUES (' || v_load_table_fields || ',
--   SDO_TOPO_MAP.CREATE_FEATURE(''' || v_topo_target_topology || ''',''' || v_new_table_name || ''',''TOPOGEOM'',' || v_dml_condition || '));
--END LOOP; END;';
--execute immediate (sql_stmt);
--commit;
--EXCEPTION
-- WHEN OTHERS THEN
--  dbms_output.put_line ('State/Place/County: ' || v_spc);
--  dbms_output.put_line (sql_stmt);
--  dbms_output.put_line (SQLERRM);
--  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
--  -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
--  RETURN;
--end;
 -- create fsl312_temp
 -- drop table fsl312_temp;
 -- create table fsl312_temp as 
 -- sql_stmt := 'select oid,cbsafp,statefp,placefp,substr((b.column_value),3,3) as countyfp,name,lsad from ' || release || '_sl160 a,table(a.pcistcoucoll) b where a.pcicbsa = ''Y''';
 -- clear columns cbsafp and sdogeometry
 --update fsl312_temp set cbsafp = null;
 --commit;
 --update fsl312_temp set sdogeometry = null;
 --commit;
 -- create a unique index on statefp,placefp,countyfp
 --create unique index fsl312_temp_idx1 on fsl312_temp (statefp,placefp,countyfp);
 -- set pciflag in fsl155
 --UPDATE fsl155 a set a.pciflag = 'Y'
 --Where a.statefp||a.placefp||a.countyfp IN (
 --    SELECT
 --      b.statefp||b.placefp||b.countyfp
 --    FROM
 --      fsl312_temp b,
 --      fsl155 a
 --    WHERE
 --      a.statefp = b.statefp and a.placefp = b.placefp and a.countyfp = b.countyfp
 --  );
 --commit;
 -- create 'feature source' table
 --drop table ACS09_fsl312;
 --create table ACS09_fsl312 as select distinct catkey,oid,cbsafp,statefp,placefp,name,lsad from fsl155 where pciflag = 'Y';
 -- create empty fsl312 table
 --drop table fsl312;
 --create table fsl312 as select * from ACS09_fsl312 where rownum < 1;
 --alter table ACS09_fsl312 add topogeom mdsys.sdo_topo_geometry;
 --
 --select tg_layer_id from user_sdo_topo_info where table_name = 'FSL155';
 --exec sdo_topo.drop_topo_geometry_layer ('MT','FSL312','TOPOGEOM');
 --exec sdo_topo.add_topo_geometry_layer('MT','FSL312','TOPOGEOM','POLYGON',child_layer_id => 48);
 -- master load procedure for fsl312
 --create or replace
 --procedure load_fsl312_master
 --as
 --array_catkey mdsys.string_array;
 --v_catkey varchar2(12);
 --sql_stmt varchar2(4000);
 --begin
 --sql_stmt := 'select distinct catkey from fsl155 where pciflag = ''Y'' order by catkey';
 --execute immediate sql_stmt bulk collect into array_catkey;
 --for i in 1..array_catkey.LAST loop
 -- v_catkey := array_catkey(i);
 -- load_fsl312 (v_catkey);
 --end loop;
 --end;
 -- subroutine for master load procedure for fsl312
 --create or replace
 --procedure load_fsl312 (v_catkey varchar2)
 --as
 --v_table_fields varchar2(1000) := 'oid,cbsafp,statefp,placefp,name,lsad';
 --v_new_table_name varchar2(32) := 'FSL312';
 --v_topo_target_topology varchar2(100) := 'MT';
 --v_load_table_fields varchar2(1000) := 'load_rec.oid,load_rec.cbsafp,load_rec.statefp,load_rec.placefp,load_rec.name,load_rec.lsad';
 --v_feature_source varchar2(32) := 'ACS09_FSL312';
 --v_dml_condition varchar2(1000);
 --sql_stmt varchar2(4000);
 --begin
 --select '''catkey = ''''' || v_catkey || '''''''' into v_dml_condition from dual;
 --begin
 --sql_stmt := 'BEGIN FOR load_rec IN (SELECT ' || v_table_fields || ' FROM ' || v_feature_source || ' where catkey = ''' || v_catkey ||''') 
 --LOOP
 -- INSERT INTO ' || v_new_table_name || ' VALUES (' || v_load_table_fields || ',
 --  SDO_TOPO_MAP.CREATE_FEATURE(''' || v_topo_target_topology || ''',''' || v_new_table_name || ''',''TOPOGEOM'',' || v_dml_condition || '));
 -- END LOOP; END;';
 --execute immediate (sql_stmt);
 --commit;
 --end;
 --EXCEPTION
 -- WHEN OTHERS THEN
 --  dbms_output.put_line ('catkey: ' || v_catkey);
 --  dbms_output.put_line (sql_stmt);
 --  dbms_output.put_line (SQLERRM);
 --  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
 --  -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
 --  RETURN; 
End process_fsl155_fsl312;
--
PROCEDURE process_fsl155 (topology varchar2, face_tbl varchar2, release varchar2, deploy varchar2)
AS
/**
 ################################################################################################################### 
 # Program Name: process_fsl155
 # Author: mz 
 # Creation Date: 09/17/2009
 # Recent Revisions: Salman - 02/22/2010 
 #
 # Purpose: 
 #  The purpose of this procedure is to process fsl155 for ACS08/ACS09/ACS10
 # 
 # Required parameters:
 #
 # Dependencies: 
 #
 ################################################################################################################### 
*/
sql_stmt varchar2(4000);
v_cnt number;
begin
 -- drop source fsl155 table if it exists
 --sql_stmt := 'drop table ' || release || '_fsl155';
 --execute immediate sql_stmt;
 --
 -- table existence check
 sql_stmt := 'select count(*) from user_tables where table_name = :1';
 execute immediate sql_stmt into v_cnt using release||'_FSL155';
 -- if table doesn't exist, then create table
 if v_cnt = 0
  then
  --
  -- create fsl155
  -- sdogeometry mdsys.sdo_geometry was previously included but no longer
  sql_stmt := 'create table '||release||'_fsl155 (oid number,statefp varchar2(2),placefp varchar2(5),countyfp varchar2(3),name varchar2(90),lsad varchar2(2),cbsafp varchar2(5),pciflag varchar2(1),catkey varchar2(12))';
  execute immediate sql_stmt;
  --
  -- load ACS09_fsl155
  sql_stmt := 'insert into '||release||'_fsl155 (statefp,placefp,countyfp) select distinct statefp,placefp,countyfp from '||face_tbl||' where placefp is not null'; -- <> ''00000''';
  execute immediate sql_stmt;
  commit;
  -- load CBSAFP into ACS09_FSL155 from FACE
  sql_stmt := 'update /*+ parallel */ '||release||'_fsl155 a set a.cbsafp = (select distinct b.cbsafp from '||face_tbl||' b where a.statefp = b.statefp and a.placefp = b.placefp and a.countyfp = b.countyfp and b.cbsafp is not null and face_id <> -1)';
  execute immediate sql_stmt;
  commit;
  --
  -- load name and lsad
  sql_stmt := 'update /*+ parallel */ '||release||'_fsl155 a set a.name = (select b.name from ' || release || '_sl160 b where a.statefp = b.statefp and a.placefp = b.placefp)';
  execute immediate sql_stmt;
  commit;
  sql_stmt := 'update /*+ parallel */ '||release||'_fsl155 a set a.lsad = (select b.lsad from ' || release || '_sl160 b where a.statefp = b.statefp and a.placefp = b.placefp)';
  execute immediate sql_stmt;
  commit;
  -- the following code is not being executed
  -- *****
  -- update ACS09_fsl155
  -- drop table coucoll;
  -- create table coucoll as select oid,placefp,name,lsad,sdogeometry,b.* from gen_acs07_mh.ACS09_sl160_cdp a, table(a.pcistcoucoll) b where a.pcicbsa = 'Y';
  -- alter table coucoll add statefp varchar2(2);
  -- alter table coucoll add countyfp varchar2(3);
  -- update coucoll set statefp = substr(column_value,1,2);
  -- commit;
  -- update coucoll set countyfp = substr(column_value,3,3);
  -- commit;
  -- insert into ACS09_fsl155 (oid,statefp,placefp,countyfp,name,lsad,sdogeometry) select oid,statefp,placefp,countyfp,name,lsad,sdogeometry from coucoll;
  -- commit;
  -- *****
  -- update catkey in ACS09_FSL155
  sql_stmt := 'update /*+ parallel */ '||release||'_fsl155 set catkey = statefp || placefp || cbsafp';
  execute immediate sql_stmt;
  commit;
  --
  -- add catkey to face
  sql_stmt := 'alter table '||face_tbl||' add catkey varchar2(12)';
  execute immediate sql_stmt;
  --
  -- update catkey in face
  sql_stmt := 'update /*+ parallel */ '||face_tbl||' set catkey = statefp || placefp || cbsafp where statefp is not null and placefp is not null and cbsafp is not null';
  execute immediate sql_stmt;
  commit;
 end if;
 --
 -- create fsl155 - check if in topo_universe
 -- sql_stmt := 'create table fsl155 as select * from ACS09_fsl155 where rownum < 1;
 -- execute immediate sql_stmt;
 -- load fsl155
 --exec load_fsl155_master;
 --create or replace procedure load_fsl155_master
 --as
 --array_spc mdsys.string_array;
 --v_spc varchar2(10);
 --sql_stmt varchar2(4000);
 --begin
 --sql_stmt := 'select distinct (statefp || placefp || countyfp) from acs08_fsl155 where placefp is not null order by statefp || placefp || countyfp';
 --execute immediate sql_stmt bulk collect into array_spc;
 --for i in 1..array_spc.LAST loop
 -- v_spc := array_spc(i);
 -- load_fsl155 (v_spc);
 --end loop;
 --end;
-- change code 
--create or replace procedure load_fsl155 (v_spc varchar2)
--as
----topo_geom sdo_topo_geometry;
--v_table_fields varchar2(200) := 'STATEFP,PLACEFP,COUNTYFP';
--v_feature_source varchar2(32) := 'ACS08_FSL155';
--v_new_table_name varchar2(32) := 'FSL155';
--v_where_clause varchar2(1000) := 'PLACEFP is not null';
--v_load_table_fields varchar2(1000) := 'load_rec.statefp,load_rec.placefp,load_rec.countyfp';
--v_topo_target_topology varchar2(32) := 'ACS09MH';
--v_dml_condition varchar2(1000);
--sql_stmt varchar2(4000);
--v_state varchar2(2);
--v_place varchar2(5);
--v_county varchar2(3);
--begin
--v_state := substr(v_spc,1,2);
--v_place := substr(v_spc,3,5);
--v_county := substr(v_spc,8,3);
--select '''statefp = ''''' || v_state || ''''' and placefp = ''''' || v_place || ''''' and countyfp = ''''' || v_county || '''''''' into v_dml_condition from dual;
--sql_stmt := 'BEGIN FOR load_rec IN (SELECT ' || v_table_fields || ' FROM ' || v_feature_source || ' where statefp = ''' || v_state || ''' and placefp = ''' || v_place || ''' and countyfp = ''' || v_county || ''' and ' || v_where_clause || ')
--LOOP
-- INSERT INTO ' || v_new_table_name || ' VALUES (' || v_load_table_fields || ',
--   SDO_TOPO_MAP.CREATE_FEATURE(''' || v_topo_target_topology || ''',''' || v_new_table_name || ''',''TOPOGEOM'',' || v_dml_condition || '));
--END LOOP; END;';
--execute immediate (sql_stmt);
--commit;
--EXCEPTION
-- WHEN OTHERS THEN
--  dbms_output.put_line ('State/Place/County: ' || v_spc);
--  dbms_output.put_line (sql_stmt);
--  dbms_output.put_line (SQLERRM);
--  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
--  -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
--  RETURN;
--end;
End process_fsl155;
--
PROCEDURE process_fsl312 (topology varchar2, fsl155 varchar2, release varchar2, deploy varchar2)
AS
/**
 ################################################################################################################### 
 # Program Name: process_fsl312
 # Author: mz 
 # Creation Date: 09/17/2009
 # Recent Revisions: Salman - 02/22/2010 
 #
 # Purpose: 
 #  The purpose of this procedure is to process fsl312 for ACS08/ACS09/ACS10
 # 
 # Required parameters:
 #
 # Dependencies: 
 #
 ################################################################################################################### 
*/
sql_stmt varchar2(4000);
v_cnt number;
begin
 -- create fsl312_temp
 -- drop table fsl312_temp;
 -- table existence check
  sql_stmt := 'select count(*) from user_tables where table_name = :1';
  execute immediate sql_stmt into v_cnt using release||'_FSL312';
 -- if table doesn't exist, then create table
  if v_cnt = 0
   then
    -- create table fsl312_temp as 
     sql_stmt := 'create table fsl312_temp as select oid,cbsafp,statefp,placefp,substr((b.column_value),3,3) as countyfp,name,lsad from ' || release || '_sl160 a,table(a.pcistcoucoll) b where a.pcicbsa = ''Y''';
     execute immediate sql_stmt;
    -- clear columns cbsafp and sdogeometry
     sql_stmt := 'update fsl312_temp set cbsafp = NULL';
     execute immediate sql_stmt;
     commit;
   --update fsl312_temp set sdogeometry = null;
   --commit;
   -- create a unique index on statefp,placefp,countyfp
    sql_stmt := 'create unique index fsl312_temp_idx1 on fsl312_temp (statefp,placefp,countyfp)';
    execute immediate sql_stmt;
   -- set pciflag in fsl155
    sql_stmt := 'UPDATE '||fsl155||' a set a.pciflag = ''Y''
                 Where a.statefp||a.placefp||a.countyfp IN (
                                               SELECT
                                                 b.statefp||b.placefp||b.countyfp
                                               FROM
                                                 fsl312_temp b,
                                                 '||fsl155||' a
                                               WHERE
                                                 a.statefp = b.statefp and a.placefp = b.placefp and a.countyfp = b.countyfp
     )';
    execute immediate sql_stmt;
    commit;
   --create 'feature source' table
   --drop table ACS09_fsl312;
    sql_stmt := 'create table '||release||'_fsl312 as select distinct catkey,oid,cbsafp,statefp,placefp,name,lsad from '||fsl155||' where pciflag = ''Y''';
    execute immediate sql_stmt;
    commit;
  end if;
  
 -- create empty fsl312 table
 --drop table fsl312;
 --create table fsl312 as select * from ACS09_fsl312 where rownum < 1;
 --alter table ACS09_fsl312 add topogeom mdsys.sdo_topo_geometry;
 --
 --select tg_layer_id from user_sdo_topo_info where table_name = 'FSL155';
 --exec sdo_topo.drop_topo_geometry_layer ('MT','FSL312','TOPOGEOM');
 --exec sdo_topo.add_topo_geometry_layer('MT','FSL312','TOPOGEOM','POLYGON',child_layer_id => 48);
 -- master load procedure for fsl312
 --create or replace
 --procedure load_fsl312_master
 --as
 --array_catkey mdsys.string_array;
 --v_catkey varchar2(12);
 --sql_stmt varchar2(4000);
 --begin
 --sql_stmt := 'select distinct catkey from fsl155 where pciflag = ''Y'' order by catkey';
 --execute immediate sql_stmt bulk collect into array_catkey;
 --for i in 1..array_catkey.LAST loop
 -- v_catkey := array_catkey(i);
 -- load_fsl312 (v_catkey);
 --end loop;
 --end;
 -- subroutine for master load procedure for fsl312
 --create or replace
 --procedure load_fsl312 (v_catkey varchar2)
 --as
 --v_table_fields varchar2(1000) := 'oid,cbsafp,statefp,placefp,name,lsad';
 --v_new_table_name varchar2(32) := 'FSL312';
 --v_topo_target_topology varchar2(100) := 'MT';
 --v_load_table_fields varchar2(1000) := 'load_rec.oid,load_rec.cbsafp,load_rec.statefp,load_rec.placefp,load_rec.name,load_rec.lsad';
 --v_feature_source varchar2(32) := 'ACS09_FSL312';
 --v_dml_condition varchar2(1000);
 --sql_stmt varchar2(4000);
 --begin
 --select '''catkey = ''''' || v_catkey || '''''''' into v_dml_condition from dual;
 --begin
 --sql_stmt := 'BEGIN FOR load_rec IN (SELECT ' || v_table_fields || ' FROM ' || v_feature_source || ' where catkey = ''' || v_catkey ||''') 
 --LOOP
 -- INSERT INTO ' || v_new_table_name || ' VALUES (' || v_load_table_fields || ',
 --  SDO_TOPO_MAP.CREATE_FEATURE(''' || v_topo_target_topology || ''',''' || v_new_table_name || ''',''TOPOGEOM'',' || v_dml_condition || '));
 -- END LOOP; END;';
 --execute immediate (sql_stmt);
 --commit;
 --end;
 --EXCEPTION
 -- WHEN OTHERS THEN
 --  dbms_output.put_line ('catkey: ' || v_catkey);
 --  dbms_output.put_line (sql_stmt);
 --  dbms_output.put_line (SQLERRM);
 --  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
 --  -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
 --  RETURN; 
End process_fsl312;
--
PROCEDURE create_fsl172_source (topology varchar2, face_tbl varchar2, release varchar2, deploy varchar2)
AS
/**
 ################################################################################################################### 
 # Program Name: create_fsl172_source
 # Author: Salman 
 # Creation Date: 02/24/2010
 # Recent Revisions:
 #
 # Purpose: 
 #  The purpose of this procedure is to load table FSL172 for ACS09
 #  From FACE:
 #   1) build one record for each distinct statefp || concityfp || placefp combination where
 #        concityfp is not null and place is not null
 #   
 # 
 # Required parameters: topology, release, deploy (schema)
 #
 # Dependencies: None
 #
 ################################################################################################################### 
*/
v_process varchar2(100) := 'Load FSL172'; -- Process
v_step varchar2(4000); -- Step
v_start_time timestamp; -- Start time
v_end_time timestamp; -- End time
v_elapsed_time interval day(5) to second (2); -- Elapsed time
v_proc_start_time timestamp; -- Procedure Start Time
v_proc_step varchar2(4000); -- Procedure Step
v_proc_end_time timestamp; -- Procedure End time
v_proc_elapsed_time interval day(5) to second (2); -- Procedure Elapsed time
--
array_tbl_name mdsys.string_array;
v_tbl_name varchar2(32) := release||'_FSL172';
array_source_location mdsys.string_array;
v_source_location varchar2(250);
array_tbl_keys mdsys.string_array;
v_tbl_keys varchar2(250);
array_deploy mdsys.string_array;
v_deploy varchar2(250);
array_release mdsys.string_array;
v_release varchar2(250) := null;
array_seq_num mdsys.sdo_number_array;
v_seq_num number;
array_tbl_statefp mdsys.string_array;
v_tbl_statefp varchar2(30);
statefp_exists number;
--
sql_stmt varchar2(4000); -- Dynamic SQL Statement 
sql_stmt2 varchar2(4000); -- Dynamic SQL Statement #2 for GEN_TRACKING
v_cnt number;
--
v_table_fields varchar2(4000);
v_load_table_fields varchar2(4000);
--
v_where_clause varchar2(4000);
v_where_clause_full varchar2(4000);
--
v_dml_condition varchar2(4000);
--
mbr_geom SDO_GEOMETRY; -- minimum bond rectangle
--
statefp_tbl varchar2(32);
v_statefp_tbl varchar2(32);
--
v_tbl_keys2 varchar2(1000);
--
array_key_data mdsys.string_array;
--
v_tbl_keys_data varchar2(4000);
--
comma_cnt number;
--
v_len_keys varchar2(4000);
--
BEGIN
 v_start_time := systimestamp;
 v_step := 'Create FSL172 source' || v_tbl_name;
 --
 -- create temp table
 --
 -- table existence check
 sql_stmt := 'select count(*) from user_tables where table_name = :1';
 execute immediate sql_stmt into v_cnt using release||'_FSL172';
 if v_cnt = 1 
  then
   -- if table exists then truncate table
   sql_stmt := 'truncate table '||release||'_fsl172';
   execute immediate sql_stmt;
 end if;
 if v_cnt = 0
  then
   -- if table doesn't exist, then create table
   sql_stmt := 'create table '||release||'_fsl172 (statefp varchar2(2), concityfp varchar2(5), placefp varchar2(5), name varchar2(90), lsad varchar2(2), topogeom mdsys.sdo_topo_geometry) nologging';
   execute immediate sql_stmt;
 end if;
 --
 -- Load into temporary table one record for each distinct statefp || concityfp || placefp from face
 --
 -- NOTE: As of 09/09/2009 the PLACEFP field in the FACE table is populated with '00000' instead of NULL. An issue related to DML_CONDITIION
 sql_stmt := 'insert /*+ append */ into '||release||'_fsl172 (statefp,concityfp,placefp) select distinct statefp, concityfp, placefp from '||face_tbl||' where concityfp is not null and placefp is not null'; -- <> ''00000''';
 -- original sql_stmt
 --  sql_stmt := 'insert /*+ append */ into fsl070_s (keys) select distinct statefp || countyfp || cousubfp || placefp from face where cousubfp is not null and placefp is not null';
 execute immediate sql_stmt;
 commit;
 --
 -- load name and lsad
 sql_stmt := 'update /*+ parallel */ '||release||'_fsl172 a set a.name = (select b.name from ' || release || '_sl160 b where a.statefp = b.statefp and a.placefp = b.placefp)';
 execute immediate sql_stmt;
 commit;
 sql_stmt := 'update /*+ parallel */ '||release||'_fsl172 a set a.lsad = (select b.lsad from ' || release || '_sl160 b where a.statefp = b.statefp and a.placefp = b.placefp)';
 execute immediate sql_stmt;
 commit;
 --
 -- determine whether index exists or not
 sql_stmt := 'select count(*) from user_indexes where index_name = :1';
 execute immediate sql_stmt into v_cnt using release||'_fsl172_IDX';
 if v_cnt = 1
  then
   -- drop index if it exists
   sql_stmt := 'drop index '||release||'_fsl172_idx';
   execute immediate sql_stmt;
 end if;
 if v_cnt = 0
  then
   -- create index if it doesn't exist
   sql_stmt := 'create index '||release||'_fsl172_idx on '||release||'_fsl172 (statefp,concityfp,placefp) nologging';
   execute immediate sql_stmt;
 end if; 
 --
 -- count records 
 sql_stmt := 'select count(*) from '||release||'_fsl172';
 execute immediate sql_stmt into v_cnt;
 dbms_output.put_line('Count: ' || v_cnt);
 -- tracking
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 GZ_TOPO_BUILD.gen_tracking_log(topology,'FSL172_S',v_process,'COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,v_release,'N/A',v_deploy);
 --
 -- generic exception handling
 --
 EXCEPTION
  WHEN OTHERS THEN
   -- error output
   dbms_output.put_line (SQLERRM);
   dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
   dbms_output.put_line (sql_stmt);
   -- tracking
   v_process := v_process || ' FAILED';
   v_step := v_step || ' FAILED';
   GZ_TOPO_BUILD.gen_tracking_log(topology,'FSL172_S',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy);
   -- RETURN;
   RAISE;
END create_fsl172_source;
--
END GZ_TOPO_BUILD;
/


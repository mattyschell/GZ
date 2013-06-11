CREATE OR REPLACE PACKAGE BODY GZ_SMPOLY AS
----------------------------------------
-- FUNCTIONS for creating tables
----------------------------------------
 FUNCTION NEW_SMALL_POLYGON_PARAMETERS RETURN GZ_TYPES.SMALL_POLYGON_PARAMETERS PIPELINED
    AS
    BEGIN
       NULL;
    END NEW_SMALL_POLYGON_PARAMETERS;
--------------------------------------------------------------------------------
 FUNCTION NEW_DELETE_EDGES_INPUT RETURN GZ_TYPES.DELETE_EDGES_INPUT PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_DELETE_EDGES_INPUT;
--------------------------------------------------------------------------------
  FUNCTION NEW_GEN_SP_TRACKING RETURN GZ_TYPES.GEN_SP_TRACKING PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_SP_TRACKING;
--------------------------------------------------------------------------------
  FUNCTION NEW_EDGE_ATTRIBUTE RETURN GZ_TYPES.EDGE_ATTRIBUTE PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_EDGE_ATTRIBUTE;

------------------------------------------------------------------------------
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
------------------------------------------------------------------------------


FUNCTION START_SP_LOGGING (
   pSchema       IN VARCHAR2,
   pJobRun       IN VARCHAR2
)RETURN VARCHAR2
AS
   --Copied from Matt's clipper
   --Create small polygon logging table for this topology
   --The jobrun = the topology you are working on
  vSchema VARCHAR2(32) := UPPER(pSchema);
  vJobRun VARCHAR2(20) := UPPER(pJobrun); -- the topology name
BEGIN
   GZ_UTILITIES.CREATE_GEN_XTEND_TRACKING_LOG(SYS_CONTEXT('USERENV', 'CURRENT_USER'),
                                                    vJobRun || '_SP_TRACKING');

   GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', pJobRun,
                                                'START_SP_LOGGING', NULL,
                                                'STARTING SP REMOVAL  JOB: ' || pJobRun);


   RETURN '0';
END START_SP_LOGGING;
----------------------------------------
-- OTHER FUNCTIONS
--------------------------------------------------------------------------------
FUNCTION COMPARE_FACES (
   p_face1 NUMBER,
   p_face2 NUMBER,
   p_face_table VARCHAR2,
   p_columns_tab VARCHAR2 default 'GEOID_COMPONENTS',
   p_comp_column VARCHAR2 default 'FACE_ID'
   ) RETURN my_comparison_table PIPELINED AS
   ----
   /*
   Stephanie 20100810
   parameters
   p_face1 NUMBER,  <- a face_id or unique feature id
   p_face2 NUMBER,  <- another face_id or unique id
   p_face_table VARCHAR2, <- the table these two faces are stored in
   p_columns_tab VARCHAR2, default 'GEOID_COMPONENTS' <- a table storing all
                 the values you want to compare in a filed called 'column_name'
   p_comp_column <- the OID or unique identifier for the feature, usually
                    face_id
   This fucntion returns a pipelined table comparing the attributes of two
   records in a table based upon a table containing the columns to compare.
   This is primarily used when interactively reviewing the zero-level
   (polygon-based) feature layer in the generalization workflow.
   This is just a helper program, and not part of the generalization work flow.
   Use it this way to get a detailed list of where two records match or are
   different...
   -- all differences (example)
   select *
   from table (SS_GEN_STUFF.compare_faces(
               12345,12346,'FACE2','geoid_components') );
   Use it this way to know if the two records are different at all...
   -- are they the same or not (example)
   select * from table (SS_GEN_STUFF.compare_faces(
                        740270,740261,'FACE2','geoid_components'))
   where compare = 'FACE_ID'
   UNION ALL
   select * from table (SS_GEN_STUFF.compare_faces(
                       740268,740261,'FACE2','geoid_components'))
   where compare = 'FACE_ID';
   */
   ----
   v_message VARCHAR2 (4000);
   v_comprec my_comparison_rec;
   v_status VARCHAR2 (4000);
   v_sql VARCHAR2(4000);
   v_overall_status VARCHAR2(4000) := 'equal';
   ----
   v_column_list GZ_TYPES.stringarray;
   ----
   v_value1 VARCHAR2(4000) := NULL;
   v_value2 VARCHAR2(4000) := NULL;
   ----
   BEGIN
      DBMS_OUTPUT.PUT_LINE('------------------------------------');
      DBMS_OUTPUT.PUT_LINE('Column: '||v_comprec.compare);
      DBMS_OUTPUT.PUT_LINE('Value1: '||v_comprec.value1);
      DBMS_OUTPUT.PUT_LINE('Value2: '||v_comprec.value2);
      DBMS_OUTPUT.PUT_LINE('Status: '||v_comprec.status);
   -- next get the list of columns to compare from the table
   v_sql := 'SELECT column_name from '||p_columns_tab;
   EXECUTE IMMEDIATE v_sql BULK COLLECT INTO v_column_list;
   DBMS_OUTPUT.PUT_LINE('------------------------------------');
   DBMS_OUTPUT.PUT_LINE(v_column_list.COUNT||' columns to check');
   -- loop through each column and compare the values
   FOR i IN 1..v_column_list.COUNT Loop
      DBMS_OUTPUT.PUT_LINE('------------------------------------');
      DBMS_OUTPUT.PUT_LINE('checking column... '||v_column_list(i));
      v_comprec.compare := NULL;
      v_comprec.value1 := NULL;
      v_comprec.value2 := NULL;
      v_comprec.status := NULL;
      v_sql := 'SELECT '||v_column_list(i)||
               ' FROM '||p_face_table ||
               ' WHERE '||p_comp_column||' = :p1';
      BEGIN
          EXECUTE IMMEDIATE v_sql INTO v_value1 USING p_face1;
          EXCEPTION
             WHEN NO_DATA_FOUND THEN
                DBMS_OUTPUT.PUT_LINE('no data found for face '||p_face1);
                v_overall_status := 'FAILED TO COMPARE';
                EXIT;
      END;
      BEGIN
          EXECUTE IMMEDIATE v_sql INTO v_value2 USING p_face2;
          EXCEPTION
             WHEN NO_DATA_FOUND THEN
                DBMS_OUTPUT.PUT_LINE('no data found for face '||p_face2);
                v_overall_status := 'FAILED TO COMPARE';
                EXIT;
      END;
      v_comprec.compare := v_column_list(i);
      v_comprec.value1 := v_value1;
      v_comprec.value2 := v_value2;
      IF (v_value1 = v_value2) OR (v_value1 IS NULL AND v_value2 IS NULL) THEN
         v_comprec.status := 'equal';
         ELSE
            v_comprec.status := 'not equal';
         IF v_overall_status = 'equal' THEN
            v_overall_status := 'not equal in: '||v_comprec.compare;
         ELSE
            v_overall_status := v_overall_status ||', '||v_comprec.compare;
         END IF;
      END IF;
      DBMS_OUTPUT.PUT_LINE('------------------------------------');
      DBMS_OUTPUT.PUT_LINE('Column: '||v_comprec.compare);
      DBMS_OUTPUT.PUT_LINE('Value1: '||v_comprec.value1);
      DBMS_OUTPUT.PUT_LINE('Value2: '||v_comprec.value2);
      DBMS_OUTPUT.PUT_LINE('Status: '||v_comprec.status);
      PIPE ROW(v_comprec);
   END LOOP;
   DBMS_OUTPUT.PUT_LINE('------------------------------------');
   DBMS_OUTPUT.PUT_LINE('------------------------------------');
   DBMS_OUTPUT.PUT_LINE('Final Status = '||v_overall_status);
   -- finally list the faces you are comparing and the overall status
   v_comprec.compare := 'FACE_ID';
   v_comprec.value1 := p_face1;
   v_comprec.value2 := p_face2;
   v_comprec.status := UPPER(v_overall_status);
   PIPE ROW(v_comprec);
END compare_faces;
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
FUNCTION are_my_polys_valid (
   pTopology VARCHAR2,
   pFeatureTab VARCHAR2,
   pPrimaryKey VARCHAR2,
   pTolerance number DEFAULT 0.05,
   pFixValErr CHAR DEFAULT 'N'
) RETURN BOOLEAN
AS

   /*
     returns true if none of your records have errors using the sdogeometry created
     from the topogeom and validate geometry with context
     pFeatureTable -- a feature table with a topogeom column
     pPrimary Key -- a column in the feature table with unique values

     Returns FALSE if any topogeom in the table
        1) can not build an SDO GEOMETRY
        2) has a gtype <> 3 (expecting single face polygons, no multi-polygons allowed
        3) fails to validate with context (using .05 meters tolerance and projection 8265?)

     If pFixValErr is set to 'Y' the program will call the topo fixer.
     Otherwise returns TRUE
     (usually run on the face-based feature table after line simplification)
   */


   vTopology varchar2(20) := UPPER(pTopology);
   vFTab varchar2(4000) := UPPER(pFeatureTab);
   vPK varchar2(4000) := UPPER(pPrimaryKey);
   vCallingModule VARCHAR2(20); -- := 'ARE_MY_POLYS_VALID';
   vFixValErr char(1) := pFixValErr;
   vSql varchar2(4000);
   vGtype NUMBER;
   vRecs GZ_TYPES.stringarray;
   vRetVal BOOLEAN := TRUE;
   vlogRetVal varchar2(10);
   vContinue BOOLEAN := TRUE;
   vMsg varchar2(4000) := 'Are_My_Polys_Valid: ';
   vResult varchar2(4000);
   vResultSubStr varchar2(5);
   vSDOGeom SDO_GEOMETRY;
   vTrackingTable varchar2(30);
   vProcess varchar2(30);
   vNote varchar2(4000);
   v_code varchar2(4000); -- erorr reporting
   v_errm varchar2(4000); -- erorr reporting

   vCallTopoFixer BOOLEAN := FALSE;
   vTopoFixRetVal number;

   tblCount number;


   --  Write log procedure()
     PROCEDURE WRITE_LOG(p_Topo VARCHAR2, p_Process VARCHAR2, p_Message VARCHAR2)
     IS

     BEGIN

           CASE p_Process
              WHEN 'SP' THEN
                  GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', p_topo, p_Process,
                                                                  p_sqlstmt=>p_Process || ' Are_My_Polys_Valid',
                                                                  p_error_msg=>p_Message);
              WHEN 'LS' THEN
                  GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('LS', p_topo, p_Process,
                                                                  p_sqlstmt=>p_Process || ' Are_My_Polys_Valid',
                                                                  p_error_msg=>p_Message);

           END CASE;

          DBMS_OUTPUT.PUT_LINE(p_Topo || ' - ' ||  p_Process || ' - ' || p_Message);
     END;


BEGIN

   vRetVal := TRUE;

   vCallingModule := pTopology;

   IF substr(pTopology, instr(pTopology,'LS'), 2) = 'LS'
   THEN
      --vCallingModule := pTopology || '_LS';
      -- Check to see if the tracking table exists
      vSQL := 'Select count(*) from user_tables where table_name = :1';
      Execute immediate vSQL into tblCount using pTopology || '_LS_TRACKING';
      If tblCount = 1 then
         vProcess := 'LS';
      Else
         vProcess := NULL;
      End If;

   ELSIF substr(pTopology, instr(pTopology,'SP'), 2) = 'SP'
   THEN
      --vCallingModule := pTopology || '_SP';
      vSQL := 'Select count(*) from user_tables where table_name = :1';
      Execute immediate vSQL into tblCount using pTopology || '_SP_TRACKING';
      If tblCount = 1 then
         vProcess := 'SP';
      Else
         vProcess := NULL;
      End If;

   ELSE
      vProcess := NULL;
      vCallingModule := 'Are_My_Polys_Valid';
   END IF;

   dbms_output.put_line('vProcess: ' || vProcess || ' vCallingModule: ' || vCallingModule);

   vsql := 'select a.'||vPK ||' '||
           'from '||vFTab||' a ' ||
           'order by a.'||vPK;

   BEGIN
      EXECUTE IMMEDIATE vSql BULK COLLECT INTO vRecs;

      dbms_output.put_line('vRecs.Count = ' || vRecs.Count);
      FOR i IN 1..vRecs.COUNT
      LOOP
          vContinue := TRUE;
          -- get SDO GEOMETRY
          vsql := 'select a.sdogeometry FROM '||
                   vFTab||' a where a.'||vPK||' = :p1';

          BEGIN
             EXECUTE IMMEDIATE vSQL INTO vSDOGeom USING vRecs(i);
          EXCEPTION
          WHEN OTHERS THEN
             vNote := 'vSQL: ' || vSQL || ' Error getting SDOGEOMETRY for '||vRecs(i) || ' SQLERRM: ' || SQLERRM;
             WRITE_LOG(vTopology, vProcess, vNote);
             vRetVal := FALSE;
             vContinue := FALSE;
          END;

         -- Validate with Context
          IF vContinue
          THEN
             IF vSDOGeom.get_Gtype != 3 THEN
               vNote := ' Record '||vRecs(i)||' gtype is BAD.  GTYPE = '||vGtype;
               WRITE_LOG(vTopology, vProcess, vNote);
               vRetVal := FALSE;
             END IF;

             vResult := SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(vSDOGeom,pTolerance);
             vResultSubStr := substr(vResult,1,5);

          END IF;

          IF vResultSubStr != 'TRUE'
          THEN
               vNote := ' Record '||vRecs(i)||' is NOT VALID. Validation Result = '||vResult;
               WRITE_LOG(vTopology, vProcess, vNote);

             IF (vResultSubStr = '13356' or vResultSubStr = '13349') and (vFixValErr = 'Y') THEN
                -- Call topo fixer here. to fix "Duplicate Vertex" errors (13356) and "Self Intersection" errors (13349)

                vCallTopoFixer := TRUE;

             ELSE

                vRetVal := FALSE;
             END IF;

          --ELSE
          --   Default is vRetVal = TRUE.  So do nothing.
          END IF;

      END LOOP;

      If vCallTopoFixer = TRUE Then
         vTopoFixRetVal := GZ_TOPOFIX.GZ_FIX_FACE(vCallingModule,vTopology,vFTab, 'SP');
         vNote := 'vTopoFixRetVal: ' || vTopoFixRetVal;
         WRITE_LOG(vTopology, vProcess, vNote);

         If vTopoFixRetVal > 0 Then
            vRetVal := FALSE;
         End If;
      End If;

     If vRetVal = TRUE Then
        vNote := 'Successfully executed are_my_polys_valid';
        WRITE_LOG(vTopology, vProcess, vNote);
     Else
        vNote := 'Executed are_my_polys_valid with a failed status';
        WRITE_LOG(vTopology, vProcess, vNote);
     End If;

      return vRetVal;

   Exception
       WHEN OTHERS THEN
          dbms_output.put_line('vSQL: ' || vSQL);
          dbms_output.put_line(' Error executing the above SQL');
          dbms_output.put_line(SQLERRM);
         vNote := 'Error executing SQL: ' || vSQL||' ErrMsg: ' || SQLERRM;
         WRITE_LOG(vTopology, vProcess, vNote);
          vRetVal := FALSE;
          vContinue := FALSE;
   End;

END are_my_polys_valid;
--------------------------------------------------------------------------------
FUNCTION are_my_poly_features_valid (
   pFeatureTab VARCHAR2,
   pPrimaryKey VARCHAR2
) RETURN BOOLEAN
AS
/*
returns true if none of your records have errors using the sdogeometry created
from the topogeom and validate geometry with context
pFeatureTable -- a feature table with a topogeom column
Returns FALSE if any topogeom in the table
   1) can not build an SDO GEOMETRY
   2) has a gtype <> 3 or a Gtype <> 7 (expecting polygons or multi-polygons )
   3) fails to validate with context (using .05 meters tolerance and projection 8265?)
   lots of these failures will be duplicate vertices (13356) - those should be fixable
Otherwise returns TRUE
(usually run on the face-based feature table after line simplification)
*/
vFTab varchar2(4000) := UPPER(pFeatureTab);
vPK varchar2(4000) := UPPER(pPrimaryKey);
vSql varchar2(4000);
vGtype NUMBER;
vRecs GZ_TYPES.stringarray;
vRetVal BOOLEAN := TRUE;
vContinue BOOLEAN := TRUE;
vMsg varchar2(4000) := 'Are My Poly Features Valid: ';
vResult varchar2(4000);
vSDOGeom SDO_GEOMETRY;
BEGIN
   vsql := 'select a.'||vPK ||' '||
           'from '||vFTab||' a ' ||
           'order by a.'||vPK;
   BEGIN
       EXECUTE IMMEDIATE vSql BULK COLLECT INTO vRecs;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         dbms_output.put_line(vMsg||' NO RECORDS FOUND in '||vFTab );
         vRetVal := TRUE;
      WHEN OTHERS
      THEN
         dbms_output.put_line(vMsg||' ERROR checking table '||vFTab||
                             '. vSql = '''||vsql||'''.  '||
                             SQLERRM );
         return FALSE;
   END;
   dbms_output.put_line(vMsg||' ----------------------------------');
   dbms_output.put_line(vMsg||' Checking '||vRecs.COUNT|| ' records in '||vFTab );
   FOR i IN 1..vRecs.COUNT
   LOOP
       vContinue := TRUE;
       -- make SDO GEOMETRY
       vsql := 'select a.topogeom.get_geometry() FROM '||
               vFTab||' a where a.'||vPK||' = :p1';
       BEGIN
          EXECUTE IMMEDIATE vSQL INTO vSDOGeom USING vRecs(i);
       EXCEPTION
          WHEN OTHERS THEN
             dbms_output.put_line(vMsg);
             dbms_output.put_line(vMsg||' -- Error building SDOGEOMETRY for '||vRecs(i));
             dbms_output.put_line(vMsg||' -- '||SQLERRM);
             dbms_output.put_line(vMsg||' ----------------------------------');
             vRetVal := FALSE;
             vContinue := FALSE;
       END;
       IF vContinue
       THEN
          vGtype := 99999;
          vsql := 'select a.topogeom.get_geometry().get_gtype() '||
                  'from '||vFTab||' a where a.'||vPK||' = :p1';
          EXECUTE IMMEDIATE vSQL INTO vGtype USING vRecs(i);
          IF (vGtype != 3 and vGtype != 7 )
          THEN
             --dbms_output.put_line(vMsg||' Record '||vRecs(i)||' gtype is good = '||vGtype);
             -- ELSE
             dbms_output.put_line(vMsg||' Record '||vRecs(i)||' gtype is BAD = '||vGtype);
             vRetVal := FALSE;
          END IF;
       END IF;
          -- Validate with Context
       IF vContinue
       THEN
          vResult := SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(vSDOGeom,.05);
       ELSE
          vResult := 'FALSE';
       END IF;
       IF vResult != 'TRUE'
       THEN
          --dbms_output.put_line(vMsg||' Record '||vRecs(i)||' is valid.');
          --dbms_output.put_line(vMsg||' ---------------------------------');
          -- ELSE
          dbms_output.put_line(vMsg||' Record '||vRecs(i)||' is NOT VALID.');
          dbms_output.put_line(vMsg||'        Gtype =  '||vGtype);
          dbms_output.put_line(vMsg||'        Validation Result = '||vResult);
          dbms_output.put_line(vMsg||'----------------------------------');
       END IF;
       IF vResult != 'TRUE'
       THEN
          vRetVal := FALSE;
       END IF;
   END LOOP;
   return vRetVal;
END are_my_poly_features_valid;
-------------------------------------------------------------------------------
FUNCTION ARE_MY_SDO_GEOMETRIES_VALID (
  pTable varchar2,
  pPKColumn varchar2 DEFAULT 'GEO_ID',
  pGeomColumn varchar2 DEFAULT 'SDOGEOMETRY',
  pTolerance number DEFAULT 0.05
) Return BOOLEAN AS
-- helper to check all the SDO GEOMETRIES in a table usign validate with context
-- and report the results
-- paramters...
----- Table = the table to check all the records in
----- PKColumn = the promary key column (for looping and reporting results)
----- GeomColumn = the column containing SDO GEOMETRIES
----- pTolerance = the tolerance to pass to validate GEOmetry with context
vTab varchar2(4000) := UPPER(pTable);
vPK varchar2(4000) := UPPER(pPKColumn) ;
vSDOColumn varchar2(4000) := UPPER(pGeomColumn);
vSql varchar2(4000);
vRetVal BOOLEAN := TRUE;
vRecs GZ_TYPES.stringarray;
vContinue BOOLEAN := FALSE;
vSDOGeom MDSYS.SDO_GEOMETRY;
vResult varchar2(4000) := 'FALSE';
vMsg varchar2(4000) := 'Are_My_Sdo_Geometries_Valid: ';
BEGIN
 vsql := 'select a.'||vPK ||' '||
           'from '||vTab||' a ' ||
           'order by a.'||vPK;
   BEGIN
       EXECUTE IMMEDIATE vSql BULK COLLECT INTO vRecs;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         dbms_output.put_line(vMsg||' No records found in '||vTab );
         vRetVal := TRUE;
      WHEN OTHERS
      THEN
         dbms_output.put_line(vMsg||' Error retrieveing records from '||vTab );
         return FALSE;
   END;
   dbms_output.put_line(vMsg||' ----------------------------------');
   dbms_output.put_line(vMsg||' Checking '||vRecs.COUNT|| ' records in '||vTab );
   FOR i IN 1..vRecs.COUNT
   LOOP
       vsql := 'select a.'||vSDOColumn||' FROM '||
               vTab||' a where a.'||vPK||' = :p1';
       EXECUTE IMMEDIATE vSQL INTO vSDOGEOM USING vRecs(i);
       vResult := SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(vSDOGeom,pTolerance);
       IF vResult != 'TRUE'
       THEN
          dbms_output.put_line(vMsg||' Record '||vRecs(i)||' is NOT VALID.');
          dbms_output.put_line(vMsg||'        Validation Result = '||vResult);
          dbms_output.put_line(vMsg||'----------------------------------');
          vRetVal := FALSE;
       END IF;
   END LOOP;
   return vRetVal;
END ARE_MY_SDO_GEOMETRIES_VALID;
--------------------------------------------------------------------------------
-- PROCEDURES TO CREATE TABLES
--------------------------------------------------------------------------------
PROCEDURE CREATE_GEN_SP_PARAMETERS (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2 DEFAULT 'SMALL_POLYGON_PARAMETERS' -- should change to 'GEN_SP_PARAMETERS'
   )
   AS
      -- Copied from Matt (GZ_CLIP) with minor modifications for small poly parm table.
      -- Stephanie 12/16/2010
      --Creates empty clip parameters table
      --Clip parameters table is a child of the SCHEMA. Theres just one per schema
      -- NOTE:  NEED TO CHANGE DEFAULT NAME to 'GEN_SP_PARAMETERS', but rest of code expects SMALL_POLYGON_PARAMETERS
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
                   || 'SELECT * FROM TABLE(' || p_schema || '.GZ_SMPOLY.NEW_SMALL_POLYGON_PARAMETERS ) ';
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
              || '      PRIMARY KEY(GEN_PROJECT_ID) '
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
                      || 'END;';
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Grant privileges on ' || p_table_name);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);


END CREATE_GEN_SP_PARAMETERS;
-----------------------------------------------------------------------------------------
--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
-----------------------------------------------------------------------------------------
PROCEDURE CREATE_DELETE_EDGES_INPUT (
   p_schema         IN VARCHAR2,
   p_table_name     IN VARCHAR2 DEFAULT 'DELETE_EDGES_INPUT'
)
AS
   --Stephanie
   --Copied from Matt's Clip Parameter Table Creator
   --Creates empty del edges input table
   --There is one for each topology created after the clip
   psql          VARCHAR2(4000);
   v_object_root VARCHAR2(4000) := p_table_name;  --??
BEGIN
   ----------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO(
                 'Create the empty table as an empty pipelined custom type');
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ----------------------------------------------------------------------------------
   psql := 'CREATE TABLE ' || p_table_name || ' ';
   psql := psql || ' NOPARALLEL NOLOGGING AS '
                || 'SELECT * FROM TABLE(' ||
                p_schema || '.GZ_SMPOLY.NEW_DELETE_EDGES_INPUT ) ';
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
  ----------------------------------------------------------------------------
  --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
  DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add constraints to ' || p_table_name);
  --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ----------------------------------------------------------------------------
    EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
           || 'ADD ('
           || '   CONSTRAINT ' || v_object_root || 'PKC '
           || '      PRIMARY KEY(EDGE_ID) '
           || ')';
   ---------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Grant privileges on ' || p_table_name);
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ----------------------------------------------------------------------------

   GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);


END CREATE_DELETE_EDGES_INPUT;
--------------------------------------------------------------------------------
PROCEDURE CREATE_GEN_SP_TRACKING (
   p_schema         IN VARCHAR2,
   p_table_name     IN VARCHAR2 DEFAULT 'GEN_SP_TRACKING'
)
AS
   --Stephanie
   --Copied from Matt's Clip Log Table Creator
   --Creates empty sp tracking table
   --There is one for each state
   psql          VARCHAR2(4000);
   v_object_root VARCHAR2(4000) := p_table_name;  --??
BEGIN
   ----------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Create the empty table as an empty pipelined custom type');
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ----------------------------------------------------------------------------------
   psql := 'CREATE TABLE ' || p_table_name || ' ';
   psql := psql || ' NOPARALLEL NOLOGGING AS '
                || 'SELECT * FROM TABLE(' || p_schema || '.GZ_SMPOLY.NEW_GEN_SP_TRACKING ) ';
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
   --DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Grant privileges on ' || p_table_name);
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ----------------------------------------------------------------------------------

   GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);


END CREATE_GEN_SP_TRACKING;
--------------------------------------------------------------------------------
PROCEDURE CREATE_EDGE_ATTRIBUTE (
   p_schema         IN VARCHAR2,
   p_table_name     IN VARCHAR2 DEFAULT 'GEN_EDGE_ATT'
)
AS
   --Stephanie
   --Copied from Matt's Clip Log Table Creator
   --Creates empty edge attribute table
   --this is tied to the topology, but not as a feature table
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
                || 'SELECT * FROM TABLE(' || p_schema || '.GZ_SMPOLY.NEW_EDGE_ATTRIBUTE ) ';
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

   GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);


END CREATE_EDGE_ATTRIBUTE;
--------------------------------------------------------------------------------
-- OTHER PROCEDURES
--------------------------------------------------------------------------------
PROCEDURE bigu_review (
 p_topology VARCHAR2,
 p_bigu_tab VARCHAR2,
 p_face_feature_tab VARCHAR2,
 p_sm_poly_edges_tab VARCHAR2,
 p_sumlev_fields_tab VARCHAR2,
 p_output_table VARCHAR2 default 'UNIQUE_SMALL_POLYS'
) AS
/*
       Stephanie 20100810
       adds records to the small poly edges table from the bigu (big unique)
       table if possible.
       parameters:
          p_topology: name of the topology
          p_bigu_tab: name of the big unique work table output from
                      'eliminate_small_polygons'
                      (usually <topo_name><smpolyrunid>_bigu)
          p_face_feature_tab: the face feature table name associated with the
                              topology (usually <toponame>_clip_face at this
                              stage)
          p_sm_poly_edges_tab: the output table from eliminate small polygons
                               (sometimes called small_poly_edges)
          p_sumlev_fields_tab: the name of the table with a sum_level
                               and tbl_keys column listing all the keys on the
                               face feature table used to make up each summary
                               level (usually called sumlev_fields)
          p_output_table: the name of the table to store any faces that are
                          important and can not be removed -- These may need to
                          be increased in size!
                          this table should not already exist.  If it does
                          the program will raise an exception.
    */
   --- Step 1:  get a list of face ids from the bigu table that are below
   --- the area threshold.
   --- Step 2: get a list of summary levels from the sumlev fields table
   --- Step 3:  Check each face to see if the summary level values
   ---          represented by that face are also represented by other faces
   ---          that are above the area threshold.
   --- Step 4:
   --- If all of the summary level combinations exist on other faces that are
   --- above the small area threshold, then they are okay to reomve.
   --- choose the longest edge and merge the small face with it's neighbor by
   --- by adding a record to the small poly edges table.
   -- variables make parameters easier to type and capital
   vTopo VARCHAR2(4000) := UPPER(p_topology);
   vBigu VARCHAR2(4000) := UPPER(p_bigu_tab);
   vFaceTable VARCHAR2(4000):= UPPER(p_face_feature_tab);
   vSPETable VARCHAR2(4000) := UPPER(p_sm_poly_edges_tab);
   vSLFields VARCHAR2(4000) := UPPER(p_sumlev_fields_tab);
   -- other variables
   vSql VARCHAR2(4000);
   vFaces GZ_TYPES.STRINGARRAY; -- list of faces to check
   vSumLevs GZ_TYPES.STRINGARRAY; -- list of summary levels to check
   vFields GZ_TYPES.STRINGARRAY; -- list of table keys for the sumary level
   vStatus VARCHAR2(10); -- KEEP or NOT_KEEP to identify the face as
                         -- essential or not.
BEGIN
   dbms_output.put_line('DOES NOTHING');
   -- Step 1
end bigu_review;
PROCEDURE LOGNOTE (
   pLogNote VARCHAR2
   )
   AS PRAGMA AUTONOMOUS_TRANSACTION;
   vUpdateLog VARCHAR2(4000);
   vLogTime TIMESTAMP;
   vLog VARCHAR2(4000);
BEGIN
   vLog := 'GEN_SP_LOG';
   vUpdateLog := 'INSERT INTO '||vLog||' values(:p1,:p2)';
   vLogTime := systimestamp;
   EXECUTE IMMEDIATE vUpdateLog USING vLogTime, pLogNote;
   COMMIT;
END;

------------------------------------------------------------------------------
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- The GENERALIZATION_SP_REMOVAL procedure is the main entry point
-- for GZ_WORKFLOW
------------------------------------------------------------------------------

PROCEDURE  GENERALIZATION_SP_REMOVAL (
    pSchema             IN VARCHAR2,
    pProjectId          IN VARCHAR2,
    pJobId              IN VARCHAR2,
    pTopo               IN VARCHAR2,
    pModules            IN VARCHAR2,
    pFromTopo           IN VARCHAR2,
    pTopoBk             IN VARCHAR2,
    pStateOutlineTable  IN VARCHAR2,
    pStateFP            IN VARCHAR2,
    pGeoIDColumn        IN VARCHAR2 default 'GEOID',
    pRunId              IN VARCHAR2 default '1',
    pCleanup            IN VARCHAR2 default 'N',
    ptopofix_qa         IN VARCHAR2 DEFAULT NULL
) AS

   --Matt! 6/7/13 added ptopofix_qa management
   
   retval     VARCHAR2(8000);
   vSchema    VARCHAR2(32) := UPPER(pSchema);
   vProjectID VARCHAR2(4) := UPPER(pProjectID); -- Z6 or Z8 (to use to look up parameters
   vTopo      VARCHAR2(20) := UPPER(pTopo); -- topology name
   vModules   VARCHAR2(20) := UPPER(pModules);
   vFromTopo  VARCHAR2(20) := UPPER(pFromTopo); -- post-clip topology name
   vStateOutlineTable   VARCHAR2(32) :=  UPPER(pStateOutlineTable);
   vStateFP   VARCHAR2(20) := UPPER(pStateFP);
   vStates    VARCHAR2(32) := pStateOutlineTable;
   vGeoIDColumn VARCHAR2(100) :=  pGeoIDColumn;
   vInScale   NUMBER;
   vArea_Cutoff NUMBER;
   vInAreaTolerance NUMBER;
   vSPEdges   VARCHAR2(32) := vTopo||'_' || pRunId || 'SPE';
   vDEInput   VARCHAR2(32) := vTopo||'_' || pRunId || 'DEI';
   vRunId     VARCHAR2(100) :=  vTopo || '_' || pRunId;
   vCleanup   VARCHAR2(10) := pCleanup;
   vTopoBk    VARCHAR2(20) := UPPER(pTopoBk);
   vParameters GZ_TYPES.SMALL_POLYGON_PARAMETERS_REC;
   vSPModules  GZ_TYPES.stringarray;
   vONCount   NUMBER;
   vINCount   NUMBER;
   -- eliminate small polygons
   vEdgeAtt   VARCHAR2(32); -- := vTopo||'_EDGE_ATT';
   vFaceTable VARCHAR2(32); -- := vTopo||'_CLIP_FACE';
   vClipTable VARCHAR2(32);
   vTestCount NUMBER;
   vEdgeTab   VARCHAR2(32); -- original edge feature table to drop
   vStep      VARCHAR2(4000);
   --vStates  VARCHAR2(4000) := pStateOutlineTable;
   --vStateFP VARCHAR2(2) := pStateFP;
   vLogNote   VARCHAR2(4000);
   --vUpdateLog VARCHAR2(4000);
   --vLog VARCHAR2(4000) := 'GEN_SP_LOG';
   vStepNum   NUMBER;
   vSQL       VARCHAR2(4000);
   vRes       BOOLEAN;
   vOutput    VARCHAR2(10);
   APP_INFO_MODULE VARCHAR2(48);
   APP_INFO_ACTION VARCHAR2(32);
   APP_INFO_CLIENT_INFO VARCHAR2(64);
   vDangleCount NUMBER;
   vtxCount NUMBER;
   --vLogTime timestamp;
   vJobID     VARCHAR2(20) := UPPER(pTopo);
   vProcess   VARCHAR2(100) := 'Small Polygon Removal ' || vProjectID || ' ' || vStateFP;
   vNote      VARCHAR2(4000) := NULL;
   sql1       varchar2(4000);
   rowCount   number;
   rowCount2 number;
   tblName    varchar2(100);
   TABLE_ARR  MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
   smPolyPrmRec GZ_TYPES.SMALL_POLYGON_PARAMETERS_REC;
   vGenClipTab   VARCHAR2(32);
   Success_Flag char(1);
   edgefix_val        VARCHAR2(1) := '0';
   vRelease           VARCHAR2(10);
   -- added for tile validation
   vTileGuess         number;
   topofix_qa           VARCHAR2(1);

BEGIN
   ------------------------------------------------------------------------------
   -- Tag the batch job
   APP_INFO_MODULE := 'GZ_smpoly.rm smpoly-'||vSchema||','||vProjectID||','||vTopo;
   APP_INFO_ACTION  := 'Step 0';
   --MT_UTIL_CONN_ID.set_module(APP_INFO_MODULE, APP_INFO_ACTION);
   --DBMS_APPLICATION_INFO.SET_MODULE(pJobName,pAction);
   DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
   ------------------------------------------------------------------------------
   vSchema := UPPER(pSchema);
   vProjectID := UPPER(pProjectID); -- Z6 or Z8 (to use to look up parameters
   vTopo := UPPER(pTopo); -- topology name
   vModules := UPPER(pModules);
   vFromTopo := UPPER(pFromTopo); -- post-clip topology name
   vStateOutlineTable :=  UPPER(pStateOutlineTable);
   vStateFP := UPPER(pStateFP);
   
   -- Read the small_polygon_parameters table
   --vGeoIDColumn :=  'GEOID';
   vSQL := 'SELECT target_scale, gen_clip_table, release FROM gz_job_setup WHERE jobid =   :1';
   EXECUTE IMMEDIATE vSQL INTO vInScale, vGenClipTab, vRelease  USING  UPPER(pJobId);

   smPolyPrmRec := gz_smpoly.GET_SMPOLY_PROJECT_PARAMETERS(vProjectID, vRelease);
   vArea_Cutoff := smPolyPrmRec.area_threshold;
   vInAreaTolerance := smPolyPrmRec.area_tolerance;

   vFaceTable := vTopo||'_' || smPolyPrmRec.FACE_FEATURE_TABLE;        -- vTopo||'_CLIP_FACE';
   vEdgeAtt := vTopo||'_' || smPolyPrmRec.EDGE_ATT_TABLE;              -- vTopo||'_EDGE_ATT';
   vEdgeTab := vFromTopo||'_' || smPolyPrmRec.CLIP_EDGE_FEATURE_TABLE; -- vTopo||'_EDGE';

   IF vGenClipTab IS NOT NULL
   THEN

      --SOP
      -- Next line updated so we only get the part of the gen_clip_table name after any "."
      vClipTable := vFromTopo||'_' || substr(vGenClipTab,(instr(vGenClipTab,'.')+1));

   ELSE

      --Special attribute clip without clip outline table
      vClipTable := vFromTopo|| '_' || 'ATTRIBUTE';

   END IF;



/*
   2012/01/17 - Sreeni Karpurapu:  Changed the order of operations and added extra checking.
   =================================================================================
   Description of all steps that can be controlled by setting the Modules parameter.
   =================================================================================
   SubStr    Step  Descr
    1          10      Drop Edge Feature Table
    2          20      Copy topology from vFromTopo to vTopo
    3          30      Remove Obsolete Nodes
    4          40      Remove Isolated Nodes
    5          50      Load Initial Edge Attribute Table
    6          60      Identify small polygons
    7          70      Prepare Delete Edges input
    8          80      Backup Topology to vTopoBk
    9          90      Run Delete Edges With Validation
   10         100      Check and Remove Dangles
               105      Check and fix conflict edges
   11         110      Update Measurements for edited faces
                115      Remove Isolated Nodes
   12         120      Check and Remove Obsolete Nodes
   13         130      Call Vertex Thinner
   14         140      Rebuild Edge Attribute Table
   15         150      Validate Topology (Checks relation$ and feature tables)
   16         160      Validate Topology 2 (Oracle -- Level 1)
   17         170     Validate geometry with context (+ Topo Fixer)
   =================================================================================
*/
   ----------------------------------------------------------------------------
   DBMS_APPLICATION_INFO.SET_ACTION('Step 1');
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('SP Work: Begin small poly logging');
   ----------------------------------------------------------------------------
   retval := GZ_SMPOLY.START_SP_LOGGING(vSchema, vTopo);
   DBMS_OUTPUT.PUT_LINE('RetVal: ' || retVal);
   IF retval != '0' THEN
      RAISE_APPLICATION_ERROR(-20001,'Problem with START_SP_LOGGING: ' || retval);
   ELSE
      DBMS_OUTPUT.PUT_LINE('Created Log Table ' || vTopo || '_SP_TRACKING');
   END IF;
   vNote :=  'Input Parameters: '
                        || ' pSchema: ' || pSchema
                        || ' pProjectId: ' || pProjectId
                        || ' pJobId: ' || pJobId
                        || ' pTopo: ' || pTopo
                        || ' pModules: ' || pModules
                        || ' pFromTopo: ' || pFromTopo
                        || ' pTopoBk: ' || pTopoBk
                        || ' pStateOutlineTable: ' || pStateOutlineTable
                        || ' pStateFP: ' || pStateFP
                        || ' pGeoIDColumn: ' || pGeoIDColumn
                        || ' pRunId: ' || pRunId
                        || ' pCleanup: ' || pCleanup
                        || ' ptopofix_qa: ' || ptopofix_qa;
   GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                     p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   dbms_output.put_line (vNote);
   
   
   IF ptopofix_qa IS NULL
   OR ptopofix_qa = 'N'
   THEN
   
      --SOP
      topofix_qa := 'N';
      
   ELSE
   
      --dont plan to fail on topofix fails for SP any more
      topofix_qa := 'Y';
   
   END IF;
   
   -- retreive parameters
   -- EXECUTE IMMEDIATE 'SELECT * '||
   --                  'FROM '||vSchema||'.GEN_SP_PARAMETERS '||
   --                  'WHERE gen_project_id := p1'
   --       INTO vParameters
   --        USING vProjectID;
   --IF length(vParameters.sp_modules) != 14  -- guessing at number I will need.
   --THEN
   --  RAISE_APPLICATION_ERROR(-20001,'Wrong number of modules in sp_modules: '
   --                         ||vParameters.sp_modules);
   --END IF;
   -- Validate Parameters
   -- Check if vFromTopo exists
   sql1 :=  'Select count(*) from user_sdo_topo_info where topology = :1';
   execute immediate sql1 into rowCount using vFromTopo;
   if rowCount = 0
   then
      vNote := 'Topology ' || vFromTopo || ' does not exist.  Terminating execution';
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);
      dbms_output.put_line ('ERROR - ' || vNote);
      RAISE_APPLICATION_ERROR(-20001, vNote);
   end if;
   -- Check if vTopo exists
   sql1 :=  'Select count(*) from user_sdo_topo_info where topology = :1';
   execute immediate sql1 into rowCount using vTopo;
   if rowCount > 0
   then
      vNote := 'Topology ' || vTopo || ' exists.  This will be purged if module flag step 20 is set to Y';
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      dbms_output.put_line (vNote);
      --RAISE_APPLICATION_ERROR(-20001, vNote);
   end if;
   -- Check if vTopobk exists
   sql1 :=  'Select count(*) from user_sdo_topo_info where topology = :1';
   execute immediate sql1 into rowCount using vTopoBk;
   if rowCount > 0
   then
      vNote := 'Topology ' || vTopoBk || ' exists.  This will be purged if module flag step 80 is set to Y';
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      dbms_output.put_line (vNote);
      -- RAISE_APPLICATION_ERROR(-20001, vNote);
   end if;
   --vStateOutlineTable table should exist
   -- How come?  It doesn't get used in this module
   -- Does it get used by any procedures we call?
   -- Nope.
   -- Removed this check 1/4/2013 -- Stephanie
   /* 
   sql1 := 'Select count(*) from user_tables where table_name = :1';
   execute immediate sql1 into rowCount using vStateOutlineTable;
   if rowCount = 0
   then
      vNote := 'State Outline Table ' || vStateOutlineTable || ' does not exist.  Terminating execution';
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      dbms_output.put_line ('ERROR - ' || vNote);
      RAISE_APPLICATION_ERROR(-20001, vNote);
   end if;
   */

   -- Check run ID vTopo || '_' || vRunId
   --if vCleanUp = 'N' then  *** vCleanUp = 'Y' doesnt seem to be cleaning up temp tables.
   tblName := vRunId || '%';
   dbms_output.put_line ('vRunID tblName: ' || tblName);
   sql1 := 'Select count(*) from user_tables where table_name like :1';
   execute immediate sql1 into rowCount using tblName;
   if rowCount > 0
   then
      vNote := 'Found tables ' || tblName || ' for Run ID:  ' || vRunID || '. Drop them';
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      -- Add code to purge tables
      sql1 := 'Select TABLE_NAME as table_name From user_tables ';
      sql1 := sql1 || ' where table_name like :1';
      dbms_output.put_line(sql1 || tblname);
      execute immediate sql1 bulk collect into table_arr using tblname;
      for i in table_arr.first..table_arr.last loop
         sql1 := 'Drop table ' || vschema || '.' || table_arr(i) || ' PURGE';
         dbms_output.put_line(sql1);
         execute immediate sql1;
      END LOOP;
   end if;
   --end if;

   -- We don't want any FSL tables in the Topology.
   SQL1 :=   'Select count(*) from user_sdo_topo_info '
                  || ' where topology = :1 '
                  || ' and table_name like :2';

    Execute immediate SQL1 into rowCount using vFromTopo, vFromTopo || '%FSL%';

    If rowCount > 0 Then
       vNote := 'Found ' || rowCount || ' FSL tables in ' || vFromTopo || '.  Calling gz_topo_helper.deregister_fsl.';
       dbms_output.put_line(vNote);
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

       GZ_TOPO_HELPER.DEREGISTER_FSL(vFromTopo, 'FSL', vSchema);

       vNote := 'Done with gz_topo_helper.deregister_fsl.';
       dbms_output.put_line(vNote);
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

    Else
       vNote := 'Did not find any FSL tables in ' || vFromTopo || ', we are good to go';
       dbms_output.put_line(vNote);
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

    End If;
----------------------------------------------------------------------------------
   --vUpdateLog := 'INSERT INTO '||vLog||' values(:p1,:p2)';
   vSPModules := GZ_UTILITIES.split(vModules);
   IF vSPModules.COUNT <> 17
   THEN
      vNote := 'ERROR - wrong number of modules to run.  Expecting 17 modules.';
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      dbms_output.put_line (vNote);
      RAISE_APPLICATION_ERROR(-20001,'Wrong number of modules (SPModules) passed');
   END IF;
   -- start logging
   -- loop through the yesses and No's and do things...
   -- drop edge table
   vStepNum := 10;
   vStep := 'Drop Edge Feature Table for '||vFromTopo;
   If vSPModules(1) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Drop Edge Table');
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
      vNote := 'Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      --vEdgeTab := vFromTopo||'_EDGE';
      DBMS_OUTPUT.PUT_LINE('About to delete feature table ' || vEdgeTab);
      sql1 := 'select  count(*) from user_sdo_topo_info where TABLE_NAME = :1 and Topology = :2';
      execute immediate sql1 into rowCount using vEdgeTab, vFromTopo;
      DBMS_OUTPUT.PUT_LINE('RowCount: ' || rowCount);
      If rowCount > 0
      then
         SDO_TOPO.DELETE_TOPO_GEOMETRY_LAYER(vFromTopo,vEdgeTab,'TOPOGEOM');
         -- delete the old edge table
         EXECUTE IMMEDIATE 'truncate table '||vEdgeTab;
         vsql := 'drop table '||vEdgeTab||' cascade constraints purge';
         EXECUTE IMMEDIATE vsql;
         vNote := 'Finished ' || vStep || ' . Dropped edge feature table ' || vEdgeTab;
      else
         dbms_output.put_line('Did not find topo geom layer ' ||vEdgeTab);
         vNote :=  '*** Edge feature table ' || vEdgeTab || ' does not exist. Finished ' || vStep ||'***';
      end if;
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;
   -- Copy Topology nd Validate Feature Tables
   vStepNum := 20;
   vStep := 'Copy '||vFromTopo||' to '||vTopo;

   If vSPModules(2) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Copy Topology and Validate Feature Tables');
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
      vNote := 'Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      -- 01/04/2012 Turned Oracle validation OFF on initial copy -- Stephanie
      GZ_TOPO_UTIL.copy_topology(vSchema,vFromTopo,vSchema,vTopo,'Y','N');

      vRes := GZ_TOPO_UTIL.validate_feature_tables(vSchema,vTopo);
      IF vRes
      THEN
         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - All Feature Tables are valid in '|| vTopo);
         vNote := 'All Feature Tables are valid in '|| vTopo;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      ELSE
         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Some Feature Tables in '|| vTopo||' are NOT VALID.');
          vNote := 'Some Feature Tables in '|| vTopo||' are NOT VALID.';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         dbms_output.put_line ('ERROR - ' || 'Some Feature Tables in '||
                             vTopo||' are NOT VALID.');
         RAISE_APPLICATION_ERROR(-20001,'ERROR - ' || 'Some Feature Tables in '||
                             vTopo||' are NOT VALID.');
      END IF;
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
       vNote := 'Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
       vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;
   -- Remove Obsolete Nodes
   vStepNum := 30;
   vStep := 'Remove Obsolete Nodes From '||vTopo;
   If vSPModules(3) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Remove Obsolete Nodes');
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      EXECUTE IMMEDIATE 'SELECT GZ_UTILITIES.COUNT_OBSOLETE_NODES('''||
                          vTopo||''') FROM DUAL' INTO vONCount;
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Obsolete Node Count = '||vONCount||' in '||vTopo);
      vNote := 'Step '||vStepNum||' - Obsolete Node Count = '||vONCount||' in '||vTopo;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      -- (drop)
      IF vONCount > 0
      THEN

         vONCount := GZ_UTILITIES.GZ_REMOVE_OBSOLETE_NODES(vTopo);

      END IF;
      --
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;
   -- Remove Isolated Nodes
   vStepNum := 40;
   vStep := 'Remove Isolated Nodes From '||vTopo;
   If vSPModules(4) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Remove Isolated Nodes');
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      EXECUTE IMMEDIATE 'SELECT count(node_id) FROM '||
                       vTopo||'_node$ where edge_id = 0 or edge_id IS NULL' INTO vINcount;
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Isolated Node Count = '||vINCount||' in '||vTopo);
      vNote := 'Step '||vStepNum||' - Isolated Node Count = '||vINCount||' in '||vTopo;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      IF vINCount > 0
      THEN
          GZ_UTILITIES.remove_isolated_nodes(vTopo);
      END IF;
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;
   -- Load Initial Edge Attribute Table
   vStepNum := 50;
   vStep := 'Load Initial Edge Attribute Table for '||vTopo;
   If vSPModules(5) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Load Initial Edge Attribute Table');
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      GZ_SMPOLY.LOAD_EDGE_SIMPLE(vSchema, vTopo);
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;
   -- Identify small polygons
   vStepNum := 60;
   vStep := 'Load identify small polygons in '||vTopo;
   If vSPModules(6) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Identify Small Polygons');
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      -- hard coding z6 for today...
      DBMS_OUTPUT.PUT_LINE('vRunID: ' || vRunId || ' vCleanup: ' || vCleanup);
      vNote := 'Calling ELIMINATE_SMALL_POLYS with: ' ||
                             ' pInSchema: ' || vSchema ||
                             ' pInMT_EDGE$: ' ||vTopo ||
                             ' pInTable: ' || vFaceTable ||
                             ' pEdgeTable: ' ||vEdgeAtt ||
                             ' pInGeoIDColumn: ' ||vGeoIDColumn ||
                             ' pOutTable: ' ||vSPEdges ||
                             ' InScale: ' ||vInScale ||
                             ' Area_Cutoff: ' ||vArea_Cutoff ||
                             ' InAreaTolerance: ' ||vInAreaTolerance ||
                             ' pRunId: ' || vRunId ||
                             ' pCleanup: '  || vCleanup;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      dbms_output.put_line(vNote);
      ELIMINATE_SMALL_POLYS (vSchema, vTopo, vFaceTable, vEdgeAtt, vGeoIDColumn, vSPEdges, vInScale, vArea_Cutoff, vInAreaTolerance, vRunId, vCleanup);
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;
   -- Prepare DelEdges Input
   vStepNum := 70;
   vStep := 'Prepare Delete Edges input for '||vTopo;
   If vSPModules(7) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Prepare DelEdges Input');
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      EXECUTE IMMEDIATE 'create table '||vDEInput||' as select * from '||vSPEdges;
      EXECUTE IMMEDIATE 'alter table '||vDEInput||' add (done varchar2(100), sqlstmt varchar2(4000), error_msg varchar2(4000))';
      EXECUTE IMMEDIATE 'update '||vDEInput||' set left_face_id = NULL, right_face_id = NULL';

      GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',vDEInput);

      COMMIT;
      -- Check table...
      -- does it match edge$?
      vTestCount := 0;
      EXECUTE IMMEDIATE 'SELECT count(*) '||
                          'FROM  '||vDEInput|| ' a, '||vTopo||'_edge$ b '||
                'WHERE a.keep_face NOT IN (b.left_face_id,b.right_face_id) '||
                'AND a.edge_id = b.edge_id'
                INTO vTestCount;
      IF vTestCount > 0
      THEN
         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - ERROR: '|| 'Problem with Delete Edges Input Table. Keep Faces do not match edge$');
         vNote := 'Step '||vStepNum||' - ERROR: '|| 'Problem with Delete Edges Input Table. Keep Faces do not match edge$';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         dbms_output.put_line ('ERROR - ' || 'Problem with Delete Edges Input Table. '||
                             'Keep Faces do not match edge$');
         RAISE_APPLICATION_ERROR(-20001,'ERROR - ' || 'Problem with Delete Edges Input Table. '||
                             'Keep Faces do not match edge$');
      END IF;
      vTestCount := 0;
      EXECUTE IMMEDIATE 'SELECT count(*) '||
                          'FROM  '||vDEInput|| ' a, '||vTopo||'_edge$ b '||
                'WHERE a.NOT_KEEP NOT IN (b.left_face_id,b.right_face_id) '||
                'AND a.edge_id = b.edge_id'
                INTO vTestCount;
      IF vTestCount > 0
      THEN
         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - ERROR: Problem with Delete Edges Input Table. Not Keep Faces do not match edge$');
         vNote := 'Step '||vStepNum||' - ERROR: Problem with Delete Edges Input Table. Not Keep Faces do not match edge$';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         dbms_output.put_line ('ERROR - ' || 'Problem with Delete Edges Input Table. '||
                               'Not Keep Faces do not match edge$');
         RAISE_APPLICATION_ERROR(-20001,'ERROR - ' || 'Problem with Delete Edges Input Table. '||
                                        'Not Keep Faces do not match edge$');
      END IF;
      -- do any edges bound the universal face?
      vTestCount := 0;
      EXECUTE IMMEDIATE 'SELECT count(*) '||
                        'FROM  '||vDEInput|| ' a, '||vTopo||'_edge$ b '||
                'WHERE b.left_face_id = -1 '||
                'AND a.edge_id = b.edge_id'
                INTO vTestCount;
      IF vTestCount > 0
      THEN
         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - ERROR: Problem with Delete Edges Input Table. An edge marked for deletion bounds the universal face.');
         vNote := 'Step '||vStepNum||' - ERROR: Problem with Delete Edges Input Table. An edge marked for deletion bounds the universal face.';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         dbms_output.put_line ('ERROR - ' || 'Problem with Delete Edges Input Table. '||
                               'An edge marked for deletion bounds the universal face.');
         RAISE_APPLICATION_ERROR(-20001,'ERROR - ' || 'Problem with Delete Edges Input Table. '||
                                        'An edge marked for deletion bounds the universal face.');
      END IF;
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;
   -- Backup Topology
   vStepNum := 80;
   vStep := 'Backup Topology '||vTopo||' to '||vTopoBk;
   If vSPModules(8) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Backup Topology');
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      -- 01/04/2012 Turned Oracle validation OFF on backup copy 
      -- (we always skip this step anyway)-- Stephanie

      GZ_TOPO_UTIL.copy_topology(vSchema,vTopo,vSchema,vTopoBk,'Y','N');

      vRes := GZ_TOPO_UTIL.validate_feature_tables(vSchema,vTopoBk);
      IF vRes
      THEN
         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - All Feature Tables are valid in '||vTopoBk);
         vNote := 'Step '||vStepNum||' - All Feature Tables are valid in '||vTopoBk;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      ELSE
         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Some Feature Tables in '|| vTopoBk||' are NOT VALID.');
         vNote := 'Step '||vStepNum||' - Some Feature Tables in '|| vTopoBk||' are NOT VALID.';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         dbms_output.put_line ('ERROR - ' || 'Some Feature Tables in '||
                                vTopo||' are NOT VALID.');
         RAISE_APPLICATION_ERROR(-20001,'ERROR - ' || 'Some Feature Tables in '||
                                  vTopo||' are NOT VALID.');
      END IF;
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;
   -- Run Delete Edges With Validation
   vStepNum := 90;
   vStep := 'RUN Delete Edges for '||vTopo;
   If vSPModules(9) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Delete Edges');
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      sql1 := 'Select Count(*) from ' || vDEInput;
      execute immediate sql1 into rowCount;
      If rowCount > 0 Then
         vNote := 'Calling gz_topo_edit.delete_edges_tpmp with vTopo: ' || vTopo || ', vFaceTable: ' || vFaceTable || ', vDEInput: ' || vDEInput || ', valFlag: ' || 'TRUE';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         GZ_TOPO_EDIT.delete_edges_tpmp(vTopo,vFaceTable,vDEInput,'TRUE');

         sql1 := 'Select Count(*) from ' || vDEInput || ' Where done <> :1';
         execute immediate sql1 into rowCount2 using 'Y';
         If rowCount2 > 0 Then
            vNote := rowCount2 || ' out of ' || rowCount  ||'( ' || rowcount/rowcount2*100|| '%) small polygons did not get removed ';
            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                              p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

            Success_Flag := 'N';
         else
            Success_Flag := 'Y';
            vNote := 'Processed all small polygons successfully';
            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                              p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         end if;

         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
         vNote := 'Step '||vStepNum||' - Finished '||vStep;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      else
         Success_Flag := 'Y';
         vNote := 'Skipping ' || vStepNum || '.  Did not find any records in Delete Edges Input table ' || vDEInput;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      end if;

      vStepNum := 100;
      vStep := 'Check for dangles and remove if found in '||vTopo;
      If vSPModules(10) = 'Y'
      THEN
         DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Check and Remove Dangles');
         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
         vNote := 'Step '||vStepNum||' - Begin '||vStep;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         EXECUTE IMMEDIATE 'SELECT count(*) '||
                                          'FROM '||vTopo||'_edge$ where left_face_id = right_face_id'
         INTO vDangleCount;

         vNote := 'Step '||vStepNum||' Dangle Count before gz_smpoly.drop_dangles = '||vDangleCount;
         dbms_output.put_line(vNote);
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         IF vDangleCount > 0
         THEN
            GZ_SMPOLY.drop_dangles(vTopo);
            -- Check if all dangles are removed
            EXECUTE IMMEDIATE 'SELECT count(*) '||
                                             'FROM '||vTopo||'_edge$ where left_face_id = right_face_id'
            INTO vDangleCount;

            vNote := 'Step '||vStepNum||' Dangle Count after gz_smpoly.drop_dangles = '||vDangleCount;
            dbms_output.put_line(vNote);
            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                              p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

            IF vDangleCount > 0
            THEN
               Success_Flag := 'N';
            END IF;

         ELSE
            dbms_output.put_line('No dangles found (seems odd - you might want to double check.)');
         END IF;

         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
         vNote := 'Step '||vStepNum||' - Finished '||vStep;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      ELSE
         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
         vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      END IF;

      ------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      -- Check and Remove Isolated Nodes
      vStepNum := 115;
      vStep := 'Remove Isolated Nodes From '||vTopo || ' after delete edges';
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Check and Remove Isolated Nodes');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ------------------------------------------------------------------------------------

      If vSPModules(11) = 'Y'
      THEN

          --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
          vNote := 'Step '||vStepNum||' - Begin '||vStep;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

          EXECUTE IMMEDIATE 'SELECT count(node_id) FROM '||
                       vTopo||'_node$ where edge_id = 0 or edge_id IS NULL' INTO vINcount;
          --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Isolated Node Count = '||vINCount||' in '||vTopo);
          vNote := 'Step '||vStepNum||' - Isolated Node Count before gz_utilities.remove_isolated_nodes = '||vINCount||' in '||vTopo;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

          IF vINCount > 0
          THEN
             GZ_UTILITIES.remove_isolated_nodes(vTopo);

             -- Check if all Isolated nodes are removed.
             EXECUTE IMMEDIATE 'SELECT count(node_id) FROM '||
                          vTopo||'_node$ where edge_id = 0 or edge_id IS NULL' INTO vINcount;
             --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Isolated Node Count = '||vINCount||' in '||vTopo);
             vNote := 'Step '||vStepNum||' - Isolated Node Count after gz_utilities.remove_isolated_nodes = '||vINCount||' in '||vTopo;
            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                              p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

             IF vINCount > 0
             THEN
                Success_Flag := 'N';
             END IF;

          END IF;

          --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
          vNote := 'Step '||vStepNum||' - Finished '||vStep;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      ELSE
         --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
         vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      END IF;

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -- Check and Remove Obsolete Nodes
   vStepNum := 120;
   vStep := 'Check for obsolete nodes and remove if found in '||vTopo;
   DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Check and Remove Obsolete Nodes');
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   If vSPModules(11) = 'Y'  --REPEAT! Turn it up to 11 twice
   THEN

      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      --
      EXECUTE IMMEDIATE 'SELECT gz_utilities.COUNT_OBSOLETE_NODES('''||
                         vTopo||''') FROM DUAL' INTO vONCount;

      vNote := 'Step '||vStepNum||' Obsolete Node Count before gz_utilities.GZ_REMOVE_OBSOLETE_NODES = '||vONcount;
      dbms_output.put_line(vNote);
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      -- (drop)
      IF vONCount > 0
      THEN

         vONCount := GZ_UTILITIES.GZ_REMOVE_OBSOLETE_NODES(vTopo);

         -- DO NOT Check if all Obsolete Nodes are removed, new utility will return the number

         IF vONCount > 0
         THEN

            --Try again like fools
            --This may never happen, or it may happen a lot
            --just adding it untested on a wild hunch. Shouldnt hurt
            vONCount := GZ_UTILITIES.GZ_REMOVE_OBSOLETE_NODES(vTopo);

         END IF;

         vNote := 'Step '||vStepNum||' Obsolete Node Count after gz_utilities.GZ_REMOVE_OBSOLETE_NODES = '||vONcount;
         dbms_output.put_line(vNote);
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         IF vONCount > 0
         THEN

            Success_Flag := 'N';

         END IF;

      END IF;

      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -- GZ_FIX_EDGE
   vStepNum := 105;
   vStep := 'Check and fix conflicting edges '||vTopo;
   DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Call GZ_FIX_EDGE');
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   If vSPModules(12) = 'Y'
   THEN

      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      --
      edgefix_val := GZ_TOPOFIX.GZ_FIX_EDGE (vJobID ,vTopo, 'SP', 'Y', 0.05, NULL, 'Y');

      IF edgefix_val = '0' 
      THEN
      
         --Fix Edge success
         vNote := 'Step '||vStepNum||' - Finished Successful '||vStep;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);
                           
      ELSIF edgefix_val <> '0'
      AND topofix_qa = 'N'
      THEN
      
         --Fix Edge not successful, but we dont want to fail SP for this reason
         vNote := 'Step '||vStepNum||' - Finished and failed '||vStep || '. But we wont fail SP for this';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);
         

      ELSIF edgefix_val <> '0' 
      AND topofix_qa = 'Y'
      THEN

         --Fail the module for a fix_edge failure.  We dont plan to do this for SP any more
         vNote := 'Step '||vStepNum||' - Fail '||vStep;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         Success_Flag :='N';
         
      ELSE
      
         vNote := 'Step '||vStepNum||' - Unknown edgefix fail '||vStep;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         Success_Flag :='N';
          RAISE_APPLICATION_ERROR(-20001, vNote);
          
      END IF;

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -- GZ_FIX_EDGE
   vStepNum := 110;
   vStep := 'Update measurements for edited faces in '||vTopo;
   DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Update Measurements for edited faces');
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   -- Update Measurements for edited faces

   If vSPModules(12) = 'Y'
   THEN

      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Begin '||vStep);
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      --
      GZ_SMPOLY.UPDATE_MEASURES_WHERE_NULL(vFaceTable,'FACE_ID');
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - Finished '||vStep);
      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      --GZ_SMPOLY.LOGNOTE('Step '||vStepNum||' - SKIPPING '||vStep);
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;




   vStepNum:=130;
   vStep := 'Call vertex thinner for '||vTopo;
   If vSPModules(13) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Calling vertex thinner');
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      vtxCount := GZ_UTILITIES.SP_VERTEX_THINNER(vTopo, vClipTable, vFaceTable);

      vNote := 'Step '||vStepNum||' removed vertex count: ' || vtxCount;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;

   -- Rebuild Edge Attribute Table
   vStepNum := 140;
   vStep := 'Rebuild the Edge Attribute Table for '||vTopo;
   If vSPModules(14) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Rebuild Edge Attribute Table');
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      EXECUTE IMMEDIATE 'DROP TABLE '||vEdgeAtt||' CASCADE CONSTRAINTS PURGE';
      GZ_SMPOLY.LOAD_EDGE_SIMPLE(vSchema,vTopo);
      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;
   -- Validate Topology
   vStepNum := 150;
   vStep := 'Validate Topology '||vTopo;
   If vSPModules(15) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Validate Topology');
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      vRes := gz_topo_util.validate_feature_tables(vSchema,vTopo);
      IF vRes
      THEN
         vNote := 'Step '||vStepNum||': Final SP Check: All Feature Tables are valid in '||vTopo;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         dbms_output.put_line(vNote);
      ELSE
         vNote :=  'Step '||vStepNum||': Final SP Check: Some Feature Table records are not valid in '||vTopo;
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         dbms_output.put_line(vLogNote);
         RAISE_APPLICATION_ERROR(-20001, vNote);
      END IF;
      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE
      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;
   -- Validate Topology 2
   vStepNum := 160;
   vStep := 'Validate Topology '||vTopo;
   If vSPModules(16) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Validate Topology 2');
      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);


      Begin

        rowCount := 0;
        sql1 := 'Select count(*) From ' || vTopo || '_EDGE$';
        Execute Immediate sql1 into rowCount;

        retVal := 'FALSE';
        --retVal := SDO_TOPO_MAP.VALIDATE_TOPOLOGY( vTopo, 'TRUE', 1);
        --retVal := GZ_UTILITIES.VALIDATE_TOPOLOGY(vtopo,rowCount);
        -- 01/04/2013
        -- Updated to tile-based validator to avoid memory issues
        -- after getting help from Matt.  We're totally guessing on the 
        -- tile count, but if any tile count we enter hits a memory error
        -- the validation utility will subdivide just that bad tile.
        -- Stephanie
        
        IF rowCount > 500000
        THEN    -- we'll guess this is a large topology
                -- that will need to
                -- be divided a lot for validation
           vTileGuess := 20;
        ELSIF RowCount > 10000
        THEN    -- we'll guess this a medium sized topology
           vTileGuess := 10;
        ELSE    -- don't bother dividing this probably smallish guy
           vTileGuess := 1;
        END IF;

        retVal := GZ_UTILITIES.GZ_VALIDATE_TOPOLOGY(vTopo,vFaceTable,'SDOGEOMETRY', p_log_type => 'SP', p_tile_target => vTileGuess);

        IF retVal = 'TRUE' THEN
           vNote := 'Oracle Topology Validation - PASSED';
            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                              p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

        ELSE
           vNote := 'Oracle Topology Validation - FAILED';
            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                              p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

           RAISE_APPLICATION_ERROR(-20001, vNote);
        END IF;


         vNote := 'Step '||vStepNum || ' Successfully validated topology';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         dbms_output.put_line(vNote);
      Exception
         When Others Then
            retVal := 'Validation Failed due to ' || SQLCODE || '-' || SQLERRM;
            vNote := retVal;
            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                              p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

           dbms_output.put_line(vNote);
           RAISE_APPLICATION_ERROR(-20001, vNote);
      End;

      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE

      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;

   -- Validate geometry with context (+topo fixer)
   vStepNum := 170;
   vStep := 'FIX_FACE and Val geom with context '||vTopo;
   If vSPModules(17) = 'Y'
   THEN
      DBMS_APPLICATION_INFO.SET_ACTION(vStepNum);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Validate Geom with context');

      vNote := 'Step '||vStepNum||' - Begin '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      
      --aka FIX_FACE
      vOutput := GZ_TOPOFIX.check_face_tab(pJobId, vTopo,vFaceTable,'FACE_ID', 'SP');
      
      If (vOutput = '0' ) 
      THEN
      
         vNote := ' FIX_FACE and Val geom with context -- successful';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

      ELSIF vOutput <> '0'
      AND ptopofix_qa = 'N'
      THEN
      
         --dbms_output.put_line('FALSE'); --whats this for?
         vNote := ' FIX_FACE and Val geom with context -- Failed but we wont fail SP for it';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         
      ELSIF vOutput <> '0'
      AND ptopofix_qa = 'Y'
      THEN
      
         --This is not the usual plan.  Fix face failures dont fail SP
         --dbms_output.put_line('FALSE'); --whats this for?
         vNote := ' FIX_FACE and Val geom with context -- Failed and we will fail SP for it';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                           p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

         Success_Flag := 'N';

      END IF;

      vNote := 'Step '||vStepNum||' - Finished '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   ELSE

      vNote := 'Step '||vStepNum||' - SKIPPING '||vStep;
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,
                        p_step=>vStepNum, p_sqlstmt=>vStep, p_error_msg=>vNote);

   END IF;

   If (Success_Flag = 'Y') then
       vNote := 'Complete - Successful';
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess, p_error_msg=>vNote);

   else
       vNote := 'Complete, but failed processing. Please check the tracking and DEI tables for more information.';
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,  p_error_msg=>vNote);

       RAISE_APPLICATION_ERROR(-20001, vNote);
   end if;

   ------------------------------
   -- Clear the session tag
   --MT_UTIL_CONN_ID.clear_module;
   DBMS_APPLICATION_INFO.SET_MODULE(NULL,NULL);
   DBMS_APPLICATION_INFO.SET_CLIENT_INFO(NULL);
   ------------------------------

EXCEPTION
   WHEN OTHERS THEN
      dbms_output.put_line('******* When-Others Error *******');
      dbms_output.put_line(SQLERRM);
      vNote := 'SQL Error Code: ' || SQLCODE || CHR(10)
            || ' SQLERRM: ' || SQLERRM || CHR(10)
            || ' Backtrace: ' || DBMS_UTILITY.format_error_backtrace;

      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG('SP', vTopo, vProcess,  p_error_msg=>vNote);
      ------------------------------
      -- Clear the session tag
      --MT_UTIL_CONN_ID.clear_module;
      DBMS_APPLICATION_INFO.SET_MODULE(NULL,NULL);
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO(NULL);
      ------------------------------
      RAISE_APPLICATION_ERROR(-20001, vNote);
END GENERALIZATION_SP_REMOVAL;

------------------------------------------------------------------------------
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
------------------------------------------------------------------------------


PROCEDURE LOAD_EDGE_SIMPLE(
pSchema VARCHAR2,
pTopology VARCHAR2
) AS
 /*
   Stephanie
   creates the edge attribute table with edge_ids and edge lengths
   gathered from the <topo>_edge$ table.
   Same as load edge from topo_build2, but it doesn't go looking for
   info from topo_universe and it will build a table from
   scratch (delete a table if it already exists first)
      pTopology - the name of the topology
      pEdgeAttTable - the name of the edge attribut table to populate
 */
vTopo VARCHAR2(20) := UPPER(pTopology);
vEdgeAtt VARCHAR2(32);
vEdgeDollar VARCHAR2(32);
vIndexName VARCHAR2(100);
sql_stmt VARCHAR2(4000);
BEGIN
vEdgeDollar := vTopo||'_EDGE$';
vEdgeAtt := vTopo||'_EDGE_ATT';
vIndexName := vEdgeAtt||'_EDGE_ID_UK';
 dbms_output.put_line('-----');
 dbms_output.put_line('Start Load Edge Simple, '||vTopo);
 dbms_output.put_line('-----');
-- create table...
GZ_SMPOLY.CREATE_EDGE_ATTRIBUTE(pSchema,vEdgeAtt);
-- load edges
 sql_stmt := 'insert into ' || vEdgeAtt || ' (EDGE_ID) '||
             'select EDGE_ID from '||vEdgeDollar;
 EXECUTE immediate sql_stmt;
 COMMIT;
 -- create index on edge_id
 sql_stmt := 'create unique index ' ||vIndexName||' on '||vEdgeAtt||
             '(EDGE_ID) noparallel nologging';
 EXECUTE immediate sql_stmt;
 -- load edgelength
 sql_stmt := 'update ' || vEdgeAtt || ' a '||
             'set a.edgelen = (select sdo_geom.sdo_length(b.geometry,0.05) '||
             'from ' ||vEdgeDollar || ' b where a.edge_id = b.edge_id)';
 EXECUTE immediate sql_stmt;
 COMMIT;
 -- load left face id
 sql_stmt := 'update ' || vEdgeAtt || ' a '||
             'set a.left_face_id = (select b.left_face_id '||
             'from ' ||vEdgeDollar || ' b where a.edge_id = b.edge_id)';
 EXECUTE immediate sql_stmt;
 COMMIT;
 -- load right face id
 sql_stmt := 'update ' || vEdgeAtt || ' a '||
             'set a.right_face_id = (select b.right_face_id '||
             'from ' ||vEdgeDollar || ' b where a.edge_id = b.edge_id)';
 EXECUTE immediate sql_stmt;
 COMMIT;
 dbms_output.put_line('-----');
 dbms_output.put_line('End Load Edge Simple, '||vTopo||', see new table, '||vEdgeAtt);
 dbms_output.put_line('-----');
 --- Add Error Capture
END load_edge_simple;

------------------------------------------------------------------------------
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
------------------------------------------------------------------------------

PROCEDURE UPDATE_MEASURES_WHERE_NULL (
   pTable         IN VARCHAR2,
   pPrimaryKey    IN VARCHAR2,
   pparallel      IN NUMBER DEFAULT 4
) AS
/*
  Purpose...
  Update all FACE measurements in the FACE FEATURE table after the delete
  edges step.
  Columns below must exist in the table...
     SDOGEOMETRY
     MBR
     areatotal
     PERIMETER
     LLX
     LLY
     URX
     URY
  given a table with an SDOGEOMETRY field and a valid TOPOGEOM field
  updates ALL the measurements listed above based on the topogeom field
  for all records
  where the SDOGEOMETRY FIELD IS NULL
  Table must exist in the schema you are logged into.
  pPrimary key (usually face_id) is used to select the records that are
  null - it must be a unique identifier column in pTable
  Author: Stephanie
*/
vPK VARCHAR2(4000) := UPPER(pPrimaryKey);
vSql VARCHAR2(4000);
vRecords GZ_TYPES.STRINGARRAY;
vSql_mbr VARCHAR2(4000);
vSql_sdo VARCHAR2(4000);
vSql_rest VARCHAR2(4000);
vCode VARCHAR2(4000);
vError VARCHAR2(4000);
vErrorCounter NUMBER := 0;
BEGIN
vSql := 'SELECT '||vPK||' FROM '||pTable||' WHERE SDOGEOMETRY IS NULL';
BEGIN
    EXECUTE IMMEDIATE vSql BULK COLLECT INTO vRecords;
EXCEPTION
   WHEN NO_DATA_FOUND THEN
     dbms_output.put_line('*******************************************************************');
     dbms_output.put_line('No records in '||pTable||'appear to have NULL sdo geometries');
     dbms_output.put_line('WARNING:  No records updated.');
     dbms_output.put_line('*******************************************************************');
   WHEN OTHERS THEN
     vCode := SQLCODE;
     vError := SQLERRM;
     dbms_output.put_line('*******************************************************************');
     dbms_output.put_line('ERROR!');
     dbms_output.put_line('SQL = '''||vCode||'''');
     dbms_output.put_line('Error Message = '''||vError||'''');
     dbms_output.put_line('*******************************************************************');
     RAISE;
END;
vSql_sdo := 'update /*+ PARALLEL ' || pparallel || ' */ ' || pTable ||
        ' a set a.sdogeometry = a.topogeom.get_geometry() '||
        ' where '||vPk||' = :p1';
vSql_mbr := 'update /*+ PARALLEL ' || pparallel || ' */ ' || pTable ||
        ' a set MBR = SDO_GEOM.SDO_MBR(SDOGEOMETRY) '||
        ' where '||vPk||' = :p1';
vSql_rest := 'update /*+ PARALLEL ' || pparallel || ' */ ' || pTable ||
        ' a set areatotal = sdo_geom.sdo_area(sdogeometry,0.05,''unit=sq_meter''),'
             ||'PERIMETER = SDO_GEOM.SDO_LENGTH(SDOGEOMETRY,0.05,''unit=meter''),'
             ||'LLX = SDO_GEOM.SDO_MIN_MBR_ORDINATE(MBR,1),'
             ||'LLY = SDO_GEOM.SDO_MIN_MBR_ORDINATE(MBR,2),'
             ||'URX = SDO_GEOM.SDO_MAX_MBR_ORDINATE(MBR,1),'
             ||'URY = SDO_GEOM.SDO_MAX_MBR_ORDINATE(MBR,2) '||
        ' where '||vPk||' = :p1';
FOR i in 1..vRecords.COUNT LOOP
    BEGIN
        -- update sdogeometry field from topogeom
        EXECUTE immediate vsql_sdo USING vRecords(i);
        COMMIT;
        -- update sdogeometry field from topogeom
        EXECUTE immediate vsql_mbr USING vRecords(i);
        COMMIT;
        -- update sdogeometry field from topogeom
        EXECUTE immediate vsql_rest USING vRecords(i);
        COMMIT;
    EXCEPTION
    WHEN OTHERS THEN
        vCode := SQLCODE;
        vError := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
        vErrorCounter := vErrorCounter + 1;
        dbms_output.put_line('Could not update measurements for face '||vRecords(i));
        dbms_output.put_line('SQL = '''||vCode||'''');
        dbms_output.put_line('Error Message = '''||vError||'''');
    END;
END LOOP;
dbms_output.put_line('****************************************************');
dbms_output.put_line('Completed Updating Measurments for '||pTable);
dbms_output.put_line('There were '||vErrorCounter|| ' errors.');
dbms_output.put_line('****************************************************');
END UPDATE_MEASURES_WHERE_NULL;
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
PROCEDURE find_and_delete_sm_poly (
 pSchema VARCHAR2,
 pTopology VARCHAR2,
 pEdgeAttTable VARCHAR2,
 pFaceFeatureTable VARCHAR2,
 pRunId VARCHAR2,
 pSmallPolyOutputTable VARCHAR2,
 pDelEdgesInputTable VARCHAR2
)
-- finds small polygon with "ELIMINATE SMALL POLYGONS"
-- then creates a DELETE_EDGES_INPUT table and runs DELETE EDGES 2 with validation
-- set up for testing School Districts.
AS
BEGIN
 dbms_output.put_line('DOES NOTHING');
 -- drop edge table
 -- remove obsolete nodes
 -- build new edge table
END find_and_delete_sm_poly;

------------------------------------------------------------------------------
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
------------------------------------------------------------------------------

PROCEDURE drop_dangles(
pTopo VARCHAR2,
pValFlag VARCHAR2 default 'TRUE',
pDEBUG VARCHAR2 default 'FALSE'
)
AS
TYPE t_number_list IS TABLE of NUMBER INDEX BY PLS_INTEGER;
vsql VARCHAR2(4000);
vdangles t_number_list;
vordinates MDSYS.SDO_ORDINATE_ARRAY;
vXmin NUMBER;
vYmin NUMBER;
vXmax NUMBER;
vYmax NUMBER;
res   VARCHAR2(10); -- SDO TOPO map results
TPMP  VARCHAR2(50) := pTopo || '_TOPOMAP';
vDebug VARCHAR2(5) := UPPER(pDEBUG);
v_code VARCHAR2(4000);
v_errm VARCHAR2(4000);
vValFlag VARCHAR2(5) := UPPER(pValFlag);
v_failures NUMBER := 0;
v_FaceId NUMBER;
v_edge_tab VARCHAR2(4000) := UPPER(pTopo||'_EDGE$');
BEGIN
    -- output lots of stuff if debugging is turned on
    IF vdebug = 'TRUE' THEN
       DBMS_OUTPUT.PUT_LINE('DROP DANGLES: ** Debugging on **');
    END IF;
    vsql := 'select edge_id from '||v_edge_tab||
            ' where left_face_id = right_face_id '||
            'order by edge_id';
    BEGIN
       EXECUTE IMMEDIATE vsql BULK COLLECT INTO vdangles;
       EXCEPTION
           WHEN OTHERS THEN
              v_code := SQLCODE;
              v_errm := SQLERRM;
              DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERROR:' || v_code || ': ' || v_errm);
              DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERORR: Problem Creating list of dangling edges.');
              DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERORR: Try '''||vsql||''';''');
    END;
    -- create a topo map to use while editing
    BEGIN
       SDO_TOPO_MAP.CREATE_TOPO_MAP(pTopo, TPMP);
       EXCEPTION
           WHEN OTHERS THEN
              v_code := SQLCODE;
              v_errm := SQLERRM;
              DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERROR:' || v_code || ': ' || v_errm);
              DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERORR: Problem Creating the initial TOPO MAP object');
              DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERORR: Try ''EXEC SDO_TOPO_MAP.DROP_TOPO_MAP('''||TPMP||''');''');
              RAISE;      -- crash
    END;
    FOR i IN 1..vdangles.COUNT LOOP
       IF vdebug = 'TRUE' THEN
          DBMS_OUTPUT.PUT_LINE('DROP DANGLES: ---------------- Removing Edge_id '||vdangles(i)||' ----------------');
       END IF;
       -- Find out if this edge in the universal face?
       vsql := 'select left_face_id from '||
               v_edge_tab||' a '||
              'where a.edge_id = :p1 ';
       EXECUTE IMMEDIATE vsql INTO v_FaceId USING vdangles(i);
       -- find coordinates to use for the TOPO MAP
       vordinates := MDSYS.SDO_ORDINATE_ARRAY();
       BEGIN
          IF v_FaceID = -1 THEN -- this is in the universal face, use the edge MBR
             vsql := 'select sdo_geom.sdo_min_mbr_ordinate(a.geometry,b.diminfo,1), '||
                     'sdo_geom.sdo_min_mbr_ordinate(a.geometry,b.diminfo,2),'||
                     'sdo_geom.sdo_max_mbr_ordinate(a.geometry,b.diminfo,1),'||
                     'sdo_geom.sdo_max_mbr_ordinate(a.geometry,b.diminfo,2) from '||
                     v_edge_tab||' a, user_sdo_geom_metadata b '||
                    'where a.edge_id = :p1 and b.table_name = :p2';
             BEGIN
                 EXECUTE IMMEDIATE vsql
                        INTO vXmin,vYmin,vXmax,vYmax
                        USING vdangles(i), v_edge_tab;
                  EXCEPTION
                    WHEN OTHERS THEN
                    v_code := SQLCODE;
                    v_errm := SQLERRM;
                    DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERROR:' || v_code || ': ' || v_errm);
                    DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERORR: Problem retrieving MBR ordinates.');
                    DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERORR: Try '''||vsql||''';');
             END;
          ELSE -- this edge is not in the universal face, just use the face MBR
             vsql := 'select b.mbr_geometry.sdo_ordinates from '||
                  v_edge_tab||' a,  '||pTopo||'_face$ b '||
                 'where a.edge_id = :p1 '||
                 'and a.left_face_id = b.face_id';
             BEGIN
                EXECUTE IMMEDIATE vsql INTO vordinates USING vdangles(i);
                  EXCEPTION
                    WHEN OTHERS THEN
                    v_code := SQLCODE;
                    v_errm := SQLERRM;
                    DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERROR:' || v_code || ': ' || v_errm);
                    DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERORR: Problem retrieving MBR ordinates.');
                    DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERORR: Try '''||vsql||''';');
             END;
             vXmin := vordinates(1);
             vYmin := vordinates(2);
             vXmax := vordinates(3);
             vYmax := vordinates(4);
          END IF;
       END; -- ending MBR ordinate collection block
       -- if the associated face is the universal face, use the MBR of the EDGE
       -- to build the topo map instead of the face.
       vsql := 'select b.mbr_geometry.sdo_ordinates from '||
               v_edge_tab||' a,  '||pTopo||'_face$ b '||
              'where a.edge_id = :p1 '||
              'and a.left_face_id = b.face_id'||
              'and a.left_face_id = -1';
       -- load topo map for the face or edge...
       RES := SDO_TOPO_MAP.LOAD_TOPO_MAP(TPMP, vxMin, vyMin, vxMax, vyMax, 'TRUE');
       IF RES = 'FALSE' THEN
          DBMS_OUTPUT.PUT_LINE('DROP DANGLES: Nothing loaded in the topomap! ');
          DBMS_OUTPUT.PUT_LINE('DROP DANGLES: '||vxMin||','||vyMin||','||vXMax||','||vYmax);
       ELSE
          DBMS_OUTPUT.PUT_LINE('DROP DANGLES: Stuff was loaded in the topomap successfully ');
       END IF;
       -- drop the edge...
       BEGIN
           SDO_TOPO_MAP.REMOVE_EDGE(NULL,vdangles(i));
           EXCEPTION
              WHEN OTHERS THEN
              -- this will happen if one of the edges identified participates in the coastline feature
              DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERROR: Unable to delete edge '||vdangles(i)||'.');
              DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERROR: Test with ''SDO_TOPO_MAP.REMOVE_EDGE('''||pTopo||''','||vdangles(i)||');''');
              v_failures := v_failures + 1;
       END;
       --SDO_TOPO_MAP.UPDATE_TOPO_MAP;
       -- validate the topology if user asked you to
        IF vValFlag = 'TRUE' THEN
          RES := SDO_TOPO_MAP.VALIDATE_TOPO_MAP(TPMP);
          IF RES <> 'TRUE' THEN
             ROLLBACK;
             SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
             DBMS_OUTPUT.PUT_LINE('DROP DANGLES: ---------------------------------------------------------');
             DBMS_OUTPUT.PUT_LINE('DROP DANGLES: Topo Map not valid rolling back.');
          ELSE
             IF vdebug = 'TRUE' THEN
                DBMS_OUTPUT.PUT_LINE('DROP DANGLES: ------------------------------------------------');
                DBMS_OUTPUT.PUT_LINE('DROP DANGLES: Topo Map validated sucessfully for edge '||vdangles(i));
             END IF;
          END IF;
       END IF;
      SDO_TOPO_MAP.COMMIT_TOPO_MAP;
      SDO_TOPO_MAP.CLEAR_TOPO_MAP(TPMP);
      COMMIT;
   END LOOP;
   SDO_TOPO_MAP.DROP_TOPO_MAP(TPMP);
   DBMS_OUTPUT.PUT_LINE('DROP DANGLES: ************************************************');
   DBMS_OUTPUT.PUT_LINE('DROP DANGLES: **');
   DBMS_OUTPUT.PUT_LINE('DROP DANGLES: ** Processing Completed with '||v_failures||' Failures');
   DBMS_OUTPUT.PUT_LINE('DROP DANGLES: **');
   DBMS_OUTPUT.PUT_LINE('DROP DANGLES: ************************************************');
EXCEPTION
  WHEN OTHERS THEN
     v_code := SQLCODE;
     v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
     DBMS_OUTPUT.PUT_LINE('DROP DANGLES ERROR: ' || v_code || ': ' || v_errm);
     SDO_TOPO_MAP.CLEAR_TOPO_MAP(TPMP);
     SDO_TOPO_MAP.DROP_TOPO_MAP(TPMP);
     DBMS_OUTPUT.PUT_LINE('DROP DANGLES: ************************************************');
     DBMS_OUTPUT.PUT_LINE('DROP DANGLES: **');
     DBMS_OUTPUT.PUT_LINE('DROP DANGLES: **     Processing DID NOT COMPLETE');
     DBMS_OUTPUT.PUT_LINE('DROP DANGLES: **          Unexpected Error');
     DBMS_OUTPUT.PUT_LINE('DROP DANGLES: **  '||v_failures||' failures caught before error');
     DBMS_OUTPUT.PUT_LINE('DROP DANGLES: **');
     DBMS_OUTPUT.PUT_LINE('DROP DANGLES: ************************************************');
END drop_dangles;

------------------------------------------------------------------------------
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Sidey's eliminate small polygons
--------------------------------------------------------------------------------
PROCEDURE ELIMINATE_SMALL_POLYS(
                             pInSchema VARCHAR2,
                             pInMT_EDGE$ VARCHAR2,
                             pInTable VARCHAR2,
                             pEdgeTable VARCHAR2,
                             pInGeoIDColumn VARCHAR2,
                             pOutTable VARCHAR2,
                             InScale NUMBER,
                             Area_Cutoff NUMBER,
                             InAreaTolerance NUMBER,
                             pRunId VARCHAR2 default 'X',
                             pCleanup VARCHAR2 default 'N'
                             )
                             AS
/*
********************************************************************************
Example parameters to use...
ELIMINATE_SMALL_POLYS('PEP09GH','MT','FACE','EDGE','GEOID','SMALL_POLY_EDGES',500000.,0.9,0.,'X','N')
********************************************************************************
 Program Name: Eliminate_Small_Polys
 Author: Sidey Timmins
 Creation Date: 02/23/09
 Updated: Mar 09/2010 To get all the edges of a no_keeper when it is surrounded
                      by the keeper polygon. Also area to automatically keep
                      reduced to a quarter of previous size. Maybe a bit small now?
Usage (parameters):
pInSchema VARCHAR2:           Schema where the topology is stored
pInMT_EDGE$ VARCHAR2:         The topology name
pInTable VARCHAR2:            The face feature table name
pEdgeTable VARCHAR2:          The edge feature table name
pInGeoIDColumn VARCHAR2:      The column in the face_feature table used to
                              determine if polys are unique (usually GEOID)
pOutTable VARCHAR2:           the name of the output table which will contain
                              edges to be removed
InScale NUMBER:               target scale - used to calculate the area_tolerance
Area_Cutoff NUMBER            defaults to 0.9 - threshold at which polygons
                              smaller than the biggest are still retained
                              regardless of their small stature.  .09 Means that
                              any small polygon will be retained if it is 90% as
                              large as the largest polygon among the polygons
                              that make up a unique GEOID.
InAreaTolerance NUMBER        area threshold at which polygons smaller than
                              the biggest are still retained. In square meters.
                              Set this negative to disable retention of polygons
                              from nonunique Geoids with (area/biggest area
                              of that GEOID) < Area cutoff but still of
                              considerable area.
pRunId VARCHAR2 default 'X'   Any characterstring - used to uniqiely identify
                              work tables if you have more than one run and
                              keep work tables.  This value must start with a
                              letter as it is used to name tables.  Usually
                              set to the name of the topology, sometimes
                              followed by a number.
pCleanup VARCHAR2 default 'N' Set to 'Y' to automatically delete work tables
                              and indexes
Output:    A table of edges to delete, and which face_id's attributes to retain
        on the resulting face.
Purpose: To make a table of edges from small discontiguous polygons (that are
      not part of unique geographies) to be removed.
Method:  Determine the edges of the nonunique geographies that do not touch a
      unique geography. First finds faces and then edges of:
          1) the unique geographies faces.
          2) the biggest geography of non unique faces
          3) faces from nonunique geographies with area >= 0.9 (the area cutoff)
          4) optionally faces from nonunique geographies with area >= area_tolerance.
     The rest of the edges can be removed.
Reference: - Based upon the CPB specification doc for preprocessing polygons
         before Generalization by Nick Padfield, 01/30/2009
Dependencies:  Edge and face feature tables must have most up to date
               measurements, and match the edge$ and face$ tables exactly
               (important measurements include edgelength, and face sdo geometry)
Additional notes about using the resulting table:
              The keep and not_keep face ids reflect the
              original state of the face$ and the face feature table,
              not the assumed updated state.
              The left and right face_ids reflect the assumed updated state
              of the face table, which will not necessarily match the face_ids
              that Oracle chooses to keep.
              When processing edges, the deleter should
                1) copy attributes from the keep face to the face_id that
                   Oracle decided to keep when you deleted the edge.
                2) Before processing any more records update all
                   occurrences of the keep and not_keep face_ids in
                   the keep and not_keep fileds of the rest of the records
                   by setting them to the value of the face_id that Oracle
                   kept when you deleted the edge.'
                3) By following step 2, all record's keep and not_keep face_ids
                   will match the face_ids in the edge$ table as you are
                   editing.  If you get to a record where keep and not_keep
                   are se to the same face_id, it means the edge can be
                   deleted without the copy attributes step.
             (Stephanie)
Modification History:
    20100809: Stepahnie changed runid to a character and uses it now to prepend
              work table names, so she could
              use the topology name and keep tables together.
              Also added noparallel nologging to all table creation statements.
              Also, with Sidey's help commented out the loop that tries to
              propogate the "future" keep face onto edges when they are inside
              adjacent small polygons.
    20100309: Sidey fixed a problem when a small island face was made up
          of more than one edge, only one of the edges was being marked
          for delete.
    20100309: Stephanie added pRrunId to use in table names, and removed
            CARTODB dependency
            Also commented out the ALTER table
            statement for the edge feature table.  We should assume the
            edge feature table has the necessary fields, and if not,
            we can add them.
            Also - added edge table name as parameter
    20100318: Updated documentation (Stephanie)
    20100330: Fixed a bug in the left and right face id population SQL. (SIDEY)
    2010 05 04 Updated for case when there is a triangle and 4 faces instead of 3
    and there is ambiguity as to which edge to drop because 2 had same length.
********************************************************************************
*/
    TYPE               TblCursorType   IS REF CURSOR;
    Table_cursor       TblCursorType;
    InSchema           VARCHAR2(100) := UPPER(pInSchema);
    GeoidColumn        VARCHAR2(100) := UPPER(pInGeoidColumn);
    outTable           VARCHAR2(100) := UPPER(pOutTable);
    EdgeTable          VARCHAR2(100) := UPPER(pEdgeTable);
    InMt_Edge$         VARCHAR2(100) := UPPER(pInMT_EDGE$);
    UniqTable          VARCHAR2(100);
    FaceTable          VARCHAR2(100) := UPPER(pInTable);
    FaceUpdateTable    VARCHAR2(100);
    NonUniqTable       VARCHAR2(100);
    NonUniqFaceTable   VARCHAR2(100);
    NonUniqEdgeTable   VARCHAR2(100);
    BigUFaceTable      VARCHAR2(100);
    EdgeKeepTable      VARCHAR2(100);
    UpdateTable        VARCHAR2(100);
    DupTable           VARCHAR2(100);
    sql_stmt           VARCHAR2(4000);
    sql_stmt2          VARCHAR2(4000);
    sql_stmt3          VARCHAR2(4000);
    vNumColumns        NUMBER; -- TO see if the EDGE feature's left/right
                               -- columns are already there
    vmsg               VARCHAR2(100) := 'Small Poly Edges: ';
    vFaceIDIndexName   VARCHAR2(100);
    vEdgeIDIndexName   VARCHAR2(100);
    vRightFaceIndexName       VARCHAR2(100);
    vLeftFaceIndexName        VARCHAR2(100);
    vFaceUFaceIndexName       VARCHAR2(100);
    vFaceUKeepFaceIndexName   VARCHAR2(100);
    loop_count         PLS_INTEGER := 0;
    kount              PLS_INTEGER := 0;
    edge_kount         PLS_INTEGER := 0;
    face_kount         PLS_INTEGER := 0;
    face_kept          PLS_INTEGER := 0;
    lcount             PLS_INTEGER := 0;
    current_count      PLS_INTEGER := 0;
    Scale              NUMBER := InScale;
    Area_tolerance     NUMBER;
    nice               NUMBER := 1.;
BEGIN
-- in case someone passes a negative scale denominator
   scale := abs(scale);
   dbms_output.put_line(vmsg||'target scale 1:'||scale);
----------------- Set Area Tolerance if user enters 0.
-- Easiest to work in square meters
   Area_tolerance := InAreaTolerance;
-- Set visible to be 0.04 inches (508m at 1:500000 scale)
-- Might want a bigger area usually
   if Area_tolerance = 0. then
      -- Area_tolerance := nice*258064. *(scale/500000.) * (scale/500000.);
      -- sharper eyes are now viewing te data so make it smaller !!!
      Area_tolerance := nice*258064.*0.25 *(scale/500000.) * (scale/500000.);
   end if;
   dbms_output.put_line(vmsg||'Area Tolerance = '||Area_tolerance||
        ' square meters (identifying edges associated with smaller polygons)');
--------------------------------------------------------------------------------
-- Put a LEFT_FACE_ID AND RIGHT_FACE_ID into EDGE
   -- if they are not there.
   sql_stmt := 'SELECT count(*) FROM ALL_TAB_COLUMNS WHERE OWNER = :p1 '||
               'AND TABLE_NAME = :p2 AND ( COLUMN_NAME = :p3 or COLUMN_NAME = :p4)';
   EXECUTE IMMEDIATE sql_stmt INTO vNumColumns USING inSchema, EdgeTable,
              'LEFT_FACE_ID','RIGHT_FACE_ID';
     dbms_output.put_line(vmsg||'l/r edge columns found in '||
                          EdgeTable||' = '||vNumColumns);
   If (vNumColumns != 2) then
     BEGIN
      EXECUTE IMMEDIATE 'ALTER TABLE '||EdgeTable||' ADD (LEFT_FACE_ID NUMBER)';
      EXECUTE IMMEDIATE 'ALTER TABLE '||EdgeTable||' ADD (RIGHT_FACE_ID NUMBER)';
     EXCEPTION
      WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not add Left and Right face_id columns to the '||
           EdgeTable||' table.');
     END;
   END IF;
   -- Update left/right face ids in the Edge feature table
   -- to make sure they match the most recent face$
   sql_stmt := 'UPDATE '||EdgeTable||
               ' r SET r.LEFT_FACE_ID = (SELECT e.LEFT_FACE_ID FROM ' ||
               inSchema ||'.'|| InMT_EDGE$ ||
               '_EDGE$ e WHERE r.EDGE_ID = e.EDGE_ID)';
   EXECUTE IMMEDIATE sql_stmt;
   sql_stmt := 'UPDATE '||EdgeTable||
               ' r SET r.RIGHT_FACE_ID = (SELECT e.RIGHT_FACE_ID FROM ' ||
               inSchema ||'.'|| InMT_EDGE$ ||
               '_EDGE$ e  WHERE r.EDGE_ID = e.EDGE_ID)';
   EXECUTE IMMEDIATE sql_stmt;
-- First get the faces whose combination of Geography is unique
   UniqTable := pRunId||'SP'||'_UNIQ';
   sql_stmt := 'CREATE TABLE ' || UniqTable || ' noparallel nologging AS SELECT ' ||  GEOIDcolumn ||
               ' from ' || InSchema ||'.' || FaceTable ||' group by ' || GEOIDcolumn ||
               ' having count(' || GEOIDColumn || ') = 1 ';
   dbms_output.put_line('sql_stmt = '||sql_stmt);
   BEGIN
      EXECUTE IMMEDIATE sql_stmt;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create '||
           UniqTable||' table.  '||
           'Try changing your run ID, or dropping this table if it already exists in your schema');
   END;

   GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',UniqTable);

-- Then Create a table of faces whose combination of Geography is not unique
   NonUniqTable := pRunId||'SP'||'_NUNQ';
   sql_stmt := 'CREATE TABLE ' || NonUniqTable || ' noparallel nologging AS SELECT ' ||  GEOIDcolumn ||
               ',max(AreaTotal) AreaTotal from ' || InSchema ||'.' || Facetable ||
               ' group by ' || GEOIDcolumn ||' having count(' || GEOIDColumn || ') > 1';
   BEGIN
      EXECUTE IMMEDIATE sql_stmt;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create '||
           NonUniqTable||' table.  '||
           'Try changing your run ID, or dropping this table if it already exists in your schema');
   END;

   GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',NonUniqTable);

-- Then create a table of Big and unique faces
   BigUFaceTable := pRunId||'SP'||'_BIGU';
   -- we always set Area Tolerance > 0, does this if statement need to be here?
   if Area_tolerance > 0. then
   sql_stmt := 'CREATE TABLE ' || BigUFaceTable || ' noparallel nologging AS SELECT f.FACE_ID,f.PERIMETER,f.' || GEOIDColumn ||
               ',f.AREATOTAL from ' || InSchema|| '.'||FaceTable||' f,'|| NonUniqTable ||
               ' n WHERE f.'|| GEOIDColumn || '= n.'|| GEOIDColumn ||
               ' AND (f.Areatotal >= '||Area_tolerance ||' or f.Areatotal >= (n.AreaTotal*' || Area_Cutoff || '))' ||
               ' UNION ALL SELECT m.FACE_ID,m.PERIMETER,m.' || GEOIDColumn ||',m.AREATOTAL from ' ||
                InSchema|| '.' || FaceTable || ' m,'|| UniqTable || ' u WHERE m.'|| GEOIDColumn || '= u.'|| GEOIDColumn;
   else
      sql_stmt := 'CREATE TABLE ' || BigUFaceTable || ' noparallel nologging AS SELECT f.FACE_ID,f.PERIMETER,f.' || GEOIDColumn ||
               ',f.AREATOTAL from ' ||InSchema||'.' || FaceTable ||' f,'|| NonUniqTable ||
               ' n WHERE f.'|| GEOIDColumn || '= n.'|| GEOIDColumn ||
               ' AND (f.Areatotal >= (n.AreaTotal*' || Area_Cutoff || '))' ||
               ' UNION ALL SELECT m.FACE_ID,m.PERIMETER,m.' || GEOIDColumn ||',m.AREATOTAL from ' ||
               InSchema|| '.' || FaceTable || ' m,'|| UniqTable || ' u WHERE m.'|| GEOIDColumn || '= u.'|| GEOIDColumn;
   end if;
   BEGIN
      EXECUTE IMMEDIATE sql_stmt;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create '||
           BigUFaceTable||' table.  '||
           'Try changing your run ID, or dropping this table if it already exists in your schema');
   END;

   GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',BigUFaceTable);

-- Then create a table of not unique and not Big faces
   NonUniqFaceTable := pRunId||'SP'||'_NUF';
   sql_stmt := 'CREATE TABLE ' || NonUniqFaceTable || ' noparallel nologging AS SELECT f.FACE_ID,f.PERIMETER,0.0 LONG_EDGE,0.0 EDGE_ID,f.AREATOTAL '  ||
                 ' from '|| InSchema|| '.' || FaceTable || ' f MINUS ' ||
                 ' SELECT b.FACE_ID,b.PERIMETER,0.0 LONG_EDGE,0.0 EDGE_ID,b.AREATOTAL from ' ||InSchema|| '.' ||
                 BigUFaceTable || ' b';
   BEGIN
      EXECUTE IMMEDIATE sql_stmt;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create '||
           NonUniqFaceTable||' table.  '||
           'Try changing your run ID, or dropping this table if it already exists in your schema');
   END;

   GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',NonUniqFaceTable);

   vFaceIDIndexName :=  NonUniqFaceTable ||'_FIDX';
   sql_stmt3 :='CREATE INDEX ' || InSchema||'.'|| vFaceIDIndexName || ' ON ' ||InSchema||'.' ||
                NonUniqFaceTable || '(FACE_ID)';
   BEGIN
      EXECUTE IMMEDIATE sql_stmt3;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create Index ('||vFaceIDIndexName||') on '||
           NonUniqFaceTable||' table. '||
           'Try changing your run ID, or dropping this index if it already exists in your schema');
   END;

-- Make a table of edges for the non unique and not Big faces
-- These edges surround these small polygons
   NonUniqEdgeTable := pRunId||'SP'||'_NUED';
    sql_stmt3 :='CREATE TABLE ' ||InSchema|| '.' || NonUniqEdgeTable ||
   ' noparallel nologging AS (SELECT distinct d.EDGE_ID,d.EDGELEN,e.LEFT_FACE_ID,e.RIGHT_FACE_ID,e.LEFT_FACE_ID KEEP_FACE ' ||
   'FROM ' ||InSchema|| '.'||EdgeTable||' d,'|| inSchema ||'.'|| InMT_EDGE$ ||'_EDGE$ e,'||NonUniqFaceTable || ' b WHERE ' ||
   '(e.LEFT_FACE_ID = b.FACE_ID or e.RIGHT_FACE_ID = b.FACE_ID)' ||
   ' AND d.edge_id = e.edge_id)';
   BEGIN
      EXECUTE IMMEDIATE sql_stmt3;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create '||
           NonUniqEdgeTable||' table.  '||
           'Try changing your run ID, or dropping this table if it already exists in your schema');
   END;

   GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',NonUniqEdgeTable);

   vEdgeIDIndexName := NonUniqEdgeTable || '_IDX';
   sql_stmt3 :='CREATE INDEX '|| InSchema|| '.' ||vEdgeIDIndexName||' ON ' ||InSchema|| '.' ||
                NonUniqEdgeTable || '(EDGE_ID)';
   BEGIN
      EXECUTE IMMEDIATE sql_stmt3;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create index '|| vEdgeIDIndexName||' on the '||
           NonUniqEdgeTable||' table.  '||
           'Try changing your run ID, or dropping this index if it already exists in your schema');
   END;
   vLeftFaceIndexName := NonUniqEdgeTable || '_LIDX';
   sql_stmt3 :='CREATE INDEX '|| InSchema|| '.'||vLeftFaceIndexName||' ON ' ||
                InSchema|| '.' || NonUniqEdgeTable || '(LEFT_FACE_ID)';
  BEGIN
      EXECUTE IMMEDIATE sql_stmt3;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create index '|| vLeftFaceIndexName||' on the '||
           NonUniqEdgeTable||' table.  '||
           'Try changing your run ID, or dropping this index if it already exists in your schema');
   END;
   vRightFaceIndexName := NonUniqEdgeTable || '_RIDX';
   sql_stmt3 :='CREATE INDEX '||InSchema|| '.' ||vRightFaceIndexName||' ON ' ||InSchema||'.' ||
                NonUniqEdgeTable || '(RIGHT_FACE_ID)';
   BEGIN
      EXECUTE IMMEDIATE sql_stmt3;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create index '|| vLeftFaceIndexName||' on the '||
           NonUniqEdgeTable||' table.  '||
           'Try changing your run ID, or dropping this index if it already exists in your schema');
   END;



-- We don't want the outer polygon chosen below so set the the edgelen to zero.
   sql_stmt3 := 'UPDATE ' ||InSchema|| '.' || NonUniqEdgeTable || ' SET EDGELEN = 0 WHERE LEFT_FACE_ID = -1 OR RIGHT_FACE_ID = -1';
    EXECUTE IMMEDIATE sql_stmt3;
  commit;
   DBMS_STATS.gather_table_stats(ownname => inSchema, tabname => NonUniqFaceTable, estimate_percent => 100, cascade => TRUE);
   DBMS_STATS.gather_table_stats(ownname => inSchema, tabname => NonUniqEdgeTable, estimate_percent => 100, cascade => TRUE);
-- Get the long edge
    sql_stmt3 :='UPDATE ' ||InSchema|| '.' || NonUniqFaceTable || ' n SET LONG_EDGE = ' ||
                '(SELECT MAX(e.EDGELEN) FROM ' || NonUniqEdgeTable ||
                ' e WHERE (e.LEFT_FACE_ID = n.FACE_ID) or (e.RIGHT_FACE_ID = n.FACE_ID) GROUP BY n.FACE_ID)';
    EXECUTE IMMEDIATE sql_stmt3;
    commit;
--                     BIG face 2                Big face 2
--                        Left                    Right
--                     ------>-----             ----<-----
--                     |  small   |             | small  |
--                     | face 1   |             | face 1 |
--                     |   R      |             |   L    |
--              ------------------------    ------------------------
---                   Big Face 3                 Big Face 3
--         New case where 4 faces involved, 3 big and 1 small
--                                               /
--                      --------                /
--                               \--------------
--                                 \          /
--            same edge length       \       / same edge length
--                                     \    /
--                                       \ /
--                                        /
--                                       /
    sql_stmt3 :='UPDATE ' ||InSchema|| '.' || NonUniqFaceTable || ' n SET EDGE_ID = ' ||
                '(SELECT -e.EDGE_ID FROM ' || NonUniqEdgeTable ||
                ' e WHERE (e.EDGELEN = n.LONG_EDGE) AND (e.LEFT_FACE_ID = n.FACE_ID) AND rownum < 2)';
     EXECUTE IMMEDIATE sql_stmt3;
     sql_stmt3 :='UPDATE ' ||InSchema|| '.' || NonUniqFaceTable || ' n SET EDGE_ID = ' ||
                '(SELECT e.EDGE_ID FROM ' || NonUniqEdgeTable ||
                ' e WHERE e.EDGELEN = n.LONG_EDGE AND e.RIGHT_FACE_ID = n.FACE_ID and rownum < 2)' ||
                ' WHERE EXISTS (SELECT g.EDGE_ID FROM ' || NonUniqEdgeTable ||
                ' g WHERE g.EDGELEN = n.LONG_EDGE  And g.right_face_id = n.face_id and rownum < 2)';
    EXECUTE IMMEDIATE sql_stmt3;
    commit;
    FaceUpdateTable := pRunId||'SP'||'_FACEU';
    sql_stmt3 :='CREATE TABLE ' ||InSchema|| '.' || FaceUpDateTable || ' noparallel nologging AS SELECT ' ||
   'e.LEFT_FACE_ID FACE_ID,e.RIGHT_FACE_ID KEEP_FACE ' ||
   ' FROM ' ||InSchema|| '.' ||NonUniqFaceTable || ' n,' ||InSchema|| '.' ||  NonUniqEdgeTable ||
   ' e WHERE -n.EDGE_ID = e.EDGE_ID UNION ' ||
   'SELECT g.RIGHT_FACE_ID FACE_ID,g.LEFT_FACE_ID KEEP_FACE ' ||
    ' FROM ' ||InSchema|| '.' ||NonUniqFaceTable || ' m,' ||InSchema|| '.' ||  NonUniqEdgeTable ||
    ' g WHERE m.EDGE_ID = g.EDGE_ID';
   BEGIN
      EXECUTE IMMEDIATE sql_stmt3;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create '||
           FaceUpdateTable||' table.  '||
           'Try changing your run ID, or dropping this table if it already exists in your schema');
   END;

   GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',FaceUpDateTable);

    vFaceUFaceIndexName := FaceUpdateTable || '_FIDX';
    sql_stmt3 :='CREATE INDEX ' ||InSchema|| '.'||  vFaceUFaceIndexName||' ON ' ||InSchema|| '.' ||
                FaceUpdateTable || '(FACE_ID)';
   BEGIN
      EXECUTE IMMEDIATE sql_stmt3;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create index '||  vFaceUFaceIndexName||' on the '||
           FaceUpdateTable||' table.  '||
           'Try changing your run ID, or dropping this index if it already exists in your schema');
   END;
    vFaceUKeepFaceIndexName := FaceUpdateTable || '_KIDX';
    sql_stmt3 :='CREATE INDEX ' ||InSchema|| '.'|| vFaceUKeepFaceIndexName||' ON ' ||InSchema|| '.' ||
                FaceUpdateTable || '(KEEP_FACE)';
   BEGIN
      EXECUTE IMMEDIATE sql_stmt3;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create index '|| vFaceUKeepFaceIndexName ||' on the '||
           FaceUpdateTable||' table.  '||
           'Try changing your run ID, or dropping this index if it already exists in your schema');
   END;
/*
Don't propogate changing face_ids 20100809
    WHILE loop_count < 20 LOOP
    dbms_output.put_line('LOOP ' || loop_count);
    loop_count := loop_count + 1;
    sql_stmt3 :='UPDATE ' ||InSchema|| '.' || NonUniqEdgeTable || ' n SET LEFT_FACE_ID = ' ||
                '(SELECT u.KEEP_FACE FROM ' || FaceUpdateTable ||
                ' u WHERE (n.LEFT_FACE_ID = u.FACE_ID))' ||
                ' WHERE EXISTS (SELECT u.KEEP_FACE FROM ' || FaceUpdateTable ||
                ' u WHERE n.LEFT_FACE_ID = u.FACE_ID)';
     EXECUTE IMMEDIATE sql_stmt3;
     commit;
     sql_stmt3 :='UPDATE ' ||InSchema|| '.' || NonUniqEdgeTable || ' n SET RIGHT_FACE_ID = ' ||
                '(SELECT u.KEEP_FACE FROM ' || FaceUpdateTable ||
                ' u WHERE (n.RIGHT_FACE_ID = u.FACE_ID))' ||
                ' WHERE EXISTS (SELECT u.KEEP_FACE FROM ' || FaceUpdateTable ||
                ' u WHERE n.RIGHT_FACE_ID = u.FACE_ID)';
     EXECUTE IMMEDIATE sql_stmt3;
     commit;
     lcount := current_count;
     EXECUTE IMMEDIATE 'SELECT COUNT(1) FROM ' || NonUniqEdgeTable || ' n,' ||
                        FaceUpdateTable || ' u WHERE ' ||
                        '(n.LEFT_FACE_ID = u.FACE_ID OR n.RIGHT_FACE_ID = u.FACE_ID)'
                        into current_count;
     dbms_output.put_line(vMsg||'updates ' || current_count);
     exit when lcount = current_count;
    END LOOP;
*/
--   We choose  depending upon the length of the edge and the direction
--   Example
--                     BIG face 2                Big face 2
--                        Left                    Right
--                     ------>-----             ----<-----
--  Lenght A(up,       |  Big     |             | Big     | Length A (up over and down)
--  over and down)     | face 2   |             | face 2  |     < Length B
--   > Length B        |   R      |             |   L     |
--              ---------Length B----------------Length B------------------
---                   Big Face 3                 Big Face 3
    sql_stmt3 :='CREATE TABLE ' ||InSchema|| '.' || OutTable || ' noparallel nologging AS SELECT -n.EDGE_ID EDGE_ID,' ||
   'd.LEFT_FACE_ID,e.RIGHT_FACE_ID,d.RIGHT_FACE_ID KEEP_FACE,d.LEFT_FACE_ID NOT_KEEP,n.AREATOTAL,n.AREATOTAL MAX_AREA ' ||
   ' FROM '||InSchema||'.' ||NonUniqFaceTable || ' n,' ||InSchema|| '.' ||  NonUniqEdgeTable ||
   ' e,' || InSchema || '.'|| InMT_EDGE$ ||'_EDGE$ d WHERE -n.EDGE_ID = e.EDGE_ID AND -n.EDGE_ID = d.EDGE_ID UNION ' ||
   'SELECT m.EDGE_ID,g.LEFT_FACE_ID,c.RIGHT_FACE_ID,c.LEFT_FACE_ID KEEP_FACE,c.RIGHT_FACE_ID NOT_KEEP,m.AREATOTAL,m.AREATOTAL MAX_AREA ' ||
    ' FROM ' ||InSchema|| '.' ||NonUniqFaceTable || ' m,' ||InSchema|| '.' ||  NonUniqEdgeTable ||
    ' g,' || InSchema || '.'|| InMT_EDGE$ ||'_EDGE$ c WHERE m.EDGE_ID = g.EDGE_ID AND m.EDGE_ID = c.EDGE_ID';
--   dbms_output.put_line(vmsg||'SQL to create OutTable = '''||sql_stmt3||'''');
   BEGIN
      EXECUTE IMMEDIATE sql_stmt3;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create '||
          OutTable||' table.  '||
           'Try changing your run ID, or dropping this table if it already exists in your schema');
   END;

   GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',OutTable);

--
-- We can get this case for a single edge - its the longest in 2 faces
-- so both faces want to be kept - so keep the biggest.
--   Example
--                     BIG face 2
--                        Left
--                     ------>-----
--  Lenght A(up,       |  Big     |
--  over and down)     | face 2   |
--   > Length B        |   R      |
--                     ---Length B--   Length B > A and C
---                    |Big Face  |  Length C
--                     |____1_____|
--
    DupTable := pRunId||'SP'||'_DUP';
    sql_stmt3 :='CREATE TABLE ' ||InSchema ||'.' || DupTable || ' noparallel nologging AS SELECT ' ||
    'EDGE_ID,MAX(AREATOTAL) MAX_AREA FROM ' || OutTable || ' GROUP BY EDGE_ID HAVING COUNT(EDGE_ID) > 1';
   BEGIN
      EXECUTE IMMEDIATE sql_stmt3;
      EXCEPTION
      WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,
          vmsg||'Error!  Could not create '||
          DupTable||' table.  '||
           'Try changing your run ID, or dropping this table if it already exists in your schema');
   END;

   GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',DupTable);

    sql_stmt3 := 'UPDATE ' || OutTable || ' o SET o.MAX_AREA = (SELECT d.MAX_AREA FROM ' || DupTable ||
    ' d WHERE d.EDGE_ID = o.EDGE_ID)';
    EXECUTE IMMEDIATE sql_stmt3;
    -- drop edges you can't delete because they are islands or the max area has been set to something other than areatotal
    EXECUTE IMMEDIATE 'DELETE FROM ' || InSchema|| '.'|| OutTable || ' WHERE AREATOTAL <> MAX_AREA or LEFT_FACE_ID =-1 or RIGHT_FACE_ID=-1';
    commit;
-- Get the other edges of a polygon that we are not keeping when the keeper wraps around the
-- not keeper and makes it into an island within the keeper
    sql_stmt3 :='INSERT INTO ' ||InSchema|| '.' || OutTable || ' SELECT d.EDGE_ID EDGE_ID,' ||
   'd.LEFT_FACE_ID,d.RIGHT_FACE_ID,d.RIGHT_FACE_ID KEEP_FACE,d.LEFT_FACE_ID NOT_KEEP,e.AREATOTAL,-1 MAX_AREA ' ||
   ' FROM ' ||InSchema|| '.' ||  OutTable ||
   ' e,' || InSchema || '.'|| InMT_EDGE$ ||'_EDGE$ d WHERE  e.EDGE_ID <> d.EDGE_ID AND ' ||
   ' (d.RIGHT_FACE_ID=e.KEEP_FACE and d.LEFT_FACE_ID=e.NOT_KEEP) AND NOT EXISTS ' ||
   ' (SELECT g.EDGE_ID FROM ' ||InSchema|| '.' ||  OutTable ||' f,' || InSchema || '.'|| InMT_EDGE$ ||'_EDGE$ g WHERE  f.EDGE_ID <> g.EDGE_ID AND ' ||
   ' (g.RIGHT_FACE_ID=f.KEEP_FACE and g.LEFT_FACE_ID=f.NOT_KEEP))';
    EXECUTE IMMEDIATE sql_stmt3;
    sql_stmt3 :='INSERT INTO ' ||InSchema|| '.' || OutTable || ' SELECT d.EDGE_ID EDGE_ID,' ||
   'd.LEFT_FACE_ID,d.RIGHT_FACE_ID,d.LEFT_FACE_ID KEEP_FACE,d.RIGHT_FACE_ID NOT_KEEP,e.AREATOTAL,-2 MAX_AREA ' ||
   ' FROM ' ||InSchema|| '.' ||  OutTable ||
   ' e,' || InSchema || '.'|| InMT_EDGE$ ||'_EDGE$ d WHERE  e.EDGE_ID <> d.EDGE_ID AND ' ||
   ' (d.LEFT_FACE_ID=e.KEEP_FACE and d.RIGHT_FACE_ID=e.NOT_KEEP) AND NOT EXISTS ' ||
   ' (SELECT g.EDGE_ID FROM ' ||InSchema|| '.' ||  OutTable ||' f,' || InSchema || '.'|| InMT_EDGE$ ||'_EDGE$ g WHERE  f.EDGE_ID <> g.EDGE_ID AND ' ||
   ' (g.LEFT_FACE_ID=f.KEEP_FACE and g.RIGHT_FACE_ID=f.NOT_KEEP))';
    EXECUTE IMMEDIATE sql_stmt3;
    commit;
    dbms_output.put_line('DONE');
   IF (pCleanup = 'Y') THEN
     dbms_output.put_line(vmsg||'Cleaning Up Work tables.');
     EXECUTE IMMEDIATE 'DROP INDEX ' || vFaceIDIndexName;
     EXECUTE IMMEDIATE 'DROP INDEX ' || vEdgeIDIndexName;
     EXECUTE IMMEDIATE 'DROP INDEX ' || vRightFaceIndexName;
     EXECUTE IMMEDIATE 'DROP INDEX ' || vLeftFaceIndexName;
     EXECUTE IMMEDIATE 'DROP INDEX ' || vFaceUFaceIndexName;
     EXECUTE IMMEDIATE 'DROP INDEX ' || vFaceUKeepFaceIndexName;
     EXECUTE IMMEDIATE 'DROP TABLE ' || DupTable;
     EXECUTE IMMEDIATE 'DROP TABLE ' || NonUniqFaceTable;
     EXECUTE IMMEDIATE 'DROP TABLE ' || NonUniqEdgeTable;
     EXECUTE IMMEDIATE 'DROP TABLE ' || BigUFaceTable;
     EXECUTE IMMEDIATE 'DROP TABLE ' || UniqTable;
     EXECUTE IMMEDIATE 'DROP TABLE ' || FaceUpdateTable;
     EXECUTE IMMEDIATE 'DROP TABLE ' || NonUniqTable;
   ELSE
     dbms_output.put_line(vmsg||'Work tables not cleaned up.  '||
                          'Execute this SQL to cleanup...');
     dbms_output.put_line('   DROP INDEX ' || vFaceIDIndexName||';');
     dbms_output.put_line('   DROP INDEX ' || vEdgeIDIndexName||';');
     dbms_output.put_line('   DROP INDEX ' || vRightFaceIndexName||';');
     dbms_output.put_line('   DROP INDEX ' || vLeftFaceIndexName||';');
     dbms_output.put_line('   DROP INDEX ' || vFaceUFaceIndexName||';');
     dbms_output.put_line('   DROP INDEX ' || vFaceUKeepFaceIndexName||';');
     dbms_output.put_line('   DROP TABLE ' || DupTable||';');
     dbms_output.put_line('   DROP TABLE ' || NonUniqFaceTable||';');
     dbms_output.put_line('   DROP TABLE ' || NonUniqEdgeTable||';');
     dbms_output.put_line('   DROP TABLE ' || BigUFaceTable||';');
     dbms_output.put_line('   DROP TABLE ' || UniqTable||';');
     dbms_output.put_line('   DROP TABLE ' || FaceUpdateTable||';');
     dbms_output.put_line('   DROP TABLE ' || NonUniqTable||';');
   END IF;
END ELIMINATE_SMALL_POLYS;

--------------------------------------------------------------------------------
--******************************************************************************
--------------------------------------------------------------------------------

   FUNCTION GET_SMPOLY_PROJECT_PARAMETERS (
      p_project_id         IN VARCHAR2,
      p_release             IN VARCHAR2
   ) RETURN GZ_TYPES.SMALL_POLYGON_PARAMETERS_REC
   AS
     -- Copied from Matt's clipper
      psql     VARCHAR2(4000);
      output   GZ_TYPES.SMALL_POLYGON_PARAMETERS_REC;
   BEGIN
      psql := 'SELECT a.* FROM SMALL_POLYGON_PARAMETERS a '
           || 'WHERE  a.gen_project_id = :p1 AND a.release = :p2';
      BEGIN
         EXECUTE IMMEDIATE psql INTO output USING UPPER(p_project_id), p_release;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Small_polygon_Parameter record not found for project id ' || p_project_id );
         WHEN TOO_MANY_ROWS THEN
            RAISE_APPLICATION_ERROR(-20001,'Found more than one small_polygon_Parameter record for project id ' || p_project_id);
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Table SMALL_POLYGON_PARAMETERS does not exist!');
            ELSE
               RAISE;
            END IF;
      END;
      RETURN output;
   END GET_SMPOLY_PROJECT_PARAMETERS;
   
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -- | Matt! Face Merge Code below here  |
   -- |                                   |                                   
   -- V                                   V
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION STRINGHASH_ADD (
      p_input_1   IN GZ_TYPES.stringhash,
      p_input_2   IN GZ_TYPES.stringhash
   ) RETURN GZ_TYPES.stringhash DETERMINISTIC
   AS

      --Matt! 10/15/12
      --Not sure if I use this

      output      GZ_TYPES.stringhash;
      llave       VARCHAR2(4000);

   BEGIN

      llave := p_input_1.FIRST;

      LOOP

         EXIT WHEN NOT p_input_1.EXISTS(llave);

         output(llave) := p_input_1(llave);

         llave  := p_input_1.NEXT(llave);

      END LOOP;

      llave := p_input_2.FIRST;

      LOOP

         EXIT WHEN NOT p_input_2.EXISTS(llave);

         IF NOT output.EXISTS(llave)
         THEN

            output(llave) := p_input_2(llave);

         END IF;

         llave  := p_input_2.NEXT(llave);

      END LOOP;

      RETURN output;

   END STRINGHASH_ADD;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------


   FUNCTION STRINGHASH_SUBTRACT (
      p_input_1   IN GZ_TYPES.stringhash,
      p_input_2   IN GZ_TYPES.stringhash
   ) RETURN GZ_TYPES.stringhash DETERMINISTIC
   AS

      --Matt! 10/17/12

      output      GZ_TYPES.stringhash;
      llave       VARCHAR2(4000);

   BEGIN

      llave := p_input_1.FIRST;

      LOOP

         EXIT WHEN NOT p_input_1.EXISTS(llave);

         IF NOT p_input_2.EXISTS(llave)
         THEN

            output(llave) := p_input_1(llave);

         END IF;

         llave  := p_input_1.NEXT(llave);

      END LOOP;

      RETURN output;

   END STRINGHASH_SUBTRACT;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private---------------------------------------------------------------------------


   FUNCTION IS_STRING_IN_ARRAY (
      p_string          IN VARCHAR2,
      p_array           IN GZ_TYPES.stringarray,
      p_like_flag       IN VARCHAR2 DEFAULT NULL  --put %s in input p_string
   ) RETURN BOOLEAN
   AS

      --Matt! 10/18/12

   BEGIN

      IF p_array.COUNT = 0
      THEN

          RETURN FALSE;

      END IF;

      IF p_string = ''
      THEN

         --assume this is unintentional
         RAISE_APPLICATION_ERROR(-20001,'Input string is null!');

      END IF;

      --should do some sorting if arrays are big
      --intended use here is handfuls of values

      FOR i IN 1 .. p_array.COUNT
      LOOP

          IF p_like_flag IS NULL
          THEN

             IF p_array(i) = p_string
             THEN

                RETURN TRUE;

             END IF;

          ELSE

             IF p_array(i) LIKE p_string
             THEN

                RETURN TRUE;

             END IF;

          END IF;

      END LOOP;

      RETURN FALSE;


   END IS_STRING_IN_ARRAY;
   

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION NUMBERARRAY_ADD_UNIQUE (
     p_input_1   IN GZ_TYPES.numberarray,
     p_input_2   IN GZ_TYPES.numberarray
   ) RETURN GZ_TYPES.numberarray DETERMINISTIC
   AS

      --Matt! 10/16/12

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
   --Public----------------------------------------------------------------------------------------

   FUNCTION DUBNUMBERARRAY_ORDER (
     p_key           IN NUMBER,
     p_val           IN NUMBER,
     p_inputarray    IN GZ_TYPES.doublenumarray,
     p_order         IN VARCHAR2 DEFAULT 'ASC'
   ) RETURN GZ_TYPES.doublenumarray DETERMINISTIC
   AS
   
      --Matt! 11/16/12
      --This is dubdiculous
      --want to keep a list of keys and values in order by value
      --for example edges and distances
      --Assumption that the input is correctly ordered
      
      output            GZ_TYPES.doublenumarray;
      all_done          PLS_INTEGER := 0;
      kounter           PLS_INTEGER := 1;
      
   BEGIN
   
      IF p_inputarray.COUNT = 0
      THEN
      
         output(1).number1 := p_key;
         output(1).number2 := p_val;
         RETURN output;         
      
      END IF;
      
   
      FOR i IN 1 .. p_inputarray.COUNT
      LOOP
         
         IF p_order = 'ASC'
         THEN
         
            IF p_val <= p_inputarray(i).number2
            AND all_done = 0
            THEN
            
               
               output(kounter).number1 := p_key;
               output(kounter).number2 := p_val;
               
               kounter := kounter + 1;
               all_done := 1;

            END IF;       
            
            output(kounter) := p_inputarray(i);  
         
         ELSIF p_order = 'DESC'
         THEN      
         
            IF p_val >= p_inputarray(i).number2
            AND all_done = 0
            THEN
            
               output(kounter).number1 := p_key;
               output(kounter).number2 := p_val;
               
               kounter := kounter + 1;
               all_done := 1;

            END IF;       
            
            output(kounter) := p_inputarray(i); 
         
         END IF;
         
         kounter := kounter + 1;
      
      END LOOP;
      
      IF all_done = 0
      THEN
      
         output(kounter).number1 := p_key;
         output(kounter).number2 := p_val;
               
      END IF;
      
      RETURN output;   
   
   END DUBNUMBERARRAY_ORDER;   

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------


   FUNCTION MANAGE_FSL_LIST (
      p_list            IN GZ_TYPES.stringhash,
      p_fsl_table       IN VARCHAR2,
      p_key_val         IN VARCHAR2,
      p_delimiter       IN VARCHAR2 DEFAULT ';'
   ) RETURN GZ_TYPES.stringhash
   AS

      --Matt! 10/18/12
      --Add a geo_id to our hash of fsl tables | geo_ids

      --Example

      --input list (values nonsense)
      -- Z699IN_FSL960V   9600000US5006360
      -- Z699IN_FSL160V   1600000US2567000;1600000US4419180

      --p_fsl_table
      --Z699IN_FSL160V

      --p_key_val
      --1600000US5010675

      --output list (values nonsense)
      -- Z699IN_FSL960V Z699IN_FSL960V
      -- Z699IN_FSL160V 1600000US2567000;1600000US4419180;1600000US5010675

      --But do not add dupes

      output            GZ_TYPES.stringhash;
      geo_id_array      GZ_TYPES.stringarray;


   BEGIN

      output := p_list;

      IF NOT p_list.EXISTS(p_fsl_table)
      THEN

         output(p_fsl_table) := p_key_val;

      ELSE

         geo_id_array := GZ_UTILITIES.SPLIT(output(p_fsl_table), p_delimiter);

         IF NOT IS_STRING_IN_ARRAY(p_key_val, geo_id_array)
         THEN

            output(p_fsl_table) := output(p_fsl_table) || p_delimiter || p_key_val;

         END IF;

      END IF;

      RETURN output;

   END MANAGE_FSL_LIST;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION SUBTRACT_FSL_LIST (
      p_list            IN GZ_TYPES.stringhash,
      p_fsl_table       IN VARCHAR2,
      p_key_val         IN VARCHAR2,
      p_delimiter       IN VARCHAR2 DEFAULT ';'
   ) RETURN GZ_TYPES.stringhash
   AS

      --Matt! 10/22/12
      --Subtract a geo_id from our hash of fsl tables | geo_ids

      --Example

      --input list (values nonsense)
      -- Z699IN_FSL960V 9600000US5006360
      -- Z699IN_FSL160V 1600000US2567000;1600000US4419180;1600000US5010675

      --p_fsl_table
      --Z699IN_FSL160V

      --p_key_val
      --1600000US4419180

      --output list
      -- Z699IN_FSL960V 9600000US5006360
      -- Z699IN_FSL160V 1600000US2567000;1600000US5010675

      output            GZ_TYPES.stringhash;
      geo_id_array      GZ_TYPES.stringarray;
      popped            PLS_INTEGER := 0;


   BEGIN

      output := p_list;

      IF NOT p_list.EXISTS(p_fsl_table)
      THEN

         RETURN output;

      ELSE

         geo_id_array := GZ_UTILITIES.SPLIT(output(p_fsl_table), p_delimiter);

         FOR i IN 1 .. geo_id_array.COUNT
         LOOP

            IF geo_id_array(i) <> p_key_val
            THEN

               IF popped = 0
               THEN

                  output(p_fsl_table) := geo_id_array(i);
                  popped := 1;

               ELSE

                  output(p_fsl_table) := output(p_fsl_table) || p_delimiter || geo_id_array(i);

               END IF;

            END IF;

         END LOOP;

         IF popped = 0
         THEN

            output.DELETE(p_fsl_table);

         END IF;

      END IF;

      RETURN output;

   END SUBTRACT_FSL_LIST;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION SUBTRACT_HASH_UNIQUE (
      p_list            IN GZ_TYPES.stringhash,
      p_subtract_list   IN GZ_TYPES.stringhash,
      p_delimiter       IN VARCHAR2 DEFAULT ';'
   ) RETURN GZ_TYPES.stringhash
   AS

      --Matt! 10/22/12
      --Subtract all geo_ids in one hash from the base hash
      --subtract list is unique key|val, never key|val1;val2;val3


      output            GZ_TYPES.stringhash;
      mykey             VARCHAR2(4000);


   BEGIN

      output := p_list;

      mykey := p_subtract_list.FIRST;

      LOOP

         EXIT WHEN NOT p_subtract_list.EXISTS(mykey);

         output := GZ_SMPOLY.SUBTRACT_FSL_LIST(output,
                                               mykey,
                                               p_subtract_list(mykey),
                                               p_delimiter);

         mykey := p_subtract_list.NEXT(mykey);

      END LOOP;

      RETURN output;

   END SUBTRACT_HASH_UNIQUE;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------
   
   FUNCTION GET_EDGE_MBR (
      p_topo            IN VARCHAR2,
      p_edge            IN NUMBER
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 9/6/10
      --Copied from clip 11/06/12

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
   --Public---------------------------------------------------------------------------------
   
   FUNCTION GET_EDGE_NODE (
      p_topo            IN VARCHAR2,
      p_edge            IN NUMBER,
      p_tip             IN VARCHAR2 DEFAULT 'START'
   ) RETURN NUMBER
   AS
   
      --Matt! 11/07/12
      
      psql              VARCHAR2(4000);
      output            NUMBER;
      
   BEGIN
   
      IF p_tip NOT IN ('START','END')
      THEN
      
         RAISE_APPLICATION_ERROR(-20001,'Tip on out with this tip ' || p_tip);
      
      END IF;
   
      psql := 'SELECT e.' || p_tip || '_node_id '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';
           
      BEGIN
      
         EXECUTE IMMEDIATE psql INTO output USING p_edge;
         
      EXCEPTION
      
      WHEN NO_DATA_FOUND
      THEN
      
         RAISE_APPLICATION_ERROR(-20001,'Found nothing for edge ' || p_edge || ' with ' || psql);
      
      WHEN OTHERS
      THEN
      
         RAISE;
         
      END;     
      
      IF p_edge IS NULL
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Nuthin for ' || p_edge);
      
      END IF;
      
      RETURN output;   
   
   END GET_EDGE_NODE;
      
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------
   
   FUNCTION GET_FEATURE_FACE_GEOMETRY (
      p_face_table      IN VARCHAR2,
      p_face_id         IN NUMBER
   ) RETURN SDO_GEOMETRY
   AS
   
      --Matt! 11/16/12
      
      psql        VARCHAR2(4000);
      output      SDO_GEOMETRY;
      
   BEGIN
   
      psql := 'SELECT a.sdogeometry '
           || 'FROM ' || p_face_table || ' a '
           || 'WHERE '
           || 'a.face_id = :p1 ';
      
      EXECUTE IMMEDIATE psql INTO output USING p_face_id;
      
      RETURN output;
   
   END GET_FEATURE_FACE_GEOMETRY;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

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
   --Public----------------------------------------------------------------------------
   
   FUNCTION GET_EDGES_BETWEEN_FACES (
      p_topology           IN VARCHAR2,
      p_face_id_1          IN NUMBER,
      p_face_id_2          IN NUMBER
   ) RETURN GZ_TYPES.numberarray 
   AS
   
      --Matt! 11/02/12
      --This SQL was getting copied and pasted too often
      
      psql              VARCHAR2(4000);
      output            GZ_TYPES.numberarray;
      
   BEGIN
      
      psql := 'SELECT e.edge_id '
           || 'FROM ' || p_topology || '_edge$ e '
           || 'WHERE '
           || '(e.left_face_id = :p1 AND e.right_face_id = :p2) OR '
           || '(e.left_face_id = :p3 AND e.right_face_id = :p4) ';
           
      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING p_face_id_1,
                                                            p_face_id_2,
                                                            p_face_id_2,
                                                            p_face_id_1;
      /*      For now                                                
      IF output.COUNT = 0
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'No shared edges between ' || p_face_id_1 || ' and ' || p_face_id_2);
         
      END IF;
      */
      
      RETURN output;
   
   END GET_EDGES_BETWEEN_FACES;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------
   
   FUNCTION GET_EDGE_BETWEEN_FACES (
      p_topology           IN VARCHAR2,
      p_face_id_1          IN NUMBER,
      p_face_id_2          IN NUMBER
   ) RETURN NUMBER 
   AS
   
      --Matt! 11/05/12
      output               GZ_TYPES.numberarray;
      
   BEGIN
   
      output := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES(p_topology,
                                                  p_face_id_1,
                                                  p_face_id_2);
                                        
      IF output.COUNT <> 1
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Should only be one edge between ' || p_face_id_1 || ' and ' || p_face_id_2);
      
      END IF;
      
      RETURN output(1);   
   
   END GET_EDGE_BETWEEN_FACES;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------
   
    FUNCTION GET_EDGES_BETWEEN_FACES_HINTED (
      p_topology           IN VARCHAR2,
      p_face_id_1          IN NUMBER,
      p_face_id_2          IN NUMBER,
      p_edge_hint          IN NUMBER,
      p_edge_hint_2        IN NUMBER DEFAULT NULL
   ) RETURN GZ_TYPES.numberarray
   AS
   
      --Matt! 11/08/12
      --Theres more than one edge between these faces.
      --I want only the edges that touch some other edge I'm confident about
      
      --     
      --         ___    2
      --         |  \
      --   0-----0---0
      --   |   1      |<--want (maybe another on the other side too)
      --   |          |
      -----------hint----------
      
      output               GZ_TYPES.numberarray;
      edgez                GZ_TYPES.numberarray;
      
   BEGIN
   
      edgez := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES(p_topology,
                                                 p_face_id_1,
                                                 p_face_id_2);
                                                 
      IF edgez.COUNT = 0
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Didnt find ANY edges with faces ' 
                                 || p_face_id_1 || ' and ' || p_face_id_2);
      
      END IF;
                                                    
      FOR i IN 1 .. edgez.COUNT
      LOOP
      
         IF p_edge_hint_2 IS NULL
         THEN
         
            IF GZ_SMPOLY.ARE_EDGES_CONNECTED(p_topology, p_edge_hint, edgez(i)) 
            THEN
            
               output(output.COUNT + 1) := edgez(i);
            
            END IF;
            
         ELSE
         
            IF  GZ_SMPOLY.ARE_EDGES_CONNECTED(p_topology, p_edge_hint, edgez(i)) 
            AND GZ_SMPOLY.ARE_EDGES_CONNECTED(p_topology, p_edge_hint_2, edgez(i)) 
            THEN
            
               output(output.COUNT + 1) := edgez(i);
            
            END IF;
         
         END IF;
      
      END LOOP;       
      
      IF output IS NULL
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Didnt find 1 edges with faces ' 
                                 || p_face_id_1 || ' and ' || p_face_id_2 || ' and edge '
                                 || p_edge_hint);   
                                 
      ELSE                                   
      
         RETURN output;
   
      END IF;
      
   END GET_EDGES_BETWEEN_FACES_HINTED;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------
   
    FUNCTION GET_EDGE_BETWEEN_FACES_HINTED (
      p_topology           IN VARCHAR2,
      p_face_id_1          IN NUMBER,
      p_face_id_2          IN NUMBER,
      p_edge_hint_1        IN NUMBER,
      p_edge_hint_2        IN NUMBER
   ) RETURN NUMBER
   AS
   
      --Matt! 11/08/12
      --Theres more than one edge between these faces.
      --I want only the 1 edge that touch 2 edges I'm confident about
      
      --     
      --         ___    2
      --         |  \
      --   0-----0---0
      --   |   1      |<--want 
      --   |          |
      -----------hint---hint----
      
      output               NUMBER;
      edgez                GZ_TYPES.numberarray;
      
   BEGIN
   
      edgez := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES(p_topology,
                                                 p_face_id_1,
                                                 p_face_id_2);
                                                 
      IF edgez.COUNT = 0
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Didnt find ANY edges with faces ' 
                                 || p_face_id_1 || ' and ' || p_face_id_2);
      
      END IF;
                                                    
      FOR i IN 1 .. edgez.COUNT
      LOOP
      
         IF  GZ_SMPOLY.ARE_EDGES_CONNECTED(p_topology, p_edge_hint_1, edgez(i)) 
         AND GZ_SMPOLY.ARE_EDGES_CONNECTED(p_topology, p_edge_hint_2, edgez(i)) 
         THEN
         
            IF output IS NULL
            THEN
            
               output := edgez(i);
               
            ELSE
            
               RAISE_APPLICATION_ERROR(-20001, 'Found 2 hinted edges, ' || output || ' and ' || edgez(i) 
                                      || ' between '|| p_face_id_1 || ' and ' || p_face_id_2);
            
            END IF;
         
         END IF;
      
      END LOOP;       
      
      IF output IS NULL
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Didnt find an edge with faces ' 
                                 || p_face_id_1 || ' and ' || p_face_id_2 || ' and edges '
                                 || p_edge_hint_1 || ',' || p_edge_hint_2);   
                                 
      ELSE                                   
      
         RETURN output;
   
      END IF;
      
   END GET_EDGE_BETWEEN_FACES_HINTED;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------
   
   FUNCTION GET_OPPOSITE_FACE (
      p_topology           IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_edge_id            IN NUMBER
   ) RETURN NUMBER 
   AS
   
      --Matt! 11/06/12
      
      psql                 VARCHAR2(4000);
      output               NUMBER;
      
   BEGIN
   
      psql := 'SELECT faces '
           || 'FROM ' || p_topology || '_edge$ UNPIVOT (faces '
           || 'FOR face '
           || 'IN (left_face_id, right_face_id)) '
           || 'WHERE '
           || 'edge_id = :p1 AND '
           || 'faces <> :p2 ';
           
      EXECUTE IMMEDIATE psql INTO output USING p_edge_id,
                                               p_face_id;
                                               
      IF output IS NOT NULL
      THEN
      
         RETURN output;
         
      ELSE
      
         RAISE_APPLICATION_ERROR(-20001, 'Didnt get an opposite face for edge ' || p_edge_id
                                       || 'and face ' || p_face_id);
      
      END IF;
   
   END GET_OPPOSITE_FACE;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------
   
   FUNCTION GET_SINGLE_OPPOSITE_FACE (
      p_topology           IN VARCHAR2,
      p_face_id            IN NUMBER
   ) RETURN NUMBER 
   AS
   
      --Matt! 11/16/12
      --2 faces are separated by just one edge
      
      psql                 VARCHAR2(4000);
      output               NUMBER;
      
   BEGIN
   
      psql := 'SELECT faces FROM ( '
           || 'SELECT e.left_face_id, e.right_face_id '
           || 'FROM ' || p_topology || '_edge$ e '
           || 'WHERE '
           || '(e.left_face_id = :p1 or e.right_face_id = :p2) AND '
           || '(left_face_id <> :p3 and right_face_id <> :p4) '
           || ') UNPIVOT (faces FOR face IN (left_face_id, right_face_id)) '
           || 'WHERE faces <> :p5 ';
        
      BEGIN
         
         EXECUTE IMMEDIATE psql INTO output USING p_face_id, p_face_id,
                                                  -1, -1,
                                                  p_face_id;
                                                  
      EXCEPTION
         WHEN TOO_MANY_ROWS 
         THEN
        
            RAISE_APPLICATION_ERROR(-20001,'Face ' || p_face_id || ' touches more than one additional face');
            
         WHEN NO_DATA_FOUND
         THEN
         
            RAISE_APPLICATION_ERROR(-20001,'Face ' || p_face_id || ' doesnt exist');
            
         WHEN OTHERS
         THEN
        
            RAISE_APPLICATION_ERROR(-20001,SQLERRM);
         
      END; 
                                               
      IF output IS NULL
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Face ' || p_face_id || ' has no opposite face');
         
      END IF;
                                               
      RETURN output;
   
   END GET_SINGLE_OPPOSITE_FACE;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------
   
   FUNCTION GET_NODES_FOR_EDGE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER
   ) RETURN GZ_TYPES.numberarray
   AS
   
      --Matt! 11/09/12
      
      output               GZ_TYPES.numberarray;
      startnode            NUMBER;
      endnode              NUMBER;
      psql                 VARCHAR2(4000);
      
   BEGIN
   
      psql := 'SELECT e.start_node_id, e.end_node_id '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';
           
      EXECUTE IMMEDIATE psql INTO startnode,
                                  endnode USING p_edge_id;
             
      IF startnode IS NULL
      THEN
      
         RAISE_APPLICATION_ERROR(-20001,'Bomba! No edge ' || p_edge_id);
      
      END IF;     
                      
      output(1) := startnode;
      output(2) := endnode;
      
      RETURN output;
   
   END GET_NODES_FOR_EDGE;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------
   
   FUNCTION GET_NODE_BETWEEN_EDGES (
      p_topology           IN VARCHAR2,
      p_edge_id_1          IN NUMBER,
      p_edge_id_2          IN NUMBER
   ) RETURN NUMBER 
   AS
   
      --Matt! 11/05/12
      
      --MUST BE
      --          -----X------
      --No loops
      
      psql                 VARCHAR2(4000);
      output               NUMBER;
      
   BEGIN
   
      psql := 'SELECT nodes FROM ( '
           || 'SELECT nodes, COUNT(nodes) kount '
           || 'FROM ' || p_topology || '_edge$ UNPIVOT (nodes '
           || 'FOR node '
           || 'IN (start_node_id, end_node_id)) '
           || 'WHERE edge_id IN (:p1,:p2) '
           || 'GROUP BY nodes '
           || ') WHERE kount = :p3 ';
      
      BEGIN
      
         EXECUTE IMMEDIATE psql INTO output USING p_edge_id_1,
                                                  p_edge_id_2,
                                                  2;
                                                  
      EXCEPTION
      WHEN OTHERS
      THEN
      
          RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' on edges ' || p_edge_id_1 || ',' || p_edge_id_2
                                         || '-- ' || psql ); 
   
      END;
      
      RETURN output;
   
   END GET_NODE_BETWEEN_EDGES;
   
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------
   
   FUNCTION GET_NODE_TIP (
      p_topology           IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_node_id            IN NUMBER
   ) RETURN VARCHAR2 
   AS
   
      --Matt! 11/06/12
      --Is the node the START or the END of the edge
      --no loops allowed      
      
      psql                 VARCHAR2(4000);
      output               VARCHAR2(8);
      
   BEGIN
   
      psql := 'SELECT node '
           || 'FROM ' || p_topology || '_edge$ UNPIVOT (nodes '
           || 'FOR node '
           || 'IN (start_node_id AS :p1, end_node_id AS :p2)) '
           || 'WHERE '
           || 'edge_id = :p3 AND '
           || 'nodes = :p4 ';
      
      BEGIN
      
         EXECUTE IMMEDIATE psql INTO output USING 'START',
                                                  'END',
                                                  p_edge_id,
                                                  p_node_id;
                                                  
      EXCEPTION
      WHEN OTHERS
      THEN
      
          RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' on edge ' || p_edge_id || ', node' || p_node_id
                                         || '-- ' || psql ); 
   
      END;
      
      RETURN output;
   
   END GET_NODE_TIP;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION IS_EDGE_BOUNDED_BY_OBSOLETE (
      p_topology        IN VARCHAR2,
      p_edge_id         IN NUMBER
   ) RETURN BOOLEAN
   AS
   
      --Matt! 11/02/12
      
      psql              VARCHAR2(4000);  
      start_node_id     NUMBER;
      end_node_id       NUMBER;
      kount             PLS_INTEGER;
   
   BEGIN
   
      psql := 'SELECT e.start_node_id, e.end_node_id '
           || 'FROM ' || p_topology || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';
      
      EXECUTE IMMEDIATE psql INTO start_node_id, end_node_id USING p_edge_id;
      
      IF start_node_id = end_node_id
      THEN
      
         --islands are never obsolete
         RETURN FALSE;
      
      END IF;
      
      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_topology || '_edge$ e '
           || 'WHERE :p1 IN '
           || '(e.start_node_id, e.end_node_id) ';
           
      EXECUTE IMMEDIATE psql INTO kount USING start_node_id;
     
      IF kount = 2
      THEN
     
         --lollipop islands?  
         --Should be ok since we tossed start=end above
         --All other lollipop-licke cases should be three-connects (right?) (bug alert here)
         RETURN TRUE;
     
      END IF;
     
      EXECUTE IMMEDIATE psql INTO kount USING end_node_id;
     
      IF kount = 2 
      THEN
     
         RETURN TRUE;
        
      END IF;
     
      RETURN FALSE;   
   
   END IS_EDGE_BOUNDED_BY_OBSOLETE;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------


   FUNCTION IS_OBSOLETE_NODE_BETWEEN_FACES (
      p_topology        IN VARCHAR2,
      p_face_id_1       IN NUMBER,
      p_face_id_2       IN NUMBER
   ) RETURN BOOLEAN
   AS
   
      --Matt! 11/02/12
      --Used to detect disallowed obsolete node case 
   
      psql              VARCHAR2(4000);
      edge_ids          GZ_TYPES.numberarray;
      
   BEGIN
   
      edge_ids := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES(p_topology,
                                                    p_face_id_1,
                                                    p_face_id_2);
                                                              
      IF edge_ids.COUNT = 1
      THEN
      
         --Most of the time here, just 2 faces facing across an edge
         RETURN FALSE;
      
      END IF;
      
      --If more than one, need to be sure that none of the edges in the list 
      --are connected via an obsolete node
      --The faces could also face off at non-connected edges, or at a lollipop
      --   TMI, just check if the edges are bounded by obsoletes
      
      FOR i IN 1 .. edge_ids.COUNT
      LOOP
      
         IF GZ_SMPOLY.IS_EDGE_BOUNDED_BY_OBSOLETE(p_topology,
                                                  edge_ids(i))
         THEN
         
            RETURN TRUE;
            
         END IF;
      
      END LOOP;
      
      RETURN FALSE;      
      
   
   END IS_OBSOLETE_NODE_BETWEEN_FACES;   
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------
   
   FUNCTION ARE_EDGES_CONNECTED (
      p_topology        IN VARCHAR2,
      p_edge_id_1       IN NUMBER,
      p_edge_id_2       IN NUMBER
   ) RETURN BOOLEAN
   AS
   
      --Matt! 11/02/12
      
      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      
   BEGIN   
      
      --what a load of.. got to be something clearer
      --I just don't like 2x index accesses with UNION ALLS
      
      psql := 'WITH '
           || ' FIRST_NODES AS '
           || '   (SELECT nodes '
           || '   FROM ' || p_topology || '_edge$ UNPIVOT (nodes '
           || '   FOR node '
           || '   IN (start_node_id, end_node_id)) '
           || '   WHERE edge_id = :p1), '
           || ' SECOND_NODES AS '
           || '   (SELECT nodes '
           || '   FROM ' || p_topology || '_edge$ UNPIVOT (nodes '
           || '   FOR node '
           || '   IN (start_node_id, end_node_id)) '
           || '   WHERE edge_id = :p2) '
           || 'SELECT COUNT (*) '
           || '   FROM FIRST_NODES '
           || '   WHERE nodes IN (SELECT * FROM SECOND_NODES) ';
           
      EXECUTE IMMEDIATE psql INTO kount USING p_edge_id_1,
                                              p_edge_id_2;
      
      IF kount = 0
      THEN
      
         RETURN FALSE;
         
      ELSE
      
         RETURN TRUE;
         
      END IF;   
    

   END ARE_EDGES_CONNECTED;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------


   FUNCTION ARE_EDGES_BTWN_FACES_CONNECTED (
      p_topology        IN VARCHAR2,
      p_face_id_1       IN NUMBER,
      p_face_id_2       IN NUMBER
   ) RETURN BOOLEAN
   AS
   
      --Matt! 11/02/12
      -- 2 Faces
      -- Something interrupts their border at a node
      -- Could be a triangle face, or some sort of chain of faces, dont care
      
      --  _______________
      --  |  B          |
      --  |     ___     |
      --  |     \ /     |
      --  -------0-------
      --  |             |      
      --  |      A      |
      --  |_____________|
      --

      
   
      psql              VARCHAR2(4000);
      edge_ids          GZ_TYPES.numberarray;
      
   BEGIN
   
      edge_ids := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES(p_topology,
                                                    p_face_id_1,
                                                    p_face_id_2);
                                                    
      IF edge_ids.COUNT <> 2
      THEN
      
         RAISE_APPLICATION_ERROR(-20001,'This should be 2 faces with 2 shared edges');
      
      END IF;
      
      IF GZ_SMPOLY.ARE_EDGES_CONNECTED(p_topology,
                                       edge_ids(1),
                                       edge_ids(2))
      THEN
      
         RETURN TRUE;
         
      ELSE
      
         RETURN FALSE;
      
      END IF;

   
   END ARE_EDGES_BTWN_FACES_CONNECTED;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------


   FUNCTION ARE_THREE_FACES_AT_A_POINT (
      p_topology        IN VARCHAR2,
      p_face_id_1       IN NUMBER,
      p_face_id_2       IN NUMBER,
      p_face_id_3       IN NUMBER
   ) RETURN BOOLEAN
   AS
   
      --Matt! 11/02/12
      
      -- TRUE
      --       |
      --    1  |
      -- ------0  3
      --    2   \
      --         \
      --
      -- FALSE! FN should be named ARE_THREE_FACES_AND_ONLY_THOSE_FACE_JOINT_AT_A_POINT_DEADCOUNT
      --       |
      --    1  |
      -- ------0-----   
      --    2   \  3
      --         \
      --
      -- FALSE
      --        |  3 
      --        |     3
      --        0--0
      --     1  |  |
      --  ------0  |   3
      --     2  |  |
      --        0--0  3
      --        | 
      --        |  3
      --        |
   
      psql              VARCHAR2(4000);
      edge_ids1         GZ_TYPES.numberarray;
      edge_ids2         GZ_TYPES.numberarray;
      edge_ids3         GZ_TYPES.numberarray;
      
   BEGIN
   
      edge_ids1 := GET_EDGES_BETWEEN_FACES(p_topology,
                                           p_face_id_1,
                                           p_face_id_2);
                                          
      IF edge_ids1.COUNT > 1
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Unexpected, these faces are connected in more than one spot: ' 
                                         || p_face_id_1 || ',' || p_face_id_2);
                                         
      ELSIF edge_ids1.COUNT = 0
      THEN
      
         RETURN FALSE;
      
      END IF;
      
      edge_ids2 := GET_EDGES_BETWEEN_FACES(p_topology,
                                           p_face_id_2,
                                           p_face_id_3);
                                          
      IF edge_ids2.COUNT > 1
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Unexpected, these faces are connected in more than one spot: ' 
                                         || p_face_id_2 || ',' || p_face_id_3);
      
      ELSIF edge_ids2.COUNT = 0
      THEN
      
         RETURN FALSE;
      
      END IF;

      
      edge_ids3 := GET_EDGES_BETWEEN_FACES(p_topology,
                                           p_face_id_1,
                                           p_face_id_3);
                                          
      IF edge_ids3.COUNT > 1
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Unexpected, these faces are connected in more than one spot: ' 
                                         || p_face_id_1 || ',' || p_face_id_3);
                                         
      ELSIF edge_ids3.COUNT = 0
      THEN
      
         RETURN FALSE;
      
      END IF;

      
      IF  ARE_EDGES_CONNECTED(p_topology, edge_ids1(1), edge_ids2(1))
      AND ARE_EDGES_CONNECTED(p_topology, edge_ids2(1), edge_ids3(1))
      AND ARE_EDGES_CONNECTED(p_topology, edge_ids1(1), edge_ids3(1))
      THEN
      
         RETURN TRUE;
         
      ELSE
      
         RETURN FALSE;
         
      END IF;   
   
   END ARE_THREE_FACES_AT_A_POINT;
    
      
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------
   
   FUNCTION ARE_3_EDGES_A_SIMPLE_CHAIN (
      p_topology        IN VARCHAR2,
      p_edges           IN GZ_TYPES.NUMBERARRAY
   ) RETURN BOOLEAN
   AS
     
      --Matt! 11/05/12
      --Started writing a generic chainer but its gonna take too long
      --Just assume 3 edges and no more
      
      --Should have 6 total start/end nodes
      --2 nodes are dupes (2 x 2) = 4
      --2 nodes are unique endpoints
      
      --Any kind of funny business, including loops, will return false
      
      psql              VARCHAR2(4000);
      kounts            GZ_TYPES.numberarray;
      
   
   BEGIN
   
      IF p_edges.COUNT <> 3
      THEN
      
         RAISE_APPLICATION_ERROR(-20001,'Expect three edges, got ' || p_edges.COUNT);
      
      END IF;
      
      psql := 'SELECT count(nodes) '
           || 'FROM ' || p_topology || '_edge$ UNPIVOT (nodes '
           || 'FOR node '
           || 'IN (start_node_id, end_node_id)) '
           || 'WHERE edge_id IN (SELECT * FROM TABLE(:p1)) '
           || 'GROUP BY nodes '
           || 'ORDER BY COUNT(nodes) ';
           
      --77770   1
      --77769   1
      --77772   2
      --77771   2
           
      EXECUTE IMMEDIATE psql BULK COLLECT INTO kounts USING GZ_UTILITIES.NUMARRAY_TO_VARRAY(p_edges);
      
      IF kounts.COUNT = 4
      AND kounts(1) = 1 
      AND kounts(2) = 1
      AND kounts(3) = 2
      AND kounts(4) = 2
      THEN
                                                    
         RETURN TRUE;
         
      ELSE
      
         RETURN FALSE;
        
      END IF;
   
   
   END ARE_3_EDGES_A_SIMPLE_CHAIN;  
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------
   
   FUNCTION IS_AC_BC_AC (
      p_topology        IN VARCHAR2,
      p_face_id_target  IN NUMBER,
      p_face_id_2       IN NUMBER,
      p_face_id_3       IN NUMBER
   ) RETURN BOOLEAN
   AS
   
      --Matt! 11/05/12
      
      --  _______________
      --  |  A          |
      --  |    ____     |
      --  |   | B  |    |
      --  ----0----0-----
      --  |             |      
      --  |      C      |
      --  |_____________|
      --
      
      psql              VARCHAR2(4000);
      edge_ids1         GZ_TYPES.numberarray;
      edge_ids2         GZ_TYPES.numberarray;
      all_edges         GZ_TYPES.numberarray;
      
   BEGIN
   
   
      edge_ids1 := GET_EDGES_BETWEEN_FACES(p_topology,
                                           p_face_id_target,
                                           p_face_id_2);
      
      edge_ids2 := GET_EDGES_BETWEEN_FACES(p_topology,
                                           p_face_id_target,
                                           p_face_id_3);
                                           
      IF (edge_ids1.COUNT > 1 AND edge_ids2.COUNT > 1)
      OR (edge_ids1.COUNT = 0 OR edge_ids2.COUNT = 0)
      THEN
       
         --Theres probably a possibility of both counts > 1 being ACBCAC
         --Ie an extra touching section. But Im not allowing it for now
         RETURN FALSE;
      
      END IF;
      
      all_edges := NUMBERARRAY_ADD_UNIQUE(edge_ids1,
                                          edge_ids2);
                                          
      IF GZ_SMPOLY.ARE_3_EDGES_A_SIMPLE_CHAIN(p_topology,
                                              all_edges)
      THEN
      
         RETURN TRUE;
         
      ELSE
      
         RETURN FALSE;
         
      END IF;

   
   END IS_AC_BC_AC; 
     
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION COUNT_EDGES_BETWEEN_FACES (
      p_topology        IN VARCHAR2,
      p_face_id_1       IN NUMBER,
      p_face_id_2       IN NUMBER
   ) RETURN PLS_INTEGER
   AS
   
      --Matt! 11/02/12
   
      psql              VARCHAR2(4000);
      edge_ids          GZ_TYPES.numberarray;
      
   BEGIN
   
      edge_ids := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES(p_topology,
                                                    p_face_id_1,
                                                    p_face_id_2);
                                                    
      RETURN edge_ids.COUNT;
   
   END COUNT_EDGES_BETWEEN_FACES;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GET_TG_LAYER_LEVEL (
      p_topology           IN VARCHAR2,
      p_table_name         IN VARCHAR2
   ) RETURN NUMBER
   AS

      --Matt! 10/09/12

      psql              VARCHAR2(4000);
      tg_layer_level    NUMBER;


   BEGIN

      psql := 'SELECT tg_layer_level '
           || 'FROM user_sdo_topo_info a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.table_name = :p2 ';

      EXECUTE IMMEDIATE psql INTO tg_layer_level USING UPPER(p_topology),
                                                       UPPER(p_table_name);

      IF tg_layer_level IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Found no tg_layer_level for topo ' || p_topology || ' fsl ' || p_table_name);

      END IF;

      RETURN tg_layer_level;


   END GET_TG_LAYER_LEVEL;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GET_FSLS (
      p_topology           IN VARCHAR2,
      p_greater_level      IN NUMBER DEFAULT NULL,
      p_allow_nuthin       IN VARCHAR2 DEFAULT 'N'
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 10/09/12

      feature_tables       GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);

   BEGIN

      psql := 'SELECT a.table_name '
           || 'FROM user_sdo_topo_info a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.tg_layer_type = :p2 ';

      IF p_greater_level IS NOT NULL
      THEN

         psql := psql || 'AND a.tg_layer_level > ' || p_greater_level || ' ';

      END IF;

      psql := psql || 'ORDER BY a.tg_layer_level DESC ';  --constancy.  Sweet constancy

      EXECUTE IMMEDIATE psql BULK COLLECT INTO feature_tables USING UPPER(p_topology),
                                                                    'POLYGON';

      IF feature_tables.COUNT = 0
      AND p_allow_nuthin = 'N'
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'No feature tables registered for topo ' || UPPER(p_topology));

      END IF;


      RETURN feature_tables;


   END GET_FSLS;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GET_FSLS_HIERARCHY (
      p_topology           IN VARCHAR2,
      p_table              IN VARCHAR2
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 10/22/12

      feature_tables       GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);

   BEGIN

      psql := 'SELECT a.table_name '
           || 'FROM user_sdo_topo_info a '
           || 'START WITH '
           || 'a.table_name = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.column_name = :p3 '
           || 'CONNECT BY PRIOR '
           || 'a.child_layer_id = a.tg_layer_id AND '
           || 'a.topology = :p4 AND '
           || 'a.column_name = :p5 '
           || 'ORDER BY tg_layer_level DESC ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO feature_tables USING UPPER(p_table),
                                                                    UPPER(p_topology),
                                                                    'TOPOGEOM',
                                                                    UPPER(p_topology),
                                                                    'TOPOGEOM';

      IF feature_tables.COUNT <= 1
      THEN

         --should return at least itself and 1 more
         RAISE_APPLICATION_ERROR(-20001, 'No hierarchy feature tables registered for topo ' || p_topology || ' table ' || p_table );

      END IF;


      RETURN feature_tables;


   END GET_FSLS_HIERARCHY;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GET_PARENT_TABLES (
      p_topology           IN VARCHAR2,
      p_child_table        IN VARCHAR2,
      p_paternity_test     IN VARCHAR2 DEFAULT 'N'
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 10/23/12

      feature_tables       GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);

   BEGIN

      psql := 'SELECT b.table_name '
           || 'FROM '
           || 'user_sdo_topo_info a, '
           || 'user_sdo_topo_info b '
           || 'WHERE '
           || 'a.table_name = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.tg_layer_type = :p3 AND '
           || 'a.column_name = :p4 AND '
           || 'b.topology = :p5 AND '
           || 'b.tg_layer_type = :p6 AND '
           || 'b.column_name = :p7 AND '
           || 'a.tg_layer_id = b.child_layer_id ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO feature_tables USING UPPER(p_child_table),
                                                                    UPPER(p_topology),
                                                                    'POLYGON',
                                                                    'TOPOGEOM',
                                                                    UPPER(p_topology),
                                                                    'POLYGON',
                                                                    'TOPOGEOM';

      IF feature_tables.COUNT = 0
      AND p_paternity_test = 'Y'  --we are here because we know theres a parent table
      THEN

         RAISE_APPLICATION_ERROR(-20001,'No parents for ' || UPPER(p_child_table) || ' ' || UPPER(p_topology));

      END IF;

      RETURN feature_tables;

   END GET_PARENT_TABLES;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GET_PARENT_OF_CHILD (
      p_topology           IN VARCHAR2,
      p_child_table        IN VARCHAR2,
      p_child_key_col      IN VARCHAR2,
      p_child_key          IN VARCHAR2,
      p_parent_table       IN VARCHAR2,
      p_parent_key_col     IN VARCHAR2,
      p_parent_exists      IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2
   AS

      --Matt! 10/23/12
      --Not sure about performance here

      psql              VARCHAR2(4000);
      output            VARCHAR2(4000);

   BEGIN

      psql := 'SELECT a.' || p_parent_key_col || ' '
           || 'FROM ' || p_parent_table || ' a, '
           || 'TABLE(a.topogeom.get_tgl_objects()) t, '
           || p_child_table || ' b '
           || 'WHERE '
           || 't.tgl_id = b.topogeom.tg_layer_id AND '
           || 't.tg_id = b.topogeom.tg_id AND '
           || 'b.' || p_child_key_col || ' = :p1 ';

      EXECUTE IMMEDIATE psql INTO output USING p_child_key;

      IF output IS NULL
      AND p_parent_exists = 'Y'
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Didnt find a parent for ' || p_child_table || ' '
                                        || p_child_key_col || ' ' || p_child_key);

      END IF;

      RETURN output;

   END GET_PARENT_OF_CHILD;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION COUNT_PARENT_TGL_OBJECTS (
      p_topology           IN VARCHAR2,
      p_parent_table       IN VARCHAR2,
      p_parent_key_col     IN VARCHAR2,
      p_parent_key         IN VARCHAR2
   ) RETURN PLS_INTEGER
   AS

      --Matt! 10/23/12

      psql                 VARCHAR2(4000);
      output               PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(t.tg_id) '
           || 'FROM ' || p_parent_table || ' a, '
           || 'TABLE(a.topogeom.get_tgl_objects()) t '
           || 'WHERE a.' || p_parent_key_col || ' = :p1 ';

      EXECUTE IMMEDIATE psql INTO output USING p_parent_key;

      IF output = 0
      OR output IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'No objects for ' || p_parent_table || ' '
                                       || p_parent_key_col || ' ' || p_parent_key);

      END IF;

      RETURN output;

   END COUNT_PARENT_TGL_OBJECTS;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GET_CHILD_TGL_OBJECT (
      p_child_table        IN VARCHAR2,
      p_child_key_col      IN VARCHAR2,
      p_child_key          IN VARCHAR2
   ) RETURN SDO_TGL_OBJECT
   AS

      --Matt! 10/24/12
      --Must be a child with a single tgl_object pointing to a parent
      --We are deleting the child

      output               SDO_TGL_OBJECT;
      psql                 VARCHAR2(4000);
      my_tg_id             NUMBER;
      my_tg_layer_id       NUMBER;


   BEGIN

      psql := 'SELECT a.topogeom.tg_id, a.topogeom.tg_layer_id '
           || 'FROM ' || p_child_table || ' a '
           || 'WHERE '
           || 'a.' || p_child_key_col || ' = :p1 ';

      EXECUTE IMMEDIATE psql INTO my_tg_id,
                                  my_tg_layer_id USING p_child_key;

      IF my_tg_id IS NULL
      OR my_tg_layer_id IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Null tg_id, tg_layer_id for ' || p_child_table || ' ' || p_child_key_col || ' ' || p_child_key);

      END IF;

      output := SDO_TGL_OBJECT(my_tg_layer_id, my_tg_id);

      RETURN output;

   END GET_CHILD_TGL_OBJECT;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GZ_GET_GEOID_FROM_FACE (
      p_topology        IN VARCHAR2,
      p_fsl_tab         IN VARCHAR2,
      p_layer_level     IN NUMBER,
      p_face_id         IN NUMBER,
      p_fsl_key_col     IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 10/10/12
      --Wrapper to gz_build_source.gz_get_oid_from_face

      output            VARCHAR2(4000);

   BEGIN

      output := gz_build_source.GZ_GET_OID_FROM_FACE(USER,
                                                     p_topology,
                                                     p_fsl_tab,
                                                     p_layer_level,
                                                     p_face_id,
                                                     '1=1',      --always expects whereclause
                                                     p_fsl_key_col);

      RETURN output;

   EXCEPTION
   WHEN OTHERS
   THEN

      IF SQLERRM LIKE '%no data found%'
      THEN

         --ORA-20001: ORA-01403: no data found
         RETURN '';

      ELSE

         RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

      END IF;

   END;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GZ_GET_FSL_FACES (
      p_topology        IN VARCHAR2,
      p_fsl_tab         IN VARCHAR2,
      p_fsl_geo_id      IN VARCHAR2,
      p_tg_layer_level  IN NUMBER,
      p_fsl_pkc_col     IN VARCHAR2 DEFAULT 'GEO_ID',
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.NUMBERARRAY
   AS

      --Matt!  11/15/11
      --Modified from the version in GZ_UTILITIES 10/09/12
      --
      --DECLARE
      -- face_ids gz_types.stringarray;
      --BEGIN
      -- face_ids := GZ_UTILITIES.GZ_GET_FSL_FACES('Z601LS_FSL050',27590310671100);
      --END;

      psql              VARCHAR2(4000);
      output            GZ_TYPES.numberarray;


   BEGIN



      CASE p_tg_layer_level

         WHEN 0 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || p_topology || '_relation$ r1, '
                 || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_pkc_col || ' = :p1 ';

         WHEN 1 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || p_topology || '_relation$ r1, '
                 || p_topology || '_relation$ r2, '
                 || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_pkc_col || ' = :p1 ';

         WHEN 2 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || p_topology || '_relation$ r1, '
                 || p_topology || '_relation$ r2, '
                 || p_topology || '_relation$ r3, '
                 || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_pkc_col || ' = :p1 ';

         WHEN 3 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || p_topology || '_relation$ r1, '
                 || p_topology || '_relation$ r2, '
                 || p_topology || '_relation$ r3, '
                 || p_topology || '_relation$ r4, '
                 || p_topology || '_face$ f '
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
                 || 'a.' || p_fsl_pkc_col || ' = :p1 ';

         WHEN 4 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || p_topology || '_relation$ r1, '
                 || p_topology || '_relation$ r2, '
                 || p_topology || '_relation$ r3, '
                 || p_topology || '_relation$ r4, '
                 || p_topology || '_relation$ r5, '
                 || p_topology || '_face$ f '
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
                 || 'a.' || p_fsl_pkc_col || ' = :p1 ';

         WHEN 5 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || p_topology || '_relation$ r1, '
                 || p_topology || '_relation$ r2, '
                 || p_topology || '_relation$ r3, '
                 || p_topology || '_relation$ r4, '
                 || p_topology || '_relation$ r5, '
                 || p_topology || '_relation$ r6, '
                 || p_topology || '_face$ f '
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
                 || 'a.' || p_fsl_pkc_col || ' = :p1 ';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Hierarchy level ' || p_tg_layer_level || ' is yet to be implemented!');

      END CASE;

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING p_fsl_geo_id;


      --is output of nothing OK?  Lets say no for now

      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Didnt find any faces for ' || p_fsl_tab || ' ' || p_fsl_pkc_col || ' ' || p_fsl_geo_id);

      END IF;

      RETURN output;


   END GZ_GET_FSL_FACES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION GET_EXPENDABLE_FSLS (
      p_release         IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_expendable_tab  IN VARCHAR2,
      p_what            IN VARCHAR2 --DONT_CHECK or WHERE_CLAUSE
   ) RETURN GZ_TYPES.stringhash
   AS

      --Matt! 10/10/12

      psql        VARCHAR2(4000);
      fsls        GZ_TYPES.stringarray;
      vals        GZ_TYPES.stringarray;
      output      GZ_TYPES.stringhash;
      face_tab    VARCHAR2(32);

   BEGIN

      --tack on face table first in case theres no explicit expendable table
      IF p_what = 'DONT_CHECK'
      THEN

         --tack on the face table
         psql := 'SELECT a.table_name '
              || 'FROM '
              || 'user_sdo_topo_info a '
              || 'WHERE '
              || 'a.topology = :p1 AND '
              || 'a.tg_layer_type = :p2 AND '
              || 'a.table_name NOT LIKE :p3 ';

         --assuming return 1 value
         EXECUTE IMMEDIATE psql INTO face_tab USING UPPER(p_topology),
                                                    'POLYGON',
                                                    '%FSL%';

         output(face_tab) := 'Y';

      END IF;

      IF p_expendable_tab IS NULL
      THEN

         RETURN output;

      END IF;

      psql := 'SELECT ''' || p_topology || '_FSL'' || a.layer || ''V'', ';

      IF p_what = 'WHERE_CLAUSE'
      THEN

         psql := psql || 'a.where_clause ';


      ELSIF p_what = 'DONT_CHECK'
      THEN

         psql := psql || '''Y'' ';

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'What the what is a ' || p_what || '?');

      END IF;

      psql := psql || 'FROM '
                   || p_expendable_tab || ' a '
                   || 'WHERE '
                   || 'a.release = :p1 AND '
                   || 'a.gen_project_id = :p2 AND ';

      IF p_what = 'WHERE_CLAUSE'
      THEN

          psql := psql || 'a.where_clause IS NOT NULL ';

      ELSE

          psql := psql || 'a.dont_check = ''Y''';

      END IF;


      BEGIN

         EXECUTE IMMEDIATE psql BULK COLLECT INTO fsls,
                                                  vals USING UPPER(p_release),
                                                             UPPER(p_project_id);

      EXCEPTION
      WHEN OTHERS
      THEN

         RETURN output;

      END;

      FOR i IN 1 .. fsls.COUNT
      LOOP

         output(fsls(i)) := vals(i);

      END LOOP;

      RETURN output;

   END GET_EXPENDABLE_FSLS;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION IS_GEOID_EXPENDABLE (
      p_release         IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_layer           IN VARCHAR2,
      p_pkc_col         IN VARCHAR2,
      p_geoid           IN VARCHAR2,
      p_where_clause    IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt!  10/09/12

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;


   BEGIN


      IF p_where_clause IS NULL
      THEN

         RETURN 'FALSE';

      ELSE

         psql := 'SELECT COUNT(*) FROM '
              || p_layer || ' a '
              || 'WHERE a.' || p_pkc_col || ' = :p1 AND '
              || '(' || p_where_clause || ')';

         EXECUTE IMMEDIATE psql INTO kount USING p_geoid;

         IF kount = 1
         THEN

            RETURN 'TRUE';

         ELSIF kount = 0
         THEN

            RETURN 'FALSE';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'WTF on ' || psql);

         END IF;


      END IF;


   END IS_GEOID_EXPENDABLE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------


   FUNCTION FSLS_ENDANGERED (
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_topology           IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_fsl_pkc            IN VARCHAR2 DEFAULT 'GEO_ID',
      p_expendable_tab     IN VARCHAR2 DEFAULT NULL,
      p_debug              IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt!  10/09/12
      --Tell me if a primitive face ID represents the last living territory in a feature summary level record

      --p_release            Standard GZ release.  Only tie in at present is to the expendable table
      --p_project_id         Standard GZ project.  Only tie in at present is to the expendable table
      --p_topology           In executing schema
      --p_face_id            Face ID that is probably a sliver or we want to get rid of it
      --p_fsl_pkc            primary key on the FLSs. Pretty much always GEO_ID
      --p_expendable_tab     Helper table containing layers and whereclauses for ugly critters that we dont mind going extinct
      --                     Or where we just dont want to bother checking (like NATION)
      --                     For example all water School Districts and Nation might look like
      --                     RELEASE   GEN_PROJECT_ID   LAYER   DONT_CHECK   WHERECLAUSE
      --                     ACS12     Z6               976                  a.sduni IN ('00000','99999');
      --                     ACS12     Z6               010     Y

      --sample:
      --select gz_smpoly.fsls_endangered('ACS12','Z6','Z699IN',2959,'GEO_ID','Z699IN_EXPENDABLE_FSLS') from dual
      --returns --> Z699IN_FSL160V  1600000US0954660

      --For now:
      --create table Z699IN_EXPENDABLE_FSLS
      --(release VARCHAR2(64), gen_project_id VARCHAR2(4), layer varchar2(30), dont_check varchar2(1), where_clause VARCHAR2(4000));

      --Find me some endangered FLSs quicklike:
      --select a.geo_id, count(t.topo_id) from z699in_fsl160v a,
      --table(a.topogeom.get_topo_elements()) t
      --group by a.geo_id
      --order by count(t.topo_id) asc

      psql                 VARCHAR2(4000);
      feature_tables       GZ_TYPES.stringarray;
      output               VARCHAR2(4000);
      current_layer_level  NUMBER;
      current_pkc_col      VARCHAR2(32);
      current_key          VARCHAR2(4000);
      current_facez        GZ_TYPES.numberarray;
      expendable           VARCHAR2(32);
      expendable_hash      GZ_TYPES.stringhash;
      dont_check_hash      GZ_TYPES.stringhash;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FSLS_ENDANGERED: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      feature_tables := GZ_SMPOLY.GET_FSLS(p_topology);

      --if table name is null these helpers return helpful null hashes

      --will always include the face table
      dont_check_hash := GZ_SMPOLY.GET_EXPENDABLE_FSLS(p_release,
                                                       p_project_id,
                                                       p_topology,
                                                       p_expendable_tab,
                                                       'DONT_CHECK');

      expendable_hash := GZ_SMPOLY.GET_EXPENDABLE_FSLS(p_release,
                                                       p_project_id,
                                                       p_topology,
                                                       p_expendable_tab,
                                                       'WHERE_CLAUSE');

      FOR i IN 1 .. feature_tables.COUNT
      LOOP

         IF p_debug = 1
         THEN
            dbms_output.put_line('checking ' || feature_tables(i));
         END IF;

         expendable := 'FALSE';

         IF NOT dont_check_hash.EXISTS(feature_tables(i))
         THEN

            current_layer_level := GZ_SMPOLY.GET_TG_LAYER_LEVEL(p_topology,
                                                                feature_tables(i));

            current_pkc_col := p_fsl_pkc; --GEO_ID

            current_key := GZ_SMPOLY.GZ_GET_GEOID_FROM_FACE(p_topology,
                                                            feature_tables(i),
                                                            current_layer_level,
                                                            p_face_id,
                                                            current_pkc_col);

            IF current_key IS NOT NULL
            THEN

               IF p_debug = 1
               THEN
                  dbms_output.put_line('current key is ' || current_key);
               END IF;

               --we now have the record for our targeted face on this layer
               --do we care about this cute little Axolotl Salamander or is it just an ugly Colombian bat?

               IF expendable_hash.EXISTS(feature_tables(i))
               THEN

                  --return TRUE if we dont care about it
                  expendable := GZ_SMPOLY.IS_GEOID_EXPENDABLE(p_release,
                                                              p_project_id,
                                                              p_topology,
                                                              feature_tables(i),
                                                              current_pkc_col,
                                                              current_key,
                                                              expendable_hash(feature_tables(i))); --pass in the where clause



               END IF;

               IF p_debug = 1
               THEN
                  dbms_output.put_line('expendable is ' || expendable);
               END IF;

               IF expendable = 'FALSE'  --either no where clause at all to check out, or we checked and its not an expendable record
               THEN                     --so continue to the face count check

                  current_facez := GZ_SMPOLY.GZ_GET_FSL_FACES(p_topology,
                                                              feature_tables(i),
                                                              current_key,
                                                              current_layer_level,
                                                              current_pkc_col,
                                                              'TOPOGEOM');
                  IF p_debug = 1
                  THEN
                     dbms_output.put_line('current face count is ' || current_facez.COUNT);
                  END IF;

                  ----------------------------------------------------------
                  --  The only spot where the output gets pieced together --
                  ----------------------------------------------------------

                  IF current_facez.COUNT = 1
                  AND current_facez(1) = p_face_id
                  THEN

                     IF output IS NULL
                     THEN

                        output := feature_tables(i) || '  ' || current_key;

                     ELSE

                       output := output || ' | ' || feature_tables(i) || '  ' || current_key;

                     END IF;

                  ELSIF current_facez.COUNT = 1
                  AND current_facez(1) <> p_face_id
                  THEN

                     RAISE_APPLICATION_ERROR(-20001,'Got mismatched single face id ' || current_facez(1) ||
                                                    ' for ' || feature_tables(i) || ' ' || current_pkc_col || ' ' || current_key);

                  ELSE

                     --just to make it obvious.
                     NULL;

                  END IF;

               END IF;

            END IF;  --no current key, this layer doesnt include this face

         END IF; --dont check hash ... on to the next layer

      END LOOP;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 100');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FSLS_ENDANGERED: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END FSLS_ENDANGERED;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------


   PROCEDURE START_MERGEFACE_LOGGING (
      p_topo           IN VARCHAR2,
      p_log_type       IN VARCHAR2, --MERGEFACE if standalone
      transaction_id   IN VARCHAR2
   )
   AS

      --Matt! 10/12/21
      --Create logging table (or add to it) for a special standalone MERGEFACE job
      --otherwise just insert a record as part of a GZ module (SP, CLIP, etc)

   BEGIN

      IF p_log_type = 'MERGEFACE'
      THEN

         --special logic for standalone mergeface usage
         --check if table exists first, otherwise user creates and drops tracking table
         --over and over on each face merge

         IF NOT GZ_UTILITIES.GZ_TABLE_EXISTS(p_topo || '_MERGEFACE_TRACKING')
         THEN

            GZ_UTILITIES.CREATE_GEN_XTEND_TRACKING_LOG(SYS_CONTEXT('USERENV', 'CURRENT_USER'),
                                                       p_topo || '_MERGEFACE_TRACKING');

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   transaction_id,
                                                   NULL,
                                                   'STARTING transaction: ' || transaction_id);

         ELSE

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   transaction_id,
                                                   NULL,
                                                   'STARTING transaction: ' || transaction_id);
         END IF;

      ELSE

         --insert the first record for some production module

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                transaction_id,
                                                NULL,
                                                'STARTING transaction: ' || transaction_id);

      END IF;

   END START_MERGEFACE_LOGGING;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   PROCEDURE DELETE_PARENT_TGL_OBJECT (
      p_topology           IN VARCHAR2,
      p_child_table        IN VARCHAR2,
      p_child_key          IN VARCHAR2,
      p_child_key_col      IN VARCHAR2,
      p_parent_key_col     IN VARCHAR2,
      p_must_delete        IN VARCHAR2 DEFAULT 'Y',
      p_depth              IN NUMBER DEFAULT 0
   )
   AS

      --Matt! 10/23/12
      --We wish to delete an entire record in a low level of the topo hierarchy (no faces will remain)
      --   Delete requires we pay attention to parent feature layers
      --who a single level up in the hierarchy has a relationship to our child?  May be several layers
      --which specific record at that one level up has the topo_id, topo_type matching the child?
      --   If none, do nothing (or error)
      --   If more than one record on a feature layer, definitely an error. Indicates overlap
      --If one parent record per layer, either
      --  use constructor with special SDO_TGL_OBJECT_ARRAY to subtract the relationship to the child
      --  if theres only one sdo_tgl_object in the parent we are deleting, call recursively to next parent up
      --     then delete the current parent

      psql                    VARCHAR2(4000);
      parent_tables           GZ_TYPES.stringarray;
      parent_key              VARCHAR2(4000);
      parent_objects_kount    PLS_INTEGER;
      parent_tgl              SDO_TGL_OBJECT;
      parent_tg_layer_id      NUMBER;


   BEGIN

      IF p_depth > 10
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Topology prob doesnt have ' || p_depth || ' layer levels');

      END IF;

      --who a single level up in the hierarchy has a relationship to our child?  May be several layers

      parent_tables := GZ_SMPOLY.GET_PARENT_TABLES(p_topology,
                                                   p_child_table);

      FOR i IN 1 .. parent_tables.COUNT
      LOOP

         --which specific record at that one level up has the topo_id, topo_type matching the child?
         parent_key := GZ_SMPOLY.GET_PARENT_OF_CHILD(p_topology,
                                                     p_child_table,
                                                     p_child_key_col,
                                                     p_child_key,
                                                     parent_tables(i),
                                                     p_parent_key_col,
                                                     p_must_delete);  --this flag when Y will cause an error if nothing found

         parent_objects_kount := GZ_SMPOLY.COUNT_PARENT_TGL_OBJECTS(p_topology,
                                                                    parent_tables(i),
                                                                    p_parent_key_col,
                                                                    parent_key);

         IF parent_objects_kount > 1
         THEN

            --use constructor with special SDO_TGL_OBJECT_ARRAY to subtract the relationsip

            --REMEMBER we are deleting the child, not reshaping it, thats why we are here
            --so we know theres just one tgl pointer
            parent_tgl := GZ_SMPOLY.GET_CHILD_TGL_OBJECT(p_child_table,
                                                         p_child_key_col,
                                                         p_child_key);

            parent_tg_layer_id := GZ_UTILITIES.GET_TG_LAYER_ID(p_topology,
                                                               parent_tables(i),
                                                               'TOPOGEOM',
                                                               'POLYGON');

            --constructor on parent
            psql := 'UPDATE ' || parent_tables(i) || ' b '
                 || 'SET b.topogeom = SDO_TOPO_GEOMETRY( '
                 || ':p1, '                                 --topology
                 || ':p2, '                                 --topology geometry type
                 || ':p3, '                                 --parent tg_layer_id
                 || 'NULL, '                                --No ids to add
                 || 'SDO_TGL_OBJECT_ARRAY(:p4) '            --one id to delete
                 || ') '
                 || 'WHERE b.' || p_parent_key_col || ' = :p5 ';

            EXECUTE IMMEDIATE psql USING p_topology,
                                         3,
                                         parent_tg_layer_id,
                                         parent_tgl,
                                         parent_key;

            COMMIT;

         ELSE

            --if theres only one sdo_tgl_object in the guy we are deleting, call recursively to next parent up
            GZ_SMPOLY.DELETE_PARENT_TGL_OBJECT(p_topology,
                                               parent_tables(i),  --parent --> child
                                               parent_key,
                                               p_parent_key_col,
                                               p_parent_key_col,
                                               'N',               --we dont know for sure if a parent even exists
                                               (p_depth + 1));    --protect from inifinite recursion

            --Now should be safe to delete the entire record
            psql := 'DELETE FROM ' || parent_tables(i) || ' a '
                 || 'WHERE a.' || p_parent_key_col || ' = :p1 ';

            EXECUTE IMMEDIATE psql USING parent_key;

            COMMIT;

         END IF;

      END LOOP;


   END DELETE_PARENT_TGL_OBJECT;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION GET_FSLS_FOR_FACE (
      p_topology           IN VARCHAR2,
      p_face_id            IN VARCHAR2,
      p_fsl_pkc            IN VARCHAR2 DEFAULT 'GEO_ID'
   ) RETURN GZ_TYPES.stringhash
   AS

      --Matt! 10/15/12
      --return key val hash of feature tables and the pkc record that contains this face
      -- Z699IN_FSL040V = 0400000US50
      -- Z699IN_FSL976V = 9760000US3300707050

      output               GZ_TYPES.stringhash;
      feature_tables       GZ_TYPES.stringarray;
      current_layer_level  NUMBER;
      current_key          VARCHAR2(4000);

   BEGIN

      feature_tables := GZ_SMPOLY.GET_FSLS(p_topology);

      FOR i IN 1 .. feature_tables.COUNT
      LOOP

         current_layer_level := GZ_SMPOLY.GET_TG_LAYER_LEVEL(p_topology,
                                                             feature_tables(i));

         IF feature_tables(i) NOT LIKE '%FACE'
         THEN

            current_key := GZ_SMPOLY.GZ_GET_GEOID_FROM_FACE(p_topology,
                                                            feature_tables(i),
                                                            current_layer_level,
                                                            p_face_id,
                                                            p_fsl_pkc);

         ELSE

            current_key := GZ_SMPOLY.GZ_GET_GEOID_FROM_FACE(p_topology,
                                                            feature_tables(i),
                                                            current_layer_level,
                                                            p_face_id,
                                                            'FACE_ID');

         END IF;

         IF current_key IS NOT NULL  --subs above return NULL if no face for the feature layer
         THEN

            output(feature_tables(i)) := current_key;

         END IF;

      END LOOP;

      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Unexpected workflow, no feature tables found for face ' || p_face_id);

      END IF;

      RETURN output;

   END GET_FSLS_FOR_FACE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION DELETE_A_FACE (
      p_topology           IN VARCHAR2,
      p_feature_table      IN VARCHAR2,
      p_key                IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_key_col            IN VARCHAR2,  --caller should pass in GEO_ID or FACE_ID usually
      p_parent_key_col     IN VARCHAR2 DEFAULT 'GEO_ID'
   ) RETURN NUMBER
   AS

      --Matt! 10/15/12
      --Based on GZ_TOPO_MERGE.DELETE_A_FACE
      --This one return 1 for deleted a face, 0 for feature extinction

      psql                 VARCHAR2(4000);
      face_kount           PLS_INTEGER;
      tg_layer_id          NUMBER;
      output               NUMBER;


   BEGIN

      psql := 'SELECT COUNT(t.topo_id) FROM '
           || p_feature_table || ' a, '
           || 'TABLE(a.topogeom.get_topo_elements()) t '
           || 'WHERE '
           || 'a.' || p_key_col || ' = :p1 ';


      EXECUTE IMMEDIATE psql INTO face_kount USING p_key;

      IF face_kount > 1
      THEN

         output := 1;

         tg_layer_id := GZ_UTILITIES.GET_TG_LAYER_ID(p_topology,
                                                     p_feature_table,
                                                     'TOPOGEOM',
                                                     'POLYGON');


         --we may use the constructor, leaving at least one face with the feature
         --other than the one we are deleting

         psql := 'UPDATE ' ||  p_feature_table || ' a '
              || 'SET '
              || 'a.topogeom = '  --Topology name, topo geom type, tg_layer_id, no topo to add, topo to delete (face_id, type)
              || 'SDO_TOPO_GEOMETRY(:p1, :p2, :p3, NULL, SDO_TOPO_OBJECT_ARRAY(SDO_TOPO_OBJECT(:p4,:p5))) '
              || 'WHERE a.' || p_key_col || ' = :p6 ';

         EXECUTE IMMEDIATE psql USING p_topology,
                                      3,
                                      tg_layer_id,
                                      p_face_id,
                                      3,
                                      p_key;

         COMMIT;

      ELSIF face_kount = 1
      THEN

         output := 0;

         --as far as I know this is best practice
         --Just delete the feature and the topogeom takes care of itself

         psql := 'DELETE FROM ' || p_feature_table || ' a '
              || 'WHERE a.' || p_key_col || ' = :p1 ';

         BEGIN

            EXECUTE IMMEDIATE psql USING p_key;

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%Cannot delete a TG object with dependent parent objects%'
            THEN

               --For simple shape changes we keep track of higher level parents
               --And update their measurements
               --In this case though we must remove the child tgl_object from the parent
               --before we delete it in the child

               --This sub can spiral up through the layers as necessary,
               --calling itself recursively though I think this is rare

               GZ_SMPOLY.DELETE_PARENT_TGL_OBJECT(p_topology,
                                                  p_feature_table,
                                                  p_key,
                                                  p_key_col,
                                                  p_parent_key_col);



               EXECUTE IMMEDIATE psql USING p_key;

            ELSE

               RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;

         COMMIT;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Got a face kount of ' || face_kount || ' for ' || p_feature_table || ' ' || p_key);

      END IF;

      RETURN output;

   END DELETE_A_FACE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   PROCEDURE ADD_A_FACE (
      p_topology           IN VARCHAR2,
      p_feature_table      IN VARCHAR2,
      p_key                IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_key_col            IN VARCHAR2  --caller should pass in GEO_ID or FACE_ID usually
   )
   AS

      --Matt! 10/15/12

      psql                 VARCHAR2(4000);
      tg_layer_id          NUMBER;

   BEGIN

      tg_layer_id := GZ_UTILITIES.GET_TG_LAYER_ID(p_topology,
                                                  p_feature_table,
                                                  'TOPOGEOM',
                                                  'POLYGON');


      --Use the constructor

      psql := 'UPDATE ' ||  p_feature_table || ' a '
           || 'SET '
           || 'a.topogeom = '  --Topology name, topo geom type, tg_layer_id, topo to add (face_id, type), no topo to delete
           || 'SDO_TOPO_GEOMETRY(:p1, :p2, :p3, SDO_TOPO_OBJECT_ARRAY(SDO_TOPO_OBJECT(:p4,:p5)), NULL) '
           || 'WHERE a.' || p_key_col || ' = :p6 ';

      EXECUTE IMMEDIATE psql USING p_topology,
                                   3,
                                   tg_layer_id,
                                   p_face_id,
                                   3,
                                   p_key;

      COMMIT;


   END ADD_A_FACE;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION SUBTRACT_FACE_FROM_FSLS (
      p_topology           IN VARCHAR2,
      p_face_id            IN VARCHAR2,
      p_fsl_hash           IN GZ_TYPES.stringhash,
      p_fsl_pkc            IN VARCHAR2 DEFAULT 'GEO_ID'
   ) RETURN GZ_TYPES.stringhash
   AS

      --Matt! 10/15/12
      --returns stringhash of features that went extinct

      current_fsl       VARCHAR2(32);
      retval            NUMBER;
      output            GZ_TYPES.stringhash;

   BEGIN

      current_fsl := p_fsl_hash.FIRST;

      LOOP

         EXIT WHEN NOT p_fsl_hash.EXISTS(current_fsl);

         IF current_fsl NOT LIKE '%FACE'
         THEN

            retval := GZ_SMPOLY.DELETE_A_FACE(p_topology,
                                              current_fsl,
                                              p_fsl_hash(current_fsl),  --the actual geo_id
                                              p_face_id,
                                              p_fsl_pkc,   --GEO_ID
                                              p_fsl_pkc);  --GEO_ID

         ELSE

            --shield the lower level utility from this carelessness
            retval := GZ_SMPOLY.DELETE_A_FACE(p_topology,
                                              current_fsl,
                                              p_fsl_hash(current_fsl),  --the actual face_id
                                              p_face_id,
                                              'FACE_ID',
                                              p_fsl_pkc);  --and also parents if needed


         END IF;

         IF retval = 0
         THEN

            --list the extinct records
            output(current_fsl) := p_fsl_hash(current_fsl);

         END IF;


         current_fsl := p_fsl_hash.NEXT(current_fsl);

      END LOOP;


      RETURN output;


   END SUBTRACT_FACE_FROM_FSLS;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION ADD_FACE_TO_FSLS (
      p_topology           IN VARCHAR2,
      p_face_id            IN VARCHAR2,
      p_fsl_hash           IN GZ_TYPES.stringhash,
      p_fsl_pkc            IN VARCHAR2 DEFAULT 'GEO_ID',
      p_face_table         IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 10/15/12

      output            VARCHAR2(4000);
      current_fsl       VARCHAR2(32);

   BEGIN

      current_fsl := p_fsl_hash.FIRST;

      LOOP

         EXIT WHEN NOT p_fsl_hash.EXISTS(current_fsl);

         IF current_fsl <> p_face_table
         THEN

            GZ_SMPOLY.ADD_A_FACE(p_topology,
                                 current_fsl,
                                 p_fsl_hash(current_fsl),  --the actual geo_id
                                 p_face_id,
                                 p_fsl_pkc);  --GEO_ID

         ELSE

            --shield the lower level utility from this carelessness
            --Yes, the face feature will temporarily have 2 primitive faces associated
            --Will clean this up in a sec
            GZ_SMPOLY.ADD_A_FACE(p_topology,
                                 current_fsl,
                                 p_fsl_hash(current_fsl),  --the actual face_id
                                 p_face_id,
                                 'FACE_ID');


         END IF;


         current_fsl := p_fsl_hash.NEXT(current_fsl);

      END LOOP;


      RETURN 'TRUE';

   END ADD_FACE_TO_FSLS;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION DETERMINE_CONFIG (
      p_topology           IN VARCHAR2,
      p_transaction_id     IN VARCHAR2,
      p_log_type           IN VARCHAR2,
      p_small_face_id      IN NUMBER,
      p_big_face_id_1      IN NUMBER,
      p_big_face_id_2      IN NUMBER DEFAULT NULL,
      p_big_face_id_3      IN NUMBER DEFAULT NULL,
      p_debug              IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2
   AS

     --Matt! 10/15/12  corresponds to
     --https://collab.ecm.census.gov/div/geo/cpb/CAMPS%20wiki%20files/merge_face.gif

     output                VARCHAR2(4);
     big_face_kount1       PLS_INTEGER;
     big_face_kount2       PLS_INTEGER;
     big_face_kount3       PLS_INTEGER;

   BEGIN
      
      IF p_big_face_id_3 IS NOT NULL
      AND p_big_face_id_2 IS NULL
      THEN
      
         IF p_debug <> 1
         THEN
            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,p_transaction_id,NULL,
                                                   'Got a big face 3 but no big face 2.  What gives?');
         END IF;
                                                   
         RAISE_APPLICATION_ERROR(-20001,'Got a big face 3 but no big face 2.  What gives?');
      
      END IF;
      
      
      IF p_big_face_id_2 IS NULL
      AND p_big_face_id_3 IS NULL
      THEN
         
         ----------------------------------------------------------------------
         --simple Config I, II (becomes I after obsolete node removal), or  III
         ----------------------------------------------------------------------
         
         IF GZ_SMPOLY.IS_OBSOLETE_NODE_BETWEEN_FACES(p_topology,
                                           p_small_face_id,
                                           p_big_face_id_1)
         THEN
               
            --Config II. Merge module is making obsolete nodes it seems. 
            --Remove them and this should turn into config I
            --If its some other config with an obsolete we may be in troubl
            
            IF p_debug <> 1
            THEN
               GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,p_transaction_id,NULL,
                                                   'This looks like config II.  Theres an obsolete node between ' 
                                                   || p_small_face_id || ' and ' || p_big_face_id_1);    
            END IF;
                                                      
            GZ_SMPOLY.REMOVE_OBSOLETE_BETWEEN_FACES(p_topology,
                                                    p_small_face_id,
                                                    p_big_face_id_1);
                  
                                                
            IF GZ_SMPOLY.IS_OBSOLETE_NODE_BETWEEN_FACES(p_topology,
                                                        p_small_face_id,
                                                        p_big_face_id_1)
            THEN
            
               IF p_debug <> 1
               THEN
                  GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,p_transaction_id,NULL,
                                                         'Failed to remove the obsolete between ' || p_small_face_id || ' and ' 
                                                         || p_big_face_id_1 || '. Bailing');
               END IF;
                                                      
               RAISE_APPLICATION_ERROR(-20001,'Failed to remove the obsolete between ' || p_small_face_id || ' and ' 
                                              || p_big_face_id_1 || '. Bailing');  
            
            ELSE
            
               IF p_debug <> 1
               THEN
                  GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,p_transaction_id,NULL,
                                                         'Removed the obsolete node.  Will figure out a configuration now');
               END IF;          
            
            END IF;

         
         END IF;
         
         IF GZ_SMPOLY.COUNT_EDGES_BETWEEN_FACES(p_topology,
                                                p_small_face_id,
                                                p_big_face_id_1) = 1
                                      
         THEN
         
            --Mr. Popularity is here. Config 1
            output := 'I';               
         
         ELSIF GZ_SMPOLY.COUNT_EDGES_BETWEEN_FACES(p_topology,
                                                   p_small_face_id,
                                                   p_big_face_id_1) = 2
         THEN                         
         
            --Indicates likely III, lollipop style
            --Dont need to check for obsolete nodes, did that above
            --But what if the 2 edges are non-connected, like the big face snakes around and taps
            --the small face on its back?  Check that
            
            IF GZ_SMPOLY.ARE_EDGES_BTWN_FACES_CONNECTED(p_topology,
                                                        p_small_face_id,
                                                        p_big_face_id_1)
            THEN
            
               output := 'III';
               
            ELSE
            
               RAISE_APPLICATION_ERROR(-20001, 'Need eyeballs on this configuration');
            
            END IF;  
            
         ELSE
         
            RAISE_APPLICATION_ERROR(-20001, ' Unknown configuration');    
         
         END IF;
         
      
      ELSIF p_big_face_id_2 IS NOT NULL
      AND p_big_face_id_3 IS NULL
      THEN
      
         -------------------------------------------------------
         --           Configs IV, V, or VI                    --
         -------------------------------------------------------

         big_face_kount1 := GZ_SMPOLY.COUNT_EDGES_BETWEEN_FACES(p_topology,
                                                                p_small_face_id,
                                                                p_big_face_id_1);
                                                                
         big_face_kount2 := GZ_SMPOLY.COUNT_EDGES_BETWEEN_FACES(p_topology,
                                                                p_small_face_id,
                                                                p_big_face_id_2);
                                                                 
         IF  big_face_kount1 = 1
         AND big_face_kount2 = 1
         THEN
         
            --IV or VI
            IF ARE_THREE_FACES_AT_A_POINT(p_topology,
                                          p_small_face_id,
                                          p_big_face_id_1,
                                          p_big_face_id_2)
            THEN
            
               output := 'IV';
               
            ELSE
            
               output := 'VI';
            
            END IF;                              
                                         
         
         ELSIF (big_face_kount1 = 1 AND big_face_kount2 = 2)
         OR    (big_face_kount1 = 2 AND big_face_kount2 = 1)
         THEN
         
            --hopefully V
            IF GZ_SMPOLY.IS_AC_BC_AC(p_topology,
                                     p_small_face_id,
                                     p_big_face_id_1,
                                     p_big_face_id_2)
            THEN
            
               output := 'V';
               
            ELSE
            
               RAISE_APPLICATION_ERROR(-20001,'unknown config, please investigate');
            
            END IF;                         
                                    
         
         ELSE
         
         
            RAISE_APPLICATION_ERROR(-20001,'Uknown configuration');
            
         END IF;
      
      
      ELSIF p_big_face_id_2 IS NOT NULL
      AND p_big_face_id_3 IS NOT NULL
      THEN
      
         -------------------------------------------------------
         -- Configs VII or VIII or something totally new     
         -------------------------------------------------------
      
         big_face_kount1 := GZ_SMPOLY.COUNT_EDGES_BETWEEN_FACES(p_topology,
                                                                p_small_face_id,
                                                                p_big_face_id_1);
                                                                
         big_face_kount2 := GZ_SMPOLY.COUNT_EDGES_BETWEEN_FACES(p_topology,
                                                                p_small_face_id,
                                                                p_big_face_id_2);
                                                                
         big_face_kount3 := GZ_SMPOLY.COUNT_EDGES_BETWEEN_FACES(p_topology,
                                                                p_small_face_id,
                                                                p_big_face_id_2);
                                                                
         
                                                                 
         IF  big_face_kount1 = 1
         AND big_face_kount2 = 1
         AND big_face_kount3 = 1
         THEN
         
            --VII or VIII 
            
            IF ARE_THREE_FACES_AT_A_POINT(p_topology,
                                          p_small_face_id,
                                          p_big_face_id_1,
                                          p_big_face_id_2)
            THEN
            
               output := 'VII';
               
            ELSE
            
               output := 'VIII';
            
            END IF;                              
                                         
         
         ELSE
         
            --This is probably some sort of a 3 face config and II, V, or VI
            --Can be programmed, but I'm not sure if it exists in a meaningful way
            RAISE_APPLICATION_ERROR(-20001,'Unknown configuration: More than 2 edges between a face');
            
         END IF;
      
      END IF;
      
      

      IF output NOT IN ('I','III','IV','V','VI','VII','VII') 
      THEN
      
         --This is the spec, basically permanent check 
         RAISE_APPLICATION_ERROR(-20001,'Unknown case ' || output);

      END IF;
      
      
      --This is the in-progress list of programmed configs we let past this point
      
      IF output IN ('I','IV','V','VII')  --note no II, it gets converted
      THEN
      
         RETURN output;
         
      ELSE
      
         RAISE_APPLICATION_ERROR(-20001,'Bridge out ahead for config ' || output);
      
      END IF;


   END DETERMINE_CONFIG;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE GZ_REMOVE_EDGE (
      p_topo           IN VARCHAR2,
      p_edge_id        IN VARCHAR2
   )
   AS

      --Matt! 9/10/12
      --Copied verbatim from the version in gz_clip

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

      create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                   p_topo,
                                                   2,
                                                   window_geom.sdo_ordinates(1),
                                                   window_geom.sdo_ordinates(2),
                                                   window_geom.sdo_ordinates(3),
                                                   window_geom.sdo_ordinates(4)
                                                   );


      --no error handler here, let the caller trap and release
      SDO_TOPO_MAP.REMOVE_EDGE(NULL,p_edge_id);

      create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);


   END GZ_REMOVE_EDGE;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION REMOVE_SHARED_EDGE (
      p_topology        IN VARCHAR2,
      p_dead_edge       IN NUMBER 
   ) RETURN GZ_TYPES.numberarray
   AS

      --Matt! 10/15/12
      --originally this was more complicated, it was based on the big-small face pairs
      --But in repeated calls over a loop those face_ids mutate
      --The only constant is the previously marked edge to be removed


      output            GZ_TYPES.numberarray;


   BEGIN

      
      --must get before removing the edge
      output := GZ_SMPOLY.GET_NODES_FOR_EDGE(p_topology,
                                             p_dead_edge);

      GZ_SMPOLY.GZ_REMOVE_EDGE(p_topology,
                               p_dead_edge);

      RETURN output;

   END REMOVE_SHARED_EDGE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION REMOVE_NODE_KINDLY (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 10/16/12
      --Exits cleanly if node has already been removed, or is simply not obsolete
      --Otherwise removes the obsolete node, and thats it.  Leaves vertex

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

      IF edge_ids.COUNT > 2
      OR edge_ids.COUNT = 0
      THEN

         --not obsolete, either 0 or 2+
         RETURN 0;

      ELSIF edge_ids.COUNT = 1
      THEN

         --Maybe this is a ring edge?  Let's leave it be for now
         RETURN 0;
         --RAISE_APPLICATION_ERROR(-20001,'How is node ' || p_node_id || ' connected to just one edge?');

      END IF;

      --set up topomap to cover the extent of both edges

      psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 OR '
           || 'e.edge_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO window_geom USING edge_ids(1),
                                                    edge_ids(2);

      --create the topomap with window

      create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
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

         create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

         RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

      END;

      --commit and drop temp topomap
      create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

      --not guaranteed buddy
      RETURN 1;


   END REMOVE_NODE_KINDLY;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   PROCEDURE REMOVE_OBSOLETE_NODES_LIST (
      p_topology           IN VARCHAR2,
      p_nodes              IN GZ_TYPES.numberarray,
      p_option             IN VARCHAR2 DEFAULT 'ZAP'
   )
   AS

      --Matt! 10/15/12

      node_kount        PLS_INTEGER;

   BEGIN

      IF p_nodes.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Why you pestering me?');

      END IF;

      FOR i IN 1 .. p_nodes.COUNT
      LOOP

         IF p_option = 'ZAP'
         THEN

            --this guy in topofix is good
            --Removes the node and then deletes the ghost vertex
            --dont bother finding out if the node is obsolete, just use the existing error handler


            BEGIN

               GZ_TOPOFIX.ZAP_NODE(p_topology,
                                   p_nodes(i));

            EXCEPTION
            WHEN OTHERS
            THEN

               IF SQLERRM LIKE '%Zap node expects node to be bound by 2 edges%'
               THEN

                  --the ones that catch in the error handler will be like case III where the "start" of the new
                  --edge was also connected to some other edge that doesnt concern us

                  NULL;

               ELSE

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

               END IF;

            END;

         ELSIF p_option = 'REMOVE'
         THEN

            --does nothing at all if node doesnt exist, or is not obsolete
            --otherwise opens tiny topomap and remvoes the obsolete node, leaving vertex behind

            --return 0 or 1, not using at the moment
            node_kount := GZ_SMPOLY.REMOVE_NODE_KINDLY(p_topology,
                                                       p_nodes(i));

         ELSE

            RAISE_APPLICATION_ERROR(-20001, 'Bad option ' || p_option);

         END IF;

      END LOOP;

   END REMOVE_OBSOLETE_NODES_LIST;
   
   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------
   
   PROCEDURE REMOVE_OBSOLETE_BETWEEN_FACES (
      p_topology        IN VARCHAR2,
      p_face_id_1       IN NUMBER,
      p_face_id_2       IN NUMBER
   )
   AS
   
      --Matt! 11/07/12
      
      edge_ids             GZ_TYPES.NUMBERARRAY;
      kount                NUMBER := 0;
      node_id              NUMBER;
   
   BEGIN  
   
   
      edge_ids := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES(p_topology,
                                                    p_face_id_1,
                                                    p_face_id_2);
                                                              
      IF edge_ids.COUNT = 1
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Why you pestering bub?  Just 1 edge btwn '
                                      || p_face_id_1 || ' ' || p_face_id_2);
      
      END IF;
      
         
      FOR i IN 1 .. edge_ids.COUNT
      LOOP      

         
         IF GZ_SMPOLY.IS_EDGE_BOUNDED_BY_OBSOLETE(p_topology,
                                                  edge_ids(i))
         THEN
            
            node_id := GZ_SMPOLY.GET_EDGE_NODE(p_topology,
                                               edge_ids(i),
                                               'START');
               
            kount := GZ_SMPOLY.REMOVE_NODE_KINDLY(p_topology,
                                                  node_id);
                           
            IF kount = 0
            THEN
                                             
               node_id := GZ_SMPOLY.GET_EDGE_NODE(p_topology,
                                                  edge_ids(i),
                                                  'END');
                  
               kount := GZ_SMPOLY.REMOVE_NODE_KINDLY(p_topology,
                                                     node_id);
                                                     
            END IF;
               
         END IF;
         
                  
         IF kount = 1
         THEN
         
            --must exit as soon as we think an obsolete node went bye
            --the edge ids in our loop are bunk at that point
         
            EXIT;
            
         END IF;
         
      END LOOP;
   
      
   END REMOVE_OBSOLETE_BETWEEN_FACES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION GET_FACE_TABLE (
      p_topology           IN VARCHAR2,
      p_not_like_clause    IN VARCHAR2  --%FSL% maybe will change some day
   ) RETURN VARCHAR2
   AS

      --Matt! 10/16/12
      --Matt can do better than this

      output            VARCHAR2(32);
      psql              VARCHAR2(4000);

   BEGIN

      psql := 'SELECT a.table_name FROM '
           || 'user_sdo_topo_info a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.column_name = :p2 AND '
           || 'a.tg_layer_type = :p3 AND '
           || 'a.table_name NOT LIKE :p4 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING UPPER(p_topology),
                                                  'TOPOGEOM',
                                                  'POLYGON',
                                                  p_not_like_clause;

      EXCEPTION

         WHEN NO_DATA_FOUND
         THEN

         RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

      WHEN OTHERS
      THEN

         RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

      END;

      IF output IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Couldnt find a face table for ' || p_topology);

      END IF;

      RETURN output;

   END GET_FACE_TABLE;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------------

    FUNCTION ADD_NEW_EDGE (
      p_topo            IN VARCHAR2,
      p_old_node_id     IN NUMBER,
      p_new_node_id     IN NUMBER,
      p_extend_edge     IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 4/03/10
      --Copied from clip and modified 11/06/12
      --Add a new edge
      --Return the new edge ids
      --Handle errors somehow


      new_edge_id       NUMBER;
      psql              VARCHAR2(4000);
      new_node_geom     SDO_GEOMETRY;
      old_node_geom     SDO_GEOMETRY;
      new_edge_geom     SDO_GEOMETRY;
      kount             PLS_INTEGER;
      connect_edges     GZ_TYPES.numberarray;
      tip               VARCHAR2(8);


   BEGIN

      --We dont trust the geometry we used to create the new nodes
      
      new_node_geom := GZ_SMPOLY.GET_NODE_GEOMETRY(p_topo,
                                                   p_new_node_id);
         
      --Get the old boy too                                          
      old_node_geom := GZ_SMPOLY.GET_NODE_GEOMETRY(p_topo,
                                                   p_old_node_id);
                                                   
      --Figure out tippy tip
      tip := GZ_SMPOLY.GET_NODE_TIP(p_topo,
                                    p_extend_edge,
                                    p_old_node_id);


      --In clip we maintain direction since we have the edge attribute table with L/R values
      --I like this here too, since it means the L/R info for the big face will match our new
      --sliver face split
      
      IF tip = 'START'
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

      ELSIF tip = 'END'
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
      --             |  <--- Who knows whats connected outside the sliver
      --             |
      --      -------0--------
      --             |<-- New edge, doesnt exist in topo yet
      --             |
      --           --|--------   <-- An unexpected edge HERE is bad 
      --             |
      --      -------0-------
      --            / \  <-- More edges I dont care about


      psql := 'SELECT DISTINCT e.edge_id '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.start_node_id IN (:p1,:p2) OR '
           || 'end_node_id IN (:p3,:p4) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO connect_edges USING p_new_node_id, p_old_node_id,
                                                                   p_new_node_id, p_old_node_id;



      --Check to see if our new planned edge crosses some other edges

      psql := 'SELECT count(*) '
           || 'FROM ' || p_topo || '_EDGE$ e '
           || 'WHERE e.edge_id NOT IN (SELECT * FROM TABLE(:p1)) AND '
           || 'SDO_RELATE(e.geometry,:p2,:p3) = :p4 ';

      EXECUTE IMMEDIATE psql INTO kount USING GZ_UTILITIES.NUMARRAY_TO_VARRAY(connect_edges),
                                              new_edge_geom,
                                              'mask=ANYINTERACT',
                                              'TRUE';

      IF kount > 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Got extra connections with ' || TO_CHAR(GZ_UTILITIES.DUMP_SDO(new_edge_geom)));
         
      END IF;


      --Lets write a book to the log before the inevitable errors
      --Do it
     
      BEGIN
     
         IF tip = 'START'
         THEN
        
            --we want direction of new node to old node
            new_edge_id := SDO_TOPO_MAP.ADD_EDGE(p_topo,
                                                 p_new_node_id,
                                                 p_old_node_id,
                                                 new_edge_geom);
        
         ELSIF tip = 'END'
         THEN
        
            --we want direction of old node to new node
            new_edge_id := SDO_TOPO_MAP.ADD_EDGE(p_topo,
                                                 p_old_node_id,
                                                 p_new_node_id,
                                                 new_edge_geom);
        
         ELSE
        
            RAISE_APPLICATION_ERROR(-20001,'Bad tip ' || tip);
        
         END IF;
     
      EXCEPTION
      WHEN OTHERS
      THEN
        
         IF SQLCODE = -29532
         THEN

            --Lets see what we scare up here
            RAISE_APPLICATION_ERROR(-20001, 'Old node: ' || p_old_node_id || ' New node: ' || p_new_node_id
                                            || ' Edge geom: ' || TO_CHAR(GZ_UTILITIES.DUMP_SDO(new_edge_geom)) 
                                            || ' ' || SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

         ELSE

            RAISE;

         END IF;
     
      END;

     RETURN new_edge_id;

   END ADD_NEW_EDGE;
   
   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------
   
    FUNCTION ADD_NEW_NODE (
      p_topo               IN VARCHAR2,
      p_edge               IN NUMBER,
      p_coord_index        IN NUMBER,
      p_node               IN SDO_GEOMETRY,
      p_is_new_shape_pt    IN VARCHAR2
   ) RETURN NUMBER
   AS

      --Matt! 4/03/10
      --Copied from clip and modified 11/06/12
      --Add a node to the topology
      --Return the new node id
      --Handle errors

      output            NUMBER;
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


         IF SQLCODE = -29532
         THEN


            IF SQLERRM LIKE '%add node results in an intersection with another edge%'
            THEN

               RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            ELSIF SQLERRM LIKE '%is not on the boundary of one or two of the faces it links%'
            THEN

               --use an explicit topomap 

               topomap := p_topo || '_TOPOMAP';

               edge_mbr := GZ_SMPOLY.GET_EDGE_MBR(p_topo,
                                                  p_edge);

               create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(topomap,
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

               create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(topomap,
                                                            p_topo,
                                                            create_it);


               RETURN output;

            ELSIF SQLERRM LIKE '%Add node results in two pieces of split edge intersecting each other%'
            THEN

               --This means we probably are working with the wrong coord index
               --No error handling here per se (yet) requires fix in the caller

               RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            ELSE

               RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            END IF;


         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);
             
         END IF;



   END ADD_NEW_NODE;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_EXISTING_NODE (
      p_topo            IN VARCHAR2,
      p_edge            IN VARCHAR2,
      p_coord_index     IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 7/09/10
      --copied and modified from clip 11/06/12

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
                   || 'FROM ' || p_topo || '_edge$ e '
                   || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO output USING p_edge;

      RETURN output;


   END GET_EXISTING_NODE;
   
   
   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------
   
   FUNCTION ADD_AN_OPPOSITE_NODE (
      p_topology           IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_node_id            IN NUMBER,
      p_face_table         IN VARCHAR2,
      p_lateral_edge_1     IN NUMBER,
      p_lateral_edge_2     IN NUMBER,
      p_lateral_edge_3     IN NUMBER DEFAULT NULL,  --For config VII 
      p_feature_face_id    IN NUMBER DEFAULT NULL,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_testing            IN NUMBER DEFAULT 0
   ) RETURN NUMBER
   AS
   
      --Matt!  11/05/12
      
      --                Node
      --                 |
      --       lateral1  V    lateral2
      --    -------------0---------------   
      -- 
      --         face of interest              /
      --  |                                   / 
      --   --------------X--------------------
      --                 ^
      --                 |
      --               target
      
      
      new_node_id          NUMBER;
      psql                 VARCHAR2(4000);
      edge_idz             GZ_TYPES.numberarray;
      ordered_edgez        GZ_TYPES.doublenumarray;
      old_node_geom        SDO_GEOMETRY;
      current_distance     NUMBER;
      target_edge_geom     SDO_GEOMETRY;
      target_edge_id       NUMBER;
      new_node_geom        SDO_GEOMETRY;
      coord_index          NUMBER;
      target_segment       GZ_TYPES.stringarray;
      test_node_geom       SDO_GEOMETRY;
      tucked_srid          NUMBER;
      is_new_shape_pt      VARCHAR2(8) := 'TRUE';  --must prove otherwise
      existing_node_flag   NUMBER := 0;            --same
      face_geometry        SDO_GEOMETRY;
      test_edge_geom       SDO_GEOMETRY;
      feature_face_id      NUMBER;
      
      
   BEGIN
   
      --Will use the feature face SDO for some tests below
      --If this is a multipart merge we cant use the small face id since it may be new
      --(ie face$ only, not in feature face table)
      --Caller saves the face for us
      
      IF p_feature_face_id IS NULL
      THEN
      
         feature_face_id := p_face_id;
         
      ELSE
      
         feature_face_id := p_feature_face_id;      
      
      END IF;
      
      --need the geoms in memory since we need to go off the grid
      --in terms of srids
      --also I think tends to be faster than sdo_nn against a biggie
      
      old_node_geom := GZ_SMPOLY.GET_NODE_GEOMETRY(p_topology,
                                                   p_node_id);
          
      tucked_srid := old_node_geom.sdo_srid;
      
      --get the edge_ids and geoms      
      
      IF p_lateral_edge_3 IS NULL
      THEN
                    
         --attempt to connect to universal face if possible
         --This may not always be true, but an alternative use is so far off...
                             
         psql := 'SELECT e.edge_id '
              || 'FROM ' || p_topology || '_edge$ e '
              || 'WHERE '
              || '(e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
              || 'e.edge_id NOT IN (:p3,:p4) AND '
              || '(e.left_face_id = :p5 OR e.right_face_id = :p6)';
         
         EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_idz USING p_face_id,
                                                                 p_face_id,
                                                                 p_lateral_edge_1,
                                                                 p_lateral_edge_2,
                                                                 -1,
                                                                 -1;
                                                                 
      ELSE
      
         --Config VII, add in the other lateral edge off to the side in order to avoid it
         
         psql := 'SELECT e.edge_id '
              || 'FROM ' || p_topology || '_edge$ e '
              || 'WHERE '
              || '(e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
              || 'e.edge_id NOT IN (:p3,:p4,:p5) AND '
              || '(e.left_face_id = :p6 OR e.right_face_id = :p7)';
         
         EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_idz USING p_face_id,
                                                                 p_face_id,
                                                                 p_lateral_edge_1,
                                                                 p_lateral_edge_2,
                                                                 p_lateral_edge_3,
                                                                 -1,
                                                                 -1;
      END IF;
      
      IF edge_idz.COUNT = 0
      THEN
      
         --No universal face edge to use, probably interior
         --Remove from the SQL
         IF p_lateral_edge_3 IS NULL
         THEN
                       
            --attempt to connect to universal face if possible
            --This may not always be true, but an alternative use is so far off...
                                
            psql := 'SELECT e.edge_id '
                 || 'FROM ' || p_topology || '_edge$ e '
                 || 'WHERE '
                 || '(e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
                 || 'e.edge_id NOT IN (:p3,:p4) ';
            
            EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_idz USING p_face_id,
                                                                    p_face_id,
                                                                    p_lateral_edge_1,
                                                                    p_lateral_edge_2;
                                                                    
         ELSE
         
            --Config VII, add in the other lateral edge off to the side in order to avoid it
            
            psql := 'SELECT e.edge_id '
                 || 'FROM ' || p_topology || '_edge$ e '
                 || 'WHERE '
                 || '(e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
                 || 'e.edge_id NOT IN (:p3,:p4,:p5) ';
            
            EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_idz USING p_face_id,
                                                                    p_face_id,
                                                                    p_lateral_edge_1,
                                                                    p_lateral_edge_2,
                                                                    p_lateral_edge_3;
         END IF;
      
      END IF;
   
      IF edge_idz.COUNT = 1
      THEN
         
         --hopefully here usually
         ordered_edgez(1).number1 := edge_idz(1);
         --distance no matter
         ordered_edgez(1).number2 := 0;
         
      ELSIF edge_idz.COUNT = 0
      THEN
      
         RAISE_APPLICATION_ERROR(-20001,'Need to look at this, the only edges bounding ' || p_face_id 
                                    || ' appear to be our lateral edges ' || p_lateral_edge_1 || ',' || p_lateral_edge_2 || ','
                                    || p_lateral_edge_3);
                                    
      ELSE
      
         --This is a pig sty
         --find the closest edge to our start point
         --save the others as backups
         
         old_node_geom.sdo_srid := NULL;         
         
         FOR i IN 1 .. edge_idz.COUNT
         LOOP
         
            target_edge_geom := GZ_CLIP.GET_EDGE_GEOMETRY(p_topology,
                                                          edge_idz(i));
            target_edge_geom.sdo_srid := NULL;
            
            current_distance := SDO_GEOM.SDO_DISTANCE(old_node_geom, target_edge_geom, .00000000000000001);
            
            ordered_edgez := GZ_SMPOLY.DUBNUMBERARRAY_ORDER(edge_idz(i),      --key
                                                            current_distance, --val
                                                            ordered_edgez,
                                                            'ASC');           --order low to hi
                     
         END LOOP;
      
      END IF;
      
      FOR i IN 1 .. ordered_edgez.COUNT
      LOOP
      
         --usually the first edge is the one we want, this is not tricksy
         --But sometimes there are problems, and backup edges come into play
         
         --ensure null srids         
         target_edge_id := ordered_edgez(i).number1;
         target_edge_geom := GZ_CLIP.GET_EDGE_GEOMETRY(p_topology,
                                                       target_edge_id);
         target_edge_geom.sdo_srid := NULL;
         old_node_geom.sdo_srid := NULL;
         
         new_node_geom := GZ_CLIP.GZ_PROJECT_PT(old_node_geom, target_edge_geom);  --hmm default tolerance?
         
         coord_index := GZ_CLIP.ACCURATE_FIND_COORD_INDEX(new_node_geom,target_edge_geom);
         
         --Tests start here, maybe we can avoid a lot of the clip mess?
         --First, does our new proposed edge fall entirely inside of the feature face we are splitting
         --I got one where the closest edge resulted in splitting across a big face
         --This should always be OK even on multisplit small face configs (ie face sdo doesnt match face$)
         --Since the edges in play at this moment bound part of the original larger face feature
         
         face_geometry := GZ_SMPOLY.GET_FEATURE_FACE_GEOMETRY(p_face_table,
                                                              feature_face_id);
         test_edge_geom := SDO_GEOMETRY(2002,
                                        tucked_srid,
                                        NULL,
                                        SDO_ELEM_INFO_ARRAY(1, 2, 1),
                                        SDO_ORDINATE_ARRAY(new_node_geom.sdo_point.X,
                                                           new_node_geom.sdo_point.Y,
                                                           old_node_geom.sdo_point.X,
                                                           old_node_geom.sdo_point.Y
                                                           ));
                                                           
         --dbms_output.put_line(GZ_UTILITIES.DUMP_SDO(face_geometry));
         --dbms_output.put_line(GZ_UTILITIES.DUMP_SDO(test_edge_geom));
                                                                                                             
         IF SDO_GEOM.RELATE(face_geometry,'mask=contains+covers+overlapbdyintersect',test_edge_geom,p_tolerance) <> 'FALSE'
         THEN
          
            EXIT;
            
         ELSIF i = ordered_edgez.COUNT
         THEN
         
            RAISE_APPLICATION_ERROR(-20001, 'Couldnt find a propah edge! Proposed new node ' 
                                            || TO_CHAR(GZ_UTILITIES.DUMP_SDO(new_node_geom)) );
         
         END IF;

      
      END LOOP;
      

      
      ---------------------------------------------------------
      --TEST1: (well not exactly a test) Are we close to an existing shape point or node?
      --At least need to see if we are projecting to an existing shape point 
      --or to an existing node (within tolerance)
      ----------------------------------------------------------
      
      
      --get x1,y1,x2,y2 for this index value
      target_segment := GZ_CLIP.GET_SEGMENT_FROM_INDEX(target_edge_geom, coord_index);
      
      
      --start side
      test_node_geom := SDO_GEOMETRY(2001,
                                     tucked_srid,
                                     SDO_POINT_TYPE(target_segment(1), target_segment(2), NULL),
                                     NULL,
                                     NULL);
                              
      new_node_geom.sdo_srid := tucked_srid;
             
      IF SDO_GEOM.RELATE(test_node_geom,'mask=EQUAL',new_node_geom,p_tolerance) <> 'FALSE'
      THEN    
      
         --Wont create a new vertex, go with the existing one
         is_new_shape_pt := 'FALSE';                          
                            
         IF coord_index <> 0
         THEN

            --usually in here if at all
            --our new node is going right on this vertex we found
            new_node_geom := test_node_geom;

         ELSE 
      
            --Picked the start node of the edge
            new_node_geom := test_node_geom;  --same as above but...
            existing_node_flag := 1;                
            
         END IF;
         
      ELSE
      
         --Check the end end of the segment if the beginning didnt match
         test_node_geom := SDO_GEOMETRY(2001,
                                        tucked_srid,
                                        SDO_POINT_TYPE(target_segment(3), target_segment(4), NULL),
                                        NULL,
                                        NULL);
                                        
         IF SDO_GEOM.RELATE(test_node_geom,'mask=EQUAL',new_node_geom, p_tolerance) <> 'FALSE'
         THEN                
       
            coord_index := coord_index + 1; --dont forget!
                                            --we are actually incrementing onto the next index

            is_new_shape_pt := 'FALSE';
            
            IF (SDO_UTIL.GETNUMVERTICES(target_edge_geom) - 1 ) <> coord_index
            THEN

               --usually here. Somewhere in the middle of the edge
               new_node_geom := test_node_geom; 

            ELSE   --ie (edge vertex count - 1 ) = coord_index

               --We have incremented ourself from the final coord index onto the next edge

               existing_node_flag := 1;
               new_node_geom := test_node_geom;

            END IF;


         END IF;
           
      END IF;
      
      --Clip has a verify coord index step here
      --could also check for intersections with other edges
      
      IF p_testing = 0
      THEN
      
         --Actually add the new node
         
         IF is_new_shape_pt = 'TRUE'                                --Just a totally new spot for this node
         OR (is_new_shape_pt = 'FALSE' AND existing_node_flag = 0)  --OR an existing vertex on an edge
         THEN

            new_node_id := GZ_SMPOLY.ADD_NEW_NODE(p_topology,
                                                  target_edge_id,
                                                  coord_index,
                                                  new_node_geom,
                                                  is_new_shape_pt);

         ELSIF is_new_shape_pt = 'FALSE'
         AND existing_node_flag = 1
         THEN

            new_node_id := GZ_SMPOLY.GET_EXISTING_NODE(p_topology,
                                                       target_edge_id,
                                                       coord_index);
         ELSE
         
            RAISE_APPLICATION_ERROR(-20001, 'Shouldnt get here');
            
         END IF;
               
      ELSE
      
         --just playin
         --lets see it first
         
         dbms_output.put_line('is_new_shape_pt ' || is_new_shape_pt);
         dbms_output.put_line('existing_node_flag ' || existing_node_flag);
         dbms_output.put_line('target_edge_id ' || target_edge_id);
         dbms_output.put_line('coord_index ' || coord_index);
         dbms_output.put_line(TO_CHAR(GZ_UTILITIES.DUMP_SDO(new_node_geom)));         
      
         new_node_id := 0;
         
      END IF;
      
      RETURN new_node_id;
   
   END ADD_AN_OPPOSITE_NODE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   PROCEDURE MERGE_FACE_GEOMETRY_WORK (
      p_topology           IN VARCHAR2,
      p_transaction_id     IN VARCHAR2,
      p_merge_case         IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_small_face_id      IN NUMBER,
      p_big_face_id_1      IN NUMBER,
      p_big_face_id_2      IN NUMBER DEFAULT NULL,
      p_big_face_id_3      IN NUMBER DEFAULT NULL,
      p_small_faces        IN OUT GZ_TYPES.NUMBERARRAY,  --synched
      p_big_faces          IN OUT GZ_TYPES.NUMBERARRAY,  --synched
      p_dead_edges         IN OUT GZ_TYPES.NUMBERARRAY,  --synched
      p_new_edges          IN OUT GZ_TYPES.NUMBERARRAY,  --separate, not really used
      p_dead_nodes         IN OUT GZ_TYPES.NUMBERARRAY,  --separate, used
      p_log_type           IN VARCHAR2
   )
   AS

      --Matt! 10/16/12
      --IT WAS COMMANDED: THERE SHALT BE NO SQL IN THIS PROCEDURE
      
      lateral_edge_1       NUMBER;
      lateral_edge_2       NUMBER;
      lateral_edge_3       NUMBER;
      lateral_edges_sto    GZ_TYPES.numberarray;
      extending_edges      GZ_TYPES.numberarray;
      extend_node          NUMBER;
      new_node_id          NUMBER;
      new_edge_id          NUMBER;
      temp_num_array       GZ_TYPES.numberarray;
      log_message          VARCHAR2(4000);
      inner_big_face_id    NUMBER;
      outer_big_face_id    NUMBER;
      small_face_id        NUMBER := p_small_face_id; --changes internally
      new_face_id          NUMBER;
      looping_extend_edge  VARCHAR2(1) := 'N';
      feature_face_id      NUMBER;

   BEGIN

      IF p_merge_case = 'I'
      THEN
      
         ---------------------------------------------
         -- I I I I I I I I I I I I I I I I I I I I I 
         ---------------------------------------------

         p_small_faces(1) := small_face_id;
         p_big_faces(1)   := p_big_face_id_1;
         
         p_dead_edges(1) := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                             small_face_id,
                                                             p_big_face_id_1);
         
         --no new edges
         --no nodes that are dead, just eventually obsolete
         
      ELSIF p_merge_case = 'IV'
      THEN
      
         ---------------------------------------------
         -- IV IV IV IV IV IV IV IV IV IV IV IV IV IV 
         ---------------------------------------------
               
         lateral_edge_1 := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                            small_face_id,
                                                            p_big_face_id_1);
                                                   
         lateral_edge_2 := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                            small_face_id,
                                                            p_big_face_id_2);
                                                            
         extending_edges(1) := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                                p_big_face_id_1,
                                                                p_big_face_id_2);
                                                            
         extend_node := GZ_SMPOLY.GET_NODE_BETWEEN_EDGES(p_topology,
                                                         lateral_edge_1,
                                                         lateral_edge_2);
           
         --while we got it. Will be dead after the lateral edges are removed
         p_dead_nodes(p_dead_nodes.COUNT + 1) := extend_node;
                                                     
         new_node_id := GZ_SMPOLY.ADD_AN_OPPOSITE_NODE(p_topology,
                                                       small_face_id,
                                                       extend_node,
                                                       p_face_table,
                                                       lateral_edge_1,
                                                       lateral_edge_2);
                                                    
         new_edge_id := GZ_SMPOLY.ADD_NEW_EDGE(p_topology,
                                               extend_node,
                                               new_node_id,
                                               extending_edges(1));
                                               
         p_new_edges(p_new_edges.COUNT + 1) := new_edge_id;
         
         --Geometry work is done, just figure out matching biggie smalls
         
         p_small_faces(1) := small_face_id;         
         
         p_small_faces(2) := GZ_SMPOLY.GET_OPPOSITE_FACE(p_topology,
                                                         small_face_id,
                                                         new_edge_id);
                        
         --is there an edge between original small face and big 1?                                 
         temp_num_array := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES(p_topology,
                                                             small_face_id,
                                                             p_big_face_id_1);
          
         IF temp_num_array.COUNT = 1
         THEN
            
            --yes, original small face is with big face 1
            p_big_faces(1)  := p_big_face_id_1;
            --new small face is with big face 2
            p_big_faces(2)  := p_big_face_id_2;
            
            p_dead_edges(1) := lateral_edge_1;
            p_dead_edges(2) := lateral_edge_2;
            
         ELSIF temp_num_array.COUNT = 0
         THEN
         
            --original small face is with big 2
            p_big_faces(1) := p_big_face_id_2;
            --new small face is with big 1
            p_big_faces(2) := p_big_face_id_1;
            
            --yes, this switches too
            p_dead_edges(1) := lateral_edge_2;
            p_dead_edges(2) := lateral_edge_1;
            
         ELSE
         
            RAISE_APPLICATION_ERROR(-20001, 'Got ' || temp_num_array.COUNT || ' edges between faces ' 
                                            || small_face_id || ' and ' || p_big_face_id_1 || '?');
         
         END IF;
          
         
      ELSIF p_merge_case = 'V'
      THEN  
      
         ---------------------------------------------
         -- V V V V V V V V V V V V V V V V V V V V V  
         ---------------------------------------------
      
         --This one is tricky
         --should be golden after we figure out which of the 2 big faces is in the middle
         --General rule for all that follows: Middle face is the one with only one edge shared with the small face
         --This will not change during looping, even after geometry modifications and face id changes
         --Can always stay grounded by the middle face<-->sliver boundary
         
         IF GZ_SMPOLY.COUNT_EDGES_BETWEEN_FACES(p_topology,
                                                small_face_id,
                                                p_big_face_id_1) = 1
         THEN
         
            inner_big_face_id := p_big_face_id_1;
            outer_big_face_id := p_big_face_id_2;
         
         ELSE
            
            inner_big_face_id := p_big_face_id_2;
            outer_big_face_id := p_big_face_id_1;
         
         END IF;
         
         
         --should be single edge
         --maybe would be better to call this the "middle edge"
         lateral_edge_1 := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                            small_face_id,
                                                            inner_big_face_id);
              
            
         --same, could be lots between inner and outer
         --Get just the edges(s) that extend into the interior
         --away from the inner-sliver joint edge                              
         extending_edges := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES_HINTED(p_topology,
                                                                    inner_big_face_id,
                                                                    outer_big_face_id,
                                                                    lateral_edge_1);
         IF extending_edges.COUNT = 1
         THEN
         
            --its the same edge and it loops around without interruption
            --must extend this looping edge on both its start and end
            extending_edges(2) := extending_edges(1);
            looping_extend_edge := 'Y';
            
         ELSIF extending_edges.COUNT = 2
         THEN
         
            --simpler
            looping_extend_edge := 'N';
         
         ELSIF extending_edges.COUNT > 2
         THEN
         
            RAISE_APPLICATION_ERROR(-20001, 'Couldnt resolve the extending edges. Instead of 2 ' 
                                            || ' got ' || extending_edges.COUNT);
         
         END IF;   
         
         
         FOR i IN 1 .. extending_edges.COUNT
         LOOP
         
            --2X for config V
            
            IF looping_extend_edge = 'N'
            THEN
            
               --can easily get the 2 lateral edge uniquely on each loop
               lateral_edge_2 := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES_HINTED(p_topology,
                                                                         small_face_id,
                                                                         outer_big_face_id,
                                                                         lateral_edge_1,
                                                                         extending_edges(i));    
                                                                   
            ELSE
            
               IF lateral_edge_2 IS NULL
               THEN
               
                  --first time through, get the 2 edges on either side 
                  lateral_edges_sto := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES_HINTED(p_topology,
                                                                                small_face_id,
                                                                                outer_big_face_id,
                                                                                lateral_edge_1,
                                                                                extending_edges(i)); 
                                                                               
                  IF lateral_edges_sto.COUNT <> 2
                  THEN
                  
                     RAISE_APPLICATION_ERROR(-20001,'Bomba! Should only be 2 edges, got ' || lateral_edges_sto.COUNT);
                  
                  END IF;
                  
                  lateral_edge_2 := lateral_edges_sto(1);
                                                                               
               ELSE
               
                  --second time, process the other side
                  lateral_edge_2 := lateral_edges_sto(2);
               
               END IF;
            
            END IF;                                                                     

            extend_node := GZ_SMPOLY.GET_NODE_BETWEEN_EDGES(p_topology,
                                                            lateral_edge_1,
                                                            lateral_edge_2);
                                                            
            --while we gots it. Will be dead after the lateral edges are removed
            p_dead_nodes(p_dead_nodes.COUNT + 1) := extend_node;
            
            new_node_id := GZ_SMPOLY.ADD_AN_OPPOSITE_NODE(p_topology,
                                                          small_face_id,
                                                          extend_node,
                                                          p_face_table,
                                                          lateral_edge_1,
                                                          lateral_edge_2);
                                                          
            new_edge_id := GZ_SMPOLY.ADD_NEW_EDGE(p_topology,
                                                  extend_node,
                                                  new_node_id,
                                                  extending_edges(i));
                           
            --this is an output                    
            p_new_edges(p_new_edges.COUNT + 1) := new_edge_id;
            
            --have to sort this out each time through the loop
            --since the face that is the original small face can flip to either side of new edge
            
            new_face_id := GZ_SMPOLY.GET_OPPOSITE_FACE(p_topology,
                                                       small_face_id,
                                                       new_edge_id);
                                                       
            --is there an edge between the inner big face and what we are calling the original small face    
            --must test with the inner face since it should always be 1:1 edgewise. Or not, on a flip. Binary      
            temp_num_array := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES(p_topology,
                                                                small_face_id,
                                                                inner_big_face_id);                                 
            IF i = 1
            THEN     
                                                
               IF temp_num_array.COUNT = 1
               THEN
                  
                  --the original small face is still under the inner face
                  --set this first chopped off pair, outer face -- new face
                  p_small_faces(p_small_faces.COUNT + 1) := new_face_id;
                  p_big_faces(p_big_faces.COUNT + 1) := outer_big_face_id;
                  
                  --lets not try to track this, just get the edge between the faces
                  --We need to get this edge now because the face ids will start mutating in the 
                  --remove_edge code later
                  p_dead_edges(p_dead_edges.COUNT + 1) := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                                                           new_face_id,
                                                                                           outer_big_face_id);
                  
               ELSIF temp_num_array.COUNT = 0
               THEN
               
                  --switched with the new edge insertion.  Freshly generated face is on the inside
                  --set this pair, outer face -- small face
                  p_small_faces(p_small_faces.COUNT + 1) := small_face_id;
                  p_big_faces(p_big_faces.COUNT + 1) := outer_big_face_id;
                  
                  p_dead_edges(p_dead_edges.COUNT + 1) := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                                                           small_face_id,
                                                                                           outer_big_face_id);
                  
                  --set the small face id to be the new face id for the next loop
                  small_face_id := new_face_id;
                                                               
               ELSE
            
                  RAISE_APPLICATION_ERROR(-20001, 'Got ' || temp_num_array.COUNT || ' edges between faces ' 
                                               || small_face_id || ' and inner ' || inner_big_face_id || '?');
            

               END IF;
               
            ELSIF i = 2
            THEN
            
               --theres gotta be a clever way to combine this section with i=1
            
               IF temp_num_array.COUNT = 1
               THEN
                  
                  --the original small face is still under the inner face
                  --set the final 2 pairs inner face -- small face and outer face -- new face
                  p_small_faces(p_small_faces.COUNT + 1) := small_face_id;
                  p_big_faces(p_big_faces.COUNT + 1) := inner_big_face_id;
                  
                  --set the matching edge between them
                  p_dead_edges(p_dead_edges.COUNT + 1) := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                                                           small_face_id,
                                                                                           inner_big_face_id);
                  --outer face -- new face                                                             
                  p_small_faces(p_small_faces.COUNT + 1) := new_face_id;
                  p_big_faces(p_big_faces.COUNT + 1) := outer_big_face_id;
                  
                  --set the matching edge between them
                  p_dead_edges(p_dead_edges.COUNT + 1) := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                                                           new_face_id,
                                                                                           outer_big_face_id);
                  
               ELSIF temp_num_array.COUNT = 0
               THEN
               
                  --switched with the new edge insertion.  Freshly generated face is on the inside
                  --set this pair, inner face -- new face, and outer face -- small face
                  p_small_faces(p_small_faces.COUNT + 1) := new_face_id;
                  p_big_faces(p_big_faces.COUNT + 1) := inner_big_face_id;
                  
                  --set edge between
                  p_dead_edges(p_dead_edges.COUNT + 1) := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                                                           new_face_id,
                                                                                           inner_big_face_id);
                                                                                           
                  --outer face -- small face                                                                      
                  p_small_faces(p_small_faces.COUNT + 1) := small_face_id;
                  p_big_faces(p_big_faces.COUNT + 1) := outer_big_face_id;
                  
                  --set edge between
                  p_dead_edges(p_dead_edges.COUNT + 1) := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                                                           small_face_id,
                                                                                           outer_big_face_id);
                                                               
               ELSE
            
                  RAISE_APPLICATION_ERROR(-20001, 'Got ' || temp_num_array.COUNT || ' edges between faces ' 
                                               || small_face_id || ' and inner ' || inner_big_face_id || '?');
            

               END IF;
               
            ELSE
            
               RAISE_APPLICATION_ERROR(-20001,'Shouldnt be 3 extending edges!');
            
            END IF;
            
         
         END LOOP;
                                                      
         
         --Result is 3 face pairs
         --and 2 new edge - dead nodes combos
         --Do the faces and edge/nodes need to be tied somehow?  Dont think so                                                   
         
      ELSIF p_merge_case = 'VII'
      THEN  
         
         ---------------------------------------------
         -- VII VII VII VII VII VII VII VII VII VII VII   
         ---------------------------------------------
         
         --the beginning here is almost  the same as IV
         
         lateral_edge_1 := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                            small_face_id,
                                                            p_big_face_id_1);
                                                   
         lateral_edge_2 := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                            small_face_id,
                                                            p_big_face_id_2);
          
         --not going to touch this territory yet
         --But we need to the edge ID to ensure that we dont accidentally connect to it                                                 
         lateral_edge_3 := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                            small_face_id,
                                                            p_big_face_id_3);
                                         
         --tuck this away for later use    
         --As the face$ face ids change we use the feature face sdo in some of the
         --add_an_opposite_node tests               
         feature_face_id := small_face_id;
                                                            
         extending_edges(1) := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                                p_big_face_id_1,
                                                                p_big_face_id_2);
                                                            
         extend_node := GZ_SMPOLY.GET_NODE_BETWEEN_EDGES(p_topology,
                                                         lateral_edge_1,
                                                         lateral_edge_2);
                                                         
         --while we got it. Will be dead after the lateral edges are removed
         p_dead_nodes(p_dead_nodes.COUNT + 1) := extend_node;
         
         --add the first new node
         new_node_id := GZ_SMPOLY.ADD_AN_OPPOSITE_NODE(p_topology,
                                                       small_face_id,
                                                       extend_node,
                                                       p_face_table,
                                                       lateral_edge_1,
                                                       lateral_edge_2,
                                                       lateral_edge_3,
                                                       feature_face_id);
                                                       
         new_edge_id := GZ_SMPOLY.ADD_NEW_EDGE(p_topology,
                                               extend_node,
                                               new_node_id,
                                               extending_edges(1));
                                               
         p_new_edges(p_new_edges.COUNT + 1) := new_edge_id;
         
         --first half of geometry work is complete, 
         --figure out the face ids and edges to be passed out
         
         p_big_faces(1)  := p_big_face_id_1;
         p_dead_edges(1) := lateral_edge_1;
                                                         
         --is there an edge between original small face and big 1?                                 
         temp_num_array := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES(p_topology,
                                                             small_face_id,
                                                             p_big_face_id_1);
          
         IF temp_num_array.COUNT = 1
         THEN
            
            --yes, original small face is cordoned off with big face 1
            p_small_faces(1) := small_face_id;
            
            --assign new face id to the continuing onward small face id variable
            small_face_id := GZ_SMPOLY.GET_OPPOSITE_FACE(p_topology,
                                                         small_face_id,
                                                         new_edge_id);
            
         ELSIF temp_num_array.COUNT = 0
         THEN
         
            --original small face has flipped.  New face is opposite big 1
            p_small_faces(1) := GZ_SMPOLY.GET_OPPOSITE_FACE(p_topology,
                                                            small_face_id,
                                                            new_edge_id);
            
            --no need to assign the small_face_id variable, its still set correctly
            
         ELSE
         
            RAISE_APPLICATION_ERROR(-20001, 'Got ' || temp_num_array.COUNT || ' edges between faces ' 
                                            || small_face_id || ' and ' || p_big_face_id_1 || '?');
         
         END IF;     
         
         
         --finish the geometry work, add one more node and edge
         --call again in case of mutation
         lateral_edge_3 := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                            small_face_id,
                                                            p_big_face_id_3);
                                                            
                                                            
         extending_edges(2) := GZ_SMPOLY.GET_EDGE_BETWEEN_FACES(p_topology,
                                                                p_big_face_id_2,
                                                                p_big_face_id_3);
                                                            
         extend_node := GZ_SMPOLY.GET_NODE_BETWEEN_EDGES(p_topology,
                                                         lateral_edge_2,
                                                         lateral_edge_3);
                                                         
         --NO. Node on this side will not be dead.  A piece of the small face lives on, as does this node
         --to another call to face_merge (config I)
         --Wont this vertex live on even after the subsequent call??
         --p_dead_nodes(p_dead_nodes.COUNT + 1) := extend_node;
         
         --add the second new node
         
         --ONLY 2 LATERALS?  Think so, its cordoned off
         new_node_id := GZ_SMPOLY.ADD_AN_OPPOSITE_NODE(p_topology,
                                                       small_face_id,
                                                       extend_node,
                                                       p_face_table,
                                                       lateral_edge_2,
                                                       lateral_edge_3,
                                                       NULL,             --no 3rd lateral
                                                       feature_face_id); --tucked away to get feature SDO
         --second new edge                                       
         new_edge_id := GZ_SMPOLY.ADD_NEW_EDGE(p_topology,
                                               extend_node,
                                               new_node_id,
                                               extending_edges(2));
                                               
         p_new_edges(p_new_edges.COUNT + 1) := new_edge_id;
         
         --Geometry work is complete
         --figure out the face ids and edges to be passed out
         
         p_big_faces(2)  := p_big_face_id_2;
         p_dead_edges(2) := lateral_edge_2;
                                                         
         --is there an edge between current "small face" and big 2?                                 
         temp_num_array := GZ_SMPOLY.GET_EDGES_BETWEEN_FACES(p_topology,
                                                             small_face_id,
                                                             p_big_face_id_2);
          
         IF temp_num_array.COUNT = 1
         THEN
            
            --yes, current small face is cordoned off with big face 2
            p_small_faces(2) := small_face_id;
            
            --No need to assign new face id, we are done

         ELSIF temp_num_array.COUNT = 0
         THEN
         
            --current small face has flipped.  New face is opposite big 2
            p_small_faces(2) := GZ_SMPOLY.GET_OPPOSITE_FACE(p_topology,
                                                            small_face_id,
                                                            new_edge_id);
            
         ELSE
         
            RAISE_APPLICATION_ERROR(-20001, 'Got ' || temp_num_array.COUNT || ' edges between faces ' 
                                            || small_face_id || ' and ' || p_big_face_id_1 || '?');
         
         END IF;     
                                                
      ELSE

         --for any new edge
         --the "start" of the edge will be a node we no longer want, including its x,y position
         --The "end" of the edge will be a new node that is not obsolete
         --Huh? Why this here

         RAISE_APPLICATION_ERROR(-20001,'No program');

      END IF;
      
      --LOG IT TO HECK
      log_message := 'After geometry work, Small face, Big face pairs with death row edges are ';
      
      FOR i IN 1 .. p_small_faces.COUNT
      LOOP
      
         log_message := log_message || '|' || p_small_faces(i) || ',' || p_big_faces(i) || ',' || ' edge ' || p_dead_edges(i);
      
      END LOOP;

      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,p_transaction_id,NULL,
                                             log_message);  
                                             
      IF p_new_edges.count > 0
      THEN
      
         log_message := 'New edges ';
         
         FOR i IN 1 .. p_new_edges.COUNT
         LOOP
         
            log_message := log_message || p_new_edges(i) || ',';
         
         END LOOP;
         
      ELSE
      
         log_message := 'No new edges';
      
      END IF;
      
      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,p_transaction_id,NULL,
                                             log_message);
         
      IF p_dead_nodes.COUNT > 0
      THEN         
                                 
         log_message := 'Dead nodes ';
         
         FOR i IN 1 .. p_dead_nodes.COUNT
         LOOP
         
            log_message := log_message || p_dead_nodes(i) || ',';
         
         END LOOP;
         
      ELSE
      
         log_message := 'No dead nodes';
         
      END IF;

      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,p_transaction_id,NULL,
                                             log_message);
                                             
   END MERGE_FACE_GEOMETRY_WORK;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   PROCEDURE UPDATE_SURVIVING_FACE (
      p_face_table         IN VARCHAR2,
      p_big_face           IN NUMBER,    --primitive face from geom work, should also match feature face
      p_small_face         IN NUMBER,    --primitive face from geom work, may match a feature face in VII or VIII
      p_merge_case         IN VARCHAR2
   )
   AS

      --Ditching this mess for UPDATE_FEATURE_FACE 11/27/12
       
      --Matt! 10/16/12
      --we removed an edge between two faces
      --All of the topogeoms are correct, including the face feature table topogeom
      --But the surviving face_id itself in the face feature table may be incorrect
      --   if the remove_edge call caused primitive face flippage
      --Set the feature face id to match the primitive face 

      psql                 VARCHAR2(4000);
      kount                PLS_INTEGER;
      primitive_face_id    NUMBER;

   BEGIN

      --first verify that only one face feature survived

      
      /*
      --Why?  The big should always be the suriving feature face
      --Except confusingly in VII and VIII both feature faces survive
      psql := 'SELECT a.face_id FROM '
            || p_face_table || ' a '
            || 'WHERE a.face_id IN (:p1, :p2)';

      EXECUTE IMMEDIATE psql INTO feature_face_id USING p_face_1,
                                                        p_face_2;
      */
      
      psql := 'SELECT COUNT(*) FROM '
            || p_face_table || ' a '
            || 'WHERE a.face_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_big_face;
                                                        
      IF kount = 1
      THEN

         --SOP 

         --get the primitive face id for whatever the surviving feature face is
         psql := 'SELECT t.topo_id FROM '
              || p_face_table || ' a, '
              || 'TABLE(a.topogeom.get_topo_elements()) t '
              || 'WHERE a.face_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO primitive_face_id USING p_big_face;

         --update no matter what, 50% of the time this is pointless

         psql := 'UPDATE ' || p_face_table || ' a '
              || 'SET a.face_id = :p1 '
              || 'WHERE a.face_id = :p2 ';

         EXECUTE IMMEDIATE psql USING primitive_face_id,
                                      p_big_face;

         COMMIT;

         --do measurments here since we have all that we need
         GZ_UTILITIES.GZ_POPULATE_MEASUREMENTS(p_face_table,
                                               'FACE_ID',
                                               'ALL',           --Face table has all the junk
                                               'ALL',
                                               .05,
                                               NULL,
                                               'FACE_ID',
                                               p_big_face);
                                               
      ELSIF kount = 0 
      AND p_merge_case = 'V'
      THEN
      
         --Case V has a big face small face pair like
         -- Asmall - Bbig 
         -- Csmall - Bbig
         
         --Where the big face is the same.  As a result, after two merges against the big face
         --we might end up with something like
         -- Asmall - Bbig --> Abig
         -- Csmall - (now Abig) --> Abig
         
         --when we look for the surviving face between original Asmall - Bbig our work is done
         --when we look for the surviving face between original Csmall - Bbig neither ID has survived 
         
         NULL;
      
      ELSE
      
         RAISE_APPLICATION_ERROR(-20001,'Didnt find a feature face left using ' || p_big_face || ','
                                        || p_small_face || ', ' || psql);
      
      END IF;


   END UPDATE_SURVIVING_FACE;
   
   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------
   
   PROCEDURE UPDATE_FEATURE_FACE (
      p_face_table         IN VARCHAR2,
      p_feature_face_id    IN NUMBER
   )
   AS
   
      --Matt! 11/27/12
      
      psql                 VARCHAR2(4000);
      kount                PLS_INTEGER;
      primitive_face_id    NUMBER;
      
      
   BEGIN
   
      psql := 'SELECT COUNT(*) FROM '
            || p_face_table || ' a '
            || 'WHERE a.face_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_feature_face_id;
   
      IF kount = 1
      THEN
 
         --This feature face is alive.  The primitive face underneath it may have changed
         --we want feature face ids to match the primitive face ids 1:1 like
         --get the primitive face id for whatever the surviving feature face is
         
         psql := 'SELECT t.topo_id FROM '
              || p_face_table || ' a, '
              || 'TABLE(a.topogeom.get_topo_elements()) t '
              || 'WHERE a.face_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO primitive_face_id USING p_feature_face_id;
         
         IF primitive_face_id <> p_feature_face_id
         THEN

            --update feature face id to match primitive face
            --would there ever be a primary key violation if the primitive face slid around?

            psql := 'UPDATE ' || p_face_table || ' a '
                 || 'SET a.face_id = :p1 '
                 || 'WHERE a.face_id = :p2 ';

            EXECUTE IMMEDIATE psql USING primitive_face_id,
                                         p_feature_face_id;

            COMMIT;
            
         END IF;

         --do measurements here since we have all that we need
         --This is usually a big face that has reshaped, but in some configs the 
         --little face also survives and reshapes
         GZ_UTILITIES.GZ_POPULATE_MEASUREMENTS(p_face_table,
                                               'FACE_ID',
                                               'ALL',           --Face table has all the junk
                                               'ALL',
                                               .05,
                                               NULL,
                                               'FACE_ID',
                                               primitive_face_id);  --primitive now is guaranteed to be feature
                                               
      ELSIF kount > 1
      THEN
      
         RAISE_APPLICATION_ERROR(-20001,'Yo, got ' || kount || ' feature faces with face_id ' || p_feature_face_id);
      
      ELSE
      
         --in most cases this is the sliver face.  Dont think theres anything we need to do
         NULL;
      
      END IF;
      
   END UPDATE_FEATURE_FACE;  

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   PROCEDURE UPDATE_EDITED_FSLS (
      p_topology           IN VARCHAR2,
      p_transaction_id     IN VARCHAR2,
      p_fsl_hash           IN GZ_TYPES.stringhash,
      p_face_table         IN VARCHAR2,
      p_fsl_pkc_col        IN VARCHAR2,
      p_log_type           IN VARCHAR2  --want to log all of these
   )
   AS

      --Matt! 10/16/12
      --hash is like
      -- Z699IN_FSL960V 9600000US5006360
      -- Z699IN_FSL160V 1600000US2567000;1600000US4419180;1600000US5010675

      mykey                VARCHAR2(4000);
      geoid_array          GZ_TYPES.stringarray;


   BEGIN

      mykey := p_fsl_hash.FIRST;

      LOOP

         EXIT WHEN NOT p_fsl_hash.EXISTS(mykey);

         --split off geoid array using ; delimiter
         geoid_array := GZ_UTILITIES.SPLIT(p_fsl_hash(mykey),';');

         IF mykey <> p_face_table --dont log face table
         THEN
         
            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,p_transaction_id,NULL,
                                                  'Updating measurements for ' || geoid_array.COUNT || ' record(s) in ' || mykey);
                                                  
         END IF;

         FOR i IN 1 .. geoid_array.COUNT
         LOOP

            IF mykey <> p_face_table  --face table updates in update_surviving_face
            THEN

               GZ_UTILITIES.GZ_POPULATE_MEASUREMENTS(mykey,
                                                     p_fsl_pkc_col,
                                                     'SDOGEOMETRY',  --feature tables only have SDO
                                                     'ALL',
                                                     .05,
                                                     NULL,
                                                     p_fsl_pkc_col,
                                                     geoid_array(i));

            END IF;


         END LOOP;

         mykey := p_fsl_hash.NEXT(mykey);

      END LOOP;



   END UPDATE_EDITED_FSLS;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   PROCEDURE MATCH_FSLS (
      p_topology           IN VARCHAR2,
      p_transaction_id     IN VARCHAR2,
      p_merge_case         IN VARCHAR2,
      p_log_type           IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_small_face_id      IN NUMBER,
      p_big_face_id        IN NUMBER,
      p_fsl_small_hash     IN GZ_TYPES.stringhash,
      p_fsl_big_hash       IN GZ_TYPES.stringhash,
      p_fsls_edited        IN OUT GZ_TYPES.stringhash,
      p_fsl_pkc_col        IN VARCHAR2,
      p_debug              IN NUMBER DEFAULT NULL
   )
   AS

      --Matt! 10/18/12
      --This is the heart of it
      --For any small-big face pair, manage the FSLS built on top to allow merge
      --Return running list of FSLS that have been edited

      --For 0-level FSLS:
      --When big and small hash both exist for a fsl but value is diff across the face divide
      --   +/- Add the small face to the big fsl, subtract the small face from the small FSL (may delete it)
      --When big exists and small doesnt, add the small face to the big FSL
      --When small exists and big doesnt, subtract the small face from the small FSL (may delete it)
      --For 1+ level FSLS:
      --When FSL (small or big) is in the hierarchy note it for measurement changes if its child FSL feature is edited in the 2 above

      fsl_key                 VARCHAR2(4000);
      higher_fsls             GZ_TYPES.stringarray;
      local_key_column        VARCHAR2(32);
      edited_record           NUMBER;
      fsls_extinct            GZ_TYPES.stringhash;
      fsls_in_hierarchy       GZ_TYPES.stringarray;


   BEGIN

      --get list of FSLS > 0 in hierarchy
      higher_fsls := GZ_SMPOLY.GET_FSLS(p_topology,
                                        0,    --greater than 0
                                        'Y'); --but allow for there to be nothing returned

      --roll through bigs first
      --1. When big and small hash both exist for a fsl but value is diff across the face divide
      --2. When big exists and small doesnt...


      fsl_key := p_fsl_big_hash.FIRST;

      LOOP

         EXIT WHEN NOT p_fsl_big_hash.EXISTS(fsl_key);


         IF fsl_key = p_face_table
         THEN

            --set this once, face_id column for face FSL, for less text below

            local_key_column := 'FACE_ID';

         ELSE

            local_key_column := p_fsl_pkc_col;

         END IF;


         IF NOT GZ_SMPOLY.IS_STRING_IN_ARRAY(fsl_key,        --skip highers till later
                                             higher_fsls)
         THEN

            IF p_fsl_small_hash.EXISTS(fsl_key)
            AND p_fsl_small_hash(fsl_key) = p_fsl_big_hash(fsl_key)
            THEN

               --make it obvious. DO NOTHING
               --FSL records for an FSL exist on both sides of the edge we are about to remove
               --and the geo_id is the same. We can remove the edge later and nothing changes
               NULL;

            ELSIF p_fsl_small_hash.EXISTS(fsl_key)
            AND p_fsl_small_hash(fsl_key) <> p_fsl_big_hash(fsl_key)
            THEN

               --1. When big and small hash both exist for a fsl but value is diff across the face divide
               -- +/-: Add the small face to the big fsl, subtract the small face from the small FSL
               --      Face table should always be in here unless we are in some weird face table future

               --no return value, this is a simple face add
               GZ_SMPOLY.ADD_A_FACE(p_topology,
                                    fsl_key,
                                    p_fsl_big_hash(fsl_key),  --the actual geo_id
                                    p_small_face_id,
                                    local_key_column);  --GEO_ID or FACE_ID

               --add this table,key to our list if not already on there (like a previous face edit call into here)
               p_fsls_edited := GZ_SMPOLY.MANAGE_FSL_LIST(p_fsls_edited,
                                                          fsl_key,
                                                          p_fsl_big_hash(fsl_key));

               --subtract the small face from the small fsl
               --This guy returns 1 for deleted a face, 0 for feature extinction
               edited_record := GZ_SMPOLY.DELETE_A_FACE(p_topology,
                                                        fsl_key,
                                                        p_fsl_small_hash(fsl_key),
                                                        p_small_face_id,
                                                        local_key_column, --face_id or geo_id
                                                        p_fsl_pkc_col);   --also pass in parent geo_id

               IF edited_record = 1
               OR edited_record = 0
               THEN

                  --lets add no matter what, so we can use this list for hierarchical
                  p_fsls_edited := GZ_SMPOLY.MANAGE_FSL_LIST(p_fsls_edited,
                                                             fsl_key,
                                                             p_fsl_small_hash(fsl_key));

                  IF edited_record = 0
                  THEN

                     --tuck this away. This FSL record no longer exists
                     --always unique, small face should only rep a single FSL geo_id per layer ever
                     fsls_extinct(fsl_key) := p_fsl_small_hash(fsl_key);

                     IF p_debug = 1
                     THEN
                        dbms_output.put_line('Extinct ' || fsl_key || ' ' || p_fsl_small_hash(fsl_key));
                     END IF;

                  END IF;

               ELSE

                  RAISE_APPLICATION_ERROR(-20001,'Unknown return value');

               END IF;


            ELSIF NOT p_fsl_small_hash.EXISTS(fsl_key)
            THEN

               --2. When big exists and small doesnt add the small face to the big FSL

               --no return value, this is a simple face add
               GZ_SMPOLY.ADD_A_FACE(p_topology,
                                    fsl_key,
                                    p_fsl_big_hash(fsl_key),  --the actual geo_id
                                    p_small_face_id,
                                    local_key_column);  --GEO_ID or FACE_ID

               --add this table,key to our list if not already on there (like a previous face edit call into here)
               p_fsls_edited := GZ_SMPOLY.MANAGE_FSL_LIST(p_fsls_edited,
                                                          fsl_key,
                                                          p_fsl_big_hash(fsl_key));

            ELSE

               RAISE_APPLICATION_ERROR(-20001,'Logic 101');

            END IF;

         END IF;

         --next big FSL
         fsl_key := p_fsl_big_hash.NEXT(fsl_key);

      END LOOP;


      --now roll through the smalls
      --3. When small exists and big doesnt, subtract the small face from the small FSL (may delete it)
      fsl_key := p_fsl_small_hash.FIRST;

      LOOP

         EXIT WHEN NOT p_fsl_small_hash.EXISTS(fsl_key);

         IF fsl_key = p_face_table
         THEN

            --set this once, face_id column for face FSL, for less text below

            local_key_column := 'FACE_ID';

         ELSE

            local_key_column := p_fsl_pkc_col;

         END IF;

         IF NOT p_fsl_big_hash.EXISTS(fsl_key)
         THEN

            --When small exists and big doesnt, subtract the small face from the small FSL (may delete it)
            --subtract the small face from the small fsl
            --This guy returns 1 for deleted a face, 0 for feature extinction
            edited_record := GZ_SMPOLY.DELETE_A_FACE(p_topology,
                                                     fsl_key,
                                                     p_fsl_small_hash(fsl_key),
                                                     p_small_face_id,
                                                     local_key_column, --face_id or geo_id
                                                     p_fsl_pkc_col);   --also pass in parent geo_id

            IF edited_record = 1
            OR edited_record = 0
            THEN

               --lets add no matter what, so we can use this list for hierarchical
               p_fsls_edited := GZ_SMPOLY.MANAGE_FSL_LIST(p_fsls_edited,
                                                          fsl_key,
                                                          p_fsl_small_hash(fsl_key));

               IF edited_record = 0
               THEN

                  --tuck this away. This FSL record no longer exists
                  --always unique, small face should only rep a single FSL geo_id per layer ever
                  fsls_extinct(fsl_key) := p_fsl_small_hash(fsl_key);

                  IF p_debug = 1
                  THEN
                     dbms_output.put_line('Extinct ' || fsl_key || ' ' || p_fsl_small_hash(fsl_key));
                  END IF;

               END IF;

            ELSE

               RAISE_APPLICATION_ERROR(-20001,'Unknown return value');

            END IF;

         END IF;


         --next small FSL
         fsl_key := p_fsl_small_hash.NEXT(fsl_key);

      END LOOP;

      --p_fsls_edited looks like
      -- Z699IN_FSL960V 9600000US5006360
      -- Z699IN_FSL160V 1600000US2567000;1600000US4419180;1600000US5010675

      --roll through the higher level FSLs and see if an edited FSL is below in the hierarchy

      FOR i IN 1 .. higher_fsls.COUNT
      LOOP

         fsls_in_hierarchy := GZ_SMPOLY.GET_FSLS_HIERARCHY(p_topology,
                                                           higher_fsls(i));

         --loop through looking for edited
         FOR j IN 1 .. fsls_in_hierarchy.COUNT
         LOOP

            IF j = 1
            THEN

               NULL; --returns self first in GET_FSLS_HIERARCHY array

            ELSE

               IF p_fsls_edited.EXISTS(fsls_in_hierarchy(j))
               THEN

                  --We should rarely get in here
                  --Lots of quickie looping above to avoid seeing this
                  --Still, all we know at this point is that an FSL record in a layer below had something happen to it

                  --MORE CODE PROBABLY GOES HERE
                  --SEE IF THE higher FSL really experienced an editable change based on the geoids in the edited list
                  --Also, need to note if the higher level FSL has also gone extinct
                  --   This extinction is currently handled in a handler of delete_a_face
                  --May be able to manage all of this in editing calcs later, we will see

                  IF p_fsl_small_hash.EXISTS(higher_fsls(i))
                  THEN

                     p_fsls_edited := GZ_SMPOLY.MANAGE_FSL_LIST(p_fsls_edited,
                                                                higher_fsls(i),
                                                                p_fsl_small_hash(higher_fsls(i)));

                  END IF;

                  IF p_fsl_big_hash.EXISTS(higher_fsls(i))
                  THEN

                     p_fsls_edited := GZ_SMPOLY.MANAGE_FSL_LIST(p_fsls_edited,
                                                                higher_fsls(i),
                                                                p_fsl_big_hash(higher_fsls(i)));

                  END IF;


               END IF;

            END IF;

         END LOOP;


      END LOOP;

      IF p_debug = 1
      THEN
         dbms_output.put_line('subtracting ' || fsls_extinct.COUNT || ' extinct fsls from edited table count ' || p_fsls_edited.COUNT);
      END IF;

      --now remove the extinct FSLS from our edited list
      p_fsls_edited := GZ_SMPOLY.SUBTRACT_HASH_UNIQUE(p_fsls_edited,
                                                      fsls_extinct);


      IF p_debug = 1
      THEN
         dbms_output.put_line('Now edited fsl table count ' || p_fsls_edited.COUNT);
      END IF;

   END MATCH_FSLS;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION FACE_MERGE (
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_topology           IN VARCHAR2,
      p_log_type           IN VARCHAR2,
      p_small_face_id      IN NUMBER,
      p_big_face_id        IN NUMBER,
      p_big_face_id_2      IN NUMBER DEFAULT NULL,
      p_big_face_id_3      IN NUMBER DEFAULT NULL,
      p_allow_extinction   IN VARCHAR DEFAULT 'N',
      p_expendable_tab     IN VARCHAR2 DEFAULT NULL,
      p_fsl_pkc            IN VARCHAR2 DEFAULT 'GEO_ID',
      p_debug              IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! 10/12/12

      --See wiki: https://collab.ecm.census.gov/div/geo/cpb/CAMPS%20Wiki/Generalization%20Merge%20Face.aspx
      --Given a sliver face that we don't want, merge that face into one (or more) neighboring faces,
      --updating all registered feature layers impacted by the face merge.

      --p_release            GZ release, like ACS12
      --p_project_id         GZ gen_project_id like Z6
      --p_topology           Topology, like Z699IN
      --p_small_face_id      The sliver face id we wish to dispatch
      --p_big_face_id        The big face that will take over the small faces territory. NULL if implied to be the only neighbor face
      --p_big_face_id_2      A second manifest destiny face
      --p_big_face_id_3      Rare third face. Indicates case VI or VII
      --p_allow_extinction   Y or N.  If N and removing a face removes the last piece of territory for a FSL
      --                     the merge will be disallowed. Face feature FSLs always excepted
      --p_expendable_tab     Table with exceptions to the allow_extinction = N rule.  For example, we put all water
      --                     records in this category sometimes
      --p_log_type           If incorporated into a production run this should be a module tag, like SP or CLIP
      --                     In standalone mode use 'MERGEFACE' to make (or add to an existing) special <topo>_mergeface_tracking
      --p_fsl_pkc            Primary key on all the FSL tables, usually geo_id.  Assumed to all be the same
      --p_debug              DBMS_OUTPUT junk control

      --SAMPLE: suggested usage in standalone mode.
      --Wrapper Error on FALSE (usually an endangered FSL) to be sure to catch it

      --DECLARE
      --output varchar2(32);
      --BEGIN
      --output := GZ_SMPOLY.FACE_MERGE('ACS12','Z6','Z699IN','MERGEFACE',15752,15761,NULL,NULL,'N','Z699IN_EXPENDABLE_FSLS');
      --
      --   IF output <> 'TRUE'
      --   THEN
      --      RAISE_APPLICATION_ERROR(-20001,output || ', see tracking table');
      --   END IF;
      --
      --END;


      transaction_id          VARCHAR2(4000);
      fsls_endangered         VARCHAR2(4000);
      output                  VARCHAR2(5) := 'FALSE';
      fsl_small_hash          GZ_TYPES.stringhash;
      fsl_big_hash            GZ_TYPES.stringhash;
      extinct_fsls            GZ_TYPES.stringhash;
      fsls_edited             GZ_TYPES.stringhash;
      ret_val                 VARCHAR2(4000);
      merge_case              VARCHAR2(4); --silly romans
      small_faces             GZ_TYPES.numberarray;
      big_faces               GZ_TYPES.numberarray;
      dead_edges              GZ_TYPES.numberarray;
      new_edges               GZ_TYPES.numberarray;
      dead_nodes              GZ_TYPES.numberarray;
      removed_edge_nodes      GZ_TYPES.numberarray;
      all_removed_edge_nodes  GZ_TYPES.numberarray;
      face_table              VARCHAR2(32);
      big_face_id_1           NUMBER;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MERGE_FACE: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --set transaction id
      IF p_big_face_id IS NOT NULL
      THEN

         big_face_id_1  := p_big_face_id;
         transaction_id := p_small_face_id || '|' || big_face_id_1;
      
      ELSE
      
         --single opposite face is the implied big face
         big_face_id_1 := GZ_SMPOLY.GET_SINGLE_OPPOSITE_FACE(p_topology,
                                                             p_small_face_id);
                                                             
         transaction_id := p_small_face_id || '|' || big_face_id_1;                                        
      
      END IF;

      IF p_big_face_id_2 IS NOT NULL
      THEN
         transaction_id := transaction_id || '|' || p_big_face_id_2;
      END IF;

      IF p_big_face_id_3 IS NOT NULL
      THEN
         transaction_id := transaction_id || '|' || p_big_face_id_3;
      END IF;


      --start tracking this transaction
      GZ_SMPOLY.START_MERGEFACE_LOGGING(p_topology,
                                        p_log_type,
                                        transaction_id);

      IF p_log_type = 'MERGEFACE'
      THEN

         --insert all the inputs into the tracker if in standalone mode
         --if part of a bigger module we will assume the module is managing inputs

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                'Inputs are (' || p_release || ',' || p_project_id || ','
                                                || p_topology || ',' || p_log_type || ','
                                                || p_small_face_id || ',' || big_face_id_1 || ',' || p_big_face_id_2 || ','
                                                || p_big_face_id_3 || ',' ||  p_allow_extinction || ',' || p_expendable_tab || ','
                                                || p_fsl_pkc || ',' || p_debug || ')',
                                                NULL,NULL,NULL,NULL,NULL,NULL,NULL);

      END IF;


      --CHECKS CHECKS

      IF p_big_face_id_3 IS NOT NULL
      AND p_big_face_id_2 IS NULL
      THEN

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                'Got a big face 3 but not a big face 2. Check inputs');

         RAISE_APPLICATION_ERROR(-20001,'Got a big face 3 but not a big face 2. Check inputs ');

      END IF;

      IF p_allow_extinction NOT IN ('Y','N')
      THEN

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                'Sorry, we key off of Y or N for p_allow_extinction.  Input is ' || p_allow_extinction);

         RAISE_APPLICATION_ERROR(-20001,'Sorry, we key off of Y or N for p_allow_extinction.  Input is ' || p_allow_extinction);

      END IF;


      IF p_allow_extinction = 'Y'
      AND p_expendable_tab IS NOT NULL
      THEN

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                'Got a table of expendable FSLs but allow extinction is set to Y. This is confusing');

         RAISE_APPLICATION_ERROR(-20001,'Got a table of expendable FSLs but allow extinction is set to Y. This is confusing');

      END IF;


      IF p_expendable_tab IS NOT NULL
      AND NOT GZ_UTILITIES.GZ_TABLE_EXISTS(p_expendable_tab)
      THEN

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                'Expendable table ' || p_expendable_tab || ' doesnt exist or is empty');

         RAISE_APPLICATION_ERROR(-20001,'Expendable table ' || p_expendable_tab || ' doesnt exist or is empty');

      END IF;
      
      --check for mistaken inputs and give a helpful message
      --I do this all the time, mistype a face id or whatever
      --small and big should touch
      
      IF GZ_SMPOLY.COUNT_EDGES_BETWEEN_FACES(p_topology,
                                             p_small_face_id,
                                             big_face_id_1) = 0
      THEN
      
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                'Looks like bad inputs, small face ' || p_small_face_id || ' and big face '
                                                || big_face_id_1 || ' dont even touch ');

         RAISE_APPLICATION_ERROR(-20001,'Looks like bad inputs, small face ' || p_small_face_id || ' and big face '
                                        || big_face_id_1 || ' dont even touch ');
         
      END IF;

      ------------------
      --set up
      ------------------

      --fed up, get it here. Should use in some of the subs below
      face_table := GZ_SMPOLy.GET_FACE_TABLE(p_topology,
                                             '%FSL%');

      ------------------------------------------------------
      --1. Check for endangered FSLs if requested
      --   get out before doing any work if theres a problem
      ------------------------------------------------------

      IF p_allow_extinction = 'N'
      THEN

         fsls_endangered := GZ_SMPOLY.FSLS_ENDANGERED(p_release,
                                                      p_project_id,
                                                      p_topology,
                                                      p_small_face_id,
                                                      p_fsl_pkc,
                                                      p_expendable_tab,
                                                      p_debug);

         --call FSLS_ENDANGERED to get a friendly text output
         --in here this is pass/fail

         IF fsls_endangered IS NOT NULL
         THEN

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                   'Exiting with FALSE. FSLS_ENDANGERED returned: ' || fsls_endangered);

            RETURN output;

         ELSE

            GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                   'Merging face ' || p_small_face_id || ' endangers no important FSLs');

         END IF;


      END IF;

      -------------------------------------------------
      --2. Determine configuration
      -------------------------------------------------

      merge_case := GZ_SMPOLY.DETERMINE_CONFIG(p_topology,
                                               transaction_id,
                                               p_log_type,
                                               p_small_face_id,
                                               big_face_id_1,
                                               p_big_face_id_2,
                                               p_big_face_id_3);

      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                             'Determined that this is configuration ' || merge_case);

      -------------------------------------------------------------------------------------
      --3. Get FSLs impacted by this face being removed
      --   Can do this before any geometry work, all will be the same after any small face subdivision
      -------------------------------------------------------------------------------------

      fsl_small_hash := GZ_SMPOLY.GET_FSLS_FOR_FACE(p_topology,
                                                    p_small_face_id,
                                                    p_fsl_pkc);


      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                             'Small face ' || p_small_face_id || ' reps ' || fsl_small_hash.COUNT || ' FSL records');



      ----------------------------------------------------------------------------------------
      --4. Geometry work for some cases here
      --   Will dice the sliver face into 2+ faces
      --   Should have no impact on feature tables
      --   But remember that the face feature table may temporarily have 1:many face-feature:face-primitives
      --   My plan is that the geometry work is the only section that must deal with cases
      --   Otherwise all other code works the same over pairs of faces or other lists of topo elements
      --      (slight exception in update_surviving_face possible)
      ----------------------------------------------------------------------------------------

      IF merge_case IN ('I','IV','V','VII')
      THEN


         GZ_SMPOLY.MERGE_FACE_GEOMETRY_WORK(p_topology,
                                            transaction_id,
                                            merge_case,
                                            face_table,
                                            p_small_face_id, --Note this face ID may not emerge, though it usually does in small_faces
                                            big_face_id_1,
                                            p_big_face_id_2,
                                            p_big_face_id_3,
                                            small_faces,     --in out  small and big faces are paired
                                            big_faces,       --in out  paired with small
                                            dead_edges,      --in out  synched with the big-small faces but that synching isnt used
                                            new_edges,       --in out  nothing using this yet, maybe other cases
                                            dead_nodes,      --in out  truly dead, not just obsolete
                                            p_log_type);
                                            
         IF big_faces.COUNT <> small_faces.COUNT 
         OR small_faces.COUNT <> dead_edges.COUNT
         THEN
         
            RAISE_APPLICATION_ERROR(-20001,'Got a mismatched count on big face, small face, dead edge ');
         
         END IF;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Not programmed');

      END IF;


      FOR i IN 1 .. big_faces.COUNT
      LOOP

         -------------------------------------------------
         --5. ID all feature tables for the big faces
         --   Must do this before removing edges since face ids will start mutating
         -------------------------------------------------


         fsl_big_hash := GZ_SMPOLY.GET_FSLS_FOR_FACE(p_topology,
                                                     big_faces(i),
                                                     p_fsl_pkc);


         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                               'Big face ' || big_faces(i) || ' reps ' || fsl_big_hash.COUNT || ' FSL records');

         ----------------------------------------------------------------
         --6. Add and subtract the sliver face feature-wise if necessary
         ----------------------------------------------------------------

         --Sub here to accepts the big_hash, small_hash, small_face_id and big_face_id
         --big face -- small face pairs
         --For 0-level FSLS:
         --When big and small hash both exist for a fsl but value is diff across the face divide
         --   +/- Add the small face to the big fsl, subtract the small face from the small FSL
         --When big exists and small doesnt, add the small face to the big FSL
         --When small exists and big doesnt, subtract the small face from the small FSL (may delete it)
         --For 1+ level FSLS:
         --When FSL (small or big) is in the hierarchy note it for measurement changes if its child FSL feature is edited in the 2 above


         GZ_SMPOLY.MATCH_FSLS(p_topology,
                              transaction_id,
                              merge_case,
                              p_log_type,
                              face_table,
                              small_faces(i),
                              big_faces(i),
                              fsl_small_hash,
                              fsl_big_hash,
                              fsls_edited,    --IN OUT running list of all FSLS edited including hierarchy Z699IN_FSL040V || 0400000US50;0400000US33
                              p_fsl_pkc,
                              p_debug);


      END LOOP;



      ----------------------------------------------------------
      --8. Delete the edge between big face(s) and small face(s)
      ----------------------------------------------------------
      

      FOR i IN 1 .. big_faces.COUNT
      LOOP

         --No reliance on big-small face pairs (they start mutating within the loop)
         --only edge ids are solid ground

         --The nodes at the start and end of the edge being removed may become obsolete, get passed back
         removed_edge_nodes := GZ_SMPOLY.REMOVE_SHARED_EDGE(p_topology, 
                                                            dead_edges(i));

         --The nodes on either end of the removed edge may be obsolete, but not necessarily so
         --Later we'll call remove obsolete nodes on each one and see what happens, dont need to know for certain
         --this adder will deduplicate
         all_removed_edge_nodes := GZ_SMPOLY.NUMBERARRAY_ADD_UNIQUE(all_removed_edge_nodes,
                                                                    removed_edge_nodes);

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                               'Removed edge between faces ' || big_faces(i) || ' and ' || small_faces(i) ||
                                               ' bounded by nodes ' || removed_edge_nodes(1) || ',' ||  removed_edge_nodes(2));

      END LOOP;


      ----------------------------------------------------
      --9. Delete any obsolete nodes and dead nodes
      ----------------------------------------------------

      --First totally annihilate any nodes that are dead
      IF dead_nodes.COUNT > 0
      THEN

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                'Deadening ' || dead_nodes.COUNT || ' dead nodes, includes edge reshape');
             
         --This calls my topofix obsolete node remover which
         --Removes the node and reshapes the edge to not include the vertex
                                           
         GZ_SMPOLY.REMOVE_OBSOLETE_NODES_LIST(p_topology,
                                              dead_nodes,
                                              'ZAP');


      ELSE

         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                               'No dead nodes to remove');


      END IF;

      --some of these nodes may be completely zapped by now
      --or just not obsolete

      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                               'Taking a crack at removing ' || all_removed_edge_nodes.COUNT ||
                                               ' possibly obsolete nodes');

      GZ_SMPOLY.REMOVE_OBSOLETE_NODES_LIST(p_topology,
                                           all_removed_edge_nodes,
                                           'REMOVE');  --use kindly "remove" option


      ----------------------------------------------------
      --10. Update face feature table with surviving face
      ----------------------------------------------------

      --of our initial inputs, some set of big face features and small face feature
      --have survived.  The primitive face ids under each survivor may have changed
      --The shape and other measurements may also have changed
      --I believe that in all configs all of the initial big face features still exist as records
      --The small face feature is usually gone, except in VII and VIII when it remains and we must
      --   at least update its measurements.  And its primitive face ID may be different
        
         

      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                'Updating face feature face ids and measurements in ' || face_table);
         
      --This subroutine is totally forgiving of feature faces that no longer exist
      
      GZ_SMPOLY.UPDATE_FEATURE_FACE(face_table, p_small_face_id);
      GZ_SMPOLY.UPDATE_FEATURE_FACE(face_table, big_face_id_1);  --dont use p_big_face_id_1 in case its null
      
      IF p_big_face_id_2 IS NOT NULL
      THEN
      
         GZ_SMPOLY.UPDATE_FEATURE_FACE(face_table, p_big_face_id_2);
      
      END IF;
      
      IF p_big_face_id_3 IS NOT NULL
      THEN
      
         GZ_SMPOLY.UPDATE_FEATURE_FACE(face_table, p_big_face_id_3);
      
      END IF;


      ---------------------------------------------------------
      --10. Update all measurements that may have been modified
      ---------------------------------------------------------

      IF fsls_edited.COUNT > 1
      THEN
      
         --should always include face
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                'Updating edited measurements for ' || (fsls_edited.COUNT - 1) || ' fsls');
                                                
      ELSE
      
         
         GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                                'No FSL measurements to update, only faces impacted');
      
      END IF;

      --This is the slowest step, especially when biggies like STATE and REGION are in the mix
      --When integrated into a program making lots of updates perhaps all measurements could be saved for one last go

      --call anyway even if just face in there. Wont do anything
      GZ_SMPOLY.UPDATE_EDITED_FSLS(p_topology,
                                   transaction_id,
                                   fsls_edited,
                                   face_table,
                                   p_fsl_pkc,
                                   p_log_type);

      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                             'Completed updating edited measurements');

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('MERGE_FACE: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_UTILITIES.GEN_EXTENDED_TRACKING_LOG(p_log_type,p_topology,transaction_id,NULL,
                                             'Exceptional Success! Exiting with TRUE');

      RETURN 'TRUE';


   END FACE_MERGE;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

END GZ_SMPOLY;
/

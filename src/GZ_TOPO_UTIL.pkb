CREATE OR REPLACE PACKAGE BODY GZ_TOPO_UTIL AS
------------------------------------------
--   NAME:
--      GZ_TOPO_UTIL
--
--   PURPOSE:
--      1. Copy a topology and all of its features
--      2. Verify if feature table records are in sync with primitives
--
--   USAGE:
--      ==============
--      Copy Topology:
--      ==============
--      Begin
--      GZ_topo_util.copy_topology(src_schema,
--                                  src_topology,
--                                  tgt_schema,
--                                  tgt_topology,
--                                  [PurgeTargetTopology_Flg Y/N -- Dflt N],
--                                  [Verify_Src_Ftre_Tbl_Flg Y/N -- Dflt Y]);
--      End;
--      /
--
--      ====================================
--      Verify Source Feature Table Records:
--      ====================================
--      Declare
--         src_schema varchar2(100) := 'ACS09GM';
--         src_topology varchar2(100) := 'MT';
--         res boolean;
--      Begin
--         res := GZ_topo_util.validate_feature_tables(src_schema, src_topology);
--         if res then
--            DBMS_OUTPUT.PUT_LINE('All Feature Tables are valid');
--         else
--            DBMS_OUTPUT.PUT_LINE('*****************************************');
--            DBMS_OUTPUT.PUT_LINE('Some Feature Table records are not valid');
--            DBMS_OUTPUT.PUT_LINE('Please check DBMS_OUTPUT');
--            DBMS_OUTPUT.PUT_LINE('*****************************************');
--         end if;
--
--      End;
--      /
--
--      ===============
--      Purge Topology:
--      ===============
--      Begin
--         GZ_topo_util.copy_topology(tgt_schema,tgt_topology);
--      End;
--      /
--
--      or
--      exec GZ_topo_util.copy_topology(tgt_schema,tgt_topology)
--
--
--
--
--   REVISIONS:
--   Ver        Date        Author           Description
--   ---------  ----------  ---------------  ------------------------------------
--   1.0        07/21/2010  Sreeni Karpurapu 1. Created this package.
-----------------------------------------------------
   PROCEDURE PURGE_TOPOLOGY(
      pTGT_SCHEMA IN VARCHAR2,
      pTGT_TOPOLOGY IN VARCHAR2,
      pFT_KEEP      IN VARCHAR2 DEFAULT NULL
   )    IS
      /*
      1.  User has to login to the target schema
      2.  Check to see if the topology exists
      3.  Delete all feature layers
      4.  Drop all feature tables
      5.  Drop Topology
      6.  Drop dollar tables
      */
      TGT_SCHEMA VARCHAR2(100)  :=  upper(pTGT_SCHEMA);
      TGT_TOPOLOGY VARCHAR2(100)  :=  upper(pTGT_TOPOLOGY);
      FTABLE_ARR MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
      SQL_STR VARCHAR2(4000);
      CURR_USER  VARCHAR2(100);
      TOPO_CNT NUMBER;
      v_code NUMBER;
      v_errm VARCHAR2(1000);
      PROCEDURE DELETE_TOPO_GEOMETRY_LAYER(
         TGT_SCHEMA IN VARCHAR2,
         TGT_TOPOLOGY IN VARCHAR2,
         TGT_TABLE IN VARCHAR2
      ) IS
      BEGIN
         DBMS_OUTPUT.PUT_LINE('DELETETOPOGEOMLAYER:' || TGT_SCHEMA || '-' || TGT_TOPOLOGY || '-' || TGT_TABLE);
         SDO_TOPO.DELETE_TOPO_GEOMETRY_LAYER( TGT_TOPOLOGY, TGT_TABLE,'TOPOGEOM');
      END;
      PROCEDURE DROP_TABLE(
         TGT_SCHEMA IN VARCHAR2,
         TGT_TABLE IN VARCHAR2
      ) IS
         SQL1 VARCHAR2(4000);
      BEGIN
         DBMS_OUTPUT.PUT_LINE('DROPTABLE:' || TGT_SCHEMA || '-' || TGT_TABLE);
         SQL1 := 'DROP TABLE ' || TGT_SCHEMA || '.' || TGT_TABLE || ' PURGE';
         Execute Immediate SQL1;
      END;
   BEGIN
      Select user into curr_user From dual;
      IF (user <> tgt_schema) THEN
         RAISE_APPLICATION_ERROR(-20001, 'You should be logged on to ' || TGT_SCHEMA || ' to run this program');
      END IF;
      -- Check if Source Topology exists
      Select count(*) into TOPO_CNT
        From all_sdo_topo_metadata
       Where owner = TGT_SCHEMA
         And topology = TGT_TOPOLOGY
         And rownum = 1;
      DBMS_OUTPUT.PUT_LINE('TGT TOPO_CNT: ' || TOPO_CNT);
      IF (TOPO_CNT = 0) THEN
         RAISE_APPLICATION_ERROR(-20001, 'Topology ' || TGT_TOPOLOGY || ' does not exist in ' || TGT_SCHEMA);
      ELSE
         DBMS_OUTPUT.PUT_LINE('Topology ' || TGT_TOPOLOGY || ' exists in ' || TGT_SCHEMA);
      END IF;
      SQL_STR := 'Select TABLE_NAME From USER_SDO_TOPO_INFO ';
      SQL_STR := SQL_STR || ' Where owner = :1 ';
      SQL_STR := SQL_STR || ' and topology = :2 ';
      SQL_STR := SQL_STR || ' and table_name is not null ';
      SQL_STR := SQL_STR || ' Order By tg_layer_id desc ';
      DBMS_OUTPUT.PUT_LINE(SQL_STR);
      EXECUTE IMMEDIATE SQL_STR BULK COLLECT INTO FTABLE_ARR USING TGT_SCHEMA, TGT_TOPOLOGY;
      IF FTABLE_ARR.COUNT > 0 THEN
         FOR i in FTABLE_ARR.FIRST..FTABLE_ARR.LAST LOOP
            DBMS_OUTPUT.PUT_LINE(FTABLE_ARR(i));
            DELETE_TOPO_GEOMETRY_LAYER(TGT_SCHEMA, TGT_TOPOLOGY, FTABLE_ARR(i));
            IF pFT_KEEP IS NULL
            THEN
               DROP_TABLE(TGT_SCHEMA, FTABLE_ARR(i));
            END IF;
         END LOOP;
      END IF;
      SDO_TOPO.DROP_TOPOLOGY(TGT_TOPOLOGY);
      DBMS_OUTPUT.PUT_LINE('DROPPED TOPOLOGY - ' || TGT_TOPOLOGY);
      Begin
         SQL_STR := 'PURGE TABLE ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_NODE$';
         DBMS_OUTPUT.PUT_LINE(SQL_STR);
         Execute Immediate SQL_STR;
      Exception
         When others then
            v_code := SQLCODE;
            v_errm := SQLERRM;
            DBMS_OUTPUT.PUT_LINE('Unable to purge  ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_NODE$ -- '|| v_code || ': ' || v_errm);
      End;
      Begin
         SQL_STR := 'PURGE TABLE ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_EDGE$';
         DBMS_OUTPUT.PUT_LINE(SQL_STR);
         Execute Immediate SQL_STR;
      Exception
         When others then
            v_code := SQLCODE;
            v_errm := SQLERRM;
            DBMS_OUTPUT.PUT_LINE('Unable to purge  ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_EDGE$ -- '|| v_code || ': ' || v_errm);
      End;
      Begin
         SQL_STR := 'PURGE TABLE ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_FACE$';
         DBMS_OUTPUT.PUT_LINE(SQL_STR);
         Execute Immediate SQL_STR;
      Exception
         When others then
            v_code := SQLCODE;
            v_errm := SQLERRM;
            DBMS_OUTPUT.PUT_LINE('Unable to purge  ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_FACE$ -- '|| v_code || ': ' || v_errm);
      End;
      Begin
         SQL_STR := 'PURGE TABLE ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_RELATION$';
         DBMS_OUTPUT.PUT_LINE(SQL_STR);
         Execute Immediate SQL_STR;
      Exception
         When others then
            v_code := SQLCODE;
            v_errm := SQLERRM;
            DBMS_OUTPUT.PUT_LINE('Unable to purge  ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_RELATION$ -- '|| v_code || ': ' || v_errm);
      End;
      Begin
         SQL_STR := 'PURGE TABLE ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_HISTORY$';
         DBMS_OUTPUT.PUT_LINE(SQL_STR);
         Execute Immediate SQL_STR;
      Exception
         When others then
            v_code := SQLCODE;
            v_errm := SQLERRM;
            DBMS_OUTPUT.PUT_LINE('Unable to purge  ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_HISTORY$ -- '|| v_code || ': ' || v_errm);
      End;
      FTABLE_ARR.DELETE;
   END;
   FUNCTION CREATE_TOPOLOGY(
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2,
      TGT_TOLERANCE IN NUMBER,
      TGT_SRID IN NUMBER
   ) RETURN NUMBER AS
      NEW_TOPOLOGY_ID NUMBER;
   BEGIN
      SDO_TOPO.CREATE_TOPOLOGY(TGT_TOPOLOGY, TGT_TOLERANCE, TGT_SRID);
      DBMS_OUTPUT.PUT_LINE('Successfully created topology ' || TGT_TOPOLOGY || ' in ' || TGT_SCHEMA || ' schema');
      Select TOPOLOGY_ID Into NEW_TOPOLOGY_ID
        From all_sdo_topo_metadata
       Where owner = TGT_SCHEMA
         And topology = TGT_TOPOLOGY
         And rownum = 1;
      RETURN NEW_TOPOLOGY_ID;
   END;
   PROCEDURE COPY_PRIMITIVES(
      SRC_SCHEMA IN VARCHAR2,
      SRC_TOPOLOGY IN VARCHAR2,
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2
   ) IS
      SQL1 VARCHAR2(4000);
   BEGIN
      SQL1 := 'INSERT INTO ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_NODE$ Select * From ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || '_NODE$';
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
      EXECUTE IMMEDIATE SQL1;
      DBMS_OUTPUT.PUT_LINE('Inserted data into ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_NODE$ from ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || '_NODE$');
      SQL1 := 'INSERT INTO ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_EDGE$ Select * From ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || '_EDGE$';
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
      EXECUTE IMMEDIATE SQL1;
      DBMS_OUTPUT.PUT_LINE('Inserted data into ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_EDGE$ from ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || '_EDGE$');
      SQL1 := 'INSERT INTO ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_FACE$ Select * From ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || '_FACE$';
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
      EXECUTE IMMEDIATE SQL1;
      DBMS_OUTPUT.PUT_LINE('Inserted data into ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_FACE$ from ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || '_FACE$');
      COMMIT;
   END;
   PROCEDURE CREATE_RELATION_SHELL(
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2
   ) IS
      /*
      Create an empty relation$ table and the zero partition
      */
      SQL1 VARCHAR2(4000);
   BEGIN
      SQL1 := '';
      SQL1 := SQL1 || ' CREATE TABLE ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_RELATION$ ';
      SQL1 := SQL1 || ' ( TG_LAYER_ID     NUMBER, ';
      SQL1 := SQL1 || '   TG_ID           NUMBER, ';
      SQL1 := SQL1 || '   TOPO_ID         NUMBER, ';
      SQL1 := SQL1 || '   TOPO_TYPE       NUMBER, ';
      SQL1 := SQL1 || '   TOPO_ATTRIBUTE  VARCHAR2(100 BYTE)) ';
      SQL1 := SQL1 || ' PARTITION BY LIST (TG_LAYER_ID)  ';
      SQL1 := SQL1 || ' (PARTITION TOPO_REL_0 VALUES (0) ';
      SQL1 := SQL1 || '   STORAGE    (  INITIAL         1M ';
      SQL1 := SQL1 || '                 MINEXTENTS      1 ';
      SQL1 := SQL1 || '                 MAXEXTENTS      2147483645 ';
      SQL1 := SQL1 || '                 BUFFER_POOL     DEFAULT)) ';
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
      EXECUTE IMMEDIATE SQL1;
      DBMS_OUTPUT.PUT_LINE('Successfully created ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_RELATION$ Shell');
   END;
   PROCEDURE CREATE_FEATURE_TABLE(
      SRC_SCHEMA IN VARCHAR2,
      SRC_FTABLE IN VARCHAR2,
      TGT_SCHEMA IN VARCHAR2,
      TGT_FTABLE IN VARCHAR2
   ) IS
   /*
     Create empty feature table and a temp feature table with all the records from the source feature table
   */
      SQL1 VARCHAR2(4000);
   BEGIN
      SQL1 := 'CREATE TABLE ' || TGT_SCHEMA || '.' || TGT_FTABLE || ' AS Select * From ' || SRC_SCHEMA || '.' || SRC_FTABLE || ' Where 1 = 2';
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
      EXECUTE IMMEDIATE SQL1;
      DBMS_OUTPUT.PUT_LINE('Successfully created ' || TGT_SCHEMA || '.' || TGT_FTABLE || ' feature table');
      SQL1 := 'CREATE TABLE ' || TGT_SCHEMA || '.' || TGT_FTABLE || 'X AS Select * From ' || SRC_SCHEMA || '.' || SRC_FTABLE;
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
      EXECUTE IMMEDIATE SQL1;
      DBMS_OUTPUT.PUT_LINE('Successfully created ' || TGT_SCHEMA || '.' || TGT_FTABLE || 'X table');
   END;
   PROCEDURE COPY_FEATURE_TABLE(
      SRC_SCHEMA IN VARCHAR2,
      SRC_FTABLE IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2,
      TGT_SCHEMA IN VARCHAR2,
      TGT_FTABLE IN VARCHAR2,
      TGT_LAYER_ID IN VARCHAR2
   ) IS
      /*
         1. Insert Feature table records from temp feature table
         2. Drop temp feature table
      */
      SQL1 VARCHAR2(4000);
      TOPO_COL VARCHAR2(400);  -- Name of the topogeom column
      Max_Seq  NUMBER;
      Seq_Name VARCHAR2(400);
   BEGIN
      --SQL1 := 'Insert into ' || TGT_SCHEMA || '.' || TGT_FTABLE || ' Select * From ' || SRC_SCHEMA || '.' || SRC_FTABLE;
      SQL1 := 'Insert into ' || TGT_SCHEMA || '.' || TGT_FTABLE || ' Select * From ' || TGT_SCHEMA || '.' || TGT_FTABLE || 'X';
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
      EXECUTE IMMEDIATE SQL1;
      COMMIT;
      DBMS_OUTPUT.PUT_LINE('Successfully inserted records into ' || TGT_SCHEMA || '.' || TGT_FTABLE || ' feature table');
      SQL1 := 'Drop table ' || TGT_SCHEMA || '.' || TGT_FTABLE || 'X PURGE';
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
      EXECUTE IMMEDIATE SQL1;
      DBMS_OUTPUT.PUT_LINE('Dropped Table ' || TGT_SCHEMA || '.' || TGT_FTABLE || 'X');
      SQL1 := 'Select Column_Name From User_Tab_Columns Where table_name = :1 and data_type = :2';
      EXECUTE IMMEDIATE SQL1 INTO TOPO_COL USING TGT_FTABLE, 'SDO_TOPO_GEOMETRY';
      SQL1 := 'SELECT NVL(MAX(a.' || TOPO_COL || '.tg_id),0) FROM ' || TGT_FTABLE || ' a';
      EXECUTE IMMEDIATE SQL1 INTO Max_Seq;
      Seq_Name := TGT_TOPOLOGY || '_TG_S' || to_char(TGT_LAYER_ID);
      EXECUTE IMMEDIATE 'DROP SEQUENCE ' || Seq_Name;
      EXECUTE IMMEDIATE 'CREATE SEQUENCE ' || Seq_Name || ' MINVALUE ' || to_char(Max_Seq+1) || ' NOCYCLE NOCACHE ';
   END;
   PROCEDURE UPDATE_TOPOLOGY_ID(
      TGT_SCHEMA IN VARCHAR2,
      TGT_FTABLE IN VARCHAR2,
      TOPOLOGY_ID IN NUMBER
   ) IS
      /*
        Update the topology id in the temp feature table
      */
      SQL1 VARCHAR2(4000);
   BEGIN
      SQL1 := 'UPDATE ' || TGT_SCHEMA || '.' || TGT_FTABLE || 'X F SET F.TOPOGEOM.TOPOLOGY_ID = :1';
      EXECUTE IMMEDIATE SQL1 USING TOPOLOGY_ID;
      COMMIT;
      DBMS_OUTPUT.PUT_LINE('Successfully updated ' || TGT_SCHEMA || '.' || TGT_FTABLE || 'X with topology_id ' || TOPOLOGY_ID);
   END;
   PROCEDURE ADD_RELATION_PARTITION(
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2,
      TGLAYER_ID IN NUMBER
   ) IS
     /*
        Add a partition for the feature table to the relation$ table
     */
      SQL1 VARCHAR2(4000);
   BEGIN
      SQL1 := '';
      SQL1 := SQL1 || ' Alter table ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_RELATION$ ';
      SQL1 := SQL1 || ' ADD PARTITION ' || TGT_TOPOLOGY || TGLAYER_ID || ' VALUES (' || TGLAYER_ID || ') ';
      SQL1 := SQL1 || ' STORAGE    ( ';
      SQL1 := SQL1 || '                 INITIAL         1M ';
      SQL1 := SQL1 || '                 MINEXTENTS      1 ';
      SQL1 := SQL1 || '                 MAXEXTENTS      2147483645 ';
      SQL1 := SQL1 || '                 BUFFER_POOL     DEFAULT ';
      SQL1 := SQL1 || '                ) ';
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1 || ' TGLAYER_ID: ' || TGLAYER_ID);
      EXECUTE IMMEDIATE SQL1; -- USING to_char(TGLAYER_ID);
   END;
   PROCEDURE COPY_RELATION_DATA(
      SRC_SCHEMA IN VARCHAR2,
      SRC_TOPOLOGY IN VARCHAR2,
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2,
      TGLAYER_ID IN NUMBER
   ) IS
      /*
        Copy relation$ table data for the feature table
      */
      SQL1 VARCHAR2(4000);
   BEGIN
      SQL1 := '';
      SQL1 := SQL1 || ' Insert into ' || TGT_SCHEMA || '.' || TGT_TOPOLOGY || '_RELATION$ ';
      SQL1 := SQL1 || ' Select * From ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || '_RELATION$ ';
      SQL1 := SQL1 || '  Where tg_layer_id = :1';
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1 || ' TGLAYER_ID: ' || TGLAYER_ID);
      EXECUTE IMMEDIATE SQL1 USING TGLAYER_ID;
      commit;
   END;
   PROCEDURE REGISTER_FEATURE_TABLE(
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2,
      FEATURE_TABLE IN VARCHAR2,
      TGLAYER_TYPE IN VARCHAR2,
      TGLAYER_ID IN NUMBER,
      CLAYER_ID NUMBER
   ) IS
   /*
      Register the feature table
   */
   BEGIN
      IF (CLAYER_ID IS NULL) THEN
         SDO_TOPO.ADD_TOPO_GEOMETRY_LAYER(TGT_TOPOLOGY, FEATURE_TABLE, 'TOPOGEOM', TGLAYER_TYPE, NULL, NULL, TGLAYER_ID);
      ELSE
         SDO_TOPO.ADD_TOPO_GEOMETRY_LAYER(TGT_TOPOLOGY, FEATURE_TABLE, 'TOPOGEOM', TGLAYER_TYPE, NULL, CLAYER_ID, TGLAYER_ID);
      END IF;
   END;
   PROCEDURE PROCESS_FEATURE(
      SRC_SCHEMA IN VARCHAR2,
      SRC_TOPOLOGY IN VARCHAR2,
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2,
      TOPOLOGY_ID IN NUMBER,
      SRC_FTABLE IN VARCHAR2,
      TGT_FTABLE IN VARCHAR2,
      TGLAYER_ID IN NUMBER,
      TGLAYER_TYPE IN VARCHAR2,
      CLAYER_ID NUMBER
   ) IS
   /*
      1. Create an empty feature table and a temp feature table with all source feature table records
      2. Update tmp feature table records with the new topology id
      3. Add a partition to the rel$ table
      4. Copy rel$ data
      5. Register the feature table
      6. Copy records from tmp feature table into the feature table.  Drop the tmp feature table.
   */
   BEGIN
      DBMS_OUTPUT.PUT_LINE('SRC FTable: ' || SRC_FTABLE || ' TGT FTable: ' || TGT_FTABLE ||' TGLAYER_ID: ' || TGLAYER_ID || ' CLAYER_ID: ' || CLAYER_ID);
      IF (CLAYER_ID IS NULL) THEN
         DBMS_OUTPUT.PUT_LINE('CLAYER_ID IS NULL');
      END IF;
      --Create an empty feature table and tmp table with all the feature records
      CREATE_FEATURE_TABLE(SRC_SCHEMA, SRC_FTABLE, TGT_SCHEMA, TGT_FTABLE);
      --Update feature records in the tem table.  Update topogeom.topology_id with new topology_id
      UPDATE_TOPOLOGY_ID(TGT_SCHEMA, TGT_FTABLE, TOPOLOGY_ID);
      --Alter rel$ table to add new matching partition
      ADD_RELATION_PARTITION(TGT_SCHEMA, TGT_TOPOLOGY, TGLAYER_ID);
      --Insert data into tgt rel$ From src rel$
      COPY_RELATION_DATA(SRC_SCHEMA, SRC_TOPOLOGY, TGT_SCHEMA, TGT_TOPOLOGY, TGLAYER_ID);
      --Register the feature (sdo_topo.add_topo_geometry_layer)
      REGISTER_FEATURE_TABLE(TGT_SCHEMA, TGT_TOPOLOGY, TGT_FTABLE, TGLAYER_TYPE, TGLAYER_ID, CLAYER_ID);
      --Copy the records from the tmp table to the feature table and drop the tmp table.
      COPY_FEATURE_TABLE(SRC_SCHEMA, SRC_FTABLE, TGT_TOPOLOGY, TGT_SCHEMA, TGT_FTABLE, TGLAYER_ID);
   END;
   PROCEDURE INIT_METADATA(
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2
   ) IS
   BEGIN
      SDO_TOPO.INITIALIZE_METADATA(TGT_TOPOLOGY );
   END;
   PROCEDURE VALIDATE_TOPOLOGY(
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2
   ) IS
      tmpstr varchar2(100);
   BEGIN
      tmpstr := sdo_topo_map.validate_topology(TGT_TOPOLOGY);
      DBMS_OUTPUT.PUT_LINE('Result of Validate Topology is ' || tmpstr);
   END;
   FUNCTION VALIDATE_FEATURE_TABLES(
      pSRC_SCHEMA IN VARCHAR2,
      pSRC_TOPOLOGY IN VARCHAR2
   ) RETURN BOOLEAN IS
      SRC_SCHEMA VARCHAR2(100) := upper(pSRC_SCHEMA);
      SRC_TOPOLOGY VARCHAR2(100) := upper(pSRC_TOPOLOGY);
      SQL1 varchar2(4000);
      SQL2 varchar2(4000);
      --SQL3 varchar2(4000);
      FTABLE_ARR MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
      TGLAYERID_ARR MDSYS.SDO_NUMBER_ARRAY := MDSYS.SDO_NUMBER_ARRAY();
      TGLAYERTYPE_ARR MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
      CLAYERID_ARR MDSYS.SDO_NUMBER_ARRAY := MDSYS.SDO_NUMBER_ARRAY();
      --SRC_FTABLE VARCHAR2(50);
      rec_count number;
      TYPE RefCursorType IS REF CURSOR;
      tgCursor     RefCursorType;
      tgid number;
      tglid number;
      valid boolean := true;
   BEGIN
      SQL1 := 'Select TABLE_NAME, TG_LAYER_ID, TG_LAYER_TYPE, CHILD_LAYER_ID ' ||
              '  From all_sdo_topo_metadata ' ||
              ' Where owner = :1 ' ||
              '   And topology = :2 ' ||
              ' Order By TG_LAYER_ID ';
      EXECUTE IMMEDIATE SQL1 BULK COLLECT INTO FTABLE_ARR, TGLAYERID_ARR, TGLAYERTYPE_ARR, CLAYERID_ARR USING SRC_SCHEMA, SRC_TOPOLOGY;
      IF FTABLE_ARR.COUNT > 0 THEN
         -- Check FTable Prefix.
         -- If the table name is prefixed with topology name, then replace the topology name
         -- else append the topology name
          FOR i in FTABLE_ARR.FIRST..FTABLE_ARR.LAST
          LOOP
             SQL1 := '';
             SQL1 := SQL1 || 'Select count(*) from (';
             SQL2 := '';
             SQL2 := SQL2 || 'select f.topogeom.tg_id, f.topogeom.tg_layer_id from ';
             SQL2 := SQL2 || SRC_SCHEMA || '.' || FTABLE_ARR(i) || ' f ';
             SQL2 := SQL2 || ' minus ';
             SQL2 := SQL2 || 'Select tg_id, tg_layer_id from ';
             SQL2 := SQL2 || SRC_SCHEMA || '.' || SRC_TOPOLOGY || '_relation$ ';
             SQL2 := SQL2 || ' Where tg_layer_id = :1';
             SQL1 := SQL1 || SQL2 || ')';
             Execute immediate SQL1 into rec_count using tglayerid_arr(i);
             if rec_count > 0 then
                DBMS_OUTPUT.PUT_LINE('Feature Table: ' || FTABLE_ARR(i) || ' has ' || rec_count || ' invalid records');
                OPEN tgCursor for SQL2 using tglayerid_arr(i);
                LOOP
                   FETCH tgCursor INTO tgid, tglid;
                   EXIT WHEN tgCursor%NOTFOUND;
                   DBMS_OUTPUT.PUT_LINE('TG_ID: ' || tgid || ' TG_LAYER_ID: ' || tglid || ' -- not found in rel$ table');
                END LOOP;
                CLOSE tgCursor;
                valid := false;
             else
                DBMS_OUTPUT.PUT_LINE('Feature Table: ' || FTABLE_ARR(i) || ' is valid');
             end if;
         END LOOP;
      ELSE
         DBMS_OUTPUT.PUT_LINE('Did not find any Feature Layers in Source Topology');
         RETURN TRUE;
      END IF;
      FTABLE_ARR.DELETE;
      TGLAYERID_ARR.DELETE;
      TGLAYERTYPE_ARR.DELETE;
      CLAYERID_ARR.DELETE;
      Return Valid;
   END;
   PROCEDURE COPY_TOPOLOGY(
      pSRC_SCHEMA IN VARCHAR2,
      pSRC_TOPOLOGY IN VARCHAR2,
      pTGT_SCHEMA IN VARCHAR2,
      pTGT_TOPOLOGY IN VARCHAR2,
      pPURGE_TGT_TOPOLOGY IN CHAR DEFAULT 'N',
      pVERIFY_SRC_TOPOLOGY IN CHAR DEFAULT 'Y'      
   ) IS
      
   BEGIN
   
      COPY_TOPOLOGY(
         pSRC_SCHEMA,
         pSRC_TOPOLOGY,
         pTGT_SCHEMA,
         pTGT_TOPOLOGY,
         pPURGE_TGT_TOPOLOGY,
         pVERIFY_SRC_TOPOLOGY,
         'Y'); --pCOPY_FTABLES
         
   END;
   PROCEDURE COPY_TOPOLOGY(
      pSRC_SCHEMA IN VARCHAR2,
      pSRC_TOPOLOGY IN VARCHAR2,
      pTGT_SCHEMA IN VARCHAR2,
      pTGT_TOPOLOGY IN VARCHAR2,
      pPURGE_TGT_TOPOLOGY IN CHAR, 
      pVERIFY_SRC_TOPOLOGY IN CHAR, 
      pCOPY_FTABLES IN CHAR
   ) IS
      SRC_SCHEMA VARCHAR2(100) := upper(pSRC_SCHEMA);
      SRC_TOPOLOGY VARCHAR2(100) := upper(pSRC_TOPOLOGY);
      TGT_SCHEMA VARCHAR2(100)  :=  upper(pTGT_SCHEMA);
      TGT_TOPOLOGY VARCHAR2(100)  :=  upper(pTGT_TOPOLOGY);
      PURGE_TGT_TOPOLOGY CHAR :=  upper(pPURGE_TGT_TOPOLOGY);
      --VERIFY_SRC_FTABLES CHAR :=  upper(pVERIFY_SRC_FTABLES);
      VERIFY_SRC_TOPOLOGY CHAR :=  upper(pVERIFY_SRC_TOPOLOGY);
      PRMTV VARCHAR2(25);
      FEATURE_TABLE VARCHAR2(50);
      CURR_USER VARCHAR2(50);
      NEW_TOPOLOGY_ID NUMBER;
      SQL1 VARCHAR2(4000);
      TOPO_CNT NUMBER(1);
      TGT_TOLERANCE NUMBER(20,16);
      TGT_SRID NUMBER(10);
      --TABLE_NAME, TG_LAYER_ID, CHILD_LAYER_ID
      FTABLE_ARR MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
      TGLAYERID_ARR MDSYS.SDO_NUMBER_ARRAY := MDSYS.SDO_NUMBER_ARRAY();
      TGLAYERTYPE_ARR MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
      CLAYERID_ARR MDSYS.SDO_NUMBER_ARRAY := MDSYS.SDO_NUMBER_ARRAY();
      SRC_FTABLE VARCHAR2(50);
      TGT_FTABLE VARCHAR2(50);
      VALID_SRC BOOLEAN;
      RETVAL   VARCHAR2(8000);
      ROW_CNT INTEGER;
      TMP_NUM INTEGER;

   BEGIN
      /*
      1.  Create target topology
          PURGE_TOPOLOGY(SRC_SCHEMA IN VARCHAR2, SRC_TOPOLOGY IN VARCHAR2);
          CREATE_TOPOLOGY(TGT_SCHEMA IN VARCHAR2, TGT_TOPOLOGY IN VARCHAR2);
      2.  Insert into target node$, edge$ and face$ tables by Selecting From source tables
          COPY_PRIMITIVES(SRC_SCHEMA IN VARCHAR2, SRC_TOPOLOGY IN VARCHAR2, TGT_SCHEMA IN VARCHAR2, TGT_TOPOLOGY IN VARCHAR2, PRMTV VARCHAR2);
      3.  Create a new rel$ shell
          CREATE_RELATION_SHELL(TGT_SCHEMA IN VARCHAR2, TGT_TOPOLOGY IN VARCHAR2);
      4.  Select new topology_id
      5.  Create each feature table
          PROCESS_FEATURE(SRC_SCHEMA IN VARCHAR2, SRC_TOPOLOGY IN VARCHAR2, TGT_SCHEMA IN VARCHAR2, TGT_TOPOLOGY IN VARCHAR2);
          a.  Create the feature table
                 COPY_FEATURE_TABLE(SRC_SCHEMA IN VARCHAR2, SRC_TOPOLOGY IN VARCHAR2, TGT_SCHEMA IN VARCHAR2, TGT_TOPOLOGY IN VARCHAR2, FEATURE_TABLE IN VARCHAR2)
          b.  Update topogeom.topology_id with new topology_id
                 UPDATE_TOPOLOGY_ID(TGT_SCHEMA IN VARCHAR2, TGT_TOPOLOGY IN VARCHAR2, FEATURE_TABLE IN VARCHAR2);
          c.  Alter rel$ table to add new matching partition
                 ADD_RELATION_PARTITION(SRC_SCHEMA IN VARCHAR2, SRC_TOPOLOGY IN VARCHAR2, TGT_SCHEMA IN VARCHAR2, TGT_TOPOLOGY IN VARCHAR2, FEATURE_TABLE IN VARCHAR2)
          d.  Insert data into tgt rel$ From src rel$
                 COPY_RELATION_DATA(SRC_SCHEMA IN VARCHAR2, SRC_TOPOLOGY IN VARCHAR2, TGT_SCHEMA IN VARCHAR2, TGT_TOPOLOGY IN VARCHAR2, FEATURE_TABLE IN VARCHAR2);
          e.  Register the feature (sdo_topo.add_topo_geometry_layer)
                 REGISTER_FEATURE_TABLE(TGT_SCHEMA IN VARCHAR2, TGT_TOPOLOGY IN VARCHAR2, FEATURE_TABLE IN VARCHAR2);
      6.  Sdo_topo.Initialize_metadata
          INIT_METADATA(TGT_SCHEMA IN VARCHAR2, TGT_TOPOLOGY IN VARCHAR2);
      */
      Select user into curr_user From dual;
      IF (user <> tgt_schema) THEN
         RAISE_APPLICATION_ERROR(-20001, 'You should be logged on to ' || TGT_SCHEMA || ' to run this program');
      END IF;
      -- Check if Source Topology exists
      Select count(*) into TOPO_CNT
        From all_sdo_topo_metadata
       Where owner = SRC_SCHEMA
         And topology = SRC_TOPOLOGY
         And rownum = 1;
      DBMS_OUTPUT.PUT_LINE('SRC TOPO_CNT: ' || TOPO_CNT);
      IF (TOPO_CNT = 0) THEN
         RAISE_APPLICATION_ERROR(-20001, 'Source Topology ' || SRC_TOPOLOGY || ' does not exist in ' || SRC_SCHEMA);
      ELSE
         DBMS_OUTPUT.PUT_LINE('Source Topology ' || SRC_TOPOLOGY || ' exists in ' || SRC_SCHEMA || '.... Going to the next step');
      END IF;

      -- 20110728
      -- Check if length of tablenames + 'X' (for internal temp tables) will be violated.
       Select  count(*) into ROW_CNT
        From all_sdo_topo_info
      where owner = SRC_SCHEMA
         and topology =  SRC_TOPOLOGY
         and (length(table_name) - length(topology) + length(TGT_TOPOLOGY)) >= 30;

      IF (ROW_CNT > 0) THEN
         Select max((length(table_name) - length(topology) + length(TGT_TOPOLOGY) + 1)-30) into TMP_NUM
           From all_sdo_topo_info
         Where owner = SRC_SCHEMA
            And topology =  SRC_TOPOLOGY;
         DBMS_OUTPUT.PUT_LINE('Please shorten the target topology name by ' || TMP_NUM || ' character(s)');
         DBMS_OUTPUT.PUT_LINE('Resulting target topology will have table names that will exceed the 30 char limit.  Please shorten the target topology name and try again');
         RAISE_APPLICATION_ERROR(-20001, 'Exceeding table name limit.  Please shorten the target topology name by alteast ' || TMP_NUM || ' character(s) and try again');

      END IF;



      -- Validate FTABLES
      IF (VERIFY_SRC_TOPOLOGY = 'Y') THEN
         Begin
            retVal := SDO_TOPO_MAP.VALIDATE_TOPOLOGY( SRC_SCHEMA ||'.'|| SRC_TOPOLOGY, 'TRUE', 1);
            If retVal <> 'TRUE' then
               dbms_output.put_line('Source Topology ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || ' is not valid.  Terminate Topology copy');
               RAISE_APPLICATION_ERROR(-20001, retVal);
            else
              dbms_output.put_line('Source Topology ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || ' is valid.  Continue with Topology copy');
            end if;
         Exception
            When Others Then

            If SQLERRM like '%java.lang.OutOfMemoryError%' Then
               dbms_output.put_line('Set Max Memory to 2147483648 and try again');
               SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(2147483648);
               retVal := SDO_TOPO_MAP.VALIDATE_TOPOLOGY( SRC_SCHEMA ||'.'|| SRC_TOPOLOGY, 'TRUE', 1);

               If retVal <> 'TRUE' then
                  dbms_output.put_line('Source Topology ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || ' is not valid.  Terminate Topology copy');
                  RAISE_APPLICATION_ERROR(-20001, retVal);
               else
                 dbms_output.put_line('Source Topology ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || ' is valid.  Continue with Topology copy');
               end if;

            Else

              retVal := 'Validation Failed due to ' || SQLCODE || '-' || SQLERRM;
              dbms_output.put_line(retVal);
              DBMS_OUTPUT.PUT_LINE('Stopping Topology Copy -- Source Topology ' || SRC_SCHEMA || '.' || SRC_TOPOLOGY || ' has invalid feature table records');
              DBMS_OUTPUT.PUT_LINE('If you want to ignore feature table verfication, re-run COPY_TOPOLOGY with VERIFY_SRC_FTABLES=N');

              RAISE_APPLICATION_ERROR(-20001, retVal);
            End If;
         End;

/*         VALID_SRC := VALIDATE_FEATURE_TABLES(SRC_SCHEMA, SRC_TOPOLOGY);
         IF (NOT VALID_SRC) THEN
         END IF;
*/

      END IF;
      -- Check if Target Topology exists
      Select count(*) into TOPO_CNT
        From all_sdo_topo_metadata
       Where owner = TGT_SCHEMA
         And topology = TGT_TOPOLOGY
         And rownum = 1;
      DBMS_OUTPUT.PUT_LINE('TGT TOPO_CNT: ' || TOPO_CNT);
      IF (TOPO_CNT >=1 AND PURGE_TGT_TOPOLOGY = 'Y') THEN
      --Purge target topology
         PURGE_TOPOLOGY(TGT_SCHEMA, TGT_TOPOLOGY);
      ELSIF (TOPO_CNT >=1 AND PURGE_TGT_TOPOLOGY = 'N') THEN
         RAISE_APPLICATION_ERROR(-20001, 'TARGET TOPOLOGY EXISTS...  Please re-run with Purge option=Y');
      ELSE
         DBMS_OUTPUT.PUT_LINE('Target Topology does not exist....  Going to the next step');
      END IF;
      Select Tolerance, SRID into TGT_TOLERANCE, TGT_SRID
        From all_sdo_topo_metadata
       Where owner = SRC_SCHEMA
         And topology = SRC_TOPOLOGY
         And rownum = 1;
      --Create target topology
      NEW_TOPOLOGY_ID := CREATE_TOPOLOGY(TGT_SCHEMA, TGT_TOPOLOGY, TGT_TOLERANCE, TGT_SRID);
      DBMS_OUTPUT.PUT_LINE('New Topology ID: ' || NEW_TOPOLOGY_ID);
      --Insert into target node$, edge$ and face$ tables by Selecting From source tables
      COPY_PRIMITIVES(SRC_SCHEMA, SRC_TOPOLOGY, TGT_SCHEMA, TGT_TOPOLOGY);
      --Create a new rel$ shell
      CREATE_RELATION_SHELL(TGT_SCHEMA, TGT_TOPOLOGY);
      --Initialize_metadata
      INIT_METADATA(TGT_SCHEMA, TGT_TOPOLOGY);
      --4.  Select new topology_id
      SQL1 := 'Select TABLE_NAME, TG_LAYER_ID, TG_LAYER_TYPE, CHILD_LAYER_ID ' ||
              '  From all_sdo_topo_metadata ' ||
              ' Where owner = :1 ' ||
              '   And topology = :2 ' ||
             -- ' AND table_name not in (''FSL400'', ''FSL510'')' ||
             -- ' Order By TG_LAYER_LEVEL, CHILD_LAYER_ID ';
              ' Order By TG_LAYER_ID ';
      EXECUTE IMMEDIATE SQL1 BULK COLLECT INTO FTABLE_ARR, TGLAYERID_ARR, TGLAYERTYPE_ARR, CLAYERID_ARR USING SRC_SCHEMA, SRC_TOPOLOGY;
      IF pCOPY_FTABLES = 'Y' and FTABLE_ARR.COUNT > 0 THEN
         -- Check FTable Prefix.
         -- If the table name is prefixed with topology name, then replace the topology name
         -- else append the topology name
         FOR i in FTABLE_ARR.FIRST..FTABLE_ARR.LAST LOOP
            --DBMS_OUTPUT.PUT_LINE('FTABLE: ' || FTABLE_ARR(i));
            SRC_FTABLE := FTABLE_ARR(i);
            IF (SRC_TOPOLOGY = substr(FTABLE_ARR(i), 1, length(SRC_TOPOLOGY))) THEN
               TGT_FTABLE := TGT_TOPOLOGY || substr(FTABLE_ARR(i), length(SRC_TOPOLOGY)+1, length(FTABLE_ARR(i)));
            ELSE
               TGT_FTABLE  := TGT_TOPOLOGY || '_' || FTABLE_ARR(i);
            END IF;
            --DBMS_OUTPUT.PUT_LINE('NEW FTABLE: ' || FTABLE_ARR(i));
            --Create each feature table
            PROCESS_FEATURE(SRC_SCHEMA, SRC_TOPOLOGY, TGT_SCHEMA, TGT_TOPOLOGY, NEW_TOPOLOGY_ID, SRC_FTABLE, TGT_FTABLE, TGLAYERID_ARR(i), TGLAYERTYPE_ARR(i), CLAYERID_ARR(i));
         END LOOP;
      ELSE
         DBMS_OUTPUT.PUT_LINE('Did not find any Feature Layers in Source Topology');
      END IF;
      --Initialize_metadata
      INIT_METADATA(TGT_SCHEMA, TGT_TOPOLOGY);
      -- Now rebuild all indexes
      REBUILD_INDEXES(TGT_TOPOLOGY);
      CREATE_INDEXES(pSRC_SCHEMA, pSRC_TOPOLOGY, pTGT_TOPOLOGY);
      --validate_topology
      --VALIDATE_TOPOLOGY(TGT_SCHEMA, TGT_TOPOLOGY);
      FTABLE_ARR.DELETE;
      TGLAYERID_ARR.DELETE;
      TGLAYERTYPE_ARR.DELETE;
      CLAYERID_ARR.DELETE;

      -- Call the fix_relation_dollar_indexes to make sure that we have the correct indexes.
      FIX_RELATION_DOLLAR_INDEXES(TGT_TOPOLOGY);

      -- Now gather stats on the %$ tables

      Begin

         GZ_UTILITIES.GATHER_TOPO_STATS(TGT_TOPOLOGY);


      Exception
          When others then
              retVal := 'Unable to gather table stats ' || SQLCODE || '-' || SQLERRM;
              dbms_output.put_line(retVal);

      End;

      Begin
         GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS', TGT_TOPOLOGY || '%');
         dbms_output.put_line('Done with GZ_PRIV_GRANTER');
      Exception
          When others then
              retVal := 'Unable to grant pivileges ' || SQLCODE || '-' || SQLERRM;
              dbms_output.put_line(retVal);
      End;

      DBMS_OUTPUT.PUT_LINE(' ');
      DBMS_OUTPUT.PUT_LINE(' ');
      DBMS_OUTPUT.PUT_LINE('============================');
      DBMS_OUTPUT.PUT_LINE('Successfully Copied Topology');
      DBMS_OUTPUT.PUT_LINE('Please validate new Topology');
      DBMS_OUTPUT.PUT_LINE('============================');
      DBMS_OUTPUT.PUT_LINE(' ');
      DBMS_OUTPUT.PUT_LINE(' ');
   END;

   Procedure Validate_TGID(
      pSCHEMA IN VARCHAR2,
      pTOPOLOGY IN VARCHAR2,
      pFTABLE IN VARCHAR2,
      pPK IN VARCHAR2,
      pOUTTABLE IN VARCHAR2,
      pTG_ID IN NUMBER,
      pTGL_ID IN NUMBER
   ) is
      SQL0 Varchar2(4000);
      SQL1 Varchar2(4000);
      SQL2 Varchar2(4000);
      TYPE RefCursorType IS REF CURSOR;
      Cur1     RefCursorType;
      tglvl number;
      topoid number;
      topotype number;
      prmCnt number;
      rowCnt number;
      iSQL Varchar2(4000) := 'Insert into ' || pOUTTABLE || ' Values (:1, :2, :3, :4, :5, :6, :7, :8, :9, :10)';
   Begin
      SQL1 := '';
      SQL1 := SQL1 || 'Select count(*) ';
      SQL2 := '';
      SQL2 := SQL2 || 'Select m.tg_layer_level, r.topo_id, r.topo_type ';
      SQL0 := '';
      SQL0 := SQL0 || '  From ' || pTopology || '_relation$ r, all_sdo_topo_metadata m ';
      SQL0 := SQL0 || ' Where m.owner = ''' || pSchema || '''';
      SQL0 := SQL0 || '   And m.topology = ''' || pTopology || '''';
      SQL0 := SQL0 || '   And m.tg_layer_id = r.tg_layer_id ';
      SQL0 := SQL0 || '   And r.tg_id = ' || pTG_ID;
      SQL0 := SQL0 || '   And r.tg_layer_id = ' || pTGL_ID;
      SQL1 := SQL1 || SQL0;
      --DBMS_OUTPUT.PUT_LINE('SQL1:');
      --DBMS_OUTPUT.PUT_LINE(SQL1);
      Execute immediate SQL1 into rowCnt;
      If rowCnt = 0 then
         --iSQL := '';
         --iSQL := iSQL || 'Insert into ' || pOUTTABLE || ' Values (:1, :2, :3, :4, :5, :6, :7, :8, :9, :10)';
         Execute Immediate iSQL using
            pTOPOLOGY,
            pFTABLE,
            pPK,
            pTG_ID,
            pTGL_ID,
            '-',
            '-',
            'Missing rel$ record',
            'F',
            sysdate;
         DBMS_OUTPUT.PUT_LINE('***ERROR*** Missing rel$ record for tgl_id: ' || pTGL_ID || ' tg_id: ' || pTG_ID);
         GOTO cont;
      End If;
      SQL2 := SQL2 || SQL0;
      --DBMS_OUTPUT.PUT_LINE('SQL2:');
      --DBMS_OUTPUT.PUT_LINE(SQL2);
      OPEN Cur1 for SQL2; -- using tglayerid_arr(i);
      LOOP
         FETCH Cur1 INTO tglvl, topoid, topotype;
         EXIT WHEN Cur1%NOTFOUND;
         --DBMS_OUTPUT.PUT_LINE('tglvl: ' || tglvl || ' topoid: ' || topoid || ' topotype: ' || topotype);
         Execute Immediate iSQL using
            pTOPOLOGY,
            pFTABLE,
            pPK,
            pTG_ID,
            tglvl,
            topoid,
            topotype,
            '-',
            'P',
            sysdate;
         if tglvl = 0 then
            --dbms_output.put_line('Level 0: ');
            if topotype = 3 then
               SQL2 := 'Select count(*) from ' || pSchema || '.' || pTopology || '_FACE$ Where face_id = :1';
               Execute Immediate SQL2 into prmCnt using topoid;
               --DBMS_OUTPUT.PUT_LINE('Level 0 prmCnt: ' || prmCnt);
            elsif topotype = 2 then
               SQL2 := 'Select count(*) from ' || pSchema || '.' || pTopology || '_EDGE$ Where edge_id = :1';
               Execute Immediate SQL2 into prmCnt using topoid;
               --DBMS_OUTPUT.PUT_LINE('Level 0 prmCnt: ' || prmCnt);
            else
               DBMS_OUTPUT.PUT_LINE('Bad TopoType');
            end if;
            If (prmCnt = 0) Then
               DBMS_OUTPUT.PUT_LINE('*****ERROR***** Missing primitive record for tgl_id: ' || pTGL_ID || ' tg_id: ' || pTG_ID);
               Execute Immediate iSQL using
                  pTOPOLOGY,
                  pFTABLE,
                  pPK,
                  pTG_ID,
                  tglvl,
                  topoid,
                  topotype,
                  'Missing primitive record',
                  'F',
                  sysdate;
            End If;
         else
            Validate_TGID(pSchema, pTopology, pFTable, pPK, pOUTTABLE, topotype, topoid);
         end if;
      END LOOP;
      Close Cur1;
      <<cont>>
         null;
   End;
   PROCEDURE VALIDATE_FTABLE(
      pSCHEMA IN VARCHAR2,
      pTOPOLOGY IN VARCHAR2,
      pFTABLE IN VARCHAR2,
      pPK IN VARCHAR2,
      pOUTTABLE IN VARCHAR2
   ) IS
/*
   This procedure calls the Validate_TGID procedure for each record in the Feature Table.
   Usage: exec GZ_topo_util.validate_ftable('KARPU001', 'MT91', 'MT91_FSL050', 'OID', 'VAL_OUT_TBL1')
            To Select records that failed validation
                select * from val_out_tbl1 where status = 'F';
   1. Creates Output table, if it doesn't exist
   2. Delete data that corresponds to the Feature Table in the Outtable
   3. Select each record from the Feature Table and call Validate_TGID
*/
      vSchema Varchar2(100) := UPPER(pSCHEMA);  --'KARPU001';
      vTopology Varchar2(100) := UPPER(pTOPOLOGY); --'MT91';
      vFTable Varchar2(100) := UPPER(pFTABLE); -- 'MT91_FSL050';
      vPK Varchar2(200) := UPPER(pPK); --'OID';
      vOUTTABLE Varchar2(50) := UPPER(pOUTTABLE); -- 'VAL_OUT_TBL';
      vPK1 Varchar2(200);
      vTG_ID Number; --:= 529;
      vTGL_ID Number; --:= 8;
      SQL1 Varchar2(4000);
      rcdCtr Number;
      TYPE RefCursorType IS REF CURSOR;
      Cur1     RefCursorType;
      CommitFreq Number := 500;
   BEGIN
         -- See if the output table exists
         SQL1 := '';
         SQL1 := SQL1 || 'Select count(*) From ALL_OBJECTS ';
         SQL1 := SQL1 || ' Where Object_Type = ''TABLE'' ';
         SQL1 := SQL1 || '   And Owner = ''' || vSchema || '''';
         SQL1 := SQL1 || '   And Object_Name = ''' || vOUTTable || '''';
         -- DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
         Execute Immediate SQL1 into rcdCtr;
         --DBMS_OUTPUT.PUT_LINE('rcdCtr: ' || rcdCtr);
         If rcdCtr = 0 Then
            -- Output table does not exist. Create the table
            SQL1 := '';
            SQL1 := SQL1 || 'Create Table ' || vOUTTABLE;
            SQL1 := SQL1 || '(TOPOLOGY VARCHAR2(25), ';
            SQL1 := SQL1 || 'FTABLE VARCHAR2(50), ';
            SQL1 := SQL1 || 'PK  VARCHAR2(50), ';
            SQL1 := SQL1 || 'TG_ID NUMBER, ';
            SQL1 := SQL1 || 'TG_LAYER_LVL NUMBER, ';
            SQL1 := SQL1 || 'TOPO_ID NUMBER, ';
            SQL1 := SQL1 || 'TOPO_TYPE NUMBER, ';
            SQL1 := SQL1 || 'COMMENTS VARCHAR2(200), ';
            SQL1 := SQL1 || 'STATUS VARCHAR2(10), ';
            SQL1 := SQL1 || 'TIMESTAMP DATE ';
            SQL1 := SQL1 || ')';
            --dbms_output.put_line('SQL1: ' || SQL1);
            EXECUTE IMMEDIATE SQL1;
         Else
            -- Output table exists.  Delete any records that corre
            SQL1 := 'Delete from ' || vOUTTABLE || ' Where FTABLE = :1';
            Execute Immediate SQL1 using vFTable;
            COMMIT;
         End If;
         SQL1 := '';
         SQL1 := SQL1 || ' Select ''' || vPK || '='' || f.' || vPK || ', ';
         SQL1 := SQL1 || '  f.topogeom.tg_id, f.topogeom.tg_layer_id';
         SQL1 := SQL1 || '   From ' || vFTable || ' f';
         --SQL1 := SQL1 || '  Where rownum = 1';
         --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
         rcdCtr := 0;
         OPEN Cur1 for SQL1; -- using tglayerid_arr(i);
         LOOP
            FETCH Cur1 INTO vPK1, vTG_ID, vTGL_ID;
            EXIT WHEN Cur1%NOTFOUND;
            rcdCtr := rcdCtr + 1;
            --DBMS_OUTPUT.PUT_LINE('vPK1: ' || vPK1 || ' vTG_ID: ' || vTG_ID || ' vTGL_ID: ' || vTGL_ID);
            Validate_TGID(vSchema, vTopology, vFTable, vPK1, vOUTTABLE, vTG_ID, vTGL_ID);
            If rcdCtr = CommitFreq then
               Commit;
               rcdCtr := 0;
            End If;
         END LOOP;
         Close Cur1;
         COMMIT;
   END;
   PROCEDURE REBUILD_INDEXES(
      pTOPOLOGY IN VARCHAR2
   ) IS
      vTopology Varchar2(100) := UPPER(pTOPOLOGY); --'MT91';
      SQL1 Varchar2(4000);
      SQL2 Varchar2(4000);
      INDEX_NAME Varchar2(100);
      TYPE RefCursorType IS REF CURSOR;
      Cur1     RefCursorType;
   Begin
      SQL1 := 'Select Index_Name FROM USER_INDEXES WHERE TABLE_NAME LIKE ''' || vTopology || '%'' AND INDEX_TYPE = ''DOMAIN''';
      --DBMS_OUTPUT.PUT_LINE(SQL1);
      OPEN Cur1 for SQL1; -- using tglayerid_arr(i);
      LOOP
         FETCH Cur1 INTO INDEX_NAME;
         EXIT WHEN Cur1%NOTFOUND;
         SQL2 := 'Alter Index ' || Index_Name || ' ReBuild';
         --DBMS_OUTPUT.PUT_LINE(SQL2);
         Execute Immediate SQL2;
      END LOOP;
      Close Cur1;
      DBMS_OUTPUT.PUT_LINE('Successfully rebuilt all indexes');
   End;

   PROCEDURE CREATE_INDEXES (
         pSRC_SCHEMA        IN VARCHAR2,
         pSRC_TOPOLOGY    IN VARCHAR2,
         pTGT_TOPOLOGY    IN VARCHAR2)
   IS
      vSTopology    VARCHAR2 (100) := UPPER (pSRC_TOPOLOGY);
      vTTopology    VARCHAR2 (100) := UPPER (pTGT_TOPOLOGY);
      SQL1         VARCHAR2 (4000);
      SQL2         VARCHAR2 (4000);
      SQL3         VARCHAR2 (4000);
      SQL4         VARCHAR2 (4000);
      SQL5         VARCHAR2 (4000);
      index_name   VARCHAR2 (100);
      index_type   VARCHAR2 (100);
      table_name   VARCHAR2 (100);
      col_list   VARCHAR2 (100);
      sdo_name   VARCHAR2 (30);
      idx_count   NUMBER;
      tab_count   NUMBER;

      TGT_TOLERANCE     NUMBER;
      TGT_SRID               NUMBER;
      
      TYPE RefCursorType IS REF CURSOR;

      Cur1         RefCursorType;
   BEGIN
      SQL1 := 'SELECT Index_Name, index_type, table_name
                     FROM ALL_INDEXES 
                   WHERE TABLE_NAME LIKE '''
            || vSTopology
            || '\_%'' ESCAPE ''\'' AND TABLE_NAME NOT LIKE''%$'''
            || ' AND owner = :1'
            || ' AND index_type != ''DOMAIN''';

      SQL2 := 'Select COUNT(1) FROM USER_INDEXES WHERE  index_name = :1';
      SQL5 := 'Select COUNT(1) FROM USER_TABLES WHERE  table_name = :1';

      --DBMS_OUTPUT.PUT_LINE(SQL1);
      OPEN Cur1 FOR SQL1 USING pSRC_SCHEMA; 

      LOOP
         FETCH Cur1 INTO index_name, index_type, table_name;

         EXIT WHEN Cur1%NOTFOUND;
         col_list := GZ_TOPO_UTIL.GET_IND_COLUMNS(pSRC_SCHEMA, table_name, index_name);

         IF (vSTopology <> vTTopology) THEN
            table_name := REPLACE (table_name, vSTopology||'_', vTTopology||'_');
            index_name := REPLACE (index_name, vSTopology||'_', vTTopology||'_');
         END IF;

         IF (index_type =  'NORMAL') THEN
            index_type := '';
         END IF;
                  
         EXECUTE IMMEDIATE SQL5 INTO tab_count USING table_name;
         EXECUTE IMMEDIATE SQL2 INTO idx_count USING index_name;
         IF (idx_count = 0) AND (tab_count > 0) THEN
            SQL3 := 'CREATE ' ||index_type||' INDEX ' ||index_name
                     || ' ON '||table_name||'( ' ||col_list|| ') NOLOGGING';
            EXECUTE IMMEDIATE SQL3;           
         END IF;
         
      END LOOP;

      CLOSE Cur1;

      SQL1 := 'SELECT a.table_name, a.index_name, a.column_name
                      FROM all_ind_columns a 
                     WHERE  a.index_owner = :1      
                         AND a.column_position = 1               
                         AND a.table_name LIKE ''' || vSTopology || '\_%'' ESCAPE ''\'' 
                         AND a.table_name NOT LIKE''%$''
                         AND a.column_name =
                                (SELECT column_name
                                   FROM all_tab_columns b
                                 WHERE     b.owner = a.index_owner
                                      AND b.data_type = ''SDO_GEOMETRY''
                                      AND b.table_name = a.table_name
                                      AND b.column_name = a.column_name)';  

      SQL4 := ' SELECT sdo_tolerance, srid ' 
            || ' FROM all_sdo_geom_metadata m, TABLE (m.diminfo) t '
            || ' WHERE m.table_name = :1 '
            || '     AND  m.column_name = :2'
            || '     AND  m.owner = :3'
            || '     AND ROWNUM = 1';
            
      --DBMS_OUTPUT.PUT_LINE(SQL1);
      OPEN Cur1 FOR SQL1 USING pSRC_SCHEMA; 

      LOOP
         FETCH Cur1 INTO table_name , index_name, sdo_name;
         EXIT WHEN Cur1%NOTFOUND;

         EXECUTE IMMEDIATE SQL4 INTO TGT_TOLERANCE, TGT_SRID
               USING table_name, sdo_name, pSRC_SCHEMA;         
         
          IF (vSTopology <> vTTopology) THEN
            table_name := REPLACE (table_name, vSTopology||'_', vTTopology||'_');
            index_name := REPLACE (index_name, vSTopology||'_', vTTopology||'_');
         END IF;
         
         EXECUTE IMMEDIATE SQL5 INTO tab_count USING table_name;
         EXECUTE IMMEDIATE SQL2 INTO idx_count USING index_name;
         IF (idx_count = 0) AND (tab_count > 0) THEN         
                  
            SQL3 := 'DELETE FROM user_sdo_geom_metadata ' 
                     || ' WHERE table_name = :1 AND  column_name = :2 ';
            EXECUTE IMMEDIATE SQL3 USING table_name, sdo_name;        
            COMMIT;   
            GZ_UTILITIES.ADD_SPATIAL_INDEX(table_name, sdo_name, TGT_SRID, TGT_TOLERANCE, NULL, NULL, index_name);
            
         END IF;
         
      END LOOP;

      CLOSE Cur1;      
      
   END CREATE_INDEXES;
   
   FUNCTION GET_IND_COLUMNS (pSchema        IN VARCHAR2,
                             pTable_name    IN VARCHAR2,
                             pIndexl_name   IN VARCHAR2)
      RETURN VARCHAR2
   AS
      SQL1       VARCHAR2 (4000);
      vResult    VARCHAR2 (100) := '';
      vColName   VARCHAR2 (100) := '';
      vOrder     NUMBER;

      TYPE RefCursorType IS REF CURSOR;

      Cur1       RefCursorType;
   BEGIN
      SQL1 :=
            'SELECT column_Name, column_position 
                        FROM all_ind_columns 
                      WHERE index_owner = :1 '
         || '      AND table_name = :2'
         || '      AND index_name = :3'
         || ' ORDER BY column_position';

      OPEN Cur1 FOR SQL1 USING pSchema, pTable_name, pIndexl_name;

      LOOP
         FETCH Cur1
         INTO vColName, vOrder;

         EXIT WHEN Cur1%NOTFOUND;

         IF (vOrder = 1)
         THEN
            vResult := vColName;
         ELSE
            vResult := vResult || ', ' || vColName;
         END IF;
      END LOOP;

      CLOSE Cur1;

      RETURN vResult;
   END GET_IND_COLUMNS;   
   
   Procedure FIX_CnnctByLp_Err(
      pTOPOLOGY IN varchar2,
      pTABLE_NAME IN Varchar2,
      pPKCol IN Varchar2,
      pPKVal IN Number,
      pSDOTopoGeomCol IN Varchar2
   ) IS
      vTOPOLOGY varchar2(100) := pTOPOLOGY;
      vTABLE_NAME Varchar2(100) := pTABLE_NAME;
      vPKCol Varchar2(100) := pPKCol;
      vPKVal Number  := pPKVal;
      vSDOTopoGeomCol Varchar2(100) := pSDOTopoGeomCol;
      TopoMap Varchar2(100) := vTOPOLOGY || '_TOPOMAP';
      tgid Number;
      tglid Number;
      newPKVal Number;
      SQL1 varchar2(4000);
      SQL2 varchar2(4000);
      ISQL1 varchar2(4000);
      tmpStr Varchar2(100);
      COL_ARR MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
      COL_TYPE_ARR MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();
   Begin
      SQL1 := 'select f.' || vSDOTopoGeomCol || '.tg_id, f.' || vSDOTopoGeomCol || '.tg_layer_id From ' || vTABLE_NAME || ' f where ' || vPKCol || ' = ' || vPKVal;
      dbms_output.put_line(SQL1);
      Execute Immediate SQL1 Into tgid, tglid;
      dbms_output.put_line('tgid: ' || tgid || ' tglid: ' || tglid);
      SQL1 := 'select max(' || vPKCol || ')+1 from ' || vTABLE_NAME;
      dbms_output.put_line(SQL1);
      Execute Immediate SQL1 Into newPKVal;
      dbms_output.put_line('newPKVal: ' || newPKVal);
      SQL1 := 'select column_name, data_type From user_tab_columns where table_name = :1 order by column_id';
      dbms_output.put_line(SQL1);
      Execute Immediate SQL1 BULK COLLECT into COL_ARR, COL_TYPE_ARR Using vTABLE_NAME;
      ISQL1 := 'Insert into ' || vTABLE_NAME || ' Select ';
      FOR i in COL_ARR.FIRST..COL_ARR.LAST
      LOOP
         --DBMS_OUTPUT.PUT_LINE(COL_ARR(i) || ': ' || COL_TYPE_ARR(i));
         IF COL_ARR(i) = vPKCol THEN
            tmpStr := newPKVal;
         ELSIF COL_TYPE_ARR(i) = 'SDO_TOPO_GEOMETRY' THEN
           tmpStr := 'NULL';
         ELSE
           tmpStr := COL_ARR(i);
         END IF;
         IF i<>COL_ARR.LAST THEN
            ISQL1 := ISQL1 || tmpStr || ', ';
         ELSE
            ISQL1 := ISQL1 || tmpStr;
         END IF;
      END LOOP;
      ISQL1 := ISQL1 || ' From ' || vTABLE_NAME || ' f where ' || vPKCol || ' = ' || vPKVal;
      DBMS_OUTPUT.PUT_LINE(ISQL1);
      Execute Immediate ISQL1;
      COL_ARR.delete;
      COL_TYPE_ARR.delete;
      --SDO_TOPO_MAP.DROP_TOPO_MAP (TopoMap);
      SDO_TOPO_MAP.CREATE_TOPO_MAP (vTOPOLOGY, TopoMap);
      SDO_TOPO_MAP.LOAD_TOPO_MAP (TopoMap, 'true');
      SQL1 := 'Update ' || vTABLE_NAME || ' f set f.' || vSDOTopoGeomCol ||
      ' = sdo_topo_map.create_feature(''' || vTOPOLOGY || ''', ''' || vTABLE_NAME || ''',''' || vSDOTopoGeomCol || ''', f.sdogeometry) ' || ' where f.' || vPKCol || ' = :1';
      dbms_output.put_line(sql1);
      Execute Immediate SQL1 using newPKVal;
      SQL1 := 'Delete from ' || vTABLE_NAME || ' Where ' || vPKCol || ' = ' || vPKVal;
      dbms_output.put_line(Sql1);
      Execute Immediate SQL1;
      SDO_TOPO_MAP.COMMIT_TOPO_MAP;
      SDO_TOPO_MAP.DROP_TOPO_MAP (TopoMap);
      SDO_TOPO.INITIALIZE_METADATA (vTOPOLOGY);
   End;
 --------------------------------------------------------------------------------
 -- *****************************************************************************
 --------------------------------------------------------------------------------
   PROCEDURE PURGE_TOPO_AND_TABLES (
      pTOPOLOGY IN VARCHAR2,
      pSCHEMA_NAME IN VARCHAR2
      ) IS

    -- This procedure added by Stephanie 1/3/2011 to help cleanup after failures
    -- in TAB 10 processing.

    -- PURPOSE:
    -- Purges the requested topology AND ANY OTHER TABLES that begin with the
    -- requested topology name in the schema
    -- ALL tables that start with the topology name, followed by an
    -- underscore, and then anything, or
    -- nothing will be purged (use carefully!)

    vsql varchar2(4000);
    vcheck varchar2(4000);
    vtablelist gz_types.stringarray;
    my_schema varchar2(4000) := UPPER(pSCHEMA_NAME);
    my_topo varchar2(4000) := UPPER(pTOPOLOGY);
    vcheck2 varchar2(4000);

    BEGIN
    ---- purge topology (there should be one and only one)
        BEGIN
            dbms_output.put_line('PURGE_TOPO_AND_TABLES: Begin purge '||
                                 'topology check...');

            vsql := 'select count(distinct topology) from user_sdo_topo_info '||
                    'where topology like '''||my_topo||'%''';
            EXECUTE IMMEDIATE vsql INTO vcheck;
            IF vcheck = 1 THEN
              gz_topo_util.purge_topology(my_schema,my_topo);
            ELSE
                IF vcheck > 1 THEN

                   vsql := 'select distinct topology from user_sdo_topo_info '||
                           'where topology like '''||my_topo||'%''';

                   dbms_output.put_line(
                   'PURGE_TOPO_AND_TABLES: This program can only work if one '||
                   'and only one topology starts with the topology name you '||
                   'entered.  More than one topology starts with the '||
                   'topology name you entered.');

                   dbms_output.put_line(
                   'PURGE_TOPO_AND_TABLES: Either purge topo and tables for the longer '||
                   'topology names first, or use te purge topology function instead '||
                   'and clean up the tables by hand.');

                   dbms_output.put_line( 'Try this SQL to see the topologies '||
                   ''''||vsql||'''');

                   raise_application_error (-20001,'PURGE_TOPO_AND_TABLES: '||
                         'Other topology names start with '||my_topo||'.');
                ELSE
                   dbms_output.put_line( 'PURGE_TOPO_AND_TABLES: '||
                   ' NOTICE! No topology called '||my_topo||' found.  Will check for
                   tables next.');
                END IF;

            END IF;
            dbms_output.put_line('PURGE_TOPO_AND_TABLES: Completed purge '||
                                 'topology steps...');

        EXCEPTION
            WHEN OTHERS THEN
                raise_application_error (-20001,'PURGE_TOPO_AND_TABLES: '||
                'problem purging topology, '||my_topo||'. SQL = '''||SQLCODE||
                ''' ERROR = '||SQLERRM||'.');
        END;

    ---- drop any associated tables
        BEGIN
            dbms_output.put_line('PURGE_TOPO_AND_TABLES: Begin purge '||
                                 'table step...');
            vsql := 'select table_name from user_tables where table_name like '''
            ||my_topo||'%''';
            EXECUTE IMMEDIATE vsql BULK COLLECT INTO vtablelist;
            vcheck := vtablelist.count;
            IF vcheck = 0 THEN
               dbms_output.put_line('PURGE_TOPO_AND_TABLES: No tables '||
               'beginning with '''||my_topo|| ''' are available to purge.');
            ELSE
               FOR i IN 1..vtablelist.count LOOP
                 vsql := 'drop table '||vtablelist(i)||
                 ' cascade constraints purge';
                 EXECUTE IMMEDIATE vsql;
               END LOOP;
            END IF;
            vsql := 'select count(*) from user_tables where table_name like '''
            ||my_topo||'%''';
            EXECUTE IMMEDIATE vsql INTO vcheck2;

            IF vcheck2 > 0 THEN
                      dbms_output.put_line('PURGE_TOPO_AND_TABLES: COULD NOT '||
                                 'COMPLETE drop table steps.  '||vcheck2||
                                 ' associated tables NOT purged.');
            ELSE
                      dbms_output.put_line('PURGE_TOPO_AND_TABLES: Completed purge '||
                                 'table steps.  '||vcheck||
                                 ' associated tables purged.');
            END IF;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                dbms_output.put_line(
                'PURGE_TOPO_AND_TABLES: No tables beginning with the topology name'||
                my_topo||' were found in user_tables in '||my_schema||'.');
            WHEN OTHERS THEN
                raise_application_error (-20001,'PURGE_TOPO_AND_TABLES: '||
                'problem purging tables. SQL = '''||vsql||
                ''' ERROR = '||SQLERRM||'.');
        END;

    END PURGE_TOPO_AND_TABLES;
 --------------------------------------------------------------------------------
 -- *****************************************************************************
 --------------------------------------------------------------------------------
   PROCEDURE DROP_TABLES_WITH_PREFIX(
      pPREFIX IN VARCHAR2,
      pSCHEMA_NAME IN VARCHAR2
   ) IS
    -- This procedure added by Stephanie 1/8/2011 to help cleanup after failures
    -- in TAB 10 processing.

    -- PURPOSE:
    -- Checks to see if there are any tables that start with the Prefix
    -- provided by the user.
    -- If there are, it checks each to see if they are registered to a topology.
    -- If they are NOT registered to a topology, this procedure drops the table.

    -- This is a bit brutal, use carefully!


   vsql varchar2(4000);
   vcount_tables number;
   vtable_list gz_types.stringarray;
   vschema varchar2(4000) := upper(pSCHEMA_NAME);
   vprefix varchar2(4000) := upper(pPREFIX);
   vIs_registered number;
   vmsg varchar2(4000) := 'DROP_TABLES_WITH_PREFIX: ';

   BEGIN

   vsql := 'SELECT table_name FROM all_tables WHERE owner = '''||vschema||
           ''' AND table_name LIKE ''' ||vprefix ||'%'' ORDER BY table_name';

     BEGIN

        EXECUTE IMMEDIATE vsql BULK COLLECT INTO vtable_list;

        vcount_tables := vtable_list.COUNT;

        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                dbms_output.put_line( vmsg||
                'No tables beginning with the prefix'||
                vprefix||' were found to be owned by '||vschema||'.');
            WHEN OTHERS THEN
                raise_application_error (-20001,vmsg||
                'problem selecting tables to drop. SQL = '''||vsql||
                ''' ERROR = '||SQLERRM||'.');
     END;

   for i in 1..vtable_list.COUNT loop

      -- check to see if it is registered with a topology

      vsql := 'select count(*) from all_sdo_topo_info '||
              'where table_name = '''||vtable_list(i)||
              ''' AND owner = '''||vschema||'''';
      BEGIN

        EXECUTE IMMEDIATE vsql INTO vIs_registered;

        EXCEPTION
            WHEN OTHERS THEN
                raise_application_error (-20001,vmsg||
                'problem checking to se if the table, '||vtable_list(i)||
                ', is registered with a topology. SQL = '''||vsql||
                ''' ERROR = '||SQLERRM||'.');
      END;

      -- if not registered drop the table

      IF vIs_registered > 0 THEN

         -- don't drop it!

         raise_application_error (-20001,vmsg||
                         'Could not drop '||vtable_list(i)||
                         ' because it is registered with a topology. '||
                         'You might want to use ''PURGE_TOPO_AND_TABLES'' '||
                         'instead.');

      ELSE

         -- drop it

         vsql := 'DROP TABLE '||vschema||'.'||vtable_list(i)||
                 ' CASCADE CONSTRAINTS PURGE';

         BEGIN

           EXECUTE IMMEDIATE vsql;

           EXCEPTION
               WHEN OTHERS THEN
                   raise_application_error (-20001,vmsg||
                   'problem dropping the table, '||vschema||'.'||
                   vtable_list(i)|| '. SQL = '''||vsql||
                   ''' ERROR = '||SQLERRM||'.');
         END;

         dbms_output.put_line(vmsg||'dropped '||vtable_list(i));

      END IF;

   END LOOP;

   END DROP_TABLES_WITH_PREFIX;

   PROCEDURE FIX_RELATION_DOLLAR_INDEXES(
      pTopology IN VARCHAR2
   ) IS

   /*

      Notes:  Relation$ table should have the following indexes
      1)  A Unique Index [TOPOLOGY]REL_PK(TG_LAYER_ID, TG_ID, TOPO_ID, TOPO_TYPE)
      2) An Index [TOPOLOGY]_REL_IDX$(TOPO_ID, TOPO_TYPE)
      3) An Index [TOPOLOGY]_REL_LID$(TG_LAYER_ID, TOPO_TYPE, TG_ID)

      Known Bugs:
      1)  If you use GZ_TOPO_UTIL.COPY_TOPOLOGY,
           SDO_TOPO.INITIALIZE_METADATA call is not creating the Unique Index
      2) When you create a Topology, it is creating the _REL_LID$ index with wrong number
          of columns

      Description:
      1) This procedure creates the Unique index if it is missing.
      2)  It creates the _REL_LID$ index if it does not exist.
           It drops and creates the _REL_LID$ if has the wrong number of fields

   */


      vTopology VARCHAR2(20) := Upper(pTopology);
      SQL1 VARCHAR2(4000);
      rowCnt NUMBER(1);
      UNIQUE_INDEX_NAME VARCHAR2(30) := vTopology || 'REL_PK';
      REL_DOLLAR_TABLE_NAME VARCHAR2(30) := vTopology || '_RELATION$';
      LID_DOLLAR_INDEX_NAME VARCHAR2(30) := vTopology || '_REL_LID$';

      TYPE nested_type IS TABLE OF VARCHAR2(30);
      lidCols nested_type;

      idxCol_ARR MDSYS.STRING_ARRAY := MDSYS.STRING_ARRAY();

      Procedure Create_LID_IDX Is

      Begin
         SQL1 := '';
         SQL1 := SQL1 || ' CREATE INDEX ' || LID_DOLLAR_INDEX_NAME || ' ON ' || REL_DOLLAR_TABLE_NAME;
         SQL1 := SQL1 || ' (TG_LAYER_ID, TOPO_TYPE, TG_ID) ';
         SQL1 := SQL1 || ' PCTFREE 10 INITRANS 20 MAXTRANS 255 ';
         SQL1 := SQL1 || ' storage (initial 1m next 5m) ';
         SQL1 := SQL1 || ' LOCAL NOLOGGING NOPARALLEL ONLINE';

         Execute Immediate SQL1;

         --SQL1 := 'Alter index ' || LID_DOLLAR_INDEX_NAME || ' NOPARALLEL';
         --Execute Immediate SQL1;

         DBMS_OUTPUT.PUT_LINE('Successfully created Index ' ||  LID_DOLLAR_INDEX_NAME || ' ON ' || REL_DOLLAR_TABLE_NAME);
      End;

   BEGIN

      DBMS_OUTPUT.PUT_LINE('UNIQUE_INDEX_NAME: ' || UNIQUE_INDEX_NAME);
      DBMS_OUTPUT.PUT_LINE('REL_DOLLAR_TABLE_NAME: ' || REL_DOLLAR_TABLE_NAME);
      DBMS_OUTPUT.PUT_LINE('LID_DOLLAR_INDEX_NAME: ' || LID_DOLLAR_INDEX_NAME);

      -- 1) Do we have a unique key on rel$ table? If not create one
      SQL1 := 'Select count(*) from user_indexes where index_name = :1';
      Execute Immediate SQL1 into rowCnt using UNIQUE_INDEX_NAME;

      If rowCnt = 0 Then
         -- Create the unique Index
          DBMS_OUTPUT.PUT_LINE('Did not find Index ' || UNIQUE_INDEX_NAME || ' on ' || REL_DOLLAR_TABLE_NAME);

          SQL1 := '';
          SQL1 := SQL1 || ' CREATE UNIQUE INDEX ' || UNIQUE_INDEX_NAME || ' ON ' || REL_DOLLAR_TABLE_NAME;
          SQL1 := SQL1 || ' (TG_LAYER_ID, TG_ID, TOPO_ID, TOPO_TYPE) ';
          SQL1 := SQL1 || ' PCTFREE 10 INITRANS 20 MAXTRANS 255 ';
          SQL1 := SQL1 || ' storage (initial 1m next 5m) ';
          SQL1 := SQL1 || ' LOCAL NOLOGGING NOPARALLEL ONLINE';

          Execute Immediate SQL1;

          -- Alter Index with noparallel
          --SQL1 := 'Alter index ' || UNIQUE_INDEX_NAME || ' NOPARALLEL';
          --Execute Immediate SQL1;

          DBMS_OUTPUT.PUT_LINE('Created Index ' || UNIQUE_INDEX_NAME || ' on ' || REL_DOLLAR_TABLE_NAME);
      End If;

      -- 2) Do we have the right lid$ index on rel$ table?

      lidCols := nested_type('TG_LAYER_ID', 'TOPO_TYPE', 'TG_ID');

      SQL1 := 'Select column_name from user_ind_columns where index_name = :1';
      Execute Immediate SQL1 bulk collect into idxCol_ARR using LID_DOLLAR_INDEX_NAME;

      DBMS_OUTPUT.PUT_LINE('No. of columns in ' || LID_DOLLAR_INDEX_NAME || ': ' || idxCol_ARR.Count);

      If idxCol_ARR.Count = 3 Then
        -- Check columns order
        FOR i IN lidCols.FIRST .. lidCols.LAST
        Loop
           If  lidCols(i) = idxCol_Arr(i) Then
               NULL;
           Else

               DBMS_OUTPUT.PUT_LINE('The composite index does not have the correct fields.... Let''s re-create the index.');
               -- Drop Index
               SQL1 := 'Drop Index ' || LID_DOLLAR_INDEX_NAME;
               Execute Immediate SQL1;

               -- Create Index
               Create_LID_IDX;
               -- Break loop
               Exit;


           End If;

        End Loop;

      ElsIf idxCol_ARR.Count = 0 Then
         DBMS_OUTPUT.PUT_LINE(LID_DOLLAR_INDEX_NAME || ' doesn''t seem to exist.  Let''s create it.');
         -- Create Index
         Create_LID_IDX;

      Else
         DBMS_OUTPUT.PUT_LINE(LID_DOLLAR_INDEX_NAME || ' has the wrong number of columns.  Let''s re-create it.');
         -- Drop Index
         SQL1 := 'Drop Index ' || LID_DOLLAR_INDEX_NAME;
         Execute Immediate SQL1;

         -- Create Index
         Create_LID_IDX;
      End If;

   EXCEPTION
      WHEN OTHERS THEN
         IF SQLCODE = -942 THEN
            DBMS_OUTPUT.PUT_LINE('Unable to locate: ' || REL_DOLLAR_TABLE_NAME);
            DBMS_OUTPUT.PUT_LINE('There is nothing to fix');

            -- RAISE;  // Do not raise an error.  A rel$ table will not exist if there are no feature layers.
         ELSE
           DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
            RAISE;
         END IF;


   END FIX_RELATION_DOLLAR_INDEXES;

 --------------------------------------------------------------------------------
 -- *****************************************************************************
 --------------------------------------------------------------------------------

END GZ_TOPO_UTIL;
/


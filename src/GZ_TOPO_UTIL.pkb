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

   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------------------------

   PROCEDURE PURGE_TOPOLOGIES (
      p_schema        IN VARCHAR2,
      p_topo_like     IN VARCHAR2
   )
   AS

      --Matt! 11/22/13
      --Purge a buncha topologies that are named like parameter 2
      --Errors on an individual purge will be saved till all topos have been attempted
      --   then spat back

      --       Ex Purge all the Z8 CL topologies I've got
      --begin
      --gz_topo_util.purge_topologies('SCHEL010','Z6%CL');
      --end;

      --       Ex scary, purge every topology known
      --begin
      --gz_topo_util.purge_topologies('SCHEL010','%');
      --end;

      psql                 VARCHAR2(4000);
      topos                MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      problems             VARCHAR2(8000) := '';


   BEGIN

      IF SYS_CONTEXT('USERENV','CURRENT_USER') <> UPPER(p_schema)
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Yeah buddy, you are running from ' || SYS_CONTEXT('USERENV','CURRENT_USER')
                                      || ' but trying to drop topos over in ' || p_schema);

      END IF;

      psql := 'SELECT DISTINCT a.topology FROM '
           || 'user_sdo_topo_info a '
           || 'WHERE topology LIKE :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO topos USING UPPER(p_topo_like);

      IF topos.COUNT = 0
      THEN

         --probably a mistake
         RAISE_APPLICATION_ERROR(-20001, 'Yeah buddy, didnt find any topos with SQL ' || psql
                                      || ' and bind variable ' || UPPER(p_topo_like));

      END IF;

      FOR i IN 1 .. topos.COUNT
      LOOP

         BEGIN

            GZ_TOPO_UTIL.PURGE_TOPOLOGY(UPPER(p_schema), topos(i));

         EXCEPTION
         WHEN OTHERS
         THEN

            problems := problems || '|Error on ' || topos(i) || ': ' || SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace;

         END;

      END LOOP;

      IF problems IS NOT NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Failed these purges: ' || problems);

      END IF;

   END PURGE_TOPOLOGIES;



   ---------------------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------------------------

   FUNCTION CREATE_TOPOLOGY (
      TGT_SCHEMA        IN VARCHAR2,
      TGT_TOPOLOGY      IN VARCHAR2,
      TGT_TOLERANCE     IN NUMBER,
      TGT_SRID          IN NUMBER,
      TGT_DIGITS_RIGHT  IN NUMBER DEFAULT 16
   ) RETURN NUMBER
   AS

      --Matt! Added digits_right_of_decimal parameterization 10/18/13

      NEW_TOPOLOGY_ID NUMBER;

   BEGIN

      SDO_TOPO.CREATE_TOPOLOGY(TGT_TOPOLOGY,
                               TGT_TOLERANCE,
                               TGT_SRID,
                               NULL,            --node_table_storage
                               NULL,            --edge_table_storage
                               NULL,            --face_table_storage
                               NULL,            --history_table_storage
                               TGT_DIGITS_RIGHT);


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
     ! Added table dropping exceptions 6/27/13.
          Tends to happen when Oracle kicks it on the same line, like the typed master key bug
   */
      SQL1 VARCHAR2(4000);
   BEGIN
      SQL1 := 'CREATE TABLE ' || TGT_SCHEMA || '.' || TGT_FTABLE || ' AS Select * From ' || SRC_SCHEMA || '.' || SRC_FTABLE || ' Where 1 = 2';
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);

      BEGIN

         EXECUTE IMMEDIATE SQL1;

      EXCEPTION
      WHEN OTHERS THEN

         IF SQLCODE = -955
         THEN

            EXECUTE IMMEDIATE 'DROP TABLE ' || TGT_SCHEMA || '.' || TGT_FTABLE;
            EXECUTE IMMEDIATE SQL1;

         ELSE

            RAISE_APPLICATION_ERROR(-20001,SQLERRM);

         END IF;

      END;

      DBMS_OUTPUT.PUT_LINE('Successfully created ' || TGT_SCHEMA || '.' || TGT_FTABLE || ' feature table');

      SQL1 := 'CREATE TABLE ' || TGT_SCHEMA || '.' || TGT_FTABLE || 'X AS Select * From ' || SRC_SCHEMA || '.' || SRC_FTABLE;
      --DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
      BEGIN

         EXECUTE IMMEDIATE SQL1;

      EXCEPTION
      WHEN OTHERS THEN

         IF SQLCODE = -955
         THEN

            EXECUTE IMMEDIATE 'DROP TABLE ' || TGT_SCHEMA || '.' || TGT_FTABLE || 'X';
            EXECUTE IMMEDIATE SQL1;

         ELSE

            RAISE_APPLICATION_ERROR(-20001,SQLERRM);

         END IF;

      END;

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

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE COPY_TOPOLOGY(
      pSRC_SCHEMA IN VARCHAR2,
      pSRC_TOPOLOGY IN VARCHAR2,
      pTGT_SCHEMA IN VARCHAR2,
      pTGT_TOPOLOGY IN VARCHAR2,
      pPURGE_TGT_TOPOLOGY IN CHAR DEFAULT 'N',
      pVERIFY_SRC_TOPOLOGY IN CHAR DEFAULT 'N',
      pGRANT_PRIVS IN CHAR DEFAULT 'Y'
   ) IS

   BEGIN

      COPY_TOPOLOGY(
         pSRC_SCHEMA,
         pSRC_TOPOLOGY,
         pTGT_SCHEMA,
         pTGT_TOPOLOGY,
         pPURGE_TGT_TOPOLOGY,
         pVERIFY_SRC_TOPOLOGY,
         pGRANT_PRIVS,
         'Y'); --pCOPY_FTABLES

   END;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE COPY_TOPOLOGY(
      pSRC_SCHEMA IN VARCHAR2,
      pSRC_TOPOLOGY IN VARCHAR2,
      pTGT_SCHEMA IN VARCHAR2,
      pTGT_TOPOLOGY IN VARCHAR2,
      pPURGE_TGT_TOPOLOGY IN CHAR,
      pVERIFY_SRC_TOPOLOGY IN CHAR,
      pGRANT_PRIVS IN CHAR DEFAULT 'Y',
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
      TGT_DIGITS_RIGHT NUMBER;
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

      --2/3/14! Changed default pVERIFY_SRC_TOPOLOGY (in wrapper) to N
      --        Added pGRANT_PRIVS option, default Y, and code below checks for REFERENCE_SCHEMAS table

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

      --Matt! Added digits_right_of_decimal 10/18/13
      Select Tolerance, SRID, digits_right_of_decimal into TGT_TOLERANCE, TGT_SRID, TGT_DIGITS_RIGHT
        From all_sdo_topo_metadata
       Where owner = SRC_SCHEMA
         And topology = SRC_TOPOLOGY
         And rownum = 1;

      --Create target topology
      NEW_TOPOLOGY_ID := CREATE_TOPOLOGY(TGT_SCHEMA,
                                         TGT_TOPOLOGY,
                                         TGT_TOLERANCE,
                                         TGT_SRID,
                                         TGT_DIGITS_RIGHT);


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

         FOR i in FTABLE_ARR.FIRST..FTABLE_ARR.LAST
         LOOP

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

         GZ_TOPO_UTIL.GATHER_TOPO_STATS(TGT_TOPOLOGY);


      Exception
          When others then
              retVal := 'Unable to gather table stats ' || SQLCODE || '-' || SQLERRM;
              dbms_output.put_line(retVal);

      End;

      Begin

         IF UPPER(pGRANT_PRIVS) = 'Y'
         AND GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS('REFERENCE_SCHEMAS')
         THEN

            GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS', TGT_TOPOLOGY || '%');
            dbms_output.put_line('Done with GZ_PRIV_GRANTER');

         END IF;

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

   --------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --------------------------------------------------------------------------------

   PROCEDURE REBUILD_INDEXES (
      pTOPOLOGY IN VARCHAR2
   ) IS

      --! 6/2/14 Limit to only tables in the topology
      --  Prior to this update it would rebuild both Z899IN% and Z899INBAK% for ex

      SQL1                    Varchar2(4000);
      SQL2                    Varchar2(4000);
      INDEX_NAME              Varchar2(100);
      TYPE RefCursorType      IS REF CURSOR;
      Cur1                    RefCursorType;

   Begin

      SQL1 := 'SELECT a.index_name FROM '
           || 'user_indexes a '
           || 'WHERE '
           || 'a.table_name LIKE :p1 AND '             --for performance, uses index
           || 'REGEXP_LIKE(a.table_name, :p2) AND '    --limits
           || 'a.index_type = :p3 AND '
           || '(a.table_name IN  '
           || '(SELECT table_name FROM user_sdo_topo_info '
           || 'WHERE topology = :p4) OR a.table_name LIKE :p5) ';

      OPEN Cur1 for SQL1 USING UPPER(pTopology) || '%',
                               '^' || UPPER(pTopology) || '_.',
                               'DOMAIN',
                               UPPER(pTopology),
                               '%$';

      LOOP

         FETCH Cur1 INTO INDEX_NAME;
         EXIT WHEN Cur1%NOTFOUND;

         SQL2 := 'Alter Index ' || Index_Name || ' ReBuild';

         Execute Immediate SQL2;

      END LOOP;

      Close Cur1;
      DBMS_OUTPUT.PUT_LINE('Successfully rebuilt all indexes');

   End;

   --------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --------------------------------------------------------------------------------

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
            GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(table_name, sdo_name, TGT_SRID, TGT_TOLERANCE, NULL, NULL, index_name);

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

  -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE REMOVE_EXACT_TOPO_DUPES (
      p_topo                  IN VARCHAR2
   )
   AS

     --Matt! 1/19/12

     psql                     VARCHAR2(4000);
     my_cursor                SYS_REFCURSOR;
     edge_ids                 GZ_TYPES.stringarray;
     edge_geoms               GZ_TYPES.geomarray;
     got_dupe                 PLS_INTEGER;
     xys                      MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();

   BEGIN

      psql := 'SELECT e.edge_id, e.geometry '
           || 'FROM '
           || p_topo || '_edge$ e ';

      OPEN my_cursor FOR psql;

      LOOP

         FETCH my_cursor BULK COLLECT INTO edge_ids, edge_geoms LIMIT 25;
         EXIT WHEN edge_ids.COUNT = 0;

            FOR i in 1 .. edge_ids.COUNT
            LOOP

               xys := edge_geoms(i).sdo_ordinates;

               --xys is in out nocopy
               --returns greater than 0 if a dupe
               got_dupe := GZ_GEOM_UTILS.REMOVE_DUPE_VERTEX(xys);

               IF got_dupe != 0
               THEN

                  --dbms_output.put_line('removing a dupe!');

                  edge_geoms(i).sdo_ordinates := xys;

                  GZ_TOPO_UTIL.GZ_CHANGE_EDGE_COORDS(p_topo,
                                                     edge_ids(i),
                                                     edge_geoms(i));

               END IF;

               xys.DELETE;

            END LOOP;

      END LOOP;


   END REMOVE_EXACT_TOPO_DUPES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE REBUILD_TOPO_DOMAIN_INDEXES (
      p_topo                  IN VARCHAR2
   )
   AS

      --Matt! 1/19/12

      psql              VARCHAR2(4000);
      baddies           GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT index_name '
           || 'FROM '
           || 'user_indexes '
           || 'WHERE '
           || 'table_name LIKE :p1 AND '
           || 'index_type = :p2 AND '
           || '(status != :p3 OR domidx_status != :p4 OR domidx_opstatus != :p5) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO baddies USING p_topo || '%',
                                                             'DOMAIN',
                                                             'VALID',
                                                             'VALID',
                                                             'VALID';

      FOR i IN 1 .. baddies.COUNT
      LOOP

         EXECUTE IMMEDIATE 'ALTER INDEX ' || baddies(i) || ' REBUILD ';

      END LOOP;


   END REBUILD_TOPO_DOMAIN_INDEXES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE REBUILD_REL_DOLLAR_INDEXES (
      p_topo                  IN VARCHAR2
   )
   AS

      --Matt! 1/18/12
      --Some bug is causing relation$ index <topo>_REL_IDX$ to be marked unuseable
      --also sometimes the same for the <topo>_REL_LID$ index
      --Rebuild them

      psql              VARCHAR2(4000);
      baddies           GZ_TYPES.stringarray;

   BEGIN


      psql := 'SELECT index_name '
           || 'FROM user_indexes '
           || 'WHERE '
           || 'table_name = :p1 AND '
           || 'status = :p2 AND '
           || 'index_type = :p3 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO baddies USING UPPER(p_topo) || '_RELATION$',
                                                             'UNUSABLE', --not unusEable
                                                             'NORMAL';

      FOR i IN 1 .. baddies.COUNT
      LOOP


         EXECUTE IMMEDIATE 'ALTER INDEX ' || baddies(i) || ' REBUILD ';

      END LOOP;

      --I have a weird suspicion that these can cascade

      IF baddies.COUNT > 0
      THEN

         EXECUTE IMMEDIATE psql BULK COLLECT INTO baddies USING UPPER(p_topo) || '_RELATION$',
                                                                'UNUSABLE', --not unusEable
                                                                'NORMAL';
         FOR j IN 1 .. baddies.COUNT
         LOOP

            EXECUTE IMMEDIATE 'ALTER INDEX ' || baddies(j) || ' REBUILD ';

         END LOOP;

      END IF;


   END REBUILD_REL_DOLLAR_INDEXES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE GZ_TOPO_TUNE_UP (
      p_topo                  IN VARCHAR2
   ) AS

      --Matt! 1/18/12

      --Consolidate all of the topo fixes and polishing into one procedure
      --Idea is that it should work for both fresh empty topologies and full final topologies
      --Whenever we come to realize something new just add it here instead of 20 places
      --Open to suggestions and feel free to add or remove to/from it

   BEGIN

      -- creates the Unique relation$ index if it is missing.
      -- creates the _REL_LID$ index if it does not exist.
      -- drops and creates the _REL_LID$ if has the wrong number of fields
      GZ_TOPO_UTIL.FIX_RELATION_DOLLAR_INDEXES(UPPER(p_topo));

      --rebuild any relation$ indexes that have become unusable
      GZ_TOPO_UTIL.REBUILD_REL_DOLLAR_INDEXES(UPPER(p_topo));

      --rebuild all domain indexes on a topo table that are not valid
      GZ_TOPO_UTIL.REBUILD_TOPO_DOMAIN_INDEXES(UPPER(p_topo));

      --stats gathered on all registered topo feature and dollar tables
      GZ_TOPO_UTIL.GATHER_TOPO_STATS(UPPER(p_topo));

      --grant privileges as specd in reference schemas on all tables named like this topology
      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',UPPER(p_topo) || '%');

      --Remove exactly matched consecutive duplicate vertices in edge$?
      --Im not 100 percent convinced that this is a good idea.  We will see
      GZ_TOPO_UTIL.REMOVE_EXACT_TOPO_DUPES(UPPER(p_topo));


      --what about isolated nodes?  Or is that getting carried away?


      --What about finding missing indexes after a copy?  I dont really know how to do that



   END GZ_TOPO_TUNE_UP;


 ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE GATHER_TOPO_STATS (
      p_topo_name       IN VARCHAR2
   )
   AS

      --Matt! 10/14/11
      --pass in a topology name, get stats gathered on all registered feature and dollar tables

      psql        VARCHAR2(4000);
      p_topo      VARCHAR2(4000) := UPPER(p_topo_name);
      kount       PLS_INTEGER;
      tabs        GZ_TYPES.stringarray;

   BEGIN

      --cant log, but better do this so we can at least see if hung up
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GATHER_TOPO_STATS: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      psql := 'SELECT a.table_name '
           || 'FROM user_sdo_topo_info a '
           || 'WHERE a.topology = :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING p_topo;

      IF tabs.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Cant find a topology named ' || p_topo);

      ELSIF tabs(1) IS NULL
      THEN

         --this may be OK if we are just carrying around primitives
         --but until I hear that it exists as a legit workflow lets call it out

         RAISE_APPLICATION_ERROR(-20001,'Topology ' || p_topo || ' exists but there are no registered feature tables ');

      END IF;

      --add the $ tables
      tabs(tabs.COUNT + 1) := p_topo || '_RELATION$';
      tabs(tabs.COUNT + 1) := p_topo || '_EDGE$';
      tabs(tabs.COUNT + 1) := p_topo || '_NODE$';
      tabs(tabs.COUNT + 1) := p_topo || '_FACE$';
      --history$? Who cares

      FOR i IN 1 .. tabs.COUNT
      LOOP

         GZ_BUSINESS_UTILS.GATHER_TABLE_STATS(tabs(i));

      END LOOP;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GATHER_TOPO_STATS: Done ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


   END GATHER_TOPO_STATS;





   ----------------------------
   --Topo Parsers and Accessors
   ----------------------------

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION GET_TG_LAYER_ID (
      p_topology                    IN VARCHAR2,
      p_table_name                  IN VARCHAR2,
      p_column_name                 IN VARCHAR2,
      p_tg_layer_type               IN VARCHAR2 --point, line, polygon
   ) RETURN NUMBER
   AS

      --Matt! 4/27/10
      --Surely there are a million versions of this fn floating about?
      --8/3/11 moved this from GZ_CLIP to GZ_UTILITIES so I can call it from elsewhere, like merge

      psql           VARCHAR2(4000);
      tg_layer_id    NUMBER;

   BEGIN

      IF UPPER(p_tg_layer_type) NOT IN ('POINT','LINE','POLYGON')
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'What the heck is a tg_layer_type of ' || p_tg_layer_type);

      END IF;

      psql := 'SELECT a.tg_layer_id FROM '
           || 'user_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.table_name = :p2 AND '
           || 'a.column_name = :p3 AND '
           || 'a.tg_layer_type = :p4 ';

      BEGIN
         EXECUTE IMMEDIATE psql INTO tg_layer_id USING UPPER(p_topology),
                                                       UPPER(p_table_name),
                                                       UPPER(p_column_name),
                                                       UPPER(p_tg_layer_type);


      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Error! No matching records found in user_sdo_topo_metadata');
         WHEN TOO_MANY_ROWS THEN
            RAISE_APPLICATION_ERROR(-20001,'Error! Found more than one record in user_sdo_topo_metadata');
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Table user_sdo_topo_metadata does not exist! How wack is that?');
            ELSE
               RAISE;
            END IF;
      END;

      RETURN tg_layer_id;

   END GET_TG_LAYER_ID;


 -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION GET_TG_LAYER_LEVEL (
      p_topology                    IN VARCHAR2,
      p_table_name                  IN VARCHAR2,
      p_column_name                 IN VARCHAR2,
      p_tg_layer_type               IN VARCHAR2 --point, line, polygon
   ) RETURN NUMBER
   AS

      --Matt! 1/14/13
      --Should combine with get_tg_layer_id into something generic

      psql              VARCHAR2(4000);
      tg_layer_level    NUMBER;

   BEGIN

      IF UPPER(p_tg_layer_type) NOT IN ('POINT','LINE','POLYGON')
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'What the heck is a tg_layer_type of ' || p_tg_layer_type);

      END IF;

      psql := 'SELECT a.tg_layer_level FROM '
           || 'user_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.table_name = :p2 AND '
           || 'a.column_name = :p3 AND '
           || 'a.tg_layer_type = :p4 ';

      BEGIN
         EXECUTE IMMEDIATE psql INTO tg_layer_level USING UPPER(p_topology),
                                                          UPPER(p_table_name),
                                                          UPPER(p_column_name),
                                                          UPPER(p_tg_layer_type);


      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Error! No matching records found in user_sdo_topo_metadata');
         WHEN TOO_MANY_ROWS THEN
            RAISE_APPLICATION_ERROR(-20001,'Error! Found more than one record in user_sdo_topo_metadata');
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Table user_sdo_topo_metadata does not exist! How wack is that?');
            ELSE
               RAISE;
            END IF;
      END;

      RETURN tg_layer_level;

   END GET_TG_LAYER_LEVEL;


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_SDO_TOPO_METADATA_CHAR (
      p_topo                     IN VARCHAR2,
      p_table                    IN VARCHAR2,
      p_what                     IN VARCHAR2,
      p_column                   IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN VARCHAR2
   AS

      --Matt! 2/14/13  @}--^--^--

      --p_topo can be schema.topo or just topo
      --   if just topo, then I will assume the topo is local
      --p_table can be schema.table or just table
      --   I will never use the schema part of that one
      --p_what is any column in user/all_sdo_topo_metadata that is defined as a varchar

      psql                 VARCHAR2(4000);
      topo                 VARCHAR2(32);
      owner                VARCHAR2(32);
      table_name           VARCHAR2(32);
      dict                 VARCHAR2(4);
      column_name          VARCHAR2(32);
      output               VARCHAR2(64);

   BEGIN

      IF p_table LIKE '%$'
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry cuz, right idea, but $ tables like ' || p_table || ' arent registered in the metadata');

      END IF;

      IF GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_topo, 'LEFT') IS NOT NULL
      THEN

         dict  := 'ALL';
         owner := GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_topo, 'LEFT');
         topo  := GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_topo, 'RIGHT');

      ELSE

         dict  := 'USER';
         owner := SYS_CONTEXT('USERENV','CURRENT_USER');
         topo  := UPPER(p_topo);

      END IF;

      IF GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_table, 'RIGHT') IS NOT NULL
      THEN

         table_name := GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_table, 'RIGHT');

      ELSE

         table_name := p_table;

      END IF;

      column_name := UPPER(p_column);

      psql := 'SELECT a.' || p_what || ' '
           || 'FROM ' || dict || '_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.owner = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.table_name = :p3 AND '
           || 'a.column_name = :p4 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING owner,
                                                  topo,
                                                  table_name,
                                                  column_name;

      EXCEPTION
      WHEN OTHERS
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'ERROR> ' || SQLERRM || ' < on ' || psql || ' with binds- '
                                 || owner || ' ' || topo || ' ' || table_name || ' ' || column_name);

      END;

      RETURN output;


   END GET_SDO_TOPO_METADATA_CHAR;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_SDO_TOPO_METADATA_NUM (
      p_topo                     IN VARCHAR2,
      p_table                    IN VARCHAR2,
      p_what                     IN VARCHAR2,
      p_column                   IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN NUMBER
   AS

      --Matt! 2/14/13  @}--^--^--

      --p_topo can be schema.topo or just topo
      --   if just topo, then I will assume the topo is local
      --p_table can be schema.table or just table
      --   I will never use the schema part of that one
      --p_what is any column in user/all_sdo_topo_metadata that is defined as a varchar

      psql                 VARCHAR2(4000);
      topo                 VARCHAR2(32);
      owner                VARCHAR2(32);
      table_name           VARCHAR2(32);
      dict                 VARCHAR2(4);
      column_name          VARCHAR2(32);
      output               NUMBER;

   BEGIN

      IF p_table LIKE '%$'
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry cuz, right idea, but $ tables like ' || p_table || ' arent registered in the metadata');

      END IF;

      IF GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_topo, 'LEFT') IS NOT NULL
      THEN

         dict  := 'ALL';
         owner := GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_topo, 'LEFT');
         topo  := GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_topo, 'RIGHT');

      ELSE

         dict  := 'USER';
         owner := SYS_CONTEXT('USERENV','CURRENT_USER');
         topo  := UPPER(p_topo);

      END IF;

      IF GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_table, 'RIGHT') IS NOT NULL
      THEN

         table_name := GZ_TOPO_UTIL.GET_X_OF_THA_DOT(p_table, 'RIGHT');

      ELSE

         table_name := p_table;

      END IF;

      column_name := UPPER(p_column);

      psql := 'SELECT a.' || p_what || ' '
           || 'FROM ' || dict || '_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.owner = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.table_name = :p3 AND '
           || 'a.column_name = :p4 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING owner,
                                                  topo,
                                                  table_name,
                                                  column_name;

      EXCEPTION
      WHEN OTHERS
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'ERROR> ' || SQLERRM || ' < on ' || psql || ' with binds- '
                                 || owner || ' ' || topo || ' ' || table_name || ' ' || column_name);

      END;

      RETURN output;

   END GET_SDO_TOPO_METADATA_NUM;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_X_OF_THA_DOT (
      p_input                    IN VARCHAR2,
      p_x                        IN VARCHAR2 DEFAULT 'LEFT'
   ) RETURN VARCHAR2 DETERMINISTIC
   AS

      --Matt! 2/12/2013

   BEGIN

      IF INSTR(p_input,'.') = 0
      THEN

         --executive decision.  Theres nothing to the side of nothing
         RETURN NULL;

      END IF;

      IF UPPER(p_x) = 'LEFT'
      THEN

         RETURN UPPER(SUBSTR(p_input, 0, (INSTR(p_input,'.') - 1) ));

      ELSIF UPPER(p_x) = 'RIGHT'
      THEN

          RETURN UPPER(SUBSTR(p_input, (INSTR(p_input,'.') + 1) ));

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'What the X is a ' || p_x || '?');

      END IF;


   END GET_X_OF_THA_DOT;



    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION COUNT_OBSOLETE_NODES (
      p_topology       IN VARCHAR2
   ) RETURN NUMBER AS

   /*

   Matt! 9/12/12 Pulled the logic here into GET_OBSOLETE_NODES (see below)
                 Changed COUNT_OBSOLETE_NODES to call GET_OBSOLETE_NODES and flip just the count

   Purpose:  Return the number of obsolete nodes in a topology based on NODE$ and EDGE$
   only.

   Author: Stephanie

   Date: 20100426

   Arguments:
   p_topology - the name of a topology in the current schema, or a schema
       name followed by a topology name whose primitive tables you have select
       access to.
       examples...
          MYTOPO
          MT
          ACS09GM.MT

   Explanation:
       Loops through each node in NODE$ and checks to see how many
       different lines are connected to it.  Single connections are okay in the
       case of islands, nodes with three connections are required for the
       topology, nodes with two and only two connections are counted.

       This function does not take feature layers into account, only
       node$ and edge$.  An obsolete node may be required to support a
       feature layer (say a road name change for example).  This function was
       written to support the generalized boundary files where all two-connected
       nodes are obsolete.

   */


    v_nodearray      GZ_TYPES.NUMBERARRAY;

   BEGIN

      v_nodearray := GZ_TOPO_UTIL.GET_OBSOLETE_NODES(p_topology);

      dbms_output.put_line('*******************************************');
      dbms_output.put_line('I found '||v_nodearray.COUNT|| ' obsolete nodes!');
      dbms_output.put_line('*******************************************');

     RETURN v_nodearray.COUNT;

   END COUNT_OBSOLETE_NODES;

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GET_OBSOLETE_NODES(
      p_topology       IN VARCHAR2
   ) RETURN GZ_TYPES.NUMBERARRAY
   AS

   --Matt! 9/12/12
   --Stole and modified the guts of Stephanie's count_obsolete_nodes (above) and put it in here
   --Made original count_obsolete_nodes call this guy so no code duplication
   --Returns the node ids of obsolete nodes in a topology

   --Loops through each node in NODE$ and checks to see how many
   --different lines are connected to it.  Single connections are okay in the
   --case of islands, nodes with three connections are required for the
   --topology, nodes with two and only two connections are counted.

   --This function does not take feature layers into account, only
   --node$ and edge$.  An obsolete node may be required to support a
   --feature layer (say a road name change for example).  This function was
   --written to support the generalized boundary files where all two-connected
   --nodes are obsolete.


   vsql           VARCHAR2(4000);
   v_total        NUMBER;
   v_islands      NUMBER;
   v_obs_count    NUMBER := 0;
   v_nodearray    GZ_TYPES.numberarray;
   output         GZ_TYPES.numberarray;

   BEGIN

      vsql := 'SELECT node_id from '||p_topology||'_node$ order by node_id';
      EXECUTE IMMEDIATE vsql BULK COLLECT INTO v_nodearray;

      FOR i IN 1..v_nodearray.COUNT
      LOOP

          vsql := 'select count(*) from '||p_topology||'_edge$ a '||
                  'where '|| v_nodearray(i)
                ||'in (a.start_node_id,a.end_node_id)';

          EXECUTE IMMEDIATE vsql INTO v_total;

          --dbms_output.put_line('NODE = '||v_nodearray(i));
          --dbms_output.put_line('  Total edges = '||v_total);

          vsql := 'select count(*) from '||p_topology||'_edge$ a '||
                  'where a.start_node_id  = :p1 '||
                  'AND a.end_node_id = :p2';

          EXECUTE IMMEDIATE vsql INTO v_islands USING v_nodearray(i),
                                                      v_nodearray(i);

          --dbms_output.put_line('  Total islands = '||v_islands);

          --IF v_total = 1 AND v_islands = 1 THEN
             --dbms_output.put_line('  This node is on an isolated island = OK');
          --END IF;

         IF v_total = 2
         AND v_islands = 0
         THEN

            --dbms_output.put_line('  Node '||v_nodearray(i)||' is OBSOLETE!');

            v_obs_count := v_obs_count + 1;

            output(v_obs_count) := v_nodearray(i);

         END IF;

      END LOOP;


     RETURN output;

   END GET_OBSOLETE_NODES;


    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION DROP_UPDATABLE_TOPOMAPS(
     p_current_topomap    IN VARCHAR2,
     p_which              IN VARCHAR2 DEFAULT 'UPDATABLE'
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt!  5/25/10
      --Called from GZ_UTILITIES.build_topo_from_spatial as well as from
      --    several spots in GZ_CLIP
      --Drops all topo maps associated with the current session or just updatable ones
      --   so we can get on with our freakin lives
      --Returns the names of the dropped topo maps in case it matters (currently does not to me)
      --added p_which and p_current_topomap parameters 5/28/10
      --fixed logic for ALL parameter 6/18/10

      --N.B.!!!
      --Though the error that makes one want to call this function occurs on load topomap
      --One must take a step back and recreate the topomap too after calling this guy
      --SDO_TOPO_MAP.CREATE_TOPO_MAP(toponame, newtopomap);
      --SDO_TOPO_MAP.LOAD_TOPO_MAP(newtopomap, 'TRUE','TRUE');

      --SAMPLE USAGE
      --Im working on a topomap called STATEFP25_TOPOMAP and just found out that
      --Some bomb left another (or several) topomap(s) out in the ether

      --DECLARE
      --topomaps GZ_TYPES.stringarray;
      --BEGIN
      --topomaps := GZ_UTILITIES.DROP_UPDATABLE_TOPOMAPS('STATEFP25_TOPOMAP','ALL');
      --END;

      topomaps          VARCHAR2(4000);
      topostring        VARCHAR2(4000);
      current_trifecta  GZ_TYPES.stringarray;
      output            GZ_TYPES.stringarray;
      kount             PLS_INTEGER := 1;
      kount2            PLS_INTEGER := 1;


   BEGIN

      topomaps := SDO_TOPO_MAP.LIST_TOPO_MAPS();

      --If nothing, get out
      --probably an error, but seems like a diff scope
      IF LENGTH(topomaps) = 0
      THEN
         RETURN output;
      END IF;

      --Annoyingly, looks like this
      --(STATEFP25_TOPOMAP, STATEFP25, read-only), (STATEFP24_TOPOMAP, STATEFP24, updatable)

      --Parens must be escaped in regexps
      --Find me the first occurrence of a ( followed by non () text, then a )
      topostring := regexp_substr(topomaps,'\([^\)^\(]+\)',1,1);

      WHILE topostring IS NOT NULL
      LOOP

         --now like: (STATEFP25_TOPOMAP, STATEFP25, read-only)
         --replace parens
         topostring := regexp_replace(topostring,'\(','');
         topostring := regexp_replace(topostring,'\)','');

         --now like STATEFP25_TOPOMAP, STATEFP25, read-only
         current_trifecta := GZ_BUSINESS_UTILS.split(topostring,',');


         IF p_which = 'UPDATABLE'
         AND UPPER(current_trifecta(3)) LIKE '%UPDATABLE%'
         AND p_current_topomap != current_trifecta(1)
         THEN

               output(kount2) := current_trifecta(1);
               SDO_TOPO_MAP.DROP_TOPO_MAP(current_trifecta(1));

               kount2 := kount2 + 1;

         ELSIF p_which = 'ALL'
         THEN

               output(kount2) := current_trifecta(1);
               SDO_TOPO_MAP.DROP_TOPO_MAP(current_trifecta(1));

               kount2 := kount2 + 1;

         END IF;

         kount := kount + 1;
         topostring := regexp_substr(topomaps,'\([^\)^\(]+\)',1,kount);

         IF kount > 1000
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Yo, we''ve just attempted to drop 1000 topo maps, thats topowack. Here they are: ' || topomaps);

         END IF;

      END LOOP;


      IF output.COUNT > 0
      THEN

         RETURN output;

      ELSE

         --This error assumes we did not call this guy without reason
         --We think there is an updatable topomap somewhere out there
         RAISE_APPLICATION_ERROR(-20001,'Inexplicably, failed to drop any of these topo maps: ' || topomaps);

      END IF;


   END DROP_UPDATABLE_TOPOMAPS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION EZ_TOPOMAP_MANAGER (
      p_topomap_name       IN VARCHAR2,
      p_topology_name      IN VARCHAR2,
      p_create_it          IN NUMBER,
      p_xLL                IN NUMBER DEFAULT NULL,
      p_yLL                IN NUMBER DEFAULT NULL,
      p_xUR                IN NUMBER DEFAULT NULL,
      p_yUR                IN NUMBER DEFAULT NULL,
      p_delta              IN NUMBER DEFAULT 0.0001,
      p_memory_size        IN NUMBER DEFAULT NULL,
      p_cost_adj           IN NUMBER DEFAULT 100
   ) RETURN NUMBER
   AS

      --Matt! 6/23/10
      --Matt! 3/17/11 Added memory management
      --Matt! 9/8/14 Added cost adjustment option

      --p_create_it
      -- 1: Create (only) topomap
      -- 2: Create and load, if any topomaps already exist nuke them
      -- 3: Commit and drop

      --Sample usage
      -- create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(newtopo,topomap,2);
      --Then do some work, occasionally call SDO_TOPO_MAP.UPDATE_TOPO_MAP etc
      -- create_it := GZ_UTILITIES.EZ_TOPOMAP_MANAGER(newtopo,topomap,create_it);

      topomap_name      VARCHAR2(4000);
      topology_name     VARCHAR2(4000);
      topomaps          GZ_TYPES.stringarray;
      pXMin             NUMBER;
      pYMin             NUMBER;
      pXMax             NUMBER;
      pYMax             NUMBER;
      create_it         NUMBER;
      raise_the_roof    NUMBER;


   BEGIN

      IF p_memory_size IS NULL
      THEN
         raise_the_roof := 2147483648;
      ELSE
         raise_the_roof := p_memory_size;
      END IF;
      
      IF p_cost_adj <> 100
      THEN
      
         EXECUTE IMMEDIATE 'ALTER SESSION SET OPTIMIZER_INDEX_COST_ADJ = ' || TO_CHAR(p_cost_adj);
      
      END IF;


      --Just in case
      topomap_name := UPPER(p_topomap_name);
      topology_name := UPPER(p_topology_name);

      IF p_create_it NOT IN (1,2,3)
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Sorry pal, p_create_it must be 1 2 or 3 and you chucked in ' || p_create_it);
      END IF;

      IF p_xLL IS NOT NULL
      AND p_yLL IS NOT NULL
      AND p_xUR IS NOT NULL
      AND p_yUR IS NOT NULL
      THEN

         --Set up window
         pXmin := p_xLL - p_delta;
         pYmin := p_yLL - p_delta;
         pXMax := p_xUR + p_delta;
         pYmax := p_yUR + p_delta;

      ELSIF p_xLL IS NOT NULL
      AND (p_yLL IS NULL OR
           p_xUR IS NULL OR
           p_yUR IS NULL)
      THEN

         RAISE_APPLICATION_ERROR(-20001,'We have some but not all of the bounding box coordinates');

      ELSE

         --Full topology topomap load
         NULL;

      END IF;


      IF p_create_it IN (1,2)
      THEN

         BEGIN

            SDO_TOPO_MAP.CREATE_TOPO_MAP(topology_name,topomap_name);
            create_it := 2;

            IF p_create_it = 2
            THEN

               IF p_xLL IS NULL
               THEN

                  SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, 'TRUE','TRUE');
                  create_it := 3;

               ELSE

                  SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, pXmin, pYmin, pXMax, pYmax, 'TRUE');
                  create_it := 3;

               END IF;

            END IF;

         EXCEPTION
         WHEN OTHERS THEN

            IF UPPER(SQLERRM) LIKE UPPER('%an updatable TopoMap object already exists%') OR
            UPPER(SQLERRM) LIKE UPPER('%a TopoMap by the same name already exists%')
            THEN

               --ex:
               --ORA-29532: Java call terminated by uncaught Java exception:
               --oracle.spatial.topo.TopoDataException:
               --an updatable TopoMap object already exists: STATEFP24_TOPOMAP

               topomaps := GZ_TOPO_UTIL.DROP_UPDATABLE_TOPOMAPS(topomap_name,'ALL');

               SDO_TOPO_MAP.CREATE_TOPO_MAP(topology_name,topomap_name);
               create_it := 2;

               IF p_create_it = 2
               THEN

                  IF p_Xll IS NULL
                  THEN
                     SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, 'TRUE','TRUE');
                  ELSE
                     SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, pXmin, pYmin, pXMax, pYmax, 'TRUE');
                  END IF;

                  create_it := 3;

               END IF;

            ELSIF UPPER(SQLERRM) LIKE '%OUTOFMEMORYERROR%'
            THEN

               --Sidey voodoo first
               dbms_session.free_unused_user_memory;

               SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(raise_the_roof);

               IF p_create_it = 2
               THEN

                  IF p_xLL IS NULL
                  THEN

                     SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, 'TRUE','TRUE');
                     create_it := 3;

                  ELSE

                     SDO_TOPO_MAP.LOAD_TOPO_MAP(topomap_name, pXmin, pYmin, pXMax, pYmax, 'TRUE');
                     create_it := 3;

                  END IF;

               END IF;

            ELSE

               ---------------------------------------------------------
               --More errors as we learn and wish to handle them go here
               ---------------------------------------------------------

               RAISE;

            END IF;

         END;


         IF p_cost_adj <> 100
         THEN
         
            --set back if we modified from default
            EXECUTE IMMEDIATE 'ALTER SESSION SET OPTIMIZER_INDEX_COST_ADJ = ' || TO_CHAR(p_cost_adj);
         
         END IF;
         
         RETURN create_it;

      END IF;


      IF p_create_it = 3
      THEN

         BEGIN

            SDO_TOPO_MAP.COMMIT_TOPO_MAP();

         EXCEPTION
         WHEN OTHERS THEN

            --ORA-29532: Java call terminated by uncaught Java exception:
            --oracle.spatial.topo.TopoDataException: no updatable TopoMap object has been created
            IF SQLCODE = -29532
            AND UPPER(SQLERRM) LIKE UPPER('%no updatable TopoMap object has been created%')
            THEN

               create_it := 1;
               RETURN create_it;

            ELSE

               ---------------------------------------------------------
               --More errors as we learn and wish to handle them go here
               ---------------------------------------------------------
               --here
               RAISE;

            END IF;

         END;


         BEGIN

            SDO_TOPO_MAP.DROP_TOPO_MAP(p_topomap_name);

         EXCEPTION
         WHEN OTHERS THEN

            --ORA-29532: Java call terminated by uncaught Java exception:
            --oracle.spatial.topo.TopoDataException: the specified TopoMap does not exist
            IF SQLCODE = -29532
            AND UPPER(SQLERRM) LIKE UPPER('%the specified TopoMap does not exist%')
            THEN

               create_it := 1;
               RETURN create_it;

            ELSE

               ---------------------------------------------------------
               --More errors as we learn and wish to handle them go here
               ---------------------------------------------------------
               RAISE;

            END IF;

         END;

         create_it := 1;
         RETURN create_it;

      END IF;


   END EZ_TOPOMAP_MANAGER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

PROCEDURE remove_obs_nodes_one_state (
topo_name VARCHAR2,
state_table VARCHAR2,
v_state VARCHAR2
)
AS
   --Matt! 9/12/12 Making SP not call this any more
   --      Not sure if anything calls it

---
-- Does not remove nodes where they are the start and end of a single edge.
--
v_topo_name VARCHAR2(4000) := UPPER(topo_name);
v_topo_map VARCHAR2(4000) := v_topo_name ||'_TOPOMAP';
v_ez_topo_mgr NUMBER := 0;

BEGIN
--------------------------------------------------------------------------------
-- 3/27/2012 - We will need to change the parameters and call in GZ_SMPOLY
-- all we really need now is the topology name.
-- At some point we should add an optional window.
-- We should change the whole procedure to call it - 'remove obsolete nodes from
-- whole topology' or something similarly descriptive

-- No logging is needed, the caller will check obs node counts and log as
-- needed.  If there are any obsolete nodes left, then that means the
-- procedure did not work.

    -- call ez_topomap_manager to open a new topomap
    -- for the whole topology, (CODE 2 to EZ_TOPOMAP_MANAGER).
    --(defaults to a buffer of 0.0001)

    v_ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(v_topo_map,v_topo_name,2);

    -- edit the topo map - remove obsolete nodes (pseudo nodes)...

    SDO_TOPO_MAP.REMOVE_OBSOLETE_NODES(NULL);

    -- commit the topomap and drop it  (CODE 3 to EZ_TOPOMAP_MANAGER).

    v_ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(v_topo_map,v_topo_name,3);

END remove_obs_nodes_one_state ;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION GZ_REMOVE_OBSOLETE_NODES (
      p_topo          IN VARCHAR2
   ) RETURN NUMBER
   AS

      --Matt! 9/12/12
      --Update! 10/26/12 to deal with face not found in cache error
      --SP module having java memory blowouts calling remove_obs_nodes_one_state above
      --Gonna try calling with node by node topomap, see how the performance goes
      --I have only done cursory performance checks.  In general, would expect this guy to be
      --   slower for big topos with lots of obsolete nodes.  For topos with few obsolete nodes
      --   this approach may be faster since lots of untouched territory doesn't have to enter cache

      --Should return 0, for 0 obsolete nodes
      --If the number is greater than 0 the caller should decide what to do

      nodez          GZ_TYPES.numberarray;
      create_it      NUMBER;
      node_geomz     GZ_TYPES.geomarray;
      psql           VARCHAR2(4000);
      my_cursor      SYS_REFCURSOR;
      output         NUMBER;

   BEGIN

      nodez := GZ_TOPO_UTIL.GET_OBSOLETE_NODES(p_topo);

      psql := 'SELECT n.geometry '
           || 'FROM '
           || p_topo || '_node$ n '
           || 'WHERE '
           || 'n.node_id IN (SELECT * FROM TABLE(:p1)) ';

      OPEN my_cursor FOR psql USING GZ_BUSINESS_UTILS.numarray_to_varray(nodez);

      LOOP

         FETCH my_cursor BULK COLLECT INTO node_geomz LIMIT 1000;
         EXIT WHEN node_geomz.COUNT = 0;

         FOR i IN 1 .. node_geomz.COUNT
         LOOP

            --dbms_output.put_line('NODE ' || gz_utilities.dump_sdo(node_geomz(i)));

            create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || '_MAP',
                                                         p_topo,
                                                         2,
                                                         node_geomz(i).sdo_point.X,
                                                         node_geomz(i).sdo_point.Y,
                                                         node_geomz(i).sdo_point.X, --allow the delta to form the window
                                                         node_geomz(i).sdo_point.Y);

            --may need an exception handler here
            --Yep. Put some voodoo on it

            BEGIN

               SDO_TOPO_MAP.REMOVE_OBSOLETE_NODES(NULL);

            EXCEPTION
            WHEN OTHERS
            THEN

               IF SQLERRM LIKE '%not found in cache%'
               THEN

                  --Java call terminated by uncaught Java exception: oracle.spatial.topo.TopoEntityNotFoundException:
                  --Face ID 67 not found in cache
                  --The usual topomap error.  Think this is a bug but haven't done SR (yet 10/26/12)
                  --Stretch the topomap a little and we have the funkiest horn section in the metropolis again

                  create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || '_MAP',
                                                               p_topo,
                                                               2,
                                                               node_geomz(i).sdo_point.X,
                                                               node_geomz(i).sdo_point.Y,
                                                               node_geomz(i).sdo_point.X, --allow the delta to form the window
                                                               node_geomz(i).sdo_point.Y,
                                                               .0004);  --quadruple delta!

                  SDO_TOPO_MAP.REMOVE_OBSOLETE_NODES(NULL);


               ELSE

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

               END IF;

            END;


            create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || '_MAP',
                                                         p_topo,
                                                         3);

         END LOOP;

      END LOOP;

      output := GZ_TOPO_UTIL.COUNT_OBSOLETE_NODES(p_topo);

      RETURN output;

   END GZ_REMOVE_OBSOLETE_NODES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

PROCEDURE remove_isolated_nodes
(p_topology VARCHAR2)
AS

TYPE mynumbers IS TABLE of NUMBER
   INDEX BY PLS_INTEGER;

v_node_list mynumbers;

v_sql VARCHAR2(4000);

BEGIN

v_sql := 'SELECT node_id FROM '||p_topology||'_node$ where edge_id = 0 or edge_id IS NULL';

EXECUTE IMMEDIATE v_sql BULK COLLECT INTO v_node_list;

FOR i IN 1..v_node_list.count LOOP

    SDO_TOPO_MAP.REMOVE_NODE(p_topology,v_node_list(i));

    commit;

END LOOP;

END remove_isolated_nodes;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE REMOVE_ISOLATED_NODES_CACHE (
      p_topo               IN VARCHAR2
   )
   AS

      --M@!  8/6/14
      --Remove_Isolated_Nodes (just above here) threw a weird Oracle topomap already exists
      --error.  I dont trust the Oracle joker's management of the topomap cache, so this version
      --is explicit.  This one will be called from clip, the one above is still called from SP I think

      nodez                GZ_TYPES.numberarray;
      node_geomz           GZ_TYPES.geomarray;
      my_cursor            SYS_REFCURSOR;
      psql                 VARCHAR2(4000);
      ez_topo_mgr          NUMBER;

   BEGIN

      psql := 'SELECT n.node_id, n.geometry '
           || 'FROM ' || p_topo || '_node$ n '
           || 'WHERE '
           || 'n.edge_id = :p1 OR n.edge_id IS NULL';

      OPEN my_cursor FOR psql USING 0;

      LOOP

         FETCH my_cursor BULK COLLECT INTO nodez, node_geomz LIMIT 500;
         EXIT WHEN nodez.COUNT = 0;

         FOR i IN 1 .. nodez.COUNT
         LOOP

            --+/- delta occurs in the manager
            ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                           p_topo,
                                                           2,
                                                           node_geomz(i).sdo_point.X,
                                                           node_geomz(i).sdo_point.Y,
                                                           node_geomz(i).sdo_point.X,
                                                           node_geomz(i).sdo_point.Y
                                                           );
            SDO_TOPO_MAP.REMOVE_NODE(NULL,
                                     nodez(i));


            ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                           p_topo,
                                                           3);

         END LOOP;

      END LOOP;

      CLOSE my_cursor;

   END REMOVE_ISOLATED_NODES_CACHE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE DEREGISTER_FEATURE_TABLES (
      p_schema             IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_drop_tabs          IN VARCHAR2 DEFAULT 'Y',
      p_min_tg_layer_level IN NUMBER DEFAULT 0,
      p_likeclause         IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 10/28/10
      --Deregisters all (or some) feature tables registered with a topology
      --Deletes the deregistered tables if requested
      --Leaves topology and primitives intact

      --Sample, deregister and drop all
      -- GZ_UTILITIES.DEREGISTER_FEATURE_TABLES('GZCPB1','DEC09');

      --Deregister and drop all greater than 0-level feature tabs
      -- GZ_UTILITIES.DEREGISTER_FEATURE_TABLES('GZCPB1','DEC09','Y',1);

      --Deregister FSL tables, but keep them
      -- GZ_UTILITIES.DEREGISTER_FEATURE_TABLES('GZCPB1','DEC09','N',0,'FSL%');



      tabs                 GZ_TYPES.stringarray;
      cols                 GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);
      tabstash             GZ_TYPES.stringhash;
      topo                 VARCHAR2(32) := UPPER(p_topo);
      pschema              VARCHAR2(32) := UPPER(p_schema);

   BEGIN

      psql := 'SELECT a.table_name, a.column_name '
           || 'FROM '
           || 'user_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.owner = :p2 AND '
           || 'a.tg_layer_level >= :p3 AND '
           || 'a.table_name IS NOT NULL ';

      IF p_likeclause IS NULL
      THEN

         psql := psql || 'ORDER BY a.tg_layer_id DESC ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs, cols USING topo,
                                                                   pschema,
                                                                   p_min_tg_layer_level;

      ELSIF p_likeclause IS NOT NULL
      THEN

         psql := psql || 'AND a.table_name LIKE :p4 '
                      || 'ORDER BY a.tg_layer_id DESC ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs, cols USING topo,
                                                                   pschema,
                                                                   p_min_tg_layer_level,
                                                                   p_likeclause;

      END IF;

      IF tabs.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Couldnt find any tables to deregister for topo ' || topo);

      END IF;


      FOR i in 1 .. tabs.COUNT
      LOOP

         BEGIN

            SDO_TOPO.DELETE_TOPO_GEOMETRY_LAYER(topo,tabs(i),cols(i));

         EXCEPTION
         WHEN OTHERS THEN

            --Add handled errors here

            RAISE;

         END;

      END LOOP;

      IF UPPER(p_drop_tabs) = 'Y'
      THEN

         FOR i in 1 .. tabs.COUNT
         LOOP

            IF NOT tabstash.EXISTS(tabs(i))
            THEN

               tabstash(tabs(i)) := '1';
               psql := 'DROP TABLE ' || tabs(i) || ' PURGE ';

               BEGIN

                  EXECUTE IMMEDIATE psql;

               EXCEPTION
               WHEN OTHERS THEN

                  --Add handled errors here
                  RAISE;

               END;

            END IF;

         END LOOP;

      END IF;

   END DEREGISTER_FEATURE_TABLES;

    ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   PROCEDURE GZ_CHANGE_EDGE_COORDS (
      p_topo          IN VARCHAR2,
      p_edge_id       IN NUMBER,
      p_edge_geom     IN SDO_GEOMETRY,
      p_clean_edge    IN NUMBER DEFAULT NULL,
      p_delta         IN NUMBER DEFAULT 0,
      p_debug         IN NUMBER DEFAULT NULL
   )
   AS

      --Matt! 2/07/11
      --This is meant to be a general purpose wrapper for SDO_TOPO_MAP.CHANGE_EDGE_COORDS
      --Like Oracle topo functions, pass in NULL for the topology if an appropriate topomap is already open
      --   If a topology comes in as the first param, we will build a window topomap in here

      --Sample usage
      --
      --begin
      --gz_topo_util.gz_change_edge_coords('Z899INT',
      --                                    6831,
      --                                    SDO_GEOMETRY(
      --                                                2002, 8265, NULL,
      --                                                SDO_ELEM_INFO_ARRAY ( 1,2,1 ),
      --                                                SDO_ORDINATE_ARRAY
      --                                                (-72.99027999999999849478626856580376625061, 41.23473700000000263798938249237835407257,
      --                                                 -72.98675699999999721967469668015837669373, 41.23544700000000062800609157420694828033,
      --                                                 -72.98672100000000284580892184749245643616, 41.23542499999999932924765744246542453766))
      --                                   );
      --end;

      --11/08/11 Changed p_clean_edge to NUMBER default .05.  Passed along as the tolerance
      --12/14/11 Attempt to manage "edge_id X not found in cache" oracle bug
      --! 2/3/14 Added p_delta option. Allows user to add more than the default .0001 buffer
      --         used in the topomap manager


      psql                 VARCHAR2(4000);
      create_it            NUMBER;
      window_geom          SDO_GEOMETRY;
      getoutofjail         PLS_INTEGER := 0;
      moved_iso_nodes      SDO_NUMBER_ARRAY;
      moved_iso_edges      SDO_NUMBER_ARRAY;
      allow_iso_moves      VARCHAR2(5) := 'FALSE';
      clean_geom           SDO_GEOMETRY;
      topotest             VARCHAR2(4000);

   BEGIN

     <<jailbreak>>

      IF p_topo IS NOT NULL
      THEN

         --Make a window for the topomap
         --got a topology name passed in

         IF getoutofjail = 0
         THEN

            --SOP
            psql := 'SELECT SDO_GEOM.SDO_MBR(e.geometry) '
                 || 'FROM '
                 || p_topo || '_edge$ e '
                 || 'WHERE '
                 || 'e.edge_id = :p1 ';

            EXECUTE IMMEDIATE psql INTO window_geom USING p_edge_id;

         ELSE

            --maybe need to load the entire topology to work around Oracle shenanigans

            psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
                 || 'FROM '
                 || p_topo || '_edge$ e ';

            EXECUTE IMMEDIATE psql INTO window_geom;

         END IF;


         IF window_geom.sdo_ordinates.COUNT != 4
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Bad window. Bad. ');

         ELSIF p_delta <> 0
         THEN

            --user is working around
            window_geom.sdo_ordinates(1) := window_geom.sdo_ordinates(1) - p_delta;
            window_geom.sdo_ordinates(2) := window_geom.sdo_ordinates(2) - p_delta;
            window_geom.sdo_ordinates(3) := window_geom.sdo_ordinates(3) + p_delta;
            window_geom.sdo_ordinates(4) := window_geom.sdo_ordinates(4) + p_delta;

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

      END IF;



      BEGIN

         --always a topomap in the session at this point

         --This is Sidey's current best understanding of
         --the signature to change_edge_coords that calls up the fewest bugs

         SDO_TOPO_MAP.CHANGE_EDGE_COORDS(NULL,
                                         p_edge_id,
                                         p_edge_geom,
                                         moved_iso_nodes,
                                         moved_iso_edges,
                                         allow_iso_moves);

      EXCEPTION
      WHEN OTHERS
      THEN

         IF UPPER(SQLERRM) LIKE '%CHANGED EDGE COORDINATE STRING SELF-INTERSECTS%'
         THEN

            --This is bogus, current workaround is manual in ADE
            --Should automate that workaround right here

            --however, just in case the error isnt bogus
            --feel free to try this. Have to ask for it with a value like .05
            IF p_clean_edge IS NOT NULL
            THEN

               clean_geom := GZ_GEOM_UTILS.REMOVE_CLOSE_ORDINATES(p_edge_geom,
                                                                 p_clean_edge);

               SDO_TOPO_MAP.CHANGE_EDGE_COORDS(NULL,
                                               p_edge_id,
                                               clean_geom,
                                               moved_iso_nodes,
                                               moved_iso_edges,
                                               allow_iso_moves);

            ELSE

               IF p_topo IS NOT NULL
               THEN

                  --commit and drop temp topomap
                  create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

               END IF;

               --tell the caller to work around this bug error
               RAISE_APPLICATION_ERROR(-20001,'ADD FAKE FACE');

            END IF;


         ELSIF UPPER(SQLERRM) LIKE '%NOT FOUND IN CACHE%'
         THEN

            --this is a bug
            --Adding the edge returned to the window just
            --   chases the error to a new edge ad infinitum
            --Automated workaround here: Make a topomap that includes the whole topology

            IF getoutofjail = 0
            THEN

               getoutofjail := 1;
               GOTO jailbreak;

            ELSE

               --out of ideas

                --GZ_UTILITIES.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
                RAISE_APPLICATION_ERROR(-20001, 'Got the not found in cache error 2x! '|| SQLERRM);

            END IF;


         ELSE     --what else gets raised here?

            IF p_topo IS NOT NULL
            THEN

               --commit and drop temp topomap
               create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

            END IF;

            RAISE;

         END IF;

      END;



      IF p_topo IS NOT NULL
      THEN

         --commit and drop temp topomap
         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

      END IF;



   END GZ_CHANGE_EDGE_COORDS;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   PROCEDURE GZ_MOVE_NODE (
      p_topo          IN VARCHAR2,
      p_node_id       IN NUMBER,
      p_move_point    IN SDO_GEOMETRY,
      p_debug         IN NUMBER DEFAULT NULL,
      p_topology      IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 1/22/11
      --This is meant to be a general purpose wrapper for sdo_topo_map.move_node
      --   Which is sdo_topo_WACK to use
      --Standard usage: pass in first arg p_topo. If this comes in, will build a window topomap here
      --Like topo functions, pass in NULL for p_topo if an appropriate topomap is already open
      --   NB: Be sure to then also include the optional parameter 5, so we know what topology this actually is

      --10/06/11 Added p_topology option.  Dont use it. Use p_topo and build topomap on the fly


      edge_ids          GZ_TYPES.stringarray;
      pos_edge_ids      GZ_TYPES.stringarray;
      edge_geoms        GZ_TYPES.geomarray;
      psql              VARCHAR2(4000);
      newnumarray       SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY();
      ordkount          PLS_INTEGER := 0;
      wack_edge_array   SDO_EDGE_ARRAY := SDO_EDGE_ARRAY();
      create_it         NUMBER;
      window_geom       SDO_GEOMETRY;
      getoutofjail      PLS_INTEGER := 0;
      passed_topo       VARCHAR2(4000);


   BEGIN

      IF p_topology IS NULL
      THEN

         --standard usage. Pass in a topology, we will build a topomap
         --use this guy for SQL
         passed_topo := p_topo;

      ELSE

         --p_topology is not null
         --nonstandard usage. Topomap exists
         passed_topo := p_topology;

      END IF;


      --get our bucket, must maintain the order
      psql := 'SELECT tt.edge_id, ABS(tt.edge_id), e.geometry '
           || 'FROM '
           || '  (SELECT rownum myorder, t.column_value edge_id '
           || '   FROM TABLE(SDO_TOPO_MAP.GET_NODE_STAR(:p1, NULL, :p2)) t '
           || '  ) tt, '
           || passed_topo || '_edge$ e '
           || 'WHERE '
           || 'ABS(tt.edge_id) = e.edge_id '
           || 'ORDER BY tt.myorder ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_ids,
                                               pos_edge_ids,
                                               edge_geoms USING passed_topo,
                                                                p_node_id;



      --edge ids are in correct order for move_node
      --positive edge_ids mean the geometry runs from our changing spot --> end
      --negative edge_ids mean the geometry runs from start node --> our change

      FOR i IN 1 .. edge_ids.COUNT
      LOOP

         newnumarray.EXTEND(edge_geoms(i).sdo_ordinates.COUNT);

         FOR j IN 1 .. (edge_geoms(i).sdo_ordinates.COUNT)/2
         LOOP


            IF edge_ids(i) > 0
            AND j = 1
            THEN

               --move start
               ordkount := ordkount + 1;
               newnumarray(ordkount) := p_move_point.sdo_point.X;
               ordkount := ordkount + 1;
               newnumarray(ordkount) := p_move_point.sdo_point.Y;

            ELSIF edge_ids(i) < 0
            AND j = (edge_geoms(i).sdo_ordinates.COUNT)/2
            THEN

               --move end
               ordkount := ordkount + 1;
               newnumarray(ordkount) := p_move_point.sdo_point.X;
               ordkount := ordkount + 1;
               newnumarray(ordkount) := p_move_point.sdo_point.Y;

            ELSE

               --transfer ordinates unchanged
               ordkount := ordkount + 1;
               newnumarray(ordkount) := edge_geoms(i).sdo_ordinates((j * 2) - 1);
               ordkount := ordkount + 1;
               newnumarray(ordkount) := edge_geoms(i).sdo_ordinates(j * 2);

            END IF;

         END LOOP;

            --this edge has a num array with an altered tip
            wack_edge_array.EXTEND;
            wack_edge_array(i) := newnumarray;

            --reset for next loop
            newnumarray.DELETE;
            ordkount := 0;

      END LOOP;


      IF p_debug = 1
      THEN

         GZ_GEOM_UTILS.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);

      END IF;


      <<jailbreak>>

      IF p_topo IS NOT NULL
      THEN

         --Make a window for the topomap

         IF getoutofjail = 0
         THEN

            --SOP
            psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
                 || 'FROM '
                 || p_topo || '_edge$ e '
                 || 'WHERE '
                 || 'e.edge_id IN (SELECT * FROM TABLE(:p1)) ';

            EXECUTE IMMEDIATE psql INTO window_geom
                                        USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(pos_edge_ids);

         ELSE

            --need to load the entire topology to work around move_node bug

            psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
                 || 'FROM '
                 || p_topo || '_edge$ e ';

            EXECUTE IMMEDIATE psql INTO window_geom;

         END IF;


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

      END IF;



      BEGIN


         --there will always be a topomap at this point
         SDO_TOPO_MAP.MOVE_NODE(NULL,
                                p_node_id,
                                wack_edge_array);

         --consider other signature for bug management as well
                                --moved_iso_nodes,
                                --moved_iso_edges,
                                --'TRUE'); or FALSE


      EXCEPTION
         WHEN OTHERS
         THEN

            --cleanliness is something
            IF p_topo IS NOT NULL
            THEN

               --commit and drop topomap
               create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

            END IF;

            IF SQLERRM LIKE '%Moved node has not moved%'
            THEN

               --probably a tolerance issue.  Maybe ignore it?
               GZ_GEOM_UTILS.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
               RAISE;

            ELSIF SQLERRM LIKE '%An edge does not maintain the position of the opposite end node%'
            THEN

               --Fracked up the order probably
               GZ_GEOM_UTILS.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
               RAISE;

            ELSIF SQLERRM LIKE '%not found in cache%'
            THEN

               --oracle.spatial.topo.TopoEntityNotFoundException: Edge ID 337 not found in cache

               --This appears to be a bug
               --Get it when not using a topomap.  Manual workaround: use a topomap

               --Also get it when using a topomap.  Adding the edge returned to the window just
               --   chases the error to a new edge ad infinitum
               --Automated workaround here: Make a topomap that includes the whole topology

               IF p_topo IS NOT NULL
               THEN

                  --We made a topomap to work with so we may get out of jail here

                  IF getoutofjail = 0
                  THEN

                     getoutofjail := 1;
                     GOTO jailbreak;

                  ELSE

                     GZ_GEOM_UTILS.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
                     RAISE_APPLICATION_ERROR(-20001, 'Got the not found in cache error 2x! '
                                       || 'SDO_TOPO_MAP.MOVE_NODE(NULL,' || p_node_id || ', <edge array>);'
                                       || SQLERRM);

                  END IF;

               ELSE

                  GZ_GEOM_UTILS.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
                  RAISE_APPLICATION_ERROR(-20001, 'Got the not found in cache error, consider using a topomap '
                                       || 'SDO_TOPO_MAP.MOVE_NODE(NULL,' || p_node_id || ', <edge array>);'
                                       || SQLERRM);

               END IF;

            ELSE

               GZ_GEOM_UTILS.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
               RAISE;

            END IF;

      END;

      IF p_debug = 1
      THEN

         dbms_output.put_line('jail is ' || getoutofjail);

      END IF;

      IF p_topo IS NOT NULL
      THEN

         --commit and drop topomap
         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

      END IF;


   END GZ_MOVE_NODE;

    ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION GZ_GET_FSL_FACES (
      p_fsl_tab       IN VARCHAR2,
      p_fsl_oid       IN NUMBER,
      p_fsl_oid_col   IN VARCHAR2 DEFAULT 'OID',
      p_topo_col      IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt!  11/15/11
      --Quickie for Sidey, not fully tested or thought through
      --
      --DECLARE
      -- face_ids gz_types.stringarray;
      --BEGIN
      -- face_ids := GZ_UTILITIES.GZ_GET_FSL_FACES('Z601LS_FSL050',27590310671100);
      --END;

      psql              VARCHAR2(4000);
      topology          VARCHAR2(4000);
      tg_layer_level    NUMBER;
      output            GZ_TYPES.stringarray;


   BEGIN

      IF NOT (GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_fsl_tab))
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Partner, table ' || p_fsl_tab || ' doesnt exist ');

      END IF;

      psql := 'SELECT a.topology, a.tg_layer_level '
           || 'FROM '
           || 'user_sdo_topo_Info a '
           || 'WHERE a.table_name = :p1 ';

      EXECUTE IMMEDIATE psql INTO topology,
                                  tg_layer_level USING UPPER(p_fsl_tab);

      CASE tg_layer_level

         WHEN 0 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         WHEN 1 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_relation$ r2, '
                 || topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         WHEN 2 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_relation$ r2, '
                 || topology || '_relation$ r3, '
                 || topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = f.face_id AND '
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         WHEN 3 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_relation$ r2, '
                 || topology || '_relation$ r3, '
                 || topology || '_relation$ r4, '
                 || topology || '_face$ f '
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
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         WHEN 4 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_relation$ r2, '
                 || topology || '_relation$ r3, '
                 || topology || '_relation$ r4, '
                 || topology || '_relation$ r5, '
                 || topology || '_face$ f '
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
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         WHEN 5 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_fsl_tab || ' a, '
                 || topology || '_relation$ r1, '
                 || topology || '_relation$ r2, '
                 || topology || '_relation$ r3, '
                 || topology || '_relation$ r4, '
                 || topology || '_relation$ r5, '
                 || topology || '_relation$ r6, '
                 || topology || '_face$ f '
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
                 || 'a.' || p_fsl_oid_col || ' = :p1 ';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Hierarchy level ' ||tg_layer_level || ' is yet to be implemented!');

      END CASE;

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING p_fsl_oid;


      --is output of nothing OK?  Lets say no for now

      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Didnt find any faces for ' || p_fsl_tab || ' ' || p_fsl_oid_col || ' ' || p_fsl_oid);

      END IF;

      RETURN output;


   END GZ_GET_FSL_FACES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION GZ_GET_FSL_BDYEDGES (
      p_fsl_tab       IN VARCHAR2,
      p_fsl_oid       IN NUMBER,
      p_fsl_oid_col   IN VARCHAR2 DEFAULT 'OID',
      p_topo_col      IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt!  11/15/11
      --Quickie for Sidey, not fully tested or thought through
      --Probably super slow, turns out we dont need this so I didnt really finish it
      --I also didnt really test extensively

      --See GZ_BUILD_SOURCE.GZ_GET_BDYEDGES for a much faster and more generic utility

      --DECLARE
      -- edges gz_types.stringarray;
      --BEGIN
      -- edges := gz_utilities.GZ_GET_FSL_BDYEDGES('Z601LS_FSL050',27590310671100);
      --END;

      psql              VARCHAR2(4000);
      output            GZ_TYPES.stringarray;
      topology          VARCHAR2(4000);
      tg_layer_level    NUMBER;
      faces             GZ_TYPES.stringarray;

   BEGIN


      IF NOT (GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_fsl_tab))
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Partner, table ' || p_fsl_tab || ' doesnt exist ');

      END IF;

      psql := 'SELECT a.topology, a.tg_layer_level '
           || 'FROM '
           || 'user_sdo_topo_Info a '
           || 'WHERE a.table_name = :p1 ';

      EXECUTE IMMEDIATE psql INTO topology,
                                  tg_layer_level USING UPPER(p_fsl_tab);

      faces := GZ_TOPO_UTIL.GZ_GET_FSL_FACES(p_fsl_tab,
                                             p_fsl_oid,
                                             p_fsl_oid_col,
                                             p_topo_col);


      --This is atrocious
      --Should be rewritten for performance

      psql := 'SELECT e.edge_id FROM '
           || topology || '_edge$ e '
           || 'WHERE '
           || 'e.left_face_id IN (SELECT * FROM TABLE(:p1)) AND '
           || 'e.right_face_id NOT IN (SELECT * FROM TABLE(:p2)) '
           || 'UNION ALL '
           || 'SELECT e.edge_id FROM '
           || topology || '_edge$ e '
           || 'WHERE '
           || 'e.right_face_id IN (SELECT * FROM TABLE(:p3)) AND '
           || 'e.left_face_id NOT IN (SELECT * FROM TABLE(:p4)) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING GZ_BUSINESS_UTILS.stringarray_to_varray(faces),
                                                            GZ_BUSINESS_UTILS.stringarray_to_varray(faces),
                                                            GZ_BUSINESS_UTILS.stringarray_to_varray(faces),
                                                            GZ_BUSINESS_UTILS.stringarray_to_varray(faces);

      --error?
      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Couldnt find any edges for ' || p_fsl_tab || ' ' || p_fsl_oid_col || ' ' || p_fsl_oid);

      END IF;

      RETURN output;


   END GZ_GET_FSL_BDYEDGES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION GZ_COUNT_FSL_OVERLAPS (
      p_topology        IN VARCHAR2,
      p_fsl_tab         IN VARCHAR2,
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN NUMBER
   AS

      --Matt 1/14/13
      --Postulate: For any GZ topology
      --   A 0 level feature table contains no spatial overlap if no topo_ids are duplicated in relation$
      --   Any 1+ level feature table contains no spatial overlap if both
      --      No topo_types (ie pointers to lower relation$ records) are duplicated in relation$ and
      --      All layer(s) on which its built, down to 0-level, contain no overlaps


      psql                 VARCHAR2(4000);
      kount                NUMBER;
      tg_layer_id          NUMBER;
      tg_layer_level       NUMBER;
      group_col            VARCHAR2(32);


   BEGIN

      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_topology,
                                                  p_fsl_tab,
                                                  p_topo_col,
                                                  'POLYGON');

      tg_layer_level := GZ_TOPO_UTIL.GET_TG_LAYER_LEVEL(p_topology,
                                                        p_fsl_tab,
                                                        p_topo_col,
                                                        'POLYGON');

      IF tg_layer_level = 0
      THEN

         group_col := 'TOPO_ID';

      ELSE

         group_col := 'TOPO_TYPE';

      END IF;

      --ex1 REGION in MT
      --select count(*) from bas13.mt_relation$
      --where tg_layer_id = 2721
      --group by topo_type
      --having count(topo_type) > 1

      --ex2 FACE feature in GZ
      --select count(*) from t699in_relation$
      --where tg_layer_id = 1
      --group by topo_id
      --having count(topo_id) > 1

      --Need an outer select count(*)
      --When the group by having return no results the inner query return no rows
      --at all, which is uncountable, its not even null

      psql := 'SELECT COUNT(*) FROM ('
           || 'SELECT COUNT(*) '
           || 'FROM ' || p_topology || '_relation$ '
           || 'WHERE '
           || 'tg_layer_id = :p1 '
           || 'GROUP BY ' || group_col || ' '
           || 'HAVING COUNT(' || group_col || ') > :p2 '
           || ')';

      EXECUTE IMMEDIATE psql INTO kount USING tg_layer_id,
                                              1;


      RETURN kount;

   END GZ_COUNT_FSL_OVERLAPS;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION GZ_COUNT_TOPO_OVERLAPS (
      p_topology        IN VARCHAR2,
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN NUMBER
   AS

      --Matt! 1/14/13
      --All-feature-table-in-topo wrapper for GZ_COUNT_FSL_OVERLAPS
      --Basically you want to check a topology and get back 0

      psql                 VARCHAR2(4000);
      tabs                 GZ_TYPES.stringarray;
      total_overlaps       NUMBER := 0;
      overlap              NUMBER;


   BEGIN

      psql := 'SELECT table_name FROM user_sdo_topo_info a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.column_name = :p2 '
           || 'ORDER BY tg_layer_level ASC ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING UPPER(p_topology),
                                                          UPPER(p_topo_col);


      FOR i IN 1 .. tabs.COUNT
      LOOP


         overlap := GZ_TOPO_UTIL.GZ_COUNT_FSL_OVERLAPS(p_topology,
                                                       tabs(i),
                                                       p_topo_col);

         total_overlaps := total_overlaps + overlap;

      END LOOP;

      RETURN total_overlaps;


   END GZ_COUNT_TOPO_OVERLAPS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE GZ_ADD_TOPO_GEOMETRY_LAYER (
      p_topo            IN VARCHAR2,
      p_table_name      IN VARCHAR2,
      p_column_name     IN VARCHAR2 DEFAULT 'TOPOGEOM',
      p_layer_type      IN VARCHAR2 DEFAULT 'POLYGON',
      p_child_tab       IN VARCHAR2 DEFAULT NULL
   )
   AS

     --Matt! 12/20/11 Wrapper for SDO_TOPO.ADD_TOPO_GEOMETRY_LAYER
     --Not sure why I didnt write this until now

      child_tg_layer_id          NUMBER;

   BEGIN

      IF p_layer_type NOT IN ('POINT', 'LINE', 'CURVE', 'POLYGON', 'COLLECTION')
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Cousin, what is a layer type of ' || p_layer_type || '?');

      END IF;


      IF p_child_tab IS NULL
      THEN

         child_tg_layer_id := NULL;

      ELSE

         child_tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_topo,
                                                           p_child_tab,
                                                           p_column_name,  --assume same as parent
                                                           p_layer_type);

      END IF;


      BEGIN

         SDO_TOPO.add_topo_geometry_layer(p_topo,
                                          p_table_name,
                                          p_column_name,
                                          p_layer_type,
                                          NULL,               --never mess with relation$ storage
                                          child_tg_layer_id); --child layer id likely NULL


      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLERRM LIKE '%Need use delete_topo_geometry_layer%'  --yes, really written like that
         THEN

            SDO_TOPO.DELETE_TOPO_GEOMETRY_LAYER(p_topo,p_table_name,p_column_name);

            --try again
            SDO_TOPO.add_topo_geometry_layer(p_topo,
                                          p_table_name,
                                          p_column_name,
                                          p_layer_type,
                                          NULL,               --never mess with relation$ storage
                                          child_tg_layer_id); --child layer id

         ELSE

            --what else can we catch here?

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

         END IF;

      END;


   END GZ_ADD_TOPO_GEOMETRY_LAYER;





   -----------------------------------------------------------------------------
   -- Topo primitive and feature creators
   -----------------------------------------------------------------------------


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   PROCEDURE BUILD_TOPO_FROM_SPATIAL (
      p_toponame           IN VARCHAR2,
      p_featuretable       IN VARCHAR2,
      p_topo_type          IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_validate_geom      IN VARCHAR2 DEFAULT 'NO',
      p_validate_tol       IN NUMBER DEFAULT .05,
      p_fix_geom           IN VARCHAR2 DEFAULT 'NO',
      p_topo_col           IN VARCHAR2 DEFAULT 'TOPOGEOM',
      p_geom_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_oid_col            IN VARCHAR2 DEFAULT NULL,
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_val         IN VARCHAR2 DEFAULT NULL
   ) AS

      --This is Matt's original create_feature wrapper
      --See ADD_TOPO_FROM_SPATIAL for a non-create_feature solution
      --I dont call this guy any more 4/27/12
      --Salman may still use it, he was at one point

      --Matt! 3/10/10
      --updated to handle and drop any other updatable topomaps attached to the calling session
      --   Matt! 5/25/10
      --Reworked updatable topomaps dropping 6/18/10
      --Added oid population option 7/10/10
      --Added options to build topo for subset of records 12/10/10

      --Expected Inputs
      --   1. A table (p_featuretable) with
      --      1a. Some sort of primary key column (p_featuretable_pkc) for ex edge_id
      --      1b. A column with geometry, populated and indexed (p_geom_col)
      --      1c. An empty topo column (p_topo_col)
      --   2. A pre-existing topology (p_toponame)
      --Output
      --   Populates the topo column and all the usual $ tables
      --   Optionally populates an oid-like column with the tg_id

      --ex: GZ_UTILITIES.BUILD_TOPO_FROM_SPATIAL('STATEFP10','GEN_ST_EDGES_HI_10','LINE','ID','YES',.05);

      --ex Feature table exists, I just want to populate some of the topo records
      --   GZ_UTILITIES.BUILD_TOPO_FROM_SPATIAL('STATEFP10','GEN_ST_EDGES_HI_10','LINE','ID','YES',.05, 'NO', 'TOPOGEOM,
      --                                        'SDOGEOMETRY', oid, 'DANGLE', 'T');  --only populate topo where DANGLE = T


      valresult            VARCHAR2(4000);
      psql                 VARCHAR2(4000);
      psql2                VARCHAR2(4000);
      kount                PLS_INTEGER;
      toponame             VARCHAR2(32) := UPPER(p_toponame);
      featuretable         VARCHAR2(32) := UPPER(p_featuretable);
      topo_type            VARCHAR2(32) := UPPER(p_topo_type);
      featuretable_pkc     VARCHAR2(32) := UPPER(p_featuretable_pkc);
      topo_col             VARCHAR2(32) := UPPER(p_topo_col);
      geom_col             VARCHAR2(32) := UPPER(p_geom_col);
      newtopomap           VARCHAR2(4000) := toponame || '_TOPOMAP';  --ever need to parameterize?
      my_cursor            SYS_REFCURSOR;
      TYPE toporec         IS RECORD (
                           id NUMBER,
                           sdogeometry SDO_GEOMETRY );
      TYPE topot           IS TABLE OF toporec;
      topotab              topot;
      topogeom_arr         GZ_TYPES.SDO_TOPO_GEOMETRY_ARRAY_PLSINT;
      featuretable_ids     GZ_TYPES.stringarray;
      featuretable_oids    GZ_TYPES.stringarray;
      topomaps             GZ_TYPES.stringarray;
      ez_topo_mgr          NUMBER;

   BEGIN



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Verify Inputs ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_validate_geom != 'NO'
      THEN

         --validate geometry, adapted from advice in topo documentation
         --http://download.oracle.com/docs/html/B14256_01/sdo_topo_concepts.htm#CIHBGDEC

         psql := 'SELECT count(*) '
              || 'FROM ' || featuretable || ' a '
              || 'WHERE '
              || 'SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.' || geom_col || ', :p1) != :p2 ';

         IF p_subset_col IS NOT NULL
         THEN

            psql := psql || ' AND ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

         END IF;

         EXECUTE IMMEDIATE psql INTO kount USING p_validate_tol,
                                                 'TRUE';

         IF kount != 0
         THEN

            --Most likely duplicate vertices

            IF p_fix_geom = 'NO'
            THEN

               RAISE_APPLICATION_ERROR(-20001,'Geometries in ' || featuretable || ' are not valid ');

            ELSE

               --Let the grasping at straws begin

               psql2 := 'UPDATE ' || featuretable || ' a '
                     || 'SET '
                     || 'a.' || geom_col || ' = SDO_UTIL.REMOVE_DUPLICATE_VERTICES(a.' || geom_col || ' , ' || p_validate_tol || ' ) ';

               EXECUTE IMMEDIATE psql2;

               EXECUTE IMMEDIATE psql INTO kount USING p_validate_tol,
                                                       'TRUE';

               IF kount != 0
               THEN

                  RAISE_APPLICATION_ERROR(-20001,'Geometries in ' || featuretable || ' are not valid even after vertex removal ');

               END IF;

            END IF;


         END IF;

      END IF;


      IF topo_type NOT IN ('POINT', 'LINE', 'CURVE', 'POLYGON', 'COLLECTION')
      THEN

         --valid inputs to SDO_TOPO.add_topo_geometry_layer
         RAISE_APPLICATION_ERROR(-20001, 'What the heck is a topo type of ' || topo_type);

      END IF;


      --???????????????????????????
      --What else should we check??
      --???????????????????????????


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Add topo layer ' || featuretable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_subset_col IS NULL
      THEN

         SDO_TOPO.add_topo_geometry_layer(toponame,featuretable,topo_col,topo_type);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Initialize topo map ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      -- Mike Z Voodoo? Salman also rceommends this
      --But Sreeni points out that if you use sensibly sized topo maps you shouldnt need it
      -- Set sdo_topo_map maximum Java memory size
      --EZ_TOPOMAP_MANAGER handles this now.  EZ!
      --SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(2147483648);


      ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(newtopomap,toponame,2);


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Create features for ' || featuretable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      psql := 'SELECT ' || featuretable_pkc || ', ' || geom_col || ' '
           || 'FROM ' || featuretable || ' ';

           IF p_subset_col IS NOT NULL
           THEN

              psql := psql || ' WHERE ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

           END IF;


      BEGIN

         OPEN my_cursor FOR psql;

         LOOP

            FETCH my_cursor BULK COLLECT INTO topotab LIMIT 100; --parameterize LIMIT?
            EXIT WHEN topotab.COUNT = 0;

            FOR i in 1 .. topotab.COUNT
            LOOP


               topogeom_arr(i) := SDO_TOPO_MAP.CREATE_FEATURE(toponame,featuretable,topo_col,topotab(i).sdogeometry);
               featuretable_ids(i) := topotab(i).id;


               IF p_oid_col IS NOT NULL
               THEN

                  --if desired, build a separate array of tg_ids
                  --to populate the source table oid-like field
                  featuretable_oids(i) := topogeom_arr(i).tg_id;

               END IF;


            END LOOP;

            --Update this bucket's worth
            --1 transaction Yo

            IF p_oid_col IS NULL
            THEN

               FORALL ii IN 1 .. featuretable_ids.COUNT
                  EXECUTE IMMEDIATE 'UPDATE ' || featuretable || ' a '
                                 || 'SET a.' || topo_col || ' = :p1 '
                                 || 'WHERE a.' || featuretable_pkc || ' = :p2 '
                  USING topogeom_arr(ii),
                        featuretable_ids(ii);

            ELSE

               FORALL ii IN 1 .. featuretable_ids.COUNT
                  EXECUTE IMMEDIATE 'UPDATE ' || featuretable || ' a '
                                 || 'SET a.' || topo_col || ' = :p1, '
                                 || 'a.' || p_oid_col || ' = :p2 '
                                 || 'WHERE a.' || featuretable_pkc || ' = :p3 '
                  USING topogeom_arr(ii),
                        featuretable_oids(ii),
                        featuretable_ids(ii);


            END IF;


            COMMIT;

            --Tidy up
            featuretable_ids.DELETE;
            topogeom_arr.DELETE;

            IF p_oid_col IS NOT NULL
            THEN
               featuretable_oids.DELETE;
            END IF;


         END LOOP;

      END;

      CLOSE my_cursor;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Commit n Drop topomap ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      SDO_TOPO_MAP.COMMIT_TOPO_MAP();
      SDO_TOPO_MAP.DROP_TOPO_MAP(newtopomap);


      --Initialize metadata here? Maybe we should parameterize this Y or N
      SDO_TOPO.INITIALIZE_METADATA(toponame);


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_SPATIAL: Complete ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --Anything to return?


   END BUILD_TOPO_FROM_SPATIAL;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

    PROCEDURE ADD_TOPO_FROM_SPATIAL (
      p_toponame           IN VARCHAR2,
      p_featuretable       IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_topo_type          IN VARCHAR2,
      p_log_type           IN VARCHAR2,
      p_geom_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_val         IN VARCHAR2 DEFAULT NULL,
      p_allow_splits       IN VARCHAR2 DEFAULT 'Y',
      p_new_layer          IN VARCHAR2 DEFAULT 'N',
      p_topomap_mbr        IN SDO_GEOMETRY DEFAULT NULL
   ) AS

      --Matt! 4/27/12
      --! 8/8/13 added "single record mode" error return
      --Generic replacement for create feature
      -- *ADD*_TOPO_FROM.. because it calls the ADD_xxx interfaces
      --Calls ADD_POLYGON/LINEAR/POINT_GEOMETRY
      --Then constructs the topogeom column

      --Sample call:
      --GZ_UTILITIES.ADD_TOPO_FROM_SPATIAL('T699LS','T699LS_CUTTER','CUTTER_ID','POLYGON',
      --                                   'MERGE','SDOGEOMETRY',NULL,NULL,'Y','Y');


      psql                 VARCHAR2(4000);
      toponame             VARCHAR2(32) := UPPER(p_toponame);
      featuretable         VARCHAR2(32) := UPPER(p_featuretable);
      featuretable_pkc     VARCHAR2(32) := UPPER(p_featuretable_pkc);
      geom_col             VARCHAR2(32) := UPPER(p_geom_col);
      newtopomap           VARCHAR2(4000) := toponame || '_TOPOMAP';  --ever need to parameterize?
      my_cursor            SYS_REFCURSOR;
      TYPE toporec         IS RECORD (
                           id NUMBER,
                           sdogeometry SDO_GEOMETRY );
      TYPE topot           IS TABLE OF toporec;
      topotab              topot;
      ez_topo_mgr          NUMBER;
      stupid_number_array  SDO_NUMBER_ARRAY;
      TYPE stupidrec       IS RECORD (
                           feat_id NUMBER,
                           prim_ids SDO_TOPO_OBJECT_ARRAY );
      TYPE stupid_arr      IS TABLE OF stupidrec INDEX BY PLS_INTEGER;
      stupid_array         stupid_arr;
      kurrent_kount        PLS_INTEGER := 0;
      tg_layer_id          NUMBER;
      me_tg_type           NUMBER;


   BEGIN


      --verify inputs?

      IF p_topo_type NOT IN ('POINT', 'LINE', 'POLYGON')
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'What the heck is a topo type of ' || p_topo_type || ' ?');

      END IF;

      IF p_topo_type = 'POINT'
      THEN
         me_tg_type := 1;
      ELSIF p_topo_type = 'LINE'
      THEN
         me_tg_type := 2;
      ELSIF p_topo_type = 'POLYGON'
      THEN
         me_tg_type := 3;
      END IF;

      --table exists?

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_TOPO_FROM_SPATIAL: Initialize topo map ' || newtopomap);
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
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_TOPO_FROM_SPATIAL: Create features for ' || featuretable);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      psql := 'SELECT ' || featuretable_pkc || ', ' || geom_col || ' '
           || 'FROM ' || featuretable || ' ';

      IF p_subset_col IS NOT NULL
      THEN

         psql := psql || ' WHERE ' || p_subset_col || ' = ''' || p_subset_val || ''' ';

      END IF;

      IF p_log_type IS NOT NULL
      THEN
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                toponame,
                                                'ADD_TOPO_FROM_SPATIAL',
                                                featuretable,
                                                'Opening cursor to call add_xxxx_geometry',
                                                NULL,NULL,NULL,psql);
      END IF;


      BEGIN

         OPEN my_cursor FOR psql;

         LOOP

            FETCH my_cursor BULK COLLECT INTO topotab LIMIT 25; --parameterize LIMIT?
            EXIT WHEN topotab.COUNT = 0;

            FOR i in 1 .. topotab.COUNT
            LOOP


               BEGIN


                  --THE CALL----
                  --**********--

                  IF p_topo_type = 'POLYGON'
                  THEN

                     stupid_number_array := SDO_TOPO_MAP.ADD_POLYGON_GEOMETRY(NULL,topotab(i).sdogeometry);

                  ELSIF p_topo_type = 'LINE'
                  THEN

                     stupid_number_array := SDO_TOPO_MAP.ADD_LINEAR_GEOMETRY(NULL,topotab(i).sdogeometry);

                  ELSIF p_topo_type = 'POINT'
                  THEN

                     --never tried this. Just return a single number
                     stupid_number_array(1) := SDO_TOPO_MAP.ADD_POINT_GEOMETRY(NULL,topotab(i).sdogeometry);

                  END IF;

                  --------------
                  --------------

                  IF p_allow_splits = 'N'
                  AND stupid_number_array.COUNT > 1
                  THEN

                     IF p_log_type IS NOT NULL
                     THEN
                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                               toponame,
                                                               'ADD_TOPO_FROM_SPATIAL',
                                                               featuretable,
                                                               'ADD_TOPO_FROM_SPATIAL error message on id ' || topotab(i).id,
                                                               NULL,NULL,NULL,NULL,NULL,
                                                               'You said no splits, but we got ' || stupid_number_array.COUNT || ' primitives',
                                                               topotab(i).sdogeometry);
                     END IF;

                     RAISE_APPLICATION_ERROR(-20001,'Comrade: Expecting to create 1 edge (or face) from each input record. '
                                                 || 'Got ' || stupid_number_array.COUNT || ' primitives '
                                                 || 'on ' || featuretable_pkc || ' ' || topotab(i).id || ' in table ' || featuretable);

                  END IF;


               EXCEPTION
               WHEN OTHERS
               THEN

                  IF p_log_type IS NOT NULL
                  THEN
                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                            toponame,
                                                            'ADD_TOPO_FROM_SPATIAL',
                                                            featuretable,
                                                            'ADD_XXXX_GEOMETRY error message--> ',
                                                            NULL,NULL,NULL,NULL,NULL,
                                                            SQLERRM
                                                            );
                  END IF;

                  IF (UPPER(SQLERRM) LIKE '%OUTOFMEMORYERROR%'
                  OR UPPER(SQLERRM) LIKE '%JAVA OUT OF MEMORY CONDITION%')  --WTF causes this instead of the first?
                  THEN

                     --nothing in this handler has ever succeeded
                     --attempting to avoid this ahead of time in the caller

                     IF p_log_type IS NOT NULL
                     THEN
                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                               toponame,
                                                               'ADD_TOPO_FROM_SPATIAL',
                                                               featuretable,
                                                               'Memory error, gonna call the magick java_memory_manager ');

                     END IF;

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

                        IF p_log_type IS NOT NULL
                        THEN
                           GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                                  toponame,
                                                                  'ADD_TOPO_FROM_SPATIAL',
                                                                  featuretable,
                                                                  'JAVA_MEMORY_MANAGER failed to clean house-->',
                                                                  NULL,NULL,NULL,NULL,NULL,
                                                                  SQLERRM);
                        END IF;

                        RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

                     END;

                  ELSIF UPPER(SQLERRM) LIKE '%ATTEMPTED TO ADD AN ISO NODE THAT LIES ON AN EXISTING EDGE OR NODE%'
                  THEN

                     --ORA-29532: Java call terminated by uncaught Java exception: oracle.spatial.topo.InvalidTopoOperationException:
                     --Attempted to add an iso node that lies on an existing edge or node
                     --Bug seen one time adding an edge that exactly touches a long line of latitude in the topology
                     --Must go into single record mode since the topo is hosed
                     RAISE_APPLICATION_ERROR(-20001, 'SINGLE RECORD MODE');

                  ELSE

                     RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

                  END IF;

               END;

               --*************---
               --TUCK whats wes haves gots
               ------------------

               --counter
               kurrent_kount := kurrent_kount + 1;

               --place the feature id in the outer record id
               stupid_array(kurrent_kount).feat_id := topotab(i).id;
               --stupid extend the nested object
               stupid_array(kurrent_kount).prim_ids := SDO_TOPO_OBJECT_ARRAY();
               stupid_array(kurrent_kount).prim_ids.EXTEND(stupid_number_array.COUNT);

               --stupid objects
               FOR jj IN 1 .. stupid_number_array.COUNT
               LOOP

                  --prim_ids: SDO_TOPO_OBJECT_ARRAY is VARRAY (1000000) of SDO_TOPO_OBJECT
                  stupid_array(kurrent_kount).prim_ids(jj) := SDO_TOPO_OBJECT(stupid_number_array(jj), me_tg_type);

               END LOOP;

               -----------------
               --End tucking the results of this poly/edge/point into our nested objects
               --Back for another
               ------------------


            END LOOP;


         END LOOP;

      END;

      CLOSE my_cursor;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_TOPO_FROM_SPATIAL: Commit n Drop topomap ' || newtopomap);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_log_type IS NOT NULL
      THEN
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                toponame,
                                                'ADD_TOPO_FROM_SPATIAL',
                                                 featuretable,
                                                 'Commit and drop topo map');
      END IF;



      SDO_TOPO_MAP.COMMIT_TOPO_MAP();
      SDO_TOPO_MAP.DROP_TOPO_MAP(newtopomap);

      IF p_log_type IS NOT NULL
      THEN
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                toponame,
                                                'ADD_TOPO_FROM_SPATIAL',
                                                 featuretable,
                                                 'Completed commit and drop topo map');
      END IF;




      --after topo map commit...

      IF p_new_layer = 'Y'
      THEN

         SDO_TOPO.add_topo_geometry_layer(toponame,featuretable,'TOPOGEOM',p_topo_type);

      END IF;

      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(toponame,
                                                  featuretable,
                                                  'TOPOGEOM',
                                                  p_topo_type);

      psql := 'UPDATE ' || featuretable || ' a '
           || 'SET '
           || 'a.topogeom = SDO_TOPO_GEOMETRY(:p1,:p2,:p3,:p4) '
           || 'WHERE a.' || featuretable_pkc || ' = :p5 ';

      IF p_log_type IS NOT NULL
      THEN
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                toponame,
                                                'ADD_TOPO_FROM_SPATIAL',
                                                featuretable,
                                                'Constructorfest on ' || stupid_array.COUNT || ' features',
                                                NULL,NULL,NULL,psql);
      END IF;

      FORALL ii IN 1 .. stupid_array.COUNT
         EXECUTE IMMEDIATE psql
         USING toponame,
               me_tg_type,
               tg_layer_id,
               stupid_array(ii).prim_ids,
               stupid_array(ii).feat_id;

      COMMIT;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('ADD_TOPO_FROM_SPATIAL: Complete ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_log_type IS NOT NULL
      THEN
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                toponame,
                                                'ADD_TOPO_FROM_SPATIAL',
                                                featuretable,
                                                'Done');
      END IF;

      --Anything to return?


   END ADD_TOPO_FROM_SPATIAL;









   --------------
   --Validation
   --------------

   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------------

   FUNCTION VALIDATE_TOPOLOGY_TILE (
      p_topo               IN VARCHAR2,
      p_tiles              IN GZ_TYPES.geomarray,
      p_delta              IN NUMBER DEFAULT 0.0001,
      p_edge_count         IN NUMBER DEFAULT 100000,
      p_memory_size        IN NUMBER DEFAULT NULL,
      p_log_type           IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt!  3/2/12
      --Tile-based wrapper for VALIDATE_TOPOLOGY (see just below)
      --Caller has tiles in her hands. Compare to GZ_VALIDATE_TOPOLOGY (also below)
      --   where caller has an extent table, no tiles

      --Like sdo_topo_map.validate_topology either return TRUE or raise an error
      --Tiles must be optimized rectangles
      --This function will add additional just-to-be-safe overlap based on the delta input
      --If the caller passes in overlapping tiles, like the MBRs of states, oh well
      --Expects the caller to know something about tiles - how many, extents, etc

      retval               VARCHAR2(4000);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VALIDATE_TOPOLOGY_TILE: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_log_type IS NOT NULL
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'VALIDATE_TOPOLOGY_TILE',
                                                p_topo,
                                                'Starting validation using ' || p_tiles.COUNT || ' tiles ');

      END IF;

      --validate our tiles

      IF p_tiles.COUNT = 0
      OR p_tiles(1).sdo_gtype IS NULL --topos have to have srids right?
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Problem with tiles - either none passed or they are empty');

      END IF;

      FOR i IN 1 .. p_tiles.COUNT
      LOOP

         IF p_tiles(i).sdo_ordinates.COUNT != 4
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Yo, I expect optimized rectangles for tiles, Im reading ordinate count '
                                            || p_tiles(i).sdo_ordinates.COUNT);

         END IF;

      END LOOP;


      --validate by tile

      FOR i IN 1 .. p_tiles.COUNT
      LOOP

         IF p_log_type IS NOT NULL
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   'VALIDATE_TOPOLOGY_TILE',
                                                   p_topo,
                                                   'Validating tile ' || i,
                                                   NULL,NULL,NULL,NULL,NULL,NULL,
                                                   p_tiles(i));

         END IF;

         BEGIN

            retval := GZ_TOPO_UTIL.VALIDATE_TOPOLOGY(p_topo,
                                                     p_edge_count,
                                                     p_memory_size,
                                                     (p_tiles(i).sdo_ordinates(1) - p_delta),
                                                     (p_tiles(i).sdo_ordinates(2) - p_delta),
                                                     (p_tiles(i).sdo_ordinates(3) + p_delta),
                                                     (p_tiles(i).sdo_ordinates(4) + p_delta));

         EXCEPTION
         WHEN OTHERS
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   'VALIDATE_TOPOLOGY_TILE',
                                                   p_topo,
                                                   'Validation failure or error, see error_msg for dets ',
                                                   NULL,NULL,NULL,NULL,NULL,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

            RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);


         END;

         IF retval != 'TRUE'
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Huhs? validate_topology returned ' || retval || ' without error');

         END IF;

      END LOOP;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VALIDATE_TOPOLOGY_TILE: Peace Out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN retval;

   END VALIDATE_TOPOLOGY_TILE;

   ------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------------

   FUNCTION VALIDATE_TOPOLOGY (
      p_topo               IN VARCHAR2,
      p_edge_count         IN NUMBER DEFAULT NULL,
      p_memory_size        IN NUMBER DEFAULT NULL,
      p_xmin               IN NUMBER DEFAULT NULL,
      p_ymin               IN NUMBER DEFAULT NULL,
      p_xmax               IN NUMBER DEFAULT NULL,
      p_ymax               IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! and GZteam  1/12/10
      --Matt! 8/8/11 added signature for x,y bounds
      --Modifications! 9/28/12 related to memory

      --This is the lowest level topo validator
      --Knows nothing of tiles
      --Does all the java memory management voodoo

      --Should always use this wrapper in place of sdo_topo_map.validate_topology
      --sample
      --    returnval := GZ_UTILITIES.VALIDATE_TOPOLOGY('MT');

      retval         VARCHAR2(4000);
      edgekount      NUMBER;
      raise_the_roof NUMBER;
      already_tried  PLS_INTEGER := 0;
      psql           VARCHAR2(4000);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VALIDATE_TOPOLOGY: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_memory_size IS NULL
      THEN
         raise_the_roof := 2147483648;
      ELSE
         raise_the_roof := p_memory_size;
      END IF;

      IF p_edge_count IS NOT NULL
      THEN

         --check edge count to decide whether or not to raise the roof
         --I dont really bother with this any more

         psql := 'SELECT count(*) '
              || 'FROM '
              || p_topo || '_edge$ ';

         EXECUTE IMMEDIATE psql INTO edgekount;

      ELSE

         --NULLS = scary
         edgekount := 0;

      END IF;

      IF edgekount > p_edge_count
      OR p_memory_size IS NOT NULL
      THEN

         --We either have buku edges
         --or the user has an override memory setting

         SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(raise_the_roof);

         --if not, go with the default, for now

      END IF;


      BEGIN

         --return TRUE or raises and error
         IF p_xmin IS NULL
         THEN

            --all of topology
            retval := SDO_TOPO_MAP.VALIDATE_TOPOLOGY(p_topo);

         ELSE

            retval := SDO_TOPO_MAP.VALIDATE_TOPOLOGY(p_topo,
                                                     p_xmin,
                                                     p_ymin,
                                                     p_xmax,
                                                     p_ymax);

         END IF;

      EXCEPTION
      WHEN OTHERS
      THEN

         --cant use real codes for java errors

         IF UPPER(SQLERRM) LIKE '%MEMORY%'
         THEN

            --Call the java memory manager with just the error message
            --he'll pull some java rabbits out of java hats
            --freeing unused memory, clearing session states, who knows what else

            GZ_BUSINESS_UTILS.JAVA_MEMORY_MANAGER(p_topo || '_edge$', --table not actually used
                                             NULL,               --sdo col not needed
                                             SQLERRM);

            --try again

            BEGIN

               IF p_xmin IS NULL
               THEN

                  retval := SDO_TOPO_MAP.VALIDATE_TOPOLOGY(p_topo);

               ELSE

                  retval := SDO_TOPO_MAP.VALIDATE_TOPOLOGY(p_topo,
                                                           p_xmin,
                                                           p_ymin,
                                                           p_xmax,
                                                           p_ymax);

               END IF;

            EXCEPTION
            WHEN OTHERS
            THEN

               IF UPPER(SQLERRM) LIKE '%MEMORY%'
               THEN

                  --caller is gonna get the memory error and will need to deal
                  RAISE;

               ELSE

                   --not valid, probably
                   --just raise, to mimic oracle topo validator
                   RAISE;

               END IF;

            END;


         ELSE

             --invalid topology
             --raise to mimic validator
             RAISE;

         END IF;


      END;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VALIDATE_TOPOLOGY: Peace Out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --valid, should return 'TRUE'
      RETURN retval;

   END VALIDATE_TOPOLOGY;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

  FUNCTION GZ_VALIDATE_TOPOLOGY (
      p_topo            IN VARCHAR2,
      p_outline_table   IN VARCHAR2,
      p_column          IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_delta           IN NUMBER DEFAULT 0.0001,
      p_log_type        IN VARCHAR2 DEFAULT NULL,
      p_tile_target     IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2
   AS


      --Matt! 8/8/11
      --Matt! 9/28/12 Made tiley and better able to handle memory errors
      --Matt! 11/15/12 added explicit tile target option.
      --!10/24/13 Fixed handler so it will properly subdivide on memory errors.  Was looking for %MEMORY% instead of %Memory%

      --This is the validator for a caller who has no tiles, or known tile count parameters
      --But does have some sort of an extent table, like a table of states

      --Called from Merge (maybe more), national validation. I have a "cookie cutter" table representing states (usually states)
      --Called from clip too, state based

      --Originally based on GZ_TOPO_BUILD.valtopo2 which may still be used by GZother modules
      --weird dependency in valtopo2 on MTUTIL schema and fsl040 table weird me out

      --sample, merge-like call
      --retval := GZ_UTILITIES.GZ_VALIDATE_TOPOLOGY('Z699LM', 'Z699LM_CUTTER');


      output            NUMBER := 1;
      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      my_cursor         SYS_REFCURSOR;
      tiles             GZ_TYPES.geomarray;
      subdivided_tiles  GZ_TYPES.geomarray;
      pxmin             NUMBER;
      pymin             NUMBER;
      pxmax             NUMBER;
      pymax             NUMBER;
      validstr          VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_VALIDATE_TOPOLOGY: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_outline_table)
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Table ' || p_outline_table || ' doesnt exist ');

      END IF;

      IF p_tile_target IS NULL
      THEN

         psql := 'SELECT COUNT(*) '
              || 'FROM ' || p_outline_table || ' ';

         EXECUTE IMMEDIATE psql INTO kount;  --usually something like 50 (for a national topo in the merge)

      ELSE

         kount := p_tile_target;

      END IF;

      --gz_tile_table almost always falls short, so request 2x what we want
      --to minimize the likelihood of memory errors

      tiles := GZ_GEOM_UTILS.GZ_TILE_TABLE(p_outline_table,
                                          (kount * 2),
                                          p_column,
                                          NULL,            --no whereclause
                                          NULL,            --no sdo filter,
                                          p_log_type,      --ex MERGE
                                          p_topo);         --log tag

      IF p_log_type IS NOT NULL
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                p_topo,
                                                'GZ_VALIDATE_TOPOLOGY',
                                                p_topo,
                                                'Validating ' || tiles.COUNT || ' tiles');

      END IF;

      FOR i IN 1 .. tiles.COUNT
      LOOP

         IF tiles(i).sdo_ordinates.COUNT != 4
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Wtf, tile ordinate count is ' || tiles(i).sdo_ordinates.COUNT);

         END IF;

         --delta is in units of the data, usually degrees
         --this is the only spot where the overlap is intentionally added
         --Nothing in the tiler, or in lower level VALIDATE_TOPOLOGY

         pxmin := tiles(i).sdo_ordinates(1) - p_delta;
         pymin := tiles(i).sdo_ordinates(2) - p_delta;
         pxmax := tiles(i).sdo_ordinates(3) + p_delta;
         pymax := tiles(i).sdo_ordinates(4) + p_delta;

         IF p_log_type IS NOT NULL
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                   p_topo,
                                                   'GZ_VALIDATE_TOPOLOGY',
                                                   p_topo,
                                                   'Validating tile ' || i,
                                                   NULL,NULL,NULL,NULL,NULL,NULL,
                                                   tiles(i));

         END IF;


         BEGIN

            --use the memory management wrapped and rewrapped validate_topology
            --All basic memory trickery happens in this sub
            --If it cant handle memory issues, then up here at the caller we need smaller tiles


            validstr := GZ_TOPO_UTIL.VALIDATE_TOPOLOGY(p_topo,
                                                       NULL,   --edge count, dont even bother any more
                                                       NULL,   --default memory size will override
                                                       pxmin,
                                                       pymin,
                                                       pxmax,
                                                       pymax);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF UPPER(SQLERRM) LIKE '%MEMORY%'
            THEN

               --ORA-29532: Java call terminated by uncaught Java exception: java.lang.OutOfMemoryError
               --Our tile is too big.  Or java is too bloated.  Its Rashomon

               --Why not use validate_topology_tile here, or in the first place?
               --This guy (GZ_VALIDATE_TOPOLOGY) has no tile info
               --He is free to tile and subdivide at will
               --validate_topology_tile is in a framework (build + output) where tile
               --counts come in as parameters

               IF p_log_type IS NOT NULL
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                         p_topo,
                                                         'GZ_VALIDATE_TOPOLOGY',
                                                         p_topo,
                                                         'Memory error (see message->) validating tile ' || i || '. Gonna subdivide this bad boy',
                                                         NULL,NULL,NULL,NULL,NULL,SQLERRM,
                                                         tiles(i));

               END IF;

               --try 25 (5x5) for now.  Desire is to do this rarely, but nail the problem when it happens
               subdivided_tiles := GZ_GEOM_UTILS.SUBDIVIDE_TILE(tiles(i),
                                                               25);

               FOR j IN 1 .. subdivided_tiles.COUNT
               LOOP


                  pxmin := tiles(j).sdo_ordinates(1) - p_delta;
                  pymin := tiles(j).sdo_ordinates(2) - p_delta;
                  pxmax := tiles(j).sdo_ordinates(3) + p_delta;
                  pymax := tiles(j).sdo_ordinates(4) + p_delta;

                  IF p_log_type IS NOT NULL
                  THEN

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                            p_topo,
                                                            'GZ_VALIDATE_TOPOLOGY',
                                                            p_topo,
                                                            'Validating tile ' || i || ' subdivided tile ' || j,
                                                            NULL,NULL,NULL,NULL,NULL,NULL,
                                                            subdivided_tiles(j));

                  END IF;

                  --allow to raise whatever, memory or invalid
                  validstr := GZ_TOPO_UTIL.VALIDATE_TOPOLOGY(p_topo,
                                                             NULL,   --edge count, dont even bother any more
                                                             NULL,   --default memory size will override
                                                             pxmin,
                                                             pymin,
                                                             pxmax,
                                                             pymax);


               END LOOP;

               --made it
               subdivided_tiles.DELETE;


            ELSE

               RAISE_APPLICATION_ERROR(-20001,'Validation failed: ' || SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;


         END;


         --should raise an error if theres a problem, this is just in case
         IF validstr != 'TRUE'
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Topology ' || p_topo || ' isnt valid ');

         END IF;

      END LOOP;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_VALIDATE_TOPOLOGY: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN 'TRUE';

   END GZ_VALIDATE_TOPOLOGY;

    ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION CLOSED_LOOPS (
      p_tablename       IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_edge_id         IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2
   AS

   --Matt! 3/12/10
   --Fix! for negative/reversed topo_ids 5/19/10
   --Bug workaround! Set "_OPTIMIZER_CONNECT_BY_COST_BASED" = false 5/20/10


   --Pass in a linear feature table and topology name
   --Returns 'Y' when the feature table contains only closed loops
   --Returns 'N edge_id:xxx' when not, xxx is any edge_id encountered
   --                        at either a T intersection or a gap

   --Caveat: I suppose some geographies (ex Incplaces) can't be modeled as closed loops

   --Assumption: We've got a standard topology edge type table
   --            containing edges, start nodes, and end nodes

   --Positive reinforcement: Repeat after me: "Carrying around buckets of edge
   --                        ids and nodes for any reasonable (sub million)
   --                        number of edges is a cakewalk for a computer"

   --Future: Could be modified to return some sort of linked list of edges

   --IN pseudocode
   --1. Grab an edge from table at a direction reversal point (attempt start to start) and call it the "root"
   --   Save the root edge id and nodes
   --   If there is any reversal point in a loop then there is always a start to start and
   --   an end to end.  We are choosing start to start as code start for no particular reason
   --   If there is no start to start, the loop loops entirely without a leaf and any edge works
   --2. Tunnel forward, connecting end to start, as far as possible
   --   Save edges and nodes along the way
   --3. Check that we have only one leaf along the way (the end)
   --4. Check that our end "leaf" is not actually the root, if yes, loop closed
   --5. Get the next edge, reversing connection type
   --6. Tunnel forward, connecting in reverse fashion
   --    Repeat from step 2
   --7. When we've closed that loop, find another edge not on the loop
   --   Start at step 2 for that loop
   --8. If weve traversed all edges successfully, Y


      tablename      VARCHAR2(32) := UPPER(p_tablename);
      topology       VARCHAR2(32) := UPPER(p_topology);
      psql           VARCHAR2(4000);
      psql2          VARCHAR2(4000);
      ouroboros      VARCHAR2(4000);
      wrapsql        VARCHAR2(4000);
      subquery       VARCHAR2(4000);
      edge_id        NUMBER;
      currentedgez   GZ_TYPES.stringarray;
      currentstartz  GZ_TYPES.stringarray;
      currentendz    GZ_TYPES.stringarray;
      leafz          GZ_TYPES.stringarray;
      kount          PLS_INTEGER;
      badedge        NUMBER;
      alledgez       GZ_TYPES.stringarray;
      allstartz      GZ_TYPES.stringarray;
      allendz        GZ_TYPES.stringarray;
      multiloopedgez GZ_TYPES.stringarray;
      singlestart    NUMBER;
      singleend      NUMBER;
      singleflag     PLS_INTEGER;
      infinitechek   PLS_INTEGER := 0;
      altersession   VARCHAR2(4000);


   BEGIN

      --Bug 6521934  May be patched in 11.2
      altersession := 'ALTER SESSION Set "_OPTIMIZER_CONNECT_BY_COST_BASED" = false';
      EXECUTE IMMEDIATE altersession;
      --store the text away so we can set it back at the various return points
      altersession := 'ALTER SESSION Set "_OPTIMIZER_CONNECT_BY_COST_BASED" = true';

      --store subquery
      subquery := 'SELECT e.edge_id, e.start_node_id, e.end_node_id '
               || 'FROM '
               || tablename || ' a, '
               || topology || '_RELATION$ r, '
               || topology || '_EDGE$ e '
               || 'WHERE '
               || 'r.tg_layer_id = a.topogeom.tg_layer_id AND '
               || 'r.tg_id = a.topogeom.tg_id AND '
               || 'e.edge_id = ABS(r.topo_id) ';  --ABS for negative topo ids
                                                  --Their original SDO direction
                                                  -- is reversed from edge$ direction


      IF p_edge_id IS NULL
      THEN

         ouroboros := 'SELECT y.edge_id FROM ( '
                   || subquery
                   || ') z, '
                   || '( '
                   || subquery
                   || ') y '
                   || 'WHERE z.start_node_id = y.start_node_id AND '
                   || 'z.edge_id != y.edge_id AND '
                   || 'rownum = 1 ';


         BEGIN

            --if this works, we have found a loop composed of at least
            --2 chains, at least one facing the other
            --    0--->0---><---0<---0
            EXECUTE IMMEDIATE ouroboros INTO edge_id;

         EXCEPTION
            WHEN NO_DATA_FOUND THEN

               --The laws I sketched on my notebook suggest that
               --There are no facing chains here, and any edge will work
               --Because it will Ouroboros on itself and we will find that munching point

               psql := 'SELECT edge_id '
                    || 'FROM  ('
                    || subquery || ') '
                    || 'WHERE '
                    || 'rownum = 1 ';
               EXECUTE IMMEDIATE psql INTO edge_id;

             WHEN OTHERS THEN
               RAISE;
         END;

      ELSE

         --This should only be used in debugging mode
         edge_id := p_edge_id;

      END IF;



      <<logoturtle>>

      --This code LOOP traverses a single physical loop

      LOOP

         --Loops with no guaranteed exit give me the fantods
         IF infinitechek > 100000
         THEN
            RAISE_APPLICATION_ERROR(-20001,'yo, we have looped this code ' || infinitechek || ' times!');
         ELSE
            infinitechek := infinitechek + 1;
         END IF;



         --First, check if our edge id is a looped edge with no other edges
         psql := 'SELECT a.start_node_id, a.end_node_id '
              || 'FROM ' || topology || '_EDGE$ a '
              || 'WHERE a.edge_id = :p1 ';
         EXECUTE IMMEDIATE psql INTO singlestart, singleend USING edge_id;


         IF singlestart = singleend
         THEN
            singleflag := 1;
         ELSE
            singleflag := 0;
         END IF;


         --tunnel as far as we can get
         --end2start (reading left to right)
         --  0--edge_id--->0---->0--->
         psql := 'SELECT q.edge_id, q.start_node_id, q.end_node_id, CONNECT_BY_ISLEAF leaf '
              || 'FROM ( '
              || subquery || ' '
              || ') q '
              || 'START WITH q.edge_id = :p1 '
              || 'CONNECT BY NOCYCLE PRIOR q.end_node_id = q.start_node_id ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO currentedgez,
                                                  currentstartz,
                                                  currentendz,
                                                  leafz USING edge_id;


         --check that only one of the leafs is a 1
         --more would indicate a "T" or "X" or more intersection

         psql2 := 'SELECT count(*) FROM '
               || 'TABLE(:p1) t '
               || 'WHERE t.column_value = :p2 ';

         EXECUTE IMMEDIATE psql2 INTO kount
                                 USING GZ_BUSINESS_UTILS.STRINGARRAY_TO_VARRAY(leafz),
                                 '1';  --varchar?

         IF kount > 1
         THEN

            --Get the first bad edge that comes back as a leaf and exit

            wrapsql := 'SELECT qq.edge_id FROM ( '
                     || psql || ' '
                     || ') qq '
                     || 'WHERE qq.leaf = :p1 '
                     || 'and rownum = 1 ';
            EXECUTE IMMEDIATE wrapsql INTO badedge USING 1;

                  psql := 'ALTER SESSION Set "_OPTIMIZER_CONNECT_BY_COST_BASED" = false';
      EXECUTE IMMEDIATE psql;

            EXECUTE IMMEDIATE altersession;
            RETURN 'N edge_id:' || badedge;

         END IF;

         IF singleflag = 1
         THEN
            --dont forget to add this edge to our bucket dummy
            alledgez := GZ_BUSINESS_UTILS.stringarray_add(alledgez,currentedgez);

            --we checked our single edge loop to make sure he connected to no one else
            --he is good
            EXIT;
         END IF;


         --stash what weve got
         alledgez := GZ_BUSINESS_UTILS.stringarray_add(alledgez,currentedgez);
         allstartz := GZ_BUSINESS_UTILS.stringarray_add(allstartz,currentstartz);
         allendz := GZ_BUSINESS_UTILS.stringarray_add(allendz,currentendz);
         --Delete the temps
         currentedgez.DELETE;
         currentstartz.DELETE;
         currentendz.DELETE;
         leafz.DELETE; --No need to stash

         --get the next edge
         --end2start means switch to end2end node in whereclause
         --  0---->0---->0--above--><--below--0
         psql := 'SELECT q.edge_id '
              || 'FROM ( '
              || subquery || ' '
              || ') q '
              || 'WHERE q.end_node_id = :p1 '  --reverse
              || 'AND q.edge_id != :p2 ';


         BEGIN

            EXECUTE IMMEDIATE psql INTO edge_id USING allendz(allendz.COUNT),
                                                      alledgez(alledgez.COUNT);
           EXCEPTION
            WHEN NO_DATA_FOUND THEN

              --We may have fully looped on this try
              psql := 'SELECT q.edge_id '
                   || 'FROM ( '
                   || subquery || ' '
                   || ') q '
                   || 'WHERE q.start_node_id = :p1 '  --reverse
                   || 'AND q.edge_id != :p2 ';

              --dbms_output.put_line(psql);
              --dbms_output.put_line(allendz(allendz.COUNT));
              --dbms_output.put_line(alledgez(alledgez.COUNT));

              BEGIN
                 EXECUTE IMMEDIATE psql INTO edge_id USING allendz(allendz.COUNT),
                                                           alledgez(alledgez.COUNT);

                 IF edge_id = alledgez(1)
                 THEN
                    -- We looped a loop, this is good
                    -- Exit the current loop and get a new edge, if applicable
                    EXIT;
                 ELSE
                    RAISE_APPLICATION_ERROR(-20001,'I, a computer, have no idea whats going on here');
                 END IF;

              EXCEPTION
                 WHEN NO_DATA_FOUND
                 THEN

                    RAISE_APPLICATION_ERROR(-20001,'Loop stuck! Check node ' || allendz(allendz.COUNT) );

              END;

            WHEN OTHERS THEN
               RAISE;
         END;

         --Check that this next edge_id isnt already in our list
         --We need to check more than just whether its the root (alledgez(1))
         --because a chain could link back on itself at a point other than the root
         --   if shaped like, for ex, a "P"

         psql := 'SELECT count(*) FROM '
              || 'TABLE(:p1) '
              || 'WHERE column_value = :p2 ';

         EXECUTE IMMEDIATE psql INTO kount USING
                                           GZ_BUSINESS_UTILS.stringarray_TO_VARRAY(alledgez),
                                           edge_id;

         IF kount = 1
         AND edge_id != alledgez(1)
         THEN
            EXECUTE IMMEDIATE altersession;
            RETURN 'N edge_id:' || edge_id;
         ELSIF kount = 1
         AND edge_id = alledgez(1)
         THEN
            -- We looped a loop, this is good
            -- Exit the current loop and get a new edge
            EXIT;
         ELSE
            --We are just partway thru a chain
            NULL;
         END IF;


         --Do we need to check for a looped single edge here?
         --Dont think so

         --tunnel as far as we can get
         --Now START2END (reading Left to Right)
         --  <--edge_id--0<---0<---<---0
         psql := 'SELECT q.edge_id, q.start_node_id, q.end_node_id, CONNECT_BY_ISLEAF leaf '
              || 'FROM ( '
              || subquery || ' '
              || ') q '
              || 'START WITH q.edge_id = :p1 '
              || 'CONNECT BY NOCYCLE PRIOR q.start_node_id = q.end_node_id ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO currentedgez,
                                                  currentstartz,
                                                  currentendz,
                                                  leafz USING edge_id;


         --check that only one of the leafs is a 1
         --more would indicate a "T" or "X" or more intersection

         psql2 := 'SELECT count(*) FROM '
               || 'TABLE(:p1) t '
               || 'WHERE t.column_value = :p2 ';

         EXECUTE IMMEDIATE psql2 INTO kount USING
                                            GZ_BUSINESS_UTILS.stringarray_TO_VARRAY(leafz),
                                            '1';  --varchar?


         IF kount > 1
         THEN

            --Get the first bad edge that comes back as a leaf and exit

            wrapsql := 'SELECT qq.edge_id FROM ( '
                     || psql || ' '
                     || ') qq '
                     || 'WHERE qq.leaf = :p1 '
                     || 'and rownum = 1 ';
            EXECUTE IMMEDIATE wrapsql INTO badedge USING 1;

            EXECUTE IMMEDIATE altersession;
            RETURN 'N edge_id:' || badedge;

         END IF;


         --stash what weve got
         alledgez := GZ_BUSINESS_UTILS.stringarray_add(alledgez,currentedgez);
         allstartz := GZ_BUSINESS_UTILS.stringarray_add(allstartz,currentstartz);
         allendz := GZ_BUSINESS_UTILS.stringarray_add(allendz,currentendz);
         --Delete the temp
         currentedgez.DELETE;
         currentstartz.DELETE;
         currentendz.DELETE;
         leafz.DELETE; --No need to stash


         --get the next edge
         --start2end means switch to start to start node in where
         --  <---0<---<--last_edge---00---->
         psql := 'SELECT q.edge_id '
              || 'FROM ( '
              || subquery || ' '
              || ') q '
              || 'WHERE q.start_node_id = :p1 '  --reverse
              || 'AND q.edge_id != :p2 ';

         BEGIN

            EXECUTE IMMEDIATE psql INTO edge_id USING allstartz(allstartz.COUNT),
                                                      alledgez(alledgez.COUNT);

            EXCEPTION
            WHEN NO_DATA_FOUND THEN

              --We may have fully looped on this try
              psql := 'SELECT q.edge_id '
                   || 'FROM ( '
                   || subquery || ' '
                   || ') q '
                   || 'WHERE q.end_node_id = :p1 '  --reverse
                   || 'AND q.edge_id != :p2 ';

                   --dbms_output.put_line(psql);
                   --dbms_output.put_line(allstartz(allstartz.COUNT));
                   --dbms_output.put_line(alledgez(alledgez.COUNT));

              BEGIN

                 EXECUTE IMMEDIATE psql INTO edge_id USING allstartz(allstartz.COUNT),
                                                           alledgez(alledgez.COUNT);
                 EXCEPTION
                 WHEN OTHERS THEN
                    RAISE_APPLICATION_ERROR(-20001,'Our loop is hitting a dead end at edge ' || alledgez(alledgez.COUNT));

              END;

              IF edge_id = alledgez(1)
              THEN
                 -- We looped a loop, this is good
                 -- Exit the current loop and get a new edge, if applicable
                 EXIT;
              ELSE
                 RAISE_APPLICATION_ERROR(-20001,'I, a computer, have no idea whats going on here');
              END IF;

            WHEN OTHERS THEN
               RAISE;
         END;

         --Check that this next edge_id isnt already in our list
         --We need to check more than just whether its the root (alledgez(1))
         --because a chain could link back on itself at a point other than the root
         --   if shaped like, for ex, a "P"

         psql := 'SELECT count(*) FROM '
              || 'TABLE(:p1) '
              || 'WHERE column_value = :p2 ';

         EXECUTE IMMEDIATE psql INTO kount USING
                                           GZ_BUSINESS_UTILS.stringarray_TO_VARRAY(alledgez),
                                           edge_id;

         IF kount = 1
         AND edge_id != alledgez(1)
         THEN
            EXECUTE IMMEDIATE altersession;
            RETURN 'N edge_id:' || edge_id;
         ELSIF kount = 1
         AND edge_id = alledgez(1)
         THEN
            -- We looped a loop, this is good
            -- Exit the current loop and get a new edge
            EXIT;
         ELSE
            --We are just partway thru a chain
            NULL;
         END IF;

         --Now we return to the start of the loop, and we are again on end2start

      END LOOP;



      --We've closed a physical loop
      --See if there are any other edges out there

      --add our loops worth of edges to the total stash
      multiloopedgez := GZ_BUSINESS_UTILS.stringarray_add(multiloopedgez,alledgez);
      alledgez.DELETE;

      --Attempt to get another edge id from the bucket
      --That is at a start-start faceoff

      psql := ouroboros --see beginning of fn
           || ' AND y.edge_id NOT IN (SELECT * FROM TABLE(:p1)) ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO edge_id
                                USING GZ_BUSINESS_UTILS.stringarray_TO_VARRAY(multiloopedgez);

         EXCEPTION
            WHEN NO_DATA_FOUND
            THEN

               --no start-starts left, check for an ouroboros
               psql := 'SELECT edge_id '
                    || 'FROM  (' || subquery || ') a '
                    || 'WHERE '
                    || 'a.edge_id NOT IN (SELECT * FROM TABLE(:p1)) '
                    || 'AND rownum = 1 ';

               BEGIN

                  --Are nested BEGINS with exceptions considered sloppy?

                  EXECUTE IMMEDIATE psql INTO edge_id
                                         USING GZ_BUSINESS_UTILS.stringarray_TO_VARRAY(multiloopedgez);

                  EXCEPTION
                     WHEN NO_DATA_FOUND
                     THEN
                        --Goody,we are done
                        EXECUTE IMMEDIATE altersession;
                        RETURN 'Y';
                     WHEN OTHERS
                     THEN
                        RAISE;
               END;


            WHEN OTHERS
            THEN
               RAISE;
      END;


      --We now have an edge_id that we believe exists on a loop or chain
      --   totally independent from the loop(s) we just closed

      --Keeping it real.  Second grade real
      GOTO logoturtle;

      --If we took computer science past second grade I guess
      --we would return the array of edges to a wrapping caller


   END CLOSED_LOOPS;

    ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   PROCEDURE IMPORT_SDOTOPO11G (
      p_topo            IN VARCHAR2,
      p_feature_tabs    IN VARCHAR2 DEFAULT NULL
   )
   AS

    --Matt! 3/30/11
    --Wrapper for Sivas ID_GEOM
    --Corrects 10g topogeom geometries imported into 11g to avoid
    --ORA-29532: Java call terminated by uncaught Java exception: java.lang.NumberFormatException: empty String
    --ORA-06512: at "MDSYS.SDO_TOPO_MAP", line 189
    --Call this after running standard import into 11g
    --Not clear to me (yet) if feature tables also have to be "touched" so its an option

    topo             VARCHAR2(4000) := UPPER(p_topo);
    psql             VARCHAR2(4000);
    kount            PLS_INTEGER;
    TYPE strarray    IS TABLE OF VARCHAR2(4000) INDEX BY PLS_INTEGER;
    feattabs         strarray;
    featcols         strarray;
    featidxs         strarray;

   BEGIN

      --checks

      psql := 'SELECT COUNT(*) FROM '
           || 'user_sdo_topo_info a '
           || 'WHERE a.topology = :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING topo;

      IF kount = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry buddy, cant find topology ' || topo || ' in this schema ');

      END IF;


      psql := 'SELECT COUNT(*) FROM '
           || 'user_tables a '
           || 'WHERE a.table_name IN (:p1,:p2,:p3) ';

      EXECUTE IMMEDIATE psql INTO kount USING topo || '_EDGE$',  --geometry
                                              topo || '_NODE$',  --geometry
                                              topo || '_FACE$';  --mbr_geometry

      IF kount != 3
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry buddy, cant find edge$ or node$ or face$ for topology ' || topo || ' in this schema ');

      END IF;

      psql := 'SELECT b.table_name, b.column_name, c.index_name '
           || 'FROM '
           || 'user_tab_columns b, '
           || 'user_indexes c '
           || 'WHERE '
           || 'b.data_type = :p1 AND '
           || 'c.index_type = :p2 AND '
           || 'b.table_name IN (:p3, :p4, :p5) AND '
           || 'b.table_name = c.table_name ';

      --Get feature tables too?
      IF p_feature_tabs IS NOT NULL
      THEN

         psql := psql || 'UNION '
              || 'SELECT a.table_name, b.column_name, c.index_name '
              || 'FROM '
              || 'user_sdo_topo_info a, '
              || 'user_tab_columns b, '
              || 'user_indexes c '
              || 'WHERE '
              || 'a.topology = :p6 AND '
              || 'b.data_type = :p7 AND '
              || 'c.index_type = :p8 AND '
              || 'a.table_name = b.table_name AND '
              || 'a.table_name = c.table_name ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO feattabs,
                                                  featcols,
                                                  featidxs USING 'SDO_GEOMETRY',
                                                                 'DOMAIN',
                                                                 topo || '_EDGE$',
                                                                 topo || '_FACE$',
                                                                 topo || '_NODE$',
                                                                 topo,
                                                                 'SDO_GEOMETRY',
                                                                 'DOMAIN';

      ELSE

         EXECUTE IMMEDIATE psql BULK COLLECT INTO feattabs,
                                                  featcols,
                                                  featidxs USING 'SDO_GEOMETRY',
                                                                 'DOMAIN',
                                                                 topo || '_EDGE$',
                                                                 topo || '_FACE$',
                                                                 topo || '_NODE$';


      END IF;


      FOR i in 1 .. feattabs.COUNT
      LOOP


         EXECUTE IMMEDIATE 'ALTER INDEX ' || featidxs(i) || ' UNUSABLE ';

         psql := 'UPDATE ' || feattabs(i) || ' a '
              || 'SET a.' || featcols(i) || ' = GZ_GEOM_UTILS.ID_GEOM(a.' || featcols(i) || ')';

         EXECUTE IMMEDIATE psql;
         COMMIT;

         EXECUTE IMMEDIATE 'ALTER INDEX ' || featidxs(i) || ' REBUILD ';


      END LOOP;


   END IMPORT_SDOTOPO11G;

    ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

   FUNCTION ADD_FAKE_FACE (
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER,
      p_dupe_geom       IN SDO_GEOMETRY,
      p_dupe_i          IN PLS_INTEGER,
      p_face_tab        IN VARCHAR2,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN NUMBER
   AS

      --Matt! 2/14
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
      psql := 'SELECT e.geometry '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO edge_geom USING p_edge_id;

      new_node_1 := p_dupe_geom;
      new_node_2 := p_dupe_geom;

      IF edge_geom.sdo_ordinates.EXISTS((p_dupe_i * 2) + 4)
      THEN

         --go two vertexes over for node1

         --geom will be ignored
         new_node_1.sdo_point.X :=  edge_geom.sdo_ordinates((p_dupe_i * 2) + 3);
         new_node_1.sdo_point.Y :=  edge_geom.sdo_ordinates((p_dupe_i * 2) + 4);

         new_node_1_id := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                                p_edge_id,
                                                new_node_1,
                                                p_dupe_i + 1,  --two coord indexes, +1 how Im counting vtxs
                                                'FALSE');



      ELSE

         --just get the end node
         psql := 'SELECT e.end_node_id '
              || 'FROM '
              || p_topo || '_edge$ e '
              || 'WHERE e.edge_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO new_node_1_id USING p_edge_id;

          psql := 'SELECT n.geometry '
              || 'FROM '
              || p_topo || '_node$ n '
              || 'WHERE n.node_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO new_node_1 USING new_node_1_id;

      END IF;


      IF edge_geom.sdo_ordinates.EXISTS((p_dupe_i * 2) - 5)
      THEN

         --go two vertexes in the opposite direction for number 2

         --geom will be ignored
         new_node_2.sdo_point.X :=  edge_geom.sdo_ordinates((p_dupe_i * 2) - 5);
         new_node_2.sdo_point.Y :=  edge_geom.sdo_ordinates((p_dupe_i * 2) - 4);

         new_node_2_id := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                                p_edge_id,
                                                new_node_2,
                                                p_dupe_i - 3, --two coord indexes, -3 how Im counting vtxs
                                                'FALSE');



      ELSE

         --just get the end node
         psql := 'SELECT e.start_node_id '
              || 'FROM '
              || p_topo || '_edge$ e '
              || 'WHERE e.edge_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO new_node_2_id USING p_edge_id;

         psql := 'SELECT n.geometry '
              || 'FROM '
              || p_topo || '_node$ n '
              || 'WHERE n.node_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO new_node_2 USING new_node_2_id;

      END IF;


      --make up a new point out in space
      --amateur-style

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


      new_edge_geom := edge_geom;
      new_ordinates.EXTEND(6);
      new_ordinates(1) := new_node_1.sdo_point.X;
      new_ordinates(2) := new_node_1.sdo_point.Y;
      new_ordinates(3) := new_pt_geom.sdo_point.X;
      new_ordinates(4) := new_pt_geom.sdo_point.Y;
      new_ordinates(5) := new_node_2.sdo_point.X;
      new_ordinates(6) := new_node_2.sdo_point.Y;

      new_edge_geom.sdo_ordinates := new_ordinates;

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

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------


   FUNCTION GET_TEMP_EDGE(
      p_topo            IN VARCHAR2,
      p_dupepoint_geom  IN SDO_GEOMETRY,
      p_edge_id         IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 2/14/11

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
   ---------------------------------------------------------------------------------

   PROCEDURE REMOVE_FAKE_FACE (
      p_topo            IN VARCHAR2,
      p_face_feat_tab   IN VARCHAR2
   )
   AS

      --Matt! 2/14/11

      psql        VARCHAR2(4000);
      face_id     NUMBER;
      edje        NUMBER;
      nodes       GZ_TYPES.stringarray;
      nodekounts  GZ_TYPES.stringarray;
      kount       PLS_INTEGER;

   BEGIN


      psql := 'SELECT a.face_id FROM ' || p_topo || '_face$ a '
           || 'WHERE a.face_id != :p1 '
           || 'MINUS '
           || 'select b.face_id FROM ' || p_face_feat_tab || '  b ';

      EXECUTE IMMEDIATE psql INTO face_id USING -1;


      psql := 'SELECT e.edge_id FROM '
           || p_topo || '_edge$ e '
           || 'WHERE (e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
           || '(e.left_face_id = :p3 OR e.right_face_id = :p4) ';

      EXECUTE IMMEDIATE psql INTO edje USING face_id, face_id,
                                             -1, -1;

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

      END LOOP;


   END REMOVE_FAKE_FACE;

END GZ_TOPO_UTIL;
/

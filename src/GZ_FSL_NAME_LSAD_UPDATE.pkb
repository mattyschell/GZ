CREATE OR REPLACE PACKAGE BODY GZ_FSL_NAME_LSAD_UPDATE AS
/******************************************************************************
   NAME:       FSL_Name_Lsad_Update
   PURPOSE:     Update FSL tables' NAME and LSAD columns
   Author :     Al Freeman

   REVISIONS:
   Ver        Date         Description
   ---------  ----------   ------------------------------------
   1.0        3/31/2010             1. Created this package.
******************************************************************************/
PROCEDURE update_summary_levels (Topology VARCHAR2, Deploy VARCHAR2) AS
    --Update FSL tables NAME and LSAD columns

Begin

    --Get the topology being processed
    sqlstmt := 'select distinct topology
                from user_sdo_topo_info
                where topology  = :1';
    EXECUTE IMMEDIATE sqlstmt into g_topology using Topology;
    
    --Set schema name
    GenSchema := Deploy;

    fsl140_update(GenSchema);
    fsl150_update(GenSchema);
    fsl155_update(GenSchema);
    fsl250_update(GenSchema);
    fsl251_update(GenSchema);
    fsl252_update(GenSchema);
    fsl254_update(GenSchema);
    fsl280_update(GenSchema);
    fsl283_update(GenSchema);
    fsl310_update(GenSchema);
    fsl314_update(GenSchema);
    fsl320_update(GenSchema);
    fsl323_update(GenSchema);
    fsl330_update(GenSchema);
    fsl332_update(GenSchema);
    fsl335_update(GenSchema);
    fsl337_update(GenSchema);
    fsl340_update(GenSchema);
    fsl345_update(GenSchema);
    fsl350_update(GenSchema);
    fsl355_update(GenSchema);
    fsl360_update(GenSchema);
    fsl364_update(GenSchema);
    fsl400_update(GenSchema);
    fsl420_update(GenSchema);
    fsl500_update(GenSchema);
    fsl550_update(GenSchema);
    fsl610_update(GenSchema);
    fsl620_update(GenSchema);
    fsl700_update(GenSchema);
    fsl871_update(GenSchema);
    fsl950_update(GenSchema);
    fsl960_update(GenSchema);
    fsl970_update(GenSchema);
    
    DBMS_OUTPUT.put_line('Name and Lsad Update Complete: ' ||topology);

END update_summary_levels;

FUNCTION find_topology_table(GenSchema IN VARCHAR2, Current_table IN VARCHAR2)
RETURN NUMBER AS

    v_table_exists                               NUMBER;
    BEGIN

    --Verify that the table to be updated exists in the topology
    sqlstmt := 'select count(*)
                    from user_tables
                    where table_name =  '''||g_table||'''';
    --DBMS_OUTPUT.put_line(sqlstmt);
    EXECUTE IMMEDIATE sqlstmt into  v_table_exists;
    if  v_table_exists  > 0 then
        Return 1; -- table to be updated does exist in the topology
    else
         Return 0; --the table does not exist in this topology; do not update this table
   end if;
END find_topology_table;
FUNCTION check_4_name_column(GenSchema  IN  VARCHAR2, Current_table  IN VARCHAR2)
RETURN NUMBER AS
    v_cntr          NUMBER;
    BEGIN
        --Check for the name column in the current FSL table being processed
       sqlstmt := ' Select count(*)
                        from user_tab_cols
                        where table_name = '''||g_table||'''
                           and  column_name = ''NAME''';
     --DBMS_OUTPUT.put_line(sqlstmt);
    EXECUTE IMMEDIATE sqlstmt INTO v_cntr;
    if  v_cntr > 0 then  --the current table description includes the 'NAME' column
       Return  1;  --the current table does not need to be altered
   else
        Return 0; --alter the current table by adding the 'NAME' column.
    end if;
END check_4_name_column;
PROCEDURE fsl140_update(GenSchema   VARCHAR2) AS
      l_table_status NUMBER;
      l_table_exists NUMBER;
      BEGIN
    --check for name column
    g_table := ''||g_topology||'_'||'FSL140';
    l_table_exists := find_topology_table(GenSchema,g_table);
    if  l_table_exists = 1 then
        l_table_status :=  check_4_name_column(GenSchema,g_table);
       --DBMS_OUTPUT.put_line(l_table_status);
       --check the returned status from the function
       if  l_table_status = 0 then
           sqlstmt := 'alter table '||g_table||' add
                           (name     varchar2(90))';
          --DBMS_OUTPUT.put_line(sqlstmt);
          EXECUTE IMMEDIATE sqlstmt;
       end if;
           sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl140  a
                       set a.lsad = ''''
                       where  tractce = ''000000''';
           --DBMS_OUTPUT.put_line(sqlstmt);
           EXECUTE IMMEDIATE sqlstmt;
           COMMIT;
    else
         null;
    end if;
    END fsl140_update;
PROCEDURE fsl150_update(GenSchema   VARCHAR2) AS
      l_table_status NUMBER;
      l_table_exists NUMBER;
    BEGIN
    --check for name column
    g_table := ''||g_topology||'_'||'FSL150';
    l_table_exists := find_topology_table(GenSchema,g_table);
    if  l_table_exists = 1 then

       l_table_status :=  check_4_name_column(GenSchema,g_table);
       --DBMS_OUTPUT.put_line(l_table_status);
       --check the returned status from the function
       if  l_table_status = 0 then
           sqlstmt := 'alter table '||g_table||' add
                           (name     varchar2(90))';
          --DBMS_OUTPUT.put_line(sqlstmt);
          EXECUTE IMMEDIATE sqlstmt;
       end if;
       --    sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl150 a
       --                set a.lsad = ''''
       --                where  blkgrpce = ''0''';
           --DBMS_OUTPUT.put_line(sqlstmt);
       --    EXECUTE IMMEDIATE sqlstmt;
           COMMIT;
    else
         null;
    end if;

    END fsl150_update;
PROCEDURE fsl155_update(GenSchema   VARCHAR2) AS
      l_table_status NUMBER;
      l_table_exists NUMBER;

    BEGIN
    --check for name column
    g_table := ''||g_topology||'_'||'FSL155';
    l_table_exists := find_topology_table(GenSchema,g_table);
    if  l_table_exists = 1 then

       l_table_status :=  check_4_name_column(GenSchema,g_table);
       --DBMS_OUTPUT.put_line(l_table_status);
       --check the returned status from the function
       if  l_table_status = 0 then
           sqlstmt := 'alter table '||g_table||' add
                           (name     varchar2(90))';
          --DBMS_OUTPUT.put_line(sqlstmt);
          EXECUTE IMMEDIATE sqlstmt;
       end if;
            --update name
           sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl155  a
                       set a.name = ''''';
           --DBMS_OUTPUT.put_line(sqlstmt);
           EXECUTE IMMEDIATE sqlstmt;
           sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl155  a
                       set a.lsad = ''''';
           --DBMS_OUTPUT.put_line(sqlstmt);
           EXECUTE IMMEDIATE sqlstmt;
           COMMIT;
    else
         null;
   end if;

    END fsl155_update;
PROCEDURE fsl250_update(pGenSchema VARCHAR2) AS
      l_table_status NUMBER;
      l_table_exists NUMBER;
    BEGIN
    --check for name column
    g_table := ''||g_topology||'_'||'FSL250';
    l_table_exists := find_topology_table(GenSchema,g_table);
    if  l_table_exists = 1 then

       l_table_status :=  check_4_name_column(GenSchema,g_table);
       --DBMS_OUTPUT.put_line(l_table_status);
       --check the returned status from the function
       if  l_table_status = 0 then
           sqlstmt := 'alter table '||g_table||' add
                           (name     varchar2(90))';
          --DBMS_OUTPUT.put_line(sqlstmt);
          EXECUTE IMMEDIATE sqlstmt;
       end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl250
                        set name = name||'' (state)''
                        where aiannhfsr = '||''''||g_state_tribe||'''
                              and  (aiannhfsr = '||''''||g_state_tribe||''' and name not like ''%(state)%'')';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
   end if;

    END fsl250_update;
PROCEDURE fsl251_update(pGenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;
    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL251';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            --update name
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl251 a
                        set a.name = ''Remainder of ''||(select b.name
                                                         from '||GenSchema||'.'||g_topology||'_'||'fsl250 b
                                                         where a.aiannhce = b.aiannhce)
                       where tribalsubce = ''999''';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl251 a
                        set a.lsad = '||'(select b.lsad
                                         from '||GenSchema||'.'||g_topology||'_'||'fsl250 b
                                         where b.aiannhce = a.aiannhce)
                        where tribalsubce = ''999''';
             --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
           -- sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl251 a
           --             set a.tribalsubce = ''999''
           --             where tribalsubce = ''000''';
            --DBMS_OUTPUT.put_line(sqlstmt);
           -- EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
   end if;

    END fsl251_update;
PROCEDURE fsl252_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL252';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl252 a
                        set a.name = '||'(select b.name
                                        from   '||GenSchema||'.'||g_topology||'_'||'fsl250 b
                                        where  a.aiannhce = b.aiannhce
                                          and  b.aiannhfsr in( '||''''||g_state_tribe||''''||','||''''||g_federal_tribe||''''||'))';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
   end if;

    END fsl252_update;
PROCEDURE fsl254_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL254';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl254 a
                        set a.name = '||'(select b.name
                                        from   '||GenSchema||'.'||g_topology||'_'||'fsl250 b
                                        where  a.aiannhce = b.aiannhce)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl254 a
                        set a.name = ''Fallon Paiute-Shoshone (Colony)''
                        where a.aiannhce = ''1070''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl254 a
                        set a.name = ''Fallon Paiute-Shoshone (Reservation)''
                        where a.aiannhce = ''1075''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
   end if;

    END fsl254_update;
PROCEDURE fsl280_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL280';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
           --update name
           /** sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl280 a
                       set a.name = '||'(select b.name
                                         from   '||GenSchema||'.'||g_topology||'_'||'fsl250 b
                                         where  a.aiannhce = b.aiannhce
                                           and  b.aiannhfsr in( '||''''||g_state_tribe||''''||','||''''||g_federal_tribe||''''||'))';
                   --DBMS_OUTPUT.put_line(sqlstmt);
                   EXECUTE IMMEDIATE sqlstmt;
                   sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl280 a
                       set a.lsad = '||'(select b.lsad
                                         from '||GenSchema||'.'||g_topology||'_'||'fsl250 b
                                         where  a.aiannhce = b.aiannhce
                                           and  b.aiannhfsr in( '||''''||g_state_tribe||''''||','||''''||g_federal_tribe||''''||'))';
                   --DBMS_OUTPUT.put_line(sqlstmt);
                   EXECUTE IMMEDIATE sqlstmt;*/

           sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl280  a
                       set a.name = ''''';
           --DBMS_OUTPUT.put_line(sqlstmt);
           EXECUTE IMMEDIATE sqlstmt;
           sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl280  a
                       set a.lsad = ''''';
           --DBMS_OUTPUT.put_line(sqlstmt);
           EXECUTE IMMEDIATE sqlstmt;
           COMMIT;
    else
         null;
  end if;

    END fsl280_update;
PROCEDURE fsl283_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL283';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
           --update name
           /** sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl283 a
                       set a.name = '||'(select b.name
                                         from   '||GenSchema||'.'||g_topology||'_'||'fsl250 b
                                         where  a.aiannhce = b.aiannhce
                                           and  b.aiannhfsr in( '||''''||g_state_tribe||''''||','||''''||g_federal_tribe||''''||'))';
                   --DBMS_OUTPUT.put_line(sqlstmt);
                   EXECUTE IMMEDIATE sqlstmt;
                   sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl283 a
                       set a.lsad = '||'(select b.lsad
                                         from '||GenSchema||'.'||g_topology||'_'||'fsl250 b
                                         where  a.aiannhce = b.aiannhce
                                           and  b.aiannhfsr in( '||''''||g_state_tribe||''''||','||''''||g_federal_tribe||''''||'))';
                   --DBMS_OUTPUT.put_line(sqlstmt);
                   EXECUTE IMMEDIATE sqlstmt; */

           sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl283  a
                       set a.name = ''''';
           --DBMS_OUTPUT.put_line(sqlstmt);
           EXECUTE IMMEDIATE sqlstmt;
           sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl283  a
                       set a.lsad = ''''';
           --DBMS_OUTPUT.put_line(sqlstmt);
           EXECUTE IMMEDIATE sqlstmt;
           COMMIT;
    else
         null;
   end if;

    END fsl283_update;
PROCEDURE fsl310_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL310';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            --Using regular expression to eliminate State Postal abbreviations
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl310
                        set name = regexp_replace(name,''^([^,]*),*([^,]*)$'',''\1'')';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl310
                        set name = ''Texarkana-Texarkana''
                        where cbsafp = ''45500''';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
   end if;

    END fsl310_update;
PROCEDURE fsl314_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
    l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL314';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            --Using regular expression to eliminate State Postal abbreviations
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl314
                        set name = regexp_replace(name,''^([^,]*),*([^,]*)$'',''\1'')';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
   end if;

    END fsl314_update;
PROCEDURE fsl320_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL320';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl320 a
                        set a.name = '||'(select b.name
                                          from   '||GenSchema||'.'||g_topology||'_'||'fsl310 b
                                          where  a.cbsafp = b.cbsafp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl320 a
                        set a.lsad = '||'(select b.lsad
                                          from '||GenSchema||'.'||g_topology||'_'||'fsl310 b
                                          where  a.cbsafp = b.cbsafp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl320_update;
PROCEDURE fsl323_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL323';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl323 a
                        set a.name = '||'(select b.name
                                          from   '||GenSchema||'.'||g_topology||'_'||'fsl314 b
                                          where  a.metdivfp = b.metdivfp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl323 a
                        set a.lsad = '||'(select b.lsad
                                          from '||GenSchema||'.'||g_topology||'_'||'fsl314 b
                                          where  a.metdivfp = b.metdivfp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl323_update;
PROCEDURE fsl330_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL330';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            --Using regular expression to eliminate State Postal abbreviations
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl330
                        set name = regexp_replace(name,''^([^,]*),*([^,]*)$'',''\1'')';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
   end if;
    END fsl330_update;
PROCEDURE fsl332_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL332';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            --Using regular expression to eliminate State Postal abbreviations
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl332
                        set name = regexp_replace(name,''^([^,]*),*([^,]*)$'',''\1'')';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl332_update;
PROCEDURE fsl335_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL335';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            --Using regular expression to eliminate State Postal abbreviations
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl335
                        set name = regexp_replace(name,''^([^,]*),*([^,]*)$'',''\1'')';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl335_update;
PROCEDURE fsl337_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL337';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            --Using regular expression to eliminate State Postal abbreviations
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl337
                        set name = regexp_replace(name,''^([^,]*),*([^,]*)$'',''\1'')';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl337_update;
PROCEDURE fsl340_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL340';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl340 a
                        set a.name = '||'(select b.name
                                          from   '||GenSchema||'.'||g_topology||'_'||'fsl330 b
                                          where  a.csafp = b.csafp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl340 a
                        set a.lsad = '||'(select b.lsad
                                          from '||GenSchema||'.'||g_topology||'_'||'fsl330 b
                                          where  a.csafp = b.csafp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl340_update;
PROCEDURE fsl345_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;
    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL345';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl345 a
                        set a.name = '||'(select b.name
                                          from   '||GenSchema||'.'||g_topology||'_'||'fsl335 b
                                          where  a.cnectafp = b.cnectafp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl345 a
                        set a.lsad = '||'(select b.lsad
                                          from '||GenSchema||'.'||g_topology||'_'||'fsl335 b
                                          where  a.cnectafp = b.cnectafp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
   end if;
    END fsl345_update;
PROCEDURE fsl350_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
    l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL350';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            --Using regular expression to eliminate State Postal abbreviations
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl350
                        set name = regexp_replace(name,''^([^,]*),*([^,]*)$'',''\1'')';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
   end if;
    END fsl350_update;
PROCEDURE fsl355_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL355';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            --Using regular expression to eliminate State Postal abbreviations
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl355
                        set name = regexp_replace(name,''^([^,]*),*([^,]*)$'',''\1'')';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl355_update;
PROCEDURE fsl360_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL360';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl360 a
                        set a.name = '||'(select b.name
                                          from   '||GenSchema||'.'||g_topology||'_'||'fsl350 b
                                          where  a.nectafp = b.nectafp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl360 a
                        set a.lsad = '||'(select b.lsad
                                          from '||GenSchema||'.'||g_topology||'_'||'fsl350 b
                                          where  a.nectafp = b.nectafp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
   end if;
    END fsl360_update;
PROCEDURE fsl364_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL364';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl364 a
                        set a.name = '||'(select b.name
                                          from   '||GenSchema||'.'||g_topology||'_'||'fsl355 b
                                          where  a.nectadivfp = b.nectadivfp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl364 a
                        set a.lsad = '||'(select b.lsad
                                          from '||GenSchema||'.'||g_topology||'_'||'fsl355 b
                                          where  a.nectadivfp = b.nectadivfp)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl364_update;
PROCEDURE fsl400_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL400';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            --Using regular expression to eliminate State Postal abbreviations
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl400
                        set name = regexp_replace(name,''^([^,]*),*([^,]*)$'',''\1'')';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl400
                        set name = ''Sayre--Waverly''
                        where uace = ''79903''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl400
                        set name = ''Point Pleasant--Gallipolis''
                        where uace = ''70507''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl400
                        set name = ''Bristol--Bristol''
                        where uace = ''10351''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl400
                        set name = ''Weirton--Steubenville''
                        where uace = ''93592''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl400
                        set name = ''Islamorada, Village of Islands''
                        where uace = ''41860''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl400
                        set name = ''Texarkana--Texarkana''
                        where uace = ''87193''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl400_update;
PROCEDURE fsl420_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL420';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
             -- DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl420 a
                        set a.name = '||'(select b.name
                                          from   '||GenSchema||'.'||g_topology||'_'||'fsl400 b
                                          where  a.uace = b.uace)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl420 a
                        set a.lsad = '||'(select b.lsad
                                          from '||GenSchema||'.'||g_topology||'_'||'fsl400 b
                                          where  a.uace = b.uace)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl420_update;
PROCEDURE fsl500_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL500';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl500
                        set  name = ''''
                        where cdfp = ''00'' or cdfp = ''98'' or cdfp = ''99''';
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl500
                        set name = LTRIM(name,''0'')
                        WHERE cdfp between ''01'' and ''53''';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl500_update;
PROCEDURE fsl550_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL550';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl550 a
                        set a.name = '||'(select b.name
                                        from   '||GenSchema||'.'||g_topology||'_'||'fsl250 b
                                        where  a.aiannhce = b.aiannhce
                                          and  b.aiannhfsr in( '||''''||g_state_tribe||''''||','||''''||g_federal_tribe||''''||'))';
           --DBMS_OUTPUT.put_line(sqlstmt);
           EXECUTE IMMEDIATE sqlstmt;
           sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl550 a
                       set a.lsad = '||'(select b.lsad
                                         from '||GenSchema||'.'||g_topology||'_'||'fsl250 b
                                         where  a.aiannhce = b.aiannhce
                                           and  b.aiannhfsr in( '||''''||g_state_tribe||''''||','||''''||g_federal_tribe||''''||'))';
           --DBMS_OUTPUT.put_line(sqlstmt);
           EXECUTE IMMEDIATE sqlstmt;
           COMMIT;
    else
         null;
    end if;
    END fsl550_update;
PROCEDURE fsl610_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL610';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl610
                        set name = LTRIM(name,''0'')
                        where sldust != ''ZZZ''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl610
                        set name = ''State Senate Districts not defined''
                        WHERE sldust = ''ZZZ''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl610_update;
PROCEDURE fsl620_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL620';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl620
                        set name = LTRIM(name,''0'')
                        where sldlst != ''ZZZ''';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl620
                        set name = ''State House Districts not defined''
                        WHERE sldlst = ''ZZZ''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl620_update;
PROCEDURE fsl700_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL700';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl700
                        set name = ''Voting Districts not defined''
                         where vtdst = ''      ''
                            and statefp in(''30'',''41'',''44'',''21'')';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl700_update;
PROCEDURE fsl871_update(GenSchema   VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;
    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL871';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            --update name
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl871 a
                        set a.name = '||'(select b.name
                                         from   '||GenSchema||'.'||g_topology||'_'||'fsl860 b
                                         where  a.zcta5ce = b.zcta5ce)';
            --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl871 a
                        set a.lsad = '||'(select b.lsad
                                         from   '||GenSchema||'.'||g_topology||'_'||'fsl860 b
                                         where  a.zcta5ce = b.zcta5ce)';
             --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
   end if;

    END fsl871_update;
PROCEDURE fsl950_update(GenSchema  VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL950';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl950 a
                        set name = ''Remainder of ''||(select b.name
                                                        from '||GenSchema||'.'||g_topology||'_'||'fsl040 b
                                                        where a.statefp = b.statefp),
                           lsad = ''00''
                       where sddoe_e = ''99999''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl950_update;
PROCEDURE fsl960_update(GenSchema VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL960';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl960 a
                        set name = ''Remainder of ''||(select b.name
                                                        from '||GenSchema||'.'||g_topology||'_'||'fsl040 b
                                                        where a.statefp = b.statefp),
                           lsad = ''00''
                       where sddoe_s = ''99999''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl960_update;
PROCEDURE fsl970_update(GenSchema  VARCHAR2) AS
     l_table_status NUMBER;
     l_table_exists NUMBER;

    BEGIN
        --check for name column
        g_table := ''||g_topology||'_'||'FSL970';
        l_table_exists := find_topology_table(GenSchema,g_table);
        if  l_table_exists = 1 then

           l_table_status :=  check_4_name_column(GenSchema,g_table);
           --DBMS_OUTPUT.put_line(l_table_status);
           --check the returned status from the function
           if  l_table_status = 0 then
               sqlstmt := 'alter table '||g_table||' add
                               (name     varchar2(90))';
              --DBMS_OUTPUT.put_line(sqlstmt);
              EXECUTE IMMEDIATE sqlstmt;
           end if;
            sqlstmt := 'update '||GenSchema||'.'||g_topology||'_'||'fsl970 a
                        set name = ''Remainder of ''||(select b.name
                                                        from '||GenSchema||'.'||g_topology||'_'||'fsl040 b
                                                        where a.statefp = b.statefp),
                           lsad = ''00''
                       where sddoe_u = ''99999''';
           --DBMS_OUTPUT.put_line(sqlstmt);
            EXECUTE IMMEDIATE sqlstmt;
            COMMIT;
    else
         null;
    end if;
    END fsl970_update;
END GZ_FSL_NAME_LSAD_UPDATE;
/


CREATE OR REPLACE PACKAGE BODY GZ_TOPO_HELPER AS
/*
--*******************************************************************************
--* NAME: TOPO_UTIL 
--*   PURPOSE:    To contain all Generalization Topological Methods
--*
--*   REVISIONS:
--*   Ver        Date        Author           Description
--*   ---------  ----------  ---------------  ------------------------------------
--*   1.0        12/15/2009  Salman Mansoor   1. Created this package body.
--*******************************************************************************
*/
PROCEDURE add_geoid_column_to_table(pInTable VARCHAR2,pNewColumnName VARCHAR2 DEFAULT 'GEO_ID') AS
   vIntable         VARCHAR2(30)         := UPPER(SUBSTR(pInTable,1,30));
   vNewColumnName   VARCHAR2(30)         := UPPER(SUBSTR(pNewColumnName,1,30));
   vFslName         VARCHAR2(30)         := UPPER(Substr(vIntable,Instr(vIntable,'_') + 1,6));
   sql_stmt         VARCHAR2(4000);
   TmpCount         NUMBER;
   ContinueStatus   BOOLEAN              := TRUE;
BEGIN
   -----------------------------------------------------------------------------
   -- Do some validation
   sql_stmt := 'SELECT count(*) FROM user_tables WHERE table_name = :1';
   EXECUTE IMMEDIATE sql_stmt INTO TmpCount USING vInTable;
   IF (TmpCount < 1) THEN
      ContinueStatus := FALSE;
      dbms_output.put_line('pInTable does not exist in current schema. Aborting now...');
   END IF;
   --------
   sql_stmt := 'SELECT count(*) FROM user_tab_columns WHERE table_name = :1 AND column_name = :2';
   EXECUTE IMMEDIATE sql_stmt INTO TmpCount USING vInTable,vNewColumnName;
   IF (TmpCount = 1) THEN
      sql_stmt := 'ALTER TABLE '||vInTable||' DROP COLUMN '||vNewColumnName;
      EXECUTE IMMEDIATE sql_stmt;
   END IF;   
   -----------------------------------------------------------------------------
   IF ContinueStatus THEN
      --------------------------------------------------------------------------
      -- Add a new column to the table
      sql_stmt := 'ALTER TABLE '||vInTable||' ADD ('||vNewColumnName||' VARCHAR2(60))';
      EXECUTE IMMEDIATE sql_stmt;
      --------------------------------------------------------------------------
      CASE vFslName
         WHEN 'FSL010' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''0100000US''';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL020' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''0200000US''||t.regionce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL030' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''0300000US''||t.divisionce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;  
         WHEN 'FSL040' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''0400000US''||t.statefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL050' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''0500000US''||t.statefp||''''||t.countyfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;         
         WHEN 'FSL060' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''0600000US''||t.statefp||''''||t.countyfp||''''||t.cousubfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL061' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''0610000US''||t.statefp||''''||t.countyfp||''''||t.cousubfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL067' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''0670000US''||t.statefp||''''||t.countyfp||''''||t.cousubfp||''''||t.submcdfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL06V' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''06V0000US''||t.statefp||''''||t.countyfp||''''||t.submcdfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL070' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''0700000US''||t.statefp||''''||t.countyfp||''''||t.cousubfp||''''||t.placefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;  
         WHEN 'FSL071' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''0710000US''||t.statefp||''''||t.countyfp||''''||t.cousubfp||''''||t.euplace';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT; 
         WHEN 'FSL080' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''0800000US''||t.statefp||''''||t.countyfp||''''||t.cousubfp||''''||t.placefp||''''||t.tractce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;  
         WHEN 'FSL140' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''1400000US''||t.statefp||''''||t.countyfp||''''||t.tractce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL150' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''1500000US''||t.statefp||''''||t.countyfp||''''||t.tractce||''''||t.blkgrpce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL155' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''1550000US''||t.statefp||''''||t.placefp||''''||t.countyfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT; 
         WHEN 'FSL157' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''1570000US''||t.statefp||''''||t.countyfp||''''||t.euplace';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL160' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''1600000US''||t.statefp||''''||t.placefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL162' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''1620000US''||t.statefp||''''||t.placefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL170' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''1700000US''||t.statefp||''''||t.concityfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL172' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''1720000US''||t.statefp||''''||t.concityfp||''''||t.placefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL230' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2300000US''||t.statefp||''''||t.anrcfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL250' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2500000US''||t.aiannhce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL251' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2510000US''||t.aiannhce||''''||t.tribalsubce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL252' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2520000US''||t.aiannhce||''R''';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL254' THEN
            -- Deal with all Trust Lands
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2540000US''||t.aiannhce||''T'''; --WHERE t.lsad <> ''78''';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
            -- Deal with Hawaiian Homelands (LSAD = 78)
            --sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''25400US''||t.aiannhce||''H'' WHERE t.lsad = ''78''';
            --EXECUTE IMMEDIATE sql_stmt;
            --COMMIT;
         WHEN 'FSL256' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2560000US''||t.aiannhce||''''||t.ttractce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL258' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2580000US''||t.aiannhce||''''||t.ttractce||''''||t.tblkgrpce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL260' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2600000US''||t.aiannhce||''''||t.statefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL270' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2700000US''||t.aiannhce||''''||t.statefp||''''||t.countyfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL280' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2800000US''||t.statefp||''''||t.aiannhce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL281' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2810000US''||t.statefp||''''||t.aiannhce||''''||t.tribalsubce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL283' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2830000US''||t.statefp||''''||t.aiannhce||''R''';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL286' THEN
            -- Deal with all Trust Lands
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''2860000US''||t.statefp||''''||t.aiannhce||''T'''; --WHERE t.artli = ''T''';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
            -- Deal with Hawaiian Homelands (LSAD = 78)
            --sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''28600US''||t.statefp||''''||t.aiannhce||''H'' WHERE t.artli = ''H''';
            --EXECUTE IMMEDIATE sql_stmt;
            --COMMIT;
         WHEN 'FSL310' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''310M100US''||t.cbsafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL311' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''311M100US''||t.cbsafp||''''||t.statefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL312' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''312M100US''||t.cbsafp||''''||t.statefp||''''||t.placefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL314' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''314M100US''||t.cbsafp||''''||t.metdivfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL315' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''315M100US''||t.cbsafp||''''||t.metdivfp||''''||t.statefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL320' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''320M100US''||t.statefp||''''||t.cbsafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL321' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''321M100US''||t.statefp||''''||t.cbsafp||''''||t.placefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL323' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''323M100US''||t.statefp||''''||t.cbsafp||''''||t.metdivfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL330' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''330M100US''||t.csafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL331' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''331M100US''||t.csafp||''''||t.statefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL332' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''332M100US''||t.csafp||''''||t.cbsafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL333' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''333M100US''||t.csafp||''''||t.cbsafp||''''||t.statefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL335' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''335M100US''||t.cnectafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL336' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''336M100US''||t.cnectafp||''''||t.statefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL337' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''337M100US''||t.cnectafp||''''||t.nectafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL338' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''338M100US''||t.cnectafp||''''||t.nectafp||''''||t.statefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL340' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''340M100US''||t.statefp||''''||t.csafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL341' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''341M100US''||t.statefp||''''||t.csafp||''''||t.cbsafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL345' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''345M100US''||t.statefp||''''||t.cnectafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL346' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''346M100US''||t.statefp||''''||t.cnectafp||''''||t.nectafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL350' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''350M100US''||t.nectafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL351' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''351M100US''||t.nectafp||''''||t.statefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL352' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''352M100US''||t.nectafp||''''||t.statefp||''''||placefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;         
         WHEN 'FSL355' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''355M100US''||t.nectafp||''''||t.nectadivfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL356' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''356M100US''||t.nectafp||''''||t.nectadivfp||''''||t.statefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL360' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''360M100US''||t.statefp||''''||t.nectafp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL361' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''361M100US''||t.statefp||''''||t.nectafp||''''||t.placefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL364' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''364M100US''||t.statefp||''''||t.nectafp||''''||t.nectadivfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL400' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''400C100US''||t.uace';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL410' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''410C100US''||t.uace||''''||t.statefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL420' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''420C100US''||t.statefp||''''||t.uace';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL430' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''430C100US''||t.uace||''''||t.statefp||''''||t.countyfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL500' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''5001200US''||t.statefp||''''||t.cdfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL510' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''5101200US''||t.statefp||''''||t.cdfp||''''||t.countyfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL511' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''5111200US''||t.statefp||''''||t.cdfp||''''||t.countyfp||''''||t.tractce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL521' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''5211200US''||t.statefp||''''||t.cdfp||''''||t.countyfp||''''||t.cousubfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL531' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''5311200US''||t.statefp||''''||t.cdfp||''''||t.placefp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL541' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''5411200US''||t.statefp||''''||t.cdfp||''''||t.concityfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL550' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''5501200US''||t.statefp||''''||t.cdfp||''''||t.aiannhce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL553' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''5531200US''||t.statefp||''''||t.cdfp||''''||t.aiannhce||''''||t.tribalsubce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL560' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''5601200US''||t.statefp||''''||t.cdfp||''''||t.anrcfp';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL610' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''610U200US''||t.statefp||''''||t.sldust';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL620' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''620L200US''||t.statefp||''''||t.sldlst';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL700' THEN
            -- When VTD has less than 6 characters
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''7000000US''||t.statefp||''''||t.countyfp||''''||lpad(t.vtdst,6,''0'') WHERE t.vtdst <> ''      ''';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
            -- When VTD has 6 blanks as a value 
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''7000000US''||t.statefp||''''||t.countyfp||''''||decode(t.vtdst,''      '',''000000'') WHERE t.vtdst = ''      ''';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL795' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''7950000US''||t.statefp_sl795||''''||t.puma5ce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL860' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''8600000US''||t.zcta5ce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL871' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''8710000US''||t.statefp||''''||t.zcta5ce';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;                     
         WHEN 'FSL950' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''9500000US''||t.statefp||''''||t.sddoe_e';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL956' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''9560000US''||t.statefp||''''||t.countyfp||''''||t.sddoe_e';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;        
         WHEN 'FSL960' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''9600000US''||t.statefp||''''||t.sddoe_s';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL966' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''9660000US''||t.statefp||''''||t.countyfp||''''||t.sddoe_s';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT; 
         WHEN 'FSL970' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''9700000US''||t.statefp||''''||t.sddoe_u';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL976' THEN
            sql_stmt := 'UPDATE '||vInTable||' t SET t.'||vNewColumnName||' = ''9760000US''||t.statefp||''''||t.countyfp||''''||t.sddoe_u';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;         
         ELSE
            dbms_output.put_line('WARNING: pInTable was not recognized');
            dbms_output.put_line('No '||vNewColumnName||' values were populated in '||vInTable);
      END CASE;
   END IF;
END add_geoid_column_to_table;
--
PROCEDURE fsl_views (pInTableFsl VARCHAR2) 
AS
   vTableFsl         VARCHAR2(30)         := UPPER(SUBSTR(pInTableFsl,1,30));
   vFslName          VARCHAR2(30)         := UPPER(Substr(vTableFsl,Instr(vTableFsl,'_') + 1,6));
   sql_stmt          VARCHAR2(4000);
   TmpCount          NUMBER;
   ContinueStatus    BOOLEAN              := TRUE;
BEGIN
   -----------------------------------------------------------------------------
   -- Do some validation
   sql_stmt := 'SELECT count(*) FROM user_tables WHERE table_name = :1';
   EXECUTE IMMEDIATE sql_stmt INTO TmpCount USING vTableFsl;
   IF (TmpCount < 1) THEN
      ContinueStatus := FALSE;
      dbms_output.put_line('pInTableFsl does not exist in current schema. Aborting now...');
   END IF;
   --------
   -----------------------------------------------------------------------------
   IF ContinueStatus THEN
      --------------------------------------------------------------------------
      CASE vFslName
         WHEN 'FSL010' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )'; 
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL020' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.regionce AS region,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL030' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.divisionce AS division,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;  
         WHEN 'FSL040' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL050' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;         
         WHEN 'FSL060' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.cousubfp AS cousub,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL061' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.cousubfp AS cousub,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL067' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.cousubfp AS cousub,
                                 a.submcdfp AS submcd,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL06V' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.submcdfp AS submcd,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL070' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.cousubfp AS cousub,
                                 a.placefp AS place,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL071' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.cousubfp AS cousub,
                                 a.euplace AS place,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL080' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.cousubfp AS cousub,
                                 a.placefp AS place,
                                 a.tractce AS tract,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL140' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.tractce AS tract,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL150' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.tractce AS tract,
                                 a.blkgrpce AS blkgrp,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL155' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.placefp AS place,
                                 a.countyfp AS county,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT; 
         WHEN 'FSL157' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.euplace AS place,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL160' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.placefp AS place,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL162' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.placefp AS place,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL170' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.concityfp AS concit,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT; 
         WHEN 'FSL172' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.concityfp AS concit,
                                 a.placefp AS place,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT; 
         WHEN 'FSL230' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.anrcfp AS anrc,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL250' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.aiannhce AS aianhh,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL251' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.aiannhce AS aianhh,
                                 a.tribalsubce as aitsce,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
           /** sql_stmt := 'Delete From fsl251v 
                         Where geo_id IN (Select GEO_ID
                                          From fsl251v
                                          Where AIANACE IN (Select AIANACE 
                                                            From fsl251v
                                                            GROUP BY AIANACE
                                                            HAVING COUNT(*) = 1)
                                          AND AIRSUBCE = ''999'')';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;*/
         WHEN 'FSL252' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.aiannhce AS aianhh,
                                 a.artli AS aihhtli,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL254' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.aiannhce AS aianhh,
                                 a.artli AS aihhtli,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL256' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.aiannhce AS aianhh,
                                 a.ttractce AS ttract,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL258' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.aiannhce AS aianhh,
                                 a.ttractce AS ttract,
                                 a.tblkgrpce AS tblkgrp,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL260' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.aiannhce AS aianhh,
                                 a.statefp AS state,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL270' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.aiannhce AS aianhh,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL280' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.aiannhce AS aianhh,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL281' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.aiannhce AS aianhh,
                                 a.tribalsubce as aitsce,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL283' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.aiannhce AS aianhh,
                                 a.artli AS aihhtli,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL286' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.aiannhce AS aianhh,
                                 a.artli AS aihhtli,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL310' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.cbsafp AS cbsa,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                        )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL311' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.cbsafp AS cbsa,
                                 a.statefp AS state,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                        )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL312' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.cbsafp AS cbsa,
                                 a.statefp AS state,
                                 a.placefp AS place,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL314' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.cbsafp AS cbsa,
                                 a.metdivfp AS metdiv,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL315' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.cbsafp AS cbsa,
                                 a.metdivfp AS metdiv,
                                 a.statefp AS state,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL320' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cbsafp AS cbsa,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL321' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cbsafp AS cbsa,
                                 a.placefp AS place,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL323' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cbsafp AS cbsa,
                                 a.metdivfp AS metdiv,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL330' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.csafp AS csa,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL331' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.csafp AS csa,
                                 a.statefp AS state,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL332' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.csafp AS csa,
                                 a.cbsafp AS cbsa,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL333' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.csafp AS csa,
                                 a.cbsafp AS cbsa,
                                 a.statefp AS state,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL335' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.cnectafp AS cnecta,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL336' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.cnectafp AS cnecta,
                                 a.statefp AS state,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL337' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.cnectafp AS cnecta,
                                 a.nectafp AS necta,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL338' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.cnectafp AS cnecta,
                                 a.nectafp AS necta,
                                 a.statefp AS state,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL340' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.csafp AS csa,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL341' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.csafp AS csa,
                                 a.cbsafp AS cbsa,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL345' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cnectafp AS cnecta,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL346' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cnectafp AS cnecta,
                                 a.nectafp AS necta,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL350' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.nectafp AS necta,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL351' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.nectafp AS necta,
                                 a.statefp AS state,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL352' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.nectafp AS necta,
                                 a.statefp AS state,
                                 a.placefp AS place,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;         
         WHEN 'FSL355' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.nectafp AS necta,
                                 a.nectadivfp AS nectadiv,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL356' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.nectafp AS necta,
                                 a.nectadivfp AS nectadiv,
                                 a.statefp AS state,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL360' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.nectafp AS necta,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL361' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.nectafp AS necta,
                                 a.placefp AS place,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL364' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.nectafp AS necta,
                                 a.nectadivfp AS nectadiv,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL400' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.uace AS ua,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL410' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.uace AS ua,
                                 a.statefp AS state,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL420' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.uace AS ua,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL430' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.uace AS ua,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL500' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cdfp AS cd,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL510' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cdfp AS cd,
                                 a.countyfp AS county,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL511' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cdfp AS cd,
                                 a.countyfp AS county,
                                 a.tractce AS tract,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL521' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cdfp AS cd,
                                 a.countyfp AS county,
                                 a.cousubfp AS cousub,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL531' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cdfp AS cd,
                                 a.placefp AS place,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL541' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cdfp AS cd,
                                 a.concityfp AS concit,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL550' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cdfp AS cd,
                                 a.aiannhce AS aianhh,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL553' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cdfp AS cd,
                                 a.aiannhce AS aianhh,
                                 a.tribalsubce as aitsce,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL560' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.cdfp AS cd,
                                 a.anrcfp AS anrc,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL610' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.sldust AS sldu,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL620' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.sldlst AS sldl,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL700' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.vtdst AS vtd,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL795' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp_sl795 AS state,
                                 a.puma5ce AS puma,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;    
         WHEN 'FSL860' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.zcta5ce AS zcta5,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT; 
         WHEN 'FSL871' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.zcta5ce AS zcta5,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;             
         WHEN 'FSL950' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.sddoe_e AS sdelm,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL956' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.sddoe_e AS sd_e,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;               
         WHEN 'FSL960' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.sddoe_s AS sdsec,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL966' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.sddoe_s AS sd_s,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL970' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.sddoe_u AS sduni,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;
         WHEN 'FSL976' THEN
            sql_stmt := 'CREATE OR REPLACE VIEW '||vTableFsl||'V 
                         AS
                         (SELECT a.geo_id,
                                 a.statefp AS state,
                                 a.countyfp AS county,
                                 a.sddoe_u AS sd_u,
                                 a.name,
                                 a.lsadstd AS lsad,
                                 a.sdogeometry                     
                          FROM  '||vTableFsl||' a
                         )';
            EXECUTE IMMEDIATE sql_stmt;
            COMMIT;         
         ELSE
            dbms_output.put_line('WARNING: pInTableFsl was not recognized ' ||vTableFsl);
      END CASE;
   END IF;
END fsl_views;
--
PROCEDURE FSL_MIRRORS (topology VARCHAR2, new_table VARCHAR2, mirror_table VARCHAR2)
AS
/**
 ################################################################################################################### 
 # Program Name: FSL_MIRRORS 
 # Author: Salman Mansoor 
 # Creation Date: 03/18/2010
 # Recent Revisions:  
 # Modification History:
 #
 # Purpose: 
 #  The purpose of this procedure is to create mirror tables of current FSL tables
 # 
 # Required parameters: new_table, mirror_table 
 #
 # Dependencies: FSL Tables 
 #
 ################################################################################################################### 
*/
sql_stmt               VARCHAR2(4000);
array_cols             MDSYS.STRING_ARRAY;          -- column array
v_table_fields         VARCHAR2(4000);              -- source table fields
v_col                  VARCHAR2(4000);              -- holds column names
v_mirror_table         VARCHAR2(100) := topology||'_'||mirror_table;
v_new_table            VARCHAR2(100) := topology||'_'||new_table;
BEGIN
    -- insert into array all the column names except TOPOGEOM 
    sql_stmt := 'select column_name from user_tab_columns where table_name = :1 and column_name <> :2 order by column_id';
    EXECUTE immediate sql_stmt bulk collect into array_cols using v_mirror_table, 'TOPOGEOM';
    --
    -- process column names from multiple lines into a single line; this creates the column names for the new feature table
    --
    v_table_fields := '';
    FOR i IN 1..array_cols.LAST
    LOOP
        v_col := array_cols(i);
        v_table_fields := v_table_fields || ',' || v_col;
    END LOOP;
    -- remove first comma
    v_table_fields := SUBSTR(v_table_fields,2,3999);
    --
    -- create table from already existing feature table source
    --
    sql_stmt := 'create table ' || v_new_table || ' nologging as select /*+ parallel 4 */ ' || v_table_fields || ' from ' || v_mirror_table;
    EXECUTE immediate sql_stmt;
END FSL_MIRRORS;
--
PROCEDURE PROCESS_FSL (Topology VARCHAR2, Face_tbl VARCHAR2, Topo_universe VARCHAR2, Topo_hierarchy VARCHAR2, Projection VARCHAR2 default 'N', Release VARCHAR2, Deploy VARCHAR2) 
AS
/**
 ###################################################################################################################
 # Program Name: process_fsl 
 # Author: Salman Mansoor 
 # Creation Date: 12/22/2010 
 # Recent Revision: 10/05/2011
 #
 # Purpose:
 #   The purpose of this procedure is to process state based fsl tables
 #    from the topology 
 #
 # Required parameters:
 # 
 #    topology = topology name
 #    face_tbl - zero level face feature table wihtout topology name prefix.
 #               (this procedure assumes the face table has the topology name
 #                as a prefix followed by an underscore, then the
 #                "face table name" upi are passing here)
 #    release = release from topo_universe table, 
 #               example ACS10 (prefix to source feature tables)
 #    deploy = schema you are running in
 #
 # Dependencies:
 #  - TOPO_UNIVERSE table
 #  - TOPO_HIERARCHY table
 #  
 #
 ###################################################################################################################
*/
vtopology           VARCHAR2(32)        := topology;
vface_tbl           VARCHAR2(32)        := face_tbl;
vdeploy             VARCHAR2(50)        := deploy;
vrelease            VARCHAR2(50)        := release;
vtopo_universe      VARCHAR2(32)        := topo_universe;
vtopo_hierarchy     VARCHAR2(32)        := topo_hierarchy;
vProjection         VARCHAR2(1)         := projection; -- Y or N
array_table_name    mdsys.string_array; -- table name array
v_table_name        VARCHAR2(32);       -- table name
sql_stmt            VARCHAR2(4000);     -- Dynamic SQL Statement
tbl_record_cnt      NUMBER;

BEGIN
    ---
    --- FSL Build Process 
    ---
    dbms_output.put_line('FSL Build Process for topology: ' ||vtopology|| ' Start');
    ---
    --- Create empty topology_fsl tables
    ---
    GZ_TOPO_HELPER.create_fsl_tables (vtopology,vtopo_universe,vrelease,vdeploy);
    ---
    --- Update field lengths 
    ---
    GZ_TOPO_BUILD.update_data_length (vtopology,vtopo_universe,vrelease,vdeploy);
    ---
    --- Register fsl tables
    ---
    GZ_TOPO_HELPER.REGISTER_FSL ('REGISTER_ALL', vtopology, Deploy => vdeploy, Topo_hierarchy => vtopo_hierarchy);
    ---
    --- Build fsl tables
    ---
    GZ_TOPO_BUILD.load_topo_fsl_state (vtopology, 'FSL', vtopo_universe, vtopo_hierarchy, vrelease, vdeploy);
    ---
    --- Update Sdogeometry
    ---
    GZ_TOPO_HELPER.update_sdogeometry (vtopology);
    ---
    --- Update Special Names and Lsad
    ---
    GZ_FSL_NAME_LSAD_UPDATE.update_summary_levels (vtopology,vdeploy);
    ---
    --- Projection for Z9 Data 
    ---
    IF (vProjection = 'Y')
     THEN
        sql_stmt := 'select /*+ PARALLEL 4 */ table_name from '||vtopo_universe||' where topology = :1 and deploy = :2 and release = :3 and table_name like :4 order by exec_order';
        EXECUTE immediate sql_stmt bulk collect INTO array_table_name USING vtopology, vdeploy, vrelease, 'FSL%';
        --
        FOR i IN 1..array_table_name.LAST
         LOOP
         --
         --
         v_table_name := array_table_name(i);
         --
         sql_stmt := 'select count(*) from '||vtopology||'_'||v_table_name;
         EXECUTE immediate sql_stmt INTO tbl_record_cnt;
         ---
         --- If Table has records  
         ---
         IF tbl_record_cnt > 0 
          THEN
           GZ_PROJECTION.PROJECT_2007_TO_ALBERS(vtopology,vtopology||'_'||v_table_name,'SDOGEOMETRY','OID',vdeploy,vtopology||'_PROJECTED_'||v_table_name,'NEW_GEOMETRY',NULL);
           ---
           --- Add Projected Geometry Column to Fsl Table 
           ---
           EXECUTE IMMEDIATE 'alter table '||vtopology||'_'||v_table_name||' rename column SDOGEOMETRY to GEOMETRY';
           EXECUTE IMMEDIATE 'alter table '||vtopology||'_'||v_table_name||' add (SDOGEOMETRY MDSYS.SDO_GEOMETRY)';
           ---
           --- Update Sdogeometry with Projected Geometry 
           ---
           sql_stmt := 'Update '||vtopology||'_'||v_table_name||' a Set a.sdogeometry = (Select b.new_geometry from '||vtopology||'_Projected_'||v_table_name||' b where b.oid = a.oid)
                        Where Exists (Select b.new_geometry from '||vtopology||'_Projected_'||v_table_name||' b where b.oid = a.oid)';
           EXECUTE immediate sql_stmt;
            
           Commit;  
            
           sql_stmt := 'Drop Table '||vtopology||'_Projected_'||v_table_name||' Purge';
           EXECUTE immediate sql_stmt;          
         END IF;
         --
        END LOOP; 
        --            
    END IF;
    ---
    --- Update Geoid, Lsad and Create Views
    ---
    GZ_TOPO_HELPER.process_views (vtopology);
    ---
    dbms_output.put_line('FSL Build Process for topology: ' ||vtopology|| ' Complete !!'); 
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
END PROCESS_FSL;
--
PROCEDURE PROCESS_FSL_DEC (Topology VARCHAR2, Face_tbl VARCHAR2, Topo_universe VARCHAR2, Topo_hierarchy VARCHAR2, Projection VARCHAR2 default 'N', Release VARCHAR2, Deploy VARCHAR2) 
AS
/**
 ###################################################################################################################
 # Program Name: process_fsl 
 # Author: Salman Mansoor 
 # Creation Date: 12/22/2010 
 # Recent Revision:
 #
 # Purpose:
 #   The purpose of this procedure is to process state based fsl tables
 #    from the topology 
 #
 # Required parameters:
 # 
 #    topology = topology name
 #    face_tbl - zero level face feature table wihtout topology name prefix.
 #               (this procedure assumes the face table has the topology name
 #                as a prefix followed by an underscore, then the
 #                "face table name" upi are passing here)
 #    release = release from topo_universe table, 
 #               example ACS10 (prefix to source feature tables)
 #    deploy = schema you are running in
 #
 # Dependencies:
 #  - TOPO_UNIVERSE table
 #  - TOPO_HIERARCHY table
 #  
 #
 ###################################################################################################################
*/
vtopology           VARCHAR2(32)        := topology;
vface_tbl           VARCHAR2(32)        := face_tbl;
vdeploy             VARCHAR2(50)        := deploy;
vrelease            VARCHAR2(50)        := release;
vtopo_universe      VARCHAR2(32)        := topo_universe;
vtopo_hierarchy     VARCHAR2(32)        := topo_hierarchy;
--vMirrors            VARCHAR2(1)         := mirrors; -- Y or N
vProjection         VARCHAR2(1)         := projection; -- Y or N
array_table_name    mdsys.string_array; -- table name array
v_table_name        VARCHAR2(32);       -- table name
sql_stmt            VARCHAR2(4000);     -- Dynamic SQL Statement
tbl_record_cnt      NUMBER;

BEGIN
    ---
    --- Update topology in topo tables 
    ---
    dbms_output.put_line('Update topology: ' ||vtopology|| ' in topo tables');
        
    sql_stmt := 'Update '||vtopo_hierarchy||' a set a.topology = :1 where a.table_name like :2';
    EXECUTE immediate sql_stmt Using vtopology, 'FSL%';
        
    sql_stmt := 'Update '||vtopo_universe||' a set a.topology = :1 where a.table_type = ''F'' and a.table_name like :2';
    EXECUTE immediate sql_stmt Using vtopology, 'FSL%';
                
    sql_stmt := 'Update '||vtopo_hierarchy||' a set a.child_layer_tbl = :1 where a.topology = :2 and a.table_name like :3';
    EXECUTE immediate sql_stmt Using vface_tbl, vtopology, 'FSL%';
    
    sql_stmt := 'Update '||vtopo_universe||' a set a.add_code2 = '''' where a.table_name = :1';
    EXECUTE immediate sql_stmt Using 'FSL010';
    
    sql_stmt := 'Update '||vtopo_universe||' a set a.deploy = :1';
    EXECUTE immediate sql_stmt Using vdeploy;
    
    Commit;
    ---
    --- Create empty topology_fsl tables
    ---
    GZ_TOPO_HELPER.create_fsl_tables (vtopology,vtopo_universe,vrelease,vdeploy);
    ---
    --- Register fsl tables
    ---
    GZ_TOPO_HELPER.REGISTER_FSL ('REGISTER_ALL', vtopology, Deploy => vdeploy, Topo_hierarchy => vtopo_hierarchy);
    ---
    --- Build fsl tables
    ---
    GZ_TOPO_BUILD.load_topo_fsl_state (vtopology, 'FSL', vtopo_universe, vtopo_hierarchy, vrelease, vdeploy);
    ---
    --- Update Sdogeometry
    ---
    GZ_TOPO_HELPER.update_sdogeom (vtopology);
    ---
    --- Mirrors 
    ---
    --IF (vMirrors = 'Y')
     --THEN
        
        --GZ_TOPO_HELPER.FSL_MIRRORS(vtopology, 'FSL280', 'FSL260');
        --GZ_TOPO_HELPER.FSL_MIRRORS(vtopology, 'FSL320', 'FSL311');
        --GZ_TOPO_HELPER.FSL_MIRRORS(vtopology, 'FSL321', 'FSL312');
        --GZ_TOPO_HELPER.FSL_MIRRORS(vtopology, 'FSL323', 'FSL315');
        --GZ_TOPO_HELPER.FSL_MIRRORS(vtopology, 'FSL336', 'FSL345');
        --GZ_TOPO_HELPER.FSL_MIRRORS(vtopology, 'FSL340', 'FSL331');
        --GZ_TOPO_HELPER.FSL_MIRRORS(vtopology, 'FSL341', 'FSL333');
        --GZ_TOPO_HELPER.FSL_MIRRORS(vtopology, 'FSL346', 'FSL338');
        --GZ_TOPO_HELPER.FSL_MIRRORS(vtopology, 'FSL360', 'FSL351');
        --GZ_TOPO_HELPER.FSL_MIRRORS(vtopology, 'FSL361', 'FSL352');
        --GZ_TOPO_HELPER.FSL_MIRRORS(vtopology, 'FSL364', 'FSL356');
    
        --sql_stmt := 'Update '||vtopology||'_FSL336 a set a.name = ''''';
        --EXECUTE immediate sql_stmt;
    
        --sql_stmt := 'Update '||vtopology||'_FSL336 a set a.lsad = ''''';
        --EXECUTE immediate sql_stmt;
    
        --Commit;
    
    --END IF;
    ---
    --- Update Special Names and Lsad
    ---
    GZ_FSL_NAME_LSAD_UPDATE.update_summary_levels (vtopology,vdeploy);
    ---
    --- Projection for Z9 Data 
    ---
    IF (vProjection = 'Y')
     THEN
        sql_stmt := 'select /*+ PARALLEL 4 */ table_name from '||vtopo_universe||' where topology = :1 and deploy = :2 and release = :3 and table_name like :4 order by exec_order';
        EXECUTE immediate sql_stmt bulk collect INTO array_table_name USING vtopology, vdeploy, vrelease, 'FSL%';
        --
        FOR i IN 1..array_table_name.LAST
         LOOP
         --
         --
         v_table_name := array_table_name(i);
         --
         sql_stmt := 'select count(*) from '||vtopology||'_'||v_table_name;
         EXECUTE immediate sql_stmt INTO tbl_record_cnt;
         ---
         --- If Table has records  
         ---
         IF tbl_record_cnt > 0 
          THEN
           GZ_PROJECTION.PROJECT_2007_TO_ALBERS(vtopology,vtopology||'_'||v_table_name,'SDOGEOMETRY','OID',vdeploy,'PROJECTED_'||v_table_name,'NEW_GEOMETRY',NULL);
           ---
           --- Add Projected Geometry Column to Fsl Table 
           ---
           EXECUTE IMMEDIATE 'alter table '||vtopology||'_'||v_table_name||' rename column SDOGEOMETRY to GEOMETRY';
           EXECUTE IMMEDIATE 'alter table '||vtopology||'_'||v_table_name||' add (SDOGEOMETRY MDSYS.SDO_GEOMETRY)';
           ---
           --- Update Sdogeometry with Projected Geometry 
           ---
           sql_stmt := 'Update '||vtopology||'_'||v_table_name||' a Set a.sdogeometry = (Select b.new_geometry from Projected_'||v_table_name||' b where b.oid = a.oid)
                        Where Exists (Select b.new_geometry from Projected_'||v_table_name||' b where b.oid = a.oid)';
           EXECUTE immediate sql_stmt;
            
           Commit;  
            
           sql_stmt := 'Drop Table Projected_'||v_table_name||' Purge';
           EXECUTE immediate sql_stmt;          
         END IF;
         --
        END LOOP; 
        --            
    END IF;
    ---
    --- Update Geoid, Lsad and Create Views
    ---
    GZ_TOPO_HELPER.process_views (vtopology);
    ---
    dbms_output.put_line('Complete !!'); 
    
END PROCESS_FSL_DEC;
--
PROCEDURE PROCESS_FSL_SD (Topology VARCHAR2, Face_tbl VARCHAR2, Release VARCHAR2, Deploy VARCHAR2) 
AS
/**
 ###################################################################################################################
 # Program Name: process_fsl 
 # Author: Salman Mansoor 
 # Creation Date: 08/19/2010 
 # Recent Revision:
 #
 # Purpose:
 #   The purpose of this procedure is to process state based fsl tables
 #    from the topology 
 #
 # Required parameters:
 # 
 #    topology = topology name
 #    face_tbl - zero level face feature table wihtout topology name prefix.
 #               (this procedure assumes the face table has the topology name
 #                as a prefix followed by an underscore, then the
 #                "face table name" upi are passing here)
 #    release = release from topo_universe table, 
 #               example ACS10 (prefix to source feature tables)
 #    deploy = schema you are running in
 #
 # Dependencies:
 #  - TOPO_UNIVERSE table
 #  - TOPO_HIERARCHY table
 #  
 #
 ###################################################################################################################
*/
vtopology           VARCHAR2(32)        := topology;
vface_tbl           VARCHAR2(32)        := face_tbl;
vdeploy             VARCHAR2(50)        := deploy;
vrelease            VARCHAR2(50)        := release;
topo_universe       VARCHAR2(32)        := 'TOPO_UNIVERSE';
topo_hierarchy      VARCHAR2(32)        := 'TOPO_HIERARCHY';
vTblList            MDSYS.STRING_ARRAY;
vCurTbl             VARCHAR2(32);
TmpCount            NUMBER;
ContinueStatus      BOOLEAN              := TRUE;
sql_stmt            VARCHAR2(4000);     -- Dynamic SQL Statement
BEGIN
    -----------------------------------------------------------------------------
    ---
    --- Do some Validation
    ---
    sql_stmt := 'SELECT count(*) FROM TOPO_UNIVERSE WHERE topology = :1';
    EXECUTE IMMEDIATE sql_stmt INTO TmpCount USING vtopology;
    IF (TmpCount > 0) THEN
      ContinueStatus := FALSE;
      dbms_output.put_line('Records already exists.');
    END IF;
    sql_stmt := 'SELECT count(*) FROM TOPO_HIERARCHY WHERE topology = :1';
    EXECUTE IMMEDIATE sql_stmt INTO TmpCount USING vtopology;
    IF (TmpCount > 0) THEN
      ContinueStatus := FALSE;
      dbms_output.put_line('Records already exists.');
    END IF;
    -----------------------------------------------------------------------------
    IF ContinueStatus THEN
        dbms_output.put_line('Inserting Records into TOPO_HIERARCHY');
        ---
        --- Inserting Records into TOPO_HIERARCHY
        ---
        sql_stmt := 'Insert into TOPO_HIERARCHY (table_name, column_name, name, gtype, seq_num) 
                     Select distinct table_name, column_name, name, gtype, seq_num 
                     From TOPO_HIERARCHY Where table_name like ''FSL%'' order by seq_num';
        EXECUTE immediate sql_stmt;
        dbms_output.put_line('Inserting Records into TOPO_UNIVERSE');
        ---
        --- Inserting Records into TOPO_UNIVERSE 
        ---
        sql_stmt := 'Insert into TOPO_UNIVERSE 
                    (TABLE_NAME,EXEC_ORDER,TABLE_TYPE,SUM_LEVEL,SUM_LEVEL_NAME,SOURCE_LOCATION,WHERE_CLAUSE,TBL_KEYS,EXCEPT_COLS,ADD_CODE1,ADD_CODE2,DEPLOY,RELEASE,TBL_PF) 
                     Select distinct TABLE_NAME,EXEC_ORDER,TABLE_TYPE,SUM_LEVEL,SUM_LEVEL_NAME,SOURCE_LOCATION,WHERE_CLAUSE,TBL_KEYS,EXCEPT_COLS,ADD_CODE1,ADD_CODE2,DEPLOY,RELEASE,TBL_PF
                     From TOPO_UNIVERSE Where table_name like ''FSL%'' order by exec_order';
        EXECUTE immediate sql_stmt;
        ---
        --- Update topology and child_layer_tbl = face_tbl
        ---
        dbms_output.put_line('Update topology: ' ||vtopology|| ' and child_layer_tbl: ' ||vface_tbl||'');
        sql_stmt := 'Update TOPO_HIERARCHY a set a.topology = :1 where a.topology is NULL';
        EXECUTE immediate sql_stmt Using vtopology;
        sql_stmt := 'Update TOPO_UNIVERSE a set a.topology = :1 where a.topology is NULL';
        EXECUTE immediate sql_stmt Using vtopology;
        sql_stmt := 'Update TOPO_HIERARCHY a set a.child_layer_tbl = :1 where a.topology = :2 and a.child_layer_tbl is NULL';
        EXECUTE immediate sql_stmt Using vface_tbl, vtopology;
        Commit;
    END IF;  
    ---
    --- Create empty topology_fsl tables
    ---
    sql_stmt := 'select table_name from user_tables where table_name like ''SL%'' '||
                'and table_name not like ''%TRACKING'' order by table_name';
    EXECUTE IMMEDIATE sql_stmt bulk collect INTO vTblList;
    FOR i IN vTblList.FIRST..vTblList.LAST
     LOOP
        vCurTbl := vTblList(i);
        sql_stmt:= 'Create table '||vtopology||'_F'||vCurTbl||' as select * from '||vCurTbl||' where rownum < 1';
        EXECUTE IMMEDIATE sql_stmt;
            --dbms_output.put_line('Processing: '||vCurTbl||'');
    END LOOP;
    ---
    --- Register fsl tables
    ---
    GZ_TOPO_HELPER.REGISTER_FSL ('REGISTER_ALL', vtopology, Deploy => vdeploy, Topo_hierarchy => topo_hierarchy);
    ---
    --- Build fsl tables
    ---
    GZ_TOPO_BUILD.load_topo_fsl_state (vtopology, 'FSL', vrelease, vdeploy, topo_universe, topo_hierarchy);
    ---
    --- Sdogeometry, Geoid, Views
    ---
    GZ_TOPO_HELPER.create_sdo_views (vtopology);
END PROCESS_FSL_SD;
--
PROCEDURE REGISTER_FSL (Operation VARCHAR2, Topology VARCHAR2, Table_Name VARCHAR2 Default NULL, Child_Layer_Table VARCHAR2 Default NULL, Deploy VARCHAR2, topo_hierarchy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: register_fsl 
 # Author: Salman 
 # Creation Date: 03/08/2010 
 # Recent Revision:
 #
 # Purpose:
 #   The purpose of this procedure is to register and unregister fsl tables
 #    from the topology 
 #
 # Required parameters:
 #  - operation - register single fsl table, register all, unregister single, unregister all fsl tables 
 #  - topology - name of topology 
 #  - table_name - name of table to register or unregister 
 #  - child_layer_table -  name of the child_layer_table 
 #  - deploy = schema deploy
 #
 # Dependencies:
 #  - TOPO_UNIVERSE table
 #  - TOPO_HIERARCHY table
 #  
 #
 ###################################################################################################################
*/
array_topology mdsys.string_array;                 -- topology
v_topology VARCHAR2(32)                            := topology;
array_table_name mdsys.string_array;               -- table name
v_table_name VARCHAR2(32)                          := table_name;
array_column_name mdsys.string_array;              -- column name
v_column_name VARCHAR2(32);
array_child_layer_tbl mdsys.string_array;          -- hierarchical position
v_child_layer_tbl VARCHAR2(32)                     := child_layer_table;
array_name mdsys.string_array;                     -- summary level name in TOPO_HIERARCHY
v_name VARCHAR2(250);
array_gtype mdsys.string_array;                    -- GTYPE
v_gtype VARCHAR2(10);                              -- geometry type
v_face VARCHAR2(32);                               -- face$ name
v_tg_layer_id NUMBER;                              -- tg_layer_id
sql_stmt VARCHAR2(4000);                           -- Dynamic SQL Statement
v_tbl_exists NUMBER;                               -- Table count 
BEGIN
    IF (Operation = 'REGISTER_ONE') THEN
        --
        dbms_output.put_line('Registering Single Fsl Table...');
        --
        sql_stmt := 'select tg_layer_id, column_name, tg_layer_type from all_sdo_topo_info where topology = :1 and table_name = :2 and owner = :3';
        EXECUTE immediate sql_stmt INTO v_tg_layer_id, v_column_name, v_gtype USING v_topology, v_child_layer_tbl, deploy;
        -- register the hierarchical feature table using tg_layer_id
        sdo_topo.add_topo_geometry_layer(v_topology,v_table_name,v_column_name,v_gtype,child_layer_id => v_tg_layer_id);
        --
        dbms_output.put_line ('Feature Table: ' || v_table_name || ' ==> (' || v_child_layer_tbl ||') registered to topology ' || v_topology);
        --
    ELSIF (Operation = 'REGISTER_ALL') THEN
        --
        dbms_output.put_line('Registering All Fsl Tables...');
        --
        -- obtain relevant information from TOPO_HIERARCHY to register feature tables in sequence
        --
        sql_stmt := 'select topology,table_name,column_name,child_layer_tbl,name,gtype from '||topo_hierarchy||' where topology = :1 and table_name like :2 order by seq_num';
        EXECUTE immediate sql_stmt bulk collect INTO array_topology,array_table_name,array_column_name,array_child_layer_tbl,array_name,array_gtype USING topology, 'FSL%';
        --
        -- register each feature table
        --
        FOR i IN 1..array_table_name.LAST 
        LOOP
            v_topology := array_topology(i);
            v_table_name := v_topology || '_' || array_table_name(i);
            v_column_name := array_column_name(i);
            v_child_layer_tbl := v_topology || '_' || array_child_layer_tbl(i);
            v_name := array_name(i);
            v_gtype := array_gtype(i);
            --
            -- check if table already registered in Topology
            --
            sql_stmt := 'select count(*) from all_sdo_topo_info where topology = :1 and owner = :2 and table_name = :3';
            EXECUTE immediate sql_stmt INTO v_tbl_exists USING v_topology, deploy, v_table_name;
            -- if table does not exist 
            IF v_tbl_exists = 0 THEN
             --
             -- register hierarchical feature tables (tg_level_id > 0)
             --
             IF v_child_layer_tbl IS NOT NULL THEN
                -- get the tg_layer_id for the parent from all_sdo_topo_info
                sql_stmt := 'select tg_layer_id from all_sdo_topo_info where topology = :1 and table_name = :2 and column_name = :3 and owner = :4';
                EXECUTE immediate sql_stmt INTO v_tg_layer_id USING v_topology, v_child_layer_tbl, v_column_name, deploy;
                -- register the hierarchical feature table using tg_layer_id
                sdo_topo.add_topo_geometry_layer(v_topology,v_table_name,v_column_name,v_gtype,child_layer_id => v_tg_layer_id);
                --
                dbms_output.put_line ('Feature Table: ' || v_table_name || ' ==> (' || v_child_layer_tbl ||') registered to topology ' || v_topology);
                --
             END IF;
            END IF;
            -- 
        END LOOP;
        --
    ELSIF (Operation = 'UNREGISTER_ONE') THEN
        --
        dbms_output.put_line('Unregistering Single Fsl Table...');
        --
        sdo_topo.delete_topo_geometry_layer(v_topology,v_table_name,'TOPOGEOM');  
        --
        dbms_output.put_line ('Feature Table: ' || v_table_name || ' de-registered from topology ' || v_topology);
        --
        EXECUTE IMMEDIATE 'delete from '||v_table_name||'';
        COMMIT;
        --
        EXECUTE IMMEDIATE 'ALTER INDEX '||v_topology||'_REL_IDX$ REBUILD';
        --EXECUTE IMMEDIATE 'ALTER INDEX '||v_topology||'_REL_LID$ REBUILD';
    ELSIF (Operation = 'UNREGISTER_ALL') THEN
        --
        dbms_output.put_line('Unregistering All Fsl Tables...');
        --
        sql_stmt := 'select table_name, column_name from all_sdo_topo_info where topology = :1 and owner = :2 and table_name like :3 and table_name is not null order by tg_layer_level desc';
        EXECUTE immediate sql_stmt bulk collect INTO array_table_name,array_column_name USING v_topology, deploy, v_topology||'_FSL%';
        --
        FOR i IN 1..array_table_name.LAST
        LOOP
            v_table_name := array_table_name(i);
            v_column_name := array_column_name(i);
            sdo_topo.delete_topo_geometry_layer (v_topology,v_table_name,v_column_name);
            dbms_output.put_line ('Feature Table: ' || v_table_name || ' (' || v_column_name ||') de-registered from topology ' || v_topology);
            EXECUTE IMMEDIATE 'delete from '||v_table_name||'';
            COMMIT;
        END LOOP;
        --
        EXECUTE IMMEDIATE 'ALTER INDEX '||v_topology||'_REL_IDX$ REBUILD';
        --EXECUTE IMMEDIATE 'ALTER INDEX '||v_topology||'_REL_LID$ REBUILD';      
    END IF;   
END REGISTER_FSL;
--
PROCEDURE DEREGISTER_FSL (Topology VARCHAR2, Table_Type VARCHAR2, Deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: register_fsl 
 # Author: Salman 
 # Creation Date: 11/01/2010 
 # Recent Revision:
 #
 # Purpose:
 #   The purpose of this procedure is to unregister and drop fsl tables
 #    from the topology 
 #
 # Required parameters:
 #  - topology - name of topology 
 #  - deploy = schema deploy
 #
 # Dependencies:
 #  - TOPO_UNIVERSE table
 #  - TOPO_HIERARCHY table
 #  
 #
 ###################################################################################################################
*/
array_topology mdsys.string_array;                 -- topology
v_topology VARCHAR2(32)                            := topology;
array_table_name mdsys.string_array;               -- table name
v_table_name VARCHAR2(32);
array_column_name mdsys.string_array;              -- column name
v_column_name VARCHAR2(32);
array_name mdsys.string_array;                     -- summary level name in TOPO_HIERARCHY
v_name VARCHAR2(250);
array_gtype mdsys.string_array;                    -- GTYPE
v_gtype VARCHAR2(10);                              -- geometry type
v_face VARCHAR2(32);                               -- face$ name
v_tg_layer_id NUMBER;                              -- tg_layer_id
v_table_type VARCHAR2(10);                         -- type of hierarchical feature table - SL or FSL
sql_stmt VARCHAR2(4000);                           -- Dynamic SQL Statement
BEGIN
    dbms_output.put_line('Unregistering All Fsl/SL Tables...');
    --
    v_table_type := table_type || '%';
    --
    sql_stmt := 'select table_name, column_name from all_sdo_topo_info where topology = :1 and owner = :2 and table_name like :3 and table_name is not null order by tg_layer_level desc';
    EXECUTE immediate sql_stmt bulk collect INTO array_table_name,array_column_name USING v_topology, deploy, v_topology||'_'||v_table_type;
    
    FOR i IN 1..array_table_name.LAST
     LOOP
        v_table_name := array_table_name(i);
        v_column_name := array_column_name(i);
        sdo_topo.delete_topo_geometry_layer (v_topology,v_table_name,v_column_name);
        dbms_output.put_line ('Feature Table: ' || v_table_name || ' (' || v_column_name ||') de-registered from topology ' || v_topology);
        EXECUTE IMMEDIATE 'drop table '||v_table_name||' purge';
     END LOOP;
     
     EXECUTE IMMEDIATE 'ALTER INDEX '||v_topology||'_REL_IDX$ REBUILD';
     --EXECUTE IMMEDIATE 'ALTER INDEX MT_REL_LID$ REBUILD';      
END DEREGISTER_FSL;
--
PROCEDURE REGISTER_FSL_ORG (Operation VARCHAR2, Topology VARCHAR2, Table_Name VARCHAR2 Default NULL, Child_Layer_Table VARCHAR2 Default NULL, Deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: register_fsl 
 # Author: Salman 
 # Creation Date: 03/08/2010 
 # Recent Revision:
 #
 # Purpose:
 #   The purpose of this procedure is to register and unregister fsl tables
 #    from the topology 
 #
 # Required parameters:
 #  - operation - register single fsl table, register all, unregister single, unregister all fsl tables 
 #  - topology - name of topology 
 #  - table_name - name of table to register or unregister 
 #  - child_layer_table -  name of the child_layer_table 
 #  - deploy = schema deploy
 #
 # Dependencies:
 #  - TOPO_UNIVERSE table
 #  - TOPO_HIERARCHY table
 #  
 #
 ###################################################################################################################
*/
array_topology mdsys.string_array;                 -- topology
v_topology VARCHAR2(32)                            := topology;
array_table_name mdsys.string_array;               -- table name
v_table_name VARCHAR2(32)                          := table_name;
array_column_name mdsys.string_array;              -- column name
v_column_name VARCHAR2(32);
array_child_layer_tbl mdsys.string_array;          -- hierarchical position
v_child_layer_tbl VARCHAR2(32)                     := child_layer_table;
array_name mdsys.string_array;                     -- summary level name in TOPO_HIERARCHY
v_name VARCHAR2(250);
array_gtype mdsys.string_array;                    -- GTYPE
v_gtype VARCHAR2(10);                              -- geometry type
v_face VARCHAR2(32);                               -- face$ name
v_tg_layer_id NUMBER;                              -- tg_layer_id
sql_stmt VARCHAR2(4000);                           -- Dynamic SQL Statement
BEGIN
    IF (Operation = 'REGISTER_ONE') THEN
        dbms_output.put_line('Registering Single Fsl Table...');
        sql_stmt := 'select tg_layer_id, column_name, tg_layer_type from all_sdo_topo_info where topology = :1 and table_name = :2 and owner = :3';
        EXECUTE immediate sql_stmt INTO v_tg_layer_id, v_column_name, v_gtype USING v_topology, v_child_layer_tbl, deploy;
        -- register the hierarchical feature table using tg_layer_id
        sdo_topo.add_topo_geometry_layer(v_topology,v_table_name,v_column_name,v_gtype,child_layer_id => v_tg_layer_id);
        dbms_output.put_line ('Feature Table: ' || v_table_name || ' ==> (' || v_child_layer_tbl ||') registered to topology ' || v_topology);
    ELSIF (Operation = 'REGISTER_ALL') THEN
        dbms_output.put_line('Registering All Fsl Tables...');
        --
        -- obtain relevant information from TOPO_HIERARCHY to register feature tables in sequence
        --
        sql_stmt := 'select topology,table_name,column_name,child_layer_tbl,name,gtype from topo_hierarchy where topology = :1 and table_name like :2 order by seq_num';
        EXECUTE immediate sql_stmt bulk collect INTO array_topology,array_table_name,array_column_name,array_child_layer_tbl,array_name,array_gtype USING topology, 'FSL%';
        --
        -- register each feature table
        --
        FOR i IN 1..array_table_name.LAST 
        LOOP
            v_topology := array_topology(i);
            v_table_name := array_table_name(i);
            v_column_name := array_column_name(i);
            v_child_layer_tbl := array_child_layer_tbl(i);
            v_name := array_name(i);
            v_gtype := array_gtype(i);
            --
            -- register hierarchical feature tables (tg_level_id > 0)
            --
            IF v_child_layer_tbl IS NOT NULL THEN
                -- get the tg_layer_id for the parent from all_sdo_topo_info
                sql_stmt := 'select tg_layer_id from all_sdo_topo_info where topology = :1 and table_name = :2 and column_name = :3 and owner = :4';
                EXECUTE immediate sql_stmt INTO v_tg_layer_id USING v_topology, v_child_layer_tbl, v_column_name, deploy;
                -- register the hierarchical feature table using tg_layer_id
                sdo_topo.add_topo_geometry_layer(v_topology,v_table_name,v_column_name,v_gtype,child_layer_id => v_tg_layer_id);
                dbms_output.put_line ('Feature Table: ' || v_table_name || ' ==> (' || v_child_layer_tbl ||') registered to topology ' || v_topology);
            END IF;
        END LOOP;
    ELSIF (Operation = 'UNREGISTER_ONE') THEN
        dbms_output.put_line('Unregistering Single Fsl Table...');
        sdo_topo.delete_topo_geometry_layer(v_topology,v_table_name,'TOPOGEOM');  
        dbms_output.put_line ('Feature Table: ' || v_table_name || ' de-registered from topology ' || v_topology);
        EXECUTE IMMEDIATE 'delete from '||v_table_name||'';
        COMMIT;
        EXECUTE IMMEDIATE 'ALTER INDEX MT_REL_IDX$ REBUILD';
        EXECUTE IMMEDIATE 'ALTER INDEX MT_REL_LID$ REBUILD';
    ELSIF (Operation = 'UNREGISTER_ALL') THEN
        dbms_output.put_line('Unregistering All Fsl Tables...');
        sql_stmt := 'select table_name, column_name from all_sdo_topo_info where topology = :1 and owner = :2 and table_name like :3 and table_name is not null order by tg_layer_level desc';
        EXECUTE immediate sql_stmt bulk collect INTO array_table_name,array_column_name USING v_topology, deploy, 'FSL%';
        FOR i IN 1..array_table_name.LAST
        LOOP
            v_table_name := array_table_name(i);
            v_column_name := array_column_name(i);
            sdo_topo.delete_topo_geometry_layer (v_topology,v_table_name,v_column_name);
            dbms_output.put_line ('Feature Table: ' || v_table_name || ' (' || v_column_name ||') de-registered from topology ' || v_topology);
            EXECUTE IMMEDIATE 'delete from '||v_table_name||'';
            COMMIT;
        END LOOP;
        EXECUTE IMMEDIATE 'ALTER INDEX MT_REL_IDX$ REBUILD';
        EXECUTE IMMEDIATE 'ALTER INDEX MT_REL_LID$ REBUILD';      
    END IF;   
END REGISTER_FSL_ORG;
--
PROCEDURE create_sdo_views (Topology VARCHAR2) 
AS
vtopology           VARCHAR2(32)        := topology;
sql_stmt            VARCHAR2(4000); -- Dynamic SQL Statement
vTblList            MDSYS.STRING_ARRAY;
vCurTbl             VARCHAR2(40);
cnt                 NUMBER;
BEGIN
   sql_stmt := 'select table_name from user_tables where table_name like :1 '||
               'and table_name not like :2 order by table_name';
   EXECUTE IMMEDIATE sql_stmt bulk collect INTO vTblList USING vtopology||'_FSL%', '%TRACKING';
   FOR i IN vTblList.FIRST..vTblList.LAST
   LOOP
        vCurTbl := vTblList(i);
        sql_stmt:= 'Alter table '||vCurTbl||' add sdogeometry mdsys.sdo_geometry';
        EXECUTE IMMEDIATE sql_stmt;
        sql_stmt:= 'UPDATE /*+ PARALLEL 4 */ '||vCurTbl||' a set a.sdogeometry = a.topogeom.get_geometry()';
        EXECUTE IMMEDIATE sql_stmt;
        Commit;
        -- Add spatial indexes 
        --
        -- Insert entry into user_sdo_geom_metadata and create spatial index, if not previously completed
        sql_stmt := 'select count(*) from user_sdo_geom_metadata where table_name = :1 and column_name = ''SDOGEOMETRY''';
        EXECUTE immediate sql_stmt INTO cnt USING vCurTbl;
        IF cnt = 0 THEN
            sql_stmt := 'insert into user_sdo_geom_metadata values (:1,''SDOGEOMETRY'',SDO_DIM_ARRAY (SDO_DIM_ELEMENT (''X'',-180,180,0.05),SDO_DIM_ELEMENT(''Y'',-90,90,0.05)), 8265)';
            EXECUTE immediate sql_stmt USING vCurTbl;
            COMMIT;
        END IF;
        sql_stmt := 'create index ' || vCurTbl || '_SIDX on ' || vCurTbl || ' (SDOGEOMETRY) indextype is mdsys.spatial_index';
        EXECUTE immediate sql_stmt;
        --dbms_output.put_line('Sdogeometry Update: '||vCurTbl||'');
   END LOOP;
   FOR i IN vTblList.FIRST..vTblList.LAST
   LOOP
        vCurTbl := vTblList(i);
        add_geoid_column_to_table(vCurTbl);
        sql_stmt:= 'Alter table '||vCurTbl||' add LSADSTD VARCHAR2(7)';
        EXECUTE IMMEDIATE sql_stmt;
        sql_stmt:= 'UPDATE /*+ PARALLEL 4 */ '||vCurTbl||' t set t.Lsadstd = (Select l.standard From Lut_Lsad l Where l.lsad = t.lsad) Where Exists (Select l.standard From Lut_Lsad l Where l.lsad = t.lsad)';
        EXECUTE IMMEDIATE sql_stmt;
        Commit;
        fsl_views(vCurTbl); 
        dbms_output.put_line('Finished Processing: '||vCurTbl||'');
   END LOOP;
END create_sdo_views;
--
PROCEDURE process_views (Topology VARCHAR2) 
AS
vtopology           VARCHAR2(32)        := topology;
sql_stmt            VARCHAR2(4000); -- Dynamic SQL Statement
vTblList            MDSYS.STRING_ARRAY;
vCurTbl             VARCHAR2(40);
cnt                 NUMBER;
BEGIN
   sql_stmt := 'select table_name from user_tables where table_name like :1 '||
               'and table_name not like :2 order by table_name';
   EXECUTE IMMEDIATE sql_stmt bulk collect INTO vTblList USING vtopology||'_FSL%','%TRACKING' ;
   FOR i IN vTblList.FIRST..vTblList.LAST
    LOOP
        vCurTbl := vTblList(i);
        add_geoid_column_to_table(vCurTbl);
        sql_stmt:= 'Alter table '||vCurTbl||' add LSADSTD VARCHAR2(7)';
        EXECUTE IMMEDIATE sql_stmt;
        sql_stmt:= 'UPDATE /*+ PARALLEL 4 */ '||vCurTbl||' t set t.Lsadstd = (Select l.standard From Lut_Lsad l Where l.lsad = t.lsad) Where Exists (Select l.standard From Lut_Lsad l Where l.lsad = t.lsad)';
        EXECUTE IMMEDIATE sql_stmt;
        Commit;
        fsl_views(vCurTbl); 
        dbms_output.put_line('Finished Processing: '||vCurTbl||'');
    END LOOP;
END process_views;
--
PROCEDURE update_sdogeometry (Topology VARCHAR2) 
AS
vtopology           VARCHAR2(32)        := topology;
sql_stmt            VARCHAR2(4000); -- Dynamic SQL Statement
vTblList            MDSYS.STRING_ARRAY;
vRowList            MDSYS.STRING_ARRAY;
vCurTbl             VARCHAR2(40);
vRow                ROWID;
cnt                 NUMBER;
BEGIN
   sql_stmt := 'select table_name from user_tables where table_name like :1 '||
               'and table_name not like :2 order by table_name';
   EXECUTE IMMEDIATE sql_stmt bulk collect INTO vTblList USING vtopology||'_FSL%','%TRACKING' ;
   
   FOR i IN vTblList.FIRST..vTblList.LAST
    LOOP
        vCurTbl := vTblList(i);
        cnt := 0;
        dbms_output.put_line('Current Table = '||vCurTbl);
        sql_stmt:= 'Alter table '||vCurTbl||' add sdogeometry mdsys.sdo_geometry';
        EXECUTE IMMEDIATE sql_stmt;
        --
        sql_stmt := 'select rowid from '||vCurTbl||' order by rowid';
        EXECUTE immediate sql_stmt bulk collect INTO vRowList; 
        --
        -- Check to see if any data exists  
        --
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
          sql_stmt:= 'UPDATE /*+ PARALLEL 4 */ '||vCurTbl||' a set a.sdogeometry = a.topogeom.get_geometry() where a.rowid = :1';
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
        cnt := 0;
        -- Add spatial indexes 
        --
        -- Insert entry into user_sdo_geom_metadata and create spatial index, if not previously completed
        sql_stmt := 'select count(*) from user_sdo_geom_metadata where table_name = :1 and column_name = ''SDOGEOMETRY''';
        EXECUTE immediate sql_stmt INTO cnt USING vCurTbl; 
        --
        IF cnt = 0 THEN
            sql_stmt := 'insert into user_sdo_geom_metadata values (:1,''SDOGEOMETRY'',SDO_DIM_ARRAY (SDO_DIM_ELEMENT (''X'',-180,180,0.05),SDO_DIM_ELEMENT(''Y'',-90,90,0.05)), 8265)';
            EXECUTE immediate sql_stmt USING vCurTbl;
            COMMIT;
        END IF;
        --
        sql_stmt := 'create index ' || vCurTbl || '_SIDX on ' || vCurTbl || ' (SDOGEOMETRY) indextype is mdsys.spatial_index';
        EXECUTE immediate sql_stmt;
        --
    END LOOP;
    --
END update_sdogeometry;
--
PROCEDURE update_sdogeom (Topology VARCHAR2) 
AS
vtopology           VARCHAR2(32)        := topology;
sql_stmt            VARCHAR2(4000); -- Dynamic SQL Statement
vTblList            MDSYS.STRING_ARRAY;
vCurTbl             VARCHAR2(40);
cnt                 NUMBER;
BEGIN
   sql_stmt := 'select table_name from user_tables where table_name like :1 '||
               'and table_name not like :2 order by table_name';
   EXECUTE IMMEDIATE sql_stmt bulk collect INTO vTblList USING vtopology||'_FSL%','%TRACKING' ;
   FOR i IN vTblList.FIRST..vTblList.LAST
    LOOP

        vCurTbl := vTblList(i);
        dbms_output.put_line('Current Table = '||vCurTbl); -- STEPHANIE
        sql_stmt:= 'Alter table '||vCurTbl||' add sdogeometry mdsys.sdo_geometry';
        dbms_output.put_line('SQL = '||sql_stmt); -- STEPHANIE
        EXECUTE IMMEDIATE sql_stmt;
        sql_stmt:= 'UPDATE /*+ PARALLEL 4 */ '||vCurTbl||' a set a.sdogeometry = a.topogeom.get_geometry()';
        EXECUTE IMMEDIATE sql_stmt;
        Commit;
        -- Add spatial indexes 
        --
        -- Insert entry into user_sdo_geom_metadata and create spatial index, if not previously completed
        sql_stmt := 'select count(*) from user_sdo_geom_metadata where table_name = :1 and column_name = ''SDOGEOMETRY''';
        EXECUTE immediate sql_stmt INTO cnt USING vCurTbl; -- STEPHANIE (commented out)
        IF cnt = 0 THEN
            sql_stmt := 'insert into user_sdo_geom_metadata values (:1,''SDOGEOMETRY'',SDO_DIM_ARRAY (SDO_DIM_ELEMENT (''X'',-180,180,0.05),SDO_DIM_ELEMENT(''Y'',-90,90,0.05)), 8265)';
            EXECUTE immediate sql_stmt USING vCurTbl;
            COMMIT;
        END IF;
        sql_stmt := 'create index ' || vCurTbl || '_SIDX on ' || vCurTbl || ' (SDOGEOMETRY) indextype is mdsys.spatial_index';
        EXECUTE immediate sql_stmt;
        --dbms_output.put_line('Sdogeometry Update: '||vCurTbl||'');
    END LOOP;
END update_sdogeom;
--
PROCEDURE create_scr_tables (topology VARCHAR2,face_tbl VARCHAR2,topo_universe VARCHAR2,release varchar2,deploy VARCHAR2)
AS
sql_stmt  VARCHAR2(4000); -- Dynamic SQL Statement
--
array_table_name mdsys.string_array; -- table name array
array_source_col mdsys.string_array; -- column array
--
tbl_check_cnt      NUMBER;                        -- table count
tbl_record_cnt     NUMBER;                        -- table record count
v_table_name       VARCHAR2(32);                  -- table name
v_tbl_type         VARCHAR2(2);                   -- table type
v_source_location  VARCHAR2(100);                 -- table location
v_where_clause     VARCHAR2(1000);                -- table where clause
v_tbl_keys         VARCHAR2(250);                 -- table keys
v_temp_vr1         VARCHAR2(32);                  -- geometry validation table name holder
v_vr1_cnt          NUMBER;                        -- number of records in first validation table
v_temp_vr2         VARCHAR2(32);                  -- geometry validation table name holder
v_spacol           VARCHAR2(11) := 'SDOGEOMETRY'; -- spatial column to be validated - fixed
v_face_tbl         VARCHAR2(32);                  -- face feature table with topology prefix
v_code1            VARCHAR2(4000);                -- special code 1
v_code2            VARCHAR2(4000);                -- special code 2
v_except_cols      VARCHAR2(4000);                -- MAF/TIGER columns not wanted in source table
v_table_fields     VARCHAR2(4000);                -- source table fields
v_col              VARCHAR2(4000);                -- holds column names
--
v_index_name VARCHAR2(50); -- index name
BEGIN
 --
 -- set face feature with topology prefix
 --
 v_face_tbl := topology||'_'||face_tbl;
 --
 sql_stmt := 'select /*+ PARALLEL 4 */ table_name from '||topo_universe||' where topology = :1 and deploy = :2 and release = :3 and table_type = :4 order by exec_order';
 EXECUTE immediate sql_stmt bulk collect INTO array_table_name USING topology, deploy, release, 'DS';
 --
 -- Check to see if derived source table exists in topo_universe 
 --
 IF (array_table_name.COUNT > 0) 
  THEN
  --
  FOR i IN 1..array_table_name.LAST
   LOOP
   --
   --
   v_table_name := array_table_name(i);
   --
   -- select all relevant data from the topo_universe table
   --
   sql_stmt := 'select source_location,where_clause,tbl_keys,add_code1,add_code2,table_type from '||topo_universe||' where topology = :1 and deploy = :2 and table_name = :3 and release = :4';
   EXECUTE immediate sql_stmt INTO v_source_location,v_where_clause,v_tbl_keys,v_code1,v_code2,v_tbl_type USING topology,deploy,v_table_name,release;
   --
   --
   sql_stmt := 'select count(*) from user_tables where table_name = :1';
   EXECUTE immediate sql_stmt INTO tbl_check_cnt USING v_table_name;
   --
   -- if table exists then drop/purge table
   --
   IF tbl_check_cnt > 0 THEN
    --
    -- drop/purge table
    --
    sql_stmt := 'drop table ' || v_table_name || ' purge';
    EXECUTE immediate sql_stmt;
   END IF;
   --
   --
   --
   IF v_tbl_type = 'DS' AND v_source_location IS NULL THEN
    BEGIN
        -- select code to be executed #1
        IF v_code1 IS NOT NULL 
         THEN
            v_code1 := regexp_replace(v_code1, 'face', v_face_tbl);
            sql_stmt := v_code1;
            EXECUTE immediate sql_stmt; -- execute first code set
            COMMIT;
        END IF;
        -- select code to be executed #2
        IF v_code2 IS NOT NULL 
         THEN
            v_code2 := regexp_replace(v_code2, 'face', v_face_tbl);
            sql_stmt := v_code2;
            EXECUTE immediate sql_stmt; -- execute second code set
            COMMIT;
        END IF;
    END;
   END IF;
   --
   -- create unique index for source table where table_keys are not null
   --
   IF v_tbl_type = 'DS' AND v_tbl_keys IS NOT NULL THEN
    sql_stmt := 'create index ' || v_table_name || '_uk on ' || v_table_name || '(' || v_tbl_keys || ') nologging';
    EXECUTE immediate sql_stmt;
   END IF;
   --
   -- grant select access to public
   --
   --
   sql_stmt := 'grant select on ' || deploy || '.' || v_table_name || ' to public';
   EXECUTE immediate sql_stmt;
   COMMIT;
 
  END LOOP;
  --
  dbms_output.put_line ('Derived Source Creation - Complete Successfully'); 
  --
 ELSE
  --
  dbms_output.put_line ('Derived Source Does Not Exist'); 
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
   --
   -- RETURN;
   RAISE;
   --
END create_scr_tables;
--

PROCEDURE create_fsl_tables (topology VARCHAR2,topo_universe VARCHAR2,release varchar2,deploy VARCHAR2)
AS
sql_stmt  VARCHAR2(4000); -- Dynamic SQL Statement
--
array_table_name mdsys.string_array; -- table name array
array_source_col mdsys.string_array; -- column array
--
tbl_check_cnt      NUMBER;                        -- table count
tbl_record_cnt     NUMBER;                        -- table record count
v_table_name       VARCHAR2(32);                  -- table name
v_tbl_type         VARCHAR2(2);                   -- table type
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
--
fsl_rep            VARCHAR2(100) := topology||'_FSL'; 
v_index_name       VARCHAR2(50); -- index name
BEGIN
 sql_stmt := 'select /*+ PARALLEL 4 */ table_name from '||topo_universe||' where topology = :1 and deploy = :2 and release = :3 and table_type in (:4,:5) order by exec_order';
 EXECUTE immediate sql_stmt bulk collect INTO array_table_name USING topology, deploy, release, 'F', 'DF';
 --
 -- Check to see if derived source table exists in topo_universe 
 --
 IF (array_table_name.COUNT > 0) 
  THEN
  --
  FOR i IN 1..array_table_name.LAST
   LOOP
   --
   --
   v_table_name := array_table_name(i);
   --
   -- select all relevant data from the topo_universe table
   --
   sql_stmt := 'select source_location,where_clause,tbl_keys,add_code1,add_code2,table_type from '||topo_universe||' where topology = :1 and deploy = :2 and table_name = :3 and release = :4';
   EXECUTE immediate sql_stmt INTO v_source_location,v_where_clause,v_tbl_keys,v_code1,v_code2,v_tbl_type USING topology,deploy,v_table_name,release;
   --
   --
   sql_stmt := 'select count(*) from user_tables where table_name = :1';
   EXECUTE immediate sql_stmt INTO tbl_check_cnt USING topology||'_'||v_table_name;
   --
   -- if table does not exist 
   --
   IF tbl_check_cnt = 0 
    THEN
     --
     IF ((v_tbl_type = 'F' OR v_tbl_type = 'DF') AND v_source_location IS NOT NULL) 
      THEN
       sql_stmt := 'create table ' ||topology||'_'||v_table_name|| ' nologging as select /*+ parallel 4*/ * from ' || v_source_location || ' where ' || v_where_clause;
       EXECUTE immediate sql_stmt;
       --
       BEGIN
         -- select code to be executed #1
         IF v_code1 IS NOT NULL 
          THEN
            v_code1 := regexp_replace(v_code1, 'fsl', fsl_rep);
            sql_stmt := v_code1;
            EXECUTE immediate sql_stmt; -- execute first code set
            COMMIT;
         END IF;
         -- select code to be executed #2
         IF v_code2 IS NOT NULL 
          THEN
            v_code2 := regexp_replace(v_code2, 'fsl', fsl_rep);
            sql_stmt := v_code2;
            EXECUTE immediate sql_stmt; -- execute second code set
            COMMIT;
         END IF;
       END;
     END IF;
     --
     -- grant select access to public
     --
     --
     sql_stmt := 'grant select on ' ||deploy|| '.' ||topology||'_'||v_table_name|| ' to public';
     EXECUTE immediate sql_stmt;
     COMMIT;
   END IF;
  END LOOP;
  --
  dbms_output.put_line ('Fsl Table Creation - Complete Successfully'); 
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
   --
   -- RETURN;
   RAISE;
   --
END create_fsl_tables;
--
PROCEDURE RELATION_TOPO_MODIFY (pTopology VARCHAR2, pInTable VARCHAR2) AS
sql_stmt        VARCHAR2(4000);
array_notkeep   MDSYS.SDO_NUMBER_ARRAY;
vTglayer_id     NUMBER;   
vkeep           NUMBER;
vnotkeep        NUMBER;
vtgid           NUMBER;
vTopology       VARCHAR2(50) := UPPER(pTopology);
vInTable        VARCHAR2(50) := UPPER(pInTable);  
vTmpTable       VARCHAR2(10) := 'TMP_FACES';
BEGIN
    sql_stmt := 'SELECT t.topogeom.tg_layer_id FROM FACE t WHERE rownum = 1';
    EXECUTE IMMEDIATE sql_stmt INTO vTglayer_id;
    -- Drop temp table if it exist 
    --IF CDB_UTIL.table_exists(vTmpTable) THEN
    --    CDB_UTIL.drop_table(vTmpTable,'COMPLETE');
    --END IF;
    --Create a temporary table 
    sql_stmt := 'CREATE TABLE '||vTmpTable||' NOLOGGING AS 
                 SELECT a.tg_layer_id, 
                        a.TG_ID, 
                        a.TOPO_ID, 
                        e.edge_id, 
                        e.left_face_id, 
                        e.right_face_id, 
                        e.keep_face, 
                        e.not_keep
                 FROM '||vTopology||'_RELATION$ a, 
                      '||vInTable||' e
                 WHERE a.topo_id = e.keep_face
                 AND a.TG_LAYER_ID = '||vTgLayer_id||'
                 ORDER BY e.keep_face';
    EXECUTE IMMEDIATE sql_stmt;
    sql_stmt := 'CREATE INDEX rel_idx on '||vTmpTable||' (keep_face, not_keep)';
    EXECUTE IMMEDIATE sql_stmt;
    sql_stmt := 'SELECT a.not_keep FROM '||vTmpTable||' a ORDER BY a.keep_face';
    EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO array_notkeep; 
    FOR i in 1..array_notkeep.COUNT LOOP
        vnotkeep := array_notkeep(i);
        sql_stmt := 'SELECT b.keep_face, b.tg_id FROM '||vTmpTable||' b WHERE b.not_keep = :1';
        EXECUTE IMMEDIATE sql_stmt INTO vkeep, vtgid USING vnotkeep;
        sql_stmt := 'UPDATE '||vTopology||'_RELATION$ c 
                     SET c.TG_ID = :1 WHERE c.TOPO_ID = :2 AND c.TG_LAYER_ID = :3';
        EXECUTE IMMEDIATE sql_stmt USING vtgid, vnotkeep, vTglayer_id;
    END LOOP;
    COMMIT;
END RELATION_TOPO_MODIFY;
--
PROCEDURE build_face2_cols (pFaceTable VARCHAR2,pFaceDollarTable VARCHAR2)
AS
/**
 #############################################################################################################
 # Program Name: build_face2_cols
 # Author: Nick.A.Padfield
 # Creation Date: 12/30/2008
 # Modification History: 07/14/2009 - mz - Automated, 10/18/2009 - Salman - Face2 
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
vTmpTable VARCHAR2(30) := 'XFACETMP';
vTmpTableIDX VARCHAR2(30) := 'XFACETMP_FACE_ID_IDX';
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
  sql_stmt := 'select table_name,tbl_pf from topo_face2_def where tbl_pf is not null order by tbl_pf';
  EXECUTE immediate sql_stmt bulk collect INTO vTblList,vColumnList;
  --
  -- loop thru table/column list
  --
  FOR i IN vTblList.FIRST..vTblList.LAST
  LOOP
   vCurTbl := vTblList(i);
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
    IF vCurColumn = 'PLACEFP' THEN
     sql_stmt := 'ALTER TABLE '||vFaceTable||' MODIFY '||vCurColumn||' VARCHAR2(' || vDataLength || ') default ''00000''';
     EXECUTE IMMEDIATE sql_stmt;
    END IF;
   END IF;
   -----------------------------------------------------------------------
   --
   -- create temp table
   sql_stmt := 'CREATE TABLE ' || vTmpTable || '  PARALLEL NOLOGGING AS SELECT t.topo_id AS face_id,TO_CHAR(sl.' || vCurColumn || ') AS ' || vCurColumn || ' FROM ' || vCurTbl || ' sl, TABLE(sl.topogeom.get_topo_elements()) t';
   EXECUTE IMMEDIATE sql_stmt;
   -- create index for temp table face_id
   sql_stmt := 'CREATE INDEX ' || vTmpTableIDX ||' ON ' || vTmpTable || '(face_id) parallel nologging';
   EXECUTE IMMEDIATE sql_stmt;
   -- remove duplicates from temp table
   sql_stmt := 'Insert into FACE2_POSTCLIP_FIX (ORIGINAL_FACE_ID) (select face_id from '|| vTmpTable ||' group by face_id having count(face_id) > 1)';
   EXECUTE IMMEDIATE sql_stmt;
   COMMIT;
   sql_stmt := 'DELETE FROM '|| vTmpTable ||'
                WHERE rowid NOT IN
                (
                   SELECT MIN(rowid)
                   FROM '|| vTmpTable ||'
                   GROUP BY face_id
                )';
   EXECUTE IMMEDIATE sql_stmt;
   COMMIT;
   -- update face2 table
   sql_stmt := 'UPDATE /*+ PARALLEL 4 */ ' || vFaceTable || ' f SET f.' || vCurColumn || ' = (SELECT t.' || vCurColumn || ' FROM ' || vTmpTable || ' t WHERE t.face_id = f.face_id) WHERE EXISTS (SELECT t.' || VCurColumn || ' FROM ' || vTmpTable || ' t WHERE t.face_id = f.face_id)';
   EXECUTE IMMEDIATE sql_stmt;
   COMMIT;
   -- drop temp table
   sql_stmt := 'DROP TABLE '||vTmpTable||' PURGE';
   EXECUTE IMMEDIATE sql_stmt;
  END LOOP;
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
     vGeoId := vGeoID || 'DECODE(f.' || vCurColumn || ',NULL,''____'',f.' || vCurColumn || ')||'',';
    END IF;
    IF vCnt2 > 1 AND vCnt2 < vCnt THEN
     -- middle of string
     vGeoId := vGeoId || '''||DECODE(f.' || vCurColumn || ',NULL,''____'',f.' || vCurColumn || ')||'',';
    END IF;
    IF vCnt2 = vCnt THEN
    -- end of string
     vGeoId := vGeoId || '''||DECODE(f.' || vCurColumn || ',NULL,''_____'',f.' || vCurColumn || ')';
    END IF;
   END LOOP;
   -- an update statement
   EXECUTE immediate vGeoId;
   COMMIT;
   --
 END IF;
END build_face2_cols;
--
PROCEDURE load_face2 (topology VARCHAR2,face_tbl VARCHAR2,face_dollar_tbl VARCHAR2,deploy VARCHAR2,release varchar2)
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
sql_stmt VARCHAR2(4000); -- Dynamic SQL Statement
vface VARCHAR2(100);
v_tg_layer_id NUMBER;
v_tg_layer_type VARCHAR2(10);
v_tg_lt NUMBER;
v_tbl_keys VARCHAR2(250);
cnt NUMBER;
BEGIN
 -- get tg_layer_id and tg_layer_type for particular topology and face_tbl
 sql_stmt := 'select tg_layer_id,tg_layer_type from user_sdo_topo_info where topology = :1 and table_name = :2';
 EXECUTE immediate sql_stmt INTO v_tg_layer_id,v_tg_layer_type USING topology,face_tbl;
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
  sql_stmt := 'INSERT INTO ' || face_tbl || ' VALUES (:1,SDO_TOPO_GEOMETRY(:2,:3,:4,SDO_TOPO_OBJECT_ARRAY(SDO_TOPO_OBJECT (:5,:6))))';
  EXECUTE immediate sql_stmt USING vface,topology,v_tg_lt,v_tg_layer_id,vface,v_tg_lt;
  COMMIT;
 END LOOP;
 --
 -- get face table keys; generally just face_id
 sql_stmt := 'select tbl_keys from topo_universe where topology = :1 and table_name = :2';
 EXECUTE immediate sql_stmt INTO v_tbl_keys USING topology,face_tbl;
 --
 -- create index on tbl_keys
 sql_stmt := 'create unique index ' || face_tbl || '_' || v_tbl_keys || '_uk on ' || face_tbl || '(' || v_tbl_keys || ') parallel nologging';
 EXECUTE immediate sql_stmt;
 --
 -- call procedure to build attribute and geo_id columns in FACE table
 GZ_TOPO_HELPER.build_face2_cols (face_tbl, face_dollar_tbl);
 --
 -- add sdogeometry field
 sql_stmt := 'alter table ' || face_tbl || ' add sdogeometry mdsys.sdo_geometry';
 EXECUTE immediate sql_stmt;
 --
 -- update sdogeometry field from topogeom
 sql_stmt := 'UPDATE /*+ PARALLEL 4 */ ' || face_tbl || ' a set a.sdogeometry = a.topogeom.get_geometry()';
 EXECUTE immediate sql_stmt;
 COMMIT;
 --
 -- insert entry into user_sdo_geom_metadata and create spatial index, if not previously completed
 sql_stmt := 'select count(*) from user_sdo_geom_metadata where table_name = :1 and column_name = ''SDOGEOMETRY''';
 EXECUTE immediate sql_stmt INTO cnt USING face_tbl;
 IF cnt = 0 THEN
  sql_stmt := 'insert into user_sdo_geom_metadata values (:1,''SDOGEOMETRY'',SDO_DIM_ARRAY (SDO_DIM_ELEMENT (''X'',-180,180,0.05),SDO_DIM_ELEMENT(''Y'',-90,90,0.05)), 8265)';
  EXECUTE immediate sql_stmt USING face_tbl;
  COMMIT;
  sql_stmt := 'create index ' || face_tbl || '_SIDX on ' || face_tbl || ' (SDOGEOMETRY) indextype is mdsys.spatial_index';
  EXECUTE immediate sql_stmt;
 END IF;
 --
 -- add columns to face
 sql_stmt := 'alter table ' || face_tbl || ' add (areatotal number, perimeter number, llx number, lly number, urx number, ury number, pa_ratio number, mbr mdsys.sdo_geometry,qc varchar2(100))';
 EXECUTE immediate sql_stmt;
 --
 -- add data to area total,perimeter,llx,lly,pa_ratio,mbr,urx,ury
 -- define perimeter and perimeter/area ratio; define first an MBR and then from it the Lower Left and Upper Right of the geometry
 sql_stmt := 'UPDATE /*+ PARALLEL 4 */  ' || face_tbl || ' set MBR = SDO_GEOM.SDO_MBR(SDOGEOMETRY)';
 EXECUTE IMMEDIATE sql_stmt;
 COMMIT;
 sql_stmt := 'UPDATE /*+ PARALLEL 4 */  ' || face_tbl || ' set areatotal = sdo_geom.sdo_area(sdogeometry,0.05),PERIMETER = SDO_GEOM.SDO_LENGTH(SDOGEOMETRY,0.05,''unit=meter''),LLX = SDO_GEOM.SDO_MIN_MBR_ORDINATE(MBR,1),LLY = SDO_GEOM.SDO_MIN_MBR_ORDINATE(MBR,2),URX = SDO_GEOM.SDO_MAX_MBR_ORDINATE(MBR,1),URY = SDO_GEOM.SDO_MAX_MBR_ORDINATE(MBR,2)';
 EXECUTE IMMEDIATE sql_stmt;
 COMMIT;
 sql_stmt := 'UPDATE /*+ PARALLEL 4 */  ' || face_tbl || ' set PA_RATIO = PERIMETER/SQRT(AREATOTAL)';
 EXECUTE IMMEDIATE sql_stmt;
 COMMIT;
 --
 -- create index on basic fields - statefp,countyfp,cousubfp,placefp
 -- NOTE: The code to create the following index needs to be made generic and therefore it is commented out
 sql_stmt := 'create index ' || face_tbl || '_idx on ' || face_tbl || ' (statefp,countyfp) parallel nologging';
 EXECUTE immediate sql_stmt;
END load_face2;
--
PROCEDURE load_topo_preface_2 (topology VARCHAR2,tbl_type VARCHAR2,release VARCHAR2,deploy VARCHAR2)
AS
/**
 ###################################################################################################################
 # Program Name: load_topo_preface_2 
 # Author: Salman 
 # Creation Date: 09/15/2009
 # Recent Revisions: 
 #
 # Purpose:
 #   The purpose of this procedure is to load the tg_level_id = 0 feature table for face2 table 
 #
 # Dependencies:
 #  - TOPO_UNIVERSE
 #  - TOPO_HIERARCHY
 #
 ###################################################################################################################
*/
v_process VARCHAR2(100) := 'Load Preface2 Feature Table for Face2'; -- Process
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
v_where_clause VARCHAR2(20) := 'STATEFP IS NOT NULL'; -- used so insert command doesn't have to be changed
mbr_geom SDO_GEOMETRY; -- minimum bond rectangle
statefp_tbl VARCHAR2(32);
v_statefp_tbl VARCHAR2(32);
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
 sql_stmt := 'select a.table_name,a.source_location,a.tbl_keys,b.seq_num from topo_universe a, topo_hierarchy b where a.table_name like :1 and a.table_type = :2 and a.topology = :3 and b.child_layer_tbl is null and a.topology = b.topology and a.table_name = b.table_name order by b.seq_num';
 EXECUTE immediate sql_stmt bulk collect INTO array_tbl_name,array_source_location,array_tbl_keys,array_seq_num USING 'PREFACE2%',tbl_type,topology;
 --
 -- Set sdo_topo_map maximum Java memory size
 sdo_topo_map.set_max_memory_size (2147483648);
 --
 -- Create topo map
 -- tracking
 v_step := 'Create Topo Map';
 v_start_time := systimestamp;
 --
 sdo_topo_map.create_topo_map(topology,'MTMAP',8000,4000,1500);
 --
 v_end_time := systimestamp;
 v_elapsed_time := v_end_time - v_start_time;
 sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
 EXECUTE immediate sql_stmt2 USING 'N/A',v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy;
 COMMIT;
 --
 -- set STATEFP table name
 v_statefp_tbl := v_release || '_SL040'; -- the 'SL040' is hardcoded and may have to be revised to search for the STATE table
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
    v_ord1 := v_ord1 - 1.0;
    v_ord2 := v_ord2 - 1.0;
    v_ord3 := v_ord3 + 1.0;
    v_ord4 := v_ord4 + 1.0;
  END IF;
  --
  v_cnt3 := NULL;
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
  EXECUTE immediate sql_stmt2 USING v_state,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy;
  COMMIT;
  -- Load topo map
  v_step := 'Load Topo Map';
  v_start_time := systimestamp;
  --
  sdo_topo_map.load_topo_map('MTMAP',v_ord1,v_ord2,v_ord3,v_ord4,'true');
  --
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
  EXECUTE immediate sql_stmt2 USING v_state,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy;
  COMMIT;
  -- Load table
  FOR i IN 1..array_seq_num.LAST
  LOOP
   v_tbl_name := array_tbl_name(i);
   v_source_location := array_source_location(i);
   v_tbl_keys := array_tbl_keys(i);
   v_seq_num := array_seq_num(i);
   v_tbl_name := array_tbl_name(i);
   v_step := 'Load feature table ' || v_tbl_name;
   -- check to see if the source table has the specific STATEFP
   sql_stmt := 'select count(rownum) from ' || v_source_location || ' where statefp = ''' || v_state_data || '''';
   EXECUTE immediate sql_stmt INTO v_cnt;
   IF v_cnt > 0 -- specific STATEFP exists in source table; if = 0 then it skips the load for the particular STATEFP
    THEN
     -- check if specific STATEFP is already loaded into feature table; ability to re-start an aborted load
     sql_stmt := 'select count(rownum) from ' || v_tbl_name || ' where statefp = ''' || v_state_data || '''';
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
      sql_stmt := 'declare topo_geom sdo_topo_geometry; BEGIN FOR load_rec IN (SELECT /*+ PARALLEL 4 */ ' || v_table_fields || ' FROM ' || v_source_location || ' where statefp = ''' || v_state_data || ''' and ' || v_where_clause || ')
             LOOP
              topo_geom := SDO_TOPO_MAP.CREATE_FEATURE(''' || topology || ''',''' || v_tbl_name || ''',''TOPOGEOM'',load_rec.sdogeometry);
              INSERT /*+ APPEND */ INTO ' || v_tbl_name || ' VALUES (' || v_load_table_fields || ',topo_geom);
             END LOOP; END;';
     EXECUTE immediate sql_stmt;
      --
      v_end_time := systimestamp;
      v_elapsed_time := v_end_time - v_start_time;
      sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
      EXECUTE immediate sql_stmt2 USING v_state,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy;
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
  commit; -- likely redundant
  --
  -- tracking
  v_end_time := systimestamp;
  v_elapsed_time := v_end_time - v_start_time;
  sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
  EXECUTE immediate sql_stmt2 USING v_state,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy;
  COMMIT;
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
  sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
  EXECUTE immediate sql_stmt2 USING v_state,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy;
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
 sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
 EXECUTE immediate sql_stmt2 USING v_state,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy;
 COMMIT;
 --
 -- end tracking
 --
 v_proc_end_time := systimestamp;
 v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
 sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
 EXECUTE immediate sql_stmt2 USING 'N/A','Created Topology','COMPLETED',v_proc_start_time,v_proc_end_time,v_proc_elapsed_time,v_release,'',v_deploy;
 COMMIT;
 --
 -- generic exception handling
 --
 EXCEPTION
 WHEN OTHERS THEN
  -- drop topo map
  sdo_TOPO_MAP.DROP_TOPO_MAP('MTMAP');
  -- error output
  dbms_output.put_line (SQLERRM);
  dbms_output.put_line (DBMS_UTILITY.format_error_backtrace);
  dbms_output.put_line (sql_stmt);
  -- tracking
  v_process := v_process || ' FAILED';
  v_step    := v_step || ' FAILED';
  sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
  EXECUTE immediate sql_stmt2 USING v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy;
  COMMIT;
  -- possible future use
  -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
  RETURN;
END load_topo_preface_2;
--
PROCEDURE load_topo_sl_1_up (topology VARCHAR2, sl_type VARCHAR2, release VARCHAR2, deploy VARCHAR2)
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
 -- get relevant information from TOPO_UNIVERSE and TOPO_HIERARCHY
 --
 -- set sl_type to query with a 'like' where clause
 v_sl_type := sl_type || '%';
 -- get relevant topo_universe and topo_hierarchy
 sql_stmt := 'select a.table_name,a.source_location,a.tbl_keys,a.deploy,a.release,b.seq_num from topo_universe a, topo_hierarchy b where a.table_name like :1 and a.table_type = :2 and a.topology = :3 and b.child_layer_tbl is not null and a.topology = b.topology and a.table_name = b.table_name order by b.seq_num';
 execute immediate sql_stmt bulk collect into array_tbl_name,array_source_location,array_tbl_keys,array_deploy,array_release,array_seq_num using v_sl_type,'F',topology;
 --execute immediate sql_stmt bulk collect into array_tbl_name,array_source_location,array_tbl_keys,array_deploy,array_release,array_seq_num using 'FSL010%','F',topology;
 --
 FOR i IN 1..array_seq_num.LAST
  LOOP
   v_tbl_name := array_tbl_name(i);
   v_source_location := array_source_location(i);
   v_tbl_keys := array_tbl_keys(i);
   v_deploy := array_deploy(i);
   v_release := array_release(i);
   v_seq_num := array_seq_num(i);
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
     sql_stmt := 'BEGIN FOR load_rec IN (SELECT ' || v_table_fields || ' FROM ' || v_source_location || ' where ' || v_where_clause_full || ')
          LOOP
           INSERT INTO ' || v_tbl_name || ' VALUES (' || v_load_table_fields || ',
           SDO_TOPO_MAP.CREATE_FEATURE(''' || topology || ''',''' || v_tbl_name || ''',''TOPOGEOM'',' || v_dml_condition || '));
          END LOOP; END;';
     EXECUTE immediate sql_stmt;
     COMMIT;
     --
    END LOOP;
   --
   v_end_time := systimestamp;
   v_elapsed_time := v_end_time - v_start_time;
   sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
   EXECUTE immediate sql_stmt2 USING v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,v_release,sql_stmt,v_deploy;
   COMMIT;
  END LOOP;
  --
  v_proc_end_time := systimestamp;
  v_proc_elapsed_time := v_proc_end_time - v_proc_start_time;
  sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
  EXECUTE immediate sql_stmt2 USING 'N/A', v_process,'COMPLETED', v_proc_start_time,v_proc_end_time, v_proc_elapsed_time, v_release, '', v_deploy;
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
  v_step := v_step || ' FAILED';
  sql_stmt2 := 'insert into gen_tracking values (:1,:2,:3,:4,:5,:6,:7,:8,:9)';
  EXECUTE immediate sql_stmt2 USING v_tbl_name,v_process,v_step,v_start_time,v_end_time,v_elapsed_time,release,sql_stmt || ' || ' || sqlerrm,deploy;
  COMMIT;
  -- possible future use
  -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
  RETURN;
END load_topo_sl_1_up;
--
PROCEDURE national_zone (pTopology VARCHAR2, ptarget_scale INTEGER DEFAULT  500000, pnice number default 1, pungentableName VARCHAR2) AS
/**
 ###################################################################################################################
 # Program Name: national_zone 
 # Author: Salman 
 # Recent Revision:
 #
 # Purpose:
 #   The purpose of this procedure is to call line simplification package 
 #    
 #
 # Required parameters:
 #  - topology - name of topology 
 #  - target_scale - target map scales 
 #  - nice -  line smoothness  
 #  - ungentablename - output table with ungeneralized edges
 #
 # Dependencies: 
 #  
 #
 ###################################################################################################################
*/
sql_stmt                        VARCHAR2(4000);
vstate                          VARCHAR2(2);
array_state                     MDSYS.STRING_ARRAY;
v_ord1                          number;
v_ord2                          number;
v_ord3                          number;
v_ord4                          number;
v_topology                      VARCHAR2(20) := UPPER(pTopology);
target_scale                    NUMBER := ptarget_scale;
nice                            NUMBER := pnice;
context_info                    SDO_KEYWORDARRAY := SDO_KEYWORDARRAY();
v_overlap_edges_table_name      VARCHAR2(30) := UPPER(pungentableName);
v_threshold                     number   := '500';
mbr_geom                        SDO_GEOMETRY;
BEGIN
  context_info.extend(3);           
  context_info(1) := 'SIDEYS_SIMPLIFY'; -- Name of custom simplify routine
  context_info(2) := target_scale;   
  context_info(3) := nice;  
  sql_stmt := 'SELECT distinct statefp FROM acs09_sl040 order by statefp';
  EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO array_state;
  sdo_topo_map.set_max_memory_size (2147483648);
  sdo_TOPO_MAP.CREATE_TOPO_MAP(v_topology, 'my_topo_map_cache', 100000, 100000, 100000);
    FOR c in 1..array_state.COUNT LOOP
        vstate := array_state(c);
        dbms_output.put_line(' processing state ' || vstate);
        sql_stmt := 'SELECT /*+ PARALLEL 4 */ sdo_geom.sdo_mbr(sdo_geom.sdo_buffer(sdo_aggr_mbr(sdogeometry), 500, .05)) 
                     FROM acs09_sl040 a WHERE a.statefp = :1';
        Execute Immediate sql_stmt into mbr_geom using vstate;
        v_ord1 := mbr_geom.sdo_ordinates(1);
        v_ord2 := mbr_geom.sdo_ordinates(2);
        v_ord3 := mbr_geom.sdo_ordinates(3);
        v_ord4 := mbr_geom.sdo_ordinates(4);
        sdo_TOPO_MAP.LOAD_TOPO_MAP('my_topo_map_cache',v_ord1,v_ord2,v_ord3,v_ord4, 'true'); 
        --generalize_util.simplify_region(v_topology,'my_topo_map_cache',v_threshold,v_ord1,v_ord2,v_ord3,v_ord4, max_edge_size=>100, overlap_edges_table_name=>v_overlap_edges_table_name,context_info => context_info);
        -- Commit TOPO map
        sdo_TOPO_MAP.COMMIT_TOPO_MAP();
        COMMIT;
        sdo_TOPO_MAP.CLEAR_TOPO_MAP('my_topo_map_cache');
    END LOOP; 
    sdo_TOPO_MAP.DROP_TOPO_MAP('my_topo_map_cache');
END national_zone;
--
PROCEDURE REMOVE_CLOSE_XYS_MASTER_HIGH (pInTable VARCHAR2, pInEdgeIdColumn VARCHAR2 default 'EDGE_ID',pInSDOGeomColumn VARCHAR2 default 'SDOGEOMETRY') AS
-- Prcedure to remove very close duplicate vertices from a table
-- (as close as 1/millionth of a degree);
   TYPE                 TblCursorType   IS REF CURSOR;
   Table_cursor         TblCursorType;
   Geometry_Array       GZ_TYPES.SDO_GEOMETRY_ARRAY := GZ_TYPES.SDO_GEOMETRY_ARRAY(); 
   Empty_GArray         GZ_TYPES.SDO_GEOMETRY_ARRAY := GZ_TYPES.SDO_GEOMETRY_ARRAY();
   Edge_ids             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   InTable              VARCHAR2(100)   :=    UPPER(pIntable);
   InEdgeIdColumn       VARCHAR2(100)   :=    UPPER(pInEdgeIdColumn);
   InSDOGeomColumn      VARCHAR2(100)   :=    UPPER(pInSDOGeomColumn);
   geom           MDSYS.SDO_GEOMETRY;
   loop_count     PLS_INTEGER := 0;
   update_count   PLS_INTEGER := 0;
   row_limit      PLS_INTEGER := 100;
   sql_stmt       VARCHAR2(4000);
   edge_id        NUMBER;
   ptolerance     NUMBER := 0.05;
   ok             BOOLEAN;
BEGIN
   sql_stmt := 'SELECT t.'||InEdgeIdColumn||',t.' || InSDOgeomColumn ||' from ' ||
                Intable||'  t  where ' ||' sdo_util.GetNumVertices(t.' ||InSDOgeomColumn ||') > 2';
   OPEN Table_cursor FOR sql_stmt;
    LOOP  
      loop_count := loop_count + 1;
-- Free memory to avoid a PGA memory spiral
      if loop_count <> 1 then         
         Geometry_Array := Empty_Garray;
         dbms_session.free_unused_user_memory;
      End if;
      FETCH Table_cursor BULK COLLECT INTO Edge_Ids,Geometry_ARRAY LIMIT ROW_LIMIT;
      Exit when Edge_ids.count = 0;
      For ii in 1..Geometry_Array.count Loop
          geom := Geometry_Array(ii);
          Edge_id := Edge_ids(ii);
          ok := GZ_TOPO_HELPER.REMOVE_CLOSE_XYS_HIGH(Geom,ptolerance);
-- We found one that has a very close coordinate
          if ok = TRUE then
             update_count := update_count + 1;
             EXECUTE IMMEDIATE 'UPDATE ' || Intable || ' SET '|| InSDOgeomColumn || '= :1 WHERE '
               ||InEdgeIdColumn|| ' = :2' Using GEOM,Edge_id;
            dbms_output.put_line('Edge_id ' || Edge_id || ' was updated');
          end if;
      End Loop;
  END LOOP;
      commit;
  dbms_output.put_line('Updates: ' || update_count);
END REMOVE_CLOSE_XYS_MASTER_HIGH;
--
PROCEDURE REMOVE_CLOSE_XYS_MASTER_HIGH2 (
 pInTable VARCHAR2,
 pPrimaryKeyColumn VARCHAR2 default 'GEO_ID',
 pInSDOGeomColumn VARCHAR2 default 'SDOGEOMETRY'
) AS
-- Prcedure to remove very close duplicate vertices from a table
-- (as close as 1/millionth of a degree);
-- same as REMOVE_CLOSE_XYS_MASTER_HIGH, except you can loop on a string
-- instead of a number
   TYPE                 TblCursorType   IS REF CURSOR;
   Table_cursor         TblCursorType;
   Geometry_Array       GZ_TYPES.SDO_GEOMETRY_ARRAY := GZ_TYPES.SDO_GEOMETRY_ARRAY(); 
   Empty_GArray         GZ_TYPES.SDO_GEOMETRY_ARRAY := GZ_TYPES.SDO_GEOMETRY_ARRAY();
   --Edge_ids             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   vTable              VARCHAR2(100)   :=    UPPER(pIntable);
   vPkColumn           VARCHAR2(100)   :=    UPPER(pPrimaryKeyColumn);
   vSDOGeomColumn      VARCHAR2(100)   :=    UPPER(pInSDOGeomColumn);
   geom           MDSYS.SDO_GEOMETRY;
   loop_count     PLS_INTEGER := 0;
   update_count   PLS_INTEGER := 0;
   row_limit      PLS_INTEGER := 100;
   sql_stmt       VARCHAR2(4000);
   -- edge_id        NUMBER;
   vPKs            GZ_TYPES.STRINGARRAY;
   vPrimaryKey    VARCHAR2(4000);
   ptolerance     NUMBER := 0.05;
   ok             BOOLEAN;
BEGIN
   sql_stmt := 'SELECT t.'||vPkColumn  ||',t.' || vSDOGeomColumn ||' from ' ||
                vTable||'  t  where ' ||' sdo_util.GetNumVertices(t.' ||vSDOGeomColumn ||') > 2';
   OPEN Table_cursor FOR sql_stmt;
    LOOP  
      loop_count := loop_count + 1;
-- Free memory to avoid a PGA memory spiral
      if loop_count <> 1 then         
         Geometry_Array := Empty_Garray;
         dbms_session.free_unused_user_memory;
      End if;
      FETCH Table_cursor BULK COLLECT INTO vPks,Geometry_ARRAY LIMIT ROW_LIMIT;
      Exit when vPks.count = 0;
      For ii in 1..Geometry_Array.count Loop
          geom := Geometry_Array(ii);
          vPrimaryKey := vPks(ii);
          ok := GZ_TOPO_HELPER.REMOVE_CLOSE_XYS_HIGH(Geom,ptolerance);
-- We found one that has a very close coordinate
          if ok = TRUE then
             update_count := update_count + 1;
             EXECUTE IMMEDIATE 'UPDATE ' || vTable || ' SET '|| vSDOgeomColumn || '= :1 WHERE '
               ||vPKColumn|| ' = :2' Using GEOM,vPrimaryKey;
            dbms_output.put_line('Primary Key ' || vPrimaryKey || ' was updated');
          end if;
      End Loop;
  END LOOP;
      commit;
  dbms_output.put_line('Updates: ' || update_count);
END REMOVE_CLOSE_XYS_MASTER_HIGH2;
/*
PROCEDURE REMOVE_CLOSE_XYS_MASTER_LOW (pInTable VARCHAR2, pInEdgeIdColumn VARCHAR2 default 'EDGE_ID',pInSDOGeomColumn VARCHAR2 default 'SDOGEOMETRY') AS
-- Prcedure to remove very close duplicate vertices from a table
-- (as close as 1/millionth of a degree);
   TYPE                 TblCursorType   IS REF CURSOR;
   Table_cursor         TblCursorType;
   Geometry_Array       CPB_TYPES.SDO_GEOMETRY_ARRAY := CPB_TYPES.SDO_GEOMETRY_ARRAY(); 
   Empty_GArray         CPB_TYPES.SDO_GEOMETRY_ARRAY := CPB_TYPES.SDO_GEOMETRY_ARRAY();
   Edge_ids             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
   InTable              VARCHAR2(100)   :=    UPPER(pIntable);
   InEdgeIdColumn       VARCHAR2(100)   :=    UPPER(pInEdgeIdColumn);
   InSDOGeomColumn      VARCHAR2(100)   :=    UPPER(pInSDOGeomColumn);
   geom           MDSYS.SDO_GEOMETRY;
   loop_count     PLS_INTEGER := 0;
   update_count   PLS_INTEGER := 0;
   row_limit      PLS_INTEGER := 100;
   sql_stmt       VARCHAR2(4000);
   edge_id        NUMBER;
   ptolerance     NUMBER := 0.5;
   ok             BOOLEAN;
BEGIN
   sql_stmt := 'SELECT t.'||InEdgeIdColumn||',t.' || InSDOgeomColumn ||' from ' ||
                Intable||'  t  where ' ||' sdo_util.GetNumVertices(t.' ||InSDOgeomColumn ||') > 2';
   OPEN Table_cursor FOR sql_stmt;
    LOOP  
      loop_count := loop_count + 1;
-- Free memory to avoid a PGA memory spiral
      if loop_count <> 1 then         
         Geometry_Array := Empty_Garray;
         dbms_session.free_unused_user_memory;
      End if;
      FETCH Table_cursor BULK COLLECT INTO Edge_Ids,Geometry_ARRAY LIMIT ROW_LIMIT;
      Exit when Edge_ids.count = 0;
      For ii in 1..Geometry_Array.count Loop
          geom := Geometry_Array(ii);
          Edge_id := Edge_ids(ii);
          ok := TOPO_UTIL.REMOVE_CLOSE_XYS_LOW(Geom,ptolerance);
-- We found one that has a very close coordinate
          if ok = TRUE then
             update_count := update_count + 1;
             EXECUTE IMMEDIATE 'UPDATE ' || Intable || ' SET '|| InSDOgeomColumn || '= :1 WHERE '
               ||InEdgeIdColumn|| ' = :2' Using GEOM,Edge_id;
            dbms_output.put_line('Edge_id ' || Edge_id || ' was updated');
          end if;
      End Loop;
  END LOOP;
      commit;
  dbms_output.put_line('Updates: ' || update_count);
END REMOVE_CLOSE_XYS_MASTER_LOW;
*/
--
PROCEDURE DROP_FSL_TABLES(pTopology VARCHAR2)
AS
/*
 helper procedure to drop all '<topology>_FSL%' tables from a topology
 Added by Stephanie 8/28/2010
*/
vsql varchar2(4000);
vtopo varchar2(4000) := UPPER(pTopology);
vtables gz_types.stringarray;
-- before re-running FSL build
-- DEREGISTER all FSL tables
BEGIN
vsql := 'SELECT table_name from user_sdo_topo_info where topology = '''||
         vTopo||''' AND TABLE_NAME LIKE '''||vTopo||'_FSL%''';
dbms_output.put_line('**** Initial SQL = '||vSql);
EXECUTE IMMEDIATE vSQL BULK COLLECT INTO vtables;
-- DEREGISTER and DROP ALL FSL TABLES
dbms_output.put_line('**** FSL tables found = '||vtables.COUNT);
for i IN 1..vtables.COUNT LOOP
   dbms_output.put_line('**** de-registering '||vtables(i));
   sdo_topo.delete_topo_geometry_layer(vTopo,vtables(i),'TOPOGEOM'); 
   commit;
   dbms_output.put_line('**** dropping '||vtables(i));
   vsql := 'DROP TABLE '||vtables(i)||' cascade constraints purge';
   commit;
   EXECUTE IMMEDIATE vSQL;
END LOOP;
   dbms_output.put_line('**** finished removing FSL tables for '||vtopo);
END DROP_FSL_TABLES;
--
FUNCTION REMOVE_CLOSE_XYS_HIGH (Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,ptolerance NUMBER default 0.05) RETURN BOOLEAN Deterministic IS
/**
 ################################################################################
 # Program Name: Remove_Close_Xys
 # Author: Sidey Timmins
 # Creation Date: 7/22/2008
 #
 # Usage:
 #   This PL/SQL procedure has 2 parameters:
 #                   Geom: the Geometry to check and fix.
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
    sql_stmt          VARCHAR2(4000);
    ii       PLS_INTEGER;
    xlast    NUMBER;
    ylast    NUMBER;
    xnew     NUMBER;
    ynew     NUMBER;
    k        PLS_INTEGER := -1;
    ycheck   NUMBER;
    xcheck   NUMBER;
    distance NUMBER := 0.;
    rings    PLS_INTEGER;
    LB          PLS_INTEGER;
    UB          PLS_INTEGER;
    j           PLS_INTEGER;
    tolerance NUMBER := ptolerance;
    tolerance2 NUMBER;
    epsilon    NUMBER := 0.0000005;
    GTYPE    NUMBER;
    SRID     NUMBER;
-- Written as a procedure so as NOT to create new geometry objects and waste memory
BEGIN
   GTYPE := geom.SDO_GTYPE;
   If Gtype = 2001 or Gtype = 2004 then
      RETURN FALSE;
   End If;
-- Detect dups because most edges are ok
    Info_Array := geom.SDO_ELEM_INFO;
    XYORD := geom.SDo_Ordinates;
    If ptolerance <= 0. then
       tolerance := 0.05;
    End if;
-- These parameters were tested from 0 to 72 degrees North with random coordinates
-- to check that this procedure got the same results as the Oracle function
-- SDO_UTIL.REMOVE_DUPLICATE_VERTICES. Note that the Oracle function may change the
-- coordinates! 
    tolerance2 := tolerance *1.01;
    ylast := XYOrd(2);
    if ylast > 50. then
      xcheck   := 0.0000016 * tolerance/0.05;
      ycheck   := 0.0000005 * tolerance/0.05;
    else
      xcheck   := 0.00000075 * tolerance/0.05;
      ycheck   := 0.00000065 * tolerance/0.05;
    End If;
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
--       If abs(xnew - xlast) < xcheck and abs(ynew - ylast) < ycheck then
          If abs(xnew - xlast) <= 0.00000125 and abs(ynew - ylast) <= 0.00000125 then
--          distance := CDB_UTIL.fast_vincenty_gcd(xnew,ynew,xlast,ylast,'m');
-- Possible candidate for removal
--          if distance < tolerance2 then
--            geometry := SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),
--                       SDO_ORDINATE_ARRAY(xnew,ynew,xlast,ylast));
--            sql_stmt := 'SELECT SDO_GEOM.SDO_LENGTH(:1,0.000000000005,''unit=meter'') from DUAL';
--            EXECUTE IMMEDIATE sql_stmt into distance using geometry; 
--          End If;
-- drop it if within tolerance (equal or less)
          if distance <= tolerance and jj <>UB then
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
          End If;
       Else
-- usual case - wide apart.
         k := k+2;
-- Store coordinates
         XYOrd(k) := xnew;
         XYOrd(k+1) := ynew;
         xlast := xnew;
         ylast := ynew;
       End If;
    End Loop;    
    END LOOP;
    k := k + 1;
    If k <> XYOrd.count then
--       dbms_output.put_line('K ' || k || ' XYOrd.count ' || XYOrd.count);
       XYOrd.trim(XYOrd.count-k);    
       SRID := geom.SDO_SRID;
       geom := MDSYS.SDO_GEOMETRY(GTYPE,SRID,NULL,Info_Array,XYOrd);
       RETURN TRUE;
    ELSE
       RETURN FALSE;
    End If;
---EXCEPTION
--   WHEN OTHERS THEN
---      CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
END REMOVE_CLOSE_XYS_HIGH;
/**
FUNCTION REMOVE_CLOSE_XYS_LOW (Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,ptolerance NUMBER default 0.05) RETURN BOOLEAN Deterministic IS
--################################################################################
--# Program Name: Remove_dupXys
--# Author: Sidey Timmins
--# Creation Date: 7/22/2008
--#
--# Usage:
--#   This PL/SQL procedure has 2 parameters:
--#                   Geom: the Geometry to check and fix.
--#   Returns TRUE only when the geometry is changed;
--#
--# Purpose: This procedure removes vertices which are too close.
--#          The resolution for our geodetic coordinates is 1 millionth of a degree.
--#          A longitude difference of 1.E-06 degrees at 64 degrees North with the
--#          same latitude is 0.047 meters and less than the 0.05 meter tolerance.
--#
--# Method: Filters vertices to determine the ones to remove.         
--# Dependencies:
--#  CDB_UTIL.fast_vincenty_gcd
--################################################################################
    XYOrd             MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
    Info_Array        MDSYS.SDO_ELEM_INFO_ARRAY;
    geometry          MDSYS.SDO_GEOMETRY;
    sql_stmt          VARCHAR2(4000);
    ii       PLS_INTEGER;
    xlast    NUMBER;
    ylast    NUMBER;
    xnew     NUMBER;
    ynew     NUMBER;
    k        PLS_INTEGER := -1;
    ycheck   NUMBER;
    xcheck   NUMBER;
    distance NUMBER := 0.;
    rings    PLS_INTEGER;
    LB          PLS_INTEGER;
    UB          PLS_INTEGER;
    j           PLS_INTEGER;
    tolerance NUMBER := ptolerance;
    tolerance2 NUMBER;
    epsilon    NUMBER := 0.0000005;
    GTYPE    NUMBER;
    SRID     NUMBER;
-- Written as a procedure so as NOT to create new geometry objects and waste memory
BEGIN
   SRID  := geom.SDO_SRID;
   GTYPE := geom.SDO_GTYPE;
   If Gtype = 2001 or Gtype = 2004 then
      RETURN FALSE;
   End If;
-- Detect dups because most edges are ok
    Info_Array := geom.SDO_ELEM_INFO;
    XYORD := geom.SDo_Ordinates;
    If ptolerance <= 0. then
       tolerance := 0.05;
    End if;
-- These parameters were tested from 0 to 72 degrees North with random coordinates
-- to check that this procedure got the same results as the Oracle function
-- SDO_UTIL.REMOVE_DUPLICATE_VERTICES. Note that the Oracle function may change the
-- coordinates! 
    tolerance2 := tolerance *1.01;
    If SRID = 8265 THEN
      ylast := XYOrd(2);
      if ylast > 50. then
        xcheck   := 0.0000016 * tolerance/0.05;
        ycheck   := 0.0000005 * tolerance/0.05;
      else
        xcheck   := 0.00000075 * tolerance/0.05;
        ycheck   := 0.00000065 * tolerance/0.05;
      End If;
    ELSE
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
       If abs(xnew - xlast) < xcheck and abs(ynew - ylast) < ycheck then
--          If abs(xnew - xlast) <= 0.00000125 and abs(ynew - ylast) <= 0.00000125 then
-- Possible candidate for removal
           If SRID = 8265 then
               distance := CDB_UTIL.fast_vincenty_gcd(xnew,ynew,xlast,ylast,'m');
               if distance < tolerance2 then
                  geometry := SDO_GEOMETRY(2002,SRID,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),
                             SDO_ORDINATE_ARRAY(xnew,ynew,xlast,ylast));
                  sql_stmt := 'SELECT SDO_GEOM.SDO_LENGTH(:1,0.000000000005,''unit=meter'') from DUAL';
                  EXECUTE IMMEDIATE sql_stmt into distance using geometry; 
               End If;
           Else
               distance := SQRT ((xnew-xlast) * (xnew-xlast) + (ynew-ylast) * (ynew-ylast));
             --  if distance < tolerance2 then
                --  geometry := SDO_GEOMETRY(2002,SRID,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),
               --              SDO_ORDINATE_ARRAY(xnew,ynew,xlast,ylast));
                --  sql_stmt := 'SELECT SDO_GEOM.SDO_LENGTH(:1,0.000000000005,''unit=meter'') from DUAL';
              --    EXECUTE IMMEDIATE sql_stmt into distance using geometry; 
            --   End If;
           End If;
-- drop it if within tolerance (equal or less)
          if distance <= tolerance and jj <>UB then
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
          End If;
       Else
-- usual case - wide apart.
         k := k+2;
-- Store coordinates
         XYOrd(k) := xnew;
         XYOrd(k+1) := ynew;
         xlast := xnew;
         ylast := ynew;
       End If;
    End Loop;    
    END LOOP;
    k := k + 1;
    If k <> XYOrd.count then
       XYOrd.trim(XYOrd.count-k);    
       SRID := geom.SDO_SRID;
       geom := MDSYS.SDO_GEOMETRY(GTYPE,SRID,NULL,Info_Array,XYOrd);
       RETURN TRUE;
    ELSE
       RETURN FALSE;
    End If;
--EXCEPTION
   --WHEN OTHERS THEN
     -- CDB_UTIL.update_cdb_code_log(SQLERRM,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
END REMOVE_CLOSE_XYS_LOW;
*/
--
PROCEDURE post_face2 (pFaceTable VARCHAR2, pFixTable VARCHAR2)
AS
/**
 ################################################################################################################### 
 # Program Name: post_face2  
 # Author: Salman 
 # Creation Date: 10/18/2009
 # Recent Revisions:
 #
 # Purpose: 
 #  The purpose of this procedure is to remove incorrect attributes after clipping 
 # 
 # Required parameters:
 #
 # Dependencies: 
 #
 ################################################################################################################### 
*/
sql_stmt        VARCHAR2(4000);
array_orig      MDSYS.SDO_NUMBER_ARRAY;
vColumnList     MDSYS.STRING_ARRAY;
vCurColumn      VARCHAR2(30);
vorig           NUMBER;
vFaceTable      VARCHAR2(50) := UPPER(pFaceTable);  
vFixTable       VARCHAR2(50) := UPPER(pFixTable);
BEGIN
    sql_stmt := 'Select column_name From user_tab_columns a Where a.table_name = :1 And a.column_name NOT IN (''FACE_ID'',''TOPOGEOM'',''SDOGEOMETRY'',''AREATOTAL'',''PERIMETER'',''LLX'',''LLY'',''URX'',''URY'',''PA_RATIO'',''MBR'',''QC'')';
    EXECUTE immediate sql_stmt bulk collect INTO vColumnList USING vFaceTable;
    sql_stmt := 'Select ORIGINAL_FACE_ID  From '||vFixTable||' Order By ORIGINAL_FACE_ID';
    EXECUTE immediate sql_stmt bulk collect INTO array_orig;
    FOR i in 1..array_orig.COUNT LOOP
        vorig := array_orig(i);
        dbms_output.put_line (vorig || ' Processing');
        --
        -- loop thru table/column list
        --
        FOR i IN vColumnList.FIRST..vColumnList.LAST
        LOOP
            vCurColumn := vColumnList(i);
            sql_stmt := 'UPDATE /*+ PARALLEL 4 */ ' || vFaceTable || ' f SET f.' || vCurColumn || ' = ''''
                         WHERE f.face_id = '||vorig||'';
            EXECUTE IMMEDIATE sql_stmt;
        END LOOP;
    END LOOP;
    COMMIT;
END post_face2;
--
PROCEDURE face_postclip_fix (pTopology VARCHAR2, pFaceTable VARCHAR2, pFixTable VARCHAR2) 
AS
/**
 ################################################################################################################### 
 # Program Name: face_postclip_fix 
 # Author: Salman 
 # Creation Date: 10/18/2009
 # Recent Revisions:
 #
 # Purpose: 
 #  The purpose of this procedure is to assign attributes to faces after clipping 
 # 
 # Required parameters:
 #
 # Dependencies: 
 #
 ################################################################################################################### 
*/
sql_stmt        VARCHAR2(4000);
array_orig      MDSYS.SDO_NUMBER_ARRAY;
array_attr      MDSYS.SDO_NUMBER_ARRAY;
vColumnList     MDSYS.STRING_ARRAY;
vCurColumn      VARCHAR2(30);
vorig           NUMBER;
vattr           NUMBER;
vTopology       VARCHAR2(50) := UPPER(pTopology);
vFaceTable      VARCHAR2(50) := UPPER(pFaceTable);  
vFixTable       VARCHAR2(50) := UPPER(pFixTable);
BEGIN
    sql_stmt := 'Select column_name From user_tab_columns a Where a.table_name = :1 And a.column_name NOT IN (''FACE_ID'',''TOPOGEOM'',''SDOGEOMETRY'',''AREATOTAL'',''PERIMETER'',''LLX'',''LLY'',''URX'',''URY'',''PA_RATIO'',''MBR'',''QC'')';
    EXECUTE immediate sql_stmt bulk collect INTO vColumnList USING vFaceTable;
    sql_stmt := 'Select ORIGINAL_FACE_ID, GET_ATTRIBUTES_FROM_FACE From '||vFixTable||' Order By ORIGINAL_FACE_ID';
    EXECUTE immediate sql_stmt bulk collect INTO array_orig, array_attr;
    FOR i in 1..array_orig.COUNT LOOP
        vorig := array_orig(i);
        vattr := array_attr(i);
        dbms_output.put_line (vorig || ' Processing');
        --
        -- loop thru table/column list
        --
        FOR i IN vColumnList.FIRST..vColumnList.LAST
        LOOP
            vCurColumn := vColumnList(i);
            sql_stmt := 'UPDATE /*+ PARALLEL 4 */ ' || vFaceTable || ' f SET f.' || vCurColumn || ' = (SELECT t.' || vCurColumn || ' FROM ' || vFaceTable || ' t WHERE t.face_id = '||vattr||')
                         WHERE f.face_id = '||vorig||'';
            EXECUTE IMMEDIATE sql_stmt;
        END LOOP;
    END LOOP;
    COMMIT;
END face_postclip_fix;
--
FUNCTION count_occurs (str varchar2, search varchar2 := null) return number
is
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
 #
 # Dependencies: 
 #   None
 #
 ################################################################################################################### 
*/
begin
if (search is null ) then 
 return length(str);
  else
 return (length(str) - nvl(length(replace(str,search,'')),0))/length(search);
end if;
end count_occurs;
--
FUNCTION create_dml_condition (tbl_keys IN VARCHAR2, tbl_keys_data IN VARCHAR2) RETURN VARCHAR2
IS
  dml_condition VARCHAR2(4000);
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
  #   The DML_CONDITION result for 1, 2 or 3 keys MUST match:
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
  -- first table_key comma
  v_hack_left1  NUMBER;
  -- second table_key comma
  v_hack_right1 NUMBER;
  -- third table_key comma
  v_hack_right1a NUMBER;
  -- first table_key_data comma
  v_hack_left2  NUMBER;
  -- second table_key data comma
  v_hack_right2 NUMBER;
  -- third table_key data comma
  v_hack_right2a NUMBER;
  -- key related
  v_key1  VARCHAR2(1000);
  v_key2  VARCHAR2(1000);
  v_key3  VARCHAR2(1000);
  v_key4  VARCHAR2(1000);
  v_key1a VARCHAR2(1000);
  v_key2a VARCHAR2(1000);
  v_key3a VARCHAR2(1000);
  v_key4a VARCHAR2(1000);
  v_apos  VARCHAR2(1) := ''''; -- apostrophies used in construction of dml_condition
BEGIN
  -- split table keys
  -- get count of commas
  comma_cnt     := count_occurs (tbl_keys,',');
  --
  -- table_keys
  -- locate first comma
  v_hack_left1  := instr(tbl_keys,',',1,1);
  -- located second comma
  v_hack_right1 := instr(tbl_keys,',',v_hack_left1 + 1,1);
  -- locate third comma
  v_hack_right1a := instr(tbl_keys,',',v_hack_right1 + 1,1);
  --
  -- table_keys_data
  -- locate first comma
  v_hack_left2  := instr(tbl_keys_data,',',1,1);
  -- locate second comma
  v_hack_right2 := instr(tbl_keys_data,',',v_hack_left2 + 1,1);
  -- locate third comma
  v_hack_right2a := instr(tbl_keys_data,',',v_hack_right2 + 1,1);
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
 -- return dml_condition value
  RETURN dml_condition;
END create_dml_condition;
--
FUNCTION create_ft_cols (table_name IN varchar2) RETURN VARCHAR2 IS v_table_fields VARCHAR2(4000);
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
array_col mdsys.string_array;     -- array of columns
v_col varchar2(32);               -- column
sql_stmt varchar2(4000);          -- Dynamic SQL Statement
begin
 -- obtain column names
 sql_stmt := 'select column_name from cols where table_name = :1 and column_name <> ''TOPOGEOM'' order by column_id';
 execute immediate sql_stmt bulk collect into array_col using table_name;
 -- process column names from multiple lines into a single line
 v_table_fields := '';
 for i in 1..array_col.LAST loop
  v_col := array_col(i);
  v_table_fields := v_table_fields || ',' || v_col;
 end LOOP;
 -- remove last comma
 v_table_fields := substr(v_table_fields,2,3999);
 return v_table_fields;
end;
--
PROCEDURE DUPLICATE_FACE_TABLE (
    pTopology              VARCHAR2,
    pFaceFeatureTable      VARCHAR2,
    pNewFaceFeatureTable   VARCHAR2
) AS
   -- Stephanie 9/3/2010
   -- procedure to copy a face feature table
   -- face feature tables are zero-level polygon topology tables
   -- with a one-to-one relationship to the <topology>_face$ table
   -- They can get messed up because of an Oracle bug if they end up with 
   -- TG_LAYER_ID = 3.  Sometimes we need to maek a copy to get a new tg_layer_id 
   -- so we can do things like get_geometry() without hitting a CONNECT BY LOOP error.
   -- parameters:  
   -- pTopology = name of the topology
   -- pFaceFeatureTable = old face feature table in the topology
   -- pNewFaceFEatureTable = what to call the new feature table
   -- USAGE NOTES:  
   --             1) assumes the face feature table has a primary key called "face_id"
   --             2) assumes the topology geometry column is called "topogeom"
   --             
   -- WATCH OUT hard-coded columns for School Districts -- MAKE THIS DYNAMIC!!!!
   --  need to update to read from a table of attribute values to copy, or select the columns from user_tab_columns
   vTopo varchar2(20) := UPPER(pTopology);
   vOldFace varchar2(100) := UPPER(pFaceFeatureTable);
   vNewFace varchar2(100) := UPPER(pNewFaceFeatureTable);
   vTGLayerId NUMBER;
   vMsg varchar2(4000) := 'Duplicate Face Table: ';
   vSql varchar2(4000);
   TYPE numberarray IS TABLE OF number
   INDEX BY PLS_INTEGER;
   vfaces numberarray;
   BEGIN
      -- 1) create table 
        vsql := 'create table '||vnewface||
                ' ( face_id number PRIMARY KEY, '||
                'topogeom SDO_TOPO_GEOMETRY ) noparallel nologging ';
        EXECUTE IMMEDIATE vsql;
        dbms_output.put_line (vMsg||vnewface || ' table created.');
      -- 2) register table
        sdo_topo.add_topo_geometry_layer(vtopo, vnewface, 'TOPOGEOM', 'POLYGON');
        commit;
        dbms_output.put_line (vMsg||'Feature Table: ' || vnewface ||
                            ' registered to topology ' || vtopo);
      -- 3) get tg_layer_id 
        vsql := 'select tg_layer_id from user_sdo_topo_info where table_name = :p1';
        EXECUTE IMMEDIATE vsql INTO vtglayerid USING vnewface;
      -- 4) add each face from face$ into the new face feature table 
      -- loop through each face id and add the face$ topo element to the topogeom
      vsql := 'select face_id from '||vtopo||'_FACE$ where face_id <> -1';
      EXECUTE IMMEDIATE vsql BULK COLLECT INTO vfaces;
      FOR i IN 1..vfaces.COUNT
      LOOP
            vsql := 'insert into '||vnewface||
                    ' VALUES ('||
                     vfaces(i)||', '||
                    'SDO_TOPO_GEOMETRY(:p1,:p2,:p3,'||
                                       'SDO_TOPO_OBJECT_ARRAY ('||
                                                               'SDO_TOPO_OBJECT(:p4,3)'||
                                                              ')'||
                                    ' ) '||
                            ')';
           EXECUTE IMMEDIATE vsql USING vtopo,
                                        3,
                                        vtglayerid,
                                        vfaces(i);
          COMMIT;
      END LOOP;
      dbms_output.put_line (vMsg||'Updated topogeom records in ' || vnewface );
      -- 5) Add all the attribute fields from old face to new face 
      vSql := 'ALTER table '||vnewface||' ADD ('||
              'AIANNHCE VARCHAR2(4000),'||
              'CBSAFP VARCHAR2(4000),'||
              'CDFP VARCHAR2(4000),'||
              'COUNTYFP VARCHAR2(4000),'||
              'COUSUBFP VARCHAR2(4000),'||
              'CSAFP VARCHAR2(4000),'||
              'METDIVFP VARCHAR2(4000),'||
              'PLACEFP VARCHAR2(4000),'||
              'SDDOE_E VARCHAR2(4000),'||
              'SDDOE_S VARCHAR2(4000),'||
              'SDDOE_U VARCHAR2(4000),'||
              'STATEFP VARCHAR2(4000),'||
              'UACE VARCHAR2(4000),'||
              'GEOID VARCHAR2(4000),'||
              'SDOGEOMETRY SDO_GEOMETRY,'||
              'AREATOTAL NUMBER,'||
              'PERIMETER NUMBER,'||
              'LLX NUMBER,'||
              'LLY NUMBER,'||
              'URX NUMBER,'||
              'URY NUMBER,'||
              'PA_RATIO NUMBER,'||
              'MBR SDO_GEOMETRY,'||
              'QC NUMBER)';
      dbms_output.put_line (vMsg||'Debugging: alter table statement = ''' || vsql||'''' );
      EXECUTE IMMEDIATE vsql;
      dbms_output.put_line (vMsg||'Added columns to ' || vnewface );
      -- Step 6: copy values based on face_id
      For ii IN 1..vFaces.Count
      LOOP
           vSql := 'UPDATE '||vnewface||
                   ' SET '||
              'AIANNHCE = (select AIANNHCE from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'CBSAFP = (select CBSAFP  from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'CDFP = (select CDFP from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'COUNTYFP = (select COUNTYFP from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'COUSUBFP = (select COUSUBFP from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'CSAFP = (select CSAFP from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'METDIVFP = (select METDIVFP from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'PLACEFP = (select PLACEFP from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'SDDOE_E = (select SDDOE_E from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'SDDOE_S = (select SDDOE_S from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'SDDOE_U = (select SDDOE_U from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'STATEFP = (select STATEFP from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'UACE = (select UACE from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'GEOID = (select GEOID from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'SDOGEOMETRY = (select SDOGEOMETRY from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'AREATOTAL = (select AREATOTAL from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'PERIMETER = (select PERIMETER from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'LLX = (select LLX from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'LLY = (select LLY from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'URX = (select URX from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'URY = (select URY from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'PA_RATIO = (select PA_RATIO from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'MBR = (select MBR from '||voldface||' where face_id = '||vFaces(ii)||'),'||
              'QC = (select QC from '||voldface||' where face_id = '||vFaces(ii)||') '||
             ' WHERE face_id = :p1';
           EXECUTE IMMEDIATE vSql USING vFaces(ii);
           COMMIT;
      END LOOP;
      dbms_output.put_line (vMsg||'Copied old face values from '||voldface ||' to ' || vnewface );
      dbms_output.put_line (vMsg||'Finished creating new face table in '||vTopo );
    END DUPLICATE_FACE_TABLE;
------------------------------------------
END GZ_TOPO_HELPER;
/


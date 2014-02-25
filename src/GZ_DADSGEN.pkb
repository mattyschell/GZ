CREATE OR REPLACE PACKAGE BODY GZ_DADSGEN
AS

   --This package is intended for code specifically needed in the CPB 
   --   generalization DADSGEN reference schema
   --Primary purpose (9/18/13) of the code is to generate a view called DADSFILE 
   --  that is agnostic about the existence of tables from which it selects
   
   -----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   
   FUNCTION PIPE_DADSFILE (
      table_name           IN VARCHAR2,
      where_clause         IN VARCHAR2 DEFAULT NULL,
      substr_unit          IN VARCHAR2 DEFAULT 'N',
      concat_diskfile      IN VARCHAR2 DEFAULT 'N',
      date_time_other      IN VARCHAR2 DEFAULT 'N',
      rerun_is_null        IN VARCHAR2 DEFAULT 'N'
   ) RETURN GZ_DADSGEN.DADSFILE_TAB PIPELINED
   AS
   
      --Matt! 9/18/13
      
      --In current generalization usage there are several calls to this function
      --   from the view definition, returning ~10,000 records
      --If we ever get into millions of records and this view is called frequently
      --   we'll need to take a look at performance.  For now we are snappy
      
      --Param Rundown
      
      -- table_name: Table name
      -- where_clause: Apply to the table. Example - 'shp_type=''PRD'' AND unit LIKE ''nation'''
      -- substr_unit: Y or N. Y = Apply SUBSTR (UNIT, 3) to the unit value. Turns 'st01' into '01'
      -- concat_diskfile: Y or N. Y = Build diskfile as 'OUTPUT_DIR || '/' || SHP_NAME
      -- date_time_other: Y or N. Y = Build date_time as SHP_CREATION_DATE
      -- rerun_is_null: Y or N. Y = Build rerun as NULL
      
      -- I think the standard view generator for a modern gz_shp_metadata table is in two parts:
      -- select * FROM TABLE(GZ_DADSGEN.PIPE_DADSFILE('GZACS12.GZ_SHP_METADATA','shp_type=''PRD'' AND unit LIKE ''st%''','Y','Y','Y','Y'))
      --   UNION ALL
      -- select * FROM TABLE(GZ_DADSGEN.PIPE_DADSFILE('GZACS12.GZ_SHP_METADATA','shp_type=''PRD'' AND unit LIKE ''nation''','N','Y','Y','Y'))
      

      psql           VARCHAR2(4000);
      output         DADSFILE_TAB;
      my_cursor      SYS_REFCURSOR;
      table_exists   PLS_INTEGER := 1;
      

   BEGIN

      psql := 'SELECT MODTAG, ';
      
      IF substr_unit = 'N'
      THEN
      
         psql := psql || 'UNIT, ';
         
      ELSE
      
         psql := psql || 'SUBSTR (UNIT, 3), ';
      
      END IF;
      
      IF concat_diskfile = 'N'
      THEN
      
         psql := psql || 'DISKFILE, ';
      
      ELSE
      
         psql := psql || 'OUTPUT_DIR || ''/'' || SHP_NAME, ';
      
      END IF;
      
      IF rerun_is_null = 'N'
      THEN
      
         psql := psql || 'RERUN, ';
      
      ELSE
      
         psql := psql || 'CAST(NULL AS VARCHAR2(1)), ';
      
      
      END IF;
      
      psql := psql || 'DEL_FLAG, ';
      
      IF date_time_other = 'N'
      THEN 
      
         psql := psql || 'DATE_TIME, ';
         
      ELSE
      
         psql := psql || 'SHP_CREATION_DATE, ';
      
      END IF;
         
      psql := psql || 'RESOLUTION, WAVE '
                   || 'FROM ' || table_name;
                  
      IF where_clause IS NOT NULL
      THEN
      
         psql := psql || ' WHERE ' || where_clause;
      
      END IF;
           
           
      BEGIN

         OPEN my_cursor FOR psql;

      EXCEPTION
      WHEN OTHERS
      THEN
         
         IF SQLCODE = -942
         THEN
            table_exists := 0;
         ELSE
            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' on ' || psql);
         END IF;        
         
      END;
      
      IF table_exists = 1
      THEN
      
         LOOP

            FETCH my_cursor BULK COLLECT INTO output LIMIT 10000;  --exadata, son
            EXIT WHEN output.COUNT = 0;

            FOR i IN 1 .. output.COUNT
            LOOP
               PIPE ROW(output(i));
            END LOOP;

         END LOOP;

         CLOSE my_cursor;
         
      END IF;

   END PIPE_DADSFILE;
   
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   
   PROCEDURE PREP_DADSGEN (
      p_tables          IN VARCHAR2 DEFAULT 'Y',
      p_views           IN VARCHAR2 DEFAULT 'Y',
      p_packages        IN VARCHAR2 DEFAULT 'Y'
   )
   AS
   
      --Matt! 9/18/13
      --Run before sending dadsgen@devbnch off to a GZ release 
      
      --exec GZ_DADSGEN.PREP_DADSGEN;
      
      psql              VARCHAR2(4000);
      tablez            MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      viewz             MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      packagez          MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      objectz           MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      error_msg         VARCHAR2(4000);
      
   
   BEGIN
   
      IF p_tables = 'Y'
      THEN
      
         psql := 'SELECT table_name FROM user_tables WHERE table_name NOT LIKE :p1';
         EXECUTE IMMEDIATE psql BULK COLLECT INTO tablez USING '%$';
         
         FOR i IN 1 .. tablez.COUNT
         LOOP
         
            EXECUTE IMMEDIATE 'GRANT SELECT ON ' || tablez(i) || ' TO "PUBLIC"';
         
         END LOOP;      
      
      END IF;
      
      IF p_views = 'Y'
      THEN
      
         psql := 'SELECT view_name FROM user_views';
         EXECUTE IMMEDIATE psql BULK COLLECT INTO viewz;
         
         FOR i IN 1 .. viewz.COUNT
         LOOP
         
            EXECUTE IMMEDIATE 'GRANT SELECT ON ' || viewz(i) || ' TO "PUBLIC"';
         
         END LOOP;               
      
      END IF;
      
      IF p_packages = 'Y'
      THEN
      
         --should be taken care by install script, but just in case we are off the trail a bit
      
         psql := 'SELECT object_name FROM user_objects WHERE UPPER(object_type) = :p1';
         EXECUTE IMMEDIATE psql BULK COLLECT INTO packagez USING 'PACKAGE';
         
         FOR i IN 1 .. packagez.COUNT
         LOOP
         
            EXECUTE IMMEDIATE 'GRANT EXECUTE ON ' || packagez(i) || ' TO "PUBLIC"';
         
         END LOOP;               
      
      END IF;
      
      
      --whats not wrong?
      
      psql := 'SELECT object_name FROM user_objects where status <> :p1';
      EXECUTE IMMEDIATE psql BULK COLLECT INTO objectz USING 'VALID';
      
      IF objectz.COUNT > 0
      THEN
      
         error_msg := 'These objects are invalid, son: ';
      
         FOR i IN 1 .. objectz.COUNT
         LOOP
         
            error_msg := error_msg || objectz(i) || ' | ';
         
         END LOOP;
      
         RAISE_APPLICATION_ERROR(-20001, error_msg);
         
      END IF;
   
   
   END PREP_DADSGEN;


END GZ_DADSGEN;
/

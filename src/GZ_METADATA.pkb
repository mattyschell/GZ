CREATE OR REPLACE PACKAGE BODY GZ_METADATA
AS

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

  FUNCTION run_shapefiles_and_metadata (
     pSchema           IN VARCHAR2,
     pJobid               IN VARCHAR2
  ) RETURN NUMBER DETERMINISTIC
  AS
  
  BEGIN
  
      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1000');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Create empty GZ_SHP_METADATA table if it does not exist.');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------
      
      --Create gz_shp_metadata if it does not exist or recreate it if it does not have any records yet.
      IF GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS('GZ_SHP_METADATA')
       THEN

            dbms_output.put_line('GZ_METADATA.run_shapefiles_and_metadata: GZ_SHP_METADATA exists and contains data!  Leave it be.');
         
      END IF;
  
      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS('GZ_SHP_METADATA')
       THEN

            dbms_output.put_line('GZ_METADATA.run_shapefiles_and_metadata: GZ_SHP_METADATA does not exist or does not have records populated yet.  Why not just remake that guy.');
            GZ_METADATA.create_gz_shp_metadata_table(pSchema);
         
      END IF;
      
      --Query GZ_SHAPEFILE_PARAMETERS table to formulate information for metadata records and calls to shapefiles
                         
      RETURN 0;
      
  END; 
  
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
  
  PROCEDURE create_gz_shp_metadata_table (
       pSchema         IN VARCHAR2
  )
  AS 
  
       p_table_name      VARCHAR2(4000) := 'GZ_SHP_METADATA';
       v_object_root   VARCHAR2(4000) := p_table_name;  --??
  
  BEGIN

     ----------------------------------------------------------------------------------
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Create metadata table: ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------
  
      GZ_WORKFLOW.CREATE_TABLE(USER, p_table_name, 'GZ_TYPES.GZ_SHP_METADATA' , 'N');
          
       
     ----------------------------------------------------------------------------------
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add constraints to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------

       EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || v_object_root || 'PKC '
              || '      PRIMARY KEY(JOBID,SHP_NAME,SHP_TYPE) '
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
      
      END;
      
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

END GZ_METADATA;
/

CREATE OR REPLACE PACKAGE GZ_METADATA
AUTHID CURRENT_USER
AS
   --Suzanne 04/03/2012
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
  
  FUNCTION run_shapefiles_and_metadata (
        pSchema           IN VARCHAR2,
        pJobid               IN VARCHAR2
  ) RETURN NUMBER DETERMINISTIC;
  
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
  
  PROCEDURE create_gz_shp_metadata_table(pSchema IN VARCHAR2);  
  
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
      
END GZ_METADATA;
/
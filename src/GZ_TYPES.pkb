CREATE OR REPLACE PACKAGE BODY GZ_TYPES AS




   FUNCTION NEW_EDGE_ATTRIBUTE RETURN GZ_TYPES.EDGE_ATTRIBUTE PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_EDGE_ATTRIBUTE;
   --
   FUNCTION NEW_EDGE_ATTRIBUTE_SIMPLIFY RETURN GZ_TYPES.EDGE_ATTRIBUTE_SIMPLIFY PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_EDGE_ATTRIBUTE_SIMPLIFY;
   
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION NEW_GEN_EXTENDED_TRACKING_LOG RETURN GZ_TYPES.GEN_EXTENDED_TRACKING PIPELINED
   AS
   BEGIN
      NULL;
   END NEW_GEN_EXTENDED_TRACKING_LOG;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION LEGAL_REFERENCE_TABLES
   RETURN GZ_TYPES.stringarray DETERMINISTIC
   AS

      --7/13/12 I dont think this is used anymore 
      --Just leaving it in to be safe
      
      --Matt! 8/3/11
      --I dont know nuthin about the reference tables
      --Except that I need REFERENCE_SCHEMAS at the moment
      --Please add more to the list, if it seems valid that a GZ job
      --should expect these to be present

      output      GZ_TYPES.stringarray;

   BEGIN

      output(1) := 'REFERENCE_SCHEMAS';
      output(2) := 'REFERENCE_FACE_FIELDS';

      --output(2) := 'REFERENCE_MIRRORS';
      --output(3) := 'REFERENCE_FIELDS';
      --output(4) := 'REFERENCE_ZERO_LEVEL';
      --output(6) := 'REFERENCE_CROSS_STATE';
      --output(7) := 'REFERENCE_ALL';

      RETURN output;

   END LEGAL_REFERENCE_TABLES;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   
   FUNCTION LEGAL_GZ_TABLES
   RETURN GZ_TYPES.stringarray DETERMINISTIC
   AS

      --7/13/12 I dont think this is used anymore 
      --Just leaving it in to be safe
      
      --Matt! 10/24/11
      --WIP. All tables in required to run GZ
      --used in the build script


      output      GZ_TYPES.stringarray;

   BEGIN

      output(1) := 'SMALL_POLYGON_PARAMETERS';
      output(2) := 'LINE_SIM_PARAMETERS';
      output(3) := 'LUT_LSAD';
      output(4) := 'GZ_SHAPEFILE_PARAMETERS';
      output(5) := 'GZ_CLIP_SETUP';
      output(6) := 'GZ_FSLBUILD_SETUP';
      output(7) := 'GZ_JOB_SETUP';
      output(8) := 'GZ_LINESIM_SETUP';
      output(9) := 'GZ_MERGE_SETUP';
      output(10) := 'GZ_QA_SETUP';
      output(11) := 'GZ_SHAPEFILE_SETUP';
      output(12) := 'GZ_SMPOLY_SETUP';
      output(13) := 'GZ_TOPOBUILD_SETUP';
      output(14) := 'GEN_CLIP_PARAMETERS';
      output(15) := 'GEN_MERGE_PARAMETERS';
      output(16) := 'GZ_PRE_SETUP';
      output(17) := 'GZ_JOB_PARAMETERS';
      output(18) := 'GZ_BUILD_SOURCE_PARAMETERS';
      output(19) := 'GZ_BUILD_SOURCE_SETUP';
      output(20) := 'FSLBUILD_PARAMETERS';
      output(21) := 'GZ_ENTITY_TABLE';
      output(22) := 'GZ_LAYERS_IN';
      output(23) := 'QA_PARAMETERS';
      output(24) := 'GZ_SHP_METADATA';
      output(25) := 'GZ_OUTPUT_SETUP';
     
      --add reference tables
      output := GZ_UTILITIES.STRINGARRAY_ADD(output, 
                                             GZ_TYPES.LEGAL_REFERENCE_TABLES());
                                             
      



      RETURN output;

   END LEGAL_GZ_TABLES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION LEGAL_GZ_RELEASE_TABLES
   RETURN GZ_TYPES.stringarray DETERMINISTIC
   AS

      --Matt! 4/24/12
      --WIP
      --All tables with a "release" in them, meaning they can be used in a project copier
      
      --should stringarray_add these to legal_gz_tables I guess, if it doesnt break anything


      output      GZ_TYPES.stringarray;

   BEGIN

      output(1) := 'GZ_LAYERS_IN';
      output(2) := 'GZ_LAYERS_OUT';
      output(3) := 'GZ_LAYERS_AGGREGATE';
      output(4) := 'GZ_LAYERS_SPLIT';
      output(5) := 'GZ_LAYERS_HIERARCHICAL';
      output(6) := 'GZ_LAYERS_SUBSET';
      output(7) := 'GZ_LAYERS_FIELDS';
      output(8) := 'GZ_LAYERS_CROSSWALK';
      output(9) := 'GZ_LAYERS_GEOID';
      --output(10) := 'FSLBUILD_PARAMETERS';
      output(10) := 'GEN_CLIP_PARAMETERS';
      output(11) := 'GEN_MERGE_PARAMETERS';
      output(12) := 'GZ_BUILD_SOURCE_PARAMETERS';
      output(13) := 'GZ_JOB_PARAMETERS';
      output(14) := 'GZ_OUTPUT_PARAMETERS';
      output(15) := 'GZ_SHAPEFILE_PARAMETERS';
      output(16) := 'LINE_SIM_PARAMETERS';
      output(17) := 'QA_PARAMETERS';
      output(18) := 'SHAPEFILE_PARAMETERS';
      output(19) := 'SMALL_POLYGON_PARAMETERS';
      output(20) := 'GZ_ENTITY_TABLE';
      output(21) := 'REFERENCE_FACE_FIELDS';

      RETURN output;
            
   END LEGAL_GZ_RELEASE_TABLES;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   
   FUNCTION LEGAL_GZ_OTHER_TABLES
   RETURN GZ_TYPES.stringarray DETERMINISTIC
   AS
   
      --Matt! 7/13/12
      --Accompanies LEGAL_GZ_RELEASE_TABLES
      --These are the weirdos without a release column
      
      output      GZ_TYPES.stringarray;
      
   BEGIN
   
      output(1) := 'REFERENCE_SCHEMAS';
      output(2) := 'LUT_LSAD';  --not sure of the future of this one
   
   
      RETURN output;
      
   END LEGAL_GZ_OTHER_TABLES;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   
   FUNCTION LEGAL_GZ_ALL_TABLES
   RETURN GZ_TYPES.stringarray DETERMINISTIC
   AS
   
      --Matt! 7/13/12
      
      output      GZ_TYPES.stringarray;
      
   BEGIN
   
      output := GZ_UTILITIES.STRINGARRAY_ADD(GZ_TYPES.LEGAL_GZ_RELEASE_TABLES(),
                                             GZ_TYPES.LEGAL_GZ_OTHER_TABLES());
   
   
      RETURN output;
      
   END LEGAL_GZ_ALL_TABLES;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


END GZ_TYPES;
/

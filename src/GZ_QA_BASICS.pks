CREATE OR REPLACE PACKAGE GZ_QA_BASICS
   AUTHID CURRENT_USER
AS
   -- Stephanie 08/01/2013

   -- a package for basic QA check on generalization jobs

   -- main entry from GZ_WORKFLOW = GENERALIZATION_RUN_BASIC_QA;

   -----------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------

   FUNCTION VERTEX_COUNT_COMPARISON (
      p_module        IN VARCHAR2,
      p_topo          IN VARCHAR2,
      p_gen_tbl       IN VARCHAR2,
      p_ungen_tbl     IN VARCHAR2,
      p_geom_column   IN VARCHAR2 DEFAULT 'SDOGEOMETRY')
      RETURN VARCHAR2;

   FUNCTION CHECK_STATE_AGREEMENT (
      p_module           IN VARCHAR2,
      p_topo             IN VARCHAR2,
      p_gen_face_tab     IN VARCHAR2,
      p_release          IN VARCHAR2,
      p_gen_project_id   IN VARCHAR2)
      RETURN VARCHAR2;


   -----------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------

   -- MAIN ENTRY POINT for GZ software...

   FUNCTION GENERALIZATION_RUN_BASIC_QA (
      p_jobid             IN VARCHAR2,
      p_output_topology   IN VARCHAR2,
      p_ungen_topology    IN VARCHAR2,
      p_gen_tbl           IN VARCHAR2,
      p_ungen_tbl         IN VARCHAR2)
      RETURN VARCHAR2;
      
END GZ_QA_BASICS;
/
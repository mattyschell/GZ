CREATE OR REPLACE PACKAGE GZ_FSL
AUTHID CURRENT_USER
AS

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- PROCEDURES
--------------------------------------------------------------------------------
PROCEDURE CREATE_FSL_TRACKING (
         p_schema IN VARCHAR2,
         p_table_name IN VARCHAR2 DEFAULT 'GEN_FSL_TRACKING');

PROCEDURE GRANT_FSL_VIEW_PERMISSIONS (
         p_topology_name varchar2,
         p_to_whom varchar2);

PROCEDURE GRANT_FSL_TABLE_PERMISSIONS (
          p_topology_name IN VARCHAR2,
          p_to_whom IN VARCHAR2);

PROCEDURE CHECK_FSLS(
         p_schema IN VARCHAR2,
         p_topology IN VARCHAR2);

PROCEDURE FIX_ALL_CONNECT_BY_ERRORS(
         p_topology IN VARCHAR2,
         p_feature_table_name IN VARCHAR2,
         p_PrimaryKeyColumn IN VARCHAR2,
         p_GeometryColumn IN VARCHAR2 DEFAULT 'TOPOGEOM');

Procedure CREATE_STATETOPO_VIEWS(
         Schema_Name VARCHAR2,
         Topology VARCHAR2 default NULL);
--------------------------------------------------------------------------------
-- FUNCTIONS
--------------------------------------------------------------------------------
FUNCTION NEW_GEN_FSL_TRACKING RETURN GZ_TYPES.GEN_FSL_TRACKING PIPELINED;

FUNCTION COUNT_BAD_GTYPES(
   p_polygon_table_name   varchar2,
   p_geometry_column_name varchar2
) RETURN number;

FUNCTION HAS_CONNECT_BY_ERROR(
   p_topology   varchar2
) RETURN boolean;

PROCEDURE FSL_BUILD(
   pSchema IN VARCHAR2,
   pTopo_In IN VARCHAR2,
   pTopoType IN VARCHAR2,   
   pFace_Tab_Ext IN VARCHAR2,
   pTopo_Universe IN VARCHAR2,
   pTopo_Hierarchy IN VARCHAR2,
   --pHas_Mirrors IN VARCHAR2,
   pProject_Id IN VARCHAR2,
   pProject_Z9 IN VARCHAR2,
   pRelease In VARCHAR2,
   pSt_Edges_Tbl in VARCHAR2,
   pJobID in VARCHAR2 default NULL   
   );

Procedure CLEANUP_FSLS(
   pTOPO_IN IN VARCHAR2,
   pSCHEMA IN VARCHAR2,
   pST_EDGES_TBL IN VARCHAR2
 );

PROCEDURE CLEANUP_FSLS_QA (pJOBID_IN   IN VARCHAR2,
                           pTOPO_IN    IN VARCHAR2,
                           pSCHEMA     IN VARCHAR2
);

Procedure Build_FSLs(
   pTOPO_IN IN VARCHAR2,
   pTopo_Type IN VARCHAR2,   
   pFACE_TABLE_EXT IN VARCHAR2,
   pFACE_TABLE IN VARCHAR2,
   pTopo_Universe IN VARCHAR2,
   pTopo_Hierarchy IN VARCHAR2,
   --pHas_Mirrors IN VARCHAR2,
   pProject_Id IN VARCHAR2,
   pProject_Z9 IN VARCHAR2,
   pRelease In VARCHAR2,
   pSchema IN VARCHAR2,
   pJobID in VARCHAR2 default NULL   
);

------------------------------------------
END GZ_FSL;
/


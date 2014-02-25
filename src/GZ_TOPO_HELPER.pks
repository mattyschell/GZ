CREATE OR REPLACE PACKAGE GZ_TOPO_HELPER AUTHID CURRENT_USER AS
/*
--*******************************************************************************
--*   NAME:       TOPO_UTIL 
--*   PURPOSE:    To contain all Generalization Topological Methods
--*
--*   REVISIONS:
--*   Ver        Date        Author           Description
--*   ---------  ----------  ---------------  ------------------------------------
--*   1.0        12/15/2009  Salman Mansoor    1. Created this package specification.
--*******************************************************************************
*/
/* GLOBAL VARIABLES BELOW HERE */                                                           
/* PROCEDURES Below Here */

PROCEDURE add_geoid_column_to_table(pInTable VARCHAR2,pNewColumnName VARCHAR2 DEFAULT 'GEO_ID');

PROCEDURE fsl_views (pInTableFsl VARCHAR2);

PROCEDURE FSL_MIRRORS (topology VARCHAR2, new_table VARCHAR2, mirror_table VARCHAR2);

PROCEDURE PROCESS_FSL (Topology VARCHAR2, Face_tbl VARCHAR2, Topo_universe VARCHAR2, Topo_hierarchy VARCHAR2, Projection VARCHAR2 default 'N', Release VARCHAR2, Deploy VARCHAR2);

PROCEDURE PROCESS_FSL_DEC (Topology VARCHAR2, Face_tbl VARCHAR2, Topo_universe VARCHAR2, Topo_hierarchy VARCHAR2, Projection VARCHAR2 default 'N', Release VARCHAR2, Deploy VARCHAR2);

-- PROCEDURE PROCESS_FSL (Topology VARCHAR2, Face_tbl VARCHAR2, Topo_universe VARCHAR2, Topo_hierarchy VARCHAR2, Mirrors VARCHAR2 default 'N', Projection VARCHAR2 default 'N', Release VARCHAR2, Deploy VARCHAR2);

PROCEDURE PROCESS_FSL_SD (Topology VARCHAR2, Face_tbl VARCHAR2, Release VARCHAR2, Deploy VARCHAR2);

PROCEDURE REGISTER_FSL (Operation VARCHAR2, Topology VARCHAR2, Table_Name VARCHAR2 Default NULL, Child_Layer_Table VARCHAR2 Default NULL, Deploy VARCHAR2, topo_hierarchy VARCHAR2);

PROCEDURE DEREGISTER_FSL (Topology VARCHAR2, Table_Type VARCHAR2, Deploy VARCHAR2);

PROCEDURE REGISTER_FSL_ORG (Operation VARCHAR2, Topology VARCHAR2, Table_Name VARCHAR2 Default NULL, Child_Layer_Table VARCHAR2 Default NULL, Deploy VARCHAR2);

PROCEDURE create_sdo_views (Topology VARCHAR2);

PROCEDURE process_views (Topology VARCHAR2);

PROCEDURE update_sdogeometry (Topology VARCHAR2);

PROCEDURE update_sdogeom (Topology VARCHAR2);

PROCEDURE create_scr_tables (topology VARCHAR2,face_tbl VARCHAR2,topo_universe VARCHAR2,release varchar2,deploy VARCHAR2);

PROCEDURE create_fsl_tables (topology VARCHAR2,topo_universe VARCHAR2,release varchar2,deploy VARCHAR2);

PROCEDURE RELATION_TOPO_MODIFY (pTopology VARCHAR2, pInTable VARCHAR2);

PROCEDURE build_face2_cols (pFaceTable VARCHAR2,pFaceDollarTable VARCHAR2);

PROCEDURE load_face2 (topology VARCHAR2,face_tbl VARCHAR2,face_dollar_tbl VARCHAR2,deploy VARCHAR2,release varchar2);

PROCEDURE load_topo_preface_2 (topology VARCHAR2,tbl_type VARCHAR2,release VARCHAR2,deploy VARCHAR2);

PROCEDURE load_topo_sl_1_up (topology VARCHAR2, sl_type VARCHAR2, release VARCHAR2, deploy VARCHAR2);

PROCEDURE national_zone (pTopology VARCHAR2,ptarget_scale INTEGER DEFAULT  500000,pnice number default 1, pungentableName VARCHAR2);

PROCEDURE REMOVE_CLOSE_XYS_MASTER_HIGH (pInTable VARCHAR2,  pInEdgeIdColumn VARCHAR2 default 'EDGE_ID',pInSDOGeomColumn VARCHAR2 default 'SDOGEOMETRY');

PROCEDURE REMOVE_CLOSE_XYS_MASTER_HIGH2 (pInTable VARCHAR2,  pPrimaryKeyColumn VARCHAR2 default 'GEO_ID',pInSDOGeomColumn VARCHAR2 default 'SDOGEOMETRY');

/*
PROCEDURE REMOVE_CLOSE_XYS_MASTER_LOW (pInTable VARCHAR2,  pInEdgeIdColumn VARCHAR2 default 'EDGE_ID',pInSDOGeomColumn VARCHAR2 default 'SDOGEOMETRY');
*/

FUNCTION REMOVE_CLOSE_XYS_HIGH (Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,ptolerance NUMBER default 0.05) RETURN BOOLEAN Deterministic;

/*
FUNCTION REMOVE_CLOSE_XYS_LOW (Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,ptolerance NUMBER default 0.05) RETURN BOOLEAN Deterministic;
*/

PROCEDURE post_face2 (pFaceTable VARCHAR2, pFixTable VARCHAR2);

PROCEDURE face_postclip_fix (pTopology VARCHAR2, pFaceTable VARCHAR2, pFixTable VARCHAR2);

PROCEDURE drop_fsl_tables (pTopology varchar2);

FUNCTION count_occurs (str varchar2, search varchar2 := null) return number;

FUNCTION create_dml_condition (tbl_keys IN VARCHAR2, tbl_keys_data IN VARCHAR2) RETURN VARCHAR2;

FUNCTION create_ft_cols (table_name IN varchar2) RETURN VARCHAR2;

PROCEDURE duplicate_face_table(
      pTopology              IN VARCHAR2,
      pFaceFeatureTable      IN VARCHAR2,
      pNewFaceFeatureTable   IN VARCHAR2
  );

END GZ_TOPO_HELPER;
/


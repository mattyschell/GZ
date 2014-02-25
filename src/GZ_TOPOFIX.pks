CREATE OR REPLACE PACKAGE GZ_TOPOFIX
AUTHID CURRENT_USER
AS


   -- Face Fixing
   -- Best entry point: Wrapper gz_topofix.check_face_tab
   -- Main logical entry: gz_topofix.gz_fix_face
   
   --For edge fixing
   --GZ_FIX_EDGE
   
   --For coastal slivers
   --GZ_COASTAL_SLIVERS

   ------------------------------------------------------------------------------------
   --   | MATT | ++++++++++++++++++++++++++++++++++++++++++++++++++ | MATT |  +++++++--
   ----\/-----\/----------------------------------------------------\/-----\/----------
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION VERIFY_CS_INPUTS (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_topo                  IN VARCHAR2,
      p_face_table            IN VARCHAR2,
      p_sliver_restart_flag   IN VARCHAR2,
      p_sliver_width          IN NUMBER,
      p_segment_length        IN NUMBER,
      p_expendable_review     IN VARCHAR2,
      p_reshape_review        IN VARCHAR2,
      p_update_feature_geom   IN VARCHAR2
   ) RETURN VARCHAR2;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GZ_COASTAL_SLIVERS (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_topo                  IN VARCHAR2,
      p_face_table            IN VARCHAR2,
      p_log_type              IN VARCHAR2,
      p_sliver_restart_flag   IN VARCHAR2 DEFAULT 'N',
      p_sliver_width          IN NUMBER DEFAULT NULL,
      p_segment_length        IN NUMBER DEFAULT NULL,
      p_expendable_review     IN VARCHAR2 DEFAULT 'N',
      p_reshape_review        IN VARCHAR2 DEFAULT 'Y',
      p_update_feature_geom   IN VARCHAR2 DEFAULT 'N', 
      p_tolerance             IN NUMBER DEFAULT .05,
      p_srid                  IN NUMBER DEFAULT 8265
   ) RETURN VARCHAR2;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   --Main entry point
   PROCEDURE START_TOPOFIX_LOGGING (
      p_gz_jobid       IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_log_type       IN VARCHAR2 --TOPOFIX if standalone, could also add to CLIP, MERGE, etc
   );

   FUNCTION GZ_FIX_FACE (
      p_gz_jobid        IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_log_type        IN VARCHAR2 DEFAULT 'TOPOFIX',
      p_hold_universal  IN VARCHAR2 DEFAULT 'Y',
      p_calc_sdo        IN VARCHAR2 DEFAULT 'N',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_qc_col          IN VARCHAR2 DEFAULT 'QC',
      p_sdo_col         IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_sdo_srid        IN NUMBER DEFAULT 8265,
      p_face_pkc        IN VARCHAR2 DEFAULT 'FACE_ID',
      p_valid_edges     IN VARCHAR2 DEFAULT 'N',
      p_debug           IN NUMBER DEFAULT 0
   ) RETURN VARCHAR2;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE VERIFY_TOPOFIX_INPUTS (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_log_type        IN VARCHAR2,
      p_calc_sdo        IN VARCHAR2,
      p_qc_col          IN VARCHAR2,
      p_sdo_col         IN VARCHAR2,
      p_face_pkc        IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_INVALID_EDGES (
      p_topo            IN VARCHAR2,
      p_sdo_col         IN VARCHAR2 DEFAULT 'GEOMETRY',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_pkc_col         IN VARCHAR2 DEFAULT 'EDGE_ID'
   ) RETURN GZ_TYPES.stringhash;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_INVALID_IDS (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_error           IN VARCHAR2,
      p_face_pkc        IN VARCHAR2 DEFAULT 'FACE_ID',
      p_sdo_col         IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN GZ_TYPES.stringarray;


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION PARSE_VALIDATION_STRING (
      p_message         IN VARCHAR2,
      p_token           IN VARCHAR2
   ) RETURN NUMBER DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_EDGE_FROM_SEGMENT (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_face_id         IN NUMBER,
      p_segment         IN SDO_GEOMETRY,
      p_face_pkc        IN VARCHAR2 DEFAULT 'FACE_ID',
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN NUMBER;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_SHORT_SEGMENT_INDEX (
      p_topo               IN VARCHAR2,
      p_edge_id            IN VARCHAR2
   ) RETURN NUMBER;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION IS_NODE_UNIVERSAL (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   ) RETURN BOOLEAN;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION IS_NODE_OBSOLETE (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   ) RETURN BOOLEAN;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION IS_EDGE_UNIVERSAL (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER
   ) RETURN BOOLEAN;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_SEGMENT_GEOM_FROM_INDEX (
      p_edge           IN SDO_GEOMETRY,
      p_index          IN PLS_INTEGER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION DUMP_CONSTELLATION (
      p_constellation      IN GZ_TYPES.TOPOFIX_CONSTELLATION
   ) RETURN VARCHAR2;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_SEGMENT_OBS_NODES (
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER,
      p_index           IN NUMBER,
      p_start_node_id   IN NUMBER,
      p_end_node_id     IN NUMBER,
      p_vertex_count    IN NUMBER
   ) RETURN NUMBER;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_EDGE_CONSTELLATION (
      p_topo               IN VARCHAR2,
      p_edge_id            IN VARCHAR2
   ) RETURN GZ_TYPES.TOPOFIX_CONSTELLATION;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE REMOVE_SHORTY_VERTEX (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_index              IN NUMBER
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION EXTEND_SEGMENT (
      p_segment            IN SDO_GEOMETRY,
      p_percent            IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION PROJECT_V_TIP (
      p_edge_geom          IN SDO_GEOMETRY,
      p_index              IN NUMBER,
      p_percent            IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_debug              IN NUMBER DEFAULT 0
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION PROJECT_V_TIP_VALID (
      p_edge_geom          IN SDO_GEOMETRY,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_debug              IN NUMBER DEFAULT 0
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION PROJECT_V_TIP_GARBAGE (
      p_edge_geom          IN SDO_GEOMETRY,
      p_projection_dist    IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_debug              IN NUMBER DEFAULT 0
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION CALC_SEGMENT_ANGLE (
      p_geom1              IN SDO_GEOMETRY,
      p_geom2              IN SDO_GEOMETRY
   ) RETURN NUMBER DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_NEXT_DIRECTED_SEGMENT (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_node_id            IN NUMBER,
      p_direction          IN VARCHAR2,
      p_ignore_edge        IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_NODE_ID(
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_tip                IN VARCHAR2
   ) RETURN NUMBER;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE RECONNECT_NODE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_shorty_index       IN NUMBER,
      p_tip                IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE ZAP_NODE (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE GZ_MOVE_EDGE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_from_node_id       IN NUMBER,
      p_to_node_id         IN NUMBER
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE MERGE_NODES (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_zap_tip            IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE KEEP_MOST_ANGULAR (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_shorty_index       IN NUMBER,
      p_start_node_id      IN NUMBER,
      p_end_node_id        IN NUMBER,
      p_edge_vertex_count  IN NUMBER,
      p_ignore_edge        IN NUMBER DEFAULT NULL
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE SLIDE_NODE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_node_id            IN NUMBER,
      p_distance           IN NUMBER DEFAULT .05
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE SLIDE_LEAST_ANGULAR_NODE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_start_node_id      IN NUMBER,
      p_end_node_id        IN NUMBER,
      p_distance           IN NUMBER DEFAULT .05
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE LENGTHEN_SHORTY (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_index              IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION ADD_FAKE_FACE (
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER,
      p_index           IN NUMBER,
      p_face_tab        IN VARCHAR2,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN NUMBER;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

    FUNCTION GET_TEMP_EDGE(
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER
   ) RETURN NUMBER;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

    PROCEDURE REMOVE_OBSOLETE_NODE (
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION CLOCKWISE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_face_id            IN NUMBER
   ) RETURN BOOLEAN;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE FIX_INTERIOR_DUPE (
      p_topo                     IN VARCHAR2,
      p_edge_id                  IN NUMBER,
      p_start_node_id            IN NUMBER,
      p_end_node_id              IN NUMBER
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE REMOVE_INTERIOR_VERTEX (
      p_topo                     IN VARCHAR2,
      p_edge_id                  IN NUMBER,
      p_shorty_index             IN NUMBER
   );
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION GET_LINEAR_FEATURE_TABS (
      p_topo                  IN VARCHAR2,
      p_tg_layer_level        IN NUMBER DEFAULT 0
   ) RETURN GZ_TYPES.stringarray;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   PROCEDURE ANNIHILATE_LINEAR_FEATURES (
      p_topo                  IN VARCHAR2,
      p_edge_id               IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   PROCEDURE GZ_TOPOFIX_REMOVE_EDGE (
      p_topo                  IN VARCHAR2,
      p_edge_id               IN VARCHAR2,
      p_delete_features       IN VARCHAR2 DEFAULT 'N',
      p_depth                 IN NUMBER DEFAULT 0
   );
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE DROP_Y_SHAPE (
      p_topo                     IN VARCHAR2,
      p_edge_id                  IN NUMBER,
      p_start_node_id            IN NUMBER,
      p_end_node_id              IN NUMBER,
      p_debug                    IN NUMBER DEFAULT NULL
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION FIX_CONSTELLATION (
      p_topo               IN VARCHAR2,
      p_face_tab           IN VARCHAR2,
      p_constellation      IN GZ_TYPES.TOPOFIX_CONSTELLATION,
      p_hold_universal     IN VARCHAR2 DEFAULT 'Y',
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN VARCHAR2;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE SELF_INTERSECTS_WORKAROUND (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_index              IN NUMBER,
      p_face_tab           IN VARCHAR2,
      p_tolerance          IN NUMBER DEFAULT .05
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION RESHAPE_EDGE_WITH_DUPLICATE (
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER,
      p_tolerance       IN NUMBER DEFAULT .05,
      p_hold_universal  IN VARCHAR2 DEFAULT 'Y',
      p_log_type        IN VARCHAR2 DEFAULT 'TOPOFIX'
   ) RETURN VARCHAR2;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION FIX_13356 (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_face_id         IN NUMBER,
      p_hold_universal  IN VARCHAR2 DEFAULT 'Y',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_sdo_col         IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_face_pkc        IN VARCHAR2 DEFAULT 'FACE_ID',
      p_log_type        IN VARCHAR2 DEFAULT 'TOPOFIX',
      p_debug           IN NUMBER DEFAULT 0
   ) RETURN VARCHAR2;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

    FUNCTION IS_FACE_INVALID (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_face_id         IN NUMBER,
      p_error           IN VARCHAR2,
      p_face_pkc        IN VARCHAR2 DEFAULT 'FACE_ID',
      p_sdo_col         IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN BOOLEAN;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------







   ----^------^-----------------------------------------------------^------^----------
   --  | MATT | ++++++++++++++++++++++++++++++++++++++++++++++++++++| MATT |++++++++--
   -----------------------------------------------------------------------------------

   ------------------------------------------------------------------------------------
   --   | SIDEY | +++++++++++++++++++++++++++++++++++++++++++++++++ | SIDEY |++++++++--
   ----\/------\/---------------------------------------------------\/------\/---------


-- A package written  1) to check for edge or polygon self-intersections or glancing blows
--                 or 2) to check for intersections or glancing blows between 2 entities.
--                       (incomplete because may not detect n with m where it does detect
--                        m with n - these are segment numbers)
--           or   >>>>3) to fix faces with 13349 errors

FUNCTION FIX_A_TRIANGLE(edge_id NUMBER,start_node NUMBER, end_node NUMBER,face_no NUMBER,pClip_face_table VARCHAR2,InGeom_Column VARCHAR2 default 'SDOGEOMETRY',ptopology VARCHAR2, pSchema VARCHAR2,tolerance NUMBER,p_log_type VARCHAR2)  RETURN VARCHAR2;

FUNCTION TEST_BETTER_EDGE(Topology VARCHAR2,face_id NUMBER,Clip_face_table VARCHAR2, InGeom_Column VARCHAR2,tolerance NUMBER default 0.05) RETURN MDSYS.SDO_LIST_TYPE;
-- function to return a STATUS of TRUE after a face is fixed
FUNCTION FIX_13349(face_id NUMBER,Intable VARCHAR2,InGeom_Column VARCHAR2,schema VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05,pUFS VARCHAR default 'FALSE',p_log_type VARCHAR2) RETURN VARCHAR2;
-- Procesdural entry point to fix a face with 13349 error
PROCEDURE FIX_FACE_13349(face_id NUMBER,Intable VARCHAR2,InGeom_Column VARCHAR2,schema VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05,UFS VARCHAR default 'FALSE',p_log_type VARCHAR2);
PROCEDURE ADD_EDGE_BETWEEN_NODES(Topology VARCHAR2,p_old_node_id NUMBER, p_new_node_id NUMBER, new_edge_geom MDSYS.SDO_GEOMETRY);
PROCEDURE ADD_EDGE_VERTEX(Topology VARCHAR2,edge_id NUMBER, measure NUMBER, new_vertex_geom MDSYS.SDO_GEOMETRY default NULL);
FUNCTION ADD_NODE_TO_EDGE(Topology VARCHAR2,edge_id NUMBER, measure NUMBER, new_node_geom MDSYS.SDO_GEOMETRY default NULL,p_is_new_shape_point VARCHAR2 default 'TRUE') RETURN NUMBER;
PROCEDURE ADD_MOVE_DROP_VERTEX(Topology VARCHAR2,edge_id NUMBER,pvertex NUMBER, pmeasure NUMBER default NULL,new_point_geom MDSYS.SDO_GEOMETRY default NULL,Add_new_vertex VARCHAR2 default NULL);
PROCEDURE MOVE_EDGE_NODE(Topology VARCHAR2,edge_id NUMBER,node_id NUMBER, pmeasure NUMBER default NULL,new_node_geom MDSYS.SDO_GEOMETRY);
PROCEDURE MOVE_EDGE_VERTEX(Topology VARCHAR2,edge_id NUMBER, vertex NUMBER,measure NUMBER, new_vertex_geom MDSYS.SDO_GEOMETRY default NULL);
FUNCTION ADD_RESHAPE_BEND(edge_to_move NUMBER, poly_geom MDSYS.SDO_GEOMETRY,face_id NUMBER,Intable VARCHAR2,InGeom_Column VARCHAR2,schema VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05) RETURN NUMBER;
-- The Universal face switch (UFS) enables changes to be made on the UF
FUNCTION CHECK_FACE_FOR_13349(face_id NUMBER,Intable VARCHAR2,InGeom_Column VARCHAR2,schema VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05,UFS VARCHAR default 'FALSE') RETURN NUMBER;
PROCEDURE CHECK_FOR_NEARBY_SEGMENTS(pTopology VARCHAR2,pInput_Table VARCHAR2,EDGE_ID_COLUMN VARCHAR2, GEOMETRY_COLUMN VARCHAR2, ptolerance NUMBER default 0.05,edge_id NUMBER default 0.0,plog_type VARCHAR2 default 'LS');
PROCEDURE CHECK_FOR_NEAR_SEGMENTS(pTopology VARCHAR2,pInput_Table VARCHAR2,EDGE_ID_COLUMN VARCHAR2, GEOMETRY_COLUMN VARCHAR2, ptolerance NUMBER default 0.05,edge_id NUMBER default 0.0,plog_type VARCHAR2 default 'LS');

FUNCTION CHECK_GEOM_SELF_INTERSECT(XYOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,allow_touches BOOLEAN,start_end IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,tolerance NUMBER,xdelta NUMBER,ydelta NUMBER,dec_digits NUMBER default 18,thousand NUMBER default 10000000.,oid number default 0,skip BOOLEAN default FALSE) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION CIRCULATE_COORDINATES(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,shift PLS_INTEGER,pstart PLS_INTEGER default 1, pend PLS_INTEGER default 0) RETURN MDSYS.SDO_ORDINATE_ARRAY;
FUNCTION CIRCULATE_NODE(Edge_id NUMBER,Topology VARCHAR2) RETURN NUMBER;
FUNCTION Concatenate_lines_Sans(pline1 IN MDSYS.SDO_GEOMETRY,pline2 IN MDSYS.SDO_GEOMETRY,p_tolerance NUMBER DEFAULT 0.05) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION CONCATENATE_XYS_INFO(Geometry1 IN MDSYS.SDO_GEOMETRY,Geometry2 IN MDSYS.SDO_GEOMETRY,INFO_ARRAY IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,List_within MDSYS.SDO_LIST_TYPE default NULL) RETURN MDSYS.SDO_ORDINATE_ARRAY;
Function Distance_Fcn(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER;
PROCEDURE DROP_EDGE(Topology VARCHAR2,p_edge_id NUMBER);
PROCEDURE DROP_NODE(Topology VARCHAR2,p_node_id NUMBER);
PROCEDURE DROP_EDGE_VERTEX(Topology VARCHAR2,edge_id NUMBER,vertex NUMBER);
FUNCTION Fast_Vincenty_gcd(x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,units VARCHAR2 DEFAULT 'm') RETURN NUMBER DETERMINISTIC;
FUNCTION FIND_CLOSEST_EDGES(pTopology VARCHAR2,face_id NUMBER,Clip_face_table VARCHAR2, InGeom_Column VARCHAR2,seg1 IN OUT NOCOPY NUMBER,seg2 IN OUT NOCOPY NUMBER,tolerance NUMBER) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION GET_Angles(Geom IN MDSYS.SDO_GEOMETRY,smallest VARCHAR2 default 'NO') RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION GET_NEARBY_EDGES(Edges_Table VARCHAR2,EDGE_ID_COLUMN VARCHAR2, GEOMETRY_COLUMN VARCHAR2,Edge_id NUMBER,pxLL NUMBER,pyLL NUMBER,pxUR NUMBER,pyUR NUMBER,SRID NUMBER,GREATER VARCHAR2 DEFAULT 'NO',pxepsilon NUMBER default NULL,pyepsilon NUMBER default NULL,ptolerance NUMBER default 0.05) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION GET_BETTER_EDGE (pTopology VARCHAR2,face_id NUMBER,edge_id IN OUT NOCOPY NUMBER,pInSchema VARCHAR2,Clip_face_table VARCHAR2,InGeom_Column VARCHAR2 default 'SDOGEOMETRY',tolerance NUMBER default 0.05,decim_digits NUMBER default 7) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION GET_CLOSEST(geom1 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                geom2 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,same BOOLEAN,xfound IN OUT NOCOPY NUMBER,yfound IN OUT NOCOPY NUMBER,where_it_is IN OUT NOCOPY NUMBER) RETURN NUMBER;
FUNCTION GET_CLOSEST_SEGMENT(
  px NUMBER,py NUMBER,Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,pSRID NUMBER,where_it_is IN OUT NOCOPY NUMBER) RETURN NUMBER;
FUNCTION GET_DIST(Geom IN MDSYS.SDO_GEOMETRY,smallest VARCHAR2 default 'YES',coords VARCHAR2 default 'YES') RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION GET_SHORT_DIST(Geom IN MDSYS.SDO_GEOMETRY,pgap NUMBER,where_is IN OUT NOCOPY NUMBER) RETURN NUMBER;
FUNCTION GET_WITHIN_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION GET_SEG(Geom MDSYS.SDO_GEOMETRY, pvtx PLS_INTEGER) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION GET_XY(Geom MDSYS.SDO_GEOMETRY, pvtx PLS_INTEGER) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION ID_EDGE_IN_13349(status VARCHAR2,face_id NUMBER,Intable VARCHAR2,InGeom_Column VARCHAR2,schema VARCHAR2,topology VARCHAR2) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION Is_pt_on_Segment(xtest IN OUT NOCOPY NUMBER,ytest IN OUT NOCOPY NUMBER, -- test point
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,  -- the line
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                       xnear IN OUT NOCOPY NUMBER,ynear IN OUT NOCOPY NUMBER,
                       pu IN OUT NOCOPY NUMBER) RETURN NUMBER DETERMINISTIC;
Function karney(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER;
FUNCTION LINE_INTERSECT( X1 IN OUT NOCOPY NUMBER,
                              Y1 IN OUT NOCOPY NUMBER,
                              X2 IN OUT NOCOPY NUMBER,
                              Y2 IN OUT NOCOPY NUMBER,
                              X3 IN OUT NOCOPY NUMBER,
                              Y3 IN OUT NOCOPY NUMBER,
                              X4 IN OUT NOCOPY NUMBER,
                              Y4 IN OUT NOCOPY NUMBER,
                              Xi IN OUT NOCOPY NUMBER,
                              Yi IN OUT NOCOPY NUMBER,
                              vertex IN OUT NOCOPY NUMBER,
                              distance IN OUT NOCOPY NUMBER,
                              tolerance NUMBER,
                              flag_2way IN OUT NOCOPY PLS_INTEGER)
            RETURN NUMBER Deterministic;
FUNCTION LRS_LOCATE_PT(measure NUMBER, edge_geom MDSYS.SDO_GEOMETRY, limit_measure VARCHAR2 default 'Y') RETURN MDSYS.SDO_GEOMETRY;
FUNCTION MATCH_EDGE(poly_geom   MDSYS.SDO_GEOMETRY, edge_id NUMBER, edge_geom   MDSYS.SDO_GEOMETRY,x IN OUT NOCOPY NUMBER, y IN OUT NOCOPY NUMBER,len IN OUT NOCOPY NUMBER, epsilon number default 0.0000005) return MDSYS.SDO_LIST_TYPE;
FUNCTION MEASURE_ANGLE (X1  NUMBER,  Y1 NUMBER,
                           XC  NUMBER,  YC  NUMBER,
                           X2  NUMBER,  Y2 NUMBER)
                                 RETURN NUMBER Deterministic;
FUNCTION MOVE_APART_2EDGES(geom1 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                geom2 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,same BOOLEAN,New_one IN OUT NOCOPY NUMBER,pUFS NUMBER,ptolerance NUMBER default 0.05) RETURN MDSYS.SDO_GEOMETRY;

FUNCTION ORIENT2D (paX   NUMBER,  paY   NUMBER,
                      pbX   NUMBER,  pbY   NUMBER,
                      pcX   NUMBER,  pcY   NUMBER)
                      RETURN NUMBER Deterministic;
FUNCTION Perpendicular(xin IN OUT NOCOPY NUMBER,yin IN OUT NOCOPY NUMBER,  -- the point
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,  -- the line
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                       xnear IN OUT NOCOPY NUMBER,ynear IN OUT NOCOPY NUMBER,  -- the perpendicular
                       always BOOLEAN default FALSE,  -- always return a distance
                       meters BOOLEAN default FALSE)  -- when true return meters
                       RETURN NUMBER deterministic;
FUNCTION PULL(err_msg VARCHAR2,find VARCHAR2, instance PLS_INTEGER default 1) return number;

PROCEDURE TRACK_APP_ERROR(msg VARCHAR2,p_table_name VARCHAR2 default NULL,p_log_type VARCHAR2 default NULL,geom MDSYS.SDO_GEOMETRY default NULL);
FUNCTION RESHAPE_LEDGE(ptopology VARCHAR2, face_no NUMBER, tolerance NUMBER,pSchema VARCHAR2, pClip_face_table VARCHAR2,InGeom_Column VARCHAR2 default 'SDOGEOMETRY',UFS VARCHAR2 default 'FALSE') RETURN VARCHAR2;
FUNCTION RESHAPE_TOO_CLOSE(pschema VARCHAR2,ptopology VARCHAR2, pedge_id1 NUMBER, pedge_id2 NUMBER, tolerance NUMBER,universal_flag VARCHAR2, log_type VARCHAR2) RETURN VARCHAR2;

FUNCTION REMOVE_CLOSE_XYS(Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,ptolerance NUMBER default 0.05)
 RETURN BOOLEAN Deterministic;

PROCEDURE
         REVERSE_ORDINATES(XY IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                             LB  PLS_INTEGER default 1,
                             InUB  PLS_INTEGER default 0) ;
FUNCTION SHORT_ORACLE_DIST(x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER)
                                 RETURN NUMBER DETERMINISTIC;

FUNCTION SIMPLE_INTERSECT( X1 IN OUT NOCOPY NUMBER,
                              Y1 IN OUT NOCOPY NUMBER,
                              X2 IN OUT NOCOPY NUMBER,
                              Y2 IN OUT NOCOPY NUMBER,
                              X3 IN OUT NOCOPY NUMBER,
                              Y3 IN OUT NOCOPY NUMBER,
                              X4 IN OUT NOCOPY NUMBER,
                              Y4 IN OUT NOCOPY NUMBER,
                              Xi IN OUT NOCOPY NUMBER,
                              Yi IN OUT NOCOPY NUMBER
                              )
            RETURN NUMBER Deterministic;

PROCEDURE    SET_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0);
PROCEDURE SET_MANY_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                     Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                      MBRs IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      skip PLS_INTEGER default 0,
                      tolerance NUMBER default 0.0,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0,
                      pbeg PLS_INTEGER default 0) ;
FUNCTION SWAP_EDGE_COORDS(Topology VARCHAR2,Edge_id NUMBER, Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY) RETURN VARCHAR2;
PROCEDURE TRACK_APP(pmsg VARCHAR2,p_table_name VARCHAR2 default NULL,p_log_type VARCHAR2,geom MDSYS.SDO_GEOMETRY default NULL);
PROCEDURE UPDATE_FACES_EACH_SIDE(edge_id NUMBER,Clip_face_table VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05);
PROCEDURE UPDATE_FACES_AROUND(node_to_move NUMBER,Intable VARCHAR2, InGeom_column VARCHAR2,SCHEMA VARCHAR2,topology VARCHAR2,tolerance NUMBER default 0.05);

-- UNit Tests
FUNCTION measure_zinger(Geom MDSYS.SDO_GEOMETRY) RETURN NUMBER;
Procedure try_check_close_edge(p_topo VARCHAR2,p_tolerance NUMBER default 0.05);
Procedure try_check_intersect_edge(p_topo VARCHAR2,p_tolerance NUMBER default 0.05);
PROCEDURE try_circulate_coordinates(shift PLS_INTEGER default 2);
PROCEDURE TRY_DISTANCE_FCN;
PROCEDURE try_check_for_self_intersect(ingeom MDSYS.SDO_GEOMETRY default NULL);
FUNCTION try_get_closest(pgeom1 MDSYS.SDO_GEOMETRY,pgeom2 MDSYS.SDO_GEOMETRY default NULL,tolerance  NUMBER default 0.05) RETURN NUMBER DETERMINISTIC;
PROCEDURE try_line_intersect(ingeom MDSYS.SDO_GEOMETRY default NULL);
procedure try_move;
PROCEDURE try_reverse_ordinates(xysin MDSYS.SDO_ORDINATE_ARRAY default NULL,lb pls_integer default 1,ub pls_integer default 0);
procedure try_set_many_mbr;
procedure test_circulate_node(Edge_id NUMBER,Topology VARCHAR2);
--==============================================================================
-- UTILITY ROUTINES Replace as required

   FUNCTION check_face_tab (p_gz_jobid         IN VARCHAR2,
                            p_topo             IN VARCHAR2,
                            p_face_tab         IN VARCHAR2,
                            p_face_pkc         IN VARCHAR2 DEFAULT 'FACE_ID',
                            p_log_type         IN VARCHAR2 DEFAULT 'TOPOFIX',
                            p_tolerance        IN NUMBER   DEFAULT .05,
                            p_sdo_srid         IN NUMBER   DEFAULT 8265)
      RETURN VARCHAR2;

   FUNCTION GZ_FIX_EDGE (
      p_gz_jobid                 IN VARCHAR2,
      p_topo                       IN VARCHAR2,
      p_log_type                 IN VARCHAR2 DEFAULT 'TOPOFIX',
      p_hold_universal         IN VARCHAR2 DEFAULT 'Y',
      p_tolerance                IN NUMBER DEFAULT .05,
      p_repeat                    IN NUMBER DEFAULT 2,
      p_checkcloseedge       IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2;
   
   
   PROCEDURE CHECK_CLOSE_EDGE_MGR (
      p_topo               IN VARCHAR2,
      p_tolerance          IN NUMBER,
      p_edgeids1           IN OUT GZ_TYPES.numberarray,
      p_edgeids2           IN OUT GZ_TYPES.numberarray,
      p_chunk_size         IN NUMBER DEFAULT 100000
   );

   FUNCTION CHECK_CLOSE_EDGE (
      pTopology        VARCHAR2,
      pedge_id          NUMBER,
      p_tolerance      NUMBER DEFAULT .05)
      RETURN NUMBER;

    FUNCTION CHECK_INTERSECTIONS_EDGE (
      p_line                     IN SDO_GEOMETRY,
      p_tolerance                IN NUMBER DEFAULT .05,
      p_debug                    IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2;
   
   FUNCTION IS_LOOP_EDGE_SELF_INTERSECTING (
      p_line                     IN SDO_GEOMETRY,
      p_tolerance                IN NUMBER DEFAULT .05,
      p_debug                    IN NUMBER DEFAULT NULL
   ) RETURN BOOLEAN;

   FUNCTION EDGE_TOO_CLOSE (
      pTopology        VARCHAR2,
      pedge_id1        NUMBER,
      pedge_id2        NUMBER,
      p_tolerance      NUMBER DEFAULT .05)
      RETURN NUMBER;

--

END;
/

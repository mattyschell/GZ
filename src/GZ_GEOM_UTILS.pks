CREATE OR REPLACE PACKAGE GZ_GEOM_UTILS
AUTHID CURRENT_USER
AS

   -- Reshape an "L" shaped edge that is too close to another edge by moving the "L"
   -- vertex away.
   PROCEDURE RESHAPE_LEDGE(State VARCHAR2,ptopology VARCHAR2, face_no NUMBER, pUS_STATE_table VARCHAR2 default 'ACS09_SL040',pEntityfp VARCHAR2 default 'STATEFP');
   -- Highly accurate distance alculation for geodetic coordinates
   FUNCTION ACCURATE_GCD( x1 IN OUT NOCOPY NUMBER, y1 IN OUT NOCOPY NUMBER,
                          x2 IN OUT NOCOPY NUMBER, y2 IN OUT NOCOPY NUMBER)
                          RETURN NUMBER DETERMINISTIC;
   -- Function to find the closest edge
   FUNCTION FIND_CLOSEST_EDGE(pTopology VARCHAR2,face_id NUMBER) RETURN MDSYS.SDO_LIST_TYPE;
   -- Function to create a reshaped edge that is not to close to the nearest edge.
   FUNCTION GET_NEAREST_EDGE (pTopology VARCHAR2,face_id NUMBER,edge_id IN OUT NOCOPY NUMBER,pInSchema VARCHAR2 default 'GZDEC10ST',tolerance NUMBER default 0.05,decim_digits NUMBER default 7) RETURN MDSYS.SDO_GEOMETRY;

-- Function to test whether a point is on a line and its near point on the
-- perpendicular from th point to the line.
FUNCTION Is_pt_on_Segment(xtest IN OUT NOCOPY NUMBER,ytest IN OUT NOCOPY NUMBER, -- test point
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,  -- the line
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                       xnear IN OUT NOCOPY NUMBER,ynear IN OUT NOCOPY NUMBER,
                       pu IN OUT NOCOPY NUMBER) RETURN NUMBER DETERMINISTIC;

-- Function to perform a Cross-product and determine the clockwiseness of
-- 3 points.
FUNCTION ORIENT2D (paX   NUMBER,  paY   NUMBER,
                      pbX   NUMBER,  pbY   NUMBER,
                      pcX   NUMBER,  pcY   NUMBER)

            RETURN          NUMBER
            Deterministic;

-- Function to perform a simple 2-D line intersection with no glancing
-- distance calculated. It intersects or does not or is paralle.
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

-- Function to reshape an edge in the topology by calling SDO_TOPO_MAP.CHANGE_EDGE_COORDS
FUNCTION SWAP_EDGE_COORDS(Topology VARCHAR2,Edge_id NUMBER, Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY) RETURN VARCHAR2;

FUNCTION before(pString VARCHAR2,
pSearchString VARCHAR2,pAppearance NUMBER DEFAULT 1)
RETURN VARCHAR2;


   PROCEDURE INSERT_SDOGEOM_METADATA (
      p_table_name      IN VARCHAR2,
      p_column_name     IN VARCHAR2,
      p_srid            IN NUMBER,
      p_tolerance       IN NUMBER DEFAULT .05
   );

   PROCEDURE ADD_SPATIAL_INDEX (
      p_table_name      IN VARCHAR2,
      p_column_name     IN VARCHAR2,
      p_srid            IN NUMBER,
      p_tolerance       IN NUMBER,
      p_local           IN VARCHAR2 DEFAULT NULL,
      p_parallel        IN NUMBER DEFAULT NULL,
      p_idx_name        IN VARCHAR2 DEFAULT NULL
   );

    FUNCTION REMOVE_HOLES (
      p_sql                IN VARCHAR2,
      p_area               IN NUMBER,
      p_tolerance          IN NUMBER
   ) RETURN sdo_geometry;


   FUNCTION REMOVE_HOLES (
      geom_in              IN SDO_GEOMETRY,
      p_id                 IN VARCHAR2,
      p_area               IN NUMBER,
      p_tolerance          IN NUMBER,
      p_log                IN VARCHAR2 DEFAULT 'N',
      p_logtable_name      IN VARCHAR2 DEFAULT NULL,
      p_validate_out       IN VARCHAR2 DEFAULT 'N'
   ) RETURN sdo_geometry;

   FUNCTION EXTRACT_HOLES (
      geom_in           IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

    FUNCTION MEASURE_SLIVER_WIDTH (
      geom_in           IN SDO_GEOMETRY,
      p_sample_kount    IN PLS_INTEGER DEFAULT 100,
      p_tolerance       IN NUMBER DEFAULT .00000005,
      p_debug           IN NUMBER DEFAULT 0
   ) RETURN NUMBER DETERMINISTIC;

   FUNCTION GZ_UNION (
      geom1_in             IN SDO_GEOMETRY,
      geom2_in             IN SDO_GEOMETRY,
      p_tolerance          IN NUMBER DEFAULT .00000005,
      p_debug              IN NUMBER DEFAULT 1,
      p_recursive          IN NUMBER DEFAULT 0
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   FUNCTION GZ_DIFFERENCE (
      geom1_in             IN SDO_GEOMETRY,
      geom2_in             IN SDO_GEOMETRY,
      p_tolerance          IN NUMBER DEFAULT .00000005,
      p_debug              IN NUMBER DEFAULT 1,
      p_recursive          IN NUMBER DEFAULT 0
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

    FUNCTION GET_XYS(
      Geom              IN MDSYS.SDO_GEOMETRY,
      ring1_to_find     PLS_INTEGER,
      edge1_to_find     PLS_INTEGER,
      ring2_to_find     PLS_INTEGER DEFAULT NULL,
      edge2_to_find     PLS_INTEGER DEFAULT NULL
   ) RETURN sdo_geometry;

   PROCEDURE GZ_ALIGN_EDGES (
      p_tab_name              IN VARCHAR2,
      p_geom_col_name         IN VARCHAR2,
      p_out_tab_name          IN VARCHAR2,
      p_tolerance             IN NUMBER DEFAULT .0000005,
      p_drop_out_tab          IN VARCHAR2 DEFAULT 'N',
      p_use_gz_difference     IN VARCHAR2 DEFAULT 'N'
   );

   PROCEDURE ALIGN_EDGES (
      geom_table_name         VARCHAR2,
      geom_column_name        VARCHAR2,
      output_table_name       VARCHAR2,
      tolerance               NUMBER,
      use_gz_difference       VARCHAR2 DEFAULT 'N'
   );

   PROCEDURE GZ_DUMP_SDO_EDGE_ARRAY (
      p_input         IN MDSYS.SDO_EDGE_ARRAY
   );

   FUNCTION REMOVE_CLOSE_ORDINATES (
      p_geom          IN SDO_GEOMETRY,
      p_distance      IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   FUNCTION WHERE_GEOM (
      g        IN SDO_GEOMETRY
   ) RETURN VARCHAR2 deterministic;

   FUNCTION ID_GEOM (
      g        IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY deterministic;

    FUNCTION DUMP_STRING_ENDPOINTS (
      geom1        IN SDO_GEOMETRY,
      geom2        IN SDO_GEOMETRY DEFAULT NULL
   ) RETURN VARCHAR2;

   FUNCTION DUMP_SDO_SUBELEMENTS (
      geom         IN SDO_GEOMETRY,
      indent       IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB;

   FUNCTION DUMP_SDO (
      geom         IN SDO_GEOMETRY,
      indent       IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB;

   FUNCTION DUMP_SDO_POINT (
      geom         IN SDO_POINT_TYPE,
      indent       IN VARCHAR2 DEFAULT ''
   ) RETURN VARCHAR2;

   FUNCTION DUMP_SDO_ELEM (
      geom         IN SDO_ELEM_INFO_ARRAY,
      indent       IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB;

   FUNCTION DUMP_SDO_ORDS (
      geom         IN SDO_ORDINATE_ARRAY,
      indent       IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB;

   FUNCTION FIX_INVALID_GEOMETRIES (
      p_schema          IN VARCHAR2,
      p_table_name      IN VARCHAR2,
      p_pkccol          IN VARCHAR2,
      p_gtype           IN NUMBER,
      p_topo            IN VARCHAR2,
      p_column_name     IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_flag_col        IN VARCHAR2 DEFAULT 'QC',
      p_intersect_size  IN NUMBER DEFAULT NULL,
      p_intersect_ratio IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2;

   FUNCTION ORDINATE_ROUNDER (
      p_geometry                IN SDO_GEOMETRY,
      p_places                  IN PLS_INTEGER DEFAULT 6
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++LRS LRS  LRS  LRS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION GZ_LOCATE_PT_DISTANCE (
      p_edge_geom       IN SDO_GEOMETRY,
      p_distance        IN NUMBER,
      p_tip             IN VARCHAR2,
      p_tolerance       IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION GZ_FIND_MEASURE (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY
   ) RETURN NUMBER DETERMINISTIC;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION GZ_FIND_MEASURE_PERCENT (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY,
      p_rounding       IN NUMBER DEFAULT 2
   ) RETURN NUMBER DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GZ_LOCATE_PT (
      p_edge           IN SDO_GEOMETRY,
      p_measure        IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GZ_PROJECT_PT (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY,
      p_tolerance      IN NUMBER DEFAULT 0.00000001
   ) RETURN SDO_GEOMETRY DETERMINISTIC;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION GET_COORD_INDEX (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY,
      p_debug          IN PLS_INTEGER DEFAULT 0
   ) RETURN PLS_INTEGER DETERMINISTIC;
   
   ------------------------------------------------------------------------------------
   --++END LRS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   


   FUNCTION REMOVE_DUPE_VERTEX (
      XYs                     IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY
   ) RETURN PLS_INTEGER;

  FUNCTION VALIDATE_LINES_WITH_CONTEXT (
      p_line1                  IN SDO_GEOMETRY,
      p_line2                  IN SDO_GEOMETRY,
      p_tolerance             IN NUMBER DEFAULT .05
   ) RETURN VARCHAR2;

   FUNCTION VALIDATE_LINES_WITH_CONTEXT (
      p_line                  IN SDO_GEOMETRY,
      p_tolerance             IN NUMBER DEFAULT .05,
      p_which_check           IN VARCHAR2 DEFAULT 'BOTH'
   ) RETURN VARCHAR2;

    FUNCTION Concatenate_lines_Sans(pline1 IN MDSYS.SDO_GEOMETRY,
    pline2 IN MDSYS.SDO_GEOMETRY,
    p_tolerance NUMBER DEFAULT 0.05) RETURN MDSYS.SDO_GEOMETRY;

    FUNCTION VALIDATE_TOUCHING_LINES (
      p_line1                    IN SDO_GEOMETRY,
      p_line2                    IN SDO_GEOMETRY,
      p_tolerance                IN NUMBER DEFAULT .05,
      p_round_digits             IN NUMBER DEFAULT 6
   ) RETURN VARCHAR2;

   FUNCTION SUBDIVIDE_TILE (
      p_geom               IN SDO_GEOMETRY,
      p_subdivisions       IN NUMBER
   ) RETURN GZ_TYPES.geomarray;



   FUNCTION GZ_TILE_TABLE (
      p_table           IN VARCHAR2,                     --can be schema.table
      p_tile_target     IN NUMBER,
      p_geom_col        IN VARCHAR2 DEFAULT 'GEOMETRY',
      p_whereclause     IN VARCHAR2 DEFAULT NULL,        --use a.xx syntax
      p_sdo_filter      IN SDO_GEOMETRY DEFAULT NULL,
      p_log_type        IN VARCHAR2 DEFAULT NULL,        --BUILD for ex
      p_log_tag         IN VARCHAR2 DEFAULT NULL,        --ex topology
      p_debug           IN NUMBER DEFAULT NULL
   ) RETURN GZ_TYPES.geomarray;


   FUNCTION EXPAND_MBR_PERCENT (
      p_mbr             IN SDO_GEOMETRY,
      p_percent         IN NUMBER DEFAULT 1
   ) RETURN SDO_GEOMETRY;

   FUNCTION DO_ORDINATES_MATCH (
      p_sdogeometry1       IN SDO_GEOMETRY,
      p_sdogeometry2       IN SDO_GEOMETRY,
      p_round              IN NUMBER DEFAULT NULL
   ) RETURN PLS_INTEGER DETERMINISTIC;

   FUNCTION SHOW_FACE_ORDINATE_DIFFERENCES (
      p_sdogeometry1       IN SDO_GEOMETRY,
      p_sdogeometry2       IN SDO_GEOMETRY,
      p_round              IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY DETERMINISTIC;






   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

END GZ_GEOM_UTILS;
/

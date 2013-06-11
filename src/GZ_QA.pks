create or replace
PACKAGE GZ_QA AUTHID CURRENT_USER AS
--==============================================================================
-- A package to compute quality assurance values for polygon entities in
-- two tables and compare the differences. Compute_QA_values is usually done
-- after generalization to ensure that the shape and area have not changed too much.
-- Two other entry points, check_all_vertices and flag_affected_entities work
-- on edges and 1) check to see if an edge is ungeneralized and then if so 2)
-- flag affeced polygon entities with a QA flag and a status showing the
-- reason for flagging.
--==============================================================================
-- Updated: 03/08/20111 Starting unit testing
    TYPE   AXES_TYPE IS RECORD ( angle NUMBER, Major_axis NUMBER, Minor_axis NUMBER,
                xLL NUMBER, yLL NUMBER, xUR NUMBER, yUR NUMBER, area NUMBER, xc NUMBER, yc NUMBER);
    TYPE   AXES_VTYPE is TABLE of AXES_TYPE;
-- Test entry point to find the moment of inertia of a geometry and print the
-- measurements found and return a single value (choice)
FUNCTION INERTIA_PARAM( geometry MDSYS.SDO_GEOMETRY default NULL, choice NUMBER default 1.)  RETURN NUMBER;

-- Procedure to histogram a column from a table and make an output graph (which
-- can be plotted with the Mapviewer)
PROCEDURE HISTOGRAM_TABLE(pInTable VARCHAR2, pInColumn VARCHAR2,
                      InBottom NUMBER default NULL,
                      InTop NUMBER default NULL,
                      cells  NUMBER default 10,Graph_Table VARCHAR2 default 'HTABLE',mark number default 2.);
-- Function to histogram a particular column of a tableand return a histogram
FUNCTION HISTOGRAM(pInTable VARCHAR2, pInColumn VARCHAR2,
                      Length_Array IN OUT NOCOPY  MDSYS.SDO_LIST_TYPE,
                      Range_Array IN OUT NOCOPY  MDSYS.SDO_LIST_TYPE,
                      InBottom NUMBER default NULL,
                      InTop NUMBER default NULL,
                      cells NUMBER default 10)
RETURN MDSYS.SDO_LIST_TYPE;
-- Procedure to draw a graph
PROCEDURE DRAW_GRAPH( Graph_table VARCHAR2, pgraph_type VARCHAR2, Title VARCHAR2, SubTitle VARCHAR2, Xin IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, Yin IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,marker NUMBER default NULL);
-- Procedure to draw a rectangle between LL and UR
PROCEDURE DRAW_RECTANGLE( Graph_Table VARCHAR2, x1 NUMBER, y1 NUMBER, x2 NUMBER, y2 NUMBER, id VARCHAR2,color number default 1);
-- Procedure to draw a line between 2 points
PROCEDURE DRAW_LINE( Graph_Table VARCHAR2, x1 NUMBER, y1 NUMBER, x2 NUMBER, y2 NUMBER, id VARCHAR2,color number default 1);
-- Procedure to draw a point
PROCEDURE Draw_Point( Graph_Table VARCHAR2, x1 NUMBER, y1 NUMBER, id VARCHAR2);
FUNCTION ANGLE(x0 NUMBER,y0 NUMBER,x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,degrees boolean default FALSE) RETURN NUMBER DETERMINISTIC;
-- Function to work out the maximum area swept out by the line and an intersecting
-- straight line drawn from start node to end node.
FUNCTION AREA_ALONG_LINE(In_start PLS_INTEGER, In_End PLS_INTEGER,
                         Xys     IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         In_Degrees Boolean default TRUE,
                         pInUseProjection Boolean default FALSE
                                ) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION AREA_OF_LINE(XYs IN MDSYS.SDO_ORDINATE_ARRAY,
                      In_Degrees Boolean default TRUE,
                      pInUseProjection Boolean default FALSE) RETURN NUMBER;
-- Function to break a closed loop into 2 lines which area_along a line can process.
FUNCTION AREA_OF_LOOP(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      In_Degrees Boolean default TRUE,
                      way_point IN OUT NOCOPY PLS_INTEGER, SRID NUMBER,
                      pInUseProjection Boolean default FALSE) RETURN NUMBER;

FUNCTION C_TAN(InX NUMBER)  RETURN NUMBER Deterministic;
-- Function to calculate the area and the centroid
FUNCTION CENTROID( XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Info MDSYS.SDO_ELEM_INFO_ARRAY, Xc IN OUT NUMBER,Yc IN OUT NUMBER,Positive BOOLEAN default TRUE) RETURN MDSYS.SDO_LIST_TYPE;
-- Function to create equal ranges
FUNCTION CREATE_EQUAL_RANGES(pInTable VARCHAR2, pInColumn VARCHAR2,
                      Amounts IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      InBottom NUMBER default NULL,
                      InTop NUMBER default NULL,
                      Incells  NUMBER default 20,
                      Maximum_range_count NUMBER default 0)
RETURN  MDSYS.SDO_LIST_TYPE;
--==============================================================================
-- Main Entry point to determine Quality Assurance values for 2 tables
PROCEDURE COMPUTE_QA_VALUES( UnGen_Schema VARCHAR2, UnGenView VARCHAR2, UnGenColumn VARCHAR2, Gen_Schema VARCHAR2, GenView VARCHAR2, GenColumn VARCHAR2, compare_table_name VARCHAR2, target_scale NUMBER, CompareGenColumn VARCHAR2 default 'RING_GEOM',parea_check NUMBER default NULL, shape_check NUMBER default 1.5);

FUNCTION COMPUTE_QA_VALUES_2007( geo_id VARCHAR2, Ugeometry  MDSYS.SDO_GEOMETRY, Ggeometry MDSYS.SDO_GEOMETRY, GenColumn VARCHAR2, compare_table_name VARCHAR2,target_scale NUMBER,parea_check NUMBER default NULL, shape_check NUMBER default 1.5) RETURN NUMBER;
--==============================================================================

PROCEDURE FLAG_AFFECTED_ENTITIES(pReport_Table VARCHAR2,pTopology VARCHAR2,pSchemaName VARCHAR2,pBad_Entities_table VARCHAR2 default NULL);
FUNCTION CHECK_NEARBY(Subject_XYs         IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                        Subject_Info_Array  IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                        Subject_MBR         IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         XYs                IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         Info_Array         IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                         MBR                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Codes              IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Subject_Segs       IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Matches            IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Widths_p           IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Widths_q           IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Lengths            IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Xps                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Yps                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Xqs                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Yqs                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         SRID               NUMBER,
                         angle_tolerance    NUMBER default 35.,
                         dist_tolerance     NUMBER default 500.,
                         segment_tolerance  NUMBER default 0.,
                         find               VARCHAR2 default 'B',
                         the_same           BOOLEAN default FALSE)
RETURN PLS_INTEGER;
FUNCTION CIRCULATE_GEOMETRY(Geometry MDSYS.SDO_GEOMETRY,shift number) RETURN MDSYS.SDO_GEOMETRY;

-- Function to shift coordinates in a loop circularly so the loop has a different
-- start and end vertex.
FUNCTION CIRCULATE_COORDINATES( Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, shift PLS_INTEGER) RETURN MDSYS.SDO_ORDINATE_ARRAY;
-- Best Distance function for 8265
Function Distance_Fcn(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER;
FUNCTION divergence(x11 NUMBER,y11 NUMBER,x22 NUMBER,y22 NUMBER, x33 NUMBER, y33 NUMBER, x44 NUMBER, y44 NUMBER,cpx NUMBER, cpy NUMBER,cutoff NUMBER,
                     xp in out nocopy number, yp in out nocopy number, xq in out nocopy number,yq in out nocopy number,perp_angle_tolerance NUMBER default 0.707107) RETURN MDSYS.SDO_LIST_TYPE;
--  Procedure to find an interior point, either the centroid or another point
-- that is always in the polygon
PROCEDURE INTERIOR_POINT( PolyXYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Info MDSYS.SDO_ELEM_INFO_ARRAY, XC IN OUT NOCOPY NUMBER,YC IN OUT NOCOPY NUMBER);
-- Function to compare 2 coordinate arrays and return a list of matching sections
FUNCTION COMPARE_EDGE_COORDINATES (XYs_Gen IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,match_count IN OUT NOCOPY PLS_INTEGER,bad_match_sections IN OUT NOCOPY PLS_INTEGER,decim_digits PLS_INTEGER default 6,bad_length PLS_INTEGER default 4) RETURN MDSYS.SDO_LIST_TYPE;
-- Function to compute eigenvectors of a u matrix
FUNCTION EIGENVECTORS(mu_matrix MDSYS.SDO_LIST_TYPE,id VARCHAR2 default NULL) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION Field_exists(pTableName VARCHAR2, pFieldName VARCHAR2, pSchemaName VARCHAR2 DEFAULT USER) RETURN BOOLEAN;
-- Function to find the attitude angle usig the MBR (incomplete)
FUNCTION Find_Attitude(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY) RETURN NUMBER;
-- Function to find a coordinate (either the x or the y or both) on a line either
-- exactly or within a small epsilon.
FUNCTION FIND_NEARBY(   try PLS_INTEGER, try_end PLS_INTEGER, pstart PLS_INTEGER, Inpend PLS_INTEGER,
                         xbuffer IN OUT NOCOPY NUMBER, ybuffer IN OUT NOCOPY NUMBER,
                         Subject_XYs        IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         Subject_Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                         Subject_MBR        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         XYs                IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         Info_Array         IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                         Codes              IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Subject_Segs       IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Matches            IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Widths_p           IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Widths_q           IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Lengths            IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Xps                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Yps                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Xqs                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         Yqs                IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                         next               IN OUT NOCOPY PLS_INTEGER,
                         SRID               NUMBER,
                         angle_tolerance    NUMBER,
                         dist_tolerance     NUMBER,
                         pfind              VARCHAR2 default 'B',
                         the_same           BOOLEAN default FALSE)
RETURN PLS_INTEGER;
FUNCTION FIND_XORY(X NUMBER, Y NUMBER,XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,epsilon NUMBER default 0.0) RETURN NUMBER;
-- Function to find a single polygon in another 2007 geometry.
FUNCTION FIND_XES_AND_YES(GXYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,UXYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Uinfo IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,UMBR IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,round_it NUMBER default 6) RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION OLDFIND_XES_AND_YES(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,UXYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Uinfo MDSYS.SDO_ELEM_INFO_ARRAY,round_it NUMBER default 6) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION GEO_BEARING(px0 NUMBER,py0 NUMBER,px1 NUMBER,py1 NUMBER,old_bearing IN OUT NOCOPY NUMBER)
                RETURN NUMBER DETERMINISTIC;
-- Function to get the next outer ring in a geometry
FUNCTION GET_OUTER_RING(ingeom MDSYS.SDO_GEOMETRY,ring IN OUT NOCOPY NUMBER) return MDSYS.SDO_GEOMETRY;
--FUNCTION GET_NARROW_WIDTH(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 500.,choose VARCHAR2 default 'SMALL',pgap PLS_INTEGER default 2) RETURN NUMBER;
--FUNCTION GET_NARROW_SEGS(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 500.,choose VARCHAR2 default 'SMALL',pgap PLS_INTEGER default 2) RETURN MDSYS.SDO_GEOMETRY;

FUNCTION GET_WIDTHS(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 500.,pgap PLS_INTEGER default 2,no_to_find PLS_INTEGER default 20,return_where BOOLEAN default TRUE) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION GET_Pipe_WIDTHS(Geom IN MDSYS.SDO_GEOMETRY,Where_is IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Matches IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Distances IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                          cutoff_width NUMBER default 500.,pgap PLS_INTEGER default 2,no_to_find PLS_INTEGER default 20) RETURN MDSYS.SDO_LIST_TYPE;
-- Function to measure the long axis of a polygon
FUNCTION MEASURE_ELONGATE_LENGTH(ingeometry MDSYS.SDO_GEOMETRY,Xc_in IN OUT NUMBER,Yc_in IN OUT NUMBER,radii IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,id VARCHAR2 default NULL) RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION METERS_TO_DEGREES(X_OR_Y VARCHAR2 default 'X',pLength NUMBER, pLatitude NUMBER, tolerance NUMBER default 0.01) RETURN NUMBER;
-- Function to compute the moments of inertia of a 2-D polygon
FUNCTION MOMENTS(inXYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Info MDSYS.SDO_ELEM_INFO_ARRAY, round_it PLS_INTEGER default 12,debug VARCHAR2 default 'FALSE') RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION Perpendicular(xin IN OUT NOCOPY NUMBER,yin IN OUT NOCOPY NUMBER,  -- the point
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,  -- the line
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                       xnear IN OUT NOCOPY NUMBER,ynear IN OUT NOCOPY NUMBER,  -- the perpendicular
                       meters BOOLEAN default FALSE,  -- when true return meters
                       perp_angle_tolerance NUMBER default 0.)
                       RETURN NUMBER deterministic;
-- Function to check if a point is in a polygon
FUNCTION POINT_IN_POLY_CC( Xc     NUMBER,
                           Yc     NUMBER,
                           XYs    IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                           pLB    NUMBER default 1,
                           pUB    NUMBER default 0
)
         RETURN         PLS_INTEGER Deterministic;
FUNCTION Radial_distances( XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Xc IN OUT NUMBER, Yc IN OUT NUMBER, no_of_radii PLS_INTEGER default 16,SRID NUMBER default 8265.) RETURN MDSYS.SDO_LIST_TYPE;
-- Function to rotate the coordinates by an angle in degrees
FUNCTION ROTATE_COORDINATES(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,angle NUMBER,x0 NUMBER,y0 NUMBER) RETURN MDSYS.SDO_ORDINATE_ARRAY;
-- Very faster arctangent function
FUNCTION FASTER_ATAN2(YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER;
-- Function to return an accurate 2-D distance between2 points
FUNCTION FAST_DISTANCE( x1 IN OUT NOCOPY NUMBER, y1 IN OUT NOCOPY NUMBER,
                        x2 IN OUT NOCOPY NUMBER, y2 IN OUT NOCOPY NUMBER,SRID NUMBER default 8265.)
                       RETURN NUMBER DETERMINISTIC;
-- Function to find and return the biggest outer ring                       
FUNCTION GET_BIG_RING(ingeom MDSYS.SDO_GEOMETRY) return MDSYS.SDO_GEOMETRY;
-- Function to return the number of rings in a geometry
Function GetNumRings( p_geometry in mdsys.sdo_geometry, p_element_type VARCHAR2 default NULL ) Return Number Deterministic;
FUNCTION Find_Distances(x IN OUT NOCOPY NUMBER,y IN OUT NOCOPY NUMBER,XYS IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,GC_distances  IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,pstart PLS_INTEGER default 1,pend PLS_INTEGER default 0,pnvert_outer PLS_INTEGER default 0,forwards VARCHAR2 default 'ASC') RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION find_nearest_edge_seg(px0 NUMBER,py0 NUMBER,Poly_Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,poly_MBR IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,allowed_range MDSYS.SDO_LIST_TYPE,near_x IN OUT NOCOPY NUMBER,near_y IN OUT NOCOPY NUMBER ,check_cosine NUMBER default 0.,nvert_outer PLS_INTEGER) RETURN PLS_INTEGER;
-- Function to return the length of the pipe (NULL when nothing found);
FUNCTION FIND_PIPES(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default'A', max_width NUMBER default 150.,  minimum_len NUMBER default 1000.,aspect_ratio NUMBER default 2.,max_angle_diff NUMBER default 35.) RETURN NUMBER;
-- Function that does the work for Find and Get
FUNCTION FIND_GET_PIPES(Geom IN MDSYS.SDO_GEOMETRY,pipe_type VARCHAR2 default 'A', max_width NUMBER default 150.,minimum_len NUMBER default 1000.,aspect_ratio NUMBER default 2.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION GET_PIPE_Segs(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., minimum_len NUMBER default 1000.,aspect_ratio NUMBER default 2.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY;
-- Function to return the pipe shape as a geometry (NULL when nothing found);
FUNCTION GET_PIPES(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., minimum_len NUMBER default 1000.,aspect_ratio NUMBER default 2.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY;


FUNCTION GET_Small_Angle(Geom IN MDSYS.SDO_GEOMETRY, angle NUMBER default 5.) RETURN NUMBER;
FUNCTION Get_Small_Angle_Geom(Geom IN MDSYS.SDO_GEOMETRY,angle NUMBER default 5.) RETURN MDSYS.SDO_GEOMETRY;

FUNCTION GET_Angles(Geom IN MDSYS.SDO_GEOMETRY,smallest NUMBER default 0.0) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION GET_SHORT_GCD(Geom IN MDSYS.SDO_GEOMETRY) RETURN NUMBER DETERMINISTIC;
-- Function callable from SQL to return the (great circle) distances between adjacent vertices
FUNCTION GET_GCDS(Geom IN MDSYS.SDO_GEOMETRY,psmallest VARCHAR2 default 'SHORT',coords VARCHAR2 default 'YES') RETURN MDSYS.SDO_LIST_TYPE;
-- Function to returns a list of short edges, Positive means they are on the state line, -ve means no.
FUNCTION GET_XYS(Geom IN MDSYS.SDO_GEOMETRY, pring1 PLS_INTEGER, pseg1_to_find PLS_INTEGER,pring2 PLS_INTEGER DEFAULT NULL,pseg2_to_find PLS_INTEGER default NULL,convert VARCHAR2 default '8265') RETURN sdo_geometry;
FUNCTION GET_SHORT_EDGES(pTopology VARCHAR2,face_id NUMBER,pInSchema VARCHAR2 default 'GZDEC10ST',tolerance NUMBER default 0.05,decim_digits NUMBER default 7) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION LINE_PARALLEL( X1 IN OUT NOCOPY NUMBER,
                              Y1 IN OUT NOCOPY NUMBER,
                              X2 IN OUT NOCOPY NUMBER,
                              Y2 IN OUT NOCOPY NUMBER,
                              X3 IN OUT NOCOPY NUMBER,
                              Y3 IN OUT NOCOPY NUMBER,
                              X4 IN OUT NOCOPY NUMBER,
                              Y4 IN OUT NOCOPY NUMBER,
                              Xp IN OUT NOCOPY NUMBER,
                              Yp IN OUT NOCOPY NUMBER,
                              Xq IN OUT NOCOPY NUMBER,
                              Yq IN OUT NOCOPY NUMBER,
                              distance_p IN OUT NOCOPY NUMBER,
                              distance_q IN OUT NOCOPY NUMBER,
                              SRID NUMBER,
                              cos_angle_tolerance NUMBER default 0.8660254,
                              dist_tolerance NUMBER default 500.0,
                              perp_angle_tolerance NUMBER default 0.707107)
            RETURN NUMBER Deterministic;
Function Get_Pipe_Geom(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Info MDSYS.SDO_ELEM_INFO_ARRAY,start_vertex PLS_INTEGER,across_vertex PLS_INTEGER,
                        
                        which PLS_INTEGER,cutoff NUMBER,PSL IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,average_width IN OUT NOCOPY NUMBER,accum_length IN OUT NOCOPY NUMBER) RETURN MDSYS.SDO_GEOMETRY;

-- Very accurate arctangent function
FUNCTION NEW_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER;
FUNCTION OLDFIND_GET_PIPES(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150.,  minimum_len NUMBER default 1000.,aspect_ratio NUMBER default 2.,max_angle_diff NUMBER default 25.) RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION PACK_GEOMETRY(geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY) RETURN VARCHAR2;
FUNCTION Perimeter(XYs IN MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER default 8265.) RETURN NUMBER DETERMINISTIC;
FUNCTION POLYGON_AREA(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER default 8265.)
           RETURN NUMBER DETERMINISTIC;
Function QUICK_SINCOS(Xin NUMBER,cosX IN OUT NOCOPY NUMBER,in_degrees VARCHAR2 default 'N') RETURN NUMBER Deterministic;

-- Function to measure the area of a spherical triangle
FUNCTION SPHTRI_AREA(x1 IN NUMBER, y1 IN NUMBER,
                        x2 IN NUMBER, y2 IN NUMBER,
                        x3 IN NUMBER, y3 IN NUMBER,SRID NUMBER default 8265.)
   RETURN NUMBER DETERMINISTIC;
-- Procedure to sort 2 arrays
PROCEDURE SHELLSORT2( Arr         IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Arr2   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       InLB          PLS_INTEGER default 1,
                       InUB          PLS_INTEGER default 0,
                       forwards       VARCHAR2 default 'ASC');
                       procedure sort_also(Arr in out nocopy Mdsys.sdo_list_type,Order_array in out nocopy mdsys.sdo_list_type);
PROCEDURE STABLESORT3( Arr           IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Order_array   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Extra_array   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       InLB          PLS_INTEGER default 1,
                       InUB          PLS_INTEGER default 0,
                       forwards      BOOLEAN default TRUE);
--==============================================================================
-- Start of unit testing procedures
PROCEDURE try_Angle;
procedure try_inertia_param(ingeom MDSYS.SDO_GEOMETRY default NULL);
function try_interior_point(ingeom MDSYS.SDO_GEOMETRY default NULL,px number default NULL,py number default NULL) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION try_boyce(ingeom MDSYS.SDO_GEOMETRY default NULL) return NUMBER;
procedure try_centroid(ingeom MDSYS.SDO_GEOMETRY default NULL);
procedure try_circulate_coordinates(shift PLS_INTEGER default 2);
PROCEDURE try_faster_atan2(yy NUMBER,XX NUMBER);
PROCEDURE try_find_xory(Geom MDSYS.SDO_GEOMETRY default NULL, px NUMBER default NULL,py NUMBER default NULL);
procedure try_get_gcds(ingeom MDSYS.SDO_GEOMETRY default NULL,smallest VARCHAR2 default 'SHORT');
PROCEDURE try_line_parallel(ingeom MDSYS.SDO_GEOMETRY default NULL);
FUNCTION try_get_pipe_geom(width number default 100.) return MDSYS.SDO_GEOMETRY;
PROCEDURE try_moments(ingeom MDSYS.SDO_GEOMETRY default NULL);
procedure try_point_in_poly_cc(ingeom MDSYS.SDO_GEOMETRY default NULL,px number default NULL,py number default NULL);
PROCEDURE try_quick_sincos;
procedure try_rotate_coordinates(angle NUMBER default 90.);
-- unit test for shellsort2
procedure try_SHELLSORT2;

procedure try_FASTER_ATAN2;
PROCEDURE try_area_of_line(Geom MDSYS.SDO_GEOMETRY default NULL);
--==============================================================================
-- Main script entry point to build the QA views

PROCEDURE Build_QA_TBLS_VIEWS(
   pGenSchema VARCHAR2,
   pGenTopology VARCHAR2,
   pGenSdoGeomCol VARCHAR2,
   pUnGenSchema VARCHAR2,
   pUnGenTopology VARCHAR2,
   pUnGenSdoGeomCol VARCHAR2,
   pOutPutTableSuffix VARCHAR2,
   pTARGET_SCALE NUMBER,
   pCOMPARE_GEN_COLUMN VARCHAR2,
   pAREA_CHECK NUMBER, -- default NULL,
   pSHAPE_CHECK NUMBER, -- default 1.5,
   pEntireTopology VARCHAR2, -- default 'Y',
   pGenTableName VARCHAR2, -- default NULL,
   pUnGenTableName VARCHAR2, -- default NULL,
   pMissingRecTBL  VARCHAR2,
   pMissingRecKey  VARCHAR2,
   pMissingRecFeatTbl VARCHAR2
   );
-- Procedure to check and rectify a geometry with a 13356 or 13349 error
FUNCTION CHECK_AND_RECTIFY(Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,tolerance NUMBER default 0.05) RETURN VARCHAR2;
-- Procedure to check all the vertices in 2 different table geometries and determine the extent of
-- generalization and report where matches of 4 or longer runs were found.
PROCEDURE CHECK_ALL_VERTICES( pInTable VARCHAR2, pGeometry_column VARCHAR2, pInTable2 VARCHAR2, pnewGeometry_column VARCHAR2, pOutput_table VARCHAR2, match_length PLS_INTEGER default 4, area_check NUMBER,percent_ungen NUMBER default 20., max_consecutive_match NUMBER default 20., pInclude_state_edges VARCHAR2 default 'NO', pprint VARCHAR2 default 'NO');
-- Procedure to check for redundant vertices which will fail generalization.
PROCEDURE CHECKFOR_CLOSE_XYS(pInTable VARCHAR2,pId_column VARCHAR2,pGeometry_column VARCHAR2,pOutput_Table VARCHAR2 default 'SHORT_EDGES',pprint VARCHAR2 default 'NO',ptolerance NUMBER default 0.05);

-- Function to check if a table exists in a schema.
FUNCTION TABLE_EXISTS ( pInTable  IN VARCHAR2, pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN;
-- Procedure to report an error and do a walkback to the offending line
PROCEDURE find_missing_records(pTopo1 VARCHAR2, pTopo2 VARCHAR2, pOutputTable VARCHAR2, pKeyField VARCHAR2, pFeatTable VARCHAR2 default NULL);
PROCEDURE create_gz_missing_geoids (p_table_name VARCHAR2);
FUNCTION NEW_GZ_MISSING_GEOIDS RETURN GZ_TYPES.GZ_MISSING_GEOIDS PIPELINED;
END GZ_QA;
/
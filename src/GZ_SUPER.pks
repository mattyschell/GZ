create or replace
PACKAGE GZ_SUPER
AUTHID CURRENT_USER
AS
--

--Updates:
--    12/01/2010 More work on self intersections
--    11/26/2010 Replaced Shellsort
--    11/23/2010 TO handle self intersections
--     9/20/2010 To handle coincident test point for island and horiz or vert segment
--     9/13/2010 To avoid going on the wrong side of islands
--     8/27/2010 To avoid "An island of face nn has an edge coincident with
--                   outer boundary" we add the 2nd and next to last vertices
--                   to figure 8 polygons.
--     8/16/2010 To drop Topomap if last run died. Also to name Edge Table
-- A package to supervise generalization of edges in a topology.
-- Supervision (hence SUPER) has one overall goal: - maintain relationships
-- that are unaffected by stretching the surface - so called topological
-- relationships. So if we reshape an edge by stretching the region around it,
-- the relationships with other edges must not change. So intersecting itself or
-- other edges must be avoided by modifying the scale where necessary for parts
-- of the edge being generalized. If you imagine a river edge with two banks on
-- either side, you can foresee that generalization of the river edge requires
-- care to avoid the banks. To accomplish  this, the procedure Simplify requires
-- a work table (Edges_toprocess) which has a nearby edge list for each edge and
-- a new or current geometry which acts as state variable to describe the current
-- shape of each edge and thus the state of the topology.  It will make this table
-- automatically if it has not been already made by calling Run_Make_Edge_Table.
--
-- Besides updating the topology with the new geometries, Simplify writes 2 status
-- tables: Edges_done and Bad_edges. The 1st table is queried to avoid processing
-- an edge twice. Since this table has the entity (state) listed it is convenient
-- to monitor progress. Bad_edges has edge_ids and the status messages for edges
-- that could not be inserted into the topology.
--
-- Assumptions: Code assumes you will run from within the schema. This only
-- affects the call to Index_exists.

-- So the entry points are:
--SQL>  exec GZ_SUPER.run_make_edge_table('MT','AA','FACE',Edges_toprocess','State_Edge_table');

-- This call  processes Maryland using ZONE at 1:500,000 scale using the topology 'MT' and the run_flag 'AA':
--SQL>  exec GZ_SUPER.simplify('24','MT','AA','FACE','ZONE',500000.,1.,'ACS09_SL040','Edges_toprocess',NULL,'Edges_Done','Bad_Edges','State_Edge_table');
-- Note the lower case names are work or status tables.
--
PROCEDURE SIMPLIFY (pstate VARCHAR2,pTopology VARCHAR2,run_flag VARCHAR2 default NULL,pmethod VARCHAR2 default 'ZONE',ptarget_scale NUMBER DEFAULT 500000., pnice NUMBER default 1., pEntity_Table VARCHAR2 default 'ACS09_SL040',pedges_table VARCHAR2 default 'EDGES_TOPROCESS',pskip_edges_table VARCHAR2 default NULL, pDone_edges_table VARCHAR2 default 'EDGES_DONE',pBad_Table VARCHAR2 default 'BAD_EDGES',pState_Edge_Table VARCHAR2 default 'STATE_EDGE_TABLE',pEntityfp VARCHAR2 default 'STATEFP',pEdge_attribute_table VARCHAR2 default 'EDGE',pclip_face_table VARCHAR2 default 'FACE',drop_work_tables VARCHAR2 default 'TRUE');
-- Assemble an edge that fails insertion into the topology at the target scale
-- by compositing parts at different scales

PROCEDURE Add_geom_metadata(pTableName VARCHAR2,pColumnName VARCHAR2,pInSchema VARCHAR2 default NULL,SRID NUMBER default 8265.,xLL NUMBER default -180.,yLL NUMBER default -90.,xUR NUMBER default 180.,yUR NUMBER default 90.,tolerance NUMBER default 0.05) ;
FUNCTION ASSEMBLE_EDGE(Status IN OUT NOCOPY VARCHAR2, Edge_id NUMBER,start_node NUMBER,end_node NUMBER,Segments IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,pscale NUMBER,nice NUMBER,nearby_edges MDSYS.SDO_LIST_TYPE,orig_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
     gen_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,Edges_Table VARCHAR2,method VARCHAR2 default 'ZONE',Topology VARCHAR2,target_scale NUMBER) RETURN NUMBER;


FUNCTION BUILD_MBR(topology     VARCHAR2,                                      
                    state_code      VARCHAR2,
                    min_x           NUMBER default NULL,
                    min_y           NUMBER default NULL,
                    max_x           NUMBER default NULL,
                    max_y           NUMBER default NULL,
                    tolerance       NUMBER default 0.05,

                    pskip_Table     VARCHAR2 default NULL,
                    pDone_Table     VARCHAR2 default 'EDGES_DONE',
                    pEdges_Table    VARCHAR2 default 'EDGES_TOPROCESS') RETURN MDSYS.SDO_ORDINATE_ARRAY;
FUNCTION CENTROID(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Xc IN OUT NUMBER,Yc IN OUT NUMBER,SRID NUMBER) RETURN NUMBER;
PROCEDURE CHECK_ALL_VERTICES(pInTable VARCHAR2,PGeometry_column VARCHAR2,pInTable2 VARCHAR2,PnewGeometry_column VARCHAR2,pOutput_table VARCHAR2,pInclude_state_edges VARCHAR2 default 'NO',pprint VARCHAR2 default 'NO');
FUNCTION CHECK_CLOCK_WISENESS(geom1 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,polygeom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,iseg IN OUT NOCOPY PLS_INTEGER,test_point IN OUT NOCOPY PLS_INTEGER,allow_loops PLS_INTEGER) RETURN NUMBER;

FUNCTION CHECK_FOR_SELF_INTERSECT(Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,dec_digits NUMBER default 7,thousand NUMBER default 100000.) RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION CHECK_POLYLR(geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,nearby_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,new_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY) RETURN MDSYS.SDO_LIST_TYPE;
PROCEDURE CHECK_TABLES(pstate VARCHAR2,run_flag VARCHAR2,pEntityFP VARCHAR2,Topology VARCHAR2,pEdge_attribute_table VARCHAR2,pclip_face_table VARCHAR2,pEntity_table VARCHAR2,pSkip_Table IN OUT NOCOPY VARCHAR2,pEdges_Table VARCHAR2,pDone_Table VARCHAR2,pState_Edge_table VARCHAR2,drop_work_tables VARCHAR2);

FUNCTION COLUMN_EXISTS ( pInColumn  IN VARCHAR2,pInTable  IN VARCHAR2, pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN;

-- Function to find the number of consecutive vertices that are the same between
-- the original geometry and the generalized one.
FUNCTION CHECK_MATCHING_XYS(tested IN OUT NOCOPY PLS_INTEGER,XyOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_Array,origXyOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_Array) RETURN PLS_INTEGER;

FUNCTION COMPARE_EDGE_COORDINATES (XYs_Gen IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,match_count IN OUT NOCOPY PLS_INTEGER,bad_match_sections IN OUT NOCOPY PLS_INTEGER,decim_digits PLS_INTEGER default 6,bad_length PLS_INTEGER default 4) RETURN MDSYS.SDO_LIST_TYPE;
PROCEDURE COPY_GEOM_COORDINATES(XYORD IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,pstart PLS_INTEGER, Iend PLS_INTEGER,toXYORD IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,next IN OUT NOCOPY PLS_INTEGER);
PROCEDURE CREATE_GZ_SUPER_TABLE(p_schema VARCHAR2 DEFAULT NULL, p_table_name VARCHAR2,Table_Abbrev VARCHAR2);
PROCEDURE CREATE_GZ_SUPER_DONE(p_schema VARCHAR2 DEFAULT NULL,p_table_name VARCHAR2);
PROCEDURE CREATE_GZ_SUPER_EDGES(p_schema VARCHAR2 DEFAULT NULL,p_table_name VARCHAR2);
PROCEDURE CREATE_GZ_SUPER_STATE(p_schema VARCHAR2 DEFAULT NULL,p_table_name VARCHAR2);

FUNCTION EDGE_SIMPLIFY(Topology VARCHAR2,Edge_id NUMBER,geometry  MDSYS.SDO_GEOMETRY,
pscale NUMBER default 500000.,nice NUMBER default 1.,tolerance NUMBER default 0.05,dec_digits PLS_INTEGER default 6,method VARCHAR2 default 'ZONE',Edges_Table VARCHAR2 DEFAULT 'EDGES_TOPROCESS')  RETURN MDSYS.SDO_GEOMETRY;
-- Simplifies an edge with the generalization function. If there are any intersectionss with nearby
-- edges, it calls find_intersection segment to lcate the segment and then get_split_vertices and
-- assemble_edge to build a new edge that does not intersect any nearby edges.
FUNCTION EDGE_SIMPLIFY(Status IN OUT NOCOPY VARCHAR2,Edge_id NUMBER,orig_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,elength NUMBER,uniq NUMBER,pscale IN OUT NOCOPY NUMBER,nice NUMBER,tolerance NUMBER,dec_digits PLS_INTEGER default 6,method VARCHAR2 default 'ZONE',Edges_Table VARCHAR2 DEFAULT 'EDGES_TOPROCESS',Topology VARCHAR2)  RETURN MDSYS.SDO_GEOMETRY;
Function estimate_area(x0 number,y0 number,x1 number,y1 number,x2 number,y2 number,
                            sinyc in out nocopy number,cosyc in out nocopy number,sin5yc in out nocopy number,factor in out nocopy number) return number;

FUNCTION FAST_DISTANCE( x1 IN OUT NOCOPY NUMBER, y1 IN OUT NOCOPY NUMBER, x2 IN OUT NOCOPY NUMBER, y2 IN OUT NOCOPY NUMBER,SRID NUMBER default 8265.) RETURN NUMBER DETERMINISTIC;
FUNCTION Fast_vincenty_gcd(x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,units VARCHAR2 DEFAULT 'm') RETURN NUMBER DETERMINISTIC;
FUNCTION FIND_COORDINATE_WITHIN_EDGE(x NUMBER,y NUMBER,XYOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_Array,tt IN OUT NOCOPY NUMBER,ptolerance NUMBER default 0.05)  RETURN NUMBER;
FUNCTION Find_Distances(x IN OUT NOCOPY NUMBER,y IN OUT NOCOPY NUMBER,XYS IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,short_distance  IN OUT NOCOPY NUMBER,exclude BOOLEAN default FALSE,pstart PLS_INTEGER default 1,pnvert_outer PLS_INTEGER default 0,forwards BOOLEAN default TRUE) RETURN NUMBER;
-- Figures out the range of vertices that a particular segment intereacts with
-- nearby edges.

FUNCTION FIND_INTERSECTION_SEGMENT(geometry1 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                                  geometry2 IN OUT NOCOPY MDSYS.SDO_GEOMETRY
                                  ) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION FIND_MATCHING_SEGMENT(seg PLS_INTEGER, XYOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,GenXYOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,epsilon NUMBER default 1.E-6) RETURN PLS_INTEGER;
FUNCTION FIND_MATCHING_XY(x NUMBER,y NUMBER, XYOrd IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,epsilon NUMBER default 1.E-6,istart PLS_INTEGER default 1) RETURN PLS_INTEGER;

FUNCTION Find_Segment(X NUMBER,Y NUMBER,PolyMBR IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,XYS IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER,distance_found IN OUT NOCOPY NUMBER,epsilon NUMBER default 0.0) RETURN PLS_INTEGER Deterministic;

PROCEDURE FIX_FIGURE8(origXYord IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,XYord IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,dec_digits PLS_INTEGER);

PROCEDURE GET_BAD_IDS(Topology VARCHAR2 default 'MT',state_code VARCHAR2,Edges_Table VARCHAR2,validate_error VARCHAR2,BAD_IDS IN OUT NOCOPY MDSYS.SDO_LIST_TYPE);
FUNCTION GET_LOOP_XYS(Edge_xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Edges_Table VARCHAR2,ID_LIST IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN MDSYS.SDO_ORDINATE_ARRAY;

FUNCTION GET_NEARBY_EDGES(Edges_Table VARCHAR2,Edge_id NUMBER,pxLL NUMBER,pyLL NUMBER,pxUR NUMBER,pyUR NUMBER,SRID NUMBER,pxepsilon NUMBER default NULL,pyepsilon NUMBER default NULL,ptolerance NUMBER default 0.05) RETURN MDSYS.SDO_LIST_TYPE;
PROCEDURE GET_SCALE(threshold IN OUT NOCOPY NUMBER,scale IN OUT NOCOPY NUMBER,target_scale NUMBER,Length NUMBER);
FUNCTION GET_SPLIT_VERTICES(edge_id NUMBER,start_node NUMBER,end_node NUMBER,
                            new_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                            nearby_edges MDSYS.SDO_LIST_TYPE,
                            Edges_Table VARCHAR2 default NULL,
                            part_geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY,whole BOOLEAN default FALSE) RETURN MDSYS.SDO_LIST_TYPE;
Function get_x_or_y(ptile NUMBER,pLLxy NUMBER,pURxy NUMBER,get_x BOOLEAN,delta IN OUT NOCOPY NUMBER) return number;
FUNCTION index_exists(pInTable VARCHAR2, pInColumn VARCHAR2, pInSchema VARCHAR2 DEFAULT NULL) RETURN BOOLEAN;
FUNCTION IS_CONNECTED(edge_id NUMBER,start_node NUMBER,end_node NUMBER,
                       nearby_edges MDSYS.SDO_LIST_TYPE,
                       Edges_Table VARCHAR2, ID_LIST IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN VARCHAR2;
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
                              det IN OUT NOCOPY NUMBER,debugit boolean default false)
            RETURN NUMBER Deterministic;
FUNCTION LOAD_TOPO_MAP_BY_QUAD(cache_name VARCHAR2,vstate VARCHAR2,Topology VARCHAR2,
pxLL IN OUT NOCOPY NUMBER,pyLL IN OUT NOCOPY NUMBER,pxUR IN OUT NOCOPY NUMBER,pyUR IN OUT NOCOPY NUMBER,tile IN OUT NOCOPY PLS_INTEGER,delta NUMBER,
                            pskip_Table     VARCHAR2 default NULL,
                            pDone_Table     VARCHAR2 default 'EDGES_DONE',
                            pEdges_Table    VARCHAR2 default 'EDGES_TOPROCESS') RETURN PLS_INTEGER;

-- Makes a useful Edge table with the edge_id,start_node,end_node,geometry,vertices and nearby edges
-- for each edge.
PROCEDURE RUN_MAKE_EDGE_TABLE(Topology VARCHAR2 default 'MT',run_flag VARCHAR2 default NULL,pedge_table VARCHAR2 default 'EDGE',pface VARCHAR2 default 'FACE',pedges_toprocesstable VARCHAR2 default 'EDGES_TOPROCESS',pState_Edge_Table VARCHAR2 default 'STATE_EDGE_TABLE',pEntityfp VARCHAR2 default 'STATEFP');
FUNCTION  MAKE_EDGE_TABLE(Topology VARCHAR2 default 'MT',mt_Edge VARCHAR2 default 'EDGE$',pedge_table VARCHAR2 default 'EDGE',Edges_toprocesstable VARCHAR2,Ignore_Table VARCHAR2 default NULL,State_Edge_table VARCHAR2) RETURN NUMBER;
FUNCTION Perpendicular(xin IN OUT NOCOPY NUMBER,yin IN OUT NOCOPY NUMBER,  -- the point
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,  -- the line
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                       xnear IN OUT NOCOPY NUMBER,ynear IN OUT NOCOPY NUMBER,  -- the perpendicular
                       always BOOLEAN default FALSE,  -- always return a distance
                       meters BOOLEAN default FALSE)  -- when true return meters
                       RETURN NUMBER deterministic;
FUNCTION PROCESS_SELF_SEGS(self_segs IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,origXYORD IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,XYORD IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION quad_tree(xLL IN OUT NOCOPY NUMBER,yLL IN OUT NOCOPY NUMBER,
                    xUR IN OUT NOCOPY NUMBER,yUR IN OUT NOCOPY NUMBER,ptile PLS_INTEGER,subdivide BOOLEAN) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION REMOVE_CLOSE_XYS(Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,ptolerance NUMBER default 0.05) RETURN BOOLEAN Deterministic;
FUNCTION ROBUST_LINE_GEN( geom MDSYS.SDO_GEOMETRY,cut NUMBER default 5) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION remove_obtuse_angles(check_angle number, In_Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY) return MDSYS.SDO_ORDINATE_ARRAY;

PROCEDURE     SET_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
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
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0,
                      pbeg PLS_INTEGER default 0) ;
PROCEDURE SHELLSORT( InArray IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,InLB PLS_INTEGER default 1,InUB         PLS_INTEGER default 0,pInDirection    VARCHAR2 default 'ASC');
FUNCTION SIMPLE_INTERSECT(   X1 IN OUT NOCOPY NUMBER,
                              Y1 IN OUT NOCOPY NUMBER,
                              X2 IN OUT NOCOPY NUMBER,
                              Y2 IN OUT NOCOPY NUMBER,
                              X3 IN OUT NOCOPY NUMBER,
                              Y3 IN OUT NOCOPY NUMBER,
                              X4 IN OUT NOCOPY NUMBER,
                              Y4 IN OUT NOCOPY NUMBER )  RETURN BOOLEAN;
FUNCTION SIMPLIFY_REGION (topology     VARCHAR2,
                                        topomap_name    VARCHAR2,
                                        pscale          NUMBER,
                                        nice            NUMBER,
                                        state_code      VARCHAR2,
                                        min_x           NUMBER default NULL,
                                        min_y           NUMBER default NULL,
                                        max_x           NUMBER default NULL,
                                        max_y           NUMBER default NULL,
                                        tolerance       NUMBER default 0.05,
                                        pBad_Table      VARCHAR2 default 'BAD_EDGES',
                                        pskip_Table     VARCHAR2 default NULL,
                                        pDone_Table     VARCHAR2 default 'EDGES_DONE',
                                        pEdges_Table    VARCHAR2 default 'EDGES_TOPROCESS',
                                        method          VARCHAR2 default 'ZONE',
                                        dec_digits      NUMBER default 6.
                                        ) RETURN NUMBER;
PROCEDURE STABLESORT2( Arr         IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Order_array   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       InLB          PLS_INTEGER default 1, 
                       InUB          PLS_INTEGER default 0,
                       forwards      BOOLEAN default TRUE);
FUNCTION SWAP_EDGE_COORDS(Topology VARCHAR2,Edge_id NUMBER, Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY) RETURN VARCHAR2;


FUNCTION TABLE_EXISTS (pInTable  IN VARCHAR2,pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN;
procedure test_find_distances;
--procedure test_check_PolyLR;
procedure test_quad_tree;
procedure test_find_intersection_segment;
procedure test_check_PolyLR;
procedure test_set_mbr;
END GZ_SUPER;
/
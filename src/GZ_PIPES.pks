create or replace
PACKAGE GZ_PIPES AUTHID CURRENT_USER AS
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
-- Uses this TYPE
--
--create or replace
--TYPE MBR_LIST_OBJ AS OBJECT 
--(
--  xmin NUMBER, ymin NUMBER, xmax NUMBER, ymax NUMBER, MBRs MDSYS.SDO_LIST_TYPE,
--  CONSTRUCTOR FUNCTION mbr_list_Obj(MBRz IN MDSYS.SDO_LIST_TYPE) RETURN SELF AS RESULT);
--
    TYPE   CHAR_ARRAY IS VARRAY(1048576) OF VARCHAR2(1);
    TYPE   VINTEGER_ARRAY IS VARRAY(1048576) OF PLS_INTEGER;
    TYPE   MBR_LIST_TYPE IS TABLE of PLS_INTEGER INDEX by PLS_INTEGER;
    
    TYPE   MBR_TYPE IS RECORD( xLL NUMBER, yLL NUMBER, xUR NUMBER, yUR NUMBER,
                                 lo NUMBER, hi NUMBER,start_overlap NUMBER, endpoints NUMBER,mbr_list MBR_LIST_TYPE);

    TYPE   MBRS_TYPE IS TABLE OF MBR_TYPE  INDEX by PLS_INTEGER;
    
    TYPE   PIPE_TYPE IS RECORD( Pipe_no NUMBER, Type VARCHAR2(1), Length NUMBER, ACCUM_LEN NUMBER, SEG NUMBER, MSEG NUMBER, 
                                  PROJECTOR NUMBER, Seg_Length NUMBER, OVERLAP NUMBER, Mseg_Length NUMBER, MOVERLAP NUMBER, WIDTH_P NUMBER,
                                 WIDTH_Q NUMBER, XP NUMBER, YP NUMBER, XQ NUMBER, YQ NUMBER, Link_bck NUMBER, Link_fwd NUMBER);
    TYPE   PIPES_TYPE IS TABLE OF PIPE_TYPE  INDEX by PLS_INTEGER;

-- We can use this type in 2 different ways:
--      1) to build a ring description:
--        a) Either a pair, start, end vertex
--     or b) a next coordinate
--
--   OR 2) to describe a linked list:
--         start vertex is the current vertex
--         next vertex is what it is linked to (usually start verex + 1 except
--         at the end of the ring)
--     or  x,y is what it is linked to. in which case next vertex is the
--         segment that x,y falls on and must be used for the next link.
--         
    TYPE   XY_VERTEX IS RECORD( Start_Vertex NUMBER, Next_vertex NUMBER,  X NUMBER, Y NUMBER);
    TYPE   XY_LIST IS TABLE OF XY_VERTEX  INDEX by PLS_INTEGER;

--    g_Widths    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    g_Matches   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    g_Where_is  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
    g_Distances MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();


Function angle_in_degrees(x1 number,y1 number,x2 number,y2 number,
                 x3 number,y3 number,x4 number,y4 number) return number;
FUNCTION AZIMUTH(Geom IN MDSYS.SDO_GEOMETRY,psegment PLS_INTEGER default 1,pinitial VARCHAR2 default 'TRUE') RETURN NUMBER;                 
FUNCTION C_TAN(InX NUMBER)  RETURN NUMBER Deterministic;

PROCEDURE find_adjacency(pInTable VARCHAR2,pInUniqIdColumn VARCHAR2,pInSdoGeomColumn VARCHAR2,pOutTableName VARCHAR2,pdistance NUMBER default 100.,pRowLimit NUMBER DEFAULT 50);

FUNCTION CHECK_NEARBY(Subject_XYs         IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                        Subject_Info_Array  IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                        Subject_MBR         IN OUT NOCOPY MBRS_TYPE,
                         XYs                IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         Info_Array         IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                         MBR                IN OUT NOCOPY MBRS_TYPE,
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
FUNCTION CIRCULATE_COORDINATES(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,shift PLS_INTEGER) RETURN MDSYS.SDO_ORDINATE_ARRAY;
FUNCTION CLASSIFY_Vertices(Pipe_pars IN OUT NOCOPY Pipes_Type,Ring_Jump IN OUT NOCOPY XY_List,pGeom IN MDSYS.SDO_GEOMETRY,  max_width NUMBER default 150., minimum_len NUMBER default 0.) RETURN CHAR_ARRAY;
Function close_measure(seg1 pls_integer, seg2 pls_integer,Info_array IN OUT NOCOPY SDO_ELEM_INFO_ARRAY,XYS_count PLS_INTEGER)  return pls_integer;
-- Best Distance function for 8265
Function Distance_Fcn(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER;
Function Old_Distance_Fcn(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER;
FUNCTION divergence(x11 NUMBER,y11 NUMBER,x22 NUMBER,y22 NUMBER, x33 NUMBER, y33 NUMBER, x44 NUMBER, y44 NUMBER,cpx NUMBER, cpy NUMBER,cutoff NUMBER,
                     xp in out nocopy number, yp in out nocopy number, xq in out nocopy number,yq in out nocopy number,perp_angle_tolerance NUMBER default 0.707107) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION PERIMETER_AREA_RATIO(Geom IN MDSYS.SDO_GEOMETRY) RETURN NUMBER DETERMINISTIC;
-- Function to find a coordinate (either the x or the y or both) on a line either
-- exactly or within a small epsilon.
FUNCTION FIND_NEARBY(   try PLS_INTEGER, try_end PLS_INTEGER, pstart PLS_INTEGER, Inpend PLS_INTEGER,
                         xbuffer IN OUT NOCOPY NUMBER, ybuffer IN OUT NOCOPY NUMBER,
                         Subject_XYs        IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         Subject_Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
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
                         pdist_tolerance    NUMBER,
                         pfind              VARCHAR2 default 'B',
                         the_same           BOOLEAN default FALSE)
RETURN PLS_INTEGER;
FUNCTION FIND_ALL_OPENINGS(Geom IN MDSYS.SDO_GEOMETRY,pipe_type VARCHAR2 default 'A',max_width NUMBER default 150., minimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) return MDSYS.SDO_GEOMETRY;

FUNCTION FIND_OPENINGS(Geom MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., pminimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_LIST_TYPE;
PROCEDURE FIND_PIPES(Intable VARCHAR2, IdColumn VARCHAR2, GeomColumn VARCHAR2,OutTable VARCHAR2,pipe_type VARCHAR2 default 'A',max_width NUMBER default 150., minimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.);
FUNCTION FIND_PIPES(Geom IN MDSYS.SDO_GEOMETRY,pipe_type VARCHAR2 default 'A',max_width NUMBER default 150., minimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN NUMBER;
FUNCTION GET_MBR_GEOM(Geom IN MDSYS.SDO_GEOMETRY,max_width NUMBER) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION GET_PIPE_WIDTH(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 125.,choose VARCHAR2 default 'HIGH',pgap PLS_INTEGER default 1) RETURN NUMBER;
FUNCTION GET_NARROW_SEGS(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 500.,choose VARCHAR2 default 'SMALL',pgap PLS_INTEGER default 2) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION GET_PIPE_ENDS(XYs MDSYS.SDO_ORDINATE_ARRAY, Pipe_pars Pipes_Type) RETURN MDSYS.SDO_ORDINATE_ARRAY;
FUNCTION GET_SansPIPE_Segs(pGeom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., minimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION GET_PIPELESS(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., pminimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 65.) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION GET_RINGS(Geometry MDSYS.SDO_GEOMETRY,ring_order MDSYS.SDO_LIST_TYPE,ring_shifts MDSYS.SDO_LIST_TYPE default NULL) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION GET_WIDE_SEGS(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 500.,pgap PLS_INTEGER default 2,shortest VARCHAR2 default 'FALSE') RETURN MDSYS.SDO_GEOMETRY;
FUNCTION GET_WIDTHS(Geom IN MDSYS.SDO_GEOMETRY,cutoff_width NUMBER default 500.,return_where VARCHAR2 default 'TRUE',pgap PLS_INTEGER default 1,shortest VARCHAR2 default 'TRUE') RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION GET_WIDTHS_BY_IDS(pTopology VARCHAR2, Edge_Id1 NUMBER, Edge_Id2 NUMBER, cutoff_width NUMBER default 100.) RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION GET_Pipe_WIDTHS(Geom IN MDSYS.SDO_GEOMETRY,Where_is IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Matches IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Distances IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                          cutoff_width NUMBER default 500.,pgap PLS_INTEGER default 2,always BOOLEAN default FALSE) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION GET_XYS( Geom IN MDSYS.SDO_GEOMETRY, 
                   pring1 PLS_INTEGER,pseg1_to_find PLS_INTEGER default 0,
                   pring2 PLS_INTEGER DEFAULT NULL, pseg2_to_find PLS_INTEGER default NULL,
   convert VARCHAR2 default '8265' ) RETURN sdo_geometry;
PROCEDURE Make_Pipe_Pairs_table(In_Table VARCHAR2,face_id NUMBER,Out_Table VARCHAR2,pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., minimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) ;

FUNCTION Measure_Gap(Geom IN MDSYS.SDO_GEOMETRY,pwidth NUMBER default 100.) RETURN NUMBER;
FUNCTION METERS_TO_DEGREES(X_OR_Y VARCHAR2 default 'X',pLength NUMBER, pLatitude NUMBER, tolerance NUMBER default 0.01) RETURN NUMBER;

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

-- Very faster arctangent function
FUNCTION FASTER_ATAN2(YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER;

FUNCTION Find_Distances(x IN OUT NOCOPY NUMBER,y IN OUT NOCOPY NUMBER,XYS IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,GC_distances  IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,pstart PLS_INTEGER default 1,pend PLS_INTEGER default 0,pnvert_outer PLS_INTEGER default 0,forwards VARCHAR2 default 'ASC') RETURN MDSYS.SDO_LIST_TYPE;

FUNCTION FIND_MPW_AT_SCALE(Geom IN MDSYS.SDO_GEOMETRY,pipe_type VARCHAR2 default 'A',scale_rf NUMBER default 500000.,paspect_ratio NUMBER default 1.) return NUMBER;
-- Function to return the length of the pipe (NULL when nothing found);
FUNCTION FIND_MIN_PIPE_WIDTH(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default'A', max_width NUMBER default 150.,  minimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN NUMBER;
-- Function that does the work for Find and Get
FUNCTION FIND_GET_PIPES(Geom IN MDSYS.SDO_GEOMETRY,pipe_type VARCHAR2 default 'A', max_width NUMBER default 150.,minimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN PIPES_TYPE;

FUNCTION GET_PIPE_Segs(pGeom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., minimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY;
-- Function to return the pipe shape as a geometry (NULL when nothing found);
FUNCTION GET_PIPES(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', pmax_width NUMBER default 0.0, pminimum_len NUMBER default 0.,aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY;


FUNCTION GET_Small_Angle(Geom IN MDSYS.SDO_GEOMETRY, max_angle_limit NUMBER default 0.) RETURN NUMBER;
FUNCTION Get_Small_Angle_Geom(Geom IN MDSYS.SDO_GEOMETRY,max_angle_limit NUMBER default 0.) RETURN MDSYS.SDO_GEOMETRY;

FUNCTION GET_Angles(Geom IN MDSYS.SDO_GEOMETRY,smallest NUMBER default 0.0, options VARCHAR2 default 'SMALL',bearings VARCHAR2 default 'FALSE') RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION GET_AZIMUTHS(Geom IN MDSYS.SDO_GEOMETRY,options VARCHAR2 default 'VI') RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION Get_Length(Geom IN MDSYS.SDO_GEOMETRY, seg_no PLS_INTEGER) RETURN NUMBER;
Function Last_Info(coord pls_integer,istart IN OUT NOCOPY PLS_INTEGER, Info_array IN OUT NOCOPY SDO_ELEM_INFO_ARRAY,XYS_count PLS_INTEGER) return pls_integer;
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
                              perp_angle_tolerance NUMBER default 0.707107,print_it BOOLEAN default FALSE)
            RETURN NUMBER Deterministic;
Function Get_Pipe_Geom(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Info MDSYS.SDO_ELEM_INFO_ARRAY,start_vertex PLS_INTEGER,across_vertex PLS_INTEGER,
                        
                        which PLS_INTEGER,cutoff NUMBER,PSL IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,average_width IN OUT NOCOPY NUMBER,accum_length IN OUT NOCOPY NUMBER) RETURN MDSYS.SDO_GEOMETRY;
Function LPADV(input NUMBER,width NUMBER,format VARCHAR2 default NULL) RETURN VARCHAR2;
-- Very accurate arctangent function
FUNCTION NEW_ARCTAN (YIn NUMBER, Xin NUMBER,degrees BOOLEAN default FALSE) RETURN NUMBER;

FUNCTION Perimeter(Geom MDSYS.SDo_GEOMETRY,pvtx1 PLS_INTEGER default 1,pvtx2 PLS_INTEGER default 0,method VARCHAR2 default 'FAST') RETURN NUMBER DETERMINISTIC;
FUNCTION Perimeter(XYs IN MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER default 8265.,pvtx1 PLS_INTEGER default 1,pvtx2 PLS_INTEGER default 0,istart PLS_INTEGER default 1, last_vertex PLS_INTEGER default 0,method VARCHAR2 default 'FAST') RETURN NUMBER DETERMINISTIC;

FUNCTION POINT_IN_POLY( Xc NUMBER, Yc NUMBER,XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                         Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY)
                          RETURN PLS_INTEGER Deterministic;
FUNCTION POLYGON_AREA(Geom MDSYS.SDO_GEOMETRY ) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION POLYGON_AREA( XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, Info MDSYS.SDO_ELEM_INFO_ARRAY, Areas IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,SRID NUMBER default 8265., Positive BOOLEAN default TRUE) RETURN NUMBER;
Function Remove_rings(Geom MDSYS.SDO_GEOMETRY,Area_threshold NUMBER, Less_than VARCHAR2 default 'TRUE') RETURN  MDSYS.SDO_GEOMETRY;
FUNCTION RING_AREA(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER default 8265.,pstart PLS_INTEGER default 1, pend PLS_INTEGER default 0)
           RETURN NUMBER DETERMINISTIC;
Function Ring_Builder(Ring_jump Xy_List,Geom MDSYS.SDO_GEOMETRY,Area_cutoff NUMBER default 0.0) RETURN MDSYS.SDO_GEOMETRY;
Function RPADV(input NUMBER,width NUMBER,format VARCHAR2 default NULL) RETURN VARCHAR2;
FUNCTION QUICK_RING_AREA(Xys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,SRID NUMBER DEFAULT 8265.,pstart PLS_INTEGER default 1, pend PLS_INTEGER default 0)
           RETURN NUMBER DETERMINISTIC;
FUNCTION TRY_RING_AREA(geom mdsys.sdo_geometry) return number;
Function QUICK_SINCOS(Xin NUMBER,cosX IN OUT NOCOPY NUMBER,in_degrees VARCHAR2 default 'N') RETURN NUMBER Deterministic;
FUNCTION QUIZ_MBR(seg PLS_INTEGER, MBRs IN OUT NOCOPY MBRS_TYPE) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION REMOVE_PIPES(Geom IN MDSYS.SDO_GEOMETRY, pipe_type VARCHAR2 default 'A', max_width NUMBER default 150., pminimum_len NUMBER default 0.,min_aspect_ratio NUMBER default 0.,max_angle_diff NUMBER default 35.) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION SET_GEOM_MANY_MBR(Geom IN MDSYS.SDO_GEOMETRY) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION SET_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0,
                      ploops  PLS_INTEGER default 2,
                      xbuffer NUMBER default 0.0, ybuffer NUMBER default 0.0) Return PLS_INTEGER;

PROCEDURE SET_MANY_MBRS(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      Info_Array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
                      MBRs IN OUT NOCOPY MBRS_TYPE,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      skip PLS_INTEGER DEFAULT 0,
                      tolerance NUMBER default 0.0,
                      pLB  PLS_INTEGER default 1,
                      pUB  PLS_INTEGER default 0,
                      pbeg PLS_INTEGER default 0);
               
Procedure setup_side(side in out nocopy pls_integer,inc1 pls_integer, next_one in out nocopy pls_integer,start_of_ring pls_integer, end_of_ring pls_integer);
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
                       PROCEDURE SHELLSORT4( Arr         IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Arr2   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Arr3   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Arr4   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       InLB          PLS_INTEGER default 1,
                       InUB          PLS_INTEGER default 0
                      );
PROCEDURE UNPACK_GEOMETRY(pInTable VARCHAR2,pInIDColumn VARCHAR2,pInGeomColumn VARCHAR2,pOutTable VARCHAR2,Renumber BOOLEAN default FALSE);

FUNCTION WEED(Pipe_type VARCHAR2,XYS IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Info_array IN OUT NOCOPY MDSYS.SDO_ELEM_INFO_ARRAY,
Codes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Subject_Segs IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Matches      IN OUT NOCOPY MDSYS.SDO_LIST_TYPE, 
  Widths_p     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Widths_q     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Lengths      IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Xps          IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Yps          IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Xqs          IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
  Yqs          IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,area_tolerance NUMBER default 0.) RETURN PLS_INTEGER;

--==============================================================================
-- Start of unit testing procedures
procedure test_get_widths;
procedure test_check_opening;
procedure test_lines(seg1 pls_integer default 1,pseg2 pls_integer default 0);
procedure test_ring_area;
Procedure test_lesser_measure;
procedure try_set_many_mbr(seg number default 1,pgeom  IN MDSYS.SDO_GEOMETRY default NULL);
procedure test_sort_also;
PROCEDURE TRY_QUICK_SINCOS;
END GZ_PIPES;
/
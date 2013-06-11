create or replace
PACKAGE GZ_UTIL_ZONE AUTHID CURRENT_USER AS
-- Current zone as in ACS09GM
--    deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.
--    rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
PROCEDURE FETCH_GEOMETRY(sql_stmt VARCHAR2,InOID1 PLS_INTEGER,
                        Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY);
FUNCTION CENTROID(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,Xc IN OUT NUMBER,Yc IN OUT NUMBER) RETURN NUMBER;
FUNCTION New_Fix_Geometry(Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,geom_error IN OUT NOCOPY VARCHAR2,XYs IN MDSYS.SDO_Ordinate_Array,closed BOOLEAN default TRUE) RETURN NUMBER;
FUNCTION FIX_Geometry(PolyXys IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,geom_error VARCHAR2) RETURN NUMBER;
FUNCTION Find_nearest(x NUMBER,y NUMBER,
                      Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,pstart PLS_INTEGER,pend PLS_INTEGER)
                      RETURN PLS_INTEGER;
PROCEDURE FIND_VIPS(allowed_deviation NUMBER,
                      max_allowed_deviation NUMBER,
                      max_allowed_angle_change NUMBER,
                      Min_In_Zone PLS_INTEGER,
                      measure NUMBER,Minimum_len NUMBER,
                      Bearings IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Vertex     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      B_to_V IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Xes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Yes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Inn_start PLS_INTEGER, Inn_End PLS_INTEGER,
                                  Nzone IN OUT NOCOPY PLS_INTEGER,
                                  Nzone_min PLS_INTEGER,
                                  Start_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  In_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Order_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Mean_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Zone_constant IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Deviation IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Max_Deviation IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Area_under_Curve IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Difference NUMBER default 0.
                                  );
PROCEDURE shellsort2(Arr IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Order_array IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,InLB PLS_INTEGER default 1,InUB PLS_INTEGER default 0);
PROCEDURE Remove_Too_Straight(Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Straight_len NUMBER,
                      Straight_angle NUMBER default 175.,
                      check_for_anchors BOOLEAN default TRUE);
FUNCTION FIND_MAX_DEVIATION(Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                            Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                            pos PLS_INTEGER,pos2 PLS_INTEGER,
                            area IN OUT NOCOPY NUMBER,
                            indexp IN OUT NOCOPY NUMBER) RETURN NUMBER;
PROCEDURE REMOVE_AREA(allowed_deviation NUMBER,
                      max_allowed_deviation NUMBER,
                      max_allowed_angle_change NUMBER,
                      min_allowed_area NUMBER,
                      max_allowed_area NUMBER,
                      max_lost_area_fraction NUMBER,
                      measure NUMBER,Minimum_len NUMBER,
                      Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Xes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Yes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Nzone IN OUT NOCOPY PLS_INTEGER,
                                  Start_Elements IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Deviation IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Max_Deviation IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Area_under_Curve IN OUT NOCOPY MDSYS.SDO_LIST_TYPE);
PROCEDURE GET_DEVIATIONS(max_allowed_deviation NUMBER,
                                  Vertex     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  B_TO_V     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Xes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Yes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Inn_start PLS_INTEGER, Inn_End PLS_INTEGER,
                                  Nzone PLS_INTEGER,
                                  loop_begin PLS_INTEGER, loop_end PLS_INTEGER,
                                  Start_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  In_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Deviation IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Max_Deviation IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Area_under_Curve IN OUT NOCOPY MDSYS.SDO_LIST_TYPE);

PROCEDURE REMOVE_VS(check_area NUMBER,check_length NUMBER,
                        Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                        Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                        Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE);
PROCEDURE REVERSE_ORDINATES(XY IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                             inLB  PLS_INTEGER default 1,
                             InUB  PLS_INTEGER default 0);
PROCEDURE ROUND_BENDS( medium_length NUMBER,
                        Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                        Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                        Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE);
PROCEDURE RUN_SIMPLIFY(In_Table VARCHAR2,scale number,nice number,out_table VARCHAR2);
PROCEDURE SET_MBR(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,XLL IN OUT NOCOPY NUMBER,yLL IN OUT NOCOPY NUMBER,xUR IN OUT NOCOPY NUMBER,yUR IN OUT NOCOPY NUMBER,pLB  PLS_INTEGER default 1,pUB  PLS_INTEGER default 0);
FUNCTION SMOOTH_LINE(psmooth_length NUMBER,minimum_length NUMBER,Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN  MDSYS.SDO_ORDINATE_ARRAY;
PROCEDURE
          STABLESORT2( Arr         IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       Order_array   IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                       InLB          PLS_INTEGER default 1,
                       InUB          PLS_INTEGER default 0,
                       forwards     BOOLEAN default TRUE);
PROCEDURE  XYBRESENHAM(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                      SQRS IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      SEGS IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      inext IN OUT NOCOPY PLS_INTEGER,
                      iside IN OUT NOCOPY NUMBER,
                      XLL IN OUT NOCOPY NUMBER,
                      yLL IN OUT NOCOPY NUMBER,
                      xUR IN OUT NOCOPY NUMBER,
                      yUR IN OUT NOCOPY NUMBER,
                      width IN OUT NOCOPY NUMBER);
FUNCTION ACCURATE_GCD( x1 IN OUT NOCOPY NUMBER, y1 IN OUT NOCOPY NUMBER,
                       x2 IN OUT NOCOPY NUMBER, y2 IN OUT NOCOPY NUMBER,
                       SRID NUMBER DEFAULT 8265.)
                       RETURN NUMBER DETERMINISTIC;
FUNCTION ANGLE(x1 NUMBER,y1 NUMBER,x0 NUMBER, y0 NUMBER, x2 NUMBER, y2 NUMBER,bearing1 IN OUT NOCOPY NUMBER, bearing2 IN OUT NOCOPY NUMBER,SRID NUMBER default 8265.) 
RETURN NUMBER  Deterministic;
FUNCTION ACCURATE_LENGTH(XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,in_pos pls_integer default 1,In_N pls_integer default 0)
                       RETURN NUMBER DETERMINISTIC;
FUNCTION AVERAGE(Array IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,In_start PLS_INTEGER, In_end PLS_INTEGER) RETURN NUMBER;
FUNCTION AREA_ALONG_LINE(
                                In_start PLS_INTEGER, In_End PLS_INTEGER,
                                Xes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                Yes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                Area_above IN OUT NOCOPY NUMBER,
                                Area_below IN OUT NOCOPY NUMBER,
                                In_Degrees Boolean default TRUE,
                                pInUseProjection Boolean default FALSE,
                                check_area Boolean default FALSE
                                ) RETURN NUMBER;
FUNCTION ATAN2(YIn NUMBER, XIn NUMBER) RETURN NUMBER Deterministic;
FUNCTION BEARINGS_ALONG_ALINE(measure NUMBER,
                                In_start PLS_INTEGER, IN_End PLS_INTEGER,
                                XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                                Mapto_vertex     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                B_to_V IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                pred_bcount IN OUT NOCOPY NUMBER
                                ) RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION
          BINARY_SEARCH(
                           PInFind NUMBER,
                           PInArray IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                           InN PLS_INTEGER, Frequency IN OUT NOCOPY PLS_INTEGER)
 RETURN PLS_INTEGER  Deterministic;
FUNCTION BUILD_APOLYGON(
                geometry1 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                geometry2 IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                tolerance NUMBER,
                cos_angle IN OUT NOCOPY NUMBER,
                sin_angle IN OUT NOCOPY NUMBER,
                xmid IN OUT NOCOPY NUMBER,
                ymid IN OUT NOCOPY NUMBER)
RETURN MDSYS.SDO_GEOMETRY;
FUNCTION FAST_ATAN2(YIn NUMBER, XIn NUMBER) RETURN NUMBER Deterministic;
FUNCTION FIND_SEGMENT (
      px1             IN NUMBER,
      py1             IN NUMBER,
      px2             IN NUMBER,
      py2             IN NUMBER,
      xes             IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
      yes             IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
      layer_tolerance   IN NUMBER DEFAULT 0.2,
      left            IN OUT NOCOPY NUMBER,
      right           IN OUT NOCOPY NUMBER,
      allow_points    BOOLEAN default FALSE
   ) RETURN NUMBER;
FUNCTION Is_Geodetic(SRID NUMBER) return BOOLEAN;
FUNCTION LINE_INTERSECT( X1 IN OUT NOCOPY NUMBER,
                              Y1 IN OUT NOCOPY NUMBER,
                              X2 IN OUT NOCOPY NUMBER,
                              Y2 IN OUT NOCOPY NUMBER,
                              X3 IN OUT NOCOPY NUMBER,
                              Y3 IN OUT NOCOPY NUMBER,
                              X4 IN OUT NOCOPY NUMBER,
                              Y4 IN OUT NOCOPY NUMBER,
                              Xi IN OUT NOCOPY NUMBER,
                              Yi IN OUT NOCOPY NUMBER)
            RETURN NUMBER Deterministic;
FUNCTION  LINE_SIMPLIFY(In_Geometry  MDSYS.SDO_GEOMETRY,
                        InScale NUMBER default 500000.,
                        In_nice_factor NUMBER default 0.0,Inminimum_len NUMBER default 0.,
                        Inlen NUMBER default 0.,ID NUMBER default 0.,InRiver_param NUMBER default -1.,InTopology VARCHAR2 default NULL,Intarget_scale NUMBER default 0.0) RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;

FUNCTION MEASURE_BENDINESS(Bearings IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,B_to_V IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
XYs IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY, rough_length IN OUT NOCOPY NUMBER,cycles IN OUT NOCOPY PLS_INTEGER,
bear_count IN OUT NOCOPY PLS_INTEGER,straight_bends IN OUT NOCOPY NUMBER,right_bends IN OUT NOCOPY NUMBER,not_right_bends IN OUT NOCOPY NUMBER) RETURN NUMBER;
FUNCTION NEW_INTERSECT_2EDGES(XYArray  IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                            XYArray2 IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY,
                            cos_angle IN OUT NOCOPY NUMBER,
                            sin_angle IN OUT NOCOPY NUMBER,
                            xmid IN OUT NOCOPY NUMBER,
                            ymid IN OUT NOCOPY NUMBER)
RETURN MDSYS.SDO_ORDINATE_ARRAY;
FUNCTION ORIENT2D (paX NUMBER, paY NUMBER,
                                  pbX NUMBER, pbY NUMBER,
                                  pcX NUMBER, pcY NUMBER)
            RETURN NUMBER Deterministic;
FUNCTION POINT_ON_LINE (
      pcX   IN NUMBER,
      pcY   IN NUMBER,
      pX1   IN NUMBER,
      pY1   IN NUMBER,
      pX2   IN NUMBER,
      pY2   IN NUMBER,
      prout IN OUT NOCOPY NUMBER,
      psout IN OUT NOCOPY NUMBER,
      layer_tolerance   IN NUMBER DEFAULT 0.2
   ) RETURN NUMBER DETERMINISTIC;
Function Rad2deg RETURN NUMBER deterministic;
FUNCTION SEARCH_HILO(To_find NUMBER,Array IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,In_end PLS_INTEGER,hi PLS_INTEGER default 1) RETURN PLS_INTEGER;
FUNCTION SEARCH_GREATER(
                           PInFind NUMBER,
                           PInArray IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                           InN PLS_INTEGER)
 RETURN PLS_INTEGER  Deterministic;
FUNCTION  SET_START_ELEMENTS(loop_begin PLS_INTEGER, loop_end PLS_INTEGER,
                           Nzone PLS_INTEGER, In_kount PLS_INTEGER,
                           B_TO_V     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                           Start_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE
                           )
                           RETURN MDSYS.SDO_LIST_TYPE;
FUNCTION SINCOS(InX NUMBER,
                                    COSX IN OUT NOCOPY NUMBER)
    RETURN NUMBER Deterministic;

FUNCTION FAST_SINCOS(InX NUMBER,cosx IN OUT NOCOPY NUMBER) RETURN NUMBER Deterministic;
FUNCTION GEO_BEARING(px0 NUMBER,py0 NUMBER,px1 NUMBER,py1 NUMBER) RETURN NUMBER DETERMINISTIC;
FUNCTION C_TAN(InX NUMBER)  RETURN NUMBER Deterministic;
FUNCTION TRIANGLE_Area(x0 IN OUT NOCOPY NUMBER,y0 IN OUT NOCOPY NUMBER,
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                        SRID NUMBER default 8265.)
                       RETURN NUMBER DETERMINISTIC;
FUNCTION UPDATE_Areas(control PLS_INTEGER,current IN OUT NOCOPY PLS_INTEGER,search PLS_INTEGER, first PLS_INTEGER, last PLS_INTEGER,
                      Keep_it    IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Xes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      Yes        IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                      length1 IN OUT NOCOPY NUMBER, length2 IN OUT NOCOPY NUMBER) RETURN NUMBER;

FUNCTION REMOVE_CLOSE_XYS(Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,scale NUMBER,area NUMBER default 0.0,ptolerance NUMBER default 0.05)
 RETURN NUMBER Deterministic;
 FUNCTION OLD_REMOVE_CLOSE_XYS(Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,ptolerance NUMBER default 0.05)
 RETURN NUMBER Deterministic;
FUNCTION fast_vincenty_gcd(x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,units VARCHAR2 default 'm') RETURN NUMBER Deterministic;
FUNCTION TRY_SMOOTHING(scale NUMBER,minimum_len NUMBER,rough_length NUMBER,cycles PLS_INTEGER,perimeter NUMBER,
Gtype NUMBER, SRID NUMBER,
Xes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Yes IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,Keep_it IN OUT NOCOPY MDSYS.SDO_LIST_TYPE) RETURN MDSYS.SDO_GEOMETRY;
FUNCTION ZONE(INX IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  In_start PLS_INTEGER, In_End PLS_INTEGER,
                                  Nzone PLS_INTEGER, Nzone_min PLS_INTEGER,
                                  Vertex     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  B_to_V IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Xes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Yes     IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Start_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  In_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Order_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Mean_zone IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Zone_constant IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                                  Minimum_len NUMBER,
                                  Min_Inzone PLS_INTEGER default 3, Difference NUMBER default 0.
                                  )
RETURN PLS_INTEGER;
FUNCTION Coarse_Sample(PolyGeom IN MDSYS.SDO_GEOMETRY,sample_dist NUMBER) RETURN MDSYS.SDO_GEOMETRY;
procedure test_accurate_length;
procedure test_centroid;
function make_a_geom(closed varchar2 default 'N',reverse_it number default 0) return mdsys.sdo_geometry;
procedure test_triangle_area;
procedure test_angle(in_angle number default 45.);
function test_bearings(geom mdsys.sdo_geometry) return number;
function test_remove_close_xys(Ingeom mdsys.sdo_geometry default NULL,ptolerance number default 0.05,closed varchar2 default 'N',reverse_it number default 0)  return mdsys.sdo_geometry;
function test_reverse_ordinates(ifrom pls_integer default 1,ito number default null,InXYord Mdsys.sdo_ordinate_array default NULL) RETURN Mdsys.sdo_ordinate_array;
END GZ_UTIL_ZONE;
/
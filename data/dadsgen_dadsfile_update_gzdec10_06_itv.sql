/* Formatted on 1/8/2013 12:33:52 PM (QP5 v5.163.1008.3004) */
CREATE OR REPLACE FORCE VIEW DADSFILE
(
   MODTAG,
   UNIT,
   DISKFILE,
   RERUN,
   DEL_FLAG,
   DATE_TIME,
   RESOLUTION,
   WAVE
)
AS
   SELECT                                                    /*+ FIRST_ROWS */
         MODTAG,
          UNIT,
          DISKFILE,
          RERUN,
          DEL_FLAG,
          DATE_TIME,
          RESOLUTION,
          WAVE
     FROM DADSGEN.DADSFILE_PRE_ACS12
   UNION ALL
   SELECT                                                    /*+ FIRST_ROWS */
         MODTAG,
          UNIT,
          DISKFILE,
          RERUN,
          DEL_FLAG,
          DATE_TIME,
          RESOLUTION,
          WAVE
     FROM DADSGEN.DADSFILE_DEC10W5_TEST
   UNION ALL
   SELECT                                                    /*+ FIRST_ROWS */
         MODTAG,
          SUBSTR (UNIT, 3),
          OUTPUT_DIR || '/' || SHP_NAME,
          NULL,
          DEL_FLAG,
          SHP_CREATION_DATE,
          RESOLUTION,
          WAVE
     FROM GZACS12.GZ_SHP_METADATA
    WHERE shp_type = 'PRD' AND UNIT LIKE 'st%'
   UNION ALL
   SELECT                                                    /*+ FIRST_ROWS */
         MODTAG,
          UNIT,
          OUTPUT_DIR || '/' || SHP_NAME,
          NULL,
          DEL_FLAG,
          SHP_CREATION_DATE,
          RESOLUTION,
          WAVE
     FROM GZACS12.GZ_SHP_METADATA
    WHERE shp_type = 'PRD' AND unit LIKE 'nation'
       UNION ALL
   SELECT                                                    /*+ FIRST_ROWS */
         MODTAG,
          SUBSTR (UNIT, 3),
          OUTPUT_DIR || '/' || SHP_NAME,
          NULL,
          DEL_FLAG,
          SHP_CREATION_DATE,
          RESOLUTION,
          WAVE
     FROM GZDEC10_06.GZ_SHP_METADATA
    WHERE shp_type = 'PRD' AND UNIT LIKE 'st%'
   UNION ALL
   SELECT                                                    /*+ FIRST_ROWS */
         MODTAG,
          UNIT,
          OUTPUT_DIR || '/' || SHP_NAME,
          NULL,
          DEL_FLAG,
          SHP_CREATION_DATE,
          RESOLUTION,
          WAVE
     FROM GZDEC10_06.GZ_SHP_METADATA
    WHERE shp_type = 'PRD' AND unit LIKE 'nation';


GRANT SELECT ON DADSFILE TO PUBLIC;

exit;

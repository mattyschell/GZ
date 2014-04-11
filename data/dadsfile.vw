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
AS                          --historical production metadata first
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE ('DADSGEN.DADSFILE_PRE_ACS12'))
   UNION ALL
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE ('DADSGEN.DADSFILE_DEC10W5'))
   UNION ALL
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE ('DADSFILE_ACS12_04'))
   UNION ALL                                                    --ACS12
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE (
                    'GZACS12.GZ_SHP_METADATA',
                    'shp_type=''PRD'' AND unit LIKE ''st%''',
                    'Y',
                    'Y',
                    'Y',
                    'Y'))
   UNION ALL
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE (
                    'GZACS12.GZ_SHP_METADATA',
                    'shp_type=''PRD'' AND unit LIKE ''nation''',
                    'N',
                    'Y',
                    'Y',
                    'Y'))
   UNION ALL                                                   --wave 6
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE (
                    'GZDEC10_06.GZ_SHP_METADATA',
                    'shp_type=''PRD'' AND unit LIKE ''st%''',
                    'Y',
                    'Y',
                    'Y',
                    'Y'))
   UNION ALL
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE (
                    'GZDEC10_06.GZ_SHP_METADATA',
                    'shp_type=''PRD'' AND unit LIKE ''nation''',
                    'N',
                    'Y',
                    'Y',
                    'Y'))
   UNION ALL                                        --ACS13 
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE (
                    'GZACS13.GZ_SHP_METADATA',
                    'shp_type=''PRD'' AND unit LIKE ''st%''',
                    'Y',
                    'Y',
                    'Y',
                    'Y'))
   UNION ALL
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE (
                    'GZACS13.GZ_SHP_METADATA',
                    'shp_type=''PRD'' AND unit LIKE ''nation''',
                    'N',
                    'Y',
                    'Y',          -----------------------------------
                    'Y'))         --Add new production schemas here...                     
   UNION ALL                      --DEVBNCH-only data starts next
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE (
                    'GZCPB7.DADSFILE_ACS12_TEST'))
   UNION ALL         
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE (
                    'GZCPB7.GZ_SHP_METADATA',
                    'shp_type=''PRD'' AND unit LIKE ''st%''',
                    'Y',
                    'Y',
                    'Y',
                    'Y'))
   UNION ALL
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE (
                    'GZCPB7.GZ_SHP_METADATA',
                    'shp_type=''PRD'' AND unit LIKE ''nation''',
                    'N',
                    'Y',
                    'Y',
                    'Y'))         
   UNION ALL                      
   SELECT "MODTAG",
          "UNIT",
          "DISKFILE",
          "RERUN",
          "DEL_FLAG",
          "DATE_TIME",
          "RESOLUTION",
          "WAVE"
     FROM TABLE (GZ_DADSGEN.PIPE_DADSFILE (
                    'GZCPB7.DADSFILE_ACS13'))

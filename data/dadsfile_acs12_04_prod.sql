CREATE TABLE DADSFILE_ACS12_04
(
  MODTAG      VARCHAR2(5 BYTE),
  UNIT        VARCHAR2(6 BYTE),
  DISKFILE    VARCHAR2(1000 BYTE),
  RERUN       VARCHAR2(1 BYTE),
  DEL_FLAG    VARCHAR2(1 BYTE),
  DATE_TIME   TIMESTAMP(6),
  RESOLUTION  VARCHAR2(30 BYTE),
  WAVE        NUMBER
);

GRANT SELECT ON DADSFILE_ACS12_04 TO PUBLIC;

SET DEFINE OFF;
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '42', '/mtdata003/geo/dads/gen/acs12_04/st42/geo_2012_g_42000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '42', '/mtdata003/geo/dads/gen/acs12_04/st42/geo_2012_g_42000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '44', '/mtdata003/geo/dads/gen/acs12_04/st44/geo_2012_g_44000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '44', '/mtdata003/geo/dads/gen/acs12_04/st44/geo_2012_g_44000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '44', '/mtdata003/geo/dads/gen/acs12_04/st44/geo_2012_g_44000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '45', '/mtdata003/geo/dads/gen/acs12_04/st45/geo_2012_g_45000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '45', '/mtdata003/geo/dads/gen/acs12_04/st45/geo_2012_g_45000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '45', '/mtdata003/geo/dads/gen/acs12_04/st45/geo_2012_g_45000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '45', '/mtdata003/geo/dads/gen/acs12_04/st45/geo_2012_g_45000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '46', '/mtdata003/geo/dads/gen/acs12_04/st46/geo_2012_g_46000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '46', '/mtdata003/geo/dads/gen/acs12_04/st46/geo_2012_g_46000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '46', '/mtdata003/geo/dads/gen/acs12_04/st46/geo_2012_g_46000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '46', '/mtdata003/geo/dads/gen/acs12_04/st46/geo_2012_g_46000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '47', '/mtdata003/geo/dads/gen/acs12_04/st47/geo_2012_g_47000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '47', '/mtdata003/geo/dads/gen/acs12_04/st47/geo_2012_g_47000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '47', '/mtdata003/geo/dads/gen/acs12_04/st47/geo_2012_g_47000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '47', '/mtdata003/geo/dads/gen/acs12_04/st47/geo_2012_g_47000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '48', '/mtdata003/geo/dads/gen/acs12_04/st48/geo_2012_g_48000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '48', '/mtdata003/geo/dads/gen/acs12_04/st48/geo_2012_g_48000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '48', '/mtdata003/geo/dads/gen/acs12_04/st48/geo_2012_g_48000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '48', '/mtdata003/geo/dads/gen/acs12_04/st48/geo_2012_g_48000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '48', '/mtdata003/geo/dads/gen/acs12_04/st48/geo_2012_g_48000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '49', '/mtdata003/geo/dads/gen/acs12_04/st49/geo_2012_g_49000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '49', '/mtdata003/geo/dads/gen/acs12_04/st49/geo_2012_g_49000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '49', '/mtdata003/geo/dads/gen/acs12_04/st49/geo_2012_g_49000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '49', '/mtdata003/geo/dads/gen/acs12_04/st49/geo_2012_g_49000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '50', '/mtdata003/geo/dads/gen/acs12_04/st50/geo_2012_g_50000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '50', '/mtdata003/geo/dads/gen/acs12_04/st50/geo_2012_g_50000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '51', '/mtdata003/geo/dads/gen/acs12_04/st51/geo_2012_g_51000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '51', '/mtdata003/geo/dads/gen/acs12_04/st51/geo_2012_g_51000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '51', '/mtdata003/geo/dads/gen/acs12_04/st51/geo_2012_g_51000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '51', '/mtdata003/geo/dads/gen/acs12_04/st51/geo_2012_g_51000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '51', '/mtdata003/geo/dads/gen/acs12_04/st51/geo_2012_g_51000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '53', '/mtdata003/geo/dads/gen/acs12_04/st53/geo_2012_g_53000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '53', '/mtdata003/geo/dads/gen/acs12_04/st53/geo_2012_g_53000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '53', '/mtdata003/geo/dads/gen/acs12_04/st53/geo_2012_g_53000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '53', '/mtdata003/geo/dads/gen/acs12_04/st53/geo_2012_g_53000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '53', '/mtdata003/geo/dads/gen/acs12_04/st53/geo_2012_g_53000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '54', '/mtdata003/geo/dads/gen/acs12_04/st54/geo_2012_g_54000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '54', '/mtdata003/geo/dads/gen/acs12_04/st54/geo_2012_g_54000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '54', '/mtdata003/geo/dads/gen/acs12_04/st54/geo_2012_g_54000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '54', '/mtdata003/geo/dads/gen/acs12_04/st54/geo_2012_g_54000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '54', '/mtdata003/geo/dads/gen/acs12_04/st54/geo_2012_g_54000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '55', '/mtdata003/geo/dads/gen/acs12_04/st55/geo_2012_g_55000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '55', '/mtdata003/geo/dads/gen/acs12_04/st55/geo_2012_g_55000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '55', '/mtdata003/geo/dads/gen/acs12_04/st55/geo_2012_g_55000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '55', '/mtdata003/geo/dads/gen/acs12_04/st55/geo_2012_g_55000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '55', '/mtdata003/geo/dads/gen/acs12_04/st55/geo_2012_g_55000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '56', '/mtdata003/geo/dads/gen/acs12_04/st56/geo_2012_g_56000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '56', '/mtdata003/geo/dads/gen/acs12_04/st56/geo_2012_g_56000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '72', '/mtdata003/geo/dads/gen/acs12_04/st72/geo_2012_g_72000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '72', '/mtdata003/geo/dads/gen/acs12_04/st72/geo_2012_g_72000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '72', '/mtdata003/geo/dads/gen/acs12_04/st72/geo_2012_g_72000_904_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '78', '/mtdata003/geo/dads/gen/acs12_04/st78/geo_2012_g_78000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', 'nation', '/mtdata003/geo/dads/gen/acs12_04/nation/geo_2012_g_00000_050_00_ln_z8', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z8', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', 'nation', '/mtdata003/geo/dads/gen/acs12_04/nation/geo_2012_g_00000_050_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', 'nation', '/mtdata003/geo/dads/gen/acs12_04/nation/geo_2012_g_00000_050_00_py_z8', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z8', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', 'nation', '/mtdata003/geo/dads/gen/acs12_04/nation/geo_2012_g_00000_310_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', 'nation', '/mtdata003/geo/dads/gen/acs12_04/nation/geo_2012_g_00000_314_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', 'nation', '/mtdata003/geo/dads/gen/acs12_04/nation/geo_2012_g_00000_330_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', 'nation', '/mtdata003/geo/dads/gen/acs12_04/nation/geo_2012_p_00000_050_00_py_z9', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z9', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', 'nation', '/mtdata003/geo/dads/gen/acs12_04/nation/geo_2012_p_00000_310_m2_py_z9', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z9', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', 'nation', '/mtdata003/geo/dads/gen/acs12_04/nation/geo_2012_p_00000_330_m2_py_z9', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z9', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '01', '/mtdata003/geo/dads/gen/acs12_04/st01/geo_2012_g_01000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '01', '/mtdata003/geo/dads/gen/acs12_04/st01/geo_2012_g_01000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '01', '/mtdata003/geo/dads/gen/acs12_04/st01/geo_2012_g_01000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '01', '/mtdata003/geo/dads/gen/acs12_04/st01/geo_2012_g_01000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '02', '/mtdata003/geo/dads/gen/acs12_04/st02/geo_2012_g_02000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '02', '/mtdata003/geo/dads/gen/acs12_04/st02/geo_2012_g_02000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '04', '/mtdata003/geo/dads/gen/acs12_04/st04/geo_2012_g_04000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '04', '/mtdata003/geo/dads/gen/acs12_04/st04/geo_2012_g_04000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '04', '/mtdata003/geo/dads/gen/acs12_04/st04/geo_2012_g_04000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '04', '/mtdata003/geo/dads/gen/acs12_04/st04/geo_2012_g_04000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '05', '/mtdata003/geo/dads/gen/acs12_04/st05/geo_2012_g_05000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '05', '/mtdata003/geo/dads/gen/acs12_04/st05/geo_2012_g_05000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '05', '/mtdata003/geo/dads/gen/acs12_04/st05/geo_2012_g_05000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '05', '/mtdata003/geo/dads/gen/acs12_04/st05/geo_2012_g_05000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '06', '/mtdata003/geo/dads/gen/acs12_04/st06/geo_2012_g_06000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '06', '/mtdata003/geo/dads/gen/acs12_04/st06/geo_2012_g_06000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '06', '/mtdata003/geo/dads/gen/acs12_04/st06/geo_2012_g_06000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '06', '/mtdata003/geo/dads/gen/acs12_04/st06/geo_2012_g_06000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '06', '/mtdata003/geo/dads/gen/acs12_04/st06/geo_2012_g_06000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '08', '/mtdata003/geo/dads/gen/acs12_04/st08/geo_2012_g_08000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '08', '/mtdata003/geo/dads/gen/acs12_04/st08/geo_2012_g_08000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '08', '/mtdata003/geo/dads/gen/acs12_04/st08/geo_2012_g_08000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '08', '/mtdata003/geo/dads/gen/acs12_04/st08/geo_2012_g_08000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '09', '/mtdata003/geo/dads/gen/acs12_04/st09/geo_2012_g_09000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '09', '/mtdata003/geo/dads/gen/acs12_04/st09/geo_2012_g_09000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '09', '/mtdata003/geo/dads/gen/acs12_04/st09/geo_2012_g_09000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '10', '/mtdata003/geo/dads/gen/acs12_04/st10/geo_2012_g_10000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '10', '/mtdata003/geo/dads/gen/acs12_04/st10/geo_2012_g_10000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '10', '/mtdata003/geo/dads/gen/acs12_04/st10/geo_2012_g_10000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '10', '/mtdata003/geo/dads/gen/acs12_04/st10/geo_2012_g_10000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '10', '/mtdata003/geo/dads/gen/acs12_04/st10/geo_2012_g_10000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '11', '/mtdata003/geo/dads/gen/acs12_04/st11/geo_2012_g_11000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '11', '/mtdata003/geo/dads/gen/acs12_04/st11/geo_2012_g_11000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '11', '/mtdata003/geo/dads/gen/acs12_04/st11/geo_2012_g_11000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '11', '/mtdata003/geo/dads/gen/acs12_04/st11/geo_2012_g_11000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '12', '/mtdata003/geo/dads/gen/acs12_04/st12/geo_2012_g_12000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '12', '/mtdata003/geo/dads/gen/acs12_04/st12/geo_2012_g_12000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '12', '/mtdata003/geo/dads/gen/acs12_04/st12/geo_2012_g_12000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '12', '/mtdata003/geo/dads/gen/acs12_04/st12/geo_2012_g_12000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '12', '/mtdata003/geo/dads/gen/acs12_04/st12/geo_2012_g_12000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '13', '/mtdata003/geo/dads/gen/acs12_04/st13/geo_2012_g_13000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '13', '/mtdata003/geo/dads/gen/acs12_04/st13/geo_2012_g_13000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '13', '/mtdata003/geo/dads/gen/acs12_04/st13/geo_2012_g_13000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '13', '/mtdata003/geo/dads/gen/acs12_04/st13/geo_2012_g_13000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '15', '/mtdata003/geo/dads/gen/acs12_04/st15/geo_2012_g_15000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '15', '/mtdata003/geo/dads/gen/acs12_04/st15/geo_2012_g_15000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '16', '/mtdata003/geo/dads/gen/acs12_04/st16/geo_2012_g_16000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '16', '/mtdata003/geo/dads/gen/acs12_04/st16/geo_2012_g_16000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '16', '/mtdata003/geo/dads/gen/acs12_04/st16/geo_2012_g_16000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '16', '/mtdata003/geo/dads/gen/acs12_04/st16/geo_2012_g_16000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '17', '/mtdata003/geo/dads/gen/acs12_04/st17/geo_2012_g_17000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '17', '/mtdata003/geo/dads/gen/acs12_04/st17/geo_2012_g_17000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '17', '/mtdata003/geo/dads/gen/acs12_04/st17/geo_2012_g_17000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '17', '/mtdata003/geo/dads/gen/acs12_04/st17/geo_2012_g_17000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '17', '/mtdata003/geo/dads/gen/acs12_04/st17/geo_2012_g_17000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '18', '/mtdata003/geo/dads/gen/acs12_04/st18/geo_2012_g_18000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '18', '/mtdata003/geo/dads/gen/acs12_04/st18/geo_2012_g_18000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '18', '/mtdata003/geo/dads/gen/acs12_04/st18/geo_2012_g_18000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '18', '/mtdata003/geo/dads/gen/acs12_04/st18/geo_2012_g_18000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '18', '/mtdata003/geo/dads/gen/acs12_04/st18/geo_2012_g_18000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '19', '/mtdata003/geo/dads/gen/acs12_04/st19/geo_2012_g_19000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '19', '/mtdata003/geo/dads/gen/acs12_04/st19/geo_2012_g_19000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '19', '/mtdata003/geo/dads/gen/acs12_04/st19/geo_2012_g_19000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '19', '/mtdata003/geo/dads/gen/acs12_04/st19/geo_2012_g_19000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '20', '/mtdata003/geo/dads/gen/acs12_04/st20/geo_2012_g_20000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '20', '/mtdata003/geo/dads/gen/acs12_04/st20/geo_2012_g_20000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '20', '/mtdata003/geo/dads/gen/acs12_04/st20/geo_2012_g_20000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '20', '/mtdata003/geo/dads/gen/acs12_04/st20/geo_2012_g_20000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '21', '/mtdata003/geo/dads/gen/acs12_04/st21/geo_2012_g_21000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '21', '/mtdata003/geo/dads/gen/acs12_04/st21/geo_2012_g_21000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '21', '/mtdata003/geo/dads/gen/acs12_04/st21/geo_2012_g_21000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '21', '/mtdata003/geo/dads/gen/acs12_04/st21/geo_2012_g_21000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '22', '/mtdata003/geo/dads/gen/acs12_04/st22/geo_2012_g_22000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '22', '/mtdata003/geo/dads/gen/acs12_04/st22/geo_2012_g_22000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '22', '/mtdata003/geo/dads/gen/acs12_04/st22/geo_2012_g_22000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '22', '/mtdata003/geo/dads/gen/acs12_04/st22/geo_2012_g_22000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '23', '/mtdata003/geo/dads/gen/acs12_04/st23/geo_2012_g_23000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '23', '/mtdata003/geo/dads/gen/acs12_04/st23/geo_2012_g_23000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '23', '/mtdata003/geo/dads/gen/acs12_04/st23/geo_2012_g_23000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '24', '/mtdata003/geo/dads/gen/acs12_04/st24/geo_2012_g_24000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '24', '/mtdata003/geo/dads/gen/acs12_04/st24/geo_2012_g_24000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '24', '/mtdata003/geo/dads/gen/acs12_04/st24/geo_2012_g_24000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '24', '/mtdata003/geo/dads/gen/acs12_04/st24/geo_2012_g_24000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '24', '/mtdata003/geo/dads/gen/acs12_04/st24/geo_2012_g_24000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '25', '/mtdata003/geo/dads/gen/acs12_04/st25/geo_2012_g_25000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '25', '/mtdata003/geo/dads/gen/acs12_04/st25/geo_2012_g_25000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '25', '/mtdata003/geo/dads/gen/acs12_04/st25/geo_2012_g_25000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '25', '/mtdata003/geo/dads/gen/acs12_04/st25/geo_2012_g_25000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '26', '/mtdata003/geo/dads/gen/acs12_04/st26/geo_2012_g_26000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '26', '/mtdata003/geo/dads/gen/acs12_04/st26/geo_2012_g_26000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '26', '/mtdata003/geo/dads/gen/acs12_04/st26/geo_2012_g_26000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '26', '/mtdata003/geo/dads/gen/acs12_04/st26/geo_2012_g_26000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '26', '/mtdata003/geo/dads/gen/acs12_04/st26/geo_2012_g_26000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '27', '/mtdata003/geo/dads/gen/acs12_04/st27/geo_2012_g_27000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '27', '/mtdata003/geo/dads/gen/acs12_04/st27/geo_2012_g_27000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '27', '/mtdata003/geo/dads/gen/acs12_04/st27/geo_2012_g_27000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '27', '/mtdata003/geo/dads/gen/acs12_04/st27/geo_2012_g_27000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '28', '/mtdata003/geo/dads/gen/acs12_04/st28/geo_2012_g_28000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '28', '/mtdata003/geo/dads/gen/acs12_04/st28/geo_2012_g_28000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '28', '/mtdata003/geo/dads/gen/acs12_04/st28/geo_2012_g_28000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '28', '/mtdata003/geo/dads/gen/acs12_04/st28/geo_2012_g_28000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '29', '/mtdata003/geo/dads/gen/acs12_04/st29/geo_2012_g_29000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '29', '/mtdata003/geo/dads/gen/acs12_04/st29/geo_2012_g_29000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '29', '/mtdata003/geo/dads/gen/acs12_04/st29/geo_2012_g_29000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '29', '/mtdata003/geo/dads/gen/acs12_04/st29/geo_2012_g_29000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '30', '/mtdata003/geo/dads/gen/acs12_04/st30/geo_2012_g_30000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '30', '/mtdata003/geo/dads/gen/acs12_04/st30/geo_2012_g_30000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '31', '/mtdata003/geo/dads/gen/acs12_04/st31/geo_2012_g_31000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '31', '/mtdata003/geo/dads/gen/acs12_04/st31/geo_2012_g_31000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '31', '/mtdata003/geo/dads/gen/acs12_04/st31/geo_2012_g_31000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '32', '/mtdata003/geo/dads/gen/acs12_04/st32/geo_2012_g_32000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '32', '/mtdata003/geo/dads/gen/acs12_04/st32/geo_2012_g_32000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '32', '/mtdata003/geo/dads/gen/acs12_04/st32/geo_2012_g_32000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '32', '/mtdata003/geo/dads/gen/acs12_04/st32/geo_2012_g_32000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '33', '/mtdata003/geo/dads/gen/acs12_04/st33/geo_2012_g_33000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '33', '/mtdata003/geo/dads/gen/acs12_04/st33/geo_2012_g_33000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '33', '/mtdata003/geo/dads/gen/acs12_04/st33/geo_2012_g_33000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '33', '/mtdata003/geo/dads/gen/acs12_04/st33/geo_2012_g_33000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '34', '/mtdata003/geo/dads/gen/acs12_04/st34/geo_2012_g_34000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '34', '/mtdata003/geo/dads/gen/acs12_04/st34/geo_2012_g_34000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '34', '/mtdata003/geo/dads/gen/acs12_04/st34/geo_2012_g_34000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '34', '/mtdata003/geo/dads/gen/acs12_04/st34/geo_2012_g_34000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '35', '/mtdata003/geo/dads/gen/acs12_04/st35/geo_2012_g_35000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '35', '/mtdata003/geo/dads/gen/acs12_04/st35/geo_2012_g_35000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '35', '/mtdata003/geo/dads/gen/acs12_04/st35/geo_2012_g_35000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '35', '/mtdata003/geo/dads/gen/acs12_04/st35/geo_2012_g_35000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '36', '/mtdata003/geo/dads/gen/acs12_04/st36/geo_2012_g_36000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '36', '/mtdata003/geo/dads/gen/acs12_04/st36/geo_2012_g_36000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '36', '/mtdata003/geo/dads/gen/acs12_04/st36/geo_2012_g_36000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '36', '/mtdata003/geo/dads/gen/acs12_04/st36/geo_2012_g_36000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '36', '/mtdata003/geo/dads/gen/acs12_04/st36/geo_2012_g_36000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '37', '/mtdata003/geo/dads/gen/acs12_04/st37/geo_2012_g_37000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '37', '/mtdata003/geo/dads/gen/acs12_04/st37/geo_2012_g_37000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '37', '/mtdata003/geo/dads/gen/acs12_04/st37/geo_2012_g_37000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '37', '/mtdata003/geo/dads/gen/acs12_04/st37/geo_2012_g_37000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '38', '/mtdata003/geo/dads/gen/acs12_04/st38/geo_2012_g_38000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '38', '/mtdata003/geo/dads/gen/acs12_04/st38/geo_2012_g_38000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '38', '/mtdata003/geo/dads/gen/acs12_04/st38/geo_2012_g_38000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '39', '/mtdata003/geo/dads/gen/acs12_04/st39/geo_2012_g_39000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '39', '/mtdata003/geo/dads/gen/acs12_04/st39/geo_2012_g_39000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '39', '/mtdata003/geo/dads/gen/acs12_04/st39/geo_2012_g_39000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '39', '/mtdata003/geo/dads/gen/acs12_04/st39/geo_2012_g_39000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '40', '/mtdata003/geo/dads/gen/acs12_04/st40/geo_2012_g_40000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '40', '/mtdata003/geo/dads/gen/acs12_04/st40/geo_2012_g_40000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '40', '/mtdata003/geo/dads/gen/acs12_04/st40/geo_2012_g_40000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '40', '/mtdata003/geo/dads/gen/acs12_04/st40/geo_2012_g_40000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '41', '/mtdata003/geo/dads/gen/acs12_04/st41/geo_2012_g_41000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '41', '/mtdata003/geo/dads/gen/acs12_04/st41/geo_2012_g_41000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '41', '/mtdata003/geo/dads/gen/acs12_04/st41/geo_2012_g_41000_e60_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '41', '/mtdata003/geo/dads/gen/acs12_04/st41/geo_2012_g_41000_e65_00_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '42', '/mtdata003/geo/dads/gen/acs12_04/st42/geo_2012_g_42000_320_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '42', '/mtdata003/geo/dads/gen/acs12_04/st42/geo_2012_g_42000_323_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
Insert into DADSFILE_ACS12_04
   (MODTAG, UNIT, DISKFILE, DEL_FLAG, DATE_TIME, RESOLUTION, WAVE)
 Values
   ('ACS12', '42', '/mtdata003/geo/dads/gen/acs12_04/st42/geo_2012_g_42000_340_m2_py_z6', 'Y', 
    TO_TIMESTAMP('7/23/2013 5:00:00.000000 PM','fmMMfm/fmDDfm/YYYY fmHH12fm:MI:SS.FF AM'), 'z6', 4);
COMMIT;

/* Formatted on 7/24/2013 4:38:32 PM (QP5 v5.163.1008.3004) */
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
     FROM DADSGEN.DADSFILE_DEC10W5
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
     FROM DADSFILE_ACS12_04
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
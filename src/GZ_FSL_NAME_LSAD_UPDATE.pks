CREATE OR REPLACE PACKAGE GZ_FSL_NAME_LSAD_UPDATE AUTHID CURRENT_USER AS
/******************************************************************************
 NAME: FSL_Name_Lsad_Update
 PURPOSE: Update FSL tables' NAME and LSAD columns
 Author : Al Freeman
 REVISIONS:
 Ver Date Description
 --------- ---------- ------------------------------------
 1.0 3/31/2010 1. Created this package.
******************************************************************************/
/** Set global variables to be used by functions/procedures in this package */
 -- GenSchema CONSTANT VARCHAR2(30) := 'ACS09GH'; --Please modify value to run in other schemas
 -- GenSchema CONSTANT VARCHAR2(30) := 'GEN_ACS07_GH3';--for testing
 GenSchema VARCHAR2(30);
 sqlstmt VARCHAR2(4000);
 g_table VARCHAR2(90);
 g_name VARCHAR2(90);
 g_federal_tribe CONSTANT VARCHAR2(1) := 'F'; --Federal tribe
 g_state_tribe CONSTANT VARCHAR2(1) := 'S'; --State tribe
 g_topology VARCHAR2(10);
PROCEDURE update_summary_levels(Topology VARCHAR2, Deploy VARCHAR2);
PROCEDURE fsl155_update(GenSchema VARCHAR2);
PROCEDURE fsl250_update(pGenSchema VARCHAR2);
PROCEDURE fsl251_update(pGenSchema VARCHAR2);
PROCEDURE fsl252_update(GenSchema VARCHAR2);
PROCEDURE fsl254_update(GenSchema VARCHAR2);
PROCEDURE fsl280_update(GenSchema VARCHAR2);
PROCEDURE fsl283_update(GenSchema VARCHAR2);
PROCEDURE fsl310_update(GenSchema VARCHAR2);
PROCEDURE fsl314_update(GenSchema VARCHAR2);
PROCEDURE fsl320_update(GenSchema VARCHAR2);
PROCEDURE fsl323_update(GenSchema VARCHAR2);
PROCEDURE fsl330_update(GenSchema VARCHAR2);
PROCEDURE fsl332_update(GenSchema VARCHAR2);
PROCEDURE fsl335_update(GenSchema VARCHAR2);
PROCEDURE fsl337_update(GenSchema VARCHAR2);
PROCEDURE fsl340_update(GenSchema VARCHAR2);
PROCEDURE fsl345_update(GenSchema VARCHAR2);
PROCEDURE fsl350_update(GenSchema VARCHAR2);
PROCEDURE fsl355_update(GenSchema VARCHAR2);
PROCEDURE fsl360_update(GenSchema VARCHAR2);
PROCEDURE fsl364_update(GenSchema VARCHAR2);
PROCEDURE fsl400_update(GenSchema VARCHAR2);
PROCEDURE fsl420_update(GenSchema VARCHAR2);
PROCEDURE fsl500_update(GenSchema VARCHAR2);
PROCEDURE fsl550_update(GenSchema VARCHAR2);
PROCEDURE fsl610_update(GenSchema VARCHAR2);
PROCEDURE fsl620_update(GenSchema VARCHAR2);
PROCEDURE fsl700_update(GenSchema VARCHAR2);
PROCEDURE fsl950_update(GenSchema VARCHAR2);
PROCEDURE fsl960_update(GenSchema VARCHAR2);
PROCEDURE fsl970_update(GenSchema VARCHAR2);
PROCEDURE fsl140_update(GenSchema VARCHAR2);
PROCEDURE fsl150_update(GenSchema VARCHAR2);
PROCEDURE fsl871_update(GenSchema VARCHAR2);
FUNCTION check_4_name_column(GenSchema VARCHAR2, Current_table VARCHAR2) RETURN NUMBER;
FUNCTION find_topology_table(GenSchema IN VARCHAR2, Current_table IN VARCHAR2) RETURN NUMBER;

END GZ_FSL_NAME_LSAD_UPDATE;
/


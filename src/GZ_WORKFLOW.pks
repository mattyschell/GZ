CREATE OR REPLACE PACKAGE GZ_WORKFLOW
AUTHID CURRENT_USER
AS
/******************************************************************************
   NAME:       GZ_WORKFLOW
   PURPOSE:

   REVISIONS:
   Ver        Date        Author           Description
   ---------  ----------  ---------------  ------------------------------------
   1.0        7/11/2011      karpu001       1. Created this package.
******************************************************************************/

  PROCEDURE Update_WorkFlow_Status(
     pJobID IN VARCHAR2,
     pModule IN VARCHAR2,
     pAction IN VARCHAR2,
     pStatus IN VARCHAR2
     );

  PROCEDURE Generalize(pJobID IN VARCHAR2);

  PROCEDURE Create_Table(pSchemaName VARCHAR2, pTableName VARCHAR2, pGzType VARCHAR2, pDropFlag VARCHAR2 DEFAULT 'N');

  PROCEDURE Insert_New_jobs(pRel IN VARCHAR2, pPrj IN VARCHAR2, pSchema IN VARCHAR2 default USER, ptopobuildtype IN VARCHAR2 default 'A');

  PROCEDURE Validate_Job(pJobID IN VARCHAR2);

  PROCEDURE Delete_Job(pJobID IN VARCHAR2);

  PROCEDURE Get_Status(pRel IN VARCHAR2, pPrj IN VARCHAR2);

  PROCEDURE Get_Status(pJobID IN VARCHAR2);

  PROCEDURE Copy_Project(pSrc_Prj IN VARCHAR2, pTgt_Prj IN VARCHAR2);

  PROCEDURE Copy_Job_Prm(pSrc_Rel IN VARCHAR2, pSrc_Prj IN VARCHAR2, pTgt_Rel IN VARCHAR2, pTgt_Prj IN VARCHAR2);

  PROCEDURE Create_Empty_Prm_Tbls;
  
  PROCEDURE CREATE_SETUP_TABLE_SUPPORT (
      setup_tbl            IN VARCHAR2
   );

  PROCEDURE Create_Empty_Setup_Tbls;

  FUNCTION GET_SHAPEFILE_PRCS_STR(pJobID IN VARCHAR2)
  RETURN VARCHAR2;

  PROCEDURE VERIFY_SETUP_TABLE (p_jobid IN VARCHAR2, p_correct IN VARCHAR2 DEFAULT 'Y') ;

   FUNCTION FIX_TOPO_FACE (
      pTopology        VARCHAR2,
      pRelease        VARCHAR2,
      pProjectID        VARCHAR2)
      RETURN NUMBER;

END GZ_WORKFLOW;
/

CREATE OR REPLACE PACKAGE GZ_LINESIM
AUTHID CURRENT_USER AS

PROCEDURE PRINT_EDGES_TOSKIP(
SOURCE_TOPOLOGY VARCHAR2,
DERIVED_TOPOLOGY VARCHAR2,
OUTPUT_TABLE_NAME VARCHAR2);

FUNCTION START_LOGGING (
   pSchema       IN VARCHAR2,
   pJobRun       IN VARCHAR2
)RETURN VARCHAR2;

PROCEDURE CREATE_GEN_LS_PARAMETERS (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2 DEFAULT 'LINE_SIM_PARAMETERS' -- should change to 'GEN_LS_PARAMETERS'
);

PROCEDURE CREATE_GEN_LS_TRACKING (
   p_schema         IN VARCHAR2,
   p_table_name     IN VARCHAR2 DEFAULT 'GEN_LS_TRACKING'
);

FUNCTION NEW_LINE_SIM_PARAMETERS RETURN GZ_TYPES.LINE_SIM_PARAMETERS PIPELINED;

FUNCTION NEW_GEN_LS_TRACKING RETURN GZ_TYPES.GEN_LS_TRACKING PIPELINED;

PROCEDURE GEN_LS_TRACKING_LOG (
     p_jobrun         IN VARCHAR2,
     p_process        IN VARCHAR2,
     p_table_name     IN VARCHAR2 DEFAULT NULL,     
     p_step           IN VARCHAR2 DEFAULT NULL,
     p_start_time     IN TIMESTAMP DEFAULT NULL,
     p_end_time       IN TIMESTAMP DEFAULT NULL,
     p_sqlstmt        IN VARCHAR2 DEFAULT NULL, 
     p_message      IN VARCHAR2 DEFAULT NULL
   );

PROCEDURE LINE_SIM (
      pSchema              IN VARCHAR2,
      pProjectId           IN VARCHAR2,
      pJobId               IN VARCHAR2,
      pTopo                IN VARCHAR2,
      pModules             IN VARCHAR2,
      pFromTopo            IN VARCHAR2,
      pTopoBk              IN VARCHAR2,
      pStateOutlineTable   IN VARCHAR2,
      pStateFP             IN VARCHAR2,
      pSkipEdgesTable      IN VARCHAR2,
      ptopofix_qa          IN VARCHAR2
      --,pReRun varchar2 DEFAULT 'N'
   );

   FUNCTION GET_LINESIM_PROJECT_PARAMETERS (
      p_project_id         IN VARCHAR2,
      p_release             IN VARCHAR2
   ) RETURN GZ_TYPES.LINE_SIM_PARAMETERS_REC;

END GZ_LINESIM;
/

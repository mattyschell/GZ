CREATE OR REPLACE PACKAGE GZ_DADSGEN
AUTHID CURRENT_USER
AS

   --This package is intended for code specifically needed in the CPB generalization DADSGEN reference schema
   --Primary purpose (9/18/13) is to generate a view called DADSFILE that is agnostic about the existence of 
   --   tables from which it selects


   --type that matches the view definition
   TYPE DADSFILE_REC IS RECORD
   (
     modtag       VARCHAR2(5),
     unit         VARCHAR2(6),
     diskfile     VARCHAR2(1000),
     rerun        VARCHAR2(1),
     del_flag     VARCHAR2(1),
     date_time    TIMESTAMP,
     resolution   VARCHAR2(30),
     wave         NUMBER
   );

   TYPE DADSFILE_TAB IS TABLE OF DADSFILE_REC;

   --function that pipes view records
   FUNCTION PIPE_DADSFILE (
      table_name           IN VARCHAR2,
      where_clause         IN VARCHAR2 DEFAULT NULL,
      substr_unit          IN VARCHAR2 DEFAULT 'N',
      concat_diskfile      IN VARCHAR2 DEFAULT 'N',
      date_time_other      IN VARCHAR2 DEFAULT 'N',
      rerun_is_null        IN VARCHAR2 DEFAULT 'N'
   ) RETURN GZ_DADSGEN.DADSFILE_TAB PIPELINED;
   
   --prep dadsgen schema for copy from devbnch to big boy world
   PROCEDURE PREP_DADSGEN (
      p_tables          IN VARCHAR2 DEFAULT 'Y',
      p_views           IN VARCHAR2 DEFAULT 'Y',
      p_packages        IN VARCHAR2 DEFAULT 'Y'
   );

 

END GZ_DADSGEN;
/

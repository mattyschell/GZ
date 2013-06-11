CREATE OR REPLACE PROCEDURE GZ_PRIV_GRANTER (
   p_ref_schema_table   IN VARCHAR2 DEFAULT 'REFERENCE_SCHEMAS',
   p_like_clause        IN VARCHAR2 DEFAULT NULL
)
AS

      --Matt! 12/03/10
      --Not sure where this is gonna end up, standalone procedure for now
      --Note default definers rights, no AUTHID CURRENT USER
      --This procedure grants privvies on the schema where it is housed

      --Sample 1, NOT logged in as GZCPB1
      --Grant privs specified in reference table on all tables in the Z609 topology
      --BEGIN
      --GZCPB1.EZ_PRIV_GRANTER('REFERENCE_SCHEMAS','Z609%');
      --END;

      --Sample 2, NOT logged in as GZCPB1
      --Unlock it all
      --BEGIN
      --GZCPB1.EZ_PRIV_GRANTER();
      --END;

      --Sample 3, as GZCPB1 unlock my own Z609 tables
      --BEGIN
      --EZ_PRIV_GRANTER(NULL,'Z609%');
      --END;


      legal_users           GZ_TYPES.stringhash;
      bunched_users         GZ_TYPES.stringhash;
      v_hashindex           VARCHAR2(4000);
      ref_table             VARCHAR2(4000);
      like_clause           VARCHAR2(4000);
      v_session_user        VARCHAR2(4000);
      v_current_user        VARCHAR2(4000);
      psql                  VARCHAR2(4000);
      tabs                  GZ_TYPES.stringarray;



BEGIN

   ref_table   := UPPER(p_ref_schema_table);

   IF p_like_clause IS NOT NULL
   THEN

      like_clause := UPPER(p_like_clause);

   END IF;

   --get caller
   v_session_user := SYS_CONTEXT('USERENV', 'SESSION_USER');

   --11G environment ...   
   --IF v_session_user NOT LIKE 'GZ%'
   --THEN

      --RAISE_APPLICATION_ERROR(-20001,'Sorry, Im pretty fast and loose with permissions, but you have to be a GZ to use this ');

   --END IF;

   --get code and table owner
   v_current_user := SYS_CONTEXT('USERENV', 'CURRENT_USER');

   psql := 'SELECT a.table_name '
        || 'FROM user_tables a '
        || 'WHERE '
        || 'a.table_name NOT LIKE :p1 ';

   IF p_like_clause IS NULL
   THEN

      EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING 'MD%';

   ELSE

      psql := psql || ' AND '
                   || 'a.table_name LIKE :p2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING 'MD%',
                                                           like_clause;

   END IF;

   IF tabs.COUNT = 0
   THEN

      --I think this is fatal (usually a typo) and I want to hear about it, but I could be convinced otherwise
      RAISE_APPLICATION_ERROR(-20001, 'Yo, cant find any tables to grant privvies to');

   END IF;


   --Get ref schemas filters out schemas that dont exist on this DB
   legal_users := GZ_UTILITIES.GET_REF_SCHEMAS(v_current_user, ref_table);

   --legal_users looks something like
   --(GZDEC10ST99) SELECT
   --(GZCPB_1)     SELECT, INSERT, UPDATE, DELETE, INDEX

   --For improved performance, lets bunch up similar grant types into a single call
   --Ex GRANT SELECT on xx to GZDECST01, GZDECST02, ....

   v_hashindex := legal_users.FIRST;

   LOOP

      EXIT WHEN NOT legal_users.EXISTS(v_hashindex);

      --switch keys and values as we bunch ala
      --(SELECT) GZDEC10ST01, GZDEC10ST02,...
      --(SELECT, INSERT, UPDATE) GZCPB_1, GZCPB_2


      IF bunched_users.EXISTS(legal_users(v_hashindex))
      AND v_hashindex != v_current_user
      THEN

         --            SELECT, INSERT, UPDATE                     GZCPB_1                   , GZCPB_2
         bunched_users(legal_users(v_hashindex)) := bunched_users(legal_users(v_hashindex)) || ', ' || v_hashindex || ' ';

      ELSIF v_hashindex != v_current_user
      THEN

         --            SELECT, INSERT, UPDATE       GZCPB_1
         bunched_users(legal_users(v_hashindex)) := v_hashindex;

      ELSE

         --no grants to self
         NULL;

      END IF;

      v_hashindex := legal_users.NEXT(v_hashindex);

   END LOOP;


   v_hashindex := bunched_users.FIRST;

   LOOP

      EXIT WHEN NOT bunched_users.EXISTS(v_hashindex);

      FOR i IN 1 .. tabs.COUNT
      LOOP

         EXECUTE IMMEDIATE 'GRANT ' || v_hashindex || ' ON ' || tabs(i) || ' TO ' || bunched_users(v_hashindex) || ' WITH GRANT OPTION ';

      END LOOP;

      v_hashindex := bunched_users.NEXT(v_hashindex);

   END LOOP;

END;
/

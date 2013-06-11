CREATE OR REPLACE PACKAGE GZ_TOPO_BUILD AUTHID CURRENT_USER 
AS

procedure load_source_for_nation (topology VARCHAR2, mt_nation_source_tbl_name VARCHAR2, mt_state_load_tbl_name VARCHAR2, vintage VARCHAR2, release VARCHAR2, deploy VARCHAR2);

procedure create_topo_tables (topology varchar2, topo_universe varchar2, release varchar2, deploy varchar2);

procedure create_topology (topology varchar2, topo_hierarchy varchar2, release varchar2, deploy varchar2, tolerance number, srid number, grid varchar2 default '16', drop_ft varchar2 default 'N');

procedure drop_topology (topo_name in varchar2, topo_owner in varchar2, release varchar2, deploy varchar2, delete_topo_tables in varchar2 default 'N');

procedure drop_topology2 (topo_name IN VARCHAR2, topo_owner IN VARCHAR2, release VARCHAR2, deploy VARCHAR2, delete_topo_tables IN VARCHAR2 DEFAULT 'N', raise_errors IN VARCHAR2 DEFAULT NULL);

procedure load_topo_sl_0 (topology varchar2, topo_universe varchar2, topo_hierarchy varchar2, release varchar2, deploy varchar2, load_statefp varchar2 default 'FALSE');

procedure load_topo_sl_0_statefp (topology varchar2, tbl_type varchar2, topo_universe varchar2, topo_hierarchy varchar2, state_tbl varchar2, release varchar2, deploy varchar2);

procedure load_topo_sl_1_up (topology varchar2, sl_type varchar2, topo_universe varchar2, topo_hierarchy varchar2, release varchar2, deploy varchar2);

procedure load_topo_fsl_state (topology VARCHAR2, sl_type VARCHAR2, topo_universe VARCHAR2, topo_hierarchy VARCHAR2, release VARCHAR2, deploy VARCHAR2);

procedure add_poly_sl_0 (topology varchar2, topo_universe varchar2, topo_hierarchy varchar2, release varchar2, deploy varchar2, load_statefp varchar2 default 'FALSE');

procedure add_poly_sl_0_statefp (topology varchar2, tbl_type varchar2, topo_universe varchar2, topo_hierarchy varchar2, state_tbl varchar2, release varchar2, deploy varchar2);

procedure load_topo_single_state (topology varchar2, tbl_type varchar2, statefp varchar2, topo_universe varchar2, topo_hierarchy varchar2, state_tbl varchar2, release varchar2, deploy varchar2);

procedure add_poly_single_state (topology varchar2, tbl_type varchar2, statefp varchar2, topo_universe varchar2, topo_hierarchy varchar2, state_tbl varchar2, release varchar2, deploy varchar2);

procedure update_division_regionce (tbl_name varchar2);

function count_occurs (str varchar2,search varchar2 := null) return number;

function create_dml_condition (tbl_keys in VARCHAR2, tbl_keys_data in varchar2) RETURN VARCHAR2;

function create_ft_cols (table_name IN varchar2) RETURN VARCHAR2;

function create_dml_condition2 (tbl_keys in VARCHAR2, tbl_keys_data in varchar2) RETURN VARCHAR2;

procedure create_cousub_estimates (source_tbl_name varchar2);

procedure valtopo (topology varchar2, state_tbl varchar2, release varchar2, deploy varchar2);

procedure valtopo2 (topology varchar2, state_table varchar2, release varchar2, deploy varchar2);

procedure update_data_length (topology varchar2, topo_universe varchar2, release varchar2, deploy varchar2);

procedure build_face_cols_mz (topology VARCHAR2,pFaceTable VARCHAR2,pFaceDollarTable VARCHAR2,topo_universe VARCHAR2);

procedure build_face_cols (topology VARCHAR2,pFaceTable VARCHAR2,pFaceDollarTable VARCHAR2,topo_universe VARCHAR2);

procedure load_face (topology varchar2, face_tbl varchar2, face_dollar_tbl varchar2, topo_universe varchar2, release varchar2, deploy varchar2);

procedure load_edge (topology varchar2, edge_tbl varchar2, edge_dollar_tbl varchar2, release varchar2, deploy varchar2);

procedure gen_tracking_log (topology IN VARCHAR2, table_name IN VARCHAR2 DEFAULT NULL, process IN VARCHAR2 DEFAULT NULL, step IN VARCHAR2 DEFAULT NULL, start_time IN TIMESTAMP DEFAULT NULL, end_time IN TIMESTAMP DEFAULT NULL, elapsed_time IN INTERVAL DAY TO SECOND DEFAULT NULL, release IN VARCHAR2 DEFAULT NULL, sqlstmt IN VARCHAR2 DEFAULT NULL, deploy IN VARCHAR2 DEFAULT NULL);

procedure create_lookup_output_tables (topology varchar2, release varchar2, deploy varchar2);

procedure add_sde_column_to_table(topology varchar2,table_type varchar2,release varchar2,deploy varchar2,pNewColumnName VARCHAR2 DEFAULT 'SDESEQ');

procedure copy_lookup_output_tables (source_schema varchar2, target_schema varchar2);

procedure execution_script (topology VARCHAR2,topo_universe VARCHAR2,topo_hierarchy VARCHAR2,statefp VARCHAR2 DEFAULT '99',state_tbl VARCHAR2 DEFAULT 'SL040',feature_mbr_only VARCHAR2 DEFAULT 'FALSE',validate VARCHAR2 DEFAULT 'Y',build_option NUMBER DEFAULT 2,cleanup VARCHAR2 DEFAULT 'N',release VARCHAR2,deploy VARCHAR2);

procedure execution_script_mz;

procedure export_import_topo (topology varchar2, release varchar2, deploy varchar2, orcl_instance varchar2, target_topology varchar2, target_deploy varchar2, orcl_instance2 varchar2);

procedure create_fsl070_source (topology varchar2, face_tbl varchar2, release varchar2, deploy varchar2);

procedure create_fsl080_source (topology varchar2, face_tbl varchar2, release varchar2, deploy varchar2);

procedure create_fsl157_source (topology varchar2, face_tbl varchar2, release varchar2, deploy varchar2);

procedure post_fsl172;

procedure process_fsl155_fsl312 (topology varchar2, release varchar2, deploy varchar2);

procedure process_fsl155 (topology varchar2, face_tbl varchar2, release varchar2, deploy varchar2);

procedure process_fsl312 (topology varchar2, fsl155 varchar2, release varchar2, deploy varchar2);

procedure create_fsl172_source (topology varchar2, face_tbl varchar2, release varchar2, deploy varchar2);

END GZ_TOPO_BUILD;
/


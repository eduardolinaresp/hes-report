************************************************************************
* Programa   : ZCLMM_CLEAN_FINAL_ENTRYSHEET                            *
* Descripción: Programa permite Quitar Indicador entrada final en HES  *
* Fecha      : 26.04.2024                                              *
* Autor      : Eduardo Linares                                         *
* Desarrollo :                                 *                       *
************************************************************************
* MODIFICACIONES                                                       *
* Fecha      :                                                         *
* Nombre     :                                                         *
* Descripción:                                                         *
************************************************************************
REPORT zclmm_clean_final_entrysheet MESSAGE-ID zmc_lock_rel_ses.
TABLES: essr.
SELECTION-SCREEN BEGIN OF BLOCK search WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS s_lblni FOR essr-lblni.
  SELECT-OPTIONS s_erdat FOR essr-erdat.
  SELECT-OPTIONS s_ernam FOR essr-ernam.
SELECTION-SCREEN END OF BLOCK search.
CLASS lcl_salv DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_data,
             checkbox  TYPE sap_bool,
             lblni     TYPE essr-lblni,
             erdat     TYPE essr-erdat,
             final     TYPE essr-final,
             ebeln     TYPE ekko-ebeln,
             bsart     TYPE ekko-bsart,
             ekorg     TYPE ekko-ekorg,
             ekgrp     TYPE ekko-ekgrp,
             lifnr     TYPE ekko-lifnr,
             name1     TYPE lfa1-name1,
             bedat     TYPE ekko-bedat,
             ebelp     TYPE ekpo-ebelp,
             matnr     TYPE ekpo-matnr,
             matkl     TYPE ekpo-matkl,
             werks     TYPE ekpo-werks,
             netwr     TYPE ekpo-netwr,
             waers     TYPE ekko-waers,
             cellcolor TYPE lvc_t_scol,
             celltype  TYPE salv_t_int4_column,
           END OF ty_data.
    DATA t_data TYPE TABLE OF ty_data.
    "-ALV Declarations
    DATA o_alv TYPE REF TO cl_salv_table.
    METHODS: init.
    METHODS get_data.
    METHODS display_alv.
    METHODS build_columns CHANGING lo_columns TYPE REF TO cl_salv_columns_table.
    METHODS set_text IMPORTING name       TYPE lvc_fname
                               s_text     TYPE scrtext_s
                               m_text     TYPE scrtext_m
                               l_text     TYPE scrtext_l
                     CHANGING  lo_columns TYPE REF TO cl_salv_columns_table.
    METHODS set_color IMPORTING name       TYPE lvc_fname
                                col        TYPE lvc_col
                                int        TYPE lvc_int
                                inv        TYPE lvc_inv
                      CHANGING  lo_columns TYPE REF TO cl_salv_columns_table.

    METHODS create_top_of_page.
    "*-----------------EVENTS-------------------------*
    METHODS on_link_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.
    METHODS: on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
    ty_rg_set41  TYPE RANGE OF ekko-bsart.
    METHODS clean_final_on_hes.
    METHODS get_configuration
      RETURNING VALUE(rg_set41_tab) TYPE ty_rg_set41.
ENDCLASS.

CLASS lcl_salv IMPLEMENTATION.
  METHOD init.
    get_data( ).
    display_alv( ).
  ENDMETHOD.
  METHOD get_data.
    FIELD-SYMBOLS <data> TYPE ty_data.
    DATA rg_set41_tab TYPE  ty_rg_set41.
    rg_set41_tab = get_configuration( ).
    SELECT
     essr~*,
     ekko~*,
     ekpo~*
     FROM essr
     INNER JOIN ekko ON ekko~ebeln EQ essr~ebeln
     INNER JOIN ekpo ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
       ON ekpo~ebeln EQ essr~ebeln
      AND ekpo~ebelp EQ essr~ebelp
    WHERE essr~lblni IN  @s_lblni AND
          essr~erdat IN  @s_erdat AND
          essr~ernam IN  @s_ernam AND
          essr~final =   @abap_true AND
          ekko~bsart IN  @rg_set41_tab
     INTO CORRESPONDING FIELDS OF TABLE @t_data.
  ENDMETHOD.

  METHOD display_alv.
    DATA lo_functions TYPE REF TO cl_salv_functions_list.
    DATA lo_display TYPE REF TO cl_salv_display_settings.
    DATA: lo_columns TYPE REF TO cl_salv_columns_table.
    DATA ls_key TYPE salv_s_layout_key .
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = o_alv
          CHANGING
            t_table      = t_data.
      CATCH cx_salv_msg INTO DATA(lo_err).
        MESSAGE ID lo_err->msgid TYPE lo_err->msgty NUMBER lo_err->msgno WITH lo_err->msgv1 lo_err->msgv1 lo_err->msgv1 lo_err->msgv1.
        RETURN.
    ENDTRY.
    "*-----------------Toolbar Design-------------------------*
    ls_key-report = sy-repid.
    o_alv->get_layout( )->set_key( value = ls_key ).
    o_alv->set_screen_status( report        = sy-repid"'SAPLSLVC_FULLSCREEN'
                              pfstatus      = 'SALV_STANDARD' "STANDARD_FULLSCREEN
                              set_functions = o_alv->c_functions_all ).
    "*-----------------Layout-Display Settings-------------------------*
    lo_display = o_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ). "zebra
    lo_display->set_list_header( 'SALV Full Template' ). "gui_title
    lo_display->set_horizontal_lines( cl_salv_display_settings=>false ).
    lo_display->set_vertical_lines( cl_salv_display_settings=>true ).
    lo_display->set_list_header( |{ TEXT-001 } { lines( t_data ) }| ).
    "*-----------------TOP-OF-PAGE-------------------------*
    me->create_top_of_page( ).
    "*-----------------Columns - Fieldcatalog-------------------------*
    lo_columns = o_alv->get_columns( ).
    lo_columns->set_optimize( abap_true ).
    lo_columns->set_key_fixation( abap_true ).
    build_columns( CHANGING lo_columns = lo_columns ).
    "*-----------------Color Column-------------------------*
    TRY.
        lo_columns->set_color_column( 'CELLCOLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.
    TRY.
        lo_columns->set_cell_type_column( 'CELLTYPE' ).
      CATCH cx_salv_data_error.
    ENDTRY.
    "*-----------------Register Events-------------------------*
    DATA: lr_events TYPE REF TO cl_salv_events_table.
    lr_events = o_alv->get_event( ).
    SET HANDLER me->on_link_click FOR lr_events.
    SET HANDLER me->on_user_command FOR lr_events.
    "*-----------------Selection-------------------------*
    DATA: lo_selections TYPE REF TO cl_salv_selections.
    lo_selections = o_alv->get_selections( ).
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
    "*-----------------Display-------------------------*
    o_alv->display( ).
  ENDMETHOD.

  METHOD build_columns.
    DATA lo_column  TYPE REF TO cl_salv_column_table.
    DATA lr_column  TYPE REF TO cl_salv_column.
    "----Alternate"
    DEFINE set_text.
      TRY .
          lr_column ?= lo_columns->get_column( &1 ).
          IF &2 NE space.
            lr_column->set_short_text( CONV #( &2 ) ).
          ENDIF.
          IF &3 NE space.
            lr_column->set_medium_text( CONV #( &3 ) ).
          ENDIF.
          IF &4 NE space.
            lr_column->set_long_text( CONV #( &4 ) ).
          ENDIF.
        CATCH cx_salv_not_found.                        "#EC NO_HANDLER
      ENDTRY.
    END-OF-DEFINITION.
    "----Alternate"
    "*-----------------Set Column Texts-------------------------*
*    set_text( EXPORTING name = 'ERSDA' s_text = 'Fecha Registro' m_text = 'Fecha Registro' l_text = 'Fecha Registro' CHANGING lo_columns = lo_columns ).
    "*-----------------Colum Design-------------------------*
    DATA tech_fields TYPE TABLE OF lvc_fname .
    tech_fields = VALUE #( ( 'CELLSTYLE' ) ( 'CELLCOLOR' ) ).
    TRY.
        lo_column ?= lo_columns->get_column( 'CHECKBOX' ).
        lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ). "editable checkbox
        lo_column->set_long_text( 'Checkbox' ).
        lo_column ?= lo_columns->get_column( 'LBLNI' ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_column ?= lo_columns->get_column( 'NETWR' ) ##NO_TEXT.
        lo_column->set_currency_column('WAERS') ##NO_TEXT.
        lo_column ?= lo_columns->get_column( 'CELLTYPE' ).
        lo_column->set_technical( ).
        LOOP AT tech_fields ASSIGNING FIELD-SYMBOL(<field>).
          lo_column ?= lo_columns->get_column( <field> ).
          lo_column->set_technical( ). "technical
        ENDLOOP.
        DATA(asd) = lo_column->get_cell_type( ).
      CATCH cx_salv_not_found INTO DATA(err).
      CATCH cx_salv_data_error INTO DATA(err_data).
    ENDTRY.

  ENDMETHOD.

  METHOD set_text.
    DATA lo_column  TYPE REF TO cl_salv_column_table.
    TRY.
        lo_column ?= lo_columns->get_column( name ).
        IF s_text IS NOT INITIAL.
          lo_column->set_short_text( s_text ).
        ENDIF.
        IF m_text IS NOT INITIAL.
          lo_column->set_medium_text( m_text ).
        ENDIF.
        IF l_text IS NOT INITIAL.
          lo_column->set_long_text( l_text ).
        ENDIF.
      CATCH cx_salv_not_found INTO DATA(err).
    ENDTRY.
  ENDMETHOD.

  METHOD set_color.
    DATA lo_column  TYPE REF TO cl_salv_column_table.
    TRY.
        DATA ls_col TYPE lvc_s_colo .
        lo_column ?= lo_columns->get_column( name ).
        lo_column->set_color( value = VALUE lvc_s_colo( col = col int = int inv = inv ) ).
      CATCH cx_salv_not_found INTO DATA(err).
    ENDTRY.

  ENDMETHOD.

  METHOD on_link_click.
    READ TABLE t_data ASSIGNING FIELD-SYMBOL(<line>) INDEX row.
    IF column = 'LBLNI'.
      SET PARAMETER ID 'LBL' FIELD <line>-lblni .
      CALL TRANSACTION 'ML81N' AND SKIP FIRST SCREEN.
    ELSEIF column = 'CHECKBOX'.
      IF <line>-checkbox IS INITIAL.
        <line>-checkbox = 'X'.
      ELSE.
        CLEAR <line>-checkbox.
      ENDIF.
      o_alv->refresh( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_user_command.
    DATA: lt_rows TYPE salv_t_row,
          lt_cols TYPE salv_t_column,
          ls_cell TYPE salv_s_cell.
    DATA: lr_selections TYPE REF TO cl_salv_selections.

    CASE e_salv_function.
      WHEN 'ALL'.
      WHEN 'SHOW_SEL'."show selection
        "display selections
        lr_selections = o_alv->get_selections( ).
        lt_rows = lr_selections->get_selected_rows( ).
        lt_cols = lr_selections->get_selected_columns( ).
        ls_cell = lr_selections->get_current_cell( ).

      WHEN 'SET_ROWS'. "set selected rows

        APPEND 4 TO lt_rows.
        APPEND 5 TO lt_rows.
        APPEND 6 TO lt_rows.
        lr_selections = o_alv->get_selections( ).
        lr_selections->set_selected_rows( lt_rows ).

*      WHEN 'SET_COLS'. "set selected cols
*
*        APPEND 'PRICE' TO lt_cols.
*        lr_selections = o_alv->get_selections( ).
*        lr_selections->set_selected_columns( lt_cols ).
*
*      WHEN 'SET_CELL'." set selected cell
*
*        ls_cell-columnname = 'PLANETYPE'.
*        ls_cell-row        = 4.
*        lr_selections = o_alv->get_selections( ).
*        lr_selections->set_current_cell( ls_cell ).

      WHEN 'SUBTOTAL'.
        DATA: lo_aggregations TYPE REF TO cl_salv_aggregations.
        DATA: lo_groups TYPE REF TO cl_salv_sorts .

        lo_aggregations = o_alv->get_aggregations( ).
        lo_aggregations->clear( ).
        lo_groups = o_alv->get_sorts( ) .
        lo_groups->clear( ).

        TRY.
            lo_groups->add_sort(
               columnname = 'EBELN'
*               position   = 1
               subtotal   = abap_true
               sequence   = if_salv_c_sort=>sort_up ).

          CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
        ENDTRY.
        o_alv->refresh(
          EXPORTING
            refresh_mode = if_salv_c_refresh=>full    " ALV: Data Element for Constants
        ).
      WHEN 'MYFUNCTION'.
        clean_final_on_hes( ).
    ENDCASE.
  ENDMETHOD.

  METHOD create_top_of_page.
    DATA: lo_top_element  TYPE REF TO cl_salv_form_layout_grid,
          lo_end_element  TYPE REF TO cl_salv_form_layout_flow,
          lo_grid         TYPE REF TO cl_salv_form_layout_grid,
          lo_header       TYPE REF TO cl_salv_form_header_info,
          lo_action       TYPE REF TO cl_salv_form_action_info,
          lo_textview1    TYPE REF TO cl_salv_form_text,
          lo_textview2    TYPE REF TO cl_salv_form_text,
          lo_textview3    TYPE REF TO cl_salv_form_text,
          lo_textview4    TYPE REF TO cl_salv_form_text,
          lo_icon         TYPE REF TO cl_salv_form_icon,
          lo_layout_grid1 TYPE REF TO cl_salv_form_layout_data_grid,
          lo_layout_grid2 TYPE REF TO cl_salv_form_layout_data_grid,
          lo_layout_grid3 TYPE REF TO cl_salv_form_layout_data_grid,
          lo_layout_grid4 TYPE REF TO cl_salv_form_layout_data_grid,
          lo_logo         TYPE REF TO cl_salv_form_layout_logo.

    CREATE OBJECT lo_top_element
      EXPORTING
        columns = 2.

    lo_top_element->create_header_information(
        row = 1
        column = 1
        text     = 'Reporte Entrada de servicios'
        tooltip  = 'Reporte Entrada de servicios' ).
    lo_grid = lo_top_element->create_grid(
              row     = 3
              column  = 1 ).
*
    lo_textview1 = lo_grid->create_text(
      row     = 1
      column  = 1
      text    = ' '                                         "#EC NOTEXT
      tooltip = ' ' ).                                      "#EC NOTEXT
*

    "Set alignment
    lo_layout_grid1 ?= lo_textview1->get_layout_data( ).
    lo_layout_grid1->set_h_align( if_salv_form_c_h_align=>left ).

    "insert icon
    CREATE OBJECT lo_icon
      EXPORTING
        icon    = '@0A@'     "#EC NOTEXT
        tooltip = 'Air'.     "#EC NOTEXT

    "logosuz
    o_alv->set_top_of_list( value = lo_top_element ).


    "*-----------------FOOTER-------------------------*
    DATA: lr_eol TYPE REF TO cl_salv_form_header_info.
    DATA  lv_text TYPE string.

    CREATE OBJECT lr_eol
      EXPORTING
        text = lv_text.     "#EC NOTEXT

    o_alv->set_end_of_list( lr_eol ).

  ENDMETHOD.


  METHOD  clean_final_on_hes.
    DATA has_modifications TYPE abap_bool.
    CLEAR has_modifications.
    IF t_data IS INITIAL.
      MESSAGE 'No existen registros de acuerdo con los parámetros Selecionados' TYPE 'I' DISPLAY LIKE 'S'.
    ELSE.

      SELECT
      essr~*,
      ekko~*,
      ekpo~*
      FROM essr
      INNER JOIN ekko ON ekko~ebeln EQ essr~ebeln
      INNER JOIN ekpo ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        ON ekpo~ebeln EQ essr~ebeln
       AND ekpo~ebelp EQ essr~ebelp
       FOR ALL ENTRIES IN @t_data
     WHERE essr~lblni =  @t_data-lblni AND
           essr~erdat =  @t_data-erdat
      INTO TABLE @DATA(essr_rel_tab).

      LOOP AT t_data INTO DATA(ls_data) WHERE checkbox = abap_true.
        has_modifications = abap_true.
        DATA(essr_rel_) = essr_rel_tab[ 1 ].
        DATA(yessr) = essr_rel_-essr.
        DATA imkpf TYPE imkpf.
        yessr-final = abap_false.
        CALL FUNCTION 'MS_DATA_ENTRY_SHEET'
          EXPORTING
            i_ekko       = essr_rel_-ekko
            i_ekpo       = essr_rel_-ekpo
            i_essr_old   = essr_rel_-essr
            i_essr_new   = yessr
            i_imkpf      = imkpf
          EXCEPTIONS
            fatal_error  = 1
            status_wrong = 2
            OTHERS       = 3.
        IF sy-subrc IS NOT INITIAL.

        ENDIF.
        CALL FUNCTION 'MS_SAVE_SERVICE_ENTRY'
          EXCEPTIONS
            OTHERS = 0.
        COMMIT WORK.
      ENDLOOP.
      IF has_modifications = abap_true.
        MESSAGE 'Se han modificado registros' TYPE 'I' DISPLAY LIKE 'S'.
        DELETE t_data WHERE checkbox = abap_true.
        o_alv->refresh( ).
      ELSE.
        MESSAGE 'Sin modificaciones' TYPE 'I' DISPLAY LIKE 'S'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_configuration.
    DATA wa_set41  LIKE LINE OF rg_set41_tab.
    SELECT *  FROM setleaf INTO TABLE @DATA(itab_set) WHERE setname =  'ZCLMM_41'.
    LOOP AT itab_set INTO DATA(wa_set01).
      wa_set41-sign   = wa_set01-valsign.
      wa_set41-option = wa_set01-valoption.
      wa_set41-low    = wa_set01-valfrom.
      wa_set41-high   = wa_set01-valto.
      APPEND wa_set41 TO rg_set41_tab.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_salv) = NEW lcl_salv( ).
  lo_salv->init( ).

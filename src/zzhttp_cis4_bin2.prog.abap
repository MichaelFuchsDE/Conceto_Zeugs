*&---------------------------------------------------------------------*
*& Report  ZZHTTP_CIS4
*&
*&---------------------------------------------------------------------*
*& 2022 CONCETO Michael Fuchs
*& Senden von TEXT Dateien an CIS/4 ueber HTTP
*& Beispiel Internet:
*& https://help.sap.com/saphelp_autoid70/helpdata/de/
*&        1f/93163f9959a808e10000000a114084/content.htm?no_cache=true
*&---------------------------------------------------------------------*

REPORT  zzhttp_cis4.

TYPES: ext_text(45) TYPE c.

*SELECTION-SCREEN BEGIN OF BLOCK eins WITH FRAME TITLE TEXT-s01.
*PARAMETERS: p_cis4 TYPE string DEFAULT 'http://cis4.it-cpi001-rt.cfapps.eu10.hana.ondemand.com:443'.
*PARAMETERS: p_pfad TYPE string DEFAULT '/cxf/pro/sundwiger/suedfactoring'.
*PARAMETERS: p_user TYPE string DEFAULT 'g-sundwiger-erp'.
*PARAMETERS: p_pass TYPE string DEFAULT 'Welcome2'.
*SELECTION-SCREEN END OF BLOCK eins.

*** HOST ist RFC Destination Typ 'G'!
PARAMETERS:   p_host TYPE rfcdest OBLIGATORY.
* Datenablage
PARAMETERS:  p_verzl  TYPE salfile-longname  LOWER CASE OBLIGATORY.
* Dateimaske
PARAMETERS:  p_fmask TYPE string OBLIGATORY.
* Datei Typ
PARAMETERS:  p_ftype(3) TYPE c DEFAULT 'CSV' OBLIGATORY.

SELECTION-SCREEN ULINE.
PARAMETERS: p_backup TYPE string LOWER CASE DEFAULT 'backup'.

SELECTION-SCREEN ULINE.
PARAMETERS: p_loesch AS CHECKBOX DEFAULT space.
PARAMETERS: p_eine   AS CHECKBOX DEFAULT 'X'.

**************************************************************************************

DATA: lt_dir TYPE TABLE OF  salfldir WITH HEADER LINE.

AT SELECTION-SCREEN OUTPUT.
  %_p_host_%_app_%-text = 'RFC HTTP Destination'.
  %_p_verzl_%_app_%-text = 'Quell-Pfad'.
  %_p_fmask_%_app_%-text = 'Quell-Dateimaske'.
  %_p_ftype_%_app_%-text = 'Filetype fuer HTTP Host'.
  %_p_backup_%_app_%-text = 'Backup Unterverzeichnis'.
  %_p_loesch_%_app_%-text = 'Quell-Dateien loeschen'.
  %_p_eine_%_app_%-text = 'nur 1 Datei uebertragen'.

INITIALIZATION.
  IF sy-sysid EQ 'NPL'.
    p_host  = 'MEINPC'.
    p_verzl = '/usr/sap/tmp'.
    p_fmask = '*.log'.
  ENDIF.

START-OF-SELECTION.
  CALL FUNCTION 'RZL_READ_DIR_LOCAL'
    EXPORTING
      name               = p_verzl
*     FROMLINE           = 0
*     NRLINES            = 1000
    TABLES
      file_tbl           = lt_dir
    EXCEPTIONS
      argument_error     = 1
      not_found          = 2
      no_admin_authority = 3
      OTHERS             = 4
    . "*Punkt
  IF sy-subrc = 0.
* Implement suitable error handling here
    LOOP AT lt_dir.
      AT FIRST.
        WRITE: / '*** Pfad:', (60) '"' && p_verzl && '"'.
      ENDAT.
      CHECK: lt_dir-name CP p_fmask AND lt_dir-size GT 0.
      WRITE: / '    ... Datei: "' && lt_dir-name && '"', 'Groesse: "' && lt_dir-size && '"'.
      PERFORM do_http_post USING p_verzl lt_dir-name.
      IF NOT p_eine IS INITIAL. EXIT. ENDIF.
    ENDLOOP.
  ENDIF.

END-OF-SELECTION.

FORM do_http_post USING filepath TYPE c filename TYPE c.

  DATA: lv_file TYPE string.
  DATA: lv_work TYPE string.
  DATA: it_data TYPE xstring VALUE ''.
  DATA: lv_s TYPE string.
  DATA: lv_n TYPE i VALUE 0.
  DATA: lv_subrc LIKE sy-subrc.
  DATA: lt_tab TYPE string OCCURS 0.

  IF filepath CA '/'.
    lv_file = filepath && '/' && filename.
    REPLACE ALL OCCURRENCES OF '//' IN lv_file WITH '/' IN CHARACTER MODE.
*    lv_file = '/' && lv_file.
    lv_work = filepath && '/' && p_backup && '/'.
    REPLACE ALL OCCURRENCES OF '//' IN lv_work WITH '/' IN CHARACTER MODE.
    lv_work = '/' && lv_work.
  ELSEIF filepath CA '\'.
    lv_file = filepath && '\' && filename.
    REPLACE ALL OCCURRENCES OF '\\' IN lv_file WITH '\' IN CHARACTER MODE.
*    lv_file = '\' && lv_file.
    lv_work = filepath && '\' && p_backup && '\'.
    REPLACE ALL OCCURRENCES OF '\\' IN lv_work WITH '\' IN CHARACTER MODE.
    lv_work = '\' && lv_work.
  ENDIF.
  PERFORM backup_dir USING lv_work.
  lv_work = lv_work && filename && '.' && sy-datum(8) && sy-uzeit(6).

  CONDENSE: lv_file, lv_work.

*  Datei in XSTRING lesen, alles aufeinmal
  OPEN DATASET lv_file FOR INPUT IN BINARY MODE.
  READ DATASET lv_file INTO it_data.
  CLOSE DATASET lv_file.

* Datei sichern in ~/work
  lv_subrc = 0.
  TRY.
      OPEN DATASET lv_work FOR OUTPUT IN BINARY MODE.
      TRANSFER it_data TO lv_work.
      CLOSE DATASET lv_work.
    CATCH cx_sy_file_close cx_sy_file_io cx_sy_file_open_mode.
      WRITE: / '        BACKUP ERROR fuer: "' && lv_work && '"'.
      lv_subrc = 4.
  ENDTRY.
  IF lv_subrc EQ 0.
    WRITE: / '        BACKUP SUCCESS fuer: "' && lv_work && '"'.
  ENDIF.

  CALL METHOD cl_http_client=>create_by_destination
    EXPORTING
      destination        = p_host
    IMPORTING
      client             = DATA(client)
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    WRITE: / 'Create failed, subrc = ', sy-subrc.
    EXIT.
  ENDIF.

*set http method post
  client->request->set_method(  if_http_request=>co_request_method_post ).
*SET protocol version
  client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

  CALL METHOD client->request->set_content_type
    EXPORTING
      content_type = 'application/octet-stream'. "/text
*    content_type = 'application/x-zip-compressed'.

  CALL METHOD client->request->set_header_field
    EXPORTING
      name  = 'filepath'
      value = '"' && filepath && '"'.

  CALL METHOD client->request->set_header_field
    EXPORTING
      name  = 'filetype'
      value = '"' && p_ftype && '"'.

  CALL METHOD client->request->set_header_field
    EXPORTING
      name  = 'filename'
      value = '"' && filename && '"'.

  DATA(len) = xstrlen( it_data ).

  CALL METHOD client->request->set_data
    EXPORTING
      data   = it_data
      offset = 0
      length = len.

  CALL METHOD client->send
    EXPORTING
      timeout                    = 10
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.
*  lv_subrc = sy-subrc.
  WRITE: / '        SUBRC(send) = "' && sy-subrc && '"', 'fuer "' && lv_file && ' "(' && len && ')"'.
  IF sy-subrc EQ 0.
*    IF NOT p_loesch IS INITIAL.
*      DELETE DATASET lv_file.
*      IF sy-subrc EQ 0.
*        WRITE: / '        geloescht: "' && lv_file && '"'.
*      ELSE.
*        WRITE: / '        Fehler:    "' && lv_file && '" nicht geloescht!'.
*      ENDIF.
*    ENDIF.
  ELSE.
    CALL METHOD client->get_last_error
      IMPORTING
        code    = DATA(subrc2)
        message = DATA(errortext2).
    WRITE: / 'communication_error( receive )',
           / 'code2: ', subrc2, 'message: ', errortext2.
    EXIT.
  ENDIF.

* receive
  CALL METHOD client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.
  IF sy-subrc <> 0.
    CALL METHOD client->get_last_error
      IMPORTING
        code    = DATA(subrc)
        message = DATA(errortext).
    WRITE: / '        communication_error( receive ) = "' && subrc && '"' ,
           / '        message: ', errortext.
    EXIT.
  ELSE.
    WRITE: / '        SUBRC(receive) = "' && sy-subrc && '"', 'fuer "' && lv_file && ' "(' && len && ')"'.
    IF NOT p_loesch IS INITIAL.
      DELETE DATASET lv_file.
      IF sy-subrc EQ 0.
        WRITE: / '        geloescht: "' && lv_file && '"'.
      ELSE.
        WRITE: / '        Fehler:    "' && lv_file && '" nicht geloescht!'.
      ENDIF.
    ENDIF.
  ENDIF.

* close
  CALL METHOD client->close
    EXCEPTIONS
      http_invalid_state = 1
      OTHERS             = 2.

ENDFORM.

FORM backup_dir USING p_dir TYPE string.
  DATA: l_com TYPE rlgrap-filename.
  DATA: xdir TYPE btch0000-text80.
  xdir = p_dir.
  CALL FUNCTION 'PFL_CHECK_DIRECTORY'
    EXPORTING
      directory         = xdir
    EXCEPTIONS
      pfl_dir_not_exist = 1.
  IF sy-subrc = 1.
    CONCATENATE 'mkdir' xdir INTO l_com SEPARATED BY space.
    CALL 'SYSTEM' ID 'COMMAND' FIELD l_com.
  ENDIF.
ENDFORM.

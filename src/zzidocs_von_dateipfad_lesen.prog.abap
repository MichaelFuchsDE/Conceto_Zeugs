REPORT zzidocs_von_dateipfad_lesen.
*&---------------------------------------------------------------------*
*& 2023 CONCETO Michael Fuchs
*&---------------------------------------------------------------------*

TYPES: ext_text(45) TYPE c.


* Datenablage
PARAMETERS:  p_verzl  TYPE salfile-longname  LOWER CASE OBLIGATORY DEFAULT '/usr/sap/tmp/xml/in/'.
*Datei-Port
PARAMETERS:  p_iport TYPE ediport-port OBLIGATORY DEFAULT 'IDOC1'.
PARAMETERS:  p_xport TYPE ediport-port OBLIGATORY DEFAULT 'XML1'.

**************************************************************************************

DATA: lt_dir TYPE TABLE OF  salfldir WITH HEADER LINE.
DATA: fname LIKE  edi_path-pthnam.
DATA: anfang(10) TYPE c.

INITIALIZATION.

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
    DELETE lt_dir WHERE name = '.' OR name = '..'.
* Implement suitable error handling here
    LOOP AT lt_dir.
      AT FIRST.
        WRITE: / '*** Pfad:', (60) '"' && p_verzl && '"'.
      ENDAT.
*      CHECK: lt_dir-name CP p_fmask AND lt_dir-size GT 0.
      WRITE: / '    ... Datei: "' && lt_dir-name && '"', 'Groesse: "' && lt_dir-size && '"'.
*      IF NOT p_eine IS INITIAL. EXIT. ENDIF.
    ENDLOOP.
  ENDIF.

END-OF-SELECTION.
  LOOP AT lt_dir.
    AT FIRST.
      WRITE: /.
      WRITE: / '********************'.
      WRITE: /.
    ENDAT.
    fname = p_verzl && '/' && lt_dir-name.
    REPLACE '//' WITH '/' INTO fname.
    OPEN DATASET fname FOR INPUT IN TEXT MODE ENCODING UTF-8.
    READ DATASET fname INTO anfang.
    CLOSE DATASET fname.
    IF anfang CS '<?xml '.
      WRITE: / '    ... XML     Filetype: "' && fname && '"'.
      CALL FUNCTION 'IDOC_XML_FROM_FILE'
        EXPORTING
          file_name            = fname
          port                 = p_xport
        EXCEPTIONS
          file_open_failed     = 1
          read_file_failed     = 2
          file_delete_failed   = 3
          event_create_failed  = 4
          segment_error        = 5
          tag_error            = 6
          control_record_error = 7
          idoc_not_stored      = 8
          marker_to_be_deleted = 9
          marker_modify_failed = 10
          OTHERS               = 11
        . "*Punkt
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ELSEIF anfang CS 'EDI_DC'.
      WRITE: / '    ... IDOC    Filetype: "' && fname && '"'.
      CALL FUNCTION 'IDOC_INBOUND_FROM_FILE'
        EXPORTING
          file_name            = fname
          port                 = p_iport
        EXCEPTIONS
          file_open_failed     = 1
          marker_to_be_deleted = 2
          read_file_failed     = 3
          idoc_not_stored      = 4
          file_delete_failed   = 5
          marker_modify_failed = 6
          event_create_failed  = 7
          first_record_invalid = 8
          invalid_record       = 9
          OTHERS               = 10
        . "*Punkt
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ELSE.
      WRITE: / '    ... UNKNOWN Filetype: "' && fname && '"'.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& Report zma_dp_strategy
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zma_dp_strategy.

CLASS lcx_invalid_operator DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

"The basic idea of the strategy pattern is to separate the distinction, which type of logic to apply from the actual logic being applied.
" In other words: To separate the CASE from the Method/Function calls underneath the WHEN-branches.
"The strategy pattern consists of three parts:
" 1. Something, that formalizes our signature, i.e. the parameters that are supplied to & retrieved from the procedures --> In this example: lcl_strategy_factory
" 2. Something, that determines, which logic should be applied --> In this example: lcl_strategy_factory=>get_strategy
" 3. The actual implementation of the various procedures --> In this example: the addition, subtraction, multiplication & division classes

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 1: That's the signature for our procedures. ONE FOR ALL!!!!
INTERFACE lif_strategy. "Formalizes the Parameters
  METHODS return_result IMPORTING ip_value1        TYPE i
                                  ip_value2        TYPE i
                        RETURNING VALUE(rv_result) TYPE char30
                        RAISING   cx_sy_zerodivide.
ENDINTERFACE.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 2: Which logic is supposed to be used?
CLASS lcl_strategy_factory DEFINITION. "Chooses the correct "variant" out of several strategies
  PUBLIC SECTION.
    CLASS-METHODS get_strategy IMPORTING ip_operation         TYPE char1
                               RETURNING VALUE(rref_strategy) TYPE REF TO lif_strategy "returns an actual arithm. operation class
                               RAISING   lcx_invalid_operator.
ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 3: The flavors of the implementation
CLASS lcl_addition DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_strategy.
    ALIASES return_result FOR lif_strategy~return_result.
ENDCLASS.

CLASS lcl_subtraction DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_strategy.
    ALIASES return_result FOR lif_strategy~return_result.
ENDCLASS.

CLASS lcl_multiplication DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_strategy.
    ALIASES return_result FOR lif_strategy~return_result.
ENDCLASS.

CLASS lcl_division DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_strategy.
    ALIASES return_result FOR lif_strategy~return_result.
ENDCLASS.

CLASS lcl_strategy_factory IMPLEMENTATION.
  METHOD get_strategy.
    CASE ip_operation.
      WHEN '+'.
        rref_strategy = NEW lcl_addition( ).
      WHEN '-'.
        rref_strategy = NEW lcl_subtraction( ).
      WHEN '*'.
        rref_strategy = NEW lcl_multiplication( ).
      WHEN '/'.
        rref_strategy = NEW lcl_division( ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE lcx_invalid_operator.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_addition IMPLEMENTATION.
  METHOD return_result.
    rv_result = ip_value1 + ip_value2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_subtraction IMPLEMENTATION.
  METHOD return_result.
    rv_result = ip_value1 - ip_value2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_multiplication IMPLEMENTATION.
  METHOD return_result.
    rv_result = ip_value1 * ip_value2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_division IMPLEMENTATION.
  METHOD return_result.
    rv_result = ip_value1 / ip_value2.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

PARAMETERS p_value1 TYPE i.
PARAMETERS p_value2 TYPE i.
PARAMETERS p_oper(1) TYPE c.

START-OF-SELECTION.
  TRY.
      DATA(lr_strategy) = lcl_strategy_factory=>get_strategy( p_oper ).
      DATA(ld_result) = lr_strategy->return_result( ip_value1 = p_value1
                                                    ip_value2 = p_value2 ).

      cl_demo_output=>new( )->begin_section( 'Result'
                           )->write_text( |{ p_value1 } { p_oper } { p_value2 } is { ld_result }|
                           )->display( ).
    CATCH lcx_invalid_operator.
      cl_demo_output=>new( )->begin_section( 'Nope'
                           )->write_text( |You better choose on of these operators: +, -, *, /|
                           )->display( ).
    CATCH cx_sy_zerodivide.
      cl_demo_output=>new( )->begin_section( 'Nope'
                         )->write_text( |Thou shalt not divide by zero!!!!!!1111|
                         )->display( ).
  ENDTRY.

//----------------------------------------------------------------------------------------------------//
// MIT License
//
// Copyright (c) 2019 Alexander Gamper, All Rights Reserved.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//----------------------------------------------------------------------------------------------------//

grammar Ada;

options { 
} 

//----------------------------------------------------------------------------------------------------//
//Parser

//2.8

pragma				: PRAGMA IDENTIFIER ('(' pragma_argument_association (',' pragma_argument_association)* ')' )? ';' ;

pragma_argument_association
                    : (pragma_argument_identifier '=>')? (name | expression)
                    | pragma_argument_aspect_mark '=>' (name | expression)
                    ;

pragma_argument_identifier
                    : IDENTIFIER
                    ;

pragma_argument_aspect_mark
                    : IDENTIFIER ('\'' IDENTIFIER)?
                    ;

//2.9 reserved words

//3.1
basic_declaration	:
    type_declaration
    | subtype_declaration
    | object_declaration
    | number_declaration
    | subprogram_declaration
    | abstract_subprogram_declaration
    | null_procedure_declaration
    | expression_function_declaration
    | package_declaration
    | renaming_declaration
    | exception_declaration
    | generic_declaration
    | generic_instantiation ;

defining_identifier	: IDENTIFIER ;

//3.2.1
type_declaration	: full_type_declaration
    | incomplete_type_declaration
    | private_type_declaration
    | private_extension_declaration
    ;

full_type_declaration	:
    TYPE IDENTIFIER (known_discriminant_part)? IS type_definition (aspect_specification)? ';'
    | task_type_declaration
    | protected_type_declaration
    ;

type_definition		:
    enumeration_type_definition
    | integer_type_definition
    | real_type_definition
    | array_type_definition
    | record_type_definition
    | access_type_definition
    | derived_type_definition
    | interface_type_definition
    ;

//3.2.2
subtype_declaration		:	SUBTYPE IDENTIFIER IS subtype_indication (aspect_specification)? ';' ;

subtype_indication		:	(null_exclusion)? subtype_mark (constraint)? ;

subtype_mark			:	full_name ;

constraint				:	scalar_constraint | composite_constraint ;

scalar_constraint		:	range_constraint | digits_constraint | delta_constraint ;

composite_constraint	:	index_constraint | discriminant_constraint ;

//3.3.1
object_declaration		:	defining_identifier_list COLON (ALIASED)? (CONSTANT)? subtype_indication (':=' expression)? (aspect_specification)? ';'
    | defining_identifier_list COLON (ALIASED)? (CONSTANT)? access_definition (':=' expression)? (aspect_specification)? ';'
    | defining_identifier_list COLON (ALIASED)? (CONSTANT)? array_type_definition (':=' expression)? (aspect_specification)? ';'
    | single_task_declaration
    | single_protected_declaration
    ;

defining_identifier_list		:	IDENTIFIER (',' IDENTIFIER)* ;

//3.3.2
number_declaration				:	defining_identifier_list COLON CONSTANT ':=' expression ';' ;

//3.4:
derived_type_definition			:	(ABSTRACT)? (LIMITED)? NEW subtype_indication ((AND interface_list)? record_extension_part)? ;

//3.5:
range_constraint				:	RANGE range ;

range							:	range_attribute_reference
    | simple_expression '..' simple_expression
    ;

enumeration_type_definition		:	'(' enumeration_literal_specification (',' enumeration_literal_specification)* ')' ;

enumeration_literal_specification	:	IDENTIFIER | defining_character_literal ;

defining_character_literal		:	CHARACTER_LITERAL ;

integer_type_definition			:	signed_integer_type_definition | modular_type_definition ;

signed_integer_type_definition	:	RANGE simple_expression '..' simple_expression ;

modular_type_definition			:	MOD expression ;

real_type_definition			:	floating_point_definition | fixed_point_definition ;

floating_point_definition		:	DIGITS expression (real_range_specification)? ;

real_range_specification		:	RANGE simple_expression '..' simple_expression ;

fixed_point_definition			:	ordinary_fixed_point_definition | decimal_fixed_point_definition ;

ordinary_fixed_point_definition	:	DELTA expression real_range_specification ;

decimal_fixed_point_definition	:	DELTA expression DIGITS expression (real_range_specification)? ;

digits_constraint				:	DIGITS simple_expression (range_constraint)? ;

//3.6
array_type_definition			:	unconstrained_array_definition | constrained_array_definition ;

unconstrained_array_definition	:	ARRAY '(' index_subtype_definition (',' index_subtype_definition)* ')' OF component_definition ;

index_subtype_definition		:	subtype_mark RANGE '<>' ;

constrained_array_definition	:	ARRAY '(' discrete_subtype_definition (',' discrete_subtype_definition)* ')' OF component_definition ;

discrete_subtype_definition		:	subtype_indication | range ;

component_definition			:	(ALIASED)? subtype_indication 
    | (ALIASED)? access_definition
    ;

index_constraint				:	'(' discrete_range (',' discrete_range)* ')' ;

discrete_range					:	subtype_indication | range ;

//3.7
discriminant_part				:	unknown_discriminant_part | known_discriminant_part ;

unknown_discriminant_part		:	'(' '<>' ')' ;

known_discriminant_part			:	'(' discriminant_specification (';' discriminant_specification)* ')' ;

discriminant_specification		:	defining_identifier_list COLON (null_exclusion)? subtype_mark (':=' default_expression)?
    | defining_identifier_list COLON access_definition (':=' default_expression)?
    ;

default_expression				:	expression ;

discriminant_constraint			:	'(' discriminant_association (',' discriminant_association)* ')' ;

discriminant_association		:	(selector_name ('|' selector_name)* '=>')? expression ;

//3.8
record_type_definition			:	((ABSTRACT)? TAGGED)? (LIMITED)? record_definition ;

record_definition				:	RECORD component_list END RECORD
    | NULL RECORD
    ;

component_list					:	component_item (component_item)*
    | (component_item)* variant_part
    | NULL ';'
    ;

component_item					:	pragma | component_declaration | aspect_clause ;

component_declaration			:	defining_identifier_list COLON component_definition (':=' default_expression)? (aspect_specification)? ';' ;

variant_part					:	CASE direct_name IS variant (variant)* END CASE ';' ;

variant							:	WHEN discrete_choice_list '=>' component_list ;

discrete_choice_list			:	discrete_choice ('|' discrete_choice)* ;

discrete_choice					:	choice_expression | subtype_indication | range | OTHERS ;

//3.9
record_extension_part			:	WITH record_definition ;

abstract_subprogram_declaration	:	(overriding_indicator)? subprogram_specification IS ABSTRACT (aspect_specification)? ';' ;

interface_type_definition		:	(LIMITED | TASK | PROTECTED | SYNCHRONIZED)? INTERFACE (AND interface_list)? ;

interface_list					:	subtype_mark (AND subtype_mark)* ;

//3.10
access_type_definition			:	(null_exclusion)? access_to_object_definition
    | (null_exclusion)? access_to_subprogram_definition
    ;

access_to_object_definition		:	ACCESS (general_access_modifier)? subtype_indication ;

general_access_modifier			:	ALL | CONSTANT ;

access_to_subprogram_definition	:	ACCESS (PROTECTED)? PROCEDURE parameter_profile
    | ACCESS (PROTECTED)? FUNCTION parameter_and_result_profile
    ;

null_exclusion					:	NOT NULL ;

access_definition				:	(null_exclusion)? ACCESS (CONSTANT)? subtype_mark
    | (null_exclusion)? ACCESS (PROTECTED)? PROCEDURE parameter_profile
    | (null_exclusion)? ACCESS (PROTECTED)? FUNCTION parameter_and_result_profile
    ;

incomplete_type_declaration		:	TYPE IDENTIFIER (discriminant_part)? (IS TAGGED)? ';' ;

//3.11
declarative_part				:	(declarative_item)* ;

declarative_item				:	basic_declarative_item | body ;

basic_declarative_item			:	basic_declaration | aspect_clause | use_clause | pragma;

body							:	proper_body | body_stub ;

proper_body						:	subprogram_body | package_body | task_body | protected_body ;

//4.1
package_name    :   IDENTIFIER ( DOT IDENTIFIER)*;

full_name       :   IDENTIFIER ( DOT ( IDENTIFIER | CHARACTER_LITERAL | STRING_LITERAL ) | TIC IDENTIFIER )*	 
                ; 

name    :   direct_name | explicit_dereference
        |   indexed_component | slice
        |   selected_component | attribute_reference
        |   type_conversion
        |   function_call
        |   CHARACTER_LITERAL
        |   qualified_expression
        |   generalized_reference | generalized_indexing
        ;

direct_name						:	IDENTIFIER | operator_symbol (TIC IDENTIFIER)?;

prefix							:	full_name ;

explicit_dereference			:	full_name DOT ALL;

implicit_dereference			:	full_name ;

indexed_component				:	full_name '(' expression (',' expression)* ')' ;

slice							:	full_name '(' discrete_range ')' ;

selected_component				:	full_name DOT selector_name ;

selector_name					:	IDENTIFIER | CHARACTER_LITERAL | operator_symbol ;

attribute_reference				:	full_name TIC attribute_designator ;

attribute_designator			:	IDENTIFIER ( '(' expression ')' )? | ACCESS | DELTA | DIGITS | MOD ( '(' expression ')' )?;

range_attribute_reference		:	prefix TIC range_attribute_designator ;

range_attribute_designator		:	RANGE ('(' expression ')' )? ;

generalized_reference			:	full_name ;

generalized_indexing			:	prefix actual_parameter_part ;

//4.2 literals

//4.3
aggregate						:	record_aggregate | extension_aggregate | array_aggregate ;

record_aggregate				:	'(' record_component_association_list ')' ;

record_component_association_list	:	record_component_association (',' record_component_association)*
    | NULL RECORD
    ;

record_component_association	:	(component_choice_list '=>')? expression
    | component_choice_list '=>' '<>'
    ;

component_choice_list			:	selector_name ('|' selector_name)*
    | OTHERS
    ;

extension_aggregate				:	'(' ancestor_part WITH record_component_association_list ')' ;

ancestor_part					:	expression | subtype_mark ;

array_aggregate					:	positional_array_aggregate | named_array_aggregate ;

positional_array_aggregate		:	'(' expression ',' expression (',' expression)* ')'
    | '(' expression (',' expression)* ',' OTHERS '=>' expression ')'
    | '(' expression (',' expression)* ',' OTHERS '=>' '<>' ')'
    ;

named_array_aggregate			:	'(' array_component_association (',' array_component_association)* ')' ;

array_component_association		:	discrete_choice_list '=>' expression
    | discrete_choice_list '=>' '<>'
    ;

//4.4
expression          :   relation (AND relation)*
                    |   relation (AND THEN relation)*
                    |   relation (OR relation)*
                    |   relation (OR ELSE relation)*
                    |   relation (XOR relation)*
                    ;

choice_expression	:	choice_relation (AND choice_relation)*
                    |   choice_relation (OR choice_relation)*
                    |   choice_relation (XOR choice_relation)*
                    |   choice_relation (AND THEN choice_relation)*
                    |   choice_relation (OR ELSE choice_relation)*
                    ;

choice_relation					:	simple_expression (relational_operator simple_expression)? ;

relation						:	simple_expression (relational_operator simple_expression)?
                                |   simple_expression (NOT)? IN membership_choice_list
                                |   raise_expression
                                ;

membership_choice_list			:	membership_choice ('|' membership_choice)* ;

membership_choice				:	simple_expression | range | subtype_mark ;

simple_expression				:	(unary_adding_operator)? term (binary_adding_operator term)* ;

term							:	factor (multiplying_operator factor)* ;

factor							:	ABS primary | NOT primary | primary (EXPON primary)?;

primary							:   name | allocator | '(' expression ')'
                                |   ('(')? conditional_expression (')')?
                                |   '(' quantified_expression ')'
                                |   aggregate
                                |   NUMERIC_LITERAL | NULL | STRING_LITERAL 
                                ;

//4.5
logical_operator				:	AND | OR | XOR ;

relational_operator				:	'=' | NEQ | '<' | '<=' | '>' | '>=' ;

binary_adding_operator			:	'+' | '-' | '&' ;

unary_adding_operator			:	'+' | '-' ;

multiplying_operator			:	'*' | '/' | MOD | REM ;

highest_precedence_operator		:	EXPON | ABS | NOT ;

conditional_expression			:	if_expression | case_expression ;

if_expression					:	IF condition THEN expression
                                    (ELSIF condition THEN expression)*
                                    (ELSE expression)?
                                ;

condition						:	expression ;

case_expression					:	CASE expression IS case_expression_alternative (',' case_expression_alternative)* ;
    
case_expression_alternative		:	WHEN discrete_choice_list '=>' expression ;

quantified_expression			:	FOR quantifier loop_parameter_specification '=>' predicate
                                |   FOR quantifier iterator_specification '=>' predicate
                                ;

quantifier						:	ALL | SOME ;

predicate						:	expression ;

//4.6:
type_conversion					:	subtype_mark'('expression')' | subtype_mark'('name')' ;

//4.7:
qualified_expression			:	subtype_mark TIC '('expression')' | subtype_mark TIC aggregate ;

//4.8:
allocator						:	NEW (subpool_specification)? subtype_indication
                                |   NEW (subpool_specification)? qualified_expression
                                ;

subpool_specification			:	'(' name ')' ;

//5.1
sequence_of_statements			:	statement (statement)* (label)* ;

statement						:	(label)* simple_statement | (label)* compound_statement ;

simple_statement		:	null_statement
    | assignment_statement
    | exit_statement
    | goto_statement
    | procedure_call_statement
    | simple_return_statement
    | entry_call_statement
    | requeue_statement
    | delay_statement
    | abort_statement
    | raise_statement
    | code_statement
    | pragma
    ;

compound_statement		:	if_statement
    | case_statement
    | loop_statement
    | block_statement
    | extended_return_statement
    | accept_statement
    | select_statement
    ;

null_statement				:	NULL ';' ;

assignment_statement		:	name ':=' expression ';' ;

exit_statement				:	EXIT (name)? (WHEN condition)? ';' ;

goto_statement				:	GOTO name ';' ;

procedure_call_statement	:	name ';'
    | prefix actual_parameter_part ';'
    ;


label						:	'<<' statement_identifier '>>' ;

statement_identifier		:	direct_name ;

if_statement	:
    IF condition THEN
    sequence_of_statements
    (ELSIF condition THEN sequence_of_statements)*
    (ELSE sequence_of_statements)?
    END IF ';'
    ;

case_statement	:
    CASE expression IS
    case_statement_alternative
    (case_statement_alternative)*
    END CASE ';'
    ;

case_statement_alternative	:	WHEN discrete_choice_list '=>' sequence_of_statements ;

loop_statement				: (statement_identifier COLON)?
    (iteration_scheme)? LOOP
    sequence_of_statements
    END LOOP (IDENTIFIER)? ';'
    ;

iteration_scheme			:	WHILE condition
    | FOR loop_parameter_specification
    | FOR iterator_specification
    ;

loop_parameter_specification	:	IDENTIFIER IN (REVERSE)? discrete_subtype_definition ;

iterator_specification			:	IDENTIFIER IN (REVERSE)? name
    | IDENTIFIER (COLON subtype_indication)? OF (REVERSE)? name
    ;

block_statement	:
    (statement_identifier COLON)?
    (DECLARE declarative_part)?
    BEGIN
    handled_sequence_of_statements
    END (IDENTIFIER)? ';'
    ;

//6.1
subprogram_declaration	:
    (overriding_indicator)?
    subprogram_specification
    (aspect_specification)? ';'
    ;

subprogram_specification	:	procedure_specification | function_specification ;

procedure_specification		:	PROCEDURE defining_program_unit_name parameter_profile ;

function_specification		:	FUNCTION defining_designator parameter_and_result_profile ;

designator					:	(parent_unit_name DOT )? IDENTIFIER | operator_symbol ;

defining_designator			:	defining_program_unit_name | defining_operator_symbol ;

defining_program_unit_name	:	(parent_unit_name DOT )? IDENTIFIER ;

operator_symbol				:	STRING_LITERAL ;

defining_operator_symbol	:	operator_symbol ;

parameter_profile			:	(formal_part)? ;

parameter_and_result_profile	:
    (formal_part)? RETURN (null_exclusion)? subtype_mark
    | (formal_part)? RETURN access_definition
    ;

formal_part					:	'(' parameter_specification (';' parameter_specification)* ')' ;

parameter_specification		:	defining_identifier_list COLON (ALIASED)? xmode (null_exclusion)? subtype_mark (':=' default_expression)?
    | defining_identifier_list COLON access_definition (':=' default_expression)?
    ;

xmode	:	 (IN)? | IN OUT | OUT ;

//6.3
subprogram_body	:
    (overriding_indicator)?
    subprogram_specification
    (aspect_specification)? IS
    declarative_part
    BEGIN
    handled_sequence_of_statements
    END (designator)? ';'
    ;

function_call	:	(direct_name | prefix) (actual_parameter_part)?
	            ;

actual_parameter_part               :	'(' parameter_association (',' parameter_association)* ')' ;

parameter_association               :	(selector_name '=>' )? explicit_actual_parameter ;

explicit_actual_parameter           :	expression | name ;

simple_return_statement             :	RETURN (expression)? ';' ;

extended_return_object_declaration	:	IDENTIFIER COLON (ALIASED)? (CONSTANT)? return_subtype_indication (':=' expression)? ;

extended_return_statement           :	RETURN extended_return_object_declaration (DO handled_sequence_of_statements END RETURN)? ';' ;

return_subtype_indication           :	subtype_indication | access_definition ;

null_procedure_declaration          :   (overriding_indicator)? procedure_specification IS NULL (aspect_specification)? ';' ;

expression_function_declaration     :   (overriding_indicator)? function_specification IS '(' expression ')' (aspect_specification)? ';' ;

//7.1
package_declaration		:	package_specification ';' ;

//package_specification	:	PACKAGE full_name	(aspect_specification)? IS
package_specification	:	PACKAGE defining_program_unit_name	(aspect_specification)? IS
    (basic_declarative_item)*
    (PRIVATE (basic_declarative_item)* )?
    END ((parent_unit_name DOT )? IDENTIFIER)?
    ;

//7.2
package_body	:	PACKAGE BODY defining_program_unit_name (aspect_specification)? IS
    declarative_part
    (BEGIN handled_sequence_of_statements)?
    END ((parent_unit_name DOT )? IDENTIFIER)? ';'
    ;

//7.3
private_type_declaration		:	TYPE IDENTIFIER (discriminant_part)? 
    IS ((ABSTRACT)? TAGGED)? (LIMITED)? PRIVATE (aspect_specification)? ';'
    ;

private_extension_declaration	:	TYPE IDENTIFIER (discriminant_part)?
    IS (ABSTRACT)? (LIMITED | SYNCHRONIZED)? NEW subtype_indication
    (AND interface_list)? WITH PRIVATE
    (aspect_specification)? ';'
    ;

//8.3.1
overriding_indicator	:	(NOT)? OVERRIDING ;

//8.4
use_clause				:	use_package_clause | use_type_clause ;

use_package_clause		:	USE package_name (',' package_name)* ';' ;

use_type_clause			:	USE (ALL)? TYPE subtype_mark (',' subtype_mark)* ';' ;

//8.5
renaming_declaration	:
    object_renaming_declaration
    | exception_renaming_declaration
    | package_renaming_declaration
    | subprogram_renaming_declaration
    | generic_renaming_declaration
    ;

object_renaming_declaration		:	IDENTIFIER COLON (null_exclusion)? subtype_mark RENAMES name (aspect_specification)? ';'
    | IDENTIFIER COLON access_definition RENAMES name (aspect_specification)? ';'
    ;

exception_renaming_declaration	:	IDENTIFIER COLON EXCEPTION RENAMES name (aspect_specification)? ';' ;

package_renaming_declaration	:	PACKAGE defining_program_unit_name RENAMES name (aspect_specification)? ';' ;

subprogram_renaming_declaration	:
    (overriding_indicator)?
    subprogram_specification RENAMES name
    (aspect_specification)? ';'
    ;

generic_renaming_declaration	:
    GENERIC PACKAGE defining_program_unit_name RENAMES name (aspect_specification)? ';'
    | GENERIC PROCEDURE defining_program_unit_name RENAMES name (aspect_specification)? ';'
    | GENERIC FUNCTION defining_program_unit_name RENAMES name (aspect_specification)? ';'
    ;

//9.1
task_type_declaration	:
    TASK TYPE IDENTIFIER (known_discriminant_part)?
    (aspect_specification)? (IS
    (NEW interface_list WITH)?
    task_definition)? ';'
    ;

single_task_declaration	:
    TASK IDENTIFIER
    (aspect_specification)? (IS
    (NEW interface_list WITH)?
    task_definition)? ';'
    ;

task_definition	:
    (task_item)*
    (PRIVATE
    (task_item)*)?
    END (IDENTIFIER)?
    ;

task_item	:	entry_declaration | aspect_clause ;

task_body	:
    TASK BODY IDENTIFIER
    (aspect_specification)? IS
    declarative_part
    BEGIN
    handled_sequence_of_statements
    END (IDENTIFIER)? ';'
    ;

//9.4
protected_type_declaration	:
    PROTECTED TYPE IDENTIFIER (known_discriminant_part)?
    (aspect_specification)? IS
    (pragma)*
    (NEW interface_list WITH)?
    protected_definition ';'
    ;

single_protected_declaration	:
    PROTECTED IDENTIFIER
    (aspect_specification)? IS
    (NEW interface_list WITH)?
    protected_definition ';'
    ;

protected_definition	:
    ( protected_operation_declaration )*
    ( PRIVATE
    ( protected_element_declaration )* )?
    END (IDENTIFIER)?
    ;

protected_operation_declaration	:	subprogram_declaration
    | entry_declaration
    | aspect_clause
    ;

protected_element_declaration	:	protected_operation_declaration
    | component_declaration
    ;

protected_body	:
    PROTECTED BODY IDENTIFIER
    (aspect_specification)? IS
    ( protected_operation_item )*
    END (IDENTIFIER)? ';'
    ;

protected_operation_item	:	subprogram_declaration
    | subprogram_body
    | null_procedure_declaration
    | expression_function_declaration
    | entry_body
    | aspect_clause
    ;

//9.5
//synchronization_kind ::= By_Entry | By_Protected_Procedure | Optional

entry_declaration	:
    (overriding_indicator)?
    ENTRY IDENTIFIER ('(' discrete_subtype_definition ')')? parameter_profile
    (aspect_specification)? ';'
    ;

accept_statement	:
    ACCEPT direct_name ('(' entry_index ')')? parameter_profile (DO
    handled_sequence_of_statements
    END (IDENTIFIER)? )? ';'
    ;

entry_index	:	expression ;

entry_body	:
    ENTRY IDENTIFIER entry_body_formal_part entry_barrier IS
    declarative_part
    BEGIN
    handled_sequence_of_statements
    END (IDENTIFIER)? ';'
    ;

entry_body_formal_part		:	('(' entry_index_specification ')')? parameter_profile ;

entry_barrier				:	WHEN condition ;

entry_index_specification	:	FOR IDENTIFIER IN discrete_subtype_definition ;

entry_call_statement		:	name (actual_parameter_part)? ';' ;

requeue_statement			:	REQUEUE name (WITH ABORT)? ';' ;

//9.6
delay_statement				:	delay_until_statement | delay_relative_statement ;

delay_until_statement		:	DELAY UNTIL expression ';' ;

delay_relative_statement	:	DELAY expression ';' ;

//9.7
select_statement			:
    selective_accept
    | timed_entry_call
    | conditional_entry_call
    | asynchronous_select
    ;

selective_accept			:
    SELECT
    (guard)?
    select_alternative
    ( OR (guard)? select_alternative )*
    ( ELSE sequence_of_statements )?
    END SELECT ';'
    ;

guard						:	WHEN condition '=>' ;

select_alternative			:
    accept_alternative
    | delay_alternative
    | terminate_alternative
    ;

accept_alternative			:
    accept_statement (sequence_of_statements)? ;

delay_alternative			:
    delay_statement (sequence_of_statements)? ;

terminate_alternative		:	TERMINATE ';' ;

timed_entry_call			:
    SELECT
    entry_call_alternative
    OR
    delay_alternative
    END SELECT ';'
    ;

entry_call_alternative		:
    procedure_or_entry_call (sequence_of_statements)? ;

procedure_or_entry_call		:
    procedure_call_statement | entry_call_statement ;

conditional_entry_call		:
    SELECT
    entry_call_alternative
    ELSE
    sequence_of_statements
    END SELECT ';'
    ;

asynchronous_select			:
    SELECT
    triggering_alternative
    THEN ABORT
    abortable_part
    END SELECT ';'
    ;

triggering_alternative		:	triggering_statement (sequence_of_statements)? ;

triggering_statement		:	procedure_or_entry_call | delay_statement ;

abortable_part				:	sequence_of_statements ;

abort_statement				:	ABORT name (',' name)* ';' ;

//10.1
compilation	: (compilation_unit)* ;

compilation_unit	:
    context_clause (library_item | subunit) (pragma)*
    ;

library_item	:	(PRIVATE)? library_unit_declaration
    | library_unit_body
    | (PRIVATE)? library_unit_renaming_declaration
    ;

library_unit_declaration	:
    subprogram_declaration | package_declaration
    | generic_declaration | generic_instantiation
    ;

library_unit_renaming_declaration	:
    package_renaming_declaration
    | generic_renaming_declaration
    | subprogram_renaming_declaration
    ;

library_unit_body		:	subprogram_body | package_body ;

parent_unit_name		:	name ;

context_clause			:	(context_item)* ;

context_item			:	(pragma)* (with_clause | use_clause | pragma) ;

with_clause				:	limited_with_clause | nonlimited_with_clause ;

limited_with_clause		:	LIMITED (PRIVATE)? WITH package_name (',' package_name)* ';' ;

nonlimited_with_clause	:	(PRIVATE)? WITH package_name (',' package_name)* ';' ;

body_stub				:	subprogram_body_stub | package_body_stub | task_body_stub | protected_body_stub ;

subprogram_body_stub	:
    (overriding_indicator)?
    subprogram_specification IS SEPARATE
    (aspect_specification)? ';'
    ;

package_body_stub		:
    PACKAGE BODY IDENTIFIER IS SEPARATE
    (aspect_specification)? ';'
    ;

task_body_stub			:
    TASK BODY IDENTIFIER IS SEPARATE
    (aspect_specification)? ';'
    ;

protected_body_stub		:
    PROTECTED BODY IDENTIFIER IS SEPARATE
    (aspect_specification)? ';'
    ;

subunit					:	SEPARATE '(' parent_unit_name ')' proper_body ;

//11.1
exception_declaration	:	defining_identifier_list COLON EXCEPTION
    (aspect_specification)? ';'
    ;

handled_sequence_of_statements	:
    sequence_of_statements
    (EXCEPTION
    exception_handler
    (exception_handler)* )?
    ;

exception_handler		:
    WHEN (choice_parameter_specification COLON )? exception_choice ('|' exception_choice)* '=>' sequence_of_statements
    ;

choice_parameter_specification	: IDENTIFIER ;

exception_choice				:	name | OTHERS ;

raise_statement					:	RAISE ';'
    | RAISE name (WITH expression)? ';'
    ;

raise_expression				:	RAISE name (WITH simple_expression)? ;

//12.1
generic_declaration				:	generic_subprogram_declaration | generic_package_declaration ;

generic_subprogram_declaration	:	generic_formal_part subprogram_specification (aspect_specification)? ';' ;

generic_package_declaration		:	generic_formal_part package_specification ';' ;

generic_formal_part				:	GENERIC (generic_formal_parameter_declaration | use_clause | pragma)* ;

generic_formal_parameter_declaration	:
    formal_object_declaration
    | formal_type_declaration
    | formal_subprogram_declaration
    | formal_package_declaration
    ;

//12.3
generic_instantiation	:
    PACKAGE defining_program_unit_name IS NEW name (generic_actual_part)? (aspect_specification)? ';'
    | (overriding_indicator)? PROCEDURE defining_program_unit_name IS NEW name (generic_actual_part)? (aspect_specification)? ';'
    | (overriding_indicator)? FUNCTION defining_designator IS NEW name (generic_actual_part)? (aspect_specification)? ';'
    ;

generic_actual_part	:	'('generic_association (',' generic_association)*')' ;

generic_association	:	(selector_name '=>')? explicit_generic_actual_parameter ;

explicit_generic_actual_parameter	:	expression | name | subtype_mark ;

//12.4
formal_object_declaration	:
    defining_identifier_list COLON xmode (null_exclusion)? subtype_mark (':=' default_expression)? (aspect_specification)? ';'
    | defining_identifier_list COLON xmode access_definition (':=' default_expression)? (aspect_specification)? ';'
    ;

//12.5
formal_type_declaration	:
    formal_complete_type_declaration
    | formal_incomplete_type_declaration
    ;

formal_complete_type_declaration	:
    TYPE IDENTIFIER (discriminant_part)? IS formal_type_definition (aspect_specification)? ';'
    ;

formal_incomplete_type_declaration	:
    TYPE IDENTIFIER (discriminant_part)? (IS TAGGED)? ';'
    ;

formal_type_definition	:
    formal_private_type_definition
    | formal_derived_type_definition
    | formal_discrete_type_definition
    | formal_signed_integer_type_definition
    | formal_modular_type_definition
    | formal_floating_point_definition
    | formal_ordinary_fixed_point_definition
    | formal_decimal_fixed_point_definition
    | formal_array_type_definition
    | formal_access_type_definition
    | formal_interface_type_definition
    ;

formal_private_type_definition			:	((ABSTRACT)? TAGGED)? (LIMITED)? PRIVATE ;

formal_derived_type_definition			:	(ABSTRACT)? (LIMITED | SYNCHRONIZED)? NEW subtype_mark ((AND interface_list)? WITH PRIVATE)? ;

formal_discrete_type_definition			:	'(''<>'')' ;

formal_signed_integer_type_definition	:	RANGE '<>' ;

formal_modular_type_definition			:	MOD '<>' ;

formal_floating_point_definition		:	DIGITS '<>' ;

formal_ordinary_fixed_point_definition	:	DELTA '<>' ;

formal_decimal_fixed_point_definition	:	DELTA '<>' DIGITS '<>' ;

formal_array_type_definition			:	array_type_definition ;

formal_access_type_definition			:	access_type_definition ;

formal_interface_type_definition		:	interface_type_definition ;

formal_subprogram_declaration			:	formal_concrete_subprogram_declaration
    | formal_abstract_subprogram_declaration
    ;

formal_concrete_subprogram_declaration	:
    WITH subprogram_specification (IS subprogram_default)? (aspect_specification)? ';'
    ;

formal_abstract_subprogram_declaration	:
    WITH subprogram_specification IS ABSTRACT (subprogram_default)? (aspect_specification)? ';'
    ;

subprogram_default	:	name | '<>' | NULL ;

//12.7
formal_package_declaration	:
    WITH PACKAGE IDENTIFIER IS NEW name formal_package_actual_part (aspect_specification)? ';'
    ;

formal_package_actual_part	:
    '(' (OTHERS '=>')? '<>' ')'
    | (generic_actual_part)?
    | '(' formal_package_association (',' formal_package_association)* (',' OTHERS '=>' '<>')? ')'
    ;

formal_package_association	:
    generic_association
    | selector_name '=>' '<>'
    ;

//13.1
aspect_clause	:	attribute_definition_clause
    | enumeration_representation_clause
    | record_representation_clause
    | at_clause
    ;

local_name	:	direct_name
    | direct_name TIC attribute_designator
    | name
    ;

aspect_specification	:
    WITH aspect_mark ('=>' aspect_definition)? (',' aspect_mark ('=>' aspect_definition)? )*
    ;

aspect_mark			:	IDENTIFIER(TIC name)? ;

aspect_definition	:	full_name | expression ;
//aspect_definition	:	name | expression | IDENTIFIER ;

attribute_definition_clause	:
    FOR local_name TIC attribute_designator USE expression ';'
    | FOR local_name TIC attribute_designator USE name ';'
    ;

enumeration_representation_clause	:
    FOR local_name USE enumeration_aggregate ';'
    ;

enumeration_aggregate :	array_aggregate ;

record_representation_clause	:
    FOR local_name USE
    RECORD (mod_clause)?
    (component_clause)*
    END RECORD ';'
    ;

component_clause	:
    local_name AT position RANGE first_bit '..' last_bit ';' ;

position		:	expression ;

first_bit		:	simple_expression ;

last_bit		:	simple_expression ;

code_statement	:	qualified_expression ';' ;

storage_pool_indicator	:	name | NULL ;

restriction	:	IDENTIFIER
    | IDENTIFIER '=>' restriction_parameter_argument ;

restriction_parameter_argument	:	name | expression ;

delta_constraint	:	DELTA simple_expression (range_constraint)? ;

at_clause			:	FOR direct_name USE AT expression ';' ;

mod_clause			:	AT MOD expression ';' ;

//----------------------------------------------------------------------------------------------------//
//Lexer

//2.3
// Keywords
ABORT : A B O R T ;
ABS : A B S ;
ABSTRACT : A B S T R A C T ;
ACCEPT : A C C E P T;
ACCESS : A C C E S S ;
ALIASED : A L I A S E D ;
ALL : A L L ;
AND : A N D ;
ARRAY : A R R A Y ;
AT : A T ;
BEGIN : B E G I N ;
BODY : B O D Y ;
CASE : C A S E ;
CONSTANT : C O N S T A N T ;
DECLARE : D E C L A R E ;
DELAY : D E L A Y ;
DELTA : D E L T A ;
DIGITS : D I G I T S ;
DO : D O ;
ELSE : E L S E ;
ELSIF : E L S I F ;
END : E N D ;
ENTRY : E N T R Y;
EXCEPTION : E X C E P T I O N;
EXIT : E X I T;
FOR : F O R;
FUNCTION : F U N C T I O N;
GENERIC : G E N E R I C;
GOTO : G O T O;
IF : I F;
IN : I N;
INTERFACE : I N T E R F A C E;
IS : I S;
LIMITED : L I M I T E D;
LOOP : L O O P;
MOD : M O D;
NEW : N E W;
NOT : N O T;
NULL : N U L L;
OF : O F;
OR : O R;
OTHERS : O T H E R S;
OUT : O U T;
OVERRIDING : O V E R R I D I N G;
PACKAGE : P A C K A G E;
PRAGMA : P R A G M A;
PRIVATE : P R I V A T E;
PROCEDURE : P R O C E D U R E;
PROTECTED : P R O T E C T E D;
RAISE : R A I S E;
RANGE : R A N G E;
RECORD: R E C O R D;
REM : R E M;
RENAMES : R E N A M E S;
REQUEUE : R E Q U E U E;
RETURN : R E T U R N;
REVERSE : R E V E R S E;
SELECT : S E L E C T;
SEPARATE : S E P E R A T E;
SOME : S O M E;
SUBTYPE : S U B T Y P E;
SYNCHRONIZED : S Y N C H R O N I Z E D;
TAGGED : T A G G E D;
TASK : T A S K;
TERMINATE : T E R M I N A T E;
THEN : T H E N;
TYPE : T Y P E;
UNTIL : U N T I L;
USE : U S E;
WHEN : W H E N;
WHILE : W H I L E;
WITH : W I T H;
XOR : X O R;

// case insensitive chars
fragment A : ('a'|'A') ;
fragment B : ('b'|'B') ;
fragment C : ('c'|'C') ;
fragment D : ('d'|'D') ;
fragment E : ('e'|'E') ;
fragment F : ('f'|'F') ;
fragment G : ('g'|'G') ;
fragment H : ('h'|'H') ;
fragment I : ('i'|'I') ;
fragment J : ('j'|'J') ;
fragment K : ('k'|'K') ;
fragment L : ('l'|'L') ;
fragment M : ('m'|'M') ;
fragment N : ('n'|'N') ;
fragment O : ('o'|'O') ;
fragment P : ('p'|'P') ;
fragment Q : ('q'|'Q') ;
fragment R : ('r'|'R') ;
fragment S : ('s'|'S') ;
fragment T : ('t'|'T') ;
fragment U : ('u'|'U') ;
fragment V : ('v'|'V') ;
fragment W : ('w'|'W') ;
fragment X : ('x'|'X') ;
fragment Y : ('y'|'Y') ;
fragment Z : ('z'|'Z') ;

DOT_DOT            :       '..'    ; 
LT_LT              :       '<<'    ; 
BOX                :       '<>'    ; 
GT_GT              :       '>>'    ; 
ASSIGN             :       ':='    ; 
RIGHT_SHAFT        :       '=>'    ; 
NEQ                :       '/='    ; 
LE                 :       '<='    ; 
GE                 :       '>='    ; 
EXPON              :       '**'    ; 
PIPE               :       '|'     ; 
CONCAT             :       '&'     ; 
DOT                :       '.'     ; 
EQ                 :       '='     ; 
LT                 :       '<'     ; 
GT                 :       '>'     ; 
PLUS               :       '+'     ; 
MINUS              :       '-'     ; 
STAR               :       '*'     ; 
DIV                :       '/'     ; 
LPAREN             :       '('     ; 
RPAREN             :       ')'     ; 
COLON              :       ':'     ; 
COMMA              :       ','     ; 
SEMI               :       ';'     ; 
TIC                :       '\''    ; 

IDENTIFIER			: NONDIGIT ( ('_')? ( NONDIGIT|DIGIT ) )* 
                    | '["' BASED_NUMERAL '"]';

//2.4
NUMERIC_LITERAL		: DECIMAL_LITERAL | BASED_LITERAL ;

//2.4.1
fragment
DECIMAL_LITERAL		: NUMERAL ('.' NUMERAL)? (EXPONENT)? ;

fragment
NUMERAL				: DIGIT ('_' | DIGIT)* ;

fragment
EXPONENT			: E ('+')? NUMERAL | E '-' NUMERAL ;

fragment
NONDIGIT			: [a-zA-Z] ; 

fragment
DIGIT				: [0-9] ; 

//2.4.2
fragment
BASED_LITERAL		: BASE '#' BASED_NUMERAL ('.' BASED_NUMERAL)? '#' (EXPONENT)? ;

fragment
BASE				: NUMERAL ;

fragment
BASED_NUMERAL		: EXTENDED_DIGIT (('_')? EXTENDED_DIGIT)* ;

fragment
EXTENDED_DIGIT		: (DIGIT | 'a'..'f' | 'A'..'F') ;

//2.5
CHARACTER_LITERAL	: '\'' . '\'' ; 

//2.6
STRING_LITERAL
                    : '"' ('\"\"' | ~('"'))* '"' ;
 
WS					: [ \t]+ -> skip ;

//2.7
COMMENT				: ('--' INPUT_CHARACTER*) -> channel(HIDDEN) ; 

fragment
INPUT_CHARACTER 
  : ~([\u000D\u000A\u0085\u2028\u2029]) //'<Any Unicode Character Except A NEW_LINE_CHARACTER>'
  ;

NEWLINE				: ( '\r' '\n'? | '\n' ) -> skip ;

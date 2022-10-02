/*
* Copyright (C) 2014 Ulrich Wolffgang <u.wol@wwu.de>
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*/

parser grammar Vba;

options { 
  tokenVocab=VbaLexer;
}

startRule : module EOF;

module :
	WS?
	endOfLine*
	(moduleHeader endOfLine*)?
	moduleConfig? endOfLine*
	moduleAttributes? endOfLine*
	moduleDeclarations? endOfLine*
	moduleBody? endOfLine*
	WS?
;

moduleHeader : VERSION WS DOUBLELITERAL WS CLASS;

moduleConfig :
	BEGIN endOfLine*
	moduleConfigElement+
	END
;

moduleConfigElement :
	ambiguousIdentifier WS? EQ WS? literal endOfLine*
;

moduleAttributes : (attributeStmt endOfLine+)+;

moduleDeclarations : moduleDeclarationsElement (endOfLine+ moduleDeclarationsElement)* endOfLine*;

moduleOption :
	OPTION_BASE WS SHORTLITERAL 					# optionBaseStmt
	| OPTION_COMPARE WS (BINARY | TEXT | DATABASE) 	# optionCompareStmt
	| OPTION_EXPLICIT 								# optionExplicitStmt
	| OPTION_PRIVATE_MODULE 						# optionPrivateModuleStmt
;

moduleDeclarationsElement :
	comment
	| declareStmt
	| enumerationStmt
	| eventStmt
	| constStmt
	| implementsStmt
	| variableStmt
	| moduleOption
	| typeStmt
	| macroStmt
;

macroStmt :
	macroConstStmt
	| macroIfThenElseStmt;

moduleBody :
	moduleBodyElement (endOfLine+ moduleBodyElement)* endOfLine*;

moduleBodyElement :
	functionStmt
	| propertyGetStmt
	| propertySetStmt
	| propertyLetStmt
	| subStmt
	| macroStmt
;


// block ----------------------------------

attributeStmt : ATTRIBUTE WS implicitCallStmt_InStmt WS? EQ WS? literal (WS? ',' WS? literal)*;

block : blockStmt (endOfStatement blockStmt)* endOfStatement;

blockStmt :
	lineLabel
    | appactivateStmt
	| attributeStmt
	| beepStmt
	| chdirStmt
	| chdriveStmt
	| closeStmt
	| constStmt
	| dateStmt
	| deleteSettingStmt
	| deftypeStmt
	| doLoopStmt
	| endStmt
	| eraseStmt
	| errorStmt
	| exitStmt
	| explicitCallStmt
	| filecopyStmt
	| forEachStmt
	| forNextStmt
	| getStmt
	| goSubStmt
	| goToStmt
	| ifThenElseStmt
	| implementsStmt
	| inputStmt
	| killStmt
	| letStmt
	| lineInputStmt
	| loadStmt
	| lockStmt
	| lsetStmt
	| macroStmt
	| midStmt
	| mkdirStmt
	| nameStmt
	| onErrorStmt
	| onGoToStmt
	| onGoSubStmt
	| openStmt
	| printStmt
	| putStmt
	| raiseEventStmt
	| randomizeStmt
	| redimStmt
	| resetStmt
	| resumeStmt
	| returnStmt
	| rmdirStmt
	| rsetStmt
	| savepictureStmt
	| saveSettingStmt
	| seekStmt
	| selectCaseStmt
	| sendkeysStmt
	| setattrStmt
	| setStmt
	| stopStmt
	| timeStmt
	| unloadStmt
	| unlockStmt
	| variableStmt
	| whileWendStmt
	| widthStmt
	| withStmt
	| writeStmt
	| implicitCallStmt_InBlock
    | implicitCallStmt_InStmt
;


// statements ----------------------------------

appactivateStmt : APPACTIVATE WS valueStmt (WS? ',' WS? valueStmt)?;

beepStmt : BEEP;

chdirStmt : CHDIR WS valueStmt;

chdriveStmt : CHDRIVE WS valueStmt;

closeStmt : CLOSE (WS fileNumber (WS? ',' WS? fileNumber)*)?;

constStmt : (visibility WS)? CONST WS constSubStmt (WS? ',' WS? constSubStmt)*;

constSubStmt : ambiguousIdentifier typeHint? (WS asTypeClause)? WS? EQ WS? valueStmt;

dateStmt : DATE WS? EQ WS? valueStmt;

declareStmt : (visibility WS)? DECLARE WS (PTRSAFE WS)? ((FUNCTION typeHint?) | SUB) WS ambiguousIdentifier typeHint? WS LIB WS STRINGLITERAL (WS ALIAS WS STRINGLITERAL)? (WS? argList)? (WS asTypeClause)?;

deftypeStmt :
	(
		DEFBOOL | DEFBYTE | DEFINT | DEFLNG | DEFCUR |
		DEFSNG | DEFDBL | DEFDEC | DEFDATE |
		DEFSTR | DEFOBJ | DEFVAR
	) WS
	letterrange (WS? ',' WS? letterrange)*
;

deleteSettingStmt : DELETESETTING WS valueStmt WS? ',' WS? valueStmt (WS? ',' WS? valueStmt)?;

doLoopStmt :
	DO endOfStatement
	block?
	LOOP
	|
	DO WS (WHILE | UNTIL) WS valueStmt endOfStatement
	block?
	LOOP
	|
	DO endOfStatement
	block
	LOOP WS (WHILE | UNTIL) WS valueStmt
;

endStmt : END;

enumerationStmt:
	(visibility WS)? ENUM WS ambiguousIdentifier endOfStatement
	enumerationStmt_Constant*
	END_ENUM
;

enumerationStmt_Constant : ambiguousIdentifier (WS? EQ WS? valueStmt)? endOfStatement;

eraseStmt : ERASE WS valueStmt (',' WS? valueStmt)*?;

errorStmt : ERROR WS valueStmt;

eventStmt : (visibility WS)? EVENT WS ambiguousIdentifier WS? argList;

exitStmt : EXIT_DO | EXIT_FOR | EXIT_FUNCTION | EXIT_PROPERTY | EXIT_SUB;

filecopyStmt : FILECOPY WS valueStmt WS? ',' WS? valueStmt;

forEachStmt :
	FOR WS EACH WS ambiguousIdentifier typeHint? WS IN WS valueStmt endOfStatement
	block?
	NEXT (WS ambiguousIdentifier)?
;

forNextStmt :
	FOR WS ambiguousIdentifier typeHint? (WS asTypeClause)? WS? EQ WS? valueStmt WS TO WS valueStmt (WS STEP WS valueStmt)? endOfStatement
	block?
	NEXT (WS ambiguousIdentifier)?
;

functionStmt :
	(visibility WS)? (STATIC WS)? FUNCTION WS? ambiguousIdentifier typeHint? (WS? argList)? (WS? asTypeClause)? endOfStatement
	block?
	END_FUNCTION
;

getStmt : GET WS fileNumber WS? ',' WS? valueStmt? WS? ',' WS? valueStmt;

goSubStmt : GOSUB WS valueStmt;

goToStmt : GOTO WS valueStmt;

ifThenElseStmt :
	IF WS ifConditionStmt WS THEN WS blockStmt (WS ELSE WS blockStmt)?	# inlineIfThenElse
	| ifBlockStmt ifElseIfBlockStmt* ifElseBlockStmt? END_IF			# blockIfThenElse
;

ifBlockStmt :
	IF WS ifConditionStmt WS THEN endOfStatement
	block?
;

ifConditionStmt : valueStmt;

ifElseIfBlockStmt :
	ELSEIF WS ifConditionStmt WS THEN endOfStatement
	block?
;

ifElseBlockStmt :
	ELSE endOfStatement
	block?
;

implementsStmt : IMPLEMENTS WS ambiguousIdentifier;

inputStmt : INPUT WS fileNumber (WS? ',' WS? valueStmt)+;

killStmt : KILL WS valueStmt;

letStmt : (LET WS)? implicitCallStmt_InStmt WS? (EQ | PLUS_EQ | MINUS_EQ) WS? valueStmt;

lineInputStmt : LINE_INPUT WS fileNumber WS? ',' WS? valueStmt;

loadStmt : LOAD WS valueStmt;

lockStmt : LOCK WS valueStmt (WS? ',' WS? valueStmt (WS TO WS valueStmt)?)?;

lsetStmt : LSET WS implicitCallStmt_InStmt WS? EQ WS? valueStmt;

macroConstStmt : MACRO_CONST WS? ambiguousIdentifier WS? EQ WS? valueStmt;

macroIfThenElseStmt : macroIfBlockStmt macroElseIfBlockStmt* macroElseBlockStmt? MACRO_END_IF;

macroIfBlockStmt :
	MACRO_IF WS? ifConditionStmt WS THEN endOfStatement
	(moduleDeclarations | moduleBody | block)*
;

macroElseIfBlockStmt :
	MACRO_ELSEIF WS? ifConditionStmt WS THEN endOfStatement
	(moduleDeclarations | moduleBody | block)*
;

macroElseBlockStmt :
	MACRO_ELSE endOfStatement
	(moduleDeclarations | moduleBody | block)*
;

midStmt : MID WS? LPAREN WS? argsCall WS? RPAREN;

mkdirStmt : MKDIR WS valueStmt;

nameStmt : NAME WS valueStmt WS AS WS valueStmt;

onErrorStmt : (ON_ERROR | ON_LOCAL_ERROR) WS (GOTO WS valueStmt | RESUME WS NEXT);

onGoToStmt : ON WS valueStmt WS GOTO WS valueStmt (WS? ',' WS? valueStmt)*;

onGoSubStmt : ON WS valueStmt WS GOSUB WS valueStmt (WS? ',' WS? valueStmt)*;

openStmt :
	OPEN WS valueStmt WS FOR WS (APPEND | BINARY | INPUT | OUTPUT | RANDOM)
	(WS ACCESS WS (READ | WRITE | READ_WRITE))?
	(WS (SHARED | LOCK_READ | LOCK_WRITE | LOCK_READ_WRITE))?
	WS AS WS fileNumber
	(WS LEN WS? EQ WS? valueStmt)?
;

outputList :
	outputList_Expression (WS? (';' | ',') WS? outputList_Expression?)*
	| outputList_Expression? (WS? (';' | ',') WS? outputList_Expression?)+
;

outputList_Expression :
	valueStmt
	| (SPC | TAB) (WS? LPAREN WS? argsCall WS? RPAREN)?
;

printStmt : PRINT WS fileNumber WS? ',' (WS? outputList)?;

propertyGetStmt :
	(visibility WS)? (STATIC WS)? PROPERTY_GET WS ambiguousIdentifier typeHint? (WS? argList)? (WS asTypeClause)? endOfStatement
	block?
	END_PROPERTY
;

propertySetStmt :
	(visibility WS)? (STATIC WS)? PROPERTY_SET WS ambiguousIdentifier (WS? argList)? endOfStatement
	block?
	END_PROPERTY
;

propertyLetStmt :
	(visibility WS)? (STATIC WS)? PROPERTY_LET WS ambiguousIdentifier (WS? argList)? endOfStatement
	block?
	END_PROPERTY
;

putStmt : PUT WS fileNumber WS? ',' WS? valueStmt? WS? ',' WS? valueStmt;

raiseEventStmt : RAISEEVENT WS ambiguousIdentifier (WS? LPAREN WS? (argsCall WS?)? RPAREN)?;

randomizeStmt : RANDOMIZE (WS valueStmt)?;

redimStmt : REDIM WS (PRESERVE WS)? redimSubStmt (WS?',' WS? redimSubStmt)*;

redimSubStmt : implicitCallStmt_InStmt WS? LPAREN WS? subscripts WS? RPAREN (WS asTypeClause)?;

resetStmt : RESET;

resumeStmt : RESUME (WS (NEXT | ambiguousIdentifier))?;

returnStmt : RETURN;

rmdirStmt : RMDIR WS valueStmt;

rsetStmt : RSET WS implicitCallStmt_InStmt WS? EQ WS? valueStmt;

savepictureStmt : SAVEPICTURE WS valueStmt WS? ',' WS? valueStmt;

saveSettingStmt : SAVESETTING WS valueStmt WS? ',' WS? valueStmt WS? ',' WS? valueStmt WS? ',' WS? valueStmt;

seekStmt : SEEK WS fileNumber WS? ',' WS? valueStmt;

selectCaseStmt :
	SELECT WS CASE WS valueStmt endOfStatement
	sC_Case*
	END_SELECT
;

sC_Selection :
    IS WS? comparisonOperator WS? valueStmt                       # caseCondIs
    | valueStmt WS TO WS valueStmt                                # caseCondTo
    | valueStmt                                                   # caseCondValue
;

sC_Case :
	CASE WS sC_Cond endOfStatement
	block?
;

// ELSE first, so that it is not interpreted as a variable call
sC_Cond :
    ELSE                                                            # caseCondElse
    | sC_Selection (WS? ',' WS? sC_Selection)*                      # caseCondSelection
;

sendkeysStmt : SENDKEYS WS valueStmt (WS? ',' WS? valueStmt)?;

setattrStmt : SETATTR WS valueStmt WS? ',' WS? valueStmt;

setStmt : SET WS implicitCallStmt_InStmt WS? EQ WS? valueStmt;

stopStmt : STOP;

subStmt :
	(visibility WS)? (STATIC WS)? SUB WS? ambiguousIdentifier (WS? argList)? endOfStatement
	block?
	END_SUB
;

timeStmt : TIME WS? EQ WS? valueStmt;

typeStmt :
	(visibility WS)? TYPE WS ambiguousIdentifier endOfStatement
	typeStmt_Element*
	END_TYPE
;

typeStmt_Element : ambiguousIdentifier (WS? LPAREN (WS? subscripts)? WS? RPAREN)? (WS asTypeClause)? endOfStatement;

typeOfStmt : TYPEOF WS valueStmt (WS IS WS type_)?;

unloadStmt : UNLOAD WS valueStmt;

unlockStmt : UNLOCK WS fileNumber (WS? ',' WS? valueStmt (WS TO WS valueStmt)?)?;

// operator precedence is represented by rule order
valueStmt :
	literal 												# vsLiteral
	| implicitCallStmt_InStmt 								# vsICS
	| LPAREN WS? valueStmt (WS? ',' WS? valueStmt)* RPAREN 	# vsStruct
	| NEW WS? valueStmt 										# vsNew
	| typeOfStmt 											# vsTypeOf
	| midStmt 												# vsMid
	| ADDRESSOF WS? valueStmt 								# vsAddressOf
	| implicitCallStmt_InStmt WS? ASSIGN WS? valueStmt 		# vsAssign

	| valueStmt WS? IS WS? valueStmt 							# vsIs
	| valueStmt WS? LIKE WS? valueStmt 						# vsLike
	| valueStmt WS? GEQ WS? valueStmt 						# vsGeq
	| valueStmt WS? LEQ WS? valueStmt 						# vsLeq
	| valueStmt WS? GT WS? valueStmt 						# vsGt
	| valueStmt WS? LT WS? valueStmt 						# vsLt
	| valueStmt WS? NEQ WS? valueStmt 						# vsNeq
	| valueStmt WS? EQ WS? valueStmt 						# vsEq

	| valueStmt WS? POW WS? valueStmt 						# vsPow
	| MINUS WS? valueStmt 									# vsNegation
	| PLUS WS? valueStmt 									# vsPlus
	| valueStmt WS? DIV WS? valueStmt 						# vsDiv
	| valueStmt WS? MULT WS? valueStmt 						# vsMult
	| valueStmt WS? MOD WS? valueStmt 						# vsMod
	| valueStmt WS? PLUS WS? valueStmt 						# vsAdd
	| valueStmt WS? MINUS WS? valueStmt 					# vsMinus
	| valueStmt WS? AMPERSAND WS? valueStmt 					# vsAmp

	| valueStmt WS? IMP WS? valueStmt 						# vsImp
	| valueStmt WS? EQV WS? valueStmt 						# vsEqv
	| valueStmt WS? XOR WS? valueStmt 						# vsXor
	| valueStmt WS? OR WS? valueStmt 						# vsOr
	| valueStmt WS? AND WS? valueStmt 						# vsAnd
	| NOT WS? valueStmt 										# vsNot
;

variableStmt : (DIM | STATIC | visibility) WS (WITHEVENTS WS)? variableListStmt;

variableListStmt : variableSubStmt (WS? ',' WS? variableSubStmt)*;

variableSubStmt : ambiguousIdentifier (WS? LPAREN WS? (subscripts WS?)? RPAREN WS?)? typeHint? (WS asTypeClause)?;

whileWendStmt :
	WHILE WS valueStmt endOfStatement
	block?
	WEND
;

widthStmt : WIDTH WS fileNumber WS? ',' WS? valueStmt;

withStmt :
	WITH WS (implicitCallStmt_InStmt | (NEW WS type_)) endOfStatement
	block?
	END_WITH
;

writeStmt : WRITE WS fileNumber WS? ',' (WS? outputList)?;


fileNumber : '#'? valueStmt;


// complex call statements ----------------------------------

explicitCallStmt :
	eCS_ProcedureCall
	| eCS_MemberProcedureCall
;

// parantheses are required in case of args -> empty parantheses are removed
eCS_ProcedureCall : CALL WS ambiguousIdentifier typeHint? (WS? LPAREN WS? argsCall WS? RPAREN)? (WS? LPAREN subscripts RPAREN)*;



// parantheses are required in case of args -> empty parantheses are removed
eCS_MemberProcedureCall : CALL WS implicitCallStmt_InStmt? '.' ambiguousIdentifier typeHint? (WS? LPAREN WS? argsCall WS? RPAREN)? (WS? LPAREN subscripts RPAREN)*;


implicitCallStmt_InBlock :
	iCS_B_MemberProcedureCall
	| iCS_B_ProcedureCall
;

iCS_B_MemberProcedureCall : implicitCallStmt_InStmt? '.' ambiguousIdentifier typeHint? (WS argsCall)? dictionaryCallStmt? (WS? LPAREN subscripts RPAREN)*;

// parantheses are forbidden in case of args
// variables cannot be called in blocks
// certainIdentifier instead of ambiguousIdentifier for preventing ambiguity with statement keywords
iCS_B_ProcedureCall : certainIdentifier (WS argsCall)? (WS? LPAREN subscripts RPAREN)*;


// iCS_S_MembersCall first, so that member calls are not resolved as separate iCS_S_VariableOrProcedureCalls
implicitCallStmt_InStmt :
	iCS_S_MembersCall
	| iCS_S_VariableOrProcedureCall
	| iCS_S_ProcedureOrArrayCall
	| iCS_S_DictionaryCall
;

iCS_S_VariableOrProcedureCall : ambiguousIdentifier typeHint? dictionaryCallStmt? (WS? LPAREN subscripts RPAREN)*;

iCS_S_ProcedureOrArrayCall : (ambiguousIdentifier | baseType) typeHint? WS? LPAREN WS? (argsCall WS?)? RPAREN dictionaryCallStmt? (WS? LPAREN subscripts RPAREN)*;

iCS_S_MembersCall : (iCS_S_VariableOrProcedureCall | iCS_S_ProcedureOrArrayCall)? iCS_S_MemberCall+ dictionaryCallStmt? (WS? LPAREN subscripts RPAREN)*;

iCS_S_MemberCall : LINE_CONTINUATION? ('.' | '!') LINE_CONTINUATION? (iCS_S_VariableOrProcedureCall | iCS_S_ProcedureOrArrayCall);

iCS_S_DictionaryCall : dictionaryCallStmt;


// atomic call statements ----------------------------------

argsCall : (argCall? WS? (',' | ';') WS?)* argCall (WS? (',' | ';') WS? argCall?)*;

argCall : LPAREN? ((BYVAL | BYREF | PARAMARRAY) WS)? RPAREN? valueStmt;

dictionaryCallStmt : '!' ambiguousIdentifier typeHint?;


// atomic rules for statements

argList : LPAREN (WS? arg (WS? ',' WS? arg)*)? WS? RPAREN;

arg : (OPTIONAL WS)? ((BYVAL | BYREF) WS)? (PARAMARRAY WS)? ambiguousIdentifier typeHint? (WS? LPAREN WS? RPAREN)? (WS? asTypeClause)? (WS? argDefaultValue)?;

argDefaultValue : EQ WS? valueStmt;

subscripts : subscript_ (WS? ',' WS? subscript_)*;

subscript_ : (valueStmt WS TO WS)? valueStmt;


// atomic rules ----------------------------------

ambiguousIdentifier :
	(IDENTIFIER | ambiguousKeyword)+
;

asTypeClause : AS WS? (NEW WS)? type_ (WS? fieldLength)?;

baseType : BOOLEAN | BYTE | COLLECTION | DATE | DOUBLE | INTEGER | LONG | SINGLE | STRING (WS? MULT WS? valueStmt)? | VARIANT;

certainIdentifier :
	IDENTIFIER (ambiguousKeyword | IDENTIFIER)*
	| ambiguousKeyword (ambiguousKeyword | IDENTIFIER)+
;

comparisonOperator : LT | LEQ | GT | GEQ | EQ | NEQ | IS | LIKE;

complexType : ambiguousIdentifier (('.' | '!') ambiguousIdentifier)*;

fieldLength : MULT WS? (INTEGERLITERAL | ambiguousIdentifier);

letterrange : certainIdentifier (WS? MINUS WS? certainIdentifier)?;

lineLabel : ambiguousIdentifier ':';

literal : HEXLITERAL | OCTLITERAL | DATELITERAL | DOUBLELITERAL | INTEGERLITERAL | SHORTLITERAL | STRINGLITERAL | TRUE | FALSE | NOTHING | NULL_;

type_ : (baseType | complexType) (WS? LPAREN WS? RPAREN)?;

typeHint : '&' | '%' | '#' | '!' | '@' | '$';

visibility : PRIVATE | PUBLIC | FRIEND | GLOBAL;

// ambiguous keywords
ambiguousKeyword :
	ACCESS | ADDRESSOF | ALIAS | AND | ATTRIBUTE | APPACTIVATE | APPEND | AS |
	BEEP | BEGIN | BINARY | BOOLEAN | BYVAL | BYREF | BYTE |
	CALL | CASE | CLASS | CLOSE | CHDIR | CHDRIVE | COLLECTION | CONST |
	DATABASE | DATE | DECLARE | DEFBOOL | DEFBYTE | DEFCUR | DEFDBL | DEFDATE | DEFDEC | DEFINT | DEFLNG | DEFOBJ | DEFSNG | DEFSTR | DEFVAR | DELETESETTING | DIM | DO | DOUBLE |
	EACH | ELSE | ELSEIF | END | ENUM | EQV | ERASE | ERROR | EVENT |
	FALSE | FILECOPY | FRIEND | FOR | FUNCTION |
	GET | GLOBAL | GOSUB | GOTO |
	IF | IMP | IMPLEMENTS | IN | INPUT | IS | INTEGER |
	KILL |
	LOAD | LOCK | LONG | LOOP | LEN | LET | LIB | LIKE | LSET |
	ME | MID | MKDIR | MOD |
	NAME | NEXT | NEW | NOT | NOTHING | NULL_ |
	ON | OPEN | OPTIONAL | OR | OUTPUT |
	PARAMARRAY | PRESERVE | PRINT | PRIVATE | PUBLIC | PUT |
	RANDOM | RANDOMIZE | RAISEEVENT | READ | REDIM | REM | RESET | RESUME | RETURN | RMDIR | RSET |
	SAVEPICTURE | SAVESETTING | SEEK | SELECT | SENDKEYS | SET | SETATTR | SHARED | SINGLE | SPC | STATIC | STEP | STOP | STRING | SUB |
	TAB | TEXT | THEN | TIME | TO | TRUE | TYPE | TYPEOF |
	UNLOAD | UNLOCK | UNTIL |
	VARIANT | VERSION |
	WEND | WHILE | WIDTH | WITH | WITHEVENTS | WRITE |
	XOR
;

remComment : REMCOMMENT;

comment : COMMENT;

endOfLine : WS? (NEWLINE | comment | remComment) WS?;

endOfStatement : (endOfLine | WS? COLON WS?)*;
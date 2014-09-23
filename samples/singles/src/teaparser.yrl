%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-

%% This is a Yecc file. Set coloration to Erlang to save your eyes.

Nonterminals
Root
RootElements RootElement Block Separator
Id Bool Int Float Char RawString CookedString ClScalar
Declarations Declaration0 Declaration DimDecl VarDecl FunDecl ExtDecl
Params Param BaseParam ValueParam NamedParam TyAssoc TyAssocs WhereEnd
Expr Expr100 Expr150 Expr200 Expr400 Expr500 Expr550 Expr552 Expr555 Expr600 Expr650 Expr700 ExprMax ExprMaxs
OrOperator AndOperator ContextPerturbation ComparisonOperators RangeOperator ContextQuery UnaryOperators FunctionCall
Atomic LTRassocs LTRassoc Conditional Elsifs Elsif Tuple CrIntension EvIntension Lambda FrozenDims FrozenDimsOrEmptyIntention
CmpOp AddOp MulOp RangeOp ContextOp PrefixOp
.

Terminals
';;' '%%'
'dim' 'fun' 'var'
'if' 'then' 'else' 'elsif' 'fi'
'where' 'end'
'ext' '::'
'<-' '->' '^|' 'v|' '=' '\\'
'.' '!'
'or' 'and'
'@'
'<' '<=' '==' '>=' '>' '!='
'+' '-' '~'
'*' '/' '%'
'..'
'#.' '#!'
bool int float char raw_string cooked_string cl_scalar
'(' '[' '{' ',' '}' ']' ')'
id
.

Rootsymbol Root.

%%% Root of grammar
Root -> RootElements                        : '$1'.
RootElements -> RootElement                     : ['$1'].
RootElements -> RootElement RootElements        : ['$1'|'$2'].
RootElement -> Block                                : '$1'.
RootElement -> Block Separator                      : '$1'.
Separator -> '%%'                                       : '$1'.
Separator -> ';;'                                       : '$1'.

Block -> Declaration        : {declaration,loc('$1'),'$1'}.
Block -> Expr               : {expr,loc('$1'),'$1'}.

%%% Tokens

Id           -> id            : '$1'.
Bool         -> bool          : '$1'.
Int          -> int           : '$1'.
Float        -> float         : '$1'.
Char         -> char          : '$1'.
RawString    -> raw_string    : '$1'.
CookedString -> cooked_string : '$1'.
ClScalar     -> cl_scalar     : '$1'.

%%% Declarations

Declaration -> DimDecl          : '$1'.
Declaration -> VarDecl          : '$1'.
Declaration -> FunDecl          : '$1'.
Declaration -> ExtDecl          : '$1'.

DimDecl -> 'dim'     Id                                              : {dim_decl,loc('$1'),'$2'}.
DimDecl -> 'dim'     Id                                    '<-' Expr : {dim_decl,loc('$1'),'$2','$4'}.
VarDecl -> 'var'     Id                                     '=' Expr : {var_decl,loc('$1'),'$2','$4'}.
FunDecl -> 'fun'     Id          Params                     '=' Expr : {fun_decl,loc('$1'),'$2','$3','$5'}.
ExtDecl -> 'ext' Int Id '::' '(' TyAssocs ')' '->' ClScalar '=' Expr : {ext_decl,loc('$1'),'$3','$6','$9','$11','$2'}.

Params -> Param        : ['$1'].
Params -> Param Params : ['$1'|'$2'].
Param -> NamedParam : '$1'.
Param -> BaseParam  : '$1'.
Param -> ValueParam : '$1'.

NamedParam ->     ExprMax : {named_param,loc('$1'),'$1'}.
BaseParam  -> '.' ExprMax : { base_param,loc('$1'),'$2'}.
ValueParam -> '!' ExprMax : {value_param,loc('$1'),'$2'}.

TyAssoc -> Id '::' ClScalar : {ext_ty,loc('$2'),'$1','$3'}.
TyAssocs -> TyAssoc          : ['$1'].
TyAssocs -> TyAssoc TyAssocs : ['$1'|'$2'].

WhereEnd -> 'where' Declarations 'end'      : {where_end,loc('$1'),'$2'}.
Declarations -> '$empty'                        : [].
Declarations -> Declaration0 Declarations       : ['$1'|'$2'].
Declaration0 -> Declaration                     : '$1'.
Declaration0 -> Declaration Separator           : '$1'.

%%% Expressions

Expr -> Expr WhereEnd   : rework_where_end({expr_with_where_end,'$1','$2'}).
Expr -> Expr100         : '$1'.

%% Kept to better display rule order
Expr100 -> OrOperator               : '$1'.
Expr150 -> AndOperator              : '$1'.
Expr200 -> ComparisonOperators      : '$1'.
%Expr400 -> LowPriorityOperators     : '$1'.
%Expr500 -> HighPriorityOperators    : '$1'.
Expr550 -> ContextPerturbation      : '$1'.
Expr552 -> RangeOperator            : '$1'.
Expr555 -> ContextQuery             : '$1'.
Expr600 -> UnaryOperators           : '$1'.
Expr650 -> FunctionCall             : '$1'.
Expr700 -> ExprMax                  : '$1'.

ExprMax -> Id                           : '$1'.
ExprMax -> Atomic                       : '$1'.
ExprMax -> Tuple                        : '$1'.
ExprMax -> CrIntension                  : '$1'.
ExprMax -> EvIntension                  : '$1'.
ExprMax -> '(' Expr ')'                 : '$2'.
ExprMax -> Conditional                  : '$1'.
ExprMax -> Lambda                       : '$1'.

ExprMaxs -> ExprMax              : ['$1'].
ExprMaxs -> ExprMax ',' ExprMaxs : ['$1'|'$3'].

Tuple -> '[' LTRassocs ']'              : {tuple,loc('$1'),'$2'}.
LTRassocs -> LTRassoc                   : ['$1'].
LTRassocs -> LTRassoc ',' LTRassocs     : ['$1'|'$3'].
LTRassoc -> Expr '<-' Expr                  : {tuple_element,loc('$1'),'$1','$3'}.

CrIntension -> '^|' FrozenDimsOrEmptyIntention Expr : {intension_creation,loc('$1'),'$2','$3'}.
EvIntension -> 'v|' ExprMax                         : {intension_evaluation,loc('$1'),'$2'}.

FrozenDimsOrEmptyIntention -> '{'          '}'             : [].
FrozenDimsOrEmptyIntention ->    FrozenDims                : '$1'.
FrozenDims ->                 '{' ExprMaxs '}'             : '$2'.

Conditional -> 'if' Expr 'then' Expr Elsifs 'else' Expr 'fi'    : {'if',loc('$1'),[{if_expr,loc('$1'),'$2','$4'}|'$5'],'$7'}.
Elsif ->    'elsif' Expr 'then' Expr    : {if_expr,loc('$1'),'$2','$4'}.
Elsifs -> '$empty'                      : [].
Elsifs -> Elsif Elsifs                  : ['$1'|'$2'].

Lambda -> '\\'            Params '->' Expr : {lambda,loc('$1'),[],'$2','$4'}.
Lambda -> '\\' FrozenDims Params '->' Expr : {lambda,loc('$1'),'$2','$3','$5'}.

%%% Priority-ordered expressions

OrOperator ->              Expr150    'or' Expr100   : {val('$2'),loc('$2'),'$1','$3'}.
OrOperator ->              Expr150                   : '$1'.
AndOperator ->             Expr200   'and' Expr150   : {val('$2'),loc('$2'),'$1','$3'}.
AndOperator ->             Expr200                   : '$1'.
ComparisonOperators ->     Expr400   CmpOp Expr400   : {val('$2'),loc('$2'),'$1','$3'}.
ComparisonOperators ->     Expr400                   : '$1'.
Expr400 ->                 Expr400   AddOp Expr500   : {val('$2'),loc('$2'),'$1','$3'}.
Expr400 ->                                 Expr500   : '$1'.
Expr500 ->                 Expr500   MulOp Expr550   : {val('$2'),loc('$2'),'$1','$3'}.
Expr500 ->                                 Expr550   : '$1'.
ContextPerturbation ->     Expr552     '@' Expr550   : {val('$2'),loc('$2'),'$1','$3'}.
ContextPerturbation ->     Expr552                   : '$1'.
RangeOperator ->           Expr555 RangeOp Expr550   : {val('$2'),loc('$2'),'$1','$3'}.
RangeOperator ->           Expr555                   : '$1'.
ContextQuery ->  ContextOp Expr600                   : {val('$1'), loc('$1'),'$2'}.
ContextQuery ->            Expr600                   : '$1'.
UnaryOperators -> PrefixOp Expr650                   : {val('$1'),loc('$1'),'$2'}.
UnaryOperators ->          Expr650                   : '$1'.
FunctionCall ->            Expr700 Params            : {call,loc('$1'),'$1','$2'}. %% Note how functions have arity ≥ 1.
FunctionCall ->            Expr700                   : '$1'.

%%% Basic data

Atomic -> Bool             : '$1'.
Atomic -> Int              : '$1'.
Atomic -> Float            : '$1'.
Atomic -> Char             : '$1'.
Atomic -> RawString        : '$1'.
Atomic -> CookedString     : '$1'.

%%% Operators

CmpOp -> '<'    : '$1'.
CmpOp -> '<='   : '$1'.
CmpOp -> '=='   : '$1'.
CmpOp -> '>='   : '$1'.
CmpOp -> '>'    : '$1'.
CmpOp -> '!='   : '$1'.

AddOp -> '+'    : '$1'.
AddOp -> '-'    : '$1'.

MulOp -> '*'    : '$1'.
MulOp -> '/'    : '$1'.
MulOp -> '%'    : '$1'.

RangeOp -> '..'     : '$1'.

ContextOp -> '#.'   : '$1'.
ContextOp -> '#!'   : '$1'.

PrefixOp -> '+'     : '$1'.
PrefixOp -> '~'     : '$1'.


%% The number of shit/reduce conflicts this grammar has
Expect 53.

Erlang code.

val (Tuple) when is_tuple(Tuple) ->
    element(1, Tuple);
val (Other) ->
    Other.
loc (Tuple) when is_tuple(Tuple) ->
    element(2, Tuple);
loc (Other) ->
    Other.


rework_where_end ({expr_with_where_end,Expr,WhereEnd}) ->
    {where_end, Loc, Decls} = WhereEnd,
    {WhereDims, WhereVarsAndFuns} = split_declarations(Decls, [], []),
    {where, Loc, Expr, WhereDims, WhereVarsAndFuns}.

split_declarations ([], Dims, Vars) -> {lists:reverse(Dims), lists:reverse(Vars)};
split_declarations ([Decl|Rest], Dims, Vars) ->
    case Decl of
        Dim={dim_decl,_,_} ->
            split_declarations(Rest, [Dim|Dims], Vars);
        Dim={dim_decl,_,_,_} ->
            split_declarations(Rest, [Dim|Dims], Vars);
        OtherDecl ->
            split_declarations(Rest, Dims, [OtherDecl|Vars])
    end.


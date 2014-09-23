%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-

%% This is a Leex file. Set coloration to Erlang to save your eyes.

Definitions.

%% Separators: ';;' or '%%'
DoubleSemiColon  = ((;;;*[\000-\040]*)+)
DoublePercent    = (([\045][\045][\045]*[\000-\040]*)+)

Comment    = (//[^\r\n]*\r?\n)+
%% Unprintables (ASCII 0 till '!', '!' excluded)
Blank      = ([\000-\040])

%% Atomic
Id           = ([a-zA-Z_][a-zA-Z0-9_]*)
Bool         = (true|false)
Int          = (([0-9]+|0[2-9A-Za-z]+))
Float        = ([0-9]+\.[0-9]+f[+-]?[0-9]+)
Char         = ('(\\([^\\]|\\)|[^\\''])')
RawString    = (`[^`]*`)
CookedString = ("(\\([^\\]|\\)|[^\\""])+")

%% Misc
LeftwardsArrow  = (<-|←)
RightwardsArrow = (->|→)
UpwardsArrow    = (↑)
DownwardsArrow  = (↓)
Equal           = (=)
Lambda          = (\\|Λ|λ|ᴧ|Ⲗ|ⲗ|𝚲|𝛌|𝛬|𝜆|𝜦|𝝀|𝝠|𝝺|𝞚|𝞴)
%% (from //en.wikipedia.org/wiki/Lambda#Character_Encodings)

%% Parameter operators
ParamDot  = (\.)
ParamMark = (!)
%% Boolean logic
OrOp  = (\|\||or)
AndOp = (&&|and)
%% Context perturbation
ContextPerturbation = (@)
%% Comparison operators
LessThan             = (<)
LessThanOrEqualTo    = (<=)
IsEqual              = (==)
GreaterThanOrEqualTo = (>=)
GreaterThan          = (>)
Different            = (!=)
%% Low priority operators (and Unary operators)
Plus  = (\+)
Minus = (-)
Tilde = (~)
%% High priority operators
Mul     = (\*)
Solidus = (/)
Percent = ([\045])
%% Range operator
RangeOp = (\.\.)
%% Context query
HashDot  = (#\.)
HashBang = (#!)

%% Extensionality
Ext   = (ext)
Homo  = (::)
%% (list compiled from Tea.g4)
Type  = (bool|bool2|bool4|bool8|bool16|char|char2|char4|char8|char16|uchar|uchar2|uchar4|uchar8|uchar16|double|double2|double4|double8|double16|short|short2|short4|short8|short16|ushort|ushort2|ushort4|ushort8|ushort16|int|int2|int4|int8|int16|uint|uint2|uint4|uint8|uint16|long|long2|long4|long8|long16|ulong|ulong2|ulong4|ulong8|ulong16|float|float2|float4|float8|float16|half|half2|half4|half8|half16|quad|quad2|quad4|quad8|quad16)


%% Keywords
Dim = (dim)
Fun = (fun)
Var = (var)
If    = (if)
Then  = (then)
Elsif = (elsif)
Else  = (else)
Fi    = (fi)
Where  = (where)
End    = (end)


Rules.
%% Note: rule order matters.

{DoubleSemiColon}  : {token,{';;',TokenLine}}.
{DoublePercent}    : {token,{'%%',TokenLine}}.
{Comment}    : skip_token.
{Blank}      : skip_token.

{Dim}       : {token,{'dim',TokenLine}}.
{Fun}       : {token,{'fun',TokenLine}}.
{Var}       : {token,{'var',TokenLine}}.
{If}            : {token,{'if',TokenLine}}.
{Then}          : {token,{'then',TokenLine}}.
{Elsif}         : {token,{'elsif',TokenLine}}.
{Else}          : {token,{'else',TokenLine}}.
{Fi}            : {token,{'fi',TokenLine}}.
{Where}             : {token,{'where',TokenLine}}.
{End}               : {token,{'end',TokenLine}}.

{Ext}       : {token,{'ext',TokenLine}}.
{Homo}      : {token,{'::',TokenLine}}.
{Type}      : {token,{cl_scalar,TokenLine,TokenChars}}.

{LeftwardsArrow}  : {token,{'<-',TokenLine}}.
{RightwardsArrow} : {token,{'->',TokenLine}}.
{UpwardsArrow}    : {token,{'^|',TokenLine}}.
{DownwardsArrow}  : {token,{'v|',TokenLine}}.
{Equal}           : {token,{'=',TokenLine}}.
{Lambda}          : {token,{'\\',TokenLine}}.

{ParamDot}      : {token,{'.',TokenLine}}.
{ParamMark}     : {token,{'!',TokenLine}}.
{OrOp}              : {token,{'or',TokenLine}}.
{AndOp}             : {token,{'and',TokenLine}}.
{ContextPerturbation}   : {token,{'@',TokenLine}}.
{LessThan}                  : {token,{'<',TokenLine}}.
{LessThanOrEqualTo}         : {token,{'<=',TokenLine}}.
{IsEqual}                   : {token,{'==',TokenLine}}.
{GreaterThanOrEqualTo}      : {token,{'>=',TokenLine}}.
{GreaterThan}               : {token,{'>',TokenLine}}.
{Different}                 : {token,{'!=',TokenLine}}.
{Plus}                          : {token,{'+',TokenLine}}.
{Minus}                         : {token,{'-',TokenLine}}.
{Tilde}                         : {token,{'~',TokenLine}}.
{Mul}                               : {token,{'*',TokenLine}}.
{Solidus}                           : {token,{'/',TokenLine}}.
{Percent}                           : {token,{'%',TokenLine}}.
{RangeOp}                               : {token,{'..',TokenLine}}.
{HashDot}                                  : {token,{'#.',TokenLine}}.
{HashBang}                                 : {token,{'#!',TokenLine}}.

{Bool}                      : {token,{bool,TokenLine,TokenChars}}.
{Int}                       : {token,{int,TokenLine,TokenChars}}.
{Float}                     : {token,{float,TokenLine,TokenChars}}.
{Char}                      : {token,{char,TokenLine,TokenChars}}.
{RawString}                 : {token,{raw_string,TokenLine,TokenChars}}.
{CookedString}              : {token,{cooked_string,TokenLine,TokenChars}}.

\(                        : {token,{'(',TokenLine}}.
\[                        : {token,{'[',TokenLine}}.
\{                        : {token,{'{',TokenLine}}.
\,                        : {token,{',',TokenLine}}.
\}                        : {token,{'}',TokenLine}}.
\]                        : {token,{']',TokenLine}}.
\)                        : {token,{')',TokenLine}}.

%% Id: last rule as it can swallow most of the other one.
{Id}          : {token,{id,TokenLine,TokenChars}}.

Erlang code.

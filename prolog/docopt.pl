:- module(
  docopt,
  [
    opt_arguments/3,
    opt_parse/4
  ]
).

:- use_module(
   library(optparse),
   [opt_parse/4 as optparse]
).

%% opt_arguments(+OptsSpec, -Opts, -PositionalArgs)
%
% Extract commandline options according to a [Docopt](http://docopt.org)
% specification.
% Same conventions as in library(optparse) are followed.
opt_arguments(OptsSpec, Opts, PositionalArgs) :-
   % we need os_argv to get past --
   current_prolog_flag(argv, Args),
   opt_parse(OptsSpec, Args, Opts, PositionalArgs).


%% opt_parse(+OptsSpec, +ApplArgs, -Opts, -PositionalArgs)
%
% Parse arguments ApplArgs according to [Docopt](http://docopt.org) spec
% OptsSpec. As in library(optparse), returns Opts as a list of Func(Key,
% Value) options, and PositionalArgs as the remaining arguments.
opt_parse(OptsSpec, ApplArgs, Opts, PositionalArgs) :-
   docopt_to_optparse(OptsSpec, OptparseSpec),
   optparse(OptparseSpec, ApplArgs, Opts, PositionalArgs).


%%%%%


%% docopt_to_optparse(+HelpString, -OptsSpec)
%
% Transform a [Docopt](http://docopt.org) help-string specification into a
% library(optparse) specification.
docopt_to_optparse(HelpString, OptsSpec) :-
   sub_string(HelpString, 0, 6, _, Usage),
   string_lower(Usage, "usage:"),
   sub_string(HelpString, 6, _, 0, NoUsage),
   split_string(NoUsage, "\n", "\s\t", HelpLines),
   (
      memberchk("", HelpLines),
      append(UsagePatterns, [""], Start),
      append(Start, OptionDescriptions, HelpLines)
   ->
      true
   ;
      OptionDescriptions = [],
      UsagePatterns = HelpLines
   ),
   usages_to_optparse(UsagePatterns, UsageSpec),
   options_to_optparse(OptionDescriptions, OptionSpec),
   append(UsageSpec, OptionSpec, AllOptsSpec),
   % add option names opt(name), arguments, merge descriptions
   merge_options(AllOptsSpec, OptsSpec).


%% usages_to_optparse(+UsagePatterns, -UsageSpec)
%
% Gets optparse spec from a list of usage patterns
usages_to_optparse(UsagePatterns, UsageSpec) :-
   maplist(usage_to_optparse, UsagePatterns, Specs),
   concat(Specs, Usage),
   add_positionals(Usage, UsageSpec).


%% options_to_optparse(+OptionDescriptions, -OptionSpec)
%
% Gets optparse spec from a list of option descriptions
options_to_optparse([], []).

options_to_optparse([H | T], Spec) :-
   split_string(H, "", "\s\t\r", [""]),
   !,
   options_to_optparse(T, Spec).

options_to_optparse([Head | Tail], OptionSpec) :-
   (
      string_lower(Head, "options:")
   ->
      Descriptions = Tail
   ;
      sub_string(Head, 0, 8, _, OptionHeader),
      string_lower(OptionHeader, "options:"),
      sub_string(Head, 8, _, 0, NoOptions),
      Descriptions = [NoOptions | Tail]
   ),
   maplist(option_to_optparse, Descriptions, Specs),
   concat(Specs, OptionSpec).


%% usage_to_optparse(+UsageString, -UsageSpec)
%
% translate one usage example into some optparse specs
% FIXME ignores the grouping, alternative, multiple and optional marks
usage_to_optparse(UsageString, UsageSpec) :-
   split_string(UsageString, "\s\t\r", "][)(|.", [_Prog | Usage]),
   maplist(argument_to_optparse, Usage, Specs),
   concat(Specs, UsageSpec).


%% option_to_optparse(+OptionString, -OptionSpec)
%
% translate one option description line into some optparse specs
option_to_optparse(OptionString, OptionSpec) :-
   split_string(OptionString, "", "\s\t\r", [Chomp]),
   (
      sub_string(Chomp, 0, 1, _, "-")
   ->
      option_to_optparse_aux(Chomp, OptionSpec)
   ;
      OptionSpec = []
   ).


%% argument_to_optparse(+Argument, -Spec)
%
% translate one fragment of usage into a partial spec

% Positional Arguments
argument_to_optparse(Argument, [[pos(Argument)]]) :-
   string_upper(Argument, Argument),
   !.

argument_to_optparse(Argument, [[pos(Argument)]]) :-
   sub_string(Argument, 0, 1, _, "<"),
   sub_string(Argument, _, 1, 0, ">"),
   !.

% Options
argument_to_optparse(Argument, Specs) :-
   sub_string(Argument, 0, 2, _, "--"),
   !,
   (
      sub_string(Argument, LOpt, 1, _, "=")
   ->
      LLOpt is LOpt - 2,
      LLLOpt is LOpt + 1,
      sub_string(Argument, 2, LLOpt, _, Option),
      sub_string(Argument, LLLOpt, _, 0, Pos),
      atom_string(Atom, Option),
      Specs = [[longflags([Atom]), pos(Pos)]]
   ;
      sub_string(Argument, 2, _, 0, Option),
      atom_string(Atom, Option),
      Specs = [[longflags([Atom])]]
   ).

argument_to_optparse(Argument, Specs) :-
   sub_string(Argument, 0, 1, _, "-"),
   !,
   % ignore the case where -fFILE means -f FILE
   string_chars(Argument, [_ | Chars]),
   findall(
      [shortflags([C])],
      member(C, Chars),
      Specs
   ).

argument_to_optparse("options", []) :-
   !.


%% option_to_optparse_aux(+OptionString, -OptionSpec)
%
% transform a line of option description starting with - to a spec
option_to_optparse_aux(OptionString, OptionSpec) :-
   (
      sub_string(OptionString, LOpt, 2, _, "  ")
   ->
      sub_string(OptionString, 0, LOpt, _, Options),
      LLOpt is LOpt + 2,
      sub_string(OptionString, LLOpt, _, 0, Descr)
   ;
      Options = OptionString,
      Descr = ""
   ),
   split_string(Options, "\s\t\r,", "\s\t\r,", OptionList),
   maplist(argument_to_optparse, OptionList, Specs),
   flatten(Specs, OptSpec),
   (
      find_default(Descr, [Default1, Default2])
   ->
      OptionSpec = [[Default1, Default2 | OptSpec]]
   ;
      OptionSpec = [OptSpec]
   ).


%% concat(+Listoflists, -List)
%
% append all elements of Listoflists in List
% no foldr in library(apply)
concat([], []).

concat([H | T], Concat) :-
   append(H, C, Concat),
   concat(T, C).


%% merge_options(+Options, -MergedOptions)
%
% merge options coming from different usage cases or descriptions
merge_options([], []).

merge_options([H | T], Options) :-
   memberchk(shortflags(F) , H),
   select(O, T, Opts),
   memberchk(shortflags(F), O),
   !,
   append(H, O, HH),
   merge_options([HH | Opts], Options).

merge_options([H | T], Options) :-
   memberchk(longflags(F) , H),
   select(O, T, Opts),
   memberchk(longflags(F), O),
   !,
   append(H, O, HH),
   merge_options([HH | Opts], Options).

merge_options([H | T], Options) :-
   (
      selectchk(pos(_), H, HH)
   ->
      H1 = [type(atom) | HH]
   ;
      H1 = [type(boolean) | H]
   ),
   merge_types(H1, H2),
   add_default(H2, H3),
   (
      (
         memberchk(longflags([Name]), H)
      ;
         memberchk(shortflags([Name]), H)
      )
   ->
      Options = [[opt(Name) | H3] | Opts]
   ;
      Options = [H3 | Opts]
   ),
   merge_options(T, Opts).


%% add_positionals(+Options, -PosOptions)
%
% add following positional arguments to the preceding option in Options
add_positionals([], []).

add_positionals([H], [H]).

add_positionals([H, [pos(P)] | T], [HH | TT]) :-
   append(H, [pos(P)], HH),
   add_positionals(T, TT).


%% find_default(+DescrString, -Default)
%
% look for a default definition in a line of description
find_default(DescrString, Default) :-
   sub_string(DescrString, _, 9, Def, DefMarker),
   string_lower(DefMarker, "[default:"),
   sub_string(DescrString, _, Def, 0, DefString),
   sub_string(DefString, Bef, 1, _, "]"),
   sub_string(DefString, 0, Bef, _, DefValue),
   split_string(DefValue, "", "\s\t\r", [Chomp]),
   (
      number_string(Number, Chomp)
   ->
      Default = [type(float), default(Number)]
   ;
      atom_string(Atom, Chomp),
      Default = [type(atom), default(Atom)]
   ).


%% merge_types(+Options, -MergedOptions)
%
% merge compatible types found from different usage cases or descriptions
merge_types(L, [T | LL]) :-
   findall(
      T,
      member(type(T), L),
      Types
   ),
   delete(L, type(_), LL),
   unify_types(Types, T), !.


%% unify_types(+Types, -UnifiedTypes)
%
% merge similar type descriptions, or atom and float into float
unify_types([], type(atom)).

unify_types([T], type(T)).

unify_types([T1, T2 | TT], T) :-
   (
      T1 == T2
   ->
      unify_types([T1 | TT], T)
   ;
      (
         T1 == atom, T2 == float
      ;
         T1 == float, T2 == atom
      )
   ->
      unify_types([float | TT], T)
   ).


%% add_default(+Options, -DefaultOptions)
%
% add a default case for typed options that do not have one already
% FIXME Actually should only be done for optional items
add_default(L, L) :-
   memberchk(default(_), L),
   !.

add_default(L, [default(false) | L]) :-
   memberchk(type(boolean), L),
   !.

add_default(L, [default('') | L]).

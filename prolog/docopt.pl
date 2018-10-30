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
   merge_options(AllOptsSpec, OptsSpec),
   write(OptsSpec),nl.


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
options_to_optparse(OptionDescriptions, OptionSpec) :-
   OptionDescriptions = [Head | Tail],
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


% ignore the grouping, alternative and optional marks
usage_to_optparse(UsageString, UsageSpec) :-
   split_string(UsageString, "\s\t\r", "][)(|.", [_Prog | Usage]),
   maplist(argument_to_optparse, Usage, Specs),
   concat(Specs, UsageSpec).


option_to_optparse(OptionString, OptionSpec) :-
   split_string(OptionString, "", "\s\t\r", [Chomp]),
   (
      sub_string(Chomp, 0, 1, _, "-")
   ->
      option_to_optparse_aux(Chomp, OptionSpec)
   ;
      OptionSpec = []
   ).


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
      sub_string(Argument, 2, LOpt - 2, _, Option),
      sub_string(Argument, LOpt + 1, _, 0, Pos),
      Specs = [[longflags([Option]), pos(Pos)]]
   ;
      sub_string(Argument, 2, _, 0, Option),
      Specs = [[longflags([Option])]]
   ).

argument_to_optparse(Argument, Specs) :-
   sub_string(Argument, 0, 1, _, "-"),
   !,
   % ignore the case where -fFILE means -f FILE
   string_chars(Argument, [_ | Chars]),
   findall(
      [shortflags([Option])],
      (
         member(C, Chars),
         atom_string(C, Option)
      ),
      Specs
   ).

argument_to_optparse("options", []) :-
   !.


option_to_optparse_aux(OptionString, [OptionSpec]) :-
   (
      sub_string(OptionString, LOpt, 2, _, "  ")
   ->
      sub_string(OptionString, 0, LOpt, _, Options)
   ;
      Options = OptionString
   ),
   split_string(Options, "\s\t\r,", "\s\t\r,", OptionList),
   maplist(argument_to_optparse, OptionList, Specs),
   flatten(Specs, OptionSpec).


% no foldr in library(apply)
concat([], []).

concat([H | T], Concat) :-
   append(H, C, Concat),
   concat(T, C).


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
      memberchk(pos(_), H)
   ->
      H1 = [type(atom) | H]
   ;
      H1 = [type(boolean) | H]
   ),
   (
      (
         memberchk(longflags([Name]), H)
      ;
         memberchk(shortflags([Name]), H)
      )
   ->
      Options = [[opt(Name) | H1] | Opts]
   ;
      Options = [H1 | Opts]
   ),
   merge_options(T, Opts).


add_positionals([], []).

add_positionals([H], [H]).

add_positionals([H, [pos(P)] | T], [HH | TT]) :-
   append(H, [pos(P)], HH),
   add_positionals(T, TT).

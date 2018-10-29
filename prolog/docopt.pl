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
   write(UsagePatterns),nl,
   write(OptionDescriptions),nl,
   usages_to_optparse(UsagePatterns, UsageSpec),
   options_to_optparse(OptionDescriptions, OptionSpec),
   append(UsageSpec, OptionSpec, OptsSpec).


%% usages_to_optparse(+UsagePatterns, -UsageSpec)
%
% Gets optparse spec from a list of usage patterns
usages_to_optparse(UsagePatterns, UsageSpec) :-
   maplist(usage_to_optparse, UsagePatterns, Specs),
   flatten(Specs, UsageSpec).


%% options_to_optparse(+OptionDescriptions, -OptionSpec)
%
% Gets optparse spec from a list of option descriptions
options_to_optparse(OptionDescriptions, OptionSpec) :-
   write(OptionDescriptions),nl,
   OptionSpec = [].


usage_to_optparse(UsageString, UsageSpec) :-
   split_string(UsageString, "\s\t\r", "", [_Prog | Usage]),
   maplist(argument_to_optparse, Usage, Specs),
   flatten(Specs, UsageSpec).


% Positional Arguments
argument_to_optparse(Argument, [pos(Argument)]) :-
   string_upper(Argument, Argument),
   !.

argument_to_optparse(Argument, [pos(Argument)]) :-
   sub_string(Argument, 0, 1, _, "<"),
   sub_string(Argument, _, 1, 0, ">"),
   !.

% Options
argument_to_optparse(Argument, Specs) :-
   sub_string(Argument, 0, 1, _, "-"),
   \+ sub_string(Argument, 0, 2, _, "--"),
   !,
   % ignore the case where -fFILE meanse -f FILE
   string_chars(Argument, [_ | Chars]),
   findall(
      [opt(Option, shortflags([Option]))],
      (
         member(C, Chars),
         atom_string(C, Option)
      ),
      Specs
   ).

argument_to_optparse("[options]", []) :-
   !.

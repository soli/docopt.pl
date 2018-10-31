:- module(
  docopt,
  [
    opt_arguments/3,
    opt_parse/4
  ]
).

:- use_module(
   library(optparse),
   [opt_parse/5 as optparse]
).


%% opt_arguments(+OptsSpec, -Opts, -PositionalArgs) is det
%
% Extract commandline options according to a [Docopt](http://docopt.org)
% specification.
% Same conventions as in library(optparse) are followed.
opt_arguments(OptsSpec, Opts, PositionalArgs) :-
   % we need os_argv to get past --
   current_prolog_flag(argv, Args),
   opt_parse(OptsSpec, Args, Opts, PositionalArgs).


%% opt_parse(+OptsSpec, +ApplArgs, -Opts, -PositionalArgs) is det
%
% Parse arguments ApplArgs according to [Docopt](http://docopt.org) spec
% OptsSpec. As in library(optparse), returns Opts as a list of Func(Key,
% Value) options, and PositionalArgs as the remaining arguments.
opt_parse(OptsSpec, ApplArgs, Opts, PositionalArgs) :-
   docopt_to_optparse(OptsSpec, OptparseSpec),
   split_single_dashes(ApplArgs, OptparseSpec, SplitArgs),
   % opt_parse writes some stuff on stderr when it fails parsing
   % current_stream(2, write, UserError),
   % open_null_stream(Null),
   % setup_call_cleanup(
   %    set_stream(Null, alias(user_error)),
      optparse(
         OptparseSpec, SplitArgs, Opts, PositionalArgs,
         [duplicated_flags(keepall)]
      ),
      % set_stream(UserError, alias(user_error))
   % ),
   !,
   (
      ground(Opts)
   ->
      true
   ;
      throw(error(instantiation_error(Opts)))
   ).


%%%%%


%% docopt_to_optparse(+HelpString, -OptsSpec) is det
%
% Transform a [Docopt](http://docopt.org) help-string specification into a
% library(optparse) specification.
docopt_to_optparse(HelpString, OptsSpec) :-
   nb_setval(optional, false),
   nb_delete(progname),
   sub_string(HelpString, Before, 6, _, Usage),
   string_lower(Usage, "usage:"),
   !,
   After is Before + 6,
   sub_string(HelpString, After, _, 0, NoUsage),
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
   merge_options(AllOptsSpec, OptsSpec1),
   add_unambiguous_longflags(OptsSpec1, OptsSpec1, OptsSpec).


%% usages_to_optparse(+UsagePatterns, -UsageSpec) is det
%
% Gets optparse spec from a list of usage patterns
usages_to_optparse(UsagePatterns, UsageSpec) :-
   maplist(usage_to_optparse, UsagePatterns, Specs),
   concat(Specs, Usage),
   add_positionals(Usage, UsageSpec).


%% options_to_optparse(+OptionDescriptions, -OptionSpec) is det
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


%% usage_to_optparse(+UsageString, -UsageSpec) is det
%
% translate one usage example into some optparse specs
% FIXME ignores the grouping, alternative, and multiple marks
usage_to_optparse(UsageString, UsageSpec) :-
   split_string(UsageString, ".|()", "", Strings),
   maplist(atom_string, Atoms, Strings),
   atomic_list_concat(Atoms, ' ', CleanUsageAtom),
   atom_string(CleanUsageAtom, CleanUsageString),
   split_string(CleanUsageString, "\s\t\r", "\s\t\r", [Prog | Usage]),
   (
      nb_current(progname, ProgName)
   ->
      true
   ;
      nb_setval(progname, Prog),
      ProgName = Prog
   ),
   (
      ProgName == Prog
   ->
      maplist(argument_to_optparse, Usage, Specs),
      concat(Specs, UsageSpec)
   ;
      UsageSpec = []
   ).


%% option_to_optparse(+OptionString, -OptionSpec) is det
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


%% argument_to_optparse(+Argument, -Spec) is det
%
% translate one fragment of usage into a partial spec
argument_to_optparse("", []) :-
   !.

argument_to_optparse("[options]", []) :-
   !,
   nb_setval(optional, true).

argument_to_optparse(Argument, Specs) :-
   sub_string(Argument, 0, 1, _, "["),
   !,
   sub_string(Argument, 1, _, 0, Arg),
   nb_setval(optional, true),
   argument_to_optparse(Arg, Specs).

argument_to_optparse(Argument, Specs) :-
   sub_string(Argument, _, 1, 0, "]"),
   !,
   sub_string(Argument, 0, _, 1, Arg),
   argument_to_optparse(Arg, Specs),
   nb_setval(optional, false).

% Positional Arguments
argument_to_optparse(Argument, [[pos(Argument)]]) :-
   string_upper(Argument, Argument),
   !.

argument_to_optparse(Argument, [[pos(Argument)]]) :-
   sub_string(Argument, 0, 1, _, "<"),
   sub_string(Argument, _, 1, 0, ">"),
   !.

% Options
argument_to_optparse(Argument, [Specs]) :-
   sub_string(Argument, 0, 2, _, "--"),
   !,
   (
      nb_getval(optional, true)
   ->
      Opt = [optional(true)]
   ;
      Opt = []
   ),
   (
      sub_string(Argument, LOpt, 1, _, "=")
   ->
      LLOpt is LOpt - 2,
      LLLOpt is LOpt + 1,
      sub_string(Argument, 2, LLOpt, _, Option),
      sub_string(Argument, LLLOpt, _, 0, Pos),
      atom_string(Atom, Option),
      Specs = [longflags([Atom]), pos(Pos) | Opt]
   ;
      sub_string(Argument, 2, _, 0, Option),
      atom_string(Atom, Option),
      Specs = [longflags([Atom]) | Opt]
   ).

argument_to_optparse(Argument, Specs) :-
   sub_string(Argument, 0, 1, _, "-"),
   !,
   (
      nb_getval(optional, true)
   ->
      Opt = [optional(true)]
   ;
      Opt = []
   ),
   % ignore the case where -fFILE means -f FILE
   string_chars(Argument, [_ | Chars]),
   findall(
      [shortflags([C]) | Opt],
      member(C, Chars),
      Specs
   ).


%% option_to_optparse_aux(+OptionString, -OptionSpec) is det
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


%% concat(+Listoflists, -List) is det
%
% append all elements of Listoflists in List
% no foldr in library(apply)
concat([], []).

concat([H | T], Concat) :-
   append(H, C, Concat),
   concat(T, C).


%% merge_options(+Options, -MergedOptions) is det
%
% merge options coming from different usage cases or descriptions
merge_options([], []).

merge_options([H | T], Options) :-
   memberchk(shortflags(F) , H),
   select(O, T, Opts),
   selectchk(shortflags(F), O, OO),
   !,
   append(H, OO, HH),
   merge_options([HH | Opts], Options).

merge_options([H | T], Options) :-
   memberchk(longflags(F) , H),
   select(O, T, Opts),
   selectchk(longflags(F), O, OO),
   !,
   append(H, OO, HH),
   merge_options([HH | Opts], Options).

merge_options([H | T], Options) :-
   (
      memberchk(pos(_), H)
   ->
      delete(H, pos(_), HH),
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


%% add_positionals(+Options, -PosOptions) is det
%
% add following positional arguments to the preceding option in Options
add_positionals([], []).

add_positionals([H], [H]) :-
   !.

add_positionals([H, [pos(P)] | T], [HH | TT]) :-
   !,
   append(H, [pos(P)], HH),
   add_positionals(T, TT).

add_positionals([H | T], [H | TT]) :-
   add_positionals(T, TT).


%% find_default(+DescrString, -Default) is det
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


%% merge_types(+Options, -MergedOptions) is det
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


%% unify_types(+Types, -UnifiedTypes) is det
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


%% add_default(+Options, -DefaultOptions) is det
%
% add a default case for typed options that do not have one already
add_default(L, LL) :-
   memberchk(default(_), L),
   !,
   delete(L, optional(true), LL).

add_default(L, [default(false) | LL]) :-
   memberchk(type(boolean), L),
   memberchk(optional(true), L),
   !,
   delete(L, optional(true), LL).

add_default(L, [default('') | LL]) :-
   select(optional(true), L, _),
   !,
   delete(L, optional(true), LL).

add_default(L, L).


add_unambiguous_longflags([], _, []).

add_unambiguous_longflags([H | T], Opts, [HH | TT]) :-
   selectchk(longflags([F]), H, H1),
   !,
   selectchk(H, Opts, OOpts),
   unambiguous(F, OOpts, L),
   HH = [longflags(L) | H1],
   add_unambiguous_longflags(T, Opts, TT).

add_unambiguous_longflags([H | T], Opts, [H | TT]) :-
   add_unambiguous_longflags(T, Opts, TT).


unambiguous('', _, []) :-
   !.

unambiguous(Atom, Opts, []) :-
   member(O, Opts),
   member(longflags([F]), O),
   sub_atom(F, 0, _, _, Atom),
   !.

unambiguous(Atom, Opts, [Atom | L]) :-
   sub_atom(Atom, 0, _, 1, Prefix),
   unambiguous(Prefix, Opts, L).


split_single_dashes([], _, []).

split_single_dashes([H | T], OptparseSpec, SplitArgs) :-
   atom_chars(H, Chars),
   (
      Chars = ['-' | Options],
      Options = [C, _ | _],
      C \= '-'
   ->
      split_options(Options, OptparseSpec, Args),
      append(Args, OtherArgs, SplitArgs)
   ;
      SplitArgs = [H | OtherArgs]
   ),
   split_single_dashes(T, OptparseSpec, OtherArgs).


split_options([], _, []).

split_options([H], _, [A]) :-
   format(atom(A), '-~w', [H]).

split_options([H1, H2 | T], OptparseSpec, Args) :-
   (
      member(Option, OptparseSpec),
      member(shortflags(L), Option),
      member(H2, L)
   ->
      format(atom(A), '-~w', [H1]),
      Args = [A | OtherArgs],
      split_options([H2 | T], OptparseSpec, OtherArgs)
   ;
      atomic_list_concat([H1, H2 | T], '', O),
      format(atom(A), '-~w', [O]),
      Args = [A]
   ).


%%%%%

% :- set_test_options([load(never)]).
:- begin_tests('testcases.docopt', [setup(get_tests_from_file)]).
:- use_module(library(plunit)).
:- use_module(library(http/json)).


:- dynamic(testcase/3).


get_tests_from_file :-
   retractall(testcase(_, _, _)),
   module_property(docopt, file(File)),
   file_directory_name(File, Directory),
   absolute_file_name('testcases.docopt', TestFile, [relative_to(Directory)]),
   open(TestFile, read, Tests),
   read_stream_to_codes(Tests, Codes),
   close(Tests),
   string_codes(String, Codes),
   split_by_substring(String ,'r"""', '\s\n\r\t', [_ | Usages]),
   maplist(usage_to_test, Usages).


usage_to_test(Usage) :-
   split_by_substring(Usage, '"""', '\s\r\n\t', [Spec, Cases]),
   split_by_substring(Cases, '$', '\s\r\n\t', [_ | TestCases]),
   maplist(testcase_to_io, TestCases, InputOutput),
   forall(
      member((Input, Output), InputOutput),
      assertz(testcase(Spec, Input, Output))
   ).


split_by_substring(String, Substring, Padding, List) :-
   (
      sub_string(String, Before, Len, _, Substring)
   ->
      sub_string(String, 0, Before, _, S1),
      split_string(S1, "", Padding, [S2]),
      New is Before + Len,
      List = [S2 | L],
      sub_string(String, New, _, 0, NewString),
      split_by_substring(NewString, Substring, Padding, L)
   ;
      split_string(String, "", Padding, List)
   ).


testcase_to_io(String, (Input, Output)) :-
   sub_string(String, 4, _, 0, NoProg),
   split_string(NoProg, '\n', '\s\t\r', [InputString | RemainderList]),
   maplist(atom_string, RemainderAtoms, RemainderList),
   atomic_list_concat(RemainderAtoms, ' ', OutputAtom),
   atom_json_term(OutputAtom, JsonOutput, [null(''), true(true), false(false)]),
   json_to_output(JsonOutput, Output),
   split_string(InputString, '\s\t\r', '\s\t\r', InputStrings),
   (
      InputStrings == [""]
   ->
      Input = []
   ;
      maplist(atom_string, Input, InputStrings)
   ).


json_to_output(json(L), Output) :-
   !,
   maplist(equal_to_functor, L, UnsortedOutput),
   sort(UnsortedOutput, Output).

json_to_output(S, S).


equal_to_functor(Name = Value, Term) :-
   (
      sub_atom(Name, 0, 2, _, '--')
   ->
      sub_atom(Name, 2, _, 0, Functor)
   ;
      sub_atom(Name, 0, 1, _, '-')
   ->
      sub_atom(Name, 1, _, 0, Functor)
   ;
      Functor = Name
   ),
   (
      is_list(Value)
   ->
      sort(Value, Argument)
   ;
      Argument = Value
   ),
   Term =.. [Functor, Argument].


map_to_functor(MapString, Term) :-
   split_string(MapString, ":", "\s\t\r\"", [Func, Arg]),
   (
      Arg == "null"
   ->
      Argument = ''
   ;
      atom_string(Argument, Arg)
   ),
   split_string(Func, "", "-", [NoDashFunc]),
   atom_string(Functor, NoDashFunc),
   Term =.. [Functor, Argument].


test(
   'testcases.docopt',
   [
      forall(testcase(Spec, Input, Expected)),
      true(Output == Expected)
   ]) :-
   catch(
      (
         opt_parse(Spec, Input, OutputList, []),
         sort(OutputList, Output)
      ),
      _Error,
      (
         Output = 'user-error'
      )
   ).


:- end_tests('testcases.docopt').

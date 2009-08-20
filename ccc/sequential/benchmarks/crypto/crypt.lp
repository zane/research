%last comments:  riccardo5

%-----------------------------------------------------------------------------
%
%               C^3: Concurrent Contract Checking
%
% Name:         ICFP09lang.tex
% Purpose:      ICFP '09 submission
%
% Author:       Christos Dimoulas
%               Northeastern University
%               chrdimo@ccs.neu.edu
%
% Created:      10 February 2009
%
%-----------------------------------------------------------------------------


\documentclass[preprint]{sigplanconf}

\usepackage{amsmath}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage{amstext}
\usepackage{amssymb}
\usepackage{amsbsy}
\usepackage{url}
\usepackage{alltt}
\usepackage[usenames]{color}
\usepackage[inference]{semantic}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bibliography formating
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\usepackage{natbib}
\bibpunct(); A{},
\let\cite=\citep



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Our macro packages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\usepackage{ndisplay}
\usepackage{ngrammar}
\usepackage{lang}

\allowdisplaybreaks[4]

\newcommand{\In}{\ \mathtt{in}\ }


% ----------------------------------------------------------------- %
% Comments and To-dos (from ADG)
% ----------------------------------------------------------------- %

% Couple of variants
%\newcommand{\displaycomment}[1]{\marginpar{\raggedright\scriptsize{#1}}}  % marginal notes
%\newcommand{\displaycomment}[1]{}                                         % for final version
\newcommand{\displaycomment}[1]{#1 {}}                                     % inlines notes

\usepackage{color}
\definecolor{dkgreen}{rgb}{0,0.4,0}
\definecolor{dkred}{rgb}{0.6,0,0}
\newcommand{\todonote}[2]{% something has to be done!
\displaycomment{{\color{dkred}{\bf#1: }#2}}%
\typeout{TODO: (\thepage) #2}}
\newcommand{\Remark}[2]{% en passant
\displaycomment{{{\color{dkgreen}(#1: #2)}}}}

\newcommand{\RIC}[1]{\todonote{Ric}{#1}}
\newcommand{\ric}[1]{\Remark{Ric}{#1}}



%riccardo1: more macros - feel free to move them somewhere else
\newcommand{\conpar}{\ensuremath{\lambda^{\mathit{ccc}}}}
\newcommand{\conparseq}{\ensuremath{\lambda^{\mathit{ccc}}_{\mathit{seq}}}}


\begin{document}

\conferenceinfo{}{} 
\copyrightyear{2009} 
\copyrightdata{[to be supplied]} 

                                      
\title{Future Contracts\thanks{This reseach was partially supported by a
grant from the US AFOSR}}

\authorinfo{{Christos Dimoulas}
             \and 
             {Riccardo Pucella}
             \and 
             {Matthias Felleisen}}
             {Northeastern University}
             {\{chrdimo,riccardo,matthias\}@ccs.neu.edu}
             
\maketitle

\begin{abstract}
Many recent research projects focus on language support for behavioral
software contracts, that is, assertions that govern the boundaries  
between software building blocks such as procedures, classes, or modules.
Contracts primarily help locate bugs in programs, but they also tend
to affect the performance of the program, especially as they become
complex.
  
In this paper, we introduce \cfont{future} contracts and parallel  
contract checking: software contracts annotated with
\cfont{future} are  
checked in parallel with the main program, exploiting the now-common
multiple-core architecture. We present both a model and a
prototype implementation of our language design. Our model comprises a
higher-order imperative 
language and we use it to prove the correctness of our design.
Our implementation is robust enough to  measure  the performance of reasonably
large benchmarks, demonstrating that the use of \cfont{future} 
contracts can lead to
significant performance  improvements.
\end{abstract}

\category{D.3.3}{Programming Languages}{} 
\terms Programming Languages; Design; Reliability
\keywords
contracts, higher-order functions, behavioral specifications, 
 software reliability

\section{Introduction}

Programmers frequently insert assertions into their programs to ensure that at
%riccardo4: we haven't talk about machine states yet...
%  various points during the execution, the machine state satisfies some
 various points during the execution, the program state satisfies some
 important logical property. For multi-component systems, assertions have
 become a popular tool to express constraints on the interface between two
%riccardo4: what given type system? also ``can not'' -> ``cannot''
%  components that the given type system can not express. When used in this
 components that a static type system cannot express. When used in this
 context assertions are called \emph{behavioral software contracts\/} or just
 contracts. While \citet{p:modules} introduced contracts as early as
 1972, they only became popular with Eiffel~\cite{m:oo-soft-constr,
%riccardo4: 
%  m:eiffel}. After that, contracts were introduced in many other 
 m:eiffel}. Later, contracts were introduced on many other 
 platforms~\cite{dh:contract-java-handshake,gsvk:sather,hmrc:turing,
 khb:jcontractor, kr:blue, k:icontract,lvh:anna,pp:contract-c++,r:assertions}.  
 Most recently, \citet{ff:ho-contracts} have introduced contracts into the world of
 higher-order functional programming languages.

%riccardo4: rewrote
% Naively speaking, contracts monitor the flow of values across contract
%  boundaries.  If the constraints are satisfied, contracts remain
%  invisible. If not, the contract monitoring system signals a contract
Naively speaking, contracts monitor the flow of values across component
 boundaries.  Contracts remain invisible as long as the constraints
 they express are satisfied. When the constraints are not satisfied,
 the contract monitoring system signals a contract
 violation, pinpointing the violation's origin, explaining how the
%riccardo4: typos
%  contract is violated, and more importantly assigning blame to the the 
 contract is violated, and mst importantly assigning blame to the
 violating component.  Blame assignment helps programmers isolate errors and
 gets them started with their debugging efforts.

Unfortunately, monitoring contracts tends to impose a significant run-time
 overhead on the program's execution, especially for defensive programmers
%riccardo5:
%  writing precise contracts. In the worst case, contract monitoring can
 wanting to write precise contracts. In the worst case, contract monitoring can
 affect the algorithmic complexity of a function even for careful programmers,
 although \citet{fgr:lazy-contracts} have recently demonstrated how
 to overcome this problem in many cases. In the average case, contract
 monitoring consumes a substantial, though constant amount of time and
 space. As a result, many programmers turn off contract monitoring or relax
 contracts so that they monitor fewer properties than desirable.

%riccardo5: rewrote slightly
%  We asked PLT Scheme users for applications
%  with complex contracts. The feedback we got is that programmers do
%  not add the contracts they would like but settle
%  for much lighter contracts. In some cases, contracts are used only in
%  the debugging phase and then they are removed  to avoid the
%  contract checking overhead. The fear of programmers of time penalty due
%  to contracts leads them to write less robust software.
We have gathered anecdotal evidence for this last
observation. 
A survey of PLT Scheme users about whether they wrote applications
with complex contracts confirmed that programmers do not generally use
the contracts they would like, but rather settle for much lighter
contracts. 
In fact, contracts are often used only in the debugging phase of software
development and then, to avoid contract-checking
overhead, they are removed from production code, exactly where
contracts are at their most useful.
Thus, programmers' fear of the run-time cost of contract checking
leads to less reliable software.

Multi-core architectures suggest an obvious way to address this problem,
%riccardo4:
%  namely, monitoring contracts in parallel to the program execution. Since
 namely by monitoring contracts in parallel with program execution. Since
 software contracts are usually functional computations---even in
 imperative languages---running them in parallel should be easy and should
 save some of the cost of evaluating them. 
%riccardo4: rewrote this bit
% Some simple experimentation
%  demonstrates, however, that such a brute-force approach is too naive. It
%  underestimates the communication and neglects the cost of synchronization
%  due to the presence of (hopefully, rare) effects in the main program and
%  on occasion in contracts. In this context, the main practical issue is
%  choosing the contracts that are better fitted for parallel
%  checking. 
Because communication between threads is not free, evaluating every single 
contract in parallel is not cost effective. Indeed simple experiments validate this intuition and show that evaluating every contract in parallel lead to 
a slow down in execution time. This impact is worsened by the presence of 
effects in
the main program and in contracts, which require synchronization
between the main program thread and the contract monitoring thread. 

The question, then, is how to choose the contracts that 
benefit from parallel evaluation.
%riccardo4: rephrased awkwardnesses
%   (in particular we don't want to emphasize ``future value'',
%   because we never personally use the notion in the paper)
% We therefore introduce the notion of \emph{future contracts} inspired by
%  Halstead's \cite{h:future} future annotations. Following Halstead's work,
%  the annotation \cfont{future} on a contract indicates that a contract
%  should be checked in parallel with the rest of the program; all other
%  contracts are executed in-line. While Halstead's future expressions
%  immediately produce a \emph{future value}, a \cfont{future} contract
To help answer this question, we introduce the notion of \emph{future
  contracts} inspired by Halstead's \citeyearpar{h:future} future
construct. Following Halstead's work, 
 the annotation \cfont{future} on a contract indicates that a contract
 should be checked in parallel with the rest of the program;
 unannotated contracts are executed in-line. While Halstead's future expressions
 immediately produce a future value, a \cfont{future} contract
 produces nothing. Instead, the main thread sends such a contract to the
 monitoring thread and proceeds until it must perform an observable
 operation (e.g., a side-effect or an I/O operation). At that point, the
 main thread waits to ensure that the contract succeeds. If so, the
 computation proceeds; otherwise the program terminates with a contract
 exception. 
 
The introduction of \cfont{future} contracts into a mostly functional
 language poses both theoretical and practical challenges. Given the
 critical nature of software contracts in the development process, we
 naturally wish to ensure that parallel contract execution does not
 undermine contract monitoring. 
%riccardo4: added (reminder that this kind of reasoning is kindda
%         important) 
This is especially important here since we are potentially adding
concurrency to the language, and ensuring correctness of concurrent
programs is well-known to be subtle.
We therefore develop a model of parallel
 contract execution for a higher-order and imperative programming language
 and prove that it is semantically equivalent to sequential contract
 monitoring, including blame assignment.


\begin{figure*}
$
\begin{array}{lcll}
 \mathbf{Programs}
 & \cPr & ::= & \cdelta \In \cE  \\
%riccardo2: precise description
% \mathbf{Syntax\,\, Elements}
 \mathbf{Declarations}
 & \cdelta & ::= & \cD \ldots \cD\\
 & \cD & ::= & \cvalrec{x}{\cc}{\cval}\\
 \mathbf{Atomic~Expressions}
 & w & ::= & x  \nsor \cval\\
 \mathbf{Expressions}
%riccardo3: spread on two lines instead of 3
 & \cE & ::=    & w \nsor \capp{w}{w} 
                  \nsor \clet{x}{\cE}{\cE} 
                  \nsor w\,\,\caop\,\,w \nsor 
                  w\,\,\crop\,\,w \nsor \cfont{string?}(w)
                  \nsor \ccons{w}{w} \nsor  \\
 &     &  &  \chd{w}
                  \nsor \ctl{w} \nsor \cmt{w} 
                   \nsor \cif{w}{\cE}{\cE} \nsor \csend{\cE}{\cE} \nsor 
                   \creceive{w}{\cE} \nsor \\
 &     &   &      \coblig{w}{\cc}{\cstr}{\cstr} \nsor
                  \cblame{\cstr} \\
%riccardo2: added spaces to make the diagram breathe a bit
 \mathbf{Relational~Operators}\qquad\quad
 &  \crop & ::= & \cfont{=} \nsor \cfont{\geq}\\
 \mathbf{Arithmetic~Operators}
 & \caop & ::= & \cfont{+} \nsor \cfont{*} \nsor \cfont{-} \nsor \cfont{/}\\
 \mathbf{Variables}
 & x     & \in & \mathit{Variables} \\
 \mathbf{Constants}
& \ci   & \in & \mathit{Integers} \\ 
 & \cstr & \in & \mathit{Strings}\\
 \mathbf{Evaluation\,\, Contexts} 
 & \cFC & ::= &\chole \nsor \clet{x}{\cFC}{\cE}\\ 
 & \cEC & ::= &\cFC \nsor \clet{x}{\cEC}{\cE} \nsor \csend{\cEC}{\cE}\\
  \mathbf{Values}&
    \cval &  ::= & \cflambda{x}{\cE} 
    \nsor \cfsigma{\ci} \nsor \cstr \nsor \ctrue \nsor \cfalse \nsor
                  \cnil \nsor \ccons{\cval}{\cval} \nsor
                 \coblig{\cval}{\cfuncontract{\cc}{\cc}}{\cstr}{\cstr}\\
   \mathbf{Contract\,\, Values}&
   \cc & ::= & \cflatcontract{\cval} \nsor \cfuncontract{\cc}{\cc} 
   \nsor \cfuturecontract{\cc}
\end{array}
$
%riccardo2: simplified
%\caption{Values, Syntax and, Evaluation Contexts and States}
\caption{Syntax of $\con$}
\label{syntax-simple}
\end{figure*}

Of course, a theoretical result is interesting only when it is backed up with
 an implementation that confirms the practical part of the conjecture. We
 have implemented our model in the context of PLT Scheme's contract system.
%riccardo5: 
%  The key element of our implementation is a carefully crafted communication
 The core of our implementation is a carefully crafted communication
 infrastructure, ensuring that our prototype is sufficiently efficient to conduct
 a number of performance tests. These tests confirm the usefulness of
 \cfont{future} contracts, and  help us formulate some
 relatively straightforward guidelines concerning the use of \cfont{future}
 contracts that we hope will encourage the wide use of comprehensive contracts 
 in software development.

In the next section we introduce a model of parallel contract checking in
 a higher-order imperative and parallel world. Then we proceed to present the
 correctness theorem, the design of the communication architecture, the
 prototype implementation, and a set of benchmarks that validate our
 conjectures concerning the performance improvements of contract
 monitoring.

 Reasonably complete proof sketches of our results appear in a
 separately submitted appendix. 

%riccardo4: 
%  \section{\con ~A Functional Language with Contracts}
 \section{\con: A Functional Language with Contracts}
\label{s:lambda-con}

\newcommand{\Pos}{\mathit{pos}}
\newcommand{\Neg}{\mathit{neg}}



\begin{figure*}[t]
\center
$
\begin{array}{cclllll}
  \cdelta & \cholds & 
    \cfsigma{j_1} \,\, \cfont{/}\,\,  \cfsigma{0}&
    \cstep &
%riccardo3: I suspect we want an actual error here...
%      \cerror{\dq{x}}
     \cerror{\dq{/0}}
\\
\cdelta & \cholds &
{
    \cfsigma{j_1} \,\, \cfont{+}\,\,  \cfsigma{j_2}}&
    \cstep &
    \cfsigma{j_1 + j_2}
\\
 \cdelta & \cholds &   
    \cfsigma{j_1} \,\, \cfont{*}\,\,\cfsigma{j_2}&
     \cstep &
    \cfsigma{j_1 * j_2}
\\
\cdelta & \cholds &
      \cfsigma{j_1} \,\, \cfont{-}\,\,  \cfsigma{j_2}&
      \cstep &
      \cfsigma{j_1 - j_2}
\\ 
\cdelta & \cholds &
     \cfsigma{j_1} \,\, \cfont{/}\,\,  \cfsigma{j_2}&
       \cstep &
     \cfsigma{j_1 / j_2}
\\
\cdelta & \cholds &
     \cfsigma{j_1} \,\, \cfont{\geq}\,\,\cfsigma{j_2}&
       \cstep &
     \ctrue

\ntab[4]\,\,\, \text{if $j_1 \geq j_2$}
\\
\cdelta & \cholds &
     \cfsigma{j_1} \,\, \cfont{\geq}\,\,\cfsigma{j_2}&
       \cstep &
    \cfalse
          
\ntab[4] \text{if $j_1 < j_2$}              
\\
\cdelta & \cholds &
      \cfsigma{j_1} \,\, \cfont{=}\,\,\cfsigma{j_2}&
       \cstep &
      \ctrue
 \ntab[4]\,\,\, \text{if $j_1 = j_2$}              
\\
\cdelta & \cholds &
       \cfsigma{j_1} \,\, \cfont{=}\,\,\cfsigma{j_2}&
       \cstep &
       \cfalse
 \ntab[4] \text{if $j_1 \neq j_2$}              
\\
\cdelta & \cholds &
       \cfont{string?}(\cval)&
       \cstep &
       \ctrue
 \ntab[4]\,\,\, \text{if $\cval \in \cstr$}              
\\
\cdelta & \cholds &
       \cfont{string?}(\cval)&
       \cstep &
       \cfalse
 \ntab[4] \text{if $\cval \notin \cstr$}              
       \\


\cdelta & \cholds &
       \capp{\cflambda{x}{\cE}}{\cval}&
       \cstep &
       \cE[x/\cval]
\\
\cdelta & \cholds &
       \clet{x}{\cval}{\cE}&
       \cstep &
       \cE[x/\cval]
\\
\cdelta & \cholds &
      x&
     \cstep&    
     \cval
%riccardo3:  trying to allight with if's above
% \ntab[4] \text{if $(\cvalrec{x}{\cc}{\cval}) \in \cdelta$} 
\ntab[5.6] \text{if $(\cvalrec{x}{\cc}{\cval}) \in \cdelta$}
\\
\cdelta & \cholds &
      \cif{\ctrue}{\cE_1}{\cE_2}&
      \cstep&
      \cE_1
\\ 
\cdelta & \cholds &
      \cif{\cfalse}{\cE_1}{\cE_2}&
      \cstep&
      \cE_2
\\
\cdelta & \cholds &
      \cmt{\cnil}&
      \cstep &
      \ctrue
\\
\cdelta & \cholds &
      \cmt{\ccons{\cval_1}{\cval_2}}&
      \cstep &
      \cfalse
\\
\cdelta & \cholds &
      \chd{\cnil}&
      \cstep &
      \cerror{\dq{hd}}
\\
\cdelta & \cholds &
      \chd{\ccons{\cval_1}{\cval_2}}&
      \cstep &
      \cval_1
\\
\cdelta & \cholds &
      \ctl{\cnil}&
      \cstep &
      \cerror{\dq{tl}}
\\
\cdelta & \cholds &
      \ctl{\ccons{\cval_1}{\cval_2}}&
      \cstep &
      \cval_2
\\
\cdelta & \cholds &
      \cblame{\cstr}&
      \cstep &
      \cerror{\cstr}
\\
  \cdelta & \cholds &
   {\coblig{\cval_1}{\cflatcontract{\cval_2}}{\Pos}{\Neg}} &
      \cstep & 
   {\clet{x}{\capp{\cval_2}{\cval_1}}{\cif{x}{\cval_1}{\cblame{\Pos}}}}
 \\ 
  \cdelta & \cholds &
        
    \capp{\coblig
                               {\cval_1}
                               {\cfuncontract{\cc_1}{\cc_2}}{\Pos}{\Neg}}
                               {\cval_2} &
     \cstep  &
    {\clet{x}{\coblig{\cval_2}{c_1}{\Neg}{\Pos}}{
     \clet{y}{\capp{\cval_1}{x}}{\coblig{y}{c_2}{\Pos}{\Neg}}}}
\\
  \cdelta & \cholds &
  {\coblig{\cval_1}{\cfuturecontract{\cflatcontract{\cval_2}}}{\Pos}{\Neg}} &
      \cstep & 
   {\csend{\clet{x}{\capp{\cval_2}{\cval_1}}{\cif{x}{\cval_1}{\cblame{\Pos}}}}
          {\cval_1}}
 \\ 
  \cdelta & \cholds &
        
    \capp{\coblig
                               {\cval_1}
                               {\cfuturecontract
                               {\cfuncontract{\cc_1}{\cc_2}}}{\Pos}{\Neg}}
                               {\cval_2} &
     \cstep  &
     {\clet{x}{\coblig{\cval_2}{\cfuturecontract{c_1}}{\Neg}{\Pos}}{
     \clet{y}{\capp{\cval_1}{x}}{\coblig{y}
            {\cfuturecontract{c_2}}{\Pos}{\Neg}}}}\\

   \end{array} 
$
%riccardo2: 
% \caption{Reduction Rules for Local Evaluation}
\caption{Reduction rules for local evaluation}
\label{local-sem-rules}
\end{figure*}

\begin{figure*}
\center
$
\begin{array}{cclclll}
     \cdelta & \cholds &
      \ccontext{\cEC}{\cE} \DPar \ctrace &
      ^{i}\ccstep &
      \ccontext{\cEC}{\cE^\prime} \DPar \ctrace
      && \text{where $\crule{\cdelta \cholds \cE}{}
      { \cE^\prime}{}$}\\
      \cdelta & \cholds &
      \ccontext{\cEC}{\csend{\cval}{\cE}} \DPar \ctrace &
      ^{i}\ccstep &
      \ccontext{\cEC}{\cE} \DPar \ctrace \\
      \cdelta & \cholds &
      \ccontext{\cEC}{\creceive{\cval}{\cE}} \DPar \ctrace &
      ^{i}\ccstep &
      \ccontext{\cEC}{\cE} \DPar \cexttrace{\ctrace}{\cval}
      && \text{if $\csize{\ctrace} < i$}\\
      \cdelta & \cholds &
      \ccontext{\cEC}{\creceive{\cval}{\cE}} \DPar \ctrace&
      ^{i}\ccstep &
      \top \DPar \ctrace
      && \text{if $\csize{\ctrace} = i$}\\
%riccardo2: used \str instead of "x"
%       \cdelta & \cholds &
%       \ccontext{\cEC}{\cE} \DPar \ctrace&
%       ^{i}\cstep &
%       \cerror{\dq{x}} \DPar \ctrace
%       && \text{if $\crule{\cdelta \cholds \cE}{}
%                                         {\cerror{\dq{x}}}{}$}\\
      \cdelta & \cholds &
      \ccontext{\cEC}{\cE} \DPar \ctrace&
      ^{i}\ccstep &
      \cerror{\cstr} \DPar \ctrace
      && \text{where $\crule{\cdelta \cholds \cE}{}
                                        {\cerror{\cstr}}{}$}\\
  \end{array} 
$

      
%riccardo2: 
% \caption{Reduction Rules for Computation Propagation and
% {\cfont{output}}, {\cfont{check}}, {\cfont{new}}, {\cfont{set}} and {\cfont{get}} Operators}
\caption{Reduction rules for computation propagation,
{\cfont{output}}, and {\cfont{check}}}
\label{seq-sem-rules}
\end{figure*}


%riccardo4: removed the space between the period and the footnote. It
%     shows in the output
The language of $\con$ is an A-normal form
$\lambda$-calculus.
%\footnote{Recall that A-normal form enforces that
%  function arguments are always values or variables, and all
% intermediate results are bound to variables.} %\cite{fsdf:essence}
%riccardo4: 
% We also introduce observable side effects through the use of an output
The language includes observable side effects through the use of an output
operator. We generalize effects to store-manipulation effects in
section~\ref{s:effects-sync},
%riccardo3: 
% but most of the subtleties we want to explore already arise for simple
% output effects.
but most subtleties already arise for simple output.


Figure~\ref{syntax-simple} presents the syntax of $\con$.  A program $D\In\cE$
consists of a sequence of recursive function definitions 
$\cdelta$ and a main
expression $\cE$. Intuitively, the functions in $\cdelta$ are the
functions that can be used in $\cE$, and those functions are protected
(from each other) by contracts on the
functions' arguments and return values. 

%riccardo3: rewrote slightly
% For simplicity, our language does not come with a type system but 
% we assume that we only deal with well-typed programs. Adding a type system
% to enforce this constraint is straightforward.
The language is typed, but to simplify the presentation in this paper, 
we do not present
the type system,
%riccardo4: added
which is completely standard. 
%riccardo5: 
% We instead assume that we are given only well-typed
% programs with respect to a standard type system.
We simply assume that we are given only 
programs that are well-typed with respect to a standard type system.


Contracts guard the flow of values. Flat contracts of the form
$\cflatcontract{\cval}$, where $\cval$ is a Boolean-valued function, 
guard first-order values. A value $u$ satisfies the
contract if the contract computation $\capp{\cval}{u}$ 
~yields $\ctrue$. Higher-order contracts of the form
$\cfuncontract{\cc_1}{\cc_2}$ guard functions; the domain contract
$\cc_1$ enforces that the argument to the function satisfies specified
properties, while the range contract $\cc_2$ enforces constraints for the
result of the function. 
%riccardo3: 
% The contract computation is implemented using the calculus proper, and
% therefore we have the full flexibility of the calculus to express the
% properties we want values to satisfy.

 The following  examples provide a taste of  contracts:
%riccardo5:
%   $$ sqrt :\cfuncontract{\cflatcontract{\cflambda{x}
%                                         {x \cfont{>} 0}}}
%                         {\cflatcontract{\cflambda{x}
%                                         {x \cfont{>} 0}}}
%   $$
% 
\[ sqrt :\cfuncontract{\cflatcontract{\cflambda{x}
                                        {x \cfont{\ge} \cfsigma{0}}}}
                        {\cflatcontract{\cflambda{x}
                                        {x \cfont{\ge} \cfsigma{0}}}}
\]

  \noindent
%riccardo5: added
where $\cfsigma{i}$ is the integer literal representation of integer
  $i$ in \con.
  That is, $sqrt$ is a function that computes the square root of
  non-negative numbers. Its contract guarantees that its inputs
  and results are at least $0$. Here is a second example:
%riccardo5:
% 
%   $$ encode :\cccfuncontract{
%            (\cfuncontract{\cflatcontract{prime?}}
%                         {\cflatcontract{prime?}})}
%                         {(\cfuncontract{\cflatcontract{\cfont{string?}}}
%                         {\cflatcontract{\cfont{string?}}})}
% $$       
\[ encode :\cccfuncontract{
           (\cfuncontract{\cflatcontract{prime?}}
                        {\cflatcontract{prime?}})}
                        {(\cfuncontract{\cflatcontract{string?}}
                        {\cflatcontract{string?}})}
\]
    \noindent
     That is, 
     $encode$ is a higher-order function that expects a function from
     prime
     numbers to prime numbers, and that returns a another function from
     strings to strings.

The key innovation of $\con$ is the  contract combinator
%riccardo4: simplified
% $\cfuturecontract{\cc}$, which is used to indicate that the contract
$\cfuturecontract{\cc}$, which indicates that the contract
$\cc$ should be checked in parallel with the 
main program. 
%riccardo4: cut - we already say that the intro, and it doesn't
%  contribute anything here
% In contrast to Halstead's \cfont{future}, \cfont{future} contract does not
% return a value. 
Semantically, $\cfuturecontract{\cc}$ is taken to be equivalent to
$\cc$. 

%riccardo4: 
% Contracts are enforced at run time. To indicate that a value
Contracts are enforced at run time. To record that a value
is guarded by a contract during execution, we use \emph{obligation}
expressions of the form $\coblig{w}{\cc}{\Pos}{\Neg}$. 
Superscripts $\Pos$ and $\Neg$ are critical for contracts guarding
functions; they enable the contract checking monitor to
properly assign blame in the case of contract failure. 
The first superscript---the positive blame position---captures the
party responsible for the values produced by a function, while the
second superscript---the negative blame position---captures the party
responsible for the values provided to the function. 
The responsible party for a contract
failure can be identified by carefully updating these
superscripts as an obligation flows through a program.

%riccardo4: consistency, and rewrote
% Expressions in $\con$ are standard for an A-normal form lambda
% calculus. We add two new forms.
Expressions in $\con$ are standard for an A-normal form
$\lambda$-calculus, with the addition of two new expression forms.
Expression $\creceive{\cval}{\cE}$ outputs value $\cval$ before
continuing as $\cE$. 
Expression $\csend{\cE_1}{\cE_2}$ is an explicit contract checking
expression: $\cE_1$ is an expression representing a contract
that must be checked before expression $\cE_2$ evaluates. 
The result value of $\cE_1$ is discarded---if the contract
does not evaluate to $\ctrue$, then it reports an error that
terminates the program. This explicit contract checking construct enables the
prallel checking of the contract; in this section, however, 
it serves only as a sequencing operator.



%\begin{figure*}
%  $
%\begin{array}[t]{l}
%\ftrans \thastype \tarrow{\mathit{Program}}
%{\mathit{Program}} \\[9pt]
%\ftrans(\cdelta = \cvalrec{x_1}{\cc_1}{\cval_{1}}
%                \ldots
%                \cvalrec{x_n}{\cc_n}{\cval_n} \Vdash
%               \cE) 
%                 = 
%          \\
%          \begin{array}[t]{l}
%              \begin{array}[t]{l}
%                 \cvalrec{x_1}{\ftrans_h(\cdelta,
%                                      x_1,
%                                      \emptyset, \cc_1)}
%                                    {\ftrans_h(\cdelta,
%                                      x_1,
%                                      \emptyset, \cE_1)}\\
%                      \ldots                 \\
%         \cvalrec{x_n}{\ftrans_h(\cdelta,
%                                      x_n,
%                                      \emptyset,\cc_n)}
%                                    {\ftrans_h(\cdelta,
%                                      x_n, 
%                                      \emptyset, \cE_n)} \Vdash \\[1em]
%        \ftrans_h(\cdelta,main,\emptyset,\cE) 
%     \end{array}                               
% \end{array}
%   \end{array}
%$\\[2em]
%
%$
%\begin{array}[t]{l}
%  \ftrans_h  \thastype  \tarrow{\mathit{Declarations} \times
%  \mathit{Variable} \times
%  \mathit{Variables}   
%                                 \times (Expression\cup Contract)}
%                                 {Expression \cup Contract}\\[9pt]
%\ftrans_h(\cdelta,n,V,\cflambda{x}{\cE}) =  
%               \cflambda{x}{\ftrans_h(\cdelta,n,V \cup {x},\cE)}\\
%               \ftrans_h(\cdelta,n,V,\capp{\cval_{p1}}{\cval_{p2}}) =  
%               \capp{\ftrans_h(\cdelta,n,V,\cval_{p1})}
%                    {\ftrans_h(\cdelta,n,V,\cval_{p2})}  \\
%\ftrans_h(\cdelta,n,V,\clet{x}{\cE_1}{\cE_2}) =  
%                 \clet{x}{\ftrans_h(\cdelta,n,V,\cE_1)}
%                          {\ftrans_h(\cdelta,n,V \cup\{x\},\cE_2)}\\
%\ftrans_h(\cdelta,n,V,x) = \begin{cases}
%    \coblig{x}{\cc}{x}{n} & \text{if $x \notin V $ and
%                                     $(\cvalrec{x}{\cc}{\cval}) \in
%                                     \cdelta$}\\
%     x & \text{otherwise}
%			   \end{cases} \\      
%         \ftrans_h(\cdelta,n,V,\cval_{p1}\,\,\caop\,\,\cval_{p2}) =  
%         {\ftrans_h(\cdelta,n,V,\cval_{p1})}\,\, \caop \,\,
%         {\ftrans_h(\cdelta,n,V,\cval_{p2})}  \\
%         \ftrans_h(\cdelta,n,V,\cval_{p1}\,\,\crop\,\,\cval_{p2}) =  
%         {\ftrans_h(\cdelta,n,V,\cval_{p1})}\,\, \crop \,\,
%         {\ftrans_h(\cdelta,V,s,\cval_{p2})}  \\
%\ftrans_h(\cdelta,n,V,\cnil) = \cnil  \\
%\ftrans_h(\cdelta,n,V,\ccons{\cval_{p1}}{\cval_{p2}}) =  
%       \ccons{\ftrans_h(\cdelta,n,V,\cval_{p1})}
%               {\ftrans_h(\cdelta,n,V,\cval_{p2})}  \\
%\ftrans_h(\cdelta,n,V,\chd{\cval_p}) =  
%               \chd{\ftrans_h(\cdelta,n,V,\cval_p)}  \\
%\ftrans_h(\cdelta,n,V,\ctl{\cval_p}) =  
%               \ctl{\ftrans_h(\cdelta,n,V,\cval_p)}  \\
%\ftrans_h(\cdelta,n,V,\cif{\cval_p}{\cE_{1}}{\cE_{2}}) =  
%               \cif{\ftrans_h(\cdelta,n,V,\cval_p)}
%               {\ftrans_h(\cdelta,n,V,\cE_{1})}
%               {\ftrans_h(\cdelta,n,V,\cE_{2})}\\
%\ftrans_h(\cdelta,n,V,\cstr) = \cstr  \\
%\ftrans_h(\cdelta,n,V,\ctrue) = \ctrue  \\
%\ftrans_h(\cdelta,n,V,\cfalse) = \cfalse  \\
%\ftrans_h(\cdelta,n,V,\cflatcontract{\cval}) =  
%               \cflatcontract{\ftrans_h(\cdelta,n,V,\cval)}  \\
%\ftrans_h(\cdelta,n,V,\cfuncontract{\cc_1}{\cc_2}) =  
%           \cfuncontract{\ftrans_h(\cdelta,n,V,\cc_1)}
%                        {\ftrans_h(\cdelta,n,V,\cc_2)}  \\
%\ftrans_h(\cdelta,n,V,\cblame{\dq{x}}) =  
%                       \cblame{\dq{x}}  \\
%\ftrans_h(\cdelta,n,V,\coblig{\cval_p}{\cc}{pos}{neg}) =  
%                  {\coblig
%                       {\ftrans_h(\cdelta,n,V,\cval_p)}
%                        {\ftrans_h(\cdelta,n,V,\cc)}
%                        {pos}{neg}}\\
%\ftrans_h(\cdelta,n,V,\csend{\cval_{p}}{\cE}) =  
%                  {\csend
%                  {\ftrans_h(\cdelta,n,V,\cval_{p})}
%                  {\ftrans_h(\cdelta,n,V,\cE)}}\\     
% \ftrans_h(\cdelta,n,V,\creceive{\cval_{p}}{\cE}) =  
%                  {\creceive
%                  {\ftrans_h(\cdelta,n,V,\cval_{p})}
%                  {\ftrans_h(\cdelta,n,V,\cE)}}\\                            
%\ftrans_h(\cdelta,n,V,\cnew{\cval_p}) =  
%           \cnew{\ftrans_h(\cval_p,n,V,\cval_p)}\\           
%\ftrans_h(\cdelta,n,V,\cset{\cval_{p1}}{\cval_{p2}}{\cE}) =  
%                  {\cset
%                  {\ftrans_h(\cdelta,n,V,\cval_{p1})}
%                  {\ftrans_h(\cdelta,n,V,\cval_{p2})}
%                  {\ftrans_h(\cdelta,n,V,\cE)}}\\           
%\ftrans_h(\cdelta,n,V,\cbang{\cval_{p}}) =  
%                  \cbang{\ftrans_h(\cdelta,n,V,\cval_{p})}              
%\end{array} 
%$ 
%\caption{Obligation Expression Injection}
%\label{oblig-inj}
%\end{figure*}
%


%riccardo3: rewrote
% Any of the function identifiers declared in $\cdelta$ can appear in
% the main expression $\cE$. To enforce contract checking, there is a
% rewriting phase prior to the evaluation of a program that enforces contract
% monitoring of functions defined in $\cdelta$.  This
% whole-program transformation is a homomorphic transformation that
% simply carries expressions to themselves, with the exception of
% references to a function identifier $f$ bound in $\cdelta$, which are
% transformed to obligations where the reference to $f$ is guarded by
% the higher-order contract associated with $f$ in $D$. The positive
% blame position of the obligation is simply the name $f$ of the
% function, while the negative blame position is the name of the caller.
% (The details of the transformation can be found in FF.)
The function identifiers declared in $\cdelta$ may be used in the main
%riccardo4: 
% expression $\cE$ or other parts in $\cdelta$. 
expression $\cE$ or functions in $\cdelta$. 
To force the contracts associated with each such
function identifier to be checked at runtime, 
we compile the program by replacing every function identifier
$f$ appearing in $\cE$ or other parts in $\cdelta$
by an obligation where the
reference to $f$ is guarded by the contract associated
%riccardo4: 
% with $f$ in $D$. This compilation process allows us to tag appropriately
with $f$ in $D$. This compilation process allows us to appropriately tag
each obligation with blame labels \cite{ff:ho-contracts}. The positive blame position of the obligation is
simply the name $f$ of the function, while the negative blame position
is the name of the caller.  

%chrdimo: removed. it is described in the prose
%\begin{figure}
%$
%\begin{array}{lcll}
% \mathbf{Output Trace}
%& \ctrace & ::= & \cmttrace \nsor \cexttrace{\ctrace}{\cval}\\
%\mathbf{States}
% & \cP_o & ::= & \cE \DPar \cmttrace\\
% & \cP   & ::= & \cE \DPar \ctrace 
%                  \nsor \cerror{\dq{x}} \DPar  \ctrace
%                  \nsor \top \DPar \ctrace \\
% & \cP_f & ::= & \cval \DPar \ctrace 
%                  \nsor \cerror{\dq{x}} \DPar  \ctrace
%                  \nsor \top \DPar \ctrace \\
%\end{array}
%$
%\caption{Output Trace and States}
%\label{states-simple}
%\end{figure}

%riccardo4: 
% Our goal is to design the semantics of our language in such a way that
% allows us to describe precisely the effectful behavior of programs. 
Our goal now is to design the semantics of our language so that we can
describe precisely the effectful behavior of programs. 
We are especially interested in nonterminating programs that produce
infinitely many outputs, such as infinite loops with output statements
in the body of the loop.

Anticipating the development in the following sections, we define
%riccardo4: 
% a two-level reduction relation defining the execution of programs: a
a two-level reduction relation representing the execution of programs: a
local reduction relation 
%riccardo4: added
$\rightarrow$ 
regulating the evaluation of pure
expressions, and a global reduction relation 
%riccardo4: added
$\Rightarrow$
regulating the evaluation of effectful expressions. 
This is similar to the semantics of CML~\cite{r:CML} and other
%riccardo4:
% concurrent languages, and is useful for introducing concurrency. 
concurrent languages.

The local reduction relation $\cdelta\cholds e\cstep e'$ 
(with $\cdelta\cholds e\cstep \cerror{str}$) regulates the evaluation of simple
expressions not involving output or explicit contract checking. 
The rules appear in figure~\ref{local-sem-rules}. 
%riccardo5: cut - might as well add \cfsigma's in our examples,
%   since we have only one.
% An integer $j$ is represented in our language as $\cfsigma{j}$.
% For simplicity in our examples we omit $\cfsigma{-}$ around integers.
%riccardo3: 
% An obligation with a flat contract is reduced to an $\cfont{if}$ expression
An obligation with a flat contract reduces to an $\cfont{if}$ expression
that checks if the guarded value satisfies the contract. 
The contract check is performed first. 
If the contract check fails, the execution terminates with an error message
%riccardo3: 
% blaming the positive position identifier. If the contract check succeeds,
% the execution proceeds by returning the previously guarded value to its
blaming the identifier in positive position. If the contract check succeeds,
execution proceeds by returning the previously guarded value to its
context without a contract. An application of a value to 
%riccardo3: 
% a function with a higher-order contract is reduced to a check of the
a function with a higher-order contract reduces to a check of the
domain contract on the argument value and a check  
of the range contract on the result of the application. The positive and
negative blame identifiers are reversed for the argument check ensuring
that contravariant portions of the contract are blamed correctly.
%riccardo3: 
% An obligation with a $\cfuturecontract{\cc}$ 
% is reduced to a check of $\cc$ on the guarded value using the
% $\csend{\cE_1}{\cE_2}$ operator only if \cc is a flat contract. Otherwise, the
% checking is postponed similarly to the $\cfuncontract{c_1}{c_2}$
% combinator except that in this case the contracts on the obligations for
% the argument and the result of the application are wrapped with the
% $\cfuture{\cc}$ combinator.
% In later sections, when we give to the meaning of the
% $\csend{\cE_1}{\cE_2}$ operator a concurrent flavor, $\cfuture{\cc}$ will 
% become a concurrent contract checking annotation.
An obligation with $\cfuturecontract{\cc}$ where $\cc$ is a flat
contract reduces to a check of $\cc$ on the guarded value using the
$\csend{\cE_1}{\cE_2}$ operator. If $c$ is a higher-order contract,
checking is postponed as in the $\cfuncontract{c_1}{c_2}$ case; the 
$\cfont{future}$ annotation travels to the two newly introduced contracts.
In later sections, when we give $\csend{\cE_1}{\cE_2}$ a concurrent
interpretation, $\cfuture{\cc}$  becomes a parallel contract
checking annotation.

 
\begin{figure*}
\begin{align*}
&  \mathit{eval^i}(\cdelta \In \cE) = 
\begin{cases}
  \cP_\mathit{f} &  
 \text{if $\crulestari{\cdelta \cholds \cE\DPar\cmttrace}
 {}{\cP_\mathit{f}}{}{i}$} \\
 \bot \DPar \ctrace  &  
 \text{if $\forall \cE'$ such that $\crulestari{\cdelta\cholds
%riccardo3: typo
%  \cE\DPar\cmttrace}{}{\DPar\cE'\DPar\ctrace}{}{i}$,
 \cE\DPar\cmttrace}{}{\cE'\DPar\ctrace}{}{i}$,
   ~$\exists \cE''$
%riccardo3: typos?
%  such that $\crulei{\cdelta\cholds \cIdent \cE''\DPar\ctrace}{}
%          {\cIdent \cE''\DPar\ctrace}{}{i}$}\\
 such that $\crulei{\cdelta\cholds \cE'\DPar\ctrace}{}
         {\cE''\DPar\ctrace}{}{i}$}\\
     \end{cases}\\[2ex]
&   \mathit{eval}(\cdelta \In \cE) = 
  \begin{cases}
    r & \text{if $\exists i \in \mathbb{N}$ such that 
    $\mathit{eval^i}(\cdelta \In \cE) = r$ 
    and $r \neq \top \DPar \ctrace$}\\ 
    \bot \DPar \hat{\ctrace}   & 
    \text{if $\forall i \in \mathbb{N}$ 
    $\mathit{eval^i}(\cdelta \In \cE) = \top \DPar \ctrace_i$ and
    $\hat{\ctrace}$ is the smallest  (possibly infinite) trace}\\ &
    \text{such that
      $\ctrace_0 \sqsubseteq \ctrace_1 \sqsubseteq \ctrace_2 \sqsubseteq
       \dots \sqsubseteq
      \hat{\ctrace}$}\\
  \end{cases}
\end{align*}
\caption{$\con$ evaluators}
\label{seq-eval-simple-res}
\label{seq-eval-simple}
\end{figure*}



The global reduction relation regulates the evaluation of expressions
involving outputs and explicit contract checking, and is defined over
\emph{states}. A state $\cE\DPar\ctrace$ is a combination of an
expression $\cE$ and a finite output trace $\ctrace$. 
To avoid having to define and reason about infinite sequences of
outputs, we follow the standard approach of approximating an infinite
sequence by its finite prefixes. 
It is technically convenient to define a family of context-sensitive
small-step reduction relations~\cite{fh:syntactic-control-state}
parameterized by a natural number $i$ standing for the maximum number
of outputs allowed in the computation. Intuitively each one of those
reduction relations describes the behavior of programs up to a finite
trace output of size $i$.
These global reduction relations take the form $\crulei{\cdelta \cholds
\cP}{}{\cP'}{}{i}$, stating that state $\cP$ reduces to $\cP'$ in the
context of declarations $D$, and that the output traces that appear in
$\cP$ and $\cP'$ are smaller than $i$. 
The rules for this relation appear in Figure~\ref{seq-sem-rules}. 
The checking reduction discards the value $\cval$ and the
execution proceeds with the evaluation of $\cE_2$. 
The output reduction additionally appends the value 
$\cval$ to the trace $\ctrace$ of the resulting state.
If the size of the trace $\ctrace$ of the state before the reduction 
step is
already equal to $i$ then the evaluation terminates with $\top \DPar \ctrace$. 
%riccardo2: not needed - already defined on different domains anyways 
% We have separated the rules in Figure~\ref{seq-sem-rules} from the
% others to simplify the presentation of our later abstract machines. 
% Intuitively, the reduction rules in Figure~\ref{local-sem-rules}
% are used in our later systems, while the
% reduction rules in Figure~\ref{seq-sem-rules} are going to be replaced by
% rules that take advantage of parallelism.
As usual, we use the notation $\crulestari{\cdelta \cholds
\cP}{}{\cP'}{}{i}$ for the transitive closure. 



For every $i$-parameterized abstract machine there are three kinds 
of states $\cP_\mathit{f}$ 
for which no reduction is defined: 
$\cval \DPar \ctrace$ corresponds to programs terminating in
a value $\cval$ with an output trace $\ctrace$ with maximum possible size
$i$; $\cerror{\cstr} \DPar \ctrace$  corresponds to programs 
terminating abnormally because of a runtime error \cerror{\cstr} with
trace output $\ctrace$ with maximum size $i$; and $\top \DPar
\ctrace$ corresponds to abnormally terminating programs due to a
reduction step that would lead to an output trace larger than $i$. The
latter case captures the finite prefix of size $i$ of a program whose
execution leads to an output trace of size larger than $i$. 

Given an initial state $\cP_{o}$ of the form $\cE \DPar \cmttrace$, 
and any natural
number $i$, the behavior of the reductions $^{i}\ccstep$ is well defined. 
We define the restricted evaluation function
$\mathit{eval^i}$ for each one of the sequential machines in
Figure~\ref{seq-eval-simple-res}. We extend the range of the evaluator
with the extra element $\bot \DPar \ctrace $ and we map
all diverging programs
with output trace $\ctrace$ to $\bot \DPar \ctrace$. 
Since each machine admits
only programs with maximum output trace $i$, even diverging programs have
a finite output trace. We use $\ctrace \sqsubseteq \ctrace^\prime$ for
the standard prefix ordering on traces.

The full evaluator $\mathit{eval}$ for $\con$ is defined  
in figure~\ref{seq-eval-simple} in terms of the restricted
evaluators for $\con$. When a program diverges
we can define its output trace by using its finite prefixes
obtained from the restricted evaluators.To show that evaluator 
$\mathit{eval}$ is meaningful, it suffices to prove it is a total function.

\begin{theorem}
  \label{paper:total-eval}
   $\mathit{eval}$ is a total function. 
\end{theorem}



 \begin{figure*}[t]
  \center
%riccardo3: {align*} would be better than array, because array changes
%the spacing to make it fit in a smaller space, but it'll do for now.
   $
  \begin{array}{cclllll}
    \cdelta & \cholds &
    {\cmq \DPar \ccontext{\cEC}{\cE} \DPar \ctrace} &
    ^{i}\ccstep_{\tau}&
    {\cmq \DPar \ccontext{\cEC}{\cE^\prime} \DPar \ctrace}& 
    {\text{where \,\crule{\cdelta \cholds \cE}{}{\cE^\prime}{}}} &
      ($m-prop$)\\

    \cdelta & \cholds & 
    {\cmq \DPar \ccontext{\cEC}{\cE} \DPar \ctrace}&
    ^{i}\ccstep_{\tau} &
      {\cerror{\dq{x}}\DPar \ctrace} &
      {\text{where \,\crule{\cdelta \cholds \cE}{}{\cerror{\dq{x}}}{}}} & 
      ($m-err$)\\

  \cdelta & \cholds & 
  {\cmq \DPar \ccontext{\cEC}{\csend{\cval}{\cE}} \DPar \ctrace}&
  ^{i}\ccstep_{\tau} &
  {\cmq \DPar \ccontext{\cEC}{\cE} \DPar \ctrace} & &
  ($m-check$)\\
      
   \cdelta & \cholds & 
   {\cmq \DPar  \ccontext{\cEC}{\creceive{\cval}{\cE}} \DPar \ctrace} &
   ^{i}\ccstep_{\tau} &
   {\cmq \DPar \ccontext{\cEC}{\cE} \DPar \cexttrace{\ctrace}{\cval}} &
   \text{if \,$\csize{\ctrace}<i$} &
      ($m-out$)\\


     \cdelta & \cholds & 
     {\cmq \DPar \ccontext{\cEC}{\creceive{\cval}{\cE}}  \DPar \ctrace }&
     ^{i}\ccstep_{\tau} &
      {\top \DPar \ctrace} & 
      \text{if \,$\csize{\ctrace}=i$} &
       ($m-fout$)
       
 \end{array}
 $

\caption{Reduction rules for global evaluation with an empty queue, 
            $\tau\in\{s,c\}$}  
\label{global-seq-sem-rules}
\end{figure*}

\begin{figure*}
  \center
  $
  \begin{array}{cclllll}
 
    \cdelta &\cholds&
    {\cE_q \cseq \cQ \DPar \ccontext{\cFC}{\cE} \DPar \ctrace}&
    ^{i}\ccstep_{c} &
    {\cE_q \cseq \cQ \DPar \ccontext{\cFC}{\cE^\prime}\DPar \ctrace}&
    \text{where \,{\crule{\cdelta \cholds \cE}{}{\cE^\prime}{}}}&
    ($s-prop$)\\

    \cdelta &\cholds&
    {\ccontext{\cEC}{\creceive{\cval}{\cE_1}}\cseq\cQ \DPar\cE\DPar\ctrace}&
    ^{i}\ccstep_{c} &
    {\ccontext{\cEC}{\cE_1}\cseq\cQ \DPar \cE \DPar \cexttrace{\ctrace}{\cval}}&
     \text{if \,$\csize{\ctrace}<i$}&
      ($m-qout$)\\

      \cdelta &\cholds &
    {\ccontext{\cEC}{\creceive{\cval}{\cE_1}}\cseq\cQ\DPar \cE \DPar \ctrace}&
      ^{i}\ccstep_{c} &
      {\top \DPar \ctrace}&
      \text{if \,$\csize{\ctrace} = i$}&
      ($m-qfout$)\\
 

      \cdelta &\cholds &
      {\ccontext{\cEC}{\cE_q}\cseq\cQ\DPar\cE\DPar \ctrace}&
      ^{i}\ccstep_{c} &
     {\ccontext{\cEC}{\cE_q^\prime}\cseq\cQ\DPar\cE\DPar\ctrace}&
     {\text{where \,{\crule{\cdelta \cholds\cE_q}{}{\cE_q^\prime}{}}}}&
      ($m-qprop$)\\


      \cdelta &\cholds &
      {\cval \cseq \cQ \DPar \cE \DPar \ctrace}&
      ^{i}\ccstep_{c} &
      {\cQ \DPar \cE \DPar \ctrace}&&
      ($m-deq$)\\

      \cdelta & \cholds & 
      {\ccontext{\cEC}{\cE_q}\cseq\cQ\DPar\cE\DPar\ctrace}&
      ^{i}\ccstep_{c} &
      {\cerror{\dq{x}} \DPar \ctrace}&
      \text{where \,{\crule{\cdelta\cholds\cE_q}{}{\cerror{\dq{x}}}{}}}&
      ($m-qerr$)\\ 

     \cdelta & \cholds &
     {\cQ \DPar \ccontext{\cFC}{\csend{\cE_1}{\cE_2}}\DPar \ctrace}&
     ^{i}\ccstep_{c} &
     {\cQ \append \cE_1 \cseq \cmq \DPar\ccontext{\cFC}{\cE_2}\DPar\ctrace}&&
     ($s-enq$)\\

     \cdelta & \cholds &
     {\ccontext{\cEC}{\csend{\cval}{\cE_2}}\cseq\cQ \DPar \cE\DPar \ctrace}&
     ^{i}\ccstep_{c} &
     {\ccontext{\cEC}{\cE_2}\cseq\cQ \DPar \cE \DPar \ctrace}& &
     ($m-qcheck$)

\end{array}
$ 
\caption{Reduction rules for global evaluation with a non empty queue}
 \label{global-par-sem}
\end{figure*}


\begin{figure*}[t]
\begin{align*}   
  & \mathit{eval^i_c}(\cdelta \In \cE) = 
\begin{cases}
  \cP_\mathit{f} &  
%riccardo2: simplified (and corrected - the \ctrace should probably be
%   \emptyset here
%   \text{if $\ccrulestari{\cdelta \cholds \cP}{}{\cP_f}{c}{i}$,
%       where $\cP\cIdent\cmq\DPar\cE \DPar \ctrace$}\\
  \text{if $\ccrulestari{\cdelta \cholds \cmq\DPar\cE \DPar \emptyset}{}
       {\cP_\mathit{f}}{c}{i}$,}\\
 \bot \DPar \varphi &  
 \text{if $\forall \cE'$ such that
  $\ccrulestari{\cdelta\cholds\cmq\DPar\cE\DPar  \cmttrace}
       {}{\cQ'\DPar\cE' \DPar \ctrace}{c}{i}$,
  ~$\exists \cE''$
      such that $\ccruleplusi{\cQ'\DPar\cE' \DPar \ctrace}{n,m}
      {\cQ''\DPar\cE'' \DPar \ctrace}{c}{i}$}\\ &
     \text{where $n$ is the 
  overall number of reductions, $m$ the number of 
  ~(m-\dots)~ reductions in}\\&
 \text{the corresponding  transition sequence,
 and $n > 0$, $m > 0$}.
 \end{cases}\\[2ex]
& \mathit{eval_c}(\cdelta \In \cE) = 
  \begin{cases}
    r & \text{if $\exists i \in \mathbb{N}$ such that 
    $\mathit{eval_c^i}(\cdelta \In \cE) = r$ 
    and $r \neq \top \DPar \ctrace$}\\ 
    \bot \DPar \hat{\ctrace} & 
    \text{if $\forall i \in \mathbb{N}$ 
    $\mathit{eval_c^i}(\cdelta \In \cE) =\top \DPar
      \ctrace_i$ and
      $\hat{\ctrace}$ is the smallest (possibly infinite) trace}\\&
      \text{such that
      $\ctrace_0 \sqsubseteq \ctrace_1  \sqsubseteq \ctrace_2
       \dots \sqsubseteq
      \hat{\ctrace}$}\\  
  \end{cases}
\end{align*}
\caption{$\ccc$ evaluators}
\label{par-eval}
\label{par-eval-res}
\end{figure*}


\section{Concurrent Contract Checking}
\label{s:parallel}


The language presented in the preceding section enables contract checking,
but the abstract machines modeling its operational semantics are sequential.
%riccardo4: 
% We present an alternative semantics for the same syntax that enables
% concurrent contract checking. We call the resulting model $\ccc$. 
We now present an alternative semantics for a language with the same
syntax (figure~\ref{syntax-simple}) that enables 
parallel contract checking. We call the resulting language $\ccc$. 
The semantics is described with a family of machines parameterized by a
natural number $i$ as in $\con$. As before a compilation step 
precedes the evaluation of a program.

%riccardo4:
% The operational semantics is now concurrent and implements a master-slave
% architecture. The master evaluates the program and sends any contract
% predicates that 
% it finds on its path in the form of $\csend{\cE_1}{\cE_2}$ expressions 
% to the slave. 
The operational semantics of $\ccc$ is concurrent and implements a master-slave
architecture. The master thread evaluates the program and sends any
contract it encounters in the context of an expression
$\csend{\cE_1}{\cE_2}$ to the slave thread. 
The slave evaluates the contract predicate $\cE_1$ that it
receives and aborts the program when a contract failure occurs. The two threads
synchronize when effects, such as outputs,  are performed or when
errors are reported.

%riccardo6:
% We use two levels of
% reductions: local reductions
% (involving either the master or the slave thread only), and global
% threads. The local evaluation relation, 
% $\cdelta\cholds\cE\cstep\cE'$ (with $\cdelta\cholds
% \cE\cstep\cerror{str}$) is unchanged (figure $\ref{local-sem-rules}$). 
Again, we use two levels of reductions: local reductions---involving
only expressions, from either the master or the slave threads---and global reductions---regulating the interaction of the master and the slave threads. The local evaluation relation is unchanged from $\con$ (figure \ref{local-sem-rules}). 

The states of $\ccc$ are just like those of $\con$, except that we add
a queue to manage communication between the master and slave
threads. 
The initial state of program $\cPr$ is the state in which the queue 
is the empty queue $\cmq$.
A queue is a finite sequence of expressions. Two queues can be appended using
%riccardo4: 
% $\append$, defined in the ususal way.
$\append$, defined in the usual way.


%riccardo4: 
% A global computation concerns the entire state and
A global computation involves the entire state and
requires the synchronization of the master and the slave thread.
We use
$\ccrulei{\cdelta\cholds \cQ\DPar\cE \DPar \ctrace}{}
        {\cQ^\prime \DPar\cE'\DPar \ctrace'}{s}{i}$
%riccardo4: 
% to denote a global computation step, from one state to another.  Figures
to denote a global computation step from state $\cQ\DPar\cE \DPar
\ctrace$ to state $\cQ^\prime \DPar\cE'\DPar \ctrace^\prime$. Figures
\ref{global-seq-sem-rules}
and \ref{global-par-sem} display the two sets of rules, which jointly specify global
progress. The rules in 
figure \ref{global-seq-sem-rules} 
 determine how the machine behaves when the
%riccardo4: 
% queue is empty, i.e., when the master may proceed independently of the
queue is empty, that is, when the master may proceed independently of the
slave. We have factored out the rules of figure \ref{global-seq-sem-rules}
because we can reuse them
for the correctness proof in a different context ($\tau$).  The rules in
figure \ref{global-par-sem} focus on the case when the queue is not empty.  They specify how
local computations affect the global state and how reductions that depend on
the global state are performed. 

The master can delegate a task to the slave using rule (s-enq). This rule 
 assigns a new meaning to the sequential operator $\csend{\cE_1}{\cE_2}$.
 The slave receives $\cE_1$ and adds it to the end of the queue. 
 When $\cE_1$ reaches the front
 of the queue, the slave starts evaluating $\cE_1$. That way, the
 expressions in the queue are evaluated exactly in the same order they
 are added, which is the same order as a sequential version
 of the program would have used.


 When the slave has finished evaluating an expression, the outcome of the
 reduction is simply discarded and the value is removed from the queue
 using rule (m-deq).
  This new semantics for $\csend{\cE_1}{\cE_2}$ 
  ensures that obligations with a $\cfuturecontract{\cc}$ contract
  ask the run-time system to parallelize the check of $\cc$, because
  those obligations reduce to $\cfont{check}$ expressions.
 
 To ensure that blame assignment and output traces are
 not affected by the nondeterministic nature of the parallel machine,
 we impose three kinds of synchronization points 
 between the master and the slave: 
 rules (m-err), (m-out) and (m-fout). 
%riccardo5: 
%  The master cannot perform any effect-full operation or signal an error or
 The master cannot perform any effectful operation or signal an error or
 terminate the program when the slave has a non-empty queue. If the slave
 has a non-empty queue, then there are expressions that in sequential mode
 would be evaluated before the synchronization point. In contrast, the
 slave can perform any output or error operation promptly, since it
 evaluates expressions with the same order that they would have been evaluated 
 on purely sequential machine.


 \subsection{Examples}

 Despite the restricted form of parallelization introduced by our semantics
 and the synchronization points, the parallel machine remains
 nondeterministic and parallelizes contract checking.

%riccardo4: 
%  Let $h$ be the following contract:
% $$
%   h=\cfuturecontract{ 
%       \cfuncontract
%                         {\cflatcontract{\mathit{even?}}}
%                         {\cflatcontract{\mathit{even?}}}}
% $$
% and consider the following program
 Let $h$ be the contract
\[
  \cfuturecontract{ 
      \cfuncontract
                        {\cflatcontract{\mathit{even?}}}
                        {\cflatcontract{\mathit{even?}}}}
\]
and consider the program
\[\begin{array}[t]{ll}
   \cvalrec{\mathit{add1}}{h}{\cflambda{x}{x+\cfsigma{1}}} \In
     {\capp{\mathit{add1}}{\cfsigma{2}}}
  \end{array}
\]
The compilation process inserts obligations as follows:
\[
 \begin{array}[t]{ll}
%riccardo4: this should be an IN, I think
%   \cvalrec{\mathit{add1}}{h}{\cflambda{x}{x+1}} \DPar
   \cvalrec{\mathit{add1}}{h}{\cflambda{x}{x+\cfsigma{1}}} \In
  {\capp{\coblig{\mathit{add1}}{h}       
                {\dq{\mathit{add1}}}{\dq{main}}}
        {\cfsigma{2}}}
  \end{array}
\]
and the result is then evaluated:
%riccardo5: rewrote the example using alignment
\begin{align*}
 & \qquad \cmq  \DPar
   {\capp{\coblig{\mathit{add1}}{h}{\dq{\mathit{add1}}}{\dq{main}}}
        {\cfsigma{2}}} \DPar \cmttrace\\
 &  \ccstep_c\\
 & \qquad \begin{aligned}[t]
    &  \cmq \\
    &  \DPar \cclet{x}
     {\coblig{\cfsigma{2}}{\cfuturecontract{\cflatcontract{\mathit{even?}}}}
                {\dq{main}}{\dq{\mathit{add1}}}}
       {\cclet{y}{\capp{\mathit{add1}}{x}}
                {\coblig{y}
                {\cfuturecontract{\cflatcontract{\mathit{even?}}}}
                                  {\dq{\mathit{add1}}}{\dq{main}}}}\\
    &  \DPar \cmttrace
   \end{aligned}\\
 & \ccstep_c\\
 & \qquad \begin{aligned}[t]
       & \cmq \\
       & \DPar
     \cclet{x}
     {\csend{\cif
              {\capp{\mathit{even?}}{\cfsigma{2}}}
              {\cfsigma{2}}{\cblame{\dq{main}}}}
             {\cfsigma{2}}}  
       {\cclet{y}{\capp{\mathit{add1}}{x}}
                {\coblig{y}
                {\cfuturecontract{
                \cflatcontract{\mathit{even?}}}}
                 {\dq{add1}}{\dq{main}}}}\\
        &  \DPar \cmttrace
   \end{aligned}
\end{align*}
%riccardo4: 
% At this point the parallel machine can either  check the argument of
% $\mathit{add1}$ locally or delegate the contract checking to the slave. If the second
% option is chosen then the master can continue with the evaluation of the
% successor of $2$ in parallel:\\
\noindent 
At this point, a sequential contract monitoring system would check the
argument of $\mathit{add1}$ locally. The semantics of $\ccc$ however
delegates the contract checking to the slave. The master can continue
with the evaluation of the successor of $2$ in parallel:
\begin{align*}
%riccardo
%  & \qquad \begin{aligned}[t]
%      & \cmq\\
%      & \DPar 
%      \cclet{x}
%      {\csend{\cif
%               {\capp{\mathit{even?}}{\cfsigma{2}}}
%               {\ctrue}{\cblame{\dq{main}}}}
%              {\cfsigma{2}}}  
%        {\cclet{y}{\capp{\mathit{add1}}{x}}
%              {\coblig{y}
%               {\cfuturecontract{\cflatcontract
%               {\mathit{even?}}}}{\dq{\mathit{add1}}}{\dq{main}}}}\\
%      & \DPar       \cmttrace
%   \end{aligned}\\
& \ccstepf{}^*_c\\
& \qquad \begin{aligned}[t]
     &   \cif
     {\capp{\mathit{even?}{\cfsigma{2}}}}
        {\cfsigma{2}}{\cblame{\dq{main}}} \cseq \cmq \\
     & \DPar 
      {\coblig{\cfsigma{3}}
      {\cfuturecontract{\cflatcontract{\mathit{even?}}}}{\dq{\mathit{add1}}}{\dq{main}}}\\
     & \DPar \cmttrace
  \end{aligned}\\
&    \ccstepf{}^*_c\\
& \qquad \begin{aligned}[t]
       &   {\cif
        {\capp{\mathit{even?}}{\cfsigma{2}}}
        {\cfsigma{2}}{\cblame{\dq{main}}}} \cseq \cmq\\
      &   \DPar
     {\csend{\cif{\capp{\mathit{even?}}{\cfsigma{3}}}
                    {\cfsigma{3}}{\cblame{\dq{\mathit{add1}}}}} 
                    {\cfsigma{3}}}\\
      &  \DPar \cmttrace
  \end{aligned}\\
&    \ccstepf{}^*_c\\
& \qquad  \cif
     {\capp{\mathit{even?}}{\cfsigma{2}}}
        {\cfsigma{2}}{\cblame{\dq{main}}} \cseq \\
& \qquad \ntab \cif{\capp{\mathit{even?}}{\cfsigma{3}}} 
                 {\cfsigma{3}}{\cblame{\dq{\mathit{add1}}}} \cseq \cmq 
 \DPar \cfsigma{3} \DPar \cmttrace\\
&  \ccstepf{}^*_c\\
& \qquad \ctrue \cseq
        \cif{\capp{\mathit{even?}}{\cfsigma{3}}} 
                 {\cfsigma{3}}{\cblame{\dq{\mathit{add1}}}} \cseq \cmq
      \DPar  \cfsigma{3} 
      \DPar \cmttrace\\
&  \ccstepf{}^*_c\\
& \qquad \cblame{\dq{\mathit{add1}}} \cseq \cmq  
    \DPar \cfsigma{3} \DPar \cmttrace
\end{align*}
which yields the final result $\cerror{\dq{add1}} \DPar \cmttrace$.

As this example shows, the master and the slave must collaborate. 
%riccardo4: 
% The master cannot ignore the will
% of its slave, however, once the slave is initiated. The slave is the one that
In particular, the master cannot ignore the will
of the slave once the slave is initiated. The slave is the one that
controls the execution of effectful operations. Moreover, the master
is not even allowed to diverge without the permission of its slave.

%riccardo4: 
% To illustrate these points, we turn to a couple more example: 
% $$h=\cfuturecontract{
To illustrate these points, we turn to a couple more example. First,
let $h$ be the following contract
\[\cfuturecontract{
           \cfuncontract
              {\cflatcontract{\cflambda{x}{\cfalse}}}
              {\cflatcontract{\cflambda{x}{\cfalse})}}}
\]
%riccardo4: added
in the following program
\[
  \begin{array}[t]{l}
    \cvalrec{loop}{h}   {\cflambda{x}{\capp{loop}{x \cfont{+} \cfsigma{1}}}} 
    \In
      \capp{loop}{\cfsigma{0}}
   \end{array}
\]
If the program is executed sequentially, then the domain contract checking
fails without calling $\mathit{loop}$. In contrast the parallel machine may choose
to send the contract to the slave and then ignore the slave for the
following steps, start evaluating the $\mathit{loop}$ call and diverge.
Such an execution disagrees with the sequential evaluation and
should be excluded from the set of valid evaluations. We therefore
restrict valid diverging executions to those that let the slave take
steps infinitely often.
This way the slave becomes the guard of sequentiality and the master of nondeterminism.

Our semantics enforces the above restriction by requiring that valid
transition sequences take steps described by 
\emph{mandatory} rules (m-\dots) infinitely often. 
 We use the notation $\ccruleplus{\cP}{n,m}{\cP^\prime}{c}$ to indicate
 that $\cP$ reduces to $\cP^\prime$ in  $n$ steps out of which $m$ are
 mandatory. 

%riccardo4: 
% Consider now the following subtle scenario:
% $$h=\cfuncontract
Consider now the following subtle scenario. Let $h$ be the following
contract 
\[\cfuncontract
              {\cfuturecontract{\cflatcontract{\mathit{hostile?}}}}
              {\cflatcontract{\cflambda{x}{\ctrue}}}
\]
%riccardo4: added
in the following program
\[
  \begin{array}[t]{l}
    \ccvalrec{attack}{h}{\cflambda{target}
    {\creceive{\dq{missile}}{\dq{mission\,accomplished}}}} 
    \\\In
    \capp{attack}{\dq{ally}}
   \end{array}
\]
When contracts are checked in-line, the domain contract of $attack$
ensures that a missile is fired only if the target is hostile.
However, a parallel and unsynchronized check of the contract may lead to
missile launching without first confirming that the target is an enemy.
This indicates that observable effects should be delayed until all
parallel contract checking has completed. Our semantics accomplishes this by
treating the effectful operators as synchronization points between the
master and the slave.

\subsection{Determinism and Correctness}\label{s:correctness}

We define 
the restricted parallel evaluators $\mathit{eval^i_c}$ in figure
%riccardo4:
% \ref{par-eval-res} in the same way as we defined $\mathit{eval^i}$ in for 
\ref{par-eval-res} in the same way as we defined $\mathit{eval^i}$ for
$\con$.

  To establish that evaluators $\mathit{eval^i_c}$ are
  functions, we need to show that all
possible executions of a program in the parallel machine have the same
observable behavior. More precisely, we must establish that all possible executions of a
 program have the same transition sequence 
 output and termination behavior. We prove this fact in the traditional way using 
an extended form of the Diamond Lemma for our language~\cite{ff:future}. 

\begin{lemma}
  \label{paper:transconsistency}
   For all $i \in \mathbb{N}$,
   if $\ccrulestari{\cdelta \cholds \cP}{}{\cP_{\mathit{f}}}{c}{i}$  
  then
  \begin{enumerate}
    \item[(a)] if $\ccrulestari{\cdelta \cholds \cP}{}{{\cP_\mathit{f}}^\prime}{c}{i}$,
      $\cP_\mathit{f} = {\cP_\mathit{f}}^\prime$
    \item[(b)] there is no transition such that $\forall k.$ 
      $\ccrulei{ \cdelta \cholds \cP}{*}{\cP_k}{c}{i}$, $m_k > 0$ and
      $\ccruleplusi{ \cdelta \cholds \cP_k}{n_k,m_k}{\cP_{k+1}}{c}{i}$.  
  \end{enumerate}
  \end{lemma}


 Lemma \ref{paper:transconsistency} can be used to show that evaluators
 $\mathit{eval^i_c}$ are functions. We define 
the evaluator $\mathit{eval_c}$ for our language in figure \ref{par-eval}
 and
we  show that $\mathit{eval_c}$ is a total function.
\begin{theorem}
  \label{paper:eval-par-function}
   $\mathit{eval_c}$ is a total function.
\end{theorem}


 Our  ultimate goal, though, is to show that the parallel language $\ccc$ is
 semantically equal to $\con$.
 In other words, we would like to prove that the
 two evaluators $\mathit{eval}_c$ and $\mathit{eval}$ are equal.

 The differences between the two languages makes it difficult to compare
 them. To overcome the problem, we define an intermediary language that
 bridges the gap between $\con$ and $\ccc$. It is sequential like
 $\con$, but shares the same machine infrastructure with $\ccc$. 

 The semantics of this new language $\seq$ is described in figures
 $\ref{local-sem-rules}$ and $\ref{global-seq-sem-rules}$. 
 We define its restricted sequential evaluators $\mathit{eval^i_s}$ 
and $\mathit{eval_s}$ in figure~\ref{seq-eval}. 

\begin{figure*}
\begin{align*}
 &  \mathit{eval_s^i}(\cdelta \In \cE) = 
\begin{cases}
  \cP_\mathit{f} &  
 \text{if $\ccrulestari{\cdelta \cholds \cmq\DPar\cE\DPar\cmttrace}{}
 {\cP_\mathit{f}}{s}{i}$}\\
 \bot \DPar \ctrace &  
 \text{if $\forall \cE'$ 
 such that $\ccrulestari{\cdelta\cholds
 \cmq \DPar \cE \DPar \cmttrace}{}
    {\cmq\DPar\cE'\DPar\ctrace}{s}{i}$,
    ~$\exists \cE''$
 such that $\ccrulei{\cdelta\cholds \cmq \DPar \cE'\DPar\ctrace}
                  {}{\cmq \DPar \cE''\DPar \ctrace }{s}{i}$}
     \end{cases}\\[2ex]
& 
  \mathit{eval_s}(\cdelta \In \cE) = 
  \begin{cases}
    r & \text{if $\exists i \in \mathbb{N}$ such that 
    $\mathit{eval_s^i}(\cdelta \In \cE) = r$ 
    and $r \neq \top \DPar \ctrace$}\\ 
    \bot \DPar \hat{\ctrace}& 
    \text{if $\forall i \in \mathbb{N}$ 
    $\mathit{eval_s^i}(\cdelta \In \cE) = \top \DPar \ctrace_i$ and
    $\hat{\ctrace}$ is the smallest (possibly infinite) trace such}\\&
    \text{that
      $\ctrace_0 \sqsubseteq \ctrace_1 \sqsubseteq \ctrace_2 \sqsubseteq
      \dots \sqsubseteq \hat{\ctrace}$}\\
  \end{cases}
\end{align*}
\caption{$\seq$ evaluators}
\label{seq-eval}
\label{seq-eval-res}
\end{figure*}

\begin{lemma}
  \label{paper:total-eval-simple}
   $\mathit{eval_s}$ is a total function.
\end{lemma}

   Now that we have established that $\mathit{eval_s}$ is a well-defined
   total function, we can easily prove that the two sequential languages
   $\con$ and $\seq$ are equivalent.

\begin{lemma}
  \label{paper:eval-equiv-eval-simple}
  $\mathit{eval_s}=\mathit{eval}$. 
\end{lemma}

 The next step is to show that $\seq$ is also equivalent to $\ccc$. 
 A first result is that the parallel language is 
 only an extension of the sequential language. Thus, the parallel language
 can admit all the transition sequences that the sequential machine admits. 

 \begin{lemma}
  \label{paper:stepsimpliesstepp}
   For all $i \in \mathbb{N}$, if 
   \ccrulei{ \cdelta \cholds \cP}{}{\cP^\prime}{s}{i} then 
  \ccrulei{ \cdelta \cholds \cP}{}{\cP^\prime}{c}{i}.  
  \end{lemma}

 Now we show that $eval_c$ is equal to $eval_s$ and thus we establish that
 the two corresponding models are equivalent.

 \begin{lemma}
  \label{paper:evalpcorrectness-i}
  $\mathit{eval_c} = \mathit{eval_s}$. 
  \end{lemma}

  \noindent Lemmas \ref{paper:eval-equiv-eval-simple}
              and \ref{paper:evalpcorrectness-i} imply that $\con$ and $\ccc$ are  equivalent.

              
 \begin{theorem}
  \label{paper:main}
  $\mathit{eval} = \mathit{eval_c}$. 
  \end{theorem}

 Equiping our system with multiple slaves that evaluate in parallel the
 elements of the queue and adapthing the correctness proof to the new
 conditions is rather straightforward.


\section{Effects Beyond Outputs}
\label{s:effects-sync}
 
  Extending our model with mutable reference cells is straightforward. It
  suffices to add  expressions for cell allocation plus operators for reading from and
  writing to reference cells. Reading and writing must be treated as synchronization
  points between the slave and the master. The master can write and read a 
  location in the store only if the queue is empty. 
  
%riccardo5: 
%   Adding a store does not significantly affect the theory of our 
%   system. Our system already deals successfully with outputs and the
  Adding a store does not significantly affect our model. 
  It already deals successfully with outputs and the
  corresponding output trace. 
%riccardo3: 
% From our point of view, the store is a simpler entity because when a
% program diverges its store is irrelevant while its output trace is
% considered part of its observable behavior. 
The store is often treated in a way that is simpler than outputs because when a
program diverges, its store is irrelevant; its output trace, in contrast, is part of its observable behavior. 
%riccardo3: 
%   An output operation signals an irreversible observable event while a store
%   operation can be masked by other operations. 
  An output is an irreversible observable event, while a store
  update can be masked by a subsequent update.
%riccardo3: 
% Thus store can be seen as a restricted form of an output trace.  The infrastructure
  The store can therefore be seen as a restricted form of an output trace. 
  The proof technique that we use to show our approach's correctness is
  sufficient to cover a modification of our language with reference cells.

%riccardo5:
%   The above reasoning, however, is applicable onlyin the limited scope of our
%   model. In a realistic world, where contract exceptions can be caught
%   and the system can proceed with a recovery procedure, the contents of
%   the store upon contract failure cannot be ignored. This

  Moreover, the addition of a store affects the practicality of our
  approach. A naive addition can lead to an
  increase in the number of 
synchronization points. Synchronization points between threads are an
important factor for performance loss in concurrent programs. But the
nature of synchronization due to operations on locations are different
than those due to output operations. Operations on different locations
do not need to be synchronized. A write operation on a location
performed by the master thread needs to wait only for writes and reads
on the same location performed by the contracts that in a sequential
execution would be checked before the write operation. This implies
that if some operations involve only locations that are not
manipulated by such contracts then those operations should not be
treated as synchronization points.
  An approximation algorithm to identify operations with this property can be
  derived from standard control flow static analysis techniques \cite{s:phd}.
  Using the control graph of a program we can collect the set of
  contracts that are executed before any given operation on the store. Using
  the data flow graph of a program we can conservatively determine which
  locations are reachable \cite{dw:aggr-upd} from the variables of a 
  contract predicate and the set of locations reachable from the 
  arguments of the given
%riccardo3:
%   operation on the store. Having in hand these pieces of information,
  operation on the store. With this information,
  we can conclude that if this set of locations has an empty intersection with
  the set of locations that are reachable from the variables of the
  contracts, the operation does not need to be a synchronization point.
%riccardo3: 
%   An implication of that is that if the contracts are purely functional
%   then store operations do not need to be synchronization points.
As an immediate consequence, store operations do not need to be
synchronization points when contracts are purely functional.

%riccardo5: 
% In functional programming where effects are not frequent, the cost of
% synchronization is not an important issue. However, in imperative
  The above reasoning, however, is applicable only within the limited
  scope of our model. In a realistic setting, where contract may throw
  exceptions that can be caught by the program, which then proceeds to
  recovery,
  the contents of the store upon contract failure cannot be
  ignored. This 
turns every cell access into a synchronization event.

In functional programming where effects are infrequent, the cost of
synchronization is not an important factor. However, in imperative
programming where almost every other operation involves access to the
store, synchronization becomes a bottleneck for the system's performance. 
The development of sophisticated and accurate effect
analysis then becomes an emerging necessity.


\begin{figure*}[p]

  \center
\small

\begin{alltt}
        #lang scheme                                                    ;; the queue module
       
        (require scheme/mpair)
       
        (define-struct node (content [done? #:mutable]))                ;; a node contains the thunk content that 
                                                                        ;; returns any kind of scheme values (the
                                                                        ;; closure of a contract projection)and the 
                                                                        ;; boolean flag done?
                                                                        ;; the done? flag is #t if the node has been
                                                                        ;; already visited

        (define-struct queue (head tail)                                ;; a queue is a list of nodes with  mutable 
                       #:mutable)                                       ;; pointers at the first and last element

        (define QUEUE  (make-queue null null))                          ;; initially the queue does not contain any
                                                                        ;; nodes 
                                                                        ;; this holds only before the first enqueue

        (define (is-queue-mt?)                                          ;; a queue is empty
          (or (null? (queue-tail QUEUE))                                ;; if it has no nodes
              (node-done? (mcar (queue-tail QUEUE)))))                  ;; or it only has a sentinel node

        (define (dequeue)                                               ;; the tail node is removed if the queue has 
          (cond ((null? (mcdr (queue-head QUEUE)))                      ;; more than one nodes   
                 (set-node-done?! (mcar (queue-head QUEUE)) #t))        ;; if the queue has only one node then the 
                (else                                                   ;; node is marked as visited but it is not 
                 (set-queue-head! QUEUE (mcdr (queue-head QUEUE))))))   ;; removed (sentinel node)

        (define (enqueue closure)                                       ;; new not visited nodes are added to the tail
          (let ((tail (queue-tail QUEUE)))
            (cond ((null? tail)
                   (let ((head-tail (mcons (make-node closure #f) null)))
                     (set-queue-head! QUEUE head-tail)
                     (set-queue-tail! QUEUE head-tail)))
                  (else
                    (begin
                      (set-mcdr! tail (mcons (make-node closure #f) null))
                      (set-queue-tail! QUEUE (mcdr tail)))))))

        (define (peek-queue)                                            ;; the contents of the unvisited head node  
          (cond ((null? (queue-head QUEUE)) 'empty)                     ;; are returned but the node is not removed 
                ((null? (mcdr (queue-head QUEUE)))                      ;; from the queue
                 (if (node-done? (mcar (queue-head QUEUE)))
                   'empty
                   (node-content (mcar (queue-head QUEUE)))))
                (else 
                  (if (node-done? (mcar (queue-head QUEUE)))            ;; enqueue may push a visited node at the 
                    (begin                                              ;; head position 
                      (set-queue-head! QUEUE (mcdr (queue-head QUEUE))) ;; the visited node is removed if the queue 
                      'empty)                                           ;; has more than one nodes 
                    (node-content (mcar (queue-head QUEUE)))))))

    ;;--------------------------------------------------------------------------                  
    ;;----------------------------------- provide ------------------------------                  

        (provide/contract
          [struct node  ((content (->   any/c)) (done? boolean?))]
          [struct queue ((head (mlistof node?)) (tail (mlistof node?)))]
          [is-queue-mt? (->   boolean?)]
          [enqueue      (-> (->   any/c) void?)]
          [dequeue      (->   void?)]
          [peek-queue   (->   (or/c (->   any/c) symbol?))])
               
  
  \end{alltt}
  \caption{The queue implementation}
  \label{queue}
\end{figure*}  


\section{Implementation}
\label{s:implementation}




%riccardo4: rewrote
% The $\con$ calculus is a model for PLT Scheme and its contract library.
%  From that point of view, $\ccc$ can be understood as a compact
%  specification for a parallel re-implementation of ths system. 
The language $\con$ is a model of PLT Scheme and its
contract library. 
From that perspective, $\ccc$ can be viewed as a compact
specification for a parallel re-implementation of the PLT contract library.
In this section, we explain how to implement $\ccc$ for a full-fledged
system.

PLT Scheme \cite{PLT} is a full-featured higher-order language with
effects
%riccardo4: soundness is distracting here
% and comes with a sophisticated and sound contracts system.
and comes with a sophisticated contract system.
Contracts are implemented as error projections~\cite{fb:contracts-proj} in 
an autonomous macro library. This organization 
facilitates changes to the contract
monitoring mechanism by limiting those changes to the library.

Although the released version of PLT Scheme does not support multi-core
programming, an experimental version offers
\cfont{places},\footnote{The prototype for \cfont{places} is being
  developed by Kevin Tew and 
Matthew Flatt at the University of Utah.} a prototype 
extension for multi-core programming.
Our system is bringing together the contract system of PLT 
Scheme and \cfont{places}. 

We use \cfont{places} to launch two operating system
threads that map to the two threads of the $\ccc$ model: 
the master and the slave. Following our model, communication between the
two parties is implemented through a shared queue. 


 Before a program's evaluation begins in the master
 thread, the master initiates the slave. The slave starts listening to the
 queue, waiting until it becomes non-empty. The master evaluates the
 program and every time it hits a flat future
 contract it adds it to the end of the
 queue. When a contract is wrapped with
 our new $\cfont{future/c}$ combinator, 
 the resulting projection is shipped to the
 queue as a closure. The slave picks the transmitted contract and
 evaluates it. If it fails, it aborts the system with a contract error
 message. Otherwise it removes the contract from the queue and waits for
 the next one. In the meantime, the master continues the evaluation of the
 rest of the program. In case the master needs to perform an output or the
 evaluation terminates, the master waits for all the elements of the queue
 to be processed and then proceeds with the output statement or terminates. 


  The central piece of our prototype is an efficient implementation of the
  queue, shown in figure $\ref{queue}$. 
  Our implementation
  is based on the algorithm suggested by \citet[paragraph
  10.3]{hs:artofmp}. 
  
  In order to ensure that access to the queue does not 
  become a bottleneck in practice, the queue is a mutable list with two 
  pointers:  one to the first and one to the last element.
  The head end is responsible for reading and dequeuing elements from the
  list and the tail end is responsible for enqueuing new elements.
  In this setting, as long as the enqueue and dequeue operations are
  applied to different locations, locks are needed only to synchronize
  internally groups of readers and writers \cite[paragraph 10.3]{hs:artofmp}. 
%riccardo5: 
%   To nake sure that enqueue and dequeue are never
%   performed on the same end, we never allow the queue to become empty
  To make sure that enqueue and dequeue are never
  performed on the same node, we use a sentinel element to  prevent the queue
  from ever becoming empty after the first enqueue. 
  
%riccardo5: 
%   Also, according to our model, our system 
%   consists of only two threads: the master which controls the tail and
%   performs enqueues  and 
%   the slave which controls the head and performs peeks and dequeues. 
%   Thus we can remove any locks from our
%   implementation, also simplifying the original algorithm.
  Our system 
  uses only two threads: the master which controls the tail of the
  queue and
  performs enqueues  and 
  the slave which controls the head and performs peeks and dequeues. 
  We therefore do not require any locks in our
  implementation, which simplifies the original algorithm.
  
  The queue is initially empty and both pointers point to empty lists.
  The first time an element is added to the queue the head and tail
  pointers are updated to point both to this first element.
  No dequeues are performed prior to this point as the slave is 
  waiting for the queue to become non-empty.
  When more elements are added, the tail pointer is updated to point 
  to the last element of the queue.

 We use an extra  tag for each element that indicates if the element has been
 visited by the slave.  When an element is added to the end of the queue
%riccardo5:
%   through its tail pointer, the tag is set to a $\dq{not\, visited}$ state. 
 through its tail pointer, the tag is set to a \textsc{NotVisited} state. 
 This way we mark checked contracts and make sure that 
 the content of a node is delivered to the slave
 at most once. Contracts are never delivered to the slave only if the
 slave has aborted the program because it discovered a contract violation.
  
  Access to the elements from the head of the queue is performed in two
  steps. First the element is picked for processing, using {\tt
  peek-queue},
  without being removed  from the queue.
  Second, when the first element is processed, the element is removed from the queue and the head pointer is
  moved to the next element, if the queue has more than one
  element.
  Otherwise, both pointers point to the same element.
  In this case, {\tt dequeue} does not removes the element
%riccardo5: 
%   from the queue but only sets its tag to $\dq{visited}$  after the slave has
  from the queue but sets its tag to \textsc{Visited}. 
  Thus we avoid having an empty queue and {\tt
  dequeue}
  and {\tt enqueue} never access destructively the same end of the queue.
     
  Visited elements may appear at the head of the queue even when 
  the queue
  contains more than one element. 
  This case arises when a number of enqueues are
  performed on a single-element queue. 
  When the {\tt peek-queue} operation
%riccardo5: 
%   finds  a $\dq{visited}$ element at the head of the queue and the 
  finds  a \textsc{Visited} element at the head of the queue and the 
  queue has more
  than one element, it removes the element and moves the head pointer to the next
  element. Because this operation can occur only if the queue has
  two or more elements, the algorithm guarantees that removal and additions to the queue
  are never applied to the same end.

  
%riccardo4: 
%   The whole \cfont{future} contracts system is implemented as a separate library that provides
%riccardo5: 
%   To implement synchronization at effectful operations, 
%   we use wrapper functions that inspect the queue to implement 
%   synchronization and that are inserted explicitly in the program's code. 
%   A real implementation  should replace these wrappers with hooks
%   inside the PLT scheme compiler. The compiler should be notified that 
  To implement synchronization at effectful operations, 
  we use wrapper functions that inspect the queue. They are inserted
  to the program's code manually.
  A real implementation would replace these wrappers with hooks
  into the PLT Scheme compiler. The compiler should be notified that 
  parallel contract checking is enabled and instrument all
  effectful operations, error reports and program termination with automatic 
  synchronization checks.

%riccardo4: 
% \section{Benchmarks and Measurements}
\section{Evaluation}
\label{s:benchmarks}

%riccardo4:  made these into subsections for consistency
% \paragraph{Benchmarks.}
\subsection{Benchmarks}

%riccardo4: typo
% While benchmarks for Scheme and PLT Scheme abounds, there are no benchmark
While benchmarks for Scheme and PLT Scheme abound, there are no benchmark
suited for evaluating the performance of contract monitoring.
In the past Findler has measured the performance of the contract system of 
DrScheme ~\cite{fcffksf:drscheme}, ~a 200 kloc program, but this is not feasible with a prototype implementation of the
contract library.

We have therefore developed a small suite of benchmarks from the existing
test suite of PLT Scheme\footnote
  {\url{http://svn.plt-scheme.org/plt/trunk/collects/tests/mzscheme/benchmarks/common/}}
and from extracts of small applications. For all these benchmarks we 
turned informal assertions in comments into formal contracts.
The architecture of our benchmarks follow a simple idea: a set of
libraries provide functionality that is guarded by contracts. The client
module imports all the libraries and makes exensive use of the provided
functions. This design corresponds to what we think is a rough sketch of a
realistic and large application with interesting contracts.


Here is a detailed description of the benchmarks, with a focus on the
contract computations that they perform: 

\begin{description}

%riccardo4: 
% \item[fber](192 lines in 4 modules)~is an interpreter for Boolean programs, i.e., Boolean
\item[fber](192 lines in 4 modules)~is an interpreter for Boolean
  programs, that is, Boolean
 expressions and functions. The interpreter's contract ensures that the
 given expressions and functions are closed and well-formed. Similar
 properties are checked by the contract of the substitution function. The
 input to the benchmark consists of a Boolean expression with 10000 leaves
 that is applied to a function whose body has also 10000 leaves.

 \item[dna](241 lines in 3 modules)~simulates the attack of a virus group on a cell's colony. A
 cell is represented as two complementary DNA chains. A virus consists of a DNA
 sequence that stands for the target DNA sequence for the virus and a
 mutated DNA sequence that replaces the target DNA sequence when the virus
 attacks a cell. The latter possibly contains two special
 complementary bases X and Y. The mutation function's contract checks if
 the target cell and the attacker virus are well-formed according to the
 above description. The input to the benchmark consists of a group of 50
 viruses of DNA length 200 and a colony of 200 cells of DNA length 500.  All
 the viruses attack all the cells of the colony.
\item[lab](258 lines in 3 modules)~is a variant of the {\tt dna} benchmark. In this
  scenario, we have two cell colonies. All the viruses of the group of
  viruses attack all the cells of the
  first colony. The second colony manages to survive an attack by one of
  the viruses and reproduces twice before the
  virus attacks again. After that the mutated cells die and then the virus
  performs its last attack. All the functions have simple type contracts
  except for one, which comes with a contract that
  checks if the attacked cell is well-formed and is used for the last two 
  attacks.
  The input to the benchmark consists of a group of 50
 viruses of DNA length 200 and two identical colonies of 200 cells of DNA length
 500.
\item[tel](356 lines in 3 modules)~implements a phone book. Each entry consists of a last name, a
 first name, a list of telephone numbers with annotations, and a list of
 email addresses (also with annotations). A phone book is a sorted list of
 entries. The contracts of the {\tt add} and {\tt remove}
 functions check if the given telephone catalog is well-formed. The input
 to the benchmark is a catalog of 100 entries, on which 2000 add and remove
 operations are performed.

\item[tel-bst](417 lines in 3 modules)~re-implements the previous phone book
 as an avl tree. The contracts of the {\tt add} and {\tt search}
 functions check if the given telephone catalog is a well-formed avl tree. 
 The input  to the benchmark is a catalog of 1000 entries, on which 
 2000 search and add operations are performed.


\item[lists](59 lines in 3 modules) ~provides a function {\tt min} to find
  the minimum element of a given sorted list of positive numbers and a
  function {\tt incr} that increments all the elements of the sorted 
  list by the minimum element
  of the list. The {\tt min} function simply returns the first element of
  the list if the list is not empty and an empty-list symbol otherwise.
  The contracts of the functions check if the given lists are sorted lists 
  of positive numbers. The input to the benchmark is a list of 500,000
  elements. 
\item[conform](702 lines in 8 modules)~turns a graph into a lattice. The application comes with a
 sets manipulation library. A set is represented as a list of distinct
 elements. The contracts on the functions of the library check if the set
 arguments are properly formed. The input to the benchmark is a graph with
 four nodes and six edges. The application is called ten times. The
 benchmark fails to run faster with \cfont{future} contracts because
 the arguments to the set library
 functions are small and evaluating the related contracts in parallel does not
 cover for the communication and synchronization cost.  

\item[graphs](731 lines in 6 modules) produces all directed graphs with $n$ nodes, distinguished
 root and out-degree bounded by 2, up to isomorphism. The
 benchmark's functions come with a number of assertions that were turned
 into contracts of the exported functions. 
 Almost all of them concern checking of
 type or other simple structural properties of the arguments and the
 results of functions.The input to the benchmark is a graph with 6 nodes.
 and returns 44 non-isomorphic graphs. The application is called three times. The benchmark fails to produce speedup
 with parallel contract execution 
 because the low complexity of its contracts do not
 cover for the communication and synchronization cost. 

\item[div](100 lines in 4 modules) partitions lists of even lengths into two equal halves.  Two
 functions are provided: a recursive and an iterative variant.  Each
 function comes with a contract that checks if the given list has even
 length. The input to the benchmark is a 200 elements list and the test
 loop performs 240,000 divisions.
\end{description}
 The last three benchmarks originate from the PLT benchmarks set.\\
 
\begin{figure*}
\[
\begin{array}{|c||c|c|c|c|c|}
\hline  
\textbf{benchmark}&
\textbf{0\% future/c}^*&
\textbf{random 50\% future/c}^*&
\textbf{random 50\% future/c speedup}&
\textbf{100\% future/c}^*&
\textbf{100\% future/c speedup}
\\\hline
\textbf{fber} & 18298 & 17565 & 1.04 & 16.328  & 1.12 
\\
\textbf{dna} & 63042 & 47516 & 1.39 & 41970 & 1.50  
\\
\textbf{lab} & 87996 & 90545 & 0.97 & 88632 & 0.99
\\
\textbf{tel} & 25318 & 22974 & 1.10 & 21443 & 1.18  
\\
\textbf{tel-bst} & 32659 & 20195 & 1.62 & 227758 & 1.18  
\\
\textbf{lists} & 14321 & 12151 & 1.20 & 12814 & 1.12
\\
\textbf{conform} & 10490 & 11324 & 0.95 & 12169 & 0.89
\\
\textbf{graphs} & 26343 & 28868 & 0.92 & 32892 & 0.80 
\\
\textbf{div} & 13264 & 110363 & 1.20 & 87014 & 1.52 
\\ \hline 
\end{array}
\]
\begin{flushleft}
  {\text{\phantom{*average real}*average real time of five executions in msec}}

\end{flushleft}  
\bigskip
\caption{Experimental results for random annotations}
\label{bench-table}
\end{figure*}

\begin{figure*}
\[
\begin{array}{|c||c|c|c|c|c|c|}
\hline  
\textbf{benchmark}&
\textbf{1/6 future/c}&
\textbf{2/6 future/c}&
\textbf{3/6 future/c}&
\textbf{4/6 future/c}&
\textbf{5/6 future/c}&
\textbf{6/6 future/c}
\\\hline
 &  &  &  & & &
\\ 
\textbf{lab} & 1.07 & 1.05 & 1.00 & 0.99 & 0.98 & 0.99
\\ 
 &  &  &  & & &
\\ \hline 
\end{array}
\]
\bigskip
\caption{The \textbf{lab} benchmark experimental results (speedup)}
\label{lab-table}
\end{figure*}

\begin{figure*}
\[
\begin{array}{|c||c|c|}
\hline  
\textbf{benchmark}&
\textbf{random 50\% future/c speedup}&
\textbf{selected 50\% future/c speedup}\\
\hline
\textbf{fber} & 1.04 & 1.07  
\\
\textbf{dna} & 1.39 & 1.78   
\\
\textbf{lab} & 0.97 & 1.06   
\\
\textbf{tel} & 1.10 & 1.18  
\\
\textbf{tel-bst} & 1.62 & 1.62  
\\
\textbf{lists} & 1.20 & 1.46
\\
\textbf{conform} & 0.95 & 0.97  
\\
\textbf{graphs} & 0.92 & 0.99 
\\
\textbf{div} & 1.20 & 1.20  
\\ \hline 
\end{array}
\]
\bigskip
\caption{Comparison of experimental results between random and selected
annotations} 
\label{comp-table}
\end{figure*}



%riccardo4: 
% \paragraph{Measurements.}
\subsection{Measurements}

For our experiments we used a
Dell Latitude D820 laptop with a Core Duo T2400 CPU @ 1.83 GHz, 667MHz
front side bus, 2MB L2 cache, 2 GB RAM memory and Ubuntu 7.10
operating system.

%riccardo4: rewrote this bit
% As a first approach to the problem of selecting the contracts that are best
% fitted to be annoted as \cfont{future} contracts,  we randomly annotate contracts as \cfont{future}
% contracts and we calculate the resulting execution speedup.
% Figure \ref{bench-table} shows the results of our experiments when half
% and all of the contracts of a benchmark are \cfont{future} contracts. 
% The use of \cfont{future/c}  leads to speedup in
% some cases but not allways. Based on an analysis of the benchmarks and our
% results we develop a series of conjectures that can help us identify the
% most suitable contracts for concurent checking. 
For our evaluation, we conducted two experiments. 
Our first experiment simulates an un-critical programmer who randomly annotates
contracts with future. Specifically, our experiment setup evaluates each 
benchmark with randomly chosen half of contracts turned into
\cfont{future} contracts.
We made five such random annotations and analyzed their average results. 
The analysis suggests some basic conjectures about contract evaluation; most 
importantly, only contracts whose cost competes with the cost of the function 
body (up to synchronization points) should be annotated with
\cfont{future/c}. 
In our second experiment we confirmed these conjectures by carefully selecting
\cfont{future} contracts and repeating the measurements.

Figure \ref{bench-table} shows the results of the first experiment.
The use of \cfont{future/c}  leads to speedup in
some cases but not always. There are two distinct reasons for a lack of
speedup:
\begin{itemize}
    \item
   The contract's asymptotic complexity is less than the complexity of
    communication. This is examplified by the \textbf{graphs} benchmark, in
    which the majority of contracts are type-like and shallow
    structure-properties contracts.  
\item
 The contracts asymptotic complexity is large but the inputs are so small
  that the cost of contract checking does not cover the
  communication cost. This is exemplified by the \textbf{conform}
  benchmark, in which all set operations are applied to sets with
  fewer than ten elements.
  \end{itemize}      


%riccardo4: 
% Also note that the effect of low-cost future contracts on the execution time
The effect of low-cost future contracts on the execution time
of a program depends on the number of calls of the functions that these
contracts guard. If a small number of low-cost contracts is sent to the slave
then the overhead of communication is insignificant. For instance for the
\textbf{tel} benchmark this accumulated cost is less that 200msec and does
not manage to cancel the speedup caused by more complex contracts. When the 
number of transmitted low-cost contracts increases however the
accumulated cost affects the speedup of the system. The \textbf{lab},
\textbf{conform} and \textbf{graphs} benchmarks demonstrate this point; 50\% 
of their contracts guard functions that are used scarcely, so when they 
are transformed to \cfont{future} contracts they don't produce significant
change of the execution time; while the other 50\% protect commonly used
functions and annotating them with \cfont{future} contracts, causew the program
execution to slow down. Simillarly, complex contracts that guard
functions that are called many times contribute much more to the reduction
of the execution time than contracts for functions that are rarely used.

The \textbf{tel-bst} and \textbf{lists} benchmarks reveal
another subtle issue. When this
benchmark is executed sequentially, most of the execution time is spent
evaluating the contracts rather than the calls of the functions they
guard. More specifically 50\% of the
contracts of the benchmark are much more complex asymptotically than the
bodies of the functions they protect. Annotating these contracts as
\cfont{future} contracts leads to saturation of the slave. Contract
checking dominates execution and the master remains idle for the last part
of the evaluation. Any contracts that reside at the queue at this point 
contribute only to the accumulated communication cost and they do not
improve the performance of the system. Practically speaking they are not
checked in parallel. 

 A system with multiple slaves could possible handle extraordinary long
 queues and the saturation issue. However, multiple threads cannot lead to
 a linear improvement of the system's performance. The communication cost leads to a significant slowdown that more concurrency  cannot cover for. Every light-weigth contract sent to the slave(s) delays
 the master thread more than its inline checking. In programs with many such
 contracts the accumulated delay becomes an issue.

 This is the case of our \textbf{lab} benchmark.
The application comes with 6 functions guarded by contracts. Figure
\ref{lab-table} shows the speedup we get when annotating different number of
contracts as \cfont{future} contracts. All the contracts are simple
contracts that capture shallow structural properties, except for one that has
a high asymptotic complexity. Also the function guarded by the latter
contract is applied to large lists and these lists are also the arguments
passed to the contract. When only this contract is turned into
a \cfont{future} contract, the speedup increases significantly. A naive
approach to parallel contract monitoring would lead a programmer to
annotate even more contracts. However, the contracts left without
\cfont{future} annotation are inappropriate for parallel checking
The delegation of
their evaluation to the slave results in gradual but significant slowdown
of the system.


Figure \ref{comp-table} presents the results of the second experiment. 
For this second experiment, we carefully chose contracts for parallel evaluation according  to their relative cost. Specifically, we annotated the contracts in the order that they best satisfy our criteria.
In every case, when 50\% percent of the contracts of a benchmark are
annotated as \cfont{future} contracts and these contracts are selected carefully
according to our conjectures rather than randomly, the system's
performance improves compared to the the random selection.

%riccardo5: 
% Our results imply that the effictive use of \cfont{future} contracts
Our results imply that the effective use of \cfont{future} contracts
demands a careful analysis of each specific application. Naive strategies
not only fail to lead to increase of speedup but also can impose a heavy
time penalty. 
As expected, when the overhead of checking a contract is more than the
communication cost, it is worth checking the contract in parallel with
the rest of the code. 
If a program comes with complex contracts then annotating these
contracts with \cfont{future} tends to result in a significant improvement of the performance of the program.
Parallel contract checking is most effective when the complexity of the 
contract of a function is comparable to the complexity of its computation
up to the first effect statement. 
Also, note that
the overhead from synchronization on output statements is not important.
For example, in the \textbf{conform} benchmark the additional cost of output 
synchronization is less than 200ms.
In general, wise use of the \cfont{future} annotation is highly 
beneficial
as it encourages programmers to add good contracts to their functions
obtaining more guarantees about their programs and restricting at the same
time the penalty of contract monitoring.

 \section{Related Work}
 \label{s:related-work}

Two separate sources inspired our work: the runtime verification
 community~\cite{atm:trace-monitors, bls:spec, cr:mop,
 hr:pathexplorer,kvlls:java-mac,llprj:jml,zkr:run-check} and work on the
 original \cfont{future}~\cite{h:future} construct of LISP and later
 Scheme.

Roughly speaking, run-time verification is analogous to monitoring a
 patient's status during surgery. A run-time verification monitor inserts
 instrumentation code into the program, and the instrumentation code sends
 the value of variables (of ``flat'' or serializable type) to a parallel
 ``monitor thread.'' Over time these transmissions create an execution
 trace, which the monitor continuously inspects for externally specified
 logical properties. These specifications tend to be defined in some
 variant of temporal logic~\cite{p:temp-logic}.  When a trace doesn't
 satisfy a specification, the run-time verification system either issues a
 warning or raises an exception.

In contrast to ordinary contract systems, a run-time verification system
 makes no attempt to synchronize the evaluation of the main program and the
 monitoring thread. When a violation is discovered, the main program may
 have progressed far beyond the point where the violation took place; in
 particular, it may have already issued damaging outputs, which a
 synchronized discovery of the violation would have prevented. Furthermore,
 in contrast to future contracts, run-time verification systems never
 in-line checks into the main program; after all, they monitor on a
 parallel thread only for a separation of concerns.
 Our contract system distributes contract monitoring over both the master
 and the slave thread.

Halstead's~\cite{h:future} \cfont{future} construct attempts to improve the
 performance of functional programs via parallel computations. Expressions
 wrapped in \cfont{future} are evaluated in parallel to the rest of the
 program.  In their place, special placeholder values
 are injected into the main computation.  When the computation applies a
 strict computational construct (+, \cfont{if}) to such placeholder values,
 the main program waits for the appropriate auxiliary thread to complete
 its computation. Our work borrows from Flanagan and Felleisen's semantic
%riccardo4: 
%  framework~\cite{ff:future} for Halstead's \cfont{future}.
 framework~\cite{ff:future} for Halstead's \cfont{future} construct.

%riccardo5: 
% Since contracts are usually formulated in a functional manner and on
%  occasion even employ special purpose declarative notations even in imperative
Since contracts are usually functional expressions, and on
occasion are written in special-purpose declarative notations even in imperative
  languages, the idea of adapting \cfont{future} from functional programming
 to contract programming is natural. The major difference between
 Halstead's \cfont{future}s and ours concerns synchronization. Our future
 contracts do not create placeholder values, because the result of contract
 expressions is never needed to evaluate the rest of the program.
 Synchronization for future contracts is inserted via a compiler or macro
 expander at effectful sites and is enforced at the end of the master
 and slave computations to preserve the meaning of loops. 
 
%riccardo5: 
%  \section{Conclusion and Future Work}
 \section{Conclusion}
\label{s:conclusion}

 This paper describes a step toward the parallelization of a contract
 monitoring system for higher-order languages. 
 We introduce the notion of \cfont{future} contracts.
 In the same spirit as the \cfont{future}~\cite{h:future} construct,
 we take advantage of the implicit parallelism of functional languages 
 to check \cfont{future} contracts in parallel with the rest of the
 program. We prove that annotating a program's contracts with our
 \cfont{future} contract combinator does
 not change the semantics of the program even when the program uses effectful
 constructs like output statements and ref cells operators. 
 We describe a prototype
 implementation of our system and through a series of benchmarks we
 show  that our technique can speedup the execution of programs with contracts.

%riccardo5: rewrote slightly 
% In the near future, we wish to consider two independent extensions of
%  this work. On one hand, we 
%  will investigate further the efficient implementation of our system and how it
%  can benefit from more than one slave and
%  what practical challenges arise in this scenario, especially due to
%  synchronizing effects in slaves. For this purpose, we will use as a
%  starting point existing research on the efficient implementation of
%  \cfont{future} in parallel versions of Scheme and Lisp for large scale 
%  shared-memory  multiprocessors \cite{f:future, khm:mul-t}
%  and especially those based on lazy task creation \cite{mkh:lazytask}.
%  On the other hand, we intend to develop effect analysis to
%  eliminate synchronization points and study the inclusion of
%  additional effectful constructs such as exceptions in our system. 
In the near future, we wish to consider two independent extensions of
this work, with an eye towards a practical and efficient
implementation of parallel contract checking. 
First, we intend to investigate how to take advantage of more than one
monitoring thread. It is not clear how to do this efficiently, because
of the additional challenges created by the need for synchronizing
between those monitoring threads to respect sequential blame
assignment. 
Second, we intend to develop an effect analysis to eliminate
synchronization points and study the inclusion of additional effectful
constructs such as exceptions in our system. 

Both of these extensions will require us to look into 
existing work on the efficient implementation of
\cfont{future} in parallel versions of Scheme and Lisp for 
shared-memory  multiprocessors \cite{f:future}
%, khm:mul-t}, 
as we
expect that some of those techniques will apply in our setting. 
The translation is not immediate---future contracts are not future
computations---but adaptation seems a possibility.
%especially for
%techniques such as lazy task creation \cite{mkh:lazytask}.

\bibliographystyle{plainnat}
\bibliography{literature}

\newpage
\appendix

\section{Appendix: Proofs}
\textit{\textbf{NOTE: For interested reviewers only}}
\subsection{Consistency of $\mathit{eval}$}

\begin{lemma}
  \label{total-eval-i}
  For all $i \in \mathbb{N}$, $\mathit{eval^i}$ is a total function. 
\end{lemma}
 \begin{proof}
   By straightforward analysis on the definition of $\mathit{eval^i}$ and
   the reduction relations $\cstep$ and $^{i}\ccstep$.
 \end{proof}


\begin{definition}[$\subseteq_\ctop$]We define the ordering relation 
  $\subseteq_\ctop$ between elements of the  range of $eval_i$ as follows:
  \begin{itemize}
     \item
    $\cval \DPar \ctrace$ $\subseteq_\ctop$ $\cval \DPar \ctrace$
     \item 
    $\bot \DPar \ctrace$ $\subseteq_\ctop$ $\bot \DPar \ctrace$
     \item 
    $\top \DPar \ctrace$ $\subseteq_\ctop$ $\,\cval \DPar \ctrace^\prime$ 
       where $\ctrace \sqsubseteq \ctrace^\prime$
     \item
    $\top \DPar \ctrace$ $\subseteq_\ctop$ $\bot \DPar \ctrace^\prime$ 
       where $\ctrace \sqsubseteq \ctrace^\prime$
     \item
    $\top \DPar \ctrace$ $\subseteq_\ctop$ $\top \DPar \ctrace^\prime$ 
       where $\ctrace \sqsubseteq \ctrace^\prime$
   \end{itemize}
\end{definition}

\begin{lemma} 
   \label{eval-i-ordered}
   Let $i \in \mathbb{N}$. $\mathit{eval^i}$ satisfies the following
  properties:
   \begin{enumerate}
     \item
    $   
    \text{
    if $\mathit{eval^i}(\cdelta \cholds \cE_o) = \,\cval \DPar \ctrace$ then
    $\mathit{eval^{i+1}}(\cdelta \cholds \cE_o) = \,\,\cval \DPar \ctrace$}$
     \item 
    $   
    \text{
    if $\mathit{eval^i}(\cdelta \cholds \cE_o) = \bot \DPar \ctrace$ then
    $\mathit{eval^{i+1}}(\cdelta \cholds \cE_o) = \bot \DPar \ctrace$}$
  \item 
    $   
    \text{
    if $\mathit{eval^i}(\cdelta \cholds \cE_o) = \top \DPar \ctrace$
    then}\\
    \text{\ntab[7]\,$\mathit{eval^{i+1}}(\cdelta \cholds \cE_o) = \,\, \cval \DPar \ctrace^\prime$ 
       or}\\
   \text{\ntab[7] 
  $\mathit{eval^{i+1}}(\cdelta \cholds \cE_o) = 
                  \bot \DPar \ctrace^\prime$ or}\\
   \text{\ntab[7]
  $\mathit{eval^{i+1}}(\cdelta \cholds \cE_o) = \top \DPar
  \ctrace^\prime$}\\
   \text{\ntab[8] where $\ctrace \sqsubseteq \ctrace^\prime$}$

   \end{enumerate}
\end{lemma}

\begin{proof}
  By the reduction rules if $\crulei{\cP}{}{\cP^\prime}{}{i}$ then
  $\crulei{\cP}{}{\cP^\prime}{}{i+1}$.
  We proceed by straightforward induction on the size of the computation 
  using the definition of $\mathit{eval^i}$and lemma
  \ref{total-eval-i}.
\end{proof}\\

\begin{lemma}
   \label{eval-i-ordered-gen}
  Let $i,j \in \mathbb{N}$. 
  If $i \leq j$ then $\mathit{eval^i} \subseteq_\ctop \mathit{eval^j}$.
  \end{lemma}

\begin{proof}
  By straightforward induction on the difference $j-i$
  using the definitions of $eval^i , \subseteq_\ctop$ and lemma 
  ~\ref{eval-i-ordered}.
\end{proof}


\begin{oldtheorem}{paper:total-eval}
\begin{theorem}
  \label{total-eval}
   $\mathit{eval}$ is a total function. 
\end{theorem}
\end{oldtheorem}

 \begin{proof}
   By lemma \ref{eval-i-ordered-gen}, we know that if $\mathit{eval^i}(\cdelta
   \cholds \cE_o) \neq \top \DPar \ctrace$ and  $\mathit{eval^j}(\cdelta
   \cholds \cE_o) \neq \top \DPar \ctrace$, then  $\mathit{eval^i}(\cdelta
   \cholds \cE_o) = \mathit{eval^j}(\cdelta \cholds \cE_o)$. Thus the first 
   branch of the definition of $\mathit{eval}$ is well-defined. 
   Also again by lemma \ref{eval-i-ordered-gen}, 
   we know that if $i\leq j$, then $\mathit{eval^i}(\cdelta \cholds \cE_o) \subseteq_\ctop
   \mathit{eval^j}(\cdelta \cholds \cE_o)$. Thus if $i \leq j$, 
   $\mathit{eval^i}(\cdelta \cholds \cE_o) = \top \DPar \ctrace_i$ and
   $\mathit{eval^j}(\cdelta \cholds \cE_o) = \top \DPar \ctrace_j$, then
    $\ctrace_i \sqsubseteq \ctrace_j$. We conclude that if 
    $\forall i \in \mathbb{N}.
    \mathit{eval^i}(\cdelta \cholds \cE_o) = \top \DPar
    \ctrace_i$ then $\exists \hat{\phi}$ that is the smallest (possibly
   infinite) trace s.t. 
    $\ctrace_0 \sqsubseteq \dots \sqsubseteq \ctrace_n \sqsubseteq
    \hat{\ctrace}$. Thus the second 
   branch of the definition of $eval$ is well-defined and since the two 
   branches are by definition disjoint, $\mathit{eval}$ is also a total function. 
 \end{proof}

 \subsection{Consistency of $\mathit{eval_c}$}


  To establish that evaluators $\mathit{eval^i_c}$ are
  functions, we need to show that all
possible executions of a program in the parallel machine have the same
observable behavior. More precisely, we must establish that all possible executions of a
 program have the same transition sequence 
 output and termination behavior. We prove this fact in the traditional way using 
an extended form of the Diamond Lemma for our language~\cite{ff:future}. 

\begin{oldtheorem}{paper:transconsistency}
\begin{lemma}
  \label{transconsistency}
   For all $i \in \mathbb{N}$,
   if $\ccrulestari{\cdelta \cholds \cP}{}{\cP_{\mathit{f}}}{c}{i}$  
  then
  \begin{enumerate}
    \item[(a)] if $\ccrulestari{\cdelta \cholds \cP}{}{{\cP_\mathit{f}}^\prime}{c}{i}$,
      $\cP_\mathit{f} = {\cP_\mathit{f}}^\prime$
    \item[(b)] there is no transition such that $\forall k.$ 
      $\ccrulei{ \cdelta \cholds \cP}{*}{\cP_k}{c}{i}$, $m_k > 0$ and
      $\ccruleplusi{ \cdelta \cholds \cP_k}{n_k,m_k}{\cP_{k+1}}{c}{i}$.  
  \end{enumerate}
  \end{lemma}
\end{oldtheorem}  

\begin{proof}
%riccardo3: moved out of enumeration environment
%   \begin{enumerate}
%     \item By lemma \ref{diamond} and the fact that based on our semantics,
%       $\cP_\mathit{f}$ and ${\cP_\mathit{f}}^\prime$ 
%       cannot take any further steps. 
%     \item We proceed by contradiction. 
%       First, assume that there is such a transition. We choose $i=k>n$
%       where $\ccruleplusi{ \cdelta \cholds \cP}{n,m}{\cP_\cval}{c}{i}$. 
%       By lemma \ref{diamond}
%       $\ccruleplusi{ \cdelta \cholds
%       \cP_k}{n^\prime,m^\prime}{\cP_\cval}{c}{i}$ 
%       since $\cP_\mathit{f}$
%       can take no further steps according to our semantics. So there
%       is a transition
%       $\ccruleplusi{ \cdelta \cholds
%       \cP_k}{n^\prime,m^\prime}{\cP_\cval}{c}{f}$.
%       Again by lemma \ref{diamond}, $m_1 + \cdots m_k + n^\prime \leq n$.
%       But $m_i > 0$ and $i > n$. So form the last two inequalities
%       we conclude that $n < m_1 + \cdots m_k$ and $m_1 + \cdots m_k \leq
%       n$ which is the desired contradiction.
%  \end{enumerate}
Part (a) follows from lemma \ref{diamond} and the fact that based on our semantics,
      $\cP_\mathit{f}$ and ${\cP_\mathit{f}}^\prime$ 
      cannot take any further steps. 

For part (b), we proceed by contradiction. 
      First, we assume that there is such a transition. We choose $k>n$
      where $\ccruleplusi{\cdelta \cholds \cP}{n,m}{\cP_f}{c}{i}$. 
      By lemma \ref{diamond},
      $\ccruleplusi{ \cdelta \cholds
      \cP_k}{n^\prime,m^\prime}{\cP_f}{c}{i}$ 
      since $\cP_\mathit{f}$
      can take no further steps according to our semantics. So there
      is a transition
      $\ccruleplusi{ \cdelta \cholds
      \cP_k}{n^\prime,m^\prime}{\cP_f}{c}{f}$.
      Again by lemma \ref{diamond}, $m_1 + \cdots m_k + n^\prime \leq n$.
      But $\forall 1 \leq j \leq k. m_j > 0$ and $k > n$. So from the
      last ~two inequalities
      we conclude that $n < m_1 + \cdots m_k$ and $m_1 + \cdots m_k \leq
      n$ which is the desired contradiction.
\end{proof}\\

\begin{lemma}
  \label{eval-par-i-function}
  For all $i \in \mathbb{N}$, $\mathit{eval^i_c}$ is\, a  function.  
  \end{lemma}

\begin{proof}
  Straightforward by lemma \ref{transconsistency} and the definition of
  $\mathit{eval^i_c}$. \end{proof}\\ 



The Diamond Lemma is the main technical tool used in our argument. 
It states that two different
transitions sequences starting from a state $\cP_0$ and reaching 
$\cP_1$ and $\cP_2$ respectively can both reduce to a state $\cP_3$. 
The Diamond Lemma also relates the number of steps involved
in the two transition sequences. 



\begin{lemma}[Diamond Lemma]
  \label{diamond}
  For all $i \in \mathbb{N}$,\\ 
  ~if $\ccruleplusi{ \cdelta \cholds \cP_0}{n_1,m_1}{\cP_1}{c}{i}$ and 
  $\ccruleplusi{ \cdelta \cholds \cP_0}{n_2,m_2}{\cP_2}{c}{i}$ then  
  $\ccruleplusi{ \cdelta \cholds \cP_1}{k_1,l_1}{\cP_3}{c}{i}$ and 
%riccardo3*: this 'o' should be the number  '0', no?
%   $\ccruleplusi{ \cdelta \cholds \cP_2}{k_2,l_2}{\cP_3}{c}{o}{i}$. 
  $\ccruleplusi{ \cdelta \cholds \cP_2}{k_2,l_2}{\cP_3}{c}{0}{i}$. 
  Also\, $m_1+k_1 \leq n_2 + l_2$, 
  $m_2+k_2 \leq n_1 + l_1$, $k_1 \leq n_2$ and, $k_2 \leq n_1$.
\end{lemma}

\begin{proofsketch} By induction on $n_1$. We use lemma 
  \ref{strip} both for the base case
  and the inductive step.
\end{proofsketch}\\  

\noindent 
To prove the Diamond Lemma, we compose many instances of the
Strip Lemma. The Strip Lemma is a restricted form of the Diamond Lemma 
where $P_1$ is reachable from $P_0$ in one step.

\begin{lemma}[Strip Lemma]
  \label{strip}
  For all $i \in \mathbb{N}$, if 
  $\ccruleplusi{ \cdelta \cholds \cP_0}{1,m_1}{\cP_1}{c}{i}$ and 
  $\ccruleplusi{ \cdelta \cholds \cP_0}{n_2,m_2}{\cP_2}{c}{i}$ then  
  $\ccruleplusi{ \cdelta \cholds \cP_1}{k_1,l_1}{\cP_3}{c}{i}$ and 
  $\ccruleplusi{ \cdelta \cholds \cP_2}{k_2,l_2}{\cP_3}{c}{i}$. 
  Also $m_1+k_1 \leq n_2 + l_2$, 
  $m_2+k_2 \leq 1 + l_1$, $k_1 \leq n_2$ and,  $k_2 \leq 1$.
  \end{lemma}

  \begin{proofsketch} By induction on $n_2$. We use lemma \ref{tile} both for the base case
  and the inductive step.
\end{proofsketch}\\

\noindent
The Tile Lemma is an even more restricted form of the Diamond Lemma.

\begin{lemma}[Tile Lemma]
  \label{tile}
  For all $i \in \mathbb{N}$, ~if\,
  $\ccruleplusi{ \cdelta \cholds \cP_0}{1,m_1}{\cP_1}{c}{i}$\, and\, 
  $\ccruleplusi{ \cdelta \cholds \cP_0}{1,m_2}{\cP_2}{c}{i}$ then  
  $\ccruleplusi{ \cdelta \cholds \cP_1}{k_1,l_1}{\cP_3}{c}{i}$ and 
  $\ccruleplusi{ \cdelta \cholds \cP_2}{k_2,l_2}{\cP_3}{c}{i}$.
  Also $m_1+k_1 \leq 1 + l_2$, 
  $m_2+k_2 \leq 1 + l_1$, $k_1 \leq 1$ and, $k_2 \leq 1$.
\end{lemma}

\begin{proofsketch} By exhaustive case analysis on the possible
  combinations of  $\ccruleplusi{ \cdelta \cholds
  \cP_0}{1,m_1}{\cP_1}{c}{i}$ and
  $\ccruleplusi{ \cdelta \cholds \cP_0}{1,m_2}{\cP_2}{c}{i}$. 
  In all the cases one transition
  proceeds with a reduction rule on the master and the other with a
  reduction rule on the slave. In case the slave step leads to an error
  then the other transition executes the same slave step, and both
  transitions reach the same state. If the slave step does not lead to an
  error then the transition that executed the slave step takes now the
  master step that the other transition executed. The other transition
  behaves accordingly and both transitions reach the same state. 
\end{proofsketch}\\ 

\begin{lemma} 
   \label{eval-par-i-ordered}
  Let $i \in \mathbb{N}$. $eval_c^i$ satisfies the following
  properties:
   \begin{enumerate}
     \item
    $   
    \text{
       if $eval_c^i(\cdelta \cholds \cE_o) = \,\,\cval \DPar \ctrace$ then
       $eval_c^{i+1}(\cdelta \cholds \cE_o) = \,\cval \DPar \ctrace$}$
     \item 
    $   
    \text{
       if $eval_c^i(\cdelta \cholds \cE_o) = \bot \DPar \ctrace$ then
       $eval_c^{i+1}(\cdelta \cholds \cE_o) = \bot \DPar \ctrace$}$
  \item 
    $   
    \text{
    if $eval_c^i(\cdelta \cholds \cE_o) = \top \DPar \ctrace$ then}\\
    \text{\ntab[7]
    $eval_c^{i+1}(\cdelta \cholds \cE_o) = \,\, \cval \DPar \ctrace^\prime$ 
       or}\\
   \text{\ntab[7] 
   $eval_c^{i+1}(\cdelta \cholds \cE_o) = \bot \DPar \ctrace^\prime$ or}\\
   \text{\ntab[7]
   $eval_c^{i+1}(\cdelta \cholds \cE_o) = \top \DPar \ctrace^\prime$}\\
   \text{\ntab[8]
      where $\ctrace \sqsubseteq \ctrace^\prime$}$
   \end{enumerate}
\end{lemma}

\begin{proof}
  By the reduction rules if $\ccrulei{\cP}{}{\cP^\prime}{c}{i}$ then
  $\ccrulei{\cP}{}{\cP^\prime}{c}{i+1}$.
  We proceed by straightforward induction on the size of the computation 
  using the definition of $eval_c^i$ and lemma \ref{eval-par-i-function}.
\end{proof}

\begin{lemma}
   \label{eval-par-i-ordered-gen}
  Let $i,j \in \mathbb{N}$. 
  If $i \leq j$ then $eval_c^i \subseteq_\ctop eval_c^j$.
  \end{lemma}

\begin{proof}
  By straightforward induction on the difference $j-i$
  using the definitions of $eval_c^i , \subseteq_\ctop$ and lemma 
  ~\ref{eval-par-i-ordered}.
\end{proof}

\begin{oldtheorem}{paper:eval-par-function}
\begin{theorem}
  \label{eval-par-function}
   $\mathit{eval_c}$ is a total function.
\end{theorem}
\end{oldtheorem}

 \begin{proof}
   By lemma \ref{eval-par-i-ordered-gen}, we know that if $eval_c^i(\cdelta
   \cholds \cE_o) \neq \top \DPar \ctrace$ and  $eval_c^j(\cdelta
   \cholds \cE_o) \neq \top \DPar \ctrace$, then  $eval_c^i(\cdelta
   \cholds \cE_o) = eval_c^j(\cdelta \cholds \cE_o)$. Thus the first 
   branch of the definition of $eval_c$ is well-defined. 
   Also again by lemma \ref{eval-par-i-ordered-gen}, 
   we know that if $i\leq j$ $eval_c^i(\cdelta \cholds \cE_o)
   \subseteq_\ctop$, then
                   $eval_c^j(\cdelta \cholds \cE_o)$. Thus if $i \leq j$, 
    $eval_c^i(\cdelta \cholds \cE_o) = \top \DPar \ctrace_i$ and
    $eval_c^j(\cdelta \cholds \cE_o) = \top \DPar \ctrace_j$, then
    $\ctrace_i \sqsubseteq \ctrace_j$. We conclude that if 
    $\forall i \in \mathbb{N}$ $eval_c^i(\cdelta \cholds \cE_o) = \top \DPar
    \ctrace_i$ then $\exists \hat{\phi}$ that is the smallest (possibly
    infinite) trace s.t. 
    $\ctrace_0 \sqsubseteq \dots \sqsubseteq \ctrace_n \sqsubseteq
    \hat{\ctrace}$. Thus the second 
   branch of the definition of $eval_c$ is well-defined and since the two 
   branches are by definition disjoint $eval_c$ is also a function. 
  \end{proof}


  \subsection{Consistency and Correctness of $\mathit{eval_s}$}

 \begin{lemma}
  \label{total-eval-simple-i}
  For all $i \in \mathbb{N}$, $\mathit{eval_s^i}$ is a total function. 
\end{lemma}

 \begin{proof}
 \begin{proof}
   By straightforward analysis on the definition of $\mathit{eval_s^i}$ and
   the reduction relations $\cstep$ and $^{i}\ccstep_{s}$.
 \end{proof}
 
 \end{proof}

\begin{lemma} 
   \label{eval-simple-i-ordered}
  Let $i \in \mathbb{N}$. $eval_s^i$ satisfies the following
  properties:
   \begin{enumerate}
     \item
    $   
    \text{
       if $eval_s^i(\cdelta \cholds \cE_o) = \,\,\cval \DPar \ctrace$ then
       $eval_s^{i+1}(\cdelta \cholds \cE_o) = \cval \DPar \ctrace$}$
     \item 
    $   
    \text{
       if $eval_s^i(\cdelta \cholds \cE_o) = \bot \DPar \ctrace$ then
       $eval_s^{i+1}(\cdelta \cholds \cE_o) = \bot \DPar \ctrace$}$
  \item 
    $   
    \text{
    if $eval_s^i(\cdelta \cholds \cE_o) = \top \DPar \ctrace$ then}\\
   \text{\ntab[7]    $eval_s^{i+1}(\cdelta \cholds \cE_o) = \,\, \cval \DPar \ctrace^\prime$ 
       or}\\
   \text{\ntab[7] 
   $eval_s^{i+1}(\cdelta \cholds \cE_o) = \bot \DPar \ctrace^\prime$ or}\\
   \text{\ntab[7] 
    $eval_s^{i+1}(\cdelta \cholds \cE_o) = \top \DPar \ctrace^\prime$
       }\\
   \text{\ntab[8]
      where $\ctrace \sqsubseteq \ctrace^\prime$}$

   \end{enumerate}
\end{lemma}

\begin{proof}
  By the reduction rules if $\ccrulei{\cP}{}{\cP^\prime}{s}{i}$ then
  $\ccrulei{\cP}{}{\cP^\prime}{s}{i+1}$.
  We proceed by straightforward induction on the size of the computation 
  using the definition of $eval_s^i$ and lemma
  \ref{total-eval-simple-i}.
\end{proof}

\begin{lemma}
   \label{eval-simple-i-ordered-gen}
  Let $i,j \in \mathbb{N}$. 
  If $i \leq j$ then $eval_s^i \subseteq_\ctop eval_s^j$.
  \end{lemma}

\begin{proof}
  By straightforward induction on the difference $j-i$
  using the definitions of $eval_s^i , \subseteq_\ctop$ and lemma 
  ~\ref{eval-simple-i-ordered}.
\end{proof}

\begin{oldtheorem}{paper:total-eval-simple}
\begin{lemma}
  \label{total-eval-simple}
   $\mathit{eval_s}$ is a total function.
\end{lemma}
\end{oldtheorem}

 \begin{proof}
   By lemma \ref{eval-simple-i-ordered-gen}, we know that if $eval_s^i(\cdelta
   \cholds \cE_o) \neq \top \DPar \ctrace$ and  $eval_s^j(\cdelta
   \cholds \cE_o) \neq \top \DPar \ctrace$, then  $eval_s^i(\cdelta
   \cholds \cE_o) = eval_s^j(\cdelta \cholds \cE_o)$. Thus the first 
   branch of the definition of $eval_s$ is well-defined. 
   Also again by lemma \ref{eval-simple-i-ordered-gen}, 
   we know that if $i\leq j$ $eval_s^i(\cdelta \cholds \cE_o) \subseteq_\ctop
                   eval_s^j(\cdelta \cholds \cE_o)$. Thus if $i \leq j$, 
    $eval_s^i(\cdelta \cholds \cE_o) = \top \DPar \ctrace_i$ and
    $eval_s^j(\cdelta \cholds \cE_o) = \top \DPar \ctrace_j$ then
    $\ctrace_i \sqsubseteq \ctrace_j$. We conclude that if 
    $\forall i \in \mathbb{N}$ $eval_s^i(\cdelta \cholds \cE_o) = \top \DPar
    \ctrace_i$ then $\exists \hat{\phi}$ that is the smallest (possibly
    infinite) trace s.t. 
    $\ctrace_0 \sqsubseteq \dots \sqsubseteq \ctrace_n \sqsubseteq
   \hat{\ctrace}$. Thus the second 
   branch of the definition of $eval_s$ is well-defined and since the two 
   branches are by definition disjoint $eval_s$ is also a total function. 
  \end{proof}


  \begin{oldtheorem}{paper:eval-equiv-eval-simple}  
\begin{lemma}
  \label{eval-equiv-eval-simple}
  $\mathit{eval_s}=\mathit{eval}$. 
\end{lemma}
\end{oldtheorem}

\begin{proofsketch}
By lexicographic induction on the length
of the maximum trace of the computation and the length of the computation.
 \end{proofsketch}\\





  \subsection{Correctness of $\mathit{eval_c}$}


  \begin{oldtheorem}{paper:stepsimpliesstepp}
 \begin{lemma}
  \label{stepsimpliesstepp}
   For all $i \in \mathbb{N}$, if 
   \ccrulei{ \cdelta \cholds \cP}{}{\cP^\prime}{s}{i} then 
  \ccrulei{ \cdelta \cholds \cP}{}{\cP^\prime}{c}{i}.  
  \end{lemma}
\end{oldtheorem}

\begin{proof}
  Straightforward since the set of $\Longrightarrow_s$ reduction rules
  are a subset of the $\Longrightarrow_c$ reduction rules. 
 \end{proof}\\  

%riccardo3: 
%  \begin{theorem}[Correctness of $\mathit{eval}_c$]

\begin{oldtheorem}{paper:evalpcorrectness-i} 
\begin{lemma}
  \label{evalpcorrectness}
  $\mathit{eval_c} = \mathit{eval_s}$. 
  \end{lemma}
\end{oldtheorem}

\begin{proof}
  We first prove that $\mathit{eval_s} \subseteq \mathit{eval_c}$.
%riccardo3: again, getting rid of _o - distracting.
  Consider a program $\cdelta \In \cE$.
%riccardo3: 
%   Then according to the definition of $eval_s$ and lemma
  Then according to the definition of $eval_s$ and theorem
  \ref{total-eval-simple}, $\exists i \in \mathbb{N}$ such that 
  $eval_s^i(\cdelta \In \cE)= r$ and $r 
  \neq \top \DPar
  \ctrace$  and  
  $eval_s(\cdelta \In \cE)= eval_s^i(\cdelta \In \cE)$ or
  $\forall i \in \mathbb{N}$ 
  $eval_s^i(\cdelta \In \cE)= \top \DPar \ctrace_i$  and  
  $eval_s(\cdelta \In \cE)= \bot \DPar \hat{\ctrace}$ where
  $\hat{\ctrace}$ is the smallest trace such that 
%riccardo3: 
%   $\ctrace_1\sqsubseteq \dots \sqsubseteq
%      \ctrace_n \sqsubseteq \hat{\ctrace}$.
  $\ctrace_1\sqsubseteq \ctrace_2\sqsubseteq \ctrace_3\sqsubseteq
  \dots \sqsubseteq \hat{\ctrace}$.

%riccardo3: 
%      In the first case, by theorem \ref{total-eval-simple},
%      we know that we can choose without loss of 
%      generality the smallest $i \in \mathbb{N}$
     In the first case, we know by theorem \ref{total-eval-simple}
     that there exists a smallest $i \in \mathbb{N}$
     such that $eval_s^i(\cdelta \In \cE)= r$ and 
     $r \neq \top \DPar  \ctrace$. We know now that either 
     $\ccruleplusi{\cP_o}{n}{\cP_\mathit{f}}{s}{i}$
     or  $r = \bot \DPar \ctrace$,
     $\ccruleplusi{\cP_o}{n}{\cP^\prime}{s}{i}$
     and there is $\cP^{\prime\prime}$ s.t. 
     $\ccruleplusi{\cP^\prime}{}{\cP^{\prime\prime}}{s}{i}$.
   By induction on $n$ and using lemmas \ref{stepsimpliesstepp} and
   \ref{eval-par-i-function} for the inductive step, we show that 
   $eval^i_c(\cdelta \In \cE)= r$.
%riccardo3: 
%    By lemma \ref{eval-par-function}, we conclude that 
   By theorem \ref{eval-par-function}, we conclude that 
   $eval_c(\cdelta \In \cE)= r$.

  In the second case, by the definition of $eval_s$
  we know that $\forall i \in \mathbb{N}$ 
  $eval_s^i(\cdelta \In \cE)= \top \DPar \ctrace$. 
  We know now that $\ccruleplusi{\cP_o}{n}{\top \DPar \ctrace_i}{s}{i}$.
   By induction on $n$ and using lemmas \ref{stepsimpliesstepp} and
   \ref{eval-par-i-function} for the inductive step, we show that 
   $eval^i_c(\cdelta \In \cE)= \top \DPar \ctrace_i $.
   By the definition of $eval_c$, we conclude that
     $eval_c(\cdelta \In \cE)= \top \DPar \hat{\ctrace}$ where
   $\hat{\ctrace}$ is the smallest trace such that 
  $\ctrace_0 \sqsubseteq \ctrace_1 \sqsubseteq
     \ctrace_2 \sqsubseteq \dots \sqsubseteq \hat{\ctrace}$.

      This
  establishes that $\mathit{eval_s} \subseteq \mathit{eval_c}$.
%riccardo3: 
%   By lemma \ref{total-eval} we  know that $\mathit{eval_s}$ is a total function,
%   by lemma \ref{eval-par-function} we know that $\mathit{eval_c}$ is a function
  By lemma \ref{total-eval-simple} we  know that $\mathit{eval_s}$ is a total function,
  by theorem \ref{eval-par-function} we know that $\mathit{eval_c}$ is a function
  on the same domain.
  Therefore we can conclude that $\mathit{eval_c} = \mathit{eval_s}$. 
 \end{proof}

 \begin{oldtheorem}{paper:main}
  \begin{theorem}
  \label{main}
  $\mathit{eval} = \mathit{eval_c}$. 
  \end{theorem}
\end{oldtheorem}
  \begin{proof}
   By lemmas \ref{eval-equiv-eval-simple}, \ref{evalpcorrectness}, ~and
   transitivity.
  \end{proof}

              


\end{document}

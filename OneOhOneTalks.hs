module OneOhOneTalks(talks) where

import Data.Time

import OneOhOne

-- IMPORTANT: Always add new talks at the top of the list!
-- The use of reverse then makes sure that the old entries
-- get to keep their old id numbers that are zipped in

talks = reverse $ zipWith (,) [(0::Int)..] $ reverse [
  SpecialEvent {
    date  = (UTCTime (fromGregorian 2015 02 18)
                    (timeOfDayToTime (TimeOfDay 12 0 0))),
    title = "SPLS @ Strathclyde",
    url = "",
    location = "Royal College Building, room RC512",
    locationurl= "http://www.strath.ac.uk/maps/royalcollegebuilding/",
    description = ""
    }
  ,

  
  Talk {
  date       = (UTCTime (fromGregorian 2015 2 11)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Conor McBride",
  institute  = "MSP",
  speakerurl = "https://personal.cis.strath.ac.uk/conor.mcbride/",
  insturl    = "",
  title      = "Totality versus Turing Completeness?",
  abstract   = "I gave an <a href='http://www.dcs.gla.ac.uk/research/spls/Mar12/'>SPLS talk</a>, which was mostly propaganda, about why people should stop claiming that totality loses Turing completeness. There was some technical stuff, too, about representing a recursive definition as a construction in the free monad whose effect is calling out to an oracle for recursive calls: that tells you what it is to be recursive without prejudicing how to run it. I'm trying to write this up double-quick as a paper for the miraculously rubbery MPC deadline, with more explicit attention to the monad morphisms involved. So I'd be grateful if you would slap down the shtick and make me more morphic. The punchline is that the Bove-Capretta domain predicate construction is a (relative) monad morphism from the free monad with a recursion oracle to the (relative) monad of Dybjer-Setzer Induction-Recursion codes. But it's worth looking at other monad morphisms (especially to the Delay monad) along the way.",
  location   = "LT1415",
  material   = [] }
 ,
  
  Talk {
  date       = (UTCTime (fromGregorian 2015 1 28)
                        (timeOfDayToTime (TimeOfDay 16 30 0))),
  speaker    = "Tarmo Uustalu",
  institute  = "Institute of Cybernetics, Tallinn",
  speakerurl = "http://www.cs.ioc.ee/~tarmo/",
  insturl    = "http://cs.ioc.ee/",
  title      = "Runners for your computations",
  abstract   = "What structure is required of a set so that computations in a given notion of computation can be run statefully this with set as the state space? Some answers: To be able to serve stateful computations, a set must carry the structure of a lens; for running interactive I/O computations statefully, a \"responder-listener\" structure is necessary etc. I will observe that, in general, to be a runner of computations for an algebraic theory (defined as a set equipped with a monad morphism between the corresponding monad and the state monad for this set) is the same as to be a comodel of this theory, ie a coalgebra of the corresponding comonad. I will work out a number of instances. I will also compare runners to handlers.",
  location   = "LT1415",
  material   = [] }
 ,
  
  Talk {
  date       = (UTCTime (fromGregorian 2015 1 22)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "James Chapman",
  institute  = "Institute of Cybernetics, Tallinn",
  speakerurl = "http://cs.ioc.ee/~james/",
  insturl    = "http://cs.ioc.ee/",
  title      = "Termination, later",
  abstract   = "It would be a great shame if dependently-typed programming (DTP) restricted us to only writing very clever programs that were a priori structurally recursive and hence obviously terminating. Put another way, it is a lot to ask of the programmer to provide the program and its termination proof in one go, programmers should also be supported in working step-by-step. I will show a technique that lowers the barrier of entry, from showing termination to only showing productivity up front, and then later providing the opportunity to show termination (convergence). I will show an example of a normaliser for STLC represented in Agda as a potentially non-terminating but nonetheless productive corecursive function targeting the coinductive delay monad.\n\n(Joint work with  Andreas Abel)",
  location   = "LT1310",
  material   = [] }
 ,

  Talk {
  date       = (UTCTime (fromGregorian 2014 12 17)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Conor McBride",
  institute  = "MSP",
  speakerurl = "https://personal.cis.strath.ac.uk/conor.mcbride/",
  insturl    = "",
  title      = "Worlds, Types and Quantification",
  abstract   = "I've managed to prove a theorem that I've been chasing for a while. The trouble, of course, was <em>stating</em> it. I'll revisit the motivation for extending type systems with an analysis of not just <em>what</em> things are but <em>where</em>, <em>when</em>, <em>whose</em>, etc. The idea is that typed constructions occur in one of a preordered set of worlds, with scoping restricted so that information flows only \"upwards\" from one world to another. Worlds might correspond to \"at run time\" and \"during typechecking\", or to computation in distinct cores, or in different stages, etc. What does the dependent function space mean in this setting? For a long time, I thought that each world had its own universal quantifier, for abstracting over stuff from that world. Failure to question this presumption is what led to failure to state a theorem I could prove. By separating quantifiers from worlds, I have fixed the problem. I'll show how to prove the key fact: if I can build something in one world and then move it to another, then it will also be a valid construction once it has arrived at its destination.",
  location   = "LT1310",
  material   = [] }
 ,

  Talk {
     date       = (UTCTime (fromGregorian 2014 12 10)
                           (timeOfDayToTime (TimeOfDay 11 0 0))),
     speaker    = "Aleks Kissinger",
     institute  = "Oxford",
     speakerurl = "http://www.cs.ox.ac.uk/people/aleks.kissinger/about.html",
     insturl    = "http://www.cs.ox.ac.uk/activities/quantum/",
     title      = "ZX and PROPs",
     abstract   = "",
     location   = "LT1310",
     material   = [] }
  ,

  SpecialEvent {
    date  = (UTCTime (fromGregorian 2014 12 03)
                    (timeOfDayToTime (TimeOfDay 14 0 0))),
    title = "HoTT reading group @ Strathclyde",
    url = "http://homepages.inf.ed.ac.uk/s1225336/hott-reading-group/",
    location = "LT1310",
    locationurl= "",
    description = "We will read the paper <a href='http://drops.dagstuhl.de/opus/volltexte/2014/4628/pdf/7.pdf'>A Model of Type Theory in Cubical Sets</a> by <a href='http://www.ii.uib.no/~bezem/'>Marc Bezem</a>, <a href='http://www.cse.chalmers.se/~coquand/'>Thierry Coquand</a> and <a href='http://www.cse.chalmers.se/~simonhu/'>Simon Huber</a>. Thierry's <a href='http://www.cse.chalmers.se/~coquand/comp.pdf'>Variation on cubical sets</a> might also be useful. <em>Administrative details</em>: meet for lunch at 12am for those who want to, reading group starts at 2pm."
    }
  ,
  
  Talk {
     date       = (UTCTime (fromGregorian 2014 11 19)
                           (timeOfDayToTime (TimeOfDay 12 0 0))),
     speaker    = "Neil Ghani and Clemens Kupke",
     institute  = "MSP",
     speakerurl = "",
     insturl    = "",
     title      = "Arrow's Theorem and Escalation",
     abstract   = "Neil and Clemens will report back from the <a href='http://www.cs.le.ac.uk/people/akurz/lsb.html'>Lorentz Center Workshop on Logics for Social Behaviour</a>.",
     location   = "LT1310",
     material   = [] }
  ,

  Talk {
     date       = (UTCTime (fromGregorian 2014 11 26)
                           (timeOfDayToTime (TimeOfDay 11 0 0))),
     speaker    = "Ross Duncan",
     institute  = "MSP",
     speakerurl = "http://personal.strath.ac.uk/ross.duncan/",
     insturl    = "",
     title      = "Diagrammatic languages for monoidal categories",
     abstract   = "Monoidal categories are essentially 2-dimensional things, so why on earth would we represent them using a linear string of symbols?  I'll talk about how to use string diagrams for monoidal categories, graph rewriting for reasoning within them, and how the syntax can be extended to handle certain kinds of infinitary expressions with the infamous !-box.  If there's time I'll finish with some half-baked (eh... basically still looking for the on switch of the oven...) ideas of how to generalise them.",
     location   = "LT1310",
     material   = [] }
  ,
  
  Talk {
     date       = (UTCTime (fromGregorian 2014 11 06)
                           (timeOfDayToTime (TimeOfDay 9 0 0))),
     speaker    = "Kevin Dunne",
     institute  = "MSP",
     speakerurl = "",
     insturl    = "",
     title      = "Comonadic Cellular Automata",
     abstract   = "Kevin will be giving an informal talk about some of the stuff he has been learning about. He'll give the definition of a cellular automaton and then talk about how this definition can be phrased in terms of a comonad.",
     location   = "LT1310",
     material   = [] }
  ,

  Talk {
     date       = (UTCTime (fromGregorian 2014 11 05)
                           (timeOfDayToTime (TimeOfDay 11 0 0))),
     speaker    = "Shin-ya Katsumata",
     institute  = "Kyoto University",
     speakerurl = "http://www.kurims.kyoto-u.ac.jp/~sinya/index-e.html",
     insturl    = "http://www.kurims.kyoto-u.ac.jp/en/index.html",
     title      = "Logical Relations for Monads by Categorical TT-Lifting",
     abstract   = "Logical relations are widely used to study various properties of typed lambda calculi. By extending them to the lambda calculus with monadic types, we can gain understanding of the properties on functional programming languages with computational effects. Among various constructions of logical relations for monads, I will talk about a categorical TT-lifting, which is a semantic analogue of Lindley and Stark's leapfrog method.\n\nAfter reviewing some fundamental properties of the categorical TT-lifting, we apply it to the problem of relating two monadic semantics of a call-by-value functional programming language with computational effects. This kind of problem has been considered in various forms: for example, the relationship between monadic style and continuation passing style representations of call-by-value programs was studied around '90s. We give a set of sufficient conditions to solve the problem of relating two monadic semantics affirmatively. These conditions are applicable to a wide range of such problems.",
     location   = "Boardroom (LT1101d)",
     material   = [] }
  ,

  Talk {
     date       = (UTCTime (fromGregorian 2014 10 27)
                           (timeOfDayToTime (TimeOfDay 14 0 0))),
     speaker    = "Dominic Orchard",
     institute  = "Imperial College London",
     speakerurl = "http://www.cl.cam.ac.uk/~dao29/",
     insturl    = "http://mrg.doc.ic.ac.uk/",
     title      = "Constructing analysis-directed semantics",
     abstract   = "All kinds of semantics are syntax directed: the semantics follows from the syntax. Some varieties of semantics are syntax and type directed. In this talk, I'll discuss syntax, type, <em>and</em> analysis directed semantics (analysis-directed semantics for short!), for analyses other than types. An analysis-directed semantics maps from terms coupled with derivations of a static program analysis into some semantic domain. For example, the simply-typed lambda calculus with an effect system maps to the category generated by a strong parametric effect monad (due to Katsumata) and a bounded-linear logic-like analysis (described as a coeffect systems) maps to a category generated by various structures related to monoidal comonads. I'll describe a general technique for building analysis-directed semantics where semantic objects and analysis objects have the same structure and are coupled by lax homomorphisms between them. This aids proving semantic properties: the proof tree of an equality for two program analyses implies the rules needed to prove equality of the programs' denotations.",
     location   = "Boardroom (LT1101d)",
     material   = [] }
  ,

  Talk {
     date       = (UTCTime (fromGregorian 2014 11 12)
                           (timeOfDayToTime (TimeOfDay 11 0 0))),
     speaker    = "Peter Hancock",
     institute  = "",
     speakerurl = "",
     insturl    = "",
     title      = "Lambda-abstraction and other diabolical contrivances",
     abstract   = "The topic is the unholy trinity of eta, zeta, and xi. I'll indicate how Curry managed to give a finite combinatorial axiomatisation of this nastiness, by anticipating (almost-but-not-<em>quite</em>) McBride et al's applicative functors.",
     location   = "LT1310",
     material   = [] }
  ,

  SpecialEvent {
    date  = (UTCTime (fromGregorian 2014 10 29)
                    (timeOfDayToTime (TimeOfDay 12 0 0))),
    title = "SICSA CSE Meeting on Effects and Coeffects Systems and their use for resource control",
    url = "http://staff.computing.dundee.ac.uk/marcogaboardi/effcoeff.html",
    location = "Dundee",
    locationurl= "",
    description = ""
    }
  ,
 
  Talk {
     date       = (UTCTime (fromGregorian 2014 10 22)
                           (timeOfDayToTime (TimeOfDay 11 0 0))),
     speaker    = "Neil Ghani",
     institute  = "MSP",
     speakerurl = "https://personal.cis.strath.ac.uk/neil.ghani/",
     insturl    = "",
     title      = "Higher dimensional parametricity and its cubical structure",
     abstract   = "Neil will talk about partial progress made during the summer on higher dimensional parametricity and the cubical structures that seem to arise. \nDetails will be kept to a minimum and, of course, concepts stressed.",
     location   = "LT1310",
     material   = [] }
  ,

  SpecialEvent {
    date  = (UTCTime (fromGregorian 2014 10 15)
                    (timeOfDayToTime (TimeOfDay 12 0 0))),
    title = "SPLS",
    url = "http://www.macs.hw.ac.uk/~rs46/spls-hwu-q3-2014/",
    location = "Heriot Watt",
    locationurl= "",
    description = ""
    }
  ,
  
 Talk {
  date       = (UTCTime (fromGregorian 2014 10 8)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Conor McBride",
  institute  = "MSP",
  speakerurl = "https://personal.cis.strath.ac.uk/conor.mcbride/",
  insturl    = "",
  title      = "\"Real-world data\" and dependent types",
  abstract   = "Conor has offered to talk to us about what he has been thinking about recently. He says this includes models, views, and dependent types.",
  location   = "LT1310",
  material   = [] }
 ,
 
  Talk {
  date       = (UTCTime (fromGregorian 2014 10 1)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Fredrik Nordvall Forsberg",
  institute  = "MSP",
  speakerurl = "https://personal.cis.strath.ac.uk/fredrik.nordvall-forsberg/",
  insturl    = "",
  title      = "Initial algebras via strong dinaturality, internally",
  abstract   = "  Or: My summer with Steve\nOr: How Christine and Frank were right, after all\nOr: Inductive types for the price of function extensionality and impredicative Set\n\n Christine Paulin-Mohring and Frank Pfenning suggested to use impredicative encodings of inductive types in the Calculus of Constructions, but this was later abandoned, since it is \"well-known\" that induction principles, i.e. dependent elimination,  can not be derived for this encoding. It now seems like it is  possible to give a variation of this encoding for which the  induction principle is derivable after all. The trick is to use  identity types to cut down the transformations of type  (Pi X : Set) . (F(X) -> X) -> X to the ones that are internally strongly dinatural, making use of a formula for a \"generalised Yoneda Lemma\" by Uustalu and Vene.",
  location   = "LT1310",
  material   = [] }
 ,

  Talk {
  date       = (UTCTime (fromGregorian 2014 8 15)
                        (timeOfDayToTime (TimeOfDay 15 0 0))),
  speaker    = "Fredrik Nordvall Forsberg",
  institute  = "MSP",
  speakerurl = "https://personal.cis.strath.ac.uk/fredrik.nordvall-forsberg/",
  insturl    = "",
  title      = "Internal parametricity",
  abstract   = "",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 5 28)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Ohad Kammar",
  institute  = "Cambridge",
  speakerurl = "http://www.cl.cam.ac.uk/~ok259/",
  insturl    = "http://www.cl.cam.ac.uk/",
  title      = "Graphical algebraic foundations for monad stacks",
  abstract   = "Ohad gave an informal overview of his current draft, with the following abstract:\n\nHaskell incorporates computational effects modularly using sequences of monad transformers, termed monad stacks. The current practice is to find the appropriate stack for a given task using intractable brute force and heuristics. By restricting attention to algebraic stack combinations, we provide a linear-time algorithm for generating all the appropriate monad stacks, or decide no such stacks exist. Our approach is based on Hyland, Plotkin, and Power's algebraic analysis of monad transformers, who propose a graph-theoretical solution to this problem. We extend their analysis with a straightforward connection to the modular decomposition of a graph and to cographs, a.k.a. series-parallel graphs.\n\nWe present an accessible and self-contained account of this monad-stack generation problem, and, more generally, of the decomposition of a combined algebraic theory into sums and tensors, and its algorithmic solution. We provide a web-tool implementing this algorithm intended for semantic investigations of effect combinations and for monad stack generation.",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 5 21)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Clemens Kupke",
  institute  = "MSP",
  speakerurl = "https://personal.cis.strath.ac.uk/clemens.kupke/",
  insturl    = "",
  title      = "Coalgebraic Foundations of Databases",
  abstract   = " This 101 is intended to be a brainstorming session on possible links between the theory of coalgebras and the theory of databases. I will outline some ideas in this direction and I am looking forward to your feedback.",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 5 14)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Guillaume Allais",
  institute  = "MSP",
  speakerurl = "https://gallais.org",
  insturl    = "",
  title      = "Resource aware contexts and proof search for IMLL",
  abstract   = "In Intuitionistic Multiplicative Linear Logic, the right introduction rule for tensors implies picking a 2-partition of the set of assumptions and use each component to inhabit the corresponding tensor's subformulas. This makes a naive proof search algorithm intractable. Building a notion of resource availability in the context and massaging the calculus into a more general one handling both resource consumption and a notion of \"leftovers\" of a subproof allows for a well-structured well-typed by construction proof search mechanism.\n\n Here is an <a href='http://gallais.org/code/LinearProofSearch/poc.LinearProofSearch.html'>Agda file</a> implementing the proof search algorithm.",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 4 7)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Jamie Gabbay",
  institute  = "Heriot-Watt",
  speakerurl = "http://www.gabbay.org.uk",
  insturl    = "http://www.macs.hw.ac.uk/departments/computer-science.htm",
  title      = "Nominal sets",
  abstract   = "",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 4 2)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Thorsten Altenkirch",
  institute  = "Nottingham",
  speakerurl = "http://www.cs.nott.ac.uk/~txa/",
  insturl    = "http://www.cs.nott.ac.uk/",
  title      = "Towards cubical type theory",
  abstract   = "",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 3 19)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Guillaume Allais",
  institute  = "MSP",
  speakerurl = "http://www.gallais.org",
  insturl    = "",
  title      = "The selection monad transformer",
  abstract   = "Guillaume presented parts of Hedges' paper <a href='http://www.eecs.qmul.ac.uk/~julesh/papers/monad_transformers.pdf'>Monad transformers for backtracking search</a> (accepted to <a href='http://www.cs.bham.ac.uk/~pbl/msfp2014/'>MSFP 2014</a>). The paper extends Escardo and Oliva's work on the selection and continuation monads to the corresponding monad transformers, with applications to backtracking search and game theory.",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 3 5)
                        (timeOfDayToTime (TimeOfDay 15 0 0))),
  speaker    = "Stuart Hannah",
  institute  = "Strathclyde Combinatorics",
  speakerurl = "https://personal.cis.strath.ac.uk/stuart.a.hannah/",
  insturl    = "http://combinatorics.cis.strath.ac.uk/",
  title      = "Lagrange inversion",
  abstract   =  "Stuart spoke about Lagrange inversion, a species-theoretic attempt to discuss the existence of solutions to equations defining species.",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 2 28)
                        (timeOfDayToTime (TimeOfDay 14 0 0))),
  speaker    = "Neil Ghani",
  institute  = "MSP",
  speakerurl = "https://personal.cis.strath.ac.uk/neil.ghani/",
  insturl    = "",
  title      = "Species",
  abstract   = "Neil spoke about how adding structured quotients to containers gives rise to a larger class of data types.",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 2 19)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Tim Revell",
  institute  = "MSP",
  speakerurl = "",
  insturl    = "",
  title      = "Synthetic Differential Geometry",
  abstract   = "Tim gave a brief introduction to Synthetic Differential Geometry. This is an attempt to treat smooth spaces categorically so we can extend the categorical methods used in the discrete world of computer science to the continuous work of physics. ",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 2 12)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Conor McBride",
  institute  = "MSP",
  speakerurl = "http://strictlypositive.org/",
  insturl    = "",
  title      = "Worlds",
  abstract   = "Conor talked about worlds (aka phases, aka times, ...): why one might bother, and how we might go about equipping type theory with a generic notion of permitted information flow.",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 2 5)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Miles Gould",
  institute  = "Edinburgh",
  speakerurl = "http://homepages.inf.ed.ac.uk/mgould1/",
  insturl    = "http://www.inf.ed.ac.uk/",
  title      = "Operads",
  abstract   = "Miles has kindly agreed to come through and tell us about Operads, thus revisiting the topic of his PhD and the city in which he did it. ",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 1 22)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Lorenzo Malatesta",
  institute  = "MSP",
  speakerurl = "",
  insturl    = "",
  title      = "Overview of (extensions of) inductive-recursive definitions",
  abstract   = "",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2014 1 8)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Conor McBride",
  institute  = "MSP",
  speakerurl = "http://strictlypositive.org/",
  insturl    = "",
  title      = "On the recently found inconsistency of the univalence axiom in current Agda and Coq",
  abstract   = "",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2013 12 18)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Ross Duncan",
  institute  = "MSP",
  speakerurl = "http://personal.strath.ac.uk/ross.duncan/",
  insturl    = "",
  title      = "Quantum Mechanics",
  abstract   = "",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2013 11 20)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Robin Adams",
  institute  = "MSP visitor",
  speakerurl = "http://www.cs.rhul.ac.uk/~robin/",
  insturl    = "",
  title      = "Classical Type Theories",
  abstract   = "In 1987, Felleisen showed how to add control operators (for things like exceptions and unconditional jumps) to the untyped lambda-calculus.  In 1990, Griffin idly wondered what would happen if one did the same in a typed lambda calculus.  The answer came out: the inhabited types become the theorems of classical logic.\n\n I will present the lambda mu-calculus, one of the cleanest attempts to add control operators to a type theory.  We'll cover the good news: the inhabited types are the tautologies of minimal classical logic, and Godel's Double Negation translation from classical to intuitionistic logic turns into the CPS translation.\n\nAnd the bad news: control operators don't play well with other types. Add natural numbers (or some other inductive type), and you get inconsistency. Add Sigma-types, and you get degeneracy (any two objects of the same type are definitionally equal).  It gets worse: add plus-types, and you break Subject Reduction.",
  location   = "LT1310",
  material   = [] }
 ,

 Talk {
  date       = (UTCTime (fromGregorian 2013 11 13)
                        (timeOfDayToTime (TimeOfDay 11 0 0))),
  speaker    = "Guillaume Allais",
  institute  = "MSP",
  speakerurl = "http://www.gallais.org",
  insturl    = "",
  title      = "Continuation Passing Style",
  abstract   =
     "I chose to go through (parts of) Hatcliff and Danvy's paper <a href='http://dl.acm.org/citation.cfm?id=178053'> \"A Generic Account of Continuation-Passing Styles\"</a> (POPL 94) which gives a nice factorization of various CPS transforms in terms of:\
    \  <ul>\
    \   <li>embeddings from STLC to Moggi's computational meta-language\
    \       (either call-by-value, call-by-name, or whatever you can come\
    \       up with)</li>\
    \   <li>followed by a generic CPS transform transporting terms from ML\
    \       back to STLC</li>\
    \ </ul>\
    \ Here is <a href='http://gallais.org/code/GenericCPS/definitions.html'>an Agda file</a> containing what we had the time to see.",
  location   = "LT1310",
  material   = [] }
 ]
    
